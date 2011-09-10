;;; tinytalk.el
;;;
;;; scott goehring (goehring@churchy.ai.mit.edu)
;;; robert earl (rearl@wookumz.ai.mit.edu)
;;; version 0.5
;;;
;;; this program has an potentially infinite number of bugs, none of
;;; which are the authors' fault.  you are encouraged to report any
;;; problems you have to the author provided you do so in a useful
;;; fashion.  suggestions accompanied with code are also welcome.
;;; ideas less so as we have plenty of ideas already.  don't post to
;;; rec.games.mud as that'll only get you flamed.
;;;
;;; you must load this before byte-compiling because of the defsetf's.
;;; i suggest:
;;; 	emacs -batch -l tinytalk.el -f batch-byte-compile tinytalk.el
;;; if you load this file into emacs and do a M-x compile it will
;;; byte-compile it for you in an inferior compile shell.
;;;
;;; you can figure out the rest from browsing the lisp code.  one of
;;; these days we'll actually get around to getting some documentation
;;; into this beast.  if you want to write documentation for it i'll
;;; gladly send you a dozen homemade chocolate brownies.
;;;
;;; changes in version 0.5:
;;; * the order of the arguments for add-world have changed
;;; * fixed several bugs.
;;; * added %[] expansions: the sexp between the [] is eval'ed and the
;;;   value interpolated into the command.
;;; * added &-expansions which are performed on all commands before
;;;   macro expansion.
;;; 
;;; changes in version 0.4:
;;; * bindings now generalized (all connection and world variables are
;;;   stored in the same way)
;;; * obarrays now being used for world and connection records.  this
;;;   should improve speed
;;;
;;; changes in version 0.3:
;;; * world-local and connection-local variables added
;;; * connection-local triggers and macros added
;;; * current connection is selected by window, not by a state variable
;;; * most internals have changed
;;; * functional macros have changed calling protocol
;;; * trigger panic disable is now available
;;;
;;; changes in version 0.2:
;;; * tinymud-filter now loops through all triggers instead of
;;;   stopping on the first hit.  this may modify the behavior of
;;;   trigger sets where a later trigger was previously overridden by
;;;   a more specific earlier trigger.
;;; * the output filling function will prefix continuation lines with
;;;   the value of fill-prefix in the output buffer.
;;; * tinymud-apply-macro will now evaluate any list forms that don't
;;;   appear to be lambdas.

(require 'cl)
(provide 'tinytalk)

(defmacro set-value (rec fld val)
  "Set field FLD of record REC to VAL."
  (list 'put rec fld val))

(defmacro get-value (rec fld)
  "Get the value of field FLD in record REC.  Returns nil if FLD is unbound."
  (list 'get rec fld))

(defsetf get-value set-value)

(defvar tinymud-default-world nil
  "*Default input world string for \\[tinymud-change-world]")
(defvar tinymud-filter-line-hook nil
  "*Function to call with args CONN and LINE on each line of input from a MUD.")
(defvar tinymud-connections (make-vector 11 nil)
  "List of current connections")
(defvar tinymud-worlds (make-vector 17 nil)
  "List of defined worlds")
(defvar tinymud-worlds-list nil)
(defvar tinymud-connection-list nil)

(defvar tinymud-global-macros nil
  "Global macros")
(defvar tinymud-global-triggers nil
  "Global triggers")

(defvar tinymud-mode-map nil
  "Major mode keymap for tinymud")
(defvar tinymud-output-mode-map nil
  "Major mode keymap for tinymud output")

(if (not tinymud-mode-map)
    (progn
      (setq tinymud-mode-map (make-keymap))))
(if (not tinymud-output-mode-map)
    (progn
      (setq tinymud-output-mode-map (make-keymap))
      (suppress-keymap tinymud-output-mode-map)))

(defun add-world (name address port &optional char pwd)
  "Add or update world NAME (character name CHAR, character password PWD,
address ADDRESS and port PORT) to the known worlds list."
  (interactive "sName: \nsAddress: \nnPort: \nsCharacter: \nsPassword: ")
  (let ((wspec (get-world name)))
    (if (not wspec)
	(progn
	  (setq wspec (intern name tinymud-worlds))
	  (if tinymud-worlds-list
	      (setf (cdr (last tinymud-worlds-list)) (list wspec))
	    (setf tinymud-worlds-list (list wspec)))
	  (setf (get-value wspec 'name) name)))
    (setf (get-value wspec 'address) address)
    (setf (get-value wspec 'port) port)
    (if char (setf (get-value wspec 'character) char))
    (if pwd (setf (get-value wspec 'password) pwd))
    wspec))

(defun duplicate-world (world newworld)
  (interactive "sWorld: \nsNew world: ")
  (if (stringp world) (setq world (get-world world)))
  (add-world newworld
	     (get-value world 'address)
	     (get-value world 'port)))

(defun set-world-character (world name pwd)
  (interactive "sWorld: \nsCharacter: \nsPassword: ")
  (if (stringp world) (setq world (get-world world)))
  (setf (get-value world 'character) name)
  (setf (get-value world 'password) pwd))

(defun get-local-binding (conn sym)
  (or (get-value conn sym)
      (get-value (get-value conn 'world) sym)))

(defun extract-word ()
  (and (not (eolp))
       (re-search-forward "\\S +" (point-max) t)
       (buffer-substring (match-beginning 0) (match-end 0))))

(defun tinymud () "Start up a tinytalk session"
  (interactive)
  (load (or (getenv "TINYMUD") "~/.tinymud") t t)
  (tinymud-change-world))

(defmacro get-conn-by-field (field value)
  (` (pred-match '(lambda (arg) (eq (get-value arg (, field)) (, value)))
		 tinymud-connection-list)))

(defun tinymud-filter (process output)
  (let ((conn (get-conn-by-field 'process process)))
    (if conn
	(progn
	  (setq output
		(concat (get-value conn 'output) output))
	  (let ((start 0))
	    (while (string-match "\r?\n" output start)
	      (let ((line (substring output start (match-beginning 0))))
		(setq start (match-end 0))
		(filter-line conn line)))
	    (setf (get-value conn 'output)
		  (substring output start)))))))

(defun check-triggers (conn line trig)
  (if trig
      (progn
	(if (string-match (car (car trig)) line)
	    (if (get-value conn 'debug)
		(tinymud-apply-macro conn (cdr (car trig))
				     (match-data-list line))
	      (condition-case err
		(tinymud-apply-macro conn (cdr (car trig))
				     (match-data-list line))
		(error (message "Trigger raised error: %s" err)))))
	(check-triggers conn line (cdr trig)))))

(defun match-data-list (line)
  (let ((cnt 0)
	(ret nil))
    (while (match-beginning cnt)
      (setq ret (cons (substring line
				 (match-beginning cnt)
				 (match-end cnt))
		      ret)
	    cnt (1+ cnt)))
    (nreverse ret)))

(defun filter-line (conn line)
  "Filter connection CONN output LINE.
Calls \"tinymud-filter-line-hook\" iff its value is non-nil.
Does trigger checking; triggers may set the variable \"handle\" to the
following values:

normal:		display this line normally.
zero:		display this line in the echo area and beep.
nonzero:	all of the above."
  (let ((handle 'normal))
    (if (not (and tinymud-filter-line-hook
		  (funcall tinymud-filter-line-hook conn line)))
	(progn
	  (if (not (get-value conn 'trigger-disable))
	      (progn
		(check-triggers conn line
				(get-value conn 'triggers))
		(check-triggers conn line
				(get-value (get-value conn 'world) 'triggers))
		(check-triggers conn line
				tinymud-global-triggers)))
	  (and handle (display-line conn line))
	  (and (numberp handle)
	       (progn
		 (or (zerop handle) (beep handle))
		 (message "%s" line)))))))

(defun tinymud-gag-this (conn args)
  "Gags the current line.  Should only be called from a trigger."
  (setq handle nil))

(defun my-fill ()
  (move-to-column (1+ fill-column))
  (skip-chars-backward "^ \n")
  (if (bolp) (skip-chars-forward "^ \n"))
  (if (not (eolp))
      (progn
	(skip-chars-backward " ")
	(insert ?\n)
	(delete-horizontal-space)
	(and fill-prefix (insert fill-prefix))
	t)
    nil))

(defun display-line (conn line)
  "Displays LINE in CONN's output buffer.  Does word wrap if
enabled for this connection."
  (save-excursion
    (set-buffer (get-value conn 'buffer))
    (goto-char (point-max))
    (insert line)
    (while (progn
	     (end-of-line)
	     (and (get-value conn 'auto-fill)
		  (> (current-column) fill-column)		  
		  (my-fill))))
    (insert "\n")))

(defun magic-login (conn)
  "Does a TinyMUD style auto-connection."
  (let ((char (get-local-binding conn 'character))
	(pwd  (get-local-binding conn 'password)))
    (if (and char pwd)
	(process-send-string (get-value conn 'process)
			     (format "\nconnect %s %s\n"
				     char pwd)))))

(defun pred-match (fun l)
  "Returns the first element of L for which FUN returns non-nil, or
nil if none do."
  (while (and l (not (funcall fun (car l))))
    (setq l (cdr l)))
  (car l))

(defun conditionally-connect (conn)
  (let ((proc (get-value conn 'process))
	(world (get-value conn 'world)))
    (if (and proc
	     (processp proc)
	     (memq (process-status proc) '(run open)))
	t
      (if (and proc
	       (processp proc))
	  (delete-process proc))
      (condition-case err
	  (setf (get-value conn 'process)
		(open-network-stream (get-value world 'name)
				     (get-value conn 'ibuffer)
				     (get-value world 'address)
				     (get-value world 'port)))
	(error (progn
		 (message "%s" err)
		 (sit-for 2)
		 (message "Connection to world %s (%s:%d) failed."
			  (get-value world 'name)
			  (get-value world 'address)
			  (get-value world 'port)))))
      (if (setq proc (get-value conn 'process))
	  (progn
	    (set-process-filter proc 'tinymud-filter)
	    (set-process-sentinel proc 'tinymud-sentinel)))
      proc)))

(defun connect-to (world)
  "Attempts to make connection to WORLD."
  (let ((wspec
	 (if (stringp world) (get-world world) world)))
    (if (null wspec)
	(progn
	(message "Don't know about world %s" world)
	nil)
      (let ((newcon (get-conn-by-field 'world wspec)))
	(if (null newcon)
	    (progn
	      (setq newcon (intern (get-value world 'name)
				   tinymud-connections))
	      (if tinymud-connection-list
		  (setf (cdr (last tinymud-connection-list)) (list newcon))
		(setf tinymud-connection-list (list newcon)))
	      (set-buffer
	       (setf (get-value newcon 'buffer)
		     (generate-new-buffer
		      (concat (get-value wspec 'name) "-output"))))
	      (tinymud-output-mode)
	      (set-buffer
	       (setf (get-value newcon 'ibuffer)
		     (generate-new-buffer
		      (get-value wspec 'name))))
	      (tinymud-mode)
	      (setq scroll-step 1)
	      (setf (get-value newcon 'world) wspec)
	      (setf (get-value newcon 'auto-fill) t)))
	(if (processp (conditionally-connect newcon))
	    (magic-login newcon))
	(set-up-tinymud-windows newcon)))))

(defun disconnect (conn)
  "Closes the current tinymud connection.
Calls the tinymud macro \" disconnect\" if it is defined, before
closing the MUD connection, to allow sending a string or performing
cleanups before disconnect."
  (if (processp (get-value conn 'process))
      (let* ((world (get-value conn 'world))
	     (def (get-macro-def conn " disconnect")))
	(if (and def
		 (memq (process-status (get-value conn 'process)) '(open run)))
	    (tinymud-apply-macro conn def nil))
	(delete-process (get-value conn 'process)))))

(defun tinymud-sentinel (proc change)
  (or (memq (process-status proc) '(open run))
      (let ((conn (get-conn-by-field 'process proc)))
	(beep t)
	(message "Lost connection in %s."
		 (get-value (get-value conn 'world) 'name))
	(condition-case nil
	    (display-line conn
			  (concat 
			   "\nProcess " (process-name proc) " " change))))))

(defun tinymud-mode () "Major mode for sending tinymud commands.
\\{tinymud-mode-map}
Variables:
  tinymud-default-world
    A non-nil value will be provided as a default for \\[tinymud-change-world].

  tinymud-mode-hook
    Called whenever a new tinymud input buffer is created."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tinymud-mode-map)
  (setq mode-name "Tinymud")
  (setq major-mode 'tinymud-mode)
  (set-syntax-table (standard-syntax-table))
  (make-local-variable 'scroll-step)
  (setq mode-line-format '("--" mode-line-buffer-identification "   " global-mode-string "   %[(" mode-name minor-mode-alist "%n: %s)%]----" (-3 . "%p") "-%-"))
  (local-set-key "\C-i" 'tinymud-scroll)
  (local-set-key "\M-\C-i" 'tinymud-reverse-scroll)
  (local-set-key "\C-m" 'tinymud-newline)
  (local-set-key "\C-c\C-y" 'tinymud-send-kill)
  (local-set-key "\C-c\C-f" 'tinymud-toggle-auto-fill)
  (local-set-key "\C-cd" 'tinymud-define-global-macro)
  (local-set-key "\C-cg" 'tinymud-global-gag)
  (local-set-key "\C-cq" 'tinymud-quit)
  (local-set-key "\C-cw" 'tinymud-change-world)
  (local-set-key "\C-co" 'switch-to-output-buffer)
  (local-set-key "\C-ca" 'add-world)
  (local-set-key "\C-ce" 'tinymud-end-of-buffer)
  (local-set-key "\C-cu" 'tinymud-delete-input)
  (local-set-key "\C-c\C-v" 'tinymud-trigger-panic)
  (local-set-key "\M-n" 'tinymud-next-conn)
  (local-set-key "\M-p" 'tinymud-previous-conn)
  (local-set-key "\C-ck" 'tinymud-kill-conn)
  (run-hooks 'tinymud-mode-hook))

(defun tinymud-trigger-panic (arg)
  (interactive "P")
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (message "Triggers %s for this connection."
	     (if (setf (get-value conn 'trigger-disable) (null arg))
		 "disabled" "enabled"))))

(defun tinymud-output-mode ()
  "Major mode for browsing the output window.  Normal editing commands
are disabled.
\\{tinymud-output-mode-map}
Variables:
  tinymud-output-mode-hook
    Called whenever a new tinymud output buffer is created."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tinymud-output-mode-map)
  (setq mode-name "Tinymud Output")
  (setq major-mode 'tinymud-output-mode)
  (set-syntax-table (standard-syntax-table))
  (setq mode-line-format '("--" mode-line-buffer-identification "   " global-mode-string "   %[(" mode-name minor-mode-alist "%n)%]----" (-3 . "%p") "-%-"))
  (make-local-variable 'scroll-step)
  (setq fill-column 79)
  (suppress-keymap tinymud-output-mode-map)
  (local-set-key "\C-i" 'tinymud-scroll)
  (local-set-key "\M-\C-i" 'tinymud-reverse-scroll)
  (local-set-key "\C-co" 'switch-to-input-buffer)
  (run-hooks 'tinymud-output-mode-hook))

(defun show-buffer-somewhere (buf)
  "Shows buffer BUF in some window.  If only one window exists it
splits the current window."
  (interactive "bBuffer: ")
  (let ((win (get-buffer-window buf)))
    (if (not win)
	(progn
	  (if (one-window-p)
	      (split-window))
	  (set-window-buffer (setq win (get-lru-window)) buf)))
    win))

(defun switch-to-input-buffer ()
  "Shows the current input buffer using show-buffer-somewhere."
  (interactive)
  (let ((conn (get-conn-by-field 'buffer (current-buffer))))
    (if conn
	(select-window (show-buffer-somewhere (get-value conn 'ibuffer)))
      (message "Current buffer has no associated input buffer."))))

(defun switch-to-output-buffer ()
  "Shows the current output buffer using show-buffer-somewhere."
  (interactive)
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (if conn
	(select-window (show-buffer-somewhere (get-value conn 'buffer)))
      (message "Current buffer has no associated output buffer."))))

(defun tinymud-toggle-auto-fill (arg)
  "Sets auto fill for the current connection based on ARG.
If ARG is negative or \"-\" turns auto fill off.
If ARG is positive turns auto fill on.
No argument toggles current value."
  (interactive "P")
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (if conn
	(message "Auto fill mode now %s."
		 (if (setf (get-value conn 'auto-fill)
			   (cond ((eq '- arg) nil)
				 ((and (numberp arg) (> arg 0)) t)
				 ((and (numberp arg) (< arg 0)) nil)
				 ((null arg)
				  (not (get-value conn 'auto-fill)))
				 ((listp arg) t)))
		     "on" "off"))
      (message "Current buffer has no connection"))))

(defun tinymud-send-kill ()
  "Sends the last stretch of killed text to the current connection
\(without macro expansion\)."
  (interactive)
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (if conn
	(process-send-string (get-value conn 'process)
			     (car kill-ring))
      (message "Current buffer has no connection"))))

(defun tinymud-newline ()
  "Sends current input command to the current connection.  Does macro
expansion." 
  (interactive)
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (if conn
	(progn
	  (end-of-line)
	  (or (eobp)
	      (progn
		(let* ((b (progn (beginning-of-line) (point)))
		       (e (progn (end-of-line) (point))))
		  (goto-char (point-max))
		  (or (bolp)
		      (insert "\n"))
		  (insert-buffer-substring (current-buffer) b e))))
	  (let ((b (progn (beginning-of-line) (point)))
		(e (progn (end-of-line) (insert "\n") (point))))
	    (tinymud-send-command conn (buffer-substring b e))))
      (message "Current buffer has no connection."))))

(defun tinymud-delete-input ()
  "Delete the current line in the current tinymud input buffer."
  (interactive)
  (delete-region (progn (end-of-line) (point))
		 (progn (beginning-of-line) (point))))

(defun tinymud-end-of-buffer (arg)
  "Go to end of current tinymud output buffer.
If ARG is non-nil, go to beginning of output buffer instead."
  (interactive "P")
  (let ((oldwin (selected-window)))
    (select-window (show-buffer-somewhere
		    (get-value (get-conn-by-field 'ibuffer (current-buffer))
			       'buffer)))
    (if arg
	(goto-char (point-min))
      (goto-char (point-max))
      (recenter 2))
    (select-window oldwin)))

(defun tinymud-send-command (conn str)
  (setq str (tinymud-expand-command conn str))
  (cond
   ((string-match "^/\\(\[^/ \t\n]+\\)\\s *\\(.*\\)\n" str)
    (let* ((name (substring str (match-beginning 1) (match-end 1)))
	   (argstr (substring str (match-beginning 2) (match-end 2)))
	   (def (get-macro-def conn name)))
      (if def
	  (tinymud-apply-macro conn def argstr)
	(message "No macro %s defined" name))))
   ((string-match "^//" str)
    (raw-send-command conn (substring str 1)))
   (t (raw-send-command conn str))))

(defun raw-send-command (conn str)
  (let ((cnt 0))
    (if (get-value conn 'partial-input)
	(setq str (concat (get-value conn 'partial-input) str)))
    (while (string-match "\n" str)
      (process-send-string (get-value conn 'process)
			   (substring str 0 (match-end 0)))
      (setq str (substring str (match-end 0)))
      (if (= (incf cnt) 10) (progn (setq cnt 0) (sit-for 1))))
    (setf (get-value conn 'partial-input) str)))

(defun split-words (str &optional max)
  (let ((ret nil)
	(start 0))
    (while (and (or (not max) (< start max))
		(string-match "[^ ]+" str start))
      (setq ret (append ret (list (substring str
					     (match-beginning 0)
					     (match-end 0))))
	    start (match-end 0)))
    ret))

(defun tinymud-expand-command (conn cmd)
  (let ((start 0)
	(res ""))
    (while (string-match "&" cmd start)
      (let ((newstart (match-beginning 0)))
	(setq res (concat res (substring cmd start newstart)))
	(setq start (1+ newstart))
	(let ((chr (aref cmd (1+ newstart))))
	  (cond
	   ((eq chr ?&)
	    (setq res (concat res "&"))
	    (incf start))
	   ((eq chr ?\[)
	    (let ((tmp (read-from-string cmd (+ 2 newstart))))
	      (princ (eval (car tmp))
		     '(lambda (x)
			(setq res (concat res (char-to-string x)))))
	      (setq start (cdr tmp))
	      (if (eq (aref cmd start) ?\])
		  (incf start))))
	   (t (setq res (concat res "&")))))))
    (concat res (substring cmd start) "\n")))

(defun tinymud-apply-string-macro (conn def arg)
  (let ((args) (argstr))
    (cond ((stringp arg) (setq argstr arg args (split-words arg)))
	  ((listp arg) (setq argstr "" args arg)))
    (let ((start 0))
      (while (string-match "%" def start)
	(let ((newstart (match-beginning 0)))
	  (raw-send-command conn (substring def start newstart))
	  (setq start (1+ newstart))
	  (let ((chr (aref def (1+ newstart))))
	    (cond
	     ((eq chr ?%)
	      (raw-send-command conn "%")
	      (incf start))
	     ((and (<= ?0 chr) (>= ?9 chr)) ;; macro args, 0-9
	      (raw-send-command conn (nth (- chr ?0) args))
	      (incf start))
	     ((eq chr ?-)
	      (setq chr (aref def (incf start)))
	      (if (and (<= ?0 chr) (>= ?9 chr))
		  (progn
		    (raw-send-command
		     conn (mapconcat 'concat (nthcdr (- chr ?0) args) " "))
		    (incf start))
		(raw-send-command "%")))
	     ((eq chr ?*)
	      (raw-send-command conn argstr)
	      (incf start))
	     ((eq chr ?\[)
	      (let ((tmp (read-from-string def (+ 2 newstart))))
		(princ (eval (car tmp))
		       '(lambda (x)
			  (raw-send-command conn (char-to-string x))))
		(setq start (cdr tmp))
		(if (eq (aref def start) ?\])
		    (incf start))))
	     (t (raw-send-command conn "%"))))))
      (raw-send-command conn (substring def start))
      (raw-send-command conn "\n"))))

(defun tinymud-apply-macro (conn def arg)
  "Expands macro defined by DEF using ARG as macro arguments."
  (cond
   ((stringp def) (tinymud-apply-string-macro conn def arg))
   ((or (and (listp def) (eq 'lambda (car def)))
	(and (symbolp def) (fboundp def)))
    (apply def (list conn arg)))
   (t (eval def))))

(defun tinymud-change-world (&optional world)
  "Changes current world to WORLD.  If WORLD is not given
it will prompt for a world.  A new connection will be created if none
exists and a reconnect will be initiated if an existing connection is
closed."
  (interactive)
  (connect-to
   (or world
       (get-world
	(let* ((completion-ignore-case t))
	  (completing-read "World: "
			   tinymud-worlds
			   nil t
			   tinymud-default-world))))))

(defmacro define-macro (env name def)
  "Add or replace a macro NAME with definition DEF in environment ENV.
If DEF is nil then delete the definition of NAME.  Also works for triggers."
  (` (let ((tmp (assoc (, name) (, env))))
       (cond
	((and (, def) tmp) (rplacd tmp (, def)))
	((, def) (setf (, env) (cons (cons (, name) (, def)) (, env))))
	(tmp (setf (, env) (delq tmp (, env))))))))

(defun get-macro-def (conn name)
  (or (cdr (assoc name (get-value conn 'macros)))
      (cdr (assoc name (get-value (get-value conn 'world) 'macros)))
      (cdr (assoc name tinymud-global-macros))))

(defun tinymud-define-world-macro (world name definition)
  "Set a macro local to WORLD named NAME to DEFINITION.
See \"tinymud-define-global-macro\" for details."
  (interactive "sWorld: \nsName: \nsDefinition: ")
  (if (stringp world) (setq world (get-world world)))
  (define-macro (get-value world 'macros) name definition))

(defun tinymud-define-conn-macro (conn name definition)
  "Set a macro local to CONN named NAME to DEFINITION.
See \"tinymud-define-global-macro\" for details."
  (define-macro (get-value conn 'macros) name definition))

(defun tinymud-define-global-macro (name definition)
  "Define a global macro NAME to be DEFINITION.
DEFINITION may be a string containing % escapes,
a symbol or a lambda form to be funcalled, a list to be evalled,
or nil, in which case the macro is deleted from the list.

Valid % escapes are:
%n	where n is a number between 0 and 9
%*	evaluates to all the macro arguments
%%	a literal \`%\'

Functions and lambdas get the arguments CONN ARGS, where CONN
is the connection this macro is executing on, and ARGS is the command line
for macros and the list of match strings for triggers."
  (interactive "sMacro: \nsDefinition: ")
  (define-macro tinymud-global-macros name definition))

(defun tinymud-global-gag (name)
  "Gag all output lines containing regexp NAME.
This just calls \"tinymud-global-trigger\" with tinymud-gag-this as RESPONSE."
  (interactive "sRegexp to gag: ")
  (tinymud-global-trigger name 'tinymud-gag-this))

(defun tinymud-world-gag (world name)
  "Gag all output lines containing regexp NAME.
This just calls \"tinymud-world-trigger\" with tinymud-gag-this as RESPONSE."
  (interactive "sWorld: \nsRegexp to gag: ")
  (if (stringp world) (setq world (get-world world)))
  (tinymud-world-trigger world name 'tinymud-gag-this))

(defun tinymud-conn-gag (conn name)
  "Gag all output lines containing regexp NAME.
This just calls \"tinymud-conn-trigger\" with tinymud-gag-this as RESPONSE."
  (tinymud-conn-trigger conn name 'tinymud-gag-this))

(defun tinymud-world-trigger (world regexp response)
  "Define a response local to WORLD, to be called if an output line
matches REGEXP, and send RESPONSE.  RESPONSE is a valid macro
definition.  See \"tinymud-define-global-macro\" for valid response
forms."
  (if (stringp world) (setq world (get-world world)))
  (define-macro (get-value world 'triggers) regexp response))

(fset 'tinymud-local-trigger 'tinymud-world-trigger)
(fset 'tinymud-local-gag ' tinymud-world-gag)
(fset 'tinymud-define-local-macro 'tinymud-define-world-macro)
      
(defun tinymud-conn-trigger (conn regexp response)
  "Define a response local to CONN, to be called if an output line
matches REGEXP, and send RESPONSE.  RESPONSE is a valid macro
definition.  See \"tinymud-define-global-macro\" for valid response
forms."
  (define-macro (get-value conn 'triggers) regexp response))

(defun tinymud-global-trigger (regexp response)
  "Define a global response to lines matching REGEXP, and send
RESPONSE as a macro definition, as outlined in
\"tinymud-define-global-macro\"."
  (define-macro tinymud-global-triggers regexp response))

(defun get-world (world)
  (pred-match '(lambda (x) (string= (downcase world)
				    (downcase (get-value x 'name))))
	      tinymud-worlds-list))

(defun tinymud-quit ()
  (interactive)
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer))))
    (if conn
	(disconnect conn)
      (message "Current buffer has no connection."))))

(defun tinymud-scroll (arg)
  (interactive "P")
  (and (listp arg)
       (setq arg (car arg)))
  (let ((oldwin (selected-window))
	(conn (or
	       (get-conn-by-field 'ibuffer (current-buffer))
	       (get-conn-by-field 'buffer (current-buffer)))))
    (select-window (show-buffer-somewhere
		    (get-value conn 'buffer)))
    (condition-case nil
	(scroll-up arg)
      (error (message "End of buffer.")))
    (select-window oldwin)))

(defun tinymud-reverse-scroll (arg)
  (interactive "P")
  (and (listp arg)
       (setq arg (car arg)))
  (let ((oldwin (selected-window))
	(conn (or
	       (get-conn-by-field 'ibuffer (current-buffer))
	       (get-conn-by-field 'buffer (current-buffer)))))
    (select-window (show-buffer-somewhere
		    (get-value conn 'buffer)))
    (condition-case nil
	(scroll-down arg)
      (error (message "Beginning of buffer.")))
    (select-window oldwin)))

(defun tinymud-next-conn (arg)
  (interactive "p")
  (setq arg (mod arg (length tinymud-connection-list)))
  (let ((conn (get-conn-by-field 'ibuffer (current-buffer)))
	(newc tinymud-connection-list))
    (while (and newc (not (eq (car newc) conn)))
      (setq newc (cdr newc)))
    (while (plusp arg)
      (setq newc (or (cdr newc) tinymud-connection-list)
	    arg (1- arg)))
    (set-up-tinymud-windows (car newc))))

(defun tinymud-previous-conn (arg)
  (interactive "p")
  (tinymud-next-conn (- arg)))

(defun tinymud-kill-conn (&optional conn)
  (interactive)
  (if (not conn)
      (setq conn (get-conn-by-field 'ibuffer (current-buffer))))
  (tinymud-next-conn 1)
  (disconnect conn)
  (kill-buffer (get-value conn 'buffer))
  (kill-buffer (get-value conn 'ibuffer))
  (setq tinymud-connection-list (delq conn tinymud-connection-list))
  (setplist conn nil))

(defun set-up-tinymud-windows (conn)
  "Set up windows to display the input and output buffer windows for
the current connection."
  (let ((iwin (or (get-buffer-window (get-value conn 'ibuffer))
		  (selected-window)))
	(owin (or (get-buffer-window (get-value conn 'buffer))
		  (get-lru-window))))
    (if (eq iwin owin) 
	(if (one-window-p)
	    (progn
	      (split-window (selected-window) (/ (* (window-height) 8) 10))
	      (setq owin (selected-window))
	      (setq iwin (progn (other-window 1) (selected-window))))
	  (setq owin (next-window owin))))
    (set-window-buffer owin (get-value conn 'buffer))
    (set-window-buffer iwin (get-value conn 'ibuffer))
    (select-window iwin)))

;;; Local variables:
;;; compile-command: "emacs -batch -l tinytalk.el -f batch-byte-compile tinytalk.el"
;;; End:

