;;; clisp.el establishes a set of key bindings and functions to support
;;; a Common Lisp running in an inferior shell.

;;; To use, set your lisp-mode-hook to:
;;;       (lambda () (require 'clisp)(start-lisp))
;;; If you want your first lisp started on a different host, use:
;;;       (lambda () (require 'clisp)(start-lisp "hostname"))

;;; There are two sets of key bindings established, one for editing
;;; lisp code and the other for interacting with the lisp listener.
;;; Both sets of bindings are available via the ^C prefix.

;;; Editing any file with in lisp mode will cause an inferior lisp to
;;; be started automatically.

;;; While editing a file in Lisp mode:
;;;   C-c l    switches to inferior lisp process (see C-c e)
;;;   M-C-l    spawns a new lisp buffer, prompting for a host.
;;; Passing code from GNU to lisp:
;;;   C-c d    evals current defun in inferior lisp process
;;;   C-c C-d  = (C-c d) + (C-c l)
;;;   C-c c    compiles current defun in inferior lisp process
;;;   C-c C-c  = (C-c c) + (C-c l)
;;;   C-c s    evals last sexpr in inferior lisp process
;;;   C-c C-s  = (C-c s) + (C-c l)
;;;   C-c r    evals current region in inferior lisp process
;;;   C-c C-r  = (C-c r) + (C-c l)
;;;   C-c b    evals current buffer in inferior lisp process
;;;   C-c C-b  = (C-c b) + (C-c l)
;;; Tags for cross-indexing source code:
;;;   C-c .    finds defun for current function in other window
;;;   C-c ,    looks for next matching defun (C-c .)
;;;   M-.      finds defun for current function (std GNU)
;;;   M-,      looks for next matching defun (std GNU)
;;;   C-c t    lists files indexed by (C-c .)
;;;   C-c C-t  recomputes lookup table for (C-c .) and (C-c t)
;;; Special lisp support:
;;;   C-c m    shows Common Lisp macro expansion of current form
;;;   C-c f    shows Common Lisp documentation for current function
;;;   C-c v    shows Common Lisp documentation for current variable
;;;   M-q      reindents current comment or defun

;;; While running in the inferior lisp:
;;;   C-c e    returns to last edited file of lisp code (see C-c l)
;;;   M-C-l    spawns a new lisp buffer, prompting for a host.
;;;   C-c l    with a prefix argument switches to that inferior lisp.
;;; In addition, all of the inferior shell mode commands are active.
;;; The more useful ones are:
;;;   C-c h    show history
;;;   C-c C-p  previous form in history list
;;;   C-c C-n  next form in history list
;;;   C-c C-a  beginning of line
;;;   C-c C-r  search backwards in history
;;;   C-c C-s  search forward in history

;;; The "[" and "]" characters can be used as "super-parens" in either
;;; mode. To insert explicit square brackets, they must be prefaced by
;;; C-q.

;;; Authors: Alberto Segre (segre@gvax.cs.cornell.edu)
;;;          David Hubbell (hubbell@svax.cs.cornell.edu)
;;;          Department of Computer Science
;;;          Cornell University
;;;          Upson Hall
;;;          Ithaca, NY  14853-7501

;;; Copyright (c) 1988 Alberto M. Segre, David L. Hubbell

;;; A portion of this code was adapted from code originally in the GNU
;;; distribution in file simple.el

;;; A portion of this code was adapted from code originally written by
;;; Wolfgang Rupprecht (wolfgang@mgm.mit.edu), as modified by David
;;; Hubbell (hubbell@svax.cs.cornell.edu).

;;; Copying is permitted under those conditions described by the GNU
;;; Emacs General Public License as clarified 11 February 1988, which
;;; is incorporated here by reference.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)
(require 'backquote)
(provide 'clisp)

;;; Set the Common Lisp to be the lisp that's run as an inferior
;;; process by shell.el. Any Common Lisp will do; we'll set the
;;; default to be Kyoto Common Lisp. Non-Common Lisps will not support
;;; macro expansion and documentation search. This variable is defvar'd 
;;; in shell.el

;;; Note that there is no need to set inferior-lisp-load-command since
;;; we're avoiding the use of /tmp to pass junk back to lisp.

(setq inferior-lisp-program "/usr/u/cap/bin/kcl")

;;; Inferior lisp prompt as in shell.el. The default value is
;;; "^[A-Za-z*]*>+" which handles an atomic package name before the
;;; ">", as in KCL.  The appropriate string regexp for Allegro Common
;;; Lisp seems to be "^\\(\\[[0-9]+\\] \\)?<cl> ", since the break
;;; level appears as "[n]" before the prompt.

(setq inferior-lisp-prompt "^[A-Za-z*]*>+")

;;; Remote shell program; used for starting a remote inferior lisp.

(defvar *remote-shell-program* "/usr/ucb/rsh"
  "The program that starts a remote shell.")

;;; File containing the motd. Will be displayed at the top of the
;;; first buffer running an inferior lisp.

(defvar *clisp-motd-file* "/usr/u/cap/.motd"
  "The filename for the message of the day to be displayed in
the first lisp buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup function called on lisp-mode-hook. Note that the lisp
;;; process should only be started one time, hence we alter
;;; lisp-mode-hook to ensure this. Fill-prefix is set to enable ESC-q
;;; to work properly on comment lines.

(defun start-lisp (&optional host) "Called by lisp-mode-hook to start lisp."
       (save-excursion
	 (or (get-process "lisp")
	     (progn (message "Starting lisp...")
		    (start-lisp-process-and-buffer "*lisp*" host)
;;; Create the history list variables for the lisp process.
		    (make-local-variable 'shell-history-list)
		    (make-local-variable 'shell-history-index)
		    (make-local-variable 'shell-last-search)
;;; Inferior Lisp mode key bindings
;;; Get rid of M-C-x from lisp-edit.el
		    (define-key inferior-lisp-mode-map "\M-\C-x" nil)
;;; Get rid of C-c C-y from shell.el.  C-c C-p is better.
		    (define-key inferior-lisp-mode-map "\C-c\C-y" nil)
		    (define-key inferior-lisp-mode-map "\C-ce" 
		      'clisp-buffer-deselect)
		    (define-key inferior-lisp-mode-map "\C-cl" 
		      'clisp-buffer-select)
		    (define-key inferior-lisp-mode-map "\M-\C-l" 
		      'clisp-create-lisp-buffer)
;;; Make sure RET doesn't send input to Lisp unless it's
;;; an s-expression.
		    (define-key inferior-lisp-mode-map "\C-m"
		      'clisp-shell-send-input-if-sexpr)
		    (define-key inferior-lisp-mode-map "\C-c\C-p"
		      'clisp-shell-previous-command)
		    (define-key inferior-lisp-mode-map "\C-c\C-n"
		      'clisp-shell-next-command)
		    (define-key inferior-lisp-mode-map "\C-c\C-r"
		      'clisp-shell-history-search-backward)
		    (define-key inferior-lisp-mode-map "\C-c\C-s"
		      'clisp-shell-history-search-forward)
		    (define-key inferior-lisp-mode-map "\C-ch"
		      'clisp-shell-list-history)
;;; SHOW-OUTPUT-FROM-SHELL must be rebound because
;;; it was originally attached to C-c C-r.
		    (define-key inferior-lisp-mode-map "\C-c["
		      'show-output-from-shell)
;;; Lisp editing mode key bindings
;;; Get rid of M-C-x from lisp-edit.el
		    (define-key lisp-mode-map "\M-\C-x"  nil)
;;; Make "[" a kind of open paren so that scan-sexps
;;; won't ignore it.
		    (modify-syntax-entry 91 "(")
;;; Install superparen as "]" in lisp mode and in inferior
;;; lisp process.
		    (define-key inferior-lisp-mode-map "]"    
		      'super-close-paren)
		    (define-key lisp-mode-map "]"    
		      'super-close-paren)
;;; Subsumes fill-paragraph.
		    (define-key lisp-mode-map "\M-q"  
		      'clisp-reindent-form)
		    (define-key lisp-mode-map "\C-cl"  
		      'clisp-buffer-select)
		    (define-key lisp-mode-map "\M-\C-l" 
		      'clisp-create-lisp-buffer)
		    (define-key lisp-mode-map "\C-cd"  
		      'clisp-eval-defun)
		    (define-key lisp-mode-map "\C-c\C-d" 
		      'clisp-eval-defun-and-go)
		    (define-key lisp-mode-map "\C-cc"  
		      'clisp-compile-defun)
		    (define-key lisp-mode-map "\C-c\C-c" 
		      'clisp-compile-defun-and-go)
		    (define-key lisp-mode-map "\C-cs"  
		      'clisp-eval-last-sexpr)
		    (define-key lisp-mode-map "\C-c\C-s" 
		      'clisp-eval-last-sexpr-and-go)
		    (define-key lisp-mode-map "\C-cr"  
		      'clisp-eval-region)
		    (define-key lisp-mode-map "\C-c\C-r" 
		      'clisp-eval-region-and-go)
		    (define-key lisp-mode-map "\C-cb"  
		      'clisp-eval-buffer)
		    (define-key lisp-mode-map "\C-c\C-b" 
		      'clisp-eval-buffer-and-go)
		    (define-key lisp-mode-map "\C-c."  
		      'find-tag-other-window)
		    (define-key lisp-mode-map "\C-c,"  
		      'tags-loop-continue)
		    (define-key lisp-mode-map "\C-ct"  
		      'clisp-list-tag-files)
		    (define-key lisp-mode-map "\C-c\C-t" 
		      'clisp-recompute-tag-table)
		    (define-key lisp-mode-map "\C-cm"  
		      'clisp-show-macro-expansion)
		    (define-key lisp-mode-map "\C-cf"  
		      'clisp-show-function-documentation)
		    (define-key lisp-mode-map "\C-cv"
		      'clisp-show-variable-documentation)
;;; Set the lisp-indent-hook to a function that recognizes
;;; Common Lisp forms.
		    (setq lisp-indent-hook 'common-lisp-indent-hook)
		    (message "Starting lisp...done.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables.

(defvar *last-lisp-buffer* nil
  "The last Lisp process buffer that the user selected, NOT its name.")

(defvar *last-edit-buffer* nil
  "The last edit (non-Lisp) buffer that the user selected.")

(defvar *max-lisp-buffer-number* 1
  "The number of the last Lisp buffer created.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MARK-EDIT-BUFFER updates the pointer to the last edit buffer
;;; selected unless the buffer is a lisp process buffer.

(defun mark-edit-buffer (buffer)
  (cond ((not (equal (substring (buffer-name buffer) 0 5) "*lisp"))
	 (setq *last-edit-buffer* buffer))))

;;; MARK-LISP-BUFFER updates the pointer to the last edit buffer
;;; selected only when the buffer is a lisp process buffer.

(defun mark-lisp-buffer (buffer)
  (cond ((equal (substring (buffer-name buffer) 0 5) "*lisp")
	 (setq *last-lisp-buffer* buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Select the Lisp process buffer.

(defun clisp-buffer-select (&optional buffernum)
  "Select the Lisp process buffer.
If the optional argument is n, select Lisp process buffer n.
If that buffer does not exist, print an error message and give up.
If there is no optional argument, select the last Lisp buffer
selected."
  (interactive "P")
  (let ((buffer-to-select
	 (cond ((not (integerp buffernum)) *last-lisp-buffer*)
	       ((= buffernum 1) (get-buffer "*lisp*"))
	       ((<= buffernum *max-lisp-buffer-number*)
		(get-buffer (format "*lisp%d*" buffernum))))))
    (cond (buffer-to-select
	   (mark-edit-buffer (current-buffer))
	   (mark-lisp-buffer buffer-to-select)
	   (switch-to-buffer buffer-to-select)
	   (goto-char (point-max)))
	  (buffernum
	   (beep)
	   (message
	    (format "Lisp buffer *lisp%d* does not exist." buffernum)))
	  (t (beep)
	     (message "No lisp buffer.")))))

;;; Return to previous edit buffer from Lisp process buffer.

(defun clisp-buffer-deselect ()
  "Return to previous edit buffer from Lisp process buffer."
  (interactive)
  (cond ((buffer-name *last-edit-buffer*)
	 (mark-lisp-buffer (current-buffer))
	 (switch-to-buffer *last-edit-buffer*))
	(t (beep)
	   (message "The last edit buffer has been killed."))))

;;; Create and switch to a new Lisp process buffer with the Lisp process
;;; running on the hostname entered.  If none is entered, use the local
;;; host.

(defun clisp-create-lisp-buffer (entered-hostname)
  "Create and switch to a new Lisp process buffer."
  (interactive "sHost name (return for local host):")
  (message "Starting new lisp process...")
  (setq *max-lisp-buffer-number* (1+ *max-lisp-buffer-number*))
  (let ((newbuffername (format "*lisp%d*" *max-lisp-buffer-number*)))
    (mark-edit-buffer (current-buffer))
    (save-excursion
      (cond ((equal entered-hostname "")
	     (start-lisp-process-and-buffer newbuffername))
	    (t (start-lisp-process-and-buffer 
		newbuffername entered-hostname))))
    (clisp-buffer-select *max-lisp-buffer-number*)
    (message "Starting new lisp process...done.")))

;;; Start up lisp process in a new *lispN* buffer unless the process
;;; already exists.  If a hostname was passed, start the Lisp process
;;; on that machine by invoking *remote-shell-program* on 3 arguments:
;;; a startup-file for make-shell, a hostname, and the
;;; inferior-lisp-program.

(defun start-lisp-process-and-buffer (buffername &optional hostname)
  (let ((processname (substring buffername
				1 (1- (length buffername)))))
    (cond ((not (get-process processname))
	   (cond (hostname
		  (switch-to-buffer
		   (make-shell processname
			       *remote-shell-program*
			       nil
			       hostname
			       inferior-lisp-program))
		  (insert
		   (format
		    "Remote Lisp Host %s\n\n" (upcase hostname))))
		 (t (switch-to-buffer
		     (make-shell processname inferior-lisp-program))))
	   (mark-lisp-buffer (get-buffer buffername))
	   (inferior-lisp-mode)
	   (set-process-filter (get-process processname)
			       (` (lambda (cls-proc cls-string)
				    (clisp-startup-filter
				     cls-proc cls-string
				     (, (get-buffer buffername))))))
	   (set-process-buffer (get-process processname) 
			       (get-buffer buffername))
	   (buffer-flush-undo (get-buffer buffername))
	   (setq last-input-start (make-marker))
	   (setq last-input-end (make-marker))
	   (process-kill-without-query (get-process processname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sends current buffer to current Lisp process and evals it.

(defun clisp-eval-buffer () 
  "Sends the current buffer to the current Lisp process and evals it."
  (interactive)
  (message "Evaling buffer...")
  (save-excursion
    (goto-char (point-max))
    (let ((end (point))) 
      (goto-char (point-min))
      (process-send-region
       (clisp-buffer-to-process-name *last-lisp-buffer*) (point) end) 
      (process-send-string
       (clisp-buffer-to-process-name *last-lisp-buffer*) "\n")
      (message "Evaling buffer...done."))))

;;; Sends current buffer to current Lisp process, evals it, and
;;; switches to lisp buffer.

(defun clisp-eval-buffer-and-go ()
  "Send the current buffer to the current Lisp process, evals it, and switches 
to lisp buffer."
  (interactive)
  (clisp-eval-buffer)
  (clisp-buffer-select))

;;; Sends current defun to current Lisp process and evals it.

(defun clisp-eval-defun ()
  "Send the current defun to the current Lisp process and evals it."
  (interactive)
  (message "Evaling defun...")
  (save-excursion
    (beginning-of-defun)
    (let ((begin (point)))
      (end-of-defun)
      (process-send-region
       (clisp-buffer-to-process-name *last-lisp-buffer*) begin (point))
      (process-send-string
       (clisp-buffer-to-process-name *last-lisp-buffer*) "\n")
      (message "Evaling defun...done."))))

;;; Sends current defun to current Lisp process, evals it, and
;;; switches to lisp buffer.

(defun clisp-eval-defun-and-go ()
  "Send the current defun to the current Lisp process, evals it, and 
switches to lisp buffer."
  (interactive)
  (clisp-eval-defun)
  (clisp-buffer-select))

;;; Sends current defun to current Lisp process and compiles it.

(defun clisp-compile-defun ()
  "Send the current defun to the current Lisp process and compiles it."
  (interactive)
  (message "Compiling defun...")
  (save-excursion
    (process-send-string (clisp-buffer-to-process-name *last-lisp-buffer*) 
			 (format "(progn %s (compile '%s))\n" 
				 (save-excursion
				   (beginning-of-defun)
				   (let ((begin (point)))
				     (end-of-defun)
				     (buffer-substring begin (point))))
				 (clisp-extract-defun-name)
				 (message "Compiling defun...done.")))))

;;; Sends current defun to current Lisp process, compiles it, and
;;; switches to lisp buffer.

(defun clisp-compile-defun-and-go ()
  "Send the current defun to the current Lisp process, evals it, and 
switches to lisp buffer."
  (interactive)
  (clisp-compile-defun)
  (clisp-buffer-select))

;;; Sends last sexpr to current Lisp process and evals it.

(defun clisp-eval-last-sexpr () 
  "Send the last sexpr to the current Lisp process and evals it."
  (interactive)
  (message "Evaling sexpr...")
  (save-excursion
    (mark-sexp -1)
    (process-send-region
     (clisp-buffer-to-process-name *last-lisp-buffer*) (point) (mark)) 
    (process-send-string
     (clisp-buffer-to-process-name *last-lisp-buffer*) "\n")
    (message "Evaling sexpr...done.")))

;;; Sends last sexpr to current Lisp process, evals it, and switches
;;; to lisp buffer.

(defun clisp-eval-last-sexpr-and-go ()
  "Send the last sexp to current Lisp process, evals it, and switches 
to lisp buffer."
  (interactive)
  (clisp-eval-last-sexpr)
  (clisp-buffer-select))

;;; Sends current region to current Lisp process and evals it.

(defun clisp-eval-region () 
  "Send the current region to current Lisp process and evals it."
  (interactive)
  (message "Evaling region...")
  (save-excursion
    (process-send-region
     (clisp-buffer-to-process-name *last-lisp-buffer*) (point)(mark))
    (process-send-string
     (clisp-buffer-to-process-name *last-lisp-buffer*) "\n")
    (message "Evaling region...done.")))

;;; Sends current region to current Lisp process, evals it, and
;;; switches to lisp buffer.

(defun clisp-eval-region-and-go ()
  "Send the current region to current Lisp process, evals it, and 
switches to lisp buffer."
  (interactive)
  (clisp-eval-region)
  (clisp-buffer-select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags are used to support indexing of functions in lisp files. This
;;; is similar to the Symbolics META-. function. The major difference
;;; is that you must have a tag table constructed ahead of time. The
;;; tag table is stored in the file TAGS in the same directory as the
;;; lisp files it indexes.

;;; This function shows you which files are indexed in the current tag
;;; table.

(defun clisp-list-tag-files ()
  "Lists all files currently in tag table."
  (interactive)
  (or tags-file-name
      (visit-tags-table "TAGS"))
  (tag-table-files)
  (with-output-to-temp-buffer "*Help*"
    (princ "Files indexed by current tag table:\n")
    (mapcar '(lambda (file) 
	      (terpri)(princ " ")(princ file))
	    tag-table-files)))

;;; Builds a new tags table in file TAGS, containing all def forms for
;;; all lisp files in the current directory.

(defun clisp-recompute-tag-table ()
  "Recomputes tags for all files in current directory."
  (interactive)
  (message "Computing tags...")
  (shell-command "etags *.lsp")
  (visit-tags-table default-directory)
  (message "Computing tags...done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diversion functions. These functions send something to Lisp for
;;; evaluation and divert the output from this form to a given buffer.
;;; As far as the user can tell, Lisp hasn't seen these forms at all.
;;; Useful for getting info from current Lisp environment into the
;;; editor.

;;; *CLISP-DIVERSION-BUFFER* tells GNU where to stuff the diverted
;;; output.

(defvar *clisp-diversion-buffer* nil 
  "Indicates buffer for clisp-diverting-filter.")

;;; Open a new buffer containing the macroexpansion of the current
;;; form. Send a backquoted form to current Lisp for evaluation, where
;;; the header of the form tells GNU to intercept it.

(defun clisp-show-macro-expansion ()
  "Show Common Lisp macro expansion for current sexpr in temporary buffer."
  (interactive)
  (message "Computing macro expansion...")
  (save-window-excursion
    (setq *clisp-diversion-buffer* 
	  (get-buffer-create "*Macroexpansion Buffer*"))
    (set-buffer *clisp-diversion-buffer*)
    (erase-buffer))
  (save-excursion
    (process-send-string 
     (clisp-buffer-to-process-name *last-lisp-buffer*)
     (format "`(*GNU* ,(let ((*print-pretty* t))
                        (format nil \"~A\" (macroexpand-1 '%s))))\n"
	     (clisp-extract-sexpr)))
    (message "Computing macro expansion...done.")))

;;; The following two functions are bound to keys.  They tell
;;; CLISP-SHOW-DOCUMENTATION which kind of symbol to search for and
;;; document.

(defun clisp-show-function-documentation ()
  "Look for first symbol name before point and show its documentation"
  (interactive)
  (clisp-show-documentation 'function))

(defun clisp-show-variable-documentation ()
  "Look for first symbol name before point and show its documentation"
  (interactive)
  (clisp-show-documentation 'variable))

;;; Open a new buffer to show the doc-string for the current function
;;; or variable from Common Lisp. Send a backquoted form to current
;;; Lisp for evaluation, where the header of the form tells GNU to
;;; intercept it.

(defun clisp-show-documentation (symtype)
  "Show Common Lisp documentation for current symbol in temporary buffer."
  (message "Fetching documentation...")
  (save-excursion
    (let ((symname (cond ((equal symtype 'function)
			  (upcase (clisp-extract-function-name)))
			 ((equal symtype 'variable)
			  (upcase (clisp-extract-variable-name))))))
      (setq *clisp-diversion-buffer* 
	    (get-buffer-create "*Documentation Buffer*"))
      (save-window-excursion
	(set-buffer *clisp-diversion-buffer*)
	(erase-buffer)
	(insert (format "%s %s: "
			(capitalize (symbol-name symtype))
			symname)))
      (and (equal symtype 'variable)
	   (process-send-string
	    (clisp-buffer-to-process-name *last-lisp-buffer*)
	    (format
	     "`(*GNU* ,(let ((*print-pretty* t))
                         (format nil \"~A\"
                           (cond ((boundp (intern \"%s\"))
                                  (eval (intern \"%s\")))
                                 (t \"Unbound\")))))\n"
	     symname symname symname)))
      (process-send-string 
       (clisp-buffer-to-process-name *last-lisp-buffer*)
       (format 
	"`(*GNU* ,(format nil \"~2%%~A\" (documentation '%s '%s)))\n"
	symname symtype))
      (message "Fetching documentation...done.")
      (sit-for 1)
      (message "Type C-x 1 to remove documentation window."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions for showing macro expansion and Common Lisp
;;; definition information.

;;; Returns the function name of current defun.

(defun clisp-extract-defun-name ()
  "Returns the function name of current defun."
  (save-excursion 
    (beginning-of-defun)
    (forward-char 1)
    (forward-sexp 2)
    (buffer-substring (point)(progn (backward-sexp 1)(point)))))

;;; Returns nearest symbol behind the point.  Keep moving backward
;;; until you find a character that is either alphanumeric or another
;;; symbol-name character.

(defun clisp-extract-variable-name ()
  "Returns nearest symbol behing the point."
  (save-excursion
    (while (and (/= (char-syntax (char-after (point))) 119)
		(/= (char-syntax (char-after (point))) 95))
      (backward-char 1))
    (buffer-substring
     (progn (backward-sexp 1)(point))
     (progn (forward-sexp 1)(point)))))

;;; Returns the function name (car position) of the current sexpr.

(defun clisp-extract-function-name ()
  "Returns function name from current non-atomic sexpr."
  (save-excursion 
    (cond ((looking-at "(")
	   (forward-char 2))
	  ((looking-at ")")
	   (forward-char 1)
	   (backward-sexp 1)
	   (forward-char 2))
	  (t (search-backward "(")
	     (forward-char 2)))
    (buffer-substring 
     (progn (backward-sexp 1)(point))
     (progn (forward-sexp 1)(point)))))

;;; Extracts the current non-atomic sexpr.

(defun clisp-extract-sexpr ()
  "Returns current non-atomic sexpr."
  (save-excursion
    (cond ((looking-at "(")
	   (buffer-substring (point)
			     (progn (forward-sexp 1)(point))))
	  ((looking-at ")")
	   (forward-char 1)
	   (buffer-substring 
	    (progn (backward-sexp 1)(point))
	    (progn (forward-sexp 1)(point))))
	  (t (search-backward "(")
	     (buffer-substring (point) 
			       (progn (forward-sexp 1)(point)))))))

;;; Returns the process name corresponding to a lisp buffer.

(defun clisp-buffer-to-process-name (buffer)
  (let ((buffername (buffer-name buffer)))
    (substring buffername 1 (1- (length buffername)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The process filters allow communication from Common Lisp to GNU (GNU to
;;; Common Lisp is supplied by GNU buffer mechanism). 

;;; CLISP-STARTUP-FILTER is a temporary filter used only until the
;;; first lisp prompt. Inserts the message of the day, but doesn't
;;; complain if the motd file doesn't exist.

(defun clisp-startup-filter (proc string buffer)
  "Startup filter function for inferior lisp process.  Prints the
message of the day if there is one and this is the first call to
this function.  Then looks for first prompt and switches control
thereafter to clisp-filter."
  (save-window-excursion
    (set-buffer buffer)
    (cond ((string-match inferior-lisp-prompt string)
	   (goto-char (point-max))
	   (insert string)
	   (move-marker last-input-start (1+ (point)))
	   (set-marker (process-mark proc) (1+ (point)))
	   (set-process-filter proc
			       (` (lambda (cls-proc cls-string)
				    (clisp-filter
				     cls-proc cls-string
				     (, buffer))))))
	  ((equal (process-name proc) "lisp")
	   (condition-case ()
	       (insert-file *clisp-motd-file*)
	     (error nil))
	   (insert string))
	  (t (insert string)))))

;;; CLISP-FILTER is the normally used filter. It simply echoes all
;;; output from Common Lisp to the lisp buffer, unless that output is
;;; marked with a special tag indicating GNU should intercept it. Any
;;; output that looks like (*GNU* output-sexpr) will change the
;;; process filter to clisp-diverting-filter until the next lisp
;;; prompt.  We can't rely on the output for GNU coming in a single
;;; string. We can, however, rely on the fact that it starts near the
;;; beginning of a string (usually in positions 1 or 2, depending on
;;; leading carriage returns and the like).

(defun clisp-filter (proc string buffer)
  "Filter function for inferior lisp process. Looks for any output from
process beginning with the marker *GNU* and intercepts it."
  (cond ((and (> (length string) 7)
	      (or (string-equal "*GNU*" (substring string 1 6))
		  (string-equal "*GNU*" (substring string 2 7))))
	 (set-process-filter proc
			     (` (lambda (cls-proc cls-string)
				  (clisp-diverting-filter
				   cls-proc cls-string
				   (, buffer)))))
	 (let ((quote-position (string-match "\"" string 6)))
	   (clisp-diverting-filter proc 
				   (substring string (+ quote-position 1))
				   buffer)))
	(t  (save-window-excursion
	      (set-buffer buffer)
	      (goto-char (point-max))
	      (insert string)
	      (move-marker last-input-start (1+ (point-max)))
	      (set-marker (process-mark proc) (1+ (point-max)))))))

;;; CLISP-DIVERTING-FILTER hijacks the output from process and dumps it
;;; in the buffer cached in *clisp-diversion-buffer*. When it reaches a
;;; lisp prompt, it stops diverting output.

(defun clisp-diverting-filter (proc string original-buffer)
  "Filter function that diverts output from process to a different buffer.
When the inferior-lisp-prompt is found, the process filter is reset to 
clisp-filter."
  (let ((prompt-position (string-match inferior-lisp-prompt string)))
    (cond (prompt-position
	   (save-window-excursion
	     (set-buffer *clisp-diversion-buffer*)
	     (goto-char (point-max))
	     (insert (substring string 0 prompt-position))
	     (delete-backward-char 4)
	     (goto-char (point-min))
	     (set-buffer-modified-p nil))
	   (display-buffer *clisp-diversion-buffer*)
	   (set-process-filter proc
			       (` (lambda (cls-proc cls-string)
				    (clisp-filter
				     cls-proc cls-string
				     (, original-buffer)))))
	   (cond ((> (length string) prompt-position)
		  (clisp-filter proc 
				(substring string (match-end 0))
				original-buffer))))
	  (t (save-window-excursion
	       (set-buffer *clisp-diversion-buffer*)
	       (goto-char (point-max))
	       (insert string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation commands.

;;; CLISP-REINDENT-FORM should rejustify a comment if it is called from
;;; within a comment line. Otherwise, if called from within a lisp
;;; form, it should reindent the entire lisp form.

(defun clisp-reindent-form ()
  "Reindents the current form, whether it be comment or code."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (cond ((looking-at ";")
	   (clisp-set-prefix-string)
	   (beginning-of-comment)
	   (let ((begin (point)))
	     (end-of-comment)
	     (fill-region-as-paragraph begin (point))))
	  (t (beginning-of-defun)
	     (next-line 1)
	     (message "Reindenting...")
	     (while (not (or (eobp)
			     (let ((indent (calculate-lisp-indent)))
			       (cond ((consp indent)
				      (zerop (car indent)))
				     (t (zerop indent))))))
	       (lisp-indent-line)
	       (next-line 1))
	     (message "Reindenting...done.")))))

;;; CLISP-SET-PREFIX-STRING is used to set the fill prefix string to the
;;; right thing for each type of comment.

(defun clisp-set-prefix-string ()
  "Determines what the fill-prefix should be depending on the comment type."
  (cond ((looking-at ";;; ")
	 (setq fill-prefix ";;; "))
	((looking-at ";; ")
	 (setq fill-prefix ";; "))
	((looking-at "; ")
	 (setq fill-prefix "; "))
	(t (setq fill-prefix ""))))

(defun beginning-of-comment ()
  "Moves to first comment line surrounding point."
  (while (and (not (bobp))
	      (progn (back-to-indentation)
		     (looking-at ";")))
    (previous-line 1))
  (next-line 1)
  (back-to-indentation))

(defun end-of-comment ()
  "Moves to last comment line surrounding point."
  (while (and (not (eobp))
	      (progn (back-to-indentation)
		     (looking-at ";")))
    (next-line 1))
  (previous-line 1)
  (end-of-line)
  (forward-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fix the indentation of FOR macro and other forms that are not normally
;;; well indented by GNU.

(put 'for 'common-lisp-indent-hook 'lisp-indent-for)
(put 'bind 'common-lisp-indent-hook 'lisp-indent-for)
(put 'repeatwhile 'common-lisp-indent-hook 'lisp-indent-for)
(put 'repeatuntil 'common-lisp-indent-hook 'lisp-indent-for)

(put 'merge 'common-lisp-indent-hook 1)
(put 'while 'common-lisp-indent-hook 1)
(put 'until 'common-lisp-indent-hook 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions and variables for Interlisp FOR macro indentation.

(defvar lisp-for-keyword-indentation 2
  "Indentation of FOR macro keywords relative to containing list.
This variable is used by the function lisp-indent-for.")

(defvar lisp-for-body-indentation t
  "Indentation of forms after FOR macro keywords relative to containing list.
This variable is used by the function lisp-indent-for to indent normal
lines (lines without FOR macro keywords).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by lisp-body-indent.")

;;; LISP-INDENT-FOR is almost exactly like LISP-INDENT-TAGBODY except that
;;; it uses the above-defined variables for indenting a FOR macro and indents
;;; keywords even if you use some on the same line as the FOR.

(defun lisp-indent-for (path state indent-point sexp-column normal-indent)
  (save-excursion
    (goto-char indent-point)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (list (cond ((looking-at "\\sw\\|\\s_")
;;; a FOR macro keyword
		 (+ sexp-column lisp-for-keyword-indentation))
		((integerp lisp-for-body-indentation)
		 (+ sexp-column lisp-for-body-indentation))
		((eq lisp-for-body-indentation 't)
		 (condition-case ()
		     (progn (backward-sexp 1) (current-column))
		   (error (1+ sexp-column))))
		(t (+ sexp-column lisp-body-indent)))
	  (elt state 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Superparen functions. These are adapted from functions in
;;; simple.el, part of the standard GNU distribution.

;;; DOES-NOT-MATCH-CLOSE-PAREN-P returns t if the character at the
;;; given character position is not a match for a close paren.

(defun does-not-match-close-paren-p (charpos)
  "Returns t if the character at the point doesn't match a close paren."
;;; 41 is the character code for a close paren.
  (/= 41
      (logand
;;; The character that the one at charpos matches is stored in the
;;; upper 8 bits of its syntax table entry.
       (lsh (aref (syntax-table)
		  (char-after charpos))
	    -8)
       ?\177)))

;;; SHOW-MATCHING-CONTEXT will either blink the (visible) open paren
;;; matching the current close paren, or print the part of this line
;;; containing the matching open paren in the minibuffer if it isn't
;;; visible in the current window.

(defun show-matching-context (lastopenpos)
  (save-excursion
    (goto-char lastopenpos)
;;; If the last-matched open paren is on the screen, just move the
;;; cursor to it temporarily.
    (cond ((pos-visible-in-window-p)
	   (sit-for 1))
;;; Otherwise, print part of the line containing the last-matched open
;;; paren.
	  (t (goto-char lastopenpos)
	     (message
	      "Matches %s"
	      (cond ((save-excursion
		       (skip-chars-backward " \t")
		       (not (bolp)))
		     (buffer-substring
		      (progn (beginning-of-line) (point))
		      (1+ lastopenpos)))
		    (t (buffer-substring
			lastopenpos
			(progn
			  (forward-char 1)
			  (skip-chars-forward "\n \t")
			  (end-of-line)
			  (point))))))))))

;;; SUPER-CLOSE-PAREN searches backwards for open parens and inserts
;;; matching close parens at the point.  If an open bracket is
;;; encountered, it is replaced with an open paren and matched, but
;;; the matching stops.
;;; If you are in Common Lisp mode, open parens within comments will
;;; be matched, so you should begin top level forms with an open bracket
;;; to keep from matching parens in comments.

(defun super-close-paren ()
  "Insert close parentheses as necessary to balance unmatched open parens."
  (interactive)
;;; If the character before the point is a quote, just insert a close
;;; bracket.  If not, don't bother looking for open parens if the
;;; point is at the beginning of the buffer.
  (cond ((= (char-syntax (char-after (- (point) 2))) ?\\ )
	 (insert "]"))
	((if (> (point) (1+ (point-min)))
;;; If you're not at the beginning of the buffer, start looking for
;;; open parens.	     
	     (let* ((openpos t)       ; must be t to pass the while test
		    (mismatch)
		    (lastopenpos t))  ; used to signal 1st iteration
;;; Insert a close paren to keep scan-sexps from returning the left
;;; end of a symbol instead of a list.
	       (insert ")")
	       (while openpos
;;; Keep looking for unmatched open parens to the left and inserting
;;; matching close parens until there are no unmatched parens.
;;; Condition-case traps errors quietly.
		 (condition-case ()
		     (setq openpos (scan-sexps (point) -1))
		   (error nil))
;;; If no new open paren has been found, then the new position will be
;;; the same as the old one.  In this case, the while loop should be
;;; terminated, so openpos should be set to nil.  Setting lastopenpos
;;; to nil signals that no open parens at all were found.
		 (cond ((equal openpos lastopenpos)
			(setq openpos nil)
			(cond ((equal lastopenpos t)
			       (setq lastopenpos nil)))))
;;; If you have found an open paren, but the syntax table says that it
;;; isn't a "paired delimiter" and doesn't match a close paren, that
;;; open paren is either mismatched or is really a open bracket.
		 (if (and
		      openpos
		      (/= (char-syntax (char-after openpos)) ?\$))
		     (setq mismatch
			   (does-not-match-close-paren-p
			    openpos)))
;;; If you have found a mismatch or open bracket, terminate the while
;;; loop.  If the last "paren" found was actually an open bracket, it
;;; should be replaced replaced with an open paren and matched with a
;;; close paren.  The open bracket is not a mismatch, but the while
;;; loop should still be exited.
		 (cond (mismatch
;;; 91 is the character code for open bracket
			(cond ((= 91 (char-after openpos))
			       (setq lastopenpos openpos)
			       (save-excursion
				 (goto-char openpos)
				 (delete-char 1)
				 (insert "("))
			       (insert ")")
			       (setq mismatch nil)))
			(setq openpos nil)))
;;; If you've found a matchable open paren, insert a close paren.
;;; Otherwise, get rid of the extra paren inserted earlier.
		 (cond (openpos
			(insert ")")
			(setq lastopenpos openpos))
		       (t (delete-backward-char 1))))
;;; If you found mismatched parens, complain.  Otherwise, show what
;;; the last paren inserted matches.
	       (cond (mismatch
		      (message "Mismatched parentheses"))
		     (lastopenpos
		      (show-matching-context lastopenpos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The remainder of this file was adapted from code originally
;;; written by Wolfgang Rupprecht (wolfgang@mgm.mit.edu) by David
;;; Hubbell (hubbell@svax.cs.cornell.edu). The original file
;;; (shellext.el) provided ksh-like extensions to shell.el; we've
;;; usurped the history mechanism and some of the shell options from
;;; Wolfgang's code, and have added much of our own code.

;;; This is a ksh-like extension to shell.el.  These extensions
;;; implement command history (backwards, forwards, back-search,
;;; forward-srearch) and history printout for an emacs shell window.
;;; The one glaring difference between this and ksh, is that all of
;;; the shell-mode commands are bound to the Control-C prefix map.
;;; (Eg.  previous command is C-c C-p).

(defvar clisp-shell-last-search "" "Last shell search string.")
(defvar clisp-shell-max-history 60
  "*Max shell history retained")
(defvar clisp-shell-history-list nil
  "History list of past shell commands.")
(defvar clisp-shell-history-index -1
  "Where we are on the history list. It is -1 unless currently
walking up/down the list")

;;; CLISP-SHELL-PREVIOUS-COMMAND replaces the region between the last
;;; character output from the shell and the end of the buffer with the
;;; (clisp-shell-history-index + 1)th element in the history list, if
;;; this element exists, and increments clisp-shell-history-index.
;;; Successive calls to CLISP-SHELL-PREVIOUS-COMMAND therefore produce
;;; successively older entries in the history list.

(defun clisp-shell-previous-command ()
  "Insert the previous command on the history list into the Lisp buffer."
  (interactive)
  (let ((history (nthcdr (1+ clisp-shell-history-index)
			 clisp-shell-history-list)))
    (cond (history
	   (delete-region (process-mark
			   (get-buffer-process (current-buffer)))
			  (point-max))
	   (goto-char (point-max))
	   (insert (car history))
	   (setq clisp-shell-history-index
		 (1+ clisp-shell-history-index)))
	  (t (error
	      "Beginning of history list (# of entries: %d)"
	      (1+ clisp-shell-history-index))))))

;;; CLISP-SHELL-NEXT-COMMAND replaces the region between the last
;;; character output from the shell and the end of the buffer with the
;;; (clisp-shell-history-index)th element in the history list, if this
;;; element exists, and decrements clisp-shell-history-index.
;;; Successive calls to CLISP-SHELL-NEXT-COMMAND therefore produce
;;; successively younger entries in the history list.

(defun clisp-shell-next-command ()
  "Insert the next command from the history list into the Lisp buffer."
  (interactive)
  (cond ((< 0 clisp-shell-history-index)
	 (delete-region (process-mark
			 (get-buffer-process (current-buffer)))
			(point-max))
	 (goto-char (point-max))
	 (insert (nth
		  (setq clisp-shell-history-index
			(1- clisp-shell-history-index))
		  clisp-shell-history-list)))
	(t (error "End of history list"))))

;;; CLISP-SHELL-HISTORY-SEARCH-BACKWARD searches chronologically
;;; backward (not structurally) through the history list for a string
;;; that is a superstring of the argument.  If such a string is found,
;;; it replaces the text between the last prompt and the end of the
;;; buffer and clisp-shell-history-index is changed to the index of
;;; this string in the history list.  If such a string is not found,
;;; clisp-shell-history-index remains unaltered.

(defun clisp-shell-history-search-backward (string)
  "Search backwards through the history list for STRING
and inserts it if the search is successful."
  (interactive (list (setq clisp-shell-last-search
			   (read-string
			    "History search for: "
			    clisp-shell-last-search))))
  (let* ((index (1+ clisp-shell-history-index)) ; start at next command
	 (history (nthcdr index clisp-shell-history-list)))
    (while (and history
		(null (string-match string (car history))))
      (setq index (1+ index)
	    history (cdr history)))
    (cond (history
	   (setq clisp-shell-history-index index)
	   (delete-region (process-mark
			   (get-buffer-process (current-buffer)))
			  (point-max))
	   (goto-char (point-max))
	   (insert (car history)))
	  (t (error "No match found, now at entry %d"
		  clisp-shell-history-index)))))

;;; CLISP-SHELL-HISTORY-SEARCH-FORWARD searches chronologically
;;; forward (not structurally) through the history list for a string
;;; that is a superstring of the argument.  If such a string is found,
;;; it replaces the text between the last prompt and the end of the
;;; buffer and clisp-shell-history-index is changed to the index of
;;; this string in the history list.  If such a string is not found,
;;; clisp-shell-history-index remains unaltered.

(defun clisp-shell-history-search-forward (string)
  "Search forwards through the history list for STRING
and inserts it if the search is successful."
  (interactive (list (setq clisp-shell-last-search
			   (read-string
			    "History search for: "
			    clisp-shell-last-search))))
;;; Reversing the list now is asymptotically more efficient than
;;; doing nth n times, where n is the length of the history list.
  (let* ((history-length (length clisp-shell-history-list))
	 (index (- history-length clisp-shell-history-index 1))
	 (reverse-history-list
	  (nthcdr index (reverse clisp-shell-history-list))))
    (while (and reverse-history-list
		(null (string-match string
				    (car reverse-history-list))))
      (setq index (1+ index)
	    reverse-history-list (cdr reverse-history-list)))
    (cond (reverse-history-list
	   (setq clisp-shell-history-index (- history-length index 1))
	   (delete-region (process-mark
			   (get-buffer-process (current-buffer)))
			  (point-max))
	   (goto-char (point-max))
	   (insert (car reverse-history-list)))
	  (t (error "No match found, now at entry %d"
		  clisp-shell-history-index)))))

;;; CLISP-SHELL-LIST-HISTORY prints the contents of the history list
;;; to a temporary *History* buffer, most recent entry first, with a
;;; '*' at the current position.

(defun clisp-shell-list-history ()
  "List the history in the *History* buffer. A '*' indicates current
position on the history list."
  (interactive)
  (with-output-to-temp-buffer "*History*"
    (let ((history clisp-shell-history-list)
	  (index 0))
      (while history
	(princ (format " %c[%d] %s\n" 
		       (if (= index clisp-shell-history-index)
			   ?* ?\ )
		       index (car history)))
	(setq history (cdr history)
	      index (1+ index)))))
  (message "Type C-x 1 to remove history window."))

;;; CLISP-SHELL-SAVE-HISTORY saves the region between the last prompt
;;; and the end of buffer onto the history list, and sets the
;;; clisp-shell-history-index to the start (most recent entry) of the
;;; list.

(defun clisp-shell-save-history ()
  "Save this command on the clisp-shell-history-list."
  (let ((command (buffer-substring
		  last-input-start (1- last-input-end))))
    (if (or (string-match "^[ \t]*$" command)
	    (string-equal command (car clisp-shell-history-list)))
	nil				; don't hang dups on list
	(setq clisp-shell-history-list
	      (cons command clisp-shell-history-list))
	(let ((prune-pt (nthcdr clisp-shell-max-history
				clisp-shell-history-list)))
	  (and prune-pt (rplacd prune-pt nil)))))
  (setq clisp-shell-history-index -1))

;;; CLISP-SHELL-SEND-INPUT-IF-SEXPR:  see documentation string.

(defun clisp-shell-send-input-if-sexpr ()
  "Send input to subshell if the last input was an s-expression.
At end of buffer, sends all text after last output as input to the
subshell if that text was an s-expression, including a newline inserted
at the end.
If the point is not at the end of the buffer and is after the last prompt,
the point moves to the end of buffer.
If still not at end, copies current line to the end of the buffer and sends
it, if it was an s-expression, after first attempting to discard any prompt
at the beginning of the line by matching the regexp that is the value of
shell-prompt-pattern if possible.  This regexp should start with \"^\"."
  (interactive)
  (or (get-buffer-process (current-buffer))
      (error "Current buffer has no process"))
  (end-of-line)
;;; The beginning of the input region will be where the process ;;
;;; left off.  If the point is after the beginning of the input ;;
;;; region, place it at the end of the buffer.
  (move-marker last-input-start
	       (process-mark (get-buffer-process (current-buffer))))
  (cond ((< (marker-position last-input-start) (point))
	 (goto-char (point-max))))
;;; If you're at the end of the buffer, just insert a newline and
;;; move the input-end marker to the end of the buffer. If you're
;;; not at the end of the buffer, copy everything from  the last
;;; prompt to the point to the end of the buffer and treat that as
;;; the input region.
  (cond ((eobp)
	 (newline)
	 (move-marker last-input-end (point)))
	(t (let* ((last-prompt-end
		 (save-excursion
		   (re-search-backward shell-prompt-pattern nil t)
		   (re-search-forward shell-prompt-pattern nil t)
		   (point)))
		(copy (buffer-substring
		       last-prompt-end
		       (point))))
	   (goto-char (point-max))
	   (move-marker last-input-start (point))
	   (insert copy)
	   (move-marker last-input-end (point)))))
;;; If the string between the start of input and the end of the buffer
;;; is an s-expression, send it to the lisp process and save it on
;;; the history list.  Otherwise, terminate.
  (let ((last-sexp-end
	 (save-excursion
	   (goto-char (marker-position last-input-start))
	   (condition-case ()
	       (scan-sexps (point) 1)
	     (error nil)))))
    (cond (last-sexp-end
;;; Even if we get an error trying to hack the working directory,
;;; still send the input to the subshell.
	   (condition-case ()
	       (save-excursion
		 (goto-char last-input-start)
		 (shell-set-directory))
	     (error (funcall shell-set-directory-error-hook)))
	   (let ((process (get-buffer-process (current-buffer))))
	     (process-send-region process last-input-start last-input-end)
	     (clisp-shell-save-history)
	     (set-marker (process-mark process) (point)))))))
