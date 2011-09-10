;;; Muddy.el - the obfuscated mud client

;;; This is a MUD client, used to make MUD connections more convenient.
;;; If you don't know what a MUD is, you're better off not knowing,
;;; since you may still have a life, friends, and family that you wish
;;; to hang on to...

;;; Author - Darin Johnson
;;;   Stuff originally by me is not copyrighted, but stuff I note as
;;;   being borrowed by others might be...  A lack of a copyright does
;;;   not mean common courtesy does not apply.
;;; Version 1.0

;;; HOW TO USE ME
;;;
;;; Basically, you give the command `muddy'.  If you don't know how to
;;; give emacs commands, stop now.  This particular client assumes that
;;; the user has emacs experience, and if you want to customize things,
;;; you will probably elisp experience as well.
;;;
;;; Once done, Muddy will prompt you for a system name.  Completion
;;; is enabled, so you can just use a prefix.  If you enter a blank
;;; line, Muddy will prompt you for a host and port to connect to,
;;; so you can use systems that haven't been predefined.  It will also
;;; prompt you for a name, so that this server can be accessed via
;;; completion if you run `muddy' again (however, currently these
;;; on-the-fly servers won't be saved in the configuration file.
;;; If you give an argument (C-u) when starting, it will directly
;;; prompt you for a host/port rather than asking for an existing server.
;;;
;;; After that, the system connects to the mud and you can give commands,
;;; etc.  There is no auto-login at the moment, but this is straight
;;; forward to add if you know elisp (I don't like putting passwords in
;;; files, so I never use such a feature anyway).
;;;
;;; You can quit by quitting the mud normally.  Or, if you kill the
;;; buffer, it will drop your connection without necessarily logging
;;; you out.  This can be rude on some systems.
;;;
;;; Features: history, some abbrevs, full (cumbersome) customization, etc.
;;; Here's a summary of the builtin key bindings:
;;;
;;;   RET   - send the command you typed to the mud.  If there are multiple
;;;           lines waiting to be sent, Muddy will just send the first line,
;;;           and then position the prompt on the next line, which will get
;;;           sent on the next RET.  This way you can easily send multiline
;;;           chunks of text without a cumbersome cut-and-past for each
;;;           line.  If an argument (C-u) is given, the line of input will
;;;           be sent that many times.
;;;
;;;   M-RET   This will resend the last command in the history.  This way
;;;           you can send the same input over and over very quickly (for
;;;           those times you feel like screaming on and on).
;;;
;;;   M-up-arrow
;;;   M-down-arrow
;;;   M-p
;;;   M-n     These will cycle through the command history by replacing the
;;;           current input (if any) with entries on the history list.
;;;           M-p gets previous history items, and M-n gets later history
;;;           items.  The up-arrow and down-arrow items will only be
;;;           defined for Emacs version 19.
;;;           If you don't understand, just try it out.
;;;
;;;  C-c C-u  Erase the current line of input.
;;;  C-u      Erase the current line of input if that's where the point is,
;;;           otherwise do universal-argument as normal.  (added because
;;;           that's the mentally hard-wired notion of ^U if you're in a
;;;           shell, and Muddy acts a lot like a shell)
;;;
;;;  DEL      This will delete backwards, but will try not to go beyond
;;;           a prompt (if can recognize a prompt).
;;;
;;;  TAB      Expand the previous word using dabbrev-expand.  (the abbrev
;;;           package if you don't know expands abbreviations)

;;; CUSTOMIZATION and INITIALIZATION
;;;
;;; Ok, here's where you need to be elisp (or at least lisp) knowledgable.
;;; Many of the variables declared after all these header comments can be
;;; given different values, so read the comments down there.
;;;
;;; When this file is loaded, an initialization file is also loaded.
;;; This file is defined in the muddy-init-file variable, and defaults
;;; to either the value of MUDDYINIT or else "muddy-init.el" (the
;;; emacs load path is searched, or you can give a full path name).
;;; A sample init file should be included along with this file.
;;;
;;; Aliases and Auto-actions:
;;;
;;; You can put any elisp code you want in this file and it will be
;;; executed, however the primary purpose is to define the list of
;;; servers.  This list is put into the `muddy-server-list' variable.
;;;
;;; Each element of this list is of the form:
;;;    (NAME  HOST  PORT  HOOK)
;;;
;;; NAME and HOST should be strings, PORT is a number, and HOOK is a
;;; function (a name or lambda function).  The NAME is used to select
;;; this server via completion when you start Muddy.  After the server
;;; starts up, HOOK will be called (after muddy-startup-hook), which can
;;; be used to add aliases, auto-actions, variable customization, etc.
;;;
;;; Aliases are regexps that match in the user's input, and auto-actions
;;; are regexps that match in the MUD's output.  These are used to
;;; automatically execute elisp code when certain patterns are detected.
;;; They are defined with the
;;;
;;;   (muddy-alias REGEXP PRIO ACTION1 ACTION2 etc)
;;;   (muddy-auto  REGEXP PRIO ACTION1 ACTION2 etc)
;;;
;;; The PRIO argument is used to sort multiple aliases and actions, so
;;; that those with lower numbered PRIO's are checked first.  REGEXP
;;; can be any regular expression.  If the REGEXP matches, then the
;;; ACTION's will be executed (REGEXP's only get a single line at a time).
;;; Remember: inside of strings, a "\" will need to be doubled!  [that is
;;; instead of "\(blah\)" you need "\\(blah\\)"]

;;; The return value of the last ACTION of an alias or auto-action has
;;; a special meaning.  If it returns nil, then the alias or auto-action
;;; is removed.  This allows for actions that will only execute a single
;;; time.  If the return value is the symbol 'repeat, then it will try
;;; re-match this alias/auto-action instead of moving on to the next.
;;; This is useful if you want to apply an action over and over until
;;; it fails to match.  (for instance, an auto-action that deletes a
;;; single ANSI control sequence would need to be repeated in case there
;;; might me multiple ANSI sequences on a single line)
;;;
;;; For example, the expression
;;;    (muddy-alias "\'" 5 (replace-match "say ") t)
;;; will cause all of the user's input lines that start with a single-quote
;;; to have that quote replaced with "say ", thus allowing "'" to be an
;;; alias for "say ".  The "t" as the last action says to keep this alias
;;; and not delete it.
;;;
;;; Obviously, the usefulness of aliases and auto-actions depends upon
;;; what you can do in the actions.  Here are some functions and macros
;;; that are useful from inside an action:
;;;
;;; replace-match
;;;     - replaces the entire matched regexp with it's argument.
;;; muddy-arg
;;;     - if 0, return the matched regexp.  If positive, return the
;;;       matching parenthesized expression in the regexp.  Basically
;;;       does (buffer-substring (match-beginning arg) (match-end arg)).
;;;       (parenthesized expressions have "\\(" and "\\)" as delimiters)
;;; muddy-delete-match
;;;     - deletes the matched regexp.  An argument if given, specifies
;;;       which parenthesized expression in the regexp to delete.
;;; muddy-send-string
;;;     - send the (string) argument as a command to the MUD.
;;; muddy-send-command
;;;     - like muddy-send-string, but echo it to the buffer first.
;;; muddy-silent-read
;;;     - read without echoing in the minibuffer (useful to get passwords)
;;; muddy-enqueue
;;; muddy-dequeue
;;;     - muddy-enqueue will queue up a command, and muddy-dequeue will
;;;       dequeue a command and send it to the MUD.  As a pair these can
;;;       be used to queue up commands to execute at a later time (such as
;;;       after next time a prompt is received).

;;; BUGS
;;;  will hang until timeout if host doesn't connect
;;;  (cursor stays in minibuffer)
;;;
;;; Doesn't nicely detect that connection has closed, so when you
;;; log out of the mud, you will probably get an 'exited abnormally' message
;;; (emacs bug)

;;; TODO
;;;
;;; Deal with "already running" (multiple connections to same host).
;;; Simplify customization (currently requires emacs knowledge)
;;;    (simplified commands for the more common actions, such as gag?)
;;; Easy removal of aliases, etc?
;;; Various "features" - simple to add if you know how, but...
;;;    Keypad bindings for directional movement
;;;    List aliases
;;;    Selective wrapping, such as only on says/emotes
;;;    Easy binding of keys to aliases
;;;    Triggers with a probability of execution
;;;    Hilighting (can do with emacs 19 faces)
;;;    Redirect input/output from files, local commands, etc.
;;;    LP ed editing mode?  FTP/MTP subprocess?
;;;    Save current configuration
;;;    Suppress duplicate lines as an option
;;;    Pre-built abbrev table (per-mud)
;;;    PMF style encrypted says
;;; See what other cool stuff in emacs 19 can be utilized...

;;;
;;; Comint not used for various reasons (insertion of alias expansion
;;; difficult for one).
;;;

;;; VARIABLE DEFINITIONS
;;; (If the doc string starts with "*", this is a "user option" that
;;; is easily settable via M-x set-variable and M-x edit-options)

(defvar muddy-init-file
  (or (getenv "MUDDYINIT")
      "muddy-init.el")
  "*This is the file muddy loads when it starts up.  It should set up the
muddy-server-list at least, and any other hooks, autos, aliases or other
customization you want.")

(defvar muddy-history-size 40
  "*Number of remembered input lines for muddy-mode")

(defvar muddy-prompt nil
  "*Prompt used by mud if any (regexp).  Although it can help if
the prompt is recognized, it isn't necessary, so just setting it to
the most common prompt you expect to receive is fine (ie, '> ' for LPmud).")

;;; You want to customize this in the muddy-init-file
(defvar muddy-server-list nil "List of known servers")

(defvar muddy-fake-prompt nil
  "*If this is set, it will be inserted before your input as a prompt.
Useful for systems that have no builtin prompt.  Buffer local.")

(defvar muddy-startup-hook nil "Run when mud started up")

(defvar muddy-wrap-lines t
  "*Word wrap overlong lines from the mud.  You can adjust the length
of the wrapped lines with set-fill-column.")

(defvar muddy-wrap-prefix nil
  "*If mud output is word wrapped (see muddy-wrap-lines), stick this
prefix before all the lines of wrapped text but the first (if set).")

;; These aren't really user customizable...

(defvar muddy-mode-map nil "Keymap for Muddy mode")

(defvar muddy-history-index nil)

(defvar muddy-history nil "History list for Muddy mode")

(defvar muddy-server nil "Current mud we're connected to in this buffer")

(defvar muddy-alias-list nil "Aliases")

(defvar muddy-auto-list nil "Auto-action list (output filters)")

(defvar muddy-queue nil "Queued commands if any")

(defvar muddy-last-command nil)

;;; BASIC FUNCTIONS
;;; Define the major mode and get a connection

(defun muddy (arg)
  "Open new connection to a mud.  See muddy-mode for more information."
  (interactive "P")
  (let* ((server (muddy-get-server arg))
         (buf (muddy-get-buffer server))
         (config-hook (nth 3 server)))
    (switch-to-buffer buf)
    (muddy-mode)
    (setq muddy-server server)
    (goto-char (point-max))
    ;; run customizable setup
    (run-hooks 'muddy-startup-hook 'config-hook)))

(defun muddy-mode ()
  "The Muddy Mode.

This mode is used to talk to a MUD (or perhaps to any other similar
command-oriented network-based server).  Normally you should start up
a connection with the `muddy' command, rather than starting
`muddy-mode' directly.

When you start with the `muddy' command, it will prompt for a server
to connect to.  These servers should be defined in the muddy-server-list
variable.  If you just hit return (sending a blank line) instead of giving
a server name, Muddy will prompt for a host and port pair to connect to,
as well as for a server name (to refer to later).

Once connected, you can send commands just by typing, and then pressing
return to send that input.  Output from the MUD will be displayed when
it arrives.

Much more information is available as comments at the start of the
muddy.el source code.

\\{muddy-mode-map}

Entry to this mode calls the value of `muddy-startup-hook' as well as
any server specific hooks."
  (interactive)

  ;; set up local variables - these will have different values for each buffer
  (kill-all-local-variables)
  (make-local-variable 'muddy-history-index)
  (make-local-variable 'muddy-history)
  (make-local-variable 'muddy-prompt)
  (make-local-variable 'muddy-fake-prompt)
  (make-local-variable 'muddy-server)
  (make-local-variable 'muddy-wrap-lines)
  (make-local-variable 'muddy-wrap-prefix)
  (make-local-variable 'muddy-alias-list)
  (make-local-variable 'muddy-auto-list)
  (make-local-variable 'muddy-queue)
  (make-local-variable 'muddy-last-command)

  (setq major-mode 'muddy-mode)
  (setq mode-name "Muddy")  ; base on mud type?
  (setq mode-line-process '(": %s"))
  (setq fill-column (1- (window-width)))
  ;; custom syntax table useful?
  (set-syntax-table (standard-syntax-table))
  (use-local-map (copy-keymap muddy-mode-map))
  (muddy-init-history)
  (setq muddy-queue nil)

  ;; BUG: this is necessary because emacs doesn't remove network
  ;; processes correctly (as of 19.19)
  (make-local-variable 'kill-buffer-hook)
  (setq kill-buffer-hook '(lambda ()
			    (let ((proc (get-buffer-process (current-buffer))))
			      (if proc (delete-process proc)))))
  )

;; Prompt for and get a server.  If arg is set, or a blank line is
;; given for the server, then prompt for a host/port pair.
(defun muddy-get-server (arg)
  "Get name of mud server"
  (interactive)
  (let* ((completion-ignore-case t)
         (server (if arg ""
		   (completing-read "Mud Server (or blank): "
				    muddy-server-list
				    nil t))))
    (if (string= server "")
        (let ((host (read-string "Host name: "))
              (port (string-to-int (read-string "Port: ")))
              (name (read-string "call this world: ")))
          (setq muddy-server-list (append muddy-server-list
                                          (list (list name host port nil))))
          (setq server name)))
    (assoc server muddy-server-list)))

;; Create a buffer for this mud, and start up a network process
(defun muddy-get-buffer (server)
  (let ((name (concat "*" (car server) "*"))
        (host (nth 1 server))
        (port (nth 2 server))
        (buf nil)
        (proc nil))
    (if (and
         ;; change this??
	 (get-buffer-process name)
	 (process-status (get-buffer-process name)))
	(error "already running"))
    (setq buf (get-buffer-create name))
    ;; open network stream isn't best choice, since we can't detect when
    ;; it is dead, and killing buffer leaves connection up.
    (setq proc (open-network-stream "Muddy" buf host port))
    (set-process-filter proc 'muddy-output-filter)
    (set-process-sentinel proc 'muddy-sentinel)
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    ; (process-kill-without-query proc t) ; require confirmation
    (buffer-disable-undo buf)
    buf))

;; This is from 'shellext.el' by Wolfgang Rupprecht <wolfgang@mgm.mit.edu>
(defun looking-at-backward (regexp)
  (let* ((begin (point))
	 (found (re-search-backward regexp nil t)))
    (goto-char begin)
    (and found (= begin (match-end 0)))))

;;;
;;; Mud IO Routines (send commands, output filter, etc)
;;;

(defun muddy-send (arg)
  "Find player input and send to the mud, expanding aliases, adding to
history, and updating process mark.  If ARG is non-zero, send input that
many times."
  (interactive "p")
  (end-of-line)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (or (null proc) (not (memq (process-status proc) '(open run))))
        (progn
         (message "No process in buffer.");
         (beep))
      (let ((start (muddy-input-start))
            command)
        (muddy-expand-alias start)
        (end-of-line)
        (setq command (buffer-substring start (point)))
        (if (not (string-match "\\`\\s *\\'" command))
            (muddy-add-history command))
        (while (not (zerop arg))
          (muddy-send-string command proc)
          (setq arg (1- arg)))
        (if (= (point) (point-max))
            (insert ?\n)
          (beginning-of-line 2))
        (set-marker (process-mark proc) (point))
        (if muddy-fake-prompt (insert muddy-fake-prompt))
        (recenter -1))
      )))

(defun muddy-send-string (string &optional proc)
  "Send a STRING to the mud.  PROC defaults to process of current buffer."
  (interactive "sString: ")
  (if (null proc) (setq proc (get-buffer-process (current-buffer))))
  (process-send-string proc (concat string "\n")))

;; Not really tested, but could be useful for automatic actions to
;; let the player know that a command was given.
(defun muddy-send-command (string &optional proc)
  "Like \\[muddy-send-string] but inserts into buffer first."
  (let ((string (concat string "\n")))
    (insert-before-markers string)
    (process-send-string (get-buffer-process (current-buffer)) string)))

(defun muddy-input-start ()
  "Find the start of the current input"
  (let ((start (process-mark (get-buffer-process (current-buffer)))))
    (goto-char start)
    (if (and muddy-fake-prompt (looking-at muddy-fake-prompt))
        (setq start (+ start (length muddy-fake-prompt))))
    start))

(defun muddy-delete-backward-char (n &optional killflag)
  "Like delete-backward-char, but won't delete past a recognized prompt
[via muddy-prompt].  The idea is to only delete player input, not mud
output, so this could be made smarter I suppose..."
  (interactive "*p\nP")
  (if (and
       (not (and muddy-prompt (looking-at-backward muddy-prompt)))
       (not (and muddy-fake-prompt (looking-at-backward muddy-fake-prompt))))
      (delete-backward-char n killflag)))

;; from lpmud.el
(defun muddy-silent-read ()
  "Read a string in the minibuffer without echoing.
Current line will be used as the prompt.  Useful for reading passwords, etc."
  (interactive)
  (let (prompt)
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (setq prompt (buffer-substring (point) end))))
    (message "%s" prompt)
    (let ((input-string "")
          (input-char)
          (cursor-in-echo-area t))
      (while (not (eq (setq input-char (read-char)) ?\r))
        (progn
          (message "%s" prompt)
          (setq input-string
                (cond
                 ((eq input-char ?\C-?)
                  (if (equal (length input-string) 0)
                      ""
                    (substring input-string 0 -1)))
                 ((eq input-char ?\C-u)
                  "")
                 (t
                  (concat input-string (char-to-string input-char)))))))
      (message "") ; clear message buffer
      input-string)))


;; The output filter, this is a major puppy.  It takes stuff the mud
;; has sent us and works it over, displaying in the buffer,
;; processing auto actions, and so forth.

(defun muddy-output-filter (proc string)
  "Filter output from remote mud process, munge it up a bit, then display it."
  (let ((data (match-data))
        (old-buf (current-buffer)))
    ;; display-buffer here if pop-to-front?
    (unwind-protect
        (let (moving start end (hasprompt nil))
          (set-buffer (process-buffer proc))
          (setq moving (= (point) (process-mark proc)))

	  (save-excursion
            (goto-char (process-mark proc))
            (setq start (point))
	    ;; Optional prompt handling
            (if (and muddy-prompt
                     (looking-at-backward muddy-prompt))
                (progn
                  (goto-char (match-beginning 0))
                  (setq start (point) hasprompt t)))
            (insert-before-markers string)
            (setq end (point))
            (if (not hasprompt)
                (set-marker (process-mark proc) end))
            (save-restriction
              (narrow-to-region (point-min) end) ;; so typeahead not seen
	      ;; clean ^M garbage (dos deserves a horrible death)
              (goto-char start)
              (while (search-forward "\r" nil t)
                (delete-backward-char 1))
	      ;; handle auto actions
              (goto-char start)
              (muddy-auto-actions))
            (goto-char (point-max))
	    ;; wrap output.  May need to re-find mark here to work correctly?
            (if muddy-wrap-lines (muddy-wrap start (process-mark proc))))

          (if moving (goto-char (process-mark proc)))
          (if (and (eobp) (get-buffer-window (process-buffer proc)))
              (let ((old-win (selected-window)))
                (select-window (get-buffer-window (process-buffer proc)))
                (recenter -1)
                (select-window old-win))))
      (set-buffer old-buf)
      (store-match-data data))))

;; automatically called when process status changes.
(defun muddy-sentinel (proc change)
  (beep)
  (if (not (memq (process-status proc) '(open run)))
      (save-excursion
        (set-buffer (process-buffer proc))
        (goto-char (point-max))
        (insert change)
        (message "Connection closed to %s" (car muddy-server))
        )))

(defun muddy-wrap (start end)
  "Word wrap buffer.  Insert muddy-wrap-prefix on any new lines formed."
  ;; can't use normal fill-region-as-paragraph
  ;; start/end specify region to wrap.  DONT call from inside narrow region,
  ;; since column numbers may be wrong.  (not a perfect wrapper, but that's
  ;; not really critical)
  (interactive "r")
  (goto-char start)
  (beginning-of-line)
  (save-restriction
    (narrow-to-region (point) end)  ;; otherwise we may wrap pending input
    (while (not (eobp))
      (let* ((lastp (point))
             (col (move-to-column (1+ fill-column))))
        (if (<= col fill-column)
            (beginning-of-line 2) ; skip this line
          (skip-chars-backward "^ \t\n")
          (if (<= (point) lastp)
              (beginning-of-line 2)
            (delete-horizontal-space)
            (insert ?\n)
            (if muddy-wrap-prefix (insert muddy-wrap-prefix))))))))

;;;
;;; History stuff
;;;

;; borrowed ring history stuff from comint.el

(defun muddy-ring-mod (n m)
  (let ((n (% n m)))
    (if (>= n 0) n
      (+ n (if (>= m 0) m (- m)))))) ; (abs m)
  
(defun muddy-ring-minus1 (index veclen)
  (- (if (= 0 index) veclen index) 1))

(defun muddy-ring-ref (ring index)
  (let* ((hd (car ring))
         (vec (cdr ring))
         (len (length vec)))
    (if (or (< index -1) (>= index len))
        nil
      (setq index (muddy-ring-mod (+ hd index) len))
      (aref vec index))))

(defun muddy-add-history (command)
  "Add a new command to the history list"
  (let* ((vec (cdr muddy-history))
         (len (length vec))
         (new-hd (muddy-ring-minus1 (car muddy-history) len)))
    (if (not (string= command muddy-last-command))
	(progn
	  (setq muddy-last-command command)
	  (setcar muddy-history new-hd)
	  (aset vec new-hd command)
	  (aset vec (muddy-ring-minus1 new-hd len) "")))))

(defun muddy-previous-history (arg)
  "Look backwards through command history.  The idea is that this,
and muddy-next-history are bound to keys so that one can quickly
cycle through the history list."
  (interactive "*p")
  (let* ((hd (car muddy-history))
         (vec (cdr muddy-history))
         (len (length vec))
	 (pmark (process-mark (get-buffer-process (current-buffer))))
         new-index)
    (cond ((< (point) pmark)
           (message "Not at end of buffer")
           (ding))
          ((null (aref vec hd))
           (message "No history")
           (ding))
          (t
           (cond ((eq last-command 'muddy-previous-history)
                  (delete-region (point) pmark))
                 (t
                  (setq muddy-history-index
                        (if (> arg 0) -1 (if (< arg 0) 1 0)))))
           (setq new-index (+ muddy-history-index arg))
           (if (null (muddy-ring-ref muddy-history new-index))
               (message "No more history")
             (setq muddy-history-index new-index)
             (insert (muddy-ring-ref muddy-history muddy-history-index)))
           (setq this-command 'muddy-previous-history)))))
    
(defun muddy-next-history (arg)
  "Look forwards through command history"
  (interactive "*p")
  (muddy-previous-history (- arg)))
  
(defun muddy-init-history ()
  (let ((vec (make-vector (+ muddy-history-size 1) nil)))
    (aset vec 0 "")   ;; initial history
    (setq muddy-history-index 0)
    (setq muddy-history (cons 0 vec))))

(defun muddy-repeat-command (arg)
  "Repeats the last command.  If bound to a key, it becomes very simple
to repeat a command over and over."
  (interactive "p")
  (if (not (eobp))
      (message "Not and end of buffer")
    (let ((string (muddy-ring-ref muddy-history 0)))
      (cond (string
             (insert string)
             (muddy-send arg))))))

;;;
;;; ALIASES/AUTO-ACTIONS (and useful configuration functions)
;;;

;; macros for easy access to the alists
(defmacro muddy-alist-re (a)
  (list 'eval (list 'car a)))
(defmacro muddy-alist-prio (a)
  (list 'car (list 'cdr a)))
(defmacro muddy-alist-prog (a)
  (list 'cdr (list 'cdr a)))

;; Look at the current input and expand any aliases found
;; (uses muddy-alias-list)
(defun muddy-expand-alias (start)
  (save-excursion
    (goto-char start)
    (if (looking-at "\\\\")
        (delete-char 1)
      (let ((l muddy-alias-list))
        (while l
          (let ((a (car l)))
            (goto-char start)
            (if (looking-at (muddy-alist-re a))
		;; found a match, perform the action
		(let ((res (eval (cons 'progn (muddy-alist-prog a)))))
		  (if (not res)
		      (setcar l nil))
		  (if (not (eq res 'repeat))
		      (setq l (cdr l))))
	      ;; else no match, so move to next
	      (setq l (cdr l)))))
	(setq muddy-alias-list (delq nil muddy-alias-list))
        ))))

;; mostly from mymud.el
;; Try to match mud output to regexps in muddy-auto-list, performing
;; the designated actions if found.  Usually called from within a
;; narrowed region.
(defun muddy-auto-actions ()
  ;; loop through each line in the region
  (while (not (eq (point) (point-max)))
    (if (looking-at " *$")
        (forward-line 1)
      (let ((l muddy-auto-list))
        ;; loop through each filter action
        (while (and l (not (looking-at " *$")))
          (let ((a (car l)))
            (if (looking-at (muddy-alist-re a))
		;; found a match, perform the action
		(let ((res (eval (cons 'progn (muddy-alist-prog a)))))
		  (if (not res)
		      (setcar l nil))
		  (if (not (eq res 'repeat))
		      (setq l (cdr l))))
	      ;; else no match, so move to next
	      (setq l (cdr l)))))
        (setq muddy-auto-list (delq nil muddy-auto-list))
        (if (looking-at "^ *$")
            (muddy-delete-line)
          (forward-line 1))))))

;; get the substring for the num'th match
(defmacro muddy-arg (num)
  "Return as a string the num'th match in the previous regexp.  0 means
the entire regexp, and positive numbers refer to any parenthesized
expressions in the regexp."
  (` (if (match-beginning (, num))
         (buffer-substring (match-beginning (, num)) (match-end (, num)))
       "")))

;; delete the num'th match
(defun muddy-delete-match (&optional num)
  "Deletes the last matched regexp.  If an arg is given, it specifies
which parenthesized expression [using \\( and \\)] in the regexp to
delete.  Useful when called from an alias or auto-action."
  (if (not num) (setq num 0))
  (delete-region (match-beginning num) (match-end num)))

;; next two are meant to be used to enqueue commands until a prompt appears
;; (ie, can queue up commands to send)
(defun muddy-enqueue (cmd)
  "Enqueue a command, which will later be removed and executed
with muddy-dequeue."
  (setq muddy-queue (append muddy-queue (list cmd))))

(defun muddy-dequeue ()
  "Remove any queued commands and send them to the mud.  The recommended
usage is to call this from an auto-action, since it is not automatically
called otherwise."
  (save-excursion
    (if muddy-queue
        (let ((cmd (car muddy-queue)))
          (setq muddy-queue (cdr muddy-queue))
          (goto-char (match-end 0))  ; insert after prompt
          (insert-before-markers (concat cmd "\n"))
          (muddy-send-string cmd)))))

;;;
;;; MISC
;;;

;; erase player input.
(defun muddy-erase-line ()
  (interactive)
  (let ((s (muddy-input-start)))
    (end-of-line)
    (delete-region s (point))))

(defun muddy-delete-line ()
  (beginning-of-line)
  (let ((s (point)))
    (forward-line 1)
    (delete-region s (point))))

(defun muddy-new-control-u ()
  "This will act as a normal universal-argument if executed with the
point before the process mark, but will erase the line if executed
after it.  This is so C-u can be used to kill the input easily (as a
shell would)."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (<= (point) (process-mark proc))
        (universal-argument)
      (muddy-erase-line))))

;;;
;;; BINDINGS
;;;

(if muddy-mode-map
    nil
  (setq muddy-mode-map (make-sparse-keymap))
  (define-key muddy-mode-map "\r" 'muddy-send)
  (define-key muddy-mode-map "\C-c\C-u" 'muddy-erase-line)
  (define-key muddy-mode-map "\C-u" 'muddy-new-control-u)   ; nicer to use
  (define-key muddy-mode-map "\e\r" 'muddy-repeat-command)
  (define-key muddy-mode-map "\^\?" 'muddy-delete-backward-char) 
  (define-key muddy-mode-map "\t" 'dabbrev-expand)
  (define-key muddy-mode-map "\ep" 'muddy-previous-history)
  (define-key muddy-mode-map "\en" 'muddy-next-history)
  (if (string-match "19" emacs-version)
      (progn
	(define-key muddy-mode-map [M-up] 'muddy-previous-history)
	(define-key muddy-mode-map [M-down] 'muddy-next-history)))
  )

;;;
;;; CONFIGURATION stuff
;;;

;; Add onto the alias or auto lists, keeping sorted with lower priority first.
(defun muddy-add-to-list (l key prio args)
  (setq l (append l (list (cons key (cons prio args)))))
  (sort l '(lambda (a b) (< (nth 1 a) (nth 1 b)))))

;; Add an alias.  These execute when matched in the user's input.
;; MATCH is a regexp.
;; PRIO is the priority of this alias (lower prio's match first).
;; ACTIONS are a list of actions to perform if this alias matches.
(defmacro muddy-alias (match prio &rest actions)
  (`
   (setq muddy-alias-list
         (muddy-add-to-list muddy-alias-list
                            (, match) (, prio) '(, actions)))))

;; Add an auto-action.  These execute when matched in the mud's output.
;; Args are the same as muddy-alias.
;; If the actions return nil, delete this auto-action (ie, only execute once).
;; If the actions return 'repeat, try again using the same match regexp.
;;    (ie, apply until no match)
;; If the actions return anything else, try the next match on the list.
(defmacro muddy-auto (match prio &rest actions)
  (`
   (setq muddy-auto-list
         (muddy-add-to-list muddy-auto-list
                            (, match) (, prio) '(, actions)))))

;;; All done...  Load the init file!
(load muddy-init-file t)

