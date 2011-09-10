;;; (Fairly) generic typeout window code.
;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>

;;; Usage:
;;; (typeout-buffer
;;; 	WindowNameString ; nil means concatenate Command and Args
;;; 	Command		 ; the shell command to run
;;; 	InputFile 	 ; nil means use /dev/null
;;; 	Arg1 .. ArgN )	 ; the remaining argumnets for Command.
;;; e.g.
;;; (typeout-buffer "Processes" "ps" nil "auxww")
;;; (typeout-buffer "Printer Queue" "lpq" nil)

;;; This causes a pop-up window with the information to appear.
;;; Hitting almost any key buries the buffer, and restores previous
;;; window/buffer configuration. The typeout buffer is NOT killed
;;; so it can still be yanked into mail messages or whatever,
;;; unless typeout-kill-buffers is set to a non-nil value.
;;; Typing ? gives a list of key bindings for scrolling about in
;;; typeout window should that be desired/necessary. You can hack
;;; typeout-mode-map if you want more key bindings. 

(require 'electric)
(provide 'typeout)

(defvar typeout-mode-map nil
  "The mode map for typeout buffers.")

(defvar typeout-kill-buffers nil
  "*If non-nil typeout buffers should be killed, rather than buried, on exit.")

(defvar typeout-command-asychronous nil
  "*If non-nil the commands run in typeout buffers should use START-PROCESS
rather than CALL-PROCESS, and Control-X should be bound to Control-X-prefix,
allowing the user to switch out of the typeout window without killing it.
Switching back into it and hitting almost any key will kill (or bury)
the window depending on the value of TYPEOUT-KILL-BUFFERS.")

(if typeout-mode-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'typeout-buffer-exit)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-h" 'Helper-help)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\C-x" 'Control-X-prefix)
    (define-key map "\C-z" 'suspend-emacs)
    (define-key map "\177" 'scroll-down) ; Delete scrolls back a page.
    (setq typeout-mode-map map)))

(defun typeout-buffer (name command &optional infile &rest args)
  "Make a typeout buffer by called NAME 
by calling the shell command COMMAND on INFILE and ARGS.
/dev/null is used as INFILE if it is nil, ARGS default to no arguments."
  (interactive)
  (save-window-excursion
    (let ((output-buffer-name (format "*%s*" (or name command)))
	  (name (or name
		    (mapconcat 'identity (cons command args) " "))))
      (if (not typeout-command-asychronous)
	  ;; For commands that you're willing to wait for....
	  (with-output-to-temp-buffer output-buffer-name
	    (set-buffer standard-output)
	    (apply 'call-process (nconc (list command infile t t) args)))
	;; For asychrononous [i.e. slow] commands use this form instead...
	(apply 'start-process
	       (nconc (list command output-buffer-name command) args)))
      (let ((buffer
	     (window-buffer (Electric-pop-up-window output-buffer-name))))
	(unwind-protect
	    (progn
	      (set-buffer buffer)
	      (typeout-buffer-mode name)
	      (catch 'typeout-bury-buffer
		(Electric-command-loop
		  'typeout-bury-buffer
		    (concat "<< Press space to "
			    (if typeout-kill-buffers "kill" "bury")
			    " this window >>")
		    t))))
	(if typeout-kill-buffers
	    (kill-buffer buffer)
	  (bury-buffer buffer))
	(message "")))))

(defun typeout-buffer-mode (name)
  "Major mode for typeout windows.
Arg NAME is string to display in mode line identification.

Anything except C-z, C-v, C-n, C-p, n, p, or DEL (which scrolls back a page)
buries the window, returning to previous window and buffer configuration.

Entry to this mode via command \\[typeout-buffer-mode] calls the value of
typeout-buffer-mode-hook if it is non-nil."
  (kill-all-local-variables)
  (use-local-map typeout-mode-map)
  (setq mode-name "Typeout Window")
  (setq mode-line-buffer-identification name)
  (make-local-variable 'Helper-return-blurb)
  (setq Helper-return-blurb "return to the Typeout Window.")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'typeout-buffer-mode)
  (goto-char (point-min))
  (run-hooks 'typeout-buffer-mode-hook))

(defun typeout-buffer-exit ()
  "Restore previous window configuration by burying or killing the
typeout buffer, depending on the value of typeout-kill-buffers."
  (interactive)
  (setq unread-command-char -1)
  (condition-case ()
      (throw 'typeout-bury-buffer nil)
    (error (kill-buffer (current-buffer)))))