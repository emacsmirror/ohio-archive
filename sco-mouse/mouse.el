;; mouse.el - Gnu emacs mouse driver for SCO Xenix or Unix.  
;; Copyright 1990 Ronald Florence (ron@mlfarm, 5.20.90)
;; 
;; This is a rudimentary mouse-driver which reads mouse events from
;; an asynchronous process.  It doesn't really work very well.  I am
;; posting it in the hope that someone will adopt and educate the
;; little rodent.  At the very least, the driver needs:
;;
;;   1. Improved mouse-motions.  The current code naively treats
;;	mouse motions as cursor motions.
;;   2. Could use local mouse-maps set by mode-hooks instead of 
;;	binding the mouse buttons in set-mouse-mode.  (suggested 
;;	by John Robinson, jr@bbn.com)
;;   3. The module and the mouse functions need less generic names.
;;
;; To install:
;;
;;   1. Compile emacsmouse
;;  	   [g]cc -O emacsmouse.c -levent -o emacsmouse
;;	and install it in ../emacs/etc.
;;   2. Install mouse.el in ../emacs/lisp.
;;   3. Make sure the pty devices (ttyp0 - ttyp??) are listed in
;;	/usr/lib/event/ttys, with the appropriate mouse.  The mouse
;;	won't work unless you compiled emacs to use ptys for processes.
;;   4. Put the line 
;;  	   (autoload 'mouse "mouse" nil t)
;;	in your ~/.emacs or ../emacs/lisp/default.el file.
;;   5. Change the mouse button bindings, or add bindings for a third
;;	button, in set-mouse-mode.
;;  
;;  To start the mouse, do "M-x mouse".  The mouse should follow the
;;  cursor.  If the mouse is phlegmatic or hyperactive, kill it and 
;;  change the sensitivity before you start the mouse again.


(provide 'mouse)

(defconst no-button "n")
(defconst left-button "l")
(defconst right-button "r")
(defconst both-buttons "b")
(defconst middle-button "c")
(defconst all-buttons "a")
(defconst left-middle-button "L")
(defconst right-middle-button "R")

(defvar sensitivity 5 
  "Sets mouse sensitivity.  The range is 1-9.")

(defvar mouse-process nil)

(defvar current-mouse-mode nil
  "The major-mode of the current mouse buffer.")


(defun mouse () 
"Reads mouse buttons and position.  The default button 
bindings in set-mouse-mode are

mode	left-button	right-button	both-buttons
----	-----------	------------	------------
vm	next-message	scroll-message	quit-vm
gnus	next-article	scroll-article	quit-newsgroup
dired	mark-deleted	visit-file	execute-deletions
buffer	mark-deleted	select-buffer	execute-deletions
shell	copy-input	send-input	interrupt-shell-subjob
default	set-mark	yank		kill-region "
  (interactive)
  (let ((live (and mouse-process
		   (eq (process-status mouse-process) 'run))))
    (if (not live)
	(save-excursion
	  (if mouse-process
	      (delete-process mouse-process))
	  (setq mouse-process
		(start-process "mouse" nil "emacsmouse" 
				(int-to-string sensitivity)))
	  (process-kill-without-query mouse-process)
	  (set-process-sentinel mouse-process 'mouse-sentinel)
	  (set-process-filter mouse-process 'mouse-filter)))))


(defun mouse-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (message "The mouse died.")))


(defun mouse-filter (proc string)
  (or (eq current-mouse-mode major-mode) (set-mouse-mode))
  (setq index 0
	x-delta 0
	y-delta 0
	oldbutton nil)
  (while 
      (string-match "[abclrLRn][+---][0-9][0-9][+---][0-9][0-9]" string index)
    (setq mouse-string (substring string (match-beginning 0))
	  button (substring mouse-string 0 1)
	  x-delta (string-to-int (substring mouse-string 1 4))
	  y-delta (string-to-int (substring mouse-string 4
				      (if (> (length mouse-string) 7) 7 nil)))
	  index (+ index 7))
    (cond ((> y-delta 0) (previous-line y-delta))
	((< y-delta 0) (next-line (- y-delta))))
    (cond ((> x-delta 0) (forward-char x-delta))
	((< x-delta 0) (backward-char (- x-delta))))
    (or (eq button oldbutton)
	(call-interactively (lookup-key mouse-map button)))
    (setq oldbutton button)))


(or (keymapp mouse-map)
    (setq mouse-map (make-sparse-keymap)))

(defun mouse-do-zip ()
  "Doesn't do anything."
  (interactive))

(defun set-mouse-mode ()
  (setq current-mouse-mode major-mode)
  (define-key mouse-map no-button 'mouse-do-zip)
  (cond ((eq major-mode (or 'vm-mode 'vm-summary-mode))
	 (define-key mouse-map left-button 'vm-next-message)
	 (define-key mouse-map right-button 'vm-scroll-forward)
	 (define-key mouse-map both-buttons 'vm-quit))
	((eq major-mode 'gnus-Group-mode)
	 (define-key mouse-map left-button 'gnus-Group-select-group)
	 (define-key mouse-map right-button 'gnus-Group-read-group) 
	 (define-key mouse-map both-buttons 'gnus-Group-next-unread-group))
	((eq major-mode 'gnus-Subject-mode)
	 (define-key mouse-map left-button 'gnus-Subject-next-unread-article)
	 (define-key mouse-map right-button 'gnus-Subject-next-page)
	 (define-key mouse-map both-buttons 'gnus-Subject-exit))
	((eq major-mode 'gnus-Article-mode)
	 (define-key mouse-map left-button 'gnus-Subject-prev-page)
	 (define-key mouse-map right-button 'gnus-Article-next-page)
	 (define-key mouse-map both-buttons 'gnus-Subject-exit))
	((eq major-mode 'dired-mode)
	 (define-key mouse-map left-button 'dired-flag-file-deleted)
	 (define-key mouse-map right-button 'dired-find-file)
	 (define-key mouse-map both-buttons 'dired-do-deletions))
	((eq major-mode 'Buffer-menu-mode)
	 (define-key mouse-map left-button 'Buffer-menu-delete)
	 (define-key mouse-map right-button 'Buffer-menu-this-window)
	 (define-key mouse-map both-buttons 'Buffer-menu-execute))
	((eq major-mode 'shell-mode)
	 (define-key mouse-map left-button 'copy-last-shell-input)
	 (define-key mouse-map right-button 'shell-send-input)
	 (define-key mouse-map both-buttons 'interrupt-shell-subjob))
	(t
	 (define-key mouse-map left-button 'set-mark-command)
	 (define-key mouse-map right-button 'yank)
	 (define-key mouse-map both-buttons 'kill-region))))
