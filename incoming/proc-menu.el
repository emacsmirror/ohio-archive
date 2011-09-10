;;; proc-menu.el --- process menu main function and support functions.

;; $Id: proc-menu.el,v 1.6 2000/02/24 17:46:10 mlombard Exp $

;;        Name: Process menu
;;   $Revision: 1.6 $
;;       $Date: 2000/02/24 17:46:10 $
;;      Author: Marco Lombardi <lombardi@sns.it>
;;  Maintainer: Marco Lombardi <lombardi@sns.it>
;;        Info: Process menu
;;    Filename: proc-menu.el
;;    Category: Utilities
;;              Programming
;;              Lisp

;;; Commentary:

;; This file redefines `list-processes' to allow user interaction with
;; the Emacs processes. The command `list-processes' shows a special
;; buffer, similar to the one shown by `list-buffers', with the
;; processes currently active in Emacs. In this buffer you can select,
;; kill, send signals to, or show info on processes, much like in
;; `list-buffers'. The single entry point is `list-processes'.
;;
;; I suggest to bound `list-processes' to C-x C-p by adding the line
;;
;;   (global-set-key [(control x) (control p)] 'list-processes)
;;
;; in your .emacs file.

;;; Code:

(defvar Process-menu-mode-map nil
  "Mode map used in the buffer `*Process List*'.")

(defvar Process-menu-process-column 9
  "Column where the process name is given in the processes list.
Do not change! This variable will be modified as required.")

;(setq Process-menu-mode-map nil)
(if Process-menu-mode-map
    ()
  (setq Process-menu-mode-map (make-keymap))
  (suppress-keymap Process-menu-mode-map t)
  (define-key Process-menu-mode-map "v" 'Process-menu-select)
  (define-key Process-menu-mode-map "f" 'Process-menu-this-window)
  (define-key Process-menu-mode-map "\C-m" 'Process-menu-this-window)
  (define-key Process-menu-mode-map "o" 'Process-menu-other-window)
  (define-key Process-menu-mode-map "\C-o" 'Process-menu-switch-other-window)
  (define-key Process-menu-mode-map "q" 'quit-window)
  (define-key Process-menu-mode-map "d" 'Process-menu-delete)
  (define-key Process-menu-mode-map "k" 'Process-menu-kill)
  (define-key Process-menu-mode-map "b" 'Process-menu-quit)
  (define-key Process-menu-mode-map "s" 'Process-menu-stop)
  (define-key Process-menu-mode-map "z" 'Process-menu-stop)
  (define-key Process-menu-mode-map "c" 'Process-menu-continue)
  (define-key Process-menu-mode-map "\C-k" 'Process-menu-delete)
  (define-key Process-menu-mode-map "x" 'Process-menu-execute)
  (define-key Process-menu-mode-map "i" 'Process-menu-info)
  (define-key Process-menu-mode-map "w" 'Process-menu-send-string)
  (define-key Process-menu-mode-map "W" 'Process-menu-send-buffer)
  (define-key Process-menu-mode-map "e" 'Process-menu-send-eof)
  (define-key Process-menu-mode-map " " 'next-line)
  (define-key Process-menu-mode-map "n" 'next-line)
  (define-key Process-menu-mode-map "p" 'previous-line)
  (define-key Process-menu-mode-map "?" 'describe-mode)
  (define-key Process-menu-mode-map "u" 'Process-menu-unmark)
  (define-key Process-menu-mode-map "g" 'Process-menu-revert)
  (define-key Process-menu-mode-map [mouse-2] 'Process-menu-mouse-select)
)

;; Process Menu mode is suitable only for specially formatted data.
(put 'Process-menu-mode 'mode-class 'special)

(defun Process-menu-mode ()
  "Major mode for editing a list of processes.
Each line describes one of the processes in Emacs.
Letters do not insert themselves; instead, they are commands.

\\{Process-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map Process-menu-mode-map)
  (setq major-mode 'Process-menu-mode)
  (setq mode-name "Process Menu")
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'Process-menu-revert-function)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'process-menu-mode-hook))

(defun Process-menu-revert ()
  "Update the list of processes."
  (interactive)
  (revert-buffer))

(defun Process-menu-revert-function (ignore1 ignore2)
  (list-processes))

(defun Process-menu-buffer (error-if-non-existent-p)
  "Return buffer associated with the current selected process.
If `ERROR-IF-NON-EXISTENT-P' is not nil, displays an error in no
buffer can be found."
  (let* ((where (save-excursion
		  (beginning-of-line)
		  (+ (point) Process-menu-process-column)))
	 (name (and (not (eobp)) (get-text-property where 'process-name))))
    (if name
	(if (get-process name)
	    (or (process-buffer (get-process name))
		(if error-if-non-existent-p
		    (error "No buffer associated with procss `%s'" name)
		  nil))
	  (if error-if-non-existent-p
	      (error "No process named `%s'" name)
	    nil))
      (if error-if-non-existent-p
	  (error "No process on this line")
	nil))))


(defun process-menu ()
  "Make a menu of processes so you can delete or select them.
Type ? after invocation to get help on commands available.
Type q immediately to make the process menu go away."
  (interactive)
  (switch-to-buffer (list-processes-noselect))
  (message
   "Commands: d, k, b, s, z, c, x, u; w, W, e; f, o, v, i; q to quit; ? for help."))


(defun process-menu-other-window ()
  "Display a list of processes in another window.
With the process list buffer, you can delete or select the processes.
Type ? after invocation to get help on commands available.
Type q immediately to make the process menu go away."
  (interactive)
  (switch-to-buffer-other-window (list-processes-noselect))
  (message
   "Commands: d, k, b, s, z, c, x, u; w, W, e; f, o, v, i; q to quit; ? for help."))

(defun Process-menu-unmark ()
  "Cancel all requested operations on process on this line and move down."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")
      (ding)
    (if (eq (char-after) ? ) ()
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert " ")))
    (forward-line 1)))

(defun Process-menu-delete ()
  "Mark process on this line to be deleted by \\<Process-menu-mode-map>\\[Process-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun Process-menu-kill ()
  "Mark process on this line to be killed by \\<Process-menu-mode-map>\\[Process-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?K)
      (forward-line 1))))

(defun Process-menu-quit ()
  "Mark process on this line to be quitted by \\<Process-menu-mode-map>\\[Process-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?Q)
      (forward-line 1))))

(defun Process-menu-stop ()
  "Mark process on this line to be stopped (break) by \\<Process-menu-mode-map>\\[Process-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?Z)
      (forward-line 1))))

(defun Process-menu-continue ()
  "Mark process on this line to be continued by \\<Process-menu-mode-map>\\[Process-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?C)
      (forward-line 1))))

(defun Process-menu-is-process (proc)
  "Return t if the process `PROC' is a real subprocess."
  (let ((status (process-status proc)))
    (not (or (eq status 'closed) (eq status 'open)))))

(defun Process-menu-execute ()
  "Execute all marks on processes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil)
	  where name char)
      (while (search-forward-regexp "^.[^-S]" nil t)
	(beginning-of-line)
	(setq char (char-after)
	      where (save-excursion
		      (beginning-of-line)
		      (+ (point) Process-menu-process-column))
	      name (and (not (eobp))
			(get-text-property where 'process-name)))
	(cond
	 ((eq char ?D) (delete-process name))
	 ((eq char ?K) (if (Process-menu-is-process name)
			   (kill-process name)))
	 ((eq char ?Q) (if (Process-menu-is-process name)
			   (quit-process name)))
	 ((eq char ?Z) (if (Process-menu-is-process name)
			   (stop-process name)))
	 ((eq char ?C) (if (Process-menu-is-process name)
			   (continue-process name))))
	(forward-line 1))))
  (sit-for 0.1)
  (list-processes))

(defun Process-menu-info ()
  "Show a buffer with information on the current process."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let* ((char (char-after))
	   (where (save-excursion
		    (beginning-of-line)
		    (+ (point) Process-menu-process-column)))
	   (name (and (not (eobp))
		      (get-text-property where 'process-name)))
	   (proc nil)
	   info-buffer)
      (if (not (setq proc (get-process name)))
	  (error "No process named `%s'" name)
	(save-excursion
	  (setq info-buffer (get-buffer-create "*Process Info*"))
	  (set-buffer info-buffer)
	  (auto-fill-mode -1)
	  (setq fill-column 20)
	  (erase-buffer)
	  (let ((buffer (process-buffer proc))
		(command (process-command proc))
		(contact (process-contact proc))
		(coding (process-coding-system proc))
		(kill (process-kill-without-query proc))
		(standard-output standard-output))
	    (process-kill-without-query proc kill)
	    (setq standard-output (current-buffer))
	    (princ "Name: ")
	    (justify-current-line 'right)
	    (princ name)
	    (princ "\nType: ")
	    (justify-current-line 'right)
	    (if (process-id proc)
		(princ "subprocess")
	      (princ "TCP network connection"))
	    (if (process-id proc)
		(progn
		  (princ "\nProcess ID: ")
		  (justify-current-line 'right)
		  (princ (process-id proc))))
	    (princ "\nProcess status: ")
	    (justify-current-line 'right)
	    (princ (process-status proc))
	    (princ "\nAssociated buffer: ")
	    (justify-current-line 'right)
	    (if (not contact)
		(princ buffer)
	      (princ "nil"))
	    (princ "\nTTY: ")
	    (justify-current-line 'right)
	    (princ (process-tty-name proc))
	    (if (process-id proc)
		(progn
		  (princ "\nCommand: ")
		  (justify-current-line 'right)
		  (princ (mapconcat (lambda (command) command)
				    command " ")))
	      (princ "\nHost: ")
	      (justify-current-line 'right)
	      (princ (car contact))
	      (princ "\nPort: ")
	      (justify-current-line 'right)
	      (princ (cadr contact)))
	    (princ "\nFilter: ")
	    (justify-current-line 'right)
	    (princ (process-filter proc))
	    (princ "\nSentinel: ")
	    (justify-current-line 'right)
	    (princ (process-sentinel proc))
	    (princ "\nDecoding system: ")
	    (justify-current-line 'right)
	    (princ (car-safe coding))
	    (princ "\nCoding system: ")
	    (justify-current-line 'right)
	    (princ (cdr-safe coding))
	    (princ "\nKill without query: ")
	    (justify-current-line 'right)
	    (if kill (princ "yes")
	      (princ "no"))
	    (goto-char (point-min))))
	(view-buffer-other-window info-buffer nil 'kill-buffer)))))
	
(defun Process-menu-send-string ()
  "Send a simple string to the current process."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let* ((char (char-after))
	   (where (save-excursion
		    (beginning-of-line)
		    (+ (point) Process-menu-process-column)))
	   (name (and (not (eobp))
		      (get-text-property where 'process-name))))
      (if (not (get-process name))
	  (error "No process named `%s'" name)
	(process-send-string name (read-string "String to send: "))))))

(defun Process-menu-send-buffer ()
  "Send the whole content of a buffer to the current process."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let* ((char (char-after))
	   (where (save-excursion
		    (beginning-of-line)
		    (+ (point) Process-menu-process-column)))
	   (name (and (not (eobp))
		      (get-text-property where 'process-name)))
	   buffer)
      (if (not (get-process name))
	  (error "No process named `%s'" name)
	(setq buffer (read-buffer "Buffer to send: " nil t))
	(if (not buffer) ()
	  (save-excursion
	    (set-buffer buffer)
	    (process-send-region name (point-min) (point-max))))))))

(defun Process-menu-send-eof ()
  "Make the current process see an end-of-file in its input."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-S]")		;header lines
      (ding)
    (let* ((char (char-after))
	   (where (save-excursion
		    (beginning-of-line)
		    (+ (point) Process-menu-process-column)))
	   (name (and (not (eobp))
		      (get-text-property where 'process-name))))
      (if (not (get-process name))
	  (error "No process named `%s'" name)
	(if (y-or-n-p "Really send EOF to process? ")
	    (process-send-string name (read-string "String to send: ")))))))

(defun Process-menu-select ()
  "Select the buffer associated to current line process."
  (interactive)
  (let ((buff (Process-menu-buffer t))
	(menu (current-buffer))
	(others ())
	tem)
    (goto-char (point-min))
    (delete-other-windows)
    (switch-to-buffer buff)
    (or (eq menu buff)
	(bury-buffer menu))))

(defun Process-menu-mouse-select (event)
  "Select the buffer whose line you click on.
`EVENT' is the standard event as described by Emacs."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (Process-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (if (and (window-dedicated-p (selected-window))
	     (eq (selected-window) (frame-root-window)))
	(switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer))))

(defun Process-menu-this-window ()
  "Select the buffer associated to the current process in this window."
  (interactive)
  (switch-to-buffer (Process-menu-buffer t)))

(defun Process-menu-other-window ()
  "Select the buffer associated to the current process in other window."
  (interactive)
  (switch-to-buffer-other-window (Process-menu-buffer t)))

(defun Process-menu-switch-other-window ()
  "Make the other window select this line's buffer process.
The current window remains selected."
  (interactive)
  (display-buffer (Process-menu-buffer t)))

(defun list-processes ()
  "Display a list of names of processes.
The list is displayed in a buffer named `*Process List*'.

The S column contains the process status (Run, [Z]stop, Signal, eXit,
Closed, Open)."
  (interactive)
  (display-buffer (list-processes-noselect)))

(defun list-processes-noselect ()
  "Create and return a buffer with a list of names of existing processes.
The buffer is named `*Process List*'."
  (let ((old-buffer (current-buffer))
	(standard-output standard-output)
	desired-point)
    (save-excursion
      (set-buffer (get-buffer-create "*Process List*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (princ "\
 S ID    Process      Buffer         Tty         Command
 - --    -------      ------         ---         -------
")
      ;; Record the column where process names start.
      (setq Process-menu-process-column 9)
      (let ((pl (process-list)))
	(while pl
	  (let* ((process (car pl))
		 (name (process-name process))
		 (buffer (process-buffer process))
		 (tty (process-tty-name process))
		 (command (process-command process))
		 (status (process-status process))
		 (id (process-id process))
		 (contact (process-contact process))
		 (ch nil)
		 name-beg name-end)
	    (cond
	     ((eq status 'run) (setq ch 'r))
	     ((eq status 'stop) (setq ch 'z))
	     ((eq status 'exit) (setq ch 'x))
	     ((eq status 'signal) (setq ch 's))
	     ((eq status 'closed)
	      (setq ch 'c)
	      (setq buffer nil)
	      (setq command (list (concat "@" (car contact) ":"
					  (int-to-string (cadr contact))))))
	     ((eq status 'open)
	      (setq ch 'o)
	      (setq buffer nil)
	      (setq command (list (concat "@" (car contact) ":"
					  (int-to-string (cadr contact))))))
	     (t (setq ch '\?)))
	    (indent-to 1)
	    (princ ch)
	    (indent-to 3 1)
	    (if id
		(princ id)
	      (princ "(TCP)"))
	    (indent-to 9 1)
	    (setq name-beg (point))
	    (princ name)
	    (setq name-end (point))
	    (put-text-property name-beg name-end
			       'process-name name)
	    (put-text-property name-beg name-end
			       'mouse-face 'highlight)
	    (indent-to 22 1)
	    (if buffer
		(princ buffer)
	      (princ "(nil)"))
	    (indent-to 37 1)
	    (if tty
		(princ tty)
	      (princ "(nil)"))
	    (indent-to 49 1)
	    (if command
		(princ (mapconcat (lambda (command) command)
				  command " "))
	      (princ "(nil)"))
	    (princ "\n"))
	  (setq pl (cdr pl)))
      (Process-menu-mode)
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
      (and desired-point
	   (goto-char desired-point))
      (current-buffer)))))

;;; proc-menu.el ends here

(provide 'proc-menu)
