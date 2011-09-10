;;; browse-mode.el  Buffer browsing facility for Emacs
;;; Copyright (C) 1992  Raul J. Acevedo (acevedo@MIT.EDU)
;;;
;;; LCD Archive Entry:
;;; browse-mode|Raul J. Acevedo|acevedo@mit.edu|
;;; Buffer browsing facility.|
;;; 1992||~/modes/browse-mode.el.Z|
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; This allows you to easily browse an Emacs buffer by binding the
;;; following keys: 
;;;
;;;    0 .. 9  Digit argument
;;;    SPC     Scroll up
;;;    DEL     Scroll down
;;;    n       Next line
;;;    p       Previous line
;;;    ,       Beginning of buffer
;;;    .       End of buffer
;;;    l       Recenter
;;;    g       Goto line
;;;    =       Show current cursor line and column
;;;    s       Search forward
;;;    r       Search backward
;;;    o       Other window
;;;    x       Exit; restores previous major mode
;;;    q       Remove this buffer's window
;;;    TAB     Switch to next buffer in Browse mode 
;;;    RET     Switch to next file buffer in Browse mode, whose file is 
;;;            in the directory as the current buffer's visited file
;;;    k       Kill buffer
;;;    K       Kill buffer and it's window
;;;
;;; Exiting browse mode leaves you in the previous major mode, with
;;; its keymap, as if you hadn't entered browse mode in the first
;;; place. 
;;;
;;; Check documentation of the function `browse-set-buffer-id' for
;;; nifty mode line hacking. 
;;;
;;; RCS users check out `browse-check-rcs-lock'. 
;;;

(provide 'browse-mode)

(if (not (assq 'browse-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(browse-mode " Browse")
				 minor-mode-alist)))

(defvar browse-check-rcs-lock t
  "*If non-nil, exiting Browse mode confirms you really want to 
edit the file if the file is unlocked under RCS.
This is determined by looking at the value of `mode-line-process'.")

(defvar browse-mode-hook 'browse-set-buffer-id
  "Hook run when you enter Browse mode.")

(defvar browse-mode-exit-hook 'browse-restore-buffer-id
  "Hook run when you exit Browse mode.")

(defvar browse-mode-map nil
  "*Keymap for Browse mode.")

(or browse-mode-map
    (progn
      (setq browse-mode-map (make-sparse-keymap))
      (define-key browse-mode-map "0" 'digit-argument)
      (define-key browse-mode-map "1" 'digit-argument)
      (define-key browse-mode-map "2" 'digit-argument)
      (define-key browse-mode-map "3" 'digit-argument)
      (define-key browse-mode-map "4" 'digit-argument)
      (define-key browse-mode-map "5" 'digit-argument)
      (define-key browse-mode-map "6" 'digit-argument)
      (define-key browse-mode-map "7" 'digit-argument)
      (define-key browse-mode-map "8" 'digit-argument)
      (define-key browse-mode-map "9" 'digit-argument)
      (define-key browse-mode-map " " 'scroll-up)
      (define-key browse-mode-map "\177" 'scroll-down)
      (define-key browse-mode-map "n" 'next-line)
      (define-key browse-mode-map "p" 'previous-line)
      (define-key browse-mode-map "b" 'bury-buffer)
; Added these JH [hetrick:19930613.1238CET]
;      (define-key browse-mode-map "u" 'half-down)
;      (define-key browse-mode-map "d" 'half-up)
      (define-key browse-mode-map "." 'beginning-of-buffer)
      (define-key browse-mode-map "," 'end-of-buffer)
      (define-key browse-mode-map "l" 'recenter)
      (define-key browse-mode-map "s" 'isearch-forward)
      (define-key browse-mode-map "r" 'isearch-backward)
      (define-key browse-mode-map "o" 'other-window)
      (define-key browse-mode-map "x" 'browse-mode)
      (define-key browse-mode-map "q" 'delete-window)
      (define-key browse-mode-map "\r" 'browse-next-directory-buffer)
      (define-key browse-mode-map "\t" 'browse-next-buffer)
      (define-key browse-mode-map "g" 'goto-line)
      (define-key browse-mode-map "=" 'browse-cursor-where)
; added "K" function below--- JH: [hetrick:19930425.2347WET]
;      (define-key browse-mode-map "K" 'kill-buffer-and-window)
      (define-key browse-mode-map "k" 'kill-buffer)))

(defun browse-mode (&optional flag)
  "\\<browse-mode-map>
Minor mode for browsing through a file or buffer.

Simply defines the special single character commands to move cursor
around more easily.  To exit this mode, hit `\\[browse-mode]'; this
will make the buffer read/write (the assumption being that you want to
now edit the buffer).  If the buffer is under RCS control, and the
file is unlocked, you are prompted if you really want to edit the
buffer.  (See the documentation for the variable
`browse-check-rcs-lock'.)

This mode runs the hooks `browse-mode-hook' on entry and
`browse-mode-exit-hook' on exit.

You can have the buffer's mode line display the number of lines and
characters in the buffer; see the documentation for the functions
`browse-set-buffer-id' and `browse-restore-buffer-id'.

Commands:
0 .. 9  Digit argument
SPC	Scroll up
DEL	Scroll down
n	Next line  === also u,d => 1/2 window scroll have been added
p	Previous line  
,	Beginning of buffer
.	End of buffer
l	Recenter
g	Goto line
=	Show current cursor line and column
s	Search forward
r	Search backward
o	Other window
x	Exit; restores previous major mode
q	Remove this buffer's window
TAB	Switch to next buffer in Browse mode 
RET	Switch to next file buffer in Browse mode, whose file is in the 
	directory as the current buffer's visited file
k	Kill buffer
K       Kill buffer and its window"
  (interactive)
  (make-local-variable 'browse-prev-local-map)
  (make-local-variable 'browse-mode)
  (cond ((if flag
	     (> 0 flag)
	     (not browse-mode))
	 (setq browse-prev-local-map (current-local-map)
	       browse-mode t
	       buffer-read-only t)
	 (use-local-map browse-mode-map)
	 (set-buffer-modified-p (buffer-modified-p))
	 (run-hooks 'browse-mode-hook))
	((or (not (and browse-check-rcs-lock
		       (equal mode-line-process " <unlocked>")))
	     (yes-or-no-p "File is unlocked under RCS; edit anyway? "))
	 (setq buffer-read-only nil
	       browse-mode nil)
	 (use-local-map browse-prev-local-map)
	 (kill-local-variable 'browse-prev-local-map)
	 (set-buffer-modified-p (buffer-modified-p))
	 (run-hooks 'browse-mode-exit-hook))))

(defun browse-what-line (pos)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun browse-cursor-where ()
  "Show point's current line and column position."
  (interactive)
  (message "Line %d, Column %d"
	   (browse-what-line (point))
	   (1+ (current-column))))

;;; I'm putting in (u,d) here===========================

(defun half-up ()             
  "u - Scroll up by window-height/2"
  (interactive)
  (scroll-up (/ (window-height) 2)))

(defun half-down ()             
  "d - Scroll down by window-height/2"
  (interactive)
  (scroll-down (/ (window-height) 2)))

; ======================================================

(defun browse-next-buffer (&optional arg)
  "Switch to another buffer in Browse mode."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let ((buffers (cdr (buffer-list))))
    (while (not (zerop arg))
      (while (and buffers
		  (not (eq 'browse-mode
			   (cdr (assq 'major-mode
				      (buffer-local-variables (car buffers)))))))
	(setq buffers (cdr buffers)))
      (setq arg (1- arg)))
    (if (car buffers)
	(progn
	  (bury-buffer)
	  (switch-to-buffer (car buffers)))
	(error "No other Browse buffers!"))))

(defun browse-get-directory (buffer)
  (let ((file (buffer-file-name buffer)))
    (and file
	 (expand-file-name (file-name-directory file)))))

(defun browse-next-directory-buffer (&optional arg)
  "Switch to another Browse buffer with filename directory same as
current buffer.  That is, the next Browse buffer selected will be
visiting a file whose directory is the same as the current buffer's
visited file directory."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let ((buffers (cdr (buffer-list)))
	(directory (browse-get-directory (current-buffer))))
    (while (not (zerop arg))
      (while (and buffers
		  (not (and (cdr (assq 'major-mode
				       (buffer-local-variables (car buffers))))
			    (equal directory (browse-get-directory (car buffers))))))
	(setq buffers (cdr buffers)))
      (setq arg (1- arg)))
    (if (car buffers)
	(progn
	  (bury-buffer)
	  (switch-to-buffer (car buffers)))
	(error "Not browsing any other files in %s!" 
	       (substring directory 0 -1)))))

(defun browse-set-buffer-id ()
  "Make mode line display number of lines and characters in buffer.
This will display 

	{l/c}

next to the buffer name in Browse mode, where `l' is the number of
lines in the buffer, and `c' is the number of characters in the
buffer.

This function is meant to be used a hook to Browse mode; for example

	(setq browse-mode-hook 'browse-set-buffer-id
	      ; Set mode line to display # of lines and characters
              browse-mode-exit-hook 'browse-restore-buffer-id
	      ; Restore mode line on Browse mode exit
	      )

See also the documentation for `browse-restore-buffer-id'."
  (make-local-variable 'browse-mode-prev-buffer-id)
  (setq browse-mode-prev-buffer-id mode-line-buffer-identification
	mode-line-buffer-identification (list "Emacs: %b {"
					      (format "%dL/%dC"
						      (browse-what-line (point-max))
						      (buffer-size))
					      "}")))

(defun browse-restore-buffer-id ()
  "Remove the display of lines/characters in buffer from the mode line.
This function is meant to be used as the `browse-mode-exit-hook' if you have 
set `browse-mode-hook' to `browse-set-buffer-id'."
  (if (and (boundp 'browse-mode-prev-buffer-id)
	   (consp browse-mode-prev-buffer-id))
      (progn
	(setq mode-line-buffer-identification browse-mode-prev-buffer-id)
	(kill-local-variable 'browse-prev-buf-id))))

(defun browse-file (filename)
  (interactive "FBrowse File: ")
  (find-file filename)
  (browse-mode))

(defun browse-file-other-window (filename)
  (interactive "FBrowse file in other window: ")
  (find-file-other-window filename)
  (browse-mode))

