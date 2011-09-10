;;; LCD Archive Entry:
;;; short-help|Chris Halverson|halverso@staff.tc.umn.edu|
;;; Makes a 5 line buffer that contains useful Emacs commands|
;;; 19-May-1993|1.1|~/misc/short-help.el.Z|

;;; This was created to aid new users in learning some basic Emacs
;;; commands. This will create a 5 line window within the largest window
;;; and then create a separate buffer that contains the commands. It 
;;; contains  the key-map of C-x C-k to kill the mode.

;;; Load with (autoload 'short-help-mode "short-help" "" t)

;;; BUGS:
;;;  o Doesn't seem to cooperate with list-buffers (C-x C-b). list-buffers
;;;    puts the list in the short-help buffer. This is a small bug, IMHO, 
;;;    since the target audience of this buffer are new and inexperienced
;;;    users who _probably_ won't make much use of list-buffers.
  
(defun short-help-mode ()
  "A separate buffer to show some basic Emacs commands."
  (interactive)
  (let ((buf (current-buffer)))
    (setq major-mode 'short-local-help)	; Mode name
    (short-create-buffer)		; Call the function to make the buffer
    (setq mode-name "*short-help*")
    (switch-to-buffer "*short-help*")
    (short-set-mode-line)
    (insert				; Put the stuff in the buffer
     "C-n next-line     C-p previous-line"
     "  C-h help       C-x C-f find-file\n"
     "C-f forward-char  C-b backward-char"
     "  C-h t tutorial C-x C-s save-buffer\n"
     "M-v scroll-back   C-v scroll-up"
     "      C-l redraw     C-x C-i insert file\n"
     "C-_ undo          C-x k kill-buffer"
     "                 C-x C-c exit emacs")
    (setq buffer-read-only t)		; Make it read only
    (pop-to-buffer buf)			; Switch back to orig buffer
    (global-set-key "\C-x\C-k" 'kill-buffer-close-window)
    (message "Welcome to the short-help buffer!") ; Be friendly
    ))

(defun short-create-buffer ()
  "Creates the Local buffer and sets the height to 5"
  (let ((w (get-largest-window)))
    (setq w (split-window w (- (window-height) 5 )))
    (set-window-buffer w (current-buffer))
    (select-window w))
  (set-buffer (get-buffer-create "*short-help*")))

(defun short-set-mode-line ()
  "Sets the mode line"
  (setq mode-line-format
	(concat "  *short-help*    "
		"  C = Control  M = Meta (Esc)"
		"   C-x C-k to quit short-help"))
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun kill-buffer-close-window (&optional arg)
  "kills current buffer and closes the window it was in.  if ARG, just bury."
  (interactive "P")
  (global-unset-key "\C-x\C-k")		; Unsets the key-map
  (if arg
      (bury-buffer)
    (kill-buffer "*short-help*"))
  (if (or (/= (1+ (window-height)) (screen-height))
	  (/= (window-width) (screen-width)))
      (delete-other-windows)))
