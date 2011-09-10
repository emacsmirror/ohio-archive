;;;===========================================================================
;;; Browse Kill Ring
;;;   written by Dave Emme
;;; Revised: Tue 09/20/88 12:10:39
;;;===========================================================================

(provide 'b-k-ring)

(defvar browse-kill-ring-keymap nil
  "Keymap used in browse-kill-ring buffer")

(defvar  browse-kill-one-window nil
  "True if there was only one window when browse-kill-ring was invoked.")

(defvar browse-kill-calling-window nil
  "Window which was current when browse-kill-ring was invoked.
The yank will go into this window.")

(defvar browse-kill-ring-bufname "*Browse Kill Ring*"
  "Name of the browse-kill-ring buffer.")

(if (not browse-kill-ring-keymap)
    (setq browse-kill-ring-keymap (make-sparse-keymap)))

(define-key browse-kill-ring-keymap " " 'browse-kill-ring-yank-line)
(define-key browse-kill-ring-keymap "y" 'browse-kill-ring-yank-line)
(define-key browse-kill-ring-keymap "Y" 'browse-kill-ring-yank-line)
(define-key browse-kill-ring-keymap "\C-y" 'browse-kill-ring-yank-line)

(defun browse-kill-ring ()
  "Browses the kill ring in another buffer.
Use C-y to yank most recent kill ring entry (number 1.) into the buffer
which was current when browse-kill-ring was invoked.  Use a numeric argument
(M-# C-y) to yank the corresponding entry.

If you move point into the browse-kill-ring buffer, the keys C-y, y, Y, or SPC
will yank the entry on the current line into the previous buffer.

Note that browse-kill-ring should always be called before its contents is used;
its buffer is not automatically updated when kills and yanks are done.  This is
normally not a problem, since yanks will cause the buffer to be deleted
automatically."
  (interactive)
  (setq browse-kill-one-window (one-window-p)
	browse-kill-calling-window (selected-window))
  (let ((n 1)
	(ring-length (length kill-ring))
	(orig-buf (current-buffer)))
    (if (= ring-length 0)
	(error "Kill ring is empty")
      (progn
	(pop-to-buffer browse-kill-ring-bufname)
	(setq buffer-read-only nil)
	(erase-buffer)
	(use-local-map browse-kill-ring-keymap)
	(while (<= n ring-length)
	  (insert (int-to-string n)
		  ". " 
		  (car kill-ring-yank-pointer)
		  "\n")
	  (setq n (1+ n))
	  (rotate-yank-pointer 1))
	(delete-backward-char 1)		;last "\n"
	(setq buffer-read-only t)
	(beginning-of-buffer)
	(pop-to-buffer orig-buf)
	(message "%d of %d kill-ring entries used."
		 ring-length kill-ring-max)))))

(defun browse-kill-ring-yank-line ()
  "Yank text represented by the current line in the
browse-kill-ring buffer into the calling buffer.
Normally bound to keys C-y, y, Y, SPC while in the
browse-kill-ring buffer."
  (interactive)
  (let ((line 0)
	(string)
	(mk))
    ;; We get the yank argument from the number at the beginning
    ;; of the line point is on.  In case of multi-line entries,
    ;; scan backwards until a numbered line is found.
    (save-excursion
      (while (= line 0)
	(beginning-of-line)
	(setq mk (point))
	(end-of-line)
	(setq string (buffer-substring mk (point)))
	(if (or (string-equal string "")
		(not (numberp
		      (setq line (car (read-from-string string))))))
	    (progn (setq line 0)
		   (previous-line 1)))))
    ;; "line" now has the logical number of the yank entry
    (select-window browse-kill-calling-window)
    (yank line)))

(defun browse-kill-yank-hook ()
  ;; Look for our buffer.  If it exists, delete it because yank will
  ;; probably invalidate its contents.  Also delete its window if it
  ;; caused the screen to split.
  (save-excursion
    (let ((buf-list (buffer-list))
	  (win (selected-window)))
      (catch 'browse-kill
	(while buf-list
	  (if (string-equal browse-kill-ring-bufname
			    (buffer-name (car buf-list)))
	      (progn
		(if browse-kill-one-window
		    (delete-windows-on (car buf-list)))
		(kill-buffer (car buf-list))
		(throw 'browse-kill nil))
	    (setq buf-list (cdr buf-list)))))
      (select-window win))))

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  ;; Modified to interact with browse-kill-ring.
  (interactive "*P")
  (browse-kill-yank-hook)
  (rotate-yank-pointer (if (listp arg) 0
			 (if (eq arg '-) -1
			   (1- arg))))
  (push-mark (point))
  (insert (car kill-ring-yank-pointer))
  (if (consp arg)
      (exchange-point-and-mark)))
