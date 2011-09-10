;prev-buffer.el

(defun switch-to-previous-buffer (n)
  "Switch to Nth previously selected buffer.  N defaults to 2,
which switches to the most recently selected buffer.
If N is 1, repeated calls will cycle through all buffers, otherwise
the first N buffers on the buffer list are rotated."
  (interactive "P")
  (if (not n)
      (setq n 2)
    (setq n (prefix-numeric-value n)))
  (if (= n 1)
      (progn
	(bury-buffer (current-buffer))
	(setq n 2)))
  (let ((buffer-list (buffer-list)))
    (while (and (> n 1) buffer-list)
      (setq n (1- n))
      (setq buffer-list (cdr buffer-list))
      (while (eq (elt (buffer-name (car buffer-list)) 0) ? )
	(setq buffer-list (cdr buffer-list))))
    (if buffer-list
	(switch-to-buffer (car buffer-list))
      (error "There aren't that many buffers"))))

(global-set-key "\e\C-l" 'switch-to-previous-buffer)
