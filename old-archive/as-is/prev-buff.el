(defun switch-to-previous-buffer (n)
  "Switch to Nth previously selected buffer.  N defaults to the number
of windows plus 1.  That is, no argument switches to the most recently
selected buffer that is not visible.  If N is 1, repeated
calls will cycle through all buffers; -1 cycles the other way.  If N
is greater than 1, the first N buffers on the buffer list are rotated.
gildea 9 Dec 88"
  (interactive "P")
  (if (not n)
      (switch-to-buffer nil)
    (let ((buffer-list (buffer-list)))
      (setq n (prefix-numeric-value n))
      (cond ((= n 1)
	     (bury-buffer (current-buffer))
	     (setq n 2))
	    ((< n 0)
	     (setq buffer-list (nreverse buffer-list))
	     (setq n (- n)))
	    (t nil))
      (while (and (> n 1) buffer-list)
	(setq n (1- n))
	(setq buffer-list (cdr buffer-list))
	(while (eq (elt (buffer-name (car buffer-list)) 0) ? )
	  (setq buffer-list (cdr buffer-list))))
      (if buffer-list
	  (switch-to-buffer (car buffer-list))
	(error "There aren't that many buffers")))))
