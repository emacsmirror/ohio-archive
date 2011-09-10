(defun ctl-meta-prefix (ctl-meta-arg)
  "Apply both Meta and Control to the next command character.
By gildea 1987"
  (interactive "P")
  (let ((ctl-meta-command
	 (key-binding (concat "\e" (char-to-string (logand 31 (read-char)))))))
    (if (commandp ctl-meta-command)
	(progn
	  (setq prefix-arg ctl-meta-arg)
	  (command-execute ctl-meta-command))
      (ding))))
