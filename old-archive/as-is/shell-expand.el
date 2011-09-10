;Here is my solution to the file name expansion problem.  I bind this
;function to TAB, so that expansion occurs in-place while I am in the
;shell buffer.

(defun shell-expand-file-name ()	; Added 8/26/86 by Mark Ardis
  "Expand the file name before point."
  (interactive)
					; Local Variables
  (let (place start path stop name full-name)
					; Body
    (message "Completing...")
    (setq place (point))
    (beginning-of-line)
    (setq stop (point))
    (goto-char place)
    (if (search-backward " " stop t)
      (progn
	(forward-char)
	(setq start (point))
	(goto-char place)
	(if (search-backward "/" start t)
	  (progn
	    (forward-char)
	    (setq path (buffer-substring start (point)))
	  ) ; progn
	; else
	  (progn
	    (setq path "")
	    (goto-char start)
	  ) ; progn
	) ; if (search-backward "/" start t)
        (setq name (buffer-substring (point) place))
        (setq full-name (file-name-completion name
			      (concat default-directory path)))
	(if (not full-name)
	  (progn
	    (goto-char place)
	    (error "Cannot complete this!")
	    ) ; progn
	  ) ; if
	(if (not (equal full-name t))
	    (progn
	      (delete-region (point) place)
	      (insert full-name)
	      (if (> (length (file-name-all-completions
			   name (concat default-directory path)))
		     1)
		  (progn
		    (ding)
		    (message "Completed, but not unique!")
		    ) ; progn
		(message "Completed.")
		) ; if
	      ) ; progn
	  ; else
	  (progn
	    (goto-char place)
	    (message "Completed---already complete.")
	    ) ; progn
	  ) ; if
      ) ; progn
    ; else
      (progn
	(goto-char place)
	(error "Nothing to complete here!")
	) ; progn
    ) ; if (search-backward " " stop t)
  ) ; let
) ; defun shell-expand-file-name
