;;; tplreplace.el -- Replace commands for Emacs.
;;; Copyright (C) 1985 Richard M. Stallman.
;;; Modified by Mark Ardis, Wang Institute, 12/14/86 for template-mode

(provide 'tplreplace)

(defun perform-replace-tpl (from-string to-string
		        query-flag regexp-flag delimited-flag
			search-function position-function replace-function
			&optional reposition-function)
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-string from-string)
	(keep-going t)
	(lastrepl nil)			;Position after last match considered.
	(help-form
	 '(concat "Query replacing "
		  from-string " with " to-string ".\n\n"
		  (substitute-command-keys query-replace-help))))
    (push-mark)
    (push-mark)
    (while (and keep-going
		(not (eobp))
		(progn
		 (set-mark (point))
		 (funcall search-function search-string nil t)))
      ;; Don't replace the null string 
      ;; right after end of previous replacement.
      (if (eq lastrepl (point))
	  (forward-char 1)
	(undo-boundary)
	(if (not query-flag)
	    (progn
	      (setq tpl-query-flag nil)
	      (funcall position-function)
	      (funcall replace-function from-string to-string)
	      )
	  (let (done replaced)
	    (setq tpl-query-flag t)
	    (while (not done)
	      (message "Query replacing %s with %s: " from-string to-string)
	      ;; Preserve the match data.  Process filters and sentinels
	      ;; could run inside read-char..
	      (let ((data (match-data)))
		(setq char (read-char))
		(store-match-data data))
	      (cond ((not (memq char '(?\e ?\ ?\, ?\. ?! ?\177 ?\C-r ?\C-w ?^)))
		     (setq keep-going nil)
		     (setq unread-command-char char)
		     (setq done t))
		    ((= char ?\e)
		     (setq keep-going nil)
		     (setq done t))
		    ((= char ?^)
		     (goto-char (mark))
		     (setq replaced t))
		    ((= char ?\ )
		     (or replaced
			 (progn
			   (funcall position-function)
			   (funcall replace-function from-string to-string)
			   ))
		     (setq done t))
		    ((= char ?\.)
		     (or replaced
			 (progn
			   (funcall position-function)
			   (funcall replace-function from-string to-string)
			   ))
		     (setq keep-going nil)
		     (setq done t))
		    ((and (not replaced) (= char ?\,))
		     (progn
		       (funcall position-function)
		       (funcall replace-function from-string to-string)
		       )
		     (setq replaced t))
		    ((= char ?!)
		     (or replaced
			 (progn
			   (funcall position-function)
			   (funcall replace-function from-string to-string)
			   ))
		     (setq done t query-flag nil))
		    ((= char ?\177)
		     (setq done t))
		    ((= char ?\C-r)
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit)))))
		    ((= char ?\C-w)
		     (delete-region (point) (mark))
		     (save-excursion (recursive-edit))
		     (setq replaced t)))
	      )))
	(setq lastrepl (point)))
      (if reposition-function
	  (funcall reposition-function)
	) ; if
      )
    (pop-mark)
    (message "Done")
    (setq tpl-query-flag t)
    keep-going))
