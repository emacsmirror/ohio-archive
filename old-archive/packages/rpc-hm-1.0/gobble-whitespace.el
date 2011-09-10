;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, March 26 1992 19:35 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/gobble-whitespace.el

(provide 'gobble-whitespace)

(defun gw-skip-blank-lines-and-comments ()
  (interactive)
  (let ((omd (match-data)))
    (unwind-protect
	(let ((current-line-string
	       (buffer-substring 
		(point)
		(save-excursion (end-of-line) (point)))))
	  (while (and (not (eobp))
		      (string-match 
		       "\\(^[ \t]*$\\)\\|\\(^[ \t]*[;]+.*$\\)"
		       current-line-string))
	    (forward-line 1)
	    (setq current-line-string
		  (buffer-substring 
		   (point)
		   (save-excursion (end-of-line) (point)))))
	  (skip-chars-forward " \t" (point-max)))
      (store-match-data omd))))
