(setq save-all nil)
(defun nodify-file () "
Insert the \\node constructs into a texinfo buffer that contains
only \\chapter, \\section ... commands."
       (interactive)
       (save-excursion
	      (let (chap last-chapter
			  last-section
			  last-subsection
			  last-subsubsection
			  all up up-all)
		     (goto-char (point-min))
		     (delete-matching-lines "^\\node")
		     (goto-char (point-min))
		     (while (re-search-forward
				   "^\\\\chapter\\|\\\\section\\|\\\\subsection\\|\\\\subsubsection"
				   nil t)
			    (re-search-forward "{\\([^}]*\\)}")
			    (replace-match "{\\1}\n\\\\cindex{\\1}" t))
		     (goto-char (point-min))
		     (while (re-search-forward
				   "^\\\\chapter\\|\\\\section\\|\\\\subsection\\|\\\\subsubsection"
				   nil t)
			    (let ((match (buffer-substring
						(match-beginning 0)
						(match-end 0))))
				   (cond ((equal match "\\chapter")
						(setq last-chapter
						       (nodify-get-name-f))
						(setq up "Top"))
					  ((equal match "\\section")
						 (setq last-section
							(nodify-get-name-f))
						 (setq up last-chapter))
					  ((equal match "\\subsection")
						 (setq last-subsection
							(nodify-get-name-f))
						 (setq up last-section))
					  ((equal match "\\subsubsection")
						 (setq last-subsubsection
							(nodify-get-name-f))
						 (setq up last-subsection))
					  (t (message "Help - no match  %s" match) (sit-for 10)))
			    (setq up-all (cons up up-all))
			    (if (bolp) (forward-char 1))
			    (setq all (cons (count-lines 1 (point)) all)))
			    )
		     (setq save-all all)
		     (message "doing the last section") (sit-for 1)
		     (goto-line (car all))
		     (insert "\\node "
			    (nodify-get-name-at-line (car all))
			    ", Top, "
			    (nodify-get-name-at-line (cadr all))
			    ", " (car up-all) "\n")
		     (message "doing the other sections") (sit-for 1)
		     (while (> (length all) 2)
			    (goto-line (cadr all))
			    (let ((cur (cadr all))
					(next (1+ (car all)))
					(prev (caddr all))
					(up (cadr up-all)))
				   (insert "\\node "
					  (nodify-get-name-at-line cur) ", "
					  (nodify-get-name-at-line next) ", "
					  (nodify-get-name-at-line prev) ", "
					  up "\n"))
			    (setq up-all (cdr up-all))
			    (setq all (cdr all)))
		     (message "doing the other sections...done") (sit-for 1)
		     (goto-line (cadr all))
		     (insert "\\node "
			    (nodify-get-name-at-line (cadr all)) ", "
			    (nodify-get-name-at-line (1+ (car all)))
			    ", Top, Top\n")
		     (goto-char (point-min))
		     (if (re-search-forward "^\\\\setfilename" nil t)
			    (progn
				   (forward-line 1)
				   (insert "\\node Top, "
					  (nodify-get-name-at-line
						 (1+ (cadr all)))
					  ", (dir)Top, (dir)Top\n")))
		     (message "doing the chapter menus") (sit-for 1)
		     (nodify-chapter-menus)
		     (message "doing the chapter menus...done") (sit-for 1)
		     )))

(defun nodify-chapter-menus ()
       (let (chap (eoc (point-max)))
	      (goto-char (point-max))
	      (while (re-search-backward "^.chapter" nil t)
		     (setq chap (cons (nodify-get-name) chap))
		     (make-section-menu "\\\\section" (point) eoc)
		     (setq eoc (point)))
	      (goto-char (point-min))
	      (if (re-search-forward "\\chapter" nil t)
		     (make-menu chap))
	      ))

(defun make-section-menu (section top bot)
       (save-excursion
	      (let (chap)
		     (goto-char bot)
		     (while (re-search-backward section top t)
			    (setq chap (cons (nodify-get-name) chap)))
		     (goto-char top)
		     (if (> (length chap) 1)
			    (progn
				   (re-search-forward section bot t)
				   (make-menu chap)))
		     chap)
	      )
       )

(defun make-menu (chap)
       (forward-line -1)
       (insert "\n\\begin{menu}\n")
       (while (car chap)
	      (insert "* " (car chap) "::\n")
	      (setq chap (cdr chap)))
       (insert "\\end{menu}\n\n"))

(defun nodify-get-name-at-line (lineno)
       (save-excursion
	      (goto-line (+ 1 lineno))
	      (nodify-get-name)))

(defun nodify-get-name ()
       (save-excursion
	      (buffer-substring
	      (progn (forward-word 1)
		     (skip-chars-forward "* {")
		     ;; (if (looking-at "[ {]") (forward-char 1))
		     (point))
	      (progn (end-of-line) (forward-char -1) (point)))))

(defun nodify-get-name-f ()
       (save-excursion
	 (buffer-substring
	  (progn 
	    (skip-chars-forward "* {")
	    (point))
	  (progn (end-of-line) (forward-char -1) (point)))))

(defun cadr (x)
       (car (cdr x)))
(defun caddr (x)
       (car (cdr (cdr x))))

