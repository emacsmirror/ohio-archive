; you should byte compile this file for efficiency sake

; needed for auto indent

(defun doug-newline ()
  "Perform newline and indent."
  (interactive)
  (newline)
  (indent-according-to-mode))

; a vi like indent fuction for fundamental mode

(defun doug-indent ()
  "Perform indent."
  (interactive)
  (let ((tmp))
    (cond
     ((equal mode-line-buffer-identification '("Insert:%17b"))
      (save-excursion
	(mvi-forward-line -1)
	(setq tmp (current-column)))
      (indent-to tmp 0))
     ((eq this-command 'mvi-O)
      (save-excursion
	(mvi-forward-line 1)
	(setq tmp (current-column)))
      (indent-to tmp 0))
     (t
      (save-excursion
	(mvi-forward-line -1)
	(setq tmp (current-column)))
      (mvi-forward-line 0)
      (delete-region (point) (progn (beginning-of-line) (point)))
      (indent-to tmp 0)))))

; an example of setup for use with the nroff mm macro package.
; illistrates the use of the text sexp stuff

(defun mm-mode ()
  (interactive)
  (make-variable-buffer-local 'paragraph-start)
  (setq paragraph-start "^\\.P$\\|^\\.H ")
  (make-variable-buffer-local 'page-delimiter)
  (setq page-delimiter "^\\.H ")
  (setq outline-regexp "\\.H 1\\|\\.H 2 \\|\\.H 3 .\\|\\.H 4 ..\\|\\.H 5 ...")
  (make-variable-buffer-local 'text-sexp-delim)
  (setq text-sexp-delim "^\\.H [12345]\\|^\\.[ABV]L\\|^\\.LE\\|^\\.D[SFE]")
  (make-variable-buffer-local 'text-sexp-data)
  (setq text-sexp-data '(
		 (".H 1" . ("^\\.H 1" t "\\.LE\\|\\.DE" nil nil t)) 
		 (".H 2" . ("^\\.H [12]" t "\\.LE\\|\\.DE" nil nil t)) 
		 (".H 3" . ("^\\.H [123]" t "\\.LE\\|\\.DE" nil nil t)) 
		 (".H 4" . ("^\\.H [1234]" t "\\.LE\\|\\.DE" nil nil t)) 
		 (".H 5" . ("^\\.H [12345]" t "\\.LE\\|\\.DE" nil nil t)) 
		 (".AL" . ("^\\.LE" t "\\.H" t nil nil))
		 (".BL" . ("^\\.LE" t "\\.H" t nil nil))
		 (".VL" . ("^\\.LE" t "\\.H" t nil nil))
		 (".DS" . ("^\\.DE" t "\\.H" t nil nil))
		 (".DF" . ("^\\.DE" t "\\.H" t nil nil))
		 )))
