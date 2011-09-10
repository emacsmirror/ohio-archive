(require 'latexinfo)

;; Praxis
;; Praxima
;; pcp
;; PC
;; PCES


(put 'ifpraxima 'latexinfo-format 'latexinfo-format-ifpraxima)
(defun latexinfo-format-ifpraxima ()
  (delete-region latexinfo-command-start
		 (progn (re-search-forward "\\\\end[ {]ifpraxima[ 	}\n]")
			(point))))

(put 'endifpraxima 'latexinfo-format 'latexinfo-discard-line)

(put 'praxima 'latexinfo-format 'latexinfo-format-Praxima)
(defun latexinfo-format-Praxima ()
  (latexinfo-parse-noarg)
  (put 'ifpraxima 'latexinfo-format 'latexinfo-discard-line)
  (put 'ifpraxima 'latexinfo-end 'latexinfo-discard-command)
  )

(put 'Praxis 'latexinfo-format 'latexinfo-format-Praxis)
(defun latexinfo-format-Praxis ()
  (latexinfo-parse-noarg)
  (insert "Praxis"))

(put 'Macsyma 'latexinfo-format 'latexinfo-format-Macsyma)
(defun latexinfo-format-Macsyma ()
  (latexinfo-parse-noarg)
  (insert "Macsyma"))

(put 'pcp 'latexinfo-format 'latexinfo-discard-line-with-args)

(put 'PC 'latexinfo-format 'latexinfo-format-PC)
(defun latexinfo-format-PC ()
  (latexinfo-parse-noarg)
  (insert "Personal Consultant"))

(put 'PCES 'latexinfo-format 'latexinfo-format-PCES)
(defun latexinfo-format-PCES ()
  (latexinfo-parse-noarg)
  (insert "Personal Consultant Expert System"))

(put 'boxed 'latexinfo-format 'latexinfo-format-boxed)
(put 'boxed 'latexinfo-end 'latexinfo-end-boxed)
(defun latexinfo-format-boxed ()
  (latexinfo-discard-line)
  (insert "\nThe menu asks the question:\n\n")
  (setq latexinfo-command-start (point))
  (setq latexinfo-command-end (1+ (point)))
  (latexinfo-push-stack 'example nil)
  (setq fill-column (- fill-column 5))
  )

(defun latexinfo-end-boxed ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'example)))
    (latexinfo-do-itemize (nth 1 stacktop)))
  (insert "\nThe choices are:\n"))


