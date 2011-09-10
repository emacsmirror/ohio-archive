(require 'latexinfo)

(defun scheme-fmt-hook ()
  (message "Running Scheme formatting hooks...") (sit-for 1)
  (let ((case-fold-search nil)
	(alist scheme-alist))
    (strip-percents)
    (setq alist scheme-delete-list)
    (while alist
      (goto-char (point-min))
      (delete-matching-lines (car alist))
      (setq alist (cdr alist)))

    (setq alist scheme-alist)
    (while alist
      (goto-char (point-min))
      (replace-regexp (car (car alist)) (cdr (car alist)) nil)
      (setq alist (cdr alist))))
  (message "Running Scheme formatting hooks... done.") (sit-for 1)
  )

(setq scheme-alist
      '(
	("{ }" . "{}")
	("{\\\\it " . "\\\\i{")
	("{\\\\tt " . "\\\\code{")
	("{\\\\tt(" . "\\\\code{(")
	("\\\\item \\[" . "\\\\item[")
	("\\\\vest " . "")
	("{\\\\cf " . "\\\\code{")
	("{\\\\em " . "\\\\b{")
	("{\\\\em{}" . "\\\\b{")
	("\\\\verb|#|" . "#")
	("\\\\#{}" . "#")
	("\\\\#" . "#")

	("\\\\nopagebreak{}" . "")
	("\\\\nopagebreak" . "")
	("\\\\unsection{}" . "")
	("\\\\unsection" . "")
	("\\\\unpenalty{}" . "")
	("\\\\unpenalty" . "")
	("\\\\nobreak{}" . "")
	("\\\\nobreak" . "")
	("\\\\rm." . ".")
	
	("\\\\lambdaexp{}" . "lambda expression")
	("\\\\lambdaexp" . "lambda expression")
	("\\\\Lambdaexp{}" . "Lambda expression")
	("\\\\Lambdaexp" . "Lambda expression")
	("\\\\exprtype" . "syntax")
	
	("^\\\\proto{\\(.*\\)}{\\(.*\\)}{\\([^}]*\\)}" .
	 "\\\\head{\\1}{}{\\3}
{(\\\\fb{\\1}\\2)}")

	("^\\\\vproto{\\(.*\\)}{\\([^}]*\\)}" .
	 "\\\\head{\\1}{}{\\2}
{\\\\vb{\\1}}")

	("^\\\\pproto{\\(.*\\)}{\\([^}]*\\)}" .
	 "\\\\head{}{}{\\2}
{\\1}")

	("^\\\\rproto{\\(.*\\)}{\\(.*\\)}{\\(.*\\)}$" .
	 "\\\\head{\\1}{}{\\3}
{(\\\\code{\\1}\\2)}")
	
	("\\\\dotsfoo{}" . "\\\\dots{}")
	("\\\\dotsfoo" . "\\\\dots")

	("\\\\schfalse{}" . "\\\\code{#f}")
	("\\\\schfalse" . "\\\\code{#f}")
	("\\\\schtrue{}" . "\\\\code{#t}")
	("\\\\schtrue" . "\\\\code{#t}")
	("\\\\backquote{}" . "`")
	("\\\\backquote" . "`")
	("\\\\backwhack{}" . "\\\\back")
	("\\\\backwhack" . "\\\\back")
	("\\\\atsign" . "@")
	("\\\\sharpsign" . "#")
	("\\\\verticalbar" . "|")
	("~\\\\cite{" . " \\\\cite{")
	))

(setq scheme-delete-list
      '())

(defun strip-percents ()
  (while (search-forward "%" nil t)
    (forward-char -1)
    (if (bobp)
	(kill-line nil)
	(if (save-excursion
	      (forward-char -1)
	      (looking-at "\\\\"))
	    (forward-char 1)
	  (kill-line nil)))))

(put 'meta 'texinfo-format 'latexinfo-hyper)
(put 'hyper 'texinfo-format 'latexinfo-hyper)
(defun latexinfo-hyper ()
  (insert "\\var{<" (texinfo-parse-arg-discard) ">}")
  (goto-char texinfo-command-start))

(put 'hyperi 'texinfo-format 'latexinfo-hyperi)
(defun latexinfo-hyperi ()
  (insert "\\var{<" (texinfo-parse-arg-discard) "-1>}")
  (goto-char texinfo-command-start))

(put 'hyperii 'texinfo-format 'latexinfo-hyperii)
(defun latexinfo-hyperii ()
  (insert "\\var{<" (texinfo-parse-arg-discard) "-2>}")
  (goto-char texinfo-command-start))

(put 'hyperj 'texinfo-format 'latexinfo-hyperj)
(defun latexinfo-hyperj ()
  (insert "\\var{<" (texinfo-parse-arg-discard) "-i>}")
  (goto-char texinfo-command-start))

(put 'hypern 'texinfo-format 'latexinfo-hypern)
(defun latexinfo-hypern ()
  (insert "\\var{<" (texinfo-parse-arg-discard) "-n>}")
  (goto-char texinfo-command-start))

(put 'vari 'texinfo-format 'latexinfo-vari)
(defun latexinfo-vari ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-1}")
  (goto-char texinfo-command-start))

(put 'varii 'texinfo-format 'latexinfo-varii)
(defun latexinfo-varii ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-2}")
  (goto-char texinfo-command-start))

(put 'variii 'texinfo-format 'latexinfo-variii)
(defun latexinfo-variii ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-3}")
  (goto-char texinfo-command-start))

(put 'variv 'texinfo-format 'latexinfo-variv)
(defun latexinfo-variv ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-4}")
  (goto-char texinfo-command-start))

(put 'varj 'texinfo-format 'latexinfo-varj)
(defun latexinfo-varj ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-i}")
  (goto-char texinfo-command-start))

(put 'varn 'texinfo-format 'latexinfo-var-n)
(defun latexinfo-var-n ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-n}")
  (goto-char texinfo-command-start))

(put 'vr 'texinfo-format 'latexinfo-vr)
(defun latexinfo-vr ()
  (insert "\\var{" (texinfo-parse-arg-discard) "}")
  (goto-char texinfo-command-start))

(put 'vri 'texinfo-format 'latexinfo-vri)
(defun latexinfo-vri ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-1}")
  (goto-char texinfo-command-start))

(put 'vrii 'texinfo-format 'latexinfo-vrii)
(defun latexinfo-vrii ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-2}")
  (goto-char texinfo-command-start))

(put 'vriii 'texinfo-format 'latexinfo-vriii)
(defun latexinfo-vriii ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-3}")
  (goto-char texinfo-command-start))

(put 'vriv 'texinfo-format 'latexinfo-vriv)
(defun latexinfo-vriv ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-4}")
  (goto-char texinfo-command-start))

(put 'vrj 'texinfo-format 'latexinfo-vrj)
(defun latexinfo-vrj ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-i}")
  (goto-char texinfo-command-start))

(put 'vrn 'texinfo-format 'latexinfo-vr-n)
(defun latexinfo-vr-n ()
  (insert "\\var{" (texinfo-parse-arg-discard) "-n}")
  (goto-char texinfo-command-start))

(put 'defining 'texinfo-format 'latexinfo-defining)
(defun latexinfo-defining ()
  (insert "\\emph{" (texinfo-parse-arg-discard) "}")
  (goto-char texinfo-command-start))

(put 'ide 'texinfo-format 'texinfo-format-noop)

(put 'mainschindex 'texinfo-format 'texinfo-parse-arg-discard)
(put 'mainindex 'texinfo-format 'texinfo-parse-arg-discard)
(put 'schindex 'texinfo-format 'texinfo-parse-arg-discard)
(put 'sharpindex 'texinfo-format 'texinfo-parse-arg-discard)
(put 'index 'texinfo-format 'texinfo-parse-arg-discard)

(put 'domain 'texinfo-format 'texinfo-format-noop)
(put 'nodomain 'texinfo-format 'texinfo-parse-arg-discard)
(put 'todo 'texinfo-format 'texinfo-parse-arg-discard)

(put 'ev 'texinfo-format 'latexinfo-format-ev)

(defun latexinfo-format-ev ()
  (texinfo-parse-noarg)
  (insert "        => "))

(put 'lev 'texinfo-format 'latexinfo-format-lev)
(defun latexinfo-format-lev ()
  (texinfo-parse-noarg)
  (insert "\n    => "))

(put 'unspecified 'texinfo-format 'latexinfo-format-unspecified)
(defun latexinfo-format-unspecified ()
  (texinfo-parse-noarg)
  (insert "\\emph{unspecified}")
  (goto-char texinfo-command-start))

(put 'error 'texinfo-format 'latexinfo-format-error)
(defun latexinfo-format-error ()
  (texinfo-parse-noarg)
  (insert "\\emph{error}"))

(put 'schemenoindent 'texinfo-format 'texinfo-format-schemenoindent)
(defun texinfo-format-schemenoindent ()
  (texinfo-push-stack 'example nil)
  (texinfo-discard-line))

(put 'schemenoindent 'texinfo-end 'texinfo-end-schemenoindent)
(defun texinfo-end-schemenoindent ()
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'example)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'tabular 'texinfo-format 'texinfo-format-tabbing)
(put 'tabbing 'texinfo-format 'texinfo-format-tabbing)
(defun texinfo-format-tabbing ()
  (texinfo-push-stack 'example nil)
  (texinfo-discard-line-with-args)
  )

(put 'tabular 'texinfo-end 'texinfo-end-tabbing)
(put 'tabbing 'texinfo-end 'texinfo-end-tabbing)
(defun texinfo-end-tabbing ()
  (texinfo-discard-line)
  (let ((stacktop
	 (texinfo-pop-stack 'example)))
    (texinfo-do-tabbing (nth 1 stacktop))))

(defun texinfo-do-tabbing (from)
  (let ((end (point)))
    (save-excursion
      (goto-char from)
      (while (search-forward "&" end t)
	(if (save-excursion
	      (bobp)
	      (forward-char -2)
	      (not (looking-at "\\\\")))
	    (progn
	      (delete-char -1)
	      (insert "\t")
	      ))))))

(put 'scheme 'texinfo-format 'texinfo-format-example)
(put 'scheme 'texinfo-end 'texinfo-end-example)

(put 'rationale 'texinfo-format 'texinfo-format-rationale)
(put 'rationale 'texinfo-end 'texinfo-discard-command)
(defun texinfo-format-rationale ()
  (texinfo-discard-line)
  (insert "Rationale: "))

(put 'note 'texinfo-format 'texinfo-format-note)
(put 'note 'texinfo-end 'texinfo-discard-command)
(defun texinfo-format-note ()
  (texinfo-discard-line)
  (insert "Note: "))

(put 'header 'texinfo-format 'texinfo-format-header)
(put 'header 'texinfo-end 'texinfo-discard-command)
(defun texinfo-format-header ()
  (save-excursion
    (forward-char 1)
    (forward-sexp 1)
    (delete-char -1))
  (goto-char texinfo-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))


(put 'evalsto 'texinfo-format 'latexinfo-format-arrow)
(put 'unsection 'texinfo-format 'texinfo-parse-noarg)

(defun latexinfo-format-syntax ()
(put 'syntax 'texinfo-format 'latexinfo-format-syntax)  
  (texinfo-parse-noarg)
  (insert "\\b{Syntax:} ")
  (goto-char texinfo-command-start))

(defun latexinfo-format-semantics ()
(put 'semantics 'texinfo-format 'latexinfo-format-semantics)  
  (texinfo-parse-noarg)
  (insert "\\b{Semantics:} ")
  (goto-char texinfo-command-start))


(defun find-entry (byte)
  (goto-char byte)
  (if (looking-at "(")
      (setq byte (1+ byte)))
  (buffer-substring byte
		    (progn
		      (skip-chars-forward "^\n\b)")
		      (point))))

(put 'modeline 'texinfo-format 'texinfo-format-code)

(put 'f 'texinfo-format 'texinfo-format-noop)
(put 'v 'texinfo-format 'texinfo-format-noop)
(put 'fb 'texinfo-format 'texinfo-format-noop)
(put 'vb 'texinfo-format 'texinfo-format-noop)
(put 'ide 'texinfo-format 'texinfo-format-noop)

