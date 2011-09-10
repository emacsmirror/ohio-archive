(defun caseless-string< (arg1 arg2) "GENDOC"
  (string< (downcase arg1) (downcase arg2)))

(defun  make-documentation () "GENDOC"
  (interactive nil)
  (let ((functions
	 (progn
	   (message "sorting functions...")
	   (sort (all-completions "" obarray 'fboundp ) 'caseless-string<)))
	(variables
	 (progn
	   (message "sorting variables...")
	   (sort (all-completions "" obarray 'boundp ) 'caseless-string<))))
    (get-buffer-create "doc-info.tex")
    (with-output-to-temp-buffer "doc-info.tex"
      (switch-to-buffer "doc-info.tex")
      (erase-buffer)
      (message "%s" "generating function info")
      (insert "\\documentstyle[twoside,twocolumn,doc-info]{article}
\\makeindex
\\title{Emacs Variables and Functions\\\\" (emacs-version) "}
\\author{gendoc.el}
\\begin{document}
\\maketitle
\\clearpage
\\footnotesize
\\tableofcontents
\\clearpage
\\sloppy

\\section{Functions}
")
      (while functions
	(if (car functions)
	    (print-doc (car functions)))
	(setq functions (cdr functions)))
      (terpri)
      (insert "
\\newpage

\\index{-----}

\\section{Variables}
")
      (message "%s" "generating variable info")
      (while variables
	(if (car variables)
	    (print-var-doc (car variables)))
	(setq variables (cdr variables)))
      (insert "
\\input{doc-info.ind}

\\end{document}
")
      (message "%s" "running replace-regexp")
      (goto-char 1)
      (while (re-search-forward
	      "^\\\\\\(subsection\\|index\\){.*[$%<>].*$" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-string "$" "\\$")(goto-char (point-min))
	  (replace-string "%" "\\%")(goto-char (point-min))
	  (replace-string ">" "$>$")(goto-char (point-min))
	  (replace-string "<" "$<$"))
	(goto-char (match-end 0)))
      (write-file "doc-info.tex")
;;
;; create a file doc-info.sty for our latex option `doc-info' which gives a bit
;; more room for the `section.subsection' number before the text of
;; the section heading in the index. When we had `x.xxx' the text
;; was hard up against the last number. We changed the 2.3em to 2.8em
;; for extra space.
;; Also set the margins to mostly fill an A4 page.
;;
      (get-buffer-create "doc-info.sty")
      (with-output-to-temp-buffer "doc-info.sty"
	(switch-to-buffer "doc-info.sty")
	(erase-buffer)
	(insert "\\def\\l@subsection{\\@dottedtocline{2}{1.5em}{2.8em}}
\\oddsidemargin -16pt
\\evensidemargin -36pt
\\topmargin -60pt
\\textheight 707pt
\\textwidth 500pt
")
	(write-file "doc-info.sty")))))


(defun print-doc (func) "GENDOC"
  (interactive nil)
  (let ((Func (intern func)))
    (let ((doc-str (documentation Func)))
      (if (equal doc-str "GENDOC")
	  (message "Discarding Function %s..." func)
	(message "Function %s..." func)
	(insert "\\subsection{" func "}
")
	(index-entry func)
	(insert "
\\begin{verbatim}
")
	(if (setq keys (where-is-internal Func))
	    (let ((next-key (car keys)))
	      (insert "Key-Mapping: ")
	      (while next-key
		(insert " `" (key-description next-key) "'")
		(setq next-key (car (setq keys (cdr keys)))))
	      (insert "

")))
	(insert
	 (if doc-str
	     (if (equal doc-str "")
		 "I don't know about this one"
	       doc-str)
	   "undocumented")
	 "
\\end{verbatim}

")
	nil))))

(defun pretty-string (str) "GENDOC"
  (let ((newstr "")(a 0)(len (length str)))
    (while (/= len a)
      (setq newstr (concat
		    newstr
		    (text-char-description
		     (string-to-char (substring str a (1+ a))))))
      (setq a (1+ a)))
    newstr))

(defun print-var-doc (var) "GENDOC"
  (interactive nil)
  (let ((Var (intern var)))
    (let ((doc-str (substitute-command-keys
		    (get Var 'variable-documentation))))
      (message "Variable %s..." var)
      (insert "\\subsection{" var "}
")
      (index-entry var)
      (insert "
\\begin{verbatim}
Value: "
	      (let ((valstr (prin1-to-string (eval Var))))
		(if (> (length valstr) 100)
		    "..."
		  (pretty-string valstr))) "

"
		  (if doc-str
		      (if (equal doc-str "")
			  "I don't know about this one!"
			doc-str)
		    "variable not documented.")
		  "
\\end{verbatim}

")
    nil)))
  
(defun index-entry (name) "GENDOC"
  (interactive nil)
;;
;; do a couple of special cases first
;;
  (if (or (string-equal name "1-") (string-equal name "-"))
      (insert "\\index{" name " " name "}
")
;;
;; OK general case of "xx-yy" ignore trailing '-'s and double '--'s
;; and produce index entries for 'xx' and 'yy'.
;;
    (let ((func-name name) end)
      (while (and func-name (> (length func-name) 0))
	(if (not (setq end (string-match "-" func-name)))
	    (setq end (length func-name)))
	(if (not (= 0 (length (setq substr (substring func-name 0 end)))))
	    (insert "\\index{" substr " " name "}
"))
	(setq func-name (if (not (= end (length func-name)))
			    (substring func-name (1+ end) nil)))))))
