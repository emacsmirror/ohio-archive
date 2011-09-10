(defun tex-to-latexinfo ()
  (interactive)
  (let ((case-fold-search nil)
	(alist latex-alist))
    ;; Fix the header for LaTeX
    (goto-char (point-min))
    (if (re-search-forward "^\\\\input texinfo\\(.*\\)" nil t)
	(replace-match "\\\\documentstyle[12pt,twoside,latexinfo]{report}
\\\\pagestyle{headings}

\\\\begin{document}

;; Delete any of these if you don't want that particular index.
\\\\newindex{cp}
\\\\newindex{vr}
\\\\newindex{fn}
\\\\newindex{tp}
\\\\newindex{pg}
\\\\newindex{ky}

" t nil))
    ;; Assume that any \ from here on in the file are to print out as \
    (replace-string "\\" "\\back " nil)
    (goto-char (point-min))
    (if (yes-or-no-p
	 "Would you like all occurences of `@@' replaced by `@'? ")
	(replace-string "@@" "@" nil)
      (if (yes-or-no-p 
	   "Would you like all occurences of `@@' replaced by `\\'? ")
	  (replace-string "@@" "\\back " nil)
	(error "You better make up your mind between one of these two options.")))
    (setq alist latex-delete-list)
    (while alist
      (goto-char (point-min))
      (delete-matching-lines (car alist))
      (setq alist (cdr alist)))
    (if (re-search-forward "^@appendix"  nil t nil)
			   (progn
			     (beginning-of-line 1)
			     (insert "\\apendix\n")))
    (setq alist latex-alist)
    (while alist
      (goto-char (point-min))
      (replace-regexp (car (car alist)) (cdr (car alist)) nil)
      (setq alist (cdr alist)))
    ))


(setq latex-alist
  '(
    ;; begin end environments
    ("^@defcv" . "\\\\begin{defcv}")
    ("^@deffn" . "\\\\begin{deffn}")
    ("^@defivar" . "\\\\begin{defivar}")
    ("^@defmac" . "\\\\begin{defmac}")
    ("^@defmethod" . "\\\\begin{defmethod}")
    ("^@defopt" . "\\\\begin{defopt}")
    ("^@defspec" . "\\\\begin{defspec}")
    ("^@deftp" . "\\\\begin{deftp}")
    ("^@defun" . "\\\\begin{defun}")
    ("^@defvar" . "\\\\begin{defvar}")
    ("^@defvr" . "\\\\begin{defvr}")
    ("^@description" . "\\\\begin{description}")
    ("^@display" . "\\\\begin{display}")
    ("^@enumerate" . "\\\\begin{enumerate}")
    ("^@example" . "\\\\begin{example}")
    ("^@flushleft" . "\\\\begin{flushleft}")
    ("^@format" . "\\\\begin{format}")
    ("^@group" . "\\\\begin{same}")
    ("^@ignore" . "\\\\begin{ignore}")
    ("^@ifinfo" . "\\\\begin{ifinfo}")
    ("^@iftex" . "\\\\begin{iftex}")
    ("^@itemize" . "\\\\begin{itemize}")
    ("^@lisp" . "\\\\begin{lisp}")
    ("^@menu" . "\\\\begin{menu}")
    ("^@quotation" . "\\\\begin{quotation}")
    ("^@tex" . "\\\\begin{tex}")
    ;; chamge the name of the group command
    ("^@end[ \t]group" . "\\\\end{same}")
    
    ;; We turn the @table into a description environment
    ;; There may be a lost of information here.
    ("^@table[ 	]+.*" . "\\\\begin{description}")
    ("^@end[ 	]table" . "\\\\end{description}")

    ;; We turn the @itemize into an itemize environment
    ;; There may be a lost of information here.
    ("^@itemize[ 	]+.*" . "\\\\begin{itemize}")

    ;; Take care of the titlepage before we remove the @end commands.
    ;; They will probably have to fix the \author and \title:
    ;; for now just put in question marks.
    ("^@titlepage" . "\\\\pagestyle{empty}
\\\\title{?}

\\\\\author{?}

\\\\date{\\\\today}

")
    ("^@end[ 	]titlepage" . "\\\\maketitle

\\\\clearpage
\\\\pagestyle{headings}
\\\\pagenumbering{roman}
\\\\tableofcontents

\\\\clearpage
\\\\pagenumbering{arabic}
")

    ;; commands with arguments on the rest of the line
    ("^@end[ 	]+\\(.+\\)" . "\\\\end{\\1}")
    ("^@printindex[ 	]+\\(.+\\)" . "\\\\printindex{\\1}")

    ;; Sections and chanpters are done LaTeX style
    ("^@chapter[ 	]+\\(.+\\)" . "\\\\chapter{\\1}")
    ("^@section[ 	]+\\(.+\\)" . "\\\\section{\\1}")
    ("^@subsection[ 	]+\\(.+\\)" . "\\\\subsection{\\1}")
    ("^@subsubsection[ 	]+\\(.+\\)" . "\\\\subsubsection{\\1}")
    ("^@unnumbered[ 	]+\\(.+\\)" . "\\\\chapter*{\\1}")
    ("^@unnumberedsec[ 	]+\\(.+\\)" . "\\\\section*{\\1}")
    ("^@unnumberedsubsec[ 	]+\\(.+\\)" . "\\\\subsection*{\\1}")
    ("^@unnumberedsubsubsec[ 	]+\\(.+\\)" . "\\\\subsubsection*{\\1}")
    ("^@appendix[ 	]+\\(.+\\)" . "\\\\chapter{\\1}")
    ("^@appendixsec[ 	]+\\(.+\\)" . "\\\\section{\\1}")
    ("^@appendixsubsec[ 	]+\\(.+\\)" . "\\\\subsection{\\1}")
    ("^@appendixsubsubsec[ 	]+\\(.+\\)" . "\\\\subsubsection{\\1}")

    
    ("^@item[ 	]+\\(.+\\)" . "\\\\item[\\1]")
    ;; This is a little different from texinfo
    ("\\]\n@itemx[ 	]+\\(.+\\)" . ", \\1]")
    ("^@sp[ 	]+\\(.+\\)" . "\\\\sp{\\1}")

    ("^@cindex[ 	]+\\(.+\\)" . "\\\\cindex{\\1}")
    ("^@vindex[ 	]+\\(.+\\)" . "\\\\vindex{\\1}")
    ("^@findex[ 	]+\\(.+\\)" . "\\\\findex{\\1}")
    ("^@tindex[ 	]+\\(.+\\)" . "\\\\tindex{\\1}")
    ("^@pindex[ 	]+\\(.+\\)" . "\\\\pindex{\\1}")
    ("^@kindex[ 	]+\\(.+\\)" . "\\\\kindex{\\1}")
    ("^@center[	 ]+\\(.+\\)" . "\\\\begin{center}\n\\1\n\\\\end{center}")
    ("^@setfilename[ 	]+\\(.+\\)" . "\\\\setfilename{\\1}")
    ("^@settitle[ 	]+\\(.+\\)" . "\\\\markboth{\\1}{\\1}")

    ;; commands with arguments already in braces
    ("@b{" . "\\\\b{")
    ("@t{" . "\\\\t{")
    ("@i{" . "\\\\i{")
    ("@r{" . "\\\\r{")
    ("@key{" . "\\\\key{")
    ("@w{" . "\\\\w{")

    ("@titlefont{" . "{\\\\Large ")
    ("@code{" . "\\\\code{")
    ("@samp{" . "\\\\samp{")
    ("@file{" . "\\\\file{")
    ("@kbd{" . "\\\\kbd{")

    ("@strong{" . "\\\\strong{")
    ("@emph{" . "\\\\emph{")

    ("@defn{" . "\\\\defn{")
    ("@dfn{" . "\\\\dfn{")
    ("@ctrl{" . "\\\\ctrl{")
    ("@var{" . "\\\\var{")
    ("@cite{" . "\\\\cite{")
    ("@pxref{" . "\\\\pxref{")
    ("@xref{" . "\\\\xref{")
    ("@inforef{" . "\\\\inforef{")
    ("@synindex{" . "\\\\synindex{")

    ;; commands without arguments
    ("@\\." . "\\\\.")
    ("^@item[ 	]*$" . "\\\\item")
    ("^@node" . "\\\\node")
    ("@noindent" . "\\\\noindent")
    ("@comment" . "\\\\comment")
    ("@bullet" . "\\\\bullet")
    ("@c[ 	]" . "\\\\c ")
    ("@arrow" . "\\\\arrow")
    
    ("@page" . "\\\\clearpage")
    ("^@bye" . "\\\\end{document}")
    ("@vskip[ 	]" . "\\\\vskip ")
    ("@TeX" . "\\\\TeX")
    ("@exdent" . "\\\\exdent")
    ("@copyright" . "\\\\copyright")
    ("@minus" . "\\\\minus")
    ("@dots" . "\\\\dots")
    ("@refill" . "\\\\refill")
    
    ("@:" . "\\\\:")
    ("@{" . "\\\\{")
    ("@}" . "\\\\}")
    ("@\\*" . "\\\\*")
    ))

(setq latex-delete-list
      '(
	;; This is in the chapter command.
	"@setchapternewpage[ 	]"
	;; these conflict with LaTeX commands.
	"^@group"
	"^@end[ 	]group"
	;; Haven't made a \summarycontents yet.
	"@summarycontents"
	;; replaced by \tableofcontents at the beginning.
	"@contents"
	))

