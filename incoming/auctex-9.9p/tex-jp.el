;;; tex-jp.el - Support for Japanese TeX.
;;
;; Support for Japanese TeX by KOBAYASHI Shinji <koba@flab.fujitsu.co.jp>
;; based on many patches developed by Japanese NetNews community.
;; Japanese message translation by MATUI Takao <mat@nuis.ac.jp>.

;;; Code:

(require 'latex)

;;; Customization

(setq TeX-format-list
      (append '(("JLATEX" japanese-latex-mode
		 "\\\\\\(documentstyle[^%\n]*{j\\|documentclass[^%\n]*{j\\)")
		("JTEX" japanese-plain-tex-mode
		 "-- string likely in Japanese TeX --"))
	      TeX-format-list))

(setq TeX-command-list
      (append (list (list "jTeX" "jtex '\\nonstopmode\\input %t'"
			  'TeX-run-TeX nil t)
		    (list "pTeX" "ptex '\\nonstopmode\\input %t'"
                          'TeX-run-TeX nil t)
		    (list "jBibTeX" "jbibtex %s" 'TeX-run-BibTeX nil nil))
	      TeX-command-list))
       
(setq LaTeX-command-style
      (append (if (string-equal LaTeX-version "2")
		  '(("^ams" "amsjlatex")
		    ("^jslides$" "jslitex")
		    ("^j-?\\(article\\|report\\|book\\)$" "jlatex"))
                '(("^j-\\(article\\|report\\|book\\)$" "jlatex")
                  ("^j\\(article\\|report\\|book\\)$" "platex")
                  ("." "jlatex")))
	      LaTeX-command-style))

(setcdr (assoc "%l" TeX-expand-list)
	(list 'TeX-style-check LaTeX-command-style))

(defvar japanese-TeX-error-messages t
  "If non-nil, explain TeX error messages in Japanese.")

(if (or (boundp 'MULE)
	(featurep 'mule))
    (if (string-match "XEmacs" emacs-version)
	(progn
	  (defvar TeX-japanese-process-input-coding-system 
	    (find-coding-system 'euc-japan)
	    "TeX-process' coding system with standard input.")
	  (defvar TeX-japanese-process-output-coding-system 
	    (find-coding-system 'junet)
	    "TeX-process' coding system with standard output."))
      (progn
	(defvar TeX-japanese-process-input-coding-system *euc-japan*
	  "TeX-process' coding system with standard input.")
	(defvar TeX-japanese-process-output-coding-system *junet*
	  "TeX-process' coding system with standard output."))))

(if (boundp 'NEMACS)
    (defvar TeX-process-kanji-code 2
      "TeX-process' kanji code with standard I/O.
0:No-conversion  1:Shift-JIS  2:JIS  3:EUC/AT&T/DEC"))

(defvar japanese-LaTeX-default-style "j-article"
  "*Default when creating new Japanese documents.")
(make-variable-buffer-local 'japanese-LaTeX-default-style)

(defvar japanese-LaTeX-style-list
  '(("book")
    ("article")
    ("letter")
    ("slides")
    ("report")
    ("jbook")
    ("j-book")
    ("jarticle")
    ("j-article")
    ("jslides")
    ("jreport")
    ("j-report"))
  "*List of Japanese document styles.")
(make-variable-buffer-local 'japanese-LaTeX-style-list)

;;; Coding system

(if (boundp 'MULE)
    (setq TeX-after-start-process-function
	  (function (lambda (process)
		      (set-process-coding-system
		       process
		       TeX-japanese-process-input-coding-system
		       TeX-japanese-process-output-coding-system)))))
(if (boundp 'NEMACS)
    (setq TeX-after-start-process-function
	  (function
	   (lambda (process)
	     (set-process-kanji-code process TeX-process-kanji-code)))))

(if (and (string-match "XEmacs" emacs-version)
	 (featurep 'mule))
    (setq TeX-after-start-process-function
	  (function (lambda (process)
		      (set-process-input-coding-system
		       process
		       TeX-japanese-process-input-coding-system)
		      (set-process-output-coding-system
		       process
		       TeX-japanese-process-output-coding-system)))))

;;; Japanese Parsing

(if (or (boundp 'MULE)
	(featurep 'mule))
(progn

(defconst LaTeX-auto-regexp-list 
  (append
   '(("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]\
\\[\\([^\]\\\\\n\r]+\\)\\]"
      (1 3 4) LaTeX-auto-optional)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-arguments)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 TeX-auto-symbol)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-env-args)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\(\\([a-zA-Z]\\|\\cj\\)+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\usepackage\\(\\[[^\]\\\\]*\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
      (2) LaTeX-auto-style)
     ("\\\\bibitem{\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defconst plain-TeX-auto-regexp-list
  '(("\\\\def\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\let\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\font\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\chardef\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\new\\(count|dimen|muskip|skip\\)\\\\\\(\\([a-z]\\|\\cj\\)+\\)[^a-zA-Z@]"
     2 TeX-auto-symbol)
    ("\\\\newfont{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 TeX-auto-symbol)
    ("\\\\typein\\[\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)\\]" 1 TeX-auto-symbol)
    ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)
    ("\\\\mathchardef\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol))
  "List of regular expression matching common LaTeX macro definitions.")

(defconst BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

))

(if (boundp 'NEMACS)
(progn

(defconst LaTeX-auto-regexp-list 
  (append
   '(("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]\
\\[\\([^\]\\\\\n\r]+\\)\\]"
      (1 3 4) LaTeX-auto-optional)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-arguments)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 TeX-auto-symbol)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-env-args)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\(\\([a-zA-Z]\\|\\z\\)+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\usepackage\\(\\[[^\]\\\\]*\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
      (2) LaTeX-auto-style)
     ("\\\\bibitem{\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defconst plain-TeX-auto-regexp-list
  '(("\\\\def\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\let\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\font\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\chardef\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\new\\(count|dimen|muskip|skip\\)\\\\\\(\\([a-z]\\|\\z\\)+\\)[^a-zA-Z@]"
     2 TeX-auto-symbol)
    ("\\\\newfont{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 TeX-auto-symbol)
    ("\\\\typein\\[\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)\\]" 1 TeX-auto-symbol)
    ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)
    ("\\\\mathchardef\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol))
  "List of regular expression matching common LaTeX macro definitions.")

(defconst BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

))

(defconst TeX-auto-full-regexp-list 
  (append LaTeX-auto-regexp-list plain-TeX-auto-regexp-list)
  "Full list of regular expression matching TeX macro definitions.")

;;; Japanese TeX modes

(defvar japanese-TeX-mode nil
  "Flag to determine if Japanese initialization is needed.")

(add-hook 'plain-TeX-mode-hook 'japanese-plain-tex-mode-initialization)

;;;###autoload
(defun japanese-plain-tex-mode ()
  "Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters plain-tex-mode."
  (interactive)
  (setq japanese-TeX-mode t)
  (plain-tex-mode))

(defun japanese-plain-tex-mode-initialization ()
  "Japanese plain-TeX specific initializations."
  (if japanese-TeX-mode
      (setq TeX-command-default "jTeX")))

(add-hook 'LaTeX-mode-hook 'japanese-latex-mode-initialization)

;;;###autoload
(defun japanese-latex-mode ()
  "Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters latex-mode."
  (interactive)
  (setq japanese-TeX-mode t)
  (latex-mode))

(defun japanese-latex-mode-initialization ()
  "Japanese LaTeX specific initializations."
  (if japanese-TeX-mode
      (progn
	(setq LaTeX-default-style japanese-LaTeX-default-style)
	(setq LaTeX-style-list japanese-LaTeX-style-list)
	(setq TeX-command-BibTeX "jBibTeX")
	(setq japanese-TeX-mode nil))))

;;; MULE and NEMACS paragraph filling.

(if (boundp 'MULE)
(if (string-lessp emacs-version "19")
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward "\n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))

    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; Ignore the handling routine related with `fill-prefix'.

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;;; patch by S.Tomura 88-Jun-30
    ;;$B!cE}9g!d(B
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])\"']*$" nil t)
      (if (eobp)
	  nil
	;; replace CR by two spaces.
	(delete-char 1)			; delete newline
	(insert "  ")))
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;; modified by K.Handa 92-Mar-2
    ;; Spacing is not necessary for charcters of no word-separater.
    ;; The regexp word-across-newline is used for this check.
    (if (not (stringp word-across-newline))
	(subst-char-in-region from (point-max) ?\n ?\ )
      (goto-char from)
      (end-of-line)
      (while (not (eobp))
	(delete-char 1)
	(if (eobp) nil			; 92.6.30 by K.Handa
	  (if (not (looking-at word-across-newline))
	      (progn
		(forward-char -1)
		(if (and (not (eq (following-char) ? ))
			 (not (looking-at word-across-newline)))
		    (progn
		      (forward-char 1)
		      (insert ? ))
		  (forward-char 1))))
	  (end-of-line))))
    ;; After the following processing, there's two spaces at end of sentence
    ;; and single space at end of line within sentence.
    ;; end of patch
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
	       (skip-chars-backward " ])\"'")
	       (memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0)
	  ;; patch by K.Handa 92-Mar-2
	  (re-break-point (concat "[ \t\n]\\|" word-across-newline))
	  ;; end of patch
	  )
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  ;; patched by S.Tomura 87-Jun-2
	  ;; Big change by K.Handa 92-Mar-2
	  ;; Move back to start of word.
	  ;; (skip-chars-backward "^ \n")
	  ;; (if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;    ;; Move back over whitespace before the word.
	  ;;    (skip-chars-forward "^ \n")
	  ;;  ;; Normally, move back over the single space between the words.
	  ;;  (forward-char -1))

	  ;; At first, find breaking point at the left of fill-column,
	  ;; but after kinsoku-shori, the point may be right of fill-column.
	  ;; 92.4.15 by K.Handa -- re-search-backward will back to prev line.
	  ;; 92.4.27 by T.Enami -- We might have gone back too much...
	  (let ((p (point)) ch)
	    (re-search-backward re-break-point nil 'mv)
	    (setq ch (following-char))
	    (if (or (= ch ? ) (= ch ?\t))
		(skip-chars-backward " \t")
	      (forward-char 1)
	      (if (<= p (point))
		  (forward-char -1))))
	  (kinsoku-shori)
	  ;; Check if current column is at the right of prefixcol.
	  ;; If not, find break-point at the right of fill-column.
	  ;; This time, force kinsoku-shori-nobashi.
	  (if (>= prefixcol (current-column))
	      (progn
		(move-to-column (1+ fill-column))
		;; 92.4.15 by K.Handa -- suppress error in re-search-forward
		(re-search-forward re-break-point nil t)
		(forward-char -1)
		(kinsoku-shori-nobashi))))
	;; end of patch S.Tomura

	;; Replace all whitespace here with one newline.
	;; Insert before deleting, so we don't forget which side of
	;; the whitespace point or markers used to be on.
	;; patch by S. Tomura 88-Jun-20
	;; 92.4.27 by K.Handa
	(skip-chars-backward " \t")
	(if mc-flag
	    ;; $B!cJ,3d!d(B  WAN means chars which match word-across-newline.
	    ;; (0)     | SPC + SPC* <EOB>	--> NL
	    ;; (1) WAN | SPC + SPC*		--> WAN + SPC + NL
	    ;; (2)     | SPC + SPC* + WAN	--> SPC + NL  + WAN
	    ;; (3) '.' | SPC + nonSPC		--> '.' + SPC + NL + nonSPC
	    ;; (4) '.' | SPC + SPC		--> '.' + NL
	    ;; (5)     | SPC*			--> NL
	    (let ((start (point))	; 92.6.30 by K.Handa
		  (ch (following-char)))
	      (if (and (= ch ? )
		       (progn		; not case (0) -- 92.6.30 by K.Handa
			 (skip-chars-forward " \t")
			 (not (eobp)))
		       (or
			(progn		; case (1)
			  (goto-char start)
			  (forward-char -1)
			  (looking-at word-across-newline))
			(progn		; case (2)
			  (goto-char start)
			  (skip-chars-forward " \t")
			  (and (not (eobp))
			       (looking-at word-across-newline)))
			(progn		; case (3)
			  (goto-char (1+ start))
			  (and (not (eobp))
			       (/= (following-char) ? )
			       (progn
				 (skip-chars-backward " ])\"'")
				 (memq (preceding-char) '(?. ?? ?!)))))))
		  ;; We should keep one SPACE before NEWLINE. (1),(2),(3)
		  (goto-char (1+ start))
		;; We should delete all SPACES around break point. (4),(5)
		(goto-char start))))
	;; end of patch
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(delete-horizontal-space)

	;; Ignore the handling routine related with `fill-prefix'.

	(LaTeX-indent-line)
	(setq prefixcol (current-column))
	;; Justify the line just ended, if desired.
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space))))
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.\n\
Prefix arg means justify too.\n\
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward " \n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))
    
    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.
    
    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;; patch by S.Tomura 88-Jun-30
    ;;$B!cE}9g!d(B
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;; (while (re-search-forward "[.?!][])\"']*$" nil t)
    ;;   (insert ? ))
    (while (re-search-forward "[.?!][])}\"']*$" nil t)
      (if (eobp)
	  nil
	;; replace CR by two spaces.
	;; insert before delete to preserve marker.
	(insert "  ")
	;; delete newline
	(delete-char 1)))
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; (subst-char-in-region from (point-max) ?\n ?\ )
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;; modified by K.Handa 92-Mar-2
    ;; Spacing is not necessary for charcters of no word-separater.
    ;; The regexp word-across-newline is used for this check.
    (if (not (stringp word-across-newline))
	(subst-char-in-region from (point-max) ?\n ?\ )
      ;;
      ;; WAN     +NL+WAN       --> WAN            + WAN
      ;; not(WAN)+NL+WAN       --> not(WAN)       + WAN
      ;; WAN     +NL+not(WAN)  --> WAN            + not(WAN)
      ;; SPC     +NL+not(WAN)  --> SPC            + not(WAN)
      ;; not(WAN)+NL+not(WAN)  --> not(WAN) + SPC + not(WAN)
      ;;
      (goto-char from)
      (end-of-line)
      (while (not (eobp))
	;; 92.8.26 , 92.8.30 by S. Tomura
	
	;; Insert SPC only when point is between nonWAN.  Insert
	;; before deleting to preserve marker if possible.
	(if (or (prog2		; check following char.
		    (forward-char)	; skip newline
		    (or (eobp)
			(looking-at word-across-newline))
		  (forward-char -1))
		(prog2		; check previous char.
		    (forward-char -1)
		    (or (eq (following-char) ?\ )
			(looking-at word-across-newline))
		  (forward-char)))
	    nil
	  (insert ?\ ))
	(delete-char 1)		; delete newline
	(end-of-line)))
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
		(skip-chars-backward " ])\"'")
		(memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0) linebeg
	  ;; patch by K.Handa 92-Mar-2
	  (re-break-point (concat "[ \n]\\|" word-across-newline))
	  ;; end of patch
	  )
      (while (not (eobp))
	(setq linebeg (point))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  ;;(skip-chars-backward "^ \n")
	  (fill-move-backward-to-break-point re-break-point)
	  (if sentence-end-double-space
	      (while (and (> (point) (+ linebeg 2))
			  (eq (preceding-char) ?\ )
			  (not (eq (following-char) ?\ ))
			  (eq (char-after (- (point) 2)) ?\.))
		(forward-char -2)
		(fill-move-backward-to-break-point re-break-point linebeg)))
	  (kinsoku-shori)
	  (if (if (zerop prefixcol)
		  (save-excursion
		    (skip-chars-backward " " linebeg)
		    (bolp))
		(>= prefixcol (current-column)))
	      ;; Keep at least one word even if fill prefix exceeds margin.
	      ;; This handles all but the first line of the paragraph.
	      ;; Meanwhile, don't stop at a period followed by one space.
	      (let ((first t))
		(move-to-column prefixcol)
		(while (and (not (eobp))
			    (or first
				(and (not (bobp))
				     sentence-end-double-space
				     (save-excursion (forward-char -1)
						     (and (looking-at "\\. ")
							  (not (looking-at "\\.  ")))))))
		  (skip-chars-forward " ")
		  ;; (skip-chars-forward "^ \n")
		  (fill-move-forward-to-break-point re-break-point)
		  (setq first nil)))
	    ;; Normally, move back over the single space between the words.
	    (if (eq (preceding-char) ? )
		(forward-char -1))))
	(if mc-flag
	    ;; $B!cJ,3d!d(B  WAN means chars which match word-across-newline.
	    ;; (0)     | SPC + SPC* <EOB>	--> NL
	    ;; (1) WAN | SPC + SPC*		--> WAN + SPC + NL
	    ;; (2)     | SPC + SPC* + WAN	--> SPC + NL  + WAN
	    ;; (3) '.' | SPC + nonSPC		--> '.' + SPC + NL + nonSPC
	    ;; (4) '.' | SPC + SPC		--> '.' + NL
	    ;; (5)     | SPC*			--> NL
	    (let ((start (point))	; 92.6.30 by K.Handa
		  (ch (following-char)))
	      (if (and (= ch ? )
		       (progn		; not case (0) -- 92.6.30 by K.Handa
			 (skip-chars-forward " \t")
			 (not (eobp)))
		       (or
			(progn	; case (1)
			  (goto-char start)
			  (forward-char -1)
			  (looking-at word-across-newline))
			(progn	; case (2)
			  (goto-char start)
			  (skip-chars-forward " \t")
			  (and (not (eobp))
			       (looking-at word-across-newline)
			       ;; never leave space after the end of sentence
			       (not (fill-end-of-sentence-p))))
			(progn	; case (3)
			  (goto-char (1+ start))
			  (and (not (eobp))
			       (/= (following-char) ? )
			       (fill-end-of-sentence-p)))))
		  ;; We should keep one SPACE before NEWLINE. (1),(2),(3)
		  (goto-char (1+ start))
		;; We should delete all SPACES around break point. (4),(5)
		(goto-char start))))
	;; end of patch
	(delete-horizontal-space)
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(LaTeX-indent-line)
	(setq prefixcol (current-column))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space))))))

(if (boundp 'NEMACS)
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward " \n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))

    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;;; patch by S.Tomura 88-Jun-30
    ;;$B!cE}9g!d(B
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (if (eobp)
	  nil
      (delete-char 1)
      (insert "  "))) ;; replace CR by two spaces.
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;;$BF|K\8l$N8l$N8e$K$O6uGr$O$J$$!#(B
    (goto-char from)
    (end-of-line)
    (while (not (eobp))
      (delete-char 1)
      (if (and (< ?  (preceding-char)) ;; + SPC + CR + X ==> + SPC + X
	       (< (preceding-char) 128)
	       (<= ?  (following-char))
	       (< (following-char) 128))
	   (insert ?\  ))
      (end-of-line))
    ;; $B<!$N=hM}$GJ8Kv$K$O(Btwo spaces$B$,$"$j!"$=$l0J30$O(Bsingle space$B$K$J$C$F$$$k!#(B
    ;; end of patch
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
	       (skip-chars-backward " ])\"'")
	       (memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward " " nil t)
      (if (<= 128 (following-char))
	  (let ((dummy 0))
	    (backward-char 1) 
	    (if (<= 128 (preceding-char))
		(delete-char 1))
	    (forward-char 1))))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	;; patched by S.Tomura 88-Jun-2
	;;(move-to-column (1+ fill-column))
	(move-to-column fill-column)
	;; end of patch
	;; patched by S.Tomura 88-Jun-16, 89-Oct-2, 89-Oct-19
	;; $B4A;z%3!<%I$N>l9g$K$O(Bfill-column$B$h$jBg$-$/$J$k$3$H$,$"$k!#(B
	(or (>= fill-column (current-column)) (backward-char 1))
	;; end of patch
	(if (eobp)
	    nil
	  ;; patched by S.Tomura 87-Jun-2
	  ;;(skip-chars-backward "^ \n")
	  ;;(if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;    (skip-chars-forward "^ \n")
	  ;;  (forward-char -1)))
	  ;; $B86B'$H$7$F(Bfill-column$B$h$j:8B&$KJ,3dE@$rC5$9!#(B
	  ;; Find a point to break lines
	     (skip-chars-backward " \t") ;; skip SPC and TAB
	     (if (or (<= 128 (preceding-char))
		     (<= 128 (following-char)) ;; 88-Aug-25
		     (= (following-char) ? )
		     (= (following-char) ?\t))
		 (kinsoku-shori)
	       (if(re-search-backward "[ \t\n]\\|\\z" ;; 89-Nov-17
				      (point-min) (point-min))
		   (forward-char 1))
	       (skip-chars-backward " \t")
	       (kinsoku-shori))
	     ;; prifixcol$B$h$j1&B&$KJ,3dE@$rC5$9!#(B
	     ;; $B$3$N>l9g$OJ,3dE@$O(Bfill-column$B$h$j1&B&$K$J$k!#(B
	     (if (>= prefixcol (current-column))
		 (progn
		   (move-to-column prefixcol)
		   (if (re-search-forward "[ \t]\\|\\z" ;; 89-Nov-17
					  (point-max) (point-max))
		       (backward-char 1))
		   (skip-chars-backward " \t")
		   (kinsoku-shori)
		   ;; $B$=$l$bBLL\$J$iJ,3d$rD|$a$k!#(B
		   (if (>= prefixcol (current-column)) (goto-char (point-max))))))
	;; end of patch S.Tomura
	;; patch by S. Tomura 88-Jun-20
	;;(delete-horizontal-space)
        ;;$B!cJ,3d!d(B
        ;; $BA43Q(B | SPC + SPC$B!v(B   --> $BA43Q(B + SPC + CR
	;; | SPC + SPC* + $BA43Q(B  --> SPC  + CR + $BA43Q(B
        ;; . | SPC + SPC +      --> . + CR
        ;; . | SPC + nonSPC     --> . + SPC + CR + nonSPC
        ;;
        ;; . | $BH>3Q(B             --> $BJ,3d$7$J$$(B
        ;; . | $BA43Q(B             --> $BJ,3d$7$J$$(B
	(if (not kanji-flag) (delete-horizontal-space)
	  (let ((start) (end))
	    (skip-chars-backward " \t")
	    (setq start (point))
	    (skip-chars-forward  " \t")
	    (setq end (point))
	    (delete-region start end)
	    (if (and (not
		      (and (save-excursion
			     (skip-chars-backward " ])\"'")
			     (memq (preceding-char) '(?. ?? ?!)))
			   (= end (+ start 2))))
		     (or (and (or (<= 128 (preceding-char))
				  (<= 128 (following-char)))
			      (< start end)
			      (not (eobp)))
			 (and (memq (preceding-char) '(?. ?? ?!))
			      (= (1+ start) end)
			      (not (eobp)))))
		(insert ?  ))))
	;; end of patch
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(LaTeX-indent-line)
	(setq prefixcol (current-column))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space)))))

;;; Support for various self-insert-command

(cond ((fboundp 'can-n-egg-self-insert-command)
       (fset 'tex-jp-self-insert-command 'can-n-egg-self-insert-command))
      ((fboundp 'egg-self-insert-command)
       (fset 'tex-jp-self-insert-command 'egg-self-insert-command))
      ((fboundp 'canna-self-insert-command)
       (fset 'tex-jp-self-insert-command 'canna-self-insert-command))
      (t
       (fset 'tex-jp-self-insert-command 'self-insert-command)))

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively 'tex-jp-self-insert-command))

;;; Error Messages

(if japanese-TeX-error-messages
    (setq TeX-error-description-list
  '(("Bad \\\\line or \\\\vector argument.*" .
"$B@~$N79$-$r;XDj$9$k!$(B\\line$B$^$?$O(B\\vector$B$N:G=i$N0z?t$,IT@5$G$9!%(B")

    ("Bad math environment delimiter.*" .
"$B?t<0%b!<%ICf$G?t<0%b!<%I3+;O%3%^%s%I(B\\[$B$^$?$O(B\\($B!$$^$?$O!$?t<0%b!<%I30$G(B
$B?t<0%b!<%I=*N;%3%^%s%I(B\\[$B$^$?$O(B\\($B$r(BTeX$B$,8+$D$1$^$7$?!%$3$NLdBj$O!$?t<0%b!<(B
$B%I$N%G%j%_%?$,%^%C%A$7$F$$$J$+$C$?$j!$3g8L$N%P%i%s%9$,$H$l$F$$$J$+$C$?$j$9(B
$B$k$?$a$K@8$8$^$9!%(B")

    ("Bad use of \\\\\\\\.*" .
"\\\\$B%3%^%s%I$,%Q%i%0%i%UCf$K$"$j$^$7$?!%$3$N;H$$$+$?$OL50UL#$G$9!%(B
$B$3$N%(%i!<%a%C%;!<%8$O(B\\\\$B$,(Bcentering$B4D6-$d(Bflushing$B4D6-$G;H$o$l$?(B
$B;~!$$"$k$$$O(Bcentering/flushing$B@k8@$,M-8z$J$H$3$m$G;H$o$l$?;~$K@8$8$^$9!%(B")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
"$BBP1~$9$k(B\\begin$BL?Na$N$J$$(B\\end$BL?Na$r(BLaTeX$B$,8+$D$1$^$7$?!%(B\\end$BL?Na$N4D(B
$B6-L>$r4V0c$($?$+!$M>J,$J(B\\begin$BL?Na$,$"$k$+!$(B\\end$BL?Na$r$o$9$l$?$+$N$$$:(B
$B$l$+$G$7$g$&!%(B")

    ("Can be used only in preamble." .
"$B%W%j%"%s%V%k$G$7$+;H$($J$$(B\\documentstyle$B!&(B\\nofiles$B!&(B\\includeonly
\\makeindex$B!&(B\\makeglossary$B$N$&$A$N$$$:$l$+$,(B\\begin{document}$B$h$j$b(B
$B8e$G;H$o$l$F$$$k$N$r(BLaTeX$B$,8!=P$7$^$7$?!%$3$N%(%i!<$O(B\\begin{document}
$B$,M>J,$K$"$C$?;~$K$b@8$8$^$9!%(B")

    ("Command name [^ ]* already used.*" .
"$B$9$G$KDj5A$5$l$F$$$kL?NaL>$^$?$O4D6-L>$KBP$7$F(B\\newcommand$B!&(B
\\newenvironment$B!&(B\\newlength$B!&(B\\newsavebox$B!&(B\\newtheorem$B$N$&$A$N$$$:(B
$B$l$+$r<B9T$7$h$&$H$7$F$$$^$9(B($B$"$k4D6-$rDj5A$9$k$HF1$8L>A0$NL?Na$,<+F0(B
$BE*$KDj5A$5$l$k$N$G!$4{$KB8:_$9$k4D6-$HF1L>$NL?Na$ODj5A$G$-$^$;$s(B)$B!%?7(B
$B$7$$L>A0$r9M$($k$+!$(B\\newcommand$B$+(B\\newenvironment$B$N>l9g$J$iBP1~$9$k(B
\\renew...$BL?Na$r;H$o$J$1$l$P$J$j$^$;$s!%(B")

    ("Counter too large." .
"$BJ8;z$G=g=xIU$1$5$l$?$b$N!$$?$V$sHV9fIU$1$5$l$?%j%9%H4D6-$N%i%Y%k$,!$(B
26$B$h$j$bBg$-$$HV9f$r<u$1<h$j$^$7$?!%Hs>o$KD9$$%j%9%H$r;H$C$F$$$k$+!$(B
$B%+%&%s%?$r:F@_Dj$7$F$7$^$C$?$+$N$$$:$l$+$G$7$g$&!%(B")

    ("Environment [^ ]* undefined." .
"$BDj5A$5$l$F$$$J$$4D6-$KBP$9$k(B\\begin$BL?Na$r(BLaTeX$B$,8+$D$1$^$7$?!%$*$=$i$/(B
$B4D6-L>$r4V0c$($?$N$G$7$g$&!%(B")

    ("Float(s) lost." .
"parbox$B$N$J$+$K(Bfigure$B4D6-!&(Btable$B4D6-$^$?$O(B\\marginpar$BL?Na$,$"$j$^$7$?(B
\($B$J$*!$(Bparbox$B$O(Bminipage$B4D6-$+(B\\parbox$BL?Na$K$h$C$F:n$i$l$k$+!$5SCm$d?^(B
$B$J$I$KBP$7$F(BLaTeX$B$,@8@.$9$k$b$N$G$9(B\)$B!%$3$l$O=PNO;~$N%(%i!<$J$N$G!$860x(B
$B$H$J$C$F$$$k4D6-$"$k$$$OL?Na$O!$(BLaTeX$B$,LdBj$rH/8+$7$?>l=j$h$j$b$@$$$V(B
$B$sA0$K$"$k2DG=@-$,$"$j$^$9!%=PNO$5$l$F$$$J$$?^!&I=!&K5Cm$J$I$,$$$/$D$+(B
$B$"$k$+$b$7$l$^$;$s$,!$$=$l$i$,860x$G$"$k$H$O8B$j$^$;$s!%(B")

    ("Illegal character in array arg." .
"array$B4D6-$^$?$O(Btabular$B4D6-$N0z?t!$$^$?$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t(B
$B$NCf$KIT@5$JJ8;z$,$"$j$^$7$?!%(B")

    ("Missing \\\\begin{document}." .
"\\begin{document}$BL?Na$h$jA0$K(BLaTeX$B$,=PNO$r9T$J$C$F$7$^$$$^$7$?!%(B
\\begin{document}$BL?Na$rK:$l$?$+!$%W%j%"%s%V%k$K2?$+4V0c$$$,$"$k$N$G$7$g$&!%(B
$BBG$A4V0c$$$K$h$kJ8;z$d!$@k8@$N8m$j$K$h$k2DG=@-$b$"$j$^$9!%Nc$($P!$0z?t$r(B
$B0O$`3g8L$rH4$+$7$?$H$+!$L?NaL>$N(B\\$B$rK:$l$?>l9g$J$I$G$9!%(B")

    ("Missing p-arg in array arg.*" .
"array$B4D6-!&(Btabular$B4D6-$N0z?t!$$"$k$$$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t$NCf$K!$(B
$B3g8L$K0O$^$l$?I=8=$N$D$$$F$$$J$$(Bp$B$,$"$j$^$7$?!%(B")

    ("Missing @-exp in array arg." .
"array$B4D6-!&(Btabular$B4D6-$N0z?t!$$"$k$$$O(B\\multicolumn$BL?Na$NBh(B2$B0z?t$NCf$K!$(B
@$BI=8=$N$D$$$F$$$J$$(B@$B$,$"$j$^$7$?!%(B")

    ("No such counter." .
"\\setcounter$BL?Na$^$?$O(B\\addtocounter$BL?Na$G!$B8:_$7$J$$%+%&%s%?$,;XDj$5$l(B
$B$^$7$?!%$*$=$i$/$?$@$N%?%$%W%_%9$G$7$g$&!%$?$@$7!$%(%i!<$,(Baux$B%U%!%$%k$NCf(B
$B$G@8$8$?>l9g$O!$(B\\newcounter$BL?Na$r%W%j%"%s%V%k$N30$G;H$C$?$N$@$H;W$o$l$^$9!%(B")

    ("Not in outer par mode." .
"figure$B4D6-!&(Btable$B4D6-$"$k$$$O(B\\marginpar$BL?Na$,?t<0%b!<%I$^$?$O(Bparbox$B$NCf(B
$B$G;H$o$l$^$7$?!%(B")

    ("\\\\pushtabs and \\\\poptabs don't match." .
"\\pushtabs$B$HBP1~$7$J$$(B\\poptabs$B$,$_$D$+$C$?$+!$$^$?$O!$BP1~$9$k(B\\poptabs
$B$r$b$?$J$$(B\\pushtabs$B$,$"$k$N$K(B\\end{tabbing}$B$,8=$l$F$7$^$$$^$7$?!%(B")

    ("Something's wrong--perhaps a missing \\\\item." .
"$B%j%9%H4D6-$NCf$K(B\\item$BL?Na$,$J$$$N$,:G$b$"$j$=$&$J%1!<%9$G$9!%(B
thebibliography$B4D6-$G0z?t$rK:$l$?>l9g$K$b@8$8$^$9!%(B")

    ("Tab overflow." .
"\\=$B$,!$(BLaTeX$B$G5v$5$l$k%?%V%9%H%C%W$N:GBg?t$rD6$($F$$$^$9!%(B")

    ("There's no line here to end." .
"\\newline$BL?Na$^$?$O(B\\\\$BL?Na$,%Q%i%0%i%U4V$K$"$j$^$9!%$3$N;H$$$+$?$O(B
$BL50UL#$G$9!%$b$76u9T$r$"$1$?$$$N$G$7$?$i!$(B\\vspace$B$r;H$C$F$/$@$5$$!%(B")

    ("This may be a LaTeX bug." .
"$B$^$C$?$/$o$1$,$o$+$i$J$/$J$C$F$7$^$$$^$7$?!%$?$V$s$3$l0JA0$K8!=P$5$l$?(B
$B%(%i!<$N$;$$$@$H;W$o$l$^$9!%$7$+$7!$(BLaTeX$B<+BN$N%P%0$G$"$k2DG=@-$b$"$j$^$9!%(B
$B$b$7$3$N%(%i!<$,F~NO%U%!%$%k$KBP$9$k:G=i$N%(%i!<$G$"$j!$2?$b4V0c$$$,8+$D(B
$B$+$i$J$$>l9g$O!$$=$N%U%!%$%k$rJ]B8$7$F!$%m!<%+%k%,%$%I$K=q$+$l$F$$$k@UG$(B
$B<T$KO"Mm$7$F$/$@$5$$!%(B")

    ("Too deeply nested." .
"$B%j%9%H4D6-$NF~$l;R$,?<$9$.$^$9!%2?CJ3,$NF~$l;R$,5v$5$l$k$+$O;H$C$F$$$k(B
$B%3%s%T%e!<%?$K0MB8$7$^$9$,!$>/$J$/$H$b(B4$BCJ3,$^$G$O5v$5$l$F$$$^$9(B($BIaDL$O(B
$B$=$l$G==J,$G$7$g$&(B)$B!%(B")

    ("Too many unprocessed floats." .
"$B$3$N%(%i!<$O(B1$B%Z!<%8Cf$N(B\\marginpar$BL?Na$,B?$9$.$k$?$a$K@8$8$k>l9g$b$"(B
$B$j$^$9$,!$$b$C$H$"$j$=$&$J$N$O!$8B3&$rD6$($F?^$dI=$rJ]B8$7$h$&$H$7$?>l(B
$B9g$G$9!%D9$$J8=q$rAHHG$7$F$$$/$H$-!$(BLaTeX$B$O?^$dI=$r8D!9$KJ]B8$7!$%Z!<(B
$B%8$NJ,3d$r9T$J$&;~$K$=$l$i$rA^F~$7$^$9!%$3$N%(%i!<$O!$%Z!<%8$X$NJ,3d$,(B
$B9T$J$o$l$kA0$K!$$"$^$j$K$bB?$/$N(Bfigure$B4D6-$d(Btable$B4D6-$,8+$D$+$C$?>l9g(B
$B$K@8$8$^$9!%$3$NLdBj$O4D6-$N$&$A$N$$$/$D$+$rJ8=q$N=*$o$j$NJ}$K0\F0$9$l(B
$B$P2r7h$G$-$^$9!%$^$?!$$3$N%(%i!<$O(B``logjam''$B$K$h$C$F@8$8$k$3$H$b$"$j$^(B
$B$9!%(B``logjam''$B$H$O!$(BLaTeX$B$,=P8==g=xDL$j$K$7$+?^I=$r=PNO$G$-$J$$$;$$$G!$(B
$B$D$^$C$F$$$k8e$m$N?^I=$N$?$a$KA0$N?^I=$r=PNO$G$-$J$/$J$k$3$H$r$$$$$^$9!%(B
$B$3$N%8%c%`$N860x$O!$Bg$-$9$.$F(B1$B%Z!<%8$J$$$7$O;XDj$5$l$?NN0h$K<}$^$i$J(B
$B$$$h$&$J?^$dI=$G$"$k2DG=@-$,$"$j$^$9!%$3$l$O!$0z?t$K(Bp$B%*%W%7%g%s$,;XDj(B
$B$5$l$F$$$J$$$H5/$-$d$9$/$J$j$^$9!%(B")

    ("Undefined tab position." .
"\\>$B!&(B\\+$B!&(B\\-$B$^$?$O(B\\<$BL?Na$G!$B8:_$7$J$$%?%V0LCV!$$9$J$o$A(B\\=$BL?Na$GDj(B
$B5A$5$l$F$$$J$$%?%V0LCV$r;XDj$7$h$&$H$7$F$$$^$9!%(B")

    ("\\\\< in mid line." .
"\\<$BL?Na$,(Btabbing$B4D6-$N9T$NESCf$K8=$l$^$7$?!%$3$NL?Na$O9T$N@hF,$K$J$1$l$P(B
$B$J$j$^$;$s!%(B")

    ("Counter too large." .
"$B5SCm$,J8;z$^$?$O5SCm5-9f$G=g=x$E$1$5$l$F$$$^$9$,!$J8;z$^$?$O5-9f$r;H$$(B
$B@Z$C$F$7$^$$$^$7$?!%$*$=$i$/(B\\thanks$BL?Na$N;H$$$9$.$G$9!%(B")

    ("Double subscript." .
"$B?t<0Cf$N(B1$B$D$NNs$K(B2$B$D$N2<IU$-J8;z$,$D$$$F$$$^$9!%Nc$($P(Bx_{2}_{3}$B$N$h$&$K!%(B
$B$3$N$h$&$JI=8=$OL50UL#$G$9!%(B")

    ("Double superscript." .
"$B?t<0Cf$N(B1$B$D$NNs$K(B2$B$D$N>eIU$-J8;z$,$D$$$F$$$^$9!%Nc$($P(Bx^{2}^{3}$B$N$h$&$K!%(B
$B$3$N$h$&$JI=8=$OL50UL#$G$9!%(B")

    ("Extra alignment tab has been changed to \\\\cr." .
"array$B4D6-$^$?$O(Btabular$B4D6-$N(B1$BNsCf$K$"$k9`L\$,B?$9$.$^$9!%8@$$49$($k$H!$(B
$BNs$N=*$o$j$^$G$K$"$k(B&$B$N?t$,B?$9$.$^$9!%$*$=$i$/A0$NNs$N:G8e$K(B\\\\$B$r$D$1(B
$B$k$N$rK:$l$?$N$G$7$g$&!%(B")

    ("Extra \\}, or forgotten \\$." .
"$B3g8L$^$?$O?t<0%b!<%I$N%G%j%_%?$,@5$7$/BP1~$7$F$$$^$;$s!%$*$=$i$/(B{$B!&(B\\[$B!&(B
\\($B$"$k$$$O(B$$B$N$&$A$N$$$:$l$+$r=q$-K:$l$?$N$G$7$g$&!%(B")

    ("Font [^ ]* not loaded: Not enough room left." .
"$B$3$NJ8=q$O8B3&$h$j$bB?$/$N%U%)%s%H$r;H$C$F$$$^$9!%$b$7J8=q$NItJ,$4$H$K(B
$BJL!9$N%U%)%s%H$,;H$o$l$F$$$k$N$J$i!$J,3d$7$F=hM}$9$l$PLdBj$O2r7h$5$l$^$9!%(B")

    ("I can't find file `.*'." .
"$BI,MW$J%U%!%$%k$,8+$D$+$j$^$;$s$G$7$?!%$b$78+$D$+$i$J$$%U%!%$%k$N3HD%;R(B
$B$,(Btex$B$N>l9g!$$"$J$?$,;XDj$7$?%U%!%$%k!$$9$J$o$A%a%$%s%U%!%$%k$^$?$O(B
\\input$BL?Na!&(B\\include$BL?Na$GA^F~$5$l$k%U%!%$%k$,8+$D$+$i$J$$$N$G$9!%(B
$B3HD%;R$,(Bsty$B$G$"$l$P!$B8:_$7$J$$J8=q%9%?%$%k$^$?$O%9%?%$%k%*%W%7%g%s$r(B
$B;XDj$7$h$&$H$7$F$$$^$9!%(B")

    ("Illegal parameter number in definition of .*" .
"$B$3$l$O$*$=$i$/!$(B\\newcommand$B!&(B\\renewcommand$B!&(B\\newenvironment$B$^$?$O(B
\\renewenvironment$BL?Na$N$J$+$G(B#$B$,@5$7$/;H$o$l$J$+$C$?$?$a$K@8$8$?%(%i!<(B
$B$G$9!%(B\\#$BL?Na$H$7$F;H$o$l$k>l9g$r=|$1$P!$(B#$B$H$$$&J8;z$O!$Nc$($P(B2$BHVL\$N(B
$B0z?t$r;XDj$9$k(B#2$B$N$h$&$K!$0z?t%Q%i%a!<%?$H$7$F$7$+;H$($^$;$s!%$^$?!$(B
$B$3$N%(%i!<$O!$>e$K$"$2$?(B4$B$D$N%3%^%s%I$,$*8_$$$KF~$l;R$K$J$C$F$$$k>l9g(B
$B$d!$(B\\newenvironment$BL?Na!&(B\\renewenvironment$BL?Na$G(B#2$B$N$h$&$J%Q%i%a!<%?(B
$B$,:G8e$N0z?t$NCf$G;H$o$l$F$$$k>l9g$K$b@8$8$^$9!%(B")

    ("Illegal unit of measure ([^ ]* inserted)." .
"$B$b$7(B
      ! Missing number, treated as zero.
$B$H$$$&%(%i!<$,5/$-$?D>8e$G$"$l$P!$$3$N%(%i!<$N860x$b$=$l$HF1$8$G$9!%(B
$B$=$&$G$J$$>l9g$O!$(BLaTeX$B$,0z?t$H$7$F(Blength$B$r4|BT$7$F$$$k$N$K(Bnumber$B$,(B
$B8=$l$?$3$H$r0UL#$7$F$$$^$9!%$3$N%(%i!<$N:G$b$"$j$,$A$J860x$OD9$5(B0$B$r(B
$BI=$o$9(B0in$B$N$h$&$JI=8=$NBe$o$j$K(B0$B$H$+$$$F$7$^$&$3$H$K$"$j$^$9!%$?$@$7!$(B
$BL?Na$N0z?t$r=q$-K:$l$?>l9g$K$b$3$N%(%i!<$,@8$8$k$3$H$,$"$j$^$9!%(B")

    ("Misplaced alignment tab character \\&." .
"array$B$^$?$O(Btabular$B4D6-$G$N9`L\6h@Z$j$K$N$_;H$o$l$k$Y$-J8;z(B&$B$,IaDL$NJ8(B
$B$NCf$K$"$j$^$7$?!%$?$V$s(B\\&$B$HF~NO$7$?$+$C$?$N$G$7$g$&!%(B")

    ("Missing control sequence inserted." .
"$B$3$N%(%i!<$O!$$*$=$i$/L?NaL>$G$J$$$b$N$r(B\\newcommand$B!&(B\\renewcommand$B!&(B
\\newlength$B$^$?$O(B\\newsavebox$B$NBh(B1$B0z?t$H$7$F;H$C$?$?$a$K@8$8$?$N$G$7$g$&!%(B")

    ("Missing number, treated as zero." .
"$B$3$N%(%i!<$O$?$$$F$$!$0z?t$H$7$F(Bnumber$B$^$?$O(Blength$B$rI,MW$H$7$F$$$kL?Na$K(B
$BBP$7$F0z?t$,M?$($i$l$J$+$C$?$?$a$K@8$8$^$9!%0z?t$r=q$-K:$l$?$N$+!$%F%-%9%H(B
$B$NCf$NBg3g8L(B([])$B$,%*%W%7%g%s0z?t$N;XDj$H4V0c$($i$l$F$7$^$C$?$+$N$I$A$i$+$G(B
$B$7$g$&!%$^$?!$?t$r@8@.$9$k(B\\value$B$N$h$&$JL?Na$d(Blength$BL?Na$NA0$K(B\\protect$B$r(B
$BCV$$$?>l9g$K$b$3$N%(%i!<$O@8$8$^$9!%(B")

    ("Missing [{}] inserted." .
"TeX$B$O4{$K$o$1$,$o$+$i$J$/$J$C$F$$$^$9!%%(%i!<%a%C%;!<%8$K$h$C$F<($5$l$F(B
$B$$$k>l=j$O$?$V$sF~NO$K4V0c$$$,$"$C$?$H$3$m$h$j$b8e$m$K$J$C$F$7$^$C$F$$$k(B
$B$G$7$g$&!%(B")

    ("Missing \\$ inserted." .
"$B$*$=$i$/!$?t<0%b!<%ICf$G$7$+;H$($J$$L?Na$r(BTeX$B$,?t<0%b!<%I30$G8!=P$7$?(B
$B$N$@$H;W$o$l$^$9!%FC$K5-=R$5$l$F$$$J$$8B$j!$(BLaTeX Book(Lamport$BCx(B,$BLu=q(B
$B$O%"%9%-!<=PHG(B)$B$N(B3.3$B@a$K$"$kE:;z!&J,?t!&?t3X5-9f$J$I$N%3%^%s%I$O$9$Y$F(B
$B?t<0%b!<%I$G$7$+;H$($J$$$N$@$H$$$&$3$H$KCm0U$7$F$/$@$5$$!%$?$H$(L?Na$,(B
$B?t<04D6-$NCf$K$"$C$?$H$7$F$b!$(Bbox$B$r@8@.$9$kL?Na$N0z?t$r=hM}$7$O$8$a$?(B
$B;~E@$G$O!$(BTeX$B$O$^$@?t<0%b!<%I$KF~$C$F$$$J$$$N$G$9!%$^$?!$$3$N%(%i!<$O!$(B
$B?t<0%b!<%ICf$G(BTeX$B$,6u9T$r8!=P$7$?>l9g$K$b@8$8$^$9!%(B")

    ("Not a letter." .
"\\hyphenation$BL?Na$N0z?t$NCf$K$J$K$+@5$7$/$J$$$b$N$,$"$j$^$9!%(B")

    ("Paragraph ended before [^ ]* was complete." .
"$BL?Na$N0z?t$NCf$KIT@5$J6u9T$,F~$C$F$7$^$C$F$$$^$9!%$*$=$i$/0z?t$N=*$o$j(B
$B$KJD$83g8L$r$D$1$k$N$rK:$l$?$N$G$7$g$&!%(B")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
"$B$3$N%(%i!<$O$"$^$j0lHLE*$G$J$$%U%)%s%H$,?t<0%b!<%I$G;H$o$l$?;~$K@8$8(B
$B$^$9!%Nc$($P!$5SCm$NCf$N?t<0$G(B\\sc$BL?Na$,;H$o$l$k$H!$(Bfootnotesize$B$N(B
small caps$B%U%)%s%H$,8F$S$@$5$l$k$3$H$K$J$j$^$9!%$3$NLdBj$O(B\\load$BL?Na$r(B
$B;H$($P2r7h$G$-$^$9!%(B")

    ("Font .* not found." .                                    
"$BL$CN$N(Bfamily/series/shape/size$B$NAH$_9g$o$;$N%U%)%s%H$,;XDj$5$l$^$7$?!%(B
$B$3$N%(%i!<$,5/$-$k%1!<%9$O(B2$B$D9M$($i$l$^$9!%(B
   1) \\size$B%^%/%m$G;H$($J$$%5%$%:$rA*Br$7$h$&$H$7$?!%(B
   2) $B$=$&$G$J$1$l$P!$4IM}<T$N$H$3$m$K9T$C$F!$%U%)%s%HA*Br%F!<%V%k$,(B
      $BIe$C$F$$$k$HJ86g$r$D$1$F$d$j$^$7$g$&(B!")

    ("TeX capacity exceeded, sorry .*" .
"TeX$B$,%a%b%j$r;H$$$-$C$F$7$^$$!$<B9T$rCfCG$7$^$7$?!%$7$+$7!$92$F$J$$$G(B
$B$/$@$5$$!%$3$N%(%i!<$,@8$8$?860x$O!$$?$V$s!$(BTeX$B$K$"$J$?$NJ8=q$r=hM}$G(B
$B$-$k$@$1$NG=NO$,$J$$$+$i$G$O$"$j$^$;$s!%(BTeX$B$K%a%b%j$r;H$$$-$i$;$?860x(B
$B$O!$$*$=$i$/F~NO$7$?%U%!%$%k$NA0$NJ}$G@8$8$?%(%i!<$G$9!%$"$J$?$,K\Ev$K(B
TeX$B$NMFNL$rD6$($?$3$H$r$7$h$&$H$7$?$N$+$I$&$+!$$=$7$F$=$N>l9g$I$&$9$l(B
$B$P$$$$$N$+$rH=CG$9$kJ}K!$r0J2<$K@bL@$7$^$9!%$b$7LdBj$,F~NO%U%!%$%kCf$N(B
$B%(%i!<$K$"$k>l9g$O!$8D!9$N%(%i!<$r2r7h$7$F$$$/J}K!$r$H$k$N$,$h$$$G$7$g(B
$B$&!%(BLaTeX$B$,C;$$%U%!%$%k$G%a%b%j$r;H$$$-$k$3$H$O$a$C$?$K$"$j$^$;$s$+$i!$(B
$B%(%i!<$N5/$-$?0LCV$h$jA0$K=hM}$7$?%Z!<%8$,?t%Z!<%8$7$+$J$1$l$P!$$^$:4V(B
$B0c$$$J$/F~NO%U%!%$%k$KLdBj$,$"$k$O$:$G$9!%(B

$B%(%i!<%a%C%;!<%8$N:G8e$K!$(BTeX$B$,;H$$$-$C$F$7$^$C$?%a%b%j$N<oN`$,<($5$l(B
$B$F$$$^$9!%$=$l$i$N$&$A0lHLE*$J$b$N$K$D$$$F!$9M$($i$l$k860x$r0J2<$K5s$2(B
$B$^$9!%(B

buffer size
===========
$B>O@a!&(B\\caption$B!&(B\\addcontentsline$B$"$k$$$O(B\\addtocontents$BL?Na$N0z?t$H(B
$B$7$FM?$($?%F%-%9%H$,D9$9$.$k>l9g$K@8$8$k$3$H$,$"$j$^$9!%$3$N%(%i!<$O(B
$B$?$$$F$$(B\\end{document}$B$r=hM}$7$F$$$k;~$K@8$8$^$9$,!$(B\\tableofcontents$B!&(B
\\listoffigures$B$"$k$$$O(B\\listoftables$BL?Na$r<B9T$7$F$$$k>l9g$K$b5/$-$k(B
$B$3$H$,$"$j$^$9!%$3$NLdBj$r2r7h$9$k$K$O!$$b$C$HC;$$%F%-%9%H$r%*%W%7%g%s(B
$B0z?t$H$7$FM?$($F$/$@$5$$!%L\<!$d?^I=0lMw$r:n@.$7$F$b!$8+=P$7$,D9$9$.$k(B
$B$HFI$_$K$/$/$J$k$O$:$G$9!%(B

exception dictionary
====================
TeX$B$,;}$C$F$$$kNN0h0J>e$K%O%$%U%M!<%7%g%s>pJs$rM?$($h$&$H$7$F$$$^$9!%(B
$B$"$^$j;H$o$J$$C18l$N(B\\hyphenation$BL?Na$r<h$j=|$$$F!$Be$o$j$K(B\\-$BL?Na$r;H$C(B
$B$F$/$@$5$$!%(B

hash size 
=========
$BL?NaL>$NDj5A$^$?$OAj8_;2>H%i%Y%k$NDj5A$,B?$9$.$^$9!%(B

input stack size 
================
$B$3$N%(%i!<$O$*$=$i$/L?NaDj5ACf$N8m$j$K$h$k$b$N$G$9!%Nc$($P!$<!$NL?Na$O(B
$B:F5"E*Dj5A$H$J$C$F$*$j!$<+J,<+?H$r;H$C$F(B\\gnu$B$rDj5A$7$F$$$^$9!%(B

          \\newcommand{\\gnu}{a \\gnu} % $B$3$l$O$@$a(B

$B$3$N(B\\gnu$BL?Na$r8+$D$1$k$H(BTeX$B$O(B\\gnu$B$,2?$r$&$_$@$9$N$+$r7hDj$7$h$&$H$7(B
$B$F$=$NKvHx$r$$$D$^$G$bDI$$$D$E$1!$$d$,$F(B``input stack''$B$r;H$$$-$C$F$7(B
$B$^$$$^$9!%(B

main memory size 
================
$B$3$l$O!$(BTeX$B$,C;$$%U%!%$%k$r=hM}$7$F$$$k;~$K;H$$$-$k2DG=@-$N$"$k%a%b%j(B
$B$N$R$H$D$G$9!%(Bmain memory$B$r;H$$$-$k$N$O<!$N(B3$B$D$N>l9g$N$$$:$l$+$G$9!%(B
\(1\)$BHs>o$KD9$/J#;($JL?Na$r?tB?$/Dj5A$7$?!%(B(2)index$B$^$?$O(Bglossary$B$r:n$C(B
$B$F$$$k$H$-!$(B1$B%Z!<%8Cf$K$"$^$j$K$bB?$/$N(B\\index$B$^$?$O(B\\glossary$BL?Na$,$"(B
$B$k!%(B(3)$B@8@.$N$?$a$N>pJs$r(BTeX$B$,J];}$7$-$l$J$$$h$&$J!$$"$^$j$K$bJ#;($J%Z!<(B
$B%8$r@8@.$7$h$&$H$7$?!%:G=i$N(B2$B$D$NLdBj$N2r7hJ}K!$OL@$i$+$G$9!%L?NaDj5A(B
$B$N?t$"$k$$$O(B\\index$B!&(B\\glossary$BL?Na$N?t$r8:$i$9$3$H$G$9!%(B3$BHVL\$NLdBj$O(B
$B$A$g$C$HLq2p$G$9!%$3$l$O!$Bg$-$J(Btabbin$B!&(Btabular$B!&(Barray$B!&(Bpicture$B4D6-$N(B
$B$;$$$G@8$8$k$3$H$,$"$j$^$9!%=PNO0LCV$,7hDj$5$l$k$N$rBT$C$F$$$k?^$dI=$G(B
TeX$B$N%a%b%j$,$$$C$Q$$$K$J$C$F$$$k$N$+$b$7$l$^$;$s!%K\Ev$K(BTeX$B$NMFNL$rD6(B
$B$($F$7$^$C$?$N$+$I$&$+D4$Y$k$?$a$K$O!$%(%i!<$N5/$3$C$?>l=j$ND>A0$K(B
\\clearpage$BL?Na$rF~$l$F$b$&0lEY%3%s%Q%$%k$r<B9T$7$F$_$F$/$@$5$$!%$b$7(B
$B$=$l$G$b%a%b%j$,B-$j$J$/$J$k$h$&$J$i!$$J$s$i$+$N<jCJ$r9V$8$kI,MW$,$"$j(B
$B$^$9!%(BTeX$B$,%Z!<%8$r@ZCG$9$k$+$I$&$+7hDj$9$k$?$a$K$OCJMnA4BN$r=hM}$7$J(B
$B$1$l$P$J$i$J$$$H$$$&$3$H$r;W$$$@$7$F$/$@$5$$!%CJMn$NESCf$K(B\\newpage$BL?(B
$BNa$rF~$l$l$P!$CJMn$N;D$j$r=hM}$9$kA0$K:#$N%Z!<%8$r(BTeX$B$K=PNO$5$;$k$3$H(B
$B$GM>M5$,$G$-$k$+$b$7$l$^$;$s(B(\\pagebreak$BL?Na$G$O$@$a$G$9(B)$B!%$b$7?^$dI=(B
$B$,N/$^$C$F$$$k$3$H$,LdBj$J$N$J$i$P!$?^I=$r$b$C$H8e$m$NJ}$K0\F0$9$k$H$+!$(B
$B$"$k$$$O$b$C$HA0$N;~E@$G=PNO$5$l$k$h$&$K$9$l$P2sHr$G$-$^$9!%$b$7$^$@J8(B
$B=q$r:n@.$7$F$$$kESCf$J$i!$$H$j$"$($:(B\\clearpage$BL?Na$rF~$l$F$*$$$F!$:G(B
$B=*HG$r:n$k;~$^$G$3$NLdBj$OC*>e$2$7$F$*$-$^$7$g$&!%F~NO%U%!%$%k$,JQ$o$k(B
$B$HLdBj$,2r>C$5$l$k>l9g$b$"$k$N$G$9!%(B

pool size 
=========
$BAj8_;2>H$N(B\\label$B$,B?$9$.$k$+!$L?Na$NDj5A$,B?$9$.$k$+$N$I$A$i$+$G$9!%(B
$B@53N$K$$$($P!$Dj5A$7$?%i%Y%kL>$*$h$SL?NaL>$K;H$C$?J8;z?t$,B?$9$.$k$H$$(B
$B$&$3$H$G$9!%$G$9$+$i!$$b$C$HC;$$L>A0$r;H$($P$3$NLdBj$O2r7h$7$^$9!%$?$@(B
$B$7!$$3$N%(%i!<$O!$(B\\setcounter$B$J$I$N%+%&%s%?L?Na$d(B\\newenvironment$B!&(B
\\newtheorem$BL?Na$N0z?t$N=*$o$j$r<($91&3g8L$rK:$l$?>l9g$K$b@8$8$^$9!%(B

save size 
=========
$B$3$N%(%i!<$O!$@k8@$NM-8zHO0O$dL?Na!&4D6-$,$"$^$j$K$b?<$/F~$l;R$K$J$C$F(B
$B$$$k>l9g$K@8$8$^$9!%$?$H$($P!$(B\\multiput$BL?Na$N0z?t$K(Bpicture$B4D6-$,$"$j!$(B
$B$=$N$J$+$K(B\\footnotesize$B@k8@$,$"$j!$$=$N@k8@$NM-8zHO0O$K(B\\multiput$BL?Na(B
$B$,$"$C$F!$$=$N0z?t$K(B... $B$H$$$&$h$&$J>l9g$G$9!%(B")

    ("Text line contains an invalid character." .
"$BF~NOCf$KIT@5$JJ8;z$,4^$^$l$F$$$^$9!%%U%!%$%k:n@.$N8m$j$K$h$C$F%F%-%9%H(B
$B%(%G%#%?$,$3$NJ8;z$rA^F~$7$F$7$^$C$?$N$G$7$g$&!%<B:]$K2?$,5/$-$?$N$+$O(B
$B%(%G%#%?$K$h$j$^$9!%F~NO%U%!%$%k$rD4$Y$F$_$F!$;XE&$5$l$?J8;z$,8+$D$+$i(B
$B$J$$>l9g$K$O%m!<%+%k%,%$%I$r8+$F$/$@$5$$!%(B")

    ("Undefined control sequence."   .  
"TeX$B$,L$Dj5A$NL?NaL>$rH/8+$7$^$7$?!%$*$=$i$/F~NO$N8m$j$G$7$g$&!%$b$7$3(B
$B$N%(%i!<$,(BLaTeX$BL?Na$N=hM}Cf$K@8$8$?>l9g$O!$$=$NL?Na$O4V0c$C$?0LCV$KCV$+(B
$B$l$F$$$^$9!%Nc$($P!$%j%9%H4D6-$NCf$G$J$$$N$K(B\\item$BL?Na$,;H$o$l$?>l9g$J$I(B
$B$G$9!%$^$?!$(B\\documentstyle$BL?Na$,$J$$>l9g$K$b$3$N%(%i!<$,@8$8$^$9!%(B")

    ("Use of [^ ]* doesn't match its definition." .
"$B$*$=$i$/IA2h$N$?$a$NL?Na$@$H;W$o$l$^$9$,!$0z?t$N;H$$$+$?$,4V0c$C$F$$(B
$B$^$9!%4V0c$C$F$$$k$N$,(B\\@array$BL?Na$N>l9g$O!$(Barray$B4D6-$+(Btabular$B4D6-$G$N(B
@$BI=8=$N0z?t$K$J$K$+8m$j$,$"$k$N$G$7$g$&!%(Bfragile$B$JL?Na$,(B\\protect$B$5$l$F(B
$B$$$J$$$N$+$b$7$l$^$;$s!%(B")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
"$BFC<lJ8;z(B#$B$,IaDL$N%F%-%9%H$NCf$K8=$l$^$7$?!%$*$=$i$/(B\\#$B$H=q$-$?$+$C$?(B
$B$N$G$7$g$&!%(B")

    ("Overfull \\\\hbox .*" .
"$B9TJ,3d$N$?$a$NE,@Z$J>l=j$,8+$D$+$i$J$+$C$?$N$G!$(B1$B9T$K<}$^$k$Y$-J,NL0J>e(B
$B$N=PNO$,9T$J$o$l$F$7$^$$$^$7$?!%(B")

    ("Overfull \\\\vbox .*" .
"$B%Z!<%8J,3d$N$?$a$NE,@Z$J>l=j$,8+$D$+$i$J$+$C$?$N$G!$(B1$B%Z!<%8$K<}$^$k$Y$-(B
$BJ,NL0J>e$N=PNO$,9T$J$o$l$F$7$^$$$^$7$?!%(B")

    ("Underfull \\\\hbox .*" .
"$BM>J,$J?bD>%9%Z!<%9$,$J$$$+$I$&$+=PNO$r3N$+$a$F$/$@$5$$!%$b$7$"$l$P!$$=(B
$B$l$O(B\\\\$BL?Na$^$?$O(B\\newline$BL?Na$K4X78$9$kLdBj$N$?$a$K@8$8$?$b$N$G$9!%Nc(B
$B$($P(B2$B$D$N(B\\\\$BL?Na$,B3$$$F$$$k>l9g$J$I$G$9!%$3$N7Y9p$O(Bsloppypar$B4D6-$d(B
\\sloppy$B@k8@$N;HMQ!$$"$k$$$O(B\\linebreak$BL?Na$NA^F~$J$I$K$h$k>l9g$b$"$j$^$9!%(B")

    ("Underfull \\\\vbox .*" .
"$B%Z!<%8$rJ,3d$9$k$?$a$NE,@Z$J>l=j$,8+$D$1$i$l$:!$==J,$J%F%-%9%H$N$J$$(B
$B%Z!<%8$,$G$-$F$7$^$$$^$7$?!%(B")

;; New list items should be placed here 
;; 
;; ("err-regexp" . "context") 
;; 
;; the err-regexp item should match anything

    (".*" . "$B$4$a$s$J$5$$!%3:Ev$9$k%X%k%W%a%C%;!<%8$,$"$j$^$;$s!%(B"))))

(provide 'tex-jp)

;;; tex-jp.el ends here
