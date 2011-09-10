;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner Sun Dec 17 22:37:52 EST 1989
;Article 1075 of comp.emacs:
;Xref: ark1 comp.lang.eiffel:232 comp.emacs:1075
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner
;>From: weiner@novavax.UUCP (Bob Weiner)
;Newsgroups: comp.lang.eiffel,comp.emacs
;Subject: Much improved version of GNU Emacs Eiffel editing mode
;Message-ID: <1694@novavax.UUCP>
;Date: 15 Dec 89 23:25:20 GMT
;Organization: Nova University, Fort Lauderdale, FL
;Lines: 705
;
;ISE, the creators of the object-oriented language Eiffel, recently
;posted a very basically modified version of Omohundro's Eiffel mode.
;This is a revision that adds a number of interesting features and simply
;works a good deal better.  It may no longer conform to ISE's indentation
;conventions which are extremely wasteful of whitespace, but it
;definitely makes the code easier to read, and of course the basic unit
;of indentation is controlled by a variable.  Here's to readable, well
;documented code.
;
;How many people would be interested in an efficient, Smalltalk-like
;browser (but better) for Eiffel that runs entirely within GNU Emacs  (no
;X windows or vt100 necessary)?  Let me know since it's already finished.
;
;			Bob Weiner
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cut Here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for editing Eiffel programs.
;; Author: Stephen M. Omohundro 
;; International Computer Science Institute
;; om@icsi.berkeley.edu
;; Created: May 26, 1989
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive Software Engineering
;; eiffel@eiffel.com
;; Date: November 15, 1989
;;    Updated to Eiffel 2.2 Syntax and Eiffel Code style outline in
;;       Eiffel: The Language (pp 239-251)
;;
;; Eiffel 2.2 keywords: 
;;
;; and as check class debug deferred define div do else elsif end ensure
;; expanded export external false feature from indexing if implies infix
;; inherit inspect invariant is language like local loop mod name not
;; obsolete old once or prefix redefine rename repeat require rescue retry
;; then true unique until variant when xor
;;
;; 
;; Bob Weiner, Motorola Inc., 9/25/89
;;  Added comment variables so comment filling is done properly with
;;    par-align.el.
;;  Added a few keywords to the mode-specific abbrev table.
;; Bob Weiner, Motorola Inc., 10/12/89
;;  Added "indexing" keyword and 'eiffel-indices' list for default entries.
;; Bob Weiner, Motorola Inc., 11/29/89
;;  Added local documentation standard headers.
;;  Added a few keybindings to insert other Eiffel construct templates.
;;  Fixed mode-specific variable settings for comments.
;; Bob Weiner, Motorola Inc., 12/01/89
;;  Fixed many indentation problems.  'rename', 'redefine', and 'define'
;;  clauses are indented very intelligently now.  Made each tabstop much
;;  narrower than ISE's conventions which leads to much more readable code
;;  that fits in 80 columns much more often also!
;; Bob Weiner, Motorola Inc., 12/01/89
;;  Added 'eiffel-line-type' command to show programmer the type of the
;;  current line.
;;  Improved comment indentation; more context sensitivity.
;;
;; What is missing?
;;   1. Line and string continuations do not indent correctly.
;;      (This could probably be remedied with a thorough
;;       look over of c-mode.el)
;;   2. Some better checking of correctness in the eiffel-elsif
;;      and eiffel-when functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following two statements, placed in a .emacs file or site-init.el,
;; will cause this file to be autoloaded, and eiffel-mode invoked, when
;; visiting .e files:
;;
;;      (autoload 'eiffel-mode "eiffel.el" "Eiffel mode" t nil)
;;      (setq auto-mode-alist
;;            (append
;;              (list (cons "\\.e$" 'eiffel-mode))
;;              auto-mode-alist))
;;
;; -*- emacs-lisp -*-

;; SET THE FOLLOWING VALUE TO TASTE.  TRY IT AND THEN ALTER AS NECESSARY.
;; IF t, AFFECTS class, function, procedure, and attribute TEMPLATES.
;; ALL OF THE ADDITIONAL HEADER INFORMATION IS GENERALLY USEFUL, NOT MOTOROLA
;; SPECIFIC.
(defvar eiffel-moto-hdr-p t
  "If t, use our Motorola developed Eiffel construct template headers.")

;; These are used only if the above setting it t.
(defconst eiffel-moto-procedure-hdrs
  '("EFFECTS" "INPUTS" "OUTPUTS" "MODIFIES" "SIGNALS" "INTERNAL"))
(defconst eiffel-moto-function-hdrs
  '("RETURNS" "INPUTS" "OUTPUTS" "MODIFIES" "SIGNALS" "INTERNAL"))
(defconst eiffel-moto-attribute-hdrs
  '("RETURNS" "SIGNALS" "INTERNAL"))


(defconst eiffel-indices
  '("names: " "keywords: " "representation: " "access: " "size: " "contents: ") 
  "Indexing criteria for Eiffel classes.")

(defvar eiffel-mode-map nil 
  "Keymap for Eiffel mode.")
(if eiffel-mode-map
    nil
  (setq eiffel-mode-map (make-sparse-keymap))
  (define-key eiffel-mode-map "\C-cc" 'eiffel-class)
  (define-key eiffel-mode-map "\C-cf" 'eiffel-function)
  (define-key eiffel-mode-map "\C-cp" 'eiffel-procedure)
  (define-key eiffel-mode-map "\C-ca" 'eiffel-attribute)
  (define-key eiffel-mode-map "\C-ci" 'eiffel-if)
  (define-key eiffel-mode-map "\C-cl" 'eiffel-loop)
  (define-key eiffel-mode-map "\C-cs" 'eiffel-set)
  (define-key eiffel-mode-map "\C-cn" 'eiffel-inspect)
  (define-key eiffel-mode-map "\C-cw" 'eiffel-when)
  (define-key eiffel-mode-map "\C-ce" 'eiffel-elsif)
  (define-key eiffel-mode-map "\t" 'eiffel-indent-line)
  (define-key eiffel-mode-map "\C-ct" 'eiffel-line-type)
  (define-key eiffel-mode-map "\r" 'eiffel-return)
  (define-key eiffel-mode-map "\177" 'backward-delete-char-untabify)
  (define-key eiffel-mode-map "\M-;" 'eiffel-comment)
  )


(defvar eiffel-mode-syntax-table nil
  "Syntax table in use in Eiffel-mode buffers.")

(if eiffel-mode-syntax-table
    nil
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq eiffel-mode-syntax-table table)))

(defvar eiffel-mode-abbrev-table nil
  "*Abbrev table in use in Eiffel-mode buffers.")
(if eiffel-mode-abbrev-table
    nil
  (define-abbrev-table 'eiffel-mode-abbrev-table ())
  (define-abbrev eiffel-mode-abbrev-table "int" "INTEGER" nil)
  (define-abbrev eiffel-mode-abbrev-table "boo" "BOOLEAN" nil)
  (define-abbrev eiffel-mode-abbrev-table "cha" "CHARACTER" nil)
  (define-abbrev eiffel-mode-abbrev-table "str" "STRING" nil)
  (define-abbrev eiffel-mode-abbrev-table "rea" "REAL" nil)
  (define-abbrev eiffel-mode-abbrev-table "dou" "DOUBLE" nil)
  (define-abbrev eiffel-mode-abbrev-table "res" "Result" nil)
  (define-abbrev eiffel-mode-abbrev-table "cre" "Create" nil)
  (define-abbrev eiffel-mode-abbrev-table "fgt" "Forget" nil)
  (define-abbrev eiffel-mode-abbrev-table "cur" "Current" nil))

(defconst eiffel-indent 3
  "*This variable gives the indentation in Eiffel-mode")

(defconst eiffel-comment-col 32
  "*This variable gives the desired comment column for comments to the right
of text.")

(defun eiffel-mode ()
  "A major editing mode for the language Eiffel.
Comments are begun with --.
Paragraphs are separated by blank lines
Delete converts tabs to spaces as it moves back.
Tab anywhere on a line indents it according to Eiffel conventions.
M-; inserts and indents a comment on the line, or indents an existing
comment if there is one.
Return indents to the expected indentation for the new line.
Skeletons of the major Eiffel constructs are inserted with:

 C-c c class           C-c i if          C-c s set-procedure
 C-c f function        C-c p procedure   C-c a attribute
 C-c l loop            M-;   comment

Abbreviations:
 int   for  INTEGER           boo  for  BOOLEAN
 cha   for  CHARACTER         str  for  STRING
 rea   for  REAL              dou  for  DOUBLE
 res   for  Result            cre  for  Create
 cur   for  Current           fgt  for  Forget

Variables controlling style:
   eiffel-indent          Indentation of Eiffel statements.
   eiffel-comment-col     Goal column for inline comments

Turning on Eiffel mode calls the value of the variable eiffel-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eiffel-mode-map)
  (setq major-mode 'eiffel-mode)
  (setq mode-name "Eiffel")
  (setq local-abbrev-table eiffel-mode-abbrev-table)
  (set-syntax-table eiffel-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'eiffel-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+[ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (run-hooks 'eiffel-mode-hook))

(defun eiffel-class ()
  "Insert a 'class' template."
  (interactive)
  (let ((cname (read-string "Class: ")))
    (if (not (e-empty-line-p))
	(progn (end-of-line)(newline)))
    (indent-to 0)                         
    (if eiffel-moto-hdr-p
	nil
      (insert "--| Author: " (user-full-name) "\n")
      (insert "--| Created: " (current-time-string) "\n\n"))
    (if eiffel-indices
	(progn (insert "indexing\n\n")
	       (mapcar '(lambda (idx)
			  (indent-to eiffel-indent)
			  (insert idx "\n"))
		       eiffel-indices)
	       (insert "\n")))
    (insert "class " (upcase cname)
            " export\n\ninherit\n\nfeature\n\ninvariant\n\nend")
    (and (not eiffel-moto-hdr-p) (insert " -- class " cname))
    )
  (re-search-backward "\ninherit" nil t)
  (eiffel-indent-line))

(defun eiffel-procedure ()
  "Insert a 'procedure' template."
  (interactive)
  (let ((pname (read-string "Procedure name: ")))
    (if (not (e-empty-line-p))
	(progn (end-of-line)(newline)))
    (indent-to eiffel-indent)
    (insert pname " () is\n")
    (if eiffel-moto-hdr-p
	(mapcar '(lambda (hdr)
		   (indent-to (* 3 eiffel-indent))
		   (insert "-- " hdr ":")
		   (indent-to-column (+ (current-column) (- 10 (length hdr))))
		   (insert "\n"))
		eiffel-moto-procedure-hdrs)
      (indent-to (* 3 eiffel-indent))
      (insert "-- \n"))
    (mapcar '(lambda (keyword)
	       (indent-to (* 2 eiffel-indent))
	       (insert keyword "\n"))
	    '("require" "local" "do" "ensure" "end;"))
    (if eiffel-moto-hdr-p
	nil
      (forward-line -1)
      (end-of-line)
      (insert " -- " pname))
    (search-backward ")" nil t)))

(defun eiffel-function ()
  "Insert a 'function' template."
  (interactive)
  (let ((fname (read-string "Function name: "))
	(type (upcase (read-string "Return type: "))))
    (if (not (e-empty-line-p))
	(progn (end-of-line)(newline)))
    (indent-to eiffel-indent)
    (insert fname " (): " type " is\n")
    (if eiffel-moto-hdr-p
	(mapcar '(lambda (hdr)
		   (indent-to (* 3 eiffel-indent))
		   (insert "-- " hdr ":")
		   (indent-to-column (+ (current-column) (- 10 (length hdr))))
		   (insert "\n"))
		eiffel-moto-function-hdrs)
      (indent-to (* 3 eiffel-indent))
      (insert "-- \n"))
    (mapcar '(lambda (keyword)
	       (indent-to (* 2 eiffel-indent))
	       (insert keyword "\n"))
	    '("require" "local" "do" "ensure" "end;"))
    (if eiffel-moto-hdr-p
	nil
      (forward-line -1)
      (end-of-line)
      (insert " -- " fname))
    (search-backward ")" nil t)))

(defun eiffel-attribute ()
  "Insert an 'attribute' template."
  (interactive)
  (if (not (e-empty-line-p))
      (progn (end-of-line)(newline)))
  (indent-to eiffel-indent)                             
  (let ((aname (read-string "Attribute name: "))
	(type (upcase (read-string "Attribute type: "))))
    (insert aname ": " type "\n")
    (if eiffel-moto-hdr-p
	(let ((opoint (point)))
	  (mapcar '(lambda (hdr)
		     (indent-to (* 3 eiffel-indent))
		     (insert "-- " hdr ":")
		     (indent-to-column (+ (current-column) (- 10 (length hdr))))
		     (insert "\n"))
		  eiffel-moto-attribute-hdrs)
	  (goto-char opoint))
      (indent-to (* 3 eiffel-indent))
      (insert "-- \n"))
    (eiffel-indent-line)
    (end-of-line)))

(defun eiffel-if ()
  "Insert an 'if' statement template."
  (interactive)
  (mapcar '(lambda (s)
	     (insert s)
	     (eiffel-indent-line))
	  '("if  then" "\n\nelse" "\n\nend;"))
  (re-search-backward " then" nil t))

(defun eiffel-inspect ()
  "Insert an 'inspect-when' statement template."
  (interactive)
  (mapcar '(lambda (s)
	     (insert s)
	     (eiffel-indent-line))
	  '("inspect " "\n\nwhen  then" "\n\nend;"))
  (beginning-of-line)
  (re-search-backward "inspect" nil t) (forward-line) (eiffel-indent-line))

(defun eiffel-when ()
  "Insert another 'when-then' clause."
  ;; Obvious improvement -- have this check to see it this is a valid
  ;; location for this construct, before inserting it.
  (interactive)
  (insert "\nwhen  then")
  (eiffel-indent-line)
  (insert "\n\n")
  (re-search-backward " then" nil t))

(defun eiffel-elsif ()
  "Insert an 'elsif-then' clause."
  ;; Obvious improvement -- have this check to see it this is a valid
  ;; location for this construct, before inserting it.
  (interactive)
  (insert "\nelsif  then")
  (eiffel-indent-line)
  (insert "\n\n")
  (re-search-backward " then" nil t))

(defun eiffel-loop ()
  "Insert a 'loop' statement template."
  (interactive)
  (mapcar '(lambda (s)
	     (insert s)
	     (eiffel-indent-line))
	  '("from  " "\n\ninvariant" "\n\nvariant" "\n\nuntil" "\n\nloop" "\n\nend;"))
  (re-search-backward "from" nil t)(forward-line)(eiffel-indent-line))

(defun eiffel-set ()
  "Inserts a function to set the value of the given variable."
  (interactive)
  (let ((aname (read-string "Attribute name: "))
	(atype (upcase (read-string "Attribute type: "))))
    (insert "set_" aname " (n" aname ": " atype ") is")
    (mapcar '(lambda (s)
	       (eiffel-indent-line)
	       (insert s))
	    '("\n-- " "\ndo"))
    (eiffel-indent-line)
    (insert "\n" aname " := n" aname)
    (eiffel-indent-line)
    (insert "\nend;")
    (if (not eiffel-moto-hdr-p) (insert " -- set_" aname))
    (eiffel-indent-line)
    (insert "\n")
    (re-search-backward "^[ \t]*--" nil t)
    (end-of-line)))

(defun eiffel-return ()
  "Indent line, insert newline and new current line line."
  (interactive)
  (eiffel-indent-line)
  (newline)
  (eiffel-indent-line))

(defun eiffel-indent-line ()
  "Indent the current line as Eiffel code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (e-calc-indent)))
  (skip-chars-forward " \t"))

;; A line is one of the following:
;;    a block end
;;    a blank, 
;;    a comment only, 
;;    begins with a block-cont-keyword, i.e. a regular keyword,
;;    begins with a qualifier-keyword,
;;    a line that continues a qualifier clause, 
;;    a block-head or general line.

(defvar e-last-indent-type nil
  "String description of type of line that was last indented.
Use to debug 'e-calc-indent' function.")

(defun eiffel-line-type ()
  "Displays type of current line.
Useful in debugging Eiffel indentation code and Eiffel syntax."
  (interactive)
  (eiffel-indent-line)
  (message (concat "Current line type is: " e-last-indent-type)))

(defun e-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (cond
    ;; At the end of or a line following an 'end'
    ((e-ends-with-end-p)
     (setq e-last-indent-type "BLOCK END")
     (+ eiffel-indent (e-get-block-indent)))
    ((e-empty-line-p)               ;an empty line 
     (setq e-last-indent-type "BLANK")
     (+ eiffel-indent (e-get-block-indent))) ;go in one from block
    ((e-comment-line-p)             ;a comment line
     (setq e-last-indent-type "COMMENT")
     (e-comment-indent))
    ((e-block-cont-p)               ;begins with cont keyword
     (setq e-last-indent-type "REGULAR KEYWORD")
     (e-get-block-indent))          ;indent same as block
    ((e-qualifier-block-p)          ;indent two times
     (setq e-last-indent-type "QUALIFIER KEYWORD")
     (+ (* 2 eiffel-indent) (e-get-block-indent))) ;goes two in
    (t                              ;block-head or something else
      (+ eiffel-indent 
	 (let ((in (e-in-qualifier-indent)))
	   (if (= in 0)
	       (setq e-last-indent-type "GENERAL")
	     (setq e-last-indent-type "QUALIFIER CONTINUED"))
	   in)
	 (e-get-block-indent)))))

(defun eiffel-comment ()
  "Edit a comment on the line.  If one exists, reindent it and move to it, 
otherwise, create one. Gets rid of trailing blanks, puts one space between
comment header comment text, leaves point at front of comment. If comment is
alone on a line it reindents relative to surrounding text. If it is before
any code, it is put at the line beginning.  Uses the variable eiffel-comment-col 
to set goal start on lines after text."
  (interactive)
  (cond ((e-comment-line-p)             ;just a comment on the line
         (beginning-of-line)
         (delete-horizontal-space)
         (indent-to (e-comment-indent))
         (forward-char 2)(delete-horizontal-space)(insert " "))
        ((e-comment-on-line-p)          ;comment already at end of line
         (cond ((e-ends-with-end-p)     ;end comments come immediately
                (e-goto-comment-beg)(delete-horizontal-space)(insert " ")
                (forward-char 2)(delete-horizontal-space)(insert " "))
               (t
                (e-goto-comment-beg)(delete-horizontal-space)
                (if (< (current-column) eiffel-comment-col)
                    (indent-to eiffel-comment-col)
                  (insert " "))
                (forward-char 2)
		(delete-horizontal-space)
		(insert " "))))
        ((e-empty-line-p)               ;put just a comment on line
         (beginning-of-line)
         (delete-horizontal-space)
         (indent-to (e-comment-indent))
         (insert "-- "))
        ((e-ends-with-end-p)            ;end comments come immediately
         (end-of-line)(delete-horizontal-space)(insert " -- "))
        (t                              ;put comment at end of line
         (end-of-line)
         (delete-horizontal-space)
         (if (< (current-column) eiffel-comment-col)
             (indent-to eiffel-comment-col)
           (insert " "))
         (insert "-- "))))
  
(defun e-ends-with-end-p ()
  "t if line ends with 'end' or 'end;' and a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\(.*[ \t]+\\)?end;?[ \t]*\\($\\|--\\)")))

(defun e-empty-line-p ()
  "True if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun e-comment-line-p ()
  "t if current line is just a comment."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "--")))

(defun e-comment-on-line-p ()
  "t if current line contains a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "[^\n]*--")))

(defun e-in-comment-p ()
  "t if point is in a comment."
  (save-excursion
    (and (/= (point) (point-max)) (forward-char 1))
    (search-backward "--" (save-excursion (beginning-of-line) (point)) t)))

(defun e-current-indentation ()
  "Returns current line indentation."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-indentation)))

(defun e-goto-comment-beg ()
  "Point to beginning of comment on line.  Assumes line contains a comment."
  (beginning-of-line)
  (search-forward "--" nil t)
  (backward-char 2))

(defun e-block-cont-p ()
  "t if line continues the indentation of enclosing block."
  (save-excursion
    (beginning-of-line)
    (looking-at e-block-keyword-regexp)))

(defconst e-block-keyword-regexp
  "\\(^\\|[ \t]+\\)\\(indexing\\|class\\|export\\|inherit\\|feature\\|rescue\
\\|invariant\\|require\\|external\\|local\\|do\\|once\\|expanded\\|when\
\\|deferred\\|ensure\\|then\\|elsif\\|else\\|variant\\|until\\|loop\\)\\([ \t]\\|$\\)"
  "Eiffel block keywords requiring special indentation.")

(defun e-qualifier-block-p ()
  "t if line gets double indent because of qualifier keyword."
  (save-excursion
    (beginning-of-line)
    (looking-at e-qualifier-regexp)))

(defconst e-qualifier-regexp
  "\\(^\\|[ \t]+\\)\\(rename\\|\\(re\\)?define\\|check\\|debug\\)\\([ \t]\\|$\\)"
  "Eiffel qualifier keywords requiring special indentation.")

(defun e-in-qualifier-indent ()
  "Indent relative to qualifier keyword if still in clause, else 0."
  ;; Assume current line does not begin with a keyword, otherwise this
  ;; function would not be called.
  (let ((qual-indent 0))
    (if (e-block-cont-p)
	nil
      (save-excursion
	(if (/= (forward-line -1) 0) ; Failed
	    nil
	  (end-of-line)
	  (if (re-search-backward (concat ";\\|\\(" e-qualifier-regexp "\\)\\|"
					  e-block-keyword-regexp)
				  nil t)
	      (progn (if (looking-at e-qualifier-regexp)
			 (progn (setq qual-indent
				      (+ 4 (- (match-end 2) (match-beginning 2))))
				(goto-char (match-end 2))
				(if (looking-at "[ \t]*\\(--\\|$\\)")
				    (setq qual-indent (* eiffel-indent 2)))))
		     (if (e-in-comment-p) (setq qual-indent 0)))
	    ))))
    qual-indent))
		
(defun e-ends-with-is ()
  "t if current line ends with the keyword 'is' and an optional comment."
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (re-search-forward "\\(^\\|[ \t]\\)is[ \t]*\\($\\|--\\)" end t))))

(defun e-move-to-prev-non-comment ()
  "Moves point to previous line excluding comment lines and blank lines. 
Returns t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*\\([^ \t---\n]\\|-[^---]\\)" nil t))

(defun e-move-to-prev-non-blank ()
  "Moves point to previous line excluding blank lines. 
Returns t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*[^ \t\n]" nil t))

(defun e-comment-indent ()
  "Return indentation for a comment line."
    (save-excursion
      (let ((in (e-get-block-indent))
	    (prev-is-blank
	      (save-excursion (and (= (forward-line -1) 0) (e-empty-line-p)))))
      (if (or (and prev-is-blank (= in 0))
	      (not (e-move-to-prev-non-blank))) ;move to prev line if there is one
	  0                                     ;early comments start to the left
	(cond ((e-ends-with-is)             ;line ends in 'is,' indent twice
	       (+ (* eiffel-indent 2) (e-current-indentation)))
	      ((e-comment-line-p)         ;is a comment, same indentation
	       (e-current-indentation))
	      (t                          ;otherwise indent once
		(+ eiffel-indent (e-current-indentation))))))))

(defun e-in-comment-p ()
  "t if point is in a comment."
  (cond ((e-comment-on-line-p)
         (let ((pt (current-column)))
           (save-excursion
             (e-goto-comment-beg)
             (if (<= (current-column) pt)
                 t
               nil))))
        (t
         nil)))

(defun e-quoted-string-on-line-p ()
  "t if a an Eiffel quoted string begins, ends, or is continued on current line."
  (save-excursion
    (beginning-of-line)
    ;; Line must either start with optional whitespace immediately followed
    ;; by a '\\' or include a '\"'.  It must either end with a '\\' character
    ;; or must include a second '\"' character.
    (looking-at "^\\([ \t]*\\\\\\|[^\"\n]*\"\\)[^\"\n]*\\(\\\\$\\|\"\\)")))

(defun e-in-quoted-string-p ()
  "t if point is in a quoted string."
  (let ((pt (point)) front)
    (save-excursion
      ;; Line must either start with optional whitespace immediately followed
      ;; by a '\\' or include a '\"'.
      (if (re-search-backward "\\(^[ \t]*\\\\\\|\"\\)"
			      (save-excursion (beginning-of-line) (point)) t)
	  (progn (setq front (point))
		 (forward-char 1)
		 ;; Line must either end with a '\\' character or must
		 ;; include a second '\"' character.
		 (and (re-search-forward
			"\\(\\\\$\\|\"\\)"
			(save-excursion (end-of-line) (point)) t)
		      (>= (point) pt)
		      (<= front pt)
		      t)))
      )))

(defun e-get-block-indent ()
  "Return the outer indentation of the current block. Returns 0 or less if it can't
find one."
  (let ((indent 0) (succeed))
    (save-excursion
      (setq succeed (e-goto-block-head))
      (cond ((not succeed) nil)
	    ;; heads ending in 'is' have extra indent
            ((looking-at "is")
             (setq indent (+ (current-indentation) eiffel-indent)))
            (t
	      (setq indent (current-indentation)))))
    (if (e-ends-with-end-p)
	(setq indent (- indent eiffel-indent)))
    (if succeed
        indent
      -20)))                            ;will put at first col if lost

(defun e-goto-block-head ()
  "Move point to the block head that would be paired with an end at point.
Return nil if none."
  (let ((depth 1))
    (while (and (> depth 0)
		;; Search for start of keyword
		(re-search-backward
		  "\\(^\\|[ \t]\\)\\(indexing\\|class\\|expanded\\|\
deferred[ \t]+class\\|if\\|from\\|check\\|inspect\\|\is\\|debug\\|\
end\\)[ \t;\n]" nil t))
      (goto-char (match-beginning 2))
      (cond ((or (e-in-comment-p)
		 (e-in-quoted-string-p))
             nil)                       ;ignore it
            ((looking-at "end")         ;end of block
             (setq depth (1+ depth)))
            (t                          ;head of block
             (setq depth (1- depth)))))
    (if (> depth 0)                     ;check whether we hit top of file
        nil
      t)))

(provide 'eiffel-mode)
;-- 
;Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
;(407) 364-2087
;

