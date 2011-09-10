;; This is sr-mode for GNU EMACS.  It provides a mode for editing sr programs.
;; Author:      David Jacobson  (jacobson@hplabs.hp.com).
;; Maintainer:  Thomas Lofgren  (d95thl@docs.uu.se)
;; 
;; Copyright (c) 1988 Hewlett-Packard Company, all rights reserved.
;; Copyright (C) 1998 by Thomas Lofgren and Helwett-Packard Company
;; 
;;                             LEGAL NOTICE
;; 
;; This sr-mode package is experimental and HP shall have no obligation to
;; maintain or support it.  HP makes no express or implied warranty of any
;; kind with respect to this software, and HP shall not be liable for any
;; direct, indirect, special, incidental or consequential damages (whether
;; based on contract, tort or any other legal theory) arising in any way from
;; use of the software.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Original distribution, David Jacobson, Dec. 30, 1988
;;      Modified, David Jacobson Feb. 10, 1989
;;
;;      Modified, Thomas Lofgren Jan 28, 1998, to run sr-load-hook.
;;      Modified, Thomas Lofgren Mar 20, 1998, to use variables instead of
;;                constants for indent depths.
;;      Modified, Thomas Lofgren Jun 9, 1998, to support font-locking.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst demo-emacs-sr-bug nil
  "*Set to t to demo the match-beginning bug in emacs.")

(defvar sr-paren-indent 1
  "*extra indent added in sr mode for every level
   of paren nesting.")

(defvar sr-block-indent 3
  "*indent for inner part of most sr constructs that
   contains other statements")

(defvar sr-guard-indent 5
  "*indent for inner part of sr guarded commands")

(defvar sr-break-indent  6
  "*Extra indent to indent unfinished statements in sr mode.")

(defvar sr-load-hook nil
  "*This hook is only run when sr-mode is loaded.")

(defvar sr-break-symbols '("(;," "(,")
  "*If the symbols \";\", \",\", or \"(\" appear at the end of a line, the
following line is (usually) considered broken only if the symbol is a 
character in the car of sr-break-symbols.  
\\[cycle-sr-break-symbols] will cycle the list to the left.")

(defvar sr-neg-paren-indent nil)
(defvar sr-neg-block-indent nil)
(defvar sr-neg-guard-indent nil)

(defun sr-internal-setup ()
  "Called to set dependent data structures from advertized ones."
  (setq sr-neg-paren-indent (- 0 sr-paren-indent))
  (setq sr-neg-block-indent (- 0 sr-block-indent))
  (setq sr-neg-guard-indent (- 0 sr-guard-indent)))

(defvar sr-mode-map nil
  "Keymap used in sr mode.")

(defvar sr-mode-abbrev-table nil
  "Abbrev table in use in sr-mode buffers.")
(define-abbrev-table 'sr-mode-abbrev-table ())

(if sr-mode-map
    ()
  (setq sr-mode-map (make-sparse-keymap))
  (define-key sr-mode-map "\177" 'backward-delete-char-untabify)
  (define-key sr-mode-map "\t" 'sr-indent-line)
  (define-key sr-mode-map "\e\C-\\" 'sr-indent-region)
  (define-key sr-mode-map "\C-c\C-i" 'sr-indent-buffer)
  (define-key sr-mode-map "\C-j" 'sr-indent-newline-and-indent)
  (define-key sr-mode-map "\C-c;" 'cycle-sr-break-symbols)
)


(defvar sr-mode-syntax-table nil
  "Syntax table used in sr-mode buffers.")

(if sr-mode-syntax-table
    ()
  (setq sr-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  sr-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" sr-mode-syntax-table)
  (modify-syntax-entry ?# "<" sr-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sr-mode-syntax-table)
  (modify-syntax-entry ?+ "." sr-mode-syntax-table)
  (modify-syntax-entry ?- "." sr-mode-syntax-table)
  (modify-syntax-entry ?= "." sr-mode-syntax-table)
  (modify-syntax-entry ?* "." sr-mode-syntax-table)
  (modify-syntax-entry ?/ "." sr-mode-syntax-table)
  (modify-syntax-entry ?% "." sr-mode-syntax-table)
  (modify-syntax-entry ?< "." sr-mode-syntax-table)
  (modify-syntax-entry ?> "." sr-mode-syntax-table)
  (modify-syntax-entry ?& "." sr-mode-syntax-table)
  (modify-syntax-entry ?| "." sr-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" sr-mode-syntax-table)
  (modify-syntax-entry ?\' "\'" sr-mode-syntax-table)
  ; I wish Gnu could know that " matched " and ' matched '.
)

(defconst sr-obarray (make-vector 127 nil)
  "A obarray for the sr symbols.
Not using the regular obarray prevents interference from anyone
else trying the same trick.")

(defvar sr-symbol-RE nil
  "A pattern to recognize symbols in sr.")

(defun setup-sr-symbols (symbol &optional beginner ender pre-indent 
				post-indent)
  "Sets SYMBOL (as string) to have BEGINNER, ENDER, PRE-INDENT,
   and POST-INDENT properties.  See source code for big comment."

; Many symbols are interned under their actual character representation,
; even though it is not normal lisp syntax, using the intern function.
; These symbols then have property lists to help sr-mode parse and
; indent. If the beginner property is 'absolute, the symbol will be indented
; with respect to the left margin instead of the previous line.  
; If beginner is 'always.  This symbol cannot begin a statement so always 
; add extra indent if it does.  If beginner is 'never or 'absolute, do not 
; add extra indent.  If the ender property is 'always, a line ending in this
; symbol cannot be the end of a statement, so always add extra indent.  If
; the ender property is 'never, a statement ending in this symbol is always
; complete, so never add extra indent to the next statement.  When the ender
; is 'special, it is equivalent to 'always if the symbol is a character in
; the car of sr-break-symbols otherwise it is equivalent to nil.
; The pre-indent and post-indent will be evaluated when indenting is done, so 
; 'sr-guard-indent we become the value of sr-guard-indent.  Those names
; are so long that an abbreviation is used g=> guard, b=>block, p=>paren, 
; these are prefixed with + or - in an obvious way.
; Conceptually the indent algorithm keeps a running indent, adding each
; symbol's pre-indent property, if it the first in a line, indenting to that
; distance, then adding post-indent.
  (let ((sym (intern symbol sr-obarray))
	(tr-alist '((+b sr-block-indent)
		    (-b sr-neg-block-indent)
		    (+g sr-guard-indent)
		    (-g sr-neg-guard-indent)
		    (+p sr-paren-indent)
		    (-p sr-neg-paren-indent))))
    (if beginner (put sym 'beginner beginner))
    (if ender (put sym 'ender ender))
    (if (and (> (length symbol) 1)
	     (not (eq (char-syntax (string-to-char symbol))  ?w)))
	(if (null sr-symbol-RE)
	    (setq sr-symbol-RE (regexp-quote symbol))
	  (setq sr-symbol-RE (concat sr-symbol-RE "\\|" (regexp-quote symbol)))))
    (if post-indent (put sym 'post-indent
			 (or (car (cdr (assq post-indent tr-alist))) post-indent)))
    (if pre-indent (put sym 'pre-indent
			(or (car (cdr (assq pre-indent tr-alist))) pre-indent)))))

(setq sr-symbol-RE nil)

(setup-sr-symbols "global"   'absolute 'never   0    '+b)  ; should be followed by ident
(setup-sr-symbols "end"      'never    nil      '-b  0)    ; may be followed by ident
(setup-sr-symbols "resource" 'absolute 'always  0    '+b)  ; should be followed by ident
(setup-sr-symbols "body"     'absolute 'always  0    '+b)  ; should be followed by ident
(setup-sr-symbols "separate" 'absolute 'never   0      0) 
(setup-sr-symbols "proc"     'absolute 'always  '+b  '+b)  ; should be followed by ident

;; Following line added by Balint Nogradi (Jan 23, 1997) based on "proc" line above
(setup-sr-symbols "procedure" 'absolute 'always  '+b  '+b)  ; should be followed by ident
(setup-sr-symbols "process"  'absolute 'always  '+b  '+b)  ; should be followed by ident
(setup-sr-symbols "import"   'never    'always  0    0)    ; should be followed list
(setup-sr-symbols "extend"   'absolute 'always  '+b  0)
(setup-sr-symbols "initial"  'absolute 'never   '+b  '+b)
(setup-sr-symbols "final"    'absolute 'never   '+b  '+b)
(setup-sr-symbols "if"       'never    'always  0    '+g)  ; should be followed by expression
(setup-sr-symbols "->"       'always   'never   0    0)    ; should be followd by block, which we should not indent
(setup-sr-symbols "st"       'always   'always)
(setup-sr-symbols "[]"       'never    'always  '-g  '+g)  ; should be followed by expression
(setup-sr-symbols "else"     'always   'always)
(setup-sr-symbols "fi"       'never    'never   '-g  0)    ; always ends statement
(setup-sr-symbols "do"       'never    'always  0    '+g)  ; should be followed by expression
(setup-sr-symbols "od"       'never    'never   '-g  0)    ; always ends statement
(setup-sr-symbols "fa"       'never    'always  0    '+b)  ; should be followed by quantifiers
(setup-sr-symbols "af"       'never    'never   '-b  0)    ; always ends statement
(setup-sr-symbols "co"       'never    nil      0    '+b)  ; can be followed by interaction quantifier
(setup-sr-symbols "//"       'never    'never   '-b  '+b)
(setup-sr-symbols "oc"       'never    'never   '-b  0)    ; always ends statement
(setup-sr-symbols "in"       'never    'always  0    '+g)  ; must match []
(setup-sr-symbols "ni"       'never    'never   '-g  0)
(setup-sr-symbols "("        'always   'special 0    '+p)  
(setup-sr-symbols ")"        'always   nil      '-p  0)
(setup-sr-symbols "["        'always   'always  0    '+p)
(setup-sr-symbols "]"        'always   nil      '-p  0)
(setup-sr-symbols "{"        'always   'always  0    '+p)
(setup-sr-symbols "}"        'always   nil      '-p  0)
(setup-sr-symbols "stop"     'never    nil)           ; can be followed by expression
						      ; This can not work exactly right, since
						      ; we can not tell difference between beg. of expr. and
						      ; beg. of stmt.
(setup-sr-symbols "skip"     'never    'never)
(setup-sr-symbols "exit"     'never    'never)
(setup-sr-symbols "next"     'never    'never)
(setup-sr-symbols "++"       'always   'never)
(setup-sr-symbols "--"       'always   'never)
(setup-sr-symbols ":=:"      'always   'always) ; put before := to avoid a bug in regexp searching
(setup-sr-symbols ":="       'always   'always)
(setup-sr-symbols ">"        'always   'always)
(setup-sr-symbols ">="       'always   'always)
(setup-sr-symbols "<"        'always   'always)
(setup-sr-symbols "<="       'always   'always)
(setup-sr-symbols "="        'always   'always)
(setup-sr-symbols "!="       'always   'always)
(setup-sr-symbols "~="       'always   'always)
(setup-sr-symbols "."        'always   'always) ; may change when floats are added
(setup-sr-symbols ":"        'always   'always)
(setup-sr-symbols ";"        'always   'special)
(setup-sr-symbols "||"       'always   'always)
(setup-sr-symbols "<<"       'always   'always)
(setup-sr-symbols ">>"       'always   'always)
(setup-sr-symbols "@"        'always   'always)
(setup-sr-symbols "+"        'always   'always)
(setup-sr-symbols "-"        'always   'always)
(setup-sr-symbols "~"        'always   'always)
(setup-sr-symbols "?"        'always   'always)
(setup-sr-symbols "*"        'always   'always)
(setup-sr-symbols "/"        'always   'always)
(setup-sr-symbols "%"        'always   'always)
(setup-sr-symbols "not"      'always   'always)
(setup-sr-symbols ","        'always   'special)
(setup-sr-symbols "and"      'always   'always)
(setup-sr-symbols "&"        'always   'always)
(setup-sr-symbols "or"       'always   'always)
(setup-sr-symbols "|"        'always   'always)
(setup-sr-symbols "xor"      'always   'always)
(setup-sr-symbols "on"       'always   'always)
(setup-sr-symbols "destroy"  'never    'always)
(setup-sr-symbols "create"   'never    'always)
(setup-sr-symbols "receive"  'never    'always)
(setup-sr-symbols "send"     'never    'always)
(setup-sr-symbols "reply"    'never    'never)
(setup-sr-symbols "return"   'never    'never)
(setup-sr-symbols "ref"      'always   'always)
(setup-sr-symbols "res"      'always   'always)
(setup-sr-symbols "const"    'never    'always)       ; should be followed by ident
;(setup-sr-symbols "bool"     nil       nil)      
;(setup-sr-symbols "char"     nil       nil)
;(setup-sr-symbols "int"      nil       nil)
(setup-sr-symbols "string"   nil       'always)       ; must be followed by (len)
(setup-sr-symbols "enum"     'always   'always)
(setup-sr-symbols "type"     'never    'always)       ; should be followed by ident
(setup-sr-symbols "var"      'nil      'always)	      ; var is wierd, since it is both 
					              ; used to declare variables and
						      ; as a parameter mode
(setup-sr-symbols "optype"   'never    'always)       ; should be followed by ident
(setup-sr-symbols "op"       'absolute 'always '+b  0); should be followed by ident
(setup-sr-symbols "external" 'absolute 'always '+b  0); just like op
(setup-sr-symbols "returns"  'always   'always)
(setup-sr-symbols "\"\""     'always   nil)           ; Does not appear in real sr.
(setup-sr-symbols "by"       'always   'always)
(setup-sr-symbols "call"     'never    'always)
(setup-sr-symbols "cap"      'always   'always)
(setup-sr-symbols "to"       'always   'always)
(setup-sr-symbols "downto"   'always   'always)


(setq sr-symbol-RE (concat sr-symbol-RE "\\|\\sw+\\|\\S "))
;;; The single character thing must go on the end to avoid a feature
;;; in the RE matcher that causes the first pattern appearing
;;; *in the RE* to be matched.

(defconst sr-symbol-RE-bol (concat "^\\(" sr-symbol-RE "\\)")
  "An RE that matches words, sr-symbols, plus single characters when
   against the left margin.")

(defconst sr-symbol-RE-eol (concat "\\(" sr-symbol-RE "\\)$")
  "An RE that matches words, sr-symbols, plus single characters when
   against the right  margin.")



(defun sr-mode ()
  "Estabishes a mode for editing SR programs.

Use \\[sr-indent-line] to indent a single line.  One or two
    C-u prefixes add or subtract sr-break-indent (when appropriate).
Use \\[newline-and-indent] to go to a new line and position the
    cursor at about the right place.  (LFD is Control-J.) Prefix
    args apply as with \\[sr-indent-line]. 
Use \\[sr-indent-region] to indent region defined by point and mark.
Use \\[sr-indent-buffer] to indent entire buffer.
Use \\[cycle-sr-break-symbols] to cycle between various options
regarding the symbols that, when appearing at the end of a line,
cause the next line to be broken.  In the default configuration, 
this toggles between a terminating semicolon causing or not causing
the following line to be tucked under.

Variables controlling indentation style:

sr-block-indent controls the indentation after constructs such as 
  \"resource\", \"proc\", \"initial\", \"fa\", etc.

sr-guard-indent controls the indentation of commands that involve 
  guards, such as \"if\", \"do\", and \"in\".

sr-break-indent controls the amount by which statement 
  continuations are tucked under the beginning of the statement.

sr-paren-ident is an extra amount that statement continuations get 
tucked under for each level of paren nesting.

sr-break-symbols controls whether \";\", \",\", and \"(\" cause
the following statement to be tucked under. 

Change these with \\[set-variable].

Good possibilities for these variables and their corresponding
coding disciplines are:
1.  (Default) sr-break-symbols = '(\"(;,\" \"(,\")).  You must use semicolons
    to separate lines of parameter specificiations and record fields.
    Avoid semicolons at ends of statements unless you change mode with
    \\[cycle-sr-break-symbols].
2.  sr-break-symbols =  '(\"\" \",\") and sr-paren-indent = sr-break-indent.  
    Semicolons may be used in any way desired.  When not inside parentheses,
    avoid ending lines with commas.  (This affects import and extend clauses.)
    \\[cycle-sr-break-symbols] can change the mode to allow these commas.
    Sometimes produces funny results on long arithmetic expressions.

In all cases, no (uncommented) keywords (e.g. initial, final) after \"end\".

Entering sr-mode calls the value of sr-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (use-local-map sr-mode-map)
  (setq mode-name "sr-mode")
  (setq major-mode 'sr-mode)
  (setq local-abbrev-table sr-mode-abbrev-table)
  (set-syntax-table sr-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sr-indent-line)
  (make-local-variable 'indent-region-function)
  (setq  indent-region-function 'sr-indent-region)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  ;; Font-lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pascal-font-lock-keywords nil t))
  (run-hooks 'sr-mode-hook)
)


(defun parse-sr (&optional begin-point end-point)
  "Parses an sr program. Output a list of strings that are genuine sr
   and all appear on one line or numbers that are buffer postions of
   newline characters.  Optional first arg specifies beginning of parse,
   which defaults to first global, resource, body, or proc that begins
   a line (intervening whitespace allowed).  Optional end-point bounds the 
   end of the parse.  It defaults to point.  Don't call this."
  (save-excursion
    (let ((cdot (point))
	  symstart
	  symend
	  symlist)
      (if (null end-point)
	  (progn
	    (end-of-line)
	    (setq end-point (point))
	    (goto-char cdot)))
      (if (null begin-point)
	  (progn
	    (re-search-backward
	       "^\\s *\\(global\\>\\|resource\\>\\|body\\>\\|proc\\>\\)" nil 'move)
	    (setq begin-point (point))))
      (goto-char begin-point)
      (if (bolp) (setq symlist (cons (point) symlist)))
      (skip-chars-forward " \t")
      (while (< (point) end-point)
	(setq symstart (point))
	(cond
	 ((eq (following-char) ?\n)
	      (forward-char)
	      (setq symlist (cons (point) symlist)))
	 ((eq (following-char) ?\")
	  (if (re-search-forward "\"[^\"]*\\(\\\\.\\[^\"]*\\)*\""
				 end-point 'move)
	      (progn
		(setq symend (point))
		(setq symlist (cons (buffer-substring symstart symend)
				   symlist)))))
	 ((eq (following-char) ?\')
	  (if (re-search-forward "\'[^\']*\\(\\\\.\\[^\']*\\)*\'"
				 end-point 'move)
	      (progn
		(setq symend (point))
		(setq symlist (cons (buffer-substring symstart symend)
				   symlist)))))
	 ((eq (following-char) ?#)
	  (end-of-line))
	 ((looking-at "/\\*")
	  (skip-over-sr-comment))
	 (t
	  (if (re-search-forward "#\\|/\\*\\|\n\\|\"\\|\'" end-point 'move)
	      (goto-char (match-beginning 0)))
	  (skip-chars-backward " \t")
	  (setq symend (point))
	  (setq symlist (cons (buffer-substring symstart symend)
			      symlist))))  ; end of cond
	(skip-chars-forward " \t")  
	)
      symlist)))
	
(defun skip-over-sr-comment(&optional search-bound)
  "Positioned before /* moves point to just after matching */.
   An optional arg bounds the search."
  (interactive)
  (if (and (re-search-forward "/\\*" search-bound t)
	   (re-search-forward "\\*/\\|\\(/\\*\\)" search-bound t))
      (let ((mb (nth 2 (match-data)))) ; work around an unfeature of 18.44.2
				       ; wherin errors occur on
				       ; (match-beginning 1) if the second
				       ; pattern did not match.
	(if mb
	    (progn (goto-char mb)
		   (skip-over-sr-comment)
		   (re-search-forward "\\*/" search-bound t))
	  t))))
	       

(defun sr-parse-into-lines (parse-list)
  "Given a parse list such as is returned by parse-sr, returns
   an array each element of which is a list (number string),
   where number is the character in the buffer of the beginning of the line
   and string contains the line, less comments, in left to right order.  Quotes
   have been replaced by just \"\".  Don't call this."
  (let ((wkpt parse-list)
	list-of-lines
	current-line
	)
    (while wkpt
      (cond ((stringp (car wkpt))
	     (setq current-line (concat
				 (if (memq (elt (car wkpt) 0) '(?\" ?\'))
				     "\"\""
				   (car wkpt))
				 current-line)))
	    ((and (numberp (car wkpt)) current-line)
	     (setq list-of-lines (cons (list (car wkpt) current-line) list-of-lines))
	     (setq current-line nil)))
      (setq wkpt (cdr wkpt)))
    list-of-lines))
      
	
(defun sr-first-sym (string)
  "Given a string, finds the first sr symbol in it and returns it as a string"
  (and string
       (string-match sr-symbol-RE-bol string)
       (substring string 0 (match-end 1))))
;;; Avoid the match-beginning bug by hard-coding a 0.

(defun sr-last-sym (string)
  "Given a string, finds the last sr symbol in it and returns it as a string"
  (and string
       (string-match sr-symbol-RE-eol string)
       (substring string (match-beginning 1) (match-end 1))))
;;; The match-beginning bug lurks here!!!  The bug is that sometimes
;;; (match-beginning mumble) returns the wrong value.  The appears to
;;; happen when the match begins with the first character, in which case
;;; (match-beginning mumble) will sometimes return 1 instead of 0.
    
(defun get-sr-indent (symbol property)
  "About like (get symbol property), except that the result is evaled, and 
   nil is converted to 0.  Intended for use in context where the symbol
   pg has the value sr-indent-guard, etc."
  (or (eval (get symbol property)) 0))

	  
(defun sr-indent-line (&optional fudge-break arg)
  "Indents a line of sr.  A optional prefix arg adds or subtracts 
   sr-break-indent.  (One C-u adds, two subtract.)
   In a program, optional FUDGE-BREAK should be nil or numeric.
   When numeric, it adds that many 
   sr-break-indents to the indent value.
   An optional second ARG non-nill causes the previous line to be indented 
   as well.  In this case, FUDGE-BREAK applies to the previous line."
  (interactive "*P")
  (sr-internal-setup)
  (let (bdot
	parse-list
	compute-result
	indent-val
	(fudge (cond ((null fudge-break) 0)
		     ((equal fudge-break '(4)) sr-break-indent)
		     ((equal fudge-break '(16)) (- 0 sr-break-indent))
		     ((equal fudge-break '-)    (- 0 sr-break-indent))
		     (t (* sr-break-indent (prefix-numeric-value fudge-break))))))
    (save-excursion
      (forward-line 0)
      (setq bdot (point))
      (setq parse-list (nreverse (sr-parse-into-lines (parse-sr))))
      (if (null parse-list)
	  ()
	;; If the dot is in a line with program characters (blank or all
	;; comments) the first thing in the parse-list would be the previous
	;; line.  So we stick on a dummy line.  Yes, it is a hack.
	(if (not (eq (car (car parse-list)) bdot))
	    (setq parse-list (cons (list bdot "DUMMY LINE") parse-list)))
	;; A hack to make sr-indent-newline-and-indent work efficiently.
	;; Clearly there must be a cleaner way to do this.
	(if arg
	    (progn
	      (setq compute-result (sr-compute-indent (cdr parse-list)))
	      (setq indent-val (if (eq (car (cdr compute-result)) 'absolute)
				   (car compute-result)
				 (+ fudge
				    (car compute-result)
				    (save-excursion
				      (goto-char (car (nth 2 parse-list)))
				      (current-indentation)))))
	      (save-excursion
		(goto-char (car (nth 1 parse-list)))
		(delete-horizontal-space)
		(indent-to indent-val)
		(forward-line 0)
		(setcar (nth 1 parse-list) (point))))  ;; repair parse list
	  (setq compute-result (sr-compute-indent parse-list))
	  (setq indent-val (if (eq (car (cdr compute-result)) 'absolute)
			       (car compute-result)
			     (+ (if arg 0 fudge)
				(car compute-result)
				(save-excursion
				  (goto-char (car (nth 1 parse-list)))
				  (current-indentation))))))
	;; point is at the beginning of the line to be indented
	(delete-horizontal-space)
	(indent-to indent-val))) ; end of save-excursion
    (if (bolp) (skip-chars-forward " \t"))))

(defun sr-indent-region (beg-reg end-reg)
  "Indents a region of SR code.  Note the indentation is relative
   to the first thing in the region.  Currently there is brain damage
   when the first thing is a tail of a broken statement."
  (interactive "*rP")
  (sr-internal-setup)
  (if (interactive-p)
      (message "Indenting..."))
  (let (parse-list
	traverse-list
	wkpt
	current-line
	running-indent)
    (save-excursion
      (goto-char beg-reg)
      (forward-line 0)
      (setq beg-reg (point))
      (goto-char end-reg)
      (end-of-line)
      (setq end-reg (point))
      (setq parse-list (nreverse (sr-parse-into-lines (parse-sr beg-reg end-reg))))
      (setq wkpt parse-list)
      (while wkpt
	(sr-compute-indent wkpt)	; do most of the work
	(setq wkpt (cdr wkpt)))
      (setq parse-list (nreverse parse-list))
      (setq wkpt parse-list)
      (while wkpt			; traverse lines in forward direction
					; converting all indents to absolute
	(setq current-line (car wkpt))
	(if (eq (nth 3 current-line) 'absolute) 
	    (setq running-indent (nth 2 current-line)) ; absolute indent
	  (setq running-indent (+ (nth 2 current-line) ; relative indent
				  (if running-indent ; nil on first line when it is not absolute
				      running-indent
				    (goto-char (car current-line))
				    (current-indentation))))
	  (setcdr (cdr current-line) (list running-indent 'absolute)))
	(setq wkpt (cdr wkpt)))		; end of while
      (setq parse-list (nreverse parse-list))
      (setq wkpt parse-list)
      (while wkpt			; traverse line backward
					; actually doing indentation
	(goto-char (car (car wkpt)))
	(delete-horizontal-space)
	(indent-to (nth 2 (car wkpt)))
	(setq wkpt (cdr wkpt)))))
    (if (interactive-p)
      (message "Indenting...done")))


(defun sr-indent-buffer ()
  "Like sr-indent-region, except does the whole buffer."
  (interactive "*")
  (if (interactive-p)
      (message "Indenting..."))
  (sr-indent-region (point-min) (point-max))
  (if (interactive-p)
      (message "Indenting...done")))

(defun sr-compute-indent (parse-list)
  "Given a PARSE-LIST of the form returned by sr-parse-into-lines,
   which is a list of line-lists (line-number text), each representing a line,
   sr-compute-indent appends data to the line-list at the head of
   parse-list so it has the form 
   (line-number text indent-distance relative-to), where relative-to
   is 'absolute or nil.  Nil means relative to the previous line, 
   i.e. the one represented by (cdr parse-list).  Returns a list
   (indent-distance relative-to)."
  (let*  ((lead-symbol-string (sr-first-sym (car (cdr (car parse-list)))))
	  (lead-symbol  (intern-soft lead-symbol-string sr-obarray))
	  (running-indent 0)
	  prevline
	  (sympos 0)
	  current-symbol-string
	  current-symbol
	  match-b
	  result-list)
    (cond
     ((eq (get lead-symbol 'beginner) 'absolute)
      (setq result-list (list (get-sr-indent lead-symbol 'pre-indent) 'absolute)))
     ((null (cdr parse-list))
      (setq result-list (list 0 nil)))
     (t (setq prevline (car (cdr (nth 1 parse-list))))
	;;(message "initial indent %d" running-indent)
	;;(sit-for 2)
	(if (sr-broken-statement (cdr parse-list))
	    (setq running-indent
		  (- running-indent sr-break-indent)))
	;;(message "after broken %d" running-indent)
	;;(sit-for 2)
	(setq running-indent
	      (- running-indent
		 (get-sr-indent (intern-soft (sr-first-sym prevline) sr-obarray) 'pre-indent)))
	;;(message "deduct preindent of %s %d" (sr-first-sym prevline) running-indent)
	;;(sit-for 2)
	(setq linelen (length prevline))
	(while (< sympos linelen)
	  (setq match-b (string-match sr-symbol-RE prevline sympos))
	  ;;(setq match-b (match-beginning 0))   ; bug in emacs lurks here
	  (if (and demo-emacs-sr-bug
		   (/= match-b (match-beginning 0)))
	      (debug "match-beginning problem" match-b (match-beginning 0)))
	  (setq sympos (match-end 0))
	  (setq current-symbol-string
		(substring prevline match-b sympos))
	  (setq current-symbol (intern-soft current-symbol-string sr-obarray))
	  (setq running-indent (+ running-indent
				  (get-sr-indent  current-symbol 'pre-indent)
				  (get-sr-indent  current-symbol 'post-indent)))
	  ;;(message "%s %d" current-symbol-string running-indent)
	  ;;(sit-for 2)
	  )  ; end of while
	(setq running-indent (+ running-indent
				(get-sr-indent lead-symbol 'pre-indent)))
	;;(message "%s %d" lead-symbol-string running-indent)
	;;(sit-for 2)
	(if (sr-broken-statement parse-list)
	    (setq running-indent
		  (+ running-indent sr-break-indent)))
	;;(message "add broken statement %d" running-indent)
	;;(sit-for 2)
	(setq result-list  (list running-indent nil))))
    (setcdr (cdr (car parse-list)) result-list)))

(defun sr-broken-statement (parse-list)
  "Returns non-nil if the line at the head of PARSE-LIST is the continuation
   of a previous broken statement."
  (let* ((head-symbol-string (sr-first-sym (car (cdr (nth 0 parse-list)))))
	 (head-symbol (and head-symbol-string
			   (intern-soft head-symbol-string sr-obarray)))
	 (head-beginner-prop (get head-symbol 'beginner))
	 tail-symbol-string
	 tail-symbol
	 tail-ender-prop)
    (if head-beginner-prop
	(eq head-beginner-prop 'always)
      (setq tail-symbol-string (sr-last-sym (car (cdr (nth 1 parse-list)))))
      (setq tail-symbol (and tail-symbol-string
			     (intern-soft tail-symbol-string sr-obarray)))
      (setq tail-ender-prop (get tail-symbol 'ender))
      (cond ((eq tail-ender-prop 'always) t)
	    ((eq tail-ender-prop 'special)
	     (string-match tail-symbol-string (car sr-break-symbols)))
	    ;;(t nil)  ; this happens automatically
	    ))))
    

(defun sr-indent-newline-and-indent (&optional fudge-break)
  "Does what it says, but more efficiently.  An optional arg works as 
   does FUDGE-BREAK in sr-indent-line."
  (interactive "*P")
  (newline)
  (sr-indent-line fudge-break t))

(defun sr-cycle-break-symbols-inner (symstr sofar)
  (if (string-match symstr (car sr-break-symbols))
      sofar
    (concat sofar symstr)))
  

(defun cycle-sr-break-symbols ()
  "Cycles the list sr-break-symbols to the left.  Typically this is to
   toggle whether or not a trailing semicolon causes the following line
   to be tucked under."
  (interactive)
  (setq sr-break-symbols (nconc (cdr sr-break-symbols)
				(list (car sr-break-symbols))))
  (if (interactive-p)
      (message "Breaking: \"%s\"   Non-breaking: \"%s\""
	       (car sr-break-symbols)
	       (sr-cycle-break-symbols-inner
		";" (sr-cycle-break-symbols-inner
		     "," (sr-cycle-break-symbols-inner
			  "(" ""))))))

(add-hook 'sr-load-hook t)

(run-hooks 'sr-load-hook)

;; ------- Font-lock support -------

(require 'font-lock)

(defvar sr-font-lock-auto-on t
  "*If non-nil, turn on font-lock unconditionally for every SR buffer.")
(defvar sr-font-lock-standard-keywords nil
  "*Regexp that matches all standard sr keywords")

(defvar sr-font-lock-keywords nil
  "*Default font lock keywords for the SR language.")
(defvar sr-font-lock-keywords-1 nil
  "*Font lock keywords for the SR language.")  
(defvar sr-font-lock-keywords-2 nil
  "*Additional font lock support for variables.")
(defvar sr-font-lock-keywords-3 nil
  "*Currently not used.")

(defvar sr-font-lock-primitive-types
  "\\(bool\\|char\\|int\\|real\\|string\\)"
  "Regexp matching primitive SR types.")

(defvar sr-font-lock-additional-types
  "\\(any\\|cap\\|file\\|ptr\\|vm\\)"
  "*Regexp matching non-primitive types.")

(defvar sr-font-lock-user-defined-types
  "\\(enum\\|rec\\|type\\|union\\)"
  "*Regexp matching patterns that let the user define types.")

(defvar sr-font-lock-builtin-constants
  "\\(false\\|n\\(oop\\|ull\\)\\|std\\(err\\|in\\|out\\)\\|true\\)"
  "*Regexp matching special constants in SR.")

(defvar sr-font-lock-identifier-regexp
  nil "*Regexp matching keywords in SR.")

(defvar sr-font-lock-all nil
  "*Font-lock matchers for SR.")

;; Default font lock support 
(setq sr-font-lock-standard-keywords
  ;; (make-regexp '(
      ;; "P" "V" "af" "and" "begin" "body" "by" "call" "const" "create" "co" 
      ;; "destroy" "do" "downto" 
      ;; "else" "end" "exit" "extend"
      ;; "fa" "fi" "final" "forward" "global" "high" 
      ;; "if" "import" "in" "initial" "low" "mod"
      ;; "new" "next" "ni" "not" "oc" "od" "on" "or" 
      ;; "reply" "receive" "ref" "res" "return" "returns" 
      ;; "sem" "send" "separate" "stop" "skip" "st" "to" "val" "var" "xor" ))
"\\([PV]\\|a\\(f\\|nd\\)\\|b\\(egin\\|ody\\|y\\)\\|c\\(all\\|o\\(\\|nst\\)\\|reate\\)\\|d\\(estroy\\|o\\(\\|wnto\\)\\)\\|e\\(lse\\|nd\\|x\\(it\\|tend\\)\\)\\|f\\([ai]\\|inal\\|orward\\)\\|global\\|high\\|i\\([fn]\\|mport\\|nitial\\)\\|low\\|mod\\|n\\(e\\(w\\|xt\\)\\|i\\|ot\\)\\|o[cdnr]\\|re\\([fs]\\|ceive\\|ply\\|turns?\\)\\|s\\(e\\(m\\|nd\\|parate\\)\\|kip\\|t\\(\\|op\\)\\)\\|to\\|va[lr]\\|xor \\)")

(let ((letter "a-zA-Z_")
      (digit  "0-9"))
  (setq sr-font-lock-identifier-regexp
	(concat "\\<\\([" letter "][" letter digit "]*\\)\\>")))

(setq sr-font-lock-keywords-1
      (list
       ;; Procs and variables
       (list
	(concat
	 "\\<\\(body\\|external\\|op\\(\\|type\\)\\|proc\\(\\|e\\(dure"
	 "\\|ss\\)\\)\\|resource\\)\\>"
	 "[ \t]*\\(\\sw+\\)?[ \t]*\\((\\sw+)\\)?)?")
	'(1 font-lock-keyword-face) '(5 font-lock-function-name-face nil t)
	'(sr-font-lock-match-variable-and-skip-to-next
	  (goto-char (or (match-beginning 2) (match-end 0))) nil
	  (1 font-lock-variable-name-face)))
       
       ;; Primitive types
       (list 
	(concat "\\<" sr-font-lock-primitive-types "\\>")
	'(0 font-lock-type-face))
       
       ;; Additional types
       (list
	(concat "\\<" sr-font-lock-additional-types "\\>")
	'(0 font-lock-type-face))

       ;; User-defined types
       (list
	(concat "\\<" sr-font-lock-user-defined-types "\\>")
	'(0 font-lock-type-face))

       ;; Special constants
       (list
	(concat "\\<" sr-font-lock-builtin-constants "\\>")
	'(0 font-lock-reference-face))

       ;; Standard keywords
       (list
	(concat "\\<" sr-font-lock-standard-keywords "\\>")
	'(1 font-lock-keyword-face))

       ;; Variables
       (list
	(concat "\\(var\\|const\\|sem\\)\\>[ \t]*\\(\\sw+\\)?")
	'(sr-font-lock-match-variable-and-skip-to-next
	  ;; Start with point after var keyword
	  (goto-char (or (match-beginning 2) (match-end 1)))
	  (goto-char (match-end 1))
	  ;; Fontify as variable name
	  (1 font-lock-variable-name-face)))))


(setq sr-font-lock-all
  (append
   (list '(sr-comments))
   sr-font-lock-keywords-1
   ;;sr-font-lock-keywords-2
   ))


(defvar sr-font-cache '((0 . normal))
  "List of (POSITION . STATE) pairs for an SR buffer.
The STATE is either `normal' or `comment'.  The POSITION is
immediately after the token that caused the state change.")

(defun sr-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(sr-font-lock-all
	  nil nil ((?_ . "w")) nil
	  (font-lock-comment-start-regexp . "#"))))

;; sr-comments
;; Pretty much completely ripped off from Fritz Knabe's sml-font.el

(defun sr-comments (limit)
  "Fontify SR comments p to LIMIT. Handles nested c-style comments."
  (let ((beg (point))
	last class)
    (while (< beg limit)
      (while (and sr-font-cache
		  (> (car (car sr-font-cache)) beg))
	(setq sr-font-cache (cdr sr-font-cache)))
      (setq last (car (car sr-font-cache)))
      (setq class (cdr (car sr-font-cache)))
      (goto-char last)
      (cond
       ((eq class 'normal)
	(cond
	 (( not (re-search-forward "\\(/\\*\\)" limit t))
	  (goto-char limit))
	 ((match-beginning 1)
	  (setq sr-font-cache (cons (cons (point) 'comment) sr-font-cache)))))
       ((eq class 'comment)
	(cond
	 ((let ((nest 1))
	    (while (and (> nest 0)
			(re-search-forward "\\(/\\*\\)\\|\\(\\*/\\)" limit t))
	      (cond
	       ((match-beginning 1) (setq nest (+ nest 1)))
	       ((match-beginning 2) (setq nest (- nest 1)))))
	    (> nest 0))
	  (goto-char limit))
	 (t
	  (setq sr-font-cache (cons (cons (point) 'normal) sr-font-cache))))
	(put-text-property (- last 2) (point) 'face 'font-lock-comment-face)))
      (setq beg (point)))))


(defun sr-font-lock-match-variable-and-skip-to-next (limit)  
  "Match, and skip over, variable declarations/definitions after point. 
Items are expected to be separated by a comma."
  ;; Skip whitespaces
  (if (looking-at "\\s *(?\\s *\\(\\[\\s *\\]\\s *\\)*")
      (goto-char (match-end 0)))
  (and
   (looking-at sr-font-lock-identifier-regexp)
   (save-match-data
     (condition-case nil
	 (save-restriction
	   (narrow-to-region (point-min) limit)
	   (goto-char (match-end 0))
	   ;; Note: Both `scan-sexps' and the second goto-char can
	   ;; generate an error which is caught by the
	   ;; `condition-case' expression.
	   (while (not (looking-at "\\s *\\(\\(,\\|;\\)\\|$\\)"))
	     (goto-char (or (scan-sexps (point) 1) (point-max))))
	   (goto-char (match-end 2)))	; non-nil
       (error t)))))

(add-hook 'sr-mode-hook 'sr-font-lock-setup)

;;(provide 'sr-font-lock)
