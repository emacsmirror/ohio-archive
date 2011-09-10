;;; tplscan.el -- Scanner for template package
;;; Copyright (C) 1987 Mark A. Ardis.

(require 'tplvars)

(provide 'tplscan)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; All global variables are in "tplvars".

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-pattern (pn pv)
  "Constructor for lexical patterns."
  (list (list 'name pn) (list 'value pv))
  ) ; defun tpl-make-pattern

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-pattern-name (p)
  "Selector for name field of lexical patterns."
  (car (cdr (assq 'name p)))
  ) ; defun tpl-pattern-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-pattern-value (p)
  "Selector for value field of lexical patterns."
  (car (cdr (assq 'value p)))
  ) ; defun tpl-pattern-value

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-token (tt tn tv)
  "Constructor for tokens."
  (list (list 'type tt) (list 'name tn) (list 'value tv))
  ) ; defun tpl-make-token

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-token-type (token)
  "Selector for type field of tokens."
  (car (cdr (assq 'type token)))
  ) ; defun tpl-token-type

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-token-name (token)
  "Selector for name field of tokens."
  (car (cdr (assq 'name token)))
  ) ; defun tpl-token-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-token-value (token)
  "Selector for value field of tokens."
  (car (cdr (assq 'value token)))
  ) ; defun tpl-token-value

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-line (indent-level token-list)
  "Constructor for lines."
  (list (list 'indent indent-level) (list 'tokens token-list))
  ) ; defun tpl-make-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-line-indent (line)
  "Selector for indentation field of lines."
  (car (cdr (assq 'indent line)))
  ) ; defun tpl-line-indent

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-line-tokens (line)
  "Selector for token-list field of lines."
  (car (cdr (assq 'tokens line)))
  ) ; defun tpl-line-tokens

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-region (start stop pattern-list)
  "Scan the text between START and STOP using PATTERN-LIST for tokens.
   Return an indented line-list of tokens."
					; Local Variables
  (let (start-col last-col this-col indent-level last-indent
		  line line-list more)
					; Body
    (goto-char start)
    (setq start-col (current-column))
    (setq line-list nil)
    (save-restriction
      (narrow-to-region start stop)
      (and (boundp 'template-scan-hook)
	   template-scan-hook
	   (funcall template-scan-hook))
      (if (eobp)
	  (setq more nil)
	(setq more t)
	) ; if (eobp)
      (while more
					; Scan a line
	(back-to-indentation)
	(setq line (tpl-scan-line start-col pattern-list))
	(setq line-list (append line-list (list line)))
					; Advance to next line
	(if (not (eobp))
	    (forward-char)
	  (setq more nil)
	  ) ; if (not (eobp))
	) ; while more
      ) ; save-restriction
					; return
    line-list
    ) ; let
  ) ; defun tpl-scan-region

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-line (start-col pattern-list)
  "Scan a line of text, returning an indentation-line of tokens.
   START-COL is the origin column for a region.
   PATTERN-LIST is the list of tokens to scan for."
					; Local Variables
  (let (this-col indent-level line)
					; Body
    (if tpl-literal-whitespace
	(progn
	  (beginning-of-line nil)
	  (setq line (tpl-make-line 0 (tpl-scan-token-list pattern-list)))
	  ) ; progn
      ; else
      (progn
	(back-to-indentation)
	(setq this-col (current-column))
	(cond
	 ((>= this-col comment-column)
	  (progn
	    (setq indent-level tpl-comment-level)
	    ) ; progn
	  ) ; comment
	 ((<= this-col start-col)
	  (progn
	    (setq indent-level 0)
	    ) ; progn
	  ) ; too small
	 (t
	  (progn
	    (setq indent-level (- this-col start-col))
	    ) ; progn
	  ) ; t
	 ) ; cond
					; Scan tokens and make into a line
	(setq line (tpl-make-line indent-level
				  (tpl-scan-token-list pattern-list)))
	) ; progn
      ) ; if tpl-literal-whitespace
					; return
    line
    ) ; let
  ) ; defun tpl-scan-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-token (pattern-list)
  "Scan the text at point and return a token.
   PATTERN-LIST is the list of tokens to scan for."
					; Local Variables
  (let (pattern pn pv token found start)
					; Body
    (setq found nil)
    (while (and pattern-list (not found))
      (setq pattern (car pattern-list))
      (setq pattern-list (cdr pattern-list))
      (setq pn (tpl-pattern-name pattern))
      (setq pv (tpl-pattern-value pattern))
      (if (looking-at pv)
	  (setq found t)
	) ; if (looking-at pattern)
      ) ; while (and pattern-list (not found))
    (if (not found)
	(error "Unable to scan text.")
      ) ; if (not found)
    (setq start (point))
    (re-search-forward pv)
    (setq token (tpl-make-token tpl-terminal-type pn
				(buffer-substring start (point))))
    token				; return
    ) ; let
  ) ; defun tpl-scan-token

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-token-list (pattern-list)
  "Scan the current line and return a list of tokens.
   PATTERN-LIST is the list of tokens to scan for."
					; Local Variables
  (let (save-list token token-list)
					; Body
    (setq token-list nil)
    (setq save-list pattern-list)
    (while (not (eolp))
      (setq pattern-list save-list)
      (setq token (tpl-scan-token pattern-list))
      (setq token-list (append token-list (list token)))
      ) ; while (not (eolp))
					; return
    token-list
    ) ; let
  ) ; defun tpl-scan-token-list

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-template ()
  "Scan the template at point and return its tree value."
					; Local Variables
  (let (start template-name template-type token-list tree save-patterns)
					; Body
    (re-search-forward tpl-begin-template-definition)
    (re-search-forward tpl-pattern-whitespace)
    (setq start (point))
    (re-search-forward tpl-pattern-symbol)
    (setq template-name (buffer-substring start (point)))
    (re-search-forward tpl-pattern-whitespace)
    (setq start (point))
    (re-search-forward tpl-pattern-word)
    (setq template-type (buffer-substring start (point)))
    (re-search-forward tpl-begin-template-body)
    (beginning-of-line 2)
    (setq start (point))
    (re-search-forward tpl-end-template-body)
    (end-of-line 0)
    (if (or (equal template-type tpl-lexical-type)
	    (equal template-type tpl-function-type))
	(setq token-list (buffer-substring start (point)))
      ; else
      (if (equal template-type tpl-string-type)
	  (setq token-list (tpl-scan-region start (point) string-patterns))
	; else
	(setq token-list (tpl-scan-region start (point) lex-patterns))
	) ; if (equal template-type tpl-string-type)
      ) ; if (or ...)
    (setq tree (tpl-make-token template-type template-name token-list))
					; return
    tree
    ) ; let
  ) ; defun tpl-scan-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-scan-placeholder ()
  "Scan the placeholder at point and return its tree value."
					; Local Variables
  (let (save start placeholder-type placeholder-name token-type)
					; Body
    (setq save (point))
    (re-search-forward tpl-begin-placeholder)
    (if (looking-at tpl-pattern-optional)
	(progn
	  (setq token-type tpl-optional-type)
	  (re-search-forward tpl-pattern-optional)
	  ) ; progn
      ; else
      (progn
	(setq token-type tpl-placeholder-type)
	) ; progn
      ) ; if (looking-at tpl-pattern-optional)
    (setq start (point))
    (if (looking-at tpl-destination-symbol)
	(forward-char (length tpl-destination-symbol))
      (re-search-forward tpl-pattern-symbol)
      ) ; if
    (setq placeholder-type (buffer-substring start (point)))
    (if (looking-at tpl-sep-placeholder)
	(progn
	  (re-search-forward tpl-sep-placeholder)
	  (setq start (point))
	  (re-search-forward tpl-pattern-symbol)
	  (setq placeholder-name (buffer-substring start (point)))
	  ) ; progn
      ; else
      (progn
	(setq placeholder-name nil)
	) ; progn
      ) ; if (looking-at tpl-sep-placeholder)
    (setq placeholder (tpl-make-token
		       token-type
		       placeholder-type
		       placeholder-name))
    (goto-char save)
					; return
    placeholder
    ) ; let
  ) ; defun tpl-scan-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-unscan (token &optional column)
  "Insert at point the values of tokens in the tree rooted by TOKEN.
     Optional second argument COLUMN specifies where to indent rigidly.
     Default is the current column."
					; Local Variables
  (let (begin-template start-column token-list line-list line save-hook)
					; Body
					; Save auto-fill-hook and reset
    (setq save-hook auto-fill-hook)
    (if (not tpl-fill-while-unscanning)
	(setq auto-fill-hook nil)
      ) ; if
					; Unscan template
    (setq begin-template (point))
    (if column
	(setq start-column column)
      ; else
      (setq start-column (current-column))
      ) ; if column
    (setq line-list (tpl-token-value token))
    (while line-list
      (setq line (car line-list))
      (setq line-list (cdr line-list))
      (if (= tpl-comment-level (tpl-line-indent line))
	  (indent-to comment-column)
	; else
	(indent-to (+ start-column (tpl-line-indent line)))
	) ; if
      (setq token-list (tpl-line-tokens line))
      (while token-list
	(setq token (car token-list))
	(setq token-list (cdr token-list))
	;(debug "tpl-unscan token:" token)
	(insert-before-markers (tpl-token-value token))
	) ; while token-list
      (if line-list
	  (newline)
	) ; if line-list
      ) ; while line-list
    (if (and (boundp 'template-unscan-hook)
	     template-unscan-hook)
	(funcall template-unscan-hook begin-template (point) start-column)
      ) ; if
					; Reset auto-fill-hook
    (setq auto-fill-hook save-hook)
    ) ; let
  ) ; defun tpl-unscan

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-fix-syntax (string)
  "Change any syntax entries in STRING from (word or symbol or quote)
   to punctuation."
					; Local Variables
  (let (char)
					; Body
    (while (> (length string) 0)
      (setq char (string-to-char string))
      (setq string (substring string 1))
      (if (or (equal (char-syntax char) ? )
	      (equal (char-syntax char) ?_)
	      (equal (char-syntax char) ?'))
	  (modify-syntax-entry char ".   ")
	) ; if
      ) ; while
    ) ; let
  ) ; defun tpl-fix-syntax

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-initialize-scan ()
  "Initialize environment for scan."
					; Local Variables
  (let ()
					; Body
					; Make all characters non-symbols
    (tpl-fix-syntax tpl-begin-placeholder)
    (tpl-fix-syntax tpl-end-placeholder)
    (tpl-fix-syntax tpl-sep-placeholder)
    (tpl-fix-syntax tpl-pattern-optional)
					; Build composite patterns
    (setq tpl-begin-optional (concat tpl-begin-placeholder
				     tpl-pattern-optional))
    (setq tpl-destination-placeholder (concat tpl-begin-placeholder
					      tpl-destination-symbol
					      tpl-end-placeholder))
    (setq tpl-pattern-placeholder (concat tpl-begin-placeholder
					  "\\(" tpl-pattern-optional "\\)?"
					  tpl-pattern-symbol
					  "\\(" tpl-sep-placeholder
					  tpl-pattern-symbol "\\)?"
					  tpl-end-placeholder))
					; Build lexical patterns
    (setq lex-patterns
	  (list
	   (tpl-make-pattern tpl-placeholder-type tpl-pattern-placeholder)
	   (tpl-make-pattern tpl-whitespace-type tpl-pattern-whitespace)
	   (tpl-make-pattern tpl-word-type tpl-pattern-word)
	   (tpl-make-pattern tpl-punctuation-type tpl-pattern-punctuation)
	   (tpl-make-pattern tpl-other-type tpl-pattern-other)
	   ))
    (setq string-patterns
	  (list
	   (tpl-make-pattern tpl-string-type tpl-pattern-string)
	   ))
    (setq tpl-newline-token
	  (tpl-make-token tpl-terminal-type tpl-newline-type nil))
    ) ; let
  ) ; defun tpl-initialize-scan

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of tplscan.el
