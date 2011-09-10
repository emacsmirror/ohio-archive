;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-indent.el - functions to indent SQL code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Author:        Peter D. Pezaris <pez@atlantic2.sbi.com>
;;  Maintainer:    sql-mode-help@atlantic2.sbi.com
;;  Version:       0.922 (beta)
;;  Last Modified: Mon Oct 30 13:59:46 1995
;;  Keywords:      isql fsql sql editing major-mode languages
;;
;;  Copyright © 1995 Peter D. Pezaris
;;
;;  This file is part of the SQL Mode package.  Refer to the sql-mode.el
;;  file for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sql-strict-syntax-p t)

(defvar sql-echo-syntactic-information-p t)

(defvar sql-basic-offset 4
  "*Indentation of SQL statements with respect to containing block.")

(defvar sql-offsets-alist
  ;  element			 offset
  '((statement			. nil)
    (where-clause		. nil)
    (set-clause			. +/2)
    (from-clause		. +/2)
    (else-clause		. -)
    (update			. nil)
    (continued-statement	. +/2)
    (conjunction		. +/2)
				     
;    (if			. [0 
    (begin-block		. nil)
    (end			. -)
    (cpp-macro			. 0)
    (go				. 0)
    (procedure-declaration	. 0)
    (go-statement		. 0)
    (beginning-of-statement	. nil)
;    (keyword			. +)
    (open-paren			. nil)
    (close-paren		. -)
    (open-brace			. nil)
    (close-brace		. -)
    (empty-line			. +/2)
    (string			. nil)
    (comment			. 10)
    (unknown			. +/2)
    )
)

(defun sql-guess-basic-syntax ()
  ;; guess the syntactic description of the current line of SQL code.
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (fullstate (sql-parse-state))
	     (state fullstate)
	     (context (buffer-syntactic-context))
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache inswitch-p
	     ;; narrow out any enclosing class
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     )

	;; get the buffer position of the most nested opening brace,
	;; if there is one, and it hasn't been narrowed out
	(save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t}")
	  (skip-chars-backward " \t")
	  (while (and state
		      (not in-method-intro-p)
		      (not containing-sexp))
	    (setq containing-sexp (car state)
		  state (cdr state))
	    (if (consp containing-sexp)
		;; if cdr == point, then containing sexp is the brace
		;; that opens the sexp we close
		(if (= (cdr containing-sexp) (point))
		    (setq containing-sexp (car containing-sexp))
		  ;; otherwise, ignore this element
		  (setq containing-sexp nil))
	      ;; ignore the bufpos if its been narrowed out by the
	      ;; containing class
	      (if (<= containing-sexp (point-min))
		  (setq containing-sexp nil)))))

	;; set the limit on the farthest back we need to search
	(setq lim (or containing-sexp
		      (if (consp (car fullstate))
			  (cdr (car fullstate))
			nil)
		      (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (following-char))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; are we in a literal?
	(setq literal (sql-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 (syntactic-context
	  (sql-add-syntax syntactic-context (sql-point 'bopl)))
;	 ((memq literal '(string))
;	  (sql-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
;	 ((memq literal '(sql))
	  ;; we need to catch multi-paragraph C comments
;	  (while (and (zerop (forward-line -1))
;		      (looking-at "^[ \t]*$")))
;	  (sql-add-syntax 'comment (sql-point-at (beginning-of-line))))
	 ;; CASE 3: in a cpp preprocessor
;	 ((eq literal 'pound)
;	  (sql-beginning-of-macro lim)
;	  (sql-add-syntax 'cpp-macro (sql-point 'boi)))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, class, or
	   ;; inline-inclass method opening brace
	   ((= char-after-ip ?{)
	    (cond
	     ;; CASE 5A.1: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.2: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		;; c-b-o-s could have left us at point-min
		(and (bobp)
		     (c-forward-syntactic-ws indent-point))
		(setq placeholder (point))
		(and (or (looking-at "enum[ \t\n]+")
			 (= char-before-ip ?=))
		     (save-excursion
		       (skip-chars-forward "^;" indent-point)
		       (/= (following-char) ?\;))))
	      (c-add-syntax 'brace-list-open placeholder))
	     ;; CASE 5A.3: inline defun open
	     (inclass-p
	      (c-add-syntax 'inline-open (aref inclass-p 0)))
	     ;; CASE 5A.4: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (c-add-syntax 'defun-open (c-point 'bol))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (= char-before-ip ?:)
		  (= char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (= char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (= (preceding-char) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5B.3: nether region after a C++ func decl
	     (t
	      (c-add-syntax 'ansi-funcdecl-cont (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and c-baseclass-key (looking-at c-baseclass-key))
		(and (or (= char-before-ip ?:)
			 (= char-after-ip ?:))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (= char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key))))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((= char-after-ip ?:)
	      (c-backward-syntactic-ws lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((= char-before-ip ?:)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5C.3: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level compound statement or a
	   ;; member init list continuation
	   ((= char-before-ip ?,)
	    (goto-char indent-point)
	    (c-backward-syntactic-ws lim)
	    (while (and (< lim (point))
			(= (preceding-char) ?,))
	      ;; this will catch member inits with multiple
	      ;; line arglists
	      (forward-char -1)
	      (c-backward-syntactic-ws (c-point 'bol))
	      (if (= (preceding-char) ?\))
		  (backward-sexp 1))
	      ;; now continue checking
	      (beginning-of-line)
	      (c-backward-syntactic-ws lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (= (preceding-char) ?:)
		   (save-excursion
		     (forward-word -1)
		     (not (looking-at c-access-key))))
	      (goto-char indent-point)
	      (c-backward-syntactic-ws lim)
	      (c-safe (backward-sexp 1))
	      (c-add-syntax 'member-init-cont (c-point 'boi))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(= (following-char) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a multiple inheritance line?
	     ((looking-at c-inher-key)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a template list continuation?
	     ((save-excursion
		(skip-chars-backward "^<" lim)
		;; not sure if this is the right test, but it should
		;; be fast and mostly accurate.
		(and (= (preceding-char) ?<)
		     (not (c-in-literal lim))))
	      ;; we can probably indent it just like and arglist-cont
	      (c-add-syntax 'arglist-cont (point)))
	     ;; CASE 5D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement-1 lim)
	      ;; skip over any access-specifiers
	      (if inclass-p
		  (while (looking-at c-access-key)
		    (forward-line 1)))
	      ;; skip over comments, whitespace
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-access-key
		 (looking-at c-access-key))
	    (c-add-syntax 'access-label (c-point 'bonl))
	    (c-add-syntax 'inclass (aref inclass-p 0)))
	   ;; CASE 5F: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (= char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and
		      (condition-case nil
			  (progn (backward-sexp 1) t)
			(error nil))
		      (= (point) (aref inclass-p 1))
		      ))))
	    (save-restriction
	      (widen)
	      (goto-char (aref inclass-p 0))
	      (c-add-syntax 'class-close (c-point 'boi))))
	   ;; CASE 5G: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (save-excursion
		   (c-backward-syntactic-ws lim)
		   (while (memq (preceding-char) '(?\; ?,))
		     (beginning-of-line)
		     (setq placeholder (point))
		     (c-backward-syntactic-ws lim))
		   (and (= (preceding-char) ?\))
			(or (not (eq major-mode 'objc-mode))
			    (progn
			      (forward-sexp -1)
			      (forward-char -1)
			      (c-backward-syntactic-ws)
			      (not (or (= (preceding-char) ?-)
				       (= (preceding-char) ?+)
				       ;; or a class category
				       (progn
					 (forward-sexp -2)
					 (looking-at c-class-key))
				       )))))
		   )
		 (save-excursion
		   (c-beginning-of-statement-1)
		   (not (looking-at "typedef[ \t\n]+"))))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (c-point 'boi)))
	   ;; CASE 5H: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  c-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (backward-sexp 1) t))
			    (looking-at c-access-key)))
		(backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (preceding-char) '(?\; ?\}))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol)))
	    (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	   ;; CASE 5I: we are at a method definition continuation line
	   ((and (eq major-mode 'objc-mode)
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-ObjC-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5J: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))				; end CASE 5
	 ;; CASE 6: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((/= (char-after containing-sexp) ?{)
	  (c-backward-syntactic-ws containing-sexp)
))))))

(defun sql-guess-basic-syntax (&optional position)
  (interactive)
  (save-excursion
    (and position (goto-char position))
    (back-to-indentation)
    (let ((syntax
	   (cond 
	    ((looking-at "where\\>")
	     'where-clause)
	    ((looking-at "and\\>\\|or\\>")
	     'conjunction)
	    ((looking-at "else\\>")
	     'else-clause)
	    ((looking-at "set\\>")
	     'set)
	    ((looking-at "end\\>")
	     'end)
	    ((looking-at "#")
	     'preprocessor)
	    ((looking-at "prodedure\\>")
	     'procedure-declaration)
	    ((looking-at "if\\>")
	     'if)
	    ((looking-at "begin\\>")
	     'begin)
	    ((looking-at sql-bos-regexp)
	     'beginning-of-statement)
	    ((looking-at "(\n")
	     'open-paren)
	    ((looking-at ")")
	     'close-paren)
	    ((looking-at "{\n")
	     'open-brace)
	    ((looking-at "}")
	     'close-brace)
	    ((looking-at "go$")
	     'go)
	    ((looking-at sql-keyword-regexp)
	     'keyword)
	    ((not (looking-at "."))
	     'empty-line)
	    (t
	     'unknown))))
;      (and (interactive-p)
	   (message "Syntax: %s" (symbol-name syntax)) (sit-for .2)
;	   )
      syntax)))

(defun sql-previous-matching-point (begin-regexp end-regexp)
  (save-excursion
    (let ((level 1))
      (while (and (> level 0)
		  (equal 0 (forward-line -1)))
	(back-to-indentation)
	(cond ((looking-at begin-regexp)
	       (setq level (1- level)))
	      ((looking-at end-regexp)
	       (setq level (1+ level)))))
      (if (> level 0)
	  0
	(point)))))

(defun sql-point-of-regexp (regexp &optional anywhere)
  (interactive)
  (save-excursion
    (if anywhere
	(progn
	  (re-search-backward regexp nil t)
	  (point))
      (let ((done nil)
	    (the-point (point)))
	(while (and (not done) (= 0 (forward-line -1)))
	  (back-to-indentation)
	  (if (looking-at regexp)
	      (progn
		(setq done t)
		(setq the-point (point)))))
	the-point))))

(defun sql-beginning-of-function ()
  (re-search-backward "^as\\>" nil t))

(defun sql-backward-statement (&optional count)
  (interactive)
  (or count (setq count 1))
  (while (> count 0)
    (sql-backward-statement-1)
    (setq count (1- count))))
  
(defun sql-backward-statement-1 ()
  (let ((opoint (point))
	(point nil))
    (save-excursion
;      (end-of-line)
      (if (eq (preceding-char) ?\()
	  (point)
	(let* ((done nil)
	       (start (save-excursion (sql-beginning-of-function)
				      (forward-word 1)
;				      (search-forward "(")
				      (point)))
	       (start-state (parse-partial-sexp start (point) 0))
	       (depth (nth 0 start-state))
	       (contain (nth 1 start-state)))
	  (while (not done)
	    (if (not (re-search-backward sql-bos-regexp contain t))
		(progn
		  (message "C")
		  (sit-for 1)
		  (setq done t
		      point (1+ contain)))
	      (let* ((state (parse-partial-sexp start (point) 0))
		     (lim (nth 1 state))
		     (d (nth 0 state)))
		(cond
		 ((= depth d)
		  (setq done t)
		  (message "A %d" depth) (sit-for 1)
		  (setq point (point)))
		 ((> depth d)
		  (setq done t)
		  (message "B") (sit-for 1)
		  (setq point contain)))))))
	(if (< point opoint)
	    (setq opoint point))))
    (goto-char opoint)))

(define-key sql-mode-map "\M-a" 'sql-backward-statement)

(setq defun-prompt-regexp
      "\\(create\\|CREATE\\) \\(procedure\\|PROCEDURE\\).*")

(defmacro sql-point-at (&rest body)
  "Execute BODY, and return the value of point when BODY is done evaluating.
When this function termintates, the value of point is restored."
  (list 'save-excursion
	(cons 'progn body)
	'(point)))

(defun sql-get-base-position (syntax)
  (interactive)
  (let ((pos (cond
	      ((eq syntax 'where-clause)
	       (sql-point-of-regexp "select\\|update\\|delete"))
	      ((eq syntax 'set)
	       (sql-point-of-regexp "update"))
	      ((eq syntax 'conjunction)
	       (sql-point-of-regexp sql-keyword-no-conjunctions-regexp))
	      ((eq syntax 'else-clause)
	       (sql-previous-matching-point "if\\>" "else\\>"))
	      ((eq syntax 'end)
	       (sql-previous-matching-point "begin\\>" "end\\>"))
	      ((eq syntax 'preprocessor)
	       nil)
	      ((eq syntax 'procedure-declaration)
	       nil)
;   ((eq syntax 'open-paren)
;    (sql-point-at (sql-beginning-of-statement)))
;   ((eq syntax 'if)
;    (sql-point-of-regexp sql-bos-regexp))
;   ((eq syntax 'begin)
;    (sql-point-of-regexp "."))
;   ((eq syntax 'beginning-of-statement)
;    (sql-point-of-regexp "."))
;   ((eq syntax 'empty-line)
;    (sql-point-of-regexp "."))
	      (t
	       (sql-point-at (sql-beginning-of-statement))))))
    (message "%d" pos)
    pos))
;    (sql-point-of-regexp sql-bos-regexp))))

(defun sql-indent-of (position)
  (save-excursion
    (goto-char position)
    (beginning-of-line)
    (current-column)))

(defun sql-indent-line ()
  (interactive)
  (let* ((old-point (point))
	 (old-indent (sql-indent-of (point)))
	 (syntax (sql-guess-basic-syntax))
	 (base (sql-get-base-position syntax))
	 (previous-syntax (sql-guess-basic-syntax base))
	 (indent (sql-get-indent (cons syntax base))))
    (save-excursion
      (back-to-indentation)
      (delete-region (point) (progn (beginning-of-line) (point)))
      (indent-to indent))
    (if (< (current-column) indent)
	(skip-chars-forward " \t"))))
    
(defun sql-previous-bol-indent (regexp)
  (save-excursion
    (let ((count 0)
	  (done nil))
      (while (and (not done)
		  (equal 0 (forward-line -1)))
	(setq count (1+ count))
	(back-to-indentation)
	(if (looking-at regexp)
	    (setq done t)))
      (if (not done)
	  0
	(current-column)))))

(defun sql-previous-indent (regexp &optional check-for-if)
  (let ((previous-bol (sql-previous-bol-indent regexp))
	(offset 0))
    (and check-for-if
	 (save-excursion
	   (beginning-of-line)
	   (and (re-search-backward (concat "^[ \t]*" sql-bos-regexp) nil t)
		(not (looking-at "[ \t]*\\(if\\|else\\)"))
		(re-search-backward (concat "^[ \t]*" sql-bos-regexp) nil t)
		(looking-at "[ \t]*\\(if\\|else\\)")
		(setq offset (- sql-basic-offset)))))
    (+ previous-bol offset)))

(defun sql-previous-indent-of (regexp)
  (save-excursion
    (if (re-search-backward regexp nil t)
	(current-column)
      0)))

(defun sql-previous-matching-indent (begin-regexp end-regexp)
  (save-excursion
    (let ((level 1))
      (while (and (> level 0)
		  (equal 0 (forward-line -1)))
	(back-to-indentation)
	(cond ((looking-at begin-regexp)
	       (setq level (1- level)))
	      ((looking-at end-regexp)
	       (setq level (1+ level)))))
      (if (> level 0)
	  0
	(current-column)))))

(defun sql-back-to-nonblank-line ()
  "Move up lines until a line with something other than whitespace is found.
Return number of lines moved."
  (let ((count 0)
	(done nil))
    (while (and (not done)
		(equal 0 (forward-line -1)))
      (setq count (1+ count))
      (back-to-indentation)
      (if (looking-at ".")
	  (setq done t)))
    (if done
	count
      0)))

(defun sql-previous-command-info ()
  (interactive)
  (let ((done nil))
    (save-excursion
      (while (and (not done)
		  (equal 0 (forward-line -1)))
	(back-to-indentation)
	(if (looking-at sql-bos-regexp)
	    (setq done t)))
      (if done
	  (cons (current-column) (buffer-substring (match-beginning 0) 
						   (match-end 0)))
	(cons 0 "")))))

(defun sql-calculate-offset ()
  (interactive)
  (let* ((info (sql-previous-command-info))
	 (offset 0)
	 (command-type (cdr info)))
    (cond ((string-equal command-type "begin")
	   (setq offset sql-basic-offset))
	  ((string-equal command-type "(")
	   (setq offset sql-basic-offset))
	  ((string-equal command-type "{")
	   (setq offset sql-basic-offset))
	  ((string-equal command-type "if")
	   (setq offset sql-basic-offset))
	  ((string-equal command-type "else")
	   (setq offset sql-basic-offset))
;	  ((string-equal command-type "select")
;	   (setq offset 7))
;	  ((string-equal command-type "update")
;	   (setq offset 7))
	  (t
	   nil))
    offset))

(defun sql-get-indent (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; sql-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol sql-offsets-alist))
	 (offset (cdr-safe match))
	 (indent nil)
	 (relative t))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (if (numberp offset)
	(setq relative nil))
    (cond
     ((not match)
      (if sql-strict-syntax-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((not offset)
      (setq offset 0))
     ((eq offset '+)
      (setq offset sql-basic-offset))
     ((eq offset '++)
      (setq offset (* 2 sql-basic-offset)))
     ((eq offset '-)
      (setq offset (- sql-basic-offset)))
     ((eq offset '--)
      (setq offset (* 2 (- sql-basic-offset))))
     ((eq offset '+/2)
      (setq offset (/ sql-basic-offset 2)))
     ((eq offset '-/2)
      (setq offset (/ (- sql-basic-offset) 2)))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset symbol)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (if relative
	(+ (if (and relpos
					;		(cdr-safe match)
		    (< relpos (save-excursion
				(beginning-of-line)
				(point))))
	       (save-excursion
		 (goto-char relpos)
		 (current-column))
	     0)
	   offset)
      offset)))

;(defun sql-indent-line ()
;  "Indent the current line as SQL code."
;  (interactive)
;  (let ((indent (sql-calculate-indent)))
;    (save-excursion
;      (back-to-indentation)
;      (delete-region (point) (progn (beginning-of-line) (point)))
;      (indent-to indent))
;    (if (< (current-column) indent)
;	(skip-chars-forward " \t"))))

;(defun sql-calculate-indent ()
;  "Return appropriate indentation for current line of SQL code.
;In usual case returns an integer: the column to indent to.
;Returns nil if line starts inside a string, t if in a comment.

;Not fully implementd (yet)"
;  (interactive)
;  (let ((offset (sql-calculate-offset))
;	(indent (current-column)))
;    (save-excursion
;      (back-to-indentation)
;      (cond 
;       ((looking-at "where\\>")
;	(setq indent (sql-previous-indent-of "select\\|update")))
;       ((looking-at "and\\>")
;	(setq indent (sql-previous-indent sql-keyword-regexp)))
;       ((looking-at "else\\>")
;	(setq indent (sql-previous-matching-indent "if\\>" "else\\>")))
;       ((looking-at "end\\>")
;	(setq indent (sql-previous-matching-indent "begin\\>" "end\\>")))
;       ((looking-at "#\\|go")
;	(setq indent 0))
;       ((looking-at "prodedure\\>")
;	(setq indent 0))
;       ((looking-at sql-bos-regexp)
;	(setq indent (+ offset (sql-previous-indent sql-bos-regexp t))))
;       ((looking-at sql-keyword-regexp)
;	(setq indent (+ offset (sql-previous-indent sql-keyword-regexp))))
;       ((looking-at "(\n")
;	(setq indent (+ sql-basic-offset
;			(sql-previous-indent sql-keyword-regexp))))
;       ((looking-at ")")
;	(setq indent (sql-previous-matching-indent "(\n" ")\n")))
;       ((looking-at "{\n")
;	(setq indent (+ sql-basic-offset
;			(sql-previous-indent sql-keyword-regexp))))
;       ((looking-at "}")
;	(setq indent (sql-previous-matching-indent "}\n" "}\n")))
;       ((not (looking-at "."))
;	(setq indent (+ offset (sql-previous-indent sql-keyword-regexp))))
;       (t
;	(setq indent (+ offset
;			(if (eq offset 0)
;			    sql-continued-statement-offset
;			  0)
;			(sql-previous-indent sql-keyword-regexp)))))
;      indent)))


(defun sql-parse-state ()
  ;; Finds and records all open parens between some important point
  ;; earlier in the file and point.
  ;;
  ;; if there's a state cache, return it
  (if (boundp 'sql-state-cache) sql-state-cache
    (let* (at-bob
	   (pos (save-excursion
		  ;; go back 2 bods, but ignore any bogus positions
		  ;; returned by beginning-of-defun (i.e. open paren
		  ;; in column zero)
		  (let ((cnt 2))
		    (while (not (or at-bob (zerop cnt)))
		      (beginning-of-defun)
		      (if (= (following-char) ?\{)
			  (setq cnt (1- cnt)))
		      (if (bobp)
			  (setq at-bob t))))
		  (point)))
	   (here (save-excursion
		   ;;(skip-chars-forward " \t}")
		   (point)))
	   (last-bod pos) (last-pos pos)
	   placeholder state sexp-end)
      ;; cache last bod position
      (while (catch 'backup-bod
	       (setq state nil)
	       (while (and pos (< pos here))
		 (setq last-pos pos)
		 (if (and (setq pos (sql-safe (scan-lists pos 1 -1)))
			  (<= pos here))
		     (progn
		       (setq sexp-end (sql-safe (scan-sexps (1- pos) 1)))
		       (if (and sexp-end
				(<= sexp-end here))
			   ;; we want to record both the start and end
			   ;; of this sexp, but we only want to record
			   ;; the last-most of any of them before here
			   (progn
			     (if (= (char-after (1- pos)) ?\{)
				 (setq state (cons (cons (1- pos) sexp-end)
						   (if (consp (car state))
						       (cdr state)
						     state))))
			     (setq pos sexp-end))
			 ;; we're contained in this sexp so put pos on
			 ;; front of list
			 (setq state (cons (1- pos) state))))
		   ;; something bad happened. check to see if we
		   ;; crossed an unbalanced close brace. if so, we
		   ;; didn't really find the right `important bufpos'
		   ;; so lets back up and try again
		   (if (and (not pos) (not at-bob)
			    (setq placeholder
				  (sql-safe (scan-lists last-pos 1 1)))
			    ;;(char-after (1- placeholder))
			    (<= placeholder here)
			    (= (char-after (1- placeholder)) ?\}))
		       (while t
			 (setq last-bod (sql-safe (scan-lists last-bod -1 1)))
			 (if (not last-bod)
			     (error "unbalanced close brace at position %d"
				    (1- placeholder))
			   (setq at-bob (= last-bod (point-min))
				 pos last-bod)
			   (if (= (char-after last-bod) ?\{)
			       (throw 'backup-bod t)))
			 ))		;end-if
		   ))			;end-while
	       nil))
      state)))