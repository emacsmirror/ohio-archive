; Path: hal.com!olivea!uunet!psinntp!ctp!ebail
; From: ebail@ctp.com (Erik Bailey)
; Newsgroups: gnu.emacs.sources
; Subject: Re: cs-mode.el
; Keywords: cstar, emacs major mode
; Date: 23 Nov 92 14:03:04 GMT
; Organization: Cambridge Technology Partners
; Nntp-Posting-Host: earth.ctp.com
; 
; In article <1ehse2INNk2e@crcnis1.unl.edu> stevew@helios.unl.edu (Steve Wu) writes:
; >  Does anyone have cs-mode.el or something like that which I can get
; >it by any means? I am working on c* and I wish someone has written the
; >cs major mode. Thanks for  you help
; 


;; C* (Connection Machine C) mode for Emacs
;; LCD Archive Entry:
;; c*-mode|Erik Bailey|ebail@ctp.com|
;; C* (Connection Manchine C) mode.|
;; 92-11-23||~/modes/c*-mode.el.Z|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a derivative of the c-mode file provided with GNU Emacs.
;; All changes by Leban@cs.umass.edu (C) Copyright 1990 Bruce Leban
;; under the exact same terms as the GNU public license agreement.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SUMMARY: C* mode knows about the syntax of C* version 6.0:
''
;; * It knows how to distinguish the `:' in a label from the `:' which
;;   precedes a shape designator.
;; * It recognizes the `where' keyword.
;;
;; OTHER CHANGES:
;;
;; * The electric `:' character was changed to reindent only for case labels.
;; * The newline character was made electric.  It reindents a line if `:' is
;;   the last character (electric-c*-newline)
;; * Added a variable `c-newline-indents' which controls whether or not
;;   newline automatically indents the next line.  (This is similar to
;;   rebinding newline to `newline-and-indent' except it doesn't conflict
;;   with the previous change.)
;; * The `#' character was made electric.  It reindents a line when `#' is
;;   the first character.  (electric-c*-pound)
;; * More graceful error handling when a missing if or unbalanced braces
;;   are encountered. (c*-backward-to-start-of-if)
;; * The c-newline-indents variable mentioned above.
;; * Changed "case[ \t]" to "case\\b" throughout (bug fix).
;; * Fixed a bug which misindented lines like ``case (...):''.
;; * Fixed a bug that misindented the first line of code if the first line
;;   of the file started with a # (fairly common on .h files).
;; * Occurrences of ?{ and ?} in the code were changed to ?\{ and ?\} to
;;   make editing easier.
;; NOTE: most of the other changes above should also be applied to c-mode.

;; The electric colon was changed to reindent the line only used when the
;; line starts with the word `case'.  Correspondingly, an electric newline
;; was added to reindent the line if the last character was a colon (since
;; it's probably a label).  A variable c-newline-indents controls whether 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fine print: C* mode believes a regular label is a line that starts
;; with a symbol followed by a colon and a whitespace character or the end of
;; line.  A case label is identified by the word `case' at the start of a
;; line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file requires c-mode to be loaded in order to work.  It appears to
;; be loaded by default in some (all?) Emacs.  If c-mode is not loaded when
;; this file is loaded, it is loaded first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most changes are marked by ;;C* for C* changes and ;;BPL otherwise.

;; This file is *not* part of GNU Emacs (although c-mode is).

;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


(provide 'c*-mode)
(provide 'cstar-mode)			;because many things call it that
					;-mjab Fri Feb 22 13:22:44 1991

; (require 'c-mode) ... doesn't work so ...
(if (eq 'autoload (car (symbol-function 'c-mode)))
    (load (car (cdr (symbol-function 'c-mode)))))

(defconst c-newline-indents nil
  "*Non-nil means make newline act like newline-and-indent.")

(defvar c*-mode-abbrev-table nil
  "Abbrev table in use in c*-mode buffers.")

(define-abbrev-table 'c*-mode-abbrev-table ())

(defvar c*-mode-map ()
  "Keymap used in C mode.")
(if c*-mode-map
    ()
  (setq c*-mode-map (make-sparse-keymap))
  (define-key c*-mode-map "\r" 'electric-c*-newline)
  (define-key c*-mode-map "{" 'electric-c*-brace)
  (define-key c*-mode-map "}" 'electric-c*-brace)
  (define-key c*-mode-map "#" 'electric-c*-pound)
  (define-key c*-mode-map ";" 'electric-c*-semi)
  (define-key c*-mode-map ":" 'electric-c*-colon)
  (define-key c*-mode-map "\e\C-h" 'mark-c-function)
  (define-key c*-mode-map "\e\C-q" 'indent-c*-exp)
  (define-key c*-mode-map "\177" 'backward-delete-char-untabify)
  (define-key c*-mode-map "\t" 'c*-indent-command))

(defvar c*-mode-syntax-table nil
  "Syntax table in use in c*-mode buffers.")

(if c*-mode-syntax-table
    ()
  (setq c*-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" c*-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" c*-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" c*-mode-syntax-table)
  (modify-syntax-entry ?+ "." c*-mode-syntax-table)
  (modify-syntax-entry ?- "." c*-mode-syntax-table)
  (modify-syntax-entry ?= "." c*-mode-syntax-table)
  (modify-syntax-entry ?% "." c*-mode-syntax-table)
  (modify-syntax-entry ?< "." c*-mode-syntax-table)
  (modify-syntax-entry ?> "." c*-mode-syntax-table)
  (modify-syntax-entry ?& "." c*-mode-syntax-table)
  (modify-syntax-entry ?| "." c*-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" c*-mode-syntax-table))

(defun cstar-mode ()			;so things like auto-modes will
  (interactive)				;work -mjab Fri Feb 22 13:26:26 1991
  (c*-mode))

(defun c*-mode ()
  "Major mode for editing C* code.
\\{c-mode-map}
This is the same as C mode except it understands about C* syntax.  It
knows about the where statement and the difference between labels
and shape declarations.
   case 3:
is indented like a label
   int:current a;
is indented like a statement.  It does not automatically reindent the line
when it sees a trailing colon (i.e., after you type int:).

The variables controlling indentation style are those used for C mode.
Set c-indent-level, not c*-indent-level.

Turning on C* mode calls the value of the variable c*-mode-hook with no
args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map c*-mode-map)
  (setq major-mode 'c*-mode)
  (setq mode-name "C*")
  (setq local-abbrev-table c*-mode-abbrev-table)
  (set-syntax-table c*-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c*-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'c*-mode-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c*-indent-line ()
  "Indent current line as C* code.
Return the amount the indentation changed by.
Changed to respect the `:'s in declarations."
  (let ((indent (calculate-c*-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-c-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((or (looking-at "case\\b")  ;;C*
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (or (looking-at ":[\t ]")	;;C*
				 (looking-at ":$")))))	;;C*
		  (setq indent (max 1 (+ indent c-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (c*-backward-to-start-of-if)
				 (current-indentation))))
		 ((= (following-char) ?\})
		  (setq indent (- indent c-indent-level)))
		 ((= (following-char) ?\{)
		  (setq indent (+ indent c-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-c*-indent (&optional parse-start)
  "Return appropriate indentation for current line as C* code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
Changed to respect the `:'s in declarations."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?\{)
		 0   ; Unless it starts a function body
	       (c-backward-to-noncomment (or parse-start (point-min)))
	       ;; Look at previous line that's at column 0
	       ;; to determine whether we are in top-level decls
	       ;; or function's arg decls.  Set basic-indent accordinglu.
	       (let ((basic-indent
		      (save-excursion
			(re-search-backward "^[^ \^L\t\n#]" nil 'move)
			(if (and (looking-at "\\sw\\|\\s_")
				 (looking-at ".*(")
				 (progn
				   (goto-char (1- (match-end 0)))
				   (forward-sexp 1)
				   (and (< (point) indent-point)
					(not (memq (following-char)
						   '(?\, ?\;))))))
			    c-argdecl-indent 0))))

		   ;; Now add a little if this is a continuation line.
		   (+ basic-indent
		      (if (or (bobp)
			      (memq (preceding-char) '(?\) ?\; ?\}))
			      (progn (beginning-of-line)
				     (= (following-char) ?#)))
					; BPL fix misindentation bug
			  0 c-continued-statement-offset)))))
	    ((/= (char-after containing-sexp) ?\{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (c-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (not (memq (following-char) '(? ?\n ?\t)))  ;;C*
			     (or (eq (char-after (- (point) 2)) ?\')
				 (memq (char-syntax (char-after (- (point) 2)))
				       '(?w ?_)))))
	       (if (eq (preceding-char) ?\,)
		   (c-backward-to-start-of-continued-exp containing-sexp))
	       (beginning-of-line)
	       (c-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?\} ?\{ ?:)))
		 ;; BPL above fixes bug in indenting some case lines.
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  c-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (c-backward-to-start-of-continued-exp containing-sexp)
		   (+ c-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?\{))
			  c-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (let ((colon-line-end 0))
		     (while (progn (skip-chars-forward " \t\n")
				   (looking-at "#\\|/\\*\\|case\\b.*:\\|[a-zA-Z0-9_$]*[ \t]:\\|[a-zA-Z0-9_$]*:$"))	;;C*
		       ;; Skip over comments and labels following openbrace.
		       (cond ((= (following-char) ?\#)
			      (forward-line 1))
			     ((= (following-char) ?\/)
			      (forward-char 2)
			      (search-forward "*/" nil 'move))
			     ;; case or label:
			     (t
			      (save-excursion (end-of-line)
					      (setq colon-line-end (point)))
			      (search-forward ":"))))
		     ;; The first following code counts
		     ;; if it is before the line we want to indent.
		     (and (< (point) indent-point)
			  (if (> colon-line-end (point))
			      (- (current-indentation) c-label-offset)
			    (current-column)))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If c-indent-level is zero,
		 ;; use c-brace-offset + c-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in c-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop c-indent-level))
			(+ c-brace-offset c-continued-statement-offset)
		      c-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the c-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 c-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun indent-c*-exp ()
  "Indent each line of the C* grouping following point.
Changed to respect the `:'s in declarations."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (c-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?\{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?\{))
		    (c-backward-to-noncomment opoint)
		    (if (not (memq (preceding-char) '(nil ?\, ?\; ?\{ ?: ?\})))
			;; Preceding line did not end in comma or semi;
			;; indent this line  c-continued-statement-offset
			;; more than previous.
			(progn
			  (c-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ c-continued-statement-offset (current-column)
				   (if at-brace c-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (c*-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-c-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or (looking-at "case\\b")  ;;C*
		    (and (looking-at "[A-Za-z]")
			 (save-excursion
			   (forward-sexp 1)
			   (or (looking-at ":[ \t]")  ;;C*
			       (looking-at ":$")))))  ;;C*
		(setq this-indent (max 1 (+ this-indent c-label-offset))))
	    (if (= (following-char) ?\})
		(setq this-indent (- this-indent c-indent-level)))
	    (if (= (following-char) ?\{)
		(setq this-indent (+ this-indent c-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
; (message "Indenting C expression...done")
  )

(defun c*-backward-to-start-of-if (&optional limit)
    "Move to the start of the last ``unbalanced'' if.
Treats `where' the same as `if'."
    (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
    (let ((if-level 1)
	  (case-fold-search nil))
	(while (not (zerop if-level))
	    (let ((oldpt (point)))	; BPL handle errors gracefully
		(catch t (unwind-protect (backward-sexp 1) (throw t t)))
		(if (= (point) oldpt)
		    (error "Unmatched else or unbalanced braces.")))
	    (cond ((looking-at "else\\b")
		   (setq if-level (1+ if-level)))
		  ((or (looking-at "if\\b")
		       (looking-at "where\\b"))	 ;;C*
		   (setq if-level (1- if-level)))
		  ((< (point) limit)
		   (setq if-level 0)
		   (goto-char limit))))))

(defun electric-c*-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or case ....
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case\\b"))  ;;C*
			     (save-excursion
			       (forward-word 1)
			       (skip-chars-forward " \t")
			       (< (point) end)))
			(progn
			  (beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (c*-indent-line)
	  (and c-auto-newline
	       (not (c-inside-parens-p))
	       (progn
		 (newline)
		 (setq insertpos (- (point) 2))
		 (c*-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following functions are new but should really be in C mode too.

(defun electric-c*-pound (arg)
  "Insert # character and possibly correct line's indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (eq (following-char) ?#))
      (c*-indent-line)))

(defun electric-c*-colon (arg)
  "Insert colon and possibly correct line's indentation.
Does not reindent line if is not unambiguously a label."
  (interactive "P")
  (if (save-excursion
	  (beginning-of-line)
	  (looking-at "[ \t]*case\\b"))
      (if c-auto-newline
	  (electric-c*-terminator arg)
	  (c*-indent-line)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg))))

(defun electric-c*-newline (arg)
  "Insert newline and possibly correct line's indentation.
Also checks the c-newline-indents variable to indent the next line."
  (interactive "P")
  (if (save-excursion
	  (skip-chars-backward " \t")
	  (eq (preceding-char) ?:))
      (c*-indent-line))
  (if c-newline-indents
      (newline (prefix-numeric-value arg))
      (newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following functions had to be changed to refer to c*- functions.

(defun c*-indent-command (&optional whole-exp)
  (interactive "P")
  "Indent current line as C* code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (c*-indent-line))
	    beg end)
	(save-excursion
	  (if c-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not c-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (c*-indent-line))))


(defun electric-c*-brace (arg)
  "Insert { or } character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if c-auto-newline (progn (c*-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (c*-indent-line)
	  (if c-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(c*-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-c*-semi (arg)
  "Insert ; character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-c*-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))
-- 
Erik Bailey   | Cambridge Technology Partners      | The usual disclaimer
              | 304 Vassar St., Cambridge MA 02139 | applies: this post is
ebail@ctp.com | (617) 374-8418                     | mine, not my employer's.
      /earth is 98% full; please remove any excess inhabitants.
