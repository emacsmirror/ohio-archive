;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TADS mode --- TADS editing commands for Emacs
;;;
;;; Copyright (C) 1994 Darin Johnson <djohnson@ucsd.edu>
;;; Portions of this code were adapted from GNU Emacs C-mode, and are
;;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.
;;;
;;; Refer to the GNU Emacs General Public License for copyright info,
;;;
;;; TADS (Text Adventure Development System) is
;;; Copyright (c) 1992 by Michael J. Roberts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LCD Archive Entry:
;;; tads|Darin Johnson|djohnson@ucsd.edu|
;;; Bare bones mode for TADS (Text Adventure Development System)|
;;; Feb-94|1.0|~/games/tads.el.Z|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ok, now that that's all out of the way...
;;;
;;; This is just like C-mode, only different.  If you don't know
;;; C-mode in Emacs, you should check that out sometime (C-h f c-mode).
;;; It's not perfect, and may mess up at times, but it's better than
;;; relying upon C or C++ mode.
;;;
;;; Here's the standard rundown for Emacs novices.
;;; In your .emacs file, put the following line:
;;;
;;;    (auto-load 'tads-mode "tads-mode")  ;; the last is path to this file
;;;
;;; Then, giving the "tads-mode" command (Control-c X tads-mode) will
;;; start things up.  To automatically start TADS mode when you edit
;;; a file ending in ".t", add the following lines as well:
;;;
;;;   (setq auto-mode-alist (append (list (cons "\\.t$" 'tads-mode))
;;;                              auto-mode-alist))
;;;
;;; TADS mode uses the C mode indentation variables as is, if you want
;;; something different than what you normally use for C, then you can
;;; use the tads-mode-hook and customize the variables (make them local
;;; as well).
;;;
;;; Things will work best if function and object definitions begin in
;;; the first column.  Outside of curly braces, some things might not
;;; work as expected (ie, sexp movement, c-beginning-of-statement, etc).
;;; This is because it is difficult to decipher TADS syntax at times
;;; from within Emacs.
;;;
;;; A big bug is that having two styles of comments isn't well supported
;;; under emacs (FSF), so that if you have quotes or parentheses inside
;;; a comment, things might mess up.  This is most common with single
;;; quotes, since these are so common.  Therefore, you can customize
;;; it such that "//" is not treated as a comment (they're ugly anyway),
;;; by setting the 'tads-no-c++-comment' *before* this file is loaded.
;;;
;;; This is not tested with GNU Emacs version 18, so if you're using
;;; that, you might want to think about upgrading (or hacking this).
;;;
;;; Anyone who wants to improve on this is perfectly free to go ahead.

;;; BUGS:
;;;   - Ok, the two-styles-of-comments as mentioned above in some
;;;     versions of Emacs.  This is exacerbated here because we
;;;     we use sexp movement more than c++ mode.
;;;
;;;   - Other things may have screwy indentation, because of the way
;;;     we go about finding the start of a function or object def.
;;;
;;;   - Things assume version 19 of FSF emacs.


(if (not (boundp 'c-mode-map))		; requires some c-mode stuff
    (load "c-mode" nil t))

;;; Customizable variables (also, the C mode ones can be customized)

(defvar tads-indent-continued-string t
  "*If t (the default), strings continued from the previous line are indented.")

(defvar tads-no-c++-comments nil
  "*Set this if indentation gets horribly confused (because of quotes in
comments and other perfectly reasonable things to do that Emacs can't
quote cope with).  This will cause things to not recognize // comments,
but those are ugly anyway.")

;;; Don't muck with these vars without your Wheaties.

(defvar tads-mode-abbrev-table nil)

(defvar tads-mode-map ()
  "Keymap used in TADS mode.")
(if tads-mode-map
    ()
  (setq tads-mode-map (make-sparse-keymap))
  (define-key tads-mode-map "{" 'electric-tads-brace)
  (define-key tads-mode-map "}" 'electric-tads-brace)
  (define-key tads-mode-map ";" 'electric-tads-semi)
  (define-key tads-mode-map "#" 'electric-tads-sharp-sign)
  (define-key tads-mode-map ":" 'electric-tads-terminator)
  ;; (define-key tads-mode-map "\e\C-h" 'mark-tads-function)
  ;; (define-key tads-mode-map "\e\C-q" 'indent-tads-exp)
  (define-key tads-mode-map "\ea" 'c-beginning-of-statement)
  (define-key tads-mode-map "\ee" 'c-end-of-statement)
  (define-key tads-mode-map "\177" 'backward-delete-char-untabify)
  (define-key tads-mode-map "\t" 'tads-indent-command))

(defvar tads-mode-syntax-table nil
  "Syntax table used in TADS mode.")
(if tads-mode-syntax-table
    ()
  (setq tads-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  (if tads-no-c++-comments
      ()
    (modify-syntax-entry ?/ ". 124" tads-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" tads-mode-syntax-table)
    (modify-syntax-entry ?\n ">" tads-mode-syntax-table)))

(defun tads-mode ()
  "Major mode for editing TADS code.  This is very similar to C mode.
Expression and list commands understand curly brackets.
Tab indents the current line.
Comments are delimited with /* ... */ {or with // ... <newline>}
Paragraphs separated with blank lines only.
Delete converts tabs to spaces as it moves back.
\\{tads-mode-map}
To control indentation style, adjust the c-mode variables, since
TADS mode uses these.  If you want indentation different from C
mode, you can use tads-mode-hook to set them after making them
local variables.

tads-indent-continued-string if set to t (the default), then strings
continued from the previous line will be indented.

Turning on TADS mode calls the value of the variable tads-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tads-mode-map)
  (set-syntax-table tads-mode-syntax-table)
  (setq major-mode 'tads-mode
	mode-name "TADS"
	local-abbrev-table tads-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function) 'tads-indent-line)
  ;;(set (make-local-variable 'indent-region-function) 'tads-indent-region)
  (set (make-local-variable 'require-final-newline) t)
  ;; I hate c++ style comments, so the block mode ones are default
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|// *")
  (set (make-local-variable 'comment-indent-function) 'tads-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (run-hooks 'tads-mode-hook))

(defconst tads-defun-regexp-1
  "compoundWord\\b\\|formatstring\\b\\|specialWords\\b")
(defconst tads-defun-regexp
  ;; This does any number of word, followed by colon, followed by a word.
  (concat
   ;; "\\(modify *\\|replace *\\|class *\\)*\\w+ *: *\\w+"
   "\\w\\(\\w\\| \\)*: *\\w+"		; faster than the above
   "\\|" tads-defun-regexp-1)
  "Regexp that marks the beginning of a TADS function or object.")

;;; Same as c-mode, but pay attention to "//" comments
(defun tads-comment-indent ()
  "Used by indent-for-comment to decide where to indent comment."
  (if (looking-at "^\\(/\\*\\|//\\)")
      0					; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max
       ;; default indent
       comment-column
       ;; non empty lines get at least 1 space before comment
       (if (bolp)
	   0
	 (1+ (current-column)))
       ;; if prev line had comment, use that indentation
       (let ((opoint (point)))
	 (beginning-of-line 0)
	 (if (re-search-forward comment-start-skip opoint t)
	     (progn
	       (goto-char (match-beginning 0))
	       (current-column))
	   0))
       ))))

(defun tads-indent-command (&optional whole-exp)
  "Indent current line as TADS code, or in some cases insert a tab character.
This uses all the indentation customization variables of C-mode."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as TADS
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (tads-indent-line))
            beg end)
        (save-excursion
          (if c-tab-always-indent
              (beginning-of-line))
          ;; Find beginning of following line.
          (save-excursion
            (forward-line 1) (setq beg (point)))
          ;; Find first beginning-of-sexp for sexp extending past this line.
          (while (< (point) beg)
            (forward-sexp 1)
            (setq end (point))
            (skip-chars-forward " \t\n")))
        (if (> end beg)
            (indent-code-rigidly beg end shift-amt "#")))
    ;; else just indent the one line
    (if (and (not c-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
        (insert-tab)
      (tads-indent-line))))

(defun tads-indent-line ()
  "Indent current line as TADS code.
Return the amount the indentation changed by."
  ;; uses some of the c-mode functions
  (let ((indent (calculate-tads-indent nil))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   ;; string
	   (setq indent
		 ;; If tads-indent-continued-string, try to indent
		 ;; to what the start of the string is indented to,
		 ;; otherwise, leave us alone.
		 ;; (might mess up if ' inside "-strings, or vice-versa)
		 (or (save-excursion
		       (and tads-indent-continued-string
			    (re-search-backward "\\s\"" nil t)
			    (current-indentation)))
		     (current-indenation))))
          ((eq indent t)
	   ;; comment
           (setq indent (calculate-c-indent-within-comment)))
          ((looking-at "[ \t]*#")
	   ;; directive
           (setq indent 0))
          (t
           (if (listp indent)
	       (setq indent (car indent))
	     ;; Check special cases (don't do this if indent was a list,
	     ;; since that means we were at the top level, and these
	     ;; cases are only for C-style code)
	     (skip-chars-forward " \t")
	     (cond ((or (looking-at c-switch-label-regexp)
			(and (looking-at "[A-Za-z]")
			     (save-excursion
			       (forward-sexp 1)
			       (looking-at ":"))))
		    (setq indent (max 1 (+ indent c-label-offset))))
		   ((and (looking-at "else\\b")
			 (not (looking-at "else\\s_")))
		    (setq indent (save-excursion
				   (c-backward-to-start-of-if)
				   (current-indentation))))
		   ((looking-at "}[ \t]*else")
		    (setq indent (save-excursion
				   (forward-char)
				   (backward-sexp)
				   (c-backward-to-start-of-if)
				   (current-indentation))))
		   ((and (looking-at "while\\b")
			 (save-excursion
			   (c-backward-to-start-of-do)))
		    ;; This is a `while' that ends a do-while.
		    (setq indent (save-excursion
				   (c-backward-to-start-of-do)
				   (current-indentation))))
		   ((= (following-char) ?})
		    (setq indent (- indent c-indent-level)))
		   ((= (following-char) ?{)
		    (setq indent (+ indent c-brace-offset)))))))
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

;; not at all like C-mode version...
(defun calculate-tads-indent (&optional parse-start)
  "Return appropriate indentation for current line as C code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
If indent is returned inside a list, this means we are at the top
level rather than being C-style code in a function body."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          (case-fold-search nil)
          state
          containing-sexp
	  next-char)
      (if parse-start
          (goto-char parse-start)
        (tads-beginning-of-defun)
	(setq parse-start (point)))
      (while (< (point) indent-point)
        (setq parse-start (point))
        (setq state (parse-partial-sexp (point) indent-point 0))
        (setq containing-sexp (car (cdr state))))
      ;; Now we've got some info, figure out what's up
      ;; State is: (paren-depth inner-list-start last-sexp instring incomment
      ;;            after-quote min-paren-depth)
      (cond ((or (nth 3 state) (nth 4 state))
	     (nth 4 state))		; Comment of string
	    ((null containing-sexp)
	     ;; We're at the top level.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     ;; returning a list, to flag us as top-level
	     (setq next-char (following-char))
	     (list
	      (cond ((or (= next-char ?\;) ; end of object def
			 (looking-at tads-defun-regexp))
		     0)
		    ((progn
		       (tads-backward-to-noncomment parse-start)
		       (= (preceding-char) ?=)) ; continued property def
		     (+ (current-indentation) c-continued-statement-offset))
		    ;; check for start of function def (already checked
		    ;; if we're a continued property def)
		    ((= next-char ?{)
		     0)			; start of function body
		    ((and (= (current-indentation) 0)
			  (memq (preceding-char) '(?\; ?})))
		     ;; just after obj def or func def
		     0)
		    ((save-excursion
		       (beginning-of-line)
		       (looking-at tads-defun-regexp)) ; first line after def'n
		     c-indent-level)
		    (t
		     ;; Normal, non continued line (we hope)
		     ;; so use indentation of prev line (watching out
		     ;; for things that could span multiple lines)
		     (if (memq (preceding-char) '(?\} ?\" ?\'))
			 (progn
			   (backward-sexp 1)
			   (skip-chars-backward " \t\n")))
		     (current-indentation)))))
	    
	    ;; Not at top level - so we go back to doing C stuff
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement (ie, we're
	     ;; inside parens or square brackets, not curlies),
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; We're part of a statement.  Continuation or new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (tads-backward-to-noncomment containing-sexp)
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  c-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (c-backward-to-start-of-continued-exp containing-sexp)
		   (+ c-continued-statement-offset (current-column)
                      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  c-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		;; If no, find that first statement and indent like it.
		(save-excursion
		  (forward-char 1)
		  (while (progn (skip-chars-forward " \t\n")
				(looking-at
				 (concat
				  "#\\|/\\*\\|//"
				  "\\|case[ \t]"
				  "\\|[a-zA-Z0-9_$]*:")))
		    ;; Skip over comments and labels following openbrace.
		    (cond ((= (following-char) ?\#)
			   (forward-line 1))
			  ((= (following-char) ?\#)
			   (forward-char 2)
			   (search-forward "*/" nil 'move))
			  (t
			   (search-forward ":"))))
		  ;; The first following code counts
		  ;; if it is before the line we want to indent.
		  (and (< (point) indent-point)
		       (current-column)))
		;; If no previous statement,
		;; indent it relative to line brace is on.
		;; For open brace in column zero, don't let statement
		;; start there too.  If c-indent-offset is zero,
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

(defun tads-beginning-of-defun ()
  "Move to what we think is start of TADS function or object,
and if not found, startof buffer."
  (beginning-of-line)
  (if (not (looking-at tads-defun-regexp))
      (and (re-search-backward (concat "^" tads-defun-regexp) nil 'move)
	   (goto-char (match-beginning 0)))))

(defun tads-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (setq opoint (point))
      (cond ((and (>= (point) (+ 2 lim))
		  (save-excursion
		    (forward-char -2)
		    (looking-at "\\*/")))
	     (search-backward "/*" lim 'move))
	    ((search-backward "//" (max lim (save-excursion
					      (beginning-of-line)
					      (point)))
			      'move))
	    (t (beginning-of-line)
	       (skip-chars-forward " \t")
	       (if (looking-at "#")
		   (setq stop (<= (point) lim))
		 (setq stop t)
		 (goto-char opoint)))))))

;; tells if we're at top level (or inside braces)
(defun tads-top-level ()
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  state)
      (tads-beginning-of-defun)
      (while (< (point) opoint)
	(setq state (parse-partial-sexp (point) opoint 0)))
      (null (car (cdr state))))))

;;; Electric commands

(defun electric-tads-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if c-auto-newline
		     (progn (tads-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (tads-indent-line)
	  (if c-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(tads-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-c-sharp-sign (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (save-excursion
	(skip-chars-backward " \t")
	(bolp))
      (let ((c-auto-newline nil))
	(electric-tads-terminator arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-tads-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-tads-terminator arg)
    (self-insert-command (prefix-numeric-value arg))
    (if (tads-top-level) (tads-indent-line))))

(defun electric-tads-terminator (arg)
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
			     (not (looking-at c-switch-label-regexp))
			     (save-excursion
			       (skip-chars-forward "a-zA-Z0-9_$")
			       (skip-chars-forward " \t")
			       (< (point) end)))
			(progn
			  (tads-beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (tads-indent-line)
	  (and c-auto-newline
	       (not (c-inside-parens-p))
	       (progn
		 (newline)
		 ;; (newline) may have done auto-fill
		 (setq insertpos (- (point) 2))
		 (tads-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

;;; Can use this stuff for font-lock mode.  (gnu version 19)
(defun tads-font-hook ()
  (if (eq major-mode 'tads-mode)
      (setq font-lock-keywords
	    (list
	     ;; preprocessor directives as comments.
	     '("^#[ \t]*[a-z]+" . font-lock-comment-face)
	     '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
	       1 font-lock-string-face)
	     ;; obj/func defs
	     '("^\\(\\w+[ \t]+\\)*\\(\\w+\\) *: *\\w+"
	       2 font-lock-function-name-face)
	     ;; others
	     (list (concat "^\\(" tads-defun-regexp-1 "\\)")
		   1 'font-lock-function-name-face)
	     ))))
;; (add-hook 'font-lock-mode-hook 'tads-font-hook)
