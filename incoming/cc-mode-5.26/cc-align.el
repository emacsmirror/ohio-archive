;;; cc-align.el --- custom indentation functions for CC Mode

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Authors:    1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-current-file)
		  (stringp byte-compile-current-file))
	     (cons (file-name-directory byte-compile-current-file)
		   load-path)
	   load-path)))
    (load "cc-defs" nil t)))
(require 'cc-engine)


;; Standard indentation line-ups

(defun c-lineup-arglist (langelem)
  "Line up the current argument line under the first argument.

Works with: arglist-cont-nonempty."
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is c-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (c-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (langelem-col (c-langelem-col langelem t)))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (c-forward-sexp -1)
		 (forward-char 1)
		 (c-forward-syntactic-ws)
		 (- (current-column) langelem-col))
	(goto-char containing-sexp)
	(or (eolp)
	    (not (memq (char-after) '(?{ ?\( ?\[)))
	    (let ((eol (c-point 'eol))
		  (here (progn
			  (forward-char 1)
			  (skip-chars-forward " \t")
			  (point))))
	      (c-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) langelem-col)
	))))

(defun c-lineup-arglist-intro-after-paren (langelem)
  "Line up a line just after the open paren of the surrounding paren or
brace block.

Works with: defun-block-intro, brace-list-intro,
statement-block-intro, statement-case-intro, arglist-intro."
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem t))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (c-point 'eol))
		       (current-column))))
      (- ce-curcol langelem-col -1))))

(defun c-lineup-arglist-close-under-paren (langelem)
  "Line up a closing paren line under the corresponding open paren.

Works with: defun-close, class-close, inline-close, block-close,
brace-list-close, arglist-close, extern-lang-close, namespace-close
\(for most of these, a zero offset will normally produce the same
result, though)."
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem t))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (current-column))))
      (- ce-curcol langelem-col))))

(defun c-lineup-close-paren (langelem)
  "Line up the closing paren under its corresponding open paren if the
open paren is followed by code.  If the open paren ends its line, no
indentation is added.  E.g:

main (int,              main (
      char **               int, char **
     )           <->    )                 <- c-lineup-close-paren

Works with: defun-close, class-close, inline-close, block-close,
brace-list-close, arglist-close, extern-lang-close, namespace-close."
  (save-excursion
    (condition-case nil
	(let (opencol spec)
	  (beginning-of-line)
	  (backward-up-list 1)
	  (setq spec (c-looking-at-special-brace-list))
	  (if spec (goto-char (car (car spec))))
	  (setq opencol (current-column))
	  (forward-char 1)
	  (if spec (progn
		     (c-forward-syntactic-ws)
		     (forward-char 1)))
	  (c-forward-syntactic-ws (c-point 'eol))
	  (if (eolp)
	      0
	    (- opencol (c-langelem-col langelem t))))
      (error nil))))

(defun c-lineup-streamop (langelem)
  "Line up C++ stream operators under each other.

Works with: stream-op."
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem)))
      (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
      (goto-char (match-beginning 0))
      (- (current-column) langelem-col))))

(defun c-lineup-multi-inher (langelem)
  "Line up the classes in C++ multiple inheritance clauses under each other.

Works with: inher-cont."
  (save-excursion
    (let ((eol (c-point 'eol))
	  (here (point))
	  (langelem-col (c-langelem-col langelem)))
      (skip-chars-forward "^:" eol)
      (skip-chars-forward " \t:" eol)
      (if (or (eolp)
	      (looking-at c-comment-start-regexp))
	  (c-forward-syntactic-ws here))
      (- (current-column) langelem-col)
      )))

(defun c-lineup-java-inher (langelem)
  "Line up Java implements and extends declarations.
If class names follows on the same line as the implements/extends
keyword, they are lined up under each other.  Otherwise, they are
indented by adding `c-basic-offset' to the column of the keyword.
E.g:

class Foo             class Foo
    extends               extends Cyphr,
        Bar    <->                Bar     <- c-lineup-java-inher
    <--> c-basic-offset

Works with: inher-cont."
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem)))
      (forward-word 1)
      (if (looking-at "[ \t]*$")
	  c-basic-offset
	(c-forward-syntactic-ws)
	(- (current-column) langelem-col)))))

(defun c-lineup-java-throws (langelem)
  "Line up Java throws declarations.
If exception names follows on the same line as the throws keyword,
they are lined up under each other.  Otherwise, they are indented by
adding `c-basic-offset' to the column of the throws keyword.  The
throws keyword itself is also indented by `c-basic-offset' from the
function declaration start if it doesn't hang.  E.g:

int foo()           int foo() throws Cyphr,
    throws     <->                   Bar,    <- c-lineup-java-throws
        Bar    <->                   Vlod    <- c-lineup-java-throws
<--><--> c-basic-offset

Works with: func-decl-cont."
  (save-excursion
    (let* ((lim (1- (c-point 'bol)))
	   (throws (catch 'done
		     (goto-char (cdr langelem))
		     (while (zerop (c-forward-token-1 1 t lim))
		       (if (looking-at "throws\\>[^_]")
			   (throw 'done t))))))
      (if throws
	  (if (zerop (c-forward-token-1 1 nil (c-point 'eol)))
	      (- (current-column) (c-langelem-col langelem))
	    (back-to-indentation)
	    (+ (- (current-column) (c-langelem-col langelem))
	       c-basic-offset))
	c-basic-offset))))

(defun c-indent-one-line-block (langelem)
  "Indent a one line block `c-basic-offset' extra.
E.g:

if (n > 0)                 if (n > 0)
    {m+=n; n=0;}    <->    {               <- c-indent-one-line-block
<--> c-basic-offset            m+=n; n=0;
                           }

The block may be surrounded by any kind of parenthesis characters.
nil is returned if the line doesn't start with a one line block, which
makes the function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (c-safe (progn (c-forward-sexp) t))
	       (<= (point) eol))
	  c-basic-offset
	nil))))

(defun c-indent-multi-line-block (langelem)
  "Indent a multi line block `c-basic-offset' extra.
E.g:

int *foo[] = {           int *foo[] = {
    NULL,                    NULL,
    {17},         <->            {       <- c-indent-multi-line-block
                                 17
                                 }
                             <--> c-basic-offset

The block may be surrounded by any kind of parenthesis characters.
nil is returned if the line doesn't start with a multi line block,
which makes the function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (or (not (c-safe (progn (c-forward-sexp) t)))
		   (> (point) eol)))
	  c-basic-offset
	nil))))

(defun c-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Various heuristics are used to handle most of the common comment
styles.  Some examples:

/*          /**         /*         /* text      /*          /**
 * text      * text       text        text      ** text      ** text
 */          */         */         */           */           */

/*********************************************************************
 * text
 ********************************************************************/

/*********************************************************************
    Free form text comments:
 In comments with a long delimiter line at the start, the indentation
 is kept unchanged for lines that start with an empty comment line
 prefix.  The delimiter line is whatever matches the
 `comment-start-skip' regexp.
*********************************************************************/

The variable `c-comment-prefix-regexp' is used to recognize the
comment line prefix, e.g. the `*' that usually starts every line
inside a comment.

Works with: The `c' syntactic symbol."
  (save-excursion
    (let* ((here (point))
	   (prefixlen (progn (back-to-indentation)
			     (if (looking-at c-comment-prefix-regexp)
				 (- (match-end 0) (point))
			       0)))
	   (starterlen (save-excursion
			 (goto-char (cdr langelem))
			 (looking-at comment-start-skip)
			 (- (save-excursion
			      (goto-char (match-end 0))
			      (skip-chars-backward " \t")
			      (point))
			    (or (match-end 1) (point))
			    1)))	; Don't count the first '/'.
	   (langelem-col (save-excursion (c-langelem-col langelem))))
      (if (and (> starterlen 10) (zerop prefixlen))
	  ;; The comment has a long starter and the line doesn't have
	  ;; a nonempty comment prefix.  Treat it as free form text
	  ;; and don't change the indentation.
	  (- (current-column) langelem-col)
	(forward-line -1)
	(back-to-indentation)
	(if (>= (cdr langelem) (point))
	    ;; On the second line in the comment.
	    (if (zerop prefixlen)
		;; No nonempty comment prefix. Align after comment
		;; starter.
		(progn
		  (goto-char (match-end 0))
		  (if (looking-at "\\([ \t]+\\).+$")
		      ;; Align with the text that hangs after the
		      ;; comment starter.
		      (goto-char (match-end 1)))
		  (- (current-column) langelem-col))
	      ;; How long is the comment starter?  if greater than the
	      ;; length of the comment prefix, align left.  if less
	      ;; than or equal, align right.  this should also pick up
	      ;; Javadoc style comments.
	      (if (> starterlen prefixlen)
		  (progn
		    (goto-char (cdr langelem))
		    (- (current-column) -1 langelem-col))
		(goto-char (match-end 0))
		(skip-chars-backward " \t")
		(- (current-column) prefixlen langelem-col)))
	  ;; Not on the second line in the comment.  If the previous
	  ;; line has a nonempty comment prefix, align with it.
	  ;; Otherwise, align with the previous nonempty line, but
	  ;; align the comment ender with the starter.
	  (when (or (not (looking-at c-comment-prefix-regexp))
		    (eq (match-beginning 0) (match-end 0)))
	    (goto-char here)
	    (back-to-indentation)
	    (if (looking-at (concat "\\(" c-comment-prefix-regexp "\\)\\*/"))
		(goto-char (cdr langelem))
	      (while (and (zerop (forward-line -1))
			  (looking-at "^[ \t]*$")))
	      (back-to-indentation)
	      (if (< (point) (cdr langelem))
		  ;; Align with the comment starter rather than
		  ;; with the code before it.
		  (goto-char (cdr langelem)))))
	  (- (current-column) langelem-col))))))

(defun c-lineup-comment (langelem)
  "Line up a comment start according to `c-comment-only-line-offset'.
If the comment is lined up with a comment starter on the previous
line, that alignment is preserved.

Works with: comment-intro."
  (save-excursion
    (back-to-indentation)
    ;; this highly kludgiforous flag prevents the mapcar over
    ;; c-syntactic-context from entering an infinite loop
    (let ((recurse-prevention-flag (boundp 'recurse-prevention-flag))
	  (col (current-column)))
      (cond
       (recurse-prevention-flag 0)
       ;; CASE 1: preserve aligned comments
       ((save-excursion
	  (and (c-forward-comment -1)
	       (= col (current-column))))
	;; we have to subtract out all other indentation
	(- col (apply '+ (mapcar 'c-get-offset
				 c-syntactic-context))))
       ;; indent as specified by c-comment-only-line-offset
       ((not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset))
       (t
	(or (cdr-safe c-comment-only-line-offset)
	    (car-safe c-comment-only-line-offset)
	    -1000))			;jam it against the left side
       ))))

(defun c-lineup-runin-statements (langelem)
  "Line up statements when the first statement is on the same line as
the block opening brace.  E.g:

int main()
{ puts (\"Hello world!\");
  return 0;                 <- c-lineup-runin-statements
}

If there is no statement after the opening brace to align with, nil is
returned.  This makes the function usable in list expressions.

Works with: The `statement' syntactic symbol."
  (if (eq (char-after (cdr langelem)) ?{)
      (save-excursion
	(let ((langelem-col (c-langelem-col langelem)))
	  (forward-char 1)
	  (skip-chars-forward " \t")
	  (unless (eolp)
	    (- (current-column) langelem-col))))))

(defun c-lineup-math (langelem)
  "Line up the current line after the equal sign on the first line in
the statement.  If there isn't any, indent with `c-basic-offset'.  If
the current line contains an equal sign too, try to align it with the
first one.

Works with: statement-cont."
  (save-excursion
    (let ((equalp (save-excursion
		    (goto-char (c-point 'boi))
		    (skip-chars-forward "^=" (c-point 'eol))
		    (and (eq (char-after) ?=)
			 (- (point) (c-point 'boi)))))
	  (langelem-col (c-langelem-col langelem))
	  donep)
      (while (and (not donep)
		  (< (point) (c-point 'eol)))
	(skip-chars-forward "^=" (c-point 'eol))
	(if (c-in-literal (cdr langelem))
	    (forward-char 1)
	  (setq donep t)))
      (if (or (not (eq (char-after) ?=))
	      (save-excursion
		(forward-char 1)
		(c-forward-syntactic-ws (c-point 'eol))
		(eolp)))
	  ;; there's no equal sign on the line
	  c-basic-offset
	;; calculate indentation column after equals and ws, unless
	;; our line contains an equals sign
	(if (not equalp)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t")
	      (setq equalp 0)))
	(- (current-column) equalp langelem-col))
      )))

(defun c-lineup-template-args (langelem)
  "Line up template argument lines under the first argument.
To allow this function to be used in a list expression, nil is
returned if there's no template argument on the first line.

Works with: template-args-cont."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (backward-up-list 1)
      (if (and (eq (char-after) ?<)
	       (zerop (c-forward-token-1 1 nil (c-point 'eol))))
	  (- (current-column) (c-langelem-col langelem))))))

(defun c-lineup-ObjC-method-call (langelem)
  "Line up selector args as elisp-mode does with function args:
Go to the position right after the message receiver, and if you are at
the end of the line, indent the current line c-basic-offset columns
from the opening bracket; otherwise you are looking at the first
character of the first method call argument, so lineup the current
line with it.

Works with: objc-method-call-cont."
  (save-excursion
    (let* ((extra (save-excursion
		    (back-to-indentation)
		    (c-backward-syntactic-ws (cdr langelem))
		    (if (eq (char-before) ?:)
			(- c-basic-offset)
		      0)))
	   (open-bracket-pos (cdr langelem))
           (open-bracket-col (progn
			       (goto-char open-bracket-pos)
			       (current-column)))
           (target-col (progn
			 (forward-char)
			 (c-forward-sexp)
			 (skip-chars-forward " \t")
			 (if (eolp)
			     (+ open-bracket-col c-basic-offset)
			   (current-column))))
	   )
      (- target-col open-bracket-col extra))))

(defun c-lineup-ObjC-method-args (langelem)
  "Line up the colons that separate args.
The colon on the current line is aligned with the one on the first
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (first-col-column (progn
			       (goto-char relpos)
			       (skip-chars-forward "^:" eol)
			       (and (eq (char-after) ?:)
				    (current-column)))))
      (if (not first-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- first-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-ObjC-method-args-2 (langelem)
  "Line up the colons that separate args.
The colon on the current line is aligned with the one on the previous
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (prev-col-column (progn
			      (skip-chars-backward "^:" relpos)
			      (and (eq (char-before) ?:)
				   (- (current-column) 1)))))
      (if (not prev-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- prev-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-inexpr-block (langelem)
  "Line up the block for constructs that use a block inside an expression,
e.g. anonymous classes in Java and lambda functions in Pike.  The body
is aligned with the start of the header, e.g. with the \"new\" or
\"lambda\" keyword.  Returns nil if the block isn't part of such a
construct.

Works with: inlambda, inexpr-statement, inexpr-class."
  (save-excursion
    (back-to-indentation)
    (let ((res (or (c-looking-at-inexpr-block)
		   (if (c-safe (backward-up-list 1)
			       (eq (char-after) ?{))
		       (c-looking-at-inexpr-block)))))
      (when res
	(goto-char (cdr res))
	(- (current-column)
	   (progn
	     (back-to-indentation)
	     (current-column)))))))

(defun c-lineup-whitesmith-in-block (langelem)
  "Line up lines inside a block in whitesmith style.
It's done in a way that works both when the opening brace hangs and
when it doesn't.  E.g:

something
    {                something {
    foo;     <->         foo;     <- c-lineup-whitesmith-in-block
    }                    }
                     <--> c-basic-offset

In the first case the indentation is kept unchanged, in the
second `c-basic-offset' is added.

Works with: defun-close, defun-block-intro, block-close,
brace-list-close, brace-list-intro, statement-block-intro, inclass,
inextern-lang, innamespace."
  (save-excursion
    (goto-char (cdr langelem))
    (back-to-indentation)
    (if (eq (char-syntax (char-after)) ?\()
	0
      c-basic-offset)))

(defun c-lineup-dont-change (langelem)
  "Do not change the indentation of the current line.

Works with: Any syntactic symbol."
  (save-excursion
    (back-to-indentation)
    (- (current-column) (c-langelem-col langelem))))


(defun c-snug-do-while (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'block-close)
	       (setq langelem (assq 'block-close c-syntactic-context))
	       (progn (goto-char (cdr langelem))
		      (if (eq (char-after) ?{)
			  (c-safe (c-forward-sexp -1)))
		      (looking-at "\\<do\\>[^_]")))
	  '(before)
	'(before after)))))

(defun c-gnu-impose-minimum ()
  "Imposes a minimum indentation for lines inside a top-level construct.
The variable `c-label-minimum-indentation' specifies the minimum
indentation amount."
  (let ((non-top-levels '(defun-block-intro statement statement-cont
			   statement-block-intro statement-case-intro
			   statement-case-open substatement substatement-open
			   case-label label do-while-closure else-clause
			   ))
	(syntax c-syntactic-context)
	langelem)
    (while syntax
      (setq langelem (car (car syntax))
	    syntax (cdr syntax))
      ;; don't adjust comment-only lines
      (cond ((eq langelem 'comment-intro)
	     (setq syntax nil))
	    ((memq langelem non-top-levels)
	     (save-excursion
	       (setq syntax nil)
	       (back-to-indentation)
	       (if (zerop (current-column))
		   (insert (make-string c-label-minimum-indentation 32)))
	       ))
	    ))))


;; Useful for c-hanging-semi&comma-criteria

(defun c-semi&comma-inside-parenlist ()
  "Controls newline insertion after semicolons in parenthesis lists.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (not (eq last-command-char ?\;))
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (not (eq (char-after) ?\()))
	  (error t))
	t
      'stop)))

;; Suppresses newlines before non-blank lines
(defun c-semi&comma-no-newlines-before-nonblanks ()
  "Controls newline insertion after semicolons.
If a comma was inserted, no determination is made.  If a semicolon was
inserted, and the following line is not blank, no newline is inserted.
Otherwise, no determination is made."
  (save-excursion
    (if (and (= last-command-char ?\;)
	     ;;(/= (point-max)
	     ;;    (save-excursion (skip-syntax-forward " ") (point))
	     (zerop (forward-line 1))
	     (not (looking-at "^[ \t]*$")))
	'stop
      nil)))

;; Suppresses new lines after semicolons in one-liners methods
(defun c-semi&comma-no-newlines-for-oneline-inliners ()
  "Controls newline insertion after semicolons for some one-line methods.
If a comma was inserted, no determination is made.  Newlines are
suppressed in one-liners, if the line is an in-class inline function.
For other semicolon contexts, no determination is made."
  (let ((syntax (c-guess-basic-syntax))
        (bol (save-excursion
               (if (c-safe (up-list -1) t)
                   (c-point 'bol)
                 -1))))
    (if (and (eq last-command-char ?\;)
             (eq (car (car syntax)) 'inclass)
             (eq (car (car (cdr syntax))) 'topmost-intro)
             (= (c-point 'bol) bol))
        'stop
      nil)))


(provide 'cc-align)
;;; cc-align.el ends here
