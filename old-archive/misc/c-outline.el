;;; c-outline.el --- outline-minor-mode support for C program editing.

;; Copyright (C) 1994 Kevin Broadey.

;; Author: Kevin Broadey <KevinB@bartley.demon.co.uk>
;; Created: 15 March 1994
;; Version: c-outline.el 1.9 dated 94/11/24 at 12:33:57
;; Keywords: C, c-mode, outline

;; LCD Archive Entry:
;; c-outline|Kevin Broadey|KevinB@bartley.demon.co.uk|
;; outline-minor-mode support for C program editing|
;; 24-Nov-1994|1.9|~/misc/c-outline.el.Z|

;; This file is not part of GNU Emacs, but it is distributed under the same
;; conditions.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; c-outline.el tailors the outline-minor-mode variables `outline-regexp' and
;; `outline-level' so that `C' statements are treated as outline headings.
;; This allows you to use outline-minor-mode commands to hide and expose blocks
;; of `C' code, provided the code is indented in a consistent manner.
;;
;; To use c-outline.el in a c-mode buffer simply type `M-x c-outline RET'.
;; To turn off outline-minor-mode, do `M-x outline-minor-mode'.

;;; Installation:

;; To use c-outline, put this in your .emacs:-
;;
;;    (autoload 'c-outline "c-outline" nil t)
;;
;; If you want to use c-outline in all your c-mode buffers, add this too:-
;;
;;    (add-hook 'c-mode-hook 'c-outline)
;;
;; c-outline's `outline-level' function looks for changes in the
;; `outline-regexp' variable made after it has been installed -- for example
;; when it is set in the "Local Variables:" section of a source file.  If it
;; detects a change it automatically reinstates the default `outline-level'
;; function.


;;; Advertisements:

;; Get out-xtra.el by Per Abrahamsen <abraham@iesd.auc.dk> for more
;; outline-mode goodies.  In particular, `outline-hide-sublevels' makes
;; setup a lot easier.
;;
;; foldout.el (by me) provides folding editor extensions for outline-mode and
;; outline-minor-mode, so with c-outline you can have a folding `C' code editor
;; without having to put in start- and end-of-fold markers.  This is a real
;; winner!
;;
;; folding.el by Jamie Lokier <u90jl@ecs.ox.ac.uk> supports folding by
;; recognising special marker text in you file.

;;; ChangeLog:

;; 1.9    24-Nov-94
;; Changed copyright statement to reference GPL version 2.  Thanks RMS.

;; 1.8    23-Nov-94
;; Install a special `outline-level' function that checks for changes made to
;; `outline-regexp' after `c-outline' has been run.  This allows the variable
;; to be set in the "Local Variables:" section without causing upset.

;; 1.7	  9-May-94
;; Refine previous fix.  *Do* add on c-outline-block-offset for "{" if previous
;; line is a comment with the same indentation.  This makes it hide under the
;; comment in the following situation:-
;;
;;	     /* comment for in-line block */
;;	     {
;;	       blah_blah_blah();
;;	     }

;; 1.6    6-May-94
;; Fix bug in level for "{".  If the previous line is a comment, don't add on
;; c-outline-block-offset as the brace is the start of an inline block.

;; 1.5    15-Apr-94
;; Make `c-outline-block-offset' automatically buffer-local when it is set
;; rather than setting it in individual buffers so the user can use
;; setq-default and get the expected behaviour.

;; 1.4    8-Apr-94
;; Need to (require 'outline) otherwise the default value of outline-regexp and
;; outline-level is NIL.

;; 1.3    23-Mar-94
;; Change level for "{" again.  Make it indentation + c-indent-level most of
;; the time, but change it to current indentation if previous line is blank or
;; indented more than indentation + c-indent-level.  This stops in-line blocks
;; getting hidden under the preceding statement, and gets around a problem with
;; `show-children' on a function heading.  If the function is like this:-
;;
;;     void func (int arg1,
;;		  int arg2)
;;     {
;;       if (something)
;;       {
;;         do-this;
;;       }
;;       else
;;       {
;;	   do-that;
;;       }
;;     }
;;
;; then previously, `show-children' on the heading would expose the entire
;; function.

;; 1.2    22-Mar-94
;; Changed level for "{" to be indentation + c-indent-level so that:-
;;
;;     if (expression)
;;     {
;;       statement;
;;     }
;;     else
;;     {
;;       statement;
;;     }
;;
;; folds up to:-
;;
;;     if (expression)...
;;     else..
;;
;; and doing `show-children' on the "if" or "else" shows the entire block in
;; one go.  The only problem with this is that in-line blocks will get hidden
;; under the preceding statement.  Get around this by preceding them by a
;; comment - they'll then be hidden under the comment:-
;;
;;     func (arg)
;;     {
;;       statement;
;;       statement;
;;
;;       /* comment for scoping block */
;;
;;       {
;;         int i;
;;
;;         statement_using_i;
;;       }
;;     }
;;

;; 1.1	  15-Mar-94
;; Released to the net.

;;; required packages

(require 'outline)

;;; Code:

(defconst c-outline-regexp "[ \t]*\\([^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)"
  "Regexp matching an outline heading in a c-mode buffer.

This matches everything except blank lines and those that look like they're
part of a milti-line block comment -- i.e. they begin with one or more \"*\"s
but don't look like a pointer dereference.")

(defconst c-outline-statement-regexp "[\n\^M][ \t]*[^*#/ \t\n\^M]"
  "Regexp matching a non-comment, non-directive heading.

This is used to find the heading to use for to get the level for a comment
in column zero or a preprocessor directive.")

(defvar c-outline-block-offset
  (if (boundp 'c-basic-offset)
      c-basic-offset			; cc-mode
    c-indent-level)			; c-mode
  "*Amount by which code following an open brace is indented.
Automatically becomes buffer-local when set in any fashion.")
(make-variable-buffer-local 'c-outline-block-offset)

(defun c-outline-install (wanted-outline-level wanted-outline-regexp)
  "Set up `outline-level' and `outline-regexp' for c-outline.
The value put into `outline-level' is not the real function.  Instead it
is a LISP expression that checks whether `outline-regexp' has been changed.
If it has the original value of `outline-level' is restored, otherwise the real
c-outline function is installed."
  (set (make-local-variable 'outline-regexp) wanted-outline-regexp)
  (set (make-local-variable 'outline-level)
	(list 'lambda '()
	      (list 'if (list 'string= 'outline-regexp wanted-outline-regexp)
		    (list 'setq 'outline-level
			  (list 'quote wanted-outline-level))
		    (list 'kill-local-variable (list 'quote 'outline-level)))
	      (list 'funcall 'outline-level))))

(defun c-outline ()
  "Set up outline-minor-mode in a c-mode buffer."
  (interactive)
  (c-outline-install 'c-outline-level c-outline-regexp)
  (outline-minor-mode 1))

(defsubst c-outline-find-statement (soh-pos)
  "Find a non-comment, non-directive heading.
Searches forwards first, then goes backward if none is found."
  (if (re-search-forward c-outline-statement-regexp nil t)
      (goto-char (1+ (match-beginning 0)))
    (goto-char (1- soh-pos))
    (if (re-search-backward c-outline-statement-regexp nil 'move)
	(goto-char (1+ (match-beginning 0))))))

(defun c-outline-level ()
  (let (soh-pos indentation soh-char level)
    (setq soh-pos (point)
	  indentation (- (- (current-column)
			    (progn (skip-chars-forward " \t")
				   (current-column))))
	  soh-char (char-after (point)))
    (setq level
	  (cond
	   ;; at beginning of buffer level is zero - can't use (bobp) to check
	   ;; this, though - I've just skipped the whitespace!
	   ((= soh-pos (point-min))
	    0)

	   ;; for statements level is current indentation - this is everything
	   ;; except comments, preprocessor directives and braces
	   ((not (memq soh-char '(?/ ?# ?{ ?})))
	    indentation)

	   ;; comments, and lines beginning with "/"
	   ((= soh-char ?/)
	    (if (/= ?* (char-after (1+ (point))))
		;; slash not followed by asterisk - this is a statement line,
		;; not a comment after all
		indentation
	      (goto-char (1- soh-pos))
	      (if (re-search-backward "[\n\^M]" nil 'move)
		  (forward-char 1))
	      (cond
	       ;; comment after comment - bury it
	       ((looking-at "[ \t]*/\\*")
		1000)

	       ;; comment at beginning-of-line - same level as next or previous
	       ;; statement
	       ((zerop indentation)
		(c-outline-find-statement soh-pos)
		(c-outline-level))

	       ;; comment not at beginning of line - level is current
	       ;; indentation
	       (t
		indentation))))

	   ;; for open braces level is indentation if previous line is indented
	   ;; more, is blank, or is a comment, and is indentation + block
	   ;; offset otherwise
	   ((= soh-char ?{)
	    (goto-char (1- soh-pos))
	    (if (re-search-backward "[\n\^M]" nil 'move)
		(forward-char 1))
	    (let ((prev-indentation (- (- (current-column)
					  (progn
					    (skip-chars-forward " \t")
					    (current-column))))))
	      (if (or
		   ;; is prev line indented more?
		   (> prev-indentation
			 (+ indentation c-outline-block-offset))
		   ;; is prev line blank?
		   (= (char-syntax (char-after (point))) ?\ )
		   ;; is prev line a comment with different indentation?
		   (and (looking-at "/\\*")
			(/= prev-indentation indentation)))
		  indentation
		(+ indentation c-outline-block-offset))))

	   ;; for close braces level is indentation + block offset
	   ((= soh-char ?})
	    (+ indentation c-outline-block-offset))

	   ;; preprocessor directives
	   ((= soh-char ?#)
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (cond
	     ;; level for #define is zero
	     ((looking-at "define")
	      0)

	     ;; level for #else, #elif, #endif is same as preceding #if
	     ((looking-at "e\\(ndif\\|l\\(se\\|if\\)\\)")
	      (if (re-search-backward "[\n\^M]#[ \t]*if" nil 'move)
		  (progn
		    (goto-char (1+ (match-beginning 0)))
		    (c-outline-level))
		;; make unmatched directives stand out like a sore thumb!
		-1))

	     ;; bury #include
	     ((looking-at "include")
	      1000)

	     ;; level for other directives is same as next or previous
	     ;; statement
	     (t
	      (c-outline-find-statement soh-pos)
	      (c-outline-level))))

	   ;; dunno what this is - give it the same level as a statement
	   ;; to be on the safe side
	   (t
	    indentation)
	   ))

    ;; go back to the beginning of the heading - this is quicker than using
    ;; save-excursion
    (goto-char soh-pos)

    ;; and the outline level is...
    level
    ))

(provide 'c-outline)

;;; c-outline.el ends here
