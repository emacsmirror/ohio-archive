;;; cc-cmds.el --- user level commands for CC Mode

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


(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;; Auto-newline and hungry-delete
(defun c-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a' or
`/ah' on the modeline after the mode name) newlines are automatically
inserted after special characters such as brace, comma, semi-colon,
and colon."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h' or `/ah' on the modeline after the mode name) the delete key
gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `c-toggle-auto-state' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))


;; Electric keys

;; Note: In XEmacs 20.3 the Delete and BackSpace keysyms have been
;; separated and "\177" is no longer an alias for both keys.  Also,
;; the variable delete-key-deletes-forward controls in which direction
;; the Delete keysym deletes characters.  The functions
;; c-electric-delete and c-electric-backspace attempt to deal with
;; this new functionality.  For Emacs 19 and XEmacs 19 backwards
;; compatibility, the old behavior has moved to c-electric-backspace
;; and c-backspace-function.

(defun c-electric-backspace (arg)
  "Deletes preceding character or whitespace.
If `c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-backspace-function' is called.

See also \\[c-electric-delete]."
  (interactive "*P")
  (if (or (not c-hungry-delete-key)
	  arg
	  (c-in-literal))
      (funcall c-backspace-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c-backspace-function 1)
	))))

(defun c-electric-delete (arg)
  "Deletes preceding or following character or whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen), then this function behaves identical to
\\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction.  So if
`c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all following whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-delete-function' is called."
  (interactive "*P")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
	       (delete-forward-p))
	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
	       delete-key-deletes-forward))
      (if (or (not c-hungry-delete-key)
	      arg
	      (c-in-literal))
	  (funcall c-delete-function (prefix-numeric-value arg))
	(let ((here (point)))
	  (skip-chars-forward " \t\n")
	  (if (/= (point) here)
	      (delete-region (point) here)
	    (funcall c-delete-function 1))))
    ;; act just like c-electric-backspace
    (c-electric-backspace arg)))

(defun c-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "*P")
  (if (or arg
	  (not (memq 'alignleft c-electric-pound-behavior))
	  (save-excursion (skip-chars-backward " \t") (not (bolp)))
	  (c-in-literal))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-electric-brace (arg)
  "Insert a brace.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after braces based on the value of `c-hanging-braces-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the brace, or
the brace is inserted inside a literal.

This function does various newline cleanups based on the value of
`c-cleanup-list'."
  (interactive "*P")
  (let* ((c-state-cache (c-parse-state))
	 (safepos (c-safe-position (point) c-state-cache))
	 (literal (c-in-literal safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      (let* ((syms
	      ;; This is the list of brace syntactic symbols that can
	      ;; hang.  If any new ones are added to c-offsets-alist,
	      ;; they should be added here as well.
	      '(class-open class-close defun-open defun-close
		inline-open inline-close
		brace-list-open brace-list-close
		brace-list-intro brace-entry-open
		block-open block-close
		substatement-open statement-case-open
		extern-lang-open extern-lang-close
		namespace-open namespace-close
		inexpr-class-open inexpr-class-close
		))
	    ;; we want to inhibit blinking the paren since this will
	    ;; be most disruptive. we'll blink it ourselves later on
	    (old-blink-paren blink-paren-function)
	    blink-paren-function
	    (insertion-point (point))
	    delete-temp-newline
	    (preserve-p (and (not (bobp))
			     (eq ?\  (char-syntax (char-before)))))
	    ;; shut this up too
	    (c-echo-syntactic-information-p nil)
	    (syntax (progn
		      ;; only insert a newline if there is
		      ;; non-whitespace behind us
		      (if (save-excursion
			    (skip-chars-backward " \t")
			    (not (bolp)))
			  (progn (newline)
				 (setq delete-temp-newline t)))
		      (self-insert-command (prefix-numeric-value arg))
		      ;; state cache doesn't change
		      (c-guess-basic-syntax)))
	    (newlines (and
		       c-auto-newline
		       (or (c-lookup-lists
			    syms
			    ;; Substitute inexpr-class and class-open
			    ;; or class-close with inexpr-class-open
			    ;; or inexpr-class-close.
			    (if (assq 'inexpr-class syntax)
				(cond ((assq 'class-open syntax)
				       '((inexpr-class-open)))
				      ((assq 'class-close syntax)
				       '((inexpr-class-close)))
				      (t syntax))
			      syntax)
			    c-hanging-braces-alist)
			   '(ignore before after)))))
	;; Do not try to insert newlines around a special (Pike-style)
	;; brace list.
	(if (and c-special-brace-lists
		 (c-intersect-lists '(brace-list-open brace-list-close
				      brace-list-intro brace-entry-open)
				    syntax)
		 (save-excursion
		   (c-safe (if (= (char-before) ?{)
			       (forward-char -1)
			     (c-forward-sexp -1))
			   (c-looking-at-special-brace-list))))
	    (setq newlines nil))
	;; If syntax is a function symbol, then call it using the
	;; defined semantics.
	(if (and (not (consp (cdr newlines)))
		 (functionp (cdr newlines)))
	    (let ((c-syntactic-context syntax))
	      (setq newlines
		    (funcall (cdr newlines) (car newlines) insertion-point))))
	;; does a newline go before the open brace?
	(if (memq 'before newlines)
	    ;; we leave the newline we've put in there before,
	    ;; but we need to re-indent the line above
	    (let ((pos (- (point-max) (point)))
		  (here (point)))
	      (forward-line -1)
	      (let ((c-state-cache (c-whack-state (point) c-state-cache)))
		;; we may need to update the cache. this should
		;; still be faster than recalculating the state
		;; in many cases
		(save-excursion
		  (save-restriction
		    (narrow-to-region here (point))
		    (if (and (c-safe (progn (backward-up-list -1) t))
			     (memq (char-before) '(?\) ?}))
			     (progn (widen)
				    (c-safe (progn (c-forward-sexp -1)
						   t))))
			(setq c-state-cache
			      (c-hack-state (point) 'open c-state-cache)))))
		(c-indent-line))
	      (setq c-state-cache (c-adjust-state (c-point 'bol) here
						  (- (point) (c-point 'bol))
						  c-state-cache))
	      (goto-char (- (point-max) pos))
	      ;; if the buffer has changed due to the indentation, we
	      ;; need to recalculate syntax for the current line, but
	      ;; we won't need to update the state cache.
	      (if (/= (point) here)
		  (setq syntax (c-guess-basic-syntax))))
	  ;; must remove the newline we just stuck in (if we really did it)
	  (and delete-temp-newline
	       (save-excursion
		 ;; if there is whitespace before point, then preserve
		 ;; at least one space.
		 (delete-indentation)
		 (just-one-space)
		 (if (not preserve-p)
		     (delete-char -1))))
	  ;; since we're hanging the brace, we need to recalculate
	  ;; syntax.  Update the state to accurately reflect the
	  ;; beginning of the line.  We punt if we cross any open or
	  ;; closed parens because its just too hard to modify the
	  ;; known state.  This limitation will be fixed in v5.
	  (save-excursion
	    (let ((bol (c-point 'bol)))
	      (if (zerop (car (parse-partial-sexp bol (1- (point)))))
		  (setq c-state-cache (c-whack-state bol c-state-cache)
			syntax (c-guess-basic-syntax))
		;; gotta punt. this requires some horrible kludgery
		(beginning-of-line)
		(makunbound 'c-state-cache)
		(setq c-state-cache (c-parse-state)
		      syntax nil))))
	  )
	;; now adjust the line's indentation. don't update the state
	;; cache since c-guess-basic-syntax isn't called when the
	;; syntax is passed to c-indent-line
	(let* ((here (point)))
	  (c-indent-line syntax)
	  (setq c-state-cache (c-adjust-state (c-point 'bol) here
					      (- (c-point 'boi) (c-point 'bol))
					      c-state-cache)))
	;; Do all appropriate clean ups
	(let ((here (point))
	      (pos (- (point-max) (point)))
	      mbeg mend tmp)
	  ;; clean up empty defun braces
	  (if (and c-auto-newline
		   (memq 'empty-defun-braces c-cleanup-list)
		   (eq last-command-char ?\})
		   (c-intersect-lists '(defun-close class-close inline-close)
				      syntax)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (eq (char-before) ?\{))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal)))
	      (delete-region (point) (1- here)))
	  ;; clean up brace-else-brace and brace-elseif-brace
	  (when (and c-auto-newline
		     (eq last-command-char ?\{)
		     (not (c-in-literal)))
	    (cond
	     ((and (memq 'brace-else-brace c-cleanup-list)
		   (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		   (progn
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (eq (match-end 0) here)))
	      (delete-region mbeg mend)
	      (insert "} else {"))
	     ((and (memq 'brace-elseif-brace c-cleanup-list)
		   (progn
		     (goto-char (1- here))
		     (setq mend (point))
		     (skip-chars-backward " \t\n")
		     (setq mbeg (point))
		     (eq (char-before) ?\)))
		   (= (c-backward-token-1 1 t) 0)
		   (eq (char-after) ?\()
		   (progn
		     (setq tmp (point))
		     (re-search-backward "}[ \t\n]*else[ \t\n]+if[ \t\n]*"
					 nil t))
		   (eq (match-end 0) tmp))
	      (delete-region mbeg mend)
	      (goto-char mbeg)
	      (insert " "))))
	  (goto-char (- (point-max) pos))
	  )
	;; does a newline go after the brace?
	(if (memq 'after newlines)
	    (progn
	      (newline)
	      ;; update on c-state-cache
	      (let* ((bufpos (- (point) 2))
		     (which (if (eq (char-after bufpos) ?{) 'open 'close))
		     (c-state-cache (c-hack-state bufpos which c-state-cache)))
		(c-indent-line))))
	;; blink the paren
	(and (eq last-command-char ?\})
	     old-blink-paren
	     (save-excursion
	       (c-backward-syntactic-ws safepos)
	       (funcall old-blink-paren)))
	))))

(defun c-electric-slash (arg)
  "Insert a slash character.

Indent the line as a comment, if:

  1. The slash is second of a `//' line oriented comment introducing
     token and we are on a comment-only-line, or

  2. The slash is part of a `*/' token that closes a block oriented
     comment.

If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  (let* ((ch (char-before))
	 (indentp (and (not arg)
		       (eq last-command-char ?/)
		       (or (and (eq ch ?/)
				(not (c-in-literal)))
			   (and (eq ch ?*)
				(c-in-literal)))
		       ))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; if we are in a literal, or if arg is given do not re-indent the
  ;; current line, unless this star introduces a comment-only line.
  (if (and (not arg)
	   (memq (c-in-literal) '(c))
	   (eq (char-before) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (eq (char-before) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      ;; shut this up
      (let (c-echo-syntactic-information-p)
	(c-indent-line))
    ))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline might be inserted.  See
the variable `c-hanging-semi&comma-criteria' for how newline insertion
is determined.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon.

Based on the value of `c-cleanup-list', this function cleans up commas
following brace lists and semicolons following defuns."
  (interactive "*P")
  (let* ((lim (c-most-enclosing-brace (c-parse-state)))
	 (literal (c-in-literal lim))
	 (here (point))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups and newline insertions if c-auto-newline is
      ;; turned on
      (if (not c-auto-newline)
	  (c-indent-line)
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(eq last-command-char ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(eq last-command-char ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (eq (char-before) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal lim)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	(c-indent-line)
	;; check to see if a newline should be added
	(let ((criteria c-hanging-semi&comma-criteria)
	      answer add-newline-p)
	  (while criteria
	    (setq answer (funcall (car criteria)))
	    ;; only nil value means continue checking
	    (if (not answer)
		(setq criteria (cdr criteria))
	      (setq criteria nil)
	      ;; only 'stop specifically says do not add a newline
	      (setq add-newline-p (not (eq answer 'stop)))
	      ))
	  (if add-newline-p
	      (progn (newline)
		     (c-indent-line)))
	  )))))

(defun c-electric-colon (arg)
  "Insert a colon.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after colons based on the value of `c-hanging-colons-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal.

This function cleans up double colon scope operators based on the
value of `c-cleanup-list'."
  (interactive "*P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 syntax newlines is-scope-op
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; insert the colon, then do any specified cleanups
      (self-insert-command (prefix-numeric-value arg))
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and c-auto-newline
		 (memq 'scope-operator c-cleanup-list)
		 (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (eq (char-before) ?:))
		 (not (c-in-literal))
		 (not (eq (char-after (- (point) 2)) ?:)))
	    (progn
	      (delete-region (point) (1- here))
	      (setq is-scope-op t)))
	(goto-char (- (point-max) pos)))
      ;; lets do some special stuff with the colon character
      (setq syntax (c-guess-basic-syntax)
	    ;; some language elements can only be determined by
	    ;; checking the following line.  Lets first look for ones
	    ;; that can be found when looking on the line with the
	    ;; colon
	    newlines
	    (and c-auto-newline
		 (or (c-lookup-lists '(case-label label access-label)
				     syntax c-hanging-colons-alist)
		     (c-lookup-lists '(member-init-intro inher-intro)
				     (let ((buffer-undo-list t))
				       (insert "\n")
				       (unwind-protect
					   (c-guess-basic-syntax)
					 (delete-char -1)))
				     c-hanging-colons-alist))))
      ;; indent the current line
      (c-indent-line syntax)
      ;; does a newline go before the colon?  Watch out for already
      ;; non-hung colons.  However, we don't unhang them because that
      ;; would be a cleanup (and anti-social).
      (if (and (memq 'before newlines)
	       (not is-scope-op)
	       (save-excursion
		 (skip-chars-backward ": \t")
		 (not (bolp))))
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-line)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (and (memq 'after (cdr-safe newlines))
	       (not is-scope-op))
	  (progn
	    (newline)
	    (c-indent-line)))
      )))

(defun c-electric-lt-gt (arg)
  "Insert a less-than, or greater-than character.
The line will be re-indented if the character inserted is the second
of a C++ style stream operator and the buffer is in C++ mode.
Exceptions are when a numeric argument is supplied, or point is inside
a literal, in which case the line will not be re-indented."
  (interactive "*P")
  (let ((indentp (and (not arg)
		      (eq (char-before) last-command-char)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-paren (arg)
  "Insert a parenthesis.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, some newline cleanups are done if
appropriate; see the variable `c-cleanup-list'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the
parenthesis, or the parenthesis is inserted inside a literal."
  (interactive "*P")
  (let (;; shut this up
	(c-echo-syntactic-information-p nil))
    (if (or arg
	    (not (looking-at "[ \t]*$"))
	    (c-in-literal (c-point 'bod)))
	(self-insert-command (prefix-numeric-value arg))
      ;; do some special stuff with the character
      (let* (;; We want to inhibit blinking the paren since this will
	     ;; be most disruptive.  We'll blink it ourselves
	     ;; afterwards.
	     (old-blink-paren blink-paren-function)
	     blink-paren-function)
	(self-insert-command (prefix-numeric-value arg))
	(c-indent-line)
	(when c-auto-newline
	  ;; Do all appropriate clean ups
	  (let ((here (point))
		(pos (- (point-max) (point)))
		mbeg mend)
	    ;; clean up brace-elseif-brace
	    (if (and (memq 'brace-elseif-brace c-cleanup-list)
		     (eq last-command-char ?\()
		     (re-search-backward "}[ \t\n]*else[ \t\n]+if[ \t\n]*("
					 nil t)
		     (save-excursion
		       (setq mbeg (match-beginning 0)
			     mend (match-end 0))
		       (= mend here))
		     (not (c-in-literal)))
		(progn
		  (delete-region mbeg mend)
		  (insert "} else if (")))
	    ;; clean up brace-catch-brace
	    (if (and (memq 'brace-catch-brace c-cleanup-list)
		     (eq last-command-char ?\()
		     (re-search-backward "}[ \t\n]*catch[ \t\n]*(" nil t)
		     (save-excursion
		       (setq mbeg (match-beginning 0)
			     mend (match-end 0))
		       (= mend here))
		     (not (c-in-literal)))
		(progn
		  (delete-region mbeg mend)
		  (insert "} catch (")))
	    (goto-char (- (point-max) pos))
	    ))
	(if old-blink-paren
	    (funcall old-blink-paren))))))



;; better movement routines for ThisStyleOfVariablesCommonInCPlusPlus
;; originally contributed by Terry_Glanfield.Southern@rxuk.xerox.com
(defun c-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With arg, to it arg times."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward "\\W*\\([A-Z]*[a-z0-9]*\\)" (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\W\\w+\\)"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (c-keep-region-active))

(defun c-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (c-forward-into-nomenclature (- arg))
  (c-keep-region-active))

(defun c-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive "*")
  (insert "::"))

(defun c-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

Unlike the built-in `beginning-of-defun' this tries to be smarter
about finding the char with open-parenthesis syntax that starts the
defun."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (c-end-of-defun (- arg))
    (while (> arg 0)
      (let ((state (nreverse (c-parse-state)))
	    prevbod bod)
	(while (and state (not bod))
	  (setq bod (car state)
		state (cdr state))
	  (if (consp bod)
	      (setq prevbod (car bod)
		    bod nil)))
	(cond
	 (bod (goto-char bod))
	 (prevbod (goto-char prevbod))
	 (t (goto-char (point-min))
	    (setq arg 0)))
	(setq arg (1- arg))))
    (c-keep-region-active)
    (= arg 0)))

(defun c-end-of-defun (&optional arg)
  "Move forward to next end of defun.  With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.
Returns t unless search stops due to beginning or end of buffer.

An end of a defun occurs right after the close-parenthesis that matches
the open-parenthesis that starts a defun; see `beginning-of-defun'."
  (interactive "p")
  (if (not arg)
      (setq arg 1))
  (if (< arg 0)
      (c-beginning-of-defun (- arg))
    (while (> arg 0)
      (let ((pos (point))
	    eol)
	(while (and (c-safe (down-list 1) t)
		    (not (eq (char-before) ?{)))
	  ;; skip down into the next defun-block
	  (forward-char -1)
	  (c-forward-sexp))
	(c-beginning-of-defun 1)
	(setq eol (c-point 'eol))
	(c-forward-sexp)
	(if (< eol (point))
	    ;; Don't move to next line for one line defuns.
	    (forward-line 1))
	(when (<= (point) pos)
	  (goto-char (point-max))
	  (setq arg 0))
	(setq arg (1- arg))))
    (c-keep-region-active)
    (= arg 0)))


(defun c-beginning-of-statement (&optional count lim sentence-flag)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the closest
preceding one, moving into nested blocks if necessary (use
\\[backward-sexp] to skip over a block).  If within a comment, or next
to a comment (only whitespace between), move by sentences instead of
statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (let* ((count (or count 1))
	 here
	 (range (c-collect-line-comments (c-literal-limits lim))))
    (while (and (/= count 0)
		(or (not lim) (> (point) lim)))
      (setq here (point))
      (if (and (not range) sentence-flag)
	  (save-excursion
	    ;; Find the comment next to point if we're not in one.
	    (if (> count 0)
		(setq range (if (c-forward-comment -1)
				(cons (point)
				      (progn (c-forward-comment 1) (point)))))
	      (skip-chars-forward " \t\n")
	      (setq range (point))
	      (setq range (if (c-forward-comment 1)
			      (cons range (point))
			    nil)))
	    (setq range (c-collect-line-comments range))))
      (if (and (< count 0) (= here (point-max)))
	  ;; Special case because eob might be in a literal.
	  (setq range nil))
      (if range
	  (if (and sentence-flag
		   (/= (char-syntax (char-after (car range))) ?\"))
	      (let* ((lit-type (c-literal-type range))
		     (beg (save-excursion
			    (goto-char (car range))
			    (looking-at (if (eq lit-type 'c)
					    comment-start-skip
					  (concat "\\("
						  c-comment-prefix-regexp
						  "\\)[ \t]*")))
			    (goto-char (match-end 0))
			    (point)))
		     (end (save-excursion
			    (goto-char (- (cdr range)
					  (if (eq lit-type 'c) 2 1)))
			    (point))))
		;; move by sentence, but not past the limit of the literal
		(save-restriction
		  (narrow-to-region beg end)
		  (c-safe (forward-sentence (if (< count 0) 1 -1)))
		  (if (and (memq lit-type '(c c++))
			   ;; Check if we stopped due to a comment
			   ;; prefix and not a sentence end.
			   (/= (point) beg)
			   (save-excursion
			     (beginning-of-line)
			     (looking-at (concat "[ \t]*\\("
						 c-comment-prefix-regexp
						 "\\)[ \t]*")))
			   (>= (point) (match-beginning 0))
			   (/= (match-beginning 1) (match-end 1))
			   (or (< (point) (match-end 0))
			       (and
				(= (point) (match-end 0))
				;; The comment prefix may contain
				;; characters that is regarded as end
				;; of sentence.
				(or (eolp)
				    (and
				     (save-excursion
				       (forward-paragraph -1)
				       (< (point) (match-beginning 0)))
				     (save-excursion
				       (beginning-of-line)
				       (or (not (re-search-backward
						 sentence-end
						 (c-point 'bopl)
						 t))
					   (< (match-end 0)
					      (c-point 'eol)))))))))
		      (setq count (+ count (if (< count 0) -1 1)))
		    (if (< count 0)
			(progn
			  ;; In block comments, if there's only
			  ;; horizontal ws between the text and the
			  ;; comment ender, stop before it.  Stop after
			  ;; the ender if there's either nothing or
			  ;; newlines between.
			  (when (and (eq lit-type 'c) (eq (point) end))
			    (widen)
			    (skip-chars-backward " \t")
			    (when (or (eq (point) end) (bolp))
			      (goto-char (cdr range)))))
		      (when (and (eq (point) beg) (looking-at "[ \t]*$"))
			;; Stop before instead of after the comment
			;; starter if nothing follows it.
			(widen)
			(goto-char (car range))))))
		;; See if we should escape the literal.
		(if (> count 0)
		    (if (< (point) here)
			(setq count (1- count))
		      (goto-char (car range))
		      (setq range nil))
		  (if (> (point) here)
		      (setq count (1+ count))
		    (goto-char (cdr range))
		    (setq range nil))))
	    (goto-char (if (> count 0) (car range) (cdr range)))
	    (setq range nil))
	;; Below we do approximately the same as
	;; c-beginning-of-statement-1 and c-end-of-statement-1, and
	;; perhaps they should be changed, but that'd likely break a
	;; lot in cc-engine.
	(goto-char here)
	(if (> count 0)
	    (condition-case nil
		;; Stop before `{' and after `;', `{', `}' and `};'
		;; when not followed by `}' or `)', but on the other
		;; side of the syntactic ws.  Move by sexps and move
		;; into parens.  Also stop before `#' when it's first
		;; on a line.
		(let ((comment-pos (not sentence-flag))
		      (large-enough (- (point-max)))
		      last last-below-line)
		  (catch 'done
		    (while t
		      (setq last (point))
		      (when (and (looking-at "{\\|^#") (/= here last))
			(unless (and c-special-brace-lists
				     (eq (char-after) ?{)
				     (c-looking-at-special-brace-list))
			  (if (and (eq (char-after) ?#)
				   (numberp last-below-line)
				   (not (eq last-below-line here)))
			      (goto-char last-below-line))
			  (throw 'done t)))
		      (if comment-pos
			  (c-forward-comment large-enough)
			(when (c-forward-comment -1)
			  ;; Record position of first comment.
			  (save-excursion
			    (c-forward-comment 1)
			    (setq comment-pos (point)))
			  (c-forward-comment large-enough)))
		      (unless last-below-line
			(if (save-excursion
			      (re-search-forward "\\(^\\|[^\\]\\)$" last t))
			    (setq last-below-line last)))
		      (cond ((bobp)	; Must handle bob specially.
			     (if (= here last)
				 (throw 'done t)
			       (goto-char last)
			       (throw 'done t)))
			    ((progn (backward-char)
				    (looking-at "[;{}]"))
			     (if (and c-special-brace-lists
				      (eq (char-after) ?{)
				      (c-looking-at-special-brace-list))
				 (skip-syntax-backward "w_") ; Speedup only.
			       (if (or (= here last)
				       (memq (char-after last) '(?\) ?})))
				   (if (and (eq (char-before) ?})
					    (eq (char-after) ?\;))
				       (backward-char))
				 (goto-char last)
				 (throw 'done t))))
			    ((= (char-syntax (char-after)) ?\")
			     (forward-char)
			     (c-backward-sexp))
			    (t (skip-syntax-backward "w_")) ; Speedup only.
			    )))
		  (if (and (numberp comment-pos)
			   (< (point) comment-pos))
		      ;; We jumped over a comment that should be investigated.
		      (goto-char comment-pos)
		    (setq count (1- count))))
	      (error
	       (goto-char (point-min))
	       (setq count 0)))
	  (condition-case nil
	      ;; Stop before `{', `}', and `#' when it's first on a
	      ;; line, but on the other side of the syntactic ws, and
	      ;; after `;', `}' and `};'.  Only stop before `{' if at
	      ;; top level or inside braces, though.  Move by sexps
	      ;; and move into parens.  Also stop at eol of lines
	      ;; starting with `#'.
	      (let ((comment-pos (not sentence-flag))
		    (large-enough (point-max))
		    last)
		(catch 'done
		  (while t
		    (setq last (point))
		    (if comment-pos
			(c-forward-comment large-enough)
		      (if (progn
			    (skip-chars-forward " \t\n\r\f")
			    ;; Record position of first comment.
			    (setq comment-pos (point))
			    (c-forward-comment 1))
			  (c-forward-comment large-enough)
			(setq comment-pos nil)))
		    (cond ((and (eq (char-after) ?{)
				(not (and c-special-brace-lists
					  (c-looking-at-special-brace-list)))
				(/= here last)
				(save-excursion
				  (or (not (c-safe (up-list -1) t))
				      (= (char-after) ?{))))
			   (goto-char last)
			   (throw 'done t))
			  ((and c-special-brace-lists
				(eq (char-after) ?})
				(save-excursion
				  (and (c-safe (up-list -1) t)
				       (c-looking-at-special-brace-list))))
			   (forward-char 1)
			   (skip-syntax-forward "w_")) ; Speedup only.
			  ((and (eq (char-after) ?})
				(/= here last))
			   (goto-char last)
			   (throw 'done t))
			  ((looking-at "^#")
			   (if (= here last)
			       (or (re-search-forward "\\(^\\|[^\\]\\)$" nil t)
				   (goto-char (point-max)))
			     (goto-char last))
			   (throw 'done t))
			  ((looking-at ";\\|};?")
			   (goto-char (match-end 0))
			   (throw 'done t))
			  ((= (char-syntax (char-after)) ?\")
			   (c-forward-sexp))
			  (t
			   (forward-char 1)
			   (skip-syntax-forward "w_")) ; Speedup only.
			  )))
		(if (and (numberp comment-pos)
			 (> (point) comment-pos))
		    ;; We jumped over a comment that should be investigated.
		    (goto-char comment-pos)
		  (setq count (1+ count))))
	    (error
	     (goto-char (point-max))
	     (setq count 0)))
	  ))
      ;; If we haven't moved we're near a buffer limit.
      (when (and (not (zerop count)) (= (point) here))
	(goto-char (if (> count 0) (point-min) (point-max)))
	(setq count 0)))
    ;; its possible we've been left up-buf of lim
    (if lim (goto-char (max (point) lim))))
  (c-keep-region-active))

(defun c-end-of-statement (&optional count lim sentence-flag)
  "Go to the end of the innermost C statement.
With prefix arg, go forward N - 1 statements.  Move forward to the end
of the next statement if already at end, and move into nested blocks
\(use \\[forward-sexp] to skip over a block).  If within a comment, or
next to a comment (only whitespace between), move by sentences instead
of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (c-beginning-of-statement (- (or count 1)) lim sentence-flag)
  (c-keep-region-active))


;; set up electric character functions to work with pending-del,
;; (a.k.a. delsel) mode.  All symbols get the t value except
;; the functions which delete, which gets 'supersede.
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for delsel (Emacs)
    (put sym 'pending-delete t)))	; for pending-del (XEmacs)
 '(c-electric-pound
   c-electric-brace
   c-electric-slash
   c-electric-star
   c-electric-semi&comma
   c-electric-lt-gt
   c-electric-colon
   c-electric-paren))
(put 'c-electric-delete    'delete-selection 'supersede) ; delsel
(put 'c-electric-delete    'pending-delete   'supersede) ; pending-del
(put 'c-electric-backspace 'delete-selection 'supersede) ; delsel
(put 'c-electric-backspace 'pending-delete   'supersede) ; pending-del


;; This is used by indent-for-comment to decide how much to indent a
;; comment in C code based on its context.
(defun c-comment-indent ()
  (if (looking-at (concat "^\\(" c-comment-start-regexp "\\)"))
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: A comment following a solitary close-brace should
	 ;; have only one space.
	 ((looking-at (concat "[ \t]*}[ \t]*\\($\\|"
			      c-comment-start-regexp
			      "\\)"))
	  (search-forward "}")
	  (1+ (current-column)))
	 ;; CASE 2: 2 spaces after #endif
	 ((or (looking-at "^#[ \t]*endif[ \t]*")
	      (looking-at "^#[ \t]*else[ \t]*"))
	  7)
	 ;; CASE 3: when c-indent-comments-syntactically-p is t,
	 ;; calculate the offset according to c-offsets-alist.
	 ;; E.g. identical to hitting TAB.
	 ((and c-indent-comments-syntactically-p
	       (save-excursion
		 (skip-chars-forward " \t")
		 (or (looking-at c-comment-start-regexp)
		     (eolp))))
	  (let ((syntax (c-guess-basic-syntax)))
	    ;; BOGOSITY ALERT: if we're looking at the eol, its
	    ;; because indent-for-comment hasn't put the comment-start
	    ;; in the buffer yet.  this will screw up the syntactic
	    ;; analysis so we kludge in the necessary info.  Another
	    ;; kludge is that if we're at the bol, then we really want
	    ;; to ignore any anchoring as specified by
	    ;; c-comment-only-line-offset since it doesn't apply here.
	    (if (save-excursion
		  (beginning-of-line)
		  (skip-chars-forward " \t")
		  (eolp))
		(c-add-syntax 'comment-intro))
	    (let ((c-comment-only-line-offset
		   (if (consp c-comment-only-line-offset)
		       c-comment-only-line-offset
		     (cons c-comment-only-line-offset
			   c-comment-only-line-offset))))
	      (apply '+ (mapcar 'c-get-offset syntax)))))
	 ;; CASE 4: If previous line is a comment-only line, use its
	 ;; indentation if it's greater than comment-column.  Leave at
	 ;; least one space between the comment and the last nonblank
	 ;; character in any case.
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at c-comment-start-regexp)
	      (setq placeholder (current-column))))
	  (goto-char opoint)
	  (skip-chars-backward " \t")
	  (max (if (bolp) 0 (1+ (current-column)))
	       placeholder
	       comment-column))
	 ;; CASE 5: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 6: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))


;; used by outline-minor-mode
(defun c-outline-level ()
  (save-excursion
    (skip-chars-forward "\t ")
    (current-column)))


(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.

`#elif' is treated like `#else' followed by `#if', so the function
stops at them when going backward, but not when going forward."
  (interactive "p")
  (c-forward-conditional (- count) -1)
  (c-keep-region-active))
  
(defun c-up-conditional-with-else (count)
  "Move back to the containing preprocessor conditional, including `#else'.
Just like `c-up-conditional', except it also stops at `#else'
directives."
  (interactive "p")
  (c-forward-conditional (- count) -1 t)
  (c-keep-region-active))

(defun c-down-conditional (count)
  "Move forward into the next preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward into the previous preprocessor conditional.

`#elif' is treated like `#else' followed by `#if', so the function
stops at them when going forward, but not when going backward."
  (interactive "p")
  (c-forward-conditional count 1)
  (c-keep-region-active))

(defun c-down-conditional-with-else (count)
  "Move forward into the next preprocessor conditional, including `#else'.
Just like `c-down-conditional', except it also stops at `#else'
directives."
  (interactive "p")
  (c-forward-conditional count 1 t)
  (c-keep-region-active))

(defun c-backward-conditional (count &optional target-depth with-else)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional."
  (interactive "p")
  (c-forward-conditional (- count) target-depth with-else)
  (c-keep-region-active))

(defun c-forward-conditional (count &optional target-depth with-else)
  "Move forward across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward across a preprocessor conditional.

`#elif' is treated like `#else' followed by `#if', except that the
nesting level isn't changed when tracking subconditionals.

The optional argument TARGET-DEPTH specifies the wanted nesting depth
after each scan.  I.e. if TARGET-DEPTH is -1, the function will move
out of the enclosing conditional.  A non-integer non-nil TARGET-DEPTH
counts as -1.

If the optional argument WITH-ELSE is non-nil, `#else' directives are
treated as conditional clause limits.  Normally they are ignored."
  (interactive "p")
  (let* ((forward (> count 0))
	 (increment (if forward -1 1))
	 (search-function (if forward 're-search-forward 're-search-backward))
	 (new))
    (unless (integerp target-depth)
      (setq target-depth (if target-depth -1 0)))
    (save-excursion
      (while (/= count 0)
	(let ((depth 0)
	      ;; subdepth is the depth in "uninteresting" subtrees,
	      ;; i.e. those that takes us farther from the target
	      ;; depth instead of closer.
	      (subdepth 0)
	      found)
	  (save-excursion
	    ;; Find the "next" significant line in the proper direction.
	    (while (and (not found)
			;; Rather than searching for a # sign that
			;; comes at the beginning of a line aside from
			;; whitespace, search first for a string
			;; starting with # sign.  Then verify what
			;; precedes it.  This is faster on account of
			;; the fastmap feature of the regexp matcher.
			(funcall search-function
				 "#[ \t]*\\(if\\|elif\\|endif\\|else\\)"
				 nil t))
	      (beginning-of-line)
	      ;; Now verify it is really a preproc line.
	      (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\|else\\)")
		  (let (dchange (directive (match-string 1)))
		    (cond ((string= directive "if")
			   (setq dchange (- increment)))
			  ((string= directive "endif")
			   (setq dchange increment))
			  ((= subdepth 0)
			   ;; When we're not in an "uninteresting"
			   ;; subtree, we might want to act on "elif"
			   ;; and "else" too.
			   (if (cond (with-else
				      ;; Always move toward the target depth.
				      (setq dchange
					    (if (> target-depth 0) 1 -1)))
				     ((string= directive "elif")
				      (setq dchange (- increment))))
			       ;; Ignore the change if it'd take us
			       ;; into an "uninteresting" subtree.
			       (if (eq (> dchange 0) (<= target-depth 0))
				   (setq dchange nil)))))
		    (when dchange
		      (when (or (/= subdepth 0)
				(eq (> dchange 0) (<= target-depth 0)))
			(setq subdepth (+ subdepth dchange)))
		      (setq depth (+ depth dchange))
		      ;; If we are trying to move across, and we find an
		      ;; end before we find a beginning, get an error.
		      (if (and (< depth target-depth) (< dchange 0))
			  (error (if forward
				     "No following conditional at this level"
				   "No previous conditional at this level"))))
		    ;; When searching forward, start from next line so
		    ;; that we don't find the same line again.
		    (if forward (forward-line 1))
		    ;; We found something if we've arrived at the
		    ;; target depth.
		    (if (and dchange (= depth target-depth))
			(setq found (point))))
		;; else
		(if forward (forward-line 1)))))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (push-mark)
    (goto-char new))
  (c-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional whole-exp)
  "Indent current line as C code, and/or insert some whitespace.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert some whitespace[*].  If
other than nil or t, then some whitespace[*] is inserted only within
literals (comments and strings) and inside preprocessor directives,
but the line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression is preserved.

  [*] The amount and kind of whitespace inserted is controlled by the
  variable `c-insert-tab-function', which is called to do the actual
  insertion of whitespace.  Normally the function in this variable
  just inserts a tab character, or the equivalent number of spaces,
  depending on the variable `indent-tabs-mode'."

  (interactive "P")
  (let ((bod (c-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c-indent-line))
	      beg end)
	  (save-excursion
	    (if (eq c-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (c-forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end (- shift-amt) "#")))
      ;; No arg supplied, use c-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (funcall c-insert-tab-function)
	  (c-indent-line)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(c-indent-line))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-in-literal bod)
	    (funcall c-insert-tab-function))
	(c-indent-line)
	)))))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in balanced expression following point.
Optional SHUTUP-P if non-nil, inhibits message printing and error checking."
  (interactive "*P")
  (let ((here (point-marker))
	end progress-p)
    (set-marker-insertion-type here t)
    (unwind-protect
	(let ((c-echo-syntactic-information-p nil) ;keep quiet for speed
	      (start (progn
		       ;; try to be smarter about finding the range of
		       ;; lines to indent. skip all following
		       ;; whitespace. failing that, try to find any
		       ;; opening brace on the current line
		       (skip-chars-forward " \t\n")
		       (if (memq (char-after) '(?\( ?\[ ?\{))
			   (point)
			 (let ((state (parse-partial-sexp (point)
							  (c-point 'eol))))
			   (and (nth 1 state)
				(goto-char (nth 1 state))
				(memq (char-after) '(?\( ?\[ ?\{))
				(point)))))))
	  ;; find balanced expression end
	  (setq end (and (c-safe (progn (c-forward-sexp 1) t))
			 (point-marker)))
	  ;; sanity check
	  (and (not start)
	       (not shutup-p)
	       (error "Cannot find start of balanced expression to indent."))
	  (and (not end)
	       (not shutup-p)
	       (error "Cannot find end of balanced expression to indent."))
	  (c-progress-init start end 'c-indent-exp)
	  (setq progress-p t)
	  (goto-char start)
	  (beginning-of-line)
	  (while (< (point) end)
	    (if (not (looking-at "[ \t]*$"))
		(c-indent-line))
	    (c-progress-update)
	    (forward-line 1)))
      ;; make sure marker is deleted
      (and end
	   (set-marker end nil))
      (and progress-p
	   (c-progress-fini 'c-indent-exp))
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-defun ()
  "Re-indents the current top-level function def, struct or class declaration."
  (interactive "*")
  (let ((here (point-marker))
	(c-echo-syntactic-information-p nil)
	(brace (c-least-enclosing-brace (c-parse-state))))
    (goto-char (or brace (c-point 'bod)))
    ;; if we're sitting at b-o-b, it might be because there was no
    ;; least enclosing brace and we were sitting on the defun's open
    ;; brace.
    (if (and (bobp) (not (eq (char-after) ?\{)))
	(goto-char here))
    ;; if defun-prompt-regexp is non-nil, b-o-d might not leave us at
    ;; the open brace. I consider this an Emacs bug.
    (and (boundp 'defun-prompt-regexp)
	 defun-prompt-regexp
	 (looking-at defun-prompt-regexp)
	 (goto-char (match-end 0)))
    ;; catch all errors in c-indent-exp so we can 1. give more
    ;; meaningful error message, and 2. restore point
    (unwind-protect
	(c-indent-exp)
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (save-excursion
    (goto-char start)
    ;; Advance to first nonblank line.
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let (endmark)
      (unwind-protect
	  (let ((c-tab-always-indent t)
		;; shut up any echo msgs on indiv lines
		(c-echo-syntactic-information-p nil)
		fence)
	    (c-progress-init start end 'c-indent-region)
	    (setq endmark (copy-marker end))
	    (while (and (bolp)
			(not (eobp))
			(< (point) endmark))
	      ;; update progress
	      (c-progress-update)
	      ;; Indent one line as with TAB.
	      (let (nextline sexpend sexpbeg)
		;; skip blank lines
		(skip-chars-forward " \t\n")
		(beginning-of-line)
		;; indent the current line
		(c-indent-line)
		(setq fence (point))
		(if (save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*#"))
		    (forward-line 1)
		  (save-excursion
		    ;; Find beginning of following line.
		    (setq nextline (c-point 'bonl))
		    ;; Find first beginning-of-sexp for sexp extending past
		    ;; this line.
		    (beginning-of-line)
		    (while (< (point) nextline)
		      (condition-case nil
			  (progn
			    (c-forward-sexp 1)
			    (setq sexpend (point)))
			(error (setq sexpend nil)
			       (goto-char nextline)))
		      (c-forward-syntactic-ws))
		    (if sexpend
			(progn
			  ;; make sure the sexp we found really starts on the
			  ;; current line and extends past it
			  (goto-char sexpend)
			  (setq sexpend (point-marker))
			  (c-safe (c-backward-sexp 1))
			  (setq sexpbeg (point))))
		    (if (and sexpbeg (< sexpbeg fence))
			(setq sexpbeg fence)))
		  ;; Since we move by sexps we might have missed
		  ;; comment-only lines.
		  (if sexpbeg
		      (save-excursion
			(while (progn
				 (forward-line 1)
				 (skip-chars-forward " \t")
				 (< (point) sexpbeg))
			  (if (looking-at c-comment-start-regexp)
			      (setq sexpbeg (c-point 'bol))))))
		  ;; If that sexp ends within the region, indent it all at
		  ;; once, fast.
		  (condition-case nil
		      (if (and sexpend
			       (> sexpend nextline)
			       (<= sexpend endmark))
			  (progn
			    (goto-char sexpbeg)
			    (c-indent-exp 'shutup)
			    (c-progress-update)
			    (goto-char sexpend)))
		    (error
		     (goto-char sexpbeg)
		     (c-indent-line)))
		  ;; Move to following line and try again.
		  (and sexpend
		       (markerp sexpend)
		       (set-marker sexpend nil))
		  (forward-line 1)
		  (setq fence (point))))))
	(set-marker endmark nil)
	(c-progress-fini 'c-indent-region)
	(c-echo-parsing-error)
	))))

(defun c-mark-function ()
  "Put mark at end of current top-level defun, point at beginning."
  (interactive)
  (let ((here (point))
	(eod (c-point 'eod))
	(state (c-parse-state)))
    ;; Are we sitting at the top level, someplace between either the
    ;; beginning of buffer, or the nearest preceding defun?  If so,
    ;; try first to figure out whether we're sitting on the
    ;; introduction to a top-level defun, in which case we want to
    ;; mark the entire defun we're sitting on.
    ;;
    ;; If we're sitting on anything else at the top-level, we want to
    ;; just mark the statement that we're on
    (if (or (and (consp (car state))
		 (= (length state) 1))
	    (null state))
	;; Are we in the whitespace after the nearest preceding defun?
	(if (and state
		 (looking-at "[ \t]*$")
		 (= (save-excursion
		      (c-backward-syntactic-ws)
		      (skip-chars-backward ";")
		      (point))
		    (cdar state)))
	    (progn
	      (setq eod (point))
	      (goto-char (caar state))
	      (c-beginning-of-statement-1))
	  (if (= ?{ (save-excursion
		      (c-end-of-statement-1)
		      (char-before)))
	      ;; We must be in a defuns's introduction
	      (progn
		(c-end-of-statement-1)
		(skip-chars-backward "{")
		(c-beginning-of-statement-1)
		(c-forward-syntactic-ws))
	    ;; Just mark the statement
	    (c-end-of-statement-1)
	    (forward-line 1)
	    (setq eod (point))
	    (c-beginning-of-statement-1)))
      ;; We are inside some enclosing brace structure, so we first
      ;; need to find our way to the least enclosing brace.  Then, in
      ;; both cases, we to mark the region from the beginning of the
      ;; current statement, until the end of the next following defun
      (while (and state)
	(or (consp (car state))
	    (goto-char (car state)))
	(setq state (cdr state)))
      (c-beginning-of-statement-1))
    (push-mark here)
    (push-mark eod nil t)))

(defun c-indent-line-or-region ()
  "When the region is active, indent it.  Otherwise indent the current line."
  ;; Emacs has a variable called mark-active, XEmacs uses region-active-p
  (interactive)
  (if (c-region-is-active-p)
      (c-indent-region (region-beginning) (region-end))
    (c-indent-command)))


;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  (cond
   ;; Be silent
   ((not c-progress-interval))
   ;; Start the progress update messages.  If this Emacs doesn't have
   ;; a built-in timer, just be dumb about it.
   ((not (fboundp 'current-time))
    (message "indenting region... (this may take a while)"))
   ;; If progress has already been initialized, do nothing. otherwise
   ;; initialize the counter with a vector of:
   ;;     [start end lastsec context]
   (c-progress-info)
   (t (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "indenting region..."))
   ))

(defun c-progress-update ()
  ;; update progress
  (if (not (and c-progress-info c-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref c-progress-info 0))
	  (end (aref c-progress-info 1))
	  (lastsecs (aref c-progress-info 2)))
      ;; should we update?  currently, update happens every 2 seconds,
      ;; what's the right value?
      (if (< c-progress-interval (- now lastsecs))
	  (progn
	    (message "indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset c-progress-info 2 now)))
      )))

(defun c-progress-fini (context)
  ;; finished
  (if (not c-progress-interval)
      nil
    (if (or (eq context (aref c-progress-info 3))
	    (eq context t))
	(progn
	  (set-marker (aref c-progress-info 1) nil)
	  (setq c-progress-info nil)
	  (message "indenting region...done")))))



;;; This page handles insertion and removal of backslashes for C macros.

(defun c-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify blank lines at the start of the region.
If the region ends at the start of a line, it always deletes the
backslash (if any) at the end of the previous line.

You can put the region around an entire macro definition and use this
command to conveniently insert and align the necessary backslashes."
  (interactive "*r\nP")
  (save-excursion
    (goto-char from)
    (let ((column c-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if (not delete-flag)
          (while (< (point) to)
            (end-of-line)
            (if (eq (char-before) ?\\)
                (progn (forward-char -1)
                       (skip-chars-backward " \t")))
            (setq column (max column (1+ (current-column))))
            (forward-line 1)))
      ;; Adjust upward to a tab column, if that doesn't push past the margin.
      (if (> (% column tab-width) 0)
          (let ((adjusted (* (/ (+ column tab-width -1) tab-width) tab-width)))
            (if (< adjusted (window-width))
                (setq column adjusted))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (< (point) endmark)
	(if (and (not delete-flag)
 		 ;; Un-backslashify the last line
 		 ;; if the region ends right at the start of the next line.
 		 (save-excursion
 		   (forward-line 1)
 		   (< (point) endmark)))
            (c-append-backslash column)
          (c-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil))))

(defun c-append-backslash (column)
  (end-of-line)
  (if (eq (char-before) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column))
    (indent-to column)
    (insert "\\")))

(defun c-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))



;;; Line breaking and paragraph filling.

;; The filling code is based on a simple theory; leave the intricacies
;; of the text handling to the currently active mode for that
;; (e.g. adaptive-fill-mode or filladapt-mode) and do as little as
;; possible to make them work correctly wrt the comment and string
;; separators, one-line paragraphs etc.  Unfortunately, when it comes
;; to it, there's quite a lot of special cases to handle which makes
;; the code anything but simple.  The intention is that it will work
;; with any well-written text filling package that preserves a fill
;; prefix.
;;
;; We temporarily mask comment starters and enders as necessary for
;; the filling code to do its job on a seemingly normal text block.
;; We do _not_ mask the fill prefix, so it's up to the filling code to
;; preserve it correctly (especially important when filling C++ style
;; line comments).  By default, we set up and use adaptive-fill-mode,
;; which is standard in all supported Emacs flavors.

(defun c-guess-fill-prefix (lit-limits lit-type)
  ;; Determine the appropriate comment fill prefix for a block or line
  ;; comment.  Return a cons of the prefix string and the column where
  ;; it ends.  If fill-prefix is set, it'll override.  Note that this
  ;; function also uses the value of point in some heuristics.
  (let* ((here (point))
	 (prefix-regexp (concat "[ \t]*\\("
				c-comment-prefix-regexp
				"\\)[ \t]*"))
	 (comment-start-regexp (if (eq lit-type 'c++)
				   prefix-regexp
				 comment-start-skip))
	 prefix-line comment-prefix res)
    (cond
     (fill-prefix
      (setq res (cons fill-prefix
		      ;; Ugly way of getting the column after the fill
		      ;; prefix; it'd be nice with a current-column
		      ;; that works on strings..
		      (let ((buffer-modified (buffer-modified-p))
			    (buffer-undo-list t)
			    (start (point)))
			(unwind-protect
			    (progn
			      (insert ?\n fill-prefix)
			      (current-column))
			  (delete-region start (point))
			  (set-buffer-modified-p buffer-modified))))))
     ((eq lit-type 'c++)
      (save-excursion
	;; Set fallback for comment-prefix if none is found.
	(setq comment-prefix "// ")
	(beginning-of-line)
	(if (> (point) (car lit-limits))
	    ;; The current line is not the comment starter, so the
	    ;; comment has more than one line, and it can therefore be
	    ;; used to find the comment fill prefix.
	    (setq prefix-line (point))
	  (goto-char (car lit-limits))
	  (if (and (= (forward-line 1) 0)
		   (< (point) (cdr lit-limits)))
	      ;; The line after the comment starter is inside the
	      ;; comment, so we can use it.
	      (setq prefix-line (point))
	    ;; The comment is only one line.  Take the comment prefix
	    ;; from it and keep the indentation.
	    (goto-char (car lit-limits))
	    (if (looking-at prefix-regexp)
		(goto-char (match-end 0))
	      (forward-char 2)
	      (skip-chars-forward " \t"))
	    (setq res
		  (if (eq (c-point 'boi) (car lit-limits))
		      ;; There is only whitespace before the comment
		      ;; starter; take the prefix straight from this
		      ;; line.
		      (cons (buffer-substring-no-properties
			     (c-point 'bol) (point))
			    (current-column))
		    ;; There is code before the comment starter, so we
		    ;; have to temporarily insert and indent a new
		    ;; line to get the right space/tab mix in the
		    ;; indentation.
		    (let ((buffer-modified (buffer-modified-p))
			  (buffer-undo-list t)
			  (prefix-len (- (point) (car lit-limits)))
			  tmp)
		      (unwind-protect
			  (progn
			    (goto-char (car lit-limits))
			    (indent-to (prog1 (current-column)
					 (insert ?\n)))
			    (setq tmp (point))
			    (forward-char prefix-len)
			    (cons (buffer-substring-no-properties
				   (c-point 'bol) (point))
				  (current-column)))
			(delete-region (car lit-limits) tmp)
			(set-buffer-modified-p buffer-modified))))
		  )))))
     (t
      (save-excursion
	(beginning-of-line)
	(if (and (> (point) (car lit-limits))
		 (not (and (looking-at "[ \t]*\\*/")
			   (eq (cdr lit-limits) (match-end 0)))))
	    ;; The current line is not the comment starter and
	    ;; contains more than just the ender, so it's good enough
	    ;; to be used for the comment fill prefix.
	    (setq prefix-line (point))
	  (goto-char (car lit-limits))
	  (if (or (/= (forward-line 1) 0)
		  (>= (point) (cdr lit-limits))
		  (and (looking-at "[ \t]*\\*/")
		       (eq (cdr lit-limits) (match-end 0)))
		  (and (looking-at prefix-regexp)
		       (<= (1- (cdr lit-limits)) (match-end 0)))
		  (and (< here (point))
		       (or (not (match-beginning 0))
			   (looking-at "[ \t]*$"))))
	      ;; The comment is either one line or the next line
	      ;; contains just the comment ender.  Also, if point is
	      ;; on the comment opener line and the following line is
	      ;; empty or doesn't match c-comment-prefix-regexp we
	      ;; assume that this is in fact a not yet closed one line
	      ;; comment, so we shouldn't look for the comment prefix
	      ;; on the next line.  In these cases we have no
	      ;; information about a suitable comment prefix, so we
	      ;; resort to c-block-comment-prefix.
	      (setq comment-prefix (or c-block-comment-prefix "")
		    res (let ((buffer-modified (buffer-modified-p))
			      (buffer-undo-list t)
			      tmp-pre tmp-post)
			  ;; The comment doesn't give any information
			  ;; about the indentation column.  We'll have to
			  ;; temporarily insert a new comment line and
			  ;; indent it to find the correct column.
			  (unwind-protect
			      (progn
				(goto-char (car lit-limits))
				(if (looking-at comment-start-regexp)
				    (goto-char (match-end 0))
				  (forward-char 2)
				  (skip-chars-forward " \t"))
				(when (eq (char-syntax (char-before)) ?\ )
				  ;; If there's ws on the current
				  ;; line, we'll use it instead of
				  ;; what's ending comment-prefix.
				  (setq comment-prefix
					(concat (substring comment-prefix
							   0 (string-match
							      "\\s *\\'"
							      comment-prefix))
						(buffer-substring-no-properties
						 (save-excursion
						   (skip-chars-backward " \t")
						   (point))
						 (point)))))
				(setq tmp-pre (point-marker))
				;; We insert an extra non-whitespace
				;; character before the line break and
				;; after comment-prefix in case it's
				;; "" or ends with whitespace.
				(insert "x\n" comment-prefix ?x)
				(setq tmp-post (point-marker))
				(c-indent-line)
				(goto-char (1- tmp-post))
				(cons (buffer-substring-no-properties
					 (c-point 'bol) (point))
				      (current-column)))
			    (when tmp-post
			      (delete-region tmp-pre tmp-post)
			      (set-marker tmp-pre nil)
			      (set-marker tmp-post nil))
			    (set-buffer-modified-p buffer-modified))))
	    ;; Otherwise the line after the comment starter is good
	    ;; enough to find the prefix in.
	    (setq prefix-line (point)))))))
    (or res
	(save-excursion
	  ;; prefix-line is the bol of a line on which we should try
	  ;; to find the prefix.
	  (let* (fb-string fb-endpos	; Contains any fallback prefix found.
		 (test-line
		  (lambda ()
		    (when (and (looking-at prefix-regexp)
			       (< (match-end 0) (1- (cdr lit-limits))))
		      (unless fb-string
			(setq fb-string (buffer-substring-no-properties
					 (match-beginning 0) (match-end 0))
			      fb-endpos (match-end 0)))
		      (unless (eq (match-end 0) (c-point 'eol))
			(throw 'found t))
		      t))))
	    (if (catch 'found
		  ;; Search for a line which has text after the prefix
		  ;; so that we get the proper amount of whitespace
		  ;; after it.  We start with the current line, then
		  ;; search backwards, then forwards.
		  (goto-char prefix-line)
		  (when (and (funcall test-line)
			     (/= (match-end 1) (match-end 0)))
		    ;; If the current line doesn't have text but do
		    ;; have whitespace after the prefix, we'll use it.
		    (throw 'found t))
		  (while (and (zerop (forward-line -1))
			      (> (point) (car lit-limits)))
		    (funcall test-line))
		  (goto-char prefix-line)
		  (while (and (zerop (forward-line 1))
			      (< (point) (cdr lit-limits)))
		    (funcall test-line))
		  nil)
		;; A good line with text after the prefix was found.
		(cons (buffer-substring-no-properties (point) (match-end 0))
		      (progn (goto-char (match-end 0)) (current-column)))
	      (if fb-string
		  ;; A good line wasn't found, but at least we have a
		  ;; fallback that matches the comment prefix regexp.
		  (cond ((string-match "\\s \\'" fb-string)
			 ;; There are ws after the prefix, so let's use it.
			 (cons fb-string
			       (progn (goto-char fb-endpos) (current-column))))
			((progn
			   ;; Check if there's any whitespace padding
			   ;; on the comment start line that we can
			   ;; use after the prefix.
			   (goto-char (car lit-limits))
			   (if (looking-at comment-start-regexp)
			       (goto-char (match-end 0))
			     (forward-char 2)
			     (skip-chars-forward " \t"))
			   (eq (char-syntax (char-before)) ?\ ))
			 (setq fb-string (buffer-substring-no-properties
					  (save-excursion
					    (skip-chars-backward " \t")
					    (point))
					  (point)))
			 (goto-char fb-endpos)
			 (skip-chars-backward " \t")
			 (let ((buffer-modified (buffer-modified-p))
			       (buffer-undo-list t)
			       (tmp (point)))
			   ;; Got to mess in the buffer once again to
			   ;; ensure the column gets correct.  :P
			   (unwind-protect
			       (progn
				 (insert fb-string)
				 (cons (buffer-substring-no-properties
					(c-point 'bol)
					(point))
				       (current-column)))
			     (delete-region tmp (point)))))
			(t
			 ;; Last resort: Just add a single space after
			 ;; the prefix.
			 (cons (concat fb-string " ")
			       (progn (goto-char fb-endpos)
				      (1+ (current-column))))))
		;; The line doesn't match the comment prefix regexp.
		(if comment-prefix
		    ;; We have a fallback for line comments that we must use.
		    (cons (concat (buffer-substring-no-properties
				   prefix-line (c-point 'boi))
				  comment-prefix)
			  (progn (back-to-indentation)
				 (+ (current-column) (length comment-prefix))))
		  ;; Assume we are dealing with a "free text" block
		  ;; comment where the lines doesn't have any comment
		  ;; prefix at all and we should just fill it as
		  ;; normal text.
		  '("" . 0)))))))
    ))

(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment, fill the
comment or the paragraph of it that point is in, preserving the
comment indentation or line-starting decorations (see the
`c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
details).

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

char description[] = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let (lit-limits lit-type fill
	;; beg and end limits the region to be filled.  end is a marker.
	beg end
	;; tmp-pre and tmp-post marks strings that are temporarily
	;; inserted at the start and end of the region.  tmp-pre is a
	;; cons of the positions of the prepended string.  tmp-post is
	;; a marker pointing to the single character of the appended
	;; string.
	tmp-pre tmp-post
	hang-ender-stuck)
    ;; Restore point on undo.  It's necessary since we do a lot of
    ;; hidden inserts and deletes below that should be as transparent
    ;; as possible.
    (if (and buffer-undo-list (not (eq buffer-undo-list t)))
	(setq buffer-undo-list (cons (point) buffer-undo-list)))
    (save-excursion
      (save-restriction
	;; Widen to catch comment limits correctly.
	(widen)
	(setq lit-limits (c-collect-line-comments (c-literal-limits nil t))
	      lit-type (c-literal-type lit-limits)))
      (forward-paragraph)
      (setq end (point-marker))
      (backward-paragraph)
      (setq beg (point)))
    (when (and (>= (point) beg) (<= (point) end))
      (unwind-protect
	  (progn
	    (cond
	     ((eq lit-type 'c++)	; Line comment.
	      (save-excursion
		;; Fill to the comment or paragraph end, whichever
		;; comes first.
		(set-marker end (min end (cdr lit-limits)))
		(when (<= beg (car lit-limits))
		  ;; The region to be filled includes the comment
		  ;; starter, so we must check it.
		  (goto-char (car lit-limits))
		  (back-to-indentation)
		  (if (eq (point) (car lit-limits))
		      ;; Include the first line in the fill.
		      (setq beg (c-point 'bol))
		    ;; The first line contains code before the
		    ;; comment.  We must fake a line that doesn't.
		    (setq tmp-pre t)))
		))
	     ((eq lit-type 'c)		; Block comment.
	      (save-excursion
		(when (>= end (cdr lit-limits))
		  ;; The region to be filled includes the comment ender.
		  (goto-char (cdr lit-limits))
		  (beginning-of-line)
		  (if (and (looking-at (concat "[ \t]*\\("
					       c-comment-prefix-regexp
					       "\\)\\*/"))
			   (eq (cdr lit-limits) (match-end 0)))
		      ;; Leave the comment ender on its own line.
		      (set-marker end (point))
		    ;; The comment ender should hang.  Replace all
		    ;; cruft between it and the last word with a 'x'
		    ;; and include it in the fill.  We'll change it
		    ;; back to a space afterwards.
		    (let ((ender-start (progn
					 (goto-char (cdr lit-limits))
					 (skip-syntax-backward "^w ")
					 (point))))
		      (goto-char (cdr lit-limits))
		      (setq tmp-post (point-marker))
		      (insert ?\n)
		      (set-marker end (point))
		      (forward-line -1)
		      (if (and (looking-at (concat "[ \t]*\\("
						   c-comment-prefix-regexp
						   "\\)[ \t]*"))
			       (eq ender-start (match-end 0)))
			  ;; The comment ender is prefixed by nothing
			  ;; but a comment line prefix.  Remove it
			  ;; along with surrounding ws.
			  nil
			(goto-char ender-start))
		      (skip-chars-backward " \t\r\n")
		      (when (/= (point) ender-start)
			(insert ?x)	; Insert first to keep marks right.
			(delete-region (point) (1+ ender-start))
			(setq hang-ender-stuck t)))))
		(when (<= beg (car lit-limits))
		  ;; The region to be filled includes the comment starter.
		  (goto-char (car lit-limits))
		  (if (looking-at (concat "\\(" comment-start-skip "\\)$"))
		      ;; Begin filling with the next line.
		      (setq beg (c-point 'bonl))
		    ;; Fake the fill prefix in the first line.
		    (setq tmp-pre t)))
		))
	     ((eq lit-type 'string)	; String.
	      (save-excursion
		(when (>= end (cdr lit-limits))
		  (goto-char (1- (cdr lit-limits)))
		  (setq tmp-post (point-marker))
		  (insert ?\n)
		  (set-marker end (point)))
		(when (<= beg (car lit-limits))
		  (goto-char (1+ (car lit-limits)))
		  (setq beg (if (looking-at "\\\\$")
				;; Leave the start line if it's
				;; nothing but an escaped newline.
				(1+ (match-end 0))
			      (point))))))
	     (t (setq beg nil)))
	    (when tmp-pre
	      ;; Temporarily insert the fill prefix after the comment
	      ;; starter so that the first line looks like any other
	      ;; comment line in the narrowed region.
	      (setq fill (c-guess-fill-prefix lit-limits lit-type))
	      (unless (string-match (concat "\\`[ \t]*\\("
					    c-comment-prefix-regexp
					    "\\)[ \t]*\\'")
				    (car fill))
		;; Oops, the prefix doesn't match the comment prefix
		;; regexp.  This could produce very confusing
		;; results with adaptive fill packages together with
		;; the insert prefix magic below, since the prefix
		;; often doesn't appear at all.  So let's warn about
		;; it.
		(message "\
Warning: `c-comment-prefix-regexp' doesn't match the comment prefix %S"
			 (car fill)))
	      ;; Find the right spot on the line, break it, insert
	      ;; the fill prefix and make sure we're back in the
	      ;; same column by temporarily prefixing the first word
	      ;; with a number of 'x'.
	      (save-excursion
		(goto-char (car lit-limits))
		(if (looking-at (if (eq lit-type 'c++)
				    c-comment-prefix-regexp
				  comment-start-skip))
		    (goto-char (match-end 0))
		  (forward-char 2)
		  (skip-chars-forward " \t"))
		(while (< (current-column) (cdr fill)) (forward-char 1))
		(let ((col (current-column)))
		  (setq beg (1+ (point))
			tmp-pre (list (point)))
		  (unwind-protect
		      (progn
			(insert ?\n (car fill))
			(insert (make-string (- col (current-column)) ?x)))
		    (setcdr tmp-pre (point))))))
	    (when beg
	      (let ((fill-paragraph-function
		     ;; Avoid infinite recursion.
		     (if (not (eq fill-paragraph-function 'c-fill-paragraph))
			 fill-paragraph-function))
		    (fill-prefix
		     (or fill-prefix
			 (when (and (eq lit-type 'c++)
				    (not (string-match
					  "\\`[ \t]*//"
					  (or (fill-context-prefix beg end)
					      ""))))
			   ;; Kludge: If the function that adapts the
			   ;; fill prefix doesn't produce the required
			   ;; comment starter for line comments, then
			   ;; force it by setting fill-prefix.
			   (car (or fill (c-guess-fill-prefix
					  lit-limits lit-type)))))))
		;; Preparations finally done!  Now we can call the
		;; real fill function.
		(save-restriction
		  (narrow-to-region beg end)
		  (fill-paragraph arg)))))
	(when (consp tmp-pre)
	  (delete-region (car tmp-pre) (cdr tmp-pre)))
	(when tmp-post
	  (save-excursion
	    (goto-char tmp-post)
	    (delete-char 1)
	    (when hang-ender-stuck
	      (skip-syntax-backward "^w ")
	      (forward-char -1)
	      (insert ?\ )
	      (delete-char 1))
	    (set-marker tmp-post nil)))))
    (set-marker end nil))
  ;; Always return t.  This has the effect that if filling isn't done
  ;; above, it isn't done at all, and it's therefore effectively
  ;; disabled in normal code.
  t)

(defun c-do-auto-fill ()
  ;; Do automatic filling if not inside a context where it should be
  ;; ignored.
  (let ((c-auto-fill-prefix
	 ;; The decision whether the line should be broken is actually
	 ;; done in c-indent-new-comment-line, which do-auto-fill
	 ;; calls to break lines.  We just set this special variable
	 ;; so that we'll know when we're called from there.  It's
	 ;; also used to detect whether fill-prefix is user set or
	 ;; generated automatically by do-auto-fill.
	 fill-prefix))
    (do-auto-fill)))

(defun c-indent-new-comment-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
If inside a comment and `comment-multi-line' is non-nil, the
indentation and line prefix are preserved (see the
`c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
details).  If inside a comment and `comment-multi-line' is nil, a new
comment of the same type is started on the next line and indented as
appropriate for comments.

If a fill prefix is specified, it overrides all the above."
  (interactive)
  (let ((fill-prefix fill-prefix)
	(do-line-break
	 (lambda ()
	   (delete-region (progn (skip-chars-backward " \t") (point))
			  (progn (skip-chars-forward " \t") (point)))
	   (if soft (insert-and-inherit ?\n) (newline 1))))
	;; Already know the literal type and limits when called from
	;; c-context-line-break.
	(c-lit-limits (if (boundp 'c-lit-limits) c-lit-limits))
	(c-lit-type (if (boundp 'c-lit-type) c-lit-type)))
    (when (boundp 'c-auto-fill-prefix)
      ;; Called from do-auto-fill.
      (unless c-lit-limits
	(setq c-lit-limits (c-literal-limits nil nil t)))
      (unless c-lit-type
	(setq c-lit-type (c-literal-type c-lit-limits)))
      (if (memq (cond ((eq c-lit-type 'pound)
		       ;; Come to think about it, "pound" is a bit
		       ;; of a misnomer, so call it "cpp" instead
		       ;; in user interaction.
		       'cpp)
		      ((null c-lit-type) 'code)
		      (t c-lit-type))
		c-ignore-auto-fill)
	  (setq fill-prefix t)		; Used as flag in the cond.
	(if (null c-auto-fill-prefix)
	    (setq fill-prefix nil))))
    (cond ((eq fill-prefix t)
	   ;; A call from do-auto-fill which should be ignored.
	   )
	  (fill-prefix
	   ;; A fill-prefix overrides anything.
	   (funcall do-line-break)
	   (insert-and-inherit fill-prefix))
	  ((progn
	     (unless c-lit-limits
	       (setq c-lit-limits (c-literal-limits nil nil t)))
	     (unless c-lit-type
	       (setq c-lit-type (c-literal-type c-lit-limits)))
	     (memq c-lit-type '(c c++)))
	   (if comment-multi-line
	       ;; Inside a comment that should be continued.
	       (let ((fill (c-guess-fill-prefix
			    (setq c-lit-limits
				  (c-collect-line-comments c-lit-limits))
			    c-lit-type))
		     (pos (point)))
		 (if (save-excursion
		       (back-to-indentation)
		       (> (point) (car c-lit-limits))
		       (looking-at c-comment-prefix-regexp))
		     (progn
		       ;; Skip forward past the fill prefix in case
		       ;; we're standing in it.
		       (while (and (< (current-column) (cdr fill))
				   (not (eolp)))
			 (forward-char 1))
		       (if (> (point) (if (and (eq c-lit-type 'c)
					       (save-excursion
						 (forward-char -2)
						 (looking-at "\\*/")))
					  (- (cdr c-lit-limits) 2)
					(cdr c-lit-limits)))
			   (progn
			     ;; The skip takes us out of the comment;
			     ;; insert the fill prefix at bol instead
			     ;; and keep the position.
			     (setq pos (copy-marker pos t))
			     (beginning-of-line)
			     (insert-and-inherit (car fill))
			     (if soft (insert-and-inherit ?\n) (newline 1))
			     (goto-char pos)
			     (set-marker pos nil))
			 (funcall do-line-break)
			 (insert-and-inherit (car fill))))
		   (funcall do-line-break)
		   (insert-and-inherit (car fill))))
	     ;; Inside a comment that should be broken.
	     (let ((comment-start comment-start)
		   (comment-end comment-end)
		   col)
	       (if (eq c-lit-type 'c)
		   (unless (string-match "[ \t]*/\\*" comment-start)
		     (setq comment-start "/* " comment-end " */"))
		 (unless (string-match "[ \t]*//" comment-start)
		   (setq comment-start "// " comment-end "")))
	       (setq col (save-excursion
			   (back-to-indentation)
			   (current-column)))
	       (funcall do-line-break)
	       (when (and comment-end (not (equal comment-end "")))
		 (forward-char -1)
		 (insert-and-inherit comment-end)
		 (forward-char 1))
	       ;; c-comment-indent may look at the current
	       ;; indentation, so let's start out with the same
	       ;; indentation as the previous one.
	       (indent-to col)
	       (insert-and-inherit comment-start)
	       (indent-for-comment))))
	  (t
	   ;; Somewhere else in the code.
	   (let ((col (save-excursion
			(while (progn (back-to-indentation)
				      (and (looking-at "^\\s *$")
					   (= (forward-line -1) 0))))
			(current-column))))
	     (funcall do-line-break)
	     (indent-to col))))))

(defalias 'c-comment-line-break-function 'c-indent-new-comment-line)
(make-obsolete 'c-comment-line-break-function 'c-indent-new-comment-line)

;; advice for indent-new-comment-line for older Emacsen
(unless (boundp 'comment-line-break-function)
  (defadvice indent-new-comment-line (around c-line-break-advice
					     activate preactivate)
    "Call `c-indent-new-comment-line' if in CC Mode."
    (if (or (boundp 'c-inside-line-break-advice)
	    (not c-buffer-is-cc-mode))
	ad-do-it
      (let (c-inside-line-break-advice)
	(c-indent-new-comment-line (ad-get-arg 0))))))

(defun c-context-line-break ()
  "Do a line break suitable to the context.

When point is outside a comment, insert a newline and indent according
to the syntactic context.

When point is inside a comment, continue it with the appropriate
comment prefix (see the `c-comment-prefix-regexp' and
`c-block-comment-prefix' variables for details).  The end of a
C++-style line comment doesn't count as inside the comment, though."
  (interactive "*")
  (let* ((c-lit-limits (c-literal-limits nil nil t))
	 (c-lit-type (c-literal-type c-lit-limits)))
    (if (or (eq c-lit-type 'c)
	    (and (eq c-lit-type 'c++)
		 (< (point)
		    (1- (cdr (setq c-lit-limits
				   (c-collect-line-comments c-lit-limits)))))))
	(let ((comment-multi-line t)
	      (fill-prefix nil))
	  (c-indent-new-comment-line))
      (delete-region (point) (progn (skip-chars-backward " \t") (point)))
      (newline)
      ;; c-indent-line may look at the current indentation, so let's
      ;; start out with the same indentation as the previous line.
      (let ((col (save-excursion
		   (forward-line -1)
		   (while (progn (back-to-indentation)
				 (and (looking-at "^\\s *$")
				      (= (forward-line -1) 0))))
		   (current-column))))
	(indent-to col))
      (c-indent-line))))


(provide 'cc-cmds)
;;; cc-cmds.el ends here
