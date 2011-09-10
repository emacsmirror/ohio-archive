;;; cc-styles.el --- support for styles in CC Mode

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
(require 'cc-vars)



;; Warning: don't eval-defun this constant or you'll break style inheritance.
(defconst c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 (inline-open . 0)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")
     )
    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("bsd"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 (inline-open . 0)
			 (inexpr-class . 0)
			 ))
     )
    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (label . 0)
			 (statement-cont . +)
			 ))
     )
    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((knr-argdecl-intro . +)
			 (label . 0)
			 (statement-cont . +)
			 (substatement-open . +)
			 (block-open . +)
			 (statement-block-intro . c-lineup-whitesmith-in-block)
			 (block-close . c-lineup-whitesmith-in-block)
			 (inline-open . +)
			 (defun-open . +)
			 (defun-block-intro . c-lineup-whitesmith-in-block)
			 (defun-close . c-lineup-whitesmith-in-block)
			 (brace-list-open . +)
			 (brace-list-intro . c-lineup-whitesmith-in-block)
			 (brace-entry-open . c-indent-multi-line-block)
			 (brace-list-close . c-lineup-whitesmith-in-block)
			 (class-open . +)
			 (inclass . c-lineup-whitesmith-in-block)
			 (class-close . +)
			 (inexpr-class . 0)
			 (extern-lang-open . +)
			 (inextern-lang . c-lineup-whitesmith-in-block)
			 (extern-lang-close . +)
			 (namespace-open . +)
			 (innamespace . c-lineup-whitesmith-in-block)
			 (namespace-close . +)
			 ))
     )
    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)))
     (c-offsets-alist . ((topmost-intro        . 0)
                         (topmost-intro-cont   . 0)
                         (substatement         . +)
			 (substatement-open    . 0)
                         (case-label           . +)
                         (access-label         . -)
                         (inclass              . ++)
                         (inline-open          . 0)
                         ))
     )
    ("linux"
     (c-basic-offset  . 8)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)))
     (c-cleanup-list . (brace-else-brace))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro     . 0)
			 (substatement-open     . 0)
			 (label                 . 0)
			 (statement-cont        . +)
			 ))
     )
    ("python"
     (indent-tabs-mode . t)
     (fill-column      . 78)
     (c-basic-offset   . 8)
     (c-offsets-alist  . ((substatement-open . 0)
			  (inextern-lang . 0)
			  (arglist-intro . +)
			  (knr-argdecl-intro . +)
			  ))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)
				))
     (c-block-comment-prefix . "")
     )
    ("java"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     ;; the following preserves Javadoc starter lines
     (c-offsets-alist . ((inline-open . 0)
			 (topmost-intro-cont    . +)
			 (statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
 			 (substatement-open     . +)
 			 (label                 . +)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro  . c-lineup-arglist-intro-after-paren)
 			 (arglist-close  . c-lineup-arglist)
 			 (access-label   . 0)
			 (inher-cont     . c-lineup-java-inher)
			 (func-decl-cont . c-lineup-java-throws)
			 ))
     )
    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING [BASE-STYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.

Optional BASE-STYLE if present, is a string and must follow
STYLE-STRING.  BASE-STYLE names a style that this style inherits from.
By default, all styles inherit from the \"user\" style, which is
computed at run time.  Style loops generate errors.

Two variables are treated specially.  When VARIABLE is
`c-offsets-alist', the VALUE is a list containing elements of the
form:

  (SYNTACTIC-SYMBOL . OFFSET)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

When VARIABLE is `c-special-indent-hook', its VALUE is added to
`c-special-indent-hook' using `add-hook'.  If VALUE is a list, each
element of the list is added with `add-hook'.

Do not change this variable directly.  Use the function `c-add-style'
to add new styles or modify existing styles (it is not a good idea to
modify existing styles -- you should create a new style that inherits
the existing style.")



;; Functions that manipulate styles
(defun c-set-style-1 (conscell dont-override)
  ;; Set the style for one variable
  (let ((attr (car conscell))
	(val  (cdr conscell)))
    (cond
     ;; first special variable
     ((eq attr 'c-offsets-alist)
      (mapcar
       (function
	(lambda (langentry)
	  (let ((langelem (car langentry))
		(offset (cdr langentry)))
	    (unless (and dont-override
			 (assq langelem c-offsets-alist))
	      (c-set-offset langelem offset))
	    )))
       val))
     ;; second special variable
     ((eq attr 'c-special-indent-hook)
      (if (eq c-special-indent-hook 'set-from-style)
	  (setq c-special-indent-hook nil))
      (if (listp val)
	  (while val
	    (add-hook 'c-special-indent-hook (car val))
	    (setq val (cdr val)))
	(add-hook 'c-special-indent-hook val)))
     ;; all other variables
     (t (if (or (not dont-override)
		(not (memq attr c-style-variables))
		(eq (symbol-value attr) 'set-from-style))
	    (set attr val))))
    ))

(defun c-set-style-2 (style basestyles dont-override)
  ;; Recursively set the base style.  If no base style is given, the
  ;; default base style is "user" (a.k.a. "cc-mode") and the recursion
  ;; stops.  Be sure to detect loops.
  (let ((vars (cdr (or (assoc (downcase style) c-style-alist)
		       (assoc (upcase style) c-style-alist)
		       (assoc style c-style-alist)
		       (error "Undefined style: %s" style)))))
    (when dont-override
      (mapcar (lambda (conscell)
		(c-set-style-1 conscell dont-override))
	      vars))
    (if (not (string-equal style "user"))
	(let ((base (if (stringp (car vars))
			(prog1
			    (downcase (car vars))
			  (setq vars (cdr vars)))
		      "user")))
	  (if (memq base basestyles)
	      (error "Style loop detected: %s in %s" base basestyles))
	  (c-set-style-2 base (cons base basestyles) dont-override)))
    (unless dont-override
      (mapcar (lambda (conscell)
		(c-set-style-1 conscell dont-override))
	      vars))))
    
(defvar c-set-style-history nil)

;;;###autoload
(defun c-set-style (stylename &optional dont-override)
  "Set CC Mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles.

The variable `c-indentation-style' always contains the buffer's current
style name.

If the optional argument DONT-OVERRIDE is non-nil, no style variables
will be overridden.  I.e. in the case of `c-offsets-alist', syntactic
symbols will only be added, not overridden, and in the case of all
other style variables, only those set to 'set-from-style can be
reassigned."
  (interactive (list (let ((completion-ignore-case t)
			   (prompt (format "Which %s indentation style? "
					   mode-name)))
		       (completing-read prompt c-style-alist nil t
					(cons c-indentation-style 0)
					'c-set-style-history))))
  (c-initialize-builtin-style)
  (c-set-style-2 stylename nil dont-override)
  (setq c-indentation-style stylename)
  (c-keep-region-active))

;;;###autoload
(defun c-add-style (style descrip &optional set-p)
  "Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " c-style-alist
				     nil nil nil 'c-set-style-history))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style c-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq c-style-alist (cons (cons style descrip) c-style-alist))))
  (and set-p (c-set-style style)))



(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  (cond
   ((eq offset '+)         (setq offset c-basic-offset))
   ((eq offset '-)         (setq offset (- c-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 c-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- c-basic-offset))))
   ((eq offset '*)         (setq offset (/ c-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- c-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (c-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		(if c-strict-syntax-p
		    (error "No offset found for syntactic symbol %s" symbol)
		  0)
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset)))
   )
  offset)

(defun c-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; c-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(if c-strict-syntax-p
	    (error "No offset found for syntactic symbol %s" symbol)
	  (setq offset 0
		relpos 0))
      (setq offset (c-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (c-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (or (and (numberp offset) offset)
	   (and (symbolp offset) (symbol-value offset))
	   0))
    ))



(defvar c-read-offset-history nil)

(defun c-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let* ((oldoff  (cdr-safe (or (assq langelem c-offsets-alist)
				(assq langelem (get 'c-offsets-alist
						    'c-stylevar-fallback)))))
	 (symname (symbol-name langelem))
	 (defstr  (format "(default %s): " oldoff))
	 (errmsg  (concat "Offset must be int, func, var, list, "
			  "or [+,-,++,--,*,/] "
			  defstr))
	 (prompt (concat symname " offset " defstr))
	 offset input interned raw)
    (while (not offset)
      (setq input (completing-read prompt obarray 'fboundp nil nil
				   'c-read-offset-history)
	    offset (cond ((string-equal "" input) oldoff)  ; default
			 ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-equal "*" input) '*)
			 ((string-equal "/" input) '/)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ;; a symbol with a function binding
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ;; a lambda function
			 ((c-safe (functionp (setq raw (read input))))
			  raw)
			 ;; a symbol with variable binding
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

;;;###autoload
(defun c-set-offset (symbol offset &optional ignored)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  The optional argument is not used
and exists only for compatibility reasons."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     #'(lambda (langelem)
			 (cons (format "%s" (car langelem)) nil))
		     (get 'c-offsets-alist 'c-stylevar-fallback))
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (c-guess-basic-syntax))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (cons ic 0))
		    )))
	  (offset (c-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (unless (c-valid-offset offset)
    (error "Offset must be int, func, var, list, or in [+,-,++,--,*,/]: %s"
	   offset))
  (let ((entry (assq symbol c-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if (assq symbol (get 'c-offsets-alist 'c-stylevar-fallback))
	  (setq c-offsets-alist (cons (cons symbol offset) c-offsets-alist))
	(error "%s is not a valid syntactic symbol" symbol))))
  (c-keep-region-active))



(defun c-copy-tree (tree)
  (if (consp tree)
      (cons (c-copy-tree (car tree))
            (c-copy-tree (cdr tree)))
    tree))

(defun c-initialize-builtin-style ()
  ;; Dynamically append the default value of most variables. This is
  ;; crucial because future c-set-style calls will always reset the
  ;; variables first to the `cc-mode' style before instituting the new
  ;; style.  Only do this once!
  (unless (get 'c-initialize-builtin-style 'is-run)
    (put 'c-initialize-builtin-style 'is-run t)
    (c-initialize-cc-mode)
    (or (assoc "cc-mode" c-style-alist)
	(assoc "user" c-style-alist)
	(progn
	  (c-add-style "user"
		       (mapcar
			(lambda (var)
			  (let ((val (symbol-value var)))
			    (cons var
				  (if (eq var 'c-offsets-alist)
				      (mapcar
				       (lambda (langentry)
					 (setq langentry
					       (or (assq (car langentry) val)
						   langentry))
					 (cons (car langentry)
					       (cdr langentry)))
				       (get var 'c-stylevar-fallback))
				    (c-copy-tree
				     (if (eq val 'set-from-style)
					 (get var 'c-stylevar-fallback)
				       val))))))
			c-style-variables))
	  (c-add-style "cc-mode" '("user"))
	  ))
    (if c-style-variables-are-local-p
	(c-make-styles-buffer-local))))

(defun c-make-styles-buffer-local (&optional this-buf-only-p)
  "Make all CC Mode style variables buffer local.
If you edit primarily one style of C (or C++, Objective-C, Java, etc)
code, you probably want style variables to be global.  This is the
default.

If you edit many different styles of C (or C++, Objective-C, Java,
etc) at the same time, you probably want the CC Mode style variables
to be buffer local.  If you do, it's advicable to set any CC Mode
style variables in a hook function (e.g. off of `c-mode-common-hook'),
instead of at the top level of your ~/.emacs file.

This function makes all the CC Mode style variables buffer local.
Call it after CC Mode is loaded into your Emacs environment.
Conversely, set the variable `c-style-variables-are-local-p' to t in
your .emacs file, before CC Mode is loaded, and this function will be
automatically called when CC Mode is loaded.

Optional argument, when non-nil, means use `make-local-variable'
instead of `make-variable-buffer-local'."
  ;; style variables
  (let ((func (if this-buf-only-p
		  'make-local-variable
		'make-variable-buffer-local))
	(varsyms (cons 'c-indentation-style (c-copy-tree c-style-variables))))
    (delq 'c-special-indent-hook varsyms)
    (mapcar func varsyms)
    ;; Hooks must be handled specially
    (if this-buf-only-p
	(make-local-hook 'c-special-indent-hook)
      (make-variable-buffer-local 'c-special-indent-hook))
    ))



(provide 'cc-styles)
;;; cc-styles.el ends here
