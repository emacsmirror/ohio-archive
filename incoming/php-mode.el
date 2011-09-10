;;; php-mode.el -- major mode for editing PHP source files

;; Author: Fred Yankowski <fcy@acm.org>
;; Keywords: PHP, PHP3, languages
;; $Id: php-mode.el,v 1.18 2000/03/28 03:14:31 fred Exp $

;; php-mode.el is Copyright (c) 1999 by Fred Yankowski <fcy@acm.org>
;;	
;;	This is free software; you can redistribute it and/or modify
;;	it under the terms of the GNU General Public License as
;;	published by the Free Software Foundation; either version 2,
;;	or (at your option) any later version.
;;	
;;	This is distributed in the hope that it will be useful, but
;;	WITHOUT ANY WARRANTY; without even the implied warranty of
;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;	GNU General Public License for more details.
;;	
;;	You should have received a copy of the GNU General Public
;;	License as the file COPYING.  If not, write to the Free
;;	Software Foundation, Inc., 59 Temple Place - Suite 330,
;;	Boston, MA 02111-1307, USA.

;;; Commentary;
;;
;; PHP mode is a major mode for editing the PHP programming language
;; <www.php.net>.  It is mostly concerned with setting up syntax
;; coloring via the font-lock library.
;;
;; To use PHP mode, add this to your ~/.emacs file:
;;
;;	(autoload 'php-mode "php-mode" "PHP editing mode" t)
;;	(add-to-list 'auto-mode-alist '("\\.php3$" . php-mode))
;;
;; Repeat the second line for any other filename suffixes that you
;; want to associate with PHP mode.  Then, install this file in some
;; directory in your Emacs load-path and run byte-compile-file on it.
;; Voila'.

;; Xemacs users report that regexp-opt is not defined.
(eval-when-compile
  (unless (fboundp 'regexp-opt)
    (defun regexp-opt (strings paren)
      (let ((open-paren (if paren "\\(" ""))
	    (close-paren (if paren "\\)" "")))
	(concat open-paren
		(mapconcat 'regexp-quote strings "\\|")
		close-paren)))))

(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version) "\
Non nil if using XEmacs.")

(let* ((php-keywords
	(eval-when-compile
	  (regexp-opt
	   '("and" "break" "case" "continue" "default" "do" "echo"
	     "else" "elseif" "exit" "extends" "for" "global" "if" "include"
	     "or" "require" "return" "static" "switch" "then"
	     "var" "while" "xor") t)))
       ;; "new" and "extends" get special treatment below

       (php-constants
	(eval-when-compile
	  (regexp-opt
	   '("FALSE" "false" "PHP_VERSION" "TRUE" "true"
	     "__LINE__" "__FILE__") t))) 

       (php-types
	(eval-when-compile
	  (regexp-opt
	   '("int" "integer" "real" "double" "float"
	     "string" "array" "object") t)))
       )

  (defconst php-font-lock-keywords-1
   (list
    '("^[ \t]*\\(class\\)[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     
    '("^[ \t]*\\(function\\)[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ))

  (defconst php-font-lock-keywords-2
   (append php-font-lock-keywords-1
    (list
     (concat "\\<\\(" php-keywords "\\)\\>")

     `(,(concat "\\<\\(" php-constants "\\)\\>")
       1 font-lock-constant-face)

     ;; handle several words specially, to include following word,
     ;; thereby excluding it from unknown-symbol checks later
     '("\\<\\(new\\|extends\\)\\s-+\\$?\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 default))

     ;; treat 'print' as keyword only when not used like a function name
     '("\\<print\\s-*(" . default)
     '("\\<print\\>" . font-lock-keyword-face)

     '("<\\?php" . font-lock-constant-face)
     '("\\?>" . font-lock-constant-face)
     )))

  (defconst php-font-lock-keywords-3
   (append
    (list
     ;; warn about 'new FooBar()' -- empty parens are tempting but wrong
     '("\\<\\(new\\)\\s-+\\(\\sw+\\)\\((\\s-*)\\)"
       (1 font-lock-keyword-face) (2 default) (3 font-lock-warning-face))
     )
    php-font-lock-keywords-2
    (list
     '("</?\\sw+>?" . font-lock-constant-face) ; <word> or </word>

     ;; warn about '$' immediately after ->
     '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
       (1 font-lock-warning-face) (2 default))

     ;; warn about $word.word -- it could be a valid concatenation,
     ;; but without any spaces we'll assume $word->word was meant.
     '("\\$\\sw+\\(\\.\\)\\sw"
       1 font-lock-warning-face)

     ;; exclude casts from bare-word treatment
     `(,(concat "(\\(" php-types "\\))")
       1 default)

     ;; Warn about bare symbols, those that don't follow '$' or precede
     ;; '('.  But first explicitly mark some words that are OK.
     '("->\\s-*\\sw+" . default)	; -->word
     '("\\$\\sw+" . default)		; $word
     '("\\<\\sw+\\s-*[[(]" . default)	; word( or word[
     '("\\<[0-9]+" . default)		; number (also matches word)
     '("\\<\\sw+\\>" . font-lock-warning-face)

     ;; Warn about ==> instead of => (why do I *do* that?)
     '("==+>" . font-lock-warning-face)
     ))))

(defconst php-font-lock-syntactic-keywords
  (if xemacsp nil
    ;; Mark shell-style comments.  font-lock handles this in a
    ;; separate pass from normal syntactic scanning (somehow), so we
    ;; get a chance to mark these in addition to C and C++ style
    ;; comments.  This only works in GNU Emacs, not Xemacs 21 which
    ;; seems to ignore this same code if we try to use it.
    (list
     ;; Mark _all_ # chars as being comment-start.  That will be
     ;; ignored when inside a quoted string.
     '("\\(\#\\)"
       (1 (11 . nil)))
     ;; Mark all newlines ending a line with # as being comment-end.
     ;; This causes a problem, premature end-of-comment, when '#'
     ;; appears inside a multiline C-style comment.  Oh well.
     '("#.*\\([\n]\\)"
       (1 (12 . nil)))
     )))

(define-derived-mode php-mode c-mode "PHP"
  "A major mode for editing PHP source code.

Key bindings:
\\{php-mode-map}"

  (setq comment-start "// "
	comment-end   ""
	comment-start-skip "// *")

  (defvar php-mode-syntax-table php-mode-syntax-table)

  (modify-syntax-entry ?_ "w" php-mode-syntax-table)
  ;; underscore considered part of word
  (modify-syntax-entry ?$ "." php-mode-syntax-table)
  ;; dollar-sign considered punctuation, not part of word

  (if xemacsp (progn
		(modify-syntax-entry ?# "< b" php-mode-syntax-table)
		(modify-syntax-entry ?\n "> b" php-mode-syntax-table)))
  ;; The above causes Xemacs to handle shell-style comments correctly,
  ;; but fails to work in GNU Emacs which fails to interpret \n as the
  ;; end of the comment.

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((php-font-lock-keywords-1
	   php-font-lock-keywords-2
	   ;; Comment-out the next line if the font-coloring is too
	   ;; extreme/ugly for you.
	   php-font-lock-keywords-3
	   )
	  nil				; KEYWORDS-ONLY: no
	  nil				; CASE-FOLD: no
	  nil				; SYNTAX-ALIST
	  nil				; SYNTAX-BEGIN
	  (font-lock-syntactic-keywords . php-font-lock-syntactic-keywords)))
)


(unless (boundp 'default)
  (defvar default 'default))
;; Created "default" symbol for GNU Emacs so that both Xemacs and GNU
;; emacs can refer to the default face by a variable named "default".

(unless (boundp 'font-lock-keyword-face)
  (copy-face 'bold 'font-lock-keyword-face))
;; font-lock-keyword-face is sure to be valid now, assuming that the
;; bold face exists

(unless (boundp 'font-lock-constant-face)
  (copy-face 'font-lock-keyword-face 'font-lock-constant-face))
;; font-lock-constant-face now exists, which Xemacs doesn't seem to have
;; by default

(provide 'php-mode)
