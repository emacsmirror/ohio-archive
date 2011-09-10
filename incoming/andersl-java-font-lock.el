;; andersl-java-font-lock.el -- Font lock support for Java.

;; Copyright (C) 1996 Anders Lindgren

;; Author: Anders Lindgren <andersl@csd.uu.se>
;; Maintainer: Anders Lindgren <andersl@csd.uu.se>
;; Created:  5 Aug 1996
;; Version: 1.0
;; Keywords: font-lock, java, languages
;; Date: 29 Aug 1996

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package supplies state-of-the-art font-lock support for the
;; language Java.
;;
;; In addition to normal fontification -- keywords, class names,
;; method names, labels, case statements etc. -- this package is
;; capable of fontifying declared variables and types, including type
;; casts.
;;
;; A type is either a primitive type or a capitalized word, possibly
;; fully quantified, e.g. `int', `Applet', and `java.awt.Color'.  Due
;; the the strong structure of Java, this package is capable of
;; distinguishing between types and normal capitalized variables!
;;
;; As an extra feature, Javadoc tags are fontified.
;;
;; This package has been inspired by, although not based on, other
;; packages for font-locking Java.


;; Examples:
;;
;;     for (int i = 0; i < 10; i++)
;;     ^^^  ^^^ ^
;;      |    |  |-- Variable face
;;      |    |----- Type face
;;      |---------- Keyword face
;;
;;
;;     foo.bar.MyType [] foo = new foo.bar.MyType[10];
;;     ^^^ ^^^ ^^^^^^    ^^^   ^^^ ^^^ ^^^ ^^^^^^
;;      |   |     |       |     |   |   |     |
;;      |   |     |       |     |   |   |     +---- Type face
;;      |   |     |       |     |   +---+---------- Reference face
;;      |   |     |       |     +------------------ Keyword face
;;      |   |     |       +------------------------ Variable name face
;;      |   |     +-------------------------------- Type face
;;      +---+-------------------------------------- Reference face
;;
;;     /* @param foo The foo:th bar we visited. */
;;        ^^^^^^ ^^^
;;           |    |
;;           |    +------- Variable name face
;;           +------------ Reference face


;; Installation:
;;
;; Place the following lines in the appropriate init file, for example
;; your ~/.emacs file.  The last line enables full fontification which
;; requires some computer power; remove it if your computer is slow or
;; if you simply don't want your Emacs to look like a Christmas tree.
;;
;;     (add-hook 'java-mode-hook 'my-java-mode-hook)
;;
;;     (defun my-java-mode-hook ()
;;       (cond (window-system
;;              (require 'andersl-java-font-lock)
;;              (turn-on-font-lock))))
;;
;;     (setq font-lock-maximum-decoration t)


;; Home, sweet home:
;;
;; The latest version of this package can be found at:
;;    http:\\www.csd.uu.se\~andersl\emacs.shtml
;;    ftp:\\ftp.csd.uu.se\pub\users\andersl\emacs\
;;
;; While you're visiting, take a look at some of my other packages.


;; Reporting bugs:
;;
;; Should you find a bug you think is related to this package, please
;; report to:
;;
;;        andersl@csd.uu.se
;;
;; When sending a bug report, please:
;;
;; * Describe exactly what you did to invoke the bug making it possible
;;   for me to reproduce it.
;;
;; * Describe what you see.
;;
;; * Describe what you expected to see.
;;
;; * Place the word "java-font-lock" in the subject line.


;; Theory of Operation:
;;
;; This package is based on some observations:
;;
;; * Object types are often capitalized. (The "capitalized
;;   type assumption".)
;;
;; * The java grammar does not have any rule on the form
;;   expr -> expr expr
;;
;; * Numerical expressions cannot be converted to object type.
;;
;;
;; From these observations, a couple of conclusions can be drawn.
;;
;; * Lines on the form: `Word word' must be type declarations.
;;
;; * `(Word)' must be a cast if it is followed by an identifier or an
;;   expression inside a pair of parentheses.

;;; Code:

(defvar java-font-lock-keywords nil
  "Default font lock keywords for the Java language.")
(defvar java-font-lock-keywords-1 nil
  "Font lock keywords for the Java language.")
(defvar java-font-lock-keywords-2 nil
  "Font lock, additional support for types and variables.")
(defvar java-font-lock-keywords-3 nil
  "Font lock, additional support for javadoc tags.")


;; Note: The regexp parser might run out of stack, should
;; `java-primitive-type-regexp' and `java-class-type-regexp' be
;; combined.

(defvar java-primitive-type-regexp
  (concat "\\<\\(b\\(oolean\\|yte\\)\\|char\\|double\\|float\\|int"
	  "\\|long\\|short\\|void\\)\\>")
  "Regexp which matches primitive types.")
(defvar java-primitive-type-regexp-count 2
  "Number of pairs of parentheses in `java-primitive-type-regexp'.")


(defvar java-class-type-regexp nil
  "Regexp which matches names of classes or interfaces.
The name is assumed to begin with a capital letter.")
(defvar java-class-type-regexp-count nil)


(defvar java-identifier-regexp nil
  "Regexp which matches Java identifiers.")
(defvar java-identifier-regexp-count nil)


(let ((capital-letter "A-Z\300-\326\330-\337")
      (letter "a-zA-Z_$\300-\326\330-\366\370-\377")
      (digit  "0-9"))

  (setq java-identifier-regexp
	(concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))
  (setq java-identifier-regexp-count 1)

  (setq java-class-type-regexp
	(concat "\\<\\([" capital-letter "][" letter digit "]*\\)\\>"))
  (setq java-class-type-regexp-count 1))


(let ((java-modifier-regexp
       (concat "\\<\\(abstract\\|const\\|final\\|native\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	       "s\\(tatic\\|ynchronized\\)\\|transient\\|volatile\\)\\>"))
      (java-modifier-regexp-count 4))

  ;; "Normal" font-lock support:
  (setq java-font-lock-keywords-1
	(list

	 ;; Keywords:
	 (list
	  (concat
	   "\\<\\("
	   "b\\(reak\\|yvalue\\)\\|"
	   "c\\(a\\(se\\|tch\\)\\|lass\\|ontinue\\)\\|"
	   "do\\|e\\(lse\\|xtends\\)\\|"
	   "f\\(inally\\|or\\)\\|"
	   "i\\(f\\|mp\\(lements\\|ort\\)\\|n\\(stanceof\\|terface\\)\\)\\|"
	   "new\\|package\\|return\\|switch\\|"
	   "t\\(hrows?\\|ry\\)\\|while\\)\\>")
	  '(1 font-lock-keyword-face))

	 ;; Modifiers:
	 (list java-modifier-regexp '(1 font-lock-type-face))

	 ;; Special constants:
	 '("\\<\\(this\\|super\\)\\>" (1 font-lock-reference-face))
	 '("\\<\\(false\\|null\\|true\\)\\>" (1 font-lock-reference-face))

	 ;; Class names:
	 (list (concat "\\<class\\>\\s *" java-identifier-regexp)
	       '(1 font-lock-function-name-face))

	 ;; Package declarations:
	 (list (concat "\\<\\(package\\|import\\)\\>\\ s*"
		       java-identifier-regexp)
	       '(2 font-lock-reference-face)
	       (list (concat "\\=\\.\\(" java-identifier-regexp "\\)")
		     nil nil '(1 (if (eq (char-after (match-end 0)) ?.)
				     'font-lock-reference-face
				   'font-lock-type-face))))

	 ;; Keywords followed by type.
	 (list (concat "\\<\\(extends\\|instanceof\\|new\\)\\>\\ s*"
		       java-identifier-regexp)
	       '(2 (if (eq (char-after (match-end 0)) ?.)
		       'font-lock-reference-face 'font-lock-type-face))
	       (list (concat "\\=\\." java-identifier-regexp)
		     '(goto-char (match-end 0)) nil
		     '(1 (if (eq (char-after (match-end 0)) ?.)
			     'font-lock-reference-face 'font-lock-type-face))))

	 ;; Keywords followed by type list.
	 (list (concat "\\<\\(implements\\|throws\\)\\>\\ s*"
		       java-identifier-regexp)
	       '(2 (if (eq (char-after (match-end 0)) ?.)
		       'font-lock-reference-face 'font-lock-type-face))
	       (list (concat "\\=\\(\\.\\|\\s *\\(,\\)\\s *\\)"
			     java-identifier-regexp)
		     '(goto-char (match-end 0)) nil
		     '(3 (if (eq (char-after (match-end 0)) ?.)
			     'font-lock-reference-face 'font-lock-type-face))))

	 ;; Constructors:
	 (list (concat
		"^\\s *\\(" java-modifier-regexp "\\s *\\)*"
		java-class-type-regexp "\\s *(")
	       (list (+ 2 java-modifier-regexp-count)
		     '(condition-case nil
			  (save-excursion
			    (goto-char (scan-sexps (- (match-end 0) 1) 1))
			    (and (looking-at "\\s *\\($\\|\\<throws\\>\\|{\\)")
				 'font-lock-function-name-face))
			(error 'font-lock-function-name-face))))

	 ;; Methods:
	 (list (concat java-primitive-type-regexp
		       "\\s *\\(\\[\\s *\\]\\s *\\)*"
		       java-identifier-regexp "\\s *(")
	       (list (+ 2 java-primitive-type-regexp-count)
		     'font-lock-function-name-face))
	 (list (concat java-class-type-regexp
		       "\\s *\\(\\[\\s *\\]\\s *\\)*"
		       java-identifier-regexp "\\s *(")
	       (list (+ 2 java-class-type-regexp-count)
		     'font-lock-function-name-face))

	 ;; Labels:
	 (list ":"
	       (list
		(concat "^\\s *" java-identifier-regexp "\\s *:")
		'(beginning-of-line) '(end-of-line)
		'(1 font-lock-reference-face)))

	 ;; `break' and `continue' destination label:
	 (list (concat "\\<\\(break\\|continue\\)\\>\\s *"
		       java-identifier-regexp)
	       '(2 font-lock-reference-face))

	 ;; Case statements:
	 ;; In Java, any constant expression is allowed.
	 '("\\<case\\>\\s *\\(.*\\):" (1 font-lock-reference-face))))

  ;; Types and declared variables:
  (setq java-font-lock-keywords-2
	(append
	 java-font-lock-keywords-1
	 (list

	  ;; primitive type, can't be confused with anyhting else.
	  (list java-primitive-type-regexp
		'(1 font-lock-type-face)
		'(font-lock-match-java-style-declaration-item-and-skip-to-next
		  (goto-char (match-end 0))
		  (goto-char (match-end 0))
		  (1 font-lock-variable-name-face)))

	  ;; Class type, or just a capitalized variable?
	  ;;
	  ;; Declarations are easy to recognize.  Capitalized words
	  ;; followed by a `)' are treated as casts if they also are
	  ;; followed by an expression.  Note that expressions
	  ;; starting with a numerical unary operator, e.g. +, can't
	  ;; be casted to an object type.
	  ;;
	  ;; The path of a fully quantified type, e.g. java.lang.Foo, is
	  ;; fontified in reference face.
	  ;;
	  ;; An access to a static field, e.g. System.out.println, is
	  ;; not fontified since it can't be distinguished from the
	  ;; usage of a capitalized variable, e.g. Foo.out.println.

	  (list (concat java-class-type-regexp
			"\\s *\\(\\[\\s *\\]\\s *\\)*"
			"\\(\\<\\|$\\|)\\s *\\([(\"]\\|\\<\\)\\)")
		'(1 (save-match-data
		      (save-excursion
			(goto-char
			 (match-beginning (+ 2 java-class-type-regexp-count)))
			(and (not (looking-at "\\<instanceof\\>"))
			     'font-lock-type-face))))
		(list (concat "\\=" java-identifier-regexp "\\.")
		      '(progn
			 (goto-char (match-beginning 0))
			 (while (or (eq (preceding-char) ?.)
				    (eq (char-syntax (preceding-char)) ?w))
			   (backward-char)))
		      '(goto-char (match-end 0))
		      '(1 font-lock-reference-face)
		      '(0 nil))		; Workaround for bug in XEmacs.
		'(font-lock-match-java-style-declaration-item-and-skip-to-next
		  (goto-char (match-end 1))
		  (goto-char (match-end 0))
		  (1 font-lock-variable-name-face))))))

  ;; Javadoc tags:
  (setq java-font-lock-keywords-3
	(append
	 java-font-lock-keywords-2
	 (list
	  '("\\(@\\(author\\|exception\\|param\\|return\\|see\\|version\\)\\)"
	    (1 font-lock-reference-face t))
	  (list (concat "@\\(param\\)\\s *" java-identifier-regexp)
		'(2 font-lock-variable-name-face t)))))

  (setq java-font-lock-keywords java-font-lock-keywords-1))



(defun font-lock-match-java-style-declaration-item-and-skip-to-next (limit)
  "Match, and skip over, variable definitions."
  (if (looking-at "\\s *\\(\\[\\s *\\]\\s *\\)*")
      (goto-char (match-end 0)))
  (and
   (looking-at java-identifier-regexp)
   (save-match-data
     (not (string-match java-primitive-type-regexp
			(buffer-substring (match-beginning 1)
					  (match-end 1)))))
   (save-match-data
     (save-excursion
       (goto-char (match-beginning 1))
       (not (looking-at
	     (concat java-class-type-regexp
		     "\\s *\\(\\[\\s *\\]\\s *\\)*\\<")))))
   (save-match-data
     (condition-case nil
	 (save-restriction
	   (narrow-to-region (point-min) limit)
	   (goto-char (match-end 0))
	   ;; Note: Both `scan-sexps' and the second goto-char can
	   ;; generate an error which is caught by the
	   ;; `condition-case' expression.
	   (while (not (looking-at "\\s *\\(\\(,\\)\\|;\\|$\\)"))
	     (goto-char (or (scan-sexps (point) 1) (point-max))))
	   (goto-char (match-end 2)))	; non-nil
       (error t)))))


;;; Inform font-lock that we're around.
;; This code works only on modern versions of Emacs. On older systems
;; you have to manually set the variables `font-lock-keywords' and
;; `font-lock-syntax-table', or upgrade your Emacs.

(require 'font-lock)

(if (not (assq 'java-mode font-lock-defaults-alist))
    (setq font-lock-defaults-alist
	  (cons
	   (cons 'java-mode

		 ;; java-mode-defaults
		 '((java-font-lock-keywords java-font-lock-keywords-1
		    java-font-lock-keywords-2 java-font-lock-keywords-3)
		   nil nil ((?_ . "w") (?$ . "w")) nil
		   (font-lock-mark-block-function . mark-defun)))

	   font-lock-defaults-alist)))


(provide 'andersl-java-font-lock)

;; andersl-java-font-lock.el ends here.
