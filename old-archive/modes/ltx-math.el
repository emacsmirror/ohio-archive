; Convenient shortcuts for editing LaTeX math text.
; 
; "`" acts as a prefix character.
; "` a" inserts "\alpha". "` `" insert "`".
; Press "C-h f LaTeX-math-mode RET" for a complete list.
; 
; You can get a reference card from
;   ftp://ftp.iesd.auc.dk/pub/emacs-lisp/math-ref.texi
; 
; This package requires GNU Emacs 19 or Lucid 19.10 or later.
; 
;;; ltx-math.el - Smart abbrevation for common math expressions.

;; Copyright (C) 1991 Kresten Krab Thorup (krab@iesd.auc.dk).
;; Copyright (C) 1993 Per Abrahamsen

;; Maintainer: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: $Id: ltx-math.el,v 5.8 1994/07/30 05:39:25 amanda Exp $
;; Keywords: wp

;; LCD Archive Entry:
;; ltx-math|Per Abrahamsen|abraham@iesd.auc.dk|
;; Abbrevations for math symbols in LaTeX|
;; 30-Jul-1994|5.8|~/modes/ltx-math.el.Z|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Convenient shortcuts for editing LaTeX math text.  

;; "`" acts as a prefix character.
;; "` a" inserts "\alpha". "` `" insert "`".
;; Press "C-h f LaTeX-math-mode RET" for a complete list.

;; You can get a reference card from 
;;   ftp://ftp.iesd.auc.dk/pub/emacs-lisp/math-ref.texi

;; This package requires GNU Emacs 19 or Lucid 19.10 or later.

;;; Code:

;;; Data:

(defvar LaTeX-math-list nil
  "AList of your personal LaTeX math symbols.  
The car of each entry is th key to be bound, the cdr is the value. 
The value can be a string or a function.")

(defconst LaTeX-math-default
  '(( ?a . "alpha" )
    ( ?b . "beta" )
    ( ?c . LaTeX-math-cal)
    ( ?d . "delta" )
    ( ?e . "epsilon" )
    ( ?f . "phi" )
    ( ?g . "gamma" )
    ( ?h . "eta" )
    ( ?k . "kappa" )
    ( ?l . "lambda" )
    ( ?m . "mu" )
    ( ?N . "nabla" )
    ( ?n . "nu" )
    ( ?o . "omega" )
    ( ?p . "pi" )
    ( ?q . "theta" )
    ( ?r . "rho" )
    ( ?s . "sigma" )
    ( ?t . "tau" )
    ( ?u . "upsilon" )
    ( ?x . "chi" )
    ( ?y . "psi" )
    ( ?z . "zeta" )
    ( ?D . "Delta" )
    ( ?G . "Gamma" )
    ( ?Q . "Theta" )
    ( ?L . "Lambda" )
    ( ?Y . "Psi" )
    ( ?P . "Pi" )
    ( ?S . "Sigma" )
    ( ?U . "Upsilon" )
    ( ?V . "Phi" )
    ( ?O . "Omega" )
    ( ?\C-f . "rightarrow" )
    ( ?\C-b . "leftarrow" )
    ( ?\C-p . "uparrow" )
    ( ?\C-n . "downarrow" )
    ( ?< . "leq" )
    ( ?> . "geq" )
    ( ?~ . "tilde" )
    ( ?I . "infty" )
    ( ?A . "forall" )
    ( ?E . "exists" )
    ( ?! . "neg" )
    ( ?i . "in" )
    ( ?* . "times" )
    ( ?. . "cdot" )
    ( ?{ . "subset" )
    ( ?} . "supset" )
    ( ?\[ . "subseteq" )
    ( ?\] . "supseteq" )
    ( ?/ . "not" )
    ( ?\\ . "setminus" )
    ( ?+ . "cup" )
    ( ?- . "cap" )
    ( ?& . "wedge" )
    ( ?| . "vee" )
    ( ?\( . "langle")
    ( ?\) . "rangle")
    ( ?\C-e . "exp")
    ( ?\C-s . "sin")
    ( ?\C-c . "cos")  
    ( ?\C-^ . "sup" )
    ( ?\C-_ . "inf" )
    ( ?\C-d . "det" )
    ( ?\C-l . "lim" )
    ( ?\C-t . "tan" )
    ( ?^ . "hat")
    ( ?v . "vee")
    ( ?0 . "emptyset" )))

;;; Keymap:

(defvar LaTeX-math-abbrev-prefix "`"
  "Prefix key for use in LaTeX-math-mode.")

(defvar LaTeX-math-keymap (make-sparse-keymap)
  "Keymap used for LaTeX-math-mode commands.")

(define-key LaTeX-math-keymap
  (concat LaTeX-math-abbrev-prefix LaTeX-math-abbrev-prefix)
  'LaTeX-math-insert-prefix)

(let ((math (append LaTeX-math-default LaTeX-math-list))
      (map (lookup-key LaTeX-math-keymap LaTeX-math-abbrev-prefix)))

  (while math
    (let* ((entry (car math))
	   (key (car entry))
	   (value (cdr entry)))
      (setq math (cdr math))

      (if (stringp value)
	  (let ((name (intern (concat "LaTeX-math-" value))))
	    (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
			     (list 'LaTeX-math-insert value 'arg)))
	    (setq value name)))
      (define-key map (if (numberp key)
			  (char-to-string key)
			(vector key)) value))))

;;; The Mode:

(defvar LaTeX-math-mode nil
  "Is LaTeX-math-mode on or off? non nil means on.")

 (make-variable-buffer-local 'LaTeX-math-mode)

(or (assoc 'LaTeX-math-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(LaTeX-math-mode " Math") minor-mode-alist)))

(or (assoc 'LaTeX-math-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'LaTeX-math-mode LaTeX-math-keymap)
		minor-mode-map-alist)))

;;;###autoload
(defun LaTeX-math-mode (&optional arg)
  "A minor mode with easy acces to TeX math macros. 

Easy insertion of LaTeX math symbols.  If you give a prefix argument,
the symbols will be surrounded by dollar signs.  The following
commands are defined:

\\{LaTeX-math-keymap}"
  (interactive "P")
  (setq LaTeX-math-mode
	(not (or (and (null arg) LaTeX-math-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (set-buffer-modified-p (buffer-modified-p)))

;;;###autoload
(fset 'latex-math-mode 'LaTeX-math-mode)

;;; Commands:

(defun LaTeX-math-insert-prefix ()
  "Insert the value of `LaTeX-math-abbrev-prefix'."
  (interactive "*")
  (let (LaTeX-math-mode)
    (call-interactively (key-binding LaTeX-math-abbrev-prefix))))

(defun LaTeX-math-insert (string dollar)
  ;; Inserts \STRING{}. If DOLLAR is non-nil, put $'s around it.
  (if dollar (insert "$"))
  ;; Allow `ltx-math.el' to be used without AUC TeX.
  (if (fboundp 'TeX-insert-macro)
      (TeX-insert-macro string)
    (insert "\\" string))

  (if dollar (insert "$")))

(defun LaTeX-math-cal (char dollar)
  "Inserts a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it."
  (interactive "*c\nP")
  (if dollar (insert "$"))
  (insert "{\\cal " (char-to-string char) "}")
  (if dollar (insert "$")))

(provide 'ltx-math)

;;; ltx-math.el ends here
