;;; fuse-generic.el --- minor mode for editing generic FEFF/UWXAFS input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Generic.el,v 1.2 1998/03/14 22:45:58 bruce Exp $

;; Copyright (C) 1997, 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Everyone is granted permission to copy, modify and redistribute this
;; and related files provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;;; Commentary:

;;; Here is a minimal list of things that needs to be done for a
;;; program.  (1) make a keyword alist.  (2) set current alist to that
;;; alist.  (4) set output files list (5) Make a template function and
;;; any other functions that are required (optional) (6) Make a
;;; function for setting hilit regexps.  (7) Set keybindings and menu
;;; items.  (7) Make the minor mode function. (9) call the minor mode
;;; hooks and provide the program.  Most of this can be done by
;;; modifying this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change-log:

;;; Code:

(require 'input)
;;(require 'fuse-gnuplot)
(require 'tempo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set mode and load hooks
(defcustom Generic-mode-hook nil
  "*Hook run when Generic minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Generic-load-hook nil
  "*Hook run when fuse-generic.el is first loaded."
  :group 'fuse-programs
  :type 'hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set keyword alist
(defvar Generic-keywords-alist 'nil
"This is the generic associated list of keywords and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Generic-keywords-alist '(
      ("foo" .
       (1 "number" "a real number"))
      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set output files for run wrapper
(defvar Generic-output-files)
(setq Generic-output-files (list ".log"))

(defvar Generic-highlight-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Generic-template
  '("# foo" p n)
  "Template for an *generic* header.  Inserted by \\[Generic-make-template].")

(tempo-define-template "Generic-template" Generic-template
		       nil
		       "Insert an empty Generic template.")
(eval-when-compile (defun tempo-template-Generic-template))

(defun Generic-make-template ()
  "Write a *generic* template using a tempo template.
Bound to \\[Generic-make-template]."
  (interactive)
  (tempo-template-Generic-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  ;;(Generic-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				        ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))
;;; end template for generic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colorization regular expressions

;;    (setq my-expr (make-regexp '("foo" "bar" "foobar" "blahblahblah")))

(defconst Generic-font-lock-keywords
      '(
					; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#;].*$" . font-lock-comment-face) ))

(defconst Generic-font-lock-keywords-1 nil)
(setq Generic-font-lock-keywords-1   Generic-font-lock-keywords)
(defconst Generic-font-lock-keywords-2 nil)
(setq Generic-font-lock-keywords-2   Generic-font-lock-keywords)

(defun Generic-set-hilit ()
  "Function used to call hilit-set-mode-patterns."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#;].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Generic-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Generic-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Generic-font-lock-keywords-2)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for generic
(defvar Generic-mode-map ()
  "Keymap used in Generic minor mode.")
(if Generic-mode-map
    ()
  (setq Generic-mode-map (make-sparse-keymap))
  (define-key Generic-mode-map "\C-c\C-tt"  'Generic-make-template)
  )
;; (defvar Generic-mode-menu nil)
;; (easy-menu-define
;;  Generic-mode-menu Generic-mode-map
;;  "Menu used in Generic mode"
;;  '("Generic"
;;    ["Make template" Generic-make-template t]
;;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; when you are changing the word generic to your new mode, be aware
;; that this word is in the imenu variable name
;; `imenu-generic-expression'.  Don't change this here or in the mode
;; function

(defvar Generic-imenu-generic-expression nil
  "Imenu generic expression for Generic mode.  See `imenu-generic-expression'.")

(defvar Generic-mode nil
  "Determines if Generic minor mode is active.")
(make-variable-buffer-local 'Generic-mode)


;;;###autoload
(defun Generic-mode (&optional arg)
  "Minor mode for editing input files for Generic.
ARG t turns on generic mode.

Defined keys in Generic minor mode:\n \\{Generic-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Generic-mode
	       (if (null arg) (not Generic-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Generic-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Generic-mode " Generic") minor-mode-alist)))
	 (or (assq 'Generic-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Generic-mode Generic-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Generic-mode
	     (progn
	       ;; watch out for the word generic in `imenu-generic-expression'!!
	       (make-local-variable 'imenu-generic-expression)
	       ;;(easy-menu-add Generic-mode-menu)
	       (setq imenu-generic-expression Generic-imenu-generic-expression
		     input-current-keywords-alist
		     (copy-alist Generic-keywords-alist)
		     input-output-files Generic-output-files
		     fuse-mouse-highlight-list Generic-highlight-list)

	       ;; set all of the variables for input file properties
	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     nil
		     input-program-stanza-flag      nil
		     input-program-data-flag        nil
		     input-program-feff-flag        nil
		     input-program-list-flag        nil
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Bruce Ravel" "ravel@phys.washington.edu")
		     input-program-parse            '(nil nil)
		     input-program-hilit            'Generic-set-hilit)

	       (run-hooks 'Generic-mode-hook)
	       (cond (input-use-hilit19 (Generic-set-hilit))))
	   ;;(easy-menu-remove Generic-mode-menu)
	   )
	 )
	(t
	 (message "Generic minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-generic)
(run-hooks 'Generic-load-hook)

;;;============================================================================
;;;
;;; fuse-generic.el end here
