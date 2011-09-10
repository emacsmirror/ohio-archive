;;; fuse-atoms.el --- minor mode for editing atoms input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Atoms.el,v 1.2 1998/03/14 22:45:57 bruce Exp $

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
;;
;; If you have calc installed on your system, then you can use the
;; definition and evaluation line syntax new to version 0.5.4.  This
;; syntax allows you to specify atomic coordinates as math
;; expressions.  Thus, a position can be (1/3) rather than 0.33333, or
;; (0.5+delta) rather than (0.5123).  Please note that the evaluation
;; line syntax is implemented in Emacs lisp and not in the Fortran
;; source of Atoms.
;;
;; See the documentation for `Atoms-evaluate-buffer', a compiled
;; function in Atoms-math.el for more details about definition and
;; evaluation syntax.
;;
;; It may be desirable to run `Atoms-evaluate-buffer' whenever saving
;; the file.  This can be done by putting on eof the following in the
;; .fuse file:
;;    (add-hook 'local-write-file-hooks 'Atoms-evaluate-buffer)
;; or
;;    (add-hook 'input-before-run-hook
;;              (function (lambda ()
;;                          (and (string= input-program-name "atoms")
;;                               (Atoms-evaluate-buffer)))))
;;
;; calc can be found at ftp://prep.ai.mit.edu/pub/gnu/calc-2.02f.tar.gz

;;; Code:

(require 'input)
(require 'fuse)
(require 'tempo)

(defcustom Atoms-mode-hook nil
  "*Hook run when Atoms minor mode is entered."
  :group 'fuse-programs
  :type 'hook)

(defcustom Atoms-load-hook nil
  "*Hook run when fuse-atoms.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defconst Atoms-edges-expr
  "k\\|l[123]")

(defvar Atoms-keywords-alist 'nil
"This is an associated list of all keywords for atoms and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Atoms-keywords-alist '(
      ("a" .
       (1 "number" "a real number, the a-axis length"))
      ("alpha" .
       (1 "number" "a real number between 0 and 180 (angle btwn b & c)"))
      ("argon" .
       (1 "number" "a real number between 0 and 1"))
      ("atom" .
       (0 "none" "no argument -- the atoms list begins on the next line"))
      ("b" .
       (1 "number" "a real number, the b-axis length"))
      ("basis" .
       (0 "none" "no argument -- the basis list begins on the next line"))
      ("beta" .
       (1 "number" "a real number between 0 and 180 (angle btwn a & c)"))
      ("c" .
       (1 "number" "a real number, the c-axis length"))
      ("center" .  ;; ** should be a tag
       (1 "string" "an atom tag specifying the central atom"))
      ("central" .  ;; ** should be a tag
       (1 "string" "an atom tag specifying the central atom"))
      ("comment" .
       (0 "title" "a user-defined comment string"))
      ("core" .  ;; ** should be a tag
       (1 "string" "an atom tag specifying the central atom"))
      ("corrections" .
       (1 "logical" "a logical value (perform McMaster calculations)"))
      ("dafs" .
       (3 "integer" "three integers   (h k l of dafs reflection)"))
      ("dopant" . ;; ** 2nd shouldbe a tag
       (3 ("element" "string" "number")
	  "a dopant list   (element   tag   number)"))
      ;;("dwarf" .
      ;; (1 "logical" "  what?!?! \"dwarf\" is a keyword???"))
      ("edge" .
       (1 "edge" "either K, L1, L2, or L3"))
      ("eqrid" .
       (1 "number" "a real number, energy grid of dafs output"))
      ("fdat" .
       (1 "logical" "a logical value (write f.dat file)"))
      ("feff" .
       (1 "writable" "a valid filename (feff.inp file)"))
      ("feff8" .
       (1 "logical" "a logical value (feff.inp for feff8)"))
      ("feout" .
       (1 "writable" "a valid filename (anom. scat. diag. file)"))
      ("gamma" .
       (1 "number" "a real number between 0 and 180 (angle btwn a & b)"))
      ("geom" .
       (1 "logical" "a logical value (write geom.dat file)"))
      ("hkl" .
       (3 "integer" "three integers   (h k l of dafs reflection)"))
      ("hole" .
       (1 "edge" "either K, L1, L2, or L3"))
      ("i0" .
       (1 "logical" "a logical value (i0 diagnostic file)"))
      ("index" .
       (1 "logical" "a logical value (numerically index atoms)"))
      ("krypton" .
       (1 "number" "a real number between 0 and 1"))
      ("mcmaster" .
       (1 "logical" "a logical value (diagnostic file)"))
      ("message" .
       (1 "integer" "an integer (run-time messages)"))
      ("modules" .
       (1 "logical" "a logical value (run-time messages)"))
      ("nepoints" .
       (1 "integer" "a real number (number of energy points in dafs output)"))
      ("nitrogen" .
       (1 "number" "a real number between 0 and 1"))
      ("noanomalous" .
       (1 "string" "an atom tag"))
      ("outfile" .
       (1 "writable" "a valid filename (feff.inp file)"))
      ("p1" .
       (1 "logical" "a logical value (write p1.inp file)"))
      ("qvec" .
       (3 "integer" "three integers   h k l of dafs reflection"))
      ("refile" .
       (1 "writable" "a valid filename (reflection file)"))
      ("reflections" .
       (3 "integer" "three integers (maximum h k l)"))
      ("rmax" .
       (1 "number" "a real number"))
      ("self" .
       (1 "logical" "a logical value"))
      ("shift" .
       (3 "number" "three real numbers"))
      ("space" .
       (1 "spcgrp" "a space group symbol"))
      ("title" .
       (0 "title" "a user-defined comment string"))
      ("unit" .
       (1 "logical" "a logical value (write unit.dat file)"))
      ("xanes" .
       (1 "logical" "a logical value (feff.inp for xanes run)"))
      ))


(defvar Atoms-output-files)
(setq Atoms-output-files (list "feff.inp" "geom.dat"))

(defvar Atoms-highlight-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template for atom.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Atoms-template
  '("title = " p n
    (Atoms-get-crystal-class)
    "core = " p "\trmax = " p "\tspace = " p n
    "atoms" n
    "! sym\tx\ty\tz\ttag" n
    p n)
  "Template for an *atoms* stanza.  Inserted by \\[Atoms-make-template].")

(tempo-define-template "Atoms-template" Atoms-template nil
		       "Insert an empty Atoms template.")
(eval-when-compile  (defun tempo-template-Atoms-template))

(defun Atoms-make-template ()
  "Write a template for *atoms* using a tempo template.
The crystal class will be prompted and entries for the appropriate lattice
parameters will be inserted in the template.  Bound to \\[Atoms-make-template]."
  (interactive)
  (tempo-template-Atoms-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit)
	  ;;(nth 1 (cdr (assoc input-program-name
		;;		     input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))



(defun Atoms-get-crystal-class ()
  "Return a list appropriate for an Atoms tempo template.
This is appropriate for the lattice parameters appropriate to a
crystal class.  The class is prompted for in the minibuffer with
completion."
  (interactive)
  (let (class lattice-card
	(classes '(("cubic"        . "c" )
		   ("hexagonal"    . "h")
		   ("trigonal"     . "3")
		   ("tetragonal"   . "4")
		   ("orthorhombic" . "o")
		   ("monoclinic"   . "m")
		   ("triclinic"    . "t"))))
    (setq class
	  (completing-read "Which crystal class? (<tab> for choices) "
			   classes nil t))
    (setq class (cdr (assoc class classes)))
    (cond ((string= class "c")
	   (setq lattice-card '(l "a = " p n)))
	  ((string= class "h")
	   (setq lattice-card '(l "a = " p "\tc = " p "\tgamma = 120" n)))
	  ((string= class "3")
	   (setq lattice-card '(l "a = " p "\talpha = " p n)))
	  ((string= class "4")
	   (setq lattice-card '(l "a = " p "\tc = " p n)))
	  ((string= class "o")
	   (setq lattice-card '(l "a = " p "\tb = " p "\tc = " p n)))
	  ((string= class "m")
	   (setq lattice-card '(l "a = " p "\tb = " p "\tc = " p n
				  "beta = " p n)))
	  (t
	   (setq lattice-card '(l "a = "     p "\tb = "    p "\tc = "     p n
				  "alpha = " p "\tbeta = " p "\tgamma = " p n))))
    lattice-card  ))

;;; end template for atom.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; special functions for atoms


(defun Atoms-clean-list-entry ()
  "This function cleans an atom or basis list.
Insert proper indentation and column separation.  Care is taken not
to go over the end of the line."
  (let (begin label x y z tag indent space )
    (if (or (input-blank-p) (input-comment-p)) ()
      (setq indent (input-make-indent-string input-list-indent nil))
      (setq space  (input-make-indent-string input-list-separate t))

      (beginning-of-line)
      (delete-horizontal-space)
      (setq begin (point))
      (forward-word 1)
      (setq label (buffer-substring-no-properties begin (point)))
      (delete-horizontal-space)

      (setq begin (point))
      (forward-word 1)
      (setq x (string-to-number (buffer-substring-no-properties begin (point))))
      (delete-horizontal-space)
      (setq begin (point))
      (forward-word 1)
      (setq y (string-to-number (buffer-substring-no-properties begin (point))))
      (delete-horizontal-space)
      (setq begin (point))
      (forward-word 1)
      (setq z (string-to-number (buffer-substring-no-properties begin (point))))
      (delete-horizontal-space)

      (setq begin (point))
      (end-of-line)
      (setq tag (buffer-substring-no-properties begin (point)))

      (setq begin (point))
      (beginning-of-line)
      (delete-region begin (point))

      (insert (format "%s%-2s%s%9.5f%s%9.5f%s%9.5f%s%-s"
		      indent label space x space y space z space tag))
      )))

(defun Atoms-jump-to-p1 ()
  "Display the p1.inp file in another buffer.
Bound to \\[Atoms-jump-to-p1]"
  (interactive)
  (let ((file "p1.inp"))
    (if (file-readable-p file) (input-jump-to-file file t)
      (message "%S does not exist.  Have you run atoms with p1=true?"
	       (file-relative-name file "./")))))

(defun Atoms-jump-to-unit ()
  "Display the unit.dat file in another buffer.
Bound to \\[Atoms-jump-to-unit]"
  (interactive)
  (let ((file "unit.dat"))
    (if (file-readable-p file) (input-jump-to-file file t)
      (message "%S does not exist.  Have you run atoms with unit=true?"
	       (file-relative-name file "./")))))

(defun Atoms-jump-to-geom ()
  "Display the geom.dat file in another buffer.
Bound to \\[Atoms-jump-to-unit]"
  (interactive)
  (let ((file "geom.dat") base)
    (setq base (file-name-sans-extension (buffer-file-name)))
    (cond ((file-exists-p file)
	   (if (file-readable-p file) (input-jump-to-file file)))
	  ((file-exists-p (concat base "-" file))
	   (setq file (concat base "-" file))
	   (if (file-readable-p file) (input-jump-to-file file)))
	  (t
	   (message (concat "The geom.dat file does not exist.  "
			    "Have you run atoms with geom=true?"))))))



;;;; end special functions for atoms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions
;;
;;    (setq my-expr (make-regexp '("a" "b" "c" "alpha" "argon" "beta"
;; 				 "center" "central" "comment" "core"
;; 				 "dafs" "dopant" "dwarf" "edge"
;; 				 "eqrid" "feff" "fdat" "feout"
;; 				 "gamma" "geom" "hkl" "hole" "i0" "index"
;; 				 "krypton" "mcmaster" "message"
;; 				 "modules" "nepoints" "nitrogen"
;; 				 "noanomalous" "outfile" "p1" "qvec"
;; 				 "refile" "reflections" "rmax"
;; 				 "self" "shift" "space" "unit" "xanes"
;;                               "corrections" "feff8" )))



(defconst Atoms-font-lock-keywords
      '(
					; comments
	("^[ \t]*![-+].*$" . font-lock-reference-face)
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#].*$" . font-lock-comment-face)
					; titles
	;;("\\<\\(title\\|comment\\).*$" . atoms-new-face)
	("\\<\\(title\\|comment\\).*$" . font-lock-string-face)
					; atom, basis, end
	("^[ \t]*\\(atoms?\\|basis\\|end\\)\\>" . font-lock-function-name-face)
					; ATOMS keywords
	("\\<\\([abc]\\|a\\(lpha\\|rgon\\)\\|beta\\|c\\(ent\\(er\\|ral\\)\\|o\\(mment\\|r\\(e\\|rections\\)\\)\\)\\|d\\(afs\\|opant\\|warf\\)\\|e\\(dge\\|qrid\\)\\|f\\(dat\\|e\\(ff8?\\|out\\)\\)\\|g\\(amma\\|eom\\)\\|h\\(kl\\|ole\\)\\|i\\(0\\|ndex\\)\\|krypton\\|m\\(cmaster\\|essage\\|odules\\)\\|n\\(epoints\\|itrogen\\|oanomalous\\)\\|outfile\\|p1\\|qvec\\|r\\(ef\\(ile\\|lections\\)\\|max\\)\\|s\\(elf\\|hift\\|pace\\)\\|unit\\|xanes\\)\\>" . font-lock-keyword-face) ))


(defconst Atoms-font-lock-keywords-1 nil)
(setq Atoms-font-lock-keywords-1 Atoms-font-lock-keywords)
(defconst Atoms-font-lock-keywords-2 nil)
(setq Atoms-font-lock-keywords-2 Atoms-font-lock-keywords)

(defun Atoms-set-hilit ()
  "Call hilit-set-mode-patterns for *atoms*."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("^[ \t]*![-+].*$" nil glob-struct)
	    ("[%!#].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
					; titles -- type define
	    ("\\<\\(title\\|comment\\).*$" nil define)
					; atom, basis, end
	    ("^[ \t]*\\(atoms?\\|basis\\|end\\)\\>" nil include)
					; ATOMS keywords
	    ("\\<\\([abc]\\|a\\(lpha\\|rgon\\)\\|beta\\|c\\(ent\\(er\\|ral\\)\\|o\\(mment\\|r\\(e\\|rections\\)\\)\\)\\|d\\(afs\\|opant\\|warf\\)\\|e\\(dge\\|qrid\\)\\|f\\(dat\\|e\\(ff8?\\|out\\)\\)\\|g\\(amma\\|eom\\)\\|h\\(kl\\|ole\\)\\|i\\(0\\|ndex\\)\\|krypton\\|m\\(cmaster\\|essage\\|odules\\)\\|n\\(epoints\\|itrogen\\|oanomalous\\)\\|outfile\\|p1\\|qvec\\|r\\(ef\\(ile\\|lections\\)\\|max\\)\\|s\\(elf\\|hift\\|pace\\)\\|unit\\|xanes\\)\\>" nil keyword)

	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Atoms-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Atoms-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Atoms-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for atoms
(defvar Atoms-mode-map ()
  "Keymap used in Atoms minor mode.")
(if Atoms-mode-map
    ()
  (setq Atoms-mode-map (make-sparse-keymap))
  (define-key Atoms-mode-map "\C-c\C-tt"  'Atoms-make-template)
					; looking at files
  (define-key Atoms-mode-map "\C-c\C-fp"  'Atoms-jump-to-p1)
  (define-key Atoms-mode-map "\C-c\C-fu"  'Atoms-jump-to-unit)
  (define-key Atoms-mode-map "\C-c\C-fg"  'Atoms-jump-to-geom)
  )
(defvar Atoms-mode-menu nil)
(easy-menu-define
 Atoms-mode-menu Atoms-mode-map
 "Menu used in Atoms mode"
 '("Atoms"
   ["--------- Templates -------------" input-t t]
   ["Make template"                     Atoms-make-template t]
   ["Open feff.inp"                     input-jump-to-log-file
    :active (input-log-file-exists-p)]
   ["--------- Running ---------------" input-t t]
   ["Run atoms, this file"              input-run-this-program-this-file t]
   ["Kill atoms run"                    input-kill-program
    :active (get-process input-process)]
   ["--------- Et cetera -------------" input-t t]
   ["Clean input file"                  input-clean-file t]
   ("Look at output files"
    ["Look at p1.inp"                   Atoms-jump-to-p1
     :active (file-exists-p "p1.inp")]
    ["Look at unit.dat"                 Atoms-jump-to-unit
     :active (file-exists-p "unit.dat")]
    ["Look at geom.dat"                 Atoms-jump-to-geom
     :active (file-exists-p "geom.dat")])
   ["Display atoms keywords"            input-display-keywords t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Atoms-imenu-generic-expression nil
  "Imenu generic expression for Atoms mode.  See `imenu-generic-expression'.")

(defvar Atoms-mode nil
  "Determines if Atoms minor mode is active.")
(make-variable-buffer-local 'Atoms-mode)

;;;###autoload
(defun turn-on-atoms ()
  "Turn on Atoms minor mode."
  (Atoms-mode t))

;;;###autoload
(defun Atoms-mode (&optional arg)
  "Minor mode for editing input files for *atoms*.
ARG t turns on Atoms minor mode.

Defined keys in Atoms minor mode:\n \\{Atoms-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Atoms-mode
	       (if (null arg) (not Atoms-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Atoms-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Atoms-mode " Atoms") minor-mode-alist)))
	 (or (assq 'Atoms-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Atoms-mode Atoms-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Atoms-mode
	     (progn
	       (make-variable-buffer-local 'Atoms-auto-jump-flag)
	       (make-local-variable 'imenu-generic-expression)
	       (setq imenu-generic-expression Atoms-imenu-generic-expression
		     input-output-files Atoms-output-files
		     input-current-keywords-alist
		     (copy-alist Atoms-keywords-alist)
		     fuse-mouse-highlight-list Atoms-highlight-list)

	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     t
		     input-program-stanza-flag      nil
		     input-program-data-flag        nil
		     input-program-feff-flag        t
		     input-program-list-flag        nil
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		           '("Bruce Ravel" "ravel@phys.washington.edu")
		     input-program-parse            '(nil nil)
		     input-program-hilit            'Atoms-set-hilit)

	       (easy-menu-add Atoms-mode-menu)
	       (cond (input-use-hilit19 (Atoms-set-hilit)))
	       (eval-and-compile        ; load Atoms-math
		 (condition-case ()
		     (require 'calc)
		   (error nil))
		 (if (featurep 'calc)
		     (require 'fuse-math)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Atoms-mode-hook))
	   (easy-menu-remove Atoms-mode-menu)
	   ;;(if (specifierp (symbol-value Atoms-toolbar))
	   ;;    (remove-specifier (symbol-value Atoms-toolbar)))
	   )
	 )
	(t
	 (message "Atoms minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-atoms)
(run-hooks 'Atoms-load-hook)

;;;============================================================================
;;;
;;; fuse-atoms.el end here
