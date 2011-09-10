;;; fuse-diffkk.el --- minor mode for editing diffkk input files with FUSE

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  22 May 1998
;; Version:  0.5.10
;; Keywords:  diffkk, Kramers-Kronig, dafs, feff, f', f"

;; $Id: $

;; Copyright (C) 1998 Bruce Ravel

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change-log:

;;; Code:

(require 'input)
(require 'fuse-gnuplot)
(require 'tempo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set mode and load hooks
(defcustom Diffkk-mode-hook nil
  "*Hook run when Diffkk minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Diffkk-load-hook nil
  "*Hook run when fuse-diffkk.el is first loaded."
  :group 'fuse-programs
  :type 'hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set keyword alist
(defvar Diffkk-keywords-alist 'nil
"This is the diffkk associated list of keywords and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Diffkk-keywords-alist '(
      ("title" .
       (1 "title" "a title/comment line"))
      ("out" .
       (1 "writable" "an output data file name"))
      ("xmu" .
       (1 "readable" "an input data file name"))
      ("isfeff" .
       (1 "logical" "a logical flag for using xmu.dat from feff"))
      ("encol" .
       (1 "integer" "an integer denoting the column for the energy array"))
      ("mucol" .
       (1 "integer" "an integer denoting the column for the xmu array"))
      ("iz" .
       (1 "integer" "the atomic number of the central atom"))
      ("e0" .
       (1 "number" "the edge energy"))
      ("egrid" .
       (1 "number" "the energy grid for output"))
      ("ewidth" .
       (1 "number" "the energy width for broadening"))
      ("epad" .
       (1 "number" "the energy grid for padding xmu.dat below E0"))
      ("npad" .
       (1 "integer" "number of points for padding xmu.dat below E0"))
      ("elow" .
       (1 "number" "an amount below the data range to extend the calculation"))
      ("ehigh" .
       (1 "number" "an amount above the data range to extend the calculation"))
      ("iprint" .
       (1 "integer" " ?? "))
      ("end" .
       (0 "none"
   "no argument.  \"diffkk\" stops reading its input file when \"end\" is found"))
      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set output files for run wrapper
(defvar Diffkk-output-files)
(setq Diffkk-output-files (list ".log"))

(defvar Diffkk-highlight-list
  (list
   '("^[ \t]*\\(\\(xmu\\)[ \t]*[ \t=,][ \t]*\\sw+\\)\\>" 0)
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diffkk template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Diffkk-template
  '("title =\t\t" p n
    "out =\t\t" p n
    "xmu =\t\t" p n
    "isfeff =\tfalse" p n
    "# encol =\t\t" p n
    "# mucol =\t\t" p n
    "iz =\t\t" p n
    "e0 =\t\t" p n
    "egrid =\t\t" p n
    "ewidth =\t" p n
    "elow =\t\t" p n
    "ehigh =\t\t" p n
    )
  "Template for an *diffkk* header.  Inserted by \\[Diffkk-make-template].")

(tempo-define-template "Diffkk-template" Diffkk-template
		       nil
		       "Insert an empty Diffkk template.")

(defun Diffkk-make-template ()
  "Write a *diffkk* template using a tempo template.
Bound to \\[Diffkk-make-template]."
  (interactive)
  (tempo-template-Diffkk-template)
  (setq input-used-tempo-flag t)
  ;;(Diffkk-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				        ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))
;;; end template for diffkk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;----------------------------------------------------------------------
;; plotting code for diffkk

(defun Diffkk-parse-out ()
  "Fetch the output file name from this input file."
  (interactive)
  (let (word)
    (save-excursion
      (goto-char (point-min))
      (while (not word)
	(cond ((re-search-forward "\\<out\\>" (point-max) t)
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (forward-word 1)
		 (setq word (input-this-word)) )))))
		 word))

(defun Diffkk-parse-title ()
  "Fetch the first title line from this input file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward "^[ \t]*title[ \t]*[ \t,=][ \t]*" (point-max) t)
	   (buffer-substring-no-properties
	    (point) (save-excursion (end-of-line) (point)) )))))

(defun Diffkk-plot-fp ()
  "Plot real part of diffkk output.
Bound to \\[Diffkk-plot-fp]"
  (interactive) (Diffkk-plot 'real))

(defun Diffkk-plot-fpp ()
  "Plot imaginary part of diffkk output.
Bound to \\[Diffkk-plot-fpp]"
  (interactive) (Diffkk-plot 'imaginary))

(defun Diffkk-plot (arg)
  "Plot xmu from correct with data.  ARG is 'real or 'imaginary."
  (interactive)
  (let (lines (file (Diffkk-parse-out)) (input-buffer (current-buffer))
	      (label (if (equal arg 'imaginary)
			 "imaginary part of f" "real part of f"))
	      (dkk (if (equal arg 'imaginary) 3 2))
	      (cl  (if (equal arg 'imaginary) 5 4)))
    (setq lines (concat (input-gnuplot-cd-and-title
			 (file-name-directory buffer-file-name)
			 (Diffkk-parse-title))
			(input-gnuplot-axis-labels "Energy (eV)" label)
			(input-gnuplot-set-terminal input-gnuplot-terminal)
			(format "plot '%s' using 1:%s title 'data',\\\n" file dkk)
			(format "     '%s' using 1:%s title 'C-L'\n" file cl ))
	  input-current-terminal input-gnuplot-terminal)
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag
					; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))


;;----------------------------------------------------------------------
;; setup toolbar for XEmacs

(when (and fuse-xemacs-p (featurep 'toolbar))
  (require 'fuse-toolbar)
  (defvar Diffkk-toolbar
    '([fuse-toolbar-template-xpm Diffkk-toolbar-template
				 t "Write a Diffkk template"]
      [fuse-toolbar-run-xpm    Diffkk-toolbar-run
			       t "Run Diffkk"]
      [fuse-toolbar-log-xpm    Diffkk-toolbar-log
			       t "Look at diffkk.log"]
      [Diffkk-toolbar-fp-xpm
       Diffkk-toolbar-fp-xpm
       t
       "Plot f', the real part of the data and Cromer-Liebermann"]
      [Diffkk-toolbar-fpp-xpm
       Diffkk-toolbar-fpp-xpm
       t
       "Plot f\", the imaginary part of the data and Cromer-Liebermann"]
      [fuse-toolbar-helper-xpm Diffkk-toolbar-helper
			       t "Display Diffkk keywords"]
      ;;[Diffkk-toolbar-document Diffkk-toolbar-document
      ;;		   t "Read the Diffkk document"]
      [fuse-toolbar-quit-xpm   Diffkk-toolbar-quit
			       t "Quit editing this input file"]
      )
    "The Diffkk toolbar.")

  (add-hook 'Diffkk-mode-hook
	    '(lambda ()
	       (and (featurep 'toolbar)
		    (fuse-make-toolbar-function Diffkk-toolbar))))

  (fset 'Diffkk-toolbar-template 'Diffkk-make-template)
  (fset 'Diffkk-toolbar-run      'input-run-this-program-this-file)
  (fset 'Diffkk-toolbar-log      'input-jump-to-log-file)
  (fset 'Diffkk-toolbar-fp       'Diffkk-plot-fp)
  (fset 'Diffkk-toolbar-fpp      'Diffkk-plot-fpp)
  (fset 'Diffkk-toolbar-helper   'input-display-keywords)
  (fset 'Diffkk-toolbar-quit     'fuse-quit)  )
;;----------------------------------------------------------------------

(defvar Diffkk-toolbar-fp-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * Diffkk_toolbar_fp_up_xpm[] = {
\"40 40 13 1\",
\" 	c #E79DE79DFFFF\",
\".	c #28A24D344924\",
\"X	c #BEFBBEFBBEFB s backgroundToolBarColor\",
\"o	c #000000000000\",
\"O	c #965896589658\",
\"+	c #104014511040\",
\"@	c #38E33CF338E3\",
\"#	c #514455555144\",
\"$	c #28A228A228A2\",
\"%	c #69A669A669A6\",
\"&	c #79E77DF779E7\",
\"*	c #00000000FFFF\",
\"=	c #FFFF61854103\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXO+@Xo@XXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXX#$XO%&XXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXooXXXXX&oo&XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXX+%XXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXo&XXXXXXXXXXXXXX*XXXXXXXX\",
\"XXXXXXXXoXXXXX&@XXXXXXXXXXXXXX*XXXXXXXXX\",
\"XXXXXXXooXXXXX#%XXXXXXXXXXXXX*XXXXXXXXXX\",
\"XXXXXXXXoXXXXX%OXXXXXXXXXXXX*XXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXX=XXXX*XX*XXXXXXXXXXXX\",
\"XXXXXXXXo=XXX*XXX==X==X*X**XXXXX==XXXXXX\",
\"XXXXXXXooX==*X**=XXXXX==XXXXXX==XXXXXXXX\",
\"XXXXXXXXoXX*=X==*XXXX*XX==XX==XXXXXXXXXX\",
\"XXXXXXXXoX*XX=XXX**X*XXXXX==XXXXXXXXXXXX\",
\"XXXXXXXXo*XXXXXXXXX*XXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXooXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXooooooooooooooooooooooooooooXXXXXX\",
\"XXXXXXXXoXXXXoXXXXoXXXXoXXXXoXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};"))
  "XPM format image used for the \"____\" button")

(defvar Diffkk-toolbar-fpp-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * Diffkk_toolbar_fpp_up_xpm[] = {
\"40 40 14 1\",
\" 	c #E79DE79DFFFF\",
\".	c #28A24D344924\",
\"X	c #BEFBBEFBBEFB s backgroundToolBarColor\",
\"o	c #965896589658\",
\"O	c #104014511040\",
\"+	c #38E33CF338E3\",
\"@	c #000000000000\",
\"#	c #79E77DF779E7\",
\"$	c #514455555144\",
\"%	c #28A228A228A2\",
\"&	c #69A669A669A6\",
\"*	c #AEBAAAAAAEBA\",
\"=	c #00000000FFFF\",
\"-	c #FFFF61854103\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXoO+X@##XXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXX$%X#&$*XXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXX#@@#XXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXX@@XXXXXXXO&XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXX@#XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXX#+XXXXXXXXXXXXXX=XXXXXXXX\",
\"XXXXXXXX@XXXXXX$&XXXXXXXXXXXXX=XXXXXXXXX\",
\"XXXXXXX@@XXXXXX&oXXXXXXXXXXXX=XXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXX=XXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXX-XXXX=XX=XXXXXXXXXXXX\",
\"XXXXXXXX@-XXX=XXX--X--X=X==XXXXX--XXXXXX\",
\"XXXXXXX@@X--=X==-XXXXX--XXXXXX--XXXXXXXX\",
\"XXXXXXXX@XX=-X--=XXXX=XX--XX--XXXXXXXXXX\",
\"XXXXXXXX@X=XX-XXX==X=XXXXX--XXXXXXXXXXXX\",
\"XXXXXXXX@=XXXXXXXXX=XXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXX@@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXX@@@@@@@@@@@@@@@@@@@@@@@@@@@@XXXXXX\",
\"XXXXXXXX@XXXX@XXXX@XXXX@XXXX@XXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};"))
  "XPM format image used for the \"____\" button")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colorization regular expressions

;;    (setq my-expr (make-regexp '("foo" "bar" "foobar" "blahblahblah")))

(defconst Diffkk-font-lock-keywords
      '(
					; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#;].*$" . font-lock-comment-face)
					; titles
	("\\<title.*$" . font-lock-string-face)
	("^[ \t]*\\(end\\|out\\|xmu\\)\\>" . font-lock-function-name-face)

	("\\<\\(e\\(0\\|grid\\|high\\|low\\|ncol\\|pad\\|width\\)\\|i\\(print\\|sfeff\\|z\\)\\|mucol\\|npad\\)\\>" . font-lock-keyword-face) ))


(defconst Diffkk-font-lock-keywords-1 nil)
(setq Diffkk-font-lock-keywords-1   Diffkk-font-lock-keywords)
(defconst Diffkk-font-lock-keywords-2 nil)
(setq Diffkk-font-lock-keywords-2   Diffkk-font-lock-keywords)

(defun Diffkk-set-hilit ()
  "Function used to call hilit-set-mode-patterns."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#;].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
	    ("\\<title.*$" nil define)
	    ("^[ \t]*\\(end\\|out\\|xmu\\)\\>" nil include)
	    ("\\<\\(e\\(0\\|grid\\|high\\|low\\|ncol\\|pad\\|width\\)\\|i\\(print\\|sfeff\\|z\\)\\|mucol\\|npad\\)\\>" nil keyword)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Diffkk-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Diffkk-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Diffkk-font-lock-keywords-2)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for diffkk
(defvar Diffkk-mode-map ()
  "Keymap used in Diffkk minor mode.")
(if Diffkk-mode-map
    ()
  (setq Diffkk-mode-map (make-sparse-keymap))
  (define-key Diffkk-mode-map "\C-c\C-tt"  'Diffkk-make-template)
  (define-key Diffkk-mode-map "\C-c\C-pr"  'Diffkk-plot-fp)
  (define-key Diffkk-mode-map "\C-c\C-pi"  'Diffkk-plot-fpp)
  (define-key Diffkk-mode-map "\C-c\C-sn"  'fuse-insert-z-number)
  )
(defvar Diffkk-mode-menu nil)
(easy-menu-define
 Diffkk-mode-menu Diffkk-mode-map
 "Menu used in Diffkk mode"
 '("Diffkk"
   ["Make template"                     Diffkk-make-template t]
   ["Insert Z number"                   fuse-insert-z-number t]
   ["Display diffkk keywords"           input-display-keywords t]
   ["--------- Running ---------------" input-t t]
   ["Run diffkk, this file"             input-run-this-program-this-file t]
   ["Kill diffkk run"                   input-kill-program
    :active (get-process input-process)]
   ["--------- Ploting ---------------" input-t t]
   ["Plot real part"                    Diffkk-plot-fp t]
   ["Plot imaginary part"               Diffkk-plot-fpp t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar Diffkk-imenu-generic-expression nil
  "Imenu generic expression for Diffkk mode.  See `imenu-generic-expression'.")

(defvar Diffkk-mode nil
  "Determines if Diffkk minor mode is active.")
(make-variable-buffer-local 'Diffkk-mode)

;;;###autoload
(defun Diffkk-mode (&optional arg)
  "Minor mode for editing input files for Diffkk.
ARG t turns on diffkk mode.

The Diffkk homepage is at http://cars.chicago.edu/~newville/diffkk/

Defined keys in Diffkk minor mode:\n \\{Diffkk-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Diffkk-mode
	       (if (null arg) (not Diffkk-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Diffkk-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Diffkk-mode " Diffkk") minor-mode-alist)))
	 (or (assq 'Diffkk-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Diffkk-mode Diffkk-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Diffkk-mode
	     (progn
	       (easy-menu-add Diffkk-mode-menu)
	       (make-local-variable 'imenu-generic-expression)
	       (setq imenu-generic-expression Diffkk-imenu-generic-expression
		     input-current-keywords-alist (copy-alist Diffkk-keywords-alist)
		     input-output-files Diffkk-output-files
		     fuse-mouse-highlight-list Diffkk-highlight-list)

	       ;; set all of the variables for input file properties
	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     t
		     input-program-stanza-flag      nil
		     input-program-data-flag        t
		     input-program-feff-flag        t
		     input-program-list-flag        nil
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Matt Newville" "newville@cars.uchicago.edu")
		     input-program-parse            '(nil nil)
		     input-program-hilit            'Diffkk-set-hilit)

	       (run-hooks 'Diffkk-mode-hook)
	       (cond (input-use-hilit19 (Diffkk-set-hilit))))
	   (easy-menu-remove Diffkk-mode-menu))
	 )
	(t
	 (message "Diffkk minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-diffkk)
(run-hooks 'Diffkk-load-hook)

;;;============================================================================
;;;
;;; fuse-diffkk.el end here
