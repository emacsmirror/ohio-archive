;;; fuse-phit.el --- minor mode for editing phit input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Phit.el,v 1.2 1998/03/14 22:45:58 bruce Exp $

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
;;;  much of feffit mode can be recycled here, possibly with a bit of
;;;  modification.  for example, path paragraph manipulation,
;;;  cleaning, and movement functions should translate well

;;; Change-log:

;;; Code:

(require 'input)
(require 'fuse-feffit)  ;; many feffit functions can be recycled for phit
(require 'fuse-gnuplot)
(require 'tempo)

(defcustom Phit-mode-hook nil
  "*Hook run when Phit minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Phit-load-hook nil
  "*Hook run when fuse-phit.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defvar Phit-features-alist nil
  "Alist containing information parsed from an phit data set.")

(defvar Phit-keywords-alist 'nil
"This is the phit associated list of keywords and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Phit-keywords-alist '(

      ("title" .
       (0 "title" "a user-defined comment string"))
      ("comment" .
       (0 "title" "a user-defined comment string"))
      ("data" .
       (1 "readable" "an input data filename"))
      ("infile" .
       (1 "readable" "an input data filename"))
      ("read" .
       (1 "readable" "an input data filename"))
      ("skey" .
       (1 "skey" "a symbolic key for a UWXAFS binary record"))
      ("nkey" .
       (1 "integer" "a numeric key for a UWXAFS binary record"))
      ("out" .
       (1 "writable" "output filename base"))
      ("log" .
       (1 "writable" "logfile file name"))
      ("logfile" .
       (1 "writable" "logfile file name"))
      ("include" .
       (1 "readable" "a filename for an include file"))
      ("echo" . ;;** symbol
       (1 "string" "a keyword to echo during runtime"))
      ("format" .
       (1 "datatype" "i/o file format (uwxafs/ascii)"))
      ("formin" .
       (1 "datatype" "input file format (uwxafs/ascii)"))
      ("formout" .
       (1 "datatype" "output file format (uwxafs/ascii)"))
      ("sigdat" .
       (1 "number" "a constant measurement uncrtainty"))
      ("sigma" .
       (0 "none" "nothing. \"sigma\" is switch for data uncertainty."))
      ("cormin" .
       (1 "number" "a real number between 0 and 1"))
      ("xaxis" . ;;** symbol?
       (1 "string" "the name of the x-axis variable"))
      ("write" .
       (1 "special" "output data range (fit or full)"))
      ("xmin" .
       (1 "number" "minimum data range"))
      ("xmax" .
       (1 "number" "maximum data range"))
      ("x1" .
       (1 "number" "a data range knot"))
      ("x2" .
       (1 "number" "a data range knot"))
      ("x3" .
       (1 "number" "a data range knot"))
      ("x4" .
       (1 "number" "a data range knot"))
      ("x5" .
       (1 "number" "a data range knot"))
      ("x6" .
       (1 "number" "a data range knot"))
      ("x7" .
       (1 "number" "a data range knot"))
      ("x8" .
       (1 "number" "a data range knot"))
      ("x9" .
       (1 "number" "a data range knot"))
      ("x10" .
       (1 "number" "a data range knot"))
      ("npoints" .
       (1 "integer" "the number of output data points"))
      ("all" .
       (0 "none" "none.  Switch for writing individual functions."))
      ("nosum" .
       (0 "none" "none.  Switch for not writing sum of functions."))
      ("noout" .
       (0 "none" "none.  Switch for writing no output files."))
      ("resid" .
       (0 "none" "none.  Switch for writing residual function."))
      ("noerr" .
       (0 "none" "none.  Switch for not doing error enanlysis."))
      ("norun" .
       (0 "none" "none.  Switch for only reading input file."))
      ("dryrun" .
       (0 "none" "none.  Switch for only reading input file."))
      ("nofit" .
       (0 "none" "none.  Switch for not performing fit."))
      ("line" .
       (1 "readable" "a file with an input lineshape"))
      ("set" .
       (1 "setguess" "a set parameter name and value"))
      ("guess" .
       (1 "setguess" "a guess parameter name and value"))
      ("function" .
       (1 "pathparam" "a function index and a math expression"))
      ("id" .
       (1 "pathparam" "a function index and a comment string"))
      ))


(defvar Phit-output-files)
(setq Phit-output-files (list ".log"))

(defvar Phit-highlight-list
  (list
   '("\\<\\(\\(data\\|line\\|include\\|infile\\|read\\)[ \t]*[ \t=,][ \t]*\\sw+\\)\\>" 0)
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; phit templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Phit-template
  '("title = "  p n
    "data = "   input-data-path p n
    "out = "    input-out-path  p n
    "format = " p n
    "xaxis = "  p "\txmin = " p "\txmax = " p n n
    input-comment-delimiter n n)
  "Template for an *phit* header.  Inserted by \\[Phit-make-template].")

(tempo-define-template "Phit-template" Phit-template
		       nil
		       "Insert an empty Phit template.")
(eval-when-compile (defun tempo-template-Phit-template))

(defun Phit-make-template ()
  "Write a header template for *phit* using a tempo template.
Bound to \\[Phit-make-template]."
  (interactive)
  (tempo-template-Phit-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				        ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))


(defvar Phit-function-template
  '("func 1 " p n
    "id   1 " p n n)
  "Template for an *phit* function paragraph.
Inserted by  \\[Phit-make-function-template].")

(tempo-define-template "Phit-function-template" Phit-function-template
		       nil
		       "Insert an empty Phit function paragraph template.")
(eval-when-compile (defun tempo-template-Phit-function-template))

(defun Phit-make-function-template ()
  "Write a template for a *phit* function paragraph using a tempo template.
Bound to \\[Phit-make-function-paragraph]."
  (interactive)
  (tempo-template-Phit-function-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (setq Feffit-path-index (1+ Feffit-path-index))
  (Feffit-renumber-paragraph Feffit-path-index)
  (cond (input-use-hilit19
	 (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				        ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))

(defun fuse-Phit-choose-template ()
  (interactive)
  (let ((alist '(("header"   . "Phit-make-template")
		 ("function" . "Phit-make-function-template")))
	type )
    (setq type (completing-read
		"What type of template? (<tab> for a complete list) " alist nil t))
    (if type
	(funcall (intern (cdr (assoc type alist)))) ) ))
;;; end of templates for phit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Phit-parse-file ()
  "Parse current data set to get information for plotting.
Store information in Phit-features-alist.  The following
items are returned: values of out, formin, formout, a list of
function indeces, datafile name, outfile base name, and first title."
  (interactive)
  (save-excursion
    (let ((case-fold-search t) lower-limit title formin sigma xaxis
	  formout format nofit allout key datafile outbase (indeces ()) mark-1
	  eol at-top )
      (setq Phit-features-alist nil)
					; define bounds
      (setq lower-limit
	    (save-excursion
	      (re-search-forward "\\<end\\>" (point-max) "to_limit")
	      (point-marker)))

					; get formin/formout
      (setq formin
	    (Feffit-parsed-value "formin"  (point-min) "uw" "uwxafs"))
      (setq formout
	    (Feffit-parsed-value "formout" (point-min) "uw" "uwxafs"))
      (setq format
	    (Feffit-parsed-value "format"  (point-min) "uw" "uwxafs"))
      (if (and (not formin) (string= format "uwxafs"))
	  (setq formin "uwxafs"))
      (if (not formin) (setq formin "ascii"))
      (if (and (not formout) (string= format "uwxafs"))
	  (setq formout "uwxafs"))
      (if (not formout) (setq formout "ascii"))

					; get output flags
					; find data, out names
      (setq datafile (Feffit-get-word (point-min) lower-limit
				      "data\\|infile\\|read"))
      (if (string= formin "uwxafs")
	  (setq key (Feffit-get-word (point-min) lower-limit "[ns]key")))


      (setq outbase  (Feffit-get-word (point-min) lower-limit "out"))
      (if outbase                       ; remove extension from outbase
	  (setq outbase (file-name-sans-extension outbase))
	(if datafile                    ; default outbase is datafile base
	    (setq outbase (file-name-sans-extension datafile))))

					; all out flag
      (setq allout (Phit-get-switch "all\\(\\|out\\)"))
      (setq nofit  (Phit-get-switch "nofit"))

					; get list of path indeces
      (goto-char (point-min))
      (while (not (input-last-paragraph-p))
	(input-forward-paragraph)
	(setq indeces (cons (nth 0 (input-path-param-value)) indeces)) )
      (setq indeces (reverse indeces))

      (goto-char (point-min))
      (setq sigma (re-search-forward "\\<sigma\\>" lower-limit t))

      (goto-char lower-limit)
      (if (re-search-backward "\\<xaxis\\>" (point-min) t)
	  (progn
	    (forward-word 2)
	    (setq xaxis (input-this-word)))
	(setq xaxis "x"))

					; first title line
      (goto-char (point-min))
      (cond ((re-search-forward "\\<title\\>" lower-limit t)
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point-marker))
	     (end-of-line)
	     (setq eol    (point-marker))
	     (goto-char mark-1)
	     (re-search-forward input-comment-expr eol "to_eol")
	     (setq title (buffer-substring-no-properties mark-1 (point)))))

					; make alist
      (setq Phit-features-alist
	    (append
	     (list (cons "title"    title))
	     (list (cons "nofit"    nofit))
	     (list (cons "xaxis"    xaxis))
	     (list (cons "sigma"    sigma))
	     (list (cons "allout"   allout))
	     (list (cons "datafile" datafile))
	     (list (cons "key"      key))
	     (list (cons "outbase"  outbase))
	     (list (cons "formin"   formin))
	     (list (cons "formout"  formout))
	     (list (cons "indeces"  indeces)) ))

      (if (interactive-p)
	  (message "Parsed %s input file: %s"
		   input-program-name
		   (file-name-nondirectory buffer-file-name)))
      ;;(message "%S" Phit-features-alist)

      )))


(defun Phit-get-switch (expr)
  "Locate a switch in a phit input file.
Take care not to match EXPR in a title or comment."
  (save-excursion
    (let ((found  nil)
	  (regexp nil))
      (goto-char (point-max))
      (setq regexp (concat "\\<" expr "\\>"))
      (while (not (or found (bobp)))
	(cond ((re-search-backward regexp (point-min) "to_limit")
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (setq found t)))))
      found)))

;;; end template for phit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions

;;    (setq my-expr (make-regexp '("skey" "nkey" "format" "formin"
;;				 "formout" "sigdat" "sigma" "cormin"
;;				 "xaxis" "write" "xmin" "xmax" "x1"
;;				 "x2" "x3" "x4" "x5" "x6" "x7" "x8"
;;				 "x9" "x10" "npoints" "all" "nosum"
;;				 "noout" "noerr" "norun" "nofit"
;;				 "dryrun" "resid" "residual")))

(defconst Phit-font-lock-keywords
      '(
					; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#].*$" . font-lock-comment-face)
					; titles
	("\\<\\(comment\\|title\\)\\>" . font-lock-string-face)
					; PHIT keywords
	("\\<\\(all\\|cormin\\|dryrun\\|form\\(at\\|in\\|out\\)\\|n\\(key\\|o\\(err\\|fit\\|out\\|run\\|sum\\)\\|points\\)\\|resid\\(\\|ual\\)\\|s\\(ig\\(dat\\|ma\\)\\|key\\)\\|write\\|x\\([123456789]\\|10\\|axis\\|m\\(ax\\|in\\)\\)\\)\\>" . font-lock-keyword-face)

	("\\<\\(data\\|echo\\|in\\(file\\|clude\\)\\|l\\(og\\(\\|file\\)\\|ine\\)\\|out\\(\\|file\\)\\|read\\)\\>" . font-lock-function-name-face)

	("^[ \t]*\\(guess\\|set\\)\\>" . font-lock-function-name-face)

	("^[ \t]*\\(fun\\(c\\|ction\\)?\\|id\\)\\>" . font-lock-type-face) ))

(defconst Phit-font-lock-keywords-1 nil)
(setq Phit-font-lock-keywords-1   Phit-font-lock-keywords)
(defconst Phit-font-lock-keywords-2 nil)
(setq Phit-font-lock-keywords-2   Phit-font-lock-keywords)

(defun Phit-set-hilit ()
  "Function used to call hilit-set-mode-patterns."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
					; comentlines
	    ("\\<\\(comment\\|title\\)\\>" nil define)
					; control keywords
	    ("\\<\\(data\\|echo\\|in\\(file\\|clude\\)\\|l\\(og\\(\\|file\\)\\|ine\\)\\|out\\(\\|file\\)\\|read\\)\\>" nil include)
					; keywords
	    ("\\<\\(all\\|cormin\\|dryrun\\|form\\(at\\|in\\|out\\)\\|n\\(key\\|o\\(err\\|fit\\|out\\|run\\|sum\\)\\|points\\)\\|resid\\(\\|ual\\)\\|s\\(ig\\(dat\\|ma\\)\\|key\\)\\|write\\|x\\([123456789]\\|10\\|axis\\|m\\(ax\\|in\\)\\)\\)\\> " nil keyword)
					;set and guess
	    ("^[ \t]*\\(guess\\|set\\)\\>" nil inlcude)
					; function parameters
	    ("^[ \t]*\\(fun\\(\\|c\\|ction\\)\\|id\\)\\>" nil glob-struct)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Phit-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Phit-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Phit-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Phit-output-files)
(setq Phit-output-files (list ".log"))
(setq input-output-files Phit-output-files)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for phit
(defvar Phit-mode-map ()
  "Keymap used in Phit minor mode.")
(if Phit-mode-map
    ()
  (setq Phit-mode-map (make-sparse-keymap))
  (define-key Phit-mode-map "\C-c\C-tt"  'Phit-make-template)
  (define-key Phit-mode-map "\C-c\C-tf"  'Phit-make-function-template)

  (define-key Phit-mode-map "\C-c\C-of"  'input-forward-paragraph)
  (define-key Phit-mode-map "\C-c\C-ob"  'input-backward-paragraph)
  (define-key Phit-mode-map "\C-c\C-om"  'input-mark-paragraph)
  (define-key Phit-mode-map "\C-c\C-ok"  'input-kill-paragraph)
  (define-key Phit-mode-map "\C-c\C-sp"  'input-snag-from-previous-paragraph)
  (define-key Phit-mode-map "\C-c\C-sn"  'input-snag-from-next-paragraph)
  (define-key Phit-mode-map "\C-c\C-sb"  'Feffit-insert-best-fit)
  (define-key Phit-mode-map "\C-c\C-sg"  'Feffit-insert-this-best-fit)

  (define-key Phit-mode-map "\C-c\C-pp"  'Phit-plot-fit)
  )
(defvar Phit-mode-menu nil)
(easy-menu-define
 Phit-mode-menu Phit-mode-map
 "Menu used in Phit mode"
 '("Phit"
   ["Make header template"                   Phit-make-template t]
   ["Make function template"                 Phit-make-function-template t]
   ["--------- Editing Shortcuts --------"   input-t t]
   ("Move/Mark/Kill"
    ["Next function paragraph"               input-forward-paragraph t]
    ["Previous function paragraph"           input-backward-paragraph t]
    ["Mark function paragraph"               input-mark-paragraph t]
    ["Kill function paragraph"               input-kill-paragraph t])
   ("Paragraph Manipulation"
    ["---- Numbering Paragraphs -----------" input-t t]
    ["Renumber paragraph"                    Feffit-renumber-paragraph t]
    ["Renumber file"                         Feffit-renumber-data-set t]
    ["Renumber file from point"              Feffit-renumber-from-point t]
    ["Reset function index"                  Feffit-set-path-index t])
    ;;["---- Editing paragraphs -------------" input-t t]
    ;;["Add parameter to all paragraphs"       Feffit-add-path-param t]
    ;;["Comment parameter in all paragraphs"   Feffit-comment-path-param t]
    ;;["Uncomment parameter in all paragraphs" Feffit-uncomment-path-param t]
    ;;["Remove parameter from all paragraphs"  Feffit-remove-path-param t]

    ;;; Why does snagging not work for id line??????
    ("Snagging"
     ["Snag from previous paragraph"       input-snag-from-previous-paragraph t]
     ["Snag from next paragraph"           input-snag-from-next-paragraph t])
    ;;("Best fit values"
    ;; ["Replace guesses with best fit"         Feffit-insert-best-fit t]
    ;; ["Replace this guess with best fit"      Feffit-insert-this-best-fit t])
   ["--------- Running ------------------"   input-t t]
   ["Run phit, master file"                  input-run-this-program-this-file t]
   ["Kill phit run"                          input-kill-program
    :active (get-process input-process)]
   ["--------- Plotting -----------------"   input-t t]
   ["Plot fit"                               Phit-plot-fit t]
   ["Toggle gnuplot terminal"                input-toggle-gnuplot-terminal t]
   ["--------- Et cetera ----------------"   input-t t]
   ["Look at log file"                       input-jump-to-log-file
    :active (input-log-file-exists-p)]
   ("Clean up"
    ["Clean line"                            input-clean-line t]
    ["Clean region"                          input-clean-region t]
    ["Clean file"                            input-clean-file t]
    ["Clean paragraph"                       Feffit-clean-path-paragraph t]
    ["Clean data set"                        Feffit-clean-data-set t])
   ("TAGS"
    ["Make TAGS file"                        Feffit-make-tags-file t]
    ["Find tag"                              find-tag t]
    ["Find tag other window"                 find-tag-other-window t]
    ["Find tag other frame"                  find-tag-other-frame t])
   ["Display phit keywords"                  input-display-keywords t]
   ;;["Reparse phit file"                      Phit-parse-file t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Phit-imenu-generic-expression
  '(("Guesses"   "\\bguess[ \t]*[ \t=,][ \t]*\\(\\w+\\)" 1)
    ("Sets"      "\\bset[ \t]*[ \t=,][ \t]*\\(\\w+\\)"   1))
  "Imenu generic expression for Phit mode.  See `imenu-generic-expression'.")

(defvar Phit-mode nil
  "Determines if Phit minor mode is active.")
(make-variable-buffer-local 'Phit-mode)


;;;###autoload
(defun Phit-mode (&optional arg)
  "Minor mode for editing input files for Phit.
ARG t turns on *phit* minor mode.

Defined keys in Phit minor mode:\n \\{Phit-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Phit-mode
	       (if (null arg) (not Phit-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Phit-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Phit-mode " Phit") minor-mode-alist)))
	 (or (assq 'Phit-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Phit-mode Phit-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Phit-mode
	     (progn
	       (make-local-variable 'imenu-generic-expression)
	       (easy-menu-add Phit-mode-menu)
	       (setq imenu-generic-expression Phit-imenu-generic-expression
		     input-current-keywords-alist
		     (copy-alist Phit-keywords-alist)
		     input-output-files Phit-output-files
		     fuse-mouse-highlight-list Phit-highlight-list)

	       (setq input-program-setguess-flag    t
		     input-program-master-flag      t
		     input-program-logfile-flag     t
		     input-program-stanza-flag      nil
		     input-program-data-flag        t
		     input-program-feff-flag        nil
		     input-program-list-flag        nil
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Bruce Ravel" "ravel@phys.washington.edu")
		     input-program-parse
		        '(Phit-parse-file Phit-features-alist)
		     input-program-hilit            'Phit-set-hilit)

	       (cond (input-use-hilit19 (Phit-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Phit-mode-hook))
	   (easy-menu-remove Phit-mode-menu))
	 )
	(t
	 (message "Phit minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-phit)
(run-hooks 'Phit-load-hook)

;;;============================================================================
;;;
;;; fuse-phit.el end here
