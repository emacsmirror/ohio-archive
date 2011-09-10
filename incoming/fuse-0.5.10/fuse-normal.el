;;; fuse-normal.el --- minor mode for editing input files for normal

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Normal.el,v 1.2 1998/03/14 22:45:58 bruce Exp $

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

;;; Code:

(require 'input)
(require 'fuse-gnuplot)
(require 'tempo)

(defcustom Normal-mode-hook nil
  "*Hook run when Normal minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Normal-load-hook nil
  "*Hook run when fuse-normal.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defvar Normal-features-alist nil
  "Alist containing information parsed from an normal data set.")

(defvar Normal-keywords-alist 'nil
"This is the normal associated list of keywords and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Normal-keywords-alist '(
      ("end" .
       (0 "none"
	  "no argument.  Normal stops reading the input file after \"end\"."))
      ("nofit" .
       (0 "none" "no argument. \"nofit\" turns off alignment."))
      ("noalign" .
       (0 "none" "nothing. \"noalign\" turns off alignment."))
      ("normalize" .
       (0 "none" "nothing. \"normalize\" turns off alignment."))
      ("derivative" .
       (0 "none" "nothing. \"derivative\" turns on derivative output."))
      ("message" .
       (1 "integer" "an index specifying run-time messages."))
      ("e0" .
       (1 "number" "the edge energy (absolute energy)"))
      ("ee" .
       (1 "number" "the edge energy (absolute energy)"))
      ("pre1" .
       (1 "number" "the first pre-edge boundary (relative to e0)"))
      ("pre2" .
       (1 "number" "the second pre-edge boundary (relative to e0)"))
      ("nor1" .
       (1 "number" "the first normalization boundary (relative to e0)"))
      ("nor2" .
       (1 "number" "the second normalization boundary (relative to e0)"))
      ("emax" .
       (1 "number" "maximum alignment energy (relative to e0)"))
      ("format" .
       (1 "datatype" "i/o file format (uwxafs/ascii)"))
      ("formin" .
       (1 "datatype" "input file format (uwxafs/ascii)"))
      ("formout" .
       (1 "datatype" "output file format (uwxafs/ascii)"))
      ("out" .
       (1 "writable" "the base name for output files"))
      ("files" .
       (0 "none" "a files list begins of the next line"))
      ("data" .
       (0 "none" "a files list begins of the next line"))
      ))

(defvar Normal-output-files '())
(defvar Normal-highlight-list
  (list
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; normal template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Normal-template
  '("e0 = " p "\t! derivative" n
    "out = " input-out-path p "\tformat = " p n
    "files " n p input-data-path n)
  "Template for an *normal* input file.  Inserted by \\[Normal-make-template].")

(tempo-define-template "Normal-template" Normal-template
		       nil
		       "Insert an empty Normal template.")
(eval-when-compile (defun tempo-template-Normal-template))

(defun Normal-make-template ()
  "Write a template for *normal* using a tempo template.
Bound to \\[Normal-make-template]."
  (interactive)
  (tempo-template-Normal-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))


;;; end template for normal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Normal-parse-file ()
  (interactive)
  (save-excursion
    (let (format formin formout basename (files ()) (keys ()) )

      (goto-char (point-max))
					; formin/out
      (setq formin
	    (Normal-parsed-value "formin"  (point-min) "uw" "uwxafs"))
      (setq formout
	    (Normal-parsed-value "formout" (point-min) "uw" "uwxafs"))
      (setq format
	    (Normal-parsed-value "format"  (point-min) "uw" "uwxafs"))
      (if (and (not formin) (string= format "uwxafs"))
	  (setq formin "uwxafs"))
      (if (not formin) (setq formin "ascii"))
      (if (and (not formout) (string= format "uwxafs"))
	  (setq formout "uwxafs"))
      (if (not formout) (setq formout "ascii"))

      (cond ((re-search-backward "\\<out\\>" (point-min) t)
	     (re-search-forward input-word-sep)
	     (setq basename (input-this-word))
	     (setq basename (file-name-sans-extension basename)) ))

      (goto-char (point-max))
      (cond ((re-search-backward "^[ \t]*\\(files\\|data\\)\\>" (point-min) t)
	     (forward-line 1)
	     (while (not (eobp))
					; skip blank and comment lines
	       (if (or (input-comment-p) (looking-at "^[ \t]*$"))
		   (forward-line 1)
					; add filename to list
		 (back-to-indentation)
		 (setq files (append files (list (input-this-word)) ))
		 (cond ((string= formin "uwxafs")
			(re-search-forward input-word-sep)
			(setq keys (append keys (list (input-this-word)))) ))
		 (forward-line 1))) ))

      (cond ((not basename)
	     (setq basename (nth 0 files))
	     (setq basename (file-name-sans-extension basename)) ))

      (setq Normal-features-alist
	    (append
	     (list (cons "formin"   formin))
	     (list (cons "formout"  formout))
	     (list (cons "basename" basename))
	     (list (cons "files"    files))
	     (list (cons "keys"     keys))  ))
      )))


(defun Normal-parsed-value (keyword limit regexp value &rest default)
  "Find a given keyword and return its value.
Care is taken if the keyword is commented out.  Used in
Normal-parse-file.  KEYWORD is the keyword to search for, LIMIT is the
extent of the search, REGEXP is the keyword value to recognize, VALUE
is the keyword value to assign if REGEXP is found, DEFAULT is the
keyword value to assign if REGEXP is not found.  Used for logical and
i/o format keywords"
  (save-excursion
    (let (match return (case-fold-search t))
      (setq match (concat "\\<" keyword "\\>"))
      (cond ((re-search-backward match limit t)   ; find keyword
	     (if (input-comment-p)                ; return default if commented
		 (setq return default)
	       (re-search-forward input-word-sep)
	       (if (looking-at regexp)
		   (setq return value)            ; found regexp
		 (setq return default))))         ; did not find regexp
	    (t
	     (setq return default)))              ; did not find keyword
      return)))                                   ; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions

;;    (setq my-expr (make-regexp '( "nofit" "noalign" "normalize"
;;				  "derivative" "message" "e0" "ee"
;;				  "pre1" "pre2" "nor1" "nor2" "emax"
;;				  "format" "formin" "formout" "out" )))



(defconst Normal-font-lock-keywords
      '(
	("[%!#].*$" . font-lock-comment-face)
	("^[ \t]*\\*.*$" . font-lock-comment-face)

	("\\<\\(end\\|files\\|data\\)\\>" . font-lock-function-name-face)

	("\\<\\(derivative\\|e\\([0e]\\|max\\)\\|form\\(at\\|in\\|out\\)\\|message\\|no\\(align\\|fit\\|r\\([12]\\|malize\\)\\)\\|out\\|pre[12]\\)\\>" . font-lock-keyword-face)	))

(defconst Normal-font-lock-keywords-1 nil)
(setq Normal-font-lock-keywords-1   Normal-font-lock-keywords)
(defconst Normal-font-lock-keywords-2 nil)
(setq Normal-font-lock-keywords-2   Normal-font-lock-keywords)

(defun Normal-set-hilit ()
  "Function used to call hilit-set-mode-patterns."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)

	    ("\\<\\(derivative\\|e\\([0e]\\|max\\)\\|form\\(at\\|in\\|out\\)\\|message\\|no\\(align\\|fit\\|r\\([12]\\|malize\\)\\)\\|out\\|pre[12]\\)\\>" nil keyword)

	    ("\\<\\(end\\|file\\|data\\)\\>" nil include)

	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Normal-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Normal-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Normal-font-lock-keywords-2)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Normal-output-files)
(setq Normal-output-files '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for normal
(defvar Normal-mode-map ()
  "Keymap used in Normal minor mode.")
(if Normal-mode-map
    ()
  (setq Normal-mode-map (make-sparse-keymap))
  (define-key Normal-mode-map "\C-c\C-tt"  'Normal-make-template)
  (define-key Normal-mode-map "\C-c\C-pa"  'Normal-plot-all)
  (define-key Normal-mode-map "\C-c\C-px"  'Normal-plot-xmu)
  (define-key Normal-mode-map "\C-c\C-pt"  'Normal-plot-this-xmu)
  (define-key Normal-mode-map "\C-c\C-pn"  'Normal-plot-this-norm)
  )
(defvar Normal-mode-menu nil)
(easy-menu-define
 Normal-mode-menu Normal-mode-map
 "Menu used in Normal mode"
 '("Normal"
   ["Make template"                 Normal-make-template t]
   ["Clean up input file"           input-clean-file t]
   ["--------- Running --------"    input-t t]
   ["Run normal, this file"         input-run-this-program-this-file t]
   ["Kill normal run"               input-kill-program
    :active (get-process input-process)]
   ["--------- Plotting -------"    input-t t]
   ["Plot all normalized files"     Normal-plot-all t]
   ["Plot all xmu files"            Normal-plot-xmu t]
   ["Plot this normalized file"     Normal-plot-this-norm t]
   ["Plot this xmu file"            Normal-plot-this-xmu t]
   ["Toggle gnuplot terminal"       input-toggle-gnuplot-terminal t]
   ["--------- Et cetera ------"    input-t t]
   ["Display normal keywords"       input-display-keywords t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Normal-imenu-generic-expression nil
  "Imenu generic expression for Normal mode.  See `imenu-generic-expression'.")

(defvar Normal-mode nil
  "Determines if Normal minor mode is active.")
(make-variable-buffer-local 'Normal-mode)


;;;###autoload
(defun Normal-mode (&optional arg)
  "Minor mode for editing input files for Normal.
ARG t turns on *normal* minor mode.

Defined keys in Normal minor mode:\n \\{Normal-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Normal-mode
	       (if (null arg) (not Normal-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Normal-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Normal-mode " Normal") minor-mode-alist)))
	 (or (assq 'Normal-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Normal-mode Normal-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Normal-mode
	     (progn
	       (make-local-variable 'imenu-generic-expression)
	       (easy-menu-add Normal-mode-menu)
	       (setq imenu-generic-expression Normal-imenu-generic-expression
		     input-current-keywords-alist
		     (copy-alist Normal-keywords-alist)
		     input-output-files Normal-output-files
		     fuse-mouse-highlight-list Normal-highlight-list)

	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     nil
		     input-program-stanza-flag      nil
		     input-program-data-flag        t
		     input-program-feff-flag        nil
		     input-program-list-flag        t
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Bruce Ravel" "ravel@phys.washington.edu")
		     input-program-parse
		        '(Normal-parse-file     Normal-features-alist)
		     input-program-hilit            'Normal-set-hilit)

	       (cond (input-use-hilit19 (Normal-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Normal-mode-hook))
	   (easy-menu-remove Normal-mode-menu))
	 )
	(t
	 (message "Normal minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-normal)
(run-hooks 'Normal-load-hook)

;;;============================================================================
;;;
;;; fuse-normal.el end here
