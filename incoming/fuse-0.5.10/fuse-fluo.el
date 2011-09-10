;;; fuse-fluo.el --- minor mode for editing fluo input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  4 June 1999
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Fluo.el,v 1.2 1998/03/14 22:45:57 bruce Exp $

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
(require 'fuse-normal)
(require 'tempo)

(if (fboundp 'file-name-extension) ()
  (defun file-name-extension (filename &optional period)
    "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that follows the last `.'.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

If PERIOD is non-nil, then the returned value includes the period
that delimits the extension, and if FILENAME has no extension,
the value is \"\"."
    (save-match-data
      (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
	(if (string-match "\\.[^.]*\\'" file)
	    (substring file (+ (match-beginning 0) (if period 0 1)))
	  (if period
	      ""))))) )



(defcustom Fluo-mode-hook nil
  "*Hook run when Fluo minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Fluo-load-hook nil
  "*Hook run when fuse-fluo.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defconst Fluo-keywords-alist ""
"This is an associated list of all keywords for fluo and their attributes.
See the documentation for `input-current-keywords-alist'."
)


(setq Fluo-keywords-alist '(
					; I/O keywords
      ;;("title" .
       ;;(0 "title" "a user-defined comment string"))
      ;;("comment" .
       ;;(0 "title" "a user-defined comment string"))
      ("e0" .
       (1 "number" "the energy origin of the data"))
      ("pre1" .
       (1 "number" "a real number (lower bound of pre-edge region)"))
      ("pre2" .
       (1 "number" "a real number (upper bound of pre-edge region)"))
      ("nor1" .
       (1 "number" "a real number (lower bound of post-edge region)"))
      ("nor2" .
       (1 "number" "a real number (upper bound of post-edge region)"))
      ("out" .
       (1 "writable" "the base of the output file name"))
      ("format" .
       (1 "datatype" "an i/o file format (uwxafs or ascii)"))
      ("formin" .
       (1 "datatype" "an input file format (uwxafs or ascii)"))
      ("formout" .
       (1 "datatype" "an output file format (uwxafs or ascii)"))
      ("angin" .
       (1 "number" "incoming angle in degrees"))
      ("angout" .
       (1 "number" "outgoing angle in degrees"))
      ("central" .
       (1 "string" "the atomic symbol of the absorbing atom"))
      ("edge" .
       (1 "string" "the absorption edge of interest"))
      ("atmwei" .
       (1 "logical" "a logical flag (atom list given as atomic weights)"))
      ("atoms" .
       (0 "none" "no argument, the atoms list begins on the following line"))
      ("files" .
       (0 "none" "no argument, the files list begins on the following line"))
      ))


(defvar Fluo-output-files)
(setq Fluo-output-files ())

(defvar Fluo-highlight-list ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template for fluo.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Fluo-template
  '("e0 = " p n
    "central = " p "\tedge = " p n
    "out = " input-out-path p n
    "atoms"  n "  " p n
    "files"  n "  " p n)
  "Template for an *fluo* stanza.  Inserted by \\[Fluo-make-template].")

(tempo-define-template "Fluo-template" Fluo-template
		       nil
		       "Insert an empty Fluo template.")
(eval-when-compile  (defun tempo-template-Fluo-template))

(defun Fluo-make-template ()
  "Write a template for *fluo* using a tempo template.
Bound to \\[Fluo-make-template]."
  (interactive)
  (tempo-template-Fluo-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))

;;; end template for fluo.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special functions for fluo

(defvar Fluo-features-alist nil
  "Alist containing information parsed from a fluo input file.")

(defun Fluo-parse-file ()
  (interactive)
  (save-excursion
    (let (format formin formout basename ext (files ()) (keys ()) )
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

      (when (re-search-backward "\\<out\\>" (point-min) t)
	(re-search-forward input-word-sep)
	(setq basename (input-this-word)
	      basename (file-name-sans-extension basename)
	      ext      (or (file-name-extension  basename nil) "") ))

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
			(setq keys (append keys (list (input-this-word)))) )
		       (t
			(setq keys (append keys (list nil))) ) )
		 (forward-line 1))) ))

      (when (not basename)
	(setq basename (nth 0 files)
	      basename (file-name-sans-extension basename)
	      ext      (or (file-name-extension  basename t) "")))

      (setq Fluo-features-alist
	    (append
	     (list (cons "formin"   formin))
	     (list (cons "formout"  formout))
	     (list (cons "basename" basename))
	     (list (cons "ext"      ext))
	     (list (cons "files"    files))
	     (list (cons "keys"     keys))  )) )))


(defun Fluo-plot-all-xmu ()
  "Plot all unnormalized xmu files in a *fluo* input file.
Bound to \\[Fluo-plot-xmu]"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (Fluo-parse-file)
    (let (plot lines file key
	       (input-buffer (current-buffer))
	       (formin  (cdr (assoc "formin"   Fluo-features-alist)))
	       (files   (cdr (assoc "files"    Fluo-features-alist)))
	       (keys    (cdr (assoc "keys"     Fluo-features-alist))))
      (cond (files
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "Raw data")
			   (input-gnuplot-set-terminal input-gnuplot-terminal)
			   (input-gnuplot-axis-labels "Energy" "Absorption")))
	     (setq input-current-terminal input-gnuplot-terminal)

	     (setq plot "plot ")
	     (while files
	       (setq file (car files)
		     key  (car keys))
	       (if (string= formin "ascii")
		   (setq lines (concat lines (format "%s'%s',\\\n" plot file)))
		 (setq lines
		       (concat lines
			       (format "%s'<mr %s, %s',\\\n" plot file key))))
	       (setq files (cdr files)
		     keys  (cdr keys)
		     plot "     "))
					; last line
	     (setq lines (substring lines 0 -3)
		   lines (concat lines "\n"))
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* (length lines) input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) )) )
	    (t
	     (message "No file list was found in this input file.")))
      )))


(defun Fluo-plot-all-out ()
  "Plot all output files from a *fluo* input file.
Bound to \\[Fluo-plot-all]"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (Fluo-parse-file)
    (let (lines plot cont (counter 0) (input-buffer (current-buffer))
		(formout  (cdr (assoc "formout"  Fluo-features-alist)))
		(files    (cdr (assoc "files"    Fluo-features-alist)))
		(basename (cdr (assoc "basename" Fluo-features-alist)))
		(ext      (cdr (assoc "ext"      Fluo-features-alist))) )
					; find file list
      (cond (files
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "Corrected data")
			   (input-gnuplot-set-terminal input-gnuplot-terminal)
			   (input-gnuplot-axis-labels
			    "Energy (eV)" "Absorption")))
	     (setq input-current-terminal input-gnuplot-terminal)
	     (setq plot "plot "
		   cont "")
	     (while files
	       (when (> counter 0) (setq plot "     " cont ",\\\n"))
	       (setq counter (1+ counter)
		     files   (cdr files))
	       (if (string= formout "ascii")
		   (setq lines (concat lines
				       (format "%s%s'%s.%03d'"
					       cont plot basename counter)))
		 (setq lines (concat lines
				     (format "%s%s'<mr %s%s, %d'"
					     cont plot ext basename counter))) ) )
	     (setq lines (concat lines "\n"))
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* (- (length lines) 5) input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) )) )
	    (t
	     (message "No file list was found in this input file.")))
      )))

(defun Fluo-plot-this-out ()
  "Plot the normalized data from the current line of a *fluo* input file.
Bound to \\[Fluo-plot-this-norm]"
  (interactive)
  (let (lines file (input-buffer (current-buffer)) basename ext
	      formin formout in_list index key eol)
    (Fluo-parse-file)
    (setq basename (cdr (assoc "basename" Fluo-features-alist))
	  ext      (cdr (assoc "ext"      Fluo-features-alist))
	  formin   (cdr (assoc "formin"   Fluo-features-alist))
	  formout  (cdr (assoc "formout"  Fluo-features-alist)))
    (save-excursion
      (if (re-search-backward "^\\s-*\\(file\\|data\\)" (point-min) t)
	  (setq in_list t)))
    (save-excursion
      (back-to-indentation)
      (setq file (input-this-word))
      (cond ((not in_list)
	     (message "Point is not on a file line."))
	    ((not  (file-readable-p file))
	     (message "That's not a readable file."))
	    (t
	     (setq index (position file
				   (cdr (assoc "files" Fluo-features-alist))
				   :test 'string=)
		   eol (save-excursion (end-of-line) (point-marker))
		   key (save-excursion
			 (if (re-search-forward input-word-sep eol t)
			     (input-this-word) "" ))
		   index (1+ index)
		   lines (concat (input-gnuplot-cd-and-title
				  (file-name-directory buffer-file-name)
				  "Raw and corrected data")
				 (input-gnuplot-set-terminal
				  input-gnuplot-terminal)
				 (input-gnuplot-axis-labels
				  "Energy (eV)" "Absorption"))
		   input-current-terminal input-gnuplot-terminal)
	     (if (string= formin "ascii")
		 (setq lines
		       (concat lines (format "plot '%s',\\\n"
					     file) ))
	       (setq lines
		     (concat lines (format "plot '<mr %s%s, %s',\\\n"
						 file ext key) )) )
	     (if (string= formout "ascii")
		 (setq lines
		       (concat lines (format "     '%s.%03d'\n"
					     basename index) ))
	       (setq lines
		     (concat lines (format "     '<mr %s%s, %d'\n"
						 basename ext index) )) )
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* 3 input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) ))))
    )))


;;; end of special functions for fluo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------------------------------------
;; setup toolbar for XEmacs

(when (and fuse-xemacs-p (featurep 'toolbar))
  (require 'fuse-toolbar)
  (set 'Fluo-toolbar-all-out-xpm  Normal-toolbar-all-nor-xpm)
  (set 'Fluo-toolbar-all-xmu-xpm  Normal-toolbar-all-xmu-xpm)
  (set 'Fluo-toolbar-this-out-xpm Normal-toolbar-this-nor-xpm)
  (defvar Fluo-toolbar
    '([fuse-toolbar-template-xpm   Fluo-toolbar-template
				   t "Write an Fluo template"]
      [fuse-toolbar-run-xpm        Fluo-toolbar-run
				   t "Run Fluo"]
      [:style 3d :size 8]
      [Fluo-toolbar-this-out-xpm   Fluo-toolbar-this-out
				   t "Plot raw and output mu(E)"]
      [Fluo-toolbar-all-out-xpm    Fluo-toolbar-all-out
				   t "Plot all output mu(E) in file"]
      [Fluo-toolbar-all-xmu-xpm    Fluo-toolbar-all-xmu
				   t "Plot all un-normalized mu(E) in file"]
      [:style 3d :size 8]
      [fuse-toolbar-helper-xpm     Fluo-toolbar-helper
				   t "Display Fluo keywords"]
      [fuse-toolbar-quit-xpm       Fluo-toolbar-quit
				   t "Quit editing this input file"]
      )
    "The Fluo toolbar.")

  (add-hook 'Fluo-mode-hook
	    '(lambda ()
	       (and (featurep 'toolbar)
		    (fuse-make-toolbar-function Fluo-toolbar))))

  (fset 'Fluo-toolbar-template 'Fluo-make-template)
  (fset 'Fluo-toolbar-run      'input-run-this-program-this-file)
  (fset 'Fluo-toolbar-this-out 'Fluo-plot-this-out)
  (fset 'Fluo-toolbar-all-out  'Fluo-plot-all-out)
  (fset 'Fluo-toolbar-all-xmu  'Fluo-plot-all-xmu)
  (fset 'Fluo-toolbar-helper   'input-display-keywords)
  (fset 'Fluo-toolbar-quit     'fuse-quit)  )
;;----------------------------------------------------------------------




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions

;; (insert (format "%s"
;; 		(regexp-quote
;; 		 (make-regexp '("format" "formin" "formout" "e0"
;; 				"pre1" "pre2" "nor1" "nor2" "atmwei"
;; 				"out" "angin" "angout" "central" "edge"
;; 				)))))

(defconst Fluo-font-lock-keywords
      '(				; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#].*$" . font-lock-comment-face)
					; titles
	("\\<\\(comment\\|title\\).*$" . font-lock-string-face)
	("\\*.*$" . font-lock-string-face)
					; atoms & files
	("\\<\\(atoms\\|files\\)\\>" . font-lock-function-name-face)
					; FLUO keywords
	("\\<\\(a\\(ng\\(in\\|out\\)\\|tmwei\\)\\|central\\|e\\(0\\|dge\\)\\|form\\(at\\|in\\|out\\)\\|nor\[12\]\\|out\\|pre\[12\]\\)\\>" . font-lock-keyword-face) ))


(defconst Fluo-font-lock-keywords-1 nil)
(setq Fluo-font-lock-keywords-1   Fluo-font-lock-keywords)
(defconst Fluo-font-lock-keywords-2 nil)
(setq Fluo-font-lock-keywords-2   Fluo-font-lock-keywords)

(defun Fluo-set-hilit ()
  "Call hilit-set-mode-patterns for *fluo*."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
					; titles -- type define
	    ("\\(comment\\|title\\).*$" nil define)
					; i/o data files
	    ("\\<\\(atoms\\|files\\)\\>" nil include)
					; FLUO keywords
	    ("\\<\\(a\\(ng\\(in\\|out\\)\\|tmwei\\)\\|central\\|e\\(0\\|dge\\)\\|form\\(at\\|in\\|out\\)\\|nor\[12\]\\|out\\|pre\[12\]\\)\\>" nil keyword)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Fluo-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Fluo-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Fluo-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for fluo
(defvar Fluo-mode-map ()
  "Keymap used in Fluo minor mode.")
(if Fluo-mode-map
    ()
  (setq Fluo-mode-map (make-sparse-keymap))
  (define-key Fluo-mode-map "\C-c\C-tt"  'Fluo-make-template)
  (define-key Fluo-mode-map "\C-c\C-pf"  'Fluo-plot-this-out)
  (define-key Fluo-mode-map "\C-c\C-pa"  'Fluo-plot-all-out)
  (define-key Fluo-mode-map "\C-c\C-px"  'Fluo-plot-all-xmu)
  )
(defvar Fluo-mode-menu nil)
(easy-menu-define
 Fluo-mode-menu Fluo-mode-map
 "Menu used in Fluo mode"
 '("Fluo"
   ["Make template"                 Fluo-make-template t]
   ["Clean up input file"           input-clean-file t]
   ["--------- Running --------"    input-t t]
   ["Run fluo, this file"           input-run-this-program-this-file t]
   ["Kill fluo run"                 input-kill-program
    :active (get-process input-process)]
   ["--------- Plotting -------"    input-t t]
   ["Plot all normalized files"     Fluo-plot-all-out t]
   ["Plot all xmu files"            Fluo-plot-all-xmu t]
   ["Plot this file"                Fluo-plot-this-out t]
   ["Toggle gnuplot terminal"       input-toggle-gnuplot-terminal t]
   ["--------- Et cetera ------"    input-t t]
   ["Display fluo keywords"         input-display-keywords t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Fluo-imenu-generic-expression nil
  "Imenu generic expression for Fluo mode.  See `imenu-generic-expression'.")
;;(make-variable-buffer-local 'speedbar-tag-hierarchy-method)
;;(setq speedbar-tag-hierarchy-method 'flat)


(defvar Fluo-mode nil
  "Determines if Fluo minor mode is active.")
(make-variable-buffer-local 'Fluo-mode)

;;;###autoload
(defun turn-on-fluo ()
  "Turn on Fluo minor mode."
  (Fluo-mode t))

;;;###autoload
(defun Fluo-mode (&optional arg)
  "Minor mode for editing input files for *fluo*.
ARG t turns on *fluo* minor mode.

Defined keys in Fluo minor mode:\n \\{Fluo-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Fluo-mode
	       (if (null arg) (not Fluo-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Fluo-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Fluo-mode " Fluo") minor-mode-alist)))
	 (or (assq 'Fluo-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Fluo-mode Fluo-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Fluo-mode
	     (progn
	       (make-local-variable 'imenu-generic-expression)
	       (easy-menu-add Fluo-mode-menu)
	       (setq imenu-generic-expression Fluo-imenu-generic-expression
		     input-current-keywords-alist
		     (copy-alist Fluo-keywords-alist)
		     input-output-files Fluo-output-files
		     fuse-mouse-highlight-list Fluo-highlight-list)

	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     nil
		     input-program-stanza-flag      nil
		     input-program-data-flag        nil
		     input-program-feff-flag        nil
		     input-program-list-flag        t
		     input-program-kweight-flag     nil
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Daniel Haskel" "haskel@phys.washington.edu")
		     input-program-parse
 		        '(Fluo-parse-file   Fluo-features-alist)
		     input-program-hilit            'Fluo-set-hilit)

	       (cond (input-use-hilit19 (Fluo-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Fluo-mode-hook))
	   (easy-menu-remove Fluo-mode-menu))
	 )
	(t
	 (message "Fluo minor mode is only for use with Input major mode"))
	))


;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-fluo)
(run-hooks 'Fluo-load-hook)

;;;============================================================================
;;;
;;; fuse-fluo.el end here
