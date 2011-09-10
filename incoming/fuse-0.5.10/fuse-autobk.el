;;; fuse-autobk.el --- minor mode for editing autobk input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Autobk.el,v 1.2 1998/03/14 22:45:57 bruce Exp $

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

(defcustom Autobk-mode-hook nil
  "*Hook run when Autobk minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Autobk-load-hook nil
  "*Hook run when fuse-autobk.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defconst Autobk-keywords-alist ""
"This is an associated list of all keywords for autobk and their attributes.
See the documentation for `input-current-keywords-alist'."
)


(setq Autobk-keywords-alist '(
					; I/O keywords
      ("title" .
       (0 "title" "a user-defined comment string"))
      ("comment" .
       (0 "title" "a user-defined comment string"))
      ("data" .
       (1 "readable" "the file name of the input xmu data"))
      ("xmu" .
       (1 "readable" "the file name of the input xmu data"))
      ("theory" .
       (1 "readable" "a chi.dat filename as an input standard "))
      ("standard" .
       (1 "readable" "a chi.dat filename as an input standard "))
      ("out" .
       (1 "writable" "the base of the output file name"))
      ("chi" .
       (1 "writable" "the base of the output file name"))
      ("format" .
       (1 "datatype" "an i/o file format (uwxafs or ascii)"))
      ("formin" .
       (1 "datatype" "an input file format (uwxafs or ascii)"))
      ("formout" .
       (1 "datatype" "an output file format (uwxafs or ascii)"))

					; energy keywords
      ("ee" .
       (1 "number" "a real number (an initial guess for the  edge energy)"))
      ("e0" .
       (1 "number" "a real number (an initial guess for the  edge energy)"))
      ("eef" .
       (1 "number" "a real number (the fixed edge energy)"))
      ("e0f" .
       (1 "number" "a real number (the fixed edge energy)"))
      ("fixe0" .
       (1 "logical" "a logical value (fix e0)"))
      ("thefix" .
       (1 "logical" "a logical value"))
      ("fixthe" .
       (1 "logical" "a logical value"))
      ("predg1" .
       (1 "number" "a real number (lower bound of pre-edge region)"))
      ("pre1" .
       (1 "number" "a real number (lower bound of pre-edge region)"))
      ("predg2" .
       (1 "number" "a real number (upper bound of pre-edge region)"))
      ("pre2" .
       (1 "number" "a real number (upper bound of pre-edge region)"))
      ("nnorm" .
       (1 "integer" "an integer"))
      ("nor1" .
       (1 "number" "a real number (lower bound of post-edge region)"))
      ("nor2" .
       (1 "number" "a real number (upper bound of post-edge region)"))
      ("step" .
       (1 "number" "a real number (specified edge step)"))
      ("edge" .
       (1 "number" "a real number (specified edge step)"))
      ("emin" .
       (1 "number" "a real number (lower bound of spline in energy)"))
      ("emax" .
       (1 "number" "a real number (upper bound of spline in energy)"))
      ("kmin" .
       (1 "number" "a real number (lower bound of spline in k)"))
      ("kmax" .
       (1 "number" "a real number (upper bound of spline in k)"))
      ("qmin" .
       (1 "number" "a real number (lower bound of spline in k)"))
      ("qmax" .
       (1 "number" "a real number (upper bound of spline in k)"))

					;  multi-energy keywords
      ("multi" .
       (1 "logical" "a logical value"))
      ("emmid" .
       (1 "number" "a real number"))
      ("emvar" .
       (1 "number" "a real number"))
      ("emwid" .
       (1 "number" "a real number"))
      ("emamp" .
       (1 "number" "a real number"))

					; fourier transform keywor
      ("kw" .
       (1 "number" "a real number (FT k-weight)"))
      ("qw" .
       (1 "number" "a real number (FT k-weight)"))
      ("kweight" .
       (1 "number" "a real number (FT k-weight)"))
      ("qweight" .
       (1 "number" "a real number (FT k-weight)"))
      ("dk" .
       (1 "number" "a real number (FT window sill width)"))
      ("dk1" .
       (1 "number" "a real number (FT lower window sill width)"))
      ("dk2" .
       (1 "number" "a real number (FT upper window sill width)"))
      ("dq" .
       (1 "number" "a real number (FT window sill width)"))
      ("dq1" .
       (1 "number" "a real number (FT lower window sill width)"))
      ("dq2" .
       (1 "number" "a real number (FT upper window sill width)"))
      ("hanning" .
       (1 "number" "a real number (FT window sill width)"))
      ("window" .
       (1 "window" "an FT window type"))
      ("iwindow" .
       (1 "integer" "an integer denoting FT window type"))
      ("rmax" .
       (1 "number" "a real number (end of background region)"))
      ("rbkg" .
       (1 "number" "a real number (end of background region)"))
      ("r1st" .
       (1 "number" "a real number (end of scaling region)"))
      ("rw" .
       (1 "number" "a real number"))
      ("rweight" .
       (1 "number" "a real number"))

					; fit and  normalization keywords
;;      ("rel" .
;;       (1 "number" "a real number"))
;;      ("weight" .
;;       (1 "number" "a real number"))
;;      ("bspline" .
;;       (1 "logical" "a logical value"))
      ("nknots" .
       (1 "integer" "an integer (fixed number of knots)"))
      ("norm" .
       (1 "special" "the normalization type (functional or edge step)"))

					; output keywords
      ("toler" .
       (1 "number" "a real number"))
      ("iprint" .
       (1 "integer" "an integer"))
      ("preout" .
       (1 "logical" "a logical flag (write pre-edge subtracted data)"))
      ("nrmout" .
       (1 "logical" "a logical flag (write normalized data)"))
      ("bkgout" .
       (1 "logical" "a logical flag (write background function)"))
      ("bkgxmu" .
       (1 "logical" "a logical flag (write background function)"))
      ("bkgksp" .
       (1 "logical" "a logical flag (write background in k-space)"))
      ("bkgchi" .
       (1 "logical" "a logical flag (write background in k-space)"))
      ("bkgrsp" .
       (1 "logical" "a logical flag (write background in r-space)"))
      ("theksp" .
       (1 "logical" "a logical flag (write standard in k-space)"))
      ("thechi" .
       (1 "logical" "a logical flag (write standard in k-space)"))
      ("thersp" .
       (1 "logical" "a logical flag (write standard in r-space)"))
      ("chirsp" .
       (1 "logical" "a logical flag (write data in r-space)"))
      ("datrsp" .
       (1 "logical" "a logical flag (write data in r-space)"))
      ("all" .
       (1 "logical" "a logical flag (write every possible output file)"))
      ))


(defvar Autobk-output-files)
(setq Autobk-output-files (list ".log"))

(defvar Autobk-highlight-list
  (list
   '("\\<\\(data\\|standard\\|theory\\|xmu\\)[ \t]*[ \t=,][ \t]*\\sw+\\>" 0)
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template for autobk.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Autobk-template
  '("title = " p n
    "data = " input-data-path p n
    "theory = " input-feff-path p n
    "out = " input-out-path p n
    "nrmout = true\tbkgxmu = true" n
    "e0 = " p n
    "kmin = " p "\tkmax = " p " \tkw = " p n
    "rbkg = " p "\tr1st = " p n
    input-stanza-delimiter n)
  "Template for an *autobk* stanza.  Inserted by \\[Autobk-make-template].")

(tempo-define-template "Autobk-template" Autobk-template
		       nil
		       "Insert an empty Autobk template.")
(eval-when-compile  (defun tempo-template-Autobk-template))

(defun Autobk-make-template ()
  "Write a template for *autobk* using a tempo template.
Bound to \\[Autobk-make-template]."
  (interactive)
  (tempo-template-Autobk-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;; input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))

;;; end template for autobk.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special functions for autobk

(defvar Autobk-features-alist nil
  "Alist containing information parsed from an autobk stanza.")


(defun Autobk-which-stanza ()
  "Return the stanza number of the current one.
The first stanza returns 1 and so on.  The returned value is an integer."
  (save-excursion
    (let ((count 1))
      (while (not (input-first-stanza-p))
	(setq count (1+ count))
	(input-previous-stanza 1) )
      count )))

(defun Autobk-fetch-ezero ()
  "Return the value of e0 for this stanza from the log file."
  (let* ((which (Autobk-which-stanza)) mark-1 value
	 (basename (file-name-sans-extension
		    (or input-master (buffer-file-name))))
	 (logname (concat basename ".log")))
    (cond ((file-exists-p logname)
	   (input-jump-to-log-file)
					; separator in autobk log file
					; is two lines of dashes (grrr!!)
	   (cond ((or
		   (= which 1)
		   (re-search-forward "\\s-*-+\\s-+-+\\s-" (point-max)
				      "move" (- which 1)))

					; skip past titles
		  (re-search-forward "^[ \t]*-+[ \t]*$" (point-max) t 2)
		  (cond ((or
			  (re-search-forward "^[ \t]*e0" (point-max) t)
			  (re-search-forward "^[ \t]*final value of e0"
					     (point-max) t) )
			 (end-of-line)
			 (setq mark-1 (point-marker))
			 (re-search-backward "[ \t]")
			 (forward-char 1)
			 (setq value
			       (buffer-substring-no-properties (point) mark-1))
			 (setq value (string-to-number value)))
			(t
			 (message "Could not find e0 in %s"
				  (file-name-nondirectory buffer-file-name))
			 )))
		 (t
		  (message "Could not find e0 in %s"
			   (file-name-nondirectory buffer-file-name))))

	   (kill-buffer (current-buffer))
	   (message nil) )
	  (t
	   (message "%s.log does not exist.  Have you run %s?"
		    basename input-program-name)))
    value
    ))


(defun Autobk-insert-ezero ()
  "Fetch the value of e0 for the current stanza.
Find it in the *autobk* log file and insert it into the current stanza.
Bound to \\[Autobk-insert-ezero]"
  (interactive)
  (let (limit mark-1 (found nil) bos (value (Autobk-fetch-ezero)))
    (save-excursion
      (input-end-of-stanza)
      (if value
	  (progn
	    (setq bos
		  (save-excursion (input-beginning-of-stanza) (point-marker)))
	    (while (not found)
	      (cond ((re-search-backward "\\<e\\(0\\|e\\)f?[ \t]*=?"
					 bos "to_limit")
		     (cond ((not (or (input-title-p) (input-comment-p)))
			    (setq limit (save-excursion (end-of-line) (point)))
			    (if (re-search-forward "\\s-\\<" limit "move")
				(progn
				  (setq mark-1 (point-marker))
				  (re-search-forward "\\s-\\<" limit "move")
				  (delete-region mark-1 (point))))
			    (delete-horizontal-space)
			    (insert (format " %9.3f\t" value))
			    (setq found t))))
		    (t
		     (input-end-of-stanza)
		     (beginning-of-line)
		     (setq mark-1 (point-marker))
		     (insert (format "e0f = %9.3f\n" value))
		     (input-repaint-command mark-1 (point))
		     (setq found t))
		    ))) ))))


(defun Autobk-parse-stanza ()
  "Parse current stanza for information for plotting *autobk* results."
  (interactive)
  (save-excursion
    (catch 'quit-parse
      (let (datafile theory format formin formout (bkgxmu t) bkgxmuval
		     title inbase outbase bkgout kspout mark-1 eos
		     (case-fold-search t))
	(setq Autobk-features-alist nil)
	(setq eos (save-excursion (input-end-of-stanza) (point-marker)))

					; find datafile name
	;;(goto-char eos)
	(setq datafile (or (input-find-word-this-stanza "data")
			   (input-find-word-this-stanza "xmu" )))
	(if datafile ()
	  (message "No data file name was found")
	  (throw 'quit-parse nil))

					; find theory file name
	(setq theory (or (input-find-word-this-stanza "theory")
			 (input-find-word-this-stanza "standard")))

					; find outfile name base
					; and construct ksp and bkg file names
	(setq outbase (or (input-find-word-this-stanza "out")
			  (input-find-word-this-stanza "chi")
			  datafile))
	(setq outbase (file-name-sans-extension outbase))
	(cond ((file-directory-p outbase)
	       (setq inbase (file-name-nondirectory   datafile))
	       (setq inbase (file-name-sans-extension inbase))
	       (setq outbase (concat outbase inbase))))

	(setq bkgout (concat outbase "e.bkg"))
	(setq bkgout (file-relative-name bkgout "./"))
	(setq kspout (concat outbase "k.chi"))
	(setq kspout (file-relative-name kspout "./"))

					; determine if bkgxmu=false
	(setq bkgxmuval (input-find-word-this-stanza "bkgxmu"))
	(if bkgxmuval (downcase (input-this-word)))
	(if (or (string= "f" (substring bkgxmuval 0 1))
		(string= "n" (substring bkgxmuval 0 1)))
	    (setq bkgxmu nil))

					; get first title
	(save-excursion
	  (input-beginning-of-stanza)
	  (cond ((re-search-forward "\\<title\\>" eos "to_limit")
		 (re-search-forward input-word-sep)
		 (setq mark-1 (point-marker))
		 (end-of-line)
		 (setq title (buffer-substring-no-properties mark-1 (point))) )))

					; get format/in/out
	(setq format (input-find-word-this-stanza "format"))
	(if (and format  (string-match "uw" format))  (setq format  "uwxafs"))
	(setq format (input-find-word-this-stanza "formin"))
	(if (and formin  (string-match "uw" formin))  (setq formin  "uwxafs"))
	(setq format (input-find-word-this-stanza "formout"))
	(if (and formout (string-match "uw" formout)) (setq formout  "uwxafs"))
	(if (and (not formin) (string= format "uwxafs"))
	    (setq formin "uwxafs")
	  (setq formin "ascii"))
	(if (and (not formout) (string= format "uwxafs"))
	    (setq formout "uwxafs")
	  (setq formout "ascii"))

					; make alist
	(setq Autobk-features-alist
	      (append
	       (list (cons "datafile" datafile))
	       (list (cons "theory"   theory))
	       (list (cons "bkgout"   bkgout))
	       (list (cons "kspout"   kspout))
	       (list (cons "bkgxmu"   bkgxmu))
	       (list (cons "title"    title))
	       (list (cons "formin"   formin))
	       (list (cons "formout"  formout))))

	(if (interactive-p)
	    (message "Parsed %s stanza in file: %s"
		     input-program-name
		     (file-name-nondirectory buffer-file-name)))
	))))


(defun Autobk-fetch-ymin-ymax (&optional file)
  "Use the awk script *minmax* to get the ordinate range for a plot.
This is used to make the arrow showing e0 in background plots.  If FILE is
not given, stanza will parsed for data file name."
  ;;(interactive)
  (let (datafile (list () ) ymin ymax pad scratch)
    (if file
	(setq datafile file)
      (Autobk-parse-stanza)
      (setq datafile (cdr (assoc "datafile" Autobk-features-alist))))
    (cond ((file-exists-p datafile)
	   (setq scratch (make-temp-name "FUSE-scratch"))
	   (setq scratch (generate-new-buffer scratch))

	   (call-process "minmax" datafile scratch t)
 	   (switch-to-buffer scratch)
 	   (goto-char (point-min))
 	   (delete-horizontal-space)
 					; ymin
 	   (re-search-forward "\\s-")
 	   (forward-char -1)
 	   (setq ymin (string-to-number
		       (buffer-substring-no-properties (point-min) (point))))
 					; ymax
 	   (delete-region (point-min) (point))
 	   (delete-horizontal-space)
 	   (end-of-line)
 	   (setq ymax (string-to-number
		       (buffer-substring-no-properties (point-min) (point))))

 	   (setq pad (* .05 (- ymax ymin)))
	   (setq ymin (- ymin pad)
		 ymax (+ ymax pad)
		 list (list ymin ymax))
 	   (kill-buffer (current-buffer)) ))

     ;;(message "ymin,ymax %S" list)
     list
     ))



(defun Autobk-run-stanza ()
  "Run *autobk* on the current stanza.  Bound to \\[Autobk-run-stanza]."
  (interactive)
  (save-excursion
    (let (top bottom stanza (filename input-stanza-name))
      (input-beginning-of-stanza)
      (setq top (point-marker))
      (input-end-of-stanza)
      (forward-line 1)
      (setq bottom (point-marker))
      (setq stanza (buffer-substring-no-properties top bottom))
      (if (file-exists-p filename) (delete-file filename))
      (setq input-stanza-program "autobk")
      (find-file filename)
      (insert stanza)
      (save-buffer)
      (kill-buffer filename)
      (rename-file filename filename t)
      (input-run-program (list "autobk" filename ""))
      )))



;;; end of special functions for autobk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions

;;    (setq my-expr (make-regexp '("format" "formin" "formout" "e0" "ee"
;;				 "e0f" "eef" "fixe0" "thefix" "fixthe"
;;				 "pre1" "pre2" "predg1" "predge2"
;;				 "nnorm" "nor1" "nor2" "step" "edge"
;;				 "emin" "kmin" "qmin" "emax" "kmax"
;;				 "qmax" "multi" "emmid" "emvar" "emwid"
;;				 "emamp" "rw" "qw" "kw" "weight"
;;				 "kweight" "qweight" "rweight" "dk"
;;				 "dq" "dk1" "dk2" "dq1" "dq2"
;;				 "hanning" "iwindow" "window" "rmax"
;;				 "rbkg" "r1st" "nknots" "norm"
;;				 "toler" "iprint" "preout" "nrmout"
;;				 "bkgout" "bkgxmu" "bkgksp" "bkgchi"
;;				 "bkgrsp" "theksp" "thechi" "thersp"
;;				 "chirsp" "datrsp" "all" )))



(defconst Autobk-font-lock-keywords
      '(
					; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%!#].*$" . font-lock-comment-face)
					; titles
	("\\<\\(comment\\|title\\).*$" . font-lock-string-face)
					; atom, basis, end
	("\\<\\(chi\\|data\\|out\\|standard\\|theory\\|xmu\\)\\>" .
	 font-lock-function-name-face)
					; AUTOBK keywords
	("\\<\\(all\\|bkg\\(chi\\|ksp\\|out\\|rsp\\|xmu\\)\\|chirsp\\|d\\([kq]\\|atrsp\\|k[12]\\|q[12]\\)\\|e\\([0e]\\|0f\\|dge\\|ef\\|m\\(a\\(mp\\|x\\)\\|in\\|mid\\|var\\|wid\\)\\)\\|f\\(ix\\(e0\\|the\\)\\|orm\\(at\\|in\\|out\\)\\)\\|hanning\\|i\\(print\\|window\\)\\|k\\(m\\(ax\\|in\\)\\|w\\(\\|eight\\)\\)\\|multi\\|n\\(knots\\|norm\\|or[12m]\\|rmout\\)\\|pre\\([12]\\|dg\\(1\\|e2\\)\\|out\\)\\|q\\(m\\(ax\\|in\\)\\|w\\(\\|eight\\)\\)\\|r\\(1st\\|bkg\\|max\\|w\\(\\|eight\\)\\)\\|step\\|t\\(he\\(chi\\|fix\\|ksp\\|rsp\\)\\|oler\\)\\|w\\(eight\\|indow\\)\\)\\>" . font-lock-keyword-face) ))


(defconst Autobk-font-lock-keywords-1 nil)
(setq Autobk-font-lock-keywords-1   Autobk-font-lock-keywords)
(defconst Autobk-font-lock-keywords-2 nil)
(setq Autobk-font-lock-keywords-2   Autobk-font-lock-keywords)

(defun Autobk-set-hilit ()
  "Call hilit-set-mode-patterns for *autobk*."
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
	    ("\\<\\(chi\\|data\\|out\\|standard\\|theory\\|xmu\\)\\>" nil include)
					; AUTOBK keywords
	    ("\\<\\(all\\|bkg\\(chi\\|ksp\\|out\\|rsp\\|xmu\\)\\|chirsp\\|d\\([kq]\\|atrsp\\|k[12]\\|q[12]\\)\\|e\\([0e]\\|0f\\|dge\\|ef\\|m\\(a\\(mp\\|x\\)\\|in\\|mid\\|var\\|wid\\)\\)\\|f\\(ix\\(e0\\|the\\)\\|orm\\(at\\|in\\|out\\)\\)\\|hanning\\|i\\(print\\|window\\)\\|k\\(m\\(ax\\|in\\)\\|w\\(\\|eight\\)\\)\\|multi\\|n\\(knots\\|norm\\|or[12m]\\|rmout\\)\\|pre\\([12]\\|dg\\(1\\|e2\\)\\|out\\)\\|q\\(m\\(ax\\|in\\)\\|w\\(\\|eight\\)\\)\\|r\\(1st\\|bkg\\|max\\|w\\(\\|eight\\)\\)\\|step\\|t\\(he\\(chi\\|fix\\|ksp\\|rsp\\)\\|oler\\)\\|w\\(eight\\|indow\\)\\)\\>" nil keyword)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Autobk-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Autobk-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Autobk-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for autobk
(defvar Autobk-mode-map ()
  "Keymap used in Autobk minor mode.")
(if Autobk-mode-map
    ()
  (setq Autobk-mode-map (make-sparse-keymap))
  (define-key Autobk-mode-map "\C-c\C-tt"  'Autobk-make-template)
					; movement
  (define-key Autobk-mode-map "\C-c\C-on"  'input-next-stanza)
  (define-key Autobk-mode-map "\C-c\C-op"  'input-previous-stanza)
  (define-key Autobk-mode-map "\C-c\C-om"  'input-mark-stanza)
  (define-key Autobk-mode-map "\C-c\C-ok"  'input-kill-stanza)
					; snagging
  (define-key Autobk-mode-map "\C-c\C-sp"  'input-snag-from-previous-stanza)
  (define-key Autobk-mode-map "\C-c\C-sn"  'input-snag-from-next-stanza)
  (define-key Autobk-mode-map "\C-c\C-se"  'Autobk-insert-ezero)
					; running
  (define-key Autobk-mode-map "\C-c\C-rs"  'Autobk-run-stanza)
					; plotting
  (define-key Autobk-mode-map "\C-c\C-pb"  'Autobk-plot-bkg)
  (define-key Autobk-mode-map "\C-c\C-pk"  'Autobk-plot-ksp)
  (define-key Autobk-mode-map "\C-c\C-pt"  'Autobk-plot-thy)
  (define-key Autobk-mode-map "\C-c\C-pa"  'Autobk-plot-all-chi)
  )
(defvar Autobk-mode-menu nil)
(easy-menu-define
 Autobk-mode-menu Autobk-mode-map
 "Menu used in Autobk mode"
 '("Autobk"
   ["--------- Templates -------------" input-t t]
   ["Make template"                     Autobk-make-template t]
   ["Clean up autobk input file"        input-clean-file t]
   ["--------- Editing Shortcuts -----" input-t t]
   ("Move/Mark/Kill"
    ["Next stanza"                      input-next-stanza t]
    ["Previous stanza"                  input-previous-stanza t]
    ["Mark this stanza"                 input-mark-stanza t]
    ["Kill this stanza"                 input-kill-stanza t])
   ["Snag from previous stanza"         input-snag-from-previous-stanza t]
   ["Snag from next stanza"             input-snag-from-next-stanza t]
   ["Insert e0 value"                   Autobk-insert-ezero t]
   ;; :active if log file exists
   ["--------- Running ---------------" input-t t]
   ["Run autobk, master file"           input-run-this-program-this-file t]
   ["Run autobk, this stanza"           Autobk-run-stanza t]
   ["Kill autobk run"                   input-kill-program
    :active (get-process input-process)]
   ["--------- Plotting --------------" input-t t]
   ["Plot background"                   Autobk-plot-bkg t]
   ["Plot chi(k)"                       Autobk-plot-ksp t]
   ["Plot all chi(k) in file"           Autobk-plot-all-chi t]
   ["Plot chi(k) with standard"         Autobk-plot-thy t]
   ["Toggle gnuplot terminal"           input-toggle-gnuplot-terminal t]
   ["Set k-weight"                      input-set-k-weight t]
   ["--------- Et cetera -------------" input-t t]
   ["Look at log file"                  input-jump-to-log-file
    :active (input-log-file-exists-p)]
   ["Display autobk keywords"           input-display-keywords t]
   ;;["Reparse stanza"                    Autobk-parse-stanza t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Autobk-imenu-generic-expression
  '((nil "data[ \t]*[ \t=,][ \t]*\\(\\w+\\)"     1))
  "Imenu generic expression for Autobk mode.  See `imenu-generic-expression'.")
;;(make-variable-buffer-local 'speedbar-tag-hierarchy-method)
;;(setq speedbar-tag-hierarchy-method 'flat)


(defvar Autobk-mode nil
  "Determines if Autobk minor mode is active.")
(make-variable-buffer-local 'Autobk-mode)

;;;###autoload
(defun turn-on-autobk ()
  "Turn on Autobk minor mode."
  (Autobk-mode t))

;;;###autoload
(defun Autobk-mode (&optional arg)
  "Minor mode for editing input files for *autobk*.
ARG t turns on *autobk* minor mode.

Defined keys in Autobk minor mode:\n \\{Autobk-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Autobk-mode
	       (if (null arg) (not Autobk-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Autobk-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Autobk-mode " Autobk") minor-mode-alist)))
	 (or (assq 'Autobk-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Autobk-mode Autobk-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Autobk-mode
	     (progn
	       (make-local-variable 'imenu-generic-expression)
	       (easy-menu-add Autobk-mode-menu)
	       (setq imenu-generic-expression Autobk-imenu-generic-expression
		     input-current-keywords-alist
		     (copy-alist Autobk-keywords-alist)
		     input-output-files Autobk-output-files
		     fuse-mouse-highlight-list Autobk-highlight-list)

	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      t
		     input-program-logfile-flag     t
		     input-program-stanza-flag      t
		     input-program-data-flag        t
		     input-program-feff-flag        t
		     input-program-list-flag        nil
		     input-program-kweight-flag     t
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Matthew Newville" "newville@cars.uchicago.edu")
		     input-program-parse
 		        '(Autobk-parse-stanza   Autobk-features-alist)
		     input-program-hilit            'Autobk-set-hilit)

	       (cond (input-use-hilit19 (Autobk-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Autobk-mode-hook))
	   (easy-menu-remove Autobk-mode-menu))
	 )
	(t
	 (message "Autobk minor mode is only for use with Input major mode"))
	))


;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-autobk)
(run-hooks 'Autobk-load-hook)

;;;============================================================================
;;;
;;; fuse-autobk.el end here
