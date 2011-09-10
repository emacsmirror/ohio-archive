;;; fuse-feffit.el --- minor mode for editing feffit input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Feffit.el,v 1.2 1998/03/14 22:45:57 bruce Exp $

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
(eval-when-compile
  (defvar Feff-features-alist ())
  (defun Feff-parse-file))

(defcustom Feffit-mode-hook nil
  "*Hook run when Feffit minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Feffit-load-hook nil
  "*Hook run when fuse-feffit.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defvar Feffit-features-alist nil
  "Alist containing information parsed from an feffit data set.")

(defvar Feffit-keywords-alist nil
  "This is the feffit associated list of keywords and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Feffit-keywords-alist '(
					; control keywords
      ("next data set" .
       (0 "none" "no argument.  This marks the end of a dat set."))
      ("title" .
       (0 "title" "a user-defined comment string"))
      ("end" .
       (0 "none" "no argument.  Nothing after \"end\" is read by feffit."))
      ("quit" .
       (0 "none" "no argument.  Quit reading input file in interactive mode."))
      ("include" .
       (1 "readable" "a filename for an include file"))
					; i/o keywords
      ("format" .
       (1 "datatype" "an i/o file format (uwxafs/ascii)"))
      ("formin" .
       (1 "datatype" "an input file format (uwxafs/ascii)"))
      ("formout" .
       (1 "datatype" "an output file format (uwxafs/ascii)"))
      ("comment" .
       (1 "string" "a comment char for ascii column data files"))
      ("mdocxx" . ;;** symbol
       (1 "integer" "an integer (maximum number of output documentation lines)"))
      ("data" .
       (1 "readable" "a readable file (chi(k) data)"))
      ("bkgfile" .
       (1 "readable" "a readable file (bkg(k) data)"))
      ("out" .
       (1 "writable" "the base name for output files"))
      ("iprint" .
       (1 "integer" "an integer (print level to feffit.run file)"))
      ("tranquada" .
       (1 "logical" "a logical flag (tranquada correction)"))
      ("norun" .
       (1 "logical" "a logical flag (doing fit and writing output)"))
      ("noout" .
       (1 "logical" "a logical flag (write output)"))
      ("nofit" .
       (1 "logical" "a logical flag (doing fit)"))
      ("prmout" .
       (1 "logical" "a logical flag (write feffit.prm file)"))
      ("pcout" .
       (1 "logical" "a logical flag (write phase corrected FT)"))
      ("pcfit" .
       (1 "logical" "a logical flag (fiting with phase corrected FT)"))
      ("bkg" .
       (1 "logical" "a logical flag (fiting background spline)"))
      ("bkgout" .
       (1 "logical" "a logical flag (write backg spline to a file)"))
      ("all" .
       (1 "logical" "a logical flag (write all paths)"))
      ("allout" .
       (1 "logical" "a logical flag (write all paths)"))
      ("kfull" .
       (1 "logical" "a logical flag (write data in k space)"))
      ("fullk" .
       (1 "logical" "a logical flag (write data in k space)"))
      ("kspout" .
       (1 "logical" "a logical flag (write data in k space)"))
      ("rspout" .
       (1 "logical" "a logical flag (write data in R space)"))
      ("qspout" .
       (1 "logical" "a logical flag (write backtransform data)"))
      ("envout" .
       (1 "logical" "a logical flag (write backtransform data)"))
      ("kspfit" .
       (1 "logical" "a logical flag (fit data in k space)"))
      ("rspfit" .
       (1 "logical" "a logical flag (fit data in R space)"))
      ("qspfit" .
       (1 "logical" "a logical flag (fit backtransform data)"))
      ("degen" .
       (1 "logical" "a logical flag (ignore path degeneracy)"))
      ("nodegen" .
       (1 "logical" "a logical flag (ignore path degeneracy)"))
      ("weight" .
       (1 "number" "a real number (relative weights for data sets)"))
					; error bar keywords
      ("sigdat" .
       (1 "number" "a real number (meas. uncertainty in k space)"))
      ("epsilon" .
       (1 "number" "a real number (meas. uncertainty in k space)"))
      ("sigk" .
       (1 "number" "a real number (meas. uncertainty in k space)"))
      ("epsk" .
       (1 "number" "a real number (meas. uncertainty in k space)"))
      ("sigr" .
       (1 "number" "a real number (meas. uncertainty in R space)"))
      ("epsr" .
       (1 "number" "a real number (meas. uncertainty in R space)"))
      ("cormin" .
       (1 "number" "a real number > 0 and < 1 (maximum reported correlation)"))
					; fit and FT control
      ("toler" .
       (1 "number" "a real number (fitting tolerance)"))
      ("rlast" .
       (1 "number" "a real number (max. R in output R space data)"))
      ("mftfit" .
       (1 "integer" "an integer (number of points in fft for fit)"))
      ("mftwrt" .
       (1 "integer" "an integer (number of points in fft for writing data)"))
      ("rmin" .
       (1 "number" "a real number (lower bound of fit in R)"))
      ("rmax" .
       (1 "number" "a real number (upper bound of fit in R)"))
      ("kmin" .
       (1 "number" "a real number (lower bound of FT)"))
      ("kmax" .
       (1 "number" "a real number (upper bound of FT)"))
      ("qmin" .
       (1 "number" "a real number (lower bound of FT)"))
      ("qmax" .
       (1 "number" "a real number (upper bound of FT)"))
      ("kweight" .
       (1 "number" "a real number (k weight for fourier transform)"))
      ("qweight" .
       (1 "number" "a real number (k weight for fourier transform)"))
      ("kw" .
       (1 "number" "a real number (k weight for fourier transform)"))
      ("qw" .
       (1 "number" "a real number (k weight for fourier transform)"))
      ("w" .
       (1 "number" "a real number (k weight for fourier transform)"))
      ("iwindow" .
       (1 "integer" "an integer (window type index)"))
      ("ikwindow" .
       (1 "integer" "an integer (window type index, k->R FT)"))
      ("irwindow" .
       (1 "integer" "an integer (window type index, R->q FT)"))
      ("dk" .
       (1 "number" "a number (k window sill width)"))
      ("dq" .
       (1 "number" "a number (k window sill width)"))
      ("dk1" .
       (1 "number" "a number (k window sill width)"))
      ("dk2" .
       (1 "number" "a number (k window sill width)"))
      ("dq1" .
       (1 "number" "a number (k window sill width)"))
      ("dq2" .
       (1 "number" "a number (k window sill width)"))
      ("dr" .
       (1 "number" "a number (R window sill width)"))
      ("dr1" .
       (1 "number" "a number (R window sill width)"))
      ("dr2" .
       (1 "number" "a number (R window sill width)"))
					; set guess local
      ("set" .
       (1 "setguess" "a set parameter name and value"))
      ("guess" .
       (1 "setguess" "a guess parameter name and value"))
      ("local" .
       (1 "setguess" "a local parameter name and value"))
      ))

(defvar Feffit-pathparams-alist 'nil
  "This is the feffit associated list of path parameters and their attributes.
This is appended to Feffit-keywords-alist.  See the documentation for
Feffit-keywords-alist and input-current-keywords-alist.")
(setq Feffit-pathparams-alist '(
					; path parameetrs
      ("path" .
       (1 "pathparam" "a path index and a readable filename"))
      ("feff" .
       (1 "pathparam" "a path index and a readable filename"))
      ("id" .
       (1 "pathparam" "a path index and a comment string"))
      ("s02" .
       (1 "pathparam" "a path index and an amplitude expression"))
      ("so2" .
       (1 "pathparam" "a path index and an amplitude expression"))
      ("amp" .
       (1 "pathparam" "a path index and an amplitude expression"))
      ("eshift" .
       (1 "pathparam" "a path index and an e0 expression"))
      ("e0" .
       (1 "pathparam" "a path index and an e0 expression"))
      ("e0shift" .
       (1 "pathparam" "a path index and an e0 expression"))
      ("ee" .
       (1 "pathparam" "a path index and an e0 expression"))
      ("ei" .
       (1 "pathparam" "a path index and an imaginary energy expression"))
      ("dphase" .
       (1 "pathparam" "a path index and a constant phase expression"))
      ("phase" .
       (1 "pathparam" "a path index and a constant phase expression"))
      ("deltar" .
       (1 "pathparam" "a path index and a delta_R expression"))
      ("delr" .
       (1 "pathparam" "a path index and a delta_R expression"))
      ("sigma2" .
       (1 "pathparam" "a path index and a sigma^2 expression"))
      ("ss2" .
       (1 "pathparam" "a path index and a sigma^2 expression"))
      ("dwf" .
       (1 "pathparam" "a path index and a sigma^2 expression"))
      ("3rd" .
       (1 "pathparam" "a path index and a third cumulant expression"))
      ("third" .
       (1 "pathparam" "a path index and a third cumulant expression"))
      ("cubic" .
       (1 "pathparam" "a path index and a third cumulant expression"))
      ("4th" .
       (1 "pathparam" "a path index and a fourth cumulant expression"))
      ("fourth" .
       (1 "pathparam" "a path index and a fourth cumulant expression"))
      ("quartic" .
       (1 "pathparam" "a path index and a fourth cumulant expression"))
      ))

(setq Feffit-keywords-alist (append Feffit-keywords-alist
				    Feffit-pathparams-alist))

;  these keywords were commented out in source code
; abort help prdump r2m2 riflag err precision nerstp dafs gauss hanning
; fhanning rpha qpha cosqr sinqr

(defvar Feffit-output-files (list ".log" ".prm"))

(defvar Feffit-highlight-list
  (list
   '("\\<\\(\\(bkgfile\\|data\\|include\\)[ \t]*[ \t=,][ \t]*\\sw+\\)\\>" 0)
   '("^[ \t]*\\(\\(path\\|feff\\)[ \t]+[1-9][0-9]*[^%!#\n]+\\)" 0)
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   ))

;;(regexp-quote "<")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hilit19 regular expressions

;;    (setq my-expr (make-regexp '("all" "allout" "bkg" "bkgout"
;;				 "comment" "cormin" "degen" "data" "dk" "dk1"
;;				 "dk2" "dq" "dq1" "dq2" "dr" "dr1"
;;				 "dr2" "envout" "epsilon" "epskr"
;;				 "espr" "format" "formin" "formout"
;;				 "fullk" "iwindow" "irwindow"
;;				 "ikwindow" "iprint" "kfull" "kspfit"
;;				 "qspfit" "rspfit" "kspout" "qspout"
;;				 "rspout" "w" "kw" "qw" "weight"
;;				 "kweight" "qweight" "mdocxx" "mftfit"
;;				 "mftwrt" "nodegen" "nofit" "noout"
;;				 "norun" "pcfit" "pcout" "prmout"
;;				 "kmax" "qmax" "rmax" "kmin" "qmin"
;;				 "rmin" "rlast" "sigdat" "sigr" "sihk"
;;				 "toler" "tranquada" )))



(defconst Feffit-font-lock-keywords
      '(
					; comments
	("^[ \t]*\\*.*$" . font-lock-comment-face)
	("[%#!].*" . font-lock-comment-face)
					; titles
	("\\<title.*$" . font-lock-string-face)
					; FEFFIT keywords
	("\\<\\(all\\(\\|out\\)\\|bkg\\(\\|out\\)\\|co\\(mment\\|rmin\\)\\|d\\([kqr]\\|egen\\|k[12]\\|q[12]\\|r[12]\\)\\|e\\(nvout\\|ps\\(ilon\\|kr\\)\\|spr\\)\\|f\\(orm\\(at\\|in\\|out\\)\\|ullk\\)\\|i\\(kwindow\\|print\\|rwindow\\|window\\)\\|k\\(full\\|m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\|w\\(\\|eight\\)\\)\\|m\\(docxx\\|ft\\(fit\\|wrt\\)\\)\\|no\\(degen\\|fit\\|out\\|run\\)\\|p\\(c\\(fit\\|out\\)\\|rmout\\)\\|q\\(m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\|w\\(\\|eight\\)\\)\\|r\\(last\\|m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\)\\|si\\(g\\(dat\\|r\\)\\|hk\\)\\|t\\(oler\\|ranquada\\)\\|w\\(\\|eight\\)\\)\\>" . font-lock-keyword-face)

	    ("\\<\\(bkgfile\\|data\\|end\\|guess\\|include\\|\\(next data set\\)\\|out\\|quit\\|reff\\)\\>" .
	 font-lock-function-name-face)
	    ("^[ \t]*\\(guess\\|local\\|set\\)\\>" . font-lock-function-name-face)

	("^[ \t]*\\(path\\|feff\\|id\\|s[0os]2\\|amp\\|e[0ei]\\|e0?shift\\|d?phase\\|del\\(ta\\)?r\\|sigma2\\|dwf\\|3rd\\|third\\|cubic\\|4th\\|fourth\\|quartic\\)\\>" . font-lock-type-face) ))

(defconst Feffit-font-lock-keywords-1 nil)
(setq Feffit-font-lock-keywords-1   Feffit-font-lock-keywords)
(defconst Feffit-font-lock-keywords-2 nil)
(setq Feffit-font-lock-keywords-2   Feffit-font-lock-keywords)

(defun Feffit-set-hilit ()
  "Set patterns for hilit19 for *feffit* input files."
  (interactive)
  (cond (input-use-hilit19
	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
					; titles -- type define
	    ("\\<title.*$" nil define)
					; feff keywords
	    ("\\<\\(all\\(\\|out\\)\\|bkg\\(\\|out\\)\\|co\\(mment\\|rmin\\)\\|d\\([kqr]\\|egen\\|k[12]\\|q[12]\\|r[12]\\)\\|e\\(nvout\\|ps\\(ilon\\|kr\\)\\|spr\\)\\|f\\(orm\\(at\\|in\\|out\\)\\|ullk\\)\\|i\\(kwindow\\|print\\|rwindow\\|window\\)\\|k\\(full\\|m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\|w\\(\\|eight\\)\\)\\|m\\(docxx\\|ft\\(fit\\|wrt\\)\\)\\|no\\(degen\\|fit\\|out\\|run\\)\\|p\\(c\\(fit\\|out\\)\\|rmout\\)\\|q\\(m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\|w\\(\\|eight\\)\\)\\|r\\(last\\|m\\(ax\\|in\\)\\|sp\\(fit\\|out\\)\\)\\|si\\(g\\(dat\\|r\\)\\|hk\\)\\|t\\(oler\\|ranquada\\)\\|w\\(\\|eight\\)\\)\\>" nil keyword)
					; i/o and flow control
	    ("\\<\\(bkgfile\\|data\\|end\\|guess\\|include\\|\\(next data set\\)\\|out\\|quit\\|reff\\)\\>" nil include)
	    ("^[ \t]*\\(guess\\|local\\|set\\)\\>" nil include)
					; path parameters
	    ("^[ \t]*\\(path\\|feff\\|id\\|s[0os]2\\|amp\\|e[0ei]\\|e0?shift\\|d?phase\\|del\\(ta\\)?r\\|sigma2\\|dwf\\|3rd\\|third\\|cubic\\|4th\\|fourth\\|quartic\\)\\>" nil glob-struct)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Feffit-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Feffit-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Feffit-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar Feffit-path-index    0)
(make-variable-buffer-local 'Feffit-path-index)
(defvar input-begin-data-set "^[ \t]*next data set" )
(defvar input-end-data-set   "^[ \t]*\\(\\<end\\>\\|\\(next data set\\)\\)" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Templates for feffit.inp
;;;  presently supported: global header, local header,
;;;                       path paragraph, zeroth path

					; Template for global header
(defvar Feffit-ghead-template
  '("formin = " p "\tformout = " p n
    "rspout = " p "yes\tqspout = " p "yes\tkspout = " p "yes" n
    "all = " p "no" n
    input-comment-delimiter n n)
  "Template for an *feffit* global header.
Inserted by  \\[Feffit-make-ghead-template].")

(tempo-define-template "Feffit-ghead-template" Feffit-ghead-template
		       nil
		       "Insert an empty Feffit global header template.")
(eval-when-compile (defun tempo-template-Feffit-ghead-template))

(defun Feffit-make-ghead-template ()
  "Write a template for a *feffit* global header using a tempo template.
Bound to \\[Feff-make-ghead-template]."
  (interactive)
  (tempo-template-Feffit-ghead-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;;input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))


					; Template for local header
(defvar Feffit-lhead-template
  '("title = " p n
    "data = " input-data-path p n
    "out = "  input-out-path  p n
    "kmin = " p "\tkmax = " p "\tdk = " p "\t\kw = " p n
    "rmin = " p "\trmax = " p n n
    input-comment-delimiter n n)
  "Template for an *feffit* local header.
Inserted by  \\[Feffit-make-lhead-template].")

(tempo-define-template "Feffit-lhead-template" Feffit-lhead-template
		       nil
		       "Insert an empty Feffit local header template.")
(eval-when-compile (defun tempo-template-Feffit-lhead-template))

(defun Feffit-make-lhead-template ()
  "Write a template for a *feffit* local header using a tempo template.
Bound to \\[Feff-make-lhead-template]."
  (interactive)
  (tempo-template-Feffit-lhead-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;;input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))


(defun Feffit-set-path-index (index)
  "Reset default path INDEX with number prompted from minibuffer.
Bound to \\[Feffit-set-path-index]"
  (interactive "*nSet path index to > ")
  (setq Feffit-path-index (1- (abs (truncate index)))))

					; Template for path paragraph
(defvar Feffit-path-template
  '("path 1 " input-feff-path p n
    "id   1 " p n n)
  "Template for an *feffit* path paragraph.
Inserted by  \\[Feffit-make-path-paragraph].")

(tempo-define-template "Feffit-path-template" Feffit-path-template
		       nil
		       "Insert an empty Feffit path paragraph template.")
(eval-when-compile (defun tempo-template-Feffit-path-template))

(defun Feffit-make-path-paragraph ()
  "Write a template for a *feffit* path paragraph using a tempo template.
Bound to \\[Feffit-make-path-paragraph]."
  (interactive)
  (tempo-template-Feffit-path-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (setq Feffit-path-index (1+ Feffit-path-index))
  (Feffit-renumber-paragraph Feffit-path-index)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;;input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))

					; Template for zeroth path paragraph
(defvar Feffit-zeroth-template
  '("s02\t0\t" p n
    "e0\t0\t"  p n n)
  "Template for an *feffit* zeroth path paragraph.
Inserted by  \\[Feffit-zeroth-path-paragraph].")

(tempo-define-template "Feffit-zeroth-template" Feffit-zeroth-template
		       nil
		       "Insert an empty Feffit zeroth path paragraph template.")
(eval-when-compile (defun tempo-template-Feffit-zeroth-template))

;; This is not properly `cleaned'
(defun Feffit-zeroth-path-paragraph ()
  "Write a template for a *feffit* zeroth path paragraph.
Use a tempo template.  Bound to \\[Feffit-zeroth-path-template]."
  (interactive)
  (let ((case-fold-search t))
    (if (not (re-search-backward input-end-data-set nil t))
	(goto-char (point-min)))
    (re-search-forward "^[ \t]*path" nil 'move)
    (forward-line -1)
    (end-of-line)
    (newline)
    (tempo-template-Feffit-zeroth-template)
    (fuse-mark-hotspots tempo-marks)
    (setq input-used-tempo-flag t)
    (cond (input-use-hilit19
	   (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				          ;;input-programs-alist))))
	   (hilit-repaint-command t)))
    (message (substitute-command-keys
	      "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots."))))


(defvar Feffit-outbase nil)

(defvar Feffit-bkg-template
  '("\nbkg = "  p "true" n
    "bkgout = " p "true" n
    "kspout = " p "true" n
    "# bkgfile = " p Feffit-outbase "k.bkg" n n)
  "Template for an *feffit* background function template.
Inserted by  \\[Feffit-make-bkg-template].")

(tempo-define-template "Feffit-bkg-template" Feffit-bkg-template
		       nil
		       "Insert an empty Feffit background function template.")
(eval-when-compile (defun tempo-template-Feffit-bkg-template))

;; This is not properly `cleaned'
(defun Feffit-make-bkg-template ()
  "Write a background template.
This is a tempo template and contains keywords needed to make *feffit*
refine the fit of the background function.  Values appropriate for
making feffit fit the background and save the result in k-space are
inserted in the template.  Bound to \\[Feffit-make-bkg-template]"
  (interactive)
  (let ((case-fold-search t))
    (Feffit-parse-data-set)
    (setq Feffit-outbase (cdr (assoc "outbase" Feffit-features-alist)))
    (search-backward "next data set" (point-min) "to_limit")
    (if (search-forward "==+==+" (point-max) t)
	(beginning-of-line))
    (tempo-template-Feffit-bkg-template)
    (fuse-mark-hotspots tempo-marks)
    (setq input-used-tempo-flag t)
    (setq Feffit-outbase nil)
    ;;(Feffit-clean-region) ;; ?
    (cond (input-use-hilit19
	   (funcall input-program-hilit)  ;;(nth 1 (cdr (assoc input-program-name
				          ;;input-programs-alist))))
	   (hilit-repaint-command t)))
    (message (substitute-command-keys
	      "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots."))))


(defun Feffit-swap-bkg-template ()
  "Toggle the values of keywords controlling the background fit in *feffit*.
To enable fitting of the background, kspout and bkgout are set
to true and bkgfile is comment out.  To enable use of an already found
background function, bkgout and kspout are set to false and bkgfile is
uncommented.  Bound to \\[Feffit-swap-bkg-template]"
  (interactive)
  (save-excursion
    (let ((continue t)
	  (case-fold-search t)
	  (upper-limit nil)
	  (lower-limit nil))
      (if (search-backward "next data set" (point-min) "to_limit")
	  (forward-line 1))
      (setq upper-limit (point-marker))
      (if (re-search-forward "\\(next data set\\)\\|\\<end\\>"
			     (point-max) "to_limit")
	  (forward-line -1))
      (setq lower-limit (point-marker))

      ;; check if bkg, bkg, and kspout are in comment or title lines,
      ;; but only title for bkgfile
					; find and swap bkg
      (while continue
	(cond ((re-search-backward "\\<bkg\\>" upper-limit "to-limit")
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (forward-word 2)
		 (input-true-false)
		 (setq continue nil)))
	      (t
	       (setq continue nil)) ) )
					; find and swap bkgout
      (setq continue t)
      (goto-char lower-limit)
      (while continue
	(cond ((re-search-backward "\\<bkgout\\>" upper-limit "to-limit")
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (forward-word 2)
		 (input-true-false)
		 (setq continue nil)))
	      (t
	       (setq continue nil))))
					; find and swap kspout
      (setq continue t)
      (goto-char lower-limit)
      (while continue
	(cond ((re-search-backward "\\<kspout\\>" upper-limit "to-limit")
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (forward-word 2)
		 (input-true-false)
		 (setq continue nil)))
	      (t
	       (setq continue nil))))
					; find and (un)comment bkgfile
      (setq continue t)
      (goto-char lower-limit)
      (while continue
	(cond ((re-search-backward "\\<bkgfile\\>" upper-limit "to-limit")
	       (if (input-title-p)
		   ()
		 (input-swap-comment)
		 (setq continue nil)))
	      (t
	       (setq continue nil))))  )))


(defun fuse-Feffit-choose-template ()
  (interactive)
  (let* ((alist '(("local header"              . "Feffit-make-lhead-template")
		  ("path paragraph"            . "Feffit-make-path-paragraph")
		  ("zeroth path"               . "Feffit-zeroth-path-paragraph")
		  ("global header"             . "Feffit-make-ghead-template")
		  ("background function"       . "Feffit-make-bkg-template")
		  ("feffit.inp from files.dat" . "Feffit-from-files")))

	 (type
	  (completing-read
	   "What type of template? (<tab> for a complete list) " alist nil t)))
    (if type
	(funcall (intern (cdr (assoc type alist)))) ) ))


;;;; end of feffit templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; special functions for feffit


;;;; Path paragraph manipulations -----------------------------------------

(defun Feffit-add-path-param (&optional arg)
  "Add a path parameter to every path paragraph.  The parameter is
prompted for in minibuffer.  <tab> completes on the names of the valid
path-parameetrs.  A value for this parameter will be prompted for in
the minibuffer and inserted as the math expression for each new parameter.
ARG non-nil means to add parameters starting at point.

\\[Feffit-add-path-param] is used to add a parameter to every paragraph
in the data set.

\\[universal-argument] \\[Feffit-add-path-param] is used to add a
parameter to every paragraph in the data set which comes after the
location of point."
  (interactive "*P")
  (let (param value space index indent limit (case-fold-search t)
	      (completion-ignore-case t))
    (save-excursion
      (setq indent (input-make-indent-string input-path-paragraph-indent nil)
	    space  (input-make-indent-string input-path-paragraph-separate t)
	    param  (completing-read "Add which parameter? "
				    Feffit-pathparams-alist nil t)
	    value  (read-string (format "Default value for %s? " param))
	    limit  (save-excursion
		     (re-search-forward "\\(next data set\\)\\|\\<end\\>"
					(point-max) "to_limit")
		     (point-marker)))
      (cond ((and arg (or (input-comment-p) (input-blank-p)))
	     (input-forward-paragraph))
	    ((and arg (input-path-parameter-p))
	     (input-backward-paragraph)
	     (input-forward-paragraph))
	    (t
	     (search-backward "next data set" (point-min) "to_limit")
	     (input-forward-paragraph)))
      (while (not (input-after-last-paragraph-p))
	(setq index (nth 0 (input-path-param-value)))
	(re-search-forward Feffit-paragraph-separate limit "to_limit")
	(beginning-of-line)
	(insert (concat indent param space index space value "\n"))
	(input-forward-paragraph))
      (input-repaint-command)
      )))

(defun Feffit-comment-path-param (&optional arg)
  "Comment out a path parameter from every path paragraph.  The
parameter is prompted for in minibuffer.  <tab> completes on the names
of the valid path-parameetrs.  ARG non-nil means to comment parameters
starting at point.

\\[Feffit-comment-path-param] is used to comment out a parameter from
every paragraph in the data set.

\\[universal-argument] \\[Feffit-comment-path-param] is used to comment out
the parameter from every paragraph in the data set which comes after the
location of point."
  (interactive "*P")
  (let ((param  (completing-read "Comment out which parameter? "
				 Feffit-pathparams-alist nil t))
	eol (case-fold-search t) (completion-ignore-case t))
    (save-excursion

      (cond ((and arg (or (input-comment-p) (input-blank-p)))
	     (input-forward-paragraph))
	    ((and arg (input-path-parameter-p))
	     (input-backward-paragraph)
	     (input-forward-paragraph))
	    (t
	     (search-backward "next data set" (point-min) "to_limit")
	     (input-forward-paragraph)))

      (while (not (input-after-last-paragraph-p))
	(cond ((input-find-path-param param)
	       (beginning-of-line)
	       (setq eol (save-excursion (end-of-line) (point)))
	       (comment-out-region (point) eol 1)
	       (input-clean-line)))
	(input-forward-paragraph))
      (input-repaint-command)
      )))


(defun Feffit-uncomment-path-param (&optional arg)
  "Uncomment a path parameter from every path paragraph.  The
parameter is prompted for in minibuffer.  <tab> completes on the names
of the valid path-parameetrs.  ARG non-nil means to uncomment parameters
starting at point.

\\[Feffit-uncomment-path-param] is used to uncomment a parameter from
every paragraph in the data set.

\\[universal-argument] \\[Feffit-uncomment-path-param] is used to uncomment
the parameter from every paragraph in the data set which comes after the
location of point."
  (interactive "*P")
  (let* ((param (completing-read "Uncomment which parameter? "
				 Feffit-pathparams-alist nil t))
	 (expr (concat "^[ \t]*[%#!*]+[ \t]*" param))
	 limit (case-fold-search t) (completion-ignore-case t))
    (save-excursion

      (cond ((and arg (or (input-comment-p) (input-blank-p)))
	     (input-forward-paragraph))
	    ((and arg (input-path-parameter-p))
	     (input-backward-paragraph)
	     (input-forward-paragraph))
	    (t
	     (search-backward "next data set" (point-min) "to_limit")
	     (input-forward-paragraph)))

      (while (not (input-after-last-paragraph-p))
	(setq limit (save-excursion
		      (re-search-forward Feffit-paragraph-separate
					 (point-max) "to_limit")
		      (point-marker)))
	(cond ((re-search-forward expr limit "to_limit")
	       (back-to-indentation)
	       (if (looking-at "[%#!*]+") (replace-match ""))
	       (input-clean-line)))
	(input-forward-paragraph))
      (input-repaint-command)
      )))

(defun Feffit-remove-path-param (&optional arg)
  "Remove a path parameter from every path paragraph.  The
parameter is prompted for in minibuffer.  <tab> completes on the names
of the valid path-parameetrs.  ARG non-nil means to remove parameters
starting at point.

\\[Feffit-remove-path-param] is used to remove a parameter from
every paragraph in the data set.

\\[universal-argument] \\[Feffit-remove-path-param] is used to remove
the parameter from every paragraph in the data set which comes after the
location of point."
  (interactive "*P")
  (let ((param (completing-read "Remove which parameter? "
				Feffit-pathparams-alist nil t))
	 mark-1 (case-fold-search t) (completion-ignore-case t))
    (save-excursion

      (cond ((and arg (or (input-comment-p) (input-blank-p)))
	     (input-forward-paragraph))
	    ((and arg (input-path-parameter-p))
	     (input-backward-paragraph)
	     (input-forward-paragraph))
	    (t
	     (search-backward "next data set" (point-min) "to_limit")
	     (input-forward-paragraph)))

      (while (not (input-after-last-paragraph-p))
	(cond ((input-find-path-param param)
	       (beginning-of-line)
	       (setq mark-1 (point-marker))
	       (forward-line 1)
	       (delete-region mark-1 (point))))
	(input-forward-paragraph))
      )))


(defun Feffit-renumber-paragraph (&optional index)
  "Renumber the current path paragraph.
If the path INDEX is not supplied, then it is prompted for in the minibuffer.
Bound to \\[Feffit-renumber-paragraph]"
  (interactive)
  (save-excursion
    (let (space mark-1 (case-fold-search t))
      (if index ()
	(setq index (string-to-number
		     (read-string "New index for this paragraph > "))))
      (if (= index 0) (setq index 1))
      (setq index (abs (truncate index)))
      (setq space  (input-make-indent-string input-path-paragraph-separate t))
      (if (re-search-backward Feffit-paragraph-separate (point-min) "to_limit")
	  (input-forward-paragraph))
      (while (not (looking-at Feffit-paragraph-separate))
	(if (input-comment-p)
	    ()
	  (forward-word 1)
	  ;; the purpose of the . is to preserve the location of the
	  ;; tempo mark if the paragraph was just made with a template
	  (insert (concat space index space "."))
	  (delete-horizontal-space)
	  (setq mark-1 (point-marker))
	  (forward-word 1)
	  (delete-region mark-1 (point))
	  (delete-horizontal-space)
	  (delete-backward-char 1))
	(forward-line 1))
      )))


(defun Feffit-renumber-data-set (&optional arg)
  "Renumber some or all path paragraphs in current data set.
The value of ARG specifies how this function behaves.

       Renumber the entire data set:
\\[Feffit-renumber-data-set] causes the entire data set to be renumbered
with the index of the first data set being prompted for in the
minibuffer.

       Renumber the entire data set with
         a specified initial index:
\\[universal-argument] n \\[Feffit-renumber-data-set] will cause the
data set to be renumbered with the first paragraph taking the value
of n.

       Renumber the data set from point:
\\[universal-argument] \\[Feffit-renumber-data-set] will cause the data
set to be renumbered after point, with the initial index being prompted
for in the minibuffer."
  (interactive "*P")
  (save-excursion
    (let ((case-fold-search t))
      (unless (input-after-last-paragraph-p)
	(cond ((and (numberp arg) (/= arg 1))
	       (setq arg (abs (truncate arg)))
	       (search-backward "next data set" (point-min) "to_limit")
	       (input-forward-paragraph))
	      ((equal arg '(4))
	       (cond ((input-first-paragraph-p)
		      (search-backward "next data set" (point-min) "to_limit")
		      (input-forward-paragraph))
		     ((or (input-comment-p) (input-blank-p))
		      (input-forward-paragraph))
		     (t
		      (input-forward-paragraph)
		      (input-backward-paragraph)))
	       (setq arg (nth 0 (input-path-param-value)))
	       (setq arg (string-to-number
			  (read-string "Index of paragraph under point > " arg))))
	      (t
	       (setq arg (string-to-number
			  (read-string
			   "Index for first paragraph > " "1")))
	       (if (or (/= arg 0) (not arg))
		   (setq arg (abs (truncate arg)))
		 (setq arg 1))
	       (search-backward "next data set" (point-min) "to_limit")
	       (input-forward-paragraph)))

	(while (not (input-after-last-paragraph-p))
	  (Feffit-renumber-paragraph arg)
	  (input-forward-paragraph)
	  (setq arg (1+ arg)))
	))))


(defun Feffit-clean-path-paragraph ()
"Tidy up indentation and column separation in the current path paragraph.
Bound to \\[Feffit-clean-path-paragraph]"
  (interactive)
  (save-excursion
    (let (begin)
      (end-of-line)
      (if (re-search-backward Feffit-paragraph-separate (point-min) "to_limit")
	  (input-forward-paragraph))
      (setq begin (point-marker))
      (input-forward-paragraph)
      (input-clean-region begin (point))
      )))

(defun Feffit-clean-data-set ()
  "Clean up all lines in the current data set.
Insert correct indentation and column separation for each line.
Bound to \\[Feffit-clean-data-set]"
  (interactive)
  (let (begin)
    (save-excursion
      (search-backward "next data set" (point-min) "to_limit")
      (setq begin (point-marker))
      (search-forward  "\\<\\(next data set\\)\\|end\\>" (point-max) "to_limit")
      (input-clean-region begin (point))
      )))


(defun Feffit-clean-pathparagraph ()
  "Insert field separations in a path paragraph line.
Assume indentation  has already happened."
  (let (space)
    (setq space (input-make-indent-string input-path-paragraph-separate t))
    (back-to-indentation)
    (forward-word 1)
    (if (looking-at (concat space "\\<\\|=")) ()
      (delete-horizontal-space)
      (insert space))
    (forward-word 1)
    (if (looking-at (concat space "\\<\\|=")) ()
      (delete-horizontal-space)
      (insert space)) ))

(defun Feffit-clean-setguess ()
  "Insert field separations in a set/guess/local line.
Assume indentation  has already happened."
  (let (space (counter 2))
    (setq space (input-make-indent-string input-set-guess-separate t))
    (back-to-indentation)
    (while (> counter 0)             ; three columns, two separations
      (forward-word 1)
      (if (looking-at (concat space "\\<")) ()
	(delete-horizontal-space)
	(cond ((looking-at "=\\|,")
	       (delete-char 1)
	       (delete-horizontal-space)))
	(insert space))
      (setq counter (1- counter)))   ; count 2,1,0
    ))

;;;; parsing data set and plotting ------------------------------------------

;; Something like this would be very useful in .fuse:
;;   (define-key input-mode-map [(f11)] 'Feffit-quick-plot-path)
;; This could be in fuse-gnuplot, but should another plotting interface
;; ever be written, this should still work!
(defun Feffit-quick-plot-path ()
  "Move forward by one paragraph and plot that path in R space."
  (interactive)
  (when (string= input-program-name "feffit")
    (when (input-forward-paragraph 1)
      ;; moving between master and include files would be nice
      (Feffit-plot-path-in-r)
      (recenter))))

(defun Feffit-parse-data-set ()
  "Parse current data set to get information for plotting.
Store them in Feffit-features-alist.  The following
items are returned: values of allout, bkgout, {krq}spout, formin,
formout, a list of path indeces, datafile name, bkgfile name, outfile
base name, and first title."
  (interactive)
  (save-excursion
    (let ((case-fold-search t)
	  upper-limit lower-limit
	  title formin formout format allout bkgout kspout
	  (rspout   t) qspout datafile bkgfile outbase indeces
	  mark-1 eol )
      (setq Feffit-features-alist nil)
					; define bounds of data set
      (search-backward "next data set" (point-min) "to_limit")
      (if (looking-at "next") (end-of-line))
      (setq upper-limit (point-marker))
      (re-search-forward "\\(next data set\\)\\|\\<end\\>" (point-max) "to_limit")
      (beginning-of-line)
      (setq lower-limit (point-marker))

					; get formin/formout
					; these are local values
      (setq formin
	    (Feffit-parsed-value "formin"  upper-limit "uw" "uwxafs"))
      (setq formout
	    (Feffit-parsed-value "formout" upper-limit "uw" "uwxafs"))
      (setq format
	    (Feffit-parsed-value "format"  upper-limit "uw" "uwxafs"))
      (if (and (not formin) (string= format "uwxafs"))
	  (setq formin "uwxafs"))
      (if (not formin) (setq formin "ascii"))
      (if (and (not formout) (string= format "uwxafs"))
	  (setq formout "uwxafs"))
      (if (not formout) (setq formout "ascii"))

					; get output flags
      (goto-char (point-max))           ; these are global values
      (setq allout
	    (Feffit-parsed-value "all\\(out\\)?" (point-min) "t\\|y" t))
      (setq bkgout
	    (Feffit-parsed-value "bkg\\(out\\)?" (point-min) "t\\|y" t))
      (setq kspout
	    (Feffit-parsed-value "kspout"        (point-min) "t\\|y" t))
      (setq rspout
	    (Feffit-parsed-value "rspout"        (point-min) "n\\|f" nil t))
      (setq qspout
	    (Feffit-parsed-value "qspout"        (point-min) "t\\|y" t))

					; find data, bkg, out names
					; these are local values
      (goto-char lower-limit)
      (setq datafile (Feffit-get-word upper-limit lower-limit "data"))
      (setq bkgfile  (Feffit-get-word upper-limit lower-limit "bkgfile"))
      (setq outbase  (Feffit-get-word upper-limit lower-limit "out"))
      (if outbase                       ; remove extension from outbase
	  (setq outbase (file-name-sans-extension outbase))
	(if datafile                    ; default outbase is datafile base
	    (setq outbase (file-name-sans-extension datafile))))

					; get list of path indeces
					; these are local values
      (goto-char upper-limit)
      (while (not (input-last-paragraph-p))
	(input-forward-paragraph)
	(setq indeces (cons (nth 0 (input-path-param-value)) indeces)) )
      (setq indeces (reverse indeces))


					; first title line
					; these are local values
      (goto-char upper-limit)
      (cond ((re-search-forward "\\<title\\>" lower-limit t)
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point-marker))
	     (end-of-line)
	     (setq eol    (point-marker))
	     (goto-char mark-1)
	     (re-search-forward input-comment-expr eol "to_eol")
	     (setq title (buffer-substring-no-properties mark-1 (point-marker)))))

					; make alist
      (setq Feffit-features-alist
	    (append
	     (list (cons "title"    title))
	     (list (cons "datafile" datafile))
	     (list (cons "bkgfile"  bkgfile))
	     (list (cons "outbase"  outbase))
	     (list (cons "kspout"   kspout))
	     (list (cons "rspout"   rspout))
	     (list (cons "qspout"   qspout))
	     (list (cons "allout"   allout))
	     (list (cons "bkgout"   bkgout))
	     (list (cons "formin"   formin))
	     (list (cons "formout"  formout))
	     (list (cons "indeces"  indeces)) ))

      (if (interactive-p)
	  (message "Parsed %s input file: %s"
		   input-program-name (file-name-nondirectory buffer-file-name)))
      ;;(message "%S" Feffit-features-alist)

      )))


(defun Feffit-get-word (upper lower keyword)
  "Search in a data set for a given keyword and return its one word value.
Care is taken not to match the keyword in a comment or
a title line.  Data set is searched from the bottom to the top.
Search is between UPPER and LOWER and KEYWORD is searched for."
  (save-excursion
    (let ((value    nil)
	  (expr     nil)
	  (continue t)
	  (eol      nil)
	  (mark-1   nil)
	  (case-fold-search t))
      (setq expr (format "\\<%s\\>" keyword))
      (goto-char lower)
      (while continue
	(cond ((re-search-backward expr upper "to_limit")
	       (if (or (input-comment-p) (input-title-p))
		   ()
		 (setq mark-1 (point-marker))
		 (end-of-line)
		 (setq eol (point-marker))
		 (goto-char mark-1)
		 (re-search-forward input-word-sep eol t)
		 (if (looking-at "[,= \t]*$")
		     ()
		   (setq value (input-this-word))
		   (setq continue nil) ) ))
	      (t
	       (setq continue nil)) ) )
      value )))



(defun Feffit-parsed-value (keyword limit regexp value &rest default)
  "Find a given keyword and return its value.
Care is taken if the keyword is commented out.  Used in
Feffit-parse-data-set.  KEYWORD is the keyword to search for, LIMIT is
extent of search REGEXP is the keyword value to recognize, VALUE is
the keyword value to assign if REGEXP is found, DEFAULT is the keyword
value to assign if REGEXP is not found.  Used for logical and i/o
format keywords"
  (save-excursion
    (let ((match nil)
	  (return nil)
	  (case-fold-search t))
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



(defun Feffit-set-plot-column (space arg)
  "Reset value of plotting column."
  (interactive "sspace: \nncolumn: ")
  (if (member arg '(2 3 4 5))
      (cond ((string= space "r")
	     (setq input-gnuplot-r-column arg)
	     (message "Now plotting column %s in R space." arg))
	    ((string= space "q")
	     (setq input-gnuplot-q-column arg)
	     (message "Now plotting column %s in backtransform space." arg))
	    ((string= space "k")
	     (message "k-space data is always 2 columns"))
	    (t
	     (message "%S is not a valid space" space)) )
    (message "%S is not a valid column number" arg) ))

;;;; mark paths for plotting ----------------------------------------------

(defvar Feffit-marked-paths ())
(make-variable-buffer-local 'Feffit-marked-paths)

(defun Feffit-mark-path-for-plot ()
  "Add the current path to the list of paths to plot.
The next time a plot is made in any space, each marked path will be
added to the plot if the path file exists.  A marked path is denoted
visually by a colored background behind the \"path\" or \"feff\"
path parameter."
  (interactive)
  (let (index (path-regex "^[ \t]*\\(path\\|feff\\)"))
    (save-excursion
      (if (or (looking-at path-regex)
	      (re-search-backward path-regex (point-min) t))
	  (setq index (string-to-number (elt (input-path-param-value) 0)))
	(re-search-forward path-regex (point-max) t)
	(back-to-indentation)
	(setq index 1))
      (unless (member* index Feffit-marked-paths :test 'equal)
	(setq Feffit-marked-paths (append Feffit-marked-paths (list index)))
	(cond (fuse-xemacs-p
	       (let* ((begin  (point-marker))
		      (end    (save-excursion (forward-word 1) (point-marker)))
		      (extent (make-extent begin end)))
		 (set-extent-endpoints extent begin end)
		 (set-extent-face extent 'Feffit-path-face)
		 (set-extent-property
		  extent 'help-echo
		  (concat "This path will be plotted next time.  "
			  "Hit S-mouse-2 to toggle marking."))))
	      (t
	       (overlay-put (make-overlay (point)
					  (save-excursion
					    (forward-word 1) (point-marker)))
			    'face 'Feffit-path-face)))) )))

(defun Feffit-mark-path-with-mouse (event)
  "Same as `Feffit-mark-or-unmark' but using the mouse.
Bound to \\[Feffit-mark-path-with-mouse]"
  (interactive "@e")
  (mouse-set-point event)
  (Feffit-mark-or-unmark))

(defun Feffit-mark-or-unmark ()
  "Mark or unmark the current path paragraph for plotting.
Bound to \\[Feffit-mark-or-unmark]"
  (interactive)
  (let ((path-regex "^[ \t]*\\(path\\|feff\\)"))
    (save-excursion
      (when (or (looking-at path-regex)
		(progn
		  (end-of-line)
		  (re-search-backward path-regex (point-min) t)))
	(let ((index (string-to-number (elt (input-path-param-value) 0))))
	  (if (member* index Feffit-marked-paths :test 'equal)
	      (Feffit-unmark-path)
	    (Feffit-mark-path-for-plot)))))))

(defun Feffit-mark-all-paths ()
  "Mark all paths in the current data set for plotting.
See `Feffit-mark-path-for-plot'.  Bound to \\[Feffit-mark-all-paths]"
  (interactive)
  (let (end (path-regex "^[ \t]*\\(path\\|feff\\)"))
    (save-excursion
      (re-search-backward "^[ \t]*next[ \t]+data[ \t]+set"
			  (point-min) "to_limit")
      (save-excursion
	(re-search-forward "\\(^[ \t]*next[ \t]+data[ \t]+set\\)\\|\\<end\\>"
			   (point-max) "to_limit")
	(setq end (point-marker)))
      (while (re-search-forward path-regex end t)
	(Feffit-mark-path-for-plot)))))

(defun Feffit-unmark-path ()
  "Clear path marking in current paragraph.
See `Feffit-mark-path-for-plot'."
  (interactive)
  (let ((index (string-to-number (elt (input-path-param-value) 0)))
	(path-regex "^[ \t]*\\(path\\|feff\\)"))
    (if (member* index Feffit-marked-paths)
	(save-excursion
	  (when (or (looking-at path-regex)
		    (re-search-backward path-regex (point-min) t))
	    (forward-word 1) (backward-char 1)
	    (cond (fuse-xemacs-p
		   (delete-extent (extent-at (point) (current-buffer) 'face)))
		  (t
		   (let ((overlays (overlays-at (point))))
		     (while overlays
		       (if (eq (overlay-get (car overlays) 'face)
			       'Feffit-path-face)
			   (delete-overlay (car overlays)))
		       (setq overlays (cdr overlays))))))
	    (setq Feffit-marked-paths
		  (remove* index Feffit-marked-paths :test '=)) ))) ))

(defun Feffit-clear-all-marked-paths ()
  "Clear all path markings.
See `Feffit-mark-path-for-plot'.  Bound to \\[Feffit-clear-all-marked-paths]"
  (interactive)
  (fuse-clear-overlays 'face 'Feffit-path-face)
  (setq Feffit-marked-paths ()))

;;;; fd2fi -- feffit.inp from files.dat -----------------------------------
(defun Feffit-from-files ()
  "Convert a files.dat file into a feffit.inp file.
User is prompted for input file name.  Set input-feff-path to look
there by default.  Paths can be filtered by amplitude, path length,
and/or number of legs The values of input-out-path and input-data-path
are used in the input file header.  Bound to \\[Feffit-from-files]"

  (interactive)
					; header template for fd2fi
  (let (point-1 point-2 point-3 point-4 point-5 eoh
		(ampmin 0) (nlegmax 10000) (reffmax 10000) fd2fi-index
		files-dot-dat mark-1 (case-fold-search t))
					; read files.dat
    (setq files-dot-dat
	  (read-file-name "Files.dat file: "
			  (concat input-feff-path "files.dat")
			  (concat input-feff-path "files.dat")
			  t))
    (insert-file-contents files-dot-dat)

					; get ampmin, reffmax, nlegmax
    (setq ampmin (read-string "Minimum amplitude [value or !] "))
    (if (string= ampmin "!") (setq ampmin 0)
      (setq ampmin (string-to-number ampmin))
      (setq reffmax (read-string "Maximum r_effective [value or !] "))
      (if (string= reffmax "!") (setq reffmax 10000)
	(setq reffmax (string-to-number reffmax))
	(if (< reffmax 2) (setq reffmax 10000))
	(setq nlegmax (read-string "Maximum number of legs [value] "))
	(setq nlegmax (string-to-number nlegmax))
	(if (< nlegmax 2) (setq nlegmax 10000))))

					; make title lines from header
    (while (not (looking-at "^[ #]*-+"))
      (insert (input-indent))
      (insert "title = ")
      (end-of-line)
      (if (> (current-column) 68)
	  (delete-backward-char (- (current-column) 68) nil))
      (delete-horizontal-space)
      (beginning-of-line 2))

					; delete next 2 lines
    (setq mark-1 (point))
    (beginning-of-line 3)
    (delete-region mark-1 (point))

					; insert feffit.inp template
    (forward-line 1)
    (setq eoh (point-marker))
    (forward-line -1)
    (newline 1)
    (Feffit-make-ghead-template)
    (goto-char eoh) (forward-line -1)
    (Feffit-make-lhead-template)
    (goto-char eoh) (forward-line -1)
    (insert "%  insert sets and guesses here\n"
	    input-comment-delimiter "\n\n")

					; make path paragraphs
					; this might not stop properly
					; if !!&& lines are not at end
    (setq   fd2fi-index 0)
    (while (and (not (eobp))
		(not (looking-at (concat "[\t\n ]*"
					 (regexp-quote comment-start)))))
      (cond ((looking-at "^[ \t]*$")
	     (forward-line 1))
	    ((input-comment-p)
	     (forward-line 1))
	    ((looking-at "^ *feff")
					; find amp, nleg, and reff for path
	     (forward-word 2) (setq point-1 (point-marker))
	     (forward-word 1) (setq point-2 (point-marker)) ;; amp
	     (forward-word 1) (setq point-3 (point-marker))
	     (forward-word 1) (setq point-4 (point-marker)) ;; nlegs
	     (end-of-line)    (setq point-5 (point-marker)) ;; reff
	     (beginning-of-line)
					; check amp, reff, nleg for each path
					; delete line or construct paragraph
	     (cond ((or (< (string-to-number
			    (buffer-substring-no-properties point-1 point-2))
			   ampmin)
			(> (string-to-number
			    (buffer-substring-no-properties point-3 point-4))
			   nlegmax)
			(> (string-to-number
			    (buffer-substring-no-properties point-4 point-5))
			   reffmax))
		    (setq mark-1 (point-marker))
		    (forward-line 1)
		    (delete-region mark-1 (point-marker)))
		   (t
		    (setq fd2fi-index (1+ fd2fi-index))
		    (insert "path " (number-to-string fd2fi-index))
		    (delete-horizontal-space)
		    (insert " " input-feff-path)
		    (search-forward "dat")
		    (insert "\nid " (number-to-string fd2fi-index) " ")
		    (setq mark-1 (point-marker))  ; delete feff's sigma2
		    (forward-word 1)
		    (search-forward " ")
		    (delete-region mark-1 (point))
		    (delete-horizontal-space)
		    (insert " amp=")            ; insert strings into id line
		    (forward-word 1) (insert ", deg=")
		    (delete-horizontal-space)
		    (forward-word 1) (insert ", nleg=")
		    (delete-horizontal-space)
		    (forward-word 1) (insert ", r_eff=")
		    (delete-horizontal-space)
		    (beginning-of-line 2)
		    (insert "\n")
			;;"delr\t"     (number-to-string fd2fi-index)
			;;"\nsigma2\t" (number-to-string fd2fi-index)
			;;"\n\n"))
		    )))))
    (goto-char (point-min))                 ; return to top of buffer
    (Feffit-clean-data-set)
    (input-repaint-command)
    (tempo-forward-mark) (tempo-forward-mark)
    (message (substitute-command-keys
	      "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots."))
    ))


;;;; visit various files --------------------------------------------------


;; in the following, need to use general name when implemented
;; an alist that gets evaluated upon reading file would be nice

(defun Feffit-jump-to-misc ()
  "Display misc.dat from the feff run in another buffer.
Set  input-feff-path to look there by default.
Bound to \\[Feffit-jump-to-misc]"
  (interactive)
  (let ((file "misc.dat"))
    (setq file (concat input-feff-path file))
    (if (file-readable-p file)
	(input-jump-to-file file)
      (message "%S not readable, set feff file path with %S"
	       file (substitute-command-keys "\\[input-set-feff-path]")))))

(defun Feffit-jump-to-files ()
  "Display files.dat from the feff run in another buffer.
Set  input-feff-path to look there by default.
Bound to \\[Feffit-jump-to-files]"
  (interactive)
  (let ((file "files.dat"))
    (setq file (concat input-feff-path file))
    (if (file-readable-p file)
	(input-jump-to-file file)
      (message "%S not readable, set feff file path with %S"
	       file (substitute-command-keys "\\[input-set-feff-path]")))))

(defun Feffit-jump-to-list ()
  "Display list.dat from the feff run in another buffer.
Set  input-feff-path to look there by default.
Bound to \\[Feffit-jump-to-list]"
  (interactive)
  (let ((file "list.dat"))
    (setq file (concat input-feff-path file))
    (if (file-readable-p file)
	(input-jump-to-file file)
      (message "%S not readable, set feff file path with %S"
	       file (substitute-command-keys "\\[input-set-feff-path]")))))

(defun Feffit-jump-to-paths ()
  "Display paths.dat from the feff run in another buffer.
Set  input-feff-path to look there by default.
Bound to \\[Feffit-jump-to-paths]"
  (interactive)
  (let ((file "paths.dat"))
    (setq file (concat input-feff-path file))
    (if (file-readable-p file)
	(input-jump-to-file file)
      (message "%S not readable, set feff file path with %S"
	       file (substitute-command-keys "\\[input-set-feff-path]")))))

(defun Feffit-display-intrp ()
  "Display output of intrp in its own buffer.
This requires that the perl script intrp is executable and in the
path.  It also requires that input-feff-path is set correctly (use
\\[input-set-feff-path]).  Bound to \\[Feffit-display-intrp]"
  (interactive)
  (let (args mark-1 mark-2 (orig (current-buffer))
	     (ok (and
		  (file-exists-p (concat input-feff-path "feff.inp" ))
		  (file-exists-p (concat input-feff-path "files.dat"))
		  (file-exists-p (concat input-feff-path "paths.dat")) )) )
    (if (not ok)
	(message
	 (substitute-command-keys
	  "Could not file feff files.  Set path to feff with \\[input-set-feff-path]"))
      (setq args (concat "-s -d " (expand-file-name input-feff-path)))
      (if input-intrp-args
	  (setq args (concat args " " input-intrp-args)))
      (and (string= " " (substring args -1 nil))
	   (setq args (substring args 0 -1)))
      (switch-to-buffer input-intrp-buffer-name)
      (setq input-originating-buffer orig)
      (goto-char (point-max))
      (setq mark-1 (point-marker))
      (insert (concat "\n >> intrp " args "\n\n"))
      (call-process "intrp" "/dev/null" input-intrp-buffer-name
		    nil args)
      (while (not (bobp))
	(cond ((re-search-backward "^[ \t]*Unknown option:"
				   (point-min) "to_limit")
	       (setq mark-2 (point-marker))
	       (beginning-of-line 2)
	       (delete-region mark-2 (point)))) )
      (goto-char mark-1)
      (recenter 2)
      (message
       (substitute-command-keys
	"\\[input-back-to-original] to return to previous buffer.")) )))

(defun Feffit-jump-to-prm-file ()
  "Display the prm file for this *feffit* input file in another buffer.
Bound to \\[Feffit-jump-to-prm-file]"
  (interactive)
  (let ((prmfile nil))
    (if input-master
	(setq prmfile input-master)
      (setq prmfile (buffer-file-name)))
    (setq prmfile (file-name-sans-extension prmfile))
    (setq prmfile (concat prmfile ".prm"))
    (if (file-exists-p prmfile)
	(input-jump-to-file prmfile)
      (message "%S does not exist.  Have you run feffit yet?"
	       (file-relative-name prmfile "./")))
    ))


;; what about include files/master files
;; what about multiple data sets?
(defun Feffit-insert-best-fit ()
  "Find and replace all guess values with their best fit values.
Values are obtained from the log file.  If input-best-fit-set-flag is
non-nil, then all guesses will be swapped to sets.  Also include
uncertainties as end of line comments.
Bound to \\[Feffit-insert-best-fit]"
  (interactive)
  (save-excursion
    (let (mark-1 word value error (guesses-alist ())
		 (counter 0) space basename expr (case-fold-search t))
      (if input-master
	  (setq basename input-master)
	(setq basename (buffer-file-name)))
      (setq basename (file-name-sans-extension basename))
      (setq basename (concat basename ".log"))
      (cond ((file-exists-p basename)
	     (input-jump-to-log-file)
	     (cond ((re-search-forward "^[ \t]*variable[ \t]*best fit value"
				       (point-max) t)
		    (forward-line 1)
		    (while (not (looking-at "^[ \t]*$"))
		      (re-search-forward "\\w")	; find and set name of guess vble
		      (forward-char -1)
		      (setq mark-1 (point-marker))
		      (re-search-forward "[ \t]")
		      (forward-char -1)
		      (setq word (buffer-substring-no-properties mark-1 (point)))
		      (search-forward "=") ; find and set best fit value
		      (re-search-forward "[ \t]+")
		      (setq mark-1 (point-marker))
		      (re-search-forward "[ \t]")
		      (forward-char -1)
		      (setq value (buffer-substring-no-properties mark-1 (point)))
		      (re-search-forward "[ \t]+") ; find uncertainty
		      (setq mark-1 (point-marker))
		      (re-search-forward "[ \t]")
		      (forward-char -1)
		      (setq error (buffer-substring-no-properties mark-1 (point)))
		      (setq guesses-alist
			    (append guesses-alist
				    (list (cons word (list value error)))))
		      (forward-line 1)))) ; next line

	     ;;(message "%S" guesses-alist)
					; return to input file
	     (message nil)
	     (kill-buffer (current-buffer))
	     (setq space (input-make-indent-string input-set-guess-separate t))
	     (while (< counter (length guesses-alist))
	       (goto-char (point-min))
	       (setq expr (concat "^[ \t]*guess.+"
				  (car (nth counter guesses-alist)) ))
	       ;;(message "expr = %S" expr)
	       (cond ((re-search-forward expr (point-max) t)
		      (setq mark-1 (point-marker))
		      (end-of-line)
		      (delete-region mark-1 (point))
		      (insert (format "%s%s  \t!  +/- %s    -- best fit value"
				      space
				      (nth 0 (cdr (nth counter guesses-alist)))
				      (nth 1 (cdr (nth counter guesses-alist))) ))
		      (if input-best-fit-set-flag (input-guess-to-set))))
	       (setq counter (1+ counter)))
	     )
	    (t
	     (message "%S does not exist.  Have you run %s?"
		      (file-name-nondirectory basename)
		      input-program-name))
	    ))))

;; This has the side effect of closing a log file that was being visited.
;; Need to fix that in jump-to-file
(defun Feffit-insert-this-best-fit ()
  "Find and replace the guess value on the current line with its best fit.
Also include uncertainty as an end of line comment.
Bound to \\[Feffit-insert-best-fit]"
  (interactive)
  (save-excursion
    (let (word value error expr space mark-1 basename logname
	       (case-fold-search t))
      (if input-master
	  (setq basename input-master)
	(setq basename (buffer-file-name)))
      (setq basename (file-name-sans-extension basename))
      (setq logname (concat basename ".log"))
      (back-to-indentation)
      (cond
       ((looking-at "guess\\>")
	(forward-word 2)
	(setq word (input-this-word))
	(setq expr (concat "^[ \t]*\|?[ \t]*\\<" word "\\>"))
	(cond ((file-exists-p logname)
	       (input-jump-to-log-file)
	       (goto-char (point-min))
	       (cond ((re-search-forward expr (point-max) t)
					; get value
		      (re-search-forward "=[ \t]+")
		      (setq mark-1 (point-marker))
		      (re-search-forward "[ \t]")
		      (forward-char -1)
		      (setq value (buffer-substring-no-properties mark-1 (point)))
					; get uncertainty
		      (re-search-forward "[ \t]+")
		      (setq mark-1 (point-marker))
		      (re-search-forward "[ \t]")
		      (forward-char -1)
		      (setq error (buffer-substring-no-properties mark-1 (point)))
		      (message nil)
		      (kill-buffer (current-buffer))
		      (setq space (input-make-indent-string
				   input-set-guess-separate t))
		      (setq mark-1 (point-marker))
		      (end-of-line)
		      (delete-region mark-1 (point))
		      (insert (format "%s%s  \t!  +/- %s    -- best fit value"
				      space value error))
		      (input-guess-to-set))
		     (t
		      (message "Tilt! Could not find %s in %s.log"
			       word basename))))
	      (t
	       (message "%s.log does not exist.  Have you run %s?"
			basename input-program-name))))
       (t
	(message "Point is not on a line containing a guess variable")))
      )))


(defun Feffit-insert-mcmaster ()
  "Read and insert the values of the McMaster corrections.
Read them from a *feff* input file, set math expressions for these
values, and add the McMaster corrections to every path paragraph.  The
name of the feff input file is prompted for in the minibuffer.
Bound to \\[Feffit-insert-mcmaster]."
  (interactive)
  (catch 'problem
    (save-excursion
					; qrtmm ampfac
      (let (sigmm y-n-prompt file space limit expr
		  (case-fold-search t) (insert-default-directory nil))
	(setq expr (concat "\\+[ \t]*" input-mcmaster-sigma))
	(setq space (input-make-indent-string input-set-guess-separate t))
	(setq y-n-prompt (format
			  "Do you need to insert a \"set %s\" line? "
			  input-mcmaster-sigma))
					; go fetch corrections from
					; a feff file
	(cond ((y-or-n-p y-n-prompt)
	       (setq file
		     (read-file-name "Tell me the feff.inp file > "
				     nil
				     (concat input-feff-path "feff.inp")
				     t
				     (concat input-feff-path "feff.inp")))

	       (if (file-exists-p file)
		   (input-jump-to-file file)
		 (message "%S does not exist" file)
		 (throw 'problem file))
	       ;; note that jumping to the file will put it in Feff
	       ;; minor mode and this function will then be known
	       (Feff-parse-file)
	       (setq sigmm  (cdr (assoc "sigmm"  Feff-features-alist)))
	       ;;(setq qrtmm  (cdr (assoc "qrtmm"  Feff-features-alist)))
	       ;;(setq ampfac (cdr (assoc "ampfac" Feff-features-alist)))
	       (message nil)
	       (kill-buffer (current-buffer))
	       (goto-char (point-min))
	       (if (or
		    (re-search-forward "^[ \t]*set\\>" (point-max) t)
		    (re-search-forward "^[ \t]*guess\\>" (point-max) t))
		   (beginning-of-line))
	       (setq limit (point-marker))
	       (insert (format "%sset%s%s%s%s\n" (input-indent "setguess")
			       space input-mcmaster-sigma space sigmm))
	       (end-of-line)
	       (input-repaint-command limit (point)) ))
	(goto-char (point-min))
					; search for lines with sigma2
					; and append " + sigmm"
	(while (not (eobp))
	  (cond ((looking-at "^[ \t]*\\(sigma2\\|ss2\\|dwf\\)\\>")
		 (end-of-line)
		 (setq limit (point-marker))
		 (beginning-of-line)
		 (cond ((re-search-forward expr limit t)
			(cond ((input-comment-p)
			       (re-search-backward input-comment-expr)
			       (insert (format " + %s " input-mcmaster-sigma)))))
		       (t
			(if (re-search-forward input-comment-expr limit "to_limit")
			    (forward-char -1))
			(insert (format " + %s " input-mcmaster-sigma))))))
	  (forward-line 1) )
	))))


(defconst Feffit-tags-regex
  (concat "'/^[ \\t]*"
	  "\\(set\\|guess\\|local\\)"
	  "[ \\t]*[=, \\t]?[ \\t]*"
	  "\\(.+\\)"
	  "[=, \\t]+/\\2/' ")
  "Regular expression for etags.")

;; this was swiped from reftex mode
(defun Feffit-make-tags-file ()
  "Make a TAGS file using etags.

This command will make a TAGS file for the current master file and all
of its include files.  Using the tags, you can easily jump between the
location where a variable is used and the location where it is set or
guessed.  If point is on a varaible where it is being used in a math
expression and you do `\\[find-tag]', display will jump to the place
where the variable is defined in a set or guess expression.
`\\[find-tag-other-window]' and `\\[find-tag-other-frame]' display the
definition of the variable in another window and frame, respectively.

Bound to \\[Feffit-make-tags-file]"
  (interactive)
  (let* ((list (Feffit-parse-include-filenames))
	 (cmd  (format "etags %s" (concat "--language=none --regex="
					  Feffit-tags-regex
					  list))))
    (message "Making TAGS file ...")
    (shell-command cmd)
    (visit-tags-table "TAGS")
    (message "Making TAGS file ... done")))

;; the idea of successively inserting the include files into a dummy
;; buffer comes from reftex
(defun Feffit-parse-include-filenames ()
  "Return a string of the current master file and all include files under it."
  (let (master buffer files found)
    (save-some-buffers)
    (if input-master
	(setq master input-master)
      (setq master (buffer-file-name)))
    (setq files (concat master " "))                  ;; prepend master to list.
    (setq buffer (get-buffer-create "-[]-includes"))  ;; an unlikely name.
    (switch-to-buffer buffer)
    (insert-file-contents master)
    (while (< (point) (point-max))
      (cond ((re-search-forward                       ;; find includes.
	      "\\<include[ \t]*[=, \t][ \t]*\\(\\b[^ \t\n]+\\b\\)"
	      (point-max) "to_limit")
	     (setq found (match-string 1))
	     (if (input-comment-p) ()   ; skip commented out includes, already
	       (if (or (string-match (concat "\\<" found "\\>") files) ;; found,
		       (not (file-exists-p found))) ;; and nonexisting ones.
		   ()
		 (setq files (concat files found " ")) ;; add to string.
		 (newline 2)
		 (forward-line -1)
		 (insert-file-contents found)        ;; insert latest.
		 )))))
    (kill-buffer buffer)
    files
    ))

;;; end of special functions for feffit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for feffit
(defvar Feffit-mode-map ()
  "Keymap used in Feffit minor mode.")
(if Feffit-mode-map
    ()
  (setq Feffit-mode-map (make-sparse-keymap))
  (define-key Feffit-mode-map "\C-c\C-tt" 'Feffit-make-path-paragraph)
  (define-key Feffit-mode-map "\C-c\C-tz" 'Feffit-zeroth-path-paragraph)
  (define-key Feffit-mode-map "\C-c\C-tg" 'Feffit-make-ghead-template)
  (define-key Feffit-mode-map "\C-c\C-tl" 'Feffit-make-lhead-template)
  (define-key Feffit-mode-map "\C-c\C-tb" 'Feffit-make-bkg-template)
  ;;(define-key Feffit-mode-map "\C-c\C-ts" 'Feffit-swap-bkg-template)
  (define-key Feffit-mode-map "\C-c\C-tf" 'Feffit-from-files)

  (define-key Feffit-mode-map "\C-c\C-of" 'input-forward-paragraph)
  (define-key Feffit-mode-map "\C-c\C-ob" 'input-backward-paragraph)
  (define-key Feffit-mode-map "\C-c\C-om" 'input-mark-paragraph)
  (define-key Feffit-mode-map "\C-c\C-ok" 'input-kill-paragraph)
  (define-key Feffit-mode-map "\C-c\C-sp" 'input-snag-from-previous-paragraph)
  (define-key Feffit-mode-map "\C-c\C-sn" 'input-snag-from-next-paragraph)
  (define-key Feffit-mode-map "\C-c\C-sb" 'Feffit-insert-best-fit)
  (define-key Feffit-mode-map "\C-c\C-sg" 'Feffit-insert-this-best-fit)
  (define-key Feffit-mode-map "\C-c\C-sm" 'Feffit-insert-mcmaster)

  (define-key Feffit-mode-map "\C-c\C-fm" 'Feffit-jump-to-misc)
  (define-key Feffit-mode-map "\C-c\C-ff" 'Feffit-jump-to-files)
  (define-key Feffit-mode-map "\C-c\C-fs" 'Feffit-jump-to-list)
  (define-key Feffit-mode-map "\C-c\C-fp" 'Feffit-jump-to-paths)
  (define-key Feffit-mode-map "\C-c\C-fi" 'Feffit-display-intrp)
  (define-key Feffit-mode-map "\C-c\C-fr" 'Feffit-jump-to-prm-file)
  (define-key Feffit-mode-map "\C-c\C-ft" 'Feffit-make-tags-file)

  (define-key Feffit-mode-map "\C-c\C-vi" 'Feffit-set-path-index)
  (define-key Feffit-mode-map "\C-c\C-vr" 'Feffit-renumber-paragraph)

  (define-key Feffit-mode-map "\C-c\C-vs" 'Feffit-renumber-data-set)
  (define-key Feffit-mode-map "\C-c\C-va" 'Feffit-add-path-param)
  (define-key Feffit-mode-map "\C-c\C-vc" 'Feffit-comment-path-param)
  (define-key Feffit-mode-map "\C-c\C-vu" 'Feffit-uncomment-path-param)
  (define-key Feffit-mode-map "\C-c\C-vd" 'Feffit-remove-path-param)

  (define-key Feffit-mode-map "\C-c\C-cp" 'Feffit-clean-path-paragraph)
  (define-key Feffit-mode-map "\C-c\C-cs" 'Feffit-clean-data-set)

  (define-key Feffit-mode-map "\C-c\C-pk" 'Feffit-plot-k)
  (define-key Feffit-mode-map "\C-c\C-pr" 'Feffit-plot-r)
  (define-key Feffit-mode-map "\C-c\C-pq" 'Feffit-plot-q)
  ;;(define-key Feffit-mode-map "\C-c\C-pK" 'Feffit-plot-path-in-k)
  ;;(define-key Feffit-mode-map "\C-c\C-pR" 'Feffit-plot-path-in-r)
  ;;(define-key Feffit-mode-map "\C-c\C-pQ" 'Feffit-plot-path-in-q)
  (define-key Feffit-mode-map "\C-c\C-ps" 'Feffit-set-plot-column)
  (define-key Feffit-mode-map "\C-c\C-pm" 'Feffit-mark-or-unmark)
  (define-key Feffit-mode-map "\C-c\C-pa" 'Feffit-mark-all-paths)
  (define-key Feffit-mode-map "\C-c\C-pc" 'Feffit-clear-all-marked-paths)
  (cond (fuse-xemacs-p
      	 (define-key Feffit-mode-map '(shift button2)
	   'Feffit-mark-path-with-mouse))
	(t
	 (define-key Feffit-mode-map [S-mouse-2]
	   'Feffit-mark-path-with-mouse)))
  )

(defvar Feffit-mode-menu nil)
(easy-menu-define
 Feffit-mode-menu Feffit-mode-map
 "Menu used in Feffit mode"
 '("Feffit"
   ("Templates "
    ["Local header template"                 Feffit-make-lhead-template t]
    ["Path paragraph template"               Feffit-make-path-paragraph t]
    ["Zeroth path template"                  Feffit-zeroth-path-paragraph t]
    ["Global header template"                Feffit-make-ghead-template t]
    ["Background function template"          Feffit-make-bkg-template t]
    ;;["Swap values of background template"    Feffit-swap-bkg-template t]
    ["Make feffit.inp from files.dat"        Feffit-from-files
     :active (file-exists-p (concat input-feff-path "files.dat"))])
   ("Plotting"
    ["Plot chi(r) and fit"                    Feffit-plot-r t]
    ["Plot chi(k) and fit, original k-space"  Feffit-plot-k t]
    ["Plot chi(q) and fit, back transform"    Feffit-plot-q t]
    "---"
    ;;["Plot path in R space"                   Feffit-plot-path-in-r t]
    ;;["Plot path in original k-space"          Feffit-plot-path-in-k t]
    ;;["Plot path in back transform space"      Feffit-plot-path-in-q t]
    ;;"---"
    ["Mark/unmark this path"                  Feffit-mark-or-unmark t]
    ["Mark all paths"                         Feffit-mark-all-paths t]
    ["Clear all marked paths"                 Feffit-clear-all-marked-paths t]
    "---"
    ("Reset plot column"
     ["Plot magnitude in R space"             (Feffit-set-plot-column "r" 4) t]
     ["Plot real part in R space"             (Feffit-set-plot-column "r" 2) t]
     ["Plot imaginary part in R space"        (Feffit-set-plot-column "r" 3) t]
     ["Plot phase in R space"                 (Feffit-set-plot-column "r" 5) t]
     "---"
     ["Plot magnitude in Q space"             (Feffit-set-plot-column "q" 4) t]
     ["Plot real part in Q space"             (Feffit-set-plot-column "q" 2) t]
     ["Plot imaginary part in Q space"        (Feffit-set-plot-column "q" 3) t]
     ["Plot phase in Q space"                 (Feffit-set-plot-column "q" 5) t])
    ["Toggle gnuplot terminal"                input-toggle-gnuplot-terminal t]
    ["Set k-weight"                           input-set-k-weight t])
   "--------- Look at files ------------"
   ("Examine Output From Feff"
    ["Display intrp"                         Feffit-display-intrp
     :active (and (file-exists-p (concat input-feff-path "feff.inp"))
		  (file-exists-p (concat input-feff-path "paths.dat"))
		  (file-exists-p (concat input-feff-path "files.dat"))) ]
    ["Look at misc.dat"                      Feffit-jump-to-misc
     :active (file-exists-p (concat input-feff-path "misc.dat"))]
    ["Look at files.dat"                     Feffit-jump-to-files
     :active (file-exists-p (concat input-feff-path "files.dat"))]
    ["Look at list.dat"                      Feffit-jump-to-list
     :active (file-exists-p (concat input-feff-path "list.dat"))]
    ["Look at paths.dat"                     Feffit-jump-to-paths
     :active (file-exists-p (concat input-feff-path "paths.dat"))])
   ["Look at log file"                       input-jump-to-log-file
    :active (input-log-file-exists-p)]
   ["Look at prm file"                       Feffit-jump-to-prm-file
    :active (input-log-file-exists-p ".prm")]
   "--------- Editing Shortcuts --------"
   ("Move/Mark/Kill"
    ["Next path paragraph"                   input-forward-paragraph t]
    ["Previous path paragraph"               input-backward-paragraph t]
    ["Mark path paragraph"                   input-mark-paragraph t]
    ["Kill path paragraph"                   input-kill-paragraph t])
   ("Paragraph Manipulation"
    ["Renumber this paragraph"               Feffit-renumber-paragraph t]
    ["Reset path index"                      Feffit-set-path-index t]
    "---- Edit Data Set ------------------"
    ["Renumber data set"                     Feffit-renumber-data-set t]
    ["Add parameter to all paragraphs"       Feffit-add-path-param t]
    ["Comment parameter in all paragraphs"   Feffit-comment-path-param t]
    ["Uncomment parameter in all paragraphs" Feffit-uncomment-path-param t]
    ["Remove parameter from all paragraphs"  Feffit-remove-path-param t]
    "---- Edit Data Set From Point -------"
    ["Renumber data set starting at point"   (Feffit-renumber-data-set '(4)) t]
    ["Add parameter starting at point"       (Feffit-add-path-param t) t]
    ["Comment parameter starting at point"   (Feffit-comment-path-param t) t]
    ["Uncomment parameter starting at point" (Feffit-uncomment-path-param t) t]
    ["Remove parameter starting at point"    (Feffit-remove-path-param t) t]
    )
   ("Clean up"
    ["Clean line"                            input-clean-line t]
    ["Clean region"                          input-clean-region t]
    ["Clean file"                            input-clean-file t]
    ["Clean paragraph"                       Feffit-clean-path-paragraph t]
    ["Clean data set"                        Feffit-clean-data-set t])
   ("Snagging"
    ["Snag from previous paragraph"          input-snag-from-previous-paragraph t]
    ["Snag from next paragraph"              input-snag-from-next-paragraph t]
    "---"
    ["Replace guesses with best fit"         Feffit-insert-best-fit t]
    ["Replace this guess with best fit"      Feffit-insert-this-best-fit t]
    "---"
    ["Insert McMaster corrections"           Feffit-insert-mcmaster t])
   "--------- Running ------------------"
   ["Run feffit, master file"                input-run-this-program-this-file t]
   ["Kill feffit run"                        input-kill-program
    :active (get-process input-process)]
   "--------- Et cetera ----------------"
   ("TAGS"
    ["Make TAGS file"                        Feffit-make-tags-file t]
    ["Find tag"                              find-tag t]
    ["Find tag other window"                 find-tag-other-window t]
    ["Find tag other frame"                  find-tag-other-frame t])
   ["Display feffit keywords"                input-display-keywords t]
   ;;["Reparse feffit file"                    Feffit-parse-data-set t]
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(eval-when-compile (if (locate-library "imenu") (require 'imenu)))
;; the data sets regexp is not really right, it really only is right
;; for data keywords at the beginning of the line and the t in the
;; character class to avoid matching next in next data set is a
;; potentially destructive hack
(defvar Feffit-imenu-generic-expression
  '((nil "\\(^[ \t]*\\|[^%#!*t][ \t]+\\)data[ \t]*[ \t=,][ \t]*\\(\\w+\\)" 2)
    ("Guesses"   "\\bguess[ \t]*[ \t=,][ \t]*\\(\\w+\\)" 1)
    ("Sets"      "\\bset[ \t]*[ \t=,][ \t]*\\(\\w+\\)"   1))
    ;;("Locals"    "\\blocal[ \t]*[ \t=,][ \t]*\\(\\w+\\)" 1))
  "Imenu generic expression for Feffit mode.
See `imenu-generic-expression'.")


(defvar Feffit-mode nil
  "Determine if Feffit minor mode is active.")
(make-variable-buffer-local 'Feffit-mode)

;;;###autoload
(defun turn-on-feffit ()
  "Turn on Feffit minor mode."
  (Feffit-mode t))

;;;###autoload
(defun Feffit-mode (&optional arg)
  "Minor mode for editing input files for *feffit*.
ARG t turns on *feffit* minor mode.

Defined keys in Feffit minor mode:\n \\{Feffit-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Feffit-mode
	       (if (null arg) (not Feffit-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Feffit-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Feffit-mode " Feffit") minor-mode-alist)))
	 (or (assq 'Feffit-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Feffit-mode Feffit-mode-map)
			 minor-mode-map-alist)))
	 ;; Add or remove the menu, and run the hook
	 (if Feffit-mode
	     (progn
	       (make-local-variable 'imenu-generic-expression)
	       (setq imenu-generic-expression Feffit-imenu-generic-expression
		     input-output-files Feffit-output-files
		     input-current-keywords-alist
		     (copy-alist Feffit-keywords-alist)
		     fuse-mouse-highlight-list Feffit-highlight-list)

	       (setq input-program-setguess-flag    t
		     input-program-master-flag      t
		     input-program-logfile-flag     t
		     input-program-stanza-flag      nil
		     input-program-data-flag        t
		     input-program-feff-flag        t
		     input-program-list-flag        nil
		     input-program-kweight-flag     t
		     input-program-eshift-flag      nil
		     input-program-program-author
		        '("Matthew Newville" "newville@cars.uchicago.edu")
		     input-program-parse
		        '(Feffit-parse-data-set Feffit-features-alist)
		     input-program-hilit            'Feffit-set-hilit)

	       ;; this helps movement between paragraphs
	       ;;(make-variable-buffer-local 'paragraph-start)
	       ;;(make-variable-buffer-local 'paragraph-separate)
	       ;;(setq paragraph-start    "[!%#* \t]*$"
	       ;;      paragraph-separate "[!%#* \t]*$")

	       (easy-menu-add Feffit-mode-menu)
	       (cond (input-use-hilit19 (Feffit-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (run-hooks 'Feffit-mode-hook))
	   (easy-menu-remove Feffit-mode-menu))
	 )
	(t
	 (message "Feffit minor mode is only for use with Input major mode"))
	))


;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-feffit)
(run-hooks 'Feffit-load-hook)

;;;============================================================================
;;;
;;; fuse-feffit.el ends here
