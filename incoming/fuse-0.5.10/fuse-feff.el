;;; fuse-feff.el --- minor mode for editing feff input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: Feff.el,v 1.2 1998/03/14 22:45:57 bruce Exp $

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
(require 'cl)
(eval-when-compile
  (defvar Feff-toolbar nil))

(defcustom Feff-mode-hook nil
  "*Hook run when Feff minor mode is entered."
  :group 'fuse-programs
  :type 'hook)
(defcustom Feff-load-hook nil
  "*Hook run when fuse-feff.el is first loaded."
  :group 'fuse-programs
  :type 'hook)

(defvar Feff-mode-map ()
  "Keymap used in Feff minor mode.")

(defvar Feff-keywords-alist 'nil
"This is an associated list of all keywords for feff and their attributes.
See the documentation for `input-current-keywords-alist'.")
(setq Feff-keywords-alist '(
					; standard cards
      ("afolp" .
       (0 "none"
	  "no argument.  Automatically overlap muffin-tins by 10-30%."))
      ("atoms" .
       (0 "none"
	  "no argument, the atoms list begins on the following line"))
      ("control" .  ;; **
       (4 "integer"
	  "four integers (potph, paths, genfmt, ff2chi; 0=off, >0=on)"))
      ("corrections" .
       (2 "number"
	  "two real numbers (real and imaginary corrections in ff2chi)"))
      ("criteria" .
       (2 "number"
	  "two real numbers (curved wave and plane wave criteria)"))
      ("customcriteria" .
       (3 "number"
	  "three real numbers used for importance factors (crit kmin kmax)"))
      ("debye" .
       (2 "number"
	  "two real numbers (temperature and Debye temperature)"))
      ("edge" .
       (2 ("edge" "number")
	  "an edge & an S0^2 value (\"EDGE\" takes strings i.e. \"K\", \"L3\")"))
      ("ellipticity" .
       (4 "number"
	  "four real numbers (ellipticity x y z of incident beam)"))
      ("end" .
       (0 "none"
	  "no argument.  Nothing beyond \"END\" is read from the input file."))
      ("exchange" .
       (3 ("integer" "number" "number")
	  "an integer + two numbers (exchange index, real & imag. corrections)"))
      ("fms" .
       (3 "number"
	  "three real numbers (scmt/full cluster size, kmax for fms)"))
      ("folp" .
       (2 ("integer" "number")
	  "an ipot and a number.  User defined muffin-tin overlap percentage"))
      ("hole" .
       (2 ("integer" "number")
	  "an integer edge code and an S0^2 value (1=K, 4=L3)"))
      ("interstitial" .
       (2  ("integer" "number")
	  "an integer and a number (method of finding and total volume)"))
      ("ion" .
       (2  ("integer" "number")
	  "an integer denoting an ipot and a number denoting its ionization"))
      ("iorder" .
       (1 "integer"
	  "an intger (see setlam) (you better know what you are doing!)"))
      ("jumprm" .
       (0 "none"
	  "no argument (remove muffin-tin discontinuities)"))
      ("ldos" .
       (3 "number"
	  "three real numbers (min & max energy and imag. part for LDOS output)"))
      ("mbconv" .
       (0 "none"
	  "no argument (model excitation spectrum convolution)"))
      ("nabsorbers" .
       (3  ("integer" "integer" "number")
	  "two integers + a number (average over absorbers: iphabs, nabs, rclabs)"))
      ("nemax" .
       (1 "integer"
	  "an integer (number of energy grid points)"))
      ("nleg" .
       (1 "integer"
	  "an integer (maximum number of legs in pathfinder)"))
      ("nogeom" .
       (0 "none"
	  "no argument.  A user-supplied geom.dat file will be used."))
      ("nohole" .
       (0 "none"
	  "no argument (compute xanes in absence of core hole)"))
      ("nstar" .
       (0 "none"
	  "no argument (write nstar.dat file in genfmt)"))
      ("overlap" .
       (1 "integer"
	  "an integer + a list (make potentials when atomic coords are unknown)"))
      ("pcriteria" .
       (2 "integer"
	  "two real numbers (keep and heap criteria)"))
      ("polarization" .
       (3 "number"
	  "three real numbers (x y z of the E vector of the incident beam)"))
      ("potentials" . ;; **
       (0 "none"
	  "no argument, the potentials list begins on the following line"))
      ("print" .  ;; **
       (4 "integer"
	  "four integers (potph, paths, genfmt, ff2chi)"))
      ("rgrid" .
       (1 "number"
	  "a real number (radial grid for potantial and phases calculation)"))
      ("rmax" .  ;; **
       (1 "number"
	  "a real number (maximum path length in pathfinder)"))
      ("rphases" .
       (0 "none"
	  "no argument (use purely real phase shifts"))
      ("rmultiplier" .
       (1 "number"
	  "a real number (volume scaling factor for atom coordinates)"))
      ("scmt" .
       (4  ("integer" "number" "number" "number")
	  "four numbers (# of iterations, ca1, ca2, energy origin)"))
      ("sig2" .
       (1 "number"
	  "a real number (the global Debye-Waller factor)"))
      ("sig3" .
       (1 "number"
	  "a real number (expansion coef. for 1st and 3rd cumulants in ff2chi)"))
      ("spin" .
       (1 "integer"
	  "an integer (central atom spin +/- 1)"))
      ("ss" .
       (4 ("integer" "integer" "number" "number")
	  "single scatt. shell info (index ipot deg rss) (use with \"OVERLAP\")"))
      ("title" .
       (0 "title"
	  "a user-defined comment string"))
      ("xanes" .
       (0 "none"
	  "no argument.  A xanes calculation will be made."))
					; feff7/xanes/matt's additions
      ;;("xion" .
      ;; (1 "readable" "readable filename containing ionizations"))
      ;;("egrid" .
      ;; (5 "special" "number of and values of energy grid knots"))
      ;;("emesh" .
      ;; (4 "special" "energy mesh values"))
      ;;("vint" .
      ;; (1 "number" "a real number (amt. subtr. from init. grid point)"))
      ;;("dcrit" .
      ;; (2 "special" "an integer and a real number"))
      ;;("force" .
      ;; (0 "none" "nothing, \"FORCE\" takes no argument"))
      ;;("rminx" .
      ;; (1 "number" "a real number"))
      ))


(defvar Feff-output-files)
(setq Feff-output-files '())
;;      (list "phase.bin"
;;	    "chi.dat" "files.dat" "list.dat" "misc.dat" "paths.dat"
;;	    "xsect.bin" "xmu.dat" "feff.bin" "potph.dat"
;;	    "xrho.bin" "xatom.bin" "rkk.bin" "xfermi.dat" "geom.dat"))

(defvar Feff-highlight-list
  (list
   '("\\([!%#]+\\|title\\>\\).*\\(<[^>]+>\\)" 2)
   '("^[ \t]control[ \t]+\\([1-9][ \t]*\\)" 1)
   '("^[ \t]control[ \t]+[1-9][ \t]+\\([1-9][ \t]*\\)" 1)
   '("^[ \t]control[ \t]+[1-9][ \t]+[1-9][ \t]+\\([1-9][ \t]*\\)" 1)
   '("^[ \t]control[ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+\\([1-9][ \t]*\\)" 1)
   '("^[ \t]control[ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+\\([1-9][ \t]*\\)" 1)
   '("^[ \t]control[ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+[1-9][ \t]+\\([1-9][ \t]*\\)" 1)
   )
  "This is for finding each of the control flags, up to 6.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template for feff.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Feff-template
  '("TITLE = " p n n
    "HOLE = "  p "\t" p n n
    "*         mphase,mpath,mfeff,mchi" n
    "CONTROL   1      1     1     1" n
    "PRINT     1      0     0     3" n n
    "RMAX" p n n
    "POTENTIALS" n
    "*   ipot   z  label" n p n n
    "ATOMS" n
    "*     x           y           z      ipot" n p n n )
  "Template for an *feff* stanza.  Inserted by \\[Feff-make-template].")

(tempo-define-template "Feff-template" Feff-template
		       nil
		       "Insert an empty Feff template.")
(eval-when-compile  (defun tempo-template-Feff-template))

(defun Feff-make-template ()
  "Write a template for *feff* using a tempo template.
The crystal class will be prompted and entries for the appropriate lattice
parameters will be inserted in the template.  Bound to \\[Feff-make-template]."
  (interactive)
  (tempo-template-Feff-template)
  (fuse-mark-hotspots tempo-marks)
  (setq input-used-tempo-flag t)
  (input-clean-file)
  (cond (input-use-hilit19
	 (funcall input-program-hilit) ;;(nth 1 (cdr (assoc input-program-name
				       ;;input-programs-alist))))
	 (hilit-repaint-command t)))
  (message (substitute-command-keys
	    "Use \\[tempo-forward-mark] and \\[tempo-backward-mark] to move between hotspots.")))

;;; end template for feff.inp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special functions for feff

(defvar Feff-features-alist nil
  "Alist containing information parsed from a feff input file.")
;;(make-variable-buffer-local 'Feff-features-alist)


(defun Feff-parse-file ()
  "Collect title and values of experimental corrections.
This is for use in plotting and other functions."
  (interactive)
  (let (title sigmm qrtmm ampfac mark-1 (case-fold-search t))
    (save-excursion
      (setq Feff-features-alist nil)
					; get first title
      (goto-char (point-min))
      (cond ((re-search-forward "\\<title\\>" (point-max) t)
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point))
	     (end-of-line)
	     (setq title (buffer-substring-no-properties mark-1 (point)))))

					; get experimental dwf correction
      (goto-char (point-max))
      (cond ((re-search-backward "^[ \t]*\\*[ \t]*\\(sum\\|mcmaster\\)"
				 (point-min) t)
	     (search-forward "corrections:")
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point))
	     (re-search-forward input-word-sep)
	     (setq sigmm (buffer-substring-no-properties mark-1 (point)))
	     (re-search-forward input-word-sep)
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point))
	     (re-search-forward input-word-sep)
	     (setq qrtmm (buffer-substring-no-properties mark-1 (point)))))

					; get experimental amplitude correction
      (goto-char (point-max))
      (cond ((re-search-backward "^[ \t]*\\*.*amplitude factor" (point-min) t)
	     (forward-word 5)
	     (re-search-forward input-word-sep)
	     (setq mark-1 (point))
	     (end-of-line)
	     (setq ampfac (buffer-substring-no-properties mark-1 (point)))))

      (setq Feff-features-alist
	    (append
	     (list (cons "title"   title))
	     (list (cons "sigmm"   sigmm))
	     (list (cons "qrtmm"   qrtmm))
	     (list (cons "ampfac"  ampfac))))
      (if (interactive-p)
	  (message "Parsed %s input file: %s"
		   input-program-name (file-name-nondirectory buffer-file-name)))
      )))

(defun Feff-control-list ()
  "Return a list of control card values.
This returns a six element list to support feff8."
  (let* ((case-fold-search t)
	 (arg "[ \t]+\\([0-9]\\)")
	 (expr (concat "^[ \t]*control" arg arg arg arg
		       "\\(" arg "\\)?\\(" arg "\\)?")) )
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward expr (point-min) t)
	  (list (string-to-number (or (match-string 1) "0"))
		(string-to-number (or (match-string 2) "0"))
		(string-to-number (or (match-string 3) "0"))
		(string-to-number (or (match-string 4) "0"))
		(string-to-number (or (match-string 6) "0"))
		(string-to-number (or (match-string 8) "0")))
	(list 0 0 0 0 0 0)
	))))


(defun Feff-print-list ()
  "Return a list of print card values.
This returns a six element list to support feff8."
  (let* ((case-fold-search t)
	 (arg "[ \t]+\\([0-9]\\)")
	 (expr (concat "^[ \t]*print" arg arg arg arg
		       "\\(" arg "\\)?\\(" arg "\\)?")) )
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward expr (point-min) t)
	  (list (string-to-number (or (match-string 1) "0"))
		(string-to-number (or (match-string 2) "0"))
		(string-to-number (or (match-string 3) "0"))
		(string-to-number (or (match-string 4) "0"))
		(string-to-number (or (match-string 6) "0"))
		(string-to-number (or (match-string 8) "0")))
	(list 0 0 0 0 0 0)
	))))


(defun Feff-control-p ()
  "Return t if point is in a CONTROL line."
  (let ((case-fold-search t))
    (save-excursion
      (back-to-indentation)
      (looking-at "control"))))

(defun Feff-swap-control ()
  "Toggle the CONTROL flag under point.
The function `input-set-and-jump' (\\[input-set-and-jump]) will run this
function if appropriate.  Bound to \\[Feff-swap-control]"
  (interactive)
  (let ((word (input-this-word)))
    (if (Feff-control-p)
	(cond ((string-match "0+" word)
	       (delete-char (* -1 (length word)))
	       (insert "1"))
	      ((string-match "[0-9]+" word)
	       (delete-char (* -1 (length word)))
	       (insert "0"))
	      (t
	       (message "Point is not under a CONTROL flag.")))
      (message "This is not a CONTROL line."))))



;; leave this hear just so things don't break
(defun Feff-set-control-properties ()
  nil)

;; in the following, need to use general name for input file
;; an alist that gets evaluated upon reading file would be nice

(defun Feff-jump-to-misc ()
  "Display the misc.dat file in another buffer.
Bound to \\[Feff-jump-to-misc]"
  (interactive)
  (let ((file "misc.dat"))
    (if (file-readable-p file) (input-jump-to-file file)
      (message "%S does not exist.  Have you run feff yet?"
	       (file-relative-name file "./")))))

(defun Feff-jump-to-files ()
  "Display the files.dat file in another buffer.
Bound to \\[Feff-jump-to-files]"
  (interactive)
  (let ((file "files.dat"))
    (if (file-readable-p file) (input-jump-to-file file)
      (message "%S does not exist.  Have you run feff yet?"
	       (file-relative-name file "./")))))

(defun Feff-jump-to-list ()
  "Display the list.dat file in another buffer.
Bound to \\[Feff-jump-to-list]"
  (interactive)
  (let ((file "list.dat"))
    (if (file-readable-p file) (input-jump-to-file file)
      (message "%S does not exist.  Have you run feff yet?"
	       (file-relative-name file "./")))))

(defun Feff-jump-to-paths ()
  "Display the paths.dat file in another buffer.
Bound to \\[Feff-jump-to-paths]"
  (interactive)
  (let ((file "paths.dat"))
    (if (file-readable-p file) (input-jump-to-file file)
      (message "%S does not exist.  Have you run feff yet?"
	       (file-relative-name file "./")))))

(defun Feff-display-log ()
  "Display results of feff run.
Feff does not really have a log file.  For feff8, show the convergence
data, otherwise show the results of intrp."
  (interactive)
  (if (string= input-program-version "8")
      (Feff-8-jump-to-convergence)
    (Feff-display-intrp)))

(defun Feff-display-intrp ()
  "Display output of *intrp*.
This is a perl script for pretty-printing the
contents of paths.dat, and files.dat in its own buffer, usually called
intrp.dat.  Bound to \\[Feff-display-intrp]"
  (interactive)
  (let ((orig (current-buffer)) mark-2)
    (cond ((and (file-exists-p "files.dat")
		(file-exists-p "paths.dat"))
	   (get-buffer-create input-intrp-buffer-name)
	   (call-process "intrp" nil input-intrp-buffer-name nil
			 (concat "-s " input-intrp-args))
	   (switch-to-buffer input-intrp-buffer-name)
	   (goto-char (point-max))
	   (while (not (bobp))
	     (cond ((re-search-backward "^[ \t]*Unknown option:"
					(point-min) "to_limit")
		    (setq mark-2 (point-marker))
		    (beginning-of-line 2)
		    (delete-region mark-2 (point)))) )
	   (goto-char (point-min))
	   (setq input-originating-buffer orig)
	   (message
	    (substitute-command-keys
	     "\\[input-back-to-original] to return to previous buffer.")))
	  (t
	   (message "intrp requires both paths.dat and files.dat"))) ))



(defun Feff-clean-potentials-entry ()
  "Insert proper indentation and column separation in the potentials list.
Care is taken not to go over the end of the line."
  (let (begin (ipot 0) (znumber 0) label indent space)
    (setq
     indent (input-make-indent-string
	     (+ input-feff-indent input-potentials-indent) nil))
    (setq space (input-make-indent-string input-potentials-separate t))

    (beginning-of-line)
    (delete-horizontal-space)
    (setq begin (point))
    (forward-word 1)
    (setq ipot (string-to-number (buffer-substring-no-properties begin (point))))
    (delete-horizontal-space)
    (setq begin (point))
    (forward-word 1)
    (setq znumber (string-to-number (buffer-substring-no-properties begin (point))))
    (delete-horizontal-space)
    (setq begin (point))
    (end-of-line)
    (setq label (buffer-substring-no-properties begin (point)))

    (setq begin (point))
    (beginning-of-line)
    (delete-region begin (point))

    (insert (format "%s%1d%s%2d%s%s" indent ipot space znumber space label))
    ))


;; this is a catastrophe!
(defun Feff-clean-atoms-entry ()
  "Insert proper indentation and column separation in the atoms list.
Care is taken not to go over the end of the line."
  (interactive)
  nil)
;;   (let (indent space eol)
;; ;;(x 0) (y 0) (z 0) (r 0) (ipot 0) label bol)
;;     (setq indent (input-make-indent-string (+ input-feff-indent
;; 					      input-list-indent) nil))
;;     (setq space  (input-make-indent-string input-atoms-separate  t))
;;
;;     (end-of-line)
;;     (setq eol (point-marker))
;;     (back-to-indentation)
;;     (delete-horizontal-space)
;;     (insert indent)
;; 					; x coordinate
;;     (re-search-forward input-word-sep eol t)
;;     (delete-horizontal-space)
;;     (insert space)
;; 					; y coordinate
;;     (re-search-forward input-word-sep eol t)
;;     (delete-horizontal-space)
;;     (insert space)
;; 					; z coordinate
;;     (re-search-forward input-word-sep eol t)
;;     (delete-horizontal-space)
;;     (insert space)
;; 					; ipot
;;     (re-search-forward input-word-sep eol t)
;;     (delete-horizontal-space)
;;     (insert space)
;;
;; 					; label and r
;;     (cond ((re-search-forward input-word-sep eol t)
;; 	   (delete-horizontal-space)
;; 	   (insert space)
;; 	   (cond ((re-search-forward input-word-sep eol t)
;; 		  (delete-horizontal-space)
;; 		  (insert space)))))
;;
;;     ;;               i  x s y  s z  s ip s lab  s   r
;;     ;;(insert (format "%s%9s%s%9s%s%9s%s%2s%s%-10s%s  %8s"
;; 		    ;;indent x space y space z space ipot space
;; 		    ;;label space r))
;;     ))


(defun Feff-radial (x y z)
  (setq x (string-to-number x)
	y (string-to-number y)
	z (string-to-number z))
  (format "%8.5" (sqrt (+ (* x x) (* y y) (* z z)))) )

(defvar Feff-comment-alist nil
  "Comment alist used for feff input files and comment mode.")
(setq Feff-comment-alist
      (append input-saved-comment-alist '((input-mode ?* 1 nil))))

(defun Feff-comment-out-region (begin end &optional arg)
  "This is a wrapper around `comment-out-region'.
Feff, unlike the other programs, requires that the comment character be
\"*\".  Also need to handle `input-feff-indent'.  BEGIN and END
are typically point and mark.  ARG non-nil uncomments."
  (interactive "*r\nP")
  (let ((alist (copy-alist input-saved-comment-alist))
	(b (min begin end))
	(e (save-excursion (goto-char (max begin end)) (point-marker))) )
    (setq comment-mode-alist (copy-alist Feff-comment-alist))
    (when arg                           ; clean up leading whitespace
      (goto-char b)
      (while (<= (point) e)
	(beginning-of-line)
	(delete-horizontal-space)
	(forward-line 1)))
    (comment-out-region b e arg)        ; do the work
    (unless arg                         ; insert indentation
      (goto-char b)
      (while (<= (point) e)
	(beginning-of-line)
	(delete-horizontal-space)
	(insert (input-make-indent-string
		 (+ input-comment-indent input-feff-indent) nil))
	(forward-line 1)) )
    (forward-line -1)
    (setq comment-mode-alist (append alist input-comment-list)) ))

;;; end of special functions for feff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special functions for feff8

;; Feff-8-write-convergence-data can be used this way in the .fuse file:
;;    (add-hook 'input-after-run-hook
;;              '(lambda ()
;;                 (and (string= input-program-name "feff")
;;                      (string= input-program-version "8")
;;                      (< (elt (Feff-control-list) 0) 1)
;;                      (Feff-8-write-convergence-data))))

(defun Feff-8-write-convergence-data ()
  "Write convergence and electronics data.
This constructs a file with some header information to identify the
feff8 run and a two column list of self-consistency loop index and
Fermi energy.  The information about charge transfer and orbital
occupancy is included in the file.  This forces a save of lots of
backup copies so that convergence studies can be made without
accidentally loosing the data."
  (interactive)
  (let (begin electronics (values ()) (buffer (current-buffer))
	      (case-fold-search t) (ii 0) (cards "\n") (counter 0)
	      (card-list (list "afolp" "scmt" "fms" "rmax" "ldos")))
    ;;            ^^^ names of cards to include in file header
    (if (and (string= input-program-name "feff")
	     (string= input-program-version "8"))
	(progn                ; first get values of Efermi at each iteration
	  (set-buffer input-program-buffer)
	  (goto-char (point-max))
	  (search-backward "")
	  (re-search-forward "mu_\\(new\\|old\\)" (point-max) "to_limit")
	  (while (not (eobp))
	    (re-search-forward "\\([-.0-9]+\\)\\>" (point-max) "to_limit")
	    (setq values (append values (list (match-string 0))))
	    (re-search-forward "mu_\\(new\\|old\\)" (point-max) "to_limit"))
					; now get electronics information
	  (search-backward "electronic" (point-min) "to_limit")
	  (setq begin (point-marker))
	  (search-forward "calculating" (point-max) "to_limit")
	  (beginning-of-line)
	  (setq electronics (buffer-substring-no-properties begin (point)))
	  (set-buffer buffer)        ; back to feff input file
	  (if values                 ; only continue if some values found
	      (progn
		(save-excursion	     ; now get card-list lines
		  (while (< counter (length card-list))
		    (goto-char (point-max))
		    (let ((expr (concat "^[ \t]*" (elt card-list counter))))
		      (if (re-search-backward expr (point-min) t)
			  (progn
			    (setq begin (point-marker))
			    (end-of-line)
			    (setq cards (concat cards
						(buffer-substring-no-properties
						 begin (point)) "\n"))
			    )))
		    (setq counter (1+ counter))))
				     ; now write the file
		(find-file Feff-8-convergence-filename)
		(erase-buffer)
		(if (file-exists-p "misc.dat")
		    (insert-file-contents "misc.dat"))
		(goto-char (point-max))
		(insert cards (make-string 70 ?-) "\n")
		(insert electronics "\n" (make-string 70 ?-) "\n")
		(insert "   index     Efermi\n")
		(comment-out-region (point-min) (point) 1)
		(while (< ii (length values))
		  (insert (format "     %3i       %s\n"
				  ii (elt values ii)))
		  (setq ii (1+ ii)))
		(let ((version-control "numbered")
		      (kept-new-versions 100)
		      (kept-old-versions 100))
		  (save-buffer))
		(if (interactive-p)
		    (setq input-originating-buffer buffer)
		  (kill-buffer (current-buffer)))
		(message "Wrote convergence data: %S"
			 Feff-8-convergence-filename)
		)
	    (message "No convergence data found in %S" input-program-buffer)
	    )))))

(defun Feff-8-jump-to-convergence ()
  "Display the convergence.dat file in another buffer.
Bound to \\[Feff-8-jump-to-convergence]"
  (interactive)
  (let ((file "convergence.dat"))
    (if (file-readable-p file) (input-jump-to-file file)
      (message "%S does not exist.  Have you run feff8 yet?"
	       (file-relative-name file "./")))))


(defun Feff-8-parse-datafile ()
  "Parse a data file name from the current feff.inp file.
This returns the data file name or nil if there is no data keyword.
This is useful for comparing xanes calculations to data, but can also be
used to compare chi(k) data to a chi(k) calculation.  The data file is on
a comment line with the keyword \"DATA\" as the first thing after the
comment character.  The data file name can be enclosed in < > for the sake
of \\[input-set-and-jump] and the angle brackets will then be stripped."
  (interactive)
  (save-excursion
    (let ((case-fold-search t) word)
      (goto-char (point-max))
      (if (re-search-backward "^[ \t]*\\*[ \t]*data" (point-min) t)
	  (progn
	    (forward-word 2)
	    (setq word (input-this-word))
	    (if (string= "<" (substring word 0 1))
		(setq word (substring word 1)))
	    (if (string= ">" (substring word -1))
		(setq word (substring word 0 -1)))
	    word)
	nil))))

;; this is a rather grungy function...a lot of things just have to be
;; fixed by hand
;; need a function to revert the buffer also
(defun Feff-instrument-feff-8 ()
  "Instrument the buffer for feff8 functionality.

This function does several things:
  1. set `input-program-version' to \"8\"
  2. bind feff8 call-backs to the toolbar
  3. set key-bindings for feff8 functions
  4. fix keyword descriptions for keywords that changed in feff8
  5. fix the mode line to indicate that feff8 functionality is set."
  (interactive)
  (cond ((and (string= input-program-name "feff")
	      (string-match "8" (elt (assoc 'Feff-mode minor-mode-alist) 1)))
	 (message "This buffer is already outfitted for feff8."))
	((string= input-program-name "feff")
	 (let ((toolbar
		'([fuse-toolbar-template-xpm Feff-toolbar-template
					     t "Write a Feff template"]
		  [fuse-toolbar-run-xpm      Feff-toolbar-run
					     t "Run Feff"]
		  [fuse-toolbar-log-xpm      Feff-toolbar-log
					     t "Look at convergence.dat"]
		  [Feff-toolbar-xmu-xpm      Feff-toolbar-xmu
					     t "Plot mu(E) and mu0(E)"]
		  [Feff-toolbar-dos-xpm      Feff-toolbar-dos
					     t "Plot LDOS"]
		  [fuse-toolbar-helper-xpm   Feff-toolbar-helper
					     t "Display Feff keywords"]
		  [fuse-toolbar-document-xpm Feff-toolbar-document
					     t "Read the Feff document"]
		  [fuse-toolbar-quit-xpm     Feff-toolbar-quit
					     t "Quit editing this input file"] )))
					; set the program version
	   (input-set-version "8")
					; fix the toolbar
	   (when (and (featurep 'toolbar)
		      (string-match "XEmacs" emacs-version))
	     (set 'Feff-toolbar toolbar)
	     (fuse-make-toolbar-function Feff-toolbar)
	     (fset 'Feff-toolbar-log 'Feff-8-jump-to-convergence)
	     (fset 'Feff-toolbar-xmu 'Feff-8-plot-xmu)
	     (fset 'Feff-toolbar-dos 'Feff-8-plot-rho))
					; fix key bindings
	   (define-key Feff-mode-map "\C-c\C-px"  'Feff-8-plot-xmu)
	   (define-key Feff-mode-map "\C-c\C-pd"  'Feff-8-plot-rho)
					; fix keyword descriptions
	   (nsubstitute
	    '("control" 6 "integer"
	      "six integers (pot, ph, fms, paths, genfmt, ff2chi; 0=off, >0=on)")
	    (assoc "control" input-current-keywords-alist)
	    input-current-keywords-alist)
	   (nsubstitute
	    '("print" 6 "integer"
	      "six integers (pot, ph, fms, paths, genfmt, ff2chi)")
	    (assoc "print" input-current-keywords-alist)
	    input-current-keywords-alist)
	   (nsubstitute
	    '("rmax" 2 "number"
	      "2 real numbers (max. path length in fms and pathfinder)")
	    (assoc "rmax" input-current-keywords-alist)
	    input-current-keywords-alist)
	   ;; potential list format changed, too
					; fix the mode line
	   (nsubstitute '(Feff-mode " Feff8")
			(assoc 'Feff-mode minor-mode-alist) minor-mode-alist)

	   (message "Buffer outfitted for feff8.")
	   ))
	(t
	 (message "This buffer does not contain a feff input file."))))

;;; end of special functions for feff8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colorization regular expressions

;;    (setq my-expr (make-regexp '("hole" "exchange" "ion" "folp"
;;				 "rmax" "debye" "rmultiplier" "ss"
;;				 "nleg" "criteria" "nogeom" "csig"
;;				 "iorder" "pcriteria" "sig2" "xanes"
;;				 "corrections" "afolp" "nemax"
;;				 "polarization" "ellipticity"
;;				 "customcrit" "rgrid" "rphases"
;;				 "nstar" "nohole" "sig3" "jumprm"
;;				 "mbconv" "spin" "xion" "egrid"
;;				 "emesh" "vintfix" "force" "minx" "dcrit"
;;                               "fms"   "ldos" "interstitial" "scmt")))


(defconst Feff-font-lock-keywords
  '(
					; comments
    ("\\*.*$" . font-lock-comment-face)
					; titles
    ("^[ \t]*title.*$" . font-lock-string-face)
					; lists
    ("^[ \t]*\\(atoms?\\|potentials?\\|overlap\\)\\>" .
     font-lock-function-name-face)
;; 					; CONTROL
;;     ("^[ \t]*control\\([ \t]+[0-9]\\)+[ \t]*" . Feff-control-face)
					; run control works
    ("^[ \t]*\\(control\\|end\\|print\\)\\>" . font-lock-type-face)
					; keywords
    ("^[ \t]*\\(afolp\\|c\\(orrections\\|riteria\\|sig\\|ustomcrit\\)\\|d\\(crit\\|ebye\\)\\|e\\(dge\\|grid\\|llipticity\\|mesh\\|xchange\\)\\|f\\(ms\\|o\\(lp\\|rce\\)\\)\\|hole\\|i\\(nterstitial\\|o\\(n\\|rder\\)\\)\\|jumprm\\|ldos\\|m\\(bconv\\|inx\\)\\|n\\(emax\\|legs?\\|o\\(geom\\|hole\\)\\|star\\)\\|p\\(criteria\\|olarization\\)\\|r\\(grid\\|m\\(ax\\|ultiplier\\)\\|phases\\)\\|s\\(cmt\\|ig[23]\\|pin\\|s\\)\\|vintfix\\|x\\(anes\\|ion\\)\\)\\>" . font-lock-keyword-face) ))


(defconst Feff-font-lock-keywords-1 nil)
(setq Feff-font-lock-keywords-1   Feff-font-lock-keywords)
(defconst Feff-font-lock-keywords-2 nil)
(setq Feff-font-lock-keywords-2   Feff-font-lock-keywords)

(defun Feff-set-hilit ()
  "Call hilit-set-mode-patterns for *feff*."
  (interactive)
  (cond (input-use-hilit19

	 (hilit-set-mode-patterns

	  'input-mode
	  '(
					; comments -- type comment
	    ("[%!#*].*$" nil comment)
	    ("^[ \t]*\\*.*$" nil comment)
					; titles -- type define
	    ("^[ \t]*title.*$" nil define)
					; atom, basis, end
	    ("^[ \t]*\\(atoms?\\|potentials?\\|overlap\\)\\>" nil include)
					; run control keywords
	    ("^[ \t]*\\(print\\|control\\|end\\)\\>" nil glob-struct)
					; all the rest
	    ("^[ \t]*\\(afolp\\|c\\(orrections\\|riteria\\|sig\\|ustomcrit\\)\\|d\\(crit\\|ebye\\)\\|e\\(dge\\|grid\\|llipticity\\|mesh\\|xchange\\)\\|f\\(ms\\|o\\(lp\\|rce\\)\\)\\|hole\\|i\\(nterstitial\\|o\\(n\\|rder\\)\\)\\|jumprm\\|ldos\\|m\\(bconv\\|inx\\)\\|n\\(emax\\|legs?\\|o\\(geom\\|hole\\)\\|star\\)\\|p\\(criteria\\|olarization\\)\\|r\\(grid\\|m\\(ax\\|ultiplier\\)\\|phases\\)\\|s\\(cmt\\|ig[23]\\|pin\\|s\\)\\|vintfix\\|x\\(anes\\|ion\\)\\)\\>" nil keyword)
	    ) nil 'case-insensitive))
	(input-use-font-lock
	 (setq input-font-lock-keywords   Feff-font-lock-keywords)
	 (setq input-font-lock-keywords-1 Feff-font-lock-keywords-1)
	 (setq input-font-lock-keywords-2 Feff-font-lock-keywords-2)) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for feff
(if Feff-mode-map
    ()
  (setq Feff-mode-map (make-sparse-keymap))
  (define-key Feff-mode-map "\C-c\C-tt"  'Feff-make-template)
  (define-key Feff-mode-map "\C-c\C-px"  'Feff-plot-xmu)
  (define-key Feff-mode-map "\C-c\C-pc"  'Feff-plot-chi)
					; looking at files
  (define-key Feff-mode-map "\C-c\C-fm"  'Feff-jump-to-misc)
  (define-key Feff-mode-map "\C-c\C-ff"  'Feff-jump-to-files)
  (define-key Feff-mode-map "\C-c\C-fs"  'Feff-jump-to-list)
  (define-key Feff-mode-map "\C-c\C-fp"  'Feff-jump-to-paths)
  (define-key Feff-mode-map "\C-c\C-fi"  'Feff-display-intrp)

  (define-key Feff-mode-map "\C-c\C-sd"  'Feff-swap-control)

  (define-key Feff-mode-map "\C-c;"      'Feff-comment-out-region)
					; feff8 stuff
  (define-key Feff-mode-map "\C-c\C-ew"  'Feff-8-write-convergence-data)
  (define-key Feff-mode-map "\C-c\C-ec"  'Feff-8-jump-to-convergence)
  (define-key Feff-mode-map "\C-c\C-ep"  'Feff-8-plot-convergence)
  (define-key Feff-mode-map "\C-c\C-ed"  'Feff-8-plot-rho)
  (define-key Feff-mode-map "\C-c\C-ex"  'Feff-8-plot-xmu)

  (define-key Feff-mode-map "\C-c8"      'Feff-instrument-feff-8)
  )
(defvar Feff-mode-menu nil)
(easy-menu-define
 Feff-mode-menu Feff-mode-map
 "Menu used in Feff mode"
 '("Feff"
   ["Make template"                     Feff-make-template t]
   ["Display feff keywords"             input-display-keywords t]
   ("Look at output files"
    ["Display intrp"                    Feff-display-intrp t]
    ["Look at misc.dat"                 Feff-jump-to-misc
     :active (file-exists-p "misc.dat")]
    ["Look at files.dat"                Feff-jump-to-files
     :active (file-exists-p "files.dat")]
    ["Look at list.dat"                 Feff-jump-to-list
     :active (file-exists-p "list.dat")]
    ["Look at paths.dat"                Feff-jump-to-paths
     :active (file-exists-p "paths.dat")])
   ("Functions for feff8"
    ["Get convergence data"             Feff-8-write-convergence-data
     :active (and (get-buffer input-program-buffer)
		  (string= input-program-version "8"))]
    ["Jump to convergence data file"    Feff-8-jump-to-convergence
     :active (and (file-exists-p Feff-8-convergence-filename)
		  (string= input-program-version "8"))]
    ["Plot convergence data"            Feff-8-plot-convergence
     :active (and (file-exists-p Feff-8-convergence-filename)
		  (string= input-program-version "8"))]
    ["Plot xanes"                       Feff-8-plot-xmu
     :active (and (file-exists-p "xmu.dat")
		  (string= input-program-version "8"))]
    ["Plot density of states"           Feff-8-plot-rho
     :active (string= input-program-version "8")]
    "---"
    ["Instrument buffer for feff 8"     Feff-instrument-feff-8 t] )
   ["Clean up feff input file"          input-clean-file t]
   ["Toggle control flag under point"   Feff-swap-control t] ;;)
   ["--------- Running ---------------" input-t t]
   ["Run feff, this file"               input-run-this-program-this-file t]
   ["Kill feff run"                     input-kill-program
    :active (get-process input-process)]
   ["--------- Plotting --------------" input-t t]
   ["Plot xmu.dat"                      Feff-plot-xmu
     :active (file-exists-p "xmu.dat")]
   ["Plot chi.dat"                      Feff-plot-chi
     :active (file-exists-p "chi.dat")]
   ["Set k-weght"                       input-set-k-weight t]
   ["Toggle gnuplot terminal"           input-toggle-gnuplot-terminal t]
   ;;["Reparse file"                      Feff-parse-file t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Feff-imenu-generic-expression nil
  "Imenu generic expression for Feff mode.  See `imenu-generic-expression'.")

(defvar Feff-mode nil
  "Determines if Feff minor mode is active.")
(make-variable-buffer-local 'Feff-mode)



;;;###autoload
(defun turn-on-feff ()
  "Turn on Feff minor mode."
  (Feff-mode t))

;;;###autoload
(defun Feff-mode (&optional arg)
  "Minor mode for editing input files for *feff*.
ARG t turns on *feff* minor mode.

Defined keys in Feff minor mode:\n \\{Feff-mode-map}"
  (interactive "P")
  (cond ((string= "Input" mode-name)
	 (setq Feff-mode
	       (if (null arg) (not Feff-mode)
		 (> (prefix-numeric-value arg) 0)))
	 (or (assq 'Feff-mode minor-mode-alist)
	     (setq minor-mode-alist
		   (cons '(Feff-mode " Feff") minor-mode-alist)))
	 (or (assq 'Feff-mode minor-mode-map-alist)
	     (setq minor-mode-map-alist
		   (cons (cons 'Feff-mode Feff-mode-map)
			 minor-mode-map-alist)))
	 (if Feff-mode
	     (progn
	       (make-local-variable 'comment-start)
	       (setq comment-start (concat "*" input-mode-variable-comment))
	       (make-variable-buffer-local 'Feff-8-convergence-filename)
	       (make-local-variable 'imenu-generic-expression)
	       (setq imenu-generic-expression Feff-imenu-generic-expression
		     input-output-files Feff-output-files
		     fuse-mouse-highlight-list Feff-highlight-list)

	       (setq input-program-setguess-flag    nil
		     input-program-master-flag      nil
		     input-program-logfile-flag     t
		     input-program-stanza-flag      nil
		     input-program-data-flag        nil
		     input-program-feff-flag        t
		     input-program-list-flag        nil
		     input-program-kweight-flag     t
		     input-program-eshift-flag      t
		     input-program-program-author
		        '("John Rehr" "jjr@phys.washington.edu")
		     input-program-parse '(nil nil)
		     input-program-hilit            'Feff-set-hilit)

	       (easy-menu-add Feff-mode-menu)
	       (setq input-current-keywords-alist
		     (copy-alist Feff-keywords-alist))
	       (cond (input-use-hilit19 (Feff-set-hilit)))
	       (and (string-match "XEmacs" emacs-version)
		    (require 'fuse-toolbar))
	       (add-hook 'local-write-file-hooks
 			 '(lambda () (untabify (point-min) (point-max))))
	       (let ((vers (input-parse-for-value "input-program-version")))
		 (when (and (stringp vers)
			    (string-match "8" vers))
		       (Feff-instrument-feff-8)))
	       (run-hooks 'Feff-mode-hook))
	   (easy-menu-remove Feff-mode-menu))
	 )
    	(t
	 (message "Feff minor mode is only for use with Input major mode"))
	))

;;; Run hook and provide ------------------------------------------------------
(provide 'fuse-feff)
(run-hooks 'Feff-load-hook)

;;;============================================================================
;;;
;;; fuse-feff.el end here
