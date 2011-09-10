;;; fuse-gnuplot.el --- plotting code for FUSE using gnuplot

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: $

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
;;  This file contains code for interfacing FUSE to gnuplot.  The only
;;  relevant code in the minor mode source code files are the key and
;;  menu bindings and the file parsing functions.  The reason for
;;  segregating all the plotting code into its own file is to
;;  facilitate future development of interfaces to other plotting
;;  programs.  Since these functions have the gnuplot syntax hardwired
;;  into them, it would be easiest to make a similarly hardwired set
;;  of functions for some other plotter which have the same names as
;;  these functions (i.e. Autobk-plot-bkg, and so on) and simply drop
;;  the new code in place of this one.
;;
;;  This file is used by putting a (require 'fuseplot) line in any
;;  minor mode that uses plotting.  Functions for plotting specific to
;;  the minor mode should then go into this file.

;;; Change-log:

;;; Code:

(require 'cl)
;;(require 'input)
(require 'gnuplot)
(eval-when-compile
  (defvar Autobk-features-alist)
  (defvar Feff-features-alist)
  (defvar Feffit-features-alist)
  (defvar Phit-features-alist)
  (defvar Normal-features-alist)
  (defvar Correct-features-alist)
  (defvar gnuplot-script-buffer-name)
  (defvar input-gnuplot-terminal)
  (defvar Feffit-marked-paths)        )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general plotting code for FUSE using gnuplot

;; need to check for uwxafs

;; eventually get rid of optional plot argument
(defun input-send-to-gnuplot (lines plot)
  "This is the workhorse for the gnuplot interface.
The argument, LINES, contains all of the lines that comprise the
gnuplot script.  This function loads the gnuplot mode, open the
gnuplot buffer, fires up gnuplot, write the LINES to the script, and
sends the script to gnuplot.  Note that LINES should have carriage
returns in it.  If PLOT is non-nil then the lines are sent to the
*gnuplot* process."
  (let ((input-buffer (current-buffer)) (frame (selected-frame))
	(xemacsp (string-match "XEmacs" emacs-version))
	;; this rather complicated bit is for putting the gnuplot
	;; script in its own frame or in the run frame
	(plist      (cond ((equal input-use-frames 'own)
			   input-gnuplot-frame-plist)
			  ((equal input-use-frames 'share)
			   input-run-frame-plist)))
	(parameters (cond ((equal input-use-frames 'own)
			   input-gnuplot-frame-parameters)
			  ((equal input-use-frames 'share)
			   input-run-frame-parameters)))
	(display    (cond ((equal input-use-frames 'own)
			   input-gnuplot-frame)
			  ((equal input-use-frames 'share)
			   input-run-frame)
			  (t nil))                ))
    (if input-use-frames		; display gp script in another frame?
	(progn				; use run frame
	  (if (or (not display)
		  (not (frame-live-p display)))
	      (progn
		(setq display
		      (if xemacsp
			  (make-frame plist)
			(make-frame parameters)))
		(select-frame display)
		(switch-to-buffer gnuplot-script-buffer-name)
		;;(set-window-dedicated-p (selected-window) t)
		(raise-frame display)
		))
	  (if input-always-raise-flag (raise-frame display))
	  (select-frame display)
	  (switch-to-buffer gnuplot-script-buffer-name)
	  ;;(set-window-dedicated-p (selected-window) t)
	  )
      (switch-to-buffer-other-window gnuplot-script-buffer-name))
    (or (featurep 'gnuplot)              (require 'gnuplot))
    (or (equal major-mode 'gnuplot-mode) (gnuplot-mode))
    (goto-char (point-max))
					; delete previous script or...
    (cond ((search-backward "#-#-#-" (point-min) t)
	   (forward-line 1)
	   (delete-region (point) (point-max)))
	  (t                            ; ...write script header
	   (insert (input-plot-message))))
    (insert lines)                      ; write the script
					; plot it and send control
    (if plot (gnuplot-send-buffer-to-gnuplot))  ; back to input file buffer
    (if input-use-frames		; display run in another frame?
	(progn
	  (select-frame frame)
	  (switch-to-buffer input-buffer)
	  (bury-buffer gnuplot-script-buffer-name)
	  (setq input-gnuplot-frame display)
	  (and (equal input-use-frames 'share)
	       (not input-run-frame)
	       (setq input-run-frame display)))
      (switch-to-buffer-other-window input-buffer)) ))

;; this is a function rather than a constant so that the command keys
;; get inserted properly
(defun input-plot-message ()
  "Return a string to write to the top of the gnuplot script buffer."
  (concat
   "#-*-gnuplot-*-\n"
   (substitute-command-keys
    "# \"\\[gnuplot-send-buffer-to-gnuplot]\" = plot buffer, ")
   (substitute-command-keys
    "\"\\[gnuplot-send-region-to-gnuplot]\" = plot region, ")
   (substitute-command-keys
    "\"\\[gnuplot-send-line-to-gnuplot]\" = plot line")
   "\n\nset data style " input-gnuplot-data-style
   "\nset timestamp\nset autoscale\n#set xrange [ : ]\n#set yrange [ : ]\n\n"
   "# Everything below the dashed line will be erased next plot\n"
   "#-#-#------------------------------------------------\n"))

(defun input-check-gnuplot (input-buffer script-buffer gnuplot-buffer)
  "Issue a message telling how to check plot.
INPUT-BUFFER, SCRIPT-BUFFER, and GNUPLOT-BUFFER are the names of the
buffers containing the input file, the gnuplot script and the gnuplot
process respectively.  This is a dumbed down version of a function for
determining whether a plot was successfully drawn."
;; Check to see if gnuplot successfully plotted what it was told to plot.
;; The last line in the gnuplot buffer should the last line in the script
;; buffer preceded by the gnuplot prompt.  If gnuplot had trouble, switch to
;; the gnuplot buffer in the other window."
;;    Need to (sit-for (* 5 input-pause)) before calling this to give gnuplot
;; time enough to do its thing."
  (message "Check gnuplot messages with %s"
	   (substitute-command-keys "\\[input-jump-to-gnuplot-buffer]")))

;;   (let ((check nil)
;; 	(mark-1 nil)
;; 	(gnuplot-cursor "\\(gnuplot>\\|>\\)[ \t]*"))
;;     (switch-to-buffer-other-window script-buffer)
;;     (goto-char (point-max))
;;     (forward-line -1)
;;     (setq mark-1 (point))
;;     (end-of-line)
;;     (setq check (buffer-substring-no-properties mark-1 (point)))
;;     (setq check (concat gnuplot-cursor check))
;;     (switch-to-buffer gnuplot-buffer)
;;     (goto-char (point-max))
;;     (beginning-of-line)
;;     ;;(forward-line -1)
;;     (cond ((looking-at (eval check))
;; 	   (switch-to-buffer script-buffer)
;; 	   (switch-to-buffer-other-window input-buffer))
;; 	  (t
;; 	   (switch-to-buffer script-buffer)
;; 	   (switch-to-buffer-other-window input-buffer)
;; 	   ;;(switch-to-buffer-other-window gnuplot-buffer)
;; 	   ;;(goto-char (point-max))))
;;     ))

;;;; end of general plotting code for FUSE using gnuplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; script building utilities

(defun input-gnuplot-set-terminal (term)
  "Return a string specifying the terminal type for gnuplot.
TERM is either postscript or the default, `input-gnuplot-default-terminal'.
This function was written assuming that the default is a screen display
terminal type, on successive screen displays, the terminal type is
not explicitly set."
  (cond ((string= term "postscript")
	 (format "set output '%s'\nset terminal %s eps mono\n"
		 input-gnuplot-postscript-file input-gnuplot-terminal))
	((not (string= input-current-terminal term))
	 (format "set terminal %s\n" input-gnuplot-terminal))
	(t ""))  )

(defun input-gnuplot-cd-and-title (dir title)
  "Return a string specifying the directory and title for the plot.
DIR is the directory for the plot, TITLE is the plot's title."
  (format "\ncd '%s'\nset title '%s'\n" dir title))

(defun input-gnuplot-axis-labels (x y)
  "Return a string specifying the axis labels for the plot.
X is the x-axis label and Y is the y-axis label."
  (format "set xlabel '%s'\nset ylabel '%s'\n" x y))

;;;; end of script building utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Autobk-mode plotting

(defun Autobk-plot-ksp ()
  "Parse the current stanza for filenames of data and output files.
Write a gnuplot script for plotting weighted chi(k), and send the
script to gnuplot.
Bound to \\[Autobk-plot-ksp]"
  (interactive) (Autobk-plot "ksp"))

(defun Autobk-plot-bkg ()
  "Parse the current stanza for filenames of data and output files.
Write a gnuplot script for over-plotting data with the background
function, then send the script to gnuplot.
Bound to \\[Autobk-plot-bkg]"
  (interactive) (Autobk-plot "bkg"))

(defun Autobk-plot-thy ()
  "Parse the current stanza for filenames of data and output files.
Write a gnuplot script for over-plotting chi(k) data with the background
standard, then send the script to gnuplot.
Bound to \\[Autobk-plot-thy]"
  (interactive) (Autobk-plot "thy"))

(defun Autobk-plot (arg)
  "Parse the current stanza for filenames of data and output files.
Write a gnuplot script for plotting the results of an *autobk* run.
ARG=bkg over-plots the data and background functions.  ARG=ksp plots
the weighted chi(k).  ARG=thy plots the data and standard.  Care is
taken to check if the uwxafs binary file format is used."
  ;;(interactive "sksp, bkg or thy? ")
  (save-excursion
   (let (datafile theory (bkgxmu t) bkgout kspout title checkit
		  (input-buffer (current-buffer)) lines (ezeroline "\n" )
		  ezero (yrange () ) )
     (Autobk-parse-stanza)		; set some stuff
     (setq datafile     (cdr (assoc "datafile" Autobk-features-alist))
	   theory       (cdr (assoc "theory"   Autobk-features-alist))
	   bkgxmu       (cdr (assoc "bkgxmu"   Autobk-features-alist))
	   title        (cdr (assoc "title"    Autobk-features-alist))
	   bkgout       (cdr (assoc "bkgout"   Autobk-features-alist))
	   kspout       (cdr (assoc "kspout"   Autobk-features-alist)))

     (cond ((not datafile)		; no data file
	    (message "no data file name was found"))
					; bkgxmu=false
	   ((and (not bkgxmu) (string= arg "bkg"))
	    (message "bkgxmu set to false in this stanza."))
					; no standard
	   ((and (not theory) (string= arg "thy"))
	    (message "no standard was supplied in this stanza."))
					; plot background or chi
	   ((or (and bkgxmu (string= arg "bkg")) (string= arg "ksp")
		(string= arg "thy"))
	    (setq checkit t)
	    (setq lines
		  (concat
		   (input-gnuplot-cd-and-title
		    (file-name-directory buffer-file-name) title)
		   (input-gnuplot-set-terminal input-gnuplot-terminal)))
	    (setq input-current-terminal input-gnuplot-terminal)
	    (cond
	     ((string= arg "bkg")       ; --- background plot --------------
	      (cond (input-gnuplot-ezero-flag
		     (setq ezero  (Autobk-fetch-ezero))
		     (setq yrange (Autobk-fetch-ymin-ymax datafile))
		     (setq ezeroline
			   (format
			    "set arrow from %9.3f,%7.3f to %9.3f,%7.3f nohead\n"
			    ezero (nth 0 yrange)
			    ezero (nth 1 yrange)))))
	      (setq lines
		    (concat lines ezeroline
			    (input-gnuplot-axis-labels
			     "energy (eV)" "absorption")
			    (format "plot '%s',\\\n" datafile)
			    (format "     '%s'\n" bkgout)))
	      (if input-gnuplot-ezero-flag
		   (setq lines (concat lines "set noarrow\n"))) )
	     ((string= arg "ksp")       ; --- chi plot ---------------------
	      (setq lines
		    (concat lines (input-gnuplot-axis-labels
				   "wavenumber" "chi(k)")
			    (format "plot '<kw w=%s %s'\n"
				    input-k-weight kspout))) )
	     ((string= arg "thy")       ; --- chi plot with theory ---------
	      (setq lines
		    (concat lines (input-gnuplot-axis-labels
				   "wavenumber" "chi(k)")
			    (format "plot '<kw w=%s %s',\\\n"
				    input-k-weight kspout)
			    (format "     '<gnufix %s | kw w=%s'\n"
				    theory input-k-weight))) )
		  )
	    (input-send-to-gnuplot lines input-plot-flag))
	   (t
	    (message
	     "ksp, bkg, and thy are the only plotting options in Autobk-mode.")))
     (cond ((and checkit input-plot-flag)
	    ;;(sit-for (* 5 input-pause))
	    ;;(if (< (* 5 input-pause) 1) (setq input-pause 0.2))
	    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				 gnuplot-buffer)))     )))


(defun Autobk-plot-all-chi ()
  "Plot chi(k) from every stanza in the current *autobk* input file.
Bound to \\[Autobk-plot-all-chi]"
  (interactive)
  (save-excursion
    (let ((ksplist ()) (counter 1) (input-buffer (current-buffer)) lines)
      (goto-char (point-max))
      (while (not (input-first-stanza-p))  ; make a list of all chi filenames
	(Autobk-parse-stanza)
	(setq ksplist
	      (cons (cdr (assoc "kspout" Autobk-features-alist)) ksplist))
	(input-previous-stanza 1))
      (Autobk-parse-stanza)
      (setq ksplist
	    (cons (cdr (assoc "kspout" Autobk-features-alist)) ksplist))
      (setq lines                       ; write script
	    (concat (input-gnuplot-cd-and-title
		     (file-name-directory buffer-file-name)
		     (format "All chi(k) from %s" (buffer-file-name)))
		    (input-gnuplot-set-terminal input-gnuplot-terminal)
		    (input-gnuplot-axis-labels "wavenumber" "chi(k)")
		    (format "plot '<kw w=%s %s'" input-k-weight (nth 0 ksplist))))
      (setq input-current-terminal input-gnuplot-terminal)
      (while (< counter (length ksplist)) ; write each element of list
	(if (not (nth counter ksplist))
	    (setq counter (1+ counter))	  ; weed out nil values
	  (setq lines
		(concat lines (format ",\\\n     '<kw w=%s %s'"
				      input-k-weight (nth counter ksplist))))
	  (setq counter (1+ counter))))
      (setq lines (concat lines "\n"))
      (input-send-to-gnuplot lines input-plot-flag)
      (if (string-match "XEmacs" emacs-version)
	  (x-disown-selection))
      (cond (input-plot-flag              ; check it...
	     ;;(setq sit-time (* input-pause (length ksplist)))
	     ;;(sit-for sit-time)
	     (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				  gnuplot-buffer) ))      )))

;;;; end Autobk-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Feff-mode plotting

(defun Feff-plot-chi ()
  "Write a *gnuplot* script for plotting the results of an *feff* run.
A *gnuplot* script is written for the total chi(k) from chi.dat using
*gnufix* and the script is sent to *gnuplot*.  Bound to \\[Feff-plot-chi]"
  (interactive)
  (Feff-plot "chi"))

(defun Feff-plot-xmu ()
  "Write a *gnuplot* script for plotting the results of an *feff* run.
A *gnuplot* script is written for mu and the background from xmu.dat
using *gnufix* and the script is sent to *gnuplot*.
Bound to \\[Feff-plot-xmu]"
  (interactive)
  (Feff-plot "xmu"))

(defun Feff-plot (arg)
  "Write a *gnuplot* script for plotting the results of an feff run.
ARG=xmu sends xmu.dat through *gnufix* and plots mu and mu0.
ARG=chi sends chi.dat through gnufix and plots chi(k)."
  ;;(interactive "schi or xmu? ")
  (let ((xmufile "xmu.dat") (chifile "chi.dat") title
	(input-buffer (current-buffer)) (plot-directory nil) lines)
    (Feff-parse-file)
    (setq title    (cdr (assoc "title"    Feff-features-alist))
	  plot-directory (file-name-directory buffer-file-name))

    (cond
     ((or (string= arg "chi") (string= arg "xmu"))
      (setq lines
	    (concat (input-gnuplot-cd-and-title plot-directory title)
		    (input-gnuplot-set-terminal input-gnuplot-terminal)))
      (setq input-current-terminal input-gnuplot-terminal)

      (cond ((string= arg "chi")
	     (setq lines
		   (concat lines
			   (input-gnuplot-axis-labels "wavenumber"
						      "calculated chi(k)")
			   (format "\nplot '<gnufix %s | kw w=%s'\n"
				   chifile input-k-weight))))
	    ((string= arg "xmu")
	     (setq lines
		   (concat lines
			   (input-gnuplot-axis-labels "energy"
						      "calculated mu(E)")
			   (format "\nplot '<gnufix %s' using 1:4,\\\n" xmufile)
			   (format   "     '<gnufix %s' using 1:5\n"    xmufile)
			    ))))
      (input-send-to-gnuplot lines input-plot-flag)
      (cond (input-plot-flag
	     ;;(sit-for (* 5 input-pause))
	     ;;(if (< (* 5 input-pause) 1) (setq input-pause 0.2))
	     (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				  gnuplot-buffer)))      )
     (t
      (message "chi and xmu are the only plotting options in Feff-mode."))) ))

(defun Feff-8-plot-convergence ()
  "Write a *gnuplot* script for plotting convergence data from a feff8 run."
  (interactive)
  (let ((input-buffer (current-buffer)) (title "convergence results")
	(plot-directory (file-name-directory buffer-file-name)) lines)
    (setq lines
	  (concat (input-gnuplot-cd-and-title plot-directory title)
		  (input-gnuplot-set-terminal input-gnuplot-terminal)))
    (setq input-current-terminal input-gnuplot-terminal)
    (setq lines (concat lines
		       (input-gnuplot-axis-labels "iteration"
						  "Fermi energy (eV)")
		       (format "\nplot '%s' with linespoints\n"
			       Feff-8-convergence-filename)))
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag
	   ;;(sit-for (* 5 input-pause))
	   ;;(if (< (* 5 input-pause) 1) (setq input-pause 0.2))
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))

(defun Feff-8-plot-rho ()
  "Write a *gnuplot* script for plotting LDOS results from a feff8 run.
The script will overplot the atomic and full LDOS for a unique potential
and an angular momentum state prompted for in the minibuffer.  The rhofix
script will be used to display the full LDOS calculation."
  (interactive)
  (let (ipot lstate lcol lines (input-buffer (current-buffer))
	(lstates '(("s" . "3") ("p" . "4") ("d" . "5") ("f" . "6"))) )
    (setq ipot (read-string "Which unique potential? [0-7, <cr>=0] " ))
    (if (string-match "^\\([0-7]\\)" ipot)  ;; read ipot, default=0
	(setq ipot (substring ipot 0 1))
      (setq ipot "0"))
    (setq lstate (concat                    ;; read ang. mom., default=s
		  (read-string "Which angular momentum? [spdf, <cr>=s] ") ".")
	  lcol   (or (cdr (assoc (substring lstate 0 1) lstates))
		     "3"))                  ;; make sure `lstate' is sensible
    (if (string= lcol "3") (setq lstate "s"))
    (setq lines
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name)
		   (format "Density of states, ipot=%s" ipot))
		  (input-gnuplot-set-terminal input-gnuplot-terminal)
		  (input-gnuplot-axis-labels "Energy (eV)"
					     (format "%s DOS" lstate))
		  (format "\nn=%s\n\n" lcol)
		  (format "plot 'rhoc0%s.dat' using 1:n,\\\n" ipot)
		  (format "     '<rhofix rhol0%s.dat' using 1:n\n" ipot) ))

    (setq input-current-terminal input-gnuplot-terminal)
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag                   ; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))

(defun Feff-8-plot-xmu ()
  "Write a *gnuplot* script for plotting xanes results from a feff8 run.
The xanes spectrum and the atomic background will be overplotted.
If the keyword \"DATA\" is found in a comment line in the feff input file
then the data file specified will be plotted along with the calculation.
The eshift script will be used along with the value of `input-eshift'
to aling the data with the calculation."
  (interactive)
  (let (lines title (datafile (Feff-8-parse-datafile))
	      (input-buffer (current-buffer))
	      (esh (or input-eshift "0")))
    (Feff-parse-file)
    (setq title (cdr (assoc "title" Feff-features-alist))
	  lines
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name) title)
		  (input-gnuplot-axis-labels "Energy (eV)" "xmu")
		  (input-gnuplot-set-terminal input-gnuplot-terminal))
	  input-current-terminal input-gnuplot-terminal)
    (if (and datafile (file-exists-p datafile))
	(setq lines (concat lines
			    (format "plot '<eshift esh=%s %s',\\\n"
				    esh datafile)
			    "     '<gnufix xmu.dat' using 1:4,\\\n"))
      (setq lines (concat lines
			  "plot '<gnufix xmu.dat' using 1:4,\\\n")) )
    (setq lines (concat lines "     '<gnufix xmu.dat' using 1:5 \n") )
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag
					; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))


;;;; end Feff-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Feffit-mode plotting

(defun Feffit-plot-k ()
  "Generate a *gnuplot* script to plot fit results in original k space.
The *kw* script is used to apply k-weighting.  If allout is true, then
all paths will be included in the *gnuplot* script.  Bound to \\[Feffit-plot-k]."
  (interactive)
  (let (logical outbase)
    (Feffit-parse-data-set)
    (setq logical (cdr (assoc "kspout" Feffit-features-alist))
	  outbase (cdr (assoc "outbase" Feffit-features-alist)))
    (cond (outbase
	   (if logical (Feffit-plot "k")
	     (message "Cannot plot k-space fit.  kspout is set to false.")))
	  (t
	   (message "No output filename is provided in this input file.")) ) ))

(defun Feffit-plot-r ()
  "Generate a *gnuplot* script to plot fit results in R space.
If allout is true, then all paths will be included in the *gnuplot*
script.  Bound to \\[Feffit-plot-r]."
  (interactive)
  (let (logical outbase)
    (Feffit-parse-data-set)
    (setq logical (cdr (assoc "rspout" Feffit-features-alist))
	  outbase (cdr (assoc "outbase" Feffit-features-alist)))
    (cond (outbase
	   (if logical (Feffit-plot "r")
	     (message "Cannot plot R-space fit.  rspout is set to false.")))
	  (t
	   (message "No output filename is provided in this input file.")) ) ))

(defun Feffit-plot-q ()
  "Generate a *gnuplot* script to plot fit results in back transform space.
If allout is true, then all paths will be included in the *gnuplot*
script.  Bound to \\[Feffit-plot-q]"
  (interactive)
  (let (logical outbase)
    (Feffit-parse-data-set)
    (setq logical (cdr (assoc "qspout" Feffit-features-alist))
	  outbase (cdr (assoc "outbase" Feffit-features-alist)))
    (cond (outbase
	   (if logical (Feffit-plot "q")
	     (message "Cannot plot q-space fit.  qspout is set to false.")))
	  (t
	   (message "No output filename is provided in this input file.")) ) ))

(defconst fuse-plot-space-stuff
  ;; spc    kw        ext     mr      kw/uwx    using  column complex
  '(("k" . ("<kw w=%s " ".chi"  "<mr " " | kw w=%s " nil nil nil
     "set xlabel 'wavenumber'\nset ylabel 'chi(k)'\n"))
    ("r" . (nil         ".rsp"  "<mr " nil " using 1:n" input-gnuplot-r-column t
     "set xlabel 'R (Angstroms)'\nset ylabel 'chi(R)'\n"))
    ("q" . (nil         ".env"  "<mr " nil " using 1:n" input-gnuplot-q-column t
     "set xlabel 'wavenumber'\nset ylabel 'back transform chi(k)'\n"))))

(defun Feffit-plot (space)
  "Make the list that is sent to input-send-to-gnuplot.
This is the workhorse function for all of the \"plot data and fit\"
functions.  This will do the right thing for each of the three spaces
and optionally write out the background and all of the individual paths."
  ;; note that (Feffit-parse-data-set) has already been called
  (let (lines indeces index (offset 2) (input-buffer (current-buffer))
	(title   (cdr (assoc "title"   Feffit-features-alist)))
	(outbase (cdr (assoc "outbase" Feffit-features-alist)))
	(formout (cdr (assoc "formout" Feffit-features-alist)))
	;;(allout  (cdr (assoc "allout"  Feffit-features-alist)))
	(bkgout  (cdr (assoc "bkgout"  Feffit-features-alist)))
					; set things specific to each space
	(kw      (elt (cdr (assoc space fuse-plot-space-stuff)) 0))
	(ext     nil)
	(mr      nil)
	(using   (elt (cdr (assoc space fuse-plot-space-stuff)) 4))
	(column  (eval (elt (cdr (assoc space fuse-plot-space-stuff)) 5)))
	(complex (elt (cdr (assoc space fuse-plot-space-stuff)) 6))
	(axes    (elt (cdr (assoc space fuse-plot-space-stuff)) 7)) )
    (when (string= formout "uwxafs")	; handle uwxafs, kweight, allout
      (setq kw  (elt (cdr (assoc space fuse-plot-space-stuff)) 3)
	    ext (elt (cdr (assoc space fuse-plot-space-stuff)) 1)
	    mr  (elt (cdr (assoc space fuse-plot-space-stuff)) 2) ))
    (if kw (setq kw (format kw input-k-weight)))
    ;;(if allout
    ;;(setq indeces (cdr (assoc "indeces" Feffit-features-alist))))
    (if Feffit-marked-paths
	(setq indeces (sort (copy-list Feffit-marked-paths) '<)))
    (setq lines				; beginning of gnuplot script
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name) title)
		  axes
		  (input-gnuplot-set-terminal input-gnuplot-terminal))
	  input-current-terminal input-gnuplot-terminal)
    (if complex
	(setq lines (concat lines "n=" column
			    "\t\t# 2=real 3=imag 4=mag 5=phase\n\n"))
      (setq lines (concat lines "\n")))
    (cond ((string= formout "ascii")	; lines for data, fit, bkg, paths
	   (setq lines (concat lines
			       "plot '" kw outbase space ".dat'" using ",\\\n"
			       "     '" kw outbase space ".fit'" using ) )
	   (if bkgout
	       (setq lines (concat lines
				   ",\\\n     '" kw outbase space ".bkg'" using)))
	   ;;(while (and allout indeces)
	   (while indeces
	     (setq index   (car indeces))
	     (if (file-exists-p (concat outbase space "." (format "%03d" index)))
		 (setq lines   (concat lines ",\\\n     '" kw outbase space "."
				       (format "%03d" index)
				   "'" using )))
	     (setq indeces (cdr indeces))) )
	  (t
	   (setq lines (concat lines
			       "plot '" mr outbase ext ", 1" kw "'" using ",\\\n"
			       "     '" mr outbase ext ", 2" kw "'" using) )
	   (when bkgout
	     (setq lines (concat lines
				 ",\\\n     '" mr outbase ext ", 3" kw "'" using)
		   offset 3))
	   ;;(while (and allout indeces)
	   (while indeces
	     (setq index   (car indeces)
		   lines   (concat lines ",\\\n     '" mr outbase ext ", "
				   (+ offset index) kw "'" using)
		   indeces (cdr indeces))) ) )
    (setq lines (concat lines "\n"))
    (input-send-to-gnuplot lines input-plot-flag) ; plot it!
    (cond (input-plot-flag
	   ;;(sit-for (* (length lines) input-pause)) ; check status of plot
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer) ))
    ))

;; The next four functions are obsolted by the new path marking features

(defun Feffit-plot-path-in-r ()
  "Plot the current path along with the data in R space.
Bound to \\[Feffit-plot-path-in-r]."
  (interactive) (Feffit-plot-path "r"))
(defun Feffit-plot-path-in-k ()
  "Plot the current path along with the data in original k space.
Bound to \\[Feffit-plot-path-in-k]."
  (interactive) (Feffit-plot-path "k"))
(defun Feffit-plot-path-in-q ()
  "Plot the current path along with the data in back transform k space.
Bound to \\[Feffit-plot-path-in-q]."
  (interactive) (Feffit-plot-path "q"))

(defun Feffit-plot-path (space)
  "Plot the current path along with the data.
SPACE is the specified space in which to make the plot.
This is the workhorse function for the \"plot path\" functions."
  (Feffit-parse-data-set)
  (let (lines index (input-buffer (current-buffer))
	      (path-regex "^[ \t]*\\(path\\|feff\\)")
					; fetch info for script
	      (outbase (cdr (assoc "outbase" Feffit-features-alist)))
	      (formout (cdr (assoc "formout" Feffit-features-alist)))
					; set things specific to each space
	      (kw      (elt (cdr (assoc space fuse-plot-space-stuff)) 0))
	      (ext     nil)
	      (mr      nil)
	      (using   (elt (cdr (assoc space fuse-plot-space-stuff)) 4))
	      (column  (eval (elt (cdr (assoc space fuse-plot-space-stuff)) 5)))
	      (complex (elt (cdr (assoc space fuse-plot-space-stuff)) 6))
	      (axes    (elt (cdr (assoc space fuse-plot-space-stuff)) 7)) )
    (when (string= formout "uwxafs")	; handle uwxafs and kweighting correctly
      (setq kw  (elt (cdr (assoc space fuse-plot-space-stuff)) 3)
	    ext (elt (cdr (assoc space fuse-plot-space-stuff)) 1)
	    mr  (elt (cdr (assoc space fuse-plot-space-stuff)) 2) ))
    (if kw (setq kw (format kw input-k-weight)))
    (save-excursion			; fetch current path index
      (if (or (looking-at path-regex)
	      (re-search-backward path-regex (point-min) t))
	  (setq index (string-to-number (elt (input-path-param-value) 0)))
	(setq index 1)))
    (setq lines				; beginning of gnuplot script
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name)
		   (format "Path %d and data" index))
		  axes
		  (input-gnuplot-set-terminal input-gnuplot-terminal))
	  input-current-terminal input-gnuplot-terminal)
    (if complex
	(setq lines (concat lines "n=" column
				    "\t\t# 2=real 3=imag 4=mag 5=phase\n\n"))
      (setq lines (concat lines "\n")))
    (if (string= formout "ascii")
	(setq lines (concat lines
			    "plot '" kw outbase space ".dat'" using ",\\\n"
			    "     '" kw outbase space (format ".%03d'" index)
			    using "\n") )
      (setq lines (concat lines
			  "plot '" mr outbase ext ", 1" kw "'" using ",\\\n"
			  "     '" mr outbase ext ", " (format "%d" (+ index 2))
			  kw "'" using "\n") ) )
    (input-send-to-gnuplot lines input-plot-flag) ; plot it!
    (cond (input-plot-flag
	   ;;(sit-for (* (length lines) input-pause)) ; check status of plot
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer) )) ))


;;;; end of Feffit-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Normal-mode plotting

(defun Normal-plot-xmu ()
  "Plot all unnormalized xmu files in a *normal* input file.
Bound to \\[Normal-plot-xmu]"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (Normal-parse-file)
    (let (plot lines file key
	       (input-buffer (current-buffer))
	       (formin  (cdr (assoc "formin"   Normal-features-alist)))
	       (files   (cdr (assoc "files"    Normal-features-alist)))
	       (keys    (cdr (assoc "keys"     Normal-features-alist))))
      (cond (files
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "Un-Normalized scans")
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


(defun Normal-plot-all ()
  "Plot all normalized files from a *normal* input file.
Bound to \\[Normal-plot-all]"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (Normal-parse-file)
    (let (lines plot (counter 0) (input-buffer (current-buffer))
		(formout  (cdr (assoc "formout"  Normal-features-alist)))
		(files    (cdr (assoc "files"    Normal-features-alist)))
		(basename (cdr (assoc "basename" Normal-features-alist))))
					; find file list
      (cond (files
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "Normalized scans")
			   (input-gnuplot-set-terminal input-gnuplot-terminal)
			   (input-gnuplot-axis-labels
			    "Energy (eV)" "Absorption")))
	     (setq input-current-terminal input-gnuplot-terminal)
	     (setq plot "plot ")
	     (while (< counter (- (length files) 1))
	       (if (> counter 0) (setq plot "     "))
	       (setq counter (1+ counter))
	       (if (string= formout "ascii")
		   (setq lines (concat lines
				       (format "%s'%s.%03d',\\\n"
					       plot basename counter)))
		 (setq lines (concat lines
				     (format "%s'<mr %s.nor, %d',\\\n"
					     plot basename counter))) ) )
	     (if (string= formout "ascii")
		 (setq lines
		       (concat lines
			       (format "     '%s.%03d'\n"
				       basename (length files))))
	       (setq lines
		     (concat lines
			     (format "     '<mr %s.nor, %d'\n"
				     basename (length files)))) )
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* (- (length lines) 5) input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) )) )
	    (t
	     (message "No file list was found in this input file.")))
      )))

(defun Normal-plot-this-xmu ()
  "Plot the unnormalized data on the current line of a *normal* input file.
Bound to \\[Normal-plot-this-xmu]"
  (interactive)
  (save-excursion
    (let (lines file formin key (input-buffer (current-buffer)))
      (Normal-parse-file)
      (setq formin  (cdr (assoc "formin"   Normal-features-alist)))
      (back-to-indentation)
      (setq file (input-this-word))
      (cond ((string= formin "uwxafs")
	     (re-search-forward input-word-sep)
	     (setq key (input-this-word))))

      (cond ((file-readable-p file)
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "xmu data")
			   (input-gnuplot-set-terminal input-gnuplot-terminal)
			   (input-gnuplot-axis-labels
			    "Energy (eV)" "Un-Normalized Absorption")))
	     (setq input-current-terminal input-gnuplot-terminal)
	     (if (string= formin "ascii")
		 (setq lines (concat lines (format "plot '%s'\n" file) ))
	       (setq lines (concat lines
				   (format "plot '<mr %s, %s'\n" file key) )) )
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* 3 input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) )) )

	    (t
	     (message "Point is not on a file line.")))
      )))


(defun Normal-plot-this-norm ()
  "Plot the normalized data from the current line of a *normal* input file.
Bound to \\[Normal-plot-this-norm]"
  (interactive)
  (save-excursion
    (let (lines file (input-buffer (current-buffer)) basename (counter 1) formout)
      (Normal-parse-file)
      (setq formout (cdr (assoc "formout"   Normal-features-alist)))
      (back-to-indentation)
      (setq file (input-this-word))
      (cond ((file-readable-p file)
	     (while (not (bobp))
	       (forward-line -1)
	       (cond ((looking-at "^[ \t]*\\(files\\|data\\)\\>")
		      (goto-char (point-min)))
		     ((or (input-comment-p) (looking-at "^[ \t]*$"))
		      ())
		     (t
		      (setq counter (1+ counter))) ) )
	     (goto-char (point-max))
	     (cond ((re-search-backward "\\<out\\>" (point-min) t)
		    (re-search-forward input-word-sep)
		    (setq basename (input-this-word))
		    (setq basename (file-name-sans-extension basename)) )
		   ((re-search-backward "^[ \t]*\\(files\\|data\\)\>"
					(point-min) t)
		    (re-search-forward "^[^ \t*%#!]*$" (point-max) 'move)
		    (back-to-indentation)
		    (setq basename (input-this-word))
		    (setq basename (file-name-sans-extension basename)) )
		   (t
		    (message "No valid output file name was found.")))
	     (setq lines
		   (concat (input-gnuplot-cd-and-title
			    (file-name-directory buffer-file-name)
			    "Normalized data")
			   (input-gnuplot-set-terminal input-gnuplot-terminal)
			   (input-gnuplot-axis-labels
			    "Energy (eV)" "Normalized Data")))
	     (setq input-current-terminal input-gnuplot-terminal)
	     (if (string= formout "ascii")
		 (setq lines
		       (concat lines (format "plot '%s.%03d'\n"
					     basename counter) ))
	       (setq lines
		     (concat lines (format "plot '<mr %s.nor, %d'\n"
						 basename counter) )) )
	     (input-send-to-gnuplot lines input-plot-flag)
	     (cond (input-plot-flag
					; check status of plot
		    ;;(sit-for (* 3 input-pause))
		    (input-check-gnuplot input-buffer gnuplot-script-buffer-name
					 gnuplot-buffer) )))
	    (t
	     (message "Point is not on a file line.")))
    )))

;;;; end of Normal-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Phit-mode plotting

(defun Phit-plot-fit ()
  "Plot a *phit* fit, possibly with each individual function.
Bound to  \\[Phit-plot-fit]"
  (interactive)
  (let (lines title xaxis sigma allout (nofit t) (fitext "fit")
	      (input-buffer (current-buffer))
	      (indeces ()) (counter 0) datafile outbase formin formout
	      (ebars "") key)
    (Phit-parse-file)
    (setq title    (cdr (assoc "title"    Phit-features-alist))
	  datafile (cdr (assoc "datafile" Phit-features-alist))
	  outbase  (cdr (assoc "outbase"  Phit-features-alist))
	  xaxis    (cdr (assoc "xaxis"    Phit-features-alist))
	  sigma    (cdr (assoc "sigma"    Phit-features-alist))
	  allout   (cdr (assoc "allout"   Phit-features-alist))
	  nofit    (cdr (assoc "nofit"    Phit-features-alist))
	  formin   (cdr (assoc "formin"   Phit-features-alist))
	  formout  (cdr (assoc "formout"  Phit-features-alist)))
    (if (string= formin "uwxafs")
	(setq key     (cdr (assoc "key"     Phit-features-alist))))
    (if   allout
	(setq indeces (cdr (assoc "indeces" Phit-features-alist))))
    (if nofit (setq fitext "nofit"))

    (setq lines (concat (input-gnuplot-cd-and-title
			 (file-name-directory buffer-file-name)
			 (or title "Phit result"))
			(input-gnuplot-axis-labels xaxis "data and fit")
			(input-gnuplot-set-terminal input-gnuplot-terminal)))
    (setq input-current-terminal input-gnuplot-terminal)
    (and sigma
	 (setq ebars " u 1:2:3 w errorbars"))
    (if (string= formin "ascii")
	(setq lines
	      (concat lines (format "plot '%s'%s,\\\n" datafile ebars)))
      (setq lines
	    (concat lines (format "plot '<mr %s, %s'%s,\\\n"
				  datafile key ebars))) )

    (if (string= formout "ascii")
	(setq lines
	      (concat lines (format "     '%s.%s'" outbase fitext)))
      (setq lines
	    (concat lines (format "     '<mr %s, 1'" outbase))) )

    (cond (allout
	   (while (< counter (length indeces))
	     (if (string= formout "ascii")
		 (setq lines
		       (concat lines
			       (format ",\\\n     '%s.%03d'"
				       outbase (string-to-number
						(nth counter indeces))) ))
	       (setq lines
		     (concat lines
			     (format ",\\\n     '<mr %s, %d'"
				     outbase (+ 1 (string-to-number
						   (nth counter indeces)))) )) )
	     (setq counter (1+ counter))
	     )))

    (setq lines (concat lines "\n"))

    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag
					; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))


;;;; end of Phit-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Xanes-mode plotting

(defun Xanes-plot-raw ()
  "Plot xchi.raw from a *xanes* run.
Bound to \\[Xanes-plot-raw]"
  (interactive)
  (let (lines (input-buffer (current-buffer)))
    (setq lines
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name) "Unconvolved chi")
		  (input-gnuplot-set-terminal input-gnuplot-terminal)
		  (input-gnuplot-axis-labels "Energy (eV)" "chi(E)")
		  "plot 'xchi.raw' using 1:6\n"))
    (setq input-current-terminal input-gnuplot-terminal)
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag                        ; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))

(defun Xanes-plot-dos ()
  "Plot xdos.dat and xdos.raw files.
ipot and angular momentum prompted from minibuffer.  Bound to \\[Xanes-plot-dos]"
  (interactive)
  (let (ipot lstate lcol lines (input-buffer (current-buffer))
	(lstates '(("s" . "4") ("p" . "5") ("d" . "6") ("f" . "7"))) )
    (setq ipot (read-string "Which unique potential? [0-7, <cr>=0] " ))
    (if (string-match "^\\([0-7]\\)" ipot)  ;; read ipot, default=0
	(setq ipot (substring ipot 0 1))
      (setq ipot "0"))
    (setq lstate (concat                    ;; read ang. mom., default=s
		  (read-string "Which angular momentum? [spdf, <cr>=s] ") "."))
    (setq lcol   (or (cdr (assoc (substring lstate 0 1) lstates))
		     "4"))                  ;; make sure `lstate' is sensible
    (if (string= lcol "4") (setq lstate "s"))
    (setq lines
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name)
		   (format "Density of states, ipot=%s" ipot))
		  (input-gnuplot-set-terminal input-gnuplot-terminal)
		  (input-gnuplot-axis-labels "Energy (eV)"
					     (format "%s DOS" lstate))
		  (format "plot 'xdos_%s.dat' using 1:%s,\\\n" ipot lcol)
		  (format "     'xdos_%s.raw' using 1:%s\n"    ipot lcol) ))

    (setq input-current-terminal input-gnuplot-terminal)
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag                   ; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Correct-mode plotting

(defun Correct-plot-xmu ()
  "Plot xmu from correct with data.
Bound to \\[Correct-plot-xmu]"
  (interactive)
  (let (lines datafile outfile (input-buffer (current-buffer)))
    (Correct-parse-file)
    (setq datafile (cdr (assoc "datafile" Correct-features-alist))
	  outfile  (cdr (assoc "outfile"  Correct-features-alist)))
    (setq lines
	  (concat (input-gnuplot-cd-and-title
		   (file-name-directory buffer-file-name)
		   "Correct result")
		  (input-gnuplot-axis-labels "Energy (eV)" "xmu")
		  (input-gnuplot-set-terminal input-gnuplot-terminal)))
    (setq input-current-terminal input-gnuplot-terminal)
    (if datafile
	(setq lines (concat lines
			    (format "plot '%s',\\\n" datafile)
			    (format "     '%s' using 1:4,\\\n" outfile)))
      (setq lines (concat lines
			  (format "plot '%s' using 1:4,\\\n" outfile))) )
    (setq lines (concat lines
			(format "     '%s' using 1:5 \n"    outfile)) )
    (input-send-to-gnuplot lines input-plot-flag)
    (cond (input-plot-flag
					; check status of plot
	   ;;(sit-for (* (length lines) input-pause) )
	   (input-check-gnuplot input-buffer gnuplot-script-buffer-name
				gnuplot-buffer)))
    ))

;;;; end of Correct-mode plotting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fuse-kill-gnuplot ()
  "Kill the gnuplot process and all associated buffers and frames."
  (interactive)
  (if (get-process gnuplot-program)
      (kill-process gnuplot-program))
  (if (get-buffer gnuplot-buffer)
      (kill-buffer gnuplot-buffer))
  (if (get-buffer gnuplot-script-buffer-name)
      (kill-buffer gnuplot-script-buffer-name))
  (if (and input-gnuplot-frame (frame-live-p input-gnuplot-frame))
      (delete-frame input-gnuplot-frame))
  ;;(if (featurep 'gnuplot)         ;; lose hook values if this is done
  ;;    (unload-feature 'gnuplot))
  ;; reset other variables??
  (setq input-current-terminal nil) )


;;; additions to gnuplot mode for fuse.  the script history gets
;;; placed onto the `gnuplot-after-run-hook'

(defcustom fuse-gnuplot-beep-flag input-beep-flag
  "*Non-nil means to beep when running into the ends of the history."
  :group 'fuse-features
  :group 'gnuplot
  :type 'boolean)
(defcustom fuse-gnuplot-history-length 10
  "*Length of gnuplot history list.
The history list is a list of complete gnuplot scripts that have
previously been sent to gnuplot.  Typically the history list is updated
as a function in `gnuplot-after-plot-buffer-hook',"
  :group 'fuse-features
  :group 'gnuplot
  :type 'integer)
(defcustom fuse-gnuplot-top-bottom-separator "^#-#-#-"
  "*Regular expression separating the top and bottom sections of the script.
This is used by the script history mechanism.  Anything written above
a line matching this regular expression will not be saved in the script
history."
  :group 'fuse-features
  :group 'gnuplot
  :type 'string)

(defvar fuse-gnuplot-used nil)
(defvar fuse-gnuplot-history ()
  "Script history list for gnuplot.")
(defvar fuse-gnuplot-history-index -1
  "Current location in gnuplot history list.")


(define-key gnuplot-mode-map "\C-c\C-n" 'fuse-gnuplot-next-script)
(define-key gnuplot-mode-map "\C-c\C-p" 'fuse-gnuplot-previous-script)

(defun fuse-gnuplot-menubar-xemacs ()
  (add-menu-button '("Gnuplot") "---")
  (add-menu-button '("Gnuplot") "Gnuplot functions for FUSE")
  (add-menu-button '("Gnuplot")
		   ["previous script in history" fuse-gnuplot-previous-script t])
  (add-menu-button '("Gnuplot")
		   ["next script in history" fuse-gnuplot-next-script t]))

(defun fuse-gnuplot-menubar-emacs ()
  (define-key-after (lookup-key gnuplot-mode-map [menu-bar Gnuplot])
    [justaline]
    '("----" . nil)
    [Kill gnuplot])
  (define-key-after (lookup-key gnuplot-mode-map [menu-bar Gnuplot])
    [fuselabel]
    '("Gnuplot functions for FUSE" . nil)
    [justaline])
  (define-key-after (lookup-key gnuplot-mode-map [menu-bar Gnuplot])
    [previous]
    '("previous script in history" . fuse-gnuplot-previous-script)
    [fuselabel])
  (define-key-after (lookup-key gnuplot-mode-map [menu-bar Gnuplot])
    [next]
    '("next script in history" . fuse-gnuplot-next-script)
    [previous]))

(cond ((string-match "XEmacs" emacs-version)
       (add-hook 'gnuplot-mode-hook 'fuse-gnuplot-menubar-xemacs))
      (t
       (add-hook 'gnuplot-mode-hook 'fuse-gnuplot-menubar-emacs)))


;;; script history utilities

(defun fuse-gnuplot-push-history (string)
  "Push the current script onto the history list.
If the history list exceeds `gnuplot-history-length' then pop the
oldest script from the list.  STRING contains the script as a single
character string."
  (interactive)
  (let ((l (length fuse-gnuplot-history)))
    (if fuse-gnuplot-history
	(setq fuse-gnuplot-history
	      (append (list string) fuse-gnuplot-history))
      (setq fuse-gnuplot-history (list string)))
    (and (> l fuse-gnuplot-history-length)
	 (setq fuse-gnuplot-history (butlast fuse-gnuplot-history 1))) )
  t)

(defun fuse-gnuplot-previous-script ()
  "Step back in gnuplot history.
Bound to \\[fuse-gnuplot-previous] in the gnuplot buffer."
  (interactive) (fuse-gnuplot-navigate-history 1))

(defun fuse-gnuplot-next-script ()
  "Step forward in gnuplot history.
Bound to \\[fuse-gnuplot-next] in the gnuplot buffer."
  (interactive) (fuse-gnuplot-navigate-history -1))

(defun fuse-gnuplot-navigate-history (place)
  "Step up and down the gnuplot history list.
This does not send a script in the history list to gnuplot, it only
displays one in the gnuplot buffer.  Care is taken to update history
list with most recent plot.  PLACE is 1 to step back and -1 to step
forward (element 0 in the list is the most recent)."
  (let ((buffer (current-buffer)))
    (cond ((not (or (equal place -1) (equal place 1)))
	   nil)
	  ((equal 1 (length fuse-gnuplot-history))
	   (if fuse-gnuplot-beep-flag (ding))
	   (message "This is the only entry in the gnuplot history."))
	  ((and (equal place -1) (<= fuse-gnuplot-history-index 0))
	   (if fuse-gnuplot-beep-flag (ding))
	   (message "This is the most current entry in the gnuplot history."))
	  ((and (equal place 1) (equal (+ 1 fuse-gnuplot-history-index)
				       (length fuse-gnuplot-history)))
	   (if fuse-gnuplot-beep-flag (ding))
	   (message "This is the oldest entry in the gnuplot history."))
	  (t
	   ;;(switch-to-buffer gnuplot-script-buffer-name)
	   (goto-char (point-max))
	   (search-backward (regexp-quote fuse-gnuplot-top-bottom-separator)
			    (point-min) "to_limit")
	   (and (looking-at (regexp-quote fuse-gnuplot-top-bottom-separator))
		(forward-line 1))
	   (and (equal place 1)
		(equal fuse-gnuplot-history-index -1)
		(setq fuse-gnuplot-history-index 0))
	   (delete-region (point) (point-max))
	   (setq fuse-gnuplot-history-index (+ fuse-gnuplot-history-index place))
	   (insert (elt fuse-gnuplot-history fuse-gnuplot-history-index))
	   (switch-to-buffer buffer)) )))


(defun fuse-gnuplot-after-plot-buffer ()
  "Update history list after plotting the buffer."
  (save-excursion
    (when (equal 'buffer gnuplot-recently-sent)
	  (if (search-backward (regexp-quote fuse-gnuplot-top-bottom-separator)
			       (point-min) "to_limit")
	      (forward-line 1))
	  (fuse-gnuplot-push-history
	   (buffer-substring-no-properties (point) (point-max)))
	  (setq fuse-gnuplot-history-index -1))))


(defvar gnuplot-up-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *up-arrow[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        4            1\",
/* colors */
\"a c #bebebe s backgroundToolBarColor\",
\"b c #000000\",
\"c c #0000ff\",
\"d c #009800\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaabccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaabccccccccbaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaabccccccccccbaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaabccccccccccccbaaaaaaaaaaaaa\",
\"aaaaaaaaaaaabccccccccccccccbaaaaaaaaaaaa\",
\"aaaaaaaaaaabbbbbbccccccbbbbbbaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabcccccbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabcccccbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbdbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccbbddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabcbddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbdddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbdddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"down\" button")

(defvar gnuplot-down-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *gnuplot-toolbar-dnhist-up[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        4           1\",
/* colors */
\"a c #bebebe s backgroundToolBarColor\",
\"b c #000000\",
\"c c #0000ff\",
\"d c #009800\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddddddbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabdddddbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabdddddbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddddbcbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabddbbccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabdbccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbcccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbcccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaabbbbbbccccccbbbbbbaaaaaaaaaaa\",
\"aaaaaaaaaaaabccccccccccccccbaaaaaaaaaaaa\",
\"aaaaaaaaaaaaabccccccccccccbaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaabccccccccccbaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaabccccccccbaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaabccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"down\" button")

(defun fuse-amend-gnuplot-toolbar ()
  (fset 'gnuplot-up-fn   'fuse-gnuplot-previous-script)
  (fset 'gnuplot-down-fn 'fuse-gnuplot-next-script)
  (make-local-variable 'gnuplot-toolbar)
  (setq gnuplot-toolbar
	'(
	  [gnuplot-line-xpm
	   gnuplot-line-fn
	   t
	   "Plot the line under point"]
	  [gnuplot-region-xpm
	   gnuplot-region-fn
	   t
	   "Plot the selected region"]
	  [gnuplot-buffer-xpm
	   gnuplot-buffer-fn
	   t
	   "Plot the entire buffer"]
	  [gnuplot-help-xpm
	   gnuplot-help-fn
	   t
	   "Look at the gnuplot process buffer"]
	  [gnuplot-doc-xpm
	   gnuplot-doc-fn
	   t
	   "Look at the gnuplot document"]
	  [gnuplot-up-xpm
	   gnuplot-up-fn
	   t
	   "View the previous script in the history"]
	  [gnuplot-down-xpm
	   gnuplot-down-fn
	   t
	   "View the next script in the history"]))
  (gnuplot-make-toolbar-function))

(add-hook 'gnuplot-after-plot-hook 'fuse-gnuplot-after-plot-buffer)
;; need to tidy up a few things so gnuplot mode plays well with fuse
(add-hook 'gnuplot-mode-hook
	  '(lambda ()
	     (setq truncate-lines t
		   gnuplot-info-display 'frame
		   gnuplot-display-process nil
		   gnuplot-echo-command-line-flag nil)
      	     (fset 'gnuplot-show-gnuplot-buffer 'input-jump-to-gnuplot-buffer)
	     (fset 'gnuplot-kill-gnuplot 'fuse-kill-gnuplot)
	     (and (featurep 'toolbar)
		  (fuse-amend-gnuplot-toolbar))))


;;; That's it! ----------------------------------------------------------------

; any final chores before leaving
(provide 'fuse-gnuplot)

;;;============================================================================
;;;
;;; fuse-gnuplot.el ends here
