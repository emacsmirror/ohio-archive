;;; fuse-toolbar.el --- toolbar support for FUSE

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  16 February 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: fuse-toolbar.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

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

;;; Change-log:

;;; Code:


(eval-and-compile        ; for Atoms-math
  (condition-case ()
      (require 'calc)
    (error nil)))
(require 'cl)

(require 'fuse-icons)    ; pixmap definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define toolbars for the various modes

(defvar fuse-toolbar
  '([fuse-toolbar-new-file-xpm         fuse-toolbar-new-file
				       t "Edit an input file"]
    [fuse-toolbar-document-xpm         fuse-toolbar-document
				       t "Read the FUSE document"]
    [fuse-toolbar-tutorial-xpm         fuse-toolbar-tutorial
				       t "Read the FUSE tutorial"]
    [fuse-toolbar-program-document-xpm fuse-toolbar-program-document
				       t "Read program documentation"]
    [fuse-toolbar-helper-xpm           fuse-toolbar-helper
				       t "A bit of help"]
    [fuse-toolbar-bug-xpm              fuse-toolbar-bug
				       t "Report a bug"]
    [:style 3d :size 8]
    [fuse-toolbar-quit-xpm             fuse-toolbar-quit
				       t "Quit FUSE"]
    [fuse-toolbar-exit-xpm             fuse-toolbar-exit
				       t "Quit Emacs"]
    )
  "The FUSE toolbar.")

(add-hook 'fuse-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function fuse-toolbar))))

(defvar Atoms-toolbar ()
  "The Atoms toolbar.")
(setq Atoms-toolbar
      '([fuse-toolbar-template-xpm Atoms-toolbar-template
				   t "Write an Atoms template"]
	[fuse-toolbar-run-xpm      Atoms-toolbar-run
				   t "Run Atoms"]
	[fuse-toolbar-log-xpm      Atoms-toolbar-log
				   t "Look at the Feff input file"]))
(if (featurep 'calc)
    (setq Atoms-toolbar
	  (append Atoms-toolbar
		  '([:style 3d :size 8]
		    [Atoms-toolbar-eval-line-xpm
		     Atoms-toolbar-eval-line
		     t
		     "Expand evaluation line under point"]
		    [Atoms-toolbar-eval-buffer-xpm
		     Atoms-toolbar-eval-buffer
		     t
		     "Expand all evaluatation lines in this buffer"]
		    [:style 3d :size 8]))))
(setq Atoms-toolbar
      (append Atoms-toolbar
	      '([fuse-toolbar-helper-xpm   Atoms-toolbar-helper
					   t "Display Atoms keywords"]
		[fuse-toolbar-document-xpm Atoms-toolbar-document
					   t "Read the Atoms document"]
		[fuse-toolbar-quit-xpm     Atoms-toolbar-quit
					   t "Quit editing this input file"])))

(add-hook 'Atoms-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Atoms-toolbar))))

(defvar Feff-toolbar
  '([fuse-toolbar-template-xpm Feff-toolbar-template
			       t "Write a Feff template"]
    [fuse-toolbar-run-xpm      Feff-toolbar-run
			       t "Run Feff"]
    [fuse-toolbar-log-xpm      Feff-toolbar-log
			       t "Look at output from intrp"]
    [:style 3d :size 8]
    [Feff-toolbar-chi-xpm      Feff-toolbar-chi
			       t "Plot chi(k)"]
    [Feff-toolbar-xmu-xpm      Feff-toolbar-xmu
			       t "Plot mu(E) and mu0(E)"]
    [:style 3d :size 8]
    [fuse-toolbar-helper-xpm   Feff-toolbar-helper
			       t "Display Feff keywords"]
    [fuse-toolbar-document-xpm Feff-toolbar-document
			       t "Read the Feff document"]
    [fuse-toolbar-quit-xpm     Feff-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Feff toolbar.")

(add-hook 'Feff-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Feff-toolbar))))

(defvar Autobk-toolbar
  '([fuse-toolbar-template-xpm   Autobk-toolbar-template
				 t "Write an Autobk template"]
    [fuse-toolbar-run-xpm        Autobk-toolbar-run
				 t "Run Autobk"]
    [fuse-toolbar-log-xpm        Autobk-toolbar-log
				 t "Look at Autobk log file"]
    [:style 3d :size 8]
    [Autobk-toolbar-chi-xpm      Autobk-toolbar-chi
				 t "Plot chi(k) from current stanza"]
    [Autobk-toolbar-xmu-xpm      Autobk-toolbar-xmu
				 t "Plot mu(E) and mu0(E) from current stanza"]
    [Autobk-toolbar-all-xpm      Autobk-toolbar-all
				 t "Plot all chi(k) in file"]
    [:style 3d :size 8]
    [fuse-toolbar-helper-xpm     Autobk-toolbar-helper
				 t "Display Autobk keywords"]
    ;;[Autobk-toolbar-document Autobk-toolbar-document
    ;;		   t "Read the Autobk document"]
    [fuse-toolbar-quit-xpm       Autobk-toolbar-quit
				 t "Quit editing this input file"]
    )
  "The Autobk toolbar.")

(add-hook 'Autobk-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Autobk-toolbar))))

(defvar Feffit-toolbar
  '([fuse-toolbar-template-xpm Feffit-toolbar-template
			       t "Write a Feffit template"]
    [fuse-toolbar-run-xpm      Feffit-toolbar-run
			       t "Run Feffit"]
    [Feffit-toolbar-log-xpm      Feffit-toolbar-log
				 t "Look at Feffit log file"]
    [Feffit-toolbar-prm-xpm      Feffit-toolbar-prm
				 t "Look at Feffit prm file"]
    [:style 3d :size 8]
    [Feffit-toolbar-k-xpm        Feffit-toolbar-k
				 t "Plot chi(k) and fit"]
    [Feffit-toolbar-r-xpm        Feffit-toolbar-r
				 t "Plot chi(R) and fit"]
    [Feffit-toolbar-q-xpm        Feffit-toolbar-q
				 t "Plot back-transformed chi(k) and fit"]
    [:style 3d :size 8]
    [fuse-toolbar-helper-xpm   Feffit-toolbar-helper
			       t "Display Feffit keywords"]
    [fuse-toolbar-quit-xpm     Feffit-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Feffit toolbar.")
    ;;    [Feffit-toolbar-document Feffit-toolbar-document
    ;;			     t "Read the Feffit document"]

(add-hook 'Feffit-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Feffit-toolbar))))

(defvar Normal-toolbar
  '([fuse-toolbar-template-xpm   Normal-toolbar-template
				 t "Write an Normal template"]
    [fuse-toolbar-run-xpm        Normal-toolbar-run
				 t "Run Normal"]
    [:style 3d :size 8]
    [Normal-toolbar-this-nor-xpm Normal-toolbar-this-nor
				 t "Plot un-normalized mu(E) under point"]
    [Normal-toolbar-all-nor-xpm  Normal-toolbar-all-nor
				 t "Plot all un-normalized mu(E) in file"]
    [Normal-toolbar-this-xmu-xpm Normal-toolbar-this-xmu
				 t "Plot un-normalized mu(E) under point"]
    [Normal-toolbar-all-xmu-xpm  Normal-toolbar-all-xmu
				 t "Plot all un-normalized mu(E) in file"]
    [:style 3d :size 8]
    [fuse-toolbar-helper-xpm     Normal-toolbar-helper
				 t "Display Normal keywords"]
    [fuse-toolbar-document-xpm   Normal-toolbar-document
				 t "Read the Normal document"]
    [fuse-toolbar-quit-xpm       Normal-toolbar-quit
				 t "Quit editing this input file"]
    )
  "The Normal toolbar.")

(add-hook 'Normal-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Normal-toolbar))))

(defvar Phit-toolbar
  '([fuse-toolbar-template-xpm Phit-toolbar-template
			       t "Write an Phit template"]
    [fuse-toolbar-run-xpm      Phit-toolbar-run
			       t "Run Phit"]
    [fuse-toolbar-log-xpm      Phit-toolbar-log
			       t "Look at Phit log file"]
    [:style 3d :size 8]
    [Phit-toolbar-plot-xpm     Phit-toolbar-plot
			       t "Plot data and fit"]
    [:style 3d :size 8]
    [fuse-toolbar-helper-xpm   Phit-toolbar-helper
			       t "Display Phit keywords"]
    [fuse-toolbar-document-xpm Phit-toolbar-document
			       t "Read the Phit document"]
    [fuse-toolbar-quit-xpm     Phit-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Phit toolbar.")

(add-hook 'Phit-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (fuse-make-toolbar-function Phit-toolbar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toolbar utilities

(defun fuse-make-toolbar-function (toolbar)
  ;;(if gnuplot-all-buttons-defined
  ;;    (progn
	;;(remove-specifier gnuplot-use-toolbar (current-buffer))
  (fuse-toolbar-setup-toolbar toolbar)
  (add-spec-to-specifier (symbol-value fuse-use-toolbar)
			 toolbar (current-buffer)))

(defvar fuse-toolbar-location "")

(defun fuse-toolbar-setup-toolbar (toolbar)
  (let ((width 46) (height 46)
	(frame (selected-frame))
	(buffer (current-buffer))
	(tag-set '(win)))
    ;;(and fuse-use-toolbar
	;; (fuse-setup-toolbar toolbar nil)
	;; (set-specifier (symbol-value fuse-use-toolbar)
	;;		(cons (current-buffer) toolbar)))
    (cond ((eq (symbol-value fuse-use-toolbar) right-toolbar)
	   ;;(if myframe
	   ;;    (set-specifier right-toolbar toolbar frame tag-set))
	   (setq fuse-toolbar-location          "right")
	   (set-specifier right-toolbar         toolbar buffer)
	   (set-specifier right-toolbar-width   width frame  tag-set))
	  ((eq (symbol-value fuse-use-toolbar) left-toolbar)
	   (setq fuse-toolbar-location          "left")
	   (set-specifier left-toolbar          toolbar buffer)
	   (set-specifier left-toolbar-width    width frame  tag-set))
	  ((eq (symbol-value fuse-use-toolbar) bottom-toolbar)
	   (setq fuse-toolbar-location          "bottom")
	   (set-specifier bottom-toolbar        toolbar buffer)
	   (set-specifier bottom-toolbar-height height frame tag-set))
	  ((eq (symbol-value fuse-use-toolbar) top-toolbar)
	   (setq fuse-toolbar-location          "top")
	   (set-specifier top-toolbar           toolbar buffer)
	   (set-specifier top-toolbar-height    height frame tag-set))) ))

;; (defun fuse-setup-toolbar (bar &optional force)  ;;package)
;;   (let ((dir fuse-glyph-directory)
;; 	(xpm (if (featurep 'xpm) "xpm" "xbm"))
;; 	icon up down disabled name)
;;     ;;(unless package
;;     ;;  (setq message-xmas-glyph-directory dir))
;;     (when dir
;;       (while bar
;; 	(setq icon (aref (car bar) 0)
;; 	      name (symbol-name icon)
;; 	      bar (cdr bar))
;; 	(when (or force
;; 		  (not (boundp icon)))
;; 	  (setq up (concat dir name "-up." xpm))
;; 	  (setq down (concat dir name "-down." xpm))
;; 	  (setq disabled (concat dir name "-disabled." xpm))
;; 	  (if (not (file-exists-p up))
;; 	      (setq bar nil
;; 		    dir nil)
;; 	    (set icon (toolbar-make-button-list
;; 		       up (and (file-exists-p down) down)
;; 		       (and (file-exists-p disabled) disabled)))))))
;;     dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fset toolbar names to actual functions

(fset 'fuse-toolbar-new-file         'fuse-find-file)
(fset 'fuse-toolbar-document         'input-fuse-document)
(fset 'fuse-toolbar-tutorial         'fuse-tutorial)
(fset 'fuse-toolbar-program-document 'input-document)
(fset 'fuse-toolbar-helper           'fuse-mini-help)
(fset 'fuse-toolbar-bug              'input-submit-feedback)
(fset 'fuse-toolbar-quit             'fuse-quit)
(fset 'fuse-toolbar-exit             'save-buffers-kill-emacs)

(fset 'Atoms-toolbar-template    'Atoms-make-template)
(fset 'Atoms-toolbar-run         'input-run-this-program-this-file)
(fset 'Atoms-toolbar-log         'input-jump-to-log-file)
(fset 'Atoms-toolbar-eval-line   'Atoms-evaluate-line)
(fset 'Atoms-toolbar-eval-buffer 'Atoms-evaluate-buffer)
(fset 'Atoms-toolbar-helper      'input-display-keywords)
(fset 'Atoms-toolbar-document    'input-atoms-document)
(fset 'Atoms-toolbar-quit        'fuse-quit)

(fset 'Feff-toolbar-template 'Feff-make-template)
(fset 'Feff-toolbar-run      'input-run-this-program-this-file)
(fset 'Feff-toolbar-log      'Feff-display-log)
(fset 'Feff-toolbar-chi      'Feff-plot-chi)
(fset 'Feff-toolbar-xmu      'Feff-plot-xmu)
(fset 'Feff-toolbar-helper   'input-display-keywords)
(fset 'Feff-toolbar-document 'input-feff-document)
(fset 'Feff-toolbar-quit     'fuse-quit)

(fset 'Autobk-toolbar-template 'Autobk-make-template)
(fset 'Autobk-toolbar-run      'input-run-this-program-this-file)
(fset 'Autobk-toolbar-log      'input-jump-to-log-file)
(fset 'Autobk-toolbar-chi      'Autobk-plot-ksp)
(fset 'Autobk-toolbar-xmu      'Autobk-plot-bkg)
(fset 'Autobk-toolbar-all      'Autobk-plot-all-chi)
(fset 'Autobk-toolbar-helper   'input-display-keywords)
;;(fset 'Autobk-toolbar-document 'input-autobk-document)
(fset 'Autobk-toolbar-quit     'fuse-quit)

(fset 'Feffit-toolbar-template 'fuse-Feffit-choose-template)
(fset 'Feffit-toolbar-run      'input-run-this-program-this-file)
(fset 'Feffit-toolbar-log      'input-jump-to-log-file)
(fset 'Feffit-toolbar-prm      'Feffit-jump-to-prm-file)
(fset 'Feffit-toolbar-k        'Feffit-plot-k)
(fset 'Feffit-toolbar-r        'Feffit-plot-r)
(fset 'Feffit-toolbar-q        'Feffit-plot-q)
(fset 'Feffit-toolbar-helper   'input-display-keywords)
;;(fset 'Feffit-toolbar-document 'input-feffit-document)
(fset 'Feffit-toolbar-quit     'fuse-quit)

(fset 'gnuplot-toolbar-line    'gnuplot-send-line-to-gnuplot)
(fset 'gnuplot-toolbar-region  'gnuplot-send-region-to-gnuplot)
(fset 'gnuplot-toolbar-buffer  'gnuplot-send-buffer-to-gnuplot)
;;(fset 'gnuplot-toolbar-quit    'fuse-quit-gnuplot)
(fset 'gnuplot-toolbar-help    'gnuplot-show-gnuplot-buffer)
(fset 'gnuplot-toolbar-uphist  'fuse-gnuplot-previous-script)
(fset 'gnuplot-toolbar-dnhist  'fuse-gnuplot-next-script)

(fset 'Normal-toolbar-template 'Normal-make-template)
(fset 'Normal-toolbar-run      'input-run-this-program-this-file)
(fset 'Normal-toolbar-this-nor 'Normal-plot-this-norm)
(fset 'Normal-toolbar-all-nor  'Normal-plot-all)
(fset 'Normal-toolbar-this-xmu 'Normal-plot-this-xmu)
(fset 'Normal-toolbar-all-xmu  'Normal-plot-xmu)
(fset 'Normal-toolbar-helper   'input-display-keywords)
(fset 'Normal-toolbar-document 'input-normal-document)
(fset 'Normal-toolbar-quit     'fuse-quit)

(fset 'Phit-toolbar-template 'fuse-Phit-choose-template)
(fset 'Phit-toolbar-run      'input-run-this-program-this-file)
(fset 'Phit-toolbar-log      'input-jump-to-log-file)
(fset 'Phit-toolbar-plot     'Phit-plot-fit)
(fset 'Phit-toolbar-helper   'input-display-keywords)
(fset 'Phit-toolbar-document 'input-phit-document)
(fset 'Phit-toolbar-quit     'fuse-quit)

;;; That's it! ----------------------------------------------------------------

;; any final chores before leaving
(provide 'fuse-toolbar)

;;;============================================================================
;;;
;;; fuse-toolbar.el end here
