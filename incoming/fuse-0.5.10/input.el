;;; input.el --- major mode for editing FEFF and UWXAFS input files

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  13 August 1997
;; Updated:  13 September 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: $

;; This file is not part of GNU Emacs.

;; Copyright (C) 1997, 1998 Bruce Ravel

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

;; New versions of this package (if they exist) may be found at:
;;   http://feff.phys.washington.edu/~ravel/fuse/
;; or
;;   ftp://atropos.nist.gov/pub/bruce/fuse/fuse-#.#.#.tar.gz
;; where #.#.# denotes the version number

;;			   OPERATING SYSTEMS
;; Tested on Linux and SGI Irix.   Developped under Linux.  Should work on
;; most unixes.
;; I have not tested this under VMS, DOS, Win95, WinNT, or OS/2.  Many of
;; the features of FUSE (running programs, plotting) will never work under
;; DOS and may fail under Win95.

;;                      INSTALLATION INSTRUCTIONS
;;
;; Most of these installation instructions are handled by a
;; configuration script and by the makefile distributed with FUSE.
;; Just unpack the distribution and follow the instructions in the
;; INSTALL file.  In short, run these three comands from the command
;; line:
;;       > sh configure  (and answer the questions that you are asked)
;;       > make
;;       > make install
;;
;; The configuration script can add lines to your .emacs file to
;; enable FUSE when editing input files with emacs.  Those lines will
;; look something like this:
;;
;;        (setq load-path (append (list "~/lisp/fuse" ) load-path))
;;        (setq auto-mode-alist
;;              (append (list (cons "\\.inp$" 'input-mode))
;;                      auto-mode-alist))
;;        (autoload 'fuse-mode  "fuse"  "FUSE" t)
;;        (autoload 'input-mode "input" "Input major mode" t)
;;        (add-hook 'dired-load-hook
;;                  '(lambda () (load-library "fuse-dired")))
;;
;; If you add these lines by hand be sure to replace "~/lisp/fuse" by
;; the location of your FUSE installation.
;;
;; See the document for installing FUSE by hand.
;;

;;                        DESCRIPTION OF PROGRAM
;;
;; FUSE is an emacs package for editing the input files to FEFF and
;; the UWXAFS analysis programs.  It contains functions for running
;; the codes from within emacs, plotting the output, generating input
;; files for the codes, and checking the syntax of the input files.
;; It contains many editing shortcuts for commonly performed editing
;; chores and incorporates syntax highlighting using either the
;; font-lock or hilit19 packages.
;;
;; This is not a good place to describe all of the features of FUSE.
;; For that, I refer you to the documentation, which is available in a
;; variety of forms, including postscript, info, and html.  In FUSE,
;; try C-c C-b d for on-line documentation.  All keybindings and
;; functions are documented.  C-h m (M-x describe-mode), C-h k (M-x
;; describe-key), and C-h f (M-x describe-function) should all be
;; useful.

;; Please contact the author (Bruce Ravel <ravel@phys.washington.edu>)
;; with any bug reports, comments, complaints, suggestions, or
;; contributions.  The command M-x input-submit-feedback (C-c C-b b)
;; (Input menu -- Miscellaneous) can be used for bug reports or any
;; other type of communication.

;;  with Imenu support in 0.4.2, speedbar can be tailored to input
;;  files with these lines in .fuse:
;;   (and (featurep 'speedbar)
;; 	  (speedbar-add-supported-extension ".inp"))

;;; History:
;;
;;  See HISTORY file from the FUSE distribution.

;;; Bugs: (well...known bugs)
;;
;; -- time stamp and switch program highlighting in pre 20 XEmacs
;; -- word boundaries in XEmacs colorization
;; -- fuse-doc is over-eager, should behave more like eldoc
;; -- edit input file during a run, lose changes when file is copied back.
;; -- template hotspot marks have trouble with previous markers.
;; -- cleaning code in feff is broken and is deadly slow in many other cases
;; -- infopath handling seems wrong

;;; Code:

(defvar fuse-xemacs-p (string-match "XEmacs" emacs-version)
  "Non-nil if this is XEmacs.")

(require 'time-stamp)
(require 'easymenu)
(require 'cl)

;; this section handles some inconsistencies between FSF and X
(if (fboundp 'frame-iconified-p)
    ()
  (defun frame-iconified-p (frame)
    (equal (frame-visible-p frame) 'icon)) )

(if (fboundp 'deiconify-frame)
    ()
  (defun deiconify-frame (frame)
    (make-frame-visible frame)) )

(if (boundp 'default-frame-plist)
    ()
  (defvar default-frame-plist default-frame-alist))

;; a few more things before we start...
(autoload 'fuse-mode     "fuse"     "fuse-mode"     t)
(autoload 'fuse-quit     "fuse"     "quit editing an input file" t)
(autoload 'fuse-doc-mode "fuse-doc" "fuse-doc-mode" t)

(eval-when-compile (defvar Feff-comment-alist ()))
;; $Id: user-custom.el,v 1.2 1998/03/14 22:46:00 bruce Exp $

;; From custom web page for compatibility between versions of custom:

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))


;; customization stuff
(defgroup fuse nil
  "Feff/UWXAFS System for Emacs."
  :prefix "fuse-"
  :prefix "input-"
  :group 'local)

(defgroup fuse-appearance nil
  "Variables controlling the appearance of FUSE on the screen."
  :prefix "fuse-"
  :prefix "input-"
  :group 'fuse)
(defgroup fuse-indentation nil
  "Indentation and separation parameters."
  :prefix "fuse-"
  :prefix "input-"
  :group 'fuse)
(defgroup fuse-features nil
  "Variables for FUSE's interfaces to other Emacs packages and to
external processes."
  :prefix "fuse-"
  :prefix "input-"
  :group 'fuse)
(defgroup fuse-programs nil
  "Program specific variables in FUSE."
  :prefix "fuse-"
  :prefix "input-"
  :group 'fuse)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; user variable definitions

(defcustom input-load-hook nil
  "*Hooks run when input-el is loaded."
  :group 'fuse
  :type 'hook)
(defcustom input-mode-hook nil
  "*Hooks run when Input mode is entered."
  :group 'fuse
  :type 'hook)

(defcustom input-after-run-hook nil
  "*Hooks run when program execution is finished.
This variable is not buffer local, so your hooks intended for
particular programs should contain conditionals."
  :group 'fuse
  :type 'hook)
;;(make-variable-buffer-local 'input-after-run-hook)
(defcustom input-before-run-hook nil
  "*Hooks run before programs are executed.
This is run very early on in FUSE's execution wrapper.  It comes
right before saving buffers and before any files are renamed and
before the frame/window for display of run-time messages is chosen.

This variable is not buffer local, so your hooks intended for
particular programs should contain conditionals."
  :group 'fuse
  :type 'hook)

					; directory paths
(defcustom fuse-base-directory  (expand-file-name "~/xlisp/fuse/")
  "*Installation directory for FUSE.
If this is set incorrectly, emacs will not be able to find documentation,
scripts needed for plotting, or toolbar icons."
  :group 'fuse
  :type 'file)

(defcustom input-bin-location             (concat fuse-base-directory "scripts/")
  "*Installation location of the scripts and programs distributed with FUSE.
This must be set correctly to use things like *kw*, *mr*, and *intrp*.  By
default this is under `fuse-base-directory'."
  :group 'fuse
  :type 'file)
(defcustom input-document-location        (concat fuse-base-directory "docs/")
  "*Installation location of the FUSE document.
This must be set correctly to use the on-line FUSE document with
\\[input-document].  The forms are info, html, and plain text.  By
default this is under `fuse-base-directory'."
  :group 'fuse
  :type 'file)
(defcustom input-program-document-location (concat fuse-base-directory "docs/")
  "*Installation location of the documents for the various programs.
This must be set correctly to use the on-line FUSE document with
\\[input-document].  The forms are info, html, and plain text.  By
default this is under `fuse-base-directory'."
  :group 'fuse
  :type 'file)
;; (defcustom fuse-glyph-directory           (concat fuse-base-directory "pixmaps/")
;;   "*Installation location of the toolbar icons.
;; This is only used if a toolbar can be displayed, thus this is used in
;; XEmacs and ignored in FSF Emacs.  By default this is under
;; `fuse-base-directory'."
;;   :group 'fuse
;;   :type 'file)

(defcustom fuse-shell "bash"
  "Shell to use to launch asynchronous subprocesses.  Bash is a good choice."
  :group 'fuse
  :type 'string)
(defcustom fuse-shell-switches "-c"
  "Shell command line switches used to lauch asynchronous subprocesses.
These MUST be appropriate to the shell specified by `fuse-shell'."
  :group 'fuse
  :type 'string)

					; appearance customization
(defcustom fuse-inhibit-startup-message nil
  "*Non-nil means to not display startup messages."
  :type 'boolean
  :group 'fuse-appearance)

(defcustom fuse-use-toolbar (if (featurep 'toolbar)
				'left-toolbar
			      nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five legal values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar', although choosing `default-toolbar'
or `top-toolbar' may be a bad idea since either will make the FUSE toolbar
replace the standard toolbar.

This is only used if a toolbar can be displayed, thus this is used in
XEmacs and ignored in FSF Emacs."
  :type '(choice (const default-toolbar)
		 (const top-toolbar)
		 (const bottom-toolbar)
		 (const left-toolbar)
		 (const right-toolbar)
		 (const :tag "No toolbar" nil))
  :group 'fuse-appearance)

(defcustom input-comment-list '((input-mode ?% 1 " "))
  "*Comment specifier list for comment-mode.
This must be a list which is one element long and that element is
itself a list.  The reason for this is that the variable from
comment-mode that controls how commenting is done is an alist.

The first element of the list is the name of the mode (input-mode in
this case), the second is the comment character, the third is the
default number of time to enter that character, the third is the
white space to enter after the character.  This is not used for
*feff*.

See the document for `comment-mode-alist'."
  :group 'fuse-appearance
  :type '(repeat (sexp :tag "List:")))
;; (defcustom input-comment-char               ?%
;;   "*Comment character used in input mode.
;; This should be a single character."
;;   :group 'fuse-appearance
;;   :type 'string)

(defcustom input-mode-variable-comment      "!!&& "
  "*This character string is the comment string in the Local Variables list.
This is pre-appended to the auto-configuration variable
written to the end of the input file by input mode."
  :group 'fuse-appearance
  :type 'string)
(defcustom input-prohibit-autoconfig-flag    nil
  "*Non-nil means not to write auto-configuration comments.
This is set to t if input-program-name is nil, `generic'
or not one of the known names."
  :group 'fuse-appearance
  :type 'boolean)
(defcustom fuse-hide-autoconfig-flag        nil
  "*Non-nil means to hide auto-configuration comments
This is done by covering them with an invisible overlay."
  :group 'fuse-appearance
  :type 'boolean)
(defcustom fuse-always-start-generic-flag         nil
  "*Non-nil means to edit unknown input files as generic.
If FUSE can determine by itself what kind of file the current file is,
then that mode will be used.  If you edit from scratch a file called,
say, foo.inp, then FUSE will drop into generic minor mode rather than
prompting you for the minor mode.  The minor mode can be set
interactively."
  :group 'fuse-appearance
  :type 'boolean)
(defcustom input-never-convert-autoconfig-flag    nil
  "*Non-nil means to never run the autoconfiguration conversion function.
You should leave this nil until you are certain that all of your files
using the old style autoconfiguration lines have been converted."
  :group 'fuse-appearance
  :type 'boolean)

(defcustom input-comment-delimiter
  "%==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+=="
  "*This string delimits the header from the paths in *feffit* input files."
  :group 'fuse-appearance
  :type 'string)
(defvar input-stanza-delimiter
  "--------------------------------------------------------"
  "*Stanza delimiter in several *uwxafs* programs.")
;;  :group 'fuse-appearance
;;  :type 'string)
(defcustom input-upcase-keywords-flag       nil
  "*Non-nil means to write out keywords in uppercase.
In *feff* keywords are always written in uppercase independent of
this variable."
  :group 'fuse-appearance
  :type 'boolean)
(defcustom input-best-fit-set-flag          nil
  "*Non-nil means to swap guess to set when using \\[feffit-insert-best-fit]."
  :group 'fuse-appearance
  :type 'boolean)

					; initialization file customization
(defcustom input-init-file                  "~/.fuse"
  "*Default name of the FUSE initialization file."
  :group 'fuse-appearance
  :type 'file)
					; run-time logfile customization
(defcustom input-run-log-interactive        "fuse-run.log"
  "*Default filename for an interactively saved run-time log.
By default this goes into the current directory."
  :group 'fuse-appearance
  :type 'file)
(defcustom input-run-log                    "~/.fuse-run.log"
  "*Default filename for the run-time log saved when Emacs is exited.
This file serves as a record of your previous session with FUSE."
  :group 'fuse-appearance
  :type 'file)
(defcustom input-run-log-max-lines          1000
  "*Maximum length of the run buffer.
When the length of the buffer exceeds this number, the excess lines
are popped from the beginning of the buffer.  If this is negative or
0, then the run buffer can grow to any size."
  :group 'fuse-appearance
  :type 'integer)

					; program execution customization
(defcustom input-stanza-name                "fuse-stanza.inp"
  "*Name of file used for program execution on an individual stanza.
This is only used in programs which run repeatedly from a single input
file, such as *autobk*."
  :group 'fuse-appearance
  :type 'string)
(defcustom input-beep-flag                  t
  "*Non-nil means to beep is certain situations.
These situations are the end of running the program and other long actions."
  :group 'fuse-appearance
  :type 'boolean)
(defcustom fuse-mouse-overlays-default-flag t
  "Initial setting for mouse overlays in FUSE."
  :group 'fuse-appearance
  :type 'boolean)
;; (defcustom input-use-frames-flag (if (string-match "XEmacs" emacs-version)
;; 				     (or (equal (console-type) 'x)
;; 					 (equal (console-type) 'ns)
;; 					 (equal (console-type) 'win32))
;; 				   window-system)

(defvar input-xemacs-windowing (and (string-match "XEmacs" emacs-version)
				    (or (equal (console-type) 'x)
					(equal (console-type) 'ns)
					(equal (console-type) 'win32)))
  "Non-nil in in a windowing environment under XEmacs.")
(defvar input-emacs-windowing (and (not (string-match "XEmacs" emacs-version))
				   window-system)
  "Non-nil in in a windowing environment under FSF Emacs.")
(defcustom input-use-frames (if (or input-xemacs-windowing
				    input-emacs-windowing)
				'own
			      nil)
  "*Non-nil means to use frames.
If this equals 'own then gnuplot and the run-time buffers will get their
own frames.  If it equals 'share, then they share a frame.  If it
is nil, then an additional frame will not be used.  In a non-wondowing
environment, this is nil by default.  When nil, the main frame will split
into two windows and the run or gnuplot buffer will be placed in the
other window."
  :type '(radio (const :tag "run and gnuplot in separate frames" own)
		(const :tag "run and gnuplot share frames" share)
		(const :tag "don't use frames" nil))
  :group 'fuse-appearance)
(defcustom input-always-raise-flag          t
  "*Non-nil means to always raise an iconified or lowered frame.
If nil, then FUSE will leave a selected frame in its current state."
  :group 'fuse-appearance
  :type 'boolean)

(defcustom input-doc-frame-plist
    (append (list 'width 80)
	    (let ((h (plist-get default-frame-plist 'height)))
	      (if h (list 'height h) (list  'height 30))))
    "Frame plist for the input document display frame."
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value")))
  :group 'fuse-appearance)
(defcustom input-doc-frame-parameters '((height . 30)
					(width  . 80))
  "Frame parameters for the input document display frame in FSF Emacs."
  :group 'fuse-appearance
  :type '(repeat (sexp :tag "Parameter:")))


					; run-time display window
(defcustom input-run-frame-plist
  '(minibuffer nil height 17 width 65 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p nil)
  "Frame plist for the input run-time display frame in XEmacs."
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value")))
  :group 'fuse-appearance)
(defcustom input-run-frame-parameters '((minibuffer . nil)
					(height . 17)
					(width . 65)
					(border-width . 0)
					(menu-bar-lines . 0)
					(unsplittable . t))
  "Frame parameters for the input run-time display frame in FSF Emacs."
  :group 'fuse-appearance
  :type '(repeat (sexp :tag "Parameter:")))

(defcustom input-gnuplot-frame-plist
  '(minibuffer t height 20 width 55 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p t)
  "Frame plist for the input gnuplot frame in XEmacs."
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value")))
  :group 'fuse-appearance)
(defcustom input-gnuplot-frame-parameters '((minibuffer . t)
					    (height . 17)
					    (width . 55)
					    (border-width . 0)
					    (unsplittable . t))
  "Frame parameters for the input gnuplot frame in FSF Emacs."
  :group 'fuse-appearance
  :type '(repeat (sexp :tag "Parameter:")))



					; indentation customization
(defcustom input-stanza-indent              0
  "*Determine indentation for stanzas.
Number of spaces or tabs placed at beginning of stanza lines as
indentation characters.  Positive values denote a number of spaces,
negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-path-paragraph-indent      0
  "*Determine indentation for path-paragraphs.
Number of spaces or tabs placed at beginning of path paragraph lines as
indentation characters.  Positive values denote a number of spaces,
negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-path-paragraph-separate   -1
  "*Determine separation between path paragraph fields.
Number of spaces or tabs placed between path paragraph fields.
Positive values denote a number of spaces, negative values denote
a number of tabs.  0 means 1 tab."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-set-guess-indent           0
  "*Determine indentation for set and guess lines.
Number of spaces or tabs placed before set, guess, and local fields.
Positive values denote a number of spaces, negative values denote a
number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-set-guess-separate        -1
  "*Determine separation between set and guess fields.
Number of spaces or tabs placed between set, guess, and local fields.
Positive values denote a number of spaces, negative values denote a
number of tabs.  0 means 1 tab."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-list-indent                2
  "*Determine indentation for most lists.
Number of spaces or tabs placed at beginning of list lines as
indentation characters.  Positive values denote a number of spaces,
negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-list-separate             -1
  "*Determine separation for most lists.
Number of spaces or tabs placed between list fields.  Positive values
denote a number of spaces, negative values denote a number of tabs.
0 means 1 tab."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-feff-indent                1
  "*Determine additional indentation for *feff* input files.
Number of additional spaces or tabs placed at beginning of lines in
a *feff* input file as indentation characters.  Positive values denote
a number of spaces, negative values denote a number of tabs.  0 means
no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-potentials-indent          7
  "*Determine indentation for potential list in *feff* input files.
Number of spaces or tabs placed at beginning of potentials list lines
in the *feff* input file as indentation characters.  Positive values
denote a number of spaces, negative values denote a number of tabs.
0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-potentials-separate        3
  "*Determine separation for potential list in *feff* input files.
Number of spaces or tabs placed between columns of potentials list
lines in the *feff* input file.  Positive values denote a number of
spaces, negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-atoms-separate             3
  "*Determine separation for atoms list in *feff* input files.
Number of spaces or tabs placed between columns of the atoms list
lines in the *feff* input file.  Positive values denote a number
of spaces, negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)
(defcustom input-comment-indent             0
  "*Determine separation for comment lines.
Number of spaces or tabs placed at beginning of comment lines
as indentation characters.  Positive values denote a number of spaces,
negative values denote a number of tabs.  0 means no space."
  :group 'fuse-indentation
  :type 'integer)

(defcustom input-emulation nil
  "*Editor emulation mode for use with FUSE.
This variable is used to assure that an emulation mode is entered when
FUSE is started.  The values are \"vi\", \"crisp\", and \"edt\"."
  :group 'fuse-features
  :type '(radio (const :tag "no emulation"        nil)
		(const :tag "emulate Vi"          "vi")
		(const :tag "emulate Crisp/Brief" "crisp")
		(const :tag "emulate edt"         "edt")  ))

					; intrp customization
(defcustom input-intrp-buffer-name          "intrp.dat"
  "*Name of buffer for displaying output from *intrp*."
  :group 'fuse-features
  :type 'string)
(defcustom input-intrp-args                 ""
  "*Additional command line arguments to *intrp*."
  :group 'fuse-features
  :type 'string)

					; mcmaster/feffit customization
(defcustom input-mcmaster-sigma             "sigmm"
  "*Default name for the McMaster sigma^2 correction.
For use in *feffit* math expressions.  Used by \\[feffit-insert-mcmaster]."
  :group 'fuse-features
  :type 'string)
(defcustom input-mcmaster-fourth            "qrtmm"
  "*Default name for the McMaster fourth cumulant correction.
For use in *feffit* math expressions.  Used by \\[feffit-insert-mcmaster]."
  :group 'fuse-features
  :type 'string)
(defcustom input-mcmaster-ampfac            "ampfac"
  "*Default name for the self absorption amplitude correction.
For use in *feffit* math expressions.  Used by \\[feffit-insert-mcmaster]."
  :group 'fuse-features
  :type 'string)

					; gnuplot customization
(defcustom gnuplot-script-buffer-name       "fuse.gp"
  "*Name of buffer to which *gnuplot* scripts are written.
This is also the default file name for saving the gnuplot script
when \\[save-buffer] is typed."
  :group 'fuse-features
  :type 'string)
(defcustom input-gnuplot-r-column           4
  "*Default column number for R space *feffit* plots.
The options are [real, imaginary, magnitude, phase] = [2,3,4,5]."
  :group 'fuse-features
  :type '(radio (const :tag "Real part,      column 2" 2)
		(const :tag "Imaginary part, column 3" 3)
		(const :tag "Magnitude,      column 4" 4)
		(const :tag "Phase,          column 5" 5)))
(defcustom input-gnuplot-q-column           2
  "*Default column number for back transform k space *feffit* plots.
The options are [real, imaginary, magnitude, phase] = [2,3,4,5]."
  :group 'fuse-features
  :type '(radio (const :tag "Real part,      column 2" 2)
		(const :tag "Imaginary part, column 3" 3)
		(const :tag "Magnitude,      column 4" 4)
		(const :tag "Phase,          column 5" 5)))
(defcustom input-gnuplot-data-style         "lines"
  "*Default line style for *gnuplot* plots."
  :group 'fuse-features
  :type '(radio (const "lines") (const "points") (const "linespoints")) )
(defcustom input-gnuplot-default-terminal   "x11"
  "*Default terminal type for *gnuplot* plots."
  :group 'fuse-features
  :type '(radio (const "x11")
		(const "postscript")))
(defvar input-gnuplot-terminal nil)
(defcustom input-gnuplot-default-ps-file    "fuse.ps"
  "*Default file name for *gnuplot* postscript output."
  :group 'fuse-features
  :type 'file)
(defvar input-gnuplot-postscript-file nil)
(defcustom input-gnuplot-ezero-flag         nil
  "*Non-nil means to show e0 in *autobk* background plots."
  :group 'fuse-features
  :type 'boolean)

(defcustom input-plot-flag                  t
  "*Non-nil means to send gnuplot script to gnuplot.
Nil means to write the script but not to fire up gnuplot.  This option
is useful for terminals incapable of graphics, e.g. Emacs running on
the Linux console."
  :group 'fuse-features
  :type 'boolean)
;; (defcustom input-pause                      0.0
;;   "*A number of seconds used by plotting functions to determine how
;; long to wait for *gnuplot* to finish plotting before going to check for
;; errors.  Increase this number if the gnuplot-program buffer is shown even
;; when a plot is correctly drawn.")

					; on-line documentation customization
(defcustom input-document-type              "info"
  "*Document type to use on-line.
This is one of `info', `html' and `text'.  `info' is the default.  The
info reader, w3, or a read-only text buffer will be used as appropriate
to display the documentation."
  :group 'fuse-features
  :type '(radio (const :tag "Emacs info, display using the info reader" "info")
		(const :tag "Hypertext,  display using w3"              "html")
		(const :tag "Plain text, display in a read-only buffer" "text")) )



					; time stamp customization
(defcustom input-time-stamp-flag            t
  "*Non-nil means to time stamp input files."
  :group 'fuse-features
  :type 'boolean)
(defcustom input-time-stamp-begin           "%# Input-mode Time-stamp: <"
  "*Beginning part of the time stamp string.
This must contain the string `Time-stamp: <', but can start with any
sequence of characters.  FUSE ends the time stamp string with a `>'"
  :group 'fuse-features
  :type 'string)
(defcustom input-time-stamp-line-limit      -8
  "*See document for `time-stamp-line-limit'.
This should be a negative number for time stamp to be at the bottom
of the file."
  :group 'fuse-features
  :type 'integer)

					; program specific customizations
;; (defcustom Atoms-auto-jump-flag             nil
;;   "*Non-nil means to jump to feff.inp immediately after running atoms.
;; This is only used in atoms minor mode."
;;   :group 'fuse-programs
;;   :type 'boolean)
(defcustom Feff-8-convergence-filename      "convergence.dat"
  "*Name of file containing convergence date.
This file will contain the Fermi energies as a function of iteration
index.  Various electronics results are also written to this file, such
as charge transfer and orbital occupancy.
Used by `Feff-8-write-convergence-data'."
  :group 'fuse-programs
  :type 'string)


					; faces

(cond ((and (featurep 'custom) (fboundp 'custom-declare-variable))
       (defface fuse-mouse-face '((((class color))
				   (:background "orange"))
				  (t
				   (:underline t)))
	 "Mouse face used when mouse over a readable filename."
	 :group 'fuse-appearance)
       (defface fuse-hotspot-face '((((class color))
				   (:background "salmon"))
				  (t
				   (:reverse t)))
	 "Face used to mark tempo hotspots."
	 :group 'fuse-appearance)
       (defface Feffit-path-face '((((class color))
				    (:background "pink"))
				   (t
				    (:reverse t)))
	 "Face used to mark feffit paths."
	 :group 'fuse-appearance))
      (t
       (copy-face 'highlight 'fuse-mouse-face)
       (set-face-background  'fuse-mouse-face   "orange")
       (copy-face 'modeline  'fuse-hotspot-face)
       (set-face-background  'fuse-hotspot-face "salmon")
       (copy-face 'modeline  'Feffit-path-face)
       (set-face-background  'Feffit-path-face  "pink")))


;;;; there are no variables intended for user customization below this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun input-customize ()
  "Customize FUSE using the Custom package."
  (interactive)
  (if (featurep 'custom)
      (customize-group 'fuse)
    (message "The custom library is not being used.")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The following variables are some lists that need to be set
;;;; correctly at install time.  As written here they are appropriate
;;;; to the status of the distribution as assembled by the author, but
;;;; may be altered by the installer to reflect which documents and
;;;; minor modes are actually installed.

;; $Id: variables.el,v 1.2 1998/03/14 22:46:01 bruce Exp $

(defvar input-documents-exist
  '(("atoms"  . "atoms")
    ("feff"   . "feff7")
    ("normal" . "normal")
    ("phit"   . "phit")
    ("fuse"   . "fuse" ))
  "*An alist of programs for which on-line documentation exists.
The car is the generic name of the program and the cdr is the file name
sans extension of the documents")

(defvar input-programs-alist
  '(("atoms"   . Atoms-mode)
    ("autobk"  . Autobk-mode)
    ("feff"    . Feff-mode)
    ("feffit"  . Feffit-mode)
    ("generic" . Generic-mode)
    ;;("xanes"   . Xanes-mode)
    ;;("correct" . Correct-mode)
    ("normal"  . Normal-mode)
    ("fluo"    . Fluo-mode)
    ("phit"    . Phit-mode)
    ("diffkk"  . Diffkk-mode)
    ("quit input-mode" . text-mode) )
  "*An alist of program names and program minor modes.
This is used by to determine how to call the minor mode.  There should
be one entry for each minor mode in *FUSE*.  The car is the program
name and the cdr is the minor mode function.  This is the alist that must
be appended to when a new minor mode is installed.  This can be done in
the .fuse file by

  (setq input-programs-alist
        (append input-programs-alist '((\"foo\"  . Foo-mode)) ))

")

(defvar input-emulation-list '("vi" "crisp" "edt")
  "List of known editor emulators available to Emacs.")

;;;; end of variables to be set at installation time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; constants
(defconst input-mode-version "0.5.10")
(defconst input-author       "Bruce Ravel")
(defconst input-author-email "ravel@phys.washington.edu")

; (defconst input-font-lock-keywords   nil
;   "Subdued level highlighting for Input mode.")
; (defconst input-font-lock-keywords-1 nil
;   "Subdued level highlighting for Input mode.")
; (defconst input-font-lock-keywords-2 nil
;   "Gaudy level highlighting for Input mode.")


;;;; regular expression constants
(defconst input-t-f-expr
  "\\<\\(t\\(rue\\)?\\|f\\(alse\\)?\\|no?\\|y\\(es\\)?\\)\\>")
(defconst input-num-expr
  "\\<-?[0-9]+\\(.[0-9]*\\)?\\([ed][+-]?[0-9]+\\)?\\>")  ;; eEdD
(defconst input-int-expr "\\<-?[0-9]+\\>")
(defconst input-one-word-expr "^\\w+$")
(defconst input-datatype-regexp "\\<\\(uwe?x?\\|asc\\|col\\)")
(defconst input-format-regexp "\\<\\(asc\\|col\\|uwe?x\\)")
(defconst input-window-regexp "\\<\\(han\\|gau\\|lor\\|par\\|wel\\)")
(defconst input-end-of-word "\\>[,= \t]?")
(defconst input-beginning-of-word       ; this is necessary???
  "\\<")
(defconst input-word-sep      ; this matches the definition used in the
  "[ \t]*[ \t,=][ \t]*")      ; fortran subroutine bwords in feff and uwxafs
(defconst input-comment-expr "[%#!]")
(defconst input-beginning-of-line-comment "^[ \t]*\\(%\\|#\\|!\\|\\*\\)")
(defconst input-path-paragraph-regexp
  "^[ \t]*\\(3rd\\|4th\\|amp\\|cubic\\|d\\(el\\(r\\|tar\\)\\|phase\\|wf\\)\\|e\\([0ei]\\|0shift\\)\\|f\\(eff\\|ourth\\|un\\(\\|c\\(\\|tion\\)\\)\\)\\|id\\|p\\(ath\\|hase\\)\\|quartic\\|s\\(02\\|igma2\\|o2\\|s2\\)\\|third\\)[ \t]+")
(defvar input-title-line-regexp "title\\|comment")


(defconst input-atoms
  '("H"                                "He"
    "Li" "Be" "B"  "C"  "N"  "O"  "F"  "Ne"
    "Na" "Mg" "Al" "Si" "P"  "S"  "Cl" "Ar"
    "K"  "Ca"
              "Sc" "Ti" "V"  "Cr" "Mn" "Fe" "Co" "Ni" "Cu" "Zn"
	      "Ga" "Ge" "As" "Se" "Br" "Kr"
    "Rb" "Sr"
              "Y"  "Zr" "Nb" "Mo" "Tc" "Ru" "Rh" "Pd" "Ag" "Cd"
              "In" "Sn" "Sb" "Te" "I"  "Xe"
    "Cs" "Ba" "La"
         "Ce" "Pr" "Nd" "Pm" "Sm" "Eu" "Gd" "Tb" "Dy" "Ho" "Er" "Tm" "Yb" "Lu"
	           "Hf" "Ta" "W"  "Re" "Os" "Ir" "Pt" "Au" "Hg"
              "Tl" "Pb" "Bi" "Po" "At" "Rn"
    "Fr" "Ra" "Ac"
         "Th" "Pa" "U"  "Np" "Pu" "Am" "Cm" "Bk" "Cf" "Es" "Fm" "Md" "No" "Lr"))

;;(defconst input-edges "\\<k\\|l[123]\\|[mno][1-5]\\>")
(defconst input-edges '("no", "k" "l1" "l2" "l3" "m1" "m2" "m3" "m4"
			"m5" "n1" "n2" "n3" "n4" "n5" "n6" "n7" "o1"
			"o2" "o3" "o4" "o5" "o6" "o7" "p1" "p2" "p3"))


;;;; variables not meant for user configuration
(defvar input-mode-name "Input")
(defvar input-program-name nil
  "Name of program in current buffer.")
(defvar input-included-program nil
  "Program name for a jumped-to include file.
This is needed so that it enters the correct minor mode.  When probed
by \\[describe-variable], this should ALWAYS be nil.  DO NOT RESET
this variable.")
(defvar input-program-buffer "\*input-mode process\*"
  "Name of buffer that programs display to while running.")
(defvar input-originating-buffer nil
  "Name of buffer containing an input file visited prior to current buffer.
This is used by \\[input-back-to-original] to return to the prior buffer.")
(make-variable-buffer-local 'input-originating-buffer)
;;(defvar input-gnuplot-terminal nil)
(defvar input-current-terminal nil
  "Contnuously update record of gnuplot terminal.
This is used to avoid resetting terminal if default (x11) type is chosen.")
(defvar input-process "input-process")
(defvar input-current-feature nil
  "Symbol of feature corresponding to the program in the current buffer.")
;; (defvar input-window nil)
;; (if (string-match "XEmacs" emacs-version)
;;     (setq input-window (console-type))
;;   (setq input-window window-system))
(defvar input-used-tempo-flag nil
  "Non-nil means a template has been written using tempo.")
(make-variable-buffer-local 'input-used-tempo-flag)
(defvar input-use-hilit19
  (and window-system (featurep 'hilit19))
  "Decide if hilit19 is used for colorization.
This is true if the emacs session is in a windowing environment and if
hilit19 is a feature.")
(defvar input-use-font-lock (featurep 'font-lock)
  "Decide if font-lock is used for colorization.
This is true if font-lock is a feature.")
(defvar input-output-files '()
  "List of output files generated by the program.")
;;(defvar input-current-output-files ())
;;(make-variable-buffer-local 'input-output-files)
(defvar input-current-keywords-alist ()
  "This alist is set to the keyword alist for the current feff/uwxafs
program.  The car of each association is a valid keyword.  The cdr of
each association is a list of attributes of that keyword.  The
attributes are: 0 -- The number of valid arguments.  1 -- The argument
type(s) of the keyword.  2 -- a description of the valid argument(s)
of that keyword

The argument type, cdr element 1, can be a string or a list of
strings.  The string or each element of the list must be one of the
type tabulated below.  If it is a string, then it specifies the
arguments in the order that they should appear in the input file.

The valid types are
  number     a floating point number or an integer
  integer    an integer
  logical    a logical value (true, false, yes, no)
  title      a user defined title line
  writable   a valid output filename
  readable   a valid input filename
  datatype   an i/o file format (uwxafs|ascii|column)
  window     a window type (hanning|welch|parzen|gaussian|lorentzian)
  none       the keyword takes no argument
  word       a string of word characters
  symbol     a string of word and symbol characters
  spcgrp     a space group symbol of up to 10 characters
  element    an element from the periodic table
  edge       an x-ray absorption edge (K, L1, L2, L3, M1, etc)
  skey       a five character alphanumeric, the symbolic key from uwxafs files
  tag        a string defined by current program
               in atoms: an atom tag
               in feff:  an ipot
               in feffit & phit: perhaps a set or guess value
  pathparam  for feffit and phit
  ignore     ignore the keyword value and jump forward by number of arguments
  setguess   a string followed by a math expression
")
;(make-variable-buffer-local 'input-current-keywords-alist)


;; These variables are used by input mode to know characteristics of
;; the input files under each of the minor mode.  For example, only
;; some programs allow master and include files.  These programs would
;; have `input-program-master-flag' set to t.  These are all buffer local.
(defvar input-program-setguess-flag    nil
  "Non-nil means program uses set and guess variables.")
(make-variable-buffer-local 'input-program-setguess-flag)
(defvar input-program-master-flag      nil
  "Non-nil means program uses master and include files.")
(make-variable-buffer-local 'input-program-master-flag)
(defvar input-program-logfile-flag     nil
  "Non-nil means program writes a log file.")
(make-variable-buffer-local 'input-program-logfile-flag)
(defvar input-program-stanza-flag     nil
  "Non-nil means program uses stanzas.")
(make-variable-buffer-local 'input-program-stanza-flag)
(defvar input-program-data-flag        nil
  "Non-nil program uses data files.")
(make-variable-buffer-local 'input-program-data-flag)
(defvar input-program-feff-flag        nil
  "Non-nil means program uses feff files.")
(make-variable-buffer-local 'input-program-feff-flag)
(defvar input-program-list-flag        nil
  "Non-nil means program uses list structures for data files.")
(make-variable-buffer-local 'input-program-list-flag)
(defvar input-program-kweight-flag     nil
  "Non-nil means program uses chi(k) data.")
(make-variable-buffer-local 'input-program-kweight-flag)
(defvar input-program-eshift-flag     nil
  "Non-nil means program has a plot function which uses the eshift script.")
(make-variable-buffer-local 'input-program-eshift-flag)
(defvar input-program-program-author   '(nil nil)
  "Used to identify author and email address of author for the program.")
(make-variable-buffer-local 'input-program-program-author)
(defvar input-program-parse            '(nil nil)
  "Used to identify the parsing function and feature alist for the minor mode.")
(make-variable-buffer-local 'input-program-parse)
(defvar input-program-hilit            nil
  "Function used to refontify input file buffer.")

(defvar fuse-mouse-highlight-list '()
  "List of regular expressions to use for setting mouse highlighting.
This is a list where elements are '(regexp count).  REGEXP is the
regular expression to match and count is the subexpression to be
highlighted as in `match-beginning' and `match-end'.")
(defvar fuse-mouse-overlays-flag nil)
(setq fuse-mouse-overlays-flag fuse-mouse-overlays-default-flag)

;; Values of variables set by configuration comments upon reading file
(defvar input-data-path nil
  "*Directory path for data input files.  Local to buffer.")
(defvar input-feff-path nil
  "*Directory path for feff input files.  Local to buffer.")
(defvar input-out-path nil
  "*Directory path for output files.  Local to buffer.")
(defvar input-k-weight "1"
  "*Default k-weight to use for plotting chi(k).")
(defvar input-master nil
  "*Name of master for multi-file input file.")
(defvar input-program-version nil
  "*Version number of program to be used with this file.
This is a kludge to handle multiple version numbers.  Suppose you want to
run feff 6.01 with some feff.inp files and feff 7.02 with others.  If 6.01
is the default then it may be named \"feff\" and this variable may be nil.
If 7.02 is to be run and it is named \"feff702\", then this variable should
be \"702\".")
(defvar input-eshift "0"
  "*Amount of e0 shift to apply to data file.
This is currently used only in the xmu plotting function for Feff8.")
(make-variable-buffer-local 'input-eshift)
;;;(defvar input-program-for-file nil
;;;  "*Name of program associated with file.  Used if not obvious.")

;;;; mode and minor mode information
(defvar input-mode-abbrev-table nil
  "Abbrev table in use in `input-mode' buffers.")
(define-abbrev-table 'input-mode-abbrev-table ())


(defvar input-font-lock-keywords-1 nil)
(defvar input-font-lock-keywords-2 nil)
(defvar input-font-lock-keywords   nil
  "Default expressions to highlight in Input mode.")

(defvar input-init-file-loaded nil)

(defvar input-doc-frame nil
  "The frame in which info is displayed.")
(defvar input-run-frame nil
  "The frame in which the run buffer is displayed.")
(defvar input-gnuplot-frame nil
  "The frame in which the gnuplot script buffer is displayed.")
(defvar gnuplot-buffer nil)

(defvar input-stanza-program nil
  "Used to communicate name of current program for a run on current stanza.")

;; variables used to control batch processing via dired buffer
(defvar input-batch-flag nil
  "Non-nil means a batch job is running.
Don't change this by hand!")
(defvar input-batch-files ()
  "List of marked files for a FUSE batch job.
Don't change this by hand!")
(defvar input-batch-index 0
  "Index in `input-batch-files' of current input file in batch job.
Don't change this by hand!")
(defvar input-started-by-batch-flag nil
  "Non-nil means the process was started during a batch run.
Don't change this by hand!")

(defvar fuse-running-this-file nil)

(defvar fuse-startup-displayed nil
  "Non-nil means that the startup messages have already been displayed.")
(defvar fuse-startup-message-lines
    '("Please use \\[input-submit-feedback] to report bugs."
      "Use \\[input-fuse-document] to read the FUSE document."
      "Use \\[input-display-keywords] to see descriptions of program keywords."
      "FUSE comes with absolutely no warranty."
      "Feel free to distribute copies of FUSE."
      "Thanks for using FUSE.  Have fun!"))
;;       "You may give out copies of FUSE.  Type \\[vm-show-copying-restrictions] to see the conditions"
;;     "VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"


;;(set 'input-gnuplot-run-buffer 'gnuplot-run-buffer)

;;;; end of variable definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code for key bindings in input mode

;; $Id: bindings.el,v 1.2 1998/03/14 22:45:58 bruce Exp $

;; functions defined in this section
;;    input-t ()

(defvar input-mode-map ()
  "Keymap used in input mode.")
(if input-mode-map
    ()
  (setq input-mode-map (make-sparse-keymap))

					; keyword functions
  (define-key input-mode-map "\M-\t"      'input-complete-keyword)
  (define-key input-mode-map "\M-?"       'input-arg-for-this-keyword)
  (define-key input-mode-map "\M-\r"      'input-check-this-keyword-arg)
  (define-key input-mode-map "\C-c;"      'comment-out-region)
					; cleaning
  (define-key input-mode-map "\C-c\C-cl"  'input-clean-line)
  (define-key input-mode-map "\C-c\C-i"   'input-clean-line)
  (define-key input-mode-map "\C-c\C-cr"  'input-clean-region)
  (define-key input-mode-map "\C-c\C-cf"  'input-clean-file)

					; tempo motion
  (define-key input-mode-map "\M-n"      'tempo-forward-mark)
  (define-key input-mode-map "\M-p"      'tempo-backward-mark)
  (define-key input-mode-map "\C-c\C-tc" 'fuse-clear-hotspot-overlays)

					; shortcut (swapping) functions
  (define-key input-mode-map "\C-c\C-ss"  'input-guess-to-set)
  (define-key input-mode-map "\C-c\C-sc"  'input-swap-comment)
  (define-key input-mode-map "\C-c\C-sa"  'input-true-false)

					; look at file
  (define-key input-mode-map "\C-c\C-fl"  'input-jump-to-log-file)
  (define-key input-mode-map "\C-c\C-fd"  'input-jump-to-data-file)
  (define-key input-mode-map "\C-c\C-fa"  'input-jump-to-master-file)

					; i/o files paths and other variables
  (define-key input-mode-map "\C-c\C-dd"  'input-set-data-path)
  (define-key input-mode-map "\C-c\C-df"  'input-set-feff-path)
  (define-key input-mode-map "\C-c\C-do"  'input-set-out-path)
  (define-key input-mode-map "\C-c\C-da"  'input-set-all-paths)
  (define-key input-mode-map "\C-c\C-dk"  'input-set-k-weight)
  (define-key input-mode-map "\C-c\C-de"  'input-set-eshift)
  (define-key input-mode-map "\C-c\C-dm"  'input-set-master)
  (define-key input-mode-map "\C-c\C-dv"  'input-set-version)
  (define-key input-mode-map "\C-c\C-di"  'input-insert-filename)
  (define-key input-mode-map "\C-c\C-dz"  'fuse-insert-z-number)

					; running programs
  (define-key input-mode-map "\C-c\C-ra"  'input-run-any-program-any-file)
  (define-key input-mode-map "\C-c\C-rr"  'input-run-this-program-this-file)
  (define-key input-mode-map "\C-c\C-rk"  'input-kill-program)

					; plotting
  (define-key input-mode-map "\C-c\C-ps"  'input-toggle-gnuplot-terminal)
  (define-key input-mode-map "\C-c\C-fg"  'input-jump-to-gnuplot-buffer)
  (define-key input-mode-map "\C-c\C-fk"  'fuse-kill-gnuplot)

  (if (and (featurep 'custom) (fboundp 'input-customize))
      (define-key input-mode-map "\C-c\C-hc"  'input-customize))

  (define-key input-mode-map "\C-c\C-ba"  'input-set-configuration)
  (define-key input-mode-map "\C-c\C-bb"  'input-submit-feedback)
  (define-key input-mode-map "\C-c\C-bc"  'input-customize)
  (define-key input-mode-map "\C-c\C-bd"  'input-document)
  (define-key input-mode-map "\C-c\C-bf"  'input-fuse-document)
  (define-key input-mode-map "\C-c\C-bk"  'input-display-keywords)
  (define-key input-mode-map "\C-c\C-bl"  'input-save-run-log)
  (define-key input-mode-map "\C-c\C-bm"  'fuse-display-startup-message)
  (define-key input-mode-map "\C-c\C-bo"  'input-visit-previous-run-log)
  (define-key input-mode-map "\C-c\C-bp"  'input-submit-program-bug-report)
  (define-key input-mode-map "\C-c\C-bs"  'input-determine-program)
  (define-key input-mode-map "\C-c\C-bv"  'input-show-version)
  (define-key input-mode-map "\C-c\C-bt"  'fuse-doc-mode)

  (define-key input-mode-map "\C-m"       'input-newline)
  (cond (fuse-xemacs-p
      	 (define-key input-mode-map '(shift button3) 'input-set-and-jump))
	(t
	 (define-key input-mode-map [S-mouse-3]  'input-set-and-jump)
	 (define-key input-mode-map [?\C-\S-l]   'input-repaint-command)) )
  )


;; This is used for returning from a jumped-to buffer.  I tried to
;; pick a key-binding that is probably not in other use.
(if fuse-xemacs-p
    (global-set-key '(control meta ?`) 'input-back-to-original)
  (global-set-key [?\C-\M-`]  'input-back-to-original))

;; This has become rather messy, but using the :active thingies, I can grey
;; options that will not work in certain situations.  I hope the greater
;; reliability/usability of the code merits the increased ugliness
(defvar input-mode-menu nil)
(easy-menu-define
 input-mode-menu input-mode-map
 "Menu used in Input mode"
 '("Input"
   [" >> FUSE <<"                          fuse-mode t]
   ;;:active (featurep 'fuse)]
   ("Toggled features"
    ["Fuse-Doc mode"                        fuse-doc-mode
     :style toggle :selected (and (featurep 'fuse-doc) fuse-doc-mode)]
    ["Mouse highlighting in this file"     fuse-toggle-mouse-overlays
     :style toggle :selected fuse-mouse-overlays-flag]
    ["Hide/show autoconfig variables"      fuse-toggle-hide-autoconfig
     :style toggle :selected fuse-hide-autoconfig-flag])
   ("Editing Shortcuts"
    ["(Un)Comment line"                    input-swap-comment t]
    ["Swap set and guess"                  input-guess-to-set
     :active input-program-setguess-flag]
    ["Swap logical value"                  input-true-false
     :active (not (string= input-program-name "feff"))]
    ["Insert a filename at point"          input-insert-filename t]
    ["Insert a Z number at point"          fuse-insert-z-number t]
    )

   ("Clean up"
    ["Clean line"                          input-clean-line t]
    ["Clean region"                        input-clean-region t]
    ["Clean file"                          input-clean-file t])

   ("Keyword operations"
    ["Display keywords, current program"   input-display-keywords t]
    ["Check keyword"                       input-check-this-keyword-arg t]
    ["Complete keyword"                    input-complete-keyword t]
    ["Describe argument of keyword"        input-arg-for-this-keyword t]
    ["Next template hotspot"               tempo-forward-mark
     :active input-used-tempo-flag]
    ["Previous template hotspot"           tempo-backward-mark
     :active input-used-tempo-flag]
    ["Clear hotspot-markings"              fuse-clear-hotspot-overlays
     :active input-used-tempo-flag])

   ("Visit files"
    ["Look at log file"                    input-jump-to-log-file
     :active (input-log-file-exists-p)]
    ["Look at data file under point"       input-jump-to-data-file
     :active input-program-data-flag]
    ["Look at master file"                 input-jump-to-master-file
     :active input-program-master-flag])

   ("Set Variables"
    ["Set path to input data"              input-set-data-path
     :active input-program-data-flag]
    ["Set path to feff files"              input-set-feff-path
     :active input-program-feff-flag]
    ["Set output path"                     input-set-out-path t]
    [" >> Set all three paths"             input-set-all-paths
     :active (and input-program-data-flag  input-program-feff-flag)]
    ["Set k-weight"                        input-set-k-weight
     :active input-program-kweight-flag]
    ["Set e0 shift"                        input-set-eshift
     :active input-program-eshift-flag]
    ["Set master file"                     input-set-master
     :active input-program-master-flag]
    ["Set program version"                 input-set-version t])

   ("Run programs"
    ["Run current program on master file"  input-run-this-program-this-file t]
    ["Run any program, any file"           input-run-any-program-any-file t]
    ["Kill running process"                input-kill-program
     :active (get-process input-process)])

   ("Gnuplot"
    ["Toggle gnuplot terminal"             input-toggle-gnuplot-terminal t]
    ["Look at gnuplot buffer"              input-jump-to-gnuplot-buffer
     :active (and gnuplot-buffer (bufferp (get-buffer gnuplot-buffer)))]
    ["Kill gnuplot buffer and process"     fuse-kill-gnuplot
     :active (and gnuplot-buffer (bufferp (get-buffer gnuplot-buffer)))] )

   ("Miscellaneous"
    ["Customize FUSE"                      input-customize
     :active (and (featurep 'custom) (fboundp 'input-customize))]
    ["Switch programs"                     input-determine-program t]
    ;;["Refresh buffer"                     input-set-configuration t]
    ["Reset autoconfiguration variables"   input-set-configuration t]
    ["Refresh highlighting"                input-repaint-command t]
    ["Update time stamp"                   time-stamp
     :active input-time-stamp-flag]
    ["Save run-time log file"              input-save-run-log t]
    ["Look at previous run-time log"       input-visit-previous-run-log
     :active (file-exists-p input-run-log)]
    ["Submit FUSE bug report"              input-submit-feedback t]
    ["Submit program bug report"           input-submit-program-bug-report t]
    ["Display startup message sequence"    fuse-display-startup-message t]
    ["Show FUSE version number"            input-show-version t])

   ["Documentation"                        input-document t]

   ))

(use-local-map input-mode-map)

(defun input-t ()
  "Dummy function to clear echo area and return t."
  (interactive) (message nil) t)

;;;; end of key bindings for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; syntax table for input mode

(defvar input-mode-syntax-table nil
  "Syntax table in use in `input-mode' buffers.")

;; space, tab, comma, equals are whitespaces
;; * % ! and # are comment starters,
;; semicolon (;), tilde (~), dot (.), underscore (_), foreslash (/),
;;           dash (-) are word characters
;; ( and ) are matched parenthesis characters
;; The following characters are undecided: @ & | : ? ` ' dquote backslash
(if input-mode-syntax-table
    ()
  (setq input-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\  " "  input-mode-syntax-table)
  (modify-syntax-entry ?\t " "  input-mode-syntax-table)
  (modify-syntax-entry ?,  " "  input-mode-syntax-table)
  (modify-syntax-entry ?=  " "  input-mode-syntax-table)
  (modify-syntax-entry ?*  "<"  input-mode-syntax-table)
  (modify-syntax-entry ?%  "<"  input-mode-syntax-table)
  (modify-syntax-entry ?!  "<"  input-mode-syntax-table)
  (modify-syntax-entry ?#  "<"  input-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  input-mode-syntax-table)
  ;;(modify-syntax-entry ?;  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?+  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?-  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?/  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?.  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?~  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?_  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?[  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?]  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?{  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?}  "w"  input-mode-syntax-table)
  (modify-syntax-entry ?<  "(>" input-mode-syntax-table)
  (modify-syntax-entry ?>  ")<" input-mode-syntax-table)
  (modify-syntax-entry ?(  "()" input-mode-syntax-table)
  (modify-syntax-entry ?)  ")(" input-mode-syntax-table)
  )

;; end of syntax section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code for keywords in input mode

;; $Id: keywords.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

;; cleaned up several predicate functions, made input-special-comment-p

;; functions defined in this section
;;    input-complete-keyword ()
;;    input-arg-for-keyword (keyword)
;;    input-arg-for-this-keyword ()
;;    input-special-keyword (keyword how &optional verbose)
;;    input-check-this-keyword-arg ()
;;    input-check-keyword-arg (&optional verbose)
;;    input-get-from-keyword-alist (keyword pos)
;;    input-display-keywords ()
;;    input-title-p ()
;;    input-comment-p ()
;;    input-list-p ()
;;    input-setguess-p ()
;;    input-path-paragraph-p ()
;;    input-blank-p ()
;;    input-repaint-command (&optional begin end)

(defun input-complete-keyword ()
  "Complete the keyword just before point.
Completion is based on a list of valid keywords for the currently
defined *feff* or *uwxafs* program.  If found, write a message to
the echo area about what kind of argument it takes.
Bound to \\[input-input-complete-keyword]"
  (interactive)
  (let* ((keyword (input-this-word)) (keyword-save keyword) mark-1)
    (setq keyword
	  (all-completions (downcase keyword) input-current-keywords-alist))
    (cond ((not keyword)                ; NOT A VALID KEYWORD
	   (message "%S is not the beginning of a keyword in %S"
		    keyword-save input-program-name))
	  ((= (length keyword) 1)       ; WRITE FULL COMPLETION
	   (setq keyword (nth 0 keyword))
	   (delete-backward-char (length keyword-save))
	   (cond ((or input-upcase-keywords-flag
		      (string= "feff" input-program-name))
		  (insert (upcase keyword)))
		 (t
		  (insert keyword)))
	   (cond (input-use-hilit19      ; highlight
		  (setq mark-1 (point))
		  (forward-word -1)
		  (funcall input-program-hilit)
		  (hilit-rehighlight-region (point) mark-1)
		  (forward-word 1)))
	   (insert " ")
	   (input-arg-for-keyword keyword))
	  (t                             ; WRITE PARTIAL COMPLETION
	   (setq keyword-save (downcase keyword-save))
	   (delete-backward-char (length keyword-save))
	   (if (or input-upcase-keywords-flag
		   (string= "feff" input-program-name))
	       (insert (upcase (try-completion keyword-save
					       input-current-keywords-alist)))
	     (insert (try-completion keyword-save
				     input-current-keywords-alist)))
	   (message "Possible completions : %S" keyword)
	   ))
    ))

(defun input-arg-for-keyword (keyword)
  "Determine kind of argument for KEYWORD.
Write a message to the echo area telling what argument is appropriate
for KEYWORD.  This information is the first item in the list associated
with keyword in the appropriate keyword alist."
  (interactive "sWhich keyword? ")
  (let ((argument (input-get-from-keyword-alist (downcase keyword) 2)))
    (if (or input-upcase-keywords-flag
	    (string= "feff" input-program-name))
	(setq keyword (upcase keyword)))
    (if argument
	(message "%S takes %s"  keyword argument)
      (message "%S is not a keyword"  keyword )) ))


(defun input-arg-for-this-keyword ()
  "Determine appropriate argument for keyword under point.
Write a message to the echo area telling what kind of argument the
keyword under point takes.  Bound to \\[input-arg-for-this-keyword]"
  (interactive)
  (cond ((input-comment-p)          (message "This is a comment line."))
	((input-title-p)            (message "This is a title line."))
	((input-stanza-separator-p) (message "This is a stanza separator."))
	(t (save-excursion
	     (input-arg-for-keyword (downcase (input-this-word))))) ))

(defun input-special-keyword (keyword how &optional verbose)
  (if verbose (message "Interpret %S as a %S keyword" keyword how))
  t)


(defun input-check-this-keyword-arg ()
  "Check if argument of keyword under point is valid for that keyword.
Argument syntax is given in the appropriate keyword-alist.  Point is
left in place.  Bound to \\[input-check-this-keyword-arg]"
  (interactive)
  (input-check-keyword-arg t))



;; Make sure not to go to next line.

;; for checking the buffer, store a list of points where problems were
;; found, then can jump to those points ala compile mode

(defun input-check-keyword-arg (&optional verbose)
  "Check if argument of keyword under point is valid for that keyword.
Argument syntax is given in the appropriate keyword-alist.  Point is
left in place.  Returns t if argument is valid and nil if it is not.
If VERBOSE is non-nil, a message about the validity of the argument is
written to the echo area."
  (let ((counter 0) (valid t) keyword keyword-save
	arg-type arg-number value this-arg-type)
    (save-excursion
      (cond
       ((input-blank-p)
	(setq valid t))
       ((input-comment-p)
	(if verbose (message "comment line -- ok"))
	(setq valid t))
       ((input-title-p)
	(if verbose (message "title line -- ok"))
	(setq valid t))
       (t                               ; get the keyword under point
	(setq keyword (downcase (input-this-word)))
	(if (or input-upcase-keywords-flag
		(string= "feff" input-program-name))
	    (setq keyword-save (upcase keyword))
	  (setq keyword-save keyword))
	(cond
	 ((and input-program-stanza-flag
	       (string-match "-----+" keyword))
	  (if verbose (message "stanza separator -- ok"))
	  (setq valid t))
	 ((not (try-completion keyword input-current-keywords-alist))
	  (if verbose (message "%S is not a keyword" keyword))
	  (setq valid nil))
	 (t
					; get a description of this keyword
	  ;;(setq arg-descr  (input-get-from-keyword-alist keyword 2))
	  (setq arg-type   (input-get-from-keyword-alist keyword 1)
		arg-number (input-get-from-keyword-alist keyword 0))
	  (cond ((listp arg-type)
		 (setq arg-number (length arg-type)))
		((stringp arg-type)
		 (if (string= arg-type "none") (setq arg-number 1))
		 (setq arg-type (make-list arg-number arg-type))))

	  (cond ((string= (elt arg-type 0) "none")
		 (if verbose (message "%S takes no argument" keyword-save))
		 (setq valid t))
		(t
		 (while (and (< counter arg-number) valid)
		   (forward-word 1)
		   (setq this-arg-type (elt arg-type counter)
			 counter (1+ counter)
			 value (input-this-word))
		   (and (string= this-arg-type "title")
			(setq this-arg-type "symbol"))
		   (setq valid
			 (and valid (input-check-this-arg value this-arg-type))) )
		 (if verbose
		     (if valid (message "Argument for %S is ok" keyword-save)
		       (message
			 "Bad argument for %S!  Use %s for keyword description."
			 keyword-save (substitute-command-keys
				       "\\[input-arg-for-this-keyword]")))) ))
	  )))))
    valid
    ))


(defun input-check-this-arg (value type)
  "Return t if VALUE is of type TYPE.
See document for `input-current-keywords-alist' for a list of keyword types."
  ;;(message "%S %S" value type)
  (let ((case-fold-search t))
    (cond ((string= type "number")
	   (string-match input-num-expr value))
	  ((string= type "integer")
	   (string-match input-int-expr value))
	  ((string= type "logical")
	   (string-match input-t-f-expr value))
	  ((string= type "element")
	   (member (capitalize value) input-atoms))
	  ((string= type "string")        ;; symbol characters?
	   (string-match "\\<[a-z_0-9]+\\>" value))
	  ((string= type "writable")
	   (file-writable-p (expand-file-name value)) )
	  ((string= type "readable")
	   (file-readable-p (expand-file-name value)) )
	  ((string= type "skey")
	   (and (string-match "[a-z0-9]" value)
		(= 5 (length value))))
	  ((string= type "datatype")
	   (string-match input-format-regexp value))
	  ((string= type "window")
 	   (string-match input-window-regexp value))
	  ((string= type "edge")
 	   (member (downcase value) input-edges))
	  ((or (string= type "ignore")
	       (string= type "setguess")
	       (string= type "pathparam")
	       (string= type "spcgrp")
	       (string= type "symbol")
	       (string= type "tag"))
	   t)
	  (t nil)
	  )))


(defun input-get-from-keyword-alist (keyword pos)
  "Get information about a keyword.
The list associated with KEYWORD is obtained from the keyword-alist
for this program and the POS-th list item is returned.  POS=0 returns
the number of arguments for KEYWORD, POS=1 returns the argument type,
and POS=3 returns a one-line description."
  (nth pos (cdr (assoc keyword input-current-keywords-alist))))


(defun input-display-keywords ()
  "Open a buffer displaying all keywords for the current program.
For *feffit* keywords, identify scope (global or local) of each keyword.
Bound to \\[input-display-keywords]"
  (interactive)
  (let* ((counter 0) (program input-program-name) scope keyword arg-descr
	 (keyword-alist (copy-alist input-current-keywords-alist))
	 (keyword-buffer-name (concat "\*" program " keywords\*")))
    (if (get-buffer keyword-buffer-name)
	(switch-to-buffer-other-window keyword-buffer-name)
      (switch-to-buffer-other-window keyword-buffer-name)
      ;;(erase-buffer)
      (insert (format "\tKeywords for %s\n\n" program))
      (if (string= program "feffit")
	  (insert
	   "(l) = keyword local to data set     (g) = global keyword\n\n    "))
      (if (string= program "feff")
	  (insert "(8) = keyword new to feff8"
		  "     <8> = keyword different in feff8\n\n    "))
      (insert "Keyword\t\tdescription of argument\n"
	      (concat (make-string 75 ?\-) "\n"))
      (while (< counter (length keyword-alist))
	(setq keyword    (car (nth counter keyword-alist)))
	(setq arg-descr  (nth 2 (cdr (nth counter keyword-alist))))
	(cond ((string= program "feffit")
	       (if (member keyword '("guess" "set" "end"
				     "formin" "format" "formout"
				     "allout" "all" "mftfit"
				     "mftwrt" "degen" "nodegen"
				     "rlast" "norun" "nofit" "quit"
				     "mdocxx" "toler" "cormin"
				     "iprint" ))
		                     ;; are these global or local??
	     	                     ;;"tranquada" "kfull" "fullk"
				     ;;"noout" "prmout" "pcout" "envout"
				     ;;"kspfit" "rspfit" "qspfit"
				     ;;"bkg" "bkgout" ))
		   (setq scope "(g)") (setq scope "(l)"))
	       (insert (format "%3s %-14s %s\n" scope keyword arg-descr)))
	      ((string= program "feff")
	       (cond ((member keyword '("ldos" "interstitial" "scmt" "fms"))
		      (setq scope "(8)"))
		     ((member keyword '("rmax" "control" "print" "potentials"))
		      (setq scope "<8>"))
		     (t
		      (setq scope "   ")))
	       (insert (format "%3s %-14s %s\n" scope keyword arg-descr)))
	      (t
	       (insert (format "%-14s %s\n" keyword arg-descr))))
	(setq counter (1+ counter)))
      (help-mode)
      (setq truncate-lines t
	    buffer-read-only t) )
    (goto-char (point-min)) ))


(defun input-title-p ()
  "Return t if point is currently in a title, nil otherwise.  This is
done by searching backwards on the current line for the words title or
(possibly) comment.  In some programs comment means something else."
  (let ((case-fold-search t) bol)
    (save-excursion
      (setq bol (save-excursion (beginning-of-line) (point)))
      (re-search-backward input-title-line-regexp bol t) )))

;; This will not recognize a comment line if point is among white
;; space preceding the comment character.  Need to
;; (back-to-indentation) before using this.

(defun input-comment-p ()
  "Return t is point is currently in a comment, nil otherwise.
This  is done by searching backwards on the current line for a comment
character.  Note that a comment character may be part of a title line,
so this function returns nil if point is currently in a title line."
  (save-excursion
    (let (limit (expr (if (string= input-program-name "feff")
			  (concat input-comment-expr "\\|\\*")
			input-comment-expr)))
      (if (input-title-p)	()
	(setq limit (save-excursion (beginning-of-line) (point)))
	(or (re-search-backward expr limit t)
	    (looking-at expr)) ))) )


(defun input-stanza-separator-p ()
  "Return t is point is currently in a stanza separator.
The stanza separator is typically a line of dashes.  This always
returns nil for a program that does not use stanzas."
  (and input-program-stanza-flag
       (save-excursion
	 (back-to-indentation)
	 (looking-at "[*!%#]*[ \t]*---+"))))

;; this is a mess
(defun input-list-p ()
  "Return t if point is currently in a list, nil otherwise.
This is done by searching backwards from the current line for a
list keyword.  It is assumed that the list keyword is the first word
on the line.  Care is taken to avoid keywords in title lines or
comment lines.  List keywords are:  for atoms, \"atoms\" or \"basis\";
for feff, \"atoms\" or \"potential\"; for normal, \"files\" or \"data\";
for fluo, \"files\", \"data\", or \"atoms\"."
  ;;(interactive)
  (save-excursion
    (let* (word list (case-fold-search t)
		(expr-alist '(("atoms"  . "\\<atom\\|\\<basis")
			      ("feff"   . "\\<atom\\|\\<poten")
			      ("fluo"   . "\\(\\<file\\|\\<data\\|\\<atoms\\)")
			      ("normal" . "\\<file\\|\\<data")))
		(expr (cdr (assoc input-program-name expr-alist))) )
      (cond ((try-completion input-program-name expr-alist)
	     (back-to-indentation)
	     (if (or (input-comment-p) (looking-at "^[ \t]*$"))
		 (setq word "not a keyword")
	       (setq word (downcase (input-this-word))))
	     (cond ((try-completion word input-current-keywords-alist)
		    (setq list nil))
		   (t
		    (forward-line -1)
		    (while (not (bobp))
		      ;;(back-to-indentation)
		      (if (or (input-comment-p) (input-blank-p))
			  (forward-line -1)
			(forward-word 1)
			(setq word (downcase (input-this-word)))
			(cond ((try-completion word input-current-keywords-alist)
			       (cond ((string-match expr word)
				      (setq list word)
				      (goto-char (point-min)))
				     (t
				      (setq list nil)
				      (goto-char (point-min)))))
			      (t
			       (forward-line -1))) )

		      )) ))
	    (t
	     (setq list nil)))
      ;;(message "list? %s" list)
      list
      )))


(defun input-setguess-p ()
  "Return t if the current line defines a set, guess, or local math expression."
  (save-excursion
    (back-to-indentation)
    (looking-at "\\(set\\|guess\\|local\\)\\>")))

;; this is not working yet
(defun input-path-paragraph-p ()
  "Return t if point is in a path paragraph, nil if it is not."
  (save-excursion
    (back-to-indentation)
    (looking-at input-path-paragraph-regexp)))

(defun input-path-parameter-p ()
  "Return t if point is in a line with a path parameter, nil if it is not."
  (save-excursion
    (back-to-indentation)
    (looking-at input-path-paragraph-regexp)))

(defun input-blank-p ()
  "Return t if point is in a line of only whitespace, nil if it is not."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))


(defun input-special-comment-p ()
  "Return t if point is in a special comment, nil if it is not.
A special comment is a region in a comment line or a title line
which is bounded by angle brackets."
  (save-excursion
    (let ((begin (save-excursion (beginning-of-line) (point)))
	  (end   (save-excursion (end-of-line) (point))) )
      (and (or (input-comment-p) (input-title-p))
	   (search-forward  ">" end   t)
	   (search-backward "<" begin t)) )))

(defun fuse-next-data-set-p ()
  "Return t if point is in a \"next data set\" line."
  (save-excursion
    (back-to-indentation)
    (forward-word 1)
    (string= (downcase (current-word)) "next")))
;;    (looking-at "next[ \t]+data[ \t]+set")))

;;Rehighlight entire buffer unless BEGIN and END are given to mark a region.
(defun input-repaint-command (&optional begin end)
  "Repaint buffer colorization.
Repaint region if BEGIN and END are set.  Use hilit19 or
font-lock and the patterns appropriate to the current program."
  (interactive)
  (cond (input-use-font-lock
	 (if (and begin end)
	     (font-lock-fontify-region begin end)
	   (font-lock-fontify-buffer)))
	(input-use-hilit19              ; reset patterns before hilit-ing
	 (funcall input-program-hilit)
	 (if (and begin end)
	     (hilit-rehighlight-region begin end)
	   (hilit-repaint-command t)))) )


;;;; end of keywords code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general template code for input mode

;; $Id: templates.el,v 1.4 1998/04/05 14:55:56 bruce Exp $

;; functions defined in this section
;;    input-set-data-path ()
;;    input-set-feff-path ()
;;    input-set-out-path ()
;;    input-set-all-paths ()
;;    input-set-k-weight ()
;;    input-set-master ()
;;    input-set-version ()
;;    input-insert-filename ()
;;    fuse-insert-z-number ()
;;    input-toggle-gnuplot-terminal ()

;; paths to output, feff, and data must end in a slash!

(defun input-set-data-path (&optional arg)
  "Set directory path for an input data file.
Get input from minibuffer if ARG is nil.
Bound to \\[input-set-data-path]"
  (interactive)
  (setq input-data-path
	(or arg
	    (read-file-name "Path to data files > " ""
			    (file-name-directory buffer-file-name) )))
  (if (string= (substring input-data-path -1 nil) "/") ()
    (setq input-data-path (concat input-data-path "/")))
  (if (string= (substring input-data-path 0 1) "~")
      (setq input-data-path (expand-file-name input-data-path)) ))

(defun input-set-feff-path (&optional arg)
  "Set directory path for input feff files.
Get input from  minibuffer if ARG is nil.
Bound to \\[input-set-feff-path]"
  (interactive)
  (setq input-feff-path
	(or arg
	    (read-file-name "Path to feff files > " ""
			    (file-name-directory buffer-file-name) )))
  (if (string= (substring input-feff-path -1 nil) "/") ()
    (setq input-feff-path (concat input-feff-path "/")))
  (if (string= (substring input-feff-path 0 1) "~")
      (setq input-feff-path (expand-file-name input-feff-path)) ))

(defun input-set-out-path (&optional arg)
  "Set directory path for output files.
Get input from  minibuffer if ARG is nil.
Bound to \\[input-set-out-path]"
  (interactive)
  (setq input-out-path
	(or arg
	    (read-file-name "Path to output files > " ""
			    (file-name-directory buffer-file-name) )))
  (if (string= (substring input-out-path -1 nil) "/") ()
    (setq input-out-path (concat input-out-path "/")))
  (if (string= (substring input-out-path 0 1) "~")
      (setq input-out-path (expand-file-name input-out-path)) ))

(defun input-set-all-paths ()
  "Set paths to data files, feff files, and output files.
Each will be  prompted in minibuffer.
Bound to \\[input-set-all-paths]"
  (interactive)
  (let (answer)
    (setq answer (read-file-name "Path to data files > " ""
				 (file-name-directory buffer-file-name) ))
    (input-set-data-path answer)
    (setq answer (read-file-name "Path to feff files > " ""
				 (file-name-directory buffer-file-name) ))
    (input-set-feff-path answer)
    (setq answer (read-file-name "Path to output files > " ""
				 (file-name-directory buffer-file-name) ))
    (input-set-out-path  answer) ))

(defun input-set-k-weight (arg)
  "Set default k-weighting for plotting chi(k).
Get input from minibuffer if ARG is nil.  The k-weight will be set to
zero for negative input.  Bound to \\[input-set-k-weight]"
  (interactive "nEnter k-weight > ")
  (if (< arg 0) (setq arg 0))
  (setq input-k-weight (number-to-string arg)))

(defun input-set-eshift (arg)
  "Set default e0 shift for the eshift script
Get input from minibuffer if ARG is nil.  The k-weight will be set to
zero for negative input.  Bound to \\[input-set-eshift]"
  (interactive "nEnter e0 shift > ")
  (setq input-eshift (number-to-string arg)))

(defun input-set-master (&optional arg)
  "Set directory path for the output files file.
Get input from  minibuffer if ARG is nil.  Bound to \\[input-set-out-path]"
  (interactive)
  (if input-program-master-flag
      (setq input-master
	    (or arg (read-file-name "Name of master file > " "")))
    (message "%s does not use include files." input-program-name)))

(defun input-set-version (&optional arg)
  "Set program version for this input file.
Get input from  minibuffer if ARG is nil.  Bound to \\[input-set-version]"
  (interactive)
  (setq input-program-version
	(or arg
	    (read-string (concat input-program-name " version number > ")))))
;;   (and (string= input-program-name "feff")
;;        (string-match "\\<8" input-program-version)
;;        (Feff-instrument-feff-8)) )

(defun input-insert-filename ()
  "Insert a filename at point, prompting for name in minibuffer.
Uses completion.  Bound to \\[input-insert-filename]"
  (interactive)
  (insert (expand-file-name (read-file-name "Filename > " "")) ))

(defun fuse-insert-z-number (&optional atom)
  "Insert the Z number appropriate to ATOM at point.
If ATOM is not supplied, it will obtained interactively.  ATOM must be
a valid element symbol."
  (interactive)
  (while (not (member atom input-atoms))
    (setq atom (capitalize (read-string "Element symbol > "))))
  (insert (number-to-string (1+ (position atom input-atoms :test 'string=)))))


(defun input-toggle-gnuplot-terminal ()
  "Toggle *gnuplot* terminal type between x11 and postscript."
  (interactive)
  (cond ((or (string= input-gnuplot-terminal "x11")
	     (not input-gnuplot-terminal))
	 (setq input-gnuplot-postscript-file
	       (read-file-name "PostScript filename: "
			       nil input-gnuplot-default-ps-file
			       nil input-gnuplot-default-ps-file))
	 (setq input-gnuplot-terminal "postscript"))
	(t
	 (setq input-gnuplot-terminal input-gnuplot-default-terminal)))
  (message "Gnuplot terminal type is now %S" input-gnuplot-terminal))

;;;; end of general templates code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code for common shortcuts in input mode

;; $Id: swap.el,v 1.4 1998/04/05 20:35:09 bruce Exp $

;; functions defined in this section
;;    input-comment-region (beg-region end-region arg)
;;    input-guess-to-set ()
;;    input-swap-comment ()
;;    input-true-false ()

(defun input-guess-to-set ()
  "Switch guess and set on the present line.
Bound to \\[input-guess-to-set]"
  (interactive)
  (let (mark-1 (case-fold-search t) (case-replace t))
    (save-excursion
      (back-to-indentation)
      (cond ((looking-at "\\(guess\\)")
	     (replace-match "set"))
	    ((looking-at "\\(set\\)")
	     (replace-match "guess"))
	    (t
	     (message "This is not a set or guess line.")))
      (cond (input-use-hilit19
	     (beginning-of-line)
	     (setq mark-1 (save-excursion (end-of-line) mark-1 (point)))
	     (funcall input-program-hilit)
	     (hilit-rehighlight-region (point) mark-1)))
      )))

(defun input-true-false ()
  "Switch logical values under point with completion.
Bound to \\[input-true-false]"
  (interactive)
  (let (eol word logical quit (case-fold-search t) (case-replace t)
	    (completion-ignore-case t) arg-type
	    (logical-alist '(("true"  . "false") ("false" . "true" )
			     ("yes"   . "no"   ) ("no"    . "yes"  ) )) )
    (if (input-blank-p) (message "This is an empty line.")
      (save-excursion
	(setq eol (save-excursion (end-of-line) (point))
	      word (input-this-word)
	      arg-type (input-get-from-keyword-alist word 1))
	;; recognize a logical valued keyword
	(cond ((and (try-completion word input-current-keywords-alist)
		    (stringp arg-type)
		    (string= "logical" arg-type))
	       (re-search-forward input-word-sep eol t))
	      ((and (try-completion word input-current-keywords-alist)
		    (stringp arg-type)
		    (not (string= "logical" arg-type)))
	       (setq quit (format "%S does not take a logical value" word)))
	      (t
	       (forward-word -1)))
	(cond (quit
	       (message quit))
	      ((looking-at "[tfyn]\\w*\\>")
	       (setq logical (substring (match-string 0) 0 1))
	       (replace-match
		(cdr (assoc (try-completion logical logical-alist)
			    logical-alist))))
	      (t
	       (message "Logical values are 'true', 'false', 'yes', and 'no'")))
	))))


;; use Noah Friedman's comment-mode
(require 'comment)
(defvar input-saved-comment-alist ()
  "Need to save a copy of `comment-mode-alist' without the input mode
additions so I can handle feff's annoying comment character.")

(defun input-swap-comment (&optional arg)
  "Comment/uncomment the present line.
Will not uncomment an auto-configuration or time stamp line.  ARG is
passed to `comment-out-region' as the number of comment characters to
insert into the current line.  ARG can be specified interactively with
\\[universal-argument] and a number.  ARG is ignored when
uncommenting.  Bound to \\[input-swap-comment]"
  (interactive "*p")
  (let (eol (alist (copy-alist input-saved-comment-alist))
	    (vblcom (concat "\\*?\\(" (regexp-quote comment-start)
			    "\\|" (regexp-quote input-time-stamp-begin)
			    "\\)")))
    (save-excursion
      (back-to-indentation)
      (if (looking-at vblcom) ()
	(cond ((looking-at "\\([%#*!]+\\)")
	       (replace-match "")
	       (delete-horizontal-space)
	       (insert (input-indent)))
	      (t
	       (delete-horizontal-space)
	       (setq eol (save-excursion (end-of-line) (point)))
	       (if (string= input-program-name "feff")
		     (setq comment-mode-alist (copy-alist Feff-comment-alist)))
	       (comment-out-region (point) eol arg)
	       (if (string= input-program-name "feff")
		     (setq comment-mode-alist (append alist input-comment-list)))
	       (beginning-of-line)
	       (insert (input-indent))))
	(cond (input-use-hilit19
	       (beginning-of-line)
	       (funcall input-program-hilit)
	       (hilit-rehighlight-region (point) eol))))      )))

(setq input-saved-comment-alist (copy-alist comment-mode-alist))
(add-hook 'input-mode-hook
	  '(lambda ()
	     (setq comment-mode-alist
		   (append comment-mode-alist input-comment-list)) ))

;;;; end of common shortcuts code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code for indentation in input mode

;; functions defined in this section
;;    input-determine-area ()
;;    input-indent ()
;;    input-make-indent-string (arg disallow-zero)
;;    input-newline ()
;;    input-clean-line
;;    input-clean-region
;;    input-clean-file

(defun input-indent (&optional location)
  "Return indentation string appropriate to textual area under point.
LOCATION is a string (one of comment, blank, title, setguess, feff,
pathparagraph, stanza, or list.  If LOCATION is nil, then
`input-determine-area' is called."
  (interactive)
  (let ((current-indent 0) (case-fold-search t))
    (if location () (setq location (input-determine-area)))
    ;;(message "%S" location)
    (cond ((string= "comment" location)
	   (setq current-indent input-comment-indent))
	  ((string= "title" location)
	   (setq current-indent input-stanza-indent))
	  ((string= "potentials" location)
	   (setq current-indent input-potentials-indent))
	  ((string= "list" location)
	   (setq current-indent input-list-indent))
	  ((string= "feff" location)
	   (setq current-indent input-stanza-indent))
	  ((string= "pathparagraph" location)
	   (setq current-indent input-path-paragraph-indent))
	  ((string= "setguess" location)
	   (setq current-indent input-set-guess-indent))
	  ((string= "blank" location)
	   (setq current-indent 0))
	  ((string= "stanza" location)
	   (setq current-indent input-stanza-indent)))
    (and (>= 0 current-indent)
	 (string= input-program-name "feff")
	 (setq current-indent (+ current-indent input-feff-indent)))
    (input-make-indent-string current-indent nil)))

(defun input-determine-area ()
  "Determine what textual area point currently inhabits.
Return one of:
  stanza  pathparagraph  feff  list  comment  title  setguess"
  (let ((list-p (input-list-p)))
    ;;(message "%S" list-p)
    (cond ((input-comment-p)                   "comment")
	  ((input-title-p)                     "title")
	  ((and list-p
		(string-match "pot" list-p))   "potentials")
	  (list-p                              "list")
	  ((string= "feff" input-program-name) "feff")
	  ((input-path-paragraph-p)            "pathparagraph")
	  ((input-setguess-p)                  "setguess")
	  ((input-blank-p)                     "blank")
	  (t                                   "stanza"))       ))


(defun input-make-indent-string (arg disallow-zero)
  "Convert an integer to an appropriate white space string.
ARG > 0 means the string is that many spaces.  ARG < 0 means that many
tabs.  If DISALLOW-ZERO is non-nil then ARG=0 gets replaced by 1 tab."
  (let ((indent-string ""))
    (if (> arg 0)
	(setq indent-string (make-string arg ?\ ))
      (setq indent-string (make-string (abs arg) ?\t )))
    (if (and disallow-zero (= arg 0))
	(setq indent-string "\t"))
    indent-string ))

(defun input-newline ()
  "Insert a newline and indent.
The indentation appropriate to the current textual area -- see documents for
`input-indent' and `input-determine-area'."
  (interactive)
  (newline)
  (delete-horizontal-space)
  (insert (input-indent (input-determine-area))) )

(defun input-clean-line ()
  "Clean current line by indenting and separating columns if appropriate.
Bound to \\[input-clean-line]"
  (interactive)
  (let (location indent)
    (save-excursion
      (setq indent (input-indent))
      (beginning-of-line)
      (if (looking-at (concat indent "\\<")) ()
	(back-to-indentation)
	(delete-horizontal-space)
	(insert indent))
					; separations?
      (back-to-indentation)
      (setq location (input-determine-area))
      (cond ((string= location "blank")
	     (delete-horizontal-space))
	    ((string= input-program-name "atoms")
	     (if (input-list-p) (Atoms-clean-list-entry)))
	    ((string= input-program-name "feffit")
	     (cond ((input-setguess-p)       (Feffit-clean-setguess))
		   ((input-path-paragraph-p) (Feffit-clean-pathparagraph)) ))
	    ((string= input-program-name "feff")
	     (cond ((string-match "poten" (concat (input-list-p) ""))
		    (Feff-clean-potentials-entry))
		   ((string-match "atom"  (concat (input-list-p) ""))
		    (Feff-clean-atoms-entry)) ))
	    )
      )))


(defun input-clean-region (begin end)
  "Clean region.
Region defined by BEGIN and END, usually point and mark."
  (interactive "*r")
  (let ((end-mark (make-marker)))
    (save-excursion
      (and (> begin end) (exchange-point-and-mark))
      (message "Cleaning ...")
      (set-marker end-mark end)
      (goto-char begin)
      (while (< (point) end-mark)
	(input-clean-line)
	(forward-line 1))
      (message "Cleaning ... done.") )))

(defun input-clean-file ()
  "Clean entire file."
  (interactive)
  (input-clean-region (point-min) (point-max)) )

;;;; end of indentation code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; process running code in input-mode

;; $Id: run.el,v 1.3 1998/03/15 02:45:23 bruce Exp $

;; functions defined in this section
;;    input-run-this-program-this-file ()
;;    input-run-any-program-any-file ()
;;    input-run-program (program file &optional version)
;;    input-after-run (process event)
;;    input-next-batch-run (&optional foo)
;;    input-kill-program ()
;;    input-trim-run-buffer ()

;; a bunch of variables for communicating between input-run-program
;; and input-after-run
(defvar input-basename nil)
(defvar input-process-directory nil)
(defvar input-program-running nil)
(defvar input-temp-string nil)
(defvar input-file-buffer nil)
(defvar input-original-frame nil)
(defvar input-current-output-files '())

;;;###autoload
(defun input-run-this-program-this-file ()
  "Run the current program on the current file.
This function appears in each of the minor mode pull-down menus in FUSE.
Bound to \\[input-run-this-program-this-file]"
  (interactive)
  (let ((file (or input-master buffer-file-name)))
    (input-run-program (list input-program-name file input-program-version))))

(defun input-run-any-program-any-file ()
  "Run a program on an input file.
Prompt in minibuffer for name of program to run and name of input file on
which to run.  Bound to \\[input-run-any-program-any-file]"
  (interactive)
  (let ((file (read-file-name "Using which input file? "
			      nil buffer-file-name t))
	(program (completing-read "Run which program? (<tab> for list) "
				  input-programs-alist)))
    (input-run-program (list program file ""))))


;; lotta work to do for feff & xanes
;;(defun input-run-program (program file &optional version)
(defun input-run-program (list)
  "This is the workhorse function for program execution.
This is called by `input-run-this-program-this-file' and by
`input-run-any-program-any-file'.

LIST is a three element list containing the program to run, the
filename of the input file, and the version string of the program.
The screen output of the program is written to the other window.  The
input file is renamed (if necessary) and care is taken not to
overwrite any pre-existing files.  `input-after-run' is the sentinel
which renames all files back to their proper names after the program
is done running.

If `input-use-frame-flag' is non-nil then the screen output is written
to a buffer in another frame."
  (let ((program (elt list 0)) (file (elt list 1)) (version (elt list 2))
	filename outfile (counter  0) (frame (selected-frame)))
					; for now allow 1 process at a time
    (cond ((string= (process-status input-process) "run")
	   (message
  "You already have a process running.  FUSE only allows one at a time."))
	  ;; be really pedantic...
	  ((and input-batch-flag (not input-started-by-batch-flag))
	   (message
	    "You are currently running a batch job.  Wait for it to finish."))
	  (t
					; save buffers then determine current
					; directory, file base name, program
					; name.  Also set some global vbls.
	   (run-hooks 'input-before-run-hook)
	   (when input-use-frames
		 (raise-frame (selected-frame)))
	   (if input-batch-flag (save-buffer)
	     (save-some-buffers))
	   (setq input-temp-string (make-temp-name "FUSE")
		 input-process-directory
		 (file-name-directory (expand-file-name file))
		 input-basename
		 (file-name-sans-extension (file-name-nondirectory file))
		 fuse-running-this-file t)
	   (cond ((string= program "feff")
		  (if (> (length input-basename) 4)
		      (setq input-basename (substring input-basename 0 -5)))))
	   (setq input-program-running program
		 input-file-buffer (current-buffer)
		 input-current-output-files input-output-files)
	   (setq input-original-frame (selected-frame))

					; if the output files of this program
					; exist and the input file is NOT
					; program.inp, rename the extant files
					; to avoid overwriting
	   (if (string= input-basename program)
	       ()
	     (while (< counter (length input-output-files))
	       (setq outfile (nth counter input-output-files))
	       (if (string= "." (substring outfile 0 1))
		   (setq filename (concat program outfile))
		 (setq filename outfile))
	       (if (file-exists-p filename)
		   (rename-file filename
				(concat input-temp-string "-" filename)
				"replace"))
	       (setq counter (1+ counter)))

					; rename this input file to program.inp
	     (if (file-exists-p (concat program ".inp"))
		 (rename-file (concat program ".inp")
			      (concat input-temp-string "-" program ".inp")
			      "replace"))
	     (rename-file file (concat program ".inp") "replace")
	     )

	   (if input-process-directory ()
	     (setq input-process-directory
		   (file-name-directory buffer-file-name)))

	   (if input-use-frames   ; display run in another frame?
	       (progn
		 (if (or (not input-run-frame)
			 (not (frame-live-p input-run-frame)))
		     (progn
		       (setq input-run-frame
			     (if (string-match "XEmacs" emacs-version)
				 (make-frame input-run-frame-plist)
			       (make-frame input-run-frame-parameters)))
		       (select-frame input-run-frame)
		       (switch-to-buffer input-program-buffer)
		       (setq truncate-lines 1)
		       ;;(set-window-dedicated-p (selected-window) t)
		       (raise-frame input-run-frame)
		       ))
		 ;; don't deiconify or raise an extant frame.  user has to choose.
		 (if input-always-raise-flag (raise-frame input-run-frame))
		 (select-frame input-run-frame)
		 (switch-to-buffer input-program-buffer)
		 (setq truncate-lines 1)
		 ;;(set-window-dedicated-p (selected-window) t)
		 )
	     (switch-to-buffer-other-window input-program-buffer))
	   (goto-char (point-max))
	   (insert (concat "\n\n-------- Running " program version
			   "\nin " input-process-directory ))
	   (if (string= input-basename program) ()
	     (insert (concat "\non " input-basename ".inp")))
	   (insert " ...\n\n")
					; need to cd to correct directory
					; before starting program
	   ;;(set-process-filter
	    (start-process input-process
			   input-program-buffer
			   fuse-shell	;; bash -c 'cd <dir> ; <prog>'
			   fuse-shell-switches
			   (concat "cd "
				   input-process-directory
				   " ; '"
				   program
				   version "'"))
	    ;;'input-strip-ctrl-m)


	   ;;(start-process-shell-command input-process
		;;			input-program-buffer
		;;			(concat "cd " input-process-directory
		;;				" ; time")
		;;			(concat program version) )
	   (goto-char (point-max))
					; use a sentinal to rename files back
					; to their proper names
	   (set-process-sentinel (get-process input-process) 'input-after-run)
	   (if input-use-frames         ; display run in another frame?
	       (select-frame frame)
	     (other-window -1))			; return to input-mode window
	   (bury-buffer input-program-buffer)
      ))    ))

;; (defun input-strip-ctrl-m (process output)
;;   "This filters program screen output and writes it to the buffer.
;; It seems that, on Windows, ^M characters get written to the end of
;; every line."
;;   (if (string-match "[\r]+" output)
;;       (replace-match "" t t string))
;;   (save-excursion
;;     (set-buffer input-program-buffer)
;;     (goto-char (point-max))
;;     (insert output)))

;; does not handle feff files correctly after run
(defun input-after-run (process event)
  "Sentinel called after a program is run.
The main job of this function is renaming files back to their proper
names after a program run is finished.  After renaming the files,
`input-trim-run-buffer' then `input-after-run-hook' are run.
PROCESS and EVENT are normal sentinel arguments."
  (let ((counter  0) filename outfile character ;; (buffer (current-buffer)))
	(list input-current-output-files))
    (if (string= input-basename input-program-running) ()
					; rename extant files
      (while list
	(setq outfile (car list))
	(cond ((string= "." (substring outfile 0 1))
	       (setq filename (concat input-program-running outfile))
	       (if (file-exists-p filename)
		   (rename-file (concat input-process-directory filename)
				(concat input-process-directory
					input-basename outfile)
				"replace"))
	       (setq character "")
	       )
	      (t
	       (setq filename outfile)
	       (if (file-exists-p filename)
		   (rename-file (concat input-process-directory filename)
				(concat input-process-directory
					input-basename "-" outfile)
				"replace"))
	       (setq character "-")
	       ))

	(if (file-exists-p (concat input-temp-string "-" filename))
	    (rename-file (concat input-process-directory
				 input-temp-string "-" filename)
			 (concat input-process-directory filename)
			 "replace") )

					; pretty-print renaming of output files
	(if input-use-frames
	    (set-buffer input-program-buffer)
	  (switch-to-buffer-other-window input-program-buffer))
	(goto-char (point-max))
	(insert (format "\n-------- Renamed %s to %s." filename
			(concat input-basename character outfile)))
	(if input-use-frames ()
	  ;;(select-frame input-original-frame)
	  (other-window -1))            ; return to input-mode window
	(setq list (cdr list)) )

					; rename input files
      (cond ((string= input-program-running "feff")
	     (rename-file (concat input-process-directory
				  input-program-running ".inp")
			  (concat input-process-directory
				  input-basename "-feff.inp")
			  "replace"))
	    (t
;; 	     (if (and (file-exists-p (concat input-process-directory
;; 					     input-basename ".inp"))
;; 		      (y-or-n-p
;; 		       (format "You have edited %s since beginning the %s run.  Copy the old one back?"
;; 			       (concat input-basename ".inp")
;; 			       input-program-running)))
	     (rename-file (concat input-process-directory
				  input-program-running ".inp")
			  (concat input-process-directory
				  input-basename ".inp")
			  "replace")))
      (if (file-exists-p
	   (concat input-temp-string "-" input-program-running ".inp"))
	  (rename-file
	   (concat input-process-directory
		   input-temp-string "-" input-program-running ".inp")
	   (concat input-process-directory input-program-running ".inp")
	   "replace"))
      )
    (input-trim-run-buffer)
    (set-buffer input-file-buffer)
    (setq fuse-running-this-file nil)
    (run-hooks 'input-after-run-hook)
    (if input-beep-flag (ding))
    (cond (input-batch-flag
	   (setq input-batch-index (1+ input-batch-index))
	   (cond ((= (length input-batch-files) input-batch-index)
 		  (setq input-batch-index  0)
 		  (setq input-batch-files ())
 		  (setq input-batch-flag nil)
 		  (setq input-started-by-batch-flag nil)
		  (message "Batch final %S complete with status %S"
			   process (substring event 0 -1)) )
		 (t
		  (message "Batch %s of %s: %S complete with status %S"
			   input-batch-index (length input-batch-files)
			   process (substring event 0 -1))
		  (if (string-match "XEmacs" emacs-version)
		      (add-timeout 1 'input-next-batch-run "foo")
		    (run-at-time 1 nil 'input-next-batch-run)))))
	  (t
	   (message "%S finished with exit status %S"
		    input-program-running (substring event 0 -1)) ) )
    ))

;;  this is better as an after run hook
;; 	   (if (and (string= input-program-running "atoms")
;; 		    Atoms-auto-jump-flag)
;; 	       (input-jump-to-log-file))

(defun input-next-batch-run (&optional foo)
  (let ((buffer (current-buffer)))
    (setq foo (length foo))  ;; silly silly!
    (set-buffer
     (find-file-noselect (elt input-batch-files input-batch-index)))
    (input-run-program (list input-program-name buffer-file-name
			     input-program-version))
    (set-buffer buffer)))


(defun input-kill-program ()
  "Kill the currently running *feff* or *uwxafs* process.
If a batch job is currently running, do not start any subsequent
processes.  Bound to \\[input-kill-program]"
  (interactive)
  (let ((frame (selected-frame)))
    (if (not (get-process input-process))
	(error "There is not a feff or uwxafs process currently running.")
      (if input-use-frames		; this seems unnecessary, but during
	  (progn			; a long run, the run frame may get killed
	    (if (or (not input-run-frame)
		    (not (frame-live-p input-run-frame)))
		(progn
		  (setq input-run-frame
			(if (string-match "XEmacs" emacs-version)
			    (make-frame input-run-frame-plist)
			  (make-frame input-run-frame-parameters)))
		  (select-frame input-run-frame)
		  (switch-to-buffer input-program-buffer)
		  ;;(set-window-dedicated-p (selected-window) t)
		  (raise-frame input-run-frame)
		  ))
	    (if input-always-raise-flag (raise-frame input-run-frame))
	    (select-frame input-run-frame)
	    (switch-to-buffer input-program-buffer)
	    ;;(set-window-dedicated-p (selected-window) t)
	    )
	(switch-to-buffer-other-window input-program-buffer))
      (goto-char (point-max))
      (kill-process input-process)
      (insert (concat "\n\t\t" input-program-running " killed!\n\n"))
      (if input-batch-flag
	  (progn
	    (setq input-batch-index  0)
	    (setq input-batch-files ())
	    (setq input-batch-flag nil)
	    (setq input-started-by-batch-flag nil)
	    (message "Batch job terminated")))
      (if input-use-frames		; display run in another frame?
	  (select-frame frame)
	(other-window -1))       	; return to input-mode window
      )))


(defun input-trim-run-buffer ()
  "Pop the top lines from the run-buffer when it exceeded a certain length."
  (let (pop)
    (if (<= input-run-log-max-lines 0)
	()
      (save-excursion
	(set-buffer input-program-buffer)
	(cond ((> (count-lines (point-min) (point-max)) input-run-log-max-lines)
	       (setq pop (- (count-lines (point-min) (point-max))
			    input-run-log-max-lines))
	       (goto-char (point-min))
	       (forward-line pop)
	       (delete-region (point-min) (point))
	       (goto-char (point-max))
	       ))))    ))

;;;; end of process running code in input-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code for motion between textual units in input-mode
;;;; functions involved in manipulating stanzas in autobk, transf

;; $Id: movement.el,v 1.2 1998/03/14 22:46:00 bruce Exp $

;; functions defined in this section
;;    input-beginning-of-stanza ()
;;    input-end-of-stanza ()
;;    input-mark-stanza ()
;;    input-kill-stanza ()
;;    input-next-stanza (arg)
;;    input-previous-stanza (arg)
;;    input-first-stanza-p ()
;;    input-last-stanza-p ()
;;    input-first-paragraph-p ()
;;    input-last-paragraph-p ()
;;    input-forward-paragraph ()
;;    input-backward-paragraph ()
;;    input-mark-paragraph ()
;;    input-kill-paragraph ()

;; does this need to be buffer local so that stanzas can be defined
;; diferently in different programs?
(defconst input-stanza-separate "^[!%#* \t]*------+"
  "Job delimiter in several uwxafs programs.
Recognized by stanza manipulating functions.")

(defun input-beginning-of-stanza ()
  "Move point to beginning of current stanza."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line)
    (if (re-search-backward input-stanza-separate (point-min) 'move)
	(forward-line 1))))

(defun input-end-of-stanza ()
  "Move point to end of current stanza."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line)
    (re-search-forward input-stanza-separate (point-max) "to_end")
    (if (eobp)
	(progn
	  (input-beginning-of-stanza)
	  (and (re-search-forward (regexp-quote comment-start)
				  (point-max) 'move)
	       (beginning-of-line -1))))    ))

(defun input-mark-stanza ()
  "Mark current stanza.
Bound to \\[input-mark-stanza]"
  (interactive)
  (input-beginning-of-stanza)
  (push-mark (point) nil t)
  (input-end-of-stanza)
  (forward-line 1))

(defun input-kill-stanza ()
  "Kill current stanza.
Bound to \\[input-kill-stanza]"
  (interactive)
  (input-mark-stanza)
  (kill-region (point) (mark)))

(defun input-next-stanza (arg)
  "Move point to beginning of next stanza.
If numeric ARG is provided, move forward that many stanzas.
Bound to \\[input-next-stanza]"
  (interactive "p")
  (or arg (setq arg 1))
  (let ((counter 0))
    (while (< counter arg)
      (cond ((input-last-stanza-p)
	     (setq counter arg)
	     (and (interactive-p) (message "This is the last stanza.")))
	    (t
	     (input-end-of-stanza)
	     (forward-line 1)
	     (setq counter (1+ counter)))) )))

(defun input-previous-stanza (arg)
  "Move point to beginning of previous stanza.
If numeric ARG is provided, move back that many stanzas.
Bound to \\[input-previous-stanza]"
  (interactive "p")
  (or arg (setq arg 1))
  (let ((counter 0))
    (while (< counter arg)
      (cond ((input-first-stanza-p)
	     (setq counter arg)
	     (and (interactive-p) (message "This is the first stanza.")))
	    (t
	     (input-beginning-of-stanza)
	     (forward-line -1)
	     (input-beginning-of-stanza)
	     (setq counter (1+ counter)))) )))

(defun input-first-stanza-p ()
  "Return t is point is in first stanza and nil if it is not."
  (save-excursion
    (input-beginning-of-stanza)
    (bobp)))

(defun input-last-stanza-p ()
  "Return t is point is in last stanza and nil if it is not."
  ;;(interactive)
  (save-excursion
    (input-end-of-stanza)
    (or (eobp) (looking-at "[ \t\n]*\\'")
	(looking-at (concat "[ \t\n]*\\(" (regexp-quote comment-start)
			    "\\|" (regexp-quote input-time-stamp-begin)
			    "\\)"))) ))

;;;; functions involved in manipulating path paragraphs in feffit, phit

(defvar Feffit-paragraph-separate "^[!%#\* \t]*$"
  "Regex separating path paragraphs in *feffit*.
A line containing only spaces, tabs, newlines, bangs (!), percent signs,
hashes (#), and/or asterixes is a paragraph separator.  Also used for
function paragraphs in *phit*.")


;; search backwards for path (fun)
;; search forwards for paragraph separate
;; check if original point is between the two.

(defun input-first-paragraph-p ()
  "Return t if point is in the first path paragraph of the data set."
  (save-excursion
    (not (input-backward-paragraph))))

(defun input-last-paragraph-p ()
  "Return t if point is in the last path paragraph of the data set."
  (save-excursion
    (not (input-forward-paragraph 1))))

(defun input-after-last-paragraph-p ()
  "Return t is point is beyond the last paragraph of the data set."
  (save-excursion
    (and (input-last-paragraph-p)
	 (not (input-path-parameter-p)))))

;; these next two fail when next data set or end are in a comment

(defun input-forward-paragraph (&optional arg)
  "Move point forward by ARG path paragraphs.
Leave point at the beginning of the paragraph.  ARG defaults to 1
but can be specified by the prefix argument.  This will not move past
\"end\" or \"next data set\".  Bound to \\[input-forward-paragraph]"
  (interactive "P")
  (let (limit not-last x (count (or arg 1)))
    (setq limit (save-excursion
		  (re-search-forward "\\(next data set\\)\\|\\<end\\>"
				     (point-max) "to_end")
		  (point-marker)))
    (dotimes (x count (max 0 count))
      (re-search-forward Feffit-paragraph-separate limit "to_limit")
      (setq not-last
	    (re-search-forward "^[ \t]*\\(path\\|fun\\)" limit t)) )
    (back-to-indentation)
    not-last))

(defun input-backward-paragraph ()
  "Move point to the beginning of the previous path paragraph.
Bound to \\[input-backward-paragraph]"
  (interactive)
  (let (limit not-first)
    (setq limit (save-excursion
		  (search-backward "next data set" (point-min) "to_end")
		  (point-marker)))
    (re-search-backward Feffit-paragraph-separate limit "to_limit")
    (setq not-first (re-search-backward "^[ \t]*\\(path\\|fun\\)" limit t))
    (back-to-indentation)
    not-first))

(defun input-mark-paragraph ()
  "Mark the current path paragraph.
Bound to \\[input-mark-paragraph]"
  (interactive)
  (let ((mark-1 nil)
	(limit  nil))
    (setq mark-1 (point))
    (search-backward "next data set"  nil "to_top")
    (setq limit (point))
    (goto-char mark-1)
    (if (re-search-backward Feffit-paragraph-separate limit "to_limit")
	(re-search-forward "^[ \t]*\\(path\\|fun\\)" nil t)
      (input-forward-paragraph))
    (beginning-of-line)
    (push-mark (point) nil t)
    (input-forward-paragraph)))

(defun input-kill-paragraph ()
  "Kill the current path paragraph.
Bound to \\[input-kill-paragraph]"
  (interactive)
  (input-mark-paragraph)
  (kill-region (mark) (point)))

;;;; functions involved in manipulating lists in atoms, normal, feff

;;(defvar input-beginning-of-list)

;;(defun input-beginning-of-list ()
;;  "Move to the beginning of the current list."
;;  (interactive)
;;  (let ((case-fold-search t))
;;    (re-search-forward input-beginning-of-list nil 'move)))


;;;; end of motion code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; snagging code in input-mode

;; $Id: snag.el,v 1.2 1998/03/14 22:46:00 bruce Exp $

;; cleaned up several path param functions, need to work on stanza snag

;; functions defined in this section
;;    input-snag-from-previous-stanza ()
;;    input-snag-from-next-stanza ()
;;    input-snag-from-stanza (arg)
;;    input-find-word-this-stanza (keyword)
;;    input-this-word ()
;;    input-find-path-param (param)
;;    input-this-path-param ()
;;    input-path-param-value ()
;;    input-tell-me-path-param ()
;;    input-find-param-this-paragraph (keyword)
;;    input-snag-from-previous-paragraph ()
;;    input-snag-from-next-paragraph ()
;;    input-snag-from-paragraph (arg)
;;    input-power (num exp)

;; things to do for snag
;;   1.  multiple arguments, use keyword alist
;;   2.  title, others snag whole line (extent of snag function?)
;;   3.  don't snag if not in that textual area

(defun input-snag-from-previous-stanza ()
  "Grab value for keyword under point from previous stanza.
Bound to \\[input-snag-from-previous-stanza]"
  (interactive) (input-snag-from-stanza nil))

(defun input-snag-from-next-stanza ()
  "Grab value for keyword under point from next stanza.
Bound to \\[input-snag-from-next-stanza]"
  (interactive) (input-snag-from-stanza t))

(defun input-snag-from-stanza (arg)
  "Driver for snagging from surrounding stanzas.
Determine the keyword of the current keyword area and grabs the
equivalent value from one of the surrounding stanzas.  If ARG is t
then the snag is from the next stanza and if ARG is nil the the snag
is from the previous stanza."
  ;;(interactive "*P")
  (let (keyword value mark-2 which where firstlast (case-fold-search t))
    (if arg (setq which "next"
		  where "last"
		  firstlast (input-last-stanza-p))
      (setq which "previous"
	    where "first"
	    firstlast (input-first-stanza-p)))

    (if (input-comment-p)
	(message "Point is in a comment.  Snag aborted.")
      (if firstlast
	  (message "Cannot snag from %s stanza.  This is the %s stanza."
		   which where)
	(if (looking-at "\\<\\w")
	    (setq keyword (input-this-word))
	  (re-search-backward input-word-sep)
	  (setq keyword (input-this-word)))
	(setq mark-2 (point))
					; goto previous/next stanza
	(if arg (input-next-stanza 1)
	  (input-previous-stanza 1))
	(setq value (input-find-word-this-stanza keyword))
	(cond (value
	       (goto-char mark-2)
	       (delete-horizontal-space)
	       (if (looking-at "[=,]") (delete-char 1))
	       (insert " = " value)
	       (message "Insert %S for %S" value keyword))
	      (t
	       (goto-char mark-2)
	       (message "%S was not found in the %s stanza" keyword which)))
      ))
    ))


(defun input-find-word-this-stanza (keyword)
  "Find the last occurrence of KEYWORD in the current stanza.
This is done by searching backwards from the end of the stanza.  This
is consistent with the parsing syntax of the uwxafs programs, which
read from the top of the input file and overwrite multiply encountered
keywords."
  (interactive "sWhich keyword?  ")
  (save-excursion
    (let (found expr bos (case-fold-search t))
      (input-end-of-stanza)
      (setq bos (save-excursion (input-beginning-of-stanza) (point-marker)))
      (setq expr (concat "\\<" keyword "[ \t,=]"))
      (while (not found)
	(cond ((re-search-backward expr bos "to_limit")
	       (cond ((or (input-title-p) (input-comment-p))
		      (setq found nil))
		     (t
		      (forward-word 2)
		      (setq found (input-this-word)))))
	      (t
	       (setq found t)))
	)
      (and (equal found t) (setq found nil))
      (and (interactive-p) (message "found %S" found))
      found
      )))

(defun input-this-word ()
  "Return the word under point and leave point at the end of the word."
  (let (begin)
    (or (looking-at "\\<") (= (current-column) 0) (forward-word -1))
    (setq begin (point-marker))
    (forward-word 1)
    (buffer-substring-no-properties begin (point))))


(defun input-find-path-param (param)
  "Search in the current path paragraph for a path parameter PARAM.
If it is found, point is left at the end of parameter and t is returned.
Otherwise nil is returned and point is left at the beginning of the path
paragraph."
  (interactive "sWhich path parameter? ")
  (let (limit expr (case-fold-search t))
    (if (re-search-backward Feffit-paragraph-separate nil t)
	(re-search-forward "^[ \t]*\\(path\\|fun\\)" nil t)
      (input-forward-paragraph))
    (beginning-of-line)
    (setq limit
	  (save-excursion
	    (re-search-forward Feffit-paragraph-separate (point-max) "to_end")
	    (point)))
    (setq expr (concat "^[ \t]*" param))
    (re-search-forward expr limit t) ))

(defun input-this-path-param ()
  "Return the path parameter on the current line.
Return nil if point is not on a line containing a path parameter."
  (save-excursion
    (let (end (case-fold-search t))
      (back-to-indentation)
      (if (not (looking-at input-path-paragraph-regexp))
	  ()
	(setq end (save-excursion (forward-word 1) (point)))
	(buffer-substring-no-properties (point) end))
  )))


(defun input-path-param-value ()
  "Return the path index and value.
Return these for the path parameter on the current line as a two element list."
  (save-excursion
    (let (limit begin index value (case-fold-search t))
					; find end of line
      (back-to-indentation)
      (setq limit (save-excursion (end-of-line) (point)))
					; get index
      (re-search-forward input-word-sep limit t)
      (setq begin (point))
      (forward-word 1)
      (setq index (buffer-substring-no-properties begin (point)))
					; get value
      (re-search-forward input-word-sep limit t)
      (setq begin (point))
      (if (re-search-forward input-comment-expr limit "to_limit")
	  (forward-char -1))
      (setq value (buffer-substring-no-properties begin (point)))
					; return index and value
      (list index value))))


(defun input-tell-me-path-param ()
  (interactive)
  (message "%S : %S" (input-this-path-param) (input-path-param-value)))

(defun input-find-param-this-paragraph (keyword)
  "Move point to beginning of line in current paragraph which contain KEYWORD."
  (let (limit (case-fold-search t))
    (input-forward-paragraph)
    (setq limit (point-marker))
    (input-backward-paragraph)
    (cond ((re-search-forward (concat "^[ \t]*" keyword) limit t)
	   (beginning-of-line) t)
	  (t nil)) ))


(defun input-snag-from-previous-paragraph ()
  "Grab value for keyword under point from previous paragraph.
Determine path parameter of the current line and
grab the equivalent value from the previous paragraph.
Bound to \\[input-snag-from-previous-paragraph]"
  (interactive)
  (input-snag-from-paragraph nil))

(defun input-snag-from-next-paragraph ()
  "Grab value for keyword under point from previous paragraph.
Determine path parameter of the current line and
grab the equivalent value from the next paragraph.
Bound to \\[input-snag-from-next-paragraph]"
  (interactive)
  (input-snag-from-paragraph t))

(defun input-snag-from-paragraph (arg)
  "Driver for snagging from surrounding stanzas.
Determines the path parameter on the current line and grab the
equivalent value from one of the surrounding paragraphs.  If ARG is t
then the snag is from the next paragraph and if ARG is nil the the
snag is from the previous paragraph."
  ;;;(interactive "*P")
  (let (keyword value mark-1 mark-2 which where firstlast
		(case-fold-search t))
					; determine value of keyword at point
    (if arg (setq which "next"
		  where "last"
		  firstlast (input-last-paragraph-p))
      (setq which "previous"
	    where "first"
	    firstlast (input-first-paragraph-p)))

    (if (input-comment-p)
	(message "Point is in a comment.  Snag aborted.")
      (if firstlast
	  (message (concat "Cannot snag from " which
			   " paragraph.  This is the " where " paragraph."))
	(setq keyword (input-this-path-param)
	      mark-2 (point-marker))
					; goto prev/next paragraph
	(if arg (input-forward-paragraph)
	  (input-backward-paragraph))
					; find param in paragraph
	(cond ((input-find-param-this-paragraph keyword)
					; grab the value
	       (setq value (nth 1 (input-path-param-value)))
	       (cond ((or (string-match "\b[ \t]+\b" value)
			  (= (length value) 0))
		      (goto-char mark-2)
		      (message "No value was found in the %s paragraph for %S"
			       which keyword))
		     (t
		      (goto-char mark-2)
		      (back-to-indentation)
		      (setq mark-1 (save-excursion (end-of-line) (point)))
					; insert space and value
		      (if (re-search-forward input-comment-expr
					     mark-1 "to_limit")
			  (forward-char -1))
		      (delete-horizontal-space)
		      (insert (input-make-indent-string
			       input-path-paragraph-separate t))
		      (insert value)
		      (just-one-space)  ; one trailing space
		      (message "Insert %S for %S" value keyword))))
	      (t
	       (goto-char mark-2)
	       (message "%S was not found in the %s paragraph" keyword which)))
	  ))
    ))


(defun input-power (num exp sign)
  "Calculate a exponential from two strings.
Calculate SIGN*NUM^EXP where both NUM and EXP are strings and SIGN is +/-1.
Return the value as a string"
  (let* ((n (string-to-number num)) (e (string-to-number exp)))
    (format "%-7.3f" (* sign n (expt 10 e)))))

;;;; end of snagging code for input mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file handling code in input-mode

;; $Id: files.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

;; allow for special coments in jump-to-data-file

;; functions defined in this section
;;    input-jump-to-file (file &optional writable create)
;;    input-back-to-original ()
;;    input-jump-to-log-file ()
;;    input-log-file-exists-p ()
;;    input-jump-to-data-file ()
;;    input-jump-to-master-file ()
;;    input-jump-to-gnuplot-buffer ()
;;    input-io-file-type ()
;;    input-display-binary-record (datafile key)
;;    input-set-and-jump (event)

(defun input-jump-to-file (file &optional writable create)
  "Open a buffer visiting FILE in the current window.
If WRITABLE is nil then `buffer-read-only' is set to t.
If CREATE is t then the file will be created if doesn't exist."
  (let ((orig (current-buffer)))
    (cond ((or (file-exists-p file) create)
	   (when input-use-frames
		 (raise-frame (selected-frame)))
	   (switch-to-buffer (find-file-noselect file))
	   (setq input-originating-buffer orig)
	   (if writable
	       (message
		(substitute-command-keys
		 "\\[input-back-to-original] to return to previous buffer."))
	     (setq buffer-read-only t)
	     (message (substitute-command-keys "\"\\[toggle-read-only]\" to make buffer writable.  \\[input-back-to-original] to return."))))
	  (t
	   (message "File %S does not exist." file)))))

(defun input-back-to-original ()
  "Return to original buffer.
This is used in a buffer displaying a file that was jumped to from an
input file.  Using return the original file to the current window.
Bound to \\[input-back-to-original]"
  (interactive)
  (if input-originating-buffer
      (switch-to-buffer input-originating-buffer)
    (message "No prior buffer.")))

(defun input-jump-to-log-file ()
  "Display the log file for the current input file in a read-only buffer.
Some programs do not have log files.  feff.inp is the log file for *atoms*
and will be visited write-enabled.  Bound to \\[input-jump-to-log-file]"
  (interactive)
  (let (logfile basename)
					; no log file
    (cond ((not input-program-logfile-flag)
	   (message "The program %S does not have a log file"
		    input-program-name))
	  ((string= input-program-name "feff")
	   (Feff-display-intrp))
	  ((string= input-program-name "atoms")     ;; atoms ==> feff.inp
	   (setq basename (file-name-sans-extension ;;  or xxx-feff.inp
			   (file-name-nondirectory buffer-file-name)))
	   (if (string= basename "atoms")
	       (setq logfile "feff.inp")
	     (setq logfile (concat basename "-feff.inp")))
	   (input-jump-to-file logfile t nil))
	  ((string= input-program-name "xanes")     ;; xanes ==> xhead.dat
	   (input-jump-to-file "xhead.dat"))
	  (t                                        ;; basename.log
	   (setq logfile (or input-master (buffer-file-name))
		 logfile (file-name-sans-extension logfile)
		 logfile (concat logfile ".log"))
	   (input-jump-to-file logfile nil)))    ))

(defun input-log-file-exists-p (&optional ext)
  "Check to see if a log file exists for this input file.
Some programs do not have log files.  feff.inp is the log
file for *atoms* and will be visited write-enabled.  A buffer
containing the output of intrp is the log file.  EXT is an optional
ending to consider other than .log.
Bound to \\[input-jump-to-log-file]"
  (let (logfile basename)
					; no log file
    (cond ((not input-program-logfile-flag)
	   nil)
	  ((string= input-program-name "feff")
	   (and (file-exists-p "paths.dat")
		(file-exists-p "files.dat")))
	  ((string= input-program-name "atoms")     ;; atoms ==> feff.inp
	   (setq basename (file-name-sans-extension ;;  or xxx-feff.inp
			   (file-name-nondirectory buffer-file-name)))
	   (if (string= basename "atoms")
	       (setq logfile "feff.inp")
	     (setq logfile (concat basename "-feff.inp")))
	   (file-exists-p logfile))
	  ((string= input-program-name "xanes")     ;; xanes ==> xhead.dat
	   (file-exists-p "xhead.dat"))
	  (t                                        ;; basename.log
	   (setq logfile (or input-master (buffer-file-name)))
	   (setq logfile (file-name-sans-extension logfile))
	   (setq logfile (concat logfile (or ext ".log"))) ;; .prm, etc
	   (file-exists-p logfile)))    ))

;; probably won't handle uwxafs records in special comments
;; this has been hacked at over time and it shows!
(defun input-jump-to-data-file ()
  "Open a read-only buffer displaying the data or feff file under
point.  Check to see if point is in a comment then make sure that
point is on a file name or on a keyword that takes a file name as an
argument.  the following file types can be jumped to: an input data
file, a feffnnnn.dat file specified in a *feffit* path paragraph, an
include file, a file in a *normal* file list, or a file named in a
special comment (defined as being in a normal comment, but bounded by
angle brackets.  A *uwxafs* data file will be displayed.  A file
specified in a title or a comment cannot be jumped to.
Bound to \\[input-jump-to-data-file]"
  (interactive)
  (let (keyword word datafile filetype mark-1 key (path nil)
		(special (input-special-comment-p)))
    (cond
     ((and (not special) (input-comment-p))
      (message "Cannot jump to file.  Point is in a comment."))
     ((and (not special) (input-title-p))
      (message "Cannot jump to file.  Point is in a title."))
     (t
      (setq mark-1 (point-marker))
      (if special
	  (setq filetype "special_comment")
	(setq word (input-this-word)
	      keyword word)
	(if (try-completion keyword input-current-keywords-alist)
	    ()
	  (re-search-backward "^\\|[ \t]*[ \t,=][ \t]*")  ;; input-word-sep)
	  (setq keyword (input-this-word)))
	(setq filetype (input-get-from-keyword-alist keyword 1))
	(cond ((string= input-program-name "feffit")
	       (save-excursion
		 (beginning-of-line)
		 (if (looking-at "[ \t]*\\(feff\\|path\\)") (setq path t))) )) )
      (cond
       (path                              ; feff path file
	(beginning-of-line)
	(forward-word 2)
	(re-search-forward input-word-sep)
	(setq datafile (input-this-word))
	(goto-char mark-1)
	(input-jump-to-file datafile))
					; data list (e.g. normal)
       (input-program-list-flag
        (goto-char mark-1)
	(if (input-list-p)
	    (progn
	      (back-to-indentation)
	      (forward-word 1)
	      (setq datafile (input-this-word))
	      (cond ((string= (input-io-file-type) "uwxafs")
		     (re-search-forward input-word-sep)
		     (setq key (input-this-word))
		     (goto-char mark-1)
		     (input-display-binary-record datafile key) )
		    (t
		     (goto-char mark-1)
		     (input-jump-to-file datafile))) )
	  (message "%S is not a readable file." word)
	  (goto-char mark-1) ))
					; other readable file
       ((and (listp filetype) (member "readable" filetype))
	(forward-word (1+ (position "readable" filetype :test 'string=)))
	(input-jump-to-file (input-this-word) t t)
	(goto-char mark-1)
	)
       ((and (stringp filetype) (or (string= filetype "readable") special))
	(and (string= filetype "readable") (re-search-forward input-word-sep))
	(setq datafile (input-this-word))
	(cond ((or (string= keyword "include") special)
	       (setq input-included-program input-program-name)
	       (goto-char mark-1)
	       (input-jump-to-file datafile t t))
	      ((string= (input-io-file-type) "uwxafs")
	       (re-search-forward input-word-sep)
	       (setq key (input-this-word))
	       (goto-char mark-1)
	       (input-display-binary-record datafile key) )
	      (t
	       (goto-char mark-1)
	       (input-jump-to-file datafile))) )
       (t
	(message "%S is not a readable file." word)
	(goto-char mark-1))) ))
    ))

(defun input-jump-to-master-file ()
  "Visit the master file for this include file.
Nothing is done if no master file is defined.
Bound to \\[input-jump-to-master-file]"
  (interactive)
  (cond (input-program-master-flag
	 (cond (input-master
		(setq input-included-program input-program-name)
		(input-jump-to-file input-master t t))
	       (t
		(message "No master file has been defined for this file."))))
	(t
	 (message "The program %S does not use include files."
		  input-program-name))))

(defun input-jump-to-gnuplot-buffer ()
  "Display the buffer containing the run-time messages from *gnuplot*.
This will display in the gnuplot frame if `input-use-frames' is non-nil.
Bound to \\[input-jump-to-gnuplot-buffer]"
  (interactive)
  (let ((orig (current-buffer))
	(plist      (cond ((equal input-use-frames 'own)
			   input-gnuplot-frame-plist)
			  ((equal input-use-frames 'share)
			   input-run-frame-plist)))
	(parameters (cond ((equal input-use-frames 'own)
			   input-gnuplot-frame-parameters)
			  ((equal input-use-frames 'share)
			   input-run-frame-parameters)))     )

    (if (bufferp (get-buffer gnuplot-buffer))
	(progn
	  (if input-use-frames		; display run in another frame?
	      (progn
		(if (or (not input-gnuplot-frame)
			(not (frame-live-p input-gnuplot-frame)))
		    (progn
		      (setq input-run-frame
			    (if fuse-xemacs-p
				(make-frame plist)
			      (make-frame parameters)))
		      (select-frame input-gnuplot-frame)
		      (switch-to-buffer gnuplot-buffer)
		      ;;(set-window-dedicated-p (selected-window) t)
		      (raise-frame input-gnuplot-frame)
		      ))
		(if input-always-raise-flag (raise-frame input-gnuplot-frame))
		(select-frame input-gnuplot-frame)
		(switch-to-buffer gnuplot-buffer)
		;;(set-window-dedicated-p (selected-window) t)
		)
	    (switch-to-buffer gnuplot-buffer))
	  (setq input-originating-buffer orig)
	  (message
	   (substitute-command-keys
	    "\\[input-back-to-original] to return to gnuplot buffer.")))
      (message "There isn't an active gnuplot session running.")) ))


;; how is this used?  is it input only or input/output?
(defun input-io-file-type ()
  "Return a string identifying the output file format.
This string  is either 'ascii' or 'uwxafs'."
  (let ((parse-function (nth 0 input-program-parse))
	(program-alist  (nth 1 input-program-parse)))
    (cond (parse-function
	   (funcall parse-function)
	   (cdr (assoc "formin" (eval program-alist))))
	  (t                ;; only a few programs use the binary format
	   "ascii")) ))

(defun input-display-binary-record (datafile key)
  "Display a *uwxafs* binary record in a buffer.
This is done by by calling *mr* and displaying the output in a scratch buffer.
DATAFILE is the *uwxafs* file and KEY is the numeric or symbolic key."
  (let ((buffer (format "%s,%s" datafile key))
	(orig (current-buffer)))
    (if (get-buffer buffer)
	(switch-to-buffer buffer)
      (switch-to-buffer buffer)
      (call-process "mr" nil t nil datafile key)
      (goto-char (point-min))
      (setq input-originating-buffer orig
	    buffer-read-only t)
      (message (substitute-command-keys
		"\\[input-back-to-original] to return to previous buffer.")) )))

(defun input-set-and-jump (event)
  "Jump to the file under point using the mouse.
The file under the mouse can be a data file, a feff file, an include
file, or a file in a list of files.  See `input-jump-to-data-file'.
If the buffer contain a feff input file and you click on a CONTROL
line the `Feff-swap-control' will be run instead of
`input-jump-to-data-file'.
Argument is a mouse EVENT.  Bound to \\[input-set-and-jump]"
  (interactive "@e")
  (mouse-set-point event)
  (if (and (string= input-program-name "feff")
	   (Feff-control-p))
      (Feff-swap-control)
    (input-jump-to-data-file)))

;;; functions for setting/clearing various overlays/extents

(defun fuse-toggle-hide-autoconfig (&optional switch)
  "Turn on or off hidden autoconfiguration variables.
Certainly on if SWITCH > 0, certainly off
if SWITCH < 0, toggle if switch is nil."
  (interactive)
  (cond ((and (numberp switch) (> 0 switch))
	 (setq fuse-hide-autoconfig-flag t))
	((and (numberp switch) (< 0 switch))
	 (setq fuse-hide-autoconfig-flag nil))
	(t
	 (setq fuse-hide-autoconfig-flag (not fuse-hide-autoconfig-flag))))
  (if fuse-hide-autoconfig-flag
      (fuse-hide-autoconfig)
    (widen)))

(defun fuse-hide-autoconfig ()
  (if fuse-hide-autoconfig-flag
      (progn
	(let ((case-fold-search t)
	      (regexp (concat input-mode-variable-comment "[ \t]*Local")))
	  (save-excursion
	    (goto-char (point-max))
	    (when (re-search-backward regexp (point-min) t)
	      (beginning-of-line)
	      (narrow-to-region (point-min) (point-marker))) )))))


(defun fuse-toggle-mouse-overlays (&optional switch)
  "Turn on or off mouse overlays/extents.
These cause color flashing (and in xemacs a help-echo) when the
mouse nears a region affected by shift-mouse-3.  See
`input-set-and-jump'  Certainly on if SWITCH > 0, certainly off
if SWITCH < 0, toggle if switch is nil."
  (interactive)
  (cond ((and (numberp switch) (> 0 switch))
	 (setq fuse-mouse-overlays-flag t))
	((and (numberp switch) (< 0 switch))
	 (setq fuse-mouse-overlays-flag nil))
	(t
	 (setq fuse-mouse-overlays-flag (not fuse-mouse-overlays-flag))))
  (if fuse-mouse-overlays-flag
      (fuse-set-all-mouse-overlays)
    (fuse-clear-overlays 'mouse-face 'fuse-mouse-face)))

(defun fuse-set-all-mouse-overlays ()
  "Set the mouse overlays/extents in this file.
See `fuse-mouse-highlight-list'."
  (interactive)
  (when fuse-mouse-overlays-flag
    (fuse-clear-overlays 'face 'fuse-mouse-face)
    (map nil 'fuse-set-mouse-overlays fuse-mouse-highlight-list)
    (if (interactive-p) (message "Mouse flashing set")))
  nil)

(defun fuse-set-mouse-overlays (list)
  (let ((regexp (car list)) (count (elt list 1))
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
	(let* ((begin (match-beginning count)) ; save these now, they are over
	       (end   (match-end count))) ; written by special-comment-p!
	  (when (or (and (not (input-comment-p))
			 (not (fuse-next-data-set-p)))
		    (or (input-special-comment-p)
			(save-excursion
			  (backward-word 1)
			  (input-special-comment-p))))
	    (cond (fuse-xemacs-p
		   (let ((extent (make-extent begin end)))
		     (set-extent-endpoints extent begin end)
		     (set-extent-mouse-face extent 'fuse-mouse-face)
		     (if (string= input-program-name "feff")
			 (set-extent-property
			  extent 'help-echo
			  "Shift-Mouse-3 to toggle this flag.")
		       (set-extent-property
			extent 'help-echo
			"Shift-Mouse-3 to view this file."))))
		  (t
		   (overlay-put (make-overlay begin end)
				'mouse-face 'fuse-mouse-face))))
	  (forward-line 1) )))))

;; why does this not obey the input-blank-p exception?
(defun fuse-mark-hotspots (list)
  "Mark each point in LIST with an overlay/extent.
The purpose of this is to mark the hotspots in tempo templates.
LIST must be a list of markers.  This works somewhat nicer in XEmacs
where the 'help-echo, 'start-open, and 'start-closed properties can be
set.  For Emacs, I move the overlay back one character so it is easier
to avoid extending the overlay, but this is not foolproof, so it is a
good idea to do \\[fuse-clear-hotspot-overlays] after filling in the template."
  (interactive)
  (save-excursion
    (while list
      (let ((marker (car list)))
	(when (markerp marker)
	      (goto-char marker)
	      (unless (or (bobp) (eobp) (input-blank-p))
		      (cond (fuse-xemacs-p
			     ;;(when (looking-at "\n")
			     ;;(insert " ") (backward-char 1))
			     (let* ((begin (1- marker))
				    (end marker)
				    (extent (make-extent begin end)))
			       (set-extent-endpoints extent begin end)
			       (set-extent-face extent 'fuse-hotspot-face)
			       (set-extent-property extent 'start-open t)
			       (set-extent-property extent 'end-open t)
			       (set-extent-property
				extent 'help-echo
				(concat "Insert value at hotspot.  "
					"M-n and M-p to move between "
					"hotspots"))))
			    (t
			     (overlay-put (make-overlay (1- marker) marker)
					  'face 'fuse-hotspot-face))))))
      (setq list (cdr list))	)))

;; want a key binding so...
(defun fuse-clear-hotspot-overlays ()
  (interactive) (fuse-clear-overlays 'face 'fuse-hotspot-face))

(defun fuse-clear-overlays (prop value)
  "Clear all overlays or extents for which PROP is VALUE."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond (fuse-xemacs-p
	   (mapcar-extents 'delete-extent nil (current-buffer)
			   (point-min) (point-max) nil prop value))
	  (t
	   (while (not (eobp))
	     (goto-char (next-overlay-change (point)))
	     (let ((overlays (overlays-at (point))))
	       (while overlays
		 (if (eq (overlay-get (car overlays) prop) value)
		     (delete-overlay (car overlays)))
		 (setq overlays (cdr overlays)))))) )))

;;;; end of file handling code in input-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bug reporting/feedback submiting code in input-mode

;; $Id: bug.el,v 1.3 1998/03/16 01:17:38 bruce Exp $

;; functions defined in this section
;;    input-submit-feedback ()
;;    input-submit-program-bug-report ()

;;;###autoload
(defun input-submit-feedback ()
  "Submit feedback on FUSE to the author <ravel@phys.washington.edu>.
Bound to \\[input-submit-feedback]"
  (interactive)
  (let ((line (make-string 70 ?-)))
    (require 'reporter)
    (and (y-or-n-p
	  "Do you really want to submit an email report about FUSE? ")
	 (y-or-n-p
	  "Variable values will be written to the mail.  Don't erase them.  OK? ")
	 (reporter-submit-bug-report
	  (format "%s <%s>" input-author input-author-email)
	  (format "FUSE (version %s)" input-mode-version)
	  (append      ; variables to display values of in mail
	   '(input-load-hook
	     input-mode-hook
	     input-after-run-hook
	     input-emulation
	     fuse-base-directory
	     input-bin-location
	     input-document-location
	     input-program-document-location
	     fuse-glyph-directory
	     fuse-use-toolbar
	     input-comment-list
	     input-mode-variable-comment
	     input-prohibit-autoconfig-flag
	     input-comment-delimiter
	     input-stanza-delimiter
	     input-upcase-keywords-flag
	     input-best-fit-set-flag
	     input-stanza-indent
	     input-path-paragraph-indent
	     input-path-paragraph-separate
	     input-set-guess-indent
	     input-set-guess-separate
	     input-list-indent
	     input-list-separate
	     input-feff-indent
	     input-potentials-indent
	     input-potentials-separate
	     input-atoms-separate
	     input-comment-indent
	     input-intrp-buffer-name
	     input-intrp-args
	     input-mcmaster-sigma
	     input-mcmaster-fourth
	     input-mcmaster-ampfac
	     gnuplot-script-buffer-name
	     input-gnuplot-r-column
	     input-gnuplot-q-column
	     input-gnuplot-data-style
	     input-gnuplot-default-terminal
	     input-gnuplot-default-ps-file
	     input-gnuplot-ezero-flag
	     input-plot-flag
	     input-init-file
	     input-run-log-interactive
	     input-run-log
	     input-run-log-max-lines
	     input-stanza-name
	     input-beep-flag
	     input-use-frames
	     input-always-raise-flag
	     input-document-type
	     input-doc-frame-plist
	     input-doc-frame-parameters
	     input-run-frame-plist
	     input-run-frame-parameters
	     input-time-stamp-flag
	     input-time-stamp-begin
	     input-time-stamp-line-limit
	     Atoms-auto-jump-flag
	     features ))
	  nil				; pre-hooks
	  nil				; post-hooks
	  (concat line                  ; salutation
	   "\nInsert your description of the bug report here.\n"
	   "Please be as specific as possible.  Feel free to send me\n"
	   "the input file causing the trouble.  You can insert the\n"
	   (substitute-command-keys
	    "troublesome input file using \\[insert-file]. \n\n\n")
	   line)
	  ))))


(defun input-submit-program-bug-report ()
  "Submit a bug report for a program.
The mail will be sent to the maintainer of the program or to the
maintainer of the *uwxafs* package.  Use this function only for
submitting bug reports about feff or the analysis software.  Use
\\[input-submit-feedback] to submit a bug report about FUSE.
Bound to \\[input-submit-program-bug-report]"
  (interactive)
  (let ((line (make-string 70 ?-)) (file (buffer-file-name)))
    (require 'reporter)
    ;;(save-some-buffers)
    (and (y-or-n-p (format
		    "Do you really want to submit a bug report about %s "
		    input-program-name))
	 (y-or-n-p
	  "Some information will be written to the mail.  Don't erase.  OK? ")
	 (reporter-submit-bug-report
	  (format "%s <%s>" (nth 0 input-program-program-author)
		            (nth 1 input-program-program-author))
	  (format
	   "Bug report about %s
         report form generated by FUSE (version %s)"
	   input-program-name input-mode-version)
	  (append			; variables to display values of in mail
	   '(file
	     input-program-name
	     input-feff-path
	     input-data-path
	     input-out-path
	     input-master
	     input-program-version ))
	  (lambda ()		 	; post-hooks
	    (if (file-exists-p file) (insert-file-contents file)))
	  nil				; pre-hooks
	  (concat line                  ; salutation
		  "\nInsert your description of the bug report here.\n"
		  "Please be as specific as possible.  Some information\n"
		  "is already included in this message.  Please also include\n"
		  "as much of the following as possible:\n"
		  "  1. The problematic data, if applicable.\n"
		  "  2. Any output files generated by the program.\n"
		  "  3. Any run-time messages written to the screen.\n"
		  "  4. Which compiler and compiler flags you used to\n"
		  "     make the executable.\n\n"
		  "If you used the program execution features of FUSE, you\n"
		  "cut and paste run-time messages from the buffer "
		  input-program-buffer
		  "\nYou can insert the troublesome data and output files \n"
		  (substitute-command-keys
		   "using \\[insert-file]. \n")
		  line)
	  ))))


;; (defconst fuse-ftp-site "atropos.nist.gov/pub/bruce/fuse/"
;;   "Main FTP site with latest version of FUSE.")
;; (defun fuse-visit-home ()
;;  "Jump to the main FTP site for `fuse-doc'."
;;  (interactive)
;;  (if (string-match "XEmacs" emacs-version)
;;     (require 'efs)
;;   (require 'ange-ftp))
;;  (require 'dired)
;;  (if input-use-frames
;;      (dired-other-frame fuse-ftp-site)
;;    (dired-other-window fuse-ftp-site)))


;;;; end of reporting/feedback submiting code in input-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; beginning of main input mode functionality + mode definition

;; $Id: main.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

(defvar input-autoconfig-alist '((input-program-name    . nil)
				 (input-master          . nil)
				 (input-data-path       . nil)
				 (input-feff-path       . nil)
				 (input-out-path        . nil)
				 (input-k-weight        . "1")
				 (input-eshift          . "0")
				 (input-program-version . nil))
  "Alist of variables auto-configured by FUSE.")

;; to add new autoconfiguration variables, add an entry here, in the
;; variable declarations in (input-mode), and in the variable
;; declarations in variables.el

;;;###autoload
(defun input-read-init-file (&optional rcfile)
  "Read the *FUSE* initialization file.
Typically this is `.fuse' in the user's home directory
or some other initialization file if the argument RCFILE is provided."
  (interactive)
  (load (or rcfile (expand-file-name input-init-file)) t nil))

(defun input-set-configuration ()
  "Refresh the Local Variable values in this file.
The Local Variable list can be edited on screen and the variables
reset using this function.  Bound to \\[input-set-configuration]"
  (interactive)
  (let ((expr (concat "^[ \t]*" (regexp-quote comment-start))) item value)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward expr (point-max) t)
	(forward-sexp)
	(setq item (buffer-substring-no-properties
		    (save-excursion (backward-sexp) (point))
		    (point)))
	(unless (string-match "Local\\|End" item)
	  (let ((eol (save-excursion (end-of-line) (point-marker))))
	    (search-forward "\"" eol "to_limit")
	    (setq value (buffer-substring-no-properties
			 (save-excursion (search-forward "\"" eol t)
					 (point-marker))
			 (point-marker))
		  value (remove* ?\" value :test 'char-equal :count 1)
		  value (remove* ?\" value :test 'char-equal :count 1
				 :from-end t))
	    (set (intern item) value)))))))

(defun input-write-configuration ()
  "Write auto-configuration lines as Local Variables.
This is used as a `write-contents-hook', thus auto-configuration is
written/updated upon saving file.  Writing is inhibited if
`input-prohibit-autoconfig-flag' is t."
  (interactive)
  (unless input-prohibit-autoconfig-flag
    (save-excursion
      (widen)
      (save-excursion
	(let ((case-fold-search t) item default begin end
	      (expr (regexp-quote input-time-stamp-begin)))
	  (goto-char (point-max))
	  (if (string= input-program-name "feff")
	      (setq expr (concat "\\*" expr)))
	  (search-backward expr (point-min) t)
					; find variables location in file
	  (if (re-search-backward (concat (regexp-quote comment-start)
					  "Local Variables:")
				  (point-min) t)
	      (forward-line 1)
	    (newline 2);; if not found, insert begin and end lines
	    (insert comment-start "Local Variables:\n")
	    (insert comment-start "End:\n") ;)
	    (forward-line -1))

	  (setq begin (point-marker)	; mark out boundries of L.V. list
		end (save-excursion
		      (re-search-forward
		       (concat (regexp-quote comment-start) "End:")
		       (point-max) "to_limit")
		      (beginning-of-line)
		      (point-marker)))
					; insert variable values
	  (dotimes (x (length input-autoconfig-alist) t)
	    (goto-char end)
	    (let ((item    (car (elt input-autoconfig-alist x)))
		  (default (cdr (elt input-autoconfig-alist x))))
	      (unless (equal (eval item) default)
		(when (re-search-backward
		       (concat (regexp-quote comment-start)
			       (symbol-name item)) begin t)
		  (beginning-of-line)
		  (delete-region (point) (save-excursion
					   (end-of-line)
					   (forward-char 1) (point))))
		(insert (format "%s%s: %S\n" comment-start
				(symbol-name item) (eval item)))))) )))
  nil))

(defun input-convert-old-configuration ()
  "Convert old-style autoconfiguration lines to a Local Variable list.
This is run when input-mode starts unless
`input-never-convert-autoconfig-flag' is non-nil."
  (let ((alist '(("data"    . input-data-path)
		 ("eshift"  . input-eshift)
		 ("feff"    . input-feff-path)
		 ("kweight" . input-k-weight)
		 ("master"  . input-master)
		 ("out"     . input-out-path)
		 ("program" . input-program-name)
		 ("version" . input-program-version)))
	(expr "^[ \t]*\\*?!!&&[ \t]+\\(data\\|eshift\\|feff\\|kweight\\|master\\|out\\|program\\|version\\):[ \t]*")
	flag value (cs ""))
    (save-excursion
      (goto-char (point-min)) ; insert begin and end lines
      (while (re-search-forward expr (point-max) t)
	(beginning-of-line)
	(if (looking-at "\\*") (setq cs "*"))
	(insert (concat cs (regexp-quote comment-start) "Local Variables:\n"))
	(goto-char (point-max))
	(re-search-backward expr (point-min) t)
	(end-of-line)
	(insert (concat "\n" cs (regexp-quote comment-start) "End:")) )
      (goto-char (point-min)) ; update each old-style line found
      (while (re-search-forward expr (point-max) t)
	(unless flag
		(message "Converting old-style autoconfiguration lines ..."))
	(setq flag t)
	(let ((item (cdr (assoc (match-string 1) alist)))
	      (eol (save-excursion (end-of-line) (point-marker))))
	  ;;(forward-sexp)  ;; why did this not work???
	  (search-forward "\"" eol "to_limit")
	  (setq value (buffer-substring-no-properties
		       (save-excursion ;;(backward-sexp)
				       (search-forward "\"" eol t)
				       (point-marker))
		       (point-marker))
		value (remove* ?\" value :test 'char-equal :count 1)
		value (remove* ?\" value :test 'char-equal :count 1 :from-end t))
	  (set item value)
	  (beginning-of-line)
	  (delete-region (point) (save-excursion (end-of-line) (point)))
	  (insert (format "%s%s: %S" (concat cs comment-start)
			  (symbol-name item) value)) )))
    (if flag (message "Converting old-style autoconfiguration lines ... done."))
    ))

(defun input-end-newline ()
  "Terminate input file with a newline.  This function is used as a
`local-write-file-hook' to assure that a file edited under
`input-mode' ends with a newline character.  This is necessary to
assure that the *uwxafs* programs correctly read the last line of the
input file."
  (save-excursion
    (unless (re-search-forward "\n\\'" (point-max) "move") (insert "\n"))
    nil))

(defun input-save-run-log (&optional arg)
  "Save contents of the run-time display buffer to a file.
This is used as a `kill-emacs-hook' as well as an interactive
function.  If ARG is non-nil (e.g. from using the
\\[universal-argument] prefix) then the run log will be displayed in a
buffer with the saved file name.  Bound to \\[input-save-run-log]"
  (interactive "P")
  (when (get-buffer input-program-buffer)
    (let ((input (current-buffer)) file)
      (if (interactive-p)
	  (setq file (read-file-name "Name for run-time log file > "
				     default-directory
				     input-run-log-interactive nil
				     input-run-log-interactive))
	(setq file input-run-log))
      (setq file (expand-file-name file))
      (switch-to-buffer file)
      (erase-buffer)
      (insert-buffer input-program-buffer)
      (write-file file)
      (unless arg
	      (kill-buffer (current-buffer))
	      (switch-to-buffer input)
	      (message "Wrote run-time log as %S" (file-name-nondirectory file)))
      )))

(defun input-visit-previous-run-log ()
  "Examine the run-time log file from your previous *FUSE* session.
Bound to \\[input-visit-previous-run-log]"
  (interactive)
  (cond ((file-readable-p input-run-log)
	 (input-jump-to-file input-run-log)
	 (set-visited-file-name nil))
	(t
	 (message "%S does not exist.  Have you ever run FUSE?"
		  input-run-log))) )

;;;###autoload
(defun input-fuse-document ()
  "Look at on-line documentation for *FUSE* using info or the w3 package."
  (interactive)  (input-document "fuse"))
(defun input-atoms-document ()
  "Look at on-line documentation for *atoms* using info or the w3 package."
  (interactive)  (input-document "atoms"))
(defun input-feff-document ()
  "Look at on-line documentation for *feff* using info or the w3 package."
  (interactive)  (input-document "feff7"))
(defun input-normal-document ()
  "Look at on-line documentation for *normal* using info or the w3 package."
  (interactive)  (input-document "normal"))
(defun input-phit-document ()
  "Look at on-line documentation for *phit* using info or the w3 package."
  (interactive)  (input-document "phit"))

;; (if (boundp 'Info-directory-list)
;;     (setq Info-directory-list (append
;; 			       Info-directory-list
;; 			       input-document-location))
;;(if (string-match input-document-location (getenv "INFOPATH"))
;;    ()
(setenv "INFOPATH" (concat (getenv "INFOPATH") ":" input-document-location)) ;)

;;;###autoload
(defun input-document (&optional doc)
  "Look at on-line documentation using info or the w3 package.
If use of both info and w3 is turned off, then the plain text document
will be displayed.  If DOC not provided, it will prompted for in
minibuffer."
  (interactive)
  (setq doc (or doc "fuse-dir"))
  (let ((location input-document-location))
    (cond (doc
	   (if input-use-frames   ; display doc in another frame?
	       (progn
		 (if (or (not input-doc-frame)
			 (not (frame-live-p input-doc-frame)))
		     (progn
		       (setq input-doc-frame
			     (if (string-match "XEmacs" emacs-version)
				 (make-frame input-doc-frame-plist)
			       (make-frame input-doc-frame-parameters)))
		       (select-frame input-doc-frame)
		       (raise-frame input-doc-frame)))
		 (if (frame-iconified-p input-doc-frame)
		     (deiconify-frame input-doc-frame))
		 (select-frame input-doc-frame)
		 (raise-frame input-doc-frame)) )
	   (cond ((string= input-document-type "html")
		  (if (string= doc "fuse-dir")
		      (setq file (concat location doc ".html"))
		    (setq file (concat location doc "_toc.html")))
		  (if (file-exists-p file)
		      (w3-open-local file)
		    (message "Could not file html file: %s" file)))
		 ((string= input-document-type "text")
		  (setq file (concat location doc ".txt"))
		  (if (file-exists-p file)
		      (input-jump-to-file file)
		    (message "Could not file text file: %s" file)))
		 (t
		  (setq file (concat location doc ".info"))
		  (if (file-exists-p file)
			(info file)
		    (message "Could not file info file: %s" file))) ))
	  (t
	   (message "Sorry, that documentation is not available.")))
    ))

(defun input-parse-for-value (vble)
  "Determine a variable value from Local Variables list.
This is needed in cases where one of the autoconfiguration variable
values is needed before `after-find-file' has set the Local Variables.
An example is that `input-determine-program' needs to know the value
of `input-program-name' to do its thing.  VBLE is the name of the
variable as a string."
  (let ((expr (concat (regexp-quote comment-start)
		      "[ \t]*" vble
		      "[ \t]*:[ \t]*\""))
	start)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward expr (point-max) t)
	(setq start (point-marker))
	(forward-sexp 1)
	(buffer-substring-no-properties start (point))))))

(defun input-determine-program (&optional prog)
  "Determine the program for the input file.
If the program auto-configuration line is set in the file, then the
program specified there is used.  Next the name of the input file is
used, i.e. *program*.inp gets set to *program*.  Finally the user is
prompted for a valid program from the minibuffer using completion.  If
nothing is selected or if an invalid program name is entered, no
program will be associated with the input file and no minor mode will
be entered.  When used interactively to switch programs, the program
is prompted for in the minibuffer.  If an invalid response is given,
then the function does nothing."
  (interactive)

  (catch 'return-early
    (let (feature mode current-program (program-list ()))

      (setq program-list (all-completions "" input-programs-alist))
      (if (interactive-p)
	  (setq current-program input-program-name))

      (cond (input-included-program
	     (setq input-program-name input-included-program)
	     (setq input-included-program nil))
	    (prog
	     (setq input-program-name prog)))

      (when (not input-program-name)	; determine program
	    (setq input-program-name (file-name-sans-extension
				      (file-name-nondirectory buffer-file-name)))
	    (cond ((and (> (length input-program-name) 5)
			(string= (substring input-program-name -5 nil) "-feff"))
		   (setq input-program-name "feff"))
		  ((string= input-program-name "p1")
		   (setq input-program-name "atoms"))
		  ((string= input-program-name
			    (file-name-sans-extension input-stanza-name))
		   (setq input-program-name input-stanza-program)) ))

      (setq input-program-name (downcase input-program-name))
					; if interactive or
					; if not found yet, ask
      (when (not (member input-program-name program-list))
	(cond ((or (interactive-p) (not fuse-always-start-generic-flag))
	       (setq input-program-name
		     (completing-read
		      "For which program is this an input file? (<tab> for list) "
		      input-programs-alist nil t)))
	      ((and fuse-always-start-generic-flag
		    (not (member input-program-name program-list)))
	       (setq input-program-name "generic"))))

      (cond ((member input-program-name program-list)
	     ;; do I want to do this?? I think not.
	     ;;(unload-feature input-current-feature)
	     (cond ((string-match "quit" input-program-name)
		    (throw 'return-early nil))
		   ((string= input-program-name "generic")
		    (setq input-prohibit-autoconfig-flag t)))
	     (if input-current-feature
		 (input-turn-off-minor-mode current-program)))
	    ((> (length input-program-name) 0)
	     (message "%S is not one of the available minor modes"
		      input-program-name)
	     (sit-for 2)
	     (setq input-prohibit-autoconfig-flag t)
	     (setq input-program-name current-program)
	     (throw 'return-early nil))
	    (t
	     (setq input-prohibit-autoconfig-flag t)
	     (setq input-program-name current-program)
	     (throw 'return-early nil)))

					; load appropriate file and set stuff
      (setq mode  (cdr (assoc input-program-name input-programs-alist)))
      (setq feature (intern (concat "fuse-" input-program-name)))
      (require feature)
      (funcall mode t)
      (setq input-current-feature feature)
      (funcall input-program-hilit)
      (cond (input-use-font-lock
	     ;; this appears to be a genuine difference between the emacsen
	     (cond ((string-match "XEmacs" emacs-version)
		    (turn-off-font-lock)
		    (put 'input-mode 'font-lock-defaults
			 '((input-font-lock-keywords
			    input-font-lock-keywords-1
			    input-font-lock-keywords-2)
			   t t ((?\_ . "w"))))
		    (turn-on-font-lock) )
		   (t
		    (font-lock-change-major-mode)) ))
	    (input-use-hilit19
	     (hilit-repaint-command t)))
      )))

;; (defun input-menu ()
;;   "Return the proper menu."
;;   input-mode-menu)

(defun input-turn-off-minor-mode (mode)
  "Turn off current minor MODE before switching to a different minor mode."
  (let ((mode-title (cdr (assoc mode input-programs-alist))))
    (funcall mode-title nil)))

(defun input-show-version ()
  "Show FUSE version number in echo area."
  (interactive)
  (message "FUSE version %s by %s, <%s>"
	   input-mode-version input-author input-author-email))

(defun input-set-time-stamp ()
  "Set time stamp in input file.
Check to see if there is a time stamp string in the last 8 lines of
a file.  If not, insert the time stamp.  Then call `time-stamp'.  Time
stamp is not written if `input-time-stamp-flag' is nil."
  (interactive)
  (when input-time-stamp-flag
    (let ((string input-time-stamp-begin))
      (save-excursion
	(if (string= input-program-name "feff")
	    (setq string (concat "*" input-time-stamp-begin)))
	(goto-char (point-max))
	(forward-line time-stamp-line-limit) ; must be a negative number!!!
	(or (search-forward "Time-stamp" (point-max) "to_limit")
	    (insert string ">\n") )
	(time-stamp)	)))
  nil)


(defun input-emulation ()
  "Add emulation functions to input-load-hook.
This is run when `input-mode' is first called, after reading the initialization
file, but before running the mode hook.  If `input-emulation' is vi,
crisp, or edt, then FUSE will be sure to enter one of those emulation
modes.  This is necessary for users than never use emacs except to run
fuse (if such a thing exists!) because they might not have a .emacs
entry to otherwise enter the appropriate emulation mode."
  (cond ((string= input-emulation "vi")
	 (eval-and-compile  (or (require 'viper) (require 'vip)))
	 ;;(and (boundp 'viper-mode) (setq viper-mode t))
	 ;;(and (boundp 'vip-mode)   (setq vip-mode t))
	 (setq vip-expert-level '2
	       vip-inhibit-startup-message 't)
	 (require 'viper)
	 (cond ((fboundp 'vip-change-state-to-vi)
		(vip-change-state-to-vi))
	       ((fboundp 'viper-change-state-to-vi)
		(viper-change-state-to-vi)))	 )
	((string= input-emulation "crisp")
	 (require 'crisp)
	 (crisp-mode))
	((string= input-emulation "edt")
	 (edt-emulation-on))    ))

;; this is blatantly swiped from VM by Kyle Jones ----------------------
(defun fuse-unsaved-message (&rest args)
  (let ((message-log-max nil))
    (apply (function message) args)))
(defun fuse-display-startup-message (&optional arg)
  (interactive)
  (if (or (interactive-p) (sit-for 1))
      (let ((lines fuse-startup-message-lines))
	(message "FUSE version %s.  Copyright (C) 1998 %s <%s>"
		 input-mode-version input-author input-author-email)
	(if arg (setq lines (cons arg lines)))
	(while (and (sit-for 4) lines)
	  (message (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (fuse-unsaved-message ""))
;; ---------------------------------------------------------------------

;;input-insert-best-fit-flag
(defun input-set-local-variables ()
  "Set `input-mode' buffer-local variables."
  (let ((vble-list '(input-comment-list
		     input-mode-variable-comment
		     input-prohibit-autoconfig-flag
		     input-comment-delimiter input-stanza-delimiter
		     input-upcase-keywords-flag
		     input-intrp-buffer-name input-intrp-args
		     input-mcmaster-sigma
		     input-mcmaster-fourth input-mcmaster-ampfac
		     input-stanza-indent input-path-paragraph-indent
		     input-path-paragraph-separate
		     input-set-guess-indent input-set-guess-separate
		     input-list-indent input-list-separate
		     input-feff-indent input-potentials-indent
		     input-potentials-separate input-atoms-separate
		     input-comment-indent input-gnuplot-terminal
		     gnuplot-script-buffer-name input-gnuplot-r-column
		     input-gnuplot-q-column input-gnuplot-postscript-file
		     input-gnuplot-ezero-flag
		     input-plot-flag input-current-keywords-alist
		     input-title-line-regexp input-output-files
		     input-stanza-name input-beep-flag input-document-type
		     input-time-stamp-flag input-time-stamp-begin
		     input-time-stamp-line-limit fuse-mouse-overlays-flag
		     fuse-running-this-file fuse-mouse-highlight-list)))
    (map nil 'make-local-variable vble-list)
    (defvar input-gnuplot-terminal input-gnuplot-default-terminal)    ))

;;;###autoload
(defun input-mode ()
  "Major mode for editing input files for *feff* and the *uwxafs*
programs.  Input mode is generally run with one of the program
specific minor modes also turned on.  Turning on `input-mode' calls
the value of the variable input-mode-hook, if that value is non-nil,
and each minor mode calls <program>-mode-hook, if that value is
non-nil.  <program> is one of
        atoms  feff  autobk  feffit
Minor modes providing support for diffkk, normal, fluo and phit are
also available.  See the document for details about adding new minor
modes to FUSE.

FUSE (The Feff/UWXAFS System for Emacs) is the work of Bruce Ravel
<ravel@phys.washington.edu> and is distributed free of charge to users
of Feff and UWXAFS (and anyone else who, for some reason, wants it).
This software comes with absolutely no warrenty (\\[describe-copying]).
Any suggestions, comments, criticisms, or contributions to FUSE are
gratefully accepted.  Use \\[input-submit-feedback] to make a bug
report or in any other way communicate with FUSE's author.

Current version numbers of programs at the time that this FUSE
distribution was assembled are:
         program   version number        document on-line in FUSE?
         -------   ---------------       ---------------------------
         atoms     2.46b or 2.47b               yes
         feff      6.01, 7.02, 8xNN             yes (7.02)
         autobk    2.61 or 2.63                 not yet
         feffit    2.32 or 2.52g                not yet

Defined keys in input-mode:\n\\{input-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'input-mode
	mode-name input-mode-name)
  (mapc 'make-variable-buffer-local
	(append '(input-program-name input-current-feature
		  input-data-path input-feff-path input-out-path
		  input-k-weight input-eshift input-master
		  input-program-name input-program-version)))
					; set many buffer-local variables
  (make-local-variable 'comment-start)
  (setq comment-start input-mode-variable-comment)
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (unless input-init-file-loaded
	  (input-read-init-file)
	  (setq input-init-file-loaded t))
  (input-set-local-variables)
  (setq input-program-name    nil       ; these two must be set
	input-current-feature nil)      ; to nil before calling
					; autoconfig variables
  (add-to-list 'exec-path input-bin-location)
  (or (string-match input-bin-location (getenv "PATH"))
      (setenv "PATH" (concat input-bin-location path-separator (getenv "PATH"))))
  (unless input-never-convert-autoconfig-flag (input-convert-old-configuration))
  (let ((prog (input-parse-for-value "input-program-name")))
    (input-determine-program prog))
  (if (string-match "quit" input-program-name)
      ;; bail out of input-mode
      (text-mode)
    (if (member input-emulation input-emulation-list) (input-emulation))
    (use-local-map input-mode-map)
    (easy-menu-add input-mode-menu input-mode-map)
    (set-syntax-table input-mode-syntax-table)
    (setq local-abbrev-table input-mode-abbrev-table)
					; set local hooks for input mode
    (make-local-hook 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'input-end-newline           nil t)
    (add-hook 'write-contents-hooks 'input-write-configuration   nil t)
    (add-hook 'write-contents-hooks 'fuse-set-all-mouse-overlays nil t)
    (add-hook 'write-contents-hooks 'fuse-clear-hotspot-overlays nil t)
    (add-hook 'kill-emacs-hook 'input-save-run-log)
					; use time-stamp.el
    (add-hook 'write-contents-hooks 'input-set-time-stamp nil t)
    (make-local-hook 'find-file-hooks)
    (add-hook 'find-file-hooks 'fuse-hide-autoconfig)
    (make-local-variable 'time-stamp-active)
    (make-local-variable 'time-stamp-line-limit)
    (setq time-stamp-active     t
	  time-stamp-line-limit input-time-stamp-line-limit)
					; run hooks then tidy up
    (make-variable-buffer-local 'font-lock-defaults)
    (setq font-lock-defaults '((input-font-lock-keywords
				input-font-lock-keywords-1
				input-font-lock-keywords-2)
			       t t ((?\_ . "w"))))
    (if fuse-mouse-overlays-flag
	(fuse-set-all-mouse-overlays))
    (run-hooks 'input-mode-hook)
    (when (and (not fuse-inhibit-startup-message)
	       (not fuse-startup-displayed))
	       ;;(or (memq this-command 'input-mode))
		;;   (null last-command))
	  (if (string-match "XEmacs" emacs-version)
	      (add-timeout 1 'fuse-display-startup-message
			   (concat "Use the toolbar to make templates,"
				   " run, plot and jump to log files."))
	    (run-at-time 1 nil 'fuse-display-startup-message))
	  (setq fuse-startup-displayed t))
    ))


;;; Run Hook ------------------------------------------------------------------

(provide 'input)
(run-hooks 'input-load-hook)

;;;============================================================================
;;;
;;; input.el ends here
