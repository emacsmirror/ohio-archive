;;; fuse-compile.el --- emacs lisp script for compiling the FUSE package

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  16 May 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: $

;; Copyright (C) 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file is used to compile and install FUSE
;; -- fuse-compile-files was inspired by dgnushack.el from the
;;    gnus package
;; -- the custom hack is taken from the custom web-page

;;; History:

;;; Bugs:

;;; Code:

(setq load-path (cons "." load-path))

(require 'cl)

(eval-when-compile (defvar imenu-generic-expression nil))

(eval-when-compile
  (condition-case ()
      (require 'hilit19)
    (error nil))
  (defun hilit-repaint-command (arg)
    arg)
  (defun hilit-set-mode-patterns (arg1 arg2 &optional arg3 arg4)
    (or arg1 arg2 arg3 arg4)))

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
	   ;; David Hughes 2nd April 1998
	   ;;(or (not speedbar-xemacsp) speedbar-xemacs20p))
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

(defvar fuse-xanes-p nil)

(defun fuse-compile-files ()
  (interactive)
  (let ((files '("comment.el"       "info-look.el"
		 "gnuplot.el"       "input.el"        "fuse.el"
		 "fuse-gnuplot.el"  "fuse-dired.el"   "fuse-doc.el"
		 "fuse-atoms.el"    "fuse-autobk.el"  "fuse-feff.el"
		 "fuse-feffit.el"   "fuse-normal.el"  "fuse-phit.el"
		 "fuse-diffkk.el"   "fuse-fluo.el"
		 "fuse-generic.el" "gnuplot-gui.el"))
	(xfiles '("fuse-toolbar.el" "fuse-icons.el"))
	;;(xanes '("fuse-xanes.el" "fuse-correct.el"))
	(xemacs (string-match "XEmacs" emacs-version))
	(byte-compile-verbose nil)
	(byte-compile-warnings nil) )
	;;(byte-compile-warnings
	;; '(free-vars unresolved callargs redefine obsolete)))
    (mapc 'fuse-compile-file files)     ; compile the essential files
    (if xemacs (mapc 'fuse-compile-file xfiles)) ; compile toolbar code
    ;;(if fuse-xanes-p (mapc 'fuse-compile-file xanes)) ; compile xanes+correct
    (fuse-compile-file "fuse-math.el")  ; compile math expressions code
    ))					; should I check for calc??

;; byte-compile-verbose	        Whether to report the function currently being
;;				compiled in the minibuffer;
;; byte-compile-warnings	List of warnings to issue, or t.  May contain
;;				'free-vars (references to variables not in the
;;					    current lexical scope)
;;				'unresolved (calls to unknown functions)
;;				'callargs  (lambda calls with args that don't
;;					    match the lambda's definition)
;;				'redefine  (function cell redefined from
;;					    a macro to a lambda or vice versa,
;;					    or redefined to take other args)
;;				'obsolete  (obsolete variables and functions)

(defun fuse-compile-file (file)
  (interactive)
  (let ((elc (concat file "c")))
    (cond ((file-newer-than-file-p elc file)
	   (message "%-15s is up to date" file))
	  ((file-newer-than-file-p file elc)
	   (if (or byte-compile-warnings byte-compile-verbose)
	       (message "\n%s Byte compiling %s:\n" (make-string 7 ?=) file))
	   (byte-compile-file file))
	  ((not (file-exists-p file))
	   (message "%-15s does not exist" file)))))


;;; fuse-compile.el ends here
