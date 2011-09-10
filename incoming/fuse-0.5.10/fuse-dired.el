;;; fuse-dired.el --- FUSE batch process hook for dired

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  18 March 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit, dired

;; $Id:$

;; Copyright (C) 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file allows batch processing of FEFF and UWXAFS programs.
;; Mark the input files in a dired buffer.  `fuse-dired-run-marked'
;; reads the list of marked files and runs the appropriate program on
;; each of them.
;;
;; Put this in your .emacs file to enable batch processing of FUSE
;; files from a dired buffer
;;
;; (add-hook 'dired-load-hook '(lambda () (load-library "fuse-dired")))

;;; Code:

(eval-when-compile (require 'dired) (require 'input))
(require 'cl)

;; These have different names in FSF 19.34 and XE 19.14.  Yeesh!
(unless (fboundp 'dired-get-marked-files)
  (fset 'dired-get-marked-files 'dired-mark-get-files))

(define-key dired-mode-map "\C-cr" 'fuse-dired-run-marked)

(defun fuse-dired-run-marked ()
  "Batch process input files in a dired buffer.
Run the appropriate Feff or UWXAFS program on each of the marked
files.  They will be run in the same order that they are displayed in
the dired buffer."
  (interactive)
  (require 'input)
  (cond (input-batch-flag
	 (message "There is a batch job currently running."))
	((string-match "Dired" mode-name)
	 (let ((buffer (current-buffer)))
	   (setq input-batch-files (dired-get-marked-files)
		 input-batch-flag t
		 input-started-by-batch-flag t)
	   (set-buffer (find-file-noselect (elt input-batch-files 0)))
	   (input-run-this-program-this-file)
	   (set-buffer buffer)))
	(t
	 (message "FUSE batch runs must be started from a dired buffer."))))

;;; fuse-dired.el ends here
