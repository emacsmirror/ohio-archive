;;; tpu-recall.el --- gmhistification of TPU-edt

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: tpu-edt gmhist

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; TPU-edt History only applies to GNU emacs version 18!

;;  This file implements minibuffer history recall on strings, file names,
;;  and commands for TPU-edt.  The affected functions are:

;;       SEARCH and REPLACE   share a common string history
;;       GET and INCLUDE      share a common file name history
;;       ADD-AT-BOL           uses an independent string history
;;       ADD-AT-EOL           uses an independent string history

;;  Also, the DO key and Gold-KP7 combination are mapped to the gmhist
;;  function gmhist-execute-extended-command, which provides command
;;  recall from the M-x prompt.

;;; Usage:

;;  The gmhist package is required for activation of the history recall
;;  mechanism.  Gmhist is a generic minibuffer history package written by
;;  Sebastian Kremer.  The package is available for anonymous ftp in the
;;  USA from:

;;       ftp.cs.buffalo.edu:pub/Emacs/gmhist.tar.Z

;;  Or from Sebastion's own site in Germany:

;;       ftp.thp.uni-koeln.de:pub/gnu/emacs/gmhist.tar.Z

;;  If you experience timeouts with that host, try

;;       ftp.uni-koeln.de:pub/gnu/emacs/gmhist.tar.Z

;;  The necessary parts of gmhist also come with Sebastian's tree dired
;;  package, found in the file diredall.tar.Z on the same ftp servers.
;;  If you already have tree dired, you don't need to fetch gmhist.

;;  Make sure you have an up to date version.  TPU-edt history recall
;;  was developed using the following versions of the gmhist files.

;;       gmhist.el      Revision 4.27  1992/04/20 17:17:47  sk RelBeta
;;       gmhist-app.el  Revision 4.16  1992/02/26 14:32:27  sk RelBeta

;;  Once you have the gmhist package, make sure that this file and the
;;  gmhist*.el(c) files are in your load path.  Then modify your .emacs
;;  file to load this file after TPU-edt has been loaded.  For example:

;;       (load "tpu-edt")                   ; Load the base TPU-edt,
;;       (load "tpu-recall")                ;   and add history recall.

;;  If you're already using gmhist, just load this file after your own
;;  gmhist setup or in your gmhist-load-hook (note that this file loads
;;  gmhist-app).  It will make the modifications to TPU-edt without
;;  molesting your customizations.

;;  Finally, note that it is safe to load this file even if gmhist is not
;;  available.  Of course, no minibuffer history functions will be enabled.

;;; Known Problems:

;;  Some TPU-edt commands contain spaces -- "what line" and "set scroll
;;  margins" for example.  The gmhist routine that reads line mode commands
;;  does not work with such commands.  The work-around is to use the emacs
;;  version of the command.  Each TPU-edt command that contains spaces has
;;  an alias of the form tpu-<command with spaces replaced by dashes>.  For
;;  example, "what line" can be invoked with the command tpu-what-line.

;;; Code:


;;; TPU-edt History only applies to GNU emacs version 18!

(cond
 ((not tpu-emacs19-p)

  ;;  Revision Information

  (defconst tpu-recall-revision "$Revision: 2.5 $"
    "Revision number of TPU-edt histification.")


  ;; Global variables

  (defvar tpu-gmhistified nil
    "If non-nil, TPU-edt has been gmhistified.")


  ;; Replace the regular expression and string prompt function.

  (defun tpu-regexp-prompt (prompt)
    "Read a string, adding 'RE' to the prompt if tpu-regexp-p is set."
    (if (featurep 'gmhist)
	(read-with-history-in 'tpu-regexp-prompt-hist
			      (concat (if tpu-regexp-p "RE ") prompt))
      (read-string (concat (if tpu-regexp-p "RE ") prompt))))

  (defun tpu-string-prompt (prompt history-symbol)
    "Read a string with PROMPT and history HISTORY-SYMBOL."
    (if (featurep 'gmhist)
	(read-with-history-in history-symbol prompt)
      (read-string prompt)))


  ;; Commands to run if gmhist is available, and after it is loaded.

  (defun tpu-gmhist-hook nil
    "Set up gmhist history symbols and key mappings."

    ;; Indicate that we've tried to gmhistify.
    (setq tpu-gmhistified t)

    ;; prompt for string searches and replacements
    (put 'tpu-regexp-prompt-hist 'no-default t)
    (put 'tpu-regexp-prompt-hist 'cursor-end t)

    ;; prompt for adding strings at bol
    (put 'tpu-add-at-bol-hist 'no-default t)
    (put 'tpu-add-at-bol-hist 'cursor-end t)

    ;; prompt for adding strings at eol
    (put 'tpu-add-at-eol-hist 'no-default t)
    (put 'tpu-add-at-eol-hist 'cursor-end t)

    ;; prompt for file names to include or get
    (gmhist-make-magic 'tpu-include 'tpu-file-name-hist)
    (gmhist-make-magic 'tpu-get 'tpu-file-name-hist)
    (put 'tpu-file-name-hist 'no-default t)
    (put 'tpu-file-name-hist 'cursor-end t)

    ;; file name hack for VMS versions of GNU emacs.
    (and (string= (symbol-value 'system-type) "vax-vms")
	 (fset 'gmhist-quote-dollars (symbol-function 'identity)))

    ;; Make up and down arrows recall commands and strings

    (let ((loc (where-is-internal 'tpu-previous-line)) (cur nil))
      (while (setq cur (car loc))
	(define-key gmhist-map cur 'gmhist-previous)
	(define-key gmhist-completion-map cur 'gmhist-previous)
	(define-key gmhist-must-match-map cur 'gmhist-previous)
	(setq loc (cdr loc)))

      (setq loc (where-is-internal 'tpu-next-line))
      (while (setq cur (car loc))
	(define-key gmhist-map cur 'gmhist-next)
	(define-key gmhist-completion-map cur 'gmhist-next)
	(define-key gmhist-must-match-map cur 'gmhist-next)
	(setq loc (cdr loc))))

    ;; If gmhist-app is available, make up and down arrows recall
    ;; commands.	While we're at it, try to leave M-x alone.

    (cond ((load "gmhist-app" t t)
	   (let ((loc (where-is-internal 'execute-extended-command)) (cur nil))
	     (while (setq cur (car loc))
	       (if (not (equal cur "\ex"))
		   (global-set-key cur 'gmhist-execute-extended-command))
	       (setq loc (cdr loc)))))))


  ;; If gmhist is already loaded, run the TPU-edt gmhist setup function,
  ;; otherwise, attempt to load it, and if that suceeds, run the TPU-edt
  ;; gmhist setup function.  Either way, don't complain if gmhist isn't
  ;; available.

  (defun tpu-gmhistify nil
    "gmhistify TPU-edt functions."
    (interactive)
    (if (featurep 'gmhist) (tpu-gmhist-hook)
      (if (load "gmhist" t t) (tpu-gmhist-hook))))

  ;; gmhistify if we're not already
  (if (not tpu-gmhistified) (tpu-gmhistify))))

;;; tpu-recall.el ends here
