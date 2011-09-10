;;; fuse.el --- the FEFF/UWXAFS System for Emacs

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  30 January 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: fuse.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

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

;;; History:
;;  current package version            comment
;; -----------------------------------------------------------------
;;  0.4.3     first stab: keyboard + menu + mouse interaction with
;;            commands; define commands for editing, documentation,
;;            configuration, bug report, quit, exit;  faces;
;;            toolbar for XEmacs (BR Jan 31 1998)

;;; Commentary:
;;   here is a good way to start fuse:
;;     alias fuse 'emacs -f fuse-mode -name FUSE -T FUSE'

;;; Bugs: (well...known bugs)

;;; Code:

(require 'input)

(defvar fuse-mode-hook nil
  "*Hooks run when FUSE is loaded.")

;; a gross hack to be sure that the fuse buffor gets pretty colors
(cond ((and (featurep 'custom) (fboundp 'custom-declare-variable))
       (defface fuse-highlight-face '((((class color) (background light))
				       (:foreground "black"
				      	:background "green"))
				      (((class color) (background dark))
				       (:foreground "black"
				        :background "green")))
	 "Face used to highlight fuse-mode commands."
	 :group 'fuse-programs)
       (defface fuse-title-face     '((((class color) (background light))
				       (:foreground "blue"))
				      (((class color) (background dark))
				       (:foreground "cadetblue")))
	 "Face used for FUSE titles."
	 :group 'fuse-programs)
       (defface fuse-command-face   '((((class color) (background light))
				       (:foreground "firebrick3"))
				      (((class color) (background dark))
				       (:foreground "pink")))
	 "Face used for FUSE command keys."
	 :group 'fuse-programs))
      (t
       (copy-face 'highlight 'fuse-highlight-face)
       (set-face-background 'fuse-highlight-face "green")

       (copy-face 'bold 'fuse-title-face)
       (set-face-foreground 'fuse-title-face "blue")

       (copy-face 'bold 'fuse-command-face)
       (set-face-foreground 'fuse-command-face "firebrick3")))

(defvar fuse-mode-map nil
  "Local keymap for FUSE-mode buffer.")
(if fuse-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'fuse-under-point)
    (define-key map "c"    'input-customize)
    (define-key map "d"    'input-fuse-document)
    (define-key map "e"    'fuse-find-file)
    (define-key map "h"    'fuse-mini-help)
    (define-key map "p"    'input-document)
    (define-key map "q"    'fuse-quit)
    (define-key map "s"    'input-submit-feedback)
    (define-key map "t"    'fuse-tutorial)
    (define-key map "v"    'input-show-version)
    (define-key map "x"    'save-buffers-kill-emacs)
    (cond ((string-match "XEmacs" emacs-version)
	   (define-key map '(button2) 'fuse-under-mouse))
	  (t
	   (define-key map [mouse-2] 'fuse-under-mouse)))
    (setq fuse-mode-map map)))



(defvar fuse-mode-menu nil)
(easy-menu-define
 fuse-mode-menu fuse-mode-map
 "Menu used in FUSE"
 '("FUSE"
   ["Open an input file"          'fuse-find-file t]
   ["Read FUSE document"          'input-fuse-document t]
   ["Read program documentation"  'input-document t]
   ["Read FUSE tutorial"          'fuse-tutorial t]
   ["Customize FUSE"              'input-customize :active (featurep 'custom)]
   ["Submit a FUSE bug report"    'input-submit-feedback t]
   ["A bit of help with Emacs"    'fuse-mini-help t]
   ["Quit FUSE"                   'fuse-quit t]
   ["Exit Emacs"                  'save-buffers-kill-emacs t]
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for fuse-mode

(defun fuse-quit ()
  "Quit FUSE."
  (interactive) (kill-buffer (current-buffer)))

(defun fuse-find-file ()
  "Prompt for an input filename."
  (interactive) (find-file (read-file-name "Name of input file > ")))

(defun fuse-under-mouse (event)
  "Fetch the command under the mouse from the FUSE screen display.
EVENT is a mouse event."
  (interactive "@e")
  (mouse-set-point event)
  (fuse-under-point)    )

(defun fuse-under-point ()
  "Fetch the command under point from the FUSE screen display."
  (interactive)
  (let (start letter (orig (point-marker)))
    (back-to-indentation)
    (cond ((looking-at "`")
	   (forward-char 1)
	   (setq start (point-marker))
	   (forward-char 1)
	   (setq letter (buffer-substring-no-properties start (point)))
	   (forward-char 1)
	   (funcall (key-binding letter)))
	  (t
	   (goto-char orig)))    ))

(defun fuse-identify-line (pt)
  "Identify the current line.
Return the completion to the sentence \"Mouse-2 to \".  PT is the
current value of point."
  (interactive)
  (let (match (alist '(("c" . "customize FUSE")
		       ("d" . "read the FUSE document")
		       ("e" . "edit a new input file")
		       ("h" . "get a bit of help")
		       ("p" . "read program documentation")
		       ("q" . "quit FUSE")
		       ("s" . "submit a bug report about FUSE by email")
		       ("t" . "read the FUSE tutorial")
		       ("x" . "exit Emacs") )))
    (save-excursion
      (goto-char pt)
      (back-to-indentation)
      (when (looking-at "`\\([cdehpqstx]\\)")
	(cdr (assoc (match-string 1) alist))))))

(defun fuse-mini-help ()
  "Write a short message to the echo area for the Emacs challenged."
  (interactive)
  (message (substitute-command-keys
	    "C-x C-c to exit Emacs, \\[keyboard-quit] to abort anything, \\[delete-other-windows] for one window")))

(defun fuse-tutorial ()
  "Look at on-line tutorial for *FUSE* using info or the w3 package."
  (interactive)  (input-document "fuse-tutorial"))


(defun fuse-set-properties ()
  "Set properties on the text in the FUSE screen display."
  (save-excursion
    (let (start extent1 extent2)
      (goto-char (point-min))
      (search-forward ">" (point-max) t)
      (set-text-properties (point-min) (point) '(face fuse-title-face))
      (while (not (eobp))
	(cond ((search-forward "`" (point-max) "to-limit")
	       (back-to-indentation)
	       (setq start (point-marker))
	       (forward-char 3)
	       (cond ((string-match "XEmacs" emacs-version)
		      (setq extent1 (make-extent start (point-marker)))
		      (set-extent-endpoints extent1 start (point-marker))
		      (set-extent-property  extent1 'face 'fuse-command-face))
		     (t
		      (set-text-properties start (point)
					   '(face fuse-command-face))))
	       (end-of-line)
	       (cond ((string-match "XEmacs" emacs-version)
		      (setq extent2 (make-extent start (point-marker)))
		      (set-extent-endpoints extent2 start (point-marker))
		      (set-extent-property extent2 'mouse-face
					   'fuse-highlight-face)
		      (set-extent-property extent2 'pointer
					   'toolbar-pointer-glyph)
		      (set-extent-property extent2 'balloon-help
					   (concat "Mouse-2 to "
						   (fuse-identify-line (point)))))
		     (t
		      (add-text-properties start (point)
				    '(mouse-face fuse-highlight-face))))
	       ))))))

;;(defvar fuse-toolbar-location "left")

(defun fuse-display ()
  "Generate the text of the FUSE screen display."
  (let (display)
    (setq display
	  (concat
	   "Welcome to FUSE\n\n"
	   "FUSE is the FEFF/UWXAFS System for Emacs\n"
	   "version " input-mode-version " by " input-author
	   " <" input-author-email ">\n\n"
	   "You may begin by typing one of the one-letter commands listed\n"
	   "below, by hitting the middle mouse button on one of the commands\n"
	   "listed below, or (in XEmacs) hitting the left mouse button on\n"
	   "the toolbar.\n\n"

	   "Users of \"vi\", \"crisp\", and \"edt\": \n"
	   "Emacs can be made to emulate these editors.  See the FUSE \n"
	   "document for details on customizing FUSE to emulate the editor\n"
	   "of your choice.\n\n"

	   "`e' = Edit an input file\n"
	   "`d' = Read the FUSE document\n"
	   "`p' = Read program documentation\n"
	   "`t' = Read the FUSE tutorial\n"
	   "`s' = Submit a bug report about FUSE\n"
	   "`h' = Don't panic!\n"
	   ))
    (if (featurep 'custom)
	(setq display (concat display "`c' = Customize FUSE\n")))
    (setq display
	  (concat
	   display
	   "----------------------------------------\n"
	   "`q' = Quit FUSE\n"
	   "`x' = Kill Emacs\n"))
    display    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for fuse-mode

(defvar fuse-mode nil
  "Determines if fuse minor mode is active.")
(make-variable-buffer-local 'fuse-mode)

;;;###autoload
(defun fuse-mode ()
  "Define the mode of the preliminary interaction screen in FUSE.
This screen is used a launching point for editing input files
using the FUSE package.

Defined keys in FUSE:\n\\{fuse-mode-map}"
  (interactive)
  (switch-to-buffer ">> FUSE <<")
  (delete-other-windows)
  (if fuse-mode nil
    (fundamental-mode)
    (kill-all-local-variables)
    (require 'input)
    (use-local-map fuse-mode-map)
    (easy-menu-add fuse-mode-menu fuse-mode-map)
    (insert (fuse-display))
    (goto-char (point-min))
    (indent-region (point-min) (point-max) 7)
    (fuse-set-properties)
    ;;(auto-fill-mode -1)
    (if abbrev-mode (abbrev-mode -1))
    (setq fuse-mode t
	  major-mode 'fuse-mode
	  mode-name "FUSE"
	  buffer-read-only t)
    (and (string-match "XEmacs" emacs-version)
	 (require 'fuse-toolbar))
    (input-read-init-file)
    (run-hooks 'fuse-mode-hook)  ))

;;; Run Hook ------------------------------------------------------------------

(provide 'fuse)
(run-hooks 'fuse-load-hook)

;;============================================================================
;;
;; fuse.el ends here
