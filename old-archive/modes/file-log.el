;;; file-log.el --- Manage a log file per file for version control.
;;
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: file-log.el,v 1.7 1997/03/02 21:28:37 fred Exp $
;; Keywords: maint vc cvs
;;
;; LCD Archive Entry:
;; file-log|Frederic Lepied|Frederic.Lepied@sugix.frmug.org|
;; Manage a log file per file for version control.|
;; 02-Mar-1997|1.7|~/modes/file-log.el.gz|
;; 
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; These simple functions  let you add  log entries for files you edit
;; as soon as you want and then insert your log entries in the comment
;; buffer  when you commit your  changes via the version control tools
;; (vc or cvs).
;;
;; The entries are  stored in files with a  .flog extension. ie. for a
;; file called  foo.bar the entries  will be in foo.bar.flog. When the
;; commit is done the corresponding files are deleted.
;;
;; To install add this line to your .emacs or to the
;; site-start.el file :
;;
;; (require 'file-log)
;;
;; You can bind the command using the following declarations for example :
;;
;; (define-key ctl-x-map "l" 'flog-add-entry)
;; (define-key ctl-x-4-map "l" 'flog-add-entry-other-window)
;; (define-key ctl-x-5-map "l" 'flog-add-entry-other-frame)
;;
;; and to add an entry in the Tools menu :
;;
;; (define-key menu-bar-tools-menu [separator-print]
;;   '("--"))
;; (define-key menu-bar-tools-menu [flog-add-entry]
;;  '("Add file log entry" . flog-add-entry))
;;
;; To insert an entry use the following functions :
;;	o flog-add-entry [C-x l]
;;	o flog-add-entry-other-window [C-x 4 l]
;;	o flog-add-entry-other-frame [C-x 5 l]

;;; Dependencies

(require 'add-log)

;;; Variables:

(defvar flog-entry "* "
  "*Line inserted when `flog-add-entry' is called.")

(defvar flog-mode-name "Flog"
  "*File log mode name displayed in mode line. See `flog-mode'.")

(defvar flog-ext ".flog"
  "*Suffix used to create a log associated with a file. See `flog-mode'.")

(setq auto-mode-alist (cons (cons (concat (regexp-quote flog-ext) "$")
				  'flog-mode)
			    auto-mode-alist))

(defvar flog-left-margin 8
  "*Column for the indentation in `flog-mode'.")

(defvar flog-fill-column 74
  "*Column for the right indentation in `flog-mode'.")

(defvar flog-auto-fill t
  "*Non nil turns on `auto-fill-mode' when entering `flog-mode'.")

(defvar flog-id "$Id: file-log.el,v 1.7 1997/03/02 21:28:37 fred Exp $"
  "Version control tag for the File Log package.")

(defvar flog-font-lock-keywords
  '(("(\\([^)\n]+\\)):" 1 font-lock-keyword-face))	; Function name.
  "Additional expressions to highlight in File Log mode.")

(defvar flog-switch-variant nil nil)
(make-variable-buffer-local 'flog-switch-variant)

(defvar flog-mode-map nil
  "Keymap for File Log major mode.")
(if flog-mode-map
    nil
  (setq flog-mode-map (make-sparse-keymap))
  (define-key flog-mode-map "\C-c\C-c" 'flog-save-and-bury))

;;; Code:

(defun flog-add-entry-other-window ()
  "Edit a log file associated with the current file in an other window.
The log file name is deduced from the file name by adding a suffix
from the value of `flog-ext'.

  At each call a newline is inserted at the beginning of the buffer
and the value of `flog-entry' is inserted.

  When you have finished editing your log, you can hit \\[flog-save-and-bury]
to save your log.."
  (interactive)
  (flog 'find-file-other-window))

(defun flog-add-entry ()
  "Edit a log file associated with the current file.
The log file name is deduced from the file name by adding a suffix
from the value of `flog-ext'.

  At each call a newline is inserted at the beginning of the buffer
and the value of `flog-entry' is inserted.

  When you have finished editing your log, you can hit \\[flog-save-and-bury]
to save your log.."
  (interactive)
  (flog 'find-file))

(defun flog-add-entry-other-frame ()
  "Edit a log file associated with the current file in an other frame.
The log file name is deduced from the file name by adding a suffix
from the value of `flog-ext'.

  At each call a newline is inserted at the beginning of the buffer
and the value of `flog-entry' is inserted.

  When you have finished editing your log, you can hit \\[flog-save-and-bury]
to save your log.."
  (interactive)
  (flog 'find-file-other-frame))

(defun flog (symbol)
  (if (not buffer-file-name)
      (error "Not visiting a file...")
    (let ((filename	(concat buffer-file-name flog-ext))
	  (func-name	(funcall (or add-log-current-defun-function
				     'add-log-current-defun))))
      (apply symbol (list filename))
      (flog-mode)
      (setq flog-switch-variant symbol)
      (goto-char (point-min))
      (if (= (point-min) (point-max))
	  (insert (make-string left-margin ? ))
	  (open-line 1))
      (insert flog-entry)
      (if func-name (insert "(" func-name "): "))
      (message "Type C-c C-c when done.")
      )))
  
(defun flog-mode ()
  "Major mode for editing file logs; like Indented Text Mode.
Prevents backups and sets `left-margin' to 8 and `fill-column' to 74.
Save and back to the associated buffer using \\[flog-save-and-bury].

This mode begins turns `auto-fill-mode' on and then runs `flog-mode-hook'."
  (interactive)
  (let ((change-log-mode-hook	nil))
    (change-log-mode))
  (if flog-auto-fill
      (auto-fill-mode 1))
  (setq major-mode 'flog-mode
	mode-name flog-mode-name
	left-margin flog-left-margin
	fill-column flog-fill-column)
  ;; suppress backups
  (set (make-local-variable 'make-backup-files) nil)
  (use-local-map flog-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(flog-font-lock-keywords t))
  (run-hooks 'flog-mode-hook))

(defun flog-save-and-bury ()
  (interactive)
  (let* ((variant flog-switch-variant)
	 (buf (current-buffer))
	 (win (get-buffer-window buf)))
    (save-buffer)
    (if (window-dedicated-p win)
	(progn
	  (delete-windows-on buf t)
	  (bury-buffer buf))
      (cond ((eq variant 'find-file-other-window)
	     (bury-buffer)
	     (other-window 1))
	    (t (bury-buffer)) ) ) ) )

(defun flog-delete-log (file)
  (let ((name (concat file flog-ext)))
    (if (file-exists-p name)
	(let ((buf (get-file-buffer name)))
	  (if buf
	      (kill-buffer buf))
	  (delete-file name)))))

;;=============================================================================
;; cvs support
;;=============================================================================
(add-hook 'cvs-commit-hooks (function flog-commit-hook))
(add-hook 'cvs-before-commit-hooks (function flog-before-commit-hook))

(defun flog-commit-hook ()
  (mapcar (function
	   (lambda(f)
	     (let ((name (concat f flog-ext)))
	       (if (file-exists-p name)
		   (insert-file name)))))
	  cvs:commit-list))

(defun flog-before-commit-hook ()
  (mapcar (function flog-delete-log) cvs:commit-list))

;;=============================================================================
;; vc support
;;=============================================================================
(add-hook 'vc-log-mode-hook 'flog-vc-log)

(defun flog-vc-log ()
  (let ((name (concat file flog-ext)))	; I should refer to vc-file-log but it
    (message "%S" name)			; is defined after the hook call.
    (if (file-exists-p name)
	(insert-file name))))

(defadvice vc-finish-logentry (after flog-after-finish-logentry activate)
  (flog-delete-log flog-current-log-file))

(defadvice vc-finish-logentry (before flog-before-finish-logentry activate)
  (setq flog-current-log-file vc-log-file))

;; For the new version of vc with vc-finish-logentry-hook hook.
;;
;; (add-hook 'vc-log-mode-hook (function flog-vc-log))
;; (add-hook 'vc-finish-logentry-hook (function flog-vc-log-commit))
;; 
;; (defun flog-vc-log ()
;;   (let ((name (concat vc-log-file flog-ext)))
;;     (message "%S" name)
;;     (if (file-exists-p name)
;; 	(insert-file name))))
;; 
;; (defun flog-vc-log-commit ()
;;   (flog-delete-log log-file))

(provide 'file-log)

;;; file-log.el ends here
