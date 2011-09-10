;; auto-diff.el -- Automatically generate diff's for directory contents.

;; Copyright (C) 1999 by Free Software Foundation, Inc.
;; Author: Steve Kemp <stevek@epc.co.uk>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;;  This package is aimed at making it simple for programmers to
;; create diff's of files that they have been working on.
;;
;;  When a file is loaded that matchs "auto-diff-directory-regexp"
;; a diff will be automatically created whenever the user saves
;; the file.
;;

;;; Usage
;;
;;  Simply add the following lines to your .emacs file:
;;
;;  ;; Generate diffs in a simple manner.
;;  (require 'auto-diff)
;;
;;  ;; example only...
;;  (add-to-list 'auto-diff-directory-regexp "/home/skx/Lisp/.*\.el")

;;; History:
;; 
;;  Version 0.1 -- Initial version.
;;  Version 0.2 -- As posted to gnu.emacs.sources
;;
;;  [ Updates available from http://GNUSoftware.com/Emacs/Lisp/ ]

;;; Code:

(defgroup auto-diff nil
  "A global function to generate diff's automatically in multiple,
specified directorys.  See 'auto-diff-directory-regexp'"
  :group 'files)

(defconst auto-diff-version "0.2"
  "The version of this file")

(defcustom auto-diff-directory-regexp nil
  "A list of regexps, all matching directories will have diffs
created when the user edits files in that directory"
  :type '(list (string :tag "Regexp"))
)

(defcustom auto-diff-program "diff"
  "The program to use to create the diff."
  :type 'string)

(defcustom auto-diff-arguments "--context"
  "The arguments to use when creating the diffs."
  :type 'string)

(defvar auto-diff-filename nil
  "The name of the current file.")

;;;###autoload
(defun auto-diff-customize ()
  "Customize auto-diff."
  (interactive)
  (customize-group "auto-diff"))


(defun auto-diff-on (filename)
  "Setup auto-diff to work on the currently loaded file."
  (make-variable-buffer-local 'kill-buffer-hook)
  (make-variable-buffer-local 'after-save-hook)
  (make-variable-buffer-local 'kill-emacs-hook)
  (make-variable-buffer-local 'auto-diff-filename)

  (add-hook 'after-save-hook 'auto-diff-write-hook)
  (add-hook 'kill-buffer-hook 'auto-diff-kill-buffer-hook)
  (add-hook 'kill-emacs-hook 'auto-diff-kill-buffer-hook)
  (setq auto-diff-filename filename)
  
  ;; Copy file to a backup one, this is so that we have a baseline
  ;; for generating the diffs from
  (if (file-exists-p filename)
      (copy-file filename (auto-diff-temp-name filename))
    (with-temp-buffer (write-file  (auto-diff-temp-name filename))))

  (message "auto-diff : Installed")
)

(defun auto-diff-off ()
  "Disable auto-diff on the currently loaded file."

  (remove-hook 'after-save-hook 'auto-diff-write-hook)
  (remove-hook 'kill-buffer-hook 'auto-diff-kill-buffer-hook)

  ;; Delete the baseline file that we created when we turned
  ;; auto-diff mode on.
  (let ((filename nil))
    (setq filename (buffer-file-name))
    (setq filename (auto-diff-temp-name filename))
    (if (file-exists-p filename)
	(delete-file filename)))

  (message "auto-diff : Removed")
)

(defun auto-diff-kill-buffer-hook ()
  "Called when the current buffer is killed.
Here we create the diff, if it is needed"
  (interactive)
  (auto-diff-off)
  t)

(defun auto-diff-write-hook ()
  "Called when the current buffer is saved.
Here we create the diff, if it is needed."
  (message "Creating Diff")

  (let ((process nil)
	(buffer nil))
    (setq buffer (get-buffer-create "*auto-dif*"))
    (setq process
	  (start-process "process" 
			 buffer 
			 auto-diff-program 
			 auto-diff-arguments
		         (auto-diff-temp-name auto-diff-filename)
			 auto-diff-filename))
    (set-process-sentinel process 'auto-diff-sentinel))
  nil
  )

(defun auto-diff-sentinel (process msg)
  "Called when the diff process has finished.
 Switch to its output buffer, and save it appropriately."
  (cond ((eq (process-status process) 'exit)
	 (progn
	   (message "Process finished %s" (process-command process))
	   (let ((buffer (current-buffer))
		 (file auto-diff-filename))
	     (switch-to-buffer "*auto-dif*")
	     (make-local-variable 'auto-diff-filename)
	     (setq auto-diff-filename file)
	     (write-file (concat auto-diff-filename ".diff"))
	     (kill-buffer (current-buffer))
	     (switch-to-buffer buffer))))))
 
(defun auto-diff-find-file-hook ()
  "Setup auto-diff, to generate diffs on files that
are contained in directories matching
'auto-diff-directory-regexp'"
  (let ((filename    nil)
	(regexp      nil)
	(regexp-list nil))

    (setq filename (expand-file-name (buffer-file-name)))
    (setq regexp-list auto-diff-directory-regexp)

    (while regexp-list
      (setq regexp (car regexp-list))
      (setq regexp-list (cdr regexp-list))
      (if (string-match regexp filename)
	  (auto-diff-on filename)))
    ))

(defun auto-diff-temp-name (filename)
  "Generate the temporary filenames that we use"
  (concat filename ".auto-diff-temp"))

; find-file-hooks
(add-hook 'find-file-hooks 'auto-diff-find-file-hook)

(provide 'auto-diff)

;;; auto-diff.el ends here
