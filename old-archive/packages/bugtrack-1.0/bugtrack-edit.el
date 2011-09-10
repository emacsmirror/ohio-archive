;;; bugtrack-edit.el --- Edit bug reports.

;; Copyright (C) 1992, 1993 Per Cederqvist.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;; Created: 1992-07-29
;; Version: Id: bugtrack-edit.el,v 1.7 1993/12/13 20:08:15 ceder Exp 
;; Keywords: tools, bugtracking, rolodex, bug, track

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
     
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See bugtrack.el and the doc strings.  This file requires cookie.el
;; from Elib.  The commentary in bugtrack.el tells you where to get
;; that file.


;;; Code:

(defvar bugtrack-saved-headers nil
  "Contents of the headers before edit started. Includes the delimiting line.")

(defvar bugtrack-saved-text nil
  "Contents of the free text before edit started.")

(defvar bugtrack-edit-mode-map nil
  "Map for editing the full text of Bugtrack bug messages.")

(if bugtrack-edit-mode-map
    nil
  (setq bugtrack-edit-mode-map (make-sparse-keymap))
  (define-key bugtrack-edit-mode-map "\C-c\C-c" 'bugtrack-edit-done)
  (define-key bugtrack-edit-mode-map "\C-c\C-]" 'bugtrack-edit-abort)
  (define-key bugtrack-edit-mode-map "\C-xn" nil) ;Avoid restriction messes.
  (define-key bugtrack-edit-mode-map "\C-xp" nil)
  (define-key bugtrack-edit-mode-map "\C-xw" nil))

;;;###autoload
(defun bugtrack-edit-mode ()
  "///Document///"
  (use-local-map bugtrack-edit-mode-map)
  (setq mode-name "Bugtrack edit")
  (setq major-mode 'bugtrack-edit-mode)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start (concat "^---$\\|" paragraph-start))
  (setq paragraph-separate (concat "^---$\\|" paragraph-separate))
  (run-hooks 'text-mode-hook 'bugtrack-edit-mode-hook))

;;;###autoload
(defun bugtrack-edit ()
  "In Bugtrack mode, start to edit the free text."
  (interactive)
  (make-local-variable 'bugtrack-saved-headers)
  (make-local-variable 'bugtrack-saved-text)
  (save-excursion
    (setq bugtrack-saved-headers
	  (buffer-substring (point-min) (bugtrack-goto-free-text-start)))
    (setq bugtrack-saved-text (buffer-substring (point) (point-max))))
  (bugtrack-edit-mode)
  (setq buffer-read-only nil)
  (set-buffer-modified-p (buffer-modified-p)) ;Update modeline.
  (goto-char (max (point) (bugtrack-free-text-start)))
  (message (substitute-command-keys
	    "Press \\[bugtrack-edit-done] to return to Bugtrack, \
\\[bugtrack-edit-abort] to abort.")))

(defun bugtrack-edit-abort ()
  "Abort Bugtrack edit mode."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert bugtrack-saved-headers bugtrack-saved-text)
  (goto-char (point-min))
  (bugtrack-mode))

(defun bugtrack-edit-done ()
  "End Bugtrack edit mode, and remember your changes."
  (interactive)
  (cond
   ((and (not (string=
	       bugtrack-saved-headers
	       (buffer-substring (point-min) (bugtrack-free-text-start))))
	 (not (prog1
		  (y-or-n-p "Headers have changed. Remember the changes? ")
		(message ""))))
    (delete-region (point-min) (bugtrack-free-text-start))
    (goto-char (point-min))
    (insert bugtrack-saved-headers)))
  (save-excursion
    (goto-char (point-max))
    (if (not (bolp))
	(insert "\n")))
  (bugtrack-mode)
  (bugtrack-update-last-edit))

;;; Local Variables:
;;; generated-autoload-file: "bugtrack-init.el"
;;; End:

;;; bugtrack-edit.el ends here
