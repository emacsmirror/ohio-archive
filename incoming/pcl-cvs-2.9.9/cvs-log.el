;;; cvs-log.el --- Major mode for browsing CVS log output

;; Copyright (C) 1999-2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: pcl-cvs cvs log
;; Version: v2_9_9
;; Revision: cvs-log.el,v 1.2 2000/03/03 20:58:09 monnier Exp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Todo:

;; - Rename to rcs-log-mode
;; - extract version info in cvs-log-current-tag

;;; Code:

(eval-when-compile (require 'cl))
(require 'pcl-cvs-defs)
(require 'pcl-cvs-util)


(defgroup cvs-log nil
  "Major mode for browsing log output for PCL-CVS."
  :group 'pcl-cvs
  :prefix "cvs-log-")

(cvs-defmap cvs-log-mode-map
  '(("n" . cvs-log-next-message)
    ("N" . cvs-log-next-file)
    ("M-n" . cvs-log-next-file)
    ("p" . cvs-log-prev-message)
    ("P" . cvs-log-prev-file)
    ("M-p" . cvs-log-prev-file))
  "CVS-Log's keymap."
  :group 'cvs-log
  :inherit cvs-mode-map)

(defvar cv-log-mode-hook nil
  "Hook run at the end of `cvs-log-mode'.")

(defface cvs-log-file-face
  '((((class color) (background light))
     (:background "grey70" :bold t))
    (t (:bold t)))
  "Face for the file header line in `cvs-log-mode'."
  :group 'cvs-log)
(defvar cvs-log-file-face 'cvs-log-file-face)

(defface cvs-log-message-face
  '((((class color) (background light))
     (:background "grey85"))
    (t (:bold t)))
  "Face for the message header line in `cvs-log-mode'."
  :group 'cvs-log)
(defvar cvs-log-message-face 'cvs-log-message-face)

(defconst cvs-log-file-re "^Working file: \\(.+\\)\n")
(defconst cvs-log-message-re "^----------------------------$")

(defconst cvs-log-font-lock-keywords
  `((,cvs-log-file-re (1 'cvs-filename-face) (0 'cvs-log-file-face append))
    (,cvs-log-message-re . cvs-log-message-face)))
(defconst cvs-log-font-lock-defaults
  '(cvs-log-font-lock-keywords t nil nil nil))

;;;; 
;;;; Actual code
;;;; 

;;;###autoload
(autoload 'cvs-log-mode "cvs-log" "Major mode for browsing CVS log output." t)
(cvs-define-major-mode "CVS-Log"
  "Major mode for browsing CVS log output."
  (set (make-local-variable 'cvs-minor-wrap-function) 'cvs-log-minor-wrap))

;;;;
;;;; Navigation
;;;;

(defun cvs-log-next-message (&optional count)
  "Move to next (COUNT'th) log message."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (cvs-log-prev-message (- count))
    (when (looking-at cvs-log-message-re) (incf count))
    (re-search-forward cvs-log-message-re nil nil count)
    (goto-char (match-beginning 0))))

(defun cvs-log-next-file (&optional count)
  "Move to next (COUNT'th) file."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (cvs-log-prev-file (- count))
    (when (looking-at cvs-log-file-re) (incf count))
    (re-search-forward cvs-log-file-re nil nil count)
    (goto-char (match-beginning 0))))

(defun cvs-log-prev-message (&optional count)
  "Move to previous (COUNT'th) log message."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (cvs-log-next-message (- count))
    (re-search-backward cvs-log-message-re nil nil count)))

(defun cvs-log-prev-file (&optional count)
  "Move to previous (COUNT'th) file."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (cvs-log-next-file (- count))
    (re-search-backward cvs-log-file-re nil nil count)))

;;;;
;;;; Linkage to PCL-CVS (mostly copied from cvs-status.el)
;;;;

(defconst cvs-log-dir-re "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$")

(defun cvs-log-current-file ()
  (save-excursion
    (forward-line 1)
    (or (re-search-backward cvs-log-file-re nil t)
	(re-search-forward cvs-log-file-re))
    (let* ((file (match-string 1))
	   (cvsdir (and (re-search-backward cvs-log-dir-re nil t)
			(match-string 1)))
	   (pcldir (and (re-search-backward cvs-pcl-cvs-dirchange-re nil t)
			(match-string 1)))
	   (dir ""))
      (let ((default-directory ""))
	(when pcldir (setq dir (expand-file-name pcldir dir)))
	(when cvsdir (setq dir (expand-file-name cvsdir dir)))
	(expand-file-name file dir)))))

(defun cvs-log-current-tag ()
  nil);; FIXME

(defun cvs-log-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(cons
		 (cons (cvs-log-current-file)
		       (cvs-log-current-tag))
		 (when (ignore-errors (mark))
		   ;; `mark-active' is not provided by XEmacs :-(
		   (save-excursion
		     (goto-char (mark))
		     (cons (cvs-log-current-file)
			   (cvs-log-current-tag))))))))
    (let ((cvs-branch-prefix (cdar data))
	  (cvs-secondary-branch-prefix (and (cdar data) (cddr data)))
	  (cvs-minor-current-files
	   (cons (caar data)
		 (when (and (cadr data) (not (equal (caar data) (cadr data))))
		   (list (cadr data)))))
	  ;; FIXME:  I need to force because the fileinfos are UNKNOWN
	  (cvs-force-command "/F"))
      (funcall f))))

(provide 'cvs-log)
;;; cvs-log.el ends here
