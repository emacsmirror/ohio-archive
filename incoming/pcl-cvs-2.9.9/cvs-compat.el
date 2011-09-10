;;; cvs-compat.el --- Compatibility functions for various Emacsen

;; Copyright (C) 1999-2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: compatibility
;; Version: v2_9_9
;; Revision: cvs-compat.el,v 1.3 2000/03/05 21:32:21 monnier Exp

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

;;; History:

;;; Code:

(require 'cl)

;;;;
;;;; String Processing
;;;;

;; doesn't exist in Emacs < 20.1
(eval-when-compile (autoload 'string-split "string"))
(unless (fboundp 'split-string)
  ;; define it in terms of elib's string's string-split
  (require 'string)
  (defun split-string (str &optional sep)
    ;; this is not quite correct since we should only eliminate
    ;; a leading and a trailing empty string, but it's enough for our use.
    (delete-if (lambda (str) (string= str "")) (string-split sep str))))

;; too bad it's only provided by dired rather than by some "neutral" lib.
(unless (fboundp 'string-replace-match)
  (require 'dired)
  (if (fboundp 'dired-string-replace-match)
      (defalias 'string-replace-match 'dired-string-replace-match)
    (require 'string)))

;;;;
;;;; Buffer management
;;;;

;; doesn't exist in Emacs < 20.1
(unless (fboundp 'save-current-buffer)
  (defmacro save-current-buffer (&rest body)
    (let ((sym (make-symbol "curbuf")))
      `(let ((,sym (current-buffer)))
	 (unwind-protect (progn ,@body) (set-buffer ,sym))))))

;; doesn't exist in Emacs < 20.1
(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buf &rest body)
    `(save-current-buffer (set-buffer ,buf) ,@body)))

;;;;
;;;; Keymaps
;;;;

;; doesn't exist in Emacs < 20.1
(unless (fboundp 'set-keymap-parent)
  (defun set-keymap-parent (keymap parent)
    (unless (and (consp keymap) (eq 'keymap (car keymap)))
      (error "Unknown keymap type"))
    (setf (cdr (last keymap)) parent)))

;; doesn't exist in Emacs
(unless (fboundp 'set-keymap-parents)
  (defun set-keymap-parents (m parents)
    (if (keymapp parents) (setq parents (list parents)))
    (set-keymap-parent
     m
     (if (cdr parents)
	 (reduce (lambda (m1 m2)
		   (let ((m (copy-keymap m1)))
		     (set-keymap-parent m m2) m))
		 parents
		 :from-end t)
       (car parents)))))

;;;; 
;;;; Custom
;;;; 

;; doesn't exist in Emacs < 20.1
(unless (fboundp 'set-face-bold-p)
  (defun set-face-bold-p (face v &optional f)
    (when v (ignore-errors (make-face-bold face)))))
(unless (fboundp 'set-face-italic-p)
  (defun set-face-italic-p (face v &optional f)
    (when v (ignore-errors (make-face-italic face)))))

;; doesn't exist in Emacs < 20.1
(ignore-errors (require 'custom))
(unless (fboundp 'defgroup)
  (defmacro defgroup (&rest rest) ()))
(unless (fboundp 'defcustom)
  (defmacro defcustom (sym val str &rest rest) `(defvar ,sym ,val ,str)))
(unless (fboundp 'defface)
  (defmacro defface (sym val str &rest rest)
    `(defvar ,sym (make-face ',sym) ,str)))

;;;;
;;;; missing functions in XEmacs
;;;;

;; doesn't exist in XEmacs
(unless (fboundp 'easy-mmode-define-minor-mode)
  (autoload 'easy-mmode-define-minor-mode "easy-mmode" nil nil 'macro))

;; doesn't exist in XEmacs
(eval-when-compile (autoload 'temp-directory ""))
(when (fboundp 'temp-directory)
  (defvar temporary-file-directory (temp-directory)))

;;;;
;;;; missing functions in Emacs
;;;;

;; doesn't exist in Emacs
(unless (fboundp 'read-directory-name)
  (defalias 'read-directory-name 'read-file-name))


(provide 'cvs-compat)
;;; cvs-compat.el ends here
