;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         dos-mode.el
;; RCS:          $Header: dos-mode.el,v 1.10 92/11/26 12:07:55 ange Exp $
;; Description:  MSDOS minor mode for GNU Emacs
;; Author:       Andy Norman, Hewlett-Packard Labs, Bristol, England.
;; Created:      Wed May  6 16:42:29 1992
;; Modified:     Thu Nov 26 12:07:10 1992 (Andy Norman) ange@hplb.hpl.hp.com
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1992  Andy Norman.
;;;
;;; Author: Andy Norman (ange@hplb.hpl.hp.com)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to ange@hplb.hpl.hp.com) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; Description:
;;;
;;; This package does two things:
;;;
;;; If a file being read into a buffer has the ^M character at the end of each
;;; line then these extra characters are discarded from the file's buffer.
;;; The trailing ^Z, if present, is discarded too.  The buffer is now put into
;;; the dos-mode minor-mode.
;;;
;;; Secondly, when a buffer in dos-mode is being written out, the ^M and
;;; optional ^Z characters are replaced before the write happens.
;;;
;;; dos-mode can be toggled for a buffer with M-x dos-mode.
 
;;; Installation:
;;;
;;; Byte-compile dos-mode.el to dos-mode.elc and put them both in a directory
;;; on your load-path.  Load the package from your .emacs file with:
;;;
;;;   (require 'dos-mode)
;;;
;;; dos-mode can't sensibly be auto-loaded; you are either using it, or you
;;; aren't.

;;; LISPDIR ENTRY for the Elisp Archive
;;; 
;;;    LCD Archive Entry:
;;;    dos-mode|Andy Norman|ange@hplb.hpl.hp.com
;;;    |MSDOS minor mode for GNU Emacs
;;;    |$Date: 92/11/26 12:07:55 $|$Revision: 1.10 $|

(provide 'dos-mode)

(defvar dos-mode-distance 200
  "Number of characters to search for RETURN when looking for a DOS file.")

(defvar dos-mode nil
  "This buffer is to be converted to/from DOS format when read/written.")

(make-variable-buffer-local 'dos-mode)
(setq-default dos-mode nil)

(or (assq 'dos-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(dos-mode " DOS") minor-mode-alist)))

(defun dos-mode (arg)
  "Toggle DOS mode.
With arg, turn DOS mode on iff arg is positive.
In DOS mode, when the buffer is saved, it is converted to DOS format first,
and when restored it is converted to UNIX format first."
  (interactive "P")
  (setq dos-mode
	(if (null arg) (not dos-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p t))

(defvar dos-seen-ctl-Z nil
  "Remember whether C-Z was seen at the end of this buffer.")

(make-variable-buffer-local 'dos-seen-ctl-Z)
(setq-default dos-seen-ctl-Z nil)

(defun dos-convert-buffer-to-unix ()
  "Converts the current buffer from DOS format to UNIX format."
  (let ((mod-p (buffer-modified-p))
	(buffer-read-only nil))
    (save-excursion

      ;; Remove all C-m's that occur at the end of each line.
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match ""))

      ;; see if there is a C-z near the end of the buffer.
      (goto-char (point-max))
      (beginning-of-line)
      (forward-line -1)			;should be safe enough
      (setq dos-seen-ctl-Z (search-forward "\C-z" nil t))
      (if dos-seen-ctl-Z
	  (replace-match "")))

    (set-buffer-modified-p mod-p)
    (setq dos-mode t)))

(defun dos-convert-buffer-to-dos ()
  "Converts the current buffer from UNIX format to DOS format."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "\r\n"))
    (if dos-seen-ctl-Z
	(progn
	  (goto-char (point-max))
	  (insert "\C-z")))))

(defun dos-check-buffer ()
  "Used as a find-file hook, if the buffer looks like a DOS format buffer
then it will be converted to a UNIX format buffer."
  (if (or dos-mode
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward "\r$"
			       (min (point-max) dos-mode-distance)
			       t)))
      (dos-convert-buffer-to-unix)))

(defun dos-write-buffer ()
  "If the current buffer was originally a DOS format buffer, write it as such."
  (if dos-mode
      (let* ((buffer-read-only nil)
	     (window (get-buffer-window (current-buffer)))
	     (p (point))
	     (m (mark))
	     (ws (and window (window-start window))))
	(dos-convert-buffer-to-dos)
	(unwind-protect
	    (write-region (point-min)
			  (point-max)
			  buffer-file-name
			  nil
			  t)
	  (dos-convert-buffer-to-unix)	;convert back again!
	  (and window
	       (set-window-start window
				 (save-excursion (goto-char ws)
						 (beginning-of-line)
						 (point))))
	  (goto-char p)
	  (set-mark m))
	t)))

(or (memq 'dos-write-buffer write-file-hooks)
    (setq write-file-hooks
	  (append write-file-hooks
		  (list 'dos-write-buffer)))) ;stick it on the end

(or (memq 'dos-check-buffer find-file-hooks)
    (setq find-file-hooks
	  (append find-file-hooks
		  (list 'dos-check-buffer))))
