;;; display-swascii.el --- set display for ISO 646-SE (swascii)

;; Copyright (C) 1995 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: display-swascii.el,v 1.2 1995/06/21 17:37:21 lenst Exp $
;; Keywords: i18n, local
;; Last edited: Wed Jun 21 19:33:55 1995 by lenst@tiny.lysator.liu.se (Lennart Staflin)

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
;;; program's author (send electronic mail to les@ida.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;; LCD Archive Entry:
;; display-swascii|Lennart Staflin|lenst@lysator.liu.se|
;; Set current display table to display ISO 646-SE (aka swascii) on a
;; ISO 8859-1 display|21-May-1995|1.2|~/misc/display-swascii.el.gz|

;;; Commentary:

;; This package allows viewing a buffer containing swascii if you have
;; a display that handles ISO 8859-1. This is done by building a display-
;; table that remaps the Swedish characters in swascii to ISO 8859-1 codes.
;; This also works if you have some kind of ISO 8859-1 emulation, like
;; the iso-ascii or iso-swed packages.

;; To turn on swascii display use M-x display-swascii
;; to turn it off use  C-u - M-x display-swascii

;;; Code:

(defvar swascii-display-table nil
  "Character table used to display the swedish version of ascii.")

;;;###autoload
(defun display-swascii (onoff)
  "Set current buffer's display table to emulate swascii (ISO 646-SE).
With a prefix argument less than 0, turn off the emulation. With an
argument greater than 0, turn emulation on. An argument of 0 toggels.
If the argument is 17, the display table used will be rebuilt from the
current `standard-display-table'."
  (interactive "p")
  ;; Create the swascii character table
  (require 'disp-table)
  (cond
   ((or (null swascii-display-table)
	(= onoff 17))
    (setq swascii-display-table (copy-sequence standard-display-table))
    (let ((l '((?ö . ?|)
	       (?Ö . ?\\)
	       (?ä . ?{)
	       (?Ä . ?\[)
	       (?å . ?})
	       (?Å . ?\]))))
      (while l
	(aset swascii-display-table (cdr (car l))
	      (aref swascii-display-table (car (car l))))
	(setq l (cdr l))))))
  (cond
   ((or (< onoff 0)
	(and (zerop onoff)
	     (eq buffer-display-table swascii-display-table)))
    (kill-local-variable 'buffer-display-table)
    (setq buffer-display-table standard-display-table))
   (t
    (setq buffer-display-table swascii-display-table)))
  (let ((w (get-buffer-window (current-buffer))))
    (if (equal (window-buffer (selected-window)) (current-buffer))
	(set-window-buffer (selected-window) (current-buffer)))
    (if w
	(set-window-buffer w (current-buffer)))))

;;; display-swascii.el ends here
