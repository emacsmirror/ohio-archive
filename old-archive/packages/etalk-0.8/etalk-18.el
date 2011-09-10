;;; etalk 18 compatibility file
;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Purpose:
;;;   To hide all the emacs-18 specific things, mostly dealing with
;;; tyrants, and vt100 keys since 18 didn't have all the nifty virtual 
;;; keys.

;; We need tyrant mode loaded for these to work.

(require 'etalk-tyrn)

(defvar etalk-tyrant-vt100-enable t
  "*Should the vt100 style arrow keys be remapped to emacs control
directionals within tyrant mode?")

(if (not etalk-tyrant-vt100-enable)
    ()
  (define-key etalk-tyrant-map "\e[A"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\e[B"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\e[C"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\e[D"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\eOA"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\eOB"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\eOC"  'etalk-tyrant-vt100-arrow)
  (define-key etalk-tyrant-map "\eOD"  'etalk-tyrant-vt100-arrow))

(defun etalk-tyrant-vt100-arrow ()
  "Recieve a vt100 arrow key?  Turn it into CTRL-F etc!"
  (interactive)
  (cond
   ((= last-input-char ?A)
    (setq last-input-char ?\C-p))
   ((= last-input-char ?B)
    (setq last-input-char ?\C-n))
   ((= last-input-char ?C)
    (setq last-input-char ?\C-f))
   ((= last-input-char ?D)
    (setq last-input-char ?\C-b))
   )
  (etalk-tyrant-fork-keypress)
)

;; end of lisp
(provide 'etalk-18)