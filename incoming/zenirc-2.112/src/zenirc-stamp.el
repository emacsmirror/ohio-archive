;;; zenirc-stamp.el --- timestamping for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;; Maintainer: ben@gnu.ai.mit.edu
;; Keywords: extensions
;; Created: 1993/06/03

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; This code is meant as a demonstration of how to use the ZenIRC
;; hook mechanism and timer code to cause ZenIRC to do something at a
;; regular interval.

;;; Code:

(require 'zenirc)

(defvar zenirc-timestamp-interval '(0 600)
  "How often to insert timestamps into the ZenIRC buffer. The default
is 600 seconds or 10 minutes. The value of this variable is a 32 bit
integer, expressed as a list of two 16 bit values, ie, the default
value of 600 seconds is expressed as (0 600).")

(defvar zenirc-last-timestamp '(0 0)
  "The time the last timestamp was inserted into the ZenIRC buffer.
You shouldn't have to frob this yourself.")

(defun zenirc-timestamp (proc now)
  "Insert a timestamp into the the ZenIRC buffer specified by the
process PROC every zenirc-timestamp-interval seconds."
  (if (zenirc-time< zenirc-timestamp-interval
		    (zenirc-time-diff now zenirc-last-timestamp))
      (progn
	(zenirc-message proc (concat "[time] " (current-time-string) "\n"))
	(setq zenirc-last-timestamp now))))

(provide 'zenirc-stamp)

(zenirc-add-hook 'zenirc-timer-hook 'zenirc-timestamp)

;; zenirc-stamp.el ends here
