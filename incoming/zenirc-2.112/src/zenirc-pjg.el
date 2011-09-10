;;;
;;;
;;; zenirc-pjg.el 
;;; Automatically annoy Zen Internet Relay Chat client
;;;
;;; Copyright (C) 1993, 1994 Ben A. Mesander
;;;
;;; Author: Ben Mesander <ben@gnu.ai.mit.edu>
;;; Maintainer: ben@gnu.ai.mit.edu
;;; Keywords: extensions
;;; Created: 1994/02/28
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
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
;;;
;;; Commentary:
;;; 
;;; OJNK
;;;
;;;
;;; Code:

(require 'zenirc)

(defvar zenirc-pjg-interval '(0 600))
(defvar zenirc-last-pjg '(0 0))
(zenirc-add-hook 'zenirc-timer-hook 'zenirc-pjg)
(defun zenirc-pjg (proc now)
  (if (zenirc-time< zenirc-pjg-interval 
		    (zenirc-time-diff now zenirc-last-pjg))
      (progn
	(process-send-string
	 proc "PRIVMSG pjg :fascist pjg.\n")
	(setq zenirc-last-pjg now))))

(provide 'zenirc-pjg)

;;; zenirc-pjg.el ends here
