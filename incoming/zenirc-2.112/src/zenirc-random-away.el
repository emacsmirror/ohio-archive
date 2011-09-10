;;; zenirc-random-away.el

;; Copyright (C) 1995 Eric Prestemon

;; Author: Eric Prestemon <ecp@io.com>
;; Maintainer: eric@american.edu
;; Keywords: zenirc, extensions
;; Created: 1995-01-04

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
;;; Code:

(require 'zenirc)

(defvar zenirc-random-away-strings
  '("I've felt better, but it cost more."
    "I want to be the one with the most cake."
    "Someday you will ache like I ache."
    "Do you have the time to listen to me whine?"
    "Neurotic in my head no doubt about it."
    "Oh what a feeling when we're dancing on the ceiling."
    "What's so funny about peace, love, and understanding?")
  "*List of strings to use for /away messages")

(defun zenirc-random-away (proc parsedmsg)
  "Change away message to a new thing."
  (and (not (zenirc-channel-p (aref parsedmsg 2)))
       (process-send-string proc
        (concat "AWAY :"
                (nth (random (length zenirc-random-away-strings))
                     zenirc-random-away-strings)
                "\n"))))

(zenirc-add-hook 'zenirc-server-PRIVMSG-hook 'zenirc-random-away t)

(provide 'zenirc-random-away)

;;; zenirc-random-away.el ends here
