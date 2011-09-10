;;; zenirc-meditate.el --- admonish others for disturbing your meditation

;; Copyright (C) 1995, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, extensions
;; Created: 1995-04-09

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

(require 'zenirc-trigger)

(defconst zenirc-meditate-response-percentage 1)

(defconst zenirc-meditate-response-list
  '("Activity through inactivity."
    "Don't bother."
    "Enlightenment does not come from typing."
    "Enlightenment does not require a keyboard."
    "Hair will grow on your palms if you keep typing."
    "Meditate, or die."
    "Once a student typed too much and died."
    "Stop fidgeting, you're bothering the others."
    "The keyboard is sure to block your mind."
    "Will you stop the infernal racquet and meditate!?"
    "Your fingers will destroy your meditation."
    "Your keyboard is not the path to enlightenment."
    "Your typing detracts from your enlightenment."))

(defun zenirc-meditate ()
  (and (< (random 1000) zenirc-meditate-response-percentage)
       (nth (random (length zenirc-meditate-response-list))
            zenirc-meditate-response-list)))

(zenirc-trigger-register "meditate" 'zenirc-meditate "^.")

(provide 'zenirc-meditate)

;; zenirc-meditate.el ends here
