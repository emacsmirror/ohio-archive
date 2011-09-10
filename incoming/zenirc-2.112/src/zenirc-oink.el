;;; zenirc-oink.el --- auto-oink

;; Copyright (C) 1995, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
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

(defconst zenirc-oink-response-percentage 1)

(defconst zenirc-oink-response-list
  '("oink"
    "oink!"
    "oink."
    "bozoink"
    "oinkage"
    "knio"
    "you will be in your oink soon"
    "don't say oink"
    ":1 s/oink/oink oink/g; b 1"
    ;; the oink combinator
    "(define oink (lambda (oink?) ((lambda (oink) (oink? (lambda (oink!) ((oink oink) oink!)))) (lambda (oink) (oink? (lambda (oink!) ((oink oink) oink!)))))))"))

(defun zenirc-oink ()
  (and (< (random 1000) zenirc-oink-response-percentage)
       (nth (random (length zenirc-oink-response-list))
            zenirc-oink-response-list)))

(zenirc-trigger-register "oink" 'zenirc-oink "oink\\|knio")

(provide 'zenirc-oink)

;; zenirc-oink.el ends here