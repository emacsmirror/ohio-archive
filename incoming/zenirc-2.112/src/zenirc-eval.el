;;; zenirc-eval.el --- join the "mi emacs es tu emacs" club

;; Copyright (C) 1997, 1998 Ray Jones

;; Author: Ray Jones <rjones@pobox.com>
;; Maintainer: rjones@pobox.com
;; Keywords: zenirc, extensions, eval, oink
;; Created: 1998-01-09


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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Whip me, beat me, make my emacs calculate primes via church
;; numerals.

;; WARNING WARNING WARNING WARNING WARNING WARNING WARNING
;;
;; *** DO NOT RUN THIS CODE UNLESS YOU KNOW WHAT IT DOES ***
;;
;; Running this code could be hazardous to your emacs, your files,
;; the machine you're using, and several other things you may not
;; wish to expose to the ministrations of random lusers, particularly
;; in the IRC world.  You have been warned.
;;
;; WARNING WARNING WARNING WARNING WARNING WARNING WARNING

;;; Code:

(require 'zenirc)
(require 'zenirc-trigger)

(defun zenirc-eval (&optional msg)
  ;; parse out the (e)valuable bit
  (string-match "(eval \\(.*\\))" msg)

  ;; provide a modicum of protection from malicious parties.
  ;; NB it can be circumvented via a similar expression.
  (let ((kill-emacs-hook 
	 #'(lambda () (error "kill-emacs called from zenirc-eval"))))
    (condition-case err
	(let ((res (prin1-to-string (eval (read (match-string 1 msg))))))
	  (if (string= "" res)
	      "OJNK!"
	    res))
      (error (format "Error: %s" err)))))

(zenirc-trigger-register "eval" 'zenirc-eval "(eval \\(.*\\))" t)

;;; zenirc-eval.el ends here.

