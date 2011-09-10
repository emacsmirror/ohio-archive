;;; zenirc-away.el --- fancy away processing for ZenIRC

;;; Copyright (C) 1994 Noah S. Friedman
;;; Copyright (C) 1998 Per Persson

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: pp@sno.pp.se
;;; Keywords: zenirc, extensions, oink
;;; Created: 1994-06-26

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

;;; Commentary:

;;; This package reduces the number of times you see a user's /away message.
;;; The first time you see that user's /away, the usual thing happens.
;;; Subsequent /away messages are suppressed until the user changes it.

;;; Code:

(require 'zenirc)

;; 101 buckets should be a reasonable size for most people (remember to use
;; a prime number to get good hashing characteristics).
;; This is not the total number of nicks you can cache, but just the number
;; of "buckets" in which nicks can be stored.  If you talk to thousands
;; and thousands of people it might help to increase the size of this
;; table, but even then it isn't necessary.
(defvar zenirc-away-table (make-vector 101 0)
  "Association list of nicknames and their /away messages.
zenirc-server-301-fancy suppresses the display of /away messages if
you've already seen them.")

(defun zenirc-server-301-fancy (proc parsedmsg)
  "Display /away message if you haven't seen it already."
  (let* ((from (zenirc-extract-nick (aref parsedmsg 3)))
         (text (aref parsedmsg 4))
         (nicksym (intern (zenirc-downcase-name from) zenirc-away-table))
         (cached (and (boundp nicksym) (symbol-value nicksym))))
    (cond ((and cached (string= text cached)))
          (t
           (set nicksym text)
           (zenirc-message proc 's301 from text)))))

(defun zenirc-server-301-signal-p (proc parsedmsg)
  "Allow signals on /away message if you haven't seen it already."
  (if (string= (aref parsedmsg 0) "301")
      (let* ((from (zenirc-extract-nick (aref parsedmsg 3)))
	     (text (aref parsedmsg 4))
	     (nicksym (intern (zenirc-downcase-name from) zenirc-away-table))
	     (cached (and (boundp nicksym) (symbol-value nicksym))))
	(if (and cached (string= text cached))
	    (setq zenirc-run-next-hook nil)))))

(provide 'zenirc-away)

(zenirc-remove-hook 'zenirc-server-301-hook 'zenirc-server-301)
(zenirc-add-hook 'zenirc-server-301-hook 'zenirc-server-301-fancy)
(zenirc-add-hook 'zenirc-signal-hook 'zenirc-server-301-signal-p)

;;; zenirc-away.el ends here
