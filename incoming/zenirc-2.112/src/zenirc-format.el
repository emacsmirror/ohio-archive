;;; zenirc-format.el --- format nick!user@host for zenirc

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1995 Noah S. Friedman

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Charles Hannum <mycroft@gnu.ai.mit.edu>
;;         Richard Todd <rmtodd@essex.ecn.uoknor.edu>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: zenirc, extensions
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

;; Format nick!user@host info the first time you see someone as
;; nick!user@host and thereafter as just nick.  Nick changes are detected
;; and handled correctly.

;; This was rewritten 1995-03-18 by friedman to use a hash table instead of
;; an alist.

;;; Code:

(require 'zenirc)

;; 101 buckets should be a reasonable size for most people (remember to use
;; a prime number to get good hashing characteristics).
;; This is not the total number of nicks you can cache, but just the number
;; of "buckets" in which nicks can be stored.  If you talk to thousands
;; and thousands of people it might help to increase the size of this
;; table, but even then it isn't necessary.
(defvar zenirc-nickuserhost-table (make-vector 101 0)
  "Table used to store nicknames and corresponding nick!user@host.")

(defun zenirc-format-nickuserhost-fancy (nickuserhost)
  (let ((nick (zenirc-extract-nick nickuserhost)))
    (if nick
	(let* ((nicksym (intern (zenirc-downcase-name nick)
                                zenirc-nickuserhost-table))
               (cached (and (boundp nicksym)
                            (symbol-value nicksym))))
	  ; possible BUG
	  (setq zenirc-run-next-hook nil)
          (set nicksym nickuserhost)
          (and cached
               (string= cached nickuserhost)
               (setq nickuserhost nick)))))
  nickuserhost)

(zenirc-add-hook 'zenirc-format-nickuserhost-hook
                 'zenirc-format-nickuserhost-fancy)

(defun zenirc-fancy-NICK (proc parsedmsg)
  (let* ((userhost (zenirc-extract-userhost (aref parsedmsg 1)))
         (to (aref parsedmsg 2)))
    (set (intern (zenirc-downcase-name to) zenirc-nickuserhost-table)
         (concat to "!" userhost))))

(zenirc-add-hook 'zenirc-server-NICK-hook 'zenirc-fancy-NICK)

;; Also hook into the /who display.
;; (added by rmtodd, rewritten by friedman)
(defun zenirc-fancy-352 (proc parsedmsg)
  ;; If the 3 arg is "Channel", this is the header.
  ;; One wonders why the header isn't a different numeric.
  (or (string= (aref parsedmsg 3) "Channel")
      (let* ((nick (aref parsedmsg 7))
             (nickuserhost (concat nick "!" (aref parsedmsg 4) "@"
                                   (aref parsedmsg 5)))
             (nicksym (intern (zenirc-downcase-name nick)
                              zenirc-nickuserhost-table)))
        (set nicksym nickuserhost))))


(zenirc-add-hook 'zenirc-server-352-hook 'zenirc-fancy-352)

(provide 'zenirc-format)

;; zenirc-format.el ends here
