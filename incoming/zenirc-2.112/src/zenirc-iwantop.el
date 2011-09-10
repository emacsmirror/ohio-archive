;;; zenirc-iwantop.el --- IWANTOP ctcp for granting channel operator bits

;; Copyright (C) 1995 Eric Prestemon
;; Copyright (C) 1995, 1996 Per Persson

;; Author: Eric Prestemon <ecp@io.com>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, extensions
;; Created: 1995-03-31

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

(defvar zenirc-iwantop-alist nil
  "*Association list of channel names and users allowed to be channel-opped.
That is, when someone sends you an IWANTOP ctcp, they are checked against
this alist to see if your client should automatically op them.

Each channel name and user is treated as a regular expression.
More than one user can be listed for each channel.

Here is an example:

    (setq zenirc-iwantop-alist
          '((\"#twilight_zone\" \"pjg@.*.buffalo.edu\" \"trillian!.*kei.com\")
            (\"#bondage\"       \".*\")))

This will allow anyone to be opped, anywhere:

    (setq zenirc-iwantop-alist '((\".*\" \".*\")))
")

; If you want ZenIRC to see other things then just IWANTOP, do stuff like
; (setq zenirc-ctcp-query-LICKMYPENIS-hook '(zenirc-ctcp-query-IWANTOP))
; in your .emacs or wherever.
(defvar zenirc-ctcp-query-IWANTOP-hook '(zenirc-ctcp-query-IWANTOP))

(defun zenirc-ctcp-query-IWANTOP (proc parsedctcp from to)
  (save-match-data
    (let ((case-fold-search t)
          (sender (zenirc-extract-nick from))
          (nick (zenirc-run-hook 'zenirc-format-nickuserhost-hook from))
          (channel (car (zenirc-parse-firstword (cdr parsedctcp))))
          (fmt-failed "NOTICE %s :Oink!\n")
	  (fmt-sorry "NOTICE %s :Missing #channel argument!\n")
          (fmt-mode   "MODE %s +o %s\n")
          (alist zenirc-iwantop-alist)
          (list nil))

      (and zenirc-verbose-ctcp
           (zenirc-message proc 'query nick to
                           (concat (car parsedctcp) " " (cdr parsedctcp))))

      (cond ((or (null channel)
                 (not (zenirc-channel-p channel)))
             (process-send-string proc (format fmt-sorry sender)))
            (t
             ;; Do a zenirc-downcase-name even though case-fold-search is t
             ;; because extra characters are translated to conform with
             ;; RFC1459.
             (setq channel (zenirc-downcase-name channel))
             (while alist
               (and (string-match (zenirc-downcase-name (car (car alist)))
                                 channel)
                    (progn
                      ;; skip the first elt of the car of alist, since
                      ;; that's just the channel name regexp
                      (setq list (cdr (car alist)))
                      (setq alist nil)))
               (setq alist (cdr alist)))

             ;; if verbose ctcp is on, tell the user we got the query
             (if (and list (zenirc-string-match-list from list))
                 (process-send-string proc (format fmt-mode channel sender))
               (process-send-string proc (format fmt-failed sender))))))))

(provide 'zenirc-iwantop)

;;; zenirc-iwantop.el ends here
