;;; zenirc-netsplit.el --- hide excessive spew from netsplits

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1995 Noah S. Friedman
;; Copyright (C) 1998 Per Persson

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Eric Prestemon <ecp@io.com>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, extensions
;; Created: 1993/03/10

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

;; This script attempts to supress excessive signon/offs and mode changes
;; due to netsplits.

;;; Code:

(require 'zenirc)

(defvar zenirc-netsplit-show-server-mode-changes-p nil
  "Set to t to enable display of server mode changes.")

(defvar zenirc-netsplit-debug nil
  "Set to t in order to enable debugging messages in the netsplit code")

;; this is a list of the form
;; (("a.b.c.d e.f.g" (time stamp) first-join "nick1" ... "nickn") ...)
;; where first-join is t or nil, depending on whether or not the first
;; join from that split has been detected or not.
(defvar zenirc-netsplit-list nil)
(make-variable-buffer-local 'zenirc-netsplit-list)

(defvar zenirc-command-wholeft-hook '(zenirc-netsplit-wholeft))

(zenirc-add-hook 'zenirc-server-JOIN-hook 'zenirc-netsplit-JOIN)
(zenirc-add-hook 'zenirc-server-MODE-hook 'zenirc-netsplit-MODE)
(zenirc-add-hook 'zenirc-server-QUIT-hook 'zenirc-netsplit-QUIT)
(zenirc-add-hook 'zenirc-timer-hook 'zenirc-netsplit-timer)

;; TODO: add messages for other languages
(defun zenirc-netsplit-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((netsplit . "[info] netsplit: %s")
     (netsplit-join . "[info] netjoin: %s")
     (netsplit-wholeft . "[info] split: %s missing: %s %s")
     )))

;; show/don't show rejoins
(defun zenirc-netsplit-JOIN (proc parsedmsg)
  (let ((nick (zenirc-downcase-name (zenirc-extract-nick (aref parsedmsg 1))))
        (list zenirc-netsplit-list)
        elt)
    (while list
      (setq elt (car list))
      (setq list (cdr list))
      (if (member nick (nthcdr 3 elt))
	  (progn
	    (setq zenirc-run-next-hook nil)
	    (if (not (car (cdr (cdr elt))))
		(progn
		  (zenirc-message proc 'netsplit-join (car elt))
		  (setcar (nthcdr 2 elt) t)))
	    ;; need to remove this nick, perhaps the whole entry here.
            ;; Note that by removing the nick now, we can't tell if further
            ;; join messages (for other channels) should also be
            ;; suppressed.
	    (if (null (nthcdr 4 elt))
		(setq zenirc-netsplit-list (delq elt zenirc-netsplit-list))
              (delete nick elt)))))))

;; hide mode changes from servers
(defun zenirc-netsplit-MODE (proc parsedmsg)
  (save-match-data
    ;; regexp matches things with a . in them, and no ! or @ in them.
    (cond ((string-match "^[^@!]+\\.[^@!]+$" (aref parsedmsg 1))
           (and zenirc-netsplit-debug
                (zenirc-message proc "[debug] server mode change.\n"))
           (or zenirc-netsplit-show-server-mode-changes-p
               (setq zenirc-run-next-hook nil))))))

;; detect netsplits
(defun zenirc-netsplit-QUIT (proc parsedmsg)
  (save-match-data
    (let* ((split (zenirc-downcase-name (aref parsedmsg 2)))
           (nick (zenirc-downcase-name (zenirc-extract-nick (aref parsedmsg 1))))
           ass)
      ;; look for arguments of the form host.name.1 host.name.2
      (if (string-match "^[^ ]+\\.[^ ]+ [^ ]+\\.[^ ]+$" split)
          (progn
            (setq zenirc-run-next-hook nil)
            (setq ass (assoc split zenirc-netsplit-list))
            (if ass
                ;; element for this netsplit exists already
                (setcdr (nthcdr 2 ass) (cons nick (nthcdr 3 ass)))
              ;; element for this netsplit does not yet exist
              (setq zenirc-netsplit-list
                    (cons (list split
                                (zenirc-time-to-int (current-time-string))
                                nil
                                nick)
                          zenirc-netsplit-list))
              (zenirc-message proc 'netsplit split)))))))

;; clean cruft from zenirc-netsplit-list older than 10 minutes
(defun zenirc-netsplit-timer (proc now)
  (let ((list zenirc-netsplit-list)
        elt)
    (while list
      (setq elt (car list))
      (setq list (cdr list))
      (and (zenirc-time< '(0 600) (zenirc-time-diff now (car (cdr elt))))
           (setq zenirc-netsplit-list (delq elt zenirc-netsplit-list))))))

;; show who's gone
(defun zenirc-netsplit-wholeft (proc parsedcmd)
  (let ((list zenirc-netsplit-list)
        elt)
    (while list
      (setq elt (car list))
      (setq list (cdr list))
      (zenirc-message proc 'netsplit-wholeft
                      (car elt)
                      (mapconcat 'identity (nthcdr 3 elt) " ")
                      (if (car (cdr (cdr elt)))
                          "(joining)"
                        "")))))

(provide 'zenirc-netsplit)

(zenirc-netsplit-install-message-catalogs)

;;; zenirc-netsplit.el ends here
