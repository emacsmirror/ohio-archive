;;; zenirc-notify.el --- Notifies you when people signon/off

;; Copyright (C) 1995, 1996, 1997, 1998 Per Persson

;; Author: Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, notify, extensions
;; Created: 1995-03-30

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

;; TODO: If notifee joins channel, don't show notification.

;;; Code:

(require 'zenirc)

(defvar zenirc-notify-list '()
  "*A list of nicknames that you want to watch for on IRC.")

(defvar zenirc-notify-interval '(0 60)
  "*Time between ISON's sent to server.")

(defvar zenirc-last-notify '(0 0)
  "Time previous ISON sent.")
(make-variable-buffer-local 'zenirc-last-notify)

(zenirc-add-hook 'zenirc-timer-hook 'zenirc-notify-timer)

;; used with notify code to see if we did /ison or not
(defvar zenirc-manual-ison nil)
(make-variable-buffer-local 'zenirc-manual-ison)

;; people you previously recieved notification of
(defvar zenirc-previous-ison nil)
(make-variable-buffer-local 'zenirc-previous-ison)

;; should you get a user@host reply on notify?
(defvar zenirc-userhost-on-notify t)
(make-variable-buffer-local 'zenirc-userhost-on-notify)

(defvar zenirc-userhost-by-notify nil)
(make-variable-buffer-local 'zenirc-userhost-by-notify)

(defvar zenirc-command-notify-hook '(zenirc-command-notify)
  "*Hook to call when a /notify command is issued in ZeniIRC.

The syntax of the command is: /notify victim.
This toggles the presence of `victim' in your notify-list.
If no victim is specified, you will see your notify-list instead.")

; hooks to make zenirc aware of the notify code.
(defvar zenirc-command-ison-hook '(zenirc-command-ison))
(defvar zenirc-command-userhost-hook '(zenirc-command-userhost))

(defun zenirc-notify-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((notify_list . "[info] Your current notify list: %s")
     (notify_on . "[info] detected %s wasting time.")
     (notify_off . "[info] detected that %s stopped wasting time.")
     (notify_current . "[info] Notificated people wasting time: %s")
     )))

(defun zenirc-notify-timer (proc now)
  "Call zenirc-command-notify-hook with arguments that cause it to send an
ISON message to the server. This is used to notice when people in the notify
list have come on or off of IRC."
  (if (zenirc-time< zenirc-notify-interval
                    (zenirc-time-diff now zenirc-last-notify))
      (progn
        (zenirc-run-hook 'zenirc-command-notify-hook proc 
			 '("notify" . "%auto"))
        (setq zenirc-last-notify now))))


;; /notify handler
;;
;; *** NOTE ***
;; this is also called from the zenirc event handling code
;;
(defun zenirc-command-notify (proc parsedcmd)
  (let ((arg (cdr parsedcmd)))
    (if (string-equal "" arg)
	; output list of notificated people (online)
	(progn 
	  (if (not zenirc-previous-ison)
	      (zenirc-message proc 'notify_current "")
	    (zenirc-message proc 'notify_current zenirc-previous-ison))
	  (zenirc-message proc 'notify_list
			  (mapconcat 'identity zenirc-notify-list " ")))
      (if (not (string-match "%" arg))
	  ; add or remove nick from zenirc-notify-list
	  (progn
	    (setq arg (zenirc-parse-words arg))
	    (while arg
	      (if (zenirc-string-match-list (car arg) zenirc-notify-list)
		  (setq zenirc-notify-list (zenirc-delete-case-insensitive 
					    (car arg) zenirc-notify-list))
		(setq zenirc-notify-list (cons (car arg) zenirc-notify-list)))
	      (setq arg (cdr arg)))
	    ; output new list of notificated people
	    (zenirc-message proc 'notify_list
			    (mapconcat 'identity zenirc-notify-list " "))))
      (if zenirc-notify-list
	  ; if automated, check to see if anything has changed
	  (process-send-string
	   proc
	   (concat "ISON " 
		   (mapconcat 'identity zenirc-notify-list " ") "\n"))))))

;; /ison nick1 [nick2 [nick3...]]
(defun zenirc-command-ison (proc parsedcmd)
  (process-send-string proc
                       (concat "ISON " (cdr parsedcmd) "\n"))
  (setq zenirc-manual-ison 1))

;; /userhost nick1 [nick2 [nick3...]]
(defun zenirc-command-userhost (proc parsedcmd)
  (process-send-string proc (concat "USERHOST " (cdr parsedcmd) "\n"))
  (setq zenirc-userhost-by-notify nil))

(defun zenirc-server-303-notify (proc parsedmsg)
  (let* ((ison-list (zenirc-parse-words (aref parsedmsg 3))))
    ; check if user issued /ison and don't want the notify code to execute
    (if zenirc-manual-ison
        (progn
          (zenirc-message proc 's303 (aref parsedmsg 3))
          (setq zenirc-manual-ison nil))
      (let ((new-list ison-list)
	    (old-list zenirc-previous-ison))
	; check if a certain nick wasn't seen the last time
	(while new-list
	  (if (not (member (car new-list)
			   zenirc-previous-ison))
	      ; check if user wants user@host displayed.
	      (if zenirc-userhost-on-notify
		  (progn
		    (process-send-string 
		     proc 
		     (concat "USERHOST " (car new-list) "\n"))
		    (setq zenirc-userhost-by-notify t))
		(zenirc-message proc 'notify_on (car new-list))))
	  (setq new-list (cdr new-list)))
	; check if a certain nick was seen the last time
	(while old-list
	  (if (not (member (car old-list)
			   ison-list))
	      (zenirc-message proc 'notify_off (car old-list)))
	  (setq old-list (cdr old-list))))
      (setq zenirc-previous-ison ison-list))))

(defun zenirc-server-302-notify (proc parsedmsg)
  (if zenirc-userhost-by-notify
      ; go on, sue me for being lazy and using @ for this check.
      ; --pp
      ; if you understand why I do this check, you deserve a nice
      ; reward. took me about 10 minutes to figure it out myself.
      ; --pp
      (if (string-match "@" (aref parsedmsg 3))
	  (zenirc-message proc 'notify_on (aref parsedmsg 3)))
    (zenirc-message proc 's302 (aref parsedmsg 3))))


(provide 'zenirc-notify)

(zenirc-notify-install-message-catalogs)

(zenirc-remove-hook 'zenirc-server-303-hook 'zenirc-server-303)
(zenirc-add-hook 'zenirc-server-303-hook 'zenirc-server-303-notify)
(zenirc-remove-hook 'zenirc-server-302-hook 'zenirc-server-302)
(zenirc-add-hook 'zenirc-server-302-hook 'zenirc-server-302-notify)

;;; zenirc-notify.el ends here
