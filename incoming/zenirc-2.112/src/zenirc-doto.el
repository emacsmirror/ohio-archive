;;; zenirc-doto.el --- do things to who, list, links replies

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1998 Per Persson

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1994/07/22

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
(require 'zenirc-command-queue)

(defun zenirc-doto-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((doto-lame-args . "[info] %s: lame argument(s)."))))


;; WHO reply handlers
;; example: send everyone on IRC a DCC chat request from the
;; telnet port of a pentagon computer.
;; /dotowho 0 (process-send-string proc (concat "PRIVMSG " (aref whoreply 7)
;;   " :\C-aDCC CHAT chat 2261613455 23\C-a\n"))

;; 315 (end of who) handler
(defun zenirc-do-to-who-end (proc parsedmsg)
  (zenirc-delete-hook 'zenirc-server-352-hook 'zenirc-do-to-who)
  (zenirc-delete-hook 'zenirc-server-315-hook 'zenirc-do-to-who-end)
  (setq zenirc-run-next-hook nil))

;; 352 (who reply) handler
;; proc is the zenirc process.
;; whoreply is the parsed server message array.
(defun zenirc-do-to-who (proc whoreply)
  (zenirc-do-to-who-function proc whoreply)
  (setq zenirc-run-next-hook nil))

;; command interface
;; /dotowho #victims (lisp-form)
;; lisp-form can use "proc", which will be set to the zenirc process
;; and "whoreply" which will be a parsed servermessage array containing
;; a 352 reply.
(defvar zenirc-command-dotowho-hook '(zenirc-command-dotowho))

(defun zenirc-command-dotowho (proc parsedcmd)
  (let* ((arg (zenirc-parse-firstword (cdr parsedcmd)))
	 (victim (car arg))
	 (command (cdr arg)))
    (if (or (string= "" victim)
            (string= "" command))
	(zenirc-message proc 'doto-lame-args "/dotowho"))
    (zenirc-dotowho victim (read command))))

;; programmatic interface
(defun zenirc-dotowho (victim command)
  (fset 'zenirc-do-to-who-function (list 'lambda '(proc whoreply) command))
  (zenirc-add-hook 'zenirc-server-315-hook 'zenirc-do-to-who-end)
  (zenirc-add-hook 'zenirc-server-352-hook 'zenirc-do-to-who)
  (process-send-string proc (concat "WHO " victim "\n")))


;; example of using who
;; /op channel - op everyone on a channel
(defvar zenirc-op-list nil)

(defvar zenirc-command-op-hook '(zenirc-command-op))

(defun zenirc-command-op (proc parsedcmd)
  (let ((victim (cdr parsedcmd)))
    (if (string= victim "")
	(zenirc-message proc 'doto-lame-args "/op")
      (setq zenirc-op-list nil)
      (zenirc-dotowho
       victim
       '(if (not (string-match "@" (aref whoreply 8)))
	    (setq zenirc-op-list (cons (aref whoreply 7) zenirc-op-list))))
      (zenirc-add-hook 'zenirc-server-315-hook 'zenirc-doto-op-end))))

(defun zenirc-doto-op-end (proc parsedmsg)
  (let* ((channel (aref parsedmsg 3))
	 (oplen (length zenirc-op-list))
	 (triples (* 3 (/ oplen 3)))
	 (remainder (% oplen 3))
	 (i 0)
	 (nicks nil))
    (while (< i triples)
      (setq nicks (concat (nth i zenirc-op-list) " "
			  (nth (1+ i) zenirc-op-list) " "
			  (nth (+ 2 i) zenirc-op-list) "\n"))
      (process-send-string proc (concat "mode " channel " +ooo " nicks))
      (setq i (+ 3 i)))
    (cond
     ((eq remainder 2)
      (setq nicks (concat (nth (- oplen 2) zenirc-op-list) " "
			  (nth (1- oplen) zenirc-op-list) "\n"))
      (process-send-string proc (concat "mode " channel " +oo " nicks)))
     ((eq remainder 1)
      (process-send-string
       proc (concat "mode " channel " +o "
		    (nth (1- oplen) zenirc-op-list) "\n"))))
    (zenirc-delete-hook 'zenirc-server-315-hook 'zenirc-op-end)))


;; server link stuff

;; 365 (end of links) handler
(defun zenirc-do-to-links-end (proc parsedmsg)
  (zenirc-delete-hook 'zenirc-server-364-hook 'zenirc-do-to-links)
  (zenirc-delete-hook 'zenirc-server-365-hook 'zenirc-do-to-links-end)
  (setq zenirc-run-next-hook nil))

;; 364 (links reply) handler
;; proc is the zenirc process.
;; linksreply is the parsed server message array.
(defun zenirc-do-to-links (proc linksreply)
  (zenirc-do-to-links-function proc linksreply)
  (setq zenirc-run-next-hook nil))

;; command interface
;; /dotolinks (lisp-form)
;; lisp-form can use "proc", which will be set to the zenirc process
;; and "linksreply" which will be a parsed servermessage array containing
;; a 364 reply.
(defvar zenirc-command-dotolinks-hook '(zenirc-command-dotolinks))

(defun zenirc-command-dotolinks (proc parsedcmd)
  (let ((arg (cdr parsedcmd)))
    (if (string= "" arg)
	(zenirc-message proc 'doto-lame-args "/dotolinks")
      (zenirc-dotolinks (read arg)))))

;; programmatic interface
(defun zenirc-dotolinks (command)
  (fset 'zenirc-do-to-links-function (list 'lambda '(proc linksreply) command))
  (zenirc-add-hook 'zenirc-server-365-hook 'zenirc-do-to-links-end)
  (zenirc-add-hook 'zenirc-server-364-hook 'zenirc-do-to-links)
  (process-send-string proc "LINKS\n"))

(defvar zenirc-command-serverversions-hook '(zenirc-server-versions))
(defun zenirc-server-versions (proc parsedcmd)
  (zenirc-dotolinks '(zenirc-queue-command (concat "VERSION "
						   (aref linksreply 4)
						   "\n"))))


;; Do things to /list reply
;; 321 RPL_LISTSTART
;; 322 RPL_LIST
;; 323 RPL_LISTEND
;; Example: set the topic of every channel on irc to be AT&T YOU WILL
;; while talking on #twilight_zone
;; /dotolist (if (not (string= (aref listreply 3) "*")) (progn
;; (zenirc-queue-command (concat "JOIN " (aref listreply 3) "\n"))
;; (zenirc-queue-command '(setq zenirc-current-victim "#twilight_zone"))
;; (zenirc-queue-command (concat "TOPIC " (aref listreply 3)
;; " :AT&T YOU WILL!!!!\n")) (zenirc-queue-command (concat "PART "
;; (aref listreply 3) "\n"))))

;; 323 (end of list) handler
(defun zenirc-do-to-list-end (proc parsedmsg)
  (zenirc-delete-hook 'zenirc-server-322-hook 'zenirc-do-to-list)
  (zenirc-delete-hook 'zenirc-server-323-hook 'zenirc-do-to-list-end)
  (setq zenirc-run-next-hook nil))

;; 322 (list reply) handler
;; proc is the zenirc process.
;; listreply is the parsed server message array.
(defun zenirc-do-to-list (proc listreply)
  (zenirc-do-to-list-function proc listreply)
  (setq zenirc-run-next-hook nil))

;; 321 (list start) handler
(defun zenirc-do-to-list-start (proc parsedmsg)
  (zenirc-delete-hook 'zenirc-server-321-hook 'zenirc-do-to-list-start)
  (setq zenirc-run-next-hook nil))

;; command interface
;; /dotolist (lisp-form)
;; lisp-form can use "proc", which will be set to the zenirc process
;; and "listreply" which will be a parsed servermessage array containing
;; a 322 reply.
(defvar zenirc-command-dotolist-hook '(zenirc-command-dotolist))

(defun zenirc-command-dotolist (proc parsedcmd)
  (let* ((arg (cdr parsedcmd)))
    (if (string= "" arg)
	(zenirc-message proc 'doto-lame-args "/dotolist"))
    (zenirc-dotolist (read arg))))

;; programmatic interface
(defun zenirc-dotolist (command)
  (fset 'zenirc-do-to-list-function (list 'lambda '(proc listreply) command))
  (zenirc-add-hook 'zenirc-server-323-hook 'zenirc-do-to-list-end)
  (zenirc-add-hook 'zenirc-server-322-hook 'zenirc-do-to-list)
  (zenirc-add-hook 'zenirc-server-321-hook 'zenirc-do-to-list-start)
  (process-send-string proc "LIST\n"))

(provide 'zenirc-doto)

(zenirc-doto-install-message-catalogs)

;;; End of zenirc-doto.el
