;;; zenirc-trigger.el

;; Copyright (C) 1997 Noah S. Friedman
;; Copyright (C) 1997, 1998 Per Persson

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, extensions, oink
;; Created: 1997-03-01

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

;; Use this package to define "triggers" regexps.
;; For example, you can define a function /time and specify a regexp that
;; sends the current time whenever someone asks "what time is it?":
;;
;; (zenirc-trigger-register "time" 'current-time-string "what time is it\\?")
;;
;; You can also use the /trigger command in the *zenirc* buffer:
;;
;; /trigger set time "what time is it\\?" current-time-string
;;
;; People have defined more interesting triggers such as horoscopes, zippy
;; quotes, etc.

;;; Code:

(require 'zenirc)
(require 'backquote)

(defvar zenirc-trigger-table nil)
(defvar zenirc-command-trigger-hook '(zenirc-command-trigger))

;; Changing this to NOTICE will generally avoid loops from other clients
;; that also load this.  But that's less fun.
(defconst zenirc-trigger-response-type "PRIVMSG")

(defun zenirc-trigger-register (name response &optional regexp pass-string)
  "Define a trigger named NAME, that runs RESPONSE when REGEXP is seen, 
passing the matched-string if PASS-STRING is true.
When a string mathcing REGEXP is detected in a zenirc buffer, the function
RESPONSE is called with no arguments by default, or with the matched string
if PASS-STRING is non-nil.  That function should return a string
which is to be sent to the originator of the message matching REGEXP.
It may instead return a list of strings, in which case each string is
sent as a separate message.

NAME can be used as a key for changing, deleting, activating, and
deactivating the trigger via /trigger subcommands."
  (zenirc-trigger-make-command name response)
  (and regexp
       (zenirc-trigger-set-trigger name regexp response pass-string)))

;;; Users shouldn't generally need to make use of anything below here.

(defun zenirc-trigger-make-command (name response)
  (let* ((symname (format "zenirc-command-%s" name))
         (cmdsym (intern symname))
         (cmdhook (intern (concat symname "-hook"))))
    (zenirc-add-hook cmdhook cmdsym)
    (fset cmdsym
          (` (lambda (proc victim)
               (zenirc-trigger-send-fn-result proc victim '(, response)))))))

(defun zenirc-trigger-set-trigger (name regexp response &optional pass-string)
  (and (stringp name)
       (setq name (intern name)))
  (let ((elt (assq name zenirc-trigger-table)))
    (cond (elt
           (setcar (nthcdr 1 elt) t)
           (setcar (nthcdr 2 elt) regexp)
           (setcar (nthcdr 3 elt) response)
	   (setcar (nthcdr 4 elt) pass-string))
          (t
           (setq zenirc-trigger-table
                 (cons (list name t regexp response pass-string)
                       zenirc-trigger-table))))))

;; Returns nil if the named trigger doesn't exist, t otherwise.
(defun zenirc-trigger-activate (name state)
  (and (stringp name) (setq name (intern name)))
  (let ((elt (assq name zenirc-trigger-table)))
    (cond (elt
           (setcar (nthcdr 1 elt) state)
           t)
          (t nil))))

(defun zenirc-trigger-enable (name)
  (zenirc-trigger-activate name t))

(defun zenirc-trigger-disable (name)
  (zenirc-trigger-activate name nil))


(defun zenirc-server-PRIVMSG-trigger (proc parsedmsg)
  (save-match-data
    (let ((case-fold-search t)
          (trigger-table zenirc-trigger-table)
          regexp fn)
      (while trigger-table
        (cond ((nth 1 (car trigger-table))
               (setq regexp (nth 2 (car trigger-table)))
               (and regexp
                    (string-match regexp (aref parsedmsg 3))
                    (let ((victim (zenirc-trigger-parse-sender parsedmsg))
                          (msg (zenirc-trigger-response
                                (nth 3 (car trigger-table))
				(and (nth 4 (car trigger-table))
				     (match-string 0 (aref parsedmsg 3))))))
                      (zenirc-trigger-send-response proc victim msg)))))
        (setq trigger-table (cdr trigger-table))))))

(defun zenirc-trigger-parse-sender (parsedmsg)
  (let ((from (aref parsedmsg 2)))
    (cond ((zenirc-names-equal-p from zenirc-nick)
           (zenirc-extract-nick (aref parsedmsg 1)))
          (t from))))

(defun zenirc-trigger-response (fn msg)
  (cond ((stringp fn) fn)
        (t (if msg
	       (funcall fn msg)
	     (funcall fn)))))

(defun zenirc-trigger-send-fn-result (proc victim fn &optional msg)
  (setq victim (cdr victim))
  (cond ((or (null victim)
             (string= "" victim))
         (setq victim zenirc-current-victim)))
  (zenirc-trigger-send-response proc victim (zenirc-trigger-response fn msg)))

(defun zenirc-trigger-send-response (proc victim msg)
  (cond ((stringp msg)
         (zenirc-message proc 'trigger-sent victim msg)
         (process-send-string proc (concat zenirc-trigger-response-type
                                           " " victim " :" msg "\n")))
        (t
         (while msg
           (zenirc-message proc 'trigger-sent victim (car msg))
           (process-send-string proc
                                (concat zenirc-trigger-response-type
                                        " " victim " :" (car msg) "\n"))
           (setq msg (cdr msg))))))


;; Parser for /trigger command.  This figures out the trigger subcommand
;; and calls the appropriate routine to handle it.
;; The function dispatched should be named "zenirc-trigger-do-FOO-command",
;; where FOO is one of `list', `set', `enable', `disable', etc.
;; With no arguments, lists available subcommands.
(defun zenirc-command-trigger (proc parsedcmd)
  (let* ((cmd (zenirc-parse-firstword (cdr parsedcmd)))
         (fn (intern-soft (concat "zenirc-trigger-do-" (car cmd) "-command"))))
    (cond ((and fn (fboundp fn))
           (funcall fn proc cmd))
          ((null (car cmd))
           (zenirc-message proc 'trigger-subcommands
                           (mapconcat 'identity
                                      (zenirc-trigger-subcommand-list)
                                      ", ")))
          (t
           (zenirc-message proc 'trigger-command-undefined (car cmd))))))

;; Returns a list of defined subcommands to /trigger.
(defun zenirc-trigger-subcommand-list ()
  (save-match-data
    (let* ((prefix "zenirc-trigger-do-")
           (suffix "-command")
           (re (concat suffix "$")))
      (sort (mapcar (function (lambda (s)
                                (substring s (length prefix)
                                           (- (length suffix)))))
              (all-completions prefix obarray
                               (function
                                (lambda (s)
                                  (string-match re (symbol-name s))))))
            'string-lessp))))

(defun zenirc-trigger-do-list-command (proc args)
  (let ((table zenirc-trigger-table))
    (zenirc-message proc 'trigger-list-head)
    (zenirc-message proc 'trigger-list-line)
    (while table
      (zenirc-message proc 'trigger-list-item
                      (nth 0 (car table))
                      (nth 1 (car table))
                      (prin1-to-string (nth 2 (car table)))
                      (prin1-to-string (nth 3 (car table))))
      (setq table (cdr table)))
    (zenirc-message proc 'trigger-list-end)))

(defun zenirc-trigger-do-set-command (proc args)
  (let* ((parsed1 (zenirc-parse-firstword (cdr args)))
         (name (car parsed1))
         (parsed2 (read-from-string (cdr parsed1)))
         (regexp (car parsed2))
         (fn (car (read-from-string (substring (cdr parsed1)
                                               (cdr parsed2))))))
    (zenirc-trigger-register name fn regexp)
    (zenirc-message proc 'trigger-enable name)))

(defun zenirc-trigger-do-delete-command (proc args)
  (let ((names (zenirc-parse-words (cdr args)))
        (known nil)
        (unknown nil)
        elt)
    (while names
      ;; If intern-soft returns nil, assq will return nil.
      (setq elt (assq (intern-soft (car names)) zenirc-trigger-table))
      (if (null elt)
          (setq unknown (cons (car names) unknown))
        (setq zenirc-trigger-table (delq elt zenirc-trigger-table))
        (setq known (cons (car names) known)))
      (setq names (cdr names)))
    (and known
         (zenirc-message proc 'trigger-deleted (nreverse known)))
    (and unknown
         (zenirc-message proc trigger-undefined (nreverse unknown)))))

(defun zenirc-trigger-do-enable-command (proc args)
  (zenirc-trigger-do-activation proc (cdr args) t))

(defun zenirc-trigger-do-disable-command (proc args)
  (zenirc-trigger-do-activation proc (cdr args) nil))

(defun zenirc-trigger-do-activation (proc args state)
  (let ((msg (if state 'trigger-enable 'trigger-disable))
        (names (zenirc-parse-words args))
        (known nil)
        (unknown nil))
    (while names
      (if (zenirc-trigger-activate (car names) state)
          (setq known (cons (car names) known))
        (setq unknown (cons (car names) unknown)))
      (setq names (cdr names)))
    (and known
         (zenirc-message proc msg (nreverse known)))
    (and unknown
         (zenirc-message proc trigger-undefined (nreverse unknown)))))


(provide 'zenirc-trigger)

(zenirc-lang-define-catalog 'english
  '((trigger-sent      . "[trigger] Sent to %s: %s")
    (trigger-enable    . "[info] Triggers enabled: %s")
    (trigger-disable   . "[info] Triggers disabled: %s")
    (trigger-deleted   . "[info] Triggers deleted: %s")
    (trigger-undefined . "[info] Undefined triggers: %s")
    (trigger-list-head . "[trigger] Name       On? Regexp          Function")
    (trigger-list-line . "[trigger] ----       --- ------          --------")
    (trigger-list-item . "[trigger] %-10s %-3s %-15s %s")
    (trigger-list-end  . "[trigger] End of list.")
    (trigger-subcommands . "[info] Trigger subcommands: %s")
    (trigger-command-undefined . "[info] undefined trigger command: %s")))

(zenirc-add-hook 'zenirc-server-PRIVMSG-hook
                 'zenirc-server-PRIVMSG-trigger 'append)

;;; zenirc-trigger.el ends here

