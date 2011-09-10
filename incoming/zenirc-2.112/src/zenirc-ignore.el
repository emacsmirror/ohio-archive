;;;  zenirc-ignore.el -- ignore module for zenirc.

;; Copyright (C) 1995, 1996 jason@marilyn.oit.umass.edu
;; Copyright (C) 1995, 1996, 1998 Per Persson

;; Author: Jason Bastek <jason@marilyn.oit.umass.edu>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, ignorance
;; Created: 96-04-11

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



;; usage: /ignore [string] {all,message,dcc,ctcp,notice,action}
;;        /unignore [string]

(require 'zenirc)

(defun zenirc-ignore-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((ignore-start-list . "[info] current ignoramuses:")
     (ignore-list . "[info]   %s")
     (ignore-end-list . "[info] (end of list)")
     (ignore-new-entry . "[info] now ignoring %s")
     (ignore-bad-command . "[info] bad ignore command.")
     (ignore-bad-keyword . "[info] unknown ignore keyword: %s")
     (ignore-usage . "[info] usage: /ignore thing {all,message,dcc,ctcp,notice,action}")
     )))

;; this is not buffer local by default, with zenirc-ignore.el it makes
;; more sense to have it buffer local
(make-variable-buffer-local 'zenirc-ignore-list)

(defvar zenirc-command-ignore-hook '(zenirc-process-ignore))
(defvar zenirc-command-unignore-hook '(zenirc-remove-from-ignore-list))

(defun zenirc-process-ignore (proc parsedcmd)
  "Add someone to the list of ignoramuses."
  (let ((lst (zenirc-parse-words (cdr parsedcmd))))
    (cond ((> (length lst) 2)
	   (zenirc-message proc 'ignore-bad-command))
	  ((= (length lst) 2)
	   (let* ((thing-to-ignore (regexp-quote (car lst)))
		  (ignore-keyword (car (cdr lst)))
		  (ignore-prefix
		   (zenirc-ignore-determine-prefix thing-to-ignore))
		  (ignore-keyword
		   (zenirc-ignore-determine-keyword ignore-keyword))
		  (ignore-string (concat ignore-prefix " " ignore-keyword)))
	     (if (null ignore-keyword)
		 (zenirc-message proc 'ignore-bad-keyword
				 ignore-keyword)
	       (zenirc-message proc 'ignore-new-entry ignore-string)
	       (setq zenirc-ignore-list
		     (cons ignore-string zenirc-ignore-list)))))
	  ((= (length lst) 1)
	   (zenirc-message proc 'ignore-usage))
	  (t
	   (zenirc-ignore-display-ignore-list proc)))))

(defun zenirc-ignore-determine-prefix (str)
  "Take the first thing typed by the user at an /ignore command, and
figure out what it's supposed to translate to.  For now, this means
that if the string doesn't look like a hostname, it's treated as a
nickname.  Otherwise, it's treated as a hostname."
  (cond ((string-match "!" str) ;; nick!user@host, don't touch it
	 str)
	((string-match "@" str)
	 (concat "^:.*!.*" str))
	(t
	 (concat "^:" str "!.*@.*"))))

(defun zenirc-ignore-determine-keyword (str)
  "Take the second thing typed by the user at an /ignore command, and
figure out what it translates to.  For now, it can be one of:

   all message dcc ctcp notice action

This returns nil if the keyword was unknown."
  (interactive)
  (let ((keyword
	 (cond ((string= str "all")     "")
	       ((string= str "message") "PRIVMSG")
	       ((string= str "dcc")     "PRIVMSG[^:]+:DCC")
	       ((string= str "ctcp")    "PRIVMSG[^:]+:")
	       ((string= str "notice")  "NOTICE")
	       ((string= str "action")  "PRIVMSG[^:]+:ACTION")
	       (t nil))))
    keyword))

(defun zenirc-ignore-display-ignore-list (proc)
  "show the current ignore list."
  (zenirc-message proc 'ignore-start-list)
  (let ((lst zenirc-ignore-list))
    (while lst
      (progn (zenirc-message proc 'ignore-list (car lst))
	     (setq lst (cdr lst))))
    (zenirc-message proc 'ignore-end-list)))

;; this removes string from zenirc-ignore-list in two cases
;; 1) if the string is an exact match
;; 2) if the string doesn't include any special regexp chars and it is
;;    a partial match
;; the reason to this is withheld because of two reasons
;; 1) the explanation would be to long
;; 2) it works
(defun zenirc-remove-from-ignore-list (proc parsedcmd)
  "Remove someone from the ignore list."
  (let ((lst (zenirc-parse-words (cdr parsedcmd))))
    (while lst
      (let ((someone (car lst)))
	(zenirc-ignore-for-each 
	 zenirc-ignore-list
	 '(lambda (x)
	    (if (or (string-equal someone x)
		    (string-match (regexp-quote someone) x))
		(setq zenirc-ignore-list (delq x zenirc-ignore-list)))))
	(setq lst (cdr lst))))
    (zenirc-ignore-display-ignore-list proc)))

(defun zenirc-ignore-for-each (lst proc)
  (if lst
    (progn (funcall proc (car lst))
	   (zenirc-ignore-for-each (cdr lst) proc))))

(define-key zenirc-mode-map "\C-c\C-i" 'zenirc-ignore-last-sender)

;; this code was originally written by
;; Richard Todd <rmtodd@essex.ecn.uoknor.edu>
(defun zenirc-ignore-last-sender ()
  (interactive)
  (let ((ignoree (zenirc-extract-userhost zenirc-privmsg-last-seen)))
    (zenirc-message (current-buffer) 'ignore-new-entry ignoree)
    (setq zenirc-ignore-list (cons (regexp-quote ignoree) zenirc-ignore-list))))

(provide 'zenirc-ignore)

(zenirc-ignore-install-message-catalogs)

;; zenirc-ignore.el ends here
