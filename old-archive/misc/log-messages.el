;From rpotter@grip.cis.upenn.edu Sun Nov  4 00:59:02 1990
;From: rpotter@grip.cis.upenn.edu (Robert Potter)
;Subject: Re: repeating last message
;Date: 18 Oct 90 23:31:00 GMT
;Organization: GRASP Lab
;Status: RO
;
;
;Douglis) writes:
;> to my knowledge, there's no way to get emacs to redisplay a message.
;> i saw some stuff in the elisp archive for "meshook", which saves
;> messages in a buffer, but it seems to rely on another package "hooks"
;> that isn't in the archive.  anyone have anything that saves/displays
;> old messages, or a pointer to "hooks"?
;
;
;I guess I'll take this opportunity to get around to posting this
;package I whipped up last month.  It's probably a reinvented wheel,
;but at least it seems to work well.
;
;	-Robert
;

;; -*-Emacs-Lisp-*-

;;; log-messages.el - facility to log the output of the "message" function.
;;; 
;;; Have you ever been frustrated by not being able to read an emacs message
;;; because it gets overwritten by another message?  Then this package is for
;;; you.  By setting the user variable "log-messages" to non-nil, you can have
;;; the messages logged for you in the buffer "*Message Log*".
;;; 
;;; The variable "non-logged-message-regexps" lets you filter out messages you
;;; know you don't want logged.  I'm not sure how useful this really is, but
;;; it's there if you want it.
;;; 
;;; Unfortunately, some messages you see in the minibuffer are not produced
;;; using the "message" function, so they will not be logged.
;;; 
;;; CAUTION: This file overwrites the function "message".

;;; Copyright (C) 1990 Robert Potter.
;;;
;;; Author: Robert Potter (rpotter@grip.cis.upenn.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author or from the Free Software Foundation, Inc., 675 Mass Ave,
;;; Cambridge, MA 02139, USA.

(require 'cl)
(provide 'log-messages)

;;
;; user variables
;; 

(defvar log-messages t
  "*If non-nil, the output of the \"message\" function will be logged in the
buffer \"*Message Log*\".")

(defvar non-logged-message-regexps
  '("Mark set"
    "Undo!"
    "\\(Failing \\|Wrapped \\)?I-search\\( backward\\)?: .*"
    "I-search\\( backward\\)?: .*"
    "(No changes need to be saved)"
    "<<< Press Space to bury the buffer list >>>"
    ""
    "fill-prefix: \".*\""
    "Type .* to restore old contents of help window\\."
    "Type .* to continue editing\\.")
  "*A list of regexps that match messages to leave out of the message log.
For best efficiency, keep them in decreasing order of likelihood.")


;;
;; specific code
;;

; save the old version of "message" in "message-no-log" before defining the new
; one
(when (not (fboundp 'message-no-log))
  (setf (symbol-function 'message-no-log)
	(symbol-function 'message)))

(defun message (&rest ARGS)
  "Print a one-line message at the bottom of the screen.
The first argument is a control string.
It may contain %s or %d or %c to print successive following arguments.
%s means print an argument as a string, %d means print as number in decimal,
%c means print a number as a single character.
The argument used by %s must be a string or a symbol;
the argument used by %d or %c must be a number.

This version will log the message in the buffer \"*Message Log*\" if the
variable \"log-messages\" is non-nil.  Use \"message-no-log\" if you really
want the old version."
  (let ((msg (apply 'format ARGS)))
    (when (and log-messages
	       (not (some (function (lambda (REGEXP)
				      (and (eq 0 (string-match REGEXP msg))
					   (= (match-end 0) (length msg)))))
			  non-logged-message-regexps)))
      (save-excursion
	(set-buffer (get-buffer-create "*Message Log*"))
	(goto-char (point-max))
	(insert msg ?\n)
	(sit-for 0)))
    (message-no-log "%s" msg)))

