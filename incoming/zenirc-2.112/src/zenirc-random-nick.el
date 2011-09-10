;;;
;;;
;;; zenirc-random-nick.el --- Choose random nicks for ZenIRC

;;; Copyright (C) 1994 Ben A. Mesander
;;; Copyright (C) 1994, 1996, 1997 Per Persson

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;;         Per Persson <pp@sno.pp.se>
;;; Maintainer: pp@sno.pp.se
;;; Keywords: extensions
;;; Created: Sun Aug 14 20:20:05 MDT 1994

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

;;; This is really annoying.

;;; Code:

(require 'zenirc)

(random t)

;; note- the rfc lies about valid characters in nicks.
;; it appears the allowed character set is:
;; [0-9][A-^][a-|] and - and _
;; 0-9 and - cannot start a nick

(defun zenirc-random-nick-char ()
  (let ((c (abs (random 69)))) ; mmm, 69
    (cond ((= 0 c) "_") 
	  ((< c 10) (char-to-string (+ c 47))) ; 0-9
	  ((< c 40) (char-to-string (+ c 55))) ; A-^
	  (t (char-to-string (+ c 57))))))     ; a-|

(defun zenirc-random-nick-string (numchars)
  (let ((i 1) (str (zenirc-random-nick-char)))
    (if (< numchars 0)
	""
      (while (and (< (string-to-char str) 58)  ; can't start with
		  (> (string-to-char str) 47)) ; [0-9]
	(setq str (zenirc-random-nick-char)))
      (while (< i numchars)
	(setq str (concat str (zenirc-random-nick-char)))
	(setq i (1+ i)))
      str)))

(defun zenirc-random-nick ()
  (setq zenirc-nick (zenirc-random-nick-string 9)))

;;; Code to automatically change nickname every now and then, this is
;;; _really_ annoying. mmm, annoying code.
;;;
;;;		<poxaV> yes, i agree, it's annoying.
;;;
(defvar zenirc-change-nick-interval '(0 600)
  "How often to change your random nickname. The default is 600
seconds or 10 minutes.")
(make-variable-buffer-local 'zenirc-change-nick-interval)

(defvar zenirc-last-nick-change '(0 0)
  "The time the last change was made in a ZenIRC buffer.")
(make-variable-buffer-local 'zenirc-last-nick-change)

;; Should this really be optional? B-)
(defvar zenirc-change-nick-mode nil
  "If zenirc-random-nick should change nickname automatically.")
(make-variable-buffer-local 'zenirc-change-nick-mode)

(defun zenirc-change-random-nick (proc now)
  "Change nickname in the specified process PROC every 
zenirc-change-nick-interval seconds."
  (if zenirc-change-nick-mode
      (if (zenirc-time< zenirc-change-nick-interval
			(zenirc-time-diff now zenirc-last-nick-change))
	  (progn
	    (process-send-string proc (concat 
				       "NICK " 
				       (zenirc-random-nick-string 9)
				       "\n"))
	    (setq zenirc-last-nick-change now)))))

(zenirc-add-hook 'zenirc-timer-hook 'zenirc-change-random-nick)
(zenirc-add-hook 'zenirc-mode-hook 'zenirc-random-nick)

(provide 'zenirc-random-nick)

;;; End of zenirc-random-nick.el
