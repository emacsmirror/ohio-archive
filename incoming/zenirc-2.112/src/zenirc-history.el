;;; zenirc-history.el --- keep a history of commands in ZenIRC

;; Copyright (C) 1996, 1998 Per Persson

;; Author: Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, history
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


(require 'zenirc)


;; default is "/server" because that's probably the first thing you want to do
(defvar zenirc-history-list '("/server")
  "A list of commands run by the user.")
(make-variable-buffer-local 'zenirc-history-list)

;; hairy variables to keep track of commands
(defvar zenirc-history-list-backward nil)
(make-variable-buffer-local 'zenirc-history-list-backward)
(defvar zenirc-history-list-forward nil)
(make-variable-buffer-local 'zenirc-history-list-forward)
(defvar zenirc-history-list-current nil)
(make-variable-buffer-local 'zenirc-history-list-current)

;; reset hairy variables when a new command is sent to the server
(defun zenirc-history-command (foo bar command)
  (setq zenirc-history-list-backward nil
	zenirc-history-list-forward nil
	zenirc-history-list-current nil
	zenirc-history-list (cons command zenirc-history-list)))

;; step up one entry in the history list
(defun zenirc-history-backward ()
  (interactive)
  (if (not zenirc-history-list-backward)
      ; initialize variables if their reset
      (setq zenirc-history-list-backward zenirc-history-list
	    zenirc-history-list-current 	     
	    (buffer-substring zenirc-process-mark (point-max))
	    zenirc-history-list-forward zenirc-history-list-forward))
  ; remove contents of line
  (beginning-of-line)
  (if (not (= (point) (point-max)))
      (delete-backward-char (- (point) (point-max))))
  ; insert previous command
  (insert (car zenirc-history-list-backward))
  ; update hairy variabels
  (setq zenirc-history-list-forward (cons 
				     zenirc-history-list-current 
				     zenirc-history-list-forward)
	zenirc-history-list-current (car zenirc-history-list-backward) 
	zenirc-history-list-backward (cdr zenirc-history-list-backward)))

;; step down one entry in the history list
(defun zenirc-history-forward ()
  (interactive)
  (if (not zenirc-history-list-forward)
      ; reset variables
      (setq zenirc-history-list-backward nil
	    zenirc-history-list-forward nil)
    ; remove contents of line
    (beginning-of-line)
    (if (not (= (point) (point-max)))
	(delete-backward-char (- (point) (point-max))))
    ; insert next command
    (insert (car zenirc-history-list-forward))
    ; update hairy variables
    (setq zenirc-history-list-backward (cons 
					zenirc-history-list-current
					zenirc-history-list-backward)
	  zenirc-history-list-current (car zenirc-history-list-forward)
	  zenirc-history-list-forward (cdr zenirc-history-list-forward))))

(provide 'zenirc-history)

(zenirc-add-hook 'zenirc-send-line-hook 'zenirc-history-command)

(define-key zenirc-mode-map "\M-p" 'zenirc-history-backward)
(define-key zenirc-mode-map "\M-n" 'zenirc-history-forward)

;; zenirc-history.el ends here
