;;; special X support for menus etc under etalk
;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
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
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Purpose:
;;;   To hide all window specific things into a sub-file to reduce load time
;;; for someone using a terminal.

(require 'etalk-tyrn)

;;
;; The menu for ETALK LOG
;;
(define-key etalk-log-mode-map [menu-bar] (make-sparse-keymap))
(define-key etalk-log-mode-map [menu-bar etalkm]
  (cons "Etalk" (make-sparse-keymap "Etalk")))
(define-key etalk-log-mode-map [menu-bar etalkm destroy]
  '("Destroy Process" . etalk-kill-process))
(define-key etalk-log-mode-map [menu-bar etalkm quit]
  '("Quit Process" . etalk-send-quit-command))
(define-key etalk-log-mode-map [menu-bar etalkm abort]
  '("Abort Call" . etalk-send-abort-command))
(define-key etalk-log-mode-map [menu-bar etalkm clean]
  '("Clean" . etalk-send-clean-command))
(define-key etalk-log-mode-map [menu-bar etalkm hosts]
  '("Hosts" . etalk-send-host-command))
(define-key etalk-log-mode-map [menu-bar etalkm devices]
  '("Devices" . etalk-send-device-command))
(define-key etalk-log-mode-map [menu-bar etalkm users]
  '("Users" . etalk-send-users-command))
(define-key etalk-log-mode-map [menu-bar etalkm phelp]
  '("Process Help" . etalk-send-help-command))
;; we don't want the edit menu hangin about...
(define-key etalk-log-mode-map [menu-bar edit] 'undefined)

;; Some requirements for active menu items.
(put 'etalk-kill-process 'menu-enable 'etalk-process)
(put 'etalk-send-quit-command 'menu-enable 'etalk-process)
(put 'etalk-send-abort-command 'menu-enable 'etalk-process)
(put 'etalk-send-clean-command 'menu-enable 'etalk-process)
(put 'etalk-send-host-command 'menu-enable 'etalk-process)
(put 'etalk-send-device-command 'menu-enable 'etalk-process)
(put 'etalk-send-users-command 'menu-enable 'etalk-process)
(put 'etalk-send-help-command 'menu-enable 'etalk-process)

;;
;; The menu for etalk connections
;;
(define-key etalk-mode-map [menu-bar] (make-sparse-keymap))
(define-key etalk-mode-map [menu-bar connections]
  (cons "Connections" (make-sparse-keymap "Connections")))
(define-key etalk-mode-map [menu-bar connections hangup]
  '("Hangup" . etalk-x-hangup))
(define-key etalk-mode-map [menu-bar connections logbuff]
  '("Show Log" . etalk-setup-windows-with-log))
(define-key etalk-mode-map [menu-bar connections windows]
  '("Refresh windows" . etalk-setup-windows))
(define-key etalk-mode-map [menu-bar connections insert-file]
  '("Insert File ..." . etalk-insert-file))
(define-key etalk-mode-map [menu-bar connections message]
  '("Send Message ..." . etalk-send-minibuffer-message))
(define-key etalk-mode-map [menu-bar connections other-users]
  '("Other Users" . etalk-remote-multilist))
(define-key etalk-mode-map [menu-bar connections play-game]
  '("Play Game ..." . etalk-x-play-game))
(define-key etalk-mode-map [menu-bar connections call]
  '("Call ..." . etalk))
;; we don't want the edit menu hangin about...
(define-key etalk-mode-map [menu-bar edit] 'undefined)

;; Some requirements for active menu items.
(put 'etalk-nuke-connection 'menu-enable '(and etalk-process etalk-tcp-list))
(put 'etalk-insert-file 'menu-enable 'etalk-tcp-list)
(put 'etalk-send-minibuffer-message 'menu-enable '(and etalk-remote-is-emacs
						       etalk-tcp-list))
(put 'etalk-remote-multilist 'menu-enable '(and etalk-remote-is-emacs
						etalk-tcp-list))
(put 'etalk-initiate-special-function 'menu-enable '(and etalk-remote-is-emacs
							 etalk-tcp-list))
(put 'etalk-x-play-game 'menu-enable '(and etalk-remote-is-emacs
					   etalk-tcp-list))

;;
;; Tyrant mode map menu
;;
(define-key etalk-tyrant-map [menu-bar] (make-sparse-keymap))
(define-key etalk-tyrant-map [menu-bar tyrant]
  (cons "Tyrant" (make-sparse-keymap "Tyrant")))
(define-key etalk-tyrant-map [menu-bar tyrant cancel]
  '("Cancel Game" . etalk-usurp-tyrant-keyed))
(define-key etalk-tyrant-map [menu-bar tyrant message]
  '("Send Message" . tyrant-send-minibuffer-message))
(define-key etalk-tyrant-map [menu-bar tyrant help]
  '("Game Help" . etalk-tyrant-help))
;; we don't want the edit menu hangin about...
(define-key etalk-tyrant-map [menu-bar edit] 'undefined)

;;
;; Special menu function designed for lists of various things.
;;
(defun etalk-x-list-2-menu (event title list &optional max)
  "Take a list and turn it into a pop-up menu.  It returns an index into
said list.  The list may have anything in it, and they need not be of the
same type.  This functin must be bound to a mouse event."

  (let ((menu))
    (setq menu
	  (cons ""			; single frame
		(list
		 (let ((tail list)
		       (head nil)
		       (i 1))
		   (cons title
			 (progn
			   (while (and tail (or (not max) (<= i max)))
			     (setq head (cons
					 (cons
					  (format "%s" 
						  ; go to smallest element
						  (let ((elt (car tail)))
						    (while (listp elt)
						      (setq elt (car elt)))
						    elt))
					  i)
					 head))
			     (setq i (1+ i))
			     (setq tail (cdr tail)))
			   (reverse head)))))))
    (let ((n (x-popup-menu event menu)))
      (if (integerp n)
	  (1- n)			;the nth starts at 0, we must start
					;at 1, or the first elt returns nil
	nil))))

;;
;; Some functions which use above menu to be bound to menu things.
;;
(defun etalk-x-hangup (event)
  "Called from pull down menu, create a menu of available connections
to be closed."
  (interactive "e")
  ;; if there isn't much about, just hangup,
  (if (<= (length etalk-tcp-list) 1)
     (progn
       (set-buffer (etalk-format etalk-local-buffer-name))
       (etalk-nuke-connection))
    ;; otherwise, lets pop up a menu to decide who to mangle..
    (let ((c (etalk-x-list-2-menu event "Hanup on who?" 
				  (cons "Everyone" etalk-tcp-list))))
      (cond
       ((not c) nil)
       ((= c 0) (etalk-zorch-all-processes)
	(message "Not feeling social anymore?"))
       (t (etalk-nuke-connection (nth (1- c) (car etalk-tcp-list))))))))

(defun etalk-x-play-game (event)
  "Called from a pull down menu, create a menu of available games to play
against somebody else.."
  (interactive "e")
  (let* ((fns (reverse etalk-legal-multiuser-functions))
	 (c (etalk-x-list-2-menu event "What to play?" fns)))
    (cond
     ((not c) nil)
     (t (etalk-initiate-special-function 
	 (car (nth c fns)))))))

;;; end lisp
(provide 'etalk-x)