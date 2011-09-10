;;;
;;;
;;; zenirc-command-queue.el --- Schedule commands for ZenIRC.

;;; Copyright (C) 1993, 1994 Ben A. Mesander

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;; Maintainer: ben@gnu.ai.mit.edu
;;; Keywords: extensions
;;; Created: 1994/07/20

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

;;; ircd 2.8 implements something called "flood control" that knocks you
;;; off of IRC if you send commands at a rate faster than .5 lines/sec.
;;; This ZenIRC module implements a queue of commands to be sent to the
;;; server and attempts to send them at a rate slower than this. In addition,
;;; you can place lisp code to be executed in the queue (to allow for 
;;; synchronization, like "execute this lisp code after all these lines have
;;; been sent to the server".) The queue is stored as a simple list in
;;; zenirc-command-queue. Note that since ZenIRC is event driven, commands
;;; may be executed no faster than the server PING time (which is usually
;;; several minutes). 

;;; Code:

(require 'zenirc)

;;;###autoload
(defvar zenirc-command-queue nil "List of commands to send to server")

;;;###autoload
(defvar zenirc-last-command-queue-exec '(0 0)) ; time last cmd sent to server

(zenirc-add-hook 'zenirc-timer-hook 'zenirc-handle-command-queue)

;;
;; queue a command to be sent to the server
;;
;;;###autoload
(defun zenirc-queue-command (command)
  (setq zenirc-command-queue (cons command zenirc-command-queue)))

;;
;; figure out how many commands to send to the server
;;
;;;###autoload
(defun zenirc-handle-command-queue (proc now)
  (let ((interval (zenirc-time-diff now zenirc-last-command-queue-exec)))
    (and zenirc-command-queue
	 (cond
	  ;; 8 or more seconds have passed - send four commands
	  ((zenirc-time< '(0 7) interval)
	   (zenirc-exec-command-queue proc now 4))
	  ;; six or seven seconds have passed - send three commands
	  ((zenirc-time< '(0 5) interval)
	   (zenirc-exec-command-queue proc now 3))
	  ;; four or five seconds have passed - send two commands
	  ((zenirc-time< '(0 3) interval)
	   (zenirc-exec-command-queue proc now 2))
	  ;; two or three seconds have passed - send one command
	  ((zenirc-time< '(0 1) interval) 
	   (zenirc-exec-command-queue proc now 1))))))

;;
;; remove commands from the queue, and send them to the server
;; execute NUM items from zenirc-command-queue that involve sending a message
;; to the server - any number of lisp forms in zenirc-command-queue may be
;; executed (lisp forms also don't reset the last exec time).
;;
;;;###autoload
(defun zenirc-exec-command-queue (proc now num)
  (let ((len) (qentry)
	(sent-to-server 0))
    (while (and zenirc-command-queue (<= sent-to-server num))
      (if (listp
	   (setq entry (nth (1- (setq len (length zenirc-command-queue)))
			    zenirc-command-queue)))
	  (eval entry)
	(process-send-string proc entry)
	(setq sent-to-server (1+ sent-to-server)
	      zenirc-last-command-queue-exec now))
      ;; remove from queue
      (if (eq len 1)
	  (setq zenirc-command-queue nil)
	(setcdr (nthcdr (- len 2) zenirc-command-queue) nil)))))
	  
(provide 'zenirc-command-queue)

;;; End of zenirc-command-queue.el
