;;; yes or no query in which C-g returns nil, but does not cancel
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
;;;  Because EMACS gets upset when you abort a recursive edit from
;;;  within a filter, this was written to compensate for y-or-n-p so
;;;  that G-g simply says NO instead of exiting.

(defvar etalk-bufferize-yorn nil
  "*Use optional buffer query instead of minibuffer query because
systems can't handle minibuffer prompts from withing filters correctly.")

(defvar etalk-stored-savefunct nil
  "Used locally to remember what to do after someone answers Y to some
request.")

(defvar etalk-yorn-map nil
  "Keymap for etalk-y-or-n-p functions.")

(if etalk-yorn-map
    ()
  (setq etalk-yorn-map (make-keymap))
  (if etalk-18-p
      (fillarray etalk-yorn-map 'etalk-yorn-wrong)
    (fillarray (nth 1 etalk-yorn-map) 'etalk-yorn-wrong))
  (define-key etalk-yorn-map "y" 'etalk-yorn-yes)
  (define-key etalk-yorn-map "n" 'etalk-yorn-no)
  (define-key etalk-yorn-map "Y" 'etalk-yorn-yes)
  (define-key etalk-yorn-map "N" 'etalk-yorn-no)
  (define-key etalk-yorn-map "\C-g" 'etalk-yorn-no))

(defvar etalk-proc-yorn-map nil
  "Keymap for etalk y-or-n-p within a buffer.")

(if etalk-proc-yorn-map
    ()
  (setq etalk-proc-yorn-map (make-keymap))
  (if etalk-18-p
      (fillarray etalk-proc-yorn-map 'etalk-proc-yorn-wrong)
    (fillarray (nth 1 etalk-proc-yorn-map) 'etalk-proc-yorn-wrong))
  (define-key etalk-proc-yorn-map "y" 'etalk-proc-yorn-yes)
  (define-key etalk-proc-yorn-map "n" 'etalk-proc-yorn-no)
  (define-key etalk-proc-yorn-map "Y" 'etalk-proc-yorn-yes)
  (define-key etalk-proc-yorn-map "N" 'etalk-proc-yorn-no)
  (define-key etalk-proc-yorn-map "\C-g" 'etalk-proc-yorn-no))

(defun etalk-bufferized-yorn (savefunct)
  "Place the y/n query into the current talk buffer instead of all
that other gunk."
		      
  (let ((m1 (point)))
    (insert (format "Would you like to use %s with %s?"  
		    savefunct etalk-remote-who))
    (set-marker etalk-point m1) ; we don't want this in the buffer
			       ; long.
    (use-local-map etalk-proc-yorn-map)	;new map to read y/n stuff
    (setq etalk-stored-savefunct savefunct)
    ))		      

(defun etalk-proc-yorn-yes ()
  "Yes answer from talk buffer"
  (interactive)
  ;; do enable thing
  (use-local-map etalk-mode-map)
  (delete-region etalk-point (point-max))
  (etalk-send-output (list (get-buffer-process (current-buffer)))
		     "\03\02y\n")
  (etalk-remote-start-function etalk-stored-savefunct)
  (tyrant-player2)
  (run-hooks 'tyrant-player2-hook)
  (setq tyrant-player1-hook nil)
  (setq tyrant-player2-hook nil)
  (setq etalk-filter-message nil)
  ;; toast that message from before!
  )
(defun etalk-proc-yorn-no ()
  (interactive)
  ;; do no thingy
  (delete-region etalk-point (point-max))
  (use-local-map etalk-mode-map)
  (message "Sending refused requrest.")
  (etalk-send-output (list (get-buffer-process (current-buffer))) 
		     "\03\02nRequest Refused!\n"))

(defun etalk-proc-yorn-wrong ()
  "Message that something is wrong in bufferized yorn."
  (interactive)
  (ding)
  (message "Please press Y or N to talk prompt!"))

(defun etalk-yorn-p (prompt)
  "Substitute for y-or-n-p because a exit-recursive-edit pisses off
emacs when y-or-n-p is called from pipe initiated io."

  (interactive)
  (save-excursion
    (if (equal (length 
		(read-from-minibuffer (concat prompt " (y or n) ")
				      "" etalk-yorn-map nil)) 0)
	nil
      t)))

(defun etalk-yorn-yes ()
  "They said yes!"
  (interactive) (insert "y") (sit-for 0) (exit-minibuffer))

(defun etalk-yorn-no ()
  "They said no!"
  (interactive) (exit-minibuffer))

(defun etalk-yorn-wrong ()
  "Say No No No No!"
  (interactive) (ding) (message "Respond with \"y\" or \"n\" please!"))

;;; end of etalk-yorn
(provide 'etalk-yorn)