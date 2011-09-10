;;; etalk special function support, etalk only functions
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
;;; Prupose:
;;;   To have functions specific to tyrant mode and other special actions
;;; which can occur between to emacs talk processes.

(require 'etalk-lgame)

(defvar etalk-special-application-function nil
  "used to keep track of special app functions for multi-user programs
during the startup (y/n querys) of the application.")

(defvar etalk-remote-remote-list nil
  "when reading in multiple users that someone else is talking to, use
this variable locally to keep track of them and query user.")

(defvar etalk-tyrant-return-buffer nil
  "Used locally to remember what buffer to return to after this buffer
is usurped.")
	
(defvar etalk-tyrant-imprisoned-process nil
  "Used locally to hold the id of the talk procces being used as the
'opponant' in a game.")

(defvar etalk-tyrant-imprisoned-user nil
  "Used locally to hold the username of the remote person")

(defvar etalk-tyrant-imprisoned-preferred-name nil
  "Used locally to hold the preferred name of the remote person")

(defvar tyrant-opponent-type nil
  "Used locally to define the type of opponant.  the value 'etalk
means a talk process, nil means etalk-ai, and it should be fset to
some value defining the AI logic.")

(defun etalk-unique-remote-p ()
  "Returns process if in local and only one remote exists, of if default is
a remote buffer. [ie. if only 1 process recipient of typed key.]"

  (if (equal (length etalk-tcp-list) 1)
      (car (car etalk-tcp-list))
    (if (equal mode-name etalk-remote-mode-string)
	(get-buffer-process (current-buffer))
      nil)))

(defun etalk-other-emacs-p (process)
  "Returns t if remote is emacs, nil if not."
  (save-excursion
    (set-buffer (process-buffer process))
    etalk-remote-is-emacs))

(defun etalk-send-minibuffer-message (mess)
  "Read message in the minibuffer, and send only if other is using
emacs."
  (interactive "sMessage: ")
  (let ((pl (list (etalk-unique-remote-p))))
    (if (and (etalk-other-emacs-p (car pl)) (car pl))
	(etalk-send-output pl (format "\03%s: %s\n" etalk-announce-as mess))
      (error "Can't send message to that process."))))

(defun etalk-hug-remote ()
  "Send a *HUG* as a minibuffer message to remote."
  (interactive)
  (etalk-send-minibuffer-message "*HUG*"))

(defun etalk-answer-to-minibuffer (answer)
  "Simple function used to print answers to question if used as the
etalk-special-request-function for debug purposes" 

  (message (format "The answer is %s." answer)))

(defun etalk-y-or-n-p (promptf &optional function)
  "Ask the remote yes or no question."

  (interactive "sPrompt for: ")
  (let ((pl (list (etalk-unique-remote-p))))
    (if (and (etalk-other-emacs-p (car pl)) (car pl))
	(save-excursion
	  (set-buffer (process-buffer (car pl)))
	  (setq etalk-special-request-function 
		(if function
		    function
		  'etalk-answer-to-minibuffer))
	  (etalk-send-output pl (format "\03\04%s\n" promptf)))
      (error "Can't prompt that process."))))

(defun etalk-initiate-special-function (function)
  "This function initializes a special function of some sort which
requires someone to answer yes or no before starting."

  (interactive 
   (let ((completion-ignore-case t))
     (list (completing-read "Function: " etalk-legal-multiuser-functions
			    nil t ""))))
  (save-excursion
    (set-buffer (process-buffer (etalk-unique-remote-p)))
    (setq etalk-special-application-function function))
  (etalk-y-or-n-p function 'etalk-initiate-special-function-answered)
  (message "Query sent."))

(defun etalk-initiate-special-function-answered (answer)
  "This function is the second half of talk-initiate-special-function
which continues on after getting the answer."

  (if (equal (string-to-char answer) ?y)
      (etalk-remote-start-function etalk-special-application-function)
    (message (substring answer 1 (length answer)))))

(defun etalk-verify-multiuser-function (function)
  "Check function against the list of two person functions available
to protect agains weirdness."

  ;; technically assq would return if it was there, but I want to pass
  ;; it back. 
  (let ((l etalk-legal-multiuser-functions)
	(ftr nil))
    (while l
      (if (equal (car (car l)) function)
	  (setq ftr (cdr (car l))))
      (setq l (cdr l)))
    ftr))

(defun etalk-remote-start-function (function)
  "Send a start function request to play games etc.  Upon receiving a
y from the remote, this function starts the local copy of function.
The remote has already double checked the startup function, and let
the user decide what to do."

  (require 'etalk-tyrn)

  ;; Whats going on here?!  AGH!
  ;; Well, we only get here from talk-filter-proc because for the
  ;; initiator, he is waiting for a responce!  For the recipient, he
  ;; just said yes to something from the filter!  Therefor, since it
  ;; isn't interactive, we can only get here from the filter and the
  ;; filter always swaps buffers to that of the recieving process
  ;; buffer.
  (let ((proc (get-buffer-process (current-buffer)))
	(who etalk-remote-who)
	(name etalk-remote-preferred-name)
	(tmp-buff (current-buffer))
	(funct nil))

    (if (setq funct (etalk-verify-multiuser-function function))
	(progn
	  ;; We need to do this to clean up the environment of
	  ;; icky functions which seem to follow you around now
	  ;; that we are running in emacs 19
	  (if (boundp 'etalk-tyrant-quit-string)
	      (progn
		(makunbound 'etalk-tyrant-quit-string)
		(fmakunbound 'etalk-tyrant-quit-string)))

	  ;; run the special mode here.
	  (require funct (format "games/%s" funct))
	  (funcall funct)

	  ;; did special mode do something?
	  (if (equal major-mode 'etalk-mode)
	      (error "Misdirected special function."))

	  ;; make a local variable on where the buffer is.
	  (make-local-variable 'etalk-tyrant-return-buffer)
	  (setq etalk-tyrant-return-buffer tmp-buff)
	
	  (make-local-variable 'etalk-tyrant-imprisoned-process)
	  (setq etalk-tyrant-imprisoned-process proc)

	  (make-local-variable 'etalk-tyrant-imprisoned-user)
	  (setq etalk-tyrant-imprisoned-user who)

	  (make-local-variable 'etalk-tyrant-imprisoned-preferred-name)
	  (setq etalk-tyrant-imprisoned-preferred-name name)

	  (make-local-variable 'tyrant-opponent-type)
	  (setq tyrant-opponent-type 'etalk)

	  (etalk-tyrannical-mode)
	  )
      (error "No such function %s!" function)
      )))

(defun etalk-remote-multilist ()
  "Request a list of users from the remote machine and connect to them
as well."
  (interactive)
  (if (etalk-unique-remote-p)
      (if (etalk-other-emacs-p (etalk-unique-remote-p))
	  (save-excursion
	    (set-buffer (process-buffer (etalk-unique-remote-p)))
	    (setq etalk-special-request-function 'etalk-remote-multilist-answer)
	    (etalk-send-output (etalk-unique-remote-p) "\03\05\n"))
	(error "Selected user is not using emacs talk!"))
    (error "Individual buffer not selected."))
  (save-excursion
    (set-buffer (process-buffer (etalk-unique-remote-p)))
    (make-local-variable 'etalk-remote-remote-list) ;remotes list of remotes
    (setq etalk-remote-remote-list nil)
    (message "Please wait for response...")
    (while (and (not (input-pending-p))
		(not etalk-remote-remote-list))
      (sit-for 1))
    (message "Please wait for response...done")
    (if etalk-remote-remote-list
	(let ((answer etalk-remote-remote-list))
	  (while (> (length answer) 0)
	    (let ((person (if (string-match "\\(,\\)" answer)
			      (substring answer 0 (match-beginning 1))
			    nil)))
	      (setq answer (substring answer (match-end 1) (length answer)))
	      (if person
		  (if (etalk-yorn-p (format "Would you like to talk with %s?" 
					    person))
		      (let ((pl (list (get-buffer-process (current-buffer)))))
			;;set up wait socket
			(etalk person -1)
			;; send them the message to connect to my socket
			;; through the host we used.
			(etalk-send-output 
			 pl (format "\03\05%s %d\n" person
				    etalk-remote-socket))
			))))))
      (setq etalk-remote-remote-list nil)))
  (message "End of list."))

(defun etalk-remote-multilist-answer (answer)
  "Parse an answer of who they are talking to."
  (if (and (boundp 'etalk-remote-remote-list))
      (setq etalk-remote-remote-list answer)))

;;; end of lisp
(provide 'etalk-spec)