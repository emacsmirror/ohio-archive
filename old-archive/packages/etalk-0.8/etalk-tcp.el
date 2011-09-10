;;; etalk tcp connection support for indivudual connections
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
;;;   This file contains the filter function run on a TCP connection to
;;; the etalk process

;;---------------------------------------------------------
;;  A special function is a /03 (C-c) followed by a function
;;  code.  If a function code is not present, it is interpreted 
;;  as a message.
;;
;;  00 - unusable
;;  01 - Mail/multilink port number for connection
;;  02 - String reply from last query
;;  03 - That persons special name
;;  04 - Y/N query on function use.
;;  05 - Request list of users you are talking to or give one.
;;  06 - Request to talk to person on port
;;  07 - Recieve a file for a second buffer
;;  08 - Startup etalk version message
;;
;;---------------------------------------------------------

(defun etalk-tcp-filter (process output)
  "Filters input from a talk tcp connection for printing, or buffer
control."
  
  (let ((oldbuffer (current-buffer))	; the buffer we were in
	(mvpoint t)			; move point at end of proc
	)
    
    ;; if the buffer hosting this process is gone, terminate session!
    
    (if (not (bufferp (process-buffer process)))
	(progn
	  ;; an impossible situation
	  (process-send-string process 
	   "\03Ooops!  Host emacs buffer deleted, terminating call.\n")
	  (etalk-nuke-connection process)
	  ;; Actually, this can't happen.. Oh well.
	  (error "You zorched a buffer from under a process bonehead!"))
      (set-buffer (process-buffer process)))
    
    (while (> (length output) 0)
      (let ((omd (match-data)))
	(if etalk-filter-message
	    ;; Just before check to make sure that we have a valid type.
	    (progn
	      (if (not etalk-filter-message-type)
		  (progn
		    (setq etalk-filter-message-type (string-to-char output))
		    (setq output (substring output 1))))
	      (if (not (string-match etalk-message-end-regexp output))
		  (progn
		    (setq etalk-filter-message (concat etalk-filter-message
						       output))
		    (setq output ""))
		(setq etalk-filter-message 
		      (concat etalk-filter-message
			      (substring output 0 (match-beginning 1))))
		(setq output (substring output (match-end 1)))
		(cond
		 ((equal etalk-filter-message-type 0)
		  (if etalk-log-all-minibuffer-messages
		      (etalk-log (concat "FROM TCP: " etalk-filter-message)))
		  (if etalk-message-to-minibuffer
		      (if (and (windowp (minibuffer-window))
			       (not (equal (selected-window) 
					   (minibuffer-window))))
			  (message etalk-filter-message))
		    ;; print the message
		    (save-excursion
		      (goto-char etalk-point)
		      (insert etalk-filter-message)
		      (move-marker etalk-point (point)))))
		 ((equal etalk-filter-message-type 1)
		  (etalk-messagetype-1 process oldbuffer mvpoint))
		 ((equal etalk-filter-message-type 2)
		  (etalk-messagetype-2 process oldbuffer mvpoint)
		  (setq mvpoint nil))
		 ((equal etalk-filter-message-type 3)
		  (etalk-messagetype-3 process oldbuffer mvpoint))
		 ((equal etalk-filter-message-type 4)
		  ;; in this, we sometime move point, otherwise no.
		  (setq mvpoint (etalk-messagetype-4 process oldbuffer mvpoint)))
		 ((equal etalk-filter-message-type 5)
		  (etalk-messagetype-5 process oldbuffer mvpoint))
		 ((equal etalk-filter-message-type 6)
		  (etalk-messagetype-6 process oldbuffer mvpoint))
		 ((equal etalk-filter-message-type 7)
		  (etalk-messagetype-7 process oldbuffer mvpoint))
		 ((equal etalk-filter-message-type 8)
		  (etalk-messagetype-8 process oldbuffer mvpoint))
		 (t
		  (message "Unknown filter command.")))
		(setq etalk-filter-message-type 0)
		(setq etalk-filter-message nil)))
	  ;; this is if we don't have a filter message to start with!
	  (if (and (< (length etalk-edit-chars) 3)
		   (not (= (string-to-char output) 3)))
	      ;; edit chars yet, get one, and go around for the rest
	      (progn
		(etalk-read-editchars process (string-to-char output))
		(setq output (substring output 1)))
	    (if (not (string-match 
		      (format etalk-text-end-regexp etalk-edit-chars)
		      output))
		;; if we have no special thingys, insert it!
		(save-excursion
		  (goto-char etalk-point)
		  (if etalk-inserted-file
		      (set-buffer etalk-inserted-file))
		  (insert output)
		  (set-buffer (process-buffer process))
		  (setq output "")
		  (move-marker etalk-point (point)))
	      ;; else, insert what we have, and continue from there!
	      (save-excursion
		(goto-char etalk-point)
		(if etalk-inserted-file
		    (set-buffer etalk-inserted-file))		
		(insert (substring output 0 (match-beginning 1)))
		(set-buffer (process-buffer process))
		(move-marker etalk-point (point))
		(setq output (substring output (match-beginning 1))))
	      (let ((tchar (string-to-char output)))
		(setq output (substring output 1))
		(cond
		 ((= tchar 3)
		  (setq etalk-filter-message "")
		  (if (> (length output) 0)
		      (if (<= (string-to-char output) etalk-max-message-types)
			  (progn
			    (setq etalk-filter-message-type
				  (string-to-char output))
			    (setq output (substring output 1)))
			(setq etalk-filter-message-type 0))
		    ;; in case output is split somehow.
		    (setq etalk-filter-message-type nil)))
		 ;; last but not least, if no messages collecting, and no
		 ;; edit chars being waited for, by golly, do something with 
		 ;; the input
		 ;; delete character is character[0] in string	   
		 ((= (aref etalk-edit-chars 0) tchar)
		  (save-excursion
		    (goto-char etalk-point)
		    (etalk-backward-delete-not-past-eob)
		    (move-marker etalk-point (point))))
		 ;; delete line is character 2
		 ((= (aref etalk-edit-chars 1) tchar)
		  (save-excursion
		    (goto-char etalk-point)
		    (etalk-delete-backward-to-char-or-bol nil)
		    (move-marker etalk-point (point))))
		 ;; delete word is character 3
		 ((= (aref etalk-edit-chars 2) tchar)
		  (save-excursion
		    (goto-char etalk-point)
		    (etalk-delete-word-backwards-not-past-eob)
		    (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))
		    (move-marker etalk-point (point))))
		 ;; what about ctrl-l?  Throw it away!
		 ((= tchar 12) 
		  (message "%s's screen was dirty. Should be ok now." 
			   etalk-remote-who))
		 ;; what about ctrl-g?  BEEP! if emacs, else insert.
		 ((= tchar 7)
		  (if etalk-remote-is-emacs
		      (ding)
		    (save-excursion
		      (goto-char etalk-point)
		      (insert (char-to-string tchar))
		      (move-marker etalk-point (point)))))
		 ;; otherwise, something is really weird
		 (t
		  (message "Something weird happened.")))))))
	(store-match-data omd)))
    
    (if mvpoint
	(progn
	  (set-buffer oldbuffer)
	  (etalk-move-realpoint (process-buffer process) 
			       (save-excursion
				 (set-buffer (process-buffer process))
				 etalk-point))))))

(defun etalk-messagetype-1 (process oldbuffer mvpoint)
  "This function is called when the message type is 1.  If multilink host 
exists, then we want to generate a connection to someone.  Else if
the number is 0, generate an error, or create the mail a message buffer."

  (let ((name etalk-remote-who)
	(where etalk-remote-where)
	(mess etalk-filter-message))
    (setq etalk-filter-message-type 0)
    (setq etalk-filter-message nil)
    ;; The following code may be obsolete.  the variable multilink-host
    ;; only occurs here.  If this causes no problems, I shall delete it
;    (if (and (boundp 'etalk-multilink-host) etalk-multilink-host)
	;; this is when someone has used talk-mail
	;; protocol to talk to someone on a
	;; different machine
;	(progn
;	  (etalk-send-output 
;	   (list etalk-multilink-host)
;	   (format "\03\05%s %s\n" 
;		   (etalk-format "%u@%m %t" etalk-remote-who etalk-remote-where 
;				etalk-remote-tty) 
;		   mess))
;	  (setq etalk-multilink-host nil))
      ;; this is normal talk-mail
    (if (equal (string-to-int mess) 0)
	(message "Mail Port Error!  Could be tyrant trouble.")
      (set-buffer oldbuffer)
      (etalk-mail-portnumber (string-to-int mess) name where)
      ;; mvpoint is local to the filter.  this is only called from
      ;; the tcp filter.
      (setq mvpoint nil)
      (message "Complete mail message.  C-c C-c to send."))))

(defun etalk-messagetype-2 (process oldbuffer mvpoint)
  "what happens when the message type is 2.  This is a yes no query
to play some game against the opponant."
  (let ((savefunct etalk-filter-message)
	(savereqfnc etalk-special-request-function))
    (setq etalk-filter-message-type 0)
    (setq etalk-filter-message nil)
    (setq etalk-special-request-function nil)
    (if savereqfnc (funcall savereqfnc savefunct)
      (message "He said %s." savefunct))
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	(progn
	  (tyrant-player1)
	  (run-hooks 'tyrant-player1-hook)
	  (setq tyrant-player1-hook nil)
	  (setq tyrant-player2-hook nil)))))

(defun etalk-messagetype-3 (process oldbuffer mvpoint)
  "This is what is run when the message type is 3.  This is usually at
startup and should parse for such things as someone's finger name."
  ;; this is the occurance early on when we want to
  ;; get the persons cool name
  (setq etalk-remote-preferred-name etalk-filter-message)
  (if etalk-remote-display-preferred-name
      (setq mode-line-buffer-identification 
	    (list etalk-version ": %15b " 
		  (etalk-format etalk-remote-display-preferred-name))))
  (set-buffer-modified-p (buffer-modified-p))
  (setq etalk-filter-message-type 0)
  (setq etalk-filter-message nil))

(defun etalk-messagetype-4 (process oldbuffer mvpoint)
  "Procedure run when the messagetype is 4.  This is a yes-no query to
play a game usually."
  (let ((savefunct etalk-filter-message)
	(mvpoint t))
    (if (etalk-verify-multiuser-function savefunct)
	(progn
	  (setq etalk-filter-message nil)
	  ;; we have a real function
	  (if (and (and (windowp (minibuffer-window)) 
			(not (equal (selected-window) (minibuffer-window))))
		   (not etalk-bufferize-yorn))
	      ;; use special talk yes no to avoid the
	      ;; use of abort-recursive-edit which makes
	      ;; talk totally flip out.  Bug in emacs? dunno. 
	      (if (etalk-yorn-p
		   (format "Would you like to use %s with %s?"  
			   savefunct etalk-remote-who))
		  ;; respond according to answer and fire
		  ;;  up special thing.
		  (progn
		    (etalk-send-output (list process) "\03\02y\n")
		    (etalk-remote-start-function savefunct)
		    (tyrant-player2)
		    (run-hooks 'tyrant-player2-hook)
		    (setq tyrant-player1-hook nil)
		    (setq tyrant-player2-hook nil)
		    ;; change back to this buffer later!
		    (setq oldbuffer (current-buffer))
		    (setq mvpoint nil))
		;; Say no becuase user hit "N"
		(etalk-send-output (list process) "\03\02nRequest refused.\n"))
	    ;; this event if user is using minibuffer, or
	    ;; person wants to use buffer queries on purpose.
	    (message "Query waiting in talk buffer!")
	    (ding)
	    (etalk-bufferized-yorn savefunct)))
      ;; Say no because it is an illegal game
      (etalk-send-output (list process) 
			"\03\02nThat game is not supported by remote.\n"))
    mvpoint))

(defun etalk-messagetype-5 (process oldbuffer mvpoint)
  "when type is 5.  This is run when someone wants to know who you are
talking to, wants to talk to someone you are talking to."
  (if (equal (length etalk-filter-message) 0)
      ;; report back with a list of who i am talking to.
      (let ((pl etalk-tcp-list) (answer ""))
	(while pl
	  (save-excursion
	    (set-buffer (process-buffer (car (car pl))))
	    (if (equal (get-buffer-process (current-buffer)) process)
		()
	      (setq answer (concat answer (etalk-format "%u@%m %t" 
						       etalk-remote-who
						       etalk-remote-where 
						       etalk-remote-tty)
				   ","))))
	  (setq pl (cdr pl)))
	(etalk-send-output (list process) (format "\03\02%s\n" answer)))
    ;; else tell person listed in message that he is
    ;; wanted and the port number.
    (let ((omd (match-data)))
      (if (string-match "\\( [0-9]+\\)" etalk-filter-message)
	  (let* ((port (substring etalk-filter-message (match-beginning 1)
				  (match-end 1)))
		 (addrlst (etalk-parse-address etalk-filter-message))
		 (name (nth 0 addrlst))
		 (mach (nth 1 addrlst))
		 (tty (nth 2 addrlst))
		 (proc2 (get-process (etalk-format 
				      etalk-tcp-string-internal
				      name mach tty))))
	    (if proc2
		(etalk-send-output (list proc2) 
				  (format "\03\06%s %s\n"
					  (etalk-format "%u@%m %t"
						       etalk-remote-who
						       etalk-remote-where
						       etalk-remote-tty)
					  port))
	      (message "%s requested talk to %s who doesn't exist."
		       etalk-remote-who etalk-filter-message)))
	(message "%s requested another user, but no port given."
		 etalk-remote-who))
      (store-match-data omd))))

(defun etalk-messagetype-6 (process oldbuffer mvpoint)
  "This one is when you recieve a message requesting a connection with
someone at a special port number."
  (let ((omd (match-data)))
    (if (string-match "\\( [0-9]+\\)" etalk-filter-message)
	(let* ((ports (substring etalk-filter-message (match-beginning 1)
				 (match-end 1)))
	       (port (string-to-int ports)))
	  ;; we are golden! We have what we need to do
	  ;; something alot like the mail reply thing!
	  ;; So, reuse the stuff!!!!
	  (save-excursion
	    (etalk etalk-filter-message port)))
      (message 
       "Someone requested a talk session, but didn't format it correctlly."))
    (store-match-data omd)))

(defun etalk-messagetype-7 (process oldbuffer mvpoint)
  "file transfer message interpreter"
  ;; In this case, we are about to recieve a file!
  (if etalk-inserted-file
      (progn
	(etalk-setup-windows-plus (get-buffer etalk-inserted-file))
	(set-buffer (process-buffer process))
	(setq etalk-inserted-file nil))
    (setq etalk-inserted-file (concat etalk-remote-who " : "
				     etalk-filter-message))
    (get-buffer-create etalk-inserted-file)
    (save-excursion
      (set-buffer etalk-inserted-file)
      (make-local-variable 'etalk-tag)
      (setq etalk-tag t))
    (message "Recieving file!")
    )
  (setq etalk-filter-message nil)
  (setq etalk-filter-message-type 0)
  )

(defun etalk-messagetype-8 (process oldbuffer mvpoint)
  "set remote's version of ETALK, or any other type which may be usefull"
  (let ((omd (match-data)))
    (if (string-match "\\([a-zA-Z_]+\\) \\([0-9]+\\)\\.\\([0-9]+\\)"
		      etalk-filter-message)
	(progn
	  (aset etalk-remote-type 0 (substring 
				     etalk-filter-message
				     (match-beginning 1)
				     (match-end 1)))
	  (aset etalk-remote-type 1 (string-to-int
				     (substring
				      etalk-filter-message
				      (match-beginning 2)
				      (match-end 2))))
	  (aset etalk-remote-type 2 (string-to-int
				     (substring
				      etalk-filter-message
				      (match-beginning 3)
				      (match-end 3))))
	  (if (string= (aref etalk-remote-type 0) etalk-program-name)
	      (progn
		(setq etalk-remote-is-emacs t)
		;; process is a local variable to tcp-filter
		;; if they are emacs, send out the preferred name type 3
		(etalk-send-output (list process) 
				   (format 
				    "\03\03%s\n" etalk-preferred-name))
		(setq etalk-remote-preferred-name nil)
		;; setup the global version of remote-emacs-p
		(save-excursion 
		  (set-buffer (etalk-format etalk-local-buffer-name))
		  (setq etalk-remote-is-emacs (etalk-all-emacs-p)))
		;; Tell our sub-process that he is an emacs client
		(etalk-send-command 
		 (format "client %d ETALK" 
			 (cdr (etalk-process-tuple process))))
		(message "Remote is using emacs too!")))))
    (store-match-data omd)))

(defun etalk-read-editchars (process tchar)
  "Read in some edit characters, and then ship out our version."
  (setq etalk-edit-chars (concat etalk-edit-chars (char-to-string tchar)))
  (if (= (length etalk-edit-chars) 3)
      (save-excursion 
	(message "Remote has connected!")
	(set-buffer (etalk-format etalk-local-buffer-name))
	(setq etalk-remote-is-emacs (etalk-all-emacs-p))
	;; ship out the version string.  will be icky for some, but oh
	;; well!  That is the price of progress. \010 is really 8 BTW
	;;
	;; Just after sending type, send KILL_LINE keyletter.  This
	;; will make the message dissapear on normal talk systems.
	;;
	;; We must swap back to prev buffer to access, however.
	(etalk-send-output (list process) 
			   (format "\03\010%s%c\n" 
				   etalk-version
				   (aref etalk-edit-characters-mine 1)))
	(ding))))

;;; end lisp
(provide 'etalk-tcp)