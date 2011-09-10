;;; etalk process support, including log buffer
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
;;;  This bit of lisp will do basic process control on the etalk binary,
;;; including such drudgery as sending commands to an existing process,
;;; and creating TCP connections to said binary (which are treated as
;;; processes...)
;;;

;;; History
;;;
;;; eml 8/13/94
;;; Use the function (system-name) to get the address of local box
;;; instead of using "127.0.0.1".  (Using variable etalk-local-host)
;;;

(defvar etalk-process-string-internal "etalk-process"
  "*String used to identify the single running version of etalk.")

(defvar etalk-local-host (system-name)
  "*String/function used to identify the machine to connect to.")

(defvar etalk-process nil
  "This variable will contain the currently running etalk binary process
information.")

(defvar etalk-waiting-process nil
  "This contains the process waiting for a user identifier.")

(defvar etalk-tcp-string-internal "talk-tcp-%u-%M-%T"
  "*String used to identify multiple TCP connections to the talk
binary.  It follows the rules of talk-format.")

(defvar etalk-tcp-list nil
  "List of all open TCP connections.")

(defvar etalk-local-socket nil
  "Integer representing the socket to which we will attach ourselves...")

(defvar etalk-remote-socket nil
  "Integer representing the socket to which others will attach themselves...")

(defvar etalk-text-end-regexp "\\([%s\03\07\14]\\)"
  "This regular expression is used to end normal inserted text for talk buffer.")

(defvar etalk-log-end-regexp "\\([\03\07]\\)"
  "This regular expression is used to end normal inserted text for talk log.")

(defvar etalk-message-end-regexp "\\(\n\\)"
  "Regular expression used to find the end of a message string.")

(defvar etalk-max-message-types 8
  "The maximum number of message types sent between talk programs..")

(defun etalk-char-sendable-string (c)
  "convert char such as ^C into the string ^C."
  (if (equal c ?\C-?)
      "^?"
    (if (<= c ?\C-Z)
	(concat "^" (char-to-string (+ c (- ?A 1))))
      (char-to-string c))))

(defun etalk-start-one-process ()
  "Startup the etalk subprocess, checking to make sure that only one
is currently running..."

  (interactive)
  ;; do nothing if the process already exists...
  (save-excursion
    (if etalk-process
	(if (or (eq (process-status etalk-process) 'exit)
		(eq (process-status etalk-process) 'signal))
	    (progn
	      (setq etalk-process nil)
	      (etalk-zorch-all-processes)
	      (message "The main talk process was dead, replacing."))))
    ;; ok, try again just in case.
    (if etalk-process
	()
      (set-buffer (etalk-log-buffer))
      (setq etalk-process (start-process etalk-process-string-internal
					 (etalk-log-buffer)
					 etalk-process-file))
      (set-process-filter etalk-process 'etalk-log-filter)
      (set-process-sentinel etalk-process 'etalk-log-sentinel)
      (setq mode-line-process '(" %s!"))
      ;; This allows us time to read in the socket identifiers.
      (message "Waiting for talk process to initialize...")
      (sit-for 3)
      ;; don't forget to tell it all about our edit caracters!
      (etalk-send-command 
       (concat "editchar " 
	       (etalk-char-sendable-string (aref etalk-edit-characters-mine 0))
	       (etalk-char-sendable-string (aref etalk-edit-characters-mine 1))
	       (etalk-char-sendable-string (aref etalk-edit-characters-mine 2))))
      ;; Now set your announcement name to subprocess.
      (etalk-send-command (concat "name " etalk-announce-as))
      (message "Waiting for talk process to initialize...done"))))
  
(defun etalk-startup-tcp-connection (socket)
  "Create a new connection through the etalk process to some remote."
  
  ;; WARNING : Do not use EDEBUG to debug this function.  The result
  ;;           is that filters are not sent when io first gets parsed!

  ;; Always call this just to make sure that we have the necessary
  ;; process
  (etalk-start-one-process)
  
  (if (not (stringp etalk-remote-who))
      (error "Required local variables not found in buffer %s"
	     (current-buffer)))

  (if (get-process (etalk-format etalk-tcp-string-internal
				 etalk-remote-who
				 etalk-remote-where
				 etalk-remote-tty))
      ;;
      ;; PERSONAL NOTE:  Eventuall look to see if it is a dead connection
      ;;                 and wipe it out if it is...
      ;;
      (error "You are already talking to them!"))
  
  ;; if we are restarting a process, get a new edit char list.
  (setq etalk-edit-chars "")
  
  (let ((newtcp nil))
    (setq newtcp (open-network-stream (etalk-format etalk-tcp-string-internal
						    etalk-remote-who
						    etalk-remote-where
						    etalk-remote-tty)
				      (current-buffer)
				      ;; This string is the standard loopback
				      ;; Used to use the number "127.0.0.1"
				      ;; but some emacses didn't like
				      ;; that version at all. :(
				      etalk-local-host
				      etalk-local-socket))
    (set-process-filter newtcp 'etalk-tcp-filter)
    (set-process-sentinel newtcp 'etalk-sentinel)
    (setq etalk-waiting-process newtcp)
    (setq etalk-tcp-list (cons (cons newtcp 0) etalk-tcp-list))
    (setq mode-line-process '(" %s!"))
    (etalk-log "Starting wait for user id value\n")
    (message "Waiting for new user id ...")
    (while (and (etalk-process-tuple) (= (cdr (etalk-process-tuple)) 0))
      (sit-for 1))			;no wait, process cant talk
    (message "Waiting for new user id ... done"))
  (if (not (etalk-process-tuple))
      (error "Lisp Error generating connection")
    (if socket
	(if (> socket 0)
	    (etalk-send-command (format "connect %d %d %s@%s %s"
					(cdr (etalk-process-tuple))
					socket
					etalk-remote-who
					etalk-remote-where
					etalk-remote-tty))
	  (etalk-send-command (format "wait %s %s@%s %s"
				      (cdr (etalk-process-tuple))
				      etalk-remote-who
				      etalk-remote-where
				      etalk-remote-tty)))
      (etalk-send-command (format "call %d %s@%s %s"
				  (cdr (etalk-process-tuple))
				  etalk-remote-who
				  etalk-remote-where
				  etalk-remote-tty))))
  (setq etalk-waiting-process nil)
  )

(defun etalk-log-filter (process output)
  "Filters input from a talk process for commands and information."

  (let ((oldbuffer (current-buffer))	; the buffer we were in
	)

    (save-excursion
      (set-buffer (process-buffer process))

      (while (> (length output) 0)
	(let ((oldmatch (match-data)))
	  ;; First, check if we are building a message for the minibuffer.
	  (if etalk-filter-message
	      (if (not (string-match etalk-message-end-regexp output))
		  (progn
		    (setq etalk-filter-message 
			  (concat etalk-filter-message output))
		    (setq output ""))
		(setq etalk-filter-message
		      (concat etalk-filter-message
			      (substring output 0 (match-beginning 1))))
		(setq output (substring output (match-end 1)))
		;; place the message into the log, unless it is flagged.
		(if (/= etalk-filter-message-type 5)
		    (save-excursion
		      (goto-char etalk-point)
		      (insert "MESSAGE : \"" etalk-filter-message "\"\n")
		      (move-marker etalk-point (point-max))))
		(cond
		 ;; unlabeled messages get a 0
		 ((equal etalk-filter-message-type 0)
		  (message etalk-filter-message))
		 ;; reporting socket we will be connecting to
		 ((equal etalk-filter-message-type 1)
		  (setq etalk-local-socket
			(string-to-int etalk-filter-message))
		  (save-excursion
		    (goto-char etalk-point)
		    (insert (format " ** local socket number is %d\n"
				    etalk-local-socket))
		    (move-marker etalk-point (point-max))))
		 ;; reporting socket others will be connecting to
		 ((equal etalk-filter-message-type 2)
		  (setq etalk-remote-socket
			(string-to-int etalk-filter-message))
		  (save-excursion
		    (goto-char etalk-point)
		    (insert (format " ** remote socket number is %d\n"
				    etalk-remote-socket))
		    (move-marker etalk-point (point-max))))
		 ;; reporting a user id struct within the binary.
		 ((equal etalk-filter-message-type 3)
		  (etalk-modify-socket (string-to-int etalk-filter-message)
				       etalk-waiting-process)
		  (save-excursion
		    (goto-char etalk-point)
		    (insert (format " ** new user process tuple is %s\n"
				    (etalk-process-tuple 
				     etalk-waiting-process)))
		    (move-marker etalk-point (point-max))))
		 ;; if we get a message that a connection is closed.
		 ((equal etalk-filter-message-type 4)
		  (let* ((omd (match-data))
			 (id (string-to-int etalk-filter-message))
			 (prc (etalk-process-tuple id)))
		    (if prc 
			(progn
			  (etalk-nuke-connection (car prc))
			  (save-excursion 
			    (goto-char etalk-point)
			    (insert (format " ** deletion of tuple %d\n"
					    (cdr prc)))
			    (move-marker etalk-point (point-max)))))))
		 ;; what abound very common messages?  Ignore special ones
		 ((equal etalk-filter-message-type 5)
		  (message etalk-filter-message))
		 ;; other cases
		 (t
		  (message "Unknown filter command....")))
		(setq etalk-filter-message-type 0)
		(setq etalk-filter-message nil))
	    ;; When there is no filter message yet...
	    (if (not (string-match etalk-log-end-regexp output))
		;; this case, no special message formatting.
		(save-excursion
		  (goto-char etalk-point)
		  (insert output)
		  (setq output "")
		  (move-marker etalk-point (point)))
	      ;; this case, we have something to parse around..
	      (save-excursion
		(goto-char etalk-point)
		(insert (substring output 0 (match-beginning 1)))
		(move-marker etalk-point (point))
		(setq output (substring output (match-beginning 1))))
	      ;; Now, look at the output to see what's there.
	      (let ((tchar (string-to-char output)))
		(setq output (substring output 1))
		(cond
		 ;; if it is a special filter...
		 ((= tchar 3)
		  (setq etalk-filter-message "")
		  (if (<= (string-to-char output) etalk-max-message-types)
		      (progn
			(setq etalk-filter-message-type 
			      (string-to-char output))
			(setq output (substring output 1)))
		    (setq etalk-filter-message-type 0)))
		 ;; if we get a little bell
		 ((= tchar 7)
		  (ding)
		  (setq output (substring output 1)))
		 (t
		  (message "Weird thing happened in log filter..."))))))
	  (store-match-data oldmatch))))

    (set-buffer oldbuffer)
    (etalk-move-realpoint (process-buffer process)
			  (save-excursion
			    (set-buffer (process-buffer process))
			    etalk-point))))

(defun etalk-log (string)
  "Add string to the end of the current log."
  (interactive "sString: ")
  (save-excursion
    (set-buffer (etalk-log-buffer))
    (goto-char etalk-point)
    (insert "lisp: " string)
    (let ((omd (match-data)))
      (if (not (string-match "\n" string)) (insert "\n"))
      (store-match-data omd))
    (move-marker etalk-point (point-max))))

(defun etalk-kill-process ()
  "Last ditch effort to kill a rampant etalk process!!"
  (interactive)
  (delete-process etalk-process))

(defun etalk-send-command-key ()
  "This function will be called from a keypress, and include that onto
the prompt string by suppressing it"
  (interactive)
  (let ((unread-command-char last-input-char))
    (call-interactively 'etalk-send-command)))

(defun etalk-send-quit-command ()
  "Send a quit command to the etalk process..."
  (interactive)
  (etalk-send-command "quit"))

(defun etalk-send-host-command ()
  "Send a host command to the etalk process..."
  (interactive)
  (etalk-send-command "host"))

(defun etalk-send-clean-command ()
  "Send a clean command to the etalk process..."
  (interactive)
  (etalk-send-command "clean"))

(defun etalk-send-help-command ()
  "Send a help command to the etalk process..."
  (interactive)
  (etalk-send-command "help"))

(defun etalk-send-abort-command ()
  "Send a abort command to the etalk process..."
  (interactive)
  (etalk-send-command "abort"))

(defun etalk-send-users-command ()
  "Send a users command to the etalk process..."
  (interactive)
  (etalk-send-command "users"))

(defun etalk-send-device-command ()
  "Send a device command to the etalk process..."
  (interactive)
  (etalk-send-command "device"))

(defun etalk-send-command (command)
  "This function will send one text command to the etalk process,
which is distinct from the tcp processes.  Commands are defined within
the c code in etalk_cmd.c"
  (interactive "sCommand: ")
  (let ((md (match-data)))
    ;; commands must end in \n, so see if it is in there...
    (if (string-match "\n" command)
	(progn
	  (process-send-string etalk-process command)
	  (etalk-log (format "Sending: %s" command)))
      (process-send-string etalk-process (concat command "\n"))
      (etalk-log (format "Sending: %s\n" command)))
    (store-match-data md))
  )

(defun etalk-log-sentinel (process event)
  "The procedure is called when the etalk process is killed.  As a result,
all TCP connections need to be closed."

  (ding)
  (etalk-log (format "Signal: %s\n" event))
    (if (or (eq (process-status process) 'exit)
	    (eq (process-status process) 'signal))
	(progn
	  (setq etalk-process nil)
	  (etalk-zorch-all-processes)
	  (message "The main talk process has been removed."))))

  
(defun etalk-sentinel (process event)
  "The procedure is called whenever a process changes status.  Used to
detect the remote process hanging up to remove a process from the talk
process list."

  (ding)
  (etalk-log (format "TCP Signal: %s\n" event))
  (if (eq (process-exit-status process) 256)
      (progn
	(etalk-remove-process-from-list process)
	(if etalk-hangup-redo-windows
	    (etalk-setup-windows))))
  (save-excursion 
    (set-buffer (etalk-format etalk-local-buffer-name))
    (setq etalk-remote-is-emacs (etalk-all-emacs-p))))

(defun etalk-send-where ()
  "This function is used to determine where output is sent.  If you
are in a remote buffer, return that buffer, else return the whole
process list."

  (if (equal (current-buffer) 
	     (get-buffer (etalk-format etalk-local-buffer-name)))
      etalk-tcp-list
    (current-buffer)))

(defun etalk-send-output (buffer-or-list output-string)
  "Send output to buffer's process only.  If buffer is nil,
then send to list of all open talk processes."

  (let ((pl etalk-tcp-list))
    (if buffer-or-list
	(if (bufferp buffer-or-list)
	    (setq pl (cons (get-buffer-process buffer-or-list) '()))
	  (if (listp buffer-or-list)
	      (setq pl buffer-or-list))))
    (while (car pl)
      ;; This check allows the TCP list to be passed in as well.
      (if (listp (car pl))
	  (process-send-string (car (car pl)) output-string)
	(process-send-string (car pl) output-string))
      (setq pl (cdr pl)))))

(defun etalk-modify-socket (socket &optional process)
  "Find the process id, and then modify etalk-tcp-list so that the process'
tuple has this new socket id in it."

  (if (not process)
      (setq process (get-buffer-process (current-buffer))))
  (let ((n '())
	(l etalk-tcp-list))
    (while l
      (if (not (equal (car (car l)) process))
	  (setq n (cons (car l) n))
	(setq n (cons (cons (car (car l)) socket) n)))
      (setq l (cdr l)))
    (setq etalk-tcp-list n)))

(defun etalk-process-tuple (&optional process)
  "find the given process in the tcp-list, and return the tuple associated
with it.  The tuple is important for getting the user id used in
the binary, or based upon the process id number if it is an integer.."

  (if (not process)
      (setq process (get-buffer-process (current-buffer))))
  (if (processp process)
      (let ((l etalk-tcp-list))
	(while (and l (not (equal (car (car l)) process)))
	  (setq l (cdr l)))
	(car l))
    (if (integerp process)
	(let ((l etalk-tcp-list))
	  (while (and l (not (equal (car (cdr l)) process)))
	    (setq l (cdr l)))
	  (car l))
      nil)))

(defun etalk-read-talkbuffer ()
  "Read in on the minibuffer a talk buffer to close with completion.
not actually used anywhere yet, and not complete."
  (let* ((l etalk-tcp-list)
	 (s nil)
	 (completelist (while l
			 (setq s (cons 
				  (cons
				   (save-excursion
				     (set-buffer (process-buffer
						  (car (car l))))
				     (etalk-format "%u@%m"))
				   1)
				  s))			       
			 (setq l (cdr l)))))
    (completing-read "Kill connection: "
		     completelist nil t ""))
  )

(defun etalk-nuke-connection (&optional process)
  "Zap the talk process.  Uses delete. Should change to
signals sometime in the near future."

  (interactive)

  (if (and (not process) 
	   (equal (current-buffer) 
		  (get-buffer (etalk-format etalk-local-buffer-name))))
      (cond
       ((equal (length etalk-tcp-list) 0)
	(if (y-or-n-p "Delete all buffers related to etalk?")
	    (let ((buflst (buffer-list)))
	      (save-excursion
		(while buflst
		  (set-buffer (car buflst))
		  (if (and (boundp 'etalk-tag) etalk-tag)
		      (kill-buffer (car buflst)))
		  (setq buflst (cdr buflst))))))
	)
       ((equal (length etalk-tcp-list) 1)
	(etalk-nuke-connection (car (car etalk-tcp-list)))
	(setq etalk-tcp-list '()))
       (t
	(if (y-or-n-p "Zorch all talk processes? ")
	    (etalk-zorch-all-processes))))
    (let ((p (if process process (get-buffer-process (current-buffer)))))
      ;; if we wish to hangup, use the hangup command.
      (etalk-send-command (format "hangup %d\n"
				  (cdr (etalk-process-tuple p))))
      (etalk-remove-process-from-list p))))

(defun etalk-remove-process-from-list (process)
  "internal:  Remove one process object from the process list."

  (let ((n '())
	(l etalk-tcp-list))
    (while l
      (if (not (equal (car (car l)) process))
	  (setq n (cons (car l) n)))
      (setq l (cdr l)))
    (setq etalk-tcp-list n)))

(defun etalk-zorch-dead-processes ()
  "Kill all the talk processes in list which are no longer running."

  (interactive)
  (let ((mlist etalk-tcp-list))
    (while mlist
      (if (equal (process-exit-status (car (car mlist))) 0)
	  ()				;Hey.. keep these
	(etalk-remove-process-from-list (car (car mlist))))
      (setq mlist (cdr mlist)))))

(defun etalk-zorch-all-processes ()
  "Kill every single talk process in the talk process list"

  (interactive)
  (let ((mlist etalk-tcp-list))
    (while mlist
      (if (equal (process-exit-status (car (car mlist))) 0)
	  (etalk-nuke-connection (car (car mlist)))
	(etalk-remove-process-from-list (car (car mlist))))
      (setq mlist (cdr mlist))))
  ;; just to make sure, set this to nil
  (setq etalk-tcp-list nil))

(defun etalk-all-emacs-p ()
  "Return t if all connections are running emacs talk, nil otherwise."

  (interactive)
  (let ((mlist etalk-tcp-list)
	(all t))
    (while (and mlist all)
      (if (etalk-other-emacs-p (car (car mlist)))
	  ()				;Hey.. keep these
	(setq all nil))
      (setq mlist (cdr mlist)))
    all))

;;; end of lisp

(provide 'etalk-proc)
