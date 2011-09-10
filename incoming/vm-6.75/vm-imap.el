;;; Simple IMAP4 (RFC 2060) client for VM
;;; Copyright (C) 1998 Kyle E. Jones
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-imap)

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-imap-protocol-error "IMAP protocol error"))
  (put 'vm-imap-protocol-error 'error-conditions
       '(vm-imap-protocol-error error))
  (put 'vm-imap-protocol-error 'error-message "IMAP protocol error"))

;; Our goal is to drag the mail from the IMAP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.
(defun vm-imap-move-mail (source destination)
  (let ((process nil)
	(folder-type vm-folder-type)
	(m-per-session vm-imap-messages-per-session)
	(b-per-session vm-imap-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(imapdrop (vm-safe-imapdrop-string source))
	(statblob nil)
	(msgid (list nil nil (vm-imapdrop-sans-password source) 'uid))
	(imap-retrieved-messages vm-imap-retrieved-messages)
	(did-delete nil)
	(source-nopwd (vm-imapdrop-sans-password source))
	auto-expunge x select source-list uid
	can-delete read-write uid-validity
	mailbox mailbox-count message-size response
	n retrieved retrieved-bytes process-buffer)
    (setq auto-expunge (cond ((setq x (assoc source
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     ((setq x (assoc (vm-imapdrop-sans-password source)
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     (t vm-imap-expunge-after-retrieving)))
    (unwind-protect
	(catch 'end-of-session
	  (if handler
	      (throw 'end-of-session
		     (funcall handler 'vm-imap-move-mail source destination)))
	  (setq process (vm-imap-make-session source))
	  (or process (throw 'end-of-session nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    ;; find out how many messages are in the box.
	    (setq source-list (vm-parse source "\\([^:]+\\):?")
		  mailbox (nth 3 source-list))
	    (setq select (vm-imap-select-mailbox process mailbox))
	    (setq mailbox-count (nth 0 select)
		  uid-validity (nth 1 select)
		  read-write (nth 2 select)
		  can-delete (nth 3 select))
	    ;; sweep through the retrieval list, removing entries
	    ;; that have been invalidated by the new UIDVALIDITY
	    ;; value.
	    (setq imap-retrieved-messages
		  (vm-imap-clear-invalid-retrieval-entries
		   source-nopwd
		   imap-retrieved-messages
		   uid-validity))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved 0 retrieved-bytes 0)
	    (setq statblob (vm-imap-start-status-timer))
	    (vm-set-imap-stat-x-box statblob imapdrop)
	    (vm-set-imap-stat-x-maxmsg statblob mailbox-count)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (catch 'skip
		(vm-set-imap-stat-x-currmsg statblob n)
		(let (list)
		  (setq list (vm-imap-get-uid-list process n n))
		  (setq uid (cdr (car list)))
		  (setcar msgid uid)
		  (setcar (cdr msgid) uid-validity)
		  (if (member msgid imap-retrieved-messages)
		      (progn
			(if vm-imap-ok-to-ask
			    (message
			     "Skipping message %d (of %d) from %s (retrieved already)..."
			     n mailbox-count imapdrop))
			(throw 'skip t))))
		(setq message-size (vm-imap-get-message-size process n))
		(vm-set-imap-stat-x-need statblob message-size)
		(if (and (integerp vm-imap-max-message-size)
			 (> message-size vm-imap-max-message-size)
			 (progn
			   (setq response
				 (if vm-imap-ok-to-ask
				     (vm-imap-ask-about-large-message
				      process message-size n)
				   'skip))
			   (not (eq response 'retrieve))))
		    (progn
		      (if (and read-write can-delete (eq response 'delete))
			  (progn
			    (message "Deleting message %d..." n)
			    (vm-imap-delete-message process n)
			    (setq did-delete t))
			(if vm-imap-ok-to-ask
			    (message "Skipping message %d..." n)
			  (message
			   "Skipping message %d in %s, too large (%d > %d)..."
			   n imapdrop message-size vm-imap-max-message-size)))
		      (throw 'skip t)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count imapdrop)
		(vm-imap-send-command process
				      (format "FETCH %d (RFC822.PEEK)" n))
		(vm-imap-retrieve-to-crashbox process destination statblob)
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(if (not auto-expunge)
		    (setq imap-retrieved-messages
			  (cons (copy-sequence msgid)
				imap-retrieved-messages))
		  ;; The user doesn't want the messages
		  ;; kept in the mailbox.
		  ;; Delete the message now.
		  (if (and read-write can-delete)
		      (progn
			(vm-imap-delete-message process n)
			(setq did-delete t)))))
	      (vm-increment n))
	    (if did-delete
		(progn
		  ;; CLOSE forces an expunge and avoid the EXPUNGE
		  ;; responses.
		  (vm-imap-send-command process "CLOSE")
		  (vm-imap-read-ok-response process)))
	    (not (equal retrieved 0)) ))
      (setq vm-imap-retrieved-messages imap-retrieved-messages)
      (and statblob (vm-imap-stop-status-timer statblob))
      (if process
	  (vm-imap-end-session process)))))

(defun vm-imap-check-mail (source)
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	(retrieved vm-imap-retrieved-messages)
	(imapdrop (vm-imapdrop-sans-password source))
	msg-count uid-validity x response select mailbox source-list)
    (unwind-protect
	(prog1
	    (save-excursion
	      (catch 'end-of-session
		(if handler
		    (throw 'end-of-session
			   (funcall handler 'vm-imap-check-mail source)))
		(setq process (vm-imap-make-session source))
		(or process (throw 'end-of-session nil))
		(set-buffer (process-buffer process))
		(setq source-list (vm-parse source "\\([^:]+\\):?")
		      mailbox (nth 3 source-list))
		(setq select (vm-imap-select-mailbox process mailbox)
		      msg-count (car select)
		      uid-validity (nth 1 select))
		(if (zerop msg-count)
		    (throw 'end-of-session nil))
		;; sweep through the retrieval list, removing entries
		;; that have been invalidated by the new UIDVALIDITY
		;; value.
		(setq retrieved
		  (vm-imap-clear-invalid-retrieval-entries imapdrop
							   retrieved
							   uid-validity))
		(setq response (vm-imap-get-uid-list process 1 msg-count))
		(if (null response)
		    nil
		  (if (null (car response))
		      ;; (nil . nil) is returned if there are no
		      ;; messages in the mailbox.
		      (throw 'end-of-session nil)
		    (while response
		      (if (not (and (setq x (assoc (cdr (car response))
						   retrieved))
				    (equal (nth 1 x) imapdrop)
				    (eq (nth 2 x) 'uid)))
			  (throw 'end-of-session t))
		      (setq response (cdr response))))
		  ;; all messages in the mailbox have already been retrieved
		  (throw 'end-of-session nil))
		(not (equal 0 (car select)))))
	  (setq vm-imap-retrieved-messages retrieved))
      (and process (vm-imap-end-session process)))))

(defun vm-expunge-imap-messages ()
  "Deletes all messages from IMAP mailbox that have already been retrieved
into the current folder.  VM sets the \\Deleted flag on all such messages
on all the relevant IMAP servers and then immediately expunges."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (let ((process nil)
	(source nil)
	(trouble nil)
	(delete-count 0)
	(vm-block-new-mail t)
	(vm-imap-ok-to-ask t)
	(did-delete nil)
	msg-count can-delete read-write uid-validity
	select-response source-list imapdrop uid-alist mailbox data mp match)
    (unwind-protect
	(save-excursion
	  (setq vm-imap-retrieved-messages
		(sort vm-imap-retrieved-messages
		      (function (lambda (a b)
				  (cond ((string-lessp (nth 2 a) (nth 2 b)) t)
					((string-lessp (nth 2 b)
						       (nth 2 a))
					 nil)
					((string-lessp (nth 1 a) (nth 1 b)) t)
					((string-lessp (nth 1 b) (nth 1 a))
					 nil)
					((string-lessp (nth 0 a) (nth 0 b)) t)
					(t nil))))))
	  (setq mp vm-imap-retrieved-messages)
	  (while mp
	    (catch 'replay
	      (condition-case error-data
		  (progn
		    (setq data (car mp))
		    (if (not (equal source (nth 2 data)))
			(progn
			  (if process
			      (progn
				(if did-delete
				    (progn
				      (vm-imap-send-command process "CLOSE")
				      (vm-imap-read-ok-response process)))
				(vm-imap-end-session process)
				(setq process nil
				      did-delete nil)))
			  (setq source (nth 2 data))
			  (setq imapdrop (vm-safe-imapdrop-string source))
			  (condition-case error-data
			      (progn
				(message "Opening IMAP session to %s..."
					 imapdrop)
				(setq process (vm-imap-make-session source))
				(if (null process)
				    (signal 'error nil))
				(set-buffer (process-buffer process))
				(setq source-list (vm-parse source
							    "\\([^:]+\\):?")
				      mailbox (nth 3 source-list)
				      select-response (vm-imap-select-mailbox
						       process mailbox)
				      msg-count (car select-response)
				      uid-validity (nth 1 select-response)
				      read-write (nth 2 select-response)
				      can-delete (nth 3 select-response))
				(setq mp
				      (vm-imap-clear-invalid-retrieval-entries
				       source
				       mp
				       uid-validity))
				(if (not (eq data (car mp)))
				    ;; this entry must have been
				    ;; discarded as invalid, so
				    ;; skip it and process the
				    ;; entry that is now at the
				    ;; head of the list.
				    (throw 'replay t))
				(if (not can-delete)
				    (error "Can't delete messages in mailbox %s, skipping..." mailbox))
				(if (not read-write)
				    (error "Mailbox %s is read-only, skipping..." mailbox))
				(message "Expunging messages in %s..." imapdrop))
			    (error
			     (if (cdr error-data)
				 (apply 'message (cdr error-data))
			       (message
				"Couldn't open IMAP session to %s, skipping..."
				imapdrop))
			     (setq trouble (cons imapdrop trouble))
			     (sleep-for 2)
			     (while (equal (nth 1 (car mp)) source)
			       (setq mp (cdr mp)))
			     (throw 'replay t)))
			  (if (zerop msg-count)
			      (progn
				(while (equal (nth 1 (car mp)) source)
				  (setq mp (cdr mp)))
				(throw 'replay t)))
			  (setq uid-alist
				(vm-imap-get-uid-list
				 process 1 msg-count))))
		    (if (setq match (rassoc (car data) uid-alist))
			(progn
			  (vm-imap-delete-message process (car match))
			  (setq did-delete t)
			  (vm-increment delete-count))))
		(error
		 (setq trouble (cons imapdrop trouble))
		 (message "Something signaled: %s"
			  (prin1-to-string error-data))
		 (sleep-for 2)
		 (message "Skipping rest of mailbox %s..." imapdrop)
		 (sleep-for 2)
		 (while (equal (nth 2 (car mp)) source)
		   (setq mp (cdr mp)))
		 (throw 'replay t)))
	      (setq mp (cdr mp))))
	  (if did-delete
	      (progn
		(vm-imap-send-command process "CLOSE")
		(vm-imap-read-ok-response process)))
	  (if trouble
	      (progn
		(set-buffer (get-buffer-create "*IMAP Expunge Trouble*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "%s IMAP message%s expunged.\n\n"
				(if (zerop delete-count) "No" delete-count)
				(if (= delete-count 1) "" "s")))
		(insert "VM had problems expunging messages from:\n")
		(nreverse trouble)
		(setq mp trouble)
		(while mp
		  (insert "   " (car mp) "\n")
		  (setq mp (cdr mp)))
		(setq buffer-read-only t)
		(display-buffer (current-buffer)))
	    (message "%s IMAP message%s expunged."
		     (if (zerop delete-count) "No" delete-count)
		     (if (= delete-count 1) "" "s"))))
      (and process (vm-imap-end-session process)))
    (or trouble (setq vm-imap-retrieved-messages nil))))

(defun vm-imap-make-session (source)
  (let ((process-to-shutdown nil)
	process
	(imapdrop (vm-safe-imapdrop-string source))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	greeting timestamp
	host port mailbox auth user pass source-list process-buffer
	source-nopwd-nombox)
    (unwind-protect
	(catch 'end-of-session
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]+\\):?")
		host (nth 1 source-list)
		port (nth 2 source-list)
;;		mailbox (nth 3 source-list)
		auth (nth 4 source-list)
		user (nth 5 source-list)
		pass (nth 6 source-list)
		source-nopwd-nombox
		(vm-imapdrop-sans-password-and-mailbox source))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in IMAP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error "No authentication method in IMAP maildrop specification, \"%s\"" source))
	  (if (null user)
	      (error "No user in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in IMAP maildrop specification, \"%s\""
		     source))
	  (if (equal pass "*")
	      (progn
		(setq pass (car (cdr (assoc source-nopwd-nombox
					    vm-imap-passwords))))
		(if (null pass)
		    (if (null vm-imap-ok-to-ask)
			(progn (message "Need password for %s" imapdrop)
			       (throw 'end-of-session nil))
		      (setq pass
			    (vm-read-password
			     (format "IMAP password for %s: "
				     imapdrop)))))))
	  ;; save the password for the sake of
	  ;; vm-expunge-imap-messages, which passes password-less
	  ;; imapdrop specifications to vm-make-imap-session.
	  (if (null (assoc source-nopwd-nombox vm-imap-passwords))
	      (setq vm-imap-passwords (cons (list source-nopwd-nombox pass)
					    vm-imap-passwords)))
	  ;; get the trace buffer
	  (setq process-buffer
		(generate-new-buffer (format "trace of IMAP session to %s"
					     host)))
	  (save-excursion
	    (set-buffer process-buffer)
	    (buffer-disable-undo process-buffer)
	    ;; clear the trace buffer of old output
	    (erase-buffer)
	    ;; Tell MULE not to mess with the text.
	    (if (or vm-xemacs-mule-p vm-fsfemacs-mule-p)
		(set-buffer-file-coding-system 'binary t))
	    (if (equal auth "preauth")
		(setq process
		      (run-hook-with-args-until-success 'vm-imap-session-preauth-hook
							host port mailbox
							user pass)))
	    (if (processp process)
		(set-process-buffer process (current-buffer))
	      (insert "starting IMAP session " (current-time-string) "\n")
	      (insert (format "connecting to %s:%s\n" host port))
	      ;; open the connection to the server
	      (setq process (open-network-stream "IMAP" process-buffer
						 host port))
	      (and (null process) (throw 'end-of-session nil))
	      (insert "connected\n"))
	    (process-kill-without-query process)
	    (make-local-variable 'vm-imap-read-point)
	    (setq vm-imap-read-point (point))
	    (if (null (setq greeting (vm-imap-read-greeting process)))
		(progn (delete-process process)
		       (throw 'end-of-session nil)))
	    (setq process-to-shutdown process)
	    ;; authentication
	    (cond ((equal auth "login")
		   (vm-imap-send-command process
					 (format "LOGIN %s %s"
						 (vm-imap-quote-string user)
						 (vm-imap-quote-string pass)))
		   (and (null (vm-imap-read-ok-response process))
			(progn
			  (setq vm-imap-passwords
				(delete (list source-nopwd-nombox pass)
					vm-imap-passwords))
			  (message "IMAP password for %s incorrect" imapdrop)
			  (sleep-for 2)
			  (throw 'end-of-session nil))))
		  ((equal auth "preauth")
		   (if (not (eq greeting 'preauth))
		       (progn
			 (message "IMAP session was not pre-authenticated")
			 (sleep-for 2)
			 (throw 'end-of-session nil))))
		  (t (error "Don't know how to authenticate using %s" auth)))
	    (setq process-to-shutdown nil)
	    process ))
      (if process-to-shutdown
	  (vm-imap-end-session process-to-shutdown)))))

(defun vm-imap-end-session (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (vm-imap-send-command process "LOGOUT")
    ;; we don't care about the response
    ;;(vm-imap-read-ok-response process)
    (if (not vm-imap-keep-trace-buffer)
	(kill-buffer (process-buffer process)))
    (if (fboundp 'add-async-timeout)
	(add-async-timeout 2 'delete-process process)
      (run-at-time 2 nil 'delete-process process))))

(defun vm-imap-stat-timer (o) (aref o 0))
(defun vm-imap-stat-x-box (o) (aref o 1))
(defun vm-imap-stat-x-currmsg (o) (aref o 2))
(defun vm-imap-stat-x-maxmsg (o) (aref o 3))
(defun vm-imap-stat-x-got (o) (aref o 4))
(defun vm-imap-stat-x-need (o) (aref o 5))
(defun vm-imap-stat-y-box (o) (aref o 6))
(defun vm-imap-stat-y-currmsg (o) (aref o 7))
(defun vm-imap-stat-y-maxmsg (o) (aref o 8))
(defun vm-imap-stat-y-got (o) (aref o 9))
(defun vm-imap-stat-y-need (o) (aref o 10))

(defun vm-set-imap-stat-timer (o val) (aset o 0 val))
(defun vm-set-imap-stat-x-box (o val) (aset o 1 val))
(defun vm-set-imap-stat-x-currmsg (o val) (aset o 2 val))
(defun vm-set-imap-stat-x-maxmsg (o val) (aset o 3 val))
(defun vm-set-imap-stat-x-got (o val) (aset o 4 val))
(defun vm-set-imap-stat-x-need (o val) (aset o 5 val))
(defun vm-set-imap-stat-y-box (o val) (aset o 6 val))
(defun vm-set-imap-stat-y-currmsg (o val) (aset o 7 val))
(defun vm-set-imap-stat-y-maxmsg (o val) (aset o 8 val))
(defun vm-set-imap-stat-y-got (o val) (aset o 9 val))
(defun vm-set-imap-stat-y-need (o val) (aset o 10 val))

(defun vm-imap-start-status-timer ()
  (let ((blob (make-vector 11 nil))
	timer)
    (setq timer (add-timeout 5 'vm-imap-report-retrieval-status blob 5))
    (vm-set-imap-stat-timer blob timer)
    blob ))

(defun vm-imap-stop-status-timer (status-blob)
  (if (fboundp 'disable-timeout)
      (disable-timeout (vm-imap-stat-timer status-blob))
    (cancel-timer (vm-imap-stat-timer status-blob))))

(defun vm-imap-report-retrieval-status (o)
  (cond ((null (vm-imap-stat-x-got o)) t)
	;; should not be possible, but better safe...
	((not (eq (vm-imap-stat-x-box o) (vm-imap-stat-y-box o))) t)
	((not (eq (vm-imap-stat-x-currmsg o) (vm-imap-stat-y-currmsg o))) t)
	(t (message "Retrieving message %d (of %d) from %s, %s..."
		    (vm-imap-stat-x-currmsg o)
		    (vm-imap-stat-x-maxmsg o)
		    (vm-imap-stat-x-box o)
		    (format "%d%s of %d%s"
			    (vm-imap-stat-x-got o)
			    (if (> (vm-imap-stat-x-got o)
				   (vm-imap-stat-x-need o))
				"!"
			      "")
			    (vm-imap-stat-x-need o)
			    (if (eq (vm-imap-stat-x-got o)
				    (vm-imap-stat-y-got o))
				" (stalled)"
			      "")))))
  (vm-set-imap-stat-y-box o (vm-imap-stat-x-box o))
  (vm-set-imap-stat-y-currmsg o (vm-imap-stat-x-currmsg o))
  (vm-set-imap-stat-y-maxmsg o (vm-imap-stat-x-maxmsg o))
  (vm-set-imap-stat-y-got o (vm-imap-stat-x-got o))
  (vm-set-imap-stat-y-need o (vm-imap-stat-x-need o)))

(defun vm-imap-send-command (process command)
  (goto-char (point-max))
  (insert-before-markers "VM ")
  (let ((case-fold-search t))
    (if (string-match "^LOGIN" command)
	(insert-before-markers "LOGIN <parameters omitted>\r\n")
      (insert-before-markers command "\r\n")))
  (setq vm-imap-read-point (point))
  (process-send-string process "VM ")
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun vm-imap-select-mailbox (process mailbox &optional just-examine)
  (let ((imap-buffer (current-buffer))
	(command (if just-examine "EXAMINE" "SELECT"))
	tok response p
	(flags nil)
	(permanent-flags nil)
	(msg-count nil)
	(uid-validity nil)
	(read-write (not just-examine))
	(can-delete t)
	(need-ok t))
    (vm-imap-send-command process (format "%s %s" command mailbox))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to %s" command))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to %s" command))
      (cond ((vm-imap-response-matches response '* 'OK 'vector)
	     (setq p (cdr (nth 2 response)))
	     (cond ((vm-imap-response-matches p 'UIDVALIDITY 'atom)
		    (setq tok (nth 1 p))
		    (setq uid-validity (buffer-substring (nth 1 tok)
							 (nth 2 tok))))
		   ((vm-imap-response-matches p 'PERMANENTFLAGS 'list)
		    (setq permanent-flags (nth 1 p)))))
	    ((vm-imap-response-matches response '* 'FLAGS 'list)
	     (setq flags (nth 2 response)))
	    ((vm-imap-response-matches response '* 'atom 'EXISTS)
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-count (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-WRITE))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-ONLY))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    (if (null flags)
	(vm-imap-protocol-error "FLAGS missing from SELECT responses"))
    (if (null msg-count)
	(vm-imap-protocol-error "EXISTS missing from SELECT responses"))
    (if (null uid-validity)
	(vm-imap-protocol-error "UIDVALIDITY missing from SELECT responses"))
    (setq can-delete (vm-imap-scan-list-for-flag flags "\\Deleted"))
    (list msg-count uid-validity read-write can-delete) ))

(defun vm-imap-get-uid-list (process first last)
  (let ((list nil)
	(imap-buffer (current-buffer))
	tok msg-num uid response p
	(need-ok t))
    (vm-imap-send-command process (format "FETCH %d:%d (UID)" first last))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to UID FETCH"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to UID FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'UID 'atom))
		 (vm-imap-protocol-error
		  "expected (UID number) in FETCH response"))
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     (setq tok (nth 1 p))
	     (setq uid (buffer-substring (nth 1 tok) (nth 2 tok))
		   list (cons (cons msg-num uid) list)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
      ;; returning nil means the uid fetch failed so return
      ;; something other than nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list )))

(defun vm-imap-ask-about-large-message (process size n)
  (let ((work-buffer nil)
	(imap-buffer (current-buffer))
	(need-ok t)
	(need-header t)
	response fetch-response
	list p
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (vm-imap-send-command process
				  (format "FETCH %d (RFC822.HEADER)" n))
	    (while need-ok
	      (setq response (vm-imap-read-response process))
	      (if (vm-imap-response-matches response 'VM 'NO)
		  (error "server said NO to header FETCH"))
	      (if (vm-imap-response-matches response 'VM 'BAD)
		  (vm-imap-protocol-error "server said BAD to header FETCH"))
	      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
		     (setq fetch-response response
			   need-header nil))
		    ((vm-imap-response-matches response 'VM 'OK 'FETCH)
		     (setq need-ok nil))))
	    (if need-header
		(vm-imap-protocol-error "FETCH OK sent before FETCH response"))
	    (setq vm-imap-read-point (point-marker))
	    (setq list (cdr (nth 3 fetch-response)))
	    (if (not (vm-imap-response-matches list 'RFC822\.HEADER 'string))
		(vm-imap-protocol-error
		 "expected (RFC822.HEADER string) in FETCH response"))
	    (setq p (nth 1 list)
		  start (nth 1 p)
		  end (nth 2 p))
	    (setq work-buffer (generate-new-buffer "*imap-glop*"))
	    (set-buffer work-buffer)
	    (insert-buffer-substring imap-buffer start end)
	    (vm-imap-cleanup-region (point-min) (point-max))
	    (vm-display-buffer work-buffer)
	    (setq minibuffer-scroll-window (selected-window))
	    (goto-char (point-min))
	    (if (re-search-forward "^Received:" nil t)
		(progn
		  (goto-char (match-beginning 0))
		  (vm-reorder-message-headers
		   nil vm-visible-headers
		   vm-invisible-header-regexp)))
	    (set-window-point (selected-window) (point))
	    (if (y-or-n-p (format "Message %d, size = %d, retrieve? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from maildrop? " n size))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-imap-retrieve-to-crashbox (process crash statblob)
  (let ((start vm-imap-read-point)
	(need-msg t)
	end fetch-response list p)
    (goto-char start)
    (vm-set-imap-stat-x-got statblob 0)
    (let* ((func
	    (function
	     (lambda (beg end len)
	       (if vm-imap-read-point
		   (progn
		     (vm-set-imap-stat-x-got statblob (- end start))
		     (if (zerop (% (random) 10))
			 (vm-imap-report-retrieval-status statblob)))))))
	   (after-change-functions (cons func after-change-functions))
	   (need-ok t)
	   response)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to message FETCH"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to message FETCH"))
	(cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	       (setq fetch-response response
		     need-msg nil))
	      ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil)))))
    (if need-msg
	(vm-imap-protocol-error "FETCH OK sent before FETCH response"))
    ;; must make the read point a marker so that it stays fixed
    ;; relative to the text and we modify things below.
    (setq vm-imap-read-point (point-marker))
    (vm-set-imap-stat-x-got statblob nil)
    (setq list (cdr (nth 3 fetch-response)))
    (if (not (vm-imap-response-matches list 'RFC822 'string))
	(vm-imap-protocol-error "expected (RFC822 string) in FETCH response"))
    (setq p (nth 1 list)
	  start (nth 1 p))
    (goto-char (nth 2 p))
    (setq end (point-marker))
    (vm-imap-cleanup-region start end)
    (vm-munge-message-separators vm-folder-type start end)
    (goto-char start)
    ;; avoid the consing and stat() call for all but babyl
    ;; files, since this will probably slow things down.
    ;; only babyl files have the folder header, and we
    ;; should only insert it if the crash box is empty.
    (if (and (eq vm-folder-type 'babyl)
	     (let ((attrs (file-attributes crash)))
	       (or (null attrs) (equal 0 (nth 7 attrs)))))
	(let ((opoint (point)))
	  (vm-convert-folder-header nil vm-folder-type)
	  ;; if start is a marker, then it was moved
	  ;; forward by the insertion.  restore it.
	  (setq start opoint)
	  (goto-char start)
	  (vm-skip-past-folder-header)))
    (insert (vm-leading-message-separator))
    ;; this will not find the trailing message separator but
    ;; for the Content-Length stuff counting from eob is
    ;; the same thing in this case.
    (vm-convert-folder-type-headers nil vm-folder-type)
    (goto-char end)
    (insert-before-markers (vm-trailing-message-separator))
    ;; Some IMAP servers don't understand Sun's stupid
    ;; From_-with-Content-Length style folder and assume the last
    ;; newline in the message is a separator.  And so the server
    ;; strips it, leaving us with a message that does not end
    ;; with a newline.  Add the newline if needed.
    ;;
    ;; HP Openmail seems to have this problem.
    (if (and (not (eq ?\n (char-after (1- (point)))))
	     (memq vm-folder-type '(From_-with-Content-Length BellFrom_)))
	(insert-before-markers "\n"))
    ;; Set file type to binary for DOS/Windows.  I don't know if
    ;; this is correct to do or not; it depends on whether the
    ;; the CRLF or the LF newline convention is used on the inbox
    ;; associated with this crashbox.  This setting assumes the LF
    ;; newline convention is used.
    (let ((buffer-file-type t)
	  (selective-display nil))
      (write-region start end crash t 0))
    (delete-region start end)
    t ))

(defun vm-imap-delete-message (process n)
  (vm-imap-send-command process (format "STORE %d +FLAGS.SILENT (\\Deleted)"
					n))
  (if (null (vm-imap-read-ok-response process))
      (vm-imap-protocol-error "STORE ... +FLAGS.SILENT (\\Deleted) failed")))

(defun vm-imap-get-message-size (process n)
  (let ((list nil)
	(imap-buffer (current-buffer))
	tok size response p
	(need-size t)
	(need-ok t))
    (vm-imap-send-command process (format "FETCH %d:%d (RFC822.SIZE)" n n))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to size FETCH"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to size FETCH"))
      (cond ((and need-size
		  (vm-imap-response-matches response '* 'atom 'FETCH 'list))
	     (setq need-size nil)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'RFC822\.SIZE 'atom))
		 (vm-imap-protocol-error
		  "expected (RFC822.SIZE number) in FETCH response"))
	     (setq tok (nth 1 p))
	     (goto-char (nth 1 tok))
	     (setq size (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    size ))

(defun vm-imap-read-greeting (process)
  (let (response)
    (setq response (vm-imap-read-response process))
    (cond ((vm-imap-response-matches response '* 'OK)
	   t )
	  ((vm-imap-response-matches response '* 'PREAUTH)
	   'preauth )
	  (t nil))))

(defun vm-imap-read-ok-response (process)
  (let (response retval (done nil))
    (while (not done)
      (setq response (vm-imap-read-response process))
      (cond ((vm-imap-response-matches response '*)
	     nil )
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq retval t done t))
	    (t (setq retval nil done t))))
    retval ))

(defun vm-imap-cleanup-region (start end)
  (if (> (- end start) 30000)
      (message "CRLF conversion and char unstuffing..."))
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t)))
  (if (> (- end start) 30000)
      (message "CRLF conversion and char unstuffing... done"))
  (set-marker end nil))

(defun vm-imapdrop-sans-password (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":"
	    (nth 3 source-list) ":"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":*")))

(defun vm-imapdrop-sans-password-and-mailbox (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":*:"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":*")))

(defun vm-imap-read-response (process)
  (let ((list nil) tail obj)
    (goto-char vm-imap-read-point)
    (while (not (eq (car (setq obj (vm-imap-read-object process)))
		    'end-of-line))
      (if (null list)
	  (setq list (cons obj nil)
		tail list)
	(setcdr tail (cons obj nil))
	(setq tail (cdr tail))))
    (vm-imap-bail-if-server-says-farewell list)
    list ))

(defun vm-imap-read-object (process &optional skip-eol)
  (let ((done nil)
	opoint
	(token nil))
    (while (not done)
      (skip-chars-forward " \t")
      (cond ((< (- (point-max) (point)) 2)
	     (setq opoint (point))
	     (accept-process-output process)
	     (goto-char opoint))
	    ((looking-at "\r\n")
	     (forward-char 2)
	     (setq token '(end-of-line) done (not skip-eol)))
	    ((looking-at "\\[")
	     (forward-char 1)
	     (let* ((list (list 'vector))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-bracket))
		 (if (eq (car obj) 'close-paren)
		     (vm-imap-protocol-error "unexpected )"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at "\\]")
	     (forward-char 1)
	     (setq token '(close-bracket) done t))
	    ((looking-at "(")
	     (forward-char 1)
	     (let* ((list (list 'list))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-paren))
		 (if (eq (car obj) 'close-bracket)
		     (vm-imap-protocol-error "unexpected ]"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at ")")
	     (forward-char 1)
	     (setq token '(close-paren) done t))
	    ((looking-at "{")
	     (forward-char 1)
	     (let (start obj n-octets)
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'atom))
		   (vm-imap-protocol-error "number expected after {"))
	       (setq n-octets (string-to-int
			       (buffer-substring (nth 1 obj)
						 (nth 2 obj))))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'close-brace))
		   (vm-imap-protocol-error "} expected"))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'end-of-line))
		   (vm-imap-protocol-error "CRLF expected"))
	       (setq start (point))
	       (while (< (- (point-max) start) n-octets)
		 (accept-process-output process))
	       (goto-char (+ start n-octets))
	       (setq token (list 'string start (point))
		     done t)))
	    ((looking-at "}")
	     (forward-char 1)
	     (setq token '(close-brace) done t))
	    ((looking-at "\042") ;; double quote
	     (forward-char 1)
	     (let ((start (point))
		   (curpoint (point)))
	       (while (not done)
		 (skip-chars-forward "^\042")
		 (setq curpoint (point))
		 (if (looking-at "\042")
		     (setq done t)
		   (accept-process-output process)
		   (goto-char curpoint))
	       (setq token (list 'string start (1- curpoint))))))
	    ((looking-at "[\000-\040\177-\377]")
	     (vm-imap-protocol-error "unexpected char (%d)"
				     (char-after (point))))
	    (t
	     (let ((start (point))
		   (curpoint (point))
		   ;; \376 instead of \377 because Emacs 19.34
		   ;; has a bug in the fastmap initialization
		   ;; code that causes it to infloop
		   (not-word-chars "^\000-\040\177-\376()[]{}")
		   (not-word-regexp "[][\000-\040\177-\376(){}]"))
	       (while (not done)
		 (skip-chars-forward not-word-chars)
		 (setq curpoint (point))
		 (if (looking-at not-word-regexp)
		     (setq done t)
		   (accept-process-output process)
		   (goto-char curpoint))
		 (setq token (list 'atom start curpoint)))))))
    (setq vm-imap-read-point (point))
    token ))

(defun vm-imap-response-matches (response &rest expr)
  (let ((case-fold-search t) e r)
    (catch 'done
      (while (and expr response)
	(setq e (car expr)
	      r (car response))
	(cond ((stringp e)
	       (if (or (not (eq (car r) 'string))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward e (nth 2 r) t) (nth 2 r)))))
		   (throw 'done nil)))
	      ((numberp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (int-to-string e)
						  (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil)))
	      ((consp e)
	       (if (not (eq (car e) (car r)))
		   (throw 'done nil))
	       (apply 'vm-imap-response-matches (cdr r) (cdr e)))
	      ((eq e 'atom)
	       (if (not (eq (car r) 'atom))
		   (throw 'done nil)))
	      ((eq e 'vector)
	       (if (not (eq (car r) 'vector))
		   (throw 'done nil)))
	      ((eq e 'list)
	       (if (not (eq (car r) 'list))
		   (throw 'done nil)))
	      ((eq e 'string)
	       (if (not (eq (car r) 'string))
		   (throw 'done nil)))
	      ;; this must to come after all the comparisons for
	      ;; specific symbols.
	      ((symbolp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (symbol-name e) (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil))))
	(setq response (cdr response)
	      expr (cdr expr)))
      t )))

(defun vm-imap-bail-if-server-says-farewell (response)
  (if (vm-imap-response-matches response 'VM 'BYE)
      (throw 'end-of-session t)))

(defun vm-imap-protocol-error (&rest args)
  (set (make-local-variable 'vm-imap-keep-trace-buffer) t)
  (signal 'vm-imap-protocol-error (list (apply 'format args))))

(defun vm-imap-scan-list-for-flag (list flag)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward flag (nth 2 e) t) (nth 2 e))
	      (throw 'done t)))
	(setq list (cdr list)))
      nil )))

(defun vm-imap-clear-invalid-retrieval-entries (source-nopwd retrieved
						uid-validity)
  (let ((x retrieved)
	(prev nil))
    (while x
      (if (and (equal source-nopwd (nth 2 (car x)))
	       (not (equal (nth 1 (car x)) uid-validity)))
	  (if prev
	      (setcdr prev (cdr x))
	    (setq retrieved (cdr retrieved))))
      (setq x (cdr x)))
    retrieved ))

(defun vm-imap-quote-string (string)
  (vm-with-string-as-temp-buffer string 'vm-imap-quote-buffer))

(defun vm-imap-quote-buffer ()
  (goto-char (point-min))
  (insert "\"")
  (while (re-search-forward "[\"\\]" nil t)
    (forward-char -1)
    (insert "\\")
    (forward-char 1))
  (goto-char (point-max))
  (insert "\""))
