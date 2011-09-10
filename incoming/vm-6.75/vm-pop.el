;;; Simple POP (RFC 1939) client for VM
;;; Copyright (C) 1993, 1994, 1997, 1998 Kyle E. Jones
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

(provide 'vm-pop)

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-cant-uidl "Can't use UIDL")
      (define-error 'vm-dele-failed "DELE command failed")
      (define-error 'vm-uidl-failed "UIDL command failed"))
  (put 'vm-cant-uidl 'error-conditions '(vm-cant-uidl error))
  (put 'vm-cant-uidl 'error-message "Can't use UIDL")
  (put 'vm-dele-failed 'error-conditions '(vm-dele-failed error))
  (put 'vm-dele-failed 'error-message "DELE command failed")
  (put 'vm-uidl-failed 'error-conditions '(vm-uidl-failed error))
  (put 'vm-uidl-failed 'error-message "UIDL command failed"))

;; Our goal is to drag the mail from the POP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.
(defun vm-pop-move-mail (source destination)
  (let ((process nil)
	(folder-type vm-folder-type)
	(m-per-session vm-pop-messages-per-session)
	(b-per-session vm-pop-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(popdrop (vm-safe-popdrop-string source))
	(statblob nil)
	(can-uidl t)
	(msgid (list nil (vm-popdrop-sans-password source) 'uidl))
	(pop-retrieved-messages vm-pop-retrieved-messages)
	auto-expunge x
	mailbox-count mailbox-size message-size response
	n retrieved retrieved-bytes process-buffer uidl)
    (setq auto-expunge (cond ((setq x (assoc source vm-pop-auto-expunge-alist))
			      (cdr x))
			     ((setq x (assoc (vm-popdrop-sans-password source)
					     vm-pop-auto-expunge-alist))
			      (cdr x))
			     (t vm-pop-expunge-after-retrieving)))
    (unwind-protect
	(catch 'done
	  (if handler
	      (throw 'done
		     (funcall handler 'vm-pop-move-mail source destination)))
	  (setq process (vm-pop-make-session source))
	  (or process (throw 'done nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    ;; find out how many messages are in the box.
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process)
		  mailbox-count (nth 0 response)
		  mailbox-size (nth 1 response))
	    ;; forget it if the command fails
	    ;; or if there are no messages present.
	    (if (or (null mailbox-count)
		    (< mailbox-count 1))
		(throw 'done nil))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved 0 retrieved-bytes 0)
	    (setq statblob (vm-pop-start-status-timer))
	    (vm-set-pop-stat-x-box statblob popdrop)
	    (vm-set-pop-stat-x-maxmsg statblob mailbox-count)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (catch 'skip
		(vm-set-pop-stat-x-currmsg statblob n)
		(if can-uidl
		    (condition-case nil
			(let (list)
			  (vm-pop-send-command process (format "UIDL %d" n))
			  (setq response (vm-pop-read-response process t))
			  (if (null response)
			      (signal 'vm-cant-uidl nil))
			  (setq list (vm-parse response "\\([\041-\176]+\\) *")
				uidl (nth 2 list))
			  (if (null uidl)
			      (signal 'vm-cant-uidl nil))
			  (setcar msgid uidl)
			  (if (member msgid pop-retrieved-messages)
			      (progn
				(if vm-pop-ok-to-ask
				    (message
				     "Skipping message %d (of %d) from %s (retrieved already)..."
				     n mailbox-count popdrop))
				(throw 'skip t))))
		      (vm-cant-uidl
		       ;; something failed, so UIDL must not be working.
		       ;; note that fact and carry on.
		       (setq can-uidl nil
			     msgid nil))))
		(vm-pop-send-command process (format "LIST %d" n))
		(setq message-size (vm-pop-read-list-response process))
		(vm-set-pop-stat-x-need statblob message-size)
		(if (and (integerp vm-pop-max-message-size)
			 (> message-size vm-pop-max-message-size)
			 (progn
			   (setq response
				 (if vm-pop-ok-to-ask
				     (vm-pop-ask-about-large-message
				      process popdrop message-size n)
				   'skip))
			   (not (eq response 'retrieve))))
		    (progn
		      (if (eq response 'delete)
			  (progn
			    (message "Deleting message %d..." n)
			    (vm-pop-send-command process (format "DELE %d" n))
			    (and (null (vm-pop-read-response process))
				 (throw 'done (not (equal retrieved 0)))))
			(if vm-pop-ok-to-ask
			    (message "Skipping message %d..." n)
			  (message
			   "Skipping message %d in %s, too large (%d > %d)..."
			   n popdrop message-size vm-pop-max-message-size)))
		      (throw 'skip t)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count popdrop)
		(vm-pop-send-command process (format "RETR %d" n))
		(and (null (vm-pop-read-response process))
		     (throw 'done (not (equal retrieved 0))))
		(and (null (vm-pop-retrieve-to-crashbox process destination
							statblob))
		     (throw 'done (not (equal retrieved 0))))
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(if (and (not auto-expunge) msgid)
		    (setq pop-retrieved-messages
			  (cons (copy-sequence msgid)
				pop-retrieved-messages))
		  ;; Either the user doesn't want the messages
		  ;; kept in the mailbox or there's no UIDL
		  ;; support so there's no way to remember what
		  ;; messages we've retrieved.  Delete the
		  ;; message now.
		  (vm-pop-send-command process (format "DELE %d" n))
		  ;; DELE can't fail but Emacs or this code might
		  ;; blow a gasket and spew filth down the
		  ;; connection, so...
		  (and (null (vm-pop-read-response process))
		       (throw 'done (not (equal retrieved 0))))))
	      (vm-increment n))
	     (not (equal retrieved 0)) ))
      (setq vm-pop-retrieved-messages pop-retrieved-messages)
      (and statblob (vm-pop-stop-status-timer statblob))
      (if process
	  (vm-pop-end-session process)))))

(defun vm-pop-check-mail (source)
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	(retrieved vm-pop-retrieved-messages)
	(popdrop (vm-popdrop-sans-password source))
	x response)
    (unwind-protect
	(save-excursion
	  (catch 'done
	    (if handler
		(throw 'done
		       (funcall handler 'vm-pop-check-mail source)))
	    (setq process (vm-pop-make-session source))
	    (or process (throw 'done nil))
	    (set-buffer (process-buffer process))
	    (vm-pop-send-command process "UIDL")
	    (setq response (vm-pop-read-uidl-long-response process))
	    (if (null response)
		;; server doesn't understand UIDL
		nil
	      (if (null (car response))
		  ;; (nil . nil) is returned if there are no
		  ;; messages in the mailbox.
		  (throw 'done nil)
		(while response
		  (if (not (and (setq x (assoc (cdr (car response)) retrieved))
				(equal (nth 1 x) popdrop)
				(eq (nth 2 x) 'uidl)))
		      (throw 'done t))
		  (setq response (cdr response))))
	      ;; all messages in the mailbox have already been retrieved
	      (throw 'done nil))
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process))
	    (if (null response)
		nil
	      (not (equal 0 (car response))))))
      (and process (vm-pop-end-session process)))))

(defun vm-expunge-pop-messages ()
  "Deletes all messages from POP mailbox that have already been retrieved
into the current folder.  VM sends POP DELE commands to all the
relevant POP servers to remove the messages."
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
	(vm-pop-ok-to-ask t)
	popdrop uidl-alist data mp match)
    (unwind-protect
	(save-excursion
	  (setq vm-pop-retrieved-messages
		(sort vm-pop-retrieved-messages
		      (function (lambda (a b)
				  (cond ((string-lessp (nth 1 a) (nth 1 b)) t)
					((string-lessp (nth 1 b)
						       (nth 1 a))
					 nil)
					((string-lessp (car a) (car b)) t)
					(t nil))))))
	  (setq mp vm-pop-retrieved-messages)
	  (while mp
	    (condition-case nil
		(catch 'replay
		  (setq data (car mp))
		  (if (not (equal source (nth 1 data)))
		      (progn
			(if process
			    (progn
			     (vm-pop-end-session process)
			     (setq process nil)))
			(setq source (nth 1 data))
			(setq popdrop (vm-safe-popdrop-string source))
			(condition-case nil
			    (progn
			      (message "Opening POP session to %s..." popdrop)
			      (setq process (vm-pop-make-session source))
			      (if (null process)
				  (signal 'error nil))
			      (message "Expunging messages in %s..." popdrop))
			  (error
			   (message
			    "Couldn't open POP session to %s, skipping..."
			    popdrop)
			   (setq trouble (cons popdrop trouble))
			   (sleep-for 2)
			   (while (equal (nth 1 (car mp)) source)
			     (setq mp (cdr mp)))
			   (throw 'replay t)))
			(set-buffer (process-buffer process))
			(vm-pop-send-command process "UIDL")
			(setq uidl-alist
			      (vm-pop-read-uidl-long-response process))
			(if (null uidl-alist)
			    (signal 'vm-uidl-failed nil))))
		  (if (setq match (rassoc (car data) uidl-alist))
		      (progn
			(vm-pop-send-command process
					     (format "DELE %s" (car match)))
			(if (null (vm-pop-read-response process))
			    (signal 'vm-dele-failed nil))
			(vm-increment delete-count)))
		  (setq mp (cdr mp)))
	      (vm-dele-failed
	       (message "DELE %s failed on %s, skipping rest of mailbox..."
			(car match) popdrop)
	       (setq trouble (cons popdrop trouble))
	       (sleep-for 2)
	       (while (equal (nth 1 (car mp)) source)
		 (setq mp (cdr mp)))
	       (throw 'replay t))
	      (vm-uidl-failed
	       (message "UIDL %s failed on %s, skipping this mailbox..."
			(car match) popdrop)
	       (setq trouble (cons popdrop trouble))
	       (sleep-for 2)
	       (while (equal (nth 1 (car mp)) source)
		 (setq mp (cdr mp)))
	       (throw 'replay t))))
	  (if trouble
	      (progn
		(set-buffer (get-buffer-create "*POP Expunge Trouble*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "%s POP message%s expunged.\n\n"
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
	    (message "%s POP message%s expunged."
		     (if (zerop delete-count) "No" delete-count)
		     (if (= delete-count 1) "" "s"))))
      (and process (vm-pop-end-session process)))
    (or trouble (setq vm-pop-retrieved-messages nil))))

(defun vm-pop-make-session (source)
  (let ((process-to-shutdown nil)
	process
	(popdrop (vm-safe-popdrop-string source))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	greeting timestamp
	host port auth user pass source-list process-buffer source-nopwd)
    (unwind-protect
	(catch 'done
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]+\\):?")
		host (nth 0 source-list)
		port (nth 1 source-list)
		auth (nth 2 source-list)
		user (nth 3 source-list)
		pass (nth 4 source-list)
		source-nopwd (vm-popdrop-sans-password source))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in POP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in POP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error
	       "No authentication method in POP maildrop specification, \"%s\""
	       source))
	  (if (null user)
	      (error "No user in POP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in POP maildrop specification, \"%s\""
		     source))
	  (if (equal pass "*")
	      (progn
		(setq pass (car (cdr (assoc source-nopwd vm-pop-passwords))))
		(if (null pass)
		    (if (null vm-pop-ok-to-ask)
			(progn (message "Need password for %s" popdrop)
			       (throw 'done nil))
		      (setq pass
			    (vm-read-password
			     (format "POP password for %s: "
				     popdrop)))))))
	  ;; save the password for the sake of
	  ;; vm-expunge-pop-messages, which passes password-less
	  ;; popdrop specifications to vm-make-pop-session.
	  (if (null (assoc source-nopwd vm-pop-passwords))
	      (setq vm-pop-passwords (cons (list source-nopwd pass)
					   vm-pop-passwords)))
	  ;; get the trace buffer
	  (setq process-buffer
		(generate-new-buffer (format "trace of POP session to %s"
					     host)))
	  (save-excursion
	    (set-buffer process-buffer)
	    (buffer-disable-undo process-buffer)
	    ;; clear the trace buffer of old output
	    (erase-buffer)
	    ;; Tell MULE not to mess with the text.
	    (if (or vm-xemacs-mule-p vm-fsfemacs-mule-p)
		(set-buffer-file-coding-system 'binary t))
	    (insert "starting POP session " (current-time-string) "\n")
	    (insert (format "connecting to %s:%s\n" host port))
	    ;; open the connection to the server
	    (setq process (open-network-stream "POP" process-buffer host port))
	    (and (null process) (throw 'done nil))
	    (insert "connected\n")
	    (process-kill-without-query process)
	    (make-local-variable 'vm-pop-read-point)
	    (setq vm-pop-read-point (point))
	    (if (null (setq greeting (vm-pop-read-response process t)))
		(progn (delete-process process)
		       (throw 'done nil)))
	    (setq process-to-shutdown process)
	    ;; authentication
	    (cond ((equal auth "pass")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "PASS %s" pass))
		   (if (null (vm-pop-read-response process))
		       (progn
			 (setq vm-pop-passwords
			       (delete (list source-nopwd pass)
				       vm-pop-passwords))
			 (message "POP password for %s incorrect" popdrop)
			 (sleep-for 2)
			 (throw 'done nil))))
		  ((equal auth "rpop")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "RPOP %s" pass))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  ((equal auth "apop")
		   (setq timestamp (vm-parse greeting "[^<]+\\(<[^>]+>\\)")
			 timestamp (car timestamp))
		   (if (null timestamp)
		       (progn
			 (goto-char (point-max))
   (insert-before-markers "<<< ooops, no timestamp found in greeting! >>>\n")
			 (throw 'done nil)))
		   (vm-pop-send-command
		    process
		    (format "APOP %s %s"
			    user
			    (vm-pop-md5 (concat timestamp pass))))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  (t (error "Don't know how to authenticate using %s" auth)))
	    (setq process-to-shutdown nil)
	    process ))
      (if process-to-shutdown
	  (vm-pop-end-session process-to-shutdown)))))

(defun vm-pop-end-session (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (vm-pop-send-command process "QUIT")
    ;; we don't care about the response
    ;;(vm-pop-read-response process)
    (kill-buffer (process-buffer process))
    (if (fboundp 'add-async-timeout)
	(add-async-timeout 2 'delete-process process)
      (run-at-time 2 nil 'delete-process process))))

(defun vm-pop-stat-timer (o) (aref o 0))
(defun vm-pop-stat-x-box (o) (aref o 1))
(defun vm-pop-stat-x-currmsg (o) (aref o 2))
(defun vm-pop-stat-x-maxmsg (o) (aref o 3))
(defun vm-pop-stat-x-got (o) (aref o 4))
(defun vm-pop-stat-x-need (o) (aref o 5))
(defun vm-pop-stat-y-box (o) (aref o 6))
(defun vm-pop-stat-y-currmsg (o) (aref o 7))
(defun vm-pop-stat-y-maxmsg (o) (aref o 8))
(defun vm-pop-stat-y-got (o) (aref o 9))
(defun vm-pop-stat-y-need (o) (aref o 10))

(defun vm-set-pop-stat-timer (o val) (aset o 0 val))
(defun vm-set-pop-stat-x-box (o val) (aset o 1 val))
(defun vm-set-pop-stat-x-currmsg (o val) (aset o 2 val))
(defun vm-set-pop-stat-x-maxmsg (o val) (aset o 3 val))
(defun vm-set-pop-stat-x-got (o val) (aset o 4 val))
(defun vm-set-pop-stat-x-need (o val) (aset o 5 val))
(defun vm-set-pop-stat-y-box (o val) (aset o 6 val))
(defun vm-set-pop-stat-y-currmsg (o val) (aset o 7 val))
(defun vm-set-pop-stat-y-maxmsg (o val) (aset o 8 val))
(defun vm-set-pop-stat-y-got (o val) (aset o 9 val))
(defun vm-set-pop-stat-y-need (o val) (aset o 10 val))

(defun vm-pop-start-status-timer ()
  (let ((blob (make-vector 11 nil))
	timer)
    (setq timer (add-timeout 5 'vm-pop-report-retrieval-status blob 5))
    (vm-set-pop-stat-timer blob timer)
    blob ))

(defun vm-pop-stop-status-timer (status-blob)
  (if (fboundp 'disable-timeout)
      (disable-timeout (vm-pop-stat-timer status-blob))
    (cancel-timer (vm-pop-stat-timer status-blob))))

(defun vm-pop-report-retrieval-status (o)
  (cond ((null (vm-pop-stat-x-got o)) t)
	;; should not be possible, but better safe...
	((not (eq (vm-pop-stat-x-box o) (vm-pop-stat-y-box o))) t)
	((not (eq (vm-pop-stat-x-currmsg o) (vm-pop-stat-y-currmsg o))) t)
	(t (message "Retrieving message %d (of %d) from %s, %s..."
		    (vm-pop-stat-x-currmsg o)
		    (vm-pop-stat-x-maxmsg o)
		    (vm-pop-stat-x-box o)
		    (format "%d%s of %d%s"
			    (vm-pop-stat-x-got o)
			    (if (> (vm-pop-stat-x-got o)
				   (vm-pop-stat-x-need o))
				"!"
			      "")
			    (vm-pop-stat-x-need o)
			    (if (eq (vm-pop-stat-x-got o)
				    (vm-pop-stat-y-got o))
				" (stalled)"
			      "")))))
  (vm-set-pop-stat-y-box o (vm-pop-stat-x-box o))
  (vm-set-pop-stat-y-currmsg o (vm-pop-stat-x-currmsg o))
  (vm-set-pop-stat-y-maxmsg o (vm-pop-stat-x-maxmsg o))
  (vm-set-pop-stat-y-got o (vm-pop-stat-x-got o))
  (vm-set-pop-stat-y-need o (vm-pop-stat-x-need o)))

(defun vm-pop-send-command (process command)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert-before-markers "PASS <omitted>\r\n")
    (insert-before-markers command "\r\n"))
  (setq vm-pop-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun vm-pop-read-response (process &optional return-response-string)
  (let ((case-fold-search nil)
	 match-end)
    (goto-char vm-pop-read-point)
    (while (not (search-forward "\r\n" nil t))
      (accept-process-output process)
      (goto-char vm-pop-read-point))
    (setq match-end (point))
    (goto-char vm-pop-read-point)
    (if (not (looking-at "+OK"))
	(progn (setq vm-pop-read-point match-end) nil)
      (setq vm-pop-read-point match-end)
      (if return-response-string
	  (buffer-substring (point) match-end)
	t ))))

(defun vm-pop-read-past-dot-sentinel-line (process)
  (let ((case-fold-search nil))
    (goto-char vm-pop-read-point)
    (while (not (re-search-forward "^\\.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let ((opoint (point)))
	(accept-process-output process)
	(goto-char opoint)))
    (setq vm-pop-read-point (point))))

(defun vm-pop-read-stat-response (process)
  (let ((response (vm-pop-read-response process t))
	list)
    (setq list (vm-parse response "\\([^ ]+\\) *"))
    (list (string-to-int (nth 1 list)) (string-to-int (nth 2 list)))))

(defun vm-pop-read-list-response (process)
  (let ((response (vm-pop-read-response process t)))
    (string-to-int (nth 2 (vm-parse response "\\([^ ]+\\) *")))))

(defun vm-pop-read-uidl-long-response (process)
  (let ((start vm-pop-read-point)
	(list nil)
	n uidl)
    (catch 'done
      (goto-char start)
      (while (not (re-search-forward "^\\.\r\n\\|^-ERR .*$" nil 0))
	(beginning-of-line)
	;; save-excursion doesn't work right
	(let ((opoint (point)))
	  (accept-process-output process)
	  (goto-char opoint)))
      (setq vm-pop-read-point (point-marker))
      (goto-char start)
      ;; no uidl support, bail.
      (if (not (looking-at "\\+OK"))
	  (throw 'done nil))
      (forward-line 1)
      (while (not (eq (char-after (point)) ?.))
	;; not loking at a number, bail.
	(if (not (looking-at "[0-9]"))
	    (throw 'done nil))
	(setq n (int-to-string (read (current-buffer))))
	(skip-chars-forward " ")
	(setq start (point))
	(skip-chars-forward "\041-\176")
	;; no tag after the message number, bail.
	(if (= start (point))
	    (throw 'done nil))
	(setq uidl (buffer-substring start (point)))
	(setq list (cons (cons n uidl) list))
	(forward-line 1))
      ;; returning nil means the uidl command failed so don't
      ;; return nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list ))))

(defun vm-pop-ask-about-large-message (process popdrop size n)
  (let ((work-buffer nil)
	(pop-buffer (current-buffer))
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (vm-pop-send-command process (format "TOP %d %d" n 0))
	    (if (vm-pop-read-response process)
		(progn
		  (setq start vm-pop-read-point)
		  (vm-pop-read-past-dot-sentinel-line process)
		  (setq end vm-pop-read-point)
		  (setq work-buffer (generate-new-buffer
				     (format "*headers of %s message %d*"
					     popdrop n)))
		  (set-buffer work-buffer)
		  (insert-buffer-substring pop-buffer start end)
		  (forward-line -1)
		  (delete-region (point) (point-max))
		  (vm-pop-cleanup-region (point-min) (point-max))
		  (vm-display-buffer work-buffer)
		  (setq minibuffer-scroll-window (selected-window))
		  (goto-char (point-min))
		  (if (re-search-forward "^Received:" nil t)
		      (progn
			(goto-char (match-beginning 0))
			(vm-reorder-message-headers
			 nil vm-visible-headers
			 vm-invisible-header-regexp)))
		  (set-window-point (selected-window) (point))))
	    (if (y-or-n-p (format "Message %d, size = %d, retrieve? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from popdrop? " n size))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-pop-retrieve-to-crashbox (process crash statblob)
  (let ((start vm-pop-read-point) end)
    (goto-char start)
    (vm-set-pop-stat-x-got statblob 0)
    (while (not (re-search-forward "^\\.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let* ((opoint (point))
	     (func
	      (function
	       (lambda (beg end len)
		 (if vm-pop-read-point
		     (progn
		       (vm-set-pop-stat-x-got statblob (- end start))
		       (if (zerop (% (random) 10))
			   (vm-pop-report-retrieval-status statblob)))))))
	     (after-change-functions (cons func after-change-functions)))
	(accept-process-output process)
	(goto-char opoint)))
    (vm-set-pop-stat-x-got statblob nil)
    (setq vm-pop-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker))
    (vm-pop-cleanup-region start end)
    ;; Some POP servers strip leading and trailing message
    ;; separators, some don't.  Figure out what kind we're
    ;; talking to and do the right thing.
    (if (eq (vm-get-folder-type nil start end) 'unknown)
	(progn
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
	  (insert-before-markers (vm-trailing-message-separator))))
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

(defun vm-pop-cleanup-region (start end)
  (if (> (- end start) 30000)
      (message "CRLF conversion and char unstuffing..."))
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t))
    (goto-char start)
    ;; chop leading dots
    (while (and (< (point) end) (re-search-forward "^\\."  end t))
      (replace-match "" t t)
      (forward-char)))
  (if (> (- end start) 30000)
      (message "CRLF conversion and char unstuffing... done"))
  (set-marker end nil))

(defun vm-pop-md5 (string)
  (let ((buffer nil))
    (unwind-protect
	(save-excursion
	  (setq buffer (generate-new-buffer "*vm-work*"))
	  (set-buffer buffer)
	  ;; call-process-region calls write-region.
	  ;; don't let it do CR -> LF translation.
	  (setq selective-display nil)
	  (insert string)
	  (if (fboundp 'md5)
	      (progn
		(goto-char (point-min))
		(insert (md5 buffer (point-min) (point-max)))
		(delete-region (point) (point-max)))
	    (call-process-region (point-min) (point-max)
				 (or shell-file-name "/bin/sh") t buffer nil
				 shell-command-switch vm-pop-md5-program))
	  ;; MD5 digest is 32 chars long
	  ;; mddriver adds a newline to make neaten output for tty
	  ;; viewing, make sure we leave it behind.
	  (vm-buffer-substring-no-properties (point-min) (+ (point-min) 32)))
      (and buffer (kill-buffer buffer)))))

(defun vm-popdrop-sans-password (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":"
	    (nth 3 source-list) ":*")))
