;;; imap.el --- imap library
;; Copyright (C) 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; imap.el is a elisp library providing an interface for talking to
;; IMAP servers.
;;
;; imap.el is roughly divided in two parts, one that parses IMAP
;; responses from the server and storing data into buffer-local
;; variables, and one for utility functions which send commands to
;; server, waits for an answer, and return information.  The latter
;; part is layered on top of the previous.
;;
;; The imap.el API consist of the following functions, other functions
;; in this file should not be called directly and the result of doing
;; so are at best undefined.
;;
;; Global commands:
;;
;; imap-open,       imap-opened,    imap-authenticate, imap-close,
;; imap-capability, imap-namespace, imap-error-text
;;
;; Mailbox commands:
;;
;; imap-mailbox-get,       imap-mailbox-map,         imap-current-mailbox, 
;; imap-current-mailbox-p, imap-search,              imap-mailbox-select,
;; imap-mailbox-examine,   imap-mailbox-unselect,    imap-mailbox-expunge
;; imap-mailbox-close,     imap-mailbox-create,      imap-mailbox-delete
;; imap-mailbox-rename,    imap-mailbox-lsub,        imap-mailbox-list
;; imap-mailbox-subscribe, imap-mailbox-unsubscribe, imap-mailbox-status
;; imap-mailbox-acl-get,   imap-mailbox-acl-set,     imap-mailbox-acl-delete
;;
;; Message commands:
;;
;; imap-fetch-asynch,                 imap-fetch,
;; imap-current-message,              imap-list-to-message-set,
;; imap-message-get,                  imap-message-map
;; imap-message-envelope-date,        imap-message-envelope-subject, 
;; imap-message-envelope-from,        imap-message-envelope-sender,
;; imap-message-envelope-reply-to,    imap-message-envelope-to,
;; imap-message-envelope-cc,          imap-message-envelope-bcc
;; imap-message-envelope-in-reply-to, imap-message-envelope-message-id
;; imap-message-body,                 imap-message-flag-permanent-p
;; imap-message-flags-set,            imap-message-flags-del
;; imap-message-flags-add,            imap-message-copyuid
;; imap-message-copy,                 imap-message-appenduid
;; imap-message-append,               imap-envelope-from
;; imap-body-lines
;;
;; It is my hope that theese commands should be pretty self
;; explanatory for someone that know IMAP.  All functions have
;; additional documentation on how to invoke them.
;;
;; imap.el support RFC1730/2060 (IMAP4/IMAP4rev1), implemented IMAP
;; extensions are RFC2195 (CRAM-MD5), RFC2086 (ACL), RFC2342
;; (NAMESPACE), RFC2359 (UIDPLUS), the IMAP-part of RFC2595 (STARTTLS)
;; (with use of external library starttls.el and program starttls) and
;; the GSSAPI / kerberos V4 sections of RFC1731 (with use of external
;; program `imtest').  It also take advantage the UNSELECT extension
;; in Cyrus IMAPD.
;;
;; Without the work of John McClary Prevost and Jim Radford this library
;; would not have seen the light of day.  Many thanks.
;;
;; This is a transcript of short interactive session for demonstration
;; purposes.
;;
;; (imap-open "my.mail.server")
;; => " *imap* my.mail.server:0"
;;
;; The rest are invoked with current buffer as the buffer returned by
;; `imap-open'.  It is possible to do all without this, but it would
;; look ugly here since `buffer' is always the last argument for all
;; imap.el API functions.
;;
;; (imap-authenticate "myusername" "mypassword")
;; => auth
;;
;; (imap-mailbox-lsub "*")
;; => ("INBOX.sentmail" "INBOX.private" "INBOX.draft" "INBOX.spam")
;;
;; (imap-mailbox-list "INBOX.n%")
;; => ("INBOX.namedroppers" "INBOX.nnimap" "INBOX.ntbugtraq")
;;
;; (imap-mailbox-select "INBOX.nnimap")
;; => "INBOX.nnimap"
;;
;; (imap-mailbox-get 'exists)
;; => 166
;;
;; (imap-mailbox-get 'uidvalidity)
;; => "908992622"
;;
;; (imap-search "FLAGGED SINCE 18-DEC-98")
;; => (235 236)
;;
;; (imap-fetch 235 "RFC822.PEEK" 'RFC822)
;; => "X-Sieve: cmu-sieve 1.3^M\nX-Username: <jas@pdc.kth.se>^M\r...."
;;
;; Todo:
;; 
;; o Parse UIDs as strings? We need to overcome the 28 bit limit somehow.
;; o Don't use `read' at all (important places already fixed)
;; o Accept list of articles instead of message set string in most
;;   imap-message-* functions.
;;
;; Revision history:
;;
;;  - 19991218 added starttls/digest-md5 patch,
;;             by Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;;             NB! you need SLIM for starttls.el and digest-md5.el
;;  - 19991023 commited to pgnus
;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-and-compile
  (autoload 'open-ssl-stream "ssl")
  (autoload 'base64-decode-string "base64")
  (autoload 'base64-encode-string "base64")
  (autoload 'starttls-open-stream "starttls")
  (autoload 'starttls-negotiate "starttls")
  (autoload 'digest-md5-parse-digest-challenge "digest-md5")
  (autoload 'digest-md5-digest-response "digest-md5")
  (autoload 'digest-md5-digest-uri "digest-md5")
  (autoload 'digest-md5-challenge "digest-md5")
  (autoload 'rfc2104-hash "rfc2104")
  (autoload 'md5 "md5")
  (autoload 'utf7-encode "utf7")
  (autoload 'utf7-decode "utf7")
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec"))

;; User variables.

(defvar imap-kerberos4-program '("imtest -m kerberos_v4 -u %l -p %p %s"
				 "imtest -kp %s %p")
  "List of strings containing commands for Kerberos 4 authentication.
%s is replaced with server hostname, %p with port to connect to, and
%l with the value of `imap-default-user'.  The program should accept
IMAP commands on stdin and return responses to stdout.")

(defvar imap-gssapi-program '("imtest -m gssapi -u %l -p %p %s")
  "List of strings containing commands for GSSAPI (krb5) authentication.
%s is replaced with server hostname, %p with port to connect to, and
%l with the value of `imap-default-user'.  The program should accept
IMAP commands on stdin and return responses to stdout.")

(defvar imap-ssl-program '("openssl s_client -ssl3 -connect %s:%p"
			   "openssl s_client -ssl2 -connect %s:%p"
			   "s_client -ssl3 -connect %s:%p"
			   "s_client -ssl2 -connect %s:%p")
  "A string, or list of strings, containing commands for SSL connections.
Within a string, %s is replaced with the server address and %p with
port number on server.  The program should accept IMAP commands on
stdin and return responses to stdout.")

(defvar imap-default-user (user-login-name)
  "Default username to use.")

(defvar imap-error nil
  "Error codes from the last command.")

;; Various variables.

(defvar imap-fetch-data-hook nil
  "Hooks called after receiving each FETCH response.")

(defvar imap-streams '(gssapi kerberos4 starttls ssl network)
  "Priority of streams to consider when opening connection to server.")

(defvar imap-stream-alist
  '((gssapi    imap-gssapi-stream-p    imap-gssapi-open)
    (kerberos4 imap-kerberos4-stream-p imap-kerberos4-open)
    (ssl       imap-ssl-p              imap-ssl-open)
    (network   imap-network-p          imap-network-open)
    (starttls  imap-starttls-p         imap-starttls-open))
  "Definition of network streams.

(NAME CHECK OPEN)

NAME names the stream, CHECK is a function returning non-nil if the
server support the stream and OPEN is a function for opening the
stream.")

(defvar imap-authenticators '(gssapi 
			      kerberos4
			      digest-md5
			      cram-md5
			      login
			      anonymous)
  "Priority of authenticators to consider when authenticating to server.")

(defvar imap-authenticator-alist 
  '((gssapi     imap-gssapi-auth-p    imap-gssapia-auth)
    (kerberos4  imap-kerberos4-auth-p imap-kerberos4-auth)
    (cram-md5   imap-cram-md5-p       imap-cram-md5-auth)
    (login      imap-login-p          imap-login-auth)
    (anonymous  imap-anonymous-p      imap-anonymous-auth)
    (digest-md5 imap-digest-md5-p     imap-digest-md5-auth))
  "Definition of authenticators.

(NAME CHECK AUTHENTICATE)

NAME names the authenticator.  CHECK is a function returning non-nil if
the server support the authenticator and AUTHENTICATE is a function
for doing the actuall authentification.")

(defvar imap-use-utf7 t
  "If non-nil, do utf7 encoding/decoding of mailbox names.
Since the UTF7 decoding currently only decodes into ISO-8859-1
characters, you may disable this decoding if you need to access UTF7
encoded mailboxes which doesn't translate into ISO-8859-1.")

;; Internal constants.  Change theese and die.

(defconst imap-default-port 143)
(defconst imap-default-ssl-port 993)
(defconst imap-default-stream 'network)
(defconst imap-coding-system-for-read 'binary)
(defconst imap-coding-system-for-write 'binary)
(defconst imap-local-variables '(imap-server
				 imap-port
				 imap-client-eol
				 imap-server-eol
				 imap-auth
				 imap-stream
				 imap-username
				 imap-password
				 imap-current-mailbox
				 imap-current-target-mailbox
				 imap-message-data
				 imap-capability
				 imap-namespace
				 imap-state
				 imap-reached-tag
				 imap-failed-tags
				 imap-tag
				 imap-process
				 imap-mailbox-data))

;; Internal variables.

(defvar imap-stream nil)
(defvar imap-auth nil)
(defvar imap-server nil)
(defvar imap-port nil)
(defvar imap-username nil)
(defvar imap-password nil)
(defvar imap-state 'closed 
  "IMAP state.
Valid states are `closed', `initial', `nonauth', `auth', `selected'
and `examine'.")

(defvar imap-server-eol "\r\n"
  "The EOL string sent from the server.")

(defvar imap-client-eol "\r\n"
  "The EOL string we send to the server.")

(defvar imap-current-mailbox nil
  "Current mailbox name.")

(defvar imap-current-target-mailbox nil
  "Current target mailbox for COPY and APPEND commands.")

(defvar imap-mailbox-data nil
  "Obarray with mailbox data.")

(defvar imap-mailbox-prime 997
  "Length of imap-mailbox-data.")

(defvar imap-current-message nil
  "Current message number.")

(defvar imap-message-data nil
  "Obarray with message data.")

(defvar imap-message-prime 997
  "Length of imap-message-data.")

(defvar imap-capability nil
  "Capability for server.")

(defvar imap-namespace nil
  "Namespace for current server.")

(defvar imap-reached-tag 0
  "Lower limit on command tags that have been parsed.")

(defvar imap-failed-tags nil 
  "Alist of tags that failed.
Each element is a list with four elements; tag (a integer), response
state (a symbol, `OK', `NO' or `BAD'), response code (a string), and
human readable response text (a string).")

(defvar imap-tag 0
  "Command tag number.")

(defvar imap-process nil
  "Process.")

(defvar imap-continuation nil
  "Non-nil indicates that the server emitted a continuation request.
The actually value is really the text on the continuation line.")

(defvar imap-log nil
  "Imap session trace.")

(defvar imap-debug nil			;"*imap-debug*"
  "Random debug spew.")


;; Utility functions:

(defsubst imap-disable-multibyte ()
  "Enable multibyte in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (set-buffer-multibyte nil)))

(defun imap-read-passwd (prompt &rest args)
  "Read a password using PROMPT.
If ARGS, PROMPT is used as an argument to `format'."
  (let ((prompt (if args
		    (apply 'format prompt args)
		  prompt)))
    (funcall (if (or (fboundp 'read-passwd)
		     (and (load "subr" t)
			  (fboundp 'read-passwd))
		     (and (load "passwd" t)
			  (fboundp 'read-passwd)))
		 'read-passwd
	       (autoload 'ange-ftp-read-passwd "ange-ftp")
	       'ange-ftp-read-passwd)
	     prompt)))

(defsubst imap-utf7-encode (string)
  (if imap-use-utf7
      (and string
	   (condition-case ()
	       (utf7-encode string t)
	     (error (message 
		     "imap: Could not UTF7 encode `%s', using it unencoded..."
		     string)
		    string)))
    string))

(defsubst imap-utf7-decode (string)
  (if imap-use-utf7
      (and string
	   (condition-case ()
	       (utf7-decode string t)
	     (error (message
		     "imap: Could not UTF7 decode `%s', using it undecoded..."
		     string)
		    string)))
    string))

(defsubst imap-ok-p (status)
  (if (eq status 'OK)
      t
    (setq imap-error status)
    nil))

(defun imap-error-text (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (nth 3 (car imap-failed-tags))))


;; Server functions; stream stuff:

(defun imap-kerberos4-stream-p (buffer)
  (imap-capability 'AUTH=KERBEROS_V4 buffer))

(defun imap-kerberos4-open (name buffer server port)
  (let ((cmds imap-kerberos4-program)
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening Kerberos 4 IMAP connection with `%s'..." cmd)
      (let* ((port (or port imap-default-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (process (start-process 
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?s server
			 ?p (number-to-string port)
			 ?l imap-default-user))))
	     response)
	(when process
	  (with-current-buffer buffer
	    (setq imap-client-eol "\n")
	    (while (and (memq (process-status process) '(open run))
			(goto-char (point-min))
                        ;; cyrus 1.6.x (13? < x <= 22) queries capabilities
		        (or (while (looking-at "^C:")
			      (forward-line))
			    t)
			;; cyrus 1.6 imtest print "S: " before server greeting
			(or (not (looking-at "S: "))
			    (forward-char 3)
			    t)
			(not (and (imap-parse-greeting)
				  ;; success in imtest < 1.6:
				  (or (re-search-forward
				       "^__\\(.*\\)__\n" nil t)
				      ;; success in imtest 1.6:
				      (re-search-forward
				       "^\\(Authenticat.*\\)" nil t))
				  (setq response (match-string 1)))))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (and imap-log
		 (with-current-buffer (get-buffer-create imap-log)
		   (imap-disable-multibyte)
		   (buffer-disable-undo)
		   (goto-char (point-max))
		   (insert-buffer-substring buffer)))
	    (erase-buffer)
	    (message "Kerberos 4 IMAP connection: %s" (or response "failed"))
	    (if (and response (let ((case-fold-search nil))
				(not (string-match "failed" response))))
		(setq done process)
	      (if (memq (process-status process) '(open run))
		  (imap-send-command-wait "LOGOUT"))
	      (delete-process process)
	      nil)))))
    done))
  
(defun imap-gssapi-stream-p (buffer)
  (imap-capability 'AUTH=GSSAPI buffer))

(defun imap-gssapi-open (name buffer server port)
  (let ((cmds imap-gssapi-program)
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening GSSAPI IMAP connection with `%s'..." cmd)
      (let* ((port (or port imap-default-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (process (start-process 
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?s server
			 ?p (number-to-string port)
			 ?l imap-default-user))))
	     response)
	(when process
	  (with-current-buffer buffer
	    (setq imap-client-eol "\n")
	    (while (and (memq (process-status process) '(open run))
			(goto-char (point-min))
			;; cyrus 1.6 imtest print "S: " before server greeting
			(or (not (looking-at "S: "))
			    (forward-char 3)
			    t)
			(not (and (imap-parse-greeting)
				  ;; success in imtest 1.6:
				  (re-search-forward
				   "^\\(Authenticat.*\\)" nil t)
				  (setq response (match-string 1)))))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (and imap-log
		 (with-current-buffer (get-buffer-create imap-log)
		   (imap-disable-multibyte)
		   (buffer-disable-undo)
		   (goto-char (point-max))
		   (insert-buffer-substring buffer)))
	    (erase-buffer)
	    (message "GSSAPI IMAP connection: %s" (or response "failed"))
	    (if (and response (let ((case-fold-search nil))
				(not (string-match "failed" response))))
		(setq done process)
	      (if (memq (process-status process) '(open run))
		  (imap-send-command-wait "LOGOUT"))
	      (delete-process process)
	      nil)))))
    done))

(defun imap-ssl-p (buffer)
  nil)

(defun imap-ssl-open (name buffer server port)
  "Open a SSL connection to server."
  (let ((cmds (if (listp imap-ssl-program) imap-ssl-program
		(list imap-ssl-program)))
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "imap: Opening SSL connection with `%s'..." cmd)
      (let* ((port (or port imap-default-ssl-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (ssl-program-name shell-file-name)
	     (ssl-program-arguments
	      (list shell-command-switch
		    (format-spec cmd (format-spec-make
				      ?s server
				      ?p (number-to-string port)))))
	     process)
	(when (setq process (ignore-errors (open-ssl-stream
					    name buffer server port)))
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (while (and (memq (process-status process) '(open run))
			(goto-char (point-max))
			(forward-line -1)
			(not (imap-parse-greeting)))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (and imap-log
		 (with-current-buffer (get-buffer-create imap-log)
		   (imap-disable-multibyte)
		   (buffer-disable-undo)
		   (goto-char (point-max))
		   (insert-buffer-substring buffer)))
	    (erase-buffer)
	    (when (memq (process-status process) '(open run))
	      (setq done process))))))
    (if done
	(progn
	  (message "imap: Opening SSL connection with `%s'...done" cmd)
	  done)
      (message "imap: Failed opening SSL connection")
      nil)))

(defun imap-network-p (buffer)
  t)

(defun imap-network-open (name buffer server port)
  (let* ((port (or port imap-default-port))
	 (coding-system-for-read imap-coding-system-for-read)
	 (coding-system-for-write imap-coding-system-for-write)
	 (process (open-network-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  (goto-char (point-min))
		  (not (imap-parse-greeting)))
	(accept-process-output process 1)
	(sit-for 1))
      (and imap-log
	   (with-current-buffer (get-buffer-create imap-log)
	     (imap-disable-multibyte)
	     (buffer-disable-undo)
	     (goto-char (point-max))
	     (insert-buffer-substring buffer)))
      (when (memq (process-status process) '(open run))
	process))))

(defun imap-starttls-p (buffer)
  (and (condition-case ()
	   (require 'starttls)
	 (error nil))
       (imap-capability 'STARTTLS buffer)))

(defun imap-starttls-open (name buffer server port)
  (let* ((port (or port imap-default-port))
	 (coding-system-for-read imap-coding-system-for-read)
	 (coding-system-for-write imap-coding-system-for-write)
	 (process (starttls-open-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  (goto-char (point-min))
		  (not (imap-parse-greeting)))
	(accept-process-output process 1)
	(sit-for 1))
      (and imap-log
	   (with-current-buffer (get-buffer-create imap-log)
	     (buffer-disable-undo)
	     (goto-char (point-max))
	     (insert-buffer-substring buffer)))
      (let ((imap-process process))
	(unwind-protect
	    (progn
	      (set-process-filter imap-process 'imap-arrival-filter)
	      (when (and (eq imap-stream 'starttls)
			 (imap-ok-p (imap-send-command-wait "STARTTLS")))
		(starttls-negotiate imap-process)))
	  (set-process-filter imap-process nil)))
      (when (memq (process-status process) '(open run))
	process))))
  
;; Server functions; authenticator stuff:

(defun imap-interactive-login (buffer loginfunc)
  "Login to server in BUFFER.
LOGINFUNC is passed a username and a password, it should return t if
it where sucessful authenticating itself to the server, nil otherwise.
Returns t if login was successful, nil otherwise."
  (with-current-buffer buffer
    (make-variable-buffer-local 'imap-username)
    (make-variable-buffer-local 'imap-password)
    (let (user passwd ret)
      ;;      (condition-case ()
      (while (or (not user) (not passwd))
	(setq user (or imap-username
		       (read-from-minibuffer 
			(concat "IMAP username for " imap-server ": ")
			(or user imap-default-user))))
	(setq passwd (or imap-password
			 (imap-read-passwd
			  (concat "IMAP password for " user "@" 
				  imap-server ": "))))
	(when (and user passwd)
	  (if (funcall loginfunc user passwd)
	      (progn
		(setq ret t
		      imap-username user)
		(if (and (not imap-password)
			 (y-or-n-p "Store password for this session? "))
		    (setq imap-password passwd)))
	    (message "Login failed...")
	    (setq passwd nil)
	    (sit-for 1))))
      ;;	(quit (with-current-buffer buffer
      ;;		(setq user nil
      ;;		      passwd nil)))
      ;;	(error (with-current-buffer buffer
      ;;		 (setq user nil
      ;;		       passwd nil))))
      ret)))

(defun imap-gssapi-auth-p (buffer)
  (imap-capability 'AUTH=GSSAPI buffer))

(defun imap-gssapi-auth (buffer)
  (eq imap-stream 'gssapi))

(defun imap-kerberos4-auth-p (buffer)
  (imap-capability 'AUTH=KERBEROS_V4 buffer))

(defun imap-kerberos4-auth (buffer)
  (eq imap-stream 'kerberos4))

(defun imap-cram-md5-p (buffer)
  (imap-capability 'AUTH=CRAM-MD5 buffer))

(defun imap-cram-md5-auth (buffer)
  "Login to server using the AUTH CRAM-MD5 method."
  (imap-interactive-login
   buffer
   (lambda (user passwd)
     (imap-ok-p
      (imap-send-command-wait
       (list
	"AUTHENTICATE CRAM-MD5"
	(lambda (challenge)
	  (let* ((decoded (base64-decode-string challenge))
		 (hash (rfc2104-hash 'md5 64 16 passwd decoded))
		 (response (concat user " " hash))
		 (encoded (base64-encode-string response)))
	    encoded))))))))

(defun imap-login-p (buffer)
  (not (imap-capability 'X-LOGIN-CMD-DISABLED buffer)))

(defun imap-login-auth (buffer)
  "Login to server using the LOGIN command."
  (imap-interactive-login buffer 
			  (lambda (user passwd)
			    (imap-ok-p (imap-send-command-wait 
					(concat "LOGIN \"" user "\" \"" 
						passwd "\""))))))

(defun imap-anonymous-p (buffer)
  t)

(defun imap-anonymous-auth (buffer)
  (with-current-buffer buffer
    (imap-ok-p (imap-send-command-wait
		(concat "LOGIN anonymous \"" (concat (user-login-name) "@" 
						     (system-name)) "\"")))))

(defun imap-digest-md5-p (buffer)
  (and (condition-case ()
	   (require 'digest-md5)
	 (error nil))
       (imap-capability 'AUTH=DIGEST-MD5 buffer)))

(defun imap-digest-md5-auth (buffer)
  "Login to server using the AUTH DIGEST-MD5 method."
  (imap-interactive-login
   buffer
   (lambda (user passwd)
     (let ((tag 
	    (imap-send-command
	     (list
	      "AUTHENTICATE DIGEST-MD5"
	      (lambda (challenge)
		(digest-md5-parse-digest-challenge
		 (base64-decode-string challenge))
		(let* ((digest-uri
			(digest-md5-digest-uri 
			 "imap" (digest-md5-challenge 'realm)))
		       (response
			(digest-md5-digest-response 
			 user passwd digest-uri)))
		  (base64-encode-string response 'no-line-break))))
	     )))
       (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
	   nil
	 (setq imap-continuation nil)
	 (imap-send-command-1 "")
	 (imap-ok-p (imap-wait-for-tag tag)))))))

;; Server functions:

(defun imap-open-1 (buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (setq imap-current-mailbox nil
	  imap-current-message nil
	  imap-state 'initial
	  imap-process (condition-case ()
			   (funcall (nth 2 (assq imap-stream 
						 imap-stream-alist))
				    "imap" buffer imap-server imap-port)
			 ((error quit) nil)))
    (when imap-process
      (set-process-filter imap-process 'imap-arrival-filter)
      (set-process-sentinel imap-process 'imap-sentinel)
      (while (and (eq imap-state 'initial)
		  (memq (process-status imap-process) '(open run)))
	(message "Waiting for response from %s..." imap-server)
	(accept-process-output imap-process 1))
      (message "Waiting for response from %s...done" imap-server)
      (and (memq (process-status imap-process) '(open run))
	   imap-process))))

(defun imap-open (server &optional port stream auth buffer)
  "Open a IMAP connection to host SERVER at PORT returning a buffer.
If PORT is unspecified, a default value is used (143 except
for SSL which use 993).
STREAM indicates the stream to use, see `imap-streams' for available
streams.  If nil, it choices the best stream the server is capable of.
AUTH indicates authenticator to use, see `imap-authenticators' for
available authenticators.  If nil, it choices the best stream the
server is capable of.
BUFFER can be a buffer or a name of a buffer, which is created if
necessery.  If nil, the buffer name is generated."
  (setq buffer (or buffer (format " *imap* %s:%d" server (or port 0))))
  (with-current-buffer (get-buffer-create buffer)
    (if (imap-opened buffer)
	(imap-close buffer))
    (mapcar 'make-variable-buffer-local imap-local-variables)
    (imap-disable-multibyte)
    (buffer-disable-undo)
    (setq imap-server (or server imap-server))
    (setq imap-port (or port imap-port))
    (setq imap-auth (or auth imap-auth))
    (setq imap-stream (or stream imap-stream))
    (when (let ((imap-stream (or imap-stream imap-default-stream)))
	    (imap-open-1 buffer))
      ;; Choose stream.
      (let (stream-changed)
	(when (null imap-stream)
	  (let ((streams imap-streams))
	    (while (setq stream (pop streams))
	      (if (funcall (nth 1 (assq stream imap-stream-alist)) buffer)
		  (setq stream-changed (not (eq (or imap-stream 
						    imap-default-stream)
						stream))
			imap-stream stream
			streams nil)))
	    (unless imap-stream
	      (error "Couldn't figure out a stream for server"))))
	(when stream-changed
	  (message "Reconnecting with %s..." imap-stream)
	  (imap-close buffer)
	  (imap-open-1 buffer)
	  (setq imap-capability nil)))
      (if (imap-opened buffer)
	  ;; Choose authenticator
	  (when (null imap-auth)
	    (let ((auths imap-authenticators))
	      (while (setq auth (pop auths))
		(if (funcall (nth 1 (assq auth imap-authenticator-alist)) 
			     buffer)
		    (setq imap-auth auth
			  auths nil)))
	      (unless imap-auth
		(error "Couldn't figure out authenticator for server"))))))
    (when (imap-opened buffer)
      (setq imap-mailbox-data (make-vector imap-mailbox-prime 0))
      buffer)))

(defun imap-opened (&optional buffer)
  "Return non-nil if connection to imap server in BUFFER is open.
If BUFFER is nil then the current buffer is used."
  (and (setq buffer (get-buffer (or buffer (current-buffer))))
       (buffer-live-p buffer)
       (with-current-buffer buffer
	 (and imap-process
	      (memq (process-status imap-process) '(open run))))))

(defun imap-authenticate (&optional user passwd buffer)
  "Authenticate to server in BUFFER, using current buffer if nil.
It uses the authenticator specified when opening the server.  If the
authenticator requires username/passwords, they are queried from the
user and optionally stored in the buffer.  If USER and/or PASSWD is
specified, the user will not be questioned and the username and/or
password is remembered in the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq imap-state 'nonauth)
      (make-variable-buffer-local 'imap-username)
      (make-variable-buffer-local 'imap-password)
      (if user (setq imap-username user))
      (if passwd (setq imap-password passwd))
      (if (funcall (nth 2 (assq imap-auth imap-authenticator-alist)) buffer)
	  (setq imap-state 'auth)))))

(defun imap-close (&optional buffer)
  "Close connection to server in BUFFER.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (and (imap-opened)
	 (not (imap-ok-p (imap-send-command-wait "LOGOUT")))
	 (message "Server %s didn't let me log out" imap-server))
    (when (and imap-process
	       (memq (process-status imap-process) '(open run)))
      (delete-process imap-process))
    (setq imap-current-mailbox nil
	  imap-current-message nil
	  imap-process nil)
    (erase-buffer)
    t))

(defun imap-capability (&optional identifier buffer)
  "Return a list of identifiers which server in BUFFER support.
If IDENTIFIER, return non-nil if it's among the servers capabilities.
If BUFFER is nil, the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (unless imap-capability
      (unless (imap-ok-p (imap-send-command-wait "CAPABILITY"))
	(setq imap-capability '(IMAP2))))
    (if identifier
	(memq (intern (upcase (symbol-name identifier))) imap-capability)
      imap-capability)))

(defun imap-namespace (&optional buffer)
  "Return a namespace hierarchy at server in BUFFER.
If BUFFER is nil, the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (unless imap-namespace
      (when (imap-capability 'NAMESPACE)
	(imap-send-command-wait "NAMESPACE")))
    imap-namespace))

(defun imap-send-command-wait (command &optional buffer)
  (imap-wait-for-tag (imap-send-command command buffer) buffer))


;; Mailbox functions:

(defun imap-mailbox-put (propname value &optional mailbox buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if imap-mailbox-data
	(put (intern (or mailbox imap-current-mailbox) imap-mailbox-data)
	     propname value)
      (error "Imap-mailbox-data is nil, prop %s value %s mailbox %s buffer %s"
	     propname value mailbox (current-buffer)))
    t))

(defsubst imap-mailbox-get-1 (propname &optional mailbox)
  (get (intern-soft (or mailbox imap-current-mailbox) imap-mailbox-data)
       propname))

(defun imap-mailbox-get (propname &optional mailbox buffer)
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-mailbox-get-1 propname (or mailbox imap-current-mailbox)))))

(defun imap-mailbox-map-1 (func &optional mailbox-decoder buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let (result)
      (mapatoms 
       (lambda (s)
	 (push (funcall func (if mailbox-decoder
				 (funcall mailbox-decoder (symbol-name s))
			       (symbol-name s))) result))
       imap-mailbox-data)
      result)))

(defun imap-mailbox-map (func &optional buffer)
  "Map a function across each mailbox in `imap-mailbox-data', returning a list.
Function should take a mailbox name (a string) as
the only argument."
  (imap-mailbox-map-1 func 'imap-utf7-decode buffer))

(defun imap-current-mailbox (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-utf7-decode imap-current-mailbox)))

(defun imap-current-mailbox-p-1 (mailbox &optional examine)
  (and (string= mailbox imap-current-mailbox)
       (or (and examine
		(eq imap-state 'examine))
	   (and (not examine)
		(eq imap-state 'selected)))))

(defun imap-current-mailbox-p (mailbox &optional examine buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-current-mailbox-p-1 (imap-utf7-encode mailbox) examine)))

(defun imap-mailbox-select-1 (mailbox &optional examine)
  "Select MAILBOX on server in BUFFER.
If EXAMINE is non-nil, do a read-only select."
  (if (imap-current-mailbox-p-1 mailbox examine)
      imap-current-mailbox
    (setq imap-current-mailbox mailbox)
    (if (imap-ok-p (imap-send-command-wait
		    (concat (if examine "EXAMINE" "SELECT") " \"" 
			    mailbox "\"")))
	(progn
	  (setq imap-message-data (make-vector imap-message-prime 0)
		imap-state (if examine 'examine 'selected))
	  imap-current-mailbox)
      ;; Failed SELECT/EXAMINE unselects current mailbox
      (setq imap-current-mailbox nil))))

(defun imap-mailbox-select (mailbox &optional examine buffer)  
  (with-current-buffer (or buffer (current-buffer))
    (imap-utf7-decode 
     (imap-mailbox-select-1 (imap-utf7-encode mailbox) examine))))

(defun imap-mailbox-examine (mailbox &optional buffer)
  "Examine MAILBOX on server in BUFFER."
  (imap-mailbox-select mailbox 'exmine buffer))

(defun imap-mailbox-unselect (&optional buffer)
  "Close current folder in BUFFER, without expunging articles."
  (with-current-buffer (or buffer (current-buffer))
    (when (or (eq imap-state 'auth)
	      (and (imap-capability 'UNSELECT)
		   (imap-ok-p (imap-send-command-wait "UNSELECT")))
	      (and (imap-ok-p 
		    (imap-send-command-wait (concat "EXAMINE \""
						    imap-current-mailbox
						    "\"")))
		   (imap-ok-p (imap-send-command-wait "CLOSE"))))
      (setq imap-current-mailbox nil
	    imap-message-data nil
	    imap-state 'auth)
      t)))

(defun imap-mailbox-expunge (&optional buffer)
  "Expunge articles in current folder in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (when (and imap-current-mailbox (not (eq imap-state 'examine)))
      (imap-ok-p (imap-send-command-wait "EXPUNGE")))))

(defun imap-mailbox-close (&optional buffer)
  "Expunge articles and close current folder in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (when (and imap-current-mailbox
	       (imap-ok-p (imap-send-command-wait "CLOSE")))
      (setq imap-current-mailbox nil
	    imap-message-data nil
	    imap-state 'auth)
      t)))

(defun imap-mailbox-create-1 (mailbox)
  (imap-ok-p (imap-send-command-wait (list "CREATE \"" mailbox "\""))))

(defun imap-mailbox-create (mailbox &optional buffer)
  "Create MAILBOX on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (imap-mailbox-create-1 (imap-utf7-encode mailbox))))

(defun imap-mailbox-delete (mailbox &optional buffer)
  "Delete MAILBOX on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "DELETE \"" mailbox "\""))))))

(defun imap-mailbox-rename (oldname newname &optional buffer)
  "Rename mailbox OLDNAME to NEWNAME on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (let ((oldname (imap-utf7-encode oldname))
	(newname (imap-utf7-encode newname)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "RENAME \"" oldname "\" "
				     "\"" newname "\""))))))

(defun imap-mailbox-lsub (&optional root reference add-delimiter buffer) 
  "Return a list of subscribed mailboxes on server in BUFFER.
If ROOT is non-nil, only list matching mailboxes.  If ADD-DELIMITER is
non-nil, a hierarchy delimiter is added to root.  REFERENCE is a
implementation-specific string that has to be passed to lsub command."
  (with-current-buffer (or buffer (current-buffer))
    ;; Make sure we know the hierarchy separator for root's hierarchy
    (when (and add-delimiter (null (imap-mailbox-get-1 'delimiter root)))
      (imap-send-command-wait (concat "LIST \"" reference "\" \""
				      (imap-utf7-encode root) "\"")))
    ;; clear list data (NB not delimiter and other stuff)
    (imap-mailbox-map-1 (lambda (mailbox)
			  (imap-mailbox-put 'lsub nil mailbox)))
    (when (imap-ok-p
	   (imap-send-command-wait 
	    (concat "LSUB \"" reference "\" \"" (imap-utf7-encode root)
		    (and add-delimiter (imap-mailbox-get-1 'delimiter root))
		    "%\"")))
      (let (out)
	(imap-mailbox-map-1 (lambda (mailbox)
			      (when (imap-mailbox-get-1 'lsub mailbox)
				(push (imap-utf7-decode mailbox) out))))
	(nreverse out)))))

(defun imap-mailbox-list (root &optional reference add-delimiter buffer)
  "Return a list of mailboxes matching ROOT on server in BUFFER.
If ADD-DELIMITER is non-nil, a hierarchy delimiter is added to
root.  REFERENCE is a implementation-specific string that has to be
passed to list command."
  (with-current-buffer (or buffer (current-buffer))
    ;; Make sure we know the hierarchy separator for root's hierarchy
    (when (and add-delimiter (null (imap-mailbox-get-1 'delimiter root)))
      (imap-send-command-wait (concat "LIST \"" reference "\" \""
				      (imap-utf7-encode root) "\"")))
    ;; clear list data (NB not delimiter and other stuff)
    (imap-mailbox-map-1 (lambda (mailbox)
			  (imap-mailbox-put 'list nil mailbox)))
    (when (imap-ok-p
	   (imap-send-command-wait 
	    (concat "LIST \"" reference "\" \"" (imap-utf7-encode root)
		    (and add-delimiter (imap-mailbox-get-1 'delimiter root))
		    "%\"")))
      (let (out)
	(imap-mailbox-map-1 (lambda (mailbox)
			      (when (imap-mailbox-get-1 'list mailbox)
				(push (imap-utf7-decode mailbox) out))))
	(nreverse out)))))

(defun imap-mailbox-subscribe (mailbox &optional buffer)
  "Send the SUBSCRIBE command on the mailbox to server in BUFFER.
Returns non-nil if successful."
  (with-current-buffer (or buffer (current-buffer))
    (imap-ok-p (imap-send-command-wait (concat "SUBSCRIBE \"" 
					       (imap-utf7-encode mailbox)
					       "\"")))))

(defun imap-mailbox-unsubscribe (mailbox &optional buffer)
  "Send the SUBSCRIBE command on the mailbox to server in BUFFER.
Returns non-nil if successful."
  (with-current-buffer (or buffer (current-buffer))
    (imap-ok-p (imap-send-command-wait (concat "UNSUBSCRIBE " 
					       (imap-utf7-encode mailbox)
					       "\"")))))

(defun imap-mailbox-status (mailbox items &optional buffer)
  "Get status items ITEM in MAILBOX from server in BUFFER.
ITEMS can be a symbol or a list of symbols, valid symbols are one of
the STATUS data items -- ie 'messages, 'recent, 'uidnext, 'uidvalidity
or 'unseen.  If ITEMS is a list of symbols, a list of values is
returned, if ITEMS is a symbol only it's value is returned."
  (with-current-buffer (or buffer (current-buffer))
    (when (imap-ok-p 
	   (imap-send-command-wait (list "STATUS \""
					 (imap-utf7-encode mailbox)
					 "\" "
					 (format "%s"
						 (if (listp items)
						     items 
						   (list items))))))
      (if (listp items)
	  (mapcar (lambda (item)
		    (imap-mailbox-get item mailbox))
		  items)
	(imap-mailbox-get items mailbox)))))

(defun imap-mailbox-acl-get (&optional mailbox buffer)
  "Get ACL on mailbox from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (when (imap-ok-p
	     (imap-send-command-wait (list "GETACL \""
					   (or mailbox imap-current-mailbox)
					   "\"")))
	(imap-mailbox-get-1 'acl (or mailbox imap-current-mailbox))))))

(defun imap-mailbox-acl-set (identifier rights &optional mailbox buffer)
  "Change/set ACL for IDENTIFIER to RIGHTS in MAILBOX from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "SETACL \""
				     (or mailbox imap-current-mailbox)
				     "\" "
				     identifier
				     " "
				     rights))))))

(defun imap-mailbox-acl-delete (identifier &optional mailbox buffer)
  "Removes any <identifier,rights> pair for IDENTIFIER in MAILBOX from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "DELETEACL \""
				     (or mailbox imap-current-mailbox)
				     "\" "
				     identifier))))))


;; Message functions:

(defun imap-current-message (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    imap-current-message))

(defun imap-list-to-message-set (list)
  (mapconcat (lambda (item)
	       (number-to-string item))
	     (if (listp list)
		 list
	       (list list))
	     ","))

(defun imap-fetch-asynch (uids props &optional nouidfetch buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-send-command (format "%sFETCH %s %s" (if nouidfetch "" "UID ")
			       (if (listp uids)
				   (imap-list-to-message-set uids)
				 uids)
			       props))))

(defun imap-fetch (uids props &optional receive nouidfetch buffer)
  "Fetch properties PROPS from message set UIDS from server in BUFFER.
UIDS can be a string, number or a list of numbers.  If RECEIVE
is non-nil return theese properties."
  (with-current-buffer (or buffer (current-buffer))
    (when (imap-ok-p (imap-send-command-wait 
		      (format "%sFETCH %s %s" (if nouidfetch "" "UID ")
			      (if (listp uids)
				  (imap-list-to-message-set uids)
				uids)
			      props)))
      (if (or (null receive) (stringp uids))
	  t
	(if (listp uids)
	    (mapcar (lambda (uid)
		      (if (listp receive)
			  (mapcar (lambda (prop)
				    (imap-message-get uid prop))
				  receive)
			(imap-message-get uid receive)))
		    uids)
	  (imap-message-get uids receive))))))
    
(defun imap-message-put (uid propname value &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if imap-message-data
	(put (intern (number-to-string uid) imap-message-data)
	     propname value)
      (error "Imap-message-data is nil, uid %s prop %s value %s buffer %s"
	     uid propname value (current-buffer)))
    t))

(defun imap-message-get (uid propname &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (get (intern-soft (number-to-string uid) imap-message-data)
	 propname)))

(defun imap-message-map (func propname &optional buffer)
  "Map a function across each mailbox in `imap-message-data', returning a list."
  (with-current-buffer (or buffer (current-buffer))
    (let (result)
      (mapatoms
       (lambda (s)
	 (push (funcall func (get s 'UID) (get s propname)) result))
       imap-message-data)
      result)))

(defmacro imap-message-envelope-date (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 0)))

(defmacro imap-message-envelope-subject (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 1)))

(defmacro imap-message-envelope-from (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 2)))

(defmacro imap-message-envelope-sender (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 3)))

(defmacro imap-message-envelope-reply-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 4)))

(defmacro imap-message-envelope-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 5)))

(defmacro imap-message-envelope-cc (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 6)))

(defmacro imap-message-envelope-bcc (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 7)))

(defmacro imap-message-envelope-in-reply-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 8)))

(defmacro imap-message-envelope-message-id (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 9)))

(defmacro imap-message-body (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (imap-message-get ,uid 'BODY)))

(defun imap-search (predicate &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-mailbox-put 'search 'dummy)
    (when (imap-ok-p (imap-send-command-wait (concat "UID SEARCH " predicate)))
      (if (eq (imap-mailbox-get-1 'search imap-current-mailbox) 'dummy)
	  (error "Missing SEARCH response to a SEARCH command")
	(imap-mailbox-get-1 'search imap-current-mailbox)))))

(defun imap-message-flag-permanent-p (flag &optional mailbox buffer)
  "Return t iff FLAG can be permanently (between IMAP sessions) saved on articles, in MAILBOX on server in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (member "\\*" (imap-mailbox-get 'permanentflags mailbox))
	(member flag (imap-mailbox-get 'permanentflags mailbox)))))

(defun imap-message-flags-set (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " FLAGS" (if silent ".SILENT") " (" flags ")"))))))

(defun imap-message-flags-del (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " -FLAGS" (if silent ".SILENT") " (" flags ")"))))))

(defun imap-message-flags-add (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " +FLAGS" (if silent ".SILENT") " (" flags ")"))))))

(defun imap-message-copyuid-1 (mailbox)
  (if (imap-capability 'UIDPLUS)
      (list (nth 0 (imap-mailbox-get-1 'copyuid mailbox))
	    (string-to-number (nth 2 (imap-mailbox-get-1 'copyuid mailbox))))
    (let ((old-mailbox imap-current-mailbox)
	  (state imap-state)
	  (imap-message-data (make-vector 2 0)))
      (when (imap-mailbox-examine mailbox)
	(prog1
	    (and (imap-fetch "*" "UID")
		 (list (imap-mailbox-get-1 'uidvalidity mailbox)
		       (apply 'max (imap-message-map
				    (lambda (uid prop) uid) 'UID))))
	  (if old-mailbox
	      (imap-mailbox-select old-mailbox (eq state 'examine))
	    (imap-mailbox-unselect)))))))

(defun imap-message-copyuid (mailbox &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-message-copyuid-1 (imap-utf7-decode mailbox))))

(defun imap-message-copy (articles mailbox
				   &optional dont-create no-copyuid buffer)
  "Copy ARTICLES (a string message set) to MAILBOX on server in
BUFFER, creating mailbox if it doesn't exist.  If dont-create is
non-nil, it will not create a mailbox.  On success, return a list with
the UIDVALIDITY of the mailbox the article(s) was copied to as the
first element, rest of list contain the saved articles' UIDs."
  (when articles
    (with-current-buffer (or buffer (current-buffer))
      (let ((mailbox (imap-utf7-encode mailbox)))
	(if (let ((cmd (concat "UID COPY " articles " \"" mailbox "\""))
		  (imap-current-target-mailbox mailbox))
	      (if (imap-ok-p (imap-send-command-wait cmd))
		  t
		(when (and (not dont-create)
			   (imap-mailbox-get-1 'trycreate mailbox))
		  (imap-mailbox-create-1 mailbox)
		  (imap-ok-p (imap-send-command-wait cmd)))))
	    (or no-copyuid
		(imap-message-copyuid-1 mailbox)))))))
      
(defun imap-message-appenduid-1 (mailbox)
  (if (imap-capability 'UIDPLUS)
      (imap-mailbox-get-1 'appenduid mailbox)
    (let ((old-mailbox imap-current-mailbox)
	  (state imap-state)
	  (imap-message-data (make-vector 2 0)))
      (when (imap-mailbox-examine mailbox)
	(prog1
	    (and (imap-fetch "*" "UID")
		 (list (imap-mailbox-get-1 'uidvalidity mailbox)
		       (apply 'max (imap-message-map
				    (lambda (uid prop) uid) 'UID))))
	  (if old-mailbox
	      (imap-mailbox-select old-mailbox (eq state 'examine))
	    (imap-mailbox-unselect)))))))

(defun imap-message-appenduid (mailbox &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-message-appenduid-1 (imap-utf7-encode mailbox))))

(defun imap-message-append (mailbox article &optional flags date-time buffer)
  "Append ARTICLE (a buffer) to MAILBOX on server in BUFFER.
FLAGS and DATE-TIME is currently not used.  Return a cons holding
uidvalidity of MAILBOX and UID the newly created article got, or nil
on failure."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (and (let ((imap-current-target-mailbox mailbox))
	     (imap-ok-p 
	      (imap-send-command-wait 
	       (list "APPEND \"" mailbox "\" "  article))))
	   (imap-message-appenduid-1 mailbox)))))
  
(defun imap-body-lines (body)
  "Return number of lines in article by looking at the mime bodystructure BODY."
  (if (listp body)
      (if (stringp (car body))
          ;; upcase for bug in courier imap server
	  (cond ((and (string= (upcase (car body)) "TEXT")
		      (numberp (nth 7 body)))
		 (nth 7 body))
		((and (string= (upcase (car body)) "MESSAGE")
		      (numberp (nth 9 body)))
		 (nth 9 body))
		(t 0))
	(apply '+ (mapcar 'imap-body-lines body)))
    0))

(defun imap-envelope-from (from)
  "Return a from string line."
  (and from
       (concat (aref from 0)
	       (if (aref from 0) " <")
	       (aref from 2) 
	       "@" 
	       (aref from 3)
	       (if (aref from 0) ">"))))


;; Internal functions.

(defun imap-send-command-1 (cmdstr)
  (setq cmdstr (concat cmdstr imap-client-eol))
  (and imap-log
       (with-current-buffer (get-buffer-create imap-log)
	 (imap-disable-multibyte)
	 (buffer-disable-undo)
	 (goto-char (point-max))
	 (insert cmdstr)))
  (process-send-string imap-process cmdstr))

(defun imap-send-command (command &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (not (listp command)) (setq command (list command)))
    (let ((tag (setq imap-tag (1+ imap-tag)))
	  cmd cmdstr)
      (setq cmdstr (concat (number-to-string imap-tag) " "))
      (while (setq cmd (pop command))
	(cond ((stringp cmd)
	       (setq cmdstr (concat cmdstr cmd)))
	      ((bufferp cmd)
	       (setq cmdstr 
		     (concat cmdstr (format "{%d}" (with-current-buffer cmd
						     (buffer-size)))))
	       (unwind-protect
		   (progn
		     (imap-send-command-1 cmdstr)
		     (setq cmdstr nil)
		     (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
			 (setq command nil);; abort command if no cont-req
		       (let ((process imap-process)
			     (stream imap-stream)
			     (eol imap-client-eol))
			 (with-current-buffer cmd
			   (when (not (equal eol "\r\n"))
			     ;; XXX modifies buffer!
			     (goto-char (point-min))
			     (while (search-forward "\r\n" nil t)
			       (replace-match eol)))
			   (and imap-log
				(with-current-buffer (get-buffer-create
						      imap-log)
				  (imap-disable-multibyte)
				  (buffer-disable-undo)
				  (goto-char (point-max))
				  (insert-buffer-substring cmd)))
			   (process-send-region process (point-min)
						(point-max)))
			 (process-send-string process imap-client-eol))))
		 (setq imap-continuation nil)))
	      ((functionp cmd)
	       (imap-send-command-1 cmdstr)
	       (setq cmdstr nil)
	       (unwind-protect
		   (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
		       (setq command nil);; abort command if no cont-req
		     (setq command (cons (funcall cmd imap-continuation)
					 command)))
		 (setq imap-continuation nil)))
	      (t
	       (error "Unknown command type"))))
      (if cmdstr
	  (imap-send-command-1 cmdstr))
      tag)))

(defun imap-wait-for-tag (tag &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (while (and (null imap-continuation)
		(< imap-reached-tag tag))
      (or (and (not (memq (process-status imap-process) '(open run)))
	       (sit-for 1))
	  (accept-process-output imap-process 1)))
    (or (assq tag imap-failed-tags)
	(if imap-continuation
	    'INCOMPLETE
	  'OK))))

(defun imap-sentinel (process string)
  (delete-process process))

(defun imap-find-next-line ()
  "Return point at end of current line, taking into account literals.
Return nil if no complete line has arrived."
  (when (re-search-forward (concat imap-server-eol "\\|{\\([0-9]+\\)}"
				   imap-server-eol)
			   nil t)
    (if (match-string 1)
	(if (< (point-max) (+ (point) (string-to-number (match-string 1))))
	    nil
	  (goto-char (+ (point) (string-to-number (match-string 1))))
	  (imap-find-next-line))
      (point))))

(defun imap-arrival-filter (proc string)
  "IMAP process filter."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (and imap-log
	 (with-current-buffer (get-buffer-create imap-log)
	   (imap-disable-multibyte)
	   (buffer-disable-undo)
	   (goto-char (point-max))
	   (insert string)))
    (let (end)
      (goto-char (point-min))
      (while (setq end (imap-find-next-line))
	(save-restriction
	  (narrow-to-region (point-min) end)
	  (delete-backward-char (length imap-server-eol))
	  (goto-char (point-min))
	  (unwind-protect
	      (cond ((eq imap-state 'initial)
		     (imap-parse-greeting))
		    ((or (eq imap-state 'auth)
			 (eq imap-state 'nonauth)
			 (eq imap-state 'selected)
			 (eq imap-state 'examine))
		     (imap-parse-response))
		    (t
		     (message "Unknown state %s in arrival filter" 
			      imap-state)))
	    (delete-region (point-min) (point-max))))))))


;; Imap parser.

(defsubst imap-forward ()
  (or (eobp) (forward-char)))

;;   number          = 1*DIGIT
;;                       ; Unsigned 32-bit integer
;;                       ; (0 <= n < 4,294,967,296)

(defsubst imap-parse-number ()
  (when (looking-at "[0-9]+")
    (prog1
	(string-to-number (match-string 0))
      (goto-char (match-end 0)))))

;;   literal         = "{" number "}" CRLF *CHAR8
;;                       ; Number represents the number of CHAR8s

(defsubst imap-parse-literal ()
  (when (looking-at "{\\([0-9]+\\)}\r\n")
    (let ((pos (match-end 0))
	  (len (string-to-number (match-string 1))))
      (if (< (point-max) (+ pos len))
	  nil
	(goto-char (+ pos len))
	(buffer-substring pos (+ pos len))))))

;;   string          = quoted / literal
;;
;;   quoted          = DQUOTE *QUOTED-CHAR DQUOTE
;;
;;   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;                     "\" quoted-specials
;;
;;   quoted-specials = DQUOTE / "\"
;;
;;   TEXT-CHAR       = <any CHAR except CR and LF>

(defsubst imap-parse-string ()
  (cond ((eq (char-after) ?\")
	 (forward-char 1)
	 (let ((p (point)) (name ""))
	   (skip-chars-forward "^\"\\\\")
	   (setq name (buffer-substring p (point)))
	   (while (eq (char-after) ?\\)
	     (setq p (1+ (point)))
	     (forward-char 2)
	     (skip-chars-forward "^\"\\\\")
	     (setq name (concat name (buffer-substring p (point)))))
	   (forward-char 1)
	   name))
	((eq (char-after) ?{)
	 (imap-parse-literal))))

;;   nil             = "NIL"

(defsubst imap-parse-nil ()
  (if (looking-at "NIL")
      (goto-char (match-end 0))))

;;   nstring         = string / nil

(defsubst imap-parse-nstring ()
  (or (imap-parse-string)
      (and (imap-parse-nil)
	   nil)))

;;   astring         = atom / string
;;
;;   atom            = 1*ATOM-CHAR
;;
;;   ATOM-CHAR       = <any CHAR except atom-specials>
;;
;;   atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
;;                     quoted-specials
;;
;;   list-wildcards  = "%" / "*"
;;
;;   quoted-specials = DQUOTE / "\"

(defsubst imap-parse-astring ()
  (or (imap-parse-string)
      (buffer-substring (point) 
			(if (re-search-forward "[(){ \r\n%*\"\\]" nil t)
			    (goto-char (1- (match-end 0)))
			  (end-of-line)
			  (point)))))

;;   address         = "(" addr-name SP addr-adl SP addr-mailbox SP
;;                      addr-host ")"
;;
;;   addr-adl        = nstring
;;                       ; Holds route from [RFC-822] route-addr if
;;                       ; non-NIL
;;
;;   addr-host       = nstring
;;                       ; NIL indicates [RFC-822] group syntax.
;;                       ; Otherwise, holds [RFC-822] domain name
;;
;;   addr-mailbox    = nstring
;;                       ; NIL indicates end of [RFC-822] group; if
;;                       ; non-NIL and addr-host is NIL, holds
;;                       ; [RFC-822] group name.
;;                       ; Otherwise, holds [RFC-822] local-part
;;                       ; after removing [RFC-822] quoting
;;
;;   addr-name       = nstring
;;                       ; If non-NIL, holds phrase from [RFC-822]
;;                       ; mailbox after removing [RFC-822] quoting
;;

(defsubst imap-parse-address ()
  (let (address)
    (when (eq (char-after) ?\()
      (imap-forward)
      (setq address (vector (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (imap-parse-nstring)))
      (when (eq (char-after) ?\))
	(imap-forward)
	address))))

;;   address-list    = "(" 1*address ")" / nil
;;
;;   nil             = "NIL"

(defsubst imap-parse-address-list ()
  (if (eq (char-after) ?\()
      (let (address addresses)
	(imap-forward)
	(while (and (not (eq (char-after) ?\)))
		    ;; next line for MS Exchange bug
		    (progn (and (eq (char-after) ? ) (imap-forward)) t)
		    (setq address (imap-parse-address)))
	  (setq addresses (cons address addresses)))
	(when (eq (char-after) ?\))
	  (imap-forward)
	  (nreverse addresses)))
    (assert (imap-parse-nil))))

;;   mailbox         = "INBOX" / astring
;;                       ; INBOX is case-insensitive.  All case variants of
;;                       ; INBOX (e.g. "iNbOx") MUST be interpreted as INBOX
;;                       ; not as an astring.  An astring which consists of
;;                       ; the case-insensitive sequence "I" "N" "B" "O" "X"
;;                       ; is considered to be INBOX and not an astring.
;;                       ;  Refer to section 5.1 for further
;;                       ; semantic details of mailbox names.

(defsubst imap-parse-mailbox ()
  (let ((mailbox (imap-parse-astring)))
    (if (string-equal "INBOX" (upcase mailbox))
	"INBOX"
      mailbox)))

;;   greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF
;;
;;   resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
;;                       ; Authentication condition
;;
;;   resp-cond-bye   = "BYE" SP resp-text

(defun imap-parse-greeting ()
  "Parse a IMAP greeting."
  (cond ((looking-at "\\* OK ")
	 (setq imap-state 'nonauth))
	((looking-at "\\* PREAUTH ")
	 (setq imap-state 'auth))
	((looking-at "\\* BYE ")
	 (setq imap-state 'closed))))

;;   response        = *(continue-req / response-data) response-done
;;
;;   continue-req    = "+" SP (resp-text / base64) CRLF
;;
;;   response-data   = "*" SP (resp-cond-state / resp-cond-bye /
;;                     mailbox-data / message-data / capability-data) CRLF
;;
;;   response-done   = response-tagged / response-fatal
;;
;;   response-fatal  = "*" SP resp-cond-bye CRLF
;;                       ; Server closes connection immediately
;;
;;   response-tagged = tag SP resp-cond-state CRLF
;;
;;   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
;;                       ; Status condition
;;
;;   resp-cond-bye   = "BYE" SP resp-text
;;
;;   mailbox-data    =  "FLAGS" SP flag-list /
;;  		        "LIST" SP mailbox-list /
;;                      "LSUB" SP mailbox-list /
;;		        "SEARCH" *(SP nz-number) /
;;                      "STATUS" SP mailbox SP "("
;;	                      [status-att SP number *(SP status-att SP number)] ")" /
;;                      number SP "EXISTS" /
;;		        number SP "RECENT"
;;
;;   message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))
;;
;;   capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
;;                     *(SP capability)
;;                       ; IMAP4rev1 servers which offer RFC 1730
;;                       ; compatibility MUST list "IMAP4" as the first
;;                       ; capability.

(defun imap-parse-response ()
  "Parse a IMAP command response."
  (let (token)
    (case (setq token (read (current-buffer)))
      (+ (setq imap-continuation
	       (or (buffer-substring (min (point-max) (1+ (point)))
				     (point-max))
		   t)))
      (* (case (prog1 (setq token (read (current-buffer)))
		 (imap-forward))
	   (OK         (imap-parse-resp-text))
	   (NO         (imap-parse-resp-text))
	   (BAD        (imap-parse-resp-text))
	   (BYE        (imap-parse-resp-text))
	   (FLAGS      (imap-mailbox-put 'flags (imap-parse-flag-list)))
	   (LIST       (imap-parse-data-list 'list))
	   (LSUB       (imap-parse-data-list 'lsub))
	   (SEARCH     (imap-mailbox-put 
			'search 
			(read (concat "(" (buffer-substring (point) (point-max)) ")"))))
	   (STATUS     (imap-parse-status))
	   (CAPABILITY (setq imap-capability 
			     (read (concat "(" (upcase (buffer-substring
							(point) (point-max)))
					   ")"))))
	   (ACL        (imap-parse-acl))
	   (t       (case (prog1 (read (current-buffer))
			    (imap-forward))
		      (EXISTS  (imap-mailbox-put 'exists token))
		      (RECENT  (imap-mailbox-put 'recent token))
		      (EXPUNGE t)
		      (FETCH   (imap-parse-fetch token))
		      (t       (message "Garbage: %s" (buffer-string)))))))
      (t (let (status)
	   (if (not (integerp token))
	       (message "Garbage: %s" (buffer-string))
	     (case (prog1 (setq status (read (current-buffer)))
		     (imap-forward))
	       (OK  (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (imap-parse-resp-text)))
	       (NO  (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (save-excursion
			(imap-parse-resp-text))
		      (let (code text)
			(when (eq (char-after) ?\[)
			  (setq code (buffer-substring (point)
						       (search-forward "]")))
			  (imap-forward))
			(setq text (buffer-substring (point) (point-max)))
			(push (list token status code text) 
			      imap-failed-tags))))
	       (BAD (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (save-excursion
			(imap-parse-resp-text))
		      (let (code text)
			(when (eq (char-after) ?\[)
			  (setq code (buffer-substring (point)
						       (search-forward "]")))
			  (imap-forward))
			(setq text (buffer-substring (point) (point-max)))
			(push (list token status code text) imap-failed-tags)
			(error "Internal error, tag %s status %s code %s text %s"
			       token status code text))))
	       (t   (message "Garbage: %s" (buffer-string))))))))))

;;   resp-text       = ["[" resp-text-code "]" SP] text
;;
;;   text            = 1*TEXT-CHAR
;;
;;   TEXT-CHAR       = <any CHAR except CR and LF>

(defun imap-parse-resp-text ()
  (imap-parse-resp-text-code))

;;   resp-text-code  = "ALERT" /
;;                     "BADCHARSET [SP "(" astring *(SP astring) ")" ] /
;;                     "NEWNAME" SP string SP string / 
;;		       "PARSE" /
;;                     "PERMANENTFLAGS" SP "(" 
;;                               [flag-perm *(SP flag-perm)] ")" /
;;                     "READ-ONLY" / 
;;		       "READ-WRITE" / 
;;	 	       "TRYCREATE" /
;;                     "UIDNEXT" SP nz-number / 
;;		       "UIDVALIDITY" SP nz-number /
;;                     "UNSEEN" SP nz-number /
;;                     resp-text-atom [SP 1*<any TEXT-CHAR except "]">]
;;
;;   resp_code_apnd  = "APPENDUID" SPACE nz_number SPACE uniqueid
;;
;;   resp_code_copy  = "COPYUID" SPACE nz_number SPACE set SPACE set
;;
;;   set             = sequence-num / (sequence-num ":" sequence-num) /
;;                        (set "," set)
;;                          ; Identifies a set of messages.  For message
;;                          ; sequence numbers, these are consecutive
;;                          ; numbers from 1 to the number of messages in
;;                          ; the mailbox
;;                          ; Comma delimits individual numbers, colon
;;                          ; delimits between two numbers inclusive.
;;                          ; Example: 2,4:7,9,12:* is 2,4,5,6,7,9,12,13,
;;                          ; 14,15 for a mailbox with 15 messages.
;; 
;;   sequence-num    = nz-number / "*"
;;                          ; * is the largest number in use.  For message
;;                          ; sequence numbers, it is the number of messages
;;                          ; in the mailbox.  For unique identifiers, it is
;;                          ; the unique identifier of the last message in
;;                          ; the mailbox.
;;
;;   flag-perm       = flag / "\*"
;;
;;   flag            = "\Answered" / "\Flagged" / "\Deleted" /
;;                     "\Seen" / "\Draft" / flag-keyword / flag-extension
;;                       ; Does not include "\Recent"
;;
;;   flag-extension  = "\" atom
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept flag-extension flags.  Server
;;                       ; implementations MUST NOT generate
;;                       ; flag-extension flags except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.
;;
;;   flag-keyword    = atom
;;
;;   resp-text-atom  = 1*<any ATOM-CHAR except "]">

(defun imap-parse-resp-text-code ()
  (when (eq (char-after) ?\[)
    (imap-forward)
    (cond ((search-forward "PERMANENTFLAGS " nil t)
	   (imap-mailbox-put 'permanentflags (imap-parse-flag-list)))
	  ((search-forward "UIDNEXT " nil t)
	   (imap-mailbox-put 'uidnext (read (current-buffer))))
	  ((search-forward "UNSEEN " nil t)
	   (imap-mailbox-put 'unseen (read (current-buffer))))
	  ((looking-at "UIDVALIDITY \\([0-9]+\\)")
	   (imap-mailbox-put 'uidvalidity (match-string 1)))
	  ((search-forward "READ-ONLY" nil t)
	   (imap-mailbox-put 'read-only t))
	  ((search-forward "NEWNAME " nil t)
	   (let (oldname newname)
	     (setq oldname (imap-parse-string))
	     (imap-forward)
	     (setq newname (imap-parse-string))
	     (imap-mailbox-put 'newname newname oldname)))
	  ((search-forward "TRYCREATE" nil t)
	   (imap-mailbox-put 'trycreate t imap-current-target-mailbox))
	  ((looking-at "APPENDUID \\([0-9]+\\) \\([0-9]+\\)")
	   (imap-mailbox-put 'appenduid
			     (list (match-string 1)
				   (string-to-number (match-string 2)))
			     imap-current-target-mailbox))
	  ((looking-at "COPYUID \\([0-9]+\\) \\([0-9,:]+\\) \\([0-9,:]+\\)")
	   (imap-mailbox-put 'copyuid (list (match-string 1)
					    (match-string 2)
					    (match-string 3))
			     imap-current-target-mailbox))
	  ((search-forward "ALERT] " nil t)
	   (message "Imap server %s information: %s" imap-server
		    (buffer-substring (point) (point-max)))))))

;;   mailbox-list    = "(" [mbx-list-flags] ")" SP
;;                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
;;
;;   mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
;;                     *(SP mbx-list-oflag) /
;;                     mbx-list-oflag *(SP mbx-list-oflag)
;;
;;   mbx-list-oflag  = "\Noinferiors" / flag-extension
;;                       ; Other flags; multiple possible per LIST response
;;
;;   mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
;;                       ; Selectability flags; only one per LIST response
;;
;;   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;                     "\" quoted-specials
;;
;;   quoted-specials = DQUOTE / "\"

(defun imap-parse-data-list (type)
  (let (flags delimiter mailbox)
    (setq flags (imap-parse-flag-list))
    (when (looking-at " NIL\\| \"\\\\?\\(.\\)\"")
      (setq delimiter (match-string 1))
      (goto-char (1+ (match-end 0)))
      (when (setq mailbox (imap-parse-mailbox))
	(imap-mailbox-put type t mailbox)
	(imap-mailbox-put 'list-flags flags mailbox)
	(imap-mailbox-put 'delimiter delimiter mailbox)))))

;;  msg_att         ::= "(" 1#("ENVELOPE" SPACE envelope /
;;                      "FLAGS" SPACE "(" #(flag / "\Recent") ")" /
;;                      "INTERNALDATE" SPACE date_time /
;;                      "RFC822" [".HEADER" / ".TEXT"] SPACE nstring /
;;                      "RFC822.SIZE" SPACE number /
;;                      "BODY" ["STRUCTURE"] SPACE body /
;;                      "BODY" section ["<" number ">"] SPACE nstring /
;;                      "UID" SPACE uniqueid) ")"
;;  
;;  date_time       ::= <"> date_day_fixed "-" date_month "-" date_year
;;                      SPACE time SPACE zone <">
;;  
;;  section         ::= "[" [section_text / (nz_number *["." nz_number]
;;                      ["." (section_text / "MIME")])] "]"
;;  
;;  section_text    ::= "HEADER" / "HEADER.FIELDS" [".NOT"]
;;                      SPACE header_list / "TEXT"
;;  
;;  header_fld_name ::= astring
;;  
;;  header_list     ::= "(" 1#header_fld_name ")"

(defsubst imap-parse-header-list ()
  (when (eq (char-after) ?\()
    (let (strlist)
      (while (not (eq (char-after) ?\)))
	(imap-forward)
	(push (imap-parse-astring) strlist))
      (imap-forward)
      (nreverse strlist))))

(defsubst imap-parse-fetch-body-section ()
  (let ((section 
	 (buffer-substring (point) (1- (re-search-forward "[] ]" nil t)))))
    (if (eq (char-before) ? )
	(prog1
	    (mapconcat 'identity (cons section (imap-parse-header-list)) " ")
	  (search-forward "]" nil t))
      section)))

(defun imap-parse-fetch (response)
  (when (eq (char-after) ?\()
    (let (uid flags envelope internaldate rfc822 rfc822header rfc822text 
	      rfc822size body bodydetail bodystructure)
      (while (not (eq (char-after) ?\)))
	(imap-forward)
	(let ((token (read (current-buffer))))
	  (imap-forward)
	  (cond ((eq token 'UID)
		 (setq uid (ignore-errors (read (current-buffer)))))
		((eq token 'FLAGS)
		 (setq flags (imap-parse-flag-list)))
		((eq token 'ENVELOPE)
		 (setq envelope (imap-parse-envelope)))
		((eq token 'INTERNALDATE)
		 (setq internaldate (imap-parse-string)))
		((eq token 'RFC822)
		 (setq rfc822 (imap-parse-nstring)))
		((eq token 'RFC822.HEADER)
		 (setq rfc822header (imap-parse-nstring)))
		((eq token 'RFC822.TEXT)
		 (setq rfc822text (imap-parse-nstring)))
		((eq token 'RFC822.SIZE)
		 (setq rfc822size (read (current-buffer))))
		((eq token 'BODY)
		 (if (eq (char-before) ?\[)
		     (push (list
			    (upcase (imap-parse-fetch-body-section))
			    (and (eq (char-after) ?<)
				 (buffer-substring (1+ (point))
						   (search-forward ">" nil t)))
			    (progn (imap-forward)
				   (imap-parse-nstring)))
			   bodydetail)
		   (setq body (imap-parse-body))))
		((eq token 'BODYSTRUCTURE)
		 (setq bodystructure (imap-parse-body))))))
      (when uid
	(setq imap-current-message uid)
	(imap-message-put uid 'UID uid)
	(and flags (imap-message-put uid 'FLAGS flags))
	(and envelope (imap-message-put uid 'ENVELOPE envelope))
	(and internaldate (imap-message-put uid 'INTERNALDATE internaldate))
	(and rfc822 (imap-message-put uid 'RFC822 rfc822))
	(and rfc822header (imap-message-put uid 'RFC822.HEADER rfc822header))
	(and rfc822text (imap-message-put uid 'RFC822.TEXT rfc822text))
	(and rfc822size (imap-message-put uid 'RFC822.SIZE rfc822size))
	(and body (imap-message-put uid 'BODY body))
	(and bodydetail (imap-message-put uid 'BODYDETAIL bodydetail))
	(and bodystructure (imap-message-put uid 'BODYSTRUCTURE bodystructure))
	(run-hooks 'imap-fetch-data-hook)))))

;;   mailbox-data    =  ...
;;                      "STATUS" SP mailbox SP "("
;;	                      [status-att SP number 
;;                            *(SP status-att SP number)] ")"
;;                      ...
;;
;;   status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
;;                     "UNSEEN"

(defun imap-parse-status ()
  (let ((mailbox (imap-parse-mailbox)))
    (when (and mailbox (search-forward "(" nil t))
      (while (not (eq (char-after) ?\)))
	(let ((token (read (current-buffer))))
	  (cond ((eq token 'MESSAGES)
		 (imap-mailbox-put 'messages (read (current-buffer)) mailbox))
		((eq token 'RECENT)
		 (imap-mailbox-put 'recent (read (current-buffer)) mailbox))
		((eq token 'UIDNEXT)
		 (imap-mailbox-put 'uidnext (read (current-buffer)) mailbox))
		((eq token 'UIDVALIDITY)
		 (and (looking-at " \\([0-9]+\\)")
		      (imap-mailbox-put 'uidvalidity (match-string 1) mailbox)
		      (goto-char (match-end 1))))
		((eq token 'UNSEEN)
		 (imap-mailbox-put 'unseen (read (current-buffer)) mailbox))
		(t
		 (message "Unknown status data %s in mailbox %s ignored" 
			  token mailbox))))))))

;;   acl_data        ::= "ACL" SPACE mailbox *(SPACE identifier SPACE
;;                        rights)
;;
;;   identifier      ::= astring
;;
;;   rights          ::= astring

(defun imap-parse-acl ()
  (let ((mailbox (imap-parse-mailbox))
	identifier rights acl)
    (while (eq (char-after) ?\ )
      (imap-forward)
      (setq identifier (imap-parse-astring))
      (imap-forward)
      (setq rights (imap-parse-astring))
      (setq acl (append acl (list (cons identifier rights)))))
    (imap-mailbox-put 'acl acl mailbox)))

;;   flag-list       = "(" [flag *(SP flag)] ")"
;;
;;   flag            = "\Answered" / "\Flagged" / "\Deleted" /
;;                     "\Seen" / "\Draft" / flag-keyword / flag-extension
;;                       ; Does not include "\Recent"
;;
;;   flag-keyword    = atom
;;
;;   flag-extension  = "\" atom
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept flag-extension flags.  Server
;;                       ; implementations MUST NOT generate
;;                       ; flag-extension flags except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.

(defun imap-parse-flag-list ()
  (let ((str (buffer-substring (point) (search-forward ")" nil t)))
	pos)
    (while (setq pos (string-match "\\\\" str (and pos (+ 2 pos))))
      (setq str (replace-match "\\\\" nil t str)))
    (mapcar 'symbol-name (read str))))

;;   envelope        = "(" env-date SP env-subject SP env-from SP env-sender SP
;;                     env-reply-to SP env-to SP env-cc SP env-bcc SP
;;                     env-in-reply-to SP env-message-id ")"
;;
;;   env-bcc         = "(" 1*address ")" / nil
;;
;;   env-cc          = "(" 1*address ")" / nil
;;
;;   env-date        = nstring
;;
;;   env-from        = "(" 1*address ")" / nil
;;
;;   env-in-reply-to = nstring
;;
;;   env-message-id  = nstring
;;
;;   env-reply-to    = "(" 1*address ")" / nil
;;
;;   env-sender      = "(" 1*address ")" / nil
;;
;;   env-subject     = nstring
;;
;;   env-to          = "(" 1*address ")" / nil

(defun imap-parse-envelope ()
  (when (eq (char-after) ?\()
    (imap-forward)
    (vector (prog1 (imap-parse-nstring);; date
	      (imap-forward))
	    (prog1 (imap-parse-nstring);; subject
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; from
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; sender
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; reply-to
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; to
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; cc
	      (imap-forward))
	    (prog1 (imap-parse-address-list);; bcc
	      (imap-forward))
	    (prog1 (imap-parse-nstring);; in-reply-to
	      (imap-forward))
	    (prog1 (imap-parse-nstring);; message-id
	      (imap-forward)))))

;;   body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil

(defsubst imap-parse-string-list ()
  (cond ((eq (char-after) ?\();; body-fld-param
	 (let (strlist str)
	   (imap-forward)
	   (while (setq str (imap-parse-string))
	     (push str strlist)
	     (imap-forward))
	   (nreverse strlist)))
	((imap-parse-nil)
	 nil)))

;;   body-extension  = nstring / number /
;;                      "(" body-extension *(SP body-extension) ")"
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept body-extension fields.  Server
;;                       ; implementations MUST NOT generate
;;                       ; body-extension fields except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.

(defun imap-parse-body-extension ()
  (if (eq (char-after) ?\()
      (let (b-e)
	(imap-forward)
	(push (imap-parse-body-extension) b-e)
	(while (eq (char-after) ?\ )
	  (imap-forward)
	  (push (imap-parse-body-extension) b-e))
	(assert (eq (char-after) ?\)))
	(imap-forward)
	(nreverse b-e))
    (or (imap-parse-number)
	(imap-parse-nstring))))

;;   body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch

(defsubst imap-parse-body-ext ()
  (let (ext)
    (when (eq (char-after) ?\ );; body-fld-dsp
      (imap-forward)
      (let (dsp)
	(if (eq (char-after) ?\()
	    (progn
	      (imap-forward)
	      (push (imap-parse-string) dsp)
	      (imap-forward)
	      (push (imap-parse-string-list) dsp)
	      (imap-forward))
	  (assert (imap-parse-nil)))
	(push (nreverse dsp) ext))
      (when (eq (char-after) ?\ );; body-fld-lang
	(imap-forward)
	(if (eq (char-after) ?\()
	    (push (imap-parse-string-list) ext)
	  (push (imap-parse-nstring) ext))
	(while (eq (char-after) ?\ );; body-extension
	  (imap-forward)
	  (setq ext (append (imap-parse-body-extension) ext)))))
    ext))

;;   body            = "(" body-type-1part / body-type-mpart ")"
;;
;;   body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
;;                     body-fld-enc SP body-fld-octets
;;
;;   body-fld-desc   = nstring
;;
;;   body-fld-dsp    = "(" string SP body-fld-param ")" / nil
;;
;;   body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
;;                     "QUOTED-PRINTABLE") DQUOTE) / string
;;
;;   body-fld-id     = nstring
;;
;;   body-fld-lang   = nstring / "(" string *(SP string) ")"
;;
;;   body-fld-lines  = number
;;
;;   body-fld-md5    = nstring
;;
;;   body-fld-octets = number
;;
;;   body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
;;
;;   body-type-1part = (body-type-basic / body-type-msg / body-type-text)
;;                     [SP body-ext-1part]
;;
;;   body-type-basic = media-basic SP body-fields
;;                       ; MESSAGE subtype MUST NOT be "RFC822"
;;
;;   body-type-msg   = media-message SP body-fields SP envelope
;;                     SP body SP body-fld-lines
;;
;;   body-type-text  = media-text SP body-fields SP body-fld-lines
;;
;;   body-type-mpart = 1*body SP media-subtype
;;                     [SP body-ext-mpart]
;;
;;   media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
;;                     "MESSAGE" / "VIDEO") DQUOTE) / string) SP media-subtype
;;                       ; Defined in [MIME-IMT]
;;
;;   media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
;;                      ; Defined in [MIME-IMT]
;;
;;   media-subtype   = string
;;                       ; Defined in [MIME-IMT]
;;
;;   media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
;;                       ; Defined in [MIME-IMT]

(defun imap-parse-body ()
  (let (body)
    (when (eq (char-after) ?\()
      (imap-forward)
      (if (eq (char-after) ?\()
	  (let (subbody)
	    (while (and (eq (char-after) ?\()
			(setq subbody (imap-parse-body)))
	      (push subbody body))
	    (imap-forward)
	    (push (imap-parse-string) body);; media-subtype
	    (when (eq (char-after) ?\ );; body-ext-mpart:
	      (imap-forward)
	      (if (eq (char-after) ?\();; body-fld-param
		  (push (imap-parse-string-list) body)
		(push (and (imap-parse-nil) nil) body))
	      (setq body
		    (append (imap-parse-body-ext) body)));; body-ext-...
	    (assert (eq (char-after) ?\)))
	    (imap-forward)
	    (nreverse body))

	(push (imap-parse-string) body);; media-type
	(imap-forward)
	(push (imap-parse-string) body);; media-subtype
	(imap-forward)
	;; next line for Sun SIMS bug
	(and (eq (char-after) ? ) (imap-forward))
	(if (eq (char-after) ?\();; body-fld-param
	    (push (imap-parse-string-list) body)
	  (push (and (imap-parse-nil) nil) body))
	(imap-forward)
	(push (imap-parse-nstring) body);; body-fld-id
	(imap-forward)
	(push (imap-parse-nstring) body);; body-fld-desc
	(imap-forward)
	(push (imap-parse-string) body);; body-fld-enc
	(imap-forward)
	(push (imap-parse-number) body);; body-fld-octets

	;; ok, we're done parsing the required parts, what comes now is one
	;; of three things:
	;;
	;; envelope       (then we're parsing body-type-msg)
	;; body-fld-lines (then we're parsing body-type-text)
	;; body-ext-1part (then we're parsing body-type-basic)
	;;
	;; the problem is that the two first are in turn optionally followed
	;; by the third.  So we parse the first two here (if there are any)...

	(when (eq (char-after) ?\ )
	  (imap-forward)
	  (let (lines)
	    (cond ((eq (char-after) ?\();; body-type-msg:
		   (push (imap-parse-envelope) body);; envelope
		   (imap-forward)
		   (push (imap-parse-body) body);; body
		   (imap-forward)
		   (push (imap-parse-number) body));; body-fld-lines
		  ((setq lines (imap-parse-number));; body-type-text:
		   (push lines body));; body-fld-lines
		  (t
		   (backward-char)))));; no match...

	;; ...and then parse the third one here...

	(when (eq (char-after) ?\ );; body-ext-1part:
	  (imap-forward)
	  (push (imap-parse-nstring) body);; body-fld-md5
	  (setq body (append (imap-parse-body-ext) body)));; body-ext-1part..
    
	(assert (eq (char-after) ?\)))
	(imap-forward)
	(nreverse body)))))

(when imap-debug			; (untrace-all)
  (require 'trace)
  (buffer-disable-undo (get-buffer-create imap-debug))
  (mapcar (lambda (f) (trace-function-background f imap-debug)) 
	  '(
	    imap-read-passwd
	    imap-utf7-encode
	    imap-utf7-decode
	    imap-error-text
	    imap-kerberos4s-p
	    imap-kerberos4-open
	    imap-ssl-p
	    imap-ssl-open
	    imap-network-p
	    imap-network-open
	    imap-interactive-login
	    imap-kerberos4a-p
	    imap-kerberos4-auth
	    imap-cram-md5-p
	    imap-cram-md5-auth
	    imap-login-p
	    imap-login-auth
	    imap-anonymous-p
	    imap-anonymous-auth
	    imap-open-1
	    imap-open
	    imap-opened
	    imap-authenticate
	    imap-close
	    imap-capability
	    imap-namespace
	    imap-send-command-wait
	    imap-mailbox-put
	    imap-mailbox-get
	    imap-mailbox-map-1
	    imap-mailbox-map
	    imap-current-mailbox
	    imap-current-mailbox-p-1
	    imap-current-mailbox-p
	    imap-mailbox-select-1
	    imap-mailbox-select
	    imap-mailbox-examine
	    imap-mailbox-unselect
	    imap-mailbox-expunge
	    imap-mailbox-close
	    imap-mailbox-create-1
	    imap-mailbox-create
	    imap-mailbox-delete
	    imap-mailbox-rename
	    imap-mailbox-lsub
	    imap-mailbox-list
	    imap-mailbox-subscribe
	    imap-mailbox-unsubscribe
	    imap-mailbox-status
	    imap-mailbox-acl-get
	    imap-mailbox-acl-set
	    imap-mailbox-acl-delete
	    imap-current-message
	    imap-list-to-message-set
	    imap-fetch-asynch
	    imap-fetch
	    imap-message-put
	    imap-message-get
	    imap-message-map
	    imap-search
	    imap-message-flag-permanent-p
	    imap-message-flags-set
	    imap-message-flags-del
	    imap-message-flags-add
	    imap-message-copyuid-1
	    imap-message-copyuid
	    imap-message-copy
	    imap-message-appenduid-1
	    imap-message-appenduid
	    imap-message-append
	    imap-body-lines
	    imap-envelope-from
	    imap-send-command-1
	    imap-send-command
	    imap-wait-for-tag
	    imap-sentinel
	    imap-find-next-line
	    imap-arrival-filter
	    imap-parse-greeting
	    imap-parse-response
	    imap-parse-resp-text
	    imap-parse-resp-text-code
	    imap-parse-data-list
	    imap-parse-fetch
	    imap-parse-status
	    imap-parse-acl
	    imap-parse-flag-list
	    imap-parse-envelope
	    imap-parse-body-extension
	    imap-parse-body
	    )))
	
(provide 'imap)

;;; imap.el ends here
