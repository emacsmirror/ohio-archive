;;; zephyr.el  an interface to the zephyr message system
;;; Copyright (C) 1992, 1993 Scott Draves (spot@cs.cmu.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This is version 2.0 (now polished a bit).  The primary advantage
;;; is that it no longer
;;; forks off a zwrite to do sends - instead, it uses a new version of
;;; tzc which is capable of sending as well as receiving.  This code
;;; was contributed by Nick Thompson (nix@cs.cmu.edu).
;;; You *will* need the new version of tzc (should be included).

;;; If you write C programs which talk to emacs, you should check out
;;; lread.h and lread.c in the tzc program.  These provide a
;;; convenient way to communicate structured data.

;;; send and receive zephyrgrams with a convenient and flexible
;;; interface.  some people say the interface feels like IRC.
;;; here's a quick feature list:
;;;    asynchronously send and receive messages from a single buffer
;;;    handles instances, multiple destinations
;;;    aliases (can translate one name to many)
;;;    "lazy" beep for notification
;;;    sender/receiver history
;;;    filter messages based on sender and/or instance
;;;    multiple buffers, each with different filters
;;;    highly customizeable
;;;
;;; there's some support for encrypted instances, but it no longer
;;; works.  to use, copy the following lines into your .emacs:
;;;
;;;  (load-library "zephyr")
;;;  (zephyr-new-buffer)
;;;
;;; this mode requires the tzc program which should have come with the
;;; distribution.  the distrubution is available by ftp from
;;; hopeless.mess.cs.cmu.edu:/usr/spot/pub/zephyr.tar.Z, and should
;;; show up in the elisp archive at some point.
;;;
;;; thanks to todd kaufmann, mark eichin, nick thompson, and hank wan
;;; for their contributions.
;;;
;;; i am always interested in bug reports and improvements to this
;;; code.  feel free to send mail.


;;; TODO
;;; 1) subscripts:  when an unrecognized instance arrives, ask if you
;;; want to read it.  remember subs in a file (.zephyr.el).  should
;;; tie into encryption.  or just have simple functions for adding and
;;; removing instances to the regexps, esp one that sez "ignore this
;;; instance for the next 30 (by default, ^U changes) minutes"
;;; 2) make send to end-of-line (?)
;;; 3) consider a fn and binding for jump to last window to receive a
;;; zgram.
;;; 4) make the hook list and beep time be locals. (?)
;;; 5) make the exposure be a sexp we send to tzc rather than a
;;; command line option.  eliminate other options same way.
;;; 6) if you send to multiple destinations, the "... done" messages
;;; cover each other up, so you can't really see all of them.

;;; CHANGES
;;; Thu Jan 21 - removed zwgc code to emulate tzc because it can't
;;; send messages (handle the input).  added a save-excursion to
;;; zephyr-do-insert so that point isn't moved in buffers that aren't
;;; visible when they receive a message.  rewrote
;;; zephyr-notify-with-scroll so it doesn't scroll the current window.
;;; Thu Jan  7 - replaced ` with cons due to incompatible versions of
;;; ` floating around.
;;; Fri Jan  1 - ugh, the recipient of "(foo)" is now "".
;;; zephyr-make-text-recipient now handles "foo(bar)".  and handle the
;;; additional information in "to" tag of tzcspew sent messages.
;;; added * to docstring of a bunch of variables.  removed
;;; zephyr-require-tzcspew. when receiving a message, formats as 
;;; "foo(bar)", rather than "foo (bar)"
;;; Sun Dec 13 - parse names a little differently:  "foo" now goes to
;;; the user foo.  "(foo)" goes to everyone reading instance foo, and
;;; "foo(bar)" goes to just user foo as instance bar.  aliases are now
;;; recursive.
;;; Sun Dec 13 - merged in changes to handle new tzc, run tzc with
;;; pipe rather than pty.  call this 2.0 because the new tzc is
;;; slightly incompatible.
;;; Mon Dec  7 - handles class MESSAGE messages that have the wrong
;;; number of fields, as can happen if the sender embeds a NULL.
;;; Mon Nov 16 - removed old code to parse output of vanilla zwgc
;;; Sat Oct 24 - if any function in the hook list returns nil, then do
;;; not call the rest of the hooks.
;;; 22/8/92 - added sentinel so when zephyr receiver dies, we are
;;; notified.  remove sentinel before restarting.
;;; 29/7/92 - added zwgc description file that emulates tzc
;;; 26/7/92 - fixed zephyr-add-timestamp, it wasn't returning the msg
;;; 11/7/92 - fixed: M-p and M-n raise error if no messages sent yet.
;;; split zephyr-notify into zephyr-notify-with-beep,
;;; zephyr-notify-with-message, and zephyr-notify-with-scroll. added
;;; zephyr-touch.  calls zephyr-touch from zephyr-insert (rather than
;;; notify).
;;; 9/7/92 - replaced a bunch of random hooks with zephyr-hook-list,
;;; including reorganizing the code, various small improvements.
;;; commented out dangling encryption stuff.  tzc now stamps with date
;;; string rather than integer so that it is human readable in the
;;; log, and also cuz emacs ints aren't big enuf. added todd's
;;; delete-messages function.  removed that bury crap.


;;; the next 4 variables control what messages you see

(defvar zephyr-senders-ignore-regexp nil
  "*if this regexp matches the sender, then ignore the message.  it's
typical to put yourself in this list so that you don't get what you
send to instances you also read.  to ignore nobody, use nil.")

(defvar zephyr-senders-accept-regexp nil
  "*only accept messages from users who match this regexp.  nil means
match everybody.")

(defvar zephyr-instances-ignore-regexp nil
  "*if this regexp mathces the instance, then ignore the message. use
nil to ignore nobody.")

(defvar zephyr-instances-accept-regexp nil
  "*only accept messages from instances that match this regexp.  nil
means match all instances")


(defvar zephyr-hook-list
  '(zephyr-parse
    zephyr-dispatch
    zephyr-parse-fields
    zephyr-add-banner
    zephyr-insert
    zephyr-notify-with-message
    zephyr-notify-with-beep
    zephyr-notify-with-scroll)
  "*a list of functions to call to process an incoming message.  a lot
of customization can be done by changing this list.  for example, to
visibly timestamp messages, add the function zephyr-add-timestamp next
to zephyr-add-banner.")


(defvar zephyr-send-divider "<<< "
  "pattern to mark the beginning of the message to be sent.
everything between it and point is considered part of the message.
everything between it and the beginning of the line is considered a
list of recipients.")

(defvar zephyr-send-divider-regexp "<<<\\( \\|$\\)"
  "regexp to match zephyr-send-divider.  it pays to be a bit lenient.")

(defvar zephyr-receive-program '("/afs/cs.cmu.edu/user/spot/bin/tzc"
				 "-e" "NET-ANNOUNCED")
  "*list containing program and its options. invoked to receive
zephyr-grams.")

(defvar zephyr-kerberos-authenticate t
  "*if true, then the client is requested to use kerberos
authentication when sending messages.  othewise use no authentication.")

(defvar zephyr-aliases '(("stooges" . ("larry" "curly" "moe")))
  "*alist of recursive aliases.")

(defvar zephyr-instance-xlate-hook 'zephyr-instance-xlate
  "call this function with each recipient.  returns a list of options
for the send program.  typically, it recognizes instances.")

(defvar zephyr-receive-divider ">>> "
  "string that appears immediately before the message body of incoming
zephyr-grams.")

(defvar zephyr-signature (concat (getenv "USER") "\n")
  "*signature to use on outgoing zephyrgrams")

(defvar zephyr-unauth-string "(NOAUTH)"
  "string that appears next to messages that are from unauthenticated
sources.")

(defvar zephyr-failed-string "(FORGED)"
  "string that appears next to messages whose authentication attempt
failed.")

(defvar zephyr-lazy-beep-time 120
  "*beep, unless a msg has been received since this many seconds ago.
if this is nil, then never beep.")

(defvar zephyr-log-buffer-limit 32000
  "*prevent the zephyr-log-buffer from expanding beyond this many
characters.  the buffer acts as a queue -- text from the beginning is
thrown away as new messages arrive.  the value nil means that the
buffer will grow without bound.")

(defvar zephyr-buffer-limit 32000
  "*prevent the *zephyr* buffer from expanding beyond this many
characters.  the buffer acts as a queue -- text from the beginning is
thrown away as new messages arrive.  the value nil means that the
buffer will grow without bound.")

(defvar zephyr-client-eom-string "\0"
  "string matching the end of the output from the zephyr client
receiving messages.  for zwgc this is usually ^G.")

(defvar zephyr-client-bom-string "\1"
  "string matching the beginning of the output from the zephyr client
receiving messages.  must be different from
zephyr-client-eom-string.")

; (defvar zephyr-encrypt-program "crypt"
;   "invoke this program to encrypt messages.")

; (defvar zephyr-decrypt-program "crypt"
;  "invoke this program to decrypt messages.")

; (defvar zephyr-cypher-is-binary t
;   "true if the cypher-text generated by zephyr-encrypt-program is
; binary.")

; (defvar zephyr-passwords '(("an-encrypted-instance" .
; 			    "some-password"))
;   "alist of instance-password pairs.  if the password is nil, the user
; is prompted when it is needed.  the passwords can be cleared with
; zephyr-clear-passwords. encrpytion doesn't work.")


; how to remove buffers when they are killed? rebind C-xk?!
(defvar zephyr-buffer-list nil
  "list of buffers that are receiving zephyrgrams")

(defun zephyr-new-buffer (&optional name)
  "create a new buffer for reading and sending zephyrgrams.  the
buffer is named *zephyr-NAME*, where NAME is printed representation of
the argument, or just *zephyr* if NAME is nil, or not provided."
  (interactive)
  (let ((name (if name
		  (concat "*zephyr-" name "*")
		"*zephyr*")))
    (switch-to-buffer name)
    (zephyr-mode)
    (zephyr-compose "")
    (setq zephyr-buffer-list (cons (current-buffer)
				   zephyr-buffer-list))))

(defun zephyr-match (s n)
  (substring s
	     (match-beginning n)
	     (match-end n)))
  
;;; convert a name to an instance/user pair
(defun zephyr-instance-xlate (name)
  (let ((l (assoc name zephyr-aliases))
	(n (string-match "(\\(.*\\))" name)))
    (cond (l (apply 'append (mapcar 'zephyr-instance-xlate (cdr l))))
	  (n (list (cons (zephyr-match name 1)
			 (if (= n 0) "" (substring name 0 n)))))
	  (t (list (cons "PERSONAL" name))))))

;;; convert string to list of strings, basically seperate it into
;;; words.  space, tab, and comma are considered seperators.
(defun zephyr-send-make-recipient-list (recipient)
  (if (string-match "[ \t,]*\\([^ \t,]+\\)" recipient)
      (let* ((beg (match-beginning 1))
	     (end (match-end 1))
	     (name (substring recipient beg end)))
	(append (funcall zephyr-instance-xlate-hook name)
		(zephyr-send-make-recipient-list
		 (substring recipient end))))
    nil))

(defun zephyr-get-instance (l)
  (if l
      (if (equal (car l) "-i")
	  (car (cdr l))
	(zephyr-get-instance (cdr l)))
    nil))

(defvar zephyr-last-recipient "nobody")

(defun zephyr-make-text-recipient (recip)
  (cond ((string= (car recip) "PERSONAL")
	 (cdr recip))
	(t (concat (cdr recip) "(" (car recip) ")"))))

(defun zephyr-send-and-compose ()
  "send the zephyrgram before point, and start composing another one."
  (interactive)
  (unwind-protect
      (let* ((cur-buf (current-buffer))
	     (log-buffer-size (progn (set-buffer zephyr-log-buffer)
				     (buffer-size))))
	(set-buffer cur-buf)
	(save-excursion
	  (let* ((end-msg (point))
		 (pat (concat "^.+\\("
			       zephyr-send-divider-regexp
			       "\\)"))
		 (end-recipient (progn (re-search-backward pat)
				       (match-beginning 1)))
		 (start-recipient (match-beginning 0))
		 (start-msg (match-end 0))
		 (msg (buffer-substring start-msg end-msg))
		 (recipient (buffer-substring start-recipient end-recipient))
		 (recip-list (zephyr-send-make-recipient-list recipient))
		 (text-recip-list (mapconcat 'zephyr-make-text-recipient
					     recip-list " ")))
	    (message (concat "Sending to " text-recip-list "..."))
	    (setq zephyr-last-recipient recipient)
	    (zephyr-touch-name recipient)
	    ;; (let* ((inst (zephyr-get-instance recip-list))
	    ;;   (a (assoc inst zephyr-passwords)))
	    ;; (if a (progn
	    ;;      (setq msg (zephyr-en/decrypt msg (cdr a) t))
	    ;;    (if zephyr-cypher-is-binary
	    ;;  (setq msg (zephyr-btoa msg))))))
	    (zephyr-send msg recip-list)))
	(zephyr-limit-buffer-size zephyr-buffer-limit)
	(set-buffer zephyr-log-buffer)
	(if (not (equal log-buffer-size (buffer-size)))
	    (progn
	      (pop-to-buffer zephyr-log-buffer)
	      (error "zwrite complained")))
	(set-buffer cur-buf))
    ;; this is protected, so in case of error sending, we still set up
    ;; for the next one
    (zephyr-compose "")))

(defun zephyr-send (msg to-list)
  ;; WARNING! WARNING! WARNING!
  ;; if you send a message without at least two parts in the "message"
  ;; field, any zwgc receiving the message will die a silent death,
  ;; and people will probably whine at you instead of at the author
  ;; of zwgc.
  (let ((str (format "%s\n"
		     (list '(tzcfodder . send)
			   (cons 'auth zephyr-kerberos-authenticate)
			   '(class . MESSAGE)
			   (cons 'recipients to-list)
			   (cons 'message (list zephyr-signature msg))))))
    ;; (print str (get-buffer "*scratch*"))
    (process-send-string zephyr-process str)))

;;; go to the end of the buffer, add a new header
(defun zephyr-send-setup (recipient)
  (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  (insert (concat recipient zephyr-send-divider)))

(defun zephyr-compose (recipient)
  "compose a zephyr-gram to be sent.  prompts for recipient, if none
is given, use last recipient."
  (interactive "sTo: ")
  (if (not (equal mode-name "Zephyr"))
      (if zephyr-buffer-list
	  (pop-to-buffer (car zephyr-buffer-list))
	(error "no zephyr buffers")))
  (if (equal recipient "")
      (zephyr-send-setup zephyr-last-recipient)
    (zephyr-send-setup recipient)))


;;; valid values for tzcspew:
;;;   message - an incoming message
;;;   error - an error from tzc
;;;   sent - message successfully sent

;;; add most of the tags to the message alist.  since tzc prints out
;;; messages in lisp-readable from, this is mostly just a call to
;;; read.  if you were using zwgc instead, you would have to do
;;; something horrible, something like what's in the preceding
;;; comments.
;;;
;;; the tags in the alist contain at least:
;;;
;;;   message - a list of strings, one per field
;;;   instance - a string, often "PERSONAL"
;;;   auth - a symbol, either 'yes, 'no, or 'failed
;;;   kind - a symbol, usually 'acked
;;;   port - an integer, the sender's port
;;;   class - a symbol, usually 'MESSAGE
;;;   opcode - a symbol, usually either nil or 'PING
;;;   sender - a string, the login name of the sender
;;;   recipient - a string, either your login name or NIL
;;;   fromhost - a string, the hostname where the msg orginated
;;;   time - a string, just like from current-time-string
;;;
;;; these tags will sometimes be added, depending on what happens to the
;;; msg and what its contents are:
;;;
;;;   buffers - a list of buffers, where the message appeared
;;;   print-as - a string, how we should display it
;;;   signature - a string, from the sender (class MESSAGE)
;;;   body - a string, the message (class MESSAGE)
;;;   host - a string, (class LOGIN)
;;;   when - a string, (class LOGIN)
;;;   tty - a string, (class LOGIN)

(defun zephyr-parse (msg)
  (let* ((r (cdr (assq 'raw-source msg)))
	 (buf (car r))
	 (start (cadr r))
	 (end (caddr r)))
    (set-buffer buf)
    (goto-char start)
    (read buf)))

(setq zephyr-tzcspew-dispatch-table
      '((message . zephyr-pass)
	(error . zephyr-error)
	(sent . zephyr-sent)
	(not-sent . zephyr-not-sent)))

;;; dispatch the message based on the tzcspew tag
(defun zephyr-dispatch (msg)
  (let* ((spewtag (cdr (assq 'tzcspew msg))))
    (if (null spewtag)
	(error "zephyr: missing tzcspew tag - obsolete version of tzc?"))
    (let* ((spewfun (cdr (assq spewtag zephyr-tzcspew-dispatch-table))))
      (funcall spewfun msg))))

(defun zephyr-pass (msg)
  msg)

(defun zephyr-error (msg)
  (let ((err (cdr (assq 'message msg))))
    (if (null err)
	(message "zephyr: tzc error")
      (message (format "zephyr: tzc error %s" err))))
  (beep)
  nil)

(defun zephyr-sent (msg)
  (let ((to (cdr (assq 'to msg))))
    (message (concat "Sending to "
		     (zephyr-make-text-recipient to)
		     "... done")))
  nil)

(defun zephyr-not-sent (msg)
  (let ((to (cdr (assq 'to msg))))
    (message (concat "zephyr: send to "
		     (zephyr-make-text-recipient to)
		     " failed")))
  (beep)
  nil)

(defvar zephyr-insert-p 'zephyr-default-insert-p
  "*predicate that returns true if its argument, a msg-alist, should
appear in the current buffer")

;;; for each zephyr buffer, check if this msg should appear in it.  if
;;; so, insert it.
(defun zephyr-insert (msg)
  (let ((buffers nil)
	(loop zephyr-buffer-list))
    (while loop
      (if (buffer-name (car loop))
	  (progn
	    (set-buffer (car loop))
	    (if (funcall zephyr-insert-p msg)
		(progn
		  (zephyr-do-insert msg)
		  (zephyr-touch msg)
		  (setq buffers (cons (car loop) buffers))))
	    (setq loop (cdr loop)))))
    (cons (cons 'buffers buffers) msg)))

(defun zephyr-default-insert-p (msg)
  (let ((instance (cdr (assq 'instance msg)))
	(sender  (cdr (assq 'sender msg)))
	(opcode  (cdr (assq 'opcode msg)))
	(msg-text (cdr (assq 'print-as msg))))

    (not (or (eq 'PING opcode)

	     (and zephyr-instances-ignore-regexp
		  (string-match zephyr-instances-ignore-regexp
				instance))
	 
	     (and zephyr-senders-ignore-regexp
		  (string-match zephyr-senders-ignore-regexp
				     sender))
	 
	     (and zephyr-instances-accept-regexp
		  (not (string-match zephyr-instances-accept-regexp
				     instance)))
	     
	     (and zephyr-senders-accept-regexp
		  (not (string-match zephyr-senders-accept-regexp
				     sender)))))))

;;; really stick it in the current buffer.  guarantee newline termination
(defun zephyr-do-insert (msg)
  (let ((msg-banner (cdr (assq 'banner msg)))
	(msg-text   (cdr (assq 'print-as msg))))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward zephyr-send-divider-regexp (point-min) t)
      (re-search-backward "^")
      (insert msg-banner
	      msg-text)
      (if (or (= 0 (length msg-text))
	      (not (equal "\n"
			  (substring msg-text
				     (- (length msg-text) 1)))))
	  (insert "\n")))))


;;; add the string used to "introduce" a message.
(defun zephyr-add-banner (msg)
  (let ((instance (cdr (assq 'instance msg)))
	(sender  (cdr (assq 'sender msg)))
	(auth (cdr (assq 'auth msg))))
    (cons (cons 'banner
		(concat (zephyr-make-text-recipient (cons instance sender))
			(cond ((eq auth 'yes) "")
			      ((eq auth 'failed) zephyr-failed-string)
			      ((eq auth 'no) zephyr-unauth-string))
			zephyr-receive-divider))
	  msg)))

;;; an example function you can add to the pipeline that
;;; timestamps messages.
(defun zephyr-add-timestamp (msg)
  (let ((banner (assq 'banner msg)))
    (if banner
	(setcdr banner (concat (cdr banner)
			       "("
			       (substring (current-time-string) 11 16)
			       ")"))))
  msg)

;;; this one comes in handy too...
(defun zephyr-dump (msg)
  (print msg (get-buffer "*scratch*"))
  msg)

; give names to the various fields and add them to the alist.  also
; add the print-as tag, containing the printed rep.
(defun zephyr-parse-fields (msg)
  (let ((class (cdr (assq 'class msg)))
	(sender (cdr (assq 'sender msg)))
	(fields (cdr (assq 'message msg))))
    (cond (; in messages, the first field is a signature, and the
	   ; second is the message body.
	   (eq class 'MESSAGE)
	   
	   (let* ((len (length fields))
		  (sig (cond ((= 2 len)
			      (string-match "\\(.*\\)\n*"
					    (car fields))
			      (zephyr-match (car fields) 1))
			     ((= 1 len)
			      sender)
			     (t sender)))
		  (body (cond ((= 2 len) (cadr fields))
			      ((= 1 len) (car fields))
			      (t (format "malformed message: %s" fields)))))
	     (append (list (cons 'print-as body)
			   (cons 'signature sig)
			   (cons 'body body))
		     msg)))
	  (; in logins, the fields are host, when, and tty.
	   (eq class 'LOGIN)
	   (let ((host (nth 0 fields))
		 (when (nth 1 fields))
		 (tty (nth 2 fields)))
	     (append (list (cons 'print-as
				 (concat "on " host " at " when))
			   (cons 'host host)
			   (cons 'when when)
			   (cons 'tty tty))
		     msg)))
	  (t (cons (cons 'print-as
			 (mapconcat '(lambda (x) x)
				    fields "\n"))
		   msg)))))


(defun zephyr-lazy-beep (now delay)
  (let ((then zephyr-lazy-beep-last))
    (setq zephyr-lazy-beep-last now) ; horrid global var
    (if (and delay (or (not then)
		       (> (time-difference-in-seconds then now)
			  delay)))
	(beep))))



;
;; convert binary to ascii, slow stupid, simple.
;(defun zephyr-btoa (s)
;  (mapconcat '(lambda (c) (int-to-string c)) s " "))
;
;; convert ascii to binary, slow stupid, simple.
;; returns nil if there is a formatting error
;(defun zephyr-atob (s)
;  (save-excursion
;    (let ((src (generate-new-buffer " atob-src"))
;	  ans)
;      (set-buffer src)
;      (insert "(" s ")")
;      (goto-char (point-min))
;      (setq ans
;	    (condition-case ERR
;		(mapconcat 'char-to-string (read src) "")
;	      (error nil)))
;      (kill-buffer src)
;      ans)))
;
;; en/decrype S using KEY.  EN is true means encrypt, otherwise
;; decrypt.
;(defun zephyr-en/decrypt (s key en)
;  (save-excursion
;    (let ((in (generate-new-buffer " endecrypt-in"))
;	  (out (generate-new-buffer " endecrypt-out"))
;	  (pgm (if en zephyr-encrypt-program
;		 zephyr-decrypt-program)))
;      (set-buffer in)
;      (insert s)
;      (call-process-region (point-min) (point-max)
;			   pgm t out nil key)
;      (kill-buffer in)
;      (set-buffer out)
;      (let ((b (buffer-string)))
;	(kill-buffer out)
;	b))))
;


(defun zephyr-notify-with-message (msg)
  (let ((buffers (cdr (assq 'buffers msg))))
    (if buffers
	(message (concat "received "
			 (or (downcase (cdr (assq 'instance msg)))
			     "instanceless")
			 " zephyrgram from "
			 (or (cdr (assq 'signature msg))
			     (cdr (assq 'sender msg))
			     "???")))))
  msg)

(defun zephyr-notify-with-beep (msg)
  (let ((buffers (cdr (assq 'buffers msg)))
	(instance (cdr (assq 'instance msg)))
	(time (cdr (assq 'time msg))))
    (if buffers
	(if (not (equal (downcase instance) "urgent"))
	    (zephyr-lazy-beep time zephyr-lazy-beep-time)
	  (beep) (beep) (beep))))
  msg)

;;; if the window where the messages appears is active, move so the
;;; end (where the message is) is visible
(defun zephyr-buffer-show-end (buf)
  (let* ((win (get-buffer-window buf)))
    (if win
	(progn
	  (set-buffer buf)
	  (if (pos-visible-in-window-p (point-max) win)
	      nil
	    (goto-char (point-max))
	    (vertical-motion (- 2 (window-height win)))
	    (set-window-start win (point)))))))


(defun zephyr-notify-with-scroll (msg)
  (let ((buffers (cdr (assq 'buffers msg)))
	(curser-win (selected-window)))
    (while buffers
      (let* ((buf (car buffers))
	     (win (get-buffer-window buf)))
	(if (and win
		 (not (eq win curser-win)))
	    (progn
	      (set-buffer buf)
	      (goto-char (point-max))
	      (vertical-motion (- 2 (window-height win)))
	      (set-window-start win (point)))))
      (setq buffers (cdr buffers))))
  msg)


(defun zephyr-limit-buffer-size (lim)
  (let ((max (point-max)))
    (if (and lim (> max lim))
	(delete-region (point-min) (- max lim)))))

(defun zephyr-receive-sentinel (proc sig)
  (let ((status (process-status proc)))
    (if (or (eq 'exit status)
	    (eq 'signal status))
	(progn
	  (message "zephyr-receive died")
	  (pop-to-buffer zephyr-log-buffer)))))

(defun zephyr-do-message-hooks (msg)
  (let ((loop zephyr-hook-list))
    (while (and loop msg) ; bail if the message becomes nil
      (setq msg (funcall (car loop) msg))
      (setq loop (cdr loop)))))

(defun zephyr-receive-filter (process string)
  (save-excursion
    (set-buffer zephyr-log-buffer)
    (let ((start (point-max))
	  (done nil))
      (goto-char start)
      (insert string)
      (goto-char start)
      (while (search-forward zephyr-client-eom-string
			     (point-max) t)
	(let ((end (point)))
	  (forward-char -1)
	  (if (search-backward zephyr-client-bom-string
			       (point-min) t)
	      (forward-char 1)
	    (goto-char (point-min)))
	  (zephyr-do-message-hooks
	   (list (list 'raw-source
		       zephyr-log-buffer (point) end)))
	  (set-buffer zephyr-log-buffer)
	  (goto-char end)))
      (zephyr-limit-buffer-size zephyr-log-buffer-limit))))


(defun zephyr-restart-receiver ()
  "kill and start another receiver process.  this is a good thing to do if
your kerberos tickets expire, causing all messages authentication to
appear failed."
  (interactive)
  (set-process-sentinel zephyr-process nil)
  (delete-process zephyr-process)
  (zephyr-start-receiver))

(defun zephyr-start-receiver ()
  (setq zephyr-lazy-beep-last nil)
  (setq zephyr-process
	(let ((process-connection-type nil))
	  (apply 'start-process
		 "zephyr-receive" zephyr-log-buffer
		 zephyr-receive-program)))
  (process-kill-without-query zephyr-process)
  (set-process-sentinel zephyr-process 'zephyr-receive-sentinel)
  (set-process-filter zephyr-process 'zephyr-receive-filter))

(defvar zephyr-previous-names nil
  "doubly linked list of names of destinations and sources of
zephyrgrams previously sent and received.  most recent is first.  no
duplicates.")

(defun cadr (l) (car (cdr l)))
(defun cddr (l) (cdr (cdr l)))
(defun cdddr (l) (cdr (cdr (cdr l))))
(defun cdadr (l) (cdr (car (cdr l))))
(defun caddr (l) (car (cdr (cdr l))))

(defun zephyr-touch (msg)
  "touch the name(s) appearing in msg"
  (let ((sender (cdr (assq 'sender msg)))
	(instance (cdr (assq 'instance msg))))
    (zephyr-touch-name sender)
    (if (not (equal "PERSONAL" instance))
	(zephyr-touch-name (concat "(" instance ")")))))

(defun zephyr-touch-name (name)
  "move NAME to head zephyr-previous-names, add if not already there."
  (if zephyr-previous-names
      (if (not (equal name (car zephyr-previous-names)))
	  (progn
	    (let ((h (cddr zephyr-previous-names)))
	      (while (not (eq h zephyr-previous-names))
		(if (equal (car h) name)
		    (progn
		      (setcar (cdddr h) (cadr h))
		      (setcdr (cdadr h) (cddr h))
		      (setq h zephyr-previous-names))
		  (setq h (cddr h)))))
	    (let ((n (cons name (cons zephyr-previous-names
				      (cddr zephyr-previous-names)))))
	      (setcar (cdddr n) n)
	      (setcdr (cdadr n) n)
	      (setq zephyr-previous-names n))))
    (let ((n (cons name (cons nil nil))))
      (setcar (cdr n) n)
      (setcdr (cdr n) n)
      (setq zephyr-previous-names n))))

(defun zephyr-replace-destination (name)
  "replace the current destination with NAME"
  (save-excursion
   (re-search-backward zephyr-send-divider-regexp)
   (let ((end-dest (point)))
     (re-search-backward "^")
     (delete-region (point) end-dest)
     (insert name))))

(defun zephyr-next-destination (arg)
  "cycle forward through previous senders/destinations"
  (interactive "*p")
  (if zephyr-previous-names
      (if (= arg 0)
	  (zephyr-replace-destination (car zephyr-previous-names))
	(progn
	  (setq zephyr-previous-names (cddr zephyr-previous-names))
	  (zephyr-next-destination (- arg 1))))))

(defun zephyr-previous-destination (arg)
  "cycle backward through previous senders/destinations"
  (interactive "*p")
  (if zephyr-previous-names
      (if (= arg 0)
	  (zephyr-replace-destination (car zephyr-previous-names))
	(progn
	  (setq zephyr-previous-names (cadr zephyr-previous-names))
	  (zephyr-previous-destination (- arg 1))))))



(defun zephyr-delete-messages-from (inst)
  "delete all messages from a particular person that appear after
point.  takes a regexp."
  (interactive "sInstance name (regexp): ")
  (let* ((receive-divider-regexp (regexp-quote zephyr-receive-divider))
	 (kill (concat "\\("
		       inst  ;; why not:  (regexp-quote inst)   ;????????
		       "\\).*"
		       receive-divider-regexp))
	 (any-divider-regexp (concat "\\("
				     receive-divider-regexp
				     "\\|"
				     zephyr-send-divider-regexp
				     "\\)")))
    (while (and (not (eobp))
		(re-search-forward kill nil t))
      (beginning-of-line 1)
      (let ((p (point))
	    (found (re-search-forward any-divider-regexp nil t 2)))
	(beginning-of-line 1)
	(if found 
	    (delete-region p (point))
	  (end-of-line 1))))))

(defvar zephyr-log-buffer nil)

(defun zephyr-mode ()

  "major mode for sending and receiving zephyr-grams.  use
zephyr-send-and-compose [\\[zephyr-send-and-compose]] to send
messages.  instances are specified by enclosing their names in
parentheses.  multiple destinations are seperated by whitespace or
commas.  to change the destination just edit it, or use zephyr-compose
[\\[zephyr-compose]].  if you want to send an instance to just one
person, use \"user(instance)\".

in the composition buffer, the destinations for the current message
appear to the left of \"<<< \".  when you send the zgram, everything
between point and \"<<< \" will be transmitted.  at any time, you can
edit the current destinations, or go back to previous messages and
edit/send them.

when a message arrives, a beep will sound, unless message has arrived
in the previous 120 (the value of zephyr-lazy-beep-time, really)
seconds.  for more elaborate notification, use zephyr-notify-hook.

the output of the receiver process is kept in *log-zephyr* buffer.  the
zephyr-log-buffer-limit and zephyr-buffer-limit variables control how
much text is saved in the buffers.  additional text is discarded.


\\{zephyr-mode-map}

this mode is highly customizable, there are many hooks and variables
you can use to change how it behaves.  here's some of what you can do:

  visibly time-stamp incoming messages (see zephyr-hook-list)

  filter out particular instances/users (see
     zephyr-instances-ignore-regexp)

  multiple receiving buffers with different hooks and regexps

  define aliases for sending to common groups of people (see
     zephyr-aliases)"

  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'zephyr-senders-ignore-regexp)
  (make-local-variable 'zephyr-instances-ignore-regexp)
  (make-local-variable 'zephyr-senders-accept-regexp)
  (make-local-variable 'zephyr-instances-accept-regexp)
  (make-local-variable 'zephyr-previous-names)

  (set-syntax-table text-mode-syntax-table)
  (use-local-map zephyr-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'zephyr-mode)
  (setq mode-name "Zephyr")
  (setq zephyr-log-buffer (get-buffer-create "*log-zephyr*"))
  (setq paragraph-start (concat "\\(" paragraph-start "\\|"
				"^.*" zephyr-send-divider-regexp "\\|"
				"^.*" zephyr-receive-divider "\\)"))
  (if (or (not zephyr-log-buffer)
	  (not (eq 'run (process-status "zephyr-receive"))))
      (zephyr-start-receiver))
  (run-hooks 'text-mode-hook 'zephyr-mode-hook))

(setq zephyr-mode-map (make-sparse-keymap))
(define-key zephyr-mode-map "\C-j" 'zephyr-send-and-compose)
(define-key zephyr-mode-map "\C-c?" 'describe-mode)
(define-key zephyr-mode-map "\C-c\C-s" 'zephyr-compose)
(define-key zephyr-mode-map "\C-c\C-r" 'zephyr-restart-receiver)
(define-key zephyr-mode-map "\C-c\C-c" 'zephyr-send-and-compose)
(define-key zephyr-mode-map "\ep" 'zephyr-previous-destination)
(define-key zephyr-mode-map "\en" 'zephyr-next-destination)
(define-key zephyr-mode-map "\C-c\C-d" 'zephyr-delete-messages-from)

;; stolen from fancy-xmouse.el by Benjamin C. Pierce (bcp@cs.cmu.edu)
(defun time-difference-in-seconds (time1 time2)
  (let* ((t1 (substring time1 11))
 	 (t2 (substring time2 11))
 	 (date1 (substring time1 0 11))
 	 (date2 (substring time2 0 11))
 	 (h1 (string-to-int (substring t1 0 2)))
 	 (h2 (string-to-int (substring t2 0 2)))
 	 (m1 (string-to-int (substring t1 3 5)))
 	 (m2 (string-to-int (substring t2 3 5)))
 	 (s1 (string-to-int (substring t1 6 8)))
 	 (s2 (string-to-int (substring t2 6 8)))
 	 (sec1 (+ (* 3600 h1) (* 60 m1) s1))
 	 (sec2 (+ (* 3600 h2) (* 60 m2) s2)))
    (+ (- sec2 sec1)
       (if (string-equal date1 date2)
 	   0
	 (* 3600 24) ; correction for passing midnight
	 ))))

