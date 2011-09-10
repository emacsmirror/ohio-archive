;; LCD Archive Entry:
;; mail-secure|Travis J.I. Corcoran|tjic@icd.teradyne.com|
;; funcs to make secure mail and News easier: autosign, autoverify, anon, etc.|
;; 30-Apr-95|0.9|~/packages/mail-secure.el.gz|
;;
;;------------------------------------------------------------
;;                    mail-secure.el
;;
;;                    Travis J.I. Corcoran        27 Apr 95
;;
;;                    bug reports to tjic@icd.teradyne.com
;;------------------------------------------------------------
;;
;;   What this package does:
;;      SIGN
;;        (1) adds a "PGP-sign-this-msg?" flag to modeline of buffers
;;        containing either mail or UseNet posts.
;;        (2) adds a func that either toggles or absolutely turns this
;;        on or off, and bind it to a key (C-c C-s).
;;        (3) hooks the send command to act on this flag.
;;      ANON
;;        (1) adds a "send-this-msg-anonymously?" flag to modeline of
;;        buffers containing either mail or UseNet posts.  <<< 1/2 LIE
;;        (2) adds a func that either toggles or absolutely turns this
;;        on or off, and bind it to a key (C-c C-a).
;;        (3) turns off pgp-signing if anonymity is on.
;;        (4) hooks the send command to act on this flag.
;;        (5) when mail is sent, if anon flag is set searches mail for
;;        info leakage (real user name, etc). and if found queries
;;        user "abort?"
;;     CRYPT
;;        (1) hook decryption to kill ^M's (from MS-DOG).
;;        (2) expand aliases before encrypting (so that keys will be
;;        found under users names)
;;        (3) hook encryption to copy subject line into msg (where it
;;        will be encrypted), then kill it in headers; don't want to
;;        leak information to snoopers.
;;        (4) if can't encrypt for all recipients, highlights
;;        offending recipients and queries user "abort?"
;;     VERIFY
;;        (1) autoverify all PGP signed msgs found in an RMAIL or
;;        Gnus buffer, unless marked w the "verified" label.
;;        Highlight the PGP delimiter blocks w a green face if
;;        verified, an orange face if key not found, or highlight
;;        entire msg in red if forged.
;;        (2) if key not known, see if key is in msg.  If user
;;        consents, add key to keyring and reverify.
;;        (3) if key still not known, offer to finger senders account
;;        to get key.  If user consents, finger account, add key to
;;        keyring, and reverify.
;;        (4) if key still not known, offer to mail keyserver to get
;;        key.  If user consents send request to keyserver.
;;        (5) if "key not found" mail received from keyserver, offer to
;;        send mail to the individual asking for key.  If user consents, send
;;        request.
;;        (6) if a user edits an verified RMAIL msg, turn off the
;;        verified label.
;;     MISC
;;        (1) if incoming mail contains key from keyserver, offer to
;;        add key to keyring.
;;        (2) if incoming mail is from keyserver, but search failed,
;;        offer to send mail to user asking him for his key.
;;        (3) add some funcs which return the string found in a mail field
;;        (4) add some funcs which kill the string found in a mail field
;;
;;   How to install:
;;        1) make sure you have emacs 19.xx
;;        2) add (load-file "/u/tjic/lisp/mail-secure.el") to your .emacs
;;        3) procure the optional elisp package "mailcrypt" (version 2.4
;;           or later) and install it
;;
;;   How to use:
;;
;;        In a mail/UseNet-post buffer type (C-c C-s) to flip PGP-sign
;;        flag, or invoke w arg of { 0, 1 } to force off or on.
;;
;;        In a mail/UseNet-post buffer type (C-c C-a) to flip
;;        anon-status, or invoke w arg of { 0, 1 } to force off or on.
;;
;;   How to customize:
;;
;;        (setq ms-own-name-regexp "tjic")
;;
;;   BUGS:
;;
;;        (1) anon: saves anon passwords into your FCC file w rest of msg.
;;        (2) anon: doesn't support anon UseNet posts.
;;        (2) anon: doesn't support cypherpunk remailers
;;
;;        (4) verification: should cope w nested signatures
;;        (5) verification: should keep signature in buffer after decrypting.
;;        (6) verification: if getting new public key by finger,
;;        should ask what keyring you want to put it on
;;        (7) verification: should search multiple keyrings for key
;;        (8) verification: should finger accounts asynchronously
;;        (9) should support the X-Pgp-Signed mail header convention

; ========== require ==========


(require 'mailcrypt)
(require 'sendmail )

(load "rnewspost.el")
(load "rmailkwd.el")
(load "rmailedit.el")

; ========== define vars ==========

(defvar ms-pgpsign-sign-this-msg nil
  "An internal per-buffer var telling us whether this msg should be PGP signed")
(defvar ms-anon-this-msg nil
  "An internal per-buffer var telling us whether this msg mailed anon")
(defvar ms-crypt-this-msg nil
  "An internal per-buffer var telling us whether this msg mailed anon")

(make-local-variable 'ms-pgpsign-sign-this-msg)
(make-local-variable 'ms-anon-this-msg)
(make-local-variable 'ms-crypt-this-msg)

(defvar ms-anon-remailer-addr "anon@anon.penet.fi"
  "The addr of the anon remailer")

(defvar ms-keyserver-addr     "pgp-public-keys@pgp.ai.mit.edu"
  "The addr of a key server")

(defvar ms-own-name-regexp nil
  "regexp of own name - make sure not included in outgoing anon mail")

(setq mc-supress-sign-query t)

(if (string-match "^3" mc-version)		; mailcrypt shouldn't bug us
	(setq mc-pgp-always-sign 'never )	; w "sign the message"
  (setq mc-pgp-always-sign nil ))		; during our hook funcs

; ========== FLAG-flip funcs ==========
(defun ms-pgpsign-sign-this-msgp (yesp)
  "With no arg, flip the pgp-sign-this-msg? bit.  W { 0 | 1 } arg, set bit."
  (interactive "P")       ; note that we're looking at the raw argument,
                          ; not the numeric version of it.  This means
                          ; that if user supplies no arg, we see nil
                          ; instead of default "1".
  (setq ms-pgpsign-sign-this-msg
		(cond ((equal yesp nil) (not  ms-pgpsign-sign-this-msg))
			  ((equal yesp 1)   t)
			  ((equal yesp 0)   nil)))
  (force-mode-line-update))				; reflect on modeline

(defun ms-anon-this-msgp (yesp)
  "With no arg, flip the anon-this-msg? bit.  W { 0 | 1 } arg, set bit."
  (interactive "P")       ; note that we're looking at the raw argument,
                          ; not the numeric version of it.  This means
                          ; that if user supplies no arg, we see nil
                          ; instead of default "1".
  (setq ms-anon-this-msg
		(cond ((equal yesp nil) (not  ms-anon-this-msg))
			  ((equal yesp 1)   t)
			  ((equal yesp 0)   nil)))
  (if  ms-anon-this-msg (setq ms-pgpsign-sign-this-msg nil) )
  (force-mode-line-update))

(defun ms-crypt-this-msgp (yesp)
  "With no arg, flip the crypt-this-msg? bit.  W { 0 | 1 } arg, set bit."
  (interactive "P")       ; note that we're looking at the raw argument,
                          ; not the numeric version of it.  This means
                          ; that if user supplies no arg, we see nil
                          ; instead of default "1".
  (setq ms-crypt-this-msg
		(cond ((equal yesp nil) (not  ms-crypt-this-msg))
			  ((equal yesp 1)   t)
			  ((equal yesp 0)   nil)))
  (force-mode-line-update))				; reflect on modeline

; ========== keymaps ==========

(define-key mail-mode-map "\C-c\C-s" 'ms-pgpsign-sign-this-msgp)
(define-key mail-mode-map "\C-c\C-a" 'ms-anon-this-msgp)
(define-key mail-mode-map "\C-c\C-e" 'ms-crypt-this-msgp)

(define-key news-reply-mode-map "\C-c\C-s" 'ms-pgpsign-sign-this-msgp)


; ========== add hooks ==========

; ----- when composing a new msg, default to signing it but non anon-ing it
(add-hook 'mail-setup-hook  (lambda () (setq ms-pgpsign-sign-this-msg t)))
(add-hook 'mail-setup-hook  (lambda () (setq ms-crypt-this-msg nil)))
(add-hook 'mail-setup-hook  (lambda () (setq ms-anon-this-msg nil)))

(add-hook 'news-reply-mode-hook (lambda () (setq ms-pgpsign-sign-this-msg t)))
(add-hook 'news-reply-mode-hook  (lambda () (setq ms-anon-this-msg nil)))


; ----- reflect signature and anon variables in modeline

(add-hook 'mail-setup-hook 'ms-modelineform-add-anon)
(add-hook 'mail-setup-hook 'ms-modelineform-add-sign)
(add-hook 'mail-setup-hook 'ms-modelineform-add-crypt)

(add-hook 'news-reply-mode-hook 'ms-modelineform-add-sign)


; ----- anon msg before sending it, and make sure no incriminating info in msg
(add-hook 'mail-send-hook '(lambda () (if ms-anon-this-msg (ms-anon-mail-message 1))))
(add-hook 'mail-send-hook '(lambda () (if ms-anon-this-msg (ms-check-for-own-name))))

; ----- crypt msg before sending it, and make sure it is encrypted for all recipients
(add-hook 'mail-send-hook '(lambda () (if ms-crypt-this-msg ( ms-check-for-encryption-failure))))
(add-hook 'mail-send-hook '(lambda () (if ms-crypt-this-msg ( mc-encrypt-message))))

; ----- sign msg before sending it
(add-hook 'mail-send-hook '(lambda () (if ms-pgpsign-sign-this-msg (mc-sign-message))))
(add-hook 'news-inews-hook '(lambda () (if ms-pgpsign-sign-this-msg (mc-sign-message))))




; ----- misc other hooks
(add-hook 'mc-pre-encryption-hook  'ms-precrypt-expand-aliases)
(add-hook 'mc-pre-encryption-hook  'ms-kill-and-copy-mail-subject)
(add-hook 'mc-post-decryption-hook  'ms-postdecrypt-kill-ms)

(add-hook 'rmail-show-message-hook 'ms-rmail-autoverify)

(add-hook 'gnus-article-prepare-hook 'ms-rmail-autoverify)

; ========== real funcs ==========


(defun ms-modelineform-add-sign ()
  ""
  (setq mode-line-format
		(ms-modelineform-add-flag
		 mode-line-format
		 (list 'ms-pgpsign-sign-this-msg
			   "-<sign: Y>"
			   "-<sign: N>"))))

(defun ms-modelineform-add-anon ()
  ""
  (setq mode-line-format
		(ms-modelineform-add-flag
		 mode-line-format
		 (list 'ms-anon-this-msg
			   "-<anon: Y>"
			   "-<anon: N>"))))

(defun ms-modelineform-add-crypt ()
  ""
  (setq mode-line-format
		(ms-modelineform-add-flag
		 mode-line-format
		 (list 'ms-crypt-this-msg
			   "--<crypt: Y>"
			   "--<crypt: N>"))))

(defun ms-modelineform-add-flag (existing-format new-flag)

 "An ugly function which takes a mode-line-format list, and adds a
  PGP-sign flag as the second to last member by recursively searching.
  This is coded as a func which dynamically alters the format string,
  as opposed to a static redeclaration of the mode-line-format list,
  bc if the user calls some other hook at run time which modifies the
  format list before we redeclare it, our redeclaration of it would
  overwrite whatever changes the user had made to the format list.

  Note #1: This func is written so that if it is invoked more than
  once on a format list, it will only add its pgp-sign flag a single
  time.

  Note #2: If this func is called on a format list of only a single
  elem, it will have a prob coping.  As format strings are never a
  single elem long, this isn't a big deal."

  ; we'll be looking for and/or adding the list
  ; (list 'ms-pgpsign-sign-this-msg "--<PGP sign: YES>" "--<PGP sign: NO >"))
  ;
  ; Why this special list?  When the modeline sftwr is figuring out what
  ; chars to display to the screen, it interps any 3 item lists it finds of
  ; the form ( variable <X> <Y> ) in the same way C interps <var> ? <X> : <Y>
  ; Therefore, inserting this list into the mode-line format list has
  ; the effect of putting a flag in the mode line

  (cons (car existing-format)   ; This func is recursive.  It returns a list
					   ; composed of the first elem of the input list, PLUS:

		(cond
		 ((equal (car existing-format)
				 new-flag)
  		        (cdr existing-format))      ; If this format string already
                               			    ; indicates signature status, no
                               			    ; need to make it do so twice.
                               			    ; Merely return rest of list.

		 ((or (equal (car (cdr existing-format)) "-%-")
			  (equal (car existing-format)
					 nil))                  ; If we see the final elem in
                                            ; the list [ "-%-" indicates
                                            ; "fill in -'s to end of line,
                                            ; and is therefore the last
                                            ; visible elem in the modeline
                                            ; list ] ...
		  (cons
		   new-flag       ; flag list, then the rest of the
		   (cdr existing-format)))          ; old format list.

		 ( t                                ; If none of these are true,
		   (ms-modelineform-add-flag ; recurse w the rest of the
			(cdr existing-format )
			new-flag)))))      ; input list


(defun ms-post-anon-message (format)
  ""
  (news-reply-mode)
  )

(defun ms-anon-mail-message (format)
  "Prepare mail msg to be sent via an anon remailer or via normal mail.
  With no arg, flip from anon to non-anon, or vice versa.
  With { 0 | 1 } arg, prepare for { regular | anonymous }.

Prepare for anonymous remailer:

  1) Add multiple X-Anon-To fields, and move the list of recipients from
	   the To: field to these fields.
  2) Add a X-Anon-Password field, and leave it empty.
  3) Make the value of the To field 'anon@anon.penet.fi'
  4) Turn off PGP-sign-msgs-before-sending flag (name: ms-pgpsign-sign-this-msg)

Prepare for regular mail:

  1) Delete the current value of the To field.
  2) Move the contents of one or more X-Anon-To fields to the To field.
  3) Delete the X-Anon-Password field.
  4) Turn on PGP-sign-msgs-before-sending flag (name: ms-pgpsign-sign-this-msg)

If you want more information on anonymous remailers, send a message
with a subject and a body containing the word 'help' to the address
'help@anon.penet.fi'."

  (save-excursion
    (cond ((or (and (equal format nil)	; *IF* user wants to flip format, and
										; we're in normal mode right now,
					(not (or (mail-position-on-field "X-Anon-To" t)
							 (mail-position-on-field "X-Anon-Password" t))))
										; *OR* user specifes that he wants
										; to go to anon-mode
			   (equal format 1))
		   (progn						; *THEN* change from a normal msg to an anon msg
;			 (setq ms-pgpsign-sign-this-msg nil)
			 (force-mode-line-update)
			 (mail-position-on-field "X-Anon-To" nil)
			 (let ((mail-to-header (get-mail-to)))
			   (if (not (string-match mail-to-header ms-anon-remailer-addr))
				   (insert mail-to-header))) ; a "To" line pting to the remailer is not
											 ; a valid addr that we want the remailer
											 ; to send the msg to...
			 ( if (get-mail-cc)
				 (progn
				   (insert (concat "," (get-mail-cc)))
				   (kill-mail-cc)))
			 (save-excursion
			   (let ((line-end))
				 (end-of-line)
				 (setq line-end (point))
				 (beginning-of-line)
				 (narrow-to-region (point) line-end)
				 (while (re-search-forward "," nil t)
				   (progn
					 (delete-char -1)
					 (newline)
					 (insert "X-Anon-To: ")))
				 (widen)))
			 (kill-mail-to)
			 (mail-to)
			 (insert ms-anon-remailer-addr)
			 (mail-position-on-field "X-Anon-Password" nil)
			 (insert (read-from-minibuffer "Anon Remailer Password: "))))
;		  ((or (and (equal format nil)	; *IF* user wants to flip format, and
;										; we're in anon mode right now,
;					(or (mail-position-on-field "X-Anon-To" t)
;						(mail-position-on-field "X-Anon-Password" t)))
;										; *OR* user specifes that he wants
;										; to go to normal-mode
;			   (equal format 0))
;
;		   (progn						; *THEN* change from an anon msg to a normal msg
;			 (setq ms-pgpsign-sign-this-msg t)
;			 (force-mode-line-update)
;			 (let ((one-addr-already-done nil))
;			   (kill-mail-anon-password)
;			   (if (string-match (get-mail-to) ms-anon-remailer-addr)
;				   (kill-mail-to))
;			   (while  (not (eq (mail-position-on-field "X-Anon-To" t) nil))
;				 (progn
;				   (mail-to)
;				   (if one-addr-already-done (insert ","))
;				   (insert (get-mail-anon-to))
;				   (backward-char 1)
;				   (if (looking-at " ") (delete-char 1) (forward-char 1))
;				   (setq one-addr-already-done t)
;				   (kill-mail-anon-to))))))
;		  (t (progn
;			   (error "You called mail-anon w a non-valid arg.  Use 0, 1 or nothing.")
;			   (sit-for 3 )))

)))


(defun ms-check-for-own-name ()
  "check for own name in outgoing anon mail, and let user abort if found."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(re-search-forward "--text follows this line--")
	(overlay-regexp ms-own-name-regexp 'highlight)
	(if (and (re-search-forward ms-own-name-regexp  nil t  )
			 (y-or-n-p "own name found in outgoing anon mail...abort?"))
		(error "own name found in outgoing anon mail.  ABORTED."))))

(defun ms-check-for-encryption-failure ()
  "check for encryption failure, and let user abort if found."
  (interactive)
  (let ((encrypt-buffer (current-buffer))
		(user)
		(errorsp ))
	(set-buffer "*MailCrypt*" )
	(goto-char (point-min))
	(while 
		(progn
		  (set-buffer "*MailCrypt*" )
		  (re-search-forward "Cannot find the public key matching userid" nil t))
	  (re-search-forward "'.*'" nil t)
	  (setq user  (buffer-substring  (+ 1 (match-beginning 0)) ( - (match-end 0) 1)))
	  (set-buffer encrypt-buffer)
	  (goto-char (point-min))
	  (overlay-regexp user 'highlight 1)
	  (setq errorsp t))
	(if (and errorsp
			 (y-or-n-p "not encrypted for all recipients...abort?"))
		(error "not encrypted for all recipients.  ABORTED."))))


(make-face 'ms-verify-good-face)
(copy-face 'highlight 'ms-verify-good-face)
(set-face-background  'ms-verify-good-face "black")
(set-face-foreground  'ms-verify-good-face "green")

(make-face 'ms-verify-unknown-face)
(copy-face 'highlight 'ms-verify-unknown-face)
(set-face-background  'ms-verify-unknown-face "black")
(set-face-foreground  'ms-verify-unknown-face "orange")


(make-face 'ms-verify-bad-face)
(copy-face 'highlight 'ms-verify-bad-face)
(set-face-background  'ms-verify-bad-face "black")
(set-face-foreground  'ms-verify-bad-face "red")


(defun ms-rmail-autoverify()
  "if an rmail msg has not been pgp verified yet, verify it and highlight approp."
  (interactive)
  (save-excursion
	(let ((verify-buffer (current-buffer)))
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" mc-pgp-signed-begin-line) nil t)

		  (let ((verify-result
				 (or					; either
				  (and (string= mode-name "RMAIL") ; this msg is a verified RMAIL msg
					   (not (eq nil (rmail-message-label-p "verified"))))
				  (if (string= mode-name "RMAIL")
					  (mc-rmail-verify-signature)
					(mc-verify-signature)))) ; or we verify it this time
				(verify-face)
				(msg-delims)
				(buffer-read-only nil))
			(goto-char (point-min))
										; we want to have a third class:
										; key not found
			(setq msg-delims
				  (mc-message-delimiter-positions
				   mc-pgp-signed-begin-line
				   mc-pgp-signed-end-line))
			(set-text-properties (car msg-delims) (cdr msg-delims)
								 (list 'face 'default))
			(cond
             ; ========== verified! ==========
			 ((eq verify-result t)
			  (overlay-regexp
    			   (concat "^" mc-pgp-signed-begin-line) 'ms-verify-good-face 1 )
			  (overlay-regexp
			       (concat "^" mc-pgp-signed-end-line  ) 'ms-verify-good-face 1 ))

             ; ========== not verifiable ==========
			 ((progn
				(save-excursion
				  (set-buffer mc-buffer-name)
				  (goto-char (point-min))
				  (re-search-forward "Can't find the right public key" nil t)))
			  (progn
				(message mode-name)
				(overlay-regexp mc-pgp-signed-begin-line 'ms-verify-unknown-face 1 )
				(overlay-regexp mc-pgp-signed-end-line 'ms-verify-unknown-face 1 )
				(if						; if
					( or				; key found in msg and user decides to snarf
					  ; ===== snarf key from msg? =====
					  (save-excursion
						(if (string= mode-name "Article")
							(set-buffer "*Article*"))

						(goto-char (point-min))
						(if (and (re-search-forward mc-pgp-key-begin-line nil t)
								 (y-or-n-p "snarf key from article? "))
							(let ((buffer-read-only nil))
							  (goto-char (point-min)) ;<<< does this really work?
							  (replace-regexp (concat "^\\(- \\)+" mc-pgp-key-begin-line)
											  mc-pgp-key-begin-line)
							  (goto-char (point-min))
							  (replace-regexp (concat "^\\(- \\)+" mc-pgp-key-end-line)
											  mc-pgp-key-end-line)
							  (goto-char (point-min))
							  (mc-snarf-key (point-min) (point-max)))
						  nil)) ; if snarfing fails, return nil
					  ; ===== get key by finger? =====
					  (and (y-or-n-p "get key via finger? ")
							   (ms-get-key-add-keyring	; get key by finger
								(ms-get-from-rmail-or-gnus ) nil)))
					 ; ===== verify again: either PASS or FAIL =====
					(progn
					  (set-buffer verify-buffer)
					  (ms-rmail-autoverify))

				  (if (y-or-n-p "get key from keyserver? ")
					  (ms-get-key-from-keyserver (ms-get-from-rmail-or-gnus)))))) ; end of cond element
             ; ========== forged! ==========
			 (t
			  (set-text-properties (car msg-delims) (cdr msg-delims)
								   (list 'face 'ms-verify-bad-face)))))))))


(defun ms-get-key-from-keyserver (user-name)
  ""
  (get-buffer-create " keyserver-mail")
  (set-buffer " keyserver-mail")
  (delete-region (point-min) (point-max))
  (mail-mode)
  (let ((buffer-read-only nil))
	(insert "To: "  ms-keyserver-addr "\n" )
	(insert "Subject: get " user-name "\n" )
	(insert "--text follows this line--" "\n\n" ))
  (let ((mail-send-hook nil))
	(mail-send)))


(defun ms-get-from-rmail-or-gnus()
  ""
  (cond ((string= mode-name "RMAIL")	 (get-rmail-from))
		((string= mode-name "Article")   (get-gnus-from))
		(t                                nil)))

(defun ms-get-key-add-keyring (addr keyring)
  ""
  ( or (and (ms-finger-pgp-key addr)			; return t iff complete success
			(mc-pgp-add-key keyring))
	   (progn
;		 (mc-display-buffer " finger-info")
		 (message "Unable to get key via finger.")
		 (sleep-for 2))))

(defun mc-pgp-add-key (keyring)
  ""
  (interactive)
  (let ((buffer " finger-info"))
    (message "Snarfing...")
	(set-buffer buffer)
    (if (mc-process-region (point-min) (point-max) nil
						   mc-pgp-path '("+batchmode" "-kaf" ))
		(not (mc-message mc-pgp-newkey-re buffer "No new keys found")) ; success

	  (mc-display-buffer buffer)		; failure
	  (mc-message mc-pgp-error-re buffer "Error snarfing PGP key" t)
	  nil )))

(defun mc-rmail-verify-signature ()
  "*Verify the signature in the current message."
  (interactive)
  (if (not (equal mode-name "RMAIL"))
      (error "mc-rmail-verify-signature called in a non-RMAIL buffer"))
  (if (mc-verify-signature)
      (progn
		(rmail-add-label "verified")
		t)
	nil))


(defun ms-finger-pgp-key  (address)
  "Resolve a user's address into pgp information if available
                                            pete k. 12/4/94"

  (save-excursion
	(get-buffer-create " finger-info")
	(set-buffer " finger-info")
	(let ((buffer-read-only nil))
	  (delete-region (point-min) (point-max))
	  (call-process "finger" nil " finger-info" nil address)
	  (let ((tmp))
		(save-excursion
		  (goto-char (point-min))
		  (if (search-forward "-----BEGIN PGP PUBLIC KEY BLOCK-----" nil t)
			  (progn
				(beginning-of-line)
				(setq tmp (point))
				(search-forward "-----END PGP PUBLIC KEY BLOCK-----" nil t)
				(end-of-line)
				(buffer-substring tmp (point)))
			'nil))))))


(defun ms-precrypt-expand-aliases ()
										; <<< stolen from a function in mailaliases.el
  (interactive)
  (let ((delimline))
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "\n"))
	(backward-char 1)
	(setq delimline (point-marker))
	(narrow-to-region (point-min) delimline) ; ----
	(goto-char (point-min))
	(perform-replace "@[a-zA-Z_]+\\( +\\|$\\)" "" nil t nil) ; get rid of in-house
	(goto-char (point-min))
	(perform-replace "@[a-zA-Z_]+ *," "," nil t nil) ; machine names
	(goto-char (point-min))
	(if mail-aliases
		(expand-mail-aliases (point-min) delimline))
	(widen)))

(defun ms-postdecrypt-kill-ms ()
  (let ((buffer-read-only nil))
	(goto-char (point-min))
	(perform-replace "$" "" nil t nil)
	(goto-char (point-min))))

; stolen from rmailedit.el.  Needed to modify so that upon ending an
; edit, the "verified" label is removed.  Unfortunately, no hook
; exists.  Sent bug report to FSF.

(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  (if (rmail-summary-exists)
      (save-excursion
	(set-buffer rmail-summary-buffer)
	(rmail-summary-enable)))
  ;; Make sure buffer ends with a newline.
  (rmail-set-attribute "verified" nil)
  (save-excursion
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    ;; Adjust the marker that points to the end of this message.
    (set-marker (aref rmail-message-vector (1+ rmail-current-message))
		(point)))
  (let ((old rmail-old-text))
    ;; Update the mode line.
    (set-buffer-modified-p (buffer-modified-p))
    (rmail-mode-1)
    (if (and (= (length old) (- (point-max) (point-min)))
	     (string= old (buffer-substring (point-min) (point-max))))
	()
      (setq old nil)
      (rmail-set-attribute "edited" t)

      (if (boundp 'rmail-summary-vector)
	  (progn
	    (aset rmail-summary-vector (1- rmail-current-message) nil)
	    (save-excursion
	      (rmail-widen-to-current-msgbeg
	        (function (lambda ()
			    (forward-line 2)
			    (if (looking-at "Summary-line: ")
				(let ((buffer-read-only nil))
				  (delete-region (point)
						 (progn (forward-line 1)
							(point))))))))
	      (rmail-show-message))))))
  (setq buffer-read-only t)
  (ms-rmail-autoverify))

(add-hook 'rmail-show-message-hook 'ms-keyserver-failure)

(defun ms-keyserver-failure ()
  "When receiving 'failed search' mail from keyserver, maybe send mail to user."
  (let ((subject (get-rmail-subject))
		(user)
		(from    (get-rmail-from))
		(mail-buffer (current-buffer)))
	(cond ((and							; (string-match ms-keyserver-addr from)
			(not (rmail-message-label-p "key-requested"))
			(string-match " Your command, GET.* FAILED" subject)
			(y-or-n-p "keyserver failed.  mail user for key "))
		   (let ((tmp-string (get-rmail-subject)))
			 (string-match " [^ ]*@[^ ]*" tmp-string)
			 (setq user (substring tmp-string (match-beginning 0) (match-end 0) )))
		   (get-buffer-create " keyserver-mail")
		   (set-buffer " keyserver-mail")
		   (setq buffer-read-only nil)
		   (delete-region (point-min) (point-max))
		   (mail-mode)
		   (insert "To: "  user "\n" )
		   (insert "Subject: please send me your PGP public key\n" )
		   (insert "--text follows this line--\n\n" )
		   (insert "Hello.  While reading either email or UseNet I came across a PGP\n")
		   (insert "signed msg from you, but did not have your public key to verify\n")
		   (insert "it with.  My mail/newsreader fingered your account for your key\n")
		   (insert "and failed.  It then tried to get your key from the keyserver at\n")
		   (insert "            " ms-keyserver-addr "\n")
		   (insert "but the key was not there.  Please mail me your key.  Thank you.\n")
		   (insert "\n")
;		   (insert "TJIC\n\n")
		   (insert "P.S.  This mail was composed by my mailreading sftwr, which automatically\n")
		   (insert "scans incoming mail, looking for failed keyserver requests, and prompts me\n")
		   (insert "whether it should automatically send this msg on my behalf.  If there is a\n")
		   (insert "bug w this sftwr (for example, you never PGP sign your msgs, so this entire\n")
		   (insert "msg makes no sense), please mail me.  If you're interested in the sftwr\n")
		   (insert "itself (a package I wrote in lisp for emacs; this is just one of the many\n")
		   (insert "crypto/privacy related things it does) please mail tjic@icd.teradyne.com\n")
		   (insert "for details.\n")
		   (let ((mail-send-hook nil))
			 (mail-send))
		   (set-buffer mail-buffer)		; go back to mail buffer
		   (rmail-add-label "key-requested")	; and note that we already bugged this person for their key
		   ))))


;; ========== MISC UTILITY FUNCS ==========

;; ----- font overlay stuff (and version stuff to control it)

(defun tjic-function-version (function)
  (nth 3 (symbol-function function)))

;(if (or (not (fboundp 'overlay-regexp))
;		( <  (tjic-function-version 'overlay-regexp) 1 ))

(defun overlay-regexp ( target-regexp overlay-face &optional count) ;
  "put a given overlay over first COUNT occurances of a given regexp.
If COUNT is not passed in, or is nil, do for all occurances."
 										; function version
  (interactive)
  (let ((buffer-read-only nil))
	(save-excursion
;	  (goto-char (point-min))
	  (let ((cntr 0))
		(while
			(and (re-search-forward target-regexp nil t  )
				 (not (equal cntr count)))
		  (progn
			(set-text-properties (match-beginning 0) (match-end 0)
								 (list 'face overlay-face))
			(setq cntr ( + cntr 1 ))))))))
;)



;; ----- return field contents

(defun get-mail-to   ()        (get-mail-field  "To" ))
(defun get-mail-subj ()        (get-mail-field  "Subject" ))
(defun get-mail-cc   ()        (get-mail-field  "CC" ))
(defun get-mail-anon-to ()   (get-mail-field  "X-Anon-To"))

(defun get-mail-field (string )
  "Get the body a field in an outgoing mail msg
   arg 'function'     specifies function to be used to move to field
   arg 'string'       specifies prompt at beginning of field"

  (save-excursion
	(if (not ( equal (mail-position-on-field string t) nil))
		(let ((temp (point)))
		  (re-search-backward (concat "^" string ":"))
		  (search-forward ":")
		  (while (looking-at " ") (forward-char))
		  (buffer-substring (point) temp))
	nil)))

(defun get-rmail-subject ()   (get-rmail-field  "Subject" ))

(defun get-rmail-from ()
  (let* ((from  (get-rmail-field "From")))
	(setq from
		  (cond ((string-match "(" from)
				 (substring from  0 (match-beginning 0) ))
				((string-match "<.*>" from)
				 (substring from (+ 1 (match-beginning 0)) (- (match-end 0) 1)))
				(t
				 from )))
	(setq from (substring from (string-match "[a-zA-Z]" from)))
	(setq from (substring from 0 (string-match " " from)))
	from ))


(defun get-gnus-from ()
  (save-excursion
   (set-buffer "*Article*")
   (get-rmail-from)))


(defun get-rmail-field (string )
  "Get the body a field in an outgoing mail msg
   arg 'function'     specifies function to be used to move to field
   arg 'string'       specifies prompt at beginning of field"

  (save-excursion
	(search-forward "\n\n" nil 'move)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (let ((temp (point)))
		(if (re-search-backward (concat "^" string ":") nil t)
			(progn
			  (search-forward ":")
			  (setq temp (point))
			  (end-of-line)
			  (buffer-substring (point) temp))
		  "")))))  ; if there's no instance of the field requested,
                   ; return a string anyway, so calling func doesn't
                   ; break

;; ----- move to field

(defun mail-reply-to ()
  "Move point to end of Reply-To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Reply-To"))

(defun mail-anon-password ()
  "Move point to end of X-Anon-Password-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "X-Anon-Password"))

(define-key mail-mode-map "\C-c\C-f\C-r" 'mail-reply-to)
(define-key mail-mode-map "\C-c\C-f\C-p" 'mail-anon-password)

;; ----- kill field

(defun kill-mail-anon-password ()
  "Kill the X-Anon-Password: field in an outgoing mail msg.  Don't Leave prompt."
  (interactive "")
  (kill-mail-field "X-Anon-Password" t ))

(defun kill-mail-anon-to ()
  "Kill the X-Anon-To: field in an outgoing mail msg.  Don't Leave prompt."
  (interactive "")
  (kill-mail-field "X-Anon-To" t ))

(defun kill-mail-cc ()
  "Kill the entire CC: field in an outgoing mail msg."
  (interactive "")
  (kill-mail-field "CC" t))

(defun kill-mail-to ()
  "Kill the To: field in an outgoing mail msg.  Leave prompt."
  (interactive "")
  (kill-mail-field "To" nil))

(defun kill-mail-subject ()
  "Kill the Subject: field in an outgoing mail msg.  Leave prompt."
  (interactive "")
  (kill-mail-field "Subject" nil))

(defun kill-mail-fcc ()
  "Kill the FCC: field in an outgoing mail msg.  Don't Leave prompt."
  (interactive "")
  (kill-mail-field "FCC" t ))


(defun kill-mail-in-reply ()
  "Kill the In-Reply-To: field in an outgoing mail msg.  Don't Leave prompt."
  (interactive "")
  (kill-mail-field "In-Reply-To" t ))

(defun kill-mail-field ( string kill-promptp )
  "Kill a field in an outgoing mail msg (perhaps including the prompt)
   arg 'function'     specifies function to be used to move to field
   arg 'string'       specifies prompt at beginning of field
   arg 'kill-promptp' specifies whether to kill the prompt

   eval as t if field existed, else nil."

  (save-excursion

	(if (not ( equal (mail-position-on-field string t) nil))
		(progn

		  (if  kill-promptp
			  (forward-char))			; if we're going to kill the entire header
										; line, lets make sure to include the \n
		  (let ((temp (point)))
			(re-search-backward (concat "^" string ":"))
			(if (not kill-promptp)
				(progn
				  (search-forward ":")))

			(kill-region temp (point))
			(if (not kill-promptp)
				(insert " ")))
		  t)
	  nil)))
                                        ; if the header still exists, add a " " at
										; the end, so that the package mail-hist doesn't
										; barf when it searches for a colon, then a char

(defun ms-kill-and-copy-mail-subject ()
  ""
  (let ((subj (get-mail-subj)))
	(if (or (not subj)
			(not (string= subj "")))
		(progn
		  (goto-char (point-min))
		  (re-search-forward mail-header-separator nil t)
		  (newline 2)
		  (insert "Subject: " subj))))
  (kill-mail-subject))


(define-key mail-mode-map "\C-c\C-k\C-c" 'kill-mail-cc)
(define-key mail-mode-map "\C-c\C-k\C-s" 'kill-mail-subject)
(define-key mail-mode-map "\C-c\C-k\C-t" 'kill-mail-to)
(define-key mail-mode-map "\C-c\C-k\C-f" 'kill-mail-fcc)
(define-key mail-mode-map "\C-c\C-k\C-i" 'kill-mail-in-reply)

; ========== MISC UNUSED SHIT ==========

; (defvar pgp-s-msg "^-----BEGIN PGP SIGNED MESSAGE-----")
; (defvar pgp-s-beg "^-----BEGIN PGP SIGNATURE-----")
; (defvar pgp-s-end "^-----END PGP SIGNATURE-----")
; (defvar pgp-s-any (concat "\\(" pgp-s-msg "\\|" pgp-s-beg "\\|" pgp-s-end "\\)"))
;
; (defun ms-pgpsign-already-signed ()
;  ""
;  (save-excursion
;	(goto-char (point-min))
;	(let ((signatures-found 0) (keep-going t) (start-point (point))
;	  (while (and keep-going (re-search-forward pgp-s-any nil t ))
;		(progn
;		  (setq signatures-found ( + 1 signatures-found ))
;		  (if (yes-or-no-p (concat
;							(if (not (equal signatures-found 1))
;								(concat (int-to-string signatures-found)
;										(ms-english-ordered-elem signatures-found) " "))
;							"non-indented PGP signature found in msg.  Delete it? "))
;			  (let ((tmp))				; delete old sig
;				( replace-regexp pgps-msg "" )
;				(goto-point (point-min))
;				(if (not (re-search-forward pgp-s-msg nil t ))
;					(error "PGP Message Begin not found.  You cope with it."))
;				(kill-line 0)
;				(if (not (re-search-forward pgp-s-beg nil t ))
;					(error "PGP signed msg missing signature!  You cope with it."))
;				(beginning-of-line )
;				(setq tmp (point))
;				(if (not (re-search-forward pgp-s-end (+ tmp 300) t ) )
;					(error "PGP signature too long.  You cope with it."))
;				(kill-region tmp (point)))
;			(if (not (yes-or-no-p "OK; don't delete old sig.  Sign it anyway?"))
;				(setq keep-going nil)))))
;	  keep-going))))



(defun ms-english-ordered-elem (x)
  (cond ((equal x 1) "-st")
		((equal x 2) "-nd")
		((equal x 3) "-rd")
		(t           "-th")))
