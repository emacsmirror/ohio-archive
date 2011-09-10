;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; PGP 2.2+ encryption/decryption routines for Mail/Rmail/MH modes.
;;
;; Copyright 1992, 1993, and 1994 by Gray Watson and Jack Repenning
;;
;; $Revision: /main/197 $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LCD Archive Entry:
;; pgp|Jack Repenning|jackr@sgi.com|
;; Pretty Good Privacy Version 2.2+ Interface|
;; $Date: 1995/08/10 00:46:34 $|$Revision: /main/197 $|~/interfaces/pgp.el.tar.gz|
;;
;; Also available via anonymous FTP from:
;;	sgigate.sgi.com:pub/pgp-aux/pgp.el.tar.gz
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose and without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies, and that
;; the name of Antaire, SGI, Silicon Graphics Inc., or Net Letters,
;; Inc not be used in advertising or publicity pertaining to
;; distribution of the document or software without specific, written
;; prior permission. 
;;
;; The Antaire Corporation, Silicon Graphics Inc., and Net Letters,
;; Inc. make no representations about the suitability of the software
;; described herein for any purpose.  It is provided "as is" without
;; express or implied warranty.
;;
;; The authors of the program may be contacted at gray.watson@letters.com
;; and jackr@engr.sgi.com.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Put the following in your .emacs to enable the pgp hooks:
;;
;;
;; MH users:
;; (autoload 'pgp-mh-folder-mode-hook "pgp")
;; (autoload 'pgp-mh-letter-mode-hook "pgp")
;;	For mh-e 4.X:
;; (setq 'mh-folder-mode-hook (list mh-folder-mode-hook 
;;				    'pgp-mh-folder-mode-hook))
;; (setq 'mh-letter-mode-hook (list mh-letter-mode-hook 
;;				    'pgp-mh-letter-mode-hook))
;;	For mh-e 5.X:
;; (add-hook 'mh-folder-mode-hook 'pgp-mh-folder-mode-hook)
;; (add-hook 'mh-letter-mode-hook 'pgp-mh-letter-mode-hook)
;;
;; Rmail users:
;; (add-hook 'rmail-mode-hook 'pgp-rmail-mode-hook)
;; (autoload 'pgp-rmail-mode-hook "pgp")
;;
;; All users (examples, if you don't like the defaults):
;; (setq pgp-just-do-it t
;;       pgp-default-send-action " header-sign"
;;       pgp-binary "/usr/local/bin/pgp"
;;       pgp-clear-sign t ; nil / 'ask / t
;;	 pgp-untabify-clearsig nil ; nil / 'ask / t
;;       pgp-ask-before-sending nil
;;       pgp-show-decrypted-boundaries t ; nil / 'ask / t
;;	 pgp-save-decoded-messages t ; nil / 'ask / t
;;       pgp-cache-passphrases 60
;;       pgp-encrypt-with-my-key t ; nil / 'ask / t
;;       pgp-my-user-id "0xE67DF57D"
;;       pgp-encrypt-prepend "some message")
;;
;; Or, while browsing mail, news or any similar sort of system, just
;; do M-x pgp-create-read-keymap.  This will make that browser 
;; PGP-aware for this Emacs session.
;;
;; Likewise, while composing mail, news, or whatever, do
;; M-x pgp-create-write-keymap to convert that composer.
;;
;; If you find other modes that can use pgp.el, and can describe hook
;; functions to ensure they get this processing loaded, drop me a
;; line, and I'll add them to the next release of pgp.el.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SECURITY
;;
;; This module makes use of:
;;
;;   a) a password routine which does not echo chars
;;   b) PGP 2.2's way of taking the password from stdin
;;   c) a secure temporary file directory.
;;
;; However, there are many other ways to overcome Unix security so BEWARE.
;; NEVER assume you have a secure system.  PGP authors recommend using a
;; standalone/personal computer to do your encryption/decryption.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LIMITATIONS
;;
;; These functions make a very half-hearted attempt to generate a search
;; string for pgp to use to find the user's public key.  See pgp-key-lookup
;; for more information and pgp-bbdb-notes for an example of better
;; functionality with some help from bbdb.
;;
;; PGP still suffers from problems with batch-mode -- often still prompting
;; for information.  If it takes an abnormally long amount of time to complete
;; you may have to ^G the process, undo and changes, and then try again.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BBDB
;;
;; The insidious Big Brother DataBase (aka BBDB) is an excellent program
;; for keeping track of net addresses, folk, and correspondence.
;;
;; By adding a special notes field (I use pgpkey) and setting the pgp bbdb
;; variables (see below), you can enter the correct PGP ID string for each
;; user whose key you know.  These routines can then supply the correct pgp
;; key automatically.
;;
;; BBDB should be available at an archive site near you or you can try under
;; the elisp section on ftp.uu.net.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatic Encryption of Outgoing Mail
;;
;; You can have pgp prompt you to encrypt all outgoing messages. This
;; works especially well with BBDB's auto-PGP-ID capabilities (see above).
;;
;; To enable this feature:
;;
;;	All users should:
;;	(setq pgp-ask-before-sending t)
;;
;;	GNU Emacs v.18 users of mail or rmail:
;;	(define-key mail-mode-map "\C-c\C-c" 'pgp-mail-send-and-exit)
;;
;;	lemacs and GNU Emacs v.19 users of mail or rmail:
;;	(add-hook 'mail-send-hook 'pgp-mail-send-hook) ; or do it the v.18 way
;;
;;	All users of mh-e:
;;	(setq mh-before-send-letter-hook 'pgp-mail-send-hook)
;;
;; WARNING: In all cases, if encryption is attempted, you'll have to
;; abort (control-G) the send while examining the pgp results if you
;; don't want the message to be delivered.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VARIABLES
;;

;;
;; Variable:			Description:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pgp-mode			Which mailer you use: 'mh or 'rmail
;; pgp-path			Where PGP stuff is
;; pgp-tmp-dir			Secure temp directory for work files.
;; pgp-binary			Full path to the PGP binary.
;; pgp-my-user-id		PGP User-ID to get your personal key.
;; pgp-encrypt-with-my-key	Auto-encrypt all messages with your key too.
;; pgp-encrypt-prepend		Cleartext message to prepend to encrypted stuff
;; pgp-bbdb-notes		Set to BBDB field to check in BBDB rec for key.
;; pgp-ask-before-sending	Ask if you want to encrypt before delivering.
;; pgp-show-decrypted-boundaries Decrypt message inside boundaries marks
;; pgp-cache-passphrases	Minutes for which to cache passphrases
;;				NOT RECOMMENDED
;;				ONLY WORKS IN V.19
;;

(defconst pgp-version "$Revision: /main/197 $"
  "Version of this copy of pgp.el.")
(defconst pgp-maintainer "jackr@sgi.com"
  "Who gets the bad news when you find a problem.")

;;
;; Mailer specific defines to enabled specific features.
;;
(defvar pgp-mode nil
  "*Indicates which mailer you use: 'mh and 'rmail are currently supported.")

;;
;; Where to find the PGP files.
;;
(defvar pgp-path (or (getenv "PGPPATH")
		     (expand-file-name "~/.pgp"))
  "*Directory where PGP stuff is.")

;;
;; Where to put the pgp.el temp files.  WARNING: should be secure directory
;;
(defvar pgp-tmp-dir (format "%s/tmp" pgp-path)
  "*Directory to put pgp.el temp files.  WARNING: should be secure.")

;;
;; Full-path to the pgp installed binary.  Maybe (expand-file-name "~/bin/pgp")
;;
(defvar pgp-binary "/usr/local/bin/pgp" "*Full path to the pgp binary.")

;;
;; The PGP User-ID for your personal key to sign and encrypt outgoing messages
;;
(defvar pgp-my-user-id (user-full-name)
  "*The PGP User ID to locate your personal key to sign and encrypt messages.")

(defvar pgp-always-confirm-uid nil
  "*Even when the PGP User ID is known, should it be prompted for (so
you can change it)?  See also the function pgp-set-uid.")

;;
;; Do everything possible without asking questions
;;
(defvar pgp-just-do-it nil
  "*If t, don't ask unnecessary questions, don't show errorless status
displays, make every effort to be painless and transparent.")

;;
;; Should the passphrase be cached, and for how long?
;;
(defvar pgp-cache-passphrases nil
  "*If nil, don't do this (recommended).
If t, cache passphrases for five minutes.
If a number, cache passphrases for this many minutes.

		       WARNING WARNING WARNING

Do NOT use this on an unsecure system.  These passphrases can be
read from a core file, if your Emacs should dump core; these
passphrases can be read from a running Emacs that supports
the emacsclient/server feature; your operating system probably
provides other ways as well.  If you do not have the ``timer'' feature
(a part of Emacs 19) available, this feature will not work.")

(defvar pgp-uid-passphrase-alist nil
  "Used to cache passphrases, which is not recommended.")

(defvar pgp-passphrase-timer nil
  "Holds timer ID for pending passphrase-timer invocation")

;;
;; Encrypt all outgoing messages with your private key also so you can read
;; your own outgoing messages.
;;
(defvar pgp-encrypt-with-my-key nil
  "*Set to t to always encrypt outgoing messages with your private key.
Set nil to allow PGP's config.txt to control this.
Set 'ask (not nil or t) to ask each time.")

;;
;; For all encrypted outgoing messages, prepend this plain-text message.
;;
(defvar pgp-encrypt-prepend nil
  "*Set to a string to plain-text prepend to outgoing encrypted messages.")

;;
;; Set this to be the bbdb notes field (I use 'pgpkey) to have pgp check
;; the field in the current BBDB entry for a PGP-ID string.  You should
;; have defined this notes field with the bbdb-insert-new-field command.
;;
;; You can use pgp/bbdb-set-key-id to retrieve key IDs from your key
;; ring.  Or, of course, you can just cut-and-paste them from wherever
;; you find them.
;;
(defvar pgp-bbdb-notes
  (if (or (featurep 'bbdb)
	  (fboundp 'bbdb))
      (progn
	(autoload 'bbdb-current-record "bbdb-com" "Load bbdb before sending")
	'pgpkey))
  "*Set to a BBDB field to have pgp check the field in the current BBDB entry for a pgp-id string.")

;;
;; Set to true if you want pgp to ask whether you want to encrypt a message
;; before sending it.
;;
;; NOTE: you will need to add the following to your .emacs to make it work:
;;
;;  (define-key mail-mode-map "\C-c\C-c" 'pgp-mail-send-and-exit)
;;
(defvar pgp-ask-before-sending nil
  "*Set to t if you want pgp to attempt to encrypt a message before
send it.  NOTE: you will need to enable pgp-mail-send-and-exit for
this to work.  See also pgp-auto-encrypt-on-send.")

;;
;; Set to " sign" in a given buffer to have pgp sign this message when
;; sending. Set to " encrypt" for auto-encryption.
;;
(defvar pgp-auto-encrypt-on-send nil
  "*Set to \" sign\" or \" encrypt\" (or use \\[pgp-auto-encrypt]) to have the
message signed/encrypted on the way out without asking. NOTE: you will
need to enable pgp-mail-send-and-exit and pgp-ask-before-sending for
this to work. This variable is always buffer-local.")

(make-variable-buffer-local 'pgp-auto-encrypt-on-send)
(put 'pgp-auto-encrypt-on-send 'permanent-local t)
(if (assoc 'pgp-auto-encrypt-on-send minor-mode-alist)
    (setcdr (assoc 'pgp-auto-encrypt-on-send minor-mode-alist)
	    (list 'pgp-auto-encrypt-on-send))
  (setq minor-mode-alist
	(append (list (list 'pgp-auto-encrypt-on-send
			    'pgp-auto-encrypt-on-send))
		minor-mode-alist)))

(defvar pgp-default-send-action nil
  "*If nothing specified for a msg, use this for pgp-auto-encrypt-on-send.")


;;
;; 
;;
(defvar pgp-clear-sign nil
  "*Set to t to do all signatures in the clear.
Set to nil (default) to let PGP's config.txt control this.
Set to 'ask (really, anything else) to ask each time.")

;;
;;
;;
(defvar pgp-untabify-clearsig t
  "*Set to nil to inhibit translation of tabs to spaces when clearsigning.
Set to 'ask (not nil or t) to ask about this.
This has to be done, because some mail transports and readers trash
tabs, invalidating your signature.")

;;
;; This will show where the decrypted message was in the mail buffer
;; by inserting a line before and after it.
;;
(defvar pgp-show-decrypted-boundaries t
  "*Set to t to insert a line before and after a message that was decrypted.
Set to 'ask (not nil or t) to ask about this.")

(defvar pgp-decode-marker "---- Below message decoded by pgp.el ----"
  "*String to mark the beginning of a decrypted/verified message.")

(defvar pgp-message-marker "---- Decoded text ----"
  "*String to mark the beginning of a decrypted/verified message.")

(defvar pgp-end-message-marker "---- End of decoded message ----"
  "*String to mark the end of the decrypted/verified message.")

;;
;; Marker line (including leading and trailing newlines) used by 
;; mail interface to separate headers from body.
;;
(defvar pgp-header-marker
  (mapconcat
   'identity
   (list "\n--text follows this line--\n"
	 "\n--------\n"
	 "\n[	]*\n")
   "\\|")
  "*Marker used by mailer to separate headers from bodies.  Default
should cover most mailers.  If you need to set it, be sure to include
leading and trailing newlines.")

;;
;; List of "OK" messages from PGP that you want emphasized.
;; (innocuous if your Emacs doesn't support faces)
;;
(defvar pgp-message-list
  (list
   "^Recipients:.*\\(\n .*\\)*"
   "^Key for user ID:.*\\(\n .*\\)*"
   "^[0-9]+-bit key.*"
   "^Also known as.*"
   "^\\(pub\\|sec\\) *[0-9]+/.*"
   "^Good signature.*"
   "^Signature made.*"
   )
  "*List of PGP messages (as regular expressions) to be marked.")

;;
;; Face used to mark them.
;;
(defvar pgp-message-face
  (if (fboundp 'make-face)
      (make-face 'pgp-message)
    nil)
  "*Face to use in highlighting PGP messages buried in other output. 
Defaults to secondary-selection.")

;;
;; List of "BAD" messages from PGP you want emphasized.
;;
(defvar pgp-warn-list
  (list "[^]*[.\"]$"
	"^But you previously approved.*"
	".*cannot open tty.*")
  "*List of PGP warnings (as regular expressions) to be marked.
Anything with a beep attached is included automatically.")

;;
;; Face used to mark them.
;;
(defvar pgp-warn-face
  (if (fboundp 'make-face)
      (make-face 'pgp-warn)
    nil)
  "*Face to use in highlighting PGP warnings buried in other output.
Defaults to highlight.")

;;
;; Catastropic errors
;;
(defvar pgp-disaster-list
  (list "^?Error: +Bad pass phrase."
	"^?Enter +pass phrase:"
	"^?Unable to get terminal characteristics: ioctl: Bad file number"
	"^This user will not be able to decrypt this message.$"
	"Assertion failed"
	"You do not have the secret key needed to decrypt this file."
	"File is conventionally encrypted"
	"You need a pass phrase to decrypt this file."
	"No passphrase; cannot decrypt."
	"Skipping encrypted data."
	"Warning: ASCII armor missing.*"
	)
  "*List of PGP catastrophic messages - abort processing.")

;;
;; Markers delimiting PGP messages in PGP output
;;
(defvar pgp-msgs-start nil
  "Buffer-local marker for beginning of PGP messages.")

(defvar pgp-msgs-end nil
  "Buffer-local marker for end of PGP messages.")

(defvar pgp-text-end nil
  "Buffer-local marker for end of text being processed.")

(make-variable-buffer-local 'pgp-msgs-start)
(put 'pgp-msgs-start 'permanent-local t)

(make-variable-buffer-local 'pgp-msgs-end)
(put 'pgp-msgs-end 'permanent-local t)

(make-variable-buffer-local 'pgp-text-end)
(put 'pgp-text-end 'permanent-local t)

;;
;; Should decoded-in-place messages be saved back to their original
;; file?
;;
;; Choices: nil		no (default)
;;	    'ask	ask first
;;	    t		yes
(defvar pgp-save-decoded-messages nil
  "*Should MH-mode decoded-in-place messages be saved back to their
original file?  t - yes; nil - no; 'ask - ask")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED EXTERNAL FUNCTIONS
;;
(require 'passwd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs variants
;;
(and window-system
     (cond
      ((string-match "Lucid" emacs-version) ; Lucid

       (fset 'pgp-redraw-frame 'redraw-screen)
       (fset 'pgp-selected-frame 'selected-screen)

       (fset 'pgp-overlayp (symbol-function 'extentp))
       (fset 'pgp-make-overlay (symbol-function 'make-extent))
       (fset 'pgp-delete-overlay (symbol-function 'delete-extent))
       (fset 'pgp-overlay-put (symbol-function 'set-extent-property))
       (or (face-background 'pgp-message)
	   (set-face-background 'pgp-message
				(face-background 'secondary-selection)))
       (or (face-foreground 'pgp-message)
	   (set-face-foreground 'pgp-message
				(face-foreground 'secondary-selection)))
       (or (face-background 'pgp-warn)
	   (set-face-background 'pgp-warn (face-background 'highlight)))
       (or (face-foreground 'pgp-message)
	   (set-face-foreground 'pgp-message
				(face-foreground 'secondary-selection))))

      ((fboundp 'overlayp)		; FSF 
       (fset 'pgp-redraw-frame 'redraw-frame)
       (fset 'pgp-selected-frame 'selected-frame)
       (fset 'pgp-overlayp (symbol-function 'overlayp))
       (fset 'pgp-make-overlay (symbol-function 'make-overlay))
       (fset 'pgp-delete-overlay (symbol-function 'delete-overlay))
       (fset 'pgp-overlay-put (symbol-function 'overlay-put))
       (or (face-foreground pgp-message-face)
	   (set-face-foreground 'pgp-message
				(face-foreground 'secondary-selection)))
       (or (face-background pgp-message-face)
	   (set-face-background 'pgp-message
				(face-background 'secondary-selection)))
       (or (face-foreground pgp-warn-face)
	   (set-face-foreground 'pgp-warn (face-foreground 'highlight)))
       (or (face-background pgp-warn-face)
	   (set-face-background 'pgp-warn (face-background 'highlight))))
      (t				; v.18?  
       (fset 'pgp-overlayp (symbol-function 'or)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LOCAL FUNCTIONS
;;

(autoload 'pgp/bbdb-note-for-key "pgp-bbdb"
  "Search the bbdb for a pgp-bbdb-notes value for key")

(autoload 'pgp-bbdb-net "pgp-bbdb")

(autoload 'pgp/bbdb-set-key-id "pgp-bbdb"
  "Ask pgp for key id for current message's sender (with ARG,
recipient), add it to BBDB."
  t)

;;
;; read in a password without showing user input and while handling ^u (kill)
;; as well as delete and ^h (erase).  does another emacs function do this?
;;
(defun pgp-read-password (user)
  (pgp-cache-passphrase user (or (pgp-lookup-passphrase user)
				 (read-passwd (format "PGP Passphrase for %s: "
						      user)))))

(defun pgp-lookup-passphrase (user)
  "Look up passphrase for USER, if provided.  If no user, provide last
used passphrase."
  (cond
   ((null user)
    (nth 1 (car pgp-uid-passphrase-alist)))
   ((assoc user pgp-uid-passphrase-alist)
    (nth 1 (assoc user pgp-uid-passphrase-alist)))))
   
(defun pgp-cache-passphrase (user pass)
  "If passphrase caching is on, and USER is non-nil, cache PASSPHRASE, 
moving to the head of the list, and noting use-time."
  (if (or (null pgp-cache-passphrases) (null user))
      nil
    (condition-case nil
	(require 'timer)
      (error (require 'itimer)))
    (let* ((time (current-time))
	   (hisec (nth 0 time))
	   (lowsec (nth 1 time))
	   (msec (nth 2 time))
	   (oldpw (nth 1 (assoc user pgp-uid-passphrase-alist))))

      (setq lowsec (+ lowsec
		      (if (numberp pgp-cache-passphrases)
			  (* pgp-cache-passphrases 60)
			300)))
      (if (> lowsec 65535)
	  (setq lowsec (- lowsec 65536)
		hisec (+ hisec 1)))

      (or (null oldpw) (eq pass oldpw) (fillarray oldpw 0))

      (setq pgp-uid-passphrase-alist
	    (append (list
		     (list user pass (list hisec lowsec msec)))
		    (pgp-remove-alist-name user pgp-uid-passphrase-alist)))
      (setq pgp-my-user-id user)
      (pgp-passphrase-timer)))
  pass)

(defun pgp-passphrase-timer ()
  "Time out any old passphrases, and reset timer as needed."
  (if  pgp-cache-passphrases
      (let ((i 0)
	    (timenow (current-time))
	    newlist item itemtime)
	;; Lose all the expired passphrases
	;; newlist ends up with the active ones
	(while (and (setq item (nth i pgp-uid-passphrase-alist))
		    (or (> (nth 0 (setq itemtime (nth 2 item)))
			   (nth 0 timenow))
			(and (= (nth 0 itemtime)
				(nth 0 timenow))
			     (> (nth 1 itemtime)
				(nth 1 timenow)))
			(and (= (nth 1 itemtime)
				(nth 1 timenow))
			     (> (nth 2 itemtime)
				(nth 2 timenow)))))
	  (setq newlist (append newlist (list item)))
	  (setq i (1+ i)))

	;; Now, reschedule self to do it again next time
	(let* ((oldest (nth 2 (nth (1- i) pgp-uid-passphrase-alist)))
	       (nexttime (+ (* 65536 (- (nth 0 oldest)
					(nth 0 timenow)))
			    (- (nth 1 oldest)
			       (nth 1 timenow))))
	       )
	  (if (< nexttime 0) (setq nexttime 1))	; in case clock rolled over
						; while processing

	  (cond
	   ((featurep 'timer)
	    (if pgp-passphrase-timer
		(cancel-timer pgp-passphrase-timer))
	    (setq pgp-passphrase-timer 
		  (if (setq pgp-uid-passphrase-alist newlist)
		      (run-at-time nexttime nil 'pgp-passphrase-timer))))
	   ((featurep 'itimer)
	    (if pgp-passphrase-timer
		(delete-itimer pgp-passphrase-timer))
	    (setq pgp-passphrase-timer
		  (if (setq pgp-uid-passphrase-alist newlist)
		      (start-itimer "pgp-itimer"
				    'pgp-passphrase-timer
				    nexttime))))))))
  t)

;;;###autoload
(defun pgp-uncache-passphrase (&optional user)
  "Remove passphrase cache for USER (optional), or first on list."
  (interactive "sUncache passphrase for user (default: all): ")
  (setq pgp-uid-passphrase-alist
	(cond
	 ((string= user "")
	  (mapcar
	   (function (lambda (entry)
		       (fillarray (nth 1 entry) 0)))
	   pgp-uid-passphrase-alist)
	  nil)
	 (user
	  (let* ((u (if (string= user "") pgp-my-user-id user))
		 (entry (assoc u pgp-uid-passphrase-alist)))
	    (if entry
		(progn
		  (fillarray (nth 1 entry) 0)
		  (pgp-remove-alist-name u pgp-uid-passphrase-alist)))))
	 (t
	  (if pgp-uid-passphrase-alist
	      (progn
		(fillarray (nth 1 (nth 0 pgp-uid-passphrase-alist)) 0)
		(cdr-safe pgp-uid-passphrase-alist)))))))

;;
;; try and auto-generate a pgp key lookup string.
;; see if you have a bbdb notes entry else returns To: address
;; with everything past the @ or ! picked off.
;;
(defun pgp-key-lookup (&optional field)
  ;; Returns a list consisting of a human-friendly name for the
  ;; (first) person named in FIELD, and (if known) their pgp key ID,
  ;; which is a more reliable way to talk to pgp.
  (or field (setq field "To"))
  (let (key)
    (setq key
	  (save-excursion
	    (require 'rfc822)
	    (goto-char (point-min))
	    (re-search-forward (concat
				"^" field ":"
				"\\(.*\\(\n[ 	].*\\)*\\)"))
	    (setq key
		  (car (rfc822-addresses
			(buffer-substring (match-beginning 1) (match-end 1)))))
	    (if (string-match "@" key) (substring key 0 (match-end 0)) key)))
    (pgp-key-canonicalize key)))

;;
;; Generate a list of keys for all the addressees of this message
;;
(defun pgp-to:keys-lookup (&optional field)
  ;; Returns a list of entries like pgp-key-lookup, but for all
  ;; to/cc addressees.
  (require 'rfc822)
  (let ((case-fold-search t)
	(keys (list 'dummy)))
    (save-excursion
      (save-restriction
	(pgp-narrow-to-headers)
	(goto-char (point-min))
	(while (re-search-forward
		(concat "^\\(" (or field "to\\|cc\\|bcc") "\\) *:")
		nil 'end)
	  (nconc
	   keys (mapcar 'pgp-key-canonicalize
			(rfc822-addresses
			 (buffer-substring (point)
					   (progn
					     (re-search-forward "^[^ ]" nil 'end)
					     (beginning-of-line)
					     (point)))))))))
    (cdr keys)))

;;
;; Translate KEY, a general user ID or email address, into the best
;; available form for interface to PGP (the key ID, if available from
;; bbdb, otherwise "addr@")
;;
(defun pgp-key-canonicalize (key)
  "Put KEY into best possible for for interface to PGP"
  (append
   (list (if (string-match "@" key)
	     (substring key 0 (match-beginning 0))
	   key))
   (if (and
	pgp-bbdb-notes
	(get-buffer bbdb-buffer-name))
       (pgp/bbdb-note-for-key key)
     (list key))))

;;
;; Narrow the current buffer to its headers
;;
(defun pgp-narrow-to-headers ()
  "Narrow current message to just the headers"
  (widen)
  (goto-char (point-min))
  (re-search-forward pgp-header-marker nil 'end) ; if headers only,
						 ; use whole buffer
  (narrow-to-region (point-min) (point)))

;;
;; check to see if pgp returned any errors.
;; right now it just makes sure we did not have a bad pass phrase
;;
(defun pgp-check-for-errors ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pgp-msgs-start)
      (narrow-to-region (point) (or pgp-msgs-end (point-max)))
      (if (catch 'pgp-disaster-found
	    (mapcar
	     (function (lambda (msg)
			 (if (save-excursion (re-search-forward msg pgp-msgs-end t))
			     (throw 'pgp-disaster-found t))))
	     pgp-disaster-list)
	    nil)
	  (progn
	    (beep)
	    (switch-to-buffer (current-buffer))
	    (pgp-mark-messages)
	    (cond
	     ((or (string-match "[Pp]ass *[Pp]hrase"
				(buffer-substring pgp-msgs-start pgp-msgs-end))
		  (string-match "cannot open tty"
				(buffer-substring pgp-msgs-start pgp-msgs-end)))
	      (pgp-uncache-passphrase)
	      (message "PGP reports bad passphrase.  Press a key to continue."))
	     (t
	      (message "PGP disaster.  Press any key to continue.")))
	    (read-char)
	    (error "Canceled.")
	    )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Apply overlays to PGP messages and warnings
;;
(defun pgp-mark-messages ()
  "Set overlays for PGP messages and warnings."
  (and (not (equal (symbol-function 'pgp-overlayp)
		   (symbol-function 'or)))
       (let (overlay overlays)
	 (save-restriction
	   (narrow-to-region (point) (or pgp-msgs-end (point-max)))

	   (mapcar
	    (function (lambda (re)
			(save-excursion
			  (while (re-search-forward re nil t)
			    (setq overlay
				  (pgp-make-overlay
				   (match-beginning 0)
				   (match-end 0)))
			    (pgp-overlay-put overlay
					     'face pgp-message-face)
			    (setq overlays (if (null overlays)
					       (list overlay)
					     (append overlays
						     (list overlay))))))))
	    pgp-message-list))

	 (save-restriction
	   (narrow-to-region (point) (or pgp-msgs-end (point-max)))
	   (mapcar
	    (function (lambda (re)
			(save-excursion
			  (while (re-search-forward re nil t)
			    (setq overlay
				  (pgp-make-overlay
				   (match-beginning 0)
				   (match-end 0)))
			    (pgp-overlay-put overlay 'face pgp-warn-face)
			    (setq overlays (if (null overlays)
					       (list overlay)
					     (append overlays
						     (list overlay))))))))
	    (append pgp-warn-list pgp-disaster-list)))
	 overlays)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; display current window temporarily, allowing reader to scroll around.
;;
(defun pgp-ephemeral-display-buffer ()
  (save-window-excursion
    (save-excursion
      (delete-other-windows)
      (let ((overlays (pgp-mark-messages))
	    key)
	(unwind-protect
	    (condition-case erval
		(while
		    (and
		     (message
		      (cond
		       ((and (pos-visible-in-window-p (point-min))
			     (not (pos-visible-in-window-p (point-max))))
			"Press SPC to scroll forward, RET to accept, A to abort.")
		       ((and (not (pos-visible-in-window-p (point-min)))
			     (pos-visible-in-window-p (point-max)))
			"Press DEL to scroll back, RET to accept, A to abort.")
		       (t
			"Press SPC to scroll forward, DEL to scroll back, RET to accept, A to abort.")))
		     (cond
		      ((eq (setq key (read-char)) ? )
		       (scroll-up)
		       t)
		      ((eq key ?\C-?)
		       (scroll-down)
		       t)
		      ((or (eq key ?q)
			   (eq key ?Q)
			   (eq key ?\n)
			   (eq key ?\r))
		       (message "")
		       nil)
		      ((or (eq key ?a)
			   (eq key ?A))
		       (signal 'error (list 'abort))))
		     ))
	      (error
	       (cond
		((memq 'abort erval)
		   t)
		((memq 'end-of-buffer erval)
		 nil)
		(t
		 (signal 'error erval)))
	      ))
	  (and overlays (mapcar
			 (function (lambda (overlay)
				     (pgp-delete-overlay overlay)))
			 overlays)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Find any PGP warnings in the buffer, return as newlined string
;;
(defun pgp-snarf-warnings ()
  (let (messages)
    (save-restriction
      (widen)
      (narrow-to-region (point) pgp-msgs-end)
      (save-excursion
	(goto-char (point-min))
	(mapcar
	 (function (lambda (re)
		     (save-excursion
		       (while (re-search-forward re nil t)
			 (setq messages
			       (concat messages
				       "\n"
				       (buffer-substring
					(match-beginning 0)
					(match-end 0))))))))
	 pgp-warn-list)))
    messages))

(defun pgp-snarf-msgs ()
  (let (messages)
    (save-restriction
      (widen)
      (narrow-to-region (point) pgp-msgs-end)
      (save-excursion
	(goto-char (point-min))
	(mapcar
	 (function (lambda (re)
		     (save-excursion
		       (while (re-search-forward re nil t)
			 (setq messages
			       (concat messages
				       "\n"
				       (buffer-substring
					(match-beginning 0)
					(match-end 0))))))))
	 pgp-message-list)))
    messages))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Like shell-command-on-region, but always runs pgp, and manages
;; stdout/stderr
;;
(defun pgp-command-on-region (start end command &optional flag)
  "Like shell-command-on-region, but provide only the arg string, not
the command name itself, which will be forced to pgp-binary.  PGP's
own messages are sorted out to the front, and a pair of markers
delimiting them is planted."
  (let ((pgp-msg-file (make-temp-name (concat pgp-tmp-dir "/")))
	(process-connection-type nil)
	(stuff (buffer-substring start end))
	pgp-proc)
    (unwind-protect
	(save-excursion
	  (if flag
	      (delete-region start end)
	    (set-buffer (get-buffer-create "*Shell Command Output*"))
	    (erase-buffer))
	  (setq pgp-proc (start-process "pgp-proc" (current-buffer)
					  "/bin/sh" "-c"
					  (format "%s 2>%s %s"
						  pgp-binary pgp-msg-file
						  command)))
	  (process-kill-without-query pgp-proc)
	  (set-marker (process-mark pgp-proc) (point))
	  (process-send-string (process-name pgp-proc) stuff)
	  (process-send-eof (process-name pgp-proc))
	  (while (equal (process-status pgp-proc) 'run)
	    (accept-process-output pgp-proc))
	  (setq pgp-text-end (point-marker)))
      (if flag
	  (goto-char start)
	;; maybe only stderr stuff
	(set-buffer (get-buffer-create "*Shell Command Output*"))
	(goto-char (point-min)))
      (setq pgp-msgs-start (point-marker))
      (setq pgp-msgs-end
	    (set-marker (make-marker)
			(+ (point)
			   (car (cdr (insert-file-contents pgp-msg-file)))))))
    (delete-file pgp-msg-file)
    (if pgp-proc (delete-process pgp-proc))

    (save-excursion
      (goto-char pgp-text-end)
      (beginning-of-line 0)
      (if (looking-at "Process pgp-proc finished")
	  (delete-region (point) pgp-text-end))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Remove PGP messages and clear out markers
;;
(defun pgp-remove-pgp-msgs ()
  "Remove any PGP messages from the buffer."
  (if pgp-msgs-start
      (save-excursion
	(delete-region pgp-msgs-start pgp-msgs-end)))
  (setq pgp-msgs-start nil
	pgp-msgs-end nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up necessary context
;;
(defmacro pgp-setup (ps-env &optional ps-uid ps-pass ps-force ps-uprompt)
  "Set PS-ENV, &optional PS-UID, and PS-PASS (symbols), (PS-FORCE
force-confirms ps-uid), prompting with PS-UPROMPT for user ID."
  (or (symbolp ps-env) (error "Wrong type argument: symbolp, %s" ps-env))
  (or (symbolp ps-uid) (error "Wrong type argument: symbolp, %s" ps-uid))
  (or (symbolp ps-pass) (error "Wrong type argument: symbolp, %s" ps-pass))

  (list 'progn
	(`(setq (, ps-env)
		(append
		 (list (concat "TMP=" pgp-tmp-dir) "PGPPASSFD=0" )
		 (, ps-env))))

	(if ps-uid
	    (`(if (or (, ps-force)
		      pgp-always-confirm-uid
		      (and (not (, ps-uid)) (not pgp-my-user-id)))
		  (let (puid)
		    (setq puid (read-string (or (, ps-uprompt) "As what user ID: ")))
		    (if (string= puid "")
			(setq (, ps-uid) pgp-my-user-id)
			(setq (, ps-uid) puid)))
		(setq (, ps-uid) pgp-my-user-id))))

	(if ps-pass
	    (`(if (not (string= (, ps-uid) "none"))
		  (setq (, ps-pass) (pgp-read-password (, ps-uid))))))
	nil				; keep pass off stack
	)
  )


(defun pgp-remove-alist-name (name alist)
  "Removes element whos car is NAME from ALIST.  Copying operation."
  (cond ((string-equal name (car (car alist)))
	 (cdr alist))
	((null alist)
	 nil)
	(t
	 (cons (car alist)
	       (pgp-remove-alist-name name (cdr alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERACTIVE FUNCTIONS
;;


;;
;; encrypt a message while in Mail mode
;;
;;;###autoload
(defun pgp-encrypt-message (&optional arg)
  "Encrypt an outgoing mail message using pgp in Mail mode.  ARG
allows changing UID."
  (interactive "*P")
  (let (key command pass receiver start user
	    (process-environment (copy-sequence process-environment)))
    
    ;; setup the environment for pgp with tmp directory
    ;; get the password and signature address
    (pgp-setup process-environment user pass
	       arg (format "Signature to use or 'none' [%s]: " pgp-my-user-id))

    ;; get the receiver obvious information
    (let ((keylist (pgp-to:keys-lookup))
	  (prompt "Encrypt message for [%s] or 'none': ")
	  (endstring "none")
	  r)

      (catch 'done
	(while t

	  (setq key (or (car keylist) (list endstring endstring))) 
	  (setq keylist (cdr keylist))

	  (setq r (if pgp-just-do-it
		      ""
		    (read-string (format prompt
					 (if (string= (car key) (car (cdr key)))
					     (car key)
					   (concat (car key)
						   " ("
						   (car (cdr key))
						   ")"))))))
	  (setq r (if (string= r "") (car (cdr key)) r))
	  (if (string= r endstring)
	      (throw 'done t)
	    (setq receiver (concat receiver "'" r "' ")))
	  (setq prompt "Encrypt message for [%s]: "
		endstring "(end)"))))
		 
    ;; should we encrypt with user's key also?
    (if (cond ((eq t pgp-encrypt-with-my-key) t)
	      ((null pgp-encrypt-with-my-key) nil)
	      (t (y-or-n-p (format "Encrypt message for yourself [%s]? "
				   pgp-my-user-id))))
	(setq receiver (concat  receiver "'" pgp-my-user-id "' "))
      )
    
    ;; copy the message into the message file
    (goto-char (point-min))
    (re-search-forward pgp-header-marker)
    (setq start (point))
    (undo-boundary)
    (if pass (insert pass))
    (insert"\n")
    
    ;; call pgp on the message
    (message "Working.  Please wait...")
    (if (string= user "none")
	(setq command (format "+batchmode -feat %s" receiver))
      (setq command (format "+batchmode -feat -su '%s' %s"
			    user receiver))
      )

    (goto-char start)
    (pgp-command-on-region start (point-max) command t)
    (pgp-check-for-errors)
    
    ;; remove the pgp stuff if the user wants
    (goto-char pgp-msgs-start)
    (cond
     ((and pgp-just-do-it (pgp-snarf-warnings))
      (if (pgp-ephemeral-display-buffer)
	  (progn
	    (pgp-remove-pgp-msgs)
	    (error "Encryption failed"))
	(pgp-remove-pgp-msgs)))
     ((and (not pgp-just-do-it) (pgp-ephemeral-display-buffer))
      (pgp-remove-pgp-msgs)
      (error "Encryption aborted"))
     (t
      (pgp-remove-pgp-msgs)
      (goto-char start)
      (if pgp-encrypt-prepend
	  (insert pgp-encrypt-prepend "\n"))))
    (message "Working.  Please wait...Done.")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sign a message in Mail mode
;;
;;;###autoload
(defun pgp-sign-message (&optional arg)
  "Sign your mail message using pgp in mail mode.  ARG allows changing
UID."
  (interactive "P")
  (let (clear command pass start user
	      (process-environment (copy-sequence process-environment)))
    
    ;; setup the environment for pgp with tmp directory
    ;; get signature.
    ;; get the password
    (pgp-setup process-environment user pass
	       arg (format "Signature to use [%s]: " pgp-my-user-id))
    
    (setq clear (cond ((eq t pgp-clear-sign) t)
		      ((null pgp-clear-sign) nil)
		      (t (y-or-n-p "Clear-text sign? "))))
    
    ;; copy the message into the message file
    (undo-boundary)
    (goto-char (point-min))
    (re-search-forward pgp-header-marker)
    (setq start (point))
    (if (and clear (cond ((eq t pgp-untabify-clearsig) t)
			 ((null pgp-untabify-clearsig) nil)
			 (t (y-or-n-p "Convert tabs to spaces? "))))
	(untabify start (point-max)))
    (insert pass "\n")
    
    ;; call pgp on the message
    (message "Working.  Please wait...")
    (setq command (format "+batchmode -fast -u '%s'" user))
    (if clear
	(setq command (concat command " +clear")))
    (pgp-command-on-region start (point-max) command t)
    (pgp-check-for-errors)
    
    ;; remove the pgp stuff if the user wants
    (goto-char start)
    (undo-start)
    (cond
     ((and pgp-just-do-it (pgp-snarf-warnings))
      (pgp-ephemeral-display-buffer)
      (undo-more 1)
      (error "Signature failed"))
     ((and (not pgp-just-do-it) (pgp-ephemeral-display-buffer))
      (undo-more 1)
      (error "Signature aborted"))
     (t
      (re-search-forward "-+BEGIN PGP \\(SIGNED \\)?MESSAGE-+\n")
      (delete-region start (match-beginning 0))))
    (if (and (not clear) pgp-encrypt-prepend)
	(progn
	  (previous-line 1)
	  (insert pgp-encrypt-prepend "\n")))
    ))

;;
;; Like pgp-sign-message, but stuffs the signature into a mail header.
;; This is a non-standard format, supported by pgp.el and one other
;; package (Rick Busdieker's pem.el).  If PGP-MIME ever stabilizes,
;; this should be replaced with that.
;;
;;;###autoload
(defun pgp-header-sign-message (&optional arg)
  "Sign current message, hiding the signature in a mail header.  ARG
allows changing UID."
  (interactive "*P")
  (save-excursion
    (undo-boundary)

    (let ((pgp-clear-sign t)		; force a clearsig
	  pgp-version pgp-signature pgp-tail-start pgp-siggie-start)
      (pgp-sign-message arg)		; aborts when unhappy
      (goto-char (point-min))

      (search-forward "-----BEGIN PGP SIGNED MESSAGE-----\n\n")
      (delete-region (match-beginning 0) (match-end 0))

      (re-search-forward
       "\n-----BEGIN PGP SIGNATURE-----\nVersion: *\\(.*\\)\n\n")
      (setq pgp-tail-start (match-beginning 0))
      (setq pgp-siggie-start (match-end 0))

      (setq pgp-version (buffer-substring (match-beginning 1)
					  (match-end 1)))

      (search-forward "-----END PGP SIGNATURE-----")
      (setq pgp-signature (buffer-substring pgp-siggie-start
					    (match-beginning 0)))

      (delete-region pgp-tail-start (point-max))

      (goto-char (point-min))

      (save-restriction			; narrow to headers
	(save-excursion			; un-trash things PGP trashes
	  (replace-regexp "^- " ""))

	(re-search-forward "^$\\|^[^a-zA-Z \t\n]")
	(narrow-to-region (point-min) (match-beginning 0))

	(goto-char (point-min))

	(cond 
	 ((re-search-forward "^X-Pgp-Version:.*\n" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	 ((re-search-forward "^X-Pgp" nil t)
	  (beginning-of-line))
	 ((re-search-forward "^Subject:" nil t)
	  (beginning-of-line))
	 (t
	  (goto-char (point-max))))      
	(insert "X-Pgp-Version: " pgp-version "\n")

	(save-excursion
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward "^X-Pgp-Signed:" nil t)
	    (beginning-of-line)
	    (delete-region (point)
			   (save-excursion
			     (forward-char 1)
			     (and (re-search-forward "^\\S " nil 'or-end)
				  (forward-char -1))
			     (point))))))

	(narrow-to-region (point)
			  (save-excursion
			    (insert "X-Pgp-Signed: " pgp-signature)
			    (point)))
	(forward-char 1)
	(replace-regexp "^." "\t\\&")

	)

      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; insert your public key into a message in mail mode
;;
;;;###autoload
(defun pgp-insert-key ()
  "Insert your pgp public key into a an outgoing mail message in mail mode"
  (interactive)
  (let (key (public (concat pgp-tmp-dir "/public"))
	    (process-environment (copy-sequence process-environment)))
    
    ;; setup the environment for pgp with tmp directory
    (pgp-setup process-environment key nil
	       t (format "Public key to extract [%s]: " pgp-my-user-id))
    
    ;; copy the public into the public file
    ;; name varies with pgp version
    (let ((public-asc (concat public ".asc")))
      (if (file-exists-p public-asc)
	  (delete-file public-asc))
    
      ;; get the public key into the buffer
      (undo-boundary)
      (if (catch 'pgp-approved
	    (save-excursion
	      (save-window-excursion
		(message "Working.  Please wait...")
		(pgp-command-on-region (point) (point)
					 (format "+batchmode -kxa '%s' %s"
						 key public) nil)
		(other-window 1)
		(goto-char (point-min))
		(throw 'pgp-approved (not (pgp-ephemeral-display-buffer)))
		)
	      ))
	  (insert-file (if (file-exists-p public-asc)
			   public-asc
			 public)))
      (delete-file (if (file-exists-p public-asc)
		       public-asc
		     public))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add the public key from the current message to your key ring
;;
;;;###autoload
(defun pgp-add-key (arg)
  "Add public key from current message to your key ring."
  (interactive "P")
  (save-excursion
    (save-window-excursion
      (cond
       ((eq pgp-mode 'mh)
	;; armour against bbdb/mh other-window confusion
	(if (and (boundp 'bbdb-use-pop-up) 
		 bbdb-use-pop-up
		 (stringp bbdb-buffer-name))
	    (delete-windows-on bbdb-buffer-name))
	(other-window 1)))
      (let ((cb (current-buffer)))
	(pgp-command-on-region (point-min) (point-max) "+batchmode -f")
	(switch-to-buffer "*Shell Command Output*")
	(if (pgp-ephemeral-display-buffer)
	    nil
	  (switch-to-buffer cb)
	  (message "Adding public key[s].  Please wait...")
	  (pgp-command-on-region (point-min) (point-max) "+batchmode -fka")
	  (goto-char (point-min))
	  (switch-to-buffer "*Shell Command Output*")
	  (pgp-ephemeral-display-buffer)
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive function to set (or unset) the silent-auto-send flag.
;;
;;;###autoload
(defun pgp-set-auto-encrypt (arg)
  "Set pgp-auto-encrypt-on-send to ARG."
  (interactive)
  (setq pgp-auto-encrypt-on-send arg)
  (message "Message auto-encryption:%s"
	   (if pgp-auto-encrypt-on-send
	       pgp-auto-encrypt-on-send
	     " off"))
  (if (fboundp 'pgp-redraw-frame)
      (pgp-redraw-frame (pgp-selected-frame))
    (redraw-display)))

;;;###autoload
(defun pgp-auto-encrypt (arg)
  "Toggle (with ARG, cancel) the current buffer's auto-encryption 
when sent out.  Interacts with pgp-ask-before-sending and
pgp-auto-encrypt-on-send."
  (interactive "P")
  (pgp-set-auto-encrypt
	(if arg
	    nil
	  (cond
	   ((string= pgp-auto-encrypt-on-send " sign") " encrypt")
	   ((string= pgp-auto-encrypt-on-send " encrypt") " header-sign")
	   ((string= pgp-auto-encrypt-on-send " header-sign") " none")
	   (t " sign")
	   ))))

;;;###autoload
(defun pgp-auto-encrypt-encrypt nil (interactive) (pgp-set-auto-encrypt " encrypt"))
;;;###autoload
(defun pgp-auto-encrypt-sign nil (interactive) (pgp-set-auto-encrypt " sign"))
;;;###autoload
(defun pgp-auto-encrypt-header-sign nil (interactive) (pgp-set-auto-encrypt " header-sign"))
;;;###autoload
(defun pgp-auto-encrypt-none nil (interactive) (pgp-set-auto-encrypt " none"))
;;;###autoload
(defun pgp-auto-encrypt-cancel nil (interactive) (pgp-set-auto-encrypt nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (Re)set user ID
;;
;;;###autoload
(defun pgp-set-uid ()
  "Prompt for and reset current user ID."
  (interactive)
  (let (env)
    (if pgp-cache-passphrases
	(pgp-setup env pgp-my-user-id nil
		   t (format "New PGP user ID [%s]: " pgp-my-user-id))
      (pgp-setup env pgp-my-user-id nil
		 t (format "New PGP user ID [%s]: " pgp-my-user-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOOK FUNCTIONS
;;
;; Use these functions to hook into your mailer and similar packages.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check to see if we want to encrypt the message before sending
;;
;;;###autoload
(defun pgp-mail-send-and-exit (arg)
  "Do some checking to see if encryption is needed and then call mail-send-and-exit."
  (interactive "P")
  (pgp-mail-send-hook)
  (mail-send-and-exit arg)
  )

;;
;; mailer hook to do the actual message processing
;;
(defun pgp-mail-send-hook ()
  "Autoencrypt outgoing mail (hookable from mailer)."
  (pgp-check-and-mark-encryptable)
  (or (save-excursion			; already encrypted or signed
	(let (case-fold-search)
	  (goto-char (point-min))
	  (re-search-forward "^-+BEGIN PGP [SIGNED ]*MESSAGE-+\n" nil
			     t)))

      (save-excursion			; ... or header-signed
	(let (case-fold-search)
	  (goto-char (point-min))
	  (re-search-forward "^X-Pgp-Signed:" nil t)))
      
      (cond				; OK, not done yet - see what
					; to do
       ((string= pgp-auto-encrypt-on-send " sign") ; auto-sign
	(pgp-sign-message))

       ((string= pgp-auto-encrypt-on-send " header-sign") ; auto-header-sign
	(pgp-header-sign-message))

       ((string= pgp-auto-encrypt-on-send " none") ; do nothing
	t)

       (pgp-auto-encrypt-on-send	; auto-encrypt
	(pgp-encrypt-message))

       (pgp-ask-before-sending	; figure out what to do
	(if (and (or			; not using bbdb or has bbdb key
		  (not pgp-bbdb-notes)
		  (and (get-buffer bbdb-buffer-name)
		       (string-match "^0x[0-9A-Z]+$"
				     (car (cdr (pgp-key-lookup))))))
		 (y-or-n-p "Encrypt this message before sending? "))
	    (pgp-encrypt-message))))))

;;
;; Suitable for a startup hook: if all the addressees have known keys,
;; then mark the message for encryption (when, eventually, it's sent)
;;
;;;###autoload
(defun pgp-check-and-mark-encryptable (&optional field)
  "If this message is addressed entirely to folks for whom we know a
key, mark it for encryption upon send."
  (interactive)
  (if (and (or (not pgp-auto-encrypt-on-send))
	   (catch 'all-keys-known
	     (mapcar (function (lambda (key)
				 (or (string-match "^0x" (nth 1 key))
				     (throw 'all-keys-known nil))))
		     (pgp-to:keys-lookup field))
	     t))
      (setq pgp-auto-encrypt-on-send " encrypt"))
  (or pgp-auto-encrypt-on-send
      (setq pgp-auto-encrypt-on-send pgp-default-send-action))
  (if (string= " encrypt" pgp-auto-encrypt-on-send)
      (message "PGP action on send:%s." pgp-auto-encrypt-on-send)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Decrypt or verify, as appropriate, as many times as needed
;;
;; Usable interactively, or as an mh-show-hook.
;;
;;;###autoload
(defun pgp-decode ()
  "Scan current message, decrypting and verifying in-place whatever
needs it.  Option pgp-save-decoded-messages controls whether the
result is saved back to the file."
  (interactive)
  (let ((initial-w-config (current-window-configuration))
	(dots "...")
	case-fold-search
	initial-buffer start
	working)
    
    (save-window-excursion
      (cond
       ((and (eq pgp-mode 'mh) (eq major-mode 'mh-folder-mode))
	(other-window 1))
       ((eq pgp-mode 'rmail)
	(rmail-edit-current-message)))

      (setq initial-buffer (current-buffer))
      (goto-char (point-min))

      (if (and (save-excursion
		 (not (search-forward (concat "\n" pgp-decode-marker "\n")
				      nil t)))
	       (save-restriction
		 (pgp-narrow-to-headers)
		 (goto-char (point-min))
		 (re-search-forward "^X-Pgp-Signed:" nil t)))

	  ;; setup the environment for pgp with tmp directory
	  ;; get the user ID and password
	  (let* ((sigstart (match-end 0))
		 (pgp-vers (if (save-excursion
				 (re-search-forward
				  "^X-Pgp-Version: *\\(.*\\)" nil
				  t))
			       (buffer-substring
				(match-beginning 1)
				(match-end 1))
			     "unknown"))
		 sigend)
	    (setq working t)
	    (message "PGP: Working %s" dots)
	    (goto-char sigstart)
	    (re-search-forward "^\\S ")
	    (setq sigend (match-beginning 0))
	    (forward-char -1)
	    (re-search-forward "^\n\\|^[^a-zA-Z \t\n].*\n")
	    (setq start (point))
	    (save-excursion		; trash stuff PGP trashes
	      (let (case-fold-search)
		(replace-regexp "^\\(-\\|From \\)" "- \\&")))
	    (insert "-----BEGIN PGP SIGNED MESSAGE-----\n\n")
	    (goto-char (point-max))
	    (insert "\n-----BEGIN PGP SIGNATURE-----\n")
	    (insert "Version: " pgp-vers "\n\n")
	    (save-excursion
	      (insert (buffer-substring sigstart sigend)))
	    (replace-regexp "^\\s +" "")
	    (beginning-of-line 2)
	    (insert "-----END PGP SIGNATURE-----\n")
	    (pgp-decode-region start (point) t))


	;; not header-signed
	(while (re-search-forward
		"^-+BEGIN PGP \\(SIGNED \\)?MESSAGE-+\n" nil t)

	  (save-window-excursion
	    (set-window-configuration initial-w-config)
	    (setq working t)
	    (message "PGP: Working %s" dots)
	    (setq dots (concat dots "."))
	    (sit-for 0))

	  (setq start (match-beginning 0))
	  (goto-char start)

	  (save-excursion
	    (re-search-forward
	     "^-+END PGP \\(SIGNATURE\\|MESSAGE\\)-+$")) ; may error out
	  
	  (pgp-decode-region start (1+ (match-end 0)) t))))
    
    (if working (message "PGP: Working ... Done."))

    (save-excursion
      (set-buffer initial-buffer)
      (if (buffer-modified-p)
	  (if (cond ((eq t pgp-save-decoded-messages) t)
		    ((null pgp-save-decoded-messages) nil)
		    (t (y-or-n-p "Save decoded message? ")))
	      (if (eq pgp-mode 'rmail)
		  (rmail-cease-edit)
		(save-buffer))
	    (set-buffer-modified-p nil))))))

(fset 'pgp-decrypt-message (symbol-function 'pgp-decode))
(fset 'pgp-verify-message (symbol-function 'pgp-decode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Decrypt or verify the region, either in place or in a pop-up
;; buffer.
;;
;;;###autoload
(defun pgp-decode-region (start end arg)
  "Do the needful for REGION.  If ARG, replacing with result (and any
messages)."
  (interactive (list (region-beginning) (region-end) current-prefix-arg))
  (let ((buf (get-buffer-create " *PGP Decode Buffer*"))
	(text (buffer-substring start end))
	(process-environment (copy-sequence process-environment))
	pass messages result)

    (undo-boundary)
    (let (encrypt-reply)
      (save-excursion
	(set-buffer buf)
	(erase-buffer)
	(insert text)
	(goto-char (point-min))
	(cond
	 ((save-excursion
	    (re-search-forward "^-----BEGIN PGP MESSAGE-----$" nil t))
	  (pgp-setup process-environment pgp-my-user-id pass
		     nil "Decrypt as user: ")
	  (insert pass)
	  (setq encrypt-reply t))
	 (t
	  (pgp-setup process-environment pgp-my-user-id nil
		     nil "Decrypt as user: ")))
	(insert "\n")
	(pgp-command-on-region (point-min) (point-max)
			       "+batchmode -f"
			       t)
	(goto-char (point-min))
	(pgp-check-for-errors)
	(setq messages (or (concat (pgp-snarf-msgs)
				   (pgp-snarf-warnings))
			   ""))
	(setq result (buffer-substring pgp-msgs-end (point-max))))
      (if arg
	  (let ((mark-bounds (cond ((eq t pgp-show-decrypted-boundaries) t)
				   ((null pgp-show-decrypted-boundaries) nil)
				   (t (y-or-n-p "Mark boundaries of decryption? "))))
		mesbegin mesend)
	    (delete-region start end)

	    (if mark-bounds (insert pgp-decode-marker "\n"))

	    (setq mesbegin (point))
	    (insert messages)
	    (setq mesend (point))
	    (save-excursion
	      (save-restriction
		(goto-char mesbegin)
		(narrow-to-region mesbegin mesend)
		(pgp-mark-messages)))

	    (if mark-bounds (insert "\n" pgp-message-marker))
	    (insert "\n" result)
	    (if mark-bounds (insert "\n" pgp-end-message-marker))
	    (insert "\n")

	    (if encrypt-reply (setq pgp-auto-encrypt-on-send " encrypt")))
	(pop-to-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Exotica annex
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add menus to menu bar, where supported
;;

;;;###autoload
(defun pgp-create-read-keymap ()
  "Create the PGP menu for use when browsing. "
  (interactive)

  (if (local-key-binding "\C-c/") (local-unset-key "\C-c/"))

  (local-set-key "\C-c/f" 'pgp-uncache-passphrase)
  (local-set-key "\C-c/d" 'pgp-decode)
  (local-set-key "\C-c/v" 'pgp-decode)
  (local-set-key "\C-c/a" 'pgp-add-key)
  (local-set-key "\C-c/n" 'pgp/bbdb-set-key-id)

  (cond
   ((string-match "Lucid" (emacs-version))
    (set-buffer-menubar current-menubar)
    (add-submenu nil (list "PGP"
			   (vector "Decrypt/Verify Message" 'pgp-decode t)
			   (vector "Grab Public Key" 'pgp-add-key t)
			   (if (featurep 'bbdb)
			       (vector "Note Sender's Key in BBDB"
				       'pgp/bbdb-set-key-id t)
			     (vector "(BBDB unavailable)"
				     'beep t)))))

   ((string-match "19" emacs-version)
    (local-set-key [menu-bar PGP]
		   (cons "PGP" (make-sparse-keymap "PGP")))
    (local-set-key [menu-bar PGP note]
		   (cons "Note Sender's Key in BBDB" 'pgp/bbdb-set-key-id))
    (local-set-key [menu-bar PGP add]
		   (cons "Grab Public Key" 'pgp-add-key))
    (local-set-key [menu-bar PGP decode]
		   (cons "Decrypt/Verify Message" 'pgp-decode))
    )

   (t					; just ignore the request
    t)))

;;;###autoload
(defun pgp-create-write-keymap ()
  "Create the PGP menu for use when writing."
  (interactive)

  (if (local-key-binding "\C-c/") (local-unset-key "\C-c/"))

  (local-set-key "\C-c/f" 'pgp-uncache-passphrase)
  (local-set-key "\C-c/e" 'pgp-auto-encrypt-encrypt)
  (local-set-key "\C-c/s" 'pgp-auto-encrypt-sign)
  (local-set-key "\C-c/h" 'pgp-auto-encrypt-header-sign)
  (local-set-key "\C-c/d" 'pgp-decode)
  (local-set-key "\C-c/x" 'pgp-insert-key)

  (cond
   ((string-match "Lucid" (emacs-version))
    (set-buffer-menubar current-menubar)
    (add-submenu nil (list "PGP"
			   (vector "Encrypt Message" 'pgp-encrypt-message t)
			   (vector "Decrypt Message" 'pgp-decode t)
			   (vector "Sign Message" 'pgp-sign-message t)
			   (vector "Sign in X-Pgp- Header" 'pgp-header-sign-message t)
			   (vector "Insert Public Key" 'pgp-insert-key t)
			   (vector "---" nil nil)
			   (vector "Encrypt on Send" 'pgp-auto-encrypt-encrypt t)
			   (vector "Sign on Send" 'pgp-auto-encrypt-sign t)
			   (vector "Header Sign on Send" 'pgp-auto-encrypt-header-sign t)
			   (vector "---" nil nil)
			   (vector "Inhibit Send Action" 'pgp-auto-encrypt-none t)
			   (vector "Default Send Action" 'pgp-auto-encrypt-cancel t)
			   (vector "Confirm Encryptability" 'pgp-check-and-mark-encryptable t))))

   ((string-match "19" emacs-version)

    (local-set-key [menu-bar PGP]
		   (cons "PGP" (make-sparse-keymap "PGP")))

    (local-set-key [menu-bar PGP confirm-encryptability]
		   (cons "Confirm Encryptability" 'pgp-check-and-mark-encryptable))
    (local-set-key [menu-bar PGP cancel-send-action]
		   (cons "Default Send Action" 'pgp-auto-encrypt-cancel))
    (local-set-key [menu-bar PGP inhibit-send-action]
		   (cons "Inhibit Send Action" 'pgp-auto-encrypt-none))
    (local-set-key [menu-bar PGP separator-pending] '("--"))

    (local-set-key [menu-bar PGP header-sign-on-send]
		   (cons "Header Sign on Send" 'pgp-auto-encrypt-header-sign))
    (local-set-key [menu-bar PGP sign-on-send] 
		   (cons "Sign on Send" 'pgp-auto-encrypt-sign))
    (local-set-key [menu-bar PGP encrypt-on-send]
		   (cons "Encrypt on Send" 'pgp-auto-encrypt-encrypt))
    (local-set-key [menu-bar PGP separator-deferred] '("--"))

    (local-set-key [menu-bar PGP insert]
		   (cons "Insert Public Key" 'pgp-insert-key))
    (local-set-key [menu-bar PGP header-sign]
		   (cons "Sign in X-Pgp- Header" 'pgp-header-sign-message))
    (local-set-key [menu-bar PGP sign]
		   (cons "Sign Message" 'pgp-sign-message))
    (local-set-key [menu-bar PGP decrypt]
		   (cons "Decrypt Message" 'pgp-decode))
    (local-set-key [menu-bar PGP encrypt]
		   (cons "Encrypt Message" 'pgp-encrypt-message))
    )

   (t					; just ignore
    t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hook into proper modes
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yes, there really are Emaxen so primitive they lack add-hook, 
;; still in circulation!
;;

;;;###autoload
(defun pgp-mh-folder-mode-hook nil
  (setq pgp-mode 'mh)
  (if (eq window-system 'x)
      (pgp-create-read-keymap))

  (add-hook 'mh-show-hook 'pgp-decode)
  (if (fboundp 'remove-hook)
      (remove-hook 'mh-folder-mode-hook 'pgp-mh-folder-mode-hook)))

;;;###autoload
(defun pgp-mh-letter-mode-hook nil
  (setq pgp-mode 'mh)
  (add-hook 'mh-letter-mode-hook 'pgp-check-and-mark-encryptable)
  (add-hook 'mh-before-send-letter-hook 'pgp-mail-send-hook)

  (if (eq window-system 'x)
      (pgp-create-write-keymap))

  (if (fboundp 'remove-hook)
      (remove-hook 'mh-letter-mode-hook 'pgp-mh-letter-mode-hook)))

;;;###autoload
(defun pgp-rmail-mode-hook nil
  (setq pgp-mode 'rmail)
  (define-key rmail-mode-map "\C-ca" 'pgp-add-key)
  (define-key rmail-mode-map "\C-cd" 'pgp-decode)
  (define-key rmail-mode-map "\C-cv" 'pgp-decode)

  (define-key mail-mode-map "\C-cd" 'pgp-decode)
  (define-key mail-mode-map "\C-ce" 'pgp-encrypt-message)
  (define-key mail-mode-map "\C-ck" 'pgp-insert-key)
  (define-key mail-mode-map "\C-cs" 'pgp-sign-message)
  (define-key mail-mode-map "\C-cv" 'pgp-decode)

  (if (eq window-system 'x)
      (pgp-create-read-keymap)))


(autoload 'reporter-submit-bug-report "reporter")

;;;###autoload
(defun pgp-report-bug nil
  "Submit a bug report on pgp.el via email."
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list (list 'pgp-disaster-list
					  'pgp-encrypt-prepend)))
    (reporter-submit-bug-report
     pgp-maintainer
     (substring pgp-version 12 -2)
     (list 'pgp-mode
	   'pgp-tmp-dir
	   'pgp-binary
	   'pgp-my-user-id
	   'pgp-always-confirm-uid
	   'pgp-just-do-it
	   'pgp-cache-passphrases
	   'pgp-passphrase-timer
	   'pgp-encrypt-with-my-key
	   'pgp-bbdb-notes
	   'pgp-ask-before-sending
	   'pgp-auto-encrypt-on-send
	   'pgp-default-send-action
	   'pgp-clear-sign
	   'pgp-untabify-clearsig
	   'pgp-show-decrypted-boundaries
	   'pgp-save-decoded-messages
	   'pgp-header-marker
	   'pgp-message-face
	   'pgp-warn-face
	   'pgp-message-list
	   'pgp-warn-list
	   'pgp-disaster-list
	   'pgp-encrypt-prepend))))
(fset 'report-pgp-el-bug (symbol-function 'pgp-report-bug)) ;backwards
							    ;compat

(if (fboundp 'add-hook)
    nil
  (defun add-hook (hook function &optional append)
    "Add to the value of HOOK the function FUNCTION unless already present.
\(It becomes the first hook on the list unless optional APPEND is non-nil, in 
which case it becomes the last).  HOOK should be a symbol, and FUNCTION may be
any valid function.  HOOK's value should be a list of functions, not a single
function.  If HOOK is void, it is first set to nil."
    (or (boundp hook) (set hook nil))
    (or (if (consp function)
	    ;; Clever way to tell whether a given lambda-expression
	    ;; is equal to anything in the hook.
	    (let ((tail (assoc (cdr function) (symbol-value hook))))
	      (equal function tail))
	  (memq function (symbol-value hook)))
	(set hook 
	     (if append
		 (nconc (symbol-value hook) (list function))
	       (cons function (symbol-value hook)))))))


;;;;;;;
(provide 'pgp)
