;;; mew-pgp.el --- PGP/MIME for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 17, 1994
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-pgp-version "mew-pgp.el version 0.48")

(require 'mew)
(eval-when-compile
  (if (mew-which-el "timer" load-path)
      (require 'timer)))

;;;
;;; PGP 2.6.x is supported. 
;;; PGP 5.x is supported. But very ad-hoc.
;;; GNUPG 0.9.x is supported. But very ad-hoc.
;;;

(defvar mew-pgp-ver nil
  "Automatically set 0 if PGP version is 2. Set 1 if 5. Set 2 if GNUPG.")

(defconst mew-pgp-ver2 0)
(defconst mew-pgp-ver5 1)
(defconst mew-pgp-verg 2)
(defconst mew-pgp-list '("PGPv2" "PGPv5" "GNUPG"))
(defconst mew-pgp-keys '(pgpv2 pgpv5 gnupg)) ;; use symbols, cases are string

;; mew-prog-pgp is used only for version check
(defvar mew-prog-pgp2  "pgp") ;; "pgp263i", PGP selection
(defvar mew-prog-pgp5  "pgp") ;; PGP selection
(defvar mew-prog-pgp5e "pgpe")
(defvar mew-prog-pgp5s "pgps")
(defvar mew-prog-pgp5v "pgpv")
(defvar mew-prog-pgp5k "pgpk")
(defvar mew-prog-gpg   "gpg") ;; PGP selection

(defvar mew-prog-pgpe (list mew-prog-pgp2 mew-prog-pgp5e mew-prog-gpg))
(defvar mew-prog-pgps (list mew-prog-pgp2 mew-prog-pgp5s mew-prog-gpg))
(defvar mew-prog-pgpv (list mew-prog-pgp2 mew-prog-pgp5v mew-prog-gpg))
(defvar mew-prog-pgpd (list mew-prog-pgp2 mew-prog-pgp5v mew-prog-gpg))
(defvar mew-prog-pgpk (list mew-prog-pgp2 mew-prog-pgp5k mew-prog-gpg))

(defconst mew-prog-pgpe-arg
  '(("-ea" "+language=en" "+batchmode=on" "+armorlines=0")
    ("-a" "+language=en" "+batchmode=on" "+armorlines=0")
    ("--encrypt" "--armor" "--batch")))
(defconst mew-prog-pgpd-arg
  '(("+language=en" "+batchmode=off")
    ("+language=en" "+batchmode=off")
    ("--decrypt")))
(defconst mew-prog-pgps-arg
  '(("-sba" "+language=en" "+batchmode=off")
    ("-ba" "+language=en" "+batchmode=off")
    ("--detach-sign" "--armor")))
(defconst mew-prog-pgpv-arg
  '(("+batchmode=on" "+language=en")
    ("+batchmode=on" "+language=en" "+force=on")
    ("--verify" "--batch")))

(defconst mew-prog-arg-output '("-o" "-o" "--output"))
(defconst mew-prog-arg-input '(nil "-o" nil))
(defconst mew-prog-arg-luserid '("-u" "-u" "--local-user"))
(defconst mew-prog-arg-ruserid '(nil "-r" "--remote-user"))

(defconst mew-prog-pgpk-add-arg
  '(("-ka" "+batchmode=on") ("-a" "+batchmode=on") ("--import" "--batch")))
(defconst mew-prog-pgpk-ext-arg
  '(("-kxfa") ("-xa") ("--export" "--armor" "--batch")))

(defconst mew-pgp-msg-signature
  '("\n\\(.*\\) signature from user \\(.*\\)\\."
    "\n\\(.*\\) signature made"
    " \\(.*\\) signature from \"\\(.*\\)\""))

(defconst mew-pgp-msg-key-id
  '("Key ID \\(\\S +\\) not found"
    ": \\(0x[0-9A-Za-z]+\\)"
    "xxx"))
(defconst mew-pgp-msg-bad-pass
  '("No passphrase" "Cannot unlock private key\\|It can only be decrypted" "bad passphrase"))
(defconst mew-pgp-msg-enter
  '("Enter" "Enter" "xxx"))
(defconst mew-pgp-msg-enter-pass 
  '("Enter pass phrase: " "Enter pass phrase: " "Enter passphrase: "))
(defconst mew-pgp-msg-reenter-pass
  '("Enter pass phrase: " "Enter pass phrase: " "Enter passphrase: "))
(defconst mew-pgp-msg-no-enckey
  '("Key matching" "No encryption keys" "public key not found"))
(defconst mew-pgp-msg-no-validkey
  '("DUMMY" "^WARNING:[ -9;-~\n]+belongs? to:" "no info to calculate a trust probability")) ;;xxx
(defconst mew-pgp-msg-no-vrfkey
  '("Key matching" "unknown keyid" "public key not found"))
(defconst mew-pgp-msg-no-keyring
  '("Keyring file" "Keyring file" "public key not found"))
(defconst mew-pgp-msg-no-seckey-or-secring
  '("You do not have the secret key"
    "Cannot find a private key"
    ": decryption failed: secret key not available"))
(defconst mew-pgp-msg-unsupported
  '("Unsupported packet format" ;; including algorithms and packets
    "Unsupported packet format\\|None of the signatures were understood"
    "xxx"))
(defconst mew-pgp-verify-addr
  '(".* \\(signature from user\\) " "\\(   \\)" "gpg: .* \\(from\\|aka\\) "))

;; 2: ASCII armor corrupted
;; 3:
;; 5:

;; 2: ERROR: or Error:

(defconst mew-pgp-msg-no-export-key
  '("Key not found" "No keys" "nothing exported"))

(defvar mew-pgp-micalg '("pgp-md5" "pgp-sha1" "pgp-sha1"))

;;
;;

(defvar mew-pgp-string nil)
(defvar mew-pgp-running nil)
(defvar mew-pgp-failure nil)

(defvar mew-pgp-decrypt-msg nil)
(defvar mew-pgp-sign-msg nil)

(defconst mew-pgp-encryption-begin "-----BEGIN PGP MESSAGE-----")
(defconst mew-pgp-signature-begin  "-----BEGIN PGP SIGNED MESSAGE-----")
(defconst mew-pgp-key-begin "-----BEGIN PGP PUBLIC KEY BLOCK-----")
(defconst mew-pgp-key-end   "-----END PGP PUBLIC KEY BLOCK-----")

(defconst mew-pgp-err-pass    'mew-err-pass)
(defconst mew-pgp-err-pubring 'mew-err-pubring)
(defconst mew-pgp-err-secring 'mew-err-secring)
(defconst mew-pgp-err-pubkey  'mew-err-pubkey)
(defconst mew-pgp-err-seckey  'mew-err-seckey)
(defconst mew-pgp-err-seckey-or-secring 'mew-err-seckey-or-secring)
(defconst mew-pgp-err-other   'mew-err-other)

(defvar mew-pgp-result-pass     "Pass phrase is wrong.")
(defvar mew-pgp-result-pubring  "No public keyring.")
(defvar mew-pgp-result-secring  "No secret keyring.")
(defvar mew-pgp-result-pubkey   "No his/her public key.")
(defvar mew-pgp-result-invalid
  "His/her public key is invalid. Sign the key by yourself, first.")
(defvar mew-pgp-result-seckey   "No your secret key.")
(defvar mew-pgp-result-seckey-or-secring 
  "PGP NOT decrypted due to no secret keyring or no your secret key.")
(defvar mew-pgp-result-other    "PGP failed for some reasons.")
(defvar mew-pgp-result-sec-succ "PGP decrypted. ")
(defvar mew-pgp-result-dec-fail "PGP NOT decrypted for some reasons.")
(defvar mew-pgp-result-unsup    "PGP unsupported signature.")

(defvar mew-pgp-prompt-enter-pass   "Enter pass phrase (%s): ")
(defvar mew-pgp-prompt-reenter-pass "Re-enter pass phrase (%s): ")

(defmacro mew-pgp-get (list-or-vec)
  (` (elt (, list-or-vec) mew-pgp-ver)))
(defmacro mew-pgp-set (vec val)
  (` (aset (, vec) mew-pgp-ver (, val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP version check
;;

(defun mew-pgp-set-version ()
  (if (not (mew-which mew-prog-pgp exec-path))
      (setq mew-pgp-ver nil)
    (save-excursion
      (mew-set-buffer-tmp)
      (call-process mew-prog-pgp nil t nil)
      (goto-char (point-min))
      (if (search-forward "PGP is now invoked" nil t)
	  (setq mew-pgp-ver mew-pgp-ver5)
	(goto-char (point-min))
	(if (search-forward "Pretty Good Privacy(tm) 2" nil t)
	    (setq mew-pgp-ver mew-pgp-ver2)
	  (goto-char (point-min))
	  (if (search-forward "gpg" nil t)
	      (setq mew-pgp-ver mew-pgp-verg)
	    (setq mew-pgp-ver nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP selection
;;

(defun mew-pgp-select ()
  "Select PGP version and set up environment for selected PGP."
  (interactive)
  (setq mew-prog-pgp
	(completing-read
	 "PGP name : "
	 (mapcar (function (lambda (x) (cons x x)))
		 (list mew-prog-pgp2 mew-prog-pgp5 mew-prog-gpg))
	 nil t))
  (mew-pgp-set-version))

(defun mew-pgp-passphrase (&optional again)
  (let ((prompt (if again
		    mew-pgp-prompt-reenter-pass
		  mew-pgp-prompt-enter-pass)))
    (setq prompt (format prompt (mew-pgp-get mew-pgp-list)))
    (if mew-use-pgp-cached-passphrase
	(mew-input-passwd prompt (mew-pgp-get mew-pgp-keys))
      (mew-input-passwd prompt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP verifying
;;

(defun mew-pgp-verify-check ()
  (let (ret keyid)
    (goto-char (point-min))
    (if (not (re-search-forward (mew-pgp-get mew-pgp-msg-signature) nil t))
	;; this is verification, so the error is about public key
	(progn
	  (goto-char (point-min))
	  (if (search-forward (mew-pgp-get mew-pgp-msg-no-vrfkey) nil t)
	      (progn
		(goto-char (point-min))
		(if (not (re-search-forward (mew-pgp-get mew-pgp-msg-key-id) nil t))
		    (setq keyid "not found")
		  (setq keyid (mew-buffer-substring
			       (match-beginning 1) (match-end 1)))
		  (if (equal mew-pgp-ver mew-pgp-ver2) ;; xxx
		      (setq keyid (concat "0x" keyid))))
		(setq ret (concat mew-pgp-result-pubkey " ID = " keyid)))
	    (goto-char (point-min))
	    (if (search-forward (mew-pgp-get mew-pgp-msg-no-keyring) nil t)
		(setq ret mew-pgp-result-pubring)
	      (goto-char (point-min))
	      (if (re-search-forward (mew-pgp-get mew-pgp-msg-unsupported) nil t)
		  (setq ret mew-pgp-result-unsup)
		;; this line must be nil since this function is used
		;; by the decryption function, too, for signed-then-encrypted
		;; messages. We can't tell whether or not signatures exist
		;; from the outside of the cipher.
		))))
      ;; Signature result is found.
      (setq ret (concat (mew-match 1) " PGP sign "))
      (goto-char (point-max))
      (if (and (boundp 'mew-inherit-from) mew-inherit-from
	       (re-search-backward (concat (concat (mew-pgp-get mew-pgp-verify-addr) ".*") mew-inherit-from) nil t))
	  (progn
	    (beginning-of-line)
	    (looking-at (concat (mew-pgp-get mew-pgp-verify-addr) "\\(.*\\)"))
	    (setq ret (concat ret (mew-match 2))))
	(goto-char (point-max))
	(re-search-backward (concat (mew-pgp-get mew-pgp-verify-addr) "\\(.*\\)") nil t)
	(setq ret (concat ret (mew-match 2))))
      ;; xxx
      (goto-char (point-min))
      (if (search-forward "not certified with enough" nil t)
	  (setq ret (concat ret " MARGINAL"))
	(goto-char (point-min))
	(if (search-forward "not trusted" nil t)
	    (setq ret (concat ret " UNTRUSTED"))
	  (goto-char (point-min))
	  (if (search-forward "not certified with a" nil t)
	      ;; PGP uses "unknown" for validity internally, but
	      ;; prints "undefined" instead of "unknown".
	      (setq ret (concat ret " UNDEFINED"))
	    (setq ret (concat ret " COMPLETE"))))))
    ret))

(defun mew-pgp-verify (file1 file2)
  (message "PGP verifying ... ")
  (let ((ioption (mew-pgp-get mew-prog-arg-input)))
    (save-excursion
      (mew-set-buffer-tmp)
      (if ioption
	  (apply (function call-process)
		 (mew-pgp-get mew-prog-pgpv)
		 nil t nil 
		 (append (mew-pgp-get mew-prog-pgpv-arg)
			 (list ioption file1 file2)))
	(apply (function call-process)
	       (mew-pgp-get mew-prog-pgpv)
	       nil t nil 
	       (append (mew-pgp-get mew-prog-pgpv-arg)
		       (list file2 file1))))
      (message "PGP verifying ... done")
      (mew-pgp-verify-check))))

(defun mew-pgp-verify-old (file)
  (message "PGP verifying ... ")
  (save-excursion
    (mew-set-buffer-tmp)
    (let ((file1 (mew-make-temp-name)))
      (apply (function call-process)
	     (mew-pgp-get mew-prog-pgpv)
	     nil t nil 
	     (append (mew-pgp-get mew-prog-pgpv-arg)
		     (list (mew-pgp-get mew-prog-arg-output) file1 file)))
      (message "PGP verifying ... done")
      (list file1 (mew-pgp-verify-check)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP encrypting
;;

(defun mew-pgp-encrypt-check ()
  (let (ret) ;; this should be nil
    (goto-char (point-min))
    (if (search-forward (mew-pgp-get mew-pgp-msg-no-enckey) nil t)
	(setq ret mew-pgp-result-pubkey)
      (goto-char (point-min))
      (if (search-forward (mew-pgp-get mew-pgp-msg-no-keyring) nil t)
	  (setq ret mew-pgp-result-pubring)
	(goto-char (point-min))
	(if (re-search-forward (mew-pgp-get mew-pgp-msg-no-validkey) nil t)
	    (setq ret mew-pgp-result-invalid))))
    ret))

(defun mew-pgp-encrypt (file1 decrypters)
  (message "PGP encrypting ... ")
  (let ((roption (mew-pgp-get mew-prog-arg-ruserid))
	file2 file3) ;; not unique if makes temp here
    (setq file2 (mew-make-temp-name))
    (save-excursion
      (mew-set-buffer-tmp)
      (insert "Version: 1\n")
      (write-region (point-min) (point-max) file2  nil 'no-msg)
      (setq file3 (concat (mew-make-temp-name) mew-pgp-ascii-suffix))
      (mew-set-buffer-tmp)
      (if roption
	  (let (decs)
	    (while decrypters
	      (setq decs (cons (car decrypters) (cons roption decs)))
	      ;; nreverse later, take care.
	      (setq decrypters (cdr decrypters)))
	    (setq decrypters (nreverse decs))
	    (apply
	     (function call-process) 
	     (mew-pgp-get mew-prog-pgpe)
	     nil t nil
	     (append (mew-pgp-get mew-prog-pgpe-arg)
		     decrypters
		     (list (mew-pgp-get mew-prog-arg-output) file3 file1))))
	(apply (function call-process) 
	       (mew-pgp-get mew-prog-pgpe)
	       nil t nil
	       (mew-pgp-get mew-prog-arg-output) file3 file1 
	       (append (mew-pgp-get mew-prog-pgpe-arg) decrypters)))
      (message "PGP encrypting ... done")
      (list file2 nil file3 nil (mew-pgp-encrypt-check)) ;; both ctes are 7bit
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP decrypting
;;

(defun mew-pgp-decrypt (file1 file2)
  ;; file1 is a key file. just ignore.
  ;; file2 is an encrypted file with PGP.
  (message "PGP decrypting ... ")
  (setq mew-pgp-running 'decrypting)
  (setq mew-pgp-string nil)
  (setq mew-pgp-decrypt-msg nil)
  (setq mew-pgp-failure nil)
  (let ((process-connection-type mew-connection-type2)
	file3 process verify)
    (setq file3 (mew-make-temp-name))
    (setq process
	  (apply
	   (function start-process)
	   "PGP decrypt"
	   (current-buffer)
	   (mew-pgp-get mew-prog-pgpd)
	   (append (mew-pgp-get mew-prog-pgpd-arg)
		   (list (mew-pgp-get mew-prog-arg-output) file3 file2))))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-pgp-process-filter1)
    (set-process-sentinel process 'mew-pgp-process-sentinel)
    ;; Wait for the termination of PGP.
    ;; Emacs doesn't provide synchronize mechanism with
    ;; an asynchronous process. So, take this way. 
    (while mew-pgp-running
	(if mew-xemacs-p
	    (accept-process-output)
	  (sit-for 1)
	  ;; accept-process-output or sleep-for is not enough
	  (discard-input)))
    (message "PGP decrypting ... done")
    (if (file-exists-p file3)
	(progn
	  (save-excursion
	    (mew-set-buffer-tmp)
	    (insert mew-pgp-string)
	    (setq verify (mew-pgp-verify-check)))
	  (if verify 
	      (setq mew-pgp-decrypt-msg
		    (concat mew-pgp-decrypt-msg "\n\t" verify))))
      ;; unpredictable error
      (mew-passwd-set-passwd (mew-pgp-get mew-pgp-keys) nil)
      (if (equal mew-pgp-decrypt-msg mew-pgp-result-sec-succ)
	  (setq mew-pgp-decrypt-msg mew-pgp-result-dec-fail)))
    (list file3 mew-pgp-decrypt-msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP signing
;;

(defun mew-pgp-sign (file1)
  (message "PGP signing ... ")
  (setq mew-pgp-running 'signing)
  (setq mew-pgp-string nil)
  (setq mew-pgp-sign-msg nil)
  (setq mew-pgp-failure nil)
  (let ((process-connection-type mew-connection-type2)
	file2 process)
    (setq file2 (concat (mew-make-temp-name) mew-pgp-ascii-suffix))
    ;; not perfectly unique but OK
    (setq process
	  (apply
	   (function start-process)
	   "PGP sign"
	   (current-buffer)
	   (mew-pgp-get mew-prog-pgps)
	   (append (mew-pgp-get mew-prog-pgps-arg)
		   (list (mew-pgp-get mew-prog-arg-luserid) mew-inherit-signer
			 (mew-pgp-get mew-prog-arg-output) file2 file1))))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-pgp-process-filter1)
    (set-process-sentinel process 'mew-pgp-process-sentinel)
    ;; Wait for the termination of PGP.
    ;; Emacs doesn't provide synchronize mechanism with
    ;; an asynchronous process. So, take this way. 
    (while mew-pgp-running
	(if mew-xemacs-p
	    (accept-process-output)
	  (sit-for 1)
	  ;; accept-process-output or sleep-for is not enough
	  (discard-input)))
    (message "PGP signing ... done")
    (if (not (file-exists-p file2)) ;; for unpredictable error
	(mew-passwd-set-passwd (mew-pgp-get mew-pgp-keys) nil))
    (list file2 nil (mew-pgp-get mew-pgp-micalg) mew-pgp-sign-msg))) ;; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PGP process functions
;;

(defun mew-pgp-process-sentinel (process event)
  (let ((decrypted mew-pgp-result-sec-succ)
	(msg ""))
    (if (not mew-pgp-failure)
	(cond
	 ((equal mew-pgp-running 'decrypting)
	  (setq mew-pgp-decrypt-msg decrypted))
	 ((equal mew-pgp-running 'signing)
	  (setq mew-pgp-sign-msg nil)))
      (cond
       ;; sign or decrypt
       ((equal mew-pgp-failure mew-pgp-err-pass)
	(setq msg mew-pgp-result-pass))
       ;; decrypt-then-verify
       ((equal mew-pgp-failure mew-pgp-err-pubring)
	(setq msg decrypted))
       ;; decrypt-then-vrify
       ((equal mew-pgp-failure mew-pgp-err-pubkey)
	(setq msg decrypted))
       ;; sign
       ((equal mew-pgp-failure mew-pgp-err-secring)
	(setq msg mew-pgp-result-secring))
       ;; sign
       ((equal mew-pgp-failure mew-pgp-err-seckey)
	(setq msg mew-pgp-result-seckey))
       ;; decrypt
       ((equal mew-pgp-failure mew-pgp-err-seckey-or-secring)
	(setq msg mew-pgp-result-seckey-or-secring))
       ;; other
       (t ;; mew-pgp-err-other or nil
	(setq msg mew-pgp-result-other)))
      (cond
       ((equal mew-pgp-running 'decrypting)
	(setq mew-pgp-decrypt-msg msg))
       ((equal mew-pgp-running 'signing)
	(setq mew-pgp-sign-msg msg))))
    (setq mew-pgp-running nil)))

(defun mew-pgp-process-filter1 (process string)
  ;; sign or decrypt, not verify
  (setq mew-pgp-string (concat mew-pgp-string string))
  (cond
   ;; no secret key or no secring for decrypt
   ((string-match (mew-pgp-get mew-pgp-msg-no-seckey-or-secring) string)
    (setq mew-pgp-failure mew-pgp-err-seckey-or-secring)
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; no secring for sign
   ((string-match (mew-pgp-get mew-pgp-msg-no-keyring) string)
    (setq mew-pgp-failure mew-pgp-err-secring)
    ;; Enter secret key filename: 
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; no secret key for sign
   ((string-match (mew-pgp-get mew-pgp-msg-no-enckey) string)
    (setq mew-pgp-failure mew-pgp-err-seckey)
    ;; Enter secret key filename: 
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; pass phrase for sign or decrypt
   ((string-match (mew-pgp-get mew-pgp-msg-enter-pass) string)
    (process-send-string process (format "%s\n" (mew-pgp-passphrase)))
    (set-process-filter process 'mew-pgp-process-filter2))

   ;; just in case
   ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
    (setq mew-pgp-failure mew-pgp-err-other)
    ;; Enter XXX:
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))))

(defun mew-pgp-process-filter2 (process string)
  (setq mew-pgp-string (concat mew-pgp-string string))
  (cond
   ;; re-enter pass phrase
   ((string-match (mew-pgp-get mew-pgp-msg-reenter-pass) string)
    (setq mew-pgp-string nil)
    (mew-passwd-set-passwd (mew-pgp-get mew-pgp-keys) nil) ;; cancel anyway
    (process-send-string process (format "%s\n" (mew-pgp-passphrase 'again)))
    (set-process-filter process 'mew-pgp-process-filter2))

   ;; pass phrases were wrong three times
   ((string-match (mew-pgp-get mew-pgp-msg-bad-pass) string)
    (setq mew-pgp-failure mew-pgp-err-pass)
    (mew-passwd-set-passwd (mew-pgp-get mew-pgp-keys) nil) ;; cancel anyway
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; no pubring for verify
   ((string-match (mew-pgp-get mew-pgp-msg-no-keyring) string)
    (setq mew-pgp-failure mew-pgp-err-pubring)
    ;; Enter public key filename: 
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; no public key for verify
   ((string-match (mew-pgp-get mew-pgp-msg-no-vrfkey) string)
    (setq mew-pgp-failure mew-pgp-err-pubkey)
    ;; Enter public key filename: 
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; after decrypted secret key, symmetric key is not uknown...
   ;; gpg: unknown cipher algorithm
   ;; no secret key or no secring for decrypt
   ((string-match (mew-pgp-get mew-pgp-msg-no-seckey-or-secring) string)
    (setq mew-pgp-failure mew-pgp-err-seckey-or-secring)
    (set-process-filter process 'mew-pgp-process-filter3))

   ;; just in case
   ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
    (setq mew-pgp-failure mew-pgp-err-other)
    ;; Enter XXX:
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))))

(defun mew-pgp-process-filter3 (process string)
  ;; ending or error
  (setq mew-pgp-string (concat mew-pgp-string string))
  ;; string may contain old "Enter"
  (cond
   ;; just in case
   ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
    ;; (setq mew-pgp-failure mew-pgp-err-other) ;; this is wrong
    ;; Enter XXX:
    (process-send-string process "\n")
    (set-process-filter process 'mew-pgp-process-filter3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; shortcut methods
;;

(defun mew-pgp-sign-letter ()
  "Sign the entire draft with PGP. Input your passphrase."
  (interactive)
  (mew-pgp-encode-letter 'pgp-signature))

(defun mew-pgp-encrypt-letter ()
  "Encrypt the entire draft with PGP."
  (interactive)
  (mew-pgp-encode-letter 'pgp-encryption))

(defun mew-pgp-sign-encrypt-letter ()
  "Sign then encrypt the entire draft with PGP. Input your passphrase."
  (interactive)
  (mew-pgp-encode-letter 'pgp-signature-encryption))

(defun mew-pgp-encrypt-sign-letter ()
  "Encrypt then sign the entire draft with PGP. Input your passphrase."
  (interactive)
  (mew-pgp-encode-letter 'pgp-encryption-signature))

(defmacro mew-pgp-encode-letter (type)
  (` (if (null mew-pgp-ver)
	 (message "%s doesn't exist" mew-prog-pgp)
       (mew-draft-make-message (, type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; key distribution
;;

(defun mew-pgp-check-old-pgp ()
  (goto-char (point-min))
  (if (re-search-forward (concat "^" mew-pgp-encryption-begin) nil t)
      'pgp-encryption
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mew-pgp-signature-begin) nil t)
	'pgp-signature
      nil)))


(defun mew-summary-decode-pgp ()
  "Decrypting/verifying old-fashioned PGP messages."
  (interactive)
  (if (null mew-pgp-ver)
      (message "%s doesn't exist" mew-prog-pgp)
    (mew-summary-msg
     (let* ((fld (mew-summary-folder-name))
	    (msg (mew-summary-message-number))
 	    (file (mew-expand-folder-get-msg fld msg))
	    file1 type result win start
	    mew-inherit-from)
       (save-excursion
	 (set-buffer (mew-buffer-message))
	 (mew-elet
	  (widen)
	  (setq win (get-buffer-window (current-buffer)))
	  (setq start (window-start win))
	  (setq type (mew-pgp-check-old-pgp))
	  (if (null type)
	      (save-excursion
		(mew-set-buffer-tmp)
		(insert-file-contents file)
		(setq type (mew-pgp-check-old-pgp))))
	  (if (null type)
	      (message "No PGP message was found")
	    (setq mew-inherit-from
		  (mew-addrstr-parse-address
		   (mew-header-get-value mew-from:)))
	    (goto-char (mew-header-end))
	    (forward-line)
	    (delete-region (point) (point-max))
	    (cond
	     ((equal type 'pgp-encryption)
	      (setq result (mew-pgp-decrypt 'dummy file)))
	     ((equal type 'pgp-signature)
	      (setq result (mew-pgp-verify-old file))))
	    (setq file1 (nth 0 result))
	    (setq mew-syntax-privacy-result
		  (concat mew-x-mew: " <body> " (nth 1 result) "\n"))
	    (save-excursion
	      (goto-char (mew-header-end))
	      (mew-decode-syntax-insert-privacy))
	    (if (not (file-exists-p file1))
		() ;; xxx
	      (mew-frwlet
	       mew-cs-text-for-read mew-cs-dummy
	       (insert-file-contents file1))
	      (delete-file file1))
	    (if mew-mule-p
		(mew-cs-decode-region (point) (point-max) mew-cs-rfc822-trans))
	    (mew-message-set-end-of)
	    (set-window-start win start)
	    (set-buffer-modified-p nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; key distribution
;;

(defun mew-attach-pgp-public-key ()
  "Extract the PGP key for the inputed user on '.'. in attachments"
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't link here.")
    (let* ((error nil)
	   (nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   user file filepath begin end)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir "")) 
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir mew-pgp-key-suffix))
      (if (null filepath)
	  (message "Could not make a file for pgp key, sorry.")
	(setq file (file-name-nondirectory filepath))
	(setq user (car (mew-input-address "Who's key? (%s): " (mew-get-my-address))))
	(save-excursion
	  (mew-set-buffer-tmp)
	  (apply (function call-process)
		 (mew-pgp-get mew-prog-pgpk)
		 nil t nil
		 (append
		  (mew-pgp-get mew-prog-pgpk-ext-arg)
		  (list user)))
	  (goto-char (point-min))
	  (if (search-forward (mew-pgp-get mew-pgp-msg-no-export-key) nil t)
	      (setq error t)
	    (goto-char (point-min))
	    (if (not (search-forward mew-pgp-key-begin nil t))
		(setq error t)
	      (beginning-of-line)
	      (setq begin (point))
	      (if (not (search-forward mew-pgp-key-end nil t))
		  (setq error t)
		(beginning-of-line)
		(forward-line)
		(setq end (point)))
	      (write-region begin end filepath nil 'no-msg))))
	(if error
	    (message "can't extract pgp key for %s" user)
	  (setq mew-encode-syntax
		(mew-syntax-insert-entry
		 mew-encode-syntax 
		 nums
		 (mew-encode-syntax-single file mew-type-apk nil user)))
	  (mew-encode-syntax-print mew-encode-syntax))))))

(defvar mew-pgp-tmp-file nil)

(defun mew-mime-pgp-keys (begin end &optional params execute)
  "A function to add PGP keys in Application/PGP-Keys to your 
public keyring."
  (interactive)
  (insert " ######   #####  ######  #     # ####### #     #\n"
	  " #     # #     # #     # #    #  #        #   #\n"
	  " #     # #       #     # #   #   #         # #\n"
	  " ######  #  #### ######  ####    #######    #\n"
	  " #       #     # #       #   #   #          #\n"
	  " #       #     # #       #    #  #          #\n"
	  " #        #####  #       #     # #######    #\n"
	  "\n\n")
  (if execute
      (mew-pgp-add-keys begin end (mew-current-get 'cache)
			(mew-buffer-message))
    (insert "\nTo add this key to your pubring, type "
	    (substitute-command-keys
	     "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))))

(defun mew-pgp-add-keys (begin end key-buf mes-buf)
  "A function to add PGP keys to your public keyring."
  (interactive)
  (if (not mew-pgp-ver)
      (message "PGP is not found")
    (if (not (y-or-n-p "Add this PGP key onto your public keyring? "))
	()
      (setq mew-pgp-tmp-file (mew-make-temp-name))
      (save-excursion
	(set-buffer key-buf)
	(mew-frwlet
	 mew-cs-dummy mew-cs-autoconv
	 (write-region begin end mew-pgp-tmp-file nil 'no-msg))
	(set-buffer mes-buf)
	(mew-elet
	 (message "Adding PGP keys ... ")
	 (apply (function call-process)
		(mew-pgp-get mew-prog-pgpk)
		nil t nil 
		(append (mew-pgp-get mew-prog-pgpk-add-arg)
			(list mew-pgp-tmp-file)))
	 (message "Adding PGP keys ... done")
	 (insert "\n\n"
		 "**************** IMPORTANT NOTE ****************\n"
		 "When Mew adds PGP keys onto your public keyring,\n"
		 "it is careless about both TRUST and VALIDITY.\n"
		 "It is YOU who set these values. Please use\n")
	 (cond
	  ((equal mew-pgp-ver mew-pgp-ver2)
	   (insert "\"pgp -ke\" and \"pgp -ks\" to change them.\n"))
	  ((equal mew-pgp-ver mew-pgp-ver5)
	   (insert "\"pgpk -e\" and \"pgpk -s\" to change them.\n"))
	  ((equal mew-pgp-ver mew-pgp-verg)
	   (insert "\"gpg --edit-key\" to change them.\n")))
	 (insert "If you don't know what TRUST and VALIDITY is,\n"
		 "you should learn the web of trust system BEFORE\n"
		 "using PGP to protect your privacy.\n"
		 "**************** IMPORTANT NOTE ****************\n")
	 (if (equal mes-buf (mew-buffer-message))
	     ()
	   (let ((inhibit-quit t)) (read-char-exclusive)))))
      (if (file-exists-p mew-pgp-tmp-file)
	  (delete-file mew-pgp-tmp-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; key fetch
;;

(defun mew-pgp-fetch-key (arg)
  "Fetch PGP public key."
  (interactive "P")
  (if (null (processp mew-pgp-fetch-key-process))
      (let (key key-list buf keyid xurl xurl-list xmew uri userid)
	(save-excursion
	  (set-buffer (mew-buffer-message))
	  (setq userid (mew-header-parse-address mew-from:))
	  (cond
	   (arg
	    (setq key-list mew-x-pgp-key-list)
	    (setq key (car key-list))
	    (while key
	      (progn
		(setq xurl (mew-header-get-value key))
		(if (and xurl (string-match "http:[^ \t\n]*" xurl))
		    (progn
		      (setq xurl (substring xurl (match-beginning 0) (match-end 0)))
		      (setq xurl-list (append xurl-list (list xurl)))))
		(setq key-list (cdr key-list))
		(setq key (car key-list)))))
	   (t (setq xmew (mew-header-get-value mew-x-mew:)))))
	(cond
	 ((and arg xurl-list)
	  (setq xurl (car xurl-list))
	  (while (and xurl (not uri))
	    (progn
	      (if (y-or-n-p (format "fetch from %s? " xurl))
		  (setq uri xurl))
	      (setq xurl-list (cdr xurl-list))
	      (setq xurl (car xurl-list)))))
	 ((and xmew
	       (string-match "key. ID = \\(0x[0-9a-fA-F]+\\)" xmew nil)
	       (setq keyid (substring xmew (match-beginning 1) (match-end 1))))
	  (if (y-or-n-p (format "Fetch Key ID=%s? " keyid))
	      (setq uri (format mew-pgp-keyserver-url-template keyid))))
	 ((and userid
	       (y-or-n-p (format "fetch User ID=%s? " userid)))
	  (setq uri (format mew-pgp-keyserver-url-template userid))
	  ))
	(if (not uri)
	    (message "can't find PGP public key's information.")
	  (setq buf (generate-new-buffer mew-buffer-pgpkey))
	  (message "key fetching...%s." uri)
	  (mew-piolet
	   mew-cs-autoconv mew-cs-autoconv
           (setq mew-pgp-fetch-key-process
                 (start-process "Get PGP Key" buf mew-prog-imcat uri)))
	  (set-process-sentinel mew-pgp-fetch-key-process 
				'mew-pgp-fetch-process-done)))
    (if (y-or-n-p "Fetching process is running. Continue fetching process? ")
	()
      (mew-pgp-fetch-process-kill))))

(defun mew-pgp-fetch-process-kill ()
  "Kill the current process fetching PGP keys."
  (interactive)
  (unwind-protect
      (if (processp mew-pgp-fetch-key-process)
	  (kill-process mew-pgp-fetch-key-process))
    (setq mew-pgp-fetch-key-process nil)))

(defun mew-pgp-fetch-process-done (proc str)
  (let* ((buf (process-buffer proc))
	 (wconf (current-window-configuration)))
    (save-excursion
      (mew-pop-to-buffer buf)
      (delete-other-windows)
      (goto-char (point-min))
      (if (and (search-forward mew-pgp-key-begin nil t)
	       (search-forward mew-pgp-key-end nil t))
	  (progn
	    (goto-char (point-max))
	    (insert "\n\n")
	    (mew-pgp-add-keys (point-min) (point-max) 
			      (current-buffer) (current-buffer)))
        (progn
	  (goto-char (point-max))
          (insert "\n\n** PGP Key fetch failure. **\n\n")
          (let ((inhibit-quit t)) (read-char-exclusive))))
      (kill-buffer buf)
      (setq mew-pgp-fetch-key-process nil)
      (set-window-configuration wconf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <Decrypt> or <Decrypt-then-Verify>
;; 
;; no seckey no secring:: <1>
;; You do not have the secret key needed to decrypt this file.
;; 
;;    <Verify> <2>
;; 	Keyring file 'pubring.pgp' does not exist. 
;; 	Enter public key filename:
;; 
;; 	Key matching expected Key ID 1B8BF431 not found in file 'pubring.pgp'.
;; 	Enter public key filename:
;; 
;; <Sign> <1>
;; 
;; A secret key is required to make a signature. 
;; Keyring file 'secring.pgp' does not exist. 
;; Enter secret key filename: 
;; 
;; A secret key is required to make a signature. 
;; Key matching userid 'hoge' not found in file 'secring.pgp'.
;; Enter secret key filename: 
;; 
;; <Encrypt>
;; 
;; Keyring file 'pubring.pgp' does not exist. 
;; 
;; Key matching userid 'hoge' not found in file 'pubring.pgp'.
;; 
;; <Verify>
;; Keyring file 'pubring.pgp' does not exist. 
;; 
;; Key matching expected Key ID 1B8BF431 not found in file 'pubring.pgp'.

(provide 'mew-pgp)

;;; Copyright Notice:

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-pgp.el ends here
