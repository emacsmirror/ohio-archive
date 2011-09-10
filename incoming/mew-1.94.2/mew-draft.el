;;; mew-draft.el --- Draft mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Sep  3, 1999

;;; Code:

(defconst mew-draft-version "mew-draft.el version 0.36")

(require 'mew)
(if mew-xemacs-p (require 'easymenu))

(defvar mew-draft-mode-map   nil)
(defvar mew-draft-header-map nil)
(defvar mew-draft-body-map   nil)

(defvar mew-draft-mode-syntax-table nil
  "*Syntax table used while in Draft mode.")

(if mew-draft-mode-syntax-table
    ()
  (setq mew-draft-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?% "." mew-draft-mode-syntax-table))

(defvar mew-draft-mode-toolbar-menu
  '("Attachment Commands"
    ["Insert a File by Linking"
     mew-attach-link
     (mew-attach-not-line012-1)]
    ["Insert a File by Copying"
     mew-attach-copy
     (mew-attach-not-line012-1)]
    ["Insert Audio"
     mew-attach-audio
     (mew-attach-not-line012-1)]
    ["Insert an External Reference"
     mew-attach-external-body
     (mew-attach-not-line012-1)]
    ["Insert a Sub-Multipart"
     mew-attach-multipart
     (mew-attach-not-line012-1)] 
    ["Read a New File into a Buffer"
     mew-attach-find-new-file
     (mew-attach-not-line012-1)]
    ["Insert PGP public keys"
     mew-attach-pgp-public-key
     (mew-attach-not-line012-1)]
    "----"
    ["Delete This Part"
     mew-attach-delete 
     (mew-attach-not-line012-1-dot)]
    "----"
    ["Describe This Part"
     mew-attach-description
     (mew-attach-not-line0-1-dot)]
    ["Specify A File Name"
     mew-attach-disposition
     (mew-attach-not-line012-1-dot)]
    ["Change the Type"
     mew-attach-type
     (mew-attach-not-line0-1-dot)]
    ["Encode with Gzip64"
     mew-attach-gzip64
     (mew-attach-not-line0-1-dot)]
    ["Encode with Base64"
     mew-attach-base64
     (mew-attach-not-line0-1-dot)]	 
    ["Encode with Quoted-Printable"
     mew-attach-quoted-printable
     (mew-attach-not-line0-1-dot)]
    ["Sign with PGP"
     mew-attach-pgp-sign
     (mew-attach-not-line0-1-dot)]
    ["Encrypt with PGP"
     mew-attach-pgp-enc 
     (mew-attach-not-line0-1-dot)]
    "----"
    ["Undo Encoding"
     mew-attach-undo
     (mew-attach-not-line0-1-dot)]
    "----"
    ["Read This File into a Buffer"
     mew-attach-find-file
     (mew-attach-not-line012-1-dot)]
    )
  )

(defvar mew-draft-mode-menu-spec
  (list
   "Mew/Draft"
   ["Cite" mew-draft-cite t]
   ["Cite without Label"  mew-draft-yank t]
   ["Insert Config:"	  mew-draft-insert-config t]
   mew-draft-mode-toolbar-menu
   ["Make MIME Message"   mew-draft-make-message        (mew-header-p)]
   ["Send Message"        mew-draft-send-letter         (not (mew-header-p))]
   ["Prepare Attachments" mew-draft-prepare-attachments (and (mew-header-p) (not (mew-attach-p)))]
   ["Insert Signature"    mew-draft-insert-signature    t]
   ["Kill Draft"          mew-draft-kill                t]
   "----"
   '("PGP"
     ["PGP Sign"              mew-pgp-sign-letter         (mew-header-p)]
     ["PGP Encrypt"           mew-pgp-encrypt-letter      (mew-header-p)]
     ["PGP Sign then Encrypt" mew-pgp-sign-encrypt-letter (mew-header-p)]
     ["PGP Encrypt then Sign" mew-pgp-encrypt-sign-letter (mew-header-p)])
   '("Privacy"
     ["All messages"               mew-draft-toggle-privacy-always t]
     ["Msgs replying to encrypted" mew-draft-toggle-privacy-encrypted t]
     ["This message"   		   mew-draft-set-privacy-type t])
   '("FIB"
     ["FIB next item"     mew-fib-next-item     (not (mew-attach-p))]
     ["FIB previous item" mew-fib-previous-item (not (mew-attach-p))]
     ["FIB flush input"   mew-fib-flush-input   (not (mew-attach-p))]
     ["FIB fill default"  mew-fib-fill-default  (not (mew-attach-p))]
     ["FIB delete frame"  mew-fib-delete-frame  (not (mew-attach-p))])))

(if mew-draft-header-map
    ()
  (setq mew-draft-header-map (make-sparse-keymap))
  (define-key mew-draft-header-map "\t"     'mew-draft-header-comp)
  (define-key mew-draft-header-map "\C-c\t" 'mew-draft-circular-comp)
  (define-key mew-draft-header-map "\e\t"   'mew-draft-expand))

(cond
 (mew-use-overlay-keymap
  (defun mew-draft-share-keymap (symmap)
    (define-key (symbol-value symmap) "\C-c\C-m" 'mew-draft-make-message)
    (define-key (symbol-value symmap) "\C-c\C-c" 'mew-draft-send-letter)
    (define-key (symbol-value symmap) "\C-c\C-a" 'mew-draft-prepare-attachments)
    (define-key (symbol-value symmap) "\C-c\C-o" 'mew-draft-insert-config)
    (define-key (symbol-value symmap) "\C-c\C-l" 'mew-draft-rehighlight)
    (define-key (symbol-value symmap) "\C-c\C-u" 'mew-draft-undo)
    (define-key (symbol-value symmap) "\C-c\C-q" 'mew-draft-kill)
    (define-key (symbol-value symmap) "\C-c\C-s" 'mew-pgp-sign-letter)
    (define-key (symbol-value symmap) "\C-c\C-e" 'mew-pgp-encrypt-letter)
    (define-key (symbol-value symmap) "\C-c\C-b" 'mew-pgp-sign-encrypt-letter)
    (define-key (symbol-value symmap) "\C-c\C-r" 'mew-pgp-encrypt-sign-letter)
    (define-key (symbol-value symmap) "\C-c\C-p\C-a" 'mew-draft-toggle-privacy-always)
    (define-key (symbol-value symmap) "\C-c\C-p\C-e" 'mew-draft-toggle-privacy-encrypted)
    (define-key (symbol-value symmap) "\C-c\C-p\C-d" 'mew-draft-set-privacy-type)
    (define-key (symbol-value symmap) "\C-x\C-s" 'mew-save-buffer))
  (if mew-draft-body-map
      ()
    (setq mew-draft-body-map (make-sparse-keymap))
    (mew-set-keymap-parent mew-draft-body-map text-mode-map)
    (define-key mew-draft-body-map "\C-c\t"       'mew-draft-insert-signature)
    (define-key mew-draft-body-map "\C-c\C-y"     'mew-draft-cite)
    (define-key mew-draft-body-map "\C-c\C-t"     'mew-draft-yank)
    (define-key mew-draft-body-map "\C-c\C-f\C-f" 'mew-fib-fill-default)
    (define-key mew-draft-body-map "\C-c\C-f\C-k" 'mew-fib-delete-frame)
    (define-key mew-draft-body-map "\C-c\C-f\C-n" 'mew-fib-next-item)
    (define-key mew-draft-body-map "\C-c\C-f\C-p" 'mew-fib-previous-item)
    (define-key mew-draft-body-map "\C-c\C-f\C-z" 'mew-fib-flush-input)
    (mew-draft-share-keymap 'mew-draft-body-map))
  (mew-draft-share-keymap 'mew-draft-header-map)
  (if mew-draft-mode-map
      ()
    (setq mew-draft-mode-map (make-sparse-keymap))
    (set-keymap-parent mew-draft-mode-map mew-draft-body-map)))
 (t
  (defun mew-draft-keyswitch ()
    "A function to implement region key binding."
    (interactive)
    (let ((key (this-command-keys))
	  command func len (i 0))
      (if (and mew-xemacs-p (= (length key) 0))
	  (setq key (vector last-command-event)))
      (setq command (lookup-key (current-global-map) key))
      (if (numberp command)
	  (setq len command
		command (lookup-key (current-global-map)
				    (mew-subsequence key 0 len))
		key (mew-subsequence key len)))
      (setq len (length key))
      (if (or (eq command 'universal-argument) (eq command 'digit-argument))
	  (catch 'keyswitch
	    (while (and (or (eq command 'universal-argument)
			    (eq command 'digit-argument))
			(let ((tmp (aref key i)))
			  (if mew-xemacs-p (setq tmp (event-to-character tmp)))
			  (and (<= ?0 tmp) (>= ?9 tmp))))
	      (setq i (1+ i)))
	    (while (< i len)
	      (if (eq 'mew-draft-keyswitch
		      (key-binding (char-to-string (aref key i))))
		  (throw 'keyswitch (setq key (mew-subsequence key i))))
	      (setq i (1+ i)))))
      (cond
       ((mew-in-attach-p)
	(setq func (lookup-key mew-draft-attach-map key)))
       ((mew-in-header-p)
	(setq func (lookup-key mew-draft-header-map key)))
       (t 
	(setq func (lookup-key mew-draft-body-map key))))
      (if (not (integerp func))
	  ()
	(setq key (mew-subsequence key 0 func))
	(setq func (lookup-key (current-global-map) key))
	(cond
	 ((mew-in-attach-p)
	  (setq func (lookup-key mew-draft-attach-map key)))
	 ((mew-in-header-p)
	  (setq func (lookup-key mew-draft-header-map key)))
	 (t 
	  (setq func (lookup-key mew-draft-body-map key)))))
      (if func
	  ()
	(setq func (lookup-key (current-global-map) key))
	(if (not (integerp func))
	    ()
	  (setq key (mew-subsequence key 0 func))
	  (setq func (lookup-key (current-global-map) key))))
      (if func
	  (while (keymapp func)
	    (if (vectorp key)
		(setq key (vconcat key (read-event)))
	      (setq key (concat key (char-to-string (read-event)))))
	    (setq func (lookup-key (current-global-map) key))))
      (if (null func)
	  (insert key) ;; just in case
	(setq this-command func)
	(run-hooks 'pre-command-hook)
	(call-interactively this-command))))
  (if mew-draft-body-map
      ()
    (setq mew-draft-body-map (make-sparse-keymap))
    (define-key mew-draft-body-map "\C-c\t"   'mew-draft-insert-signature))
  (if mew-draft-mode-map
      ()
    (setq mew-draft-mode-map (make-sparse-keymap))
    (let ((begin ?\40) (end ?\177))
      (while (<= begin end)
	(define-key mew-draft-mode-map 
	  (char-to-string begin) 'mew-draft-keyswitch)
	(setq begin (1+ begin))))
    (define-key mew-draft-mode-map "\C-m"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-n"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-p"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-f"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-b"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\t"       'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\e\t"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-c\t"   'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-d"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-o"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-q"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-t"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-w"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-k"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\r"       'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\n"       'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-y"     'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-c\C-d" 'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-c\C-r" 'mew-draft-keyswitch)
    (define-key mew-draft-mode-map "\C-c\C-y" 'mew-draft-cite)
    (define-key mew-draft-mode-map "\C-c\C-t" 'mew-draft-yank)
    (define-key mew-draft-mode-map "\C-c\C-l" 'mew-draft-rehighlight)
    (define-key mew-draft-mode-map "\C-c\C-m" 'mew-draft-make-message)
    (define-key mew-draft-mode-map "\C-c\C-u" 'mew-draft-undo)
    (define-key mew-draft-mode-map "\C-c\C-c" 'mew-draft-send-letter)
    (define-key mew-draft-mode-map "\C-c\C-s" 'mew-pgp-sign-letter)
    (define-key mew-draft-mode-map "\C-c\C-e" 'mew-pgp-encrypt-letter)
    (define-key mew-draft-mode-map "\C-c\C-b" 'mew-pgp-sign-encrypt-letter)
    (define-key mew-draft-mode-map "\C-c\C-r" 'mew-pgp-encrypt-sign-letter)
    (define-key mew-draft-mode-map "\C-c\C-p\C-a" 'mew-draft-toggle-privacy-always)
    (define-key mew-draft-mode-map "\C-c\C-p\C-e" 'mew-draft-toggle-privacy-encrypted)
    (define-key mew-draft-mode-map "\C-c\C-p\C-d" 'mew-draft-set-privacy-type)
    (define-key mew-draft-mode-map "\C-c\C-q" 'mew-draft-kill)
    (define-key mew-draft-mode-map "\C-c\C-a" 'mew-draft-prepare-attachments)
    (define-key mew-draft-mode-map "\C-c\C-f\C-f" 'mew-fib-fill-default)
    (define-key mew-draft-mode-map "\C-c\C-f\C-k" 'mew-fib-delete-frame)
    (define-key mew-draft-mode-map "\C-c\C-f\C-n" 'mew-fib-next-item)
    (define-key mew-draft-mode-map "\C-c\C-f\C-p" 'mew-fib-previous-item)
    (define-key mew-draft-mode-map "\C-c\C-f\C-z" 'mew-fib-flush-input)
    (define-key mew-draft-mode-map "\C-c\C-o" 'mew-draft-insert-config)
    (define-key mew-draft-mode-map "\C-x\C-s" 'mew-save-buffer)
    )))

(if mew-xemacs-p
    ()
  (easy-menu-define
   mew-draft-mode-menu
   mew-draft-mode-map
   "Menu used in Draft mode."
   mew-draft-mode-menu-spec)
  (if mew-use-overlay-keymap
      (easy-menu-define
       mew-draft-header-menu
       mew-draft-header-map
       "Menu used in Draft mode."
       mew-draft-mode-menu-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft mode
;;;

(defvar mew-draft-encrypted-p nil)
(defvar mew-draft-privacy-error nil)
(defvar mew-draft-protect-privacy-type nil)

(defun mew-draft-mode (&optional encrypted)
  "\\<mew-draft-mode-map>
Mew Draft mode:: major mode for composing a MIME message.
Key actions are different in each region: Header, Body, and Attachment.

To send a draft, type \\[mew-draft-make-message] and \\[mew-draft-send-letter].  To make multipart, type
\\[mew-draft-prepare-attachments], edit attachments, type \\[mew-draft-make-message] and \\[mew-draft-send-letter].

*Whole buffer key assignment:

\\[mew-draft-make-message]	Make a MIME message. Charset guess, mapping directory structure 
	to multipart, and so on.
\\[mew-draft-send-letter]	Send this message. If you skipped '\\[mew-draft-make-message]', the header was
	modified and you are asked, \"The header was modified. 
	Send this message? \". Type y or n. Mew sends the message 
	in background. So, when you exit Emacs, you may be asked, 
	\"Active processes exist; kill them and exit anyway? (yes or no)\".
	In this case, check if *mew watch* buffer exist. If so, never 
	exit Emacs because Mew is still sending the message.
	If executed \\[universal-argument], send this message
	without killing the draft. This is convenient 
	to send messages to multiple people modifying the draft.

\\[mew-draft-prepare-attachments]	Prepare an attachment region in the bottom of the draft.
	To compose a multipart message, you should execute this 
	command first.

\\[mew-draft-insert-config]	Insert the Config: field with 'mew-config-guess-alist'
	in the header.
\\[mew-draft-rehighlight]  Highlight header and body again.

\\[mew-draft-undo]	Undo '\\[mew-draft-make-message]'.
\\[mew-draft-kill]	Kill this draft.

\\[mew-pgp-sign-letter]	Sign the entire draft with PGP. Input your passphrase.
\\[mew-pgp-encrypt-letter]	Encrypt the entire draft with PGP.
\\[mew-pgp-sign-encrypt-letter]	Sign then encrypt the entire draft with PGP.
	Input your passphrase.
\\[mew-pgp-encrypt-sign-letter]	Encrypt then sign the entire draft with PGP.
	Input your passphrase.

\\[mew-draft-toggle-privacy-always]	Toggle whether or not all drafts are protected.
\\[mew-draft-toggle-privacy-encrypted]	Toggle whether or not drafts replying to encrypted messages 
		are protected.
\\[mew-draft-set-privacy-type]	Set privacy service which will be effective when \\[mew-draft-make-message].
\\<mew-draft-header-map>
*Header region key assignment:

\\[mew-draft-header-comp]	Complete field keys.
	Complete and expand an address short name.
	Complete folder names.
\\[mew-draft-circular-comp]	Complete your mail domain.
\\[mew-draft-expand]	Replace an address with 'NAME <address>'.

*Body region key assignment:

\\<mew-draft-body-map>\\[mew-draft-insert-signature]	Insert '~/.signature' on the cursor point.
\\<mew-draft-mode-map>\\[mew-draft-cite]	Copy and paste a part of message from Message mode WITH
	citation prefix and label.
	1. Roughly speaking, it copies the body in Message mode. 
	   For example, if text/plain is displayed, the entire Message 
	   mode is copied. If message/rfc822 is displayed, the body 
	   without the header is copied.
	2. If called with '\\[universal-argument]', the header is also copied if exists.
	3. If an Emacs mark exists, the target is the region between 
	   the mark and the cursor.
\\[mew-draft-yank]	Copy and paste a part of message from Message mode WITHOUT
	citation prefix and label.

*Attachments region Key assignment:
\\<mew-draft-attach-map>
\\[mew-attach-forward]	Go to the first subdirectory.
\\[mew-attach-backforward]	Go to the parent directory.
\\[mew-attach-next]	Go to the next file in the current directory.
\\[mew-attach-previous]	Go to the previous file in the current directory.

\\[mew-attach-copy]	Copy a file (via networks) on '.'.
	To copy a remote file, use the '/[user@]hostname:/filepath' syntax.
\\[mew-attach-link]	Link a file with a symbolic link on '.'.
\\[mew-attach-delete]	Delete this file or this directory.
\\[mew-attach-multipart]	Create a subdirectory(i.e. multipart) on '.'.
\\[mew-attach-find-file]	Open this file into a buffer.
\\[mew-attach-find-new-file]	Open a new file into a buffer on '.'.
\\[mew-attach-external-body]	Input external-body on '.'.
\\[mew-attach-audio]	Sampling voice and insert as audio file on '.'.
\\[mew-attach-pgp-public-key]	Extract the PGP key for the inputed user on '.'.
\\[mew-attach-description]	Input a description(Content-Description:).
\\[mew-attach-disposition]	Change the file name(Content-Disposition:).
\\[mew-attach-type]	Change the data type(Content-Type:).
\\[mew-attach-charset]	Specify charset for a Text/* object.

\\[mew-attach-base64]	Put the 'B' mark to encode with Base64.
\\[mew-attach-quoted-printable]	Put the 'Q' mark to encode with Quoted-Printable.
\\[mew-attach-gzip64]	Put the 'G' mark to encode with Gzip64. This is applicable 
	only to Text/Plain and Application/Postscript since compression 
	is not effective other objects. For example, JPEG is already 
	compressed.
\\[mew-attach-pgp-sign]	Put the 'PS' mark to sign with PGP.
\\[mew-attach-pgp-enc]	Put the 'PE' mark to encrypt with PGP. 
	Input decryptors' addresses.
\\[mew-attach-undo]	Unmark. The original mark appears.

* Fill blanks
\\<mew-draft-mode-map>
Prepare '~/.mew-fib' like;

	name:  Kazuhiko Yamamoto
	email: Kazu@Mew.org

If you receive a message like;

	Your name : |>name<|
	Your e-mail address: |>email<|

Type \\<mew-summary-mode-map>\\[mew-summary-reply] in Summary mode to enter Draft mode. 
Then type \\<mew-draft-mode-map>\\[mew-draft-yank], \\[mew-fib-fill-default], and \\[mew-fib-delete-frame] makes following
draft.

	Your name : Kazuhiko Yamamoto
	Your e-mail address: Kazu@Mew.org

In this way, mew-fil fills up items quoted like |> <| from '~/.mew-fib'.
The fill functions described below.

\\[mew-fib-fill-default]	Fill |>item<| from '~/.mew-fib'.
\\[mew-fib-delete-frame]	Delete all quotations, i.e. |> <|.
\\[mew-fib-next-item]	Jump to the next fib item.
\\[mew-fib-previous-item]	Jump to the previous fib item.
\\[mew-fib-flush-input]	Flush input from '~/.mew-fib'.

Moreover, '~/.mew-fib' supports aliases like;

	email: Kazu@Mew.org
	e-mail:

"
  (interactive)
  (auto-save-mode mew-draft-mode-auto-save)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
        (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mew-header-separator)
  (setq major-mode 'mew-draft-mode)
  (use-local-map mew-draft-mode-map)
  (set-syntax-table mew-draft-mode-syntax-table)
  (setq mew-cache-message-number (file-name-nondirectory (buffer-file-name)))
  (cd (expand-file-name mew-home))
  (if mew-require-final-newline
      (progn
	(make-local-variable 'require-final-newline)
	(setq require-final-newline t)))
  (if mew-xemacs-p
      (progn
	(set-buffer-menubar current-menubar)
	(add-submenu nil mew-draft-mode-menu-spec)))
  (mew-draft-toolbar-update)
  (make-local-variable 'mew-draft-encrypted-p)
  (setq mew-draft-encrypted-p encrypted)
  (make-local-variable 'mew-draft-privacy-error)
  (setq mew-draft-privacy-error nil)
  (make-local-variable 'mew-draft-protect-privacy-type)
  (setq mew-draft-protect-privacy-type nil)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (draft-mode-name)
  (run-hooks 'text-mode-hook 'mew-draft-mode-hook)
  ;; must be here for auto-fill
  (if (and auto-fill-function mew-temacs-p)
      (progn
	(make-local-variable 'auto-fill-function)
	(setq auto-fill-function (function mew-draft-auto-fill))))
  (setq buffer-undo-list nil))

(defun draft-mode-name ()
  (let (sub)
    (cond
     (mew-draft-protect-privacy-type
      (setq sub (nth 2 (assoc mew-draft-protect-privacy-type
			      mew-privacy-database))))
     ((and mew-draft-encrypted-p mew-protect-privacy-encrypted)
      (setq sub (nth 2 (assoc mew-protect-privacy-encrypted-type
			      mew-privacy-database))))
     (mew-protect-privacy-always
      (setq sub (nth 2 (assoc mew-protect-privacy-always-type
			      mew-privacy-database)))))
    (if sub
	(setq mode-name (concat "Draft " sub))
      (setq mode-name "Draft"))
    (force-mode-line-update)))

(defun mew-draft-auto-fill ()
  (do-auto-fill)
  (if (mew-in-header-p)
      (save-excursion
	(beginning-of-line)
	(while (not (or (looking-at "[^ \t]+:\\|[ \t]") (bobp)))
	  (insert "\t")
	  (forward-line -1)
	  (beginning-of-line)))))

(defun mew-draft-rename (file)
  (if (string-match
       (format "^%s\\(.*\\)$" (file-name-as-directory mew-mail-path))
       file)
      (rename-buffer (concat "+" (mew-match 1 file)))))

;; +draft/1 -> +draft/mime/1
;; This function is hard coding due to mew-draft-mime-folder, sigh...
(defun mew-draft-to-mime (draft)
  (concat (file-name-as-directory mew-draft-mime-folder)
	  (file-name-nondirectory draft)))

(defun mew-attachdir (&optional draft)
  (mew-expand-folder (mew-draft-to-mime (or draft (buffer-name)))))

(defun mew-draft-header-insert-alist (halist)
  "Insert field-body: and field-value. Return the value of
the Body: field."
  (let ((case-fold-search t)
	key val ret)
    (while halist
      (setq key (car (car halist)))
      (setq val (cdr (car halist)))
      (setq halist (cdr halist))
      (if (not (string-match ":$" key))
	  (setq key (concat key ":")))
      (if (string-match mew-body: key)
	  (setq ret val)
	(mew-draft-header-insert key val)))
    ret))

(defun mew-insert-address-list (field adrs del force-insert)
  (let ((cnt 0) (beg (point)) med adr)
    (while adrs
      (setq adr (car adrs) adrs (cdr adrs))
      (if (mew-is-my-address del adr)
	  ()
	(if (equal cnt 0)
	    (insert adr)
	  (insert ", " adr))
	(setq del (cons (concat "^" (regexp-quote adr) "$") del))
	(setq cnt (1+ cnt))))
    (if (or force-insert (> cnt 0))
	(progn
	  (beginning-of-line)
	  (insert field " ")
	  (setq med (point))
	  (end-of-line)
	  (insert "\n")
	  (mew-header-fold-region beg (point) med 'use-tab)))
    del))

(defun mew-insert-address-list2 (field adrs)
  (if (null adrs)
      ()
    (let ((beg (point)) med)
      (insert field " ")
      (setq med (point))
      (insert (car adrs))
      (setq adrs (cdr adrs))
      (while adrs
	(insert ", " (car adrs))
	(setq adrs (cdr adrs)))
      (insert "\n")
      (mew-header-fold-region beg (point) med 'use-tab))))

(defun mew-draft-header (&optional subject nl to cc newsgroups in-reply-to references other-headers fromme)
;; to -- string or list
;; cc -- string or list
;; nl -- one empty line under "----", which is necessary if
;;      attachment is prepared
  (let ((del (mew-get-my-address-regex-list)) ;; deleting list for Cc:
	body)
    (goto-char (point-min))
    ;; Insert To: first.
    ;; All addresses inserted on To: are appended to del.
    (cond
     ((null to) (insert mew-to: " \n"))
     ((stringp to) ;; To: inputed from the mini-buffer.
      ;; Don't check to is mine. Cc: is also string
      ;; We believe that user never specifies the same address of To: to Cc:.
      (insert mew-to: " " to "\n"))
     ;; To: collected by reply
     ((listp to)
      (if fromme
	  (mew-insert-address-list2 mew-to: to)
	(setq del (mew-insert-address-list mew-to: to del t)))))
    (cond
     ((null cc) ()) ;; do nothing 
     ((stringp cc) ;; Cc: inputed from the mini-buffer.
      (insert mew-cc: " " cc "\n"))
     ((listp cc) ;; Cc: collected by reply.
      (if fromme
	  (mew-insert-address-list2 mew-cc: cc)
	(mew-insert-address-list mew-cc: cc del nil))))
    (mew-draft-header-insert mew-newsgroups:  newsgroups)
    (mew-draft-header-insert mew-cc:          mew-cc)
    (mew-draft-header-insert mew-subj:        (or subject ""))
    (mew-draft-header-insert mew-from:        mew-from)
    (mew-draft-header-insert mew-fcc:         mew-fcc)
    (mew-draft-header-insert mew-dcc:         mew-dcc)
    (mew-draft-header-insert mew-reply-to:    mew-reply-to)
    (mew-draft-header-insert mew-in-reply-to: in-reply-to)
    (mew-draft-header-insert mew-references:  references)
    (if (and mew-x-face-file
	     (file-exists-p (expand-file-name mew-x-face-file)))
	(let ((xface))
	  (save-excursion
	    (mew-set-buffer-tmp)
	    (insert-file-contents (expand-file-name mew-x-face-file))
	    (setq xface (mew-buffer-substring (point-min)
					      (max (buffer-size) 1))))
	  (mew-draft-header-insert mew-x-face: xface)))
    (mew-draft-header-insert mew-x-mailer: mew-x-mailer)
    (setq body (mew-draft-header-insert-alist other-headers))
    (mew-draft-header-insert-alist mew-header-alist)
    (if (and mew-use-config-imget-for-draft
	     (not (string-equal mew-config-imget mew-config-default)))
	(mew-draft-header-insert mew-config: mew-config-imget))
    (mew-header-prepared)
    ;; on the body
    (if nl (insert "\n"))
    (if body (save-excursion (insert body)))
    ;; move the cursor after "To: "
    (goto-char (point-min))
    (forward-char 4))) ;; Don't use (end-of-line) since the value may exist.

(defun mew-draft-send-letter (&optional preserve)
  "Send this message. If you skipped 'C-cC-m', the header was
modified and you are asked, \"The header was modified.
Send this message? \". Type y or n. Mew sends the message 
in background. So, when you exit Emacs, you may be asked, 
\"Active processes exist; kill them and exit anyway? (yes or no)\".
In this case, check if *mew watch* buffer exists. If so, never 
exit Emacs because Mew is still sending the message.
If executed \\[universal-argument], send this message
without killing the draft. This is convenient 
to send messages to multiple people modifying the draft."
  (interactive "P")
  (run-hooks 'mew-send-hook)
  (if (mew-header-p)
      (condition-case nil
	  (progn
	    (mew-draft-make-message)
	    (if (or (not mew-ask-send)
		    (y-or-n-p (format "The header was modified. Send this message? ")))
		(mew-draft-real-send-letter preserve)
	      (mew-draft-undo)))
	(quit
	 (mew-draft-undo)))
    (mew-draft-real-send-letter preserve)))

(defun mew-draft-real-send-letter (&optional preserve)
  (let ((attachdir (mew-attachdir))
	(msg (file-name-nondirectory (buffer-file-name)))
	(process-connection-type mew-connection-type1)
	keep config unknown)
    (run-hooks 'mew-real-send-hook)
    (set-buffer-modified-p t) ;; ensure to save
    (mew-frwlet
     mew-cs-dummy mew-cs-mime-trans
     (save-buffer))
    (setq config (mew-header-get-value mew-config:))
    (and config (setq config (mew-split config ?,)))
    (while config
      (if (not (member (car config) mew-config-list))
	  (if unknown
	      (setq unknown (concat unknown "," (car config)))
	    (setq unknown (car config))))
      (setq config (cdr config)))
    (if (and unknown
	     (not (y-or-n-p (format "Unknown Config: selector '%s'. Send this message anyway? " unknown))))
	(message
	 (substitute-command-keys
	  "To send this message, edit Config: then type '\\<mew-draft-mode-map>\\[mew-draft-send-letter]'"))
      ;; learning short names
      (if (and mew-use-auto-alias mew-addrbook-append-domain-p)
	  ;; If mew-addrbook-append-domain-p is nil, automatic
	  ;; short names would be conflicted to local users.
	  (mapcar (function mew-addrbook-alias-add)
		  (mew-header-parse-address-list (list mew-to: mew-cc:))))
      ;; Fcc: vs folders
      (let ((folders (mew-header-get-value mew-fcc:)) folder)
	(if (null folders)
	    ()
	  (setq folders (mew-addrstr-parse-value-list2 folders))
	  (while folders
	    (setq folder (car folders))
	    (if (or (mew-folder-mailp folder)
		    (mew-folder-local-newsp folder)
		    (mew-folder-imapp folder)
		    (file-name-absolute-p folder))
		()
	      (setq folder (concat "+" folder)))
	    (mew-folder-check folder 'force-to-create)
	    (setq folders (cdr folders)))))
      ;;
      (if preserve
	  ;; leave the draft
	  (setq keep "--preserve=on")
	(setq keep "--preserve=off")
	(mew-overlay-delete-buffer)
	(kill-buffer (current-buffer))
	;;
	(if (mew-current-get 'window)
	    (progn
	      (set-window-configuration (mew-current-get 'window))
	      (mew-current-set 'window nil))))
      (set-buffer (generate-new-buffer mew-buffer-watch))
      ;; watch buffer
      (setq mew-watch-buffer-process
	    (mew-im-start-process mew-prog-imput
				  "Send"
				  "-draftfolder" mew-draft-folder
				  "-draftmessage" msg
				  keep
				  "-watch" "-verbose"))
      (mew-set-process-cs mew-watch-buffer-process
			  mew-cs-autoconv mew-cs-mime-trans)
      (set-process-sentinel mew-watch-buffer-process 'mew-watch-sentinel)
      (message "Sending a message in background")
      ;; keep +draft/mime/X alive if "C-uC-cC-c".
      (or preserve (mew-delete-directory-recursively attachdir)))))

(defun mew-watch-sentinel (process event)
  (let ((cbuf (current-buffer)) (kbuf (process-buffer process)))
    (set-buffer kbuf)
    (goto-char (point-min))
    (if (null (re-search-forward (format "^%s: ERROR:" mew-prog-imput) nil t))
	(progn
	  (set-buffer cbuf)  ;; to avoid cursor-in-echo-area bug
	  (kill-buffer kbuf)) ;; set-buffer before kill-buffer
      (ding)
      (message "Send failed")
      (beginning-of-line)
      (switch-to-buffer (process-buffer process))
      (local-set-key "\C-c\C-q" 'mew-kill-buffer))))

;;
;; Citation
;;

(defun mew-draft-auto-set-input-method ()
  (if (and (fboundp 'activate-input-method)
	   mew-charset-input-method-alist)
      (let* ((charset (mew-charset-guess-region
		       (mew-header-end) (or (mew-attach-begin) (point-max))))
	     (method (if (stringp charset)
			 (cdr (mew-assoc-case-equal
			       charset mew-charset-input-method-alist 0)))))
	(if (stringp method)
	    (progn
	      (activate-input-method method)
	      (message "Set input method to %s" method))))))

(defun mew-draft-yank (&optional arg force)
  "Copy and paste a part of message from Message mode WITHOUT
citation prefix and label.
1. Roughly speaking, it copies the body in Message mode. For example,
   if text/plain is displayed, the entire Message mode is copied.
   If message/rfc822 is displayed, the body without the header is copied.
2. If called with '\\[universal-argument]', the header is also copied if exists.
3. If an Emacs mark exists, the target is the region between the mark and 
   the cursor."
;; MUST take care of C-x C-x
;; MUST be able to cancel by C-x u
  (interactive "P")
  (if (and (not force) (or (mew-in-header-p) (mew-in-attach-p)))
      (message "You cannot cite a message here.")
    (let (cite beg end)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(save-restriction
	  (widen)
	  (cond
	   (arg 
	    (setq beg (point-min) end (point-max)))
	   ((mew-mark) 
	    (setq beg (region-beginning) end (region-end)))
	   ((mew-header-p)
	    ;; header exists in Message mode
	    (mew-header-goto-body)
	    (setq beg (point) end (point-max)))
	   (t
	    (setq beg (point-min) end (point-max))))
	  (setq cite (mew-buffer-substring beg end))))
      (push-mark (point) t t) ;; for C-x C-x
      (insert cite)
      (mew-draft-auto-set-input-method))))

(defvar mew-message-citation-buffer nil
  "This value is used by mew-gnus.el to specify a buffer from where
you can cite.")

(defun mew-draft-cite (&optional arg force)
  "Copy and paste a part of message from Message mode WITH
citation prefix and label.
1. Roughly speaking, it copies the body in Message mode. For example,
   if text/plain is displayed, the entire Message mode is copied.
   If message/rfc822 is displayed, the body without the header is copied.
2. If called with '\\[universal-argument]', the header is also copied if exists.
3. If an Emacs mark exists, the target is the region between the mark and 
   the cursor."
;; MUST take care of C-x C-x
;; MUST be able to cancel by C-x u
  (interactive "P")
  (if (and (not force) (or (mew-in-header-p) (mew-in-attach-p)))
      (message "You cannot cite a message here.")
    (let ((nonmewbuf mew-message-citation-buffer) ;; buffer local, so copy here
	  cite beg end ref-msgid tbuf)
      (save-excursion
	;;
	;; extract the body without header
	;;
	(setq tbuf (or nonmewbuf (mew-buffer-message)))
	(if tbuf
	    (set-buffer tbuf)
	  (error "No buffer to be cited."))
	(save-restriction
	  ;; first prepare "cite"
	  (widen)
	  (cond
	   ;; arg will be effect in mew-cite-original
	   ((mew-mark) 
	    (setq beg (region-beginning) end (region-end)))
	   ((mew-header-p)
	    ;; header exists in Message mode. Skip the header
	    ;; because we will concatenate it to cite later.
	    (mew-header-goto-body)
	    (setq beg (point) end (point-max)))
	   (t
	    (setq beg (point-min) end (point-max))))
	  (setq cite (mew-buffer-substring beg end)))
	;; concat the header
	;; see also mew-summary-reply
	(setq tbuf (or nonmewbuf
		       (save-excursion
			 (set-buffer (mew-buffer-message))
			 (if (mew-header-p) (current-buffer)))
		       ;; header exists only in cache if multipart
		       (mew-cache-hit (mew-current-get 'message))))
	(if tbuf
	    (set-buffer tbuf)
	  (error "No buffer to be cited."))
	(save-restriction
	  (widen)
	  (mew-header-goto-end)
	  (setq cite (concat (mew-buffer-substring (point-min) (point)) 
			     "\n" cite))
          (setq ref-msgid (mew-header-get-value mew-message-id:))))
      ;; 
      ;; Draft mode, insert the header and the body.
      ;;

      ;; append message-id to references
      (if (and ref-msgid (mew-header-p))
          (save-excursion
            (let ((ref (mew-header-get-value mew-references:))
                  (refl nil) rb)
              (if (not ref)
                  ()
                (setq rb ref)
                (while (string-match "<[^>]+>" rb)
                  (setq refl (cons (mew-match 0 rb) refl)) 
                  (setq rb (substring rb (match-end 0))))
                (if (member ref-msgid refl)
                    (setq ref-msgid nil)))
              (if (null ref-msgid)
                  ()
                (setq ref (concat ref (if ref "\n\t") ref-msgid))
                (mew-header-delete-lines (list mew-references:))
                (if (null refl)
                    (goto-char (mew-header-end)))
                (mew-draft-header-insert mew-references: ref)))))
      (save-restriction
	;; this gets complicated due to supercite, please don't care
	(narrow-to-region (point)(point)) ;; for (goto-char (point-min))
	(insert cite)
	(push-mark (point) t t)
	(goto-char (point-min)))
      (cond
       (mew-cite-hook
	(run-hooks 'mew-cite-hook))
       (t (mew-cite-original arg)))
      (mew-draft-auto-set-input-method)
      (or force (mew-draft-rehighlight)))))

(defun mew-cite-original (&optional arg)
  (if (< (marker-position (mark-marker)) (point))
      (exchange-point-and-mark))
  (let ((beg (point)) (end (marker-position (mark-marker)))
        label prefix)
    (save-restriction
      (narrow-to-region beg end)
      (condition-case nil
          (setq label (mew-cite-strings))
        (error
	 (error "Syntax of mew-cite-format was changed. Read explanation of mew-cite-fields")))
      (if (null mew-cite-prefix-function)
          (setq prefix mew-cite-prefix)
        (setq prefix (funcall mew-cite-prefix-function)))
      (if mew-cite-prefix-confirmp
          (let ((ask (read-string 
                      (format "Prefix (\"%s\"): " prefix) "")))
            (if (not (string= ask "")) (setq prefix ask))))
      ;; C-u C-c C-y cites body with header.
      (if (eq arg nil) 
	  ;; header has been already cited. So, delete it.
	  (delete-region beg (progn (mew-header-goto-body) (point))))
      (insert label)
      (push-mark (point) t t) ;; for C-x C-x
      (and (bolp) (insert prefix))
      (while (equal 0 (forward-line))
	(or (equal (point) (point-max))
	    (insert prefix))))))

(defun mew-cite-get-value (field)
  (let ((value (mew-header-get-value field))
	repl func)
    (if (and (string= mew-from: field) value
	     (setq func (mew-addrbook-func mew-addrbook-for-cite-label)))
	(progn
	  (setq repl (funcall func (mew-addrstr-parse-address value)))
	  (if repl (setq value repl))))
    (or value "")))

(defun mew-cite-strings ()
  "A function to create cite label according to 
'mew-cite-format' and 'mew-cite-fields'."
  (if (null mew-cite-fields)
      ""
    (apply (function format)
	   mew-cite-format
	   (mapcar (function mew-cite-get-value) mew-cite-fields))))

(defun mew-cite-prefix-username ()
  "A good candidate for mew-cite-prefix-function.
The citation style is 'from_address> ', e.g. 'kazu> '"
  (let* ((from (mew-header-parse-address mew-from:))
	 (user (mew-addrstr-extract-user from))
	 (func (mew-addrbook-func mew-addrbook-for-cite-prefix))
	 nickname prefix)
    (if func (setq nickname (funcall func from)))
    (setq prefix (or nickname user))
    (if mew-ask-cite-prefix
	(setq prefix (read-string "Citation prefix: " prefix)))
    (concat prefix mew-cite-prefix)))

;;
;;
;;

(defun mew-draft-kill ()
  "Kill this draft."
  (interactive)
  (if (y-or-n-p "Kill draft message? ")
      (let ((attachdir (mew-attachdir)) ;; attachdir must be here
	    (file (buffer-file-name))
	    (buf (current-buffer)))
	(mew-overlay-delete-buffer)
	(save-buffer)
	(kill-buffer buf)
	(if (file-exists-p file) (delete-file file))
	(if (mew-current-get 'window)
	    (progn
	      (set-window-configuration (mew-current-get 'window))
	      (mew-current-set 'window nil)))
	(mew-delete-directory-recursively attachdir)
	(message "Draft was killed"))
    (message "Draft was not killed")))

(defun mew-draft-insert-config (&optional nohighlight)
  "Insert the Config: header."
  (interactive)
  (let* ((config-cur (mew-header-get-value mew-config:))
	 (config-gus (mew-refile-guess-by-alist1 mew-config-guess-alist))
	 (config-new (if config-gus (mew-join "," config-gus))))
    (if (and mew-ask-config (not (interactive-p)))
	(setq config-new (mew-input-config config-new)))
    (if (and (interactive-p) (not config-new))
	(setq config-new ""))
    (if config-new
	(if (and config-cur
		 (or (string= config-cur config-new)
		     (not
		      (y-or-n-p
		       (format "Do you want to replace Config value with %s? "
			       config-new)))))
	    ()
	  (cond
	   (nohighlight
	    (save-excursion
	      (mew-header-delete-lines (list mew-config:))
	      (goto-char (mew-header-end))
	      (mew-draft-header-insert mew-config: config-new)))
	   (t
	    (widen)
	    (push-mark (point) t t) ;; for C-x C-x
	    (mew-header-delete-lines (list mew-config:))
	    (goto-char (mew-header-end))
	    (mew-draft-header-insert mew-config: config-new)
	    (forward-line -1)
	    (end-of-line)
	    (mew-draft-rehighlight)))))))

(defun mew-draft-insert-signature ()
  "Insert the signature file specified by mew-signature-file.
If attachments exist and mew-signature-as-lastpart is *non-nil*,
the file is attached to the last part. Otherwise, the file is 
inserted into the body. If mew-signature-insert-last is *non-nil*,
the file is inserted to the end of the body. Otherwise, inserted
the cursor position."
  (interactive)
  (let ((sigfile (expand-file-name mew-signature-file)))
    (if (not (file-exists-p sigfile))
	(message "No signature file %s" sigfile)
      (if (and (mew-attach-p) mew-signature-as-lastpart)
	  (progn
	    (goto-char (point-max))
	    (forward-line -2)
	    (mew-attach-forward)
	    (mew-attach-copy sigfile "Signature")
	    (mew-attach-disposition "") ;; nil is NG.
	    (mew-attach-description mew-signature-description))
	(if mew-signature-insert-last 
	    (progn
	      (if (null (mew-attach-p))
		  (goto-char (point-max))
		(goto-char (1- (mew-attach-begin))))
	      (end-of-line)
	      (if (null (bolp)) (insert "\n"))))
	(insert-file-contents sigfile)))))

;;
;;
;;

(defun mew-save-buffer ()
  "Save this buffer with the mew-cs-draft coding-system"
  (interactive)
  (mew-frwlet
   mew-cs-dummy mew-cs-draft
   (save-buffer)))

(defun mew-draft-rehighlight ()
  "Highlight header and body again."
  (interactive)
  (mew-highlight-header)
  (mew-draft-header-keymap)
  (mew-highlight-body))

;;
;; Privacy
;;

(defun mew-draft-toggle-privacy-always ()
  "Toggle whether or not all drafts are protected."
  (interactive)
  (setq mew-protect-privacy-always (not mew-protect-privacy-always))
  (message "Set mew-protect-privacy-always to %s"
	   mew-protect-privacy-always)
  (draft-mode-name))

(defun mew-draft-toggle-privacy-encrypted ()
  "Toggle whether or not drafts replying to encrypted messages are 
protected."
  (interactive)
  (setq mew-protect-privacy-encrypted (not mew-protect-privacy-encrypted))
  (message "Set mew-protect-privacy-encrypted to %s"
	   mew-protect-privacy-encrypted)
  (draft-mode-name))

(defun mew-draft-set-privacy-type ()
  "\\<mew-draft-mode-map>
Set privacy service which will be effective when \\[mew-draft-make-message]."
  (interactive)
  (let ((alist (mapcar (function (lambda (x)
				   (cons (symbol-name (car x))(car x))))
		       mew-privacy-database))
	str)
    
    (setq str (completing-read "Input privacy services : " alist nil t))
    (if (stringp str)
	(setq mew-draft-protect-privacy-type
	      (cdr (assoc str alist)))))
  (draft-mode-name))

(provide 'mew-draft)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-draft.el ends here
