;;; mew-vars.el --- Variables and Constants for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 22, 1997
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-vars-version "mew-vars.el version 0.05")

(require 'mew-env)

;;;
;;; Global Constants
;;;

;; Must be capitalized!

(defconst mew-subj: "Subject:")
(defconst mew-to:   "To:")
(defconst mew-cc:   "Cc:")
(defconst mew-from: "From:")
(defconst mew-date: "Date:")

(defconst mew-resent-to:   "Resent-To:")
(defconst mew-resent-cc:   "Resent-Cc:")
(defconst mew-resent-from: "Resent-From:")
(defconst mew-resent-date: "Resent-Date:")

(defconst mew-reply-to:    "Reply-To:")
(defconst mew-newsgroups:  "Newsgroups:")
(defconst mew-followup-to: "Followup-To:")

(defconst mew-bcc:  "Bcc:")
(defconst mew-dcc:  "Dcc:")
(defconst mew-fcc:  "Fcc:")

(defconst mew-resent-bcc:   "Resent-Bcc:")
(defconst mew-resent-dcc:   "Resent-Dcc:")

(defconst mew-message-id: "Message-Id:")
(defconst mew-references:  "References:")
(defconst mew-in-reply-to: "In-Reply-To:")

(defconst mew-apparently-to: "Apparently-To:")
(defconst mew-destination:-list
  (list mew-to: mew-cc: mew-bcc: mew-apparently-to:))

(defconst mew-received: "Received:")

(defconst mew-x-mailer: "X-Mailer:")
(defconst mew-x-face:   "X-Face:")
(defconst mew-x-mew:    "X-Mew:")

(defconst mew-keywords: "Keywords:")
(defconst mew-config:   "Config:")
(defconst mew-body:	"Body:")

(defconst mew-mv:     "Mime-Version:")
(defconst mew-mv:-num "1.0")

(defconst mew-ct:  "Content-Type:")
(defconst mew-cte: "Content-Transfer-Encoding:")
(defconst mew-cd:  "Content-Description:")
(defconst mew-cid: "Content-ID:")
(defconst mew-cdp: "Content-Disposition:")

(defconst mew-ct-txt "Text/Plain")
(defconst mew-ct-msg "Message/Rfc822")
(defconst mew-ct-ext "Message/External-Body")
(defconst mew-ct-sts "Message/Delivery-Status")
(defconst mew-ct-mlm "Multipart/Mixed")
(defconst mew-ct-mls "Multipart/Signed")
(defconst mew-ct-mle "Multipart/Encrypted")
(defconst mew-ct-mld "Multipart/Digest")
(defconst mew-ct-ado "Audio/Basic")
(defconst mew-ct-apo "Application/Octet-Stream")
(defconst mew-ct-pgs "application/pgp-signature") ;; due to the RFC 1847 bug
(defconst mew-ct-pge "application/pgp-encrypted") ;; due to the RFC 1847 bug
(defconst mew-ct-apk "Application/Pgp-Keys")

(defconst mew-us-ascii "us-ascii")

(defconst mew-type-txt (list mew-ct-txt (list "charset" mew-us-ascii)))
(defconst mew-type-msg (list mew-ct-msg))
(defconst mew-type-mlm (list mew-ct-mlm))
(defconst mew-type-ado (list mew-ct-ado))
(defconst mew-type-apo (list mew-ct-apo))
(defconst mew-type-apk (list mew-ct-apk))

(defconst mew-b64  "base64")
(defconst mew-qp   "quoted-printable")
(defconst mew-xg   "x-gzip64")
(defconst mew-7bit "7bit")
(defconst mew-8bit "8bit")
(defconst mew-bin  "binary")

(defconst mew-decode-value 
  (list mew-b64 mew-qp mew-xg mew-7bit mew-8bit mew-bin))
(defconst mew-decode-composite-value
  (list mew-7bit mew-8bit mew-bin))

;; must be list of list.
(defconst mew-mime-fields
  (list (list mew-ct:  0 'analyze)
	(list mew-cte: 1 'extract)
	(list mew-cd:  2 'decode)
	(list mew-cid: 3 nil)
	(list mew-cdp: 4 'analyze)))

;; strictly, content-* are necessary but they are not practical.
(defvar mew-field-database
  (list (list mew-to:            'mailbox    'struct)
	(list mew-cc:            'mailbox    'struct)
	(list mew-from:          'mailbox    'struct)
	(list mew-apparently-to: 'mailbox    'struct)
	(list mew-bcc:           'mailbox    'struct)
	(list mew-dcc:           'mailbox    'struct)
	(list mew-reply-to:      'mailbox    'struct)
	(list mew-resent-to:     'mailbox    'struct)
	(list mew-resent-cc:     'mailbox    'struct)
	(list mew-resent-bcc:    'mailbox    'struct)
	(list mew-resent-dcc:    'mailbox    'struct)
	(list mew-resent-from:   'mailbox    'struct)
	(list mew-mv:            'mime       'struct)
	(list mew-subj:          'text       'text)
	(list mew-keywords:      'comma-text 'text)
	(list mew-received:      'unstruct   'unstruct)
	(list mew-references:    'unstruct   'unstruct)
	(list mew-x-face:	 'unstruct   'unstruct))
  "(field enc dec)")

(defmacro mew-field-type-for-encoding (key)
  (` (or (nth 1 (assoc (capitalize (, key)) mew-field-database)) 'unstruct)))

(defmacro mew-field-type-for-decoding (key)
  (` (or (nth 2 (assoc (capitalize (, key)) mew-field-database)) 'text)))

(defconst mew-syntax-magic
  (+ (length [key beg end pri]) (length mew-mime-fields)))

(defconst mew-x-mailer
  (concat mew-version " on "
          (if (string-match "XEmacs" emacs-version) "XEmacs" "Emacs")
          (format " %d.%d " emacs-major-version emacs-minor-version)
          (and (boundp 'xemacs-codename) (concat "(" xemacs-codename ")"))
          (and (boundp 'mule-version) (concat "/ Mule " mule-version)))
  "*A value inserted into X-Mailer: field in Draft mode if *non-nil*.")

(defconst mew-buffer-message     "*Mew message*")
(defconst mew-buffer-mime       " *mew mime*")
(defconst mew-buffer-cache      " *mew cache*")
(defconst mew-buffer-watch       "*Mew watch*")
(defconst mew-buffer-tmp        " *mew tmp*")
(defconst mew-buffer-burst      " *mew burst*")
(defconst mew-buffer-completions "*Mew completions*")
(defconst mew-buffer-ext        " *mew ext*")
(defconst mew-buffer-hello      " *mew hello*")
(defconst mew-buffer-pgpkey      "*Mew PGP Key*")
(defconst mew-buffer-addrbook    "*Mew Addrbook*")

(defconst mew-buffer-list
  (list mew-buffer-mime mew-buffer-watch mew-buffer-tmp mew-buffer-burst
	mew-buffer-completions mew-buffer-ext mew-buffer-hello
	mew-buffer-pgpkey mew-buffer-addrbook))

(defconst mew-home (file-name-as-directory "~"))

(defconst mew-ext-suffix       ".ext")
(defconst mew-text-suffix      ".txt")
(defconst mew-audio-suffix     ".au")
(defconst mew-pgp-key-suffix   ".pka")
(defconst mew-pgp-ascii-suffix ".asc")

;;;
;;; Buffer local variables
;;;

(defvar mew-summary-buffer-start-point nil)
(defvar mew-summary-buffer-process nil)
(defvar mew-summary-buffer-string nil)
(defvar mew-summary-buffer-config nil)
(defvar mew-summary-buffer-begin nil)
(defvar mew-summary-buffer-error nil)
(defvar mew-summary-buffer-wrong-pws nil)
(defvar mew-summary-buffer-reviews nil)
(defvar mew-summary-buffer-direction nil)
(defvar mew-summary-buffer-folder-cache-time nil)
(defvar mew-summary-buffer-refile nil)
(defvar mew-summary-buffer-disp-msg t)
(defvar mew-summary-buffer-left-msgs "-")
(defvar mew-watch-buffer-process nil)
(defvar mew-cache-attribute nil)
(defvar mew-cache-folder nil)
(defvar mew-cache-message-number nil)
(defvar mew-syntax-icon-spec nil)
(defvar mew-syntax-multi-form nil)
(defvar mew-syntax-privacy-result nil)
(defvar mew-decode-syntax nil)
(defvar mew-decode-error nil)
(defvar mew-decode-not-decrypted nil)
(defvar mew-encode-syntax nil)
(defvar mew-overlay-cursor-line nil)
(defvar mew-complete-window-config nil)

(mapcar (function make-variable-buffer-local)
	(list 'mew-summary-buffer-start-point
	      'mew-summary-buffer-process
	      'mew-summary-buffer-string
	      'mew-summary-buffer-config
	      'mew-summary-buffer-begin
	      'mew-summary-buffer-error
	      'mew-summary-buffer-wrong-pws
	      'mew-summary-buffer-reviews
	      'mew-summary-buffer-direction
	      'mew-summary-buffer-folder-cache-time
	      'mew-summary-buffer-refile
	      'mew-summary-buffer-disp-msg
	      'mew-summary-buffer-left-msgs
	      'mew-watch-buffer-process
	      'mew-cache-attribute
	      'mew-cache-folder
	      'mew-cache-message-number
	      'mew-syntax-icon-spec
	      'mew-syntax-multi-form
	      'mew-syntax-privacy-result
	      'mew-encode-syntax
	      'mew-decode-syntax
	      'mew-decode-error
	      'mew-decode-not-decrypted
	      'mew-overlay-cursor-line
	      'mew-complete-window-config))

;;;
;;; Global Variables
;;;

;;
;; Hooks
;;

(defvar mew-env-hook nil
  "*Hook called at initialize time before setting environment.")
(defvar mew-init-hook nil
  "*Hook called at initialize time.")
(defvar mew-summary-mode-hook nil
  "*Hook called in Summary mode.")
(defvar mew-virtual-mode-hook nil
  "*Hook called in Virtual mode.")
(defvar mew-draft-mode-hook nil
  "*Hook called in Draft mode.")
(defvar mew-draft-mode-newdraft-hook nil
  "*Hook called in Draft mode only when new draft is prepared.")
(defvar mew-draft-mode-reedit-hook nil
  "*Hook called in Draft mode only when an existing draft is used as draft.")
(defvar mew-message-mode-hook nil
  "*Hook called in Message mode.")
(defvar mew-message-hook nil
  "*Hook called whenever message displayed.")
(defvar mew-make-message-hook nil
  "*Hook called before making a message in Draft mode. A good example
is as follows:
  (add-hook 'mew-make-message-hook 'ispell-message)")
(defvar mew-send-hook nil
  "*Hook called before sending a message in Draft mode.
This hook is meaningless now and is remained for backward-compatibility.")
(defvar mew-real-send-hook nil
  "*Hook called before really sending a message in Draft mode")
(defvar mew-suspend-hook nil
  "*Hook called on suspend.")
(defvar mew-quit-hook nil
  "*Hook called on quit.")
(defvar mew-summary-inc-sentinel-hook nil
  "*Hook called when inc finished.")
(defvar mew-summary-scan-sentinel-hook nil
  "*Hook called when scan finished.")
(defvar mew-summary-exec-hook nil
  "*Hook called when mew-summary-exec finished.")
(defvar mew-syntax-format-hook nil
  "*Hook called when mew-syntax-format is called.")
(defvar mew-addrbook-mode-hook nil
  "*Hook called in Addrbook mode.")

;;
;;
;; Sub-programs
;;

(defvar mew-prog-imget       "imget")
(defvar mew-prog-imls        "imls")
(defvar mew-prog-imcat       "imcat")
(defvar mew-prog-imclean     "imclean")
(defvar mew-prog-impath      "impath")
(defvar mew-prog-impack      "impack")
(defvar mew-prog-immv        "immv")
(defvar mew-prog-imrm        "imrm")
(defvar mew-prog-imput       "imput")
(defvar mew-prog-imsort      "imsort")
(defvar mew-prog-imstore     "imstore")
(defvar mew-prog-imgrep      "imgrep")
(defvar mew-prog-imjoin      "imjoin")
(defvar mew-prog-mime-encode "mewencode")
(defvar mew-prog-mime-decode "mewdecode")
(defvar mew-prog-unshar      "unshar")
(defvar mew-prog-uumerge     "uumerge")
(defvar mew-prog-tar         "tar")
(defvar mew-prog-compress    "compress")
(defvar mew-prog-gzip        "gzip")
(defvar mew-prog-utime       "touch")
(defvar mew-prog-pgp         "pgp"
  "*PGP name for version check.")

(defvar mew-prog-uncompface  "uncompface")
(defvar mew-prog-shell       "/bin/sh")
;;(defvar mew-prog-shell       "cmd.exe")

(defvar mew-prog-text/html     "netscape")
(defvar mew-prog-text/html-arg nil)

(defvar mew-prog-shell-arg   "-c")

(defvar mew-prog-im-arg '("--help=no" "--debug=no"))

(defvar mew-prog-imget-arg-list nil
  "*Argument list for mew-prog-imget")
(defvar mew-prog-imls-arg-list nil
  "*Argument list for mew-prog-imls")

;;
;; MIME types
;;

(defmacro mew-ct-linebasep (ct)
  (` (mew-member-case-equal (, ct) mew-mime-content-type-text-list)))

(defvar mew-mime-content-type-text-list
  '("Text/Plain" "Text/Html"
    "Application/Postscript"
    "Message/Rfc822")
  "Content-Type: list for text which are treated as text when encoded.")

(defvar mew-mime-content-type-multipart-list
  '("Multipart/Mixed" "Multipart/Alternative"
    ;;"Multipart/Digest" "Multipart/Parallel"
    )
  "Candidate of 'Content-Type: Multipart' when CT: is changed in draft buffer."
  )

(defvar mew-mime-content-type-binary-list
  '("Application/Octet-Stream"
    "Application/Pdf"
    "Image/Jpeg" "Image/Gif" "Image/Tiff"
    "Image/x-xwd" "Image/x-xbm" "Image/x-xpm" 
    "Image/x-bmp" "Image/Png" "Audio/Basic" "Video/Mpeg")
  "Content-Type: list to save as binary file in mew-summary-save on Mule.")

(defvar mew-mime-content-type-others-list
  '("Message/Rfc822" "Message/External-body"))

(defvar mew-mime-content-type-list
  (append mew-mime-content-type-text-list
	  mew-mime-content-type-multipart-list
	  mew-mime-content-type-binary-list
	  mew-mime-content-type-others-list)
  "Candidate of Content-Type: when CT: is changed in draft buffer.")

(defvar mew-mime-content-type-ignore-cdp
  '("Message/.*" "Multipart/.*")
  "Content-Type: list to ignore for insertion Content-Disposition:.")

;;
;; Separators
;;

(defvar mew-path-separator "/")
(defvar mew-header-separator "----")
(defvar mew-eoh (format "^\\(%s\\|\\)$" (regexp-quote mew-header-separator)))
(defvar mew-lwsp "^[ \t]")
(defvar mew-address-separator ":, \t\n")
(defvar mew-page-delimiter "^\^L")
(defvar mew-keyval "^\\([^ \t:]+:?\\)[ \t]*")
;; "^\\([^ \t:]+:?\\)[ \t]*\\(.*\\)$" faces a stupid error. 
;; "$" doesn't mean end-of-line if the second match is empty, sigh.

;;
;; Limits
;;

(defvar mew-file-max-size 100000
  "*The max size of messages. If the size of a message is greater
than mew-file-max-size, Mew skips MIME analysis.")

(defvar mew-header-max-length 100
  "*If the length of a header exceeds this value, 
it is not arranged nor MIME decoded.
See also mew-header-max-depth.")

(defvar mew-header-max-depth 50
  "*A value to decide loop depth for header field syntax analysis.
It was known as mew-loop-depth.
See also mew-header-max-length.")

(defvar mew-references-max-count 3
  "*A value for the max number of message IDs in References: when reply.
Non-integer means no limit.")

(defvar mew-lisp-max-length 1000
  "*Mew saves an internal lisp structure to a file truncating
to this file.")

(defvar mew-expand-max-depth 5
  "*A value to limit Addrbook expansion loop.")

;; 
;; Mail Addresses
;;

(defvar mew-mail-domain-list nil
  "*Your e-mail address domain list like 
(\"Mew.org\" \"iijlab.net\").
They are used for mail-domain completion in Draft mode(C-cTAB)")

(defvar mew-mail-domain (or (car mew-mail-domain-list) "")
  "*Your e-mail address domain.")

(defvar mew-mail-address-list nil
  "*The addresses included in this list never appear on the Cc:
field on a draft buffer.")

(cond 
 ((string= mew-mail-domain "")
  (defvar mew-mail-address (user-login-name)))
 (t
  (defvar mew-mail-address (concat (user-login-name) "@" mew-mail-domain))))

;;
;; Window
;;

(defvar mew-window-configuration 
  '((summary (1  0))
    (message (8 31))
    (draft   (1  0)))
  "*Ratio of windows")

(defvar mew-window-use-full nil
  "*Dynamic window configuration when nil. This variable will be obsolated
because mew will support mew-window-percent(e.g 50, 100, etc.).")

;;
;; Appearance
;;

(defvar mew-mode-line-id '("Mew: %12b")
  "*A default value of mode-line-buffer-identification for each Mew mode.")

(defvar mew-multipart-icon-position 'right
  "*Position where multipart icons are displayed. 
If 'left, displayed at the left size of the default toolbar.
If 'right, displayed at the right size of the default toolbar.
Otherwise, not displayed.")

;; Low level
(defvar mew-folder-list-function 'mew-folder-list)

;;
;; Summary mode
;;

(defvar mew-decode-quoted nil
  "*Decode quoted MIME encoded-word when *non-nil*. 
  Skip decode when nil.")

(defvar mew-summary-cache-use t
  "*If *non-nil*, insert mew-summary-cache-file into Summary mode buffer 
when the Summary mode is visited and it does not exist in buffer. 
mew-summary-cache-file is automatically saved on inc and scan.")

(defvar mew-cache-prefetch t
  "*If non-nil, Mew prefetches the next message in Summary mode
and analyzes it while you are reading the current message.")

(defvar mew-cache-prefetch-remote t
  "*If non-nil, Mew prefetches the next message in remote folder.")

(defvar mew-cache-size 10
  "*Number of buffer for message cache.")

(defvar mew-summary-cache-file ".mew-cache"
  "*Cache file for Summary mode contents.")

(defvar mew-summary-touch-file ".mew-touch"
  "*Time-stamp file for message folders.")

(defvar mew-window-home-buffer "*scratch*"
  "*Buffer name to return if window stack is empty.")

(defvar mew-summary-show-direction 'next
  "*Direction for SPC at end of message. 
'up 'down 'next(current direction) 'stop.
Other values are considered as 'stop.
See also 'mew-summary-mark-direction'.")

(defvar mew-summary-mark-direction 'next
  "*Direction after marking a message
'up 'down 'next(current direction) 'stop.
Other values are considered as 'stop.
See also 'mew-summary-show-direction'.")

(defvar mew-summary-trace-directory t
  "*If non-nil, change to the folder directory when 
mew-summary-goto-folder.")

(defvar mew-summary-recenter-p t
  "*If *non-nil*, the current message is recentered in Summary mode when
displayed.")

(defvar mew-summary-scan-width nil
  "*If *non-nil*, used as --width value.")

(defvar mew-save-dir mew-home
  "*The default directory to save messages or parts in Summary mode.
See also 'mew-copy-dir'.")

(defvar mew-summary-preserve-dir nil
  "*If non-nil, the previous directory is used as the default 
directory for save, etc.  See also 'mew-draft-preserve-dir'.")

(defvar mew-summary-jump-message-then-display t
  "*If non-nil 'mew-summary-jump-message' displays the message
where the cursor jumped.")

(defvar mew-summary-jump-top-then-display t
  "*If non-nil 'mew-summary-jump-top' displays the top message
where the cursor jumped to the top.")

(defvar mew-summary-jump-bottom-then-display t
  "*If non-nil 'mew-summary-jump-bottom' displays the bottom message
where the cursor jumped to the bottom.")

;;
;; Message mode
;;

(defvar mew-end-of-message-string "[End of message]"
  "*A value inserted on the end of message buffer if *non-nil*.")

(defvar mew-end-of-part-string "[Message is continued]"
  "*A value inserted on the end of message buffer if *non-nil*.")

(defvar mew-break-pages t
  "*If *non-nil*, a message is broken by mew-page-delimiter.")

(defvar mew-field-other-visible t
  "*If *non-nil*, fields which are not found in 'mew-field-spec'
are displayed after visible fields. Otherwise they are
hidden before visible fields (and after invisible fields).")

;;
;; Draft mode
;;

(defvar mew-reply-to nil
  "*A value inserted into Reply-To: field in Draft mode if *non-nil*.")

(defvar mew-fcc nil
  "*A value inserted into Fcc: field in Draft mode if *non-nil*.")

(defvar mew-cc nil
  "*A value inserted into Cc: field in Draft mode if *non-nil*.")

(defvar mew-dcc nil
  "*A value inserted into Dcc: field in Draft mode if *non-nil*.")

(defvar mew-from-list nil
  "*A list of From: for circular completion in Draft mode.")

(defvar mew-from (car mew-from-list)
  "*A value inserted into From: field in Draft mode if *non-nil*.")

(defvar mew-header-alist nil
  "*Alist of key and value pair for header field to be inserted on draft.
'((\"X-fingerprint:\" . \"6B 63 38 88 67 5E 96 8E  CE A4 62 73 3F 11 64 94\")
  (\"X-URL:\" . \"http://www.Mew.org/~kazu/\"))"
  )

(defvar mew-require-final-newline t 
  "*If non-nil, Mew adds a new line to the draft if ended without a new
line.")

(defvar mew-field-delete-common '("From " ">From")
  "A field list to be deleted for reediting/resending/forwarding/saving.")

(defvar mew-field-delete nil) ;; backward compatibility

(defvar mew-field-delete-for-reediting
  (or mew-field-delete
      (list mew-date: mew-message-id: mew-from: mew-x-mailer: mew-config:
	    mew-mv: mew-received:
	    "Sender:" "Return-Path:" "Delivery-Date:" "Lines:"
	    "X-Dispatcher" "X-Resent-Dispatcher:" "X-UIDL:"
	    "Forwarded:" "Replied:"
	    "Resent" "Prev-Resent"))
  "*A field list to be deleted when edit again.")

(defvar mew-field-delete-for-resending
  (list mew-config: mew-received: "Return-Path:" "Delivery-Date:" "Lines:"
	"Resent-" "X-Resent-"
	"X-UIDL:" "Forwarded:" "Replied:")
  "*A field list to be deleted when resending.")

(defvar mew-field-delete-for-forwarding ()
  "*A field list to be deleted when forwarding.")

(defvar mew-field-delete-for-saving ()
  "*A field list to be deleted when saving a message to a file.")

(defvar mew-copy-dir mew-home
  "*The default directory from which attachments are copied in Draft mode.
See also 'mew-save-dir'.")

(defvar mew-draft-preserve-dir nil
  "*If non-nil, the previous directory is used as the default 
directory for copy, etc.  See also 'mew-summary-preserve-dir'.")

(defvar mew-draft-mode-auto-save t
  "*If t, a draft is repeatedly saved to 'buffer-auto-save-file-name'
by 'do-auto-save'. If automatic saving is enabled and Emacs is crashed,
the '.save-' file remains. If this value is nil, automatic saving does
not work resulting that no garbage file remains.")

(defvar mew-draft-backup-file ".mew-backup")
(defvar mew-draft-syntax-file ".mew-syntax")
(defvar mew-draft-coverpage "CoverPage")

(defvar mew-attach-move-by-line nil
  "*If non-nil, 'mew-attach-{next,previous}' move the cursor line by line.")

;;
;; IM Config
;;

(defvar mew-config-guess-alist nil
  "*If *non-nil*, this value is used to guess values of the Config: field.
The syntax is exactly the same as 'mew-refile-guess-alist'.")

(defvar mew-config-insert-when-prepared nil 
  "*If *non-nil*, the Config: field is inserted according to
'mew-config-guess-alist' when the draft is prepared.")

(defvar mew-config-insert-when-composed nil
  "*If *non-nil*, the Config: field is inserted according to
'mew-config-guess-alist' when the message is composed.")

(defconst mew-config-default "default"
  "The default value for IM Config to avoid hard cording.
Do not change this value.")

(defvar mew-config-list nil
  "A list of config cases appeared in .im/Config.
It is used for circular completion in Draft mode.
This variable is automatically set. Do NOT set this by yourself.")

(defvar mew-config-imget mew-config-default
  "A config value specified to imget. Use '\\<mew-summary-mode-map>\\[mew-summary-config-imget] in Summary mode
to change this value with mew-config-list.")

;;
;; Completion
;;

(defvar mew-fields
  (list mew-from: mew-to: mew-cc: mew-subj: mew-dcc: mew-fcc: mew-bcc:
	mew-reply-to: mew-followup-to: mew-newsgroups: mew-config:
	mew-resent-to: mew-resent-cc: mew-resent-bcc: mew-resent-dcc:
	mew-resent-from:)
  "*Completion field list in Draft mode.")

(defvar mew-field-completion-switch
  '(("To:"          . mew-complete-address)
    ("Cc:"          . mew-complete-address)
    ("Dcc:"         . mew-complete-address)
    ("Bcc:"         . mew-complete-address)
    ("Reply-To:"    . mew-complete-address)
    ("Fcc:"         . mew-complete-folder)
    ("Resent-To:"   . mew-complete-address)
    ("Resent-Cc:"   . mew-complete-address)
    ("Resent-Dcc:"  . mew-complete-address)
    ("Resent-Bcc:"  . mew-complete-address)
    ("Config:"      . mew-complete-config))
  "*Completion function alist concerned with the key.")

(defvar mew-field-circular-completion-switch
  '(("To:"          . mew-circular-complete-domain)
    ("Cc:"          . mew-circular-complete-domain)
    ("Dcc:"         . mew-circular-complete-domain)
    ("Bcc:"         . mew-circular-complete-domain)
    ("Reply-To:"    . mew-circular-complete-domain)
    ("Resent-To:"   . mew-circular-complete-domain)
    ("Resent-Cc:"   . mew-circular-complete-domain)
    ("Resent-Dcc:"  . mew-complete-address)
    ("Resent-Bcc:"  . mew-complete-address)
    ("From:"        . mew-circular-complete-from)
    ("Resent-From:" . mew-circular-complete-from)
    ("Config:"      . mew-circular-complete-config))
  "*Circular completion function alist concerned with the key.")

(defvar mew-field-expansion-switch
  '(("To:"        . mew-expand-address)
    ("Cc:"        . mew-expand-address)
    ("Dcc:"       . mew-expand-address)
    ("Bcc:"       . mew-expand-address)
    ("Reply-To:"  . mew-expand-address)
    ("Resent-To:" . mew-expand-address)
    ("Resent-Cc:" . mew-expand-address)
    ("Resent-Dcc:"  . mew-complete-address)
    ("Resent-Bcc:"  . mew-complete-address))
  "*Expansion function alist concerned with the key.")

;;
;; Citation
;;

(defvar mew-cite-prefix "> "
  "*Prefix of citation.")
(defvar mew-cite-hook nil
  "*If you want to use super-cite, (setq mew-cite-hook 'sc-cite-original).")
(defvar mew-before-cite-hook nil
  "Called in mew-summary-reply-with-citation before citation.")

(defvar mew-cite-prefix-function nil
  "*Function called on citations. A good candidate is 
'mew-cite-prefix-username")

(defvar mew-cite-prefix-confirmp nil
  "*If *non-nil*, ciattion prefix (such as 'kazu> ') is
confirmed to be used.")

(defvar mew-cite-fields (list mew-from: mew-subj: mew-date:)
  "*The fields that you want to extract as citation label. 
If you change this valuable, you must change mew-cite-format.
The value of the first field becomes the first argment for mew-cite-format.
(e.g. The default first argment is a value of From: field.)
The value of the second field becomes the second argment for mew-cite-format.
....
If this is nil, label is not generated.")

(defvar mew-cite-format "From: %s\nSubject: %s\nDate: %s\n\n"
  "*Format for the citation label.")

(defvar mew-summary-reply-position 'body
  "If 'body, the cursor locates in the beginning of the body. 
Otherwise, the cursor is after To:.")

(defvar mew-summary-reply-with-citation-position 'end
  "If 'body, the cursor locates in the beginning of the body. 
If 'end, the cursor locates after the citation.
Otherwise, the cursor is after To:.")

(defvar mew-charset-input-method-alist nil
  "*Alist of charset and input-method.  When a message is cited into
Draft mode on X/Emacs 20 or later AND the charset of the message is
found in this variable, the corresponding input-method is automatically
selected. An example configuration is as follows:
(setq mew-charset-input-method-alist
      '((\"iso-8859-1\" . \"latin-1-postfix\")
	(\"iso-8859-2\" . \"latin-2-postfix\")))
")

;;
;; Signature
;; 

(defvar mew-signature-file "~/.signature"
  "*A signature file to be inserted in Draft mode. To support multiple
signature files, use 'c-sig.el'.")

(defvar mew-signature-insert-last nil 
  "*If *non-nil*, the signature file is inserted in the last of body. 
Otherwise, it is inserted at the currect point. If you created multipart
and mew-signature-as-lastpart is *non-nil*, this variable is ignored.")

(defvar mew-signature-as-lastpart nil
  "*If *non-nil*, the signature file is appended as the final part 
when you create multipart.")

(defvar mew-signature-description "My signature"
  "*This variable is used as a description if the signature is appended
as the final part.")

;;
;; Temporary solution to decide To: when reply
;;

(defvar mew-noreplyto-to-list (list mew-from:)
  "*To: field list picked at mew-summary-reply if Reply-To: doesn't exist.")

(defvar mew-noreplyto-cc-list (list mew-to: mew-cc: mew-apparently-to:)
  "Cc: field list picked at mew-summary-reply if Reply-To: doesn't exist.")

(defvar mew-replyto-to-list (list mew-reply-to: mew-from:)
  "*To: field list picked at mew-summary-reply if Reply-To: exists.")

(defvar mew-replyto-cc-list (list mew-to: mew-cc: mew-apparently-to:)
  "*Cc: field list picked at mew-summary-reply if Reply-To: exists.")

(defvar mew-fromme-to-list (list mew-to: mew-apparently-to:)
  "*To: field list picked at mew-summary-reply if From: is me.")

(defvar mew-fromme-cc-list (list mew-cc:)
  "*Cc: field list picked at mew-summary-reply if From: is me.")
;;
;; Folders
;;

(defvar mew-folder-mode 448 ;; decimal for octal 0700
  "Secure file mode for folders. 448(0700 in octal) is STRONGLY recommended
for privacy reasons.")

(defvar mew-file-mode 384 ;; decimal for octal 0600
  "Secure file mode. 384(0600 in octal) is STRONGLY recommended
for privacy reasons.")

(defvar mew-use-folders-file-p t
  "*If *non-nil*, Mew tries to load mew-folders-file to make bootup faster.
And '\\[universal-argument] \\[mew-status-update]' saves folder list to mew-folders-file.")

(defvar mew-folders-file ".folders"
  "*A file to save folder list. It is used on start-up to
make start-up much faster.")

;; xxx hard coding...
(defvar mew-folders-ignore '("+from" "+draft" "=")
  "*Folder list to contain messages whose To: is you and From: is
personal, e.g. a friend. Directories under this folder is ignored
for refile guess.")

(defvar mew-folders-default-folder "+from"
  "*A default folder used by mew-guess-by-default.
mew-guess-by-default append the From: value to it.
e.g. '+from/kazu'")

(defvar mew-folder-list-skip-pattern
  (if (and (eq system-type 'windows-nt)
	   (not (featurep 'meadow)))
      "^[0-9]+$"
    nil)
  "This value is used to make collecting folder faster.
If nil, Mew skips directories whose link count is 2, that is
they don't have subdirectories. Unfortunately, link count is
not available on some OSes. So, if a regular express pattern is 
set to this value, Mew skips directories whose name are matched
this value. A good example value is \"^[0-9]+$\",
especially for Windows (excluding WNT).")

(defvar mew-touch-folder-p nil) 

;;
;; Refile and delete
;;

(defvar mew-msg-rm-policy 'trashonly
  "*Set remove policy. You can set one of the followings:
'totrash   : Refile to the +trash folder if not in the +trash folder.
             Just unmark the 'D' mark if in the +trash folder.
'always    : Really remove messages marked with 'D' always anyway.
'trashonly : Really remove messages marked with 'D' if in the +trash folder.
             In other folders, refile to the +trash folder.
'uselist   : Really remove messages marked with 'D' if in a folder found
	     in @samp{mew-msg-rm-folder-list}. In other folders, refile 
             to the +trash folder.
otherwise  : considered as 'totrash.")

(defvar mew-msg-rm-folder-list nil
  "*Folder list to remove message really.")

;;
;; File
;;

(defvar mew-file-append-p nil
  "*If *non-nil*, a message or a part is appended to the existing file
on '\\<mew-summary-mode-map>\\[mew-summary-save]'. Otherwise overwrited.")

(defvar mew-temp-file-initial
  (expand-file-name (user-login-name) (or (getenv "TMP")
					  (getenv "TEMP")
					  "/tmp"))
  "*Hint to make a secure directory on the local file system. On
setup phase Mew make a secure directory from this variable and set
mew-temp-file a file name prefix contained the directory name. The
directory must be unreadable from others, otherwise it might become a
big security hole. And this directory must not be gained access
through network to prevent tire-tapping. Mew never uses 
'call-process-region' rather does use 'call-process' creating a
temporary file with mew-temp-file by itself.  If 'call-process-region'
is used, Emacs creates a temporary file (probably in /tmp). So bad
guys can wiretap the temporary file.")

(defvar mew-delete-temp-file t
  "*If *non-nil*, delete temporary files when external commands terminate.")

;;
;; Demo
;;

(defvar mew-demo t
  "*Mew demo is displayed at boot time if *non-nil*.")

(defvar mew-demo-picture mew-xemacs-p
  "*A picture of cats is displayed if *non-nil* on XEmacs.")

(defvar mew-icon-mew "Mew.png")
(defvar	mew-icon-mew-mono "Mew.xbm")
(defvar	mew-icon-mew-mule-bitmap-image "Mew.img")

(defvar mew-logo nil)

;;
;; Ask?
;; 

(defvar mew-ask-cc t
  "*Prompt user for CC: field if *non-nil*.")

(defvar mew-ask-subject nil
  "*Prompt user for Subject: field when send if *non-nil*.")

(defvar mew-ask-range nil)

(defvar mew-ask-newsgroups nil
  "*If *non-nil*, prompt user if he/she want to include Newsgroups: field.")

(defvar mew-ask-config nil
  "*If *non-nil*, prompt user if he/she want to include/change Config: 
field.")

(defvar mew-ask-cite-prefix nil
  "*If *non-nil*, ask citation prefix when cite a message.")

(defvar mew-ask-pack t
  "*If *non-nil*, ask whether or not you really want to pack.")

(defvar mew-ask-send t
  "*If *non-nil*, ask whether or not you really want to send the message
which you composed without explicit C-cC-m.")

(defvar mew-ask-flush-queue nil
  "*If *non-nil*, ask whether or not you really want to flush queue.")

;;
;; Field Magic
;;

(defvar mew-reply-string "Re: ")
(defvar mew-reply-regex "^Re:")
(defvar mew-forward-string "Fw: ")
(defvar mew-forward-regex "^\\(Fw\\|Forward\\):")

;;
;; Use
;;

(defvar mew-use-imap nil
  "*If nil, both 'immv' and 'imrm' are called synchronously for speed when
'\\<mew-summary-mode-map>\\[mew-summary-exec] is typed. If *non-nil*, they are called asynchronously to catch 
user's password for IMAP operation.")

(defvar mew-summary-imap-cache nil
  "*If *non-nil*, use mew-summary-cache-file for imap folders.")

(defvar mew-summary-imap-nocache-folders '("%inbox" "%#mhinbox" "%#mh/inbox")
  "*List of imap folders, not create mew-summary-cache-file.")

(defvar mew-use-immv nil
  "*If non-nil, Mew uses 'immv' for refile. Otherwise, Mew 
refiles messages by Elisp-version code.")

(defvar mew-use-text/enriched (equal mew-mule-ver 3)
  "*If non-nil, Mew highlights enriched format text messages.")

(defvar mew-use-config-imget-for-draft nil
  "*If non-nil, Mew inserts the Config: field with a non-default
value in Draft mode.")

(defvar mew-use-symbolic-link-for-forwarding nil
  "*If nil, messages to be forwarded is copied to +draft when
'\\<mew-summary-mode-map>\\[mew-summary-forward]' and '\\[mew-summary-multi-forward]' are used. Otherwise, symbolic links are
created (if the feature is provided).")

(defvar mew-use-timer t
  "*If non-nil, timer is used to expire cached passphrases.")

(defvar mew-use-cached-passwd nil
  "*if non-nil, password for imget, which you inputed, is cached.")

(defvar mew-use-pgp-cached-passphrase nil
  "*if non-nil, PGP passphrase, which you inputed, is cached to be used
for further PGP processes.")

(defvar mew-use-imget-assoc nil
  "*if non-nil, password is asked like
	Password (proto/auth:user@host):.
Otherwise, asked like
	Password (case):.
")

;;
;; X face
;;

(defvar mew-x-face-filter (list mew-prog-uncompface "ikon2xbm"))
(defvar mew-x-face-prog "xv")
(defvar mew-x-face-args nil)
(defvar mew-x-face-file "~/.xface"
  "*If *non-nil* and the file exists, X-Face: fields is inserted.")

;;
;; Auto
;;

(defvar mew-auto-get t
  "If *non-nil*, Mew gets messages from your spool automatically. 
\"\\[universal-argument] \\[mew]\" equals to \"\\[mew]\" with 'mew-auto-get'
revered.")
 

(defvar mew-auto-flush-queue nil
  "If *non-nil* and if there are queued messages in  IM's QueueDir,
they are flushed at 'imget'(i.e. '\\<mew-summary-mode-map>\\[mew-summary-get]'). This idea saves money in 
dial up environment.")

;;
;; Pick and Sort
;;

(defvar mew-pick-default-field nil
  "*Default prefix string to be appeared when inputing a pick pattern.
A good example is \"from=\".")

(defvar mew-pick-duplicate-msgid      "dup-msgid")
(defvar mew-pick-duplicate-subj-msgid "dup-subj-msgid")

(defvar mew-pick-field-list
  (list "head=" "body=" "all="
	"to=" "cc=" "subject=" "dcc=" "fcc=" "bcc=" "date="
	"reply-to=" "followup-to=" "from=" "newsgroups="
	mew-pick-duplicate-msgid mew-pick-duplicate-subj-msgid)
  "*A list of key for pick pattern.")

(defvar mew-sort-default-key "date"
  "*Default sort key when inputing a sort key. 
Its format is key:mode where more is found in 'mew-sort-mode'.")

(defvar mew-sort-default-key-alist nil
  "*Alist of (folder . sort-key) to decide a default sort-key
of a specific folder. An example is follows:
(setq mew-sort-default-key-alist
      '((\"+tmp/beginners\" . \"x-sequence\")
        (\"+tmp/elips\" . \"x-mail-count\")))")

(defvar mew-sort-key-alist
  '(("date" . "date") ("subject") ("from") ("to") ("newsgroups")
    ("posted" . "date") ("x-sequence" . "num") ("x-mail-count" . "num")
    ("x-ml-count" . "num"))
  "*List of fields for 'mew-summary-sort'.
Each element is (FIELD-NAME) or (FIELD-NAME . MODE).
MODE is one of \"date\" (sort by chronological order) or
               \"num\"  (sort by numerical order) or
               \"text\" (sort by alphabetical order) or
               \"ml\"   (modify subject).
(nil means \"text\").")

(defvar mew-sort-modes '("date" "num" "text" "ml"))

;;
;; Range
;;

(defvar mew-input-range-list
  '("update" "all" "first:" "prev:" "next:" "last:")
  "*A list used by range completion.")

(defvar mew-range-auto-alist 
  '(("^+draft" . "all") (".*" . "update"))
  "*Alist of folder-range pair for automatic scan.")

(defvar mew-range-interactive-alist
  '(("^+draft" . "all") (".*" . "update"))
  "*Alist of folder-range pair for interactive scan.")

;;
;; PGP Public key fetch
;;

(defvar mew-pgp-keyserver-url-template
  "http://pgp5.ai.mit.edu:11371/pks/lookup?op=get&search=%s"
;;  "http://pgp.nic.ad.jp/cgi-bin/pgpsearchkey.pl?op=get&search=%s"
  )

(defvar mew-x-pgp-key-list
  '("x-pgp-key:" "x-pgp-key-url:" "x-pgp-public-key:" "x-pgp-public-key-url:"
    "x-pgp5-key:" "x-pgp5-key-url:" "x-pgp5-public-key:"
    "x-pgp5-public-key-url:" "x-public-key:"))
   
(defvar mew-pgp-fetch-key-process nil)

;;
;; Marks
;;

(defvar mew-mark-multi  ?@)
(defvar mew-mark-review ?*)
(defvar mew-mark-delete ?D)
(defvar mew-mark-refile ?o) ;; do you like "^"?
(defvar mew-mark-tmp    ?%) ;; temporary use only.

;;
;; Highlight
;;

(defvar mew-cursor-mark ">"
  "*The mark in the beginning of the cursor line if
mew-use-cursor-mark is *non-nil*. ")
(defvar mew-use-cursor-mark nil
  "*If *non-nil*, show mew-cursor-mark in the beginning of the cursor line.
This is convenient if underline is not available.")

(defvar mew-use-highlight-cursor-line t
  "*Put underline on the current line in Summary mode.")
(defvar mew-use-highlight-mouse-line mew-xemacs-p
  "*Paint the line where mouse locates in Summary mode.")
(defvar mew-use-highlight-mark t
  "*Paint marked lines in Summary mode.")
(defvar mew-use-highlight-header t
  "*Paint header in Message and Draft mode.")
(defvar mew-use-highlight-body nil
  "*Paint body in Message and Draft mode.")
(defvar mew-use-highlight-url t
  "*Emphasize URL lines in Message mode.")
(defvar mew-use-highlight-url-regex
  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "*Regular expression to find URL.")

(defvar mew-use-highlight-x-face mew-xemacs-p
  "*Iconify X-Face: on XEmacs in Message mode.")

(defvar mew-highlight-body-max-line 200)

(defvar mew-highlight-url-max-size 10000
  "*Use bold font at url maximize size.
If nil, all contents are parsed.")

;;
;; Styles and colors
;;

(defvar mew-highlight-cursor-line-face 'underline
  "*Face to highlight the cursor line in Summary and Virtual mode")
(defvar mew-highlight-mouse-line-function
  (function mode-motion-highlight-line)
  "*A function to highlight the mouse line in Summary and Virtual mode")
(defvar mew-highlight-url-face         'bold
  "*Face to highlight URL in Message mode")
(defvar mew-highlight-url-mouse-face   'highlight
  "*Mouse face to highlight URL in Message mode")

(defvar mew-highlight-header-face-list
  '(mew-highlight-header-face-subject
    mew-highlight-header-face-from
    mew-highlight-header-face-to
    mew-highlight-header-face-key
    mew-highlight-header-face-private
    mew-highlight-header-face-important
    mew-highlight-header-face-marginal
    mew-highlight-header-face-xmew
    mew-highlight-header-face-xmew-bad)
  "*A list of face symbol name to highlight header.
Each name should be 'mew-highlight-header-face-<word>'.
Each face will be created from 'mew-highlight-header-style-<word>' and
'mew-highlight-header-color-<word>'. These faces can be used in
'mew-field-spec'.")

(defvar mew-highlight-header-style-subject   'bold)
(defvar mew-highlight-header-style-from      'bold)
(defvar mew-highlight-header-style-to        'bold)
(defvar mew-highlight-header-style-key       'bold)
(defvar mew-highlight-header-style-private   'bold)
(defvar mew-highlight-header-style-important 'bold)
(defvar mew-highlight-header-style-marginal  'bold)
(defvar mew-highlight-header-style-xmew      'bold)
(defvar mew-highlight-header-style-xmew-bad  'bold)

(defvar mew-highlight-header-color-subject   "firebrick")
(defvar mew-highlight-header-color-from      "Purple")
(defvar mew-highlight-header-color-to        "DarkOrange2")
(defvar mew-highlight-header-color-key       "ForestGreen")
(defvar mew-highlight-header-color-private
  (if (or mew-xemacs-p (fboundp 'frame-face-alist))
      (face-foreground 'default)
    nil))
(defvar mew-highlight-header-color-important "Blue")
(defvar mew-highlight-header-color-marginal  "gray50")
(defvar mew-highlight-header-color-xmew      "chocolate")
(defvar mew-highlight-header-color-xmew-bad  "red")

(defvar mew-highlight-body-face-list
  '(mew-highlight-body-face-quote
    mew-highlight-body-face-comment)
  "*A list of face symbol name to highlight body.
Each name should be 'mew-highlight-body-face-<word>'.
Each face will be created from 'mew-highlight-body-style-<word>' and
'mew-highlight-body-color-<word>'. These faces can be used in
'mew-highlight-body-keywords'.")

(defvar mew-highlight-body-style-quote   'default)
(defvar mew-highlight-body-style-comment 'default)

(defvar mew-highlight-body-color-quote   "ForestGreen")
(defvar mew-highlight-body-color-comment "gray50")

;; xxx hard coding...
(defvar mew-highlight-mark-folder-list '("+inbox")
  "*A folder list to highlight marked lines. If 't, 
marked lines in all folders are highlight. ")

(defvar mew-highlight-mark-face-list
  '(mew-highlight-mark-face-review
    mew-highlight-mark-face-multi
    mew-highlight-mark-face-delete
    mew-highlight-mark-face-refile)
  "*A list of face symbol name to highlight marked lines.
Each name should be 'mew-highlight-mark-face-<word>'.
Each face will be created from 'mew-highlight-mark-style-<word>' and
'mew-highlight-mark-color-<word>'. These faces can be used in
'mew-highlight-mark-keywords'.")

(defvar mew-highlight-mark-style-review 'default)
(defvar mew-highlight-mark-style-multi  'default)
(defvar mew-highlight-mark-style-delete 'default)
(defvar mew-highlight-mark-style-refile 'default)

(defvar mew-highlight-mark-color-review "Blue")
(defvar mew-highlight-mark-color-multi  "Purple")
(defvar mew-highlight-mark-color-delete "firebrick")
(defvar mew-highlight-mark-color-refile "ForestGreen")

(defvar mew-field-spec
  '(("^Resent-\\(From\\|To\\|Cc\\|Date\\)" t
     mew-highlight-header-face-important
     mew-highlight-header-face-important)
    ("^Subject:$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-subject)
    ("^From:$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-from)
    ("^\\(To\\|Apparently-To\\):$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-to)
    ("^Cc:$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-to)
    ("^Newsgroups:$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-to)
    ("^Date:$" t)
    ("^Reply-To:$" t)
    ("^X-Mailer:$" t)
    ("^X-Mew:$" t
     mew-highlight-header-face-important
     mew-highlight-header-face-xmew)
    ("^\\(Received\\|Return-Path\\|Sender\\|Errors-To\\):$" nil)
    ("^\\(Message-Id\\|Posted\\|In-Reply-To\\|References\\|Precedence\\):$" nil)
    ("^Delivered-" nil)
;;    ("^Content-" t)
    ("^\\(Mime-Version\\|Lines\\):$" nil)
    ("^From$" nil)
    ("^X-" nil
     mew-highlight-header-face-private
     mew-highlight-header-face-marginal))
  "*An alist of field spec for Message mode. Each spec
consists of field-regular-expression, visible-p, face-for-key, 
and face-for-value. Fields whose visible-p is t are displayed in 
Message mode in the defined order. Fields whose visible-p is nil are
hidden in Message mode. Type DEL to see them. Fields not matched
to field-regular-expressions are operated by the value of
'mew-field-other-visible'. If face-for-key is omitted, 
'mew-highlight-header-face-key' is used. If face-for-value is not
present, mew-highlight-header-face-marginal is used.")

(defvar mew-highlight-body-keywords
  '(("^[ \t]*\\(\\w*[A-Za-z0-9'-]*[>|]+.*\\)"
     mew-highlight-body-face-quote)
    ("^#+.*"
     mew-highlight-body-face-comment))
  "*A list of ('body-key-regex' face-for-body).
This is used to highlight body.")

(defvar mew-highlight-body-keywords-regex nil)

(defvar mew-highlight-mark-keywords
  (list (cons mew-mark-review 'mew-highlight-mark-face-review)
	(cons mew-mark-multi  'mew-highlight-mark-face-multi)
	(cons mew-mark-delete 'mew-highlight-mark-face-delete)
	(cons mew-mark-refile 'mew-highlight-mark-face-refile))
  "A list of mark-face pair to used in Summary mode where its folder name
is found in 'mew-highlight-mark-folder-list'.")

;;
;; Error
;;

(defvar mew-error-unknown-charset "**UNKNOWN CHARSET**")
(defvar mew-error-illegal-base64 " **BASE64 ENCODING ERROR** ")
(defvar mew-error-charset-unknown "unknown")

;;
;; MIME control
;;


(defvar mew-mime-content-type
  '(("multipart/*" nil         nil     nil            mew-icon-multipart)
    ("audio/basic" "\\.au$"    mew-b64 mew-prog-audio mew-icon-audio)
    ("image/gif"   "\\.gif$"   mew-b64 mew-prog-gif   mew-icon-image)
    ("image/tiff"  "\\.tiff$"  mew-b64 mew-prog-tiff  mew-icon-image)
    ("image/jpeg"  "\\.jpe?g$" mew-b64 mew-prog-jpeg  mew-icon-image)
    ("image/png"   "\\.png$"   mew-b64 mew-prog-png   mew-icon-image)
    ("image/x-xwd" "\\.xwd$"   mew-b64 mew-prog-xwd   mew-icon-image)
    ("image/x-xbm" "\\.xbm$"   mew-b64 mew-prog-xbm   mew-icon-image)
    ("image/x-xpm" "\\.xpm$"   mew-b64 mew-prog-xpm   mew-icon-image)
    ("image/x-bmp" "\\.bmp$"   mew-b64 mew-prog-bmp   mew-icon-image)
    ("image/.*"    "^$"        mew-b64 mew-prog-image mew-icon-image)
    ("video/mpeg"  "\\.mpe?g$" mew-b64 mew-prog-mpeg  mew-icon-video)
    ("message/rfc822"          "^[0-9]+$" nil
     mew-prog-rfc822          mew-icon-message/rfc822)
    ("message/external-body"   "\\.ext$"  nil
     mew-prog-external-body   mew-icon-message/external-body)
    ("message/delivery-status" "^$"       nil
     mew-prog-delivery-status mew-icon-text)
    ("application/postscript" "\\.ps$"   mew-qp
     mew-prog-postscript mew-icon-application/postscript)
    ("application/pdf"        "\\.pdf$"  mew-b64
     mew-prog-pdf        mew-icon-application/postscript)
    ("application/pgp-keys"    "\\.pka$" nil
     mew-prog-pgp-keys   mew-icon-unknown)
    ("application/octet-stream"
      "\\.tar\\.?g?z?$\\|\\.tgz$\\|\\.gz$\\|\\.Z$\\|\\.lzh$\\|\\.zip$\\|\\.bin$\\|\\.pgp$"
      mew-b64 mew-prog-octet-stream mew-icon-application/octet-stream)
    ("text/html"     "\\.html$" nil     mew-prog-html      mew-icon-text)
    ("text/enriched" "\\.rtf$"  nil     mew-prog-enriched  mew-icon-text)
    ;; must be here
    ("text/plain"    ".*"       nil     mew-prog-plain     mew-icon-text)
    ("text/.*"       "^$"       nil     mew-prog-text      mew-icon-text)
    (".*" "^$" nil mew-prog-octet-stream mew-icon-unknown))
  "(content-type filename <encoding> <prog> <icon>)"
   )

;;
;; Addrbook
;;

(defvar mew-use-auto-alias t
  "*If non-nil, addresses on the To: and Cc: field in Draft mode
will be automatically leaned as alias.")

(defvar mew-alias-comment-regex "^;.*$\\|#.*$"
  "*Regular expression for the old Aliases.")

(defvar mew-addrbook-comment-regex "^;.*$\\|#.*$"
  "*Regular expression for Addrbook.")

(defvar mew-addrbook-unexpand-regex "-$"
  "Regular expression not to expand in Draft mode.
For example, consider a shortname 'friends-' which is
to be expanded into many addresses. You may want to retain
'friends-' of 'To: party attendance:friends-;'.")

(defvar mew-addrbook-append-domain-p t
  "If non-nil, addresses, which don't have domain part in a header,
will be appended 'mew-mail-domain' when composing.")

(defvar mew-addrbook-override-by-newone t
  "If non-nil, the 'user' entry in 'mew-alias-auto-alist'
is override by a new entry of (user different-address). 
This means that addresses in To: and Cc: in Draft mode are
always learned with an exception 'user' is defined in Addrbook.
If nil,  the old 'user' entry remains.")

(defvar mew-addrbook-switch
  '((shortname . mew-addrbook-shortname-get)
    (address   . identity)
    (username  . mew-addrstr-extract-user)
    (nickname  . mew-addrbook-nickname-get)
    (name      . mew-addrbook-name-get))
  "Function database to get each field of Addrbook.")

(defvar mew-addrbook-for-cite-label nil
  "*How to replace the From: value in cite label
with Addrbook. See 'mew-addrbook-switch'.")
(defvar mew-addrbook-for-cite-prefix 'username
  "*How to replace the From: value in cite prefix
with Addrbook. See 'mew-addrbook-switch'.")
(defvar mew-addrbook-for-address-expansion 'name
  "*How to replace an address in address fields
with Addrbook. See 'mew-addrbook-switch'.")

;;(defvar mew-addrbook-for-summary 'nickname) ;; in the future...

;;
;; Privacy
;;

(defvar mew-privacy-database
  (list
   (list 'pgp-signature  (list (list mew-ct-mls mew-ct-pgs)) "PS")
   (list 'pgp-encryption (list (list mew-ct-mle mew-ct-pge)) "PE")
   (list 'pgp-signature-encryption
	 (list (list mew-ct-mls mew-ct-pgs) (list mew-ct-mle mew-ct-pge))
	 "PSPE")
   (list 'pgp-encryption-signature
	 (list (list mew-ct-mle mew-ct-pge) (list mew-ct-mls mew-ct-pgs))
	 "PEPS"))
  "*Alist of key, a list of privacy Content-Type, and its mark.")

(defvar mew-protect-privacy-always nil
  "*If non-nil, a draft is to be protected according to
'mew-protect-privacy-always-type'.")

(defvar mew-protect-privacy-always-type 'pgp-signature
  "*A type of privacy protection for all drafts.
Currently, 'pgp-signature, 'pgp-encryption, 'pgp-signature-encryption,
'pgp-signature-encryption, and nil are available. Since signature
does not require receiver's public key, signature service may be
appropriate for this value. ")

(defvar mew-protect-privacy-encrypted nil
  "*If non-nil, a draft replying a encrypted message is to be protected 
according to 'mew-protect-privacy-encrypted-type'.")

(defvar mew-protect-privacy-encrypted-type 'pgp-encryption
  "*A type of privacy protection for drafts replying encrypted
messages. Currently, 'pgp-signature, 'pgp-encryption, 
'pgp-signature-encryption, 'pgp-signature-encryption, 
and nil are available. ")


(defvar mew-passwd-timer-unit 10
  "*Minutes of timer unit to cancel the cached passwords.")

(defvar mew-passwd-lifetime 2
  "*Number of 'mew-passwd-timer-unit' to cancel the cached passwords.")

(defvar mew-passwd-reset-timer t
  "If non-nil, a timer of a cached password is cleared when
the password is used.")

;;
;; Misc
;;

(defvar mew-print-function (function lpr-buffer))

(defvar mew-prog-text/html-arg-hack nil
  "*A kind of hook to modify arguments of mew-prog-text/html. 
A good candidate is \"mew-prog-text/html-netscape-remote\".")

(defvar mew-prog-text/html-netscape-remote-format "openURL(%s)"
  "*A format used in \"mew-prog-text/html-netscape-remote\" to
embed a temporary local HTML file. It must contains one %s, which
will be replaced with the file name. Another example is 
\"openURL(%s,new-windows)\".")

;;
;; Environments
;;

(cond
 ((memq system-type '(OS/2 emx))
  (require 'mew-os2))
 ((eq system-type 'windows-nt)
  (require 'mew-win32))
 (t
  (require 'mew-unix))
 )

(provide 'mew-vars)

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

;;; mew-vars.el ends here
