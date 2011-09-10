;; anon-remail.el v1.4, anonymous remailer interface
;; Written by michael shiplett <walrus@umich.edu>
;; Comments or suggestions welcomed.
;; Formatting based on mailcrypt.el by Jin Choi <jsc@mit.edu>.

;; LCD Archive Entry:
;; anon-remail.el|michael shiplett|walrus@umich.edu|
;; Interface to ease use of anonymous, encrypting remailers.|
;; 07-Mar-1994|Version 1.4|~/interfaces/anon-remail.el.Z|

;;{{{ License

;; This file is intended to be used with FSF and Lucid Emacs.

;; Copyright (c) 1994 Regents of The University of Michigan.
;; All Rights Reserved.

;; Permission to use, copy, modify, and distribute this software and
;; its documentation for any purpose and without fee is hereby
;; granted, provided that the above copyright notice appears in all
;; copies and that both that copyright notice and this permission
;; notice appear in supporting documentation, and that the name of The
;; University of Michigan not be used in advertising or publicity
;; pertaining to distribution of the software without specific,
;; written prior permission. This software is supplied as is without
;; expressed or implied warranties of any kind.

;;}}}
;;{{{ Change Log

;;{{{ Changes from 1.3

;; * Added University of Michigan copyright information.
;; * First release since initial cypherpunks release.

;;}}}
;;{{{ Changes from 1.2

;; * Add support for mailcrypt menubar under lemacs
;; * Modify code for menu-item insertion for FSF emacs

;;}}}
;;{{{ Changes from 1.1

;; * Added `ar-add-to-mailcrypt-write-menu'
;; * Gave better documentation for `ar-wrap-message'
;;   and `ar-wrap-message-for-many-remailers'.

;;}}}
;;{{{ Changes from 1.0

;; * Added `ar-wrap-message-for-many-remailers'
;; * Changed `ar-start-hook' to `ar-wrap-hook'
;; * Used folding-mode to organize file
;; * Added LCD entry

;;}}}

;;}}}
;;{{{ Usage

;;{{{ Requirements

;; I do not know if this works with Emacs 18.

;; Mailcrypt 1.3 or higher is required for menus to work.

;; Unless you know of and use RIPEM-based remailers, you must have
;; mailcrypt configured to use PGP.

;;}}}
;;{{{ Installation

;; Add something like the following to your .emacs file (of course, replace
;; the phony remailer addresses with ones you want to use):

;; (autoload 'ar-wrap-message "anon-remail" nil t)
;; (add-hook 'ar-wrap-hook
;;   (function (lambda ()
;;	        (setq ar-remailer-list
;;		      '(
;;                     "remailer@somewhere.com"
;;                     "remailer@elsewhere.org"
;;		      )))))

;; The following bit of code in one's .emacs file will add the
;; anon-remail functions to the mailcrypt-write menu. (All the
;; mailcrypt code is from the mailcrypt.el v1.3 mode example.)

;; Combined mailcrypt-write and anon-remail menu
;; (defun mc-install-write-mode ()
;;  (require 'mailcrypt)
;;  (if (eq window-system 'x)
;;      (progn
;;	(mc-create-write-menu-bar)
;;	(require 'anon-remail)		; Specific to anon-remail
;;	(ar-add-to-mailcrypt-write-menu))) ; Specific to anon-remail
;;  (local-set-key "\C-ce" 'mc-encrypt-message)
;;  (local-set-key "\C-cs" 'mc-sign-message)
;;  (local-set-key "\C-ci" 'mc-insert-public-key))

;;}}}
;;{{{ Notes

;; The ultimate recipient's address must be in a valid ``To:'' format.
;; An address depending on alias expansion will not work because your
;; mail program (MH, Elm, mail, etc.) will not get a chance to process
;; them before the message is wrapped.

;; If you have a way to expand mail aliases in place, then you could
;; call this function from `ar-wrap-hook' and ignore the above
;; warning.

;; You may optionally sign the first wrapping, i.e., the recipient
;; wrapping. Subsequent wrappings should not be signed unless you do
;; not care about anonymity while in the remailing web.

;; After the message has been fully wrapped, a list will appear in the
;; minibuffer--this is the route the message will take.

;;}}}

;;}}}
;;{{{ To Do:

;; Add suggestions for default keybindings.

;; Install menu items at the *bottom* of the mailcrypt menu in FSF
;; emacs.

;; Modify mc-encrypt to take a boolean argument for whether to sign
;; the message.

;; Allow for different remailer lists based on whether the transit
;; delay one wants, e.g., fast, normal, or slow.

;;}}}

(require 'mailcrypt)

;;{{{ Variables & Hooks

(defvar ar-remailer-list nil "*List of remailers from which to choose.")
(defvar ar-hops 3 "*Number of remailers among which to pass message.")

(defvar ar-wrap-hook nil)

;;}}}
;;{{{ Utility Functions

;;{{{ ar-choose-remailer

(defun ar-choose-remailer ()
  "*Select a random remailer from `ar-remailer-list'."
  (let (number-of-remailers remailer)
    ;; Choose a remailer
    (setq number-of-remailers (length ar-remailer-list))
    (or number-of-remailers
	(error "No remailers!"))
    (nth (random number-of-remailers) ar-remailer-list)))

;;}}}
;;{{{ ar-set-recipient

(defun ar-set-recipient (recipient)
  "*Set the ``To:'' field of a message. This will not work on
a multi-line ``To:''."
  (or recipient
   (error "No recipient!"))

  (goto-char (point-min))
  (search-forward "To:")
  (let ((beg (point)))
    (end-of-line)
    (delete-region beg (point)))
  (insert " " recipient))

;;}}}

;;}}}
;;{{{ Wrapping Functions

;;{{{ ar-wrap-message

(defun ar-wrap-message (&optional hops)
  "*Encrypt the current buffer for an individual, then wrap and
encrypt it for HOPS number of remailers. If HOPS is nil, use the value
of `ar-hops'."
  (interactive "P")
  (run-hooks 'ar-wrap-hook)
  (ar-wrap-message-for-individual)
  (let ((route (ar-wrap-message-for-many-remailers hops)))
    (message "%s" route)))

;;}}}
;;{{{ ar-wrap-message-for-many-remailers

(defun ar-wrap-message-for-many-remailers (&optional hops)
  "*Do not encrypt the current buffer for an individual before
wrapping and encrypting for HOPS number of remailers. If HOPS is nil,
use the value of `ar-hops'. This can be useful if you are sending the
message to a mailing list, where you want it to appear in cleartext."
  (interactive "P")
  (run-hooks 'ar-wrap-hook)
  (let ((remailer-path (list (mail-fetch-field "to" nil t))))
    (if (not hops)
	(setq hops ar-hops))
    (while (< 0 hops)
      (let ((remailer (ar-choose-remailer)))
	;; `remailer-path' is to prevent us from sending to the same
	;; remailer twice in a row.  It gives the path the message
	;; will take.
	(while (string= remailer (car remailer-path))
	  (setq remailer (ar-choose-remailer)))
	(setq remailer-path (cons remailer remailer-path))
	(ar-wrap-for-remailer remailer)
	(setq hops (1- hops))))
    remailer-path))

;;}}}
;;{{{ ar-wrap-for-remailer

(defun ar-wrap-for-remailer (remailer)
  "*Wrap the current buffer for a specified remailer."
  (let (recipient)
    ;; Keep track of whom should receive the resent message
    (setq recipient (mail-fetch-field "to" nil t))

    ;; Add the magic redirection words
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (setq start (point))
    (insert "::\nRequest-Remailing-To: " recipient "\n\n")

    ;; Wrap the message for the remailer
    (mc-encrypt-message remailer nil)

    ;; Add in the final magic remailer incantation
    (goto-char start)
    (insert "::\nEncrypted: PGP\n\n")

    ;; Set the message to be sent to the remailer
    (ar-set-recipient remailer)
    ))

;;}}}
;;{{{ ar-wrap-message-for-individual

(defun ar-wrap-message-for-individual ()
  "*Does the initial wrap for a message not intended for a remailer"
  ;; Figure out to whom the message is currently intended
  (let (recipient)
    (setq recipient (mail-fetch-field "to" nil t))
    (mc-encrypt-message recipient nil)
    ))

;;}}}

;;}}}
;;{{{ Menubar

;;{{{ ar-add-to-mailcrypt-write-menu

;; We glom onto the mailcrypt menu. Until I learn how to check for the
;; mailcrypt menu, it is the responsibility of the user to make sure
;; there is a mailcrypt menu.

(defun ar-add-to-mailcrypt-write-menu ()
  (cond
   ((string-match "Lucid" (emacs-version))
    (progn
      (add-menu-item '("Mailcrypt")
		     "Wrap & Encrypt Message"
		     'ar-wrap-message
		     t)
      (add-menu-item '("Mailcrypt")
		     "Wrap Message"
		     'ar-wrap-message-for-many-remailers
		     t)))
   ((string< "19" emacs-version)
    (define-key (current-local-map)
      [menu-bar mailcrypt wrap-encrypt]
      (cons "Wrap & Encrypt Message"  'ar-wrap-message))
    (define-key (current-local-map)
      [menu-bar mailcrypt wrap]
      (cons "Wrap Message"  'ar-wrap-message-for-many-remailers)))
   (t nil)))

;;}}}

;;}}}

(provide 'anon-remail)


;; Local Variables:
;; folded-file: t
;; End:
