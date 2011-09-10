;;; -*- emacs-lisp -*-
;;;
;;; name: mew-mailto.el
;;; version: 0.6
;;; description: some mailto support for mew
;;; creation date: 1998-11-07
;;; author: "Sen Nagata" <sen@eccosys.com>
;;; warning: not optimized at all

;;; required:
;;;
;;;   -mew (1.94 and up -- uses mew-user-agent-compose)
;;;   -rfc2368.el
;;;   -thingatpt.el (comes w/ emacs) for thing-at-pt

;;; installation:
;;;
;;;   -put this file (and rfc2368.el) in an appropriate directory (so emacs 
;;;    can find it)
;;;
;;;   <necessary>
;;;   -put:
;;;
;;;     (add-hook 'mew-init-hook (lambda () (require 'mew-mailto)))
;;;
;;;    in your .emacs file.

;;; details:
;;;
;;;   this package provides a number of interactive functions
;;; (commands) for the user.  each of the commands ultimately creates a
;;; draft message based on some information.  the names of the commands
;;; and brief descriptions are:
;;;
;;;     1) mew-mailto-compose-message-from-mailto-url
;;;            make a draft message from a user-specified mailto: url
;;;
;;;     2) mew-mailto-compose-message-from-mailto-url-at-point
;;;            make a draft message from a mailto: url at point

;;; usage:
;;;
;;;   -invoke mew
;;;   -try out the commands mentioned above in 'details'

;;; History:
;;
;; 0.6
;;
;; 1999-07-30
;;
;;   fixed a bug in mew-mailto-compose-message-from-mailto-url that could
;;   lead to duplicate headers in messages
;;
;; 1999-06-01
;;
;;   added compatibility garbage for xemacs -- thingatpt.el vs browse-url.el
;;
;; 1999-05-31
;;
;;   rewrote to use rfc2368.el
;;   removed a lot of functionality
;;
;; 0.5
;;
;; 1999-05-04:
;;
;;   modified the interface to mew-mailto-compose-message-from-mailto-url
;; as suggested by Umekichi <umekichi@bu.iij4u.or.jp>.  see the interface
;; to browse-url-mail in browse-url.el for details.

;; how should we handle the dependecy on mew?
;; doing the following seems to have catastrophic effects on my machine :-(
;(require 'mew)

;; will this work?
(eval-when-compile 
  (require 'mew))

(defconst mew-mailto-version "mew-mailto.el 0.6")

;; use rfc2368 support -- should be useable for things other than mew too
(require 'rfc2368)

;; yucky compatibility stuff...
(if (and (string-match "^XEmacs \\([0-9.]+\\)" (emacs-version))
	 (< (string-to-int (match-string 1 (emacs-version))) 21))
    ;; for xemacs versions less than 21, use browse-url.el
    (progn
      (require 'browse-url)
      (fset 'mew-mailto-url-at-point 
	    'browse-url-url-at-point))
  ;; for everything else, use thingatpt.el
  (progn
    (require 'thingatpt)
    (fset 'mew-mailto-url-at-point
	  (lambda () 
	    (thing-at-point 'url)))))
	   
(defun mew-mailto-compose-message-from-mailto-url (url &optional dummy)
  "Compose a messsage from URL.  The optional second argument, DUMMY, exists
to match the interface provided by browse-url-mail -- DUMMY does not do
anything."
  (interactive "sURL: ")
  (if (string-match rfc2368-mailto-regexp url)
      (let* ((other-headers (rfc2368-parse-mailto-url url))
	     ;; the reason i'm bothering to extract To and Subject is
	     ;; because i want them to appear at reasonable places in
	     ;; the headers (not in some random order) -- i'll need to
	     ;; do the same for any other headers that work that way too...
	     (to (cdr (assoc-ignore-case "to" other-headers)))
	     (subject (cdr (assoc-ignore-case "subject" other-headers))))

	;; if these are not removed from other-headers, we will get duplicates
	(remove-alist 'other-headers "To")
	(remove-alist 'other-headers "Subject")

	;; mew doesn't handle specifying a message body via
	;; mew-user-agent-compose yet -- rms said he would support
	;; specifying a body via the compose-mail interface after
	;; emacs 20.4 is released
	(mew-user-agent-compose to subject other-headers))
    (message "Not a mailto: url.")))
	  
;; prepare a message from a mailto: url at point
(defun mew-mailto-compose-message-from-mailto-url-at-point ()
  "Compose a message from a mailto url found at point."
  (interactive)
  (let ((url (mew-mailto-url-at-point)))
    (if (string-match rfc2368-mailto-regexp url)
	(mew-mailto-compose-message-from-mailto-url url)
      ;; tell the user that we didn't find a mailto: url at point
      (message "No mailto: url detected at point."))))
      
;; since this will be used via 'require'...
(provide 'mew-mailto)

;; end of mew-mailto.el