;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; PGP 2.2+ encryption/decryption routines for Mail/Rmail/MH modes.
;;
;; Copyright 1992, 1993, and 1994 by Gray Watson and Jack Repenning
;;
;; $ClearCase: pgp.el@@/main/149 $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LCD Archive Entry:
;; pgp|Jack Repenning|jackr@sgi.com|
;; Pretty Good Privacy Version 2.2+ Interface|
;; $Date: 1995/02/27 $|$ClearCase: pgp.el@@/main/149 $|~/interfaces/pgp.el.tar.gz|
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
;; BBDB-related routines used by pgp.el
;; 
;; These need to be here, as some are invocations of bbdb macros,
;; which won't byte-compile properly for non-bbdb users.
;;

(require 'bbdb)
(require 'bbdb-com)


;;
;; Look up KEY in the bbdb, return its pgp-bbdb-note (i.e., the key
;; ID), if any.
;;
(defun pgp/bbdb-note-for-key (key)
  "Search the bbdb for a pgp-bbdb-notes value for key"
  (save-excursion
    (let (rec)
      (set-buffer bbdb-buffer-name)
      (if (and (setq rec (pgp-bbdb-net (concat "^" key)))
	       (setq rec (bbdb-record-getprop (car rec)
					    pgp-bbdb-notes)))
	  (list rec)
	(list key)))))

;;
;; Look up a key in the email part of bbdb
;;

(defun pgp-bbdb-net (key)
  (interactive "sRegular expression: ")
  (require 'bbdb-com)
  (bbdb-search (bbdb-records) nil nil key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pick up PGP key ID for sender of current message, add it to BBDB.
;;
(defun pgp/bbdb-set-key-id (arg)
  "Ask pgp for key id for current message's sender (with ARG,
recipient), add it to BBDB."
  (interactive "P")
  (if pgp-bbdb-notes
      (save-window-excursion
	(require 'bbdb-com)
	(cond
	 ((and (eq pgp-mode 'mh) (eq major-mode 'mh-folder-mode))
	  ;; get to the window with the msg
	  ;; armour against bbdb/mh other-window confusion
	  (if (and (boundp 'bbdb-use-pop-up) 
		   bbdb-use-pop-up
		   (stringp bbdb-buffer-name))
	      (delete-windows-on bbdb-buffer-name))
	  (other-window 1))
	 (t
	  t)
	 )
	(let* ((uuid (pgp-key-lookup (if (not arg) "From")))
	       (user (nth 0 uuid))
	       (uid (nth 1 uuid)))
	  (pgp-command-on-region
	   (point-min) (point-min)
	   (format "+batchmode -fkv '%s@'" user))
	  (pop-to-buffer "*Shell Command Output*")
	  (goto-char (point-min))
	  (if (pgp-ephemeral-display-buffer)
	      nil
	    (goto-char (point-min))
	    (cond
	     ((save-excursion (re-search-forward "bits/keyID *Date" nil t))
	      ;; 2.3-2.7 format
	      (re-search-forward "^pub\\s +[0-9]+/\\([0-9A-Z]+\\)" nil t))
	     ((save-excursion (re-search-forward "Date *keyID/bits"
						 nil t))
	      ;; 2.9 format
	      (re-search-forward
	       "^PUB +[0-9]+/[0-9]+/[0-9]+ +\\([0-9A-Z]+\\)" nil t))
	     (t
	      (error "PGP: Unexpected PGP output format.")))
	    (let ((key (concat
			"0x"
			(buffer-substring (match-beginning 1)
					  (match-end 1))))
		  (user (concat user
				(if (string-match "@" user) "" "@"))))
	      (set-buffer bbdb-buffer-name)
	      (bbdb-annotate-message-sender user t)
	      (bbdb user nil)
	      (let ((brrn (bbdb-record-raw-notes (bbdb-current-record t))))
		(if (and (consp brrn)
			 (assq pgp-bbdb-notes brrn))
		    (bbdb-record-putprop (bbdb-current-record t)
					 pgp-bbdb-notes key)
		  (bbdb-insert-new-field pgp-bbdb-notes key))))))
	)))

;;;;
(provide 'pgp-bbdb)
