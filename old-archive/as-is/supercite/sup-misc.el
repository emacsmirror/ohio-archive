;; sup-misc.el -- Version 2.0

;; This file contains miscellaneous support functions for the
;; Supercite 2.0 package.  Things like non-hooked, overload support
;; for RMAIL, mh-e and other `unconforming' email readers.  Also,
;; other functions that have been contributed that aren't strictly
;; supercite's responsibility, but help make things easier for
;; supercite users.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.

;; Some of this software was written as part of the supercite author's
;; official duty as an employee of the United States Government and is
;; thus in the public domain.  You are free to use that particular
;; software as you wish, but WITHOUT ANY WARRANTY WHATSOEVER.  It
;; would be nice, though if when you use any of this code, you give
;; due credit to the author.

;; Other parts of this code were written by other people.  Wherever
;; possible, credit to that author, and the copy* notice supplied by
;; the author are included with the functions.

;; ======================================================================
;; Author (unless stated otherwise):
;;
;; NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                         and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899

;; Want to be on the Supercite mailing list?
;;
;; Send articles to supercite@cme.nist.gov or uunet!cme-durer!supercite
;; Send administrative queries/requests to supercite-request@cme.nist.gov
;;	or uunet!cme-durer!supercite-request

(provide 'sup-misc)

;; ======================================================================
;; configure overloading
;;
(defvar sc-overload-rnewspost nil
  "Set to non-nil to overload rnewspost.el diffs.")
(defvar sc-overload-sendmail nil
  "Set to non-nil to overload sendmail.el diffs.")
(defvar sc-overload-mh-e nil
  "Set to non-nil to overload mh-e diffs.")
(defvar sc-use-version1-interface nil
  "Set to non-nil to use version 1.11 interface.")
(defvar sc-load-rmail-extensions nil
  "Set to non-nil to load Khalid's rmail extensions.")


(if sc-overload-rnewspost (progn

;; ======================================================================
;; overlay for diffs to rnewspost.el
;;
(defun news-reply-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (mail-yank-original arg)
  (exchange-point-and-mark)
  ;; added 14-Jun-1989 baw: warsaw@cme.nist.gov, uunet!cme-durer!warsaw
  ;; generalized, hookified citations
  (run-hooks 'news-reply-header-hook))
 
;; added 14-Jun-1989 baw: warsaw@cme.nist.gov, uunet!cme-durer!warsaw
;; generalized, hookified citations
(defvar news-reply-header-hook
  '(lambda ()
	 (insert "In article " news-reply-yank-message-id
			 " " news-reply-yank-from " writes:\n\n"))
  "Hook for inserting a header at the top of a yanked message.")


)) ;; end-if-overload-rnewspost
(if sc-overload-sendmail (progn

;; ======================================================================
;; overlay for diffs to sendmail.el
;;
(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  ;; mod 28-Jul-1989 baw: warsaw@cme.nist.gov, uunet!cme-durer!warsaw
	  ;; generalized, hookified citations
	  (run-hooks 'mail-yank-hooks))
	(exchange-point-and-mark)
	(if (not (eolp)) (insert ?\n)))))

;; added 28-Jul-1989 baw: warsaw@cme.nist.gov, uunet!cme-durer!warsaw
;; generalized, hookified citations
(defvar mail-indention-spaces 3
  "Set to number of spaces to indent when yanking a reply.")

;; added 28-Jul-1989 baw: warsaw@cme.nist.gov, uunet!cme-durer!warsaw
;; generalized, hookified citations
(defvar mail-yank-hooks
  '(lambda ()
     (indent-rigidly (point) (mark) mail-indention-spaces))
  "Hook to run citation function.  Expects POINT and MARK to be set to the
region to cite.")


)) ;; end if-overload-sendmail
(if sc-overload-mh-e (progn

;; ======================================================================
;; mh-e diffs supplied by Mark D. Baushke
;;      Internet: mdb@ESD.3Com.COM
;;      UUCP:     {3comvax,auspex,sun}!bridge2!mdb
;;
;; mod 22-Sep-1989 mdb:: mdb@ESD.3Com.COM, {3comvax,auspex,sun}!bridge2!mdb
;; modified to conditionally use mhl instead of always using it.
(defun mh-insert-letter (prefix-provided folder msg)
  "Insert a message from any folder into the current letter.
Removes the message's headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
If (optional) prefix argument provided, do not indent and do not delete
headers.
Leaves the mark before the letter and point after it."
  (interactive
   (list current-prefix-arg
	 (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
	 (read-input (format "Message number%s: "
			     (if mh-sent-from-msg
				 (format " [%d]" mh-sent-from-msg)
				 "")))))
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((start (point-min))
	  (msg-filename (mh-expand-file-name msg (mh-expand-file-name folder))))
      (if (equal msg "") (setq msg (format "%d" mh-sent-from-msg)))
      (if mhl-formfile
	  (if (stringp mhl-formfile)
	      (mh-exec-lib-cmd-output
	       "mhl" "-nobell" "-noclear" "-form" mhl-formfile msg-filename)
	    (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear" msg-filename))
	(insert-file-contents msg-filename t))
      (when (not prefix-provided)
	    (mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
	    (set-mark start)		; since mh-clean-msg-header moves it
	    (mh-insert-prefix-string mh-ins-buf-prefix)))))

;; mod 7-Sep-1989 mdb: mdb@ESD.3Com.COM, {3comvax,auspex,sun}!bridge2!mdb
;; generalized, hookified citations
(defun mh-insert-prefix-string (ins-string)
  ;; Preface each line in the current buffer with STRING.
  (setq mh-ins-string ins-string)
  (save-excursion
    (set-mark (point-max))
    (goto-char (point-min))
    (run-hooks 'mh-yank-hooks)))

;; added 7-Sep-1988 mdb: mdb@ESD.3Com.COM, {3comvax,auspex,sun}!bridge2!mdb
;; generalized, hookified citations
(defvar mh-ins-string nil
  "A temporary set by mh-insert-prefix prior to running mh-yank-hooks .")

;; added 7-Sep-1988 mdb: mdb@ESD.3Com.COM, {3comvax,auspex,sun}!bridge2!mdb
;; generalized, hookified citations
(defvar mh-yank-hooks
  '(lambda ()
     (save-excursion
       (goto-char (point))
       (or (bolp) (forward-line 1))
       (while (< (point) (mark))
	 (insert mh-ins-string)
	 (forward-line 1))))
  "Hook to run citation function. Expects POINT and MARK to be set to
the region to cite.")

)) ;; end overload mh-e
(if sc-use-version1-interface (progn

;; ======================================================================
;; backward compatibility for superyank 1.11 (interactive version)
;; author: Barry A. Warsaw (warsaw@cme.nist.gov, uunet!cme-durer!warsaw)
;;
(defun sc-yank-original (arg)
  "Interactive version of the rewritten mail-yank-original function.
This is provided for backward compatibility with version 1.x but
should not be used any more.  Instead use the hook-ized version
`sc-cite-original'. Support for this function ends with this release."
  (interactive "P")
  (if mail-reply-buffer
      (let* ((sc-confirm-always-p (if (consp arg)
				      t
				    sc-confirm-always-p)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(sc-cite-original))))


)) ;; end use version1 interface
(if sc-load-rmail-extensions (progn

;; ======================================================================
;; load rmail extensions provided by Khalid Satar
;; Khalid Sattar                   JANET  : admin@uk.ac.exeter.cs
;; Computer Science Dept.          UUCP   : admin@expya.uucp
;; University of Exeter            BITNET : admin%uk.ac.exeter.cs@ukacrl
;; Exeter, UK.                     Tel    : +44 392 264062
;;
(defun mail-rmail-next-mesg (arg)
  "Pop RMAIL window and go the next unread message.  If ARG is
specified then go ARG messages forward/backward"
  (interactive "p")
  (if (not mail-reply-buffer)
      (error "You need to compose mail via rmail for this to work")
    (let ((mail-buffer (current-buffer)))
      (switch-to-buffer-other-window mail-reply-buffer)
      (rmail-next-undeleted-message arg)
      (switch-to-buffer-other-window mail-buffer))))
     
(defun mail-rmail-previous-mesg (arg)
  "Pop RMAIL window and go the previous unread message.  If ARG is
specified then go ARG messages backward"
  (interactive "p")
  (mail-rmail-next-mesg (- arg)))


)) ;; end rmail extensions
