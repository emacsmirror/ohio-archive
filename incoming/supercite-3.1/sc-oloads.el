;;; sc-oloads.el --- Supercite overloading for old emacsen

;; Author: 1993 Barry A. Warsaw, Century Computing, Inc. <bwarsaw@cen.com>
;; Maintainer:    bwarsaw@cen.com
;; Created:       01-Jun-1993
;; Version:       3.1
;; Last Modified: 1993/09/21 22:00:04
;; Keywords:      supercite overloading sendmail mail news

;; sc-oloads.el revision: 3.10

;; Copyright (C) 1993 Barry A. Warsaw

;; This file is not yet part of GNU Emacs, and hopefully never will be.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file provides an overloading mechanism for interfacing
;; supercite with mail-yank-hooks and mail-citation-hook illiterate
;; Emacsen and MUAs.  This file comes unsupported since new versions
;; of Emacs, both FSF 19 and Lucid 19, as well as all known mail and
;; news readers, should support the proper interface.  If not, its
;; time to get them patched.

;; NOTE: if you are running any of the following systems, (as of
;; 24-Aug-1993) you do *not* need this file:
;;
;; VM 4.40 or later under any Emacsen.
;; MH-E 3.7 or later under any Emacsen.
;; RMAIL, GNUS, or RNEWS under FSF Emacs 19.16 or later
;; RMAIL, GNUS, or RNEWS under Lucid Emacs 19.8 or later
;; Hyperbole under any Emacsen

;; If you use any other versions, including especially FSF Emacs 18.5x
;; with RMAIL, GNUS, RNEWS or GNEWS, you must use this file to hook in
;; Supercite.  The defuns in this file are smart enought to only
;; install themselves when necessary.  Please see the accompanying
;; texinfo manual for details.

;; Want to be on the Supercite mailing list?
;;
;; Send articles to:
;;         INET: supercite@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite
;; 
;; Send administrivia (additions/deletions to list, etc) to:
;;         INET: supercite-request@anthem.nlm.nih.gov
;;         UUCP: uunet!anthem.nlm.nih.gov!supercite-request

;;; Code:

(defun sc-oload-defun (origsym oloadsym &optional force-p)
  "Overload functions with their Supercite savvy equivalents.
ORIGSYM is the symbol name of the original function and OLOADSYM is
the symbol name of the new function.  If the original symbol is not
yet bound, it will not be overloaded, unless optional FORCE-P is
non-nil.  Also, if the symbol has already been overloaded, it will not
be overloaded again."
  (let ((property 'sc-overloaded))
    (and (or force-p (fboundp origsym))
	 (not (get origsym property))
	 (progn
	   (fset origsym (symbol-function oloadsym))
	   (put origsym property property))
	 )))

(defun sc-perform-overloads ()
  "Overload the functions necessary to run Supercite in your Emacsen.
This function is smart enough to only perform overloads for those
systems that are not already Supercite savvy.  This is current as of
10-Aug-1993."
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\)" emacs-version)
      (let ((major (substring emacs-version (match-beginning 1) (match-end 1)))
	    (minor (substring emacs-version (match-beginning 2) (match-end 2)))
	    (lucidp (string-match "Lucid" emacs-version)))
	(setq major (string-to-int major)
	      minor (string-to-int minor))
	;;
	;; first lets see if we need to patch sendmail.el routines
	;;
	(cond
	 ;; people running Hyperbole should use its built-in
	 ;; overloading routines instead of Supercite's.
	 ((boundp 'hyperb:version)
	  (message "Supercite will defer to Hyperbole's overloads."))
	 ;; FSF Emacs 19.15 and Lucid Emacs 19.8 and beyond need no
	 ;; modifications.  They should work out of the box.
	 ((and (= major 19)
	       (or (and (not lucidp)
			(>= minor 15))
		   (and lucidp
			(>= minor 8))))
	  (message "Your Emacs has what it takes without Supercite's help."))
	 ;; check to see if we're running an older Emacsen that's
	 ;; already been patched.
	 ((and (boundp 'mail-citation-hook)
	       (fboundp 'mail-indent-citation))
	  (message "Your Emacs has mail-citaiton-hook support"))
	 ;; All GNU Emacs 18's, Epoch 4's, FSF Emacs 19.14 and older,
	 ;; and Lucid Emacs 19.7 and older need to be modified
	 ((or (<= major 18)
	      (and (not lucidp)
		   (<= minor 14))
	      (and lucidp
		   (<= minor 7)))
	  (message "You have an old Emacs. Performing the sendmail overloads")
	  ;; function mail-indent-citation may not ever be bound so
	  ;; we'll just force it
	  (sc-oload-defun 'mail-yank-original 'sc-mail-yank-original)
	  (sc-oload-defun 'mail-indent-citation 'sc-mail-indent-citation t))
	 ;; anything else is a mystery to us
	 (t (error "Supercite can't figure out your Emacs configuration."))
	 ) ; end-cond
	;;
	;; Now for rnewspost.el stuff.  We only need to do this for
	;; GNU Emacs 18.  All v19 Emacsen do the right thing.
	;;
	(if (= major 19)
	    ()
	  (sc-oload-defun 'news-reply-yank-original
			  'sc-news-reply-yank-original))
	;;
	;; GNEWS stuff should *always* get overloaded as far as I know
	;;
	(sc-oload-defun 'reply-yank 'sc-gnews-reply-yank)
	(sc-oload-defun 'group-reply-yank 'sc-group-reply-yank)
	(sc-oload-defun 'group-follow-yank 'sc-group-follow-yank)
	;;
	;; That just about covers everything as far as I know
	;;
	)))


;; sendmail.el overload functions. This is the heart of supercite
;; conformance by packages which rely on distribution emacs elisp.
;; Note that this now uses the new mail-citation-hook variable as the
;; hook interface.  This should be safe for GNU 18.5x and Lucid 19.6
;; Emacsen.
(defvar mail-yank-prefix nil
  "*Prefix insert on lines of yanked message being replied to.
nil means use indentation.")
(defvar mail-indentation-spaces 3
  "*Number of spaces to insert at the beginning of each cited line.
Used by `mail-yank-original' via `mail-indent-citation'.")
(defvar mail-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and mail agents should no longer use it.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), a default action is taken
instead of no action.")

(defun sc-mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    (mail-yank-clear-headers start (mark t))
    (if (null mail-yank-prefix)
	(indent-rigidly start (mark t) mail-indentation-spaces)
      (save-excursion
	(goto-char start)
	(while (< (point) (mark t))
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

(defun sc-mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (goto-char start)
	  (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
					   mail-indentation-spaces)))
	    (if mail-citation-hook
		(run-hooks 'mail-citation-hook)
	      (if mail-yank-hooks
		  (run-hooks 'mail-yank-hooks)
		(mail-indent-citation)))))
	(let ((zmacs-regions nil))
	  (exchange-point-and-mark))
	(if (not (eolp)) (insert ?\n)))))


;; rnewspost.el overload functions.  Not strictly necessary for supercite
;; to work but it reduces the amount of manual cleaning the user has to
;; do for GNUS and other news readers.
(defun sc-news-reply-yank-original (arg)
  "Supercite version of `news-reply-yank-original'.
Insert the message being replied to in the reply buffer.  Puts point
before the mail headers and mark after body of the text.  Calls
`mail-yank-original' to actually yank the message into the buffer and
cite text.  

If `mail-yank-original' is not overloaded by supercite, each nonblank
line is indented ARG spaces (default 3).  Just `\\[universal-argument]'
as ARG means don't indent and don't delete any header fields."
  (interactive "P")
  (mail-yank-original arg)
  (exchange-point-and-mark)
  (run-hooks 'news-reply-header-hook))

(defvar news-reply-header-hook
  '(lambda ()
     (insert "In article " news-reply-yank-message-id
	     " " news-reply-yank-from " writes:\n\n"))
  "*Hook for inserting a header at the top of a yanked message.")


;; gnews overloads supplied by:
;; Piet* van Oostrum, Dept of Computer Science, Utrecht University,
;; Padualaan 14, P.O. Box 80.089, 3508 TB Utrecht, The Netherlands.
;; Telephone: +31 30 531806   Uucp:   uunet!mcsun!ruuinf!piet
;; Telefax:   +31 30 513791   Internet:  piet@cs.ruu.nl   (*`Pete')
;;
;; His overload functions were provided in a mail message dated
;; 5-Oct-1990, in which he also gives this caveat:
;; 
;; I use Gnews as my newsreader. I hacked a few definitions to use
;; supercite with gnews. The definitions are a hack in that you can't
;; easily customize them to get the original behaviour, but anyway,
;; here are they:

(defvar gnews-reply-header-hook nil
  "*Hook for inserting a header at the top of a yanked message.")

(defun sc-gnews-reply-yank (arg)
  "Supercite version of original `gnews-reply-yank' function."
  (interactive "P")
  (open-line 2)
  (delete-blank-lines)
  (forward-char 1)
  (mail-yank-original arg)
  (exchange-point-and-mark)
  (run-hooks 'gnews-reply-header-hook))

(defun sc-group-reply-yank (pfx arg)
  "Reply by e-mail to current article with original article inserted.
With non-nil prefix argument PFX, set up the current article for e-mail
forwarding."
  (interactive "P\np")
  (if pfx (group-forward-article)
    (if (group-reply)
	(progn
	  (forward-paragraph)
	  (sc-gnews-reply-yank 1)
	  (run-hooks 'group-reply-hook)
	  (set-buffer-modified-p nil)))))

(defun sc-group-follow-yank (pfx arg)
  "Follow up to current article with that article inserted.
Indented using the value of `reply-prefix'. Otherwise identical to
`group-follow', which see."
  (interactive "P\np")
  (let ((end (< article-final article-current))
	file goal-column)
    (if end (setq file (read-file-name "Include file: " nil "" t)
		  file (if (string< "" file) (expand-file-name file))))
    (if (group-follow pfx arg)
	(progn
	  (if file
	      (progn
		(goto-char (point-max))
		(insert ?\n?\n)
		(insert-file file)
		(if (looking-at "^\\(Newsgroups\\|Path\\|From\\): ")
		    (delete-region 1 (point)))))
	  (forward-paragraph)
	  (if (not file) (open-line 1))
	  (forward-line 1)
	  (if (not end)
	      (sc-gnews-reply-yank 1))
	  (run-hooks 'group-follow-hook)
	  (set-buffer-modified-p nil)))))


;; useful stuff
(provide 'sc-oloads)

;;; sc-oloads.el ends here
