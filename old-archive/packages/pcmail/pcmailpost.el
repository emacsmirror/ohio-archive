;;;; GNU-EMACS PCMAIL mail reader

;;  Written by Mark L. Lambert
;;
;;  Internet: markl@us.oracle.com 
;;  USMail:   Oracle Corporation
;; 	      500 Oracle Parkway, box 659410
;;	      Redwood Shores CA 94065
;;  voice:    (415) 506 2912
;;  FAX:      (415) 506 7226

;; Copyright (C) 1989, 1993 Mark L. Lambert

;; This file is not officially part of GNU Emacs, but is being
;; donated to the Free Software Foundation.  As such, it is
;; subject to the standard GNU-Emacs General Public License,
;; referred to below.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defvar pcmail-posting-address "nntp-poster" 
	"Mail address of NNTP posting program")

(defun pcmail-inews ()
  "Command to begin editing a news article to be posted.  Note that this
sends mail to the address specified in pcmail-posting-address.  This 
defaults to \"nntp-poster\", which should be a sendmail alias for a 
program that delivers the message to an appropriate NNTP (network news 
transfer protocol) server and from there onto the usenet."
  (interactive)
  (or (y-or-n-p "Are you sure you want to post to the net? ")
      (error "Aborted."))
  (pop-to-buffer "*post-news*")
  (mail-mode)
  (erase-buffer)
  (pcmail-news-setup pcmail-posting-address nil nil nil nil))

(defun pcmail-news-setup (to subject in-reply-to newsgroups replybuffer)
  "Args: (to subject in-reply-to newsgroups replybuffer)
Set up the mail buffer and run 'news-setup-hook."
  (setq mail-reply-buffer replybuffer)
  (let ((mail-setup-hook nil))
    (if (null to)
	;; this hack is needed so that inews wont be confused by 
	;; the fcc: and bcc: fields
	(let ((mail-self-blind nil)
	      (mail-archive-file-name nil))
	  (mail-setup to subject in-reply-to nil replybuffer)
	  (beginning-of-line)
	  (kill-line 1)
	  (goto-char (point-max)))
      (mail-setup to subject in-reply-to nil replybuffer))
    (goto-char (point-max))
    (re-search-backward "^To:" (point-min) 'move)
    (forward-line)
    (insert "Reply-to: \n")
    (insert "Newsgroups: " (or newsgroups "") "\n")
    (run-hooks 'news-setup-hook)))
