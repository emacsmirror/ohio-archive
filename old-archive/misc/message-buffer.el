;;; message-buffer.el --- log messages in their own buffer

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Ray Nickson <nickson@cs.uq.oz.au>,
;;         Michael Ernst <mernst@theory.lcs.mit.edu>
;; Created: 22 Sep 1994
;; Last Modified: 3 Oct 1994
;; Keywords:

;; This file is distributed under the same conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This code makes messages be logged in the *Messages* buffer in addition
;; to being displayed in the echo area.  It doesn't work when `message' is
;; called from C code, only when it's called from Emacs Lisp.

;; Put the following line (without the semicolons) in your .emacs file:
;;   (autoload 'message-buffer "message-buffer" "Log messages in a buffer" t)
;; Then, do  M-x message-buffer RET  to enable message logging.
;; Do  C-u M-x message-buffer RET  to disable message logging.

;; This code only works in Emacs 19.  For Emacs 18, use message-buffer-18.el.

;; This code is originally by Ray Nickson.
;; Michael Ernst added various cleanups, including
;;  * consistent variable names
;;  * new variables message-buffer-at-end-p, message-buffer-timestamp-p
;;  * additional forms in message-buffer-non-saved-messages
;;  * make message-buffer-non-saved-messages a regexp; no mapconcat per message
;;  * save and restore match data
;;  * provide statement
;;  * message-buffer function for enabling/disabling logging
;;  * compliant formatting and headers
;;  * LCD archive entry

;; LCD Archive Entry:
;; message-buffer|Ray Nickson, Michael Ernst|mernst@theory.lcs.mit.edu|
;; Log messages and errors in their own buffer|
;; 03-Oct-1994|1.0|~/misc/message-buffer.el.Z|

;;; Code:

(defvar message-buffer-max-size 10000
  "*Maximum size to let the *Messages* buffer grow to.
If zero or nil, don't write a *Messages* buffer at all.")

(defvar message-buffer-at-end-p nil
  "*Non-nil if messages should be inserted into the end of the messages buffer.")

(defvar message-buffer-timestamp-p t
  "*Non-nil if timestamps should be inserted into the messages buffer.")

(defvar message-buffer-non-saved-messages
  (mapconcat 'identity
	     '("^Undo!$"
	       "^Mark set$"
	       "^\\(Regexp\\|Failing \\)?I-Search\\( backward\\)?:"
	       "^Mark saved where search started$"
	       "^NNTP: Reading"
	       "^Commands: d, s, x; 1, 2, m, u, q; delete; ~;  \\? for help\\.$"
	       "^Type C-x 4 b RET to restore the other window\\.  M-C-v to scroll the help\\.$"
	       "^Type C-x 1 to remove help window\\.  M-C-v to scroll the help\\.$")
	     "\\|")
  "*Regexp matching messages not saved in the messages buffer.")

(defun message-buffer (&optional arg)
  "Enable logging messages in their own buffer.
With arg, turn logging on if and only if arg is positive."
  (interactive "P")
  (if arg
      (progn
	;; Do this *before* disabling, so it gets logged.
	(message "Message logging disabled.")
	(ad-disable-advice 'message 'after 'message-log)
	(ad-disable-advice 'error 'around 'message-log)
	(ad-activate 'message)
	(ad-activate 'error))
    (progn
      (ad-enable-advice 'message 'after 'message-log)
      (ad-enable-advice 'error 'around 'message-log)
      (ad-activate 'message)
      (ad-activate 'error)
      (message "Message logging enabled."))))

(defadvice message (after message-log first disable)
  "Log the message in the *Messages* buffer unless it matches
`message-buffer-nonsaved-messages'."
  (log-message ad-return-value))

(defadvice error (around message-log first disable)
  "Log the message in the *Messages* buffer unless it matches
`message-buffer-nonsaved-messages'."
  (condition-case data
      ad-do-it
    (error (log-message (car (cdr data)))
	   (signal (car data) (cdr data)))))

(defun log-message (msg)
  "Log MSG in the *Messages* buffer unless it matches `message-nonsaved-messages'."
  (if (not (or (not message-buffer-max-size)
	       (zerop message-buffer-max-size)
	       (null msg)
	       (string-equal "" msg)
	       (let ((old-match-data (match-data)))
		 (prog1
		     (string-match message-buffer-non-saved-messages msg)
		   (store-match-data old-match-data)))))
      (save-excursion
	(set-buffer (get-buffer-create "*Messages*"))
	(if (> (buffer-size) message-buffer-max-size)
	    (if message-buffer-at-end-p
		(delete-region (point-min) (/ message-buffer-max-size 2))
	      (delete-region (/ message-buffer-max-size 2) (point-max))))
	(if message-buffer-at-end-p
	    (goto-char (point-max))
	  (goto-char (point-min)))
	(if message-buffer-timestamp-p
	    (insert (current-time-string) ": "))
	(insert msg "\n")))
  msg)

(provide 'message-buffer)

;;; message-buffer.el ends here
