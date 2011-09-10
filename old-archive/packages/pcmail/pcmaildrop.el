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

;;;; mail drop-specific functions: transfer a mail drop's contents to a 
;;;; folder buffer, transform its message headers to RFC-822 format

;;;; global variables

;;; system-defined globals

(defvar pcmail-primary-folder-name (downcase (user-login-name))
  "The name of your primary folder.  New mail always arrives here.")

(defvar pcmail-last-mail-drop-type nil
  "Name of last mail drop type given to the pcmail-load-mail-drop command.")

;;; utility functions

(defun pcmail-read-mail-drop (folder-name drop-list)
  "Transfer new mail from mail drops to a specified folder.
Args: (folder-name drop-list)
 Get mail from the mail drops in drop-list, appending it to FOLDER-NAME.  
Update all message vectors and auto-pigeonhole messages as necessary.
Assume folder-name is current buffer.  Leave buffer widened."
  (let ((opoint)
	(omsgs pcmail-total-messages))
    (widen)
    (setq opoint (point-max))
    (unwind-protect
	(mapcar 'pcmail-insert-mail-drop-contents drop-list)
      (pcmail-set-message-vectors opoint)
      (pcmail-set-nmessages folder-name pcmail-total-messages)
      (pcmail-change-in-folder-list folder-name pcmail-total-messages))
    (unwind-protect
	(progn
	  (and pcmail-pigeonhole-hook
	       (let ((n (1+ omsgs)))
		 (while (<= n pcmail-total-messages)
		   (funcall pcmail-pigeonhole-hook n)
		   (setq n (1+ n))))))
      (cond ((> pcmail-total-messages omsgs)
	     (and pcmail-read-mail-drop-hook
		  (funcall pcmail-read-mail-drop-hook pcmail-total-messages
			   (- pcmail-total-messages omsgs)))
	     (pcmail-save-buffer))))
    (- pcmail-total-messages omsgs)))

(defun pcmail-insert-mail-drop-contents (mail-drop)
  "Insert contents of specified mail drop into the current buffer
Args: (mail-drop)
  Using MAIL-DROP's insert-function property, insert MAIL-DROP's
contents into the current buffer and convert the contents to folder format."
  (let ((tofile) (insert-fn) (opoint) (newmsgs 0)
	(insert-info)
	(make-backup-files (and make-backup-files (buffer-modified-p)))
	(buffer-read-only nil))
    (or (setq insert-fn (get mail-drop 'insert-function))
	(error "Missing transfer function for mail drop type %s" mail-drop))
    (message "Checking %s..." mail-drop)
    (setq tofile (funcall insert-fn mail-drop))
    (cond ((and tofile (file-exists-p tofile))
	   (setq opoint (goto-char (point-max))
		 insert-info (insert-file-contents tofile))
	   (goto-char (point-max))

	   ; if we inserted mail and the mail didn't end in a newline, add one
	   (or (zerop (nth 1 insert-info))
	       (= (preceding-char) ?\n)
	       (insert ?\n))
	   (and (get mail-drop 'log-mail-drop)
		(copy-file tofile
			   (expand-file-name (get mail-drop 'log-mail-drop)
					     pcmail-directory) t))
	   (condition-case nil
	       (delete-file tofile)
	     (file-error nil))
	   (setq newmsgs
		 (pcmail-convert-region-to-folder-format mail-drop opoint
						  (point-max)))))
    (message "Checking %s...done (%s new message%s)"
	     mail-drop (if (zerop newmsgs) "no" (int-to-string newmsgs))
	     (pcmail-s-ending newmsgs))))

(defun pcmail-convert-region-to-folder-format (mail-drop start end)
  "Convert region from native mail drop format to foler-specific format.
Args: (mail-drop start end)
  Convert the messages between buffer positions START and END from native
mail drop format to folder-specific format.  The processing is two step: 
converting the message header to RFC822 format, and then converting the
RFC822 message to folder-format.  There are per-mail-drop conversion functions
performing general and site-specific conversion to RFC822 format.
Site-specific processing is used in Oracle's environment to clean up ugly 
and redundant message headers.  There are also per-folder-format conversion
functions to perform any additional processing to convert RFC822 text into
a message which can be stored in the folder.

All processing functions assume the buffer is narrowed from the current message
forward, and that point is at the start of the first message to be processed."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((case-fold-search t)
	    (header-proc-fn (get mail-drop 'header-processing-function))
	    (msg-start-regexp (get mail-drop 'msg-start-regexp))
	    (newmsgs 0))
	(cond ((not header-proc-fn)
	       (message "Missing header-processing property in mail drop %s" 
			mail-drop)
	       (ding)
	       (sit-for 1))
	      ((not msg-start-regexp)
	       (message "Missing msg-start-regexp property in mail drop %s" 
			mail-drop)
	       (ding)
	       (sit-for 1)))
	(while (not (eobp))
	  (cond ((and header-proc-fn 
		      msg-start-regexp 
		      (looking-at msg-start-regexp))
		 (funcall header-proc-fn mail-drop))
		(t
		 (pcmail-process-unknown-message mail-drop)))
	  (and (zerop (% (setq newmsgs (1+ newmsgs)) pcmail-progress-interval))
	       (message "Checking %s...%d" mail-drop newmsgs))
	  (narrow-to-region (point) (point-max)))
	newmsgs))))

;;; default header processing routine

(defun pcmail-process-unknown-message (mail-drop)
  "Process a message of unknown type.
  Args: none
This routine is called when there is no match for a mail drop message-begin
regular expression.  Assumes the buffer is narrowed from point to end of 
buffer."
  (insert "From pcmail " (current-time-string) 
	  "\nDate: " (pcmail-todays-date) "\n"
	  "From: \"The Mail Reader\" <pcmail>\n"
	  "To: " pcmail-primary-folder-name "\n"
	  "Subject: Could not convert this message from "
	  (prin1-to-string mail-drop) " format to " 
	  (prin1-to-string pcmail-folder-format) " format"
	  pcmail-header-delim)
  (goto-char (point-min))
  (pcmail-convert-message-to-folder-format 'spool-mail-drop))

(provide 'pcmaildrop)
