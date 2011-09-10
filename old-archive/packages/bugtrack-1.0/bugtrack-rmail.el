;;; bugtrack.el --- Connect bugtrack to the RMAIL mode.

;; Copyright (C) 1992, 1993 Per Cederqvist.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;; Created: 1992-07-29
;; Version: Id: bugtrack-rmail.el,v 1.13 1993/12/13 21:13:49 ceder Exp 
;; Keywords: tools, bugtracking, rolodex, bug, track

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
     
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See bugtrack.el and the doc strings.

;;; Code:

(provide 'bugtrack-rmail)

(defvar bugtrack-memorized-msg-id nil
  "Message ID. Used by Bugtrack.")


;; Internal functions

(defun bugtrack-rmail-narrow-to-headers (msg)
  "Narrow to the full headers of message number MSG.
Move point to beginning of the headers."
  (rmail-maybe-set-message-counters)
  (widen)
  (let* ((case-fold-search t)
	 (next (rmail-msgend msg)))
    (goto-char (rmail-msgbeg msg))
    (forward-line 2)
    (if (looking-at "summary-line")
	(forward-line 1))
    (cond
     ((looking-at (regexp-quote "*** EOOH ***"))
      ;; RMAIL is showing the full header. It follows *** EOOH ***.
      (forward-line 1)
      (narrow-to-region
       (point)
       (progn (search-forward "\n\n" next)
	      (match-beginning 0))))
     (t
      ;; RMAIL is showing a trimmed header. The original header
      ;; is at point, and extends until *** EOOH ***.
      (narrow-to-region
       (point)
       (progn (search-forward "\n*** EOOH ***\n" next)
	      (match-beginning 0))))))
  (goto-char (point-min)))

(defun bugtrack-insert-message-id (comment msg-id)
  "Bugtrack internal. Args: COMMENT MSG-ID"
  (if (null msg-id)
      (error "Message ID not remembered."))
  (if (string-match ":" comment)
      (error "comment must not contain any colon (:)"))
  (save-excursion
    (bugtrack-goto-free-text-start)
    (search-backward "End mails.\n")
    (let ((buffer-read-only nil))
      (insert "\t" comment ": " msg-id "\n"))))

(defun bugtrack-select-message ()
  "Bugtrack internal. Return message id of the wanted mail.
Point is moved to line after the mail that is used. Thus repeatedly
using this function will circle through the mails."
  (cond
   ;; If point is on an mail line, return that message id.
   ((and
     (< (save-excursion (goto-char (point-min))
			(search-forward "\nAssociated mails:\n")
			(point))
	(point))
     (< (point)
	(save-excursion (bugtrack-goto-free-text-start)
			(search-backward "\nEnd mails.")
			(point))))
    (prog1
	(buffer-substring
	 (save-excursion (beginning-of-line) (search-forward ": ") (point))
	 (save-excursion (end-of-line) (point)))
      (beginning-of-line 2)))
   ;; Otherwise, return the message id of the first mail.
   (t
    (prog1
	(buffer-substring
	 (progn (goto-char (point-min))
		(search-forward "\nAssociated mails:\n")
		(if (looking-at "End mails")
		    (error "There is no associated mail!"))
		(search-forward ": ")
		(point))
	 (progn (end-of-line) (point)))
      (beginning-of-line 2)))))

(defun bugtrack-hunt-message-id (msg-id rmail-buffer)
  "Bugtrack internal. Search for MSG-ID in RMAIL-BUFFER.
Display it using pop-to-buffer and return t if it is found.
Otherwise, return nil.
Return nil immediately if RMAIL-BUFFER is not in rmail mode."
  (let ((obuf (current-buffer)))
    (set-buffer rmail-buffer)
    (cond
     ((not (eq major-mode 'rmail-mode))
      (set-buffer obuf)
      nil)
     (t
      (rmail-maybe-set-message-counters)
      (let ((omin (point-min))
	    (omax (point-max))
	    (opt (point))
	    (case-fold-search t)
	    (found nil))
	(widen)
	(goto-char (point-min))
	(while (and (not found)
		    (re-search-forward (concat "\nMessage-id:[ \t]+"
					       (regexp-quote msg-id))
				       nil t)
		    (not (eobp)))
	  (let ((number 1))
	    (while (and (not found)
			(<= number rmail-total-messages))
	      (if (> (rmail-msgend number) (point))
		  (setq found number)
		(setq number (1+ number))))))
	(cond
	 (found
	  (pop-to-buffer rmail-buffer)
	  (rmail-show-message found)
	  t)
	 (t
	  (goto-char opt)
	  (narrow-to-region omin omax)
	  (set-buffer obuf)
	  nil)))))))

;; User functions in RMAIL mode

;;;###autoload
(defun bugtrack-rmail-memorize-msg-id ()
  "In RMAIL mode, memorize the msg id for Bugtrack mode."
  (interactive)
  (save-restriction
    (bugtrack-rmail-narrow-to-headers rmail-current-message)
    (setq bugtrack-memorized-msg-id
	  (mail-fetch-field "message-id")))
  (message "Remembering %s" bugtrack-memorized-msg-id))


;;;###autoload
(defun bugtrack-rmail-create-bug (bugtrack-file summary version mail-comment
						severity priority difficulty)
  "In RMAIL mode, create a Bugtrack bug report from the current mail.
Args: BUGTRACK-FILE SUMMARY VERSION MAIL-COMMENT SEVERITY PRIORITY DIFFICULTY.

BUGTRACK-FILE should be an existing Bugtrack file.
SUMMARY is a string that must not contain any colons.
VERSION is a string with the version number of buggy product.
MAIL-COMMENT is a string that is associated with this mail.
SEVERITY, PRIORITY and DIFFICULTY are decimal numbers in the range
0-9, the others are strings that should not contain any newline."
  (interactive
   (list (bugtrack-read-bug-file-name)
	 (read-from-minibuffer "Summary: ")
	 (read-from-minibuffer "Version: ")
	 (read-from-minibuffer "MAIL note: ")
	 (bugtrack-read-digit "Severity (0=unimportant, 9=important): ")
	 (bugtrack-read-digit "Priority (0=low, 9=high): ")
	 (bugtrack-read-digit "Difficulty (0=easy, 9=hard): ")))

  (let ((msg-id nil)
	(reporter nil)
	(site ""))
    (save-restriction
      (bugtrack-rmail-narrow-to-headers rmail-current-message)
      (setq msg-id (mail-fetch-field "message-id"))
      (setq reporter (or (mail-fetch-field "from")
			 (mail-fetch-field "sender")))
      (if (string-match "@[^> (]*" reporter)
	  (setq site (substring reporter (1+ (match-beginning 0))
				(match-end 0)))))
    (bugtrack bugtrack-file)
    (bugtrack-create-bug summary version
			 severity priority difficulty reporter site)
    (bugtrack-insert-message-id mail-comment msg-id)))

;;+++///An elib version of this function should exist!
(defun bugtrack-filter (predicate list &rest extra-args)
  "Apply PREDICATE to each element on LIST.
Args: PREDICATE LIST &rest EXTRA-ARGS.
Return a new list consisting of those elements that PREDICATE
returns non-nil for.

If more than two arguments are given the remaining args are
passed to PREDICATE."
  ;; Avoid recursion - this should work for LONG lists also!
  (let* ((head (cons 'dummy-header nil))
	 (tail head))
    (while list
      (if (apply predicate (car list) extra-args)
	  (setq tail (setcdr tail (list (car list)))))
      (setq list (cdr list)))
    (cdr head)))

;;;###autoload
(defun bugtrack-rmail-search-msg-id (include-closed)
  "In RMAIL mode, search for any bugs about the current mail.
If a prefix argument is given, this will also search in closed bugs."
  (interactive "P")
  (save-restriction
    (bugtrack-rmail-narrow-to-headers rmail-current-message)
    (setq bugtrack-previous-regexp
	  (regexp-quote (mail-fetch-field "message-id"))))
  (setq bugtrack-search-include-closed include-closed)
  (setq bugtrack-search-buffers
	(bugtrack-filter
	 (function (lambda (b) 
		     (set-buffer b)
		     (eq major-mode 'bugtrack-mode)))
	 (buffer-list)))
  (if (null bugtrack-search-buffers)
      (error "No bugtrack buffers to search in present."))
  (switch-to-buffer-other-window (car bugtrack-search-buffers))
  (setq bugtrack-search-buffers (cdr bugtrack-search-buffers))
  (setq bugtrack-search-start (point))
  (bugtrack-search-continue))

;; This function does not really belong in bugtrack, but, given the
;; code I needed to write for bugtrack I almost got this functionality
;; for free.

;;;###autoload
(defun bugtrack-rmail-view-commented ()
  "In RMAIL mode, view the commented mail (if it can be found).
Ask for an RMAIL file to search in, if it can not be found in any
visited RMAIL file. Signal an error if it is not found there either."
  (interactive)
  (let ((id
	 (save-restriction
	   (save-excursion
	     (bugtrack-rmail-narrow-to-headers rmail-current-message)
	     (mail-fetch-field "In-Reply-To")))))
    (if (null id)
	(error "No In-Reply-To field found."))
    (if (not (string-match "<.+>" id))
	(error "No message-id found in In-Reply-To: %s" id))
    (bugtrack-hunt-mail-or-ask
     (substring id (match-beginning 0) (match-end 0))
     (buffer-list))))

;; User functions in bugtrack mode.

;;;###autoload
(defun bugtrack-insert-memorized-id (comment)
  "In Bugtrack mode, insert the memorized RMAIL message id in the current bug.
The message ID should be memorized with bugtrack-rmail-memorize-msg-id."
  (interactive "sShort comment: ")
  (bugtrack-insert-message-id comment bugtrack-memorized-msg-id))

;;;###autoload
(defun bugtrack-display-mail ()
  "In Bugtrack mode, display associated mail."
  (interactive)
  (bugtrack-hunt-mail-or-ask (bugtrack-select-message) (buffer-list)))


(defun bugtrack-hunt-mail-or-ask (msg-id buffers)
  "Search for an email with message-id equal to MSG-ID in BUFFERS.
Only those buffers that are in RMAIL mode are actually searched.
Ask the user for the name of an RMAIL file if the message is not found
in any of the buffers. Print an error message if it is not found there
either."
  (let ((found nil))
    (while (and buffers (not found))
      (if (bugtrack-hunt-message-id msg-id (car buffers))
	  (setq found t)
	(setq buffers (cdr buffers))))
    (cond
     ((not found)
      (set-buffer (find-file-noselect
		   (read-file-name "RMAIL file to search in: "
				   nil nil t)))
      (require 'rmail)
      (rmail-mode)
      (if (not (bugtrack-hunt-message-id msg-id (current-buffer)))
	  (error "Can not find Message-Id: %s" msg-id))))))

;;; Local Variables:
;;; generated-autoload-file: "bugtrack-init.el"
;;; End:

;;; bugtrack-rmail.el ends here
