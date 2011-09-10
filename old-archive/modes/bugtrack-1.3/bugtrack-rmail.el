;;; bugtrack.el --- Connect bugtrack to the RMAIL mode.

;; Copyright (C) 1992, 1993, 1994 Per Cederqvist.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;; Created: 1992-07-29
;; Version: Id: bugtrack-rmail.el,v 1.21 1994/01/06 21:42:01 ceder Exp 
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

(require 'bugtrack)
(provide 'bugtrack-rmail)

(defvar bugtrack-rmail-rules nil
  "*List of rules to find default Bugtrack file for mail messages.
Each rule is tried in order, and the first which matches is used
as a default when bugtrack-rmail-read-bug-file-name prompts for a
Bugtrack buffer.

Each rule is a list of three elements.  The first is a regexp.  The
rule matches if the email contains a match for it.  The second element
is the default directory, and the third is the default file name.  If
the second or third elements are ``nil'' they default to
bugtrack-default-dir and \"BUGS\".


Example use:
    (setq bugtrack-default-dir \"~/bugs/\")
    (setq bugtrack-rmail-rules '((\"[bB]ugtrack\" \"~/bugtrack/\" \"BUGS\")
				 (\"[pP]cl-cvs\" \"~/pcl-cvs/\" \"BUGS\")
				 (\"cvs\" nil \"cvs\")))")

(defvar bugtrack-rmail-subject-cookie-map nil
  "*Map of magic subject cookies.
Bugtrack can insert a magic cookie in the subject line of a mail
whenever you create a bug report from that mail.  The cookie typically
looks something like ``##emacs-bug-18##''.  If you send a reply to the
mail the cookie will be included in the subject.  When you get a reply
to the reply Bugtrack can use the magic cookie to find the bug.

This variable should be set to a list of associations.  Each
association is a list whose first element is a path to a Bugtrack
file, and whose second element is a string to include in the cookie.
The string should not contain SPC or TAB.
Example:
	(setq bugtrack-rmail-subject-cookie-map
	      '((\"~/bugtrack/BUGS\" \"Bugtrack\")
		(\"~/bugs/emacs\" \"emacs\")))")

(defvar bugtrack-auto-mail-file-list nil
  "*List of RMAIL files for Bugtrack to load when searching for a message.")

(defvar bugtrack-memorized-msg-id nil
  "Message ID. Used by Bugtrack.")

(defvar bugtrack-rmail-create-bug-where nil
  "*Determine where bugtrack-rmail-create-bug should display the Bugtrack file.
Permissible values are:
    nil     Replace the RMAIL window with the Bugtrack buffer (default)
    'window Display the Bugtrack buffer in another window
    'frame  Display the Bugtrack buffer in another frame")


;;;; Internal functions

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

(defun bugtrack-rmail-extract-cookies ()
  "Return a list of all bugs mentioned in the Subject: of the current mail.
This looks for Bugtrack cookies of the form ##BUGFILE-IDENTIFIER-bug-BUGID##.
BUGFILE-IDENTIFIER is looked up in bugtrack-rmail-subject-cookie-map
to find a corresponding Bugtrack file. BUGID is the number of the bug.
Cookies which cannot be looked up using bugtrack-rmail-subject-cookie-map
are ignored."
  (let ((res nil)
	(subject (mail-fetch-field "Subject"))
	(pos 0)
	cookie)
    (while (string-match "##\\([^ \t]+\\)-bug-\\([0-9]+\\)##" subject pos)
      (setq cookie (substring subject (match-beginning 1) (match-end 1)))
      (mapcar
       (function
	(lambda (map)
	  (if (string= (car (cdr map)) cookie)
	      (setq res
		    (cons
		     (list (expand-file-name (car map))
			   (string-to-int 
			    (substring subject (match-beginning 2)
				       (match-end 2))))
		     res)))))
       bugtrack-rmail-subject-cookie-map)
      (setq pos (match-end 0)))
    (nreverse res)))

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

(defun bugtrack-all-bugtrack-buffers ()
  "Return a list of all currently loaded Bugtrack buffer."
  (bugtrack-filter
   (function (lambda (b) 
	       (set-buffer b)
	       (eq major-mode 'bugtrack-mode)))
   (buffer-list)))

(defun bugtrack-assert-rmail-mode ()
  "Signal an error unless the current major mode is rmail-mode."
  (if (not (eq major-mode 'rmail-mode))
      (error "This command only available in RMAIL mode.")))

(defun bugtrack-rmail-read-bug-file-name ()
  "Interactively prompt user for name of bugtrack file.
The default is determined by examining the current RMAIL message
according to the rules supplied by the user in the variable
bugtrack-rmail-rules (which see).  If no match is found, the default
directory is taken from bugtrack-default-dir, and the name defaults to
BUGS."
  (bugtrack-assert-rmail-mode)
  (let ((defaultdir bugtrack-default-dir)
	(defaultname "BUGS")
	(rules bugtrack-rmail-rules))
    (save-restriction
      (goto-char (point-min))
      (while rules
	(cond
	 ((re-search-forward (car (car rules)) nil t)
	  (if (car (cdr (car rules)))
	      (setq defaultdir (car (cdr (car rules)))))
	  (if (car (cdr (cdr (car rules))))
	      (setq defaultname (car (cdr (cdr (car rules))))))
	  (setq rules nil))
	 (t (setq rules (cdr rules))))))
    (expand-file-name
     (read-file-name (format "Bugtrack file (default %s): " defaultname)
		     defaultdir defaultname)
     defaultdir)))

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

;;;; User functions in RMAIL mode

;;;###autoload
(defun bugtrack-rmail-memorize-msg-id ()
  "In RMAIL mode, memorize the msg id for Bugtrack mode."
  (interactive)
  (save-excursion
    (save-restriction
      (bugtrack-rmail-narrow-to-headers rmail-current-message)
      (setq bugtrack-memorized-msg-id
	    (mail-fetch-field "message-id"))))
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
   (list (bugtrack-rmail-read-bug-file-name)
	 (read-from-minibuffer "Summary: ")
	 (read-from-minibuffer "Version: ")
	 (read-from-minibuffer "MAIL note: ")
	 (bugtrack-read-digit "Severity (0=unimportant, 9=important): ")
	 (bugtrack-read-digit "Priority (0=low, 9=high): ")
	 (bugtrack-read-digit "Difficulty (0=easy, 9=hard): ")))

  (let ((site "")
	(rmail-buf (current-buffer))
	(maps bugtrack-rmail-subject-cookie-map)
	(toggle nil)
	msg-id reporter bug-id cookie dist)

    ;; Find some info in the message
    (save-restriction
      (bugtrack-rmail-narrow-to-headers rmail-current-message)
      (setq msg-id (mail-fetch-field "message-id"))
      (setq reporter (or (mail-fetch-field "from")
			 (mail-fetch-field "sender")))
      (if (string-match "@[^> (]*" reporter)
	  (setq site (substring reporter (1+ (match-beginning 0))
				(match-end 0)))))

    ;; Create the new bug, inserting the info we just fetched
    (bugtrack bugtrack-file bugtrack-rmail-create-bug-where)
    (setq bug-id (bugtrack-create-bug 
		  summary version severity priority difficulty reporter site))
    (bugtrack-insert-message-id mail-comment msg-id)

    ;; Insert a cookie in the subject if this Bugtrack file
    ;; is mentioned in bugtrack-rmail-subject-cookie-map.
    (while maps
      (cond
       ((string= (file-truename (expand-file-name (car (car maps))))
		 buffer-file-truename)
	;; The file was mentioned!
	(setq cookie (format "##%s-bug-%d## " (car (cdr (car maps))) bug-id))
	(set-buffer rmail-buf)
	(rmail-maybe-set-message-counters)
	;; save-excursion doesn't work if we toggle the header.
	(setq dist (- (point-max) (point)))
	(save-restriction
	  (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
	  (goto-char (point-min))
	  (forward-line 1)
	  (if (= (following-char) ?1) 
	      (setq toggle t)))
	(if toggle (rmail-toggle-header))
	(let ((buffer-read-only nil)
	      (case-fold-search t))
	  (goto-char (point-min))
	  (re-search-forward "subject: ")
	  (insert cookie))
	(if toggle (rmail-toggle-header))
	(goto-char (- (point-max) dist))
	(setq maps nil))
       (t
	(setq maps (cdr maps)))))))

;;;###autoload
(defun bugtrack-rmail-search-msg-id ()
  "In RMAIL mode, search for any bugs about the current mail.
This will search closed as well as active bugs.  If there are
any Bugtrack cookies in the subject, use them before using the
message Id."
  (interactive)
  (save-restriction
    (bugtrack-rmail-narrow-to-headers rmail-current-message)
    (setq bugtrack-pending-bugs (bugtrack-rmail-extract-cookies))
    (setq bugtrack-previous-regexp
	  (regexp-quote (mail-fetch-field "message-id"))))
  (setq bugtrack-search-include-closed t)
  (setq bugtrack-search-buffers (bugtrack-all-bugtrack-buffers))
  (setq bugtrack-search-start t)

  (if (and (null bugtrack-pending-bugs) (null bugtrack-search-buffers))
      (error "No bugtrack buffers to search in present."))
  (switch-to-buffer-other-window (current-buffer))
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
     (substring id (match-beginning 0) (match-end 0)))))

;;;; User functions in bugtrack mode.

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
  (bugtrack-hunt-mail-or-ask (bugtrack-select-message)))


(defun bugtrack-hunt-mail-or-ask (msg-id)
  "Search for an email with message-id equal to MSG-ID.
This function first searches in all RMAIL files which are already
loaded in this emacs, then in all RMAIL files mentioned in the
variable bugtrack-auto-mail-file-list (which see), and finally prompts
the user for the name of an RMAIL file to search in.  The search stops
as soon as a matching message is found.  Signal an error if the message
cannot be found anywhere."
  (let ((buffers (buffer-list))
	(found nil)
	(files bugtrack-auto-mail-file-list))

    ;; 1. Search all present buffers.
    (while (and buffers (not found))
      (if (bugtrack-hunt-message-id msg-id (car buffers))
	  (setq found t)
	(setq buffers (cdr buffers))))

    ;; 2. Search all RMAIL files.
    (while (and files (not found))
      (set-buffer (find-file-noselect (car files)))
      (require 'rmail)
      (rmail-mode)
      (if (bugtrack-hunt-message-id msg-id (current-buffer))
	  (setq found t)
	(setq files (cdr files))))

    ;; 3. Ask the user.
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
