;;; bugtrack.el --- Store and retrieve bug reports.

;; Copyright (C) 1992, 1993, 1994 Per Cederqvist.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;; Created: 1992-07-29
;; Version: Id: bugtrack.el,v 1.30 1994/01/06 22:54:29 ceder Exp 
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

;; Overview
;; --------

;; Bugtrack is a support system for tracking bug reports.  As a new
;; bug report arrive, you create a ``bug'', assigning it a priority,
;; severity and difficulty.  The bug reports are collected in folders,
;; much like RMAIL folders.  You can get a summary of the bug reports,
;; sorted by priority, severity, difficulty or chronological.  The
;; summary normally excludes ``closed'' bugs, but if you give a prefix
;; argument the summary will also include inactive bug reports.

;; Bugtrack is tightly coupled to RMAIL.  When you receive a bug
;; report, you only have to press ``C'' and answer a few questions to
;; create the bug.  Later on, you can find the email that corresponds
;; to the bug with a single keystroke.  More than one email can be
;; associated with a bug, making it easy to review all emails about
;; this particular bug when you finally decide to do something about
;; it.

;; (Lack of) documentation
;; -----------------------

;; There is no fancy Texinfo manual for this code, but the doc string
;; for bugtrack-mode should contain all the documentation you need.
;; So, eval this buffer, and type "M-x bugtrack RET C-h m" to see more
;; documentation.

;; Installation
;; ------------

;; The summary feature of bugtrack requires elib, available via
;; anonymous ftp from lysator.liu.se in pub/emacs/elib-*.tar.gz.  The
;; current release of elib is 0.07.  You should be able to test
;; everything except the summary commands without getting elib.

;; This code has been tested on GNU Emacs 19.22.  It probably doesn't
;; work on Emacs 18, and I don't plan to support it for Emacs 18
;; (unless someone pays me to do it).

;; Byte-compile all files:
;;        emacs -batch -l default -f batch-byte-compile *.el

;; The "-l default" might be necessary so that Emacs knows where to
;; find elib.  Make sure that bugtrack*.el and bugtrack*.elc are on
;; your load-path.  Load bugtrack-init, and you are ready to go!  To
;; make Bugtrack available to you automatically you can put the
;; following line in your .emacs:
;;        (require 'bugtrack-init)

;; Bugs
;; ----

;; Send bug reports and feedback to ceder@lysator.liu.se.  Unless I get
;; some feedback it is unlikely that I will make new releases of this
;; code.  I wrote it because I needed it, and now I feel that it is
;; good enough for my purposes.  I have used it for more than 16
;; months (since September 1992), and I think that it is time to make
;; this public.

;; I need a word for the things that are present in the BUGS buffer.
;; I sometimes call them ``bug'', sometimes ``bug report''.  I think I
;; need a third word.

;; Bugtrack should be coupled to other mail readers, and to GNUS and
;; GNEWS.  However, I don't plan to do that myself, since I use RMAIL
;; and don't receive bug reports via news.  I hope someone will
;; contribute code for connecting the other mail readers to bugtrack.

;;; File format:

;; This is a regexp-like description of the format of the BUGS file.

;;; Version: 1
;;; Unused-Bug-Id: 1897
;;; Note: This is the header of a bugtrack file.
;;; Note: If you see this, it means that there are
;;; Note: no *known* bugs to track.
;;; ^L
;;; Id: <num>
;;; Summary: <text-sans-\n>
;;; (Version: <free-text-sans-\n>)?
;;; Created: <userid>: <date>
;;; Currently handled by: <login id>
;;; Severity: <digit>
;;; Priority: <digit>
;;; Difficulty: <digit>
;;; (Closed: <userid>: <date>)?
;;; Last edit: <userid>: <date>
;;; Reported by: <emal-addr>
;;; Site: <free-text-sans-\n>
;;; Associated mails:
;;; (<tab><text-without-NL-or-:>: <Message-ID-sans-\n>\n)*
;;; End mails.
;;; ---
;;; <Free text-without-\n\n>
;;; ^L

;;; Code:

(provide 'bugtrack)


;;;; Configuration and other variables/constants

(defvar bugtrack-default-dir nil
  "*Default directory for Bugtrack files are kept.
This is used as the default directory when prompting for Bugtrack files.
If nil (the default), use current working directory as default.")

(defconst bugtrack-maintainer-address "ceder@lysator.liu.se")

;;;; The modes and keymaps.

(put 'bugtrack-mode 'mode-class 'special)

(defvar bugtrack-summary-handle nil
  "The summary cookie handle associated with this bugtrack buffer.
Buffer local to each bugtrack buffer.")

(defvar bugtrack-pending-bugs nil
  "List of bugs to display.
Each bug is a list on the form (FILENAME ID), where FILENAME
is a full path to a Bugtrack file, and ID is a the number of the
bug.")

(defvar bugtrack-previous-regexp ""
  "In Bugtrack, the regexp the previous search used.")

(defvar bugtrack-search-start nil
  "Location in current Bugtrack buffer where the current search started.")

(defvar bugtrack-search-include-closed nil
  "If nil, the current Bugtrack search ignores matches in closed bugs.
Use a prefix argument to bugtrack-search to set this flag for the
current search.  There is currently no way to give this variable a
default valut; it is set each time you start a search.")

(defvar bugtrack-search-buffers nil
  "List of Bugtrack buffers that the current search should continue in.")

(defvar bugtrack-browse-mode-map nil
  "Keymap for the Bugtrack browse mode")

(if bugtrack-browse-mode-map
    nil
   (define-prefix-command 'bugtrack-summary-prefix-map)
  (setq bugtrack-browse-mode-map (make-keymap))
  (suppress-keymap bugtrack-browse-mode-map)
  (define-key bugtrack-browse-mode-map " " 'scroll-up)
  (define-key bugtrack-browse-mode-map "," 'bugtrack-search-continue)
  (define-key bugtrack-browse-mode-map "a" 'bugtrack-add-change-log-head)
  (define-key bugtrack-browse-mode-map "c" 'bugtrack-create-bug)
  (define-key bugtrack-browse-mode-map "e" 'bugtrack-edit)
  (define-key bugtrack-browse-mode-map "f" 'bugtrack-display-mail)
  (define-key bugtrack-browse-mode-map "h" 'bugtrack-summary-by-id)
  (define-key bugtrack-browse-mode-map "j" 'bugtrack-show-bug)
  (define-key bugtrack-browse-mode-map "s" 'bugtrack-summary-prefix-map)
  (define-key bugtrack-browse-mode-map "si" 'bugtrack-summary-by-id)
  (define-key bugtrack-browse-mode-map "sd" 'bugtrack-summary-by-difficulty)
  (define-key bugtrack-browse-mode-map "sp" 'bugtrack-summary-by-priority)
  (define-key bugtrack-browse-mode-map "ss" 'bugtrack-summary-by-severity)
  (define-key bugtrack-browse-mode-map "C" 'bugtrack-close-bug)
  (define-key bugtrack-browse-mode-map "M" 'bugtrack-insert-memorized-id)
  (define-key bugtrack-browse-mode-map "S" 'bugtrack-search)
  (define-key bugtrack-browse-mode-map "\en" 'bugtrack-next-bug)
  (define-key bugtrack-browse-mode-map "\ep" 'bugtrack-previous-bug)
  (define-key bugtrack-browse-mode-map "n" 'bugtrack-next-unclosed-bug)
  (define-key bugtrack-browse-mode-map "p" 'bugtrack-previous-unclosed-bug)
  (define-key bugtrack-browse-mode-map "q" 'bugtrack-quit)
  (define-key bugtrack-browse-mode-map ">" 'bugtrack-goto-last-bug)
  (define-key bugtrack-browse-mode-map "<" 'bugtrack-goto-first-bug)
  (define-key bugtrack-browse-mode-map "\C-?" 'scroll-down))

;;;###autoload
(defun bugtrack-mode ()
  "Specialized major mode for editing Bugtrack files.

Movement commands:
------------------
\\<bugtrack-browse-mode-map>
One page forward:   \\[scroll-up]            One page backward:      \\[scroll-down]
One line forward:   \\[next-line]            One line backwark:      \\[previous-line]
Next unclosed bug:  \\[bugtrack-next-unclosed-bug]              Previous unclosed bug:  \\[bugtrack-previous-unclosed-bug]
Next bug:           \\[bugtrack-next-bug]            Previous bug:           \\[bugtrack-previous-bug]
Last bug:           \\[bugtrack-goto-last-bug]              First bug:              \\[bugtrack-goto-first-bug]
Go to a bug Id:     \\[bugtrack-show-bug]  (Give Id as prefix or suffix).

All of the above commands can take a prefix argument.  Just type in
the digits before pressing the key mentioned above.

Search for regexp:    \\[bugtrack-search]   (Give prefix argument to search closed bugs as well.)
Continue last search: \\[bugtrack-search-continue]

Write a ChangeLog heading: \\[bugtrack-add-change-log-head]

Creating and modifying bug reports
----------------------------------
Create a new bug report: \\[bugtrack-create-bug]
Close a bug:             \\[bugtrack-close-bug]
Full text edit:          \\[bugtrack-edit]  (\\<bugtrack-edit-mode-map>Abort your changes with \\[bugtrack-edit-abort],
                             or install them with \\[bugtrack-edit-done].)


Saving the buffer\\<bugtrack-browse-mode-map>
-----------------
Save it:           \\[save-buffer]
Save and burry it: \\[bugtrack-quit]


RMAIL interface\\<rmail-mode-map>
---------------
In RMAIL mode, press  \\[bugtrack-rmail-create-bug]  to create a bug report. The current
\"Message-ID:\" will be inserted in the \"Associated mail:\" field,
and the \"Reported by:\" will be filled in from the \"From:\" field.
In RMAIL mode, press \\[bugtrack-rmail-search-msg-id] to find any bug reports 
that has that mail in its \"Associated mail:\" field.

You can press  \\[bugtrack-rmail-memorize-msg-id]  to remember a \"Message-ID:\", and then insert
the memorized message id in the \"Associated mails:\" field with  \\<bugtrack-browse-mode-map>\\[bugtrack-insert-memorized-id].

\\[bugtrack-display-mail]  circles through the mails in the bug report.  You will be asked
for the name of an RMAIL file to search for the mail if Emacs
can not find it in any of its buffers.


Summaries
---------
Summary by Id:         s i  (or  \\[bugtrack-summary-by-id])
Summary by difficulty: \\[bugtrack-summary-by-difficulty]
Summary by priority:   \\[bugtrack-summary-by-priority]
Summary by severity:   \\[bugtrack-summary-by-severity]

You will get a new buffer, in which you can type: \\<bugtrack-summary-mode-map>

\\[bugtrack-summary-goto-previous]   to display the previous bug.
\\[bugtrack-summary-goto-bug]   to display the current bug.
\\[bugtrack-summary-goto-next]   to display the next bug.
\\[cookie-next-cookie]  or  \\[cookie-previous-cookie]  to move forward or backward in the summary
               buffer without displaying the bug.

Useful variables:
	bugtrack-default-dir   Where you store your Bugtrack files.

You can use \\[bugtrack-submit-bug-report] to send a bug report (or
any other kind of feedback) on Bugtrack."

  (interactive)
  (let ((sum-hnd bugtrack-summary-handle))
    (kill-all-local-variables)
    (make-local-variable 'bugtrack-summary-handle)
    (setq bugtrack-summary-handle sum-hnd))
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'bugtrack-revert-buffer)
  (setq buffer-read-only t)
  (use-local-map bugtrack-browse-mode-map)
  (setq mode-name "Bugtrack browse")
  (setq major-mode 'bugtrack-mode)
  (bugtrack-narrow-to-page-and-update-mode-line)
  (force-mode-line-update)
  (run-hooks 'bugtrack-mode-hook))


;;;; Utility functions

(defun bugtrack-bump-id ()
  "Bump the id in the Bugtrack file's header and return the new id."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^Unused-Bug-Id: \\([0-9]*\\)$")
    (let ((id (string-to-int (buffer-substring (match-beginning 1)
					       (match-end 1)))))
      (let ((buffer-read-only nil))
	(replace-match (format "Unused-Bug-Id: %d" (1+ id))))
      id)))

(defun bugtrack-update-last-edit ()
  "In Bugtrack mode, update the Last edit: field."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\nLast edit: " (bugtrack-free-text-start))
    (let ((buffer-read-only nil))
      (delete-region (point) (progn (end-of-line) (point)))
      (insert (user-login-name) ": " (current-time-string)))))

(defun bugtrack-free-text-start ()
  "Bugtrack internal: Return the start position of the free text area.
Do not move point."
  (save-excursion
    (bugtrack-goto-free-text-start)))

(defun bugtrack-goto-free-text-start ()
  "Bugtrack internal: Return the start position of the free text area.
Also move point to that position."
  (progn
    (goto-char (point-min))
    (search-forward "\n---\n")
    (point)))

(defun bugtrack-bug-closed-p ()
  "Return non-nil if the current bug report is closed.
The buffer is expected to be narrowed around the current bug report.
The value returned is actually the \"userid: date\" part of the
line that indicates that it is closed."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\nClosed: " (bugtrack-free-text-start) t)
	(buffer-substring (point) (save-excursion (end-of-line) (point)))
      nil)))
		   
(defun bugtrack-revert-buffer (a b)
  "Bugtrack version of revert-buffer."
  (let ((id (progn (goto-char (point-min)) (re-search-forward "Id: ")
		   (read (current-buffer))))
	(revert-buffer-function nil))
    (revert-buffer a b)
    (bugtrack-mode)
    (bugtrack-show-bug id)))

(defun bugtrack-read-bug-file-name ()
  "Interactively prompt user for name of bugtrack file.
The variable bugtrack-default-dir, if set, determines the default
directory to look in."
  (expand-file-name
   (read-file-name "Bugtrack file (default BUGS): "
		   bugtrack-default-dir "BUGS")))

(defun bugtrack-narrow-to-page-and-update-mode-line (&optional count)
  "Make text outside current page invisible, and update mode-line counter.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in.
Updates the counters in the mode-line."
  (widen)

  ;; Emacs 19 moves to the next page if point is immediately before
  ;; the \C-l. This is not what we want, so, if we are not about to
  ;; move, move back, narrow to the current page, and move forward
  ;; again.
  (cond
   ((and (looking-at "\C-l"))
    (backward-char)
    (narrow-to-page count)
    (if (null count)
	(forward-char)))
   (t
    (narrow-to-page count)))

  ;; Update the counters in the mode line.
  (save-restriction
    (widen)
    (save-excursion
      (let ((count-backward 0)
	    (count-forward 0)
	    (where (point)))
	(while (re-search-backward page-delimiter (point-min) t)
	  (setq count-backward (1+ count-backward)))
	(goto-char where)
	(while (re-search-forward page-delimiter (point-max) t)
	  (setq count-forward (1+ count-forward)))
	(setq mode-line-process
	      (format " %d/%d" count-backward
		      (1- (+ count-backward count-forward))))))))

(defun bugtrack-read-digit (prompt)
  "Read a digit from the minibuffer, prompting with PROMPT.
Return the digit as an integer in the interval 0-9."
  (message "%s" prompt)
  (let ((ans nil)
	(cursor-in-echo-area t))
    (while (not ans)
      (setq ans (read-char))
      (if (and (>= ans ?0) (<= ans ?9))
	  (setq ans (- ans ?0))
	(setq ans nil)
	(ding)
	(message "%s (0-9)!" prompt)))
    (message "")
    ans))

;;;; User commands outside Bugtrack mode

;;;###autoload
(defun bugtrack (file &optional where)
  "Visit a bugtrack FILE. The FILE is created if it doesn't exist.
Optional second argument WHERE can be 'window to display the bug in
another window, or 'frame to display it in another frame"
  (interactive
   (list (bugtrack-read-bug-file-name)))
  (if (file-directory-p file)
      (setq file (concat (file-name-as-directory file) "BUGS")))
  (cond
   ((eq where 'frame)
    (find-file-other-frame file))
   ((eq where 'window)
    (find-file-other-window file))
   (t
    (find-file file)))
  (widen)
  (let ((opoint (point)))
    (if (eq (point-min) (point-max))
	(insert "Version: 1
Unused-Bug-Id: 1
Note: This is the header of a bugtrack file.
Note: If you see this, it means that there are
Note: no *known* bugs to track.
\f\n"))
    (bugtrack-mode)
    (if (eq opoint 1)
	(bugtrack-goto-first-bug)
      (bugtrack-narrow-to-page-and-update-mode-line)
      (if (or (= (point-min) (point-max))
	      (= (point-min) 1))
	  (bugtrack-goto-first-bug)))))

;;;###autoload
(defun bugtrack-other-window (file)
  "Run Bugtrack on a FILE in another window."
  (interactive
   (list (expand-file-name
	  (read-file-name (format "Bugtrack file (default BUGS): ")
			  nil "BUGS"))))
  (bugtrack file 'window))

;;;###autoload
(defun bugtrack-other-frame (file)
  "Run Bugtrack on a FILE in another frame."
  (interactive
   (list (expand-file-name
	  (read-file-name (format "Bugtrack file (default BUGS): ")
			  nil "BUGS"))))
  (bugtrack file 'frame))

;;;###autoload
(defun bugtrack-version ()
  "Display the version number (1.3) of bugtrack.
The version number is displayed in the minibuffer.  This function
also returns the version number, as a string."
  (interactive)
  (message "Bugtrack version %s" "1.3")
  "1.3")

;;;###autoload
(defun bugtrack-submit-bug-report ()
  "Submit via mail a bug report on bugtrack."
  (interactive)
  (require 'reporter)
  ;; Need defvar for all variables.
  (require 'bugtrack-rmail)
  (require 'bugtrack-edit)
  (require 'bugtrack-sum)
  (reporter-submit-bug-report 
   bugtrack-maintainer-address
   "Bugtrack 1.3"
   (list 'bugtrack-default-dir
	 'bugtrack-saved-headers
	 'bugtrack-saved-text
	 'bugtrack-rmail-rules
	 'bugtrack-rmail-subject-cookie-map
	 'bugtrack-auto-mail-file-list
	 'bugtrack-memorized-msg-id
	 'bugtrack-rmail-create-bug-where
	 'bugtrack-basic-buffer
	 'bugtrack-default-dir
	 'bugtrack-pending-bugs
	 'bugtrack-previous-regexp
	 'bugtrack-search-start
	 'bugtrack-search-include-closed
	 'bugtrack-search-buffers)
   nil nil
   "Thanks for sending a bug report/suggestion/feedback.
If you have more than one bug to report or suggestion
to make, PLEASE send them in separate messages!"))


;;;; User commands in Bugtrack browse mode

;;; Moving around in the buffer.

(defun bugtrack-previous-unclosed-bug (count)
  "In Bugtrack mode, go to previous non-closed bug report.
With prefix argument COUNT, move backward COUNT non-closed reports."
  (interactive "p")
  (if (< count 0)
      (bugtrack-next-unclosed-bug (- count))
    (let ((last-open (point)))
      (while (> count 0)
	(bugtrack-narrow-to-page-and-update-mode-line -1)
	(cond
	 ;; If we arrived before the first bug report, bug up to the
	 ;; last known open bug report. Never move to a later bug
	 ;; report than the one we started at.
	 ((= (point-min) 1)
	  (widen)
	  (goto-char last-open)
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (message "No previous bug report")
	  (setq count 0))
	 ;; Decrease the count if this bug was nog yet closed.
	 ((not (bugtrack-bug-closed-p))
	  (setq last-open (point))
	  (setq count (1- count)))))))
  (goto-char (point-min)))

(defun bugtrack-next-unclosed-bug (count)
  "In Bugtrack mode, go to next non-closed bug report.
With prefix argument COUNT, move forward COUNT non-closed reports."
  (interactive "p")
  (if (< count 0)
      (bugtrack-previous-unclosed-bug (- count))
    (let ((last-open (point)))
      (while (> count 0)
	(bugtrack-narrow-to-page-and-update-mode-line 1)
	(cond
	 ;; If we arrived past the last bug report, bug up to the
	 ;; last known open bug report. Never move to an earlier bug
	 ;; report than the one we started at.
	 ((= (point-min) (point-max))
	  (widen)
	  (goto-char last-open)
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (message "No more bug reports")
	  (setq count 0))
	 ;; Decrease the count if this bug was nog yet closed.
	 ((not (bugtrack-bug-closed-p))
	  (setq last-open (point))
	  (setq count (1- count)))))))
  (goto-char (point-min)))

(defun bugtrack-previous-bug (count)
  "In bugtrack mode, go to previous bug report.
With prefix argument COUNT, move backward COUNT reports."
  (interactive "p")
  (bugtrack-narrow-to-page-and-update-mode-line (- count))
  (cond
   ((= (point-min) 1)
    (message "No previous bug reports")
    (bugtrack-narrow-to-page-and-update-mode-line 1)
    (if (= (point-min) (point-max))
	(bugtrack-narrow-to-page-and-update-mode-line -1))))
  (goto-char (point-min)))

(defun bugtrack-next-bug (count)
  "In bugtrack mode, go to next bug report.
With prefix argument COUNT, move forward COUNT reports."
  (interactive "p")
  (bugtrack-narrow-to-page-and-update-mode-line count)
  (cond
   ((= (point-min) (point-max))
    (message "No more bug reports")
    (bugtrack-narrow-to-page-and-update-mode-line -1)))
  (goto-char (point-min)))

(defun bugtrack-show-bug (id)
  "Show bug number ID."
  (interactive "NBug Id: ")
  (let ((opoint (point)))
    (widen)
    (goto-char (point-min))
    (cond
     ((search-forward (format "\f\nId: %d\n" id) nil t)
      (bugtrack-narrow-to-page-and-update-mode-line)
      (goto-char (point-min)))
     (t
      (goto-char opoint)
      (bugtrack-narrow-to-page-and-update-mode-line)
      (message "No such bug Id: %d" id)))))
	       

(defun bugtrack-goto-first-bug ()
  "In Bugtrack mode, go to the first bug report."
  (interactive)
  (widen)
  (goto-char (point-min))
  (bugtrack-narrow-to-page-and-update-mode-line 1)
  (while (and (not (= (point-min) (point-max)))
	      (bugtrack-bug-closed-p))
    (bugtrack-narrow-to-page-and-update-mode-line 1))
  (if (= (point-min) (point-max))
      (progn
	(widen)
	(goto-char (point-min))
	(bugtrack-narrow-to-page-and-update-mode-line))))

(defun bugtrack-goto-last-bug ()
  "In Bugtrack mode, go to the last unclosed bug report."
  (interactive)
  (widen)
  (goto-char (point-max))
  (bugtrack-narrow-to-page-and-update-mode-line -1)
  (while (and (not (= (point-min) 1))
	      (bugtrack-bug-closed-p))
    (bugtrack-narrow-to-page-and-update-mode-line -1)))

;;; Searching

(defun bugtrack-search (regexp &optional include-closed)
  "In Bugtrack mode, search forward for REGEXP.
Display the bug report where REGEXP was found.
Closed bugs are ignored, unless an optional prefix argument is given.

Use bugtrack-search-continue to serach for more hits."
  (interactive "sBugtrack search (regexp): \nP")
  ;; Remember these for bugtrack-search-continue.
  (setq bugtrack-previous-regexp regexp)
  (setq bugtrack-search-start t)
  (setq bugtrack-search-include-closed include-closed)
  (setq bugtrack-search-buffers (list (current-buffer)))
  (bugtrack-search-continue))

(defun bugtrack-search-continue ()
  "In Bugtrack mode, continue last bugtrach-search or bugtrack-rmail-find-id."
  (interactive)
  (if bugtrack-pending-bugs
      (progn
	(bugtrack (car (car bugtrack-pending-bugs)))
	(bugtrack-show-bug (car (cdr (car bugtrack-pending-bugs))))
	(setq bugtrack-pending-bugs (cdr bugtrack-pending-bugs)))
    (if (eq bugtrack-search-start t)
	(progn
	  (if (null bugtrack-search-buffers)
	      (error "No search in progress."))
	  (set-buffer (car bugtrack-search-buffers))
	  (setq bugtrack-search-buffers (cdr bugtrack-search-buffers))
	  (setq bugtrack-search-start (point))))
    (widen)
    (let ((limit (if (< (point) bugtrack-search-start) 
		     bugtrack-search-start
		   nil))
	  (res nil)
	  (start (point))
	  (start-buf (current-buffer)))
      (while (not res)
	(setq res (re-search-forward bugtrack-previous-regexp limit 'move))
	(cond
	 ;; Wrap at end of buffer?
	 ((and (null res) (null limit))
	  (setq limit bugtrack-search-start)
	  (goto-char (point-min))
	  (re-search-forward ""))
	 ;; Any more buffers to search in?
	 ((and (null res) bugtrack-search-buffers)
	  (goto-char start)
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (set-buffer (car bugtrack-search-buffers))
	  (setq bugtrack-search-buffers (cdr bugtrack-search-buffers))
	  (widen)
	  (setq start (point))
	  (setq bugtrack-search-start (point))
	  (setq limit nil))
	 ;; Search fail?
	 ((null res)
	  (goto-char start)
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (switch-to-buffer start-buf)
	  (message "No more matches for %s" bugtrack-previous-regexp)
	  (ding)
	  (setq res 'nope))
	 ;; Found it (regardless of wether it is closed)?
	 (bugtrack-search-include-closed
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (switch-to-buffer (current-buffer)))
	 ;; Found it in a closed bug? If so, skip it.
	 ((progn
	    (bugtrack-narrow-to-page-and-update-mode-line)
	    (bugtrack-bug-closed-p))
	  (widen)
	  (setq res nil))
	 ;; Found it?
	 (res
	  (bugtrack-narrow-to-page-and-update-mode-line)
	  (switch-to-buffer (current-buffer))))))))

;;; ChangeLog support

(defun bugtrack-add-change-log-head (&optional whoami file-name other-window)
  "Write a heading in a ChangeLog file for the current bug.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  If nil, uses `change-log-default-name'.
Third arg OTHER-WINDOW non-nil means visit in other window."
  (interactive (list current-prefix-arg
		     (prompt-for-change-log-name)))
  (let (bugid summary)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Id: ")
      (setq bugid (buffer-substring (point) (progn (end-of-line) (point))))
      (re-search-forward "Summary: ")
      (setq summary (buffer-substring (point) (progn (end-of-line) (point)))))
    (add-change-log-entry whoami file-name other-window t)
    (delete-region (progn (beginning-of-line) (point))
		   (progn (end-of-line) (point)))
    (insert (format "\t* Id %s: %s" bugid summary))))

(defun bugtrack-add-change-log-head-other-window (&optional whoami file-name)
  "Write a heading in a ChangeLog file for the current bug.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  
If nil, uses `change-log-default-name'."
  (interactive (list current-prefix-arg
		     (prompt-for-change-log-name)))
  (bugtrack-add-change-log-head whoami file-name t))

;;;; Creating and modifying entries

(defun bugtrack-create-bug (summary 
			    version
			    severity priority difficulty 
			    reporter site
			    &optional text)
  "Create a bug in the current Bugtrack buffer.
Args: SUMMARY VERSION SEVERITY PRIORITY DIFFICULTY REPORTER SITE &optional TEXT

SEVERITY, PRIORITY and DIFFICULTY are decimal numbers in the range
0-9, the others are strings that should not contain any newline. TEXT may
contain newlines.

Returns the bug id of the new bug."
  (interactive
   (list (read-from-minibuffer "Summary: ")
	 (read-from-minibuffer "Version: ")
	 (bugtrack-read-digit "Severity (0=unimportant, 9=important): ")
	 (bugtrack-read-digit "Priority (0=low, 9=high): ")
	 (bugtrack-read-digit "Difficulty (0=easy, 9=hard): ")
	 (read-from-minibuffer "Reported by: ")
	 (read-from-minibuffer "Site: ")))

  (widen)
  (let ((buffer-read-only nil)
	(id (bugtrack-bump-id)))
    (goto-char (point-max))
    (insert (format "Id: %d\n" id))
    (insert "Summary: " summary "\n")
    (insert "Version: " version "\n")
    (insert "Created: " (user-login-name) ": " (current-time-string) "\n")
    (insert "Currently handled by: \n")
    (insert (format "Severity: %d\n" severity))
    (insert (format "Priority: %d\n" priority))
    (insert (format "Difficulty: %d\n" difficulty))
    (insert "Last edit: " (user-login-name) ": " (current-time-string) "\n")
    (insert "Reported by: " reporter "\n")
    (insert "Site: " site "\n")
    (insert "Associated mails:\n")
    (insert "End mails.\n---\n")
    (if text
	(insert text "\n"))
    (insert "\f\n")
    (bugtrack-goto-last-bug)
    id))


(defun bugtrack-close-bug ()
  "In Bugtrack mode, close the current bug report."
  (interactive)
  (if (bugtrack-bug-closed-p)
      (error "The bug is already closed"))
  (cond
   ((y-or-n-p "Close this bug? ")
    (save-excursion
      (goto-char (point-min))
      (search-forward "\nDifficulty: ")
      (forward-line 1)
      (let ((buffer-read-only nil))
	(insert "Closed: " (user-login-name)
		": " (current-time-string) "\n"))))))


;;;; Misc. commands in the bugtrack buffer

(defun bugtrack-quit ()
  "In Bugtrack mode, save the buffer and bury it."
  (interactive)
  (save-buffer)
  (bury-buffer))

;;; Local Variables:
;;; generated-autoload-file: "bugtrack-init.el"
;;; End:

;;; bugtrack.el ends here
