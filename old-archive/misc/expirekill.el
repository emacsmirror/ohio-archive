; Status: RO
; From: "Ben Liblit" <liblit@z-code.com>
; Subject: expire-kill 2.6
; Date: Mon, 19 Sep 1994 22:22:23 -0700
; 
; --
; --PART-BOUNDARY=.19409192222.ZM974.z-code.com
; Content-Type: text/plain; charset=us-ascii
; 
; Attached below are the sources for version 2.6 of expire-kill.  Please note
; that the recommended filename has been changed from "expire-kill" to
; "expirekill".  Please remove misc/expire-kill.el.Z, and place the attached
; sources in misc/expirekill.el.Z.  Thank you.
; 
; --PART-BOUNDARY=.19409192222.ZM974.z-code.com
; X-Zm-Content-Name: expirekill.el
; Content-Description: Text
; Content-Type: text/plain ; name="expirekill.el" ; charset=us-ascii ; x-irix-type=AsciiTextFile
; 
;;; expirekill.el --- expiring kill patterns for GNUS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; File:         expirekill.el
;; Description:  Expiring kill patterns for GNUS
;; Author:       Ben Liblit <liblit@z-code.com>
;; Created:      Wed Mar 2 1993
;; Modified:     Mon Sep 19 21:37:31 1994 (Ben) liblit@z-code.com
;; Version:	 $Id: expirekill.el,v 2.6 1994/09/20 04:37:54 liblit Exp $
;; Keywords:     news
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1993, 1994  Ben Liblit.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to liblit@z-code.com or
;;; liblit@well.sf.ca.us) or from the Free Software Foundation, Inc.,
;;; 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;;
;;; This package augments the standard GNUS kill file mechanism to
;;; allow expiring kill patterns.  Time stamps may be stored with
;;; patterns, and if a pattern's time stamp indicates that it has not
;;; been matched in a long period of time, that pattern is removed.

;;; Installation:
;;;
;;; Optionally byte-compile expirekill.el to expirekill.elc and put
;;; them both in a directory on your load-path.  To load expire-kill
;;; when GNUS first up, add the following to your .emacs:
;;;
;;;   (setq gnus-startup-hook
;;;         '(lambda ()
;;;            (require 'expirekill)))
;;;
;;; Autoloading based on the function "expire-kill" will *not* work
;;; properly, as expirekill needs to hook itself into other parts of
;;; GNUS before the first kill file is loaded.
;;;
;;; Also, please note that expire-kill needs to use either Dave
;;; Gillespie's calc package or Edward Reingold's calendar package for
;;; performing date calculations.  The variable expire-date-package
;;; should be set to either 'calc or 'calendar, depending on which you
;;; wish to use.

;;; Background and Motivation:
;;;
;;; The standard GNUS kill file mechanism is fairly powerful and
;;; flexible.  However, its usefulness is limited by the fact that
;;; kill patterns remain active indefinitely, unless manually removed
;;; by the user.  This makes certain uses of kill files highly
;;; impractical.
;;;
;;; For example, one might wish to use a subject-matching kill pattern
;;; to mark articles in a discussion thread that one is not interested
;;; in.  Using standard (gnus-kill ...), though, means that the
;;; pattern will remain in the kill file long after the thread itself
;;; has died out.  As time goes on, kill files will become bloated
;;; with patterns that have long ceased to be active.
;;;
;;; This package provides a time stamped alternative to "gnus-kill".
;;; The function "expire-kill" takes similar arguments, and performs
;;; the same function.  However, "expire-kill" also takes a time stamp
;;; argument (stored as a string) that indicates the last time its
;;; pattern was successfully matched.  Thus, instead of:
;;;
;;;   (gnus-kill "Subject" "cheese")
;;;
;;; in a kill file, one might see:
;;;
;;;   (expire-kill "Subject" "cheese" "<Tue Mar 2, 1993>")
;;;
;;; which would perform the same kill actions as "gnus-kill", but
;;; which additionally records that it hasn't actually seen a subject
;;; of "cheese" since March 2.
;;;
;;; A new method of applying kill files allows these time stamps to be
;;; updated when matches are made.  Other support functions sweep
;;; through a newsgroup's kill file and delete patterns that have not
;;; been matched in a long enough time (seven days, by default).
;;; Updated kill files are saved back to disk, or optionally deleted
;;; entirely if *all* their patterns have expired.
;;;
;;; Note that expire-kill is backward compatible with standard GNUS
;;; kill files.  Calls to "gnus-kill" and other elisp still work as
;;; before, and will never be expired.

;;; Usage:
;;;
;;; To use this package, simply add calls to "expire-kill" to your
;;; GNUS kill files.  The first two arguments specify a header field
;;; and a regexp pattern, just as for "gnus-kill".  The third should
;;; be a string or integer that can be parsed into an initial time
;;; stamp.
;;;
;;; You should never need to construct these calls by hand, though.
;;; Instead, a suite of functions are provided that add calls, or
;;; portions of calls, for you.  All of these are suitable for calling
;;; via M-x, or for binding onto keys.  In fact, each function
;;; described below has a corresponding variable with the same name.
;;; If that variable is set to a string representing some sequence of
;;; keys, that key seuqence will be bound to evoke the corresponding
;;; function in the appropriate buffers.  These functions (and
;;; associated variables) are as follows:
;;;
;;;   - expire-summary-kill-same-subject
;;;   - expire-summary-kill-same-subject-and-select
;;;   - expire-summary-kill-thread
;;;   - expire-summary-kill-thread-and-select
;;;
;;; These functions should be used from the *Summary* buffer.  The
;;; first two functions add expiring kill patterns for the subject of
;;; the article at the cursor.  The second two functions add expiring
;;; kill patterns for followups to the article at the cursor.  All
;;; four functions mark any articles already in the *Summary* buffer
;;; that match their targets.  Furthermore, the "-and-select" forms
;;; immediately select the next unread article.
;;;
;;;   - expire-kill-file-kill-by-subject
;;;   - expire-kill-file-kill-by-thread
;;;
;;; These two functions may be used while editing a kill file.  They
;;; insert "expire-kill" calls to match the most recently seen subject
;;; and followups to the current article, respectively.
;;;
;;;   - expire-kill-file-insert-time-stamp
;;;
;;; This function is also intended for use while editing kill files.
;;; It will insert a time stamp corresponding to the present time
;;; after the cursor.  This can be useful for finishing up hand
;;; written calls to "expire-kill".
;;;
;;; By default, expire-summary-kill-same-subject-and-select is bound
;;; to "C-c C-k" and expire-summary-kill-thread-and-select is bound to
;;; "C-c k", both in the *Summary* buffer.  None of the others are
;;; bound by default, although this may easily be customized by
;;; setting the same-named variable to the desired key sequence.

;;; Known Bugs and Limitations:
;;;
;;; For simplicity's sake, empty (whitespace only) kill files are not
;;; deleted until the next time their newsgroup is selected.  It might
;;; be nicer to delete empties as soon as the last s-expression is removed.
;;;
;;; Some users symlink kill files, using one such file for several
;;; related newsgroups.  We try to do right by these users, not
;;; deleting empty kill files that are also symlinks.  However,
;;; patterns may tend to expire more quickly when a kill file is
;;; shared.  If a pattern doesn't match in one group, it can be
;;; expired before it even gets to look at a second group.

;;; Acknowledgments:
;;;
;;; Thanks for release 2.x go out to the many users who were kind
;;; enough to discuss and suggest improvements over earlier releases.
;;; In particular, Dave Disser's insightful correspondence has
;;; inspired many of 2.x's enhancements.  Don Wells, Bill Oakley, and
;;; Rod Whitby also deserve recognition for their suggestions, bug
;;; reports, and invaluable feedback.

;;; LISPDIR ENTRY for the Elisp Archive
;;; 
;;;    LCD Archive Entry:
;;;    expire-kill|Ben Liblit|liblit@z-code.com|
;;;    expiring kill patterns for GNUS|
;;;    19-Sep-1994|2.6|~/misc/expirekill.el.Z|

;;; Code:

;;;; ------------------------------------------------------------
;;;; User customization variables.
;;;; ------------------------------------------------------------

(defvar expire-maximum-age 7
  "*Longest time a pattern can go unmatched before being removed.
The units on this measure are in days, and its value should be an
integer.")


(defvar expire-flush-frequency 'group
  "*Determines how often modified kill file buffers are flushed.
If set to the atom 'always, then flushing happens after every
modification.  If set to 'group, then flushing occurs when you exit
the current newsgroup.  If set to 'session, then modifications are
flushed only when you exit GNUS itself.  Otherwise, no automatic
actions are taken.

The variable expire-flush-action determines what it actually means to
\"flush\" a modified kill file buffer.")


(defvar expire-flush-action 'kill
  "*Determines what to do with buffers of modified kill files.
If set to the atom 'kill, then the modified buffer is saved and
killed.  If set to 'save, then the modified buffer is saved, but is
not killed.  Otherwise, no actions are taken.

The variable expire-flush-frequency determines how frequently the
requested action is taken.")


(defvar expire-simplify-subject t
  "*Determines whether simplified subjects will be used as kill patterns.
If nil, the original subject will be used in subject-based kill
patterns.  Otherwise, the subject will first be simplified using
gnus-simplify-subject.")

(defvar expire-truncate-subject nil
  "*If set to a number, subject patterns are truncated at that length.
UUCP often cuts off subjects at 24 characters.  Truncating kill
patterns at this limit will tend to improve hit rates.  If
expire-simplify-subject is set, simplification happens first, then
truncation.")


(defvar expire-append-kills nil
  "*Determines where new patterns should be placed in kill files.
If nil, new patterns are placed at the beginning of kill files.
Otherwise, they are added at the end.  If you're not sure how best to
set this, you don't need to worry about it.")


(defvar expire-summary-kill-same-subject nil
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the *Summary* buffer.  Otherwise, no
binding will be made for this function.")

(defvar expire-summary-kill-same-subject-and-select "\C-c\C-k"
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the *Summary* buffer.  Otherwise, no
binding will be made for this function.")

(defvar expire-summary-kill-thread nil
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the *Summary* buffer.  Otherwise, no
binding will be made for this function.")

(defvar expire-summary-kill-thread-and-select "\C-ck"
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the *Summary* buffer.  Otherwise, no
binding will be made for this function.")

(defvar expire-kill-file-kill-by-subject "\C-c\C-x\C-s"
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the kill file buffers.  Otherwise, no
binding will be made for this function.")

(defvar expire-kill-file-kill-by-thread "\C-c\C-x\C-t"
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the kill file buffers.  Otherwise, no
binding will be made for this function.")

(defvar expire-kill-file-insert-time-stamp "\C-c\C-x\C-i"
  "*Key sequence to which to bind the function of the same name.
If set to a string representing a key sequence, that sequence will
evoke the same-named function in the kill file buffers.  Otherwise, no
binding will be made for this function.")


(defvar expire-delete-empties 't
  "*Determines whether or not empty kill files should be deleted.
If set to the atom 'ask, then the user will be asked each time.  If
set to some other non-nil value, then empties will be deleted
automatically the first time they are applied to a newsgroup.  If nil,
no deletions will be performed.")


(defvar expire-kill-default-command '(gnus-summary-mark-as-read nil "x")
  "*Default command to be evaluated by expire-kill if none is given.
This must be a lisp expression or a string representing a key sequence.")


(defvar expire-after-apply-hook (function
				 (lambda ()
				   (gnus-expunge "Xx")))
  "*A normal hook called after all kill files have been applied.
This hook is called after the global and local kill files have been
applied to the currently-selected newsgroup.

If you used to finish off all of your kill files with a call to
gnus-expunge, you can factor all of that out into this hook.  This
reduces kill file size, and makes it easy to delete empty ones.")


(defvar expire-load-hook nil
  "*A normal hook called after expire-kill is loaded in.
This can be a good place to put custom key bindings.")


(defvar expire-date-package 'calendar
  "*The name of the package to use for date calculations.
This should be set to an atom for which expire-date-package-profiles
has an entry, and which may be loaded using (require ...).  Currently
supported packages include calc and calendar.

Note that stamps may be read from either format, assuming both are
available.  New stamps, however, will always be generated using the
selected package.")


(defvar expire-date-package-profiles
  '((calendar (calendar-absolute-from-gregorian (calendar-current-date))
	      (- (calendar-absolute-from-gregorian (calendar-current-date))
		 timestamp))
    (calc (calc-eval "floor(now())")
	  (calc-eval "floor(now() - $)" 'raw timestamp)))
  "*A list of profiles of known date calculation packages.
Each profile is, itself, a list of three values.

 - The first is the package name as an atom, which may be used in
   expire-date-package and which is used in (require ...) at load
   time.

 - The second is an s-expression that returns the current date.

 - The third is an s-expression that returns the (integral) difference
   between the date stored in the variable \"timestamp\" and the
   current date.")


(defconst expire-version
  (let ((revision "$Revision: 2.6 $"))
    (if (string-match "[0-9]+\\(\\.[0-9]+\\)*" revision)
	(substring revision (match-beginning 0) (match-end 0))
      revision))
  "The current version, of revision number, of expire-kill.
Be sure to include this in any bug reports.")

(defconst expire-maintainer-address "Ben Liblit <liblit@z-code.com>"
  "Name and e-mail address of expire-kill's maintainer.")

;;;; ------------------------------------------------------------
;;;; Dependencies.
;;;; ------------------------------------------------------------

;;; We need to plug into several GNUS hooks, and add-hook is the
;;; cleanest way to do so.  Several implementations exist; any should
;;; suffice.  Some Emacsi may have add-hook built in.

(or (fboundp 'add-hook)
    (require 'add-hook))

;;; Backquoting is used to construct the augmented commands that
;;; expire-kill passes down to gnus-kill.

(require 'backquote)

;;; All of expire-kill's date calculations are handled by outside
;;; packages.  Currently supported are Dave Gillespie's winning calc
;;; package, and Edward Reingold's equally winning calendar package.
;;; If you don't have either, you should.  Inquire at your
;;; neighborhood elisp archive.

;;; Note that if you are using calendar and receive errors about a
;;; void function named current-time-zone, you will need to initialize
;;; some of calendar's variables for it.  Those variables are
;;; calendar-time-zone, calendar-standard-time-zone-name, and
;;; calendar-daylight-time-zone-name.  See calendar.el for further
;;; information.

(and (boundp 'expire-date-package)
     (require expire-date-package))

;;; Common Lisp defines a really handy (case...) form, that we use for
;;; checking the value of various user options.  We also use its
;;; convenient push and pop functions.

(require 'cl)

;;; Normally, GNUS should already be loaded by the time we are loaded.
;;; Just in case, though, make sure it is there.

(require 'gnus)

;;;; ------------------------------------------------------------
;;;; Internal-use variables.
;;;; ------------------------------------------------------------

(defvar expire-current-kill-buffer nil
  "The buffer of the kill file currently (or recently) being applied.")

(defvar expire-modified-buffers nil
  "A list of all kill file buffers that may have been modified recently.")


(defun expire-current-date ()
  "Return some representation of today's date."
  (eval (nth 1 (assq expire-date-package
	       expire-date-package-profiles))))


(defun expire-days-since (timestamp)
  "Return the integral number of days between today and TIMESTAMP."
  (eval (nth 2 (assq expire-date-package
		     expire-date-package-profiles))))

;;;; ------------------------------------------------------------
;;;; User functions.
;;;; ------------------------------------------------------------

(defun expire-submit-bug-report ()
  "Submit a bug report on expire-kill via e-mail."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   expire-maintainer-address
   (concat "expire-kill " expire-version)
   '(expire-maximum-age
      expire-flush-frequency
      expire-flush-action
      expire-simplify-subject
      expire-truncate-subject
      expire-delete-empties
      expire-kill-default-command
      expire-after-apply-hook
      expire-load-hook
      expire-date-package
      gnus-version
      gnus-apply-kill-hook
      gnus-exit-group-hook
      gnus-exit-gnus-hook
      gnus-use-long-file-name
      gnus-kill-file-name)))

(defun expire-kill (field pattern timestamp &optional command all)
  "If FIELD of an article matches REGEXP, update TIMESTAMP and execute COMMAND.
If no COMMAND is given, the value of expire-kill-default-command is
used.  If optional 5th argument ALL is non-nil, articles marked are
also applied to.  If FIELD is an empty string (or nil), entire article
body is searched for.  COMMAND must be a lisp expression or a string
representing a key sequence."
(let (successful)
  (let ((command (or command expire-kill-default-command)))
    (gnus-kill field pattern
	       (` (progn (setq successful t)
			 (, (if (stringp command)
				(list 'execute-kbd-macro command)
			      command))))
	       all))
  (if successful
      (expire-restamp field pattern
		      (expire-convert-timestamp timestamp)
		      command all)
    (expire-filter (expire-convert-timestamp timestamp)))))


(defun expire-kill-file-kill-by-subject (ask)
  "Insert expiring KILL command for current subject.
Argument ASK non-nil (C-u if called interactively) allows the user to
edit the pattern before it is inserted."
  (interactive "P")
  (let* ((subject (if gnus-current-kill-article
		      (gnus-header-subject
		       (gnus-find-header-by-number
			gnus-newsgroup-headers
			gnus-current-kill-article))
		    ""))
	 (cutoff (and (numberp expire-truncate-subject)
		      (min expire-truncate-subject
			   (length subject)))))
    (expire-insert-kill "Subject"
			(regexp-quote
			 (substring (if expire-simplify-subject
					(gnus-simplify-subject subject)
				      subject)
				    0 cutoff))
			ask)))


(defun expire-kill-file-kill-by-thread (ask)
  "Insert expiring KILL command for current thread.
Argument ASK non-nil (C-u if called interactively) allows the user
to edit the pattern before it is inserted."
  (interactive "P")
  (expire-insert-kill "References"
		      (if gnus-current-kill-article
			  (regexp-quote
			   (gnus-header-id
			    (gnus-find-header-by-number
			     gnus-newsgroup-headers
			     gnus-current-kill-article)))
			"")
		      ask))


(defun expire-kill-file-insert-time-stamp ()
  "Insert a time stamp for the current date after point.
Handy for finishing up hand written calls to expire-kill."
  (interactive)
  (prin1 (expire-current-date)
	 (current-buffer)))


(defun expire-summary-kill-same-subject (ask)
  "Add a local, expiring kill pattern for the current subject.
Also, mark all articles with this subject in the current buffer as
read, but do not select the next unread article.  Argument ASK non-nil
(C-u if called interactively) allows the user to edit the pattern
before it is inserted."
  (interactive "P")
  (expire-summary-kill-using 'expire-kill-file-kill-by-subject ask)
  (gnus-summary-kill-same-subject nil))


(defun expire-summary-kill-same-subject-and-select (ask)
  "Add a local, expiring kill pattern for the current subject.
Also, mark all articles with this subject in the current buffer as
read and select the next unread article.  Argument ASK non-nil (C-u if
called interactively) allows the user to edit the pattern before it is
inserted."
  (interactive "P")
  (expire-summary-kill-using 'expire-kill-file-kill-by-subject ask)
  (gnus-summary-kill-same-subject-and-select nil))


(defun expire-summary-kill-thread (ask)
  "Add a local, expiring kill pattern for the current thread.  Also,
mark all articles in the current thread as read.  Argument ASK non-nil
(C-u if called interactively) allows the user to edit the pattern
before it is inserted."
  (interactive "P")
  (expire-summary-kill-using 'expire-kill-file-kill-by-thread ask)
  (gnus-summary-kill-thread nil))


(defun expire-summary-kill-thread-and-select (ask)
  "Add a local, expiring kill pattern for the current thread.  Also,
mark all articles in the current thread as read and select the next
unread article.  Argument ASK non-nil (C-u if called interactively)
allows the user to edit the pattern before it is inserted."
  (interactive "P")
  (expire-summary-kill-thread ask)
  (if (memq (gnus-summary-article-number)
	    gnus-newsgroup-unreads)
      (gnus-summary-select-article)
    (gnus-summary-next-unread-article)))

;;;; ------------------------------------------------------------
;;;; Internal-use hook functions.
;;;; ------------------------------------------------------------

(defun expire-apply-kill-file ()
  "Apply kill files to the current newsgroup.  The global kill file,
if it exists, is loaded in the standard manner.  The local kill file,
however, is read and evaluated one s-expression at a time.  This
allows calls to \"expire-kill\" to modify themselves.  If the local
kill file consists of nothing but whitespace, it may be deleted,
depending upon the value of expire-delete-empties.

The hook expire-after-apply-hook is executed after both the global and
local kill files have been applied.  If neither the global nor the
local kill file actually existed, though, this hook is ignored."
  (let (kill-files-applied)
    (let ((global-kill-file (gnus-newsgroup-kill-file nil)))
      (and (file-readable-p global-kill-file)
	   (progn
	     (message "Loading %s..." global-kill-file)
	     (load (gnus-newsgroup-kill-file nil) 'noerr nil 'nosufx)
	     (message "Loading %s...done" global-kill-file)
	     (setq kill-files-applied 't))))
    (let ((local-kill-file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (and (or (file-readable-p local-kill-file)
	       (get-file-buffer local-kill-file))
	   (save-excursion
	     (message "Loading %s..." local-kill-file)
	     (find-file local-kill-file)
	     (push (setq expire-current-kill-buffer (current-buffer))
		   expire-modified-buffers)
	     (goto-char (point-min))
	     (if (re-search-forward "[^ \t\r\n\f]" nil 'noerr)
		 (progn
		   (goto-char (point-min))
		   (let ((original-syntax-table (syntax-table)))
		     (set-syntax-table emacs-lisp-mode-syntax-table)
		     (condition-case nil
			 (expire-eval-buffer local-kill-file)
		       (end-of-file (set-syntax-table original-syntax-table))))
		   (setq kill-files-applied 't)
		   (message "Loading %s...done" local-kill-file)
		   (bury-buffer expire-current-kill-buffer)
		   (expire-possibly-flush 'always))
	       (if (and expire-delete-empties
			(not (file-symlink-p local-kill-file))
			(if (eq expire-delete-empties 'ask)
			    (y-or-n-p
			     (format "Delete empty %s " local-kill-file))
			  t))
		   (progn
		     (message "Deleting %s..." local-kill-file)
		     (delete-file local-kill-file)
		     (set-buffer-modified-p nil)
		     (kill-buffer nil)
		     (message "Deleting %s...done" local-kill-file))
		 (bury-buffer expire-current-kill-buffer)
		 (expire-possibly-flush 'always))))))
    (and kill-files-applied
	 (run-hooks 'expire-after-apply-hook))))


(defun expire-exit-group ()
  "Possibly flush all modified buffers.
Should be called from gnus-exit-group-hook."
  (expire-possibly-flush 'group))


(defun expire-exit-gnus ()
  "Possibly flush all modified buffers.
Should be called from gnus-exit-gnus-hook."
  (expire-possibly-flush 'session))

;;;; ------------------------------------------------------------
;;;; Internal-use buffer functions.
;;;; ------------------------------------------------------------

(defun expire-eval-buffer (name)
  "Evaluate the s-expressions following point in the current buffer,
one at a time.  NAME gives the buffer's displayed name."
  ;; Note: while this function could be quite elegant if written
  ;; tail-recursively, even optimizing byte-compilers have difficulty
  ;; making tail-recursion as efficient as a flat loop.  This is
  ;; primarily due to Lisp's dynamic scoping.
  (while 't
    (set-buffer gnus-summary-buffer)
    (eval (read expire-current-kill-buffer))
    (set-buffer expire-current-kill-buffer)
    (message "Loading %s...%d%%"
	     name
	     (/ (* 100 (point))   ;; Re-evaluate (point-max) each time,
		(point-max)))))   ;; as the buffer can change in size.


(defun expire-possibly-flush (frequency)
  "Possibly flush modified kill file buffers.
Flushing actually happens only if argument FREQUENCY and the variable
expire-flush-frequency are the same."
  (and (eq frequency expire-flush-frequency)
       (expire-flush)))


(defun expire-flush ()
  "Flush modified buffers as called for by expire-flush-action.
Depending on the value of this variable, we either save and kill the
buffers in expire-modified-buffers just save them, or don't do
anything.  Also, reset expire-modified-buffers to nil when we are done."
  ;; Note: while this function could be quite elegant if written
  ;; tail-recursively, even optimizing byte-compilers have difficulty
  ;; making tail-recursion as efficient as a flat loop.  This is
  ;; primarily due to Lisp's dynamic scoping.
  (while expire-modified-buffers
    (let ((buffer (pop expire-modified-buffers)))
      (and (buffer-name buffer)
	   (case expire-flush-action
		 (kill (expire-save-if-modified buffer)
		       (kill-buffer buffer))
		 (save (expire-save-if-modified buffer)))))))


(defun expire-save-if-modified (buffer)
  "Save BUFFER, but only if it has been modified.
This prevents unsightly \"(No changes need to be saved)\" messages."
  (and (buffer-modified-p buffer)
       (save-excursion
	 (set-buffer buffer)
	 (save-buffer)
	 (bury-buffer))))

;;;; ------------------------------------------------------------
;;;; Internal-use pattern functions.
;;;; ------------------------------------------------------------

(defun expire-convert-timestamp (timestamp)
  "Convert a time stamp to the user's preferred format.
If TIMESTAMP is a string, it is assumed to be in calc format; integers
are assumed to belong to calendar.  An equivalent string or integer is
returned, depending upon the value of expire-date-package."
  (cond ((integerp timestamp)
	 (case expire-date-package
	   (calendar timestamp)
	   (calc (require 'calendar)
		 (calc-eval "date($)" nil (1+ timestamp)))))
	((stringp timestamp)
	 (case expire-date-package
	   (calc timestamp)
	   (calendar (require 'calc)
		     (1- (calc-eval "date($)" 'raw timestamp)))))))


(defun expire-restamp (field pattern timestamp &optional command all)
  "Replace a call to \"expire-kill\" with one having an updated time
stamp.  The s-expression before the point is deleted, and a new one is
inserted that calls \"expire-kill\" with the given FIELD and REGEXP,
and the current time as its time stamp.  If the current time is not
different from TIMESTAMP, however, the buffer is not modified.
Optional arguments COMMAND and ALL correspond to those passed to the
original expire-kill call, and if given will be reproduced in the new
call."
(let ((now (expire-current-date)))
  (or (equal timestamp now)
      (progn (set-buffer expire-current-kill-buffer)
	     (delete-region (point)
			    (progn (backward-sexp)
				   (point)))
	     (delete-blank-lines)
	     (delete-blank-lines)
	     (expire-insert-kill field pattern nil now command all)))))


(defun expire-insert-kill (field pattern ask &optional timestamp command all)
  "General purpose function to produce \"expire-kill\" calls.
Inserts a call to \"expire-kill\" with the given FIELD and REGEXP.  If
third argument ASK is non-nil, the user will be allowed to edit the
regexp. An optional fourth argument provides the TIMESTAMP; if none is
given, a stamp for the current time is used.  Fifth and sixth optional
arguments COMMAND and ALL specify the corresponding optional arguments
to the expire-kill call."
  (prin1 (append (list 'expire-kill
		       field
		       (if ask
			   (read-from-minibuffer (concat field ":  ")
						 pattern)
			 pattern)
		       (or timestamp
			   (expire-current-date)))
		 (cond (all (list command all))
		       (command (list command))))
	 (current-buffer))
  (or (looking-at "\n")
      (insert ?\n)))


(defun expire-filter (timestamp)
  "Delete outdated calls to \"expire-kill.\"
If TIMESTAMP is older than the age limit given by expire-maximum-age,
delete the s-expression before the point.  Presumably, this
corresponds to an outdated \"expire-kill\" call."
  (and (> (expire-days-since timestamp)
	  expire-maximum-age)
       (progn
	 (set-buffer expire-current-kill-buffer)
	 (let ((end-of-sexp (point)))
	   (backward-sexp)
	   (delete-region (point) end-of-sexp)
	   (delete-blank-lines)
	   (delete-blank-lines)
	   (and (eobp)
		(insert ?\n))))))


(defun expire-summary-kill-using (kill-mode-function ask)
  "In the *Summary* buffer, add a new expiring kill pattern.
First argument FUNCTION should be the name of a function to be called,
with no arguments, in the local kill file to actually insert the new
pattern.  If second argument ASK is non-nil, allow the user to edit
the kill pattern before it is inserted."
  (let* ((gnus-current-kill-article (gnus-summary-article-number))
	 (kill-file (gnus-newsgroup-kill-file gnus-newsgroup-name))
	 (kill-directory (file-name-directory kill-file)))
    (save-window-excursion
      (or (file-exists-p kill-directory)
	  (make-directory kill-directory t))
      (find-file kill-file)
      (goto-char (if expire-append-kills
		     (point-max)
		   (point-min)))
      (funcall kill-mode-function ask)
      (push (current-buffer) expire-modified-buffers)
      (bury-buffer)
      (expire-possibly-flush 'always))))

;;;; ------------------------------------------------------------
;;;; Initialization.
;;;; ------------------------------------------------------------

;;; Bind each of the major user-callable functions, if the same-named
;;; variable is set to a string representing a key sequence.

(mapcar (function
	 (lambda (binding)
	   (let ((function (car binding))
		 (keymap (cdr binding)))
	     (and (stringp (symbol-value function))
		  (define-key
		    (symbol-value keymap)
		    (symbol-value function)
		    function)))))
	'((expire-summary-kill-same-subject . gnus-summary-mode-map)
	  (expire-summary-kill-same-subject-and-select . gnus-summary-mode-map)
	  (expire-summary-kill-thread . gnus-summary-mode-map)
	  (expire-summary-kill-thread-and-select . gnus-summary-mode-map)
	  (expire-kill-file-kill-by-subject . gnus-kill-file-mode-map)
	  (expire-kill-file-kill-by-thread . gnus-kill-file-mode-map)
	  (expire-kill-file-insert-time-stamp . gnus-kill-file-mode-map)))
	

;;; Install ourselves as the kill file applier of choice.
;;; The use of setq here instead of add-hook is intentional.

(setq gnus-apply-kill-hook 'expire-apply-kill-file)


;;; Plug in to some exit conditions that might prompt flushing.

(add-hook 'gnus-exit-group-hook 'expire-exit-group)
(add-hook 'gnus-exit-gnus-hook 'expire-exit-gnus)


;;; Announce our presence and call any user hooks.

(provide 'expirekill)
(provide 'expire-kill)
(run-hooks 'expire-load-hook)


;;; expirekill.el ends here

--PART-BOUNDARY=.19409192222.ZM974.z-code.com--

