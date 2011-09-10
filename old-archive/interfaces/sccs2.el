;;;;
;;;;	sccs.el
;;;;
;;;;	"SCCS: The condom of promiscuous programmers"
;;;;
;;;;	"SCCS, the source motel!  Programs check in and never check out!"
;;;;	 -- Ken Thompson
;;;;
;;;;	Written and maintained by Johnathan Vail (JV), Tegra Varityper
;;;;		vail@tegra.com
;;;;		(508) 663-7435
;;;;	Modified by Kevin Rodgers (KR), Martin Marietta
;;;;		kevin@traffic.den.mmc.com
;;;;		(303) 790-3971

;;;;	Revision History:
;;;;	19 Feb 1993 KR	- sccs-do: check for null buffer-file-name
;;;;	 2 Feb 1993 KR	- sccs-do: reworked compilation-error-list parsing
;;;;	11 Jan 1993 KR	- defined sccs-read-comment and count-string-matches
;;;;	21 Dec 1992 KR	- sccs-prefix bound in global keymap, define sccs-prs
;;;;	 3 Dec 1992 KR	- modularize the "Display SCCS output" code in sccs-do
;;;;	 3 Dec 1992 JV	- incorporate new changes
;;;;	30 Nov 1992 KR	- wrap calls to sccs-delta-comments in save-excursion
;;;;	30 Nov 1992 KR	- sccs-delget and -deledit can supply a default comment
;;;;	30 Nov 1992 KR	- Added sccs-delta-comments logic to sccs-deledit
;;;;	25 Nov 1992 JV	- Changes to fold back to my original code
;;;;	19 Nov 1992 KR	- Fixed computation of current line in sccs-do
;;;;	10 May 1992 KR	- Defined sccs-clean, -deledit, -unedit, and -diffs
;;;;	10 May 1992 KR	- sccs-delget: tegra-datestamp -> sccs-delta-comments
;;;;	10 May 1992 KR	- sccs-do: display *SCCS* buffer, instead of message
;;;;	10 May 1992 KR	- sccs-do: next-error hacks simplified
;;;;	 9 May 1992 KR	- Changed keybinding scheme to use local keymap
;;;;	14 Nov 1989 JV	- Examine error list to decide to reparse errors
;;;;	10 Nov 1989 JV	- Add next-error hacks
;;;;	 8 Nov 1989 JV	- Add Comment defaults
;;;;	 7 Nov 1989 JV	- Fix column position bug
;;;;	 6 Nov 1989 JV	- Another day, another hack

;;;;	Purpose:
;;;;	Perform common 'SCCS' commands from within Emacs.
;;;;	See sccs-keymap for the defined functions.

;;;;	Installation:
;;;;	1. Put this file in a directory that is a member of load-path, and
;;;;	   byte-compile it for better performance.
;;;;	2. Put this form in ~/.emacs:
;;;;	   (require 'sccs)
;;;;    3. Users on systems which do not support the SCCS 'prt' subcommand 
;;;;	   (e.g. SGI) should also put this form in ~/.emacs:
;;;;	   (define-key sccs-keymap "p" (function sccs-prs))
;;;;	4. Users who wish to supply multi-line comments when checking in
;;;;	   files with sccs-delget and -deledit may want to see sccs-
;;;;	   comment.el; those who wish to use JV's customization should
;;;;	   see the comment block above sccs-delta-comments' definition.

;;;;	Usage:
;;;;	While visiting a file or directory, invoke the sccs commands for
;;;;	information, or to check the file in or out (as appropriate) of
;;;;	SCCS.

;;;;	Known bugs:
;;;;	1. Some systems do not support the SCCS prt subcommand.  The work-
;;;;	   around is described under "Installation", above.

;;;;	LCD Archive Entry:
;;;;	sccs2|Johnathan Vail, Kevin Rodgers|
;;;;    vail@tegra.com, kevin@traffic.den.mmc.com|
;;;;	Emacs interface for common SCCS operations.|
;;;;	1993-02-23|2.0|~/packages/sccs2.el.Z|


;;;;	Copyright notice:
;;;;    Copyright (C) 1993 Johnathan Vail
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 1, or (at your option)
;;;;    any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program; if not, write to the Free Software
;;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;


;;;;	To do:
;;;;    1. Find a way to pass flags other than -rsid and -ycomment to 'sccs'
;;;;       commands (e.g. 'sccs diffs -C'); either a global variable
;;;;       sccs-command-args that can be lambda-bound, or a property:
;;;;       (get 'diffs 'sccs-command-args) => ("-C")
;;;;    2. Provide appropriate hooks.


(provide 'sccs)


;;; Keybindings:

(defvar sccs-keymap nil
  "Keymap for bindings of 'SCCS' functions:
\\{sccs-keymap}
")

(if (null sccs-keymap)
    (progn
      (setq sccs-keymap (make-sparse-keymap))
      (define-key sccs-keymap "i" (function sccs-info))
      (define-key sccs-keymap "p" (function sccs-prt))
      (define-key sccs-keymap "d" (function sccs-diffs))
      (define-key sccs-keymap "c" (function sccs-create))
      (define-key sccs-keymap "C" (function sccs-clean))
      (define-key sccs-keymap "g" (function sccs-get))
      (define-key sccs-keymap "e" (function sccs-edit))
      (define-key sccs-keymap "D" (function sccs-delget))
      (define-key sccs-keymap "E" (function sccs-deledit))
      (define-key sccs-keymap "U" (function sccs-unedit))
      (define-key sccs-keymap "o" (function sccs-display-output))))

(defconst sccs-prefix "\C-c\C-s"
  "*Prefix key for sccs-keymap in global keymap, if non-nil.
The value of sccs-prefix is only used when the 'sccs' library is
initially loaded.

Here's how to bind 'sccs' commands locally instead of globally:
(setq find-file-hooks
      (cons (function (lambda ()
			(let ((sccs-prefix nil)) ; prevent global binding
			  (if (and (file-directory-p \"SCCS\")
				   (or (not (featurep 'sccs))
				       (not (eq (key-binding local-sccs-prefix)
						sccs-keymap))))
			      (progn
				(load-library \"sccs\")
				(local-set-key \"\\C-c\\C-c\" sccs-keymap))))))
	    find-file-hooks))
")

(if sccs-prefix
    (global-set-key sccs-prefix sccs-keymap))


;; Options:

;;
;; sccs-delta-comments
;;
;; This variable can be used for generating a default comment string
;; which can be edited in the mini-buffer.  The following is what I have
;; in my .emacs file for picking the first comment string I put in the
;; file of the current day.  I include it here as an example of one way
;; to use this variable. [JV]
;;
;;(defun tegra-get-comment-for-sccs ()
;;  "Gets the comment from today's timestamp entry (if any)"
;;
;;  (save-excursion
;;    (and (fboundp 'tegra-datestamp)
;;	 (goto-char (point-min))
;;	 (re-search-forward (concat (tegra-datestamp) "\\(.*$\\)") nil t)
;;	 (buffer-substring (match-beginning 1) (match-end 1)))))
;;
;;(setq sccs-delta-comments (function tegra-get-comment-for-sccs))
;;

(defvar sccs-delta-comments nil
  "*Either nil or a function that takes no arguments and returns a string.
If not nil, its value is invoked by sccs-delget and -deledit to provide a
default 'SCCS' delta comment when they are called interactively; when they
are called non-interactively with a nil COMMENTS arg, its value (when non-
nil) is invoked to generate the comment.")


;;
;; sccs-buffer-name
;;

(defvar sccs-buffer-name "*SCCS*"
  "*The name of the buffer to which 'SCCS' output is sent.")

;;
;; sccs-show-output
;;

(defvar sccs-show-output nil
  "*If non-nil, display the output from 'SCCS' commands in a buffer;
otherwise, display a synopsis of the output in the echo area.")



;;; SCCS funtions:


(defun sccs-info ()
  "*Display the files in the current directory checked out under 'SCCS'
for editing.  Invoked by \\[sccs-info]."
  (interactive)
  (let ((sccs-show-output t))
    (sccs-do "info" nil)))


(defun sccs-prt ()
  "*Display the 'SCCS' history for the current file.  Invoked by \\[sccs-prt]."
  (interactive)
  (let ((sccs-show-output t))
    (sccs-do "prt" buffer-file-name)))

(defun sccs-prs ()
  "*Display the 'SCCS' history for the current file.  Invoked by \\[sccs-prs]."
  (interactive)
  (let ((sccs-show-output t))
    (sccs-do "prs" buffer-file-name)))


(defun sccs-diffs (version)
  "*If the current buffer is not read-only, compare the file (on disk) to the
VERSION under 'SCCS' control.  Invoked by \\[sccs-diffs]."
  (interactive "sVersion [most recent]: ")
  (if buffer-read-only
      (error "Current buffer is read-only")
    (progn
      (if (buffer-modified-p)
	  ;; Give the user the option to save the buffer:
	  (if (y-or-n-p "Current buffer has been modified; save it? ")
	      (save-buffer)))
      (sccs-do "diffs" buffer-file-name version))))


(defun sccs-create ()
  "*If the current buffer is read-only or has not been modified, put the
current file under 'SCCS' control.  Invoked by \\[sccs-create]."
  (interactive)
  (if (or buffer-read-only (not (buffer-modified-p)))
      (sccs-do "create" t)
    (error "Current buffer is not read-only or has been modified")))


(defun sccs-clean ()
  "*Remove all files checked-in under 'SCCS' from the current directory.
Invoked by \\[sccs-clean]."
  (interactive)
  (sccs-do "clean"))


(defun sccs-get (version)
  "*If the current buffer is read-only, retrieve the 'SCCS' VERSION (which
defaults to the most recent delta id) of the current file into the buffer.
Invoked by \\[sccs-get]."
  (interactive "sVersion [most recent]: ")
  (if buffer-read-only
      (sccs-do "get" t version)
    (error "Current buffer is not read-only")))


(defun sccs-edit ()
  "*If the current buffer is read only, retrieve a new version of the file
>from 'SCCS' for editing into the buffer.  Invoked by \\[sccs-edit]."
  (interactive)
  (if buffer-read-only
      (sccs-do "edit" t)
    (error "Current buffer is not read-only")))


(defun sccs-delget (comments)
  "*If the current buffer is not read-only and has not been modified (or is
saved upon query), check it in under 'SCCS' with the annotation COMMENTS, and
retrieve the new version into the buffer.
If COMMENTS is nil and sccs-delta-comments is not nil, it's value will be
invoked and the return value will be used instead.
Invoked by \\[sccs-delget]."
  (interactive (list (sccs-read-comment (if sccs-delta-comments
					    (save-excursion
					      (funcall sccs-delta-comments))))))
  (if (and (not buffer-read-only)
	   (or (not (buffer-modified-p))
	       (and (y-or-n-p "Current buffer has been modified; save it? ")
		    (progn
		      (save-buffer)
		      t))))
      (progn
	(if (and (not (interactive-p))
		 (null comments)
		 sccs-delta-comments)
	    (setq comments
		  (save-excursion
		    (funcall sccs-delta-comments))))
	(sccs-do "delget" t nil comments))
    (error "Current buffer is read-only or has been modified")))


(defun sccs-deledit (comments)
  "*If the current buffer is not read-only and has not been modified (or is
saved upon query), check it in under 'SCCS' with the annotation COMMENTS, get
a new version for editing and retrieve it into the buffer.
If COMMENTS is nil and sccs-delta-comments is not nil, it's value will be
invoked and the return value will be used instead.
Invoked by \\[sccs-deledit]."
  (interactive (list (sccs-read-comment (if sccs-delta-comments
					    (save-excursion
					      (funcall sccs-delta-comments))))))
  (if (and (not buffer-read-only)
	   (or (not (buffer-modified-p))
	       (and (y-or-n-p "Current buffer has been modified; save it? ")
		    (progn
		      (save-buffer)
		      t))))
      (progn
	(if (and (not (interactive-p))
		 (null comments)
		 sccs-delta-comments)
	    (setq comments
		  (save-excursion
		    (funcall sccs-delta-comments))))
	(sccs-do "deledit" t nil comments))
    (error "Current buffer is read-only or has been modified")))


(defun sccs-unedit ()
  "*If the current buffer is not read-only, revoke the 'SCCS' edit (and any
changes written to the file), and retrieve the most recent version into the
buffer.  Invoked by \\[sccs-unedit]."
  (interactive)
  (if (not buffer-read-only)
      (sccs-do "unedit" t)
    (error "Current buffer is read-only")))


(defun sccs-display-output ()
  "*Display the output from the most recent 'SCCS' command in a buffer."
  (interactive)
  (let ((sccs-buffer (get-buffer sccs-buffer-name)))
    (if sccs-buffer
	(let ((restore-display-key;; see print-help-return-message in help.el
	       (substitute-command-keys
		(if (one-window-p t)
		    (if pop-up-windows
			"\\[delete-other-windows]"
		      "\\[switch-to-buffer] RET")
		  "\\[switch-to-buffer-other-window] RET"))))
	  (display-buffer sccs-buffer)
	  (message (format "Type %s to restore display without %s buffer"
			   restore-display-key sccs-buffer-name)))
      (error "'%s' buffer does not exist." sccs-buffer-name))))


;;;
;;; SCCS utility:
;;;


(defun sccs-do (command file &optional version comments)
  "Exececute sccs subcommand COMMAND on FILE (may be nil) with optional
arguments VERSION and COMMENTS (may be empty strings).  If FILE is t,
use the file currently visited and re-visit it after the 'SCCS'
operation \(invalidating the mark-ring\); then if compilation-error-list
is also bound to a list, reparse it."
  ;; Translate FILE to filename and get an 'SCCS' output buffer:
  (let ((filename
	 (cond ((null file) nil)
	       ((eq file 't)
		(if buffer-file-name
		    (file-name-nondirectory buffer-file-name)
		  (error "Current buffer is not visiting a file.")))
	       ((stringp file) (file-name-nondirectory file))
	       (t nil)))
	(directory
	 (cond ((or (null file)
		    (eq file 't))
		(if buffer-file-name
		    (file-name-directory buffer-file-name)
		  default-directory))
	       ((stringp file) (file-name-directory file))
	       (t "/"))))
    ;; Translate optional arguments to SCCS subcommand flags:
    (cond ((null version))
	  ((string-equal version "") (setq version nil))
	  (t (setq version (concat "-r" version))))
    (cond ((null comments))
	  ((string-equal comments "") (setq comments nil))
	  (t (setq comments (concat "-y" comments))))
    ;; Execute SCCS command and capture its output in a buffer:
    (save-excursion
      (set-buffer (get-buffer-create sccs-buffer-name))
      (setq default-directory directory)
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (let ((sccs-args
	     (nconc (if version (list version))
		    (if comments (list comments))
		    (if filename (list filename)))))
	(apply (function message) "sccs %s %s..." command sccs-args)
	(apply (function call-process) "sccs" nil t nil command sccs-args)
	(apply (function message) "sccs %s %s...Done" command sccs-args))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    ;; Re-visit FILE:
    (if (eq file 't)
	(let* ((column (current-column))
	       (line (+ (count-lines (point-min) (point))
			(if (zerop column) 1 0)))
	       (reparse (and (boundp 'compilation-error-list)
			     (listp compilation-error-list)
			     (let ((buffer (current-buffer))
				   (errors compilation-error-list)
				   (buffer-error-marked-p nil))
			       (while (and errors (not buffer-error-marked-p))
				 (if (eq (marker-buffer
					  (car (cdr (car errors))))
					 buffer)
				     (setq buffer-error-marked-p t))
				 (setq errors (cdr errors)))
			       buffer-error-marked-p))))
	  (find-alternate-file filename)
	  ;; Restore point:
	  (goto-line line)
	  (move-to-column column)
	  ;; Reparse remaining *compilation* errors, if necessary:
	  (if reparse			; see next-error (compile.el)
	      (save-excursion
		(set-buffer "*compilation*")
		(set-buffer-modified-p nil) ; ?
		(if (consp compilation-error-list) ; not t, nor ()
		    (setq compilation-parsing-end
			  (marker-position
			   (car (car compilation-error-list)))))
		(compilation-forget-errors)
		(compilation-parse-errors)))))
    ;; Display SCCS output:
    (if (or sccs-show-output
	    (null (sccs-display-message command filename)))
	(sccs-display-output))))


;; sccs-display-message options (not to be configured by users):

(defvar sccs-error-regexp "\\'ERROR.*$"
  "Regular expression to match 'SCCS' error messages.")

(defvar sccs-edit-regexp
  "^\\([0-9.]+\\)[^0-9.]new delta \\([0-9.]+\\)[^0-9.]\\([0-9]+\\) lines"
  "Regular expression to match 'SCCS' edit messages.
The first subexpression matches the old version number,
the second subexpression matches new version number, and
the third subexpression matches the number of lines in the file.")

(defvar sccs-delta-regexp
  "^\\([0-9.]+\\)[^0-9.]\\([0-9.]+ inserted\\)[^0-9.]\\([0-9]+ deleted\\)$"
  "Regular expression to match 'SCCS' diff summary.
The first subexpression matches the version number,
the second subexpression matches reported insertions,
and the third subexpression matches the reported deletions.")

(defvar sccs-version-regexp "^[0-9.]+$"
  "Regular expression to match the most recent 'SCCS' version.")

  
(defun sccs-display-message (&optional command filename)
  "Display a synopsis of the output of the most recent 'SCCS' command
in the echo area, prefixed by optional arguments COMMAND and FILENAME.
Returns nil if an error was reported or if the output format is not
recognized."
  (let ((sccs-buffer (get-buffer sccs-buffer-name))
	(synopsis nil)
	(result t))
    (if sccs-buffer
	(save-excursion
	  (set-buffer sccs-buffer)
	  (goto-char (point-min))
	  (cond ((looking-at sccs-error-regexp)
		 (setq synopsis (buffer-substring (match-beginning 0)
						  (match-end 0))
		       result nil))
		((re-search-forward sccs-edit-regexp nil t)
		 (setq synopsis
		       (format "%s -> %s (%s lines)"
			       (buffer-substring (match-beginning 1)
						 (match-end 1))
			       (buffer-substring (match-beginning 2)
						 (match-end 2))
			       (buffer-substring (match-beginning 3)
						 (match-end 3)))))
		((re-search-forward sccs-delta-regexp nil t)
		 (setq synopsis
		       (format "-> %s (%s, %s)"
			       (buffer-substring (match-beginning 1)
						 (match-end 1))
			       (buffer-substring (match-beginning 2)
						 (match-end 2))
			       (buffer-substring (match-beginning 3)
						 (match-end 3)))))
		((re-search-forward sccs-version-regexp nil t)
		 (setq synopsis
		       (format "-> %s"
			       (buffer-substring (match-beginning 0)
						 (match-end 0)))))
		(t (setq result nil)))
	  (if synopsis
	      (message "sccs %s %s: %s"
		       (or command "")
		       (if filename (file-name-nondirectory filename) "")
		       synopsis))
	  result)
      (error "'%s' buffer does not exist." sccs-buffer-name))))


(defun sccs-read-comment (default)
  "Prompt for and read an 'SCCS' comment string, with DEFAULT as the
initial contents of the minibuffer.  DEFAULT may be nil; if it is a
string, it may contain embedded newlines."
  (let ((enable-recursive-minibuffers t)) ; select-window ... read-string
    (save-window-excursion
      (select-window (minibuffer-window))
      (enlarge-window (- (+ (count-string-matches "\n" (or default ""))
			    1)
			 (window-height (selected-window))))
      (read-string "Comments: " default))))

(defun count-string-matches (regexp string &optional start)
  "Return the number of matches for REGEXP in STRING.  If optional
argument START is non-nil, count matches from that index in STRING."
  (let ((match-data (match-data))
	(count 0))
    (unwind-protect
	(while (string-match regexp string start)
	  (setq count (1+ count)
		start (match-end 0)))
      (store-match-data match-data))
    count))
