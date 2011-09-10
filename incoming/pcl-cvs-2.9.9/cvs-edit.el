;;; cvs-edit.el --- Major mode for editing CVS commit messages

;; Copyright (C) 1999-2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: pcl-cvs cvs commit log
;; Version: v2_9_9
;; Revision: cvs-edit.el,v 1.8 2000/03/05 21:32:21 monnier Exp

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

;; Todo:

;; - Remove a single leading `* <file>' in cvs-edit-insert-changelog
;; - Rename to something like log-message or vc-log
;; - Move in VC's code
;; - Add compatibility for VC's hook variables

;;; Code:

(eval-when-compile (require 'cl))
(require 'add-log)			; for all the ChangeLog goodies
(require 'pcl-cvs-util)
(require 'ring)
(require 'vc)

;;;; 
;;;; Global Variables
;;;; 

(defgroup cvs-edit nil
  "Major mode for editing commit messages for PCL-CVS."
  :group 'pcl-cvs
  :prefix "cvs-edit-")

;; compiler pacifiers
(defvar cvs-buffer)

(cvs-defmap cvs-edit-mode-map
  `(("\C-c\C-c" . cvs-edit-done)
    ("\C-c\C-a" . cvs-edit-insert-changelog)
    ("\C-c\C-f" . cvs-edit-show-files)
    ("\C-c?" . cvs-edit-mode-help))
  "Keymap for the `cvs-edit-mode' (used when editing cvs log messages)."
  :group 'cvs-edit
  :inherit (if (boundp 'vc-log-entry-mode) vc-log-entry-mode))

(defcustom cvs-edit-confirm t
  "*If non-nil, `cvs-edit-done' will request confirmation.
If 'changed, only request confirmation if the list of files has
  changed since the beginning of the cvs-edit session."
  :group 'cvs-edit
  :type '(choice (const changed) (const t) (const nil)))

(defcustom cvs-edit-keep-buffer nil
  "*If non-nil, don't hide the buffer after `cvs-edit-done'."
  :group 'cvs-edit
  :type 'boolean)

(defvar cvs-commit-buffer-require-final-newline t
  "Obsolete, use `cvs-edit-require-final-newline'.")

(defcustom cvs-edit-require-final-newline
  cvs-commit-buffer-require-final-newline
  "*Enforce a newline at the end of commit log messages.
Enforce it silently if t, query if non-nil and don't do anything if nil."
  :group 'cvs-edit
  :type '(choice (const ask) (const t) (const nil)))

(defcustom cvs-edit-setup-invert nil
  "*Non-nil means `cvs-edit' should invert the meaning of its SETUP arg.
If SETUP is 'force, this variable has no effect."
  :group 'cvs-edit
  :type 'boolean)

(defcustom cvs-edit-hook '(cvs-edit-insert-cvs-template
			   cvs-edit-insert-changelog)
  "*Hook run at the end of `cvs-edit'."
  :group 'cvs-edit
  :type '(hook :options (cvs-edit-insert-cvs-template
			 cvs-edit-insert-changelog)))

(defcustom cvs-edit-mode-hook nil
  "*Hook run when entering `cvs-edit-mode'."
  :group 'cvs-edit
  :type 'hook)

(defcustom cvs-edit-done-hook nil
  "*Hook run before doing the actual commit.
This hook can be used to cleanup the message, enforce various
conventions, or to allow recording the message in some other database,
such as a bug-tracking system.  The list of files about to be committed
can be obtained from `cvs-edit-files'."
  :group 'cvs-edit
  :type '(hook :options (cvs-edit-delete-common-indentation
			 cvs-edit-add-to-changelog)))

(defvar cvs-changelog-full-paragraphs t
  "*If non-nil, include full ChangeLog paragraphs in the CVS log.
This may be set in the ``local variables'' section of a ChangeLog, to
indicate the policy for that ChangeLog.

A ChangeLog paragraph is a bunch of log text containing no blank lines;
a paragraph usually describes a set of changes with a single purpose,
but perhaps spanning several functions in several files.  Changes in
different paragraphs are unrelated.

You could argue that the CVS log entry for a file should contain the
full ChangeLog paragraph mentioning the change to the file, even though
it may mention other files, because that gives you the full context you
need to understand the change.  This is the behaviour you get when this
variable is set to t.

On the other hand, you could argue that the CVS log entry for a change
should contain only the text for the changes which occurred in that
file, because the CVS log is per-file.  This is the behaviour you get
when this variable is set to nil.")

;;;; Internal global or buffer-local vars

(defconst cvs-edit-files-buf "*cvs-edit-files*")
(defvar cvs-edit-initial-files nil)
(defvar cvs-edit-callback nil)
(defvar cvs-edit-listfun nil)

;;;; 
;;;; Actual code
;;;; 

;;;###autoload
(defun cvs-edit (callback &optional setup listfun &rest ignore)
  "Setup a buffer to enter a log message.
The buffer will be put in `cvs-edit-mode'.
If SETUP is non-nil, the buffer is then erased and `cvs-edit-hook' is run.
Mark and point will be set around the entire contents of the
buffer so that it is easy to kill the contents of the buffer with \\[kill-region].
Once you're done editing the message, pressing \\[cvs-edit-done] will call
`cvs-edit-done' which will end up calling CALLBACK to do the actual commit."
  (when (and cvs-edit-setup-invert (not (eq setup 'force)))
    (setq setup (not setup)))
  (when setup (erase-buffer))
  (cvs-edit-mode)
  (set (make-local-variable 'cvs-edit-callback) callback)
  (set (make-local-variable 'cvs-edit-listfun) listfun)
  (when setup (run-hooks 'cvs-edit-hook))
  (goto-char (point-min)) (push-mark (point-max))
  (set (make-local-variable 'cvs-edit-initial-files) (cvs-edit-files))
  (message (substitute-command-keys
	    "Press \\[cvs-edit-done] when you are done editing.")))

(define-derived-mode cvs-edit-mode text-mode "CVS-Edit"
  "Major mode for entering commit messages.
This mode is intended for entering messages in a *cvs-commit*
buffer when using PCL-CVS.  It provides a binding for the
\\[cvs-edit-done] command that should be used when done editing
to trigger the actual commit, as well as a few handy support
commands.
\\{cvs-edit-mode-map}")

(defun cvs-edit-hide-buf (&optional buf where)
  (when (setq buf (get-buffer (or buf cvs-edit-files-buf)))
    (let ((win (get-buffer-window buf where)))
      (if win (ignore-errors (delete-window win))))
    (bury-buffer buf)))

(defun cvs-edit-done ()
  "Finish editing the log message and commit the files.
This can only be used in the *cvs-commit* buffer.
With a prefix argument, prompt for cvs commit flags.
If you want to abort the commit, simply delete the buffer."
  (interactive)
  (if (and (> (point-max) 1)
	   (/= (char-after (1- (point-max))) ?\n)
	   (or (eq cvs-edit-require-final-newline t)
	       (and cvs-edit-require-final-newline
		    (y-or-n-p
		     (format "Buffer %s does not end in newline.  Add one? "
			     (buffer-name))))))
      (save-excursion
	(goto-char (point-max))
	(insert ?\n)))
  (if (boundp 'vc-comment-ring) (ring-insert vc-comment-ring (buffer-string)))
  (let ((win (get-buffer-window cvs-edit-files-buf)))
    (if (and cvs-edit-confirm
	     (not (and (eq cvs-edit-confirm 'changed)
		       (equal (cvs-edit-files) cvs-edit-initial-files)))
	     (progn
	       (cvs-edit-show-files)
	       (not (y-or-n-p "Really commit ? "))))
	(progn (when (not win) (cvs-edit-hide-buf))
	       (message "Oh, well!  Later maybe?"))
      (run-hooks 'cvs-edit-done-hook)
      (cvs-edit-hide-buf)
      (unless cvs-edit-keep-buffer
	(cvs-bury-buffer (current-buffer)
			 (when (boundp 'cvs-buffer) cvs-buffer)))
      (call-interactively cvs-edit-callback))))

(defun cvs-edit-files ()
  "Return the list of files that are about to be committed."
  (ignore-errors (funcall cvs-edit-listfun)))


(defun cvs-edit-insert-changelog ()
  "Insert a log message by looking at the ChangeLog.
The idea is to write your ChangeLog entries first, and then use this
command to commit your changes.

To select default log text, we:
- find the ChangeLog entries for the files to be checked in,
- verify that the top entry in the ChangeLog is on the current date
  and by the current user; if not, we don't provide any default text,
- search the ChangeLog entry for paragraphs containing the names of
  the files we're checking in, and finally
- use those paragraphs as the log text."
  (interactive)
  (cvs-insert-changelog-entries (cvs-edit-files))
  (cvs-edit-delete-common-indentation))

(defun cvs-edit-mode-help ()
  "Provide help for the `cvs-edit-mode-map'."
  (interactive)
  (if (eq last-command 'cvs-edit-mode-help)
      (describe-function major-mode)
    (message
     (substitute-command-keys
      "Type `\\[cvs-edit-done]' to finish commit.  Try `\\[describe-function] cvs-edit-done' for more help."))))

(defun cvs-edit-delete-common-indentation ()
  "Unindent the current buffer rigidly until at least one line is flush left."
  (save-excursion
    (let ((common (point-max)))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if (not (looking-at "^[ \t]*$"))
            (setq common (min common (current-indentation))))
        (forward-line 1))
      (indent-rigidly (point-min) (point-max) (- common)))))

(defun cvs-edit-show-files ()
  "Show the list of files to be committed."
  (interactive)
  (let* ((files (cvs-edit-files))
	 (editbuf (current-buffer))
	 (buf (get-buffer-create "*cvs-edit-files*")))
    (with-current-buffer buf
      (cvs-edit-hide-buf buf 'all)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (mapconcat 'identity files "\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (save-selected-window
	(cvs-pop-to-buffer-same-frame buf)
	(shrink-window-if-larger-than-buffer)
	(selected-window)))))

(defun cvs-edit-insert-cvs-template ()
  "Insert the template specified by the CVS administrator, if any."
  (interactive)
  (when (file-readable-p "CVS/Template")
    (insert-file-contents "CVS/Template")))
  

(defun cvs-edit-add-to-changelog ()
  "Insert this log message into the appropriate ChangeLog file."
  (interactive)
  ;; Yuck!
  (unless (string= (buffer-string) (ring-ref vc-comment-ring 0))
    (ring-insert vc-comment-ring (buffer-string)))
  (dolist (f (cvs-edit-files))
    (let ((buffer-file-name (expand-file-name f)))
      (save-excursion
	(vc-comment-to-change-log)))))

;;;; 
;;;; functions for getting commit message from ChangeLog a file...
;;;; Courtesy Jim Blandy
;;;; 

(defun cvs-narrow-changelog ()
  "Narrow to the top page of the current buffer, a ChangeLog file.
Actually, the narrowed region doesn't include the date line.
A \"page\" in a ChangeLog file is the area between two dates."
  (or (eq major-mode 'change-log-mode)
      (error "cvs-narrow-changelog: current buffer isn't a ChangeLog"))

  (goto-char (point-min))

  ;; Skip date line and subsequent blank lines.
  (forward-line 1)
  (if (looking-at "[ \t\n]*\n")
      (goto-char (match-end 0)))

  (let ((start (point)))
    (forward-page 1)
    (narrow-to-region start (point))
    (goto-char (point-min))))

(defun cvs-changelog-paragraph ()
  "Return the bounds of the ChangeLog paragraph containing point.
If we are between paragraphs, return the previous paragraph."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*$")
        (skip-chars-backward " \t\n" (point-min)))
    (list (progn
            (if (re-search-backward "^[ \t]*\n" nil 'or-to-limit)
                (goto-char (match-end 0)))
            (point))
          (if (re-search-forward "^[ \t\n]*$" nil t)
              (match-beginning 0)
            (point)))))

(defun cvs-changelog-subparagraph ()
  "Return the bounds of the ChangeLog subparagraph containing point.
A subparagraph is a block of non-blank lines beginning with an asterisk.
If we are between sub-paragraphs, return the previous subparagraph."
  (save-excursion
    (end-of-line)
    (if (search-backward "*" nil t)
        (list (progn (beginning-of-line) (point))
              (progn 
                (forward-line 1)
                (if (re-search-forward "^[ \t]*[\n*]" nil t)
                    (match-beginning 0)
                  (point-max))))
      (list (point) (point)))))

(defun cvs-changelog-entry ()
  "Return the bounds of the ChangeLog entry containing point.
The variable `cvs-changelog-full-paragraphs' decides whether an
\"entry\" is a paragraph or a subparagraph; see its documentation string
for more details."
  (if cvs-changelog-full-paragraphs
      (cvs-changelog-paragraph)
    (cvs-changelog-subparagraph)))

(defvar user-full-name)
(defvar user-mail-address)
(defun cvs-changelog-ours-p ()
  "See if ChangeLog entry at point is for the current user, today.
Return non-nil iff it is."
  ;; Code adapted from add-change-log-entry.
  (let ((name (or (and (boundp 'add-log-full-name) add-log-full-name)
		  (and (fboundp 'user-full-name) (user-full-name))
		  (and (boundp 'user-full-name) user-full-name)))
        (mail (or (and (boundp 'add-log-mailing-address) add-log-mailing-address)
		  ;;(and (fboundp 'user-mail-address) (user-mail-address))
		  (and (boundp 'user-mail-address) user-mail-address)))
	(time (or (and (boundp 'add-log-time-format)
		       (functionp add-log-time-format)
		       (funcall add-log-time-format))
		  (format-time-string "%Y-%m-%d"))))
    (looking-at (regexp-quote (format "%s  %s  <%s>" time name mail)))))

(defun cvs-changelog-entries (file)
  "Return the ChangeLog entries for FILE, and the ChangeLog they came from.
The return value looks like this:
  (LOGBUFFER (ENTRYSTART . ENTRYEND) ...)
where LOGBUFFER is the name of the ChangeLog buffer, and each
\(ENTRYSTART . ENTRYEND\) pair is a buffer region."
  (save-excursion
    (let ((changelog-file-name 
	   (let ((default-directory
		   (file-name-directory (expand-file-name file))))
	     ;; `find-change-log' uses `change-log-default-name' if set
	     ;; and sets it before exiting, so we need to work around
	     ;; that memoizing which is undesired here
	     (setq change-log-default-name nil)
	     (find-change-log))))
      (set-buffer (find-file-noselect changelog-file-name))
      (unless (eq major-mode 'change-log-mode) (change-log-mode))
      (goto-char (point-min))
      (if (looking-at "\\s-*\n") (goto-char (match-end 0)))
      (if (not (cvs-changelog-ours-p))
	  (list (current-buffer))
	(save-restriction
	  (cvs-narrow-changelog)
	  (goto-char (point-min))
	  
	  ;; Search for the name of FILE relative to the ChangeLog.  If that
	  ;; doesn't occur anywhere, they're not using full relative
	  ;; filenames in the ChangeLog, so just look for FILE; we'll accept
	  ;; some false positives.
	  (let ((pattern (file-relative-name
			  file (file-name-directory changelog-file-name))))
	    (if (or (string= pattern "")
		    (not (save-excursion
			   (search-forward pattern nil t))))
		(setq pattern (file-name-nondirectory file)))

	    (let (texts)
	      (while (search-forward pattern nil t)
		(let ((entry (cvs-changelog-entry)))
		  (push entry texts)
		  (goto-char (elt entry 1))))

	      (cons (current-buffer) texts))))))))

(defun cvs-changelog-insert-entries (buffer regions)
  "Insert those regions in BUFFER specified in REGIONS.
Sort REGIONS front-to-back first."
  (let ((regions (sort regions 'car-less-than-car))
        (last))
    (dolist (region regions)
      (when (and last (< last (car region))) (newline))
      (setq last (elt region 1))
      (apply 'insert-buffer-substring buffer region))))

(defun cvs-insert-changelog-entries (files)
  "Given a list of files FILES, insert the ChangeLog entries for them."
  (let ((buffer-entries nil))

    ;; Add each buffer to buffer-entries, and associate it with the list
    ;; of entries we want from that file.
    (dolist (file files)
      (let* ((entries (cvs-changelog-entries file))
             (pair (assq (car entries) buffer-entries)))
        (if pair
            (setcdr pair (cvs-union (cdr pair) (cdr entries)))
          (push entries buffer-entries))))

    ;; Now map over each buffer in buffer-entries, sort the entries for
    ;; each buffer, and extract them as strings.
    (dolist (buffer-entry buffer-entries)
      (cvs-changelog-insert-entries (car buffer-entry) (cdr buffer-entry))
      (when (cdr buffer-entry) (newline)))))

(provide 'cvs-edit)
;;; cvs-edit.el ends here
