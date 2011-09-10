;;; GENERIC VERSION MANAGEMENT INTERFACE.
;;;
;;; This file contains various routines that connect to various
;;; configuration management tools and various styles of checking in and
;;; checking out files.  It works with RCS and SCCS.  It can easily be
;;; extended to work with RCS or SCCS frontends.
;;;
;;; No key bindings to the functions are given, because this is a part
;;; of a larger package of mine.

;;; This software and documentation were written while I, Brian Marick,
;;; was an employee of Gould Computer Systems.  They have been placed in
;;; the public domain.  They were extensively rewritten at the University
;;; of Illinois.

;;; Chris Liebman's dired-rcs provided some of the code for the dired
;;; interface.

(require 'cl)		; We use CL functions. 
(provide 'config)

;;; Naming conventions (to avoid clashes with other packages).
;;; Public names begin with "config-".
;;; Public names used with dired begin with "dired-config-"
;;; Public names used with the Buffer menu begin with "buffmenu-config"
;;; Private names begin with "cu-" (short for config-util).
;;; Names for functions that deal with version control systems begin with
;;; the names of those systems:  "RCS-", "SCCS-", or "compress-".


(defconst *config-version* "$Header: /home/src/emacs/custom/lynx/lisp/RCS/config.el,v 1.3 90/05/17 09:57:19 marick Exp $ -- First released version")

;; $Log:	config.el,v $
;; Revision 1.3  90/05/17  09:57:19  marick
;; Andy's SCCS-tools-path.
;; 
;; Revision 1.2  90/03/13  08:30:15  marick
;; This version has Buffer Menu commands.
;; 
;; Revision 1.1  89/11/14  08:34:24  marick
;; First released version.
;; 

;===%%SF%% vars (Start)  ===

;;; Variables the user may want to set:

(defvar *config-type* 'SCCS
  "The type of configuration control you use.  Stock ones are
	RCS		use RCS to check in and out of RCS subdirectory.
	SCCS		Use SCCS to check in and out of SCCS subdirectory.
Others are easily added.
")

(defvar *config-verbose-commands* t
  "If T shell-commands are printed into display buffers before being
executed.")

(defvar *config-filename-filter-list*
        '(cu-standard-filter cu-query-filter)
	"Whole-directory checkins are pruned by passing lists of files to
successive filters in this list.  See CONFIG-DIR-IN.")

(defvar *config-dired-interface* t
  "If T, add these config commands to dired mode:
	l = display the log for the file. (config-log)
	D - show differences between the file and the archive. (config-diff)
	I - check in the file. (config-in)
	O - check out the file. (config-out)
	> - flag this file as ready to be checked in
	< - The same as '>'.
	A - flag all checked-out files.
	X - checkin all marked files."
)

(defvar *config-buffmenu-interface* t
  "If T, add these config commands to dired mode:
	l = display the log for the file. (config-log)
	D - show differences between the file and the archive. (config-diff)
	I - check in the file. (config-in)
	O - check out the file. (config-out)
	> - flag this file as ready to be checked in
	< - The same as '>'.
	A - flag all checked-out files.
	X - checkin all marked files."
)

;;; Hack: we choose a large number so as not to conflict with any user
;;; registers but still allow us to use registers to save previous
;;; comments.  This lets us merge the automatic saving of old comments
;;; with the mechanism that makes it easy for the user to retrieve old
;;; comments at any time.
(defvar *config-comment-register* 52525
  "If set to a character, the log comment message will be copied into the
register with that name.  You can use it when making up later comments.")

;;; A. Glew added this.
(defvar SCCS-tools-path ""
	"Directory where SCCS tools reside - empty if in $PATH, /usr/sccs/ on a SUN, etc.")


;;; Variables the user will probably leave alone:

(defvar *config-reuse* nil
  "If T and *config-comment-register* contains text, use that text
instead of asking for a comment.  If 'QUERY, use the text only if the
user gives permission.  If NIL, always ask for a comment.")

(defconst *config-diff-bufname* "diffs for config"
  "Differences displayed here.")
(defconst *config-log-bufname* "log for config"
  "File change logs displayed here.")
(defconst *config-comment-bufname* "comments for config"
  "Checkin comments gathered here.")
(defconst *config-results-bufname* "results for config"
  "Command results displayed here.")

(defconst *config-header-delim* "--text follows this line--")

(defconst *config-shell* "/bin/sh"
  "Config uses the Bourne shell for subcommands.  This is its name.")

;;; When we're trying to check in a list of files, we have to use a
;;; recursive edit, because we want to present one log buffer at a
;;; time.  Normally, though, we avoid recursive editing, handling
;;; things more like rmail mode and the like.
(defvar *config-recursing* nil)

;;; The number of seconds to sleep when printing transient message to
;;; minibuffer.
(defvar *config-sleep* 1)

;===%%SF%% vars (End)  ===

;===%%SF%% macros (Start)  ===
;;; These have to be here so that the compiler can see them.
;;; Actually, I suppose the Elisp compiler probably doesn't
;;; macroexpand at compile time.

(defmacro with-buffer (buffer &rest forms)
  "Execute the given FORMS in the given buffer, creating it if
necessary.  The current buffer is restored on exit.  There's no
change in window configuration -- set-buffer is used."
  (let ((saved-buf (gensym)))
    (` (let (( (, saved-buf) (current-buffer)))
        (cu-set-buffer-create (, buffer))
	(unwind-protect (progn (,@ forms))
	                (set-buffer (, saved-buf)))))))

;;; This is used to prevent default directory from getting changed in 
;;; whatever buffer a command is called from.  It should be used in
;;; functions that can cause recursive edits.
(defmacro with-function-home (buffer directory &rest forms)
  "Execute the body of the macro inside the given BUFFER, with the
given DIRECTORY as the default-directory of that buffer.  Everything
is restored on exit."
    (let ((saved-dir (gensym)))
      (` (with-buffer (, buffer)
	   (let* (( (, saved-dir) default-directory)
		  (default-directory (, directory)))
	     (unwind-protect (progn (,@ forms))
	       (setq default-directory (, saved-dir))))))))

;; So they indent reasonably.
(put 'with-buffer 'lisp-indent-hook 1)
(put 'with-function-home 'lisp-indent-hook 1)
    
;===%%SF%% macros (End)  ===



;===%%SF%% interfaces (Start)  ===
;;; These are the major user interfaces.

;;; Buffer pops up.  Cursor remains in old buffer.
;;; Current-buffer is unchanged.
(defun config-diff (file)
  "This function shows the differences between a FILE and the most
recent version in its archive file."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (cu-maybe-save-file file)
  (setq file (cu-expand-and-check file 'no-dir))
  (with-buffer *config-diff-bufname*
    (erase-buffer)
    (insert "Diff for " (file-name-nondirectory file) ?\n)
    (if (config-archive-p file)
	(config-shell-command (funcall (cu-get 'diff-command) file))
      (insert "   There is no archive for this file yet.")))
  (display-buffer *config-diff-bufname*))


(defun config-maybe-make-archive-dir (file)
  "This function checks whether an archive directory for FILE exists.
If not, it will, on request, try to make the directory.
Otherwise, it errors out."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir 'missing-ok))
  (funcall (cu-get 'assert-good-filename) file)
  (let ((dirname (funcall (cu-get 'archive-dir) file)))
    (cond ((file-directory-p dirname)
	   (if (interactive-p)
	       (message "Archive already exists."))
	   t)
	  ((y-or-n-p "No directory for archive.  Make it? ")
	   (shell-command (concat "mkdir " dirname) nil))
	  (t
	   (error "No directory for archive file.")))))
    
(defun config-archive-p (file)
  "This function returns T if an archive for FILE exists; NIL otherwise."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir 'missing-ok))
  (funcall (cu-get 'assert-good-filename) file)
  (let ((retval (file-exists-p (config-archive file))))
    (if (interactive-p)
	(if retval
	    (message "Archive exists.")
	  (message "Archive does not exist.")))
    retval))
      

(defun config-archive (file)
  "This function returns the name of the archive file corresponding to 
FILE.  There's no guarantee that the archive actually exists.
Use CONFIG-ARCHIVE-P to tell."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir 'missing-ok))
  (funcall (cu-get 'assert-good-filename) file)
  (let ((retval (funcall (cu-get 'archive-file) file)))
    (if (interactive-p)
	(message retval))
    retval))

(defun config-assert-compiled (source extension) 
  "This function checks whether SOURCE with its extension replaced by
EXTENSION is newer than SOURCE. If EXTENSION is not given, it
is taken to be \".o\"."
  (interactive "FSource File: \nsExtension: (.o) ")
  (cu-assert-system-available)
  (setq source (cu-expand-and-check source 'no-dir))
  (if (equal extension "")
      (setq extension ".o"))
  (let ((object (concat (cu-file-basename source) extension)))
    (if (or (not (file-exists-p object))
	    (file-newer-than-file-p source object))
	(error "You haven't compiled the file yet."))))

;; On error:  No change in window layout.
;; On success:  Pop to checked-out file, cursor in that buffer;
;; display results buffer.
(defun config-out (file)
  "Check out an editable version of FILE.
   A corresponding archive file must exist.  FILE must not already be
checked out.  If FILE is in an Emacs buffer, it must not be modified.
Most version control systems won't check out onto a writable file, but
that's left up to them.
   After the checkout, you'll be visiting the FILE, which will then be
writable."

  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir 'missing-ok))
  (funcall (cu-get 'assert-good-filename) file)
  (if (not (config-archive-p file))
      (error "No archive for this file.  Must check in before checking out."))
  (if (config-out-p file)
      (error "The file is already checked out for editing."))
  (let ((file-buffer (get-file-buffer file)))	; May be nil.
    (if (and file-buffer
	     (buffer-modified-p file-buffer))
	(error "The file's buffer is modified -- can't replace with checked out version.")
      (let* ((file-directory (file-name-directory file))
	     (command (funcall (cu-get 'checkout-command) file))
	     (newfile (funcall (cu-get 'checkout-newfile) file)))
	(cu-prepare-results-buffer file)
	(config-shell-command command)
	(funcall (cu-get 'pop-to-changed-file) newfile)
	(display-buffer *config-results-bufname*)
	(setq buffer-read-only nil)))))

(defun config-out-p (file)
  "This function returns T if the FILE is out for editing, NIL if it's
not and an error if it can't tell.  A FILE that doesn't have an archive
file is NOT out for editing, so CONFIG-OUT-P will return NIL.  To
distinguish between these two variants of NIL, use CONFIG-ARCHIVE-P."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file nil 'missing-ok))
  (funcall (cu-get 'assert-good-filename) file)
  (let ((retval (funcall (cu-get 'file-out-p) file)))
    (if (interactive-p)
	(if retval
	    (message "File is checked out for editing.")
	  (message "File is not checked out.")))
    retval))

;;; Buffer pops up.  Cursor remains in old buffer.
;;; Current-buffer is unchanged.
(defun config-show-out (directory)
  "This function shows you which files are checked out for editing in
DIRECTORY." 
  (interactive (list (file-name-as-directory 
		      (read-file-name "Directory: "
				      default-directory default-directory nil))))
  (cu-assert-system-available)
  (with-buffer *config-results-bufname*
    (let* ((filelist  (funcall (cu-get 'out-list) directory)))
      (cu-prepare-results-buffer (concat "Files out in directory " directory "\n"))
      (if filelist
	  (cu-filelist-in-buffer filelist *config-results-bufname*)
	(insert "<<< No files are checked out >>>")))
    (display-buffer *config-results-bufname*)))
  

;; On error:  No change in window layout.
;; On success:  Pop to reverted file; display results buffer.
;; Note:  if a comment is being gathered, it and its buffer are thrown
;; away -- that's often the point at which you decide to undo.
(defun config-undo-out (file)
  "This function undoes the effect of CONFIG-OUT:  The file is reverted
to the version in the archive file, and the file is made read-only.
  Any changes made to the file are lost.
  The function signals an error if the file isn't out for editing, or
if there is no archive file."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir))
  (funcall (cu-get 'assert-good-filename) file)
  (if (not (config-archive-p file))
      (error "No archive for this file, so it can't ever have been checked out."))
  (if (not (config-out-p file))
      (error "The file hasn't been checked out for editing."))

  ;; Any pending comment is now irrelevant.
  (if (get-buffer *config-comment-bufname*)
      (kill-buffer *config-comment-bufname*))

  (let* ((command (funcall (cu-get 'undo-out-command) file)))
    (cu-prepare-results-buffer file)
    (config-shell-command command)
    (funcall (cu-get 'pop-to-changed-file) file)
    (display-buffer *config-results-bufname*)
    (setq buffer-read-only t)))

  
;; Normally:  diff buffer and comment buffer pop up.  Cursor in
;; comment buffer.
;; Checkin in progress:  current comment buffer pops up.  
;;   If they want to continue that checkin, cursor to the comment buffer.
;;   Otherwise, just like ordinary checkin.
;; File not checked out:  No change in display.
;; Comment log to be reused?
;;   When question asked:  Diff buffer pops up.  Cursor stays behind.
;;   If no:  Like ordinary checkin.
;;   If yes: the diff buffer will go away.  The results buffer will be
;;   visible.
(defun config-in (file)
  "This function allows you to check in a FILE.
   If you're editing the file, it's saved before being checked in.
   This function displays two buffers.  One shows the differences between
the checked-out version of the file and the most recent version in the
archive. You should type comments (for the archive log) in the other
buffer.  When finished, type ^C^C to continue with the checkin.
   If you were visiting the file before checkin, you'll still be in it
after checkin, except that the file will be in synch with the archive
copy and it will be read-only.
   The archive need not exist to check in the file.  It will be created.
"
  (interactive "FFile: ")
  (cu-assert-system-available)
  (cu-maybe-save-file file)
  (setq file (cu-expand-and-check file 'no-dir))
  (funcall (cu-get 'assert-good-filename) file)

  ;; First do the error checking
  (if (and (config-archive-p file)
	   (not (config-out-p file)))
      (error "File isn't checked out for editing."))

  (config-maybe-make-archive-dir file)

  ;; We may want to abort an in-progress checkin.
  (when (or (not (config-checking-in-p))
	    (progn
	      (pop-to-buffer *config-comment-bufname*)
	      (y-or-n-p "Already checking in a file.  Forget that checkin? ")))

    ;; If there's no previous text, *config-reuse* might as well be NIL.
    (let ((*config-reuse*
	   (if (get-register *config-comment-register*)
	       *config-reuse*
	     nil)))
	     
      (when (not (eq *config-reuse* t))
	;; They have to think about what they've done -- show it.
	(config-diff file))

      (when (eq *config-reuse* 'QUERY)
	;; Promote or demote *config-reuse* according to what they decide.
	(view-register *config-comment-register*)
	(switch-to-buffer *config-diff-bufname*)
	(setq *config-reuse*
	      (y-or-n-p (format "Reuse previous comment in this file, %s? "
				(file-name-nondirectory file)))))

      ;; At this point, one of two things can happen.  If there's a
      ;; previous log message to reuse, we just hand the file and the
      ;; previous message to CU-DO-CHECKIN.  If not, we create the comment
      ;; buffer.  When the user types ^C^C there, the resulting function
      ;; (CONFIG-FINISH-CHECKIN) will call CU-DO-CHECKIN.
      (if *config-reuse*
	  (cu-do-checkin file (get-register *config-comment-register*))
	(cu-display-two-buffers *config-comment-bufname*
				*config-diff-bufname*)
	(cu-checkin-mode)
	;; We need to stash the file in the buffer so that we can 
	;; retrieve it when the user types C-c C-c.
	(make-local-variable 'checkin-file)
	(setq checkin-file file)
	(erase-buffer)
	(insert "Log comment for " file "\n")
	(insert *config-header-delim* "\n")
	(message "Type in the log comment; end with ^C^C.")
	(if *config-recursing*
	    (recursive-edit))))))


(defun config-kill-checkin ()
  "This command stops any in-progress checkins, undoing the effects of
CONFIG-IN or CONFIG-DIR-IN."
  (interactive)
  (cu-assert-system-available)
  (if (get-buffer *config-comment-bufname*)
      (kill-buffer *config-comment-bufname*))
  ;; Top level gets us out of recursive edits and clears *config-recursing*.
  (when *config-recursing*
    (cu-results-logit "***Checkin cancelled***")
    (throw 'list-checkin-tag nil)))


;; Buffer pops up.  Cursor remains in old buffer.
;;; Current-buffer is unchanged.
(defun config-log (file)
  "This file shows the log file for the given FILE."
  (interactive "FFile: ")
  (cu-assert-system-available)
  (setq file (cu-expand-and-check file 'no-dir 'missing-ok))
  (with-buffer *config-log-bufname*
    (erase-buffer)
    (insert "Log for " file ".\n")
    (if (config-archive-p file)
	(config-shell-command (funcall (cu-get 'log-command) file))
      (insert "There is no archive.\n")))
  (display-buffer *config-log-bufname*))


(defun config-version ()
  "Display the version number."
  (interactive)
  (message *config-version*))

;   ===%%SF%% interfaces/group (Start)  ===
(defun config-all-in (directory)
  "This routine calls CONFIG-IN on all the checked-out files in the
DIRECTORY. You'll be given the option of using the previous file's log
message on each new file."
  ;; The interactive 'D' option doesn't do quite the right thing -- RET
  ;; often gives you a file, not a directory.
  (interactive (list (file-name-as-directory 
		      (read-file-name "Directory: "
				      default-directory default-directory nil))))
  (cu-assert-system-available)

  (with-function-home *config-results-bufname* directory
     (when (not (cu-duplicate-checkin-p))
       (let ((filelist (funcall (cu-get 'out-list) directory))
	     (*config-reuse* 'QUERY))
	 (cu-prepare-results-buffer "Checking in checked-out files.")
	 (cu-list-in (cu-query-filter filelist))))))


(defun config-buffers-in ()
  "This routine calls CONFIG-IN on all the checked-out files currently being 
edited.  You'll be given the option of using the previous file's log
message on each new file."
  (interactive)

  (cu-assert-system-available)
  (with-function-home *config-results-bufname* default-directory
     (when (not (cu-duplicate-checkin-p))
       (let ((filelist (cu-outness-filter (config-buffmenu-filelist)))
	     (*config-reuse* 'QUERY))
	 (cu-prepare-results-buffer "Checking in checked-out buffers.")
	 (cu-list-in (cu-query-filter filelist))))))


;;; During intermediate checkins:
;;;   Same rules as config-in:  comment log in one window, diff in the other.
;;; At end:
;;;   The results buffer is the only buffer visible.
(defun config-dir-in (directory)
  "This routine calls CONFIG-IN on the files in the DIRECTORY
argument.  You'll be given the option of using the previous file's log
message on each new file.

These kinds of files are never checked in:
- Directories.
- Backup and auto-save files.
- C object files (ending in .o)
- Library files (ending in .a)
- Compressed files (ending in .Z or .z)
- Compiled elisp files (ending in .elc)
- Files with archive files that are not checked out for editing.

Exception:  If the file indeed has an archive file, and is checked
out for editing, it will be checked in regardless of its name.

If a file has no archive file, you'll be asked whether you want to
check the file in (thus creating an archive).

All of these rules can be overridden by modifying
*config-filename-filter-list*." 

  ;; The interactive 'D' option doesn't do quite the right thing -- RET
  ;; often gives you a file, not a directory.
  (interactive (list (file-name-as-directory 
		      (read-file-name "Directory: "
				      default-directory default-directory nil))))
  (cu-assert-system-available)

  ;; Save buffers in case there's an edited file that doesn't have an
  ;; on-disk image yet.
  (save-some-buffers)

  (with-function-home *config-results-bufname* directory
     (when (not (cu-duplicate-checkin-p))
       (cu-prepare-results-buffer "Checking in a list of files.")
       (let* ((filelist
	       (cu-apply-file-filters (directory-files directory 'full)))
	      (*config-reuse* *config-reuse*))
	 (if (and (> (length filelist) 1)
		  (yes-or-no-p
		   "Should the first comment be silently used for all files? "))
	     (setq *config-reuse* t)
	   (setq *config-reuse* 'QUERY))
	 (cu-list-in filelist)))))

;   ===%%SF%% interfaces/group (End)  ===

;   ===%%SF%% interfaces/dired (Start)  ===
;;;   DIRED interface.  Some code and ideas stolen from Chris Liebman.


;;; Push our setup function on the dired-mode hook. 
(cond ((not (boundp 'dired-mode-hook))
       (setq dired-mode-hook 'dired-config-setup))
      ((listp dired-mode-hook)
       (push 'dired-config-setup dired-mode-hook))
      (t
       (setq dired-mode-hook
	     (list 'dired-config-setup dired-mode-hook))))

;;; Note that you can set *config-dired-interface* at any time.
;;; Any direds after that will have dired-config key bindings.
(defun dired-config-setup ()
  "Setup config key bindings for dired."
  (when *config-dired-interface*
    (local-set-key "l" 'dired-config-log)
    (local-set-key "D" 'dired-config-diff)
    (local-set-key "I" 'dired-config-in)
    (local-set-key "O" 'dired-config-out)
    (local-set-key ">" 'dired-config-flag-file-for-checkin)
    (local-set-key "<" 'dired-config-flag-file-for-checkin)
    (local-set-key "A" 'dired-config-flag-all-out)
    (local-set-key "X" 'dired-config-checkin-flagged-files)))

;;;
;;; Dired commands.
;;;

(defun dired-config-out ()
  "In dired, call CONFIG-OUT to check out a file."
  (interactive)
  (cu-assert-system-available)
  (let* ((filename (dired-get-filename)))
    (config-out filename)))

(defun dired-config-in ()
  "In dired, call CONFIG-IN to check in a file."
  (interactive)
  (cu-assert-system-available)
  (let ((filename (dired-get-filename)))
    (config-in filename)))

(defun dired-config-log ()
  "In dired, call CONFIG-LOG to show a file's archive log."
  (interactive)
  (cu-assert-system-available)
  (config-log (dired-get-filename)))

(defun dired-config-diff ()
  "In dired, diff the working file against the latest checked in one."
  (interactive)
  (cu-assert-system-available)
  (config-diff (dired-get-filename)))

(defun dired-config-flag-file-for-checkin (arg)
  "In dired, mark a file for checkin.  'X' performs the checkin.
With arg, repeat over ARG lines"
  (interactive "p")
  (cu-assert-system-available)
  (let ((buffer-read-only nil))
    (dired-repeat-over-lines arg
      '(lambda ()
	 (let* ((file (dired-get-filename))
		(is-archive-file (config-archive-p file)))
	   (if (and is-archive-file
		    (not (config-out-p file)))
	       (error "File is not checked out for editing")
	     (if (not is-archive-file)
		 (message "Note: file has no archive."))
	     (let ((buffer-read-only nil))
	       (delete-char 1)
	       (insert "<"))))))))


;; Note:  calling dired-config-flag-file-for-checkin is gross wastage.
;; It does much unnecessary checking.
;; Someday clean this up to be a simple traversal of the dired buffer.
(defun dired-config-flag-all-out ()
  "In dired, check in all the checked-out file in the directory."
  (interactive)
  (cu-assert-system-available)
  (let ((filelist (funcall (cu-get 'out-list) default-directory)))
    (dolist (file (mapcar 'file-name-nondirectory filelist))
      (beginning-of-buffer)	; Love them O(n^2) algorithms
      (if (re-search-forward (concat " " file "$") nil t)
	  (dired-config-flag-file-for-checkin 1)
	(error "Checked out file not in dired list -- dired buffer out of date?")))
    (if (null filelist)
	(message "No files checked out."))))
    

;; Note:  we delete the flags only after the file list passes the
;; query filter.  That's so the user can reject the list of files and
;; go edit it (by flagging/unflagging more files).
(defun dired-config-checkin-flagged-files ()
  "In dired, checkin all the files that have been flagged for checkin."
  (interactive)
  (cu-assert-system-available)
  (let ((file-list nil)
	(buffer-read-only nil)
	(*config-reuse* 'QUERY))
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "^<" nil t)
	(push (dired-get-filename) file-list)))
    (setq file-list (nreverse file-list))
    (with-function-home *config-results-bufname* default-directory
      (when (not (cu-duplicate-checkin-p))
	(cu-prepare-results-buffer "Checking in from DIRED")
	(setq file-list (cu-query-filter file-list))
	(when file-list
	  (save-excursion
	    (goto-char 1)
	    (while (re-search-forward "^<" nil t)
	      (delete-char -1)
	      (insert " ")))
	  (cu-list-in file-list))))))

;   ===%%SF%% interfaces/dired (End)  ===


;   ===%%SF%% interfaces/buffmenu (Start)  ===
;;;   Buffer menu interface


;      ===%%SF%% interfaces/buffmenu/initialization (Start)  ===

;;; Push our setup function on the buffer-menu-mode hook. 
(cond ((not (boundp 'buffer-menu-mode-hook))
       (setq buffer-menu-mode-hook 'buffmenu-config-setup))
      ((listp buffer-menu-mode-hook)
       (push 'buffmenu-config-setup buffer-menu-mode-hook))
      (t
       (setq buffer-menu-mode-hook
	     (list 'buffmenu-config-setup buffer-menu-mode-hook))))

;;; Note that you can set *config-buffmenu-interface* at any time.
;;; Any buffmenus after that will have buffmenu-config key bindings.
(defun buffmenu-config-setup ()
  "Setup config key bindings for buffmenu."
  (when *config-buffmenu-interface*
    (local-set-key "l" 'buffmenu-config-log)
    (local-set-key "D" 'buffmenu-config-diff)
    (local-set-key "I" 'buffmenu-config-in)
    (local-set-key "O" 'buffmenu-config-out)
    (local-set-key ">" 'buffmenu-config-flag-file-for-checkin)
    (local-set-key "<" 'buffmenu-config-flag-file-for-checkin)
    (local-set-key "A" 'buffmenu-config-flag-all-out)
    (local-set-key "X" 'buffmenu-config-checkin-flagged-files)))

;      ===%%SF%% interfaces/buffmenu/initialization (End)  ===

;      ===%%SF%% interfaces/buffmenu/util (Start)  ===

(defun config-buffmenu-on-header-p ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at " MR Buffer")
	(looking-at " -- ------"))))

(defun config-buffmenu-file-name (verbose)
  "Return the filename of the current line, NIL if buffer has no file."
  (save-excursion
    (if (config-buffmenu-on-header-p)
	(error "No buffer on this line."))
    (let ((buffer (Buffer-menu-buffer nil))
	  file-name)
      (cond ((null buffer)
	     (if verbose (message "Buffer has been deleted."))
	     nil)
	    ((setq file-name (buffer-file-name buffer))
	     file-name)
	    (t
	     (if verbose (message "No file for this buffer."))
	     nil)))))

;;; Return a list of filenames corresponding to active buffers.
;;; The list of buffers is available only from the Buffer menu buffer.
;;; We don't change the buffer menu out from under the user, even
;;; though the menu may be out of date.  If we have to create the
;;; buffer menu, we destroy it later.
(defun config-buffmenu-filelist ()
  (let ((created-menu-p (not (get-buffer "*Buffer List*")))
	(current-buf (current-buffer)))
    (when created-menu-p
	(list-buffers 'files-only))
    (set-buffer "*Buffer List*")
    (save-excursion
      (beginning-of-buffer)
      (next-line 2)
      (let ((filelist nil)
	    (result nil))
	(while (not (eobp))
	  (setq result (config-buffmenu-file-name nil))
	  (if result (push result filelist))
	  (forward-line 1))
	(if created-menu-p
	    (kill-buffer "*Buffer List*"))
	(set-buffer current-buf)
	(nreverse filelist)))))


;; Like dired, except ARG must be positive.
(defun buffmenu-config-repeat-over-lines (arg function)
  (beginning-of-line)
  (if (config-buffmenu-on-header-p)
      (error "No file on this line"))
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (save-excursion
      (beginning-of-line)
      (funcall function))
    (forward-line 1)))


;      ===%%SF%% interfaces/buffmenu/util (End)  ===


(defun buffmenu-config-out ()
  "In the Buffer Menu, call CONFIG-OUT to check out a file."
  (interactive)
  (cu-assert-system-available)
  (let* ((filename (config-buffmenu-file-name 'verbose)))
    (if filename (config-out filename))))

(defun buffmenu-config-in ()
  "In the Buffer Menu, call CONFIG-IN to check in a file."
  (interactive)
  (cu-assert-system-available)
  (let ((filename (config-buffmenu-file-name 'verbose)))
    (if filename (config-in filename))))

(defun buffmenu-config-log ()
  "In the Buffer Menu, call CONFIG-LOG to show a file's archive log."
  (interactive)
  (cu-assert-system-available)
  (let ((filename (config-buffmenu-file-name 'verbose)))
    (if filename (config-log filename))))

(defun buffmenu-config-diff ()
  "In the Buffer Menu, diff the working file against the latest checked in one."
  (interactive)
  (cu-assert-system-available)
  (let ((filename (config-buffmenu-file-name 'verbose)))
    (if filename (config-diff filename))))

;;; Note that the position of point moves forward ARG lines.
(defun buffmenu-config-flag-file-for-checkin (arg)
  "In the Buffer Menu, mark a file for checkin.  'X' performs the checkin.
With arg, repeat over ARG lines."
  (interactive "p")
  (cu-assert-system-available)
  (let ((buffer-read-only nil)
	(be-verbose (= arg 1))) ;; Warnings confusing if arg is, say, 4.
    (buffmenu-config-repeat-over-lines arg
      '(lambda ()
	 (let ((file (config-buffmenu-file-name be-verbose)))
	   (if file
	      (let ((is-archive-file (config-archive-p file)))
		(if (and is-archive-file
			 (not (config-out-p file)))
		    (error "File is not checked out for editing")
		  (if (not is-archive-file)
		      (message "Note: file has no archive."))
		  (let ((buffer-read-only nil))
		    (delete-char 1)
		    (insert "I"))))))))))


;; Note:  calling buffmenu-config-flag-file-for-checkin is gross wastage.
;; It does much unnecessary checking.  
(defun buffmenu-config-flag-all-out ()
  "In the Buffer Menu, check in all the checked-out buffers."
  (interactive)
  (cu-assert-system-available)
  (save-excursion
    (beginning-of-buffer)
    (next-line 2)
    (let ((file))
      (while (not (eobp))
	(setq file  (config-buffmenu-file-name nil))
	(if (and file (config-out-p file))
	    ;; Kludge -- this does a forward-line.
	    (buffmenu-config-flag-file-for-checkin 1)
	  (forward-line 1)))))
  (message "Done"))
    

;; Note:  we delete the flags only after the file list passes the
;; query filter.  That's so the user can reject the list of files and
;; go edit it (by flagging/unflagging more files).
;;
;; Note:  If they flag files, then delete them, then execute the flags,
;; they lose.  Not worth checking for.
(defun buffmenu-config-checkin-flagged-files ()
  "In the Buffer Menu, check in all the files that have been flagged for checkin."
  (interactive)
  (cu-assert-system-available)
  (let ((file-list nil)
	(buffer-read-only nil)
	(*config-reuse* 'QUERY))
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "^I" nil t)
	(push (config-buffmenu-file-name nil) file-list)))
    (setq file-list (nreverse file-list))
    (with-function-home *config-results-bufname* default-directory
      (when (not (cu-duplicate-checkin-p))
	(cu-prepare-results-buffer "Checking in from Buffer Menu")
	(setq file-list (cu-query-filter file-list))
	(when file-list
	  (save-excursion
	    (goto-char 1)
	    (while (re-search-forward "^I" nil t)
	      (delete-char -1)
	      (insert " ")))
	  (cu-list-in file-list))))))

;   ===%%SF%% interfaces/buffmenu (End)  ===
  
;===%%SF%% interfaces (End)  ===


;===%%SF%% generics (Start)  ===

;;; A variety of generic commands are specialized according to the
;;; *config-type*.  They are:
;;;
;;; checkout-command:  Return a command that checks out an editable version
;;;	of a file.
;;; checkout-newfile:  The name of the file after checked out.  (For
;;; 	RCS and SCCS, it's the same -- but that might not be the case
;;;	for protocols where the file is checked out into a "work"
;;;     directory.)
;;; undo-out-command:  Undo the checkout-command.
;;; checkin-command:  Return a command that checks in a version of a file.
;;;     It takes the file and a comment for the archive log as arguments.
;;; pop-to-changed-file:  Visit the just-checked-out/in file.  Make
;;; 	sure it's in synch with the disk version.  
;;; massage-comment:  Massage the comment so it's suitable for handing to a 
;;;     checkin command.
;;; archive-dir:  Given a filename, return the directory where its
;;; 	archive would be.
;;; archive-file:  Given a filename, return the full pathname for its
;;; 	archive. 
;;; file-out-p:  Returns the filename if the file is checked out for
;;; 	editing; NIL otherwise.  Should return NIL if there's no archive
;;;	file.
;;; out-list:  Returns a list of which files are
;;;     checked out for editing.  Files should be full pathnames.
;;; log-command:  Return a command that can be used to print the
;;;     log for a given file.
;;; diff-command:  Return a command that diffs the FILE against the
;;; 	top version in the archive.
;;; assert-good-filename:  Error out if the filename is bad for some
;;;	reason (typically if the name is too long).
;;; sample-executable:  An executable in this version-control system.
;;; 	Used to see if the system actually exists on this machine.
;;; 	If it doesn't, you want to abort immediately, not after the user's
;;; 	typed a huge long log comment

;   ===%%SF%% generics/interfaces (Start)  ===
;;; Each of the config-types has various functions attached to its property
;;; list.  These are manipulated by these functions:

(defun cu-add (config-type generic-name specific-name)
  (put config-type generic-name specific-name))

(defun cu-get (function-name)
  (let ((result (get *config-type* function-name)))
    (if (not result)
	(error "Program error -- cu-get: No function %s.  Is *config-type* correct?"
	       (symbol-name function-name)))
    result))
;   ===%%SF%% generics/interfaces (End)  ===

;   ===%%SF%% generics/util (Start)  ===
;;; These are used with several different CM programs.

(defun cu-checkout-newfile (file)
  file)

;; Don't freshly visit a new file if we're recursing -- this is just
;; one of many being processed.  DO revert the file, since we want
;; them to be in synch.

(defun cu-pop-to-changed-file (file)
  (let ((file-buffer (get-file-buffer file)))
    (cond (file-buffer
	   (pop-to-buffer file-buffer)
	   (revert-buffer 'no-auto-save 'no-confirm))
	  ((not *config-recursing*)
	   (find-file-other-window file)))))

;; Escape quotes and slashes.
;; The different shells treat newlines in quoted strings differently.
;; They also treat slashes within quoted strings differently.
;; Rather than anticipating all possible shells, we'll assume /bin/sh.
(defun cu-massage-comment ()
  (beginning-of-buffer) (replace-regexp "\\\\" "\\\\\\\\")
  (beginning-of-buffer) (replace-regexp "\"" "\\\\\""))

;;; Any filename is good on BSD.  (We don't check for pathname limits.)
;;; On system V, a file must have 2 characters free for an archive tag
;;; (the "s." prefix for SCCS, or the ",v" suffix for RCS).
(defun cu-assert-good-filename (file)
  (let ((basename (file-name-nondirectory file)))
    (if (and (eq system-type 'usg-unix-v)
	     (>= (length basename) 13))
	(cond ((eq *config-type* 'SCCS)
	       (error "%s is too long for the archive prefix 's.'"
		      basename))
	      ((eq *config-type* 'RCS)
	       (error "%s is too long for the archive suffix ',v'"
		      basename))
	      (t
	       (error "Program error: cu-assert-good-filename called with bad *config-type*"))))))

;   ===%%SF%% generics/util (End)  ===


;   ===%%SF%% generics/SCCS (Start)  ===

(defun SCCS-checkout-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file))))
    (concat "cd " file-directory "; " SCCS-tools-path "get -e SCCS/s."
	    (file-name-nondirectory file))))

(cu-add 'SCCS 'checkout-command 'SCCS-checkout-command)


;; Note -- use a delta followed by an explicit get, instead of delta -n,
;; because delta -n leaves the file editable.
(defun SCCS-checkin-command (file comment)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(name (file-name-nondirectory file)))
    (if (config-archive-p file)
	(concat "cd " file-directory "; "
		SCCS-tools-path "delta -y\"" comment "\" SCCS/s." name "; "
		SCCS-tools-path "get SCCS/s." name)
      (concat "cd " file-directory "; "
	      SCCS-tools-path "admin -i" file " -y\"" comment "\" SCCS/s." name "; "
	      "/bin/rm " name " ; "
	      SCCS-tools-path "get SCCS/s." name))))

(cu-add 'SCCS 'checkin-command 'SCCS-checkin-command)


(defun SCCS-diff-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(file-only (file-name-nondirectory file)))
    (concat "cd " file-directory ";"
            SCCS-tools-path "get -p SCCS/s." file-only " > /tmp/ci$$;"
	    "diff " file-only " /tmp/ci$$;"
	    "/bin/rm /tmp/ci$$")))

(cu-add 'SCCS 'diff-command 'SCCS-diff-command)


(defun SCCS-log-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(file-only (file-name-nondirectory file)))
    (concat "cd " file-directory ";"
	    SCCS-tools-path "prs SCCS/s." file-only)))
(cu-add 'SCCS 'log-command 'SCCS-log-command)
  

(defun SCCS-archive-dir (file)
  (concat (file-name-directory file) "SCCS"))
(cu-add 'SCCS 'archive-dir 'SCCS-archive-dir)

(defun SCCS-archive-file (file)
  (concat (file-name-as-directory (SCCS-archive-dir file))
	  "s."
	  (file-name-nondirectory file)))
(cu-add 'SCCS 'archive-file 'SCCS-archive-file)

(defun SCCS-file-out-p (file)
  (file-exists-p
     (concat (file-name-as-directory (SCCS-archive-dir file))
	  "p."
	  (file-name-nondirectory file))))
(cu-add 'SCCS 'file-out-p 'SCCS-file-out-p)

(defun SCCS-out-list (dir)
  (let* ((dir (file-name-as-directory dir))
	 (command (concat "cd " dir "SCCS\nls p.*"))
	 (list nil))
    (with-buffer " *config show out buffer*"
      (erase-buffer)
      (let ((shell-file-name *config-shell*))
	(shell-command command t))
      (beginning-of-buffer)
      (while (re-search-forward "^p\\.\\(.*\\)$" nil t)
	(push (cu-make-full-path dir
				 (buffer-substring (match-beginning 1)
						   (match-end 1)))
	      list))
      (cond ((null list)
	     nil)
	    ((not (file-exists-p (car list)))
	     nil)				; Must be an error message
	    (t
	     (nreverse list))))))
(cu-add 'SCCS 'out-list 'SCCS-out-list)

(defun SCCS-undo-out-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(file-only (file-name-nondirectory file)))
    (concat "cd " file-directory ";"
	    "/bin/rm -f SCCS/p." file-only ";"
	    "/bin/rm -f " file-only ";"
	    SCCS-tools-path "get SCCS/s." file-only)))
(cu-add 'SCCS 'undo-out-command 'SCCS-undo-out-command)

(cu-add 'SCCS 'checkout-newfile 'cu-checkout-newfile)
(cu-add 'SCCS 'pop-to-changed-file 'cu-pop-to-changed-file)
(cu-add 'SCCS 'massage-comment 'cu-massage-comment)
(cu-add 'SCCS 'assert-good-filename 'cu-assert-good-filename)

(cu-add 'SCCS 'sample-executable (concat SCCS-tools-path "prs"))

;   ===%%SF%% generics/SCCS (End)  ===

;   ===%%SF%% generics/RCS (Start)  ===

(defun RCS-checkout-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file))))
    (concat "cd " file-directory "; co -l "
	    (file-name-nondirectory file))))
(cu-add 'RCS 'checkout-command 'RCS-checkout-command)

(defun RCS-checkin-command (file comment)
  (let ((file-directory (directory-file-name (file-name-directory file))))
    (concat "cd " file-directory "; ci -u -f -m\"" comment "\" "
	    (file-name-nondirectory file))))
(cu-add 'RCS 'checkin-command 'RCS-checkin-command)

(defun RCS-log-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file))))
    (concat "cd " file-directory "; rlog "
	    (file-name-nondirectory file))))
(cu-add 'RCS 'log-command 'RCS-log-command)

(defun RCS-archive-dir (file)
  (concat (file-name-directory file) "RCS"))
(cu-add 'RCS 'archive-dir 'RCS-archive-dir)

(defun RCS-archive-file (file)
  (concat (file-name-as-directory (RCS-archive-dir file))
	  (file-name-nondirectory file)
	  ",v"))
(cu-add 'RCS 'archive-file 'RCS-archive-file)

(defun RCS-file-out-p (file)
  (save-window-excursion
    (let ((file-directory (directory-file-name (file-name-directory file)))
	  (file-only (file-name-nondirectory file))
	  (buffer "outness buffer for config"))
      (pop-to-buffer buffer)
      (erase-buffer)
      (insert file ?\n)
      (shell-command (concat "cd " file-directory " ;rlog -L -R " file-only) t)
      (cond ((= (length (buffer-string)) 0)
	     (kill-buffer buffer)
	     nil)
	    ((progn (beginning-of-buffer)
		    (search-forward "rlog error"
				    (point-max) t))
	     (kill-buffer buffer)
	     nil)
	    ((progn (beginning-of-buffer)
		    (search-forward (concat "RCS/" file-only ",v")
				    (point-max) t))
	     (kill-buffer buffer)
	     t)
	    (t
	     ;; Probably archive file doesn't exist.  Return nil, but
	     ;; leave the buffer lying around.
	     nil)))))


(cu-add 'RCS 'file-out-p 'RCS-file-out-p)

(defun RCS-out-list (dir)
  (let* ((dir (file-name-as-directory dir))
	 (command (concat "cd " dir "RCS\nrlog -L -R *,v"))
	 (list nil))
    (with-buffer " *config show out buffer*"
      (erase-buffer)
      (let ((shell-file-name *config-shell*))
	(shell-command command t))
      (beginning-of-buffer)
      (while (re-search-forward "^\\(.*\\),v$" nil t)
	(push (cu-make-full-path dir
				 (buffer-substring (match-beginning 1) 
						   (match-end 1)))
	      list))
      (cond ((null list)
	     nil)
	    ((not (file-exists-p (car list)))
	     nil)				; Must be an error message
	    (t
	     (nreverse list))))))

(cu-add 'RCS 'out-list 'RCS-out-list)

(defun RCS-undo-out-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(file-only (file-name-nondirectory file)))
    (concat "cd " file-directory ";"
	    "/bin/rm -f " file-only ";"
	    "rcs -u " file-only ";"
	    "co " file-only)))
(cu-add 'RCS 'undo-out-command 'RCS-undo-out-command)

(defun RCS-diff-command (file)
  (let ((file-directory (directory-file-name (file-name-directory file)))
	(file-only (file-name-nondirectory file)))
    (concat "cd " file-directory ";"
            "rcsdiff " file-only)))
(cu-add 'RCS 'diff-command 'RCS-diff-command)

(cu-add 'RCS 'checkout-newfile 'cu-checkout-newfile)
(cu-add 'RCS 'pop-to-changed-file 'cu-pop-to-changed-file)
(cu-add 'RCS 'massage-comment 'cu-massage-comment)
(cu-add 'RCS 'assert-good-filename 'cu-assert-good-filename)

(cu-add 'RCS 'sample-executable "rcs")

;   ===%%SF%% generics/RCS (End)  ===

;===%%SF%% generics (End)  ===


;===%%SF%% util (Start)  ===

;   ===%%SF%% util/checkin (Start)  ===
(defvar cu-checkin-mode-map nil)
(if cu-checkin-mode-map
    nil
  (setq cu-checkin-mode-map (make-sparse-keymap))
  (define-key cu-checkin-mode-map "\C-c\C-c" 'config-finish-checkin))

(defun cu-checkin-mode ()
  "Like text mode, except for one command:

C-c C-c  config-finish-checkin
"
  (kill-all-local-variables)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map cu-checkin-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'cu-checkin-mode)
  (setq mode-name "Checkin")
  (run-hooks 'text-mode-hook 'cu-checkin-mode-hook))


;;; This does the actual work of the checkin.
;;; After finished, the results buffer is in one window.  The cursor
;;; is not in that window.  The file (if it was being edited) is in
;;; the other window.
;;; 
;;; It's kludgy that you have to do the massaging within a buffer, but
;;; there's so much more that works in a buffer -- regexp-replaces,
;;; for example.
(defun cu-do-checkin (file comment)
  (cu-prepare-results-buffer file)
  (let ((massage-buffer (get-buffer-create " Massage buffer")))
    (with-buffer massage-buffer
       (erase-buffer)
       (insert comment)
       (funcall (cu-get 'massage-comment))
       (setq comment (buffer-string)))
    (kill-buffer massage-buffer))
  (config-shell-command (funcall (cu-get 'checkin-command) file comment))
  (if (get-file-buffer file)
      (progn
	(funcall (cu-get 'pop-to-changed-file) file)
	(setq buffer-read-only t)))
  (display-buffer *config-results-bufname*))

;; This is called when the user types ^C^C in checkin-mode.
;; Comment buffer goes away.  The results buffer is displayed.
;; The cursor is not in that buffer.  If the file was in a buffer, that buffer
;; must be visible in one window, with the cursor in it.  
(defun config-finish-checkin ()
  (interactive)
  (cu-assert-system-available)
  (let ((file checkin-file))	; restore stashed filename.
    (beginning-of-buffer)
    (search-forward *config-header-delim*)
    (next-line 1)
    (beginning-of-line)
    (kill-region (point-min) (point))
    (let ((comment (buffer-string)))
      (set-register *config-comment-register* comment)
      (cu-do-checkin file (buffer-string)))
    (kill-buffer *config-comment-bufname*)
    (if *config-recursing*
	(exit-recursive-edit))))



  
;;; You're checking in if the config comment buffer is modified.
;;; It's okay to create the buffer; we assume it's about to be
;;; used anyway.
(defun config-checking-in-p ()
  (buffer-modified-p (get-buffer-create *config-comment-bufname*)))
  
;   ===%%SF%% util/checkin (End)  ===

;   ===%%SF%% util/group-checkin (Start)  ===

(defun cu-list-in (list) 
  "This routine is passed a list of files to check in.  A 'Do you want
to reuse the last log message' question is asked for every checkin but
the first.  Results of checkins will go into a cumulative results
buffer, which will be displayed at the end of the routine."
  
  (if (null list)			; Filters may have emptied list.
      (message "No files to check in.")
    (set-register *config-comment-register* nil)
    (let ((*config-recursing* t))
      (catch 'list-checkin-tag		; See CONFIG-KILL-CHECKIN
	(dolist (file list)
	  (if (and (config-archive-p file)
		   (not (config-out-p file)))
	      (cu-results-message "==> %s is not checked out for editing" file)
	    (save-excursion (config-in file)))
	  ;; Bury the buffer -- else the display can be confusing.
	  (bury-buffer *config-results-bufname*)))
      (switch-to-buffer *config-results-bufname*)
      (delete-other-windows)
      (message "List checkin done."))))


;; Duplicate checkin handling:
;; There are two potential problems -- the config comment buffer is
;; up for editing or another recursive checkin is in progress (in
;; which case, the first is probably also true).  In both cases, you
;; probably want to know right away (not when you finally get to
;; checking in a file), so the tests should be made here instead of
;; waiting for a test in config-in to catch things.
;;
;; Once tested, how to handle?  Could ask y/n questions and kill
;; buffers and abort recursive edits.  I tried it.  It's too
;; complicated.  We just tell her the problem and let her fix it --
;; she's much more likely to do the right thing.
(defun cu-duplicate-checkin-p ()
  (if (or (config-checking-in-p)
	  *config-recursing*)
      (let ((buf " Config Temp "))
	(pop-to-buffer buf)
	(momentary-string-display
	 "You are in the middle of a checkin.  You must finish it or
abort it before starting this group checkin.  You can kill the checkin
with CONFIG-KILL-CHECKIN."
	 (point))
	(kill-buffer buf)
	t)
    nil))

;      ===%%SF%% util/group-checkin/filters (Start)  ===
;;;
;;; General note on filters:
;;;   It's often a good idea to put a note about what files you
;;;   filtered out in the results buffer, which tends to pop up at
;;;   informative times. 

(defun cu-apply-file-filters (filelist)
  "Apply all the filters in *CONFIG-FILENAME-FILTER-LIST* to the
FILELIST and return the result."
  (dolist (filter *config-filename-filter-list*)
    (setq filelist (funcall filter filelist)))
  filelist)
	     


(defun cu-standard-filter (inlist)
   "This routine returns a filtered version of INLIST.  The following
files are always removed from the list:
- Directories.
- Backup and auto-save files.
- C object files (ending in .o)
- Library files (ending in .a)
- Compressed files (ending in .Z or .z)
- Compiled elisp files (ending in .elc)
- Files with archive files that are not checked out for editing.

Exception:  If the file indeed has an archive file, and is checked
out for editing, it will be checked in regardless of its name.

If the file has an archive file, and is checked out for editing, it
will be checked in regardless of name.

This function is typically the value of *config-filename-filter-list*"
   (let ((outlist nil))
     (dolist (file inlist)
       (let ((file-only (file-name-nondirectory file))
	     (file-out-p (config-out-p file)))	; Save redundant
					        ; expensive check. 
	 (cond (file-out-p
		(push file outlist))
	       ((file-directory-p file)
		(cu-results-message "Not checking in %s -- it's a directory." file-only))
	       ((backup-file-name-p file)
		(cu-results-message "Not checking in %s -- it's a backup file." file-only))
	       ((auto-save-file-name-p file-only)
		(cu-results-message "Not checking in %s -- it's a checkpoint file." file-only))
	       ((cu-c-object-file-name-p file)
		(cu-results-message "Not checking in %s -- it's an object file." file-only))
	       ((cu-elisp-object-file-name-p file)
		(cu-results-message "Not checking in %s -- it's an object file." file-only))
	       ((cu-compressed-file-name-p file)
		(cu-results-message "Not checking in %s -- it's a compressed file." file-only))
	       ((cu-library-file-name-p file)
		(cu-results-message "Not checking in %s -- it's a library file." file-only))
	       ((and (config-archive-p file)
		     (not file-out-p))
		(cu-results-message "Not checking in %s -- it's not checked out." file-only))
	       (t
		 (push file outlist)))))
     (nreverse outlist)))

(defun cu-c-object-file-name-p (file)
  "Return non-nil if FILE is an object file name (*.o)"
  (string-match "\\.o$" file))

(defun cu-elisp-object-file-name-p (file)
  "Return non-nil if FILE is an object file name (*.elc)"
  (string-match "\\.elc$" file))

(defun cu-library-file-name-p (file)
  "Return non-nil if FILE is a library file name (*.a)"
  (string-match "\\.a$" file))

(defun cu-compressed-file-name-p (file)
  "Return non-nil if FILE is the name of a compressed file (*.[Zz])"
  (string-match "\\.[Zz]$" file))


(defun cu-query-filter (filelist)
  "This filter asks the user whether to check in a list of files."
  (when filelist
    (let ((buffer (get-buffer-create " *Checkin Query*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert "Files to be checked in:\n")
      (insert "-----------------------\n")
      (cu-filelist-in-buffer filelist buffer)
      ;; Kludge -- we assume something interesting to see may also be in
      ;; the results buffer.
      (cu-display-two-buffers *config-results-bufname* buffer)
      (prog1
	  (if (yes-or-no-p "Check in these files? ")
	      filelist
	    nil)
	(kill-buffer buffer)))))

	     

(defun cu-outness-filter (inlist)
   "This routine returns a filtered version of INLIST.  Files that
are not checked out are removed from the list."
   (let ((outlist nil))
     (dolist (file inlist)
       (if (config-out-p file)
	   (push file outlist)))
     (nreverse outlist)))

;      ===%%SF%% util/group-checkin/filters (End)  ===

;   ===%%SF%% util/group-checkin (End)  ===

;   ===%%SF%% util/results (Start)  ===  Results buffer manipulation

;;; Get the config-results buffer ready for a command.
;;; Normally, the buffer is cleared.  When config is recursing (for a
;;; group checkin, for example), it's not.
(defun cu-prepare-results-buffer (header)
  (cu-set-buffer-create *config-results-bufname*)
  (if (not *config-recursing*)
      (erase-buffer))
  (end-of-buffer)
  (insert header ?\n))

(defun cu-results-message (&rest args)
  "Print the message to both the minibuffer and the results buffer.
Pause to give the user a chance to read message."
  (apply 'message args)
  (sleep-for *config-sleep*)
  (apply 'cu-results-logit args))

(defun cu-results-logit (&rest args)
  "Print the message to the end of the results buffer, suffixing with newline."
  (save-window-excursion
    (switch-to-buffer *config-results-bufname*)
    (end-of-buffer)
    (insert (apply 'format args) "\n")))

;   ===%%SF%% util/results (End)  ===

;   ===%%SF%% util/systems (Start)  === ; Do they exist?

;;; Make a pathname from a directory and a filename.  UNIX-specific.
(defun cu-make-full-path (directory filename)
  (if (null directory)
      filename
    (concat (file-name-as-directory directory) filename)))

;;; Return the pathname if a file exists in the path; otherwise,
;;; return NIL.
(defun cu-file-exists-in-path-p (filename path)
    (cond ((null path)
	   nil)
	  ((file-exists-p (cu-make-full-path (car path) filename)))
	  ((cu-file-exists-in-path-p filename (cdr path)))))

(defvar cu-saved-config-type nil
  "Stashed value of *config-type*  -- avoids repeated checking.")

(defvar cu-system-checks-out nil
  "System has been checked -- it was available.")

(defun cu-assert-system-available ()
  "Call this routine at the beginning of every interactive function.
It checks whether the *config-type* system is in the EXEC-PATH.  This
minimizes the chance of botched operations."
  ;; The check is made only if the config-type has changed or it's
  ;; never yet been made. 
  (when (or (not (eq cu-saved-config-type *config-type*))
	    (not cu-system-checks-out))
    (setq cu-saved-config-type *config-type*)
    (setq cu-system-checks-out nil)
    (let ((executable (cu-get 'sample-executable)))
	  (if (cu-file-exists-in-path-p executable exec-path)
	      (setq cu-system-checks-out t)
	    (error "No executable %s -- does %s exist on this system?"
		   executable (symbol-name *config-type*))))))
					    
;   ===%%SF%% util/systems (End)  ===	

;   ===%%SF%% util/misc (Start)  ===

;;; Strips .<foo> from name.  (Note -- (cu-file-basename "foo.bar.baz") ==> "foo")
;;; Note:  simplified by fact that (substring "foo" 0 NIL) ==> "foo").
(defun cu-file-basename (name)
  (substring name 0 (string-match "\\." name)))


;;; Execute command, results in current buffer.  If
;;; *config-verbose-commands*, also put the command in the buffer.
(defun config-shell-command (command)
  (end-of-buffer)
  (if *config-verbose-commands*
      (insert "Command is:\n" command "\nResults are:\n"))
  (let ((shell-file-name *config-shell*))
    (shell-command command t)))


(defun cu-expand-and-check (file &optional no-dir missing-ok)
  "This function expands and substitutes the filename.  It also
signals an error if the file doesn't really exist (unless MISSING-OK
is non-NIL).  If the optional NO-DIR argument is non-nil, it will
complain if the FILE is really a directory."
  (if (and (not (file-exists-p file))
	   (not missing-ok))
      (error "File %s doesn't exist." file))
  (if (and no-dir
	   (file-directory-p file))
      (error "%s is a directory." file))
  (expand-file-name (substitute-in-file-name file)))

;;; If editing the file, save it.
(defun cu-maybe-save-file (file)
  (let ((file-buffer (get-file-buffer file)))
    (if file-buffer
	(save-window-excursion
	  (switch-to-buffer file-buffer)
	  (basic-save-buffer)))))

;; Like set-buffer, but doesn't error out if the buffer doesn't exist.
(defun cu-set-buffer-create (bufname)
  (let ((buf (get-buffer-create bufname)))
    (set-buffer buf)))

(defun cu-display-two-buffers (current other)
  "Make sure these two buffers are displayed.  CURRENT becomes the
current buffer."
  (switch-to-buffer current)
  (delete-other-windows)
  (display-buffer other))

(defun cu-filelist-in-buffer (filelist buffer)
  "Format the given FILELIST into the given BUFFER.  There's no change
in the display.  Only the non-directory parts of the filenames are shown."
  (with-buffer buffer
    (setq fill-column 70)
    (dolist (file (mapcar 'file-name-nondirectory filelist))
      (if (> (current-column) 59)
	  (insert ?\n)
	(or (= (current-column) 0)
	    (indent-to (* (/ (+ (current-column) 19) 20) 20) 1)))
      (insert file))
    (goto-char (point-min))))


;   ===%%SF%% util/misc (End)  ===

;===%%SF%% util (End)  ===






