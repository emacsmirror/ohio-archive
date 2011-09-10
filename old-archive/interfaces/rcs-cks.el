;; Date: 	Wed, 10 Oct 90 20:36:12 EDT
;; From: Chris Siebenmann <cks@white.toronto.edu>
;; 
;;  Here's the most recent version:
;; 
;;; rcs-cks.el	deal with rcs'd files in a natural way
;; Copyright (C) 1989 Chris Siebenmann under the terms of the FSF GNU
;; General Public License, version 1
;; modified and added to by Ralph Finch (rfinch@water.ca.gov)

;; $Header: /usr/users/cks/share/lisp/RCS/rcs-cks.el,v 1.35 90/05/24 16:05:16 cks Stab $
; Last edited: Mon Apr  9 16:33:40 1990 by cks (Chris Siebenmann) on grumpy.white

(provide 'rcs)


;;;;			Some Documentation
;; This file provides a convenient interface to rcs that allows you
;; to easily check out, check in, and log changes to files that you
;; edit, as well as automatically check out a file when you try and
;; edit it, and record on the mode line who has locked the file.
;; Please report any bugs found to cks@white.toronto.edu, and send me
;; a copy of anything interesting you add.
;;
;; Some portions of this package assume you are using strict locking
;; on your files, either by default or having it set explicitly.
;; In particular, rcs-co-file (and thus rcs-co-buffer) assumes that
;; a writable file by the same name as the one you are checking out is
;; a danger sign, and asks if you want to overwrite it.
;; I recommend the usage of strict locking.
;;
;; Suggested usage: enable strict locking, and make all source files
;; non-writable. Then whenever you try to edit a file that's
;; read-only, you know you have to check it out; if the file hasn't
;; already been placed under RCS, you will be prompted for an initial
;; description. Conversly, a modifiable file is one you have locked,
;; and can thus change freely.
;;
;; Problems when used by root:
;;  file-writable-p always returns true when your uid is 0, so you
;; will always be asked if you want to overwrite the file when you
;; check it out. There are two solutions: 1) use the mode line hook
;; to show you who has the file locked, if anyone or 2) load an
;; improved version of file-writeable-p that works a bit better when
;; you're root (send email to me for a copy of mine).
;;  The second caution is that if you're su'd to root files you check
;; out will have the locker recorded as root instead of you, although
;; who checked them back in will be correctly recorded. This appears
;; to be an unavoidable problem with rcs; co needs to have an
;; argument that lets you specify who is locking the file.
;;
;;  Rcs itself is free, and can be obtained via ftp from
;; arthur.cs.purdue.edu from pub/RCS, or from a number of uucp
;; archive sites. You will also need a version of GNU diff, also
;; widely available from ftp and uucp archive sites.

;;  To use, bind the available functions to convenient keys. A good
;; set of functions to start by binding are rcs-ci-buffer,
;; rcs-co-buffer, and rcs-log-buffer. Note that rcs-co-buffer will
;; automatically create an RCS directory and check in the current
;; buffer if the file is not currently under RCS. This makes it
;; convenient to make all files for a software package read-only; then
;; when you need to change one, just load it in and do an
;; rcs-co-buffer.
;;  Most user-callable functions come in two flavors; one that works
;; on the current buffer, and one that works on any file
;; (rcs-log-buffer and rcs-refresh-buffer are the only exceptions).
;; The versions that operate on the current buffer make it very
;; convenient to (for example) load in a file, notice that it isn't
;; locked out, and check it out in order to change it.


;;; Functions available:
;; rcs-co-{file,buffer,tags}-	Check out a file or a buffer, or
;;                              series of tags files. If the
;;				file isn't already RCS'd, it will be
;;				checked in automatically.
;; rcs-ci-{file,buffer,tags}-	Check in a file or buffer, or series
;;                              of tags files. The file
;;				will be left checked out unlocked,
;;				although by default the buffer will be
;;				killed (see rcs-ci-buffer-kills-buffer).
;; rcs-log-buffer	-	Check in a buffer, and then
;;				immediately check it out locked again.
;;				Handy for logging intermediate stages
;;				in changes.
;; rcs-diff-{file,buffer}	rcsdiff the current version of the
;;				file against the most recent RCS
;;				version. Useful for seeing what
;;				changes you just made.
;; rcs-show-log-{buffer,file}	Show the RCS change log for a given
;;				file or buffer.
;; rcs-revert-{file,buffer}	Revert a file or buffer back to the
;;				last checked in version. An easy way
;;				to blow away changes you've decided
;;				you don't want, or back out of
;;				checking out locked a file you decide
;;				not to change.
;; rcs-refresh-buffer		Reload the current buffer from its file,
;;				usually because someone else has just
;;				unlocked it.
;;  The following two functions are intended to be placed on hooks,
;; instead of being called directly.
;; rcs-try-file	-		Put this on your find-file-not-found-hooks
;;				hook. If you try and edit a file that
;;				has been RCS'd and not checked out, it
;;				will automatically check it out for
;;				you.
;; rcs-hack-modeline	-	Put this on your find-file-hooks hook;
;;				when run, it will put a legend up
;;				about who has locked the file being
;;				edited in that buffer.
;;				Caution -- this can be slow.
;; Eg, to put both on hooks, add the following lisp code to your .emacs.
;; (setq find-file-not-found-hooks (list 'rcs-try-file))
;; (setq find-file-hooks (list 'rcs-hack-modeline))


;;; Hooks available:
;; rcs-hook	-	run at the end of this file (ie after all
;;			the functions have been loaded).
;; rcs-new-dir-hook	run when a new RCS directory has been created.
;; rcs-new-file-hook	run when a file is checked in for the first time.
;; (see rcs-ci-file for more details on the latter two)
;; All hooks are run with run-hooks.


;;; User-changeable variables.
(defvar rcs-use-other-win t
  "*If non-nil, pop to a separate window when doing two-window things
(log entries and displaying diffs).")
(defvar rcs-use-directories t
  "*If non-nil, checkin will make a RCS directory if none exists.")
(defvar rcs-diff-options "-qc"
  "*If non-nil, any additional options rcsdiff will be given.")
(defvar rcs-ci-buffer-kills-buffer nil
  "*If non-nil, when a rcs-ci-buffer is done with no prefix arg, that buffer
is killed.")
(defvar rcs-executable-path nil
  "*If non-nil, the default path to find an RCS command on.")
(defvar rcs-edit-mode nil
  "*If non-nil, the log buffer will be placed in this mode. Otherwise,
the log buffer will be in default-major-mode.")
(defvar rcs-use-login-name t
  "*If non-nil, checkins will use the name you're currently logged in under,
instead of the name for your current UID (eg, if you su to root and use emacs
to check in a file, the RCS log will have your user name in it instead of
'root').")
(defvar rcs-make-backup-files t
  "*If non-nil, backups of checked-in files are made according to
the make-backup-files variable.  Otherwise, prevents backups being made.")
(defvar rcs-initial-branch "1"
  "*If non-nil, branch number to assign an initial checkin.")
(defvar rcs-initial-rev "0"
  "*If non-nil, revision number to assign an initial checkin.")
(defvar rcs-initial-access nil
  " If non-nil, access list to assign to an initial checkin.")
(defvar rcs-keep-log nil
  "*If non-nil, keeps old log.  Otherwise gives user blank log message
for each checkin.")
(defvar rcs-force-checkin nil
  "*If non-nil, force a checkin even if the file has not changed.")
(defvar rcs-error-time 3
  "*The length of time the rcs package will wait after an error
message has been displayed before proceeding.")

;; Hooks
(defvar rcs-hook nil  "*Hooks run at the end of this file.")
(defvar rcs-new-dir-hook nil
  "*Hooks run when a new ./RCS directory is created.")
(defvar rcs-new-file-hook nil
  "*Hooks run when a file has been checked in for the first time.")


;;;; The code
;; Please, no flames about my Lisp style (constructive suggestions
;; gleefully welcomed); this is the first large piece of ELisp I've
;; written, and I'm *sure* there's lots of ways to improve it. Your
;; help in finding them is appreciated :-).


;;; Variables the user won't normally want to change
(defvar rcs-log-buffer "#rcs log#")
(defvar rcs-temp-buffer "#rcs temp#")
(defvar rcs-exec-path nil
  "Path rcs searches to find executables. Built from rcs-executable-path
and exec-path.")
; set this to a small shell that starts fast.
(defvar rcs-shell-path "/bin/sh"
  "*If non-nil, the file name to load inferior shells for RCS commands from.
If nil, shell-file-name's value is used instead.")
(defvar rcs-use-prev-log nil
  "*If non-nil, rcs-ci-file will not prompt for new log but simply
use old log buffer.")


;;; utility functions.

;; kill and reload a buffer from a file.
(defun buf-kill-and-reload (fn)
  "Given FILE, cause the current version of that file to be loaded into a
buffer. If the file was already in a buffer already, the buffer will be
refreshed to contain the latest version of the file. If the buffer has
been modified, the file will NOT be saved.
 Makes some attempt to keep the mark and point the same if the buffer
was around before."
  (let ((buf (get-file-buffer fn)))
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (let ((curp (point))
		(curm (mark)))
	    (set-buffer-modified-p nil)
	    (kill-buffer buf)
	    (find-file fn)
	    (set-mark curm)
	    (goto-char curp)))
      (find-file fn))
    ))

(defun make-rcs-name (fn)
  "Make the name for an RCS file from the normal file name."
  (if (not (string-match ",v$" fn))
      (concat fn ",v")
    fn))

(defun make-normal-name (fn)
  "Make a normal file name from an RCS file name."
  (if (string-match ",v$" fn)
      (setq fn (substring fn 0 (match-beginning 0))))
  (if (string-match "RCS/" fn)
      (setq fn (concat
		(substring fn 0 (match-beginning 0))
		(substring fn (match-end 0)))))
  fn)

(defun is-rcs-file-p (fn)
  "Return t if FILE is an RCS file."
  (string= (substring fn -2) ",v"))

(defun has-rcs-file-p (fn)
  "Return t if FILE has an RCS file."
  (if (or (file-exists-p (concat (file-name-directory fn) "RCS/"
				 (make-rcs-name (file-name-nondirectory fn))))
	  (file-exists-p (make-rcs-name fn)))
      t
    nil))

;; find where a command is.
(defun find-exec-command (cmd paths)
  "Return the full path to CMD from PATHS or just CMD if not found."
  (cond ((not paths) cmd)
	(t (if (file-exists-p (concat (car paths) "/" cmd))
	       (concat (car paths) "/" cmd)
	     (find-exec-command cmd (cdr paths))))))

;; get a log entry into the log buffer
(defun get-rcs-log (banner buf)
  "Prompting with BANNER, get a RCS log entry into the given BUFFER."
  (save-excursion
    (save-window-excursion
      (if (not rcs-use-other-win)
	  (switch-to-buffer buf)
	(switch-to-buffer-other-window buf))
      (if (not rcs-keep-log)
	  (erase-buffer))
      (if rcs-edit-mode
	  (funcall rcs-edit-mode))
      (message
       (substitute-command-keys
	(concat banner " entry; \\[exit-recursive-edit] to end, \\[abort-recursive-edit] to abort.")))
      (recursive-edit)
      (message "Finished entry"))))

(defun buffer-to-list (buffer-name)
  "Return a list, each line in BUFFER-NAME."
  (set-buffer buffer-name)
  (goto-char (point-min))
  (if (re-search-forward "^.+$" (point-max) t)
      (progn
	(setq buf-list
	       (list (buffer-substring (match-beginning 0) (match-end 0))))
	(while (re-search-forward "^.+$" (point-max) t)
	  (setq buf-list
		(append (list (buffer-substring (match-beginning 0) (match-end 0)))
			buf-list)))
	(reverse buf-list))))

; Nice list-into-string function provided by
; Sebastian Kremer, Institute for Theoretical Physics,
; University of Cologne, West Germany
; BITNET: ab027@dk0rrzk0.bitnet
(defun list-to-string (list-of-strings)
  "Take LIST-OF-STRINGS and return a string composed of all the elements
of the list with spaces between each."
  (mapconcat (function identity) list-of-strings " "))

(defun string-to-list (string-of-lists)
  "Given a STRING-OF-LISTS, (characters separated by whitespace),
returns a list with the string elements."
  (let ((i 0)
	(is 0)
	(list-all nil))
    (while (string-match
	    "[ \t]*\\([^ \t]+\\)[ \t]*"
	    string-of-lists
	    is)
      (progn
	(setq list-all (cons (substring string-of-lists
					(match-beginning 1)
					(match-end 1))
			     list-all))
	(setq is (match-end 1))
	(setq i (1+ i))))
    (reverse list-all)))

(defun list-loc (list target)
  "Given LIST, find TARGET in the list, and return its index (0=first).
If not found, return nil"
  (catch 'match-found
    (let ((i 0))
      (while list
	(if (equal (car list) target)
	    (throw 'match-found i)
	  (setq list (cdr list)))
	(setq i (+ i 1))))
    nil))

(defun rcs-list-files (&optional directory-name)
  "Returns a list of RCS files in current directory, or
optional DIRECTORY-NAME."
  (if (not (and directory-name
	   (file-directory-p directory-name)))
      (setq directory-name "./"))
  (setq directory-name (file-name-as-directory directory-name))
  (if (file-directory-p (setq rcs-directory (concat directory-name "RCS")))
      (directory-files rcs-directory t ",v$")
    (directory-files directory-name t ",v$")))


;;;; mainline functions.
;; Most functions have two forms; one which operates on a file, and
;; one which operates on the current buffer. The latter are obviously
;; made out the former. Some functions have a third form, one that
;; operates on a list of files contained in the current tags file.


;;; checkout functions
;; edit an RCS file by checking it out locked.
(defun rcs-co-file (fn)
  "Check out and visit a file. If the file is already in a buffer, it is
refreshed with the latest version from disk. If the file is in a buffer and
the buffer has been modified, it will not be saved (the ice is thin here).
If no RCS file exists for the file, it will go through an initial checkin."
  (interactive "FFile to check out: \n")
  (catch 'co-error
    (if (is-rcs-file-p fn)
	(setq fn (make-normal-name fn)))
    (if (not (has-rcs-file-p fn))
	;; no RCS file for this file? Create one, then.
	;; this will wind up with the file locked.
	(if (not (rcs-ci-file fn t))
	    (progn
	      (message "Cannot create initial file.")
	      (sit-for rcs-error-time)
	      (throw 'co-error nil))))
    (if (and (file-exists-p fn)
	     (file-writable-p fn))
	(if (y-or-n-p (concat "File " fn " is writeable; overwrite? "))
	    (call-process "rm" nil nil nil "-f" (expand-file-name fn))
	  (progn
	    (message " ")
	    (throw 'co-error nil))))
    (message "Checking out %s..." fn)
    ;; we must cd to the correct directory, and then execute the rcs
    ;; command. we must expand the directory to its proper name lest
    ;; rcs-shell-path be a shell that doesn't do it for us.
    (call-process rcs-shell-path nil nil nil "-c"
		  (concat "cd " (expand-file-name
				 (file-name-directory fn)) "; "
				 (find-exec-command "co" rcs-exec-path) " -l "
				 (file-name-nondirectory fn)))
    (buf-kill-and-reload fn)
    (message "Checkout done")))

;; Edit the current buffer after an RCS checkout
(defun rcs-co-buffer ()
  "Check out the current buffer."
  (interactive)
  (if (buffer-file-name)
      (rcs-co-file (buffer-file-name))
    (message "No file associated with current buffer")))

(defun rcs-co-tags (&optional use-1st-log)
  "Perform rcs checkout of current tag table.  If no prefix argument
is given, get initial log file for each newly checked-in tag file;
with prefix, use 1st initial log entry for all new RCS files."
  (interactive "P")
  (save-excursion
    (save-window-excursion
      (let ((next-file-list (tag-table-files))
	    (old-keep-log rcs-keep-log)
	    (rcs-keep-log t)
	    (old-use-prev-log rcs-use-prev-log)
	    (rcs-use-prev-log nil))
	(while next-file-list
	  (progn
	    (setq fn (car next-file-list))
	    (find-file fn)
	    (rcs-co-file fn)
	    (setq next-file-list (cdr next-file-list))
	    (if use-1st-log
		(setq rcs-use-prev-log t))))
	(setq rcs-use-prev-log old-use-prev-log)
	(setq rcs-keep-log old-keep-log)
	(message "All files processed.")))))


;;; checkin functions
;; Check back in a file
(defun rcs-ci-file (fn &optional locked)
  "Use RCS to check back in FILE, with a given comment. If a prefix
argument is given, the file is left locked; otherwise, it is left unlocked.
The file will always still exist. A checkin is forced if the variable
rcs-force-checkin is t; otherwise, if the file is unchanged, it is simply
left locked or unlocked.  If the file is in a buffer and has been
modified, it will be saved first.  rcs-ci-file returns nil if it
detected an error.

 The hook rcs-new-dir-hook is run after a new RCS directory is created;
the hook rcs-new-file-hook is run after a file is checked in the first
time. When this happens, 'fn' is the file being checked in, and 'dir'
is the just-created directory."
  (interactive "fFile to check in: \nP")
  (if (is-rcs-file-p fn)
      (setq fn (make-normal-name fn)))
  (catch 'ci-error
    (if (not (file-exists-p fn))
	(progn
	  (message "File %s doesn't exist to check in." fn)
	  (sit-for rcs-error-time)
	  (throw 'ci-error nil)))
    (if (and (get-file-buffer fn)
	     (buffer-modified-p (get-file-buffer fn)))
	(progn
	  (if (not rcs-make-backup-files)
	      (progn
		(make-local-variable 'make-backup-files)
		(setq make-backup-files nil)))
	  (write-file fn)))
    (let ((log (get-buffer-create rcs-log-buffer))
	  (buf (get-file-buffer fn))
	  (buftmp (get-buffer-create rcs-temp-buffer))
	  (dir (concat (expand-file-name (file-name-directory fn)) "RCS"))
	  (inital nil))
      (if (not (has-rcs-file-p fn))
	  (progn
	    (if (and (not (file-directory-p dir))
		     rcs-use-directories)
		(progn
		  (message "Creating RCS directory %s..." dir)
		  (call-process "mkdir" nil nil nil dir)
		  (if (file-directory-p dir)
		      t
		    (message "Cannot create RCS directory %s" dir)
		    (sit-for rcs-error-time)
		    (throw 'ci-error nil))
		  (run-hooks 'rcs-new-dir-hooks)))
	    (if (not rcs-use-prev-log)
		(get-rcs-log "General descriptive text" log))
	    (setq initial t))
	(if (not rcs-use-prev-log)
	    (get-rcs-log "Description of changes" log))
	(setq initial nil))
      (save-excursion
	(set-buffer buftmp)
	(erase-buffer)
	(save-excursion
	  (set-buffer log)
	  (message "Checking in %s..." fn)
	  (call-process-region
	   1 (point-max) rcs-shell-path nil buftmp t "-c"
	   (concat "cd " (expand-file-name (file-name-directory fn))
		   "; "
		   (find-exec-command "ci" rcs-exec-path)
		   (if rcs-force-checkin
		       " -f "
		     " ")
		   (if (and rcs-use-login-name
			    (user-login-name))
		       (concat " -w" (user-login-name) " "))
		   (if locked "-l "
		     "-u ")
		   (if (and initial rcs-initial-branch rcs-initial-rev)
		       (concat "-r"
			       rcs-initial-branch "." rcs-initial-rev " "))
		   (file-name-nondirectory fn)))
	  (if (not rcs-keep-log)
	      (kill-buffer log)))
	;; check for error in checkin
	(goto-char (point-min))
	(if (re-search-forward "error:" nil t)
	    (progn
	      (message "Cannot check in RCS file %s" fn)
	      (sit-for rcs-error-time)
	      (throw 'ci-error nil)))
	;; check if no change in versions
	(if (not rcs-force-checkin)
	    (progn
	      (goto-char (point-min))
	      (if (re-search-forward "unchanged with respect" nil t)
		  (progn
		    (message "RCS file %s unchanged; left %s" fn
			     (if locked
				 "locked."
			       "unlocked."))
		    (sit-for rcs-error-time)
		    (if (not locked)
			(progn
			  ;; unlock the file instead
			  (erase-buffer)
			  (call-process
			   rcs-shell-path nil buftmp t "-c"
			   (concat "cd "
				   (expand-file-name
				    (file-name-directory fn))
				   "; "
				   (find-exec-command "co" rcs-exec-path)
				   " -f -u "
				   (file-name-nondirectory fn)))))))))
	(if initial
	    (progn
	      (run-hooks 'rcs-new-file-hook)
	      (if (and initial rcs-initial-access)
		  (let ((buftmp (get-buffer-create rcs-temp-buffer)))
		    (save-excursion
		      (set-buffer buftmp)
		      (erase-buffer)
		      (call-process
		       rcs-shell-path nil buftmp t "-c"
		       (concat "cd " (expand-file-name
				      (file-name-directory fn))
			       "; "
			       (find-exec-command "rcs" rcs-exec-path)
			       " -a" rcs-initial-access " "
			       (file-name-nondirectory fn)))
		      (goto-char (point-min))
		      (if (re-search-forward "error:" nil t)
			  (progn
			    (message "Cannot set access list on RCS file")
			    (throw 'ci-error nil)))
		      (kill-buffer buftmp))))))
	(if buf
	    (buf-kill-and-reload fn))
	(message "Checkin done")
	t))))

;; check back in the current buffer
(defun rcs-ci-buffer (&optional flag)
  "Check back in and unlock the current buffer. Saves the current buffer
first. If prefix argument given, inverts the sense of
rcs-ci-buffer-kills-buffer."
  (interactive "P")
  (if (buffer-file-name)
      (progn
	(if (rcs-ci-file (buffer-file-name) nil)
	    (if (or (and rcs-ci-buffer-kills-buffer (not flag))
		    (and flag (not rcs-ci-buffer-kills-buffer)))
		(kill-buffer (current-buffer)))
	  ))
    (message "No file associated with the current buffer")))

;; make a change entry, ie. do a ci but leave the file locked.
(defun rcs-log-buffer ()
  "Record a change to the current buffer, but keep on editing it. Saves the
current buffer first."
  (interactive)
  (if (buffer-file-name)
      (rcs-ci-file (buffer-file-name) 't)
    (message "No file associated with the current buffer")))

(defun rcs-ci-tags (&optional use-1st-log)
  "Perform rcs checkin of current tag table.  If no prefix argument
is given, get log file for each tag file; with prefix, use 1st log
entry for all tag files."
  (interactive "P")
  (save-excursion
    (save-window-excursion
      (let ((next-file-list (tag-table-files))
	    (old-keep-log rcs-keep-log)
	    (rcs-keep-log t)
	    (old-use-prev-log rcs-use-prev-log)
	    (rcs-use-prev-log nil))
	(while next-file-list
	  (progn
	    (setq fn (car next-file-list))
	    (find-file fn)
	    (rcs-ci-file fn)
	    (setq next-file-list (cdr next-file-list))
	    (if use-1st-log
		(setq rcs-use-prev-log t))))
	(setq rcs-use-prev-log old-use-prev-log)
	(setq rcs-keep-log old-keep-log)
	(message "All files processed.")))))


;;; rcsdiff functions
;; Find differences between the current version of a file and the last RCS
;; version and display these in a buffer.
(defun rcs-diff-file (fn)
  "Run an rcsdiff on FILE and display the differences in another buffer."
  (interactive "fFile to diff: ")
  (if (not (has-rcs-file-p fn))
      (message (concat "No RCS file exists for " fn))
    (if (not (file-exists-p fn))
	(message "File %s has not been checked out" fn)
      (let ((buf (get-buffer-create (concat "# rcs diff : "
					    fn " #"))))
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer))
	(message "Diffing %s..." fn)
	(call-process rcs-shell-path nil buf nil "-c"
		    (concat "cd " (expand-file-name
				   (file-name-directory fn)) "; "
			    (find-exec-command "rcsdiff" rcs-exec-path) " "
			    rcs-diff-options " "
			    (file-name-nondirectory fn)))
	(if rcs-use-other-win
	    (switch-to-buffer-other-window buf)
	  (switch-to-buffer buf))
	(goto-char (point-min))
	(message "Done")))))

;; Diff the current buffer.
(defun rcs-diff-buffer ()
  "Run an rcsdiff on the current buffer. The file will not be saved first."
  (interactive)
  (if (buffer-file-name)
      (rcs-diff-file (buffer-file-name))
    (message "No file associated with the current buffer.")))


;;; revert things
;; Nuke a checked-out version, for whatever reason.
(defun rcs-revert-file (fn)
  "Unlock and revert FILE to its last RCS'd version. Handy when you locked
a file that you later decided not to change. If the file was in a buffer,
reload the buffer with the reverted version; otherwise, the file is not
loaded."
  (interactive "fFile to revert: ")
  (let ((inbuf (get-file-buffer fn))
	(buf (get-buffer-create rcs-temp-buffer)))
    (if (not (has-rcs-file-p fn))
	(message (concat "No RCS file exists for " fn))
      (message "Reverting RCS file %s..." fn)
      (call-process rcs-shell-path nil buf nil "-c"
		    (concat "cd " (expand-file-name
				   (file-name-directory fn)) "; rcs -u "
			    (file-name-nondirectory fn)
			    " ; rm -f " (file-name-nondirectory fn)
			    " ; " (find-exec-command "co" rcs-exec-path) " "
			    (file-name-nondirectory fn)))
      (if inbuf
	  (buf-kill-and-reload fn))
      (message "Done"))))

;; revert the current buffer.
(defun rcs-revert-buffer ()
  "Unlock and revert the current buffer to its last RCS'd version. Does not
save any changes."
  (interactive)
  (if (not (buffer-file-name))
      (message "No file associated with the current buffer.")
    (not-modified)
    (rcs-revert-file (buffer-file-name))))


;;; show logfiles
;; show the rcs logfile for a given file.
(defun rcs-show-log-file (fn)
  "Show the RCS log for FILE."
  (interactive "FFile to show log of: ")
  (if (not (has-rcs-file-p fn))
      (message (concat "No RCS file exists for " fn))
    (let ((buf (get-buffer-create (concat "# rcs log : "
					  fn " #"))))
      (save-excursion
	(set-buffer buf)
	(erase-buffer))
      (message "Getting log for %s..." fn)
	(call-process rcs-shell-path nil buf nil "-c"
		      (concat "cd " (expand-file-name
				     (file-name-directory fn)) "; "
			      (find-exec-command "rlog" rcs-exec-path) " "
			      (file-name-nondirectory fn)))
	(if rcs-use-other-win
	    (switch-to-buffer-other-window buf)
	  (switch-to-buffer buf))
	(goto-char (point-min))
	(if rcs-use-other-win
	    (select-window (previous-window)))
	(message "Done"))))

;; show the log for the current buffer.
(defun rcs-show-log-buffer ()
  "Show the current buffer's RCS log."
  (interactive)
  (if (buffer-file-name)
      (rcs-show-log-file (buffer-file-name))
    (message "No file associated with the current buffer.")))


;;; refresh the current buffer from the on-disk copy
;; reload the current file.
(defun rcs-refresh-buffer ()
  "Reload the current buffer."
  (interactive)
  (if (buffer-file-name)
      (buf-kill-and-reload (buffer-file-name))
    (message "No file associated with current buffer")))


;;;; hook functions

;;; try a rcs checkout when trying to find a file
;; This function is suitable for being used as a hook on
;; find-file-not-found-hooks
(defun rcs-try-file ()
  "Function to check out automatically a RCS file when a find-file fails.
Checks out the file, but does not lock it. Put this on your
find-file-not-found-hooks hook."
  (let ((fn (buffer-file-name)))
    (if (not (has-rcs-file-p fn))
	nil
      (message "Checking out %s..." fn)
      (call-process rcs-shell-path nil nil nil "-c"
		    (concat "cd " (expand-file-name
				   (file-name-directory fn)) "; "
			    (find-exec-command "co" rcs-exec-path) " "
			    (file-name-nondirectory fn)))
      (message "Checkout done")
      (insert-file-contents fn t))))


;;; utility fn's to find about lockers of an rcs file
;; this section is rather slow; it's unfortunate that there isn't a faster
;; way of doing this. Oh well...

;; find out who is locking the various versions of a RCS file.

; ugh, recursive with a gory regexp. ugh blech.
(defun rcs-match-lockers-forward (list-to-date)
  "Adjunct to rcs-list-of-lockers; recursively builds the list of lockers."
  ; look for " <user>: <revision level>"
  (if (not (re-search-forward "[ \t]*\\([^:;]*\\): [0-9]*\\.[0-9]*"
			      (point-max) t))
      list-to-date ; match failed, return what we have
    ; skip to the end of the match, pull out the <user> portion, and recurse.
    (goto-char (match-end 0))
    (rcs-match-lockers-forward (cons (buffer-substring (match-beginning 1)
						       (match-end 1))
				     list-to-date))))

;; who has the file locked
(defun rcs-list-of-lockers (fn)
  "Returns a list (of strings) of the people who have FILE locked
under RCS, or nil if there are no lockers or the file is not under RCS."
  (if (not (has-rcs-file-p fn))
      nil
    (let ((buf (get-buffer-create rcs-temp-buffer)))
      (save-excursion
	(set-buffer buf)
	(erase-buffer))
      (call-process rcs-shell-path nil buf nil "-c"
		    (concat "cd " (expand-file-name
				   (file-name-directory fn)) "; "
			    (find-exec-command "rlog" rcs-exec-path) " -h "
			    (file-name-nondirectory fn)))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-min))
	  ; find the "locks: ..." line
	  (if (not (re-search-forward "^locks:[ \t]*"
				      (point-max) t))
	      nil
	    (goto-char (match-end 0))
	    (save-excursion
	      (forward-line)
	      (let ((beg (point)))
		(end-of-buffer)
		(delete-region beg (point))))
	    (rcs-match-lockers-forward nil))))))

;; occasionally-useful predicate
(defun rcs-file-is-locked-by-you-p (fn)
  "Returns T if FILE is locked by you under RCS, and NIL otherwise."
  (let ((lockers-list (rcs-list-of-lockers fn))
	(your-name (user-login-name)))
    ; undoubtedly there is a find-string-in-list function hiding SOMEWHERE,
    ; and all I have to do is find it. in the mean time, use this hideous
    ; mess.
    (catch 'match-found
      (while lockers-list
	(if (equal (car lockers-list) your-name)
	    (throw 'match-found t)
	  (setq lockers-list (cdr lockers-list))))
      nil)))


;;; show who has a file locked in the modeline
;; Give us some vaguely useful information in the modeline.
(defun rcs-hack-modeline ()
  "Modify the current modeline to tell whether the file is under RCS, and
who the lockers are. Can be called interactively if you really want to."
  (interactive)
  (if (and buffer-file-name
	   (has-rcs-file-p buffer-file-name))
      (progn
	; since this is a buffer-local thing, we don't want everyone to
        ; share this nice label...
	(make-local-variable 'global-mode-string)
	(or global-mode-string (setq global-mode-string '("")))
	(let ((locker-list (rcs-list-of-lockers buffer-file-name))
	      l-string)
	  (if (not locker-list)
	      (setq global-mode-string
		    (append global-mode-string '(" [unlocked]")))
	    (setq l-string (list-to-string locker-list))
	    (setq global-mode-string
		  (append global-mode-string
			  (list (concat " [locked by " l-string "]")))))))))


;;;; startup actions

; Expand exec-path to include rcs-executable-path if necessary
(if rcs-executable-path
    (setq rcs-exec-path (cons rcs-executable-path exec-path))
  (setq rcs-exec-path exec-path))
; set rcs-shell-path if necessary
(if (not rcs-shell-path)
    (setq rcs-shell-path shell-file-name))

; Run any startup hooks necessary.
(run-hooks 'rcs-hook)

