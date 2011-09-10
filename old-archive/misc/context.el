;; LCD Archive Entry:
;; context|David Neves|neves@ils.nwu.edu|
;; Save some context from previous editing sessions.|
;; 06-Oct-1993||~/misc/context.el.Z|
;;
;;; File: context.el
;;; Date: Oct 6 '93 (now works in versions 18 & 19 of Emacs)
;;;
;;; Author: David Neves, neves@ils.nwu.edu
;;;
;;; Documentation:
;;; Save some context from previous editing sessions.  Currently only
;;; the location of the point is saved (in the file ~/.emacs_context).
;;; Thereafter, whenever a file is read into a buffer you will find 
;;; yourself back at the point where you left off.
;;;
;;; 2 User commands:
;;; 1.  "Meta-x cdired" (I usually just type
;;; "Meta-x cdir" as a shortcut).  It runs DIRED on your
;;; list of context files so that you can easy jump to a file that you
;;; have edited recently.
;;;
;;; 2.  "Meta-x context-restore".  (I usually type "Meta-x cont")
;;; You type in a number or a directory name.  Useful if you want to quickly
;;; read in a bunch of project files from your context list.
;;; a.  If you type in a number then the last n files visited in your 
;;;     context list are read in.  
;;;     e.g. typing 5 will read in the 5 most recent files in the context list
;;; b.  If you type in a directory then all your context files contained
;;;     within that directory (or a subdirectory of it) will be read in.
;;;     e.g. typing ~/foodir will read in all context files contained within
;;;     ~/foodir.  typing "/" is a way of reading in all your context files.
;;; -- This is clumsy interface and maybe I'll fix it someday.  Suggestions
;;; -- welcomed.
;;;
;;; The context is automatically saved when the user exits Emacs.  
;;; The user can also save the context while in Emacs by typing
;;; Meta-x save-context
;;; The user can prevent the context from being saved by setting the
;;; context-flag to nil.
;;;
;;; see saveplace.el in the emacs 19 distribution for a similar program.
;;; This program has more features than that one.
;;;
;;; To install:
;;;  1.  Byte compile this file (context.el) with
;;;      Meta-x byte-compile-file
;;;  2.  Put the compiled file (context.elc) in a directory in load-path.
;;;      (or use an absolute pathname for the load in step 3 below,
;;;       e.g. (load "~/emacs_stuff/context.elc")    )
;;;  3.  Put the following 2 lisp expressions in your .emacs file
;;;      (load "context.elc")
;;;      (read-context)       ;reads the context from the context file.
;;;
;;; Changes:
;;; Oct '93 -  Get CDired to work in version 19.  Take out call to
;;;            reverse in version 19 of cdired.  Take out repeated code
;;;            that somehow snuck in.  Document and fix context-restore.
;;; Oct '93 -  Adapted to Emacs 19 by Johan Vromans - jv@mh.nl (Thanks Johan)
;;;            This file should work in Emacs versions 18 & 19.
;;; Oct '92    Cdired Flags to tcsh are now -fc rather than -f-c
;;; ===
;;; 1988
;;; Mon May  8 handle the tcsh shell. don't read in its .cshrc file.
;;; Fri Sep 23 quote file names for ls in cdired in case they have 
;;;            shell sensitive characters in them.
;;; Thu Sep 15 move RMAIL file check to a check for rmail-mode.
;;;            Context is saved even for files whose point is at
;;;            the beginning so they can be viewed by cdired.
;;; Wed Sep 7  Sys V shells don't like ; to start lines.  Fixed.
;;;            Other misc changes made.
;;; Sun Sep 4  Installed cdired command to run DIRED on context files.
;;; 1987
;;; Wed Nov 25 Don't save context of files/directories in context-ignore-files
;;; Tue Nov 24 reverting a buffer now turns off context
;;;
;;; Known bugs:
;;; -  if dired is run on the home directory then the cdired buffer is
;;;    overwritten. (In version 18)
;;; -  In older versions of 18.x emacs...
;;;    If you read in a file by giving it as an argument to emacs,
;;;      e.g. emacs foo
;;;    you will find yourself at the beginning of foo, not at the context
;;;    point.  Solution: get a newer version of Emacs.
;;; -  If you are running multiple Emacs then the last one exited will
;;;    determine the final form of the context file.  I'm not sure
;;;    how to fix this.
 
(defconst context-file "~/.emacs_context" "*File for Emacs context")

(defvar context-alist nil "Association list holding some file context.
  The structure is ( (file-name1 point) (file-name2 point) ...)")

(defvar context-max-size 50 ;why 50?  why not?
  "*Maximum number of files that context is saved for.
If not a number (e.g. nil) then the number of files is allowed to
grow arbitrarily large.  This will result in slower performance because
the context-alist is searched linearly.")

(defvar context-flag t
  "*If non-nil the `save-context' command will always be run before Emacs is
exited and context will be applied to files that are read in.  In other words,
you can turn off all context processing by setting this flag to nil.")

(defvar context-ignore-files
  (list "/tmp")  ;use "list" so one can evaluate expressions
  "*List of files and directories to ignore for context processing")

(defmacro context-second (l)  (list 'car (list 'cdr l)))
(defmacro context-get-filename (l) (list 'car l))
(defmacro context-get-point (l)  (list 'context-second l))
(defun context-emacs-major-version ()
  "Return major version of Emacs, e.g. 18 or 19"
  (car (read-from-string (substring emacs-version 0 2))))

;;; Call get-context when a file is loaded into a buffer.
;;; Should only add get-context to file-file-hooks if it isn't there.
;;;  (just in case this file is loaded more than once.)
(if (not (memq 'get-context find-file-hooks))
    (setq find-file-hooks (cons 'get-context find-file-hooks)))

;;; Turn off context processing when reverting a buffer so you stay
;;; at the current point rather than being sent to the context point.
(if (null revert-buffer-function)
    (setq revert-buffer-function 
	  (function (lambda (&optional arg noconfirm) 
		      (let ((context-flag nil)
			    (revert-buffer-function nil))
			(revert-buffer arg noconfirm))))))

(defun read-context ()
   "Read in an Emacs context.  Usually done when Emacs is initially called.
    This function should be called in .emacs ."
   (interactive)
      (if (not (file-exists-p context-file)) (setq context-alist nil)
	(load context-file t t t)))

;;; Apply the context that is saved for the current file.
;;; Called in find-file-hooks (i.e. when a file is loaded).
;;; Doesn't apply context if context-flag is nil.
(defun get-context nil
  (if context-flag
      (let* ((buf (current-buffer))
	     (file-name (buffer-file-name buf))
	     file-data)
	(if (null file-name) nil
	  (setq file-data (assoc file-name context-alist))
	  (if (null file-data) nil
	    (goto-char (context-get-point file-data)))))))

(defun save-context-maybe ()
  "Save context if context-flag is not nil."
   (if context-flag
       (save-context)))

(defun save-context ()
  "Save context (currently, the point) of all Emacs buffers.
The context information goes into a file whose name is stored 
in the variable 'context-file')."
  (interactive)
  (save-excursion
    (mapcar (function read-buffer-context) (reverse (buffer-list)))
    (let ((buf (get-buffer-create "*context*"))
	  nth-part)
      (cond ((numberp context-max-size)
	     (setq nth-part (nthcdr (1- context-max-size) context-alist))
	     (if nth-part (rplacd nth-part nil))));reduce size of context-alist
      (set-buffer buf)
      (erase-buffer)
      (insert "(setq context-alist '(")
      (mapcar (function (lambda (l) 
			  ;; print function in 18.4x outputs 2 newlines
			  ;; so use terpri and prin1 instead
			  (terpri buf)
			  (prin1 l buf))) context-alist)
      (insert "))")
      (if (file-exists-p context-file) (delete-file context-file))
      (write-region 1 (point-max) context-file nil 'nomessage)
      (kill-buffer buf))))

;;; place buffer context in the list "context-alist".
;;; If it already exists in that list then also move that
;;; information to the front of the alist.
(defun read-buffer-context (buf)
  (let ((file-name (buffer-file-name buf))
	buffer-data
	assoc-result
	before
	pointloc
	file-data)
    (set-buffer buf)
    (setq pointloc (point))
    (setq buffer-data (list pointloc)) ;only save the point
    (if (or (null file-name) 
	    ;; rmail assumes point is at position 1 when RMAIL
	    ;; file is read in.
	    (eq major-mode 'rmail-mode)  ;thanks Graham
	    (context-ignore-file file-name)) nil
      (setq assoc-result (context-assoc file-name context-alist))
      (setq file-data (car assoc-result))
      (if (null file-data) (setq context-alist 
				 (cons (cons file-name buffer-data) 
				       context-alist))
	(rplacd file-data buffer-data) ;associate new context with file name
	;; move (file data) to front of alist.
	;; The first n entries are deleted when emacs is finished.
	(setq before (context-second assoc-result))
	(if (null before) nil                  ;already at front
	  (rplacd before (cdr (cdr before)))   ;else splice it out
	  (setq context-alist (cons file-data context-alist)))))))

(if (>= (context-emacs-major-version) 19)
    (add-hook 'kill-emacs-hook 'save-context-maybe)
  ;; change kill-emacs so that context will be saved out when you leave emacs.
  (if (not (fboundp 'original-kill-emacs))
      (fset 'original-kill-emacs (symbol-function 'kill-emacs)))

  (defun kill-emacs (&optional query)
    "End this Emacs session.
Prefix ARG or optional first ARG non-nil means exit with no questions asked,
even if there are unsaved buffers.  If Emacs is running non-interactively
and ARG is an integer, then Emacs exits with ARG as its exit code.

If the variable `context-flag' is non-nil,
the function save-context will be called first."
  (interactive "P")
  (save-context-maybe)
  (original-kill-emacs query)))

;;; returns true if no context should be saved out for filename
(defun context-ignore-file (filename)
  (let ((ignore-list context-ignore-files)
        (answer nil))
   (while (and ignore-list (null answer))
     (if (context-match (car ignore-list) filename) (setq answer t)
       (setq ignore-list (cdr ignore-list))))
   answer))

;;; version of assoc that returns 2 values (in a list)
;;; (pair found, position before it)
;;; e.g. (context-assoc 'foo '((a b) (c d) (foo bar) (e f)))
;;;      ((foo bar) ((c d) (foo bar) (e f)))
;;; We are also returning the position before it
;;;  so that we can splice it out of the list with rplacd.
;;; if car of result is nil then failure - we failed to find the item.
;;; if cadr of result is nil then the item is at the front of the list.
(defun context-assoc (key alist)
  (let ((before nil) (current alist))
    (if (equal key (car (car current))) nil
      (setq current (cdr current))
      (while (and current (not (equal key (car (car current)))))
	(setq before current)
	(setq current (cdr current))))
    (list (car current) before)))

;;; is str1 at the front of str2?
(defun context-match (str1 str2)
  (let ((result (string-match str1 str2)))
    (and (numberp result) (zerop result))))

;;; return the first n elements in l
(defun context-first-n (l n)
  (let (nl)
    (while (not (zerop n))
      (setq nl (cons (car l) nl))
      (setq l (cdr l))
      (setq n (1- n)))
    nl))

;;; load the last n files you've looked at.
;;; or load all context files contained within a supplied directory
(defun context-restore (arg)
  (interactive "x Type # of last visited files to load or type directory name: ")
  (let ((calist context-alist)
	filename)
    (if (numberp arg)
	(let (filename)
	  (mapcar (function (lambda (filepair)
			      (setq filename (context-get-filename filepair))
			      (and (stringp filename) (file-exists-p filename)
				   (find-file filename))))
		  (context-first-n calist arg)))
      (setq arg (expand-file-name (prin1-to-string arg)))
      (setq calist (reverse calist))
      (message arg)
      (while calist
	(setq filename (car (car calist)))
	(and (string-match arg filename)
	     (file-exists-p filename)
	     (message filename) (find-file filename))
	(setq calist (cdr calist))))))
    
(if (>= (context-emacs-major-version) 19)

    ;; for version 19
    (defun cdired nil 
      "Apply DIRED to files for which some state was saved.
The first time cdired is called multiple calls to ls are made
and the cdired buffer is created.  Subsequent calls to cdired
return the user to that cdired buffer."
      (interactive)
      (let* ( (homedirectory (expand-file-name "~/"))
	      (homelength (length homedirectory)) 
	      (filelist
	       (mapcar (function (lambda (filepair) 
			   (context-strip-homedir (context-get-filename filepair))))
		       context-alist)))
	(dired (cons homedirectory filelist))))


  ;; for version 18
  (autoload 'dired-mode "dired")
  (defun cdired nil 
    "Apply DIRED to files for which some state was saved.
The first time cdired is called multiple calls to ls are made
and the cdired buffer is created.  Subsequent calls to cdired
return the user to that cdired buffer."
    (interactive)
    (let* ((dirname " *context-dired*")
	   (homedirectory (expand-file-name "~/"))
	   (homelength (length homedirectory)) 
	   (buffer (get-buffer-create dirname))
	   (lsstring "")
	   (lsswitches (concat "ls " dired-listing-switches " "))
	   (shell-name (file-name-nondirectory shell-file-name))
	   (nocshrc "")
	   filename) 
      (switch-to-buffer buffer)
    ;;; don't load .cshrc file if using /bin/csh
      (if (or (string=  shell-name "csh")
	      (string=  shell-name "tcsh"))
	  (setq nocshrc "f"))
      (if (not (= (point-min) (point-max))) nil  ;is buffer empty?
	(setq default-directory homedirectory) 
	(mapcar (function (lambda (filepair)
			    (setq filename (car filepair))	
			    (if (file-exists-p filename)
				(setq lsstring (concat lsswitches "'"
						       (context-strip-homedir filename) "';"
						       lsstring)))))
		(reverse context-alist))
		(message "Running ls on context files ...")
		(call-process shell-file-name nil buffer nil
			      (concat "-" nocshrc "c") lsstring)
		(message "")
		(goto-char (point-min))
		(dired-mode homedirectory))))
  ) ;end of if

;;; strip off home directory from start of filenames.
(defun context-strip-homedir (filename)
  (if (context-match homedirectory filename) (substring filename homelength)
    filename))
