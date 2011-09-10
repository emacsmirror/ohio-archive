;;; context.el
;;; Wed Sep  7 23:53:52 1988
;;;
;;; Bugs, suggestions to 
;;;   (david neves, neves@cs.wisc.edu, neves@uwvax.UUCP)
;;;
;;; Documentation:
;;; Save some context between editing sessions.  Currently only
;;; the location of the point is saved (in the file ~/.emacs_context).
;;; Thereafter, whenever a file is read into a buffer you will find 
;;; yourself back at the point where you left off.
;;;
;;; The context is saved when the user types Meta-x save-context or
;;; when exiting Emacs with the save-context flag set to true.
;;;
;;; To use:
;;; Put the following in your .emacs file
;;; (load "context.elc") ;i.e. after byte compiling this file
;;; (read-context)       ;reads the context from the context file.
;;; The only user command is "meta-x cdired".  It runs DIRED on your
;;; list of context files.
;;;
;;; Changes:
;;; Wed Sep 7  Sys V shells don't like ; to start lines.  Fixed.
;;;            Other misc changes made.
;;; Sun Sep 4  Installed cdired command to run DIRED on context files.
;;; Wed Nov 25 Don't save context of files/directories in context-ignore-files
;;; Tue Nov 24 reverting a buffer now turns off context
;;;
;;; Known bugs:
;;; -  if dired is run on the home directory then the cdired buffer is
;;;    overwritten.
;;; -  cdired is slow.
;;; -  If you read in a file by giving it as an argument to emacs,
;;;      e.g. emacs foo
;;;   you will find yourself at the beginning of foo, not at the context
;;;   point.  Might be able to kludge around this bug but a better fix
;;;   is for the GNU people to change the goto-line in startup.el so
;;;   that it is only called when line is not equal to 0.
;;; -  If you are running multiple Emacs then the last one exited will
;;;   determine the final form of the context file.  I'm not sure
;;;   how to fix this.
 
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
  (list (expand-file-name "~/RMAIL") ;kludge because rmail assumes that
	                             ;find-file leaves point at 1
	"/tmp")
  "*List of files and directories to ignore for context processing")

;;; change kill-emacs so that context will be saved out when you leave emacs.
(if (not (fboundp 'original-kill-emacs))
    (fset 'original-kill-emacs (symbol-function 'kill-emacs)))

;;; Call get-context when a file is loaded into a buffer.
;;; Should only add get-context to file-file-hooks if it isn't there.
;;;  (just in case the file is loaded more than once.)
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

(defmacro second (l)  (list 'car (list 'cdr l)))
(defmacro context-get-point (l)  (list 'second l))

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

(defun save-context ()
  "Save context (currently, the point) of all Emacs buffers.
The context information goes into a file whose name is stored 
in the variable 'context-file')."
  (interactive)
  (save-excursion
    (mapcar (function read-buffer-context) (buffer-list))
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
	point-loc
	start-of-buffer-flag
	file-data)
    (set-buffer buf)
    (setq pointloc (point))
    (setq start-of-buffer-flag (eql 1 pointloc))
    (setq buffer-data (list pointloc)) ;only save the point
    ;; since we are currently only saving the point, don't
    ;; save out context if the point is at the beginning of
    ;; the buffer (i.e. is equal to 1).
    (if (or (null file-name) 
	    (context-ignore-file file-name) start-of-buffer-flag) nil
      (setq assoc-result (context-assoc file-name context-alist))
      (setq file-data (car assoc-result))
      (if (null file-data) (setq context-alist 
				 (cons (cons file-name buffer-data) 
				       context-alist))
	(rplacd file-data buffer-data) ;associate new context with file name
	;; move (file data) to front of alist.
	;; The first n entries are deleted when emacs is finished.
	(setq before (second assoc-result))
	(if (null before) nil                  ;already at front
	  (rplacd before (cdr (cdr before)))   ;else splice it out
	  ;; again, only save context if point is not at start of buffer
	  (if (not start-of-buffer-flag)
	      (setq context-alist (cons file-data context-alist))))))))


(defun kill-emacs (&optional query)
  "End this Emacs session.
Prefix ARG or optional first ARG non-nil means exit with no questions asked,
even if there are unsaved buffers.  If Emacs is running non-interactively
and ARG is an integer, then Emacs exits with ARG as its exit code.

If the variable `context-flag' is non-nil,
the function save-context will be called first."
  (interactive "P")
  (if context-flag (save-context))
  (original-kill-emacs query))

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
	 (nocshrc "")
	 filename) 
    (switch-to-buffer buffer)
    ;;; don't load .cshrc file if using /bin/csh
    (if (string= (file-name-nondirectory shell-file-name) "csh")
	(setq nocshrc "-f"))
    (if (not (= (point-min) (point-max))) nil  ;is buffer empty?
      (setq default-directory homedirectory) 
      (mapcar (function (lambda (filepair)
                (setq filename (car filepair))	
		(if (file-exists-p filename)
		    (setq lsstring (concat lsswitches
					   (context-strip-homedir filename) ";"
					   lsstring)))))
	      (reverse context-alist))
      (message "Running ls on context files ...")
      ;;; some shells don't like a leading semicolon so delete it.
      (call-process shell-file-name nil buffer nil
		    (concat nocshrc "-c") lsstring)
      (message "")
      (goto-char (point-min))
      (dired-mode homedirectory))))

;;; strip off home directory from start of filenames.
(defun context-strip-homedir (filename)
  (if (context-match homedirectory filename) (substring filename homelength)
    filename))

;David Neves, Computer Sciences Department, University of Wisconsin-Madison
;Usenet:  {rutgers,ucbvax}!uwvax!neves
;Arpanet: neves@cs.wisc.edu
