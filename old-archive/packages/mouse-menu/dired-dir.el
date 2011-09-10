;;; Additions for Dired to enable directory creation/deletion.
;;; Russell A. Ritchie, <russell@uk.ac.strath.hci>. Wed Apr 27 15:09:09 1988
;;; + directory [un]compression.
;;; George R. S. Weir, <george@uk.ac.strath.hci>. Fri May  6 08:46:04 1988
;;; + directory purge.
;;; George R. S. Weir, <george@uk.ac.strath.hci>. Thu May 12 10:16:53 1988
;;; + directory byte-recompile.
;;; Russell Ritchie, <russell@uk.ac.strath.hci>. Fri May 13 11:22:49 1988
;;; + smart grep function.
;;; Russell Ritchie, <russell@uk.ac.strath.hci>. Fri May 27 10:25:16 1988
;;; + regexp matching deletion mark function.
;;; Russell Ritchie, <russell@uk.ac.strath.hci>. Fri May 27 10:59:10 1988
;;; + symbolic link creation.
;;; Russell Ritchie, <russell@uk.ac.strath.hci>. Mon Nov  7 15:11:15 1988

(require 'dired)			; Load the standard library.
(provide 'dired-dir)		; Let the world in general know.

;; Change dired-mode documentation (by redefinition) to reflect added functionality.

(defun dired-mode (dirname)
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.
Instead, type d to flag a file (or directory) for Deletion.
Type u to Unflag a file (remove its D flag).
  Type Rubout to back up one line and unflag.
Type x to eXecute the deletions requested.
Type f to Find the current line's file
  (or Dired it, if it is a directory).
Type o to find file or dired directory in Other window.
Type s to search for files containing something.
Type # to flag temporary files (names beginning with #) for Deletion.
Type ~ to flag backup files (names ending with ~) for Deletion.
Type . to flag numerical backups for Deletion.
Type q to Query for regexp specifying files to flag for Deletion.
  (Spares dired-kept-versions or its numeric argument.)
Type a to flag all temporary (#) and backup (~) files for Deletion.
Type P (purge) to do ``a'' then ``x''.
Type l to make a symbolic link to another file.
Type m to make a new directory.
Type r to rename a file.
Type c to copy a file.
Type v to view a file in View mode, returning to Dired when done.
Type g to read the directory again.  This discards all deletion-flags.
Space and Rubout can be used to move down and up by lines.
Also: C -- compress this file (or directory).  
      U -- uncompress this file (or directory).
      B -- byte compile this file.
 M, G, O -- change file's mode, group or owner.
\\{dired-mode-map}"
  (kill-all-local-variables)    
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'dired-revert)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (make-local-variable 'dired-directory)
  (setq dired-directory dirname)
  (setq default-directory 
	(if (file-directory-p dirname)
	    dirname (file-name-directory dirname)))
  (setq mode-line-buffer-identification (list "Dired: %17b"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (use-local-map dired-mode-map)
  (run-hooks 'dired-mode-hook))

;; Redefine this since we know how to delete directories now.

(defun dired-flag-file-deleted (arg)
  "In dired, flag the current line's file for deletion.
With arg, repeat over several lines."
  (interactive "p")
  (dired-repeat-over-lines
    arg
    (function (lambda ()
		(let ((buffer-read-only nil))
		  (delete-char 1)
		  (insert "D"))))))

;; Redefine this too, since we can delete directories now.

(defun dired-do-deletions ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let (delete-list answer)
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "^D" nil t)
	(setq delete-list
	      (cons (cons (dired-get-filename t) (1- (point)))
		    delete-list))))
    (if (null delete-list)
	(message "(No deletions requested)")
      (save-window-excursion
	(switch-to-buffer " *Deletions*")
	(erase-buffer)
	(setq fill-column 70)
	(let ((l (reverse delete-list)))
	  ;; Files should be in forward order for this loop.
	  (while l
	    (if (> (current-column) 59)
		(insert ?\n)
	      (or (bobp)
		  (indent-to (* (/ (+ (current-column) 19) 20) 20) 1)))
	    (insert (car (car l)))
	    (setq l (cdr l))))
	(goto-char (point-min))
	(setq answer (yes-or-no-p "Delete these files? ")))
      (if answer
	  (let ((l delete-list)
		failures)
	    ;; Files better be in reverse order for this loop!
	    ;; That way as changes are made in the buffer
	    ;; they do not shift the lines still to be changed.
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((buffer-read-only nil))
		(condition-case ()
		    (let ((the-file (concat default-directory (car (car l)))))
		      (if (file-directory-p the-file)
			  ;; We know how to delete directories now!
			  (dired-delete-directory the-file)
			(delete-file the-file))
		      (delete-region (point)
				     (progn (forward-line 1) (point))))
		  (error (delete-char 1)
			 (insert " ")
			 (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if failures
		(message "Deletions failed: %s"
			 (prin1-to-string failures))))))))


;;; Define the additional features.

(defun dired-make-directory (newdir)
  "Make a directory called NEWDIR in the current directory."
  (interactive "sMake directory called: ")
  (let ((full-dirname (expand-file-name newdir)))
    (if (file-exists-p full-dirname)
	(error "%s already exists in %s" newdir default-directory)
      (let ((current-line (1+ (count-lines 1 (point))))) 
	(call-process "mkdir" nil nil nil full-dirname)
	(dired-revert)
	(goto-line current-line)
	(dired-move-to-filename)))))	; Go to this line's filename.

(defun dired-empty-directory (dir)
  "Return t if DIR is empty, i.e. (directory-files DIRNAME) => '(\".\" \"..\")."
  (equal (directory-files dir) (list "." "..")))

(defun dired-delete-directory (&optional dirname)
  "Delete directory on this line or supplied (expanded!) optional arg DIRNAME."
  (interactive)
  (let ((dirname (or dirname (dired-get-filename))))
    (if (cond
	  ((not (file-directory-p dirname))
	   (message "%s is not a directory." dirname))
	  ((string= (file-name-nondirectory dirname) "..")
	   (message "Cannot delete parent directory."))
	  ((string= (file-name-nondirectory dirname) ".")
	   (message "Cannot delete current directory."))
	  ((not
	    (or (dired-empty-directory dirname)
		(yes-or-no-p
		 (format "%s is not empty, delete anyway? " dirname))))
	   (message "%s is not empty." dirname))
	  (t (call-process "rm" nil nil nil "-r" dirname)
	     nil))			; Make certain success returns NIL. 
	;; Message always returns a string (strings are non-nil).
	(progn 
	  (sit-for 1)			; Let the reason be read.
	  (error)))))			; Let dired-do-deletions know we bombed.

;; Bind the directory creation function to "m".
(define-key dired-mode-map "m" 'dired-make-directory)

;; Since we can now delete and make directories, it would be nice to
;; be able to copy them too... 

(defun dired-copy-file (to-file &optional overwrite)
  "Copy this file (or directory) to TO-FILE, if optional prefix arg
OVERWRITE is t, and TO-FILE is an existing directory prompt for
decision about whether to overwrite it rather than putting this file
or directory in it. If OVERWRITE is non-nil and not t, overwrite."
  (interactive "FCopy to: \nP")
  (let ((from-file (dired-get-filename)))
    (setq to-file (expand-file-name to-file))
    (if (not (file-directory-p from-file))
	(copy-file from-file to-file)
      (if (and
	   (file-exists-p to-file)
	   overwrite
	   (if (eq overwrite t)
	       (yes-or-no-p
		(format "%s already exists, overwrite? " to-file))
	     t))
	  (dired-delete-directory to-file))
      (call-process "cp" nil nil nil "-r" from-file to-file))
    (dired-add-entry (file-name-directory to-file)
		     (file-name-nondirectory to-file))))

;; Similarly, renaming (or moving) should be able to handle directories...

(defun dired-rename-file (to-file &optional overwrite)
  "Rename this file (or directory) to TO-FILE, if optional prefix arg
OVERWRITE is a number, and TO-FILE is an existing directory prompt for
decision about whether to overwrite it rather than putting this file
or directory in it. If OVERWRITE is non-nil and not a number, overwrite."
  (interactive
   (list (read-file-name
	  (format "Rename %s to: "
		  (file-name-nondirectory (dired-get-filename)))
	  nil (dired-get-filename))
	 (car current-prefix-arg)))
  (let ((from-file (dired-get-filename)))
    (setq to-file (expand-file-name to-file))
    (if overwrite
	(rename-file from-file to-file overwrite)
      (call-process "mv" nil nil nil from-file to-file))
    (dired-revert)))

;;; Dired extensions to handle compression and uncompression of
;;; directories
;;; George R. S. Weir, Scottish HCI Centre, <george@uk.ac.strath.hci>. 
;;; Fri May  6 08:46:04 1988

(defun dired-compress ()
  "Compress the file or directory on the current line of the Dired buffer."
  (interactive)
  (let ((from-file (dired-get-filename)))
    (if (file-directory-p from-file)
	(dired-compressdir from-file)
      (let ((buffer-read-only nil))
	(if (string-match "\\.Z$" from-file)
	    (error "%s is already compressed!" from-file)
	  (message "Compressing %s..." from-file)
	  (call-process "compress" nil nil nil "-f" from-file)
	  (message "Compressing %s... done" from-file)
	  (dired-redisplay (concat from-file ".Z")))))))

(defun dired-compressdir (dir-name)
  "Compress directories from dired.
Asks for confirmation before running 'compressdir' on supplied arg DIR-NAME."
  (let* ((basename (file-name-nondirectory dir-name))
	 (print-dir-name (cond ((string= "." basename) "current directory")
			       ((string= ".." basename) "parent directory")
			       (t basename))))
    (if (yes-or-no-p (format "Compress all files in %s? " print-dir-name))
	(progn
	  (message "Compressing all files in %s..." print-dir-name)
	  (call-process "compressdir" nil nil nil dir-name)
	  (message "Compressing all files in %s... done" print-dir-name)
	  (dired-revert dir-name)))))

(defun dired-uncompress ()
  "Uncompress the file or directory on the current line of the Dired buffer."
  (interactive)
  (let ((from-file (dired-get-filename)))
    (if (file-directory-p from-file)
	(dired-uncompressdir from-file)
      (let ((buffer-read-only nil))
	(if (not (string-match "\\.Z$" from-file))
	    (error "%s is not compressed!" from-file)
	  (message "Uncompressing %s..." from-file)
	  (call-process "uncompress" nil nil nil from-file)
	  (message "Uncompressing %s... done" from-file)
	  (dired-redisplay (substring from-file 0 -2)))))))

(defun dired-uncompressdir (dir-name)
  "Uncompress directories from dired.
Asks for confirmation before running 'uncompressdir' on supplied arg DIR-NAME."
  (let* ((basename (file-name-nondirectory dir-name))
	 (print-dir-name (cond ((string= "." basename) "current directory")
			       ((string= ".." basename) "parent directory")
			       (t basename))))
    (if (yes-or-no-p (format "Uncompress all files in %s? " print-dir-name))
	(progn
	  (message "Uncompressing all files in %s..." print-dir-name)
	  (call-process "uncompressdir" nil nil nil dir-name)
	  (message "Uncompressing all files in %s... done" print-dir-name)
	  (dired-revert dir-name)))))

;; 'dired-purge' function to remove all backup (~) and autosave (#) files
;; George R. S. Weir, <george@uk.ac.strath.hci>. Thu May 12 10:16:53 1988

(defun dired-purge ()
  "Purge (with confirmation) all backup~ and #autosave files in current dir."
  (interactive)
  (dired-flag-backup-and-auto-save-files)
  (dired-do-deletions))

;; Bind dired-purge to "P" and dired-flag-backup-and-auto-save-files to "a".
(define-key dired-mode-map "P" 'dired-purge)
(define-key dired-mode-map "a" 'dired-flag-backup-and-auto-save-files)

;; Redefine dired-byte-recompile to work correctly on directory names.

(defun dired-byte-recompile ()
  "Byte recompile this file or directory."
  (interactive)
  (let* ((buffer-read-only nil)
	 (from-file (dired-get-filename))
	 (to-file (substring from-file 0 -3)))
    (if (file-directory-p from-file)
	(if (yes-or-no-p
	     (format "Byte-recompile all .el files in %s? "
		     (let ((file-name (file-name-nondirectory from-file)))
		       (cond ((string= file-name ".") "current directory")
			     ((string= file-name "..") "parent directory")
			     (t file-name)))))
	    (byte-recompile-directory from-file t))
      (if (string-match "\\.el$" from-file) nil
	(error "%s is uncompilable!" from-file))
      (byte-compile-file from-file))))

(defun dired-grep ()
  "Prompt for search pattern, and file pattern regexps, 
Check search pattern:
  for non-null, confirm if only whitespace, quote if contains whitespace.
Check file pattern:
  for non-null, confirm if only whitespace.
Proceed if checks succeed and grep for search pattern in file pattern."
  (interactive)
  (let* ((pattern (read-input "Look for what?: "))
	 (search-pattern 
	  (cond ((string= "" pattern)
		 (error "Cannot look for instances of the empty string"))
		((white-spacep pattern)
		 (if (y-or-n-p "Look for instances of white space? ")
		     (format "\"%s\"" pattern)
		   (error "Quit")))
		((string-match "[ \t]+" pattern)
		 (format "\"%s\"" pattern))
		(t pattern)))
	 (files
	  (read-input
	   (concat "Look for " pattern " in what files? (default is all): ")
	   "*"))
	 (search-files
	  (cond
	   ((string= "" files)
	    (error
	     "Cannot search for %s without files being specified." pattern))
	   ((white-spacep files)
	    (if (y-or-n-p
		 "Does the filename really consist solely of white space? ")
		(format "\"%s\"" files)
	      (error "Quit")))
	   (t files))))
    (grep (concat search-pattern " " search-files))))

;; Install this on "s" (for Search).
(define-key dired-mode-map "s" 'dired-grep)

(defun dired-flag-regexp-files ()
  "Match files containing (prompted for) REGEXP for deletion."
  (interactive)
  (let ((regexp (read-input "Mark files matching REGEXP: ")))
    (cond
     ((string= "" regexp)
      (error "Cannot match files with a null regular expression."))
     ((white-spacep regexp)
      (if (not (y-or-n-p
		"Try to delete files with whitespace in their names? "))
	  (error "Quit")))
     (t (dired-map-dired-file-lines
	 (function (lambda (x y) (if (string-match regexp x)
				     (dired-flag-file-deleted 1)))))))))

;; Install this on "q" (for Query for regexp).

(define-key dired-mode-map "q" 'dired-flag-regexp-files)

(defun dired-make-symbolic-link (filename linkname)
  "Make a symbolic link to FILENAME called LINKNAME in the current directory."
  (interactive
   (let* ((file (read-file-name "Make link to which file: " nil nil t))
	  (link (read-from-minibuffer
		 "Name for link: " (file-name-nondirectory file))))
     (list file link)))
  (let ((current-line (1+ (count-lines 1 (point))))) 
    (condition-case nil
	(make-symbolic-link filename linkname)
      (file-already-exists (error "%s already exists." linkname)))
    (dired-revert)
    (goto-line current-line)
    (dired-move-to-filename)))

(define-key dired-mode-map "l" 'dired-make-symbolic-link)

