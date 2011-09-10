;;;; shadow-files.el
;;; LCD Archive Entry:
;;; shadow-files|Boris Goldowsky|boris@prodigal.psych.rochester.edu|
;;; Helps you keep identical copies of files on multiple hosts.|
;;; 11/23/92|version 1.2|~/misc/shadow-files.el.Z|

;;; USE: put (require 'shadow-files) in your .emacs; add clusters (if
;;; necessary) and file groups with shadow-define-cluster, shadow-define-group,
;;; and shadow-define-regexp-group (see the documentation for these functions
;;; for information on how and when to use them).  After doing this once, 
;;; everything should be automatic.
;;;     If you need to remove or edit a cluster or file group, you can edit the
;;; .shadows buffer, then type M-x shadow-read to load in the new information
;;; (if you do not do this, your changes could be overwritten!).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEPENDENCIES:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  All of the packages mentioned below are available from archive sites,
;;; including archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive.

;;; ANGE-FTP.  This file could be modified (with some loss of generality and
;;; cleanliness of the user interface) to use only the standard ftp library by
;;; replacing the call to write-region with:
;;;  (ftp-write-file (shadow-primary (shadow-site s)) (shadow-file s))

(require 'ange-ftp)

;;; ADD-HOOK.  Several implementations of this are available.  I'd
;;; use ange-ftp-add-hook, but that is capable of messing up write-file-hooks.

(require 'add-hook)

;;; CL, the common lisp library in the standard emacs distribution.

(require 'cl)

;;; SYMLINK-FIX.  Symbolic links can cause nasty surprises, so I recommend
;;; loading this package.  However, it is not actually necessary, so comment
;;; out the next two lines if you want, and proceeed at your own risk.

(setq symlink-overload-expand-file-name t)
(require 'symlink-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar shadow-info-file "~/.shadows"
  "*File to keep shadow information in.  
If this is nil, the information will not be read from or saved to a file.")

(defvar shadow-noquery nil
  "*If t, always copy shadow files without asking.")

(defvar kill-emacs-hooks nil
  ;; Note, this is the one symbol defined in this file which does not begin
  ;; with shadow- .  However, if it is already defined, we don't clobber it.
  "*Functions to run before exiting emacs.
This is a replacement for kill-emacs-hook, which only allowed one hook
function.")

;;; The following two variables should in most cases initialize themselves
;;; correctly.  They are provided as variables in case the defaults are wrong
;;; on your machine.

(defvar shadow-system-name (system-name)
  "The complete hostname of this machine.")

(defvar shadow-homedir (expand-file-name (getenv "HOME"))
  ;; Call to expand-file-name is in case we are using symlink-fix
  "The directory that shadow file specs are assumed to be relative to 
\(on this machine), if not specified as absolute pathnames.")

(defvar shadow-clusters nil
  "List of host clusters.")

(defvar shadow-literal-groups nil
  "List of files that are shared between hosts.
This list contains shadow structures with literal filenames, created by
shadow-define-group.")

(defvar shadow-regexp-groups nil
  "List of file types that are shared between hosts.
This list contains shadow structures with regexps matching filenames, 
created by shadow-define-regexp-group.")

(defvar shadow-marked-files nil
  "List of files that need to be copied to remote hosts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct shadow-cluster
  "Structure for holding information about host clusters.
The shadow-clusters variable associates the names of clusters to these
structures."
  primary
  regexp)

(defstruct shadow
  "Structure for holding information about shadows of files.
The site can be a cluster \(symbol) or a hostname \(string).  The file can be
either a literal filename, or a regexp.  The buffer may is only filled in once
something is in the shadow-marked-files list."
  site
  file
  buffer
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-level Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-define-cluster (name primary regexp)
  "Define a new `cluster.'
This is a group of hosts that share files, so that copying to or from
one of them is sufficient to update the file on all of them.  Clusters are
defined by a NAME, the name of a PRIMARY host \(the one we copy files to), and
a REGEXP that matches the hostname of all the sites in the cluster."
  (interactive (let* ((name (read-no-blanks-input "Cluster name: " ""))
		      (primary (read-no-blanks-input "Primary host: " name))
		      (regexp (read-string "Regexp matching all host names: "
					   (regexp-quote primary))))
		 (list (intern name) primary regexp)))
  (let ((c (cons name (make-shadow-cluster :primary primary
					   :regexp regexp))))
    (when (not (member c shadow-clusters))
      (push c shadow-clusters)
      (shadow-write))))

(defun shadow-define-group (&rest shadows)
  "Set things up so that one file is shared between hosts.
Prompts for hostnames and the file's name on each host.  When any of these is
edited, the new file will be copied to each of the other locations.  Filenames
may be either absolute or relative to the home directory; sites can be specific
hostnames or names of clusters \(see shadow-define-cluster).
  Noninteractively, each arg is a dotted pair of a site and a filename."
  (interactive (let (args site file)
		 (while (setq site (shadow-read-site))
		   (setq args (cons (cons site (read-string "Filename: "
							    (cdar args)))
				    args)))
		 args))
  (push (mapcar (function (lambda (pair) 
			    (make-shadow :site (car pair)
					 :file (cdr pair))))
		shadows)
	shadow-literal-groups)
  (shadow-write))

(defun shadow-define-regexp-group (regexp sites)
  "Set things up so that a group of files are shared between hosts.
Files matching REGEXP are shared between the list of SITES;
the filenames must be identical on all hosts \(if they aren't, use
shadow-define-group instead of this function).  Each site can be either a
hostname or the name of a cluster \(see shadow-define-cluster)."
  (interactive (let ((regexp (read-string "Filename regexp: " 
					  (if (buffer-file-name)
					      (regexp-quote 
					       (file-name-nondirectory
						(buffer-file-name))))))
		     site sites)
		 (while (setq site (shadow-read-site))
		   (push site sites))
		 (list regexp sites)))
  (push (mapcar (function (lambda (site)
			    (make-shadow :site site
					 :file regexp)))
		sites)
	shadow-regexp-groups)
  (shadow-write))

(defun shadow-write-marked-files ()
  "FTP all files in shadow-marked-files list to their shadows.
This is invoked from kill-emacs-hook, so you do not need to call it
explicitly."
  (interactive)
  (let (notdone)
    (dolist (s shadow-marked-files)
      (if (or shadow-noquery 
	      (y-or-n-p (format "Write shadow file %s:%s?" 
				(shadow-site s)
				(shadow-file s)))) 
	  (let ((buffer (condition-case i
			    (set-buffer (shadow-buffer s))
			  (error (if (y-or-n-p 
				      (format
				       "Buffer killed -- ftp %s anyway?"
				       (shadow-file s)))
				     (set-buffer
				      (find-file-noselect 
				       (expand-file-name (shadow-file s)
							 shadow-homedir))))))))
	    (when buffer
	      (save-restriction
		(widen)
		(condition-case i 
		    (write-region (point-min) (point-max) ; see note 1 above
				  (concat "/" (shadow-primary (shadow-site s))
					  ":" (shadow-file s)))
		  (error (setq notdone (cons s notdone)))))))
	(setq notdone (cons s notdone))))
    (setq shadow-marked-files notdone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-member (item list)
  "Like `member' or `memq', but uses EQUAL for comparison."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun shadow-of (buffer)
  "If BUFFER's file has shadows, return the list of shadow structures."
  (let* ((file-abs (buffer-file-name buffer))
	 (file-rel (if (string-match (concat "^" (regexp-quote shadow-homedir))
				     file-abs)
		       (substring file-abs (1+ (match-end 0)))))
	 (found nil))
    (dolist (group shadow-literal-groups)
      (if (some (function 
		 (lambda (s)
		   (let ((f (shadow-file s)))
		     (and (string-equal file-abs 
					(expand-file-name f shadow-homedir))
			  (shadow-site-match (shadow-site s) 
					     shadow-system-name)))))
		group)
	  (setq found (append found 
			      (shadow-what-to-copy shadow-system-name
						   nil buffer group)))))
    (dolist (group shadow-regexp-groups)
      (if (and (or (string-match (shadow-file (car group)) file-abs)
		   (if file-rel
		       (string-match (shadow-file (car group)) file-rel)))
	       (some (function
		      (lambda (s)
			(shadow-site-match (shadow-site s) 
					   shadow-system-name)))
		     group))
	  (setq found (append found
			      (shadow-what-to-copy shadow-system-name
						   (or file-rel file-abs)
						   buffer group)))))
    found))

(defun shadow-what-to-copy (site file buffer group)
  "Return list of shadow structures indicating the copy operations that need to
  be performed in order to reflect a modification made at SITE to FILE/BUFFER
  which has the given GROUP of shadow files.  If file argument is nil, trust
  the filenames in the structures in group.  You probably don't want to use
  this unless you are the shadow-of function \(which I doubt :-)."
  (let (found)
    (dolist (s group)
      (if (not (shadow-site-match (shadow-site s) site))
	  (push (make-shadow :site (shadow-primary (shadow-site s))
			     :file (or file (shadow-file s))
			     :buffer buffer)
		found)))
    found))

(defun shadow-mark-file-for-write ()
  "Add the current file to the list of shadow-marked-files,
if it is on the shadow-file-list."
  (let ((shadows (shadow-of (current-buffer))))
    (dolist (s shadows)
      (when (not (shadow-member s shadow-marked-files))
	(push s shadow-marked-files)
	(message "Use %s to copy this file to %s."
		 (substitute-command-keys "\\[shadow-write-marked-files]")
		 (shadow-primary (shadow-site s)))
	(sit-for 1))))
   nil); Return nil for write-file-hooks

(defun shadow-read-site ()
  "Read a site or cluster name from the minibuffer."
  (let ((ans (read-no-blanks-input "Site or cluster [RET when done]: " "")))
    (cond ((equal "" ans) nil)
	  ((assoc (intern ans) shadow-clusters) (intern ans))
	  (t ans))))

(defun shadow-site-match (site1 site2)
  "See if SITE1 matches SITE2.  
Each may be a string or a cluster; if they are clusters,
regexp of site1 will be matched against the primary of site2."
  (setq site2 (shadow-primary site2))
  (if (symbolp site1)
      (string-match (shadow-cluster-regexp (cdr (assoc site1 shadow-clusters)))
		    site2)
    (string-equal site1 site2)))

(defun shadow-primary (host)
  (if (symbolp host)
      (shadow-cluster-primary (cdr (assoc host shadow-clusters)))
    host))

(defun shadow-read ()
  (interactive)
  (when shadow-info-file
    (save-excursion
      (set-buffer (find-file-noselect shadow-info-file))
      (eval-current-buffer nil))))

(defun shadow-write ()
  (when shadow-info-file
    (save-excursion
      (set-buffer (find-file-noselect shadow-info-file))
      (delete-region (point-min) (point-max))
      (insert (format "(setq shadow-clusters '%s)\n\n" 
		      shadow-clusters)
	      (format "(setq shadow-literal-groups '%s)\n\n" 
		      shadow-literal-groups)
	      (format "(setq shadow-regexp-groups '%s)\n\n" 
		      shadow-regexp-groups)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook us up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-kill-emacs-hook ()
  "Make there be more than one kill-emacs-hook,
so we can hook ourselves up without messing up any other packages."
  (run-hooks 'kill-emacs-hooks))

(when (not (and (boundp 'kill-emacs-hook)
		(eq kill-emacs-hook 'shadow-kill-emacs-hook)))
  (setq kill-emacs-hooks (if (boundp 'kill-emacs-hook) kill-emacs-hook nil))
  (setq kill-emacs-hook 'shadow-kill-emacs-hook)
  (add-hook 'kill-emacs-hooks 'shadow-write-marked-files))

(add-hook 'write-file-hooks 'shadow-mark-file-for-write)

(define-key ctl-x-4-map "s" 'shadow-write-marked-files)

(shadow-read)

(provide 'shadow-files)

