;; cvs.el --- Light cvs support for emacs (ediff + mode line)
;;
;; Copyright (C) 1995, Frederic Lepied <fred@sugix.frmug.fr.net>
;;
;; Author: Frederic Lepied <fred@sugix.frmug.fr.net>
;; Version: $Id: cvs.el,v 1.9 1995/09/11 20:59:32 fred Exp $
;; Keywords: cvs ediff mode-line
;;
;; LCD Archive Entry:
;; cvs|Frederic Lepied|fred@sugix.frmug.fr.net|
;; Light cvs support for emacs (ediff + mode line).|
;; 11-Sep-1995|1.9|~/modes/cvs.el.gz|

;; This program is free  software;  you can redistribute   it and/or modify  it
;; under the terms of  the GNU General Public  License as published by the Free
;; Software  Foundation; either version  2 of the  License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it  will be useful, but WITHOUT
;; ANY WARRANTY;  without even  the   implied warranty  of  MERCHANTABILITY  or
;; FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU General  Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free  Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.
;;
;; Purpose of this package:
;;   1. Display CVS revision in mode line.
;;   2. Compare file changes between CVS revision using ediff.
;;   3. Some keystrokes and menu entries to execute cvs status, cvs log and
;; cvsann (Thanks to Ray Nickson <nickson@cs.uq.oz.au>).
;;   4. Simple interface to cvs commit and cvs update commands.
;;
;; Installation:
;;   put cvs.el in a directory in your load-path and byte compile it.
;;   then put (require 'cvs) in your .emacs or in site-start.el
;;

;;=============================================================================
;; dependencies
;;=============================================================================
(require 'easymenu)

;;=============================================================================
(defvar cvs:version "$Id: cvs.el,v 1.9 1995/09/11 20:59:32 fred Exp $"
  "Version number of cvs.el. To communicate with bug report")

;;=============================================================================
(defvar cvs:current-revision  nil
  "Stores the CVS revision number of the file")
(make-variable-buffer-local 'cvs:current-revision)

;;=============================================================================
(defvar cvs-temp-dir (or (getenv "TMPDIR")
			 (getenv "TMP")
			 (getenv "TEMP"))
  "* if non nil, cvs-temp-dir is the directory where to extract
versions.")

;;=============================================================================
(defvar cvs-command "cvs"
  "Name of the cvs command including path if needed")

;;=============================================================================
(defvar cvsann-command "cvsann"
  "Name of the cvsann command including path if needed")

;;=============================================================================
(defvar cvs-root nil
  "* if non nil, cvs-root is the base directory of the CVS repository.")

;;=============================================================================
(defvar cvs-mode-hooks nil
  "Hooks run when cvs-mode is initialized")

;;=============================================================================
(defvar cvs-load-hooks nil
  "Hooks run when cvs.el has been loaded")

;;=============================================================================
(defvar cvs-commit-hooks nil
  "Hooks run entering commit buffer")

;;=============================================================================
(defvar cvs-before-commit-hooks nil
  "Hooks run before commiting")

;;=============================================================================
(defvar cvs-add-hooks nil
  "Hooks run after adding a file into CVS with cvs-add")

;;=============================================================================
(defvar cvs-shell
  (if (memq system-type '(ms-dos emx))
      shell-file-name
    "/bin/sh")
  "Name of the shell used in cvs commands.")

;;=============================================================================
(defvar cvs-shell-command-option
  (cond ((eq system-type 'ms-dos)
         (if (boundp 'shell-command-option)
             shell-command-option
           "/c"))
        (t                              ;Unix & EMX (Emacs 19 port to OS/2)
         "-c"))
  "Shell argument indicating that next argument is the command.")

;;=============================================================================
(defvar cvs-file-option "-f"
  "CVS option to read log from a file. Some CVS versions need \"-f\" others
need \"-F\".")

;;=============================================================================
(defvar cvs-never-use-emerge nil
  "* don't merge update conflicts with emerge function from ediff package
if set to t")

;;=============================================================================
(defvar cvs-save-prefix ".#"
  "prefix used by cvs to save a file if there are conflicts while updating")

;;=============================================================================
;; minor mode status variable (buffer local).
;;=============================================================================
(defvar cvs-mode nil
  "Status variable to switch to CVS minor mode if sets to t")
(make-variable-buffer-local 'cvs-mode)
(put 'cvs-mode 'permanent-local t)

;;=============================================================================
;; minor mode status variable (buffer local).
;;=============================================================================
(defvar cvs:mark nil
  "Status variable to say if a file will be commited in the next commit command.")
(make-variable-buffer-local 'cvs:mark)
(put 'cvs:mark 'permanent-local t)

;;=============================================================================
(defvar cvs:commit-list nil
  "List of files uppon which to perform cvs commit")

;;=============================================================================
;; minor mode entry point.
;;=============================================================================
(defun cvs-mode (&optional arg)
  "
Help to admin CVS controlled files :
	\\[cvs-log]		display cvs log output.
	\\[cvs-status]		display cvs status output.
	\\[cvs-who]		display who is responsable of the selected region.
	\\[cvs-ediff-internal]		run ediff between current file and a revision.
	\\[cvs-ediff]	run ediff between two revisions of the file.
	\\[cvs-diff]	display diff between current file and a revision.
	\\[cvs-update]	update the file from the repository.
	\\[cvs-revert]	revert the file to a previous version from the repository.
	\\[cvs-merge-backup]	run a merger to remove conflict from the update process.
	\\[cvs-mark]	add a file to the commit list.
	\\[cvs-flush]	Make the commit list empty.
	\\[cvs-commit]	perform the cvs commit command on the commit list."
  (setq cvs-mode (if (null arg) (not cvs-mode)
		   (is-under-cvs)))
  (run-hooks 'cvs-mode-hooks))

;;=============================================================================
;; register cvs minor mode keymap and mode line display.
;;=============================================================================
(defvar cvs:map (make-sparse-keymap)
  "CVS minor mode keymap")

(defvar cvs:commit-map (make-sparse-keymap)
  "CVS commit edition buffer keymap")

(define-key cvs:map "\C-cvw" 'cvs-who)
(define-key cvs:map "\C-cvo" 'cvs-log)
(define-key cvs:map "\C-cvs" 'cvs-status)
(define-key cvs:map "\C-cvd" 'cvs-ediff-internal)
(define-key cvs:map "\C-cvi" 'cvs-diff)
(define-key cvs:map "\C-cv\C-d" 'cvs-ediff)
(define-key cvs:map "\C-cvm" 'cvs-mark)
(define-key cvs:map "\C-cvc" 'cvs-commit)
(define-key cvs:map "\C-cvl" 'cvs-list)
(define-key cvs:map "\C-cvf" 'cvs-flush)
(define-key cvs:map "\C-cvu" 'cvs-update-file)
(define-key cvs:map "\C-cve" 'cvs-merge-backup)
(define-key cvs:map "\C-cvr" 'cvs-revert)
(define-key cvs:commit-map "\C-c\C-c" 'cvs-do-commit)
(define-key cvs:commit-map "\C-cvl" 'cvs-list)
(easy-menu-define
 cvs:menu
 cvs:map
 "CVS minor mode keymap"
 '("CVS"
   ["Log" cvs-log t]
   ["Status" cvs-status t]
   ["Update" cvs-update-file t]
   ["Revert" cvs-revert t]
   ["Merge backup" cvs-merge-backup t]
   ["Who" cvs-who t]
   ["EDiff" cvs-ediff-internal t]
   ["Diff" cvs-diff t]
   ["EDiff two revs" cvs-ediff t]
   ["--------" nil t]
   ["Mark to commit" cvs-mark t]
   ["Commit files" cvs-commit t]
   ["List files" cvs-list t]
   ["Flush List" cvs-flush t]
   ))

(defconst cvs:entry
  (list 'cvs-mode (cons "" '(" CVS:" cvs:current-revision)))
  "Entry to display CVS revision number in mode line")

(or (assq 'cvs-mode minor-mode-alist)
    (setq minor-mode-alist (cons cvs:entry minor-mode-alist)))

(or (assq 'cvs-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'cvs-mode cvs:map)
				     minor-mode-map-alist)))

;;=============================================================================
(defconst cvs:mark-entry
  (list 'cvs:mark " commit")
  "Entry to display CVS revision number in mode line")

(or (assq 'cvs:mark minor-mode-alist)
    (setq minor-mode-alist (cons cvs:mark-entry minor-mode-alist)))

;;=============================================================================
(defun is-under-cvs ()
  "Test if the file in the current buffer is under CVS.
If so, set the variables cvs:current-revision and cvs-mode."
  (interactive)
  (let ((result nil)
	current-revision)
    (if (and buffer-file-name
	     (not (string-match "^/[^/:]*[^/:]:" buffer-file-name))) ; reject remote files
	(progn
	  (save-excursion
	    (let ((filename buffer-file-name)
		  (buffer (current-buffer))
		  (entries-filename (concat (file-name-directory
					     buffer-file-name)
					    "CVS/Entries")))
	      (if (file-exists-p entries-filename)
		  (progn
		    (set-buffer (cvs:find-file-noselect entries-filename))
		    (goto-char 1)
		    (if (re-search-forward (concat "^/" 
						   (file-name-nondirectory filename)
						   "/\\([^/]*\\)/")
					   nil t)
			(progn
			  (setq result t)
			  (setq current-revision (buffer-substring
						  (match-beginning 1)
						  (match-end 1)))
			  )
		      )
		    ))))
	  (if result
	      (setq cvs:current-revision current-revision
		    cvs-mode t)
		)))
    result))

;;=============================================================================
(defun cvs-ediff (old-rev new-rev)
  "Run Ediff between versions old-rev and new-rev of the current buffer."
  (interactive "sFirst version to visit (default is latest version): 
sSecond version to visit (default is latest version): ") 
  (let ((old-vers (cvs-version-other-window old-rev)))
    (other-window 1)
    (cvs-version-other-window new-rev)
    ;; current-buffer is now supposed to contain the old version
    ;; in another window
    ;; We delete the temp file that was created by vc.el for the old
    ;; version
    (ediff-buffers old-vers (current-buffer)
		   (list (` (lambda () (delete-file (, (buffer-file-name))))))
		   'ediff-revision)
    ))

;;=============================================================================
(defun cvs-ediff-internal (rev)
  "Run Ediff on version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (let ((newvers (current-buffer)))
    (cvs-version-other-window rev)
    ;; current-buffer is now supposed to contain the old version
    ;; in another window
    ;; We delete the temp file that was created by vc.el for the old
    ;; version
    (ediff-buffers (current-buffer) newvers
		   (list (` (lambda () (delete-file (, (buffer-file-name))))))
		   'ediff-revision)
    ))

;;=============================================================================
(defun cvs-version-other-window (rev)
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (if buffer-file-name
      (let* ((version (if (string-equal rev "")
			  "lst"
			rev))
	     (filename (if cvs-temp-dir
			   (concat (file-name-as-directory cvs-temp-dir)
				   (file-name-nondirectory buffer-file-name)
				   ".~" version "~")
			 (concat buffer-file-name ".~" version "~"))))
	(if (or (file-exists-p filename)
		(cvs:checkout (file-name-nondirectory buffer-file-name) rev
			      filename))
	    (find-file-other-window filename)))))

;;=============================================================================
(defun cvs:checkout (filename rev output-name)
  "checkout filename with revision rev to output-name"
  (let ((command (if (string= rev "")
		     (format "%s %s update -Q -p %s > %s" cvs-command
			     (if cvs-root
				 (format "-d %s" cvs-root) "")
			     filename output-name)
		     (format "%s %s update -r %s -Q -p %s > %s" cvs-command
			     (if cvs-root
				 (format "-d %s" cvs-root) "")
			     rev filename output-name))))
    (message "Retrieving version with command : %s" command)
    (if (/= (call-process cvs-shell nil nil t cvs-shell-command-option command)
	    0)
	(error "Error while retrieving %s version of %s into %s"
	       (if (string= "" rev) "last" rev) filename output-name)
      output-name)))

;;=============================================================================
(defun cvs-add (msg)
  "Add the current file into the CVS system"
  (interactive "sEnter description: ")
  (let ((command (format "%s %s add -m \"%s\" %s" cvs-command
			 (if cvs-root
			     (format "-d %s" cvs-root) "")
			 msg
			 (file-name-nondirectory buffer-file-name)))
	(filename (file-name-nondirectory buffer-file-name))
	(buf (get-buffer-create "*CVS Add output*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))
    (if (= (call-process cvs-shell nil buf t cvs-shell-command-option command)
	   0)
	(progn
	  (is-under-cvs)
	  (run-hooks 'cvs-add-hooks))
      (cvs:display-temp-buffer buf "add")
      (error "Error while registering %s into CVS" filename))))

;;=============================================================================
(defun cvs-update-file ()
  "Update the current file from the repository"
  (interactive)
  (let ((revision cvs:current-revision)
	(filename buffer-file-name)
	(command-args (if cvs-root
			  (list "-d" cvs-root "update"
				(file-name-nondirectory buffer-file-name))
			(list "update" (file-name-nondirectory
					buffer-file-name))))
	(buf (get-buffer-create "*CVS Update output*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))
    (if (= (apply 'call-process cvs-command nil buf t command-args) 0)
	(cvs:merge-or-revert buf (file-name-nondirectory filename) revision)
      (cvs:display-temp-buffer buf "update")
      (error "Error while updating %s"
	     (file-name-nondirectory buffer-file-name)))))

;;=============================================================================
(defun cvs:merge-or-revert (buf filename revision)
  "Revert the buffer or run ediff-merge-files-with-ancestor to merge the
conflicts."
  (let ((conflict (save-excursion
		    (set-buffer buf)
		    (goto-char (point-min))
		    (search-forward "conflicts" nil t))))
    (if conflict
	(message "conflict detected while updating %s" filename))
    (if (or (not conflict)
	    cvs-never-use-emerge
	    (and (or (fboundp 'ediff-merge-files-with-ancestor)
		     (fboundp 'emerge-files-with-ancestor))
		 (not (y-or-n-p "Conflicts detected do you want to run emerge ? ")))
	    )
	(revert-buffer t t)
      (rename-file filename (concat filename ".old") t)
      (rename-file (concat cvs-save-prefix
			   (file-name-nondirectory filename) "." revision)
		   filename t)
      (let ((ancestor (concat filename ".~" revision "~"))
	    (new (concat filename ".~lst~")))
	(cvs:checkout filename revision ancestor)
	(cvs:checkout filename "" new)
	(if (fboundp 'ediff-merge-files-with-ancestor)
	    (progn
	      (ediff-merge-files-with-ancestor new filename ancestor
					       (list (` (lambda () 
							  (delete-file (, ancestor))
							  (delete-file (, new))))))
;; 	      (delete-other-windows)
;; 	      (write-file filename)
;; 	      (revert-buffer t t)
	      )
	  (emerge-files-with-ancestor nil new filename ancestor filename
				     (list (` (lambda () 
						(delete-file (, ancestor))
						(delete-file (, new))))))
	  ))
      )))

;;=============================================================================
(defun cvs-merge-backup ()
  (interactive)
  (let* ((filename buffer-file-name)
	 (files (directory-files (file-name-directory filename) nil
				 (concat "\\.#" (file-name-nondirectory filename) "\\..*")))
	 )
    (if (not files)
	(error "No backup file to merge!"))
    (if (> 1 (length files))
	(error "too much backup files (%d)" (length files)))
    (let ((version (save-excursion
		     (let ((buf (set-buffer (generate-new-buffer "* CVS work *")))
			   result)
		       (insert (car files))
		       (goto-char 1)
		       (re-search-forward (concat "\\.#" (file-name-nondirectory filename) "\\.\\(.*\\)$"))
		       (setq result (buffer-substring (match-beginning 1) (match-end 1)))
		       (kill-buffer buf)
		       result))))
      (if (or (fboundp 'ediff-merge-files-with-ancestor)
	      (fboundp 'emerge-files-with-ancestor))
	    (let ((ancestor (concat filename ".~" version "~"))
		  (new (concat filename ".~lst~")))
	      (cvs:checkout filename version ancestor)
	      (cvs:checkout filename "" new)
	      (if (fboundp 'ediff-merge-files-with-ancestor)
		  (progn
		    (ediff-merge-files-with-ancestor new (car files) ancestor
						     (list (` (lambda () 
								(delete-file (, ancestor))
								(delete-file (, new))))))
		    ;; 	      (delete-other-windows)
		    ;; 	      (write-file filename)
		    ;; 	      (revert-buffer t t)
		    )
		(emerge-files-with-ancestor nil new (car files) ancestor filename
					    (list (` (lambda () 
						       (delete-file (, ancestor))
						       (delete-file (, new))))))
		))
	    )))
    )

;;=============================================================================
(defun cvs:repository(&optional filename)
  "Retrieve repository name of current CVS file"
  (let ((result nil)
	(filename (or filename buffer-file-name)))
    (if filename
	(let ((repository-filename (concat (file-name-directory
					    filename)
					   "CVS/Repository")))
	  (save-excursion
	    (if (file-exists-p repository-filename)
		(progn
		  (set-buffer (cvs:find-file-noselect repository-filename))
		  (goto-char 1)
		  (end-of-line)
		  (setq result (buffer-substring 1 (point)))
		  )))))
    result))
	 

;;=============================================================================
(defun cvs-log ()
  "Show the CVS log for the current buffer's file."
  (interactive)
  (cvs:call-command cvs-command "*CVS Log*" "log"
		    (list "log" (file-name-nondirectory (buffer-file-name)))))

;;=============================================================================
(defun cvs-status ()
  "Show the CVS status information for the current buffer's file."
  (interactive)
  (cvs:call-command cvs-command "*CVS Status*" "status"
		    (list "status" (file-name-nondirectory (buffer-file-name))
			  )))

;;=============================================================================
(defun cvs-who (start end)
  "Who was responsible for the CVS-controlled code in the region?"
  (interactive "r")
  (save-restriction
    (widen)
    (setq start (count-lines 1 start)
          end   (count-lines 1 end))
    (let ((file (file-name-nondirectory 
		 (file-name-nondirectory (buffer-file-name))))
          (all  (count-lines 1 (point-max))))
      (cvs:call-command cvsann-command "*CVS Who*" "who" (list file))
      (set-buffer "*CVS Who*")
      (let ((buffer-read-only nil))
	(erase-buffer)
	(cvs:call-command "cvsann" "*CVS Who*" (list file))
	(setq buffer-read-only nil)
	(if (< (count-lines 1 (point-max)) all)
	    nil
	  (goto-char 1)
	  (forward-line end)
	  (delete-region (point) (point-max))
	  (goto-char 1)
	  (forward-line start)
	  (delete-region 1 (point)))))))

;;=============================================================================
(defun cvs-diff (rev)
  "Run cvs diff with version REV of the current buffer."
  (interactive "sVersion to visit (default is latest version): ")
  (cvs:call-command cvs-command "*CVS Diff*" "diff"
		    (append (list "diff")
			    (if (string= "" rev) 
				(list (file-name-nondirectory
				       (buffer-file-name)))
			      (list "-r" rev (file-name-nondirectory
					      (buffer-file-name)))))))

;;=============================================================================
(defun cvs-revert (rev)
  "Revert current file from a version from repository"
  (interactive "sVersion to revert from (default is latest version): ")
  (if (yes-or-no-p (concat "All your changes on " (file-name-nondirectory
						   (buffer-file-name))
			   " will be lost! Do you want to continue ? "))
      (progn
	(rename-file (buffer-file-name) (concat (buffer-file-name) ".old") t)
	(let ((revision cvs:current-revision)
	      (filename buffer-file-name)
	      (command-args (append (if cvs-root
					(list "-d" cvs-root "update")
				      (list "update"))
				    (if (string= rev "")
					()
				      (list "-r" rev))
				    (list (file-name-nondirectory buffer-file-name))))
	      (buf (get-buffer-create "*CVS Revert output*")))
	  (save-excursion
	    (set-buffer buf)
	    (setq buffer-read-only nil)
	    (erase-buffer))
	  (if (= (apply 'call-process cvs-command nil buf t command-args) 0)
	      (revert-buffer t t)
	    (cvs:display-temp-buffer buf "revert")
	    (error "Error while revertingting %s"
		   (file-name-nondirectory buffer-file-name)))))))

;;=============================================================================
(defun cvs-mark ()
  "Mark the current file to be committed in the commit command"
  (interactive)
  (if (not (member buffer-file-name cvs:commit-list))
      (progn
	(setq cvs:commit-list (cons buffer-file-name cvs:commit-list))
	(setq cvs:mark t))
    (setq cvs:commit-list (delete buffer-file-name cvs:commit-list))
    (setq cvs:mark nil))
 (force-mode-line-update))

;;=============================================================================
(defun cvs-flush ()
  "Flush the list of files to be committed"
  (interactive)
  (mapcar (function (lambda (c)
		      (if (get-file-buffer c)
			  (save-excursion
			    (set-buffer (get-file-buffer c))
			    (setq cvs:mark nil)))))
	  cvs:commit-list)
  (setq cvs:commit-list nil)
  (force-mode-line-update t))

;;=============================================================================
(defun cvs-commit ()
  "Setup a buffer to enter comment associated with the commit process.
Use " 
  (interactive)
  (save-some-buffers nil)
  (if (null cvs:commit-list)
      (setq cvs:commit-list (list buffer-file-name)))
  (let ((dir default-directory))
    (switch-to-buffer-other-window (get-buffer-create "*CVS Commit*"))
    (setq default-directory dir)
    (erase-buffer)
    (insert "\n")
    (insert "CVS: ----------------------------------------------------------------------\n")
    (insert "CVS: Enter Log.  Lines beginning with `CVS: ' are removed automatically\n")
    (insert "CVS: committing files:\n")
    (mapcar (function (lambda(c)
			(insert "CVS: " c "\n")))
		cvs:commit-list)
    (insert "CVS: ----------------------------------------------------------------------\n")

    (use-local-map cvs:commit-map)
    (goto-char 0)
    (run-hooks 'cvs-commit-hooks)
    (message "Type C-c C-c when done.")
    ))

;;=============================================================================
(defun cvs-do-commit ()
  "Commit the list of files cvs:commit-list"
  (interactive)
  (goto-char 0)
  (flush-lines "^CVS: .*$")
  (run-hooks 'cvs-before-commit-hooks)
  (let* ((filename (make-temp-name (concat (or (and cvs-temp-dir
						    (file-name-as-directory cvs-temp-dir))
					       "/tmp/")
					   "cvs")))
	 (command-list (if cvs-root
			   (list "-d" cvs-root "commit" cvs-file-option filename)
			 (list "commit" cvs-file-option filename))))
    (write-file filename)
    (bury-buffer)
    (other-window -1)
    (let ((buf (set-buffer (get-buffer-create "*CVS Commit output*"))))
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (mapcar
       (lambda(elt)
	 (setq default-directory (car elt))
	 (message "Committing in %s..." (car elt))
	 (if (/= (apply 'call-process cvs-command nil t t
			(append command-list (cdr elt)))
		 0)
	     (progn
	       (cvs:display-temp-buffer buf "commit")
	       (error "Error while committing %S in %s" (cdr elt) (car elt))
	       ))
	 (message "Committing in %s...done" (car elt)) )
       (cvs:files-to-alist cvs:commit-list))
      (insert "------------------------------------------------------------------------------\n")
      (cvs:display-temp-buffer buf "commit")
      (delete-file filename)
      (save-excursion
	(mapcar
	 (lambda(name)
	   (let ((buf (get-file-buffer name)))
	     (if buf
		 (progn
		   (set-buffer buf)
		   (revert-buffer t t)))))
	 cvs:commit-list))))
  (cvs-flush))

;;=============================================================================
(defun cvs:files-to-alist(l)
  "Sort a list of files to an alist containing the directory as the key and
the list of file names without directory as the value"
  (let ((alist nil)
	(elt nil))
    (while l
      (setq elt (assoc (file-name-directory (car l)) alist))
      (if elt
	  (setcdr elt
		   (nconc (cdr elt) (list (file-name-nondirectory (car l)))))
	(setq alist (cons 
		     (cons (file-name-directory (car l))
			   (list (file-name-nondirectory (car l))))
		     alist)))
      (setq l (cdr l)))
    alist))

;;=============================================================================
(defun cvs-list ()
  "List the files to commit cvs:commit-list in a buffer"
  (interactive)
  (let ((dir default-directory))
    (set-buffer (get-buffer-create "* CVS List of files to commit *"))
    (setq buffer-read-only nil)
    (setq default-directory dir)
    (erase-buffer)
    (goto-char 0)
    (if cvs:commit-list
	(mapcar (function (lambda(c)
			    (insert c)(insert "\n")))
		cvs:commit-list)
      (insert "No file to commit\n"))
    (set-buffer-modified-p nil)
    (cvs:display-temp-buffer (current-buffer) "list")))

;;=============================================================================
;; extract from files.el and modified not to ask question to the user if the
;; file needs to be reloaded.
;;=============================================================================
(defun cvs:find-file-noselect (filename)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (error "%s is a directory." filename)
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
	   ;; Find any buffer for a file which has same truename.
	   (other (and (not buf) (find-buffer-visiting filename)))
	   error)
      ;; Let user know if there is a buffer with the same truename.
      (if other
	  (progn
	    ;; Optionally also find that buffer.
	    (if (or find-file-existing-other-name find-file-visit-truename)
		(setq buf other))))
      (if buf
	  (or (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    (t
		     (file-name-nondirectory filename)
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
	  (setq buf (create-file-buffer filename))
	  (set-buffer buf)
	  (erase-buffer)
	  (condition-case ()
	      (insert-file-contents filename t)
	    (file-error
	     (setq error t)
	     ;; Run find-file-not-found-hooks until one returns non-nil.
	     (let ((hooks find-file-not-found-hooks))
	       (while (and hooks
			   (not (and (funcall (car hooks))
				     ;; If a hook succeeded, clear error.
				     (progn (setq error nil)
					    ;; Also exit the loop.
					    t))))
		 (setq hooks (cdr hooks))))))
	  ;; Find the file's truename, and maybe use that as visited name.
	  (setq buffer-file-truename truename)
	  (setq buffer-file-number number)
	  ;; On VMS, we may want to remember which directory in a search list
	  ;; the file was found in.
	  (and (eq system-type 'vax-vms)
	       (let (logical)
		 (if (string-match ":" (file-name-directory filename))
		     (setq logical (substring (file-name-directory filename)
					      0 (match-beginning 0))))
		 (not (member logical find-file-not-true-dirname-list)))
	       (setq buffer-file-name buffer-file-truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name
		    (setq filename
			  (expand-file-name buffer-file-truename))))
	  ;; Set buffer's default directory to that of the file.
	  (setq default-directory (file-name-directory filename))
	  ;; Turn off backup files for certain file names.  Since
	  ;; this is a permanent local, the major mode won't eliminate it.
	  (and (not (funcall backup-enable-predicate buffer-file-name))
	       (progn
		 (make-local-variable 'backup-inhibited)
		 (setq backup-inhibited t)))
	  (after-find-file error t)))
      buf)))

;;=============================================================================
;; NB duplicated from misc-fns.el
;;=============================================================================
(defun cvs:call-command (program bufname name &optional args)
  "Call PROGRAM synchronously in a separate process.
Input comes from /dev/null.
Output goes to a buffer named BUFNAME, which is created or emptied first, and
displayed afterward (if non-empty).
Optional third arg is a list of arguments to pass to PROGRAM."
  (let ((dir default-directory))
    (set-buffer (get-buffer-create bufname))
    (setq default-directory dir)
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply 'call-process program nil t nil 
	   (if cvs-root
	       (append (list "-d" cvs-root) args)
	     args))
    (goto-char 0)
    (cvs:display-temp-buffer (current-buffer) name)))

(defun cvs:display-temp-buffer (buf name)
  "Display buffer setting it read only, unmodified and binding key q
to bury-buffer" 
  (save-excursion
    (set-buffer buf)
    (cvs-info-mode name)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t))
  (display-buffer buf))

;;=============================================================================
;; major mode stuff to display CVS info buffers
;;=============================================================================
(defvar cvs-info-mode-hooks nil
  "Hooks run when entering cvs-info-mode")

(defvar cvs-info-mode-map nil
  "key map used by cvs-info-mode")

(if cvs-info-mode-map
    ()
  (setq cvs-info-mode-map (make-keymap))
  (define-key cvs-info-mode-map "q" 'bury-buffer)
  (define-key cvs-info-mode-map " " 'scroll-up)
  (define-key cvs-info-mode-map "\177" 'scroll-down)
)

(defun cvs-info-mode (name)
  "Major mode to display CVS information buffers.
Special commands:
\\{cvs-info-mode-map}

Turning on cvs-info-mode runs the hooks `cvs-info-mode-hooks'."
  (interactive "s")
  (use-local-map cvs-info-mode-map)
  (setq mode-name (concat "CVS " name))
  (setq major-mode 'cvs-info-mode)
  (run-hooks 'cvs-info-mode-hooks)
)

;;=============================================================================
(defun cvs:hook ()
  "Find-file and revert-buffer hooks to position the ediff variable
ediff-version-control to process diff between CVS revisions"
  (if (is-under-cvs)
      (progn
	(if (not (boundp 'ediff-version-control-package))
	    (setq ediff-version-control-package 'vc))
	(make-local-variable 'ediff-version-control-package)
	(setq ediff-version-control-package 'cvs))))

(add-hook 'find-file-hooks (function cvs:hook))
(add-hook 'after-revert-hook (function cvs:hook))

(run-hooks 'cvs-mode-hooks)

;;=============================================================================
(provide 'cvs)

;;; end of cvs.el
