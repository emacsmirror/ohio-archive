;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manifest.el --- create and maintain a MANIFEST file for a directory
;; Author          : Sridhar Anandakrishnan <sak@essc.psu.edu>
;; Created On      : Tue Jun 21 13:35:47 1994
;; Last Modified By: Dave Brennan
;; Last Modified On: Tue Jul 12 11:55:43 1994
;; Update Count    : 5
;; Version         : $Revision: 1.13 $
;; RCS-Id          : $Id: manifest.el,v 1.13 1994/06/30 18:02:45 sak Exp sak $
;; Status          : Released to gnu.emacs.sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Purpose of this package:
;; This is my attempt at creating an "annotated dired".  This is
;; particularly useful with hyperbole where an entry in a manifest
;; file is an implicit button and link to the file.  See 
;;	 "ftp:/anonymous@ftp.cs.brown.edu/pub/hyperbole"
;; for more information about hyperbole. 
;;
;; If a file called MANIFEST (or whatever you have set
;; `manifest-manifest-file' to) exists in this directory, when you
;; save a file, an entry for that file is added to the MANIFEST (if it
;; isn't there already).
;;
;; Set the variable `manifest-make-manifest-quietly' to NOT be
;; prompted for a `section' and a `brief description' for the file.
;; The section will be `miscellaneous' and the brief description will
;; be blank.
;; 
;; Installation instructions
;; To install manifest, put in .emacs:
;;   (load "manifest")
;;   (setq manifest-ignore-dirs '("~/Mail" "~/News" "~/tmp")) ;; e.g.
;;   (add-hook 'write-file-hooks 'manifest-update-manifest)
;;   (add-hook 'dired-mode-hook  ;; bind A in dired to add a MANIFEST entry
;;	      (function
;;	       (lambda ()
;;	         (define-key dired-mode-map "A" 'manifest-dired-update-manifest))))
;;
;; User functions: 
;; manifest-make-manifest: will make a manifest
;;     file and prompt for a section and brief description for each
;;     file in this dir (except those in manifest-ignore-files).
;;
;; List of user-variables (after loading manifest, place cursor
;; on variable and type C-h v):
;;   manifest-default-entries
;;   manifest-desc-col
;;   manifest-ignore-files
;;   manifest-ignore-dirs
;;   manifest-make-manifest-quietly
;;   manifest-manifest-file

;; Usage instructions:
;; This program works by binding `manifest-update-manifest' to
;; write-file-hooks.  manifest-update-manifest always returns nil so that
;; normal writing of files is done.  The file is entered into the manifest if:
;;   1. it isn't already in the manifest-file
;;   2. it is not matched by one of the regexps in
;;	`manifest-ignore-files'.  
;;   3. if the `Local Variables:' section of MANIFEST includes an
;;	entry for `manifest-local-ignore-files' those regexps are
;;	checked first, then `manifest-ignore-files'.  So as an example:
;;
;;Local Variables:
;;manifest-local-ignore-files: ("^junk" "^fort[0-9]+")
;;End:
;;
;;   4. it is not in a directory mentioned in `manifest-ignore-dirs'
;;
;; The user is prompted for a section-heading for the file unless:
;;   1. `manifest-make-manifest-quietly' is t (in which case the section is
;;	"miscellaneous") 
;;   2. the file is found in ``manifest-default-entries' which is a list of the'
;;	form (file-regexp . (default-section . [default-description]).
;;	The default-description is optional.
;; Similarly, the user is prompted for a brief description of the file
;; unless `manifest-make-manifest-quietly' is t.
;;
;; Known bugs:
;;
;; LCD Archive Entry:
;; manifest|Sridhar Anandakrishnan|sak@essc.psu.edu|
;; Create and update a MANIFEST file for a directory.|
;; 22-Jun-1994|1.13|~/misc/manifest.el.Z|
;;
;; History:
;; $Log: manifest.el,v $
; Revision 1.13  1994/06/30  18:02:45  sak
; Added an extra arg to `manifest-update-manifest' that will force
; adding of file to the MANIFEST.  ie ignore the `manifest-ignore-dirs'
; and `manifst-ignore-files' regexps.
;
; Revision 1.12  1994/06/30  14:32:45  sak
; Replaced a `for'-type loop in ignore-files checking with a `mapcar'
;
; Revision 1.11  1994/06/29  20:07:45  sak
; Changed name from make-manifest.el to manifest.el
;
; Revision 1.10  1994/06/29  17:02:20  sak
; Added option for Local Variables: entry of
; `manifest-local-ignore-files' so that a MANIFEST file can set its own
; ignore regexps.  Must be a list of regexps.
;
;; Revision 1.9  1994/06/28  20:15:50  sak
;; Added error-handler around `manifest-update-manifest' so that it always
;; returns nil.  That way the rest of write-file-hooks are called and the
;; file is saved properly
;
;; Revision 1.8  1994/06/28  19:03:01  sak
;; Changed all mm- prefixes to manifest-.
;; Removed `manifest-toggle-make-manifest'.  Now if a MANIFEST file exists,
;; then the saved file is entered.  If it doesn't exist, nothing is done.
;; Note that if `manifest-update-manifest' is called interactively with an
;; explicit filename, then a MANIFEST file is created and the file
;; entered.
;
;; Revision 1.7  1994/06/27  14:43:27  sak
;; Added variable `manifest-ignore-dirs'
;;
;; Revision 1.6  1994/06/24  18:22:17  sak
;; `manifest-dired-update-manifest' added to call
;; `manifest-update-manifest' from dired.
;;
;; Revision 1.5  1994/06/23  18:29:47  sak
;; Added RCS files to `manifest-ignore-files'
;;
;; Revision 1.4  1994/06/23  18:26:35  sak
;; The buffer name is extended by retaining the directory foo/MANIFEST so
;; that many MANIFESTs can be easily distinguished.
;;
;; Revision 1.3  1994/06/23  14:19:43  sak
;; Minor documentation changes
;;
;; Revision 1.2  1994/06/23  14:09:57  sak
;; Directory entries will now have a "/" after the name
;;
;; Revision 1.1  1994/06/23 14:05:49   sak
;; Initial revision

;;
;;    Copyright (C) 1994  Sridhar Anandakrishnan <sak@essc.psu.edu>
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;{{{ User options

(defvar manifest-manifest-file "MANIFEST"
  "*Variable to specify the MANIFEST file.")
(defvar manifest-ignore-files 
  '("\\(.*\\(\\.dvi\\|\\.log\\)$\\)"	; tex files
    ".*\\.o$"   			; .o files
    ".*\\.elc$" 			; .elc files
    "^\\."				; .login, etc. files
    "\\(^RCS$\\|.*,v$\\)"		; RCS files
    )
  "*List of regexps to match files that will be ignored.
i.e. files matching any of these regexps will not entered into the manifest.")
(defvar manifest-ignore-dirs
  nil
  "*List of directories in which no MANIFEST will be created.
~/Mail, ~/News, ~/tmp, ~/data, e.g.")
(defvar manifest-default-entries 
  '(("^MANIFEST$" . ("documentation" . "List of files in this directory"))
    ("^INSTALL$" . ("documentation" . "Installation instructions for the code in this directory"))
    ("^README$" . ("documentation" . "Important info about this dir"))
    ("^ChangeLog$" . ("documentation" . "Change Log for this directory."))
    ("\\.\\(texi\\|doc\\)\\(\\.gz\\|\\.Z\\|$\\)" .
     ("documentation")))
  "*List of default sections and descriptions for files.
List is of the form (file-regexp . ( default-section . default-description)).")
(defvar manifest-desc-col 15 
  "*Column to begin brief-description of files in manifest.")
(defvar manifest-make-manifest-quietly nil
  "*Set to 't to not prompt for brief description.
Most valuable when initally making a manifest.")

;;}}}

;;{{{ Program variables

(defvar manifest-section-regexp "^--- \\(.*\\) ---$"
  "Regexp to match a section header.")
(defvar manifest-section-end "^$\\|^Local Variables:[ \t]*$"
  "Regexp to mark end of a section (a blank line).")
(defvar manifest-sections-alist nil
  "Alist of section titles for manifest. Buffer local.")
(defvar manifest-section-initial nil
  "Initial prompt for section title. Buffer local.")
(defvar manifest-manifest-buffer-name nil
  "The extended buffer name to avoid confusion.  Buffer local.")
(defvar manifest-local-ignore-files nil
  "List of regexps to ignore files. 
Usually set by the Local Variables: section of the manifest file.")
(make-variable-buffer-local 'manifest-sections-alist)
(make-variable-buffer-local 'manifest-section-initial)
(make-variable-buffer-local 'manifest-manifest-buffer-name)
(make-variable-buffer-local 'manifest-local-ignore-files)

;;}}}

;;{{{ Main functions

(defun manifest-make-manifest (&optional dir)
  "Make a manifest file for DIR.  Current directory if DIR not specified."
  (interactive "DMake manifest: ")
  (let ((files (directory-files dir nil "^[^.#].*[^~]$" nil))
	(manifest (find-file-noselect (concat dir manifest-manifest-file))))
    (save-excursion
      (save-restriction
	(set-buffer manifest)
	(while files
	  (let ((file (car files)))
	    (manifest-update-manifest-1 file)
	    (setq files (cdr files))))))))
	  
(defun manifest-update-manifest (&optional filename force)
  "Update the manifest with this FILENAME.  Non-nil FORCE forces add of file.
If no filename is specified, use the filename of the calling buffer.

The file is entered into the manifest if:
   1. it isn't already in the manifest-file
   2. it is not matched by one of the regexps in
	`manifest-ignore-files'.  
   3. if the `Local Variables:' section of MANIFEST includes an
	entry for `manifest-local-ignore-files' those regexps are
	checked first, then `manifest-ignore-files'.  So as an example:

Local Variables:
manifest-local-ignore-files: (\"^junk\" \"^fort[0-9]+\")
End:

   4. it is not in a directory mentioned in `manifest-ignore-dirs'
"
  (interactive "fAdd to manifest: ")
  (condition-case nil			; in case there is an error
      (catch 'ignore
	(let ((manifest-name 
	       (concat default-directory manifest-manifest-file)))
	  ;; if `manifest-update-manifest' is called interactively,
	  ;; filename exists and the manifest is created-if-necessary
	  ;; or updated.
	  (or filename
	      (if (not (file-exists-p manifest-name))
		  (throw 'ignore nil))	; quit if no explicit filename and
					; no manifest file

	      (setq filename (file-name-nondirectory (buffer-file-name))))
	  (save-excursion
	    (save-restriction
	      (let ((manifest (find-file-noselect manifest-name)))
		(set-buffer manifest)
		;; do the work of updating manifest
		(manifest-update-manifest-1 filename force)

		;; if nothing added to manifest, 
		;;   don't keep buffer...
		(or (buffer-modified-p manifest) 
		    (kill-buffer manifest)) 

		;; since `manifest-update-manifest' is a hook for
 		;; write-file-hooks, make sure to return nil so that
		;; the rest of the hook are called and the file saved
		;; properly.
		'nil)))))		; always, always, always, 
    (error 
     (progn
       (message "Error in make-manifest!")
       (sit-for 1)
       nil))))				; always return nil.

(defun manifest-dired-update-manifest ()
  "In dired, add the file on this line to manifest."
  (interactive)
  (and (require 'dired)
       (let ((file (dired-get-filename 'no-dir t)))
	 (and file
	      (manifest-update-manifest file 't))))) ;force add of
						     ;file to MANIFEST


;;}}}

;;{{{ Functions where the work is done

(defun manifest-update-manifest-1 (filename &optional force)
  "Do the work of checking if FILENAME should be ignored, 
or is already in the manifest."
  (catch 'ignore
    (let ((strip-file (strip-gz filename)) ; strip-file is the filename
					; with any trailing .gz stripped
	  (this-dir (file-name-directory (expand-file-name
					  (buffer-file-name))))
					; see if MANIFEST has a
					; buffer-local value for
					; ignore-files
	  (ignore-files (append manifest-local-ignore-files
				manifest-ignore-files)))

      ;;
      ;; see if the file should NOT be entered in the MANIFEST for any
      ;; reason.  Note that if `force' is t then add regardless of the
      ;; various regexps.
      ;;

      ;; check if directory in which filename resides is on the
      ;; `manifest-ignore-dirs' list
      ;; if found exit
      (or force 
	   (let ((dirs manifest-ignore-dirs))
	     (while dirs
	       (if (string-match 
		    (file-name-as-directory (expand-file-name (car dirs)))
		    this-dir)
		   (throw 'ignore nil)
		 (setq dirs (cdr dirs))))))
      ;; check if filename (stripped) is on the
      ;; `manifest-ignore-files' regexp if found exit
      (or force
	   (mapcar (function 
		    (lambda (regexp)
		      (if (string-match regexp strip-file)
			  (throw 'ignore nil))))
		   ignore-files))
      ;; search for filename (stripped) followed by either space, tab
      ;; or / at beginning of line. if found exit
      (goto-char (point-min))
      (if (re-search-forward (concat "^" strip-file "[ \t/]+") nil t)
	  (throw 'ignore nil))

      ;;
      ;; Enter the filename into the MANIFEST
      ;;

      ;; make the buffer-name a little more identifiable on a
      ;; buffer-list by adding the final directory.  Otherwise you end
      ;; with a bunch of MANIFEST, MANIFEST<2>, etc..
      (or manifest-manifest-buffer-name
	  (and 
	   ;; the first time into the buffer, `manifest-manifest-buffer-name'
	   ;; is nil, and we set it correctly AND set the buffer name.
	   ;; From now on, we won't touch it
	   (setq manifest-manifest-buffer-name
		 (let ((manifest-file 
			(file-name-nondirectory (buffer-file-name))))
		   (if (string-match "^/.*/\\([^/]*/\\)" this-dir)
		       (concat (substring this-dir 
					  (match-beginning 1) (match-end 1))
			       manifest-file))))
	   (rename-buffer manifest-manifest-buffer-name t)))
      ;; insert an entry for filename (stripped)
      (manifest-insert-entry strip-file)
      ;; return nil so that the regular write-file stuff gets done.
      ;; Note that if a fn in `write-file-hooks' returns non-nil, it
      ;; is that functions responsibility to save the buffer
      nil)))

(defun manifest-insert-entry (filename)
  "Insert FILENAME into manifest, prompting for brief description.
In addition ask for a \"section\" to put the entry into."
  (let (section
	description
	(sections (manifest-make-sections-alist)))

    ;;
    ;; Look for the `section' to put the file into
    ;; 1. Either it is in the defaults list
    ;; 2. Or it is entered by the user
    ;; 3. Or the user enters nothing, and it is called `miscellaneous'
    ;;

    ;; look in the defaults list for a default section for this
    ;; filename
	(catch 'default
	  (let ((defaults manifest-default-entries)
		(case-fold-search nil))
	    (while defaults
	      (if (string-match (car (car defaults)) filename)
		  (progn
		    (setq section (car (cdr (car defaults)))
			  description (cdr (cdr (car defaults))))
		    (throw 'default 't)))
	      (setq defaults (cdr defaults))))
	;; no default now check for `manifest-make-manifest-quietly'
	;; and set section to miscellaneous
	(if manifest-make-manifest-quietly
	    (setq section "miscellaneous")))

    ;; prompt for section to put file into unless provided with one
    ;; above from the defaults list
    (or section
	(setq manifest-section-initial (or manifest-section-initial "miscellaneous")
	      section (downcase 
		       (completing-read  
			(format "MANIFEST section for %s: " filename)
			manifest-sections-alist nil nil manifest-section-initial))))
    ;; new section?...
    (if (not (assoc section manifest-sections-alist))
	;; ...then add to alist
	(setq manifest-sections-alist 
	      (cons (cons section (1+ (length manifest-sections-alist))) 
		    manifest-sections-alist)))
    (setq manifest-section-initial section)
    (goto-char (point-min))
    ;; Search for requested section...and add new section if not there already
    ;; to manifest
    (if (not (re-search-forward 
	      (concat "--- " (upcase section) " ---")
	      nil 'move))
	;; add a new section at the beginning of the file.  Format is
	;; the section header followed by two CR, with point left on
	;; the section header (similar to what would happen if the
	;; section were found.
	(progn
	  (goto-char (point-min))
	  (insert "--- " (upcase section) " ---\n\n")
	  (forward-line -2)))
    (forward-line 1)

    ;;
    ;; Enter the file
    ;;

    (let ((marker (make-marker)))
      (set-marker marker (point))
      ;; ask for brief description of file
      (insert filename
	      ;; add a / after directories
 	      (if (file-directory-p filename) "/" " ") 
	      ;; pad to `manifest-desc-col' with spaces
	      (make-string (let ((len (length filename)))
			     (if (> len manifest-desc-col) 
				 0 
			       (- manifest-desc-col len))) ? )
	      " - "
	      ;; prompt for brief description
	      (or description 
		  (if manifest-make-manifest-quietly "")
		  (read-from-minibuffer 
		   (format "MANIFEST summary of %s: " filename)))
	      "\n")
      ;; and sort this section
      (re-search-forward manifest-section-end nil 'move)
      (sort-lines nil marker (point)))))

(defun manifest-make-sections-alist ()
    ;; if `manifest-sections-alist' has not already been generated, do
    ;; so now.
    (or manifest-sections-alist
	(setq manifest-sections-alist
	      (let (sections
		    (num 1))
		(goto-char (point-min))
		;; search for the section header and add it to alist
		(while (re-search-forward manifest-section-regexp nil 't)
		    (setq sections (cons (cons 
					  (downcase 
					   (buffer-substring 
					    (match-beginning 1)
					    (match-end 1)))
					  num) 
					 sections))
		    (setq num (1+ num)))
		sections))))

(defun strip-gz (filename)
  "Strip the trailing gzip from FILENAME before entering into manifest."
  (if (string-match "\\(\\.gz\\|\\.z\\|\\.Z\\)$" filename)
      (substring filename 0 (match-beginning 1))
    filename))

;;}}}

;;{{{ Emacs local variables

;; Local variables:
;; folded-file: t
;; end:

;;}}}
	
;;;; manifest.el ends here
