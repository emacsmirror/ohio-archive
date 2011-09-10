;;; autoinclude.el
;;;+ ------------------------------------------------------------
;;;  Abstract:
;;;
;;;  The following defines an association list for files to be
;;;  automatically included when a new file is created, and a function
;;;  which automatically inserts these files; the idea is to include
;;;  default files much as the mode is automatically set using
;;;  auto-mode-alist.
;;;
;;;  The auto-include-alist consists of dotted pairs of
;;;  ( REGEXP . FILENAME ) where REGEXP is a regular expression, and
;;;  FILENAME is the file name of a file which is to be included into
;;;  all new files matching the regular expression with which it is
;;;  paired.
;;;
;;;  To use: 
;;;     load autoinclude.el
;;;     setq auto-include-directory to an appropriate value, which
;;;       must end in "/"
;;;     set the find-file-not-found-hooks list to include
;;;       (include-auto-include-files)
;;;
;;;  Author:  Charlie Martin
;;;           Department of Computer Science and
;;;           National Biomedical Simulation Resource
;;;           Box 3709
;;;           Duke University Medical Center
;;;           Durham, NC 27710
;;;
;;;  Date: Fri Jul  1 16:15:31 EDT 1988
;;;
;;;  Copyright (c) 1988 Charles R. Martin
;;;
;;;  Copying is permitted under those conditions described by the GNU
;;;  Emacs General Public License as clarified 11 February 1988, which
;;;  is incorporated here by reference.
;;;
;;; Modified by Alberto M. Segre (1988) Cornell University.
;;; (segre@gvax.cs.cornell.edu)
;;;
;;;- ------------------------------------------------------------

;;; Define the auto-include-alist
(defvar auto-include-alist nil "\
Alist of file name patterns and corresponding include files for
creation of new files.  The include files are standard file
headers or trailers found at \"auto-include-file-path\".  Each
element looks like (REGEXP . FILENAME).  Creating a file whose
name matches REGEXP causes FILENAME to be included.")
(setq auto-include-alist (mapcar 'purecopy
				 '(("\\.lsp$" . "header.lsp"))))

;;; Establish a default value for auto-include-directory
(defvar auto-include-directory nil "\
Directory from which auto-included files are taken.")
(setq auto-include-directory "~/.auto/")

;;; Include the file if name match found in auto-include-alist.
;;; Uses buffer-file-name, searches auto-include-alist for a matching
;;; REGEXP, then does 'insert-file' to include that file.
;;;
(defun include-auto-include-files ()
  "Include the file from the include directory if regexp match
found in auto-include-alist.  Silently terminates if the file name
matches none of the regular expressions."

  (let ((alist auto-include-alist)
        (name buffer-file-name)
        (include-file nil))

    ;; remove backup suffixes from file name
    (setq name (file-name-sans-versions name))

    ;; find first matching alist entry
    (while (and (not include-file) alist)
      (if (string-match (car (car alist)) name)
          (setq include-file (cdr (car alist)))
        (setq alist (cdr alist))))

    ;; Now, if we found an appropriate include file, include it
    (if include-file
        (let ((file (concat auto-include-directory include-file)))
          (if (file-readable-p file)
	      ;; Changed 7/4/88 ams to goto end of file and unmark as
	      ;; changed.
              (progn (insert-file file)
		     (goto-char (point-max))
		     (set-buffer-modified-p nil))
	    ;;; Corrected 7/4/88 ams to include file name.
            (message "Auto-include: file %s not found" file))))))

;;; Add autoinclude handling to find-file-not-found-hooks.

(or (memq 'include-auto-include-files find-file-not-found-hooks)
    (setq find-file-not-found-hooks (nconc find-file-not-found-hooks
					   '(include-auto-include-files))))
;;;
;;; End of file
;;; ------------------------------------------------------------
