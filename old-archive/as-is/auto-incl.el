;From arpa-unix-emacs-request@WILMA.BBN.COM Fri Jul  1 20:55:42 1988
;Received: from wilma by WILMA.BBN.COM id aa08208; 1 Jul 88 18:44 EDT
;Received: from BBN.COM by WILMA.BBN.COM id aa08204; 1 Jul 88 18:44 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 1 Jul 88 20:45:00 GMT
;From: Charlie Martin <duke!crm@mcnc.org>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Auto-include function for Emacs
;Message-Id: <11977@duke.cs.duke.edu>
;Organization: Duke University CS Dept.; Durham, NC
;Source-Info:  From (or Sender) name not authenticated.
;
;
;I don't know about you, but I find that I usually have an idiosyncratic
;collection of things that I ALWAYS (well, nearly always, close enough)
;include in certain files.  For example, my makefiles always include
;something like
;
;    #
;
;    
;    # $Header$
;    # $Log$
;    # 
;    # Local Variables:
;    # mode: fundamental
;    # fill-column: 75
;    # comment-start: "# "
;    # comment-end: "\n"
;    # End:
;
;Similarly, my C files always have the same old stuff in them, prologues
;etc.  I keep these things in an include directory, and have been
;inserting them in the file with C-Xi.
;
;This small elisp brick eliminates the necessity; it provides a function
;"include-auto-include-files" which can be made part of the
;find-file-not-found-hooks list.  When an "auto-include-alist" is set up,
;and include-auto-include-files is in the find-file-not-found-hooks list,
;then the appropriate file is included automatically.
;
;Setup:
;	in .emacs
;	(load "autoinclude")
;	(setq find-file-not-found-hooks '(include-auto-include-files))
;	(setq auto-include-directory "/usr/nbsr/crm/include/")
;
;This works for me; let me know if you-all find any bugs.  Also, this is
;one of my first serious LISPs; if any wizards want to tell me what is
;stylistically wrong, I'd be glad to listen.
;
;
;------------------------------ cut here ------------------------------
;;; Make this -*-emacs-lisp-*- mode, please.

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
;;;- ------------------------------------------------------------

;;; Define the auto-include-alist
(defvar auto-include-alist nil "\
Alist of file name patterns and corresponding include files for
creation of new files.  The include files are standard file
headers or trailers found at \"auto-include-file-path\".  Each
element looks like (REGEXP . FILENAME).  Creating a file whose
name matches REGEXP causes FILENAME to be included.")
(setq auto-include-alist (mapcar 'purecopy
                                 '(("\\.tex$" . "tex-include.tex")
                                   ("\\.c$" . "c-include.c")
                                   ("\\.h$" . "h-include.c")
                                   ("[Mm]akefile" . "makefile.inc")
                                   ("\\.bib$" . "tex-include.tex"))))

;;; Establish a default value for auto-include-directory
(defvar auto-include-directory nil "\
Directory from which auto-included files are taken.")
(setq auto-include-directory ".")

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
              (insert-file file)
            (message "Auto-include: file %s not found"))))))
;;;
;;; End of file
;;; ------------------------------------------------------------

;-- 
;Charlie Martin (crm@cs.duke.edu,mcnc!duke!crm) 

