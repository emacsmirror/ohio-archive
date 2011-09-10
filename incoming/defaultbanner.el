;;; $Id: defaultbanner.el,v 1.10 1997/03/16 13:23:09 queinnec Exp $
;;; Copyright (C) 1994-1997 by C.Queinnec (Polytechnique & INRIA)

;;; LCD Archive Entry:
;;; defaultbanner|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; Update banners or headers when files are visited.|
;;; $Date: 1997/03/16 13:23:09 $|$Revision: 1.10 $|
;;; ~/misc/defaultbanner.el.Z|

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; It is usual when distributing a set of files to give them a
;;; constant header or banner (sometimes with a pleasant
;;; drawing). Some packages do that with some parameters (autoinsert,
;;; auto-template, defaultcontent and others, all in LCD). But very
;;; often, one wants to change the banner and it is a pain to
;;; consistently update all the files of a distribution to reflect
;;; that new banner. This package addresses this point and is
;;; compatible with the defaultcontent.el package.

;;; Whenever a file is visited, its banner is automatically inspected
;;; and updated if needed. A hook is run that knows about the various
;;; possible banners (antichronologically ordered), identifies which
;;; one is used and replace it by the newest possible. When you decide
;;; to change the banner you just add the new one in front of the
;;; possible banners and that's all!

;;; The interactive function db-check-file allows to run the hook even
;;; if not installed. Another function db-check-every-file updates all
;;; the files given in argument. 

;;; To use this package, add to your .emacs (taking care of load-path)
;;;          (require 'defaultbanner)
;;; and it is ready. Then you have to set up the db-banners-alist 
;;; variable to determine which files must be updated wrt which banners.

;;; Demonstration:

;;; (i) load the defaultbanner package 
;;; (ii) visit a file named "foo.bar"
;;;	defaultbanner will see that it does not contain any banner
;;;	and will insert ";;; This is a boring banner." 
;;; (iii) Move to the "foo.bar" buffer
;;;	You can even add some lines before and after the banner but
;;;	do not try to modify it (or at least leave the word banner in it :)
;;; (iv) Do \M-x db-demo
;;;	This will install a new banner associated to foo.bar and 
;;;	and will visit again the foo.bar file thus forcing the banner
;;;	to be updated. 
;;; (v) Look!
;;;	a single undo will remove the change.

;;; Repository:

;;; Newer versions will be sent to the LCD Archive but may appear earlier
;;; on ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/defaultbanner.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     file://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html

;;; Code:

;;; Which banner for which file is specified by this variable.  The
;;; two last entries there are for documentation, more on them at the end
;;; of this file. 

(defvar db-banners-alist 
  '(("foo\\.bar$"              db-demo-foobar-banner
          (message "This is the DefaultBanner demonstration...")
          (sleep-for 1)
          (setq db-verbose t)
          t )
    ;; Real entries (see definition of db-meroon* variables below)
    ("DEA/Meroon-V3/.*\\.scm$" db-demo-meroon3-banner)
    ("DEA/Meroon4/.*\\.scm$"   db-demo-meroon4-banner)
    )
  "DefaultBanner Package from Christian.Queinnec@INRIA.fr

The db-banners-alist holds an Alist that specifies the files whose
banners have to be checked.  Elements of this Alist looks like (REGEXP
NAME . FENDER). If a file is visited whose name matches one of the
REGEXP (and if FENDER is present and evaluates to true), then the
first lines (as controlled by db-maximal-line variable) of the file
are checked with respect to the value of the variable NAME.

The NAME variable must have a value structured as a list of possible
banners ie a list of strings. The first string represents the up to
date banner to insert, the remaining strings represent obsolete
banners that should be replaced if still present in the file. If no
obsolete banner is recognized, the up to date banner is inserted
starting at db-starting-line. " )

;;; When a banner is inserted for the first time, it is inserted at
;;; the line specified by the following variable.

(defvar db-starting-line 2
  "This is the line number starting at which a banner should be inserted." )

;;; To avoid looking for large parts of files, banners are only looked
;;; for in the first lines of a file as specified by the following
;;; varable.

(defvar db-maximal-line 25
  "This is the last line where the banner can appear. 
No banner are searched beyond that limit." )

;;; Controls what the user will see.

(defvar db-verbose t
  "This variable tells if true what defaultbanner does." )

;;; end of public variables.
;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(defvar db-obsolete-banner-hook nil
  "This hook is run by DefaultBanner just before an obsolete banner is to be
replaced by the up to date banner. It can also be used to make the buffer
writable perhaps by retrieving a new version of the file." )

(defvar db-absent-banner-hook nil
  "This hook is run by DefaultBanner when no banner is found, just before the
up to date banner is to be inserted. It can also be used to make the buffer
writable perhaps by retrieving a new version of the file." )

;;; These two hooks can be used with the vc (version control) package
;;; to retrieve new versions. A possible value is:
;;; (add-hook 'db-obsolete-banner-hook 'vc-toggle-read-only)
;;; (add-hook 'db-absent-banner-hook 'vc-toggle-read-only)

;;; This is the heart of the package. It explores the current buffer
;;; to substitute old banners by the new one or insert the new one if
;;; no older one is found. If an obsolete banner is replaced, the
;;; db-obsolete-banner-hook is run just before. If no banner was
;;; present, the db-absent-banner-hook is run ust before the insertion
;;; of the up to date banner. 

;;; If you are disatisfied with the new banner, a simple undo will
;;; restaure the previous state. If the buffer is readonly, run one of
;;; the hooks then restart the whole process since if the file is
;;; checked out, its content may have changed. The hook may trigger
;;; db-check-buffer so we must avoid a double update of the banner.

(defun db-update-banner (banners)
  "Search the beginning of the current buffer for an occurrence of one of 
the strings in banners. If any is found, replace it by the first string
of banners which is the desired new banner."
  (if (consp banners)
      (let ((all-banners    banners)
            (new-banner     (car banners))
            (replaced       nil)
            (pmax           nil) )
        (save-excursion
          (goto-line db-maximal-line)
          (setq pmax (point))
          ;; recurse on every pattern from the most recent one...
          (while (consp banners)
            (goto-char (point-min))
            (if (or (search-forward (car banners) pmax t)
                    (re-search-forward (car banners) pmax t) )
                (progn
                  ;; no need to replace the newest banner if already present
                  (if (not (equal (car banners) new-banner))
                      (cond
                       (buffer-read-only
                        (message "DefaultBanner: %s is readonly!" 
                                 (buffer-name) )
                        (run-hooks 'db-obsolete-banner-hook)
                        (if buffer-read-only
                            (progn
                              (message "DefaultBanner: %s cannot be updated!"
                                       (buffer-name) )
                              (error) )
                          (db-update-banner all-banners) ) )
                       (t
                        (replace-match new-banner t nil)
                        (if db-verbose
                            (message "DefaultBanner: banner updated!") ) ) ) )
                  ;; mark replacement as done
                  (setq replaced t)
                  ;; break while-loop
                  (setq banners nil) )
              ;; recurse on the older banners...
              (setq banners (cdr banners)) ) )
          ;; if no replacement was done, insert the new banner
          (if (not replaced)
              (cond (buffer-read-only
                     (message "DefaultBanner: %s is readonly!" 
                              (buffer-name) )
                     (run-hooks 'db-absent-banner-hook)
                     (if buffer-read-only
                         (progn
                           (message "DefaultBanner: %s cannot be updated!"
                                    (buffer-name) )
                           (error) )
                       (db-update-banner all-banners) ) )
                    (t
                     (goto-line db-starting-line)
                     (insert new-banner)
                     (if db-verbose
                         (message "DefaultBanner: banner inserted!") ) ) ) )
          t ) ) ) )

;;; This is the function to run whenever a file is visited. It updates
;;; banner. You may have to run it by hand (or menu, or mouse (see
;;; MousyMacro)) if you visit a readonly file.

(defun db-check-buffer ()
  "This function tries to update an obsolete banner in the current buffer.
This is a function belonging to the DefaultBanner package.
See db-banners-alist documentation."
  (interactive)
  (let ((alist db-banners-alist)
        (name  (file-name-sans-versions buffer-file-name))
        (data  nil) )
    ;; find first matching alist entry
    (while (and (not data) alist)
      (if (string-match (car (car alist)) name)
          (setq data (cdr (car alist)))
        (setq alist (cdr alist)) ) )
    ;; exploit data if any
    (if data
        (let ((banner (car data))
              (fender (cdr data)) )
          (if db-verbose
              (progn
                (message "DefaultBanner: processing %s..." banner)
                (sleep-for 1) ) )
          ;; Continue only if the fender yields true
          (if (eval (cons 'progn (cons 't fender)))
              ;; fetch the banners list
              (if (boundp banner)
                  (let ((banners (eval banner)))
                    ;; process the banners
                    (condition-case ()
                        (db-update-banner banners)
                      (error nil) ) )
                (progn
                  (message "DefaultBanner: Banner specification %s unbound"
                           banner )
                  (sleep-for 1) ) ) ) ) ) ) )
           
;;; Run after all the other hooks to benefit from them. If the
;;; defaultcontent package is loaded, it is better for defaultbanner
;;; to be processed after.

(add-hook 'find-file-hooks 'db-check-buffer t)

;;; This function can be used interactively to change a file.

(defun db-check-file (file &optional savep)
  "Process a file and save it if SAVEP is true.
This is a function belonging to the DefaultBanner package.
See db-banners-alist documentation."
  (interactive "FFind file: ")
  (if db-verbose
        (progn
          (message "DefaultBanner: Processing file %s..." file)
          (sleep-for 1) ) )
  (save-excursion
    (find-file file)
    ;; In case db-check-buffer is not present.
    (if (not (member 'db-check-buffer find-file-hooks))
        (db-check-buffer) )
    (if savep (save-buffer)) ) )
;;; Test: (db-check-file "defaultbanner.el")

;;; This function is used to change a set of files.  It might be
;;; insteresting to use dired directly to kill this bunch of buffers.

(defun db-check-every-file (files &optional savep)
  "Process a series of files, save them if SAVEP is true. 
This is a function belonging to the DefaultBanner package.
See db-banners-alist documentation."
  (while (consp files)
    (db-check-file (car files) savep)
    ;; recurse...
    (setq files (cdr files)) )
  (message "DefaultBanner: all files processed.") )
;;; Test: (db-check-every-file '("defaultbanner.el" "defaultbanner.el"))

;;; So this package can be required.

(provide 'defaultbanner)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This is used for the demonstration. See section above describing
;;; the demonstration.

(defvar db-demo-foobar-banner
  '("
;;; This is a boring banner.
" )
    "This variable belongs to the DefaultBanner package and is used for
demonstration only." )

(defun db-demo ()
  "Change the banner for the foo.bar file. This function belongs to the 
DefaultBanner package and is used for demonstration only."
  (interactive)
  (setq db-demo-foobar-banner 
        (append (list "
;;; This is the new banner                                '''
;;;                         updated by DefaultBanner     O~O
;;; on multiple lines.                                    -  {Whaooo!!!}
"
                      ;; in case the user slighty change the boring banner
                      "
;;;.*banner.*
" )
                db-demo-foobar-banner ) )
  ;; Force DefaultBanner to revisit the file.
  (db-check-buffer) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Examples of banner specification. 

;;; Meroon is an object system written in Scheme that I distribute
;;; since 1992. The banners changed from times to times. Here follows
;;; the banners for two major releases. whenever a Meroon file is
;;; visited, its banner is updated if obsolete.

;;; NOTE: Pay attention when using *, + or other regexp-sensible
;;; characters in banners, this may conflict with
;;; re-search-forward. Observe, for instance, the second element of
;;; db-meroon3-banner.

(defvar db-demo-meroon3-banner
  (list                                 
   ;; This is the up to date banner
   ;; NOTE: these banners start and end with newlines.
   "
;;; Copyright (c) 1990-94 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************
"
   ;; Note the little regexps in the following. This is safe since these
   ;; strings only describe obsolete banners.
   "
;;; Copyright (c) .* by Christian Queinnec. All rights reserved.

;;;[ \t*]*
;;;.*
;;; * Meroon *
;;; * Christian Queinnec *
;;;.*
;;;[ \t*]*
" )
  "This variable describes the banners of Meroon-V3. 
It is used by the DefaultBanner package." )

(defvar db-demo-meroon4-banner 
  (cons "
;;;		(((((((((((((((  M e r o o n 4  )))))))))))))))
;;;		( An Efficient Class System written in Scheme )
;;;		(         Christian.Queinnec@INRIA.fr         )
;;;             (  Ecole Polytechnique & INRIA--Rocquencourt  )
;;;		((((((((((((((((((((((( )))))))))))))))))))))))
"
        db-demo-meroon3-banner )
  "This variable describes the banners of Meroon4. 
It is used by the DefaultBanner package." )

;;; end of defaultbanner.el
