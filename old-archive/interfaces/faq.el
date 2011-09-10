;;; Description:
;;; Switch to a buffer visiting an archived Usenet newsgroup FAQ file,
;;; creating one if none already exists.

;;; Copyright:
;;; Copyright (C) 1993 Kevin Rodgers
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Martin Marietta has not disclaimed any copyright interest in
;;; faq.el.
;;;
;;; Kevin Rodgers			kevin@traffic.den.mmc.com
;;; Martin Marietta MS A16401		(303) 790-3971
;;; 116 Inverness Dr. East
;;; Englewood CO 80112 USA

;;; Installation:
;;; 1. Put this file in a directory that is a member of load-path, and
;;;    byte-compile it for better performance.
;;; 2. Put this form in default.el or ~/.emacs:
;;;    (autoload (function find-faq) "faq"
;;;      "*Find the archived Usenet NEWSGROUP FAQ file..." t)
;;; 3. If you want to bind find-faq to a key in default.el or ~/.emacs,
;;;    I suggest using C-x F:
;;;    (global-set-key "\C-xF" (function find-faq))

;;; Usage:
;;; M-x find-faq

;;; Notes:
;;; 1. If your site does not archive Usenet FAQs locally, ange-ftp must
;;;    be installed to access a remote archive.  See faq-host.
;;; 2. find-faq depends on the archive at faq-host being organized
;;;    according to a two-level directory structure: the directory
;;;    faq-dir should contain subdirectories named for for Usenet
;;;    newsgroups, which in turn contain the FAQ files.  (See the
;;;    relevant "To do" item.)

;;; To do:
;;; 1. Handle other archive organizations, such as the *.answers/X-faq/
;;;    partN structure based on the FAQ Archive-name: header and
;;;    described in the *.answers "Introduction to the *.answers
;;;    newsgroups" FAQ.
;;; 2. Consider whether faq-default-newsgroup (and perhaps the other
;;;    user options) should be buffer local variables, so that, for
;;;    example, Emacs-Lisp mode could set it to "gnu.emacs.help", C mode
;;;    could set it to "comp.lang.c", C++ mode could set it to
;;;    "comp.lang.c++", etc.
;;; 3. Provide more newsgroup-to-faq mappings in faq-default-faqs.
;;;    [Users are encouraged to submit the names of introductory FAQs
;;;    for newsgroups that they read to the author.]
;;; 4. Accept "canonical" FAQ file names, and search for compressed (.z
;;;    or .Z) files, or files with different capitalization.  This would
;;;    simplify maintenance of the file names in the faq-default-faqs
;;;    association list.

;;; Acknowledgments:
;;; Thorbjoern Hansen <thansen@diku.dk>, for his suggestions.
;;; Jonathan I. Kamens <jik@GZA.COM>, for the FAQ archive at MIT.
;;; Andy Norman <ange@hplb.hpl.hp.com>, for ange-ftp.

;;; LCD Archive Entry:
;;; faq|Kevin Rodgers|kevin@traffic.den.mmc.com|
;;; Switch to a buffer visiting an archived Usenet newsgroup FAQ file.|
;;; 19-Apr-1993|1.2.1|~/interfaces/faq.el.Z|


;; Package interface:

(provide 'faq)


;; User options:

(defvar faq-host "rtfm.mit.edu" ; nee' pit-manager.mit.edu
  "*Name of remote host where Usenet newsgroup FAQs are archived, or nil if
local.
See faq-user and \\[find-faq].")

(defvar faq-dir "/pub/usenet"
  "*Name of directory where Usenet newsgroup FAQs are archived.
See \\[find-faq].")

(defvar faq-user "anonymous"
  "*Name of user to access Usenet newsgroup FAQs archived remotely, or nil if
same as user login name.
See faq-host and \\[find-faq].")

(defvar faq-default-newsgroup "news.newusers.questions"
  "*The name of the default Usenet NEWSGROUP for \\[find-faq].")

(defvar faq-default-faqs
  '(("news.newusers.questions" .
     "Welcome_to_news.newusers.questions!_(weekly_posting).Z")
    ("gnu.emacs.help" .
     "GNU_Emacs_FAQ_(0_5):_Intro_Contents.Z"))
  "*An association list whose keys are Usenet newsgroups names that index
their default FAQ file name for \\[find-faq].")

(defvar find-faq-hooks nil
  "*A function or list of functions that are run before \\[find-faq] returns.")

(defvar faq-load-hooks nil
  "*A function or list of functions that are run when faq.el is loaded.
This is most useful for adding new entries to faq-default-faqs or
enabling \\[find-faq]'s interactive argument history mechanism, like
this:

\(setq faq-load-hooks 
      \(function \(lambda \(\)
		  \(setq faq-default-faqs
			\(cons '\(\"comp.lang.lisp\" .
				\"FAQ:_Lisp_Frequently_Asked_Questions_1_6_[Monthly_posting].Z\"\)
			      faq-default-faqs\)\)
		  \(require 'gmhist\)\)\)\)
")


;; Commands:

(defun find-faq (newsgroup faq)
  "*Find the archived Usenet NEWSGROUP FAQ file.  Completion is provided
for both the NEWSGROUP name and FAQ file name, and history is provided
for the NEWSGROUP name if the gmhist package is loaded \(see faq-load-
hooks\).
See faq-host, faq-dir, faq-user, faq-default-newsgroup, and
faq-default-faqs."
  (interactive
   (let* ((insert-default-directory	; simplify read-file-name prompt
					; (and read relative pathnames)
	   nil)
	  (default-directory
	    (if faq-host
		(progn
		  (require 'ange-ftp)	; remote filename syntax
		  (file-name-as-directory
		   (concat "/" faq-user "@" faq-host ":" faq-dir)))
	      (file-name-as-directory faq-dir)))
	  (default-newsgroup-dir
	    (file-name-as-directory (or faq-default-newsgroup "")))
	  (newsgroup-dir
	   (if (featurep 'gmhist)
	       (progn
		 (put 'find-faq-newsgroup-dir-history 'initial-hist
		      (list default-newsgroup-dir))
		 (read-file-name-with-history-in
		  'find-faq-newsgroup-dir-history
		  "Newsgroup (append trailing /): "
		  nil
		  (if (boundp 'find-faq-newsgroup-dir-history)
		      (car find-faq-newsgroup-dir-history)
		    default-newsgroup-dir)
		  t))
	     (read-file-name (format "Newsgroup (append trailing /): [%s] "
				     default-newsgroup-dir)
			     nil
			     default-newsgroup-dir
			     t)))
	  (default-faq-file			; newsgroup-specific
	    (or (cdr (assoc (directory-file-name newsgroup-dir)
			    faq-default-faqs))
		""))
	  (faq-file
	   (progn
	     (setq default-directory
		   (concat default-directory newsgroup-dir))
	     (read-file-name (format "FAQ: [%s] " default-faq-file)
			     nil
			     default-faq-file
			     t))))
     (list (directory-file-name newsgroup-dir)
	   faq-file)))
  (let ((newsgroup-faq
	 (concat (if faq-host
		     (concat "/" faq-user "@" faq-host ":" ))
		 (file-name-as-directory faq-dir)
		 (file-name-as-directory newsgroup)
		 faq)))
    (if (file-exists-p newsgroup-faq)
	(progn
	  (find-file newsgroup-faq)
	  (setq buffer-read-only t)
	  (run-hooks 'faq-hooks))
      (error "File %s does not exist." newsgroup-faq))))


;; Run faq-load-hook:
(run-hooks 'faq-load-hooks)
