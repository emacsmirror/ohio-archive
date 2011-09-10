;;; labbook.el --- provides a labbook log
;; $Revision: 1.2 $
;; $Date: 2000/04/17 16:11:19 $

;; This file is not part of Emacs

;; Author: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Keywords: logging labbook

;;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Status:
;;
;; This file is somewhat hacky and will probably remain so. I should
;; at the moment be considered beta release software until its got
;; a little more use. 

;;; Commentary:
;;
;; The package provides the ability to keep a labbook. It is based on
;; (and uses) the "ChangeLog" features of emacs, but differs in that
;; it keeps all of its log messages in one place. From a specifiable
;; root directory it creates a file per month in a directory per
;; year. It derives its format from the ChangeLog format except that
;; the heading is not necessarily the name of a file. 

;;; Bugs
;;
;; 1) The completing read buisness does not happen if current buffer is
;; not file associated which it a bit of a pain. There are a couple of
;; obvious hacks around this. (Change to a file name buffer, and have
;; completing read kill this as a suggestion!

;;; ToDo
;;
;; 1) Should define derived mode for these change logs, so that I can
;; use a slightly different keymap. Would be nice to have functions
;; for moving backwards for a period of time in menu items for
;; instance
;; 2) Linkage to calendar mode perhaps?

;;; History
;;
;; $Log: labbook.el,v $
;; Revision 1.2  2000/04/17 16:11:19  lord
;; Added more hooks. Changed standard file name.
;; Fixed bug with variable name (thanks to Rodney Stromlund for this).
;;
;; Revision 1.1  2000/04/11 19:23:56  phil
;; Initial checkin
;;
;;



(require 'add-log)

(if (not (boundp add-log-file-name-function))
    (setq add-log-file-name-function nil))


(define-key ctl-x-4-map "l" 'labbook-add-to-labbook)
(define-key ctl-x-4-map "v" 'labbook-view-labbook)
(define-key change-log-mode-map "\C-c\C-c" 'labbook-save-and-exit)

(defvar labbook-before-enter-hook nil 
  "Run before entering labbook.
This hook is run after the labbook buffer is available. The file name 
of this buffer is available in `labbook-file'. The buffer will not yet 
be in changelog mode")

(defvar labbook-after-enter-hook nil
  "Run after entering labbook.")

(defvar labbook-exit-hook nil
  "Run before labbook exits")

(defvar labbook-file nil
  "The last labbook file opened" )

(defvar labbook-base-labbook-directory "~/.labbook" 
  "Base directory for the labbook. This will be created if it does not
exist. This will be the root directory within which will be created
the directories named after the year")

(defvar labbook-completions
  '(("meeting")
    ("ideas")))

(defun labbook-view-labbook()
  (interactive)
  (let ((file (labbook-get-months-file)))
    (find-file-noselect file)
    (setq labbook-file file)
    (set-buffer (get-file-buffer file))
    (run-hooks 'labbook-before-enter-hook)
    (change-log-mode)
    (view-mode)
    (goto-char (point-min))
    (search-forward (funcall add-log-time-format) nil t)
    (switch-to-buffer (get-file-buffer file))
    (run-hooks 'labbook-after-enter-hook)))
   
(defun labbook-add-to-labbook()
  (interactive)
  (let ;; major hack alert. 
      ((labbook-standard-add-log-file-name-function add-log-file-name-function)
       (add-log-file-name-function 'labbook-fetch-entry-name)
       (labbook-standard-add-log-current-defun-function add-log-file-name-function)
       (add-log-keep-changes-together t)
       (add-log-current-defun-function 'labbook-fetch-function-name)
       (add-log-full-name " ")
       (add-log-mailing-address " "))
    (setq labbook-file (labbook-get-months-file))
    (run-hooks 'labbook-before-enter-hook)
    (add-change-log-entry nil labbook-file)
    (run-hooks 'labbook-after-enter-hook)))

(defun labbook-get-months-file()
  (let* ((time (decode-time (current-time)))
	 (year (nth 5 time))
	 (month (nth 4 time))
	 ;;we want the directory to get the directory name on the
	 ;;basis of the year
	 (directory (labbook-translate-to-directory year))
	 ;;and the file name of the basis of the month
	 (filename (labbook-translate-to-filename month))
	 ;;now resolve these names into fully qualified names.
	 (full-directory-name 
	  (concat (file-name-as-directory labbook-base-labbook-directory)
		  directory))
	 (full-file-name
	  (concat (file-name-as-directory full-directory-name)
		  filename)))
    ;;make sure the main directory exists
    (if (not (file-exists-p labbook-base-labbook-directory))
	(if (yes-or-no-p "The root directory of the labbook does not exist. Create it? ")
	    (make-directory labbook-base-labbook-directory)
	  (error "Cant not continue without root directory. See `labbook-base-labbook-directory'" )))
    ;;make sure its a directoy and not a file
    (if (not (file-directory-p labbook-base-labbook-directory))
	(error "The root directory of labbook already exists but is a file. See `labbook-base-labbook-directory'"))
    ;;make sure this years directory exists and that it is a directory
    (if (not (file-exists-p full-directory-name))
	(progn (make-directory full-directory-name)
	       (message 
		(concat "Creating a new directory for this years labbook. "
			"Happy new year, or welcome to labbook, or see `labbook-base-labbook-directory'"))
	       (sleep-for 2)))
    (if (not (file-directory-p labbook-base-labbook-directory))
	(error "The directory for this years labbook exists but is a file. Please remove this file to continue" ))
    ;;thats all the checking done. Now return the file name
    full-file-name))

(defun labbook-translate-to-directory (year)
  year)

(defun labbook-translate-to-filename (month)
  (concat month ".log"))

(defun labbook-fetch-function-name()
  "For the labbook we dont need a function name so kill it"
  nil)

(defun labbook-fetch-entry-name( filename )
  "Function prompts the event name to go into the log.
It suggests as a default the entry name calculated in the same way
that the normal changelog function does."
  ;;would be pretty nice to turn this into completing-read, taking its
  ;;completion list from a variable for standard entry names.
  (completing-read "Labbook entry name: "
		   ;;completions
		   labbook-completions
		   nil nil
		   ;;this is stolen straight from add-log.el with
		   ;;names the variable names changed. Give the
		   ;;standard guess as a basic option
		   (if labbook-standard-add-log-file-name-function
		       (funcall labbook-standard-add-log-file-name-function buffer-file-name)
		     (if (string-match
			  (concat "^" (regexp-quote (file-name-directory
						     filename)))
			  buffer-file-name)
			 (substring buffer-file-name (match-end 0))
			    (file-name-nondirectory buffer-file-name)))
		   ;;we dont need a keymap
		   nil
		   ;;not sure what this option does
		   nil
		   ;;this is the history list
		   'labook-entry-history))

(defun labbook-save-and-exit()
  "This function saves and er...exits."
  (interactive)
  (run-hooks 'labbook-exit-hook)
  (save-buffer nil)
  (kill-buffer (current-buffer)))

(provide 'labbook)

;;; labbook.el ends here















