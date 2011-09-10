;;; profile-support.el --- additional profile library support for
;;;                        GNU Emacs Lisp library test suites

;; Copyright (C) 1999
;;        Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 03 March 1999
;; Version: 1.00
;; Keywords: lisp, tools, profiles

;; This file is part of GNU Emacs.

;;; Commentary:
;; This file contains two new functions, profile-sort and
;; profile-write, to supplement those in the profile library. They are
;; needed by the test-*.el files that test library packages.
;;
;; It also contains a new version of profile-print which changes it to
;; not drop the fourth field when its value is zero.  Without that
;; change, profile-sort must use a UNIX-specific external sort
;; program.  With the change, it can use sort-numeric-fields, and thus,
;; run on all platforms to which Emacs has been ported, including
;; several non-UNIX ones.

;;; Code:

;; Force load of profile library first, so that we can replace its
;; profile-print with a new version below:
(require 'profile)


(provide 'profile-support)


(defconst profile-support-version "1.00"
  "Version number of profile-support library.")


(defconst profile-support-date "[03-Mar-1999]"
  "Revision date of profile-support library.")


(defun profile-sort ()
  "Sort the profile in buffer *profile* into order by function name,
followed by a second copy sorted by average time per call."
  (save-excursion
    (set-buffer "*profile*")
    (goto-line 3)
    (let* ((start (point))
	   (end (point-max))
	   (profile-title (buffer-substring (point-min) start))
	   (profile-lines (buffer-substring start end)))
      (sort-lines nil start end)
      (goto-char end)
      (insert "\n"
	      "Profile by decreasing average time\n"
	      profile-title)
      (setq start (point))
      (insert profile-lines)
      ;; Old UNIX-specific code before repair of profile-print (see below)
      ;; (if (string-lessp (substring emacs-version 0 2) "20") ; earlier than 20.x
      ;;    (shell-command-on-region start (point-max) "sort +3nr -4 +0 -1" t)
      ;;   (shell-command-on-region start (point-max) "sort +3nr -4 +0 -1" t t nil))
      ;; New system-independent code after repair of profile-print (see below)
      (sort-numeric-fields -1 start (point-max))
      ;; Alas, several Emacs sorting functions, including
      ;; sort-numeric-fields, lack a sort order argument, so we have
      ;; to reverse the lines to get the desired descending order,
      ;; sigh...
      (reverse-region start (point-max))))
  (goto-char (point-min))
  (set-buffer-modified-p nil))


(defun profile-write (filename)
  "Write the contents of the *profile* buffer to the file FILENAME."
  (set-buffer "*profile*")
  (write-file filename))


;; This function is a modification of that in emacs/20.3.6/lisp/emacs-lisp/profile.el
(defun profile-print (entry)
  "Print one ENTRY (from `profile-time-list')."
  (let* ((calls (car (cdr entry)))
	 (timec (cdr (cdr entry)))
	 (avgtime (and (not (zerop calls))
		       (/ (+ (car timec)
			     (/ (cdr timec) (float profile-million)))
			  calls))))
    (insert (format (concat "%-"
			    (int-to-string profile-max-fun-name)
			    "s %7d %10d.%06d")
		    (car entry) calls (car timec) (cdr timec))
	    ;; Change by: Nelson H. F. Beebe <beebe@math.utah.edu>
	    ;; Date: Wed Mar  3 10:03:47 1999
	    ;; DISABLE: Dropping this field prevents use of sort-numeric-fields!
	    ;;	    (if (null avgtime)
	    ;;		"\n"
	    ;;	      (format " %18.6f\n" avgtime))
	    ;; Here is the new code:
		    (format " %18.6f\n" (or avgtime 0.0)))))


(defun profile-system-id ()
  "Return a string with information about the current system, for use
as a benchmark record."
  (concat
   (format "Date:                 %s %s\n" (current-time-string)
	   (current-time-zone))
   (format "Emacs location:       %s%s\n" invocation-directory invocation-name)
   (format "Emacs version:        %s\n" emacs-version)
   (format "Hostname:             %s\n" (system-name))
   ;; Cannot use load-average, because it throws an error
   ;; "load-average not implemented for this operating system" on some
   ;; systems (e.g., SGI IRIX 6.4), sigh...
   ;; (format "Load average:         %s\n" (load-average t))
   (format "System configuration: %s\n" system-configuration)
   (format "System type:          %s\n" system-type)
   (format "User email address:   %s\n" user-mail-address)
   (format "User personal name:   %s\n" user-full-name)))
