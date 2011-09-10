;;; debian-bug.el --- report a bug to Debian's bug tracking system

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.

;; Author: Francesco PotortÅÏ <pot@gnu.org>
;; Keywords: debian, bug, reporter
;; Version: $Id: debian-bug.el 1.5 1999/09/23 16:09:31 pot Exp pot $

;; debian-bug.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; debian-bug.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;

;;; Todo:

;; - -f does not work (at least with bug)
;; - apply contributed patch
;; - error conditions: nonexistent package
;; - add doc strings and commentary
;; - Help on menu of pseudo packages and severity levels (how???)
;; - we assume sed, uname, date are there; is that ok?

;;; Code:

(defvar debian-bug-mail-address
  "Debian GNU/Linux bug list <submit@bugs.debian.org>")

(defvar debian-bug-program nil)

(defvar debian-bug-status-file "/var/lib/dpkg/status")

(defvar debian-bug-severity-alist
  '(("critical") ("grave") ("important") ("normal") ("wishlist")))

(defvar debian-bug-pseudo-packages
  '("base" "bootdisk" "rootdisk" "bugs.debian.org" "ftp.debian.org"
    "nonus.debian.org" "www.debian.org" "manual" "project" "general"
    "kernel" "lists.debian.org"))

(defvar debian-bug-packages-obarray nil)
(defvar debian-bug-packages-date nil)

(defalias 'report-debian-bug 'debian-bug)

;;; Functions:

(defun debian-bug-intern (name)
  (intern name debian-bug-packages-obarray))

(defun debian-bug-fill-packages-obarray ()
  ""
  (if (and (vectorp debian-bug-packages-obarray)
	   (equal debian-bug-packages-date
		  (nth 5 (file-attributes debian-bug-status-file))))
      nil
    (let (len ok
	  (packages (length debian-bug-pseudo-packages)))
      (message "Building list of installed packages...")
      (with-temp-buffer
	(setq ok
	      (zerop
	       (call-process "sed" debian-bug-status-file t nil
			     "-n"
			     "/^Package: / { s///\nh\n}\n/^Version:/ { g\np\n}")))
	(if ok (setq packages (+ packages
				 (count-lines (point-min) (point-max)))))
	(setq len (1- (ash 4 (logb packages))))
	(setq debian-bug-packages-obarray (make-vector len 0))
	(mapcar 'debian-bug-intern debian-bug-pseudo-packages)
	(if ok (mapcar 'debian-bug-intern (split-string (buffer-string))))
	(setq debian-bug-packages-date
	      (nth 5 (file-attributes debian-bug-status-file)))
	)))
  (vectorp debian-bug-packages-obarray))

(defun debian-bug-init-program ()
  (or debian-bug-program
      (setq debian-bug-program
	    (cond
	     ((zerop (call-process "bug" nil nil nil "--help"))
	      "bug")
	     ((zerop (call-process "reportbug" nil nil nil "--help"))
	      "reportbug")
	     (t
	      "sed")
	     ))))

(defun debian-bug-report-string (package severity)
  (with-temp-buffer
    (cond

     ;; reportbug
     ((string= debian-bug-program "reportbug")
      (call-process debian-bug-program nil '(t t) nil
		    "-p" "-s" "" "-S" severity package)
      (goto-char (point-min))
      (delete-region (point) (progn (forward-line 5) (point))))

     ;; bug
     ((string= debian-bug-program "bug")
      (call-process debian-bug-program nil '(t t) nil
		    "-p" "-s" "" "-S" severity package))

     ;; neither reportbug nor bug
     ((string= debian-bug-program "sed")
      (insert "Package: " package
	      "\nVersion: ")
      (call-process "sed" debian-bug-status-file '(t t) nil
		    "-n" (concat "/^Package: " package
				 "$/,/^$/s/^Version: //p"))
      (or (bolp)
	  (call-process "date" nil '(t t) nil "+N/A; reported %Y-%m-%d"))
      (insert "Severity: " severity
	      "\n\n\n\n-- System Information"
	      "\nDebian Release: ")
      (insert-file-contents-literally "/etc/debian_version")
      (goto-char (point-max))
      (insert "Kernel Version: ")
      (call-process "uname" nil '(t t) nil "-a"))
     )
    (buffer-string)
    ))

(defun debian-bug ()
  "Submit via mail a bug report do Debian"
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	package severity prefill)
    (if (and
	 (if (y-or-n-p
	      "Do you want to submit a bug report to the Debian maintainers? ")
	     t (message "") nil)
	 (require 'reporter)
	 (debian-bug-fill-packages-obarray)
	 (debian-bug-init-program)
	 (setq package (completing-read
			"Package name (or file name preceded by -f): "
			debian-bug-packages-obarray
			nil nil nil nil (current-word)))
	 (setq severity (completing-read
			 "Severity (default normal): "
			 debian-bug-severity-alist
			 nil t nil nil "normal"))
	 (setq prefill (debian-bug-report-string package severity))
	 (reporter-submit-bug-report debian-bug-mail-address
				     package
				     nil nil nil nil))
	(progn
	  (delete-region (point) (point-max))
	  (save-excursion (insert prefill))
	  (forward-line 4)
	  (set-window-start (selected-window) (point-min) t)
	  ))))
