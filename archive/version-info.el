;;; version-info.el --- Some utilities for operating on lisp file versions.

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Keywords: lisp, extensions
;; Version: 0.8  ;)
;; Homepage: http://www.gnusoftware.com/Emacs/Lisp/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  This is the first cut at a package to allow lisp coders to
;; query other packages for version information.
;;
;;  In general testing for features by comparing version information
;; is probably a bad idea.  However, there are several cases when
;; having the ability to find version information might be a good
;; thing.

;;; Justification:

;;  Some time ago there was some discussion about the idea of
;; creating, and maintaining, a "central" repository for the storage
;; of Emacs Lisp code.
;;
;;  There are several existing repositorys presently in operation,
;; the proposal was more grandiose than just a repository though;
;; the intention was to create some form of packaging system which
;; would allow users to easily install + upgrade Lisp with the
;; minimum of effort.

;;  I think that this is a good idea, and have spent a little time
;; working on some things that would be necessary for such an endeaver.
;;
;;  So far I've created a nice tree control which will be used for
;; browsing hierarchies of packages, and this file which provides the
;; needed version information.

;;; ToDo:

;;  For the central repository to work there needs to be some system
;; for building indexes, and some form of "package", whether it's a
;; proper packing system, or just zip file is something thats open
;; to debate.

;;  My personal inclination is to have some collections of .zip files,
;; and a Debian Apt-like system for indexing the files.

;;

;;; History:
;; 

;;  Initial release.

;;; Code:

(defvar version-directory-sep-char (if (eq system-type 'windows-nt) "\\" "/")
  "The character to use for seperating directorys.")

(defvar version-tags nil
  "A list of regexps that define version numbers.
This list is in the form of a list of cons cells,
the first part is a regular expression that is searched
for, and the second part is the number which should
be used by `string-match' to find the version number.

For example:
 (\"-version[ 	]*\"*\\([\\.0123456789]+\\)\"\" . 1)")

(setq version-tags nil)

;; Setup the version tags..
;;  If anybody finds a usefull regexp I'd be keen to include
;; it here .. mail me.
(if version-tags
    nil
  (add-to-list 'version-tags
	       (cons
		"\\(.*\\)[vV][eE][rR][sS][iI][oO][nN][ \t]*:[ \t]*[\\.,a-zA-Z \t]*\\([\\.0123456789]+\\)\\(.*\\)"
		2))
  (add-to-list 'version-tags
	       (cons
		"\\$*[Ii][Dd]:[ \t]*\\(.*\\),v[ \t]*\\([\\.0123456789]+\\)"
		2))
  (add-to-list 'version-tags
	       (cons
		"-version[ \t]*\"*\\([\\.0123456789]+\\)\""
		1))
  )


(defun version-find-lisp-file( FILE )
  "Find a lisp file on the load path.
FILE is assumed to contain the suffix .el"
  (interactive "sFind the location of file : ")
  (let ((paths load-path)
	(found nil))
    (if (file-exists-p FILE)
	(setq found FILE))
    (while (and paths (not found))
      (if (file-exists-p (concat (car paths) version-directory-sep-char FILE))
	  (setq found (concat (car paths) version-directory-sep-char FILE)))
      (setq paths (cdr paths)))
    found))


(defun version-find-lisp-version( FILE )
  "Find the version number of a lisp file.
FILE may be a complete path to a file, or a file that
is contained on the current `load-path'"
  (interactive "sFind the version of file : ")
  (let ((version nil)
	(string-regexp nil)
	(string-match nil)
	(tags  nil)
	(file  FILE) ;; Temp copy, for error msg.
	)

    (setq FILE (version-find-lisp-file FILE))
    (if (not FILE)
	(error "Cannot find file %s" file))

    (setq tags version-tags)
    (with-temp-buffer
      (insert-file FILE)

      ;; Go through all the version tag regexps. untill
      ;; a match is found, or we've run out of regexps.
      (while (and (not version)
		  tags)
	(setq string-regexp (car (car tags)))
	(setq string-match  (cdr (car tags)))
	(goto-char (point-min))
	(if (re-search-forward string-regexp (point-max) t)
	    (setq version (match-string-no-properties string-match)))
	(setq tags (cdr tags))
	)
      )
    version
    )
  )


(defun version-number-lessp ( FIRST SECOND )
  "Compare two version strings.
Returns t if FIRST is less than SECOND, otherwise
it returns nil"
  (if (or (not FIRST)
	  (not SECOND))
      nil
    (progn
      ;; Non nil version numbers
      (let ((f nil)
	    (s nil))
	(setq f (version-string-to-int FIRST))
	(setq s (version-string-to-int SECOND))
	(< f s)
))))


(defun version-number-equalp ( FIRST SECOND )
  "Compare two version strings.
Returns t if FIRST equal to SECOND, otherwise
it returns nil"
  (if (or (not FIRST)
	  (not SECOND))
      nil
    (progn
      ;; Non nil version numbers
      (let ((f nil)
	    (s nil))
	(setq f (version-string-to-int FIRST))
	(setq s (version-string-to-int SECOND))
	(= f s)
))))

(defun version-string-to-int (STRING)
  "Convert a version number, of the form a.b.c.d.. to an integer.
Argument STRING is the version number, as a string."
  (let ((string   "")	;Copy string.
	(len (length STRING))
string-to-number	(idx 0)
	(found nil)
	)
    (while (< idx len)
      (if (and (= (aref STRING idx) 46)
	       (not found))
	  (progn
	    (setq found t)
	    (setq string (concat string  "."))
	    ))
      (if (not (= (aref STRING idx) 46))
	       (setq string (concat string  (substring STRING idx (+ idx 1)))))
      (setq idx (+ 1 idx)))
    (string-to-number string)
    )
  )

(defun version-display-directory ( DIR )
  "Display a list of all Lisp files in a directory, with version info.
This is a utility function whos main purpose is to test the `version'
finding/parsing code.
DIR should be the path to a directory containing .el files, which are
scanned for version information."
  (interactive "DDirectory: ")
  (let ((file nil)
	(new (get-buffer-create "*Version-Info*"))
	)
    (setq dirs (directory-files DIR))
    (set-buffer new)
    (while dirs
      (setq file (car dirs))
      (setq dirs (cdr dirs))
      (if (eq (string-match "\\." file) 0)
	  nil
	(if (and (not (file-directory-p file))
		 (string-match "el$" file))
	    (insert (concat "\nFile :\t" file " Version:\t" (version-find-lisp-version file))))
	)
      )
    (pop-to-buffer new)))

(provide 'version-info)

;;; version-info.el ends here

