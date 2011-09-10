;; symlink-fix: Fix to remove symbolic links from file pathnames.
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; jbw%bucsf.bu.edu@bu-it.bu.edu (school year)
;; joew@uswest.com (summer)

(or (fboundp 'original-expand-file-name)
    (fset 'original-expand-file-name
	  (symbol-function 'expand-file-name)))

(defun expand-file-name (file-name &optional directory)
  "Convert FILENAME to absolute, and canonicalize it.
Second arg DEFAULT is directory to start with if FILENAME is relative
:(does not start with slash); if DEFAULT is nil or missing,
the current buffer's value of default-directory is used.
Filenames containing . or .. as components are simplified;
initial ~ is expanded.  See also the function  substitute-in-file-name.
This has been modified to resolve all symbolic links from FILENAME.
The original function definition is stored on original-expand-file-name."
  (let (left right split link)
    (setq right (original-expand-file-name file-name directory))
    (setq left "")
    (while (not (equal right ""))
      (setq split (split-file-name right))
      (setq left (concat left (car split)))
      (setq right (cdr split))
      (setq link (file-symlink-p left))
      (if (null link)
	  nil
	(if (eq 0 (length link)) (setq link "."))
	(cond ((not (eq (aref link 0) ?/))
	       (setq split (split-file-name link))
	       (setq left (hack-local-link left (car split)))
	       (setq right (concat (cdr split) right)))
	      (t
	       (setq right (concat link right))
	       (setq left "")))))
    (original-expand-file-name left)))

(defun split-file-name (file-name)
  "Splits FILENAME into two strings, and returns a list of the two
strings.  The first string will be the first filename compenent in
FILENAME, plus any leading /s, and the second string will be the rest
of FILENAME, possibly a string of length 0."
  (if (string-match "^\\(/*[^/]+\\)\\(/.+\\)$" file-name)
      (cons (substring file-name (match-beginning 1) (match-end 1))
	    (substring file-name (match-beginning 2) (match-end 2)))
    (cons file-name "")))

(defun hack-local-link (file-name link)
  "Takes FILENAME and LINK and returns a string which substitutes LINK
for the last component of FILENAME."
  (string-match "^\\(.+/\\)[^/]+$" file-name)
  (concat (substring file-name (match-beginning 1) (match-end 1))
	  link))

(provide 'symlink-fix)
