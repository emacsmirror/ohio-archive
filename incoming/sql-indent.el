;;; sql-indent.el --- indentation of SQL statements

;; Copyright (C) 2000  Alex Schroeder

;; Emacs Lisp Archive Entry
;; Filename: sql-indent.el
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: languages
;; Description: indentation of SQL statements
;; URL: http://www.geocities.com/kensanata/emacs.html

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Indent SQL statements.

;; As the indentation of SQL statements depends not only on the previous
;; line but also on the current line, empty lines cannot always be
;; indented correctly.  Hitting TAB on a line containing only whitespace
;; will remove all whitespace and make the line empty; hitting TAB on an
;; empty line will indent the line to the same column as the previous
;; line; hitting TAB again will indent or outdent the line one step.
;; Hitting TAB after certain keywords will insert a tab character as well
;; as indent the line.

;; Usage note: Loading this file will make all SQL mode buffers created
;; from then on use `sql-indent' for indentation.  A possible way to
;; install sql-indent.el would be to add the following to your .emacs:

;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;;; Code:

(require 'sql)

;; Need the following to allow GNU Emacs 19 to compile the file.
(require 'regexp-opt)

(defcustom sql-indent-first-column-regexp
  (concat "^\\s-*" (regexp-opt '(
"select" "update" "insert" "delete" "set" "from" "where" "and" "or"
"into" "--") t) "\\(\\b\\|\\s-\\)")
  "Regexp matching keywords relevant for indentation.
The regexp matches lines which start SQL statements and it matches lines
that should be indented at the same column as the start of the SQL
statement.  The regexp is created at compile-time.  Take a look at the
source before changing it.  All lines not matching this regexp will be
indented by `sql-indent-offset'."
  :type 'regexp
  :group 'SQL)

(defcustom sql-indent-offset 8
  "*Offset for SQL indentation."
  :type 'number
  :group 'SQL)

(defcustom sql-indent-maybe-tab t
  "If non-nil, `sql-indent' will insert a tab after some keywords.
In particular, if the current line before point matches 
`sql-indent-first-column-regexp', then a tab will be inserted."
  :type 'boolean
  :group 'SQL)

(defvar sql-indent-debug nil
  "If non-nil, `sql-indent' will output debugging messages.")

(defun sql-indent ()
  "Indent current line in a SQL statement."
  (interactive)
  (let ((fall-back (save-excursion
		     (beginning-of-line)
		     (skip-syntax-forward " ")
		     (current-column))))
    (save-excursion
      (indent-line-to
       (save-excursion
	 (let* (;; remember current column
		(now (current-column))
		;; should current line be at first column
		(curr (progn (beginning-of-line)
			     (looking-at sql-indent-first-column-regexp)))
		;; if we are at a line with only whitespace
		(space (looking-at "^\\s-+$"))
		;; if we are at an empty line
		(empty (looking-at "^$"))
		;; is previous non-empty line at first column
		(prev (progn (beginning-of-line 0)
			     (while (and (not (bobp))
					 (looking-at "^\\s-*$"))
			       (forward-line -1))
			     (looking-at sql-indent-first-column-regexp)))
		;; column of previous non-empty line start
		(col (progn (skip-syntax-forward " ")
			    (current-column)))
		;; is previous non-empty opens or closes a recursive thing
		(paren (let ((start (point))
			     (end (progn (end-of-line) (point))))
			 (nth 0 (parse-partial-sexp start end))))
		first)
	   ;; if we are the very first statement in the buffer, then this
	   ;; line should not be unindented
	   (if (and curr (not prev)
		    (not (search-backward-regexp 
			  sql-indent-first-column-regexp nil t)))
	       (setq first t))
	   ;; debug
	   (if sql-indent-debug
	       (message "curr %S, prev %S, first %S, space %S, empty %S, paren %d, col %d, now %d" 
			curr prev first space empty paren col now))
	   ;; action
	   (cond (first fall-back)
		 (empty col)
		 ((and space (not (= now col))) 0)
		 (space
		  (if prev 
		      (+ col sql-indent-offset)
		    (- col sql-indent-offset)))
		 ((and prev (or (> paren 0) (not curr)))
		  (+ col sql-indent-offset))
		 ((and curr (or (not prev) (< paren 0)))
		  (- col sql-indent-offset))
		 (t col))))))
    ;; maybe insert a tab at the end of the line
    (if (and sql-indent-maybe-tab
	     (save-excursion
	       (beginning-of-line)
	       (looking-at (concat sql-indent-first-column-regexp 
				   "\\s-*$"))))
	(progn
	  (end-of-line)
	  (delete-backward-char (skip-syntax-backward " "))
	  (tab-to-tab-stop))
      ;; skip whitespace anyway
      (skip-syntax-forward " "))))

(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (setq indent-line-function 'sql-indent))))

(provide 'sql-indent)
