;;; $Header: /home/user3/miles/src/elisp/RCS/strhash.el,v 1.5 1992/04/16 13:44:21 miles Exp $
;;; ----------------------------------------------------------------
;;; strhash.el -- String keyed hash-tables
;;; Copyright (C) April 1992, Miles Bader <miles@cogsci.ed.ac.uk>
;;; ----------------------------------------------------------------
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------
;;;
;;; A set of wrapper macros for intern & intern-soft to implement
;;; string-keyed hash-tables.
;;;

(provide 'strhash)

(require 'backquote)
(require 'setf)

(defconst strhash-default-size 91)

(defmacro make-strhash-table (&optional size)
  (` (make-vector (, (or size string-hash-default-size)) 0)))

(defmacro strhash-get (table string)
  (` (symbol-value (intern-soft (, string) (, table)))))

(defmacro strhash-has-entry-p (table string)
  (` (intern-soft (, string) (, table))))

(defsetf strhash-get (table string) (val)
  (` (set (intern (, string) (, table)) (, val))))

(defmacro strhash-map (fun table) 
  (` (mapatoms (function (lambda (sym)
			   (funcall (, fun)
				    (symbol-name sym)
				    (symbol-value sym))))
	       (, table))))
