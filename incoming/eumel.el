;;; eumel.el --- an alternative for `insert-file'.

;; Copyright (C) 1994 Ralph Schleicher

;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
;; Keywords: local mail news

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:


(defvar eumel-head
  (concat
   "                               ,,,\n"
   "                              (o o)\n"
   "---------------------------oOO-(_)-OOo---------------------------\n")
  "Piece of text inserted at the beginning of a verbatim file listing.")

(defvar eumel-tail
  "---------------------------oOo-----oOo---------------------------\n"
  "Piece of text inserted at the end of a verbatim file listing.")

(defun eumel (file-name)
  "Insert contents of file FILE-NAME into buffer after point.
The value of the variables `eumel-head' and `eumel-tail' will be inserted
at the top respective bottom of the file contents."
  (interactive "*FEumel on file: ")
  (insert eumel-head)
  (condition-case nil
      (if (file-exists-p file-name)
	  (if (file-readable-p file-name)
	      (if (not (file-directory-p file-name))
		  (forward-char (car (cdr (insert-file-contents file-name))))
		(error "Eumel:%s: Cannot insert a directory" file-name))
	    (error "Eumel:%s: Permission denied" file-name))
	(error "Eumel:%s: No such file or directory" file-name))
    (error nil))
  (insert eumel-tail))

(define-key global-map [?\C-x ?\M-i] 'eumel)


(provide 'eumel)

;;; eumel.el ends here
