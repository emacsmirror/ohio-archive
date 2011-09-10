;;; etalk game directory search routines
;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Purpose:
;;;   This file contains the stuff needed to look at the load path
;;; and find all "games" that might be used within tyrant-mode

(defun etalk-search-load-path-for-games (aiflag)
  "Search each element of load-path for a games directory, and append
all files therin into tuples of symbols."
  (let ((paths load-path)		;temp path list
	(syms nil))			;new list of symbols
    (while paths
      (if (file-exists-p (car paths))
	  (if (directory-files (car paths) nil "^games$")
	      (setq syms (append syms (etalk-read-symbols 
				       (car paths) aiflag)))))
      (setq paths (cdr paths)))
    syms))

(defun etalk-read-symbols (path aiflag)
  "Read in a directory of games from the .el files to the game tuples"
  (let ((syms nil)
	(dlist (directory-files 
		(concat path 
			(if (/= (aref path (1- (length path))) ?/) "/")
			"games")
		nil ".el$"))
	(omd (match-data)))
    (while dlist
      (if (string-match "-ai" (car dlist))
	  (if (and aiflag (string-match "\\(-ai\\).el" (car dlist)))
	      (let* ((newsym (substring (car dlist) 0 (match-beginning 1)))
		     (newaisym (substring (car dlist) 0 (match-end 1)))
		     (newid (read newsym))
		     (newai (read newaisym)))
		(setq syms (cons (list (etalk-casafy-game-string newsym)
				       newid newai)
				 syms))))
	(if (and (not aiflag) 
		 (not (string-match "-lib.el" (car dlist)))
		 (string-match "\\(.el\\)" (car dlist)))
	    (let* ((newsym (substring (car dlist) 0 (match-beginning 1)))
		   (newid (read newsym)))
	      (setq syms (cons (cons (etalk-casafy-game-string newsym)
				     newid)
			       syms)))))
      (setq dlist (cdr dlist)))
    (store-match-data omd)
    syms))

(defun etalk-casafy-game-string (str)
  "Take a string from a file name, like word-thing, and turn it into
something nice and readable like Word Thing"
  (let ((l (length str))
	(tmp 0))
    (while (< tmp l)
      (if (= (aref str tmp) ?-)
	  (aset str tmp ? ))
      (setq tmp (1+ tmp))))
  (capitalize str))

(defvar etalk-legal-multiuser-functions nil
  "contains an association list of strings which are allowable for 2
or more person talk utilities." )

(defvar etalk-legal-tyrant-ai-functions nil
  "contains an association list of strings which are allowable for 2
person games which have AI programs associated with them")

(setq etalk-legal-multiuser-functions (etalk-search-load-path-for-games nil))
(setq etalk-legal-tyrant-ai-functions (etalk-search-load-path-for-games t))

;;; end of lisp
(provide 'etalk-lgame)
