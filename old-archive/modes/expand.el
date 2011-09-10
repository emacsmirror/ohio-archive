
;; expand.el --- minor mode to make abbreviations more usable.
;;
;; Copyright (C) 1995, Frederic Lepied <fred@sugix.frmug.fr.net>
;;
;; Author: Frederic Lepied <fred@sugix.frmug.fr.net>
;; Version: $Id: expand.el,v 1.3 1995/02/25 21:39:54 fred Exp $
;; Keywords: abbrev
;;
;; LCD Archive Entry:
;; expand|Frederic Lepied|fred@sugix.frmug.fr.net|
;; A minor mode to make abbreviations more usable.|
;; 25-Feb-1995|1.3|~/modes/expand.el.gz|
;; 
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; Purpose of this package:
;;   1. Make abbreviations more usable.
;;   2. Expand abbreviations only when they are at the end of a line.
;;   3. Position the cursor after expansion to a place specified by advance.
;;   4. Indent the expanded region.
;;   5. If a list of points as been provided with the abbreviation definition,
;;   the functions expand-jump-to-previous-mark and expand-jump-to-next-mark
;;   moved from mark to mark.
;;
;; Installation:
;;   * store this file somewhere in your load-path and byte compile it.
;;   * put (require 'expand) in your .emacs or in site-start.el or generate
;; autoloads.
;;   * and according to the mode install your expansion table.
;;
;;   For example for c-mode, you could declare your abbrev table with :
;;
;; (defconst c-expand-list
;;   '(("if" "if () {\n \n} else {\n \n}" (5 10 21))
;;     ("uns" "unsigned ")
;;     ("for" "for(;;) {\n\n}" (5 6 7 11))
;;     ("switch" "switch () {\n\n}" (9 13))
;;     ("case" "case :\n\nbreak;\n" (6 8 16))
;;     ("do" "do {\n\n} while ();" (6 16))
;;     ("while" "while () {\n\n}" (8 12))
;;     ("default" "default:\n\nbreak;" 10)
;;     ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
;;   "Expansions for C mode")
;; 
;; (expand-add-abbrevs c-mode-abbrev-table c-expand-list)
;; 
;;   and enter expand-mode with the following hook :
;;
;; (add-hook 'c-mode-hook (function (lambda() (expand-mode))))
;;
;;   you can also bind jump functions to some keys :
;;
;; (define-key expand-map '[C-tab] 'expand-jump-to-next-mark)
;; (define-key expand-map '[C-S-tab] 'expand-jump-to-previous-mark)
;;
;; Commentaries:
;;   Has been tested under emacs 19.28 and XEmacs 19.11.
;;   Many thanks to Heddy Boubaker <boubaker@cenatls.cena.dgac.fr>.
;;   Please send me a word to give me your feeling about this mode or
;; to explain me how you use it (your expansions table for example).

;; expand is not a replacement for abbrev it is just a layer above it.
;; You could always declare your abbreviations with define-abbrev to have
;; the abbrev behavior with expand-mode.

(defvar expand-mode nil
  "Status variable for expand-mode")
(make-variable-buffer-local 'expand-mode)

;;;###autoload
(defun expand-mode(&optional arg)
  "Toggle expand mode.
With argument ARG, turn expand mode on if ARG is positive.
In expand mode, inserting an abbreviation at the end of a line
causes it to expand and be replaced by its expansion."
  (interactive "P")
  (setq expand-mode (if (null arg) (not expand-mode)
		       (> (prefix-numeric-value arg) 0)))
  (if expand-mode
      (setq abbrev-mode nil)))

;;;###autoload
(defvar expand-map (make-sparse-keymap)
  "key map used in expand-mode.")
(define-key expand-map " " 'expand)

(or (assq 'expand-mode minor-mode-alist)
    (setq minor-mode-alist (cons (list 'expand-mode " Expand")
				 minor-mode-alist)))

(or (assq 'expand-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'expand-mode expand-map)
				     minor-mode-map-alist)))
 
;;;###autoload
(defun expand-add-abbrevs (table abbrevs)
  "Add a list of abbrev to the table.
Each abbrev description entry has the following format :
	(abbrev expansion arg)
where
      abbrev is the abbreviation to replace.
      expansion is the replacement string or a function which will make
the expansion. For example you could use the DMacros package to generate such functions.
      arg is an optional  element which can be a  number or a  list of
numbers. If arg is a  number, the cursor will be  placed at arg  chars
from  the beginning of  the expanded text.   If expansion is a list of
numbers the cursor will be placed according to the first number of the
list from the beginning of the expanded text and  marks will be placed
and you  will  be able to  visit  them  cyclicaly  with the  functions
expand-jump-to-previous-mark  and expand-jump-to-next-mark. If arg  is
omitted, the cursor will be placed at the end of the expanded text."
  (if (null abbrevs)
      table
    (expand-add-abbrev table (nth 0 (car abbrevs)) (nth 1 (car abbrevs))
		       (nth 2 (car abbrevs)))
    (expand-add-abbrevs table (cdr abbrevs))))

(defvar expand-list nil "Temporary variable used by expand-mode.")

(defvar expand-pos nil
  "If non nil, stores a vector containing markers to positions defined by the last expansion.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-pos)

(defvar expand-index 0
  "Index of the last marker used in expand-pos.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-index)

(defvar expand-point nil
  "Beginning of the expanded region.
This variable is local to a buffer.")
(make-variable-buffer-local 'expand-point)

(defun expand-add-abbrev (table abbrev expansion arg)
  "Add one abbreviation and provide the hook to move to the specified
position"
  (let* ((string-exp (if (and (symbolp expansion) (fboundp expansion))
			 nil
		       expansion))
         (position   (if (and arg string-exp)
			 (if (listp arg)
			     (- (length expansion) (1- (car arg)))
			   (- (length expansion) (1- arg)))
		       0)))
    (define-abbrev
      table
      abbrev
      (or string-exp "")
      (if string-exp
	  (if (and (listp arg)
		   (not (null arg)))
	      (` (lambda()
		   (expand-build-list (, (length string-exp)) '(, arg))
		   (setq expand-point (point))
		   (backward-char (, position))))

	    (` (lambda()
		 (setq expand-point (point))
		 (backward-char (, position)))))
        expansion))))

;;;###autoload
(defun expand()
  "Do the expansion job if we are at the end of a line or insert space"
  (interactive)
  (or (if (or (eolp)
	      (abbrev-not-expand (previous-word)))
	  (let ((p (point)))
	    (when (expand-abbrev)
	      (if (not expand-point)
		  t
		(if (vectorp expand-list)
		    (expand-build-marks expand-point))
		 (indent-region p expand-point nil)
		 t
		 ))))
      (insert " "))
  (setq expand-point nil))

(defun abbrev-not-expand(word)
  "Test if an abbrev as a hook"
  (or
   (and (intern-soft word local-abbrev-table)
	(not (symbol-function (intern-soft word local-abbrev-table))))
   (and (intern-soft word local-abbrev-table)
	(not (symbol-function (intern-soft word local-abbrev-table))))))

(defun previous-word ()
  "Return the previous word"
  (save-excursion
    (let ((p (point)))
      (backward-word 1)
      (buffer-substring p (point)))))

(defun expand-jump-to-previous-mark()
  "Move the cursor to previous mark created by the expansion"
  (interactive)
  (when expand-pos
    (setq expand-index (1- expand-index))
    (if (< expand-index 0)
	(setq expand-index (1- (length expand-pos))))
    (goto-char (aref expand-pos expand-index))))

(defun expand-jump-to-next-mark()
  "Move the cursor to next mark created by the expansion"
  (interactive)
  (when expand-pos
    (setq expand-index (1+ expand-index))
    (if (>= expand-index (length expand-pos))
	(setq expand-index 0))
    (goto-char (aref expand-pos expand-index))))

(defun expand-build-list (len l)
  "Build a vector of offset positions from the list of positions" 
  (expand-clear-markers)
  (setq expand-list (vconcat l))
  (let ((i 0))
    (while (< i (length expand-list))
      (aset expand-list i (- len (1- (aref expand-list i))))
      (setq i (1+ i))))
  )

(defun expand-build-marks (p)
  "Transform the offsets vector into a marker vector" 
  (when expand-list
    (setq expand-index 0)
    (setq expand-pos (make-vector (length expand-list) nil))
    (let ((i (1- (length expand-list))))
      (while (>= i 0)
	(aset expand-pos i (copy-marker (- p (aref expand-list i))))
	(setq i (1- i))))
    (setq expand-list nil)))

(defun expand-clear-markers ()
  "Make the markers point nowhere"
  (when expand-pos
    (let ((i (1- (length expand-pos))))
      (while (>= i 0)
	(set-marker (aref expand-pos i) nil)
	(setq i (1- i))))
    (setq expand-pos nil)))

(provide 'expand)

;; end of expand.el

