;; thingatpt+.el --- Extensions to `thingatpt.el'.
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Tue Feb 13 16:47:45 1996
;; Version: $Id: thingatpt+.el,v 1.3 2001/01/03 17:48:20 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:48:17 2001
;;   Update count: 350
;; Keywords: extensions, matching, mouse, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Extensions to `thingatpt.el'.
;;
;;  New functions defined here:
;;
;;    `bounds-of-form-at-point', `bounds-of-form-nearest-point',
;;    `bounds-of-symbol-at-point', `bounds-of-symbol-nearest-point',
;;    `bounds-of-thing-nearest-point', `form-at-point-with-bounds',
;;    `form-nearest-point', `form-nearest-point-with-bounds',
;;    `forward-char-same-line', `list-nearest-point',
;;    `number-nearest-point', `sentence-nearest-point',
;;    `sexp-nearest-point', `symbol-at-point-with-bounds',
;;    `symbol-name-nearest-point', `symbol-nearest-point',
;;    `symbol-nearest-point-with-bounds',
;;    `thing-at-point-with-bounds', `thing-nearest-point',
;;    `thing-nearest-point-with-bounds', `word-nearest-point'.
;;
;;
;;  ***** NOTE: The following functions defined in `thingatpt.el'
;;              have been REDEFINED HERE:
;;
;;  `bounds-of-thing-at-point' - Added optional argument SYNTAX-TABLE.
;;  `form-at-point' - Added optional argument SYNTAX-TABLE.
;;  `symbol-at-point' - 
;;     Original definition:
;;          (defun symbol-at-point () (form-at-point 'sexp 'symbolp))
;;     With point on toto in "`toto'" (in Emacs Lisp mode), the
;;     original definition returned `toto, not toto.  With point on
;;     toto in "`toto'," (note comma), that definition returned nil.
;;     The definition given here returns toto in both of these cases.
;;  `thing-at-point' - Added optional argument SYNTAX-TABLE.
;;
;;
;;  A reminder (the doc strings are not so good):
;;
;;    These functions, defined in `thingatpt.el', all move point:
;;      `beginning-of-thing', `end-of-sexp', `end-of-thing',
;;      `forward-symbol', `forward-thing', `forward-whitespace'
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `thingatpt.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "thingatpt" '(require 'thingatpt+))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: thingatpt+.el,v $
;; RCS Revision 1.3  2001/01/03 17:48:20  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.2  2001/01/03 17:02:10  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/09/14 17:24:26  dadams
;; RCS Initial revision
;; RCS
; Revision 1.1  1997/03/20  08:14:56  dadams
; Initial revision
;
; Revision 1.9  1996/07/01  13:27:18  dadams
; (trivial)
;
; Revision 1.8  1996/06/14  14:37:06  dadams
; Cosmetics.
;
; Revision 1.7  1996/06/11  09:01:33  dadams
; bounds-of-symbol-at-point, bounds-of-symbol-nearest-point, symbol-at-point,
; symbol-at-point-with-bounds, symbol-name-nearest-point,
; symbol-nearest-point, symbol-nearest-point-with-bounds:
;   No longer use a syntax-table arg.  Always dealing with elisp symbols, so
;   use emacs-lisp-mode-syntax-table.
;
; Revision 1.6  1996/06/04  11:04:37  dadams
; Removed old-form-at-point (not used).
;
; Revision 1.5  1996/04/05  14:37:59  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.4  1996/03/20  10:08:33  dadams
; 1. Added redefinitions of thing-at-point, form-at-point, with optional
;    syntax table arg.
; 2. Added: thing-nearest-point-with-bounds, bounds-of-thing-nearest-point,
;           thing-nearest-point, form-nearest-point-with-bounds,
;           bounds-of-form-nearest-point, form-nearest-point,
;           word-nearest-point, sentence-nearest-point, sexp-nearest-point,
;           number-nearest-point, list-nearest-point.
; 3. symbol-at-point: Added optional syntax table arg.
; 4. symbol-nearest-point-with-bounds: Now defined in terms of
;    form-nearest-point-with-bounds.
; 5. bounds-of-form-at-point: Added args THING and PRED.
;
; Revision 1.3  1996/03/20  07:34:20  dadams
; 1. Require cl.el.
; 2. Added redefinition of bounds-of-thing-at-point: New arg SYNTAX-TABLE.
; 3. thing-at-point-with-bounds, form-at-point-with-bounds,
;    bounds-of-form-at-point, symbol-at-point-with-bounds,
;    bounds-of-symbol-at-point, symbol-nearest-point-with-bounds,
;    bounds-of-symbol-nearest-point, symbol-nearest-point,
;    symbol-name-nearest-point: New arg SYNTAX-TABLE.
;
; Revision 1.2  1996/03/08  14:37:42  dadams
; 1. Copyright.
; 2. Added: thing-at-point-with-bounds, form-at-point-with-bounds,
;    bounds-of-form-at-point, symbol-at-point-with-bounds,
;    bounds-of-symbol-at-point
; 3. symbol-at-point: 2nd arg ('symbolp) to form-at-point to ensure interned.
; 4. Moved here from drew-strings.el: symbol-nearest-point-with-bounds,
;    bounds-of-symbol-nearest-point, symbol-nearest-point,
;    symbol-name-nearest-point.
; 5. symbol-nearest-point-with-bounds: Use symbol-at-point-with-bounds, not
;    bounds-of-thing-at-point.
;
; Revision 1.1  1996/02/15  13:49:58  dadams
; Initial revision
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

(require 'thingatpt) ;; bounds-of-thing-at-point, form-at-point
(require 'cl) ;; when, unless

(provide 'thingatpt+)

;;;;;;;;;;;;;;;;;;;;;;


;; Copied from `misc-cmds.el'.
(unless (fboundp 'forward-char-same-line)
  (defun forward-char-same-line (&optional arg)
    "Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Returns the signed number of chars moved if /= ARG, else returns nil."
    (interactive "p")
    (let* ((start (point))
           (fwd-p (natnump arg))
           (max (save-excursion
                  (if fwd-p (end-of-line) (beginning-of-line))
                  (- (point) start))))
      (forward-char (if fwd-p (min max arg) (max max arg)))
      (and (< (abs max) (abs arg)) max))))


;;; THINGS ----------------------------------------------------------

(or (fboundp 'old-bounds-of-thing-at-point)
(fset 'old-bounds-of-thing-at-point (symbol-function
                                     'bounds-of-thing-at-point)))

;; REPLACES ORIGINAL in `thingatpt.el': 
;; Added optional argument SYNTAX-TABLE.
;; NOTE: All of the other functions here are based on this function.
;;;###autoload
(defun bounds-of-thing-at-point (thing &optional syntax-table)
  "Determine the start and end buffer locations for the THING at point,
where THING is an entity for which there is a either a corresponding
`forward-'THING operation, or corresponding `beginning-of-'THING and
`end-of-'THING operations, eg. `word', `sentence', `defun'.

Returns a consp `(START . END)' giving the START and END positions.
Returns nil if no such THING is found.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (if syntax-table
      (let ((buffer-syntax (syntax-table)))
        (unwind-protect
            (progn
              (set-syntax-table syntax-table)
              (old-bounds-of-thing-at-point thing))
          (set-syntax-table buffer-syntax)))
    (old-bounds-of-thing-at-point thing)))

;;;###autoload
(defun thing-at-point-with-bounds (thing &optional syntax-table)
  "Returns (THING START . END) with START and END of THING,
where THING is the `thing-at-point' (which see).
START and END are the car and cdr of the `bounds-of-thing-at-point'.
Returns nil if no such THING is found.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (let ((bounds (bounds-of-thing-at-point thing syntax-table)))
    (and bounds (cons (buffer-substring (car bounds) (cdr bounds)) bounds))))



;; REPLACES ORIGINAL in `thingatpt.el': Added optional argument SYNTAX-TABLE.
;;;###autoload
(defun thing-at-point (thing &optional syntax-table)
  "Return the THING at point (a string)--see `bounds-of-thing-at-point'.
Returns nil if no such THING is found.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point thing syntax-table)))
      (and bounds (buffer-substring (car bounds) (cdr bounds))))))

;;;###autoload
(defun thing-nearest-point-with-bounds (thing &optional syntax-table)
  "Returns (THING START . END) with START and END of THING,
where THING is the `thing-nearest-point' (which see).
Returns nil if no such THING is found.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (let ((thing+bds (thing-at-point-with-bounds thing syntax-table))
        (ind1 0) (ind2 0)
        (bobp (bobp)) (eobp (eobp))
        (bolp (bolp)) (eolp (eolp))
        (updown 1))
    ;; IND2: Loop over lines (alternately up and down).
    (while (and (not thing+bds) (not (and bobp eobp)))
      (setq updown (- updown))          ; Switch directions up/down (1/-1).
      (save-excursion
        (condition-case ()
            (previous-line (* updown ind2)) ; 0, 1, -1, 2, -2, ...
          (beginning-of-buffer (setq bobp t)) (end-of-buffer (setq eobp t))
          (error nil))
        ;; Don't try to go beyond buffer limit.
        (unless (or (and bobp (natnump updown)) (and eobp (< updown 0)))
          (setq thing+bds (thing-at-point-with-bounds thing syntax-table))
          (setq bolp (bolp)) (setq eolp (eolp)) (setq ind1 0)
          (save-excursion
            ;; IND1: Loop over chars in same line (alternately left and right),
            ;; until either found thing or both line limits reached.
            (while (and (not thing+bds) (not (and bolp eolp)))
              (incf ind1)
              (unless bolp (save-excursion ; Left.
                             (setq bolp (forward-char-same-line (- ind1)))
                             (setq thing+bds (thing-at-point-with-bounds
                                              thing syntax-table))))
              (unless (or thing+bds eolp) ; Right.
                (save-excursion
                  (setq eolp (forward-char-same-line ind1))
                  (setq thing+bds (thing-at-point-with-bounds thing
                                                              syntax-table)))))
            (setq bobp (bobp)) (setq eobp (eobp)))))
      ;; Increase search line distance every second time (once up, once down).
      (when (or (natnump updown) (zerop ind2)) (incf ind2))) ; 0,1,1,2,2...
    thing+bds))

;;;###autoload
(defun bounds-of-thing-nearest-point (thing &optional syntax-table)
  "Returns (START . END) with START and END of `thing-nearest-point'
of type THING.  Returns nil if no such THING is found.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (let ((thing+bds (thing-nearest-point-with-bounds thing syntax-table)))
    (and thing+bds (cdr thing+bds))))

;;;###autoload
(defun thing-nearest-point (thing &optional syntax-table)
  "Returns the THING nearest to the cursor, if any, else returns nil.
\"Nearest\" to point is determined as follows:
  The nearest THING on the same line is returned, if there is any.
      Between two THINGs equidistant from point on the same line, the
      leftmost is considered nearer.
  Otherwise, neighboring lines are tried in sequence:
  previous, next, 2nd previous, 2nd next, 3rd previous, 3rd next, etc.
      This means that between two THINGs equidistant from point in
      lines above and below it, the THING in the line above point
      (previous Nth) is considered nearer to it.

A related function:
  `thing-at-point' returns the THING under the cursor, or nil if none.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (let ((thing+bds (thing-nearest-point-with-bounds thing syntax-table)))
    (and thing+bds (car thing+bds))))


;;; FORMS ----------------------------------------------------------

;;;###autoload
(defun form-at-point-with-bounds (&optional thing pred syntax-table) 
  "Returns (FORM START . END), START and END the char positions of FORM,
where FORM is the `form-at-point'.  Returns nil if no form is found.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds."
  (let* ((thing+bds (thing-at-point-with-bounds (or thing 'sexp)
                                                   syntax-table))
         (sexp (and thing+bds
                    (condition-case nil 
                        (read-from-whole-string (car thing+bds))
                      (error nil)))))   ; E.g. tries to read `.'.
    (and sexp (or (not pred) (funcall pred sexp))
         (cons sexp (cdr thing+bds)))))

;;;###autoload
(defun bounds-of-form-at-point (&optional thing pred syntax-table)
  "Returns (START . END), with START and END of `form-at-point'.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds."
  (let ((form+bds (form-at-point-with-bounds thing pred syntax-table)))
    (and form+bds (cdr form+bds))))



;; REPLACES ORIGINAL in `thingatpt.el': 
;; Added optional argument SYNTAX-TABLE.
;;;###autoload
(defun form-at-point (&optional thing pred syntax-table)
"Returns the form nearest to the cursor, if any, else returns nil.
The form is a Lisp entity, not necessarily a string.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds." 
  (let ((sexp (condition-case nil 
		  (read-from-whole-string (thing-at-point (or thing 'sexp)
                                                          syntax-table))
		(error nil))))
    (if (or (not pred) (funcall pred sexp)) sexp)))


;; NOTE: The definition of this function is exactly the same as that of
;; `thing-nearest-point-with-bounds', except that `form-at-point-with-bounds'
;; is used, rather than `thing-at-point-with-bounds'.
;;;###autoload
(defun form-nearest-point-with-bounds (&optional thing pred syntax-table)
  "Returns (FORM START . END), START and END the char positions of FORM,
where FORM is the `form-nearest-point'.
Returns nil if no such form is found.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds."
  (let ((form+bds (form-at-point-with-bounds thing pred syntax-table))
        (ind1 0) (ind2 0)
        (bobp (bobp)) (eobp (eobp))
        (bolp (bolp)) (eolp (eolp))
        (updown 1))
    ;; IND2: Loop over lines (alternately up and down).
    (while (and (not form+bds) (not (and bobp eobp)))
      (setq updown (- updown))          ; Switch directions up/down (1/-1).
      (save-excursion
        (condition-case ()
            (previous-line (* updown ind2)) ; 0, 1, -1, 2, -2, ...
          (beginning-of-buffer (setq bobp t))
          (end-of-buffer (setq eobp t))
          (error nil))
        ;; Don't try to go beyond buffer limit.
        (unless (or (and bobp (natnump updown)) (and eobp (< updown 0)))
          (setq form+bds (form-at-point-with-bounds thing pred syntax-table))
          (setq bolp (bolp)) (setq eolp (eolp))
          (setq ind1 0)
          (save-excursion
            ;; IND1: Loop over chars in same line (alternately left and right),
            ;; until either found form or both line limits reached.
            (while (and (not form+bds) (not (and bolp eolp)))
              (incf ind1)
              (unless bolp (save-excursion ; Left.
                             (setq bolp (forward-char-same-line (- ind1)))
                             (setq form+bds (form-at-point-with-bounds
                                             thing pred syntax-table))))
              (unless (or form+bds eolp) ; Right.
                (save-excursion
                  (setq eolp (forward-char-same-line ind1))
                  (setq form+bds (form-at-point-with-bounds thing pred
                                                            syntax-table)))))
            (setq bobp (bobp)) (setq eobp (eobp)))))
      ;; Increase search line distance every second time (once up, once down).
      (when (or (natnump updown) (zerop ind2)) (incf ind2))) ; 0,1,1,2,2...
    form+bds))

;;;###autoload
(defun bounds-of-form-nearest-point (&optional thing pred syntax-table)
  "Returns (START . END) with START and END of `form-nearest-point'.
Returns nil if no such form is found.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds."
  (let ((form+bds (form-nearest-point-with-bounds thing pred syntax-table)))
    (and form+bds (cdr form+bds))))

;;;###autoload
(defun form-nearest-point (&optional thing pred syntax-table)
  "Returns the form nearest to the cursor, if any, else returns nil.
\"Nearest\" to point is determined as for `thing-nearest-point'.

Optional arguments:
  THING is the kind of form desired (default: `sexp').
  PRED is a predicate that THING must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use while determining bounds."
  (let ((form+bds (form-nearest-point-with-bounds thing pred syntax-table)))
    (and form+bds (car form+bds))))


;;; SYMBOLS ----------------------------------------------------------

;;;###autoload
(defun symbol-at-point-with-bounds ()
  "Returns (SYMBOL START . END) with START and END of SYMBOL,
where SYMBOL is the `symbol-at-point' (which see).
Returns nil if no such Emacs Lisp symbol is found."
  (form-at-point-with-bounds 'symbol 'symbolp emacs-lisp-mode-syntax-table))

;;;###autoload
(defun bounds-of-symbol-at-point ()
  "Returns (START . END) with START and END of `symbol-at-point'."
  (let ((symb+bds (symbol-at-point-with-bounds)))
    (and symb+bds (cdr symb+bds))))


;; REPLACES ORIGINAL in `thingatpt.el':
;; Original defn: (defun symbol-at-point () (form-at-point 'sexp 'symbolp))
;; With point on toto in "`toto'" (in Emacs Lisp mode), that definition
;; returned `toto, not toto.  With point on toto in "`toto'," (note comma),
;; that definition returned nil.  The following definition returns toto
;; in both of these cases.
;; Note also that (form-at-point 'symbol) would not be a satisfactory
;; definition either, because it doesn't ensure that the symbol syntax
;; really represents an interned symbol.
;;;###autoload
(defun symbol-at-point ()
  "Returns the Emacs Lisp symbol under the cursor, or nil if none.

Some related functions:
  `symbol-nearest-point' returns the symbol nearest the cursor, or nil.
  `symbol-name-nearest-point' returns the name of
    `symbol-nearest-point' as a string, or \"\" if none.
  `symbol-name-before-point' returns the string naming the symbol at or
    before the cursor (even if it is on a previous line) or \"\" if none.
  `word-before-point' returns the word (a string) at or before cursor.
Note that these last three functions return strings, not symbols."
  ;; Needs to satisfy both: 1) symbol syntax, 2) be interned.
  (form-at-point 'symbol 'symbolp emacs-lisp-mode-syntax-table))     

;;;###autoload
(defun symbol-nearest-point-with-bounds ()
  "Returns (SYMBOL START . END) with START and END of SYMBOL,
where SYMBOL is the `symbol-nearest-point' (which see).
Returns nil if no such Emacs Lisp symbol is found."
  (form-nearest-point-with-bounds 'symbol 'symbolp
                                  emacs-lisp-mode-syntax-table))

;;;###autoload
(defun bounds-of-symbol-nearest-point ()
  "Returns (START . END) with START and END of `symbol-nearest-point'."
  (let ((symb+bds (symbol-nearest-point-with-bounds)))
    (and symb+bds (cdr symb+bds))))

;;;###autoload
(defun symbol-nearest-point ()
  "Returns the Emacs Lisp symbol nearest the cursor, or nil if none.
\"Nearest\" to point is determined as for `thing-nearest-point'.

Some related functions:
  `symbol-at-point' returns the symbol under the cursor, or nil if none.
  `symbol-name-nearest-point' returns the name of `symbol-nearest-point'
    as a string, or \"\" if none.
  `symbol-name-before-point'  returns the string naming the symbol at or
    before the cursor (even if it is on a previous line) or \"\" if none.
  `word-at-point' returns the word at point, or nil if none.
  `word-nearest-point' returns the word nearest point, or \"\" if none.
  `word-before-point' returns the word at or before the cursor as a string.
Note that these last three functions return strings, not symbols."
  (let ((symb+bds (symbol-nearest-point-with-bounds)))
    (and symb+bds (car symb+bds))))


;;; MISC: SYMBOL NAMES, WORDS, SENTENCES, etc. ----------------------------

;;;###autoload
(defun symbol-name-nearest-point ()
  "String naming the Emacs Lisp symbol nearest point, or \"\" if none.
\"Nearest\" to point is determined as for `thing-nearest-point'."
  (let ((symb (symbol-nearest-point)))
    (if symb (symbol-name symb) "")))

;;;###autoload
(defun word-nearest-point (&optional syntax-table)
  "Returns the word (a string) nearest to point, if any, else \"\".
\"Nearest\" to point is determined as for `thing-nearest-point'.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (thing-nearest-point 'word syntax-table))

;;;###autoload
(defun sentence-nearest-point (&optional syntax-table)
  "Returns the sentence (a string) nearest to point, if any, else \"\".
\"Nearest\" to point is determined as for `thing-nearest-point'.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (thing-nearest-point 'sentence syntax-table))

;;;###autoload
(defun sexp-nearest-point (&optional syntax-table)
  "Returns the sexp (a string) nearest to point, if any, else \"\".
\"Nearest\" to point is determined as for `thing-nearest-point'.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (form-nearest-point 'sexp syntax-table))

;;;###autoload
(defun number-nearest-point (&optional syntax-table)
  "Returns the number nearest to point, if any, else nil.
\"Nearest\" to point is determined as for `thing-nearest-point'.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (form-nearest-point 'sexp 'numberp syntax-table))

;;;###autoload
(defun list-nearest-point (&optional syntax-table)
  "Returns the list nearest to point, if any, else nil.
\(Thus, this does not distinguish between finding no list and finding
the empty list.)
\"Nearest\" to point is determined as for `thing-nearest-point'.

The optional arg is a SYNTAX-TABLE to use while determining bounds."
  (form-nearest-point 'list 'listp syntax-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  thingatpt+.el ends here
