;; ========================================================================
;; thing-at-point.el -- Get the thing at point
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Thu Mar 28 13:48:23 1991
;; Last Modified By: Mike Williams
;; Last Modified On: Mon May 27 09:29:02 1991
;; RCS Info        : $Revision: 1.1 $ $Locker:  $
;; ========================================================================
;; NOTE: this file must be recompiled if changed.
;;
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991
;;
;; This file is not part of GNU Emacs, but is made available under the
;; same conditions.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'thing-at-point)

;;=== Usage ===============================================================
;;
;; (autoload 'thing-at-point "thing-at-point")

;; This file provides routines for getting the `thing' at the location of
;; point, whatever that `thing' happens to be.  The `thing' is defined by
;; it's beginning and end positions in the buffer.  There are several ways
;; to determine these two positions:
;;
;;   (1) Using regular expression search.  If regular expressions are given
;;       on the 'beginning-regexp and 'end-regexp properties of 'thing,
;;       beginning and end points are located using 
;;          (re-search-backward beginning-regexp nil t)
;;       and
;;          (re-search-forward end-regexp nil t)
;;       
;;   (2) Using the corresponding beginning-of-thing and end-of-thing
;;       operators. If these operators exist, or are explicitly named using
;;       the 'beginning-op and 'end-op properties of 'thing, beginning and
;;       end points are located using  
;;          (beginning-of-thing)
;;       and
;;          (end-of-thing)
;;       
;;   (3) Using the corresponding forward-thing operator.  If this operator
;;       exists, or is explicitly named using the 'forward-op property of
;;       'thing, beginning and end points are located using
;;          (forward-thing -1)
;;       and
;;          (forward-thing 1)
;;          
;; Note that these different methods may be mixed: eg. a regular expression
;; search may be used to find the beginning of the `thing', while and
;; explicit operator is used to find the end.
;;
;; The function bounds-of-thing-at-point returns these beginning and end
;; points.  The function thing-at-point extracts the corresponding text
;; from the buffer.
;;
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;=== General routines ====================================================

(defun bounds-of-thing-at-point (THING)
  "Determine the start and end buffer locations for the THING at point,
where THING is an entity for which there is a either a corresponding
forward-THING operation, or corresponding beginning-of-THING and
end-of-THING operations, eg. 'word, 'sentence, 'defun.
  Return a cons cell '(start . end) giving the start and end positions."
  (let ((beginning-regexp (eval (get THING 'beginning-regexp)))
	(end-regexp (eval (get THING 'end-regexp)))
	(beginning-op (or (get THING 'beginning-op)
			  (intern-soft (format "beginning-of-%s" THING))))
	(end-op (or (get THING 'end-op)
		    (intern-soft (format "end-of-%s" THING))))
	(forward-op (or (get THING 'forward-op)
			(intern-soft (format "forward-%s" THING))))
	(orig-point (point)))
    (condition-case ()
	(save-excursion
	  (let ((start
		 (progn
		   (cond
		    (beginning-regexp
		     (re-search-backward beginning-regexp nil t))
		    ((fboundp beginning-op) (funcall beginning-op))
		    ((fboundp forward-op) (funcall forward-op -1))
		    (t (error "No beginning operation for %s" THING)))
		   (point)))
		(end
		 (progn
		   (cond
		    (end-regexp
		     (re-search-forward end-regexp nil t))
		    ((fboundp end-op) (funcall end-op))
		    ((fboundp forward-op) (funcall forward-op 1))
		    (t (error "No end operation for %s" THING)))
		   (point))))
	    (if (and start (<= start orig-point) end (<= orig-point end))
		(cons start end))))
      (error nil))))

(defun thing-at-point (THING)
  "Return the THING at point, where THING is an entity defined by
bounds-of-thing-at-point."
  (let ((bounds (bounds-of-thing-at-point THING)))
    (if bounds 
	(buffer-substring (car bounds) (cdr bounds)))))

(defun word-at-point () (thing-at-point 'word))
(defun sexp-at-point () (thing-at-point 'sexp))
(defun sentence-at-point () (thing-at-point 'sentence))

;;=== read-from-whole-string ==============================================
;;
;; Included 'cos it's useful.

(defun read-from-whole-string (STR)
  "Read a lisp expression from STR, signalling an error if the entire string
was not used."
  (let* ((read-data (read-from-string STR))
	 (more-left 
	  (condition-case oops
	      (progn (read-from-string (substring STR (cdr read-data)))
		     t)
	    (end-of-file nil))))
    (if more-left
	(error "Can't read whole string")
      (car read-data))))

;;=== Special cases =======================================================

;;=== Symbols ===
;; Beginning: skip expression prefix characters

(defun beginning-of-symbol ()
  (forward-sexp -1)
  (while (looking-at "\\s'") (forward-char)))

(put 'symbol 'beginning-op 'beginning-of-symbol)
(put 'symbol 'end-op 'forward-sexp)

(defun symbol-at-point () (read-from-whole-string (thing-at-point 'symbol)))

;;=== Lists ===
;; Beginning: Regexp search
;; End: Use forward-list

(put 'list 'beginning-regexp "\\s(")
(put 'list 'end-op 'forward-list)

;;=== Strings ===
;; Define operators for beginning/end

(defun beginning-of-string ()
  (let ((end (point)) 
	parse-data in-string)
    (beginning-of-defun)
    (setq parse-data (parse-partial-sexp (point) end))
    (setq in-string (nth 3 parse-data))
    (if in-string 
	(progn (re-search-backward "\\\"") in-string)
      (error "Not within string"))))

(defun end-of-string ()
  (forward-char 1)
  (let ((end-char (beginning-of-string)))
    (forward-char 1)
    (search-forward (char-to-string end-char))))

;;=== Filenames ===
;; Define operators for beginning/end

(defvar file-name-chars "~/A-Za-z0-9---_.$#%,"
  "Characters allowable in filenames.")

(defun beginning-of-filename ()
  (interactive)
  (let ((regexp (format "[^%s]+" file-name-chars)))
    (if (re-search-backward regexp)
	(forward-char 1))))
    
(defun end-of-filename ()
  (interactive)
  (let ((regexp (format "[%s]+" file-name-chars)))
    (re-search-forward regexp)))

;;=== END of thing-at-point.el ============================================

