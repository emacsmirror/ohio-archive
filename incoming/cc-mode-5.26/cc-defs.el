;;; cc-defs.el --- compile time definitions for CC Mode

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Authors:    1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Get all the necessary compile time definitions.
(require 'custom)
(require 'derived)			;only necessary in Emacs 20

;; cc-mode-19.el contains compatibility macros that should be compiled
;; in if needed.
(if (or (not (fboundp 'functionp))
	(not (condition-case nil
		 (progn (char-before) t)
	       (error nil)))
	(not (condition-case nil
		 (progn (char-after) t)
	       (error nil)))
	(not (fboundp 'when))
	(not (fboundp 'unless)))
    (require 'cc-mode-19))


(defmacro c-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; eod  -- end of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  `(save-excursion
     ,(if (and (eq (car-safe position) 'quote)
	       (symbolp (eval position)))
	  (let ((position (eval position)))
	    (cond
	     ((eq position 'bol)  `(beginning-of-line))
	     ((eq position 'eol)  `(end-of-line))
	     ((eq position 'boi)  `(back-to-indentation))
	     ((eq position 'bonl) `(forward-line 1))
	     ((eq position 'bopl) `(forward-line -1))
	     ((eq position 'bod)  `(c-beginning-of-defun-1))
	     ((eq position 'eod)  `(c-end-of-defun-1))
	     ((eq position 'iopl) `(progn
				     (forward-line -1)
				     (back-to-indentation)))
	     ((eq position 'ionl) `(progn
				     (forward-line 1)
				     (back-to-indentation)))
	     (t (error "unknown buffer position requested: %s" position))))
	;;(message "c-point long expansion")
	`(let ((position ,position))
	   (cond
	    ((eq position 'bol)  (beginning-of-line))
	    ((eq position 'eol)  (end-of-line))
	    ((eq position 'boi)  (back-to-indentation))
	    ((eq position 'bonl) (forward-line 1))
	    ((eq position 'bopl) (forward-line -1))
	    ((eq position 'bod)  (c-beginning-of-defun-1))
	    ((eq position 'eod)  (c-end-of-defun-1))
	    ((eq position 'iopl) (progn
				   (forward-line -1)
				   (back-to-indentation)))
	    ((eq position 'ionl) (progn
				   (forward-line 1)
				   (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position)))))
     (point)))


(defmacro c-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  `(condition-case nil
       (progn ,@body)
     (error nil)))

(defsubst c-beginning-of-defun-1 ()
  ;; Wrapper around beginning-of-defun.
  ;;
  ;; NOTE: This function should contain the only explicit use of
  ;; beginning-of-defun in CC Mode.  Eventually something better than
  ;; b-o-d will be available and this should be the only place the
  ;; code needs to change.  Everything else should use
  ;; (c-beginning-of-defun-1)
  (if (and (fboundp 'buffer-syntactic-context-depth)
	   c-enable-xemacs-performance-kludge-p)
      ;; XEmacs only.  This can improve the performance of
      ;; c-parse-state to between 3 and 60 times faster when
      ;; braces are hung.  It can also degrade performance by
      ;; about as much when braces are not hung.
      (let (pos)
	(while (not pos)
	  (save-restriction
	    (widen)
	    (setq pos (scan-lists (point) -1
				  (buffer-syntactic-context-depth)
				  nil t)))
	  (cond
	   ((bobp) (setq pos (point-min)))
	   ((not pos)
	    (let ((distance (skip-chars-backward "^{")))
	      ;; unbalanced parenthesis, while illegal C code,
	      ;; shouldn't cause an infloop!  See unbal.c
	      (when (zerop distance)
		;; Punt!
		(beginning-of-defun)
		(setq pos (point)))))
	   ((= pos 0))
	   ((not (eq (char-after pos) ?{))
	    (goto-char pos)
	    (setq pos nil))
	   ))
	(goto-char pos))
    ;; Emacs, which doesn't have buffer-syntactic-context-depth
    (beginning-of-defun))
  ;; if defun-prompt-regexp is non-nil, b-o-d won't leave us at the
  ;; open brace.
  (and defun-prompt-regexp
       (looking-at defun-prompt-regexp)
       (goto-char (match-end 0))))

(defsubst c-end-of-defun-1 ()
  ;; Replacement for end-of-defun that use c-beginning-of-defun-1.
  (while (and (c-safe (down-list 1) t)
	      (not (eq (char-before) ?{)))
    ;; skip down into the next defun-block
    (forward-char -1)
    (c-forward-sexp))
  (c-beginning-of-defun-1)
  (c-forward-sexp))

(defmacro c-forward-sexp (&optional arg)
  ;; like forward-sexp except
  ;;   1. this is much stripped down from the XEmacs version
  ;;   2. this cannot be used as a command, so we're insulated from
  ;;      XEmacs' losing efforts to make forward-sexp more user
  ;;      friendly
  ;;   3. Preserves the semantics most of CC Mode is based on
  (or arg (setq arg 1))
  `(goto-char (or (scan-sexps (point) ,arg)
		  ,(if (numberp arg)
		       (if (> arg 0) `(point-max) `(point-min))
		     `(if (> ,arg 0) (point-max) (point-min))))))

(defmacro c-backward-sexp (&optional arg)
  ;; See c-forward-sexp and reverse directions
  (or arg (setq arg 1))
  `(c-forward-sexp ,(if (numberp arg) (- arg) `(- ,arg))))

(defsubst c-beginning-of-macro (&optional lim)
  ;; Go to the beginning of a cpp macro definition.  Leaves point at
  ;; the beginning of the macro and returns t if in a cpp macro
  ;; definition, otherwise returns nil and leaves point unchanged.
  ;; `lim' is currently ignored, but the interface requires it.
  (let ((here (point)))
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (forward-line -1))
    (back-to-indentation)
    (if (and (<= (point) here)
	     (eq (char-after) ?#))
	t
      (goto-char here)
      nil)))

(defsubst c-forward-comment (count)
  ;; Insulation from various idiosyncrasies in implementations of
  ;; `forward-comment'.  Note: Some emacsen considers incorrectly that
  ;; any line comment ending with a backslash continues to the next
  ;; line.  I can't think of any way to work around that in a reliable
  ;; way without changing the buffer though.  Suggestions welcome. ;)
  (let ((here (point)))
    (if (>= count 0)
	(when (forward-comment count)
	  ;; Emacs includes the ending newline in a b-style
	  ;; (c++) comment, but XEmacs don't.  We depend on the
	  ;; Emacs behavior (which also is symmetric).
	  (if (and (eolp) (nth 7 (parse-partial-sexp here (point))))
	      (condition-case nil (forward-char 1)))
	  t)
      ;; When we got newline terminated comments,
      ;; forward-comment in all supported emacsen so far will
      ;; stop at eol of each line not ending with a comment when
      ;; moving backwards.  The following corrects for it when
      ;; count is -1.  The other common case, when count is
      ;; large and negative, works regardless.  It's too much
      ;; work to correct for the rest of the cases.
      (skip-chars-backward " \t\n\r\f")
      (if (bobp)
	  ;; Some emacsen return t when moving backwards at bob.
	  nil
	(re-search-forward "[\n\r]" here t)
	(if (forward-comment count)
	    (if (eolp) (forward-comment -1) t))))))

(defmacro c-add-syntax (symbol &optional relpos)
  ;; a simple macro to append the syntax in symbol to the syntax list.
  ;; try to increase performance by using this macro
  `(setq syntax (cons (cons ,symbol ,relpos) syntax)))

(defmacro c-add-class-syntax (symbol classkey)
  ;; The inclass and class-close syntactic symbols are added in
  ;; several places and some work is needed to fix everything.
  ;; Therefore it's collected here.
  `(save-restriction
     (widen)
     (let ((symbol ,symbol)
	   (classkey ,classkey))
       (goto-char (aref classkey 1))
       (if (and (eq symbol 'inclass) (= (point) (c-point 'boi)))
	   (c-add-syntax symbol (point))
	 (c-add-syntax symbol (aref classkey 0))
	 (if (and c-inexpr-class-key (c-looking-at-inexpr-block))
	     (c-add-syntax 'inexpr-class))))))

(defmacro c-auto-newline ()
  ;; if auto-newline feature is turned on, insert a newline character
  ;; and return t, otherwise return nil.
  `(and c-auto-newline
	(not (c-in-literal))
	(not (newline))))

(defsubst c-intersect-lists (list alist)
  ;; return the element of ALIST that matches the first element found
  ;; in LIST.  Uses assq.
  (let (match)
    (while (and list
		(not (setq match (assq (car list) alist))))
      (setq list (cdr list)))
    match))

(defsubst c-lookup-lists (list alist1 alist2)
  ;; first, find the first entry from LIST that is present in ALIST1,
  ;; then find the entry in ALIST2 for that entry.
  (assq (car (c-intersect-lists list alist1)) alist2))

(defsubst c-langelem-col (langelem &optional preserve-point)
  ;; convenience routine to return the column of langelem's relpos.
  ;; Leaves point at the relpos unless preserve-point is non-nil.
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here))
      )))

(defmacro c-update-modeline ()
  ;; set the c-auto-hungry-string for the correct designation on the modeline
  `(progn
     (setq c-auto-hungry-string
	   (if c-auto-newline
	       (if c-hungry-delete-key "/ah" "/a")
	     (if c-hungry-delete-key "/h" nil)))
     (force-mode-line-update)))

(defsubst c-keep-region-active ()
  ;; Do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  This is not needed
  ;; for Emacs.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst c-region-is-active-p ()
  ;; Return t when the region is active.  The determination of region
  ;; activeness is different in both Emacs and XEmacs.
  (cond
   ;; XEmacs
   ((and (fboundp 'region-active-p)
	 (boundp 'zmacs-regions)
	 zmacs-regions)
    (region-active-p))
   ;; Emacs
   ((boundp 'mark-active) mark-active)
   ;; fallback; shouldn't get here
   (t (mark t))))

(defsubst c-major-mode-is (mode)
  (eq (derived-mode-class major-mode) mode))

(defmacro c-with-syntax-table (table &rest code)
  ;; Temporarily switches to the specified syntax table in a failsafe
  ;; way to execute code.
  `(let ((c-with-syntax-table-orig-table (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table ,table)
	   ,@code)
       (set-syntax-table c-with-syntax-table-orig-table))))
(put 'c-with-syntax-table 'lisp-indent-function 1)


(provide 'cc-defs)
;;; cc-defs.el ends here
