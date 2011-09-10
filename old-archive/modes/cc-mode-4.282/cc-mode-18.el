;;; cc-mode-18.el --- compatibility for cc-mode in Emacs 18

;; Copyright (C) 1985-1995 Free Software Foundation, Inc.

;; Authors:         1994-1995 Barry A. Warsaw
;; Maintainer:      cc-mode-help@merlin.cnri.reston.va.us
;; Created:         8-Feb-1994, split from cc-mode.el
;; Version:         1.14
;; Last Modified:   1995/08/29 16:05:38
;; Keywords: c languages oop

;; This file is not yet part of GNU Emacs, and hopefully never will be.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file provides hooks into cc-mode for use with Emacs 18. All
;; native Emacs 18 support has been moved out of cc-mode.el proper. If
;; you find you need this file (i.e. you're using Emacs 18), you
;; should SERIOUSLY consider upgrading to Emacs 19.  Emacs 18 simply
;; doesn't have what it takes to edit C++ code; e.g. it cannot handle
;; more than one comment style in a buffer, as C++ and Objective-C
;; require.
;;
;; To use this, put the following in your .emacs file:
;;
;; (require 'cc-mode-18)
;; (or (assq 'c-emacs18-common-hook c-mode-common-hook)
;;     (setq c-mode-common-hook (cons 'c-emacs18-common-hook
;;                                    c-mode-common-hook)))
;;
;; I cannot provide any direct support for Emacs 18 support anymore,
;; but I will install and distribute contributed patches.  This file,
;; and Emacs 18 support in general should be considered sold "As Is".
;; Caveat Emptor! :-)  Note that cc-mode version 5 will not work with
;; Emacs 18 at all, so that's another reason to upgrade.

;;; Code:
(require 'cc-mode)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar c-untame-characters '(?\')
  "*Utilize a backslashing workaround of an Emacs 18 syntax deficiency.
If non-nil, this variable should contain a list of characters which
are prepended by a backslash in comment regions.  By default, the list
contains only the most troublesome character, the single quote.  To be
completely safe, set this variable to:

    '(?\( ?\) ?\' ?\{ ?\} ?\[ ?\])")

(defvar c-backscan-limit 2000
  "*Character limit for looking back while skipping syntactic whitespace.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

;; extend the keymap
(defun c-emacs18-common-hook ()
  ;; Do all the set up necessary to get cc-mode working in Emacs 18,
  ;; but make sure we only do it once
  (if (or (get 'c-mode 'c-emacs-18)
	  (not (featurep 'cc-mode))
	  (not (memq 'v18 c-emacs-features)))
      ;; do nothing if we aren't in Emacs 18, or cc-mode hasn't been
      ;; loaded yet, or we've already run this hook
      nil
    ;; extend the keymap for use with Emacs 18. c-mode-map must
    ;; already be set up (i.e. from cc-mode.el)
    (define-key c-mode-map "\C-c'" 'c-tame-comments)
    (define-key c-mode-map "'"     'c-tame-insert)
    (define-key c-mode-map "["     'c-tame-insert)
    (define-key c-mode-map "]"     'c-tame-insert)
    (define-key c-mode-map "("     'c-tame-insert)
    (define-key c-mode-map ")"     'c-tame-insert)

    ;; Vanilla Emacs 18 doesn't support mult-style comments.  We'll do
    ;; the best we can, but some strange behavior may be encountered.
    ;; UPGRADE YOUR EMACS!
    (modify-syntax-entry ?/  ". 124" c-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"  c-mode-syntax-table)
    (modify-syntax-entry ?\n ">"     c-mode-syntax-table)

    (modify-syntax-entry ?/  ". 124" c++-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"  c++-mode-syntax-table)
    (modify-syntax-entry ?\n ">"     c++-mode-syntax-table)

    ;; set up comment skipping interface functions, and other
    ;; non-compatible functions
    (fset 'c-forward-syntactic-ws 'c-emacs18-fsws)
    (fset 'c-backward-syntactic-ws 'c-emacs18-bsws)
    (fset 'c-in-literal 'c-emacs18-il)
    (fset 'c-mark-function 'c-emacs18-mark-function)
    (fset 'c-insert-special-chars 'c-emacs18-insert-special-chars)

    ;; Emacs 18 can't handle parse-sexp-ignore-comments == t
    (setq parse-sexp-ignore-comments nil)

    ;; put the property out there so we don't do this again
    (put 'c-mode 'c-emacs-18 'c-emacs-18)
    ))


;; Emacs 18 support for whitespace skipping and literal parsing.
;;
;; Emacs 19 has nice built-in functions to do this, but Emacs 18 does
;; not. This is the best we can do in vanilla Emacs 18.

(defun c-emacs18-fsws (&optional lim)
  ;; Forward skip syntactic whitespace for Emacs 18.
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; c++ comment
       ((looking-at "//") (end-of-line))
       ;; c comment
       ((looking-at "/\\*") (re-search-forward "*/" lim 'noerror))
       ;; preprocessor directive
       ((and (= (c-point 'boi) (point))
	     (= (following-char) ?#))
	(end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(defun c-emacs18-bsws (&optional lim)
  ;; Backward skip syntactic whitespace for Emacs 18.
  (let ((lim (or lim (c-point 'bod)))
	literal stop)
    (if (and c-backscan-limit
	     (> (- (point) lim) c-backscan-limit))
	(setq lim (- (point) c-backscan-limit)))
    (while (and (not stop) (> (point) lim))
      (skip-chars-backward " \t\n\r\f" lim)
      ;; c++ comment
      (if (eq (setq literal (c-in-literal lim)) 'c++)
	  (progn
	    (skip-chars-backward "^/" lim)
	    (skip-chars-backward "/" lim)
	    (while (not (or (and (= (following-char) ?/)
				 (= (char-after (1+ (point))) ?/))
			    (<= (point) lim)))
	      (skip-chars-backward "^/" lim)
	      (skip-chars-backward "/" lim)))
	;; c comment
	(if (eq literal 'c)
	    (progn
	      (skip-chars-backward "^*" lim)
	      (skip-chars-backward "*" lim)
	      (while (not (or (and (= (following-char) ?*)
				   (= (preceding-char) ?/))
			      (<= (point) lim)))
		(skip-chars-backward "^*" lim)
		(skip-chars-backward "*" lim))
	      (or (bobp) (forward-char -1)))
	  ;; preprocessor directive
	  (if (eq literal 'pound)
	      (progn
		(beginning-of-line)
		(setq stop (<= (point) lim)))
	    ;; just outside of c block
	    (if (and (= (preceding-char) ?/)
		     (= (char-after (- (point) 2)) ?*))
		(progn
		  (skip-chars-backward "^*" lim)
		  (skip-chars-backward "*" lim)
		  (while (not (or (and (= (following-char) ?*)
				       (= (preceding-char) ?/))
				  (<= (point) lim)))
		    (skip-chars-backward "^*" lim)
		    (skip-chars-backward "*" lim))
		  (or (bobp) (forward-char -1)))
	      ;; none of the above
	      (setq stop t))))))))


;; Return `c' if in a C-style comment, `c++' if in a C++ style
;; comment, `string' if in a string literal, `pound' if on a
;; preprocessor line, or nil if not in a comment at all.  Optional LIM
;; is used as the backward limit of the search.  If omitted, or nil,
;; `beginning-of-defun' is used."
(defun c-emacs18-il (&optional lim)
  ;; Determine if point is in a C/C++ literal
  (save-excursion
    (let* ((here (c-point 'eol))
	   (state nil)
	   (match nil)
	   (lim  (or lim (c-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "\\(/[/*]\\)\\|[\"']\\|\\(^[ \t]*#\\)"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a C++ style comment
	       ((string= "//" match)
		(if (<= here (progn (end-of-line) (point))) 'c++))
	       ;; looking at the opening of a C block comment
	       ((string= "/*" match)
		(if (not (re-search-forward "*/" here 'move)) 'c
		  (if (= (+ (match-beginning 0) 2) here) 'c)))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       ((string-match "[ \t]*#" match)
		(if (<= here (progn (end-of-line) (point))) 'pound))
	       (t nil)))
	) ; end-while
      state)))


(defun c-emacs18-mark-function ()
  "Put mark at end of a C/C++ defun, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark)
  (beginning-of-defun)
  (backward-paragraph))


(defun c-emacs18-insert-special-chars (arg)
  ;; insert last-command-char in the buffer and possibly tame it
  (let ((numarg (prefix-numeric-value arg))
	(literal (c-in-literal)))
    (and (memq literal '(c c++))
	 (memq last-command-char c-untame-characters)
	 (= numarg 1)
	 (insert "\\"))
    (self-insert-command numarg)))


;; Workarounds for GNU Emacs 18 scanning deficiencies
(defun c-tame-insert (arg)
  "Safely inserts certain troublesome characters in comment regions.
See also the variable `c-untame-characters'."
  (interactive "P")
  (let ((literal (c-in-literal)))
    (c-insert-special-chars arg)))

(defun c-tame-comments ()
  "Backslashifies all untamed in comment regions found in the buffer.
See also the variable `c-untame-characters'."
  (interactive)
  ;; make the list into a valid charset, escaping where necessary
  (let ((charset (concat "^" (mapconcat
			      (function
			       (lambda (char)
				 (if (memq char '(?\\ ?^ ?-))
				     (concat "\\" (char-to-string char))
				   (char-to-string char))))
			      c-untame-characters ""))))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
	(skip-chars-forward charset)
	(if (and (not (zerop (following-char)))
		 (memq (c-in-literal) '(c c++))
		 (/= (preceding-char) ?\\ ))
	    (insert-char  ?\\ 1))
	(if (not (eobp))
	    (forward-char 1))))))


;; set up an Emacs session to use this stuff
;;(let ((hookfunsym 'c-emacs18-common-hook))
;;  (if (fboundp 'add-hook)
;;      (add-hook 'c-mode-common-hook hookfunsym)
    ;; do it the hard way
;;    (if (not (memq hookfunsym c-mode-common-hook))
;;	(setq c-mode-common-hook (cons hookfunsym c-mode-common-hook))
;;      )))

(provide 'cc-mode-18)
;; cc-mode-18.el ends here
