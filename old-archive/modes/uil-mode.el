;;;
;;; File: uil-mode.el
;;;
;;; Description: Tools and mode for uil files
;;;
;;;-----------------------------------------------------------------------------

;; This file is intended for use with GNU Emacs.  The code is in the
;; currently in the public domain, but the original author and the
;; current maintainer are willing to release it under the GPL so that
;; it can become part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;
;;; Originally written by Lionel Cons CERN/PS
;;;
;;; Modified by Jay Berkenbilt (ejb@ERA.COM)
;;; $Id: uil-mode.el,v 1.12 1994/09/19 22:39:07 ejb Exp $
;;;
;;; I have little or no time to maintain this software, but if you send me
;;; bug reports or patches I will see what I can do.  -ejb
;;;

;; Documentation
;; =============
;;
;; This file implements a mode and some tools to aid in editing UIL
;; code.  UIL is the "User Interface Language" used with Motif.
;;
;; To install this, simply copy it into a place within your emacs lisp
;; load path.  If you are using gnu emacs 19, this will probably be
;; /usr/local/lib/emacs/site-lisp.  
;;
;; To use uil-mode automatically on files whose names end with .uil,
;; you should put the following in your .emacs file:
;; 
;; (autoload 'uil-mode "uil-mode" "Major mode for editing uil files" t)
;; (set-variable 'auto-mode-alist
;; 	      (append '(("\\.uil$" . uil-mode)) auto-mode-alist))
;; 
;; Uil mode uses the hook `uil-hook,' so you may define this to run
;; any code you wish to have executed for each uil-mode buffer.  There
;; are a few parameters that can be set by users to affect the way uil
;; mode operates.  They are grouped together in the code below.
;; Search for "customized".
;;
;;
;; LCD Archive Entry:
;; uil-mode|Lionel Cons, E. Jay Berkenbilt|ejb@ERA.COM|
;; Major mode and utility functions for editing Motif UIL code|
;; 18-Sep-1994|1.0|~/modes/uil-mode.el.Z|
;;


;;;-----------------------------------------------------------------------------
;;; Variables and constants
;;;-----------------------------------------------------------------------------

(defconst uil-mode-version "1.0" "*Version of uil-mode")

;;; The following variables can be customized by users.
(defconst uil-indent-level 2
  "*Indentation of uil statements with respect to containing block.")
(defconst uil-justified-comment-pattern "![^!]"
  "*Comment pattern to left-justify.  All other comment patterns are justified
according to the normal indentation rules.")
(defconst uil-auto-insert-semicolon t
  "*If non-nil, automatically insert semicolon when '}' is entered.")
;; End of user-customizable variables

(defvar uil-mode-syntax-table nil
  "Syntax table used while in uil mode.")

(defvar uil-mode-abbrev-table nil
  "Abbrev table used while in uil mode.")
(define-abbrev-table 'uil-mode-abbrev-table ())

(if uil-mode-syntax-table
    ()
  (setq uil-mode-syntax-table c-mode-syntax-table))

(defvar uil-mode-map nil "")
(if uil-mode-map
    ()
  (setq uil-mode-map (make-sparse-keymap))
  (define-key uil-mode-map "\}" 'uil-close-brace)
  (define-key uil-mode-map "\177" 'uil-delete)
  (define-key uil-mode-map "\t" 'uil-indent-line)
  (define-key uil-mode-map "\M-\C-a" 'uil-beginning-of-defun)
  (define-key uil-mode-map "\M-\C-e" 'uil-end-of-defun)
  )

;;;+++--------------------------------------------------------------------------
;;; Beginning/end of defun for Uil
;;;-----------------------------------------------------------------------------
(defun uil-beginning-of-defun ()
  "Move backward to next beginning of object definition.
Returns t unless search stops due to end of buffer."
  (interactive)
  (and (re-search-backward "^object" nil 'move)
       (progn (beginning-of-line) t)))

(defun uil-end-of-defun ()
  "Move forward to next end of object definition.
An end of a definition is found by moving forward from the beginning of one."
  (interactive)
  (let ((pt (point))
	(bod (uil-beginning-of-defun)))
    (if bod
	;; found corresponding bod
	(progn (forward-list 1)
	       (forward-line 1)
	       (or (> (point) pt) ; was inside of a defun
		   (progn (goto-char pt) ; was outside
			  (and (re-search-forward "^object" nil 'move)
			       (progn (forward-list 1) (forward-line 1) t)))))
      ;; was before first bod
      (and (re-search-forward "^object" nil 'move)
	   (progn (forward-list 1) (forward-line 1) t)))))

;;;+++--------------------------------------------------------------------------
;;; Misc functions
;;;-----------------------------------------------------------------------------
(defun uil-last-non-white-char (delta-line)
  "Returns the last non-white, out of comment character of specified line,
or nil if line is out of range,
or SPC if line is empty and is the first of buffer.
Bug: this function does not handle correctly lines with strings containing
\\xxx\\ or which are split with newlines."
  (save-excursion
    (if (/= (forward-line delta-line) 0)
	nil ; can't go there
      (beginning-of-line)
      (let ((pt (point))
	    (c (char-after (point)))
	    (in-string1 nil) ; with single quote
	    (in-string2 nil) ; with double quote
	    (done nil))
	;; find end of 'real' line
	(while (and (not (eobp)) (not done))
	  (cond ((= c ?')
		 (if in-string1
		     (setq in-string1 nil)
		   (if (not in-string2)
		       (setq in-string1 t)))
		 (setq pt (1+ pt)))
		((= c ?\")
		 (if in-string2
		     (setq in-string2 nil)
		   (if (not in-string1)
		       (setq in-string2 t)))
		 (setq pt (1+ pt)))
		((= c ?\\)
		 (if (or in-string1 in-string2)
		     (setq pt (+ pt 2))
		   (setq pt (1+ pt))))
		((= c ?!)
		 (if (or in-string1 in-string2)
		     (setq pt (1+ pt)) ; not a comment
		   (setq done t))) ; first comment found
		((= c ?\n)
		 (setq done t)) ; end the search even if it's in a string
		(t (setq pt (1+ pt)))
		)
	  (if (not done) (setq c (char-after pt))))
	;; now search back for non-white char
	(setq pt (1- pt))
	(setq c (char-after pt))
	(while (and (not (bobp)) (or (= c ? ) (= c ?\t)))
	  (setq pt (1- pt))
	  (setq c (char-after pt)))
	(if (bobp) ?  c)
	))))

(defun uil-first-non-white-char (delta-line)
  "Returns the first non-white character of specified line,
or nil if line is out of range,
or SPC if line is the last of buffer."
  (save-excursion
    (if (/= (forward-line delta-line) 0)
	nil ; can't go there
      (beginning-of-line)
      (let ((pt (point))
	    (c (char-after (point))))
	(while (and (not (eobp)) (or (= c ? ) (= c ?\t)))
	  (setq pt (1+ pt))
	  (setq c (char-after pt)))
	(if (eobp) ?  c)
	))))

;;;+++--------------------------------------------------------------------------
;;; Uil functions (in uil mode)
;;;-----------------------------------------------------------------------------
(defun uil-indentation (delta-line)
  (save-excursion
    (forward-line delta-line)
    (beginning-of-line)
    (forward-to-indentation 0)
    (if (or (eobp) (= (char-after (point)) ?\n))
	0
      (current-column))))

(defun uil-indent-line ()
  "Indent uil line. Does not handle /* ... */ comments correctly.
List enclosed in parenthesis are indented to allow:
  arg = list (
    item1, item2,
    item3, item4
  );"
  (interactive)
  (let (indent)
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      ;; if we have deleted -> eob, add a new line
      (if (and (equal t require-final-newline) (eobp))
	  (save-excursion (newline)))
      (let ((first-char (char-after (point))))
	(if (null first-char)
	    (setq first-char ?\000))
	;; try to find a simple indentation
	(cond ((= first-char ?!)
	       (if (looking-at uil-justified-comment-pattern)
		   (setq indent 0))
	       (= first-char ?c)
	       (if (looking-at "character_set[ \t]*=")
		   (setq indent uil-indent-level)))
	      ((= first-char ?e)
	       (if (looking-at "end[ \t]+module[ \t]*;") (setq indent 0)))
	      ((= first-char ?i)
	       (cond ((looking-at "identifier[ \t\n!]+") (setq indent 0))
		     ((looking-at "include[ \t]+file") (setq indent 0))))
	      ((= first-char ?l)
	       (if (looking-at "list[ \t\n!]+") (setq indent 0)))
	      ((= first-char ?m)
	       (if (looking-at "module[ \t\n!]+") (setq indent 0)))
	      ((= first-char ?n)
	       (if (looking-at "names[ \t]*=") (setq indent uil-indent-level)))
	      ((= first-char ?o)
	       (cond ((looking-at "objects[ \t]*=")
		      (setq indent uil-indent-level))
		     ((looking-at "object[ \t\n!]+") (setq indent 0))))
	      ((= first-char ?p)
	       (if (looking-at "procedure[ \t\n!]+") (setq indent 0)))
	      ((= first-char ?v)
	       (cond ((looking-at "value[ \t\n!]+") (setq indent 0))
		     ((looking-at "version[ \t]*=")
		      (setq indent uil-indent-level))))
	      )
	(if (null indent)
	    (let ((n -1) (previous-first-char nil))
	      ;; find previous non-empty, non-commented line
	      (setq previous-first-char (uil-first-non-white-char n))
	      (while (and previous-first-char ; OK
			  (or (= previous-first-char ?\n) ; empty line
			      (= previous-first-char ?!))) ; commented line
		(setq n (1- n))
		(setq previous-first-char (uil-first-non-white-char n)))
	      (if (null previous-first-char)
		  ;; this is the 'first' line of buffer, left justify
		  (setq indent 0)
		;; general case
		(let ((previous-last-char (uil-last-non-white-char n)))
		  ;; basis is previous indentation
		  (setq indent (uil-indentation n))
		  ;; if not , or ; or & indent one level
		  (if (and (/= previous-last-char ?,)
			   (/= previous-last-char ?\;)
			   (/= previous-last-char ?\&))
		      (setq indent (+ indent uil-indent-level)))
		  ;; if starts with a }, unindent one level
		  (if (= first-char ?})
		      (setq indent (- indent uil-indent-level)))
		  ;; if starts with a {, unindent one level
		  (if (= first-char ?{)
		      (setq indent (- indent uil-indent-level)))
		  ;; if starts with a close parenthesis, unindent two levels
		  (if (= first-char ?\))
		      (setq indent (- indent uil-indent-level
				      uil-indent-level)))
		  ;; special case for default object variant
		  (if (= previous-last-char ?})
		      (setq indent 0))
		  ))))
	(indent-to-column indent)))
    (if (< (current-column) indent)
	(forward-to-indentation 0))))
    
(defun uil-delete ()
  "Delete previous char if in line; delete one level of indentation if in
indentation."
  (interactive)
  (let (indentation)
    (save-excursion
      (beginning-of-line)
      (forward-to-indentation 0)
      (setq indentation (current-column)))
    (if (and (<= (current-column) indentation)
	     (>= (current-column) uil-indent-level))
	(backward-delete-char-untabify uil-indent-level nil)
      (backward-delete-char-untabify 1 nil))))

(defun uil-close-brace (arg)
  "Insert a close brace '}' and indent line.  If uil-auto-insert-semicolon,
insert required ';'."
  (interactive "P")
  (let (indented)
    (if (save-excursion
	  (skip-chars-backward " \t")
	  (bolp))
	(progn
	  ;; Go through contortions so that line is reindented before
	  ;; matching parenthesis are found.
	  (insert last-command-char)
	  (save-excursion (uil-indent-line))
	  (delete-char -1)
	  (setq indented t)))
    ;; Use self-insert-command so that parenthesis matching is performed
    ;; automatically based on the syntax table
    (self-insert-command (prefix-numeric-value arg))
    (if uil-auto-insert-semicolon
	(insert ";"))
    (if (not indented)
	(save-excursion (uil-indent-line)))))

;;;+++--------------------------------------------------------------------------
;;; Uil mode
;;;-----------------------------------------------------------------------------

(defun uil-mode ()
  "Major mode for editing uil files.  Special commands:
\\{uil-mode-map}
Turning on uil-mode calls the value of the variable uil-mode-hook,
if that value is non-nil.
Warning: the indentation algorithm does not handle /* ... */ comments."
  (interactive)
  (kill-all-local-variables)
  (use-local-map uil-mode-map)
  (setq mode-name "Uil")
  (setq major-mode 'uil-mode)
  (setq local-abbrev-table uil-mode-abbrev-table)
  (set-syntax-table uil-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-start)
  (setq comment-start "! ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "! *")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'uil-indent-line)

  (cond ((or (string-match "Lucid" emacs-version)
	     (string-match "^18\\." emacs-version)
	     (boundp 'epoch::version))
	 (make-variable-buffer-local 'compilation-error-regexp)
	 (setq compilation-error-regexp))
	
	((string-match "^19\\." emacs-version)
	 (setq
	  compilation-error-regexp-alist 
	  (append '(
		    ("line:\\s-*\\([0-9]+\\)\\s-*file:\\s-*\\([^ \n]+\\)" 2 1)
		    )
		  compilation-error-regexp-alist)))
	(t nil))

  (run-hooks 'uil-mode-hook))

;;; uil-mode.el ends here
