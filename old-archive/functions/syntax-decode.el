;;*****************************************************************************
;;
;; Filename:	syntax-decode.el
;;
;; LCD Archive Entry:
;; syntax-decode|Rod Whitby|rwhitby@research.canon.oz.au|
;; Decode comment characteristics from the syntax table.|
;; 1992-11-22|1.13|~/functions/syntax-decode.el.Z|
;;
;; Copyright (C) 1992  Rod Whitby
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Authors: 1992	Rod Whitby, <rwhitby@research.canon.oz.au>
;;          1991	Ken Wood, <kwood@austek.oz.au>
;;
;; Description:	Calculate regular expressions used to match comments in the
;;		current major mode. Also calculates strings that may be used
;;		to begin & end comments in the major mode.
;;
;;		The values calculated are assigned to the buffer-local
;;		variables syndecode-comment-start-regexp,
;;		syndecode-comment-end-regexp, syndecode-comment-start-string,
;;		and syndecode-comment-end-string.
;;
;;		If the function decode-syntax-table is run more than once in
;;		the same buffer, later invocations do nothing. Each time a new
;;		syntax table is decoded, its data is "cached" for use next
;;		time that mode is encountered.
;;
;;		It should prove fairly simple to extract extra features from
;;		the syntax table - drop me a line if you need something else
;;		and we can work something out.
;;
;;		To install this package so that other packages can use it,
;;		add this line to your .emacs:
;;
;;    (autoload 'decode-syntax-table "syntax-decode" "autoloadable function" t)
;;
;;*****************************************************************************

;; $Id: syntax-decode.el,v 1.13 1992/11/22 21:11:15 rwhitby Exp $

(defvar syndecode-comment-start-regexp nil
  "\
Regexp to match the start of comments in the current mode. This value
is more reliable than the comment-start variable, since it is
determined directly from the syntax table. Will be nil if comments are
not defined in the current syntax table.")

(defvar syndecode-comment-end-regexp nil
  "\
Regexp to match the end of comments in the current mode. This value is
more reliable than the comment-end variable, since it is determined
directly from the syntax table. Will be nil if comments are not
defined in the current syntax table.")

(defvar syndecode-comment-continue-regexp nil
  "\
Regexp to match a continuation of comments in the current mode (i.e.
whitespace followed by syndecode-comment-start-regexp).  This value is
determined directly from the syntax table. Will be nil if comments are not
defined in the current syntax table.")

(defvar syndecode-comment-start-string nil
  "\
Preferred string to be used to begin comments in the current mode.
Will be nil if comments are not defined in the current syntax table.")

(defvar syndecode-comment-end-string nil
  "\
Preferred string to be used to terminate comments in the current mode.
Will be nil if comments are not defined in the current syntax table or if
comments can be terminated by a newline.")

(defvar syndecode-done-this-buffer nil
  "\
Buffer-local variable indicating whether the syntax table for this buffer
has been decoded or not.")

(make-variable-buffer-local 'syndecode-comment-start-regexp)
(make-variable-buffer-local 'syndecode-comment-end-regexp)
(make-variable-buffer-local 'syndecode-comment-continue-regexp)
(make-variable-buffer-local 'syndecode-comment-start-string)
(make-variable-buffer-local 'syndecode-comment-end-string)
(make-variable-buffer-local 'syndecode-done-this-buffer)

(defvar syndecode-mode-feature-alist nil
  "\
Alist of major modes and their associated comment data as extracted
>from the syntax table. Acts as a cache when syntax-decode is run
under the same major mode more than once.")


(defun decode-syntax-table ()
  "\
Parse the syntax table for the current mode and figure set the variables
`syndecode-comment-start-regexp', `syndecode-comment-end-regexp',
`syndecode-comment-continue-regexp', `syndecode-comment-start-string' and
`syndecode-comment-end-string'."
  ;; Check to make sure this buffer hasn't already been done first.
  (if (not syndecode-done-this-buffer)
      ;; First check to see if the syntax table for this mode has been
      ;; decoded at some time in the past, by checking in the "cache"
      ;; for the previously extracted values.
      (let* ((cached-syntax-list (assq major-mode
				       syndecode-mode-feature-alist)))
	(if cached-syntax-list
	    (progn
	      (setq cached-syntax-list (cadr cached-syntax-list))
	      (setq syndecode-comment-start-regexp (nth 0 cached-syntax-list))
	      (setq syndecode-comment-end-regexp (nth 1 cached-syntax-list))
	      (setq syndecode-comment-continue-regexp
		    (nth 2 cached-syntax-list))
	      (setq syndecode-comment-start-string (nth 3 cached-syntax-list))
	      (setq syndecode-comment-end-string (nth 4 cached-syntax-list))
	      )
	  ;; If not cached, then must calculate the value from the current
	  ;; syntax table.
	  (progn
	    ;; Iterate over the syntax table & decode each character.
	    (let (
		  (debug-on-error t)
		  (tmp-syntax-table (append (syntax-table) nil))
		  (table-index 0)
		  (code nil)
		  (stripped-code nil)
		  (char nil)
		  (comm-start-string nil)
		  (comm-end-string nil)
		  (char1-long-comm-start nil)
		  (char2-long-comm-start nil)
		  (char1-long-comm-end nil)
		  (char2-long-comm-end nil)
		  (long-comm-start-string nil)
		  (long-comm-end-string nil)
		  temp-alist-cell
		  )
	      (while (and (< table-index 255) tmp-syntax-table)
		(progn
		  ;; Extract the current code & character
		  (setq code (car tmp-syntax-table))
		  (setq char (char-to-string table-index))
		  (setq stripped-code (logand code 255))
		  
		  ;; First, check if the flags for two-character comments
		  ;; are set
		  (if (/= 0 (logand (lsh code -16) 1))
		      (setq char1-long-comm-start char))
		  (if (/= 0 (logand (lsh code -17) 1))
		      (setq char2-long-comm-start char))
		  (if (/= 0 (logand (lsh code -18) 1))
		      (setq char1-long-comm-end char))
		  (if (/= 0 (logand (lsh code -19) 1))
		      (setq char2-long-comm-end char))
		  
		  ;; Now check for single-character comments
		  (if (= stripped-code 11)
		      (setq comm-start-string (concat comm-start-string char)))
		  ;; else
		  (if (= stripped-code 12)
		      (setq comm-end-string (concat comm-end-string char)))
		  
		  ;; Move to the next element of the syntax table.
		  (setq table-index (+ table-index 1))
		  (setq tmp-syntax-table (cdr tmp-syntax-table))
		  ))
	      
	      ;; Now, build the long (two character) comment strings, if their
	      ;; component variables are defined.
	      (if (and char1-long-comm-start char2-long-comm-start)
		  (progn
		    (setq long-comm-start-string
			  (concat char1-long-comm-start char2-long-comm-start))
		    (setq syndecode-comment-start-regexp
			  (concat (regexp-quote char1-long-comm-start)
				  (regexp-quote char2-long-comm-start)))))
	      (if (and char1-long-comm-end char2-long-comm-end)
		  (progn
		    (setq long-comm-end-string (concat char1-long-comm-end
						       char2-long-comm-end))
		    (setq syndecode-comment-end-regexp
			  (concat (regexp-quote char1-long-comm-end)
				  (regexp-quote char2-long-comm-end)))))
	      
	      ;; Now create the comment start & end regexps from the comment
	      ;; start & end strings.
	      
	      ;; Extract each character from comm-start-string and add it
	      ;; verbatim to comment-start-regexp, a list of alternatives.
	      (let ((comm-start-index 0)
		    (comm-start-length (length comm-start-string)))
		(while (< comm-start-index comm-start-length)
		  (progn
		    (if syndecode-comment-start-regexp
			(setq syndecode-comment-start-regexp
			      (concat syndecode-comment-start-regexp "\\|"
				      (regexp-quote
				       (substring comm-start-string
						  comm-start-index
						  (1+ comm-start-index)))))
		      (setq syndecode-comment-start-regexp
			    (regexp-quote (substring comm-start-string
						     comm-start-index
						     (1+ comm-start-index)))))
		    (setq comm-start-index (1+ comm-start-index)))))
	      
	      ;; Extract each character from comm-end-string and add it
	      ;; verbatim to comment-end-regexp, a list of alternatives.
	      (let ((comm-end-index 0)
		    (comm-end-length (length comm-end-string)))
		(while (< comm-end-index comm-end-length)
		  (progn
		    (if syndecode-comment-end-regexp
			(setq syndecode-comment-end-regexp
			      (concat syndecode-comment-end-regexp "\\|"
				      (regexp-quote
				       (substring comm-end-string
						  comm-end-index
						  (1+ comm-end-index)))))
		      (setq syndecode-comment-end-regexp
			    (regexp-quote (substring comm-end-string
						     comm-end-index
						     (1+ comm-end-index)))))
		    (setq comm-end-index (1+ comm-end-index)))))
	      
	      ;; Set up the comment continue regexp.
	      (setq syndecode-comment-continue-regexp
		    (and syndecode-comment-start-regexp
			 (concat "\\s-*\\("
				 syndecode-comment-start-regexp
				 "\\)")))
	      
	      ;; Set up the comment start string.
	      (setq syndecode-comment-start-string
		    ;; Prefer the two-character comment sequence
		    (or long-comm-start-string
			;; Failing that, use one of the single character
			;; comment starting sequences.
			(if comm-start-string
			    (substring comm-start-string -1))))
	      
	      ;; Now, set up the comment end string.
	      (setq syndecode-comment-end-string
		    ;; Set it to nil if newlines can terminate comments
		    (and (not (and comm-end-string
				   (string-match "\n" comm-end-string)))
			 ;; Otherwise, prefer the two character comment
			 ;; sequence
			 (or long-comm-end-string
			     ;; Failing that, one of the single character
			     ;; comment terminators.
			     (if comm-end-string
				 (substring comm-end-string -1)))))
	      
	      ;; Store the newly determined syntax features into the syntax
	      ;; "cache" for lookup if this mode is encountered again later.
	      (setq temp-alist-cell
		    (list (list major-mode
				(list syndecode-comment-start-regexp
				      syndecode-comment-end-regexp
				      syndecode-comment-continue-regexp
				      syndecode-comment-start-string
				      syndecode-comment-end-string))))
	      ;; Add the current syntax features to the cache.
	      (if syndecode-mode-feature-alist
		  (setq syndecode-mode-feature-alist
			(append temp-alist-cell syndecode-mode-feature-alist))
		(setq syndecode-mode-feature-alist temp-alist-cell))
	      )))
	;; Set a flag to indicate the syntax table in this buffer has been
	;; decoded.
	(setq syndecode-done-this-buffer t)
	))) ;; end of defun

(provide 'syntax-decode)
