;;; sc-unsupp.el --- unsupported Supercite hacks and ideas

;; Author: 1993 Barry A. Warsaw, Century Computing, Inc. <bwarsaw@cen.com>
;; Maintainer:    bwarsaw@cen.com
;; Created:       February 1993
;; Version:       3.1
;; Last Modified: 1993/09/22 18:53:29
;; Keywords: Supercite 3, hacks

;; sc-unsupp.el revision: 3.7

;; Copyright (C) 1993 Barry A. Warsaw, except where explicitly stated
;; otherwise.

;; This file is not part of GNU Emacs, nor is it ever likely to be.

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

;;; Description:
;;
;; THIS FILE IS NOT INTENDED TO BE LOADED AS-IS. YOU SHOULD CUT AND
;; PAST THE BITS OUT OF HERE THAT YOU WANT TO PLAY WITH.
;;
;; This file contains some ideas and hacks that I've been toying with
;; for Supercite 3.  It is completely unsupported and may not even be
;; up-to-date with respect to Supercite 3's interfaces.  Some of this
;; stuff may eventually make it into Supercite 3 in a similar or
;; different form.  If you run with any of these ideas and get some
;; stuff you think is interesting, I'd be happy to take a look.  Email
;; it the supercite mailing list (see the texinfo manuals for details).

(error "File sc-unsupp.el is not intended to be loaded!")

;;; Code:

;; add-hook.  Emacs 19 has a nice facility for adding and removing
;; functions from a hook variable.  Emacs 18 does not have this stuff
;; defined but its provided here so you can use it to conform to the
;; interfaces described in the Supercite texinfo manual.

(defun member (elt list)
  "Returns non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT."
  (while (and list
	      (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

;;; Taken from Lucid Emacs 19.4 .../lisp/prim/subr.el
(defun add-hook (hook-var function &optional at-end)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
  ;;(interactive "SAdd to hook-var (symbol): \naAdd which function to %s? ")
  (if (not (boundp hook-var)) (set hook-var nil))
  (let ((old (symbol-value hook-var)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(setq old (list old)))
    (if (member function old)
	nil
      (set hook-var
	   (if at-end
	       (append old (list function))
	     (cons function old))))))

(defun remove-hook (hook-var function)
  "Remove a function from a hook, if it is present.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to remove (compared with `eq')."
  (let (val)
    (cond ((not (boundp hook-var))
	   nil)
	  ((eq function (setq val (symbol-value hook-var)))
	   (setq hook-var nil))
	  ((consp val)
	   (set hook-var (delq function val))))))


;; mail-add-header.  Here is a really cool couple of functions for
;; letting you set up extra cute outgoing mail headers.  I use this to
;; insert an X-Attribution: field automatically in all outgoing mail
;; (VM) and news (GNUS) messages.  This was contributed by Mike
;; Williams, mike-w@cs.aukuni.ac.nz, and might be available on the
;; elisp archive machine.  Also is an example of how I use it to set
;; up X-Attribution.

;; LCD Archive Entry:
;; mail-add-header|Mike Williams|mike-w@cs.aukuni.ac.nz
;; |Functions to add and remove named mail headers
;; |91-07-03||~/functions/mail-add-header.el.Z

(defun mail-add-header (HEADER CONTENTS &optional REPLACE)
  "Add the specified HEADER to the current mail message, with the given 
CONTENTS.  
If the header already exists, the contents are left unchanged, unless optional 
argument REPLACE is non-nil."
  (save-excursion
    (let ((header-exists (mail-position-on-field HEADER)))
      ;; Delete old contents if REPLACE is set
      (if (and header-exists REPLACE)
	  (let ((end (point))
		(beg (progn
		       (re-search-backward (concat HEADER ": "))
		       (goto-char (match-end 0)))))
	    (delete-region beg end)))
      ;; Add new contents if REPLACE is set, or this is a new header.
      (if (or (not header-exists) REPLACE)
	  (progn (insert CONTENTS) CONTENTS)))))

(defun mail-remove-header (HEADER)
  "Remove the specified HEADER from the current mail message."
  (save-excursion
    (if (mail-position-on-field HEADER 'soft)
	(let ((end (point))
	      (beg (progn
		     (re-search-backward (concat HEADER ": "))
		     (goto-char (match-beginning 0)))))
	  (delete-region beg (1+ end))))))


(defvar baw:x-attribution "BAW"
  "*Value of X-Attribution field.")

(defun baw:x-attribution ()
  "Add the X-Attribution header."
  (mail-add-header "X-Attribution" baw:x-attribution))

;; add this to your .emacs file to make attributions work, but be sure
;; to change baw:x-attribution to something more appropriate.
;;(add-hook 'mail-setup-hook 'baw:x-attribution)



;; code citing.  One of the things that should be possible with sc3 is
;; the special citing of code lines.  You usually don't want code
;; (e.g. C code) filled and it would be nice to put a different
;; attribution string on code.  Unfortunately, its quite difficult to
;; come up with a regexp to recognize code.  This approach is slightly
;; different, looking at the length of the line to see if its code or
;; not.  I've had limited success with this, but not enough to put it
;; into Supercite proper.

(defvar sc-code-column 60
  "*Max length of code lines.")

(defun sc-cite-perhaps-code-line ()
  "True if length of line is less than `sc-code-column'.
Length of line is counted from the first non-whitespace character to
last character (whitespace or not) on line."
  (< (- (regi-pos 'eol) (regi-pos 'boi)) sc-code-column))

(defun sc-cite-code-line ()
  "Cite the current line as if it is code.  Do not fill it."
  (insert sc-citation-leader "|" sc-citation-separator)
  (sc-fill-if-different ""))

(defvar sc-recite-as-code-frame
  '(((sc-cite-regexp)  (progn (sc-uncite-line)
			      (sc-cite-code-line)))
    )
  "Standard frame for reciting a region of code.")

;;(defvar sc-cite-me-you-frame
(setq sc-cite-me-you-frame
  '((begin                                (sc-fill-if-different))
    ("^\\s *\\(you\\|me\\)[>}]+\\s *"     (sc-swap-me-and-you) nil t)
    ("^[ \t]*$"                           (sc-fill-if-different ""))
    ((concat "^" sc-reference-tag-string) nil)
    ((sc-cite-regexp "")                  (sc-cite-coerce-nested-citation))
    ((sc-cite-regexp)                     (sc-cite-coerce-cited-line))
    (t                                    (sc-cite-new-line-with-you))
    (end                                  (sc-fill-if-different ""))
    )
;;  "Standard frame for citing by swapping me's for you's.")
  )



;; No short attrib strings.  I use "initials" as my default preferred
;; attibution style, but it can sometimes give me 1 character long
;; attributions.  I don't like that.  This works around it.

(defun baw:no-short-attribs ()
  (if (<= (length attribution) 1)
      (let ((sc-attribs-postselect-hook nil)
	    (sc-preferred-attribution-list nil)
	    (sc-confirm-always-p t)
	    (sc-use-only-preference-p nil)
	    (sc-mail-mumble ""))
	(sc-select-attribution)
	(setq attribution (sc-mail-field "sc-attribution")
	      citation    (sc-mail-field "sc-citation"))
	)))

(add-hook 'sc-attribs-postselect-hook 'baw:no-short-attribs)



;; Miscellaneous stuff.  My bro' Craig uses a non-standard citation
;; style, and this tries to recognize that style.  Also, I'm putzing
;; around with a me/you cite frame where, in a 1-to-1 correspondance,
;; Supercite will do away with the more formal name strings and use
;; me> and you>, inverting the sense correctly.  It sort of works but
;; not good enough to make it into Supercite proper.

(defun baw:coerce-craig-cite ()
  (beginning-of-line)
  (let ((start (point))
	(end (progn (skip-chars-forward "-") (point))))
    (delete-region start end)
    (if (not (looking-at "[ \t]*$"))
	(insert "    me> ")))
  nil)

(defun baw:cite-line-with-possible-fill (&optional citation)
  "Cite a single line of uncited text.
Optional CITATION overrides any citation automatically selected."
  (if sc-fixup-whitespace-p
      (fixup-whitespace))
  (let ((prefix (or citation
		    (cdr (assoc "sc-citation" sc-mail-info))
		    sc-default-attribution)))
    (insert prefix)
    (let ((boi (regi-pos 'boi))
	  (eot (save-excursion
		 (end-of-line)
		 (skip-chars-backward " \t")
		 (point))))
      (if (< (- eot boi) fill-column)
	  (sc-fill-if-different "")	;re-init sc-fill-begin
	(sc-fill-if-different prefix))))
  nil)

(setq baw:default-cite-frame
  '((begin                   (sc-fill-if-different))
    ("^[ \t]*$"              (sc-fill-if-different ""))
    (sc-reference-tag-string nil)
    (sc-nested-citation-p    (sc-add-citation-level))
    ((sc-cite-regexp)        (sc-cite-coerce-cited-line))
    (t                       (baw:cite-line-with-possible-fill))
    (end                     (sc-fill-if-different ""))
    )
  )

(setq baw:craig-cite-frame
  '((begin                 (sc-fill-if-different))
    ("^-+"                 (baw:coerce-craig-cite))
    ("^[ \t]*$"            (sc-fill-if-different ""))
    ("^[ \t]*>[ \t]*.*$"   (progn
			     (sc-uncite-line)
			     (sc-cite-line "    me> ")))
    (sc-nested-citation-p  (sc-add-citation-level))
    ((sc-cite-regexp)      (sc-cite-coerce-cited-line))
    (t                     (sc-cite-line))
    (end                   (sc-fill-if-different ""))
    )
  )

(setq sc-cite-frame-alist
      '(("sc-author"  (("Craig.*Warsaw" . baw:craig-cite-frame)))
	("cc"         (("^$"            . sc-cite-me-you-frame)))
	("newsgroups" (("^$"            . sc-cite-me-you-frame)))
	))
(setq sc-default-cite-frame baw:default-cite-frame)


(defun baw:sc-pre-cite-hook ()
  (let ((sc-mail-mumble nil))
    (if (or (sc-mail-field "cc")
	    (sc-mail-field "newsgroups"))
	(setq sc-preferred-header-style 4)
      (setq sc-preferred-header-style 0))))

;; citing
(defun sc-cite-swap-me-and-you ()
  "Swaps \"me\" attributions to \"you\"'s and vice versa."
  (let* ((case-fold-search t)
	 (youme-regexp "\\(you\\|me\\)")
	 (regexp (sc-cite-regexp youme-regexp)))
    (if (re-search-forward regexp (regi-pos 'eol) 'move)
	(let ((oldprefix (sc-submatch 0))
	      newprefix)
	  (delete-region (match-beginning 0) (match-end 0))
	  (string-match (concat "\\(.*\\)" youme-regexp "\\(.*\\)") oldprefix)
	  (setq newprefix
		(concat (sc-submatch 1 oldprefix)
			(if (string= (downcase (sc-submatch 2)) "you")
			    "me" "you")
			(sc-submatch 3 oldprefix)))
	  (insert newprefix)
	  (sc-fill-if-different newprefix)
	  )))
  nil)

(defun sc-cite-new-line-with-you ()
  "Cite an uncited line with \"you>\"."
  (let ((prefix (concat sc-citation-leader "you" sc-citation-delimiter
			sc-citation-separator)))
    (insert prefix)
    (sc-fill-if-different prefix))
  nil)

(defun sc-cite-coerce-nested-citation ()
  "Coerce a nested citation line into a me/you line."
  (let* ((citation (sc-guess-nesting))
	 ;; even number of >'s means me, odd means you
	 (who (if (and citation
		       (= (% (length citation) 2) 0))
		  "you" "me"))
	 insert-p)
    (if (looking-at (sc-cite-regexp))
	(progn
	  (setq insert-p (/= (match-end 0) (regi-pos 'bonl)))
	  (delete-region (match-beginning 0) (match-end 0))))
    (if (not insert-p)
	(list (cons 'step 0))
      (insert sc-citation-leader who
	      sc-citation-delimiter
	      sc-citation-separator)
      nil)))

;;(defun sc-recite-paragraph ()
;;  "Recites the current paragraph.
;;The region need not be set, but point should be within a line of cited
;;text."
;;  (let* ((attrib (sc-guess-attribution))
;;	 (citation (sc-cite-regexp attrib))
;;	 (start (point))
;;	 (end (point)))
;;    ;; first find the beginning of the paragraph
;;    (regi-interpret
;;     (list
;;      (list citation '(list '(step . -1)))
;;      (list t '(progn
;;		 (setq start (point))
;;		 '('abort))))
;;     (point) (point-max))
;;    (goto-char start)
;;    ))
