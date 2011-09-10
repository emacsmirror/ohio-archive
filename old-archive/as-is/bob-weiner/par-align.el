From apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com Fri May 19 14:36:03 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 06:16:52 GMT
From: Bob Weiner <apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: par-align.el, you definitely want this one for GNU Emacs
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; FILE:         par-align.el
;; SUMMARY:      Improved paragraph fill and align functions
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc.
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    14-Apr-89
;; LAST-MOD:     28-Apr-89 at 00:25:08 by Bob Weiner
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   This package allows GNU Emacs paragraph commands to deal more
;;   reasonably with indented text and embedded comments.  It lets you
;;   realign and set the fill prefix on one line of a paragraph, e.g. the
;;   first, and then have all the other lines match up when you fill the
;;   paragraph.
;;   
;;   I think the 'fill-paragraph-and-align' command eliminates the need for
;;   Kyle Jones, 'C Comment Edit' package.  Additionally, there is no need
;;   to edit in a special buffer.
;;   
;;   If you set the fill-prefix to some value, then set it to nil (set it at
;;   the beginning of a line) and then fill the paragraph, the old
;;   fill-prefix will be stripped off and the paragraph will be filled
;;   properly with no prefix.  Then just add the new fill-prefix, if any,
;;   that you want and refill again.
;;   
;;   If you have ever tried to fill Emacs Lisp documentation strings or
;;   embedded comments, you know that all you get is an ugly mess.  You end
;;   up having to fill after putting blank lines at the beginning and end.
;;   Now you can fill properly without making any inline changes.  The fill
;;   routines do not fill the first line of documentation strings since it is
;;   used by the apropos commands and may intentionally be short.
;;   
;;   The fill and movement commands also recognize multiple paragraphs
;;   within a single comment stretch, just like the ones you see in this
;;   description text.  Just remember to set the fill prefix properly, just
;;   as you normally would for the fill-paragraph command.
;;   
;;   When filling a region, fill-prefix-prev (old fill prefix) can be
;;   replaced by fill-prefix throughout the entire region before each
;;   paragraph is filled.  Simply call the 'fill-region-and-align-all'
;;   command.
;;   
;;   Always fill with point before some of the text to be filled.  If you
;;   try to fill when point is on a line following all of the text to be
;;   filled, the next paragraph may be filled by mistake, e.g. when filling
;;   with point near the end of a comment string.
;;
;;   C comments of the form (with any number of '*' chars):
;;   /*
;;    * <text>
;;    * <text>
;;    */
;;   are filled well.
;;
;;   Multiple line C comments of the form:
;;   /*        */
;;   /*        */
;;   are not filled well, so don't use them.  If you have some already, you
;;   can convert them to the kind above with the
;;   'c-comment-make-prefix-only' command found in this package.
;;
;;   Fortran comments of the form:
;;   C
;;   C <text>
;;   C <text>
;;   C
;;   are filled well.
;;
;;   The commands bound immediately below do all of this work.
;;
;; DESCRIP-END.


;; Suggested key bindings
;;
(global-set-key "\M-j" 'fill-paragraph-and-align)
(global-set-key "\C-x\C-j" 'fill-region-and-align-all)
(define-key text-mode-map "\C-x\C-j" 'fill-region-and-align)
(define-key indented-text-mode-map "\C-x\C-j" 'fill-region-and-align)
(define-key outline-mode-map "\C-x\C-j" 'fill-region-and-align)

;; The next two functions are suitable as replacements for the standard
;; paragraph movement commands if you want them to recognize paragraph
;; boundaries the way the fill functions below do.
;;
;; Suggested key bindings
;;
;;(global-set-key "\M-n" 'forward-para)
;;(global-set-key "\M-p" 'backward-para)

(defun forward-para (&optional arg)
  "Move forward to end of paragraph.  With ARG, do it arg times.
A line which  paragraph-start  matches either separates paragraphs,
if  paragraph-separate  matches it also, or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (paragraph-filter 'forward-paragraph arg))

(defun backward-para (&optional arg)
  "Move backward to start of paragraph.  With ARG, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line, except
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (paragraph-filter 'forward-paragraph (- arg)))


(defconst fill-prefix-prev nil
  "Previous string inserted at front of new line during filling, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix-prev)

;; Redefine this function so that it sets 'fill-prefix-prev' also.
(defun set-fill-prefix ()
  "Set the fill-prefix to the current line up to point.
Also sets fill-prefix-prev to previous value of fill-prefix.
Filling expects lines to start with the fill prefix and reinserts the fill
prefix in each resulting line."
  (interactive)
  (setq fill-prefix-prev fill-prefix
	fill-prefix (buffer-substring
		      (save-excursion (beginning-of-line) (point))
		      (point)))
  (if (equal fill-prefix-prev "")
      (setq fill-prefix-prev nil))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix cancelled")))

(defun fill-paragraph-and-align (justify-flag)
  "Fill current paragraph.  Prefix arg JUSTIFY-FLAG means justify as well.
Does not alter fill prefix on first line of paragraph.  Any whitespace
separated version of the fill-prefix, or fill-prefix-prev when fill-prefix is
nil, at the beginning of lines is deleted before the fill is performed.  This
aligns all lines in a paragraph properly after the fill-prefix is changed.
Works well within text, singly delimited C comments, Lisp comments and
documentation strings, and Fortran comments."
  (interactive "P")
  (paragraph-filter 'fill-para-align justify-flag))

(defun fill-region-and-align-all (justify-flag)
  "Fill each line in region.  Prefix arg JUSTIFY-FLAG means justify as well.
Replace any whitespace separated version of fill-prefix-prev with fill-prefix
in all lines of region.  This aligns all lines throughout the region properly
after the fill-prefix is changed.  Works well within text, singly delimited C
comments, Lisp comments and documentation strings, and Fortran comments.
See also documentation for fill-region-and-align."
  (interactive "P")
  (paragraph-filter 'fill-region-align justify-flag t))

(defun fill-region-and-align (justify-flag &optional align-all)
  "Fill each line in region.  Prefix arg JUSTIFY-FLAG means justify as well.
Optional ALIGN-ALL non-nil means replace fill-prefix-prev with fill-prefix in
all lines of region, otherwise, does not alter prefix in paragraph separator
lines and first lines of paragraphs.
Any whitespace separated version of fill-prefix, or fill-prefix-prev when
fill-prefix is nil, at the beginning of appropriate lines is removed before the
fill is performed.  This aligns all lines in a paragraph properly after the
fill-prefix is changed.  Works well within text, singly delimited C comments,
Lisp comments and documentation strings, and Fortran comments."
  (interactive "P")
  (paragraph-filter 'fill-region-align justify-flag align-all))

(defun paragraph-filter (func arg1 &optional arg2)
;;;                         LISP          C            Fortran
;;;                         ----------------------------------------
;;; comment-start           ";"           "/* "        nil (^[Cc*])        
;;; comment-start-skip      ";+ *"        "/\\*+ *"    "![ \t]*"
;;; comment-end             "" ([^J^L])   " */"        "" (^J)
;;;
  (let* ((*lsp* (or (eq major-mode 'emacs-lisp-mode)
		 (eq major-mode 'lisp-interaction-mode)
		 (eq major-mode 'scheme-mode)
		 (eq major-mode 'lisp-mode)))
	 (extra-para-sep
	   (concat "^"
		   ;; Don't change the '?' in the following line to a '+', it
		   ;; will break certain fill boundary conditions.
		   (if (eq major-mode 'fortran-mode) "[cC*]?")
		   "[ \t]*\\("
		   ;; Don't change the '?' in the following line to a '+', it
		   ;; will break certain fill boundary conditions.
		   (if comment-start-skip
		       (concat "\\(" comment-start-skip "\\)?"))
		   (if (eq major-mode 'c-mode)
		       "\\|\*+/?")
		   "\\)[ \t]*$"
		   (if *lsp* "\\|^[ \t]*\\([[(]\\|\"\\)")
		   ))
	 (paragraph-separate
	   (concat paragraph-separate "\\|" extra-para-sep))
	 (paragraph-start
	   (concat paragraph-start "\\|" extra-para-sep)))
    (if arg2
	(funcall func arg1 arg2)
      (funcall func arg1))))

(defun fill-region-align (justify-flag &optional align-all)
  (if align-all
      (replace-fill-str fill-prefix-prev fill-prefix))
  (save-excursion
    (let ((end (max (point) (mark))))
      (goto-char (min (point) (mark)))
      (while (and (not (eobp)) (< (point) end))
	(fill-para-align justify-flag align-all)
	(forward-paragraph)
	;; Forward to real paragraph end if not in lisp mode
	(or *lsp* (re-search-forward (concat "\\'\\|" paragraph-separate)))))))

(defun fill-para-align (justify-flag &optional leave-prefix)
  (save-excursion
    (end-of-line)
    ;; Backward to para begin
    (re-search-backward (concat "\\`\\|" paragraph-separate))
    (forward-line (if (looking-at extra-para-sep) 2 1))
    (let ((region-start (point)))
      (forward-line -1)
      (let ((from (point)))
	(forward-paragraph)
	;; Forward to real paragraph end if not in lisp mode
	(or *lsp* (re-search-forward (concat "\\'\\|" paragraph-separate)))
	(beginning-of-line)
	(or leave-prefix
	    (replace-fill-str
	      (or fill-prefix fill-prefix-prev)
	      "" nil region-start (point)))
	(fill-region-as-paragraph from (point) justify-flag)))))

(defun replace-fill-str (fill-str-prev fill-str &optional suffix start end)
  "Replace whitespace separated FILL-STR-PREV with FILL-STR.
Optional SUFFIX non-nil means replace at ends of lines, default is beginnings.
Optional arguments START and END specify the replace region, default is the
current region."
  (if fill-str-prev
      (progn (if (not start) (setq start (min (point) (mark))))
	     (if (not end)   (setq end   (max (point) (mark))))
	     (if (not fill-str) (setq fill-str ""))
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (goto-char (point-min))
		 (perform-replace
		   (concat
		     (if suffix nil "^")
		     "[ \t]*"
		     (regexp-quote
		       ;; Get non-whitespace separated fill-str-prev
		       (substring
			 fill-str-prev
			 (or (string-match "[^ \t]" fill-str-prev) 0)
			 (if (string-match
			       "[ \t]*\\(.*[^ \t]\\)[ \t]*$" fill-str-prev)
			     (match-end 1))))
		       "[ \t]*"
		       (if suffix "$"))
		   fill-str nil t nil))))))

(defun c-comment-make-prefix-only ()
  "Make multiply-delimited C comments in region singly delimited.
Converts comments of form:
/*  <text>  */      to    /*
/*  <text>  */             * <text>
                           * <text>
                           */"
  (interactive)
  (replace-fill-str "/*" " * ")
  (replace-fill-str "*/" "" t)
  (save-excursion
    (goto-char (min (point) (mark)))
    (insert "/*\n"))
  (save-excursion
    (goto-char (max (point) (mark)))
    (insert (if (bolp) " */\n" "\n */"))))

(provide 'par-align)


-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


