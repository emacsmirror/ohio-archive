;*****************************************************************************
;
; Filename:	tek-src-hilite.el
;
; Copyright (C) 1991  Ken Wood
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 1, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Author:		Ken Wood, <kwood@austek.oz.au>
; Organisation:		Austek Microsystems Pty Ltd, Australia.
; Released with permission from Austek Microsystems.
;
; Description:	Highlight comments in source code buffers. Highlighting is
;		updated on find-file and save-buffer.
;
;		Button styles may be customised by means of X11 resources.
;		The resource name to use is "src-comment".
;		See the file tek-style-utils.el for details.
;
;		You may need to modify the variable
;		tek-highlight-merge-comments if you change the highlighting
;		style away from the default. See the documentation for this
;		variable for further details.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-src-hilite.el,v 1.5 1991/11/21 02:58:53 kwood Exp $

(provide 'tek-src-hilite)
(require 'epoch-running)

; Put the whole thing inside a test to get it to compile under emacs.
(if running-epoch
    (progn
      
      (require 'tek-style-utils)

      (defvar tek-src-comment-foreground "blue3"
	"\
Foreground color used to highlight comments if no value is defined in
the X11 resources and the display device supports color. On monochrome
screens a different font is used in place of the different color.")
      
      (defvar tek-src-comment-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-src-comment-foreground)
	    (tek-build-style "src-comment"
			     nil nil
			     tek-src-comment-foreground (background)
			     (background) (foreground))
	  ; Otherwise, define the style to use a different font.
	  (tek-build-style "src-comment" nil (or tek-italic-bold-fixed-font
						 tek-bold-fixed-font
						 tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"\
Style or attribute used to display characters in source code comments.")


      ; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ; Do things the old way - using attributes.
      
	    (defvar tek-src-comment-style tek-src-comment-styleorattribute
	      "\
Style used for displaying comments in source code when attributes are
used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-src-comment-styleorattribute (reserve-attribute))
      
	    ;Bind the comment style to the comment attribute
	    (set-attribute-style tek-src-comment-styleorattribute
				 tek-src-comment-style)
	    ))


      (defvar tek-highlight-done-this-buffer nil
	"\
Buffer-local variable indicating whether any comments have been
highlighted in this buffer or not.")

      (defvar tek-highlight-comment-continue-regexp nil
	"\
Buffer-local variable used to decide when adjacent comments may be
considered a single block. A search string which allows only
whitespace between comments.")

      (make-variable-buffer-local 'tek-highlight-done-this-buffer)
      (make-variable-buffer-local 'tek-highlight-comment-continue-regexp)


      (defvar tek-highlight-merge-comments t
	"\
*If non-nil then adjacent comments which are separated only by
whitespace may be merged, i.e. highlighted by a single button which
runs from the start of the first comment to the end of the last
comment. This variable is t by default, as this results in a
significant speedup in syntaxes which have newline-terminated
comments. It should be set to nil if the comment highlighting style
makes whitespace visible. Underlining and changing the background
color are two things that do this.")


      ;
      ; Function which does the actual highlighting
      ;
      (defun tek-highlight-comments ()
	"\
Actual source code highlighting function. Called by
tek-highlight-comments-on-find and tek-highlight-comments-on-write."
	; Silently do nothing if there are no regexps to search with.
	(if (and syndecode-comment-start-regexp
		 syndecode-comment-end-regexp)
	    (let ((starting-point (point-min))
		  comment-start-begin
		  comment-start-end
		  comment-end-end)
	      (save-excursion
		(goto-char (point-min))
		; Algorithm is: search for start of a comment,
		; make sure it really is a comment; then highlight
		; from there to the end of the comment.
		;
		; First, find a comment-start sequence.
		(while (re-search-forward syndecode-comment-start-regexp
					  nil t)
		  (progn
		    (setq comment-start-begin (match-beginning 0))
		    (setq comment-start-end (match-end 0))
		    ; Check that the comment start sequence really does
		    ; indicate the start of a comment, and that it's not
		    ; inside a string etc.
		    (setq state (parse-partial-sexp
				 starting-point comment-start-end))
		    (if (nth 4 state)
			; Yes, this is really the start of a comment
			(progn
			  (goto-char comment-start-end)
			  ; Find the end of the comment by searching
			  ; for a comment terminating sequence
			  (re-search-forward syndecode-comment-end-regexp
					     nil t)
			  (setq comment-end-end (point))
			  ; Now, searches are faster than adding buttons,
			  ; so see if we can extend this button to cover any
			  ; following comments.
			  (while (and tek-highlight-merge-comments
				      (looking-at
				       tek-highlight-comment-continue-regexp))
			    (progn (re-search-forward
				    tek-highlight-comment-continue-regexp
				    nil t)
				   (re-search-forward
				    syndecode-comment-end-regexp
				    nil t)
				   (setq comment-end-end (point))))
			  ; Highlight the comment
			  (add-button comment-start-begin comment-end-end
				      tek-src-comment-styleorattribute)
			  ; Start the next syntax parse at the end of the
			  ; comment just processed.
			  (setq starting-point comment-end-end)
			  )))))
	      ; Set a flag to indicate there are highlighted comments in
	      ; this buffer.
	      (setq tek-highlight-done-this-buffer t)
	      ))) ; end of defun


      ; Function to be called by find-file-hooks.
      (defun tek-highlight-comments-on-find ()
	"\
Function to highlight all the comments in the current buffer. Intended
to be called by find-file-hooks."
	; Extract comment details from the current syntax table. This will
	; do nothing if this function has already been run in this buffer.
	(decode-syntax-table)
	(if syndecode-comment-start-regexp
	    (setq tek-highlight-comment-continue-regexp
		  (concat "[ \t\n]*\\(" syndecode-comment-start-regexp
			  "\\)")))
	(tek-highlight-comments))


      ; Function to be called by write-file-hooks.
      (defun tek-highlight-comments-on-write ()
	"\
Function to highlight all the comments in the current buffer. Intended
to be called by write-file-hooks."
	; Check to see if there is any highlighting currently in effect
	(if tek-highlight-done-this-buffer
	    ; If so, remove & redo highlighting.
	    (progn
	      ; Clean up first - saves memory
	      (clear-buttons)
	      (tek-highlight-comments)))
	; Have to return nil or write-file-hooks will get stuffed up.
	nil)


      )) ; end: running-epoch test
