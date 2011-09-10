;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LISP CODE DIRECTORY INFORMATION
;;;
;;; LCD Archive Entry:
;;; markup|Bill Richardson|willrich@cs.utah.edu|
;;; Minor mode for electronic editorial markup|
;;; 26-May-1993|version 1.01|~/epoch/markup.el.Z|
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:          markup.el
;;; Description:   Minor mode for electronic editorial markup.
;;; Requirements:  Epoch 4.2 or greater, compiled with DEFINE_CHANGE_FUNCTIONS.
;;; Author:        Bill Richardson (willrich@cs.utah.edu)
;;; Date:          April 9, 1993
;;; Copyright:     (C) Copyright 1993 William F. Richardson
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CHANGE HISTORY:
;;;
;;; 9-Apr-93  Version 1.0  willrich
;;;           First public release.
;;;
;;; 26-May-93 Version 1.01 willrich
;;;           Fixed bug in recognizing C-mode comments.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COPYRIGHT NOTICE
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 1, or (at your option) any later
;;; version.
;;;
;;; This program is distributed in the hope that it will be useful, but 
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SUMMARY
;;;
;;; The markup package provides Epoch users the ability to make comments
;;; or changes to a document, while keeping those changes logically
;;; separate from the original text.  Several commands are supplied to
;;; operate on the marked text.  A minor mode is provided which will
;;; insure that the original text remains unchanged.  The package is
;;; written in Emacs Lisp, and requires Epoch 4.2 or later, compiled with
;;; DEFINE_CHANGE_FUNCTIONS defined.
;;;
;;; The markup package was written as an aid in publishing.  Often when
;;; an article is being written, part of the process is to have a
;;; colleague review the document and suggest changes.  Usually this is
;;; done by giving a printed copy of the article to the reviewer who
;;; marks all over it, and then returns it to the author.  The author
;;; then has to manually incorporate the comments and suggestions into
;;; the original document.
;;;
;;; With this package, the reviewing process is handled electronically.
;;; The rough draft of the article is sent to the reviewer directly, who
;;; edits the file while in markup mode.  This prevents him/her from
;;; accidently changing the original contents, while displaying all the
;;; suggested changes and deletions in different colors.  The modified
;;; file is saved and returned to the author.  The author can quickly
;;; move from one suggestion to another, and may incorporate the changes
;;; into the article with a minimum of effort.  The zone information
;;; which allows the author to see the reviewer's changes is saved as
;;; part of the file in a (hopefully) transparent fashion.
;;;
;;; Refer to the function documentation for 'markup-mode for more information.
;;;
;;; Binding markup functions to keys is done at the end of this file. I
;;; prefer the prefix "\C-c", but others may not.  Feel free to change it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTALLATION
;;;
;;; To install the markup package, just load it from your .emacs file. You
;;; should test for Epoch, since the package won't work under Emacs:
;;;
;;;     (when (boundp 'epoch::version)
;;;        (load "markup" t))
;;;
;;;
;;; You may also wish to change the default markup styles:
;;;
;;;     (when (and (boundp 'epoch::version)
;;;                (load "markup" t))
;;;        (setq markup::insert-style
;;;              (markup::make-style '(("background" . "green")
;;;                                    ("foreground" . "yellow"))))
;;;        (setq markup::delete-style
;;;              (markup::make-style '(("background" . "red")
;;;                                    ("foreground" . "white")))))
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)					; distributed with Emacs
(provide 'markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff to deal with minor mode.
;;;

;; Function to toggle or set markup-mode
(defun markup-mode (&optional arg)
  "Markup is a minor mode which allows editorial markup of files.
An optional ARG turns markup-mode on if value is greater than 0, else off.
In markup mode, the original text cannot be modified, but new text is
inserted in a different style.  Several commands are provided to mark
portions of the text in various ways, whether in markup mode or not, but
some are restricted while in markup mode to prevent modifying original text.

The markup commands bound to keys are C-c followed by:
\\{markup::command-prefix-key}
While in markup mode, comments may be inserted with M-;

There are five types of markup regions. These are used by the reviewer to
suggest changes to the original author.  The author may ignore the
suggestion completely, or may Accept or Destroy it.  The key bindings for
these actions are deliberately made difficult to type.  The functions
markup-accept-all, markup-accept-region, markup-destroy-all, and
markup-destroy-region may be used instead of keys.


Inserted text -- This is new text which the reviewer has inserted. If this
  change is Accepted, the text marked for insertion will be marked as
  original text.  If this change is Destroyed, the new text is removed from
  the buffer.

Deleted text -- This is text that the reviewer suggests be deleted. If
  this change is Accepted, the text marked for deletion will be removed
  from the buffer.  If this change is Destroyed, the text will be marked as
  original.

Comment text -- This is text which contains comments from the reviewer to
  the author.  It should not be part of the final document. If the buffer
  has a defined comment syntax, the markup::insert-comment function will
  attempt to use that syntax to delimit the reviewer's comments.  Comments
  cannot be Accepted.  They can only be Destroyed or marked as original.

Replacement text -- This is text that has replaced original text.  This
  should primarily be used to correct misspelled words, since massive
  rewriting is not the job of the reviewer.  If this change is Accepted,
  it becomes original text.  If it is Destroyed, the original text is
  restored in place of the text marked as replacement.  Note that markup
  mode will only allow one word at a time to be replaced.  There is a
  limit to how much of the original text section is saved along with each
  replacement text.  The amount is controlled by markup::save-text-length.

Highlighted text -- This has no editorial meaning, but is often useful as
  a place holder, or to draw attention to specific sections.


Purpose of the markup package:

The markup package allows users to make changes or additions to files in
a way that makes the changes immediately obvious to another user, and
also allows those changes to be selectively incorporated or ignored.
However, files edited with the markup package may still be edited with
editors other than Epoch.

Describing how the package may be useful is best done through an example:

Suppose I'm writing a paper to be published.  I get most of it written,
and then give a printed copy to a friend to review.  My friend writes
comments in the margins, crosses out words or paragraphs, inserts new
text in a few places, and returns this mess back to me.  I then have to
go through the document and transcribe any changes into my source file.

With the markup package, I can just email my friend the source file.  She
would make her comments, additions, and deletions with Epoch using markup
mode, and email the changed file back to me.  When I edit this new
version in my Epoch buffer, I see her changes in different colors.  For
example, her comments might be displayed in blue, her additions to my
text in green, and her suggestions as to what to delete in red.  All of
my original text would still be displayed in my default colors.

As I scan forward in the file, the colors make it easy to spot her
suggestions.  There are also commands to move forward or backward to the
next suggestion.  At each place, I can quickly accept, reject, or ignore
her changes.  If I accept her change, a single command will make the
correct changes to my file, by either deleting, replacing, or inserting
the suggested version.  I don't have to retype anything.  If I reject a
change, another command will discard inserted text or re-mark suggested
deletions as original text.  I don't have to delete or rephrase anything
here either.

The markup mode functions may be useful for just myself as well.  When my
friend is editing the file in markup mode, she is prevented from actually
changing my original text, and all of her comments and rephrasings appear
in different colors.  If I edit a file with markup mode disabled, I can
still mark sections of text for deletion, conditionally insert text, or
add comments to myself.  This has no effect on the contents of the text
file, so I can still print it out, or run it though LaTeX, or whatever.
However, by marking sections in different colors, I can quickly find
places in the file where I may wish to rethink some things.  So the
markup package my be useful while composing articles as well.


Saving markup information:

Loading the markup package installs functions to save and restore markup
information automatically.  When a file is written, the markup data is
saved by appending text to the end of the file.  If the buffer has a
comment syntax defined, the markup information is commented out according
to that syntax.  When a file is read, it is searched for the markup
information and the appropriate zones are restored.

Further documentation is available through comments in the source file."
  
  ;; Here's the body of the markup-mode function
  (interactive)
  (setq markup-mode
        (if (null arg) (not markup-mode)
          (> (prefix-numeric-value arg) 0)))
  (cond
   (markup-mode					; just turned it on
    (setq before-change-function 'markup::before-change)
    (setq after-change-function 'markup::after-change)
    (markup::turn-on-comment-key))
   (t						; just turned it off
    (setq before-change-function nil)
    (setq after-change-function nil)
    (markup::turn-off-comment-key))))


;; Variable to track state of markup-mode in each buffer.
(defvar markup-mode nil
  "Flag which is true when markup-mode is enabled.
See also markup-mode function documentation.")
(make-variable-buffer-local 'markup-mode)
(setq-default markup-mode nil)

;; Make before and after function hooks local to the buffer.
(make-variable-buffer-local 'before-change-function)
(make-variable-buffer-local 'after-change-function)

;; Register markup-mode as a minor mode.
(or (assq 'markup-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(markup-mode " Markup")
		minor-mode-alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set up default styles for each type of text.  Suggested changes are
;;; indicated by regions of text which appearing in different styles.
;;; Epoch refers to these regions as zones.
;;;

;; Creates a style from an alist of properties
(defun markup::make-style (styles)
  "Makes a new style with the parameters given in alist.

Each component of the alist looks like (\"style\" . \"value\").

For example, to set the foreground color to red and the font to 9x15bold, use
        '((\"foreground\" . \"red\") (\"font\" . \"9x15bold\"))

The \"style\" is in fact concatenated onto \"set-style-\" and this function
is called to set the style value.

The allowable style parameters are background, background-stipple,
cursor-background, cursor-foreground, cursor-stipple, font, foreground,
pixmap, stipple, underline and tag.  See the corresponding set-style-*
functions for their effects."
  
  (let ((new-style (make-style)))
    (while styles
      (funcall (intern
		(concat "set-style-" (car (car styles))))
	       new-style (cdr (car styles)))
      (setq styles (cdr styles)))
    new-style))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User variable definitions.
;;;

;;;
;;; Styles for various types of text.  These are customized for a screen
;;; which by default has yellow text on a navy blue background.
;;;

(defvar markup::insert-style
  (markup::make-style '(("foreground" . "green")))
  "*Markup style for inserted text.")

(defvar markup::delete-style
  (markup::make-style '(("foreground" . "red")))
  "*Markup style for deleted text.")

(defvar markup::replace-style
  (markup::make-style '(("foreground" . "white")))
  "*Markup style for replaced text.")

(defvar markup::comment-style
  (markup::make-style '(("foreground" . "LightSkyBlue")))
  "*Markup style for comment text.")

(defvar markup::highlight-style
  (markup::make-style '(("background" . "gray70")))
  "*Markup style for highlighted text.")

;;;
;;; What happens when comments are inserted.
;;;

(defvar markup::comment-newline-before t
  "*True if comments should insert newline first.")
(make-variable-buffer-local 'markup::comment-newline-before)

(defvar markup::comment-newline-after nil
  "*True if comments should insert newline afterwards.")
(make-variable-buffer-local 'markup::comment-newline-after)

(defvar markup::comment-begin-at-eol t
  "*True if comments should start at the end of the current line.")
(make-variable-buffer-local 'markup::comment-begin-at-eol)

;;;
;;; How inserted text is treated while in markup mode.
;;;

(defvar markup::insert-zone-soft t
  "*True if inserted text should try to match the preceding zone.
When new text is inserted at the very end of a comment zone,
it normally would not be part of the comment.  Setting this to T
checks to see if there is an adjacent zone to use instead.")
(make-variable-buffer-local 'markup::insert-zone-soft)

;;;
;;; How much replacement text should be saved with the file.
;;;

(defvar markup::save-text-length 500
  "*Maximum length of original text to be saved with replace zones.
Replacement text longer than this will not be saved along with the file.
Replacing large sections of text should not be the responsibility of the
reviewer.  To indicate that a major change is needed, insert a 
comment indicating that a section should be rewritten by the author.")
(make-variable-buffer-local 'markup::save-text-length)

;;;
;;; Should we advance to the next change after each Accept or Destroy?
;;;

(defvar markup::advance-after-change t
  "*True if point should advance after each Accept or Destroy.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal program variables.
;;;

(defconst markup::style-for-new-text 'insert
  "Style used to insert new text in markup mode.
This is an internal variable.  Don't screw with it.")
(make-variable-buffer-local 'markup::style-for-new-text)

(defvar markup::old-local-map nil
  "Holds local keymap in effect before entering markup mode.
This is an internal variable.  Don't screw with it.")
(make-variable-buffer-local 'markup::old-local-map)
(setq-default markup::old-local-map nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to deal with zones.
;;;
;;; Each markup suggestion is indicated by epoch zones.  The data field of
;;; each markup zone contains a vector of information about the suggestion.
;;; The vector in the zone data field contains one of these six patterns:
;;; 
;;;   [ 'markup-d  'delete ]                    ; Original, unchangeable text
;;;   [ 'markup-d  'highlight ]                 ; Original, unchangeable text
;;;   [ 'markup    'insert ]			; Changeable
;;;   [ 'markup    'comment ]			; Changeable
;;;   [ 'markup    'replace  nil ]		; Changeable, original lost
;;;   [ 'markup    'replace  "original text" ]	; Changeable, original saved
;;; 
;;; The original text of a replacement zone should only be lost if the file
;;; is saved without very long text-saving enabled, and then reloaded.
;;; It is not good style to replace large quantities of original text,
;;; since that is the responsibility of the author, not the reviewer.
;;; 


;; Create a new zone.  This is a wrapper around the epoch function so that
;; the buffer is marked as modified when zones are changed by markup.
(defun markup::add-zone (start end style &optional data buffer)
  "Wrapper around epoch::add-zone so buffer is modified by markup zones."
  (set-buffer-modified-p t)
  (add-zone start end style data buffer))


;; Move a zone.  This is a wrapper around the epoch function so that
;; the buffer is marked as modified when zones are changed by markup.
(defun markup::move-zone (zone &optional start end buffer)
  "Wrapper around epoch::move-zone so buffer is modified by markup zones."
  (set-buffer-modified-p t)
  (move-zone zone start end buffer))


;; Delete a zone.  This is a wrapper around the epoch function so that
;; the buffer is marked as modified when zones are changed by markup.
(defun markup::delete-zone (zone)
  "Wrapper around epoch::delete-zone so buffer is modified by markup zones."
  (set-buffer-modified-p t)
  (delete-zone zone))


;; Make a new zone.  The vectors for non-replace zones are constants,
;; but the replace zone vector also contains the old/original text.
;; A zero-length old-text argument means no replacement text is known.
(defun markup::make-zone (start end type &optional old-text)
  "Makes a new markup zone for a given region."
  (let ((zone (markup::add-zone start end nil)))
    (cond
     ((eq type 'delete)
      (set-zone-style zone markup::delete-style)
      (set-zone-data zone [ markup-d delete ]))
     ((eq type 'highlight)
      (set-zone-style zone markup::highlight-style)
      (set-zone-data zone [ markup-d highlight ]))
     ((eq type 'insert)
      (set-zone-style zone markup::insert-style)
      (set-zone-data zone [ markup insert ]))
     ((eq type 'comment)
      (set-zone-style zone markup::comment-style)
      (set-zone-data zone [ markup comment ]))
     ((eq type 'replace)
      (set-zone-style zone markup::replace-style)
      (set-zone-data zone (vector 'markup 'replace
				  (cond
				   ((null old-text) ; no text--get from buffer
				    (buffer-substring start end))
				   ((zerop (length old-text)) ; no string
				    nil)	; so no replacement
				   (t		; otherwise, use it
				    old-text))))))))
      

;; Returns T if zone is a markup zone
(defun markup::markup-zone-p (zone)
  "Returns T if a zone is a markup zone."
  (if zone					; if not zone, return nil
      (let ((data (zone-data zone)))		; get data field
	(and (vectorp data)			; must be vector
	     (or (eq 'markup (aref data 0))	; must be 'markup or 'markup-d
		 (eq 'markup-d (aref data 0))))))) ; in first element


;; Returns T if zone is a changeable markup zone
(defun markup::changeable-p (zone)
  "Returns T if zone is a changeable markup zone."
  (let ((data (zone-data zone)))		; get data field
    (and (vectorp data)				; must be vector
	 (eq 'markup (aref data 0)))))		; must be 'markup in first elt.


;; Returns T if zone is a non-changeable markup zone
(defun markup::original-p (zone)
  "Returns T if zone is a non-changeable markup zone."
  (let ((data (zone-data zone)))		; get data field
    (and (vectorp data)				; must be vector
	 (eq 'markup-d (aref data 0)))))	; must be 'markup-d in first


;; Returns T if zone is a markup zone that can be accepted
(defun markup::acceptable-p (zone)
  "Returns T if zone is a markup zone that can be accepted."
  (let ((data (zone-data zone)))		; get data field
    (and (vectorp data)				; must be vector
	 (or
	  (and (eq 'markup-d (aref data 0))	; delete zone
	       (eq 'delete (aref data 1)))
	  (and (eq 'markup (aref data 0))	; insert
	       (eq 'insert (aref data 1)))
	  (and (eq 'markup (aref data 0))	; replace
	       (eq 'replace (aref data 1)))))))


;; Returns T if zone is a markup zone that can be destroyed
(defun markup::destroyable-p (zone)
  "Returns T if zone is a markup zone that can be destroyed."
  (let ((data (zone-data zone)))		; get data field
    (and (vectorp data)				; must be vector
	 (or (eq 'markup-d (aref data 0))	; original
	     (and (eq 'markup (aref data 0))	; or modified
		  (not (and			; except for
			(eq 'replace (aref data 1)) ; replacement zone
			(null (aref data 2))))))))) ; without original text



;; This returns a list of the markup zones.
(defun markup::zone-list ()
  "Returns a list of all markup zones in the current buffer."
  (let ((z-list nil))
    (dolist (z (zone-list))			; look at all zones
      (if (and (markup::markup-zone-p z)	; pick out markup zones
	       (/= (zone-start z) (zone-end z))) ; with some length
	  (push z z-list)))			; save 'em
    (reverse z-list)))				; return 'em in order


;; Stretch an adjacent zone if possible, or make a new one,
;; so that the region is a zone of the correct style.
(defun markup::insert-zone (start end type)
  "Ensures that a region is in a particular style."
  (let ((next-zone (zone-at (- start 1)))
	(style (cond
		((eq type 'insert) markup::insert-style)
		((eq type 'comment) markup::comment-style)
		((eq type 'replace) markup::replace-style)
		((eq type 'delete) markup::delete-style)
		((eq type 'highlight) markup::highlight-style))))
    (cond ((and next-zone			; if there is one to the left
		(eq style (zone-style next-zone))) ; and it's what we want
	   (markup::move-zone next-zone		; just stretch it to cover
			      (zone-start next-zone) end))
	  ((and (setq next-zone (zone-at end))	; if there is one to the right
		(eq style (zone-style next-zone))) ; and it's what we want
	   (markup::move-zone next-zone		; just stretch it to cover
			      start (zone-end next-zone)))
	  (t					; otherwise make a new one
	   (markup::make-zone start end type)))))


;; This is a soft version of markup::insert-zone.  If there is an
;; adjacent changeable markup zone, that zone is stretched instead of
;; using the given style.  This function is only called by
;; markup::after-change, when inserting into no zone at all.  This lets
;; text inserted at the end of a markup zone be in the same zone. It is
;; selectable with a user variable.
(defun markup::insert-zone-soft (start end type)
  "Suggests that a region be inserted in a particular style."
  (let ((next-zone (zone-at (- start 1)))
	(style (cond
		((eq type 'insert) markup::insert-style)
		((eq type 'comment) markup::comment-style)
		((eq type 'replace) markup::replace-style)
		((eq type 'delete) markup::delete-style)
		((eq type 'highlight) markup::highlight-style))))
    (cond ((and next-zone			; if there is one to the left
		(markup::changeable-p next-zone)) ; and it's changeable
	   (markup::move-zone next-zone		; just stretch it to cover
			      (zone-start next-zone) end))
	  ((and (setq next-zone (zone-at end))	; if there is one to the right
		(eq style (zone-style next-zone))) ; and it's what we want
	   (markup::move-zone next-zone		; just stretch it to cover
			      start (zone-end next-zone)))
	  (t					; otherwise make a new one
	   (markup::make-zone start end type)))))


;; This splits a zone into two parts.  It should only be called if
;; it is certain that the zone entirely overlaps the region specified.
(defun markup::split-zone (old-zone start stop)
  "Splits an existing zone into two parts."
  (let ((new-zone (markup::add-zone		; make new zone for right side
		   stop
		   (zone-end old-zone)
		   (zone-style old-zone)
		   (zone-data old-zone))))
    (set-zone-transient new-zone		; copy the flags, too.
			(zone-transient-p old-zone))
    (set-zone-read-only new-zone
			(zone-read-only-p old-zone))
    (markup::move-zone old-zone			; move original zone to left
		       (zone-start old-zone) start)))


;; This attempts to be polite about other zones, by moving them or
;; splitting them into two parts before creating a new zone in the middle.
;; This will only be called after inserting new text into a zone, so
;; we don't have to worry about partially or completely covered zones.
(defun markup::split-and-insert-zone (start stop type)
  "Splits a zone or region so that a markup zone appears inside it."
  (let ((old-zone (zone-at start))
	(new-zone nil))
    (if (= start (zone-start old-zone))		; if start is start of old zone
	(markup::move-zone old-zone		; just move old one to the end
			   stop		
			   (zone-end old-zone))
      (markup::split-zone old-zone start stop))	; else split it in two
    (markup::insert-zone start stop type)))	; make new one for me.


;; This removes or splits or stretches the zones in a list until
;; the specified region contains no zones.  Each zone in the list
;; must actually be visible from within the region.
(defun markup::zap-zones-in-region (the-zones start stop)
  "Removes all zones in a list from a given region."
  (dolist (z the-zones)				; look at each one
    (cond ((and (>= (zone-start z) start)
		(<= (zone-end z) stop))		; if entirely inside
	   (markup::delete-zone z))		; then delete it
	  ((>= (zone-start z) start)		; if overlaps on right
	   (markup::move-zone z stop (zone-end z))) ; move it to the right
	  ((<= (zone-end z) stop)		; if overlaps on the left
	   (markup::move-zone z (zone-start z) stop)) ; move it to the left
	  (t					; else it covers entire region
	   (markup::split-zone z start stop))))) ; so split it into two parts


;; This returns T if there are changeable markup zones contained
;; in a list of zones.
(defun markup::changeable-zones-in (the-zones)
  "Returns T if there are changeable zones in a zone list."
  (cond ((null the-zones)			; if empty list, nil
	 nil)
	((markup::changeable-p (car the-zones)) ; if found one, t
	 t)
	(t					; else keep looking
	 (markup::changeable-zones-in (cdr the-zones)))))


;; This returns zones which may be affected by a text change in a given
;; region.  The Epoch function zones-in-region does not detect zones with
;; both endpoints outside the region.  If any zones completely span the
;; region, then adding the zones visible at the starting point to the
;; list within the region will provide all the affected zones.
(defun markup::affected-zones (start end)
  "Returns a list of zones visible in region."
  (let ((zones-at-start (zones-at start))
	(zone-list (zones-in-region start end t)))
    (while zones-at-start
      (if (not (memq (car zones-at-start) zone-list))
	(setq zone-list (cons (car zones-at-start) zone-list)))
      (setq zones-at-start (cdr zones-at-start)))
    zone-list))


;; Mark a region with a given style
(defun markup::mark-region-as-style (start stop style)
  "Marks a region as belonging to a given style."
  (let ((the-zones (markup::affected-zones start stop)))
    (if (markup::changeable-zones-in the-zones)
	(error "Region overlaps multiple markup zones."))
    (markup::zap-zones-in-region the-zones start stop)
    (markup::insert-zone start stop style)))


;; returns T if region contains original text
(defun markup::bad-zone-to-change (start stop)
  "Returns T if region contains original text."
  (let (flag-str
	beg
	end
	j
	(the-zones (markup::affected-zones start stop))) ; get list of zones
    (if the-zones				; if got a list,
	(progn
	  (setq flag-str (make-string (- stop start) ?x)) ; make a flag string
	  (dolist (zone the-zones)		; look at each zone
	    (when (markup::changeable-p zone)	; if it's okay to change
	      (setq beg (- (max (zone-start zone) start) start))
	      (setq end (- (min (zone-end zone) stop) start))
	      (setq j beg)
	      (dotimes (i (- end beg))		; clear section of flag-str
		(aset flag-str j ?o)
		(incf j))))
	  (string-match "x" flag-str))		; if any x's left, it's bad
      t)))					; else, original text == bad




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These are the functions that handle implicit text changes.  They deal
;;; with text that is inserted or deleted by the normal Epoch editing
;;; commands, such as delete-char, yank, kill, undo, etc.  They are
;;; enabled in markup mode, and are the primary difference between markup
;;; mode and regular mode.  These won't work if DEFINE_CHANGE_FUNCTIONS
;;; was not #defined when Epoch was compiled.
;;;


;; This is called before any text is changed.  If the operation is an
;; insertion, it does nothing.  If the operation is a deletion or a
;; replacement, then it is only allowed if the affected text lies
;; entirely inside Replace, Comment, or Insert markup zones.  Deleting
;; or replacing text from the original document (which include text
;; inside a Delete zone) is not allowed while in markup mode.
(defun markup::before-change (pos delpos) 
  "Called automatically before any text changes in markup mode.
It is active while in markup mode, and inactive otherwise.  It detects
delete and replace operations and stops them if they will affect
original text."
  (if (and (/= pos delpos)			; if deletion or replacement
	   (markup::bad-zone-to-change pos delpos)) ; and shouldn't be changed
      (error "You can't modify original text while in markup mode.")))


;; This is called after any text is changed.  Since all Delete and
;; Replace operations which affect original text will be caught and
;; stopped before the change occurs, this only has to deal with Insert
;; operations which may require splitting original text and inserting a
;; new zone.
(defun markup::after-change (pos inspos dellen) 
  "Called automatically after each text change in markup mode.
It is active while in markup mode, and inactive otherwise.  Inserted
text is placed in a new markup zone if necessary.  Deleted and
replaced text is prevented from affecting original text by the
markup::before-change function."
  (let (start-zone)
    (condition-case err
	(cond					; see what just happened
	 
	 ;; INSERTION:
	 ;; If inserting text in a changeable markup zone, fine.
	 ;; Otherwise, make a new zone containing the inserted text,
	 ;; splitting or moving other zones if necessary.
	 ((= dellen 0)				; if insertion
	  (setq start-zone (zone-at pos))
	  (cond
	   ;; inserting inside no zone at all -- make or strech one.
	   ((null start-zone)
	    (if markup::insert-zone-soft
		(markup::insert-zone-soft pos inspos
					  markup::style-for-new-text)
	      (markup::insert-zone pos inspos markup::style-for-new-text)))
	   
	   ;; inserting inside a changeable markup zone -- do nothing
	   ((markup::changeable-p start-zone)
	    nil)
	   
	   ;; else it must be inserting in some other zone -- split it
	   (t
	    (markup::split-and-insert-zone pos inspos
					   markup::style-for-new-text))))
	 
	 ;; REPLACEMENT:
	 ((= dellen (- inspos pos))		; if replacement
	  nil)					; do nothing
	 
	 ;; DELETION:
	 ((= pos inspos)			; if deletion
	  nil)					; do nothing
	 
	 ;; SOMETHING UNEXPECTED:
	 ;; I can't think what might happen to alter the text without
	 ;; using an insertion, deletion, or replacement.  If something
	 ;; does occur, print a message about it and hope for the best.
	 (t
	  (beep)
	  (message "strange change at %s: ins %s, del %s"
		   pos inspos dellen)))
      (error
       (beep)
       (message "*After change error: %s" (prin1-to-string err))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These are functions to change and mark text explicitly.  They are
;;; always active, in markup mode or not.  Since original text should not
;;; be modified in markup mode, some of these functions are limited when
;;; markup mode is active.
;;;

;; Insert a comment zone
(defun markup::insert-comment ()
  "Inserts a markup comment. If the major mode has a comment syntax
defined, the insertion is delimited by the appropriate characters. The
variables markup::comment-begin-at-eol, markup::comment-newline-before,
and markup::comment-newline-after control how the comment is inserted."
  (interactive)
  (let ((c-start (if comment-start comment-start ""))
	(c-end (if comment-end comment-end ""))
	pos)
    (unwind-protect
	(progn
	  (setq markup::style-for-new-text 'comment)
	  (if markup::comment-begin-at-eol (end-of-line))
	  (if markup::comment-newline-before (insert "\n"))
	  (insert c-start " ")
	  (setq pos (point))
	  (insert " " c-end)
	  (if markup::comment-newline-after (insert "\n"))
	  (goto-char pos))
      (setq markup::style-for-new-text 'insert))))


;; Mark a region as highlighted text
(defun markup::mark-region-as-highlight (start stop)
  "Marks the region as text to be highlighted.
This has no editorial meaning, but is occasionally useful."
  (interactive "r")
  (markup::mark-region-as-style start stop 'highlight))


;; Mark a region as deleted text.
(defun markup::mark-region-as-delete (start stop)
  "Marks the region as text to be deleted."
  (interactive "r")
  (markup::mark-region-as-style start stop 'delete))


;; Mark from point to eol as deleted text.
(defun markup::mark-to-eol-as-delete ()
  "Marks from point to the end of the line as deleted text."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (end-of-line)
      (markup::mark-region-as-style pos (point) 'delete))))


;; Mark a single word as deleted text.  This is useful for replacing
;; one word with another, which is a common need.
(defun markup::mark-word-as-delete ()
  "Mark the word under point for delete, then move to end of the word.
If point is at the end of a word, the region marked for deletion is from
point to the end of the next word.  The correct way to change a word is
to mark it for deletion and then insert the new word immediately after.
This command marks the current word and moves to the end so new text can
be inserted.  By repeating this command, several words can be marked."
  (interactive)
  (let (start stop)
    (cond
     ((or (looking-at "\\w")		; on a word now
	  (looking-at "\\b\\w"))	; or at the beginning of it
      (re-search-backward "\\b" nil)	; go to start of it
      (re-search-forward "\\w*\\b" nil)	; go to end of it
      (setq start (match-beginning 0))	; and mark it
      (setq stop (match-end 0))
      (markup::mark-region-as-style start stop 'delete)
      (goto-char stop))			; and go to end
     ((and (looking-at "\\b")		; else if at the end of a word
	   (setq start (point))		; (remember where I am)
	   (re-search-forward "\\w+\\b" nil)) ; and if another word follows
      (setq stop (match-end 0))		; then mark it
      (markup::mark-region-as-style start stop 'delete)
      (goto-char stop))			; and go to end
     (t
      (error "Can't locate word under or before point.")))))



;; Mark a region as replacement text -- not allowed in markup mode.
(defun markup::mark-region-as-replace (start stop)
  "Marks the region as replaced text.  Disabled in markup mode."
  (interactive "r")
  (if markup-mode
    (error "You can't modify original text while in markup mode.")
    (markup::mark-region-as-style start stop 'replace)))


;; Mark a region as inserted text -- not allowed in markup mode.
(defun markup::mark-region-as-insert (start stop)
  "Marks region as inserted text.  Disabled in markup mode."
  (interactive "r")
  (if markup-mode
      (error "You can't modify original text while in markup mode.")
    (markup::mark-region-as-style start stop 'insert)))


;; Mark a region as comment text -- not allowed in markup mode.
(defun markup::mark-region-as-comment (start stop)
  "Marks a region as a comment.  Disabled in markup mode."
  (interactive "r")
  (if markup-mode
    (error "You can't modify original text while in markup mode.")
    (markup::mark-region-as-style start stop 'comment)))


;; Mark a single word as replacement text.  This is useful for changing
;; misspelled words, so it is allowed even while in markup mode.  This is
;; the only case where original text can be changed in markup mode.
(defun markup::mark-word-as-replace ()
  "Mark the word under or before point for replacement.
Used primarily to correct misspelled words.  This is the only function
that allows changes to original text while in markup mode."
  (interactive)
  (let (start stop)
    (save-excursion
      (if (or (looking-at "\\w")
	      (re-search-backward "\\w" nil))
	  (progn
	    (re-search-backward "\\b" nil)
	    (re-search-forward "\\w*\\b" nil)
	    (setq start (match-beginning 0))
	    (setq stop (match-end 0))
	    (markup::mark-region-as-style start stop 'replace))
	(error "Can't locate word under or before point.")))))


;; Mark current zone as original -- mostly for use outside of markup-mode
(defun markup::mark-change-as-original ()
  "Marks the current markup zone as original text.
It won't let you make additions to original text while in markup mode."
  (interactive)
  (let ((z (zone-at)))
    (cond ((null z)				; original -- do nothing
	   (message "The text under point is already original."))
	  ((markup::changeable-p z)		; changeable zone -- maybe
	   (if markup-mode
	       (error "You can't modify original text while in markup mode.")
	     (markup::delete-zone z)))
	  ((markup::original-p z)		; delete zone -- okay
	   (markup::delete-zone z)))))


;; Accept changes suggested by current markup zone -- disabled in markup mode.
(defun markup::accept-change ()
  "Implements the current markup change.  Disabled in markup mode.
Text marked for deletion is deleted.  Comments are unchanged.
Text marked for insertion or replacement is re-marked as original.
If markup::advance-after-change is true, point then advances to the
next suggestion."
  (interactive)
  (if markup-mode
      (error "You can't modify original text while in markup mode."))
  (let ((z (zone-at)))
    (cond ((null z)				; no zone -- do nothing
	   nil)
	  ((markup::changeable-p z)		; changeable zone
	   (if (eq 'comment (aref (zone-data z) 1)) ; if comment
	       (error "%s%s" "Comments can't be Accepted.  They can only be "
		      "Destroyed or marked as original.")
	     (markup::delete-zone z)))		; else mark as original
	  ((markup::original-p z)		; delete or highlight zone
	   (if (eq 'delete (aref (zone-data z) 1))
	       (kill-region (zone-start z) (zone-end z)) ; kill deletes only
	     (error "%s%s" "Highlit text can't be Accepted. It can only be "
		    "Destroyed or marked as original.")))))
  (if (and markup::advance-after-change		; skip to next one
	   (not (markup::markup-zone-p (zone-at)))) ; unless I'm already on it
      (markup::next-change)))
			    

;; Deletes changes suggested by current markup zone.
(defun markup::destroy-change ()
  "Destroys the current markup change.
Text marked for deletion is re-marked as original.  Comments and
inserted text are deleted.  Replacement text reverts to the original.
If markup::advance-after-change is true, point then advances to the
next suggestion."
  (interactive)
  (let ((z (zone-at)))
    (cond ((null z)				; no zone -- do nothing
	   nil)
	  ((markup::original-p z)		; original text -- unmark it
	   (markup::delete-zone z))
	  ((markup::changeable-p z)		; changeable zone -- kill it
	   ;; replace zones should revert to original text
	   (if (eq 'replace (aref (zone-data z) 1))
	       (if (setq old-string (aref (zone-data z) 2))
		   ;; if we've got original text to replace it
		   (let ((prev-state markup-mode)) ; check current mode
		     (unwind-protect		; and restore later
			 (progn
			   (markup-mode 0)	; insert as original text
			   ;; kill suggested text
			   (kill-region (zone-start z) (zone-end z))
			   ;; insert original text
			   (insert old-string))
		       (if prev-state (markup-mode 1)))) ; restore mode
		 ;; else don't have original text for replacement
		 (error "I don't know what text this replaced."))
	     ;; not replacement zone -- just kill it
	     (kill-region (zone-start z) (zone-end z))))))
  (if (and markup::advance-after-change		; skip to next one
	   (not (markup::markup-zone-p (zone-at)))) ; unless I'm already on it
      (markup::next-change)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to move to next or previous markup zone.
;;;

;; Move forward to the beginning of next markup zone.
(defun markup::next-change ()
  "Moves point to the beginning of the next markup zone.
Returns nil if there is no next zone, else returns new point value."
  (interactive)
  (let ((pos (point))
	(newpos nil)
	(the-zones (markup::zone-list)))	; ordered list of markup zones
    (if the-zones
	(while the-zones			; find first one > pos
	  (if (> (zone-start (car the-zones)) pos)
	      (progn
		(setq newpos (zone-start (car the-zones)))
		(setq the-zones nil))
	    (setq the-zones (cdr the-zones)))))
    (if newpos
	(goto-char newpos)
      (message "No markup zones after point."))
    newpos))					; return new position


;; Move backward to beginning of previous markup zone.
(defun markup::prev-change ()
  "Moves point to the beginning of the previous markup zone.
Returns nil if there is no previous zone, else returns new point value."
  (interactive)
  (let ((pos (point))
	(newpos nil)
	(the-zones (reverse (markup::zone-list)))) ; reverse ordered list
    (if the-zones
	(while the-zones			; find first one < pos
	  (if (< (zone-start (car the-zones)) pos)
	      (progn
		(setq newpos (zone-start (car the-zones)))
		(setq the-zones nil))
	    (setq the-zones (cdr the-zones)))))
    (if newpos
	(goto-char newpos)
      (message "No markup zones before point."))
    newpos))					; return new position


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions which affect all zones in a region.
;;;

;; accept all changes in region
(defun markup-accept-region (start end)
  "This Accepts all markup changes in the region.
Use caution. Not everything can be undone."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (let (found)
      (while (and (setq found (markup::prev-change))
		  (>= found start))
	(if (markup::acceptable-p (zone-at))
	    (markup::accept-change))))))


;; destroy all changes in region
(defun markup-destroy-region (start end)
  "This Destroys all markup changes in the region.
Use caution. Not everything can be undone."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (let (found)
      (while (and (setq found (markup::prev-change))
		  (>= found start))
	(if (markup::destroyable-p (zone-at))
	    (markup::destroy-change))))))


;; accept all changes in buffer
(defun markup-accept-all ()
  "This Accepts all markup changes in the current buffer.
Use caution. Not everything can be undone."
  (interactive)
  (when (y-or-n-p "Do you really want to Accept all changes? ")
    (markup-accept-region (point-min) (point-max))))


;; destroy all changes in buffer
(defun markup-destroy-all ()
  "This Destroys all markup changes in the current buffer.
Use caution. Not everything can be undone."
  (interactive)
  (when (y-or-n-p "Do you really want to Destroy all changes? ")
    (markup-destroy-region (point-min) (point-max))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff to save and reload zones.  Modified from save-zones.el,
;;; which is included with Epoch.
;;;
;;; These functions will enable zone information in a buffer to be saved as
;;; it is written to file, then restored when the file is loaded again.
;;; Markup information is stored at the end of the buffer, commented out
;;; appropriately according to the buffer's major mode.  If the major
;;; mode does not support comments, the information is still stored. The
;;; information being saved consists of the zone's start and end positions,
;;; the zone's style, and the line number of the zone start.  The line
;;; number is not used, but is included for those miserable wretches who
;;; can't use this package but still want a hint as to where the changes are.
;;; Also saved is the original text for replacment zones, provided the
;;; original text is not longer than markup::save-text-length.  In
;;; addition, the position of point after inserting the markup zone
;;; header is saved as part of the information. This is done so that if a
;;; mailer adds lines to the beginning of the file, the zones can be
;;; restored anyway.  Otherwise you'd have to strip off the mail header
;;; before you loaded the file.
;;;

;; Remove zone information from end of buffer.  If no-mod is t,
;; do not change the modification status of the buffer.
(defun markup::purge-zone-info (&optional no-mod)
  "Removes markup zone information from end of buffer.
If optional argument is non-nil, don't change modification status."
  (let ((prev-state markup-mode))		; save previous markup state
    (unwind-protect				; to restore later
	(let* ((c-start (if comment-start comment-start ""))
	       (c-end (if comment-end comment-end ""))
	       (start
		(concat "\n" c-start " *-* Markup Info Start *-*" c-end))
	       (finish
		(concat "\n" c-start " *-* Markup Info End *-*" c-end))
	       (mod-state-before (buffer-modified-p))
	       beg end)
	  (save-excursion
	    (goto-char (point-min))
	    (if (search-forward start nil t)
		(progn
		  (beginning-of-line)
		  (setq beg (- (point) 1))
		  (if (search-forward finish nil t)
		      (progn
			(setq end (1+ (point)))	; get final newline, too.
			(markup-mode 0)		; turn markup off for delete
			(delete-region beg end))))))
	  (if no-mod (set-buffer-modified-p mod-state-before)))
      (if prev-state (markup-mode 1)))))	; restore markup state


;; This is same as Emacs' what-line, but without printing the message.
(defun markup::what-line (pos)
  "Returns the line number of a given position."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
       (1+ (count-lines 1 (point))))))


;; Save original text from replace zone in a readable form.  This is
;; necessary to handle linefeed characters embedded in the text, which
;; do not work correctly for most languages if just embedded as text.
;; The string is represented as a list of strings, broken by linefeed
;; characters, which are represented with a symbol. Null strings are saved.
(defun markup::save-text (text c-start c-end)
  "Writes the original text from a replace zone into the buffer."
  (let ((i 0)
	(j 0)
	(n (- (length text) 1)))
    (insert "\n" c-start "(" c-end)		; always save something
    (if (and text				; even if it's an empty list
	     (< n markup::save-text-length))
	(progn
	  (while (setq i (string-match "\n" text j))
	    (if (/= i j)
		(insert "\n" c-start
			(prin1-to-string (substring text j i))
			c-end))
	    (insert "\n" c-start "NL" c-end)
	    (setq j (+ i 1)))
	  (if (< j n)
	      (insert "\n" c-start
		      (prin1-to-string (substring text j (+ n 1)))
		      c-end))))
    (insert "\n" c-start ")" c-end)))		; close the list


;; Save markup zones at end of the buffer.
(defun markup::save-zones ()
  "Saves markup zone information at the end of the buffer."
  (let ((here nil)				; scratch
	(prev-state markup-mode))		; save previous markup state
    (unwind-protect				; to restore later
	(let (( zlist (markup::zone-list))
	      (c-start (if comment-start comment-start ""))
	      (c-end (if comment-end comment-end "")))
	  (markup-mode 0)			; turn markup off temporarily
	  (markup::purge-zone-info)		; clear zone info from buffer
	  (when zlist				; only deal with markup zones
	    (save-excursion
	      (goto-char (point-max))
	      ;; begin markup section
	      (insert "\n" c-start " *-* Markup Info Start *-*" c-end)
	      (setq here (point))		; save offset for mail headers
	      (insert "\n" c-start "Ofs " (prin1-to-string here) " " c-end)
	      (dolist (z zlist)			; write out each zone
		(let ((start (zone-start z))
		      (end (zone-end z))
		      (z-data (zone-data z)))		      
		  (insert "\n" c-start		; write the basic zone info
			  (prin1-to-string
			   (list start end
				 (aref z-data 1)
				 'Line (markup::what-line start)))
			  c-end)
		  ;; write the original text for replace zones
		  (if (eq (aref z-data 1) 'replace)
		      (markup::save-text (aref z-data 2) c-start c-end))))
	      ;; end of markup section
	      (insert "\n" c-start " *-* Markup Info End *-*" c-end "\n"))))
      (if prev-state (markup-mode 1))))		; restore markup state
  nil)						; continue with write hooks


;; Retrieve original text for replace zone from buffer.
(defun markup::restore-text (c-start c-end)
  "Returns original text for a replace zone, as found in the buffer."
  (let ((out nil)
	(start (concat "\n" c-start))
	(end (regexp-quote (concat ")" c-end))))
    (search-forward (concat c-start "(" c-end) )
    (while (and (search-forward start nil t)
		(not (looking-at end)))
      (setq out (cons (read (current-buffer)) out))
      (backward-char 1))
    (mapconcat '(lambda (x) (if (stringp x) x "\n")) (reverse out) "")))


;; Restore zones to current buffer based on zone information at end of file.
;; This is normally called only when files are loaded.
(defun markup::restore-zones ()
  "Restores markup zones based on information at end of buffer."
  (condition-case nil
      (let ((c-start (if comment-start comment-start ""))
	    (c-end (if comment-end comment-end ""))
	    (offset 0)
	    look-here re-look-here here-now here-then
	    hunt-for start end data type old-text style)
	(setq hunt-for
	      (concat "\n" c-start " *-* Markup Info Start *-*" c-end))
	(setq look-here
	      (concat "\n" c-start "Ofs "))
	(setq re-look-here
	      (regexp-quote look-here))
	(save-excursion
	  (goto-char (point-min))
	  (when (search-forward hunt-for nil t)
	    ;; compute offset for mail headers
	    (when (looking-at re-look-here)
	      (setq here-now (point))
	      (search-forward look-here nil t)
	      (setq here-then (read (current-buffer)))
	      (setq offset (if (>= here-now here-then)
			       (- here-now here-then)
			     0)))
	    ;; now load all markup ranges
	    (while (search-forward "(" nil t)
	      (backward-char 1)
	      (setq data (read (current-buffer))) ; read data as list
	      (when (listp data)
		(setq start (+ offset (nth 0 data)))
		(setq end (+ offset (nth 1 data)))
		(setq type (nth 2 data))
		(setq old-text
		      (if (eq 'replace type)	; if replace zone, get text
			  (markup::restore-text c-start c-end)))
		(markup::make-zone start end type old-text)))))
	(markup::purge-zone-info t)		; remove zone stuff
	(set-buffer-modified-p nil))		; brand new file
    ;; catch errors here
    (error
     (beep)
     (message "Warning: Markup zone info is obsolete or garbled."))))


;;;
;;; Install hooks for automatic save/restore of markup zones.
;;;

(or (memq 'markup::save-zones write-file-hooks)
    (setq write-file-hooks (cons 'markup::save-zones write-file-hooks)))
(or (memq 'markup::restore-zones find-file-hooks)
    (setq find-file-hooks (cons 'markup::restore-zones find-file-hooks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set up bindings for markup functions.  This will change the global
;;; keymap so that the markup commands work even when not in markup mode.
;;; The only key that is rebound while in markup mode is the one to insert
;;; comments, and that is rebound locally.
;;;

;; Bind markup commands to keys following markup prefix
(define-prefix-command 'markup::command-prefix-key)
(define-key 'markup::command-prefix-key "k" 'markup::mark-to-eol-as-delete)
(define-key 'markup::command-prefix-key "d" 'markup::mark-region-as-delete)
(define-key 'markup::command-prefix-key "=" 'markup::mark-word-as-delete)
(define-key 'markup::command-prefix-key "h" 'markup::mark-region-as-highlight)
(define-key 'markup::command-prefix-key ";" 'markup::mark-region-as-comment)
(define-key 'markup::command-prefix-key "i" 'markup::mark-region-as-insert)
(define-key 'markup::command-prefix-key "r" 'markup::mark-region-as-replace)
(define-key 'markup::command-prefix-key "$" 'markup::mark-word-as-replace)
(define-key 'markup::command-prefix-key "o" 'markup::mark-change-as-original)
(define-key 'markup::command-prefix-key "A" 'markup::accept-change)
(define-key 'markup::command-prefix-key "D" 'markup::destroy-change)
(define-key 'markup::command-prefix-key "n" 'markup::next-change)
(define-key 'markup::command-prefix-key "p" 'markup::prev-change)
(define-key 'markup::command-prefix-key "\C-n" 'markup::next-change)
(define-key 'markup::command-prefix-key "\C-p" 'markup::prev-change)

;; Copy function definition into variable definition, too,
;; So that the documentation function can get at it.
(setq markup::command-prefix-key
      (symbol-function 'markup::command-prefix-key))
(put 'markup::command-prefix-key
     'variable-documentation
     "This is an internal variable.  Don't screw with it.")


;; Bind local key to insert a markup comment
(defun markup::turn-on-comment-key ()
  "This binds a key to insert markup comments."
  (setq markup::old-local-map			; old map is either
	(or (current-local-map)			; whatever is in use
	    (make-sparse-keymap)))		; or a blank one
  (use-local-map (copy-keymap markup::old-local-map))
  (if (lookup-key (current-local-map) "\M-;")
      (local-unset-key "\M-;"))
  (local-set-key "\M-;" 'markup::insert-comment))


;; Rebind comment key to original function
(defun markup::turn-off-comment-key ()
  "This rebinds the markup comment key to its original function."
  (when markup::old-local-map
    (use-local-map markup::old-local-map)
    (setq markup::old-local-map nil)))


;; Finally, install key bindings in the global key map, with C-c prefix.
;; Individual users may prefer to change this.  I like it here.
(define-key (current-global-map) "\C-c" 'markup::command-prefix-key)

;;;
;;; End of markup package.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
