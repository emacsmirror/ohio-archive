;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!yale!cs.yale.edu!briscoe-duke Fri Apr 20 11:12:40 EDT 1990
;Article 1539 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!yale!cs.yale.edu!briscoe-duke
;From: briscoe-duke@CS.YALE.EDU (Duke Briscoe)
;Newsgroups: gnu.emacs
;Subject: Re: Emacs and vi tags file
;Message-ID: <23289@cs.yale.edu>
;Date: 19 Apr 90 19:33:21 GMT
;References: <DLS.90Apr18125933@ccird1.uucp>
;Sender: news@cs.yale.edu
;Reply-To: briscoe-duke@CS.YALE.EDU (Duke Briscoe)
;Distribution: gnu
;Organization: Yale University Computer Science Dept, New Haven CT  06520-2158
;Lines: 126
;
;In article <DLS.90Apr18125933@ccird1.uucp> dls@ccird1.uucp (Darren Swartzendruber) writes:
;>What I am after is an emacs implementation of this.  It must read
;>vi tag files.  It must be context sensitive (when I go M-., it knows
;>that I am after the tags on which my curser is on).  Most of all,
;>it must be able to push tag refernces on a stack so they can be popped.
;
;I think the following code does what you want.  Gnu emacs tags are
;compatible with vi tag files I'm pretty sure.  I use the tagify.el
;package written by Dan LaLiberte to create tags files rather than the
;ctags or etags programs.  It makes it easier to customize what tags
;get generated.
;
;First, here is a short fix which keeps tag searches from massively
;reorganizing your buffer list, moving buffers to the top of the buffer
;list even when no match is found in the buffer.  I work with large
;sets of files and it messes up the context of my editing to have 30
;buffers piled on top of the main 3 or 4 buffers I really want to work
;with.
;
;===========================
;;; A modification of the standard GNU tags functions.

;;; This file should fix some problems with the way the tags functions
;;; work.  They shouldn't be selecting buffers in which they find
;;; nothing, it clutters up the buffer list.

(defun next-file (&optional initialize)
  "Select next file among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  (interactive "P")
  (if initialize
      (setq next-file-list (tag-table-files)))
  (cond (next-file-list
         (set-buffer (find-file-noselect (car next-file-list)))
         (setq next-file-list (cdr next-file-list)))
        (t (error "All files processed."))))

(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  (interactive)
  (if first-time
      (progn (next-file t)
	     (goto-char (point-min))))
  (while (not (eval tags-loop-form))
    (next-file)
    (message "Scanning file %s..." buffer-file-name)
    (goto-char (point-min)))
  (switch-to-buffer (current-buffer)))
;====================================================
;
;Here is a file which implements something I call trail markers.  It
;maintains a doubly linked list of markers, and allows one to move back
;and forth between buffers.  Not a stack, but perhaps more useful.  The
;function trail-tag-loop is like the M-, command, but leaves a marker
;at the location you were at before continuing your tags search.
;
;====================================================

;;; Author: Duke Briscoe briscoe@cs.yale.edu 4/5/90

;;; Using markers to switch between buffers.  Useful when doing
;;; tags-searches which take you across many files.

(defvar trail-marks nil "A doubly linked list of markers.")

;; The car of each element of trail-marks should point to the cons
;; whose car is the previous element, thus forming a doubly linked
;; list.

(defun mark-trail ()
  "Puts the current location on the trail-marks list."
  (interactive)
  (setq trail-marks (cons (cons nil (point-marker)) trail-marks))
  (if (cdr trail-marks) (setcar (car (cdr trail-marks)) trail-marks))
  (setq current-trail-mark trail-marks)
  (message "Trail mark set."))

(defun clear-trail ()
  "Sets the trail-marks and current-trail-mark to nil.  May help garbage collection."
  (interactive)
  (while trail-marks
    (set-marker (cdr (car trail-marks)) nil)
    (setq trail-marks (cdr trail-marks)))
  (setq current-trail-mark nil))

(defvar current-trail-mark nil "A pointer into trail-marks list.")

(defun next-trail-mark ()
  "Switch to the buffer and location of the trail mark after the
current trail mark."
  (interactive)
  (cond ((and current-trail-mark (car (car current-trail-mark)))
         (setq current-trail-mark (car (car current-trail-mark)))
         (goto-trail-marker (cdr (car current-trail-mark))))
        (t (error "No next trail mark."))))

(defun previous-trail-mark ()
  "Switch to the buffer and location of the trail mark before the
current trail mark."
  (interactive)
  (cond ((and current-trail-mark (cdr current-trail-mark))
         (setq current-trail-mark (cdr current-trail-mark))
         (goto-trail-marker (cdr (car current-trail-mark))))
        (t (error "No previous trail mark."))))

(defun goto-trail-marker (tm)
  "Switches to where trail marker is, perhaps another buffer."
  (let ((mb (marker-buffer tm)))
    (cond (mb
           (switch-to-buffer mb)
           (goto-char (marker-position tm)))
          (t (error "Marker's buffer has disappeared.")))))

(global-set-key "\M-#" 'mark-trail)
(global-set-key "\M-&" 'previous-trail-mark)
(global-set-key "\M-*" 'next-trail-mark)

(fset 'trail-tag-loop "\M-#\M-,")
(global-set-key "\M-`" 'trail-tag-loop)
;====================================================
;
;Duke Briscoe
;briscoe@cs.yale.edu
