;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs Tue May  8 15:00:41 1990
;Article 993 of gnu.emacs.bug:
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs
;>From lrs@indetech.com (Lynn Slater)
;Newsgroups: gnu.emacs.bug
;Subject: Pop mark across buffers
;Message-ID: <m0hPirJ-0000FBC@fire.indetech.com>
;Date: 30 Apr 90 22:04:00 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 313
;
;
;> When working in multiple-file programs, and using find-tag a lot, it
;> would be nice if there were an analogue to pop-mark within a buffer so
;> that you could pop back to where you were looking (like Info-last).
;
;Here is a rather old but still good change that allow this. I can browse
;functions following an execution thread and then "pop" the stack back to
;any previous node -- a great timesaver!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; location.el --- enhancements to find-tag that allow backtracking path.
;; Author          : Lynn Slater
;; Created On      : Wed Dec  2 14:19:18 1987
;; Last Modified By: Lynn Slater
;; Last Modified On: Fri Oct 28 14:41:00 1988
;; Update Count    : 8
;; Status          : Not cleaned up, but reliable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1988 Lynn Randolph Slater, Jr.
;; This file might become part of GNU Emacs.
;;
;; This file is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; this file so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.

;; Make this file location.el, byte-compile it in your path

(provide 'location)

(defvar user-location-list nil
  "Stores list of where the user was.
   Each entry is of the form (buffer-name . point)
   Entries may be added by the fcn record-user-location.
   Entries may be altered by the fcn rerecord-user-location.
   Entries may be revisited with revisit-last-user-location.
   Entries may be forgotten with forget-last-user-location.
   Entries may be undone by backtrack-to-last-user-location.

   This is intended to form a set of general utilities usefull for any
   modes or command sets that visit lots of buffer or files.  See the
   extended tags browsing system for examples.")

(defun reset-user-location-list ()
  "See the documentation on the variable user-location-list"
  (interactive)
  (setq user-location-list nil))

(defun record-user-location (&optional buffer-name point)
  "Stores the  buffer and point. Uses current buffer and point by default.
   The old location is pushed down on the stack. See
   user-location-list variable"
  (interactive)
  (setq user-location-list (cons (cons (or buffer-name (buffer-name))
				       (or point (point)))
				 user-location-list)))

(defun mark-and-record ()
  "Sets the mark but also records the current location so that you may
   return to this particular place through the
   backtrack-to-last-user-location command (bound to \\[backtrack-to-last-user-location])."
  (interactive)
  (record-user-location)
  (set-mark-command nil))

(defun mark-and-record ()
  "Sets the mark but also records the current location so that you may
   return to this particular place through the
   backtrack-to-last-user-location command (bound to \\[backtrack-to-last-user-location])."
  (interactive)
  (record-user-location)
  (set-mark-command nil))

(defun rerecord-user-location (&optional buffer-name point)
  "Stores the  buffer and point. Uses current buffer and point by default."
  (forget-last-user-location)
  (record-user-location))

(defun revisit-last-user-location ()
  "Switches buffer and point to the last values"
  (interactive)
  (switch-to-buffer (car (car user-location-list)))
  (goto-char (or (cdr (car user-location-list)) (point))))	  

(defun forget-last-user-location ()
  "Forgets last stored buffer and point. See last-user-location-list variable"
  (setq user-location-list (cdr user-location-list)))

(defun backtrack-to-last-user-location (&optional junk1 junk2 junk3 junk4)
  "Goes to last stored buffer and point. See last-user-location-list variable
   Also pops the location off of the stack
   Acts as (switch-to-buffer nil) if there is no stored last location."
  (interactive)
  (revisit-last-user-location)
  (forget-last-user-location))

;; user-location-list
;; (revisit-last-user-location)
;; (backtrack-to-last-user-location)
;; reset-user-location-list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To enhance tags, uncomment the following in this file or replace
;; find-tag in tags.el
;;    (Release 18.51)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-ok->(load "tags")

;;-ok->(defun find-tag (tagname &optional next other-window)
;;-ok->  "Find tag (in current tag table) whose name contains TAGNAME.
;;-ok-> Selects the buffer that the tag is contained in
;;-ok->and puts point at its definition.
;;-ok-> If TAGNAME is a null string, the expression in the buffer
;;-ok->around or before point is used as the tag name.
;;-ok-> If second arg NEXT is non-nil (interactively, with prefix arg),
;;-ok->searches for the next tag in the tag table
;;-ok->that matches the tagname used in the previous find-tag.
;;-ok->
;;-ok->See documentation of variable tags-file-name."
;;-ok->  (interactive (if current-prefix-arg
;;-ok->		   '(nil t)
;;-ok->		 (find-tag-tag "Find tag: ")))
;;-ok->  (let (buffer file linebeg startpos)
;;-ok->    (save-excursion
;;-ok->     (visit-tags-table-buffer)
;;-ok->     (if (not next)
;;-ok->	 (goto-char (point-min))
;;-ok->       (setq tagname last-tag))
;;-ok->     (setq last-tag tagname)
;;-ok->     (while (progn
;;-ok->	      (if (not (search-forward tagname nil t))
;;-ok->		  (error "No %sentries containing %s"
;;-ok->			 (if next "more " "") tagname))
;;-ok->	      (not (looking-at "[^\n\177]*\177"))))
;;-ok->     (search-forward "\177")
;;-ok->     (setq file (expand-file-name (file-of-tag)
;;-ok->				  (file-name-directory tags-file-name)))
;;-ok->     (setq linebeg
;;-ok->	   (buffer-substring (1- (point))
;;-ok->			     (save-excursion (beginning-of-line) (point))))
;;-ok->     (search-forward ",")
;;-ok->     (setq startpos (read (current-buffer))))
;;-ok->    (if (not next) (record-user-location)) ;; lrs
;;-ok->    (if other-window
;;-ok->	(find-file-other-window file)
;;-ok->      (find-file file))
;;-ok->    (widen)
;;-ok->    (push-mark)
;;-ok->    (let ((offset 1000)
;;-ok->	  found
;;-ok->	  (pat (concat "^" (regexp-quote linebeg))))
;;-ok->      (or startpos (setq startpos (point-min)))
;;-ok->      (while (and (not found)
;;-ok->		  (progn
;;-ok->		   (goto-char (- startpos offset))
;;-ok->		   (not (bobp))))
;;-ok->	(setq found
;;-ok->	      (re-search-forward pat (+ startpos offset) t))
;;-ok->	(setq offset (* 3 offset)))
;;-ok->      (or found
;;-ok->	  (re-search-forward pat nil t)
;;-ok->	  (error "%s not found in %s" pat file)))
;;-ok->    (beginning-of-line))
;;-ok->  (setq tags-loop-form '(find-tag nil t))
;;-ok->  ;; Return t in case used as the tags-loop-form.
;;-ok->  t)

(global-set-key  "\e\C-l" 'backtrack-to-last-user-location)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define xwindow support for ez code browsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;->(defun find-tag-at-point ()
;;->  "finds the tag at the point without user interaction.
;;->   If the tag is the same as the last tag, the next occurance of the
;;->   tag is found instead."
;;->  (let ((this-tag  (save-excursion
;;->		     (buffer-substring
;;->		      (progn (backward-sexp 1) (point))
;;->		      (progn (forward-sexp 1) (point))))))
;;->    (if (equal this-tag last-tag) ;; use string-matfch instead?
;;->	(find-tag last-tag t)
;;->      (find-tag this-tag))
;;->    ))
;;->
;;->(defun x-mouse-set-point-or-hunt (arg)
;;->  "Select Emacs window mouse is on, and move point to mouse position."
;;->  (let* ((relative-coordinate (x-mouse-select arg))
;;->	 (rel-x (car relative-coordinate))
;;->	 (rel-y (car (cdr relative-coordinate)))
;;->	 (old-p (point)))
;;->    (if relative-coordinate
;;->	(progn
;;->	  (move-to-window-line rel-y)
;;->	  (move-to-column (+ rel-x (current-column)))
;;->	  (if (eq (point) old-p);; click twice to find tag
;;->	      (find-tag-at-point)))
;;->      (progn
;;->	;;(x-scroll-window arg)
;;->	)
;;->      )))
;;->
;;->(defun x-scroll-window (arg)
;;->  (if (< (car arg) (/ (window-width) 2))
;;->      (scroll-down (/ (window-height) 2))
;;->    (scroll-up (/ (window-height) 2))))
;;->
;;->(defun x-mouse-select (arg)
;;->  "Select Emacs window the mouse is on."
;;->  (let ((start-w (selected-window))
;;->	(done nil)
;;->	(w (selected-window))
;;->	(rel-coordinate nil)
;;->	(arg2 (list (car arg) (- (car (cdr arg)) 1)))
;;->	)
;;->	;;(message "looking for select %s" arg) (sit-for 1)
;;->    (while (and (not done)
;;->		(null (setq rel-coordinate
;;->			    (coordinates-in-window-p arg w))))
;;->      (setq w (next-window w))
;;->      (if (eq w start-w)
;;->	  (setq done t)))
;;->    (if rel-coordinate
;;->	(select-window w)
;;->      (progn;; scroll instead
;;->	;;(message "looking for scroll %s" arg2) (sit-for 1)
;;->	(setq w (selected-window))
;;->	(setq done ())
;;->	(while (and (not done)
;;->		    (null (setq rel-coordinate
;;->				(coordinates-in-window-p arg2 w))))
;;->	  (setq w (next-window w))
;;->	  (if (eq w start-w)
;;->	      (setq done t)))
;;->	;;(message "found rel %s" rel-coordinate) (sit-for 2)
;;->	(if rel-coordinate
;;->	    (progn
;;->	      (select-window w)
;;->	      (if (< (car rel-coordinate) (/ (window-width) 2))
;;->		  (scroll-down (/ (window-height) 2))
;;->		(scroll-up (/ (window-height) 2)))))
;;->	(setq rel-coordinate ())
;;->	))
;;->    rel-coordinate))
;;->
;;->(defun x-mouse-find-more (arg)
;;->  ""
;;->  (find-tag last-tag t))
;;->
;;->(defun mouse-find-more (window x y)
;;->  ""
;;->  (find-tag last-tag t))
;;->
;;->(defun mouse-drag-move-point-or-find (window x y)
;;->  (let ((pt (point))
;;->	(w (selected-window)))
;;->    (mouse-drag-move-point window x y)
;;->    (if (and (eq w (selected-window))
;;->	     (eq pt (point)))
;;->	(find-tag-at-point))))
;;->	
;;->(defun quick-browse ()
;;->  "Activates the quick browse key mappings:
;;->   Left= find-tag. Be sure to hold down the key to see the messages
;;->   Middle = tags-loop-continue
;;->   Right = backtrack-to-last-user-location"
;;->  (interactive)
;;->  (cond ((eq window-system 'x)
;;->	 ;; (substitute-key-definition 'x-mouse-select
;;->	 ;;			    'x-mouse-set-point-or-hunt
;;->	 ;;			    mouse-map)
;;->	 ;; (substitute-key-definition 'x-mouse-set-mark
;;->	 ;;			    'x-mouse-find-more
;;->	 ;;			    mouse-map)
;;->	 ;; (substitute-key-definition 'x-mouse-set-point
;;->	 ;;			    'backtrack-to-last-user-location
;;->	 ;;			    mouse-map)
;;->	 (define-key mouse-map x-button-left 'x-mouse-set-point-or-hunt)
;;->	 (define-key mouse-map x-button-middle 'x-mouse-find-more)
;;->	 (define-key mouse-map x-button-right 'backtrack-to-last-user-location)
;;->	 )
;;->	((null window-system)
;;->	 (global-set-mouse '(text left)	  'mouse-drag-move-point-or-find)
;;->	 (global-set-mouse '(text middle) 'mouse-find-more)
;;->	 (global-set-mouse '(text right)  'backtrack-to-last-user-location))
;;->	(t (error "Unrecognized window system for quick browsal"))))
;;->
;;->(defun end-quick-browse ()
;;->  "De-Activates the quick browse key mappings."
;;->  (interactive)
;;->  (cond ((eq window-system 'x)
;;->	 (define-key mouse-map x-button-right 'x-mouse-select)
;;->	 (define-key mouse-map x-button-left 'x-mouse-set-mark)
;;->	 (define-key mouse-map x-button-middle 'x-mouse-set-point)
;;->	 )
;;->	((null window-system)
;;->	 (global-set-mouse '(text        left)	'mouse-drag-move-point)
;;->	 (global-set-mouse '(text	middle)	'mouse-set-mark-and-stuff)
;;->	 (global-set-mouse '(text	right)	'emacs-menu-eval)
;;->	 )
;;->	(t (error "Unrecognized window system for quick browsal"))))
;;->
;;->;; (load "location")


