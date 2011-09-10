;Return-Path: <jbw%bucsf.BU.EDU@bu-cs.bu.edu>
;Date:  Sun, 16 Apr 89 18:07:22 edt
;From: Joe Wells <jbw%bucsf.BU.EDU@bu-cs.bu.edu>
;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: Scrolling package
;
;Here is the latest version of the enhanced scrolling package I posted
;about half a year ago.  It has bug fixes and enhancements.
;
;Enjoy!
;
;--
;Joe Wells <jbw@bucsf.bu.edu>
;jbw%bucsf.bu.edu@bu-it.bu.edu
;...!harvard!bu-cs!bucsf!jbw
;-------------------------scroll-fix.el--------------------------------
;; Improved window scrolling commands.
;; Copyright (C) 1988, 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; Last changed: Sun Apr 16 12:02:20 1989 by jbw (Joseph Wells) on bucsf
;; jbw%bucsf.bu.edu@bu-it.bu.edu (school year)
;; joew%uswest@boulder.colorado.edu (summer)

;; The ideas for this package were derived from the C code in
;; src/window.c and elsewhere.  The functions in this file should
;; always be byte-compiled for speed.  The functions really don't know
;; what to do with an argument of '-, which results from C-u - or ESC
;; -.  I could use some suggestions on that also.  Someone should
;; rewrite this in C (as part of src/window.c) for speed.

;; This package redefines scroll-up and scroll-down.  The new
;; functions are also available on scroll-up-in-place and
;; scroll-down-in-place.  These names however conflict with names in
;; term/sun.el, which has caused problems for people on Suns.

;; This doesn't work properly when the buffer is all one long line.

(require 'backquote)

(defmacro abs (n)
  (`(let ((m (, n)))
      (if (< m 0) (- m) m))))

;; both negative or both non-negative
(defmacro same-sign (x y)
  (`(let ((z (, y)))
     (if (< (, x) 0)
	 (< z 0) (>= z 0)))))

(defvar scroll-in-place t
  "*Whether to use the default scrolling or the new enhanced scrolling
when either scroll-down-in-place or scroll-up-in-place is called.")

(defvar scroll-in-place-replace-original nil
  "*Whether scroll-up or scroll-down should also use the new enhanced
scrolling capability.  This only applies if scroll-in-place is
non-nil, otherwise only the original built-in scrolling is used.")

(defvar sip:goal-column 0
  "DON'T USE THIS!
Current goal column for scrolling motion.  It is
the column where point was at the start of current run of scrolling
commands.")

(defvar sip:default-motion nil
  "DON'T USE THIS!
Default argument to scroll-up-in-place or scroll-down-in-place, when
repeated with no intervening command and no argument.  This is the
last argument used.")

(defvar sip:eob-motion nil
  "DON'T USE THIS!
Amount of motion to be used by scroll-up-in-place or
scroll-down-in-place when repeated after hitting the end/beginning of
the buffer with no intervening command and no argument.  This is the
amount of vertical motion that was actually done on the last scroll
operation (which was less than requested, because of buffer
boundaries).")

(defvar sip:eob-blank-limit nil
  "DON'T USE THIS!
This is the minimum amount of text that is required on the last
screen.  scroll-up-in-place will refuse to scroll any more than this.
Normally this is one less than the number of text lines in the window.
However, if a sequence of scrolling commands starts with less text on
the last screen, this is remembered here.")

(defvar sip:eob-last-point nil
  "DON'T USE THIS!
Following a scroll operation that attempted to scroll past the
beginning or end of the buffer, point is moved to the beginning of end
of buffer.  This is the location of point before the move.")

(defun scroll-in-place (arg)
  "Toggle the scroll-in-place enhanced scrolling feature.  With
argument, turn feature on if arg is positive.  The enhanced scrolling
of the scroll-in-place feature makes all scrolling actions reversible.
No more pounding on the arrow keys trying to get back to your place in
the buffer.  Try it, you'll like it."
  (interactive "P")
  (setq scroll-in-place
	(if (null arg)
	    (not scroll-in-place)
	  (> (prefix-numeric-value arg) 0))))

(defun scroll-down-in-place (&optional n)
  "Scroll text of current window downward ARG lines; or near full screen if
no ARG.  When calling from a program, supply a number as argument or nil.
Leaves point in same row and column of window."
  (interactive "P")
  (if (and scroll-in-place
	   (or scroll-in-place-replace-original
	       (eq this-command 'scroll-down-in-place)))
      (scroll-in-place-command n -1)
    (original-scroll-down n))
  nil)

(or (fboundp 'original-scroll-down)
    (fset 'original-scroll-down (symbol-function 'scroll-down)))
(fset 'scroll-down (symbol-function 'scroll-down-in-place))

(defun scroll-up-in-place (&optional n)
  "Scroll text of current window upward ARG lines ; or near full screen if
no ARG.  When calling from a program, supply a number as argument or nil.
Leaves point in same row and column of window."
  (interactive "P")
  (if (and scroll-in-place
	   (or scroll-in-place-replace-original
	       (eq this-command 'scroll-up-in-place)))
      (scroll-in-place-command n 1)
    (original-scroll-up n))
  nil)

(or (fboundp 'original-scroll-up)
    (fset 'original-scroll-up (symbol-function 'scroll-up)))
(fset 'scroll-up (symbol-function 'scroll-up-in-place))

(defun scroll-in-place-command (arg direction)
  "Scroll text of current window ARG lines in DIRECTION direction.  If
ARG is null, figures out how much to scroll based on previous
scrolling, or scrolls window-height minus next-screen-context-lines.
If ARG is '- (the symbol), scrolls window in - DIRECTION direction.
DIRECTION is either 1 or -1.  Leaves point in same row and column of
window."
  ;;(message "%s %s %s %s %s %s %s"
  ;;arg last-command this-command sip:default-motion
  ;;sip:eob-blank-limit sip:eob-motion sip:eob-last-point)
  (let* ((window (selected-window))
	 (height (- (window-height window)
		    (if (eq window (minibuffer-window)) 0 1)))
	 ;; lines is going to be the number of lines that we finally
	 ;; decide to move, with negative meaning towards the
	 ;; beginning of the buffer.
	 (lines (- height next-screen-context-lines))
	 (default-lines (if (< lines 0) 1 lines))
	 (n (prefix-numeric-value arg))
	 (first-scroll
	  (not (memq last-command '(scroll-down-in-place scroll-up-in-place))))
	 moved)
    
    ;; Barf on zero ARG argument
    (and (eq arg 0)
	 (while t (signal 'args-out-of-range (list 'arg arg))))
    ;; Barf on DIRECTION not 1 or -1
    (or (= direction 1) (= direction -1)
	(while t (signal 'args-out-of-range (list 'direction direction))))
    
    ;; Figure out how much vertical motion to use.  An explicit
    ;; argument is always given precedence.  An explicit argument of
    ;; '- (the symbol) means negative default scrolling (needs work).
    ;; If no explicit argument is given, we decide based on several
    ;; factors.  First, if a immediately preceding scroll command ran into
    ;; a buffer boundary, and didn't go full distance, and this is a
    ;; scroll in the opposite direction, go back the amount that last
    ;; command traveled.  Second, if following a prior scroll use the
    ;; last explicit argument.  Else, default lines are calculated to
    ;; leave next-screen-context-lines of scrolling context on the
    ;; screen.
    (cond ((or (numberp arg) (consp arg))
	   (setq lines n
		 sip:default-motion n
		 sip:eob-motion nil
		 sip:eob-last-point nil))
	  ((eq arg '-)			;needs more work
	   (setq lines default-lines
		 direction (- direction)
		 sip:eob-motion nil
		 sip:eob-last-point nil))
	  (first-scroll
	   (setq lines default-lines
		 sip:default-motion default-lines
		 sip:eob-motion nil
		 sip:eob-last-point nil))
	  (sip:eob-last-point
	   (or (bobp) (eobp)
	       (error "sip:eob-last-point set erroneously!"))
	   (and (eq sip:eob-last-point (point))
		(error "sip:eob-last-point set to buffer boundary!"))
	   ;; In this case, we're doing the actual motion right here.
	   (setq lines 0)
	   ;; Only do something if scrolling away from buffer boundary.
	   (and (same-sign direction (- sip:eob-last-point (point)))
		(goto-char sip:eob-last-point)
		(setq sip:eob-last-point nil)))	;only once
	  ((and sip:eob-motion
		(same-sign direction sip:eob-motion))
	   (setq lines (abs sip:eob-motion)
		 sip:eob-motion nil
		 sip:eob-last-point nil)) ;this must be done second
	  (t
	   ;; second or later in a sequence without an explicit arg
	   (setq lines sip:default-motion
		 sip:eob-motion nil
		 sip:eob-last-point nil)))
    (setq lines (* direction lines))
    
    ;; On the first scroll command, record the current column and
    ;; whether and where the end-of-buffer is in the window.
    (cond (first-scroll
	   (setq sip:goal-column (or (and track-eol (eolp) 9999)
				     (current-column)))
	   ;; We record the number of lines of text in the window
	   ;; minus one.  We will use this later to make scrolling up
	   ;; from the end of buffer and then back down completely
	   ;; reversible.  Otherwise, the feature of scrolling so
	   ;; that the end of buffer is never higher than the last
	   ;; line of the window would prevent reversibility.
	   (setq sip:eob-blank-limit
		 (save-excursion
		   (goto-char (window-start window))
		   (vertical-motion (1- height))))))
    
    ;; if point not in window, center window around point
    (save-excursion
      (cond ((not (pos-visible-in-window-p (point) window))
	     (vertical-motion (/ (- height) 2))
	     (set-window-start window (point)))))
    
    ;; Execute the scrolling motion.
    (and
     (not (= lines 0))
     (catch 'buffer-boundary
       (save-excursion
	 (goto-char (window-start window))
	 (cond ((< lines 0)	        ; upward -- scrolling down
		(cond ((bobp)
		       (ding)
		       (message (get 'beginning-of-buffer 'error-message))
		       (throw 'buffer-boundary t)))
		(setq moved (vertical-motion lines)))
	       ((> lines 0)		; downward -- scrolling up
		;; When checking for downward motion scrolling up, we
		;; actually move somewhere between one and two screens
		;; worth of distance, to make sure that the end of
		;; buffer will be at least sip:eob-blank-limit lines
		;; down from the first line.  sip:eob-blank-limit is
		;; the number of lines of text after the first line
		;; that were in this window at the time the current
		;; sequence of scrolling commands started.
		(setq moved (+ (vertical-motion (+ lines
						   sip:eob-blank-limit))
			       (vertical-motion (- sip:eob-blank-limit))))
		(cond ((< moved 1)
		       (ding)
		       (message (get 'end-of-buffer 'error-message))
		       (throw 'buffer-boundary t))))
	       (t (error "Impossible value")))
	 (set-window-start window (point)))
       (if (< (abs moved) (abs lines))
	   (setq sip:eob-motion moved))
       ;; keep point on same line of window
       (vertical-motion moved)
       ;; keep point on same column of window
       (move-to-column sip:goal-column)
       nil)
     ;; We are already at the limit of scrolling.  Move point to the
     ;; buffer boundary instead.
     (cond ((and (< lines 0) (not (bobp)))
	    (setq sip:eob-last-point (point))
	    (goto-char (point-min)))
	   ((and (> lines 0) (not (eobp)))
	    (setq sip:eob-last-point (point))
	    (goto-char (point-max))))))
  nil)

(provide 'scroll-in-place)

