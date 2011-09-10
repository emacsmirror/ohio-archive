; Date: Tue, 9 Oct 90 14:48:23 EDT
; From: Ken Laprade <laprade@trantor.harris-atd.com>
; Subject: mouse.el enhancements and mouse-help.el
; 
; The changes to epoch::coords-to-point in mouse.c that I sent in last month
; allow the mouse::handler to distinguish between button events actually in a
; window, on a mode line, or in the (inactive) minibuffer area.  Here is the
; mouse.el that I am now using.  I have added support for separate
; mouse-map's for each of these three areas.  There are also some functions
; and sample mouse bindings for things that do not require motion (those are
; in motion.el).
; 
; I have also included mouse-help.el.  This contains describe-mouse and
; describe-mouse-briefly, which accept a mouse press and print documentation
; on its binding, and mouse-helper, which pops up a small screen listing the
; mouse::global-map bindings.  All expect to be used with my version of
; mouse.el.
; 
; -- 
; Ken Laprade			INTERNET: laprade@trantor.harris-atd.com
; Harris Corporation 		Usenet:  ...!uunet!x102a!trantor!laprade
; PO Box 37, MS 3A/1912		Voice: (407)727-4433
; Melbourne, FL 32902		FAX: (407)729-2537
; 
; 
;;; Copyright (C) 1990  Kenneth C. Laprade <laprade@trantor.harris-atd.com>
;;;
;;; This file is for use with Epoch, a modified version of GNU Emacs.
;;; Requires Epoch 3.2 with my patch to mouse.c.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; bute WITHOUT ANY WARRANTY. No author or distributor accepts
;;; responsibility to anyone for the consequences of using this code
;;; or for whether it serves any particular purpose or works at all,
;;; unless explicitly stated in a written agreement.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this code, but only under the conditions described in the
;;; GNU Emacs General Public License, except the original author nor his
;;; agents are bound by the License in their use of this code.
;;; (These special rights for the author in no way restrict the rights of
;;;  others given in the License or this prologue)
;;; A copy of this license is supposed to have been given to you along
;;; with Epoch so you can know your rights and responsibilities. 
;;; It should be in a file named COPYING.  Among other things, the
;;; copyright notice and this notice must be preserved on all copies. 

;;; This file provides two things:
;;;    The functions describe-mouse and describe-mouse-briefly accept a mouse
;;; press and give documetation on the bindings to both the up and down
;;; events.
;;;    The function mouse-helper pops up an epoch screen with a buffer listing
;;; all the mouse bindings.  Only one set of bindings is shown at a time.
;;; Pressing the mouse on a keyword in the mode line will change the set of
;;; bindings that is displayed.


;;; Mouse help:
(defun describe-mouse-briefly (&optional repeat)
  "Print a short message about what a mouse button does.  With REPEAT,
keep accepting buttons and describing them until \\[keyboard-quit]."
  (interactive "P")
  (describe-mouse repeat t))

(defun describe-mouse (&optional repeat brief)
  "Display documentation of the function(s) a mouse button invokes.  With
REPEAT (prefix interactively), keep accepting buttons and describing them
until \\[keyboard-quit].  With BRIEF, just make a short message."
  (interactive "P")
  (push-event 'button 'mouse::help-handler)
  (unwind-protect
      (let ((first t))
	(while (or first repeat)
	  (let ((mouse-press-value nil)
		(mouse-press-scr nil)
		(mouse-press-wait t)
		(echo-keystrokes 0))
	    (while mouse-press-wait
	      (if (or first (not brief))
		  (message (if brief "Describe mouse button briefly:"  "Describe mouse button:")))
	      (sit-for 1 t)
	      (if (input-pending-p)
		  (read-char)))
	    (message "")
	    (let* ((number (nth 3 mouse-press-value))
		   (modstate (nth 4 mouse-press-value))
		   (arg (epoch::coords-to-point (nth 1 mouse-press-value) (nth 2 mouse-press-value) mouse-press-scr))
		   (buffer (and arg (nth 1 arg)))
		   ;; Find which button table (window, mode, minibuf).
		   (number-offset (if (and (eq mouse-press-scr (minibuf-screen))
					   (= (minibuffer-depth) 0))
				      mouse-minibuf
				    (if (null (car arg))
					mouse-mode
				      0)))
		   (down-index (+ mouse-down
				  (if (/= 0 (logand modstate shift-mod-mask)) mouse-shift 0)
				  (if (/= 0 (logand modstate control-mod-mask)) mouse-control 0)
				  (if (/= 0 (logand modstate meta-mod-mask)) mouse-meta 0)
				  (* mouse::button-size (+ number number-offset -1))))
		   (up-index (+ (- down-index mouse-down) mouse-up))
		   (map (and arg (symbol-buffer-value 'mouse::local-map buffer)))
		   (down-defn (or (and (vectorp map) (aref map down-index))
				  (aref mouse::global-map down-index)))
		   (up-defn (or (and (vectorp map) (aref map up-index))
				(aref mouse::global-map up-index))))
	      (if (and (or (null down-defn) (integerp down-defn))
		       (or (null up-defn) (integerp up-defn)))
		  (message "button is undefined")
		(if brief
		    (message "DOWN: %s  UP: %s"
			     (function-description down-defn)
			     (function-description up-defn))
		  (with-output-to-temp-buffer "*Help*"
		    (if down-defn (progn
				    (princ "DOWN - ")
				    (prin1 down-defn)
				    (princ ":\n")
				    (if (documentation down-defn)
					(princ (documentation down-defn))
				      (princ "not documented"))
				    (princ "\n\n")))
		    (if up-defn (progn
				  (princ "UP - ")
				  (prin1 up-defn)
				  (princ ":\n")
				  (if (documentation up-defn)
				      (princ (documentation up-defn))
				    (princ "not documented"))))
		    (print-help-return-message))
		  (epoch::redisplay-screen))))
	    (setq first nil))))
    (pop-event 'button)))

(defun mouse::help-handler (type value scr)
  (setq mouse-press-value value
	mouse-press-scr scr
	mouse-press-wait (nth 0 value)))
;;; --------------------------------------------------------------------------
;;; Mouse-helper: a popup screen describing all mouse button combinations.

(defvar mouse-helper-screen nil "Screen for mouse-helper.")
(defvar auto-unmap-screens nil)		;just in case there isn't any.
(defvar last-mouse-helper-mode 1)
(defvar mouse-helper-mouse-map (create-mouse-map))
(define-mouse mouse-helper-mouse-map mouse-mode-left mouse-down 'mouse-helper-mode-down)
(define-mouse mouse-helper-mouse-map mouse-mode-left mouse-up 'mouse-helper-mode-up)
(define-mouse mouse-helper-mouse-map mouse-left mouse-down (function (lambda (arg) (mapraised-screen))))
(define-mouse mouse-helper-mouse-map mouse-left mouse-up t)

(defconst mouse-helper-keys (list "WINDOW-DOWN" "WINDOW-UP"
				  "MODE-DOWN" "MODE-UP"
				  "MINIBUF-DOWN" "MINIBUF-UP"))
(defconst mouse-helper-modes (list "WINDOW   mode   minibuf      DOWN"
				   "WINDOW   mode   minibuf"
				   "window   MODE   minibuf      DOWN"
				   "window   MODE   minibuf"
				   "window   mode   MINIBUF      DOWN"
				   "window   mode   MINIBUF"))
(defconst mouse-helper-offsets (list (+ mouse-down (* mouse-window mouse::button-size))
				     (+ mouse-up (* mouse-window mouse::button-size))
				     (+ mouse-down (* mouse-mode mouse::button-size))
				     (+ mouse-up (* mouse-mode mouse::button-size))
				     (+ mouse-down (* mouse-minibuf mouse::button-size))
				     (+ mouse-up (* mouse-minibuf mouse::button-size))))

(defun mouse-helper-mode-down (arg)
  "Show bindings for down presses."
  (mouse::set-point arg)
  (let ((i (cond ((< mouse::x 7) 0)
		 ((< mouse::x 14) 2)
		 (4))))
    (mouse-helper-show (elt mouse-helper-keys i))
    (setq mode-line-format (elt mouse-helper-modes i))
    (setq last-mouse-helper-mode i)))

(defun mouse-helper-mode-up (arg)
  "Show bindings for up presses.  If dragged, leave down presses showing."
  (if (/= mouse::clicks 0)
      (progn (mouse::set-point arg)
	     (let ((i (cond ((< mouse::x 7) 1)
			    ((< mouse::x 14) 3)
			    (5))))
	       (mouse-helper-show (elt mouse-helper-keys i))
	       (setq mode-line-format (elt mouse-helper-modes i))
	       (setq last-mouse-helper-mode i)))))


(defun mouse-helper (&optional force geometry font file)
  "Map or unmap the mouse helper screen.  If the screen does not exist, it
is created listing the various mouse functions.  With non-nil FORCE (prefix
interactively), the screen is recreated and mapped regardless of whether it
already exists.  By clicking on the mode line, the contents can be changed
for window, mode, or minibuf bindings.  Down bindings are shown while the
button is help down.  Up bindings are shown when the button is released.
If there is no up binding, the corresponding down binding is shown with a
`>' prefixed.  If FORCE is 'nomap, the helper screen is left unmapped.
GEOMETRY and FONT are used to create the screen.  FILE has a listing of the
text for the helper buffer.  In this file, keywords `WINDOW', `MODE', and
'MINIBUF' with suffixes `UP' and `DOWN' indicate the different sections.
The associated section is enclosed by blank lines following the keyword.
Without FILE, the helper text is built from the current global mouse
bindings."
  (interactive "P")
  (if (and (screenp mouse-helper-screen)
	   (get-screen-id mouse-helper-screen))
      (progn
	(if (or (not (get-buffer " *mouse-helper*"))
		(not (memq mouse-helper-screen (screens-of-buffer " *mouse-helper*"))))
	    (setq force (or force t)))
	(if (or (eq force 'nomap)
		(and (not force) (memq mouse-helper-screen (screen-list))))
	    (progn (unmap-screen mouse-helper-screen)
		   (setq auto-unmap-screens (delq 'mouse-helper-screen auto-unmap-screens)))
	  (mapraised-screen mouse-helper-screen)
	  (or (memq 'mouse-helper-screen auto-unmap-screens)
	      (setq auto-unmap-screens (append auto-unmap-screens (list 'mouse-helper-screen)))))))
  (if (or force
	  (not (screenp mouse-helper-screen))
	  (not (get-screen-id mouse-helper-screen)))
      (save-excursion
	(set-buffer (get-buffer-create " *mouse-helper*"))
	(erase-buffer)
	(if (and (stringp file) (file-exists-p file))
	    (insert-file-contents file)
	  (let ((keys mouse-helper-keys)
		(offsets mouse-helper-offsets)
		(width (if geometry (string-to-int geometry) 100)))
	    (while keys
	      (insert "\n\n" (car keys) "\n\n")
	      (build-mouse-help mouse::global-map (car offsets) width)
	      (setq keys (cdr keys)
		    offsets (cdr offsets)))))
	(mouse-helper-show (elt mouse-helper-keys last-mouse-helper-mode))
	(setq mode-line-format (elt mouse-helper-modes last-mouse-helper-mode))
	(use-local-mouse-map mouse-helper-mouse-map)))
  (if (and (screenp mouse-helper-screen)
	   (get-screen-id mouse-helper-screen))
      (set-window-buffer (epoch::selected-window mouse-helper-screen)
			 " *mouse-helper*")
    (setq mouse-helper-screen
	  (create-screen " *mouse-helper*"
			 (screen-attributes (or geometry "100x9+250+-26") "MOUSE"
					    (cons 'font (or font "5x8")))))
    (if (eq force 'nomap)
	(progn (unmap-screen mouse-helper-screen)
	       (setq auto-unmap-screens (delq 'mouse-helper-screen auto-unmap-screens)))
      (or (memq 'mouse-helper-screen auto-unmap-screens)
	  (setq auto-unmap-screens (append auto-unmap-screens (list 'mouse-helper-screen)))))))

(defvar uninteresting-fncs (list 'mouse::set-point 'mouse-set-point
				 'start-mouse-drag 'call-interactively
				 'quote 'save-excursion 'set-buffer
				 'nth  'undo-boundary 'setq))

(defun interesting-function (l)
  "Return the first function mentioned in the list that is not in
uninteresting-fncs."
  (let ((f nil))
    (while (and l (listp l)
		(if (functionp f)
		    (memq f uninteresting-fncs)
		  t))
      (setq f (car l))
      (if (and f (listp f))
	  (setq f (interesting-function f)))
      (setq l (cdr l)))
    f))

(defun function-description (func &optional max)
  "Return a string describing the function FUNC.  If it is a lambda list,
any documentation string is used.  If there is no documentation string, the
first function in the list that is not in uninteresting-fncs is used.
With optional MAX, limits string to MAX characters."
  (if (null func)
      ""
    (let ((s (if (functionp func)
		 (documentation func))))
      (if (and (listp func)
	       (eq (car func) 'lambda))
	  (or s
	      (setq func (interesting-function func)))
	(setq s nil))
      (or s (setq s (format "%s" func)))
      (substring s 0 (if max (min (length s) max))))))

(defconst mouse-helper-states-text (list "PLAIN" "CONTROL" "SHIFT" "META"
					   "C-S" "M-C" "M-S" "M-C-S"))
(defconst mouse-helper-states (list mouse-down mouse-control mouse-shift mouse-meta
				      mouse-control-shift mouse-meta-control
				      mouse-meta-shift mouse-meta-control-shift))

(defun build-mouse-help (map offset width)
  "Build the mouse-helper text from bindings in MAP at OFFSET.  For any
buttons with no binding on one edge, the other edge's entry is used (with a
`>' prefixed).  WIDTH is the number of columns each line should fit in."
  (let ((states mouse-helper-states)
	(states-text mouse-helper-states-text)
	entry)
    (setq width (/ (- width 10) 3))
    (while states
      (insert (car states-text))
      (indent-to 8 1)
      (if (setq entry (aref map (+ (car states) (* mouse-left mouse::button-size) offset)))
	  (insert (function-description entry width))
	(if (setq entry (aref map (logxor
				   (+ (car states) (* mouse-left mouse::button-size) offset)
				   mouse-up)))
	    (insert ?> (function-description entry (1- width)))))
      (indent-to (+ 9 width) 1)
      (if (setq entry (aref map (+ (car states) (* mouse-middle mouse::button-size) offset)))
	  (insert (function-description entry width))
	(if (setq entry (aref map (logxor
				   (+ (car states) (* mouse-middle mouse::button-size) offset)
				   mouse-up)))
	    (insert ?> (function-description entry (1- width)))))
      (indent-to (+ 10 width width) 1)
      (if (setq entry (aref map (+ (car states) (* mouse-right mouse::button-size) offset)))
	  (insert (function-description entry width))
	(if (setq entry (aref map (logxor
				   (+ (car states) (* mouse-right mouse::button-size) offset)
				   mouse-up)))
	    (insert ?>(function-description entry (1- width)))))
      (insert ?\n)
      (setq states (cdr states)
	    states-text (cdr states-text)))))

(defun mouse-helper-show (keyword)
  "Narrow the mouse-helper buffer to text associated with regexp KEYWORD.
Assumes mouse-helper buffer is selected."
  (widen)
  (goto-char (point-min))
  (re-search-forward keyword)
  (let ((beg (if (search-forward "\n\n" nil t)
		 (match-end 0) (point))))
    (search-forward "\n\n" nil 'end)
    (while (eq (preceding-char) ?\n)
      (backward-char))
    (narrow-to-region beg (point)))
  (goto-char (point-min)))
