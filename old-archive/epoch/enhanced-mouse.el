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
;;; Copyright (C) 1990  Alan M. Carroll
;;;
;;; This file is for use with Epoch, a modified version of GNU Emacs.
;;; Requires Epoch 3.2 or later.
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
;;;
(provide 'epoch-mouse)
(require 'cl)
(require 'epoch-button "button")
;;;
;;; Mouse event handler - extended by Ken Laprade <laprade@trantor.harris-atd.com>. 
;;; Mouse events are set to come in on the event Q, and are then dispatched.
;;; For each button, there are two 16-element tables, each entry being a list of
;;; handler functions. The table is indexed by the modifier&transistion state.
;;; One table is used for button presses in the mode line and the other for
;;; presses in the window proper.
;;;
;;; There are also tables for mouse presses in the minibuffer when it is
;;; not active.  Functions in this map are always called with a nil argument.
;;;
(defconst mouse::button-size 16)
(defconst mouse::table-size (* 3 3 mouse::button-size))
(defvar mouse::global-map (make-vector mouse::table-size nil))
(defvar mouse::local-map nil)
(make-variable-buffer-local 'mouse::local-map)
;;; --------------------------------------------------------------------------
;;;
;;; define the button states
;;;
(defvar shift-mod-mask 1 "Mask for Shift modifier down")
(defvar shift-lock-mod-mask 2 "Mask for Shift Lock modifier down")
(defvar control-mod-mask 4 "Mask for Control modifier down")
(defvar meta-mod-mask 8 "Mask for Meta (mod1) modifier down")
(defvar keyboard-mod-mask
  (+ shift-mod-mask control-mod-mask meta-mod-mask)
  "Mask for any of the keyboard modifiers"
)

(defvar mouse1-mask 256 "Mask for mouse button 1 down")
(defvar mouse2-mask 512 "Mask for mouse button 2 down")
(defvar mouse3-mask 1024 "Mask for mouse button 3 down")
(defvar mouse4-mask 2048 "Mask for mouse button 4 down")
(defvar mouse5-mask 4096 "Mask for mouse button 5 down")
(defvar mouse-any-mask (logior mouse1-mask mouse2-mask mouse3-mask mouse4-mask mouse5-mask)
"Mask for any of the mouse buttons")
;;;
;;; the button/mod constant definitions
;;;
(defconst mouse-window 0)
(defconst mouse-left 0)
(defconst mouse-middle 1)
(defconst mouse-right 2)
(defconst mouse-mode 3)
(defconst mouse-mode-left 3)
(defconst mouse-mode-middle 4)
(defconst mouse-mode-right 5)
(defconst mouse-minibuf 6)
(defconst mouse-minibuf-left 6)
(defconst mouse-minibuf-middle 7)
(defconst mouse-minibuf-right 8)

(defconst mouse-down 0)
(defconst mouse-up 1)

(defconst mouse-shift 2)
(defconst mouse-shift-up (+ mouse-shift mouse-up))
(defconst mouse-control 4)
(defconst mouse-control-up (+ mouse-control mouse-up))
(defconst mouse-control-shift (+ mouse-shift mouse-control))
(defconst mouse-control-shift-up (+ mouse-control-shift mouse-up))
(defconst mouse-meta 8)
(defconst mouse-meta-up (+ mouse-meta mouse-up))
(defconst mouse-meta-shift (+ mouse-shift mouse-meta))
(defconst mouse-meta-shift-up (+ mouse-meta-shift mouse-up))
(defconst mouse-meta-control (+ mouse-meta mouse-control))
(defconst mouse-meta-control-up (+ mouse-meta-control mouse-up))
(defconst mouse-meta-control-shift (+ mouse-shift mouse-control mouse-meta))
(defconst mouse-meta-control-shift-up (+ mouse-meta-control-shift mouse-up))
;;; --------------------------------------------------------------------------
;;; handler installation, etc.
(defun mouse::verify-arguments (button modstate)
  (when (or (< button mouse-left) (> button mouse-minibuf-right))
    (error "Button specifier out of range")
  )
  (when (or (< modstate 0) (>= modstate mouse::button-size))
    (error "Button modifier out of range")
  )
)
;;; --------------------------------------------------------------------------
;;;
;;; This is called for all the mouse events. I'm just going to assume that
;;; every function wants to know where the mouse event was in point/buffer
;;; terms, and is going to go through this effort anyway.
;;;
(defun mouse::xy-to-point (arg)
"Convert an (X Y) list to point. Assumes that the current window is correct. Returns nil if (X Y) is not in the selected window"
  (let
    (
      (rel-coordinate (coordinates-in-window-p arg (selected-window)))
    )
    (if rel-coordinate
      (save-excursion
	(move-to-window-line (cadr rel-coordinate))
	(let ( (h (window-hscroll)) )
	  (move-to-column
	    (+ 
	      (if (> h 0) (- h 1) h) ;deal with scrolled lines
	      (current-column)	;deal with wrapped lines
	      (car rel-coordinate)
	    )
	  )
	)
	(point)			;return value
      )
      nil				;return for XY not in window
    )
  )
)

;;;
(defun mouse::convert-xy-screen (arg scr)
"Convert an (X Y) list and SCREEN into a list of (POINT BUFFER WINDOW SCREEN)"
  (let
    (
      (cw (selected-window))
      (result nil)
    )
    (unwind-protect
      (progn
	(epoch::select-screen scr)	;bypass clever stuff
	;; first, find the window the point was in
	(let
	  (
	    (start-w (selected-window))
	    (done nil)
	    (w (selected-window))
	    (rel-coordinate nil)
	  )
	  (while
	    (and
	      (not done)
	      (null (setq rel-coordinate (coordinates-in-window-p arg w)))
	    )
	    (setq w (next-window w))
	    (if (eq w start-w) (setq done t))
	  )
	  (when rel-coordinate
	    (select-window w)
	    ;; the right window is selectioned, and we have the window-relative	
    ;; co-ordinates. we can finally calculate point!
	    (unwind-protect
	      (save-excursion
		(move-to-window-line (cadr rel-coordinate))
		(let ( (h (window-hscroll)) )
		  (move-to-column
		    (+ 
		      (if (> h 0) (- h 1) h) ;deal with scrolled lines
		      (current-column)	;deal with wrapped lines
		      (car rel-coordinate)
		    )
		  )
		)
		(setq result
		  (list (point) (current-buffer) w (current-screen))
		)
	      )
	      ;; CLEAN-UP
	      (select-window start-w)	;restore in case of window switch
	    )
	  )
	)
      )
      ;; CLEAN-UP
      (select-window cw)		;; this will force the screen back too!
    )
    result
  )
)
;;; --------------------------------------------------------------------------
;;;
(defvar mouse::down-buffer nil "Buffer where the mouse last was pressed.")
(defvar mouse::down-number-offset 0
  "Where the mouse last was pressed (window, mode, minibuf).")
(defvar mouse::event-data nil
  "Raw data value mouse::handler was called with (press/release x y button mod-state).")
(defvar mouse::x 0 "X screen position of mouse, just in case somebody wants it.")
(defvar mouse::y 0 "Y screen position of mouse, just in case somebody wants it.")
(defvar mouse::last-spot nil
  "Mouse data value of last event (point buffer window screen).")
(defvar mouse::clicks 0
  "Number of times mouse was pressed and released in the same place.")
(defvar mouse::clicking nil "t if mouse hasn't moved and same button is being pressed.")

(defun mouse::handler (type value scr)
  (let*
    (
      (number (nth 3 value))
      (edge (nth 0 value))
      (modstate (nth 4 value))
      (epoch::event-handler-abort nil)	;prevent lossage
      (arg (epoch::coords-to-point (nth 1 value) (nth 2 value) scr))
      (buffer (and arg (nth 1 arg)))
      ;; Find which button table.  We want to stay in the same set of tables
      ;; (window, mode, minibuf) as any down press.
      (number-offset (if mouse::down-buffer
			 mouse::down-number-offset
		       (if (and (eq (nth 2 arg) (minibuffer-window))
				(= (minibuffer-depth) 0))
			   mouse-minibuf
			 (if (null (car arg))
			     mouse-mode
			   0))))
    )

    ;; Minibuf presses get no args.
    (if (= number-offset mouse-minibuf)
	(setq arg nil))

    ;; Count clicks as a convenience for some functions.
    (setq mouse::clicking
      (and
        (equal (nth 3 mouse::event-data) number)
	(equal mouse::down-number-offset number-offset)
        (or edge (equal (logand (nth 4 mouse::event-data) keyboard-mod-mask)
			(logand modstate keyboard-mod-mask)))
	(equal mouse::last-spot arg)
        (equal mouse::x (nth 1 value))
        (equal mouse::y (nth 2 value))
      )
    )
    (if mouse::clicking
	(or edge
	  ;; count click on button up.
	  (setq mouse::clicks (1+ mouse::clicks))
	)
      (setq mouse::clicks 0)
    )
    (setq mouse::event-data value)
    (setq mouse::x (nth 1 value))
    (setq mouse::y (nth 2 value))

    (setq number (+ number number-offset))
;    (message "clicks:%d number:%d scr:%s value:%s arg:%s" mouse::clicks number scr value arg)

    ;; find the handler list and try to dispatch
    (let*
      (
        (index
	  (+
	    (if edge mouse-down mouse-up)
	    (if (/= 0 (logand modstate shift-mod-mask)) mouse-shift 0)
	    (if (/= 0 (logand modstate control-mod-mask)) mouse-control 0)
	    (if (/= 0 (logand modstate meta-mod-mask)) mouse-meta 0)
	    (* mouse::button-size ( - number 1 ))
	  )
	)
	(map
	  (if (and mouse::down-buffer (not edge))
	      ;; force release into press buffer, for simulated grab
	      (symbol-buffer-value 'mouse::local-map mouse::down-buffer)
	    ;; ELSE if there's an arg, use the arg buffer
	    (and arg (symbol-buffer-value 'mouse::local-map buffer))
	  )
	)
	(handler
	  (or
	    (and (vectorp map) (aref map index))
	    (aref mouse::global-map index)
	  )
	)
      )
      ;; Record down circumstances for next event.
      (setq mouse::down-buffer (and edge buffer))
      (if edge
	  (setq mouse::down-number-offset number-offset))

      ;; Do it.
      (when (and handler (functionp handler))
        (funcall handler arg)
      )
    )
    (setq mouse::last-spot arg)
  )
)
;;; --------------------------------------------------------------------------
(defmacro mouse::index (button modstate)
  (`
    (+ (, modstate) (* (, button) (, mouse::button-size)))
  )
)
;;;
(defun copy-mouse-map (from to)
  (when (null to) (setq to (make-vector mouse::table-size nil)))
  (let ( (i 0) )
    (while (< i mouse::table-size)
      (aset to i (aref from i))
      (incf i)
    )
  )
  to					; return value
)
;;;
(defun create-mouse-map (&optional source-map)
  (if (vectorp source-map)
    (copy-mouse-map source-map nil)
    (make-vector mouse::table-size nil)
  )
)
;;;
(defun local-set-mouse (button modstate function)
  (mouse::verify-arguments button modstate)
  (when (null mouse::local-map)
    (setq mouse::local-map (create-mouse-map mouse::global-map))
  )
  (aset mouse::local-map (mouse::index button modstate) function)
)
;;;
(defun global-set-mouse (button modstate function)
"Set the global mouse map to have BUTTON with MODIFIER call FUNCTION"
  (mouse::verify-arguments button modstate)
  (aset mouse::global-map (mouse::index button modstate) function)
)
;;;
(defun define-mouse (map button modstate function)
"Set an entry in the MAP for BUTTON and MODIFIER to FUNCTION"
  (when (not (vectorp map)) (error "Map must be a vector"))
  (aset map (mouse::index button modstate) function)
)
;;;
(defun use-local-mouse-map (map &optional buffer)
  (when (not (vectorp map)) (error "Invalid mouse map"))
  (if (bufferp buffer)
    (save-excursion
      (set-buffer buffer)
      (setq mouse::local-map map)
    )
    (setq mouse::local-map map)
  )
)
;;;
(defun kill-local-mouse-map (&option buffer)
"Remove the local mouse map for the option BUFFER (if nil, current buffer)"
  (if (bufferp buffer)
    (save-excursion
      (set-buffer buffer)
      (kill-local-variable 'mouse::local-map)
    )
    (kill-local-variable 'mouse::local-map)
  )
)
;;; --------------------------------------------------------------------------
(defun mouse::set-point (arg)
  "Select Epoch window mouse is on, and move point to mouse position."
  (select-screen (nth 3 arg))
  (if (nth 2 arg) (select-window (nth 2 arg)))
  (if (car arg) (goto-char (car arg)))
)
;;;
(defun mouse::copy-button (button &optional kill)
  "Copy the text in the BUTTON into the X cut buffer and into the Epoch kill ring.
If button does not exist, the X cut buffer is emptied."
  (if (buttonp button)
    (let
      (
        (beg (epoch::button-start button))
	(end (epoch::button-end button))
      )
      (if (null beg) (setq beg 0))
      (if (null end) (setq end 0))
      (epoch::store-cut-buffer (buffer-substring beg end))
      (if (/= beg end)
	(if kill
	    (delete-region beg end)
	  (copy-region-as-kill beg end)
        )
      )
    )
    (epoch::store-cut-buffer "")
  )
)
;;;
(defun mouse::paste-cut-buffer (arg)
    (let ( (buff (nth 1 arg)) )
      (when (and buff (bufferp buff))
      (save-excursion
	(set-buffer (nth 1 arg))
	(goto-char (car arg))
	(insert (epoch::get-cut-buffer))
	(undo-boundary)
	(setq last-command nil)
      )
    )
  )
)
;;; --------------------------------------------------------------------------
;;;
;;; Install things
;;;
(push-event 'button 'mouse::handler)
(setq epoch::mouse-events t)
;;;
;;; --------------------------------------------------------------------------
;;; Macros useful for small mouse bindings that aren't worth defining
;;; a separate function:
(defmacro mousefun (&rest body)
  (`
   (function (lambda (arg) (,@ body )))))

(defmacro mousefun-set-point (&rest body)
  (`
   (function (lambda (arg) (mouse::set-point arg) (,@ body )))))
;;; --------------------------------------------------------------------------
;;; Some mouse functions that do not require motion:

(fset 'mouse-set-point 'mouse::set-point)

(defun abort-isearch () "Abort any isearch in progress."
  (condition-case err
      (throw 'search-done t)
    (no-catch nil)))

(defun mouse-set-spot (arg)
  "Set point at mouse.  With double-click, set mark there as well.
Blinks matching paren if sitting after one.  Intended to be bound
to a window down button."
  (let ((buf (current-buffer))
	(p (point)))
    (mouse::set-point arg)
    (if (and (equal p (point))
	     (equal buf (current-buffer)))
	(if (and (= mouse::clicks 1)
		 (not (eq (mark) (point))))
	    (push-mark))
      (setq mouse::clicks 0))
    (if (eq (char-syntax (preceding-char)) ?\))
	  (blink-matching-open)))
  (abort-isearch))

(defun mouse-select-buffer (arg)
  "Select the window indicated with the mouse.  With drag, adjust
the size of the window (either horizontally or vertically).
With double click, save point with mark-location-form and bury buffer.
Intended to be bound to a mode-line up button."
  (let ((window (selected-window)))
    (mouse::set-point mouse::last-spot)
    (cond ((= mouse::clicks 0)
	   (let ((growth (- mouse::y (nth 1 (window-edges)) (window-height) -1)))
	     (if (= growth 0)
		 ;; No vertical motion, must be horizontal motion.
		 (or (= (window-width) (screen-width))
		     (enlarge-window-horizontally (- mouse::x (car (window-edges)) (window-width))))
	       (or (one-window-p)
		   (enlarge-window growth))))
	   ;; Don't change selected window when adjusting size.
	   (if (window-point window) (select-window window)))
	  ((> mouse::clicks 1)
	   (if (boundp 'mark-location-form) (eval mark-location-form))
	   (bury-buffer)))))

(defun mouse-vscroll (arg)
  "Vertical scroll mouse spot to top of window if it mouse in the lower half
of the window or to the bottom of the window if mouse is in the upper half of
the window.  With a drag, scroll the line at the down press to the mouse
location at the up press.  This should be bound to an up button."
	   (mouse::set-point mouse::last-spot)
		   (let ((window-line (- mouse::y (nth 1 (window-edges)))))
		     (recenter (cond ((< mouse::clicks 1)
				      (min (max window-line 0) (- (window-height) 2)))
				     ((< window-line (/ (window-height) 2))
				      -1)
				     (t 0)))))

(defun mouse-yank (arg)
  "Set point at mouse and yank text from kill ring."
  (mouse::set-point arg)
  (if (> mouse::clicks 0)		; Abort if mouse moved.
      (progn
	(undo-boundary)
	(yank)
	(setq last-command 'yank)
	(abort-isearch))))

(defun mouse-split-window (arg)
  "Split the window vertically at the spot the mouse is clicked."
  (select-screen (nth 3 arg))
  (select-window (nth 2 arg))
  (let ((height (if (car arg)
		    (count-lines (window-start) (car arg))
		  (/ (window-height) 2))))
    (if (or (> window-min-height height)
	    (> window-min-height (- (window-height) height)))
	(error "Window size is too small")
      (split-window-vertically height))))

(defvar highlight-attribute (reserve-attribute) "Attribute for highlight buttons.")
(set-attribute-global highlight-attribute (background) (foreground) (background) (foreground))
(setq epoch::buttons-modify-buffer nil)
(defvar grab-button nil "Highlighted region for mouse grab functions.")

(defun mouse-select-thing (arg)
  "Highlight thing at mouse ARG.  It will be grab-button.  This function
is intended to be bound to a down button.  The corresponding up button
should probably delete grab-button."
  (require 'thing)
  (save-excursion
    (set-buffer (nth 1 arg))
    (let* ((place (thing-boundaries (car arg)))
	   (start (car place))
	   (end (cdr place)))
      (delete-button grab-button)
      (setq grab-button (add-button start end highlight-attribute))))
  (abort-isearch)
  (epoch::redisplay-screen))

(defun mouse-grab-thing (arg)
  "Insert grab-button at point.  Intended as an up button following
mouse-select-thing as a down button."
  (if (and (> mouse::clicks 0)		; Abort grab if mouse moved.
	   (button-buffer grab-button))
      (progn
	(save-excursion
	  (set-buffer (button-buffer grab-button))
	  (setq last-command nil)
	  (copy-region-as-kill (button-start grab-button)
			       (button-end grab-button)))
	(undo-boundary)
	(insert-buffer-substring (button-buffer grab-button)
				 (button-start grab-button)
				 (button-end grab-button))
	(setq last-command nil)))
  (delete-button grab-button)
  (epoch::redisplay-screen))

(defun mouse-kill-thing (arg)
  "Kill region highlighted by grab-button.  Intended as an up button following
mouse-select-thing as a down button."
  (if (and (> mouse::clicks 0)		; Abort if mouse moved.
	   (button-buffer grab-button))
      (save-excursion
       (set-buffer (button-buffer grab-button))
       (undo-boundary)
       (setq last-command nil)
       (delete-region (button-start grab-button)
		      (button-end grab-button))))
  (delete-button grab-button)
  (epoch::redisplay-screen))

(defun mouse-copy-thing (arg)
  "Copy region highlighted by grab-button to kill ring.  Intended as
an up button following mouse-select-thing as a down button."
  (if (and (> mouse::clicks 0)		; Abort if mouse moved.
	   (button-buffer grab-button))
      (save-excursion
       (set-buffer (button-buffer grab-button))
       (setq last-command nil)
       (copy-region-as-kill (button-start grab-button)
			    (button-end grab-button))))
  (delete-button grab-button)
  (epoch::redisplay-screen))

(defun mouse-isearch-thing (arg)
  "Start isearch with thing as default.  Type ^S to actually do the search."
  (require 'thing)
  (setq search-last-string
	(if (and (> mouse::clicks 0)
		 (button-buffer grab-button))
	    (progn
	      (mouse::set-point arg)
	      (goto-char (button-end grab-button))
	      (buffer-substring (button-start grab-button) (button-end grab-button)))))
  (delete-button grab-button)
  (epoch::redisplay-screen)
  (if (> mouse::clicks 0) (isearch t)))
;;; --------------------------------------------------------------------------
;;; Some sample bindings:
;;;
;(global-set-mouse mouse-left mouse-down 'mouse-set-spot)
;(global-set-mouse mouse-left mouse-up t)
;(global-set-mouse mouse-mode-left mouse-up 'mouse-select-buffer)
;(global-set-mouse mouse-left mouse-shift-up 'mouse-vscroll)
;; CONTROL-LEFT: isearch thing at mouse:
;(global-set-mouse mouse-left mouse-control 'mouse-select-thing)
;(global-set-mouse mouse-left mouse-control-up 'mouse-isearch-thing)
;; META-CONTROL-LEFT: grab thing at mouse:
;(global-set-mouse mouse-left mouse-meta-control 'mouse-select-thing)
;(global-set-mouse mouse-left mouse-meta-control-up 'mouse-grab-thing)
;; SHIFT-MODE-LEFT/MIDDLE/RIGHT: Make the mode line a simulated scroll bar:
;(global-set-mouse mouse-mode-left mouse-shift
;		  (mousefun-set-point (scroll-up (/ (* (window-height) 
;						       (- mouse::x (car (window-edges))))
;						    (window-width)))))
;(global-set-mouse mouse-mode-middle mouse-shift
;		  (mousefun-set-point (goto-char (/ (* (point-max) 
;						       (- mouse::x (car (window-edges))))
;						    (window-width)))))
;(global-set-mouse mouse-mode-right mouse-shift
;		  (mousefun-set-point (scroll-down (/ (* (window-height)
;							 (- mouse::x (car (window-edges))))
;						      (window-width)))))
;; META-LEFT: find-tag at point; mode, find next tag; mini, find-tag interactively:
;(global-set-mouse mouse-left mouse-meta (mousefun-set-point (find-tag (find-tag-default))))
;(global-set-mouse mouse-mode-left mouse-meta (mousefun-set-point (tags-loop-continue nil)))
;(global-set-mouse mouse-minibuf-left mouse-meta (mousefun (call-interactively 'completing-find-tag)))
;; META-CONTROL-SHIFT-LEFT: split window vertically; mode, split vertically in half:
;(global-set-mouse mouse-left mouse-meta-control-shift 'mouse-split-window)
;(global-set-mouse mouse-mode-left mouse-meta-control-shift 'mouse-split-window)
;; META-CONTROL-SHIFT-MIDDLE: delete window:
;(global-set-mouse mouse-middle mouse-meta-control-shift (mousefun (delete-window (nth 2 arg))))
;(global-set-mouse mouse-mode-middle mouse-meta-control-shift (mousefun (delete-window (nth 2 arg))))
;; MINI-LEFT: extended command:
;(global-set-mouse mouse-minibuf-left mouse-down 'execute-extended-command)
