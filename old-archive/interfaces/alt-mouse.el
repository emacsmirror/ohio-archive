; Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ELF.TN.CORNELL.EDU!eirik
; >From: eirik@ELF.TN.CORNELL.EDU (Eirik Fuller)
; Newsgroups: gnu.emacs
; Subject: mouse support
; Date: 24 Jul 90 03:01:04 GMT
; Organization: GNUs Not Usenet
; 
; The enclosed lisp code provides an alternative to the X11 mouse
; support provided with GNU emacs.  Comments are welcome.  A companion
; posting will provide an example of how to use it without X11.
; 
; 
; This mouse support distinguishes clicks and drags.  A click is a pair
; of button events (down then up) at the same location, while a drag has
; two different locations.  A "click location" is simply the coordinates
; of a matched event pair, while a "drag region" is the text between the
; two locations in an event pair.  A distinction is also made between
; events that land on mode lines and those that don't.
; 
; It is possible to use this mouse support outside of the X environment.
; For example, the Tektronix 4404 terminal emulator has escape sequences
; which, with the right elisp file, provide all of the necessary
; functionality for this mouse support.  For such terminals, replace
; "the X cut buffer" in what follows with "an emacs lisp variable known
; only within one emacs process".
; 
; Here is a summary of the various bindings:
; 
; left click:	move point to the click location
; middle click:	paste the X cut buffer at the click location
; right click:	copy the text between point and the click location
; 		into the X cut buffer
; 
; left drag:	copy the drag region to the X cut buffer
; middle drag:	paste the drag region at point
; right drag:	cut the drag region to the X cut buffer
; 
; left & middle mode line drag:	move the mode line
; right mode line drag:		scroll the indicated window
; 
; left mode line click:		select the indicated window
; middle mode line click:	scroll the indicated window
; right mode line click:	scroll the indicated window
; 
; There are three bindings which scroll the indicated window, each in
; its own way.  The right mode line drag can scroll in either direction,
; depending on whether the initial or final point is on a mode line.
; The middle mode line click treats the mode line as a coordinate axis,
; with zero at the midpoint and the window height (+/-) at each end.
; The coordinate tells how far to scroll.  The right mode line click
; treats the mode line as a coordinate axis from zero to (point-max).
; The coordinate tells what part of the buffer to scroll to.
; 
; It is possible to operate on drag regions larger than a window, if
; each end of such a region is in its own window (window in the emacs
; sense, not the X11 sense).  Drags which cross buffer boundaries do
; nothing; this provides a way to cancel a drag.
; 
; These comments are all of the documentation for this mouse support.
; Feel free to write more; if you do, please send it (or code
; improvements, bug reports, or suggestions) to eirik@elf.tn.cornell.edu
; 

(provide 'x-mouse)
(provide 'mouse)

(fillarray mouse-map 'ignore)

(mapcar (function (lambda (s)
		    (define-key mouse-map s 'mouse-button-down)))
	'("\000" "\001" "\002"))
(define-key mouse-map "\004" 'mouse-right-up)
(define-key mouse-map "\005" 'mouse-middle-up)
(define-key mouse-map "\006" 'mouse-left-up)

(defvar mouse-point nil
  "The mouse location during a down event.  Used by code for up events.")
(defvar mouse-window nil
  "The window corresponding to mouse-point.")
(defvar mouse-modeline nil
  "The window above the modeline pointed at by mouse-point")
(defvar mouse-cut-buffer ""
  "A local substitute for the X cut buffer, for terminals with mice")

(defun mouse-get-cut-buffer () 
  (if (eq window-system 'x) (x-get-cut-buffer) mouse-cut-buffer))

(defun mouse-store-cut-buffer (string)
  (if (eq window-system 'x) (x-store-cut-buffer string)
    (setq mouse-cut-buffer string)))

(global-set-key "\C-x\C-@"
		(function (lambda () 
			    "Process all queued mouse events."
			    (interactive)
			    (while (> (x-mouse-events) 0)
			      (x-proc-mouse-event)))))

(defun window-from-x-y (point)
  "The window containing screen coordinates POINT."
  (if (> (nth 1 point)
	 (- (1- (screen-height)) (window-height (minibuffer-window))))
      (if (zerop (minibuffer-depth)) nil (minibuffer-window))
    (let* ((start (selected-window)) (w start) (which nil))
      (while (not (or (if (coordinates-in-window-p point w) (setq which w))
		      (eq (setq w (next-window w)) start))))
      which)))

(defun mouse-button-down (point)
  "Save away the window containing screen coordinates POINT"
  (setq mouse-window (window-from-x-y point))
  (setq mouse-point point)
  (setq mouse-modeline
	(if mouse-window nil
	    (window-from-x-y
	     (list (car point) (1- (nth 1 point)))))))

(defun window-below (window)
  (let ((w (next-window window t)) (which nil))
    (while (and (<= (nth 3 (window-edges w))
		    (nth 3 (window-edges window)))
		(if (eq (setq w (next-window w t)) window)
		    (setq w nil) t)))
    w))

(defun window-min-height ()
  "The value of window-min-height, except for the minibuffer."
  (if (eq (selected-window) (minibuffer-window)) 1 window-min-height))

(defun move-bottom (w x)
  "Move the mode line beneath window W by X lines"
    (let ((now (selected-window))
	  (below (window-below w)))
      (select-window below)
      (let ((y (max
		(min x (- (window-height w) window-min-height))
		(- (window-min-height) (window-height below)))))
	(if (eq below (minibuffer-window))
	    (enlarge-window y)
	  (progn
	    (select-window w)
	    (shrink-window y))))
      (select-window now)))

(defun move-side (window delta)
  "Adjust vertical boundary"
  (let ((w (selected-window)))
    (select-window window)
    (enlarge-window (if (> delta 0) delta (- 0 delta)) t)
    (select-window w)))

(defun drag-modeline (point)
  "If a button down found a mode line, move it on button up; else return nil"
  (if mouse-modeline
      (let ((y (- (nth 1 mouse-point) (nth 1 point)))
	    (up (list (car point) (1- (nth 1 point)))))
	(if (zerop y)
	    (if (coordinates-in-window-p up mouse-modeline)
		(select-window mouse-modeline)
	      (move-side mouse-modeline (- (car mouse-point) (car point))))
	  (move-bottom mouse-modeline y))
	(setq mouse-modeline nil)
	t)))

(defun move-to-x-y (point)
  "Move to screen coordinates given by POINT; return resulting location"
  (if point
      (let ((rel (coordinates-in-window-p point (selected-window))))
	(if rel (progn
		  (move-to-window-line (nth 1 rel))
		  (move-to-column (+ (car rel) (current-column)
				     (max 0 (1- (window-hscroll)))))))))
  (point))

(defun mouse-same-buffer-p (point)
  "Use this to discard irrelevent button up events"
  (let ((w (window-from-x-y point)))
    (and mouse-window w
	 (eq (window-buffer mouse-window)
	     (window-buffer w)))))

(defun mouse-region (point)
  "The string surrounded by two mouse events; nil if not within one buffer"
  (if (mouse-same-buffer-p point)
      (let ((region "") (w (selected-window)))
	(select-window mouse-window)
	(setq region (buffer-substring
		      (save-excursion
			(move-to-x-y mouse-point))
		      (progn
			(select-window (window-from-x-y point))
			(save-excursion
			  (move-to-x-y point)))))
	(select-window w)
	region)))

(defun mouse-scroll-to (point abs)
  "Treat the mode line as a sideways scroll bar"
  (let ((w (selected-window))
	(edges (window-edges mouse-modeline)))
    (select-window mouse-modeline)
      (unwind-protect
	  (if abs
	      (let ((x (- (nth 2 edges) (car edges))))
		(goto-char (/ (* 
			       (- (car point) (car edges))
			       (point-max)) x))
		(recenter (/ (window-height) 2)))
	    (let ((x (/ (+ (car edges) (nth 2 edges)) 2)))
	      (scroll-up (/ (* (- (car point) x)
			       (window-height))
			    (- x (car edges))))))
	(select-window w))))

(defun mouse-insert-cut-buffer (point)
  "Insert the mouse cut buffer where the mouse is pointing"
  (let ((w (selected-window)))
    (select-window (window-from-x-y point))
    (save-excursion
      (move-to-x-y point)
      (insert (mouse-get-cut-buffer)))
    (select-window w)))

(defun mouse-extend-selection (point)
  "If in buffer of selected window, set mouse cut buffer"
  (let ((w (selected-window)))
    (and w mouse-window (eq (window-buffer mouse-window) (window-buffer w))
	 (mouse-store-cut-buffer
	  (let ((string
		 (buffer-substring
		  (point)
		  (progn (select-window mouse-window)
			 (save-excursion (move-to-x-y point))))))
	    (select-window w)
	    string)))))

(defun mouse-kill-region (point)
  "Store region bounded by mouse events in cut buffer and delete it"
  (if (mouse-same-buffer-p point)
      (let ((w (selected-window)) beg end)
	(select-window mouse-window)
	(save-excursion
	  (setq beg (move-to-x-y mouse-point)))
	(select-window (window-from-x-y point))
	(save-excursion
	  (setq end (move-to-x-y point)))
	(mouse-store-cut-buffer (buffer-substring beg end))
	(delete-region beg end)
	(select-window w)
	t)))

(defun mouse-left-up (point)
  "Copy dragged text to cut buffer, or position cursor, or drag mode line"
  (or (drag-modeline point)
      (let ((region (mouse-region point)))
	(and region
	     (if (zerop (length region))
		 (progn (select-window mouse-window)
			(move-to-x-y point))
	       (mouse-store-cut-buffer region))))))

(defun mouse-middle-up (point)
  "Insert dragged text, or cut buffer, or drag mode line"
  (if mouse-modeline
      (if (equal point mouse-point)
	  (mouse-scroll-to point nil)
	(drag-modeline point))
    (let ((region (mouse-region point)))
      (and region
	   (if (zerop (length region))
	       (mouse-insert-cut-buffer point)
	     (insert region))))))

(defun mouse-scroll-window (point window delta)
  (and (eq window (window-from-x-y point))
       (let ((w (selected-window)))
	 (select-window window)
	 (unwind-protect
	     (scroll-up delta))
	 (select-window w))))

(defun mouse-right-up (point)
  "Extend the selection if no drag, or delete what's dragged"
  (let ((delta (- (nth 1 mouse-point) (nth 1 point))))
    (if mouse-modeline
	(if (zerop delta)
	    (mouse-scroll-to point t)
	  (mouse-scroll-window point mouse-modeline delta))
      (if mouse-window
	  (if (equal point mouse-point)
	      (mouse-extend-selection point)
	    (or (mouse-kill-region point)
		(window-from-x-y point)
		(mouse-scroll-window (list (car point) (1- (nth 1 point)))
			       mouse-window delta)))))))


