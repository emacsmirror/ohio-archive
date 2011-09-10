;;;; mandel.el - Mandelbrot program for GNU Emacs 19 - version 1.0

;; Copyright (C) 1995 Jim Blandy

;; Author: Jim Blandy <jimb@cyclic.com>
;; Keywords: languages

;; mandel.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mandel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package plots areas of the Mandelbrot set in Emacs windows.
;;; It requires GNU Emacs version 19, and X11R5 or later.
;;;
;;; It currently wants to use at least 10 colors, so it will run only
;;; on displays with at least four bits per pixel.  If you would like
;;; to adapt it to work on shallower displays, I'd be happy to receive
;;; patches.
;;;
;;; To use, load this file and type M-x mandelbrot RET.  You might
;;; want to make your window smallish first; it's a bit slow.
;;;
;;; If you want to have it loaded automatically, put the file in some
;;; directory in your Emacs load path, and then put the following form
;;; in your .emacs file:
;;;   (autoload 'mandelbrot "mandel" "Plot an area of the Mandelbrot set." t)
;;;
;;; If you change mand-insert-display to use text properties instead
;;; of overlays, this program really shows up the deficiencies in the
;;; implementation of text properties.  Overlays aren't so bad.
;;;
;;; For some reason, when you kill the *Mandelbrot Set* buffer, the
;;; overlays' markers don't get freed (according to garbage-collect).
;;; Is this a bug in my code, or a bug in Emacs?  I wish I had a
;;; debugger that worked under Solaris.
;;;
;;; Future improvements:
;;;
;;;	It's a pain in the neck to wait for this thing to do 200
;;;	iterations on every point in the set before going on to other
;;;	fleeing points, which are what we're usually interested in
;;;	anyway.  It should do one pass with a maximum of ten
;;;	iterations, then increase the limit and make another pass over
;;;	those points it hasn't caused to diverge yet, and then
;;;	another...  Stop as soon as the user generates some input.
;;;	This means we'll get accurate pictures at high magnifications
;;;	without having to wait an aeon at low magnifications.
;;;
;;;     There's a package out there (whose name I can't remember) that
;;;     lets you run emacs lisp things "in the background" ---
;;;     i.e. between commands.  This should run that way.
;;;
;;;	We should use the Distance Bound Method, as described in
;;;	Peitgen et al.  (Or is that Pietgen?)  It won't give us nice
;;;	colors, but it would be good for a first pass - fill in the
;;;	colors later.
;;;
;;;	Julia sets are nice, too, of course.  And what were those
;;;	formuli that that old Amiga program used?
;;;
;;; $Id: mandel.el,v 1.2 1995/04/21 15:09:54 jimb Exp $

;;; Code:

(require 'float)
(require 'faces)


;;;; Parameters users may want to tweedle.

(defvar mand-max-iterations 200
  "*Maximum length of sequence to test for divergence.
If a point's sequence does not diverge after this many elements, we
guess that the point is in the Mandelbrot set.")

(defvar mand-member-color "black")

(defvar mand-num-fleeing-colors 10
  "*The number of different colors to use to display fleeing points.")

(defvar mand-char-aspect-ratio 1.8
  "*The ratio of the visual height of a character cell to its width.
It's not important that this be terribly accurate, since it is only
used to correct the display; if it is wrong, your Mandelbrot set display
will be stretched along one axis.
This is usually a floating point number.")

(defvar mand-fleeing-color-function nil
  "*A function to call to compute colors for fleeing faces, or nil.
If it's a function, it should take one argument, a number between 0
and 1, and return a color name.
If it's nil, the Mandelbrot code will choose a function suitable for
your display when executed.")


;;;; What is the set?

(defun not-mand-member-p (c)
  "Return nil if C is in the Mandelbrot set, non-nil otherwise.
C has the form (REAL . IMAGINARY), representing a complex number.
If C is not in the mandelbrot set, the return value is the number of
iterations it took for the point to escape the circle of radius 2
centered at the origin (points outside this circle always diverge)."
  (let* ((cr (float (car c)))
	 (ci (float (cdr c)))
	 (zr cr)
	 (zi ci)
	 (iter 0))
    (while (and (< iter mand-max-iterations)
		(<= (+ (* zr zr) (* zi zi)) 4))
      ;; We're computing
      ;;    zr + i zi := (zr + i zi)^2 + (cr + i ci)
      ;; Separating this into real and imaginary portions, that's
      ;;    zr = zr^2 - zi^2 + cr
      ;; and
      ;;    zi = 2 zr zi + ci
      (setq zi (prog1 (+ (* 2 zr zi) ci)
		 (setq zr (+ (* zr zr) (- (* zi zi)) cr)))
	    iter (1+ iter)))
    (if (< iter mand-max-iterations) iter)))


;;;; Deciding how to color the set.

(defvar mand-fleeing-faces nil
  "A vector of faces to use for fleeing points.
These faces have their background and foreground set to the same
color, so they obscure any characters they are applied to.  This is
deliberate - we store information about which points diverged and
which didn't in the buffer text.")

(defvar mand-last-fleeing-color-function nil
  "The function we used the last time we built mand-fleeing-faces.")


(defun mand-build-faces ()
  "Set up `mand-fleeing-faces', if it isn't already initialized.
If `mand-num-fleeing-colors' or `mand-fleeing-color-function' has
changed since we last built the face array, rebuild it."
  (if (or (not (vectorp mand-fleeing-faces))
	  (/= (length mand-fleeing-faces) mand-num-fleeing-colors)
	  (not (eq mand-fleeing-color-function
		   mand-last-fleeing-color-function)))
      (progn
	(setq mand-fleeing-faces (make-vector mand-num-fleeing-colors nil))
	(let ((i 0))
	  (while (< i mand-num-fleeing-colors)
	    (let ((face-sym (intern (format "mand-face-%d" i)))
		  (face-color (funcall mand-fleeing-color-function
				       (/ (float i) mand-num-fleeing-colors))))
	      (aset mand-fleeing-faces i face-sym)
	      (make-face face-sym)
	      (set-face-background face-sym face-color)
	      (set-face-foreground face-sym face-color))
	    (setq i (1+ i))))
	(setq mand-last-fleeing-color-function
	      mand-fleeing-color-function)))

  (make-face 'mand-member)
  (set-face-background 'mand-member "black"))

(defun mand-choose-color-scale (window)
  "Return a color scale function appropriate for display in WINDOW."
  (cond

   ;; Are we running X?
   ((eq window-system 'x)
    (cond

     ;; This is just an attempt to avoid uglier errors.
     ((< (x-display-color-cells (window-frame window)) 16)
      (error "display doesn't have enough colormap entries"))

     ((x-display-color-p) 'mand-sin-scale)
     (t 'mand-gray-scale)))

   ;; Probably nothing will work anyway.
   (t (error "mandelbrot requires a window system to display properly"))))


(defun mand-gray-scale (ratio)
  "Return the color whose white/black ratio is RATIO."
  (format "RGBi:%f/%f/%f" ratio ratio ratio))

(defun mand-sin-scale (ratio)
  (let ((angle (* ratio pi)))
    (apply 'format "RGBi:%f/%f/%f"
	   (mapcar (lambda (angle) (let ((s (/ (+ 1 (sin angle)) 2)))
				     (* s s)))
		   (list angle
			 (+ angle (/ (* pi 4) 3))
			 (+ angle (/ (* pi 2) 3)))))))


;;;; Deciding where to plot the set.

(defvar mand-requested-bounds nil
  "Internal variable used by mand-mode.  Buffer-local.

This holds the region of the Mandelbrot set that should be displayed
in the current buffer, stored as a pair of complex numbers.  We
calculate our display scale so that this area fits in the current
window.

Note that, if the window doesn't have the same aspect ratio as this
rectangle, we may display extra area outside this rectangle.

See also mand-plotting-bounds.")

(defvar mand-plotting-bounds nil
  "Internal variable used by mand-mode.  Buffer-local.

This holds the region of the Mandelbrot set that we last plotted in
this window, stored as a pair of complex numbers.  This completely
contains mand-requested-bounds, and is usually larger, because we
try to preserve the aspect ratio of mand-requested-bounds.

See also mand-requested-bounds.")

(defvar mand-character-rectangle-size nil
  "Internal variable used by mand-mode.  Buffer-local.
The number of columns and lines in the mandelbrot plot - a pair of
the form (COLUMNS . LINES).")

(defun mand-canonify-rectangle (rect)
  "Re-arrange RECT so that the car is northwest of the cdr.
This returns a new rectangle (a pair of complex numbers), with the
coordinates rearranged so that both coordinates of the car are less
than or equal to the corresponding coordinates of the cdr."
  (let ((c1 (car rect))
	(c2 (cdr rect)))
    (cons (cons (min (car c1) (car c2))
		(min (cdr c1) (cdr c2)))
	  (cons (max (car c1) (car c2))
		(max (cdr c1) (cdr c2))))))

(defsubst mand-rect-width (rect)
  "Return the width of rectangle RECT.
RECT must be in canonical form."
  (- (car (cdr rect)) (car (car rect))))

(defsubst mand-rect-height (rect)
  "Return the height of rectangle RECT.
RECT must be in canonical form."
  (- (cdr (cdr rect)) (cdr (car rect))))

(defun mand-choose-plotting-bounds (columns lines)
  "Set mand-plotting-bounds to fit mand-requested-bounds in COLUMNS x LINES.
This function chooses a value for mand-plotting-bounds that is as small
as possible while completely enclosing mand-requested-bounds and
having the same aspect ratio as COLUMNS x LINES."

  ;; Canonify the bounding rectangles.
  (setq mand-requested-bounds (mand-canonify-rectangle mand-requested-bounds))

  ;; This error message may be more informative than a simple
  ;; "Arithmetic Error" (and definitely preferable to a breakpoint
  ;; trap on a DECstation).
  (or (/= mand-char-aspect-ratio 0)
      (error "mand-char-aspect-ratio should not be zero"))

  ;; Characters aren't square; derive the size of the window
  ;; in a way that uses the same units for the horizontal and
  ;; vertical directions.
  (let* ((width columns)
	 (height (* lines mand-char-aspect-ratio)))
      
    ;; We will probably have some slack - will it be on the top and
    ;; bottom, or on the sides?  Imagine the window scaled to have
    ;; the same width as -request-, and see if it overflows -request-.
    (let ((height-if-widths-match
	   (/ (float (* height (mand-rect-width mand-requested-bounds)))
	      width)))

      (if (> height-if-widths-match (mand-rect-height mand-requested-bounds))

	  ;; The plot will be higher than the requested bounds - slack
	  ;; on the top and bottom.
	  (let ((slack (/ (- height-if-widths-match
			     (mand-rect-height mand-requested-bounds))
			  2)))
	    (setq mand-plotting-bounds
		  (cons (cons (car (car mand-requested-bounds))
			      (- (cdr (car mand-requested-bounds)) slack))
			(cons (car (cdr mand-requested-bounds))
			      (+ (cdr (cdr mand-requested-bounds)) slack)))))

	;; The plot will be lower than the requested bounds - slack
	;; on the left and right.
	(let* ((width-if-heights-match
	       (/ (float (* width (mand-rect-height mand-requested-bounds)))
		  height))
	       (slack (/ (- width-if-heights-match
			    (mand-rect-height mand-requested-bounds))
			 2)))
	  (setq mand-plotting-bounds
		(cons (cons (- (car (car mand-requested-bounds)) slack)
			    (cdr (car mand-requested-bounds)))
		      (cons (+ (car (cdr mand-requested-bounds)) slack)
			    (cdr (cdr mand-requested-bounds))))))))))

(defun mand-col-row-to-complex (column row)
  "Return the complex point corresponding to the character position COL, ROW."
  (cons (+ (car (car mand-plotting-bounds))
	   (/ (float (* column (mand-rect-width mand-plotting-bounds)))
	      (car mand-character-rectangle-size)))
	(+ (cdr (car mand-plotting-bounds))
	   (/ (float (* row (mand-rect-height mand-plotting-bounds)))
	      (cdr mand-character-rectangle-size)))))
			       

;;;; Plotting the set.

(defvar mand-overlay-list nil
  "Internal variable used by mand-mode.  Buffer-local.
This is a list of the overlays we've created in this buffer, so we
can delete them when we start a new plot.")


(defun mand-insert-display (rect width height)
  "Insert a plot of the mandelbrot set in RECT, WIDTH x HEIGHT in size.
The plot is rectangle of text WIDTH characters wide and HEIGHT lines high.
RECT should be a rectangle, in canonical form."

  (mand-build-faces)
    
  ;; Arrange c1 and c2 so that c1 is the upper-left corner of the
  ;; rectangle, and c2 the lower right.
  (let ((c1 (car rect))
	(c2 (cdr rect)))

    (let ((message
	   (let ((float-output-format "%.2g"))
	     (format "Plotting %f+%fi to %f+%fi..."
		     (car c1) (cdr c1) (car c2) (cdr c2)))))

      (message message)

      ;; Set c2 to be the width and height of the area.
      (setq c2 (cons (- (car c2) (car c1))
		     (- (cdr c2) (cdr c1))))

      (let (ix rx start input-available)
	(setq ix 0)
	(while (and (< ix height) (not input-available))
	  (setq rx 0
		start (point))
	  (insert-char ?\  width)
	  (while (< rx width)
	    (let ((not-in-set (not-mand-member-p
			       (cons (+ (/ (float (* rx (car c2))) width)
					(car c1))
				     (+ (/ (float (* ix (cdr c2))) height)
					(cdr c1))))))
	      ;; Text properties are terribly slow (I think they
	      ;; weren't implemented very carefully), so it's faster
	      ;; to use overlays here, believe it or not.  But, geez,
	      ;; it would be really nice to be able to cut and paste
	      ;; the Mandelbrot display.
	      (let ((o (make-overlay (+ start rx) (+ start rx 1))))
		(setq mand-overlay-list (cons o mand-overlay-list))
		(overlay-put
		 o
		 'face (if not-in-set
			   (aref mand-fleeing-faces
				 (% not-in-set (length mand-fleeing-faces)))
			 'mand-member))))
	    (setq rx (1+ rx)))
	  (setq input-available (not (sit-for 0)))
	  (insert ?\n)
	  (setq ix (1+ ix)))

	(message "%s %s"
		 message
		 (if input-available "Interrupted." "Done."))))))


;;;; User interface.

;;;###autoload
(defun mandelbrot ()
  "Plot the mandelbrot set in the current window.
This uses spaces for pixels, so the resolution usually isn't very
high, unless you're using a very small font."
  (interactive)

  (let ((buffer (get-buffer-create "*Mandelbrot Set*")))
    (set-buffer buffer)
    (mand-mode)
    (switch-to-buffer buffer)
    (setq mand-requested-bounds '((-2 . -2) . (2 . 2)))
    (setq mand-character-rectangle-size
	  (cons (1- (window-width (selected-window)))
		(1- (window-height (selected-window)))))

    (mand-refresh)))


(defun mand-refresh ()
  "Plot the mandelbrot set in the current buffer.
Choose the scale such that the area specified in mand-requested-bounds
is as large as possible, but fits within the buffer's window."
  (interactive)

  (message "Plotting (getting ready)...")

  (let ((window (get-buffer-window (current-buffer))))

    ;; If this buffer is currently visible in a window, adjust it to
    ;; fit that window's boundaries.
    (if window
	(setq mand-character-rectangle-size
	      (cons (1- (window-width window))
		    (1- (window-height window)))))

    (mand-choose-plotting-bounds (car mand-character-rectangle-size)
				 (cdr mand-character-rectangle-size))

    ;; If the user hasn't specified one, choose a color scale function.
    (or mand-fleeing-color-function
	(setq mand-fleeing-color-function (mand-choose-color-scale window)))

    (erase-buffer)
    (mapcar 'delete-overlay mand-overlay-list)
    (setq mand-overlay-list nil)

    (mand-insert-display mand-plotting-bounds
			 (car mand-character-rectangle-size)
			 (cdr mand-character-rectangle-size))
    (goto-char (point-min))
    (if window (set-window-start window (point)))))
      
(defun mand-zoom-in (event)
  "Magnify the area around the mouse click by a factor of two."
  (interactive "e")
  ;; Swallow the mouse-up event.
  ;; This kludge is necessary because we have to bind mand-zoom-in
  ;; to down-mouse-1, because there's a global binding for
  ;; down-mouse-1, which we have to mask; we can't just define it to
  ;; do nothing, because then describe-key won't work correctly.
  (read-event)
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (let ((new-center
	   (mand-col-row-to-complex (current-column)
				    (progn
				      (beginning-of-line)
				      (count-lines (point-min) (point)))))
	  (half-new-width
	   (/ (float (mand-rect-width  mand-requested-bounds)) 4))
	  (half-new-height
	   (/ (float (mand-rect-height mand-requested-bounds)) 4)))
      (setq mand-requested-bounds
	    (cons (cons (- (car new-center) half-new-width)
			(- (cdr new-center) half-new-height))
		  (cons (+ (car new-center) half-new-width)
			(+ (cdr new-center) half-new-height))))

      (mand-refresh))))

(defun mand-zoom-out (event)
  "Unzoom, showing four times the area current displayed, centered at the click."
  (interactive "e")
  ;; Swallow the mouse-up event.
  (read-event)
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (let ((new-center
	   (mand-col-row-to-complex (current-column)
				    (progn
				      (beginning-of-line)
				      (count-lines (point-min) (point)))))
	  (half-new-width
	   (mand-rect-width  mand-requested-bounds))
	  (half-new-height
	   (mand-rect-height mand-requested-bounds)))
      (setq mand-requested-bounds
	    (cons (cons (- (car new-center) half-new-width)
			(- (cdr new-center) half-new-height))
		  (cons (+ (car new-center) half-new-width)
			(+ (cdr new-center) half-new-height))))

      (mand-refresh))))
    
(defun mand-circle-radius-two ()
  "Change scale so that the whole Mandelbrot set just fits in the window."
  (interactive)
  (setq mand-requested-bounds '((-2 . -2) . (2 . 2)))
  (mand-refresh))

(defun mand-move-subr (delta)
  "Move the viewing area in the direction specified by DELTA, a complex."
  (let* ((scaled-delta
	  (cons (* (car delta)
		   (mand-rect-width mand-requested-bounds))
		(* (cdr delta)
		   (mand-rect-height mand-requested-bounds)))))
    (setq mand-requested-bounds
	  (cons (cons (+ (car (car mand-requested-bounds))
			 (car scaled-delta))
		      (+ (cdr (car mand-requested-bounds))
			 (cdr scaled-delta)))
		(cons (+ (car (cdr mand-requested-bounds))
			 (car scaled-delta))
		      (+ (cdr (cdr mand-requested-bounds))
			 (cdr scaled-delta)))))
    
    (mand-refresh)))

(defun mand-move-up (prefix)
  "Shift the viewing area up the plane (towards negative imaginary infinity)."
  (interactive "p")
  (mand-move-subr (cons 0 (- prefix))))

(defun mand-move-down (prefix)
  "Shift the viewing area down the plane (towards positive imaginary infinity)."
  (interactive "p")
  (mand-move-subr (cons 0 prefix)))

(defun mand-move-left (prefix)
  "Shift the viewing area left on the plane (towards negative real infinity)."
  (interactive "p")
  (mand-move-subr (cons (- prefix) 0)))

(defun mand-move-right (prefix)
  "Shift the viewing area right on the plane (towards positive real infinity)."
  (interactive "p")
  (mand-move-subr (cons prefix 0)))


(defvar mand-mode-map
  (let ((mand-mode-map (make-sparse-keymap)))
    (define-key mand-mode-map [down-mouse-1] 'mand-zoom-in)
    (define-key mand-mode-map [down-mouse-2] 'mand-zoom-out)
    (define-key mand-mode-map " "     'mand-refresh)
    (define-key mand-mode-map "\C-m"  'mand-refresh)
    (define-key mand-mode-map "t"     'mand-circle-radius-two)
    (define-key mand-mode-map [up]    'mand-move-up)
    (define-key mand-mode-map [down]  'mand-move-down)
    (define-key mand-mode-map [left]  'mand-move-left)
    (define-key mand-mode-map [right] 'mand-move-right)
    mand-mode-map)
  "Keymap used for mand-mode, the Mandelbrot set navigation mode.")

(defun mand-mode ()
  "Major mode for navigating mandelbrot sets.
\\<mand-mode-map>
\\[mand-zoom-in] magnifies the area around the mouse click by a factor of two.
\\[mand-zoom-out] zooms out, placing the point clicked upon at the center
	of the new screen.
\\[mand-refresh] recalculates the display.  If you have changed the
	size of the window, the new plot is resized to fit within the
	new bounds.
The arrow keys shift the viewing area.
\\[mand-circle-radius-two] zooms out to your original position, viewing the
	entire Mandelbrot set.

If your display supports a lot of colors, you may want to increase the
value of the variable `mand-num-fleeing-colors', which specifies the
number of different colors to use to display fleeing points.

\\{mand-mode-map}"
  (interactive)
  (setq major-mode 'mand-mode
	mode-name "Mandelbrot")
  (use-local-map mand-mode-map)
  (set (make-local-variable 'mand-requested-bounds) nil)
  (set (make-local-variable 'mand-plotting-bounds) nil)
  (set (make-local-variable 'mand-overlay-list) nil)
  (set (make-local-variable 'mand-character-rectangle-size) nil))

(put 'mand-mode 'mode-class 'special)
	


(provide 'mandel)
