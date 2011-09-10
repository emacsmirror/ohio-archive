;From: Paul Dworkin <paul@media-lab.media.mit.edu>
;To: allegra!kautz@eddie.mit.edu (Henry Kautz)
;Cc: info-gnu-emacs@prep.ai.mit.edu
;Subject: Re: determining current window line? 
;Date: Sat, 18 Mar 89 15:46:29 EST
;
;    Date: Thu, 16 Mar 89 18:21:25 EST
;    From: allegra!kautz@eddie.mit.edu (Henry Kautz)
;    Subject: determining current window line?
;
;    Is there a function to determine the line point is on relative to the
;    current top of window?  E.g., so that (move-to-window-line (foobar))
;    would be a no-op.  Using count-lines to count between (window-start)
;    and (point) fails in case long lines are wrapped.
;
;
;OK.  You finally made me do it.  I went wandering through the source and
;tried to make some sense of all the motion commands.  The results are
;below, but I ain't proud.
;
;It turns out there are three different types of information you might
;want to know:
;      - cursor position in the window's text, ignoring screen wdith
;		current-line, current-column
;      - cursor position relative to the top right corner of the window
;		current-window-line, current-window-column
;      - cursor position relative to the top right corner of the screen
;		current-screen-line, current-screen-column
;
;Of these,  current-column  already existed.
;
;I know these could be made better/more efficient, but I don't think I can
;work on this for one more second without getting ill.  Others are welcome to
;go at it.  current-window-line  gets its info iteratively.  Blech.
; current-window-column  is a major hack.
;
;Now people can go ahead and tell me which of these functions already existed.
;
;	    -Paul Dworkin    paul@media-lab.media.mit.edu  (617) 253-0668, 0664
;			     Media Labs, E15-318 M.I.T., Cambridge MA  02139
;-----------


(defun current-line ()
  "Return the vertical position of point in the current window.  Top line is 0.
Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

;  current-column  already exists.

(defun current-window-line ()
  "Return the vertical distance between the top of the window and the point.
Differs from  current-line  in that it returns a value based solely
on screen position, unrelated to the text underneath.  I.e. wrapped lines
get counted multiple times."
  (save-excursion
    (vertical-motion 0)
    (let ((p (point))
	  (lines 0))
      (goto-char (window-start))
      (while (< (point) p)
	(vertical-motion 1)
	(setq lines (1+ lines)))
      lines)))

; This is a hack.  It should be done better than all this guessing.
(defun current-window-column ()
  "Return the horizontal distance between the left edge of the window and the
point.  Differs from  current-column  in that it returns a value based solely
on screen position, unrelated to the text underneath."
  (let ((col (+ (% (current-column) (1- (window-width))))))
    (cond ((or truncate-lines
	       (and truncate-partial-width-windows
		    (< (window-width) (screen-width))))
	   (if (>= (current-column) (1- (window-width)))
	       (1- (window-width))
	     col))
	  ((or (/= col 0) (= (current-column) 0))
	   col)
	  (t
	   (save-excursion
	     (let ((p (point)))
	       (end-of-line)
	       (if (= p (point))
		   (1- (window-width))
		 0)))))))

(defun current-screen-line ()
  "Return the horizontal position of the point on the screen.  Top is 0.
This is absolute screen position, unrelated to the screen's contents."
  (+ (car (cdr (window-edges))) (current-window-line)))

(defun current-screen-column ()
  "Return the vertical position of the point on the screen.  Left is 0.
This is absolute screen position, unrelated to the screen's contents."
  (+ (car (window-edges)) (current-window-column)))


