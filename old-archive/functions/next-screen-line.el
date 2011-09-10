;;; next-screen-line.el --- A next-line that moves by screen lines.
;; Copyright (c) 1992 by Govind N. Kamat <kamat@ece.UC.EDU>
;; May be copied, modified and distributed under the
;; GNU Emacs General Public License.
;;
;; Author: Govind N. Kamat <kamat@ece.UC.EDU>
;; Version: 2.1
;; Keywords: next-line wrap line movement scrolling
;;
;;; Commentary:
;; Often having to edit files with more than 80 columns, I prefer that 
;; next-line and previous-line not jump over wrapped continuation 
;; lines.  Here is a drop-in replacement for next-line-internal that 
;; moves the cursor by screen lines rather than newline-separated 
;; lines.  This is especially handy while editing binary files.  The 
;; functions `beginning-of-line' and `end-of-line', bound to C-a and 
;; C-e respectively, are also redefined to behave similarly.
;;
;; Nothing is different when these functions are called 
;; non-interactively, so other packages should not be affected. 
;; During interactive use, the old behavior can be had by setting the 
;; variable `next-line-move-newlines' to true. 
;;
;; Another feature of this package is that if scroll-step is set to 1, 
;; it avoids point being centered in the window.  By default, merely 
;; setting this variable does not suffice, as can be seen when 
;; attempting to scroll rapidly with C-n, and also with wrapped lines. 
;;
;;; Installation:
;; Put this in a file named "next-screen-line.el" in the load-path of 
;; your Emacs.  Byte-compile it with M-x byte-compile-file.  Put the 
;; following line in your .emacs file:
;;
;;   (require 'next-screen-line)
;;
;; LCD Archive Entry:
;; next-screen-line|Govind N. Kamat|kamat@ece.UC.EDU|
;; A next-line that moves by screen lines.|
;; 4-Apr-1993|2.1|~/functions/next-screen-line.el.Z|

(provide 'next-screen-line)

(defvar next-line-move-newlines nil
  "*Make next-line and previous-line move by newline-separated lines instead of
screen lines.")

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.).

If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(next-line-internal arg)
	(if (= opoint (point))
	    (progn (end-of-line) (insert ?\n))))
    (next-line-internal arg))
  (and (= scroll-step 1)
       (> arg 0)
       (or (pos-visible-in-window-p) (recenter -1)))
  nil)

(defun previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with negative argument instead..  It is usually easier
to use and more reliable (no dependence on goal column, etc.).

If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (next-line-internal (- arg))
  (and (= scroll-step 1)
       (> arg 0)
       (or (pos-visible-in-window-p) (recenter 0)))
  nil)

(defun next-line-internal (arg)
  (if (not (memq last-command '(next-line previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		t
	      (current-column))
	    next-line-internal-flag nil))
  (let* ((w (1- (window-width)))
	 (g (or goal-column temporary-goal-column))
	 (pos (if (eq g t) w (% g w)))
	 c)
    (and (= pos 0)
	 (> g 0)
	 (or (and (> (current-column) 0)
		  (= (or (char-after (point)) 10) 10))
	     (>= (or goal-column 0) w))
	 (setq next-line-internal-flag t))
    (if (or next-line-move-newlines
	    ;; maintain compatibility when not interactive
	    (not (memq this-command '(next-line previous-line))))
	(if (not (integerp selective-display))
	    (forward-line arg)
	  (while (> arg 0)
	    (end-of-line)
	    (vertical-motion 1)
	    (setq arg (1- arg)))
	  (while (< arg 0)
	    (beginning-of-line)
	    (vertical-motion -1)
	    (setq arg (1+ arg))))
      (vertical-motion arg)
      (and (eobp) (vertical-motion 0)))
    (cond
     ((eq g t) (end-of-line))
     ((or next-line-move-newlines
	  truncate-lines
	  (> (window-hscroll) 0)
	  (and (< w (1- (screen-width))) truncate-partial-width-windows))
      (move-to-column g))
     (t (setq g (point) c (current-column))
	(setq c (* w (/ c w)))
	(move-to-column (+ (if next-line-internal-flag w pos) c))
	(and (/= g (save-excursion (vertical-motion 0) (point)))
	     (forward-char -1))))))

(fset 'line-move 'next-line-internal)	; for version 18.55

(and (subrp (symbol-function 'beginning-of-line))
     (fset 'real-beginning-of-line (symbol-function 'beginning-of-line)))
(and (subrp (symbol-function 'end-of-line))
     (fset 'real-end-of-line (symbol-function 'end-of-line)))

;; Thanks to Dan LaLiberte for suggesting the following.

(defun beginning-of-line (&optional arg)
  "Move point to beginning of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error.
If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (if (or (interactive-p) executing-kbd-macro)
      (let ((this-command 'next-line)
	    (goal-column 0)
	    next-line-internal-flag)
	(next-line-internal (1- arg)))
    (real-beginning-of-line arg)))

(defun end-of-line (&optional arg)
  "Move point to end of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error.
If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (if (and (or (interactive-p) executing-kbd-macro)
	   (not (or next-line-move-newlines
		    truncate-lines
		    (> (window-hscroll) 0)
		    (and (< (window-width) (screen-width))
			 truncate-partial-width-windows))))
      (let ((this-command 'next-line)
	    (goal-column (1- (window-width)))
	    next-line-internal-flag)
	(next-line-internal (1- arg)))
    (real-end-of-line arg)))
