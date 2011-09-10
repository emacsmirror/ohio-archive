;; "Picture mode" -- editing using quarter-plane screen model.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

;; This file is part of GNU Emacs.

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

;;; This can get hacked up, we can't keep all of picture mode around, even if we
;;;  would use it, it would clash as a mode.

(provide 'my-picture)

(defun move-to-column-force (column)
  "Move to column COLUMN in current line.
Differs from move-to-column in that it creates or modifies whitespace
if necessary to attain exactly the specified column."
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
	(indent-to column)
      (if (and (/= col column)
	       (= (preceding-char) ?\t))
	  (let (indent-tabs-mode)
	    (delete-char -1)
            (indent-to col)
            (move-to-column column))))))


;; Picture Movement Commands

(defun picture-end-of-line (&optional arg)
  "Position point after last non-blank character on current line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  (skip-chars-backward " \t" (prog1 (point) (end-of-line))))

(defun picture-forward-column (arg)
  "Move cursor right, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (move-to-column-force (+ (current-column) arg)))

(defun picture-backward-column (arg)
  "Move cursor left, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (move-to-column-force (- (current-column) arg)))

(defun picture-move-down (arg)
  "Move vertically down, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (let ((col (current-column)))
    (picture-newline arg)
    (move-to-column-force col)))

(defconst picture-vertical-step 0
  "Amount to move vertically after text character in Picture mode.")

(defconst picture-horizontal-step 1
  "Amount to move horizontally after text character in Picture mode.")

(defun picture-move-up (arg)
  "Move vertically up, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (picture-move-down (- arg)))

(defun picture-movement-right ()
  "Move right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 1))

(defun picture-movement-left ()
  "Move left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 -1))

(defun picture-movement-up ()
  "Move up after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 0))

(defun picture-movement-down ()
  "Move down after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 0))

(defun picture-movement-nw ()
  "Move up and left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 -1))

(defun picture-movement-ne ()
  "Move up and right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 1))

(defun picture-movement-sw ()
  "Move down and left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 -1))

(defun picture-movement-se ()
  "Move down and right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 1))

(defun picture-set-motion (vert horiz)
  "Set VERTICAL and HORIZONTAL increments for movement in Picture mode.
The mode line is updated to reflect the current direction."
  (setq picture-vertical-step vert
	picture-horizontal-step horiz)
  (setq mode-name
	(format "Picture:%s"
		(car (nthcdr (+ 1 (% horiz 2) (* 3 (1+ (% vert 2))))
			     '(nw up ne left none right sw down se)))))
  ;; Kludge - force the mode line to be updated.  Is there a better
  ;; way to this?
  (set-buffer-modified-p (buffer-modified-p))
  (message ""))

(defun picture-move ()
  "Move in direction of  picture-vertical-step  and  picture-horizontal-step."
  (picture-move-down picture-vertical-step)
  (picture-forward-column picture-horizontal-step))

(defun picture-motion (arg)
  "Move point in direction of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control motion."
  (interactive "p")
  (picture-move-down (* arg picture-vertical-step))
  (picture-forward-column (* arg picture-horizontal-step)))

(defun picture-motion-reverse (arg)
  "Move point in direction opposite of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control motion."
  (interactive "p")
  (picture-motion (- arg)))


;; Picture insertion and deletion.

(defun picture-self-insert (arg)
  "Insert this character in place of character previously at the cursor.
The cursor then moves in the direction you previously specified
with the commands picture-movement-right, picture-movement-up, etc.
Do \\[command-apropos]  picture-movement  to see those commands."
  (interactive "p")
  (while (> arg 0)
    (setq arg (1- arg))
    (move-to-column-force (1+ (current-column)))
    (delete-char -1)
    (insert last-input-char)
    (forward-char -1)
    (picture-move)))

(defun picture-clear-column (arg)
  "Clear out ARG columns after point without moving."
  (interactive "p")
  (let* ((opoint (point))
	 (original-col (current-column))
	 (target-col (+ original-col arg)))
    (move-to-column-force target-col)
    (delete-region opoint (point))
    (save-excursion
     (indent-to (max target-col original-col)))))

(defun picture-backward-clear-column (arg)
  "Clear out ARG columns before point, moving back over them."
  (interactive "p")
  (picture-clear-column (- arg)))

(defun picture-clear-line (arg)
  "Clear out rest of line; if at end of line, advance to next line.
Cleared-out line text goes into the kill ring, as do
newlines that are advanced over.
With argument, clear out (and save in kill ring) that many lines."
  (interactive "P")
  (if arg
      (progn
       (setq arg (prefix-numeric-value arg))
       (kill-line arg)
       (newline (if (> arg 0) arg (- arg))))
    (if (looking-at "[ \t]*$")
	(kill-ring-save (point) (progn (forward-line 1) (point)))
      (kill-region (point) (progn (end-of-line) (point))))))

(defun picture-newline (arg)
  "Move to the beginning of the following line.
With argument, moves that many lines (up, if negative argument);
always moves to the beginning of a line."
  (interactive "p")
  (if (< arg 0)
      (forward-line arg)
    (while (> arg 0)
      (end-of-line)
      (if (eobp) (newline) (forward-char 1))
      (setq arg (1- arg)))))

(defun picture-open-line (arg)
  "Insert an empty line after the current line.
With positive argument insert that many lines."
  (interactive "p")
  (save-excursion
   (end-of-line)
   (open-line arg)))

(defun picture-duplicate-line ()
  "Insert a duplicate of the current line, below it."
  (interactive)
  (save-excursion
   (let ((contents
	  (buffer-substring
	   (progn (beginning-of-line) (point))
	   (progn (picture-newline 1) (point)))))
     (forward-line -1)
     (insert contents))))


;; Picture Tabs
;;; deleted -fer


;; Picture Rectangles

(defconst picture-killed-rectangle nil
  "Rectangle killed or copied by \\[picture-clear-rectangle] in Picture mode.
The contents can be retrieved by \\[picture-yank-rectangle]")

(defun picture-clear-rectangle (start end &optional killp)
  "Clear and save rectangle delineated by point and mark.
The rectangle is saved for yanking by \\[picture-yank-rectangle] and replaced
with whitespace.  The previously saved rectangle, if any, is lost.
With prefix argument, the rectangle is actually killed, shifting remaining
text."
  (interactive "r\nP")
  (setq picture-killed-rectangle (picture-snarf-rectangle start end killp)))

(defun picture-clear-rectangle-to-register (start end register &optional killp)
  "Clear rectangle delineated by point and mark into REGISTER.
The rectangle is saved in REGISTER and replaced with whitespace.
With prefix argument, the rectangle is actually killed, shifting remaining
text."
  (interactive "r\ncRectangle to register: \nP")
  (set-register register (picture-snarf-rectangle start end killp)))

(defun picture-snarf-rectangle (start end &optional killp)
  (let ((column (current-column))
	(indent-tabs-mode nil))
    (prog1 (save-excursion
             (if killp
                 (delete-extract-rectangle start end)
               (prog1 (extract-rectangle start end)
                      (clear-rectangle start end))))
	   (move-to-column-force column))))

(defun picture-yank-rectangle (&optional insertp)
  "Overlay rectangle saved by \\[picture-clear-rectangle]
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is inserted instead,
shifting existing text.  Leaves mark at one corner of rectangle and
point at the other (diagonally opposed) corner."
  (interactive "P")
  (if (not (consp picture-killed-rectangle))
      (error "No rectangle saved.")
    (picture-insert-rectangle picture-killed-rectangle insertp)))

(defun picture-yank-rectangle-from-register (register &optional insertp)
  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
  (interactive "cRectangle from register: \nP")
  (let ((rectangle (get-register register)))
    (if (not (consp rectangle))
	(error "Register %c does not contain a rectangle." register)
      (picture-insert-rectangle rectangle insertp))))

(defun picture-insert-rectangle (rectangle &optional insertp)
  "Overlay RECTANGLE with upper left corner at point.
Optional argument INSERTP, if non-nil causes RECTANGLE to be inserted.
Leaves the region surrounding the rectangle."
  (let ((indent-tabs-mode nil))
    (if (not insertp)
	(save-excursion
	  (delete-rectangle (point)
			    (progn
			      (picture-forward-column (length (car rectangle)))
			      (picture-move-down (1- (length rectangle)))
			      (point)))))
    (push-mark)
    (insert-rectangle rectangle)))


;; Picture Keymap, entry and exit points.

(defun picture-clean ()
  "Eliminate whitespace at ends of lines."
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "[ \t][ \t]*$" nil t)
     (delete-region (match-beginning 0) (point)))))
