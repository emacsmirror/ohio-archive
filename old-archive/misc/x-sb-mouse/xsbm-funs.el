;;; xsbm-funs.el : functions used by x-sb-mouse
;;; Copyright (C) 1992 Sullivan Beck
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------


(defun x-mouse-mini-p (arg)
  "If arg is in the minibuffer, return the minibuffer window, else NIL."
  (let* ((s (screen-height))
         (w (minibuffer-window))
         (m (window-height w))
         (y (car (cdr arg))))
    (if (>= y (- s m))
        w
      '())))

(defun x-mouse-mode-p (arg &optional win)
  "If arg is on a mode line, return the window, else NIL."
  (let* ((w (if win win (x-mouse-window arg)))
         (m (eq w (minibuffer-window)))
         (y (car (cdr arg)))
         (bot   (nth 3 (window-edges w))))
    (if (= y (1- bot))
        (if m () w)
      ())))

(defun x-mouse-border-p (arg &optional win)
  "If arg is on a border, returns the window, else NIL."
  (let* ((w (if win win (x-mouse-window arg)))
         (x (car arg))
         (s (screen-width))
         (r (nth 2 (window-edges w))))
    (if (= x (1- r))
        (if (= x (1- s))
            '()
          w)
      '())))

(defun x-mouse-inter-p (arg &optional win)
  "If arg is on an intersection, returns the window, else NIL."
  (let ((w (if win win (x-mouse-window arg))))
    (if (and (x-mouse-mode-p arg w) (x-mouse-border-p arg w))
        w
      '())))

(defun x-mouse-window (arg)
  "Returns the window that the mouse is currently in, even the minibuffer.
Also works for the mode line which the old version didn't."
  (let* ((start-w (selected-window))
         (done nil)
         (x (car arg))
         (y (car (cdr arg)))
         (w start-w)
         (edges nil)
         (top nil)
         (bot nil)
         (left nil)
         (right nil))
    (while (not done)
      (setq edges (window-edges w))
      (setq left  (nth 0 edges))
      (setq top   (nth 1 edges))
      (setq right (nth 2 edges))
      (setq bot   (nth 3 edges))
      (if (and (>= x left) (< x right)
               (>= y top)  (< y bot))
          (setq done t)
        (setq w (next-window w t))
        (if (eq w start-w)
            (setq done t w nil))))
    w))

(defun x-mouse-coords (arg &optional win)
  "Returns the coordinates of the mouse with respect to the window it's in.
NIL is returned if the mouse is on the mode line or border."
  (let* ((w (if win win (x-mouse-window arg)))
         (m (x-mouse-mode-p arg w))
         (b (x-mouse-border-p arg w)))
    (if (or m b)
        ()
      (coordinates-in-window-p arg w))))

(defun x-mouse-pos (win char)
  "Returns the position of char in the window.
The position returned is EXACTLY the same as if a mouse event occurred at
that point.  Win must be a window other than the minibuffer and char must
be visible."
  (let* ((edges (window-edges win))
	 (x     (nth 0 edges))
	 (y     (nth 1 edges))
	 (right (nth 2 edges))
	 (wid   (window-width win))
	 (swid  (screen-width))
	 trun col col0 pt)
    (save-window-excursion
      (select-window win)
      (setq trun truncate-lines
	    col  (current-column)
	    pt   (point))
      (save-excursion
	(goto-char (window-start))
	(if (= (point) pt) ()
	  (if (/= swid right)
	      (setq wid (1- wid)))
	  (while (<= (point) pt)
	    (vertical-motion 1)
	    (setq y (1+ y)))
	  (vertical-motion -1)
	  (setq y    (1- y)
		col0 (current-column)
		x    (+ x (- col (- col0 (% col0 (1- wid)))))))))
    (list x y)))

(defun x-mouse-point (arg &optional win)
  "Returns the POINT of the arg in the window it's in."
  (let* ((w      (if win win (x-mouse-window arg)))
         (coords (x-mouse-coords arg w))
         (x ())
         (y ())
         (margin-column ())
         (prompt-width ()))
    (save-excursion
      (save-window-excursion
        (select-window w)
        (if coords
            (progn
              (setq prompt-width (if (x-mouse-mini-p arg)
                                     (if (boundp 'minibuffer-prompt-width)
                                         minibuffer-prompt-width 0) 0)
                    x (car coords)
                    y (car (cdr coords)))
              (move-to-window-line y)
              (setq margin-column
                    (if (or truncate-lines (> (window-hscroll) 0))
                        (current-column)
                      (- (current-column)
                         (% (current-column) (1- (window-width))))))
              (move-to-column (+ x (1- (max 1 (window-hscroll)))
                                 (if (= (point) 1)
                                     (- prompt-width) 0)
                                 margin-column))))
        (point)))))

(setq x-mouse-resize 
      '((default . x-mouse-resize)))
(defun x-mouse-resize ()
  "Test to see if a resize is possible.  Do it if it is."
  (let* ((sw (selected-window))
         (ow x-mouse-win-d)
         (nw x-mouse-win-u)
         (oy (car (cdr x-mouse-pos-d)))
         (ox (car x-mouse-pos-d))
         (ny (car (cdr x-mouse-pos-u)))
         (nx (car x-mouse-pos-u))
         (ot x-mouse-type-d)
         (nt x-mouse-type-u)
         (edges (window-edges nw))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bot (nth 3 edges)))
    (cond
     ;; drag lowest mode line up
     ((and (eq ot 'mode) (eq nt 'window) (or (= oy (1- bot)) (= oy (1- top)))
           (x-mouse-mini-p (list ox (1+ oy))))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow))
     ;; drag any mode line into a window
     ((and (eq ot 'mode) (eq nt 'window) (or (= oy (1- bot)) (= oy (1- top))))
      (select-window ow)
      (enlarge-window (- ny oy)))
     ;; drag any border into a window
     ((and (eq ot 'border) (eq nt 'window)
           (or (= ox (1- right)) (= ox (1- left))))
      (select-window ow)
      (enlarge-window (- nx ox) t))
     ;; drag an inter into a window
     ((and (eq ot 'inter) (eq nt 'window)
           (or (= oy (1- bot)) (= oy (1- top)) (= ox (1- right))
               (= ox (1- left))))
      (select-window ow)
      (enlarge-window (- ny oy))
      (enlarge-window (- nx ox) t))
     ;; drag lowest mode line down
     ((and (eq ot 'mode) (eq nt 'mini)
           (= oy (- (1- (screen-height)) (window-height (minibuffer-window))))
           (> (- (window-height (minibuffer-window)) (- ny oy)) 0))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow))
     ;; drag an inter in the lowest mode line down
     ((and (eq ot 'inter) (eq nt 'mini)
           (= oy (- (1- (screen-height)) (window-height (minibuffer-window))))
           (> (- (window-height (minibuffer-window)) (- ny oy)) 0))
      (select-window (minibuffer-window))
      (enlarge-window (- oy ny))
      (select-window ow)
      (enlarge-window (- nx ox) t))
    )
    (select-window sw))
  t)
