;; Copyright (C) 2000 Michel Schinz
          
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; $Id: pack-windows.el,v 1.1.1.1 2000/07/10 07:15:58 schinz Exp $

(require 'cl)

(defcustom pw-max-iteration 10
  "`pack-windows' performs at most this number of iterations to pack windows."
  :type 'integer)

(defcustom pw-verbose nil
  "If true, `pack-windows' will say how much iterations it performed."
  :type 'boolean)

;; Information about windows is encoded in pairs: the CAR contains the
;; window, the CDR its ideal or final height.
(defsubst make-win-pair (window ideal-height) (cons window ideal-height))
(defsubst emacs-window (win-pair) (car win-pair))
(defsubst ideal-height (win-pair) (cdr win-pair))
(defsubst set-ideal-height (win-pair height) (setcdr win-pair height))

(defun pw-frame-windows (frame pred)
  "Return a list of all windows in FRAME satisfying PRED, minibuffer excepted."
  (loop for win being the windows of frame
	if (and (not (window-minibuffer-p win))
		(funcall pred win))
	collect win))

;; This is mostly like `window-buffer-height' from windows.el, but
;; with the MAX parameter, and one bug less.
(defun pw-window-buffer-height (window max)
  "Return height (in screen lines) WINDOW's buffer, bounded by MAX."
  (save-excursion
    (set-buffer (window-buffer window))
    (goto-char (point-min))
    (1+ (nth 2 (compute-motion (point-min)
			       '(0 . 0)
			       (point-max)
			       (cons 0 (1- max))
			       (1- (window-width window))
			       nil
			       window)))))

(defun pw-compute-heights (windows avail-height)
  "Compute new heights of WINDOWS so that they fit in AVAIL-HEIGHT."
  (unless (null windows)
    (let* ((avg-height (/ avail-height (length windows)))
	   (fit-in-avg-p #'(lambda (win)
			     (<= (ideal-height win) avg-height)))
	   (fitting-windows (remove-if-not fit-in-avg-p windows)))
      (if (null fitting-windows)
	  ;; No fitting windows, we divide available height among all.
	  (let ((err (% avail-height (length windows))))
	    (loop for win in windows
		  for index from 1 do
		  (set-ideal-height
		   win
		   (if (<= index err) (1+ avg-height) avg-height))))
	;; Some windows fit, we leave them as-is and restart the
	;; process with the remaining windows.
	(pw-compute-heights (remove-if fit-in-avg-p windows)
			    (- avail-height
			       (reduce #'+ fitting-windows
				       :key #'ideal-height)))))))

(defsubst pw-shrink-value (win-pair)
  "Return the difference between the current and ideal size of WIN-PAIR"
  (- (window-height (emacs-window win-pair))
     (ideal-height win-pair)))

(defsubst pw-max-shrink-value (win-pairs)
  "Return the element of WIN-PAIRS with maximum shrink value.
See `pw-shrink-value'."
  (reduce #'(lambda (best new)
	      (if (> (pw-shrink-value new) (pw-shrink-value best))
		  new
		best))
	  win-pairs))

(defun pack-windows ()
  "Resize all windows vertically to display as much information as possible.

Only windows that are on the left edge of the frame are taken into
account. The vertical space available in the frame is first divided
among all these windows. Then any window requireing less lines than it
got to display its whole buffer is shrinked, and the freed space is
divided equally among all the other windows.

If some vertical space remains afterwards, it is given in totality to
the currently selected window.

Do not shrink any window to less than `window-min-height'.

Shrink windows iteratively, performing at most `pw-max-iteration'
iterations. The number of iterations really performed will be
displayed in the echo area if `pw-verbose' is non-nil."
  (interactive)
  (let* ((emacs-windows (pw-frame-windows (selected-frame)
					  #'(lambda (w)
					      (zerop (car (window-edges w))))))
	 (avail-height (reduce #'+ emacs-windows :key #'window-height))
	 (windows (mapcar #'(lambda (win)
			      (make-win-pair win
					     (max window-min-height
						  (1+ (pw-window-buffer-height
						       win
						       (1- avail-height))))))
			  emacs-windows))
	 (desired-height (reduce #'+ windows :key #'ideal-height)))

    ;; If all windows fit, we give any "superfluous" height to the
    ;; first one in the list (the selected one, provided it's aligned
    ;; on the left margin) and proceed. Otherwise, we distribute the
    ;; height available among all windows.
    (if (<= desired-height avail-height)
	(let ((first-win (car windows)))
	  (set-ideal-height first-win (+ (ideal-height first-win)
					 (- avail-height desired-height))))
      (pw-compute-heights windows avail-height))

    ;; At this point, the sum of the ideal heights of all windows is
    ;; guaranteed to be equal to the available height:
    ;(assert (= (reduce #'+ windows :key #'ideal-height)
    ;	       avail-height))

    ;; Resize windows. We have to iterate since resizing one
    ;; window also resizes its neighbours.
    ;; We try our best not to delete any window in the process, but if
    ;; that happens, we restore the current window configuration and
    ;; display a message.
    (let ((win-config (current-window-configuration)))
      (condition-case nil
	  (save-selected-window
	    (when (> (length windows) 1)
	      ;; At each iteration we chose the window that has to be
	      ;; shrinked the most, in an attempt to avoid killing
	      ;; neighbouring windows.
	      (loop for iter from 1 to pw-max-iteration
		    finally (when pw-verbose
			      (message "pack-windows: %d iterations" iter))
		    until (loop for wins = windows then (remove win-pair wins)
				while wins
				for win-pair = (pw-max-shrink-value wins)
				for shrink = (pw-shrink-value win-pair)
				sum shrink into total-shrink
				finally return (zerop total-shrink) do
			    
				(select-window (emacs-window win-pair))
				(shrink-window (pw-shrink-value win-pair))))))
	  (error
	   (message "Cannot pack windows without deleting one, sorry")
	   (set-window-configuration win-config))))

    ;; Display as much information as possible in all windows.
    (save-selected-window
      (dolist (win windows)
	(let ((emacs-win (emacs-window win)))
	  (select-window emacs-win)
	  (when (= (window-end emacs-win) (point-max))
	    (save-excursion
	      (goto-char (point-max))
	      (recenter -1))))))))

(provide 'pack-windows)
