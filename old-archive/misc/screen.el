;;; Tools to configure your GNU Emacs windows
;;; Copyright (C) 1991 Kyle E. Jones 
;;;
;;; LCD Archive Entry:
;;; screen|Kyle E. Jones|kyle@uunet.uu.net|
;;; Tools to configure your GNU Emacs windows.|
;;; 1991||~/misc/screen.el.Z|
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
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@uunet.uu.net.

(provide 'screen)

(defun screen-map ()
  "Returns a list containing complete information about the current
configuration of windows and buffers.  Call the function
set-screen-map with this list to restore the current
window/buffer configuration.

This is much like the function window-configuration except that
the informatoin is returned in a form that can be saved and
restored across multiple Emacs sessions."
  (list (window-map) (buffer-map) (position-map)))

(defun set-screen-map (map)
  "Restore the window/buffer configuration described by MAP,
which should be a list previously returned by a call to
screen-map."
  (set-window-map (nth 0 map))
  (set-buffer-map (nth 1 map))
  (set-position-map (nth 2 map)))

(defun window-map ()
  (let (w maps map0 map1 map0-edges map1-edges x-unchanged y-unchanged)
    (setq maps (mapcar 'window-edges (screen-window-list)))
    (while (cdr maps)
      (setq map0 maps)
      (while (cdr map0)
	(setq map1 (cdr map0)
	      map0-edges (screen-find-window-map-edges (car map0))
	      map1-edges (screen-find-window-map-edges (car map1))
	      x-unchanged (and (= (car map0-edges) (car map1-edges))
			       (= (nth 2 map0-edges) (nth 2 map1-edges)))
	      y-unchanged (and (= (nth 1 map0-edges) (nth 1 map1-edges))
			       (= (nth 3 map0-edges) (nth 3 map1-edges))))
	(cond ((and (not x-unchanged) (not y-unchanged))
	       (setq map0 (cdr map0)))
	      ((or (and x-unchanged (eq (car (car map0)) '-))
		   (and y-unchanged (eq (car (car map0)) '|)))
	       (nconc (car map0) (list (car map1)))
	       (setcdr map0 (cdr map1)))
	      (t
	       (setcar map0 (list (if x-unchanged '- '|)
				  (car map0)
				  (car map1)))
	       (setcdr map0 (cdr map1))))))
    (car maps)))

(defun set-window-map (map)
  (if (eq (selected-window) (minibuffer-window))
      (delete-other-windows (next-window (minibuffer-window)))
    (delete-other-windows))
  (let (map-width map-height)
    (setq map-width (screen-compute-map-width map)
	  map-height (screen-compute-map-height map))
    (screen-apply-window-map map (next-window (minibuffer-window)))))

(defun buffer-map ()
  (let ((w-list (screen-window-list))
	b list)
    (while w-list
      (setq b (window-buffer (car w-list))
	    list (cons (list (buffer-file-name b)
			     (buffer-name b))
		       list)
	    w-list (cdr w-list)))
    (nreverse list)))

(defun set-buffer-map (buffer-map)
  (let ((w-list (screen-window-list)) wb)
    (while (and w-list buffer-map)
      (setq wb (car buffer-map))
      (set-window-buffer
       (car w-list)
       (if (car wb)
	   (or (get-file-buffer (car wb))
	       (find-file-noselect (car wb)))
	 (get-buffer-create (nth 1 wb))))
      (setq w-list (cdr w-list)
	    buffer-map (cdr buffer-map)))))

(defun position-map ()
  (let ((sw (selected-window))
	(w-list (screen-window-list))
	list)
    (while w-list
      (setq list (cons (list (window-start (car w-list))
			     (window-point (car w-list))
			     (window-hscroll (car w-list))
			     (eq (car w-list) sw))
		       list)
	    w-list (cdr w-list)))
    (nreverse list)))

(defun set-position-map (position-map)
  (let ((w-list (screen-window-list)) (osw (selected-window)) sw p)
    ;; select a window we don't care about so that when we select
    ;; another window its buffer will be moved up in the buffer
    ;; list.
    (select-window (minibuffer-window))
    (while (and w-list position-map)
      (setq p (car position-map))
      (and (car p) (set-window-start (car w-list) (car p)))
      (and (nth 1 p) (set-window-point (car w-list) (nth 1 p)))
      (and (nth 2 p) (set-window-hscroll (car w-list) (nth 2 p)))
      (and (nth 3 p) (setq sw (car w-list)))
      ;; move this buffer up in the buffer-list
      (select-window (car w-list))
      (setq w-list (cdr w-list)
	    position-map (cdr position-map)))
    (select-window (or sw osw))))

(defun screen-window-list (&optional mini)
  "Returns a list of Lisp window objects for all Emacs windows.
Optional first arg MINIBUF t means include the minibuffer window
in the list, even if it is not active.  If MINIBUF is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (let* ((first-window (next-window (minibuffer-window)))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window mini)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w mini)))
    windows))

(defun screen-apply-window-map (map)
  (let (horizontal)
    (while map
      (cond
       ((numberp (car map)) (setq map nil))
       ((eq (car map) '-) (split-window-vertically))
       ((eq (car map) '|) (split-window-horizontally) (setq horizontal t))
       (t
	(if (cdr map)
	    (enlarge-window
	     (if horizontal
		 (- (/ (* (screen-compute-map-width (car map)) (screen-width))
		       map-width)
		    (1+ (window-width))) ;; 1+ cuz | is part of window
	       (- (/ (* (screen-compute-map-height (car map))
			(1- (screen-height)))
		     map-height)
		  (window-height)))
	     horizontal))
	(if (not (numberp (car (car map))))
	    (screen-apply-window-map (car map)))
	(and (cdr map) (select-window (next-window)))
	(and (cdr (cdr map)) (split-window nil nil horizontal))))
      (setq map (cdr map)))))

(defun screen-apply-window-map (map current-window)
  (let (horizontal)
    (while map
      (cond
       ((numberp (car map)) (setq map nil))
       ((eq (car map) '-))
       ((eq (car map) '|) (setq horizontal t))
       (t
	(if (cdr map)
	    (split-window
	     current-window
	     (if horizontal
		 (1- (/ (* (screen-compute-map-width (car map)) (screen-width))
		       map-width))
	       (/ (* (screen-compute-map-height (car map))
		     (- (screen-height) (window-height (minibuffer-window))))
		  map-height))
	     horizontal))
	(if (not (numberp (car (car map))))
	    (setq current-window
		  (screen-apply-window-map (car map) current-window)))
	(and (cdr map) (setq current-window (next-window current-window)))))
      (setq map (cdr map)))
    current-window ))

(defun screen-find-window-map-edges (map)
  (let (nw-edges se-edges)
    (setq nw-edges map)
    (while (and (consp nw-edges) (not (numberp (car nw-edges))))
      (setq nw-edges (car (cdr nw-edges))))
    (setq se-edges map)
    (while (and (consp se-edges) (not (numberp (car se-edges))))
      (while (cdr se-edges)
	(setq se-edges (cdr se-edges)))
      (setq se-edges (car se-edges)))
    (if (eq nw-edges se-edges)
	nw-edges
      (setq nw-edges (copy-sequence nw-edges))
      (setcdr (nthcdr 1 nw-edges) (nthcdr 2 se-edges))
      nw-edges )))

(defun screen-compute-map-width (map)
  (let ((edges (screen-find-window-map-edges map)))
    (- (nth 2 edges) (car edges))))

(defun screen-compute-map-height (map)
  (let ((edges (screen-find-window-map-edges map)))
    (- (nth 3 edges) (nth 1 edges))))

(defun screen-nullify-map-elements (map &optional buffer-file-name buffer-name
					window-start window-point
					window-hscroll selected-window)
  (let (p)
    (setq p (nth 1 map))
    (while p
      (and buffer-file-name (setcar (car p) nil))
      (and buffer-name (setcar (cdr (car p)) nil))
      (setq p (cdr p)))
    (setq p (nth 2 map))
    (while p
      (and window-start (setcar (car p) nil))
      (and window-point (setcar (cdr (car p)) nil))
      (and window-hscroll (setcar (nthcdr 2 (car p)) nil))
      (and selected-window (setcar (nthcdr 3 (car p)) nil))
      (setq p (cdr p)))))

(defun screen-replace-map-element (map what function)
  (let (mapi mapj p old new)
    (cond ((eq what 'buffer-file-name)
	   (setq mapi 1 mapj 0))
	   ((eq what 'buffer-name)
	    (setq mapi 1 mapj 1))
	   ((eq what 'window-start)
	    (setq mapi 2 mapj 0))
	   ((eq what 'window-point)
	    (setq mapi 2 mapj 1))
	   ((eq what 'window-hscroll)
	    (setq mapi 2 mapj 2))
	   ((eq what 'selected-window)
	    (setq mapi 2 mapj 3)))
    (setq p (nth mapi map))
    (while p
      (setq old (nth mapj (car p))
	    new (funcall function old))
      (if (not (equal old new))
	  (setcar (nthcdr mapj (car p)) new))
      (setq p (cdr p)))))
