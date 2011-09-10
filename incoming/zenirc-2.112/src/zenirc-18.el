;;; zenirc-18.el --- compatibility functions for Emacs 18

;;; Copyright (C) 1994, 1995 Noah S. Friedman
;;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, zenirc
;; Created: 1995-01-01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; This file mainly just defines functions used by zenirc which don't exist
;; in emacs 18, but do exist in emacs 19.  Everything here is original with
;; the author unless indicated otherwise.

;; TODO: Fix a current-time[-string].

;;; Code:


;; From GNU Emacs 19.27 subr.el
(defmacro zenirc-save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list
     'let (list (list original '(match-data)))
     (list 'unwind-protect
           (cons 'progn body)
           (list 'store-match-data original)))))

(or (fboundp 'save-match-data)
    (fset 'save-match-data 'zenirc-save-match-data))


;; From emacs 19.29 window.el
(defun zenirc-walk-windows (proc &optional minibuf all-frames)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, when a separate minibuffer frame is active,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.  But if the
minibuffer does not count, only windows from WINDOW's frame count.

third arg ALL-FRAMES is ignored in emacs 18; it exists solely for
compatibility with emacs 19."
  ;; If we start from the minibuffer window, don't fail to come back to it.
  (if (window-minibuffer-p (selected-window))
      (setq minibuf t))
  (let* ((walk-windows-start (selected-window))
	 (walk-windows-current walk-windows-start))
    (while (progn
	     (setq walk-windows-current
		   (next-window walk-windows-current minibuf))
	     (funcall proc walk-windows-current)
	     (not (eq walk-windows-current walk-windows-start))))))

(or (fboundp 'walk-windows)
    (fset 'walk-windows 'zenirc-walk-windows))


(defun zenirc-delete (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'."
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (equal elt (car l))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (equal elt (car list))
      (cdr list)
    list))

(or (fboundp 'delete)
    (fset 'delete 'zenirc-delete))


(defun zenirc-force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL then force then force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(or (fboundp 'force-mode-line-update)
    (fset 'force-mode-line-update 'zenirc-force-mode-line-update))


(defun zenirc-member (x y)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
  (while (and y (not (equal x (car y))))
      (setq y (cdr y)))
    y)

(or (fboundp 'member)
    (fset 'member 'zenirc-member))


;; In emacs 19, the window end is managed internally and getting it is done
;; with a builtin function.  In emacs 18, we have to search for it by
;; scanning forward until point is no longer "visible".

(defun zenirc-window-end (&optional window)
  "Return position at which display currently ends in WINDOW."
  (or window (setq window (selected-window)))
  (let ((orig-buf (current-buffer))
        point-max)
    (set-buffer (window-buffer window))
    (setq point-max (point-max))
    (set-buffer orig-buf)
    (cond
     ((pos-visible-in-window-p point-max window)
      point-max)
     (t
      (let ((incr (min (* (window-width window) (window-height window))
                       (- point-max (window-start window))))
            (end (window-start window)))
        (while (not (zerop (setq incr (/ incr 2))))
          (while (and (< end point-max)
                      (pos-visible-in-window-p end window))
            (setq end (+ end incr)))
          (setq end (- end incr)))
        (1+ end))))))

(or (fboundp 'window-end)
    (fset 'window-end 'zenirc-window-end))


(provide 'zenirc-18)

;;; zenirc-18.el ends here
