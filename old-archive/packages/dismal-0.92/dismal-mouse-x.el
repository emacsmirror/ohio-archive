;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : dismal-mouse-x.el
;;;; Author          : Frank Ritter
;;;; Created On      : Thu Jul 23 00:55:46 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Tue Nov 24 12:58:43 1992
;;;; Update Count    : 16
;;;; 
;;;; PURPOSE
;;;; 	Sets up the mouse for dismal!
;;;; TABLE OF CONTENTS
;;;;
;;;;	i.	Inits, vars, requires, keymap mods
;;;;	I.	dismal-x-mouse functions
;;;;	II.	Utilities
;;;;	III.	Set the keys
;;;; 
;;;; Copyright 1992, Frank E. Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dismal-mouse-x)


;;;
;;;	i.	Inits, vars, requires, keymap mods
;;;

(defvar old-x-button-left-up nil)
(defvar old-x-button-left-down nil)

;; tricky dance to remember old keybindings, but only if dismal 
;; hasn't been already loaded. 
(if (and (eq window-system 'x) (boundp 'mouse-map) mouse-map)
    (progn
      (require 'x-mouse)
      (let ((left-up (lookup-key mouse-map x-button-left-up))
            (left-down (lookup-key mouse-map x-button-left)))
        (if (not (eq 'dismal-x-mouse-display-cell left-up))
            (setq old-x-button-left-up left-up))
        (if (not (eq 'dismal-x-mouse-display-cell left-down))
            (setq old-x-button-left-down left-down))
      )))


;;;
;;;	I.	dismal-x-mouse functions
;;;

(defun dismal-x-mouse-display-cell (arg)
  (cond ((member major-mode '(dismal-mode spa-mode))
         (dismal-visit-cell dismal-current-row dismal-current-col)
         (if (fboundp old-x-button-left-up)
             (funcall old-x-button-left-up arg)))
        (t (if (fboundp old-x-button-left-up)
               (funcall old-x-button-left-up arg)
             (message "You have a dismal buffer somewhere, you're lucky!")))  ))

(defun dismal-x-mouse-set-point (arg)
  "Select Emacs window mouse is on, and move point to mouse position,
even if in dimsal."
  (x-mouse-set-point arg)
  (if (not (member major-mode '(dismal-mode spa-mode)))
      (if (fboundp old-x-button-left-down)
          (funcall old-x-button-left-down arg))
    (dismal-jump-to-cell (- (count-lines (point-min) (point))
                            dismal-first-data-line)
                         (dismal-char-col-to-dismal-col (current-column)))  ))


;;;
;;;	II.	Utilities
;;;


;;;
;;;	III.	Set the keys
;;;

(if (and (eq window-system 'x) (boundp 'mouse-map) mouse-map)
    (progn
(define-key mouse-map x-button-left 'dismal-x-mouse-set-point)
(define-key mouse-map x-button-left-up 'dismal-x-mouse-display-cell)
     ))
