;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : dismal-metacolumn.el
;;;; Author          : Frank Ritter
;;;; Created On      : Mon Jun  1 13:05:14 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Sep  7 14:37:01 1992
;;;; Update Count    : 31
;;;; 
;;;; PURPOSE
;;;; 	Implement metacolumn manipulations for dismal.
;;;; TABLE OF CONTENTS
;;;;	I.	dismal-set-metacolumn
;;;;	II.	dismal-insert-metacolumn-cells
;;;;	III.	dismal-insert-z-box
;;;;	IV.	dismal-align-metacolumns
;;;;	V.	Utilities
;;;; 
;;;; Copyright 1992, Frank E. Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dismal-metacolumn)


;;;
;;;	I.	dismal-set-metacolumn
;;;

(defun dismal-set-metacolumn (initial-col)
 "Set the metacolumn, which is used to create two meta-columns in 
the spreadsheet."
 (interactive "P")
 (let ((new-col (or initial-col 
                (read-from-minibuffer
                  (format "New middle column value (was %s): " 
                          (dismal-convert-number-to-colname dismal-middle-col))
                  (format "%s" (dismal-convert-number-to-colname 
                                  dismal-current-col))
                   minibuffer-local-map t))))
  (setq new-col (dismal-convert-colname-to-number new-col))
  (cond ( (or (not new-col) (numberp new-col))
          (setq dismal-middle-col new-col)
             (setq dismal-middle-col-name
                   (dismal-convert-number-to-colname dismal-middle-col))
             (set-buffer-modified-p t)
             (message "dismal-middle-col set to %s" dismal-middle-col))
       ( t (error "dismal-middle-col must be a number")))))


;;;
;;;	II.	dismal-insert-metacolumn-cells
;;;

(defun dismal-insert-metacolumn-cells (&optional arg col row)
  "Insert ARG cells in the metacolumn that COL (default, current-col) is in,
at ROW (default, current-row)."
 (interactive)
 (if (not arg) (setq arg (abs (- dismal-current-row (dismal-mark-row)))))
 (if (not col) (setq col dismal-current-col))
 (if (not row) (setq row dismal-current-row))
 (dismal-metacolumn-guards)
 (if dismal-interactive-p
     (message "Inserting %s metacolumn cells at %s %s..." arg row col))
 (dismal-save-excursion
 (if (> col dismal-middle-col)
     ;; do column left
     (dismal-insert-range-cells row (1+ dismal-middle-col)
                                    row dismal-max-col arg)
     ;; else do column right
  (dismal-insert-range-cells row 0 row dismal-middle-col arg))))


;;;
;;;	III.	dismal-insert-z-box
;;;

(defun dismal-insert-z-box (initial-arg)
  "Insert ARG rows of cells on each side of dismal-middle-col,
starting at the rows of point and mark, which must be on opposite 
sides of the middle-col."
 (interactive "P")
 (dismal-metacolumn-guards)
        (setq r1r dismal-current-row)
        (setq r1c dismal-current-col)
        (setq r2r (dismal-mark-row))
        (setq r2c (dismal-mark-col))
 (let* ((arg (or initial-arg (abs (- r1r r2r))))
        (dismal-interactive-p nil))
 (if (not (or (and (<= r1c dismal-middle-col) (> r2c dismal-middle-col))
              (and (<= r2c dismal-middle-col) (> r1c dismal-middle-col))))
     (error 
       "Point and mark must be on opposite sides of dismal-middle-col, col %s" 
       dismal-middle-col))
 (message "Inserting Z box of %s cells at rows %s and %s..." arg r1r r2r)
 ;; Chose the row to go first
 (dismal-save-excursion
   (dismal-insert-metacolumn-cells arg r1c r1r)
   (dismal-insert-metacolumn-cells arg r2c r2r)
   (dismal-redraw-range (min r1r r2r) (max r1r r2r)))))


;; (dismal-insert-range-cells 11 0 11 dismal-middle-col 1)
;; (dismal-insert-range-cells 10 (1+ dismal-middle-col) 10 dismal-max-col 1)
;; (dismal-insert-range-cells 10 3 10 5 1)


;;;
;;;	IV.	dismal-align-metacolumns
;;;

(defun dismal-align-metacolumns ()
  "Align the metacolumns so that point and mark are on the same line,
keeping other parts of the columns still aligned."
 (interactive)
 (dismal-metacolumn-guards)
 (dismal-save-excursion
        (setq r1r dismal-current-row)
        (setq r1c dismal-current-col)
        (setq r2r (dismal-mark-row))
        (setq r2c (dismal-mark-col))
 (let* (first-row first-col-start first-col-end
        ;; these are used to do insertion
        second-row second-col-start second-col-end 
        (arg (abs (- r1r r2r))) )
 (if (not (or (and (<= r1c dismal-middle-col) (> r2c dismal-middle-col))
              (and (<= r2c dismal-middle-col) (> r1c dismal-middle-col))))
     (error "Point & mark must be across dismal-middle-col, col %s (aka #%s)"
            (dismal-convert-number-to-colname dismal-middle-col)
            dismal-middle-col))
 (if (= r1r r2r)
     nil    ;; you are done, jump down to telling
 ;; Chose the row to go first, and set the columns up
 (cond ((> r1r r2r)  ;; point is after mark
        (setq first-row r2r) (setq second-row r1r)
        (cond ((> r1c dismal-middle-col)  ;; point is left of mark
               (setq first-col-start 0)
               (setq first-col-end dismal-middle-col)
               (setq second-col-start (1+ dismal-middle-col))
               (setq second-col-end dismal-max-col))
              (t  ;; point is right of mark
               (setq first-col-start (1+ dismal-middle-col))
               (setq first-col-end dismal-max-col)
               (setq second-col-start 0)
               (setq second-col-end dismal-middle-col))))
       (t (setq first-row r1r) (setq second-row r2r)   ;; point is before mark
        (cond ((> r1c dismal-middle-col)  ;; point is left of mark
               (setq first-col-start (1+ dismal-middle-col))
               (setq first-col-end dismal-max-col)
               (setq second-col-start 0)
               (setq second-col-end dismal-middle-col))
              (t  ;; point is right of mark
               (setq first-col-start 0)
               (setq first-col-end dismal-middle-col)
               (setq second-col-start (1+ dismal-middle-col))
               (setq second-col-end dismal-max-col)))))
 (if dismal-interactive-p
     (if (= first-col-start 0)
         (message "Aligning row %s (R) to row %s (L)..." second-row first-row)
       (message "Aligning row %s (R) to row %s (L)..." first-row second-row)))
 ;; Insert some blank cells in front of earlier column
 ;; (my-message "inserting %s %s to %s %s  N cells %s" 
 ;;             first-row first-col-start first-row first-col-end arg)
 (dismal-insert-range-cells first-row first-col-start
                            first-row first-col-end arg)
 ;; Insert some blank cells after the later column
 ;; (my-message "inserting %s %s to %s %s  N cells %s" 
 ;;       (1+ second-row) second-col-start (1+ second-row) second-col-end arg)
 (dismal-insert-range-cells (1+ second-row) second-col-start 
                            (1+ second-row) second-col-end arg)
 ;; Delete blank lines in region
 (dismal-delete-blank-rows (- first-row arg) (+ second-row arg))
 (dismal-redraw-range (max 0 (- first-row arg)) (+ second-row arg)))

 (and dismal-interactive-p
      (progn
        (message "Aligning rows %s to row %s...Finished." first-row second-row)
        (beep t)))  )))


;;;
;;;	V.	Utilities
;;;

;; provides a set of tests to make sure that you can play with the meta-columns
(defmacro dismal-metacolumn-guards ()
  '(cond ((not (aref dismal-mark 0)) (error "Mark not set."))
         ((or (not dismal-middle-col) (not (numberp dismal-middle-col)))
          (error "dismal-middle-col not set."))))
