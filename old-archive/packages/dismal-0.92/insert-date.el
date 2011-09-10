;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : insert-date.el
;;;; Author          : Frank Ritter
;;;; Created On      : Sun Oct  6 23:43:48 1991
;;;; Last Modified By: Thomas McGinnis
;;;; Last Modified On: Wed Jul 15 16:35:48 1992
;;;; Update Count    : 7
;;;; 
;;;; PURPOSE
;;;; 	Provides functions to insert the date and time into buffers.
;;;; Original code by Erik Altmann.
;;;;
;;;; TABLE OF CONTENTS
;;;; 	i.	Variables and inits.
;;;;	I.	Insert-date-string
;;;; 
;;;; Copyright 1991, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'insert-date)


;;;
;;; 	i.	Variables and inits.
;;;

(defvar insert-date-with-month-namep nil
  "*Print out the month in insert-date-string as letters, and in
30-Oct-91 order, rather than as 10-30-91.")

;; 3-6-91 -  tested only for march:
(defconst *date-table*
    '(("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4)
      ("May" 5) ("Jun" 6) ("Jul" 7) ("Aug" 8)
      ("Sep" 9) ("Oct" 10) ("Nov" 11) ("Dec" 12))
  "Maps into numbers the month strings returned by current-time-string.")


;;;
;;;	I.	Erik's insert-date-string & insert-time-string
;;;
;;; Code from Erik Altmann on a quick hack to insert date and time on 
;;; headerless files.  Not really necc. for soar-mode, but what the heck.

;; 3-6-91 -
;; (current-time-string)
;; -> "Wed Mar  6 10:31:12 1991"
;;     012345678901234567890123

(defun insert-time-string ()
  "Inserts an Al-like time-stamp after point."
  (interactive)
  (insert-before-markers
   (format "%s%s" (substring (current-time-string) 11 13)
       (substring (current-time-string) 14 16))))

(defun insert-current-time-string ()
  "Inserts a full time-stamp after point."
  (interactive)
  (insert-before-markers
   (format "%s%s" (current-time-string))) )

(defun insert-date-string (arg)
  "Inserts the current date after point, in m-d-y format.  With prefix
argument, inserts the weekday first."
  (interactive "P")
  (let ((time-string (current-time-string)))
    (if arg
        ;; insert day before date:
        (insert-before-markers
        (format "%s " (downcase (substring time-string 0 3)))))
    ;; insert date:
    (insert-before-markers
     (if insert-date-with-month-namep
	 (format "%s-%s-%s -"
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string 4 7)
		 (substring time-string  -2 nil))
	 (format "%s-%s-%s - "
		 (car (cdr (assoc (substring time-string 4 7) *date-table*)))
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string  -2 nil))))))

