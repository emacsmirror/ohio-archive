;;; zenirc-prime-p.el --- flag prime numbers

;; Copyright (C) 1997, 1998 Ray Jones

;; Author: Ray Jones <rjones@pobox.com>
;; Maintainer: rjones@pobox.com
;; Keywords: zenirc, extensions, oink, "mmmm, primes"
;; Created: 1997-11-13


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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary: mmm, primes

;;; Code:

(require 'zenirc)
(require 'zenirc-trigger)

;; the method to find primes isn't fancy, just the sieve of erasthonese

(defvar *zenirc-prime-list* '(2)
  "List of numbers already determined to be prime.")
(defvar *zenirc-prime-list-end* *zenirc-prime-list*
  "Last pair in *zenirc-prime-list*, kept for efficiency reasons.")

(defconst *zenirc-recent-count* 10
  "Number of primes to remember in *zenirc-recent-primes*.")
(defvar *zenirc-recent-primes* (make-list *zenirc-recent-count* 0)
  "Primes recently seen, kept to keep from flagging the same prime over and over.")
  
(defvar *zenirc-next-test* 3
  "Smallest number not yet tested for insertion into *zenirc-prime-list*.")

;; lest this get really out of control
(defconst *zenirc-max-to-store* 10000
  "Maximum value to ever insert into *zenirc-prime-list*.")

(defun zenirc-primep (n)
  (if (< n *zenirc-max-to-store*)
      (if (<= *zenirc-next-test* n)
	  (zenirc-expand-prime-list n)
	(member n *zenirc-prime-list*))
    (let ((test-limit (truncate (sqrt n)))
	  (l *zenirc-prime-list*)
	  (lastmod 1))
      ;; if *zenirc-max-to-store* is less than (sqrt most-positive-fixnum),
      ;; this could expand the list more than *zenirc-max-to-store* should
      ;; allow.
      (zenirc-expand-prime-list test-limit)
      (while (and l
		  (not (= lastmod 0))
		  (<= (car l) test-limit))
	(setq lastmod (mod n (car l)))
	(setq l (cdr l)))
      (not (= lastmod 0)))))

(defun zenirc-expand-prime-list (n)
  (while (<= *zenirc-next-test* n)
    (let ((test-limit (truncate (sqrt *zenirc-next-test*)))
	  (l *zenirc-prime-list*)
	  (lastmod 1))
      (while (and (not (= lastmod 0))
		  (<= (car l) test-limit))
	(setq lastmod (mod *zenirc-next-test* (car l)))
	(setq l (cdr l)))
      (if (not (= lastmod 0))
	  (progn
	    (setcdr *zenirc-prime-list-end* `(,*zenirc-next-test*))
	    (setq *zenirc-prime-list-end* (cdr *zenirc-prime-list-end*)))))
    (setq *zenirc-next-test* (+ 1 *zenirc-next-test*)))
  (= (car *zenirc-prime-list-end*) n))


(defun zenirc-primep-filter (str)
  (let ((num (string-to-int str)))
    (if (and (not (member num *zenirc-recent-primes*))
	     (zenirc-primep num))
	(progn
	  (setq *zenirc-recent-primes* (cons num *zenirc-recent-primes*))
	  (setcdr (nthcdr (- *zenirc-recent-count* 1) *zenirc-recent-primes*) nil)
	  (format "%s is prime." str))
      nil)))

(zenirc-trigger-register "primep" 'zenirc-primep-filter 
			 "[1-9][0-9][0-9][0-9]*" t)

;;; zenirc-prime-p.el ends here.

