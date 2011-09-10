;; baw-alist.el -- Version 1.0

;; association list utilities providing insertion, deletion, sorting
;; fetching off key-value pairs in association lists.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.

;; This software was written as part of the author's official duty as
;; an employee of the United States Government and is thus in the
;; public domain.  You are free to use this software as you wish, but
;; WITHOUT ANY WARRANTY WHATSOEVER.  It would be nice, though if when
;; you use this code, you give due credit to the author.

;; ======================================================================
;; Author:
;;
;; NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                         and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899

;; ======================================================================
;; Modification history:
;;
;; modified:  6-Sep-1989 baw (create alists from flat lists)
;; modified:  1-Sep-1989 baw (updated pd notice)
;; created : 17-Aug-1989 baw

;; ======================================================================
;; features
;;
(provide 'baw-alist)


;; ======================================================================
;; sort an association list so that key-value pair containing key
;; is at the head of the alist
;;
(defun asort (alist-symbol key)
  "Sort the alist referenced by ALIST-SYMBOL so that the key-value
pair matching KEY is at the head of the list. Doesn't affect the order
of any other key-value pair.  Returns the sorted list."
  (set alist-symbol
       (sort (copy-alist (eval alist-symbol))
	     (function (lambda (a b) (equal (car a) key))))))


;; ======================================================================
;; make an alist element
;;
(defun aelement (key value)
  "Returns an alist element by making a list of a cons cell containing
KEY as the car and VALUE as the cdr."
  (list (cons key value)))


;; ======================================================================
;; return the head symbol of alist
;;
(defun aheadsym (alist)
  "Return the key symbol at the head of ALIST."
  (car (car alist)))


;; ======================================================================
;; check for "not headness" of key
;;
(defun anot-head-p (alist key)
  "Check the ALIST to see if the key-value pair matching KEY is at the
head of the list. Returns nil if ALIST is nil or if the key-value pair
is at the head of ALIST. Return t if key-value pair is not at the head
of ALIST.  ALIST is not altered."
  (not (equal (aheadsym alist) key)))


;; ======================================================================
;; put the key value pair into the head of the alist
;;
(defun aput (alist-symbol key &optional value)
  "Inserts into the alist referenced by ALIST-SYMBOL, the key-value
pair made from KEY and optionally, VALUE.  Returns the alist. If the
key-value pair can be found in the alist, and the optional VALUE is
supplied non-nil, then the returned alist will have the value
associated with KEY set to VALUE.  If VALUE is not supplied or is nil,
the key-value pair will not be modified, but will be moved to the head
of the alist. If the key-value pair cannot be found in the alist, it
will be inserted into the head of the alist. Returns nil if ALIST is
nil."
  (let ((elem (aelement key value))
	alist)
    (asort alist-symbol key)
    (setq alist (eval alist-symbol))
    (cond ((null alist) (set alist-symbol elem))
	  ((anot-head-p alist key) (set alist-symbol (nconc elem alist)))
	  (value (setcar alist (car elem)))
	  (t alist))))


;; ======================================================================
;; delete the key from alist
;;
(defun adelete (alist-symbol key)
  "Deletes from the alist referenced by ALIST-SYMBOL, the key-value
pair matching KEY.  Returns the alist."
  (asort alist-symbol key)
  (let ((alist (eval alist-symbol)))
    (cond ((null alist) nil)
	  ((anot-head-p alist key) alist)
	  (t (set alist-symbol (cdr alist))))))


;; ======================================================================
;; return the value associated with key
;;
(defun aget (alist key &optional keynil-p)
  "Returns the value in ALIST that is associated with KEY. Optional
KEYNIL-P describes what to do if the *value* associated with KEY is
nil.  If KEYNIL-P is not supplied or is nil, and the value is nil,
then KEY is returned.  If KEYNIL-P is non-nil, then nil would be
returned.

If no key-value pair matching KEY could be found in ALIST, then nil is
returned.  Returns nil if ALIST is nil. ALIST remains unchanged."

  (let ((copy (copy-alist alist)))
    (cond ((null alist) nil)
	  ((progn (asort 'copy key)
		  (anot-head-p copy key)) nil)
	  ((cdr (car copy)))
	  (keynil-p nil)
	  ((car (car copy)))
	  (t nil))))


;; ======================================================================
;; make an alist from one or two flat lists
;;
(defun amake (alist-symbol keylist &optional valuelist)
  "Make an association list, ALIST-SYMBOL, where each car in KEYLIST
becomes a key and is associated with the appropriate value in
VALUELIST.  If VALUELIST is not supplied or is nil, then each key is
associated with nil.  KEYLIST and VALUELIST should have the same
number of elements, but it isn't necessary; if VALUELIST is smaller,
remaining keys are associated with nil; if VALUELIST is larger, extra
values are ignored.  Returns the alist."
  (let ((keycar (car keylist))
	(keycdr (cdr keylist))
	(valcar (car valuelist))
	(valcdr (cdr valuelist)))
    (cond ((null keycdr)
	   (aput alist-symbol keycar valcar))
	  (t
	   (amake alist-symbol keycdr valcdr)
	   (aput alist-symbol keycar valcar))))
  (eval alist-symbol))
