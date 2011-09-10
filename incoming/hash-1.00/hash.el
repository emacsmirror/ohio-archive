;;; hash.el --- hash table support for Emacs Lisp library code

;; Copyright (C) 1999
;;        Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 27 February 1999
;; Version: 1.00
;; Keywords: associative arrays, hash tables, key-value maps, scatter tables

;; This file is part of GNU Emacs.

;;; Commentary:

;; This is a collection of routines to provide fast dynamically-resized
;; hash table support for Emacs Lisp library code.  Because the code
;; uses floating-point arithmetic, this code will run only in emacs
;; version 19 or later.
;;
;; Hash tables provide key/value association with O(1) cost of
;; insertion, deletion, and lookup.  Ordinary lists, association lists,
;; and vectors, of N elements in Emacs Lisp may have O(N) cost for some
;; of these operations.
;;
;; Because knowledge of the cost of these functions may be critical to
;; the caller, each function's documentation string ends with a
;; bracketed cost estimate as a final paragraph.
;;
;; The code here has been modeled on a small set of functions (about 60
;; lines, or less than 8% of this file) borrowed from the file
;; emacs-20.3.6/src/ange-ftp.el, on the hash functions defined in Common
;; Lisp, and on the hash functions defined by Brian W. Kernighan and Rob
;; Pike in ``The Practice of Programming'', Addison-Wesley, 1999, ISBN
;; 0-201-61586-X.  However, the function names have been changed to use
;; a common prefix, hash-, as is conventional in Emacs Lisp libraries,
;; the argument lists have sometimes been extended, and several
;; additional functions are provided.
;;
;; ------------------------------------------------------------------------
;; PUBLIC FUNCTIONS:
;;
;; Always-used functions:
;;
;;	add entry to table:		hash-put-entry		[cost: O(1),
;;								but O(N) if
;;								table is grown
;;								in this call]
;;
;;	retrieve entry from table:	hash-get-entry		[cost: O(1)]
;;
;; Commonly-used functions:
;;
;;	create table:			hash-create-table	[cost: O(N)]
;;
;;	delete table:			hash-delete-table	[cost: O(N)]
;;
;;	delete entry from table:	hash-delete-entry	[cost: O(1)]
;;
;;	test for key/value in table:	hash-entry-p		[cost: O(1)]
;;
;; 	apply a function to all
;;	entries in table:		hash-apply		[cost: O(N)]
;;
;; Rarely-used functions:
;;
;;	test if object is a hash table:	hash-table-p		[cost: O(1)]
;;
;; 	apply a function to all entries
;;	in table, in sorted order:	hash-apply-sorted	[cost: O(N)]
;;
;; 	return a list of keys:		hash-get-key-list 	[cost: O(N)]
;;
;; 	return a sorted list of keys:	hash-get-key-list-sorted
;;								[cost: O(N lg N)]
;;
;;	return a list of (key value)
;;	sublists:			hash-get-key-value-list [cost: O(N)]
;;
;;	return a sorted list of
;;	(key value) sublists:		hash-get-key-value-list-sorted
;;								[cost: O(N ln N)]
;;
;;	return current
;;	case-insensitive flag:		hash-get-case-insensitive
;;								[cost: O(1)]
;;
;;	return count of key/value pairs
;;	in hashtable:			hash-get-cursize	[cost: O(1)]
;;
;;	return size of hashtable:	hash-get-maxsize	[cost: O(1)]
;;
;; 	return hashtable rehash size:	hash-get-rehash-size	[cost: O(1)]
;;
;; 	return hashtable rehash
;;	threshold:			hash-get-rehash-threshold [cost: O(1)]
;;
;; ------------------------------------------------------------------------
;; Notes:
;;	* Except for hash-table-p, all public functions taking a
;; 	  non-nil hashtable argument assume that it is a valid hashtable
;; 	  properly initialized by hash-create-table.  They do NOT check
;; 	  it for validity or consistency, since that is a relatively
;; 	  expensive operation (though still O(1)).
;;
;;	* All functions return a value, as documented in their docstring.
;;
;;	* Hash keys may be ANY Emacs Lisp object; they will be
;;	  converted to strings internally using prin1-to-string if
;;	  necessary.  Letter-case is significant in key comparisons. If
;;	  you want to have case-insensitive string comparisons, an
;;	  optional argument to hash-create-table can request this.
;;
;;	* All public functions that take a hashtable argument allow it
;;	  to be omitted, or equivalently, given as nil: a public
;;	  global, hash-default-hashtable, will then be used instead.
;;	  This is very convenient if you only need to store and retrieve
;;	  key/value pairs, and are unconcerned about possibly overriding
;;	  key/value associations stored by some other package, since you
;;	  can then use just hash-put-entry and hash-get-entry.  However,
;;	  package authors should ALWAYS use a private hashtable.
;;
;; =====================================================================
;;
;; PUBLIC GLOBAL VARIABLES:
;;	default hash table:		hash-default-hashtable
;;
;; =====================================================================
;;
;; PRIVATE INTERNAL CONSTANTS AND FUNCTIONS (NOT intended for use
;; outside this file, and thus not further documented here in this
;; preamble):
;;
;;	hash-internal-copy-obarray 				[cost: O(N)]
;;	hash-internal-default-rehash-size			[cost: 0]
;;	hash-internal-default-rehash-threshold			[cost: 0]
;;	hash-internal-default-size				[cost: 0]
;;	hash-internal-get-case-insensitive			[cost: O(1)]
;;	hash-internal-get-cursize 				[cost: O(1)]
;;	hash-internal-get-maxsize 				[cost: O(1)]
;;	hash-internal-get-obarray 				[cost: O(1)]
;;	hash-internal-get-rehash-size 				[cost: O(1)]
;;	hash-internal-get-rehash-threshold 			[cost: O(1)]
;;	hash-internal-make-hash-area				[cost: O(1)]
;;	hash-internal-make-key					[cost: O(1)]
;;	hash-internal-object-size				[cost: 0]
;;	hash-internal-set-case-insensitive			[cost: O(1)]
;;	hash-internal-set-cursize 				[cost: O(1)]
;;	hash-internal-set-maxsize 				[cost: O(1)]
;;	hash-internal-set-maxsize-with-checks 			[cost: O(1)]
;;	hash-internal-set-obarray 				[cost: O(1)]
;;	hash-internal-set-rehash-size 				[cost: O(1)]
;;	hash-internal-set-rehash-size-with-checks		[cost: O(1)]
;;	hash-internal-set-rehash-threshold 			[cost: O(1)]
;;	hash-internal-set-rehash-threshold-with-checks		[cost: O(1)]
;;
;;; Code:

(require 'primes)
(provide 'hash)


(defconst hash-version "1.00"
  "Version number of hash library.")


(defconst hash-date "[01-Mar-1999]"
  "Revision date of hash library.")


;;;; ------------------------------------------------------------
;;;; Hash table support.
;;;; ------------------------------------------------------------

;;----------------------------------------------------------------------
;; PRIVATE INTERNAL FUNCTIONS:
;;
;; Functions in this section are intended to be used exclusively by
;; functions in following sections, and are NOT to be used outside this
;; file.  That way, the details of the hash table representation are
;; concealed from user code, and can be changed without fear of
;; invalidating user code.
;;
;; Nothing below this section knows anything about the internal
;; representation of hash tables, except the names and types of their
;; components.  The components themselves are accessed exclusively by
;; the hash-internal-get-* and hash-internal-set-* functions.
;;
;; A hash table is implemented as a vector of six elements:
;;
;;	0:	obarray (a low-level hash table of fixed size,
;;		implemented as a vector of lists of overflow chains,
;;		by the Emacs Lisp kernel in C).
;;
;;	1:	maximum length of obarray vector (this is always forced
;;		to the nearest prime number equal to, or greater than,
;;		the requested size, for collision-frequency reduction).
;;
;;	2:	current number of elements stored in obarray.
;;
;;	3:	rehash-size: an integer greater than zero specifying the
;;		number of elements to add when the table becomes full,
;;		or a floating-point number greater than one, which is
;;		the ratio of the new size to the old size.
;;
;;	4:	rehash-threshold: an integer between 1 and rehash-size,
;;		inclusive (in which case the table is grown when the
;;		number of elements stored exceeds that value), or a
;;		floating-point number between zero and one, exclusive,
;;		representing the fraction of the table that is allowed
;;		to be filled before growing it.  If it is an integer
;;		value, it is adjusted suitably when the table grows.
;;
;;	5:	case-insensitive: non-nil if key comparisons in this
;;		table are to ignore lettercase.
;;
;; For speed, all of the hash-internal-get-* and hash-internal-set-*
;; functions are implemented as inline functions, using (defsubst ...)
;; instead of (defun ...).  They are all very short, so their inlining
;; will not significantly impact byte-compiled code size.


(defconst hash-default-hashtable nil
  "Default hash table to use when the hashtable argument is omitted.")


(defconst hash-internal-default-rehash-size 1.5
  "Default rehash size for hash tables.")


(defconst hash-internal-default-rehash-threshold 0.9
  "Default rehash threshold value for hash tables.

According to Donald E. Knuth, ``The Art of Computer Programming, Volume
3, Sorting and Searching'', second edition, 1998, Fig. 44, p. 545, ISBN
0-201-89685-0, with separate chaining (which is what the Emacs kernel
obarray code does), a hash table load factor of 0.9 guarantees an
average number of probes below 1.5 for both successful and unsuccessful
searches.")


(defconst hash-internal-default-size 31
  "Default size of a new hash table.")


(defconst hash-internal-object-size 6
  "Number of elements in the private internal object that represents a
hash table.")


(defun hash-internal-copy-obarray (new-obarray old-obarray)
  "Copy all elements of OLD-OBARRAY into NEW-OBARRAY, and return NEW-OBARRAY.

\[cost: O(N)]"
  (mapatoms
   (function
    (lambda (oldsym)
      (let* ((key (get oldsym 'key))
	     (newsym (intern key new-obarray)))
	(put newsym 'key key)
	(put newsym 'val (get oldsym 'val)))))
   old-obarray)
  new-obarray)


(defsubst hash-internal-get-case-insensitive (hashtable)
  "Return the case-insensitive flag of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 5))


(defsubst hash-internal-get-cursize (hashtable)
  "Return the current size of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 2))


(defsubst hash-internal-get-maxsize (hashtable)
  "Return the maximum size of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 1))


(defsubst hash-internal-get-obarray (hashtable)
  "Return the obarray element of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 0))


(defsubst hash-internal-get-rehash-size (hashtable)
  "Return the rehash size of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 3))


(defsubst hash-internal-get-rehash-threshold (hashtable)
  "Return the rehash threshold of HASHTABLE.

\[cost: O(1)]"
  (aref hashtable 4))


(defsubst hash-internal-hashtable (&optional hashtable)
  "Return HASHTABLE if it is non-nil and not omitted, else
hash-default-hashtable, which will be created if necessary.

\[cost: O(1)]"
  (cond
   (hashtable hashtable)
   (hash-default-hashtable hash-default-hashtable)
   (t (setq hash-default-hashtable (hash-create-table)))))


(defsubst hash-internal-make-hash-area ()
  "Return a low-level empty hash area.

\[cost: O(1)]"
  (make-vector hash-internal-object-size 0))


(defsubst hash-internal-make-key (key hashtable)
  "Convert KEY into a suitable key for HASHTABLE, and return the new key.

\[cost: O(1)]"
  (let ((new-key (if (stringp key)
		     key
		   (prin1-to-string key))))
    (cond
     ((hash-internal-get-case-insensitive hashtable) (downcase new-key))
     (t new-key))))


(defsubst hash-internal-set-case-insensitive (value hashtable)
  "Set the case-insensitive flag of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 5 value))


(defsubst hash-internal-set-cursize (value hashtable)
  "Set the current size of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 2 value))


(defsubst hash-internal-set-maxsize (value hashtable)
  "Set the maximum size of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 1 value))


(defsubst hash-internal-set-maxsize-with-checks (value hashtable)
  "Set the maximum size of HASHTABLE to the nearest prime not less than
VALUE, and return that new size.

\[cost: O(1)]"
  (aset hashtable 1 (this-or-next-prime value)))


(defsubst hash-internal-set-obarray (value hashtable)
  "Set the obarray element of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 0 value))


(defsubst hash-internal-set-rehash-size (value hashtable)
  "Set the rehash size of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 3 value))


(defsubst hash-internal-set-rehash-size-with-checks (value hashtable)
  "Set the rehash size of HASHTABLE to the closest acceptable value to
VALUE, and return that new value.

\[cost: O(1)]"

  (hash-internal-set-rehash-size
   (or
    (and (integerp value) (> value 0) value)
    (and (floatp value) (> value 1.0) value)
    hash-internal-default-rehash-size)
  hashtable))


(defsubst hash-internal-set-rehash-threshold (value hashtable)
  "Set the rehash threshold of HASHTABLE to VALUE, and return VALUE.

\[cost: O(1)]"
  (aset hashtable 4 value))


(defsubst hash-internal-set-rehash-threshold-with-checks (value hashtable)
  "Set the rehash threshold of HASHTABLE to the closest acceptable value
to VALUE, and return that new value.

\[cost: O(1)]"
  (hash-internal-set-rehash-threshold
     (or
      (and (integerp value) (< 0 value)
	   (<= value (hash-internal-get-maxsize hashtable)) value)
      (and (floatp value) (< 0.0 value) (< value 1.0) value)
      hash-internal-default-rehash-threshold)
  hashtable))


(defun hash-internal-maybe-grow-hashtable (hashtable)
  "Check whether there is space to add a new entry to HASHTABLE, and if
not, create an enlarged copy, overwritten on the old one.  Return
HASHTABLE.

\[cost: O(1), or O(N) if table grows]"
  (let* ((maxsize (hash-internal-get-maxsize hashtable))
	 (new-cursize (1+ (hash-internal-get-cursize hashtable)))
	 (rehash-size (hash-internal-get-rehash-size hashtable))
	 (rehash-threshold (hash-internal-get-rehash-threshold hashtable)))
    (if 
	(or
	 (and (floatp rehash-threshold) (> new-cursize (* rehash-threshold maxsize)))
	 (and (integerp rehash-threshold) (> new-cursize rehash-threshold)))
	(hash-internal-grow-hashtable hashtable)))
  hashtable)


(defun hash-internal-grow-hashtable (hashtable)
  "Create and return an enlarged hashtable, overwritten in HASHTABLE."
  (let* ((maxsize (hash-internal-get-maxsize hashtable))
	 (rehash-size (hash-internal-get-rehash-size hashtable))
	 (rehash-threshold (hash-internal-get-rehash-threshold hashtable))
	 (new-maxsize (or
		       (and (floatp rehash-size)
			    (ceiling (* rehash-size (float maxsize))))
		       (and (integerp rehash-size) (+ rehash-size maxsize))
		       (+ 30 maxsize)	;should never happen, but just in case...
		       ))
	 (new-maxsize
	  (next-prime
	   (max new-maxsize (1+ (hash-internal-get-cursize hashtable)))))
					; this ensures that size MUST increase
	 (old-obarray (hash-internal-get-obarray hashtable))
	 (new-obarray (make-vector new-maxsize 0)))
    (hash-internal-copy-obarray new-obarray old-obarray)
    (hash-internal-set-obarray new-obarray hashtable)
    (hash-internal-set-maxsize new-maxsize hashtable)
    (hash-internal-set-rehash-size-with-checks
     (or
      (and (floatp rehash-size) rehash-size)
      (and (integerp rehash-size)
	   (ceiling (* (float new-maxsize)
		       (/ (float rehash-size) (float (max 1 maxsize))))))
      hash-internal-default-rehash-size)
     hashtable)
    (hash-internal-set-rehash-threshold-with-checks
     (or
      (and (floatp rehash-threshold) rehash-threshold)
      (and (integerp rehash-threshold)
	   (floor (* (float rehash-threshold)
		     (/ (float new-maxsize) (float (max 1 maxsize)))))
	   hash-internal-default-rehash-threshold))
     hashtable))
  hashtable)

;;----------------------------------------------------------------------
;; PUBLIC FUNCTIONS:

(defun hash-apply (funct &optional hashtable arg)
  "Call FUNCT on each key and value in HASHTABLE (or if HASHTABLE is
omitted or is nil, hash-default-hashtable), passing ARG as a third
argument to FUNCT.  Return nil.

Key/value pairs are accessed in unpredictable order.

This, and its companion, hash-apply-sorted, is the ONLY way provided to
loop over all key/value pairs in a hash table.

\[cost: O(N)]"
  (mapatoms
   (function
    (lambda (sym)
      (funcall funct (get sym 'key) (get sym 'val) arg)))
   (hash-internal-get-obarray (hash-internal-hashtable hashtable))))


(defun hash-apply-sorted (funct &optional predicate hashtable arg)
  "Call FUNCT on each key and value in HASHTABLE (or if HASHTABLE is
omitted or is nil, hash-default-hashtable), passing ARG as a third
argument to FUNCT.  Return nil.

Key/value pairs are accessed in sorted order by key, where the order
is determined by the PREDICATE function argument.  Its default value
is (function string-lessp), producing keys in ascending order.

This, and its companion, hash-apply, is the ONLY way provided to loop
over all key/value pairs in a hash table.

\[cost: O(N lg N)]"
  (let* ((the-hashtable (hash-internal-hashtable hashtable))
	 (keys (hash-get-key-list-sorted predicate the-hashtable)))
    (mapcar
     (function
      (lambda (key)
	(funcall funct key (hash-get-entry key the-hashtable) arg)))
     keys))
  nil)


(defun hash-create-table (&optional case-insensitive size rehash-size
				    rehash-threshold)
  "Return a newly-constructed hashtable.

If CASE-INSENSITIVE is supplied, and non-nil, then all key string
comparisons in this table will ignore lettercase.  Otherwise, string
comparisons are case sensitive.

SIZE, if supplied, will automatically be adjusted to the nearest prime
number not less than SIZE, to reduce the hash index collision frequency.
Because the hash table will grow automatically as needed, it should
rarely be necessary to specify this argument.

REHASH-SIZE, if supplied, is an integer greater than zero specifying the
number of elements to add when the table becomes full, or a
floating-point number greater than one, which is the ratio of the new
size to the old size.  If REHASH-SIZE is out of bounds, or omitted, a
default value is silently provided.

REHASH-THRESHOLD, if supplied, is an integer between 1 and SIZE,
inclusive, (in which case the table is grown when the number of elements
stored exceeds that value), or a floating-point number between zero and
one, exclusive, representing the fraction of the table that is allowed
to be filled before growing it.  If REHASH-THRESHOLD is out of bounds,
or omitted, a default value is silently provided.

\[cost: O(N)]"
  (let ((hashtable (hash-internal-make-hash-area))
	 (new-size (this-or-next-prime
		    (or
		     (and (integerp size) (> size 0) size)
		     hash-internal-default-size))))

    (hash-internal-set-obarray (make-vector new-size 0) hashtable)
    (hash-internal-set-maxsize-with-checks new-size hashtable)
    (hash-internal-set-case-insensitive case-insensitive hashtable)
    (hash-internal-set-cursize 0 hashtable)
    (hash-internal-set-rehash-size-with-checks rehash-size hashtable)
    (hash-internal-set-rehash-threshold-with-checks rehash-threshold hashtable)

    hashtable))


(defun hash-delete-table (&optional hashtable)
  "Delete the contents of HASHTABLE (or if HASHTABLE is omitted or is
nil, hash-default-hashtable), and return nil.

Always use this function in an assignment, to ensure that your hashtable
variable is kept consistent:
	(setq hashtable (hash-delete-table hashtable))

\[cost: O(N)]"
  (let ((the-hashtable (hash-internal-hashtable hashtable)))
    (hash-apply
     (function (lambda (key value obarr) (unintern key obarr)))
     the-hashtable
     (hash-internal-get-obarray the-hashtable))

    (hash-internal-set-case-insensitive nil the-hashtable)
    (hash-internal-set-cursize nil the-hashtable)
    (hash-internal-set-maxsize nil the-hashtable)
    (hash-internal-set-obarray nil the-hashtable)
    (hash-internal-set-rehash-size nil the-hashtable)
    (hash-internal-set-rehash-threshold nil the-hashtable)

    (cond
     (hashtable (setq hashtable nil))
     (hash-default-hashtable (setq hash-default-hashtable nil))))
  nil)


(defun hash-delete-entry (key &optional hashtable)
  "Delete KEY and its associated value from HASHTABLE (or if HASHTABLE
omitted or is is nil, hash-default-hashtable).  Return t if the
key/value pair existed, else nil.

\[cost: O(1)]"
  (let ((the-hashtable (hash-internal-hashtable hashtable)))
    (if (unintern (hash-internal-make-key key the-hashtable)
		  (hash-internal-get-obarray the-hashtable))
	(hash-internal-set-cursize
	 (max 0 (1- (hash-internal-get-cursize the-hashtable)))
	 the-hashtable))))


(defun hash-entry-p (key &optional hashtable)
  "Return KEY if there is an association for KEY in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable), else nil.

\[cost: O(1)]"
  (let ((the-hashtable (hash-internal-hashtable hashtable)))
    (intern-soft (hash-internal-make-key key the-hashtable)
		 (hash-internal-get-obarray the-hashtable))))


(defun hash-get-case-insensitive (&optional hashtable)
  "Return the case-insensitive flag for HASHTABLE (or if HASHTABLE is
omitted or is nil, hash-default-hashtable).

\[cost: O(1)]"
    (hash-internal-get-case-insensitive (hash-internal-hashtable hashtable)))


(defun hash-get-cursize (&optional hashtable)
  "Return the count of the number of element stored in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable).

\[cost: O(1)]"
    (hash-internal-get-cursize (hash-internal-hashtable hashtable)))


(defun hash-get-entry (key &optional hashtable)
  "Return the value associated with KEY in HASHTABLE (or if HASHTABLE is
omitted or is nil, hash-default-hashtable), else nil.

\[cost: O(1)]"
  (let* ((the-hashtable (hash-internal-hashtable hashtable))
	 (sym (hash-entry-p (hash-internal-make-key key the-hashtable)
			    the-hashtable)))
    (and sym (get sym 'val))))


(defun hash-get-key-list (&optional hashtable)
  "Return a list of all the active keys in HASHTABLE (or if HASHTABLE is
omitted or is nil, hash-default-hashtable), as strings.  The order of
the keys is unpredictable.

\[cost: O(N)]"
  (all-completions "" (hash-internal-get-obarray
		       (hash-internal-hashtable hashtable))))


(defun hash-get-key-list-sorted (&optional predicate hashtable)
  "Return a sorted list of all the active keys in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable), as strings.

Sort order is determined by the PREDICATE function argument.  Its
default value is (function string-lessp), producing keys in ascending
order.

\[cost: O(N lg N)]"
  (sort (all-completions "" (hash-internal-get-obarray
			     (hash-internal-hashtable hashtable)))
	(or predicate (function string-lessp))))


(defun hash-get-key-value-list (&optional hashtable)
  "Return a list of sublist (key value) pairs in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable).  The order of
the keys is unpredictable.

\[cost: O(N)]"
  (mapcar
   (function (lambda (key) (list key (hash-get-entry key hashtable))))
   (hash-get-key-list hashtable)))


(defun hash-get-key-value-list-sorted (&optional predicate hashtable)
  "Return a list of sublists (key value) pairs in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable), sorted by
keys.

Sort order is determined by the PREDICATE function argument.  Its
default value is (function string-lessp), producing keys in ascending
order.

\[cost: O(N ln N)]"
  (mapcar
   (function (lambda (key) (list key (hash-get-entry key hashtable))))
   (hash-get-key-list-sorted predicate hashtable)))


(defun hash-get-maxsize (&optional hashtable)
  "Return the size of HASHTABLE (or if HASHTABLE is omitted or is nil,
hash-default-hashtable).  This includes both empty and used cells, and
is always constrained to be at least as large as the value returned by
hash-get-cursize, for lookup efficiency.

\[cost: O(1)]"
    (hash-internal-get-maxsize (hash-internal-hashtable hashtable)))


(defun hash-get-rehash-size (&optional hashtable)
  "Return the rehash-size of HASHTABLE (or if HASHTABLE is omitted or is
nil, hash-default-hashtable).  This may be either floating-point or
integer.  See the documentation of hash-create-table for interpretation
of this value.

\[cost: O(1)]"
    (hash-internal-get-rehash-size (hash-internal-hashtable hashtable)))


(defun hash-get-rehash-threshold (&optional hashtable)
  "Return the rehash-threshold of HASHTABLE (or if HASHTABLE is omitted
or is nil, hash-default-hashtable).  This may be either floating-point
or integer.  See the documentation of hash-create-table for
interpretation of this value.

\[cost: O(1)]"
    (hash-internal-get-rehash-threshold (hash-internal-hashtable hashtable)))


(defun hash-put-entry (key value &optional hashtable)
  "Record an association between KEY and VALUE in HASHTABLE (or if
HASHTABLE is omitted or is nil, hash-default-hashtable).  Return VALUE.

\[cost: O(1), or O(N) if table grows]"

  (let ((the-hashtable (hash-internal-hashtable hashtable)))
    (hash-internal-maybe-grow-hashtable the-hashtable)

    ;; In order to correctly track the number of elements in the table,
    ;; we use intern-soft to check if the key is already present.  If it
    ;; is not, we call intern to create an entry for it, and then we
    ;; update the cursize value.  Then we install the new key and value.

    (let* ((new-key (hash-internal-make-key key the-hashtable))
	   (sym (intern-soft new-key (hash-internal-get-obarray the-hashtable))))
      (if (null sym)
	  (progn
	    (setq sym (intern new-key (hash-internal-get-obarray the-hashtable)))
	    (hash-internal-set-cursize 
	     (1+ (hash-internal-get-cursize the-hashtable)) the-hashtable)))
      (put sym 'val value)
      (put sym 'key new-key)))
  value)


(defun hash-table-p (object)
  "Return OBJECT if OBJECT is a hash table, else nil.

\[cost: O(1)]"
  (and (vectorp object)
       (= (length object) hash-internal-object-size)
       (vectorp (hash-internal-get-obarray object))
       (>= (hash-internal-get-maxsize object) 0)
       (>= (hash-internal-get-cursize object) 0)
       (<= (hash-internal-get-cursize object)
	   (hash-internal-get-maxsize object))
       (or (integerp (hash-internal-get-rehash-size object))
	   (floatp (hash-internal-get-rehash-size object)))
       (or (integerp (hash-internal-get-rehash-threshold object))
	   (floatp (hash-internal-get-rehash-threshold object)))
       object))

;;; hash.el ends here
