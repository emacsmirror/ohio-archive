;;; test-hash.el --- tests of Emacs Lisp library code in hash.el

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
;; This file carries out validation tests of all of the functions in
;; the hash.el library. Invoke M-x test-hash to run the test suite.
;; There should be no output, other than a progress report of test
;; names, written to a temporary buffer, *test-hash*.
;; A profiled version is available with M-x test-hash-with-profile;
;; the profile is written to a temporary buffer, *profile*.
;;
;; For batch mode use, usually invoked from a "make check" run, the
;; functions test-hash-and-kill-emacs and
;; test-hash-with-profile-and-kill-emacs are provided.  They save
;; the test results in the files test-hash.results and, if profiling
;; was used, test-hash.profile, then kill emacs with an exit code
;; (capped at 255) that counts the number of errors detected.

;;; Code:

(require 'hash)
(require 'profile-support)	; for M-x test-hash-with-profile*


(defconst test-hash-version "1.00"
  "Version number of test-hash library.")


(defconst test-hash-date "[05-Mar-1999]"
  "Revision date of test-hash library.")


(defvar test-hash-error-count 0
  "Count of errors detected by test-hash.")


(defconst test-hash-buffer "*test-hash*"
  "Name of the temporary buffer in which test results are recorded.")


(defvar test-hash-test-count 0
  "Global counter for testing hash-apply and hash-apply-sorted.")


(defun string-less-or-equalp (s1 s2)
  "Return t if S1 is lexicographically less than or equal to S2, and nil
otherwise."
  (or (string-equal s1 s2)
      (string-lessp s1 s2)))


(defun string-greaterp (s1 s2)
  "Return t if S1 is lexicographically greater than S2, and nil
otherwise."
  (not (string-less-or-equalp s1 s2)))


(defun test-hash-assert-message (message)
  (setq test-hash-error-count (1+ test-hash-error-count))
  (princ (format "error: %s\n" message)))


(defun test-hash-assert (test message)
  (if (not test)
      (test-hash-assert-message  message)))


(defun test-hash-create-delete-table ()
  (test-hash-report "hash-create-delete-table")
  (let ((sizes '(0 1 2 3 10 100 1000)))
    (while (and (car sizes)
		(integerp (car sizes)))
      (hash-delete-table (hash-create-table nil (car sizes)))
      (setq sizes (cdr sizes)))))


(defun test-hash-entry-p ()
  (test-hash-report "hash-entry-p")
  (let ((sizes '(0 1 2 3 10 100 1000)))
    (while (and (car sizes)
		(integerp (car sizes)))
      (let ((hashtable (hash-create-table nil (car sizes))))
	(test-hash-assert (not (hash-entry-p "foo" hashtable))
			  "(not (hash-entry-p \"foo\" hashtable))")
	(hash-put-entry "foo" "value-of-foo" hashtable)
	(test-hash-assert (hash-entry-p "foo" hashtable)
			  "(hash-entry-p \"foo\" hashtable)")
	(setq sizes (cdr sizes))))))


(defun test-hash-delete-entry ()
  (test-hash-report "hash-delete-entry")
  (let ((sizes '(0 1 2 3 10 100 1000)))
    (while (and (car sizes)
		(integerp (car sizes)))
      (let ((hashtable (hash-create-table nil (car sizes))))
	(test-hash-assert (not (hash-entry-p "foo" hashtable))
			  "(not (hash-entry-p \"foo\" hashtable))")
	(hash-put-entry "foo" "value-of-foo" hashtable)
	(test-hash-assert (hash-entry-p "foo" hashtable)
			  "(hash-entry-p \"foo\" hashtable)")
	(hash-delete-entry "foo" hashtable)
	(test-hash-assert (not (hash-entry-p "foo" hashtable))
			  "(not (hash-entry-p \"foo\" hashtable))")
	(setq sizes (cdr sizes))))))


(defun test-hash-prepare-buffer (buffer-name)
  "Check whether a buffer named BUFFER-NAME already exists, and if so,
rename it uniquely, so that BUFFER-NAME can be used as the name of a
new buffer." 
  (if (get-buffer buffer-name)
      (save-excursion
	(set-buffer buffer-name)
	(rename-uniquely))))


(defun test-hash-random-fill (count hashtable)
  "Fill HASHTABLE with COUNT (key, value) pairs, where each key is
identical to its corresponding value, except that it has been filtered
by prin1-to-string, and the keys are randomly chosen.  Return nil."
  (while (> count 0)
    (let ((value (random)))
      (hash-put-entry (prin1-to-string value) value hashtable))
    (setq count (1- count)))
  nil)


(defun test-hash-random-fill-with-list (count hashtable)
  "Fill HASHTABLE with COUNT (key, value) pairs, where each key is
identical to its corresponding value, except that it has been filtered
by prin1-to-string, and the keys are randomly chosen.

Return a list of these generated pairs, for later independent testing."
  (let ((pairlist nil))
    (while (> count 0)
      (let* ((value (random))
	     (key (prin1-to-string value)))
	(hash-put-entry key value hashtable)
	(setq pairlist (append pairlist (list (list key value)))))
      (setq count (1- count)))
    pairlist))


(defun test-hash-apply ()
  (test-hash-report "hash-apply")
  (let ((sizes '(1 2 5 10 100 1000)))
    (while (and (car sizes)
		(integerp (car sizes)))
      (let ((hashtable (hash-create-table nil (car sizes))))
	(test-hash-random-fill (car sizes) hashtable)
	(setq test-hash-test-count 0)
	(hash-apply
	 (function test-hash-increment-test-count)
	 hashtable)
	(test-hash-assert (= test-hash-test-count (car sizes))
			  "(= test-hash-test-count (car sizes))"))
      (setq sizes (cdr sizes)))))


(defun test-hash-apply-sorted ()
  (test-hash-report "hash-apply-sorted")
  (let ((sizes '(1 2 5 10 100 1000)))
    (while (and (car sizes)
		(integerp (car sizes)))
      (let ((hashtable (hash-create-table nil (car sizes))))
	(test-hash-random-fill (car sizes) hashtable)

	(setq test-hash-test-count 0)
	(hash-apply-sorted
	 (function test-hash-increment-test-count)
	 nil
	 hashtable)
	(test-hash-assert (= test-hash-test-count (car sizes))
			  "(= test-hash-test-count (car sizes))")

	(setq test-hash-test-count 0)
	(hash-apply-sorted
	 (function test-hash-increment-test-count)
	 (function string-greaterp)
	 hashtable)
	(test-hash-assert (= test-hash-test-count (car sizes))
			  "(= test-hash-test-count (car sizes))"))
      (setq sizes (cdr sizes)))))


(defun test-hash-get-key-list ()
  (test-hash-report "hash-get-key-list")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(fill-count 100)
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil 2000))
      (test-hash-random-fill fill-count  hashtable)
      (test-hash-assert (= fill-count (length (hash-get-key-list hashtable)))
			"(= fill-count (length (hash-get-key-list hashtable)))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-key-list-sorted ()
  (test-hash-report "hash-get-key-list-sorted")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(fill-count 100)
	(hashtable nil)
	(key-list nil)
	(key-value-list nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (setq key-value-list (test-hash-random-fill-with-list fill-count hashtable))
      (setq key-list (mapcar (function (lambda (x) (car x))) key-value-list))
      (test-hash-assert (equal (sort (copy-sequence key-list) (function string-lessp)) (hash-get-key-list-sorted nil hashtable))
			"(equal (sort (copy-sequence key-list) (function string-lessp)) (hash-get-key-list-sorted nil hashtable))")
      (test-hash-assert (equal (sort (copy-sequence key-list) (function string-greaterp)) (hash-get-key-list-sorted (function string-greaterp) hashtable))
			"(equal (sort (copy-sequence key-list) (function string-greaterp)) (hash-get-key-list-sorted (function string-greaterp) hashtable))")
      (setq hashtable (hash-delete-table hashtable))
      (setq sizes (cdr sizes)))))


(defun test-hash-get-key-value-list ()
  (test-hash-report "hash-get-key-value-list")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(fill-count 100)
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil 2000))
      (test-hash-random-fill fill-count hashtable)
      (test-hash-assert (= fill-count (length (hash-get-key-value-list hashtable)))
			"(= fill-count (length (hash-get-key-value-list hashtable)))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-key-value-list-sorted ()
  (test-hash-report "hash-get-key-value-list-sorted")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(fill-count 100)
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (test-hash-random-fill fill-count hashtable)
      (test-hash-assert (= fill-count (length (hash-get-key-value-list-sorted nil hashtable)))
			"(= fill-count (length (hash-get-key-value-list-sorted nil hashtable)))")
      (test-hash-assert (= fill-count (length (hash-get-key-value-list-sorted (function string-greaterp) hashtable)))
			"(= fill-count (length (hash-get-key-value-list-sorted (function string-greaterp) hashtable)))")
      (setq hashtable (hash-delete-table hashtable))
      (setq sizes (cdr sizes)))))


(defun test-hash-table-p ()
  (test-hash-report "hash-table-p")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (test-hash-assert (hash-table-p hashtable)
			"(hash-table-p hashtable)")
      (test-hash-assert (not (hash-table-p nil))
			"(not (hash-table-p hashtable))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-case-insensitive ()
  (test-hash-report "hash-get-case-insensitive")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (test-hash-assert (null (hash-get-case-insensitive hashtable))
			"(null (hash-get-case-insensitive hashtable))")
      (hash-delete-table hashtable)
      (setq hashtable (hash-create-table t (car sizes)))
      (test-hash-assert (hash-get-case-insensitive hashtable)
			"(hash-get-case-insensitive hashtable)")
      (hash-put-entry "HaShKeY" "hashvalue" hashtable)
      (test-hash-assert (string-equal (hash-get-entry "hashkey" hashtable) "hashvalue")
			"(string-equal (hash-get-entry \"hashkey\" hashtable) \"hashvalue\")")
      (test-hash-assert (string-equal (hash-get-entry "HASHKEY" hashtable) "hashvalue")
			"(string-equal (hash-get-entry \"HASHKEY\" hashtable) \"hashvalue\")")
      (test-hash-assert (string-equal (hash-get-entry "HaShKeY" hashtable) "hashvalue")
			"(string-equal (hash-get-entry \"HaShKeY\" hashtable) \"hashvalue\")")
      (hash-delete-table hashtable)
      (setq sizes (cdr sizes)))))


(defun test-hash-get-cursize ()
  (test-hash-report "hash-get-cursize")
  (let ((sizes '(1 2 3 10 100 1000))
	(fill-count 100)
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (test-hash-random-fill fill-count hashtable)
      (test-hash-assert (= (hash-get-cursize hashtable) fill-count)
			"(= (hash-get-cursize hashtable) fill-count)")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-maxsize ()
  (test-hash-report "hash-get-maxsize")
  (let ((sizes '(1 2 3 10 100 1000))
	(hashtable nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (test-hash-assert (= (hash-get-maxsize hashtable) (this-or-next-prime (car sizes)))
			"(= (hash-get-maxsize hashtable) (this-or-next-prime (car sizes)))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-rehash-size ()
  (test-hash-report "hash-get-rehash-size")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(hashtable nil)
	(rehash-size 8.125))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes) rehash-size))
      (test-hash-assert (= (hash-get-rehash-size hashtable) rehash-size)
			"(= (hash-get-rehash-size rehash-size))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-rehash-threshold ()
  (test-hash-report "hash-get-rehash-threshold")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(hashtable nil)
	(rehash-threshold 0.875))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes) nil rehash-threshold))
      (test-hash-assert (= (hash-get-rehash-threshold hashtable) rehash-threshold)
			"(= (hash-get-rehash-threshold rehash-threshold))")
      (setq sizes (cdr sizes)))))


(defun test-hash-get-put-entry ()
  (test-hash-report "hash-get-put-entry")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(get-value nil)
	(hashtable nil)
	(k 0)
	(nputs 10)
	(put-key nil)
	(put-value nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (setq k 0)
      (while (< k nputs)
	(setq put-value (random))
	(setq put-key (prin1-to-string put-value))
	(hash-put-entry put-key put-value hashtable)
	(setq get-value (hash-get-entry put-key hashtable))
	(test-hash-assert (equal put-value get-value)
			  "(equal put-value get-value)")
	(setq k (1+ k)))
      (setq sizes (cdr sizes)))))


(defun test-hash-get-put-delete-entry ()
  "Test hash table get/put/delete operations by carrying out a
large number of these operations, randomly choosing which to do, with
a slight bias in favor of put over delete, so that the table gradually
fills.  The hash library's internal count of table elements should match
that computed here, and every element stored must also be retrievable,
unless it has been deleted."
  (test-hash-report "hash-get-put-delete-entry")
  (let ((sizes '(0 1 2 3 10 100 1000))
	(element-count nil)
	(get-value nil)
	(hashtable nil)
	(k 0)
	(nputs 1000)	; 10000 makes this test run too long, but if you change it,
			; adjust the percentage cutoff in the (cond ...) below
	(percentage nil)
	(put-key nil)
	(put-value nil))
    (while (and (car sizes)
		(integerp (car sizes)))
      (setq hashtable (hash-create-table nil (car sizes)))
      (setq k 0)
      (setq element-count 0)
      (while (< k nputs)
	(setq percentage (random 101)) ; generate a number in 0..100, inclusive
	(setq put-value (random))
	(setq put-key (prin1-to-string put-value))

	;; With a 40% excess of puts over deletes, we will generate about
	;; 0.4*nputs (= 400) elements in the table on completion, which
	;; is more than full, forcing growth, for all but the last
	;; values of sizes.
	(cond
	 ((<= percentage 70)		; 70% of the time, do a put, get, and validate

	  (setq element-count (1+ element-count))
	  (hash-put-entry put-key put-value hashtable)

	  (setq get-value (hash-get-entry put-key hashtable))
	  (test-hash-assert (equal put-value get-value)
			    "(equal put-value get-value)"))

	 (t				; 30% of the time, do a put, delete, get, and validate

	  (setq element-count (1+ element-count))
	  (hash-put-entry put-key put-value hashtable)

	  (setq element-count (1- element-count))
	  (hash-delete-entry put-key hashtable)

	  (setq get-value (hash-get-entry put-key hashtable))
	  (test-hash-assert (equal nil get-value)
			    "(equal nil get-value)")))

	;; The library's internal count should match ours
	(test-hash-assert (equal element-count (hash-get-cursize hashtable))
			  "(equal element-count (hash-get-cursize hashtable))")
	(setq k (1+ k)))
      (setq sizes (cdr sizes)))))


(defun test-hash-increment-test-count (key value arg)
  (setq test-hash-test-count (1+ test-hash-test-count)))


(defun test-hash-report (msg)
  "Display a message in both the echo area, and standard output, the
latter with a final newline."
  (message "testing %s ..." msg)
  (princ (format "test of %s ...\n" msg)))


(defun test-hash ()
  "Test all of the functions in hash.el.  There should be no output,
other than a progress report of test names, written to a temporary
buffer, *test-hash*.  That buffer name can be changed by assigning a
new string to the variable test-hash-buffer.  An existing buffer
of that name is first renamed to a unique name."
  (interactive)
  (test-hash-prepare-buffer test-hash-buffer)
  (with-output-to-temp-buffer test-hash-buffer
    (setq test-hash-error-count 0)
    (message "This may take a while: buffer %s contains report" test-hash-buffer)
    (sit-for 2)
    (princ "There should be no output here other than the test names\n\n")
    (test-hash-create-delete-table)
    (test-hash-entry-p)
    (test-hash-delete-entry)
    (test-hash-apply)
    (test-hash-apply-sorted)
    (test-hash-get-key-list)
    (test-hash-get-key-list-sorted)
    (test-hash-get-key-value-list)
    (test-hash-get-key-value-list-sorted)
    (test-hash-table-p)
    (test-hash-get-case-insensitive)
    (test-hash-get-cursize)
    (test-hash-get-maxsize)
    (test-hash-get-rehash-size)
    (test-hash-get-rehash-threshold)
    (test-hash-get-put-entry)
    (test-hash-get-put-delete-entry)))


(defun test-hash-with-profile ()
  "Run test-hash with profiling, writing the profile to a temporary
buffer, *profile*.  That buffer name can be changed by assigning a new
string to the variable profile-buffer.  An existing buffer of that
name is first renamed to a unique name."
  (interactive)
  (setq profile-functions-list
	'(
	    hash-apply
	    hash-apply-sorted
	    hash-create-table
	    hash-delete-entry
	    hash-delete-table
	    hash-entry-p
	    hash-get-case-insensitive
	    hash-get-cursize
	    hash-get-entry
	    hash-get-key-list
	    hash-get-key-list-sorted
	    hash-get-key-value-list
	    hash-get-key-value-list-sorted
	    hash-get-maxsize
	    hash-get-rehash-size
	    hash-get-rehash-threshold
	    hash-put-entry
	    hash-table-p

	    next-prime
	    nth-prime
	    prev-prime
	    prime-p

	    ;; We include these three internal functions in the profile
	    ;; because (1) they are not inlined, and (2) non-zero
	    ;; profile data for them verifies that they have been
	    ;; called.
	    hash-internal-copy-obarray
	    hash-internal-grow-hashtable
	    hash-internal-maybe-grow-hashtable

	    test-hash-apply
	    test-hash-apply-sorted
	    test-hash-create-delete-table
	    test-hash-delete-entry
	    test-hash-entry-p
	    test-hash-get-case-insensitive
	    test-hash-get-cursize
	    test-hash-get-key-list
	    test-hash-get-key-list-sorted
	    test-hash-get-key-value-list
	    test-hash-get-key-value-list-sorted
	    test-hash-get-maxsize
	    test-hash-get-put-entry
	    test-hash-get-put-delete-entry
	    test-hash-get-rehash-size
	    test-hash-get-rehash-threshold
	    test-hash-table-p
	    ))
  (profile-finish)
  (profile-functions)
  (test-hash)
  (test-hash-prepare-buffer profile-buffer)
  (profile-results)
  (profile-sort)
  (profile-finish)
  (save-excursion
    (set-buffer profile-buffer)
    (goto-char (point-min))
    (insert (profile-system-id) "\f\n")))


;; The remaining functions are intended for use in batch mode only,
;; usually in a "make check" run.

(defun test-hash-internal-kill-emacs ()
  "Kill emacs with a process exit code equal to the number of test
failures (but capped at 255 because of the UNIX exit code size
limit)."
  (kill-emacs (cond
	       ((< test-hash-error-count 256) test-hash-error-count)
	       (t 255))))


(defun test-hash-and-kill-emacs ()
  "Invoke test-hash, then kill emacs with a process exit code equal to
the number of test failures (but capped at 255 because of the UNIX exit
code size limit).

This function is normally invoked only in batch mode from a
\"make check\" run."
  (test-hash)
  (set-buffer test-hash-buffer)
  (write-file "test-hash.results")
  (test-hash-internal-kill-emacs))


(defun test-hash-with-profile-and-kill-emacs ()
  "Invoke test-hash-with-profile, save the profile in a file
test-hash.profile, then kill emacs with a process exit code equal to the
number of test failures (but capped at 255 because of the UNIX exit code
size limit).

This function is normally invoked only in batch mode from a
\"make check\" run."
  (test-hash-with-profile)
  (set-buffer test-hash-buffer)
  (write-file "test-hash.results")
  (profile-write "test-hash.profile")
  (test-hash-internal-kill-emacs))


;;; test-hash.el ends here
