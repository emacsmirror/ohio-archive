;;; test-primes.el --- tests of Emacs Lisp library code in primes.el

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
;; Keywords: prime numbers, primality testing

;; This file is part of GNU Emacs.

;;; Commentary:
;; This file carries out validation tests of all of the functions in
;; the primes.el library. Invoke M-x test-primes to run the test
;; suite.  There should be no output, other than a progress report of
;; test names, written to a temporary buffer, *test-primes*.
;; A profiled version is available with M-x test-primes-with-profile;
;; the profile is written to a temporary buffer, *profile*.
;;
;; For batch mode use, usually invoked from a "make check" run, the
;; functions test-primes-and-kill-emacs and
;; test-primes-with-profile-and-kill-emacs are provided.  They save the
;; test results in the file
;;
;;	test-primes.results.HOSTNAME.YYYY-MM-DD-hh-mm-ss
;;
;; and, if profiling was used, in
;;
;;	test-primes.profile.HOSTNAME.YYYY-MM-DD-hh-mm-ss
;;
;; then kill emacs with an exit code (capped at 255) that counts the
;; number of errors detected.
;;
;; The following constants and functions from this file should be
;; directly usable in similar test-PACKAGENAME.el files: feel free to
;; make copies of them, with the test-primes prefix suitably changed:
;;
;;	(defconst test-primes-date "[05-Mar-1999]" ...)
;;	(defconst test-primes-version "1.00" ...)
;;
;;	(defvar test-primes-error-count 0 ...)
;;
;;	(defun test-primes () ...)
;;	(defun test-primes-and-kill-emacs () ...)
;;	(defun test-primes-assert (test message) ...)
;;	(defun test-primes-assert-message (message) ...)
;;	(defun test-primes-internal-kill-emacs () ...)
;;	(defun test-primes-prepare-buffer (buffer-name) ...)
;;	(defun test-primes-report (msg) ...)
;;	(defun test-primes-system-stamp () ...)
;;	(defun test-primes-with-profile () ...)
;;	(defun test-primes-with-profile-and-kill-emacs () ...)
;;
;;; Code:

(require 'primes)
(require 'profile-support)	; for M-x test-primes-with-profile*


(defconst test-primes-version "1.00"
  "Version number of test-primes library.")


(defconst test-primes-date "[05-Mar-1999]"
  "Revision date of test-primes library.")


(defvar test-primes-error-count 0
  "Count of errors detected by test-primes.")


(defconst test-primes-primes-list-1
  '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89
    97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179
    181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269
    271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367
    373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461
    463 467 479 487 491 499)
  "List of primes up to 500.")


(defconst test-primes-primes-list-2
  '(134217529 134217541 134217593 134217613 134217617 134217649 134217689)
  "List of primes between 2^{27}-200 and 2^{27}; these should be near
integer limits for 32-bit Emacs.")


(defconst test-primes-nonprimes-list-1
  '(4 6 8 9 10 12 14 15 16 18 20 21 22 24 25 26 27 28 30 32 33 34 35
    36 38 39 40 42 44 45 46 48 49 50 51 52 54 55 56 57 58 60 62 63 64
    65 66 68 69 70 72 74 75 76 77 78 80 81 82 84 85 86 87 88 90 91 92
    93 94 95 96 98 99 100 102 104 105 106 108 110 111 112 114 115 116
    117 118 119 120 121 122 123 124 125 126 128 129 130 132 133 134
    135 136 138 140 141 142 143 144 145 146 147 148 150 152 153 154
    155 156 158 159 160 161 162 164 165 166 168 169 170 171 172 174
    175 176 177 178 180 182 183 184 185 186 187 188 189 190 192 194
    195 196 198 200 201 202 203 204 205 206 207 208 209 210 212 213
    214 215 216 217 218 219 220 221 222 224 225 226 228 230 231 232
    234 235 236 237 238 240 242 243 244 245 246 247 248 249 250 252
    253 254 255 256 258 259 260 261 262 264 265 266 267 268 270 272
    273 274 275 276 278 279 280 282 284 285 286 287 288 289 290 291
    292 294 295 296 297 298 299 300 301 302 303 304 305 306 308 309
    310 312 314 315 316 318 319 320 321 322 323 324 325 326 327 328
    329 330 332 333 334 335 336 338 339 340 341 342 343 344 345 346
    348 350 351 352 354 355 356 357 358 360 361 362 363 364 365 366
    368 369 370 371 372 374 375 376 377 378 380 381 382 384 385 386
    387 388 390 391 392 393 394 395 396 398 399 400 402 403 404 405
    406 407 408 410 411 412 413 414 415 416 417 418 420 422 423 424
    425 426 427 428 429 430 432 434 435 436 437 438 440 441 442 444
    445 446 447 448 450 451 452 453 454 455 456 458 459 460 462 464
    465 466 468 469 470 471 472 473 474 475 476 477 478 480 481 482
    483 484 485 486 488 489 490 492 493 494 495 496 497 498 500)
  "List of nonprimes up to 500.")


(defconst test-primes-nonprimes-list-2
  '(134217629 134217630 134217631 134217632 134217633 134217634
    134217635 134217636 134217637 134217638 134217639 134217640
    134217641 134217642 134217643 134217644 134217645 134217646
    134217647 134217648 134217650 134217651 134217652 134217653
    134217654 134217655 134217656 134217657 134217658 134217659
    134217660 134217661 134217662 134217663 134217664 134217665
    134217666 134217667 134217668 134217669 134217670 134217671
    134217672 134217673 134217674 134217675 134217676 134217677
    134217678 134217679 134217680 134217681 134217682 134217683
    134217684 134217685 134217686 134217687 134217688 134217690
    134217691 134217692 134217693 134217694 134217695 134217696
    134217697 134217698 134217699 134217700 134217701 134217702
    134217703 134217704 134217705 134217706 134217707 134217708
    134217709 134217710 134217711 134217712 134217713 134217714
    134217715 134217716 134217717 134217718 134217719 134217720
    134217721 134217722 134217723 134217724 134217725 134217726
    134217727)
  "List of nonprimes between 2^{27}-100 and 2^{27}; these should be
near integer limits for 32-bit Emacs.")


(defconst test-primes-buffer "*test-primes*"
  "Name of the temporary buffer in which test results are recorded.")


(defun test-primes-assert-message (message)
  (setq test-primes-error-count (1+ test-primes-error-count))
  (princ (format "error: %s\n" message)))


(defun test-primes-assert-is-prime (n)
  (if (not (prime-p n))
      (test-primes-assert-message (format "(prime-p %d)\n" n))))


(defun test-primes-assert-is-nonprime (n)
  (if (prime-p n)
      (test-primes-assert-message (format "(not (prime-p %d))\n" n))))


(defun test-primes-assert (test message)
  (if (not test)
      (test-primes-assert-message  message)))


(defun test-primes-gcd-1 ()
  "Test (gcd m n) for all pairs (m,n) with values in 0..50."
  (let ((m 1)
	(n nil)
	(result-1 nil)
	(result-2 nil)
	(limit 50))			;original 100 made test too long
    (while (<= m limit)
      (setq n 1)
      (while (<= n m)	; loop over lower triangle of (m,n)
	;; OPTIMIZATION: to save an expensive (format ...) call in this
	;; double loop, we precompute the test results, and only call
	;; test-primes-assert if the test fails.
	(setq result-1 (gcd m n))
	(setq result-2 (gcd n m))

	;; Check for argument symmetry
	(if (not (= result-1 result-2))
	    (test-primes-assert (= result-1 result-2)
				(format "(= (gcd %d %d) (gcd %d %d))" m n n m)))

	;; Check that the result really is a common divisor of both m and n
	(if (not (= (% m result-1) 0))
	    (test-primes-assert (= (% m result-1) 0)
				(format "(= (%% %d %s) 0)" m result-1)))

	(if (not (= (% n result-1) 0))
	    (test-primes-assert (= (% n result-1) 0)
				(format "(= (%% %d %s) 0)" n result-1)))

	;; I would like to have a test here that the result is the
	;; GREATEST common divisor, but I cannot think of a fast way
	;; just now.  A third nested loop running from (gcd n m) up to n
	;; could check for no further divisors, but that would give this
	;; function O(N^3) running time, which is too expensive.
	;;
	;; I think the algorithm on p. 104 of
	;;
	;;	Ronald L. Graham, Donald E. Knuth and Oren Patashnik, Concrete
	;;	Mathematics, Addison-Wesley, Reading, MA, USA, 1989, ISBN
	;;	0-201-14236-8.
	;;
	;; is what should be used here.

	(setq n (1+ n)))
      (setq m (1+ m)))))


(defun test-primes-gcd-2 ()
  "Test (gcd m n) for all pairs (0,n) with n in 0..100."
  (let ((n 0)
	(result nil)
	(limit 100))
    (while (<= n limit)
      (setq result (gcd 0 n))
      (test-primes-assert (= n result)
			  (format "(= %d %s)" n result))
      (setq n (1+ n)))))


(defun test-primes-gcd-3 ()
  "Test (gcd m n) for all pairs (m,0) with m in 0..100."
  (let ((m 0)
	(result nil)
	(limit 100))
    (while (<= m limit)
      (setq result (gcd m 0))
      (test-primes-assert (= m result)
			  (format "(= %d %s)" m result))
      (setq m (1+ m)))))


(defun test-primes-gcd-4 ()
  "Test (gcd m n) for values that should cause a nil return."
  (let ((k)
	(the-input-list '(
			  'symbol-name
			  "string-value"
			  "12.0"
			  (function (lambda ()))
			  t
			  nil
			  '()
			  '(12)
			  )))
    (while (car the-input-list)
      (setq k (car the-input-list))
      (test-primes-assert (null (gcd k k))
			  (format "(null (gcd %s %s)" k k))
      (setq the-input-list (cdr the-input-list)))))


(defun test-primes-gcd ()
  "Test (gcd m n) for all pairs (m,n) with values in 0..50, and for
special arguments that should cause a nil return."
  (test-primes-report "gcd")
  (test-primes-gcd-1)
  (test-primes-gcd-2)
  (test-primes-gcd-3)
  (test-primes-gcd-4))


(defun test-primes-lcm-1 ()
  "Test (lcm m n) for all pairs (m,n) with values in 1..50."
  (let ((m 1)
	(n nil)
	(result-1 nil)
	(result-2 nil)
	(limit 50))			;original 100 made test too long
    (while (<= m limit)
      (setq n 1)
      (while (<= n m)	; loop over lower triangle of (m,n)
	;; OPTIMIZATION: to save an expensive (format ...) call in this
	;; double loop, we precompute the test results, and only call
	;; test-primes-assert if the test fails.
	(setq result-1 (lcm m n))
	(setq result-2 (lcm n m))

	;; Check for argument symmetry
	(if (not (= result-1 result-2))
	    (test-primes-assert (= result-1 result-2)
				(format "(= (lcm %d %d) (lcm %d %d))" m n n m)))

	;; Check that the result really is a common multiple of both m and n
	(if (not (= (% result-1 m) 0))
	    (test-primes-assert (= (% result-1 m) 0)
				(format "(= (%% %s %d) 0)" result-1 m)))

	(if (not (= (% result-1 n) 0))
	    (test-primes-assert (= (% result-1 n) 0)
				(format "(= (%% %s %d) 0)" result-1 n)))

	;; I would like to have a test here that the result is the
	;; LEAST common multiple, but I cannot think of a fast way
	;; just now.

	(setq n (1+ n)))
      (setq m (1+ m)))))


(defun test-primes-lcm-2 ()
  "Test (lcm m n) for all pairs (0,n) with n in 0..100."
  (let ((n 0)
	(result nil)
	(limit 100))
    (while (<= n limit)
      (setq result (lcm 0 n))
      (test-primes-assert (= 0 result)
			  (format "(= %d %s)" 0 result))
      (setq n (1+ n)))))


(defun test-primes-lcm-3 ()
  "Test (lcm m n) for all pairs (m,0) with m in 0..100."
  (let ((m 0)
	(result nil)
	(limit 100))
    (while (<= m limit)
      (setq result (lcm m 0))
      (test-primes-assert (= 0 result)
			  (format "(= %d %s)" 0 result))
      (setq m (1+ m)))))


(defun test-primes-lcm-4 ()
  "Test (lcm m n) for values that should cause a nil return."
  (let ((the-input-list '("string-value" "12.0" nil '() '(12) 134217727))
	(k))
    (while (car the-input-list)
      (setq k (car the-input-list))
      (test-primes-assert (null (lcm k k))
			  (format "(null (lcm %s %s)" k k))
      (setq the-input-list (cdr the-input-list)))))


(defun test-primes-lcm ()
  "Test (lcm m n) for all pairs (m,n) with values in 0..50, and for
special arguments that should cause a nil return."
  (test-primes-report "lcm")
  (test-primes-lcm-1)
  (test-primes-lcm-2)
  (test-primes-lcm-3)
  (test-primes-lcm-4))


(defun test-primes-prepare-buffer (buffer-name)
  "Check whether a buffer named BUFFER-NAME already exists, and if so,
rename it uniquely, so that BUFFER-NAME can be used as the name of a
new buffer."
  (if (get-buffer buffer-name)
      (save-excursion
	(set-buffer buffer-name)
	(rename-uniquely))))


(defun test-primes-prime-p ()
  (test-primes-report "prime-p")
  (mapcar (function test-primes-assert-is-prime) test-primes-primes-list-1)
  (mapcar (function test-primes-assert-is-prime) test-primes-primes-list-2)
  (mapcar (function test-primes-assert-is-nonprime) test-primes-nonprimes-list-1)
  (mapcar (function test-primes-assert-is-nonprime) test-primes-nonprimes-list-2))


(defun test-primes-report (msg)
  "Display a message in both the echo area, and standard output, the
latter with a final newline."
  (message "testing %s ..." msg)
  (princ (format "test of %s ...\n" msg)))


(defun test-primes-next-prime-from-list-should-be (the-list)
  "Given a list of consecutive primes in THE-LIST, check that next-prime
works correctly for all but the last of them."
  (while (cddr the-list)
    (test-primes-next-prime-should-be (car the-list) (cadr the-list))
    (setq the-list (cdr the-list))))


(defun test-primes-next-prime-should-be (this next)
  (if (not (= (next-prime this) next))
      (test-primes-assert-message
       (format "(next-prime %d) should be %d\n" this next))))


(defun test-primes-next-prime ()
  (test-primes-report "next-prime")
  (test-primes-next-prime-should-be -100 2)
  (test-primes-next-prime-should-be 0 2)
  (test-primes-next-prime-should-be 3 5)
  (test-primes-next-prime-should-be 491 499)
  (test-primes-next-prime-should-be 134217649 134217689)
  (test-primes-next-prime-from-list-should-be test-primes-primes-list-1)
  (test-primes-next-prime-from-list-should-be test-primes-primes-list-2))


(defun test-primes-nth-prime ()
  (test-primes-report "nth-prime")
  (test-primes-assert (null (nth-prime 0)) "(null (nth-prime 0))")
  (test-primes-assert (= (nth-prime 1) 2) "(= (nth-prime 1) 2)")
  (test-primes-assert (= (nth-prime 10) 29) "(= (nth-prime 10) 29)")
  (test-primes-assert (= (nth-prime 100) 541) "(= (nth-prime 10) 29)")
  (test-primes-assert (= (nth-prime 1000) 7919) "(= (nth-prime 1000) 7919)")
  ;; Suppress this last test: its run time is too long...
  ;; (test-primes-assert (= (nth-prime 100000) 104729) "(= (nth-prime 100000) 104729)")
)


(defun test-primes-prev-prime-from-list-should-be (the-list)
  "Given a list of consecutive primes in THE-LIST, check that prev-prime
works correctly for all but the first of them."
  (while (cadr the-list)
    (test-primes-prev-prime-should-be (cadr the-list) (car the-list))
    (setq the-list (cdr the-list))))


(defun test-primes-prev-prime-should-be (this prev)
  (if (not (= (prev-prime this) prev))
      (test-primes-assert-message (format "(prev-prime %d) should be %d\n"))))


(defun test-primes-prev-prime ()
  (test-primes-report "prev-prime")
  (test-primes-assert (null (prev-prime 2)) "(null (prev-prime 2))")
  (test-primes-prev-prime-should-be 3 2)
  (test-primes-prev-prime-should-be 5 3)
  (test-primes-prev-prime-should-be 541 523)
  (test-primes-prev-prime-should-be 499 491)
  (test-primes-prev-prime-should-be 134217689 134217649)
  (test-primes-prev-prime-from-list-should-be test-primes-primes-list-1)
  (test-primes-prev-prime-from-list-should-be test-primes-primes-list-2))


(defun test-primes-prime-factors-1 ()
  "Factor all integers from 1 to 2000, and compare the product of the
factors with the original integer."
  (let ((k 1)
	(limit 200)
	(result nil)
	(the-list nil))
    (while (<= k limit)
      (setq the-list (prime-factors k))
      ;; OPTIMIZATION: to save an expensive (format ...) call in
      ;; this loop, we precompute the test result, and only call
      ;; test-primes-assert if the test fails.
      (setq result (= k (eval (cons '* the-list))))
      (if result
	  (test-primes-assert result
			      (format "(= %d (eval (cons '* %s)))" k the-list)))
      (setq k (1+ k)))))


(defun test-primes-prime-factors-2 ()
  "Factor all integers 2^p, 2^p-1, 2^p+1 for p in 1..18, and compare the
product of the factors with the original integer.  Ideally, we should
test values up to 2^{27}-1 (the range of Emacs Lisp integers in 32-bit
systems), but that takes too long with the current implementation of
\(factor ...): its run time is worst-case O(N/2)."
  (let ((limit 18)
	(k nil)
	(p 1)
	(the-input-list nil))
    (while (<= p limit)
      (setq the-input-list (list (1- (expt 2 p)) (expt 2 p) (1+ (expt 2 p))))
      (while (car the-input-list)
	(setq k (car the-input-list))
	(test-primes-assert (= k (eval (cons '* (prime-factors k))))
			    (format "(= %d (eval (cons '* %s)))" k (prime-factors k)))
	(setq the-input-list (cdr the-input-list))
	(setq p (1+ p))))))


(defun test-primes-prime-factors-3 ()
  "Check that nil is returned for invalid arguments to (prime-factors ...)."
  (let ((the-input-list '(-100 -1 0 1 "string-value" "12.0" nil '() '(12)))
	(k 0))
    (while (car the-input-list)
      (setq k (car the-input-list))
      (test-primes-assert (null (prime-factors k))
			  (format "(null (prime-factors %s)" k))
      (setq the-input-list (cdr the-input-list)))))


(defun test-primes-prime-factors ()
  (test-primes-report "prime-factors")
  (test-primes-prime-factors-1)
  (test-primes-prime-factors-2)
  (test-primes-prime-factors-3))


(defun test-primes-primes-between ()
  (test-primes-report "primes-between")
  (test-primes-assert (equal (primes-between 'alpha 'omega) '())
		      "(equal (primes-between 'alpha 'omega) '())")
  (test-primes-assert (equal (primes-between -10 0) '())
		      "(equal (primes-between -10 0) '())")
  (test-primes-assert (equal (primes-between 0 10) '(2 3 5 7))
		      "(equal (primes-between 0 10) '(2 3 5 7))")
  (test-primes-assert
   (equal (primes-between 0 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
   "(equal (primes-between 0 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))")
)


(defun test-primes-system-stamp ()
  "Return a string consisting of the system name and an ISO 8601 date
and time value YYYY-MM-DD-HH-MM-SS (ISO 8601 space and colons have
been changed to hyphens for use in portable filenames)."
  (concat (system-name) "."
	  (format-time-string "%Y-%m-%d-%H-%M-%S" (current-time))))


(defun test-primes-this-or-next-prime-should-be (this next)
  (if (not (= (this-or-next-prime this) next))
      (test-primes-assert-message
       (format "(this-or-next-prime %d) should be %d\n" this next))))


(defun test-primes-this-or-next-prime ()
  (test-primes-report "this-or-next-prime")
  (test-primes-this-or-next-prime-should-be -100 2)
  (test-primes-this-or-next-prime-should-be 0 2)
  (test-primes-this-or-next-prime-should-be 2 2)
  (test-primes-this-or-next-prime-should-be 3 3)
  (test-primes-this-or-next-prime-should-be 491 491)
  (test-primes-this-or-next-prime-should-be 134217649 134217649)

  (test-primes-this-or-next-prime-should-be 4 5)
  (test-primes-this-or-next-prime-should-be 6 7)
  (test-primes-this-or-next-prime-should-be 490 491)
  (test-primes-this-or-next-prime-should-be 134217648 134217649)
)


(defun test-primes-this-or-prev-prime-should-be (this next)
  (if (not (= (this-or-prev-prime this) next))
      (test-primes-assert-message
       (format "(this-or-prev-prime %d) should be %d\n"))))


(defun test-primes-this-or-prev-prime ()
  (test-primes-report "this-or-prev-prime")
  (test-primes-assert (null (this-or-prev-prime 1))
		      "(null (this-or-prev-prime 1))")
  (test-primes-this-or-prev-prime-should-be 2 2)
  (test-primes-this-or-prev-prime-should-be 3 3)
  (test-primes-this-or-prev-prime-should-be 5 5)
  (test-primes-this-or-prev-prime-should-be 491 491)
  (test-primes-this-or-prev-prime-should-be 134217649 134217649)

  (test-primes-this-or-prev-prime-should-be 4 3)
  (test-primes-this-or-prev-prime-should-be 6 5)
  (test-primes-this-or-prev-prime-should-be 492 491)
  (test-primes-this-or-prev-prime-should-be 134217650 134217649)
)


(defun test-primes ()
  "Test all of the functions in primes.el.  There should be no output,
other than a progress report of test names, written to a temporary
buffer, *test-primes*.  That buffer name can be changed by assigning a
new string to the variable test-primes-buffer.  An existing buffer
of that name is first renamed to a unique name."
  (interactive)
  (test-primes-prepare-buffer test-primes-buffer)
  (with-output-to-temp-buffer test-primes-buffer
    (setq test-primes-error-count 0)
    (message "This may take a while: buffer %s contains report" test-primes-buffer)
    (sit-for 2)
    (princ "There should be no output here other than the test names\n\n")
    (test-primes-gcd)
    (test-primes-lcm)
    (test-primes-prime-p)
    (test-primes-next-prime)
    (test-primes-nth-prime)
    (test-primes-prev-prime)
    (test-primes-prime-factors)
    (test-primes-primes-between)
    (test-primes-this-or-next-prime)
    (test-primes-this-or-prev-prime)))


(defun test-primes-with-profile ()
  "Run test-primes with profiling, writing the profile to a temporary
buffer, *profile*.  That buffer name can be changed by assigning a new
string to the variable profile-buffer.  An existing buffer of that
name is first renamed to a unique name."
  (interactive)
  (setq profile-functions-list
	'(
	    gcd
	    lcm
	    next-prime
	    nth-prime
	    prev-prime
	    prime-factors
	    prime-p
	    primes-between
	    this-or-next-prime
	    this-or-prev-prime

	    test-primes
	    test-primes-and-kill-emacs
	    test-primes-assert
	    test-primes-assert-is-nonprime
	    test-primes-assert-is-prime
	    test-primes-assert-message
	    test-primes-gcd
	    test-primes-gcd-1
	    test-primes-gcd-2
	    test-primes-gcd-3
	    test-primes-gcd-4
	    test-primes-internal-kill-emacs
	    test-primes-lcm
	    test-primes-lcm-1
	    test-primes-lcm-2
	    test-primes-lcm-3
	    test-primes-lcm-4
	    test-primes-next-prime
	    test-primes-next-prime-from-list-should-be
	    test-primes-next-prime-should-be
	    test-primes-nth-prime
	    test-primes-prepare-buffer
	    test-primes-prev-prime
	    test-primes-prev-prime-from-list-should-be
	    test-primes-prev-prime-should-be
	    test-primes-prime-factors
	    test-primes-prime-factors-1
	    test-primes-prime-factors-2
	    test-primes-prime-factors-3
	    test-primes-prime-p
	    test-primes-primes-between
	    test-primes-report
	    test-primes-system-stamp
	    test-primes-this-or-next-prime
	    test-primes-this-or-next-prime-should-be
	    test-primes-this-or-prev-prime
	    test-primes-this-or-prev-prime-should-be
	    test-primes-with-profile
	    test-primes-with-profile-and-kill-emacs
	    ))
  (profile-finish)
  (profile-functions)
  (test-primes)
  (test-primes-prepare-buffer profile-buffer)
  (profile-results)
  (profile-sort)
  (profile-finish)
  (save-excursion
    (set-buffer profile-buffer)
    (goto-char (point-min))
    (insert (profile-system-id) "\f\n")))


;; The remaining functions are intended for use in batch mode only,
;; usually in a "make check" run.

(defun test-primes-internal-kill-emacs ()
  "Kill emacs with a process exit code equal to the number of test
failures (but capped at 255 because of the UNIX exit code size
limit)."
  (kill-emacs (cond
	       ((< test-primes-error-count 256) test-primes-error-count)
	       (t 255))))


(defun test-primes-and-kill-emacs ()
  "Invoke test-primes, then kill emacs with a process exit code equal to
the number of test failures (but capped at 255 because of the UNIX exit
code size limit).

This function is normally invoked only in batch mode from a
\"make check\" run."
  (test-primes)
  (set-buffer test-primes-buffer)
  (write-file (concat "test-primes.results" "." (test-primes-system-stamp)))
  (test-primes-internal-kill-emacs))


(defun test-primes-with-profile-and-kill-emacs ()
  "Invoke test-primes-with-profile, save the profile in a file
test-primes.profile, then kill emacs with a process exit code equal to the
number of test failures (but capped at 255 because of the UNIX exit code
size limit).

This function is normally invoked only in batch mode from a
\"make check\" run."
  (test-primes-with-profile)
  (let ((stamp (test-primes-system-stamp)))
    (set-buffer test-primes-buffer)
    (write-file (concat "test-primes.results" "." stamp))
    (profile-write (concat "test-primes.profile" "." stamp))
    (test-primes-internal-kill-emacs)))


;;; test-primes.el ends here
