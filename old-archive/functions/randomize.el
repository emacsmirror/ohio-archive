; Path: hal.com!decwrl!mips!pacbell.com!iggy.GW.Vitalink.COM!widener!dsinc!bagate!cbmvax!devon!dastari!dastari!hermit
; From: hermit@dastari.uucp (Mark Buda)
; Newsgroups: gnu.emacs.sources
; Subject: randomizing text
; Date: 26 Mar 92 19:34:26 GMT
; Reply-To: hermit%dastari.uucp@devon.lns.pa.us
; Organization: Competitive Computer Systems, Inc.
; 
;; Here are a couple functions I whipped up to take a region or
;; rectangle and randomize it. (It was the first thing I wrote in
;; Elisp.) The original purpose was to be able to include sample
;; reports from an accounting system in the documentation without
;; having to make up test data or reveal somebody's actual data.

;; randomize-rectangle randomizes a rectangle; randomize-text does the
;; region.

;; LCD Archive Entry:
;; randomize|Mark Buda|hermit@dastari.uucp|
;; Randomize a rectangle or region by replacing letters and numbers randomly.|
;; 92-03-26||~/functions/randomize.el.Z|

;; Capital letters are turned into other capital letters, lowercase
;; into other lowercase, and numbers into other numbers. Everything
;; else is left alone.

;; Copyright 1991 Mark R. Buda (hermit%dastari.uucp@devon.lns.pa.us).
;; You can do whatever you want with this except distribute it without
;; source code or misrepresent its authorship. (Meaning, you must
;; preserve this copyright notice, and if you distribute a modified
;; version, you must note that in the source.)

;; randomize some text

(load-library "rect")

(defun rnd (number)
  "Generate a random integer x, 0 <= x < NUMBER"
  (interactive "nRandom from 0 to: ")
  (setq number (% (random) number))
  (if (< number 0) (- number) number))

(defun randomize-text (start end)
  "Replace text and numbers in the region with randomly generated text."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char start)
      (while (/= (point) end)
	(cond ((looking-at "[0-9]")
	       (delete-char 1)
	       (insert-char (+ ?0 (rnd 10)) 1))
	      ((looking-at "[a-z]")
	       (delete-char 1)
	       (insert-char (+ ?a (rnd 26)) 1))
	      ((looking-at "[A-Z]")
	       (delete-char 1)
	       (insert-char (+ ?A (rnd 26)) 1))
	      (t (forward-char 1)))))))

(defun randomize-rectangle (start end)
  "Replace text and numbers in a rectangle with randomly generated text."
  (interactive "r")
  (operate-on-rectangle
   (function (lambda (start before end)
	       (randomize-text start (point))))
   start end nil))
