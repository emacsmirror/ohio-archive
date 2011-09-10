;From: indetech!lrs@ai.mit.edu (Lynn Slater)
;Newsgroups: gnu.emacs
;Subject: Alternative to prefix-region
;Message-ID: <m0fq6S5-000213C@spinel.indetech.uucp>
;Date: 4 Aug 89 15:27:00 GMT
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 52
;
;Inclosed are two soimple functions which I have found to be very handy.
;The first, insert-box, does what prefix-region does except that it can
;insert into the middle of lines as well as at the front of the line.

(defun insert-box (start end text)
  "Insert a text prefix at a column in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START.
   The rough inverse of this function is kill-rectangle."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (setq cc  (current-column))
      (while (< (point) end) ;; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(move-to-column cc)) ;; use move-to-column-force if you have it
      (move-marker end nil))))

(defun insert-end (start end text)
  "Insert a text prefix at the end in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (end-of-line)	
      (while (< (point) end);; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(end-of-line)	
	)
      (move-marker end nil))))

;-- Happy Hacking 
;===============================================================
;Lynn Slater -- {sun, ames, pacbell}!indetech!lrs or lrs@indetech.uucp
;42075 Lawrence Place, Fremont Ca 94538
;Office (415) 438-2048; Home (415) 796-4149; Fax (415) 438-2034
;===============================================================
