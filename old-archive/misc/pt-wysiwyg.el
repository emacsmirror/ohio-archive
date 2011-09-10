;From: lrs@indetech.uucp (Lynn Slater)
;Newsgroups: gnu.emacs
;Subject: Re: automatic horizontal scrolling
;Message-ID: <1989Aug23.210346.28643@indetech.uucp>
;Date: 23 Aug 89 21:03:46 GMT
;References: <815@pitstop.West.Sun.COM>
;Reply-To: lrs@indetech.UUCP (Lynn Slater)
;Distribution: gnu
;Organization: Independence Technologies, Inc. Fremont, CA
;Lines: 72
;Keywords: scrolling, horizontal
;
;> I feel that an interesting feature is missing in gnuemacs: automatic
;> horizontal scrolling 
;
;I have some of this in an enhanced picture mode. point-wysiwyg directly
;addresses your wish. This and other core functions are included below.

(defun move-to-column-force (column)
  "Move to column COLUMN in current line.
Differs from move-to-column in that it creates or modifies whitespace
if necessary to attain exactly the specified column.

This version (non-standard) insures that the column is visible,
scrolling if needed."
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
        (indent-to column)
      (if (and (/= col column)
               (= (preceding-char) ?\t))
          (let (indent-tabs-mode)
            (delete-char -1)
            (indent-to col)
            (move-to-column column)))))
  (point-wysiwyg)
  )

(defun point-wysiwyg ()
  "scrolls the window horozontally to make point visible"
  (let*  ((min (window-hscroll))
          (max (- (+ min (window-width)) 2))
          (here (current-column))
          (delta (/ (window-width) 2))
          )
    (if (< here min)
        (scroll-right (max 0 (+ (- min here) delta)))
      (if (>= here  max)
          (scroll-left (- (- here min) delta))
        ))))
  
(defun window-wysiwyg-point ()
  "Makes point become the visible point
   Moves point, not the scroll.
   Current version good only for picture mode"
  (interactive)
  (let*  ((min (window-hscroll))
          (max (+ min (window-width)))
          (here (current-column))
          (delta (/ (window-width) 2))
          )
    (if (< here min)
        (move-to-column min)
      (if (>= here  max)
          (move-to-column (- max 3))
        ))))


;Now, you just have to get point-wysiwyg called from all commands that move
;point. :->  I have done this for a variant of picture mode.  There is
;probably a general way to associate this with the truncate line variable so
;that if lines are not truncated, point-wysiwig is called by the lower level
;code, but I have not explored this.
;
;===============================================================
;Lynn Slater -- {sun, ames, pacbell}!indetech!lrs or lrs@indetech.uucp
;42075 Lawrence Place, Fremont Ca 94538
;Office (415) 438-2048; Home (415) 796-4149; Fax (415) 438-2034
;===============================================================
