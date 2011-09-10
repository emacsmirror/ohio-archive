;;; mypaedia.el --- supplement file for $B!V%^%$%Z%G%#%"#9#7!W(B
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: $Id: mypaedia.el,v 1.2 1999/05/23 17:27:30 knishida Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup-package)

(defconst mypaedia-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" ",A4(B") ("ha122" ",0,(B") ("ha123" ",07(B") ("ha124" ",0#(B") ("ha125" ",0'(B")
     ("ha126" ",0I(B") ("ha127" ",0U(B") ("ha128" ",0M(B") ("ha129" ",0r(B") ("ha12a" "`")
     ("ha12b" nil "1/2") ("ha12c" nil "1/3") ("ha12d" ",AA(B") ("ha12e" ",Aa(B")
     ("ha12f" ",A@(B") ("ha130" ",A`(B") ("ha133" ",AB(B") ("ha134" ",Ab(B") ("ha136" ",AD(B")
     ("ha137" ",Ad(B") ("ha138" ",AC(B") ("ha139" ",Ac(B") ("ha13a" ",AE(B") ("ha13b" ",Ae(B")
     ("ha13c" ",AF(B") ("ha13d" ",Af(B") ("ha154" ",Aj(B") ("ha157" ",AK(B") ("ha158" ",Ak(B")
     ("ha161" ",Am(B") ("ha162" ",AL(B") ("ha168" ",AO(B") ("ha169" ",Ao(B") ("ha16a" ",0!(B")
     ("ha16b" ",0Z(B") ("ha16e" ",B#(B") ("ha17a" ",AS(B") ("ha17b" ",As(B") ("ha17c" ",AR(B")
     ("ha17d" ",Ar(B") ("ha222" ",AT(B") ("ha223" ",At(B") ("ha224" ",AV(B") ("ha225" ",Av(B")
     ("ha226" ",AU(B") ("ha227" ",Au(B") ("ha228" ",AX(B") ("ha229" ",Ax(B") ("ha22e" ",01(B")
     ("ha22f" ",00(B") ("ha242" ",Az(B") ("ha243" ",AY(B") ("ha244" ",Ay(B") ("ha247" ",A\(B")
     ("ha248" ",A|(B") ("ha25b" ",0H(B") ("ha25c" ",0L(B")
     ("za121" nil ",A4(B") ("za122" nil ",0,(B") ("za123" nil ",07(B") ("za124" nil ",0#(B")
     ("za125" nil ",0'(B") ("za126" nil ",0I(B") ("za127" nil ",0U(B") ("za128" nil ",0M(B")
     ("za129" nil ",0r(B") ("za12a" nil "`") ("za12b" nil "1/2")
     ("za12c" nil "1/3") ("za12d" nil ",AA(B") ("za12e" nil ",Aa(B") ("za12f" nil ",A@(B")
     ("za130" nil ",A`(B") ("za133" nil ",AB(B") ("za134" nil ",Ab(B") ("za136" nil ",AD(B")
     ("za137" nil ",Ad(B") ("za138" nil ",AC(B") ("za139" nil ",Ac(B") ("za13a" nil ",AE(B")
     ("za13b" nil ",Ae(B") ("za13c" nil ",AF(B") ("za13d" nil ",Af(B") ("za154" nil ",Aj(B")
     ("za157" nil ",AK(B") ("za158" nil ",Ak(B") ("za161" nil ",Am(B") ("za162" nil ",AL(B")
     ("za168" nil ",AO(B") ("za169" nil ",Ao(B") ("za16a" nil ",0!(B") ("za16b" nil ",0Z(B")
     ("za16e" nil ",B#(B") ("za17a" nil ",AS(B") ("za17b" nil ",As(B") ("za17c" nil ",AR(B")
     ("za17d" nil ",Ar(B") ("za222" nil ",AT(B") ("za223" nil ",At(B") ("za224" nil ",AV(B")
     ("za225" nil ",Av(B") ("za226" nil ",AU(B") ("za227" nil ",Au(B") ("za228" nil ",AX(B")
     ("za229" nil ",Ax(B") ("za22e" nil ",01(B") ("za22f" nil ",00(B") ("za242" nil ",Az(B")
     ("za243" nil ",AY(B") ("za244" nil ",Ay(B") ("za247" nil ",A\(B") ("za248" nil ",A|(B")
     ("za25b" nil ",0H(B") ("za25c" nil ",0L(B"))))

(defconst mypaedia-arranges
  '(lookup-arrange-gaijis
    mypaedia-arrange-references
    lookup-arrange-default-headings
    lookup-arrange-fill-lines))

(setq lookup-package-dictionary-options
      (list (cons ':gaiji-table mypaedia-gaiji-table)
	    (cons ':arranges mypaedia-arranges)))

(defconst mypaedia-reference-regexp
  (cond ((eq lookup-package-agent 'ndtp)
	 "$B"*(B\\(.*\\)<\\([0-9a-f:]+\\)>\n")
	((eq lookup-package-agent 'ndeb)
	 "<reference>$B"*(B\\(.*\\)</reference=\\([0-9a-f:]+\\)>\n")))

(defun mypaedia-arrange-references (entry)
  ;; $B$3$N<-=q$O%j%s%/$,%F%-%9%H$N:G8e$K$^$H$a$i$l$F$$$k$N$@$,!"(B
  ;; $BB>$N<-=q$K9g$o$;$F!"%F%-%9%H$NESCf$K:n$k!#(B
  (let ((dictionary (lookup-entry-dictionary entry))
	heading code start end)
    (while (re-search-forward mypaedia-reference-regexp nil t)
      (setq start (match-beginning 0) end (match-end 0))
      (setq heading (match-string 1) code (match-string 2))
      (setq entry (lookup-make-entry dictionary code heading))
      (delete-region start end)
      (if (search-backward (concat "$B"M(B" heading) nil t)
	  (setq start (match-beginning 0) end (match-end 0))
	(insert "$B"*(B" heading "\n")
	(setq end (1- (point))))
      (lookup-set-link start end entry))))

;;; mypaedia.el ends here
