;;; chujiten.el --- supplement file for 「新英和・和英中辞典」
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: $Id: chujiten.el,v 1.2 1999/05/23 17:27:30 knishida Exp $

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

(defconst chujiten-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" "(+)") ("ha122" "(++)") ("ha123" "(+++)") ("ha124" "(o)")
     ("ha125" ",B"" "^") ("ha126" nil "【") ("ha127" nil "】")
     ("ha128" "[") ("ha129" "]") ("ha12a" nil "~") ("ha12b" "-")
     ("ha12c" nil "'-") ("ha12d" nil "`-") ("ha12e" nil "a") ("ha12f" nil "e")
     ("ha130" "i") ("ha131" nil "^") ("ha132" ",B"" "^") ("ha133" nil ",")
     ("ha134" ",0N") ("ha135" nil ",0'(B") ("ha136" nil ",0:(B") ("ha137" nil ",0!(B")
     ("ha138" nil ",07(B") ("ha139" nil ",05(B") ("ha13a" nil ",0,(B") ("ha13b" ",A4" "'")
     ("ha13c" nil "E") ("ha13d" ",Aa" "a") ("ha13e" ",Ai" "e") ("ha13f" ",Am" "i")
     ("ha140" ",As" "o") ("ha141" ",Az" "u") ("ha142" nil ",0+(B") ("ha143" nil ",0'(B")
     ("ha144" nil ",0:(B") ("ha145" nil ",0!(B") ("ha146" nil ",07(B") ("ha147" nil ",05(B")
     ("ha148" nil ",0,(B") ("ha149" nil "`") ("ha14a" ",A`" "a") ("ha14b" ",Ah" "e")
     ("ha14c" ",Al" "i") ("ha14d" ",Ar" "o") ("ha14e" ",Ay" "u") ("ha14f" nil ",0+(B")
     ("ha150" ",0+(B") ("ha151" ",BA" "A") ("ha152" nil "B") ("ha153" ",BF" "C")
     ("ha154" nil "D") ("ha155" ",AI" "E") ("ha156" nil "F") ("ha157" nil "G")
     ("ha158" nil "H") ("ha159" ",BM" "I") ("ha15a" ",BE" "L") ("ha15b" nil "M")
     ("ha15c" ",BS" "O") ("ha15d" nil "P") ("ha15e" nil "Q") ("ha15f" ",B@" "R")
     ("ha160" ",B&" "S") ("ha161" nil "T") ("ha162" ",BZ" "U") ("ha163" nil "V")
     ("ha164" nil "X") ("ha165" ",B]" "Y") ("ha166" ",B," "Z") ("ha167" ",Ba" "a")
     ("ha168" ",Bi" "e") ("ha169" ",Bm" "i") ("ha16a" ",Bs" "o") ("ha16b" ",Bz" "u")
     ("ha16c" ",B}" "y") ("ha16d" ",A@" "A") ("ha16e" ",AH" "E") ("ha16f" ",AL" "I")
     ("ha170" ",AR" "O") ("ha171" nil ",0$(B") ("ha172" nil "") ("ha173" nil ",0$(B")
     ("ha174" nil "") ("ha175" ",Af") ("ha176" "") ("ha177" nil "S")
     ("ha178" nil "T") ("ha179" ",AY" "U") ("ha17a" nil "V") ("ha17b" ",A`" "a")
     ("ha17c" ",Ah" "e") ("ha17d" ",Al" "i") ("ha17e" ",Ar" "o")
     ("ha221" ",Ay" "u") ("ha222" nil "y") ("ha223" ",0;(B") ("ha224" ",0>(B")
     ("ha225" ",0<(B") ("ha226" ",0'(B") ("ha227" ",0:(B") ("ha228" ",0!(B") ("ha229" ",07(B")
     ("ha22a" ",05(B") ("ha22b" ",03(B") ("ha22c" ",0I") ("ha22d" ",0L") ("ha22e" ",0M")
     ("ha22f" ",0U") ("ha230" nil "/t") ("ha231" nil "|o") ("ha232" nil "_?")
     ("ha233" ",0r(B") ("ha234" ",0,(B") ("ha235" ",B3") ("ha236" "~") ("ha237" ",Ac")
     ("ha238" ",Aq") ("ha239" ",Ax") ("ha23a" ",AE") ("ha23b" ",B~") ("ha23c" nil "*")
     ("ha23d" ",A(") ("ha23e" ",AV") ("ha23f" ",Ad") ("ha240" ",Ak") ("ha241" ",Ao")
     ("ha242" ",Av") ("ha243" ",A|") ("ha244" "^") ("ha245" ",Ab") ("ha246" ",Aj")
     ("ha247" ",An") ("ha248" ",At") ("ha249" nil "=") ("ha24a" nil "=a")
     ("ha24b" "=e") ("ha24c" "=i") ("ha24d" "=o") ("ha24e" "=u") ("ha24f" "=y")
     ("ha250" ",Bc")  ("ha253" ",BH") ("ha255" ",Bh") ("ha257" ",B5") ("ha258" ",Bx")
     ("ha259" ",B9") ("ha25a" nil "m") ("ha25b" nil "4") ("ha25c" nil "8")
     ("ha25d" nil "b") ("ha25e" nil "f") ("ha25f" nil "h") ("ha260" nil "n")
     ("ha261" nil "p") ("ha262" nil "q") ("ha263" nil "r") ("ha264" nil "t")
     ("ha265" nil "1") ("ha266" nil "3") ("ha267" nil "s") ("ha268" nil "Y-")
     ("ha269" nil "$") ("ha26a" nil "\"") ("ha26b" "*") ("ha26c" nil "N")
     ("ha26d" nil "E") ("ha26e" nil "C") ("ha26f" nil "D") ("ha270" nil "G")
     ("ha271" nil "N") ("ha272" nil "P") ("ha273" nil "Q")
     ("ha274" nil ",00(B") ("ha275" nil "") ("ha276" nil ",0=(B") ("ha277" nil "")
     ("ha278" nil ",00(B") ("ha279" nil "") ("ha27a" nil "e") ("ha27b" nil "1")
     ("ha27c" nil "2") ("ha27d" nil "3") ("ha27e" nil "4") ("ha321" nil "5")
     ("ha322" nil "6") ("ha323" nil "7") ("ha324" nil "8") ("ha325" nil "9")
     ("ha326" nil "0") ("ha327" nil "-") ("ha328" nil "+") ("ha329" nil "n")
     ("ha32a" nil "x") ("ha32b" nil "b") ("ha32c" nil "1") ("ha32d" nil "2")
     ("ha32e" nil "3") ("ha32f" nil "4") ("ha330" nil "5") ("ha331" nil "6")
     ("ha332" nil "7") ("ha333" nil "8") ("ha334" nil "9") ("ha335" nil "0")
     ("ha336" nil "-") ("ha337" nil "+") ("ha338" nil "a") ("ha339" nil "e")
     ("ha33a" nil "i") ("ha33b" nil "j") ("ha33c" nil "k") ("ha33d" nil "m")
     ("ha33e" nil "n") ("ha33f" nil "p") ("ha340" nil "r") ("ha341" nil "s")
     ("ha342" nil "v") ("ha343" nil "x") ("ha344" nil "y") ("ha345" nil "z")
     ("ha346" nil "'") ("ha347" nil "`") ("ha348" nil "\"") ("ha349" nil "^")
     ("ha34a" nil "v") ("ha34b" nil "-") ("ha34c" nil "~") ("ha34d" nil "*")
     ("ha34e" nil ",") ("ha34f" nil ",") ("ha350" nil ".") ("ha351" nil ",B3")
     ("ha354" "♩") ("ha357" ",00(B") ("ha358" ",Aa") ("ha359" ",Ai") ("ha35a" ",As")
     ("ha35b" ",A`") ("ha35c" ",Ah") ("ha35d" ",Ab") ("ha35e" ",Aj") ("ha35f" ",An")
     ("ha360" ",At") ("ha361" ",Ad") ("ha362" ",Ak") ("ha363" ",Av") ("ha364" ",A|")
     ("ha365" ",Aq") ("ha366" nil "a") ("ha367" ",0N")
     ("ha372" nil "2") ("ha373" nil "3") ("ha374" nil "0") ("ha375" nil "1")
     ("ha376" nil "2") ("ha377" nil "3") ("ha378" nil "4") ("ha379" nil "5")
     ("ha37a" nil "6") ("ha37b" nil "8") ("ha37c" nil "1") ("ha37d" nil "4")
     ("ha37e" nil "5") ("ha421" nil "7") ("ha422" nil "8") ("ha423" nil "9")
     ("ha424" nil "a") ("ha425" nil "G") ("ha426" nil "P")
     ("ha429" nil "x") ("ha42a" nil "+") ("ha42b" nil "-") ("ha42c" nil "+")
     ("ha430" nil "0") ("ha431" nil "6") ("ha432" ",AE") ("ha433" nil "n")
     ("za321" "[名]") ("za322" "[代]") ("za323" "[形]") ("za324" "[動]")
     ("za325" "[副]") ("za326" "[接]") ("za327" "[前]") ("za328" "[冠]")
     ("za329" "[間]") ("za32a" "[助") ("za32b" "動]") ("za32c" "[接")
     ("za32d" "頭]") ("za32e" "尾]") ("za32f" "[U]") ("za330" "[C]")
     ("za331" "(単)") ("za332" "(複)") ("za333" "[A]") ("za334" "[P]")
     ("za335" "(自)") ("za336" "(他)") ("za337" "[成") ("za338" "句]")
     ("za339" nil "<音>") ("za33a" nil "<例>") ("za33b" nil "<メモ>")
     ("za33c" nil "<一覧>") ("za33f" "☞") ("za34e" "−") ("za34f" "↔")
     ("za37c" "(C)") ("za37d" "(R)") ("za722" "⇒"))))

(defconst chujiten-arranges
  '(lookup-arrange-gaijis
    chujiten-arrange-references
    chujiten-arrange-levels
    lookup-arrange-fill-lines))

(setq lookup-package-dictionary-options
      (list (cons ':gaiji-table chujiten-gaiji-table)
	    (cons ':arranges chujiten-arranges)
	    (cons ':stemmer 'stem-english)))

(defconst chujiten-reference-regexp
  (cond ((eq lookup-package-agent 'ndtp)
	 "→<\\([0-9a-f:]+\\)>")
	((eq lookup-package-agent 'ndeb)
	 "<reference>→</reference=\\([0-9a-f:]+\\)>")))

(defconst chujiten-eiwa-reference-regexp
  (concat chujiten-reference-regexp "\\([a-zA-Z' ]*[０-９]*\\>\\)"))

(defconst chujiten-waei-reference-regexp
  (concat chujiten-reference-regexp "\\([^ ,.\n]*\\)"))

(defun chujiten-arrange-references (entry)
  (if (chujiten-menu-entry-p entry)
      (lookup-arrange-references entry)
    (let ((case-fold-search nil)
	  (dictionary (lookup-entry-dictionary entry))
	  (regexp (if (chujiten-eiwa-entry-p entry)
		      chujiten-eiwa-reference-regexp
		    chujiten-waei-reference-regexp))
	  start region code heading)
      (while (re-search-forward regexp nil t)
	(setq start (match-beginning 0))
	(setq code (match-string 1) heading (match-string 2))
	(replace-match (concat "→" heading) t t)
	(setq entry (lookup-make-entry dictionary code heading))
	(lookup-set-link start (point) entry)))))

(defun chujiten-arrange-levels (entry)
  (if (chujiten-eiwa-entry-p entry)
      (chujiten-eiwa-arrange-levels entry)
    (chujiten-waei-arrange-levels entry)))

(defun chujiten-eiwa-arrange-levels (entry)
  ;; 見出し語を level 1
  (when (looking-at "\\(([+o]+)\\)?\\([^/\n]*\\) *\\(/[^/\n]+/\\)?$")
    (lookup-make-region-heading (match-beginning 2) (match-end 2) 1))
  (forward-line)
  ;; level 2-6
  (let ((case-fold-search nil) n
	(regexp (concat "^\\(−\\[[^]\n]+\\]\\)\\|"	; level 2
			"^\\([A-Z]\\>\\)\\|"		; level 3
			"^\\([0-9]+\\)?\\([a-z]\\)?\\>\\|" ; level 4, 5
			"^\\(\\*.*\\)"))) ; level 6
    (while (re-search-forward regexp nil t)
      (setq n 1)
      (while (<= n 6)
	(if (match-beginning n)
	    (lookup-make-region-heading
	     (match-beginning n) (match-end n) (1+ n)))
	(setq n (1+ n))))))

(defun chujiten-waei-arrange-levels (entry)
  (lookup-make-region-heading (point) (progn (end-of-line) (point)) 1)
  (forward-line)
  (while (re-search-forward "^\\([0-9]+\\)\\|^\\(【文例】\\)?\\(\\*.*\\)" nil t)
    (if (match-beginning 1)
	(lookup-make-region-heading (match-beginning 1) (match-end 0) 4)
      (lookup-make-region-heading (match-beginning 3) (match-end 3) 6))))

(defun chujiten-eiwa-entry-p (entry)
  (string< (lookup-entry-code entry) "6e8d"))

(defun chujiten-menu-entry-p (entry)
  (let ((code (lookup-entry-code entry)))
    (or (string< code "17a2") (string< "a773" code))))

;;; chujiten.el ends here
