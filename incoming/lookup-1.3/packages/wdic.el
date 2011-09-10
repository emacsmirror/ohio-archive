;;; wdic.el --- supplement package for $B!XDL?.MQ8l$N4pACCN<1!Y(B
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Mito <mit@nines.nec.co.jp>
;; Target: ndic 1.0
;; Format: 1.1
;; Version: 1.1

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

(defconst wdic-arrange-functions
  '(wdic-arrange-references
    lookup-arrange-default-headings))

(setq lookup-package-agent-options
      (list (cons ':title  "$BDL?.MQ8l$N4pACCN<1(B")
            (cons ':arranges wdic-arrange-functions)))

(setq lookup-package-dictionary-options-alist
      `(("wave" . ((:title . "WDIC $BGHF0JT(B")))
	("yogo" . ((:title . "WDIC $BMQ8lJT(B")))
	("info" . ((:title . "WDIC $B>pJsJT(B")))
	("soft" . ((:title . "WDIC $B@=IJJT(B")))
	("tech" . ((:title . "WDIC $B5;=QJT(B")))
	("elec" . ((:title . "WDIC $BEE;RJT(B")))
	("comm" . ((:title . "WDIC $BDL?.>pJsJT(B")))
	("tran" . ((:title . "WDIC $BDL?.5;=QJT(B")))
	("moe"  . ((:title . "WDIC $BK($(JT(B")))
	("dev"  . ((:title . "WDIC $BAuCVJT(B")))
	("know" . ((:title . "WDIC $BCN<1JT(B")))
	("moe1" . ((:title . "WDIC $BK($(JT#1(B")))
	("moe2" . ((:title . "WDIC $BK($(JT#2(B")))
	("org"  . ((:title . "WDIC $BCDBNJT(B")))
	("paso" . ((:title . "WDIC $BMQ8lJT#2(B")))
	("rail" . ((:title . "WDIC $BE4F;JT(B")))
	("std"  . ((:title . "WDIC $B5,3JJT(B")))
        ))

(defun wdic-arrange-references (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
	start end heading url reference
        (ja-ascii-regexp "^\\(\\cA\\|[$B!!(B-$B!?!'(B-$B!w!N(B-$B!.!P(B-$B!A(B]\\)+$"))
    (while (re-search-forward "^ *\\($B"'(B\\|$B"&(B\\|$B!z(B\\|$B"!(B\\) *\\([^ \n]+\\)"
                              nil t)
      (setq start (match-beginning 2)
            end (match-end 2)
            heading (buffer-substring-no-properties start end)
            type (buffer-substring-no-properties
                  (match-beginning 1) (match-end 1)))
      (if (and (string-match "\\($B"'(B\\|$B"&(B\\)" type)
               (string-match " *\\([a-z]+://[^ ]+\\)" heading))
	  (setq url (match-string 1 heading)
		reference (lookup-make-url-reference url url))
        (setq heading (if (string-match ja-ascii-regexp heading)
                       (japanese-hankaku heading)
                     heading))
	(setq reference (lookup-make-reference dictionary heading heading))
	(lookup-reference-make-dynamic reference 'lookup-dynamic-code-search))
      (lookup-set-link start end reference))))

;;; wdic.el ends here
