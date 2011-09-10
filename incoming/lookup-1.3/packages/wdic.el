;;; wdic.el --- supplement package for 『通信用語の基礎知識』
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
      (list (cons ':title  "通信用語の基礎知識")
            (cons ':arranges wdic-arrange-functions)))

(setq lookup-package-dictionary-options-alist
      `(("wave" . ((:title . "WDIC 波動編")))
	("yogo" . ((:title . "WDIC 用語編")))
	("info" . ((:title . "WDIC 情報編")))
	("soft" . ((:title . "WDIC 製品編")))
	("tech" . ((:title . "WDIC 技術編")))
	("elec" . ((:title . "WDIC 電子編")))
	("comm" . ((:title . "WDIC 通信情報編")))
	("tran" . ((:title . "WDIC 通信技術編")))
	("moe"  . ((:title . "WDIC 萌え編")))
	("dev"  . ((:title . "WDIC 装置編")))
	("know" . ((:title . "WDIC 知識編")))
	("moe1" . ((:title . "WDIC 萌え編１")))
	("moe2" . ((:title . "WDIC 萌え編２")))
	("org"  . ((:title . "WDIC 団体編")))
	("paso" . ((:title . "WDIC 用語編２")))
	("rail" . ((:title . "WDIC 鉄道編")))
	("std"  . ((:title . "WDIC 規格編")))
        ))

(defun wdic-arrange-references (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
	start end heading url reference
        (ja-ascii-regexp "^\\(\\cA\\|[　-／：-＠［-｀｛-〜]\\)+$"))
    (while (re-search-forward "^ *\\(▼\\|▽\\|★\\|◆\\) *\\([^ \n]+\\)"
                              nil t)
      (setq start (match-beginning 2)
            end (match-end 2)
            heading (buffer-substring-no-properties start end)
            type (buffer-substring-no-properties
                  (match-beginning 1) (match-end 1)))
      (if (and (string-match "\\(▼\\|▽\\)" type)
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
