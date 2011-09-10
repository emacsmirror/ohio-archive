;;; iwakoku.el --- supplement file for 「岩波国語辞典」
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: $Id: iwakoku.el,v 1.2 1999/05/23 17:27:30 knishida Exp $

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

(defconst iwakoku-gaiji-table
  (lookup-new-gaiji-table
   '(("za321" "[名]") ("za322" "[代]") ("za323" "[形]") ("za324" "[動]")
     ("za325" "[副]") ("za326" "[接]") ("za327" "[前]") ("za328" "[冠]")
     ("za329" "[間]") ("za32a" "[助") ("za32b" "動]") ("za32c" "[接")
     ("za32d" "頭]") ("za32e" "尾]") ("za32f" "[U]") ("za330" "[C]")
     ("za331" "(単)") ("za332" "(複)") ("za333" "[A]") ("za334" "[P]")
     ("za335" "(自)") ("za336" "(他)") ("za337" "[成") ("za338" "句]")
     ("za339" nil "<音>") ("za33a" nil "<例>") ("za33b" nil "<メモ>")
     ("za33c" nil "<一覧>") ("za33f" "☞") ("za34f" "↔")
     ("za37c" "(C)") ("za37d" "(R)") ("za722" "⇒"))))

(defconst iwakoku-arranges
  '(iwakoku-fix-by-black-list
    lookup-arrange-gaijis
    lookup-arrange-references
    lookup-arrange-default-headings
    iwakoku-arrange-headings
    lookup-arrange-fill-lines))

(setq lookup-package-dictionary-options
      (list (cons ':gaiji-table iwakoku-gaiji-table)
	    (cons ':arranges iwakoku-arranges)))

(defun iwakoku-fix-by-black-list (entry)
  (let ((code (lookup-entry-code entry)))
    (cond
     ((string= code "212d:d8")
      (search-forward "さ\nた") (replace-match "さた")))))

(defun iwakoku-arrange-headings (entry)
  (while (re-search-forward "\\(\\[.\\]\\)\\|\\(([0-9]+)\\)\\|([ア-ン]+)" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0)
				(if (match-beginning 1) 3
				  (if (match-beginning 2) 4 5)))
    ;; この辞書は、項目が一行にずらずらと並べられているので、
    ;; 適当な個所で改行しておく。
    (save-excursion
      (goto-char (match-beginning 0))
      (unless (or (bolp) (get-text-property (- (point) 2) 'lookup-heading))
	(newline)))))

;;; iwakoku.el ends here
