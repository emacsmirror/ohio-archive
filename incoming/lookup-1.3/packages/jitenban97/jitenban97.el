;;; jitenban97.el --- supplement package for 『辞・典・盤97』
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: 1.0

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

(setq lookup-package-agent-options
      '((:title . "辞・典・盤97")))

(setq lookup-package-dictionary-options-alist
      (cond
       ((eq lookup-package-agent 'ndtp)
	;; ndtp を用いる場合、サーバでの設定に従って辞書名を修正して下さい。
	;; 例えば NDTPD の設定ファイルで name JITENBAN97 とした場合には、
	;; 辞書名は次のようになります。
	(list (cons "JITENBAN97/IWAKOKU" (lookup-package-load "iwakoku"))
	      (cons "JITENBAN97/CHUJITEN" (lookup-package-load "chujiten"))
	      (cons "JITENBAN97/CHIEZO" (lookup-package-load "chiezo"))
	      (cons "JITENBAN97/MYPAEDIA" (lookup-package-load "mypaedia"))))
       ((eq lookup-package-agent 'ndeb)
	;; ndeb の場合は、変更の必要はありません。
	(list (cons "IWAKOKU" (lookup-package-load "iwakoku"))
	      (cons "CHUJITEN" (lookup-package-load "chujiten"))
	      (cons "CHIEZO" (lookup-package-load "chiezo"))
	      (cons "MYPAEDIA" (lookup-package-load "mypaedia"))))))

;;; jitenban97.el ends here
