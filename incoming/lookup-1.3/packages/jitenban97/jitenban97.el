;;; jitenban97.el --- supplement package for $B!X<-!&E5!&HW(B97$B!Y(B
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
      '((:title . "$B<-!&E5!&HW(B97")))

(setq lookup-package-dictionary-options-alist
      (cond
       ((eq lookup-package-agent 'ndtp)
	;; ndtp $B$rMQ$$$k>l9g!"%5!<%P$G$N@_Dj$K=>$C$F<-=qL>$r=$@5$7$F2<$5$$!#(B
	;; $BNc$($P(B NDTPD $B$N@_Dj%U%!%$%k$G(B name JITENBAN97 $B$H$7$?>l9g$K$O!"(B
	;; $B<-=qL>$O<!$N$h$&$K$J$j$^$9!#(B
	(list (cons "JITENBAN97/IWAKOKU" (lookup-package-load "iwakoku"))
	      (cons "JITENBAN97/CHUJITEN" (lookup-package-load "chujiten"))
	      (cons "JITENBAN97/CHIEZO" (lookup-package-load "chiezo"))
	      (cons "JITENBAN97/MYPAEDIA" (lookup-package-load "mypaedia"))))
       ((eq lookup-package-agent 'ndeb)
	;; ndeb $B$N>l9g$O!"JQ99$NI,MW$O$"$j$^$;$s!#(B
	(list (cons "IWAKOKU" (lookup-package-load "iwakoku"))
	      (cons "CHUJITEN" (lookup-package-load "chujiten"))
	      (cons "CHIEZO" (lookup-package-load "chiezo"))
	      (cons "MYPAEDIA" (lookup-package-load "mypaedia"))))))

;;; jitenban97.el ends here
