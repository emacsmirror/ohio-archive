;;; lookup-package.el --- management of supplement package
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-package.el,v 1.2 1999/05/23 17:27:20 knishida Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; $BJd=u%Q%C%1!<%8$H$O!"(BLookup $B$GMxMQ$9$k<-=q$NJdB-E*$J@_Dj$r!"FCDj$N=q<0(B
;; $B$K=>$C$FDj5A$7$?@_Dj%U%!%$%k=8$N$3$H$r8@$&!#$3$N%U%!%$%k$G$O!"Jd=u%Q%C(B
;; $B%1!<%8$r9=C[!&MxMQ$9$k$?$a$KI,MW$J4X?t$rMQ0U$7$F$$$k!#(B
;; 
;; $B0J2<!"%P!<%8%g%s(B 1.1 $B$NJd=u%Q%C%1!<%8$N=q<0$K$D$$$F@bL@$9$k!#(B

;; Package Files:
;; 
;; $BJd=u%Q%C%1!<%8$O!"C10l$N@_Dj%U%!%$%k$H$7$F!"$"$k$$$OJ#?t$N@_Dj%U%!%$(B
;; $B%k$r4^$`%G%#%l%/%H%j$H$7$F9=C[$5$l$k!#$$$:$l$N>l9g$K$b!"%Q%C%1!<%8$K(B
;; $B$O8GM-$N%Q%C%1!<%8L>$rIU$1$kI,MW$,$"$k!#Nc$($P(B "space" $B$H$$$&L>$N%Q%C(B
;; $B%1!<%8$r:n$k>l9g$K$O!"9=@.$O<!$N$h$&$K$J$k!#(B
;; 
;; o $BC10l%U%!%$%k$N>l9g(B
;; 
;;   space.el       $B@_Dj%U%!%$%k(B
;; 
;; o $BJ#?t%U%!%$%k$N>l9g(B
;; 
;;   space/         $B%G%#%l%/%H%j(B
;;     space.el     $B%^%9%?!<%U%!%$%k(B
;;     sun.el       $B@_Dj%U%!%$%k(B1
;;     earth.el     $B@_Dj%U%!%$%k(B2
;;     ...          ($B$$$/$D$"$C$F$b$h$$(B)
;; 
;; $B@_Dj%U%!%$%k$O(B "$B%Q%C%1!<%8L>(B.el" $B$H$9$k!#J#?t%U%!%$%k$rMQ$$$k>l9g$K$O!"(B
;; $B%Q%C%1!<%8L>$N%G%#%l%/%H%j$r:n$C$F!"$=$NCf$KI,MW$J$@$1$N%U%!%$%k$rMQ0U(B
;; $B$9$k!#$3$N>l9g!"%a%$%s$H$J$k@_Dj%U%!%$%k(B($B%^%9%?!<%U%!%$%k(B)$B$+$i!"<B:]$N(B
;; $B@_Dj%U%!%$%k$rFI$_9~$`$h$&$K5-=R$9$k!#(B

;; Package Contents:
;; 
;; $B@_Dj$O4pK\E*$K!"0J2<$NFs$D$NJQ?t$r%;%C%H$9$k$3$H$K$h$C$F9T$J$&!#(B
;; 
;; lookup-package-agent-options - $B8!:w%(!<%8%'%s%H$N%*%W%7%g%s$r@_Dj$9$k!#(B
;; lookup-package-dictionary-options-alist - $B<-=q$N%*%W%7%g%s$r@_Dj$9$k!#(B
;; 
;; $BNc$($P<!$N$h$&$K%U%!%$%k$K=q$1$P!"$=$l$GJd=u%Q%C%1!<%8$H$7$F@.$jN)$D!#(B
;; 
;;   (setq lookup-package-agent-options
;;         '((title . "The Immeasurable Space")))
;; 
;;   (setq lookup-package-dictionary-options-alist
;;         '(("sun" . ((title . "The Scorching Sun")))
;;           ("earth" . ((title . "The Earth as Mother")))))
;; 
;; $B@_Dj$N;EJ}$O!"B>$N%*%W%7%g%s@_Dj$HF1MM$G$"$k!#$?$@$7!"$3$3$G(B "sun" $B$H$+(B
;; "earch" $B$H$+$J$C$F$$$k$N$O<-=qL>$G$"$j!"<-=q(BID $B$G$O$J$$$3$H$KCm0U!#8e$+(B
;; $B$i%f!<%6$K$h$j%(!<%8%'%s%H(BID $B$,;XDj$5$l!"%*%W%7%g%s$H$7$F40@.$9$k!#(B
;; 
;; $B@_Dj$O<-=qKh$K9T$J$&J}$,@0M}$,IU$/$N$G!"<-=q%*%W%7%g%s$O8DJL$N%U%!%$%k(B
;; $B$GDj5A$7$F!"$=$l$r%m!<%I$77k2L$r$^$H$a$k$H$$$&J}K!$,9M$($i$l$k!#$=$l$r(B
;; $B%5%]!<%H$9$k$?$a$NJ}K!$,MQ0U$5$l$F$$$k!#(B

;; Supplement Files:
;; 
;; $B<-=q$N%*%W%7%g%s$r<B:]$KDj5A$7$F$$$k8D!9$N%U%!%$%k$r!"FC$KJd=u%U%!%$%k(B
;; $B$H8F$V!#Jd=u%U%!%$%k$G$O!"<!$NJQ?t$r%;%C%H$9$k!#(B
;; 
;; lookup-package-dictionary-options - $B<-=q$N%*%W%7%g%s$r@_Dj$9$k(B
;; 
;; $B$3$NJQ?t$K$O(B($B<-=qL>H4$-$N(B)$B%*%W%7%g%s$N$_$r%;%C%H$9$k!#Jd=u%U%!%$%k$O(B
;; $B4X?t(B `lookup-package-load' $B$K$h$C$FFI$_9~$^$l!"%;%C%H$5$l$?CM$,JV$5$l(B
;; $B$k!#Nc$($P<!$N$h$&$K$7$FMxMQ$9$k!#(B
;; 
;; -- sun.el --
;; 
;;   (setq lookup-package-dictionary-options
;;         '((title . "The Scorching Sun")
;;           ...))
;; 
;; -- space.el --
;; 
;;   (require 'lookup-package)
;; 
;;   (setq lookup-package-dictionary-options-alist
;;         (list (cons "sun" (lookup-package-load "sun"))
;;               ...))
;; 
;; `lookup-package-load' $B$O!"$3$N%U%!%$%k$GDj5A$7$F$$$k$N$G!"$"$i$+$8$a(B
;; require $B$7$F$*$/I,MW$,$"$k!#E57?E*$J%Q%C%1!<%8$O$3$l$G40@.$9$k!#(B

;; Useful Variables:
;; 
;; $B$"$k<o$N<-=q$O!"J#?t$N8!:w%(!<%8%'%s%H$K$h$jMxMQ$5$l$k$3$H$,$"$k!#$=$N(B
;; $B$?$aJd=u%Q%C%1!<%8$b!"$=$l$>$l$N%(!<%8%'%s%H$KBP1~$5$;$?J}$,ET9g$,$$$$!#(B
;; $B$b$7@_Dj$r%(!<%8%'%s%H$K$h$j6hJL$7$?$$>l9g!"Jd=u%U%!%$%k$G$OJQ?t(B 
;; `lookup-package-agent' $B$r;2>H$9$k$3$H$,=PMh$k!#$3$NJQ?t$K$O!"%Q%C%1!<%8(B
;; $B$,FI$_9~$^$l$k$H$-!"$=$l$,E,MQ$7$h$&$H$9$k%(!<%8%'%s%H$N%/%i%9$,%;%C%H(B
;; $B$5$l$k!#$3$l$K$h$j!"Nc$($P<!$N$h$&$KMxMQ$G$-$k!#(B
;; 
;;   (setq lookup-package-dictionary-options
;;         (cond
;;          ((eq lookup-package-agent 'ndic))
;;           '((title . "The Sun by ndic")))
;;          ((eq lookup-package-agent 'ndict))
;;           '((title . "The Sun by ndict")))
;;          ...))
;; 
;; $B$^$?!"%Q%C%1!<%8$K4^$^$l$k$=$NB>$N%U%!%$%k$rMxMQ$9$kEy$NL\E*$N$?$a$K!"(B
;; `lookup-package-file-directory' $B$r;2>H$G$-$k!#$3$l$K$OJd=u%U%!%$%k$N(B
;; $B$"$k%G%#%l%/%H%j$HF1$8%G%#%l%/%H%j$,<($5$l$k!#(B

;; Package Example:
;; 
;; $B<B:]E*$JJd=u%Q%C%1!<%89=C[$N<BNc$K$D$$$F$O!"4{B8$N%Q%C%1!<%8$r;2>H$7$F(B
;; $B$[$7$$!#$$$/$D$+$N%Q%C%1!<%8$O(B Lookup $B$N%[!<%`%Z!<%8$+$iF@$k$3$H$,=PMh$k!#(B
;; 
;;   http://www.ring.gr.jp/openlab/lookup/packages/

;;; Code:

(require 'lookup)

(defconst lookup-package-version "1.1"
  "$BJd=u%Q%C%1!<%8$N=q<0%P!<%8%g%s!#(B")

;;;
;:: Customizable variables
;;;

(defcustom lookup-package-directory
  (expand-file-name "packages" lookup-data-directory)
  "*$BJd=u%Q%C%1!<%8$,<}$a$i$l$k%G%#%l%/%H%j!#(B"
  :type 'directory
  :group 'lookup-setup)

;;;
;:: Package variables
;;;

(defvar lookup-package-agent nil
  "$B%Q%C%1!<%8$rMxMQ$7$h$&$H$9$k%(!<%8%'%s%H$N%/%i%9$,@_Dj$5$l$k!#(B")

(defvar lookup-package-file-directory nil
  "$BJd=u%U%!%$%k$N$"$k%G%#%l%/%H%j$,@_Dj$5$l$k!#(B")

(defvar lookup-package-agent-options nil
  "$B%Q%C%1!<%8$,Ds6!$9$k%(!<%8%'%s%H!&%*%W%7%g%s$r@_Dj$9$k!#(B")

(defvar lookup-package-dictionary-options nil
  "$BJd=u%U%!%$%k$,Ds6!$9$k<-=q%*%W%7%g%s$r@_Dj$9$k!#(B")

(defvar lookup-package-dictionary-options-alist nil
  "$B%Q%C%1!<%8$,Ds6!$9$k<-=q%*%W%7%g%s$NO"A[%j%9%H$r@_Dj$9$k!#(B")

;;;
;:: Construct functions
;;;

(defun lookup-package-load (name)
  "$BJd=u%U%!%$%k$rFI$_9~$_!"$=$3$G@_Dj$5$l$F$$$k<-=q%*%W%7%g%s$rJV$9!#(B
NAME $B$K$OJd=u%U%!%$%kL>$r;XDj$9$k!#3HD%;R$OI,MW$J$$!#(B
$BJd=u%U%!%$%k$G$O!"JQ?t(B `lookup-package-dictionary-options' $B$r;XDj$9$k(B
$BI,MW$,$"$k!#$=$NCM$,$3$N4X?t$NJV5QCM$H$J$k!#(B"
  (load (expand-file-name name lookup-package-file-directory) nil t)
  lookup-package-dictionary-options)

;;;
;:: Setup functions
;;;

(defun lookup-use-package (agent-id package-name)
  "$BJd=u%Q%C%1!<%8$r%m!<%I$9$k!#(B
AGENT-ID $B$O!"%Q%C%1!<%8$rE,MQ$9$k%(!<%8%'%s%H(BID$B!#(B
PACKAGE-NAME $B$O!"%Q%C%1!<%8L>!"$"$k$$$O%Q%C%1!<%8$N@dBP%Q%9!#(B
$B@dBP%Q%9$G$J$$>l9g!"%Q%C%1!<%8$OJQ?t(B `lookup-package-directory' $B$,(B
$B<($9%G%#%l%/%H%j$KE83+$7$F$*$/I,MW$,$"$k!#(B
$B$3$N4X?t$O(B `lookup-agent-options-alist' $B5Z$S(B
`lookup-dictionary-options-alist' $B$NCM$r=q$-49$($k!#(B"
  (let ((path (expand-file-name package-name lookup-package-directory))
	(lookup-package-agent (progn (string-match "^[^+]+" agent-id)
				     (intern (match-string 0 agent-id))))
	(lookup-package-file-directory lookup-package-directory)
	lookup-package-agent-options lookup-package-dictionary-options-alist)
    (message "Loading package %s..." package-name)
    ;; load package
    (unless (load path t t)
      (setq lookup-package-file-directory path)
      (unless (load (expand-file-name package-name path) t t)
	(error "Cannot find supplement package: %s" package-name)))
    ;; set agent options
    (lookup-foreach (lambda (option)
		      (lookup-set-agent-option agent-id
					       (car option) (cdr option)))
		    lookup-package-agent-options)
    ;; set dicitonary options
    (lookup-foreach (lambda (pair)
		      (let ((id (concat agent-id ":" (car pair))))
			(lookup-foreach (lambda (option)
					  (lookup-set-dictionary-option
					   id (car option) (cdr option)))
					(cdr pair))))
		    lookup-package-dictionary-options-alist)
    (message "Loading package %s...done" package-name)
    t))

(provide 'lookup-package)

;;; lookup-package.el ends here
