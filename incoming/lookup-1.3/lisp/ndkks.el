;;; ndkks.el --- Lookup KAKASI interface
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndkks.el,v 1.2 1999/05/23 17:27:23 knishida Exp $

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

;;; Code:

(require 'lookup)

(defconst ndkks-version "1.0")

;;;
;:: Customizable variables
;;;

(defgroup ndkks nil
  "Lookup KAKASI interface."
  :group 'lookup-agents)

(defcustom ndkks-program-name "kakasi"
  "*KAKASI $B$N%W%m%0%i%`L>!#(B"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-dictionary-title "$B4A;z$+$J<-E5(B"
  "*ndkks $B<-=q$N%?%$%H%k!#(B"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-content-format
  '(t "\n" ("-JH") "\n" "$B!Z?6$j2>L>![(B" ("-JH" "-f" "-p") "\n")
  "*ndkks $B<-=q$,=PNO$9$k%(%s%H%jFbMF$N%U%)!<%^%C%H!#(B
$B%j%9%H$N3FMWAG$H$7$F!"J8;zNs!"J8;zNs$N%j%9%H!"5Z$S(B `t' $B$r;XDj$G$-$k!#(B
$BJ8;zNs$N>l9g!"$=$l$,$=$N$^$^A^F~$5$l$k!#(B
$B%j%9%H$N>l9g!"$=$l$r0z?t$H$7$F(B KAKASI $B$,8F$S=P$5$l!"$=$N7k2L$,A^F~$5$l$k!#(B
`t' $B$N>l9g!"8!:w8l$,A^F~$5$l$k!#(B"
  :type '(repeat (radio :tag "$B=PNOFbMF(B"
			(const :tag "$B8!:w8l(B" t)
			(string :tag "$BJ8;zNs(B")
			(repeat :tag "KAKASI $B8F$S=P$7(B"
				(string :tag "option"))))
  :group 'ndkks)

(defcustom ndkks-process-coding-system lookup-kakasi-coding-system
  "*Coding system for KAKASI process."
  :type 'symbol
  :group 'ndkks)


;;;
;:: Internal variables
;;;

(if (string-match "^19" emacs-version) 
    (defconst ndkks-valid-charsets (list lc-jp))
  (defconst ndkks-valid-charsets '(japanese-jisx0208)))

;;;
;:: types
;;;

;; ndkks agent:
;;
;;   (ndkks)

(put 'ndkks ':methods '(exact))

(put 'ndkks ':arranges
     '(lookup-arrange-default-headings
       lookup-arrange-fill-lines))

(put 'ndkks ':adjusts 
     '(lookup-adjust-goto-min))

;; ndkks dictionary:
;;
;; CODE  - none
;; NAME  - same as `ndkks-program-name'

;; ndkks entry:
;;
;; CODE    - input string
;; HEADING - same as CODE above


;;;
;:: Interface functions
;;;

(put 'ndkks 'setup 'ndkks-setup)
(defun ndkks-setup (agent)
  (unless (featurep 'mule)
    (error "ndkks requires `mule' feauture."))
  (call-process ndkks-program-name nil 0)	; check KAKASI exists
  (list (lookup-new-dictionary agent nil ndkks-program-name
			       ndkks-dictionary-title)))

(put 'ndkks 'clear 'ndkks-clear)
(defun ndkks-clear (agent)
  (ndkks-process-kill))

(put 'ndkks 'search 'ndkks-dictionary-search)
(defun ndkks-dictionary-search (dictionary query)
  (let ((string (lookup-query-string query)))
    (and
     ;; xxx: $B4A;z$,4^$^$l$F$$$k$+D4$Y$?$$$N$@$,!"$I$&$d$l$P$$$$$s$@$m$&(B?
     ;; xxx: $B$H$j$"$($:!"E,Ev$K%A%'%C%/!#(B
     (let ((charsets (find-charset-string string)))
       (catch 'return
	 (while charsets
	   (if (memq (car charsets) ndkks-valid-charsets)
	       (throw 'return t)
	     (setq charsets (cdr charsets))))))
     (string-match "[^$B$"(B-$B$s%"(B-$B%s!<#A(B-$B#Z#a(B-$B#z(B]" string)
     (list (lookup-make-entry dictionary string string)))))

(put 'ndkks 'content 'ndkks-dictionary-content)
(defun ndkks-dictionary-content (dictionary entry)
  (let ((string (lookup-entry-code entry)))
    (mapconcat (lambda (element)
		 (cond ((eq element t) string)
		       ((stringp element) element)
		       ((listp element) (ndkks-process-require element string))
		       (t (error "Invalid format element: %S" element))))
	       ndkks-content-format "")))


;;;
;:: KAKASI process
;;;

(defvar ndkks-process-alist nil)

(defun ndkks-get-process (args)
  (let ((process (lookup-assoc-ref ndkks-process-alist args)))
    (unless (and process (eq (process-status process) 'run))
      (if process (kill-process process))
      (let ((buffer (lookup-open-process-buffer " *ndkks*")))
	(setq process (apply 'start-process "ndkks" buffer
			     ndkks-program-name args))
	(process-kill-without-query process)
	;; $B5/F08e!">/$7;~4V$rCV$+$J$$$H!":G=i$N8!:w$,$&$^$/$$$+$J$$!#(B
	(sleep-for 0.1)
	(let ((coding ndkks-process-coding-system))
	  (when coding
	    (set-process-coding-system process coding coding)))
	(setq ndkks-process-alist
	      (lookup-assoc-set ndkks-process-alist args process))))
    process))

(defun ndkks-process-require (args string)
  (lookup-process-require (ndkks-get-process args) (concat string "\n") "\n"))

(defun ndkks-process-kill ()
  (while ndkks-process-alist
    (lookup-process-kill (cdar ndkks-process-alist))
    (setq ndkks-process-alist (cdr ndkks-process-alist))))

(provide 'ndkks)

;;; ndkks.el ends here
