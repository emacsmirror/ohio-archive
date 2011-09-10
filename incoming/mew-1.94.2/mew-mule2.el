;;; mew-mule2.el --- Environment of Mule version 2 for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-mule2-version "mew-mule2.el version 0.09")

;; must be here
(if (fboundp 'find-coding-system)
    (fset 'mew-coding-system-p (symbol-function 'find-coding-system))
  (fset 'mew-coding-system-p (symbol-function 'coding-system-p)))

;; In the context of Mew, 'charset' means MIME charset.
;; 'cs' means the internal representation of Emacs (was known as Mule).

;;
;; User CS definitions
;;

(define-ccl-program mew-ccl-*-to-lf
  '(1
    ((read r0)
     (loop
      (if (r0 == 13)
	  ((write 10)
	   (read-if (r1 == 10)
		    (read r0)
		    (r0 = (r1 + 0)))
	   (repeat))
	(write-read-repeat r0)))))
  "Read text with any eol converting to the LF eol.")

(define-ccl-program mew-ccl-lf-to-lf
  '(1
    ((read r0)
     (loop
      (write-read-repeat r0))))
  "Write text with the LF eol.")

(define-ccl-program mew-ccl-lf-to-crlf
  '(2
    ((loop
      (read-if (r0 == 10)
	       (write 13))
      (write r0)
      (repeat))))
  "Write text with the CRLF eol.")

(define-ccl-program mew-ccl-lf-to-cr
  '(1
    ((loop
      (read-if (r0 == 10)
	       (write 13)
	       (write r0))
      (repeat))))
  "Write text with the CR eol.")

(make-coding-system 'mew-cs-text
		    4 ?= "No conversion" nil
		    (cons mew-ccl-*-to-lf mew-ccl-lf-to-lf))

(make-coding-system 'mew-cs-text-lf
		    4 ?= "No conversion" nil
		    (cons mew-ccl-*-to-lf mew-ccl-lf-to-lf))

(make-coding-system 'mew-cs-text-crlf
		    4 ?= "No conversion" nil
		    (cons mew-ccl-*-to-lf mew-ccl-lf-to-crlf))

(make-coding-system 'mew-cs-text-cr
		    4 ?= "No conversion" nil
		    (cons mew-ccl-*-to-lf mew-ccl-lf-to-cr))

(defvar mew-cs-dummy          '*noconv*)
(defvar mew-cs-binary         '*noconv*)
(defvar mew-cs-text-for-read  'mew-cs-text)
(defvar mew-cs-text-for-write 'mew-cs-text-lf)
(defvar mew-cs-autoconv       '*autoconv*)
(defvar mew-cs-7bit           '*iso-2022-ss2-7*)
(defvar mew-cs-7bit-crlf      '*iso-2022-ss2-7*dos)
(defvar mew-cs-mime-trans     '*iso-2022-ss2-7*)
(defvar mew-cs-rfc822-trans   '*iso-2022-ss2-7*) 
(defvar mew-cs-draft          '*iso-2022-ss2-7*)
(defvar mew-cs-scan           '*ctext*)
(defvar mew-cs-infile         '*autoconv*)
(defvar mew-cs-outfile        '*iso-2022-ss2-7*)
(defvar mew-cs-virtual        (if (mew-coding-system-p '*ctext*unix)
				  '*ctext*unix '*ctext*)) ;; ^M as it is
(defvar mew-cs-pick           '*euc-japan*)

(defvar mew-cs-database
  '(("us-ascii"    (0)             nil           "7bit"
                                   nil           "Q")
    ("iso-8859-1"  (0 129)         *iso-8859-1*  "quoted-printable"
                                   *iso-8859-1*  "Q")
    ("iso-8859-2"  (0 130)         *iso-8859-2*  "quoted-printable"
                                   *iso-8859-2*  "Q")
    ("iso-8859-3"  (0 131)         *iso-8859-3*  "quoted-printable"
                                   *iso-8859-3*  "Q")
    ("iso-8859-4"  (0 132)         *iso-8859-4*  "quoted-printable"
                                   *iso-8859-4*  "Q")
    ("koi8-r"	   (0 140)         *koi8*        "quoted-printable"
                                   *koi8*        "Q")
    ("iso-8859-6"  (0 135)         *iso-8859-6*  "base64"
                                   *iso-8859-6*  "B")
    ("iso-8859-7"  (0 134)         *iso-8859-7*  "base64"
                                   *iso-8859-7*  "B")
    ("iso-8859-8"  (0 136)         *iso-8859-8*  "base64"
                                   *iso-8859-8*  "B")
    ("iso-8859-9"  (0 141)         *iso-8859-9*  "quoted-printable"
                                   *iso-8859-9*  "Q")
    ("tis-620"     (0 133 128)     *tis620*      "base64"
                                   *tis620*      "B")
    ("iso-2022-jp" (0 138 146 144) *iso-2022-jp* "7bit"
                                   *iso-2022-jp* "B")
    ("euc-kr"      (0 147)         *euc-kr*      "base64"
                                   *euc-kr*      "B")
    ("iso-2022-kr" (0 147)         *iso-2022-kr* "7bit"
                                   *euc-kr*      "B")
    ("gb2312"      (0 145)         *euc-china*   "base64"
                                   *euc-china*   "B")
    ("hz-gb-2312"  (0 145)         *hz*          "base64"
                                   *hz*          "B")
    ("big5"        (0 152 153)     *big5*        "base64"
                                   *big5*        "B")
    ("iso-2022-jp-2" (0 138 146 144 145 147 148 129 134) 
     *iso-2022-ss2-7* "7bit" *iso-2022-ss2-7* "B")
    ;; charset-to-cs purpose only
    ("euc-jp"          nil *euc-japan*)
    ("shift_jis"       nil *sjis*)
    ("cn-gb"           nil *euc-china*) ;; the same as gb2312 above
    ("cn-big5"         nil *big5*)      ;; the same as big5 above
;   ("iso-2022-cn"     nil xxx)
;   ("iso-2022-cn-ext" nil xxx)
    ("iso-2022-int-1"  nil *iso-2022-int-1*)
    ))

;;
;; Leading characters
;;

(defvar mew-lc-ascii 0)
(defvar mew-lc-kana  137)
(defvar mew-lc-jp    146)
(fset 'mew-make-char (symbol-function 'make-character))

(defun mew-char-charset (char)
  (cond
   ((<= char 128) 0)
   (t (aref (char-to-string char) 0))))

;;
;; CS
;;

(defun mew-find-cs-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((re-official "[^\232-\235\240-\377")
	    (re-private "\\|[\232-\235][^\000-\237")
	    lclist lc mc-flag)
	(while (re-search-forward (concat re-official "]" re-private "]")
				  nil t)
	  (setq lc (preceding-char))
	  (cond
	   ((<= lc ?\177)
	    (setq lc 0)
	    (setq re-official (concat re-official "\000-\177")))
	   ((< lc ?\240)
	    (setq re-official (concat re-official (char-to-string lc))))
	   (t
	    (setq re-private (concat re-private (char-to-string lc))))
	   )
	  (setq lclist (cons lc lclist)))
	lclist))))

;; to internal
(defun mew-cs-decode-region (beg end cs)
  (if cs (code-convert-region beg end cs '*internal*)))

;; to extenal
(defun mew-cs-encode-region (beg end cs)
  (if cs (code-convert-region beg end '*internal* cs)))

;; to internal
(defun mew-cs-decode-string (str cs)
  (if cs (code-convert-string str cs '*internal*) str))

;; to external
(defun mew-cs-encode-string (str cs)
  (if cs (code-convert-string str '*internal* cs) str))

;;
;; Process environment
;;

(defun mew-set-process-cs (pro from-pro to-pro)
  (set-process-coding-system pro from-pro to-pro))

(defmacro mew-plet (&rest body)
  (` (let ((call-process-hook nil)
	   (default-process-coding-system
	     (cons '*noconv* '*noconv*)))
       (,@ body))))

(defmacro mew-piolet (input output &rest body)
  (` (let ((call-process-hook nil)
	   (default-process-coding-system
	     (cons (, input) (, output))))
       (,@ body))))

(defmacro mew-pioalet (input output arg &rest body)
  (` (let ((call-process-hook nil)
	   (default-process-coding-system
	     (cons (, input) (, output)))
	   (pathname-coding-system (, arg)))
       (,@ body))))

(defmacro mew-flet (&rest body)
  (` (let ((file-coding-system          '*noconv*)
	   (file-coding-system-for-read '*noconv*)
	   jam-zcat-filename-list
	   jka-compr-compression-info-list)
       (,@ body))))

(defmacro mew-frwlet (read write &rest body)
  (` (let ((file-coding-system          (, write))
	   (file-coding-system-for-read (, read))
	   jam-zcat-filename-list
	   jka-compr-compression-info-list)
       (,@ body))))

;;
;; Post conversion
;;

(defmacro mew-cs-post-conv (cs)
  (` (get (, cs) 'post-read-conversion)))

;;
;;
;;

(fset 'mew-aref (symbol-function 'sref))
(fset 'mew-charlen (symbol-function 'char-bytes))

;;
;;
;;

(require 'mew-mule)
(provide 'mew-mule2)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-mule2.el ends here
