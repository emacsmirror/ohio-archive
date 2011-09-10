;;; mew-mule3.el --- Environment of Mule version 3 for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-mule3-version "mew-mule3.el version 0.14")

;; must be here
(if (fboundp 'find-coding-system)
    (fset 'mew-coding-system-p (symbol-function 'find-coding-system))
  (fset 'mew-coding-system-p (symbol-function 'coding-system-p)))

;; In the context of Mew, 'charset' means MIME charset.
;; 'cs' means the internal representation of Emacs (was known as Mule).

;;
;; User CS definitions
;;

;; iso-2022-7bit-ss2 is iso-2022-jp-2

(cond
 (mew-xemacs-p
  (copy-coding-system 'no-conversion      'mew-cs-text)
  (copy-coding-system 'no-conversion-unix 'mew-cs-text-lf)
  (copy-coding-system 'no-conversion-dos  'mew-cs-text-crlf)
  (copy-coding-system 'no-conversion-mac  'mew-cs-text-cr))
 (t
  (define-coding-system-alias 'mew-cs-text      'raw-text)
  (define-coding-system-alias 'mew-cs-text-lf   'raw-text-unix)
  (define-coding-system-alias 'mew-cs-text-crlf 'raw-text-dos)
  (define-coding-system-alias 'mew-cs-text-cr   'raw-text-mac)))

(defvar mew-cs-dummy          'binary)
(defvar mew-cs-binary         'binary)
(defvar mew-cs-text-for-read  'mew-cs-text)
(defvar mew-cs-text-for-write 'mew-cs-text-lf)
(defvar mew-cs-autoconv       'undecided)
(defvar mew-cs-7bit           'iso-2022-7bit-ss2)
(defvar mew-cs-7bit-crlf      'iso-2022-7bit-ss2-dos)
(defvar mew-cs-mime-trans     'iso-2022-7bit-ss2)
(defvar mew-cs-rfc822-trans   'iso-2022-7bit-ss2)
(defvar mew-cs-draft          'iso-2022-7bit-ss2)
(defvar mew-cs-scan           'ctext)
(defvar mew-cs-infile         'undecided) ;; guess in +draft
(defvar mew-cs-outfile        'iso-2022-7bit-ss2)
(defvar mew-cs-virtual        (if (mew-coding-system-p 'ctext-unix)
				  'ctext-unix 'ctext)) ;; ^M as it is
(defvar mew-cs-pick           'euc-jp)

(defvar mew-cs-database
  '(("us-ascii"    (ascii)                    nil        "7bit"
                                              nil        "Q")
    ("iso-8859-1"  (ascii latin-iso8859-1)    iso-8859-1 "quoted-printable"
                                              iso-8859-1 "Q")
    ("iso-8859-2"  (ascii latin-iso8859-2)    iso-8859-2 "quoted-printable"
                                              iso-8859-2 "Q")
    ("iso-8859-3"  (ascii latin-iso8859-3)    iso-8859-3 "quoted-printable"
                                              iso-8859-3 "Q")
    ("iso-8859-4"  (ascii latin-iso8859-4)    iso-8859-4 "quoted-printable"
                                              iso-8859-4 "Q")
    ("koi8-r"	   (ascii cyrillic-iso8859-5) koi8-r     "quoted-printable"
                                              koi8-r     "Q")
    ("iso-8859-6"  (ascii arabic-iso8859-6)   iso-8859-6 "base64"
                                              iso-8859-6 "B")
    ("iso-8859-7"  (ascii greek-iso8859-7)    iso-8859-7 "base64"
                                              iso-8859-7 "B")
    ("iso-8859-8"  (ascii hebrew-iso8859-8)   iso-8859-8 "base64"
                                              iso-8859-8 "B")
    ("iso-8859-9"  (ascii latin-iso8859-9)    iso-8859-9 "quoted-printable"
                                              iso-8859-9 "Q")
    ("tis-620"     (ascii thai-tis620)        tis620     "base64"
                                              tis620     "B")
    ("iso-2022-jp"
     (ascii latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978)
                                              iso-2022-jp "7bit"
                                              iso-2022-jp "B")
    ("euc-kr" (ascii korean-ksc5601)          euc-kr      "base64"
                                              euc-kr      "B")
    ("iso-2022-kr" (ascii korean-ksc5601)     iso-2022-kr "7bit"
                                              euc-kr      "B")
    ("gb2312" (ascii chinese-gb2312)          cn-gb-2312  "base64"
                                              cn-gb-2312  "B")
    ("hz-gb-2312" (ascii chinese-gb2312)      hz-gb-2312  "7bit"
                                              hz-gb-2312  "B")
    ("big5" (ascii chinese-big5-1 chinese-big5-2)
                                              chinese-big5 "base64"
                                              chinese-big5 "B")
    ("iso-2022-jp-2" 
     (ascii
      latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978 japanese-jisx0212
      chinese-gb2312 korean-ksc5601 latin-iso8859-1 greek-iso8859-7)
                                              iso-2022-7bit-ss2 "7bit"
                                              iso-2022-7bit-ss2 "B")
    ;; charset-to-cs purpose only
    ("euc-jp"          nil euc-japan)
    ("shift_jis"       nil shift_jis)
    ("cn-gb"           nil cn-gb-2312)   ;; the same as gb2312 above
    ("cn-big5"         nil chinese-big5) ;; the same as big5 above
    ("iso-2022-cn"     nil iso-2022-cn)
    ("iso-2022-cn-ext" nil iso-2022-cn-ext)
    ("iso-2022-int-1"  nil iso-2022-int-1)
    ))

;;
;; Leading characters
;;

(defvar mew-lc-ascii 'ascii)
(defvar mew-lc-jp    'japanese-jisx0208)
(defvar mew-lc-kana  'katakana-jisx0201)
(defalias 'mew-make-char 'make-char)
(defalias 'mew-char-charset 'char-charset)

;;
;; CS
;;

(defalias 'mew-find-cs-region 'find-charset-region)

;; to internal
(defun mew-cs-decode-region (beg end cs)
  (if cs (decode-coding-region beg end cs)))

;; to extenal
(defun mew-cs-encode-region (beg end cs)
  (if cs (encode-coding-region beg end cs)))

;; to internal
(defun mew-cs-decode-string (str cs)
  (if cs (decode-coding-string str cs) str))

;; to external
(defun mew-cs-encode-string (str cs)
  (if cs (encode-coding-string str cs) str))

;;
;; Process environment
;;

(defun mew-set-process-cs (process read write)
  (set-process-coding-system process read write))

(defmacro mew-plet (&rest body)
  `(let ((call-process-hook nil)
	 (coding-system-for-read  'binary)
	 (coding-system-for-write 'binary))
     ,@body))

(defmacro mew-piolet (read write &rest body)
  `(let ((call-process-hook nil)
	 (coding-system-for-read  ,read)
	 (coding-system-for-write ,write))
     ,@body))

(defmacro mew-pioalet (read write arg &rest body)
  `(let ((call-process-hook nil)
	 (coding-system-for-read  ,read)
	 (coding-system-for-write ,write)
	 (file-name-coding-system ,arg))
     ,@body))

(defmacro mew-flet (&rest body)
  `(let ((coding-system-for-read  'binary)
	 (coding-system-for-write 'binary)
	  jam-zcat-filename-list
	  jka-compr-compression-info-list)
     ,@body))

(defmacro mew-frwlet (read write &rest body)
  `(let ((coding-system-for-read  ,read)
	 (coding-system-for-write ,write)
	  jam-zcat-filename-list
	  jka-compr-compression-info-list)
     ,@body))

;;
;; Post conversion
;;

(defun mew-cs-post-conv (cs) ()) ;; not necessary because internally applied

;;
;;
;;

(cond
 ((and mew-temacs-p (string< emacs-version "20.2.90"))
  (fset 'mew-aref (symbol-function 'sref))
  (fset 'mew-charlen (symbol-function 'char-bytes)))
 (t
  (fset 'mew-aref (symbol-function 'aref))
  (defmacro mew-charlen (c) (` 1)))
 )

;;
;;
;;

(require 'mew-mule)
(provide 'mew-mule3)

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

;;; mew-mule3.el ends here
