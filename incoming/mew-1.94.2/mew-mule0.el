;;; mew-mule0.el --- Environment of non-Mule for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-mule0-version "mew-mule0.el version 0.10")

;; In the context of Mew, 'charset' means MIME charset.
;; 'cs' means the internal representation of Emacs (was known as Mule).

;;
;; User CS definitions
;;

(defvar mew-cs-dummy          nil)
(defvar mew-cs-binary         nil)
(defvar mew-cs-text-for-read  nil)
(defvar mew-cs-text-for-write nil)
(defvar mew-cs-autoconv       nil)
(defvar mew-cs-7bit           nil)
(defvar mew-cs-7bit-crlf      nil)

(defvar mew-cs-mime-trans     nil)
(defvar mew-cs-rfc822-trans   nil)
(defvar mew-cs-draft          nil)
(defvar mew-cs-scan           nil)
(defvar mew-cs-infile         nil)
(defvar mew-cs-outfile        nil)
(defvar mew-cs-virtual        nil)
(defvar mew-cs-pick           nil)

(defvar mew-cs-database
  '(("us-ascii"      (0)   nil   "7bit"             nil "Q")
    ("iso-8859-1"    (0 1) dummy "quoted-printable" nil "Q")))

;;
;; Leading characters
;;

(defvar mew-lc-ascii 0)
(defvar mew-lc-kana  nil) ;; dummay

(defun mew-char-charset (char)
  ())

;;
;; CS
;;

(defun mew-find-cs-region (start end)
  (let (ret)
    (save-excursion
      (goto-char beg)
      (if (re-search-forward "[\000-\177]" end t)
	  (setq ret (cons 0 ret)))
      (goto-char beg)
      (if (re-search-forward "[\200-\377]" end t)
	  (setq ret (cons 1 ret))))
    ret))

;; to internal
(defmacro mew-cs-decode-region (beg end cs)
  (` ()))

;; to extenal
(defmacro mew-cs-encode-region (beg end cs)
  (` ()))

;; to internal
(defmacro mew-cs-decode-string (str cs)
  (` (, str)))

;; to external
(defmacro mew-cs-encode-string (str cs)
  (` (, str)))

;;
;; Process environment
;;

(defmacro mew-set-process-cs (pro from-pro to-pro)
  (` ()))

(defmacro mew-plet (&rest body)
  (` (let ((call-process-hook nil))
       (,@ body))))

(defmacro mew-piolet (input output &rest body)
  (` (let ((call-process-hook nil))
       (,@ body))))

(defmacro mew-pioalet (input output arg &rest body)
  (` (let ((call-process-hook nil))
       (,@ body))))

(defmacro mew-flet (&rest body)
  (` (let (jam-zcat-filename-list
	   jka-compr-compression-info-list)
       (,@ body))))

(defmacro mew-frwlet (read write &rest body)
  (` (let (jam-zcat-filename-list
	   jka-compr-compression-info-list)
       (,@ body))))

;;
;; Post conversion
;;

(defmacro mew-cs-post-conv (cs)
  (` ()))

;;
;;
;;

(fset 'mew-aref (symbol-function 'aref))
(defmacro mew-charlen (c) (` 1))

;;
;;
;;

(require 'mew-mule)
(provide 'mew-mule0)

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

;;; mew-mule0.el ends here
