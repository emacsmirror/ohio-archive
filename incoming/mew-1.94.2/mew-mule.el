;;; mew-mule.el --- Environment of Mule common for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jul 15, 1998
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-mule-version "mew-mule.el version 0.03")

;;
;; Charset
;;

(defun mew-charset-guess-string (str)
  (save-excursion
    (mew-set-buffer-tmp)
    (insert-string str)
    (mew-charset-guess-region (point-min) (point-max))))

(defun mew-charset-guess-region (beg end)
  "Guess minimum charset name."
  (interactive "r")
  (if (not mew-mule-p)
      (save-excursion
	(goto-char beg)
	(if (re-search-forward "[\200-\377]" end t)
	    "iso-8859-1"
	  mew-us-ascii))
    (let* ((tcsl (mew-find-cs-region beg end))
	   (N (length tcsl))
	   (alst mew-cs-database)
	   i a acsl aret csl ret)
      (while alst
	(setq a (car alst))
	(setq acsl (nth 1 a))
	(setq aret (nth 0 a))
	(setq alst (cdr alst))
	(catch 'loop
	  (setq i 0)
	  (while (< i N)
	    (if (member (nth i tcsl) acsl)
		()
	      (setq aret nil)
	      (setq acsl nil)
	      (throw 'loop nil))
	    (setq i (1+ i))))
	(if (null ret)
	    (setq ret aret)
	  (if (and acsl (< (length acsl) (length csl)))
	      (setq ret aret csl acsl))))
      (or ret mew-error-charset-unknown))))

(defun mew-charset-sanity-check (beg end)
  "Eliminate illegal characters"
  (interactive "r")
  (if (and (not (equal mew-mule-ver 0))
	   (member mew-lc-kana (mew-find-cs-region beg end)))
      (progn
	(require 'mew-lang-jp)
	(mew-zenkaku-katakana-region beg end)
	(message "Converted Hankaku Kana to Zenkaku Kana!!")
	(ding)
	(sit-for 1))))

(defvar mew-charset-list
  (mapcar (function (lambda (x) (nth 0 x))) mew-cs-database))

(defun mew-charset-to-data (charset)
  (if charset
      (mew-assoc-case-equal charset mew-cs-database 0)))

(defun mew-charset-to-cs (charset)
  (if charset
      (nth 2 (mew-assoc-case-equal charset mew-cs-database 0))))

(defun mew-charset-to-cte (charset)
  (if charset
      ;; used for body
      (nth 3 (mew-assoc-case-equal charset mew-cs-database 0))))

(provide 'mew-mule)

;;; Copyright Notice:

;; Copyright (C) 1998, 1999 Mew developing team.
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

;;; mew-mule.el ends here
