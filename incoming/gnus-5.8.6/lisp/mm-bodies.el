;;; mm-bodies.el --- Functions for decoding MIME things
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-and-compile
  (or (fboundp  'base64-decode-region)
      (require 'base64))
  (autoload 'binhex-decode-region "binhex"))

(require 'mm-util)
(require 'rfc2047)
(require 'qp)
(require 'uudecode)

;; 8bit treatment gets any char except: 0x32 - 0x7f, CR, LF, TAB, BEL,
;; BS, vertical TAB, form feed, and ^_
(defvar mm-7bit-chars "\x20-\x7f\r\n\t\x7\x8\xb\xc\x1f")

(defcustom mm-body-charset-encoding-alist
  '((iso-2022-jp . 7bit)
    (iso-2022-jp-2 . 7bit))
  "Alist of MIME charsets to encodings.
Valid encodings are `7bit', `8bit', `quoted-printable' and `base64'."
  :type '(repeat (cons (symbol :tag "charset")
		       (choice :tag "encoding"
			       (const 7bit)
			       (const 8bit)
			       (const quoted-printable)
			       (const base64))))
  :group 'mime)

(defun mm-encode-body ()
  "Encode a body.
Should be called narrowed to the body that is to be encoded.
If there is more than one non-ASCII MULE charset, then list of found
MULE charsets are returned.
If successful, the MIME charset is returned.
If no encoding was done, nil is returned."
  (if (not (featurep 'mule))
      ;; In the non-Mule case, we search for non-ASCII chars and
      ;; return the value of `mail-parse-charset' if any are found.
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "[^\x0-\x7f]" nil t)
	    (or mail-parse-charset
		(mm-read-charset "Charset used in the article: "))
	  ;; The logic in `mml-generate-mime-1' confirms that it's OK
	  ;; to return nil here.
	  nil))
    (save-excursion
      (goto-char (point-min))
      (let ((charsets (mm-find-mime-charset-region (point-min) (point-max)))
	    charset)
	(cond
	 ;; No encoding.
	 ((null charsets)
	  nil)
	 ;; Too many charsets.
	 ((> (length charsets) 1)
	  charsets)
	 ;; We encode.
	 (t
	  (let ((charset (car charsets))
		start)
	    (when (or t
		      ;; We always decode.
		      (not (mm-coding-system-equal
			    charset buffer-file-coding-system)))
	      (while (not (eobp))
		(if (eq (mm-charset-after) 'ascii)
		    (when start
		      (save-restriction
			(narrow-to-region start (point))
			(mm-encode-coding-region start (point) charset)
			(goto-char (point-max)))
		      (setq start nil))
		  (unless start
		    (setq start (point))))
		(forward-char 1))
	      (when start
		(mm-encode-coding-region start (point) charset)
		(setq start nil)))
	    charset)))))))

(defun mm-body-encoding (charset &optional encoding)
  "Do Content-Transfer-Encoding and return the encoding of the current buffer."
  (let ((bits (mm-body-7-or-8)))
    (cond
     ((eq bits '7bit)
      bits)
     ((and (not mm-use-ultra-safe-encoding)
	   (or (eq t (cdr message-posting-charset))
	       (memq charset (cdr message-posting-charset))
	       (eq charset mail-parse-charset)))
      bits)
     (t
      (let ((encoding (or encoding
			  (cdr (assq charset mm-body-charset-encoding-alist))
			  (mm-qp-or-base64))))
	(when mm-use-ultra-safe-encoding
	  (setq encoding (mm-safer-encoding encoding)))
	(mm-encode-content-transfer-encoding encoding "text/plain")
	encoding)))))

(defun mm-body-7-or-8 ()
  "Say whether the body is 7bit or 8bit."
  (cond
   ((not (featurep 'mule))
    (if (save-excursion
	  (goto-char (point-min))
	  (skip-chars-forward mm-7bit-chars)
	  (eobp))
	'7bit
      '8bit))
   (t
    ;; Mule version
    (if (and (null (delq 'ascii
			 (mm-find-charset-region (point-min) (point-max))))
	     ;;!!!The following is necessary because the function
	     ;;!!!above seems to return the wrong result under
	     ;;!!!Emacs 20.3.  Sometimes.
	     (save-excursion
	       (goto-char (point-min))
	       (skip-chars-forward mm-7bit-chars)
	       (eobp)))
	'7bit
      '8bit))))

;;;
;;; Functions for decoding
;;;

(defun mm-decode-content-transfer-encoding (encoding &optional type)
  (prog1
      (condition-case error
	  (cond
	   ((eq encoding 'quoted-printable)
	    (quoted-printable-decode-region (point-min) (point-max)))
	   ((eq encoding 'base64)
	    (base64-decode-region
	     (point-min)
	     ;; Some mailers insert whitespace
	     ;; junk at the end which
	     ;; base64-decode-region dislikes.
	     ;; Also remove possible junk which could
	     ;; have been added by mailing list software.
	     (save-excursion
	       (goto-char (point-min))
	       (while (re-search-forward "^[\t ]*\r?\n" nil t)
		 (delete-region (match-beginning 0) (match-end 0)))
	       (point-max))))
	   ((memq encoding '(7bit 8bit binary))
	    ;; Do nothing.
	    )
	   ((null encoding)
	    ;; Do nothing.
	    )
	   ((memq encoding '(x-uuencode x-uue))
	    (funcall mm-uu-decode-function (point-min) (point-max)))
	   ((eq encoding 'x-binhex)
	    (funcall mm-uu-binhex-decode-function (point-min) (point-max)))
	   ((functionp encoding)
	    (funcall encoding (point-min) (point-max)))
	   (t
	    (message "Unknown encoding %s; defaulting to 8bit" encoding)))
	(error
	 (message "Error while decoding: %s" error)
	 nil))
    (when (and
	   (memq encoding '(base64 x-uuencode x-uue x-binhex))
	   (equal type "text/plain"))
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n" t t)))))

(defun mm-decode-body (charset &optional encoding type)
  "Decode the current article that has been encoded with ENCODING.
The characters in CHARSET should then be decoded."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (if (or (not charset) 
	  (eq 'gnus-all mail-parse-ignored-charsets)
	  (memq 'gnus-all mail-parse-ignored-charsets)
	  (memq charset mail-parse-ignored-charsets))
      (setq charset mail-parse-charset))
  (save-excursion
    (when encoding
      (mm-decode-content-transfer-encoding encoding type))
    (when (featurep 'mule)
      (let ((mule-charset (mm-charset-to-coding-system charset)))
	(if (and (not mule-charset)
		 (listp mail-parse-ignored-charsets)
		 (memq 'gnus-unknown mail-parse-ignored-charsets))
	    (setq mule-charset 
		  (mm-charset-to-coding-system mail-parse-charset)))
	(when (and charset mule-charset
		   ;; buffer-file-coding-system
		   ;;Article buffer is nil coding system
		   ;;in XEmacs
		   (mm-multibyte-p)
		   (or (not (eq mule-charset 'ascii))
		       (setq mule-charset mail-parse-charset))
		   (not (eq mule-charset 'gnus-decoded)))
	  (mm-decode-coding-region (point-min) (point-max) mule-charset))))))

(defun mm-decode-string (string charset)
  "Decode STRING with CHARSET."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (if (or (not charset) 
	  (eq 'gnus-all mail-parse-ignored-charsets)
	  (memq 'gnus-all mail-parse-ignored-charsets)
	  (memq charset mail-parse-ignored-charsets))
      (setq charset mail-parse-charset))
  (or
   (when (featurep 'mule)
     (let ((mule-charset (mm-charset-to-coding-system charset)))
       (if (and (not mule-charset)
		(listp mail-parse-ignored-charsets)
		(memq 'gnus-unknown mail-parse-ignored-charsets))
	   (setq mule-charset 
		 (mm-charset-to-coding-system mail-parse-charset)))
       (when (and charset mule-charset
		  (mm-multibyte-p)
		  (or (not (eq mule-charset 'ascii))
		      (setq mule-charset mail-parse-charset)))
	 (mm-decode-coding-string string mule-charset))))
   string))

(provide 'mm-bodies)

;; mm-bodies.el ends here
