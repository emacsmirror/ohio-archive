;;; mew-header.el --- Mail header stuff for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-header-version "mew-header.el version 0.15")

(require 'mew)

(defvar mew-anonymous-recipients ":;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header functions
;;;

(defmacro mew-header-goto-next ()
  '(while (looking-at mew-lwsp)
     (forward-line)))

;; see also mew-header-end
(defmacro mew-header-goto-end ()
  '(progn
     (goto-char (point-min))
     (if (re-search-forward mew-eoh nil t)
	 (beginning-of-line)
       (goto-char (point-max))
       (if (eolp) (insert "\n")))))

(defmacro mew-header-goto-body ()
  '(progn
     (mew-header-goto-end)
     (forward-line)))

(defun mew-header-get-value (field)
  "currently, when no match, it returns nil."
  ;; maybe called in narrowed region.
  ;; we can't widen for citation.
  (let ((case-fold-search t)
	(regex (format "^%s[ \t]*" field))
	start match ret)
    (save-excursion
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(catch 'only-one
	  (while (re-search-forward regex nil t)
	    (setq start (match-end 0))
	    (forward-line)
	    (mew-header-goto-next)
	    (setq match (mew-buffer-substring start (1- (point))))
	    (if (string= "" match)
		()
	      (if ret
		  (setq ret (concat ret "," match))
		(setq ret match)
		(if (equal field mew-from:) (throw 'only-one nil))))))))
    ret))

(fset 'mew-header-existp (symbol-function 'mew-header-get-value))

(defun mew-make-field-regex (fields)
  (concat "^\\(" (mapconcat (function identity) fields "\\|") "\\)"))

(defun mew-header-delete-lines (fields)
  (if (null fields)
      ()
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex fields))
	  start)
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (setq start (match-beginning 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (delete-region start (point)))))))

(defun mew-header-replace-lines (fields prefix)
  (if (null fields)
      ()
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex fields)))
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (beginning-of-line)
	  (insert prefix)
	  (forward-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header insertion
;;;

(defun mew-draft-header-insert (field value)
  "No encoding. Just insert."
  (if (and value (stringp field))
      (insert field " " value "\n")))

(defmacro mew-draft-header-fill (field value)
  (` (if (not (mew-header-existp (, field)))
	 (mew-draft-header-insert (, field) (, value)))))

(defun mew-header-insert (key value)
  (if (and value (stringp key))
      (let ((beg (point)) params med par parname parval)
	(if (listp value)
	    (progn
	      (setq params (cdr value))
	      (setq value (car value))))
	(insert key)
  	(insert " ")
	(setq med (point))
	(if (string-match "^[\t -~]*$" value)
	    (insert value)
	  (mew-header-encode-text value nil (length key)))
	(while params
	  (setq par (car params))
	  (setq parname (nth 0 par))
	  (setq parval (nth 1 par))
	  (setq params (cdr params))
	  (insert ";")
	  (cond
	   ((string-match "^[-a-zA-Z0-9]+$" parval)
	    ) ;; do nothing
	   ((and (equal (mew-charset-guess-string parval) mew-us-ascii)
		 (not (string-match "\"" parval)))
	    (setq parval (concat "\"" parval "\"")))
	   (t
	    (setq parval (mew-param-encode parval))
	    (setq parname (concat parname "*"))))
	  (insert " " parname "=" parval))
	(insert "\n")
	(mew-header-fold-region beg (point) med))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level functions to parse fields
;;;

;;
;; The purpose of this function is to purse and extract value
;; of address or content header. text is not target!
;;
;; ADDRP is t if STR are addresses.
;; ADDRP is nil if STR is a value of content-*.
;;
;; 'Winnie (The) Pooh <"W. Pooh" (loves "honey") @ (somewhere in) England>'
;; ->
;; '"W. Pooh"@England'
;;
;; For 'destination', SEP is ",".
;; For Content-*, SEP is ";".
;;
;; The special (i.e. SEP) is one of delimiters.
;; 'linear-white-space' is another delimiter.
;; 'quoted-string', 'comment', and 'domain-literal' is self-delimiting.
;; But 'domain-literal' (e.g. [1.2.3.4]) is meaningless.
;; So, the delimiters consists of SEP, SP, TAB, CRLF, "(", ")", and <">.
;; 
;; 'qtext' remains. (It will be removed by mew-param-decode.)
;; ## how about 'quoted-pair'?
;;
;; 'comment' must be symmetric. That is, it must have the exact number of
;; ")" against that of ")". The entire 'comment', even if nested, is 
;; ignored.
;;
;; 'encoded-word' is meaningless.
;; (1) Remove 'comment' so 'encoded-word' in 'comment' is meaningless.
;; (2) 'phrase' of 'mailbox' will be ignored because 'addr-spec' in 
;; 'route-addr' will be extracted. So, 'encoded-word' in 'phrase' is
;; meaningless.

(defmacro mew-addrstr-parse-syntax-list-check-depth ()
  '(progn
     (setq sep-cnt (1+ sep-cnt))
     (if (and (integerp mew-header-max-depth)
	      (>= sep-cnt mew-header-max-depth))
	 (progn
	   (message "Too many values. Truncate values over mew-header-max-depth.")
	   (ding)
	   (sit-for 2)
	   (throw 'max nil)))))

(defun mew-addrstr-parse-syntax-list (str sep addrp)
  (let* ((i 0) (len (length str))
	 (par-cnt 0) (tmp-cnt 0) (sep-cnt 0)
	 (tmp (make-string len ?x))
	 c ret)
    (catch 'max
      (while (< i len)
	(setq c (aref str i))
	(cond
	 ((char-equal c ?\")
	  (aset tmp tmp-cnt c)
	  (setq tmp-cnt (1+ tmp-cnt))
	  (setq i (1+ i))
	  (catch 'quote
	    (while (< i len)
	      (setq c (aref str i))
	      (cond
	       ((char-equal c ?\")
		(aset tmp tmp-cnt c)
		(setq tmp-cnt (1+ tmp-cnt))
		(throw 'quote nil))
	       ((char-equal c ?\n)
		(setq i (1+ i))
		(catch 'fold-quote
		  (while (< i len)
		    (setq c (aref str i))
		    (cond
		     ((or (char-equal c ?\t) (char-equal c 32))
		      (setq i (1+ i)))
		     ((char-equal c ?\")
		      (aset tmp tmp-cnt c)
		      (setq tmp-cnt (1+ tmp-cnt))
		      (throw 'quote nil))
		     (t
		      (aset tmp tmp-cnt c)
		      (setq tmp-cnt (1+ tmp-cnt))
		      (throw 'fold-quote nil))))))
	       (t
		(aset tmp tmp-cnt c)
		(setq tmp-cnt (1+ tmp-cnt))))
	      (setq i (1+ i)))))
	 ((char-equal c ?\()
	  (setq par-cnt 1)
	  (setq i (1+ i))
	  (catch 'comment
	    (while (< i len)
	      (setq c (aref str i))
	      (cond
	       ((char-equal c ?\()
		(setq par-cnt (1+ par-cnt)))
	       ((char-equal c ?\))
		(setq par-cnt (1- par-cnt))
		(if (equal par-cnt 0) (throw 'comment nil))))
	      (setq i (1+ i)))))
	 ((char-equal c ?<)
	  (cond
	   (addrp
	    (let (rbeg rend)
	      (setq i (1+ i))
	      (setq rbeg i)
	      (while (and (< i len) (not (char-equal (aref str i) ?>)))
		(setq i (1+ i)))
	      (setq rend i);; note: to be used for substring, so not 1-.
	      ;; should not be nested but easy to implement...
	      (setq ret (cons (car (mew-addrstr-parse-syntax-list
				    (substring str rbeg rend) sep t))
			      ret)))
	    (while (and (< i len) (not (char-equal (aref str i) sep)))
	      (setq i (1+ i)))
	    (setq tmp-cnt 0)
	    (mew-addrstr-parse-syntax-list-check-depth))
	   (t
	    ;; just ignore
	    (while (and (< i len) (not (char-equal (aref str i) ?>)))
	      (setq i (1+ i))))))
	 ((char-equal c ?\n))
	 ((char-equal c ?\t))
	 ((char-equal c 32))
	 ((char-equal c sep)
	  (if (> tmp-cnt 0)
	      (setq ret (cons (substring tmp 0 tmp-cnt) ret)))
	  (setq tmp-cnt 0)
	  (mew-addrstr-parse-syntax-list-check-depth))
	 (t 
	  (aset tmp tmp-cnt c)
	  (setq tmp-cnt (1+ tmp-cnt)))
	 )
	(setq i (1+ i))))
    (if (> tmp-cnt 0)
	(setq ret (cons (substring tmp 0 tmp-cnt) ret)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; High-level functions to parse fields
;;;

(defmacro mew-addrstr-parse-value (value)
  (` (car (mew-addrstr-parse-value-list (, value)))))

(defmacro mew-addrstr-parse-value-list (value)
  (` (mew-addrstr-parse-syntax-list (, value) ?\; nil)))

(defmacro mew-addrstr-parse-value2 (value)
  (` (car (mew-addrstr-parse-value-list2 (, value)))))

(defmacro mew-addrstr-parse-value-list2 (value)
  (` (mew-addrstr-parse-syntax-list (, value) ?, nil)))

;;

(defmacro mew-addrstr-parse-address (address)
  (` (car (mew-addrstr-parse-address-list (, address)))))

(defmacro mew-addrstr-parse-address-list (address)
  (` (mew-addrstr-parse-syntax-list (, address) ?, t)))

(defmacro mew-header-parse-address (field)
  (` (car (mew-header-parse-address-list (list (, field))))))

(defmacro mew-header-parse-address-list (field-list)
  (` (mew-addrstr-parse-syntax-list
      (mapconcat (function mew-header-get-value) (, field-list) ",")
      ?, t)))

(defun mew-header-parse-address-list2 (field-list)
  "Collect addresses from FIELD-LIST. 
Remove anonymous addresses."
  (let ((vals (mew-addrstr-parse-syntax-list
	       (mapconcat (function mew-header-get-value) field-list ",")
	       ?, t))
	ret)
    (while vals
      (if (not (string-match mew-anonymous-recipients (car vals)))
	  (setq ret (cons (car vals) ret)))
      (setq vals (cdr vals)))
    (nreverse ret)))

;;

(defmacro mew-addrstr-extract-user-list (addr-list)
  (` (mapcar (function mew-addrstr-extract-user) (, addr-list))))

(defun mew-addrstr-extract-user (addr)
  "Extracts username from ADDR"
  (if (string-match "@.*:" addr) ;; xxx what's this?
      (setq addr (substring addr (match-end 0) nil))
    (setq addr (mew-replace-character addr 32 ?_))
    (setq addr (substring addr 0 (string-match "%" addr)))
    (setq addr (substring addr 0 (string-match "@" addr)))
    ;; just for refile:  "To: recipients:;" -> recipients
    (setq addr (substring addr 0 (string-match mew-anonymous-recipients addr)))
    ;; removing Notes domain
    (setq addr (substring addr 0 (string-match "/" addr)))))

;;

(defun mew-addrstr-append-domain (addr)
  (if mew-addrbook-append-domain-p
      (if (string-match "@" addr)
	  addr
	(concat addr "@" mew-mail-domain))
    addr))

(defun mew-addrstr-expand-alias (alias)
  (if (and mew-addrbook-unexpand-regex
	   (string-match mew-addrbook-unexpand-regex alias))
      (list alias) ;; not expand
    (let ((addrs (mew-alias-get alias)))
      (setq addrs (mapcar (function mew-chop) (mew-split addrs ?,)))
      (setq addrs (delete "" addrs))
      (mapcar (function mew-addrstr-append-domain) addrs))))

(defun mew-addrstr-canonicalize-address (addr-str)
  "Expand aliases for each addresses in ADDR-STR. 
Then prepend mew-mail-domain if a domain part doesn't exist."
  (let ((addrs (mapcar (function mew-chop) (mew-split addr-str ?,)))
	addr ret)
    (while addrs
      (setq addr (car addrs))
      (setq addrs (cdr addrs))
      (setq ret (nconc ret (mew-addrstr-expand-alias addr))))
    ret))

(provide 'mew-header)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-header.el ends here
