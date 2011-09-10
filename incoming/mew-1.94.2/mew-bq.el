;;; mew-bq.el --- Base64 and Quoted-Printable encoding for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 20, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-bq-version "mew-bq.el version 0.04")

(require 'mew)

(defvar mew-header-encode-switch
  (if (fboundp 'base64-encode-string)
      '(("B" . base64-encode-string)
	("Q" . mew-header-encode-qp))
    '(("B" . mew-header-encode-base64)
      ("Q" . mew-header-encode-qp))))

(defvar mew-header-decode-switch
  (if (fboundp 'base64-decode-string)
      '(("B" . base64-decode-string)
	("Q" . mew-header-decode-qp))
    '(("B" . mew-header-decode-base64)
      ("Q" . mew-header-decode-qp))))

(defconst mew-base64-char64
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defconst mew-base64-char256
  (let ((i 0) (len (length mew-base64-char64)) (s (make-string 256 0)))
    (while (< i len)
      (aset s (aref mew-base64-char64 i) i)
      (setq i (1+ i)))
    s))

(defconst mew-header-decode-regex 
  "=\\?\\([^? \t]+\\)\\?\\(.\\)\\?\\([^? \t]+\\)\\?=")

;;;
;;;
;;;

(defun mew-header-sanity-check-string (str)
  (if (null str)
      str
    (while (string-match "[\000-\010\012-\037\177]+" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(substring str (match-end 0)))))
    str))

(defun mew-header-sanity-check-region (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "[\000-\010\013-\037\177]+" nil t) ;; allow \n
      (replace-match "" nil t))))

(defun mew-header-encode (str)
  ;; sanity check should be done
  (let* ((charset (mew-charset-guess-string str))
	 (data (mew-charset-to-data charset))
	 (b-or-q (nth 5 data))
	 (cs (nth 4 data))
	 (fun (cdr (mew-assoc-case-equal b-or-q mew-header-encode-switch 0)))
	 (estr (mew-cs-encode-string str cs)))
    (concat "=?" charset "?" b-or-q "?" (funcall fun estr) "?=")))

(defun mew-header-decode (charset b-or-q estr)
  (let* ((fun (cdr (mew-assoc-case-equal b-or-q mew-header-decode-switch 0)))
	 (cs (mew-charset-to-cs charset))
	 ret)
    (cond
     ((and (null cs) (not (mew-case-equal charset mew-us-ascii)))
      mew-error-unknown-charset)
     (fun ;; cs may be nil
      (setq ret (mew-cs-decode-string
		 (or (funcall fun estr) mew-error-illegal-base64)
		 cs))
      (mew-header-sanity-check-string ret))
     (t
      estr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base64 encoding
;;;

(defun mew-header-encode-base64 (str256)
  (let* ((len (length str256))
	 (ret (make-string (* (/ (+ len 2) 3) 4) ?=))
	 (pad (% len 3))
	 (lim (- len pad))
	 (i -1) (j -1) c)
    (while (< (setq i (1+ i)) lim)
      (setq c (logior (lsh (aref str256 i) 16)
		      (lsh (aref str256 (setq i (1+ i))) 8)
		      (aref str256 (setq i (1+ i)))))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (lsh c -18)))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (logand (lsh c -12) 63)))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (logand (lsh c -6) 63)))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (logand c 63))))
    (cond
     ((= pad 1)
      (setq c (aref str256 i))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (lsh c -2)))
      (aset ret (1+ j)
	    (aref mew-base64-char64 (lsh (logand c 3) 4))))
     ((= pad 2)
      (setq c (logior (lsh (aref str256 i) 8)
		      (aref str256 (1+ i))))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (lsh c -10)))
      (aset ret (setq j (1+ j))
	    (aref mew-base64-char64 (logand (lsh c -4) 63)))
      (aset ret (1+ j)
	    (aref mew-base64-char64 (logand (lsh c 2) 63)))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base64 decoding
;;;

(defun mew-header-decode-base64 (str64)
  (let* ((len (length str64))
	 ret
	 (i 0) (j -1) (padlen 0) c)
    (if (string-match "=+$" str64)
	(setq padlen (- (match-end 0) (match-beginning 0))))
    (cond
     ((or (string-match "[^a-zA-Z0-9+/=]" str64)
	  (not (zerop (logand len 3)))
	  (< padlen 0)
	  (> padlen 2))
      nil)  ;; return value
     ((zerop (setq len (- len padlen))) "")
     (t
      (setq ret (make-string (/ (* len 3) 4) ?a))
      (while 
	  (progn 
	    (setq
	     c (logior
		(lsh (aref mew-base64-char256 (aref str64 i)) 18)
		(lsh (aref mew-base64-char256 (aref str64 (setq i (1+ i)))) 12)
		(lsh (aref mew-base64-char256 (aref str64 (setq i (1+ i)))) 6)
		(aref mew-base64-char256 (aref str64 (setq i (1+ i))))))
	    (aset ret (setq j (1+ j)) (lsh c -16))
	    (< (setq i (1+ i)) len))
	(aset ret (setq j (1+ j)) (logand (lsh c -8) 255))
	(aset ret (setq j (1+ j)) (logand c 255)))
      (if (< padlen 2)
	  (aset ret (1+ j) (logand (lsh c -8) 255)))
      (if (zerop padlen)
	  (aset ret (1+ (1+ j)) (logand c 255)))
      ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quoted-printable encoding
;;;

(defun mew-header-encode-qp (str)
  (let* ((len (length str))
	 (ret (make-string (* len 3) ?a))
	 (i 0) (j 0) char)
    (while (< i len)
      (setq char (aref str i))
      (cond
       ((char-equal char 32)
	(aset ret j ?_))
       ((and (> char 32)
	     (< char 126)
	     (not (char-equal char ?=))
	     (not (char-equal char ??))
	     (not (char-equal char ?_))) ;; space
	(aset ret j char))
       (t
	(aset ret j ?=)
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (lsh char -4)))
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (logand char 15)))))
      (setq i (1+ i) j (1+ j)))
    (substring ret 0 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quoted-printable decoding
;;;

(defmacro mew-hexchar-to-int (hex)
  (` (cond
      ((and (<= ?0 (, hex)) (<= (, hex) ?9))
       (- (, hex) ?0))
      ((and (<= ?A (, hex)) (<= (, hex) ?F))
       (+ (- (, hex) ?A) 10))
      ((and (<= ?a (, hex)) (<= (, hex) ?f))
       (+ (- (, hex) ?a) 10)))))

(defun mew-header-decode-qp (qpstr &optional key)
  (let* ((len (length qpstr))
	 (ret (make-string len ?a))
	 (i 0) (j 0) char)
    (setq key (or key ?=))
    (while (< i len)
      (setq char (aref qpstr i))
      (cond
       ((char-equal char ?_)
	(aset ret j 32))
       ((char-equal char key)
	(aset ret j (+ (* (mew-hexchar-to-int (aref qpstr (1+ i))) 16)
		       (mew-hexchar-to-int (aref qpstr (+ i 2)))))
	(setq i (+ i 2)))
       (t
	(aset ret j char)))
      (setq i (1+ i) j (1+ j)))
    (substring ret 0 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2047 encoding
;;

;; DRUMS says that each line should be less than or equal to 78
;; excluding CRLF.
;; RFC2047 says that encoded-word must be less than or equal to 75.
;; RFC2047 says that each line which includes one or more encoded-words
;; must be less than or equal to 76.

(defvar mew-encode-word-max-length 75)
(defvar mew-field-max-length 76)

;; If possible, mew-header-encode-string should expect the length of
;; results and split 'str' before encoding so that every 'encoded-word'
;; fits in 75 length. However, it is very difficult first because
;; it is very difficult to know actual length of 'str' after convention
;; from the internal representation to charset encoding. Second because
;; ISO-2022-JP can't be simply split. If split, extra escape sequences
;; appear. Moreover, it is not effective to expect the length of results
;; because 'str' is short enough in most cases. So, we measure the length
;; of results. If it is longer than 75, 'str' is split and 'substr1' and
;; 'substr2' are encoded.... Repeat this recursively but not so deeply.

(defun mew-header-encode-string (str &optional key-len)
  (let* ((max mew-encode-word-max-length)
	 (encoded-word (mew-header-encode str)))
    (if key-len
	(setq max (- max key-len)))
    (if (> (length encoded-word) max)
        (let ((med (/ (length str) 2))
              (i 0))
          (while (< i med)
            (setq i (+ i (mew-charlen (mew-aref str i)))))
          (append
           (mew-header-encode-string (substring str 0 i) key-len)
           (mew-header-encode-string (substring str i nil))))
      (list encoded-word))))

(defun mew-header-encode-split-string (str)
  "Split STR to need-to-encode string and non-encode-string."
  (let ((start 0) beg end ret)
    (while (string-match "\\(^\\|[ \t]+\\)[\t -~]+\\($\\|[ \t]+\\)" str start)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (if (equal start beg)
	  (setq ret (cons (substring str beg end) ret))
	(setq ret (cons (substring str beg end)
			(cons (substring str start beg) ret))))
      (setq start end))
    (if (/= start (length str))
	(setq ret (cons (substring str start nil) ret)))
    (nreverse ret)))

(defun mew-header-encode-comma-text (str)
  (let ((str-list (mapcar (function mew-chop) (mew-split str ?,))))
    (mew-header-encode-text (car str-list))
    (setq str-list (cdr str-list))
    (while str-list
      (insert ", ") ;; must be fold here
      (mew-header-encode-text (car str-list))
      (setq str-list (cdr str-list)))))

(defmacro mew-header-encode-cond (c)
  (` (cond
      ((> (, c) 127) ;; non-ascii
       (if (equal status 'space)
	   (progn
	     (insert (substring str bound i))
	     (setq bound i)))
       (setq status 'non-ascii))
      ;; end of non-ascii
      (t ;; ascii
       (cond
	((equal status 'space)
	 (insert (substring str bound i)) ;; spaces
	 (setq bound i)
	 (setq status 'ascii))
	((equal status 'ascii)
	 (setq status 'ascii))
	((equal status 'non-ascii)
	 (setq status 'non-ascii))
	((equal status 'non-ascii-space)
	 (mew-header-encode-text (substring str bound SBOUND))
	 ;; non-ascii
	 (insert (substring str SBOUND i)) ;; spaces
	 (setq bound i)
	 (setq status 'ascii))))
      ;; end of ascii
      )))

(defmacro mew-header-encode-cond2 (opt)
  (` (cond
      ((equal status 'ascii)
       (insert (substring str bound i)))
      ((equal status 'space)
       (insert (substring str bound i)))
      ((equal status 'non-ascii)
       (mew-header-encode-text (substring str bound i)) (, opt))
      ((equal status 'non-ascii-space)
       (mew-header-encode-text (substring str bound SBOUND) (, opt))
       (insert (substring str SBOUND i))))))

(defun mew-header-encode-addr (str)
  (let* ((len (length str))
	 (i 0) (bound 0) (status 'space)
	 SBOUND open c I)
    ;; status space, ascii, non-ascii, non-ascii-space
    ;; assumptions:
    ;;  <> doesn't contain non-ascii characters.
    ;;  () doesn't recurse.
    ;; if " " contains non-ascii, cause an error.
    (while (< i len)
      (setq c (mew-aref str i))
      (cond
       ;; quote
       ((char-equal c ?\")
	(setq I (1+ i))
	(setq open t)
	(catch 'quote
	  (while (< I len)
	    (setq c (mew-aref str I))
	    (cond
	     ((char-equal c ?\")
	      (setq open nil)
	      (throw 'quote nil))
	     ((> c 127)
	      (mew-draft-undo)
	      (error "Only ASCII is allowed in quoted-string in the header. ")))
	    (setq I (+ I (mew-charlen c)))))
	(if open
	    (progn
	      (mew-draft-undo)
	      (error "Quote string must be closed in the header. ")))
	(mew-header-encode-cond ?a)
	(setq i I))
       ;; end of quote
       ;; comment
       ((char-equal c ?\()
	(mew-header-encode-cond2 nil)
	(insert "(")
	(setq i (1+ i))
	(setq bound i)
	(setq status 'ascii)
	(setq open t)
	(let (qp)
	  (catch 'comment
	    (while (< i len)
	      (setq c (mew-aref str i))
	      (cond
	       ((char-equal c ?\))
		(setq open nil)
		(throw 'comment nil))
	       ((char-equal c ?\")
		(setq qp t))
	       ((> c 127)
		(setq status 'non-ascii)))
	      (setq i (+ i (mew-charlen c)))))
	  (if (and qp (equal status 'non-ascii))
	      (progn
		(mew-draft-undo)
		(error "Only ASCII is allowed in quoted-string in the header. "))))
	(if open
	    (progn
	      (mew-draft-undo)
	      (error "Comment must be closed in the header. ")))
	(mew-header-encode-cond2 'comment)
	(if (equal i len)
	    ()
	  (insert ")")
	  (setq bound (1+ i)))
	(setq status 'space))
       ;; end of ()
       ;; route
       ((char-equal c ?<)
	(mew-header-encode-cond2 nil)
	(if (or (char-equal (char-before (point)) 32)
		(char-equal (char-before (point)) ?\t))
	    (insert "<")
	  (insert " <"))
	(setq i (1+ i))
	(setq bound i)
	(setq status 'ascii)
	(setq open t)
	(catch 'route
	  (while (< i len)
	    (setq c (mew-aref str i))
	    (cond
	     ((char-equal c ?>)
	      (setq open nil)
	      (throw 'route nil))
	     ((> c 127)
	      (mew-draft-undo)
	      (error "<> must contain ASCII only. "))
	     (t
	      (insert c)))
	    (setq i (+ i (mew-charlen c)))))
	(if open
	    (progn
	      (mew-draft-undo)
	      (error "<> must be closed in the header. ")))
	(if (equal i len)
	    ()
	  (insert ">")
	  (setq bound (1+ i)))
	(setq status 'space))
       ;; end of <>
       ;; space
       ((or (char-equal c 32) (char-equal c ?\t))
	(cond
	 ((or (equal status 'ascii) (equal status 'space))
	  (insert (substring str bound i)) ;; 'ascii
	  (setq bound i)
	  (setq status 'space))
	 ((equal status 'non-ascii)
	  (setq status 'non-ascii-space)
	  (setq SBOUND i))))
       ;; end of white space
       ;; comma
       ((char-equal c ?,)
	(mew-header-encode-cond2 nil)
	(insert ", ")
	(setq i (1+ i))
	(catch 'comma
	  (while (< i len)
	    (setq c (mew-aref str i))
	    (if (or (char-equal c 32) (char-equal c ?\t) (char-equal c ?\n))
		() ;; loop
	      (throw 'comma nil))
	    (setq i (1+ i))))
	;; get back to the end of white spaces
	(setq bound i)
	(setq c 32)
	(setq i (1- i))
	(setq status 'space))
       ;; end of comma
       ;; the others
       (t (mew-header-encode-cond c)))
      ;; end of outside cond
      (setq i (+ i (mew-charlen c))))
    ;; end of while
    (mew-header-encode-cond2 nil)))

(defun mew-header-encode-text (str &optional comment key-len)
  ;; 'comment' means that we are in RFC822 comment "(...)".
  (let ((str-list (mew-header-encode-split-string str))
	head-is-e e-list)
    (if (string-match "^[\t -~]+$" (car str-list))
	(progn
	  ;; ascii
	  (insert (car str-list))
	  (setq str-list (cdr str-list)))
      (setq head-is-e t))
    (while str-list
      ;; encoded-words
      (if (and key-len head-is-e)
	  (progn
	    (setq e-list (mew-header-encode-string (car str-list) key-len))
	    (setq head-is-e nil))
	(setq e-list (mew-header-encode-string (car str-list))))
      (insert (mapconcat (function identity) e-list " "))
      ;; ascii
      (setq str-list (cdr str-list))
      (if (car str-list)
	  (progn
	    (insert (car str-list))
	    (setq str-list (cdr str-list)))))))

(defun mew-header-fold-region (beg end med &optional use-tab)
  (let ((limit1 med) limit2)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (not (eobp))
	(while (> (- (setq limit2 (save-excursion (end-of-line) (point)))
		     (point))
		  mew-field-max-length)
	  (forward-char mew-field-max-length)
	  (if (re-search-backward "[ \t]" limit1 t)
	      (progn
		(insert "\n")
		(if use-tab
		    (progn
		      (delete-char 1)
		      (insert "\t"))))
	    ;; Ugh!
	    (if (re-search-forward "[ \t]" limit2 t) ;; hold on ayway
		(progn
		  (backward-char)
		  (insert "\n")
		  (if use-tab
		      (progn
			(delete-char 1)
			(insert "\t"))))
	      (forward-line))) ;; give up this line
	  (setq limit1 (1+ (point))))
	(forward-line)
	(setq limit1 (1+ (point)))))))

(defun mew-header-encode-region (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (mew-header-sanity-check-region (point-min) (point-max))
    (mew-charset-sanity-check (point-min) (point-max))
    (goto-char (point-min))
    (let (key med type str start last)
      (while (not (eobp))
	(setq start (point))
	(if (not (looking-at mew-keyval))
	    (forward-line)
	  (setq key (mew-match 1))
	  (setq med (match-end 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (setq last (1- (point)))
	  (if (= last med)
	      ()
	    (if (equal (list mew-lc-ascii) (mew-find-cs-region med last))
		() ;; no encoding
	      (setq str (buffer-substring med (1- (point)))) ;; excluding \n
	      (delete-region med (point))
	      (setq type (mew-field-type-for-encoding key))
	      (cond
	       ((equal type 'mailbox)
		(mew-header-encode-addr str))
	       ((equal type 'mime)
		(mew-header-encode-addr str))
	       ((equal type 'comma-text)
		(mew-header-encode-comma-text str))
	       ((equal type 'text)
		(mew-header-encode-text str nil (length key)))
	       ((equal type 'unstruct)
		(mew-header-encode-text str nil (length key))))
	      (insert "\n")) ;; previously deleted, so insert here
	    (mew-header-fold-region start (point) med)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2047 decoding
;;

(defun mew-header-unfold-region (type)
  (goto-char (point-min))
  (cond
   ((equal type 'struct)
    ;; If each line doesn't end with ",", unfold it.
    ;; In Page 5 of RFC822 says, "Unfolding is accomplished by
    ;; regarding CRLF immediately followed by a LWSP-char as
    ;; equivalent to the LWSP-char". However, it also says, 
    ;; "In structured field bodies, multiple linear space ASCII 
    ;; characters (namely HTABs and SPACEs) are treated as single 
    ;; spaces and may freely surround any symbol." So, remove
    ;; continuous white spaces.
    (while (re-search-forward ",[ \t]+\n" nil t)
      (replace-match ",\n" nil t))
    (goto-char (point-min))
    (while (re-search-forward "\\([^,]\\)[ \t]*\n[ \t]+" nil t)
      (replace-match (concat (mew-match 1) " ") nil t)))
   ((equal type 'text)
    ;; In Page 5 of RFC822 says, "Unfolding is accomplished by
    ;; regarding CRLF immediately followed by a LWSP-char as
    ;; equivalent to the LWSP-char".
    (while (re-search-forward "\n\\([ \t]\\)" nil t)
      (replace-match (mew-match 1) nil t)))
   (t ;; unstruct
    )))

(defun mew-header-decode-region (type rbeg rend &optional unfold)
  "RFC 2047 decoding. This is liberal on the one point from RFC 2047.
That is, each line may be more than 75. "
  (save-restriction
    (narrow-to-region rbeg rend)
    (goto-char (point-min))
    (if (and (not unfold) 
	     (not (re-search-forward mew-header-decode-regex nil t)))
	(mew-header-unfold-region type)
      (let ((endq-regex "\\(\\\\*\\)\"")
	    (next-regex (concat "[ \t]*" mew-header-decode-regex))
	    regex beg end cs-str esc )
	(if (or mew-decode-quoted (memq type '(text comma-text)))
	    ;; unstructured or allow decoding in quoted-string
	    (setq regex mew-header-decode-regex)
	  ;; structured or deny decoding in quoted-string
	  (setq regex (concat endq-regex "\\|" mew-header-decode-regex)))
	(mew-header-unfold-region type)
	;; In Page 10 of RFC 2047 says, "When displaying a particular 
	;; header field that contains multiple 'encoded-word's, any 
	;; 'linear-white-space' that separates a pair of adjacent 
	;; 'encoded-word's is ignored".
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  ;; encoded-word in quoted-string should not be decoded
	  ;; according to RFC 2047. However, if users wish
	  ;; (ie mew-decode-quoted is *non-nil*), decode it.
	  (goto-char (match-beginning 0))
	  ;; count escapes leads to double quote char
	  (if (not (eq (following-char) ?\\))
	      (setq esc 0)
	    (setq esc (- (match-end 1) (match-beginning 1))))
	  (forward-char esc)
	  ;; Now pointer is skipped leading escapes
	  (if (not (eq (following-char) ?\"))
	    (while (looking-at next-regex)
	      (setq beg (match-beginning 0)
		    end (match-end 0)
		    cs-str (mew-header-decode (mew-match 1)
					      (mew-match 2)
					      (mew-match 3)))
	      (delete-region beg end)
	      (insert cs-str))
	    ;; beginning of quoted string
	    (goto-char (match-end 0))
	    (if (= 0 (% esc 2))
	      ;; skip to end of quoted-string
		(while (and (re-search-forward endq-regex nil t)
			    (setq esc (- (match-end 1) (match-beginning 1)))
			    (= 1 (% esc 2)))
		  (goto-char (match-end 0))))
	    ))))
    (goto-char (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2231
;;

(defun mew-param-encode (str)
  (let* ((charset (mew-charset-guess-string str))
         (data (mew-charset-to-data charset))
         (cs (nth 4 data))
         (estr (mew-cs-encode-string str cs))
	 (len (length estr))
	 (ret (make-string (* len 3) ?a))
	 (i 0) (j 0) char)
    (while (< i len)
      (setq char (aref estr i))
      (if (or (and (<= ?0 char) (<= char ?9))
	      (and (<= ?a char) (<= char ?z))
	      (and (<= ?A char) (<= char ?Z)))
	  (aset ret j char)
	(aset ret j ?%)
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (lsh char -4)))
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (logand char 15))))
      (setq i (1+ i) j (1+ j)))
    (concat charset "''" (substring ret 0 j))))

(defun mew-param-decode (whole-value)
  (let* ((value-params (mew-addrstr-parse-value-list whole-value))
	 (value (car value-params))
	 (params (cdr value-params))
	 (max 0) num
	 ret ext ea ext-sort entry charset cs
	 paramname-value paramname paramvalue)
    (while params
      (setq paramname-value (mew-param-analyze (car params)))
      (setcar paramname-value (downcase (nth 0 paramname-value)))
      (if (equal (length paramname-value) 2)
	  (setq ret (cons paramname-value ret))
	(setq ext (cons paramname-value ext))
	(setq num (nth 2 paramname-value))
	(if (> num max) (setq max num)))
      (setq params (cdr params)))
    (if (null ext)
	(cons value (nreverse ret)) ;; fast return
      (setq max (1+ max))
      (while ext
	(setq ea (car ext))
	(setq paramname (nth 0 ea))
	(setq paramvalue (nth 1 ea))
	(setq num (nth 2 ea))
	(if (setq entry (assoc paramname ext-sort))
	    (progn
	      (aset (nth 1 entry) num paramvalue)
	      (if (not (equal num 0))
		  ()
		(setcar (nthcdr 2 entry) (nth 3 ea)) ;; charset
		(setcar (nthcdr 3 entry) (nth 4 ea)))) ;; lang
	  (setq entry (make-vector max nil))
	  (aset entry num paramvalue)
	  (setq ext-sort
		(cons (list paramname entry (nth 3 ea) (nth 4 ea))
		      ext-sort)))
	(setq ext (cdr ext)))
      (while ext-sort
	(setq ea (car ext-sort))
	(setq paramvalue nil)
	(setq paramname (nth 0 ea))
	(setq entry (nth 1 ea))
	(setq charset (nth 2 ea))
	(setq num 0)
	(catch 'concat-loop
	  (while (< num max)
	    (if (aref entry num)
		(setq paramvalue (concat paramvalue (aref entry num))) ;; xxx
	      (throw 'concat-loop nil))
	    (setq num (1+ num))))
	(if charset
	    (progn
	      (setq cs (mew-charset-to-cs charset))
	      (if (and (null cs) (not (mew-case-equal charset mew-us-ascii)))
		  (setq paramvalue mew-error-unknown-charset)
		(setq paramvalue (mew-cs-decode-string paramvalue cs)))))
	(setq ret (cons (list paramname paramvalue) ret))
	(setq ext-sort (cdr ext-sort)))
      (cons value (nreverse ret))))) ;; late return

(defun mew-param-analyze (param)
  "Return (paramname paramvalue) or (paramname paramvalue section charset lang)."
  (let* (name section asterisk value charset lang)
    (if (not (string-match
	      "^\\([^=*]+\\)\\(\\|\\*[0-9]+\\)\\(\\*?\\)=\\(.*\\)$" param))
	()
      (setq name (mew-match 1 param))
      (setq section (mew-match 2 param)) ;; *21
      (setq asterisk (string= (mew-match 3 param) "*"))
      (setq value (mew-match 4 param))
      (if (string= section "")
	  (if asterisk
	      (setq section 0)
	    (setq section nil))
	(setq section (string-to-int (substring section 1 nil))))
      (if (null asterisk)
	  ;; delete quote
	  (if (and (< 1 (length value))
		   (char-equal (aref value 0) 34)
		   (char-equal (aref value (1- (length value))) 34))
	      (setq value (substring value 1 -1)))
	(if (and (equal section 0)
		 (string-match "^\\([^']*\\)'\\([^']*\\)'\\(.*\\)$" value))
	    (progn
	      (setq charset (mew-match 1 value))
	      (setq lang (mew-match 2 value))
	      (if (string= lang "") (setq lang nil))
	      (setq value (mew-match 3 value))
	      (if (string= value "") (setq value nil))))
	(setq value (mew-header-decode-qp value ?%)))
      (if section
	  (list name value section charset lang)
	(list name value)))))

(provide 'mew-bq)

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

;;; mew-bq.el ends here
