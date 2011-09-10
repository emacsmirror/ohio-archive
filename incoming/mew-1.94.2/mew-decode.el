;;; mew-decode.el --- MIME syntax decoder for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-decode-version "mew-decode.el version 0.26")

(require 'mew)

(defvar mew-prog-mime-decode-switch
  (list (cons mew-b64 '("-b"))
	(cons mew-qp  '("-q"))
	(cons mew-xg  '("-g"))))

(defvar mew-prog-mime-decode-text-switch
  (list (cons mew-b64 '("-b" "-t"))
	(cons mew-qp  '("-q"))
	(cons mew-xg  '("-g" "-t"))))

(defvar mew-decode-multipart-encrypted-switch
  '(("application/pgp-encrypted" mew-pgp-decrypt mew-pgp-ver mew-prog-pgp)))

(defvar mew-decode-multipart-signed-switch
  '(("application/pgp-signature" mew-pgp-verify mew-pgp-ver mew-prog-pgp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MIME decoder
;;

(defmacro mew-decode-error (error-msg)
  (` (progn (setq mew-decode-error (, error-msg)) (error ""))))

(defmacro mew-decode-narrow-to-header (&rest body)
  (` (progn
       (if (re-search-forward mew-eoh nil t)
	   (beginning-of-line)
	 (goto-char (point-max))
	 (insert "\n"))
       (if (and (integerp mew-header-max-length)
		(> (count-lines (point-min) (point)) mew-header-max-length))
	   (mew-decode-error
	    (substitute-command-keys
	     "Too long header. To see the message, type '\\<mew-summary-mode-map>\\[mew-summary-display-command]'")))
       (save-restriction
	 (narrow-to-region (point-min) (point))
	 (goto-char (point-min))
	 (,@ body)))))

(defun mew-header-arrange (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (mew-elet
     (let (ch-beg ch-end vs-beg vs-end contents cbeg)
       (setq ch-beg (next-single-property-change (point-min) 'mew-noncontents))
       (if (null ch-beg)
	   ()
	 (setq vs-beg (next-single-property-change (point-min) 'mew-visible))
	 (if vs-beg
	     (setq vs-end (next-single-property-change vs-beg 'mew-visible))
	   (if mew-field-other-visible
	       (progn
		 (setq vs-beg (next-single-property-change (point-min)
							   'mew-others))
		 (if vs-beg 
		     (setq vs-end (next-single-property-change vs-beg
							       'mew-others))
		   (setq vs-beg ch-beg)
		   (setq vs-end ch-beg)))
	     (setq vs-beg ch-beg)
	     (setq vs-end ch-beg)))
	 (setq ch-end (point-max))
	 (if (null vs-end) (setq vs-end (point-max)))
	 (mew-decode-header-property-region ch-beg ch-end)
	 (setq contents (buffer-substring ch-beg ch-end))
	 (delete-region ch-beg ch-end)
	 (if (nth 1 (mew-assoc-match2 "Content-" mew-field-spec 0))
	     (progn
	       ;; visible
	       (goto-char vs-end)
	       (setq cbeg (point))
	       (insert contents)
	       (put-text-property cbeg (point) 'mew-visible t)) ;; used later
	   ;; invisible
	   (goto-char vs-beg)
	   (setq cbeg (point))
	   (insert contents)
	   (put-text-property cbeg (point) 'mew-visible nil)) ;; for XEmacs
	 (goto-char (point-max)))
       (mew-decode-syntax-insert-privacy)
       (goto-char (if (get-text-property (point-min) 'mew-visible)
		      (point-min)
                    (or (next-single-property-change (point-min) 'mew-visible)
			(point-max))))
       (recenter 0)
       (save-excursion (mew-highlight-x-face (point-min) (point-max))))))
  (mew-header-goto-end)
  (if (eobp)
      (mew-header-set "\n") ;; analyzed
    (mew-header-set nil))) ;; asis

(defun mew-decode-header-property-region (BEG END)
  ;; see also mew-highlight-header
  (if (and (or window-system mew-xemacs-p) mew-use-highlight-header)
      (mew-elet
       (let ((defkey (intern-soft "mew-highlight-header-face-key"))
	     (defval (intern-soft "mew-highlight-header-face-marginal"))
	     key beg med N-spec key-face val-face)
	 (save-restriction
	   (narrow-to-region BEG END)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (if (not (looking-at mew-keyval))
		 (forward-line)
	       (setq key (mew-match 1))
	       (setq beg (match-beginning 0))
	       (setq med (match-end 0))
	       (forward-line)
	       (mew-header-goto-next)
	       (setq N-spec (mew-assoc-match3 key mew-field-spec 0))
	       (setq key-face (or (nth 3 N-spec) defkey))
	       (setq val-face (or (nth 4 N-spec) defval))
	       (put-text-property beg med 'face key-face)
	       (put-text-property med (point) 'face val-face))))))))

(defun mew-decode-rfc822-header (&optional no-property)
  "A function to handle RFC822 header.
Called on the beginning of the header in the narrowed region.
 - Decode and highlight RFC822 fields excluding MIME fields.
 - Delete X-Mew: fields.
 - Arrange decoded-RFC822-fields, mew-mv:, MIME fields in order.
The cursor moves between mew-mv: and MIME fields.
Return the existence MIME-Version: and the value of Subject:."
  (setq no-property (or no-property
			(not (and (or window-system mew-xemacs-p)
				  mew-use-highlight-header))))
  (let* ((case-fold-search t)
	 (visibles (make-list (length mew-field-spec) nil))
         (defkey (intern-soft "mew-highlight-header-face-key"))
         (defval (intern-soft "mew-highlight-header-face-marginal"))
	 key beg med subj from contents others
	 key-face val-face N N-spec visiblep mimep)
    (mew-decode-narrow-to-header
     (while (not (eobp))
       (if (not (looking-at mew-keyval))
	   (forward-line)
	 (setq key (capitalize (mew-match 1)))
	 (setq beg (match-beginning 0))
	 (setq med (match-end 0))
	 (forward-line)
	 (mew-header-goto-next)
	 (setq N-spec (mew-assoc-match3 key mew-field-spec 0))
	 (setq N (nth 0 N-spec))
	 (setq visiblep (nth 2 N-spec))
	 (cond
	  ((mew-case-equal key mew-x-mew:)
	   ;; deleting X-Mew: on the RFC822 header
	   (delete-region beg (point)))
	  ((string-match "^Content-" key)
	   ;; Due to PGP/MIME, properties are not put here.
	   (setq contents (cons (buffer-substring beg (point)) contents))
	   (delete-region beg (point)))
	  (t
	   (mew-header-decode-region
	    (mew-field-type-for-decoding key) med (point))
	   (cond
	    ((mew-case-equal key mew-from:)
	     (setq from (mew-addrstr-parse-address
			 (buffer-substring med (1- (point))))))
	    ((mew-case-equal key mew-subj:)
	     (setq subj (buffer-substring med (1- (point)))))
	    ((mew-case-equal key mew-mv:)
	     ;; MIME-Version:
	     (setq mimep (string-match
			  mew-mv:-num
			  (mew-addrstr-parse-value
			   (buffer-substring med (point)))))))
	   (if no-property
	       ()
	     (setq key-face (or (nth 3 N-spec) defkey))
	     (setq val-face (or (nth 4 N-spec) defval))
	     (put-text-property beg med 'face key-face)
	     (put-text-property med (point) 'face val-face))
	   (cond
	    ((null N-spec) ;; others
	     (setq others (cons (buffer-substring beg (point)) others))
	     (delete-region beg (point)))
	    (visiblep
	     (setcar (nthcdr N visibles)
		     (concat (nth N visibles)
			     (buffer-substring beg (point))))
	     (delete-region beg (point)))
	    (t ;; invisible
	     ())))))))
    (put-text-property (point-min) (point) 'mew-invisible t)
    (put-text-property (point-min) (point) 'mew-noncontents t)
    (if mew-field-other-visible
        ()
      (setq beg (point))
      (mapcar (function insert) (nreverse others))
      (put-text-property beg (point) 'mew-others t)
      (put-text-property beg (point) 'mew-noncontents t))
    (setq beg (point))
    (mapcar (function (lambda (x) (and (stringp x) (insert x)))) visibles)
    ;; for recenter in Message mode
    (put-text-property beg (point) 'mew-visible t)
    (put-text-property beg (point) 'mew-noncontents t)
    (if mew-field-other-visible
        (progn
          (setq beg (point))
          (mapcar (function insert) (nreverse others))
          (put-text-property beg (point) 'mew-others t)
	  (put-text-property beg (point) 'mew-noncontents t)))
    ;; the beginning of the content header
    (save-excursion (mapcar (function insert) (nreverse contents)))
    ;; 'mew-contents doesn't work due to PGP/MIME
    (list mimep subj from)))

(defun mew-decode-mime-header (&optional dct)
  "A function to handle content header.
Called on the beginning of the content header in the narrowed region
Return a part syntax after moving the beginning of the content body."
  (let ((case-fold-search t)
	(vec (make-vector (length mew-mime-fields) nil))
	key med attr n act value syntax)
    (mew-decode-narrow-to-header
     (while (not (eobp))
       (if (not (looking-at mew-keyval))
	   (forward-line)
	 (setq key (capitalize (mew-match 1)))
	 (setq med (match-end 0))
	 (forward-line)
	 (mew-header-goto-next)
	 (setq attr (assoc key mew-mime-fields))
	 (if (not attr)
	     ()
	   (setq n (nth 1 attr))
	   (setq act (nth 2 attr))
	   (cond
	    ((eq act 'analyze)
	     (setq value (mew-param-decode
			  (buffer-substring med (1- (point))))))
	    ((eq act 'extract)
	     (setq value (mew-addrstr-parse-value
			  (buffer-substring med (1- (point))))))
	    ((eq act 'decode)
	     (if mew-decode-DECODE
		 (mew-header-decode-region 'text med (point) t))
	     ;; mew-header-decode-region goes to the max point in
	     ;; the narrowed region. So, this must be (point).
	     (setq value (buffer-substring med (1- (point))))))
	   (aset vec n value)))))
    (if (eobp)
	(insert "\n")
      (forward-line))
    ;; the beginning of the content body
    (setq syntax (vconcat (list 'single (point) nil nil) vec))
    (or (mew-syntax-get-ct syntax)
	(mew-syntax-set-ct syntax (or dct mew-type-txt)))
    syntax))

(defun mew-decode-mime-body (ctl cte &optional tocs)
  ;; ((point), (point-max)) (not point-min)
  ;; If tocs is t, don't decode fromcs, don't encode tocs, no post-conv
  (let* ((ct (mew-syntax-get-value ctl 'cap))
	 (textp (string-match "^Text/" ct))
	 (linebasep (or textp (mew-ct-linebasep ct)))
	 (switch (if linebasep
		     mew-prog-mime-decode-text-switch
		   mew-prog-mime-decode-switch))
	 (beg (point))
	 opt file charset post-conv fromcs)
    (if (or (null cte) (mew-member-case-equal cte mew-decode-composite-value))
	()
      (cond
       ((and (mew-case-equal cte mew-b64)
	     (fboundp 'base64-decode-region))
	(base64-decode-region beg (point-max))
	(if linebasep
	    (progn
	      (goto-char beg)
	      (while (search-forward "\r\n" nil t) (replace-match "\n")))))
       ((mew-which mew-prog-mime-decode exec-path)
	(setq opt (cdr (mew-assoc-case-equal cte switch 0)))
	(if (null opt)
	    ;; Treated as Application/Octet-Stream.
	    ;; Never reach here when decoding.
	    (mew-decode-error (concat "Unknown CTE: " cte))
	  (setq file (mew-make-temp-name))
	  (mew-frwlet
	   mew-cs-dummy mew-cs-text-for-write
	   ;; NEVER use call-process-region for privacy reasons
	   (write-region beg (point-max) file nil 'no-msg))
	  (delete-region beg (point-max))
	  (mew-piolet
	   mew-cs-binary mew-cs-dummy
	   ;; mew-prog-mime-decode converts CRLF to LF, so
	   ;; read input as binary.
	   (apply (function call-process) mew-prog-mime-decode 
		  file t nil opt))
	  (if (file-exists-p file) (delete-file file))))
       (t
	(mew-decode-error (concat mew-prog-mime-decode " doesn't exist")))))
    ;; charset conversion
    (if (and textp
	     (not (eq tocs t))
	     (setq charset (mew-syntax-get-param ctl "charset")))
	(progn
	  (setq fromcs (mew-charset-to-cs charset))
	  (mew-cs-decode-region beg (point-max) fromcs)
	  (cond
	   (tocs
	    (mew-cs-encode-region beg (point-max) tocs))
	   ((setq post-conv (mew-cs-post-conv fromcs))
	    (let ((mc-flag t))
	      (funcall post-conv beg (point-max)))))))
    linebasep))

;;
;; Kick start function
;;

(defvar mew-decode-LIMIT  nil)
(defvar mew-decode-DECODE t)

(defun mew-decode (fld msg)
  ;; in cache buffer
  (mew-erase-buffer)
  (goto-char (point-min))
  (setq mew-decode-error nil)
  (setq mew-decode-not-decrypted nil)
  (mew-decode-syntax-clear)
  (mew-insert-message fld msg mew-cs-text-for-read nil)
  ;; afer reading the file
  (if mew-mule-p
      (cond
       ((boundp 'mc-flag)
	(setq mc-flag nil)) ;; for re-search-forward
       ((fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte t))))
  ;; Illegal messages may not have end-of-header.
  ;; Truncated messages may not have end-of-header.
  (if (re-search-forward mew-eoh nil t)
      ()
    (setq mew-decode-error "No end-of-header(null line) in the top level")
    (goto-char (point-max))
    (if (not (bolp)) (insert "\n"))
    (insert "\n"))
  (goto-char (point-min))
  (setq mew-decode-LIMIT nil)
  (setq mew-decode-DECODE t)
  (if mew-debug
      (let ((debug-on-error t))
	(setq mew-decode-syntax
	      (mew-decode-message (mew-decode-syntax-rfc822-head) 0))
	(mew-decode-syntax-set))
    (condition-case nil
	(progn
	  (setq mew-decode-syntax 
		(mew-decode-message
		 ;; Call internalform with VIRTUAL content header
		 ;;     CT: message/rfc822 (virtual)
		 ;; 
		 ;;     Header(RFC822 header + content header)
		 ;;
		 ;;     Body(content body)
		 (mew-decode-syntax-rfc822-head) 0))
	  (mew-decode-syntax-set))
      (error
       (widen)
       (mew-header-goto-body)
       ;; min, point - 1, point, point-max
       (setq mew-decode-syntax (mew-decode-syntax-rfc822))))))


;;
;; the function "m":: for message
;;

(defun mew-decode-message (syntax cnt)
  ;; Called on the beginning of the RFC822 header in the narrowed region
  ;; hbeg is certainly the beginning of the VIRTUAL content body(i.e. min).
  ;; hend will have to set to the end of PHYSICAL content header(i.e. end)
  ;; after analyzing the physical content header and body since CD:'s 
  ;; length in the physical content header will change(no need to say
  ;; about the end of the physical content header).
  ;;
  ;;     Content-Type: Message/Rfc822    == virtual content header
  ;;
  ;;(min)Decoded RFC822 fields           == virtual content body
  ;;     MIME-Version: 1.0
  ;;(cur)MIME fields                     == physical content header
  ;;(end)
  ;;     Content-Body                    == physical content body
  ;;(max)
  (let* (msf mimep subj mew-inherit-from part)
    (if (and mew-decode-LIMIT (>= cnt mew-decode-LIMIT))
	;; don't recurse anyway. don't decode the header
	(setq mimep nil)
      ;; even if mew-decode-DECODE is nil, we can and must
      ;; decode the header here.
      (setq msf (mew-decode-rfc822-header)) ;; on the physical
      (setq mimep (nth 0 msf))
      (setq subj (nth 1 msf))
      (setq mew-inherit-from (nth 2 msf)))
    (setq cnt (1+ cnt))
    ;; the beginning of the physical content header (cur)
    (cond 
     (mimep ;; MIME
      (save-restriction
	(narrow-to-region (point) (point-max))
	(setq part (mew-decode-singlepart cnt nil 'message))
	;; hend is always 1 char smaller than the beginning of 
	;; the physical content body
	(mew-syntax-set-key syntax 'message)
	(mew-syntax-set-end syntax (1- (mew-syntax-get-begin part)))
	(or (mew-syntax-get-cd syntax) (mew-syntax-set-cd syntax subj))
	(mew-syntax-cat syntax part))) ;; return value
     (t ;; RFC822
      ;; the beginning of the meaningless physical content header
      (if (re-search-forward mew-eoh nil t)
	  (forward-line)
	(mew-decode-error "No end-of-header(null line) in RFC822 message"))
      ;; the beginning of the BODY(i.e. the physical content body)
      (if mew-decode-DECODE
	  (mew-cs-decode-region (point) (point-max) mew-cs-rfc822-trans))
      (mew-syntax-set-key syntax 'message)
      (mew-syntax-set-end syntax (1- (point)))
      (or (mew-syntax-get-cd syntax) (mew-syntax-set-cd syntax subj))
      (mew-decode-syntax-rfc822 syntax)
      ;; (point-min), (point) - 1, (point), (point-max)
      ))))

;;
;; the function "S":: for singlepart
;;

(defun mew-decode-singlepart (cnt &optional dct parent)
  ;; Called on the beginning of the content header in the narrowed region
  (let* ((case-fold-search t) (begin (point))
	 (syntax (mew-decode-mime-header dct))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (cte (or (mew-syntax-get-cte syntax) mew-7bit))
	 (encap nil))
    ;; the beginning of the content body
    (cond
     ((not (mew-member-case-equal cte mew-decode-value))
      (mew-syntax-set-ct syntax mew-type-apo))
     ((string-match "^Message/" ct)
      (if (not (mew-member-case-equal cte mew-decode-composite-value))
	  (mew-syntax-set-ct syntax mew-type-apo)
	(cond
	 ((mew-case-equal mew-ct-msg ct)
	  (if (equal parent 'message) (setq encap t))
	  (save-restriction
	    (narrow-to-region (point) (point-max))
	    (setq syntax (mew-decode-message syntax cnt))))
	 ((mew-case-equal mew-ct-ext ct)
	  (let* ((at (mew-syntax-get-param ctl "access-type"))
		 (func (cdr (mew-assoc-case-equal at mew-ext-include-switch 0))))
	    (if (not (and func (fboundp func)))
		()
	      (save-excursion
		(goto-char (point-max)) ;; phantom body
		(funcall func ctl))
	      (delete-region begin (point))
	      (setq syntax (mew-decode-singlepart cnt)))))
	 ((mew-case-equal mew-ct-sts ct)
	  ;; do nothing
	  )
	 (t
	  ;; xxx how about message/partinal?
	  (mew-syntax-set-ct syntax mew-type-apo)
	  ))))
     ;; Multipart, decoding is not required
     ((string-match "^Multipart/" ct)
      (if (not (mew-member-case-equal cte mew-decode-composite-value))
	  (mew-syntax-set-ct syntax mew-type-apo)
	(cond
	 ((mew-case-equal mew-ct-mld ct)
	  ;; semantics into digest
	  (setq syntax (mew-decode-multipart syntax cnt mew-type-msg)))
	 ((mew-case-equal mew-ct-mls ct)
	  (setq syntax (mew-decode-multipart-signed syntax cnt)))
	 ((mew-case-equal mew-ct-mle ct)
	  (if (boundp 'mew-inherit-prefetching) ;; xxx
	      (signal 'quit "")
	    (setq syntax (mew-decode-multipart-encrypted syntax cnt))))
	 (t
	  (setq syntax (mew-decode-multipart syntax cnt nil))))))
     ;; Others
     (t
      (if (and (equal parent 'message) (not (mew-case-equal mew-ct-txt ct)))
	  (setq encap t))
      ;; even if cte is nil, call mew-decode-mime-body for charset conversion
      (if mew-decode-DECODE (mew-decode-mime-body ctl cte))))
    ;; ct may be changed to apo
    (if (not (mew-case-equal mew-ct-msg (car (mew-syntax-get-ct syntax))))
	(mew-syntax-set-end syntax (point-max)))
    (if encap
	;; Mew allows text/plain and multipart/* for body.
	;; If other CT: is embedded under message, it should be
	;; encapsulated in multipart/mixed.
	(let ((head mew-encode-syntax-multi-head))
	  ;; begin for multipart syntax is important because
	  ;; the begin will be used by the parent to set hend
	  (mew-syntax-set-begin head (mew-syntax-get-begin syntax))
	  (mew-syntax-set-end head (point-max))
	  (mew-syntax-cat head syntax)) ;; return value
      syntax))) ;; return value

;;
;; the function "M":: for multipart
;;

(defun mew-decode-multipart (syntax cnt &optional dct)
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (boundary (regexp-quote (mew-syntax-get-param ctl "boundary")))
	 (parts []) part
	 obound ebound bregex start break)
    (if (null boundary)
	(mew-decode-error "No boundary parameter for multipart"))
    (mew-syntax-set-key syntax 'multi)
    (setq obound (concat "--" boundary))
    (setq ebound (concat "--" boundary "--"))
    (setq bregex (concat "^--" boundary "\\(\\|--\\)$"))
    (if (not (re-search-forward (concat "^" obound "$") nil t))
	(mew-decode-error (format "No first boundary for %s" ct)))
    (forward-line)
    (setq start (point)) ;; the beginning of the part
    (catch 'multipart
      (while t
	(if (not (re-search-forward bregex nil t))
	    (mew-decode-error (format "No last boundary for %s" ct)))
	(setq break (string= (regexp-quote (mew-match 0)) ebound))
	(forward-line) ;; the beginning of the next part
	(save-excursion
	  (forward-line -1)
	  (beginning-of-line) ;; just in case
	  (forward-char -1) ;; skip the preceding CRLF
	  ;; the end of the part
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    ;; the beginning of the part
	    (setq part (mew-decode-singlepart cnt dct nil))
	    (setq parts (vconcat parts (vector part)))))
	(setq start (point)) ;; the beginning of the part
	(if break 
	    (throw 'multipart (vconcat syntax parts)))))))

;;
;; the function "D":: for decryption
;;

(defun mew-decode-multipart-encrypted (syntax cnt)
  ;; called in narrowed region
  ;;
  ;;     CT: M/E; proto; bound;
  ;;
  ;;(cur)--bound
  ;;             (the key part)
  ;;     --bound
  ;;             (the encrypted part)
  ;;     --bound--
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (boundary (regexp-quote (mew-syntax-get-param ctl "boundary")))
	 (switch mew-decode-multipart-encrypted-switch)
	 file1 file2 file3 syntax1 syntax3 func unknown existp proto
	 start result file3result privacy
	 oregex eregex)
    (if (null boundary)
	(mew-decode-error "No boundary parameter for multipart"))
    (setq oregex (concat "^--" boundary "$"))
    (setq eregex (concat "^--" boundary "--$"))
    ;;
    (if (not (re-search-forward oregex nil t))
	(mew-decode-error "No first boundary for Multipart/Encrypted"))
    (forward-line) ;; the beginning of the key part
    (setq start (point))
    ;;
    (if (not (re-search-forward oregex nil t))
	(mew-decode-error "No second boundary for Multipart/Encrypted"))
    (beginning-of-line)
    (setq syntax1 (mew-decode-security-singlepart start (1- (point))))
    (setq proto (car (mew-syntax-get-ct syntax1)))
    (setq func (mew-decode-get-security-func proto switch))
    (setq existp (mew-decode-get-security-existence proto switch))
    (if func
	(if existp
	    (setq file1 (mew-save-decode-form syntax1)))
      (setq unknown t))
    (forward-line) ;; the beginning of the encrypted part
    (setq start (point)) 
    ;;
    (if (not (re-search-forward eregex nil t))
	(mew-decode-error "No third boundary for Multipart/Encrypted"))
    (beginning-of-line)
    (if (and func existp)
	(setq file2 (mew-save-decode-form
		     (mew-decode-security-singlepart start (1- (point))))))
    ;;
    (delete-region (point-min) (point-max))
    ;; 
    ;; Call protocol function
    (cond
     (unknown
      (setq result (concat "unknown protocol " proto)))
     ((not existp)
      (setq result (concat (mew-decode-get-security-prog proto switch)
			   " doesn't exist")))
     (t
      (setq file3result (funcall func file1 file2))
      (setq file3 (nth 0 file3result) result (nth 1 file3result))))
    ;;
    (if (and func existp (file-exists-p file3))
	(mew-flet 
	 (insert-file-contents file3)
	 (put-text-property (point-min) (point-max) 'mew-noncontents nil)
	 ;; because of RICH functionality of RFC1847... Gee dirty!
	 (mew-decode-crlf-magic))
      (insert "\n") ;; CT: text/plain; charset=us-ascii
      (insert "Multipart/Encrypted could not be decrypted.\n")
      (setq mew-decode-not-decrypted t))
    ;; Throw away garbage
    (and file1 (file-exists-p file1) (delete-file file1))
    (and file2 (file-exists-p file2) (delete-file file2))
    (and file3 (file-exists-p file3) (delete-file file3))
    ;; Analyze the decrypted part
    (goto-char (point-min))
    (setq syntax3 (mew-decode-singlepart cnt nil nil))
    (setq privacy (mew-syntax-get-privacy syntax3))
    (if privacy (setq result (concat result "\n\t")))
    (mew-syntax-set-privacy
     syntax3 (cons (list mew-ct-mle proto result) privacy))
    syntax3))

;;
;; the function "V":: for verification
;;

(defun mew-decode-multipart-signed (syntax cnt)
  ;; called in narrowed region
  ;;
  ;;     CT: M/S; proto; bound; micalg;
  ;;
  ;;(cur)--bound
  ;;             (the signed part)
  ;;     --bound
  ;;             (the key part)
  ;;     --bound--
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (boundary (regexp-quote (mew-syntax-get-param ctl "boundary")))
	 (switch mew-decode-multipart-signed-switch)
	 file1 file2 syntax2 syntax3 func unknown existp proto
	 end1 start2 result privacy
	 oregex eregex)
    (if (null boundary)
	(mew-decode-error "No boundary parameter for multipart"))
    (setq oregex (concat "^--" boundary "$"))
    (setq eregex (concat "^--" boundary "--$"))
    ;;
    (if (not (re-search-forward oregex nil t))
	(mew-decode-error "No first boundary for Multipart/Signed"))
    (forward-line)
    ;; the beginning of the signed part
    (delete-region (point-min) (point)) ;; deleting content-header
    (goto-char (point-min)) ;; just in case
    ;;
    (if (not (re-search-forward oregex nil t))
	(mew-decode-error "No second boundary for Multipart/Signed"))
    (beginning-of-line) 
    (setq end1 (1- (point))) ;; the end of the signed part
    (forward-line) ;; the beginning of the key part
    (setq start2 (point)) 
    ;;
    (if (not (re-search-forward eregex nil t))
	(mew-decode-error "No third boundary for Multipart/Signed"))
    (beginning-of-line) ;; the end of the encrypted part + 1
    (setq syntax2 (mew-decode-security-singlepart start2 (1- (point))))
    (setq proto (car (mew-syntax-get-ct syntax2)))
    (setq func (mew-decode-get-security-func proto switch))
    (setq existp (mew-decode-get-security-existence proto switch))
    (if func
	(if existp
	    (progn
	      (setq file1 (mew-save-transfer-form (point-min) end1 'retain))
	      (setq file2 (mew-save-decode-form syntax2))))
      (setq unknown t))
    ;;
    (delete-region end1 (point-max))
    ;; Now the signed part only
    ;; Call protocl function
    (cond
     (unknown
      (setq result (concat "unknown protocol " proto)))
     ((not existp)
      (setq result (concat (mew-decode-get-security-prog proto switch)
			   " doesn't exist")))
     (t
      (setq result (funcall func file1 file2))))
    ;; Throw away garbage
    (and file1 (file-exists-p file1) (delete-file file1))
    (and file2 (file-exists-p file2) (delete-file file2))
    ;; Analyze the signed part
    (goto-char (point-min))
    (setq syntax3 (mew-decode-singlepart cnt nil nil))
    (setq privacy (mew-syntax-get-privacy syntax3))
    (if privacy (setq result (concat result "\n\t")))
    (mew-syntax-set-privacy
     syntax3 (cons (list mew-ct-mls proto result) privacy))
    syntax3))

(defmacro mew-decode-get-security-func (proto switch)
  (` (nth 1 (mew-assoc-case-equal (, proto) (, switch) 0))))

(defmacro mew-decode-get-security-existence (proto switch)
  (` (symbol-value (nth 2 (mew-assoc-case-equal (, proto) (, switch) 0)))))

(defmacro mew-decode-get-security-prog (proto switch)
  (` (symbol-value (nth 3 (mew-assoc-case-equal (, proto) (, switch) 0)))))

(defun mew-decode-security-singlepart (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (mew-decode-singlepart 0)))) ;; 0 is dummy

(defun mew-save-decode-form (syntax)
  (mew-flet
   (let ((file (mew-make-temp-name)))
     (write-region (mew-syntax-get-begin syntax)
		   (mew-syntax-get-end syntax)
		   file nil 'no-msg)
     file)))

(defun mew-decode-crlf-magic ()
  (let ((case-fold-search t)
	(cte mew-7bit)
	key start match)
    (save-excursion
      (goto-char (point-min))
      (catch 'header
	(while (re-search-forward 
		"^\r?$\\|^Content-Transfer-Encoding:[ \t]*" nil t)
	  (setq key (mew-match 0))
	  (setq start (match-end 0))
	  (if (string-match "^\r?$" key)
	      (progn
		(save-restriction
		  (if (string-match mew-bin cte)
		      (narrow-to-region (point-min) (1+ start))
		    (narrow-to-region (point-min) (point-max)))
		  (goto-char (point-min))
		  (while (search-forward "\r\n" nil t)
		    (replace-match "\n" nil t)))
		(throw 'header nil)))
	  (forward-line)
	  (mew-header-goto-next)
	  (setq match (mew-buffer-substring start (1- (point))))
	  (setq cte (mew-addrstr-parse-value match)))))))

(provide 'mew-decode)

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

;;; mew-decode.el ends here
