;;; mew-encode.el --- MIME syntax encoder for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-encode-version "mew-encode.el version 0.23")

(require 'mew)

(defvar mew-prog-mime-encode-switch
  (list (cons mew-b64 '("-b"))
	(cons mew-qp  '("-q"))
	(cons mew-xg  '("-g"))))

(defvar mew-prog-mime-encode-text-switch
  (list (cons mew-b64 '("-b" "-t"))
	(cons mew-qp  '("-q"))
	(cons mew-xg  '("-g" "-t"))))

(defvar mew-encode-multipart-encrypted-switch
  '(("application/pgp-encrypted" . mew-pgp-encrypt)))

(defvar mew-encode-multipart-signed-switch
  '(("application/pgp-signature" . mew-pgp-sign)))

;;;
;;; Making a header
;;;

(defun mew-draft-remove-illegal-null-lines ()
  (if (mew-header-end)
      (save-excursion
	(save-restriction
	  (goto-char (mew-header-end))
	  (if (not (bolp)) (insert "\n"))
	  (narrow-to-region (point-min) (1- (mew-header-end)))
	  (goto-char (point-min))
	  (mew-elet
	   (while (re-search-forward "^$" nil t)
	     (delete-char 1)))))))

(defun mew-draft-ask-subject ()
  (if (and mew-ask-subject
	   (not (mew-header-existp mew-subj:)))
      ;; value is null or Subject: doesn't exsit
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (concat "^\\(" mew-subj: "\\)[ \t]*$")
			       (1- (mew-header-end)) t)
	    (progn
	      (replace-match "\\1 ")
	      (insert (read-string (concat mew-subj: " "))))))))

(defun mew-draft-ask-newsgroups ()
  (if (and mew-ask-newsgroups
	   (mew-header-existp mew-newsgroups:))
      (if (y-or-n-p "Do you want to post to NetNews? ")
	  ()
	(mew-header-delete-lines (list mew-newsgroups:)))))

(defun mew-encode-canonicalize-address-region (BEG END fields)
  (let ((case-fold-search t)
	(regex (mew-make-field-regex fields))
	start vals val addrs addr ret insl ins prefix suffix)
    (save-excursion
      (save-restriction
	(narrow-to-region BEG END)
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (setq start (match-end 0))
	  (forward-line)
	  (while (looking-at mew-lwsp)
	    (delete-backward-char 1)
	    (forward-line))
	  (setq val (mew-buffer-substring start (1- (point))))
	  (delete-region start (1- (point)))
	  (backward-char 1)
	  ;;
	  (setq vals (mapcar (function mew-chop) (mew-split val ?, ?: ?\;)))
	  (while vals
	    (setq val (car vals))
	    (setq vals (cdr vals))
	    (setq ins nil addrs nil)
	    (cond
	     ((string-match "^\\([^:]+:\\)\\([^;]+\\);$" val)
	      (setq prefix (mew-match 1 val))
	      (setq addr (mew-match 2 val))
	      (setq addr (mapcar (function mew-chop) (mew-split addr ?,)))
	      (while addr
		(setq addrs
		      (nconc addrs (mew-addrstr-expand-alias (car addr))))
		(setq addr (cdr addr)))
	      (setq ins (mapconcat (function identity) addrs ","))
	      (setq ins (list (concat prefix ins ";"))))
	     ((and (setq addr (mew-addrstr-parse-address val))
		   (string-match (concat "\\(.*<\\)" (regexp-quote addr) "\\(>.*\\)")
				 val))
	      (setq prefix (mew-match 1 val))
	      (setq suffix (mew-match 2 val))
	      (setq addr (mew-addrstr-append-domain addr))
	      (setq addrs (list addr))
	      (setq ins (list (concat prefix addr suffix))))
	     (t
	      (setq addrs (mew-addrstr-expand-alias val))
	      (setq ins (copy-sequence addrs))))
	    (setq insl (nconc insl ins))
	    (setq ret (nconc ret addrs)))
	  (insert " " (mapconcat (function identity) insl ", "))
	  (setq insl nil)
	  (forward-line))))
    ret))

(defun mew-draft-make-header (&optional addsep)
  (if (mew-header-existp mew-mv:)
      ()
    (goto-char (mew-header-end))
    (mew-header-insert mew-mv: mew-mv:-num))
  (mew-header-encode-region (point-min) (mew-header-end))
  (if addsep ;; reedit
      (progn
	;; To:
	;; Content-*
	;; ---
	(mew-header-clear) ;; mew-header-p returns nil
	;; To:
	;; Content-*
	(insert "\n"))
    ;; To:
    ;; ----
    ;; Content-*
    (mew-header-clear) ;; mew-header-p returns nil
    ;; To:
    ;; Content-*
    )
  (mew-header-goto-end)
  (mew-highlight-header-region (point-min) (point))
  (set-window-start (get-buffer-window (current-buffer)) (point-min))
  (mew-draft-toolbar-update))

;;;
;;; Making a message
;;;

(defun mew-draft-make-message (&optional privacy)
  "Make a MIME message. Guess charsets, convert the directory structure 
to multipart, and so on."
  (interactive)
  (widen)
  (run-hooks 'mew-make-message-hook)
  (if (not (mew-header-p))
      (progn
	(ding)
	(message "Already made!"))
    (condition-case nil
	(let (multip type receivers)
	  (mew-draft-remove-illegal-null-lines)
	  (mew-draft-ask-subject)
	  (mew-draft-ask-newsgroups)
	  (if mew-config-insert-when-composed
	      (mew-draft-insert-config))
	  (goto-char (mew-header-end))
	  (forward-line) ;; necessary for PGP
	  (setq receivers (mew-encode-canonicalize-address-region
			   (point-min) (point) mew-destination:-list))
	  (message "Making a MIME letter ...")
	  (if (mew-header-existp mew-ct:) ;; re-editing multipart
	      (progn
		(mew-draft-make-backup 'single)
		(mew-draft-make-header 'addsep))
	    (if (not (mew-attach-p))
		(setq mew-encode-syntax (mew-encode-syntax-single "text-file"))
	      (mew-attach-clear)
	      (if (mew-attach-valid-p)
		  (setq multip t)
		(setq mew-encode-syntax (mew-encode-syntax-single "text-file"))
		))
	    (mew-draft-make-backup (not multip))
	    ;; save syntax before setting privacy
	    (if (or (mew-syntax-get-privacy mew-encode-syntax) ;; specified
		    (and (null privacy) mew-draft-privacy-error)) ;; bypass
		()
	      (cond
	       (privacy
		(setq type privacy))
	       (mew-draft-protect-privacy-type
		(setq type mew-draft-protect-privacy-type))
	       ((and mew-protect-privacy-encrypted mew-draft-encrypted-p)
		(setq type mew-protect-privacy-encrypted-type))
	       (mew-protect-privacy-always
		(setq type mew-protect-privacy-always-type)))
	      (mew-syntax-set-privacy
	       mew-encode-syntax
	       (nth 1 (assoc type mew-privacy-database)))
	      ;; receivers are ignored when signing
	      (mew-syntax-set-decrypters mew-encode-syntax receivers))
	    (let ((mew-inherit-signer (mew-get-my-address)))
	      (if multip
		  (mew-draft-make-multi)
		(mew-draft-make-single)))
	    (mew-draft-make-header))
	  (save-buffer)
	  (setq mew-encode-syntax nil) ;; for undo
	  (setq buffer-undo-list nil)
	  (message "Making a MIME letter ... done"))
      (mew-draft-undo)))) ;; may not work due to timing

;;;
;;; Making singlepart
;;;

(defun mew-draft-make-single ()
  (goto-char (mew-header-end)) ;; due to illegal null lines in the header
  (forward-line)
  (mew-encode-singlepart mew-encode-syntax nil nil t))

(defun mew-encode-mime-body (ctl cte file)
  ;; If file is 't', target is buffered.
  ;; text should be buffered
  ;; 	- specified charset is a rare case
  ;; 	- copy overhead may be small
  (let* ((ct (mew-syntax-get-value ctl 'cap))
         (textp (string-match "^Text/" ct))
	 (charset (if textp (mew-syntax-get-param ctl "charset")))
         (linebasep
          (or textp
              (mew-member-case-equal ct mew-mime-content-type-text-list)))
         (switch (if linebasep
                     mew-prog-mime-encode-text-switch
                   mew-prog-mime-encode-switch))
         (beg (point))
	 opt file1)
    (cond
     (textp
      (if (and (stringp file) (file-readable-p file))
	  (mew-frwlet
	   (or (mew-charset-to-cs charset) mew-cs-infile) mew-cs-dummy
	   (insert-file-contents file)))
      (mew-charset-sanity-check beg (point-max))
      (setq charset (or charset
			(mew-charset-guess-region beg (point-max))
			mew-us-ascii))
      (setq cte (or cte (mew-charset-to-cte charset) mew-b64))
      (cond
       ((mew-case-equal cte mew-7bit)) ;; stay with internal
       ((and (mew-case-equal cte mew-b64) (fboundp 'base64-encode-region))
	(mew-cs-encode-region beg (point-max) (mew-charset-to-cs charset))
	(goto-char beg)
	(while (search-forward "\n" nil t) (replace-match "\r\n"))
	(base64-encode-region beg (point-max))
	(goto-char (point-max))
	(insert "\n"))
       ((mew-which mew-prog-mime-decode exec-path)
        (setq opt (cdr (mew-assoc-case-equal cte switch 0)))
	(if (null opt)
	    (error (concat "Unknown CTE: " cte))
	  (setq file1 (mew-make-temp-name))
	  (mew-frwlet
	   mew-cs-dummy (mew-charset-to-cs charset)
	   (write-region beg (point-max) file1 nil 'no-msg))
	  (delete-region beg (point-max))
	  (mew-piolet
	   mew-cs-text-for-read mew-cs-dummy
           (apply (function call-process) mew-prog-mime-encode
		  file1 t nil opt))
	  (if (file-exists-p file1) (delete-file file1))))
       (t
	(error (concat mew-prog-mime-encode " doesn't exist")))))
     (t
      ;; non-text
      (cond
       ((null cte)
	(setq cte mew-7bit)
	(mew-frwlet
	 (if linebasep
	     (if (mew-case-equal ct mew-ct-msg)
		 mew-cs-rfc822-trans
	       mew-cs-text-for-read)
	   mew-cs-binary)
	 mew-cs-dummy
	 (insert-file-contents file)))
       ((and (mew-case-equal cte mew-b64) (fboundp 'base64-encode-region))
	(mew-frwlet
	 (if linebasep mew-cs-text-for-read mew-cs-binary)
	 mew-cs-dummy
	 (insert-file-contents file))
	(if linebasep
	    (progn
	      (goto-char beg)
	      (while (search-forward "\n" nil t) (replace-match "\r\n"))))
	(base64-encode-region beg (point-max))
	(goto-char (point-max))
	(insert "\n"))
       ((mew-which mew-prog-mime-decode exec-path)
        (setq opt (cdr (mew-assoc-case-equal cte switch 0)))
	(if (null opt)
	    (error (concat "Unknown CTE: " cte))
	  (mew-piolet
	   mew-cs-text-for-read mew-cs-dummy
           (apply (function call-process) mew-prog-mime-encode
		  file t nil opt))))
       (t
	(error (concat mew-prog-mime-encode " doesn't exist"))))))
    (list (if charset (list "charset" charset)) cte)))

(defun mew-encode-singlepart (syntax &optional path depth buffered)
  ;; path is nil if called make-single or security multipart
  ;; buffered is t if called make-single
  (let* ((file (expand-file-name (mew-syntax-get-file syntax) path))
	 (ctl (mew-syntax-get-ct syntax))
         (ct (mew-syntax-get-value ctl 'cap))
	 (cte (mew-syntax-get-cte syntax))
	 (cd (mew-syntax-get-cd syntax))
	 (cdpl (mew-syntax-get-cdp syntax))
	 (privacy (mew-syntax-get-privacy syntax))
	 (beg (point))
	 charset-cte charset bodybeg)
    (setq charset-cte (mew-encode-mime-body ctl cte (or buffered file)))
    (goto-char beg)
    (setq charset (nth 0 charset-cte))
    (setq cte (nth 1 charset-cte))
    (if charset
	(progn
	  (setq ctl (mew-syntax-get-params ctl))
	  (setq ctl (mew-delete (car charset) ctl))
	  (setq ctl (cons ct (cons charset ctl)))))
    (mew-header-insert mew-ct: ctl)
    (mew-header-insert mew-cte: cte)
    (and cd (mew-header-insert mew-cd: cd))
    (and cdpl (mew-header-insert mew-cdp: cdpl))
    (insert "\n")
    ;; header "\n" (cur) [text]
    (setq bodybeg (point))
    (goto-char (point-max))
    (if (and (equal ct mew-ct-msg) mew-field-delete-for-forwarding)
	(save-restriction
	  (narrow-to-region bodybeg (point-max))
	  (mew-header-delete-lines mew-field-delete-common)
	  (mew-header-delete-lines mew-field-delete-for-forwarding)))
    (if privacy 
	(mew-encode-security-multipart
	 beg privacy depth (mew-syntax-get-decrypters syntax)))
    (goto-char (point-max))))

;;;
;;; Making multipart
;;;

(defun mew-draft-make-multi ()
  ;; delete delimiter
  (goto-char (mew-header-end)) ;; due to illegal null lines in the header
  (forward-line)
  (let ((beg (point))
	(syntax mew-encode-syntax) 
	(path (mew-expand-folder mew-draft-mime-folder))
	buffered)
    ;; Just after the header
    (save-excursion
      ;; See if a cover page is empty or not
      (while (and (looking-at "^$") (not (eobp)))
	(forward-line))
      (if (not (eobp))
	  ;; The cover page exists.
	  (setq buffered t)
	;; The cover page doesn't exist.
	;; Remove the cover page entry from the syntax.
	(setq syntax (mew-syntax-remove-entry syntax '(1)))
	(delete-region beg (point-max))))
    (mew-encode-multipart syntax path 0 buffered)))

(defvar mew-default-boundary "--%s(%s_%s)--")

(defun mew-boundary-get (&optional string)
  (if (null string) (setq string "Next_Part"))
  (format mew-default-boundary
	  string
	  (mew-replace-character (current-time-string) 32 ?_)
	  (mew-random-string)))

(defun mew-encode-multipart (syntax path depth &optional buffered)
  (let* ((boundary
	  (mew-boundary-get ;; 0 is nil for Next_Part
	   (if (> depth 0) (format "BOUNDARY%s" (int-to-string depth)))))
	 (fullname (expand-file-name (mew-syntax-get-file syntax) path))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (cte (mew-syntax-get-cte syntax))
	 (cd (mew-syntax-get-cd syntax))
	 (privacy (mew-syntax-get-privacy syntax))
	 (len (length syntax))
	 (cnt mew-syntax-magic)
	 (beg (point)))
    (mew-header-insert mew-ct: (list ct (list "boundary" boundary)))
    (mew-header-insert mew-cte: (or cte mew-7bit))
    (and cd (mew-header-insert mew-cd: cd))
    (while (< cnt len)
      (insert (concat "\n--" boundary "\n"))
      (if (mew-syntax-multipart-p (aref syntax cnt))
	  (mew-encode-multipart (aref syntax cnt) fullname (1+ depth))
	(mew-encode-singlepart
	 (aref syntax cnt) fullname (1+ depth)
	 (if (equal cnt mew-syntax-magic) buffered nil)))
      (setq cnt (1+ cnt)))
    (insert (concat "\n--" boundary "--\n"))
    (if privacy 
	(mew-encode-security-multipart
	 beg privacy depth (mew-syntax-get-decrypters syntax)))
    (goto-char (point-max))))

;;;
;;; Privacy services
;;;

(defun mew-encode-security-multipart (beg privacy depth decrypters)
  (save-restriction
    (narrow-to-region beg (point-max))
    (let (protocol ct)
      (while privacy
	(goto-char (point-min)) 
	(setq ct (nth 0 (car privacy)))
	(setq protocol (nth 1 (car privacy)))
	(setq privacy (cdr privacy))
	(cond 
	 ((mew-case-equal mew-ct-mle ct)
	  (mew-encode-multipart-encrypted ct protocol depth decrypters))
	 ((mew-case-equal mew-ct-mls ct)
	  (mew-encode-multipart-signed ct protocol depth)))))))

(defun mew-security-multipart-boundary (depth)
   (if depth
       (mew-boundary-get (format "Security_Multipart%s" (int-to-string depth)))
     (mew-boundary-get "Security_Multipart")))

(defun mew-save-transfer-form (beg end retain)
  ;; called in the narrowed region
  (let ((sbeg beg) (send end) (draft-buf (current-buffer)) file)
    (if retain
	(progn
	  (mew-set-buffer-tmp)
	  ;; tmp buffer
	  (insert-buffer-substring draft-buf beg end)
	  (setq sbeg (point-min) send (point-max))))
    (if mew-cs-7bit-crlf
	()
      (goto-char sbeg) ;; just in case
      (while (search-forward "\n" nil t) (replace-match "\r\n" nil t))
      (setq send (point-max)))
    (setq file (mew-make-temp-name))
    (mew-frwlet
     mew-cs-dummy mew-cs-7bit-crlf
     (write-region sbeg send file nil 'no-msg))
    (if retain
	(mew-erase-buffer)
      (delete-region sbeg send))
    (set-buffer draft-buf)
    file)) ;; return value

(defun mew-encode-multipart-encrypted (ct protocol depth decrypters)
  ;; called in the narrowed region
  (let* ((boundary (mew-security-multipart-boundary depth))
	 (switch mew-encode-multipart-encrypted-switch) ;; save length
	 (func (cdr (mew-assoc-case-equal protocol switch 0)))
	file1 file2 file3 cte2 cte3 fc error)
    (setq decrypters (cons mew-inherit-signer decrypters))
    ;; Write the part converting line breaks.
    (setq file1 (mew-save-transfer-form (point-min) (point-max) nil))
    ;; The narrowed region stores nothing
    ;; Call the protocol function
    (condition-case nil
	(setq fc (funcall func file1 decrypters))
      (error
       (if (file-exists-p file1) (delete-file file1))
       (error "unknown error for %s. Check %s, anyway" 
	      mew-ct-mle mew-temp-dir)))
    (setq file2 (nth 0 fc) cte2 (nth 1 fc) file3 (nth 2 fc) cte3 (nth 3 fc))
    (setq error (nth 4 fc))
    (if error
	(progn
	  (if (file-exists-p file1) (delete-file file1))
	  (if (file-exists-p file2) (delete-file file2))
	  (if (file-exists-p file3) (delete-file file3))
	  (mew-draft-undo)
	  (setq mew-draft-privacy-error t)
	  (error error))
      ;; Create multipart content-header
      (mew-header-insert mew-ct: (list ct
				       (list "protocol" protocol)
				       (list "boundary" boundary)))
      (insert (format "\n--%s\n" boundary))
      ;; Insert control keys
      (mew-encode-singlepart 
       (mew-encode-syntax-single file2 (list protocol) cte2))
      (insert (format "\n--%s\n" boundary))
      ;; Insert encrpted body
      (mew-encode-singlepart 
       (mew-encode-syntax-single file3 mew-type-apo cte3))
      (insert (format "\n--%s--\n" boundary))
      ;; Throw away the garbage 
      (if (file-exists-p file1) (delete-file file1))
      (if (file-exists-p file2) (delete-file file2))
      (if (file-exists-p file3) (delete-file file3)))))

(defun mew-encode-multipart-signed (ct protocol depth)
  ;; called in the narrowed region
  (let* ((boundary (mew-security-multipart-boundary depth))
	 (switch mew-encode-multipart-signed-switch);; save length
	 (func (cdr (mew-assoc-case-equal protocol switch 0)))
	 file1 file2 micalg cte2 fmc error)
    (setq file1 (mew-save-transfer-form (point-min) (point-max) 'retain))
    ;; The narrowed region still the ORIGINAL part (i.e. line breaks are LF)
    ;; Call the protocol function
    (condition-case nil
	(setq fmc (funcall func file1))
      (error
       (if (file-exists-p file1) (delete-file file1))
       (error "unknown error for %s. Check %s, anyway" 
	      mew-ct-mls mew-temp-dir)))
    (setq file2 (nth 0 fmc) cte2 (nth 1 fmc) micalg (nth 2 fmc))
    (setq error (nth 3 fmc))
    (if error
	(progn
	  (if (file-exists-p file1) (delete-file file1))
	  (if (file-exists-p file2) (delete-file file2))
	  (mew-draft-undo)
	  (setq mew-draft-privacy-error t)
	  (error error))
      (goto-char (point-min))
      ;; Before the signed part
      ;; Create multipart content-header
      (mew-header-insert mew-ct: (list ct
				       (list "protocol" protocol)
				       (list "micalg" micalg)
				       (list "boundary" boundary)))
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      ;; After the sigend part
      (insert (format "\n--%s\n" boundary))
      (mew-encode-singlepart 
       (mew-encode-syntax-single file2 (list protocol) cte2))
      (insert (format "\n--%s--\n" boundary))
      ;; Throw away the garbage 
      (if (file-exists-p file1) (delete-file file1))
      (if (file-exists-p file2) (delete-file file2)))))

;;;
;;; backup and undo
;;;

(defun mew-draft-make-backup (&optional single)
  ;; back up the draft and its syntax
  (let* ((attachdir (mew-attachdir))
	 (backup-file (expand-file-name mew-draft-backup-file attachdir))
	 (syntax-file (expand-file-name mew-draft-syntax-file attachdir))
	 (syntax mew-encode-syntax)) ;; mew-encode-syntax is buffer local
    (if (not (file-exists-p attachdir)) (mew-make-directory attachdir)) ;;for single part
    (mew-frwlet
     mew-cs-dummy mew-cs-draft
     (write-region (point-min) (point-max) backup-file nil 'no-msg))
    (if (not single)
	(save-excursion
	  (mew-set-buffer-tmp)
	  (prin1 syntax (current-buffer)) ;; different buffer, so use syntax
	  (terpri (current-buffer))
	  (mew-frwlet
	   mew-cs-dummy mew-cs-draft
	   (write-region (point-min) (point-max) syntax-file nil 'no-msg))))))

(defun mew-draft-undo ()
  "Get back to the draft before making MIME message."
  (interactive)
  (mew-elet
   (let* ((attachdir (mew-attachdir))
	  (backup-file (expand-file-name mew-draft-backup-file attachdir))
	  (syntax-file (expand-file-name mew-draft-syntax-file attachdir))
	  (syntax nil))
     (if (not (file-exists-p backup-file))
	 (message "Can't undo")
       (mew-erase-buffer)
       (mew-frwlet
	mew-cs-draft mew-cs-dummy
	(insert-file-contents backup-file))
       (delete-file backup-file)
       ;;
       (mew-header-clear) ;; erase the old header separator
       (mew-header-prepared)
       (if (not (file-exists-p syntax-file))
	   () ;; single
	 (save-excursion
	   (mew-frwlet
	    mew-cs-draft mew-cs-dummy
	    (find-file-read-only syntax-file))
	   (goto-char (point-min))
	   (setq syntax (read (current-buffer)))
	   (kill-buffer (current-buffer)))
	 (setq mew-encode-syntax syntax) ;; buffer local
	 (mew-draft-prepare-attachments)
	 (delete-file syntax-file))
       (mew-draft-toolbar-update)
       (setq buffer-undo-list nil)))))

(provide 'mew-encode)

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

;;; mew-encode.el ends here
