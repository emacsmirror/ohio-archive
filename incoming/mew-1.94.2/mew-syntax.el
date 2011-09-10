;;; mew-syntax.el --- Internal syntax for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-syntax-version "mew-syntax.el version 0.18")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-encode-syntax
;;
;; <esgl> = [ 'single file (dcr) (<epri>) (CT:)   CTE: CD: nil (CDP:) ]
;; <emul> = [ 'multi  dir/ (dcr) (<epri>) ("mul") CTE: CD: nil (CDP:) 1*<eprt> ]
;; <eprt> = <esgl> | <emul>
;; <epri> = list of (mew-ct-mls mew-ct-pgs)
;;
;; mew-decode-syntax
;;
;; <dmsg> = [ 'message hbeg hend (<dpri>) ("msg") CTE: CD: CID: (CDP:) <dbdy> ]
;; <dsgl> = [ 'single   beg  end (<dpri>) (CT:)   CTE: CD: CID: (CDP:) ]
;; <dmul> = [ 'multi    beg  end (<dpri>) ("mul") CTE: CD: CID: (CDP:) 1*<dprt> ]
;; <dbdy> = <dmul> | Text/Plain <dsgl>
;; <dprt> = <dmsg> | <dsgl> | <dmul>
;; <dpri> = list of (mew-ct-mls mew-ct-pgs RESULT) in reverse order ;; for cons

;;
;;
;;

(defmacro mew-syntax-singlepart-p (syntax)
  (` (eq (aref (, syntax) 0) 'single)))

(defmacro mew-syntax-multipart-p (syntax)
  (` (eq (aref (, syntax) 0) 'multi)))

(defmacro mew-syntax-message-p (syntax)
  (` (eq (aref (, syntax) 0) 'message)))

;;
;;
;;

(defmacro mew-syntax-get-key (syntax)
  (` (aref (, syntax) 0)))

(defmacro mew-syntax-set-key (syntax key)
  (` (aset (, syntax) 0 (, key))))

(defmacro mew-syntax-get-begin (syntax)
  (` (aref (, syntax) 1)))

(defmacro mew-syntax-set-begin (syntax begin)
  (` (aset (, syntax) 1 (, begin))))

(defmacro mew-syntax-get-end (syntax)
  (` (aref (, syntax) 2)))

(defmacro mew-syntax-set-end (syntax end)
  (` (aset (, syntax) 2 (, end))))

(defmacro mew-syntax-get-privacy (syntax)
  (` (aref (, syntax) 3)))

(defmacro mew-syntax-set-privacy (syntax privacy)
  (` (aset (, syntax) 3 (, privacy))))

(defmacro mew-syntax-get-ct (syntax)
  (` (aref (, syntax) 4)))

(defmacro mew-syntax-set-ct (syntax ct)
  (` (aset (, syntax) 4 (, ct))))

(defmacro mew-syntax-get-cte (syntax)
  (` (aref (, syntax) 5)))

(defmacro mew-syntax-set-cte (syntax cte)
  (` (aset (, syntax) 5 (, cte))))

(defmacro mew-syntax-get-cd (syntax)
  (` (aref (, syntax) 6)))

(defmacro mew-syntax-set-cd (syntax cd)
  (` (aset (, syntax) 6 (, cd))))

(defmacro mew-syntax-get-cid (syntax)
  (` (aref (, syntax) 7)))

(defmacro mew-syntax-set-cid (syntax cid)
  (` (aset (, syntax) 7 (, cid))))

(defmacro mew-syntax-get-cdp (syntax)
  (` (aref (, syntax) 8)))

(defmacro mew-syntax-set-cdp (syntax cdp)
  (` (aset (, syntax) 8 (, cdp))))

(defmacro mew-syntax-get-part (syntax)
  (` (aref (, syntax) 9)))

(defmacro mew-syntax-set-part (syntax part)
  (` (aset (, syntax) 9 (, part))))

;; alias for draft syntax

(defmacro mew-syntax-get-file (syntax)
  (` (aref (, syntax) 1)))

(defmacro mew-syntax-set-file (syntax file)
  (` (aset (, syntax) 1 (, file))))

(defmacro mew-syntax-get-decrypters (syntax)
  (` (aref (, syntax) 2)))

(defmacro mew-syntax-set-decrypters (syntax decrypters)
  (` (aset (, syntax) 2 (, decrypters))))

;; for content parameters

(defun mew-syntax-get-value (ctl &optional capitalize)
  (if capitalize
      (capitalize (car ctl))
    (car ctl)))

(defmacro mew-syntax-get-params (ctl)
  (` (cdr (, ctl))))

;; ctl = (value (pname pvalue) (pname pvalue) ...)
;; ctl = ((pname pvalue) (pname pvalue) ...)
(defmacro mew-syntax-get-param (ctl member)
  (` (mew-header-sanity-check-string (nth 1 (assoc (, member) (, ctl))))))

;; need to setq
(defmacro mew-syntax-cat (syntax part)
  (` (vconcat (, syntax) (vector (, part)))))

(defun mew-syntax-cdp-format (file)
  (if file (list "attachment" (list "filename" file))))

;; Encryption

(defun mew-syntax-encrypted-p (syntax)
  (let ((plist (mew-syntax-get-privacy (mew-syntax-get-part syntax))))
    (catch 'loop
      (while plist
	(if (mew-case-equal (nth 0 (car plist)) mew-ct-mle)
	    (throw 'loop t))
	(setq plist (cdr plist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Entry functions
;;

(defun mew-syntax-get-entry (syntax nums)
  (cond
   ((null nums) syntax) ;; single & message match
   ((mew-syntax-message-p syntax)
    (let ((body (mew-syntax-get-part syntax)))
      (if (mew-syntax-multipart-p body)
	  (mew-syntax-get-entry (mew-syntax-get-part syntax) nums)
	;; nums sould be "1"
	body)))
   ((mew-syntax-multipart-p syntax)
    (if (null nums) syntax
      (mew-syntax-get-entry
       (aref syntax (+ mew-syntax-magic (1- (car nums)))) (cdr nums))))))

(defun mew-syntax-insert-entry (syntax nums entry)
  (let* ((root syntax)
	 (child entry)
	 grand parent
	 (nl (length nums))
	 (rev (reverse nums))
	 (n0 (nth 0 rev))
	 (n1 (nth 1 rev))
	 (ns (reverse (nthcdr 2 rev))))
    (cond
     ((= nl 1)
      (setq parent root)
      (mew-syntax-add-entry parent n0 child))
     (t
      (if (= nl 2)
	  (setq grand root)
	(setq grand (mew-syntax-get-entry root ns)))
      (setq parent (mew-syntax-get-entry grand (list n1)))
      (setq parent (mew-syntax-add-entry parent n0 child))
      (aset grand (+ mew-syntax-magic (1- n1)) parent)
      root))))

(defun mew-syntax-add-entry (syntax n entry)
  "Must not use in functions other than mew-syntax-insert-entry"
  (let* ((len (1+ (length syntax)))
	 (vec (make-vector len nil))
	 (cnt 0) (thr (+ mew-syntax-magic (1- n))))
    (while (< cnt thr)
      (aset vec cnt (aref syntax cnt))
      (setq cnt (1+ cnt)))
    (aset vec cnt entry)
    (setq cnt (1+ cnt))
    (while (< cnt len)
      (aset vec cnt (aref syntax (1- cnt)))
      (setq cnt (1+ cnt)))
    vec ;; return value
    ))

(defun mew-syntax-remove-entry (syntax nums)
  (let* ((root syntax)
	 grand parent
	 (nl (length nums))
	 (rev (reverse nums))
	 (n0 (nth 0 rev))
	 (n1 (nth 1 rev))
	 (ns (reverse (nthcdr 2 rev))))
    (cond
     ((= nl 1)
      (setq parent root)
      (mew-syntax-cut-entry parent n0))
     (t
      (if (= nl 2)
	  (setq grand root)
	(setq grand (mew-syntax-get-entry root ns)))
      (setq parent (mew-syntax-get-entry grand (list n1)))
      (setq parent (mew-syntax-cut-entry parent n0))
      (aset grand (+ mew-syntax-magic (1- n1)) parent)
      root))))

(defun mew-syntax-cut-entry (syntax n)
  "Must not use in functions other than mew-syntax-remove-entry"
  (let* ((len (1- (length syntax)))
	 (vec (make-vector len nil))
	 (cnt 0) (thr (+ mew-syntax-magic (1- n))))
    (while (< cnt thr)
      (aset vec cnt (aref syntax cnt))
      (setq cnt (1+ cnt)))
    (while (< cnt len)
      (aset vec cnt (aref syntax (1+ cnt)))
      (setq cnt (1+ cnt)))
    vec ;; return value
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 
;;

(defvar mew-syntax-number-text-regex "^.....\\([.0-9]+\\) +")
(defvar mew-syntax-number-icon-regex "<\\([0-9.]+\\)>")

(defun mew-summary-goto-part (msg part)
  (goto-char (point-min))
  (cond
   ((equal major-mode 'mew-virtual-mode)
    (if (re-search-forward (format "\r.*%s" msg) nil t)
	(re-search-forward (format "^.....%s" part) nil t)))
   ((equal major-mode 'mew-summary-mode)
    (if (re-search-forward (format "^ *%s" msg) nil t)
	(re-search-forward (format "^.....%s" part) nil t))))
  (beginning-of-line))

(defun mew-summary-goto-message ()
  (if (mew-in-decode-syntax-p)
      (progn
	(goto-char (mew-decode-syntax-begin))
	(forward-line -1))))

(defun mew-syntax-number ()
  (let ((event last-command-event)
	ret str)
    (if (and mew-icon-p
	     (mouse-event-p event)
	     (event-over-toolbar-p event)
	     (or (button-press-event-p event)
		 (button-release-event-p event)))
	(progn
	  (setq str (toolbar-button-help-string (event-toolbar-button event)))
	  ;; last-pressed-toolbar-button can't be used.
	  (if (string-match mew-syntax-number-icon-regex str)
	      (setq ret (mew-match 1 str)))))
    (or ret
	(if (or (mew-in-attach-p)
		(mew-in-decode-syntax-p))
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at mew-syntax-number-text-regex)
		  (mew-match 1)))))))

(defmacro mew-syntax-number-to-nums (strnum)
  (` (if (, strnum)
	 (mapcar (function string-to-int) (mew-split (, strnum) ?.))
       nil)))

(defmacro mew-syntax-nums ()
  '(mew-syntax-number-to-nums (mew-syntax-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; get number
;;;
    
(defun mew-summary-message-number ()
  (let ((event last-command-event)
	ret str)
    (if (and mew-icon-p	
	     ;; exclude press button 2 in summary buffer.
	     ;; exclude pulldown menu in Summary mode.
	     ;; exclude popup menu of multipart icon because
	     ;; the cursor has already moved.
	     (mouse-event-p event)
	     (event-over-toolbar-p event)
	     (or (button-press-event-p event)     ;; right button
		 (button-release-event-p event))) ;; left button
	(if last-pressed-toolbar-button
	    (progn
	      (setq str (toolbar-button-help-string 
			 last-pressed-toolbar-button))
	      (if (string-match "^\\([0-9]+\\) " str)
		  (setq ret (mew-match 1 str))))))
    (if ret
	ret
      (if (not (mew-in-decode-syntax-p))
	  (save-excursion
	    (beginning-of-line)
	    (cond 
	     ((equal major-mode 'mew-summary-mode)
	      (if (looking-at mew-summary-message-regex)
		  (mew-match 1)
		nil))
	     ((equal major-mode 'mew-virtual-mode)
	      (if (looking-at ".*\r \\([-+%=].*\\) \\(.*\\)$")
		  (mew-match 2)
		nil))
	     (t nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-encode-syntax
;;

(defun mew-encode-syntax-single (file &optional ctl cte cd cid cdp 
				      privacy decrypters)
  ;; cid is just for beauty
  ;; if cdp is *non-nil*, set cdp from file.
  (let* ((attr (mew-attr-by-file file))
	 (ct (mew-attr-get-ct attr)))
    (or cte (setq cte (mew-attr-get-cte attr)))
    (if (null ctl) (setq ctl (list ct)))
    (if (and cdp 
	     (not (mew-member-match ct mew-mime-content-type-ignore-cdp t)))
	(setq cdp file)
      (setq cdp nil))
    (setq cdp (mew-syntax-cdp-format cdp))
    (vconcat [single] (list file decrypters privacy ctl cte cd cid cdp))))

(defun mew-encode-syntax-multi (dir ct)
  (if (not (string-match (concat mew-path-separator "$") dir))
      (setq dir (file-name-as-directory dir)))
  (vconcat [multi] (list dir) [nil nil] (list ct) [nil nil nil nil]))

(defun mew-encode-syntax-initial (dir)
  (vconcat
   (mew-encode-syntax-multi dir mew-type-mlm)
   ;; ensure to guess charset ....
   (list (mew-encode-syntax-single mew-draft-coverpage (list mew-ct-txt)))))

(defun mew-encode-syntax-initial-multi (dir n)
  (let ((i 1) (ret))
    (while (<= i n)
      (setq ret (vconcat ret (list (mew-encode-syntax-single
				    (int-to-string i)))))
      (setq i (1+ i)))
    (vconcat (mew-encode-syntax-multi dir mew-type-mlm)
	     (list (mew-encode-syntax-single mew-draft-coverpage 
					     (list mew-ct-txt)))
	     ret)))

(defconst mew-encode-syntax-dot
  [nil "." nil nil ("") nil nil nil nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-decode-syntax
;;

(defun mew-decode-syntax-rfc822 (&optional msg-head)
  ;; msg-head may include CD:
  (if (null msg-head) (setq msg-head (mew-decode-syntax-rfc822-head t)))
  (vconcat msg-head (vector (mew-decode-syntax-text))))

(defun mew-decode-syntax-rfc822-head (&optional reg-hend)
  (vector 'message (point-min)
	  (and reg-hend
	       (save-excursion (forward-line -1) (beginning-of-line) (point)))
	  nil mew-type-msg nil nil nil nil))

(defun mew-decode-syntax-text ()
  (vector 'single (point) (point-max) nil mew-type-txt nil nil nil nil))

(defconst mew-encode-syntax-multi-head 
  (vector 'multi nil nil nil mew-type-mlm nil nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syntax printer
;;

(defun mew-encode-syntax-print (syntax)
  (interactive)
  (mew-elet
   (let ((end nil)
	 (nums (mew-syntax-nums)))
     (cond
      ((mew-attach-p)
       (goto-char (point-max))
       (re-search-backward (concat "^" mew-draft-attach-boundary-end "$") nil t)
       (setq end (point))
       (re-search-backward (concat "^" mew-draft-attach-boundary-begin "$") nil t)
       (forward-line)
       (delete-region (point) end)
       (setq mew-syntax-multi-form nil)
       (setq mew-syntax-icon-spec nil)
       (mew-syntax-multipart syntax nil nil 'mew-draft-button)
       (mapcar (function insert-and-inherit)
	       (nreverse mew-syntax-multi-form))
       (if mew-xemacs-p
	   (progn
	     (goto-char (mew-attach-begin))
	     (insert "X")))
       (put-text-property (mew-attach-begin) (point-max) 'read-only t)
       (mew-front-nonsticky (mew-attach-begin) (1+ (mew-attach-begin)))
       (mew-rear-sticky (1- (point-max)) (point-max))
       (if mew-xemacs-p
	   (progn
	     (goto-char (1- (mew-attach-begin)))
	     (delete-char 1)))
       (if mew-icon-p
	   (mew-syntax-print-icon-spec (nreverse mew-syntax-icon-spec)
				       mew-draft-toolbar))
       (mew-attach-goto-number 'here nums))))))

;;
;;
;;

(defun mew-decode-syntax-print (sumbuf syntax form spec)
  ;; message buffer
  (let ((part (mew-syntax-get-part syntax))
	(cbuf (current-buffer)))
    (if (not (mew-syntax-multipart-p part))
	()
      (set-buffer sumbuf)
      (forward-line)
      (mew-elet
       (let ((pos (point)))
	 (mew-decode-syntax-begin-set)
	 (mapcar (function insert-and-inherit) form)
	 (if (equal pos (point))
	     ;; Nothing was printed.
	     (mew-decode-syntax-remove)
	   (mew-decode-syntax-end-set)
	   (put-text-property (mew-decode-syntax-begin) (mew-decode-syntax-end)
			      'face 'default)))
       (if mew-icon-p
	   (mew-syntax-print-icon-spec spec mew-summary-toolbar))
       (mew-summary-goto-message)
       (set-buffer-modified-p nil))
      (set-buffer cbuf))))

;;
;;
;;

(defun mew-decode-syntax-clear ()
  (setq mew-syntax-multi-form nil)
  (mew-syntax-clear-icon-spec)
  (mew-decode-syntax-clear-privacy))

(defun mew-decode-syntax-set ()
  ;; cache buffer
  (let ((mc-flag t)
	(part (mew-syntax-get-part mew-decode-syntax)))
    (if (mew-syntax-multipart-p part)
	(progn
	  (mew-syntax-multipart part 'decoding nil 'mew-summary-button 'body)
	  (setq mew-syntax-multi-form (nreverse mew-syntax-multi-form))
	  (setq mew-syntax-icon-spec (nreverse mew-syntax-icon-spec)))
      (mew-decode-syntax-set-privacy part "body"))))

(defun mew-syntax-multipart (syntax dec part func &optional body)
  (let* ((ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
	 (cd (or (mew-syntax-get-cd syntax) ""))
	 (cnt mew-syntax-magic)
	 (num 1)
	 (len (length syntax))
	 strpart subsyntax)
    ;; multipart itself is displayed only when encoding.
    (if dec
	(mew-decode-syntax-set-privacy
	 syntax
	 (concat (if part (concat part " "))
		 (if body "body ")
		 "multi"))
      (mew-syntax-format syntax part dec)
      (mew-syntax-set-icon-spec part
				ct cd
				(mew-attr-get-icon (mew-attr-by-ct ct)) func))
    (while (< cnt len)
      (if part
	  (setq strpart (concat part "." (int-to-string num)))
	(setq strpart (int-to-string num)))
      (setq subsyntax (aref syntax cnt))
      (cond
       ((mew-syntax-multipart-p subsyntax)
	(mew-syntax-multipart subsyntax dec strpart func nil))
       ((mew-syntax-message-p subsyntax)
	(mew-syntax-message subsyntax dec strpart func))
       ((mew-syntax-singlepart-p subsyntax)
	(mew-syntax-singlepart subsyntax dec strpart func
			       (and body (equal cnt mew-syntax-magic)))))
      (setq cnt (1+ cnt))
      (setq num (1+ num)))
    (if dec 
	()
      (if part
	  (setq part (concat part "." (int-to-string num)))
	(setq part (int-to-string num)))
      (mew-syntax-format mew-encode-syntax-dot part dec)
      (mew-syntax-set-icon-spec part "Attach Here" cd mew-icon-blank func))))

(defun mew-syntax-singlepart (syntax dec part func first)
  ;; part is valid only when called by mew-syntax-multipart.
  (let ((ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
	(cd (or (mew-syntax-get-cd syntax) "")))
    ;; see also mew-mime-message/rfc822.
    (if (and dec
	     ;; the first singlepart in multipart under message if t
	     ;; the first singlepart under message if 'body
	     first
	     ;; CT: is text/plain but not attached file.
	     (mew-case-equal ct mew-ct-txt))
	() ;; skip displaying.
      ;; reach here only when called by mew-syntax-multipart.
      (mew-syntax-format syntax part dec)
      (mew-syntax-set-icon-spec part ct cd 
				(mew-attr-get-icon (mew-attr-by-ct ct)) func))
    (if dec (mew-decode-syntax-set-privacy
	     syntax
	     (if (equal first 'body)
		 (if part (concat part " body") "body")
	       part)))))

(defun mew-syntax-message (syntax dec part func)
  (let ((ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
	(cd (or (mew-syntax-get-cd syntax) ""))
	(subsyntax (mew-syntax-get-part syntax)))
    (mew-syntax-format syntax part dec)
    (if dec (mew-decode-syntax-set-privacy
	     syntax
	     (format "%s message" part)))
    (mew-syntax-set-icon-spec part ct cd 
			      (mew-attr-get-icon (mew-attr-by-ct ct)) func)
    (cond
     ((mew-syntax-multipart-p subsyntax)
      (mew-syntax-multipart subsyntax dec part func 'body))
     ((mew-syntax-message-p subsyntax)
      ) ;; never happens
     ((mew-syntax-singlepart-p subsyntax)
      ;; text/plain only
      (mew-syntax-singlepart subsyntax dec part func 'body)))))

;012345678901234567890123456789012345678901234567890123456789012345678901234567
;<4>snss<27-2                   >ss<24+2                    >ss<16            >

(defun mew-syntax-format (syntax number dec)
  (let* ((file (if (not dec) (mew-syntax-get-file syntax)))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (char (mew-syntax-get-param ctl "charset"))
	 (cte (mew-syntax-get-cte syntax)) ;; cte may be nil
	 (cd (mew-syntax-get-cd syntax))
	 (cdpl (mew-syntax-get-cdp syntax))
	 (filename (mew-syntax-get-param cdpl "filename"))
	 (decrypters-list (mew-syntax-get-decrypters syntax))
	 (decrypters (and (not dec) decrypters-list
			  (mew-join "," decrypters-list)))
	 (cd-or-dec cd)
	 (privacy (mew-syntax-get-privacy syntax))
	 (space " ") (SP 32)
	 (cnt "..") (lcnt (length cnt))
	 (LT (- (window-width) 2))
	 (ln (length number))
	 (lm 4)
	 (lt 27) (ltc (- lt lcnt))
	 (ld (* (/ (- LT lm lt) 5) 3)) (ldc (- ld lcnt))
	 (lf (- LT lm ln lt ld 8)) (lfc (- lf lcnt))
	 (AR "*") (lfc* (1- lfc)) (asterisk nil)
	 (case-fold-search t)
	 (marks (make-string lm SP))
	 (i 0) (N (length privacy))
	 ctm ctp)

    (run-hooks 'mew-syntax-format-hook)
    (if (string-match "Text/" ct)
	(if char
	    (setq ct (concat ct "(" char ")"))
	  (if dec
	      (setq ct (concat ct "(" mew-us-ascii ")"))
	    (setq ct (concat ct "(guess)")))))
    (if (null privacy)
	(if (null cte)
	    ()
	  (setq cte (downcase cte))
	  (cond
	   ((or (equal cte mew-7bit)
		(equal cte mew-8bit)
		(equal cte mew-bin))
	    ;; no mark
	    )
	   ((equal cte mew-b64) (aset marks 0 ?B))
	   ((equal cte mew-qp)  (aset marks 0 ?Q))
	   ((equal cte mew-xg)  (aset marks 0 ?G))
	   (t                   (aset marks 0 ?X))))
      
      (if dec (setq privacy (reverse privacy)))
      (while (< i N)
	(setq ctm (nth 0 (nth i privacy)))
	(setq ctp (nth 1 (nth i privacy)))
	(cond
	 ((string-match "pgp"  ctp) (aset marks (* i 2) ?P))
	 ((string-match "moss" ctp) (aset marks (* i 2) ?M)))
	(cond
	 ((string-match mew-ct-mle ctm) (aset marks (1+ (* i 2)) ?E))
	 ((string-match mew-ct-mls ctm) (aset marks (1+ (* i 2)) ?S)))
	(setq i (1+ i))))
    
    (if (< lm (length marks))
	(setq marks (substring marks 0 lm))
      (setq marks (concat marks (make-string (- lm (length marks)) SP))))
        
    (if (< lt (length ct))
	(setq ct (concat (substring ct 0 ltc) cnt))
      (setq ct (concat ct (make-string (- lt (length ct)) SP))))

    (if (and (not dec) decrypters) (setq cd-or-dec decrypters))
    (if (null cd-or-dec)
	(setq cd-or-dec (make-string ld SP))
      (if (< ld (mew-string-width cd-or-dec))
	  (setq cd-or-dec (concat (mew-substring cd-or-dec ldc) cnt))
	(setq cd-or-dec
	      (concat cd-or-dec 
		      (make-string (- ld (mew-string-width cd-or-dec)) SP)))))
    (cond
     (filename
      (setq file filename))
     ((and file (not (equal file ".")) (not (string-match "/$" file)))
      (setq asterisk t)
      (setq file (concat file AR))))
    (if file
	(if (< lf (mew-string-width file))
	    (if asterisk
		(setq file (concat (mew-substring file lfc*) AR cnt))
	      (setq file (concat (mew-substring file lfc) cnt)))))
    (setq mew-syntax-multi-form
	  (cons (concat
		 marks
		 (if number (concat space number))
		 space space
		 ct
		 space space
		 cd-or-dec
		 space space
		 file
		 "\n")
		mew-syntax-multi-form))))

(defun mew-decode-syntax-delete ()
  (if (mew-decode-syntax-p)
      (let ((cbuf (current-buffer))
	    (pos (make-marker)))
	(set-buffer (mew-decode-syntax-buffer))
	(mew-syntax-clear-icon-spec)
	(mew-summary-toolbar-update)
	(set-marker pos (point))
	(mew-elet
	 (delete-region (mew-decode-syntax-begin) (mew-decode-syntax-end)))
	(mew-decode-syntax-remove)
	(goto-char (marker-position pos))
	(mew-highlight-cursor-line)
	(set-buffer-modified-p nil)
	(set-buffer cbuf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; icon spec
;;

(defvar mew-syntax-icon-spec nil)

(defun mew-syntax-set-icon-spec (part ct cd icon func)
  (if mew-icon-p
      (setq mew-syntax-icon-spec 
	    (cons
	     (vector icon func t
		     ;; cache buffer
		     (format "%s <%s> (%s) %s"
			     mew-cache-message-number
			     (or part "top") ct cd))
	     mew-syntax-icon-spec))))

(defun mew-syntax-clear-icon-spec ()
  (setq mew-syntax-icon-spec nil))

(defun mew-syntax-print-icon-spec (spec bar)
  (let ((toolbar))
    (cond
     ((eq mew-multipart-icon-position 'left)
      (setq toolbar (append spec mew-icon-separate-spec bar)))
     ((eq mew-multipart-icon-position 'right)
      (setq toolbar (append bar mew-icon-separate-spec spec)))
     (t (setq toolbar bar)))
    (set-specifier default-toolbar (cons (current-buffer) toolbar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decode privacy
;;

(defun mew-decode-syntax-set-privacy (syntax label)
  (let ((privacy (mew-syntax-get-privacy syntax))
	results)
    (while privacy
      (setq results (concat results (nth 2 (car privacy))))
      (setq privacy (cdr privacy)))
    (if results
	(setq mew-syntax-privacy-result
	      (concat mew-syntax-privacy-result
		      mew-x-mew:
		      (format " <%s> " label)
		      results
		      "\n")))))

(defun mew-decode-syntax-clear-privacy ()
  (setq mew-syntax-privacy-result nil))

(defun mew-decode-syntax-insert-privacy ()
  (if mew-syntax-privacy-result
      (let ((beg (point)))
	(insert mew-syntax-privacy-result)
	(mew-decode-header-property-region beg (point))
	(save-restriction
	  (narrow-to-region beg (point))
	  (goto-char (point-min))
	  (while (re-search-forward "BAD.*sign" nil t)
	    (put-text-property
	     (match-beginning 0)
	     (match-end 0)
	     'face
	     (intern-soft "mew-highlight-header-face-xmew-bad")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markers
;;

(defvar mew-marker-decode-syntax-begin (make-marker))
(defvar mew-marker-decode-syntax-end (make-marker))

(defvar mew-overlay-header-keymap nil)
(defvar mew-overlay-attach-keymap nil)

(mapcar (function make-variable-buffer-local)
	'(mew-overlay-header-keymap
	  mew-overlay-attach-keymap))

;; location

(defmacro mew-in-decode-syntax-p ()
  '(and (marker-position mew-marker-decode-syntax-begin)
	(marker-position mew-marker-decode-syntax-end)
	(>= (point) (marker-position mew-marker-decode-syntax-begin))
	(<  (point) (marker-position mew-marker-decode-syntax-end))))

(defmacro mew-in-header-p ()
  '(let ((end (mew-header-end)))
     (and end (<= (point) end))))

(defmacro mew-in-attach-p ()
  '(let ((beg (mew-attach-begin)))
     (and beg (> (point) beg)))) ;; excluding the preceding \n

;; existence

(defmacro mew-decode-syntax-p ()
  '(and (marker-position mew-marker-decode-syntax-begin)
	(marker-position mew-marker-decode-syntax-end)))

(defmacro mew-header-p ()
  '(next-single-property-change (point-min) 'read-only))

(defmacro mew-attach-p ()
  '(if (/= (point-max) 1)
       (get-text-property (1- (point-max)) 'mew-attach-end)))

(defmacro mew-attach-valid-p ()
  '(> (length mew-encode-syntax) (1+ mew-syntax-magic)))

;; point

(defmacro mew-decode-syntax-begin ()
  '(marker-position mew-marker-decode-syntax-begin))

(defmacro mew-decode-syntax-end ()
  '(marker-position mew-marker-decode-syntax-end))

(defmacro mew-header-end ()
  '(mew-header-p))

(defmacro mew-attach-begin ()
  '(if (mew-attach-p)
       (let ((beg (previous-single-property-change
		   (point-max) 'mew-attach-begin)))
	 (if beg (1- beg) nil))))

;;

(defmacro mew-decode-syntax-begin-set ()
  '(set-marker mew-marker-decode-syntax-begin (point)))

(defmacro mew-decode-syntax-end-set ()
  '(set-marker mew-marker-decode-syntax-end (point)))

(defmacro mew-decode-syntax-remove ()
  '(progn
     (set-marker mew-marker-decode-syntax-begin nil)
     (set-marker mew-marker-decode-syntax-end nil)))

(defmacro mew-decode-syntax-buffer ()
  '(set-buffer (marker-buffer mew-marker-decode-syntax-begin)))

(defun mew-summary-end-of-message-p ()
  (let (pos beg end)
    (save-excursion
      (set-buffer (mew-decode-syntax-buffer))
      (setq pos (point))
      (setq end (mew-decode-syntax-end))
      (goto-char end)
      (forward-line -1)
      (beginning-of-line)
      (setq beg (point))
      (and (<= beg pos) (< pos end)))))

;;

(defmacro mew-header-set (sep)
  (` (mew-elet
      (let ((end (point)))
	(if (, sep)
	    (insert (, sep))
	  (forward-line))
	(put-text-property end (point) 'read-only t)
	(mew-front-nonsticky end (1+ end))
	(mew-rear-nonsticky (1- (point)) (point))
	end))))

(defmacro mew-header-clear ()
  ;; the cursor moves to the end of the header (with some exceptions)
  '(mew-elet
    (mew-header-goto-end) ;; do not use mew-header-end
    (let ((pos (point)))
      (forward-line)
;;      (put-text-property pos (point) 'read-only nil)
      ;; If the body contains the read-only property, mew-header-p
      ;; makes a mistake. So, remove the read-only property from
      ;; the entire buffer.
      (put-text-property (point) (point-max) 'read-only nil)
      (delete-region pos (point)))))

;;

(defmacro mew-attach-set ()
  '(mew-elet
    (let (beg)
      (goto-char (point-max))
      (if (null (bolp)) (insert "\n"))
      (setq beg (point))
      (insert "\n")
      (insert mew-draft-attach-boundary-begin)
      (insert "\n")
      (insert mew-draft-attach-boundary-end)
      (insert "\n")
      (put-text-property beg (1+ beg) 'mew-attach-begin t)
      (put-text-property (1- (point)) (point) 'mew-attach-end t)
      (beginning-of-line)
      (mew-draft-attach-keymap))))

(defmacro mew-attach-clear ()
  '(if (mew-attach-p)
       (save-excursion
	 (mew-elet
	  (delete-region (mew-attach-begin) (point-max)))
	 (if mew-use-overlay-keymap
	     (mew-overlay-delete mew-overlay-attach-keymap)))))

(defmacro mew-header-prepared ()
  '(progn
     (mew-header-set (concat mew-header-separator "\n"))
     (if mew-config-insert-when-prepared
	 (mew-draft-insert-config 'nohighlight))
     (mew-highlight-header)
     (mew-draft-header-keymap)))

(defmacro mew-draft-header-keymap ()
  '(save-excursion
     (if mew-use-overlay-keymap
	 (if (mew-overlay-p mew-overlay-header-keymap)
	     (mew-overlay-move mew-overlay-header-keymap
			       (point-min) (1+ (mew-header-end)))
	   (setq mew-overlay-header-keymap
		 (mew-overlay-make (point-min) (1+ (mew-header-end))))
	   (mew-overlay-put mew-overlay-header-keymap
			    (if mew-xemacs-p 'keymap 'local-map)
			    mew-draft-header-map)
	   (mew-rear-sticky mew-overlay-header-keymap)))))

(defmacro mew-draft-attach-keymap ()
  '(progn
     (if mew-use-overlay-keymap
	 (if (mew-overlay-p mew-overlay-attach-keymap)
	     (mew-overlay-move mew-overlay-attach-keymap
			       (1+ (mew-attach-begin)) (point-max))
	   (setq mew-overlay-attach-keymap
		 (mew-overlay-make (1+ (mew-attach-begin)) (point-max)))
	   (mew-overlay-put mew-overlay-attach-keymap
			    (if mew-xemacs-p 'keymap 'local-map)
			    mew-draft-attach-map)))))

(provide 'mew-syntax)

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

;;; mew-syntax.el ends here
