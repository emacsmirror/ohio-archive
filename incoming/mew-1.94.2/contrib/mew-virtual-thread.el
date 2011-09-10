;; -*- Mode: Emacs-Lisp -*-
;;  $Id: mew-virtual-thread.el,v 1.4.2.1 1999/10/20 11:20:55 kazu Exp $
;;
;; mew-virtual-thread.el --- "Virtual Thread mode for Mew, easy and safety :-)"
;;
;;                         "Hideyuki SHIRAI" <shirai@rdmg.mgcs.mei.co.jp>
;;                                            Created: <05/25/1999>
;;                                Revised: Time-stamp: <09/02/1999 11:08 shirai>
;;
;;; Usage
;;; 1. Put a below line on your ~/.emacs.
;; (eval-after-load "mew" '(require 'mew-virtual-thread))
;;
;;; 2. When type the "T" in summary-mode, list the threaded messages.
;;;    Selection of the messages has the four ways as described below.
;;;
;;;  2.1. without "C-u" and no "review mark".
;;;       => 'matched input REGEXP' messages.
;;;  2.2. without "C-u" and some "review mark".
;;;       => marked messages.
;;;  2.3. with "C-u" and the region.
;;;       => messages in the region.
;;;  2.4. with "C-u" and no region.
;;;       => messages after point.
;;;
;;; 3. When type the "T" in virtual-thread-mode with any marks,
;;;    The marks transfer to original folder.
;;;

(defconst mew-virtual-thread-version "mew-virtual-thread.el 0.16")

(eval-when-compile (require 'mew))

(defvar mew-virtual-thread-get-remote t
  "If non-nil, get remote folder's messages, before make a threaded summary buffer.")

(add-hook 'mew-summary-mode-hook
	  '(lambda ()
	     (define-key mew-summary-mode-map "T" 'mew-virtual-thread)))

(defun mew-virtual-thread (&optional arg)
  "\"Virtual Thread Mode\" execute."
  (interactive "P")
  (if (and (eq major-mode 'mew-virtual-mode)
	   mew-virtual-thread-original-folder)
      (mew-virtual-thread-mark-transfer arg)
    (mew-summary-only
     (if (not (mew-summary-exclusive-p))
	 ()
       (let* ((fld (buffer-name))
	      (src fld)
	      range beg end)
	 (if (not arg)
	     (progn
	       (setq range (mew-summary-mark-collect
			    mew-mark-review (point-min) (point-max)))
	       (if range
		   ()
		 (call-interactively 'mew-virtual-thread-mark-regexp)
		 (setq range (mew-summary-mark-collect
			      mew-mark-review (point-min) (point-max)))
		 (mew-summary-batch-unmark (list mew-mark-review) nil)))
	   (if (or (and (boundp 'mark-active) mark-active)
		   (and (functionp 'region-exists-p) (region-exists-p)))
	       (setq range (mew-virtual-thread-number
			    (min (region-beginning) (region-end))
			    (max (region-beginning) (region-end))))
	     (setq range (mew-virtual-thread-number
			  (progn (beginning-of-line) (point)) (point-max)))))
	 (or (listp range) (setq range (list range)))
	 (if (and mew-virtual-thread-get-remote (mew-folder-remotep fld))
	     (save-excursion
	       (goto-char (point-min))
	       (re-search-forward (concat "^ *" (car range)))
	       (beginning-of-line)
	       (mapcar (function
			(lambda (msg)
			  (if (re-search-forward (concat "^ *" msg) nil t)
			      (beginning-of-line))
			  (mew-summary-im-start
			   mew-prog-imcat fld nil msg nil nil
			   mew-cs-text-for-read 'noinsert)))
		       range)
	       (if (mew-folder-newsp fld)
		   (setq src (expand-file-name (substring fld 1) mew-temp-dir))
		 (setq src (mew-imap-folder-dir fld mew-temp-dir)))))
	 (if range
	     (mew-virtual-thread-scan
	      fld src
	      (mew-virtual-thread-range-conv range))
	   (message "Can't make Virtual thread mode.")))))))

(defun mew-virtual-thread-mark-transfer (&optional arg)
  (interactive "P")
  (if (not (and (eq major-mode 'mew-virtual-mode)
		mew-virtual-thread-original-folder))
      (message "This command can be used in Virtual Thread mode only.")
    (let ((review-msgs (mew-summary-mark-collect
			mew-mark-review (point-min) (point-max)))
	  (multi-msgs (mew-summary-mark-collect
		       mew-mark-multi (point-min) (point-max)))
	  (orig-fld mew-virtual-thread-original-folder)
	  (thread-buffer (get-buffer (current-buffer)))
	  tmp-msgs)
      (setq review-msgs (mew-uniq-list
			 (sort (mapcar 'string-to-int review-msgs) '<)))
      (setq multi-msgs (mew-uniq-list
			(sort (mapcar 'string-to-int multi-msgs) '<)))
      (setq tmp-msgs multi-msgs)
      (while tmp-msgs
	(if (memq (car tmp-msgs) review-msgs)
	    (setq multi-msgs (delete (car tmp-msgs) multi-msgs)))
	(setq tmp-msgs (cdr tmp-msgs)))
      (if (not (or review-msgs multi-msgs))
	  (message "No mark collect.")
	(if (null arg)
	    (mew-kill-buffer))
	(mew-virtual-thread-goto-folder orig-fld)
	(save-excursion
	  (and (or (mew-summary-mark-collect
		    mew-mark-review (point-min) (point-max))
		   (mew-summary-mark-collect
		    mew-mark-multi (point-min) (point-max)))
	       (y-or-n-p (format "Unmark '%c', '%c' ? "
				 mew-mark-review mew-mark-multi))
	       (mew-summary-batch-unmark (list mew-mark-review mew-mark-multi) 'msg))
	  (goto-char (point-min))
	  (let (msg)
	    (while (search-forward-regexp "^ *\\([1-9][0-9]*\\)" nil t)
	      (setq msg (string-to-number (mew-match 1)))
	      (if (and (not (mew-in-decode-syntax-p))
		       (not (mew-summary-marked-p)))
		  (cond
		   ((memq msg review-msgs)
		    (mew-summary-mark-as mew-mark-review))
		   ((memq msg multi-msgs)
		    (mew-summary-mark-as mew-mark-multi))))))
	  (set-buffer-modified-p nil)
	  (delete-other-windows)
	  (if (null arg)
	      ()
	    (split-window-vertically)
	    (mew-pop-to-buffer thread-buffer))
	  (message "Mark transfer done."))))))

(defun mew-virtual-thread-mark-regexp (regex)
  (interactive "sVirtual Thread Regexp: ")
  (if (not (equal regex ""))
      (save-excursion
        (goto-char (point-min))
        (while (and (not (eobp))
		    (re-search-forward regex nil t))
	  (if (or (mew-in-decode-syntax-p)
		  (mew-summary-marked-p))
	      ()
	    (mew-summary-mark-as mew-mark-review))
	  (forward-line)))))

(defun mew-virtual-thread-number (beg end)
  (let (msgs)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^ *\\([1-9][0-9]*\\)" end t)
	(if (or (mew-summary-marked-p) (mew-in-decode-syntax-p))
	    ()
	  (setq msgs (cons (mew-match 1) msgs)))))
    (nreverse msgs)))

(defun mew-virtual-thread-range-conv (range)
  (let (num snum enum ret)
    (message "Range conversion start...")
    (while range
      (setq num (string-to-number (car range)))
      (cond
       ((not snum)
	(setq snum num))
       ((and snum (not enum))
	(if (= num (1+ snum))
	    (setq enum num)
	  (setq ret (cons (number-to-string snum) ret))
	  (setq snum num)))
       (t ;; (and snum enum)
	(if (= num (1+ enum))
	    (setq enum num)
	  (setq ret (cons (concat (number-to-string snum)
				  "-"
				  (number-to-string enum))
			  ret))
	  (setq snum num)
	  (setq enum nil))))
      ;; final message
      (if (null (setq range (cdr range)))
	  (cond
	   ((not enum)
	    (if (= num (1+ snum))
		(setq ret (cons (concat (number-to-string snum)
					"-"
					(number-to-string num))
				ret))
	      (setq ret (cons (number-to-string num) ret))))
	   (t ;; (enum)
	    (setq ret (cons (concat (number-to-string snum)
				    "-"
				    (number-to-string enum))
			    ret))))))
    (nreverse ret)))

(defvar mew-virtual-thread-original-folder nil)
(make-variable-buffer-local 'mew-virtual-thread-original-folder)

(defun mew-virtual-thread-scan (fld src range)
  (let ((vfld (if (mew-folder-remotep fld)
		  (concat "++" fld "-thread")
		(concat "++" (substring fld 1) "-thread")))
	buf num)
    (setq buf (get-buffer-create vfld))
    (switch-to-buffer buf)
    (if (not (mew-summary-exclusive-p))
	()
      (setq mew-virtual-thread-original-folder fld)
      (delete-other-windows)
      (if (eq major-mode 'mew-virtual-mode)
	  ()
	(mew-virtual-mode)
	(mew-folder-setup (buffer-name))
	(mew-buffers-setup (buffer-name)))
      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (goto-char (point-max))
      (condition-case nil
	  (let ((process-connection-type mew-connection-type1))
	    (message "Listing %s ..." vfld)
	    (setq mew-summary-buffer-start-point (point))
	    (setq mew-summary-buffer-string nil) ;; just in case
	    (mew-piolet
	     mew-cs-virtual mew-cs-dummy
	     (setq mew-summary-buffer-process
		   (apply (function start-process)
			  mew-prog-imls	;; name
			  (current-buffer)
			  mew-prog-imls	;; program
			  (format "--width=%d" (if mew-summary-scan-width
						   mew-summary-scan-width
						 (if (< (window-width) 80)
						     80
						   (window-width))))
			  (format "--mimedecodequoted=%s" (if mew-decode-quoted
							      "yes" "no"))
			  (append mew-prog-im-arg
				  (list
				   "--thread=yes"
				   (concat "--src=" src))
				  (if (listp range)
				      range
				    (list range))))))
	    (mew-set-process-cs mew-summary-buffer-process
				mew-cs-virtual mew-cs-dummy)
	    (set-process-filter mew-summary-buffer-process
				'mew-virtual-thread-scan-filter)
	    (set-process-sentinel mew-summary-buffer-process
				  'mew-summary-scan-sentinel)
	    (setq mew-summary-buffer-reviews nil)
	    (process-kill-without-query mew-summary-buffer-process))
	(quit
	 (set-process-sentinel mew-summary-buffer-process nil)
	 (setq mew-summary-buffer-start-point nil)
	 (setq mew-summary-buffer-process nil)
	 (setq mew-summary-buffer-string nil)
	 (setq mew-summary-buffer-reviews nil)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))

(defun mew-virtual-thread-scan-filter (process string)
  (let* ((after-change-function nil)
	 (after-change-functions nil)
	 (obuf (current-buffer))
	 (opos (point))
	 (omax (point-max))
	 (prog (process-name process))
	 (regex-wrong-pw
	  (format "^%s: ERROR: invalid password (\\([^\)]+\\))[^\n]*\n"
		  prog))
	 (regex-err 
	  (format "^%s: ERROR: [^\n]*\n" prog))
	 (regex-passwd "^Password (\\([^\)]+\\))")
	 wpw fld)
    ;; save-excursion is not usefule because sometime we want to 
    ;; move the cursor forward.
    (set-buffer (process-buffer process)) ;; necessary
    (setq fld mew-virtual-thread-original-folder)
    (setq mew-summary-buffer-string 
	  (concat mew-summary-buffer-string string)) ;; nil can concat
    (if (string-match regex-wrong-pw mew-summary-buffer-string)
	(progn
	  (setq wpw (mew-match 1 mew-summary-buffer-string))
	  (mew-passwd-set-passwd wpw nil)
	  (setq mew-summary-buffer-wrong-pws
		(cons wpw mew-summary-buffer-wrong-pws))
	  (mew-summary-scan-filter-skip)))
    (if (string-match regex-err mew-summary-buffer-string)
	(progn
	  (setq mew-summary-buffer-wrong-pws 
		(cons 'mew-err mew-summary-buffer-wrong-pws))
	  (mew-summary-scan-filter-skip)))
    (if (string-match regex-passwd mew-summary-buffer-string)
	(progn
	  (process-send-string
	   process
	   (format "%s\n" (mew-summary-scan-passwd
			   (mew-match 1 mew-summary-buffer-string))))
	  (setq mew-summary-buffer-string "")))
    ;; just for imls
    (while (string-match "^\\( *\\([0-9]+\\).*\\)\n" mew-summary-buffer-string)
      (goto-char (point-max))
      ;; the cursor moves forward!
      (let ((buffer-read-only nil))
	(insert (mew-match 1 mew-summary-buffer-string)
		"\r " fld " " (mew-match 2 mew-summary-buffer-string)
		"\n"))
      (setq mew-summary-buffer-string
	    (substring mew-summary-buffer-string (match-end 0))))
    (if (or (equal opos mew-summary-buffer-start-point)
	    (not (equal opos omax)))
	;; move the cursor to the original position.
	(goto-char opos))
    (set-buffer obuf)))

(defun mew-virtual-thread-goto-folder (fld)
  (mew-summary-goto-folder nil fld)
  (while (processp mew-summary-buffer-process)
    (sit-for 1)
    (discard-input)))

(provide 'mew-virtual-thread)

;; end here.
