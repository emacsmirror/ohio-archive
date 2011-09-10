;;; mew-scan.el --- Scanning messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-scan-version "mew-scan.el version 0.28")

(require 'mew)

(defvar mew-summary-inbox-position (make-marker))

(defun mew-summary-get (&optional arg)
  "Get +inbox asynchronously."
  (interactive "P")
  (let ((inbox (mew-inbox-folder)))
    (if (get-buffer inbox)
	(switch-to-buffer inbox)
      (mew-summary-folder-create inbox))
    (if (and mew-summary-trace-directory (mew-folder-localp inbox))
	(cd (mew-expand-folder inbox)))
    (mew-summary-folder-cache-manage inbox)
    (if (and mew-summary-cache-use (mew-summary-folder-dir-newp))
	(progn
	  ;; scan the gap
	  (or arg (goto-char (point-max)))
	  (mew-summary-scan-body mew-prog-imls
				 'mew-summary-mode
				 inbox
				 mew-cs-scan
				 (mew-input-range
				  inbox mew-range-auto-alist mew-ask-range))
	  ;; wait for asynchronous process
	  (if mew-xemacs-p
	      (while mew-summary-buffer-process
		(accept-process-output)
		(sit-for 0.1));; to flush
	    (while mew-summary-buffer-process (sit-for 1)))))
    (set-marker mew-summary-inbox-position (point) (current-buffer))
    ;; for C-xC-x
    (or arg (goto-char (point-max)))
    (mew-summary-scan-body mew-prog-imget
			   'mew-summary-mode
			   inbox
			   mew-cs-scan)))

(defun mew-summary-exchange-point ()
  "Exchange the current point and the marker."
  (interactive)
  (mew-summary-only
   (and (equal (buffer-name) (mew-inbox-folder)) ;; xxx
	(marker-position mew-summary-inbox-position)
	(goto-char (marker-position mew-summary-inbox-position)))))

(defun mew-summary-ls (&optional arg jump)
  "List this folder asynchronously."
  (interactive "P")
  (mew-summary-only
   (let* ((folder (buffer-name))
	  (askp mew-ask-range)
	  (alist mew-range-auto-alist)
	  scanp range lines)
     (mew-summary-folder-cache-manage folder)
     ;; check scanp before (mew-mark-clean)
     (cond
      ((interactive-p)
       (setq alist mew-range-interactive-alist)
       (setq askp t)
       (setq scanp t))
      ((and mew-summary-cache-use mew-summary-imap-cache
	    (not (member folder mew-summary-imap-nocache-folders))
	    (mew-folder-imapp folder)
	    (file-exists-p (mew-expand-folder folder mew-summary-cache-file)))
       (setq scanp t))
      ((mew-folder-remotep folder) ;; xxx
       (setq alist mew-range-interactive-alist)
       (setq askp t)
       (setq scanp t))
      ((and mew-summary-cache-use (mew-summary-folder-dir-newp))
       (setq scanp t)))
     ;;
     (mew-mark-clean)
     (set-buffer-modified-p nil)
     (if (or jump (mew-folder-imapp folder)) (goto-char (point-max)))
     (mew-buffers-setup folder)
     (if (not scanp)
	 (or arg (goto-char (point-max)))
       (setq range (mew-input-range folder alist askp))
       (or arg (goto-char (point-max)))
       (if (equal (car range) "all")
	   (setq lines (mew-summary-mark-collect3 mew-mark-review)))
       (mew-summary-scan-body mew-prog-imls 
			      'mew-summary-mode
			      folder
			      mew-cs-scan
			      range
			      nil
			      nil
			      lines)))))

;;
;; Scan
;;

(defun mew-summary-scan-body (prog mode folder read &optional range folders grep reviews)
  (save-excursion
    (set-buffer (get-buffer-create folder))
    (if (not (mew-summary-exclusive-p))
	()
      (condition-case nil
	  (let ((process-connection-type mew-connection-type1))
	    (buffer-disable-undo (current-buffer))
	    (if (not (equal major-mode mode)) (funcall mode))
	    (mew-window-configure (current-buffer) 'summary)
	    (mew-current-set 'message nil)
	    (mew-current-set 'part nil)
	    (mew-current-set 'cache nil)
	    (setq mew-summary-buffer-direction 'down)
	    (mew-decode-syntax-delete)
	    (cond
	     ((string-match mew-prog-imget prog)
	      (if (string= mew-config-imget mew-config-default)
		  (message "Getting %s ..." folder)
		(message "Getting %s (%s)..." folder mew-config-imget)))
	     ((string-match mew-prog-imls prog)
	      (message "Listing %s ..." folder)
	      (if (or (equal 'erase (car (cdr range)))
		      (equal mode 'mew-virtual-mode))
		  (mew-erase-buffer))))
	    (setq mew-summary-buffer-start-point (point))
	    (setq mew-summary-buffer-string nil) ;; just in case
	    (setq mew-summary-buffer-config mew-config-imget)
	    (setq mew-summary-buffer-error nil)
	    (setq mew-summary-buffer-wrong-pws nil)
	    (mew-pioalet
	     read mew-cs-pick mew-cs-pick
	     (setq mew-summary-buffer-process
		   (apply (function start-process) 
			  prog;; name
			  (current-buffer) 
			  prog;; program
			  (format "--width=%d" (if mew-summary-scan-width
						   mew-summary-scan-width
						 (if (< (window-width) 80)
						     80
						   (window-width))))
			  (format "--mimedecodequoted=%s" (if mew-decode-quoted
							      "yes" "no"))
			  (append mew-prog-im-arg ;; xxx
				  (cond
				   ((string-match mew-prog-imget prog)
				    (append
				     (list (concat "--config="
						   mew-summary-buffer-config)
					   "--scaninboxonly=yes")
				     mew-prog-imget-arg-list))
				   ((string-match mew-prog-imls prog)
				    (cond
				     ((equal mode 'mew-summary-mode)
				      (append
				       (list (format "--thread=%s"
						     (if (mew-folder-newsp folder) 
							 "yes" "no")))
				       (list (concat "--src=" folder))
				       mew-prog-imls-arg-list
				       (if (listp (car range))
					   (car range)
					 (list (car range)))))
				     ((equal mode 'mew-virtual-mode)
				      (list
				       (concat "--src=" (mew-join "," folders))
				       (concat "--grep=" grep))))))))))
	    (mew-set-process-cs mew-summary-buffer-process read mew-cs-dummy)
	    (set-process-filter mew-summary-buffer-process
				'mew-summary-scan-filter)
	    (set-process-sentinel mew-summary-buffer-process
				  'mew-summary-scan-sentinel)
	    (setq mew-summary-buffer-reviews reviews)
	    (process-kill-without-query mew-summary-buffer-process))
	(quit
	 (set-process-sentinel mew-summary-buffer-process nil)
	 (setq mew-summary-buffer-start-point nil)
	 (setq mew-summary-buffer-process nil)
	 (setq mew-summary-buffer-string nil)
	 (setq mew-summary-buffer-reviews nil))))))

(defun mew-summary-scan-passwd (src)
  (let ((prompt "Enter password"))
    (if mew-use-imget-assoc
	(setq prompt (format "%s (%s)" prompt src))
      (if (not (equal mew-summary-buffer-config mew-config-default))
	  (setq prompt (format "%s (%s)" prompt mew-summary-buffer-config))))
    (setq prompt (concat prompt " : "))
    (if mew-use-cached-passwd
	(mew-input-passwd prompt src)
      (mew-input-passwd prompt))))

(defmacro mew-summary-scan-filter-skip ()
  '(setq mew-summary-buffer-string
	 (concat
	  (substring mew-summary-buffer-string 0 (match-beginning 0))
	  (substring mew-summary-buffer-string (match-end 0)))))

(defun mew-summary-scan-filter (process string)
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
	  (format "^%s: ERROR: \\([^\n]*\\)\n" prog))
	 (regex-imget-greeting "^imget: Getting new messages[^\n]*\n")
	 (regex-passwd "^Password (\\([^\)]+\\))")
	 wpw)
    ;; save-excursion is not usefule because sometime we want to 
    ;; move the cursor forward.
    (set-buffer (process-buffer process)) ;; necessary
    (setq mew-summary-buffer-string (concat mew-summary-buffer-string string))
    (if (string-match regex-wrong-pw mew-summary-buffer-string)
	(progn
	  (setq wpw (mew-match 1 mew-summary-buffer-string))
	  (mew-passwd-set-passwd wpw nil)
	  (setq mew-summary-buffer-wrong-pws
		(cons wpw mew-summary-buffer-wrong-pws))
	  (mew-summary-scan-filter-skip)))
    (if (string-match regex-err mew-summary-buffer-string)
	(progn
	  (setq mew-summary-buffer-error
		(mew-match 1 mew-summary-buffer-string))
	  (mew-summary-scan-filter-skip)))
    (if (string-match regex-passwd mew-summary-buffer-string)
	(progn
	  (process-send-string
	   process
	   (format "%s\n" (mew-summary-scan-passwd
			   (mew-match 1 mew-summary-buffer-string))))
	  (setq mew-summary-buffer-string "")))
    (if (string-match regex-imget-greeting mew-summary-buffer-string)
	(mew-summary-scan-filter-skip))
    (while (string-match "^ *[0-9]+.*\n" mew-summary-buffer-string)
      (goto-char (point-max))
      ;; the cursor moves forward!
      (mew-elet
       (insert (mew-match 0 mew-summary-buffer-string)))
      (setq mew-summary-buffer-string
	    (substring mew-summary-buffer-string (match-end 0))))
    (if (or (equal opos mew-summary-buffer-start-point)
	    (not (equal opos omax)))
	;; move the cursor to the original position.
	(goto-char opos))
    (set-buffer obuf)))

(defun mew-summary-scan-sentinel (process event)
  (let ((prog (process-name process))
	folder)
    (save-excursion
      (set-buffer (process-buffer process)) ;; necessary
      (setq folder (buffer-name))
      (cond
       (mew-summary-buffer-wrong-pws
	(cond
	 (mew-use-imget-assoc
	  (message "Password is wrong (%s)!"
		   (mapconcat (function identity)
			      mew-summary-buffer-wrong-pws
			      ",")))
	 ((not (equal mew-summary-buffer-config mew-config-default))
	  (message "Password is wrong (%s)!" mew-summary-buffer-config))
	 (t
	  (message "Password is wrong!"))))
       ;; must be here
       (mew-summary-buffer-error
	(message "%s" mew-summary-buffer-error))
       (t
	(mew-elet
	 (let ((reviews mew-summary-buffer-reviews))
	   (goto-char (point-max))
	   (keep-lines (concat mew-summary-message-regex))
	   ;; save cache only when success
	   (while reviews
	     (goto-line (car reviews))
	     (mew-summary-mark-as mew-mark-review)
	     (setq reviews (cdr reviews)))
	   (mew-summary-folder-cache-save folder)
	   (mew-highlight-mark-region
	    mew-summary-buffer-start-point (point-max))
	   (cond
	    ((string-match mew-prog-imget prog)
	     ;; the last messages are examined. this is not friendly
	     ;; to imget's assoc. But imget's assoc itself is awkward.
	     (cond
	      ((string-match (format "^%s: no \\(new \\)?message" prog)
			     mew-summary-buffer-string)
	       (message "No new message"))
	      ((string-match (format "^%s: \\([0-9]+\\) message" prog)
			     mew-summary-buffer-string)
	       (message "%s message(s)"
			(mew-match 1 mew-summary-buffer-string)))
	      (t
	       (message "Getting %s ... done" folder))))
	    ((string-match mew-prog-imls prog)
	     (message "Listing %s ... done" folder)))))))
      (set-buffer-modified-p nil)
      (setq mew-summary-buffer-start-point nil)
      (setq mew-summary-buffer-process nil)
      (setq mew-summary-buffer-string nil)
      (setq mew-summary-buffer-config nil)
      (setq mew-summary-buffer-error nil)
      (setq mew-summary-buffer-wrong-pws nil)
      (setq mew-summary-buffer-reviews nil)
      (cond
       ((string-match mew-prog-imget prog)
	;; On PPP environment, executing "imget" lets the dial be up.
	;; So, it's a good idea to flush queue at this time
	;; if messages to be sent exist.
	(if (and mew-auto-flush-queue (mew-flushable-p))
	    (let ((mew-ask-flush-queue nil))
	      (sit-for 1)		; wait
	      (mew-summary-flush-queue)))))
      (cond
       ((string-match mew-prog-imget prog)
	(run-hooks 'mew-summary-inc-sentinel-hook))
       ((string-match mew-prog-imls prog)
	(run-hooks 'mew-summary-scan-sentinel-hook))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; process control
;;; mew-summary-buffer-process is a key to see if exclusive
;;

(defun mew-summary-exclusive-p ()
  (cond
   ((eq mew-summary-buffer-process t)
    (message "Try again later.")
    nil) ;; not exclusive
   ((processp mew-summary-buffer-process)
    (message "%s is running. Try again later."
	     (process-name mew-summary-buffer-process))
    nil) ;; not exclusive
   (t t))) ;; exclusive

(defun mew-summary-kill-subprocess ()
  "\\<mew-summary-mode-map>
Kill a process in Summary mode such as 'imget' and 'imls'.
Sometime a process accidentally remains in Summary mode. 
In this situation, you cannot execute '\\[mew-summary-get]', '\\[mew-summary-ls]', nor '\\[mew-summary-exec]'.
Use this command to solve this problem."
  (interactive)
  (unwind-protect
      (if (null (processp mew-summary-buffer-process))
	  (message "No process to kill. This buffer is unlocked anyway.")
	(message "%s was killed" (process-name mew-summary-buffer-process))
	(kill-process mew-summary-buffer-process))
    (setq mew-summary-buffer-process nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary file cache
;;;

(defun mew-summary-compare-times (type)
  (let* ((dir (file-chase-links (mew-expand-folder (buffer-name))))
	 (tdir (if mew-touch-folder-p
		   (mew-file-get-time
		    (expand-file-name mew-summary-touch-file
				      (mew-expand-folder dir)))
		 (mew-file-get-time dir)))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (tcache (mew-file-get-time cache))
	 (tbuf mew-summary-buffer-folder-cache-time)
	 t1 t2)
    (cond
     ((eq type 'dir-cache)
      (setq t1 tdir)
      (setq t2 tcache))
     ((eq type 'cache-buf)
      (setq t1 tcache)
      (setq t2 tbuf)))
    (cond
     ((null t1) nil)
     ((null t2) t) ;; do update
     ((> (nth 0 t1) (nth 0 t2)) t)
     ((= (nth 0 t1) (nth 0 t2))
      (if (> (nth 1 t1) (nth 1 t2)) t nil)) ;; nil if equal
     (t nil))))

(defmacro mew-summary-folder-dir-newp ()
  '(mew-summary-compare-times 'dir-cache))

(defmacro mew-summary-folder-cache-newp ()
  '(mew-summary-compare-times 'cache-buf))

(defmacro mew-summary-folder-cache-updatep (folder)
  (` (and mew-summary-cache-use
	  (or (mew-folder-localp (, folder))
	      (and mew-use-imap mew-summary-imap-cache
		   (not (member (, folder) mew-summary-imap-nocache-folders))
		   (mew-folder-imapp (, folder)))))))

(defun mew-summary-folder-cache-manage (folder)
  (switch-to-buffer folder)
  (if (mew-summary-folder-cache-updatep folder)
      (let ((cache (mew-expand-folder folder mew-summary-cache-file)))
        (if (and (file-exists-p cache)
		 (or (mew-summary-folder-cache-newp) (mew-folder-imapp folder)))
	    (mew-elet
	     (mew-erase-buffer)
	     (mew-frwlet
	      mew-cs-scan mew-cs-dummy
	      (insert-file-contents cache))
	     (setq mew-summary-buffer-folder-cache-time 
		   (mew-file-get-time cache))
	     (mew-summary-batch-unmark (list mew-mark-refile) nil)
	     (mew-highlight-mark-region (point-min) (point-max))
	     (set-buffer-modified-p nil)))))
  (if (not (equal major-mode 'mew-summary-mode)) (mew-summary-mode)))

(defun mew-summary-folder-cache-save (folder)
  (if (mew-summary-folder-cache-updatep folder)
      (let ((cache (mew-expand-folder (buffer-name) mew-summary-cache-file)))
	(if (file-writable-p cache)
	    (save-restriction
	      (widen)
	      (if (mew-decode-syntax-p)
		  (let ((cbuf (current-buffer))
			(min (point-min))
			(max (point-max))
			(beg (mew-decode-syntax-begin))
			(end (mew-decode-syntax-end)))
		    (mew-set-buffer-tmp)
		    (insert-buffer-substring cbuf min beg)
		    (insert-buffer-substring cbuf end max)
		    (mew-frwlet
		     mew-cs-dummy mew-cs-scan
		     (write-region (point-min) (point-max) cache nil 'no-msg))
		    (set-buffer cbuf))
		(mew-frwlet
		 mew-cs-dummy mew-cs-scan
		 (write-region (point-min) (point-max) cache nil 'no-msg)))
	      (setq mew-summary-buffer-folder-cache-time
		    (mew-file-get-time cache)))))))

(provide 'mew-scan)

;;
;; Config for imget
;;

(defun mew-summary-config-imget ()
  "Set the config value for imget."
  (interactive)
  (setq mew-config-imget (mew-input-config mew-config-imget)))

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

;;; mew-scan.el ends here
