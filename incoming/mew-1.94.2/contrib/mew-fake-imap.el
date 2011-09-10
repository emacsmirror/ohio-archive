;; -*- Mode: Emacs-Lisp -*-
;;       $Id: mew-fake-imap.el,v 1.1.2.1 1999/10/20 11:20:54 kazu Exp $
;;
;; mew-fake-imap.el: Fake IMAP (message caching method).
;;
;;                                "Hideyuki SHIRAI" <Shirai@rdmg.mgcs.mei.co.jp>
;;
;;;; Usage: Put your ~/.emacs.
;; (eval-after-load "mew" '(require 'mew-fake-imap))
;;;; and set the bellowing values as you like.

(defconst mew-fake-imap-version "mew-fake-imap 0.03")

(defvar mew-fake-imap-nofolder-list '("%inbox" "%#mhinox" "%#mh/inbox")
  "*A IMAP folder list to local saving.
If 't', no folders are local saving. If 'nil', all folders are local saving. ")

(defvar mew-fake-imap-gzip "gzip" "*Compress program.")

(defvar mew-fake-imap-gzip-option '("--stdout")
  "*Compress program option.")

(defvar mew-fake-imap-gunzip "gzip" "*Uncompress program.")

(defvar mew-fake-imap-gunzip-option '("--stdout" "--decompress")
  "*Uncompress program option.")

(defvar mew-fake-imap-localfile-suffix ".gz"
  "*Local file's suffix.")

(defvar mew-fake-imap-touch "touch" "*Touch program.")

(eval-when-compile (require 'mew))

;; summary mode additional keybind.
(add-hook
 'mew-summary-mode-hook
 (lambda ()
   (define-key mew-summary-mode-map "\M-Is" 'mew-fake-imap-cache-save)
   (define-key mew-summary-mode-map "\M-Ig" 'mew-fake-imap-cache-get)
   (define-key mew-summary-mode-map "\M-Id" 'mew-fake-imap-cache-delete)
   (define-key mew-summary-mode-map "\M-Ir" 'mew-fake-imap-cache-reget)
   (define-key mew-summary-mode-map "\M-ID" 'mew-fake-imap-cache-incorrect-delete)))

(defun mew-fake-imap-cache-save ()
  "Save to local folder."
  (interactive)
  (message "Saving messages ... ")
  (mew-fake-imap-tmp-to-local (mew-summary-folder-name))
  (message "Saving messages ... done"))

(defun mew-fake-imap-cache-get (&optional arg)
  "Get remote messages to local."
  (interactive "P")
  (let ((msgs (mew-fake-imap-get-msgs arg))
	(fld (mew-summary-folder-name)))
    (mew-fake-imap-cache-get-msgs fld msgs)))

(defun mew-fake-imap-cache-delete (&optional arg)
  "Delete messages in local."
  (interactive "P")
  (let ((msgs (mew-fake-imap-get-msgs arg))
	(fld (mew-summary-folder-name)))
    (mew-fake-imap-cache-delete-msgs fld msgs)))

(defun mew-fake-imap-cache-reget (&optional arg)
  "Reget remote messages to local."
  (interactive "P")
  (let ((msgs (mew-fake-imap-get-msgs arg))
	(fld (mew-summary-folder-name)))
    (mew-fake-imap-cache-delete-msgs fld msgs)
    (mew-fake-imap-cache-get-msgs fld msgs)))

(defun mew-fake-imap-cache-get-msgs (fld msgs)
  (let (tmpdir)
    (if (mew-folder-newsp fld)
	(setq tmpdir (expand-file-name (substring fld 1) mew-temp-dir))
      (setq tmpdir (mew-imap-folder-dir fld mew-temp-dir)))
    (if (file-directory-p tmpdir)
	()
      (mew-make-directory tmpdir)
      (if (/= mew-folder-mode (mew-file-get-mode tmpdir))
	  (set-file-modes tmpdir mew-folder-mode)))
    (save-excursion
      (message "Caching messages ... ")
      (goto-char (point-min))
      (re-search-forward (concat "^ *" (car msgs)))
      (beginning-of-line)
      (mapcar (function
	       (lambda (msg)
		 (if (re-search-forward (concat "^ *" msg) nil t)
		     (beginning-of-line))
		 (mew-summary-im-start
		  mew-prog-imcat fld nil msg nil nil mew-cs-text-for-read 'noinsert)
		 (and (featurep 'mew-win32) (sit-for 0.1))))
	      msgs)
      (message "Caching messages ... done !!"))))

(defun mew-fake-imap-cache-delete-msgs (fld msgs)
  (let (tmpdir localdir tmpfile localfile)
    (if (mew-folder-newsp fld)
	(setq tmpdir (expand-file-name (substring fld 1) mew-temp-dir))
      (setq tmpdir (mew-imap-folder-dir fld mew-temp-dir))
      (setq localdir (mew-imap-folder-dir fld mew-mail-path))
      (message "Deleting messages ... ")
      (mapcar (function
	       (lambda (msg)
		 (if (not (string-match "^[1-9][0-9]*$" msg))
		     ()
		   (setq tmpfile (expand-file-name msg tmpdir))
		   (if (and tmpdir
			    (file-exists-p tmpfile)
			    (file-writable-p tmpfile))
		       (delete-file tmpfile))
		   (setq localfile (expand-file-name (concat msg mew-fake-imap-localfile-suffix)
						     localdir))
		   (if (and localdir
			    (file-exists-p localfile)
			    (file-writable-p localfile))
		       (delete-file localfile)))))
	      msgs)
      (mew-cache-flush)
      (message "Deleting messages ... done"))))

(defun mew-fake-imap-cache-incorrect-delete ()
  "Delete all incorrect messages."
  (interactive)
  (if (not (mew-summary-exclusive-p))
      ()
    (let ((fld (mew-summary-folder-name))
	  msgs existmsgs tmpdir localdir tmpfile localfile nosuffix)
      (if (not (mew-folder-remotep fld))
	  (error "This command can be used in \"REMOTE folder\" only.")
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^ *\\([1-9][0-9]*\\)" nil t)
	    (if (or (mew-summary-marked-p) (mew-in-decode-syntax-p))
		()
	      (setq msgs (cons (mew-match 1) msgs)))))
	(setq msgs (nreverse msgs))
	(message "Deleting incorrect messages ... ")
	(if (mew-folder-newsp fld)
	    (setq tmpdir (expand-file-name (substring fld 1) mew-temp-dir))
	  (setq tmpdir (mew-imap-folder-dir fld mew-temp-dir)))
	(setq existmsgs (file-name-all-completions "" tmpdir))
	(mapcar (function
		 (lambda (x)
		   (if (or (not (string-match "^[1-9][0-9]*$" x))
			   (member x msgs))
		       ()
		     (setq tmpfile (expand-file-name x tmpdir))
		     (if (and tmpdir
			      (file-exists-p tmpfile)
			      (file-writable-p tmpfile))
			 (delete-file tmpfile)))))
		existmsgs)
	(if (not (mew-folder-imapp fld))
	    ()
	  (setq localdir (mew-imap-folder-dir fld mew-mail-path))
	  (setq existmsgs (file-name-all-completions "" localdir))
	  (mapcar (function
		   (lambda (x)
		     (if (not (string-match (concat
					     "^\\([1-9][0-9]*\\)"
					     mew-fake-imap-localfile-suffix
					     "$")
					    x))
			 ()
		       (setq nosuffix (mew-match 1 x))
		       (if (member nosuffix msgs)
			   ()
			 (setq localfile (expand-file-name x localdir))
			 (if (and localdir
				  (file-exists-p localfile)
				  (file-writable-p localfile))
			     (delete-file localfile))))))
		  existmsgs))
	(message "Deleting incorrect messages ... done")))))
      
;; copy form mew-virtual-thread.el.
(defun mew-fake-imap-get-msgs (arg)
  (if (not (mew-summary-exclusive-p))
      ()
    (let ((fld (mew-summary-folder-name))
	  msgs beg end)
      (if (not (mew-folder-remotep fld))
	  (error "This command can be used in \"REMOTE folder\" only.")
	(if (not arg)
	    (progn
	      (setq msgs (mew-summary-mark-collect
			  mew-mark-review (point-min) (point-max)))
	      (if msgs
		  ()
		(call-interactively 'mew-fake-imap-mark-regexp)
		(setq msgs (mew-summary-mark-collect
			    mew-mark-review (point-min) (point-max)))
		(mew-summary-batch-unmark (list mew-mark-review) nil)))
	  (if (or (and (boundp 'mark-active) mark-active)
		  (and (functionp 'region-exists-p) (region-exists-p)))
	      (setq msgs (mew-fake-imap-number
			  (min (region-beginning) (region-end))
			  (max (region-beginning) (region-end))))
	    (setq msgs (mew-fake-imap-number
			(progn (beginning-of-line) (point)) (point-max)))))
	(or (listp msgs) (setq msgs (list msgs)))
	msgs))))

(defun mew-fake-imap-mark-regexp (regex)
  (interactive "sFake IMAP Regexp: ")
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

(defun mew-fake-imap-number (beg end)
  (let (msgs)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^ *\\([1-9][0-9]*\\)" end t)
	(if (or (mew-summary-marked-p) (mew-in-decode-syntax-p))
	    ()
	  (setq msgs (cons (mew-match 1) msgs)))))
    (nreverse msgs)))

;;
;; IMAP FAKEs.
;; Copy local message to temp directory, or remove local files.

(defadvice mew-summary-im-start (before start-fake activate)
  (condition-case err
      (mew-summary-im-start-fake imprg src dsts msgs discard arg rcs noinsert)
  (error nil)))

(defun mew-summary-im-start-fake (imprg src dsts msgs discard &optional arg rcs noinsert)
  (if (or (not (mew-folder-imapp src))
	  (eq mew-fake-imap-nofolder-list t)
	  (mew-folder-member src mew-fake-imap-nofolder-list))
      ()
    (let ((tmpdir (mew-imap-folder-dir src mew-temp-dir))
	  (localdir (mew-imap-folder-dir src mew-mail-path))
	  tmpfile localfile)
      (cond
       (discard	;; immv, imrm, imclean
	(or (listp msgs) (setq msgs (list msgs)))
	(mapcar (function (lambda (x)
			    (setq localfile (expand-file-name
					     (concat x mew-fake-imap-localfile-suffix)
					     localdir))
			    (and (file-readable-p localfile)
				 (file-writable-p localfile)
				 (delete-file localfile))))
		msgs))
       (t ;; imcat
	(setq localfile (expand-file-name
			 (concat msgs mew-fake-imap-localfile-suffix) localdir))
	(setq tmpfile (expand-file-name msgs tmpdir))
	(if (file-directory-p tmpdir)
	    ()
	  (mew-make-directory tmpdir)
	  (if (/= mew-folder-mode (mew-file-get-mode tmpdir))
	      (set-file-modes tmpdir mew-folder-mode)))
	(if (and (not (file-exists-p tmpfile))
		 (file-readable-p localfile))
	    (save-excursion
	      (mew-set-buffer-tmp)
	      (mew-plet
	       (apply (function call-process)
		      mew-fake-imap-gunzip nil (current-buffer) nil
		      (append mew-fake-imap-gunzip-option (list localfile))))
	      (mew-flet
	       (write-region (point-min) (point-max) tmpfile nil 'nomsg))
	      (if (/= mew-file-mode (mew-file-get-mode tmpfile))
		  (set-file-modes tmpfile mew-file-mode))
	      (erase-buffer)
	      (apply (function call-process)
		     mew-fake-imap-touch nil nil nil (list localfile)))))))))

;;
;; Save temp messages to local.

(defadvice mew-temp-dir-clean-up (before finish-fake activate)
  (mew-buffers-clean-up))

(defadvice mew-remote-folder-cache-delete (before finish-fake activate)
  (condition-case err
      (mew-fake-imap-tmp-to-local folder)
    (error nil)))

(defun mew-fake-imap-tmp-to-local (folder)
  (if (or (not (mew-folder-imapp folder))
	  (eq mew-fake-imap-nofolder-list t)
	  (mew-folder-member folder mew-fake-imap-nofolder-list))
      ()
    (let ((tmpdir (mew-imap-folder-dir folder mew-temp-dir))
	  (localdir (mew-imap-folder-dir folder mew-mail-path))
	  msgs tmpfile localfile
	  tmptime tmptimea tmptimeb localtime localtimea localtimeb)
      (if (or (not (file-exists-p tmpdir))
	      (not (file-directory-p tmpdir))
	      (not (file-exists-p localdir))
	      (not (file-directory-p localdir)))
	  ()
	(setq msgs (file-name-all-completions "" tmpdir))
	(mapcar (function
		 (lambda (msg)
		   (if (not (string-match "^[1-9][0-9]*$" msg))
		       ()
		     (setq tmpfile (expand-file-name msg tmpdir))
		     (setq localfile (expand-file-name
				      (concat msg mew-fake-imap-localfile-suffix)
				      localdir))
		     (if (and (file-readable-p tmpfile)
			      (file-writable-p tmpfile))
			 (if (not (file-exists-p localfile))
			     (mew-fake-imap-tmp-to-local-move tmpfile localfile)
			   (setq tmptime (mew-file-get-time tmpfile))
			   (setq tmptimea (car tmptime))
			   (setq tmptimeb (car (cdr tmptime)))
			   (setq localtime (mew-file-get-time localfile))
			   (setq localtimea (car localtime))
			   (setq localtimeb (car (cdr localtime)))
			   (if (or (> tmptimea localtimea)
				   (and (= tmptimea localtimea) (> tmptimeb localtimeb)))
			       (mew-fake-imap-tmp-to-local-move tmpfile localfile)))))))
		msgs)))))

(defun mew-fake-imap-tmp-to-local-move (tmpfile localfile)
  (mew-set-buffer-tmp)
  (mew-plet
   (apply (function call-process)
	  mew-fake-imap-gzip nil (current-buffer) nil
	  (append mew-fake-imap-gzip-option (list tmpfile))))
  (mew-flet
   (write-region (point-min) (point-max) localfile nil 'nomsg))
  (if (/= mew-file-mode (mew-file-get-mode localfile))
      (set-file-modes localfile mew-file-mode))
  (kill-buffer (current-buffer)))

(provide 'mew-fake-imap)

;; ends here.
