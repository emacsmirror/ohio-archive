;; -*- Mode: Emacs-Lisp -*-
;;                       $Id: mew-nmz.el,v 1.7.2.6 2000/02/28 03:52:41 kazu Exp $
;;
;; mew-nmz.el: Another Mew search method, powered by Namazu.
;;
;;                                "Hideyuki SHIRAI" <shirai@rdmg.mgcs.mei.co.jp>
;;
;;;; Usage: Put your ~/.emacs.
;; (eval-after-load "mew" '(require 'mew-nmz))
;;

(defconst mew-nmz-version "mew-nmz.el 0.50")

(eval-when-compile (require 'mew))
(and (locate-library "namazu")
     (eval-when-compile (require 'namazu)))

;; Variables
(defvar mew-nmz-index-path "~/Namazu" "*Namazu index top directory.")
(defvar mew-nmz-index-mail "Mail" "*Namazu index mail directory.")
(defvar mew-nmz-index-news "News" "*Namazu index local news directory.")
(defvar mew-nmz-index-imap "Imap" "*Namazu index IMAP directory.")

(defvar mew-nmz-namazu-version nil
  "*Automatically set 'v1 if Namazu version 1. Set 'v2 if Namazu version 2.")

(defvar mew-nmz-use-fast-pick t "*Use fast pick mode.")
(defvar mew-nmz-use-backslash
  (if (memq system-type '(OS/2 emx windows-nt)) t nil)
  "*If non-nil, convert / => \ for Windows")

(defvar mew-nmz-prog "namazu" "*Namazu program name.")

(defvar mew-nmz-db-max 64 "*Namazu max index")
(defvar mew-nmz-query-max-length 256 "*Namazu query string max length.")

(defvar mew-nmz-prog-mknmz "mknmz" "*Namazu make index program.")
(defvar mew-nmz-prog-mknmz-args '("-q")
  "*Mknmz's argment, in addition to \"-U\", \"-h\".")
(defvar mew-nmz-prog-mknmz-include "~/Namazu/mknmz-inc.pl" "*Include file for mknmz.")

(defvar mew-nmz-prog-gcnmz "gcnmz" "*Namazu refresh index program.")
(defvar mew-nmz-use-gcnmz-folders (list mew-inbox-folder)
  "*Exec gcnmz after mknmz for index refresh, 't' means all folders.")

(defvar mew-nmz-mknmz-skip-folders
  (if (and (boundp 'mew-draft-folder) (boundp 'mew-trash-folder)
	   (boundp 'mew-queue-folder) (boundp 'mew-attach-folder)
	   mew-draft-folder mew-trash-folder mew-queue-folder mew-attach-folder)
      (list mew-draft-folder mew-trash-folder mew-queue-folder mew-attach-folder
	    "+schedule" "=draft")
    (list "+draft" "+trash" "+schedule" "=draft"))
  "*Skip folders regexp, when make index.")
(defvar mew-nmz-mknmz-skip-news t "*Skip local news folders, when make index.")
(defvar mew-nmz-mknmz-use-mode-line t "*Display \"nmz\" in mode line , when mknmzing.")
(defvar mew-nmz-line-id '("Mew(nmz): %7b")
  "*A value of mode-line-buffer-identification for Mew summary mode, when mknmzing.")

(defvar mew-nmz-pick-default-field nil
  "*Default prefix string to be appeared when inputing a namazu pick pattern.
A good example is \"+from:\".")
(defvar mew-nmz-pick-field-list
  '("+subject:" "+from:" "+to:" "+newsgroups:" "+date:"
    "+message-id:" "+cc:" "+in-reply-to:" "+references:")
  "*A list of key for namazu pick pattern.")

(defvar mew-nmz-imap-localfile-suffix ".gz"
  "*IMAP local file's suffix, need mew-fake-imap.el.")

(defvar mew-nmz-pick-gather-field-list
  (list (list mew-from: 'address "+from:" "+to:" "+cc:")
	(list mew-to: 'address "+from:" "+to:" "+cc:")
	(list mew-cc: 'address "+from:" "+to:" "+cc:")
	(list mew-message-id: 'msgid "+message-id:" "+in-reply-to:" "+references:")
	(list mew-in-reply-to: 'msgid "+message-id:" "+in-reply-to:" "+references:")
	(list mew-references: 'msgid "+message-id:" "+in-reply-to:" "+references:"))
  "*A list of completion keyword from message.")

(defvar mew-nmz-mark-unindexed mew-mark-review "*Mark for type unindexed messages.")

(defvar mew-nmz-use-namazu-el (locate-library "namazu")
  "*Use namazu-mode from mew.")
(defvar mew-nmz-namazu-full-window t "*Use namazu-mode full window.")

;; Key Bind
(add-hook 'mew-summary-mode-hook
	  '(lambda ()
	     (define-key mew-summary-mode-map "z/" 'mew-nmz-search)
	     (define-key mew-summary-mode-map "z?" 'mew-nmz-search-mark)
	     (define-key mew-summary-mode-map "zV" 'mew-nmz-virtual)
	     (define-key mew-summary-mode-map "zm" 'mew-nmz-mknmz)
	     (define-key mew-summary-mode-map "zu" 'mew-nmz-mark-unindexed)
	     (define-key mew-summary-mode-map "z^" 'mew-nmz-search-parent)
	     (define-key mew-summary-mode-map "zp" 'mew-nmz-search-parent)
	     (define-key mew-summary-mode-map "zn" '(lambda (arg)
						      (interactive "P")
						      (mew-nmz-search-parent t)))
	     (define-key mew-summary-mode-map "zN" 'mew-nmz-namazu)))

(add-hook 'mew-message-mode-hook
	  '(lambda ()
	     (define-key mew-message-mode-map "zp" 'mew-nmz-search-msgid-at-point)
	     (define-key mew-message-mode-map "zr" 'mew-nmz-search-msgid-region)))

(add-hook 'mew-quit-hook
	  '(lambda ()
	     (setq mew-nmz-indexed-folders nil)))

;; An addition for virtual mode.
(add-hook 'mew-virtual-mode-hook
	  '(lambda ()
	     (define-key mew-summary-mode-map "zj" 'mew-virtual-original-message)))

(defun mew-virtual-original-message (&optional arg)
  "Show original message location.
If executed with 'C-u', jump to original message folder and number."
  (interactive "P")
  (if (not (equal major-mode 'mew-virtual-mode))
      (message "This command can be used in Virtual mode only")
    (save-excursion
      (let (folder msg)
	(beginning-of-line)
	(if (not (re-search-forward
		  "\r \\([^ ]*\\) \\([1-9][0-9]*\\)$"
		  (save-excursion (end-of-line) (point)) t))
	    ()
	  (setq folder (mew-match 1)
		msg (mew-match 2))
	  (message "Original message at %s/%s" folder msg)
	  (if (not arg)
	      ()
	    (mew-nmz-goto-folder-msg folder msg)
	    (message "Original message at %s/%s... jump done." folder msg)))))))

;; internal variable, don't modify.
(defvar mew-nmz-mknmz-process nil)
(make-variable-buffer-local 'mew-nmz-mknmz-process)
(defvar mew-nmz-pick-pattern-hist nil)
(defvar mew-nmz-gather-header-list nil)
(defvar mew-nmz-namazu-last-folder nil)
(defvar mew-nmz-indexed-folders nil)
(defvar mew-nmz-namazu-version1-str "^  Search Program of Namazu v1\.[34]\.")
(defvar mew-nmz-namazu-version2-str "^namazu of Namazu [1-9.]+")

;; macros
(defmacro mew-nmz-expand-folder (folder)
  "Convert folder to namazu-index-dir."
  (` (cond
      ((mew-folder-mailp (, folder))
       (expand-file-name
	(substring (, folder) 1 nil)
	(expand-file-name  mew-nmz-index-mail mew-nmz-index-path)))
      ((mew-folder-local-newsp (, folder))
       (expand-file-name
	(substring (, folder) 1 nil)
	(expand-file-name mew-nmz-index-news mew-nmz-index-path)))
      ((mew-folder-imapp (, folder))
       (mew-imap-folder-dir
	(, folder)
	(expand-file-name mew-nmz-index-imap mew-nmz-index-path)))
      (t nil))))

(defmacro mew-nmz-dir-to-folder (nmzdir)
  "Convert namazu-index-directory to folder."
  (` (progn
       (if (string-match (concat "^\\(.*\\)" (regexp-quote mew-path-separator) "$")
			 (, nmzdir))
	   (setq (, nmzdir) (substring (, nmzdir) (match-beginning 1) (match-end 1))))
       (cond
	((string-match
	  (concat "^"
		  (regexp-quote (expand-file-name mew-nmz-index-mail mew-nmz-index-path))
		  (regexp-quote mew-path-separator)
		  "\\(.*\\)$")
	  (, nmzdir))
	 (concat "+" (substring (, nmzdir) (match-beginning 1) (match-end 1))))
	((string-match
	  (concat "^"
		  (regexp-quote (expand-file-name mew-nmz-index-news mew-nmz-index-path))
		  (regexp-quote mew-path-separator)
		  "\\(.*\\)$")
	  (, nmzdir))
	 (concat "=" (substring (, nmzdir) (match-beginning 1) (match-end 1))))
	((string-match
	  (concat "^"
		  (regexp-quote (expand-file-name mew-nmz-index-imap mew-nmz-index-path))
		  (regexp-quote mew-path-separator)
		  "\\(.*\\)$")
	  (, nmzdir))
	 (mew-nmz-dir-to-folder-imap (substring (, nmzdir) (match-beginning 1) (match-end 1))))
	(t nil)))))

;;
;; Namazu Version check.
(defun mew-nmz-version-set ()
  (interactive)
  (if mew-nmz-namazu-version
      ()
    (mew-set-buffer-tmp)
    (mew-piolet
     mew-cs-autoconv mew-cs-pick
     (apply (function call-process)
	    mew-nmz-prog nil t nil (list "-L" "en" "-v")))
    (goto-char (point-min))
    (if (re-search-forward mew-nmz-namazu-version2-str nil t)
	(setq mew-nmz-namazu-version 'v2)
      (goto-char (point-min))
      (if (re-search-forward mew-nmz-namazu-version1-str nil t)
	  (setq mew-nmz-namazu-version 'v1)
	(ding)
	(message "Something error occor. (Namazu version check)")
	(sit-for 1)))))

(add-hook 'mew-init-hook 'mew-nmz-version-set)

;;
;; "Make Index" functions.
(defun mew-nmz-mknmz (&optional folder remove callp)
  "Make namazu index for mew-nmz.
If executed with 'C-u', remove index files at the specified folder."
  (interactive)
  (save-excursion
    (let ((msgenb (interactive-p))
	  (suffix "")
	  procname)
      (if (not (mew-which mew-nmz-prog-mknmz exec-path))
	  (message "Please install mknmz.")
	(and current-prefix-arg (setq remove t))
	(if  (not folder)
	    (setq folder (mew-input-folder (buffer-name)))
	  (setq folder (directory-file-name folder)))
	(if (mew-folder-imapp folder)
	    (setq suffix mew-nmz-imap-localfile-suffix))
	(if (or (mew-folder-newsp folder)
		(mew-folder-virtualp folder)
		(mew-nmz-skip-folder folder)
		(and mew-nmz-mknmz-skip-news
		     (string-match "^=" folder)))
	    (and msgenb (message "Can't make namazu index in %s." folder))
	  (let ((folder-dir (mew-expand-folder folder))
		(namazu-dir (mew-nmz-expand-folder folder))
		(bufname (concat " *mew mknmz*" folder)))
	    (setq procname (concat mew-nmz-prog-mknmz "-" folder))
	    (if (get-process procname)
		(and msgenb (message "Detect running mknmz process in %s." folder))
	      (if (and folder-dir namazu-dir
		       (file-directory-p folder-dir))
		  (let ((file-list (directory-files folder-dir 'full-name
						    (concat "^[1-9][0-9]*" suffix "$")
						    'no-sort))
			(temp-file (expand-file-name
				    (mew-nmz-make-temp-name "mknmz_") mew-temp-dir))
			(prog-args mew-nmz-prog-mknmz-args)
			(exist-msg nil)
			file)
		    (if (and (eq 'v2 mew-nmz-namazu-version)
			     (not (string= suffix "")))
			(setq prog-args (append prog-args (list "-U")))
		      (setq prog-args (append prog-args (list "-Uh"))))
		    (while (and mew-nmz-use-backslash (file-exists-p temp-file))
		      (message "Warning!! same name det.")
		      (sit-for 1)
		      (setq temp-file (expand-file-name
				       (mew-nmz-make-temp-name "mknmz_") mew-temp-dir)))
		    (mew-set-buffer-tmp)
		    (while file-list
		      (setq file (car file-list))
		      (setq exist-msg t)
		      (if (not (file-directory-p file))
			  (if (and mew-nmz-use-backslash
				   (eq mew-nmz-namazu-version 'v1))
			      (insert (mew-nmz-slash-to-backslash file) "\n")
			    (insert file "\n")))
		      (setq file-list (cdr file-list)))
		    (mew-frwlet
		     mew-cs-autoconv mew-cs-pick
		     (write-region (point-min) (point-max) temp-file nil 'no-msg))
		    (kill-buffer (current-buffer))
		    (if (or (not exist-msg) remove)
			(and (file-directory-p namazu-dir)
			     (mew-nmz-index-delete namazu-dir)))
		    (if (not exist-msg)
			(progn
			  (delete-file temp-file)
			  (and msgenb (message "%s has no message." folder)))
		      (and (not (file-directory-p namazu-dir))
			   (mew-make-directory namazu-dir))
		      (if mew-nmz-prog-mknmz-include
			  (let ((inc-file (expand-file-name mew-nmz-prog-mknmz-include)))
			    (and (file-exists-p inc-file)
				 (setq prog-args (append prog-args
							 (list "-I" inc-file))))))
		      (setq prog-args (append prog-args
					      (list "-F" temp-file
						    "-O" namazu-dir)))
		      (if (file-name-all-completions "NMZ.lock" namazu-dir)
			  (progn
			    (message "Warning!! Something error in %s's index." folder)
			    (sit-for 1)))
		      (set-buffer (get-buffer-create bufname))
		      (buffer-disable-undo (current-buffer))
		      (erase-buffer)
		      (mew-frwlet
		       mew-cs-autoconv mew-cs-pick
		       (insert folder "\n")
		       (insert temp-file "\n"))
		      (if callp
			  (progn
			    (message "Mew mknmz (%s) ..." folder)
			    (mew-piolet
			     mew-cs-autoconv mew-cs-pick
			     (apply (function call-process)
				    mew-nmz-prog-mknmz
				    nil
				    (current-buffer)
				    nil
				    prog-args))
			    (if (and mew-nmz-prog-gcnmz
				     (not remove)
				     (or (eq mew-nmz-use-gcnmz-folders t)
					 (member folder mew-nmz-use-gcnmz-folders))
				     (mew-which mew-nmz-prog-gcnmz exec-path))
				(progn
				  (message "Mew mknmz (%s) ... refresh" folder)
				  (mew-piolet
				   mew-cs-autoconv mew-cs-pick
				   (apply (function call-process)
					  mew-nmz-prog-gcnmz
					  nil
					  (current-buffer)
					  nil
					  (list
					   (and (eq mew-nmz-namazu-version 'v2)
						"--no-backup")
					   (expand-file-name "NMZ" namazu-dir)))
				   (if (eq mew-nmz-namazu-version 'v1)
				       (let ((backfn (directory-files namazu-dir t ".*.BAK$")))
					 (while backfn
					   (if (file-writable-p (car backfn))
					       (delete-file (car backfn)))
					   (setq backfn (cdr backfn))))))))
			    (delete-file temp-file)
			    (goto-char (point-min))
			    (if (search-forward-regexp "^ERROR:.*$" nil t)
				(message "Mew mknmz (%s) ... %s." folder (mew-match 0))
			      (message "Mew mknmz (%s) ... done." folder))
			    (set-buffer-modified-p nil)
			    (kill-buffer (current-buffer)))
			(and msgenb (message "Mew mknmz (%s) ..." folder))
			(mew-piolet
			 mew-cs-autoconv mew-cs-pick
			 (setq mew-nmz-mknmz-process
			       (apply (function start-process)
				      procname
				      (current-buffer)
				      mew-nmz-prog-mknmz
				      prog-args)))
			(if (and mew-nmz-mknmz-use-mode-line
				 folder
				 (get-buffer folder)
				 (buffer-name (get-buffer folder)))
			    (save-excursion
			      (set-buffer (get-buffer folder))
			      (setq mode-line-buffer-identification mew-nmz-line-id)
			      (set-buffer-modified-p nil)))
			(set-process-sentinel mew-nmz-mknmz-process
					      'mew-nmz-mknmz-sentinel))
		      ))))))))))

(defun mew-nmz-mknmz-sentinel (process event)
  (save-excursion
    (let (folder msg)
      (set-buffer (process-buffer process))
      (goto-char (point-min))
      (if (not (looking-at "^.*$"))
	  (setq msg "Mew mknmz ... something error occur.")
	(setq folder (mew-match 0))
	(forward-line)
	(if (not (looking-at "^.*$"))
	    (setq msg (format "Mew mknmz (%s) ... something error occur." folder))
	  (let ((tmpfile (mew-match 0)))
	    (if (search-forward-regexp "^ERROR:.*$" nil t)
		(setq msg (format "Mew mknmz (%s) ... %s." folder (mew-match 0)))
	      (setq msg (format "Mew mknmz (%s) ... done" folder))
	      (if (not (or (null mew-nmz-indexed-folders)
			   (member folder mew-nmz-indexed-folders)))
		  (setq mew-nmz-indexed-folders
			(cons folder mew-nmz-indexed-folders))))
	    (and (file-readable-p tmpfile)
		 (file-writable-p tmpfile)
		 (condition-case err
		     (delete-file tmpfile)
		   (error nil))))))
      (setq mew-nmz-mknmz-process nil)
      (if (and mew-nmz-mknmz-use-mode-line
	       folder
	       (get-buffer folder)
	       (buffer-name (get-buffer folder)))
	  (save-excursion
	    (set-buffer (get-buffer folder))
	    (setq mode-line-buffer-identification mew-mode-line-id)
	    (set-buffer-modified-p nil)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (message "%s" msg)
      (sit-for 1))))

;; mode-line-buffer-identification modifier
(add-hook 'mew-summary-mode-hook
	  '(lambda ()
	     (if (and mew-nmz-mknmz-use-mode-line
		      (get-process (concat mew-nmz-prog-mknmz "-" (buffer-name))))
		 (setq mode-line-buffer-identification mew-nmz-line-id)
	       (setq mode-line-buffer-identification mew-mode-line-id))
	     (set-buffer-modified-p nil)))

(defun mew-nmz-mknmz-all-folders (&optional arg)
  "Make namazu index all folders."
  (interactive "P")
  (if (not arg)
      ()
    (message "folder setup ...")
    (mew-folder-setup nil (interactive-p))
    (message "folder setup ... Done."))
  (save-excursion
    (let ((folder-list mew-folder-list))
      (while (car folder-list)
	(condition-case err
	    (mew-nmz-mknmz (car folder-list) nil t)
	  (error nil))
	(setq folder-list (cdr folder-list)))))
  (message "Namazu make index done."))

(defun mew-nmz-mark-unindexed ()
  "Mark unindexed messages."
  (interactive)
  (mew-summary-only
   (if (mew-summary-exclusive-p)
       (save-excursion
	 (if (and (mew-summary-mark-collect
		   mew-nmz-mark-unindexed (point-min) (point-max))
		  (y-or-n-p (format "Unmark '%c' ? " mew-nmz-mark-unindexed)))
	     (if (fboundp 'mew-summary-batch-unmark) ;; 1.94.2
		 (mew-summary-batch-unmark (list mew-nmz-mark-unindexed) 'msg)
	       (mew-mark-undo-marks (list mew-nmz-mark-unindexed))))
	 (let* ((url-file-name
		 (expand-file-name
		  (if (eq 'v2 mew-nmz-namazu-version)
		      "NMZ.field.uri" "NMZ.field.url")
		  (mew-nmz-expand-folder (buffer-name))))
		(marked-messages 0)
		(unindexed-messages 0)
		(suffix (if (mew-folder-imapp (buffer-name))
			    mew-nmz-imap-localfile-suffix ""))
		msgnums)
	   (if (not (file-exists-p url-file-name))
	       (message "%s has no index file." (buffer-name))
	     (save-excursion
	       (mew-set-buffer-tmp)
	       (message "checking %s..." (file-name-nondirectory url-file-name))
	       (insert-file-contents url-file-name)
	       (while (search-forward-regexp
		       (concat "/\\([1-9][0-9]*\\)" suffix "$") nil t)
		 (setq msgnums
		       (cons (string-to-number
			      (buffer-substring (match-beginning 1) (match-end 1)))
			     msgnums)))
	       (kill-buffer (current-buffer)))
	     (message "checking %s ..." (buffer-name))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (if (and
		    (looking-at "^ *\\([1-9][0-9]*\\)")
		    (not (memq (string-to-number (mew-match 1)) msgnums))
		    (not (mew-in-decode-syntax-p)))
		   (progn
		     (setq unindexed-messages (1+ unindexed-messages))
		     (if (mew-summary-marked-p)
			 ()
		       (mew-summary-mark-as mew-nmz-mark-unindexed)
		       (setq marked-messages (1+ marked-messages)))))
	       (forward-line))
	     (cond
	      ((= unindexed-messages 1)
	       (message "%d message doesn't have index, %d marked."
			unindexed-messages marked-messages))
	      ((> unindexed-messages 1)
	       (message "%d messages don't have index, %d marked."
			unindexed-messages marked-messages))
	      (t
	       (message "all messages have index.")))))))))

;;
;; "search Message-ID" functions.
(defun mew-nmz-search-parent (&optional child mid)
  "Search *parent* message and jump to that.
If executed with 'C-u', search *child* message."
  (interactive "P")
  (let ((folder (mew-summary-folder-name))
	(idh (list (list mew-references: mew-in-reply-to:)
		   (list mew-message-id:)))
	(message (if child "children" "parent"))
	mess ref pid rh)
    (if mid
	(setq pid (list mid)
	      idh nil)
      (if (not (or (mew-summary-message-number) (mew-syntax-number)))
	  (error "No message here.")
	(save-excursion
	  (mew-summary-display nil)
	  (setq mess (or (mew-cache-hit
			  (cons (buffer-name) (mew-summary-message-number)))
			 (mew-buffer-message)))
	  (set-buffer mess)
	  (if child
	      (setq idh (car (cdr idh)))
	    (setq idh (car idh)))
	  (while idh
	    (setq rh (car idh))
	    (setq ref (mew-header-get-value rh))
	    (while (and ref (string-match "<[^>]+>" ref))
	      (setq pid (cons (mew-match 0 ref) pid))
	      (setq ref (substring ref (match-end 0))))
	    (setq idh (cdr idh)))
	  (if (not pid)
	      (error "No required header.")))))
    (if (mew-syntax-number)
	(while (not (mew-summary-message-number))
	  (forward-line -1)))
    (set-marker mew-summary-inbox-position (point) (current-buffer))
    (message "Searching %s ..." message)
    (let ((pattern1 "")
	  (pattern2 "")
	  (addpattern (if child "+in-reply-to:" "+message-id:"))
	  (range nil))
      (setq pattern1 (concat addpattern (car pid)))
      (if child
	  (setq addpattern "+references:")
	(setq pid (delete (car pid) pid)))
      (while pid
	(if (> (length (concat pattern2 addpattern (car pid)))
	       mew-nmz-query-max-length)
	    (setq pid nil)
	  (setq pattern2 (concat pattern2 addpattern (car pid)))
	  (setq addpattern (if child " | +references:" " | +message-id:"))
	  (setq pid (delete (car pid) pid))))
      (let ((pattern (list pattern1 pattern2)))
	(while (and (null range) pattern)
	  (if mid
	      ()
	    (message "Searching %s ... %s" message folder)
	    (setq range (mew-nmz-pick folder (car pattern)))
	    (if (not child) (setq range (nreverse range))))
	  (if range
	      ()
	    ;; all folder search
	    (message "Searching %s ... other folders" message)
	    (setq range (mew-nmz-multi-pick
			 (mew-nmz-expand-folder-regexp "*")
			 (car pattern) t))
	    (if (null range)
		(setq pattern (cdr pattern))
	      (setq folder (car (car range)))
	      (setq range (car (cdr (car range))))
	      (if (not child) (setq range (nreverse range)))
	      ))))
      (if (null range)
	  (error "No message found.")
	(if (eq major-mode 'mew-virtual-mode)
	    (save-excursion
	      (goto-char (point-min))
	      (if (not (re-search-forward
			(concat "\r " (regexp-quote folder) " " (car range) "$")
			nil t))
		  ()
		(setq folder (buffer-name))
		(beginning-of-line)
		(looking-at "^ *\\([1-9][0-9]*\\)[^0-9]")
		(setq range (list (mew-match 1))))))
	(mew-nmz-goto-folder-msg folder (car range))
	(message "Searching %s ... %s/%s." message folder (car range))))))

(defun mew-nmz-search-msgid-at-point ()
  (interactive)
  (let (start end (pos (point)))
    (if (and (re-search-backward "<" (save-excursion (beginning-of-line) (point)) t)
	     (setq start (point))
	     (re-search-forward ">" (save-excursion (end-of-line) (point)) t)
	     (setq end (point)))
	(mew-nmz-search-msgid (buffer-substring start end))
      (message "No Message-ID."))))

(defun mew-nmz-search-msgid-region (start end)
  (interactive "r")
  (mew-nmz-search-msgid (buffer-substring start end)))

(defun mew-nmz-search-msgid (mid)
  (interactive "sMessage-ID: ")
  (if (string-match "\\(<[^>]+>\\)" mid)
      (let ((mew-window-use-full t)
	    (mew-use-full-window t))
	(mew-nmz-search-parent nil (mew-match 1 mid)))
    (message "No Message-ID.")))

;;
;; "Search(like mew + im)" functions.
(defun mew-nmz-search-mark (&optional arg)
  "Namazu pick messages according to a pick pattern which you input,
then put the '*' mark onto them. If called with C-u, target is
the messages in the region."
  (interactive "P")
  (mew-summary-only
   (if arg
       (mew-nmz-search-mark-region (region-beginning) (region-end))
     (mew-nmz-search-mark-region (point-min) (point-max)))))

(defun mew-nmz-search-mark-region (r1 r2)
  (interactive "r")
  (if (equal (point-min) (point-max))
      (message "No messages in this buffer.")
    (let ((folder (buffer-name))
	  pattern first last range)
      (setq pattern (mew-nmz-input-pick-pattern))
      (message "Namazu picking messages in %s ..." folder)
      (goto-char r1)
      (if (eobp)
	  () ;; r1 <= r2, so if r1 = (point-max) then no messages.
	(setq first (mew-summary-message-number))
	(goto-char r2)
	(if (eobp)
	    (progn
	      (forward-line -1)
	      (setq r2 (point))))
	(setq last (mew-summary-message-number))
	(setq range (mew-nmz-pick folder pattern first last)))
      (message "Namazu picking messages in %s ... done" folder)
      (if (null range)
	  (message "No message to be marked.")
	(message "Marking messages ... ")
	(goto-char r1)
	(while (and range (< (point) r2))
	  (if (re-search-forward (format "^[ ]*%s[^0-9]" (car range)) nil t)
	      (if (not (mew-summary-marked-p))
		  (mew-summary-mark-as mew-mark-review)))
	  (setq range (cdr range)))
	(beginning-of-line)
	(set-buffer-modified-p nil)
	(message "Marking messages ... done")))))

(defun mew-nmz-search (&optional arg)
  "Namazu pick messages according to a pick pattern which you input,
then list them up used."
  (interactive "P")
  (mew-summary-only
   (let ((folder (mew-input-folder (buffer-name)))
	 (pattern nil)
	 (range nil))
     (if (null (file-directory-p (mew-expand-folder folder)))
	 (message "No such folder %s" folder)
       (setq pattern (mew-nmz-input-pick-pattern))
       (message "Namazu picking messages in %s ..." folder)
       (setq range (mew-nmz-pick folder pattern))
       (message "Namazu picking messages in %s ... done" folder)
       (if (fboundp 'mew-summary-folder-create)  ;; 1.94.2
	   (if (get-buffer folder)
	       (switch-to-buffer folder)
	     (mew-summary-folder-create folder))
	 (mew-summary-switch-to-folder folder))
       (if range
	   (if (and (not arg)
		    mew-nmz-use-fast-pick
		    mew-summary-cache-use
		    (not (mew-summary-folder-dir-newp))
		    (mew-summary-exclusive-p))
	       (let ((buffer-read-only nil))
		 (goto-char (point-min))
		 (while (not (eobp))
		   (if (or (not (looking-at "^[ ]*\\([1-9][0-9]*\\)"))
			   (and (looking-at "^[ ]*\\([1-9][0-9]*\\)")
				(not (member (mew-match 1) range))))
		       (delete-region (point)
				      (progn (forward-line) (point)))
		     (forward-line)))
		 (goto-char (point-min))
		 (set-buffer-modified-p nil))
	     (mew-summary-scan-body mew-prog-imls
				    'mew-summary-mode
				    folder
				    mew-cs-scan
				    (list range 'erase))))))))

;;
;; "Namazu virtual" function.
(defun mew-nmz-virtual ()
  "Another virtual mode with namazu."
  (interactive)
  (mew-summary-only
   (let ((vfolder (concat
		   "++"
		   (mew-input-string "Namazu virtual folder name %s(%s): "
				     "" ;; dummy
				     "vnamazu")))
	 (folders (mew-input-folders (buffer-name)))
	 (grep (mew-nmz-input-pick-pattern))
	 (imapregex (concat
		     "^"
		     (regexp-quote (expand-file-name mew-nmz-index-imap mew-nmz-index-path))))
	 namazu-dirs namazu-fast-dirs namazu-im-dirs)
     (if (null mew-nmz-indexed-folders)
	 (mew-nmz-gather-indexed-folder))
     (mapcar '(lambda (folder)
		(cond
		 ((string-match "^.*\\*$" folder)
		  (setq namazu-dirs (append namazu-dirs
					    (mew-nmz-expand-folder-regexp folder))))
		 ((member folder mew-nmz-indexed-folders)
		  (setq namazu-dirs (cons (mew-nmz-expand-folder folder) namazu-dirs)))))
	     folders)
     (setq namazu-dirs (mew-uniq-list namazu-dirs))
     (if (null namazu-dirs)
	 (message "Please make namazu index.")
       (let ((num 1) fld)
	 (if (and mew-nmz-use-fast-pick mew-summary-cache-use)
	     (mapcar '(lambda (dir)
			(if (mew-nmz-folder-dir-newp dir)
			    (if (string-match imapregex dir)
				(if (and
				     (setq fld (mew-nmz-dir-to-folder dir))
				     (get-buffer fld)
				     (mew-summary-exclusive-p))
				    (setq namazu-fast-dirs (cons dir namazu-fast-dirs))
				  (message "IMAP folder (%s) is exclusive." fld))
			      (setq namazu-im-dirs (cons dir namazu-im-dirs)))
			  (setq namazu-fast-dirs (cons dir namazu-fast-dirs))))
		     namazu-dirs)
	   (mapcar '(lambda (dir)
		      (if (string-match imapregex dir)
			  ()
			(setq namazu-im-dirs (cons dir namazu-im-dirs))))
		   namazu-dirs))
	 (set-buffer (get-buffer-create vfolder))
	 (switch-to-buffer vfolder)
	 (buffer-disable-undo (current-buffer))
	 (setq buffer-read-only nil)
	 (erase-buffer)
	 (mew-window-configure (current-buffer) 'summary)
	 (mew-virtual-mode)
	 (mew-folder-setup (buffer-name))
	 (mew-buffers-setup (buffer-name))
	 (mew-current-set 'message nil)
	 (mew-current-set 'part nil)
	 (mew-current-set 'cache nil)
	 (setq mew-summary-buffer-direction 'down)
	 (mew-decode-syntax-delete)
	 (setq buffer-read-only nil)
	 (switch-to-buffer vfolder)
	 (if namazu-fast-dirs
	     (progn
	       (message "Namazu picking ...")
	       (let ((fld-msgs (mew-nmz-multi-pick namazu-fast-dirs grep)))
		 (while fld-msgs
		   (let* ((fld (car (car fld-msgs)))
			  (msgs (car (cdr (car fld-msgs))))
			  (cache (expand-file-name mew-summary-cache-file
						   (mew-expand-folder fld))))
		     (message "Namazu picking ... (%s)" fld)
		     (set-buffer (get-buffer-create mew-buffer-tmp))
		     (erase-buffer)
		     (mew-frwlet mew-cs-scan mew-cs-dummy
				 (insert-file-contents cache))
		     (goto-char (point-min))
		     (let ((str ""))
		       (while (not (eobp))
			 (if (and (looking-at "^[ ]*\\([1-9][0-9]*\\)\\([ ]*.*\\)$")
				  (member (mew-match 1) msgs))
			     (setq str (concat str
					       (format "%5d%s" num (mew-match 2))
					       "\r " fld " " (mew-match 1) "\n")
				   num (1+ num)))
			 (forward-line))
		       (set-buffer vfolder)
		       (insert str)
		       (setq str "")))
		   (setq fld-msgs (cdr fld-msgs))))
	       (if (fboundp 'mew-summary-batch-unmark) ;; 1.94.2
		   (mew-summary-batch-unmark (list mew-mark-multi
						   mew-mark-review
						   mew-mark-delete
						   mew-mark-refile
						   mew-mark-tmp) nil)
		 (mew-mark-undo-marks (list mew-mark-multi
					    mew-mark-review
					    mew-mark-delete
					    mew-mark-unlink
					    mew-mark-refile
					    mew-mark-tmp) 'nomsg))))
	 (goto-char (point-min))
	 (setq buffer-read-only t)
	 (if (null namazu-im-dirs)
	     (message "Listing %s ... done" vfolder)
	   (condition-case nil
	       (let ((process-connection-type mew-connection-type1))
		 (message "Listing %s ..." vfolder)
		 (setq mew-summary-buffer-start-point (point))
		 (setq mew-summary-buffer-string nil) ;; just in case
		 (mew-pioalet
		  mew-cs-virtual mew-cs-pick mew-cs-pick
		  (setq mew-summary-buffer-process
			;; imls --namazu=yes --grep=pattern --src=namazu's index,... number
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
					"--namazu=yes"
					(concat "--grep=" grep)
					(concat "--src=" (mew-join "," namazu-im-dirs))
					(int-to-string num))))))
		 (mew-set-process-cs mew-summary-buffer-process
				     mew-cs-virtual mew-cs-dummy)
		 (set-process-filter mew-summary-buffer-process
				     'mew-summary-scan-filter)
		 (set-process-sentinel mew-summary-buffer-process
				       'mew-summary-scan-sentinel)
		 (setq mew-summary-buffer-reviews nil)
		 (process-kill-without-query mew-summary-buffer-process))
	     (quit
	      (set-process-sentinel mew-summary-buffer-process nil)
	      (setq mew-summary-buffer-start-point nil)
	      (setq mew-summary-buffer-process nil)
	      (setq mew-summary-buffer-string nil)
	      (setq mew-summary-buffer-reviews nil)))))))))

;;
;; Use namazu-mode.
(add-hook 'namazu-mode-hook
	  '(lambda ()
	     (define-key namazu-mode-map "m" 'mew-nmz-namazu-goto-mew)
	     (define-key namazu-mode-map "M" 'mew-nmz-namazu-return-mew)
	     (if (featurep 'xemacs)
		 (define-key namazu-mode-map [(shift button2)] 'mew-nmz-namazu-view-at-mouse)
	       (define-key namazu-mode-map [S-mouse-2] 'mew-nmz-namazu-view-at-mouse))))

(defun mew-nmz-namazu (&optional arg)
  "Use namazu-mode from mew."
  (interactive "P")
  (mew-summary-only
   (if (not mew-nmz-use-namazu-el)
       (message "Please install \"namazu.el\".")
     (if (null mew-nmz-indexed-folders)
	 (mew-nmz-gather-indexed-folder))
     (setq mew-nmz-namazu-last-folder (mew-summary-folder-name))
     (if (or arg (not (mew-nmz-namazu-goto-namazu)))
	 (let ((folders (mew-input-folders (buffer-name)))
	       (grep (mew-nmz-input-pick-pattern))
	       namazu-dirs dirlen)
	   (mapcar '(lambda (folder)
		      (cond
		       ((string-match "^.*\\*$" folder)
			(setq namazu-dirs (append namazu-dirs
						  (mew-nmz-expand-folder-regexp folder))))
		       ((member folder mew-nmz-indexed-folders)
			(setq namazu-dirs (cons (mew-nmz-expand-folder folder) namazu-dirs)))))
		   folders)
	   (setq namazu-dirs (mew-uniq-list namazu-dirs))
	   (setq dirlen (length namazu-dirs))
	   (if (not (> dirlen mew-nmz-db-max))
	       ()
	     (message "Warning: assigned indexes over DB_MAX.")
	     (sit-for 1))
	   ;; (setq dirlen (- dirlen mew-nmz-db-max))
	   ;; (setq namazu-dirs (nreverse (nthcdr dirlen namazu-dirs))))
	   (and (not (fboundp 'namazu))
		(require 'namazu))
	   (namazu 0 (mew-join " " namazu-dirs) grep)
	   (cond
	    ((boundp 'namazu-history)
	     (setq namazu-history (cons grep namazu-history)))
	    ((boundp 'namazu-keyword-history)
	     (setq namazu-keyword-history (cons grep namazu-keyword-history))))
	   (namazu-jump-next)
	   (if mew-nmz-namazu-full-window (delete-other-windows)))))))

(defun mew-nmz-namazu-goto-mew ()
  (interactive)
  (let ((pattern (concat "^\\(~?/.*\\)/\\([1-9][0-9]*\\)\\("
			 mew-nmz-imap-localfile-suffix
			 " \\| \\).*$"))
	fld msg)
    (beginning-of-line)
    (if (not (re-search-forward pattern nil t))
	(namazu-view)
      (setq fld (mew-match 1))
      (setq msg (mew-match 2))
      (setq fld (mew-nmz-url-to-folder fld))
      (beginning-of-line)
      (mew-nmz-goto-folder-msg fld msg))))

(defun mew-nmz-namazu-return-mew ()
  (interactive)
  (if mew-nmz-namazu-last-folder
      (mew-summary-goto-folder nil mew-nmz-namazu-last-folder)))

(defun mew-nmz-namazu-goto-namazu ()
  (if (not (and (boundp 'namazu-buffer)
		namazu-buffer
		(get-buffer namazu-buffer)
		(buffer-name (get-buffer namazu-buffer))
		(pop-to-buffer namazu-buffer)))
      nil
    (if mew-nmz-namazu-full-window (delete-other-windows))
    t))

(defun mew-nmz-namazu-view-at-mouse (event)
  "Namazu's mouse interface for Mew."
  (interactive "e")
  (set-buffer (event-buffer event))
  (goto-char (event-point event))
  (let ((pos (point))
	(mew-mail-pattern
	 (concat "^\\("
		 (if (not mew-nmz-use-backslash)
		     (expand-file-name mew-mail-path)
		   (concat "/"
			   (substring (expand-file-name mew-mail-path) 0 1)
			   "|"
			   (substring (expand-file-name mew-mail-path) 2)))
		 "\\|~/Mail\\)/.*/[1-9][0-9]*\\("
		 mew-nmz-imap-localfile-suffix
		 "\\)?"))
	(mew-news-pattern
	 (concat "^\\("
		 (if (not mew-nmz-use-backslash)
		     (expand-file-name mew-news-path)
		   (concat "/"
			   (substring (expand-file-name mew-news-path) 0 1)
			   "|"
			   (substring (expand-file-name mew-news-path) 2)))
		 "\\|~/News\\)/.*/[1-9][0-9]*\\("
		 mew-nmz-imap-localfile-suffix
		 "\\)?"))
	pos-title pos-url)
    (end-of-line)
    (and (re-search-backward namazu-output-title-pattern nil t)
	 (setq pos-title (point))
	 (goto-char pos)
	 (re-search-forward namazu-output-title-pattern nil t)
	 (re-search-backward namazu-output-url-pattern nil t)
	 (> (point) pos-title)
	 (setq pos-url (point))
	 (setq pos (point)))
    (goto-char pos)
    (beginning-of-line)
    (and (not pos-url)
	 (re-search-forward namazu-output-url-pattern nil t)
	 (setq pos-url (point)))
    (goto-char pos)
    (cond
     ((and pos-title pos-url
	   (or (looking-at mew-mail-pattern)
	       (looking-at mew-news-pattern)))
      (mew-nmz-namazu-goto-mew))
     ((and pos-title pos-url)
      (namazu-view))
     ((and pos-url (> namazu-current-page 0))
      (namazu-prev-page))
     ((and pos-title (< namazu-current-page namazu-max-page))
      (namazu-next-page))
     (t (message "nothing to do.")))))

;;
;; Input "Namazu pattern" funcions.
(defun mew-nmz-input-pick-pattern ()
  "Input mew-nmz pick pattern."
  (mew-input-clear)
  (let ((mew-nmz-gather-header-list (mew-nmz-pick-pattern-gather-header)))
    (setq mew-input-complete-function (function mew-nmz-pick-pattern))
    (let ((keymap (copy-keymap mew-input-map)) ret)
      (define-key keymap " " nil)
      (setq ret (read-from-minibuffer "Namazu pick pattern : "
				      mew-nmz-pick-default-field
				      keymap
				      nil
				      'mew-nmz-pick-pattern-hist))
      (mew-decode-syntax-delete)
      ret)))

(defun mew-nmz-pick-pattern-gather-header ()
  (if mew-nmz-pick-gather-field-list
      (save-excursion
	(let ((buf (mew-cache-hit
		    (cons (buffer-name) (mew-summary-message-number))))
	      (gather-list mew-nmz-pick-gather-field-list)
	      ret-list gather header duplchk)
	  (if (not (and buf
			(get-buffer buf)
			(buffer-name (get-buffer buf))))
	      ()
	    (set-buffer buf)
	    (while gather-list
	      (setq gather (car gather-list))
	      (setq header (mew-header-get-value (car gather)))
	      (if (and header (car (cdr gather)))
		  (cond
		   ((eq (car (cdr gather)) 'msgid)
		    (while (and header (string-match "<[^>]+>" header))
		      (let ((mid (mew-match 0 header)))
			(setq header (substring header (match-end 0)))
			(if (not (member mid duplchk))
			    (let ((prefix (nthcdr 2 gather)))
			      (setq duplchk (cons mid duplchk))
			      (while prefix
				(setq ret-list (cons (concat (car prefix) mid) ret-list))
				(setq prefix (cdr prefix))))))))
		   ((eq (car (cdr gather)) 'address)
		    (let ((addrs (mew-addrstr-parse-address-list header)))
		      (mapcar
		       '(lambda (addr)
			  (if (not (member addr duplchk))
			      (let ((prefix (nthcdr 2 gather)))
				(setq duplchk (cons addr duplchk))
				(while prefix
				  (setq ret-list (cons (concat (car prefix) addr) ret-list))
				  (setq prefix (cdr prefix))))))
		       addrs)))))
	      (setq gather-list (cdr gather-list)))
	    (if ret-list
		(setq ret-list (append ret-list (list (concat " " (make-string 70 ?-))))))
	    (nreverse ret-list))))))

(defun mew-nmz-pick-pattern ()
  (let* ((pat (mew-delete-pattern))
	 (clist (append mew-nmz-pick-field-list
			mew-nmz-gather-header-list)))
    (if (null pat)
	(mew-complete-window-show clist)
      (mew-complete
       pat
       (mapcar (function list) clist)
       "Namazu pick pattern "
       nil))))

;;
;; "Namazu search engine" funcions.
(defun mew-nmz-pick (folder pattern &optional first last)
  "Get message numbers with only one folder."
  (let ((namazu-dir (mew-nmz-expand-folder folder))
	(suffix (if (mew-folder-imapp folder)
		    mew-nmz-imap-localfile-suffix ""))
	msgs-int prog-args)
    (if (or (mew-folder-newsp folder)
	    (mew-folder-virtualp folder))
	(progn
	  (message "Can't namazu search in %s." folder)
	  (sit-for 1)
	  nil)
      (if (or (null namazu-dir)
	      (not (file-directory-p namazu-dir))
	      (not (file-exists-p (expand-file-name "NMZ.i" namazu-dir))))
	  (progn
	    (message "Please make namazu index in %s, first." folder)
	    (sit-for 1)
	    nil)
	(save-excursion
	  (if first (setq first (string-to-int first)))
	  (if last (setq last (string-to-int last)))
	  (mew-set-buffer-tmp)
	  (buffer-disable-undo (current-buffer))
	  (setq prog-args (append (if (eq mew-nmz-namazu-version 'v1)
				      (list "-aeS")
				    (list "--all" "--list" "--early"))
				  (list pattern)
				  (list namazu-dir)))
	  (mew-pioalet
	   mew-cs-autoconv mew-cs-pick mew-cs-pick
	   (apply (function call-process)
		  mew-nmz-prog nil t nil prog-args))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at (concat "^.*"
				    (regexp-quote mew-path-separator)
				    "+\\([1-9][0-9]*\\)"
				    suffix "$"))
		(let ((msgnum (string-to-int (mew-match 1))))
		  (if (and (or (not first)
			       (>= msgnum first))
			   (or (not last)
			       (<= msgnum last)))
		      (setq msgs-int (cons msgnum msgs-int)))))
	    (forward-line))
	  (setq msgs-int (sort msgs-int '<))
	  (mapcar 'int-to-string msgs-int))))))

(defun mew-nmz-multi-pick (namazu-dirs pattern &optional catch)
  "Get message numbers with many folders."
  (let ((tmpdirs namazu-dirs)
	(beg 0)
	(end mew-nmz-db-max)
	prog-args intmsgs retmsgs sortfld cell nmzdirs)
    (while tmpdirs
      (setq nmzdirs (cons (delete nil (mew-sublist namazu-dirs beg end)) nmzdirs))
      (setq tmpdirs (nthcdr mew-nmz-db-max tmpdirs))
      (setq beg (+ beg mew-nmz-db-max))
      (setq end (+ end mew-nmz-db-max)))
    (save-excursion
      (while (and nmzdirs
		  (or (not catch)
		      (and catch (null intmsgs))))
	(mew-set-buffer-tmp)
	(buffer-disable-undo (current-buffer))
	(setq prog-args (append (if (eq mew-nmz-namazu-version 'v1)
				    (list "-aS")
				  (list "--all" "--list"))
				(list pattern)
				(car nmzdirs)))
	(mew-pioalet
	 mew-cs-autoconv mew-cs-pick mew-cs-pick
	 (apply (function call-process)
		mew-nmz-prog nil t nil prog-args))
	(goto-char (point-min))
	(let ((msgregex (concat "^\\(.*\\)"
				(regexp-quote mew-path-separator)
				"\\([1-9][0-9]*\\)")) ;; ???
	      dir msgnum)
	  (while (not (eobp))
	    (if (not (looking-at msgregex))
		()
	      (setq dir (mew-buffer-substring (match-beginning 1) (match-end 1)))
	      (setq msgnum (string-to-int
			    (mew-buffer-substring (match-beginning 2) (match-end 2))))
	      (if (not (setq cell (assoc dir intmsgs)))
		  (setq intmsgs (cons (list dir (list msgnum)) intmsgs))
		(setq intmsgs (delete cell intmsgs))
		(setq cell (cons (car cell) (list (cons msgnum (car (cdr cell))))))
		(setq intmsgs (cons cell intmsgs))))
	    (forward-line))
	  (setq nmzdirs (cdr nmzdirs))))
      (if (null intmsgs)
	  nil
	(setq retmsgs intmsgs)
	(while retmsgs
	  (setq sortfld (cons (car (car retmsgs)) sortfld))
	  (setq retmsgs (cdr retmsgs)))
	(setq sortfld (sort sortfld 'string<))
	(while sortfld
	  (setq cell (assoc (car sortfld) intmsgs))
	  (setq retmsgs
		(cons
		 (list (mew-nmz-url-to-folder (car cell))
		       (mapcar 'int-to-string
			       (sort (car (cdr cell)) '<)))
		 retmsgs))
	  (setq sortfld (cdr sortfld)))
	;; '((folder (msg ...)) (folder (msg ...)) ...)
	(nreverse retmsgs)))))

;;
;; miscellaneous functions
(defun mew-nmz-dir-to-folder-imap (str)
  (if (string-match "^@[^#]+#[^/]+/\\(.*\\)$" str)
      (concat "%" (substring str (match-beginning 1) (match-end 1)))))
     
(defun mew-nmz-goto-folder-msg (fld msg)
  (if (and (mew-folder-imapp fld)
	   (and (get-buffer fld)
		(buffer-name (get-buffer fld))))
      (pop-to-buffer fld)
    (mew-summary-goto-folder nil fld)
    (while (processp mew-summary-buffer-process)
      (sit-for 1)
      (discard-input)))
  (mew-summary-jump-message msg)
  (mew-summary-display 'force))

(defun mew-nmz-make-temp-name (prefix)
  (let ((time (current-time)))
    (setq time (mapconcat '(lambda (x)
			     (format "%d" x))
			  time ""))
    (concat prefix time)))

(defun mew-nmz-slash-to-backslash (dir)
  "Convert '/' to '\'."
  (if (string-match "\\\\" mew-path-separator)
      dir
    (let ((backslash "\\")
	  (pos 0))
      (while (string-match (regexp-quote mew-path-separator) dir pos)
	(setq dir (concat (substring dir 0 (match-beginning 0))
			  backslash
			  (substring dir (match-end 0))))
	(setq pos (1+ (match-end 0))))
      dir)))

(defun mew-nmz-url-to-folder (url)
  "Convert namazu's output url to folder."
  (and mew-nmz-use-backslash
       (string-match "^/\\([a-zA-Z]\\)|\\(/.*\\)" url)
       (setq url (concat
		  (substring url (match-beginning 1) (match-end 1))
		  ":"
		  (substring url (match-beginning 2) (match-end 2)))))
  (setq url (expand-file-name url))
  (cond
   ((string-match (concat
		   "^"
		   (regexp-quote
		    (concat (expand-file-name mew-mail-path) mew-path-separator))
		   "\\(@.*\\)$")
		  url)
    (mew-nmz-dir-to-folder-imap (substring url (match-beginning 1) (match-end 1))))
   ((string-match (concat
		   "^"
		   (regexp-quote
		    (concat (expand-file-name mew-mail-path) mew-path-separator))
		   "\\(.*\\)$")
		  url)
    (concat "+" (substring url (match-beginning 1) (match-end 1))))
   ((string-match (concat
		   "^"
		   (regexp-quote
		    (concat (expand-file-name mew-news-path) mew-path-separator))
		   "\\(.*\\)$")
		  url)
    (concat "=" (substring url (match-beginning 1) (match-end 1))))

   (t (message "Ignore url(%s) in %s." url
	       (if (eq mew-nmz-namazu-version 'v1) "NMZ.field.url" "NMZ.field.uri")))))

(defun mew-nmz-expand-folder-regexp (folder)
  "Convert folder to namazu-index-dir with '*' expand."
  (if (null mew-nmz-indexed-folders)
      (mew-nmz-gather-indexed-folder))
  (let ((nmz-fld-list mew-nmz-indexed-folders)
	namazu-dir-list fld)
    (if (not (string-match "^\\([^*]+\\)\\*?$" folder))
	(if mew-use-imap
	    (setq folder "^[+=%]")
	  (setq folder "^[+=]"))
      (setq folder (mew-match 1 folder))
      (if (string-match "^\\(.*\\)/$" folder)
	  (setq folder (concat "^" (regexp-quote (mew-match 1 folder))))
	(setq folder (concat "^" (regexp-quote folder)))))
    (while (setq fld (car nmz-fld-list))
      (and (string-match folder fld)
	   (setq namazu-dir-list
		 (cons (mew-nmz-expand-folder fld) namazu-dir-list)))
      (setq nmz-fld-list (cdr nmz-fld-list)))
    (nreverse namazu-dir-list)))

(defun mew-nmz-gather-indexed-folder ()
  "Gather indexed folder."
  (interactive)
  (let ((folder-list mew-folder-list)
	nmzdir namazu-folder-list)
    (mapcar '(lambda (fld)
	       (and (setq nmzdir (mew-nmz-expand-folder fld))
		    (file-directory-p nmzdir)
		    (file-exists-p (expand-file-name "NMZ.i" nmzdir))
		    (setq namazu-folder-list
			  (cons (directory-file-name fld) namazu-folder-list))))
	    folder-list)
    (setq mew-nmz-indexed-folders (nreverse namazu-folder-list))
    (and (interactive-p) (message "Gather indexed folder...done."))))

(defun mew-nmz-index-delete (namazu-dir)
  "Delete namazu index files."
  (let ((nmz-list (file-name-all-completions "NMZ\." namazu-dir)))
    (mapcar '(lambda (file)
	       (and (file-writable-p (expand-file-name file namazu-dir))
		    (delete-file (expand-file-name file namazu-dir))))
	    nmz-list)))

(defun mew-nmz-skip-folder (fld)
  (let ((fls mew-nmz-mknmz-skip-folders))
    (catch 'match
      (while fls
	(if (string-match (concat "^" (car fls)) fld)
	    (throw 'match t))
	(setq fls (cdr fls)))
      nil)))

(defun mew-nmz-folder-dir-newp (namazu-dir)
  (let* ((folder (mew-nmz-dir-to-folder namazu-dir))
	 (buf (get-buffer folder)))
    (if (not buf)
	(let* ((dir (file-chase-links (mew-expand-folder folder)))
	       (tdir (if (and mew-touch-folder-p (boundp 'mew-summary-touch-file))
			 (nth 5 (file-attributes
				 (expand-file-name mew-summary-touch-file
						   (mew-expand-folder dir))))
		       (nth 5 (file-attributes dir))))
	       (da (car tdir))
	       (db (car (cdr tdir)))
	       (cache (expand-file-name mew-summary-cache-file dir))
	       (tcache (nth 5 (file-attributes cache)))
	       (fa (car tcache))
	       (fb (car (cdr tcache))))
	  (cond
	   ((null tdir) nil)
	   ((null tcache) t)  ;; no cache, do update!
	   ((> da fa) t)
	   ((= da fa) (if (> db fb) t nil)) ;; nil if same
	   (t nil)))
      (set-buffer buf)
      (if (and (not (mew-summary-folder-dir-newp))
	       (mew-summary-exclusive-p))
	  nil t))))

(provide 'mew-nmz)

;; end here.
