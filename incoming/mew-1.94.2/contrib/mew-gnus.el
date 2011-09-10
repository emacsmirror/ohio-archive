;; mew-gnus.el
;;
;; Temporary solution to link Mew to Gnus.
;; This code will be obsolated because Mew supports USENET news soon.
;;
;; To use mew-gnus.el, put the following codes in your .emacs.
;;
;;   (add-hook
;;    'gnus-group-mode-hook
;;    (function
;;     (lambda ()
;;       (require 'mew-gnus)
;;       (define-key gnus-group-mode-map "a" 'mew-gnus-post-news))))
;;
;;   (add-hook
;;    'gnus-summary-mode-hook
;;    (function
;;     (lambda ()
;;       (define-key gnus-summary-mode-map "a" 'mew-gnus-post-news)
;;       (define-key gnus-summary-mode-map "r" 'mew-gnus-reply)
;;       (define-key gnus-summary-mode-map "R" 'mew-gnus-reply-with-citation)
;;       (define-key gnus-summary-mode-map "f" 'mew-gnus-mail-forward))))
;;
;;   (setq gnus-default-article-saver 'gnus-summary-save-in-mew)
;;

(eval-when-compile
  (require 'gnus)
  (if (not (or (string-match "^GNUS [34]" gnus-version)
	       (string-match "^Gnus v5.0" gnus-version)
	       (string-match "^5.[0-3]" gnus-version-number)))
      (require 'gnus-sum)))

(require 'mew)

(defvar mew-prog-imstore "imstore")
(defvar mew-prog-imstore-arg "--dst=%s")
;(defvar mew-prog-imstore "/usr/local/lib/mh/rcvstore")
;(defvar mew-prog-imstore-arg "%s")

(defvar mew-gnus-save-fixed-folder nil
  "*If specified, always use it as a candidate to save article.")

(defvar mew-gnus-save-preserve-dot t
  "*If nil, use hierarchical directory to save article.")

(defvar mew-gnus-save-news-folder nil
  "*If non-nil, use news folder as a default candicate to save an article.")

(defun mew-gnus-newsgroup-name ()
  (if mew-gnus-save-preserve-dot
      gnus-newsgroup-name
    (gnus-newsgroup-directory-form gnus-newsgroup-name)))

(defun gnus-summary-save-in-mew (&optional folder)
  "Save this article to Mail or News folder (using `imstore').
Optional argument FOLDER specifies folder name."
  (interactive)
  (mew-gnus-init)
  (let ((gnus-show-mime nil)
	(gnus-article-display-hook nil))
    (gnus-summary-select-article t t))  ;; force to display all headers
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (or mew-folder-alist
	  (setq mew-folder-list (mew-folder-make-list nil)
		mew-folder-alist (mew-folder-make-alist mew-folder-list)))
      (let ((folder
	     (or folder
		 (mew-input-folder
		  (or mew-gnus-save-fixed-folder
		      (car (mew-refile-guess-by-alist))
		      (concat (if mew-gnus-save-news-folder "=" "+")
			      (mew-gnus-newsgroup-name))))))
	    (errbuf (get-buffer-create " *GNUS imstore*")))
	(if (not (memq (aref folder 0) '(?+ ?=)))
	    (message (format
		      "First letter of '%s' must be '+' or '='."
		      folder))
	  (if (mew-folder-check folder)
	      (unwind-protect
		  (mew-piolet
		   mew-cs-infile mew-cs-outfile
		   (call-process-region (point-min) (point-max)
					mew-prog-imstore nil errbuf nil
					(format mew-prog-imstore-arg folder)))
		(set-buffer errbuf)
		(if (zerop (buffer-size))
		    (message "Article saved in folder: %s" folder)
		  (message "%s" (buffer-string)))
		(kill-buffer errbuf))))))))

(defun mew-gnus-init ()
  "Initialize mew if mew does not invoked yet."
  (if mew-mail-path
      nil
    (mew-init)
    (if (get-buffer mew-buffer-hello)
	(kill-buffer mew-buffer-hello))))

(defun mew-gnus-post-news ()
  "Post a news using mew."
  (interactive)
  (mew-gnus-init)
  (let ((file (mew-folder-new-message mew-draft-folder)))
    (mew-summary-prepare-draft
     (mew-current-set 'window (current-window-configuration))
     (delete-other-windows)
     (switch-to-buffer (find-file-noselect file))
     (mew-draft-rename file)
     (mew-draft-header nil nil 'no nil "")
     (goto-char (point-min))
     (search-forward "Newsgroups: ")
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook))))

(defun mew-gnus-reply (&optional yank)
  "Reply or followup to GNUS article using mew.
Optional argument YANK means yank original article."
  (interactive)
  (mew-gnus-init)
  (let ((file (mew-folder-new-message mew-draft-folder))
	from cc subject to reply-to newsgroups in-reply-to references
	distribution)
    (mew-summary-prepare-draft
     (mew-current-set 'window (current-window-configuration))
     (delete-other-windows)
     (gnus-summary-display-article (gnus-summary-article-number) t) ;;redisplay
     (pop-to-buffer gnus-article-buffer)
     (goto-char (point-max))
     (push-mark (point) t t)
     (goto-char (point-min))
     (search-forward "\n\n" nil t)
     (let ((split-window-keep-point t))
       (split-window-vertically))

     (setq from (mew-addrstr-parse-address-list (gnus-fetch-field "From"))
	   subject (let ((subject (gnus-fetch-field "Subject")))
		     (if (and subject
			      (not (string-match "^[Rr][Ee]:.+$" subject)))
			 (concat "Re: " subject) subject))
	   reply-to (gnus-fetch-field "Reply-to")
	   to (or reply-to from)
	   cc (gnus-fetch-field "Cc")
	   newsgroups (or (gnus-fetch-field "Followup-To")
			  (gnus-fetch-field "Newsgroups"))
	   distribution (gnus-fetch-field "Distribution"))

     ;; see comments at mew-summary-reply() function
     (let ((old-message-id  (gnus-fetch-field "Message-Id"))
	   (old-in-reply-to (gnus-fetch-field "In-Reply-To"))
	   (old-references  (gnus-fetch-field "References"))
	   (regex "<[^>]+>")
	   (start 0) tmp-ref skip)
       (if (and old-message-id (string-match regex old-message-id))
	   (setq old-message-id (mew-match 0 old-message-id))
	 (setq old-message-id nil))
       (if (and old-in-reply-to (string-match regex old-in-reply-to))
	   (setq old-in-reply-to (mew-match 0 old-in-reply-to))
	 (setq old-in-reply-to nil))
       (if (null old-message-id)
	   () ;; we don't care even if old-references exist.
	 (setq in-reply-to old-message-id)
	 (if (null old-references)
	     (setq tmp-ref (if old-in-reply-to 
			       (list old-in-reply-to old-message-id)
			     (list old-message-id)))
	   (while (string-match "<[^>]+>" old-references start)
	     (setq start (match-end 0))
	     (setq tmp-ref (cons (mew-match 0 old-references) tmp-ref)))
	   (if (and old-in-reply-to (not (member old-in-reply-to tmp-ref)))
	       (setq tmp-ref (cons old-in-reply-to tmp-ref)))
	   (setq tmp-ref (nreverse (cons old-message-id tmp-ref))))
	 (if (integerp mew-references-max-count)
	     (setq skip (- (length tmp-ref) mew-references-max-count)))
	 (if (and (numberp skip) (> skip 0))
	     (setq tmp-ref (nthcdr skip tmp-ref)))
	 (setq references (mew-join "\n\t" tmp-ref))))
     (switch-to-buffer-other-window (find-file-noselect file))
     (mew-draft-rename file)
     (mew-draft-header subject nil to cc newsgroups in-reply-to references)
     (if (stringp distribution)
	 (save-excursion
	   (goto-char (point-min))
	   (search-forward "Newsgroups:")
	   (forward-line 1)
	   (insert (concat "Distribution: " distribution "\n"))))
     (if (eq mew-summary-reply-position 'body)
	 (progn
	   (goto-char (mew-header-end))
	   (forward-line)))
     (make-variable-buffer-local 'mew-message-citation-buffer)
     (setq mew-message-citation-buffer gnus-article-buffer)
     (undo-boundary)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook)
     (if yank
	 (progn
	   (goto-char (point-max))
	   (mew-draft-cite))))))

(defun mew-gnus-reply-with-citation ()
  "Reply or followup to GNUS article using mew.
Original article is yanked automatically."
  (interactive)
  (mew-gnus-reply t))

(defun mew-gnus-mail-forward (&optional buffer)
  "Forward the current message to another user using mew."
  (interactive)
  (mew-gnus-init)
  (mew-current-set 'window (current-window-configuration))
  (gnus-summary-display-article (gnus-summary-article-number)) ;; redisplay
  (pop-to-buffer gnus-article-buffer)
  (let* ((subject (concat "[" gnus-newsgroup-name "] "
			  (or (gnus-fetch-field "subject") "")))
	 (draft (mew-folder-new-message mew-draft-folder))
	 (dirname (file-name-nondirectory draft)))
    (mew-summary-prepare-draft
     (mew-gnus-buffer-copy draft
			   (or (and (boundp 'gnus-original-article-buffer)
				    gnus-original-article-buffer)
			       gnus-article-buffer))
     (let ((split-window-keep-point t))
       (split-window-vertically))
     (switch-to-buffer-other-window (find-file-noselect draft))
     (mew-draft-rename draft)
     (mew-draft-header subject 'nl)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook)
     (setq mew-encode-syntax (mew-encode-syntax-initial-multi dirname 1))
     (save-excursion
       (mew-draft-prepare-attachments t)))))

(defun mew-gnus-buffer-copy (draft buffer)
  (let* ((mimefolder (mew-draft-to-mime draft))
         (mimedir (mew-expand-folder mimefolder)))
    (if (null (file-directory-p mimedir))
        (mew-make-directory mimedir)
      (if (null (mew-directory-empty-p mimedir))
          (if (y-or-n-p (format "%s is not empty. Delete it? " mimefolder))
              (progn
                (mew-delete-directory-recursively mimedir)
                (mew-make-directory mimedir)))))
    (save-excursion
      (set-buffer buffer)
      (write-region (point-min) (point-max)
		    (mew-folder-new-message mimefolder)))))

(provide 'mew-gnus)
;;; mew-gnus.el ends here
