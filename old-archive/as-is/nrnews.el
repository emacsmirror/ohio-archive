;;;  rnews.el	

;;;  Copyright (C) 1987 Free Software Foundation
;;;     Author Thomas Narten, narten@cs.purdue.edu
;;;	Please send suggestions and corrections to the above address.
;;;
;;;  This file contains rnews, a GNU Emacs front end to the readnews

;; GNU Emacs is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; GNU Emacs so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.

;; Inspired by package for Unipress emacs authored by Bob Webber and Robert
;; Kirby, 1983



;;; Mode hooks:

(defmacro push (v l)
  (list 'setq l (list 'cons v l)))

(defvar rnews-newsgroup-list nil    "List of newsgroups for completion.")

(defvar rnews-auto-display nil "NIL -> automatically display message
when you go to it")

(defvar rnews-skip-marked-articles t "When going to the next article,
skip over those marked as \"typed\" ")

(defvar rnews-mode-hook nil
  "*Invoked in rnews on loading the news package*")

(defvar rnews-mode-map (make-sparse-keymap) "Keymap for rnews-mode.")

(defvar rnewspost-mode-map (make-sparse-keymap) "Keymap for rnews post mode.")

(defvar rnews-draft-number 0 "Each draft is a separate file")

(defvar rnews-newsbox-window-height 4 "how big to make the newsbox window when
resizing")

(defvar rnews-insert-prefix ">" "prefix to prepend to each line in articles
being included to a posted message")

(defvar rnews-news-directory "/usr/spool/news" 
  "Where the news files are kept")

(defvar rnews-lib-dir "/usr/local/lib/news" "news lib directory, where the 
active file and various binaries resides")

(defvar rnews-full-header nil "Nil forces only message sans header to be shown")
(defvar rnews-direction t "non nil -> going in next direction, nil -> 
previous direction")
(autoload 'mh-rmail "mh-e")
(autoload 'mh-find-path "mh-e")



;;; Entry points:
(defun rnews (&optional args)
  "Switch to .Newsbox buffer, where the subject fields of all 
news articles are kept. Here we can display, mark for deletion, read new
articles, etc. The interface is modeled roughly after the Rand MH mail system.
"
  (interactive "P")
  (rnews-switch-to-newsbox-buffer)
  (setq buffer-read-only t)
  (setq rnews-dont-auto-display t)
  (if (null rnews-newsgroup-list)
      (setq rnews-newsgroup-list (rnews-make-newsgroup-list)))
  (rnews-mode)
)

(defun rnews-switch-to-newsbox-buffer () 
  "Switch to Newsbox buffer, which contains the articles"
  (if (equal (buffer-name) "Message")
      (switch-to-buffer-other-window ".Newsbox")
    (switch-to-buffer ".Newsbox"))
  (if (equal (buffer-file-name) nil)
      (progn
	(find-alternate-file (format "%s/.Newsbox" (getenv "HOME")))
	(setq buffer-read-only t))))

(defun rnews-make-newsgroup-list ()
  "Returns a list of valid newsgroups suitable for name completion"
  (save-window-excursion
    (switch-to-buffer "*rnews-temp*" t)
    (delete-region (point-min) (point-max))
    (insert-file (format "%s/active" rnews-lib-dir))
    (goto-char (point-min))
    (let ((list nil))
      (while (not (eobp))
	(let ((start (point)))
	  (search-forward " " nil t)
	  (let ((newsgroup (buffer-substring start (- (point) 1))))
	    (push (list  newsgroup) list))
	  (next-line 1)
	  (beginning-of-line)))
      list)))

(defun rnews-prompt-for-newsgroup (prompt)
  "Prompt for a newsgroup. Return the newsgroup."
  (completing-read (format "%s Newsgroup? " prompt)
		   rnews-newsgroup-list  nil "t" nil)
)

(defun rnews-subscribe-to-group ()
  "Start subscribing to the given news group."
  (interactive)
  (let ((newsgroup (rnews-prompt-for-newsgroup "subscribe to ")))
    (save-window-excursion
      (find-file (format "%s/.newsrc" (getenv "HOME")))
      (goto-char (point-min))
      (if (search-forward newsgroup nil t)
	  (progn
	    (if (looking-at ":[ \t]*[1-9]+-[1-9]+\n")
		(message (format "You already subscribe to %s" newsgroup))
	      (if (looking-at "!")
		  (progn
		    (delete-char 1)
		    (insert-string ":")))))
	(progn
	  (beginning-of-line)
	  (insert-string newsgroup ":0-0\n")))
      (write-file (buffer-file-name)))))


(defun rnews-unsubscribe-from-group ()
  "stop subscribing to the given news group."
  (interactive)
  (let ((newsgroup (rnews-prompt-for-newsgroup "unsubscribe from ")))
    (save-window-excursion
      (find-file (format "%s/.newsrc" (getenv "HOME")))
      (goto-char (point-min))
      (if (re-search-forward (format "^%s" newsgroup) nil t)
	  (progn
	    (if (looking-at ":")
		  (progn
		    (delete-char 1)
		    (insert-string "!"))
	    (message (format "You don't currently subscribe to %s" 
			     newsgroup))))
	(progn
	  (beginning-of-line)
	  (insert-string newsgroup ":0-0\n")))
	(write-file (buffer-file-name)))))
  
(defun rnews-mode () 
  "Rnews Mode is used for reading USENET Newsgroups articles.
New readers can find additional help in newsgroup: news.announce.newusers.
All normal editing commands are turned off.
Instead, these commands are available:

\\{rnews-mode-map}
"
  (use-local-map rnews-mode-map)
  (setq major-mode 'rnews-mode)
  (setq mode-name "rnews")
  (if (and (boundp 'rnews-mode-hook) rnews-mode-hook)
      (funcall rnews-mode-hook)))

(defun rnews-incorporate-new-news () "Read news from all unread newsgroups."
  (interactive)
  (rnews-read-from-newsgroup "all")
)

(defun rnews-read-from-newsgroup (newsgroup) "Read news from the specified 
newsgroup."
  (interactive "s")
  (rnews-switch-to-newsbox-buffer)
  (let ((buffer-read-only nil))
    (goto-char (point-max))
    (save-excursion
      (narrow-to-region (dot) (dot))
      (if (equal newsgroup "all")
	  (progn
	    (message "Reading all unread news")
	    (call-process "readnews" nil t nil "-e"))
	(progn
	  (message (format "Reading from newsgroup %s" newsgroup))
 	  (call-process "readnews" nil t nil 
			"-e" "-n" newsgroup)))

      (goto-char (point-min))
      (if (looking-at "No news.")
	  (kill-line 1))
      (while (not (eobp))
	(insert "  ")
	(forward-line 1))
      (widen)
      )
    (save-excursion
      (end-of-line)
      (if (eobp)
	  (message "No new news.")
	))))

(defun rnews-show-article-full-header () "Show the current article with headers"
  (interactive)
  (let ((rnews-full-header "t"))
    (rnews-show-article)))

(defun rnews-show-article () "Show the current article"
  (interactive)
  (rnews-switch-to-show-buffer)
  (let ((article-filename (format "%s/%s" rnews-news-directory 
				  (rnews-get-article-file-name))))
    (if (equal buffer-file-name article-filename)
	()
      (progn 
	(setq buffer-read-only nil)
	(delete-region (point-min) (point-max))
	(insert-file-contents   article-filename )
	(rnews-mark-article-shown)
	(rnews-set-message-mode-line)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	)))
  (goto-char (point-min))
  (if (null rnews-full-header)
      (progn
	(search-forward "\n\n")
	(rnews-mode)
	(recenter 0)
	)
    )
  (if rnews-dont-auto-display
      (progn 
	(setq rnews-dont-auto-display nil)
	(rnews-resize-windows)))
  (rnews-switch-to-newsbox-buffer)
  (recenter (/ (window-height) 2))
  )

(defun rnews-mark-article-shown () "mark the current article as having
been displayed."
  (interactive)
  (save-window-excursion
      (rnews-switch-to-newsbox-buffer)
      (let ((buffer-read-only nil))
	(beginning-of-line)
	(delete-char 1)
	(insert-string "T")
	(beginning-of-line)
	)))

(defun rnews-delete-article (a) "delete the current article (without looking
at it)"
  (interactive "P")
  (rnews-switch-to-newsbox-buffer)
  (let ((count (prefix-numeric-value a))
	  (buffer-read-only nil))
      (while (> count 0)
	(beginning-of-line)
	(delete-char 1)
	(insert-string "D")
	(beginning-of-line)
	(setq count (- count 1))
	(rnews-move-to-next-article 1))))
      
 

(defun rnews-unmark-article () "unmark the current article as having
been displayed."
  (interactive)
  (save-window-excursion
    (rnews-switch-to-newsbox-buffer)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert-string " ")
      (beginning-of-line)
      )))



(defun rnews-switch-to-show-buffer () "Go to the window that the article
is to be shown in. "
  (if (equal (buffer-name) "Message")
      ()
    (switch-to-buffer-other-window "Message"))
)

(defun rnews-sync-current-article () 
  "Syncronize the article in the show buffer with the current article in the 
newsbox"
  (save-window-excursion
    (rnews-switch-to-newsbox-buffer)
    (rnews-show-article)
    ))

(defun rnews-get-article-file-name () "Get the file name of the article
that we are currently at"
  (save-excursion
    (set-buffer ".Newsbox")
    (beginning-of-line)
    (forward-char 2)
    (copy-to-buffer "*rnews-temp*" (dot) 
		    (progn (search-forward " ") (backward-char 1) (dot)))
    (beginning-of-line)
    (set-buffer "*rnews-temp*")
    (replace-string "." "/")
    (buffer-substring (point-min) (point-max))
    ))
  
(defun rnews-move-to-next-article (&optional a) "move to  the next article, 
moving in the appropriate direction"
  (interactive "P")
  (if rnews-direction
      (rnews-next-unmarked-article a)
    (rnews-previous-unmarked-article a)))

(defun rnews-next-article (&optional a) "Position the curser at the next article"
  (interactive "P")
  (rnews-switch-to-newsbox-buffer)
  (setq rnews-direction t)
  (beginning-of-line)
  (forward-char 1)
  (if rnews-skip-marked-articles
      (progn
	(re-search-forward "^  " nil "true" (prefix-numeric-value a))
	(cond ((eobp) (progn
			(previous-line 1)
			(error "No more articles")))))
    (progn
	(re-search-forward "^[T ] " nil "true" (prefix-numeric-value a))
	(if (eobp) (progn
		     (previous-line 1)
		     (error "No more articles") ))))

  (beginning-of-line)
  (if (null rnews-dont-auto-display)
   (progn
     (rnews-show-article))))

(defun rnews-previous-article (a) "Position the curser at the next article"
  (interactive "P")
  (rnews-switch-to-newsbox-buffer)
  (setq rnews-direction nil)
  (beginning-of-line)
  (if rnews-skip-marked-articles
      (if (re-search-backward "^ " nil "true" (prefix-numeric-value a))
	  ()
	  (error "No Previous Article"))
    (progn
	(if (re-search-backward "^[T ] " nil "true" (prefix-numeric-value a))
	    ()
	    (error "No more articles"))))

  (if (null rnews-dont-auto-display)
   (progn
     (rnews-show-article))))


 (defun rnews-previous-unmarked-article (a) 
  "Position the curser at the previous unmarked article"
  (interactive "P")
  (let ((rnews-skip-marked-articles (null rnews-skip-marked-articles)))
    (rnews-previous-article (prefix-numeric-value a))))

(defun rnews-next-unmarked-article (a) 
  "Position the curser at the previous unmarked article"
  (interactive "P")
  (let ((rnews-skip-marked-articles (null rnews-skip-marked-articles)))
    (rnews-next-article (prefix-numeric-value a))))


(defun rnews-resize-windows () "Resize the windows to get small window
of subjects, large window for messages"
  (interactive)
  (rnews-show-article)
  (rnews-switch-to-newsbox-buffer)
  (shrink-window (- (- (window-height) 1) rnews-newsbox-window-height))
  )


(defun rnews-compact () "Delete all articles in .Newsbox except those that 
haven't been marked in anyway. We may want to view those later."
  (interactive)
  (rnews-switch-to-newsbox-buffer)
  (message "compacting newsbox")
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "D ")
	    (kill-line 1)
	  (forward-line 1))))
    (message "")))

(defun rnews-scroll-message () "Go to next page of message"
  (interactive)
  (rnews-switch-to-newsbox-buffer)
  (scroll-other-window))


(defun rnews-forward-via-mail () "Forward the current article via mail"
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send)
  (rnews-sync-current-article)
  (goto-char (point-max))
  (insert-string "------- Forwarded News Article " 
		 (rnews-get-article-id) "\n\n")
  (insert-file (format "%s/%s" rnews-news-directory 
		       (rnews-get-article-file-name)))
  (goto-char (point-max))
  (insert-string "\n------- End of Forwarded News Article\n")
  (goto-char (point-min))
  (search-forward "Subject:")
  (end-of-line)
  (insert-string (rnews-get-subject-line))
  (goto-char (point-min))
  (end-of-line)
  (set-buffer-modified-p nil)
)
(defun rnews-get-article-id () "Get the id associated with an the article"
  (save-excursion
    (set-buffer "Message")
    (goto-char (point-min))
    (search-forward "Message-ID:")
    (buffer-substring (dot) (progn (end-of-line) (dot)))
    ))

(defun rnews-get-subject-line () "Get the subject line of an article"
  (save-excursion
    (set-buffer "Message")
    (goto-char (point-min))
    (re-search-forward "[Ss]ubject:[ \t]*")
    (buffer-substring (dot) (progn (end-of-line) (dot)))))

(defun rnews-reply-via-mail () "reply to the current article via mail"
  (interactive)
  (rnews-sync-current-article)
  (mh-find-path)
  (call-interactively 'mh-send)
  (goto-char (point-min))
  (re-search-forward "[Tt]o:")
  (end-of-line)
  (insert-string (rnews-get-from-field))
  (goto-char (point-min))
  (re-search-forward "[Ss]ubject:")
  (end-of-line)
  (insert-string "Re: " (rnews-get-subject-line))
  (search-forward "\n\n")
  (set-buffer-modified-p nil))

(defun rnews-get-from-field () "Get the from line from the article"
  (save-excursion
    (set-buffer "Message")
    (goto-char (point-min))
    (re-search-forward "[Ff]rom:[ \t]*")
    (buffer-substring (dot) (progn (end-of-line) (dot)))
    ))

(defun rnews-get-news-groups () "Get the newsgroups 
the current article goes to"
  (save-excursion
    (set-buffer "Message")
    (goto-char (point-min))
    (re-search-forward "[Nn]ewsgroups:[ \t]*")
    (buffer-substring (dot) (progn (end-of-line) (dot)))
    ))
  
(defun rnews-get-draft () "Get a new buffer for posting an article"
  (setq rnews-draft-number (+ rnews-draft-number 1))
  (find-file (format "%s/.draft_%d" (getenv "HOME") rnews-draft-number))
  (erase-buffer)
  (rnewspost-mode)
)
(defun rnewspost-mode () "turn on rnews mode bindings"
  (text-mode)
  (use-local-map rnewspost-mode-map)
  (setq major-mode 'rnewspost-mode)
  (setq mode-name "rnewspost")
  (if (and (boundp 'rnewspost-mode-hook) rnewspost-mode-hook)
      (funcall rnewspost-mode-hook)))
(defun rnews-compose-article () "Compose an article to be posted"
  (interactive)
  (rnews-get-draft)
  (goto-char (point-min))
  (insert-string "Subject: \nNewsgroups: \n\n")
  (goto-char (point-min))
  (end-of-line)
)

(defun rnews-post-this-article () "Post the article in this buffer"
  (interactive)
  (message "posting article...")
  (let ((filename (buffer-file-name)))
    (write-file filename)
    (set-buffer "*rnews-temp*")
    (erase-buffer)
    (call-process (format "%s/inews" rnews-lib-dir)
		      filename  "*rnews-temp*" nil "-h")
    (if (= (buffer-size) 0)
	  (message "Article Posted")
	(progn
	  (switch-to-buffer "*rnews-temp*")
	  (error "article not posted")))))

(defun rnews-compose-followup () "Compose a followup message to the current
article"
  (interactive)
  (rnews-sync-current-article)
  (rnews-get-draft)
  (goto-char (point-min))
  (insert-string "Subject: " (rnews-get-subject-line) "\n")
  (goto-char (point-min))
  (re-search-forward "Subject:[ \t]*")
  (if (looking-at "[rR]e:")
      ()
    (insert-string "Re: "))
  (goto-char (point-max))
  (insert-string "Newsgroups: " (rnews-get-news-groups) "\n\n"))
  
(defun rnews-set-message-mode-line () "Set the mode line for an article
being displayed. Mode line contains sender and subject."
  (setq mode-line-format (format "Message from: %s on %s----%%p%%-" 
				 (rnews-get-from-field)
				 (rnews-get-subject-line)))
)
(defun rnews-insert-article ()
  "Insert the article being replied to in the current message"
  (interactive)
  (rnews-sync-current-article)
  (save-excursion
    (insert-string "In article " (rnews-get-article-id) " " 
		   (rnews-get-from-field) " writes:\n")
    (narrow-to-region (dot) (dot))
    (insert-string (rnews-get-article-text))
    (goto-char (point-min))
    (while (not (eobp)) (progn
			  (beginning-of-line)
			  (insert-string rnews-insert-prefix)
			  (next-line 1)))
    (widen)
))

(defun rnews-get-article-text () "Get the text of the article"
  (save-excursion
    (set-buffer "Message")
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (buffer-substring (dot) (progn (goto-char (point-max)) (dot)))
    ))

(defun rnews-toggle-next-article-mode () "toggle scrolling modes between 
skipping typed articles and showing them"
  (interactive)
  (if  rnews-skip-marked-articles
      (progn
	(setq rnews-skip-marked-articles nil)
	(message "skipping typed articles"))
    (progn
      (setq rnews-skip-marked-articles t)
      (message "Not skipping typed articles" ))))

(defun rnews-newsbox-mode () "change mode of newsbox scanning"
  (interactive)
  (if rnews-dont-auto-display
      (rnews-show-article)
       (progn
	 (setq rnews-dont-auto-display t)
	 (rnews-switch-to-newsbox-buffer)
	 (delete-other-windows))))

(defun rnews-next-article-in-digest ()
  "Go to the next article in a digest"
  (interactive)
  (rnews-switch-to-show-buffer)
  (let ((case-fold-search nil))
    (if (not (search-forward "\nFrom:" nil t))
	  (error "No more messages")))
  (re-search-backward "^\n")
  (next-line 1)
  (recenter 0)
  (re-search-forward "^\n")
  (rnews-switch-to-newsbox-buffer))



(define-key rnewspost-mode-map "\C-c\C-s" 'rnews-post-this-article)
(define-key rnewspost-mode-map "\C-c\C-i" 'rnews-insert-article)
(define-key rnews-mode-map "i" 'rnews-incorporate-new-news)
(define-key rnews-mode-map "t" 'rnews-show-article)
(define-key rnews-mode-map "T" 'rnews-show-article-full-header)
(define-key rnews-mode-map "u" 'rnews-unmark-article)
(define-key rnews-mode-map "N" 'rnews-next-article)
(define-key rnews-mode-map "n" 'rnews-next-unmarked-article)
(define-key rnews-mode-map "P" 'rnews-previous-article)
(define-key rnews-mode-map "p" 'rnews-previous-unmarked-article)
(define-key rnews-mode-map "." 'rnews-resize-windows)
(define-key rnews-mode-map "d" 'rnews-delete-article)
(define-key rnews-mode-map "\C-c\C-c" 'rnews-compact)
(define-key rnews-mode-map " " 'rnews-scroll-message)
(define-key rnews-mode-map "r" 'rnews-reply-via-mail)
(define-key rnews-mode-map "f" 'rnews-forward-via-mail)
(define-key rnews-mode-map "c" 'rnews-compose-article)
(define-key rnews-mode-map "F" 'rnews-compose-followup)
(define-key rnews-mode-map "\C-c\C-i" 'rnews-insert-article)
(define-key rnews-mode-map "?" 'describe-mode)
(define-key rnews-mode-map "U" 'rnews-unsubscribe-from-group)
(define-key rnews-mode-map "s" 'rnews-subscribe-to-group)
(define-key rnews-mode-map "m" 'rnews-newsbox-mode)
(define-key rnews-mode-map "M" 'rnews-toggle-next-article-mode)
(define-key rnews-mode-map "l" 'rnews-next-article-in-digest)
