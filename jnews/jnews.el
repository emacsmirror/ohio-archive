; @(#)jnews.el	1.1 2/14/90
;[Insert Gnu Copyleft here]
;
;Jnews is Yet Another News Reading Package.  Its philosophy follows that of
;DIRED.  Users read news by DIREDing their jnews subdirectory, which contains
;summary files for each interesting newsgroup.  When a user edits a summary
;file, jnews-mode is invoked.  The summary file is composed of line line
;summaries of each article, indicating the article number, date, author,
;subject, and length.  Users move to the article they want using standard Emacs
;keys, and hit 'e' to edit that message.
;
;A summary file is created by jnews, a C program that efficiently scans the
;usenet directory, which is in the /usr/spool/news hierarchy.  It does this
;incrementally so that when new articles arrive, the old article summary
;lines are not recalculated.  This happens very fast, unless you are building
;a new summary file for a large newsgroup.  The speed for compiling a new
;summary is approximately 12 articles/second on an unloaded Sun 3/60 running
;SunOS 3.5, accessing news spool files on a Sun 386i running SunOS 4.01
;using NFS on an idle ethernet.
;
;It might be preferable to make the jnews program part of the Emacs executable,
;rather than doing a call-process, but that would make it harder for people
;to try out.  If the FSF people want to integrate it in to Emacs, it shouldn't
;be difficult.  Of course, everything that the jnews program does can be done
;in Emacs lisp, but not without reading in all of each article.  Jnews only
;has to scan the first few lines to look at the header information.  Then it
;closes the file and goes on to the next one.
;
; Written by Joshua Marantz, Viewlogic Systems Inc. February, 1989.
; Additions and changes by Kim Letkeman - May, 1990.
;    Changes include: 
;       - when quitting either an article or a summary, the user is
;         popped back to the buffer at the previous level. If there
;         are no more articles, the user is popped all the way from
;         the last article to the top level.
;       - hooked up the ability to save an article into a file or a
;         mail message, much like RNEWS.
;       - hooked up the stuff to allow mailing a message, following up
;         to usenet, or posting a new message.
;       - changed jnews-goto-unread to pop right into the article which
;         saves typing and time.
;					

;----------------------Jnews Global Variables---------------------------------

(defvar jnews-program (expand-file-name "~kim/bin/jnews")
  "Pathname of jnews summary building program.")

(defvar jnews-summary-directory (concat (getenv "HOME") "/jnews/")
  "Directory in which to save newsgroup headers.")

(defvar jnews-mode-map (make-sparse-keymap)
  "Local keymap for jnews summaries.")

(defvar jnews-article-mode-map (make-sparse-keymap)
  "Local keymap for jnews articles.")

(defvar jnews-current-newsgroup nil
  "Name of buffer containing current article's newsgroup summary")
;(make-variable-buffer-local 'jnews-current-newsgroup)

(autoload 'rmail-output "rmailout"
  "Save the current message into a mail folder."
  t)

(autoload 'news-save-item-in-file "rnews"
  "Save the current message into a file."
  t)

(autoload 'news-reply "rnewspost"
  "Compose and post a reply to the current article on USENET.
While composing the reply, use \\[mail-yank-original] to yank the original
message into it."
  t)

(autoload 'news-mail-other-window "rnewspost"
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  t)

(autoload 'news-post-news "rnewspost"
  "Begin editing a new USENET news article to be posted."
  t)

(autoload 'news-mail-reply "rnewspost"
  "Mail a reply to the author of the current article.
While composing the reply, use \\[mail-yank-original] to yank the original
message into it."
  t)

(defvar rmail-last-file (expand-file-name "~/mbox.news"))


;-----------------------Utility functions of general use-----------------------

(defun describe-local-map ()
  "Print help on each key that is bound in the current mode map"
  (interactive)
  (let ((help-keys (cdr (current-local-map))))
    (pop-to-buffer "*Help*")
    (erase-buffer)
    (mapcar '(lambda (key-fun)
               (insert-char (car key-fun) 1)
               (indent-to-column 3)
               (insert (prin1-to-string (cdr key-fun)))
               (indent-to-column 26)
               (save-excursion (insert (documentation (cdr key-fun))))
               (move-to-column (- (screen-width) 1))
               (delete-region (point) (point-max))
               (insert "\n"))
            help-keys)))

(defun summarize-local-map ()
  "Print one line message in minibuffer summarizing current local map"
  (interactive)
  (let ((help-keys (cdr (current-local-map))))
    (save-excursion
      (switch-to-buffer "*Help*")
      (erase-buffer)
      (mapcar '(lambda (key-fun)
                 (insert (char-to-string (car key-fun)) ":"
                         (prin1-to-string (cdr key-fun)) " "))
              help-keys)
      (message (buffer-substring (point-min) (point-max))))))

(defun save-and-kill ()
  "Save this buffer with no backup and kill it"
  (interactive)
  (save-buffer 0)
  (kill-buffer (current-buffer)))

(defun kill-current-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun backward-line (arg)
  "Go to beginning of previous line"
  (interactive "p")
  (forward-line (- arg)))

; These next two functions were stolen from rnews.el
(defun string-subst-char (new old string)
  "Substitute character NEW for character OLD in STRING"
  (let (index)
    (setq old (regexp-quote (char-to-string old))
	  string (substring string 0))
    (while (setq index (string-match old string))
      (aset string index new)))
  string)

;;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;;; modified by tower@prep Nov 86, josh@vx Jan 89
(defun caesar-bounded-region (bound1 bound2 &optional n)
  "Caesar rotation of region defined by BOUND1 and BOUND2 by N (default 13)"
  (interactive "dmp")
  (let ((rot (cond ((or (null current-prefix-arg)
                        (not (numberp n))
                        (= (% n 26) 0))
                    13)
                   ((< n 0) (- 26 (% (- n) 26)))
                   (t (% n 26)))))
    (if (or (not (boundp 'caesar-translate-table))
            (/= (aref caesar-translate-table ?a) (+ ?a rot)))
        (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
          (message "Building caesar-translate-table...")
          (setq caesar-translate-table (make-vector 256 0))
          (while (< i 256)
            (aset caesar-translate-table i i)
            (setq i (1+ i)))
          (setq lower (concat lower lower) upper (upcase lower) i 0)
          (while (< i 26)
            (aset caesar-translate-table (+ ?a i) (aref lower (+ i rot)))
            (aset caesar-translate-table (+ ?A i) (aref upper (+ i rot)))
            (setq i (1+ i)))
          (message "Building caesar-translate-table... done")))
    (let* ((from (min bound1 bound2))
           (to (max bound1 bound2))
           (i 0)
           (str (buffer-substring from to))
           (len (length str))
           (buffer-read-only nil))
      (while (< i len)
        (aset str i (aref caesar-translate-table (aref str i)))
        (setq i (1+ i)))
      (goto-char from)
      (kill-region from to)
      (insert str))))

;-------------------------Jnews Summary mode Functions------------------------

(defun jnews ()
  "Start a Jnews session"
  (interactive)
  (dired (concat (getenv "HOME") "/jnews")))

(defun jnews-back-to-menu ()
  "Leave summary mode"
  (interactive)
  (save-and-kill)
  (pop-to-buffer (get-buffer "jnews")))

(defun jnews-read-article ()
  "Read the article referred to on the current line"
  (interactive)
  (let* ((newsgroup (file-name-nondirectory buffer-file-name))
         (news-path (concat news-path (string-subst-char ?/ ?. newsgroup) "/"))
         (message-number
          (save-excursion
            (beginning-of-line)
            (if (= (point) (point-min)) (error "No more articles"))
            (forward-char 1)
            (re-search-forward "[ ]*")
            (let ((msgno-start (point)))
              (search-forward "|")
              (buffer-substring msgno-start (- (point) 1))))))
    (if (not (file-exists-p (concat news-path message-number)))
        (progn
          (jnews-expired)
          (message "Article has expired!"))
      (progn
        (jnews-flag)
        (pop-to-buffer (concat newsgroup ":" message-number))
        (insert-file (concat news-path message-number))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (message "")
        (jnews-article-mode newsgroup)))))

(defun jnews-article-has-expired ()
  (let* ((newsgroup (file-name-nondirectory buffer-file-name))
         (news-path (concat news-path (string-subst-char ?/ ?. newsgroup)
                            "/"))
         (message-number
          (save-excursion
            (beginning-of-line)
            (if (= (point) (point-min)) (error "No more articles"))
            (forward-char 1)
            (re-search-forward "[ ]*")
            (let ((msgno-start (point)))
              (search-forward "|")
              (buffer-substring msgno-start (- (point) 1))))))
    (not (file-exists-p (concat news-path message-number)))))

(defun jnews-goto-unread ()
  "Move point to the first unseen article"
  (interactive)
  (search-forward "\n*" nil t)
  (beginning-of-line)
  (while (jnews-article-has-expired)
    (jnews-expired))
  (if (looking-at "^[*]")
      (jnews-read-article)))

(defun jnews-update ()
  "Run a C program to look for new articles in the current newsgroup"
  (interactive)
  (save-buffer 0)
  (message "Updating %s..." (file-name-nondirectory buffer-file-name))
  (call-process jnews-program
                nil nil nil
                (file-name-nondirectory buffer-file-name)
                news-path)
  (revert-buffer t t)
  (setq buffer-read-only t)
  (message "Updating %s...done" (file-name-nondirectory buffer-file-name)))

(defun jnews-add-newsgroup (newsgroup)
  "Add a new newsgroup"
  (interactive "sNewsgroup: ")
  (find-file (concat jnews-summary-directory newsgroup))
  (jnews-mode))

(defun jnews-flag ()
  "Flag the current message as previously read"
  (interactive)
   (beginning-of-line)
   (if (= (following-char) ?*)
       (let ((buffer-read-only nil))
         (delete-char 1)
         (insert-char ?  1)))
   (forward-line 1))

(defun jnews-expired ()
  "Flag the current message as previously read"
  (interactive)
   (beginning-of-line)
   (if (not (= (following-char) ?#))
       (let ((buffer-read-only nil))
         (delete-char 1)
         (insert-char ?# 1)))
   (forward-line 1))

; Do a binary search for the boundary of expired messages!!
(defun jnews-expunge-expired ()
  "Expunge all expired messages"
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (let ((buffer-read-only nil)
        (begin-deletion (point))
        (low (point))
        (high (point-max)))

    ; Get close with a binary search
    (while (> (- high low) 80)
      (goto-char (/ (+ low high) 2))
      (beginning-of-line)
      (if (jnews-article-has-expired)
          (setq low (point))
        (setq high (point))))

    ; Make sure we have not missed with linear searches
    (while (and (> (point) (point-min))
                (not (jnews-article-has-expired)))
      (forward-line -1))
    (while (and (> (point) (point-min))
                (< (point) (point-max))
                (jnews-article-has-expired))
      (forward-line 1))
    (if (> (point) (point-min))
        (delete-region begin-deletion (point)))))

(defun jnews-unflag ()
  "Unflag current message as if it had never been read"
  (interactive)
   (beginning-of-line)
   (if (= (following-char) ? )
       (let ((buffer-read-only nil))
         (delete-char 1)
         (insert-char ?* 1)))
   (forward-line 1))

;; Jnews mode is suitable only for specially formatted data.
(put 'jnews-mode 'mode-class 'special)

(defun jnews-mode ()
  "Mode for browsing through newsgroup summaries."
  (interactive)
  (if (not (eq major-mode 'jnews-mode))
      (progn
          (kill-all-local-variables)    
          (setq major-mode 'jnews-mode)
          (setq mode-name "Summaries")
          (use-local-map jnews-mode-map)
          (setq mode-line-buffer-identification '("Jnews: %17b"))
          (jnews-update)
          (run-hooks 'jnews-mode-hook))))

; Set up the jnews keymap when jnews.el is loaded
(define-key jnews-mode-map "?" 'summarize-local-map)
(define-key jnews-mode-map "b" 'scroll-down)
(define-key jnews-mode-map " " 'scroll-up)
(define-key jnews-mode-map "*" 'jnews-unflag)
(define-key jnews-mode-map "r" 'jnews-flag)
(define-key jnews-mode-map "\C-i" 'jnews-goto-unread)
(define-key jnews-mode-map "x" 'jnews-expunge-expired)
(define-key jnews-mode-map "u" 'jnews-update)
(define-key jnews-mode-map "q" 'jnews-back-to-menu)
(define-key jnews-mode-map "p" 'backward-line)
(define-key jnews-mode-map "n" 'forward-line)
(define-key jnews-mode-map "h" 'describe-local-map)
(define-key jnews-mode-map "e" 'jnews-read-article)
(define-key jnews-mode-map "a" 'jnews-add-newsgroup)


;--------------------The following commands are used in article mode-----------

(defun jnews-next-article (arg)
  "Delete the current article and look at the next one"
  (interactive "p")
  (let* ((newsgroup jnews-current-newsgroup)
         (summary-window (get-buffer-window newsgroup)))
    (kill-buffer (current-buffer))
    (if (null summary-window)
        (set-buffer newsgroup)
      (select-window summary-window))
    (forward-line (- arg 1))
    (jnews-read-article)))

(defun jnews-previous-article (arg)
  "Delete the current article and look at the previous one"
  (interactive "p")
  (jnews-next-article (- arg)))

(defun jnews-rotate-article ()
  "Caesar-rotate article"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (caesar-bounded-region (point) (point-max) 13)))

(defun jnews-back-to-summary ()
  "Kill this article's buffer and pop back into summary buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (pop-to-buffer (get-buffer jnews-current-newsgroup))
  (if (looking-at "^$")
      (jnews-back-to-menu)))

(defun jnews-mark-article ()
  "Mark the text of the current article"
  (interactive)
  (goto-char (point-min))
  (search-forward "\n\n")
  (set-mark (point-max)))

(defun jnews-article-mode (newsgroup)
  "Mode for browsing through newsgroup articles."
  (kill-all-local-variables)    
  (setq major-mode 'jnews-article-mode)
  (setq mode-name "Article")
  (use-local-map jnews-article-mode-map)
  (setq mode-line-buffer-identification '("Jnews: %17b"))
  (setq jnews-current-newsgroup newsgroup)
  (run-hooks 'jnews-article-hook))

; Set up the jnews keymap when jnews.el is loaded
(define-key jnews-article-mode-map "?" 'summarize-local-map)
(define-key jnews-article-mode-map "b" 'scroll-down)
(define-key jnews-article-mode-map " " 'scroll-up)
(define-key jnews-article-mode-map "." 'beginning-of-buffer)
(define-key jnews-article-mode-map "\C-c\C-c" 'jnews-rotate-article)
(define-key jnews-article-mode-map "q" 'jnews-back-to-summary)
(define-key jnews-article-mode-map "p" 'jnews-previous-article)
(define-key jnews-article-mode-map "n" 'jnews-next-article)
(define-key jnews-article-mode-map "o" 'news-save-item-in-file)
(define-key jnews-article-mode-map "\C-o" 'rmail-output)
(define-key jnews-article-mode-map "r" 'news-mail-reply)
(define-key jnews-article-mode-map "f" 'news-reply)
(define-key jnews-article-mode-map "a" 'news-post-news)
(define-key jnews-article-mode-map "m" 'news-mail-other-window)
(define-key jnews-article-mode-map "M" 'jnews-mark-article)
(define-key jnews-article-mode-map "h" 'describe-local-map)
