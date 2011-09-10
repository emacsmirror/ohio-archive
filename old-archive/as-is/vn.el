;;;From: karl@tut.cis.ohio-state.edu (Karl Kleinpaste)
;;;Newsgroups: comp.emacs
;;;Subject: Updated vn.el
;;;Date: 7 Dec 87 14:14:40 GMT
;;;Organization: OSU

;;;As I mentioned last week, I've been doing some things with the vn.el
;;;newsreader which was posted to comp.emacs a few weeks back.  It had a
;;;number of bugs which made it almost impossible for me to use, but I've
;;;taken care of all the outright bugs I could find.  In the process, I
;;;reformatted it, because its existing formatting (indentation) looked
;;;pretty pathological to me as a relative novice to Emacs lisp hacking.
;;;Anyhow, this is the complete new package, not diffs.  The newslist.c
;;;that was in the author's previous posting will do fine; I didn't
;;;notice any bugs in it.

;;;One small request to people creating new modes for GNU Emacs: please
;;;don't do anything particularly abnormal with keymaps when you define a
;;;new mode.  Vn was using C-h as a backward-page character of some sort;
;;;it really threw me off when I wanted to use "help" commands.  I've
;;;changed that particular item to use, I believe, DEL.

;;;There are a few things that could afford to be done better, like
;;;getting back to the correct window following a reply or followup, and
;;;perhaps setting vn-header-on back to nil if it had previously been nil
;;;but had to be set to t for the purpose of following up.  I've just
;;;made it workable for myself, not perfect.

;;; USENET news reader for gnu emacs
;; Copyright (C) 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; written by Steve Murphy, Midvale, Utah, Oct. 1987
;; Dedicated to Ryan (He and this code share Birthdays)

;; I would suggest that those dissatified with the poor coding,
;; hacked up kloojes, etc. dedicate their energies to fixing it rather
;; than complaining!

;; This is an obvious imitation of vn, written in C by Bob McQueer
;; This 'plagiarism' is actually meant to be a tribute, as vn was/is by far
;; my news reader of choice.

;; I left out some of the functionality, like printing, 'selected' functions,
;; and such-like, because of duplicate functionality in gnu emacs, or
;; just laziness or lack of time. Note the lack of 'digestify'.
;; Also note that sending mail to the author of the current article
;; works, but... when you return things are awry. I believe this to be
;; the fault of rnewspost.el and mail.el. Perhaps I'll post something
;; later on this...

;; the c-code for 'newslist' is part of this package-- I didn't feel expert
;; enough to generate the code in lisp. It was easier for me to come out
;; with a small program that would generate the pages that vn does, showing
;; the article numbers, groups, subject lines, lines, and author. All the
;; data vn-mode needs to update the .newsrc is in the text of the vn buffer.
;; speed is comparable to vn's in generating this list.

;; The concept of 'pages' in vn was lost in the translation--purposely
;; on my part, as I personally like just a long list, and to have as much
;; visible on a page as possible. For those enthusiasts who like it the
;; other way, there are 'narrow' functions that could be put to use, but
;; I didn't feel it was worth it.

;; Two different modes are used, one for paging thru the headers, the other
;; for actually reading the article. The package cleans up after itself, 
;; leaving the windows the way they were before the package was invoked.

;; dired and rnews were freely copied here, and several 'autoloads' are
;; included from these sources. Why TOTALLY re-invent the wheel?

;; why did I do it? 1. Someone else indicated a wish for something like this,
;; 2. I wanted it myself, 3. The Challenge of It (why climb Mt. Everest?)
;; and lastly, 4. It was educational.

;; Enjoy. Ps. If the Gnu people want this, they are welcome to it...
;; if they don't.. no sweat, I don't blame them.
;; I'm giving it to them nevertheless, and thus to all who happen on it.

(provide 'vn)
(require 'mail-utils)

(autoload 'rmail-output "rmailout" 
"Append this message to Unix mail file named FILE-NAME" t)

(autoload 'news-convert-format "rnews" "get rid of the headers" t)
(autoload 'string-subst-char "rnews" "with args new-char old-char string,
replace old-char with new-char in string" t)
(autoload 'news-mail-other-window "rnewspost"
 "Send mail in another window. While composing the message, use
\\[mail-yank-original] to yank the original message into it."
 t)
(autoload 'news-caesar-buffer-body "rnews" "rotate the article" t)

(autoload 'news-mail-reply "rnewspost"
 "Mail a reply to the author of the current article. While composing
 the reply, use \\[mail-yank-original] to yank the original message into it."
 t)

(defvar news-startup-file "$HOME/.newsrc" "Contains ~/.newsrc")
(defvar newsbox-dir "$HOME/.newsbox" "The newsbox directory")
(defvar vn-buffer nil "which buffer vn is running in")
(defvar vn-read-buffer nil "which buffer the article is read in")
(defvar vn-initial-window-config nil "how it was before we mucked..")

(defun vn-find-buffer ()
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (eq major-mode 'vn-mode)
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(create-file-buffer "*VN-Buffer*"))))

(defun vn-read-find-buffer ()
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (eq major-mode 'vn-read-mode)
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(create-file-buffer "*Usenet Article*"))))

(defun vn-readin (buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (widen)
      (erase-buffer)
      (message "Checking for unread news...")
      (call-process "newslist" nil buffer nil)
      (message "Checking for unread news...done.")
      (if (not (zerop (buffer-size)))
	  (goto-char (point-min))))))

(defvar vn-window nil "the window vn is running in")

(defun vn ()
  "A simulation of vn for emacs.
Dired displays a list of unread articles.
You can move around in it with the usual commands.
Type `h' after entering vn for more info."
  (interactive)
  (setq vn-initial-window-config (current-window-configuration))
  (switch-to-buffer (vn-noselect))
  (setq vn-buffer (current-buffer))
  (vn-del-non-groups)
  (if (zerop (buffer-size))
      (progn
	(kill-buffer (current-buffer))
	(set-window-configuration vn-initial-window-config)
	(message "No News! (Good News?)"))
    (setq vn-window (selected-window))
    (cd (substitute-in-file-name newsbox-dir))))

(defun vn-other-window ()
  "VN simulation for emacs.
Like M-x vn but selects in another window."
  (interactive)
  (switch-to-buffer-other-window 
    (vn-noselect))
  (setq vn-window (selected-window))
  (setq vn-buffer (current-buffer)))

(defun vn-noselect ()
  "Like M-x vn but returns the vn buffer 
as value, does not select it."
  (let ((buffer (vn-find-buffer)))
    (save-excursion
      (set-buffer buffer)
      (vn-readin buffer)
      (vn-move-to-article)
      (vn-mode))
    buffer))

(defun vn-read-other-window ()
  "VN simulation for emacs.
Like M-x vn but selects in another window."
  (interactive)
  (switch-to-buffer-other-window 
    (vn-read-noselect))
  (setq vn-read-buffer (current-buffer))
  (cd (substitute-in-file-name newsbox-dir)))

(defun vn-read-noselect ()
  "Like M-x vn but returns the vn read buffer 
as value, does not select it."
  (let ((buffer (vn-read-find-buffer)))
    (save-excursion
      (set-buffer buffer)
      (vn-read-mode))
    buffer))

(defvar vn-mode-map nil 
  "Local keymap for vn-mode buffers.")
(defvar vn-read-mode-map nil 
  "Local keymap for vn-mode buffers.")

(if vn-mode-map
    nil
  (setq vn-mode-map (make-keymap))
  (suppress-keymap vn-mode-map)
  (define-key vn-mode-map " " 'vn-read-one-article)
  (define-key vn-mode-map "r" 'vn-read-one-article)
  (define-key vn-mode-map "R" 'vn-read-all-articles)
  (define-key vn-mode-map "q" 'vn-quit)
  (define-key vn-mode-map "p" 'vn-up-article)
  (define-key vn-mode-map "n" 'vn-down-article)
  (define-key vn-mode-map "k" 'vn-up-article)
  (define-key vn-mode-map "j" 'vn-down-article)
  (define-key vn-mode-map "\ep" 'vn-prev-group)
  (define-key vn-mode-map "\en" 'vn-next-group)
  (define-key vn-mode-map "\C-m" 'vn-next-page)
  (define-key vn-mode-map "\177" 'vn-prev-page)
  (define-key vn-mode-map "s" 'vn-save-article)
  (define-key vn-mode-map "S" 'vn-save-all-articles)
  (define-key vn-mode-map "w" 'vn-update-rc-to-cursor)
  (define-key vn-mode-map "W" 'vn-update-rc-to-eogroup)
  (define-key vn-mode-map "\C-w" 'vn-update-rc-all)
  (define-key vn-mode-map "m" 'news-mail-other-window)
  (define-key vn-mode-map "a" 'news-post-news)
  (define-key vn-mode-map "u" 'vn-unsubscribe-group)
  (define-key vn-mode-map "h" 'vn-toggle-header-display))

(if vn-read-mode-map
    nil
  (setq vn-read-mode-map (make-keymap))
  (suppress-keymap vn-read-mode-map)
  (define-key vn-read-mode-map "n" 'vn-next-article)
  (define-key vn-read-mode-map "p" 'vn-prev-article)
  (define-key vn-read-mode-map "q" 'vn-abort-reading)
  (define-key vn-read-mode-map "Q" 'vn-abort-reading-next-group)
  (define-key vn-read-mode-map "." 'vn-beginning-of-article)
  (define-key vn-read-mode-map "e" 'vn-end-of-article)
  (define-key vn-read-mode-map "\C-m" 'vn-scroll-line)
  (define-key vn-read-mode-map "m" 'news-mail-other-window)
  (define-key vn-read-mode-map "a" 'news-post-news)
  (define-key vn-read-mode-map "r" 'vn-news-mail-reply)
  (define-key vn-read-mode-map "f" 'vn-news-reply)
  (define-key vn-read-mode-map "s" 'vn-save-this-article)
  (define-key vn-read-mode-map " " 'vn-next-page)
  (define-key vn-read-mode-map "\177" 'vn-prev-page)
  (define-key vn-read-mode-map "z" 'vn-toggle-rot)
  (define-key vn-read-mode-map "h" 'vn-toggle-header))

;; vn mode is suitable only for specially formatted data.
(put 'vn-mode 'mode-class 'special)
(put 'vn-read-mode 'mode-class 'special)

(defun vn-mode ()
  "Mode for reading Usenet News in a vn sort of way.
\\{vn-mode-map}"
  (kill-all-local-variables)    
  (setq major-mode 'vn-mode)
  (setq mode-name "Vn")
  (setq mode-line-buffer-identification '("Emacs: %17b"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (use-local-map vn-mode-map)
  (run-hooks 'vn-mode-hook))

(defun vn-read-mode ()
  "Mode for reading Usenet News articles in a vn sort of way.
\\{vn-read-mode-map}"
  (kill-all-local-variables)    
  (setq major-mode 'vn-read-mode)
  (setq mode-name "Vn-Read")
  (setq mode-line-buffer-identification '("VN-Read: %17b"))
  (setq case-fold-search nil)
  (use-local-map vn-read-mode-map)
  (run-hooks 'vn-read-mode-hook))

(defun vn-move-to-article ()
  "Move cursor to nearest following article"
  (if (not (zerop (buffer-size)))
      (progn
	(beginning-of-line)
	(while (and (not (looking-at "  [0-9]"))
		    (not (eobp)))
	  (forward-line 1)))))

(defun vn-current-group ()
  "return the current group of this article in path format"
  (save-excursion
    (let (dirname)
      (setq dirname (vn-current-group-dot))
      (string-subst-char ?/ ?. dirname))))

(defun vn-current-group-dot () 
  "return the current group of this article in dotted notation"
  (save-excursion
    (progn
      (set-buffer vn-buffer)
      (search-backward "==== Group: ")
      (skip-chars-forward "^:")
      (forward-char 2)
      (let (dirname)
	(setq dirname (buffer-substring (point)
					(progn
					  (skip-chars-forward "^ ")
					  (point))))
	dirname))))

(defun vn-current-article ()
  "get number of current article"
  (save-excursion
    (set-buffer vn-buffer)
    (skip-chars-forward "> ")
    (let ((beg (point)))
      (skip-chars-forward "0123456789")
      (buffer-substring beg (point)))))

(defun vn-current-article-file ()
  "return full path of article file"
  (concat "/usr/spool/news/" (vn-current-group) "/" (vn-current-article)))

(defun vn-up-article ()
  "move cursor to previous article line"
  (interactive)
  (re-search-backward "^  [0-9]" nil t)
  (beginning-of-line)
)

(defun vn-down-article ()
  "move cursor to next article line (skip group headers)"
  (interactive)
  (forward-line)
  (beginning-of-line)
  (re-search-forward "^  [0-9]" nil t)
  (beginning-of-line)
)

(defun vn-news-mail-reply ()
  "write mail to an article's originator"
  (interactive)
  (if (not vn-header-on)
      (vn-toggle-header))
  (news-mail-reply)
)

(defun vn-news-reply ()
  "write a followup article"
  (interactive)
  (if (not vn-header-on)
      (vn-toggle-header))
  (news-reply)
)

(defun vn-quit (arg)
  "end vn session and update .newsrc file"
  (interactive 
    "nUpdating .newsrc: 0=do not, 1=all, 2=to cursor, 3=to cursor group: ")
  (let ((newsrcbuf (find-file-noselect
		     (substitute-in-file-name news-startup-file))))
    (if (equal arg 1) (update-all-groups newsrcbuf))
    (if (equal arg 2) (update-all-groups-to-cursor newsrcbuf))
    (if (equal arg 3) (update-all-groups-to-eogroup newsrcbuf))
    (set-buffer newsrcbuf)
    (save-buffer)
    (kill-buffer (current-buffer))
    (kill-buffer vn-buffer)
    (if vn-read-buffer
	(kill-buffer vn-read-buffer))
    (set-window-configuration vn-initial-window-config)
    ))

(defun update-all-groups (newsrcbuf)
  "update all groups"
  (vn-add-missing-groups newsrcbuf)
  (goto-char (point-max))
  (forward-line -1)
  (vn-update-current-group-to-cursor newsrcbuf)
  (re-search-backward "^.... Group:")
  (forward-line -1)
  (while (not (bobp))
    (vn-update-current-group-to-cursor newsrcbuf)
    (re-search-backward "^.... Group:")
    (forward-line -1))
  (vn-unsubscribe-groups newsrcbuf))

(defun update-all-groups-to-cursor (newsrcbuf)
  "update all groups"
  (vn-add-missing-groups newsrcbuf)
  (vn-update-current-group-to-cursor newsrcbuf)
  (re-search-backward "^.... Group:")
  (forward-line -1)
  (while (not (bobp))
    (vn-update-current-group-to-cursor newsrcbuf)
    (re-search-backward "^.... Group:")
    (forward-line -1))
  (vn-unsubscribe-groups newsrcbuf))
	
(defun update-all-groups-to-eogroup (newsrcbuf)
  "update all groups"
  (vn-add-missing-groups newsrcbuf)
  (beginning-of-line)
  (if (looking-at "^.... Group:")
      (forward-line 1))
  (if (not (re-search-forward "^.... Group" nil t))
      (progn
	(goto-char (point-max))
	(forward-line -1))
    (forward-line -1))
  (vn-update-current-group-to-cursor newsrcbuf)
  (re-search-backward "^.... Group:" nil t)
  (forward-line -1)
  (while (not (bobp))
    (vn-update-current-group-to-cursor newsrcbuf)
    (re-search-backward "^.... Group:" nil t)
    (forward-line -1))
  (vn-unsubscribe-groups newsrcbuf))
	
(defun vn-del-non-groups ()
  "delete groups that aren't in active"
  (save-excursion
    (if (not (zerop (buffer-size)))
	(let ((newsrcbuf (find-file-noselect
			   (substitute-in-file-name news-startup-file)))
	      last-art last-group)
	  (goto-char 0)
	  (while (re-search-forward "---- Group:" nil t)
	    (forward-char 1)
	    (setq last-group (buffer-substring (point)
					       (progn
						 (skip-chars-forward "^ ")
						 (point))))
	    (set-buffer newsrcbuf)
	    (if (re-search-forward (concat last-group ":") nil t)
		(progn
		  (beginning-of-line)
		  (kill-line 1)
		  (save-buffer)
		  (kill-buffer newsrcbuf)))
	    (set-buffer vn-buffer)
	    (beginning-of-line)
	    (kill-line 1))))))
  
(defun vn-add-missing-groups (newsrcbuf) "plug in the missing groups"
  (let (last-art last-group)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "++++ Group:" nil t)
	(forward-char 1)
	(setq last-group (buffer-substring (point)
					   (progn
					     (skip-chars-forward "^ ")
					     (point))))
	(set-buffer newsrcbuf)
	(goto-char (point-max))
	(insert last-group)
	(insert ": 0\n")
	(set-buffer vn-buffer)
	(toggle-read-only)
	(beginning-of-line)
	(kill-line 1)
	(toggle-read-only)))))

(defun vn-unsubscribe-groups (newsrcbuf)
  "mark groups unsubscribed"
  (let (last-art last-group)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "---- Group:" nil t)
	(forward-char 1)
	(setq last-group (buffer-substring (point)
					   (progn
					     (skip-chars-forward "^ ")
					     (point))))
	(set-buffer newsrcbuf)
	(if (re-search-forward (concat last-group ":") nil t)
	    (progn
	      (delete-char -1)
	      (insert "!")))
	(set-buffer vn-buffer)))))

(defun vn-update-current-group-to-cursor (newsrcbuf)
  "update the .newsrc file"
  (let (last-art last-group)
    (save-excursion
      (set-buffer vn-buffer)
      (if (not (bolp))
	  (beginning-of-line))
      (if (not (bobp))
	  (progn
	    (while (and (looking-at ".... Group:")
			(not (bobp)))
	      (forward-line -1))
	    (if (not (bobp))
		(progn
		  (setq last-art (vn-current-article))
		  (setq last-group (vn-current-group-dot))
		  (message "updating %s-%s" last-group last-art )
		  (set-buffer newsrcbuf)
		  (goto-char 0)
		  (if (re-search-forward (concat "^" last-group "[:!]") nil t)
		      (progn
			(forward-char 1)
			(if (looking-at "0")
			    (progn
			      (delete-char 1)
			      (insert "1-0")
			      (forward-char -3)))
			(skip-chars-forward "^-")
			(forward-char 1)
			(delete-region (point) (progn (end-of-line) (point)))
			(insert last-art))
		    (message "Couldn't find %s" (concat last-group))))))))))

(defun vn-prev-group ()
  "jump to first article line in previous group"
  (interactive)
  (search-backward "==== Group: ")
  (search-backward "==== Group: ")
  (vn-move-to-article))

(defun vn-next-group ()
  "jump to first article line in next group"
  (interactive)
  (search-forward "==== Group: ")
  (vn-move-to-article))

(defun vn-read-article ()
  "open up another buffer to view article in"
  (interactive)
  (beginning-of-line)
  (toggle-read-only)
  (delete-char 1)
  (insert ">")
  (toggle-read-only)
  (beginning-of-line)
  (let ((filename (vn-current-article-file)))
    (vn-read-other-window)
    (erase-buffer)
    (let ((start (point)))
      (insert-file-contents filename)
      (if (not vn-header-on)
	  (news-convert-format))
      (goto-char start)
      (if vn-header-on
	  (forward-line 1)))))

(defvar vn-read-sequence nil "indicates we are in read-all mode")

(defun vn-read-one-article ()
  "open up another buffer to view article in"
  (interactive)
  (setq vn-read-sequence nil)
  (vn-read-article))

(defun vn-read-all-articles ()
  "open up another buffer and view all the articles
serially in it from current article to end of group"
  (interactive)
  (setq vn-read-sequence t)
  (vn-read-article))

(defun vn-save-article ()
  "save the current article to a disk file"
  (interactive)
  (beginning-of-line)
  (toggle-read-only)
  (delete-char 1)
  (insert ">")
  (toggle-read-only)
  (beginning-of-line)
  (call-interactively 'vn-save-article-2 nil)
  (beginning-of-line)
  (toggle-read-only)
  (delete-char 1)
  (insert " ")
  (toggle-read-only)
  (beginning-of-line))

(defun vn-save-article-2 (file)
  "save the current article to a disk file"
  (interactive "FCopy to File Name:")
  (copy-file (concat "/usr/spool/news/" (vn-current-group) 
		     "/" (vn-current-article)) file 1 nil))

(defun vn-save-all-articles (file)
  "save all the articles from current article in this group to a disk file"
  (interactive "FSave them (from current article to end of group to file: ")
  (let ((newsrcbuf (find-file-noselect file))
	(curgroup (vn-current-group))
	currartfile)
    (while (and (equal curgroup (vn-current-group))
		(not (eobp)))
      (setq currartfile (vn-current-article-file))
      (set-buffer newsrcbuf)
      (goto-char (point-max))
      (insert-file currartfile)
      (set-buffer vn-buffer)
      (vn-down-article))
    (set-buffer newsrcbuf)
    (save-buffer)
    (kill-buffer newsrcbuf)))

(defun vn-update-rc-to-cursor ()
  "all articles on or above cursor will be marked as read in the .newsrc file"
  (interactive)
  (save-excursion
    (let ((newsrcbuf (find-file-noselect
		       (substitute-in-file-name news-startup-file))))
      (update-all-groups-to-cursor newsrcbuf)
      (set-buffer newsrcbuf)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun vn-update-rc-to-eogroup ()
  "all articles on or above the current group
will be marked as read in the .newsrc file"
  (interactive)
  (save-excursion
    (let ((newsrcbuf (find-file-noselect
		       (substitute-in-file-name news-startup-file))))
      (update-all-groups-to-eogroup newsrcbuf)
      (set-buffer newsrcbuf)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun vn-update-rc-all()
  "all articles scheduled to be read will
be marked as read in the .newsrc file"
  (interactive)
  (save-excursion
    (let ((newsrcbuf (find-file-noselect
		       (substitute-in-file-name news-startup-file))))
      (update-all-groups newsrcbuf)
      (set-buffer newsrcbuf)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun vn-unsubscribe-group ()
  "toggle between subscription/unsubscription of/from current group"
  (interactive)
  (save-excursion
    (if (re-search-backward "^.... Group: " nil t)
	(progn
	  (re-search-forward "^.... Group: " nil t)
	  (beginning-of-line)
	  (if (looking-at "====")
	      (progn
		(toggle-read-only)
		(delete-char 4)
		(insert "----")
		(toggle-read-only))
	    (toggle-read-only)
	    (delete-char 4)
	    (insert "====")
	    (toggle-read-only))))))

(defun vn-toggle-header-display ()
  "Toggle showing ALL the header lines"
  (interactive)
  (if vn-header-on
      (setq vn-header-on nil)
    (setq vn-header-on t)))

;; ------------reader mode stuff------------------------

(defun vn-next-article ()
  "back to the vn buffer if no further articles"
  (interactive)
  (erase-buffer)
;  (set-buffer vn-buffer)
;  (select-window vn-window)
  (pop-to-buffer vn-buffer)
  (let ((curgroup (vn-current-group)))
    (beginning-of-line)
    (toggle-read-only)
    (delete-char 1)
    (insert " ")	
    (toggle-read-only)
    (beginning-of-line)
    (vn-down-article)
    (if (and vn-read-sequence
	     (equal curgroup (vn-current-group)))
	(vn-read-article))))

(defun vn-prev-article ()
  "back to the vn buffer if no previous articles"
  (interactive)
  (erase-buffer)
;  (set-buffer vn-buffer)
;  (select-window vn-window)
  (pop-to-buffer vn-buffer)
  (let ((curgroup (vn-current-group)))
    (beginning-of-line)
    (toggle-read-only)
    (delete-char 1)
    (insert " ")	
    (toggle-read-only)
    (beginning-of-line)
    (vn-up-article)
    (if (and vn-read-sequence
	     (equal curgroup (vn-current-group)))
	(vn-read-article))))

(defun vn-abort-reading ()
  "back to the buffer, and no other articles"
  (interactive)
  (erase-buffer)
  (setq vn-read-sequence nil)
  (let ((buffer (vn-find-buffer)))
;    (set-buffer buffer)
;    (select-window vn-window)
    (pop-to-buffer buffer)
    (beginning-of-line)
    (toggle-read-only)
    (delete-char 1)
    (insert " ")	
    (toggle-read-only)
    (beginning-of-line)
    (vn-down-article)))

(defun vn-abort-reading-next-group ()
  "back to the buffer, the first article in the next group"
  (interactive)
  (erase-buffer)
  (setq vn-read-sequence nil)
  (let ((buffer (vn-find-buffer)))
;    (set-buffer buffer)
;    (select-window vn-window)
    (pop-to-buffer buffer)
    (beginning-of-line)
    (toggle-read-only)
    (delete-char 1)
    (insert " ")	
    (toggle-read-only)
    (beginning-of-line)
    (vn-next-group)))

(defun vn-beginning-of-article ()
  "back to the beginning of the article..."
  (interactive)
  (beginning-of-buffer))

(defun vn-end-of-article ()
  "jump to the end of the article"
  (interactive)
  (end-of-buffer))

(defun vn-scroll-line ()
  "scroll line up one"
  (interactive)
  (scroll-up 1))

(defun vn-toggle-rot ()
  "caesar en/de code the current article"
  (interactive)
  (news-caesar-buffer-body)
)

(defvar vn-header-on nil "nil or t, no or yes to verbose header...")

(defun vn-toggle-header ()
  "to display those header lines or not to..."
  (interactive)
  (if vn-header-on 
      (progn
	(setq vn-header-on nil)
	(save-excursion
	  (goto-char (point-min))
	  (news-convert-format)))
    (setq vn-header-on t)
;    (set-buffer vn-buffer)
;    (select-window vn-window)
    (pop-to-buffer vn-buffer)
    (vn-read-article)))

(defun vn-next-page ()
  "scroll article/article-list up a page"
  (interactive)
  (scroll-up))

(defun vn-prev-page ()
  "scroll article/article-list down a page"
  (interactive)
  (scroll-down)
  (move-to-window-line 0))

(defun vn-save-this-article (file)
  "save this article by appending to a file"
  (interactive "FSave item in file:")
  (append-to-file (point-min) (point-max) file))
