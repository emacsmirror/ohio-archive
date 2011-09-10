;;; mew-mark.el --- Marking for Mew Summary and Virtual mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  2, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-mark-version "mew-mark.el version 0.12")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; level 1: *, @
;;; level 2: D
;;; level 3: o

(defvar mew-mark-delete-switch
  (list 
   (cons mew-mark-multi  'mew-mark-keep-line)
   (cons mew-mark-review 'mew-mark-keep-line)
   (cons mew-mark-delete 'mew-mark-delete-line)
   (cons mew-mark-refile 'mew-mark-delete-line)))

(defmacro mew-mark-delete-get-func (mark)
  (` (cdr (assoc (, mark) mew-mark-delete-switch))))

(defvar mew-mark-undo-switch
  (list 
   (cons mew-mark-multi  'mew-mark-unmark)
   (cons mew-mark-review 'mew-mark-unmark)
   (cons mew-mark-delete 'mew-mark-unmark)
   (cons mew-mark-refile 'mew-mark-unrefile)))

(defmacro mew-mark-undo-get-func (mark)
  (` (cdr (assoc (, mark) mew-mark-undo-switch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; for mew-mark-delete-switch
;;;

(defun mew-mark-delete-line ()
  (beginning-of-line)
  (let ((start (point)))
    (forward-line)
    (delete-region start (point))))

(defun mew-mark-keep-line ()
  ()) ;; do nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; for mew-mark-undo-switch
;;;

(defun mew-mark-unmark ()
  "Delete the mark on this message anyway"
  (save-excursion
    (mew-elet
     (beginning-of-line)
     (if (re-search-forward mew-summary-message-regex nil t)
	 (progn
	   (delete-char 1)
	   (insert " ")
	   (mew-highlight-unmark-line))))))

(defun mew-mark-unrefile ()
  "Delete refile state and delete the mark"
  (mew-refile-reset (mew-summary-message-number))
  (mew-mark-unmark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic functions and macros for mark
;;;

(defun mew-summary-marked-p ()
  "See if this message is marked"
  (save-excursion
    (beginning-of-line)
    (cond 
     ((looking-at (concat mew-summary-message-regex " \\|^$")) nil)
     (t t))))

(defun mew-summary-get-mark ()
  (let (mark)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at (concat mew-summary-message-regex "\\(.\\)")))
	  nil
	(setq mark (mew-match 2)) ;; regex includes \( \)
	(cond 
	 ((string= " " mark) nil)
	 (t (string-to-char mark)))))))

(defun mew-summary-mark-as (mark &optional force)
  "Mark this message if possible"
  (mew-elet
   (cond 
    ((or force (null (mew-summary-marked-p)))
     (save-excursion
       (beginning-of-line)
       (if (re-search-forward mew-summary-message-regex nil t)
	   (progn
	     (delete-char 1)
	     (insert (char-to-string mark))
	     (mew-highlight-mark-line mark)))))
    (t (message "Already marked")))))

(defmacro mew-mark-regex (mark)
  (` (concat mew-summary-message-regex
	     (regexp-quote (char-to-string (, mark))))))

(defmacro mew-mark-list-regex (mark-list)
  (` (concat mew-summary-message-regex 
	     "["
	     (mapconcat (function char-to-string) (, mark-list) "")
	     "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entire buffer
;;;

(defun mew-summary-mark-exist-p (mark-list)
  "See if this Summary mode has one or more marked messages"
  (let ((regex (mew-mark-list-regex mark-list)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun mew-summary-mark-collect (mark &optional begin end)
  (save-excursion
    (let ((regex (mew-mark-regex mark))
	  (msglist nil))
      (goto-char (if begin begin (point-min)))
      (while (re-search-forward regex end t)
	(setq msglist (cons (mew-summary-message-number) msglist)))
      (nreverse msglist))))


(defun mew-summary-mark-collect2 (mark)
  "For virtual mode, this function returns a list of
cons pairs of folder and number"
  (save-excursion
    (let ((regex (mew-mark-regex mark))
          (msglist nil))
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq msglist (cons 
                       (cons
                        (mew-summary-folder-name)
                        (mew-summary-message-number))
                       msglist)))
      (nreverse msglist))))

(defun mew-summary-mark-collect3 (mark)
  (save-excursion
    (let ((regex (mew-mark-regex mark))
	  (i 1)
	  ret)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at regex)
	    (setq ret (cons i ret)))
	(forward-line)
	(setq i (1+ i)))
      (nreverse ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Weak marks :: Reviewing and Multi
;;;

(defun mew-summary-mark-weak (mark)
  (mew-summary-msg-or-part
   (save-excursion
     (mew-summary-goto-message)
     (let ((cmark (mew-summary-get-mark)))
       (cond
	((or (equal cmark mew-mark-refile) (equal cmark mew-mark-delete))
	 (message "Already strongly marked as %s" (char-to-string cmark)))
	((equal cmark mark)
	 (message "Already marked as %s" (char-to-string cmark)))
	(t ;; no mark
	 (if cmark (mew-summary-undo-one))
	 (mew-summary-mark-as mark))))
     (set-buffer-modified-p nil))))

;; "@" in Summary mode
(defun mew-summary-multi ()
  "\\<mew-summary-mode-map>
Put the multi the '@' mark on this message for '\\[mew-summary-multi-forward]', '\\[mew-summary-unshar]', 
'\\[mew-summary-uudecode]', '\\[mew-summary-burst-multi]'. It can overlay the '*' mark. 
The cursor stays always."
  (interactive)
  (mew-summary-mark-weak mew-mark-multi))

;; "*" in Summary mode
(defun mew-summary-review ()
  "\\<mew-summary-mode-map>Put the review the '*' mark on this message. 
Use '\\[mew-summary-display-review-down]' or '\\[mew-summary-display-review-up]' to jump to a message marked with '*'.
It can overlay '@'. The cursor stays always.
See also '\\[mew-summary-mark-refile]', '\\[mew-summary-mark-delete]', '\\[mew-summary-mark-regexp]', and '\\[mew-summary-mark-all]'."
  (interactive)
  (mew-summary-mark-weak mew-mark-review))

(defun mew-summary-down-mark (mark)
  (forward-line)
  (cond 
   ((re-search-forward (mew-mark-regex mark) nil t)
    (beginning-of-line)
    t)
   (t 
    (forward-line -1)
    (message "No more marked message")
    nil)))

(defun mew-summary-display-review-down ()
  "Jump to the message marked with '*' below."
  (interactive)
  (if (mew-summary-down-mark mew-mark-review)
      (mew-summary-display nil)))

(defun mew-summary-up-mark (mark)
  (cond 
   ((re-search-backward (mew-mark-regex mark) nil t)
    t)
   (t 
    (message "No more marked message")
    nil)))

(defun mew-summary-display-review-up ()
  "Jump to the message marked with '*' above."
  (interactive)
  (if (mew-summary-up-mark mew-mark-review)
      (mew-summary-display nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strong marks :: Removing
;;;

(defun mew-summary-delete (count)
  "Put the delete mark(default is 'D') on this message.
This can overlay other marks. When it overlays, the cursor stays
on the message. If it marks newly, displays the next message."
  (interactive "P")
  (mew-summary-only
   (mew-summary-msg-or-part
    (mew-summary-goto-message)
    (mew-decode-syntax-delete)
    (if (not (numberp count))
	(if (mew-summary-delete-one)
	    (progn
	      ;; for C-x C-x
	      (beginning-of-line)
	      (push-mark (point) t t)
	      (mew-summary-display-after mew-summary-mark-direction)))
      (while (and (> count 0) (not (eobp)))
	(setq count (1- count))
	(mew-summary-delete-one 'force)
	(forward-line))
      (while (< count 0)
	(if (bobp)
	    (setq count 0)
	  (setq count (1+ count)))
	(mew-summary-delete-one 'force)
	(forward-line -1)))
    (set-buffer-modified-p nil))))

(defun mew-summary-delete-one (&optional force)
  (let ((mark (mew-summary-get-mark))
	(zmacs-regions nil))
    (cond
     ((equal mark mew-mark-delete)
      (or force (message "Already marked as delete"))
      nil)
     ((equal mark mew-mark-refile)
      (if (or force (y-or-n-p "Already marked as refile. Delete it? "))
	  (progn
	    (mew-summary-undo-one force)
	    (save-excursion (mew-summary-mark-as mew-mark-delete))))
      nil)
     ((or (equal mark mew-mark-review) (equal mark mew-mark-multi))
      (mew-summary-undo-one force)
      (save-excursion (mew-summary-mark-as mew-mark-delete))
      nil)
     (t ;; no mark
      (save-excursion (mew-summary-mark-as mew-mark-delete))
      t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo
;;;

(defun mew-summary-undo (count)
  "Cancel the mark on this message."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-goto-message)
   (mew-decode-syntax-delete)
   (save-excursion
     (if (not (numberp count))
	 (mew-summary-undo-one 'force)
       (while (and (> count 0) (not (eobp)))
	 (setq count (1- count))
	 (mew-summary-undo-one 'force)
	 (forward-line))
       (while (< count 0)
	 (if (bobp)
	     (setq count 0)
	   (setq count (1+ count)))
	 (mew-summary-undo-one)
	 (forward-line -1))))
   (set-buffer-modified-p nil)))

(defun mew-summary-undo-one (&optional force)
  "Undo one mark here."
  (interactive)
  (cond 
   ((mew-summary-marked-p)
    (beginning-of-line)
    (looking-at (concat mew-summary-message-regex "\\([^ +0-9]\\)"))
    (let* ((mark (string-to-char (mew-match 2)))
	   (func (mew-mark-undo-get-func mark)))
      (if func (funcall func))))
   ((eobp) (or force (message "No message")))
   (t (or force (message "No mark")))))

(defun mew-summary-undo-all ()
  "Cancel all marks according to what you input."
  (interactive)
  (let ((char nil) (ociea cursor-in-echo-area))
    (unwind-protect
	(progn
	  (message "Input mark : ")
	  (setq cursor-in-echo-area t)
	  (setq char (read-char))
	  (message "Input mark : %s" (char-to-string char)))
      (setq cursor-in-echo-area ociea))
    (if (assoc char mew-mark-undo-switch)
	(mew-summary-batch-unmark (list char) 'msg)
      (message "Mark %s is not supported" (char-to-string char)))))

;;"mx" extensions
(defun mew-summary-mark-undo-all ()
  "Unmark all message marked with 'o' or 'D'."
  (interactive)
  (mew-summary-batch-unmark (list mew-mark-delete mew-mark-refile) 'msg))

(defun mew-summary-batch-unmark (mark-list msg)
  (mew-decode-syntax-delete)
  (let* ((regex (mew-mark-list-regex mark-list)))
    (if msg (message "Unmarking ..."))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(mew-summary-undo-one 'force)))
    (if msg (message "Unmarking ... done")))
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "mx" extensions
;;;

(defun mew-summary-mark-all (&optional arg)
  "Put the '*' mark onto all messages which are not marked."
  (interactive "P")
  (if arg
      (mew-summary-mark-region (region-beginning) (region-end))
    (mew-summary-mark-region (point-min) (point-max))))

(defun mew-summary-mark-region (beg end)
  "Put the '*' mark onto all messages which are not marked between
BEG and END."
  (interactive "r")
  (let ((regex (concat mew-summary-message-regex " ")))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
	(if (mew-in-decode-syntax-p)
	    ()
	  (mew-summary-mark-as mew-mark-review))))))

(defun mew-summary-mark-regexp (regex)
  "Put the '*' mark onto Mall messages matched to a regular expression."
  (interactive "sRegexp: ")
  (if (not (equal regex ""))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (if (or (mew-summary-marked-p) (mew-in-decode-syntax-p))
	      ()
	    (mew-summary-mark-as mew-mark-review))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exchange
;;;

(defun mew-summary-exchange-mark (old-mark new-mark)
  (let ((regex (mew-mark-regex old-mark)))
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward regex nil t))
	  (message "No marked messages")
	(beginning-of-line)
	(mew-elet
	 (while (re-search-forward regex nil t)
	   (delete-backward-char 1)
	   (insert (char-to-string new-mark))
	   (mew-highlight-mark-line new-mark)))))))
   
(defun mew-summary-mark-delete ()	;; * -> D
  "Put the delete mark onto all messages marked with '*'."
  (interactive)
  (mew-summary-only
   (mew-summary-exchange-mark mew-mark-review mew-mark-delete)))

(defun mew-summary-mark-multi ()	;; * -> @
  "Change the '*' mark into the '@' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-multi))

(defun mew-summary-mark-review ()	;; @ -> *
  "Change the '@' mark into the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-multi mew-mark-review))

(defun mew-summary-mark-swap ()		;; @ <-> *
  "Swap the '@' mark and the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-multi mew-mark-tmp)
  (mew-summary-exchange-mark mew-mark-review mew-mark-multi)
  (mew-summary-exchange-mark mew-mark-tmp mew-mark-review))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning trash
;;;

(defun mew-summary-clean-trash ()
  "Remove all messages in +trash."
  (interactive)
  (if (not (yes-or-no-p (format "Remove all messages in %s? " mew-trash-folder)))
      ()
    (message "Removing all messages in %s ..." mew-trash-folder)
    (let ((msgs (directory-files (mew-expand-folder mew-trash-folder)
				 nil "^[0-9]+$"))
	  (cfile (mew-expand-folder
		  mew-trash-folder mew-summary-cache-file))
	  (cbuf))
      (mew-summary-imclean mew-trash-folder msgs)
      (if (get-buffer mew-trash-folder)
	  (save-excursion
	    (set-buffer mew-trash-folder)
	    (mew-erase-buffer)))
      ;; remove and touch the cache file.
      (if (file-exists-p cfile)
	  (save-excursion
	    (setq cbuf (find-file-noselect cfile))
	    (set-buffer cbuf)
	    (mew-erase-buffer)
	    (write-region (point-min) (point-max) cfile nil 'no-msg)
	    (set-buffer-modified-p nil)
	    (kill-buffer cbuf))))
    (message "Removing all messages in %s ... done" mew-trash-folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks
;;;

(defun mew-mark-delete-region (begin end)
  (mew-elet
   (let ((m (make-marker))
	 (regex (concat mew-summary-message-regex "\\([^ +0-9]\\)"))
	 mark func)
     (set-marker m end)
     (goto-char begin)
     (while (re-search-forward regex m t)
       (setq mark (string-to-char (mew-match 2)))
       (setq func (mew-mark-delete-get-func mark))
       (if func (funcall func)))
     (set-buffer-modified-p nil))))

(defun mew-summary-exec ()
  "\\<mew-summary-mode-map>
Process marked messages. To cancel the '*' mark, use '\\[mew-summary-undo]' or '\\[mew-summary-undo-all]'."
  (interactive)
  (mew-summary-only
   (mew-summary-exec-region (point-min) (point-max))))

(defun mew-summary-exec-current ()
  "Process the current marked messages."
  (interactive)
  (mew-summary-only
   (mew-summary-goto-message)
   (mew-decode-syntax-delete)
   (let (beg end)
     (save-excursion
       (beginning-of-line)
       (setq beg (point))
       (end-of-line)
       (setq end (1+ (point))))
     (mew-summary-exec-region beg end))))

(defvar mew-summary-exec-error-msg nil)

(defun mew-summary-exec-region (beg end)
  "Process marked messages between BEG and END."
  (interactive "r")
  (mew-summary-only
   (if (not (mew-summary-exclusive-p))
       ()
     (save-excursion
       (save-restriction
	 (narrow-to-region beg end)
	 (goto-char (point-min))
	 (message "Collecting marks ...")
	 (condition-case nil
	     (let ((msgs (mew-summary-mark-collect
			  mew-mark-refile (point-min) (point-max)))
		   (dels (mew-summary-mark-collect
			  mew-mark-delete (point-min) (point-max)))
		   (src (buffer-name))
		   err-folders)
	       (if (and (null msgs) (null dels))
		   (message "No marks")
		 ;; opening...
		 (setq mew-summary-buffer-process t)
		 (message "Refiling and deleting ...")
		 (mew-window-configure (current-buffer) 'summary)
		 (mew-current-set 'message nil)
		 (mew-decode-syntax-delete)
		 ;; refiling and deleting...
		 (if (setq err-folders (mew-summary-exec-sanity-check msgs))
		     (progn
		       (setq mew-summary-buffer-process nil)
		       (ding)
		       (message 
			"Nothing proceeded. Folder(s) MUST be a file!: %s"
			(mew-join "," err-folders)))
		   (mew-summary-exec-refile src msgs)
		   (mew-summary-exec-delete src dels)
		   ;; ending...
		   (mew-mark-delete-region (point-min) (point-max))
		   (mew-summary-folder-cache-save src)
		   (mew-refile-guess-save)
		   (run-hooks 'mew-summary-exec-hook)
		   (set-buffer-modified-p nil)
		   (mew-summary-reset-mode-line (current-buffer))
		   (setq mew-summary-buffer-process nil)
		   (message "Refiling and deleting ... done")
		   ))) ;; end of let
	   (quit
	    (set-buffer-modified-p nil)
	    (setq mew-summary-buffer-process nil))))))))

(defun mew-summary-exec-sanity-check (msgs)
  (if mew-use-imap
      nil ;; no sanity check
    (let (msg dst dsts uniq-dsts udst err-dsts dir)
      (while msgs
	(setq msg (car msgs))
	(setq msgs (cdr msgs))
	(setq dsts (cdr (assoc msg mew-summary-buffer-refile)))
	(while dsts
	  (setq dst (car dsts))
	  (setq dsts (cdr dsts))
	  (if (not (member dst uniq-dsts))
	      (setq uniq-dsts (cons dst uniq-dsts)))))
      (if (not (member mew-trash-folder uniq-dsts))
	  (setq uniq-dsts (cons mew-trash-folder uniq-dsts)))
      (while uniq-dsts
	(setq udst (car uniq-dsts))
	(setq uniq-dsts (cdr uniq-dsts))
	(setq dir (mew-expand-folder udst))
	(if (file-exists-p dir)
	    (if (file-directory-p dir)
		(if (not (file-writable-p dir))
		    (set-file-modes dir mew-folder-mode))
	      (setq err-dsts (cons udst err-dsts))) ;; NG
	  (mew-make-directory dir)))
      err-dsts)))

(defun mew-summary-exec-refile (src msgs)
  (let (dsts tmp msg msg-dsts dsts-msgses)
    (while msgs
      (setq msg (car msgs))
      (setq msgs (cdr msgs))
      (setq msg-dsts (assoc msg mew-summary-buffer-refile))
      (setq dsts (cdr msg-dsts))
      (if dsts ;; sanity check
	  (if (setq tmp (assoc dsts dsts-msgses))
	      (setq dsts-msgses (cons (append tmp (list msg))
				      (delete tmp dsts-msgses)))
	    (setq dsts-msgses (cons (list dsts msg) dsts-msgses))
	    ))
      (setq mew-summary-buffer-refile
	    (delete msg-dsts mew-summary-buffer-refile)))
    ;; refile at once
    (while dsts-msgses
      (mew-summary-immv src (car dsts-msgses))
      (setq dsts-msgses (cdr dsts-msgses)))))

(defun mew-summary-exec-delete (src dels)
  (if dels
      (let ((rm-it nil))
	(cond
	 ((equal mew-msg-rm-policy 'always)
	  (setq rm-it t))
	 ((equal mew-msg-rm-policy 'trashonly)
	  (if (equal src mew-trash-folder)
	      (setq rm-it t)))
	 ((equal mew-msg-rm-policy 'uselist)
	  (if (mew-member-match src mew-msg-rm-folder-list)
	      (setq rm-it t))))
	;; (t ;; 'totrash)
	(if rm-it
	    (mew-summary-imclean src dels)
	  (if (equal src mew-trash-folder)
	      (let ((regex (mew-mark-regex mew-mark-delete)))
		(goto-char (point-min))
		(while (re-search-forward regex nil t)
		  (mew-summary-undo-one)))
	    (mew-summary-imrm src dels))))))

(defun mew-summary-immv (src dsts-msgs)
  (let* ((dsts (car dsts-msgs)) ;; (+foo +bar)
	 (msgs (cdr dsts-msgs)) ;; (1 2 3)
	 (myselfp (mew-folder-member src dsts))
	 (dsts-tmp dsts)
	 msgs- msg srcfile dstfile dst num)
    (if (or mew-use-immv (mew-folder-remotep src)
	    (catch 'remote
	      (while dsts-tmp
		(if (mew-folder-remotep (car dsts-tmp))
		    (throw 'remote t))
		(setq dsts-tmp (cdr dsts-tmp)))))
	(mew-summary-im-start mew-prog-immv src dsts msgs t)
      (if myselfp
	  ;; msg stays in the src folder with the same number
	  (progn
	    (setq dsts (delete src dsts))
	    (while msgs
	      (setq msg (car msgs))
	      (setq msgs (cdr msgs))
	      (if (mew-file-regular-p (mew-expand-folder src msg))
		  (setq msgs- (cons msg msgs-))))
	    (setq msgs- (nreverse msgs-))
	    (setq msgs msgs-))
	(setq dst (car dsts)) 
	(setq dsts (cdr dsts))
	(setq num (string-to-int (mew-folder-new-message dst t)))
	(while msgs
	  (setq srcfile (mew-expand-folder src (car msgs)))
	  (setq msgs (cdr msgs))
	  (if (not (and (file-exists-p srcfile) (file-writable-p srcfile)))
	      ()
	    (setq msgs- (cons (int-to-string num) msgs-))
	    (setq dstfile (mew-expand-folder dst (int-to-string num)))
	    (setq num (1+ num))
	    (rename-file srcfile dstfile)))
	(mew-touch-folder dst)
	(setq msgs- (nreverse msgs-))
	(setq src dst)
	) ;; myselfp
      (while dsts
	(setq dst (car dsts)) 
	(setq dsts (cdr dsts))
	(setq num (string-to-int (mew-folder-new-message dst t)))
	(setq msgs msgs-)
	(while msgs
	  (setq srcfile (mew-expand-folder src (car msgs)))
	  (setq msgs (cdr msgs))
	  (setq dstfile (mew-expand-folder dst (int-to-string num)))
	  (setq num (1+ num))
	  (mew-link srcfile dstfile))
	(mew-touch-folder dst)))
    (if (not myselfp)
	()
      (goto-char (point-min))
      (setq msgs msgs-) ;; anyway
      (while msgs
	;; illegal marks remain and the lines will be deleted.
	(if (re-search-forward
	     (format "^ *%s%s" (car msgs)
		     (regexp-quote (char-to-string mew-mark-refile)))
	     nil t)
	    (mew-mark-unmark))
	(setq msgs (cdr msgs))))))

(defun mew-summary-imrm (src dels)
  (if (or mew-use-immv (mew-folder-remotep src))
      (mew-summary-im-start mew-prog-imrm src nil dels t)
    (let* (num srcfile dstfile)
      (setq num (string-to-int (mew-folder-new-message mew-trash-folder t)))
      ;; must be here after ensuring that +trash exists.
      (while dels
	(setq srcfile (mew-expand-folder src (car dels)))
	(setq dels (cdr dels))
	(setq dstfile (mew-expand-folder mew-trash-folder
					 (int-to-string num)))
	(setq num (1+ num))
	(if (mew-file-regular-p srcfile)
	    ;; if not, the marked line will be deleted anyway.
	    (rename-file srcfile dstfile)))
      (mew-touch-folder mew-trash-folder))))

(defun mew-summary-imclean (src dels)
  (if (or mew-use-immv (mew-folder-remotep src))
      (mew-summary-im-start mew-prog-imclean src nil dels t "--quiet=yes")
    (let (file)
      (while dels
	(setq file (mew-expand-folder src (car dels)))
	(setq dels (cdr dels))
	(if (mew-file-regular-p file)
	    ;; if not, the marked line will be deleted anyway.
	    (delete-file file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clean up marks!
;;;

(defun mew-mark-clean (&optional sum-buf)
  "Process marked messages for this folder"
  (if sum-buf
      (set-buffer sum-buf)
    (setq sum-buf (buffer-name)))
  (if (mew-summary-mark-exist-p (list mew-mark-delete mew-mark-refile))
      (if (y-or-n-p (format "Marks exist in %s. Process them? " sum-buf))
	  (mew-summary-exec))))

(defun mew-mark-init ()
  (add-hook 'kill-emacs-hook (function mew-mark-clean-up)))

(defun mew-mark-clean-up ()
  "Process marked messages for all Summary modes.
Typically called by kill-emacs-hook."
  (mew-decode-syntax-delete)
  (let ((bufs mew-buffers) buf)
    (save-excursion
      (while bufs
	(setq buf (car bufs))
	(setq bufs (cdr bufs))
	(if (bufferp (get-buffer buf))
	    (mew-mark-clean buf)))))
  (remove-hook 'kill-emacs-hook (function mew-mark-clean-up)))
;; Saving marks is a really bad idea.
;; First because there is no way to fill the gap if the folder is newer
;; than the cache at quitting.
;; Even if the cache is newer, saving marks faces dilemma if 
;; multiple Emacses run.

(provide 'mew-mark)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
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

;;; mew-mark.el ends here
