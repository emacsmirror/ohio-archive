;;; mew-attach.el --- attachments for Mew Draft mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-attach-version "mew-attach.el version 0.18")

(require 'mew)

(defvar mew-draft-attach-map nil
  "A region map for attachments in Draft mode")

(if mew-draft-attach-map
    ()
  (setq mew-draft-attach-map (make-keymap))
  (define-key mew-draft-attach-map "\C-m" 'mew-attach-newline)
  (define-key mew-draft-attach-map "\C-f" 'mew-attach-forward)
  (define-key mew-draft-attach-map "\C-b" 'mew-attach-backforward)
  (define-key mew-draft-attach-map "\C-n" 'mew-attach-next)
  (define-key mew-draft-attach-map "\C-p" 'mew-attach-previous)
  (define-key mew-draft-attach-map "a"    'mew-attach-audio)
  (define-key mew-draft-attach-map "c"    'mew-attach-copy)
  (define-key mew-draft-attach-map "C"    'mew-attach-charset)
  (define-key mew-draft-attach-map "d"    'mew-attach-delete)
  (define-key mew-draft-attach-map "y"    'mew-attach-link-message)
  (define-key mew-draft-attach-map "D"    'mew-attach-description)
  (define-key mew-draft-attach-map "P"    'mew-attach-disposition)
  (define-key mew-draft-attach-map "e"    'mew-attach-external-body)
  (define-key mew-draft-attach-map "f"    'mew-attach-find-file)
  (define-key mew-draft-attach-map "F"    'mew-attach-find-new-file)
  (define-key mew-draft-attach-map "l"    'mew-attach-link)
  (define-key mew-draft-attach-map "m"    'mew-attach-multipart)
  (define-key mew-draft-attach-map "T"    'mew-attach-type)
  (define-key mew-draft-attach-map "G"    'mew-attach-gzip64)
  (define-key mew-draft-attach-map "B"    'mew-attach-base64)
  (define-key mew-draft-attach-map "Q"    'mew-attach-quoted-printable)
  (define-key mew-draft-attach-map "S"    'mew-attach-pgp-sign)
  (define-key mew-draft-attach-map "E"    'mew-attach-pgp-enc)
  (define-key mew-draft-attach-map "p"    'mew-attach-pgp-public-key)
  (define-key mew-draft-attach-map "U"    'mew-attach-undo)
  (if mew-use-overlay-keymap
      (mew-draft-share-keymap 'mew-draft-attach-map))
  )

(if mew-xemacs-p
    ()
  (if mew-use-overlay-keymap
      (easy-menu-define
       mew-draft-attach-menu
       mew-draft-attach-map
       "Menu used in Draft mode."
       mew-draft-mode-menu-spec)))

(defconst mew-draft-attach-boundary-begin
  "------------------------------ attachments ------------------------------")
(defconst mew-draft-attach-boundary-end
  "--------0-1-2-3-4-5-6-7-8-9----------------------------------------------")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; goodies
;;

(defun mew-draft-prepare-attachments (&optional no-scroll)
  "Prepare an attachment region in the bottom of the draft.
To compose a multipart message, you should execute this command first."
  (interactive)
  (if (not (mew-header-p))
      (message "Attachments region can be prepared before making a message only")
    (if (mew-attach-p)
	(message "Attachments region have been already prepared")
      (let* ((attachdir (mew-attachdir))
	     (cp (expand-file-name mew-draft-coverpage attachdir))
	     (dir (file-name-nondirectory attachdir)))
	(if (mew-attach-p)
	    (message "Attachments already exist.")
	  (mew-attach-set)
	  (if (not (file-exists-p attachdir)) (mew-make-directory attachdir))
	  (if (not (file-exists-p cp)) 
	      (write-region 1 1 cp 'append 'no-msg));; touch
	  (if (not mew-encode-syntax)
	      (setq mew-encode-syntax (mew-encode-syntax-initial dir)))
	  (mew-encode-syntax-print mew-encode-syntax)
	  (mew-attach-goto-number 'here '(2))
	  (if (and (not no-scroll)
		   (not (pos-visible-in-window-p
			 (point-max)
			 (get-buffer-window (current-buffer)))))
	      (set-window-start (get-buffer-window (current-buffer))
				(mew-attach-begin)))
	  (setq buffer-undo-list nil))))))

(defun mew-attach-expand-path (syntax nums)
  "ignore the first dir"
  (let ((path ""))
    (while (cdr nums)
      (setq syntax (aref syntax (+ mew-syntax-magic (1- (car nums)))))
      (setq path (concat path (mew-syntax-get-file syntax)))
      (setq nums (cdr nums)))
    path))

(defun mew-attach-line ()
  "Return the line number for the cursor from the beginning of the 
attachments region. 
0 if on the beginning boundary. 
-1 if on the ending boundary."
  (let (ret max)
    (save-excursion
      (beginning-of-line)
      (setq ret (count-lines (1+ (mew-attach-begin)) (point)))
      (goto-char (point-max))
      (beginning-of-line)
      (setq max (count-lines (1+ (mew-attach-begin)) (point)))
      (if (or (= ret max) (= ret (1- max))) -1 ret))))


(defun mew-attach-line-lastp ()
  (let ((here (point)))
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      (equal (count-lines here (point)) 2))))

(defun mew-attach-move-onto-file ()
  (end-of-line)
  (re-search-backward " [^ ]" nil t)
  (forward-char))

(defun mew-attach-move-onto-body ()
  (goto-char (1+ (mew-attach-begin)))
  (forward-char -1))

(defun mew-attach-directory-p ()
  (save-excursion
    (mew-attach-move-onto-file)
    (looking-at (concat "[^ ]+" mew-path-separator))))

(defun mew-attach-dot-p ()
  (save-excursion
    (mew-attach-move-onto-file)
    (looking-at "\\.$")))

;; cursor only
(defun mew-attach-line0-1 ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(or (= line 0) (= line -1)))))

;; cursor only
(defun mew-attach-line01-1 ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(or (= line 0) (= line 1) (= line -1)))))

;; insert commands
(defun mew-attach-not-line012-1 ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(not (or (= line 0) (= line 1) (= line 2) (= line -1))))))

;; find commands
(defun mew-attach-not-line012-1-dot ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(not (or (mew-attach-dot-p)
		 (= line 0) (= line 1) (= line 2) (= line -1))))))


;; delete commands
 
(defun mew-attach-not-line02-1-dot ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(not (or (mew-attach-dot-p) (= line 0) (= line 2) (= line -1))))))


;; modify commands
(defun mew-attach-not-line0-1-dot ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(not (or (mew-attach-dot-p) (= line 0) (= line -1))))))

;; corsor
(defun mew-attach-line1-dot ()
  (if (mew-attach-p)
      (let ((line (mew-attach-line)))
	(or (mew-attach-dot-p) (= line 1)))))

(defun mew-attach-goto-number (direction nums)
  (let (numreg)
    (setq nums (nreverse nums))
    (cond
     ((equal direction 'next)
      (setq nums (cons (1+ (car nums)) (cdr nums))))
     ((equal direction 'prev)
      (setq nums (cons (1- (car nums)) (cdr nums))))
     ((equal direction 'up)
      (setq nums (cdr nums)))
     ((equal direction 'down)
      (setq nums (cons 1 nums)))
     (t ()))
    (if (null nums)
	(progn
	  (goto-char (1- (mew-attach-begin)))
	  (re-search-forward "^..... " nil t)
	  (mew-attach-move-onto-file))
      (setq numreg (int-to-string (car nums)))
      (setq nums (cdr nums))
      (while nums
	(setq numreg (concat (int-to-string (car nums)) "." numreg))
	(setq nums (cdr nums)))
      (setq numreg (regexp-quote numreg))
      (goto-char (1+ (mew-attach-begin)))
      (re-search-forward (concat "^....." numreg " ") nil t)
      (mew-attach-move-onto-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; commands
;;

;;
;; cursor commands
;;

(defun mew-attach-newline ()
  "Insert RET before the attachments region."
  (interactive)
  (if (equal (point) (1+ (mew-attach-begin)))
      (progn
	(insert "\n")
	(mew-draft-attach-keymap))))

(defun mew-attach-forward ()
  "Go to the first subdirectory in attachments."
  (interactive) 
  (if (mew-attach-line0-1)
      (mew-attach-goto-number 'here nil)
    (mew-attach-move-onto-file)
    (if (mew-attach-directory-p)
	(mew-attach-goto-number 'down (mew-syntax-nums))
      (mew-attach-goto-number 'here (mew-syntax-nums)))))

(defun mew-attach-backforward ()
  "Go to the parent directory in attachments."
  (interactive)
  (if (mew-attach-line0-1)
      (mew-attach-move-onto-body)
    (let ((nums (mew-syntax-nums)))
      (if (= (length nums) 1)
	  (mew-attach-goto-number 'here nil)
	(mew-attach-goto-number 'up (mew-syntax-nums))))))

(defun mew-attach-previous ()
  "Go to the previous file in the current directory in attachments."
  (interactive)
  (if (mew-attach-line01-1)
      (mew-attach-move-onto-body)
    (if mew-attach-move-by-line
	(progn
	  (forward-line -1)
	  (mew-attach-move-onto-file))
      (let* ((nums (mew-syntax-nums))
	     (last (nth (1- (length nums)) nums)))
	(if (= last 1)
	    (mew-attach-goto-number 'here (mew-syntax-nums))
	  (mew-attach-goto-number 'prev (mew-syntax-nums)))))))

(defun mew-attach-next ()
  "Go to the next file in the current directory in attachments."
  (interactive)
  (if (mew-attach-line0-1)
      (mew-attach-goto-number 'here nil)
    (if mew-attach-move-by-line
	(if (mew-attach-line-lastp)
	    ()
	  (forward-line)
	  (mew-attach-move-onto-file))
      (if (mew-attach-line1-dot)
	  (mew-attach-goto-number 'here (mew-syntax-nums))
	(mew-attach-goto-number 'next (mew-syntax-nums))))))

;;
;; delete commands
;;

(defun mew-attach-delete ()
  "Delete this file or this directory in attachments."
  (interactive)
  (if (not (mew-attach-not-line02-1-dot))
      (message "Can't delete here.")
    (if (equal (mew-attach-line) 1)
	(if (y-or-n-p "Delete entire attachments? ")
	    (progn
	      (mew-delete-directory-recursively (mew-attachdir))
	      (setq mew-encode-syntax nil)
	      (mew-attach-clear)
	      (if mew-icon-p
		  (set-specifier default-toolbar
				 (cons (current-buffer) mew-draft-toolbar)))
	      (message "attachments were deleted"))
	  (message "attachments were not deleted"))
      (let* ((nums (mew-syntax-nums))
	     (subdir (mew-attach-expand-path mew-encode-syntax nums))
	     (attachdir (mew-attachdir))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (name (mew-syntax-get-file syntax))
	     (ename (if (equal subdir "") name (concat subdir name)))
	     (fullname (expand-file-name ename attachdir))
	     (dirp (string-match (concat mew-path-separator "$") name))
	     (msg (if dirp
		      "Delete %s with contained files? "
		    "Delete %s? ")))
	;; attachdir / {subdir/} name
	(if (not (y-or-n-p (format msg ename)))
	    ()
	  (message "Deleting %s ... " ename)
          (if (file-exists-p fullname)
              (if dirp
                  (mew-delete-directory-recursively fullname)
                (delete-file fullname)))
	  (message "Deleting %s ... done" ename)
	  (setq mew-encode-syntax
		(mew-syntax-remove-entry mew-encode-syntax nums))
	  (mew-encode-syntax-print mew-encode-syntax))))))

;;
;; insert commands
;;

(defun mew-attach-multipart ()
  "Create a subdirectory(i.e. multipart) on '.' in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't create a sub-multipart here.")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (insert-default-directory nil)
	   (attachdir (mew-attachdir))
	   dir dirpath)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir "")) 
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / dir
      (setq dirpath (mew-random-filename attachdir))
      (if (file-exists-p dirpath)
	  (message "Could not make a directory, sorry")
	(setq dir (file-name-nondirectory dirpath))
	(mew-make-directory dirpath)
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax 
	       nums
	       (mew-encode-syntax-multi dir mew-type-mlm)))
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-duplicate (func action setmode &optional from to default)
  (if (not (mew-attach-not-line012-1))
      (message "Can't %s here." action)
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   (frompath from)
	   (efile (and to (concat subdir to)))
	   (tofile to)
	   FILE)
      (while (or (null frompath)
		 (file-directory-p (file-chase-links frompath)))
	(setq frompath (mew-draft-input-file-name
			(concat (capitalize action) " from : ") default)))
      (while (or (null efile)
		 (file-exists-p (expand-file-name efile attachdir)))
	(setq tofile (mew-input-string 
		      (concat (capitalize action) " to %s(%s): ")
		      subdir
		      (file-name-nondirectory frompath)))
	(setq efile (concat subdir tofile)))
      (setq FILE (expand-file-name efile attachdir))
      (funcall func frompath FILE)
      (if (and setmode (/= mew-file-mode (mew-file-get-mode FILE)))
	  (set-file-modes FILE mew-file-mode))
      (setq mew-encode-syntax
	    (mew-syntax-insert-entry
	     mew-encode-syntax 
	     nums
	     (mew-encode-syntax-single tofile nil nil nil nil 'cdp)))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-link (&optional from to)
  "Link a file with a symbolic link on '.' in attachments.
FROM must be absolute path. TO must be a file name."
  (interactive)
  (mew-attach-duplicate (function mew-symbolic-link) "link" nil from to))

(defun mew-attach-copy (&optional from to)
  "Copy a file (via networks) on '.' in attachments.
FROM must be absolute path. TO must be a file name.
To copy a remote file, use the \"/[user@]hostname:/filepath\" syntax."
  (interactive)
  (mew-attach-duplicate (function copy-file) "copy" t from to))

(defun mew-attach-link-message ()
  "Link the message displayed in Message mode with a symbolic link
on '.' in attachments."
  (interactive)
  (let ((fld-msg (mew-current-get 'message))
	default)
    (if fld-msg
	(setq default (mew-expand-folder-get-msg (car fld-msg) (cdr fld-msg))))
    (if mew-use-symbolic-link-for-forwarding
	(mew-attach-duplicate (function mew-symbolic-link) "link" nil nil nil default)
      (mew-attach-duplicate (function copy-file) "copy" nil nil nil default))))

(defun mew-attach-find-new-file ()
  "Open a new file into a buffer on '.' in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't find a new file here.")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir "")) 
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir mew-text-suffix))
      (if (null filepath)
	  (message "Could not make a text file, sorry.")
	(setq file (file-name-nondirectory filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax 
	       nums
	       (mew-encode-syntax-single file (list mew-ct-txt) ;; for charset
					 nil nil nil nil)))
	(mew-encode-syntax-print mew-encode-syntax)
	;;
	(find-file filepath)
	(text-mode)
	;; buffer switched
	(local-set-key "\C-c\C-q" 'mew-kill-buffer)))))

(defun mew-attach-audio ()
  "Sampling voice and insert as audio file on '.' in attachments"
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't attach audio data here.")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath recorded)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir "")) 
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir mew-audio-suffix))
      (if (null filepath)
	  (message "Could not make a file for audio, sorry.")
	(setq file (file-name-nondirectory filepath))
	(save-excursion
	  (mew-set-buffer-tmp)
	  (condition-case nil
	      (progn
		(if (not (y-or-n-p "Are you ready? "))
		    (message "Nothing is recoded.")
		  (message
		   (substitute-command-keys
		    "Type '\\[keyboard-quit]' to finish recording..."))
		  (mew-plet
		   (apply (function call-process)
			  (nth 0 mew-prog-audio2)
			  nil t nil
			  (nth 1 mew-prog-audio2)))))
	    (quit
	     (message
	      (substitute-command-keys
	       "Type '\\[keyboard-quit]' to finish recording... done."))
	     (setq recorded t)
	     (mew-flet
	      (write-region (point-min) (point-max) filepath nil 'no-msg)))))
	(if recorded
	    (setq mew-encode-syntax ;; buffer local
		  (mew-syntax-insert-entry
		   mew-encode-syntax
		   nums
		   (mew-encode-syntax-single file mew-type-ado
					     nil nil nil 'cdp))))
	(mew-encode-syntax-print mew-encode-syntax)))))

;;
;; modify commands
;;

(defun mew-attach-undo ()
  "Unmark. The original mark appears."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't undo encoding here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums)))
      (mew-syntax-set-privacy syntax nil)
      (mew-syntax-set-decrypters syntax nil)
      (mew-syntax-set-cte syntax
			  (mew-syntax-get-cte
			   (mew-encode-syntax-single 
			    (mew-syntax-get-file syntax))))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-type ()
  "Change the data type(Content-Type:) in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't change %s here" mew-ct:)
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (file (mew-syntax-get-file syntax))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   cte)
      (setq ct (mew-input-type "Type for %s (%s): " file ct
			       (if (mew-syntax-multipart-p syntax)
				   mew-mime-content-type-multipart-list
				 mew-mime-content-type-list)))
      (setq ctl (list ct))
      (mew-syntax-set-ct syntax ctl)
      (setq cte (mew-attr-get-cte (mew-attr-by-ct ct)))
      (mew-syntax-set-cte syntax cte)
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-charset ()
  "Specify charset for a Text/* object in attachments."
  (interactive)
  (if (not mew-mule-p)
      (message "This commands cannot be used on this Emacs 19")
    (if (not (mew-attach-not-line0-1-dot))
	(message "Can't specify character set here")
      (let* ((nums (mew-syntax-nums))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (file (mew-syntax-get-file syntax))
	     (ctl (mew-syntax-get-ct syntax))
	     (ct (mew-syntax-get-value ctl 'cap))
	     (char (mew-syntax-get-param ctl "charset"))
	     charset
	     (case-fold-search t))
	(if (not (string-match "text/" ct))
	    (message "Can't specify character set to %s" ct)
	  (if (null char) (setq char "guess"))
	  (setq charset (mew-input-type "Charset for %s (%s): "
					file char mew-charset-list))
	  (if (equal charset "guess")
	      (setq ctl (list ct))
	    (setq ctl (list ct (list "charset" charset))))
	  (mew-syntax-set-ct syntax ctl)
	  (mew-encode-syntax-print mew-encode-syntax))))))

(defun mew-attach-description (&optional cd)
  "Input a description(Content-Description:) in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't describe here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ocd (or (mew-syntax-get-cd syntax) (mew-syntax-get-file syntax))))
      (if (null cd)
	  (setq cd (read-string "Description: " ocd)))
      (if (equal cd "")
	  (mew-syntax-set-cd syntax nil)
	(mew-syntax-set-cd syntax cd))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-disposition (&optional cdp)
  "Input Content-Disposition: in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1-dot))
      (message "Can't set disposition here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ocdp (or (mew-syntax-get-param (mew-syntax-get-cdp syntax)
					   "filename")
		     (mew-syntax-get-file syntax))))
      (if (null cdp)
	  (setq cdp (read-string "Filename: " ocdp)))
      (if (equal cdp "") (setq cdp nil)) ;; to clear
      (mew-syntax-set-cdp syntax (mew-syntax-cdp-format cdp))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-base64 ()
  "Put the 'B' mark to encode with Base64 in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't encode with base64 here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   (case-fold-search t))
	(if (or (string-match "^Message/" ct) (string-match "^Multipart/" ct))
	    (message "Can't encode with base64 here")
	  (mew-syntax-set-cte syntax mew-b64)
	  (mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-quoted-printable ()
  "Put the 'Q' mark to encode with Quoted-Printable in attachments"
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't encode with quoted-printable here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   (case-fold-search t))
	(if (or (string-match "^Message/" ct) (string-match "^Multipart/" ct))
	    (message "Can't encode with quoted-printable here")
	  (mew-syntax-set-cte syntax mew-qp)
	  (mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-gzip64 ()
  "Put the 'G' mark to encode with Gzip64 in attachments. This is 
applicable only to Text/Plain and Application/Postscript since compression 
is not effective other objects. For example, JPEG is already compressed."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Can't apply gzip here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   (case-fold-search t))
      (if (or (string-match "^Message/" ct)
	      (string-match "^Multipart/" ct)
	      (not (mew-ct-linebasep ct)))
	  ;; never use compress to types other than text/plain or 
	  ;; application/postscript. Indeed, compression is not
	  ;; effective to compressed data such as jpeg.
	  (message "Can't apply gzip to %s" ct)
	(mew-syntax-set-cte syntax mew-xg)
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-pgp-sign ()
  "Put the 'PS' mark to sign with PGP in attachments."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Can't PGP sign here"))
   ((null mew-pgp-ver)
    (message "%s doesn't exist" mew-prog-pgp))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (pgp-sign (nth 1 (assoc 'pgp-signature mew-privacy-database))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car pgp-sign) privacy)
	(message "Already marked."))
       (t
	(mew-syntax-set-privacy 
	 syntax (append privacy pgp-sign))
	(mew-encode-syntax-print mew-encode-syntax)))))))

(defun mew-attach-pgp-enc ()
  "Put the 'PE' mark to encrypt with PGP in attachment. 
Input decryptors' addresses."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Can't PGP encrypt here"))
   ((null mew-pgp-ver)
    (message "%s doesn't exist" mew-prog-pgp))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (towhom (mew-header-parse-address-list mew-destination:-list))
	   (pgp-enc (nth 1 (assoc 'pgp-encryption mew-privacy-database))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car pgp-enc) privacy)
	(message "Already marked."))
       (t
	;; ask towhom before set privacy for C-g
	(if towhom
	    (setq towhom (mew-input-address "To (%s): " (mew-join "," towhom)))
	  (setq towhom (mew-input-address "To: ")))
	;; towhom is already canonicalized.
	(mew-syntax-set-privacy 
	 syntax (append privacy pgp-enc))
	(mew-syntax-set-decrypters syntax towhom)
	(mew-encode-syntax-print mew-encode-syntax)))))))

;;
;; find commands
;;

(defun mew-attach-find-file ()
  "Open this file into a buffer in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1-dot))
      (message "Can't find a file here.")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))	   
	   (file (mew-syntax-get-file syntax))
	   efile FILE)
      (setq efile (if (equal subdir "") file (concat subdir file)))
      (setq FILE (expand-file-name efile attachdir))
      (find-file FILE)
      ;; buffer switched
      (local-set-key "\C-c\C-q" 'mew-kill-buffer)
      (if (file-symlink-p FILE)
	  (message "This file is indicated by a symbolic link. Take care.")))))

(provide 'mew-attach)

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

;;; mew-attach.el ends here
