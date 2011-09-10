;; mew.el --- Messaging in the Emacs World

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1994
;; Revised: Feb 28, 1999

;;; Commentary:

;; The updated version is available from:
;;	ftp://ftp.Mew.org/pub/Mew/mew-current.tar.gz
;;	http://www.Mew.org/
;;
;; Minimum setup:
;;	(autoload 'mew "mew" nil t)
;;	(autoload 'mew-send "mew" nil t)
;;	(setq mew-mail-domain-list '("your mail domain"))
;;	(setq mew-icon-directory "icon directory")
;;
;; Optional setup (e.g. for C-xm):
;;	(autoload 'mew-user-agent-compose "mew" nil t)
;;	(if (boundp 'mail-user-agent)
;;	    (setq mail-user-agent 'mew-user-agent))
;;	(if (fboundp 'define-mail-user-agent)
;;	    (define-mail-user-agent
;;	      'mew-user-agent
;;	      'mew-user-agent-compose
;;	      'mew-draft-send-letter
;;	      'mew-draft-kill
;;	      'mew-send-hook))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew version
;;;

(defconst mew-version-number "1.94.2"
  "Version number for this version of Mew.")
(defconst mew-version (format "Mew version %s" mew-version-number)
  "Version string for this version of Mew.")
(provide 'mew)
(require 'mew-vars)
(require 'mew-func)

(defun mew-version-show ()
  "Show mew-version in minibuffer."
  (interactive)
  (message "%s" mew-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For developers
;;;

(defvar mew-debug nil)
;;(setq mew-debug nil)
;;(setq mew-debug t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bootstrap
;;;

(defun mew (&optional arg)
  "Execute Mew. If 'mew-auto-get' is 't', messages stored in your
spool are fetched to the +inbox folder and messages in the +inbox
folder are listed up in Summary mode. If 'mew-auto-get' is 'nil', list
up messages in the inbox folder. If '\\[universal-argument]' is specified, perform this
function thinking that 'mew-auto-get' is reversed."
  (interactive "P")
  (mew-window-push)
  (if (null mew-mail-path) (mew-init))
  (let ((auto (if arg (not mew-auto-get) mew-auto-get)))
    (if auto
	(mew-summary-get)
      (mew-summary-goto-folder t (mew-inbox-folder)))))

(defun mew-send (&optional to cc subject)
  "Execute Mew then prepare a draft. This may be used as library
function."
  (interactive)
  (mew-current-set 'window (current-window-configuration))
  (if (null mew-mail-path) (mew-init))
  (mew-summary-send to cc subject))

(defun mew-user-agent-compose (&optional to subject other-headers continue
                                             switch-function yank-action
                                             send-actions)
  "Set up mail composition draft with Mew.
This is 'mail-user-agent' entry point to Mew.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

A Draft buffer is prepared according to SWITCH-FUNCTION.

CONTINUE, YANK-ACTION and SEND-ACTIONS are ignored."
  (if (null mew-mail-path) (mew-init))
  (let* ((draft (mew-folder-new-message mew-draft-folder))
	 (attachdir (mew-attachdir draft)))
    (mew-current-set 'window (current-window-configuration))
    (mew-window-configure (current-buffer) 'draft)
    (mew-summary-prepare-draft
     (if switch-function
	 (let ((special-display-buffer-names nil)
	       (special-display-regexps nil)
	       (same-window-buffer-names nil)
	       (same-window-regexps nil))
	   (funcall switch-function (find-file-noselect draft)))
       (switch-to-buffer (find-file-noselect draft)))
     (mew-draft-rename draft)
     (mew-delete-directory-recursively attachdir)
     (mew-draft-header subject nil to nil nil nil nil other-headers)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook))))

;;;
;;; Functions for boot time
;;;

(defun mew-init ()
  (mew-hello)
  (message "Setting Mew world ...")
  (run-hooks 'mew-env-hook)
  (mew-set-environment)
  (run-hooks 'mew-init-hook)
  (mew-status-update t)
  (mew-passwd-setup)
  (if (get-buffer mew-buffer-hello) (kill-buffer mew-buffer-hello))
  (message "Setting Mew world ... done"))

(defun mew-set-environment (&optional no-dir)
  (let (error-message)
    (condition-case nil
	(progn
	  ;; sanity check
	  (cond
	   ((string-match "^18" emacs-version)
	    (setq error-message "Not support Emacs 18 nor Mule 1\n")
	    (error ""))
	   ((null mew-mail-domain-list)
	    (setq error-message "Must set 'mew-mail-domain-list'")
	    (error "")))
	  ;; initialize IM variables
	  (if (setq error-message (mew-config-init)) (error ""))
	  ;; initializing
	  (or no-dir (mew-buffers-init))
	  (or no-dir (mew-temp-dir-init))
	  (mew-mark-init)
	  (mew-refile-init))
      (error
       (set-buffer mew-buffer-hello)
       (goto-char (point-max))
       (insert "\n\nMew errors:\n\n")
       (and error-message (insert error-message))
       (set-buffer-modified-p nil)
       (setq buffer-read-only t)
       ;; cause an error again
       (error "Mew found some errors above.")))))

(defun mew-status-update (arg)
  "Read Addrbook and update its information. If executed with '\\[universal-argument]',
information of folders is also updated in addition to that of
Addrbook. If 'mew-use-folders-file-p' is 't', the list of
folders is stored in '~/Mail/.folders'. The default value is 't'."
  (interactive "P")
  (message "Updating status ... ")
  (if (interactive-p) (mew-set-environment 'no-dir))
  (if arg (mew-folder-setup nil (interactive-p)))
  (mew-addrbook-setup)
  (mew-pgp-set-version)
  (mew-highlight-face-setup mew-highlight-header-face-list)
  (mew-highlight-face-setup mew-highlight-body-face-list)
  (mew-highlight-face-setup mew-highlight-mark-face-list)
  (mew-highlight-make-keywords-regex)
  (mew-uniq-variables)
  (message "Updating status ...   done"))

(defun mew-uniq-variables ()
  (setq mew-mime-content-type-list (mew-uniq-list mew-mime-content-type-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Config
;;;

(defvar mew-mail-path nil)
(defvar mew-news-path nil)
(defvar mew-queue-path nil)
(defvar mew-inbox-folder nil)
(defvar mew-draft-folder nil)
(defvar mew-addrbook-file nil)
(defvar mew-alias-file nil)
(defvar mew-petname-file nil)
(defvar mew-trash-folder nil)
(defvar mew-imap-account nil)
(defvar mew-config-cases nil)
(defvar mew-config-case-inbox nil)

(defvar mew-path-alist
  '(("MailPath" . mew-mail-path)
    ("NewsPath" . mew-news-path)
    ("QueuePath" . mew-queue-path)
    ("InboxFolder" . mew-inbox-folder)
    ("DraftFolder" . mew-draft-folder)
    ("TrashFolder" . mew-trash-folder)
    ("AddrBookFile" . mew-addrbook-file)
    ("AliasFile"    . mew-alias-file)
    ("PetnameFile"  . mew-petname-file)
    ("ImapAccount" . mew-imap-account)
    ("ConfigCases" . mew-config-cases)
    ("ConfigCaseInbox" . mew-config-case-inbox)))

(defvar mew-draft-mime-folder nil)

(defmacro mew-config-error (var key)
  (` (if (null (, var))
	 (setq errmsg (concat errmsg "\t" (, key)
			      " was NOT found in Config.\n")))))

(defun mew-config-init ()
  (mew-set-buffer-tmp)
  (if (not (mew-which mew-prog-impath exec-path))
      (format "%s is not found in 'exec-path'" mew-prog-impath)
    (mew-im-call-process nil mew-prog-impath "--path=yes")
    (goto-char (point-min))
    (let (key value pair)
      (while (not (eobp))
	(if (looking-at "^\\([^=\n]+\\)=\\(.+\\)$")
	    ;; Petname may be null string.
	    (progn
	      (setq key (mew-match 1))
	      (setq value (mew-match 2))
	      (if (setq pair (mew-assoc-match2 key mew-path-alist 0))
		  (set (cdr pair) value))))
	(forward-line)))
    (if mew-config-cases 
	(setq mew-config-list (mew-split mew-config-cases ?,))
      (setq mew-config-list (list mew-config-default)))
    (if mew-config-case-inbox
	(let (tmp)
	  (setq mew-config-case-inbox (mew-split mew-config-case-inbox ?,))
	  (while mew-config-case-inbox
	    (setq tmp (cons (mew-split (car mew-config-case-inbox) ?:) tmp))
	    (setq mew-config-case-inbox (cdr mew-config-case-inbox)))
	  (setq mew-config-case-inbox (nreverse tmp))))
    (let (errmsg)
      (mew-config-error mew-mail-path "MailDir")
      (mew-config-error mew-news-path "NewsDir")
      (mew-config-error mew-inbox-folder "InboxFolder")
      (mew-config-error mew-draft-folder "DraftFolder")
      (mew-config-error mew-trash-folder "TrashFolder")
      (if errmsg
	  errmsg ;; return value
	;; xxx hard coding... +draft/mime
	(setq mew-draft-mime-folder
	      (concat (file-name-as-directory mew-draft-folder) "mime"))
	(mew-config-set-modes (list mew-mail-path mew-news-path))
	(mew-config-create-folders (cons mew-draft-folder
					 (cons mew-trash-folder
					       (mew-inbox-folders))))
	nil)))) ;; return value

(defun mew-config-set-modes (dirs)
  (let (dir)
    (while dirs
      (setq dir (file-chase-links (car dirs)))
      (setq dirs (cdr dirs))
      (if (file-exists-p dir)
	  (if (/= mew-folder-mode (mew-file-get-mode dir))
	      (set-file-modes dir mew-folder-mode))))))

(defun mew-config-create-folders (folders)
  (let (target)
    (while folders
      (setq target (mew-expand-folder (car folders)))
      (if (file-exists-p target)
	  ()
	(mew-make-directory target)
	(message "%s was created" target))
      (setq folders (cdr folders)))))

(defun mew-config-clean-up ()
  (setq mew-mail-path nil)
  (setq mew-news-path nil)
  (setq mew-queue-path nil)
;;  (setq mew-inbox-folder nil)
  (setq mew-draft-folder nil)
  (setq mew-petname-file nil)
  (setq mew-trash-folder nil)
  (setq mew-imap-account nil)
  (setq mew-config-cases nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folders
;;;

(defvar mew-folder-list nil)
(defvar mew-folder-alist nil)

(defun mew-folder-setup (&optional new-folder interactivep)
  (cond
   (new-folder
    (if (mew-folder-member new-folder mew-folder-list)
	()
      (setq mew-folder-list (sort (cons new-folder mew-folder-list)
				  (function mew-string<)))
      (setq mew-folder-alist (mew-folder-make-alist mew-folder-list))))
   (t
    (setq mew-folder-list (mew-folder-make-list interactivep))
    (setq mew-folder-alist (mew-folder-make-alist mew-folder-list)))))

(defun mew-folder-delete (folder)
  (setq mew-folder-list (delete folder mew-folder-list))
  (setq mew-folder-alist (delq (assoc folder mew-folder-alist)
			       mew-folder-alist)))
 
(defun mew-folder-clean-up ()
  (setq mew-folder-list nil)
  (setq mew-folder-alist nil))

(defun mew-folder-make-list (updatep)
  (save-excursion
    (let ((case-fold-search t)
	  (folders ())
	  (folder nil)
	  (start nil)
	  (file (expand-file-name mew-folders-file mew-mail-path)))
      (mew-set-buffer-tmp)
      (cond
       ((and (not updatep)
	     mew-use-folders-file-p
	     (file-readable-p file))
	(insert-file-contents file))
       (t
	(mapcar (function (lambda (x) (insert x "\n")))
		(nconc (funcall mew-folder-list-function "+")
		       (funcall mew-folder-list-function "=")))
	(if mew-use-imap
	    (mew-folder-mail-to-imap))
	(if mew-use-folders-file-p
	    (write-region (point-min) (point-max) file nil 'no-msg))))
      (goto-char (point-min))
      (while (not (eobp))
	(setq start (point))
	(if (not (or (looking-at "[+=]")
		     (and mew-use-imap (looking-at "%"))))
	    (forward-line)
	  (forward-line)
	  (setq folder (mew-buffer-substring start (1- (point))))
	  (if (and (car folders)
		   (string-match (concat "^" (regexp-quote 
					      (file-name-as-directory
					       (car folders))))
				 folder))
	      ;; regexp-quote is not necessary since not "+".
	      (setq folders 
		    (cons folder 
			  (cons (file-name-as-directory (car folders))
				(cdr folders))))
	    (setq folders (cons folder folders)))))
      (sort (nreverse folders) (function mew-string<)))))

(defun mew-folder-mail-to-imap ()
  (goto-char (point-min))
  (if mew-imap-account
      (while (re-search-forward "^+@[^#]+#[^/]+/\\(.*\\)$" nil t)
	(replace-match (concat "%" (mew-match 1))))))

(defmacro mew-folder-make-alist (list)
  (` (mapcar (function mew-folder-pair) (, list))))

(defun mew-folder-pair (folder)
  (let* ((dir (directory-file-name (mew-folder-to-dir folder)))
	 ;; foo/bar  -> foo/bar
	 ;; foo/bar/ -> foo/bar
	 (subdir (file-name-nondirectory dir)))
	 ;; foo/bar -> bar 
	 ;; foo -> foo
    (if (mew-folders-ignore-p folder)
	(list folder nil)
      (list folder subdir))))

(defun mew-folders-ignore-p (folder)
  (let ((ignores mew-folders-ignore))
    (catch 'ignore
      ;; while always returns nil
      (while ignores
	(if (string-match (concat "^" (car ignores)) folder)
	    (throw 'ignore t))
	(setq ignores (cdr ignores))))))

(defun mew-string< (a b)
  (let ((case-fold-search nil) (ret (string< a b)))
    (if (or (string-match (concat "^" (regexp-quote a)) b)
	    (string-match (concat "^" (regexp-quote b)) a))
	(not ret)
      ret)))

(defun mew-inbox-folder ()
  (cond
   ((string= mew-config-imget mew-config-default);; this may be lengthy
    mew-inbox-folder)
   ((assoc mew-config-imget mew-config-case-inbox)
    (nth 1 (assoc mew-config-imget mew-config-case-inbox)))
   (t 
    mew-inbox-folder)))

(defun mew-inbox-folders ()
  (if (null mew-config-case-inbox)
      (list mew-inbox-folder)
    (let ((inboxes (mapcar (function (lambda (x) (nth 1 x)))
			   mew-config-case-inbox)))
      (if (member mew-inbox-folder inboxes)
	  inboxes
	(cons mew-inbox-folder inboxes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window configuration stack
;;;

(defvar mew-window-stack nil)

(defun mew-window-clean-up ()
  (setq mew-window-stack nil))

(defun mew-window-push ()
  (let ((frame (selected-frame))
	(config (current-window-configuration)))
    (setq mew-window-stack (cons (cons frame config) mew-window-stack))))

(defun mew-window-pop ()
  (let* ((frame (selected-frame))
	 (assoc (assoc frame mew-window-stack)))
    (if (and assoc (window-configuration-p (cdr assoc)))
	(set-window-configuration (cdr assoc))
      (switch-to-buffer (get-buffer-create mew-window-home-buffer)))
    (setq mew-window-stack (delete assoc mew-window-stack))))

;;;
;;; Message buffer
;;;

(defun mew-buffer-message ()
  (if window-system
      (concat
       mew-buffer-message
       (int-to-string
	(mew-member-case-equal 
	 (cdr (assq
	       'window-id
	       (frame-parameters (selected-frame))))
	 (sort
	  (mapcar (function (lambda (frame)
			      (cdr (assq 'window-id
					 (frame-parameters frame)))))
		  (frame-list))
	  (function string<)))))
    mew-buffer-message))

;;;
;;; Window configuration
;;;

(defun mew-window-configure (nbuf action)
;;; action : summary, message, draft or list
;;; list for action (1 0)  for Summary only
;;; list for action (3 10) for Summary and Message
  (let* ((windows
	  (if (listp action) 
	      action
	    (car (cdr (assq action mew-window-configuration)))))
	 (obufwin (get-buffer-window (current-buffer)))
	 (msgwin  (get-buffer-window (mew-buffer-message)))
	 (height nil) (winsum nil) (sum-height 0) (msg-height 0))
    (setq height (+ (if obufwin (window-height obufwin) 0)
		    (if msgwin  (window-height msgwin)  0)))
    (if (or mew-window-use-full
	    (<= height (* 2 window-min-height)))
	(progn
	 ;; Delete other windows and use full emacs window.
	 (delete-other-windows)
	 (setq height (window-height (selected-window)))))
    (if (get-buffer (mew-buffer-message))
	(delete-windows-on (mew-buffer-message))
      (save-excursion
	(set-buffer (get-buffer-create (mew-buffer-message)))
	;; "truncate?" is asked in Message mode.
	;; so set the same toolbar as Sumamry mode
	(mew-summary-toolbar-update)
	(mew-message-mode)))
    (setq winsum (apply (function +) windows))
    (if (not (zerop (nth 0 windows)))
	(setq sum-height (max window-min-height
			     (/ (* height (nth 0 windows)) winsum))))
    (if (and (equal action 'message) (equal (% sum-height 2) 1)) 
	(setq sum-height (1+ sum-height)))
    (if (not (zerop (nth 1 windows)))
	(setq msg-height (max window-min-height
			     (- height sum-height))))
    (setq height (+ sum-height msg-height))
    (if (null (zerop sum-height))
	(switch-to-buffer nbuf 'norecord))
    (if (zerop msg-height)
	()
      (split-window nil sum-height)
      (other-window 1)
      (switch-to-buffer (mew-buffer-message) 'norecord))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffers
;;;

(defvar mew-buffers nil)

(defun mew-buffers-init ()
  (setq mew-buffers (mew-inbox-folders))) ;; for quiting

(defun mew-buffers-setup (folder)
  (if (not (mew-folder-member folder mew-buffers))
      (setq mew-buffers (cons folder mew-buffers))))

(defun mew-buffers-bury ()
  (let ((buffers mew-buffers))
    (while buffers
      (if (get-buffer (car buffers))
	  (bury-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun mew-buffers-clean-up ()
  (while mew-buffers
    (if (get-buffer (car mew-buffers))
	(mew-kill-buffer (car mew-buffers)))
    (setq mew-buffers (cdr mew-buffers)))
  (mew-buffers-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporary directory
;;;

(defvar mew-temp-dir nil)  ;; the default is "/tmp/user_name_uniq"
(defvar mew-temp-file nil) ;; the default is "/tmp/user_name_uniq/mew"

(defun mew-temp-dir-init ()
  "Setting temporary directory for Mew.
mew-temp-file must be local and readable for the user only
for privacy/speed reasons. "
  (setq mew-temp-dir (make-temp-name mew-temp-file-initial))
  (mew-make-directory mew-temp-dir)
  (set-file-modes mew-temp-dir mew-folder-mode)
  (setq mew-temp-file (expand-file-name "mew" mew-temp-dir))
  (add-hook 'kill-emacs-hook (function mew-temp-dir-clean-up)))

(defun mew-temp-dir-clean-up ()
  "A function to remove Mew's temporary directory recursively. 
It is typically called by kill-emacs-hook."
  (remove-hook 'kill-emacs-hook (function mew-temp-dir-clean-up))
  (if (and mew-temp-dir (file-exists-p mew-temp-dir))
      (mew-delete-directory-recursively mew-temp-dir))
  (setq mew-temp-dir nil)
  (setq mew-temp-file nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teer down
;;;

(defun mew-bury-buffer (&optional buf)
  (bury-buffer buf)
  (delete-windows-on buf t))

(defun mew-kill-buffer (&optional buf)
  "Erase the current mode(buffer)."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
	 (folder (if (bufferp buf) (buffer-name buf) buf)))
    (if (eq major-mode 'mew-virtual-mode)
	(mew-folder-delete folder))
    (if (and (eq major-mode 'mew-summary-mode)
	     (mew-folder-remotep folder))
	(mew-remote-folder-cache-delete folder))
    (if (get-buffer buf)
	(progn
	  (save-excursion
	    (set-buffer buf)
	    (mew-overlay-delete-buffer))
	  (kill-buffer buf)))))

(defun mew-buffer-message-clean-up (func)
  (if window-system
      (let ((bl (buffer-list))
	    (regexp (concat "^" (regexp-quote mew-buffer-message)))
	    b bn)
	(while bl
	  (setq b (car bl))
	  (setq bl (cdr bl))
	  (if (and (setq bn (buffer-name b))
		   (string-match regexp bn))
	      (funcall func b))))
    (funcall func (mew-buffer-message))))

(defun mew-buffer-draft-clean-up ()
  (let ((bl (buffer-list))
	(regexp (concat "^" (regexp-quote mew-draft-folder)))
	b bn)
    (while bl
      (setq b (car bl))
      (setq bl (cdr bl))
      (if (and (setq bn (buffer-name b))
	       (string-match regexp bn)
	       (equal 'mew-draft-mode
		      (save-excursion
			(set-buffer b)
			major-mode)))
	  (mew-kill-buffer b)))))


(defmacro mew-quit-toolbar-update ()
  '(if (fboundp 'redraw-frame) ;; for BOW
       (redraw-frame (selected-frame)))) ;; update toolbar

(defun mew-summary-suspend ()
  "Suspend Mew then switch to another buffer. All buffers of 
Mew remain, so you can resume with buffer operations."
  (interactive)
  (mew-buffer-message-clean-up (function mew-bury-buffer))
  (mew-buffers-bury)
  (mew-window-pop)
  (mew-quit-toolbar-update)
  (run-hooks 'mew-suspend-hook))

(defun mew-summary-quit ()
  "Quit Mew. All buffers of Mew are erased."
  (interactive)
  (if (not (y-or-n-p "Quit Mew? "))
      ()
    ;; killing buffers
    (mew-buffer-message-clean-up (function mew-kill-buffer)) ;; Message mode
    (mew-buffer-draft-clean-up) ;; Draft mode
    (mew-cache-clean-up)
    (mapcar (function mew-kill-buffer) mew-buffer-list) ;; other buffers
    ;;
    (mew-mark-clean-up)
    (mew-buffers-clean-up) ;; Summary mode and Virtual mode
    (mew-temp-dir-clean-up)
    ;;
    (run-hooks 'mew-quit-hook)
    ;;
    ;; lastly, clean up variables
    ;;
    (mew-folder-clean-up)
    (mew-current-clean-up)
    (mew-addrbook-clean-up)
    (mew-passwd-clean-up)
    ;;
    (mew-window-pop)
    (mew-window-clean-up)
    (mew-quit-toolbar-update)
    ;;
    (mew-config-clean-up) ;; MUST be last
    ;; flush minibuffer
    (message "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load Mew libraries
;;;
(require 'mew-addrbook)
(require 'mew-complete)
(require 'mew-minibuf)
(require 'mew-cache)
(require 'mew-encode)
(require 'mew-decode)
(require 'mew-mime)
(require 'mew-mark)
(require 'mew-header)
(require 'mew-pgp)
(require 'mew-bq)
(require 'mew-syntax)
(require 'mew-scan)
(require 'mew-pick)
(require 'mew-summary)
(require 'mew-virtual)
(require 'mew-message)
(require 'mew-draft)
(require 'mew-attach)
(require 'mew-demo)
(require 'mew-refile)
(require 'mew-ext)
(require 'mew-fib)
(require 'mew-sort)
(require 'mew-highlight)

;;; Copyright Notice:

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew.el ends here
