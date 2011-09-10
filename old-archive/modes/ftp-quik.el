;;; 1.      Documentation	ftp-quik-mode.el

;;; -------------------------------------------------------------------
;;; LISPDIR ENTRY for the Emacs Lisp Archive
;;; 
;;;    LCD Archive Entry:
;;;    ftp-quik|Terrence Brannon|tb06@pl122f.eecs.lehigh.edu|
;;;    Quik access to dired'ing of ange-ftp and normal paths|
;;;    28-Jul-1993|1.0|~/modes/ftp-quik.el.Z|
;;; -------------------------------------------------------------------

;;; -------------------------------------------------------------------
;;;
;;; ftp-quik-mode.el
;;;
;;; When you absolutely, positively have to be at that ftp site
;;; yesterday.:)
;;;
;;; When a file is in ftp-quik-mode, one keystroke initiates a dired
;;; on an ange-ftp or normal path-name.
;;;
;;; -------------------------------------------------------------------


;;; -------------------------------------------------------------------
;;;
;;; Installation
;;;
;;; REQUIRED
;;; In your .emacs place:
;;;
;;; (autoload 'ftp-quik-mode "ftp-quik" nil t)
;;; (setq auto-mode-alist (append '(("\\.ftp-quik$" . ftp-quik-mode))
;;;                              auto-mode-alist))
;;;
;;; SUGGESTED
;;; (global-set-key "\C-c\C-q" 'find-daily)
;;;
;;; (defun find-daily ()
;;;   (interactive)
;;;   (find-file "~/emacs/daily.ftp-quik"))
;;;
;;; It is also strongly suggested that you be running ange-ftp and
;;; tree-dired.
;;;
;;; -------------------------------------------------------------------

;;; -------------------------------------------------------------------
;;;
;;; Usage:
;;;
;;; Find a file with extension .ftp-quik
;;; type A and a pathname in ange-ftp or normal format
;;; if the pathname is ange-ftp format, then an entry is automatically
;;; added to your .netrc for it. If not, then the entry is simply
;;; added to the current buffer
;;;
;;; now connect to the directory by typing c
;;; delete the entry by typing D y
;;;
;;; -------------------------------------------------------------------


;;; 2.      Variable Definitions	ftp-quik-mode.el

(defvar ftp-quik-path-regexp "\\(.*\\)@\\(.*\\):\\(.*\\)" "")


;;; 3.      ftp-quik-mode	ftp-quik-mode.el

(defun ftp-quik-mode()
"Major Mode for Using ftp-quik. 
\\{ftp-quik-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq 
   buffer-read-only  t
   mode-name         "ftp-quik"
   major-mode        'ftp-quik-mode
   goal-column       0)


  (setq ftp-quik-mode-map (make-sparse-keymap))

  (define-key ftp-quik-mode-map "n" 'next-line)
  (define-key ftp-quik-mode-map "p" 'previous-line)
  (define-key ftp-quik-mode-map "c" 'ftp-quik-connect-to-site)
  (define-key ftp-quik-mode-map "A" 'ftp-quik-add-site)
  (define-key ftp-quik-mode-map "D" 'ftp-quik-delete-site)

  (use-local-map ftp-quik-mode-map))

;;; 3.1.    ftp-quik-connect-to-site ()	ftp-quik-mode.el

(defun ftp-quik-connect-to-site ()
  (interactive)
  (let (
	(bol (progn (beginning-of-line) (point)))
	(eol (progn (end-of-line) (point)))
	)
    (setq site (buffer-substring bol eol))
    (dired-other-window site)))

;;; 3.2.    ftp-quik-add-site (path)	ftp-quik-mode.el

(defun ftp-quik-add-site (path)
  (interactive "D")

  (setq buffer-read-only nil)
  (insert (concat path "\n"))
  (setq buffer-read-only t)

  (if (string-match ftp-quik-path-regexp path)
      (ftp-quik-dump-path-to-netrc (match-data) path)))

;;; 3.2.1.  ftp-quik-dump-path-to-netrc (match-info path)	ftp-quik-mode.el

(defun ftp-quik-dump-path-to-netrc (match-info path)
  (save-excursion
    (find-file "~/.netrc")
    (goto-char (point-min))
    (store-match-data match-info)
    (insert
     (concat
      "machine "   (substring path (match-beginning 2) (match-end 2)) " "
      "login "     (substring path (match-beginning 1) (match-end 1)) " " 
      "password "  (concat (user-login-name) "@" (system-name)) "\n"))
    (write-file "~/.netrc")
    (kill-buffer (current-buffer))))


  

;;; 3.3.    ftp-quik-delete-site ()	ftp-quik-mode.el

(defun ftp-quik-delete-site ()
  (interactive)
  (if (y-or-n-p "Delete site? ")
      (progn
	(beginning-of-line)
	(setq buffer-read-only nil)
	(kill-line 1)
	(setq buffer-read-only t))))


;;; 4.      Emacs local variables	ftp-quik-mode.el

;; Local variables:
;; folded-file: t
;; end:
