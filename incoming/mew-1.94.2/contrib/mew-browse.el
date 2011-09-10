;;; mew-browse.el --- Handling URI with browse-url.el

;; Author: Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
;; Modify: Shuichi Kitaguchi <kit@Mew.org>
;; Created: May 19, 1999
;; Revised: May 24, 1999

;;;
;;; ~/.emacs settings.
;;;
;;; ... anything browse-url setting ...
;; (require 'mew-browse)

;;; SHIFT + (Middle|Right)-Click = browse-url or mew-user-agent-compose
;;; for Emacs
;; (define-key global-map [S-mouse-2] 'browse-url-at-mouse)
;;; for XEmacs
;; (define-key global-map [(shift button2)] 'browse-url-at-mouse)
;;

;;; Appending URI to specified file.
;;
;;   mew-browse-noask                 ... ask or not when browse
;;   mew-browse-append-file           ... URL collection file name
;;   mew-browse-append-always-file    ... always, append URL to file (for dial-up)
;;   mew-browse-append-always-mailto  ... always, URL is mailto: (for emacs19.28)
;;   mew-browse-append-file-sort      ... always, sort URL file
;;
;;; example:
;;   (setq mew-browse-noask                nil)
;;   (setq mew-browse-append-file          "~/.browse")
;;   (setq mew-browse-append-always-file   nil)
;;   (setq mew-browse-append-always-mailto nil)
;;   (setq mew-browse-append-file-sort nil)
;;

;;; Use mew-url-mailto instead of url-mailto in W3.
;;
;; (cond
;;  ((locate-library "url-mail")
;;   (eval-after-load "url-mail"
;;     '(fset 'url-mailto (symbol-function 'mew-url-mailto))))
;;  ((locate-library "url")
;;   (eval-after-load "url"
;;     '(fset 'url-mailto (symbol-function 'mew-url-mailto)))))
;;

(eval-when-compile (require 'mew))

(if (string-match "XEmacs" emacs-version)
    (defvar mew-browse-button [(button2)] "*Mouse button in message mode.")
  (defvar mew-browse-button [mouse-2] "*Mouse button in message mode."))

(setq browse-url-browser-function 'mew-browse-url)

(add-hook 'mew-init-hook
	  (lambda ()
	    (progn 
	      (define-key mew-message-mode-map mew-browse-button 'browse-url-at-mouse)
	      )))

(defvar mew-browse-url-mailto-switch-func nil
  "*Which do you like, nil, 'switch-to-buffer-other-window or 'switch-to-buffer-other-frame ?")

(setq browse-url-regexp "\\(\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]+\\)\\|\\(\\([^-A-Za-z0-9!_.%]\\|^\\)[-A-Za-z0-9._!%]+@[A-Za-z0-9][-A-Za-z0-9._!]+[A-Za-z0-9]\\)")

(defvar mew-browse-noask                t   "*Ask or not when browse.")
(defvar mew-browse-append-file          nil "*URL collection file.")
(defvar mew-browse-append-always-file   nil "*For dialup user")
(defvar mew-browse-append-always-mailto nil "*For emacs19.28")
(defvar mew-browse-append-file-sort     nil "*Sort URL file.")

(defun mew-browse-url (url &optional args)
  "Exec browse URL or mew-user-agent-compose with parsing RFC2368."
  (interactive
   (list (read-from-minibuffer "Mew URL: ")))
  (if (or (not (boundp 'mew-mail-path)) (null mew-mail-path))
      (save-excursion (mew)))
  (let* ((append-buffer (and mew-browse-append-file
			     (string= buffer-file-name
				      (expand-file-name mew-browse-append-file))))
	 (append-nil (or append-buffer (not mew-browse-append-file)))
	 (append-all (and (not append-nil) mew-browse-append-always-file))
	 (append-ask (and (not append-nil) (not mew-browse-append-always-file)))
	 (browse-all (or append-buffer mew-browse-noask))
	 (browse-ask (and (not append-buffer) (not mew-browse-noask))))
    (string-match "\\([a-zA-Z0-9][-a-zA-Z0-9!_=?#$@~`%&*+|\\/.,:]+\\)" url)
    (setq url (substring url (match-beginning 0) (match-end 0)))
    (if (not (string-match ":" url))    ;; emacs19.28 only
	(if (and (not mew-browse-append-always-mailto)
		 (not (y-or-n-p (format "mailto:%s(y) or ftp://%s(n)? " url url))))
	    (setq url (concat "ftp://" url))
	  (setq url (concat "mailto:" url))))
    (cond
     ((and append-all browse-all)
      (mew-browse-url-append url)
      (mew-browse-url-start url))
     ((and append-ask browse-all)
      (if (y-or-n-p (format "Append %s? " url))
	  (mew-browse-url-append url))
      (mew-browse-url-start url))
     ((and append-nil browse-all)
      (mew-browse-url-start url))
     ((and append-all browse-ask)
      (mew-browse-url-append url)
      (if (y-or-n-p (format "Browse %s? " url))
	  (mew-browse-url-start url)))
     ((and append-nil browse-ask)
      (if (y-or-n-p (format "Browse %s? " url))
	  (mew-browse-url-start url)))
     (t ;; (and append-ask browse-ask)
      (if (y-or-n-p (format "Browse %s(y) or Append(n)? " url))
	  (mew-browse-url-start url)
	(mew-browse-url-append url)))
     )))

(defun mew-browse-url-append (url)
  (let ((file (expand-file-name mew-browse-append-file))
	(beg))
    (save-excursion
      (find-file file)
      (set-buffer (current-buffer))
      (goto-char (point-min))
      (while (search-forward url nil t)
	(progn
	  (beginning-of-line)
	  (setq beg (point))
	  (forward-line)
	  (delete-region beg (point))))
      (goto-char (point-max))
      (insert url "\n")
      (if mew-browse-append-file-sort
	  (sort-lines nil (point-min) (point-max)))
      (write-file file)
      (kill-buffer (current-buffer))
      (message "Append %s to %s done." url file)
      )))

(defun mew-browse-url-start (url)
  (message "Browse %s." url)
  (cond
   ((string-match "^mailto:" url)
    (mew-browse-url-mailto url))
   ((and (symbolp mew-ext-prog-url) (fboundp mew-ext-prog-url))
    (funcall mew-ext-prog-url url))
   ((equal mew-ext-prog-url "w3")
    (require 'w3)
    (w3-fetch-other-frame url))
   (t
    (apply (function start-process)
	   (format "*mew %s*" mew-ext-prog-url)
	   nil mew-ext-prog-url 
	   (append mew-ext-prog-url-args (list url))))))

(defun mew-url-mailto (url)
  "Exec mew-user-agent-compose with parsing RFC2368."
  (interactive
   (list (read-from-minibuffer "Mew mailto: ")))
  (if (or (not (boundp 'mew-mail-path)) (null mew-mail-path))
      (save-excursion (mew)))
  (mew-browse-url-mailto url))

(defun mew-browse-url-mailto (url)
  (let (tmp to subject other)
      (and (boundp 'url-working-buffer)
	   url-working-buffer
	   (get-buffer url-working-buffer)
	   (kill-buffer url-working-buffer))
      (and (functionp 'url-view-url) (url-view-url t)
	   (setq other (cons (cons (capitalize "x-url-from") (url-view-url t)) other)))
      (while (string-match "[ \t]+" url)
	(setq url (concat (substring url 0 (match-beginning 0))
			  (substring url (match-end 0)))))
      (if (string-match "^mailto:" url)
	  (setq tmp (mew-browse-url-mailto-decamp (substring url (match-end 0))))
	(setq tmp (mew-browse-url-mailto-decamp url)))
      (if (string-match "^\\([^?]+\\)" tmp)
	  (progn
	    (setq to (mew-browse-url-mailto-hex-to-string
		      (substring tmp (match-beginning 1) (match-end 1))))
	    (setq tmp (substring tmp (match-end 0)))))
      (while (string-match "^[?&]\\([^=]+\\)=\\([^&]*\\)" tmp)
	(let ((hname (substring tmp (match-beginning 1) (match-end 1)))
	      (hvalue (mew-browse-url-mailto-hex-to-string
		       (substring tmp (match-beginning 2) (match-end 2)))))
	  (setq tmp (substring tmp (match-end 0)))
	  (cond
	   ((string-match "^to$" hname)
	    (if to
		(setq to (concat to ", " hvalue))
	      (setq to hvalue)))
	   ((string-match "^subject$" hname)
	    (setq subject hvalue))
	   (t
	    (setq other (cons (cons (capitalize hname) hvalue) other))))))
      (let ((mew-x-mailer mew-x-mailer))
	(and (functionp 'url-view-url) (url-view-url t)
	     (setq mew-x-mailer
		   (concat mew-x-mailer " / " url-package-name "-" url-package-version)))
	(mew-user-agent-compose to subject other nil mew-browse-url-mailto-switch-func))))

(defun mew-browse-url-mailto-decamp (str)
  (save-match-data
    (while (string-match "&amp;" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			"&"
			(substring str (match-end 0)))))
    str))

(defun mew-browse-url-mailto-hex-to-string (str)
  (save-match-data
    (while (string-match "%\\([0-9a-fA-F][0-9a-fA-F]\\)" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(make-string
			 1
			 (mew-browse-url-mailto-2hexs-to-int
			  (substring str (match-beginning 1) (match-end 1))))
			(substring str (match-end 0)))))
    str))

(defun mew-browse-url-mailto-2hexs-to-int (hex)
  (+ (* 16 (mew-hexchar-to-int (aref hex 0)))
     (mew-hexchar-to-int (aref hex 1))))

(provide 'mew-browse)
;;; mew-browse.el ends here

