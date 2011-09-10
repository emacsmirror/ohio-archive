;;; mew-ext.el --- Message/External-Body support for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 13, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-ext-version "mew-ext.el version 0.21")

(require 'mew)
(eval-when-compile
  (cond
   ((mew-which-el "efs" load-path)
    (require 'efs))
   ((mew-which-el "ange-ftp" load-path)
    (require 'ange-ftp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; customize variables
;;;

(defvar mew-ext-prog-url "netscape")
(defvar mew-ext-prog-url-args '("-install"))

;; If you want to use w3.el instead of "netscape", put the 
;; following in .emacs.
;;(setq mew-ext-prog-url "w3")
;;(setq mew-ext-prog-url-args nil)

;; If you want to use lynx instead of "netscape", put the 
;; following in .emacs.
;;(setq mew-ext-prog-url "kterm")
;;(setq mew-ext-prog-url-args '("-e" "lynx" "-color"))

(defvar mew-ext-anon-ftp-method 'ftp
  "A method to get the message body for access-type=anon-ftp.
If 'ftp is specified, ange-ftp or efs is used. If 'http is specified,
w3 is used.")

;;
;; encode
;;

(defvar mew-ext-default-access-type "anon-ftp")

(defvar mew-ext-ftp-server-list 
  '("ftp.Mew.org" "sh.wide.ad.jp"))

(defvar mew-ext-encode-switch
  '(("ftp"         . mew-ext-encode-ftp)
;;    ("tftp"        . mew-ext-encode-tftp)
    ("anon-ftp"    . mew-ext-encode-anon-ftp)
    ("local-file"  . mew-ext-encode-local-file)
    ("mail-server" . mew-ext-encode-mail-server)
    ("url"         . mew-ext-encode-url)))

;;
;; decode
;;

(defvar mew-ext-switch
  '(("ftp"         . mew-ext-ftp)
    ("tftp"        . mew-ext-tftp)
    ("anon-ftp"    . mew-ext-anon-ftp)
    ("mail-server" . mew-ext-mail-server)
    ("url"         . mew-ext-url))) ;; RFC2017

(defvar mew-ext-include-switch 
  '(("local-file" . mew-ext-include-local-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; environment variables
;;;

(cond
 ((mew-which-el "efs" load-path)
  (defun mew-ext-file-name-completion (file path)
    (require 'efs)
    (let ((efs-tmp-name-template mew-temp-file))
      (efs-file-name-completion file path)))
  (defun mew-ext-file-name-all-completions (file path)
    (require 'efs)
    (let ((efs-tmp-name-template mew-temp-file))
      (efs-file-name-all-completions file path)))
  (defun mew-ext-expand-dir (host user dir)
    (require 'efs)
    (let ((efs-tmp-name-template mew-temp-file) exp)
      (setq exp (efs-expand-file-name (format "/%s@%s:%s" user host dir)))
      (if (string-match ".*:\\(.*\\)$" exp)
	  (mew-match 1 exp))))
  (defun mew-ext-copy-file-internal (remote local passwd)
    (require 'efs)
    (let ((efs-tmp-name-template mew-temp-file)
	  (efs-generate-anonymous-password passwd)
	  (parsed (efs-ftp-path remote)))
      (efs-copy-file-internal remote parsed local nil 
			      nil nil nil nil t 'image))))
 ((mew-which-el "ange-ftp" load-path)
  (defun mew-ext-file-name-completion (file path)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file))
      (ange-ftp-file-name-completion file path)))
  (defun mew-ext-file-name-all-completions (file path)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file))
      (ange-ftp-file-name-all-completions file path)))
  (defun mew-ext-expand-dir (host user dir)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file) exp)
      (setq exp (ange-ftp-expand-file-name (format "/%s@%s:%s" user host dir)))
      (if (string-match ".*:\\(.*\\)$" exp)
	  (mew-match 1 exp))))
  (defun mew-ext-copy-file-internal (remote local passwd)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file)
	  (ange-ftp-generate-anonymous-password passwd))
      (ange-ftp-copy-file-internal remote local t nil nil nil t))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encode
;;;

(defun mew-attach-external-body ()
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't insert external-body here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath ct)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir "")) 
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir mew-ext-suffix))
      (if (file-exists-p filepath)
	  (message "Could not make a file for external-body, sorry.")
	(setq file (file-name-nondirectory filepath))
	(setq ct (mew-ext-encode filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax 
	       nums
	       (mew-encode-syntax-single file ct)))
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-create-content-id ()
  ;; this is not unique if used with very short interval.
  ;; but it's ok
  (format "<%s.%s.%s@%s>" (nth 0 (current-time)) (nth 1 (current-time)) 
	  (emacs-pid) (system-name)))

(defun mew-ext-encode (filename)
  (let (buf ret access-type ct name)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-ext))
      (setq buf (current-buffer))
      (mew-erase-buffer)
      ;;content-header
      (setq access-type (mew-input-general
			 "Access type" mew-ext-encode-switch t 
			 mew-ext-default-access-type))
      (setq ret (funcall (cdr (assoc access-type mew-ext-encode-switch))))
      ;;message-header
      (cond 
       ((string= access-type "url")
	(setq name "URL")
	(setq ct "Text/Html"))
       ((string= access-type "mail-server")
	(setq name "Mail server's file")
	(setq ct mew-ct-apo))
       (t 
	(setq name (file-name-nondirectory
		    (mew-chop (mew-syntax-get-param ret "name"))))
	;; name is quoted
	(if (equal name "")
	    (setq ct mew-ct-apo)
	  (setq ct (capitalize (mew-attr-get-ct (mew-attr-by-file name)))))))
      (setq ct (mew-input-type "Type for %s (%s): " name ct
			       mew-mime-content-type-list))
      (mew-header-insert mew-ct: ct)
      (mew-header-insert mew-cid: (mew-create-content-id))
      (insert "\n")
      (if (not (string= access-type "mail-server"))
	  () ;; message-body is not necessary
	;;message-body
	(insert (read-string "Input message to the mail-server: "))
	(insert "\n"))
      (write-file filename))
    (kill-buffer buf)
    (cons mew-ct-ext (cons (list "access-type" access-type) ret))))

(defun mew-ext-encode-ftp ()
  ;; "name" "site" "directory" "mode"
  (let ((mew-ext-host (mew-input-general 
		       "FTP server"
		       (if mew-ext-ftp-server-list 
			   (mapcar (function list) 
				   mew-ext-ftp-server-list))
		       nil
		       (car mew-ext-ftp-server-list)))
	mew-ext-user path dir file ret)
    (setq ret (list (list "site" mew-ext-host)))
    (setq mew-ext-user (read-string (format "User name at %s: " mew-ext-host)
				(user-login-name)))
    (setq path (mew-input-rfile "Filename :"))
    (setq file (file-name-nondirectory path))
    (setq dir (file-name-directory path))
    (if (and dir (string-match mew-home dir))
	(setq dir (mew-ext-expand-dir mew-ext-host mew-ext-user dir)))
    (cond
     (dir
      (setq ret (cons (list "directory" dir) ret))
      (setq ret (cons (list "name" file) ret)))
     (t
      (setq ret (cons (list "name" file) ret))))
    ret))

(defun mew-ext-encode-tftp ()
  ;; xxx not yet
  )

(defun mew-ext-encode-anon-ftp ()
  ;; "name" "site" "directory" "mode"
  (let ((mew-ext-user "anonymous")
	(mew-ext-host (mew-input-general
		       "FTP server"
		       (if mew-ext-ftp-server-list 
			   (mapcar (function list) 
				   mew-ext-ftp-server-list))
		       nil
		       (car mew-ext-ftp-server-list)))
	path dir file ret)
    (setq ret (list (list "site" mew-ext-host)))
    (setq path (mew-input-rfile "Filename :"))
    (setq file (file-name-nondirectory path))
    (setq dir (file-name-directory path))
    (if (and dir (string-match mew-home dir))
	(setq dir (mew-ext-expand-dir mew-ext-host mew-ext-user dir)))
    (cond
     (dir
      (setq ret (cons (list "directory" dir) ret))
      (setq ret (cons (list "name" file) ret)))
     (t
      (setq ret (cons (list "name" file) ret))))
    ret))

(defun mew-ext-encode-local-file ()
  ;; "name" "site"
  (let ((file (mew-draft-input-file-name "File name: ")))
    (list (list "name" (expand-file-name file)))))

(defun mew-ext-encode-mail-server ()
  ;; "server" "subject"
  (let (server subject)
    (setq server (car (mew-input-address "Server address: ")))
    (setq subject (read-string (concat mew-subj: " ")))
    (list (list "server" server)
	  (list "subject" subject))))
   
(defun mew-ext-encode-url ()
  ;; "url"
  (let ((url (read-string "URL: ")))
    (list (list "url" url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decode
;;;

;;
;; exclude
;;

(defun mew-mime-external-body (begin end params &optional execute)
  ;; message-buffer
  (let* ((access-type (mew-syntax-get-param params "access-type"))
	 (program (cdr (mew-assoc-match access-type mew-ext-switch 0))))
    ;; xxx expire
    (if (and (symbolp program) (fboundp program))
	(funcall program begin end params execute))))

(defun mew-ext-ftp (begin end params execute)
  (mew-elet
   (let* ((site (mew-syntax-get-param params "site"))
	  (directory (mew-syntax-get-param params "directory"))
	  (name (mew-syntax-get-param params "name"))
	  (size (mew-syntax-get-param params "size"))
	  (getit t) (username "")
	  filepath localfile lfname remotefile)
     (if directory
	 (setq filepath (concat (file-name-as-directory directory) name))
       (setq filepath name))
     (if (and filepath (string-match "^[A-Za-z]:/.+" filepath)) ;; drive letter
	 (setq filepath (substring filepath 2 (match-end 0))))
     (mew-erase-buffer)
     (insert " ####### ####### ######  \n"
	     " #          #    #     # \n"
	     " #          #    #     # \n"
	     " #####      #    ######  \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     "\n\n")
     (insert "You can get the message content by FTP\n\n")
     (mew-insert "Site:\t%s\n" site)
     (mew-insert "File:\t%s\n" filepath)
     (mew-insert "Size:\t%s bytes\n" size)
     (if (null execute)
	 (insert "\nTo get this file, type "
		 (substitute-command-keys
		  "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))
       (setq username (read-string (format "User name at %s: " site)
				   (user-login-name)))
       (setq remotefile (format "/%s@%s:%s" username site filepath))
       (setq localfile (mew-summary-input-file-name "Save to: " name))
       (setq lfname (file-name-nondirectory localfile))
       (if (file-exists-p localfile)
	   (if (y-or-n-p (format "%s exists. Overwrite? " lfname))
	       (delete-file localfile)
	     (setq getit nil)
	     (message "The file wasn't retrieved")))
       (if getit (mew-ext-copy-file-internal remotefile localfile nil))))))

(defun mew-ext-tftp (begin end params execute)
  (message "access-type TFTP is not supported yet"))

(defun mew-ext-anon-ftp (begin end params execute)
  (mew-elet
   (let* ((site (mew-syntax-get-param params "site"))
	  (directory (mew-syntax-get-param params "directory"))
	  (name (mew-syntax-get-param params "name"))
	  (size (mew-syntax-get-param params "size"))
	  (getit t)
	  filepath localfile lfname remotefile url)
     (if directory
	 (setq filepath (concat (file-name-as-directory directory) name))
       (setq filepath name))
     (if (and filepath (string-match "^[A-Za-z]:/.+" filepath)) ;; drive letter
	 (setq filepath (substring filepath 2 (match-end 0))))
     (mew-erase-buffer)
     (insert " Anonymous \n"
	     " ####### ####### ######  \n"
	     " #          #    #     # \n"
	     " #          #    #     # \n"
	     " #####      #    ######  \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     "\n\n")
     (insert "You can get the message content by FTP\n\n")
     (mew-insert "Site:\t%s\n" site)
     (mew-insert "File:\t%s\n" filepath)
     (mew-insert "Size:\t%s bytes\n" size)
     (if (null execute)
	 (insert "\nTo get this file, type "
		 (substitute-command-keys
		  "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))
       (setq remotefile (format "/%s@%s:%s" "anonymous" site filepath))
       (if (string-match "^[/\:]" filepath)
	   (setq url (format "ftp://%s%s" site filepath))
	 (setq url (format "ftp://%s/%s" site filepath)))
       (setq localfile (mew-summary-input-file-name "Save to: " name))
       (setq lfname (file-name-nondirectory localfile))
       (if (file-exists-p localfile)
	   (if (y-or-n-p (format "%s exists. Overwrite? " lfname))
	       (delete-file localfile)
	     (setq getit nil)
	     (message "The file wasn't retrieved")))
       (if (not getit)
	   ()
	 (cond
	  ((eq mew-ext-anon-ftp-method 'ftp)
	   (mew-ext-copy-file-internal remotefile localfile mew-mail-address))
	  ((eq mew-ext-anon-ftp-method 'http)
	   (require 'w3)
	   (w3-fetch url))))))))

(defun mew-ext-mail-server (begin end params execute)
  (mew-elet
   (let ((server (mew-syntax-get-param params "server"))
	 (subject (mew-syntax-get-param params "subject"))
	 (size (mew-syntax-get-param params "size"))
	 (start nil))
     (mew-erase-buffer)
     (insert " #     #    #      ###   #\n"
	     " ##   ##   # #      #    #\n"
	     " # # # #  #   #     #    #\n"
	     " #  #  # #     #    #    #\n"
	     " #     # #######    #    #\n"
	     " #     # #     #    #    #\n"
	     " #     # #     #   ###   #######\n"
	     "\n\n")
     (insert "You can get the message by e-mail\n\n")
     (mew-insert "Server:\t\t%s\n" server)
     (mew-insert "Size:\t%s bytes\n" size)
     (save-excursion
       (set-buffer (mew-current-get 'cache))
       (save-restriction
	 (narrow-to-region begin end)
	 (goto-char (point-min))
	 ;; find a phantom body (in RFC1521)
	 (re-search-forward "^$" nil t)
	 (forward-line)
	 (setq start (point))))
     ;; pickd up source from 'mew-send
     (if (null execute)
	 (insert "\nTo send this mail, type "
		 (substitute-command-keys
		  "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))
       (mew-summary-send server nil subject)
       (goto-char (point-max))
       (insert-buffer-substring (mew-current-get 'cache) start end)
       (mew-draft-make-message)
       (if (y-or-n-p "Send this message? ")
	   (mew-draft-real-send-letter))))))

(defun mew-ext-url (begin end params execute)
  (mew-elet
   (let ((url (mew-syntax-get-param params "url"))
	 (size (mew-syntax-get-param params "size"))
	 (process-connection-type mew-connection-type1))
     (mew-erase-buffer)
     (insert "#     # ######  #\n"
	     "#     # #     # #\n"
	     "#     # #     # #\n"
	     "#     # ######  #\n"
	     "#     # #   #   #\n"
	     "#     # #    #  #\n"
	     " #####  #     # #######\n"
	     "\n\n")
     (mew-insert "URL:\t\t%s\n" url)
     (mew-insert "Size:\t%s bytes\n" size)
     (insert (format "Browser:\t%s\n"
		     (cond ((and (symbolp mew-ext-prog-url)
				 (fboundp mew-ext-prog-url))
			    (symbol-name mew-ext-prog-url))
			   ((stringp mew-ext-prog-url) mew-ext-prog-url)
			   (t "none"))))
     (if (null execute)
	 (insert "\nTo show this URL, type "
		 (substitute-command-keys
		  "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))
       (cond
	((and (symbolp mew-ext-prog-url) (fboundp mew-ext-prog-url))
	 (funcall mew-ext-prog-url url))
	((equal mew-ext-prog-url "w3")
	 (require 'w3)
	 (w3-fetch-other-frame url))
	(t
	 (apply (function start-process)
		(format "*mew %s*" mew-ext-prog-url)
		mew-buffer-tmp mew-ext-prog-url 
		(append mew-ext-prog-url-args (list url)))))))))

;;
;; include
;;

(defun mew-ext-include-local-file (params)
  (mew-flet
   (let* ((file (mew-syntax-get-param params "name")))
     (if (file-exists-p file)
	 (insert-file-contents file)))))

(provide 'mew-ext)

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

;;; mew-ext.el ends here
