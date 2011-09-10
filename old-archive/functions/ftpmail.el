;; ftpmail.el - Some functions for easy ftpmail editing & saving
;;
;; Version 0.2
;;
;; Copyright (C) 1993 Sascha Wildner <swildner@channelz.GUN.de>
;;
;; This file is part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License is available by anonymous ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;
;;
;; To install ftpmail.el, copy it to a lisp directory, byte-compile-file it and
;; put the following line in your .emacs file:
;;
;; (autoload 'ftpmail "ftpmail" "Send mail to an ftpmail server." t)
;;
;;
;; The following functions (and their default bindings) are added to mail mode:
;;
;; C-c C-r	 insert a new ftpmail command
;;	C-c C-r C-b    btoa		C-c C-r C-c    cd
;;	C-c C-r C-p    compress		C-c C-r C-k    delete
;;	C-c C-r C-d    dir	       	C-c C-r C-f b  force btoa
;;	C-c C-r C-f m  force mime	C-c C-r C-f u  force uuencode
;;	C-c C-r C-t    get		C-c C-r C-z    gzip
;;	C-c C-r C-h    help		C-c C-r C-l    ls
;;	C-c C-r C-m    mime		C-c C-r C-e a  mode ascii
;;	C-c C-r C-e b  mode binary	C-c C-r C-n b  no btoa
;;	C-c C-r C-n c  no compress	C-c C-r C-n z  no gzip
;;	C-c C-r C-n m  no mime		C-c C-r C-n u  no uuencode
;;	C-c C-r C-o    open		C-c C-r C-q    quit
;;	C-c C-r C-r    reply-to		C-c C-r C-s    size
;;	C-c C-r C-u    uuencode
;;
;; When in Rmail mode, the default save name of uuencoded files is determined
;; by looking at the begin line and attaching ".uu" to the file name.  This
;; is done by the function ftpmail-uuencoded-save-name.
;;
;; There are two variables:
;;
;; ftpmail-default-ftpmail-host
;;	This is the default ftpmail server (for the To: field).
;;
;; ftpmail-default-ftp-host
;;	This is the default ftp server (for the open command).
;;
;;
;; --- HISTORY ---
;; 16 Aug 93 Sascha Wildner <swildner@channelz.GUN.de>
;;		Created ftpmail.el
;;
;; 16 Nov 93 Sascha Wildner <swildner@channelz.GUN.de>
;;		Added function ftpmail-other-window
;;		Arguments for ftpmail commands are now read via the minibuffer
;;
;; 19 Nov 93 Sascha Wildner <swildner@channelz.GUN.de>
;;		Added minibuffer message to ftpmail-quit
;;
;; 02 Dec 93 Sascha Wildner <swildner@channelz.GUN.de>
;;		Arguments for ftpmail commands are now read via (interactive)
;;		ftpmail-reply-to will use user's address as default
;;
;; 04 Dec 93 Mike Long <mike.long@analog.com>
;;		Removed reference to free variable p
;;
;; 07 Dec 93 Sascha Wildner <swildner@channelz.GUN.de>
;;		Added function ftpmail-uuencoded-save-name
;;		In Rmail mode, the default save name for uuencoded files is
;;		  now determined by ftpmail-uuencoded-save-name
;;		Changed ftpmail-get's binding to C-c C-r C-t
;;
;;
;; LCD Archive Entry:
;; ftpmail.el|Sascha Wildner|swildner@channelz.GUN.de|
;; Some functions for easy ftpmail editing & saving.|
;; 07-Dec-1993|0.2|~/functions/ftpmail.el.Z|


(defvar ftpmail-default-ftpmail-host "ftpmail@decwrl.dec.com"
  "The default destination for ftpmail mails.")

(defvar ftpmail-default-ftp-host "prep.ai.mit.edu"
  "The default host for ftpmail queries.")


(setq mail-setup-hook
      '(lambda ()
	 (define-key mail-mode-map "\C-c\C-r\C-b" 'ftpmail-btoa)
	 (define-key mail-mode-map "\C-c\C-r\C-c" 'ftpmail-cd)
	 (define-key mail-mode-map "\C-c\C-r\C-p" 'ftpmail-compress)
	 (define-key mail-mode-map "\C-c\C-r\C-k" 'ftpmail-delete)
	 (define-key mail-mode-map "\C-c\C-r\C-d" 'ftpmail-dir)
	 (define-key mail-mode-map "\C-c\C-r\C-fb" 'ftpmail-force-btoa)
	 (define-key mail-mode-map "\C-c\C-r\C-fm" 'ftpmail-force-mime)
	 (define-key mail-mode-map "\C-c\C-r\C-fu" 'ftpmail-force-uuencode)
	 (define-key mail-mode-map "\C-c\C-r\C-t" 'ftpmail-get)
	 (define-key mail-mode-map "\C-c\C-r\C-z" 'ftpmail-gzip)
	 (define-key mail-mode-map "\C-c\C-r\C-h" 'ftpmail-help)
	 (define-key mail-mode-map "\C-c\C-r\C-l" 'ftpmail-ls)
	 (define-key mail-mode-map "\C-c\C-r\C-m" 'ftpmail-mime)
	 (define-key mail-mode-map "\C-c\C-r\C-ea" 'ftpmail-mode-ascii)
	 (define-key mail-mode-map "\C-c\C-r\C-eb" 'ftpmail-mode-binary)
	 (define-key mail-mode-map "\C-c\C-r\C-nb" 'ftpmail-no-btoa)
	 (define-key mail-mode-map "\C-c\C-r\C-nc" 'ftpmail-no-compress)
	 (define-key mail-mode-map "\C-c\C-r\C-nz" 'ftpmail-no-gzip)
	 (define-key mail-mode-map "\C-c\C-r\C-nm" 'ftpmail-no-mime)
	 (define-key mail-mode-map "\C-c\C-r\C-nu" 'ftpmail-no-uuencode)
	 (define-key mail-mode-map "\C-c\C-r\C-o" 'ftpmail-open)
	 (define-key mail-mode-map "\C-c\C-r\C-q" 'ftpmail-quit)
	 (define-key mail-mode-map "\C-c\C-r\C-r" 'ftpmail-replyto)
	 (define-key mail-mode-map "\C-c\C-r\C-s" 'ftpmail-size)
	 (define-key mail-mode-map "\C-c\C-r\C-u" 'ftpmail-uuencode)))

(setq rmail-mode-hook
      '(lambda ()
	 (setq rmail-output-file-alist '(("^begin [0-7][0-7][0-7] .+"
					  .
					  (ftpmail-uuencoded-save-name))))))


(defun ftpmail-btoa ()
  "Insert a \"btoa\" command."
  (interactive)
  (insert "btoa\n"))

(defun ftpmail-cd (dir)
  "Insert a \"cd\" command."
  (interactive "scd ")
  (if (not (string= "" dir))
      (insert "cd " dir "\n")))

(defun ftpmail-compress ()
  "Insert a \"compress\" command."
  (interactive)
  (insert "compress\n"))

(defun ftpmail-delete (file)
  "Insert a \"delete\" command."
  (interactive "sdelete ")
  (if (not (string= "" file))
      (insert "delete " file "\n")))

(defun ftpmail-dir (&optional dir)
  "Insert a \"dir\" command."
  (interactive "sdir ")
  (insert "dir " dir "\n"))

(defun ftpmail-force-btoa ()
  "Insert a \"force btoa\" command."
  (interactive)
  (insert "force btoa\n"))

(defun ftpmail-force-mime ()
  "Insert a \"force mime\" command."
  (interactive)
  (insert "force mime\n"))

(defun ftpmail-force-uuencode ()
  "Insert a force \"uuencode\" command."
  (interactive)
  (insert "force uuencode\n"))

(defun ftpmail-get (file)
  "Insert a \"get\" command."
  (interactive "sget ")
  (if (not (string= "" file))
      (insert "get " file "\n")))

(defun ftpmail-gzip ()
  "Insert a \"gzip\" command."
  (interactive)
  (insert "gzip\n"))

(defun ftpmail-help ()
  "Insert a \"help\" command."
  (interactive)
  (insert "help\n"))

(defun ftpmail-ls (&optional dir)
  "Insert an \"ls\" command."
  (interactive "sls ")
  (insert "ls " dir "\n"))

(defun ftpmail-mime ()
  "Insert a \"mime\" command."
  (interactive)
  (insert "mime\n"))

(defun ftpmail-mode-ascii ()
  "Insert a \"mode ascii\" command."
  (interactive)
  (insert "mode ascii\n"))

(defun ftpmail-mode-binary ()
  "Insert a \"mode binary\" command."
  (interactive)
  (insert "mode binary\n"))

(defun ftpmail-no-btoa ()
  "Insert a \"no btoa\" command."
  (interactive)
  (insert "no btoa\n"))

(defun ftpmail-no-compress ()
  "Insert a \"no compress\" command."
  (interactive)
  (insert "no compress\n"))

(defun ftpmail-no-gzip ()
  "Insert a \"no gzip\" command."
  (interactive)
  (insert "no gzip\n"))

(defun ftpmail-no-mime ()
  "Insert a \"no mime\" command."
  (interactive)
  (insert "no mime\n"))

(defun ftpmail-no-uuencode ()
  "Insert a no \"uuencode\" command."
  (interactive)
  (insert "no uuencode\n"))

(defun ftpmail-open (host)
  "Insert an \"open\" command."
  (interactive "sopen ")
  (if (string= "" host)
      (if (not (string= "" ftpmail-default-ftp-host))
	  (progn
	    (setq host ftpmail-default-ftp-host)
	    (insert "open " host "\n")))
    (insert "open " host "\n")))

(defun ftpmail-quit ()
  "Append a \"quit\" command."
  (interactive)
  (goto-char (point-max))
  (insert "quit\n")
  (message (substitute-command-keys "Type \\[mail-send-and-exit] to send mail and exit.")))

(defun ftpmail-replyto (addr)
  "Insert a \"reply-to\" command."
  (interactive "sreply-to ")
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator nil t)
    (beginning-of-line)
    (next-line 2)
    (let ((p (point)))
      (previous-line 1)
      (if (not (search-forward "reply-to " p t))
	  (progn
	    (if (string= "" addr)
		(setq addr (concat (user-login-name) "@" (system-name))))
	    (insert "reply-to " addr "\n"))))))

(defun ftpmail-size (size)
  "Insert a \"size\" command."
  (interactive "ssize ")
  (if (not (string= "" size))
      (insert "size " size "\n")))

(defun ftpmail-uuencode ()
  "Insert a \"uuencode\" command."
  (interactive)
  (insert "uuencode\n"))


(defun ftpmail-uuencoded-save-name ()
  "Return a file name for a uuencoded file."
  (save-excursion
    (re-search-forward "^begin [0-7][0-7][0-7] .+" (point-max) t)
    (concat (buffer-substring (+ 10 (match-beginning 0)) (match-end 0)) ".uu")))


(defun ftpmail ()
  "Send mail to an ftpmail server."
  (interactive)
  (mail nil ftpmail-default-ftpmail-host "request"))

(defun ftpmail-other-window ()
  "Send mail to an ftpmail server in another window."
  (interactive)
  (mail-other-window nil ftpmail-default-ftpmail-host "request"))
