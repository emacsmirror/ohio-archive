;From: jv@mhres.mh.nl (Johan Vromans)
;Newsgroups: comp.emacs
;Subject: GNU Emacs startup service
;Message-ID: <3468@mhres.mh.nl>
;Date: 19 Aug 89 09:01:54 GMT
;Organization: Multihouse NV, the Netherlands
;Lines: 76
;
;I like GNU Emacs to be started from my .profile when I login, and I
;use the following startup file to have Emacs present me screenful of
;things I want to know when I login, e.g. mail status, disk status
;etc.. It could invoke the mail handler for me, but my mail is
;automatically fetched by Emacs as soon as it appears.
;
;Take it for what it's worth, I like it.
;
;;; StartUp.el
;;;
;;; Automatic services performed upon startup
;;;
;;; Usage: enter the following line in your .profile or .login:
;;;
;;;	emacs -l StartUp.el
;;;
;;; Note: BSD sites need to change "remsh" into "rsh".
;;;
;;; This file is hereby declared Public Domain
;;;
;;;	1989, Johan Vromans

;; setup the startup buffer
(setq StartUp-buffer "*StartUp*")
(get-buffer-create StartUp-buffer)
(switch-to-buffer StartUp-buffer)
(erase-buffer)

(insert "Starting up...\n\n")

(defun jv-check-file (file msg)
  "Checks if FILE exists and has a non-zero length. Inserts MESSAGE if
this is true."
  (if (and
       (file-exists-p file)
       (> (nth 7 (file-attributes file)) 0))
      (insert (concat msg "\n"))))

(jv-check-file "~/maildir/ToDo"   "There are things to be done.")
(jv-check-file "~/maildir/printq" "There are things to be printed.")
(jv-check-file "~/maildir/risks"  "There are RISKs to be read.")

(defun jv-check-mail-file (user)
  "Checks if there is a non-empty mailbox for USER. Inserts a message if so."
  (let ((the-file (concat rmail-spool-directory user)))
    (if (and
	 (file-exists-p the-file)
	 (> (nth 7 (file-attributes the-file)) 0))
	(insert (concat "There is mail for " user ".\n")))))

;; Check mine and other relevant mail files
(mapcar 'jv-check-mail-file 
	'("jv" "root" "system" "news" "mailer" "mems" "uucp"))

;; Disk status on this system
(insert "\nDisk Status:\n")
(call-process "df" nil t nil)

;; Disk status on the news archive.
(insert "\nDisk Status on mh_eng:\n")
(call-process "remsh" nil t nil "mh_eng" "df")

(insert "\n")
(call-process "fortune" nil t nil)

;; Wrap up
(insert "\nStartup complete.\n")
(goto-char (point-min))
(set-buffer-modified-p nil)

;;; End of StatUp.el
;-- 
;Johan Vromans				       jv@mh.nl via internet backbones
;Multihouse Automatisering bv		       uucp: ..!{mcvax,hp4nl}!mh.nl!jv
;Doesburgweg 7, 2803 PL Gouda, The Netherlands  phone/fax: +31 1820 62944/62500
;------------------------ "Arms are made for hugging" -------------------------
