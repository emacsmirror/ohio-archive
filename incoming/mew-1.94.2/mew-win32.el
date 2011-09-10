;;; mew-win32.el --- Settings for Mew on Win32

;; Author:  Shuichi Kitaguchi <kit@Mew.org>
;; Created: Dec 05, 1997
;; Revised: Nov 20, 1999

;;; Code:

(defconst mew-win32-version "mew-win32.el 0.33.1")

;;; for NTEmacs User
;;
;; put mw32script.el(in Meadow's archive) into load-path.
;;

;;
;;  ~/.emacs settings
;;

;;; for PGP/MIME
;;    mew-prog-pgp           ...  executable name for PGP/MIME
;;
;;; example
;;    (setq mew-prog-pgp  "pgp.exe")      ;; PGP
;;    (setq mew-prog-pgp  "gpg.exe")      ;; GNUPG

;;; for Windows NT with NTFS
;;(eval-after-load "mew-win32"
;;  '(setq mew-touch-folder-p nil))

;;; for PRINTING
;;    mew-w32-prog-print     ...  print command
;;    mew-w32-prog-print-arg ...  print command argument
;;    mew-w32-cs-print       ...  coding-system for printing
;;    define-process-argument-editing  ...  for argument-editing
;;
;;; example
;;   (setq mew-w32-prog-print     "notepad.exe")
;;   (setq mew-w32-prog-print-arg "/p")
;;   (setq mew-w32-cs-print       '*sjis*dos)      ;; Mule for Win32
;;   (setq mew-w32-cs-print       'shift_jis-dos)  ;; Meadow
;;   (define-process-argument-editing "/notepad\\.exe$"
;;     (lambda (x)
;;       (general-process-argument-editing-function x nil t)))
;;   (setq mew-print-function (function mew-w32-print-buffer))

;; Win32 programs.
(defvar mew-w32-exec           "fiber.exe")
(defvar mew-w32-prog-print     "notepad.exe")
(defvar mew-w32-prog-print-arg nil)


;; Emacs version dependent variables.
(cond
;; ((featurep 'xemacs)	; XEmacs?
;;  (setq mew-prog-shell-arg  shell-command-switch)
  
 ((eq 20 emacs-major-version)
  (if (boundp 'w32-system-coding-system)
      (setq mew-cs-pick w32-system-coding-system))
  (setq mew-prog-shell-arg  shell-command-switch)
  
  (if (featurep 'meadow);; Meadow
      (progn
	(require 'mw32script)
	(mw32script-init))
    (if (require 'mw32script nil t);; NTEmacs
	(progn
	  (mw32script-make-pathext-regexp)
	  (fset 'call-process-original (symbol-function 'call-process))
	  (defun call-process (PROGRAM INFILE BUFFER DISPLAY &rest PROGRAM-ARGS)
	    (let (prog sargs)
	      (setq prog (mw32script-openp PROGRAM))
	      (if (not prog)
		  (progn
		    (setq prog (mew-which PROGRAM exec-path))
		    (setq sargs (mw32script-resolve-script prog))))
	      (if sargs
		  (apply (function call-process-original)
			 (car sargs) INFILE BUFFER DISPLAY
			 prog PROGRAM-ARGS)
		(apply (function call-process-original)
		       PROGRAM INFILE BUFFER DISPLAY PROGRAM-ARGS))))
	  (fset 'start-process-original (symbol-function 'start-process))
	  (defun start-process (NAME BUFFER PROGRAM &rest PROGRAM-ARGS)
	    (let (prog sargs)
	      (setq prog (mw32script-openp PROGRAM))
	      (if (not prog)
		  (progn
		    (setq prog (mew-which PROGRAM exec-path))
		    (setq sargs (mw32script-resolve-script prog))))
	      (if sargs
		  (apply (function start-process-original)
			 NAME BUFFER (car sargs) prog PROGRAM-ARGS)
		(apply (function start-process-original)
		       NAME BUFFER PROGRAM PROGRAM-ARGS))))))
    ))
 (t				 ;; Mule for Windows
  (setq mew-cs-pick         win32-system-coding-system)
  (setq mew-use-timer       nil)
  (setq mew-prog-shell-arg  shell-command-option)
  (require 'win32-script)
  (define-process-argument-editing
    "/[^./]+$"
    'script-process-argument-editing-function 'last)))

;; common programs.
(setq mew-prog-mime-encode  "mewencode.exe")
(setq mew-prog-mime-decode  "mewdecode.exe")
(setq mew-prog-tar          "tar.exe")
(setq mew-prog-compress     "compress.exe")
(setq mew-prog-gzip         "gzip.exe")
(setq mew-prog-shell        shell-file-name)

;(setq mew-prog-pgp          "pgp.exe")
(setq mew-prog-pgp2         "pgp.exe")
(setq mew-prog-pgp5         "pgp.exe")
(setq mew-prog-pgp5e        "pgpe.exe")
(setq mew-prog-pgp5s        "pgps.exe")
(setq mew-prog-pgp5v        "pgpv.exe")
(setq mew-prog-pgp5k        "pgpk.exe")
(setq mew-prog-gpg          "gpg.exe")

(setq mew-touch-folder-p    t)
(setq mew-delete-temp-file  nil)

(setq mew-prog-uncompface   "uncompface.exe")

(setq mew-prog-text/html     mew-w32-exec)
(setq mew-prog-text/html-arg nil)
(setq mew-ext-prog-url       mew-w32-exec)
(setq mew-ext-prog-url-args  nil)


;; printing
(defun mew-w32-print-buffer ()
  (interactive)
  (setq tempfile (mew-make-temp-name))
  (mew-frwlet
   mew-cs-dummy mew-w32-cs-print
   (write-region (point-min) (point-max) tempfile nil nil))
  (setq w32-start-process-show-window t)
  (cond
   ((eq mew-w32-prog-print-arg nil)
    (call-process mew-w32-prog-print nil nil nil tempfile))
   (t
    (call-process mew-w32-prog-print nil nil nil mew-w32-prog-print-arg tempfile)))
  (setq w32-start-process-show-window nil)
  (delete-file tempfile))


;; MIME setting
(defvar mew-prog-plain           '(mew-mime-text/plain     () nil))
(defvar mew-prog-html            '(mew-mime-text/html      () nil))
(defvar mew-prog-enriched        '(mew-mime-text/enriched  () nil))
(defvar mew-prog-text            '(mew-mime-text/plain     () nil))
(defvar mew-prog-audio           (list mew-w32-exec        () t))
(defvar mew-prog-audio2          (list mew-w32-exec        () t))
(defvar mew-prog-gif             (list mew-w32-exec        () t))
(defvar mew-prog-tiff            (list mew-w32-exec        () t))
(defvar mew-prog-jpeg            (list mew-w32-exec        () t))
(defvar mew-prog-xwd             (list mew-w32-exec        () t))
(defvar mew-prog-xbm             (list mew-w32-exec        () t))
(defvar mew-prog-xpm             (list mew-w32-exec        () t))
(defvar mew-prog-png             (list mew-w32-exec        () t))
(defvar mew-prog-bmp             (list mew-w32-exec        () t))
(defvar mew-prog-image           (list mew-w32-exec        () t))
(defvar mew-prog-mpeg            (list mew-w32-exec        () t))
(defvar mew-prog-rfc822          '(mew-mime-message/rfc822 () nil))
(defvar mew-prog-external-body   '(mew-mime-external-body  () nil))
(defvar mew-prog-delivery-status '(mew-mime-text/plain     () nil))
(defvar mew-prog-postscript      (list mew-w32-exec        () t))
(defvar mew-prog-pgp-keys        '(mew-mime-pgp-keys       () nil))
(defvar mew-prog-pdf             (list mew-w32-exec        () t))
(defvar mew-prog-octet-stream    (list mew-w32-exec        () t))

(defvar mew-prog-w32             (list mew-w32-exec        () t))

(setq mew-mime-content-type
      (append
       '(
	 ;; Audio (mew-prog-audio)
	 ("audio/x-wav"     "\\.wav$"   mew-b64 mew-prog-audio mew-icon-audio)
	 ("audio/x-aiff"    "\\.aif?f$" mew-b64 mew-prog-audio mew-icon-audio)
	 ("audio/x-midi"    "\\.midi?$" mew-b64 mew-prog-audio mew-icon-audio)
	 ;; Image (mew-prog-image)
	 ("image/x-pcx"     "\\.pcx$"   mew-b64 mew-prog-image mew-icon-image)
	 ("image/x-tga"     "\\.tga$"   mew-b64 mew-prog-image mew-icon-image)
	 ;; Video (mew-prog-mpeg)
	 ("video/x-msvideo" "\\.avi$"   mew-b64 mew-prog-mpeg  mew-icon-video)
	 ("video/quicktime" "\\.mov$"   mew-b64 mew-prog-mpeg  mew-icon-video)
	 ;; Executable (mew-prog-w32)
	 ("application/octet-stream" "\\.exe$\\|\\.com$\\|\\.cmd$\\|\\.bat$"
	  mew-b64 mew-prog-w32 mew-icon-application/octet-stream)
	 ;; Archive (mew-prog-w32)
	 ("application/octet-stream"
	  "\\.taz$|\\.tar\\.?b?z?2?$\\|\\.bz2?$\\|\\.arj$\\|\\.zoo$\\|\\.rar$|\\.cab$"
	  mew-b64 mew-prog-w32 mew-icon-application/octet-stream)
	 ;; Document (mew-prog-w32)
	 ("application/x-dvi" "\\.dvi$"
	  mew-b64 mew-prog-w32 mew-icon-application/postscript)
	 ;; MS Application (mew-prog-w32)
	 ("application/msword" "\\.doc$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 ("application/vnd.ms-excel" "\\.xls$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 ("application/vnd.ms-powerpoint" "\\.ppt$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 ;; OASYS
	 ("application/vnd.fujitsu.oasys" "\\.oas$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 ("application/vnd.fujitsu.oasys2" "\\.oa2$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 ("application/vnd.fujitsu.oasys3" "\\.oa3$"
	  mew-b64 mew-prog-w32 mew-icon-text)
	 )
       mew-mime-content-type))

(setq mew-mime-content-type-list
      (append
       '(
	 ;; Image
	 "image/x-pcx" "image/x-pic"
	 ;; Audio
	 "audio/x-wav" "audio/x-aiff" "audio/x-midi"
	 ;; Video
	 "video/x-msvideo" "video/quicktime"
	 ;; Document
	 "application/pdf" "application/x-dvi"
	 ;; MS Application
	 "application/msword" "application/vnd.ms-excel"
	 "application/vnd.ms-powerpoint"
	 ;; OASYS
	 "application/vnd.fujitsu.oasys"
	 "application/vnd.fujitsu.oasys2"
	 "application/vnd.fujitsu.oasys3"
	 )
       mew-mime-content-type-list))

(provide 'mew-win32)

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

;;; mew-win32.el ends here
