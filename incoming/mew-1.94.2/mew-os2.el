;;
;;  mew-os2.el
;;     --- OS/2 specific settings & external/internal MIME methods.
;;
;; Author:  OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 18, 1996
;; Revised: Aug 30, 1999


;;; Code
(defconst mew-os2-version "mew-os2.el v0.28")

(defvar mew-os2-load-hook nil
 "*Hook called after mew-os2 has been loaded.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding-system

(setq mew-cs-text-for-write 'mew-cs-text-crlf)
(cond
 ((featurep 'mew-mule2) ;; 2.3@19.x
    (if (not (boundp '*ctext*dos))
      (make-coding-system
       '*ctext* 2		;; xxx
       ?X "Coding-system used in X as Compound Text Encoding."
       t	;; xxx '*ctext*unix, '*ctext*dos, '*ctext*mac, too
        (list lc-ascii lc-ltn1 lc-invalid lc-invalid
          nil 'ascii-eol 'ascii-cntl)))
    (setq mew-cs-scan     '*ctext*dos)
    (setq mew-cs-virtual  mew-cs-scan) ;; remove ^M
    ;; for *.cmd (by NAKAGAWA Takayuki)
    (require 'os2-process)
    )
 ((featurep 'mew-mule3) ;; MULE 3.0
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS/2 & shell specific variables

;; os2-process.el
;;(setq mew-prog-shell (file-name-nondirectory os2-process-comspec-cmd))
;;(setq mew-prog-shell-arg "/c")

;(setq mew-prog-utime ;; NOT fullpath
;  (file-name-nondirectory
;    (os2-process-openp "utime" exec-path os2-process-exec-suffixes t)))
(setq mew-prog-uncompface "uncompface.exe")

(setq mew-prog-mime-encode "mewencode.exe")
(setq mew-prog-mime-decode "mewdecode.exe")

;; nsclient: "http://www.t3.rim.or.jp/~homy/"
(defvar mew-ext-prog-url "nsclient.exe")

(autoload 'browse-url-at-mouse "browse-url" nil t)
(autoload 'browse-url-interactive-arg "browse-url" nil t)

(defun browse-url-nsclient (url)
  (interactive (browse-url-interactive-arg "Netscape URL: "))
  (x-set-selection 'PRIMARY url)
  (start-process "nsclient" nil "nsclient.exe" url))

;;(add-hook 'mew-init-hook (function (lambda ()
;;  (define-key mew-message-mode-map [mouse-2] 'browse-url-at-mouse))))

;;(setq browse-url-browser-function (function browse-url-nsclient))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'mew-summary-x-face (C-cC-x)
;;  rexX-Face
;;    => http://web.kyoto-inet.or.jp/people/fuji0924/os2mew.html

;;(setq mew-x-face-filter (list mew-prog-uncompface "icon2xbm")) ;; rexX-Face/icon2xbm.cmd
;;(setq mew-x-face-prog "d:/tool/pmview/pmview.exe")  ;; PMview, GBM/2
;;(setq mew-x-face-args '("/Wpos=\(,,,,For\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(setq mew-touch-folder-p t)
;; Perl
(setenv "PERL_BADLANG" "0")
(setenv "PERL_BADFREE" "0")

;; Archive interface for OS/2 imcat
(defvar mew-os2/archive-prefix "archived")
(defvar mew-os2/archive-suffix ".zip")

(defconst mew-os2/archive
  (concat mew-os2/archive-prefix mew-os2/archive-suffix))

(defun mew-os2/archive-exist-p (fld)
  (file-exists-p (mew-expand-folder fld mew-os2/archive)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIME methods

(defvar mew-os2/mime-method "mew-mime.cmd")
(defvar mew-prog-plain '(mew-mime-text/plain () nil))
(defvar mew-prog-enriched '(mew-mime-text/enriched () nil))
;;(defvar mew-prog-html '(mew-mime-text/html () nil))
;; Netscape/nsclient: "x:/foo/bar.html" => "file:///x|/foo/bar.html"
(defvar mew-prog-html (list mew-os2/mime-method (list "text/html") t))
(defvar mew-prog-text '(mew-mime-text/plain () nil))
(defvar mew-prog-audio
  (list mew-prog-shell (list mew-prog-shell-arg "cat - > /dev/audio") nil))
(defvar mew-prog-audio2
  (list mew-prog-shell (list mew-prog-shell-arg "cat < /dev/audio") nil))
(defvar mew-prog-gif (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'gif))
			 '(mew-mime-image/gif () nil)
		       (list mew-os2/mime-method (list "image/gif") t)))
(defvar mew-prog-jpeg (if (and window-system mew-xemacs-p
			       (valid-image-instantiator-format-p 'jpeg))
			  '(mew-mime-image/jpeg () nil)
			(list mew-os2/mime-method (list "image/jpeg") t)))
(defvar mew-prog-xwd (list mew-os2/mime-method (list "image/x-xwd") t))
(defvar mew-prog-xbm
  (if (fboundp 'bitmap-insert-xbm-buffer)  ;; bitmap-mule/bitmap.el
      '(mew-mime-image/x-xbm-for-mule () nil)
    (if (and window-system mew-xemacs-p
	     (valid-image-instantiator-format-p 'xbm)) ;; XEmacs
	'(mew-mime-image/xbm () nil)
      (list mew-os2/mime-method (list "image/x-xbm") t))))

(defvar mew-prog-bmp (list mew-os2/mime-method (list "image/x-bmp") t))
(defvar mew-prog-image (list mew-os2/mime-method (list "image/oth") t))
(defvar mew-prog-mpeg (list mew-os2/mime-method (list "video/mpeg") t))
(defvar mew-prog-rfc822 '(mew-mime-message/rfc822 () nil))
(defvar mew-prog-external-body '(mew-mime-external-body () nil))
(defvar mew-prog-delivery-status '(mew-mime-text/plain () nil))
(defvar mew-prog-postscript (list mew-os2/mime-method (list "application/postscript") t))
(defvar mew-prog-pgp-keys '(mew-mime-pgp-keys () nil))
(defvar mew-prog-octet-stream '(mew-mime-application/octet-stream () nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other MIME Content-Types & methods

;; Audio
(defvar mew-prog-wav
  (list mew-os2/mime-method (list "audio/x-wav") t))
(defvar mew-prog-aiff
  (list mew-os2/mime-method (list "audio/x-aiff") t))
(defvar mew-prog-midi
  (list mew-os2/mime-method (list "audio/x-midi") t))

;; Image
(defvar mew-prog-os2-bmp
  (list mew-os2/mime-method (list "image/x-os2-bmp") t))
(defvar mew-prog-tiff
  (list mew-os2/mime-method (list "image/tiff") t))
(defvar mew-prog-pic
  (list mew-os2/mime-method (list "image/x-pic") t))
(defvar mew-prog-pcx
  (list mew-os2/mime-method (list "image/x-pcx") t))
(defvar mew-prog-prm
  (list mew-os2/mime-method (list "image/x-prm") t))
(defvar mew-prog-pgm
  (list mew-os2/mime-method (list "image/x-pgm") t))
(defvar mew-prog-xpm
  (list mew-os2/mime-method (list "image/x-xpm") t))
(defvar mew-prog-png (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'png))
			 '(mew-mime-image/png () nil)
		       (list mew-os2/mime-method (list "image/png") t)))
(defvar mew-prog-meta
  (list mew-os2/mime-method (list "image/x-os2-meta") t))

;; Movie
(defvar mew-prog-msvideo
  (list mew-os2/mime-method (list "video/x-msvideo") t))
(defvar mew-prog-qt
  (list mew-os2/mime-method (list "video/quicktime") t))

;; misc
(defvar mew-prog-pdf
  (list mew-os2/mime-method (list "application/pdf") t))
(defvar mew-prog-mac
  (list mew-os2/mime-method (list "application/mac-binhex40") t))
(defvar mew-prog-view
  (list mew-os2/mime-method (list "application/x-os2-inf") t))
(defvar mew-prog-viewhelp
  (list mew-os2/mime-method (list "application/x-os2-hlp") t))
(defvar mew-prog-caesar-table
  (if (fboundp 'mew-mime-text/x-rot13-47)
      '(mew-mime-text/x-rot13-47 () nil)
    '(mew-mime-text/plain () nil)))


(setq mew-mime-content-type (append '(
 ;; Audio
   ("audio/x-wav"
    "\\.wav$"
    mew-b64
    mew-prog-wav
    mew-icon-audio
    )
   ("audio/x-aiff"
    "\\.aif?f$"
    mew-b64
    mew-prog-aiff
    mew-icon-audio
    )
   ("audio/x-midi"
    "\\.midi?$"
    mew-b64
    mew-prog-midi
    mew-icon-audio
    )
 ;; Image
   ("image/x-os2-bmp"
    "\\.bmp$"
    mew-b64
    mew-prog-os2-bmp
    mew-icon-image
    )
   ("image/tiff"
    "\\.tif?f$"
    mew-b64
    mew-prog-tiff
    mew-icon-image
    )
   ("image/x-tiff"
    "\\.tif?f$"
    mew-b64
    mew-prog-tiff
    mew-icon-image
    )
   ("image/x-pic"
    "\\.pic$"
    mew-b64
    mew-prog-pic
    mew-icon-image
    )
   ("image/x-pcx"
    "\\.pcx$"
    mew-b64
    mew-prog-pcx
    mew-icon-image
    )
   ("image/x-prm"
    "\\.prm$"
    mew-b64
    mew-prog-prm
    mew-icon-image
    )
   ("image/x-pgm"
    "\\.pgm$"
    mew-b64
    mew-prog-pgm
    mew-icon-image
    )
   ("image/x-xpm"
    "\\.xpm$"
    mew-b64
    mew-prog-xpm
    mew-icon-image
    )
   ("image/x-os2-meta"
    "\\.meta?$"
    mew-b64
    mew-prog-meta
    mew-icon-image
    )
 ;; Movie
   ("video/x-msvideo"
    "\\.avi$"
    mew-b64
    mew-prog-msvideo
    mew-icon-video
    )
   ("video/quicktime"
    "\\.mov$"
    mew-b64
    mew-prog-qt
    mew-icon-video
    )
 ;; misc (binary file)
   ("application/octet-stream"
    "\\.exe$\\|\\.com$\\|\\.cmd$\\|\\.tar\\.?b?z?2?$\\|\\.bz2?$\\|\\.lzh$\\|\\.zip$\\|\\.arj$\\|\\.zoo$\\|\\.rar$\\|\\.img$"
    mew-b64
    mew-prog-octet-stream
    mew-icon-application/octet-stream
    )
   ("application/mac-binhex40"
    "\\.hqx$"
    mew-b64
    mew-prog-mac
    mew-icon-application/octet-stream
    )
   ("application/x-os2-inf"
    "\\.inf$"
    mew-b64
    mew-prog-view
    mew-icon-application/octet-stream
    )
   ("application/x-os2-hlp"
    "\\.hlp$"
    mew-b64
    mew-prog-viewhelp
    mew-icon-application/octet-stream
    )
   ("text/x-rot13-47"
    "\\.rot$"
    nil
    mew-prog-caesar-table
    mew-icon-text
   )
 ) mew-mime-content-type ))


(setq mew-mime-content-type-list (append
  '(
    "application/mac-hexbin40"
    "application/x-os2-inf"
    "application/x-os2-hlp"
    "image/x-os2-bmp" "image/tiff" "image/x-tiff"
    "image/x-pcx" "image/x-pic" "image/x-xpm"
    "image/x-prm" "image/x-pgm" "image/x-os2-meta"
    "audio/x-wav" "audio/x-aiff" "audio/x-midi"
    "video/x-msvideo" "video/quicktime"
    )
    mew-mime-content-type-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal method for image/x-xbm. -- 'bitmap-mule/bitmap.el' required.
;;
(defun mew-mime-image/x-xbm-for-mule (begin end &optional params execute)
  (if (> end begin)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(mew-elet
	 (insert-buffer-substring (mew-current-get 'cache) begin end)
	 (if (or mew-end-of-message-string mew-end-of-part-string) ;; xxx
	     (goto-char (point-max)) ;; necessary?
	   (setq last-point (point-max))
	   (bitmap-insert-xbm-buffer (mew-buffer-message))
	   (delete-region (point-min) last-point)))
	(set-buffer-modified-p nil) ;; xxx
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fasten mew-folder-list() w/ REXX
(defvar mew-prog-rxfolders "RxFolders")

;(if (os2-process-openp mew-prog-rxfolders exec-path '(".exe" ".cmd" "") t)
;    (setq mew-folder-list-function 'mew-os2/folders-list))


(defun mew-os2/folders-list (prefix)
  (let (folder folders-list
	(path (cond
	       ((string= "+" prefix) mew-mail-path)
	       ((string= "=" prefix) mew-news-path))))
    (mew-set-buffer-tmp)
    (call-process mew-prog-rxfolders nil t nil path)
    (goto-char (point-min))
    (while (re-search-forward ".+$" nil t)
      (setq folder (concat prefix (mew-match 0)))
      (setq folders-list (cons folder folders-list)))
    folders-list))


;;; End
(provide 'mew-os2)
(run-hooks 'mew-os2-load-hook)

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

;; mew-os2.el ends here
