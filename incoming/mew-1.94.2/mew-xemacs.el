;;; mew-xemacs.el --- Environment of XEmacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-xemacs-version "mew-xemacs.el version 0.09")

;;
;; Common
;;

(cond
 ((not mew-icon-p)
  (defvar mew-icon-directory nil)
  (defvar mew-icon-separate-spec nil)
  (defvar mew-icon-blank nil)
  (defvar mew-icon-audio nil)
  (defvar mew-icon-image nil)
  (defvar mew-icon-video nil)
  (defvar mew-icon-application/postscript nil)
  (defvar mew-icon-application/octet-stream nil)
  (defvar mew-icon-message/rfc822 nil)
  (defvar mew-icon-message/external-body nil)
  (defvar mew-icon-text nil)
  (defvar mew-icon-multipart nil)
  (defvar mew-icon-unknown nil)
  (defvar mew-summary-toolbar nil)
  (defvar mew-draft-toolbar nil))
 (t
  (defvar mew-icon-directory nil)

  (defvar mew-icon-separate
    (toolbar-make-button-list
     (expand-file-name "mew-sep.xpm" mew-icon-directory)))

  (defvar mew-icon-separate-spec
    (list [mew-icon-separate nil nil ""]))

  (defvar mew-icon-blank
    (toolbar-make-button-list
     (expand-file-name "mew-Blank.xpm" mew-icon-directory)))

  (defvar mew-icon-audio 
    (toolbar-make-button-list
     (expand-file-name "mew-Audio.xpm" mew-icon-directory)))

  (defvar mew-icon-image
    (toolbar-make-button-list
     (expand-file-name "mew-Image.xpm" mew-icon-directory)))

  (defvar mew-icon-video
    (toolbar-make-button-list
     (expand-file-name "mew-Video.xpm" mew-icon-directory)))

  (defvar mew-icon-application/postscript
    (toolbar-make-button-list
     (expand-file-name "mew-Postscript.xpm" mew-icon-directory)))

  (defvar mew-icon-application/octet-stream
    (toolbar-make-button-list
     (expand-file-name "mew-Octet-Stream.xpm" mew-icon-directory)))

  (defvar mew-icon-message/rfc822
    (toolbar-make-button-list
     (expand-file-name "mew-Rfc822.xpm" mew-icon-directory)))

  (defvar mew-icon-message/external-body
    (toolbar-make-button-list
     (expand-file-name "mew-External.xpm" mew-icon-directory)))

  (defvar mew-icon-text
    (toolbar-make-button-list
     (expand-file-name "mew-Text.xpm" mew-icon-directory)))

  (defvar mew-icon-multipart
    (toolbar-make-button-list
     (expand-file-name "mew-Folder.xpm" mew-icon-directory)))

  (defvar mew-icon-unknown
    (toolbar-make-button-list
     (expand-file-name "mew-Unknown.xpm" mew-icon-directory)))

  ;;
  ;; Summary mode
  ;;

  (defvar mew-summary-toolbar-icon-show
    (toolbar-make-button-list
     (expand-file-name "mew-show.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-next
    (toolbar-make-button-list
     (expand-file-name "mew-next.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-prev
    (toolbar-make-button-list
     (expand-file-name "mew-prev.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-inc
    (toolbar-make-button-list
     (expand-file-name "mew-inc.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-write
    (toolbar-make-button-list
     (expand-file-name "mew-write.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-reply
    (toolbar-make-button-list
     (expand-file-name "mew-reply.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-forward
    (toolbar-make-button-list
     (expand-file-name "mew-forward.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar-icon-refile
    (toolbar-make-button-list
     (expand-file-name "mew-refile.xpm" mew-icon-directory)))

  (defvar mew-summary-toolbar
    '(
      [mew-summary-toolbar-icon-show
       mew-summary-show
       t
       "Read Forward"]
      [mew-summary-toolbar-icon-next
       mew-summary-display-down
       t
       "Show Next Message"]
      [mew-summary-toolbar-icon-prev
       mew-summary-display-up
       t
       "Show Previous Message"]
      [mew-summary-toolbar-icon-inc
       mew-summary-get
       t
       "Check New Messages"]
      [mew-summary-toolbar-icon-write
       mew-summary-send
       t
       "Write Message"]
      [mew-summary-toolbar-icon-reply
       mew-summary-reply
       t
       "Reply to This Message"]
      [mew-summary-toolbar-icon-forward
       mew-summary-forward
       t
       "Forward This Message"]
      [mew-summary-toolbar-icon-refile
       mew-summary-refile
       t
       "Refile This Message"]
      ))

  ;;
  ;; Draft mode
  ;;

  (defvar mew-draft-toolbar-icon-comp
    (toolbar-make-button-list
     (expand-file-name "mew-comp.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-send
    (toolbar-make-button-list
     (expand-file-name "mew-send.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-attach
    (toolbar-make-button-list
     (expand-file-name "mew-attach.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-cite
    (toolbar-make-button-list
     (expand-file-name "mew-cite.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-yank
    (toolbar-make-button-list
     (expand-file-name "mew-yank.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-pgp-sign
    (toolbar-make-button-list
     (expand-file-name "mew-pgp-sign.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-pgp-enc
    (toolbar-make-button-list
     (expand-file-name "mew-pgp-enc.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar-icon-pgp-sigenc
    (toolbar-make-button-list
     (expand-file-name "mew-pgp-sigenc.xpm" mew-icon-directory)))

  (defvar mew-draft-toolbar
    '(
      [mew-draft-toolbar-icon-comp
       mew-draft-make-message
       (mew-header-p)
       "Compose Message"]
      [mew-draft-toolbar-icon-send
       mew-draft-send-letter
       (not (mew-header-p))
       "Send Message"]
      [mew-draft-toolbar-icon-cite
       mew-draft-cite
       (mew-header-p)
       "Cite Message"]
      [mew-draft-toolbar-icon-yank
       mew-draft-yank
       (mew-header-p)
       "Cite Message without Label"]
      [mew-draft-toolbar-icon-attach
       mew-draft-prepare-attachments
       (and (mew-header-p) (not (mew-attach-p)))
       "Prepare Attachments"]
      [mew-draft-toolbar-icon-pgp-sign
       mew-pgp-sign-letter
       (mew-header-p)
       "Sign Message with PGP"]
      [mew-draft-toolbar-icon-pgp-enc
       mew-pgp-encrypt-letter
       (mew-header-p)
       "Encrypt Message with PGP"]
      [mew-draft-toolbar-icon-pgp-sigenc
       mew-pgp-sign-encrypt-letter
       (mew-header-p)
       "Sign Then Encrypt Message with PGP"]
      ))

  ;;
  ;; Button
  ;; 

  (define-key toolbar-map 'button3   'pressed-and-activate-toolbar-button)
  (define-key toolbar-map 'button3up 'release-and-activate-toolbar-button)

  (defun mew-summary-button ()
    "Call back function for toolbar of Summary mode. 
If event is button 1, show a part.
If event is button 3, show a menu."
    (interactive)
    (let* ((msg (mew-summary-message-number))
	   (part (mew-syntax-number))
	   (nums (mew-syntax-number-to-nums part))
	   (button (event-button last-command-event)))
      (mew-summary-goto-part msg part)
      (mew-summary-recenter)
      (cond
       ((eq button 1)
	(mew-summary-show-part msg nums))
       ((eq button 3)
	(popup-menu mew-summary-mode-toolbar-menu)))))

  (defun mew-summary-show-part (msg nums)
    "Show a part according to the clicked icon."
    (interactive)
    (let ((fld (mew-summary-folder-name))
	  (ofld-msg (mew-current-get 'message))
	  (buf (buffer-name)))
      (if (null nums) 
	  (message "No message")
	(mew-summary-toggle-disp-msg 'on)
	(unwind-protect
	    (progn
	      (mew-window-configure buf 'message)
	      ;; message buffer
	      (mew-summary-display-part 
	       (mew-cache-decode-syntax (mew-cache-hit ofld-msg)) nums))
	  (mew-pop-to-buffer buf)))))

  (defun mew-draft-button ()
    "Call back function for toolbar of Draft mode. 
If event is button 1, show a part.
If event is button 3, show a menu."
    (interactive)
    (let ((nums (mew-syntax-nums))
	  (button (event-button last-command-event)))
      (mew-attach-goto-number 'here nums)
      (cond
       ((eq button 1)
	(mew-draft-show-attach nums))
       ((eq button 3)
	(popup-menu mew-draft-mode-toolbar-menu)))))

  ;; This is a toy at present. Support only CT: Image/*.
  ;; To make Summary and Draft symmetric, left button click on icon
  ;; should display the attachment. 
  (defun mew-draft-show-attach (nums)
    "Show a part according to the clicked icon."
    (interactive)
    (let ((case-fold-search t)
	  (str (toolbar-button-help-string last-pressed-toolbar-button))
	  (image-extent (extent-at (point-max) nil nil nil 'at))
	  ct)
      (if (and image-extent (glyphp image-extent))
	  (mew-overlay-delete image-extent))
      (if (null (string-match "(\\(.*\\))" str))
	  ()
	(setq ct (mew-match 1 str))
	(if (string-match "^Image/" ct)
	    (let* ((subdir (mew-attach-expand-path mew-encode-syntax nums))
		   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
		   (name (mew-syntax-get-file syntax))
		   (ename (if (equal subdir "") name (concat subdir name)))
		   (file (expand-file-name ename (mew-attachdir)))
		   (attr (mew-attr-by-ct ct))
		   (program (mew-attr-get-prog attr))
		   (options (mew-attr-get-opt attr))
		   (async   (mew-attr-get-async attr))
		   (zmacs-regions nil) ;; for XEmacs
		   (format (cond
			    ((and (string-match "jpeg" ct)
				  (valid-image-instantiator-format-p 'jpeg))
			     'jpeg)
			    ((and (string-match "gif" ct)
				  (valid-image-instantiator-format-p 'gif)) 
			     'gif)
			    ((and (string-match "xbm" ct)
				  (valid-image-instantiator-format-p 'xbm))
			     'xbm)
			    ((and (string-match "xpm" ct)
				  (valid-image-instantiator-format-p 'xpm))
			     'xpm)
			    ((and (string-match "png" ct)
				  (valid-image-instantiator-format-p 'png))
			     'png)
			    (t nil)))
		   glyph) 
	      (if format
		  (progn
		    (message "Loading image...")
		    (setq glyph (make-glyph (vector format :file file)))
		    (if (eq format 'xbm)
			(set-glyph-property glyph 'face 'x-face))
		    (set-extent-begin-glyph
		     (mew-overlay-make (point-max) (point-max)) glyph)
		    (message "Loading image...done."))
		(if (and (stringp program) (mew-which program exec-path))
		    (if async
			(mew-mime-start-process program options file)
		      (mew-mime-call-process program options file)))))))))

  (defun pressed-and-activate-toolbar-button (event)
    "A replacement function 'press-toolbar-button' so that
popup menu can be implemented."
    (interactive "_e")
    (or (button-press-event-p event)
	(error "%s must be invoked by a mouse-press" this-command))
    (let ((button (event-toolbar-button event)) callback)
      (if (null (toolbar-button-p button))
	  ()
	(setq last-pressed-toolbar-button button)
	(if (and (setq callback (toolbar-button-callback button))
		 (or (equal callback 'mew-summary-button)
		     (equal callback 'mew-draft-button)))
	    (if (null (toolbar-button-enabled-p button))
		()
	      ;; (setq toolbar-active t) is meaningless... why?
	      (setq this-command callback)
	      (if (symbolp callback)
		  (call-interactively callback)
		(eval callback)))
	  ;; emulate press-toolbar-button
	  (setq this-command last-command)
	  (setq toolbar-active t)
	  (set-toolbar-button-down-flag button t)))))

  )) ;; end of cond


(defmacro mew-summary-toolbar-update ()
  '(if mew-icon-p
       (set-specifier default-toolbar
		      (cons (current-buffer) mew-summary-toolbar))))

(defmacro mew-draft-toolbar-update ()
  '(if mew-icon-p
       (set-specifier default-toolbar
		      (cons (current-buffer) mew-draft-toolbar))))

(defvar mew-x-emacs-end-of-message nil)
(defvar mew-x-emacs-end-of-part nil)

(defmacro mew-message-set-end-of-message ()
  '(progn
     (if (not (glyphp mew-x-emacs-end-of-message))
	 (setq mew-x-emacs-end-of-message
	       (make-glyph
		(vector 'string :data mew-end-of-message-string))))
     (mew-overlay-put mew-message-overlay
		      'begin-glyph
		      mew-x-emacs-end-of-message)))

(defmacro mew-message-set-end-of-part ()
  '(progn
     (if (not (glyphp mew-x-emacs-end-of-part))
	 (setq mew-x-emacs-end-of-part
	       (make-glyph
		(vector 'string :data mew-end-of-part-string))))
     (mew-overlay-put mew-message-overlay
		      'begin-glyph
		      mew-x-emacs-end-of-part)))

(defmacro mew-message-set-end-of-nil ()
  '(mew-overlay-put mew-message-overlay 'begin-glyph nil))
  
(provide 'mew-xemacs)

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

;;; mew-xemacs.el ends here
