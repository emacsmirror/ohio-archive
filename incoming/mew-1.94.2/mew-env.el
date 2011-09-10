;;; mew-env.el --- Environment setup for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  6, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-env-version "mew-env.el version 0.14")

(require 'mew)

(defvar mew-temacs-p nil)
(defvar mew-xemacs-p nil)
(defvar mew-mule-p   nil)
(defvar mew-icon-p   nil)
(defvar mew-mule-ver 0)

(if (featurep 'mule) (setq mew-mule-p t))

(cond
 ((featurep 'xemacs)
  (setq mew-temacs-p nil)
  (setq mew-xemacs-p t)
  (if (and (valid-image-instantiator-format-p 'xpm) (featurep 'toolbar))
      (setq mew-icon-p t))
  (require 'mew-xemacs)
  (if (equal emacs-major-version 19)
      (require 'mew-mule0)
    (if (null mew-mule-p)
	(require 'mew-mule0)
      (setq mew-mule-ver 3)
      (require 'mew-mule3))))
 (t
  (setq mew-temacs-p t)
  (setq mew-xemacs-p nil)
  (require 'mew-temacs)
  (if (null mew-mule-p)
      (require 'mew-mule0)
    (if (string< mule-version "3")
	(progn
	  (setq mew-mule-ver 2)
	  (require 'mew-mule2))
      (setq mew-mule-ver 3)
      (require 'mew-mule3)))))

(cond 
 (mew-xemacs-p
  (or (find-face 'underline)
      (progn (make-face 'underline)
	     (set-face-underline-p 'underline t)))
  (defmacro mew-overlay-p (ovl)
    (` (and (extentp (, ovl)) (extent-live-p (, ovl)))))
  (defun mew-overlay-make (beg end)
    (let ((ovl (make-extent beg end)))
      (set-extent-property ovl 'mew t)
      ovl))
  (defun mew-overlay-move (overlay beg end &optional buffer)
    (set-extent-endpoints overlay beg end))
  (defun mew-overlay-get (overlay prop)
    (extent-property overlay prop))
  (defun mew-overlay-put (overlay prop value)
    (set-extent-property overlay prop value))
  (defun mew-overlay-delete (ovl)
    (and (extent-property ovl 'mew) (delete-extent ovl)))
  (defun mew-overlay-delete-region (beg end)
    (interactive "r")
    (mapcar (function mew-overlay-delete)
	    (extent-list (current-buffer) beg end)))
  (defun mew-front-sticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'start-closed t)
      (put-text-property beg-or-ovl end 'start-closed t)))
  (defun mew-front-nonsticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'start-open t)
      (put-text-property beg-or-ovl end 'start-open t)))
  (defun mew-rear-sticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'end-open nil)
      (put-text-property beg-or-ovl end 'end-open nil)))
  (defun mew-rear-nonsticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'end-open t)
      (put-text-property beg-or-ovl end 'end-open t)))
  (fset 'mew-buffer-substring (symbol-function 'buffer-substring))
  (defun mew-mark () (mark t))
  (defun mew-pop-to-buffer (buf)
    (setq buf (get-buffer-create buf))
    (select-window (display-buffer buf nil (selected-frame)))
    (set-buffer buf))
  (defun mew-timer (min func)
    (add-timeout (* min 60) func nil)))
 (mew-temacs-p
  (if window-system (require 'faces))
  (fset 'mew-overlay-p (symbol-function 'overlayp))
  (defun mew-overlay-make (beg end)
    (let ((ovl (make-overlay beg end)))
      (overlay-put ovl 'mew t)
      ovl))
  (fset 'mew-overlay-move (symbol-function 'move-overlay))
  (fset 'mew-overlay-get (symbol-function 'overlay-get))
  (fset 'mew-overlay-put (symbol-function 'overlay-put))
  (fset 'mew-overlay-delete (symbol-function 'delete-overlay))
  (defun mew-overlay-delete (ovl)
    (and (overlay-get ovl 'mew) (delete-overlay ovl)))
  (if (fboundp 'overlays-in)
      (defun mew-overlay-delete-region (beg end)
	(interactive "r")
	(mapcar (function mew-overlay-delete) (overlays-in beg end)))
    (defun mew-overlay-delete-region (beg end)
      (interactive "r")
      (let ((cur (if (overlays-at beg) beg (next-overlay-change beg))))
	(while (and (<= cur end) (overlays-at cur))
	  (mapcar (function mew-overlay-delete) (overlays-at cur)))
	(setq cur (next-overlay-change cur)))))
  (defun mew-front-sticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'front-sticky t)
      (put-text-property beg-or-ovl end 'front-sticky t)))
  (defun mew-front-nonsticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'front-sticky nil)
      (put-text-property beg-or-ovl end 'front-sticky nil)))
  (defun mew-rear-sticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'rear-nonsticky nil)
      (put-text-property beg-or-ovl end 'rear-nonsticky nil)))
  (defun mew-rear-nonsticky (beg-or-ovl &optional end)
    (if (mew-overlay-p beg-or-ovl)
	(mew-overlay-put beg-or-ovl 'rear-nonsticky t)
      (put-text-property beg-or-ovl end 'rear-nonsticky t)))
  (require 'easymenu)
  (if (fboundp 'buffer-substring-no-properties)
      (fset 'mew-buffer-substring
	    (symbol-function 'buffer-substring-no-properties))
    (defun mew-buffer-substring (beg end)
      "Return the text from BEG to END, without text properties, as a string."
      (let ((string (buffer-substring beg end)))
	(set-text-properties 0 (length string) nil string)
	string)))
  (defun mew-mark () (marker-position (mark-marker)))
  (fset 'mew-pop-to-buffer (symbol-function 'pop-to-buffer))
  (defun mew-timer (min func)
    (run-at-time (format "%d min" min) nil func))))

(defun mew-overlay-delete-buffer ()
  (save-restriction
    (widen)
    (mew-overlay-delete-region (point-min) (point-max))))

(if (string< "19.34" emacs-version)
    (defvar mew-use-overlay-keymap t)
  (defvar mew-use-overlay-keymap nil))

(if (fboundp 'characterp)
    (fset 'mew-characterp (symbol-function 'characterp))
  (fset 'mew-characterp (symbol-function 'integerp)))

(if (fboundp 'string-width)
    (fset 'mew-string-width (symbol-function 'string-width))
  (fset 'mew-string-width (symbol-function 'length)))

(if (fboundp 'local-variable-p)
    (if mew-xemacs-p
        (defmacro mew-local-variable-p (var)
          (` (local-variable-p (, var) (current-buffer))))
      (fset 'mew-local-variable-p (symbol-function 'local-variable-p)))
  (defun mew-local-variable-p (var)
    (assoc var (buffer-local-variables))))

(if (fboundp 'set-keymap-parent)	; for Emacs (or XEmacs)
    (defalias 'mew-set-keymap-parent 'set-keymap-parent)
  (if (fboundp 'set-keymap-parents)	; for XEmacs
      (defalias 'mew-set-keymap-parent 'set-keymap-parents)
    (defun mew-set-keymap-parent (keymap parent) ; for Emacs19
      (if (not (keymapp keymap))
	  (error "ERROR: not keymap, %s" keymap)
	(if (and parent (not (keymapp parent)))
	    (error "ERROR: not keymap, %s" parent)
	  (catch 'done
	    (while (cdr keymap)
	      (if (eq (car (cdr keymap)) 'keymap)
		  (throw 'done (setcdr keymap parent)))
	      (setq keymap (cdr keymap)))
	    (nconc keymap parent)
	    parent))))))

;; to avoid competition with mh-e.el, sigh.
(if (rassq 'mh-letter-mode auto-mode-alist)
    (setq auto-mode-alist
	  (delete (rassq 'mh-letter-mode auto-mode-alist)
		  auto-mode-alist)))

;; tricky way to tell users that subprocess is running
(or (assq 'mew-summary-buffer-process minor-mode-alist)
    (setq minor-mode-alist (cons '(mew-summary-buffer-process " Running")
                                 minor-mode-alist)))

(defvar mew-connection-type1 nil
  "Connection type for many processes. 't' means PTY and 'nil' means PIPE.
PIPE is usually recommended for speed but some OSes such as Linux 
requires PTY.")

(defvar mew-connection-type2 t
  "Connection type for processes that requires a password. ")

(cond
 ((fboundp 'make-symbolic-link)
  (defun mew-symbolic-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p (file-chase-links filename))
	(error "Can't make a symbolic link to directory")
      (make-symbolic-link filename newname OK-IF-ALREADY-EXISTS)))
  (defun mew-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p (file-chase-links filename))
	(error "Can't make a link to directory")
      (add-name-to-file filename newname OK-IF-ALREADY-EXISTS))))
 (t
  (defun mew-symbolic-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p filename)
	(error "Can't make a copy of directory")
      (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate)))
  (defun mew-link (filename newname &optional OK-IF-ALREADY-EXISTS)
    (if (file-directory-p filename)
	(error "Can't make a copy of directory")
      (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate)))))

(cond
 ((fboundp 'string-to-char-list)
  (defalias 'mew-string-to-list 'string-to-char-list))
 ((fboundp 'string-to-list)
  (defalias 'mew-string-to-list 'string-to-list)))

(if (fboundp 'char-width)
    (defalias 'mew-char-width 'char-width)
  (defmacro mew-char-width (x) 1))

(provide 'mew-env)

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

;;; mew-env.el ends here
