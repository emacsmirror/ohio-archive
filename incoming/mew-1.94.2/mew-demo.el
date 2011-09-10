;;; mew-demo.el --- Startup demo for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-demo-version "mew-demo.el version 0.12")

(require 'mew-vars)

(defconst mew-hello-message 
"

Welcome to Mew world.

Mew -- Messaging in the Emacs World

%s

Copyright (C) 1994-1999 Kazu Yamamoto

Please send comments to Kazu@Mew.org.

"
)

(defconst mew-demo-string
'(
 "/\\\\ - \\\\/"

 "-\\\\ - \\\\/" "\\\\\\ - \\\\/" "|\\\\ - \\\\/" "/\\\\ - \\\\/"
 "-\\\\ - \\\\/" "\\\\\\ - \\\\/" "|\\\\ - \\\\/" "/\\\\ - \\\\/"

 "/|\\ - \\\\/"  "//\\ - \\\\/" "/-\\ - \\\\/" "/\\\\ - \\\\/"
 "/|\\ - \\\\/"  "//\\ - \\\\/"  "/-\\ - \\\\/" "/\\\\ - \\\\/"

 "/\\| - \\\\/" "/\\/ - \\\\/" "/\\- - \\\\/" "/\\\\ - \\\\/" 
 "/\\| - \\\\/" "/\\/ - \\\\/" "/\\- - \\\\/" "/\\\\ - \\\\/"

 "/\\\\ - |\\/" "/\\\\ - /\\/" "/\\\\ - -\\/" "/\\\\ - \\\\/"
 "/\\\\ - |\\/" "/\\\\ - /\\/" "/\\\\ - -\\/" "/\\\\ - \\\\/"

 "/\\\\ - \\|/" "/\\\\ - \\//" "/\\\\ - \\-/" "/\\\\ - \\\\/"
 "/\\\\ - \\|/" "/\\\\ - \\//" "/\\\\ - \\-/" "/\\\\ - \\\\/"

 "/\\\\ - \\\\-" "/\\\\ - \\\\\\" "/\\\\ - \\\\|" "/\\\\ - \\\\/"
 "/\\\\ - \\\\-" "/\\\\ - \\\\\\" "/\\\\ - \\\\|" "/\\\\ - \\\\/"
 )
)

(defun mew-hello ()
  (mew-window-configure (get-buffer-create mew-buffer-hello) '(1 0))
  (let (left-margin fill-column ext)
    (mew-erase-buffer)
    (setq left-margin 0)
    (setq fill-column (window-width))
    (insert (format mew-hello-message mew-version))
    (center-region (point-min) (point-max))
    (cond
     ((and mew-xemacs-p window-system)
      (goto-char (point-min))
      (setq ext (mew-overlay-make (point-min) (point-max)))
;;      (set-extent-face ext 'bold-italic) ;; not work for 14 dot...
      (set-extent-face ext 'bold)
      (goto-char (point-max))
      (cond
       ((and mew-demo-picture
	     (valid-image-instantiator-format-p 'png))
	(mew-flet
	 (setq mew-logo
	       (make-glyph (vector 'png ':file 
 				   (expand-file-name mew-icon-mew
					 	     mew-icon-directory)))))
	(indent-to (startup-center-spaces mew-logo))
	(set-extent-begin-glyph (mew-overlay-make (point) (point)) mew-logo)
	(goto-char (point-min))))
      (sit-for 0)) ;; to redraw
     ((and (featurep 'bitmap) mew-demo-picture)
      (or
       (condition-case nil
	   (insert-file-contents
	    (expand-file-name mew-icon-mew-mule-bitmap-image
			      mew-icon-directory))
	 (file-error nil))
       (condition-case nil
	   (progn
	     (save-excursion
	       (mew-set-buffer-tmp)
	       (insert-file-contents
		(expand-file-name mew-icon-mew-mono mew-icon-directory)))
	     (bitmap-insert-xbm-buffer mew-buffer-tmp))
	 (file-error nil)))
      (center-region (point-min) (point-max))
      (sit-for 0)) ;; to redraw
     (t
      (insert "/\\\\ - \\\\/")
      (center-line)
      (end-of-line)
      (insert (make-string (1- (- (window-width) (current-column))) ?\ ))
      (end-of-line)
      (sit-for 0) ;; to redraw
      (if (and mew-demo window-system) (mew-demo)))
     )))

(defun mew-demo (&optional string)
  (let* ((list (or string mew-demo-string))
	 (wl (window-width))
	 (ul (length (regexp-quote "/\\ - \\/")))
	 (pl (/ (- wl ul) 2))
	 (pre (make-string pl 32))
	 (suf (make-string (1- (- (- wl pl) ul)) 32)))
    (save-window-excursion
      (select-window (get-buffer-window (get-buffer mew-buffer-hello)))
      (while list
	(mew-demo-print (car list) pre suf)
	(mew-demo-loop)
	(setq list (cdr list)))
      )))

(defun mew-demo-print (string prefix suffix)
  (goto-char (point-max))
  (let ((end (point)))
    (beginning-of-line)
    (delete-region (point) end))
  (insert (concat prefix string suffix)))

(defun mew-demo-loop ()
  (sit-for 0.02))

(provide 'mew-demo)

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

;;; mew-demo.el ends here
