;;; mew-highlight.el --- Highlight for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 18, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-highlight-version "mew-highlight.el version 0.04")

(require 'mew)

;;
;; functions
;;

(defun mew-summary-highlight-setup ()
  "A function to setup mouse line for Summary and Virtual mode."
  (if (and mew-xemacs-p mew-use-highlight-mouse-line)
      (setq mode-motion-hook mew-highlight-mouse-line-function)))

(defalias 'mew-virtual-highlight-setup 'mew-summary-highlight-setup)

(defun mew-highlight-cursor-line ()
  "A function to highlight the cursor line in Summary and Virtual mode."
  (if mew-use-highlight-cursor-line
      (mew-elet
       (if (mew-overlay-p mew-overlay-cursor-line)
	   (mew-overlay-move mew-overlay-cursor-line
			     (save-excursion (beginning-of-line) (point))
			     (save-excursion (end-of-line) (point)))
	 (setq mew-overlay-cursor-line
	       (mew-overlay-make
		(save-excursion (beginning-of-line) (point))
		(save-excursion (end-of-line) (point))))
	 (mew-overlay-put mew-overlay-cursor-line 'face 
			  mew-highlight-cursor-line-face))))
  (if mew-use-cursor-mark
      (progn
	(if (mew-local-variable-p 'overlay-arrow-position)
	    ()
	  (make-local-variable 'overlay-arrow-position)
	  (setq overlay-arrow-position (make-marker)))
	(if (mew-local-variable-p 'overlay-arrow-string)
	    ()
	  (make-local-variable 'overlay-arrow-string)
	  (setq overlay-arrow-string mew-cursor-mark))
	(set-marker overlay-arrow-position
		    (save-excursion (beginning-of-line) (point))))))

(defun mew-highlight-url ()
  "A function to highlight URL in Message mode."
  (if mew-use-highlight-url
      (save-excursion
	(mew-elet
	 (let ((url-regex mew-use-highlight-url-regex)
	       bound overlay)
	   (goto-char (point-min))
	   (setq bound
		 (if mew-highlight-url-max-size
		     (save-excursion
		       (goto-char (+ mew-highlight-url-max-size (point)))
		       (end-of-line)
		       (point))
		   nil))
	   (while (re-search-forward url-regex bound t)
	     (setq overlay (mew-overlay-make (match-beginning 0)
					     (match-end 0)))
	     (mew-overlay-put overlay 'face mew-highlight-url-face)
	     (mew-overlay-put overlay
			      'mouse-face mew-highlight-url-mouse-face)))))))


(defmacro mew-highlight-this-folder-p ()
  '(and (or window-system mew-xemacs-p) mew-use-highlight-mark
	(or (eq mew-highlight-mark-folder-list t)
	    (mew-folder-member (buffer-name)
			       mew-highlight-mark-folder-list))))

(defun mew-highlight-mark-region (beg end)
  (interactive "r")
  (if (mew-highlight-this-folder-p)
      (save-excursion
	(mew-elet
	 (let ((regex (concat mew-summary-message-regex "\\([^ ]\\)"))
	       face)
	   (goto-char beg)
	   (while (re-search-forward regex end t)
	     (setq face (cdr (assoc (string-to-char (mew-match 2))
				    mew-highlight-mark-keywords)))
	     (if face
		 (put-text-property
		  (save-excursion (beginning-of-line) (point))
		  (save-excursion (end-of-line) (point))
		  'face face))))))))

(defun mew-highlight-mark-line (mark)
  (if (mew-highlight-this-folder-p)
      (mew-elet
       (let ((face (cdr (assoc mark mew-highlight-mark-keywords))))
	 (if face
	     (put-text-property
	      (save-excursion (beginning-of-line) (point))
	      (save-excursion (end-of-line) (point))
	      'face face))))))

(defun mew-highlight-unmark-line ()
  (if (mew-highlight-this-folder-p)
      (remove-text-properties 
       (save-excursion (beginning-of-line) (point))
       (save-excursion (end-of-line) (point))
       '(face nil))))

(defun mew-unhighlight-region (BEG END)
  (mew-overlay-delete-region BEG END))

(defun mew-unhighlight-header ()
  (save-restriction
    (widen)
    (mew-unhighlight-region (point-min) (mew-header-end))))

(defun mew-unhighlight-body ()
  (save-restriction
    (widen)
    (mew-unhighlight-region
     (+ (mew-header-end) (length mew-header-separator) 1)
     (or (mew-attach-begin) (point-max)))))

(defun mew-highlight-header-region (BEG END)
  "A function to highlight header in Message and Draft mode."
  (if (and (or window-system mew-xemacs-p) mew-use-highlight-header)
      (mew-elet
       (let ((defkey (intern-soft "mew-highlight-header-face-key"))
	     (defval (intern-soft "mew-highlight-header-face-marginal"))
	     key beg med n-spec overlay key-face val-face)
	 (save-excursion
	   (mew-unhighlight-region BEG END)
	   (save-restriction
	     (narrow-to-region BEG END)
	     (goto-char (point-min))
	     (while (not (eobp))
	       (if (not (looking-at mew-keyval))
		   (forward-line)
		 (setq key (mew-match 1))
		 (setq beg (match-beginning 0))
		 (setq med (match-end 0))
		 (forward-line)
		 (mew-header-goto-next)
		 (setq n-spec (mew-assoc-match3 key mew-field-spec 0))
		 (setq key-face (or (nth 3 n-spec) defkey))
		 (setq val-face (or (nth 4 n-spec) defval))
		 (setq overlay (mew-overlay-make beg med))
		 (mew-overlay-put overlay 'face key-face)
		 (setq overlay (mew-overlay-make med (point)))
		 (mew-overlay-put overlay 'face val-face)))))))))

(defun mew-highlight-header ()
  (save-restriction
    (widen)
    (mew-highlight-header-region (point-min) (mew-header-end))))

(defun mew-highlight-body ()
  "A function to highlight body in Message mode."
  (if (and (or window-system mew-xemacs-p) mew-use-highlight-body)
      (save-excursion
	(save-restriction
	  (mew-elet
	   (let ((keywords mew-highlight-body-keywords)
		 (line 1)
		 beg1 end1 overlay assoc key)
	     (widen)
	     (if (mew-header-p)
		 (progn
		   (mew-unhighlight-body)
		   (goto-char (mew-header-end))
		   (narrow-to-region (point)
				     (or (mew-attach-begin) (point-max)))))
	     (while (and (not (eobp)) (< line mew-highlight-body-max-line))
	       (if (looking-at mew-highlight-body-keywords-regex)
		   (progn
		     (setq beg1 (match-beginning 0))
		     (setq end1 (match-end 0))
		     (setq key (mew-match 0))
		     (if (setq assoc (mew-assoc-match2 key keywords 0))
			 (progn
			   (setq overlay (mew-overlay-make beg1 end1))
			   (mew-overlay-put overlay 'face (nth 1 assoc))))))
	       (forward-line)
	       (setq line (1+ line)))))))))

;;
;; X-Face:
;;

;; uncompface: ftp://ftp.cs.indiana.edu/pub/faces/xfaces/xfaces-<ver>.tar.Z
;; icontopbm: ftp://ftp.x.org/R5contrib/netpbm-<ver>.tar.gz

(defun mew-highlight-x-face (beg end)
  "A function to display X-Face."
  (if (and mew-use-highlight-x-face mew-use-highlight-x-face-function)
      (funcall mew-use-highlight-x-face-function beg end)))

(cond
 (mew-xemacs-p
  ;;(autoload 'highlight-headers-x-face-to-pixmap "highlight-headers")
  (if (mew-which-el "highlight-headers" load-path)
      (require 'highlight-headers)) ;; due to the timing problem.
  ;; now this is in the "mail-lib" package.
  (defvar mew-use-highlight-x-face-function
    (function (lambda (beg end)
		(interactive)
		(if (and (or window-system mew-xemacs-p)
			 (mew-which mew-prog-uncompface exec-path)
			 mew-use-highlight-x-face)
		    (save-excursion
		      (goto-char beg)
		      (mew-elet
		       (let (overlay xface)
			 (while (re-search-forward 
				 "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n" end t)
			   (setq overlay (mew-overlay-make (match-beginning 0)
							   (match-end 0)))
			   (mew-overlay-put overlay 'invisible t)
			   (setq xface (highlight-headers-x-face-to-pixmap
					(match-beginning 1)
					(match-end 1)))
			   (save-excursion
			     (goto-char beg)
			     (if (re-search-forward
				  (concat "^\\(" mew-from: "\\).*") end t)
				 (progn
				   (setq overlay (mew-overlay-make
						  (match-end 1) (match-end 1)))
				   (set-extent-begin-glyph overlay xface))))
			   )))))))))
 (t
  (defvar mew-use-highlight-x-face-function nil
    "*On Text Emacs, this function is called if mew-use-highlight-x-face
is *non-nil*. This is a temporary solution.")
  (if (and mew-use-highlight-x-face
	   (mew-which mew-prog-uncompface exec-path)
	   (mew-which-el "bitmap" load-path)
	   (mew-which-el "mew-xface-mule" load-path))
      (require 'mew-xface-mule))))

;;
;; Setup
;;

(defun mew-highlight-face-setup (flist)
  "A function to create faces according to FLIST.
FLIST is a list of face name symbol and its name convention is
'mew-highlight-<word1>-face-<word2>'. A base face is copied from
'mew-highlight-<word1>-style-<word2>' then a color is set from
'mew-highlight-<word1>-color-<word2>'."
  (if (or window-system mew-xemacs-p)
      (let (fname str style color)
	(while flist
	  (setq fname (car flist))
	  (setq flist (cdr flist))
	  (set fname fname)
	  (setq str (symbol-name fname))
	  (string-match "^\\(mew-highlight-.*-\\)face\\(-.*\\)$" str)
	  (setq style (intern-soft
		       (concat (mew-match 1 str) "style" (mew-match 2 str))))
	  (setq color (intern-soft
		       (concat (mew-match 1 str) "color" (mew-match 2 str))))
	  (copy-face (symbol-value style) fname)
	  (set-face-foreground fname (symbol-value color))))))

(defun mew-highlight-make-keywords-regex ()
  (setq mew-highlight-body-keywords-regex
	(mapconcat (function car) mew-highlight-body-keywords "\\|")))

(provide 'mew-highlight)

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

;;; mew-highlight.el ends here
