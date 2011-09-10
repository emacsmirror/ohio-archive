;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;;; Function aliases later to be redefined for XEmacs usage.

(eval-and-compile
  (defvar gnus-xemacs (string-match "XEmacs" emacs-version)
    "Non-nil if running under XEmacs."))

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-down-mouse-3 [down-mouse-3])
(defvar gnus-down-mouse-2 [down-mouse-2])
(defvar gnus-widget-button-keymap nil)
(defvar gnus-mode-line-modified
  (if (or gnus-xemacs
	  (< emacs-major-version 20))
      '("--**-" . "-----")
    '("**" "--")))

(eval-and-compile
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'appt-select-lowest-window "appt"))

;;; Mule functions.

(defun gnus-mule-max-width-function (el max-width)
  `(let* ((val (eval (, el)))
	  (valstr (if (numberp val)
		      (int-to-string val) val)))
     (if (> (length valstr) ,max-width)
	 (truncate-string-to-width valstr ,max-width)
       valstr)))

(eval-and-compile
  (if gnus-xemacs
      (gnus-xmas-define)
    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions.")))

(eval-and-compile
  (cond
   ((not window-system)
    (defun gnus-dummy-func (&rest args))
    (let ((funcs '(mouse-set-point set-face-foreground
				   set-face-background x-popup-menu)))
      (while funcs
	(unless (fboundp (car funcs))
	  (defalias (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs)))))))

(eval-and-compile
  (let ((case-fold-search t))
    (cond
     ((string-match "windows-nt\\|os/2\\|emx\\|cygwin32"
		    (symbol-name system-type))
      (setq nnheader-file-name-translation-alist
	    (append nnheader-file-name-translation-alist
		    (mapcar (lambda (c) (cons c ?_))
			    '(?: ?* ?\" ?< ?> ??))
		    '((?+ . ?-))))))))

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)

(defun gnus-ems-redefine ()
  (cond
   (gnus-xemacs
    (gnus-xmas-redefine))

   ((featurep 'mule)
    ;; Mule and new Emacs definitions

    ;; [Note] Now there are three kinds of mule implementations,
    ;; original MULE, XEmacs/mule and Emacs 20+ including
    ;; MULE features.  Unfortunately these API are different.  In
    ;; particular, Emacs (including original MULE) and XEmacs are
    ;; quite different.  Howvere, this version of Gnus doesn't support
    ;; anything other than XEmacs 20+ and Emacs 20.3+.

    ;; Predicates to check are following:
    ;; (boundp 'MULE) is t only if MULE (original; anything older than
    ;;                     Mule 2.3) is running.
    ;; (featurep 'mule) is t when every mule variants are running.

    ;; It is possible to detect XEmacs/mule by (featurep 'mule) and
    ;; checking `emacs-version'.  In this case, the implementation for
    ;; XEmacs/mule may be shareable between XEmacs and XEmacs/mule.

    (defvar gnus-summary-display-table nil
      "Display table used in summary mode buffers.")
    (defalias 'gnus-max-width-function 'gnus-mule-max-width-function)
    (defalias 'gnus-summary-set-display-table (lambda ()))

    (when (boundp 'gnus-check-before-posting)
      (setq gnus-check-before-posting
	    (delq 'long-lines
		  (delq 'control-chars gnus-check-before-posting))))

    (defun gnus-summary-line-format-spec ()
      (insert gnus-tmp-unread gnus-tmp-replied
	      gnus-tmp-score-char gnus-tmp-indentation)
      (put-text-property
       (point)
       (progn
	 (insert
	  gnus-tmp-opening-bracket
	  (format "%4d: %-20s"
		  gnus-tmp-lines
		  (if (> (length gnus-tmp-name) 20)
		      (truncate-string-to-width gnus-tmp-name 20)
		    gnus-tmp-name))
	  gnus-tmp-closing-bracket)
	 (point))
       gnus-mouse-face-prop gnus-mouse-face)
      (insert " " gnus-tmp-subject-or-nil "\n")))))

(defun gnus-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defun gnus-add-minor-mode (mode name map)
  (if (fboundp 'add-minor-mode)
      (add-minor-mode mode name map)
    (set (make-local-variable mode) t)
    (unless (assq mode minor-mode-alist)
      (push `(,mode ,name) minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode map)
	    minor-mode-map-alist))))

(defun gnus-x-splash ()
  "Show a splash screen using a pixmap in the current buffer."
  (let ((dir (nnheader-find-etc-directory "gnus"))
	pixmap file height beg i)
    (save-excursion
      (switch-to-buffer (gnus-get-buffer-create gnus-group-buffer))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(when (and dir
		   (file-exists-p (setq file (concat dir "x-splash"))))
	  (with-temp-buffer
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (ignore-errors
	      (setq pixmap (read (current-buffer))))))
	(when pixmap
	  (unless (facep 'gnus-splash)
	    (make-face 'gnus-splash))
	  (setq height (/ (car pixmap) (frame-char-height))
		width (/ (cadr pixmap) (frame-char-width)))
	  (set-face-foreground 'gnus-splash "Brown")
	  (set-face-stipple 'gnus-splash pixmap)
	  (insert-char ?\n (* (/ (window-height) 2 height) height))
	  (setq i height)
	  (while (> i 0)
	    (insert-char ?  (* (/ (window-width) 2 width) width))
	    (setq beg (point))
	    (insert-char ?  width)
	    (set-text-properties beg (point) '(face gnus-splash))
	    (insert "\n")
	    (decf i))
	  (goto-char (point-min))
	  (sit-for 0))))))

(defun gnus-article-display-xface (beg end)
  "Display an XFace header from between BEG and END in the current article.
This requires support for XPM or XBM images in your Emacs and the
external programs `uncompface', `icontopbm' and either `ppmtoxpm' (for
XPM support) or `ppmtoxbm' (for XBM support).  On a GNU/Linux system
these might be in packages with names like `compface' or `faces-xface'
and `netpbm' or `libgr-progs', for instance.

This function is for Emacs 21+.  See `gnus-xmas-article-display-xface'
for XEmacs."
  (save-excursion
    (let ((cur (current-buffer))
	  image type)
      (when (and (fboundp 'image-type-available-p)
		 (cond ((image-type-available-p 'xpm) (setq type 'xpm))
		       ((image-type-available-p 'xbm) (setq type 'xbm))))
	(with-temp-buffer
	  (insert-buffer-substring cur beg end)
	  (call-process-region (point-min) (point-max) "uncompface"
			       'delete '(t nil))
	  (goto-char (point-min))
	  (insert "/* Width=48, Height=48 */\n")
	  (and (eq 0 (call-process-region (point-min) (point-max) "icontopbm"
					  'delete '(t nil)))
	       (eq 0 (call-process-region (point-min) (point-max)
					  (if (eq type 'xpm)
					      "ppmtoxpm"
					    "pbmtoxbm")
					  'delete '(t nil)))
	       (setq image (create-image (buffer-string) type t))))
	(when image
	  (goto-char (point-min))
	  (re-search-forward "^From:" nil 'move)
	    (insert-image image " "))))))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: '(redefine callargs)
;; End:

;;; gnus-ems.el ends here
