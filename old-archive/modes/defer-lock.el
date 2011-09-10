;;; defer-lock.el --- Defer on-the-fly fontification for fast Font Lock mode.

;; Copyright (C) 1995 Simon Marshall.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: faces files
;; Version: 1.01

;; LCD Archive Entry:
;; defer-lock|Simon Marshall|simon@gnu.ai.mit.edu|
;; Defer Font Lock mode (with fast on-the-fly fontification).|
;; 13-Nov-1995|1.01|~/modes/defer-lock.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; To make on-the-fly fontification faster by deferring it until Emacs is idle.
;;
;; See also the lazy-lock and fast-lock packages.

;; Kudos:
;;
;; To Bob Glickstein <bobg@z-code.com>.  The basis for this package is his
;; Defer package, providing "a generalized mechanism for deferring function
;; calls for a number of seconds".  Maybe Bob will finish it up one day.

;; Installation:
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put in your ~/.emacs:
;;
;; (autoload 'turn-on-defer-lock "defer-lock"
;;   "Unconditionally turn on Defer Lock mode.")
;;
;; (add-hook 'font-lock-mode-hook 'turn-on-defer-lock)
;;
;; Start up a new Emacs and use font-lock as usual (except that you can use the
;; so-called "gaudier" fontification regexps without frustration).

;; Feedback:
;;
;; Feedback is welcome.
;; To submit a bug report (or make comments) please use the mechanism provided:
;;
;; M-x defer-lock-submit-bug-report RET
;;
;; Yes, defer-lock.el should probably be part of lazy-lock.el.  Then again,
;; maybe defer-lock.el will mushroom in size as lazy-lock.el did.

;; History:
;;
;; 1.00--1.01:
;; - Use `make-local-hook' and associated features for Emacs 19.30.

(require 'font-lock)

(defun defer-lock-submit-bug-report ()
  "Submit via mail a bug report on defer-lock.el."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "simon@gnu.ai.mit.edu" "defer-lock 1.01"
     '(defer-lock-defer-time defer-lock-defer-post-command-hooks) nil nil
     (concat "Hi Si.,

I want to report a bug.  I've read the `Bugs' section of `Info' on Emacs, so I
know how to make a clear and unambiguous report.  To reproduce the bug:

Start a fresh Emacs via `" invocation-name " -no-init-file -no-site-file'.
In the `*scratch*' buffer, evaluate:"))))

;; Let's define `emacs-minor-version' if no-one else has.
(if (not (boundp 'emacs-minor-version))
    (eval-and-compile
      (defconst emacs-minor-version
	(save-match-data
	  (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	  (string-to-int
	   (substring emacs-version (match-beginning 1) (match-end 1)))))))

;; User Variables:

(defvar defer-lock-defer-time (if (featurep 'lisp-float-type) 0.25 1)
  "*Time in seconds to delay before beginning on-the-fly fontification.
Normal fontification occurs if there is no input within this time.")

(defvar defer-lock-defer-post-command-hooks
  '(show-paren-command-hook lazy-lock-post-command-fontify-windows)
  "*List of hook functions that should happen before deferral.
Defer Lock's own `defer-lock-post-command-function' will not be added to
`post-command-hook' before any of these hook functions.  Therefore the hook
functions in this list are run before `defer-lock-post-command-function'.")

;; Variables:

(defvar defer-lock-mode nil)		; not for modeline
(defvar defer-lock-begin nil)		; marker for after change deferral
(defvar defer-lock-end nil)		; marker for after change deferral
(defvar defer-lock-buffers nil)		; buffers for after change deferral

(put 'defer-lock-begin 'permanent-local t)
(put 'defer-lock-end 'permanent-local t)

;; User Functions:

;;;###autoload
(defun defer-lock-mode (&optional arg)
  "Toggle Defer Lock mode.
With arg, turn Defer Lock mode on if and only if arg is positive.

When Defer Lock mode is enabled, on-the-fly fontification is deferred until
Emacs has been idle for `defer-lock-defer-time' seconds.

Deferred fontification will not occur before `post-command-hook' has executed
all of those functions in `defer-lock-defer-post-command-hooks' (if present)."
  (interactive "P")
  (set (make-local-variable 'defer-lock-mode)
       (if arg (> (prefix-numeric-value arg) 0) (not defer-lock-mode)))
  (if (and defer-lock-mode (not font-lock-mode))
      ;; Turned on `defer-lock-mode' rather than using `font-lock-mode-hook'.
      (progn
	(add-hook 'font-lock-mode-hook 'turn-on-defer-lock)
	(font-lock-mode 1))
    ;; Let's get down to business.
    (make-local-variable 'defer-lock-begin)
    (if (or (not (markerp defer-lock-begin))
	    (and (boundp 'lazy-lock-mode) lazy-lock-mode))
	(setq defer-lock-begin (make-marker)))
    (make-local-variable 'defer-lock-end)
    (if (or (not (markerp defer-lock-end))
	    (and (boundp 'lazy-lock-mode) lazy-lock-mode))
	(setq defer-lock-end (make-marker)))
    (if (< emacs-minor-version 30)
	(make-local-variable 'post-command-hook)
      (make-local-hook 'post-command-idle-hook))
    (defer-lock-hook (if (< emacs-minor-version 30)
			 'post-command-hook
		       'post-command-idle-hook)
      'defer-lock-post-command-function
      defer-lock-mode defer-lock-defer-post-command-hooks)
    (defer-lock-hook 'after-change-functions 'defer-lock-after-change-function
      defer-lock-mode)
    (defer-lock-hook 'after-change-functions 'font-lock-after-change-function
      (not defer-lock-mode))))

;;;###autoload
(defun turn-on-defer-lock ()
  "Unconditionally turn on Defer Lock mode."
  (defer-lock-mode 1))

;; Functions:

(defun defer-lock-hook (hook function add &optional defer)
  "Add to, or remove from, the value of HOOK the function FUNCTION.
If ADD and DEFER non-nil, don't add before any of the functions in DEFER."
  (cond ((not add)
	 (if (< emacs-minor-version 30)
	     (remove-hook hook function)
	   (remove-hook hook function t)))
	((not defer)
	 (if (< emacs-minor-version 30)
	     (add-hook hook function nil)
	   (add-hook hook function nil t)))
	(t
	 (let* ((original (symbol-value hook)) (copy original))
	   (mapcar (function (lambda (defer-function)
			       (let ((exists (memq defer-function copy)))
				 (if exists (setq copy exists)))))
		   defer)
	   (cond ((not (equal original copy))
		  (setcdr copy (cons function (cdr copy))))
		 ((< emacs-minor-version 30)
		  (add-hook hook function))
		 (t
		  (add-hook hook function nil t)))))))

;; We should use an overlay with an `insert-behind-hooks' property to extend
;; the deferral when an insertion occurs immediately after it, but it isn't
;; called right in Emacs 19.29.  (Better, an overlay `rear-sticky' property.)
;; Instead we use this kludge of making the deferral region extend one
;; character past the end of the insertion, with the corresponding hackery when
;; inserting at the buffer end, and knowing where the buffer end really is.

(defun defer-lock-after-change-function (beg end old-len)
  ;; Defer region, but fontify any current deferred region if we can't combine.
  ;; We use markers, so that most marker changes are automagically done for us.
  (save-restriction
    (widen)
    (cond ((not (marker-position defer-lock-begin))
	   ;; There's no deferral currently, so just make a new one.
	   (set-marker defer-lock-begin beg)
	   (set-marker defer-lock-end (1+ end))
	   (setq defer-lock-buffers (cons (current-buffer)
					  defer-lock-buffers)))
	  ((and (= end (point-max)) (= (marker-position defer-lock-end) beg))
	   ;; Good grief.  We do this to extend when inserting at point-max.
	   (set-marker defer-lock-end (point-max)))
	  ((or (> (marker-position defer-lock-begin) beg)
	       (< (marker-position defer-lock-end) end))
	   ;; It can't be combined, fontify it.  New deferral is the region.
	   (font-lock-after-change-function
	    (marker-position defer-lock-begin)
	    (1- (marker-position defer-lock-end)) 0)
	   (set-marker defer-lock-begin beg)
	   (set-marker defer-lock-end (1+ end))))))

(defun defer-lock-post-command-function ()
  ;; Fontify deferral iff we wait `defer-lock-defer-time' without interruption.
  (if (and defer-lock-buffers (not executing-kbd-macro)
	   (not (input-pending-p)) (sit-for defer-lock-defer-time))
      (save-excursion
	(while defer-lock-buffers
	  (if (and (condition-case nil
		       (set-buffer (car defer-lock-buffers))
		     (error nil))
		   (marker-position defer-lock-begin))
	      (save-restriction
		(widen)
		(font-lock-after-change-function
		 (marker-position defer-lock-begin)
		 (1- (marker-position defer-lock-end)) 0)
		(set-marker defer-lock-begin nil)))
	  (setq defer-lock-buffers (cdr defer-lock-buffers))))))

;; Provide ourselves:

(provide 'defer-lock)

;;; defer-lock.el ends here
