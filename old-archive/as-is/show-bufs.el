;;; show-bufs.el: Put some or all buffers in equally-sized windows
;;; Copyright (C) 1992 Wayne Mesard
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; DESCRIPTION
;;   This module contains:
;;    1) some commands which show groups of buffers satisfying various 
;;       criteria (e.g., "all buffers in C mode," or "all modified buffers").
;;    2) a function, show-some-buffers, which allows you to easily write
;;       new commands if the ones I provided in (1) don't fill your needs.
;;   Originally, I just set out to write the show-file-buffers command.
;;   But you know how these things go once you decide to do something 
;;   Right(tm).  This general-purpose buffer selector is the result.
;;
;;   show-file-buffers
;;     Display all the buffers that are visiting files.  To automatically
;;     show all files specified on the command line when Emacs starts
;;     up, add the following to your ~/.emacs file:
;;
;;       (setq window-setup-hook '(lambda () (if (> (length (buffer-list)) 3)
;;                                               (progn (require 'show-bufs)
;;                                                      (show-file-buffers)))))
;;
;;     or if you're not worried about every little byte:
;;
;;       (require 'show-bufs)
;;       (setq window-setup-hook 'show-file-buffers)
;; 
;;   show-all-buffers
;;     Display every buffer even if it's not associated with a file.
;;     NOTE THAT THIS IS INCOMPATIBLE with what this function did in 
;;     show-bufs v1.1!  Use show-file-buffers for the old behavior.
;;
;;   show-top-buffers
;;     Display the N most recently accessed buffers.
;;
;;   show-modified-buffers
;;   show-modified-file-buffers
;;   show-c-buffers
;;     Three commands which use the function show-some-buffers to select
;;     buffers for display.  Use these as a model to define new commands
;;     to fit your needs.  Basically, what you do is call
;;     show-some-buffers with a single argument: a predicate function
;;     that takes a buffer and returns nil or non-nil depending on the
;;     state of the buffer.  Emacs has several built-in functions like
;;     this (such as buffer-modified-p), or you can pass in your own
;;     lambda expression (as was done in show-c-buffers).
;;
;;   Wayne Mesard: wmesard@cs.stanford.edu

;;; HISTORY
;;    1.3 wmesard - Nov 11, 1992: Added show-top-buffers.
;;    1.2 wmesard - Aug  5, 1992: Rewrote it the way it should be: so that 
;;                                user can specify arbitrary predicates.
;;    1.1 wmesard - Aug  2, 1992: Added note on autoloading to the doc
;;    1.0 wmesard - Jul 31, 1992: Created.

;; LCD Archive Entry:
;; show-bufs|Wayne Mesard|wmesard@cs.stanford.edu|
;; Put some or all buffers in equally-sized windows|
;; 92-11-30|1.3|~/as-is/show-bufs.el.Z|

;;; 
;;; COMMANDS
;;; 

;;
;; These commands use show-some-buffers with built-in Emacs predicates.
;;

(defun show-file-buffers ()
  "Display all file buffers.
To automatically show all files specified on the command line when Emacs 
starts up, add the following to your ~/.emacs file:

     (setq window-setup-hook '(lambda () (if (> (length (buffer-list)) 3)
					    (progn (require 'show-bufs)
						   (show-file-buffers)))))
"
  (interactive)
  (show-some-buffers (function buffer-file-name)))

(defun show-all-buffers ()
  "Displays all buffers."
  (interactive)
  (show-some-buffers (function identity)))

(defun show-modified-buffers ()
  "Display all the modified buffers."
  (interactive)
  (show-some-buffers (function buffer-modified-p)))

;; 
;; These use show-some-buffers with predicates that we define.
;;

(defun show-c-buffers ()
  "Display all the buffers that are in C mode."
  (interactive)
  (show-some-buffers 
   ;; T if X is in c-mode. NIL otherwise.
   (function (lambda (x) (set-buffer x) (eq 'c-mode major-mode)))
   ))


(defun show-top-buffers (n)
  "Display the N most recently visited buffers.  
N defaults to the number of windows currently visible (which has the
effect of making them all the same size and making sure that each buffer
is only displayed once."
  (interactive "P")
  (setq n (if n 
	      (prefix-numeric-value n)
	    (wsm-window-count)))
  (let ((show-top-buffer-list (wsm-sublist (buffer-list) 0 (1+ n))))
    (show-some-buffers
     ;; T if X is in the list we built. NIL otherwise.
     (function (lambda (x) (memq x show-top-buffer-list))))
    ))

;; A couple of helper functions for show-top-buffers.

(defun wsm-window-count ()
  (save-excursion
    (let ((start (selected-window))
	  (i 1))
      (while (progn (other-window 1) (not (eq (selected-window) start)))
	(setq i (1+ i)))
      i)))
	  
(defun wsm-sublist (lis from to)
;; Returns a sublist of LIS, starting at index FROM and ending before TO.
;; Todo: Handle negative args like substring.
;; Warning: args out of range just terminates, it's not an error condtion.
  (let ((i 0)
	(sublis nil))
    (while (and lis (< i to))
      (if (>= i from)
	  (setq sublis (cons (car lis) sublis)))
      (setq lis (cdr lis)
	    i (1+ i)))
    (reverse sublis)))
      

;; Uses show-some-buffers with a predicate that returns non-nil for
;; buffers which have been modified and have files associated with them.
;; Uses the return value of show-some-buffers to give the user feedback
;; when no buffers need to be displayed.

(defun show-modified-file-buffers ()
  "Display all the modified file buffers."
  (interactive)
  (if (null (show-some-buffers
	     (function (lambda (x) (and (buffer-modified-p x) 
					(buffer-file-name x))))
	     ))
      (message "(No files need saving)")
    ))

;;; 
;;; PUBLIC FUNCTIONS
;;; 

(defun show-some-buffers (pred)
  "Display all the buffers (except minibuffer) for which PRED returns non-nil.
PRED is a function which takes a buffer as an argument.
Returns nil if there were no buffers to display, non-nil otherwise."
  (show-these-buffers
   (let ((minibuf (window-buffer (minibuffer-window))))
     ;; Get a list of all the buffers and delete the minibuffer and those
     ;; that don't match PRED
     (delq nil
	   (mapcar (function (lambda (x) (if (and (not (eq minibuf x))
						  (funcall pred x))
					     x nil)))
		   (buffer-list))
	   ))
   ))

(defun show-these-buffers (buflst)
  "Display all the buffers in BUFLST, a list of buffers or buffer names."
  (if buflst
      (let ((lines (/ (1- (screen-height)) (length buflst))))
	(if (< lines window-min-height)
	    (error 
	     "Too many buffers.  Each window would be less than %d lines tall."
	     window-min-height))
	(delete-other-windows)
	;; Seed the first window
	(switch-to-buffer (car buflst))
	;; Then split it and put the next buf in the new win.  Repeat for each
	(mapcar (function (lambda (x)
			    (split-window-vertically lines)
			    (switch-to-buffer (get-buffer-create x))
			    (other-window 1)
			    ))
		(cdr buflst))
	(other-window 1)
	t) ; return non-nil if we did anything
    ))


;;; 
;;; MODULE NAME
;;; 

(provide 'show-bufs)

