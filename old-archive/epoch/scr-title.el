; From: hws@ICSI.Berkeley.EDU (Heinz Schmidt)
; Subject: *scratch* the title
; Date: Wed, 5 Dec 90 20:58:47 PST
; Status: RO
; 
; 
; I thought I forward this also to the mailing list (hope you don't mind
; Lawrence), there were some questions like the one below on the list for 
; some time now ...
; 
; ldh@eagle.svl.cdc.com (Lawrence Hare) writes:
; 
;         Subject: Mac rootless and Epoch.
;         Date: Wed, 28 Nov 90 13:49:38 PST
; 
; [lines deleted]
; 	working well although it will occasionally just stop, like a
; 	hang, but then start up when CR is wholloped. Do you know how
; 	I can default the main screen title bar to the file name instead
; 	of *scratch*.
; 
;         Thanks again.
; 
; Lawrence, sorry for the late response, 
; 
; here is some code that you may load from your .emacs file or simply 
; include in there. Included a command to change the title explicitly.
; See the doc for more and enjoy! If you have problems or need more info
; let me know...
; 
; -- Heinz
; 
; PS: I (require 'mini-cl) in my .emacs, didn't check below whether the code
; uses Common Lisp functions not normally available in Emacs-Lisp.
; 
;;; -*- Mode: Emacs-Lisp; Syntax: Common-lisp; Base: 10; -*-
;;; File: scr-title.el
;;; Author: Heinz Schmidt (hws@icsi.berkeley.edu)
;;; Created: Thu Nov 29 19:40:34 1990
;;; Copyright (C) 1990, International Computer Science Institute
;;;*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;* FUNCTION: Explicit and startup setting of screen and icon titles,
;;;*           Native Epoch creates screens with current buffer title, but
;;;*           titles are not changed when other buffer is selected. 
;;;*
;;;* RELATED PACKAGES: builds on epoch.el hook *create-screen-hook*
;;;*
;;;* HISTORY: 
;;;*  Nov 29 19:40 1990 (hws): buffer-title-screen split off my .epoch 
;;;*   and added the hook functionality.
;;;*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun buffer-title-screen (&optional arg)
  "Redefines the title of the screen to be the name of the current buffer.
The icon name is changed appropriately. 
With \\[universal-argument] 0 prompts for the title,
     \\[universal-argument] positive chooses a default title independent
of buffer names."
  (interactive "P")
  (let ((title 
	 (cond ((null arg)
		(concat 
		 (format "%s" (buffer-name (current-buffer)))
		 (if include-system-name (format " @ %s" (system-name)) "")))
	       ((zerop arg) (read-input "Screen title: "))
	       ((= arg 1)
		(concat 
		 (format "%s" (buffer-name (current-buffer)))
		 (if include-system-name (format " @ %s" (system-name)) "")))
	       (t (default-screen-title)))))
    (title title)
    (icon-name title)))

(defvar screen-count 0)
(defun default-screen-title ()
  (concat 
   (format "Epoch %d" (setq screen-count (1+ screen-count)))
   (if include-system-name		; respect user profile settings
       (format " @ %s" (system-name))
     "")))

(defun title-create-screen-hook (alist)
  "A hook to push into *create-screen-alist-hook*. Defines the title for screens
and their icons. Set include-system-name to NIL if the title gets too long for
your window manager."
  (let ((tit (assq 'title alist))
	(icon (assq 'icon-name alist))
	(newtitle (default-screen-title)))
    (rplacd tit newtitle)
    (rplacd icon newtitle))
  alist
  )

(defvar screen-title-is-buffer-name nil
  "*When T, the screen title is made the buffer name whenever the screen is 
selected.")		      ; can we do better? no hook when mode-line changes.

(defun auto-buffer-title-screen-hook ()
  (when screen-title-is-buffer-name (buffer-title-screen nil)))

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Activate screen title commands

;close to title refresh \C-za, on a key free in native Epoch.
(global-set-key "\C-zb" 'buffer-title-screen) 

(push 'auto-buffer-title-screen-hook *select-screen-hook*)
(push 'title-create-screen-hook *create-screen-alist-hook*)

;;; The first screen is already created when this file is processed. Set
;;; title explicitly.

(buffer-title-screen 2)





