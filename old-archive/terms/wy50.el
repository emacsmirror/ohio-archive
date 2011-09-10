; Article: 265 of gnu.emacs.sources
; Path: utkcs2!emory!att!pacbell.com!decwrl!ucbvax!tut.cis.ohio-state.edu!unreplyable!garbage
; From: lrs@indetech.com (Lynn Slater x2048)
; Newsgroups: gnu.emacs.sources
; Subject: Wy50.el
; Date: 21 Apr 91 22:03:00 GMT
; Organization: Source only  Discussion and requests in gnu.emacs.help.
; 
; Here is a reposting of an old file. Another wyse 50 file was just posted; I
; do not know which of these two postings would be best to use.
; 
; 
; -*- File: ~/local/term/wy50.el
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wy50.el -- drives wy50 with softkeys
;; 
;; AFSID           : $__Header$
;; Author          : Lynn Slater x2048
;; Created On      : Wed Jan 23 16:13:58 1991
;; Last Modified By: Lynn Slater x2048
;; Last Modified On: Tue Feb 12 14:10:02 1991
;; Update Count    : 2
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;; 23-Jan-1991		Lynn Slater x2048	
;;    header
;; PURPOSE
;; TABLE OF CONTENTS
;;   function-key-not-defined -- Used to display a messasge about a function key not being defined.
;;   scroll-down-in-place -- Select Previous-complex-command
;;   scroll-up-in-place -- Select Previous-complex-command
;;   prev-complex-command -- Select Previous-complex-command
;;   rerun-prev-command -- Repeat Previous-complex-command.
;;   wy50-init-softkeys -- Initiliazes the wy50 softkeys for emacs bindings.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup wy50 function keys.
;; Copyright (C) 1987 Free Software Foundation, Inc.
;; Author/Integrator: Lynn Slater

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defun function-key-not-defined ()
  "Used to display a messasge about a function key not being defined."
  (interactive)
  (beep)
  (message "Function key not defined."))

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(defun prev-complex-command ()
  "Select Previous-complex-command"
  (interactive)
  (if (zerop (minibuffer-depth))
      (repeat-complex-command 1)
    (previous-complex-command 1)))

(defun rerun-prev-command ()
  "Repeat Previous-complex-command."
  (interactive)
  (eval (nth 0 command-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On the wy50, the softkeys need to be initialized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; user should (setq wy50-esc-bracket nil) in ~/.emacs to preserve
;;; old keybindings
(defvar wy50-esc-bracket t
  "If non-nil (the default) the former binding of esc-[ is replaced
   and esc-[ becomes the prefix character for wy50 function keys.")

(defvar wy50-setup-softkeys t
  "If non-nil (the default) special control sequences are sent to the wy50
   to define the softkeys to emacs-compatable meanings.")

(defvar wy50-raw-map (make-sparse-keymap)
  "*Keymap for ESC-[ encoded wy50 softkeys")

(defun wy50-softkey (number &optional shiftedp)
  "Returns the special unique key associated with the requested
   shifted or unshifted softkey.  For lack of any better binding,
   the values are taken from the wy50 manual, page 4-8."
  (let ((index (if shiftedp (+ 16 number) number)))
    (substring  "@ABCDEFGHIJKLMNO`abcdefghijklmno" (1- index) index)))

(defun wy50-init-softkeys ()
  "Initiliazes the wy50 softkeys for emacs bindings."
  (interactive)
  (let ((i 0)
	(char))
    (setq wy50-esc-bracket t);; must be there or why bother
    (while (< i 32)
      (setq char (aref "@ABCDEFGHIJKLMNO`abcdefghijklmno" i))
      (send-string-to-terminal (format "z%c[%c" char char))
      (setq i (1+ i)))))

(if wy50-setup-softkeys (wy50-init-softkeys))

(if wy50-esc-bracket
    (progn
      (define-key esc-map "[" wy50-raw-map) ; Install wy50-raw-map

      (define-key wy50-raw-map (wy50-softkey 1)    'set-mark-command) 
      (define-key wy50-raw-map (wy50-softkey 1 t)  'set-mark-command)
      ;; note: control softkey1 also sets the mark by fortunate chance
      (define-key wy50-raw-map (wy50-softkey 2)    'scroll-down) 
      (define-key wy50-raw-map (wy50-softkey 2 t)  'scroll-down-in-place) 
      (define-key wy50-raw-map (wy50-softkey 3)    'scroll-up) 
      (define-key wy50-raw-map (wy50-softkey 3 t)  'scroll-up-in-place) 
      (define-key wy50-raw-map (wy50-softkey 4)    'switch-to-buffer) 
      (define-key wy50-raw-map (wy50-softkey 4 t)  'list-buffers) 
      (define-key wy50-raw-map (wy50-softkey 5)    'other-window) 
      (define-key wy50-raw-map (wy50-softkey 5 t)  'other-window)
      (define-key wy50-raw-map (wy50-softkey 6)    'split-window-vertically)
      (define-key wy50-raw-map (wy50-softkey 6 t)  'split-window-horizontally)
      (define-key wy50-raw-map (wy50-softkey 7)    'delete-other-windows) 
      (define-key wy50-raw-map (wy50-softkey 7 t)  'delete-other-windows) 
      (define-key wy50-raw-map (wy50-softkey 8)    'scroll-other-window)
      (define-key wy50-raw-map (wy50-softkey 8 t)  'scroll-other-window)
      (define-key wy50-raw-map (wy50-softkey 9)    'kill-region)
      (define-key wy50-raw-map (wy50-softkey 9 t)  'copy-region-as-kill)
      (define-key wy50-raw-map (wy50-softkey 10)   'yank)
      (define-key wy50-raw-map (wy50-softkey 10 t) 'yank-pop)
      (define-key wy50-raw-map (wy50-softkey 11)   'toggle-auto-fill-mode)
      (define-key wy50-raw-map (wy50-softkey 11 t) 'toggle-overwrite-mode)
      (define-key wy50-raw-map (wy50-softkey 12)   'delete-char)
      (define-key wy50-raw-map (wy50-softkey 12 t) 'kill-line)
      (define-key wy50-raw-map (wy50-softkey 13)   'backward-char) 
      (define-key wy50-raw-map (wy50-softkey 13 t) 'beginning-of-line)
      (define-key wy50-raw-map (wy50-softkey 14)   'forward-char) 
      (define-key wy50-raw-map (wy50-softkey 14 t) 'end-of-line)  
      (define-key wy50-raw-map (wy50-softkey 15)   'previous-line) 
      (define-key wy50-raw-map (wy50-softkey 15 t) 'scroll-down) 
      (define-key wy50-raw-map (wy50-softkey 16)   'next-line) 
      (define-key wy50-raw-map (wy50-softkey 16 t) 'scroll-up)
      ))

;;; Since .emacs gets loaded before this file, a hook is supplied
;;; for you to put your own bindings in.
(defvar wy50-softkey-hooks nil
  "List of forms to evaluate after setting up the wy50 softkeys.")

(let ((hooks wy50-softkey-hooks))
  (while hooks
    (eval (car hooks))
    (setq hooks (cdr hooks))
    ))

;; to customize your softkeys, include a form like the following in
;; your .emacs file  
;; (setq wy50-softkey-hooks
;;     '((define-key wy50-raw-map (wy50-softkey 14)   'forward-char) 
;;    	(define-key wy50-raw-map (wy50-softkey 14 t) 'end-of-line)
;; 	.
;; 	.
;; 	.
;; 	))

-*- End File: ~/local/term/wy50.el


