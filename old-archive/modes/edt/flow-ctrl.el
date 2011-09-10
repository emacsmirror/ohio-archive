;;; File:  flow-ctrl.el, v 1.3
;;;
;;;               -------   -------------   ---------------------
;;;               F l o w   C o n t r o l   A d j u s t m e n t s
;;;               -------   -------------   ---------------------
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1991 Kevin Gallagher
;;;
;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  Send bug reports and suggestions for improvement to Kevin Gallagher
;;;  (kgallagh@digi.lonestar.org).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;;;; Terminals that use XON/XOFF flow control can cause problems with
;;;; GNU Emacs users.  This file contains elisp code that makes it
;;;; easy for a user to deal with this problem, when using such a
;;;; terminal. 
;;;; 
;;;; To make this facility available for use to all users, place this
;;;; file, flow-ctrl.el, into your site's public emacs/lisp
;;;; directory and add the following command to your site's default.el
;;;; file: 
;;;; 
;;;;           (require 'flow-ctrl)
;;;;      
;;;; To invoke these adjustments, a user need only define the variable
;;;; terminal-uses-flow-control-chars in his/her own .emacs file.  The
;;;; variable must contain a list of one or more terminal types in use
;;;; by that user which require flow control adjustments.  Here's an
;;;; example: 
;;;; 
;;;;           (setq terminal-uses-flow-control-chars 
;;;;                 '("vt200" "vt300" "vt101" "vt131"))
;;;; 
;;;; A message will appear, if the adjustments are made, during
;;;; initialization for at least 6 seconds informing the user that the
;;;; adjustments have been invoked.  If you wish these adjustments to
;;;; be made quietly, then add the following line to your .emacs file:
;;;; 
;;;;           (setq do-flow-control-adjustments-quietly t)
;;;; 

(defun evade-flow-control ()
  "Replace C-s with C-\ and C-q with C-^ and tell emacs to pass C-s
and C-q characters to OS."
  (interactive)
  ;; Tell emacs to pass C-s and C-q to OS.
  (set-input-mode nil t)
  ;; Initialize translate table, saving previous mappings, if any.
  (let ((the-table (make-string 128 0)))
    (let ((i 0)
	  (j (length keyboard-translate-table)))
      (while (< i j)
	(aset the-table i (elt keyboard-translate-table i))
	(setq i (1+ i)))
      (while (< i 128)
	(aset the-table i i)
	(setq i (1+ i))))
    (setq keyboard-translate-table the-table))
  ;; Swap C-s and C-\
  (aset keyboard-translate-table ?\034 ?\^s)
  (aset keyboard-translate-table ?\^s ?\034)
  ;; Swap C-q and C-^
  (aset keyboard-translate-table ?\036 ?\^q)
  (aset keyboard-translate-table ?\^q ?\036)
  (if (not (boundp 'do-flow-control-adjustments-quietly))
      (progn
	(message (concat 
		   "XON/XOFF adjustment for " 
		   (getenv "TERM") 
		   ":  use C-\\ for C-s  and  use C-^ for C-q."))
	(sleep-for 6))) ; Give user a chance to see message.
  )
(if (boundp 'terminal-uses-flow-control-chars)
    (let ((term (getenv "TERM"))
          hyphend)
      ;; Strip off hyphen and what follows
      (while (setq hyphend (string-match "[-_][^-_]+$" term))
        (setq term (substring term 0 hyphend)))
      (let ((len (length terminal-uses-flow-control-chars))
            (idx 0)
            (temp-term nil))
        (while (and (< idx len)
                    (not temp-term))
          (if (string-equal term 
                            (nth idx terminal-uses-flow-control-chars))
              (progn
                (evade-flow-control)
                (setq temp-term term))
              (setq idx (1+ idx)))))))

(provide 'flow-ctrl)
