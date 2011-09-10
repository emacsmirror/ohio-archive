; From: kevin@traffic.den.mmc.com (Kevin Rodgers)
; Subject: GNU Elisp Archive submission: sccs-comment.el
; Date: Mon, 1 Mar 93 09:10:59 MST
; 
; 
; >Newsgroups: gnu.emacs.sources
; >Path: news.den.mmc.com!csn!magnus.acs.ohio-state.edu!zaphod.mps.ohio-state.edu!howland.reston.ans.net!europa.eng.gtefsd.com!uunet!ulowell!das.wang.com!wang!tegra!vail
; >From: vail@tegra.com (Johnathan Vail)
; >Subject: sccs-comment.el Release
; >Message-ID: <C3310x.3HF@tegra.com>
; >Organization: Tegra-Varityper, Inc. Billerica, MA
; >Date: Sat, 27 Feb 1993 00:50:56 GMT
; >Lines: 169
; >
; >This is the sccs comment string editing package written by Kevin
; >Rodgers.  It is an option added to the previosly posted sccs.el
; >package.
; >
; >
; >happy hacking, jv
; >
; > _____
; >|     | Johnathan Vail     vail@tegra.com     (508) 663-7435
; >|Tegra| jv@n1dxg.ampr.org    N1DXG@448.625-(WorldNet)
; > -----  MEMBER: League for Programming Freedom (league@prep.ai.mit.edu)
; 
;;;;	sccs-comment.el
;;;;
;;;;	Define sccs-edit-comment, which pops up a buffer for editing an
;;;;	'SCCS' delta comment, then returns the buffer's contents as a
;;;;	string.  This for use with sccs.el (written by Jonathan Vail)
;;;;	version 2.0, by providing a suitable functional value for
;;;;	sccs-delta-comments.
;;;;
;;;;    Copyright (C) 1993 Kevin Rodgers
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 1, or (at your option)
;;;;    any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program; if not, write to the Free Software
;;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
;;;;	Martin Marietta has not disclaimed any copyright interest in
;;;;	sccs-comment.el.
;;;;
;;;;	Kevin Rodgers				kevin@traffic.den.mmc.com
;;;;	Martin Marietta MS A16401		(303) 790-3971
;;;;	116 Inverness Dr. East
;;;;	Englewood CO 80112 USA
;;;;
;;;;	Installation:
;;;;	1. Put this file in a directory that is a member of load-path, and
;;;;	   byte-compile it for better performance.
;;;;	2. Put these forms in ~/.emacs:
;;;;	   (setq sccs-delta-comments (function sccs-edit-comment))
;;;;	   (autoload 'sccs-edit-comment "sccs-edit-comment"
;;;;	   	  "Pop up a buffer for editing an 'SCCS' delta comment...")
;;;;
;;;;	Usage:
;;;;	See sccs.el first: sccs-edit-comment is invoked via sccs-delget
;;;;	and sccs-deledit.
;;;;
;;;;	You may want to set the sccs-edit-comment-hooks variable; for
;;;;	example: (setq sccs-edit-comment-hooks (function text-mode))
;;;;
;;;;	LCD Archive Entry:
;;;;	sccs-comment|Kevin Rodgers|kevin@traffic.den.mmc.com|
;;;;	sccs-delta-comments customization for sccs.el 2.0.|
;;;;	1993-02-23|1.0|~/misc/sccs-comment.el.Z|

(require 'sccs)

(defvar sccs-comment-buffer-name "*Comment*"
  "The name of the buffer used by sccs-edit-comment.")

(defvar sccs-edit-comment-hooks nil
  "The hook variable used by sccs-edit-comment.")

(defun sccs-edit-comment ()
  "Pop up a buffer for editing an 'SCCS' delta comment, and return the
buffer contents as a string.  The hook variable edit-comment-hooks is
run after the buffer is created.

This function invokes a recursive edit, which is exited by sccs-exit-
edit-comment; it is intended for use as a value of sccs-delta-comments
\(which see\)."
  (save-excursion
    (save-window-excursion
      (pop-to-buffer (get-buffer-create sccs-comment-buffer-name))
      (widen)
      (erase-buffer)
      (run-hooks 'sccs-edit-comment-hooks)
      (unwind-protect			; restore exit-recursive-edit keybinding
	  (catch 'sccs-edit-comment	; catch and return comment
	    (sccs:substitute-key-definition (function exit-recursive-edit)
					    (function sccs-exit-edit-comment)
					    (current-global-map)
					    t)
	    (message "Type %s to return the contents of the '%s' buffer, or %s to abort"
		     (substitute-command-keys "\\[sccs-exit-edit-comment]")
		     sccs-comment-buffer-name
		     (substitute-command-keys "\\[abort-recursive-edit]"))
	    (recursive-edit)
	    ;; In case exit-recursive-edit was explicitly invoked:
	    (error "Recursive edit of '%s' buffer exited without returning \
comment string"
		   sccs-comment-buffer-name))
	(sccs:substitute-key-definition (function sccs-exit-edit-comment)
					(function exit-recursive-edit)
					(current-global-map)
					t)))))
	  
(defun sccs-exit-edit-comment ()
  "*Return the contents of the sccs-comment-buffer-name buffer to sccs-
edit-comment."
  (interactive)
  (let ((sccs-comment-buffer (get-buffer sccs-comment-buffer-name)))
    (if (and (or sccs-comment-buffer
		 (error "'%s' buffer does not exist" sccs-comment-buffer-name))
	     (or (> (recursion-depth) 0)
		 (error "No recursive edit is in progress")))
	(save-excursion
	  (set-buffer sccs-comment-buffer)
	  (let ((comment (buffer-string)))
	    (set-buffer-modified-p nil)
	    ;; (bury-buffer)		; not necessary, since catch is
					; protected by save-window-excursion
	    (throw 'sccs-edit-comment comment)))))) ; exits recursive-edit, too


;; Define sccs:substitute-key-definition (compatible with substitute-
;; key-definition):

(defun sccs:substitute-key-definition (olddef newdef keymap &optional recur)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.

If the optional argument RECUR is non-nil, also recursively substitute
NEWDEF for OLDEF in keymaps accessible from KEYMAP via a non-empty
prefix key.  If RECUR is t, do so for all accessible keymaps; otherwise,
it should be a predicate \(i.e. a function of one argument\) which is
applied to each accessible keymap to determine \(according to whether
the result is non-nil\) whether the substitution will be performed in it."
(if (keymapp keymap)
    (progn
      (cond ((and (vectorp keymap) (= (length keymap) 128))
	     (let ((i 0))
	       (while (< i 128)
		 (if (eq (aref keymap i) olddef)
		     (aset keymap i newdef))
		 (setq i (1+ i)))))
	    ((and (consp keymap) (eq (car keymap) 'keymap))
	     (let ((key-defs (cdr keymap)))
	       (while key-defs
		 (if (eq (cdr-safe (car-safe key-defs)) olddef)
		     (setcdr (car key-defs) newdef))
		 (setq key-defs (cdr key-defs))))))
      (if recur
	  ;; accessible-keymaps returns all keymaps recursively
	  ;; accessible from its argument keymap, so the recursive call
	  ;; to sccs:substitute-key-definition should disable any further
	  ;; recursion by explicitly passing nil as the fourth argument.
	  (let ((sub-keymap-alist (cdr (accessible-keymaps keymap))))
	    (while sub-keymap-alist
	      (if (or (eq recur t)
		      (funcall recur (cdr (car sub-keymap-alist))))
		  (sccs:substitute-key-definition olddef newdef
						  (cdr (car sub-keymap-alist))
						  nil))
	      (setq sub-keymap-alist (cdr sub-keymap-alist))))))))



