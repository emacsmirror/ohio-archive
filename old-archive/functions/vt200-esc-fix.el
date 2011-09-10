;;; File:  vt200-esc-fix.el, v 3.1a (emacs 18[.58] version)
;;;
;;;               -----------   -----   -----
;;;               E s c a p e   K e y   F i x
;;;               -----------   -----   -----
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;; Thanks to Kevin Gallagher for TERM-in-list and Joe Wells for
;;; encouragement to code everthing right.  


;; LCD Archive Entry:
;; vt200-esc-fix|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Put ESC key in convenient reach on VT220.|
;; 93-02-12|3.1|~/functions/vt200-esc-fix.el.Z|

;;Archived at archive.cis.ohio-state.edu

;;;
;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  Send bug reports and suggestions for improvement to Jeff Morgenthaler
;;;  (jpmorgen@wisp4.physics.wisc.edu).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;;;; VT200 type terminals have no escape key. This causes great
;;;; trouble for emacs and another well known UNIX editor.  The most
;;;; popular solution to this problem is to run the terminal in vt100
;;;; mode (if possible).  This unfortunately makes the function keys
;;;; inaccessible.  Another solution is to get the terminal to remap
;;;; some keys (vt300s and xterms do this).  The usual remapping is to
;;;; put the ~/` key down on the </> key and put the < and > above the
;;;; , and . where they belong.  Also, the F11 can be made to send a
;;;; real ESC, not an ESC[23~.

;;;; Unfortunately, those of us who are stuck with old vt200 terminals
;;;; can't do this.  Emacs is smart enough to allow a partial
;;;; remapping of keys, but there are still problems.  This code
;;;; solves or works around as many of these problems as I know about.
;;;; (tell be about any more that you find).  By default it maps ` to
;;;; ESC and C-c ` to `.  Unfortunately, C-x ` (next-error) is no
;;;; longer accessible, so it is mapped to C-x ~.  

;;;; To make this facility available for all users, place this file,
;;;; vt200-esc-fix.el, into your site's public emacs/lisp directory
;;;; and add the following command to your site's default.el file:

;;;; 		(require 'vt200-esc-fix)


;;;; This code can either be called interactively with "M-x escape-key-fix"
;;;; or started automatically with the code:

;;;;  (setq terminal-needs-escape-key
;;;;        '("vt200" "vt300"))

;;;; in the user's .emacs file.  To turn it off, just use "M-x
;;;; escape-key-fix" again.  

;;;; The ESC key can be remapped to another key on the keyboard that
;;;; sends a single character (not the function keys, sorry) with the
;;;; code:

;;;; (setq escape-key-replacement ?<)
;;;; (setq escape-key-fix-displaced-function 'scroll-left)
;;;; (setq escape-key-fix-ctrl-seq "\C-xl")
;;;; (setq escape-key-fix-ctrl-seq-message "Use C-x l for C-x <.")

;;;; You can bind type-escape-key-replacement to any key.  For
;;;; instance, if you wanted the F11 key to send ` (or your customized
;;;; escape-key-replacement), you would use the code:

;;;; (setq term-setup-hook
;;;;       (function
;;;;        (lambda ()
;;;; 	 (and (fboundp 'enable-arrow-keys)
;;;; 	      (progn
;;;; 		(enable-arrow-keys)
;;;; 		(define-key CSI-map "23~" 'type-escape-key-replacement)   ; F11
;;;; 		)
;;;; 	      )
;;;; 	 )))


;; Define default ESC replacement and associated work arounds.  Tell me if 
;; there are any more conflicts.

(defvar escape-key-replacement ?`
  "*ascii code of escape key replacement.")

(defvar escape-key-replacement-replacement "\C-c\e"
  "* Key sequence allowing access to escape-key-replacement.")

(defvar escape-key-fix-ctrl-seq "\C-x~"
  "* Control character sequence to replace the sequence displaced by
remapping the escape-key-replacement key to ESC.  For example C-x ` is
displaced by remapping ` to ESC.  It is remapped to C-x ~ by default.")


(defvar escape-key-fix-displaced-function 'next-error
  "* Function assigned to key sequence displaced by remapping 
escape-key-replacement to ESC.")

(defvar escape-key-fix-ctrl-seq-message "Use C-x ~ for C-x `."
  "* Message informing users of control sequence remappings.")


(global-set-key escape-key-replacement-replacement 
		'type-escape-key-replacement)

(global-set-key escape-key-fix-ctrl-seq escape-key-fix-displaced-function)
;; Replace C-x ` with C-x ~ by default.  Doesn't work for sub modes )-:.  

(defun init-keyboard-translate-table ()
  "Initialize translate table, saving previous mappings, if any."
  (let ((the-table (make-string 256 0)))
    (let ((i 0)
	  (j (length keyboard-translate-table)))
      (while (< i j)
	(aset the-table i (elt keyboard-translate-table i))
	(setq i (1+ i)))
      (while (< i 256)
	(aset the-table i i)
	(setq i (1+ i))))
    (setq keyboard-translate-table the-table)))

(defun TERM-in-list (term-list)
  "Returns t if the current terminal (TERM) is in term-list.  Drops everthing 
in TERM past the first hyphen."
  (setq TERM-in-list nil)
  (let ((term (getenv "TERM"))
	hyphend)
    ;; Strip off hyphen and what follows
    (while (setq hyphend (string-match "[-_][^-_]+$" term))
      (setq term (substring term 0 hyphend)))
    (let ((len (length term-list))
	  (idx 0)
	  (temp-term nil))
      (while (and (< idx len)
		  (not temp-term))
	(if (string-equal term 
			  (nth idx term-list))
	    (progn
	      (setq TERM-in-list t)
	      (setq temp-term term))
	  (setq idx (1+ idx))))))
  (setq TERM-in-list TERM-in-list))

(defvar escape-key-fix-flag nil
  "*Flag to indicate if escape is remapped.")


(defun type-escape-key-replacement (arg)
  "Inserts the character escape-key-replacement, since it is overwritten
by escape-key-fix."
  (interactive "p")
  (insert-char escape-key-replacement arg))

(defun escape-key-fix (arg)
  "A quick fix for vt200 type keyboards which have no escape key.
This function remaps escape-key-replacement (` by default) to ESC.
What happens to the ` key?  Well this is a problem.  It can be
inserted as a character using C-c `, but it cannot be used in a
control character sequence like C-x `.  Instead, use C-x ~ (you may
have to do some redefining in your favorite minor modes for this.)  If
there are other problems, you will have to turn off escape-key-fix (it
toggles).

This function is not intended for permanent use.  The best solution to
the problem of the vt200 escape key is to fix your hardware.  In X
windows, you can use xmodmap, on vt300s, you can remap keys in the
setup menu."

  (interactive "p")  
  (if (> arg 1)
      (setq escape-key-fix-flag nil))
  ;; Turn escape-key-fix on for an argument greater than 1
  ;;  (e.g. C-u M-x escape-key-fix)
  (if (< arg 0) 
      (setq escape-key-fix-flag t))
  ;; Turn escape-key-fix off for negative argument
  (if escape-key-fix-flag
      ;; reset ESC and escape-key-replacement
      (progn
	(init-keyboard-translate-table)
	(aset keyboard-translate-table ?\e ?\e)
	(aset keyboard-translate-table escape-key-replacement escape-key-replacement)
	(message (concat "ESC and "
			 (single-key-description escape-key-replacement)
			 " are reset."))
	(setq escape-key-fix-flag nil)
	)
    ;; Make the "escape-key-replacement" key send ESC.
    ;; Can't swap them directly, since that messes up arrow/function keys.
    (progn
          (init-keyboard-translate-table)
      (aset keyboard-translate-table  escape-key-replacement ?\e)

      (message (concat "The "(single-key-description escape-key-replacement)
		       " key now sends ESC.  "
		       "C-c "(single-key-description escape-key-replacement)
		       " is now " 
		       (single-key-description escape-key-replacement)
		       ".  " escape-key-fix-ctrl-seq-message))

      (sleep-for 1) ; Give user a chance to see message.
      (setq escape-key-fix-flag t)
      )
    )
)

(defun auto-escape-key-fix ()
  "Assigns the ESC key to the key named in escape-key-replacement using 
escape-key-fix if the current terminal is is the list terminal-needs-escape.  
Drops everthing in TERM past the first hyphen."
  (if (boundp 'terminal-needs-escape-key)
      (if (TERM-in-list terminal-needs-escape-key)
	  (escape-key-fix 4))
    )
)

(auto-escape-key-fix)

(provide 'vt200-esc-fix)
