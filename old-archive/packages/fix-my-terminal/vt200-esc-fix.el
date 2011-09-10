;;; File:  vt200-esc-fix.el, v 4.0 (emacs 18[.58] version)
;;;
;;;               -----------   -----   -----
;;;               E s c a p e   K e y   F i x
;;;               -----------   -----   -----
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;; Thanks to Joe Wells for encouragement to code everything right.  


;; LCD Archive Entry:
;; fix-my-terminal|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Fixes:  flow control, arrow keys, and vt200 function keys|
;; 93-03-23|1.0|~/packages/fix-my-terminal.tar.Z

;; Archived at archive.cis.ohio-state.edu

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
;;;; inaccessible (including "do," "find," etc) and puts the ESC key
;;;; in a rather inconvenient place (F11).  Another solution is to run
;;;; the terminal in vt200 mode and use M-x enable-arrow-keys.  This
;;;; maps F11 (which is really ESC[23~) to ESC-prefix.  Unfortunately,
;;;; ESC-prefix is not really ESC and some parts of emacs (like
;;;; isearch) have troubles with it (try to terminate an isearch with
;;;; F11 and you will get a [23~ in your buffer).

;;;; The best solution is to get the terminal to remap some keys
;;;; (vt300s and xterms do this).  The usual remapping is to put the
;;;; ~/` key down on the </> key and put the < and > above the , and .
;;;; where they belong.  Also, F11 can sometimes be made to send a
;;;; real ESC, not an ESC[23~.

;;;; This code simulates the above remappings as best as emacs allows.
;;;; First of all, there is no way software can tell the difference
;;;; between , and shift-, if the terminal sends the same character
;;;; for both.  This means that something is going to get lost.  Since
;;;; only one key is being remapped to ESC (` by default), I have
;;;; chosen to lose it.  

;;;; You can probably see what's going to happen when you run this
;;;; code: everything is the same on the keyboard, except that when
;;;; you press ` you get ESC instead.  How will you get `?  This is
;;;; where the fun begins.  

;;;; Earlier versions of this code assumed that you would just turn
;;;; escape-key-fix off whenever you wanted `.  You can still do this,
;;;; and in many respects this is the least confusing thing to do.

;;;; In order to achieve easier access to the ` key, I have mapped
;;;; C-c-ESC to ` (this will actually look like C-c ` on your
;;;; keyboard).  You can also make the F11 key send ` with the code:

;;;; (setq term-setup-hook
;;;;     (function
;;;;       (lambda ()
;;;; 	 (and (fboundp 'enable-arrow-keys)
;;;; 	      (progn
;;;; 		(enable-arrow-keys)
;;;; 		(define-key CSI-map "23~" 'type-escape-key-replacement)   ; F11
;;;;           ))
;;;; 	 ))
;;;; )

;;;; Unfortunately, this is not good enough.  When processing C-x and
;;;; C-c events, emacs looks for literal keystrokes.  So, you cannot
;;;; use "C-x F11" for "C-x `."  Nor can you use C-x C-c ` (you will
;;;; be popped out of emacs).  Therefore, more keys have to be
;;;; remapped!  

;;;; Again, since the ` key is the only messed up key, I have remapped
;;;; only the key bindings that use it (and that I know about).  So
;;;; far, I've remapped C-x ` (next-error) to C-x ~.  The binding of
;;;; C-x ~ (nil) is saved so that it can be restored when
;;;; escape-key-fix is turned off.  Tell me if there are any other
;;;; bindings that are effected and I will add them to
;;;; escape-key-fix-rebind-list.

;;;; Unfortunately, there will be a fundamental problem if any local
;;;; modes remap these keys.  Code will have to be added to each of
;;;; these modes to remap things out of the way of `.  


;;;; Installation:

;;;; To make this facility available for all users, place this file,
;;;; vt200-esc-fix.el, into your site's public emacs/lisp directory
;;;; and add the following command to your site's default.el file:

;;;;		(require 'vt200-esc-fix)
;;;;            (auto-escape-key-fix)


;;;; NOTE.  You must also have flow-ctrl.el on your load path.

;;;; This code can either be called interactively with "M-x escape-key-fix"
;;;; or started automatically with the code:

;;;;  (setq terminal-needs-escape-key
;;;;        '("vt200" "vt201" "vt220" "vt240"))

;;;; in the user's .emacs file.  To turn it off, just use "M-x
;;;; escape-key-fix" again.  

;;;; The ESC key can be remapped to another key on the keyboard that
;;;; sends a single character (not the function keys, sorry) with the
;;;; code:

;;;; (setq escape-key-replacement ?<)
;;;; (setq escape-key-fix-rebind-alist '(("\C-c\e" . "\C-c\e") 
;;;;				          ("\C-x<" . "\C-xl")))

;;;; You can bind type-escape-key-replacement to any key.  For
;;;; instance, if you wanted C-t to send ` (or your customized
;;;; escape-key-replacement), you would use the code:

;;;; (global-set-key "\C-t" 'type-escape-key-replacement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vt200-esc-fix)
(require 'flow-ctrl)

;; Define default ESC replacement and associated work arounds.  Tell me if 
;; there are any more conflicts.

(defvar escape-key-replacement ?`
  "*Ascii code of escape key replacement.  Example of setting this variable: 
(setq escape-key-replacement ?<).")

(defvar escape-key-fix-rebind-alist '(("\C-c\e" . "\C-c\e") 
				     ("\C-x`" . "\C-x~"))
  "*List containing pairs of key sequences.  The first element of each pair 
is the key sequence messed up by escape-key-fix, the second is the key 
sequence which replaces it.  If the key sequence is repeated, that key
sequence is bound to type-escape-key-replacement.  Example of setting
this variable: 
\(setq escape-key-fix-rebind-alist '((""\\C-c\\e"" . ""\\C-c\\e"") 
				     (""\\C-x<"" . ""\\C-xl""))).")

(defvar escape-key-fix-overmapped-complex-keys nil
  "*List of cons cells.  The first element of each cell is a key sequence,
the second is its binding when escape-key-fix is not active.")

(defvar escape-key-fix-remap-message nil
  "*Message informing users of control sequence remappings.")

(defun escape-key-fix-remap-complex-keys (rebind-alist)
  "Remaps complex key sequences (like C-x `) for escape-key-fix.
Creates escape-key-fix-overmapped-complex-keys and
escape-key-fix-remap-message."
  (while rebind-alist 
    ;; Unpack rebind-alist.
    (let ((old-key (car (car rebind-alist)))
	  (new-key (cdr (car rebind-alist))))
      ;; Make a list of overmapped keys so they can be restored later
      (setq escape-key-fix-overmapped-complex-keys 
	    (cons (cons new-key (key-binding new-key))
		  escape-key-fix-overmapped-complex-keys))
      (if (string-equal new-key old-key)
	  ;; Signal to bind this key sequence to type-escape-key-replacemant
	  (progn
	    (global-set-key new-key 'type-escape-key-replacement)
	    (setq escape-key-fix-remap-message 
		  (concat escape-key-fix-remap-message 
			  (key-description new-key)
			  " is now " 
			  (single-key-description escape-key-replacement)
			  ".  ")))
	(progn
	  (global-set-key new-key (key-binding old-key))
	  (setq escape-key-fix-remap-message 
		(concat escape-key-fix-remap-message "Use "
			(key-description new-key)
			" for " 
			(key-description old-key)
			".  "))))
      (setq rebind-alist (cdr rebind-alist)))))


(defun escape-key-fix-restore-complex-keys (overmapped-list)
    (while overmapped-list 
      (let ((restore-key (car (car overmapped-list)))
	    (displaced-function (cdr (car overmapped-list))))
	(global-set-key restore-key displaced-function)
	(setq overmapped-list (cdr overmapped-list))
	)))


(defun type-escape-key-replacement (arg)
  "Inserts the character escape-key-replacement, since it is overwritten
by escape-key-fix."
  (interactive "p")
  (insert-char escape-key-replacement arg))

(defvar escape-key-fix-flag nil
  "*Flag to indicate if escape is remapped.")


(defun escape-key-fix (arg)
  "A quick fix for vt200 type keyboards which have no escape key.
This function remaps escape-key-replacement \(` by default\) to ESC
and provides hooks for remapping all other effected keys and key
sequences \(escape-key-fix-rebind-alist\) for remaping effected complex
control character sequences \(like C-x `\).  ALL remappings are
restored when escape-key-fix is called again \(it toggles\).

This function is not intended for permanent use.  The best solution to
the problem of the vt200 escape key is to fix your hardware.  In X
windows, you can use xmodmap, on vt300s, you can remap keys in the
setup menu."

  (interactive
   (list
    (if current-prefix-arg
	;; If an argument is specified, then turn on if non-negative else
	;; turn off if negative.
	(>= (prefix-numeric-value current-prefix-arg) 0)
      ;; If no argument is specified, then toggle.
      'toggle)))

  (setq escape-key-fix-flag
	(if (eq arg 'toggle)
	    (not escape-key-fix-flag)
	 arg))
  ;; Thanks to the elisp prowess of jbw for the above sequence!

  (if escape-key-fix-flag
      (progn
	;; Make the "escape-key-replacement" key send ESC.
	;; Can't swap them directly, since that messes up arrow/function keys.
	(init-keyboard-translate-table)
	(aset keyboard-translate-table  escape-key-replacement ?\e)
	(setq escape-key-fix-remap-message nil)
	(escape-key-fix-remap-complex-keys escape-key-fix-rebind-alist)
	(message (concat "The "(single-key-description escape-key-replacement)
			 " key now sends ESC.  "
			 escape-key-fix-remap-message))
	(sleep-for 1) ; Give user a chance to see message.
      )
    ;; reset ESC, escape-key-replacement, and the list of keys in 
    ;; escape-key-fix-overmapped-complex-keys
    (progn
      (init-keyboard-translate-table)
      (aset keyboard-translate-table ?\e ?\e)
      (aset keyboard-translate-table 
	    escape-key-replacement escape-key-replacement)
      (escape-key-fix-restore-complex-keys 
       escape-key-fix-overmapped-complex-keys)
      (setq escape-key-fix-overmapped-complex-keys nil)
      (message 
       (concat "The " (single-key-description escape-key-replacement)
	       " key is no longer ESC.  All associated bindings are reset."))
      )
    )
  )

(defun auto-escape-key-fix ()
  "Assigns the ESC key to the key named in escape-key-replacement using 
escape-key-fix if the current terminal is is the list terminal-needs-escape.  
Drops everthing in TERM past the first hyphen."
  (if (boundp 'terminal-needs-escape-key)
      (if (TERM-in-list terminal-needs-escape-key)
	  (escape-key-fix t))
    )
  )
