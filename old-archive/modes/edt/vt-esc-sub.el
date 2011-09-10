;;; File:  vt-esc-sub.el, v 1.2
;;;
;;;               -----   -----   -----------------------
;;;               E S C   K e y   S u b s t i t u t i o n
;;;               -----   -----   -----------------------
;;;
;;;
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

;;;; Some VT series terminals, such as the VT220, have no escape key.
;;;; To enter an ESC from this terminal, users must type C-[, which
;;;; some people find annoying.  The function vt-esc-sub below allows
;;;; F11 to be used as ESC, in most cases.
;;;;
;;;; The GNU Emacs VT200 series terminal support file found in the
;;;; Emacs distribution, lisp/term/vt200.el, defines a function called
;;;; enable-arrow-keys, which enables arrow and functions keys, and
;;;; sets up F11 as a substitute ESC key when the ESC key is the first
;;;; key of a key sequence.  It does this by binding the character
;;;; sequence generated by the F11 key to ESC-prefix, which is what
;;;; ESC is bound to.  BUT this ONLY provides support for the use of
;;;; F11 for ESC in the situations where ESC is the first key of a key
;;;; sequence.
;;;;
;;;; By the way, as VT200 series terminal users know,
;;;; enable-arrow-keys is NOT set up to be invoked automatically,
;;;; because doing so breaks the standard Emacs binding to ESC [,
;;;; backward-paragraph.  But losing this standard binding is not a
;;;; big loss to most people, so most VT200 users want
;;;; enable-arrow-keys to be invoked.  Therefore, we ensure, below,
;;;; that enable-arrow-keys is called.
;;;;
;;;; Unfortunately, this does not solve the problem when the ESC
;;;; key is a second or later key in a key sequence.  There are two
;;;; such standard Emacs bindings: 
;;;;
;;;;           C-x ESC    bound to  repeat-complex-command
;;;;           ESC ESC    bound to  eval-expression
;;;;
;;;; I also like to make the personal binding of
;;;; 
;;;;           C-c ESC    bound to  electric-command-history
;;;;
;;;; Here's how to tell Emacs to let you use F11 for ESC in the above
;;;; cases:
;;;; 
;;;; First, you need to invoke vt-esc-sub at the appropriate time,
;;;; which binds C-x F11 to invoke repeat-complex-command and F11 F11
;;;; to invoke eval-expression.  For this to work, the F11 key must be
;;;; enabled.  So the function invokes enable-arrow-keys in case the
;;;; user has not done so already.  
;;;; 
;;;; Second, you need to tell Emacs to allow F11 for ESC in your
;;;; custom binding with a function, called enable-my-F11-bindings,
;;;; defined in your .emacs files.  Here's an example:
;;;;
;;;; (defun enable-my-F11-bindings ()
;;;;   (define-key (current-global-map) "\C-c\e" nil)
;;;;   (define-key (current-global-map) "\C-c\e[23~" 'electric-command-history)
;;;;   (let ((save-key (lookup-key electric-history-map "\e")))
;;;;     (define-key electric-history-map "\e" nil)
;;;;     (define-key electric-history-map "\e[23~" save-key)))
;;;; 
;;;; NOTE:  The above modifications to electric-history-map only work if 
;;;;        echistory has been loaded first.
;;;;
;;;; To make this facility available for use to all users, place this
;;;; file, vt-esc-sub.el, into your site's public emacs/lisp directory
;;;; and add the following command to your site's default.el file: 
;;;; 
;;;;   (load "vt-esc-sub")
;;;;      
;;;; The appropriate time to execute the function vt-esc-sub is AFTER
;;;; Emacs loads the terminal specific file from the directory
;;;; lisp/term in the Emacs directories.  This is done AFTER the
;;;; .emacs file is loaded by Emacs, so we must use term-setup-hook to
;;;; accomplish this automatically.  The following code added to your
;;;; .emacs file solves the problem:
;;;; 
;;;;   (setq term-setup-hook 'vt-esc-sub)
;;;;
;;;; If you are already using term-setup-hook to invoke some other
;;;; initialization function (edt-emulation-on, for example), then the
;;;; following code in your .emacs file will work instead:
;;;; 
;;;;   (defun my-term-setup ()
;;;;     (vt-esc-sub)
;;;;     (edt-emulation-on))
;;;;   (setq term-setup-hook 'my-term-setup)
;;;; 
;;;; (Note that vt-esc-sub is called BEFORE edt-emulation-on in the
;;;; example.  This is so the EDT emulation picks up the new key
;;;; definitions.)
;;;;

(defun vt-esc-sub ()
  "Set up bindings so the VT series F11 key behaves like an ESC key.
If the terminal is not the appropriate type, then the function returns
without doing anything."
  (interactive)
  (if (and (fboundp 'enable-arrow-keys)
	   (not (equal (lookup-key CSI-map "34~") 1)))
      (progn
	(enable-arrow-keys)
        (define-key ctl-x-map "\e" nil)
        (define-key ctl-x-map "\e[23~" 'repeat-complex-command)
        (define-key esc-map "\e" nil)
        (define-key esc-map "\e[23~" 'eval-expression)
        (define-key repeat-complex-command-map "\e" nil)
	(define-key repeat-complex-command-map 
	    "\e[23~p" 'previous-complex-command)
	(define-key repeat-complex-command-map 
	    "\e[23~n" 'next-complex-command)
	(if (fboundp 'enable-my-F11-bindings)
	    (enable-my-F11-bindings)))))