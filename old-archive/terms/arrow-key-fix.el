;;; File:  arrow-key-fix.el, v 1.1 (emacs 18[.58] version)
;;;
;;;               -----   ---------   -------
;;;               F i x   A r r o w   K e y s
;;;               -----   ---------   -------
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;;

;; LCD Archive Entry:
;; arrow-key-fix|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Turns on the arrow AND function keys.|
;; 22-Mar-1993|1.1|~/terms/arrow-key-fix.el.Z|


;; Archived at archive.cis.ohio-state.edu.


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

;; When emacs first starts up, it knows nothing about the arrow keys.
;; Emacs was designed to work just fine this way!  However, many users
;; are accustomed to using the arrow keys from other editors.  This is
;; where the problem begins.  Emacs runs on all kinds of hardware,
;; much of which has different ways of handling the arrow keys.  You
;; can see that adding support for all of them could be hellish.

;; Fortunately, support for emacs users on ANSI terminals (e.g. vt100,
;; vt200, etc.), has been added through code in keypad.el and the
;; files in lisp/term.  However, if you read these files, you will
;; find that the arrow keys on ANSI terminals are transmitted via a
;; complicated set of characters which have the unfortunate feature of
;; starting with ESC-[ and that in emacs, ESC-[ means
;; backward-paragraph.

;; Why not just overmap backwards-paragraph for all emacs users?
;; That's just what many sites do, since many more people like the
;; arrow keys than backward-paragraph.  Since each site does the setup
;; in a slightly different way, a user usually can't get
;; backward-paragraph back if they like it over the arrow keys.  Also,
;; to turn on the arrow keys, the site must use a term-setup-hook,
;; which may conflict with the user's term-setup-hook.

;; Isn't there a way to turn on the arrow/function keys by default,
;; but allow someone to define ESC-[ as backward-paragraph if they
;; want?  That's just what this code does.

;; WARNING!  Not all of emacs is set up to work well with ANSI arrow
;; keys.  Just try terminating an I-search with an arrow key.  Getting
;; used to C-p C-n, etc. is not such a bad idea.

;; INSTALLATION

;; Copy this file into a directory recognized by emacs and put the line
;; (load "arrow-key-fix")
;; in your site's site-init.el file and the line
;; (auto-enable-arrow-keys) 
;; in the default.el file _after_ defining any term-setup-hook (see
;; below).  In order for everything to work properly, YOU MUST
;; RECOMPILE EMACS.

;; If you can't recompile your emacs for some reason, you can fix the
;; arrow keys for yourself by putting the lines:

;; (load "arrow-key-fix" nil t)
;; (auto-enable-arrow-keys) 

;; in your .emacs file AFTER you define a term-setup-hook (if any).  

;; This code depends on the terminal setup stuff that comes standard
;; with the emacs 18 version.  If you don't have your terminal type
;; properly defined (e.g. vt100 or vt200), it might not work right.

;; NOTE: extending setup-hooks is tricky business.  I had to define a
;; variable in which to save the already existing term-setup-hook (I
;; called it site-term-setup-hook).  If you want to have more
;; automatic term setup stuff for your site, YOU MUST USE A DIFFERENT
;; NAME in which to save the user's setup hook (e.g.
;; user-term-setup-hook).  Here is an example (to be put in the
;; default.el file):

;;(setq user-term-setup-hook term-setup-hook)
;;(setq term-setup-hook
;;      (function
;;       (lambda ()
;;	 ;; A horrible hack to get remove key on LK105 keyboards
;;	 ;; working in emacs' own xterms.  You must feed the following
;;	 ;; code to xmodmap to get this to work:
;;
;;	 ;; keycode 140 = F17
;;	 ;; ! Fix dead "Remove" key for emacs by mapping it to F17
;;	 ;; ! Code in default.el maps F17 to 'kill-region.  
;;	 ;; ! Note that remove key is thus messed up for other software.
;;	 ;; ! Hope that emacs-19 has a better fix.
;;
;;	 (define-key CSI-map "31~" 'kill-region)
;;	 (and user-term-setup-hook
;;	      (funcall user-term-setup-hook))
;;	 )))


(define-key esc-map "[" nil)
;; ESC-[ (backward-paragraph) will be overmapped by the terminal
;; setup code that gets the arrow keys working.  Set it to nil here so
;; when called from default.el, auto-enable-arrow-keys knows whether
;; or not a user has mucked with it (e.g. really wants it to be
;; backwards-paragraph or anything else for that matter).

(defun auto-enable-arrow-keys ()
  "Politely turns on the arrow/function keys without overmapping the
user's term-setup-hook or a preference for ESC-[.  To reset ESC-[ to
its default value, put the code:
\(define-key esc-map ""["" 'backwards-paragraph\)
in your .emacs file."

  ;; Save existing term-setup-hook here.  Note: the order of execution
  ;; is important (and confusing): the function auto-enable-arrow-keys
  ;; must be called after the term-setup-hook is defined, but
  ;; term-setup-hook itself won't be executed until later.  Hence any
  ;; variables containing old term-setup-hooks must be global and have
  ;; unique names. 

  (setq site-term-setup-hook term-setup-hook)
  (setq term-setup-hook
	(function
	 (lambda ()
	   (and 
	    ;; Make sure the vt100/200 keymaps have been read in:
	    (fboundp 'enable-arrow-keys)
	    ;; and user hasn't expressed an opinion about ESC-[
	    (not (lookup-key esc-map "["))
	    (enable-arrow-keys))
	   ;; Call the old term-setup-hook whether or not the arrow
	   ;; keys were enabled.  
	   (and site-term-setup-hook
		(funcall site-term-setup-hook))
	   )))
  )
