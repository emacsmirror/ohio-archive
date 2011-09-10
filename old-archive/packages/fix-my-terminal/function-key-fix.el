;;; File:  function-key-fix.el, v 1.1 (emacs 18[.58] version)
;;;
;;;               -----   ---------------   -------
;;;               F i x   F u n c t i o n   K e y s
;;;               -----   ---------------   ------
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;;

;; LCD Archive Entry:
;; fix-my-terminal|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Fixes:  flow control, arrow keys, and vt200 function keys|
;; 93-03-23|1.0|~/packages/fix-my-terminal.tar.Z

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

;; When emacs first starts up, it knows nothing about the arrow or
;; function keys.  Emacs was designed to work just fine this way!
;; However, many users are accustomed to using these keys from other
;; editors.  This is where the problem begins.  Emacs runs on all
;; kinds of hardware, much of which has different ways of handling the
;; function keys.  You can see that adding support for all of them
;; could be hellish.

;; Fortunately, support for emacs users on ANSI terminals (e.g. vt100,
;; vt200, etc.), has been added through code in keypad.el and the
;; files in lisp/term.  However, if you read these files, you will
;; find that the arrow/function keys on ANSI terminals are transmitted
;; via a complicated set of characters which have the unfortunate
;; feature of starting with ESC-[ and that in emacs, ESC-[ means
;; backward-paragraph.

;; Due to some elisp magic (sparse key maps), vt100 terminal arrow
;; keys have been working for a long time.  However, this magic does
;; not extend to the vt200 type function keys.  Users who like to use
;; these keys should put the following lines in their .emacs files.

;;        (require 'function-key-fix)                   
;;        (auto-function-key-fix)                       
;;        ;; ESC-[ as backward-paragraph is lost.       
;;        ;; !!! No term-setup-hook below this point !!!

;; or call function-key-fix from their term-setup-hook (see vtxxx.el
;; for an example).

;; WARNING!  Not all of emacs is set up to work well with ANSI
;; arrow/function keys.  Just try terminating an I-search with an
;; arrow key.  Getting used to C-p C-n, etc. is not such a bad idea.

;; This code depends on the terminal setup stuff that comes standard
;; with the emacs 18 version.  If you don't have your environment
;; variable TERM properly defined (e.g. vt100 or vt200), it might not
;; work right.

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
;;         ;; A horrible hack to get remove key on VT200 keyboards        
;;         ;; working in emacs' own xterms.  You must feed the following  
;;         ;; code to xmodmap to get this to work:                        
;;                                                                        
;;         ;; keycode 140 = F17                                           
;;         ;; ! Fix dead "Remove" key for emacs by mapping it to F17      
;;         ;; ! Code in default.el maps F17 to 'kill-region.              
;;         ;; ! Note that remove key is thus messed up for other software.
;;         ;; ! Hope that emacs-19 has a better fix.                      
;;                                                                        
;;         (and (fboundp 'enable-arrow-keys)                              
;;              (progn                                                    
;;                (enable-arrow-keys)                                     
;;                (define-key CSI-map "31~" 'kill-region))   ; F17        
;;              )                                                         
;;         (and user-term-setup-hook                                      
;;              (funcall user-term-setup-hook))                           
;;         )                                                              
;;       ))                                                               

(provide 'function-key-fix)

(defun auto-function-key-fix ()
  "Turns on the arrow/function keys without overmapping a previously
defined term-setup-hook.  To use this, put the following lines in your
.emacs file:

(require 'function-key-fix)                   
(auto-function-key-fix)                       
;; ESC-[ as backward-paragraph is lost.       
;; !!! No term-setup-hook below this point !!!"

  ;; Save existing term-setup-hook here.  Note: the order of execution
  ;; is important (and confusing): the function auto-function-key-fix
  ;; must be called after the term-setup-hook is defined, but
  ;; term-setup-hook itself won't be executed until later.  Hence any
  ;; variables containing old term-setup-hooks must be global and have
  ;; unique names. 

  (setq site-term-setup-hook term-setup-hook)
  (setq term-setup-hook
	(function
	 (lambda ()
	   (and 
	    ;; Make sure vtxxx has been read in
	    (fboundp 'enable-arrow-keys)
	    (enable-arrow-keys))
	   ;; Call the old term-setup-hook whether or not the function
	   ;; keys were enabled.  
	   (and site-term-setup-hook
		(funcall site-term-setup-hook))
	   )))
  )
