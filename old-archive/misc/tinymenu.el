;; @(#) tinymenu.el -- Simple echo-area menus for less used functions.

;; @(#) $Id: tinymenu.el,v 1.6 1996/02/01 15:07:14 jaalto Release_1 $
;; @(#) $Keywords: tools, menu $
;; $KnownCompatibility: 19.28 $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1996 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Feb 1 1996
;;
;; To get information on this program use ident(1) or do M-x timu-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinymenu|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Simple echo-area menus for calling less used functions easily.|
;; 21-Jan-1996|1.6|~/misc/tinymenu.el.gz|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;	(require 'tinymenu)
;;
;;  Or use autoload, which is prefered. Your emacs starts up faster
;;
;;	(autoload 'timu-menu "tinymenu" t t)
;;
;;  To bring up the menu (or menus), bind the main function into some key:
;;
;;      (global-set-key "\C-cM"   'timu-menu)
;;
;;  If you want to access some special menus, you can use this:
;;
;;	(global-set-key "\C-M0"    'timu-menu)  ;; the main menu
;;	(global-set-key "\C-M1"
;;	   '(lambda () (interactive) (timu-menu my-menu1)))
;;	(global-set-key "\C-M2"
;;	   '(lambda () (interactive) (timu-menu my-menu2)))
;;
;;   Just make sure you have defined the variables my-menu1 and my-menu2
;;   which hold the menu information.

;;}}}
;;{{{ docs

;;; Commentary:

;; PREFACE
;; ========================================
;; - Oh Boy, my ~/.emacs.keys file started looking like a mess.. There
;;   were hundreds of bindings that were impossible to remember
;;   any more and  file size currently shows 74K. Of course it
;;   holds hook settings too, so it's not all key bindings.
;;
;; - Many times I was staring at the buffer and wondering
;;
;;    "..I'm sure I have bound function that deletes blank lines
;;     somewhere..but I can't remember where did I bind it!"
;;
;; - Normally I'd reach for the M-x COMMAND to try to
;;   remember the function name, but other times I'd hit C-x b to
;;   bring up all bindings. Then I would start tedious search
;;   with C-s to find the function I was longing for...
;;
;; - I thought that there definitely had to be some way to
;;   construct menu that would use echo area. Instead of
;;   remembering that I had created (bindings)
;;
;;	"C-zmf"		, C-z prefix, m = maps, f = folding-minor-mode
;;	"C-zmF"		, ... auto-fill minor mode
;;
;;   I can now call, say F1, to bring up menu which would assist
;;   me to find proper function.
;;
;; - Now I don't have to use m-x COMMAND almost any more, because
;;   all seldom used functions are behind this menu :-)
;;
;; - Oh yes, One more thing that really is nice. Have you ever
;;   struggled with emacs key bindings? What if you work in  3 - 5
;;   different sites, where every site has it's own version of
;;   emacs.. even XE against GNU? You're starting to get the idea
;;   --> it's almost impossible to have same bindings in every
;;       emacs, especially if some are in X and some are in tty.
;;
;;   But cure is this package! You just define the menu and the
;;   hot-key to bring up the menu and voila: you always have
;;   functions in available in same "key bindings" (via menu)
;;
;; - Hope you enjoy this another 'tiny tool' as much as I do.


;;; Change Log:

;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Feb	1	1996	[jari]		19.28	v1.7		Release_1
;; - Okay. Put this to ohio. Small text changes only.
;;
;; Feb	1	1996	[jari]		19.28	v1.6		NotReleased
;; - Still bugs in the FLAG handling. May this time...
;;
;; Feb	1	1996	[jari]		19.28	v1.5		NotReleased
;; - Oops, I didn't realize that I had used English "time" for my
;;   library abbreviation (ti)ny (me)nu --> changed all names to timu
;;   (ti)ny (m)en(u)
;;
;; Feb	1	1996	[jari]		19.28	v1.4		NotReleased
;; - Added new parameter FLAG to the list.now it's possible return
;;   back to list after calling a function or evaling a form. Nice.
;;
;; Feb	1	1996	[jari]		19.28	v1.3		NotReleased
;; - Some finishing touches.
;;
;; Feb	1	1996	[jari]		19.28	v1.2		NotReleased
;; - Rewrote the character reading routines. Now they are available
;;   in tinylibm.el too.
;;
;; Feb	1	1996	[jari]		19.28	v1.1		NotReleased
;; - Started. Works nicely.

;;}}}

;;; Code:

;;{{{ setup: variables

;;; .......................................................... version ...

(defconst timu-version-id
  "$Id: tinymenu.el,v 1.6 1996/02/01 15:07:14 jaalto Release_1 $"
  "Latest modification time and version number.")



(defun timu-version ()
  "Version information."
  (interactive)
  (message timu-version-id))

;;; ....................................................... &variables ...

;;;  A sample menu. This is how you use the package
;;;
(defvar timu-menu-sample
  '(
    "1=test1, 2=test2, m=mode, u = undefined , e = eval "
    ((?1 . (  (timu-test1 1 2 3)))	;this does not have FLAG
     (?2 . (t (timu-test2)))		;notice FLAG used.
     (?m . timu-menu-mode)
     (?u . timu-menu-not-exist)		;this variable does not exist :-)
     (?e . (t (progn
		(message "menu item evaled.")
		(sleep-for 1)
		)))
    ))
  "*This variable holds menu structure. Characters 'q' and 'Q' are reserved
for quitting the menu prompt, so they cannot be used for keys. The structure
must have items.

Tha FLAG is optional. It says if the menu should be shown after
function has completed. If FLAG is missing, the menu is not displayed
after the function call.

'(
  DISPLAYED-MENU-STRING
  ((CHARTER-KEY    . ANOTHER-MENU-VARIABLE-SYMBOL)
   (CHARACTER-KEY  . ([FLAG] (FUNCTION-NAME PARAMETER PARAMETER...))
   (CHARACTER-KEY  . ([FLAG] (FORM-TO-EVAL))
   ..
   )))
")

(defconst timu-menu-mode
  '(
    "c, C=C++, l=lisp, t=text, f=funda, F=autofill, / = back to root menu"
    ((?c . ( (c-mode)))
     (?C . ( (cc-mode)))
     (?l . ( (lisp-mode)))
     (?t . ( (text-mode)))
     (?f . ( (fundamental-mode)))
     (?F . ( (auto-fill-mode)))
     (?/ . timu-menu-sample)		;back to ROOT menu
     ))
  "*a sample mode menu.")

;;}}}

;;{{{ code: lib funcs

;;; ................................................ tinylibm.el funcs ...
;;; These are stolen from there to make this package self standing.

(defsubst timu-read-char-safe-until (&optional prompt)
  "Read character until given. Discards any events that are
not characters.

Returns
  character
"
  (let* (ch)
    (while (eq 'ignore (setq ch  (read-char-safe prompt))))
    ch
    ))


(defsubst timu-read-char-safe (&optional prompt)
  "Read character safely. The read-char command chokes if you happen to
move mouse. This function returns 'ignore if the read-char couldn't read
answer otherwise it returns normal character.

See function read-char-safe-until too.

Return:
  ch        ,character code
  'ignore   ,if read failed
"
  (condition-case nil
      (progn
	(if  prompt (message prompt))
	(discard-input)			;this is a must
	(read-char))
    (error
     'ignore)))

;;}}}
;;{{{ code: test funcs

;;; ....................................................... test funcs ...

(defun timu-test1 (&optional arg1 arg2 arg3)
  "Menu test function."
  (message (format "function 1 called with args: %s %s %s" arg1 arg2 arg3)))

(defun timu-test2 ()
  "Menu test function."
  (message (format "function 2 called"))
  (sleep-for 1)
  )

;;}}}
;;{{{ code: menu

;;; ........................................................ menu func ...



(defun timu-menu (&optional menu)
  "Simple echo area menu. Plese read the documentation of variable
timu-menu to see the structure of menu."
  (interactive)
  (let* ((m		(or menu timu-menu-sample))
	 (loop		t)
	 prompt flag alist
	 key  elt
	 eval-form
	 )
    (while loop
      (setq prompt	(nth 0 m)
	    alist	(nth 1 m))

      ;;  Safe reading. Eg. moving the mouse confuses read-char
      (setq key (timu-read-char-safe-until prompt))

      ;; .................................................. what key ? ...
      (cond
       ((memq key '(?q ?Q))
	(setq loop nil))

       ((setq elt (assq key alist))
	(setq elt (cdr elt))

;;;	(d! (format "%s" elt)

        ;; ................................. new menu or call function ...
	(cond
	 ((symbolp elt)
	  (if (not (boundp elt))
	      (error (format "Menu variable does not exist: %s" elt))
	    ;;  replace with another menu
	    (setq m (eval elt))
	    ))
	 ((listp elt)

	  ;;  See if there is flag.
	  (cond
	   ((eq 2 (length elt))
	    (setq flag t)
	    (setq elt (nth 1 elt)))
	   (t
	    (setq flag nil)
	    (setq elt (nth 0 elt))))

	  (cond
	   ((fboundp (car elt))		;is first element function ?
	    (setq eval-form elt)
	    (setq loop flag))
	   (t
	    (error "Menu structure error %s" elt)
	    )))))
       (t
	;;  Key not found from list, keep looping
	))

      (message "")			;clear echo area

      (if (not (null eval-form))
	  (eval eval-form))
      )))

;;}}}


;;; tinymenu.el ends here
;;; ............................................................. &eof ...
