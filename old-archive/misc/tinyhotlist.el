;; @(#) tinyhotlist.el -- Hotlist of buffers in X, easy add, easy remove

;; @(#) $Id: tinyhotlist.el,v 1.4 1995/05/19 11:34:27 jaalto Release_1 jaalto $
;; @(#) $Keywords: X, popup, hotlist $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      May 15 1995
;;
;; To get information on this program use ident(1) or do M-x tiho-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinyhotlist|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Hotlist of buffers in X, easy add, easy remove|
;; 19-May-1995|1.4|~/misc/tinyhotlist.el.Z|

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

;;; Intallation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'tinytihotlist)
;;
;; or use autoload, more prefered
;;
;;     (autoload 'tiho-control "tinyhotlist" t t)
;;
;; Suggested keybinding
;;
;;     (global-set-key [C-S-mouse-3] 'tiho-control)
;;
;; Before you can use hotlist, read the documentation of function
;; 'tiho-control'


;;}}}
;;{{{ Briefly

;;; Commentary:

;;; Briefly:
;; o Provides X popup where you can add/remove current buffer
;;   with ease, kinda 'most recent work file list'

;;}}}
;;{{{ Documentation

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tiho-" in front of
;;   every function & variable. It stands for '(ti)ny (ho)tlist'
;; - variable names contain letters 'tiho-:', excecpt version vars.

;; PREFACE
;; ========================================
;; - I'm big fan of 'msb.el', but when it comes to files I really need
;;   some  companion with it. When I'm having an emacs session, it's
;;   very common that when I have 20 C++ files in emacs, I start
;;   reading news while the compile it going on its own buffer
;;   [normally I have 15min break], then I see some interesting
;;   article and I want to try some lisp code .. let's load couple of
;;   .emacs.XXXX configurable files. Then I realize that there is mail
;;   coming, because dragbar-time.el tells me that, I switch to rmail
;;   and start reading the latest messages... Within that 15 minute my
;;   emacs if full of buffers and When I use MSB to navigate through
;;   them I get frustrated: "Where was that buffer again, do I need to
;;   go level 3 before I can get that file visible.."
;;
;; - The navigation is especially problem if I'm only working with
;;   handful of source files _actively_, while I still have 30+ files
;;   _loadaded_ "just in case", or "for reference lookup".
;; - What I really need, is a simple hotlist for my most recent files,
;;   where I can put/remove items very easily, and which I can
;;   immediately call. No more deep level stepping with msb.el.
;;
;; - Don't get me wrong, I'd never give msb away, it's superb in class
;;   of it's own, but I also need the hotlist, because the 'most
;;   recent files' page in msb changes dynamically whenever I change
;;   buffers, but my hotlist stays the same, unless I remove a item.
;;
;; HOW TO USE THE HOTLIST
;; ========================================
;; - When you load this el, it creates hotlist cache to store the hotlist
;;   items. Obviously the list will be empty at first, but after you
;;   have added entry to it, you'll get the hotlist visible. Read the
;;   documentation of function
;;
;;	tiho-control
;;
;;   to see how entries are added or removed from hotlist.


;;}}}
;;{{{ history

;; HISTORY OF CHANGES
;; ========================================
;; May	19	1995	[jaalto]	v1.4		Release_1
;; - Only corrected some spelling errors in text, LCD etc.
;;
;; May	19	1995	[jaalto]	v1.3		Release_1
;; - Added sorting of list. Couple of user options more. Removed
;;   library dependency, since only 1 func used. Have used this
;;   extensively now, and seems working ok.
;;
;; May	16	1995	[jaalto]	v1.2		NotReleased
;; - Some minor message bugs, when adding/removing. Corrected.
;; - One serious bug: no selection was not handled. Corrected.
;;
;; May	16	1995	[jaalto]	v1.1		NotReleased
;; - Finished the code. Seems working quite well, I expect bugs
;;   still...
;;
;; May	15	1995	[jaalto]	v1.0		- first funcs


;; To do list:
;; ========================================
;; <empty>


;;}}}

;;; Code:

;;{{{ require

(require 'cl)
;;; (require 'tinylib)

;;}}}


;;; ............................................................ &bind ...

;;; ... hooks to use .......................................... &hooks ...


;;; ... private variables ................................... &private ...

(defvar tiho-:cache nil
  "*Hotlist cache.")

(defvar tiho-:package "tiho"
  "*Preseeded by messages, set to nil for no package info.")

;;; ... user configurable ...................................... &conf ...

(defvar tiho-:list-max 40
  "*Maximum members in hotlist.")

(defvar tiho-:title "     hotlist     "
  "*Title of menu")

;;  handy if you want to call from non-mouse, eg. pressing key.
;;  --> set event parameter to nil when calling func  tiho-control
;;
(defvar tiho-:x-coord 170
  "*Default menu coordinate.")

(defvar tiho-:y-coord 170
  "*Default menu coordinate.")

(defvar tiho-:msg-sleep 1
  "*Message display time. Set this at least to 1, since there may be
successive messages and moving mouse will clear message buffer otw.")




;;{{{ version

;;; ... version notice ...................................... &version ...


(defconst tiho-version
  "$Revision: 1.4 $"
  "Latest version number.")


(defconst tiho-version-id
  "$Id: tinyhotlist.el,v 1.4 1995/05/19 11:34:27 jaalto Release_1 jaalto $"
  "Latest modification time and version number.")

(defconst tiho-version-doc
  "tinyhotlist.el -- Hotlist of buffers in X, easy add, easy remove

First created: May 15 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tiho-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tiho-version-doc
       "\n\ncurrent version:\n" tiho-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &funcs ###

;;; ----------------------------------------------------------------------
;;; ##lib , my private version currently..
;;;
(defun tiho-msg-lib(error &rest args)
  "Prints packages messages. If error is t, indicate error condition."
  (let* ((pkg (or (concat tiho-:package ": ") ""))
	 )
    (ti::m-msg pkg error args)

    (sleep-for tiho-:msg-sleep)
    ))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tiho-msg (error &rest args)
  "Prints packages messages. If error is t, indicate error condition."
  (let* ((pkg (or (concat tiho-:package ": ") ""))
	 (error (if error "*err" ""))
	 )

    (setq args (mapconcat 'concat args ""))
    (message  (concat pkg error args))
    (sleep-for tiho-:msg-sleep)
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tiho-add (buffer)
  "Adds buffer to hotlist. Arg must be STRING.
Returns t or nil if added.
"
  (let* ((ptr tiho-:cache)
	 el
	 exist
	 )
    ;;  We have to check if it exists already...
    (while ptr
      (setq el (car ptr))
      (if (equal el buffer)
	  (setq ptr nil exist t)) 	;Stop scanning
      (setq ptr (cdr ptr)))

    ;;  Add only if not there.
    (if exist
	nil
      (setq tiho-:cache (append tiho-:cache (list buffer)))
      ;;  Keep it in sorted order.
      (setq tiho-:cache (sort tiho-:cache 'string-lessp))
      t)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tiho-remove (buffer)
  "Removes buffer from hotlist. Arg must be STRING.
Return t or nil if removed.
"
  (let* ((ptr tiho-:cache)
	 list el
	 ret
	 )
    (while ptr
      (setq el (car ptr))
      (if (not (equal el buffer))
	  (setq list (append list (list el)))
	;;  Stop scanning
	(setq tiho-:cache (append list (cdr ptr)))
	(setq ptr nil  ret t)
	)
      (setq ptr (cdr ptr)))
    ret
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tiho-kill ()
  "Kills [clears] whole hotlist."
  (interactive)
  (let* ()
    (setq tiho-:cache nil)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tiho-cache-update ()
  "Removes all killed buffers from cache."
  (let* ((ptr tiho-:cache)
	 buffer
	 list
	 )
    (while ptr
      (setq buffer (car ptr))
      (if (get-buffer buffer)
	  (setq list (append list (list buffer))))
      (setq ptr (cdr ptr))
      )
    (setq tiho-:cache list)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tiho-list2menu (list)
  "Converts string list into mouse menu. Items are numbered starting from 0."
  (let* (ret
	 str
	 (i 0)
	 )
    (while list
      (setq str (car list))
      (setq ret (append ret (list (cons str i))))
      (setq list (cdr list))
      (setq i (1+ i))
      )
    ret
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tiho-show-menu (event &optional title)
  "Pops the menu and selects the buffer.
If EVENT is nil, uses default coordinates to display the menu.
Returns:
      buffer name or nil.
"
  (interactive "e")
  (let* (menu
	 (title  (or title tiho-:title))
	 ;;  Allow calling from keypress also.
	 (xy     (list tiho-:x-coord  tiho-:y-coord))
	 (event  (or event (list xy  (selected-window))))
	 ans
	 item-list
	 buffer
	 )
;;;  too inefficent to be here...
;;;    (tiho-cache-update)
    (if (null tiho-:cache)
	nil
      (setq item-list  (tiho-list2menu tiho-:cache))
      (setq menu
	    (cons title
		  (list (cons title item-list))))
      (setq ans (x-popup-menu  event menu))
      (if (null ans)
	  nil
	(setq buffer (nth ans tiho-:cache))))
    buffer
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tiho-control (event &optional arg)
  "Controll center for hotlist.
The optional ARG can be:
nil           ,show the hotlist
0             ,kill all members from hotlist.
- nbr         ,[negative nbr], remove item from hotlist
+ nbr         ,[positive nbr], add current active buffer to hotlist
C-u           ,[or listp], remove active buffer from hotlist
"
  (interactive "e\nP")
  (let* ((buffer (buffer-name))
	 (menu   tiho-:cache)
	 ret
	 )
    (cond
     ;; ...................................................... display ...
     ((null arg)
      (if (null menu)
	  (tiho-msg nil  "Empty hotlist.")
	(setq ret (tiho-show-menu event))
	(if (null ret)
	    nil				;no selection
	  (if (get-buffer ret)
	      (switch-to-buffer ret)
	    (tiho-msg nil "Not exist [" ret "]. Removed from hotlist.")
	    (tiho-cache-update))))
	)
     ;; ................................................... remove/add ...
     ((integerp arg)
      (cond
       ((equal 0 arg)
	(tiho-kill)
	(tiho-msg nil "Hotlist killed.")
	)
       ((> arg 0)
	(if (tiho-add buffer)
	    (tiho-msg nil "Added [" buffer "] to hotlist.")
	  (message "Already in hotlist.")))
       ((< arg 0)
	(if (null menu)
	    (message "Empty hotlist.")
	   (setq ret (tiho-show-menu event "--Remove item--"))
	   (if (null ret) nil		;user cancelled
	     (tiho-remove ret)
	     (tiho-msg nil "Removed. [" ret "]")
	     ))
	)))
     ;; ............................................... remove current ...
     ((listp arg)
      (if (tiho-remove buffer)
	  (tiho-msg nil "removed [" buffer "] from hotlist.")
	(tiho-msg nil
		  "Not in hotlist.")))
     )
    ))



(provide 'tinyhotlist)
;;; ................ end of tinyhotlist.el ...............................
