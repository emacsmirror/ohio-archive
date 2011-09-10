;; @(#) tinymacro.el -- Fast way to assign newly created macro to key

;; @(#) $Id: tinymacro.el,v 1.2 1995/03/24 07:57:11 jaalto Release_1 jaalto $
;; @(#) $Keywords: macro, key bind, making variables $
;; $KnownCompatibility: 18.57 , 19.28 $
;; $outlineRegexp: $
;; $bookMarkRegexp: $
;; This file is *NOT* part of GNU emacs


;;{{{ Documentation

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      Mar 23 1995
;; Version:      $Revision: 1.2 $
;; state:        $State: Release_1 $
;;
;; To get information on this program use ident(1) or do M-x tim-version


;; LCD Archive Entry:
;; tinymacro|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Fast way to assign newly created macro to key|
;; 24-Mar-1995|1.2|~/misc/tinymacro.el.Z|

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

;;; Installation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'tinymacro)
;;
;; OR autoload, preferable way
;;
;;     (autoload 'tim-assign "tinymacro" "" t)

;;; Commentary:

;;; In short:
;; o  When you have created macro, just hit tim-assign to put it into any key.
;; o  To see the macro assignments to keys, just call tim-macro-info


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tim-" in front of
;;   every function & variable. It stands for '(ti)ny (m)acro
;; - variable names contain letters 'tim-:'

;; PREFACE
;; ========================================
;; - Started as a very little project when mosh@ramanujan.cs.albany.edu
;;   (Mohsin-Ahmed) Mar 17 -95 sent a post to g.e.h asking for easy way
;;   to assign newly created macro to some key.
;; - I sent a reply, where I had made simple function to do such thing,
;;   unfortunately Mohsin bounced me and complained that it didn't
;;   work right :') and that he didn't want only one macro, which was
;;   recycled every time. I had coded it so that it used only _one_,
;;   and the same function name every time it was called, effectively
;;   erasing the old macro..
;; - I started twisting the code little bit, and so I finally came up with
;;   this, which uses user configurable macro-name-functions.
;;
;; - Seems to be working in the 18.57, but I plan no support for it.
;;   I tried to put some macro into F1, and I never know it appeared
;;   as "C-x 4 ~ 1" .. so much key key strokes to represent F1 in 18.57,wow!
;;   No wonder I never programmed the F keys in 18.


;; HISTORY OF CHANGES
;; ========================================
;;
;;
;; Mar 24	1995    19.28   v1.2	[jaalto]	Release_1
;; - More goodies: funcs tim-macro-info , tim-func2key-desc , hooks ...
;;
;; Mar 23	1995    19.28   v1.1	[jaalto]	NotReleased
;; - First implementation sent to Mohsin.


;; To do list:
;; ========================================
;;

;;}}}

(provide 'tinymacro)

;;; Code:

;;; ............................................................ &bind ...

;;; ... private variables ................................... &private ...


(defvar tim-:stack-ptr 0
  "Keeps record of available stack  space.")

(defvar tim-:last-macro-func nil
  "holds last function SYMBOL that were used in assignment.")

(defvar tim-:last-macro-key nil
  "holds last key STRING or VECTOR that were used in assignment.")


;;; ........................................................... &hooks ...

(defvar tim-macro-assigned-hook nil
  "*If new macro were asiigned, this hook will be run. The function
SYMBOL that was used is in variable tim-:last-macro-func")


;;; ... user configurable ...................................... &conf ...

(defvar tim-:general-name "tim--macro"
  "*The lisp name to use, when assigning name to last kbd macro")

(defvar tim-:wrap-query nil
  "*If set to non-nil, then the user is asked before the macro stack
if recycled when it got full. This means that macro numbering, starts
from 0 agaon.")

(defvar tim-:replace-key-query nil
  "*When NON-NIL; If user selects keys that already hold some functionality,
eg. function or previous macro he will be asked for confirmation before
new macro to it assigned to these keys.")

(defvar tim-:stack-max 10
  "*Maximum stack depth of unique macronames. The name run from 0..max,
and wraps to 0 after max.")

(defvar tim-:tmp-buffer "*temp*"
  "*Temporary buffer. Eg. displaying the macro bindings to keys.")

;;; ... version notice ...................................... &version ...


(defconst tim-version
  "$Revision: 1.2 $"
  "Latest version number.")

(defconst tim-version-id
  "$Id: tinymacro.el,v 1.2 1995/03/24 07:57:11 jaalto Release_1 jaalto $"
  "Latest modification time and version number.")

(defconst tim-version-doc
  " tinymacro.el -- Fast way to assign newly created macro to key

First created: Mar 23 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi>
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi>

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tim-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tim-version-doc
       "\n\ncurrent version:\n" tim-version-id)
      (pop-to-buffer  ob)
    ))

;;; ########################################################### &funcs ###



;;; ----------------------------------------------------------------------
;;;
(defun tim-yn ()
  "Return t if [yY] pressed, otherwise returns nil."
  (let (ch)
    (setq ch (read-char))
    (if (or (equal ?y ch) (equal ?Y ch))
	t nil)))


;;; ----------------------------------------------------------------------
;;;
(defun tim-create-symbol()
  "Creates macro variable. Returns NEW or EXISTING SYMBOL."
  ;;  Make good habit to copy globals, so that user knows what the
  ;;  function uses. Makes maintaining easier.
  (let* ((max tim-:stack-max)
	 (sp tim-:stack-ptr)
	 (q tim-:wrap-query)
	 (n tim-:general-name)
	 (make q)			; Q: allewed to make new macro ?
	 sym
	 sym2
	 new
	 ret
	 )
    (defun stack++ ()
      (concat n (if (< sp max)		; 0..max
		    (setq sp (1+ sp))
		  (setq sp 0))))

    (if (or (null q) (< sp max))	; yes, go ahead with new
	(setq new (stack++))
      (message "Macro stack full, wrap y/[n] ?")
      (if (tim-yn)
	  (setq new (stack++))
	))

    (if (null new) nil			;  Must update stack
      (setq tim-:stack-ptr sp)
      (setq ret (intern-soft new))	; returns symbol

      (if ret nil			; Already exist
	(setq sym (make-symbol new))	; make new variable from scratch
	;;   Seem stricky ?, just a)make it b)set to nil c)put into ret val
	(setq sym2 (intern new))
	(set sym2 nil)
	(setq ret sym2)
	))
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tim-create-name()
  "Creates macro name."
  ;;  Make good habit to copy globals, so that user knows what the
  ;;  function uses. Makes maintaining easier.
  (let* ((max tim-:stack-max)
	 (sp tim-:stack-ptr)
	 (q tim-:wrap-query)
	 (n tim-:general-name)
	 new
	 )
    (defun stack++ ()
      (concat n (if (< sp max)		; 0..max
		    (setq sp (1+ sp))
		  (setq sp 0))))

    (if (or q (< sp max))		; yes, go ahead with new
	(setq new (stack++))
      (message "Macro stack full, wrap y/[n] ?")
      (if (tim-yn)
	  (setq new (stack++))
	))

    (if new				; Must update stack
	(setq tim-:stack-ptr sp))
    new
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tim-func2key-desc (func mode)
  "Tries to find Key for the function if it exists.
The returned STRING  value depends on the mode.
MODE:
  'desc  --> string representation like: 1 --> C-a
  'raw   --> direct translation of code: 1 --> ^A
"
  (let* (key-desc			;the final result
	 key				;vector, if func bound
	 desc
	 (meta-str (char-to-string 27))	;that "^["
	 )

    (setq key
	  (or (where-is-internal  func global-map t)
	      (where-is-internal  func  (current-local-map) t)))

    (if (null key) nil
      (setq key-desc
	    (mapconcat
	     '(lambda (x)
		(cond
		 ((eq mode 'raw)
		  (if (< x 255)		; normal character
		      (char-to-string x)
		    ;;   - The high numbers are like M-f = 883710 in my kbd
		    ;;   - translate the kbd code first
		    (setq desc (single-key-description x))
		    (if (null (string-match "M-." desc))
			""		;don't know what it was !
		      (concat meta-str (substring desc -1 nil)))
		    ))
		 ((eq mode 'desc)
		  (single-key-description x))
		 ))
	     key
	     ""
	     ))
      (if (string= key-desc "")	;nothing found, option wrong ?
	  (setq key-desc nil))
      )

    key-desc
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tim-macro-info ()
  "Shows which macros are assigned to which keys."
  (interactive)
  (let* ((i 0)
	 (sp tim-:stack-ptr)
	 (max tim-:stack-max)
	 (buf tim-:tmp-buffer)
	 (base tim-:general-name)
	 (ob (current-buffer))		;original
	 (round 0)
	 bp				;buffer pointer
	 name
	 key
	 )
    (while (< i (1+ max))
      (setq name (concat base i)   i (1+ i)   key "")
      (if (null (intern-soft name)) nil	;not use yet

	(if (> round 0) nil		;do only once
	  (setq bp (get-buffer-create buf))
	  (set-buffer bp) (erase-buffer)
	  (insert (format "Stack pointer : %d\n" sp )))
	(if (null (setq key (tim-func2key-desc (intern name) 'desc)))
	    (setq key "[??]"))		;should never happen
	(insert (format "%-10s %s\n" key name))
	(setq round (1+ round))
	))

    (if (and (interactive-p) (eq 0 round))
	(message "No macros bound or set."))
    (switch-to-buffer-other-window bp)
    ;; (set-buffer ob)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tim-assign (&optional key)
  "Names last macro and assigns it to user defined key.
Runs tim-macro-assigned-hook if key macro gets installed.
The query options should be turned off if you calkl this within
function, since it always return nil if the options were on.

Input:
  key  Should be valid emacs key-bind-sequence/key-vector
Returns:
  t    is assigned
  nil  not assigned keyboard-quit

"
  (interactive)
  (let* ((q tim-:replace-key-query)	;permission to assign
	 (do-it (not q))		;when query=nil, do-it=t
	 (f-name "")			;func name
	 yn				;temporary function
	 macro-name			;our new macro !
	 lookup				;what was found

	 ;; --- 1 ---
 	 ;; The bullet proof way to find key bind for abort
	 (abort-ch (tim-func2key-desc 'keyboard-quit 'raw))

	 ;; --- 2 --
	 ;; - Or we just say where it is... Nobody relocates it anyway
	 ;; - concat does the same as char-to-string, it's also shorter :-/
	 ;; (abort-ch (concat ?\007))
	 )


    (if key nil				;read the key if not given
      (setq key (read-key-sequence "Set last macro to key(s): ")))

    (if (equal key abort-ch)
	(progn
	  (if (interactive-p) (message "Not assigned."))
	  nil)

      ;;  Search the key, if it's already assigned
      ;;
      (setq lookup
	    (or (lookup-key (current-local-map) key)
		(lookup-key global-map key) key))

      (if (null lookup) nil		;skip this
	(cond				;what we found ?
	 ((and (symbolp lookup) (fboundp lookup))	;just plain function
	  (setq f-name (symbol-name lookup)))
	 ((vectorp lookup)
	  (setq f-name "[..vector]"))
	 (t
	  (setq f-name "[unidentified]"))))

      (if (or (null q) (null f)) nil	;it's free for us
	(if (null (interactive-p)) nil	;ask only if interactive, otw discard
	  (message
	   (concat "Key already occupied by (" f-name ") , remove it y/[n]? "))
	  (setq do-it (tim-yn))))

      (if (null do-it) nil
	(setq macro-name (tim-create-symbol))
	(name-last-kbd-macro macro-name)
	(global-set-key key macro-name)

	(setq tim-:last-macro-func macro-name ;set >> GLOBALS <<
	      tim-:last-macro-key key)

	(if (interactive-p)
	    (message
	     (concat "Assigned to macro name : " (symbol-name macro-name))))
	(run-hooks 'tim-macro-assigned-hook)
	t
	)
      );; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ... abort-key
    ))


;;; ................ end of tinymacro.el ...................................
