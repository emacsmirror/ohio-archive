;; @(#) tinyfold.el -- extra functions for folding.el

;; @(#) $Id: tinyfold.el,v 1.5 1994/12/21 10:03:26 jaalto Release_2 jaalto $
;; @(#) $Keywords: tools $
;; $KnownCompatibility: 18.57 , 19.28 $

;; This file is *NOT* part of GNU emacs


;;{{{ Documentation

;; Copyright (C) 1994 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;;               (Nov. 12. 1992  Nokia Telecommunications)
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      Dec 1 1994
;; Version:      $Revision: 1.5 $
;; state:        $State: Release_2 $
;;
;; To get information on this program use ident(1) or do M-x tic-version


;; LCD Archive Entry:
;; tinyfold|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Extra functions for folding.el|
;; 01-Dec-1994|1.5|~/misc/tinyfold.el.Z|

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

;;; Intallation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;	(require tinyfold')
;;	(fold-tiny-install-keys)	  ;this installs keybindings

;;; Commentary:

;;; In short:
;; o   enchancement functions for use with folding.el
;; o   move to next/prev fold , the pick-move is great !
;; o   open/close folds in region , delete current fold marks in _this_ fold
;; o   convert fold marks according to current mode

;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tifo-" in front of
;;   function & variable. It stands for '(ti)ny (fo)ld
;; - Because this file contains additions to existing module I'm using
;;   fold- as a function prefix for those additional functions.
;; - variable names contain letters '-v-'.

;; PREFACE
;; ========================================
;; - When I used 18.57, I came across with the marvellous packet folding.el
;;   which I immediately started using in daily basis. I knew there existed
;;   outline-mode, but I haven't got the time to dwell into it. It seemed
;;   to depend on STRING that would tell the level of heading, somehow
;;   different approach that folding.el which used rigid FOLD marks {{{ }}}
;;   to restrict the fold region.
;; - First I thought that I'll just add these functions to folding.el, but
;;   the Author of it weren't reachable anymore via email. Account did
;;   exist, but no person ever replied to my queries.
;; - These are only experimentary functions, I'll try to fix them if
;;   something serious happens. *Please* experiment with them first, before
;;   you start using them in 2Meg project... :-)
;;

;; HISTORY OF CHANGES
;; ========================================
;; Nov. 1-5 1994  Jari v1.3		Release_1
;; - First implemented
;; Dec 21   1994  Jari v1.5		Release_2
;; - added pick-move, really good movement
;; - added install-keys

;; To do list:
;; ========================================
;;

;;}}}

(provide 'tinyfold)

;;; Code:

;;; ..... Keybingings ..........................................&bind ...
;;; Copy suitable ones from here to your .emacs


(defun fold-tiny-install-keys ()
  "Installs tinyfold.el keys to folding-mode-map [folding.el].
Returns:
   nil if not installed. If called interactively, prints also error message."
  (interactive)
  (if (null (boundp 'folding-mode-map))
      (progn				;print error if called interactively
	(if (interactive-p)
	    (message "Keys not installed, folding-mode-map not present."))
	nil)				;return error
    ;;  I want to set this func, so that it's closer to the 'o'
    (define-key folding-mode-map "\C-c\C-p" 'fold-whole-buffer)

    ;; my own additions
    (define-key folding-mode-map "\C-c\C-d" 'fold-del)
    (define-key folding-mode-map "\C-c\C-v" 'fold-pick-move)
    (define-key folding-mode-map "\C-cv" 'fold-move-up)

    ;; own & also replaces... remove folds/top level
    (define-key folding-mode-map "\C-c\C-r" 'fold-region-open)
    (define-key folding-mode-map "\C-c\C-t" 'fold-region-close)
    t))

;;; ..... user configurable ................................... &conf ...

;;; ..... private variables ................................ &private ...

;;; ..... version notice ................................... &version ...


(defconst tifo-version
  "$Revision: 1.5 $"
  "version number.")


(defconst tifo-version-id
  "$Id: tinyfold.el,v 1.5 1994/12/21 10:03:26 jaalto Release_2 jaalto $"
  "Latest modification time and version number.")

(defconst tifo-version-doc
  "tinyfold.el -- extra functions for folding.el

First created: Sep. 29th 1994
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tifo-version ()
  "tinycom.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tifo-version-doc
       "\n\ncurrent version:\n" tifo-version-id)
      (pop-to-buffer  ob)
    ))


;;; ########################################################### &funcs ###



;;; ----------------------------------------------------------------------
;;;
(defun fold-get-mode-marks (mode)
  "Returns fold markers for certain mode. nil if no markers for mode
Ret: (beg end)
"
  (interactive)
  (let* (b
	 e
	 (ok t)
	 (i 0) el
	 (list fold-mode-marks-alist)
	 )
    (catch 'out
      (while (and ok			; search for mode name
		  (setq el (elt list i))); until members...
	(if (equal (elt el 0) mode)
	    (setq ok nil))
	(setq i (1+ i)))

      (if ok
	  (progn
	    (message "*err: current mode not found from fold's mode list")
	    (throw 'out t)))

      ;; ok , we're in busines.
      (setq b (elt el 1)) (setq e (elt el 2))
      )
    (list b e)))




;;; ----------------------------------------------------------------------
;;;
(defun fold-cnv-to-major-folds ()
  "Requires folding.el 1.6.2
Many times I use several modes in buffer. Eg. Lisp won't
let me do TAB for comments, so I switch to fundamental-mode.
But I might forget to get back to Lisp-mode before I execute
C-cC-f fold-region --> gets wrong kind of fold marks.

This function replaces smartly all fold marks }}} and {{{
to major mode's fold marks. See folding.el for more info about
fold marks in various modes.

As a side effecs also corrects all foldings to standard notation.
Eg. following ,where correct fold-beg should be \"# {{{ \"
Note that /// marks foldings.


  ///                  ;wrong fold
  #     ///           ;too many spaces, fold format error
  # ///title            ;ok, but title too close

  produces

  # ///
  # ///
  # /// title

<<< Remember to 'unfold' whole buffer before using this >>>

"
  (interactive)
  (let (
	el				; element
	(i 0)				; count
	n				; name
	b				; begin
	e				; end
	(bm "{{{")			; begin match mark
	(em "}}}")			;
	p				; point markers
	pp				;
	)

    (catch 'out				; is folding active/loaded ??

      (if (boundp 'fold-mode-marks-alist)
	  (setq list fold-mode-marks-alist)
	(message "folding.el not loaded. Cannot convert")
	(throw 'out t))

      (setq el (fold-get-mode-marks major-mode))
      (if (eq nil (nth 0 el)) (throw 'out t))	; ** no mode found

      ;; ok , we're in busines. Search whole buffer and replace.
      (setq b (elt el 0)) (setq e (elt el 1))
      (save-excursion
	;; handle begin mark
	(goto-char (point-min))		; start from beginnig of buf
	(while (re-search-forward bm nil t)
	  ;; set the end position for fold marker
	  (setq pp (point))      (beginning-of-line)
	  (if (looking-at b) 		; should be mode-marked; ok, ignore
	      (goto-char pp)		; note that beg-of-l cmd, move rexp
	    (delete-region (point) pp)
	    (insert b))			; replace with right fold mark
	  )

	;; handle end marks , identical func compared to prev.
	(goto-char (point-min))
	(while (re-search-forward em nil t)
	  (setq pp (point))	  (beginning-of-line)
	  (if (looking-at e)
	      (goto-char pp)
	    (delete-region (point) pp)
	    (insert e)))
	) ;; excur

      ;; ---------------------------------------- catch 'out
      )))




;;; ----------------------------------------------------------------------
;;;
(defun fold-del ()
  "Requires folding.el 1.6.2
Deletes folding marks that were added with C-c C-f. Usually the
fold to be removed is contextually decided. Normally you're asked
permission to remove the fold first. Normal undo will restore deletion.

Inside sublevel folds:
- If you are inside fold level, and NO subfolds are visible, then the
  current wrapper fold is removed.
- If you are inside fold level, and there IS subfold, then it depends
  on your position what will happen. You'll get information text on
  tricky situations.

Outside all folds:
- if you're inside fold...see prev.
- If you have open fold OR subfold visible when looking forward
  the fold will be opened/removed.


Notes:
   Note that I use /// to mark foldings, 'cause I don't wan't it to become
   confused due to these examples. The [x] marks the current cursor
   position.

Eg 1. If we are inside C-c >, the wrapper fold will  be removed


    /[x]// levelfold    <---  this wrap will be gone
	some text
    \\\                 <--   and this too...and fold level decremented


    /// levelfold
    [x]                 <--  The cursor here, you'll be asked what to do.
    /// subfold ...

    \\\

Eg 2. If we are outside of all folds, then the next fold will be opened
      and the text inside it will become visible.

    [x]
    /// test1 ...     <-- this will be opened and folds removed
    /// test2 ...


    /// test1
    [x]                   ; cursor might also be inside opened fold
    /// opened fold   <-- removed
      text inside
    \\\               <-- this too


"
  (interactive)
  (let* (
	 ch
	 (stack  (length fold-stack))	;How deep we are?
	 (RET ?\015)
	 (SPC ?\040)
	 (M   ?\r)			;\015 the invisible marker CR
	 (MS  "\r")			; string
	 (show-len  40)			; how many chars to dis. 'fold title
	 bm em
	 bmr emr			; beg/end mark rexp

	 in-f				; inside fold flag
	 efm-f				; end FOLD mark }}} flag


	 bfm-p	bbfm-p			; 'beg fold' mark beg.
	 efm-p
	 b				; begin
	 e				; end
	 p,pp				; point markers
	 del-w				; wrapper
	 del-sf				; subfold
	 el
	 line
	 )

    (catch 'out
      ;;  get mode markers
      (setq el (fold-get-mode-marks major-mode))
      (if (eq nil (nth 0 el)) (throw 'out t))	; ** no mode found

      (setq bm (elt el 0)) (setq em (elt el 1)) ; markers defined for mode
      (setq bmr (concat "^" bm))
      (setq emr (concat "^" em))



      ;;   ok , we're in busines.
      (save-excursion
	(goto-char (point-max))
	(delete-blank-lines)
	(forward-line -1)		; just adjusting position automatically
	(setq e (point))
	(setq efm-f (looking-at emr)))	; if inside C-c > , then then }}}


      ;; if we are inside fold, the narrow is in effect and point-min <> 1
      (setq in-f (if (eq 1 (point-min)) nil t))

      (beginning-of-line)
      (setq p (point))			; record user's position
      (setq pp (count-lines 1 p))	; this is more accurate position

      ;;   try to locate first BEG fold mark. If we're inside fold there might
      ;;   not be subfolds
      (save-excursion
	(if (eq (point) (point-min))	; must not be at the beg of = inside
	    (forward-line))		; because gives false subfold
	(if (eq nil (re-search-forward bmr nil t)) t ;skip, not found
	  (beginning-of-line) (setq bfm-p (point))))

      ;;   try to locate BEG fold mark upward
      (save-excursion
	(if (eq nil (re-search-backward bmr nil t)) t ;skip, not found
	  (beginning-of-line) (setq bbfm-p (point))))

      ;;
      ;;   try to locate first END fold mark.
      ;;   The text might be C-C C-s opened and cursor inside it
      (save-excursion
	(if (eq (point) (point-max))	;
	    (end-of-line))		;
	(if (eq nil (re-search-forward emr nil t)) t ;skip, not found
	  (beginning-of-line) (setq efm-p (point))))

      (if (eq nil (or efm-p bfm-p bbfm-p)) (throw 'out t)) ;nothing to do

      ;; (d! "FLAGS: in BB b E p pp s" in-f bbfm-p bfm-p efm-p p pp stack)

      (if in-f nil			; not inside  --> subfold then
	(if (> stack 0)			; look at stack
	    (setq del-sf t))
	)

      (if (null in-f) nil
	(if (eq nil bfm-p)		; inside ok but no subfolds
	    (setq del-w t)
	  ;;   inside AND subfolds found
	  ;;   what the user wants to remove ?

	  (message "remove wrapper[cr]/next[spc] fold:")
	  (setq ch (read-char))

	  (if (or (eq ch RET) (eq ch SPC)) t    ; skip,
	    (throw 'out t))		;cancelled operation
	  (if (eq ch SPC)
	      (setq del-sf t)
	    (setq del-w t))
	  ))

      ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      (if (eq nil del-w) nil
	(fold-exit)			; handle wrap request
	(fold-show)
	(save-excursion
	  (zap-to-char 1 ?\n) (delete-char 1)

	  (goto-char (point-max))
	  (if (eq nil (re-search-backward emr nil t)) nil
	    (zap-to-char 1 ?\n))
	  )
	(throw 'out t)
	)

      ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ;; Now there was subfold remove request
      ;; Is the fold opened C-c C-s and user inside it?
      ;; (d! "SUB fold ? " del-sf)

      (if (and del-sf (eq nil efm-p))
	  (if bfm-p			; No E-marker found, so check B-marker
	      (goto-char bfm-p)		; subfold visible
	    (throw 'out t))		; no folds...
	;; E-marker found
	(cond
	 ((looking-at bm) nil)		; user sits on the B-marker
	 ((eq nil bfm-p)
	  (goto-char bbfm-p))		; no subfolds inside {{ }}
	 ((< bfm-p efm-p)		; {{ x {sub }}  {new / there is sub
	  (goto-char bfm-p))
	 ((> bfm-p efm-p)		; {{ x }} {new  / no sub inside
	  (goto-char bbfm-p)		; only inside fold, goto beg mark
	  )
	 (t				; What next ? don't know...
	  nil))
	)



      ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ;;   fold is opened, so where are we in it?
      ;; (d!  "SUB X " (point))

      (beginning-of-line)
      (save-excursion
	(setq line
	      (buffer-substring
	       (progn (beginning-of-line) (point))
	       (progn (end-of-line) (point))))
	;; Unfortunately there is those nasty ^M chars
	;;
	(if (string-match MS line)
	    (setq line (substring line 0 (match-beginning 0))))
	(if (< (length line) show-len ) nil	; not too long
	  (setq line (substring line 0 show-len))) ;make it shorter
	)

      ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ;; comfirm the deletion
      (message (concat "del fold [ok]   " line))
      (setq ch (read-char))
      (if (not (eq ch RET)) (throw 'out t)) ; calcelled

      ;;   sitting on "fold closed" ?
      ;;   RE = fold-mark + skip-all + till-hide-mark
      (if (eq nil (looking-at (concat bmr "[^\r]*\r"))) nil ;skip, no...
        ;; (d! "show")
	(fold-show))			; open it !
      (zap-to-char 1 ?\n)		; ZAP the markers
      (re-search-forward emr nil t)
      (beginning-of-line) (zap-to-char 1 ?\n)

      ;; this is not very smart, but ...
      (goto-line pp)			; restore line position

      ;; ---------------------------------------- catch 'out
      )))




;;; ----------------------------------------------------------------------
(defun fold-pick-move ()
  "Pick the movement it thinks user wanted. If at the end of fold, then
moves at the beginning and viceversa. If not sitting on fold marker,
then searches forwar. If this fails, then try backward :-/

If placed oved closed fold moves to the next fold. When no next
folds are visible, stops moving. [always moves out of fold mark]
"
  (interactive)
  (let* (el
	 bm

	 )
    ;;  get mode markers
    (catch 'out
      (setq el (fold-get-mode-marks major-mode))
      (if (eq nil (nth 0 el)) (throw 'out t))	; ** no mode found
      (setq bm (elt el 0))		; markers defined for mode
      (setq em (elt el 1))		; markers defined for mode
      (beginning-of-line)

      (cond
       ;;   closed fold
       ((if (looking-at (concat bm "\\|" em ".*\.\.\."))
	    (progn
	      (forward-line 1)
	      (re-search-forward  bm nil t))))
       ((if (looking-at em)  (re-search-backward bm nil t)))
       ((if (looking-at bm) (re-search-forward  em nil t)))
       (t
	(if (null (re-search-forward bm nil t)) ;just forward then
	    (re-search-forward em nil t))))
    )))



;;; ----------------------------------------------------------------------
(defun fold-move (&optional direc)
  "Moves up/down fold headers. Backward if direc is non-nil
returns nil if not moved = no next marker.
"
  (interactive)
  (let* (el
	 bm
	 fun
	 )
    ;;  get mode markers
    (catch 'out
      (setq el (fold-get-mode-marks major-mode))
      (if (eq nil (nth 0 el)) (throw 'out t))	; ** no mode found
      (setq bm (elt el 0))		; markers defined for mode
      ;; (d! "F" bm)
      (fset 'fun 're-search-forward)
      (if (not (eq nil direc))
	  (fset 'fun 're-search-backward))
      (fun (concat "^" bm) nil t)
    )))

(defun fold-move-up ()
  "Moves upward fold headers"
  (interactive)
  (fold-move 1))


;;; ----------------------------------------------------------------------
(defun fold-region-oc (beg end &optional close)
  "Open/closes all folds up to one level inside region
Closes if optional arg is non-nil.
"
  (let (ff b e)
    ;;    Somehow the end marked changed inside loop if I used it ??
    ;;    --> copied to local variables, and now works ok...
    ;;    can't explain why it did it!
    (setq b beg) (setq e end)
    (fset 'ff 'fold-show)
    (if (eq nil close) nil		;skip, default
      (fset 'ff 'fold-hide))

    (save-excursion
      (goto-char b)
      (d! "A " (point) b e)
      (while (and  (fold-move)
		   (< (point) e))
	;;   the fold-show/hide will move point to beg-of-line
	;;   So we must move to the end of line to continue search.
	(ff) (end-of-line)
	))))


(defun fold-region-open (beg end)
  "opens all folds inside region"
  (interactive "r")
  (fold-region-oc beg end))

(defun fold-region-close (beg end)
  "opens all folds inside region"
  (interactive "r")
  (fold-region-oc beg end 1))




(defun fold-mark ()
  "Marks current fold area. If the cursor is not on the fold MARK,
then search forward and mark that fold.
**NOT IMPLEMENTED YET**
"
  (let* (b
	 (el (fold-get-mode-marks major-mode))
	 (bm (nth 0 el))
	 (em (nth 1 el))
	 (bmr (concat "^" bm))
	 (emr (concat "^" em))
	 )
    (catch 'out
      (throw 'out t)
      (if (and bm em) nil		;skip, marks ok
	(message "No fold MARKS defined in alist.")
	(throw 'out t))
      (save-excursion
	(beginning-of-line)
	(if (or (looking-at bmr)	;position at the beginning
		(re-search-forward bmr nil t))
	    ))
      )))

