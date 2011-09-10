;; @(#) tinytab.el -- Programmers TAB mov/handler minor mode. Very flexible.

;; @(#) $Id: tinytab.el,v 1.10 1995/11/02 09:07:09 jaalto Release_2 jaalto $
;; @(#) $Keywords: tab, minor mode $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Oct 26 1994
;;
;; To get information on this program use ident(1) or do M-x tit-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinytab|Jari Aalto|jari.aalto@ntc.nokia.com|
;; TAB handling minor mode, eg. step 4 chars bck/fwd , insert/del 4 spaces |
;; 02-Nov-1995|1.10|~/misc/tinytab.el.Z|

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
;;     (require 'tinytab)
;;
;; - OR use this; your .emacs will load much  quicker
;;
;;     (autoload 'tit-mode    "tinytab"		t t)
;;
;; - It will load the file only when you turn the mode on for the first time.
;; - Suggested keybinding (compare to tinyindent.el, only available key)
;;
;;	(global-set-key		[C-S-backtab]	'tit-mode)
;;
;;	;;  For non Windowed
;;	;;  -->  let's make C-cm as prefix for all "minor mode*s
;;	(global-unset-key	"\C-cm"		nil)
;;	(global-set-key		"\C-cm+"	'tit-mode)
;;
;;
;; Advertise:
;; - You relly must get these two packages too, if you're serious about
;;   formatting text. They're located in same directory in ohio as this
;;   is.
;;
;;	tinyeat.el	--  Guess Eating blocks of text forward, backward
;;	tinyindent.el	--  like indented-text-mode, but minor mode.

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o  Programmable TAB. If you set the spacing to 4, you can virtually
;;    program "blindly" without any other modes.
;; o  Includes moving commands: tab-forward, tab-backward
;; o  Includes modify commands: tab-insert, tab-delete


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tit-" in front of
;;   every function & variable. It stands for '(ti)ny (t)ab minor mode.
;; - variable names contain letters 'tit-:', excecpt hook/mode vars.

;; PREFACE
;; ========================================
;; - Again, I saw a post in gnu.emacs.sources (what an source of
;;   inspiration!), where someone asked:
;;
;;	"Is there anyway to reset the number of spaces that TAB does?
;;	 Like, I want to make it jump four spaces instead of the
;;	 usual whatever.How can I set the tabs to 4?
;;	"
;;
;;   And the typical answer was:
;;
;;      "In .emacs, set the variable tab-stop-list, like so:
;;       (setq tab-stop-list (list 4 8 12 ...))
;;      "
;;
;; - Well, I didn't want to touch the original tab-stop-list,
;;   since I want to have 8 tabs normally. But I like to have 4 tabs
;;   when I'm doing my shell programming or whatever text
;;   formatting, I though it was time to turn on my
;;   think-and-create-mode.
;; - The aim was very simple: Develop a minor mode, that you can turn
;;   on and off, which handles _only_ tab movement. That shouldn't
;;   be hard at all.
;; - Yes, If someone of you know, I have also created tinyindent.el,
;;   which is kinda tab-alike-mode too. But there is a substle, but
;;   very important difference... it behaves according to previous
;;   lines, so its job is to "line up" with the previous text(code).
;;
;;   Instead this mode was supposed to be plain rigid. The tab goes where
;;   I want it, and I can control the amount of movement to either
;;   direction, back or forward.
;;
;; - After 3 hours this package was ready and running. And I sure like
;;   this one as much as tinyindent.el. These two make happy couple :=)
;;
;; So, what is this thing?
;; ........................................
;; - Basicly tab-stop-list implementation through minor mode if
;;   some analogy can be drawn.
;; - You only set one variable, that controls the amount of
;;   movement, whereas you would have to put many values inside
;;   tab-stop-list. The variable is:
;;
;;	 tit-mode-name-div-factor
;;
;; - The advantage is, than you don't have to alter the tab-stop-list at
;;   all. When the mode is off, the tabs are back again as they were (or as
;;   the other mode thinks how the tab should behave)
;; - But what I like most, is the movement and deletion funtions, that
;;   really make day shine when you program. They all operate according
;;   to the variable  tit-mode-name-div-factor. That's the greatest
;;   advantage of using this minor mode.
;; - Now when you have to delete backward "one indentation level", say
;;   the value of the div-factor is 4, you can have it with this package.
;;   The nearest column that satisfies the div-factor is used when
;;   "indenting back". See the example:
;;
;;   Before:
;;             *		<< cursor here
;;   123 567 9012		<< columns, starting from 1
;;
;;   After:
;;          *			<< cursor here, at div-factor compliant point
;;   123 567 9012		<< columns, starting from 1
;;
;; - Howabout turning on permanent tit-mode with value of 8,
;;   would we say goodbye to tab-stop-list altogether :-^)
;; - Don't forget to look at function
;;
;;	tit-4-8-toggle
;;
;;   Which allows you to switch between 4 and 8 tabs mode with one keypress.
;;
;;
;; Finally
;; ........................................
;; - If you're wondering how minor modes are implemented, this
;;   is perfect file that will teach you that. Just look at the code
;;   (it's so minimal) and you'll be writing your own minor mode
;;   in no time.
;;
;; Development note
;; ........................................
;; - This package solely concerns TAB key. Nothing outside of it is
;;   counted and I won't add any other keys.

;;}}}
;;{{{ history

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Nov	2	1995	[jari]		19.28	v1.10		Release_2
;; - Renamed many function/var names. Made better key setting function
;;   for user in nonWindowed emacs. Now uses "add-minor-mode"
;;   function from my libs.
;;
;; Oct	28	1995	[jari]		19.28	v1.9		NotReleased
;; - Okay, I forgot. The analogy must be for the -del function too,
;;   so now there is new variable. That's it for tonaight, it's
;;   01:00 and I got to go to bed or i sleep right now...
;;   tit-:tab-special-del-funcs	    :+ func list
;;   tit-tab-backward-del	    :! modified
;;
;; Oct	28	1995	[jari]		19.28	v1.8		NotReleased
;; - It's past midnight and my inspiration still flows.. Now I wanted to
;;   have special indentation if previous line held comment, so I
;;   integrated the gap between this package and tinyindent.el
;;   NEXTP			    :+ from my libs..
;;   tit-:tab-special-insert-funcs	    :+ This is ist of funcs to call
;;   tit-tab-forward-insert	    :! modified to take advantage of it
;;
;; Oct	27	1995	[jari]		19.28	v1.7		Release_1
;; - I'm pretty satisfied now, so let's make this first alpha.
;;   OHIO, here I come...
;;
;; Oct	27	1995	[jari]		19.28	v1.6		NotReleased
;; - Cleaned compile errors away.
;;
;; Oct	27	1995	[jari]		19.28	v1.5		NotReleased
;; - Added missing ###autoload.
;; - Hmm, The tab-insert and tab-del gives me headache. This file turned
;;   into much more complex than I intended at the beginning.
;; - Now the problem was with moving "inside" the tabs char, that  caused
;;   "\t" code to be replaced with multiple spaces. And when going
;;   forward, we really don't want to leave "    " + "    ", instead we
;;   like to see nice "\t"'s while we advance..
;; - But I did all these, since this is _the_ tab mode.
;;   EOLP,BOLP..		:+ from my lib
;;   ti::s-tabify		:- deleted, not needed in this version
;;   tit-XXX			:! renamed all tab functions
;;   tit-tab-forward-insert	:! fully reprogrammed
;;   tit-tab-backward-del	:! fully reprogrammed
;;
;; Oct	27	1995	[jari]		19.28	v1.4		NotReleased
;; - Samll change to ky defining function.. C-tab goes forward.
;;
;; Oct	27	1995	[jari]		19.28	v1.3		NotReleased
;; - The sugar missed... The tab width should be shown in the modeline.
;;   Now it displays it
;;   tit-:mode-name		:+ mode name moved here
;;   tit-set-mode-name		:+ function to change the modename
;;   tit-mode-name		:! no more user variable
;;
;; Oct	27	1995	[jari]		19.28	v1.2		NotReleased
;; - Bugs crawling out. Serious error happened when the *tmp* buffer
;;   was used for tabify. If it accidentally were tuned to read-only, the
;;   tab-inset couldn't use it.
;; - The backward tab deletion was also a flop. Corrected.
;; - Correected by adding couple of more lib funcs. Added "*" to
;;   appropriate interactive funcs.
;; - I had an exellent idea, new 4-8 function that allows swithing
;;   tabs real fast...oh boy this is getting good!
;;   use-tmp-buffer		:+ lib func
;;   ti::m-tmp-buffer		:+ lib func
;;   tit-4-8-toggle		:+ fast 4-8-4 tab switching
;;   tit-tab-backward-del	:! deleted chars, error in regexp
;;   tit-tab-backward-del	:! totally rewritten
;;   tit-mode-map-define-keys	:! more instructions
;;
;; Oct	26	1995	[jari]		19.28	v1.1		Created

;; To do list:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.


;;}}}

;;; Code:

;;; ......................................................... &require ...

(require 'backquote)

;;{{{ setup: variables

;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar tit-load-hook nil
  "*Hook that is run when package is loaded.")

(defvar tit-mode-hooks nil
  "*Hook that is run when mode is turned on.")

(defvar tit-mode-name nil		;
  "Minor mode name. Changed by program automatically.")


;;; ... private variables ................................. &v-private ...

;;; ... user configurable .................................. &v-config ...



(make-variable-buffer-local 'tit-mode)
(defvar tit-mode nil
  "*Mode on/off variable.")

(defvar tit-mode-map nil
  "Minor keymap \\[tit-mode]")

(defvar tit-:div-factor 4
  "*The tab division.")

;;   Simple name is enough. Think this as "Tab +" or "extended tab" -mode
;;
(defvar tit-:mode-name " +"
  "*Minor mode name")


;;  If I accidentally press key I didn't meant to, I want to know
;;  about it. Like in empty line, where is no visual aids
;;
(defvar tit-:verbose t
  "*Enable Insert/Delete messages.")


;;  Different users may want to set the keys differently.
;;
(defvar tit-:key-func 'tit-mode-map-define-keys
  "*Function to define the keys for mode.")


;;  For special cases, it nice to 'hook up' some
;;  other function. Like special indent from tinyindent.el
;;
(defvar tit-:tab-special-insert-funcs nil
  "*List of functions to call before tit-tab-forward-insert is performed.
If any of these functions return non-nil, it will be assumed,
that the special tab handling were performed, so the tit-tab-forward-insert
function returns immediately.

If all funcs return nil. The normal tit-tab-forward-insert is performed.
")


;;  For special cases, it nice to 'hook up' some
;;  other function. Like special indent from tinyindent.el
;;
(defvar tit-:tab-special-del-funcs nil
  "*List of functions to call before tit-tab-backward-del is performed.
If any of these functions return non-nil, it will be assumed,
that the special tab handling were performed, so the tit-tab-backward-del
function returns immediately.

If all funcs return nil. The normal tit-tab-backward-del is performed.
")



;;}}}

;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tit-:version
  "$Revision: 1.10 $"
  "Latest version number.")


(defconst tit-:version-id
  "$Id: tinytab.el,v 1.10 1995/11/02 09:07:09 jaalto Release_2 jaalto $"
  "Latest modification time and version number.")

(defconst tit-:version-doc
  "tinytab.el -- Programmers TAB mov/handler minor mode. Very flexible.

First created: Oct 26 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi>
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi>

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tit-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tit-:version-doc
       "\n\ncurrent version:\n" tit-:version-id)
      (pop-to-buffer  ob)
    ))

;;}}}
;;{{{ Lib

;;; ############################################################# &Lib ###
;;; ** Do not copy these, they will be in tinylibXX.el distribution
;;;    released later.
;;; ** This section will be removed from this file when the libs are out.


(defvar ti:-tmp-buffer "*tmp*" "No docs")




(defmacro BOLP ()
  "Returns beginning of line. Preserves point"
  (` (save-excursion
       (beginning-of-line) (point))))

(defmacro BOLPP ()
  "Returns end of line. Moves point."
  (` (progn
       (beginning-of-line) (point))))

(defmacro EOLP ()
  "Returns end of line. Preserves point."
  (` (save-excursion
       (end-of-line) (point))))

(defmacro EOLPP ()
  "Returns beginning of line. Moves point."
  (` (progn
       (end-of-line) (point))))

(defmacro PMIN ()
  "goes to point-min"
  (` (goto-char (point-min))))

(defmacro PMAX ()
  "goes to point-max"
  (` (goto-char (point-max))))

(defmacro NEXTP (list)
  "Advances list pointer with cdr."
  (` (setq (, list) (cdr (, list)))))



;;; ----------------------------------------------------------------------
;;;
(defmacro var-toggle (var &optional arg)
  "Usefull for for functions that use arg 0 = off, 1 = on, nil = toggle.
Minor modes behave this way.

VAR is set to following values when ARG is:
  arg 0     VAR -> nil
  arg nbr   VAR -> t
  arg nil   VAR -> not(var)     toggles variable
"
  (` (setq (, var)
	   (if (or
		;;   test if ARG is == 0
		(and (integerp (, arg))
		     (zerop (, arg)))
		;;   OR test if ARG == NIL
		(and (null (, arg))
		     (, var)))
	       nil
	     t))
     ))





;;; ----------------------------------------------------------------------
;;; - The buffer is *not* cleared, only put to consistent state
;;;
(defun use-tmp-buffer (&optional buffer)
  "Creates one if not exist. Removes read-only. Uses \"*tmp*\" by default.
Puts buffer to fundamental-mode.

Returns:
  buffer
"
  (let* ((buffer  (get-buffer-create (or buffer "*tmp*")))
	 )
    (save-excursion
      (set-buffer buffer)
      (fundamental-mode)
      (setq buffer-read-only nil)
      )
    buffer
    ))



;;; ----------------------------------------------------------------------
;;;
(defun ti::m-tmp-buffer ()
  "Returns packages tmp buffer and prepares it for use."
  (use-tmp-buffer ti:-tmp-buffer))


;;; ----------------------------------------------------------------------
;;; - Why doesn't emacs offer this simple interface by default ?
;;;
(defun add-minor-mode (mode-name-sym mode-func-sym mode-map)
  "Add the minor mode into emacs. If mode exists, do nothing.

Input:
  mode-name-sym		,mode name, variable symbol
  mode-func-sym		,mode to turn on, func symbol
  mode-map		,keymap
"
  (or (assq mode-func-sym minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons mode-func-sym  mode-map)
		  minor-mode-map-alist)))

  ;;  Update minor-mode-alist
  (or (assq  mode-func-sym minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list mode-func-sym mode-name-sym)
		  minor-mode-alist)))
  )



;;}}}
;;{{{ code: misc

;;; ########################################################### &Funcs ###

;;; These are moved here, only because the add-minor-mode func
;;; must be defined first, otw we cannot call it.


;;; ----------------------------------------------------------------------
;;;
(defun tit-mode-map-define-keys ()
  "Defines keys to tit-mode-map."

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . Non Wwindow . .
  (define-key   tit-mode-map "\C-i"		'tit-tab-forward-insert)
  (define-key   tit-mode-map "\e\C-i"		'tit-tab-backward-del)

  ;;  Hmmmm, Control and Shift are not recognized in NonWindow
  ;;  This even given me error...

;;;_  (define-key   tit-mode-map "C-\C-i"		'tit-tab-forward)
;;;_  (define-key   tit-mode-map "S-\C-i"		'tit-tab-backward-del)

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. X keys . .
  ;;  These are recognized by X only

  (define-key   tit-mode-map [tab]		'tit-tab-forward-insert)
  (define-key   tit-mode-map [C-tab]		'tit-tab-forward)
  (define-key   tit-mode-map [S-backtab]	'tit-tab-backward-del)


  ;; Some PC have this as S-backtab (PC throught Xceed to unix)
  (define-key	tit-mode-map [S-kp-tab]		'tit-tab-backward-del)


  ;;  I would have put this key into ALT-TAB, but my HP-kbd
  ;;  doesn't recognize that at all :-(.
  ;;  I have put this to faster key:
  ;;
  ;;	(define-key   tit-mode-map [?\C-`]		'tit-tab-backward)
  ;;
  ;;  Which is the first non-shift key at upper left in my kbd.
  (define-key   tit-mode-map [?\e tab]		'tit-tab-backward)

  ;;  This is real life saver... easy swithing between the tabs
  ;;  I also recommend to relocate this command to some fast key.
  ;;  I have put it into C-1, which is close to TAB key.
  ;;
  ;;      (define-key   tit-mode-map [?\C-1]		'tit-4-8-toggle)
  ;;
  (define-key   tit-mode-map [?\e \C-tab]	'tit-4-8-toggle)
  )


(if tit-mode-map
    nil
  (setq tit-mode-map  (make-sparse-keymap))
  (funcall tit-:key-func)
  (add-minor-mode 'tit-mode-name 'tit-mode tit-mode-map)
  )

;;}}}
;;{{{ code: tab


;;; ----------------------------------------------------------------------
;;;
(defun tit-set-mode-name ()
  "Sets mode name according to tabs in effect."
  (interactive)
  (let* ((base  tit-:mode-name)
	 (val   tit-:div-factor)
	 )
    (setq tit-mode-name (concat base val))
    (force-mode-line-update)
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tit-4-8-toggle ()
  "Toggles tab width from 8 to 4 or from 4 to 8."
  (interactive)
  (let* ((verb  (interactive-p))
	 (val   tit-:div-factor)
	 )
    (cond
     ((not (integerp val))
      (setq val 4))			;default value

     ((eq 4 val)
      (setq val 8))

     ((eq 8 val)
      (setq val 4))
     )

    (setq tit-:div-factor val)		;update
    (tit-set-mode-name)
    (if verb				;this does no harm....
	(message (concat "Tab factor is now " val)))
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tit-tab-backward-del ()
  "Tab movement backward, deletes whitespace, if all chars are
whitespaces preceeding the point AND the final position is in div-factor.

Eg. If you factor is 4, and there is 2 spaces before your cursor \"*\",
    Thi func will not delete the extra spaces, because it can't reach
    position 8.

	 bar Geezy *
	 12345678901234
            ^   ^     ^

In this case, calling the function is no-op.

References:
  tit-:tab-special-del-funcs
  tit-:div-factor

"
  (interactive "*")
  (let* ((funcs tit-:tab-special-del-funcs)
	 (div   tit-:div-factor)
	 (verb  tit-:verbose)
	 (col   (current-column))
	 (dest  (- col (% col div)))
	 (bol   (BOLP))
	 MARK
	 str
	 eob				;flag
	 p p2				;points
	 )

    (catch 'cancel

      (while funcs			;special tab handlling ?
	(if (funcall (car funcs))
	    (throw 'cancel t))
	(NEXTP funcs))

      ;; ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^ normal deletion ^ ^
      (setq MARK  (save-excursion
		    (if (eobp)
			(setq eob t)
		      (forward-char 1))	;push marker one forward
		    (point-marker)))

      (if (= col dest )			; would be exact
	  (setq dest (- col div )))
      (if (< dest 0)
	  (setq dest 0))


      (if (= col 0)			;beg of line
	  nil
	(move-to-column dest 'force)	;converts tabs to spaces.

	;; consider following:
	;;    actual         seen
	;;    12345678       123456789
	;;    ----------------------------------
	;;    #\thello      "#       hello"	,suppose cursor is in "h"
	;;    |  |
	;;    |  point 3
	;;    point 1
	;;
	;;    Now you indent back by 4, this is what happens
	;;    12345678       12345678
	;;    #   hello	   "#   hello"
	;;    |   |
	;;    |	 point 5			, Geez!
	;;    point 1
	;;
	;;    The tab is converted and it caused all point to be altered.
	;;    That's why we have to use the marker, because it stays
	;;    releative to text, in this case just _behind_ the letter "h"
	;;

	(setq p  (if eob
		     (marker-position MARK)
		   (1- (marker-position MARK))))
	(setq p2 (point))
	(setq str (buffer-substring p2 p))


;;;       (d! p2 p str dest col (point) )

	(if (not (string-match "^[ \t]+$" str))
	    (progn
	      (if verb (message "Can't reach previous tab position"))
	      (goto-char p)		;do not move. Stay put.
	      )
	  (delete-region p2 p)

	  (if verb (message "Deleted"))
	  ))

      (setq MARK nil)			;kill the marker
      )))

;;; ----------------------------------------------------------------------
;;;
(defun tit-tab-backward ()
  "Logical tab movement backward, until bol."
   (interactive)
   (let* ((div   tit-:div-factor)
	  (dest  (- (current-column) div))
	  )

     (if (< dest 0)
	 (setq dest 0))

     (move-to-column dest 'force)
     ))

;;


;;; ----------------------------------------------------------------------
;;;
(defun tit-tab-forward-insert ()
  "Tab movement forward, inserts spaces or tabs, see
variable indent-tabs-mode.

References:
  tit-:tab-special-insert-funcs
  tit-:div-factor
"
   (interactive "*")
   (let* ((funcs tit-:tab-special-insert-funcs)
	  (div   tit-:div-factor)
	  (verb  tit-:verbose)
	  (col   (current-column))
	  (nbr   (- div (% col div)))
	  status			;call status
	  eob				;flag
	  MARK				;marker
	  str
	  )

     (catch 'cancel

       (while funcs			;special tab handlling ?
	 (if (funcall (car funcs))
	     (throw 'cancel t))
	 (NEXTP funcs))

       (cond
	((= 0 nbr)
	 (setq str (make-string div ?\ )))
	(t
	 (setq str (make-string nbr ?\ ))
	 ))

       (insert str)

       (if (null indent-tabs-mode)
	   nil
	 ;; - When we insert non-tabs, like in mode "tab 4", what happens is
	 ;;   that we insert "    " + "    " ie. 4 + 4 spaces.
	 ;; - but, we really like them to be like one "\t" code in text,
	 ;;   So, let's fix the line every time something is inserted.
	 ;; - We have to use markers again due to tabify.
	 ;; - The EOB is special case
	 ;;
	 (setq MARK (save-excursion
		      (if (eobp)
			  (setq eob t)
			(forward-char 1))
		      (point-marker)))

	 (tabify (BOLP) (point))
	 (goto-char (if eob
			(EOLPP)
		      (1- (marker-position MARK))))

	 (setq MARK nil)			;kill it
	 )
       (if verb (message "Insert"))
       )				;the catch

     ))


;;; ----------------------------------------------------------------------
;;;
(defun tit-tab-forward ()
  "Step on logical tab forward. Does not insert anything. Stops at EOL.
Tabs are converted to spaces when needed; because you can't step into
'\t' code otherwise..
"
   (interactive)
   (let* ((div   tit-:div-factor)
	  (col   (current-column))
	  (nbr   (- div (% col div)))
	  (eol   (EOLP))
	  (dest  (+ (point) nbr))
	  str
	  )
     (if (> dest eol)
	 (end-of-line)
       (move-to-column (+ col nbr)) 'force)
     ))



;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tit-mode (&optional arg)
  "Tab movement minor mode, based on division factor.

References:
  tit-:div-factor

Mode description:
\\{tit-mode}

"
  (interactive)
  (var-toggle tit-mode arg)		;so simple with right tools :-/
  (force-mode-line-update)
  (if (interactive-p)
      (message
       (concat "Tab minor mode is "
	       (if tit-mode "on" "off"))))
  (tit-set-mode-name)
  (run-hooks 'tit-mode-hooks)
  )

;;}}}
;;{{{ example

;;; ......................................................... &example ...
;;; - Code can be easily extracted with function tinylib.el/ti::m-pkg-rip-magic
;;;
;;; - As usual, I provide some examples for the user.
;;; - I find using the tinyindent.el as much as this mode, so
;;;   here is some integration of the too.
;;;
;;;   The code will do special indent if
;;;   o  you're at the beginning of line, and there is comment
;;;      above you.

;;* (setq  tit-:tab-special-insert-funcs '(my-tit-special-tab-insert))
;;* _
;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-tit-special-tab-insert ()
;;*   "Special tab handling. Continues comment if needed"
;;*   (let* (fill
;;* 	 )
;;*     (require 'tinyindent)
;;*     (if (not (= 0 (current-column)))
;;* 	nil
;;*       (save-excursion
;;* 	(forward-line -1)		;peek previous line ?
;;* 	(setq fill (tii-special-handle)))
;;* _
;;*       (if (not (stringp fill))
;;* 	  nil
;;* 	(insert fill)
;;* 	t				;we handled it!
;;* 	))
;;*     ))

;;}}}


(provide   'tinytab)
(run-hooks 'tit-load-hook)

;;; ............................................................. &eof ...
