;; @(#) tinyindent.el -- TAB indentation minor-mode, smart

;; @(#) $Id: tinyindent.el,v 1.12 1995/09/02 10:32:14 jaalto Release_3 $
;; @(#) $Keywords: X, editing, indentation with TAB $
;; $KnownCompatibility: XE19.12 , FSF19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;;{{{ Id

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com> (Finland)
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Sep 18 1994
;;
;; To get information on this program use ident(1) or do M-x tii-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el
;;
;; Original idea by Alan K. Stebbens in autoindent.el


;; LCD Archive Entry:
;; tinyindent|Jari Aalto|jari.aalto@ntc.nokia.com|
;; TAB indentation minor-mode, like indented-text-mode but in minor mode form|
;; 02-Sep-1995|1.12|~/misc/tinyindent.el.Z|

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
;;{{{ Installation

;; ....................................................... &t-install ...
;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;     (require 'tinyindent)
;;
;; - OR use this; your .emacs will load much  quicker
;;
;;     (autoload 'tii-mode    "tinyindent" t t)
;;     (autoload 'tii-tt-mode "tinyindent" t t)
;;
;; Suggested keybindings, you're going to use them a lot..
;;
;;     	(global-set-key [C-tab]     'tii-tt-mode) ;; this is toggle
;;	(global-set-key [S-backtab] 'tii-mode)    ;; this is on/off mode

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o  General block editing or indentation MINOR mode. Replacement for
;;    indented-text-mode.
;; o  Takes over the TAB and BACKSPACE key.
;; o  Looks back to guess right indentation. and uses relative indent
;;    when not at BOL.
;; o  Special indentation is suggested if cursor is at BOL and user defined
;;    regexp is matched in line above. (like adding multiple c++ comments)
;; o  Extra tii-tt-mode for writing descriptions within comments. This
;;    allows user to choose when to use HARD tab or SOFT tab = relative
;;    to the text above. TAB TAB inserts hard tab, TAB SPC inserts soft tab.

;; PREFACE
;; ========================================
;; - The original auto-indent-mode from autoindent.el was very short
;;   and limited, so I thought I extend it a little...here is the
;;   result.  Thank you Alan for giving me a push to extend your code
;;   into new directions.  When I spoke with Alan and he gave me free
;;   hands, because he hadn't used the .el for quite a long time.
;; - I wasn't satisfied with the  indent-relative function, so
;;   I coded a preprocessor for it. Now the cursor won't jump
;;   all over the line if the previous one was empty. Just
;;   try original  M-x indent-relative when there is empty line above
;;   and you'll see what I mean.
;;
;; - And where this module really shines: Has it ever been easier to line
;;   up variables according to '=' or in within lisp 'let*', or writing
;;   mail messages while this mode is turned on...
;;
;; WORD ABOUT DEFINING KEYS TO MAPS
;; ........................................
;; - Do not EVER use local-set-key inside some my-XXX-mode-hook,
;;   because you may loose these keybindings when minor modes are turned on.
;; - Always use (define-key XXX-mode-map <key> <func>) inside hooks
;;   and your keybindings will not be affected.
;;
;;
;; Simple example for lisp mode:		*righ version*
;;
;;   (add-hook 'lisp-mode-hook 'my-lisp-hook)
;;   (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
;;
;;   (defun my-lisp-hook ()
;;     "My lisp setings"
;;     (define-key shared-lisp-mode-map		;; <<<<
;;       "\M-e"
;;       '(lambda () (interactive) "Eval buffer"
;;          (eval-current-buffer) (message "eval ok"))))
;;
;;  *wrong version*
;;
;;   (defun my-lisp-hook ()
;;     "My lisp setings"
;;     (local-set-key				;; <<<<
;;       "\M-e"
;;       '(lambda () (interactive) "Eval buffer"
;;          (eval-current-buffer) (message "eval ok"))))
;;
;; - In the latter case, turning on the tii<or any> minor mode would
;;   have caused losting your ESC-e keybinding.


;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tii-" in front of
;;   every function & variable. It stands for '(ti)ny (i)indent
;; - variable names contain letters 'tii-:', excecpt version/hook/mode vars.


;;}}}
;;{{{ History

;; CHANGE HISTORY
;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Sep  2	1995	[jari]		19.28	v1.12		Release_3
;; - Added ###autoload directives. Added tii-:bol, so that user can decide
;;   if he wants special BOLP handling or not.
;;
;; Aug  31	1995	[jari]		19.28	v1.11		NotReleased
;; - Thanks for Anders Lindgren, andersl@csd.uu.se (author of follow-mode)
;;   and his explanation about using minor modes right I was able to solve
;;   the hangup. I also added note about how the keybinding should
;;   be accoplished withing mode hooks.
;;
;; Aug  28	1995	[jari]		19.28	v1.10		NotReleased
;; - Total hangup with previous version, now I'm using standard minor
;;   map approach, but tii-mode keymap overrides definitions on the
;;   current map. Should investigate why ?
;; - Needs little adjustments for indenting from the BOL.
;;
;; Aug  28	1995	[jari]		19.28	v1.9		NotReleased
;; - Finally removed that horrible local-set-key "\t" .. hacks and started
;;   using 19.28 emacs minor modes. Now the Tab key behaves lot more
;;   expectedly.
;; - Made variables buffer local and removed buffer local
;;   variable create function. Much cleaner now.
;; - Cleaned the whole tii-relative function. Uses now event if possible,
;;   so that it won't choke to events as previously used read-char .
;;
;; Mar  22	1995	[jari]		19.28	v1.8		Release_2
;; - The modeline didn't show the "mode" , this was due to lacking
;;   local variable definitions.
;;
;; Feb  9	1995	[jari]		19.28	v1.2-1.7       	Release_1
;; - Development versions.
;;
;; Sep 18	1994	[jari]		18.57	v1.1		first code

;; To do, bugs
;; ============================================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.

;;}}}

;;; Code:

;;{{{ require

;;; ......................................................... &require ...


;; FSF user can use levents to read keypress, then it
;; won't break while read-char would...

(defvar tii-:use-events
  (if (and (not (boundp 'xemacs-logo))	;are we in FSF ?
	   (boundp 'emacs-minor-version)
	   (> emacs-minor-version 27))	;at least in 19.28 levents exist
      (progn
	(if (fboundp 'event-to-character)
	    nil				;loaded ok
	  (load-library "levents"))	;has no provide statement
	t)				;yes, we can use events
    nil)
  "*Determines if events can be used for reading keypress.")

;;}}}
;;{{{ setup: all

;;; .......................................................... &v-bind ...


(defun tii-mode-map-define-keys ()
  "Defines keybindings to tii-mode-map"
  (define-key  tii-mode-map "\t"        'tii-relative)

  ;;  eg. lisp-mode uses backward-delete-char-untabify which is
  ;;  uncomfortable in editing.
  (define-key  tii-mode-map [backspace] 'delete-backward-char)
  )


;;   `tii-mode-map' must be defined as a keymap
;;   otherwise won't the `define-key' statement be able
;;   to store new key in it.
;;
(defvar tii-mode-map nil
  "*Minor keymap, only for TAB change. Copy of current-local-map")

(if tii-mode-map
    nil
  (setq tii-mode-map  (make-sparse-keymap))
  (tii-mode-map-define-keys))

;;; ......................................................... &v-hooks ...

(defvar tii-mode-load-hook nil
  "*Hook run when file is loaded.")

(defvar tii-mode-hook nil
  "*Hook run when tii-mode turned on")



;;; .......................................................... &v-mode ...

(make-variable-buffer-local 'tii-mode)
(defvar tii-mode nil
  "If set, indicates that auto-indent mode is active.  This variable is
automatically set by invoking \\[tii-mode].")

(make-variable-buffer-local 'tii-tt-mode)
(defvar tii-tt-mode nil
  "*Hard tab submode.")


;;; Install the keymap,activated as soon as the variable `tii-mode' is set.
(or (assq 'tii-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'tii-mode tii-mode-map)
		minor-mode-map-alist)))


;;  Update minor-mode-alist
(or (assq 'tii-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tii-mode tii-:mode-str)
		minor-mode-alist)))



;;; ... private variables ................................. &v-private ...

(make-variable-buffer-local 'tii-:RET)
(defvar tii-:RET nil
  "Global return value used, when multiple values are neede.
Shouldn't interest regular user.")


(make-variable-buffer-local 'tii-cp)
(defvar tii-cp 0  "Internal. Current point")

(make-variable-buffer-local 'tii-cl)
(defvar tii-cl 0  "Internal. Current line")



;;; ... user configurable .................................. &v-config ...

;;; - The BOL is special, because when you write code, the crucial
;;;   point is line start: you decide indentation or cursor positioning with
;;;   that first keystroke.
(defvar tii-:bol t
  "*Flag that determines if beg. of line should be treated differently.")


(defvar tii-:special-re
  (concat
   "^[ \t]*\\(//\\|\#\\|!\\|REM\\)[ \t]*"
   "\\|^;;:?;?[ \t]*"    ; don't put ;;+, since someone may draw ;;;;;;;;;;...
   )
  "*Sometimes single indent isn't enough. For example it would be convenient
to write long C++ comments by hitting the TAB on the next line. Original
RE handles considers these as special cases.
!          .Xdefauls or X-related files
#          Perl, awk, shell
//         C++
;;;        Lisp
REM        Oracle Sqlplus, SQL files in general
")


(defvar tii-:mode-str-orig " Tii"
  "*String to be displayed in mode line.")

(defvar tii-:tt-mode-str-orig " TiiT"
  "*String to be displayed in mode line.")


;;  This is not user option
(make-variable-buffer-local ' tii-:mode-str)
(defvar tii-:mode-str tii-:mode-str-orig
  "Current minor mode status displayed. Changed dynamically.")

;;; .................. version notice ...................... &version ...

(defconst tii-version
  "$Revision: 1.12 $"
  "Latest version number only.")

(defconst tii-version-id
  "$Id: tinyindent.el,v 1.12 1995/09/02 10:32:14 jaalto Release_3 $"
  "Latest modification time and version number.")

(defconst tii-version-doc
  "tii.el -- auto-indent minor mode

First created: 7 Aug  87
Author       : Alan K. Stebbens   <aks@hub.ucsb.edu>
Maintainer   : Jari Aalto         <jari.aalto@ntc.nokia.com>
Modified by  : Jari Aalto

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tii-version ()
  "autoindent.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tii-version-doc
       "\ncurrent version:\n" tii-version-id)
      (pop-to-buffer  ob)
    ))


;;}}}

;;; ########################################################### &Funcs ###

;;{{{ code: misc



;;; ----------------------------------------------------------------------
;;;
(defmacro tii-modeline ()
  "Forces modeline update to happen."
  (set-buffer-modified-p (buffer-modified-p)))


;;; ----------------------------------------------------------------------
;;; The function 'make-local-vars was added by Darryl Okahata,
;;; for autoindent.el, ** not used currently, demonstration only
;;;
(defun tii-make-local-vars (&rest list)
  "Make every variable listed in LIST a variable local to a buffer.
LIST is of the form:
        (VAR1 VALUE1 VAR2 VALUE2 ...)
Each VARn is the name of the variable to make into a local variable, and
the corresponding VALUEn is the initial value to give to the local
variable.
"
  (let (var value)
    (while list
      (setq var (car list))
      (setq value (car (cdr list)))
      (make-local-variable var)
      (set var value)
      (setq list (cdr (cdr list)))
      )))


;;; ----------------------------------------------------------------------
;;; - The idea of reading events is originally from tinylibq.el
;;;
(defun tii-read-char ()
  "Reads input. Non character, eg. mouse event in considered as abort.

References:
  tii-:use-events
"
  (let* ((ev-loop t)
	 (table				;tab generates [tab], not char \t
	  '((tab        ?\t)
	    (backspace  8)
	    ))
	 (ignore-table
	  '(ignore mouse-movement))
	 ch
	 el
	 event
	 )
    (if (null tii-:use-events)
	(setq ch (read-char))
      ;; - By using event reading, we can accept any action and it doesn't
      ;;   break this function.
      ;; - moving the mouse generates couple of unwanted events, ignore
      ;;   them. It also removes message.
;;      (sit-for 0.5)                   ;to clear the buffer
      (while ev-loop
	(discard-input)
	(setq event (read-event))
	(if (and (listp event)
		 (memq (car event) ignore-table))
	    t				;run loop again
	  (setq ev-loop nil)))

      ;;  Now, check if this is special event first...
      (if (setq el (assoc event table))
	  (setq ch (nth 1 el))
	(setq ch (event-to-character event)))
      )
    ch
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tii-confirm (msg)
  "Confirms action. RET/SPC = ok. The real character pressed is available
thru global variable tii-:RET."
  (let* (ch)
    (message msg)
    (setq ch (tii-read-char)) (message "")
    (setq tii-:RET ch)
    (cond
     ((or (eq ch ?\015) (eq ch ?\040))	; RET/SPC
      t)
     (t
      nil))))

(defun tii-print-p (ch)
  "Determines if character can be printed normally.
Returns t if so."
  (if (and (integerp ch)
	   (or (memq ch '(?\t ?\n ?\r )) ;is it here, 9=tab
	       (and (> ch 31)		;or within this range ?
		    (< ch 127))))
      t
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tii-read-line (&optional pp)
  "Reads whole line from buffer"
  (save-excursion
    (if (not (null pp))
	(goto-char pp))
    (buffer-substring
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point) ))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tii-check-line (mode &optional arg)
  "Applies some commands to the line.

MODE controls the behaviour:
'prev     ,look at the previous line                  ARG=regexp
'next     ,look at the next line                      ARG=regexp
nbr       ,look at point, at line beginning           ARG=regexp
'prevc    'return last column position of previous line

"
  (let* (ret)
    (save-excursion
      (if (null (symbolp mode)) nil	;handle literals
	(cond
	 ((eq 'prev mode)
	  (forward-line -1) (setq ret (looking-at arg)))
	 ((eq 'next mode)
	  (forward-line 1)  (setq ret (looking-at arg)))
	 ((eq 'prevc mode)
	  (forward-line -1) (end-of-line)
	  (setq ret (current-column)))
	 ))

      (if (integerp mode)		;user gave POINT to look at
	  (progn
	  (goto-char mode) (beginning-of-line)
	  (setq ret (looking-at arg))))

      ret
      )))

;;}}}
;;{{{ engine

;;; .......................................................... &engine ...

;;; ----------------------------------------------------------------------
;;;
(defun tii-special-handle ()
  "Handle some special lines -- like gin-mode,  but simpler.
Supposes that point is at the beginning of investigated line.
Moves point 1 line forward af ter done.

Returns:
  filling pattern to use at front of line or nil
"
  ;;  Look for some special lines, like C++
  (let* (
	 (p	   (point))
	 (bp	   (save-excursion (beginning-of-line) (point)))
	 (s-re	   tii-:special-re)
	 fill
	 imode line
	 )
    (if (null (looking-at s-re))
	nil
      ;;  back to original line
;;      (d! (get-line) "#look spe" (looking-at s-re) s-re)
      (forward-line 1)		;otherwise visible to user whn Question
      (setq imode (tii-confirm "indent special?"))
      (if (null imode)
	  nil				;skip
	(setq line (tii-read-line (- (point) 1)))
	(string-match s-re line)
	(setq fill
	      (substring line (match-beginning 0) (match-end 0)))
	))
    fill
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tii-relative ()
  "Almost like indent-relative, but handles some special cases.
- if the above line if NOT empty, then we indent relatively automatically
- if above line IS empty, then ask if normal TAB/relative indent.

References:
  tii-:special-re
"
  (interactive)
  (let* (
	 (prev-empty 	(tii-check-line 'prev "[ \t]*$"))
	 (prev-col  	(tii-check-line 'prevc))
	 (tt-mode	tii-tt-mode)
	 (bolp-flag     tii-:bol)
	 (p		(point))
	 (cur-col	(current-column))
	 (imode	        t)
         (SPC		?\ )

	 bp ep				;BEG END point
	 fill
	 line
	 ch
	 skip
	 )

    (catch 'cancel
      (save-excursion
	(setq bp (save-excursion (beginning-of-line) (point)))
	(setq ep (save-excursion (end-of-line) (point)))

	;;  make sure these are NOT nil
	(if (null tii-cp) (setq tii-cp 0))
	(if (null tii-cl) (setq tii-cl 0))

	;;  Count lines has A BUG! , If I'm at the beg of line
        ;;  or 1 char forward it gives different values!
	;;
	(setq line (count-lines 1 p))
	(if (or (eq p bp) (eobp)) (setq line (1+ line))) ;BEG of line error

	;;   - the user has answered to question, we are on the same line
	;;   - if he is at the beginning, then ALWAYS ask (forced ask)
	(if prev-empty
	    (if (and			;already asked ?
		 (>= tii-cp bp)
		 (<= tii-cp ep)
		 )
		(setq skip 1))
	  (if (null (bolp)) (setq skip 2))          ;BOL ?
	  (if (< prev-col cur-col) (setq skip 3))   ;previous line is shorter
	  )

;;;  (d! skip "POINT" p  " " tii-cl line " bp ep  " bp ep tii-cp)

	(if skip  (throw 'cancel t))	;we were on this line already

	(setq tii-cl line)		;update line number
	(setq tii-cp p)			;current point position

	;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	;;  The real engine
	(setq tii-:RET nil)
	(cond
	 ((bobp)
	  (tab-to-tab-stop))
	 ((bolp)
	  (forward-line -1)		;Check previous line
	  (if (setq fill (tii-special-handle))
	      (progn
		(throw 'cancel t))
	    (forward-line 1)
	    (if tii-tt-mode		;skip if set
		nil
	      (if (null bolp-flag)
		  (setq imode t)
		(setq imode (tii-confirm "indent relative?")))
	      ))
	  (setq ch tii-:RET)		;this was pressed
	  ))
	))

    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch end ^^^
    (if fill
	(insert fill)			;see save-excursion, fill is set
      (cond
       (tii-tt-mode
	(setq ch (or ch (tii-read-char)))

	(cond
	 ((eq ch SPC)
	  (indent-relative))
	((eq ch ?\t)
	 (tab-to-tab-stop)
	 (setq ch nil)			;tab stop already does this
	 )
	(t
	 (indent-relative)))
	)
       (t
	(cond
	 (imode
	  (indent-relative)		;other char follows
	  (if (eq ch SPC)		;kill space, because it means YES
	      (setq ch nil))
	  )
	 (t
	  (tab-to-tab-stop)		;use hard tab
	  (setq ch nil)			;kill the TAB char
	  ))
	))

      ;; (d! imode ch (tii-print-p ch))
      ;;  the TAB char automatically moves to tab-to-tab-stop
      ;;  if it's inserted
      (if (and (tii-print-p ch)
	       (not (eq ch SPC)	))	;this is already handled
	  (insert ch))		;add the character, don't loose it

      )))

;;}}}
;;{{{ modes

;;; ........................................................... &modes ...


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tii-tt-mode (&optional arg)
  "Toggles tii-tt-mode
This isn't really mode. It just turns one flag on in tii-mode, so that
it behaves a little differently. If the tii-mode is not running, it
wiil be turned on. turning off tii-tt-mode _does_not_ end tii-mode !

Sometimes you want to control between 'hard' tab and 'soft' tab =
relative indent. This mode causes second character to be read after
tab key is hit. The following happens:

TAB TAB     ,inserts hard tab
TAB SPC     ,indent relative without inserting space char.
TAB x       ,indents relative and inserting character x
"
  (interactive "")
  (setq tii-tt-mode (if (or (zerop (prefix-numeric-value arg))
			    (and (null arg) tii-tt-mode))
			nil
		      t))
  (if tii-tt-mode
      (progn
	(if tii-mode nil		;is main mode off ?
	  (setq tii-tt-mode t)
	  (tii-mode)			;turn it on
	  )
	(setq tii-:mode-str tii-:tt-mode-str-orig)
	(if (interactive-p) (message "tii-tt-mode is t"))
	)
    (if (interactive-p) (message "tii-tt-mode is nil"))
    (setq tii-:mode-str tii-:mode-str-orig)
    )
  (tii-modeline)
  )


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tii-mode (&optional arg)
  "Enable, disable, or toggle tii-mode if ARG is nonzero, zero, or
nil.  Tii-mode works by invoking indent-relative for TAB.

Indentation is determined according to previous lines. Special
indent happens only at the beginning of line, where user is asked if
he wants to have relative or \"hard\" indentation.

See also tii-tt-mode.
"
  (interactive "P")
  (if (setq tii-mode (if (or (zerop (prefix-numeric-value arg))
			     (and (null arg) tii-mode))
			 nil
		       t))
      ;; .......................................................... on ...
      (progn
	(if tii-tt-mode
	    nil				;Hard tab mode already set
	  (setq tii-:mode-str tii-:mode-str-orig))
	(run-hooks 'tii-mode-hook)
	(if (interactive-p) (message "tii-mode is t"))
	)
    ;; ........................................................... off ...
    ;;  restore the values
    (setq tii-tt-mode nil)
    (if (interactive-p) (message "tii-mode is nil"))
    )
  (tii-modeline)
  )

;;}}}

(provide 'tinyindent)
(run-hooks 'tii-mode-load-hook)

;;; ...................................................... end of file ...
