;; @(#) tinyeat.el -- Eating blocks of text forward, backward

;; @(#) $Id: tinyeat.el,v 1.11 1995/09/28 08:30:13 jaalto Release_3 jaalto $
;; @(#) $Keywords: deleting text, killing text $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Documentation

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Apr 2 1995
;;
;; To get information on this program use ident(1) or do M-x tie-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el


;; LCD Archive Entry:
;; tinyeat|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Eating blocks of text forward, backward. No more ESC-d or or DEL/BACKSPACE|
;; 28-Sep-1995|1.11|~/misc/tinyeat.el.Z|

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
;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;	(require 'tinyeat)
;;
;; - Or use autoload, your emacs starts a bit up faster
;;
;;	(autoload 'tie-forward-preserve		"tinyeat" t t)
;;	(autoload 'tie-backward-preserve	"tinyeat" t t)
;;	(autoload 'tie-delete-paragraph		"tinyeat" t t)
;;      (autoload 'tie-kill-line		"tinyeat" t t)
;;	(autoload 'tie-kill-line-back		"tinyeat" t t)
;;	(autoload 'tie-kill-buffer-lines	"tinyeat" t t)
;;      (autoload 'tie-kill-buffer-lines-min	"tinyeat" t t)
;;
;; - To make use of these functions you should do couple of keybindings
;;   I use the following. The M- is same as ALT key in my HP-UX.
;;
;;	(global-set-key [C-backspace]     'tie-forward-preserve)
;;	(global-set-key [M-backspace]	  'tie-backward-preserve)
;;      (global-set-key [C-S-backspace]	  'tie-delete-paragraph)
;;      (global-set-key [M-S-backspace]	  'tie-kill-line-back)  ; Alt-shift
;;      (global-set-key [C-M-S-backspace] 'tie-kill-buffer-lines); Alt-sf-ctr
;;      (global-set-key [?\e backspace]	  'tie-kill-buffer-lines-min);
;;
;;	;;  - For mouse convenience, this doesn't put text into cut buffer
;;	;;    and it doesn't signal any errors...
;;      ;;    in HP-ux this means Alt-k
;;      (global-set-key [8388770]         'tie-kill-line)


;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o  Determines how much text should be eaten around current cursor
;;    position. Normally eats extra spaces, extra newlines, next word
;;    next statement , next comment ... whatever is appropriate
;; o  When you grow accustomed to this, it probably almost replaces your
;;    single BACKSPACE and DEL keys.

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tie-" in front of
;;   every function & variable. It stands for '(ti)ny (e)at
;; - variable names contain letters 'tie-:', excecpt version/hook/mode vars.

;; PREFACE
;; ========================================
;; - I got sick of moving cursor around the point and using DEL/BACKSPACE
;;   for example when writing C++ and LISP symbols. Say I have:
;;
;;	(defun lisp-symbol-name-myname          ()
;;                              *
;;
;;   And I decide I want to change that 'myname' to something else. Normally
;;   I'd have to reach out for ESC-d for kill-word to delete 'myname'.
;;   Then I'd type a new name:
;;
;;	(defun lisp-symbol-name-mynew           ()
;;				     *
;;
;;   Now I notice that there are extra spaces involved, I don't want those
;;   before the '()' marks for function name, I have to call fixup-whitespace
;;   for that ... damn it's not bound to any keys by default, so I have to
;;   type it the long way round: 'M-x fixup-whitespace'. now I'm beginning
;;   to think: "Yeah, Why I didn't bind it to keys...".
;;       next thing I notice that the 'mynew' wasn't good name after all,
;;   So I decide to delete 3 words backward. Now, how do I do that?
;;
;;	(defun lisp-symbol-name-mynew ()
;;				     *
;;
;;   Where is the command to delete backward ... (I start remembering)
;;   After spending valuable minutes to finding the delete-backward-word
;;   command: "Damn again, there is no such command". I end up tapping
;;   backspace until I reach the correction point:
;;
;;	(defun lisp- ()
;;	            *
;;   And start typing new name...
;;
;;	(defun lisp-my-func ()
;;
;;   Then I suddenly notice that there is too many newlines above my
;;   newly created function: "I really should delete those 5 extra ones".
;;   "Now, how do I kill backward ? The kill-line in C-k kills only forward"
;;   ...
;;
;; - As you can notice, we often spend most of our time to position the cursor
;;   in the right places and deleting text over there.. over here ..
;;   typing more .. changing our mind ... etc.
;; - I thought it was time to do something creative, so that I wouldn't
;;   have to worry about the deleting of text so much.
;; - This .el provides _smart_ deleting of text, whether you want to delete
;;   forward of backward. Naturally it isn't capable of miracles, it just
;;   does few guesses, and a guess may be wrong.
;;
;; - If you end up in a situation where you suddenly notice that vast
;;   area of text have just vanished from you buffer, remember, there
;;   is no need to panic. Just send a bug report to me, and hit UNDO
;;   and use normal DEL/BACKSPACE. Emacs is great, because it lets
;;   programs, like this, to do mistakes ... while allowing you to
;;   restore what it did.
;;
;; - I'd be surprised if you ever want to discard this .el when you have
;;   tried it. :-/

;;}}}
;;{{{ history

;; CHANGE HISTORY
;; ....................................................... &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Sep	28	1995    [jari]		19.28	v1.11		Release_3
;; - errors corrected: the macros EOLP,BOLP .. missed
;; - added tie-kill-line, which preserves mouse cut buffer.
;; - added interactive to tie-kill-buffer-lines-min
;;
;; Sep	28	1995    [jari]		19.28	v1.10		Release_2
;; - Small error in parameter kill-lines "arg" --> "back"
;;
;; Sep	26	1995    [jari]		19.28	v1.8-9		NotReleased
;; - Added more keybind/autoload statements to the installation
;; - Added missing "##autoload" for new funcs.
;;
;; Sep	26	1995    [jari]		19.28	v1.7		NotReleased
;; - Added trivial buffer text kill functions tie-kill-buffer-lines,
;;   tie-kill-buffer-lines-min
;;
;; Sep	26	1995    [jari]		19.28	v1.6		NotReleased
;; - Corrected little byte compile error. Also one missing ')' in func.
;;
;; Sep	26	1995    [jari]		19.28	v1.5		NotReleased
;; - Added tie-kill-line-back, which reverses the kill-line function.
;;   very handy!
;;
;; Sep	1	1995    [jari]		19.28	v1.4		NotReleased
;; - Little more fine tuning. Deleting multiple newlines is now more
;;   substle: N lines --> 1 sperating line --> Delete that 1 line
;;   The old version didn't keep the cursor at the same place all the time
;;   when killing newlines forward.
;; - Added "Line Cleared" message. Deleted 3 x \n testing from the main
;;   section, because 2 x \n section could do the same.
;; - Added ###autolad directives. Some functions didn't have 'tie-' prefix.
;;
;; Jun	5	1995    [jari]		19.28	v1.3		Release_2
;; - No changes, since this has been working like charm for me.
;; + Added one usefull eat function: current paragraph which is suggested to
;;   be put on C-S-backspace.
;;
;; Apr	2	1995    [jari]		19.28	v1.2		Release_1
;; - Finished all funcs.
;;
;; Apr	2	1995    [jari]		19.28	v1.1		NotReleased
;; - Moved my not-so-good-kill functions here and rewrote everything.

;; Thank you list:
;; nickson@cs.uq.oz.au (Ray Nickson)
;; - His little function tie-looking-back-at gave me an inspiration
;;   to develop this package.
;;
;; Advertise
;; - See tinymouse.el for optimal use of your mouse. Eg with mouse you can
;;   scroll the buffer lot easier than you thought... and there is function
;;   that really overwrites when you paste text with mouse [emacs can't
;;   do this by default, even when ovwrt-mode is on]
;; - Other "tiny tools" in the ~/misc directory of OHIO.

;; To do list:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.



;;}}}

;;; Code:

;;{{{ setup: variables

;;; ......................................................... &require ...

(require 'backquote)

;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar tie-:load-hook nil
  "Hook that is run when package is loaded.")

;;; ... private variables ................................. &v-private ...

;;; ... user configurable .................................. &v-config ...

(defvar tie-:verbose t
  "*Allows/disallows messages to be printed.")

;;}}}
;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tie-version
  "$Revision: 1.11 $"
  "Latest version number.")


(defconst tie-version-id
  "$Id: tinyeat.el,v 1.11 1995/09/28 08:30:13 jaalto Release_3 jaalto $"
  "Latest modification time and version number.")

(defconst tie-version-doc
  "tinyeat.el -- Eating blocks of text forward, backward

First created: Apr 2 1995
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tie-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tie-version-doc
       "\n\ncurrent version:\n" tie-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &Funcs ###


;;; ....................................................... &lib-funcs ...
;;; from tinylibm.el -- these will disappear when libs are gen. avail.


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



;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defmacro tie-space-p (ch)
  "Returns t is character CH is space or tab."
  (` (memq (, ch) '(?\t ?\ ))))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-backward ()
  "Eats backward. See tie-eat function."
  (interactive)
  (tie-eat t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-backward-preserve ()
  "Eats forward, but handles spaces differently. See tie-eat function."
  (interactive)
  (tie-eat t t))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-forward ()
  "Eats forward. See tie-eat function."
  (interactive)
  (tie-eat))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-forward-preserve ()
  "Eats forward, but hadles spaces differently. See EAT function."
  (interactive)
  (tie-eat nil t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-join-lines()
  "Join this and next line with possible space, and go to the joint"
  (interactive)
  (end-of-line
  (if (eobp) nil
    (kill-line)
    (fixup-whitespace))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-kill-line ()
  "This is same as kill-line, except the killed text isn't put
into cut buffer. This way you can retain mouse selection in cut buffer.

This inly interests people who can use mouse.
"
  (interactive)
  (if (eobp)
      nil				;skip
    (cond
     ((eolp)
      (delete-char 1))
     (t
      (delete-region (point) (EOLP))
      ))
    ))



;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-kill-line-back ()
  "reverses kill-line."
  (interactive)
  (cond
   ((and (not (bobp)) (bolp))
    (backward-delete-char 1)
    )
  (t
   (delete-region (point) (BOLP))
   ))
  )


;;; ----------------------------------------------------------------------
;;; Just to ease calling it from key
;;;
;;;###autoload
(defun tie-kill-buffer-lines-min (&optional back)
  "Kill till point-min"
  (interactive)
  (tie-kill-buffer-lines 'back))

;;; ----------------------------------------------------------------------
;;; very trivial, but I need this all the time
;;;
;;;###autoload
(defun tie-kill-buffer-lines (&optional back)
  "Kills to the point-max or to the point-min with ARG."
  (interactive)
  (cond
   (back
    (delete-region (point) (point-min)))
  (t
   (delete-region (point) (point-max)))
  ))



;;}}}
;;{{{ misc2

;;; ----------------------------------------------------------------------
;;; ** not used right now, dementration only
;;;
;;; - I found this little function very long time ago and it's
;;;   very usefull. the bad thing is that it is _IMPOSSIBLE_SLOW.
;;; - In my first tries I used it very often, but because of the speed
;;;   problem, I started to use CHARACTER based looking: next and
;;;   previous for determining what to do. (see the numerous memq)
;;; - This is included here so that someone may use it if needed.
;;;
;;; Date:	22 Sep 94
;;; Newsgroups: gnu.emacs.help
;;; Subject:	Re: looking-at backwards?
;;; Author:	nickson@cs.uq.oz.au (Ray Nickson)
;;;
(defun tie-looking-back-at (regexp)
  "Return t if text immediately before point matches REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.
"
  (let ((found) (here (point)))
    (while (and (not found)
                (re-search-backward regexp nil t))
      (setq found (= (match-end 0) here)))
    (goto-char here)
    found))





;;; ----------------------------------------------------------------------
;;; This extremely handy deletion function, I kill C++ code with this
;;; all the time
;;;
;;;###autoload
(defun  tie-delete-paragraph ()
  "Deletes current paragraph, separated by empty lines. "
  (interactive "*")
  (let* (beg
	 end
	 (re "^[ \t]*$")
	 )
    (save-excursion
      (if (null (re-search-backward re nil t)) nil
	(beginning-of-line)
	(forward-line 1)		;exlude space
	(setq beg (point))
	))
    (save-excursion
      (if (null (re-search-forward re nil t)) nil
	(beginning-of-line)
	(setq end (point))
	))

    (if (not (and beg end))
	(message "Can't find paragraph bounds.")
      (kill-region beg end))
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tie-char (&optional direction distance)
  "Reads character towards the direction from current point:
nil = forward, non-nil backward. DISTANCE 0/nil means reading from
current position.

Returns:
  nbr	,read char value
  nil	,if the position is not within point-min-marker and
         point-max-marker.
"
  (let* (
	 (beg  (point-min-marker))
	 (end  (point-max-marker))
	 (pos  (or distance 0))
	 (dest (if direction (- (point) (1+ pos)) (+ (point) pos)))
	 (read (if (or (< dest beg) (> dest end)) nil t))
	 )
    (if (null read) nil			;allowed to read ?
      (char-after dest))
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tie-space-del-at-point (&optional back preserve)
  "Delete whitespace at point. If optional PRESERVE is given, then
deletes towards the BACK only. if BACK is non-nil the deletion
is headed backward.

Input:
  ch-p          ,previous char
  ch            ,current char
  ch-n          ,next char
"
  (let* (
	 (charf	  (if back 'skip-chars-backward 'skip-chars-forward))
	 (p	  (point))
	 (verb	  tie-:verbose)
	 (ch	  (tie-char back 0))		;sitting on it if looking fwd
	 (ch-p	  (tie-char back -1))
	 (ch-n	  (tie-char back 1))
	 )
    (cond
     ((and back (tie-space-p ch-p) (eq ch ?\n))
      (delete-horizontal-space)
      (if verb (message "*line cleared"))
      t)
     ((eq ch ?\n)			;no spaces before, do nothing
      nil)
     ((or (and (tie-space-p ch) (tie-space-p ch-n)) ;at least two spaces
	  (and (tie-space-p ch-p) (tie-space-p ch)))
      (if (null preserve)
	  (fixup-whitespace)
	(funcall charf " \t")
	(delete-region p (point)))
      t)
     (t
      (delete-horizontal-space)
      t))
  ))


;;}}}
;;{{{ engine

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tie-eat (&optional back space-preserve)
  "Eats *appropriate* text forward, if BACK, then backward.

The optional SPACE-PRESERVE changes the space eating:

A.  when it is NIL and BACK is anything.   * marks the cursor.
         text1 text1        *     text2  text2
    -->  text1 text1 text2  text2                   ;one space left

B.  when it is NON-NIL and BACK nil
         text1 text1        *     text2  text2
    -->  text1 text1        *text2  text2            ;dels right spaces

C.  when it is NON-NIL and BACK t
         text1 text1        *     text2  text2
         text1 text1*     text2  text2               ;dels left spaces
"
  (interactive)
  (let (
	(p	 (point))
	(syntaxf (if back 'skip-syntax-backward 'skip-syntax-forward))
	(charf   (if back 'skip-chars-backward 'skip-chars-forward))
	(verb    tie-:verbose)
	ch ch-n ch-p ch-nn
	)
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
    (setq ch (tie-char back 0))		;sitting on it if looking fwd
    (setq ch-p  (tie-char back -1))	;previous
    (setq ch-n (tie-char back 1))	;next
    (setq ch-nn (tie-char back 2))	;following next

;;d     (d!
;;d      (char-to-string ch-p )
;;d      (char-to-string ch )
;;d      (char-to-string ch-n )
;;d      (char-to-string ch-nn))


    (cond
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; BEG of buffer or END of buffer
     ((eq nil ch)
      (if verb
	  (message
	   (concat (if (bobp) "Beginning" "End") " of buffer")))
      )

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ((tie-space-p ch)			;one whitespace
      (tie-space-del-at-point back space-preserve)
      (if (and verb (looking-at "$"))		;it handled this
	  (message "Line cleared"))
      )

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - Multiple  newlines, squeeze to one only
     ((and (eq ch ?\n) (eq ch-n ?\n))
      (funcall charf "\n")
      (if (null back)
	  (backward-char 1)		;do not join, leave 1 EMPTY newline
	(forward-char 1))
;;;      (d! "multi n" p (point))
      (delete-region p (point))
      )

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - at the end of line I suppose add previous line to it.
     ((eq ch ?\n)
;;;      (d! "1 n" space-preserve)
      (if (tie-space-del-at-point back space-preserve)
	  nil
	(if (null back)			;which direction
	    (delete-char 1)
	  (if (not (eq 0 (funcall syntaxf  "\\_")))	;try to move
	      (delete-region p (point))	;moved!
	    (backward-char 1)
	    (delete-region p (point))))
	))

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; WORD handling (blocks)
     (t					;eat next word
      (funcall syntaxf "\\s ")		;ignore spaces
;;;      (d! "t , syntax " (char-to-string (char-syntax ch)))
      ;;   - What is next char after whitespace ??
      ;;   - With these following conditionals we set the point
      ;;     to appropriate position and after COND we run the kill command
      (cond
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       ((equal ?w (char-syntax ch))
;;;     (d! "A word")
        (funcall syntaxf "\\sw")
	)
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       ((and (memq ch   '(?- ?_ ?:))
	     (memq (char-syntax ch-n)  '(?w ?\ ))
	     )
	;;  This is really har to understand... execpt for  the author
	;;  1) Is CH variable start, delimiter ?
	;;  2) AND is the NEXT-CH word or whitespace
	(funcall syntaxf  "\\_")
	(funcall syntaxf  "\\sw")
;;;	(d! "A sym")
	)
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       (t
;;;	(d! "A other")
	;; punctuation, comment, the rest ... skip non important stuff
	(funcall charf "^ \t\na-zA-Z0-9")
	))
      (delete-region p (point))
      ))
    ))

;;}}}


(provide 'tinyeat)
(run-hooks 'tie-:load-hook)

;;; ................ end of tinyeat.el ...................................
