;; @(#) tinypatch.el --  Emacs 19.28 std distrib. corrections

;; @(#) $Id: tinypatch.el,v 1.8 1995/06/08 06:08:52 jaalto Release_2 jaalto $
;; @(#) $Keywords: emacs distribution, corrections $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $packageinstallre: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      May 12 1995
;;
;; To get information on this program use ident(1) or do M-x tipa-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el


;; LCD Archive Entry:
;; tinypatch|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Emacs 19.28 std distrib. corrections |
;; 08-Jun-1995|1.8|~/misc/tinypatch.el.Z|

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

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp load path, add following into your
;;  ~/.emacs startup file, The patches take effect _IMMEDIATELY_
;;
;;     (require 'tinypatch)
;;
;;
;;  This package will handle also autoloaded functions as well as
;;  already loaded functions right. Patched function will have
;;  text 'Patched.' in their documentation string.
;;
;;  If you wish to restore the default functions use command:
;;
;;	(tipa-unpatch)

;;}}}
;;{{{ Documentation

;;; Commentary:

;; ..................................................... &t-commentary ...
;;; Briefly:
;; o   Enchancements, or minor corrections to emacs 19.28 distribution.


;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tipa-" in front of
;;   every function & variable. It stands for '(ti)ny (pa)tches
;; - variable names contain letters 'tipa-:', except version vars.

;; PREFACE
;; ========================================
;; - As a freetime programmer I use emacs extensively for all of my
;;   tasks I can imagine. After all it stays at front of me 10h per
;;   day at work, sometimes more when I'm going NET.
;; - I developed several packages, and felt I needed the
;;   debug-on-error permanently turned on, so that I could correct any
;;   mistakes or troubles I had with my .el files or with someone
;;   else's packet. Let's say, I felt safe, when it was turned on.
;; - But, [a big but], while I was playing with the emacs, it screwed my
;;   view time to time to ridiculous errors like:
;;
;;	Signalling: (beginning-of-buffer)
;;      --     when PgUp was pushed dooown..
;;
;;      Signalling: (end-of-buffer)
;;      --     now I used PgDoooown.
;;
;;      Signalling: (error "No dynamic expansion "mumbleGumble" ...
;;      --     I pressed ESC / to get expansion...
;; 	--     Seriously, why do you throw error about that?
;;
;;      Signalling: (error "Non-character input-event")
;;      --     This is what I get all the time, I started some
;;             command and clicked mouse...
;;      --     especially 'ispell-region' and 'click' is annoying
;;	--     This is caused by using read-char instead of read-event cmd
;;
;;      Dragging mouse upward
;;      --     The event didn't move the point, while downward drag moved,
;;             well now it does...
;;
;; - I didn't want to continuously turn ON/OFF the debug, since I
;;   couldn't know when the 'real error' hit and make my .el sauce. so
;;   I fixed these things by copying the functions from the emacs
;;   distribution and making the appropriate changes.
;;
;; - My guidelines were that:
;;   o  It is not appropriate to call 'error' command for wrong
;;      reasons. Eg. to get out of function and terminate the task,
;;      _when_ the condition happened was _not_ fatal. After all, why
;;      they call error to give 'informational messages' , like beginning
;;      of buffer, end of buffer ?
;;   o  Shouldn't the commands support the mouse too? If they didn't,
;;      at least what they could do is to ignore them, not to choke
;;	completely [like in ispell].
;;
;; - I sent several suggestions and patches, but RMS felt these
;;   changes I was proposing were so minor that he said:
;;
;;        "It seems like a marginal issue,..."
;;	  "Sorry, I can't spend time on this.  It doesn't seem essential..."
;;        "I think it is better if I spend the time fixing problems that are
;;         clearly in need of fixing..."
;;
;;   Okay, I understood, he was responsible for much more that just
;;   fixing some minor things in emacs .el files. When they work [for
;;   most of the poeple], why change them ?
;;
;; - That is the reason to release these 'patches', I sure would have
;;   hoped, that these things would be corrected to 19.29
;;   distribution, since the changes are  _so_ minor. Now I have to
;;   keep extra copies of functions in this separate file. I hope
;;   someone in the GNU finds time to think about these changes, from
;;   the developers point of view  [who have debug-on-error turned on
;;   most of the time]
;;
;; - Emacs is the only beaty I can dream of when it comes to
;;   programming, but why not make it a little bit more user
;;   friendly... and not letting it hit us so hard with those false alarms...
;;
;;   May 12 1995 "Jari Aalto" <jaalto@tre.tele.nokia.fi> using FSF 19.28

;;   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

;; About patches
;; ========================================
;; - In front of every function is explained what the patch corrects or
;;   how does it change the original behaviour. Propably you don't
;;   even realize that the function is changed, until you go back to
;;   old one with debug-on-error t ...
;; - These patches are ment to be _little_corrections_ only, not total
;;   rewrite of funcs or so.
;; - If you find something similar that should be corrected, let me
;;   hear about it. I only use one side of the emacs, so I propably
;;   have corrected only those things that I use frequently.
;; - Your name and patch description will be included of cource.
;;
;; How it works ?
;; ========================================
;; - Emacs has variable after-load-alist, which activates whenever
;;   file beeing loaded is found from there. Autoload functions cause
;;   library to be loaded, so we have a chance to override the
;;   functions by loading our patches immediately after the functions
;;   get defined.
;;
;; - This file hooks itself to that list and after the original library
;;   has been loaded it loads this file again and appropriate
;;   function(s) are replaced with patched ones.
;; - If the function is already in memory, loading this file will
;;   immediately replace any unpatched function.
;;
;; - The unload function does nothing else, than removing the forms
;;   from after-load-alist that cause this file to be loaded.
;;   Then it loads original files again, so that patched functions are
;;   wiped out.
;;
;; Which functions are patched ?
;; ========================================
;; - You can check in emacs which functions are patched by
;;   looking at the variables patched-LIBRARY, where library is some
;;   emacs .el file. The variable holds all functions that are patched
;;   for that library.
;;
;; - There is also function tipa-patched-p for programmers, which
;;   returns t if the function is patched.
;;
;; What about 'advice.el'
;; ========================================
;; - Frankly, I don't know what happens if you have some of these functions
;;   adviced when they get patched. Propably the advice goes away
;;   immediately, because the old function is overwritten.
;; - Best solution is to keep 'advice' commands in a separate file,
;;   which you can load as many times as you wish (if they 'drop off')


;;}}}
;;{{{ History

;; CHANGE HISTORY
;; ........................................................ &t-history ...
;; Jun	8	1995	19.28   v1.8		[jaalto] 	Release_2
;; - Added completion.el, mouse.el  patches.
;;
;; May	12	1995	19.28   v1.3-1.7	[jaalto] 	NotReleased
;; - Byte compiler warnings corrected, now compiles nicely.
;; - Corrected form variables: because of copying the variable that got
;;   defined was 'patched-dabbrev' , when it should have been
;;   patched-LIBRARY --> The patches didn't take effect properly.
;; - overload allow function had extra test for variable definition,
;;   removed, because a) if funtion was in memory b) the variable never
;;   got defined, unless the file were loaded again.
;;
;; May	12	1995	19.28   v1.2		[jaalto] 	Release_1
;; - Made complete package after testing.
;;
;; May	12	1995	19.28   v1.1		[jaalto] 	- started


;; To do list:
;; ========================================
;; - Add more patches :-/

;;}}}


;;; Code:

;;; .......................................................... &v-bind ...

;;{{{ private

;;; ... private variables ................................. &v-private ...


(defconst  tipa-:forms-ispell
      '(progn
	 (defvar patched-ispell "ispell-command-loop")
	 (load "tinypatch")))
(eval-after-load "ispell"  tipa-:forms-ispell)

(defconst  tipa-:forms-dabbrev
      '(progn
	 (defvar patched-dabbrev "dabbrev-expand")
	 (load "tinypatch")))
(eval-after-load "dabbrev"  tipa-:forms-dabbrev)

;;  Propably loaded already, but this doesn't hurt
(defconst  tipa-:forms-files
      '(progn
	 (defvar patched-files "basic-save-buffer-1")
	 (load "tinypatch")))
(eval-after-load "files"  tipa-:forms-files)



(defconst  tipa-:forms-mouse
      '(progn
	 (defvar patched-mouse "mouse-drag-region")
	 (load "tinypatch")))
(eval-after-load "mouse"  tipa-:forms-mouse)


;;  Propably loaded already, but this doesn't hurt
(defconst  tipa-:forms-simple
      '(progn
	 (defvar patched-simple "next-history-element")
	 (load "tinypatch")))
(eval-after-load "simple"  tipa-:forms-simple)


(defconst  tipa-:forms-comletion
      '(progn
	 (defvar patched-completion
	   (concat "check-completion-length "
		   "add-completion "
		   "kill-completionext-history-element"
		   ))
	 (load "tinypatch")))
(eval-after-load "completion"  tipa-:forms-comletion)

;;}}}

;;; ... user configurable .................................. &v-config ...

;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tipa-version
  "$Revision: 1.8 $"
  "Latest modification time and version number.")


(defconst tipa-version-id
  "$Id: tinypatch.el,v 1.8 1995/06/08 06:08:52 jaalto Release_2 jaalto $"
  "Latest modification time and version number.")

(defconst tipa-version-doc
  "tinymouse.el -- Mouse surfing. Powerful mouse functions!

First created: May 12 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tipa-version ()
  "tinycom.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tipa-version-doc
       "\n\ncurrent version:\n" tipa-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ misc

(defmacro tipa-autoload-p (fun)
  "Tests if function is in its autoload form."
  (list
   'and
   (list 'fboundp fun)
   (list 'listp fun)
   (list 'eq ''autoload (list 'car (list 'symbol-function fun)))))

;;; ----------------------------------------------------------------------
;;;
(defun tipa-patched-p (func)
  "Checks if fuction is patched. Arg must be SYMBOL."
  (and (fboundp func)
       (string-match
	"Patched"
	(or (documentation func) ""))))

;;; ----------------------------------------------------------------------
;;;
(defun tipa-overload-allow (func func-patch-var)
  "Determines if this function should be overloaded NOW."
  (and (fboundp func)
       (not (tipa-autoload-p func))	;not in autoload state ?
;;;       (boundp func-patch-var)		;it's loaded
       (null (tipa-patched-p func))	;already patched...
       ))

;;}}}

;;; ----------------------------------------------------------------------
;;;
(defun tipa-patch-1 (&optional restore)
  "lowlevel patch control. If called with arg UNpatches all funcs."
  (cond
   (restore
    (tipa-cleanup  tipa-:forms-ispell)  (load "ispell")
    (tipa-cleanup  tipa-:forms-dabbrev) (load "dabbrev")
    (tipa-cleanup  tipa-:forms-files)   (load "files")
    (tipa-cleanup  tipa-:forms-simple)  (load "simple")
    (tipa-cleanup  tipa-:forms-mouse)   (load "mouse")
    (tipa-cleanup  tipa-:forms-completion)  (load "completion")
    )
   (t
    (load "tinypatch")			;puts the forms back
    )))

;;; ----------------------------------------------------------------------
;;;
(defun tipa-cleanup (ELT)
  "Removes load form ELT from after-load-alist."
  (let* (ptr
	 fn
	 forms
	 frm
	 el
	 prev
	 base
	 )
    (setq ptr after-load-alist)

    (while ptr
      (setq el (car ptr)  fn (car el) forms (cdr el))
;;;      (d! ">>" fn (prin1-to-string forms))
      (setq prev forms base forms)	;here is the FORMS list

      (while forms
	(setq frm (car forms))
;;;	(d! fn (prin1-to-string frm)
;;;	    (equal frm ELT)
;;;	    (equal base forms))


	;; change form to nil
	(if (equal frm ELT)
	    (setcar forms nil))

	;;  This method removes permanently
;;;@	(if (equal frm ELT)
;;;@	    (if (equal base forms)
;;;@		(setcdr el  (cdr forms))     ;remove first element
;;;@	      (setcdr  prev (cdr forms))))   ;nay other element
	(setq prev forms)
	(setq forms (cdr forms))
	)
      (setq ptr (cdr ptr))
      )
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tipa-unpatch ()
  "Will discard any paches, and restore default functions."
  (interactive)
  (tipa-patch-1 t))

(defun tipa-patch ()
  "Will patch the functions again."
  (interactive)
  (tipa-patch-1 nil))




;;; ......................................................... &patches ...



;;{{{ ispell.el

;;; .......................................................... &ispell ...
;;; FSF 19.28 std distribution
;;; - If you accidentally happen to touch mouse, this breaks,
;;;   because it uses read-char command --> fix it GNU ISPELL version 4.
;;;
;;; - If you load file "levents.el" you can now use mouse with ispell
;;;    Mouse-2                  ,No
;;;    C-mouse2 and mouse-3     ,Abort

(tipa-overload-allow 'ispell-command-loop 'patched-ispell)



(if (tipa-overload-allow 'ispell-command-loop 'patched-ispell)
    (progn
(defun ispell-command-loop (word start end message)
  "Patched."
  (let ((flag t)
        (rescan nil)
        first-line)
    (if (null message)
        (setq first-line (concat "No near misses for '" word "'"))
      (setq first-line (concat "Near misses for '" word "'")))
    (ispell-highlight start end)
    (while flag
      (ispell-show-choices word message first-line)
      (message "Ispell command: ")
      (undo-boundary)
      (let (c                           ;was (downcase (read-char)))
	    event
	    (event-ch-ignore ?Z)	;pass thru command, mouse movement ...
	    ev-ch
            replacement
	    (EVENT (fboundp 'event-to-character)) ;can events be used ?
	    )
	(if (not EVENT)
	    (setq c (downcase (read-char)))
	  (discard-input)
	  (setq event (read-event))

	  ;;  Mouse generates lists...
	 (setq ev-ch
	       (if (not (listp event))
		   nil			;regular keypress
		 ;; Handle mouse
		 (sit-for 0.1)		;slow down a bit!
		 (discard-input)
		 (setq ev-ch (car event))))

	  (cond
	   ((memq ev-ch '(down-mouse-2 mouse-2)) ;for NO
	    (setq c ?\ ))
	   ((or (memq ev-ch '(down-mouse-3 mouse-3)) ;for ABORT
		(memq ev-ch '(C-down-mouse-2 mouse-3))) ; in case 3 not exist
	    (setq c ?q))
	   (t
	    (setq c (or (event-to-character event) event-ch-ignore))))
	  )

        (cond ((and (>= c ?0)
                    (<= c ?9)
                    (setq replacement (nth (- c ?0) message)))
               (ispell-replace start end replacement)
               (setq flag nil))
              ((= c ?q)
               (throw 'ispell-quit nil))
              ((= c (nth 3 (current-input-mode)))
               (keyboard-quit))
              ((= c ? )
               (setq flag nil))
              ((= c ?r)
               (ispell-replace start end (read-string "Replacement: "))
               (setq rescan t)
               (setq flag nil))
              ((= c ?i)
               (ispell-insert word)
               (setq flag nil))
              ((= c ?a)
               (ispell-accept word)
               (setq flag nil))
              ((= c ?l)
               (let ((val (ispell-do-look word)))
                 (setq first-line (car val))
                 (setq message (cdr val))))
              ((= c ??)
               (message
                "Type 'C-h d ispell' to the emacs main loop for more help")
               (sit-for 2))
	      ((= c event-ch-ignore) nil)
              (t
               (message "Bad ispell command")
               (sit-for 2)))))
    rescan)
  (sit-for 0.4)
  (discard-input)			;mouse-2 paste! No!
  )
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))

;;}}}
;;{{{ dabbrev.el

;;; ......................................................... &dabbrev ...
;;; FSF 19.28 std distribution
;;; dabbrev.el
;;; - I hate when I have debug on error ON , and it says "No dynamic abbrevs"
;;;   --> that is _not_ error condition
;;;
;;; - Man.. , this function should have been much shorter..
;;;


(if (tipa-overload-allow 'dabbrev-expand 'patched-dabbrev)
    (progn
(defun dabbrev-expand (arg)
  "Patched. Expand previous word \"dynamically\".
Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are considered.

If `case-fold-search' and `case-replace' are non-nil (usually true)
then the substituted word may be case-adjusted to match the abbreviation
that you had typed.  This takes place if the substituted word, as found,
is all lower case, or if it is at the beginning of a sentence and only
its first letter was upper case.

A positive prefix arg N says to take the Nth backward DISTINCT
possibility.  A negative argument says search forward.  The variable
`dabbrev-backward-only' may be used to limit the direction of search to
backward if set non-nil.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried."
  (interactive "*P")
  (let (abbrev expansion old which loc n pattern
        (do-case (and case-fold-search case-replace))
	first
	)
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    ;; loc -- place where expansion is found
    ;;    (to start search there for next expansion if requested later)
    ;; do-case -- non-nil if should transform case when substituting.
    (catch 'giveup
      (save-excursion
	(if (and (null arg)
		 (eq last-command this-command)
		 last-dabbrevs-abbrev-location)
	    (progn
	      (setq abbrev last-dabbrevs-abbreviation)
	      (setq old last-dabbrevs-expansion)
	      (setq which last-dabbrevs-direction))
	  (setq which (if (null arg)
			  (if dabbrevs-backward-only 1 0)
                        (prefix-numeric-value arg)))
	  (setq loc (point))
	  (forward-word -1)
	  (setq last-dabbrevs-abbrev-location (point)) ; Original location.
	  (setq abbrev (buffer-substring (point) loc))
	  (setq old abbrev)
	  (setq last-dabbrevs-expansion-location nil)
	  (setq last-dabbrev-table nil))          ; Clear table of things seen.

	(setq pattern (concat "\\b" (regexp-quote abbrev) "\\(\\sw\\|\\s_\\)+"))
	;; Try looking backward unless inhibited.
	(if (>= which 0)
          (progn
            (setq n (max 1 which))
            (if last-dabbrevs-expansion-location
                (goto-char last-dabbrevs-expansion-location))
            (while (and (> n 0)
                        (setq expansion (dabbrevs-search pattern t do-case)))
              (setq loc (point-marker))
              (setq last-dabbrev-table (cons expansion last-dabbrev-table))
              (setq n (1- n)))
            (or expansion
                (setq last-dabbrevs-expansion-location nil))
            (setq last-dabbrevs-direction (min 1 which))))

      (if (and (<= which 0) (not expansion)) ; Then look forward.
          (progn
            (setq n (max 1 (- which)))
            (if last-dabbrevs-expansion-location
                (goto-char last-dabbrevs-expansion-location))
            (while (and (> n 0)
                        (setq expansion (dabbrevs-search pattern nil do-case)))
              (setq loc (point-marker))
              (setq last-dabbrev-table (cons expansion last-dabbrev-table))
              (setq n (1- n)))
            (setq last-dabbrevs-direction -1))))

    (if expansion nil
      (setq first (string= abbrev old))
      (setq last-dabbrevs-abbrev-location nil)
      (if (not first)
	  (progn (undo-boundary)
		 (search-backward old)
		 (if (eq major-mode 'picture-mode)
		     (picture-replace-match abbrev t 'literal)
		   (replace-match abbrev t 'literal))))


      (message
       (format
	(if first
	    "No dynamic expansion for \"%s\" found."
	  "No further dynamic expansions for \"%s\" found.")
	abbrev))
      (throw 'giveup t)
      )

    (if (not expansion) nil
      ;; Success: stick it in and return.
      (undo-boundary)
      (search-backward old)
      ;; Make case of replacement conform to case of abbreviation
      ;; provided (1) that kind of thing is enabled in this buffer
      ;; and (2) the replacement itself is all lower case.
      ;; First put back the original abbreviation with its original
      ;; case pattern.
      (save-excursion
        (if (eq major-mode 'picture-mode)
            (picture-replace-match abbrev t 'literal)
          (replace-match abbrev t 'literal)))
      (search-forward abbrev)
      (let ((do-case (and do-case
                          (string= (substring expansion 1)
                                   (downcase (substring expansion 1))))))
        ;; First put back the original abbreviation with its original
        ;; case pattern.
        (save-excursion
          (replace-match abbrev t 'literal))
;;; This used to be necessary, but no longer,
;;; because now point is preserved correctly above.
;;;     (search-forward abbrev)
        (if (eq major-mode 'picture-mode)
            (picture-replace-match (if do-case (downcase expansion) expansion)
                                   (not do-case)
                                   'literal)
          (replace-match (if do-case (downcase expansion) expansion)
                         (not do-case)
                         'literal)))
      ;; Save state for re-expand.
      (setq last-dabbrevs-abbreviation abbrev)
      (setq last-dabbrevs-expansion expansion)
      (setq last-dabbrevs-expansion-location loc)
      )

    )))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))


;;}}}
;;{{{ simple.el

;;; .......................................................... &simple ...
;;; FSF 19.28 std distribution simple.el
;;; - I don't want to see minibuffer end as "error" , it screws up
;;;   my view.


(if (tipa-overload-allow 'next-history-element 'patched-simple)
    (progn
(defun next-history-element (n)
  "Patched. Insert the next element of the minibuffer history into the
minibuffer."
  (interactive "p")
  (let ((narg (min (max 1 (- minibuffer-history-position n))
                   (length (symbol-value minibuffer-history-variable)))))
    (if (= minibuffer-history-position narg)
	(progn
	  (setq error-cond
		(if (= minibuffer-history-position 1)
		    "End of history; no next item"
		  "Beginning of history; no preceding item"))
	  (message error-cond)
	  )
      (erase-buffer)
      (setq minibuffer-history-position narg)
      (let ((elt (nth (1- minibuffer-history-position)
                      (symbol-value minibuffer-history-variable))))
        (insert
         (if minibuffer-history-sexp-flag
             (prin1-to-string elt)
           elt)))
      (goto-char (point-min)))))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))
;;}}}
;;{{{ files.el

;;; ........................................................... &files ...
;;;  FSF 19.28 std distribution simple.el
;;;  I hate , when Error is displayed while I was cancelling the job
;;;  with "n", "I don't want to overwrite"


(if (tipa-overload-allow 'basic-save-buffer-1 'patched-files)
    (progn
;; This does the "real job" of writing a buffer into its visited file
;; and making a backup file.  This is what is normally done
;; but inhibited if one of write-file-hooks returns non-nil.
;; It returns a value to store in setmodes.
(defun basic-save-buffer-1 ()
  "Patched."
  (catch 'quit
    (let (tempsetmodes setmodes)
      (if (not (file-writable-p buffer-file-name))
        (let ((dir (file-name-directory buffer-file-name)))
          (if (not (file-directory-p dir))
              (error "%s is not a directory" dir)
            (if (not (file-exists-p buffer-file-name))
                (error "Directory %s write-protected" dir)
              (if (yes-or-no-p
                   (format "File %s is write-protected; try to save anyway? "
                           (file-name-nondirectory
                            buffer-file-name)))
                  (setq tempsetmodes t)
                (message "Attempt to save to a file ")
		(throw 'quit t)
		)))))
    (or buffer-backed-up
        (setq setmodes (backup-buffer)))
    (let ((dir (file-name-directory buffer-file-name)))
      (if (and file-precious-flag
               (file-writable-p dir))
          ;; If file is precious, write temp name, then rename it.
          ;; This requires write access to the containing dir,
          ;; which is why we don't try it if we don't have that access.
          (let ((realname buffer-file-name)
                tempname temp nogood i succeed)
            (setq i 0)
            (setq nogood t)
            ;; Find the temporary name to write under.
            (while nogood
              (setq tempname (format "%s#tmp#%d" dir i))
              (setq nogood (file-exists-p tempname))
              (setq i (1+ i)))
            (unwind-protect
                (progn (clear-visited-file-modtime)
                       (write-region (point-min) (point-max)
                                     tempname nil realname)
                       (setq succeed t))
              ;; If writing the temp file fails,
              ;; delete the temp file.
              (or succeed (delete-file tempname)))
            ;; Since we have created an entirely new file
            ;; and renamed it, make sure it gets the
            ;; right permission bits set.
            (setq setmodes (file-modes buffer-file-name))
            ;; We succeeded in writing the temp file,
            ;; so rename it.
            (rename-file tempname buffer-file-name t))
        ;; If file not writable, see if we can make it writable
        ;; temporarily while we write it.
	;; But no need to do so if we have just backed it up
        ;; (setmodes is set) because that says we're superseding.
        (cond ((and tempsetmodes (not setmodes))
               ;; Change the mode back, after writing.
               (setq setmodes (file-modes buffer-file-name))
               (set-file-modes buffer-file-name 511)))
        (write-region (point-min) (point-max)
                      buffer-file-name nil t)))
    setmodes)))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))


;;}}}
;;{{{ completion.el

;;; ...................................................... &completion ...
;;; - I don't want to see errors when the completion string is too short,
;;;   messag is sufficent


(if (tipa-overload-allow 'check-completion-length 'patched-completion)
    (progn
(defun check-completion-length (string)
  "Patched."
  (if (< (length string) completion-min-length)
      (progn
	(message "The string \"%s\" is too short to be saved as a completion."
		 string)
	nil)
      (list string)))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))

(if (tipa-overload-allow 'add-completion 'patched-completion)
    (progn
(defun add-completion (string &optional num-uses last-use-time)
  "Patched. Add STRING to completion list, or move it to head of list.
The completion is altered appropriately if num-uses and/or last-use-time is
specified."
  (interactive (interactive-completion-string-reader "Completion to add"))
  (if (null (check-completion-length string))
      nil
    (let* ((current-completion-source (if (interactive-p)
					  cmpl-source-interactive
                                        current-completion-source))
	   (entry (add-completion-to-head string)))

      (if num-uses (set-completion-num-uses entry num-uses))
      (if last-use-time
	  (set-completion-last-use-time entry last-use-time))
      )))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))


(if (tipa-overload-allow 'kill-completion 'patched-completion)
    (progn
(defun kill-completion (string)
  "Patched."
  (interactive (interactive-completion-string-reader "Completion to kill"))
  (if (check-completion-length string)
      (delete-completion string))
  )
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))



(if (tipa-overload-allow 'complete 'patched-completion)
    (progn
(defun complete (&optional arg)
  "Patched. Fill out a completion of the word before point.
Point is left at end.  Consecutive calls rotate through all possibilities.
Prefix args ::
  control-u :: leave the point at the beginning of the completion rather
               than at the end.
  a number  :: rotate through the possible completions by that amount
  `-'       :: same as -1 (insert previous completion)
 {See the comments at the top of `completion.el' for more info.}"
  (interactive "*p")
  ;;; Set up variables
  (catch 'quit
    (cond ((eq last-command this-command)
	   ;; Undo last one
	   (delete-region cmpl-last-insert-location (point))
	   ;; get next completion
	   (setq cmpl-current-index (+ cmpl-current-index (or arg 1)))
	   )
	  (t
	   (if (not cmpl-initialized-p)
	       (initialize-completions));; make sure everything's loaded
	   (cond ((consp current-prefix-arg);; control-u
		  (setq arg 0)
		  (setq cmpl-leave-point-at-start t)
		  )
		 (t
		  (setq cmpl-leave-point-at-start nil)
		  ))
	   ;; get string
	   (setq cmpl-original-string (symbol-before-point-for-complete))
	   (cond ((not cmpl-original-string)
		  (setq this-command 'failed-complete)
		  (message
		   (format
		    (concat
		     "To complete, the point must"
		     "be after a symbol at least %d "
		     "character long.")
		     completion-prefix-min-length))
		  (throw 'quit t)
		  ))
	   ;; get index
	   (setq cmpl-current-index (if current-prefix-arg arg 0))
	   ;; statistics
	   (cmpl-statistics-block
	    (note-complete-entered-afresh cmpl-original-string))
	   ;; reset database
	   (completion-search-reset cmpl-original-string)
	   ;; erase what we've got
	   (delete-region cmpl-symbol-start cmpl-symbol-end)
	   ))

    ;; point is at the point to insert the new symbol
    ;; Get the next completion
    (let* ((print-status-p
	    (and (>= baud-rate completion-prompt-speed-threshold)
		 (not (minibuffer-window-selected-p))))
	   (insert-point (point))
	   (entry (completion-search-next cmpl-current-index))
	   string
	   )
      ;; entry is either a completion entry or a string (if cdabbrev)

      ;; If found, insert
      (cond (entry
	     ;; Setup for proper case
	     (setq string (if (stringp entry)
			      entry (completion-string entry)))
	     (setq string (cmpl-merge-string-cases
			   string cmpl-original-string))
	     ;; insert
	     (insert string)
	     ;; accept it
	     (setq completion-to-accept string)
	     ;; fixup and cache point
	     (cond (cmpl-leave-point-at-start
		    (setq cmpl-last-insert-location (point))
		    (goto-char insert-point))
		   (t;; point at end,
		    (setq cmpl-last-insert-location insert-point))
		   )
	     ;; statistics
	     (cmpl-statistics-block
	      (note-complete-inserted entry cmpl-current-index))
	     ;; Done ! cmpl-stat-complete-successful
	     ;;display the next completion
	     (cond
	      ((and print-status-p
		    ;; This updates the display and only prints if there
		    ;; is no typeahead
		    (sit-for 0)
		    (setq entry
			  (completion-search-peek
			   completion-cdabbrev-prompt-flag)))
	       (setq string (if (stringp entry)
				entry (completion-string entry)))
	       (setq string (cmpl-merge-string-cases
			     string cmpl-original-string))
	       (message "Next completion: %s" string)
	       ))
	     )
	    (t;; none found, insert old
	     (insert cmpl-original-string)
	     ;; Don't accept completions
	     (setq completion-to-accept nil)
	     ;; print message
	     ;; This used to call cmpl19-sit-for, an undefined function.
	     ;; I hope that sit-for does the right thing;don't know, rms.
	     (if (and print-status-p (sit-for 0))
		 (message "No %scompletions."
			  (if (eq this-command last-command) "more " "")))
	     ;; statistics
	     (cmpl-statistics-block
	      (record-complete-failed cmpl-current-index))
	     ;; Pretend that we were never here
	     (setq this-command 'failed-complete)
	     )))))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))


;;}}}
;;{{{ mouse

;;; ........................................................... &mouse ...
;;; There is one major flaw in this function: it doesn't allow us to move
;;; point when dragging up! Let's correct it...
;;;

(if (tipa-overload-allow 'mouse-drag-region 'patched-mouse-drag-region)
    (progn
(defun mouse-drag-region (start-event)
  "Patched. Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
In Transient Mark mode, the highlighting remains once you
release the mouse button.  Otherwise, it does not."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* (sp
	 (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-frame (window-frame start-window))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (click-count (1- (event-click-count start-event))))
    (setq mouse-selection-click-count click-count)
    (mouse-set-point start-event)
    (let ((range (mouse-start-end start-point start-point click-count)))
      (move-overlay mouse-drag-overlay (car range) (nth 1 range)
                    (window-buffer start-window)))
    (deactivate-mark)
    (let (event end end-point)
      (track-mouse
        (while (progn
                 (setq event (read-event))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))
          (if (eq (car-safe event) 'switch-frame)
              nil
            (setq end (event-end event)
                  end-point (posn-point end))

            (cond
              ;; Are we moving within the original window?
             ((and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (goto-char end-point)
              (let ((range (mouse-start-end start-point (point) click-count)))
                (move-overlay mouse-drag-overlay (car range) (nth 1 range))))

             (t
              (let ((mouse-row (cdr (cdr (mouse-position)))))
                (cond
                 ((null mouse-row))
                 ((< mouse-row top)
                  (mouse-scroll-subr start-window (- mouse-row top)
                                     mouse-drag-overlay start-point))
                 ((>= mouse-row bottom)
                  (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                     mouse-drag-overlay start-point)))))))))


      (if (consp event)
;;; When we scroll into the mode line or menu bar, or out of the window,
;;; we get events that don't fit these criteria.
;;;            (eq (get (event-basic-type event) 'event-kind) 'mouse-click)
;;;            (eq (posn-window (event-end event)) start-window)
;;;            (numberp (posn-point (event-end event)))
          (let ((fun (key-binding (vector (car event))))
		(ovs (overlay-start mouse-drag-overlay))
		(ove (overlay-end mouse-drag-overlay))
		)
            (if (not (= ovs ove))
                (let (last-command this-command)
		  (if (= ovs start-point)
		      (progn (push-mark ovs t t)
			     (goto-char ove))
		    (push-mark ove t t)
		    (goto-char ovs))
                  (copy-region-as-kill (point) (mark t)))
              (goto-char ove)
              (setq this-command 'mouse-set-point))))
      (delete-overlay mouse-drag-overlay))))
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
))

;;}}}


(provide 'tinypatch)
;;; ................ end of tinypatch.el ...................................
