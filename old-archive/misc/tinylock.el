;; @(#) tinylock.el -- Simple emacs locking utility

;; @(#) $Id: tinylock.el,v 1.5 1995/09/01 18:36:33 jaalto Exp jaalto $
;; @(#) $Keywords: locking , access $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Id

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Feb 11 1995
;;
;; To get information on this program use ident(1) or do M-x til-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinylock|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Simple emacs locking utility|
;; 01-Sep-1995|1.5|~/misc/tinylock.el.Z|

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
;; Put this file on your Emacs-Lisp load path, add one of these into your
;; ~/.emacs startup file
;;
;; - Normal load
;;	(require 'tinylock)
;;
;; - Autoload, your emacs starts up faster, prefered
;;
;;	(autoload 'til-lock "tinylock" "Lock emacs" t)
;;
;; - ESC-l, suggested keybinding, replaces downcase-word binding
;;   because you can accomplish the same with C-x C-l,
;;   downcase-region.
;;
;;	(global-set-key "\M-l" 'til-lock)     ;; Suggested keybinding.

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o   Locks emacs completely until right key is entered.
;; o   False attemps are stored into history log.
;; o   Blanks or displays buffer message when locked.
;; o   Hooks: before and after lock is entered/removed

;; PREFACE
;; ========================================
;; - This is again one of those 'tiny tools' I came up with time to time.
;;   It was inspired by a post to gnu.emacs.help group where someone
;;   asked for emacs locking capability. 2 years ago I did similar
;;   terminal locking shell script, but I never thought it ewould be
;;   nice for emacs too.
;; - I had a sketch of what to do 2 months ago (Feb 11 -95 now), but I was so
;;   busy I never completed this el. So I couldn't post the answer to
;;   person asking for the locking capability.
;; - I find this usefull, when I'm off work and want to set my emacs in
;;   consistent state, so that no-one accidentally messes my C++ project
;;   files. I normally leave all xterms and emacses open during the
;;   weekend and anyone may use my X-tube meanwhile.
;; - Hope people find this usefull!


;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "til-" in front of
;;   every function & variable. It stands for '(ti)ny (l)ock.
;; - variable names contain letters '-:', except version/hook vars.

;;}}}
;;{{{ History

;; ........................................................ &t-history ...
;; Sep 1	1995	[jari]		19.28	v1.5		Release_4
;; - If there were running process that could call 'error', like GNUS,
;;   this program would break and another user could access the emacs.
;;   Now we kill all running processes, before shutting down to prevent any
;;   other tasks to interrupt us.
;; - Added ###autoload directives. Added til-load-hook.
;;
;; Mar 7	1995	[jari]		19.28	v1.4		Release_3
;; - Added til-:blank-kill, so that it wont't crowd buffer-menu.
;; - Added til-:h-limit + several funcs to limit locking time if needed.
;;
;; Mar 1	1995	[jari]		19.28	v1.3		NotReleased
;; - Added 'blank' capability and restoring the screen. Throw err. fixed.
;; - autoload setup presented. Added til-[before,after]-lock-hook
;;
;; Feb	13	1995	[jari]		19.28	v1.2		Release_2
;; - til-history: Now keeps record of false enries,
;; - password input is now more bullet proof, has new parameter to limit input.
;; - Uses events for reading confirm now, now you can use mouse also :-/
;; - Corrected the library load, using unwind-protect now. Prg Date corr.
;; - Fixed one TIR- function to use modulename TIL-. Wait time corrected.
;;
;; Feb	11	1995	[jari]		19.28	v1.1		Release_1
;; - First *complete* implementation.


;; Bugs, to do list:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.

;;}}}

;;{{{ require

;;; ......................................................... &require ...

(defun til-errmsg (msg)
  "Prints error messages of the module."
  (error (concat "tinylock : " msg)))


;; Without the 'event' reading, the locking is impossible.
(if (fboundp 'event-to-character)
    nil	;loaded ok
  (condition-case err
      ;;   Have to use this: 19.28 distribution had no 'provide statement...
      (load-library "levents")
    (file-error
     (error "cannot load levent.el"))))

;;}}}

;;; Code:

;;{{{ setup: variables
;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar til-before-lock-hook nil
  "*Hook that is run just before keyboard is locked with inhibit-quit")

(defvar til-after-lock-hook nil
  "*Hook that is run after lock is removed.")

(defvar til-load-hook nil
  "Hook run after file is loaded..")

;;; ... private variables ................................. &v-private ...

(defconst til-:history nil
  "\(DATE PASSWD\) A storage where attempts of entering locked emacs is put.
Cleared every time lock takes effect.")

(defconst til-:RET nil
  "Just global returning variable.")

;;; ... user configurable .................................. &v-config ...

(defvar til-:in-msg "Lock emacs ? [y]/n"
  "*String that asks if user wants to enable lock.")

(defvar til-:out-msg "Emacs unlocked."
  "*String that signifies that lock is no longer in effect.")

(defvar til-:key-msg "This emacs is locked, enter key:"
  "String which is displayed to user who tries to access locked emacs.")

(defvar til-:entry-err "Unauthorized access."
  "*Sring that signals invalid password for opening emacs.")

(defvar til-:entry-err-sleep 2
  "*Time in seconds that is waited until new entry is possible.")

(defvar til-:lock-in-effect ">> LOCKED <<"
  "*Message that signifies that lock has been turned on.")

(defvar til-:save-buffers t
  "*If non-nil, then all buffers are saved before locking takes place.
It is adviced to keep tha value t, since you might not be able access
emacs again, or someone may kill it by accident.")

(defvar til-:proc-kill t
  "*Kills automatically all processes before locking up.
This utility will not perform locking if there is detected any running
processes.")

(defvar til-:hist-buf "*til-hist*"
  "*Buffer to output the history data.")

(defvar til-:blank-bn "*blank*"
  "*Buffer name used when screen is blanked.")

(defvar til-:blank-kill t
  "*Tells is the blank buffer will be removed after lock is removed.")

(defvar til-:blank-when-locked t
  "*If non-nil, the buffer til-:blank is shown, created if not exist.")

;; Just in case, allthoug I don't use this myself.
(defvar til-:h-limit nil
  "*If for some reason you forgot your password and you have many valuable
files inside emacs, which of course were saved before locking, but you
feel that it's too much trouble to set up same configuration again:
points and temp buffers and so on, then this variable may help you.

It determines hour limit that keeps the lock in effect. After the time
is up, it removes the lock automatically. This may prevent some
deadlock situations.  Notice, that the time cannot be counted when
emacs is locked, only after you press a key.

The hour value calculated which is compared against this is not
accurate and may be off by +24h if the month changes in the same day
you lock your emacs. So if you have set 2 hours you may end up waiting 26h.

Setting to nil means no hour limit: Locked untiol right password given.
")


;;}}}
;;{{{ setup: -- version notice

;;; ... version info ...................................... &v-version ...

(defconst til-version
  "$Revision: 1.5 $"
  "Latest version number.")


(defconst til-version-id
  "$Id: tinylock.el,v 1.5 1995/09/01 18:36:33 jaalto Exp jaalto $"
  "Latest modification time and version number.")

(defconst til-version-doc
  "tinylock.el --  simple emacs locking utility

First created: Feb 11 1995
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun til-version ()
  "tinylock.el information."
  ;; (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       til-version-doc
       "\n\ncurrent version:\n" til-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}


;;{{{ code: misc funcs

;;; ########################################################## &Macros ###

(defmacro til-time-dd (time)
  "Rrturn Day from time"
  (list 'string-to-int (list 'substring time 8 10)))

(defmacro til-time-hh (time)
  "Return hour from time"
  (list  'string-to-int (list 'substring time -13 -11)))

;;; ########################################################### &Funcs ###

;;; ----------------------------------------------------------------------
(defun til-msg (&rest args)
  "Prints modules messages."
  ;; pasting elements together
  (let ((msg
	 (concat "TIL : "  (mapconcat 'concat args " ")))
	)
    (message msg) (sleep-for 2)
    ))


;;; ----------------------------------------------------------------------
;;; gt = greater than test
;;;
(defun til-hour-gt (time hour)
  "Returns t if tm + hour > current time. Supposes 31 days in month if wraps."
  (let* ((tm	(current-time-string))	;current time
	 (tm-h	(til-time-hh tm))
	 (tm-d	(til-time-dd tm))
	 (t-h	(til-time-hh time))	;time given to us
	 (t-d	(til-time-dd time))
	 ;;   Have wrrapped month, from 31d --> 1d ?
	 (days (if (< tm-d t-d)		;yes
		   (+ (- 31 t-d) tm-d)
		 (- tm-d t-d)))
	 (hours (+ (* 24 days) (- tm-h t-h)))
	 )
    (if (> hours hour) t nil)
    ))

;;; ----------------------------------------------------------------------
;;;
(defun til-print-p (ch)
  "Determines if character can be printed normally.
Returns t if so."
  (if (and (not (null ch))       ;it must not be nil
	   (integerp ch)
	   (or (memq ch '(?\t ?\n ?\r ))
	       (and
		(> ch 31)
		(< ch 127))))
      t nil))


;;; ----------------------------------------------------------------------
;;;
(defun til-confirm (msg)
  "Confirms action. y/Y/RET/SPC = ok. The real character pressed is available
thru global variable til-:RET."
  (let* (ch event)
    (message msg)
    (setq event (read-event)) (setq ch (event-to-character event))
    (message "")
    (setq til-:RET ch)			; ** SET GLOBAL **
    (cond
     ((or (eq event 'return) (eq ch ?\040)	; ret/spc
	  (eq ch ?y) (eq ch ?Y))
      t)
     (t
      nil))))

;;; ----------------------------------------------------------------------
;;;
(defun til-add-history (passwd)
  "Adds to record log til-history, any attemps to enter locked emacs."
  (let* ((d (current-time-string))
	 )
    (setq til-:history
	  (append  til-:history
		   (list (list d passwd))))
    ))


;;; ----------------------------------------------------------------------
;;;
(defun til-proc-control (&optional mode )
  "Possible mode values:
  'kill      ,kill all processes
  nil        ,return all prosesses in string form
"
  (let* ((list  (process-list))
	 ret
	 )
    (if list
	(mapcar
	 (lambda (x)
	   (cond
	    ((eq mode nil)
	     (setq ret (concat (or ret "") (prin1-to-string x))))
	    (t
	     (delete-process x))
	    ))
	 list))
    ret
    ))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun til-history (&optional bp)
  "displays entry history. Optionally to given buffer BP"
  (interactive)
  (let* ((l  til-:history)
	 (ob (current-buffer))		;hmm, relly don't need this..
	 (i 0)
	 el d p
	 )
    (if bp nil				;user gave this
      (setq bp (get-buffer-create til-:hist-buf))
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp))

    (while l
      (setq el (car l))
      (setq d (nth 0 el))    (setq p (or (nth 1 el) "<nil>"))
      (insert
       (format "%2d: %-27s %s\n" i d p))
      (setq i (1+ i))
      (setq l (cdr l))			;advance pointer
      )

    ))


;;}}}
;;{{{ code: reading input
;;; ................................................... &reading-input ...


;;; ----------------------------------------------------------------------
;;;
(defun til-read-input-invisible ()
  "Read keyboard input. Aborts immediately if input is not valid ASCII.
RETURN: nil or input
"
  ;; (interactive)
  (let* ((echo-keystrokes 0)		;prevent showing
	 event str ch
	 )
    (catch 'done
      (discard-input)
      (while t
	(setq event (read-event))
	(setq ch (event-to-character event))
	(cond
	 ((or (null ch) (eq ch ?\n))
	  (throw 'done t))
	 ((til-print-p ch)
	  (setq str (concat str (char-to-string ch)))))
	))
    str))


;;; ----------------------------------------------------------------------
;;;
(defun til-read-input-as-passwd (&optional max)
  "Returns kbd entry, MAX len. Echoes stars instead of chars."
  (let* (str
	ch event len
	(i 0)
	(loop t)
	(cursor-in-echo-area nil)
	(max (or max 80))			;maximum string
	(bar (make-string (+ max 2) ?* ))
	)
    (while loop
      (setq event (read-event))
      (setq ch (event-to-character event))
      (if (til-print-p ch)
	  (progn
	    (if (>= (length str) max)
		(beep)			;signal error
	      (setq str (concat str (char-to-string ch)))
	      (message (substring bar 0 (length str))))
	    )
	(cond
	 ((eq event 'return) (setq loop nil))
	 ((eq event 'backspace)			;remove last char
	  (setq len (length str))
	  (if (= len 0 ) nil
	    (setq str (substring str 0 (1- len))))
	  )))
      (message (substring bar 0 (length str)))
      )
    (message "")
    str
    ))

;;}}}
;;{{{ code: main

;;; ------------------------------------------------------------ &main ---
;;;
;;;###autoload
(defun til-lock ()
  "Locks emacs until password is given."
  (interactive)
  (let* (
	 (cursor-in-echo-area nil)

	 ;;  It's good programming style NOT to use globals directly
	 ;;  inside code This way maintainer sees at glance what it uses.
	 (lock-in-effect til-:lock-in-effect)
	 (proc-kill      til-:proc-kill)
	 (in-msg	 til-:in-msg)
	 (out-msg	 til-:out-msg)
	 (key-msg	 til-:key-msg)
	 (entry-err	 til-:entry-err)
	 (wait		 til-:entry-err-sleep)

	 (blank		 til-:blank-when-locked)
	 (blank-bn	 til-:blank-bn)
	 (blank-bp (if blank		;make buf-ptr if needed
		       (get-buffer-create blank-bn)))
	 (blank-kill     til-:blank-kill)

	 (h-limit        til-:h-limit)
	 (time           (current-time-string))
	 (loop		 t)
	 time-h
	 psw ans
	 str
	 )

    (catch 'done
      (if (null (til-confirm in-msg)) (throw 'done t))

      ;;  It's better to save work, you may forgot the password :-/
      (if til-:save-buffers 	  (save-some-buffers))

      ;;  Make sure there is no running processes
      (if proc-kill
	  (til-proc-control 'kill)	;get rid of them
	(if (setq str (til-proc-control))
	    (if (y-or-n-p (concat "Kill processes: " str " "))
		(progn
		  (til-proc-control 'kill)
		   (sit-for 0.3)	;next message is not shown otw?
		   )
	      (message "Cannot lock before all processes are killed.")
	      (throw 'done t))))

      (message "Now enter lock string...") (sit-for 1) (message "")

      (if (setq psw (til-read-input-as-passwd)) nil
	(til-msg "empty pasword. Cancelling...")
	(throw 'done t))		;it was empty


      (run-hooks 'til-before-lock-hook)

      ;;   we need to restore windows config when we return
      (save-window-excursion
	(save-excursion

	  (if (null blank) nil
	    (switch-to-buffer blank-bp t)
	    (delete-other-windows)	;delete all other windows
	    ;;   This is necessary for some unknown reason
	    ;;   otw, the sreen is not shown
	    (sit-for 1)			;
	    )

	  ;; Now we make interrupting impossible, C-g won't work now on...
	  (setq inhibit-quit t)
	  (setq til-:history nil)	;clear the log buffer

	  (message lock-in-effect)

	  (while loop
	    (if (not (input-pending-p)) nil	;wait for kbd event
	      (discard-input)
	      (message key-msg) (sleep-for 1) (message "")
	      (discard-input)
	      (setq ans (til-read-input-invisible))
	      (cond
	       ((string-equal ans psw)
		(setq loop  nil))	; right password, let user in
	       (t
		(til-add-history ans)	; record to log
		(message entry-err)
		(sit-for wait)))
	      )
	    ;;  should we remove the lock? -- check time lock
	    (if (null h-limit) nil	;locked forever
	      (if (til-hour-gt time time-h)
		  (setq loop nil)))
	    )))

      (if (and blank blank-kill) (kill-buffer blank-bp))
      (message out-msg)
      (setq quit-flag nil inhibit-quit nil)       ; restore flags
      (run-hooks 'til-after-lock-hook)

      nil
      )))

;;}}}


(provide 'tinylock)
(run-hooks 'til-load-hook)
;;; ................ end of tinylock.el ..................................
