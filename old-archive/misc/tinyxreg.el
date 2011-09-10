;; @(#) tinyxreg.el -- Restoring points/win cfg stroed in reg. via X-popup

;; @(#) $Id: tinyxreg.el,v 1.12 1995/10/13 06:54:30 jaalto Release_5 jaalto $
;; @(#) $Keywords: X, menu, registers $
;; $KnownCompatibility: 19.28, 19.29 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Oct 4 1995
;;
;; To get information on this program use ident(1) or do M-x tixr-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinyxreg|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Stores point/win cfg to X-POPUP LIST while storing it to register|
;; 13-Oct-1995|1.12|~/misc/tinyxreg.el.Z|

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
;;	(require 'tinyxreg)
;;
;; - Or use autoload, prefered, since your emacs starts up faster.
;;   Code can be easily extracted with function tinylib.el/ti::m-pkg-rip-magic
;;
;;* (autoload 'tixr-jump-to-register		"tinyxreg" t t)
;;* (autoload 'tixr-jump-to-register-mouse	"tinyxreg" t t)
;;* (autoload 'tixr-point-to-register		"tinyxreg" t t)
;;* (autoload 'tixr-point-to-register-mouse	"tinyxreg" t t)
;;* (autoload 'tixr-remove-reg			"tinyxreg" t t)
;;* (autoload 'tixr-trash			"tinyxreg" t t)
;;
;; - To get default bindings automatically, put following hook definition
;;   into your emacs. Otw you have to define your own keys.
;;
;;* (setq tixr-load-hook 'tixr-default-bindings)
;;



;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o  Stores point/win cfg to X-POPUP LIST while storing it to register
;; o  Allows you to use X-popup to pick register associated with the file.
;;    imenu.el should enable using this in non-X environment too.


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tixr-" in front of
;;   every function & variable. It stands for '(ti)ny (X) popup for
;;   storage (r)egisters
;; - variable names contain letters 'tixr-:', excecpt version/hook/mode vars.

;; PREFACE
;; ========================================
;; - I saw post in comp.emacs where cpg@cs.utexas.edu (Carlos Puchol) asked:
;;
;;        I find that my life would be remarkably eased if only I
;;        could "jump" to the marks from a menu.
;;
;;        Please, let me know if i can implement this myself
;;        through some sort of macro or something.
;;
;;
;; - As a hobby I started quicly sketching what the person should
;;   do.. I dind't plan to write any code. It just happened that I
;;   got interested in the subject and started experimenting with
;;   couple of functions.
;;
;; - As a result I had complete package in hand within an hour or
;;   so, which I also posted to the group. Later on I properly
;;   packaged all the functions here and rewote whole thing.
;;
;; REGISTER UPDATE NOTE
;; ========================================
;; - If you wonder why some of the registers disappear from the popup
;;   while you were sure you just stored some point to them, the reason is
;;   here.
;; - If you kill some buffer, or reload it again with find-alternate-file
;;   that means that the register references "die".
;; - That's why the main function tixr-jump-to-register calls a house
;;   keeping function tixr-update to make sure you can't select invalid
;;   registers. So, trust the poup, what is available, is shown to you :-)
;;
;; READING THE INPUT -- FSF 19.27+ user's benefit
;; ===============================================
;; - If you're using FSF emacs that supports read-event function, this
;;   program will take adavantage of it. It means that when you've
;;   called tixr-point-to-register it waits for input and read the
;;   following char.
;; - But moving a mouse disturbs the function
;;
;;	read-char
;;
;;   And it usually signals error
;;
;;	"non-character input-event"
;;
;;   If you cause action type of "event" by moving a mouse a little.
;; - In FSF case, program uses more bulletproof way for reading the input,
;;   but I am not aware of similar method in XEmacs. If you know how to do
;;   the same there, drop me a note and example function.
;; - XEmacs user's just have to be extra carefull right now...
;;
;; BYTE COMPILE NOTE
;; ========================================
;; - Follow these steps and you should have no troubles. You need to load
;;   the function into memory first, because the file will do some
;;   'require' statements that cannot be executed right if you just
;;   go directly to phase 2. The phase 1 guarantees that the library
;;   functions are available during byte-compilation.
;;
;;   1. M-x load-file    ~/elisp/tinyxreg.el  ;; remember to add the ".el" !
;;   2. M-x byte-compile ~/elisp/tinyxreg.el
;;

;;}}}
;;{{{ history

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Oct	13	1995	[jari]		19.28	v1.12		Release_5
;; - Made the levent loading safe, it doesn't hang now when it has
;;   condition-case around it.
;; - Corrected some typing errors in text. No actual code changes.
;;
;; Oct	9	1995	[jari]		19.28	v1.11		NotReleased
;; - Still little fine tuning: now it's detected if user uses "yank at
;;   mouse point". If he is, the cursor is positioned to the mouse position
;;   beforefe asking storing it to register.
;; - Now actually displays the register name. There was a bug in, which
;;   wiped messages too early.
;;   message-flash		    :! updated new macro
;;   tixr-msg			    :! now uses sit-for, not sleep-for
;;    tixr-jump-to-register	    :! Added missing 'remove' arg
;;
;; Oct	9	1995	[jari]		19.28	v1.10		NotReleased
;; - If user pressed mouse while register were asked, the code "broke"
;;   because the character returned was "nil". Now there is error message
;;   is non character is given. And another configure variable to force
;;   input.
;;   tixr-:wait-till-char-given	    :+ should we wait the right input ?
;;
;; Oct	9	1995	[jari]		19.28	v1.9b		Release_4
;; - Added 'reading the input' section to explain why you might get
;;   error "non-character input-event".
;;
;; Oct	5	1995	[jari]		19.28	v1.9a		NotReleased
;; - Added 'reading the input' section to explain why you might
;;   get error signalled.
;; - Forget to make sort function user onfigurable, now there is var for it.
;;   tixr-:imenu-sort-func          :+ how popup will be sorted
;;
;; Oct	5	1995	[jari]		19.28	v1.8		NotReleased
;; - Added 'register update note' section, where the house keeping is
;;   explained.
;; - Moved default keys behind a function, it's more easy to put them into
;;   effect. now you just set: (setq tixr-load-hook 'tixr-default-bindings)
;; - A bug in tixr-jump-to-register: the popup list was created _before_
;;   housekeeping took effect. Now it takes precedence and you get only
;;   valid points.
;;
;; Oct	5	1995	[jari]		19.28	v1.7		NotReleased
;; - Added 'byte compile note' section. Also removed the byte compile hacks
;;   that jeopardised using this package.
;; - Added global-unset-key hint to the 'installation' which makes C-z
;;   available for prefix key.
;; - Also added message flashing .. they get wiped
;;   tixr-:msg-time		   :+ new variable to control disp. time
;;   message-flash		   :+ new lib func
;;   tixr-msg			   :+ hadles messaging
;;
;; Oct	5	1995	[jari]		19.28	v1.6		NotReleased
;; - It's 00:30 right now and this package it still evolving...
;;   I decided to take advantage of the FSF events features _if_ they
;;   are available. This makes it possible to ignore mouse movement while
;;   reading user input. Now giving register aster mouse click is much
;;   easier and you don't get
;;
;;	"(error "Non-character input-event")"
;;
;;   Errors any more if you happened to move mouse while it was expecting
;;   you to give register name via read-char.
;;
;;   use-fsf-events		   :+ detect and turn on events in fsf
;;   ti::q-read-char		   :+ better read-char
;;   tixr-point-to-register	   :! rewrote the interactve part
;;
;; Oct	4	1995	[jari]		19.28	v1.5		Release_3
;; - More fine tuning. What happens if you kill buffer where is marker ?
;;  of course the marker becomes invalid, so I have to keep up to date...
;;  list-prepend		   :+ lib func
;;  tixr-update			   :+ kills dead markers before showing
;;  tixr-jump-to-register-mouse	   :! calls tixr-update now
;;
;; Oct	4	1995	[jari]		19.28	v1.4		Release_2
;; - Hmmm, there were some errors that by byte copiler function didn't
;;   notice. Should check that function I trust... Now comiles cleanly.
;;   BOLP ..			   :+ lib macros added
;;   register-live-p		   :+ lib macro missing
;;
;; Oct	4	1995	[jari]		19.28	v1.3		Release_1
;; - Some more final touches. Made description texts user configurable,
;;   so that he doesn't necessary write his own function.
;;   make-fake-event		   :+ new macro for lib
;;   tixr-:buffer-fmt		   :+ name len desc field
;;   tixr-:wcfg-fmt		   :+ window entry desc form
;;   tixr-event			   :+ simulate keyboard
;;   tixr-trash			   :+ to kill the popup list
;;   tixr-point-to-register-mouse  :! rewrote
;;   tixr-point-to-register	   :! interactive rewrote
;;
;; Oct	4	1995	[jari]		19.28	v1.2		NotReleased
;; - Changed all 'my-' prefixes to package prefix 'tixr-'. Total rewrite
;;   Nothing like the one I posted yesterday. This is the _real_ thing.
;;
;; Oct	3	1995	[jari]		19.28	v1.1		Created
;; - Small version posted to gnu.emacs.help.



;; To do list:
;; ========================================
;; - Some of these functions are in general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.


;;}}}

;;; Code:

;;; ......................................................... &require ...

(require 'cl)
(require 'backquote)
(require 'imenu)


;;{{{ setup: -- private

;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar tinyxr-load-hook nil
  "Hook that is run when package is loaded.")


;;; ... private variables ................................. &v-private ...


(defvar tixr-:preg  nil
  "Holds point markers.")

(defvar tixr-:wreg  nil
  "Holds window markers.")

;;}}}
;;{{{ setup: -- user vars

;;; ... user configurable .................................. &v-config ...


(defvar tixr-:wait-till-char-given t
  "*Non-nil causes waiting till register character is given.
Any mouse clicks or events are ignored
[provided events are supported in current emacs].
")

(defvar tixr-:imenu-sort-func 'imenu--sort-by-name
  "*The imenu's popup sort function. See imenu.el

'imenu--sort-by-name  Gives you registers soted by buffer name.
")

(defvar tixr-:msg-time 0.5
  "*Time value to display messages in seconds.")

;;  handy if you want to call from non-mouse, eg. pressing key.
;;
(defvar tixr-:x-coord 170
  "*Default menu coordinate.")

(defvar tixr-:y-coord 170
  "*Default menu coordinate.")


(defvar tixr-:description-func  'tixr-description
  "*Function to return popup description string.
Function should accept two arguments: REGISTER and WINDOW-ARG")


(defvar tixr-:title  "Register list"
  "*Popup title.")


;;;
(defvar tixr-:buffer-fmt "%-20s"
  "*This allows you to size the filename length area reserved for
default popup description.

Note:  The entries itself are stored in this form, so changing this
affets only new entries.
")


(defvar tixr-:wcfg-fmt '(concat "\177 Win " bn)
  "*This is the Window config FORM that is evaled when
the description is put into the list. You can use variable BN
to refer current buffer name.

Remember that list will be sorted later, so you may want to have
common beginning for all win cfg registers.
")

;;}}}


;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tixr-version
  "$Revision: 1.12 $"
  "Latest version number.")


(defconst tixr-version-id
  "$Id: tinyxreg.el,v 1.12 1995/10/13 06:54:30 jaalto Release_5 jaalto $"
  "Latest modification time and version number.")

(defconst tixr-version-doc
  "tinyxreg.el -- Restoring points/win cfg stroed in reg. via X-popup

First created: Oct 4 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tixr-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tixr-version-doc
       "\n\ncurrent version:\n" tixr-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &Funcs ###



;;{{{ lib

;;; ....................................................... &lib-funcs ...
;;; ** NOTICE **
;;;    Do not copy these, since they are soon available
;;;    tinylibm.el , tinylib.el tinylibXXX..el

;;; ----------------------------------------------------------------------
;;;
(defun use-fsf-events ()
  "Loads FSF levents.el if running right emacs.

Returns:
  t	,events loaded
  nil
"
  (if (and (not (boundp 'xemacs-logo))  ;are we in FSF ?
	   (boundp 'emacs-minor-version)
	   (> emacs-minor-version 27))  ;at least in 19.28 levents exist
      (progn
	(if (not (fboundp 'event-to-character))
	    (condition-case		;let's be cautious with load
		(progn
		  (load-library "levents") ;has no provide statement
		  t)			   ;yes, we can use events
		(error			;if load failed
		 nil))
	  ))
    nil
    ))




;;; FSF 19.27+ user:  this makes possible to ignore mouse while reading input
;;;
(use-fsf-events)			;enable events !!


(defmacro message-flash (time &optional sleep &rest body)
  "Flash message briefly for TIME [default 0.2] in seconds and wipe it away.

SLEEP instructs to use sleep-for instead of sit-for. It's recommended to
use SLEEP if you want to be sure the message is seen. Possible values:
  'sleep        ,use sleep-for function
   any          ,use sit-for function

The BODY can be any expression yielding a string.
"
  (`
   (progn
     (message (,@ body))
     (if (equal 'sleep (, sleep))
	 (sleep-for (or (, time) 1))
       (sit-for (or (, time) 0.2)))
     (message "")
     )))


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

(defmacro NEXTP (list)
  "Advances list pointer with cdr."
  (` (setq (, list) (cdr (, list)))))


(defmacro list-add (list object &optional test)
  "Appends OBJECT to LIST.  When optional TEST
is non-nil, tests if OBJECT already exist before adding.
"
  (` (if (or (null (, test))
             (not (member (, object) (, list)) ))
         (setq (, list)
               (append (, list) (list (, object)))))))



;;;
(defmacro list-prepend (list object &optional test)
  "Sticks destructively OBJECT on the front of LIST. When optional TEST
is non-nil, tests if OBJECT already exist before adding.

Example:
   nbr-list                     --> '\(2 3 4\)
   \(list-prepend 1 nbr-list\)  --> '\(1 2 3 4\)
"
  (` (if (or (null (, test))
	     (not (memq (, object) (, list)) ))
	 (setq
	  (, list)
	  (cons (, object) (, list))))))



(defun make-fake-event (x y &optional mouse-sym)
  "Make fake event. X and Y are col and row coordinates and MOUSE-SYM
is mouse's event description symbol. Default is 'mouse-1.

Remeber: this is not full blown fake, just sufficent one, if
caller uses any of 'posn-' function, this doesn't fool it.
"
  (`
   (list
    (or (, mouse-sym) 'mouse-1 )
    (list
     (selected-window)
     1					;<just some calue>
     (cons (, x) (, y) )
     -23723628
     ))))

;;; See register.el::insert-register
;;;
(defmacro register-live-p (char)
  "Tests if register CHAR contains valid window configuration or mark"
  (` (let ((val (get-register (, char))))
       (if (or (consp val)		;window config
	       (and (markerp val)		;mark
		    (marker-buffer val))
	       )
	   t
	 nil
	 ))))



;;; ----------------------------------------------------------------------
;;;
(defun ti::q-read-char (&optional msg)
  "Reads character. Non character, eg. mouse event in considered as abort.
You can supply optional MESSAGE.
"
  (let* ((ev-loop t)
	 (EVENT (fboundp 'event-to-character)) ;can events be used ?

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
    (if msg (message msg))
    (if (null EVENT)
	(setq ch (read-char))
      ;; - By using event reading, we can accept any action and it doesn't
      ;;   break this function.
      ;; - moving the mouse generates couple of unwanted events, ignore
      ;;   them. It also removes message.
;;      (sit-for 0.5)                   ;to clear the buffer
      (while ev-loop
	(discard-input)
	(if msg (message msg))
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

    (if (null EVENT) nil		;clear the buffer
      (sit-for 0.2 )
      (discard-input))

    ch
    ))





;;}}}
;;{{{ misc

;;; ...................................................... &norm-funcs ...

;;; ----------------------------------------------------------------------
;;;
(defun tixr-msg (msg)
  (message-flash tixr-:msg-time 'sit msg))

;;; ----------------------------------------------------------------------
;;;
(defmacro tixr-event ()
  "Returns fake event"
  (make-fake-event tixr-:x-coord  tixr-:y-coord))


;;; ----------------------------------------------------------------------
;;;
(defun tixr-list ()
  "Returns register list, point list + window list."
  (let* ((ptr   tixr-:wreg)
	 (list  (copy-sequence tixr-:preg))
	 )
    ;;  concat two lists
    (while ptr
      (list-add list (car ptr))
      (NEXTP ptr))
    list
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tixr-default-keys ()
  "Installs default keybindings."
  (interactive)

  ;;  There is no other good use for these
  (global-set-key "\C-x/"           'tixr-point-to-register)
  (global-set-key "\C-x\\"          'tixr-point-remove-register)

  ;;  The "C-c j" is like C-x j , but showing the popup
  (global-set-key "\C-cj"           'tixr-jump-to-register)

  ;;  C-x is so easy to reach with left hand... and free
  (global-set-key [?\C-x mouse-1]   'tixr-jump-to-register-mouse)
  (global-set-key [?\C-x S-mouse-1] 'tixr-point-to-register-mouse)

  (if (interactive-p)
      (tixr-msg "Register Keys bound ok."))
  )




;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tixr-remove-reg (char &optional arg)
  "Removes register CHAR from stored window and point lists.
ARG suggests looking in window list.
"
  (interactive "cRemove register: \nP")
  (let* ((ptr (if arg tixr-:wreg  tixr-:preg))
	 elt
         )
    (if (null (setq elt (rassq char ptr)))
	nil
      (if arg
	  (setq tixr-:wreg (delete elt tixr-:wreg))
	(setq tixr-:preg (delete elt tixr-:preg)))
    )))


;;; ----------------------------------------------------------------------
;;;
(defun tixr-update ()
  "Kills all registers from lists that are not alive any more.
Eg. marker dies if you revert the buffer; kill and load it again."
  (let* ((ptr tixr-:preg)
	 elt
	 reg
	 list
	 )
    ;;  We simple copy valid elements to another list
    (while ptr
      (setq  elt (car ptr)  reg (cdr elt))
      (if (register-live-p reg)
	  (list-prepend list elt))
      (NEXTP ptr))

    (setq tixr-:preg list   ptr tixr-:wreg)
    ))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tixr-trash ()
  "Empties both window and point caches."
  (interactive)
  (setq tixr-:preg nil   tixr-:wreg nil)
  (if (interactive-p)
      (tixr-msg "Register lists trashed."))
  )

;;; ----------------------------------------------------------------------
;;;
(defun tixr-kill-reg (char)
  "kills register from all lists."
  (tixr-remove-reg char nil)
  (tixr-remove-reg char 'window)
  )



;;; ----------------------------------------------------------------------
;;;
(defun tixr-add-reg (char arg &optional desc)
  "Stores register CHAR to window or point list.
ARG tells to store to window list.
"
  (let* (
	 (desc (if (stringp desc)
		   desc
		 (char-to-string char)))
	 (data (cons desc char))
         )
    (if arg
	(list-add tixr-:wreg  data)
      (list-add tixr-:preg  data))
    ))

;;}}}

;;{{{ storing

;; ----------------------------------------------------------------------
;;; So that you call this from mouse
;;;
(defun tixr-description (reg-nbr &optional arg)
  "Return description text for popup list"
  (let* ((bn   (file-name-nondirectory (buffer-name)))
	 (cfg  tixr-:wcfg-fmt)
	 )
    (format (concat tixr-:buffer-fmt " %4s %s")

	    (if arg
		;;  the 177 should print nice block
		;;  so that sorting puts cfg entries last
		(eval tixr-:wcfg-fmt)
	      bn)

	    (if arg
		""
	      (int-to-string
	       (count-lines (point-min-marker) (BOLP))))

	    (char-to-string reg-nbr)	;arg 2
	    )))

;;; ----------------------------------------------------------------------
;;;
;;;
;;;###autoload
(defun tixr-point-to-register-mouse (event)
  "See tixr-point-to-register."
  (interactive "e")

  ;;
  ;;	- User using "flying" mouse paste mode? See var mouse-yank-at-point
  ;;	- If he is, then move cursor visually to mouse point first.
  (if (null mouse-yank-at-point)
      (mouse-set-point event))

  (call-interactively 'tixr-point-to-register)
  )

;;; ----------------------------------------------------------------------
;;; based on register.el::point-to-register
;;;
;;;###autoload
(defun tixr-point-to-register (char &optional arg verb)
  "Store current location of point in register REGISTER and to X-popup list.
With prefix ARGUMENT, store current frame configuration. VERBOSE enables
message printing.


Use \\[tixr-point-to-register]  to go to that location or restore the
configuration.
"
  ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ interactive handling ^^^
  (interactive
   (list

    (let (CHAR
	  (msg
	   (cond
	    (current-prefix-arg
	     "Store Window cfg to register: " )
	    (t
	     "Store point to register: ")))
	  )
      (setq CHAR (ti::q-read-char msg))

      ;;  Should we keep asking if use accidentally pressed mouse, a "click"
      (if (and (null CHAR)  tixr-:wait-till-char-given)
	  (while (null (setq CHAR (ti::q-read-char msg)))))

      (if (null CHAR)			;user mouse-click ?
	  (error "Non-character input. Please select register name."))

      ;;  Show where it got stored.
      (tixr-msg (concat msg (char-to-string CHAR)))
      CHAR
      )

    current-prefix-arg
    ))


  ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the body ^^^
  (let* ((dfunc   tixr-:description-func)
	 (verb    (or verb (interactive-p)))
	 desc
         )

    (setq desc				;get the popup description
	  (if (fboundp dfunc)
	      (funcall dfunc char arg)
	    nil))

    (tixr-remove-reg char arg)
    (tixr-add-reg    char arg desc)

    ;;   Now the normal emacs thing
    (set-register
     char
     (if (null arg)
	 (point-marker)
       (current-frame-configuration)
       ))
    ))

;;}}}
;;{{{ jumping

;;; ----------------------------------------------------------------------
;;; - for calling from keybord
;;;
;;;###autoload
(defun tixr-jump-to-register (&optional remove)
  "See tixr-jump-to-register."
  (interactive)
  (tixr-jump-to-register-mouse nil remove))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tixr-jump-to-register-mouse (event &optional remove)
  "Displays X-list of register. Restores register or
optionally REMOVES register from X-list.
"
  (interactive "e\nP")
  (let* (
	 (imenu-sort-function  tixr-:imenu-sort-func)
	 (event                (or event (tixr-event)))	;kbd call
         (title                tixr-:title)
	 (verb		       (interactive-p))
         list	 data
	 char
         )
    (tixr-update)			;update register list
    (setq list (tixr-list))

    (if (null list)
        (if verb (message "Sorry, both register lists are empty."))
      (setq data (imenu--mouse-menu  list event title))

      (if (null data)
	  (if verb (tixr-msg "Register action Cancelled"))
	(setq char (cdr data))

	(cond
	 (remove
	  ;;  Remove from both lists
	  (tixr-kill-reg char)
	  (cond
	   (verb
	    (tixr-msg
	     (concat " Register ["
		     (char-to-string char) "] removed"))
	    (sleep-for 1)		;too fast otw when you move mouse..
	    )))

	 (t
	  (jump-to-register (cdr data) nil))
	 )))
    ))


;;}}}



(provide   'tinyxreg)
(run-hooks 'tixr-load-hook)
;;; ............................................................. &eof ...
