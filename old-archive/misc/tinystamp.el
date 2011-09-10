;; @(#) tinystamp.el -- A simple document stamping tool.

;; @(#) $Id: tinystamp.el,v 1.12 1995/08/25 06:18:42 jaalto Released jaalto $
;; @(#) $Keywords: tools, time, file control $
;; $KnownCompatibility: FSF 18.57, 19.28, Lucid 19.10 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Id

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com> (Finland)
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Sept 28th 1994, on a rainy day of fall.
;;
;; To get information on this program use ident(1) or do M-x tis-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinystamp|Jari Aalto|jari.aalto@ntc.nokia.com|
;; A simple document stamping tool|
;; 25-Aug-1995|1.12|~/misc/tinystamp.el.Z|

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
;;   ~/.emacs startup file.
;; - if you want to put function directly into write-file-hooks,
;;   it will handle save-some-buffers command also.
;;
;;     (require 'tinystamp)
;;
;; - plus add one of the following
;;
;;     (tis-install "i" "k")		;installing tinystamp to C-x C-s
;;     (tis-install "i" "h")		;installing [h]ooked version
;;
;; - To uninstall BOTH immediately
;;
;;     (tis-install "u" "i")		; just the "u" is significant

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o   Updates date & mail address every time you save a file. Checks validity!
;; o   You can choose keybind or hook version installation.
;; o   user configurable stamp string via function.

;; PREFACE
;; =================================================
;; Mar 28 1995
;; - People still reminds me that "Hey didn't you know about standard
;;   FSF timestamp.el which does just the same", what they don't realize
;;   is, that when I made this .el it was not available for me! I had 18.57
;;   still running till the end of 1994... besides, I like this one more,
;;   because it's RCS compatible and warns errors in stamps.
;;
;; Sep 28 1994
;; - There is nothing fancy, program just looks for 2 document stamping
;;   directives and updates them whenever you save file with save-buffer
;; - Those who know GNU's RCS revision control tool, are familiar
;;   with the stamping standard I have chosen. The RCS also uses $
;;   character for delimiting it's own documenting strings.  This way
;;   you can easily use ident(1) to collect valuable information on
;;   any file.
;;
;; - I wouldn't suggest adding many stamping directives, because the overhead
;;   of looking many stamps increases. No-one wants to wait for long
;;   when he has pushed C-x C-s [save].
;;
;; STAMP UPDATE
;; - If you pull out code from gnu.emacs.sources group and later you
;;   want to make some modifications and you hit C-x C-s ... God, It touched
;;   the original stamp!
;; - To prevent this happening, there is "no re" that suppresses stamping
;;   on files whose filename matches regexp and whose stamp ID is not
;;   your login name. You have to manually change the stamp ID to your login
;;   name to get it run again in that file.
;;
;; HELP
;; - Brief help is available by calling
;;        describe-function tis-save-buffer
;;
;; KEY BINDINGS
;; - C-x C-s automatically updates your stamps. See installation


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tis-" in front of
;;   every function. It stands for '(ti)ny (s)tamp'


;;}}}
;;{{{ history

;; HISTORY
;; ........................................................ &t-history ...
;; [jaalto]           ,<jaalto@tre.tele.nokia.fi>

;; Aug 	35 	1995  	[jaalto]	19.28	v1.13		Released
;; - If you read other person's file that has stamp, and you accidentally
;;   do C-x C-s, then the stamp will be updated. To prevent stamping
;;   other peoples files, there is now variable tis-no-stamp-re
;;
;; Mar 	30 	1995  	[jaalto]	19.28	v1.12		Released
;; - The installation functions modified, there was problems if
;;   switched between hook & key. Now they return status values.
;;
;; Mar 	28 	1995  	[jaalto]	19.28	v1.11		Released
;; - Corrected key installation so, that it removes hook installation.
;;   Now they are mutually exlusive.
;; - Added some more customization features.
;; + Now user can make his own functions to compose the stamps
;;   tis-contact-func , tis-doc-func
;; + two hooks: tis-stamp-in-hook , tis-stamp-out-hook
;;
;; Jan 	19 	1995  	[jaalto]	19.28	v1.9 - 1.10     Released
;; - Small error in hook version install and stamping corrected.
;;
;; Jan 	11 	1995 	[jaalto]	18.57	v1.8		Released
;; - added nice install/uninstall feature.
;;
;; Dec  2 	1994 	[jaalto]	18.57	v1.7		Release_1
;; - Corrected 19.28 bug in when using zap-to-char. Now uses it's
;;   own zap-to-char function and does not depend on emacs version.
;; ! The tis-current-time and companions revised radically.
;;
;; Nov  3 	1994 	[jaalto]	18.57	v1.4 - 1.6	NotReleased
;; - The documentation layout changed. Corrected 'Contactid' rexp
;; - Version funcs added to get info from emacs
;;
;; Sep 	24 	1994  	[jaalto]       18.57	still v1.3	NotReleased
;; - Hmm. The 'ident' couldn't handle separate word tags like 'Doc id:',
;;   only 'Docid:' -- single words only. Notice that it WON'T allow
;;   spaces around ":". The "Docid         :nicely formatted" isn't
;;   recognized by ident.
;; - I kind of hate that. It should allow spaces and separate words.
;;   The stamp still accept spaces, if you want them, but remember that
;;   ident won't recognize it.
;; ! Changed stamp rexp marks 'Doc id:' --> 'Docid:"...
;;
;; Sep 	20 	1994  	[jaalto]       18.57	v1.3		NotReleased
;; - Having used RCS quite carelessly for 2 years I realized that
;;   the '$' marks have _rigid_ syntax which must be followed.
;;   It goes like this: "TAGkeyword:SPACE<anything>SPACETAG"
;;   --> my stamping format wasn't exactly like that.
;; - There is RCS tool named 'ident' which I found two days ago, if
;;   run on file, it'll print out all tags which has the form
;;   I described. See man pages for more bout ident. And try to
;;   run it over this file!
;; ! Corrected stamping format to conform RCS style.
;; ! Several functions were 'beautified'
;; + Added tis-month-alist for quicker scanning
;; + added tis-msg funtion to display messages centrally
;;
;; Oct  4 	1994  	[jaalto]	18.57	v1.2		NotReleased
;; - I started using folding.el to write my code and it narrowed region
;;   so that the head of file weren't visible to my stamping code -->
;;   no stamping were done if was inside narrowing.
;; - Corrected so that temporarily removes narrowing, before
;;   looking for keywords that could need stampings. Returns to narroving
;;   when done. Seems to work fine right now...
;; - Thanks to Job Ganzevoort <job@flits.ace.nl> who presented way
;;   to temporarily un-narrow.
;;
;; Sep  9 	1994  	[jaalto]	18.57	v1.1		NotReleased
;; - Added two functions:
;;   	tis-s-nth and tis-current-glo-time
;;   which returns globally understandable time string.
;; - Added option "GLO" for tis-country-time-format.
;; - Added directives for unix WHAT command to the beginning of file.
;;
;; Sep  6 	1994  	[jaalto]  	18.57			First created

;;; To do:
;; ========================================
;; <empty>

;;}}}




;;{{{ istallation: key bindings, hooks

;;; Code:

;;; .......................................................... &v-bind ...
;;;      Replaces your save-buffer command
;;;      - Install only when the variable is set to t prior loading


(defun tis-install (mode method)
  "Installs or uninstalls Document stamping feature.
MODE can be 'uninstall/install' or just u/i
METHOD can be 'hook/key' or just h/k

The uninstall will remove both, so the METHOD does not matter.
"
  (interactive "s([i]nstall/uninstall): \nsmethod (hook/[k]ey) :")
  (let ((verb (interactive-p)))
    (if (string-match "^[uU]"  mode)
	(progn
	  (tis-uninstall-hook) (tis-uninstall-key)
	  (if verb (message "Both uninstalled"))
	  nil				;return nil when UNinstalled
	  )
      (cond
       ((string-match "^[hH]"  method)
	(tis-install-hook))
       ((or (string-match "^[kK]"  method)
	    (= (length method) 0)		;return pressed
	    )
	(tis-install-key))
       (t
	(if verb (message "TIS: Don't know what install did you mean"))
	nil)
       ))))


(defvar tis-install-v-old-key nil
  "Holds storage of original function placed on C-x C-s.")

(defun tis-is-key-in ()
  "Checks if key is installed."
  (eq 'tis-save-buffer (lookup-key  global-map "\C-x\C-s")))

(defun tis-install-key ()
  "Changes C-s C-x keybinging. Uninstalls hooked TIS."
  ;; Let's make sure we aren't installed already
  (if (tis-is-key-in) t		;already there
    (if (null (tis-uninstall-hook))
	nil
      (setq tis-install-v-old-key (key-binding "\C-x\C-s"))
      (global-set-key "\C-x\C-s" 'tis-save-buffer)
    t)))


(defun tis-uninstall-key ()
  "Restores keybinding set by [TIS]. Return nil if cannot uninstall"
  (if (and tis-install-v-old-key
	   (tis-is-key-in))
      (global-set-key "\C-x\C-s" tis-install-v-old-key))
  (if (tis-is-key-in) nil t))

;;; ......................................................... &v-hooks ...


(defvar tis-load-hook nil
  "*Hook run when file has been loaded")

(defvar tis-stamp-in-hook nil
  "*Hook to be run before [possible] stamp is updated.")

(defvar tis-stamp-out-hook nil
  "*Hook to be run after [possible] stamp is updated. If you chose keybind
installation, the hook is executed before save-buffer is called, actually
before any write-file-hooks is called. If you chose hooked version, the
calling happens inside the write-file-hooks.

References:
   You may check variable tis-stamp-str string which contains data is
   something was touched.")

;;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ..

;;;       - Install as hook if requested.
(defmacro tis-is-hook-in ()
  "Checks is TIS is hooked."
  (list 'memq ''tis-save-buffer 'write-file-hooks))

(defun tis-install-hook ()
  "Installs Hooked version, removes possible [TIS] keybinding.
Return nil if cannot install; key binding couldn't be removed."
  (if (null (tis-uninstall-key))
      nil
    (or (tis-is-hook-in)
	(setq write-file-hooks
	      (cons 'tis-save-buffer write-file-hooks)))
    ))


(defun tis-list-delete (sym-el sym-list)
  "Removes named SYM-EL from SYM-LIST ,which is 1-dimensional list"
  (let* (l
	 (list (eval sym-list))
	 )
    (if (not (memq sym-el list))
	nil				;not there
      (mapcar
       '(lambda (el)
	  (if (not (eq el sym-el))	;don't put this on the list
	      (setq l (append l (list el) ))))
       list
      )
      (set sym-list l))
    ))


(defun tis-uninstall-hook ()
  "Removes hook placed by TIS."
  (tis-list-delete 'tis-save-buffer 'write-file-hooks)
  (not (tis-is-hook-in))		;ret t if not in there
  )


;;}}}

;;{{{ setup: -- private

;;; ... private variables ................................. &v-private ...

(defvar tis-mch ?$
 "*The marker char for id delimiter")

(defmacro tis-m ()
  "The marker in rex format for BEG .. END matching"
  (list 'regexp-quote (list 'char-to-string 'tis-mch)))


;;;     I don't want update to happen here!!!
;;;      --> use concat for creating rex-str
(defvar tis-docid-rex (concat (tis-m) (regexp-quote "Docid") "[ \t]*:")
 "*The update string to search.")

(defvar tis-contactid-rex (concat (tis-m) (regexp-quote "Contactid") "[ \t]*:")
 "*The update string to search.")

(defconst tis-month-alist
  '(("jan" . 1)     ("feb" . 2)    ("mar" . 3)    ("apr" . 4)
    ("may" . 5)     ("jun" . 6)    ("jul" . 7)    ("aug" . 8)
    ("sep" . 9)     ("oct" . 10)   ("nov" . 11)   ("dec" . 12))
  "Associated list of month names for quick scanning")

(defconst tis-msg-sleep 2
  "delay for showing notices/errors.")

(defconst tis-stamp-str ""
  "The tis-save-buffer will update this string according to what was stamped.
It could be: '*stamped: doc, contact'")


;;}}}
;;{{{ setup: user configurable

;;; ... user configurable .................................. &v-config ...


(defvar tis-no-stamp-re nil
  "*If buffer-filename matches this regegexp AND the samps login name
is not same as yours, then do not stamp the file. ")

;;;	Set time/date format for various countries
;;;	EUR = European  dd.mm.yy format
;;;	USA = yy/mm/dd
;;;     GLO = Globally undertandable format: Jan. 6th 1994
;;;
(defvar tis-country-time-format "GLO"
 "*How to insert the time/date for stamps.")


(defvar tis-allow-stamp-func 'tis-allow-stamp-p
  "*Function to check if the stamping is allowed for this file.")

;; if you change this, you better make sure  tis-allow-stamp-func
;; still works properly.
(defvar tis-doc-func 'tis-get-doc-str
  "*Function to compose the docid string.")

(defvar tis-contact-func 'tis-get-contact-str
  "*Function to compose the contact string")

(defvar tis-contactid-info "email@some.com"
 "*This text will be inserted to Contact field.")

;;;	For more info, see info pages on "case-fold-search"
(defvar tis-cs-rex nil
 "*Case sensitive flag for rex: t=no, nil=yes.")

(defvar tis-sleep 1
 "*How many secs to display stamp notice. Set 0 for no delay.")

(defvar tis-verbose t
  "*Controls if 'stamped xxxx' message is displayed just before saving.
If hook version is installed, these messages are never displayed. ")


;;}}}

;;{{{ setup: -- VERSION

;;; ... version info ...................................... &v-version ...

(defconst tis-version
  "$Revision: 1.12 $"
  "Latest modification time and version number.")

(defconst tis-version-id
  "$Id: tinystamp.el,v 1.12 1995/08/25 06:18:42 jaalto Released jaalto $"
  "Latest modification time and version number.")

(defconst tis-version-doc
  "tinystamp.el -- A simple document stamping tool.

First created: Sep. 6th 1994
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tis-version ()
  "tinystamp.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tis-version-doc
       "\ncurrent version:\n" tis-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}


;;{{{ code: other functions

;;; ########################################################### &Funcs ###


(defun tis-msg (msg &optional eflag)
  "Print TIS package's error message. Show error if eflag raised"
  (message
   (concat (if eflag "*err "  "")
	   "tir: " msg))
  ;;  There may be successive messages...
  (sleep-for tis-msg-sleep))





;;; ----------------------------------------------------------------------
;;;
(defun tis-s-month-n(str)
  "Returns month number for string. Accepts Jan or January with any case.
If Month not recognized, returns nil \"\" "
  (let* (
	 (l (length str))
	 (s (downcase			; cut to 3 chars
	     (if (> l 3) (substring str 0 3) str)))
	 e				;element
	 )
    (if (null (setq e (assoc s tis-month-alist)))
	nil
      (cdr e))
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tis-s-nth(n)
  "Returns string representing number position.
INPUT: string or number as digit."
  (interactive)
  (let (nn)
    (setq nn n)
    (if (stringp n) (setq nn (string-to-int n)))
    (cond
     ((eq nn 1) "st")
     ((eq nn 2) "nd")
     (t "th"))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tis-time-list()
  "Returns list of elements derived from current-time
RET LIST: \( dd mm ...\)
    0 dd     day, number
    1 mm     month, number
    2 yy     year, 2 last numbers
    3 tt     hh:mm
    4 wd     week day, eg. Mon
    5 m      month, string
    6 yyyy   whole year
"
  (interactive)
  (let (time m mm dd yy tt wd yyyy)
    (setq time (current-time-string))  ;"Wed Oct 14 22:21:05 1987"
    (setq m (substring time 4 7))
    (setq mm (or (tis-s-month-n m) ""))
    ;;    we remove trailing space  "2 " --> 2 --> "2"
    (setq dd (int-to-string (string-to-int (substring time 8 10))))
    (setq tt (substring time -13 -8))
    (setq yy (substring time -2 nil))
    (setq yyyy (substring time -4 nil))

    (list dd mm yy tt wd m yyyy)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tis-current-time(&optional class)
  "Returns current time string in various formats.
optional CLASS selects choices, default is 0.
0     = Global format
1     = European format
2     = USA format
"
  (interactive)
  (let (dd m mm yy tt p)
    (setq l (tis-time-list))  	; returns list of time elements
    (setq dd (nth 0 l))
    (setq mm (nth 1 l))
    (setq m  (nth 5 l))
    (setq yy (nth 6 l))
    (setq tt (nth 3 l))

    (setq p (tis-s-nth dd))	; set position: st,nd,th

    (cond
     ((eq class 1)
      (concat dd "." mm "." yy "  " tt)) ;14.10.87  22:21
     ((eq class 2)
      (concat yy "/" mm "/" dd "  " tt)) ;87/10/14  22:21
     (t
      (concat m ". " dd p " " yy "  " tt))) ;14.10.87  22:21
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tis-get-time-str(c-id)
  "Returns current time string in various national formats.
INPUT : country-id as string, not case sensitive
OUTPUT: fomatted time string
"
  (interactive)
  (let (id)
    (setq id (downcase c-id))
    (cond
     ((string= id "eur") (tis-current-time 1))
     ((string= id "usa") (tis-current-time 2))
     ((string= id "glo") (tis-current-time))
     (t 					; unrecognized country
      (tis-msg "tis-get-time-str:please add in your country-id and func")
      ""))    				        ; return empty string
    ))


;;; ----------------------------------------------------------------------
;;; - Call this function only, when REX is matched. The point will
;;;   then be at right position.
;;; INPUT
;;;   str	,informative: what rex looked?  "docid", "contactid"...
;;; RETURN
;;;   t		,ok
;;;   nil	,error
;;;
(defun tis-rex-end-check (str)
  "if rex matched stamp beginning, this makes sure it ENDs ok."
  (let ((fid (concat "stamp err:: " str)) ;func id
	(rexM (tis-m))			  ; the ending marker, copy global
	p pnl pMark fid ret
	)

    (catch 'err
      (setq p (point))		; we are here right now
      (save-excursion
	(if (re-search-forward "\n" nil t)
	    (setq pnl (point))	; here is newline
	  (tis-msg (concat fid " newline missing") 1)
	  (throw 'err t)))		; there must be newline

      (save-excursion
	(if (re-search-forward rexM  nil t)
	    (setq pMark (point))      ; here is marker
	  (tis-msg  (concat fid " ending marker missing") 1)
	  (throw 'err t)))


      ;; - this means that there is open($) text \n text end($).
      ;;   The text must be single lined.
      (if (< pMark pnl) t		; ok
	(tis-msg (concat fid " CRs not allowed inside markers") 1)
	(throw 'err t))
      (setq ret t))
    ;; ----------- catch end -----------------------------
    ret))


;;}}}
;;{{{ code: main functions




;;; ----------------------------------------------------------------------
;;;
(defun tis-ztc(ch)
  "My zap to char implementation.
I started this .el with 18.57, but I found out that they changed the
function definition of zap-to-char in 19.xx, where ztc _deletes_ the last
character in 19, but in 18 it doesn't.

This function only zaps to next occurence fwd, not deleting ch.

RETURN: t,nil         ,if killed something
"
  (let ((p (point))
	c ret
	)
    (save-excursion
      (setq c (char-to-string ch))
      (setq ret (search-forward c nil t))
      (if ret
	  (delete-region p (1- (point))))
      ret)))


;;; ----------------------------------------------------------------------
;;;
(defun tis-get-doc-str()
  "Composes Doc string for stamp."
  (let* (
	 (user (user-full-name))
	 (country tis-country-time-format)
	 (insTime (tis-get-time-str country))
	 (stamp (concat " " insTime "  " user " "))
	 )
    stamp
    ))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tis-allow-stamp-p ()
  "Checks is stamping is allowed for this file"
  (let* (
	 (rex tis-docid-rex)		; copy global here
	 (login (or (user-login-name) "#$$#~!@"))
	 (re  tis-no-stamp-re)
	 stamp
	 )
   (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward rex  nil t))
	  nil				; skip, not found
	(beginning-of-line)
	(setq stamp
	      (buffer-substring
	       (point)
	       (progn (end-of-line) (point))))
	(if (not (and (stringp re) (stringp stamp)))
	    t				;go ahead and stamp!
	  (if (and (not (string-match login stamp))
		   (string-match re (buffer-file-name)))
	      nil t))
	)
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tis-docid()
  "Stamps rex tis-docid-rex if found. RET=t if stamped"
  ;; (interactive)
  (let* (
	 (case-fold-search tis-cs-rex)	; set our serch mode
	 (rex tis-docid-rex)		; copy global here
	 (compose tis-doc-func)		; makes the string
	 (ch tis-mch)			; the marker
	 ret stamp
	 )

    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward rex  nil t)) nil  ; skip, not found
	(if (null (tis-rex-end-check "docid")) nil ; skip, format invalid
	  ;;   we use zap instead of kill-line, because
	  ;;   kill can't handle empty spaces at the end of line...
	  (tis-ztc ch)			;up till this
	  (setq stamp (funcall compose))
	  (if (null (stringp stamp))
	      (tis-msg "docid compose error" t)
	    (insert stamp)
	    (setq ret t)))))
    ret))


;;; ----------------------------------------------------------------------
;;;
(defun tis-get-contact-str()
  "Composes contact string."
  (concat " " tis-contactid-info " "))



;;; ----------------------------------------------------------------------
;;;
(defun tis-contactid()
  "Stamps rex tis-contactid-rex if found. RET=t if stamped"
  ;; (interactive)
  (let (
	(case-fold-search tis-cs-rex)	; set our serch mode
	(rex tis-contactid-rex)		; copy global here
	(compose tis-contact-func)	; makes the string
	(ch tis-mch)
	ret stamp
	)
    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward rex  nil t))	nil; skip, not found
	(if (null (tis-rex-end-check "contactid")) nil ;skip, format invalid
	  ;; we use zap instead of kill-line, because
	  ;; kill can't handle empty spaces at the end of line...
	  (tis-ztc ch)			;up till this
	  (setq stamp (funcall compose))
	  (if (null (stringp stamp))
	      (tis-msg "Contact compose error" t)
	    (insert stamp)
	    (setq ret t)))))
    ret))




;;; ----------------------------------------------------------------------
;;;
(defun tis-save-buffer()
  "To help keeping track of documentation changes.
If you replace save-buffer function with this it'll stamp following
fields everytime you save your document. If they don't exist, it
behaves exactly like normal save-buffer.

  $Docid\:$ 		the date/time and full user name.
  $Contactid\:$		the tis-contactid-info string.

If there are multiple stamps, only the first one met, starting from
the beginning of file, will be touched. This approach has been chosen
so that it would take least time possible to do stamping before file
is saved.
  You might wonder why there is $ marks surrounding the words. The
format for revision control system 'GNU rcs' requires $ marks if you
run their program ident\(1\) over the file. Ident detects only rigidly
formatted Directives  so that any keyword of the following form can be
printed out.

  $keyword:<sapce>.......desc text<space>$

You cannot modify contents of these fields by hand, because function
kills old stampings and writes new ones every time it's called. It is
also recommended that you _put_ directive '@(#)' at the beginning
of these stamps; that way the UNIX 'what'-command can display these
fields.
  If there were any errors during stamping the user will be notified.
More information is available in tinystamp.el where this function
belongs.

References:
   tis-install
   tis-stamp-hook
   tis-stamp-str
"
  (interactive)
  (let (m 				;message
	(beg		(point-min-marker))
	(end		(point-max-marker))
	(end-max	(point-max))
	(wait		tis-sleep)
	(is-hooked	(tis-is-hook-in))
	(verb		tis-verbose)
	(stamp-check    tis-allow-stamp-func)
	end-wmax
	)

    (setq  tis-stamp-str nil)		;empty the global

    (run-hooks 'tis-stamp-in-hook)
    (unwind-protect
	(progn
	  (widen)
	  (setq end-wmax (point-max))

	  (if (null (funcall stamp-check)) ;stamping permitted ?
	      nil
	    (if (tis-docid) (setq m (concat m "doc")))
	    (if (tis-contactid) (setq m (concat m ", contact"))))
	  )
      (save-excursion
	(set-buffer (marker-buffer beg))
	;; what about after widen ? Were we in narrow mode ?
	(if (not (= end-wmax end-max))
	    (narrow-to-region beg end)))
      )

    ;;    Handle message displaying only if stamp was done
    (if (or is-hooked
	    (null m))
	nil		; skip, don't display message
      (message (concat "*stamped:" m ))
      (setq tis-stamp-str m)		; update global >>GLO<<
      (if (> wait 0) (sleep-for wait)) 	; delay for message
      )

    (run-hooks 'tis-stamp-out-hook)
    (if is-hooked
	nil				; gotta return NIL from hook func
      (save-buffer))
    ))

;;}}}


(provide 'tinystamp)
(run-hooks 'tis-load-hook)

;; ----------------------- end tinystamp.el ------------------------------
