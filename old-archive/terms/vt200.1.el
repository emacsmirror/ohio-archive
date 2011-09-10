;;; File:  vt200.el, v 2.0 (emacs 18[.58] version)
;;;
;;;               -------   -------------   ---------------------
;;;               F l o w   C o n t r o l   A d j u s t m e n t s
;;;               -------   -------------   ---------------------
;;;
;;;
;;; Copyright (C) 1993 Jeff Morgenthaler
;;; Copyright (C) 1985 Joe Kelsey
;;;

;; LCD Archive Entry:
;; vt200.el|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Support for all the function keys of the vt200 keyboard|
;; 22-Mar-1993|2.9|~/terms/vt200.el|


;; Archived at archive.cis.ohio-state.edu


;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  Send bug reports and suggestions for improvement to Jeff Morgenthaler
;;;  (jpmorgen@wisp4.physics.wisc.edu).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;; This is an improvement on the vt200.el that comes standard with the
;; emacs 18 distribution.  It is intended as a drop in replacement.
;; It works in concert with an improved version of keypad.el and
;; vt200-esc-fix.el (and optionally arrow-key-fix.el) also found in
;; this archive to provide full support for the vt200 keyboard.

;; Once installed in the term/ directory of your emacs distribution,
;; this file is loaded automatically when the terminal type is set to
;; vt200 (after .emacs and default.el have run.)  You can load it all
;; the time with the statement:

;; (load "term/vt200" nil t).  

;; This sets up an alist for keypad.el to translate into emacs
;; commands (see the documentation in keypad.el).  The only change
;; (other than documentation) that I've made is the function of the
;; "Insert Here" key.  I think that it should do a yank, the inverse
;; of the kill-region that "Remove" does.  With code in keypad.el, I
;; have also made the "Do" key mean M-x.  This makes that section of
;; function keys suitable for first time emacs users.

;; In keypad.el, I have provided a map of the function keys F12, F13,
;; and F14 to fill-paragraph, fill-region, and set-fill-prefix.  I
;; have done this more as an example than anything else: I don't think
;; that numbered function keys are very self explanatory.  I do use
;; these mappings, though, since on a real brain-dead vt200 with the
;; old vt200.el and no vt200-esc-fix.el, I had type F11-q, F11-g,
;; and "C-x ." for these functions (where F11 is ESC).  I use these
;; functions a lot and find it desirable to save the key strokes.  The
;; proximity of F12 to F11 made it easy to remember.  If you do fill
;; stuff a lot, you might find these useful too.  

;; I have purposefully left F6 through F10 unmapped so you can add
;; things there.  F1 through F5 are usually reserved for terminal
;; setup stuff.  If you want to add your own stuff or change the stuff
;; I've provided, use a variant of the following code in your .emacs
;; file (note, you no longer have to call enable-arrow-keys):

;; (setq term-setup-hook
;;      (function
;;       (lambda ()
;;	 (define-key CSI-map "21~" 'mh-rmail)         ; F10
;;	 ))
;; )

;; Users who like ESC-[ as backward-paragraph and don't need arrow
;; keys can be appeased by setting the variable no-arrow-keys in the
;; user's .emacs file or better yet, setting it in the site's
;; default.el file and using arrow-key-fix.el so the user can define
;; ESC-[ without ever knowing that it has anything to do with arrow
;; keys.  


(require 'keypad)

(defvar CSI-map nil
  "The CSI-map maps the CSI function keys on the VT200 keyboard.
The CSI keys are the dark function keys, and are only active in
VT200-mode, except for the arrow keys.")

(defun enable-arrow-keys ()
  "Enable the use of the VT200 arrow keys and dark function keys.
 Because of the nature of the VT200, this unavoidably breaks
 the standard Emacs command ESC \[; therefore, it is not done by default,
 but only if you give this command."
  (interactive)
  (global-set-key "\e[" CSI-map))

(if (not (boundp 'no-arrow-keys))
    (global-set-key "\e[" CSI-map))
;; I debated long and hard whether or not to do this and decided that
;; no one would grab this code and then not want it turned on.  If a
;; site has users who want backward-paragraph as ESC-[ instead of
;; arrow keys, they need only define the variable no-arrow-keys.
;; Alternately, the site manager may want to set no-arrow-keys in
;; default.el and use arrow-key-fix.el which allows the user to define
;; ESC [ to backward-paragraph directly.  


(if CSI-map
    nil
  (setq CSI-map (make-keymap))		; <ESC>[ commands
  (setup-terminal-keymap CSI-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("1~" . ?f)	   ; Find
	      ("2~" . ?0)	   ; Insert Here
	      ("3~" . ?k)	   ; Re-move
	      ("4~" . ?s)	   ; Select
	      ("5~" . ?P)	   ; Prev Screen
	      ("6~" . ?N)	   ; Next Screen
	      ("17~" . ?\C-f)	   ; F6
	      ("18~" . ?\C-g)	   ; F7
	      ("19~" . ?\C-h)	   ; F8
	      ("20~" . ?\C-i)	   ; F9
	      ("21~" . ?\C-j)	   ; F10
	      ("23~" . ESC-prefix) ; F11 (ESC)
	      ("24~" . ?\C-l)	   ; F12 (fill-paragraph)
	      ("25~" . ?\C-m)	   ; F13 (fill-region)
	      ("26~" . ?\C-n)	   ; F14 (set-fill-prefix)
	      ("31~" . ?\C-q)	   ; F17
	      ("32~" . ?\C-r)	   ; F18
	      ("33~" . ?\C-s)	   ; F19
	      ("34~" . ?\C-t)	   ; F20
	      ("28~" . ??)	   ; Help
	      ("29~" . ?x))))	   ; Do

(defvar SS3-map nil
  "SS3-map maps the SS3 function keys on the VT200 keyboard.
The SS3 keys are the numeric keypad keys in keypad application mode
\(DECKPAM).  SS3 is DEC's name for the sequence <ESC>O which is
the common prefix of what these keys transmit.")

(if SS3-map
    nil
  (setq SS3-map (make-keymap))		; <ESC>O commands
  (setup-terminal-keymap SS3-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("M" . ?e)	   ; Enter
	      ("P" . ?\C-a)	   ; PF1
	      ("Q" . ?\C-b)	   ; PF2
	      ("R" . ?\C-c)	   ; PF3
	      ("S" . ?\C-d)	   ; PF4
	      ("l" . ?,)	   ; ,
	      ("m" . ?-)	   ; -
	      ("n" . ?.)	   ; .
	      ("p" . ?0)	   ; 0
	      ("q" . ?1)	   ; 1
	      ("r" . ?2)	   ; 2
	      ("s" . ?3)	   ; 3
	      ("t" . ?4)	   ; 4
	      ("u" . ?5)	   ; 5
	      ("v" . ?6)	   ; 6
	      ("w" . ?7)	   ; 7
	      ("x" . ?8)	   ; 8
	      ("y" . ?9)))	   ; 9

     (define-key global-map "\eO" SS3-map))