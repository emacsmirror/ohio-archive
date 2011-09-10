;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!uunet!aai!leo Mon May 14 23:31:52 EDT 1990
;Article 1627 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!uunet!aai!leo
;From: leo%aai@uunet.uu.net
;Newsgroups: gnu.emacs
;Subject: Re: emacs on vt220 terminal
;Message-ID: <1990May9.021728.4030%aai@uunet.uu.net>
;Date: 9 May 90 02:17:28 GMT
;References: <8125@cadillac.CAD.MCC.COM> <KIM.90May7220218@kannel.lut.fi>
;Organization: Amerinex Artificial Intelligence
;Lines: 145
;
;kim@kannel.lut.fi (Kimmo Suominen) writes:
;
;>>>>>> On 7 May 90 13:51:29 GMT, kwan@cadillac.cad.mcc.com said:
;
;>Johnny> All other control keys seem to work but I cannot figure out where
;>Johnny> the Meta key is or how to compose one.
;
;>The META key is usually ESC on all terminals.  However, VT200 and VT300
;>-series terminals don't have ESC (unless you run it in VT52 mode).
;
;The escape key is also F11 in vt100 mode, not just vt52 mode.  I use the
;following in my .emacs file to make the ` key into the escape, and the F11 key
;into the ` when in vt100 mode:
;
;; swap the escape and ` keys when on a vt100 or vt200, enable arrow keys
;(if (or (equal (getenv "TERM") "vt200")
;	(equal (getenv "TERM") "vt100"))
;    (progn
;       (setq vt200-swap-esc-and-tick t)
;       (setq term-setup-hook 'enable-arrow-keys)))
;
;and here's the vt200.el to go along with it:

;; ...emacs/lisp/term/vt200.el
;; vt200 series terminal stuff.
;; April 1985, Joe Kelsey
;; January 1990, Skip Montanaro - swap ESC and `, add copyright hdr.

;; Copyright (C) 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(require 'keypad)

(defvar vt200-swap-esc-and-tick nil
  "If non-nil, the keyboard translation of the ESC and ` keys are swapped.")

(defvar CSI-map nil
  "The CSI-map maps the CSI function keys on the VT200 keyboard.
The CSI keys are the dark function keys, and are only active in
VT200-mode, except for the arrow keys.")

(defun enable-arrow-keys ()
  "Enable the use of the VT200 arrow keys and dark function keys.
Because of the nature of the VT200, this unavoidably breaks
the standard Emacs command ESC [; therefore, it is not done by default,
but only if you give this command."
  (interactive)
  (global-set-key "\e[" CSI-map))

;; I suggest that someone establish standard mappings for all of
;; the VT200 CSI function keys into the function-keymap.

(if CSI-map
    nil
  (setq CSI-map (make-keymap))		; <ESC>[ commands
  (setup-terminal-keymap CSI-map
	    '(("A" . ?u)	   ; up arrow
	      ("B" . ?d)	   ; down-arrow
	      ("C" . ?r)	   ; right-arrow
	      ("D" . ?l)	   ; left-arrow
	      ("1~" . ?f)	   ; Find
	      ("2~" . ?I)	   ; Insert Here
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
	      ("24~" . ?\C-l)	   ; F12
	      ("25~" . ?\C-m)	   ; F13
	      ("26~" . ?\C-n)	   ; F14
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

(if vt200-swap-esc-and-tick
  (let ((the-table (make-string 128 0)))
    (let ((i 0))
      (while (< i 128)
	(aset the-table i i)
	(setq i (1+ i))))
    ;; Swap ESC and `
    (aset the-table ?\e ?`)
    (aset the-table ?` ?\e)
    (setq keyboard-translate-table the-table)))
;-- 
;Leo	leo@aai.com   leo%aai@uunet.uu.net   ...uunet!aai!leo
