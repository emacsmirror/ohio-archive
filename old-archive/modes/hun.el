;;; hun.el - Convenient buffer-local minor mode for editing Hungarian documents
;;;
;;; Copyright (C) 1995 by Z. Kiraly (Eotvos University, Budapest)
;;;
;;; Hungarian accents = Magyar ikezetek
;;;
;;; Author: Zoltan Kiraly (kiraly@cs.elte.hu)
;;; Thanks to E. W. Kiss for a lot of help
;;;
;;; Created 30 March 1995
;;; Version 1.1 (first public release)
;;; 96-01-10
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1.1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.	If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; LCD Archive Entry:
;;; hun|Zoltan Kiraly|kiraly@cs.elte.hu|
;;; Convenient buffer-local minor mode for editing Hungarian documents|
;;; 10-Jan-1996|1.1|~/modes/hun.el.gz|
;;;
;;;-----------------------------------------------------------------------
;;;
;;; Description:
;;;
;;; Buffer-local minor mode for typing in Hungarian text.
;;; Choosable keyboard layout, accented keys can be typed in by one (usually
;;; unmodified) key. Two layouts (called English and Hungarian), can be toggled
;;; by one key, in each of them the other is accessible with Meta.
;;; Many (currently 37) predefined layouts you can choose from.
;;; Can handle overwrite mode, numeric args, i-search, 
;;; search&replace and upcase/downcase.
;;;
;;; Installation:
;;;
;;; Either put this file in your machines central site-lisp directory and the 
;;; following three lines in site-lisp/site-start.el (without ;;;) :
;;; (global-set-key [f8] 'accent-mode)
;;; (global-set-key [kp-f4] 'accent-mode)
;;; (autoload 'accent-mode "hun" "Allow editing Hungarian documents" t)
;;;
;;; or put this file in your home directory and put in your .emacs file
;;; the following three lines (without ;;;) :
;;; (global-set-key [f8] 'accent-mode)
;;; (global-set-key [kp-f4] 'accent-mode)
;;; (autoload 'accent-mode "~/hun" "Allow editing Hungarian documents" t)
;;;
;;;
;;; Usage:
;;; 
;;; turn on by: M-x accent-mode or with a hot-key (currently F8 or PF4). 
;;; Keyboard toggle between accented and normal 
;;; (called Hung end Eng sub-modes) by: 
;;; M-x toggle-hung-and-eng-modes or with a hot-key (currently F6 or PF4).
;;; turn off by: M-x accent-mode or with a hot-key (currently F8).
;;;
;;; In any of the two sub-modes the keys of the other submode also works, 
;;; but prefixed with Meta! 
;;; And the original Meta-key sequences which covered by this can be accessed
;;; with C-M-key.
;;;
;;; Choose a layout convinient for you (see Costumization below),
;;; and put the corresponding (setq my-favorite-keyboard-map ... line
;;; in your .emacs.
;;; Press F8 or PF4 when in the buffer which you want to edit
;;;     in accent mode!
;;;
;;;
;;;
;;; Costumization: 
;;;
;;; there are some keyboard layouts (called maps) which you can 
;;; choose from. Take your favorite (e.g. tex8) and put in your .emacs the
;;; following (without ;;;):
;;; (setq my-favorite-keyboard-map 'tex8)
;;; The possible layouts now: twr1 -- twr8, tex1 -- tex8, 
;;; twrzy1 -- twrzy8, texzy1 -- texzy8
;;; windows, windowszy, six-three, six-threezy and nil
;;;
;;; The layouts:
;;; in twr, tex and windows maps always
;;; a  capital letter is a shifted normal letter (except Z in tex8) and
;;;   a   is on  '            i   is on  ;          m   is on  `
;;; The tex? maps are essentially the same as the corresponding twr? maps 
;;; but the \ key is always free and { and } can be accessed as C-( and C-)
;;; 
;;;          twr1    twr2    twr3    twr4    twr5    twr6    twr7    twr8
;;;
;;;   s       =       =       =       /       /       \       /       /
;;;   v       0       /       0       -       -       -       [       [
;;;   u       [       [       [       [       [       [       -       -
;;;   z       ]       ]       \       ]       \       ]       \       \
;;;   |       -       -       -       =       =       =       ]       ]
;;;   {       \       \       ]       \       ]       /       =       =
;;; 
;;;          tex1    tex2    tex3    tex4    tex5    tex6    tex7    tex8 
;;;
;;;   s       =       =       =       /       /      M-o      /       /
;;;   v       0       /       0       -       -       -       [       [
;;;   u       [       [       [       [       [       [       -       -
;;;   z       ]       ]      M-u      ]      M-u      ]      M-u   M-. M-,
;;;   |       -       -       -       =       =       =       ]       ]
;;;   {       /      M-u      ]      M-u      ]       /       =       =
;;;
;;; windows map is identical with twr6 map.
;;; six-three is very special: a i m s z  are on  M-a M-e M-i M-o M-u
;;;   v   is on  [    u   is on  M-[    |   is on  ]     {   is on  M-]
;;;
;;; maps with zy in the name are identical to the ones without zy
;;; but y and z are interchanged.
;;;
;;; nil map translates nothing.
;;;
;;;
;;;
;;; Less important costumization: (examples)
;;;
;;; (setq my-favorite-accented-x-font "-*-fixed-medium-*-14-*-iso8859-1")
;;; (setq default-submode-of-accent-mode "eng")
;;; (setq my-favorite-eng-map 'nil-map)
;;; (define-key english-mode-map [f1] 'toggle-hung-and-eng-modes)
;;; (define-key hungarian-mode-map [f1] 'toggle-hung-and-eng-modes)
;;;
;;; Making your own keyboard map:
;;; Study the keyboard maps (e.g. defvar twr1-hung-map ...) below,
;;; and write your own maps called my-hung-map and my-eng-map in your .emacs!
;;; Then put/change the following line:
;;; (setq my-favorite-keyboard-map 'my)
;;;
;;;
;;; And the hooks you can use:
;;; accent-mode-hook, end-accent-mode-hook, 
;;; hungarian-mode-hook, english-mode-hook.
;;; 
;;;
;;; Known bugs:
;;; none
;;;
;;; Bugs:
;;; probably many
;;;
;;;
;;; Todo:
;;; Changing layouts in the middle of session (e.g. from twr1 to twr3)
;;; Sorting (?) (seems very hard except sorting lists which is not too useful)
;;; Choosable file encoding (at least between iso and dc)
;;; New simpler data structure for the maps
;;; Choosable display encoding
;;; I am really interested in simplification of any part of this code.
;;;
;;;
;;; History
;;;
;;; Revision 1.1 1996/1/10 17:06:42 kiraly
;;; Write in usage, description and costumization.
;;; Rename standard maps and put in many new ones, make the zy maps.
;;; Make costumization easier (my-favorite-keyboard-map) and 
;;; (setq my-favorite-keyboard-map 'my)
;;; Bug fixes in isearch.
;;;
;;; Revision 1.01 1995/8/31 17:30:05 kiraly
;;; Replacing explicit 8388608 with ?\M-\C-\@
;;;
;;; Revision 1.0 1995/4/6 20:20:13 kiraly
;;; First usable version - only for local usage



;;;-----------------------------------------------------------------------
;;;                         Variables

(defvar accent-mode nil
  "  Non-nil if using Accent mode as a minor mode.")
(make-variable-buffer-local 'accent-mode)
(put 'accent-mode 'permanent-local t)
(or (assq 'accent-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(accent-mode " Accent")))))

(defvar hungarian-mode nil
  "  Non-nil if using Hungarian keyboard mode as a minor mode.
  Works only if Accent minor mode is on.")
(make-variable-buffer-local 'hungarian-mode)
(put 'hungarian-mode 'permanent-local t)
(or (assq 'hungarian-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(hungarian-mode " Hung")))))

(defvar english-mode nil
  "  Non-nil if using English keyboard mode as a minor mode.
  Works only if Accent minor mode is on.")
(make-variable-buffer-local 'english-mode)
(put 'english-mode 'permanent-local t)
(or (assq 'english-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(english-mode " Eng")))))

(defvar hungarian-mode-in-isearch nil
  "  Non-nil if using Hungarian keyboard mode in isearch.
  Works only if Accent minor mode is on.")
(make-variable-buffer-local 'hungarian-mode-in-isearch)

(defvar english-mode-in-isearch nil
  "  Non-nil if using English keyboard mode in isearch.
  Works only if Accent minor mode is on.")
(make-variable-buffer-local 'english-mode-in-isearch)


(defvar hungarian-display nil
 "  Indicate whether Hungarian display is on"
)

(defvar hungarian-mode-map nil
  "  Keyboard map for Hungarian mode")
(if hungarian-mode-map
    nil
  (setq hungarian-mode-map (make-sparse-keymap)))

(or (assq 'hungarian-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'hungarian-mode hungarian-mode-map)
		minor-mode-map-alist)))

(defvar english-mode-map nil
  "  Keyboard map for English mode")
(if english-mode-map
    nil
  (setq english-mode-map (make-sparse-keymap)))

(or (assq 'english-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'english-mode english-mode-map)
		minor-mode-map-alist)))

(defvar hung-translation-map nil
"  Translation map for Hungarian keyboard"
)
(make-variable-buffer-local 'hung-translation-map)

(defvar eng-translation-map nil
"  Translation map for English keyboard but accented chars with Meta"
)
(make-variable-buffer-local 'eng-translation-map)

(defvar current-translation-map nil
"  Current translation map for Hungarian/English keyboard" 
)
(make-variable-buffer-local 'current-translation-map)

(defvar current-translation-map-in-isearch nil
"  Current translation map for Hungarian/English keyboard
  when in isearch" 
)
(make-variable-buffer-local 'current-translation-map-in-isearch)

(defvar isearch-mode-map-@ nil
"  Map for saving the original values of isearch-mode-map
  in order to restore that map in isearch-mode-end-hook"
)
(make-variable-buffer-local 'isearch-mode-map-@)


;;;-----------------------------------------------------------------------
;;;                         Maps
;;;
;;;           Hint: read this part in accent-mode!


;;;;;;;;;;;;;;;;;;;;;;;;;;; Twriter style maps ;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar twr1-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-0 . ?0) (?\M-\) . ?\)) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\) (?\C-\) . ?\)) (?\C-\( . ?\()
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?0 . ?v) (?\) . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\- . ?|) (?\_ . ?\) (?\\ . ?{) (?\| . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style first map (v on 0 { on \\)"
)

(defvar twr1-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-0 . ?v) (?\M-\) . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-\\ . ?{) (?\M-\| . ?[)
(?\C-\\ . ?\\) (?\C-\) . ?\)) (?\C-\( . ?\()
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style first map"
)

(defvar twr2-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\` . ?\`) (?\M-\/ . ?\/) (?\M-\? . ?\?)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) (?\C-\\ . ?\\)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?\/ . ?v) (?\? . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\- . ?|) (?\_ . ?\) (?\\ . ?{) (?\| . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style second map (v on 0 { on \\)"
)

(defvar twr2-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-\/ . ?v) (?\M-\? . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-\\ . ?{) (?\M-\| . ?[)
(?\C-\\ . ?\\)
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style second map"
)

(defvar twr3-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-0 . ?0) (?\M-\) . ?\)) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\) (?\C-\) . ?\)) (?\C-\( . ?\()
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?0 . ?v) (?\) . ?V) (?\[ . ?u) (?\{ . ?U)
(?\\ . ?z) (?\| . ?Z) (?\- . ?|) (?\_ . ?\) (?\] . ?{) (?\} . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style third map (v on 0 z on \\ { on ])"
)

(defvar twr3-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-0 . ?v) (?\M-\) . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\\ . ?z) (?\M-\| . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
(?\C-\\ . ?\\) (?\C-\) . ?\)) (?\C-\( . ?\()
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style third map"
)

(defvar twr4-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\= . ?|) (?\+ . ?\) (?\\ . ?{) (?\| . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style fourth map (s on / v on - | on =)"
)

(defvar twr4-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-\\ . ?{) (?\M-\| . ?[)
(?\C-\\ . ?\\)
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style fourth map"
)

(defvar twr5-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\\ . ?z) (?\| . ?Z) (?\= . ?|) (?\+ . ?\) (?\] . ?{) (?\} . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style fifth map (s on / v on - | on = z on \\ { on ])"
)

(defvar twr5-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\\ . ?z) (?\M-\| . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
(?\C-\\ . ?\\)
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style fifth map"
)

(defvar twr6-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\\ . ?s) (?\| . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\= . ?|) (?\+ . ?\) (?\/ . ?{) (?\? . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style sixth map (s on \\ v on - | on = { on /)"
)

(defvar twr6-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\\ . ?s) (?\M-\| . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-\/ . ?{) (?\M-\? . ?[)
(?\C-\\ . ?\\)
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style sixth map"
)

(defvar windows-hung-map
twr6-hung-map
"  Translation map for Hungarian keyboard --
  Windows-style map (s on \\ v on - | on = { on /)"
)

(defvar windows-eng-map
twr6-eng-map
"  Translation map for English keyboard but accented chars with Meta --
  Windows-style map"
)

(defvar twr7-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\| . ?\|) (?\M-\\ . ?\\) 
(?\C-\\ . ?\\)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\[ . ?v) (?\{ . ?V) (?\- . ?u) (?\_ . ?U)
(?\\ . ?z) (?\| . ?Z) (?\] . ?|) (?\} . ?\) (?\= . ?{) (?\+ . ?[)
)
"  Translation map for Hungarian keyboard --
  Twriter-style seventh map (s on / v on [ u on - z on \\ | on = { on ])"
)

(defvar twr8-hung-map 
twr7-hung-map
"  Translation map for Hungarian keyboard --
  Twriter-style eighth map (s on / v on [ u on - z on \\ | on = { on ])"
)

(defvar twr7-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\[ . ?v) (?\M-\{ . ?V) (?\M-\- . ?u) (?\M-\_ . ?U)
(?\M-\\ . ?z) (?\M-\| . ?Z) (?\M-\] . ?|) (?\M-\} . ?\) (?\M-\= . ?{) (?\M-\+ . ?[)
(?\C-\\ . ?\\)
)
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style seventh map"
)

(defvar twr8-eng-map 
twr7-eng-map
"  Translation map for English keyboard but accented chars with Meta --
  Twriter-style eighth map"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;; TeX style maps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tex1-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-0 . ?0) (?\M-\) . ?\)) (?\M-\` . ?\`)
(?\M-\~ . ?\~) (?\M-\/ . ?\/) (?\M-\? . ?\?)
(?\C-\) . ?\)) (?\C-\( . ?\()
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?0 . ?v) (?\) . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\- . ?|) (?\_ . ?\) (?\/ . ?{) (?\? . ?[)
)
"  Translation map for Hungarian keyboard --
  TeX-style first map (v on 0 { on /)"
)

(defvar tex1-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-0 . ?v) (?\M-\) . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-\/ . ?{) (?\M-\? . ?[)
(?\C-\) . ?\)) (?\C-\( . ?\()
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style first map"
)

(defvar tex2-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\` . ?\`) (?\M-\/ . ?\/) (?\M-\? . ?\?)
(?\M-\~ . ?\~)
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?\/ . ?v) (?\? . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\- . ?|) (?\_ . ?\) (?\M-u . ?{) (?\M-U . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style second map (v on 0 { on M-u)"
)

(defvar tex2-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-\/ . ?v) (?\M-\? . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-u . ?{) (?\M-U . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style second map"
)

(defvar tex3-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-0 . ?0) (?\M-\) . ?\)) (?\M-\` . ?\`)
(?\M-\~ . ?\~)
(?\C-\) . ?\)) (?\C-\( . ?\()
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\= . ?s) (?\+ . ?S) (?0 . ?v) (?\) . ?V) (?\[ . ?u) (?\{ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\- . ?|) (?\_ . ?\) (?\] . ?{) (?\} . ?[)
)
"  Translation map for Hungarian keyboard --
  TeX-style third map (v on 0 z on M-u { on ])"
)

(defvar tex3-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\= . ?s) (?\M-\+ . ?S) (?\M-0 . ?v) (?\M-\) . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\M-\- . ?|) (?\M-\_ . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
(?\C-\) . ?\)) (?\C-\( . ?\()
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style third map"
)

(defvar tex4-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) 
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\= . ?|) (?\+ . ?\) (?\M-u . ?{) (?\M-U . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style fourth map (s on / v on - | on = { on M-u)"
)

(defvar tex4-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-u . ?{) (?\M-U . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style fourth map"
)

(defvar tex5-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) 
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\= . ?|) (?\+ . ?\) (?\] . ?{) (?\} . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style fifth map (s on / v on - | on = z on M-u { on ])"
)

(defvar tex5-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style fifth map"
)

(defvar tex6-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) 
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\M-o . ?s) (?\M-O . ?S) (?\- . ?v) (?\_ . ?V) (?\[ . ?u) (?\{ . ?U)
(?\] . ?z) (?\} . ?Z) (?\= . ?|) (?\+ . ?\) (?\/ . ?{) (?\? . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style sixth map (s on M-o v on - | on = { on /)"
)

(defvar tex6-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-o . ?s) (?\M-O . ?S) (?\M-\- . ?v) (?\M-\_ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\] . ?z) (?\M-\} . ?Z) (?\M-\= . ?|) (?\M-\+ . ?\) (?\M-\/ . ?{) (?\M-\? . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style sixth map"
)

(defvar tex7-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) 
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\[ . ?v) (?\{ . ?V) (?\- . ?u) (?\_ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\] . ?|) (?\} . ?\) (?\= . ?{) (?\+ . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style seventh map (s on / v on [ u on - z on M-u | on = { on ])"
)

(defvar tex7-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\[ . ?v) (?\M-\{ . ?V) (?\M-\- . ?u) (?\M-\_ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\M-\] . ?|) (?\M-\} . ?\) (?\M-\= . ?{) (?\M-\+ . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style seventh map"
)

(defvar tex8-hung-map 
'(
(?\M-\- . ?\-) (?\M-\_ . ?\_) (?\M-\= . ?\=) (?\M-\+ . ?\+) (?\M-\[ . ?\[)
(?\M-\{ . ?\{) (?\M-\] . ?\]) (?\M-\} . ?\}) (?\M-\' . ?\') (?\M-\" . ?\")
(?\M-\; . ?\;) (?\M-\: . ?\:) (?\M-\/ . ?\/) (?\M-\? . ?\?) (?\M-\` . ?\`)
(?\M-\~ . ?\~) 
(?\' . ?a) (?\" . ?A)  (?\; . ?i) (?\: . ?I) (?\` . ?m) (?\~ . ?M)
(?\/ . ?s) (?\? . ?S) (?\[ . ?v) (?\{ . ?V) (?\- . ?u) (?\_ . ?U)
(?\M-\. . ?z) (?\M-\, . ?Z) (?\] . ?|) (?\} . ?\) (?\= . ?{) (?\+ . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for Hungarian keyboard --
  TeX-style eighth map (s on / v on [ u on - z on M-. Z on M-, | on = { on ])"
)

(defvar tex8-eng-map 
'(
(?\M-\' . ?a) (?\M-\" . ?A) (?\M-\; . ?i) (?\M-\: . ?I) (?\M-\` . ?m) (?\M-\~ . ?M)
(?\M-\/ . ?s) (?\M-\? . ?S) (?\M-\[ . ?v) (?\M-\{ . ?V) (?\M-\- . ?u) (?\M-\_ . ?U)
(?\M-\. . ?z) (?\M-\, . ?Z) (?\M-\] . ?|) (?\M-\} . ?\) (?\M-\= . ?{) (?\M-\+ . ?[)
(?\C-\( . ?\{) (?\C-\) . ?\})
)
"  Translation map for English keyboard but accented chars with Meta --
  TeX-style eighth map"
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;Special maps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar six-three-hung-map 
'(
(?\M-\C-\[ . ?\[) (?\M-\C-\{ . ?\{) (?\M-\C-\] . ?\]) (?\M-\C-\} . ?\})
(?\M-\C-a . ?\M-a) (?\M-\C-e . ?\M-e) (?\M-\C-i . ?\M-i) (?\M-\C-o . ?\M-o) 
(?\M-\C-u . ?\M-u) 
(?\M-a . ?a) (?\M-A . ?A)  (?\M-e . ?i) (?\M-E . ?I) (?\M-i . ?m) (?\M-I . ?M)
(?\M-o . ?s) (?\M-O . ?S) (?\[ . ?v) (?\{ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-u . ?z) (?\M-U . ?Z) (?\] . ?|) (?\} . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
)
"  Translation map for Hungarian keyboard --
  6:3-style map (a on M-a ... z on M-u v on [ u on M-[ | on ] { on M-])"
)

(defvar six-three-eng-map 
'(
(?\M-\C-a . ?a) (?\M-\C-A . ?A)  (?\M-\C-e . ?i) (?\M-\C-E . ?I) (?\M-\C-i . ?m) (?\M-\C-I . ?M)
(?\M-\C-o . ?s) (?\M-\C-O . ?S) (?\M-\C-\[ . ?v) (?\M-\C-\{ . ?V) (?\M-\[ . ?u) (?\M-\{ . ?U)
(?\M-\C-u . ?z) (?\M-\C-U . ?Z) (?\M-\C-\] . ?|) (?\M-\C-\} . ?\) (?\M-\] . ?{) (?\M-\} . ?[)
)
"  Translation map for English keyboard but accented chars with Meta-Control --
  6:3-style map"
)

(defvar nil-map nil
"  Translate nothing -- can be used in my-favorite-eng-map"
)

(defvar my-hung-map nil
"  Write your own translation map for Hungarian sub-mode into this variable!"
)
(defvar my-eng-map nil
"  Write your own translation map for English sub-mode into this variable!"
)

(defvar hung-maps
'(
(twr1 . twr1-hung-map) (twr2 . twr2-hung-map) (twr3 . twr3-hung-map) (twr4 . twr4-hung-map) (twr5 . twr5-hung-map) (twr6 . twr6-hung-map) (twr7 . twr7-hung-map) (twr8 . twr8-hung-map) (twrzy1 . twrzy1-hung-map) (twrzy2 . twrzy2-hung-map) (twrzy3 . twrzy3-hung-map) (twrzy4 . twrzy4-hung-map) (twrzy5 . twrzy5-hung-map) (twrzy6 . twrzy6-hung-map) (twrzy7 . twrzy7-hung-map)  (twrzy8 . twrzy8-hung-map)
(tex1 . tex1-hung-map) (tex2 . tex2-hung-map) (tex3 . tex3-hung-map) (tex4 . tex4-hung-map) (tex5 . tex5-hung-map) (tex6 . tex6-hung-map) (tex7 . tex7-hung-map) (tex8 . tex8-hung-map) (texzy1 . texzy1-hung-map) (texzy2 . texzy2-hung-map) (texzy3 . texzy3-hung-map) (texzy4 . texzy4-hung-map) (texzy5 . texzy5-hung-map) (texzy6 . texzy6-hung-map) (texzy7 . texzy7-hung-map) (texzy8 . texzy8-hung-map)
(windows . windows-hung-map) (windowszy . windowszy-hung-map) (six-three . six-three-hung-map) (six-threezy . six-threezy-hung-map) (my . my-hung-map)
)
"  The alist of all hung-maps"
)

(defvar eng-maps
'(
(twr1 . twr1-eng-map) (twr2 . twr2-eng-map) (twr3 . twr3-eng-map) (twr4 . twr4-eng-map) (twr5 . twr5-eng-map) (twr6 . twr6-eng-map) (twr7 . twr7-eng-map) (twr8 . twr8-eng-map) 
(twrzy1 . twrzy1-eng-map) (twrzy2 . twrzy2-eng-map) (twrzy3 . twrzy3-eng-map) (twrzy4 . twrzy4-eng-map) (twrzy5 . twrzy5-eng-map) (twrzy6 . twrzy6-eng-map) (twrzy7 . twrzy7-eng-map)  (twrzy8 . twrzy8-eng-map)
(tex1 . tex1-eng-map) (tex2 . tex2-eng-map) (tex3 . tex3-eng-map) (tex4 . tex4-eng-map) (tex5 . tex5-eng-map) (tex6 . tex6-eng-map) (tex7 . tex7-eng-map) (tex8 . tex8-eng-map) 
(texzy1 . texzy1-eng-map) (texzy2 . texzy2-eng-map) (texzy3 . texzy3-eng-map) (texzy4 . texzy4-eng-map) (texzy5 . texzy5-eng-map) (texzy6 . texzy6-eng-map) (texzy7 . texzy7-eng-map) (texzy8 . texzy8-eng-map)
(windows . windows-eng-map) (windowszy . windowszy-eng-map) (six-three . six-three-eng-map) (six-threezy . six-threezy-eng-map) (my . my-eng-map)
)
"  The alist of all eng-maps"
)

;;;;;;;;;;;;;;;;;;;;;;;;; Generating zy maps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar zy-maps
'(
(twr1-eng-map . twrzy1-eng-map) (twr2-eng-map . twrzy2-eng-map) (twr3-eng-map . twrzy3-eng-map) (twr4-eng-map . twrzy4-eng-map) (twr5-eng-map . twrzy5-eng-map) (twr6-eng-map . twrzy6-eng-map) (twr7-eng-map . twrzy7-eng-map) (twr8-eng-map . twrzy8-eng-map)
(tex1-eng-map . texzy1-eng-map) (tex2-eng-map . texzy2-eng-map) (tex3-eng-map . texzy3-eng-map) (tex4-eng-map . texzy4-eng-map) (tex5-eng-map . texzy5-eng-map) (tex6-eng-map . texzy6-eng-map) (tex7-eng-map . texzy7-eng-map) (tex8-eng-map . texzy8-eng-map)
(windows-eng-map . windowszy-eng-map) (six-three-eng-map . six-threezy-eng-map)
(twr1-hung-map . twrzy1-hung-map) (twr2-hung-map . twrzy2-hung-map) (twr3-hung-map . twrzy3-hung-map) (twr4-hung-map . twrzy4-hung-map) (twr5-hung-map . twrzy5-hung-map) (twr6-hung-map . twrzy6-hung-map) (twr7-hung-map . twrzy7-hung-map) (twr8-hung-map . twrzy8-hung-map)
(tex1-hung-map . texzy1-hung-map) (tex2-hung-map . texzy2-hung-map) (tex3-hung-map . texzy3-hung-map) (tex4-hung-map . texzy4-hung-map) (tex5-hung-map . texzy5-hung-map) (tex6-hung-map . texzy6-hung-map) (tex7-hung-map . texzy7-hung-map) (tex8-hung-map . texzy8-hung-map)
(windows-hung-map . windowszy-hung-map) (six-three-hung-map . six-threezy-hung-map)
(nil-map . nilzy-map)
)
"  The alist of all map - zy-map pairs"
)


(progn (let ((zym zy-maps))
 (while zym
  (progn (let ((normap (car (car zym))) (zymap (cdr (car zym))))
    (set  zymap (append (symbol-value normap) '(
(?z . ?y) (?Z . ?Y) (?y . ?z) (?Y . ?Z))))
    (setq zym (cdr zym))
 )))
))


;;; This just a reminder for myself:
;;; Ezenkivul: kellenek change-style jellegu commandok, mivel 
;;; my-favorite...-map erteket nem eleg atallitani, kell hozza meg
;;; a define-key-k atallitasa es a restore-map ujrageneralasa
;;; Minden map-et jol kell dokumentalni es a change-style-okat menube rakni
;;; Csunyan meg lehetne oldani accent-mode kikapcs. my-fav-hung/eng-map-ek 
;;; atallitasa es accent-mode visszakapcs. segitsegevel.


;;;-----------------------------------------------------------------------
;;;                 Costumization
(defvar my-favorite-keyboard-map
'twr1
"*  Which keyboard mapping I want to use when in Accent mode
  Possibilities: 'twr1 ... 'twr8, 'tex1 ... 'tex8
                 'twrzy1 ... 'twrzy8, 'texzy1 ... 'texzy8
                 'windows 'windowszy 'six-three 'six-threezy and 'nil"
)
 


(defvar my-favorite-hung-map nil
"*  Which keyboard mapping I want to use when in Hungarian minor mode"
)

(defvar my-favorite-eng-map nil
"*  Which keyboard mapping I want to use when in English minor mode"
)

(defvar default-submode-of-accent-mode "hung"
"*  This variable holds a string which is either \"hung\" or \"eng\"
  When turning on the accent-mode this keyboard mapping will be the default
  from the actual hung-translation-map and eng-translation-map.
  Later this can be toggled by F6 or PF4."
)

(defvar my-favorite-accented-x-font nil
"  If non-nil and we are in X windows then the default font will be changed
  to the string-value of this var when entering accent-mode first."
)



;;;--------------------------------------------------------------------
;;;                  Auxiliary functions

(defun translate-and-self-insert (arg)
 "Translate the character typed using current-translation-map and call
  self-insert-command with the result"
(interactive "P")
(setq mapped-last-command-event last-command-event)
(setq lastev last-command-event)
(setq lll (length (this-command-keys)))
  (if (> lll 1)
    (if (or (eq (aref (this-command-keys) (- lll 2)) 27)
	    (eq (aref (this-command-keys) (- lll 2)) 'escape))
      (setq lastev (+ lastev ?\M-\C-\@))
  ))
(if (assoc lastev current-translation-map)
  (setq mapped-last-command-event 
  (cdr (assoc lastev current-translation-map)))
)
(setq last-command-event mapped-last-command-event)
(self-insert-command (prefix-numeric-value arg))
)

(defun translate-and-isearch-process-search-char (arg)
 "  Translate the character typed in isearch-mode 
  using current-translation-map-in-isearch and call
  isearch-process-search-char with the result"
(interactive "P")
(setq mapped-last-command-event last-command-event)
(setq lastev last-command-event)
(setq lll (length (this-command-keys)))
  (if (> lll 1)
    (if (or (eq (aref (this-command-keys) (- lll 2)) 27)
	    (eq (aref (this-command-keys) (- lll 2)) 'escape))
      (setq lastev (+ lastev ?\M-\C-\@))
  ))
(if (assoc lastev current-translation-map-in-isearch)
  (setq mapped-last-command-event 
  (cdr (assoc lastev current-translation-map-in-isearch)))
)
(setq last-command-event mapped-last-command-event)
(isearch-process-search-char last-command-event)
)

(defun toggle-hung-and-eng-modes (arg)
  "  Toggle between Hungarian and English minor modes if Accent mode is on."
(interactive "P")
(if accent-mode
(if hungarian-mode (english-mode) (hungarian-mode)))
)

(defun toggle-hung-and-eng-modes-in-isearch (arg)
  "  Toggle between Hungarian and English minor modes 
  inside current isearcg if Accent mode is on."
(interactive "P")
(if (and accent-mode isearch-mode)
(if hungarian-mode-in-isearch 
    (progn
      (setq hungarian-mode-in-isearch nil)
      (setq english-mode-in-isearch t)
      (setq current-translation-map-in-isearch eng-translation-map)
      (if (keymapp isearch-mode-map-@) (setq isearch-mode-map (copy-keymap isearch-mode-map-@)))
      (define-keyboard-map eng-translation-map isearch-mode-map 
	              'translate-and-isearch-process-search-char)
      (if (equal (cdr (assoc ?z eng-translation-map)) ?y)
	  (interchange-control-z-y isearch-mode-map))
      (define-key isearch-mode-map [f6] 'toggle-hung-and-eng-modes-in-isearch)
      (define-key isearch-mode-map [kp-f4] 'toggle-hung-and-eng-modes-in-isearch))
  (setq hungarian-mode-in-isearch t)
  (setq english-mode-in-isearch nil)
  (setq current-translation-map-in-isearch hung-translation-map)
  (if (keymapp isearch-mode-map-@) (setq isearch-mode-map (copy-keymap isearch-mode-map-@)))
  (define-keyboard-map hung-translation-map isearch-mode-map 
	              'translate-and-isearch-process-search-char)
  (if (equal (cdr (assoc ?z hung-translation-map)) ?y)
      (interchange-control-z-y isearch-mode-map))
  (define-key isearch-mode-map [f6] 'toggle-hung-and-eng-modes-in-isearch)
  (define-key isearch-mode-map [kp-f4] 'toggle-hung-and-eng-modes-in-isearch)
)))




(defun use-hungarian-and-english-minor-modes-in-minibuffer ()
  "  Enable Hungarian or English minor modes in minibuffer
   if one of them is on in the buffer from which we invoked the minibuffer and
   if we are doing search or replace"
(interactive)
(if (or (string-match "[sS]earch" (minibuffer-prompt)) 
        (string-match "[rR]eplace" (minibuffer-prompt)))
  (progn
    (save-excursion
     (set-buffer (other-buffer t t))
     (setq hungarian-mode-@ hungarian-mode)
     (setq english-mode-@ english-mode)
     (setq accent-mode-@ accent-mode)
     (setq hung-translation-map-@ hung-translation-map)
     (setq eng-translation-map-@ eng-translation-map)
     (setq current-translation-map-@ current-translation-map)
    )
    (setq hungarian-mode hungarian-mode-@)
    (setq english-mode english-mode-@)
    (setq accent-mode accent-mode-@)
    (setq hung-translation-map hung-translation-map-@)
    (setq eng-translation-map eng-translation-map-@)
    (setq current-translation-map current-translation-map-@)
))
)

(defun redefine-keys-as-hungarian-in-isearch-mode-map ()
  "Redefine keys used in Hungarian or English minor modes in isearch 
   mode"
(interactive)
     (if hungarian-mode 
          (progn
	     (setq hungarian-mode-in-isearch t)
	     (setq current-translation-map-in-isearch hung-translation-map)
             (setq isearch-mode-map-@ (copy-keymap isearch-mode-map)) 
             (define-keyboard-map hung-translation-map isearch-mode-map 
                                 'translate-and-isearch-process-search-char)
	     (if (equal (cdr (assoc ?z hung-translation-map)) ?y)
		 (interchange-control-z-y isearch-mode-map))
             (define-key isearch-mode-map [f6] 'toggle-hung-and-eng-modes-in-isearch)
             (define-key isearch-mode-map [kp-f4] 'toggle-hung-and-eng-modes-in-isearch))
     )
     (if english-mode
          (progn
	     (setq english-mode-in-isearch t)
	     (setq current-translation-map-in-isearch eng-translation-map)
             (setq isearch-mode-map-@ (copy-keymap isearch-mode-map))
             (define-keyboard-map eng-translation-map isearch-mode-map 
                                 'translate-and-isearch-process-search-char)
	     (if (equal (cdr (assoc ?z eng-translation-map)) ?y)
		 (interchange-control-z-y isearch-mode-map))
             (define-key isearch-mode-map [f6] 'toggle-hung-and-eng-modes-in-isearch)
             (define-key isearch-mode-map [kp-f4] 'toggle-hung-and-eng-modes-in-isearch))
     )
)

(defun restore-isearch-mode-map-keys ()
  "  Restore keys from isearch-mode-map-@ if exists"
(if (keymapp isearch-mode-map-@) (setq isearch-mode-map isearch-mode-map-@))
(setq isearch-mode-map-@ nil)
)

(defun interchange-control-z-y (sub-mode-map)
  "  Interchange the roles of control- meta- etc. z and y"
   (let ((kl '(?\C-z ?\M-z ?\M-Z ?\M-\C-z ?\C-y ?\M-y ?\M-Y ?\M-\C-y))
         (rl '("\C-y" "\M-y" "\M-Y" "\M-\C-y" "\C-z" "\M-z" "\M-Z" "\M-\C-z")))
        (while kl
            (progn
               (let ((bdg (key-binding (car rl))))
                  (if bdg 
                       (define-keyboard-map (list (cons (car kl) (car kl)))
                                            sub-mode-map bdg)))
            (setq kl (cdr kl))
            (setq rl (cdr rl))))
))

(defun standard-display-hungarian ()
 "Display Hungarian characters"
  (interactive)
(if (not hungarian-display)
(progn
(setq hungarian-display t)  
(require 'case-table)
  (let ((downcase (concat (car (standard-case-table)))))
  (set-case-syntax-pair 193 225 downcase) ; \'a
  (set-case-syntax-pair 201 233 downcase) ; \'e
  (set-case-syntax-pair 205 237 downcase) ; \'\i
  (set-case-syntax-pair 211 243 downcase) ; \'o
  (set-case-syntax-pair 214 246 downcase) ; \"o
  (set-case-syntax-pair 213 245 downcase) ; \H o
  (set-case-syntax-pair 218 250 downcase) ; \'u
  (set-case-syntax-pair 220 252 downcase) ; \"u
  (set-case-syntax-pair 219 251 downcase) ; \H u
  (set-standard-case-table (list downcase nil nil nil)))
(set-case-table (standard-case-table))
(require 'disp-table)
  (standard-display-8bit 225 225)
  (standard-display-8bit 193 193)
  (standard-display-8bit 233 233)
  (standard-display-8bit 201 201)
  (standard-display-8bit 237 237)
  (standard-display-8bit 205 205)
  (standard-display-8bit 243 243)
  (standard-display-8bit 211 211)
  (standard-display-8bit 246 246)
  (standard-display-8bit 214 214)
  (standard-display-8bit 245 245)
  (standard-display-8bit 213 213)
  (standard-display-8bit 250 250)
  (standard-display-8bit 218 218)
  (standard-display-8bit 252 252)
  (standard-display-8bit 220 220)
  (standard-display-8bit 251 251)
  (standard-display-8bit 219 219)
(if (and my-favorite-accented-x-font (string-equal window-system "x")) 
  (set-default-font my-favorite-accented-x-font)
)
(redraw-display)
)))




;;;-----------------------------------------------------------------------
;;;              Keyboard-maps


;;;(defun define-hung-map (map &optional comm)
;;;  "  Define Hungarian keyboard map in keymap map 
;;;   If optional argument comm is given use it instead of 
;;;   the default 'translate-and-self-insert command"
;;;(if (not comm) (setq comm 'translate-and-self-insert))
;;;(progn (let ((htm hung-translation-map))
;;; (while htm
;;;  (progn
;;;    (define-key map (vector (car (car htm))) comm)
;;;    (setq htm (cdr htm))
;;; ))
;;;))
;;;)

;;;(defun define-eng-map (map &optional comm)
;;;  "  Define back English keyboard map in keymap map
;;;   but keep reaching accented letters by Meta 
;;;   If optional argument comm is given use it instead of 
;;;   the default 'translate-and-self-insert command"
;;;(if (not comm) (setq comm 'translate-and-self-insert))
;;;(progn (let ((etm eng-translation-map))
;;; (while etm
;;;  (progn
;;;    (define-key map (vector (car (car etm))) comm)
;;;    (setq etm (cdr etm))
;;; ))
;;;))
;;;)

(defun define-keyboard-map (translation-map map &optional comm)
  "  Define the keys in the given ``translation-map'' acting as
   translate-and-self-insert command in keymap ``map''.
   If optional argument comm is given use it instead of 
   the default 'translate-and-self-insert command"
(if (not comm) (setq comm 'translate-and-self-insert))
(progn (let ((tm translation-map))
 (while tm
  (progn
    (define-key map (vector (car (car tm))) comm)
    (setq tm (cdr tm))
 ))
))
)


;;;---------------------------------------------------------------
;;;                     Main part


(defun hungarian-mode ()
 "Set the keyboard for typing Hungarian letters
  The original keys can be reached by prefixing them by Meta"
  (interactive)
(if accent-mode 
 (progn
  (setq hungarian-mode t)
  (setq english-mode nil)
  (setq current-translation-map hung-translation-map)
  (run-hooks 'hungarian-mode-hook)
  (force-mode-line-update)
 )
 (error "Hungarian keyboard minor mode can be used only when Accent minor mode is on")
))


(defun english-mode ()
 "Set the keyboard back to the original setting,
  but allow using Hungarian letters by prefixing them by Meta"
  (interactive)
(if accent-mode 
 (progn
  (setq english-mode t)
  (setq hungarian-mode nil)
  (setq current-translation-map eng-translation-map)
  (run-hooks 'english-mode-hook)
  (force-mode-line-update)
 )
  (error "English keyboard minor mode can be used only when Accent minor mode is on")
))

;;;###autoload
(defun accent-mode (arg)
  "Toggle accent minor-mode.
  With arg, turn accent mode on iff arg is positive.
  Allow displaying and typing Hungarian characters and upcase/lowcase them."
  (interactive "P")
(setq accent-mode
	(if (if (null arg) (not accent-mode)
	      (> (prefix-numeric-value arg) 0))
           t)
)
(if accent-mode
(progn
(if (not hungarian-display) 
(standard-display-hungarian))

(if (not my-favorite-hung-map)
  (setq my-favorite-hung-map (cdr (assoc my-favorite-keyboard-map hung-maps)))
)  
(if (not my-favorite-eng-map)
  (setq my-favorite-eng-map (cdr (assoc my-favorite-keyboard-map eng-maps)))
)  

(if hung-translation-map 
  nil
  (if (listp (symbol-value my-favorite-hung-map))
    (setq hung-translation-map (symbol-value my-favorite-hung-map))
    (setq hung-translation-map twr1-hung-map)
    (error "my-favorite-hung-map must contain a name of an association list")
  )
)

(if eng-translation-map 
  nil
  (if (listp (symbol-value my-favorite-eng-map))
    (setq eng-translation-map (symbol-value my-favorite-eng-map))
    (setq eng-translation-map twr1-eng-map)
    (error "my-favorite-eng-map must contain a name of an association list")
  )
)
(define-keyboard-map hung-translation-map hungarian-mode-map)
(if (equal (cdr (assoc ?z hung-translation-map)) ?y)
    (interchange-control-z-y hungarian-mode-map))
(define-keyboard-map eng-translation-map english-mode-map)
(if (equal (cdr (assoc ?z eng-translation-map)) ?y)
    (interchange-control-z-y english-mode-map))
(define-key english-mode-map [f6] 'toggle-hung-and-eng-modes)
(define-key english-mode-map [kp-f4] 'toggle-hung-and-eng-modes)
(define-key hungarian-mode-map [f6] 'toggle-hung-and-eng-modes)
(define-key hungarian-mode-map [kp-f4] 'toggle-hung-and-eng-modes)

  (add-hook 'minibuffer-setup-hook
  'use-hungarian-and-english-minor-modes-in-minibuffer)
  (add-hook 'isearch-mode-hook
  'redefine-keys-as-hungarian-in-isearch-mode-map)
  (add-hook 'isearch-mode-end-hook
  'restore-isearch-mode-map-keys)

(if (string-equal default-submode-of-accent-mode "eng")
   (english-mode)
   (hungarian-mode)
)

(run-hooks 'accent-mode-hook)
(force-mode-line-update)
)
(progn
(setq hungarian-mode nil)
(setq english-mode nil)
(setq hung-translation-map nil)
(setq eng-translation-map nil)
(setq current-translation-map nil)
;;; display???
(run-hooks 'end-accent-mode-hook)
(force-mode-line-update)
)))

;;;--------------------------------------------------------------------
;;;                     Initialization



(global-set-key [f8] 'accent-mode)
(global-set-key [kp-f4] 'accent-mode)
;;; Because Meta keys used in accent mode shadows some important keys
;;; (e.g. M-.) we rebind them with C-M- so one can use C-M-. 
;;; instead of M-.
(define-key esc-map [?\C-^] 'delete-indentation)
(define-key esc-map [?\C-\\] 'delete-horizontal-space)
(define-key esc-map [?\C-=] 'count-lines-region)
(define-key esc-map [?\C-|] 'shell-command-on-region)
(define-key esc-map [?\C-;] 'indent-for-comment)
(define-key esc-map [?\C-{] 'backward-paragraph)
(define-key esc-map [?\C-}] 'forward-paragraph)
(define-key esc-map [?\C-'] 'abbrev-prefix-mark)
(define-key esc-map [?\C-/] 'dabbrev-expand)
(define-key esc-map [?\C-.] 'find-tag)
(define-key esc-map [?\C-,] 'tags-loop-continue)
(define-key esc-map [?\C-~] 'not-modified)
(define-key esc-map [?\C-\(] 'insert-parentheses)
(define-key esc-map [?\C-\)] 'move-past-close-and-reindent)
(define-key esc-map [?\C-u] 'upcase-word)

(provide 'hun)
