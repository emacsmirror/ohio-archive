;;; russian.el --- display, translate and edit buffers containing
;; russian characters in various encodings. Supports different fonts
;; (or transliterations), buffer encodings, keyboard layouts and
;; insertion modes. 
;;
;; See also "Other foreign languages" for information about languages
;; other than Russian. 

;; REQUIRES GNU Emacs version 19

;; Copyright (C) 1994 Valery Alexeev <ava@math.jhu.edu>

;; Author: Valery Alexeev <ava@math.jhu.edu>
;;   Comments, suggestions and bug reports welcome.
;; Created: 14 Jan 1994
;; Version: 1.0
;; Keywords: foreign, russian

;; For COPYING CONDITIONS use GNU GENERAL PUBLIC LICENSE including the
;; NO WARRANTY FOR ANYTHING WHATSOEVER clause.

;;; Commentary:
;;
;; With this program, you will be able to:
;;   1) Display buffers containing russian characters in arbitrary
;; encodings without changing the buffers' contents if you have at least
;; one russian font installed on your system.
;;   2) Same as above if you do not have any russian fonts at all, using
;; your favourite transliteration scheme.
;;   3) Translate whole buffers or regions of text from one russian
;; standard to another, including arbitrary transliterations.
;;   4) Consequently, print files with russian characters if you have at
;; least one printable russian font.
;;   5) Type russian characters in arbitrary encodings and using
;; arbitrary keyboard layouts. 
;;
;; The basic concept of this program is an encoding which is simply a
;; string of 66 characters or a list of 66 strings, for all the
;; letters of Russian alphabet, 33 letters in lower case first, then
;; 33 letters in upper case. There are also extra bits of information
;; for encodings that mess up the usual ASCII characters, such as
;; "jcuken" for example. You can easily add as many new encodings as
;; you like, see "Customisization" below to find out how to do this.  
;; 
;;; The predefined encodings are:
;; 
;; 8koi     KOI-8 RFC 1489 = old KOI-8 GOST 19768-74 with SMALL IO and
;;            CAPITAL IO added, used in relcom.* newsgroups and much of e-mail
;; 7koi     KOI-7
;; alt      Alternativnyj Variant = MS DOS code page CP 866
;; gostcii  ISO 8859-5
;; osn      Osnovnoj Variant (the only difference in the cyrillic
;;            range between this standard and GOSTCII is CAPITAL IO)
;; cp1251   MS Windows code page CP 1251
;; mac      Macintosh standard
;; dkoi     DKOI-8 (Russian EBCDIC) GOST 19768-87, obscure
;; cp500    code page CECP 500, obscure
;; ebcdic   EBCDIC GOST 19768-74, obscure
;; ascii    american keyboard, phonetic transliteration,
;;            probably, the most natural keyboard layout
;; jcuken   russian typewriter keyboard layout, used in some TrueType
;;            fonts for MS Windows 
;; libcon   Library of Congress transliteration standard
;; naive    the most common transliteration
;; tex      AMSTeX & LaTeX transliteration
;; broken-8koi  this is what some mailers/gateways do to your email
;;              to/from Russia 
;; ascii2   one more transliteration, without SMALL IO, CAPITAL IO and
;;            CAPITAL HARD SIGN
;;            
;; Information about various standards was taken in part from files in 
;; the directory 
;;   /anonymous@nic.funet.fi:/pub/culture/russian/comp/characters,
;; in particular from 
;; cyrillic.encoding.faq by Andras Kornai <andras@calera.com>,  
;; lettermappings.gz by Dmitri Vulis <dlv@sunyvms1.bitnet> and RFC 1489. 
;; 
;; If you know of more standards, please contribute. 

;;; Installation:
;; Put the following in your .emacs, modified according to your
;; preferences. Below the default values are shown that will be
;; assigned automatically if you fail to set them yourself. 
;;   
;;       ;; Not nesessary unless you set different values.
;;       (setq russian-font-name "alt")
;;       (setq russian-buffer-name "8koi")
;;       (setq russian-keyboard-name "ascii")
;;       (setq russian-mode-name "8koi")
;;      
;;       ;; This is important. Make sure the program is on your
;;       ;; load-path (check it with C-h v load-path).     
;;       (require 'russian)   
;;
;; If you want to envoke the russian display automatically, add
;;       (russian-display) 
;;    
;; Finally, don't forget to M-x byte-compile-file russian.el. 

;;; Usage:

;;; Display:    
;; The function 
;;
;;       M-x russian-display 
;;       
;; sets the standard display table so that you can see russian
;; characters encoded according to various standards. The actual file
;; or a buffer won't change, only the way of presenting it. The
;; function needs two parameters: the buffer type and the font. For
;; the buffer type you will be prompted. The font is the font already
;; installed on your system or a transliteration which for the
;; purposes of this program is also considered to be a font. You are
;; not likely to change it often, so the function uses a value already
;; set in your .emacs or the default one. Interactively, you can change
;; this value with M-x russian-set-font. With a positive numerical
;; argument, for example   
;;       
;;       ESC 1 M-x russian-display      
;;    
;; only the display table of the current buffer will be set, and this
;; won't affect other buffers. This way, you can display 
;; many buffers at once, each in its own encoding and even using
;; it's own font (or a transliteration). With a nonpositive argument,
;; for example 
;;     
;;       ESC - 1 M-x russian-display
;;    
;; it acts much as the function without any argument but resets the
;; current buffer's display table so it adheres the default display again. 
;;    
;; The function also has its counterpart,
;;    
;;       M-x russian-undisplay
;;
;; which restores the standard display table to its original state,
;; that is displaying characters higher than 127 in the octal notation.
;; Similarly, given a positive argument it will affect only the local
;; display and with a nonpositive argument it will in addition make
;; the local table obey the default. 

;;; Translation.
;;
;;       M-x russian-translate-region and
;;       M-x russian-translate-buffer
;;
;; will prompt you for the buffer encoding and the new encoding. It
;; translates even from a non 1-to-1 encoding such as the "libcon" or
;; the "naive" encodings and does a better job at it than you'd
;; expect. Translation from "tex" is not currently implemented. It
;; doesn't seem to be very useful and I felt that writing a decoding
;; rule for "tex" would be a tedious and a fruitless job. But if you
;; need it let me know. The completion is enabled so when prompted you
;; can hit TAB to see all available standards. 
;; The translation between the "string" type encodings (see below) is
;; the fastest, translation from a "string" type to a "list" type is
;; considerably slower and the translation from the "list" type 
;; sometimes gets really slow depending on the size of a region/buffer.

;;; Editing.
;;  
;;       M-x russian-insert-mode
;;
;; toggles a minor mode. When in this mode, typing `a' will actually
;; produce a character corresponding to `a' in the encoding chosen,
;; for example, `\301' in KOI-8. The encoding is set in your .emacs.
;; Interactively, you can choose the encoding by invoking this
;; function with a positive argument. The negative argument will
;; always turn the mode off. A "minor" means that all bindings not
;; directly affected by this mode such as all C-... and M-...
;; keystrokes of the major mode remain in effect, as well as the
;; syntax tables and everything else. So you can use this minor mode
;; while in the Mail mode, for example.

;;; Changing the parameters.
;;
;; As has been already explained, you will be prompted for the
;; parameters that you are most likely to change often. At any rate,
;; there are four dedicated functions for changing them
;;
;;       M-x russian-set-font
;;       M-x russian-set-buffer
;;       M-x russian-set-keyboard
;;       M-x russian-set-mode

;;; Customization:
;;
;; All customization should go **before** the line (require 'russian)
;; to take effect. You can customize this program in several ways.
;;   1) Currently all the predefined standards can be used for
;; translation but not all of them can be set as font, buffer,
;; keyboard or insertion mode names. You can change this by setting 
;; the variables 
;;
;;       russian-font-additional-list  
;;       russian-buffer-additional-list 
;;       russian-keyboard-additional-list 
;;       russian-mode-additional-list
;;
;; For example,
;;
;;       (setq russian-font-additional-list 
;;         '("dkoi" "cp500"))
;;
;;   2) You can add more encodings. First, you have to define an
;; encoding russian-encoding-whatever and it should be a string of
;; 66 characters (for 33 russian letters in the lower case, then 33
;; letters in the upper case), or a list of 66 strings. You should
;; take a look at the definitions of various encodings in this file.
;; Then, if your encoding is non-standard even in the ASCII range,
;; you should also define russian-encoding-whatever-filter-from and
;; russian-encoding-whatever-filter-to, each of them being a list of 2
;; strings defining the translation rules, compare "jcuken". If your
;; encoding is of the "list" type and you are planning to translate
;; from it then you also need russian-encoding-whatever-decoding-rule.  
;; The latter is a tree-like data structure much as a keymap which
;; contains characters or strings of characters that have to be
;; inserted when a partial completion was succesful.  
;; Positive number at the end or in the middle of a
;; branch means "insert the i-th letter of the Russian alphabet",
;; 0<i<66+1. A negative number i means "insert a character -i" and i=0
;; means "do not insert anything". This flexibility allows programming
;; many different situations easily, for example the case of "e" at
;; the beginning of the word in the "libcon" transliteration. Check
;; out russian-encoding-libcon-decoding-rule for more details.
;;
;; Then you need to define russian-encoding-additional-alist which has
;; the following format
;;
;;  (setq russian-encoding-additional-alist '(
;;     (PROMPT ENCODING MDLNENAME DECODING-RULE FILTER-FROM FILTER-TO)
;;     (same for the second additional encoding etc.)
;;  ))
;;
;; Here PROMPT is a string easy to type, for example "wtvr", ENCODING
;; is the name of the variable defining the encoding, in our case 
;; russian-encoding-whatever, MDLNENAME is a string for the modeline, i.e.
;; "WHATEVER", DECODING-RULE is russian-encoding-whatever-decoding-rule 
;; or nil, and FILTERS are russian-encoding-whatever-filter-to (resp.
;; from) or nil.
;;
;;; Restrictions:
;; 1) russian-buffer-additional-list and russian-keyboard-additional-list
;; should contain only names of encodings of the "string" type.
;; 2) encodings that are nonstandard even in the ASCII range should
;; have the "string" type. 

;;; Other foreign languages.
;;
;; I thought about writing a generic package so that fitting it for a
;; particular language would be simply a matter of plugging in the
;; right data. In the end, however, I decided against this in favor of
;; a more straightforward approach.
;; To make an <insert the language>.el simply copy this file, 
;; M-x query-replace russian RET <insert the language> RET,
;; change the language-specific encodings and default-alists and adjust
;; headers and documentation accordingly. For most letter-based
;; languages that should be all.  

;; LCD Archive Entry:
;; russian.el|Valery Alexeev|ava@math.jhu.edu|
;; Display, translate and edit buffers containing russian characters.|
;; 14-Jan-1994|1.0|~/packages/russian.el.Z|

;;; Code:

;;;; Basic definitions and preliminary functions

;; User configurable variables 
;; (configure them in your .emacs, not here). 
(defvar russian-font-name "alt")
(defvar russian-buffer-name "8koi")
(defvar russian-keyboard-name "ascii")
(defvar russian-mode-name "8koi")

(defvar russian-encoding-additional-alist nil)
(defvar russian-font-additional-list nil) 
(defvar russian-buffer-additional-list nil)
(defvar russian-keyboard-additional-list nil) 
(defvar russian-mode-additional-list nil)
;; User configurable variables end here.

;;;; Defaults.
(defconst russian-encoding-default-alist
  '(("ascii" russian-encoding-ascii "ASCII")
     ("7koi" russian-encoding-7koi "KOI-7")
     ("alt"  russian-encoding-alt "ALT")
     ("8koi"  russian-encoding-8koi "KOI-8")
     ("mac" russian-encoding-mac "MAC")
     ("cp1251" russian-encoding-cp1251 "CP1251")
     ("osn" russian-encoding-osn "OSN")
     ("gostcii" russian-encoding-gostcii "GOSTCII")
     ("naive" russian-encoding-naive "NAIVE" 
      russian-encoding-naive-decoding-rule)
     ("libcon" russian-encoding-libcon "LibCon"
      russian-encoding-libcon-decoding-rule)
     ("broken-8koi" russian-encoding-broken-8koi "BrknKOI-8" 
      russian-encoding-broken-8koi-decoding-rule)
     ("tex" russian-encoding-tex "CyrTeX")
     ("jcuken" russian-encoding-jcuken "JCUKEN" nil
      russian-encoding-jcuken-filter-from russian-encoding-jcuken-filter-to)
     ("cp500" russian-encoding-cp500 "CP500")
     ("ebcdic" russian-encoding-ebcdic "EBCDIC")
     ("dkoi" russian-encoding-dkoi "DKOI")
     ("ascii2" russian-encoding-ascii2 "ASCII-2")))
(defconst russian-font-default-list 
  '("ascii" "7koi" "alt" "8koi" "mac" "cp1251" "osn" 
    "gostcii" "naive" "libcon" "jcuken")) 
(defconst russian-buffer-default-list 
  '("ascii" "7koi" "alt" "8koi" "mac" "cp1251" "osn" 
    "gostcii" "jcuken" "cp500" "ebcdic" "dkoi")) 
(defconst russian-keyboard-default-list
  '("ascii" "jcuken")) 
(defconst russian-mode-default-list 
  '("7koi" "alt" "8koi" "mac" "cp1251" "osn" "gostcii" 
    "naive" "libcon" "broken-8koi" "tex" "jcuken"))

(defconst russian-safe-encoding-name "8koi"
"This should be an encoding of the \"string\" type and coincide with
ASCII for characters below 127.") 


;;;; Setting the variables.
(defvar russian-font nil)
(defvar russian-buffer nil)
(defvar russian-keyboard nil)
(defvar russian-mode nil)

(defvar russian-encoding-alist 
  (append russian-encoding-default-alist russian-encoding-additional-alist))
(defvar russian-font-list 
  (append russian-font-default-list russian-font-additional-list)) 
(defvar russian-buffer-list 
  (append russian-buffer-default-list russian-buffer-additional-list))
(defvar russian-keyboard-list 
  (append russian-keyboard-default-list russian-keyboard-additional-list)) 
(defvar russian-mode-list 
  (append russian-mode-default-list russian-mode-additional-list)) 

(defvar russian-font-alist nil)
(defvar russian-buffer-alist nil)
(defvar russian-keyboard-alist nil)
(defvar russian-mode-alist nil)

(let ((i 0)
      (max (length russian-font-list)))
  (while (< i max)
    (setq russian-font-alist
	  (cons (list (nth i russian-font-list)) russian-font-alist))
    (setq i (+ 1 i))))
(let ((i 0)
      (max (length russian-buffer-list)))
  (while (< i max)
    (setq russian-buffer-alist
	  (cons (list (nth i russian-buffer-list)) russian-buffer-alist)) 
    (setq i (+ 1 i))))
(let ((i 0)
      (max (length russian-keyboard-list)))
  (while (< i max)
    (setq russian-keyboard-alist
	  (cons (list (nth i russian-keyboard-list)) russian-keyboard-alist)) 
    (setq i (+ 1 i))))
(let ((i 0)
      (max (length russian-mode-list)))
  (while (< i max)
    (setq russian-mode-alist
	  (cons (list (nth i russian-mode-list)) russian-mode-alist)) 
    (setq i (+ 1 i))))
  



;;;; Definitions for encodings.
(defconst russian-encoding-ascii
  (concat
   "abwgde^vzijklmnoprstufhc=[]#yx\\`q"
   "ABWGDE&VZIJKLMNOPRSTUFHC+{}$YX|~Q"))
  
(defconst russian-encoding-7koi
  (concat
   "ABWGDE#VZIJKLMNOPRSTUFHC^[]_YX\\@Q"
   "abwgde$vzijklmnoprstufhc~{}\"yx|`q"))
  
(defconst russian-encoding-alt
  (concat
   "\240\241\242\243\244\245\361\246\247"
   "\250\251\252\253\254\255\256\257"
   "\340\341\342\343\344\345\346\347"
   "\350\351\352\353\354\355\356\357"
   "\200\201\202\203\204\205\360\206\207"
   "\210\211\212\213\214\215\216\217"
   "\220\221\222\223\224\225\226\227"
   "\230\231\232\233\234\235\236\237"))
  
(defconst russian-encoding-8koi
  (concat
   "\301\302\327\307\304\305\243\326\332"
   "\311\312\313\314\315\316\317\320"
   "\322\323\324\325\306\310\303\336"
   "\333\335\337\331\330\334\300\321"
   "\341\342\367\347\344\345\263\366\372"
   "\351\352\353\354\355\356\357\360"
   "\362\363\364\365\346\350\343\376"
   "\373\375\377\371\370\374\340\361"))
  
(defconst russian-encoding-gostcii 
  (concat
   "\320\321\322\323\324\325\361\326\327"
   "\330\331\332\333\334\335\336\337"
   "\340\341\342\343\344\345\346\347"
   "\350\351\352\353\354\355\356\357"
   "\260\261\262\263\264\265\241\266\267"
   "\270\271\272\273\274\275\276\277"
   "\300\301\302\303\304\305\306\307"
   "\310\311\312\313\314\315\316\317"))

(defconst russian-encoding-osn 
  (concat
   "\320\321\322\323\324\325\361\326\327"
   "\330\331\332\333\334\335\336\337"
   "\340\341\342\343\344\345\346\347"
   "\350\351\352\353\354\355\356\357"
   "\260\261\262\263\264\265\360\266\267"
   "\270\271\272\273\274\275\276\277"
   "\300\301\302\303\304\305\306\307"
   "\310\311\312\313\314\315\316\317"))

(defconst russian-encoding-cp1251
  (concat
   "\340\341\342\343\344\345\270\346\347"
   "\350\351\352\353\354\355\356\357"
   "\360\361\362\363\364\365\366\367"
   "\370\371\372\373\374\375\376\377"
   "\300\301\302\303\304\305\250\306\307"
   "\310\311\312\313\314\315\316\317"
   "\320\321\322\323\324\325\326\327"
   "\330\331\332\333\334\335\336\337"))

(defconst russian-encoding-mac
  (concat
   "\340\341\342\343\344\345\336\346\347"
   "\350\351\352\353\354\355\356\357"
   "\360\361\362\363\364\365\366\367"
   "\370\371\372\373\374\375\376\377"
   "\200\201\202\203\204\205\335\206\207"
   "\210\211\212\213\214\215\216\217"
   "\220\221\222\223\224\225\226\227"
   "\230\231\232\233\234\235\236\237"))

(defconst russian-encoding-cp500
  (concat 
   "\254\151\355\356\353\357\111\354\277"
   "\200\375\376\373\374\255\256\131"
   "\104\105\102\106\103\107\234\110"
   "\124\121\122\123\130\125\126\127"
   "\220\217\352\372\276\240\252\266\263"
   "\235\332\233\213\267\270\271\253"
   "\144\145\142\146\143\147\236\150"
   "\164\161\162\163\170\165\166\167"))

(defconst russian-encoding-dkoi
  (concat
   "\167\170\257\215\212\213\131\256\262"
   "\217\220\232\233\234\235\236\237"
   "\252\253\254\255\214\216\200\266"
   "\263\265\267\261\260\264\166\240"
   "\271\272\355\277\274\275\102\354\372"
   "\313\314\315\316\317\332\333\334"
   "\336\337\352\353\276\312\273\376"
   "\373\375\165\357\356\374\270\335"))

(defconst russian-encoding-ebcdic
  (concat
   "\237\240\252\253\254\255\335\256\257"
   "\260\261\262\263\264\265\266\267"
   "\270\271\272\273\274\275\276\277"
   "\312\313\314\315\316\317\332\333"
   "\130\131\142\143\144\145\102\146\147"
   "\150\151\160\161\162\163\164\165"
   "\166\167\170\200\212\213\214\215"
   "\216\217\220\232\233\234\235\236"))

(defconst russian-encoding-jcuken
  (concat 
   "f,dult/;pbqrkvyjghcnea[wxio]sm'.z"
   "F<DULT?:PBQRKVYJGHCNEA{WXIO}SM\">Z"))

(defconst russian-encoding-naive
  '("a" "b" "v" "g" "d" "e" "e" "zh" "z"
    "i" "j" "k" "l" "m" "n" "o" "p"
    "r" "s" "t" "u" "f" "h" "c" "ch"
    "sh" "sch" "'" "y" "'" "e" "yu" "ya"
    "A" "B" "V" "G" "D" "E" "E" "Zh" "Z"
    "I" "J" "K" "L" "M" "N" "O" "P"
    "R" "S" "T" "U" "F" "H" "C" "Ch"
    "Sh" "Sch" "'" "Y" "'" "E" "Yu" "Ya"))

(defconst russian-encoding-libcon
  '("a" "b" "v" "g" "d" "e" "e" "zh" "z"
    "i" "j" "k" "l" "m" "n" "o" "p"
    "r" "s" "t" "u" "f" "x" "ts" "ch"
    "sh" "shch" "\"" "y" "'" "e" "ju" "ja"
    "A" "B" "V" "G" "D" "E" "E" "ZH" "Z"
    "I" "J" "K" "L" "M" "N" "O" "P"
    "R" "S" "T" "U" "F" "X" "TS" "CH"
    "SH" "SHCH" "\"" "Y" "'" "E" "JU" "JA"))

(defconst russian-encoding-broken-8koi
  '("=C1" "=C2" "=D7" "=C7" "=C4" "=C5" "=A3" "=D6" "=DA"
    "=C9" "=CA" "=CB" "=CC" "=CD" "=CE" "=CF" "=D0"
    "=D2" "=D3" "=D4" "=D5" "=C6" "=C8" "=C3" "=DE"
    "=DB" "=DD" "=DF" "=D9" "=D8" "=DC" "=C0" "=D1"
    "=E1" "=E2" "=F7" "=E7" "=E4" "=E5" "=B3" "=F6" "=FA"
    "=E9" "=EA" "=EB" "=EC" "=ED" "=EE" "=EF" "=F0" 
    "=F2" "=F3" "=F4" "=F5" "=E6" "=E8" "=E3" "=FE" 
    "=FB" "=FD" "=FF" "=F9" "=F8" "=FC" "=E0" "=F1"))

(defconst russian-encoding-broken-8koi-decoding-rule
  '(nil
    (?\= (nil
	  (?0 (nil
	       (?9 . -9)))
	  (?2 (nil
	       (?0 . -32)))
	  (?\n . 0)
	  (?A (nil
	       (?3 . 7)))
	  (?B (nil
	       (?3 . 40)))
	  (?C (nil
	       (?0 . 32)
	       (?1 . 1)
	       (?2 . 2)
	       (?3 . 24)
	       (?4 . 5)
	       (?5 . 6)
	       (?6 . 22)
	       (?7 . 4)
	       (?8 . 23)
	       (?9 . 10)
	       (?A . 11)
	       (?B . 12)
	       (?C . 13)
	       (?D . 14)
	       (?E . 15)
	       (?F . 16)))
	  (?D (nil
	       (?0 . 17)
	       (?1 . 33)
	       (?2 . 18)
	       (?3 . 19)
	       (?4 . 20)
	       (?5 . 21)
	       (?6 . 8)
	       (?7 . 3)
	       (?8 . 30)
	       (?9 . 29)
	       (?A . 9)
	       (?B . 26)
	       (?C . 31)
	       (?D . 27)
	       (?E . 25)
	       (?F . 28)))
	  (?E (nil
	       (?0 . 65)
	       (?1 . 34)
	       (?2 . 35)
	       (?3 . 57)
	       (?4 . 38)
	       (?5 . 39)
	       (?6 . 55)
	       (?7 . 37)
	       (?8 . 56)
	       (?9 . 43)
	       (?A . 44)
	       (?B . 45)
	       (?C . 46)
	       (?D . 47)
	       (?E . 48)
	       (?F . 49)))
	  (?F (nil
	       (?0 . 50)
	       (?1 . 66)
	       (?2 . 51)
	       (?3 . 52)
	       (?4 . 53)
	       (?5 . 54)
	       (?6 . 41)
	       (?7 . 36)
	       (?8 . 63)
	       (?9 . 62)
	       (?A . 42)
	       (?B . 59)
	       (?C . 64)
	       (?D . 60)
	       (?E . 58)
	       (?F . 61)))))))

(defconst russian-encoding-libcon-decoding-rule
  '(nil
    (?a . 1)
    (?b . 2)
    (?c (24
	 (?h . 25)))
    (?d . 5)
    (?e (6
	 (?f (nil
	      (?f . [31 22 22])))
	 (?l (nil
	      (?e (nil
		   (?k (nil
			(?t (nil
			     (?r . [31 13 6 12 20 18])))))))))))
    (?f . 22)
    (?g . 4)
    (?h . 23)
    (?i . 10)
    (?j (11
	 (?a . 33)
	 (?o . 7)
	 (?u . 32)))
    (?k (12
	 (?h . 23)))
    (?l . 13)
    (?m . 14)
    (?n . 15)
    (?o . 16)
    (?p (17
	 (?o ([17 16]
	      (?e ([17 16 6]
		   (?t . [17 16 31 20])))
	      (?r (nil
		   (?t (nil
			(?s (nil
			     (?m . [17 16 18 20 19 14])))))))))))
    (?r (18
	 (?t (nil
	      (?s (nil
		   (?i (nil
			(?g . [18 20 19 10 4])))))))))
    (?s (19
	 (?o (nil
	      (?v (nil
		   (?e (nil
			(?t (nil
			     (?s (nil
				  (?k . [19 16 3 6 20 19 12])))))))))))
	 (?c (nil
	      (?h (27
		   (?e ([27 6]
			(?z . [19 25 6 9])))
		   (?i (nil
			(?t (nil
			     (?a . [19 25 10 20 1])))))
		   (?a (nil
			(?s (nil
			     (?t . [19 25 1 19 20])))))))))
	 (?h (26
	      (?c (nil
		   (?h . 27)))))))
    (?t (20
	 (?s (24
	      (?c (nil
		   (?h . [20 27])))
	      (?j (nil
		   (?a . [20 19 33])))
	      (?t . [20 19 20])
	      (?y (nil
		   (?a . [20 19 33])))))))
    (?u . 21)
    (?v . 3)
    (?w . 3)
    (?x . 23)
    (?y (29
	 (?a . 33)
	 (?o . 7)
	 (?u . 32)))
    (?z (9
	 (?h . 8)))
    (?A . 34)
    (?B (35
	 (?' . [35 63])))
    (?C (57
	 (?H . 58)
	 (?h . 58)))
    (?D (38
	 (?' . [38 63])))
    (?E (39
	 (?V ([39 36]
	      (?M . [64 36 47])))))
    (?F (55
	 (?' . [55 63])))
    (?G (37
	 (?' . [37 63])))
    (?H (56
	 (?' . [56 63])))
    (?I . 43)
    (?J (44
	 (?A . 66)
	 (?a . 66)
	 (?O . 40)
	 (?o . 40)
	 (?U . 65)
	 (?u . 65)))
    (?K (45
	 (?' . [45 63])
	 (?H . 56)
	 (?h . 56)))
    (?L (46
	 (?' . [46 63])))
    (?M (47
	 (?' . [47 63])))
    (?N (48
	 (?' . [48 63])))
    (?O . 49)
    (?P (50
	 (?' . [50 63])
	 (?O (nil
	      (?R (nil
		   (?T (nil
			(?S (nil
			     (?M . [50 49 51 53 52 47])))))))))))
    (?R (51
	 (?' . [51 63])
	 (?T (nil
	      (?S (nil
		   (?I (nil
			(?G . [51 53 52 43 37])))))))))
    (?S (52
	 (?' . [52 63])
	 (?o (nil
	      (?v (nil
		   (?e (nil
			(?t (nil
			     (?s (nil
				 (?k . [52 16 3 6 20 19 12])))))))))))
	 (?C (nil
	      (?H (60
		   (?I (nil
			(?T (nil
			     (?A . [52 58 43 53 34])))))
		   (?A (nil
			(?S (nil
			     (?T . [52 58 34 52 53])))))))))
	 (?H (59
	      (?' . [59 63])
	      (?C (nil
		   (?H . 60)))))
	 (?c (nil
	      (?h (60
		   (?i (nil
			(?t (nil
			     (?a . [52 25 10 20 1])))))
		   (?a (nil
			(?s (nil
			     (?t . [52 25 1 19 20])))))))))
	 (?h (59
	      (?c (nil
		   (?h . 60)))))))
    (?T (53
	 (?' . [53 63])
	 (?s (57
	      (?j (nil
		   (?a . [53 19 33])))
	      (?t . [53 19 20])
	      (?y (nil
		   (?a . [53 19 33])))))
	 (?S (57
	      (?J (nil
		   (?A . [53 52 66])))
	      (?T . [53 52 53])
	      (?Y (nil
		   (?A . [53 52 66])))))))
    (?U . 54)
    (?V (36
	 (?' . [36 63])))
    (?W (36
	 (?' . [36 63])))
    (?X (56
	 (?' . [56 63])))
    (?Y (62
	 (?A . 66)
	 (?O . 40)
	 (?U . 65)
	 (?a . 66)
	 (?o . 40)
	 (?u . 65)))
    (?Z (42
	 (?' . [42 63])
	 (?H (41
	      (?' . [41 63])))
	 (?h . 41)))
    (?\' . 30)
    (?\  (nil
	  (?s (nil
	       (?c (nil
		    (?h ([-32 19 25]
			 (?i (nil
			      (?p . [-32 27 10 17])))))))))
	  (?e (nil
	       (?n . [-32 31 15])
	       (?k . [-32 31 12])
	       (?p . [-32 31 17])
	       (?t . [-32 31 20])))
	  (?E (nil
	       (?k . [-32 64 12])
	       (?p . [-32 64 17])
	       (?K . [-32 64 45])
	       (?P . [-32 64 50])
	       (?t . [-32 64 20])
	       (?T . [-32 64 53])))))
    (?\t (nil  
	  (?s (nil
	       (?c (nil
		    (?h ([-9 19 25]
			 (?i (nil
			      (?p . [-9 27 10 17])))))))))
	  (?e (nil
	       (?k . [-9 31 12])
	       (?p . [-9 31 17])
	       (?t . [-9 31 20])))
	  (?E (nil
	       (?k . [-9 64 12])
	       (?p . [-9 64 17])
	       (?K . [-9 64 45])
	       (?P . [-9 64 50])
	       (?t . [-9 64 20])
	       (?T . [-9 64 53])))))
    (?\n (nil  
	  (?s (nil
	       (?c (nil
		    (?h ([-10 19 25]
			 (?i (nil
			      (?p . [-10 27 10 17])))))))))
	  (?e (nil
	       (?n . [-10 31 15])
	       (?k . [-10 31 12])
	       (?p . [-10 31 17])
	       (?t . [-10 31 20])))
	  (?E (nil
	       (?k . [-10 64 12])
	       (?p . [-10 64 17])
	       (?K . [-10 64 45])
	       (?P . [-10 64 50])
	       (?t . [-10 64 20])
	       (?T . [-10 64 53])))))
    (?\f (nil  
	  (?s (nil
	       (?c (nil
		    (?h ([-12 19 25]
			 (?i (nil
			      (?p . [-12 27 10 17])))))))))
	  (?e (nil
	       (?n . [-12 31 15])
	       (?k . [-12 31 12])
	       (?p . [-12 31 17])
	       (?t . [-12 31 20])))
	  (?E (nil
	       (?k . [-12 64 12])
	       (?p . [-12 64 17])
	       (?K . [-12 64 45])
	       (?P . [-12 64 50])
	       (?t . [-12 64 20])
	       (?T . [-12 64 53])))))
    (?\r (nil  
	  (?s (nil
	       (?c (nil
		    (?h ([-13 19 25]
			 (?i (nil
			      (?p . [-13 27 10 17])))))))))
	  (?e (nil
	       (?n . [-13 31 15])
	       (?k . [-13 31 12])
	       (?p . [-13 31 17])
	       (?t . [-13 31 20])))
	  (?E (nil
	       (?k . [-13 64 12])
	       (?p . [-13 64 17])
	       (?K . [-13 64 45])
	       (?P . [-13 64 50])
	       (?t . [-13 64 20])
	       (?T . [-13 64 53])))))))

(defconst russian-encoding-naive-decoding-rule
  russian-encoding-libcon-decoding-rule)

(defconst russian-encoding-tex
  '("{a}" "b" "v" "g" "d" "e" "\\\"e" "{zh}" "z"
    "i" "{\\u\\i}" "k" "l" "m" "n" "o" "p"
    "r" "{s}" "{t}" "u" "f" "{h}" "{ts}" "{ch}" 
    "{sh}" "{sch}" "{\\cdprime}" "{y}" "{\\cprime}" "\\'e" "{yu}" "{ya}"
    "{A}" "B" "V" "G" "D" "E" "\\\"E" "{ZH}" "Z"
    "I" "{\\u\\I}" "K" "L" "M" "N" "O" "P"
    "R" "{S}" "{T}" "U" "F" "{H}" "{TS}" "{CH}"
    "{SH}" "{SCH}" "{\\Cdprime}" "{Y}" "{\\Cprime}" "\\'E" "{YU}" "{YA}"))

(defconst russian-encoding-ascii2
  (concat
   "abwgde\243vzijklmnoprstufhc^[]_yx\\@q"
   "ABWGDE\263VZIJKLMNOPRSTUFHC~{}\377YX|`Q"))

;;;; Additional encodings outside the cyrillic range.
(defconst russian-encoding-jcuken-filter-from
  '("~`!@#$%^&*_-+=" 
    "+=_!/\":<>?-,;."))
(defconst russian-encoding-jcuken-filter-to
  '("~`!@#$%^&*_-+={[}]:;\"'<,>.?/"
    " $@       !_~`(())%+$$^-&=*#"))


;;;; Functions for changing the variables.
(defun russian-set-font ()
  (interactive)
  (let ((font-name
	 (completing-read 
	  (concat "New russian font or a transliteration (default "
		  russian-font-name
		  "): ")
	  russian-font-alist nil t)))
    (if (not (equal font-name ""))
	(setq russian-font-name font-name))))

(defun russian-set-buffer ()
  (interactive)
  (let ((buffer-name
	 (completing-read 
	  (concat "New encoding of a russian buffer (default "
		  russian-buffer-name
		  "): ")
	  russian-buffer-alist nil t)))
    (if (not (equal buffer-name ""))
	(setq russian-buffer-name buffer-name))))

(defun russian-set-keyboard ()
  (interactive)
  (let ((keyboard-name
	 (completing-read 
	  (concat "New russian keyboard (default "
		  russian-keyboard-name
		  "): ")
	  russian-keyboard-alist nil t)))
    (if (not (equal keyboard-name ""))
	(setq russian-keyboard-name keyboard-name))))

(defun russian-set-mode ()
  (interactive)
  (let ((mode-name
	 (completing-read 
	  (concat "New russian insertion mode (default "
		  russian-mode-name
		  "): ")
	  russian-mode-alist nil t)))
    (if (not (equal mode-name ""))
	(setq russian-mode-name mode-name))))


;;;; Working engine.
(defun russian-univ-nth (string-or-list i)
  (if (listp string-or-list)
      (nth i string-or-list)
    (aref string-or-list i)))

(defun russian-univ-vector (char-or-string)
  (if (stringp char-or-string)
      (apply 'vector (append char-or-string nil))
    (vector char-or-string)))

(defun russian-univ-append (char-or-string string-or-list)
  (if (listp string-or-list)
      (append (vector  char-or-string) string-or-list)
    (concat (vector  char-or-string) string-or-list)))

(defvar russian-temporary-table nil)

(defun russian-get-table (numeric display max long-from long-to) 
  (let* ((from (eval (nth 1 long-from)))
	 (to (eval (nth 1 long-to)))
	 (from-filter (eval (nth 4 long-from)))
	 (to-filter (eval (nth 5 long-to)))
	 (from-filter-from (eval (nth 0 from-filter)))
	 (from-filter-to (eval (nth 1 from-filter)))
	 (to-filter-from (eval (nth 0 to-filter)))
	 (to-filter-to (eval (nth 1 to-filter))))
    (if numeric
	(progn
	  (setq russian-temporary-table (make-string max ?a))
	  (let ((i 0))
	    (while (< i max)
	      (aset russian-temporary-table i i)
	      (setq i (+ 1 i)))))
      (setq russian-temporary-table (make-vector max nil)))
    (let ((real-from from)
	  (real-to to))
      (cond
       ((and from-filter-from to-filter-from)
	(let ((i 0) j
	      char1 char2 char3
	      (flag t)
	      (len1 (length from-filter-from))
	      (len2 (length to-filter-from)))
	  (while (< i len1)
	    (progn
	      (setq char1 (aref from-filter-from i))
	      (setq char2 (aref from-filter-to i))
	      (setq j 0)
	      (while (and flag (< j len2))
		(if (equal char2 (aref to-filter-from j))
		    (progn
		      (setq flag nil)
		      (setq real-from (concat (vector char1) real-from))
		      (setq real-to
			    (if numeric
				(concat (vector (aref to-filter-to j)) real-to) 
			      (russian-univ-append 
			       (russian-univ-nth to-filter-to j) 
			       real-to))))
		  (setq j (+ 1 j))))
	      (if flag 
		  (progn
		    (setq real-from (concat (vector char1) real-from))
		    (setq real-to
			  (if numeric
			      (concat (vector char2) real-to)
			    (russian-univ-append
			     char2 real-to))))))
	    (setq i (+ 1 i)))))
       (from-filter-from
	(let ((i 0)
	      (len1 (length from-filter-from)))
	  (while (< i len1)
	    (progn
	      (setq real-from 
		    (concat (vector (aref from-filter-from i)) real-from))
	      (setq real-to
		    (if numeric
			(concat (vector (aref from-filter-to i)) real-to)
		      (russian-univ-append
		       (russian-univ-nth from-filter-to i)
		       real-to)))
	      (setq i (+ 1 i))))))
       (to-filter-to
	(let ((i 0)
	      (len2 (length to-filter-to)))
	  (while (< i len2)
	    (progn
	      (setq real-from 
		    (concat (vector (aref to-filter-from i)) real-from))
	      (setq real-to
		    (if numeric
			(concat (vector (aref to-filter-to i)) real-to)
		      (russian-univ-append
		       (russian-univ-nth to-filter-to i)
		       real-to)))
	      (setq i (+ 1 i))))))
       (t))
      (let ((i 0)
	    (len (length real-from)))
	(while (< i len)
	  (aset russian-temporary-table
		(aref real-from i)
		(if numeric
		    (aref real-to i)
		  (russian-univ-nth real-to i)))
	  (setq i (+ 1 i))))
      (if display
	  (let ((i 0))
	    (while (< i max)
	      (if (aref russian-temporary-table i)
		  (aset russian-temporary-table
			i
			(russian-univ-vector 
			 (aref russian-temporary-table i))))
	      (setq i (+ 1 i))))))))


;;;; Display part.
(defun russian-display (&optional arg)
  (interactive "P")
  (if (interactive-p) (call-interactively 'russian-set-buffer))
  (let* ((buffer-long 
	  (assoc russian-buffer-name russian-encoding-alist))
	 (font-long
	  (assoc russian-font-name russian-encoding-alist)))

    (russian-get-table nil t 261 buffer-long font-long)

    (if arg
	(if (> (prefix-numeric-value arg) 0)
	    (setq buffer-display-table russian-temporary-table)
	  (progn
	    (setq buffer-display-table nil)
	    (setq standard-display-table russian-temporary-table)))
      (setq standard-display-table russian-temporary-table))))

(defun russian-undisplay (&optional arg)
  (interactive "P")
  (if arg 
      (if (> (prefix-numeric-value arg) 0)
	  (setq buffer-display-table (make-vector 261 nil))
	(progn
	  (setq buffer-display-table nil)
	  (setq standard-display-table (make-vector 261 nil))))
    (setq standard-display-table (make-vector 261 nil))))

;;;; Translation.

(defun russian-translate-string-list (start end translate-table)
  (save-excursion
    (narrow-to-region start end)
    (goto-char 1)
    (while (not (eobp))
      (let* ((char (following-char))
	     (char-to (aref translate-table char)))
	(if char-to
	    (progn
	      (delete-char 1)
	      (insert char-to))
	  (forward-char))))
    (widen)))

(defun russian-translate-decoding-rule (start end decoding-rule to)
  (save-excursion
    (let (decoding-list
	  chars-moved
	  char-to 
	  chars-erase 
	  flag-read-more 
	  flag-match 
	  char-read)
      (narrow-to-region start end)
      (goto-char 1)
      (while  (not (eobp))
	(setq decoding-list decoding-rule)
	(setq chars-moved 0)
	(setq char-to nil)
	(setq chars-erase 0)
	(setq flag-read-more t)
	(setq flag-match t)
	(setq char-read nil)
	(catch 'read-more
	  (progn
	    (setq flag-match t)
	    (while (and flag-read-more (not (eobp)))
	      (progn
		(setq flag-match t)
		(setq char-read (following-char))
		(let ((i 1))
		  (progn
		    (while (and flag-match (< i (length decoding-list)))
		      (progn
			(if (eq char-read (car (nth i decoding-list)))
			    (progn
			      (setq flag-match nil)
			      (setq decoding-list (cdr (nth i decoding-list)))
			      (if (atom decoding-list)
				  (progn
				    (setq flag-read-more nil)
				    (setq chars-erase (+ 1 chars-moved))
				    (setq char-to decoding-list)
				    (throw 'read-more t))
				(progn
				  (setq decoding-list (car decoding-list))
				  (if (car decoding-list)
				      (progn
					(setq char-to (car decoding-list))
					(setq chars-erase (+ 1 chars-moved))))))))
			(setq i (+ 1 i)))
		      )
		    (if flag-match (setq flag-read-more nil))
		    (forward-char)
		    (setq chars-moved (+ 1 chars-moved))))))))
	(backward-char chars-moved)
	(delete-char chars-erase)
	(if char-to
	    (if (vectorp char-to)
		(progn
		  (let ((j 0)
			(n (length char-to)))
		    (while (< j n)
		      (let ((char (aref char-to j)))
			(cond
			 ((> char 0)
			  (insert
			   (if (listp to)
			       (nth (- char 1) to)
			     (aref to (- char 1)))))
			 ((< char 0)
			  (insert (- char)))
			 (t)))
		      (setq j (+ 1 j)))))
	      (cond
	       ((> char-to 0)
		(progn
		  (insert
		   (if (listp to)
		       (nth (- char-to 1) to)
		     (aref to (- char-to 1))))))
	       ((< char-to 0)
		(insert (- char-to)))
	       (t)))
	  (or
	   (eobp)
	   (forward-char))))
      (widen))))

    
(defun russian-translate-region 
  (start end &optional non-interactive from-name to-name)
  "non-interactive should be t if used non-interactively, from-name
and to-name are strings."
  (interactive "r")
  (if (null non-interactive)
      (progn
	(setq from-name
	      (completing-read
	       "Translate from the encoding: "
	       russian-encoding-alist nil t))
	(setq to-name
	      (completing-read
	       "Translate to the encoding: "
	       russian-encoding-alist nil t))))
  (let* 
      (translate-table
       (from-long (assoc from-name russian-encoding-alist))
       (to-long (assoc to-name russian-encoding-alist))
       (from (eval (nth 1 from-long)))
       (to (eval (nth 1 to-long))))
    
    (if (listp from)
	(if (nth 4 to-long)
	    (progn
	      (narrow-to-region start end)
	      (russian-translate-region (point-min) (point-max) t 
					from-name russian-safe-encoding-name)
	      (russian-translate-region (point-min) (point-max) t 
					russian-safe-encoding-name to-name)
	      (widen))
	  (let ((decoding-rule (eval (nth 3 from-long))))
	    (russian-translate-decoding-rule start end decoding-rule to)))

      (if (listp to)
	  (progn
	    (russian-get-table nil nil 256 from-long to-long)
	    (russian-translate-string-list start end 
					   russian-temporary-table))
	(progn
	  (russian-get-table t nil 256 from-long to-long) 
	  (translate-region start end russian-temporary-table)))))) 


(defun russian-translate-buffer ()
  (interactive)
  (russian-translate-region (point-min) (point-max) nil))



;;;; Russian insertion modes.

(defvar russian-insertion-mode nil
  "Non-nil if using russian mode as a minor mode of some other mode.")
(make-variable-buffer-local 'russian-insertion-mode)
(put 'russian-insertion-mode 'permanent-local t)

(defvar russian-insertion-mode-string " RUSSIAN")

(or (assq 'russian-insertion-mode minor-mode-alist)
    (setq minor-mode-alist 
	  (append minor-mode-alist
		  (list '(russian-insertion-mode 
			  russian-insertion-mode-string)))))

(defvar russian-insertion-mode-map nil)

(defvar russian-insertion-mode-table nil)

(defun russian-insertion-mode (&optional arg)
  "Toggle russian minor mode.
With arg, turn russian minor mode on if arg is positive (and prompt
for the encoding), off otherwise."
  (interactive "P")
  (setq russian-insertion-mode-string " RUSSIAN")
  (setq russian-insertion-mode
	(if (null arg) (not russian-insertion-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if russian-insertion-mode
      (progn
	(if (and
	     arg
	     (> (prefix-numeric-value arg) 0))
	    (call-interactively 'russian-set-mode nil))
	(let ((long-from (assoc russian-keyboard-name russian-encoding-alist))
	      (long-to (assoc russian-mode-name russian-encoding-alist)))
	  (setq russian-insertion-mode-map (make-sparse-keymap))
	  (russian-get-table nil nil 256 long-from long-to)
	  (setq russian-insertion-mode-table russian-temporary-table)
	  (let ((i 0))
	    (while (< i 256)
	      (let ((char (aref russian-insertion-mode-table i)))
		(if char
		    (define-key russian-insertion-mode-map  
		      (char-to-string i) 
		      'russian-perform-insertion)))
	      (setq i (1+ i))))
	  (run-hooks 'russian-insertion-mode-hook)
	  (setq russian-insertion-mode-string 
		(concat " " 
			(nth 2 long-to))))))
  (set-buffer-modified-p (buffer-modified-p))
  (let ((i 0) test)
    (while (< i (length minor-mode-map-alist))
      (setq test (nth i minor-mode-map-alist))
      (if (eq 'russian-insertion-mode (car test))
	  (progn
	    (setq minor-mode-map-alist (delq test minor-mode-map-alist))
	    (setq i (- i 1))))
      (setq i (+ 1 i))))
  (setq minor-mode-map-alist
	(cons (cons 'russian-insertion-mode russian-insertion-mode-map)
	      minor-mode-map-alist)))

(defun russian-perform-insertion ()
  (interactive)
  (insert (aref russian-insertion-mode-table last-command-event)))


(provide 'russian)

;;; russian.el ends here
