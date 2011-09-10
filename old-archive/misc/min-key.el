;;; @ min-key.el - minor mode for remapping the keyboard
;;;
;;; $Id: min-key.el,v 5.11 1993/07/28 00:30:55 amanda Exp $
;;;
;;; LCD Archive Entry:
;;; min-key|Per Abrahamsen|abraham@iesd.auc.dk|
;;; Remapping the keyboard and character set conversion|
;;; 28-Jul-1993|5.11|~/misc/min-key.el.Z|

(provide 'min-key)
(require 'min-bind)

;;; @@ Copyright
;;;
;;; Copyright (C) 1992 Per Abrahamsen
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; @@ Description
;;;
;;; This module provide a convenient way to remap the keymap.
;;;
;;; You provide it with a list of keys and a list of string they map
;;; into, and this module will create a minor mode implementing
;;; mapping.  The mapping will allow you to get the original key, if
;;; you press the same key twice.  
;;;
;;; It is up to you to modify the syntax table to reflect what a
;;; character is in the displayed text.

;;; @@ Encodings
;;;
;;; If you know of some encoding that you would find useful,
;;; please send it to me so I can add it to the list.
;;;
;;; Address: auc-tex_mgr@iesd.auc.dk

(defvar language-encodings
  '(("Danish" "DK"
     ;; I could add \"{u} here but then I would be able to spell Hans 
     ;; Hyttel's name correctly, removing a major source of fun.
     ("646" "Key" "Old" "TeX"    "L1")
     ("ISO 646 DK" "Keyboard Layout" "Digraphs" "TeX" "ISO 8859 Latin 1")
     ("["   ":"	  "Ae"  "{\\AE}" "\306")
     ("\\"  "\""  "Oe"  "{\\O}"  "\330")
     ("]"   "{"	  "Aa"  "{\\AA}" "\305")
     ("{"   ";"	  "ae"  "{\\ae}" "\346")
     ("|"   "'"	  "oe"  "{\\o}"  "\370")
     ("}"   "["	  "aa"  "{\\aa}" "\345"))
    ("Swedish" "SE"
     ("646" "Key" "Old" "TeX" "L1")
     ("ISO 646 SE" "Keyboard Layout" "Digraphs" "TeX" "ISO 8859 Latin 1")
     ("["   "\""   "Ae"  "\\\"{A}" "\304")
     ("\\"  ":"  "Oe"  "\\\"{O}" "\326")
     ("]"   "{"   "Aa"  "{\\AA}"  "\305")
     ("{"   "'"   "ae"  "\\\"{a}" "\344")
     ("|"   ";"   "oe"  "\\\"{o}" "\366")
     ("}"   "["   "aa"  "{\\aa}"  "\345"))
    ("German" "DE"
     ("Key" "Old" "TeX" "L1")
     ("Keyboard Layout" "Digraphs" "TeX" "ISO 8859 Latin 1")
     ("\""  "A\"" "\\\"{A}" "\304")
     (":"   "O\"" "\\\"{O}" "\326")
     ("{"   "U\"" "\\\"{U}" "\334")
     ("'"   "a\"" "\\\"{a}" "\344")
     (";"   "o\"" "\\\"{o}" "\366")
     ("["   "u\"" "\\\"{u}" "\374")
     ("z"   "y"   "y"       "y")
     ("y"   "z"   "z"       "z"))
    ("Russian" "CYR"
     ;; From: jsmith@king.mcs.drexel.edu (Justin R. Smith)
     ;;
     ;; Here is a keyboard mapping for Cyrillic text (Tex mode corresponds
     ;; to the AMS Cyrillic encoding and the koi mode produces the
     ;; koi8 encoding scheme, which is the Cyrillic version of ASCII).
     ;; We have a number of Russian students who want to be able to
     ;; edit cyrillic texts. We use a version of emacs with an 8-bit patch
     ;; and a Cyrillic font under X-windows.
     ;; ---
     ;; Cyrillic keyboard mapping table.
     ;; Produced by Serge Vakulenko, <vak@kiae.su>, Moscow.
     ;;
     ;; Russian YAWERTY keyboard layout implemented.
     ;; Cyrillic characters are entered in koi8 encoding.
     ;;
     ;; Public domain.  Share and enjoy.
     ;;
     ("Key"  "TeX" "koi")
     ( "Keyboard Layout"  "TeX" "koi8")
     ("q" "{ya}" "\321")
     ("Q" "{Ya}" "\361")
     ("w" "v" "\327")
     ("W" "V" "\327")
     ("e" "e" "\305")
     ("E" "E" "\345")
     ("r" "r" "\322")
     ("R" "R" "\362")
     ("t" "t" "\324")
     ("T" "T" "\364")
     ("y" "y" "\331")
     ("Y" "Y" "\371")
     ("u" "u" "\325")
     ("U" "U" "\365")
     ("i" "i" "\311")
     ("I" "I" "\351")
     ("o" "o" "\317")
     ("O" "O" "\357")
     ("p" "p" "\320")
     ("P" "P" "\360")
     ("[" "{sh}" "\333")
     ("{" "{Sh}" "\373")
     ("]" "{shch}" "\335")
     ("}" "{Shch}" "\375")
     ("a" "a" "\301")
     ("A" "A" "\341")
     ("s" "{s}" "\323")
     ("S" "{S}" "\363")
     ("d" "d" "\304")
     ("D" "D" "\344")
     ("f" "f" "\306")
     ("F" "F" "\346")
     ("g" "g" "\307")
     ("G" "G" "\347")
     ("h" "{kh}" "\310")
     ("H" "{Kh}" "\350")
     ("j" "{\\u\\i}" "\312")
     ("J" "{\\u I}" "\352")
     ("k" "k" "\313")
     ("K" "K" "\353")
     ("l" "l" "\314")
     ("L" "L" "\354")
     ("z" "z" "\332")
     ("Z" "Z" "\372")
     ("x" "{\\cprime}" "\330")
     ("X" "{\\Cprime}" "\370")
     ("c" "{ts}" "\333")
     ("C" "{Ts}" "\373")
     ("v" "{zh}" "\306")
     ("V" "{Zh}" "\346")
     ("b" "b" "\302")
     ("B" "B" "\342")
     ("n" "n" "\316")
     ("N" "N" "\356")
     ("m" "m" "\315")
     ("M" "M" "\355")
     ("\\" "\\\'e" "\334")
     ("\|" "\\\'E" "\374")
     ("`" "yu" "\300")
     ("~" "{Yu}" "\340")
     ("=" "{ch}" "\336")
     ("+" "{Ch}" "\376")
     ("\#" "{\\cdprime}" "\337"))
    ("Keyboard Layout" "Key"
     ;; I would like to add the AZERTY here.
     ("QW" "DV")
     ("QWERTY" "Dvorak")
     ("q"  "/")
     ("w"  ",")
     ("e"  ".")
     ("r"  "p")
     ("t"  "y")
     ("y"  "f")
     ("u"  "g")
     ("i"  "c")
     ("o"  "r")
     ("p"  "l")
     ("["  ";")
     ("]"  "=")
     ("a"  "a")
     ("s"  "o")
     ("d"  "e")
     ("f"  "u")
     ("g"  "i")
     ("h"  "d")
     ("j"  "h")
     ("k"  "t")
     ("l"  "n")
     (";"  "s")
     ("'"  "-")
     ("z"  "'")
     ("x"  "q")
     ("c"  "j")
     ("v"  "k")
     ("b"  "x")
     ("n"  "b")
     ("m"  "m")
     (","  "w")
     ("."  "v")
     ("/"  "z")
     ("Q"  "?")
     ("W"  "<")
     ("E"  ">")
     ("R"  "P")
     ("T"  "Y")
     ("Y"  "F")
     ("U"  "G")
     ("I"  "C")
     ("O"  "R")
     ("P"  "L")
     ("{"  ":")
     ("}"  "+")
     ("A"  "A")
     ("S"  "O")
     ("D"  "E")
     ("F"  "U")
     ("G"  "I")
     ("H"  "D")
     ("J"  "H")
     ("K"  "T")
     ("L"  "N")
     (":"  "S")
     ("\"" "_")
     ("X"  "Q")
     ("C"  "J")
     ("V"  "K")
     ("B"  "X")
     ("N"  "B")
     ("M"  "M")
     ("<"  "W")
     (">"  "V")
     ("?"  "Z")
     ("-"  "[")
     ("="  "]")
     ("_"  "{")
     ("+"  "}")
     ("Z"  "\""))
    ("ISO 8859 Latin 1" "L1"
     ;; The Mono, Old, 646, RFC, ^H maps taken from a table Markus
     ;; Kuhn <unrza3@cd4680fs.rrze.uni-erlangen.de> posted on
     ;; comp.std.internat and comp.mail.mime.
     ("L1"   "Mono" "Old" "646" "RFC" "^H" "TeX")
     ("ISO 8859 Latin 1" "Monograph" "Digraph"
      "ISO 646" "RFC 1345" "Backspace" "TeX")
     ("\240" " "    " "    " "  "NS"  " "    "~")
     ("\241" "!"    "!"    "!"  "!I"  "!"    "!`")
     ("\242" "c"    "c"    "c"  "Ct"  "c\b|" "{\\cent}") ;No TeX
     ("\243" "?"    "?"    "?"  "Pd"  "L\b-" "{\\pounds}") 
     ("\244" "?"    "?"    "$"  "Cu"  "o\bX" "{\\currency}") ;No TeX
     ("\245" "Y"    "Y"    "Y"  "Ye"  "Y\b=" "{\\yen}") ;No TeX
     ("\246" "|"    "|"    "|"  "BB"  "|"    "{\\brokenbar}") ;No TeX
     ("\247" "?"    "?"    "?"  "SE"  "?"    "{\\S}") 
     ("\250" "\""   "\""   "\"" "':"  "\""   "\\\"{}")
     ("\251" "c"    "(c)"  "(c)" "Co" "(c)"  "{\\copyright}")
     ("\252" "a"    "a"    "a"  "-a"  "a\b_" "\\underline{a}")
     ("\253" "<"    "<<"   "<<" "<<"  "<<"   "$\\ll$")
     ("\254" "-"    "-"    "-"  "NO"  "-\b," "$\\neg$")
     ("\255" "-"    "-"    "-"  "--"  "-"    "\\-")
     ("\256" "R"    "(R)"  "(R)" "Rg" "(R)"  "{\\tm}") ;No TeX
     ("\257" "-"    "-"    "-"  "'-"  "-"    "\\={}")
     ("\260" " "    " "    " "  "DG"  " "    "$^0$")
     ("\261" "?"    "+/-"  "+/-" "+-" "+\b_" "$^+_-$")
     ("\262" "2"    "2"    "2"  "2S"  "2"    "$^2$")
     ("\263" "3"    "3"    "3"  "3S"  "3"    "$^3$")
     ("\264" "'"    "'"    "'"  "''"  "'"    "\\'{}")
     ("\265" "u"    "u"    "u"  "My"  "u"    "$\mu$")
     ("\266" "P"    "P"    "P"  "PI"  "P"    "{\\P}")
     ("\267" "."    "."    "."  ".M"  "."    "$\\cdot$")
     ("\270" ","    ","    ","  "',"  ","    "\\c{}")
     ("\271" "1"    "1"    "1"  "1S"  "1"    "$^1$")  
     ("\272" "o"    "o"    "o"  "-o"  "o\b_" "$\\frac{o}{}$")
     ("\273" ">"    ">>"   ">>" ">>"  ">>"   "$\\gg$")
     ("\274" "?"    " 1/4" " 1/4" "14" " 1/4" "$\\frac{1}{4}$")
     ("\275" "?"    " 1/2" " 1/2" "12" " 1/2" "$\\frac{1}{2}$")
     ("\276" "?"    " 3/4" " 3/4" "34" " 3/4" "$\\frac{3}{4}$")
     ("\277" "?"    "?"    "?"  "?I"  "?"    "?`")
     ("\300" "A"    "A"    "A"  "A!"  "A\b`" "\\`{A}")
     ("\301" "A"    "A"    "A"  "A'"  "A\b'" "\\'{A}")
     ("\302" "A"    "A"    "A"  "A>"  "A\b^" "\\^{A}")
     ("\303" "A"    "A"    "A"  "A?"  "A\b~" "\\~{A}")
     ("\304" "A"    "Ae"   "["  "A:"  "A\b\"" "\\\"{A}")
     ("\305" "A"    "Aa"   "]"  "AA"  "Aa"   "{\\AA}")
     ("\306" "A"    "AE"   "["  "AE"  "AE"   "{\\AE}")
     ("\307" "C"    "C"    "C"  "C,"  "C\b," "\\c{C}")
     ("\310" "E"    "E"    "E"  "E!"  "E\b`" "\\`{E}")
     ("\311" "E"    "E"    "@"  "E'"  "E\b'" "\\'{E}")
     ("\312" "E"    "E"    "E"  "E>"  "E\b^" "\\^{E}")
     ("\313" "E"    "E"    "E"  "E:"  "E\b\"" "\\\"{E}")
     ("\314" "I"    "I"    "I"  "I!"  "I\b`" "\\`{I}")
     ("\315" "I"    "I"    "I"  "I'"  "I\b'" "\\'{I}")
     ("\316" "I"    "I"    "I"  "I>"  "I\b^" "\\^{I}")
     ("\317" "I"    "I"    "I"  "I:"  "I\b\"" "\\\"{I}")
     ("\320" "D"    "D"    "D"  "D-"  "D\b-" "{\\Eth}")	;No TeX
     ("\321" "N"    "N"    "N"  "N?"  "N\b~" "\\\"{N}")
     ("\322" "O"    "O"    "O"  "O!"  "O\b`" "\\`{O}")
     ("\323" "O"    "O"    "O"  "O'"  "O\b'" "\\'{O}")
     ("\324" "O"    "O"    "O"  "O>"  "O\b^" "\\^{O}")
     ("\325" "O"    "O"    "O"  "O?"  "O\b~" "\\~{O}")
     ("\326" "O"    "Oe"   "\\" "O:"  "O\b\"" "\\\"{O}")
     ("\327" "x"    "x"    "x"  "*X"  "x"    "$\\times$")
     ("\330" "O"    "Oe"   "\\" "O/"  "O\b/" "{\\O}")
     ("\331" "U"    "U"    "U"  "U!"  "U\b`" "\\`{U}")
     ("\332" "U"    "U"    "U"  "U'"  "U\b'" "\\'{U}")
     ("\333" "U"    "U"    "U"  "U>"  "U\b^" "\\^{U}")
     ("\334" "U"    "Ue"   "^"  "U:"  "U\b\"" "\\\"{U}")
     ("\335" "Y"    "Y"    "Y"  "Y'"  "Y\b'" "\\'{Y}")
     ("\336" "T"    "Th"   "Th" "TH"  "Th"   "{\\Thorn}") ;No TeX
     ("\337" "s"    "ss"   "ss" "ss"  "ss"   "{\\ss}")
     ("\340" "a"    "a"    "a"  "a!"  "a\b`" "\\`{a}")
     ("\341" "a"    "a"    "a"  "a'"  "a\b'" "\\'{a}")
     ("\342" "a"    "a"    "a"  "a>"  "a\b^" "\\^{a}")
     ("\343" "a"    "a"    "a"  "a?"  "a\b~" "\\~{a}")
     ("\344" "a"    "ae"   "{"  "a:"  "a\b\"" "\\\"{a}")
     ("\345" "a"    "aa"   "}"  "aa"  "aa"   "{\\aa}")
     ("\346" "a"    "ae"   "{"  "ae"  "ae"   "{\\ae}")
     ("\347" "c"    "c"    "c"  "c,"  "c\h," "\\c{c}")
     ("\350" "e"    "e"    "e"  "e!"  "e\b`" "\\`{e}")
     ("\351" "e"    "e"    "`"  "e'"  "e\b'" "\\'{e}")
     ("\352" "e"    "e"    "e"  "e>"  "e\b^" "\\^{e}")
     ("\353" "e"    "e"    "e"  "e:"  "e\b\"" "\\\"{e}")
     ("\354" "i"    "i"    "i"  "i!"  "i\b`" "\\`{\\i}")
     ("\355" "i"    "i"    "i"  "i'"  "i\b'" "\\'{\\i}")
     ("\356" "i"    "i"    "i"  "i>"  "i\b^" "\\^{\\i}")
     ("\357" "i"    "i"    "i"  "i:"  "i\b\"" "\\\"{\\i}")
     ("\360" "d"    "d"    "d"  "d-"  "d\b-" "{\\eth}")	;No TeX
     ("\361" "n"    "n"    "n"  "n?"  "n\b~" "\\~{n}")
     ("\362" "o"    "o"    "o"  "o!"  "o\b`" "\\`{o}")
     ("\363" "o"    "o"    "o"  "o'"  "o\b'" "\\'{o}")
     ("\364" "o"    "o"    "o"  "o>"  "o\b^" "\\^{o}")
     ("\365" "o"    "o"    "o"  "o?"  "o\b~" "\\~{o}")
     ("\366" "o"    "oe"   "|"  "o:"  "o\b\"" "\\\"{o}")
     ("\367" ":"    ":"    ":"  "-:"  "-\b:" "$\\div$")
     ("\370" "o"    "oe"   "|"  "o/"  "o\b/" "{\\o}")
     ("\371" "u"    "u"    "u"  "u!"  "u\b`" "\\`{u}")
     ("\372" "u"    "u"    "u"  "u'"  "u\b'" "\\'{u}")
     ("\373" "u"    "u"    "u"  "u>"  "u\b^" "\\^{u}")
     ("\374" "u"    "ue"   "~"  "u:"  "u\b\"" "\\\"{u}")
     ("\375" "y"    "y"    "y"  "y'"  "y\b'" "\\'{y}")
     ("\376" "t"    "th"   "th" "th"  "th"   "{\\thorn}") ;No TeX
     ("\377" "y"    "ij"   "y"  "y:"  "y\b\"" "\\\"{y}")))
  "*Language dependend mapping between different encodings.

This list determines how to map between different encodings of the
character set of a various langauges.

Each element in the list is itself a list.  

The first element is the name of the language.

The second element is a short name of the language.

The third element is a list of short strings, each representing a
possible encoding of the language's character set.

The fourth element is a list of long strings, each representing a the
same encodings of the language's character set.

The remaining elements each represents the encodings of one character.
The encodings must appear in the same sequence as they were found in
the second element.")

;;; @@ Mode Line

(or (assoc 'keyboard-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(keyboard-mode (" " keyboard-language-name
						  ":" keyboard-from-name
						  "->" keyboard-to-name))
				 minor-mode-alist)))

(defvar keyboard-mode nil
  "Flag indicating whether keyboard mode is currently on.")

 (make-variable-buffer-local 'keyboard-mode)

(defvar keyboard-language-name nil
  "The short name of the language currently being mapped by keyboard mode.")

 (make-variable-buffer-local 'keyboard-language-name)

(defvar keyboard-from-name nil
  "The short name of the encoding assumed for the keyboard.")

 (make-variable-buffer-local 'keyboard-from-name)

(defvar keyboard-to-name nil
  "The short name of the encoding assumed for the display.")

 (make-variable-buffer-local 'keyboard-to-name)

(defvar keyboard-double nil
  "If not nil, map a key pressed twice to the original definition.")

 (make-variable-buffer-local 'keyboard-double)

;;; @@ Variables

(defvar keyboard-language-entry nil
  "A list of character encodings for the selected language.")

 (make-variable-buffer-local 'keyboard-language-entry)

(defvar keyboard-from-entry nil
  "The index into the character encodings for the source encoding.")

 (make-variable-buffer-local 'keyboard-from-entry)

(defvar keyboard-to-entry nil
  "The index into the character encodings for the destination
encoding.")

 (make-variable-buffer-local 'keyboard-to-entry)

;;; @@ Keyboard Mode

(defun keyboard-mode (arg)
  "Toggle keyboard-mode.
With prefix arg, turn keyboard-mode on iff arg is positive.

This mode will map the keyboard for use in situations where the
encoding of the language for the text you want to write does not match
the encoding of the language on the keyboard.

The possible mapping is determined by value of the variable
language-encodings, see that for a list of available languages and
character set encodings. 

keyboard-mode will bind keys for all strings representing characters in
one encoding to insert strings representing characters in another
enencoding.  

To get the string corresponding to the original key, press the key
twice.  This is a very convenient way to remap the keyboard for
languages where a given character seldom occur twice in a row, and
where you don't need the original keys too often.  When this is not
the case, e.g. if you do a Dvorak to QWERTY mapping, you can supress
the ``backmapping'' for keyboard keys by setting the variable
keyboard-double to nil.

Before calling this function, you should set the mapping with
`keyboard-set-mapping'."

  (interactive "P")
  (cond ((not keyboard-language-entry)
	 ;; Not ready -- query user for language and encodings
	 (keyboard-query-mapping))

	((or (and (null arg) keyboard-mode)
	     (<= (prefix-numeric-value arg) 0))
	 ;; Turn it off	 
	 (if keyboard-mode
	     (minor-unbind 'keyboard-mode)))

	(keyboard-mode
	 ;; Already on, do nothing
	 )

	(t
	 ;; Turn it on
	 (let ((map (make-sparse-keymap)))
	   (mapcar (function (lambda (entry)
			       (define-key map
				 (nth keyboard-from-entry entry)
				 'keyboard-map-function)))
		   keyboard-language-entry)
	   (minor-add-to-keymap 'keyboard-mode map)
;	   (if keyboard-double
;	       (progn
;		 (minor-add-to-keymap 'keyboard-mode map 'minibuffer-local-map)
;		 (minor-add-to-keymap 'keyboard-mode map
;				      'minibuffer-local-completion-map)
;		 (minor-add-to-keymap 'keyboard-mode map
;				      'minibuffer-local-must-match-map)))
	   )
	 (minor-set-variable 'keyboard-mode 'keyboard-mode t)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun keyboard-map-function (arg)
  "Keyboard mapping function.

Insert the string the current key has been mapped into.  When called
with an argument, insert string that many times.  If keyboard-double
is not nil, call original binding for this key that many times
instead. 

If keyboard-double is not nil and no argument is given, check if point
is located immediately after string.  In that case, remove the string
before point, and call the original binding of this key."
  (interactive "*P")
  ;; We get the from string by lokking at the keys
  (let ((from (minor-iflemacs
	       (mapconcat (function (lambda (event)
				      (char-to-string
				       (event-to-character event t))))
			   (this-command-keys) "")
	       (this-command-keys)))
	to)
    ;; We get the to string by looking after the from string in all
    ;; the encoding, then chosing the to string when we find the from
    ;; string. 
    (mapcar (function (lambda (entry)
			(if (string-equal (nth (minibuffer-value
						'keyboard-from-entry)
					       entry)
					  from)
			    (setq to (nth (minibuffer-value 'keyboard-to-entry)
					  entry)))))
	    (minibuffer-value 'keyboard-language-entry))
    (cond (arg
	   (let ((count (prefix-numeric-value arg)))
	     (while (> count 0)
	       (if (minibuffer-value 'keyboard-double)
		   (minor-call-shadow 'keyboard-mode from)
		 (insert to))
	       (setq count (- count 1)))))
	  ((and (minibuffer-value 'keyboard-double)
		(save-excursion
		  (search-backward to (- (point) (length to)) t)))
	   (replace-match "" t t)
	   (minor-call-shadow 'keyboard-mode from))
	  (t
	   (insert to)))))

(defun keyboard-set-mapping (language from to double)
  "Choose language and encodings for keyboard mapping."

  ;; Turn it of.
  (if keyboard-mode
      (keyboard-mode -1))

  ;; Set the entries
  (let ((entry (assoc language language-encodings)))
    (setq keyboard-language-entry (nthcdr 4 entry))
    (setq keyboard-from-entry (list-index from (nth 3 entry)))
    (setq keyboard-to-entry (list-index to (nth 3 entry)))
    (setq keyboard-language-name (nth 1 entry))
    (setq keyboard-from-name (nth keyboard-from-entry (nth 2 entry)))
    (setq keyboard-to-name (nth keyboard-to-entry (nth 2 entry)))
    (setq keyboard-double double)))

(defun keyboard-query-mapping ()
  "Query the user for what mapping to use, and use it."
  (interactive)
  (let* ((completion-ignore-case t)
	 (default-language (car (car language-encodings)))
	 (language-answer (completing-read (concat "Language: (default "
						   default-language ") ")
					   language-encodings nil t nil))
	 (language (if (> (length language-answer) 0)
		       language-answer
		     default-language))
	 (entry (assoc language language-encodings))
	 (encodings (nth 3 entry))
	 (default-from (car encodings))
	 (from-answer (completing-read (concat "Keyboard: (default "
					       default-from ") ")
				       (mapcar 'list encodings) nil t nil))
	 (from (if (> (length from-answer) 0) from-answer default-from))
	 (default-to (car encodings))
	 (to-answer (completing-read (concat "Screen: (default "
					     default-to ") ")
				     (mapcar 'list encodings) nil t nil))
	 (to (if (> (length to-answer) 0) to-answer default-to))
	 (double (y-or-n-p "Get original by pressing a key twice? ")))

    (keyboard-set-mapping language from to double)
    (keyboard-mode 1)))

;;; @@ Conversion
;;;
;;; This is not the optimal way to do character convertions, since we
;;; really do not want to ask about the language in many cases.
;;; However, since we already have the table we can as well provide
;;; the conversion functions.

(defun convert-character-encoding (begin end language from-set to-set)
  "Convert text between two encodings of the same language.

Tekst between BEGIN and END is assumed to be written in LANGUGE with
the character encoding FROM, and will be converted to the character
encoding TO."
  (let* ((language-entry (assoc language language-encodings))
	 (encodings (nth 3 language-entry))
	 (from (list-index from-set encodings))
	 (to (list-index to-set encodings))
	 (codes (nthcdr 4 language-entry))
	 (entry codes)
	 (string ""))
    (while entry
      (if (not (string-equal "" string))
	  (setq string (concat string "\\|")))
      (setq string (concat string (regexp-quote (nth from (car entry)))))
      (setq entry (cdr entry)))

    (save-excursion
      (goto-char begin)
      (while (re-search-forward string nil t)
	(goto-char (match-end 0))
	(let ((string (buffer-substring (match-beginning 0) (match-end 0)))
	      (entry codes))
	  (while entry
	    (if (string-equal (nth from (car entry)) string)
		(replace-match (nth to (car entry)) t t))
	    (setq entry (cdr entry))))))))

(defun convert-character-query (begin end)
  "Convert characters in reqion, query user for what conversion to use."
  (interactive "*r")
  (let* ((completion-ignore-case t)
	 (default-language (car (car language-encodings)))
	 (language-answer (completing-read (concat "Language: (default "
						   default-language ") ")
					   language-encodings nil t nil))
	 (language (if (> (length language-answer) 0)
		       language-answer
		     default-language))
	 (entry (assoc language language-encodings))
	 (encodings (nth 3 entry))
	 (default-from (car encodings))
	 (from-answer (completing-read (concat "From: (default "
					       default-from ") ")
				       (mapcar 'list encodings) nil t nil))
	 (from (if (> (length from-answer) 0) from-answer default-from))
	 (default-to (car encodings))
	 (to-answer (completing-read (concat "To: (default "
					     default-to ") ")
				     (mapcar 'list encodings) nil t nil))
	 (to (if (> (length to-answer) 0) to-answer default-to)))
    (convert-character-encoding begin end language from to)))

;;; @@ Miscellaneous

(defun list-index (element list)
  "Return the position of ELEMENT in LIST.
First position is 0."
  (let ((count 0))
    (while (not (equal element (car list)))
      (if (null list)
	  (error "`%s' not found." element))
      (setq count (+ count 1))
      (setq list (cdr list)))
    count))

(defun minibuffer-value (symbol)
  "Return the value of symbol.
If this is evaluated in the minibuffer, use value from other window."
  (if (eq (minibuffer-window) (selected-window))
      (save-window-excursion
        (other-window 1)
        (symbol-value symbol))
    (symbol-value symbol)))

;;; @@ Emacs

(run-hooks 'after-min-key-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
