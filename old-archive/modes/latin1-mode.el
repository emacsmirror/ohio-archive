;;; latin1-mode.el --- minor mode for language specific floating accents

;; Copyright (C) 1994 Bernd Petersohn

;; Author: Bernd Petersohn <muecke@cs.tu-berlin.de>
;; Created: 9 August 1994
;; Version: latin1-mode.el,v 2.7 1994/10/07 19:43:01 bp Exp
;; Keywords: i18n

;; This program is intended to be used with GNU Emacs version 19.23 or
;; higher.  Lucid-Emacs/XEmacs is not supported (yet).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; latin1-mode|Bernd Petersohn|muecke@cs.tu-berlin.de|
;; Adjustable, language specific floating accents for ISO Latin 1|
;; 07-Oct-1994|2.7|~/modes/latin1-mode.el.gz|

;;; Commentary:

;; Before you read further: This mode is for typing accented ISO
;; 8859/1 characters on keyboards that do not support them.  If you
;; are looking for keyboard layout mappers, stuff that supports the
;; accent keys on your special keyboard, or for translators of cryptic
;; accent character encodings and the like, you will not be pleased by
;; `latin1-mode'.  There are other packages around that may match your
;; needs better.

;; The latin1 minor mode implements floating accents -- or "dead
;; typewriter keys" -- for ISO 8859/1 characters in a way similar to
;; the one `iso-accents-mode' uses, but with the enhancement that the
;; set of composible characters can be restricted to the specific
;; character set of one or more European languages.  Convenient key
;; stroke commands and a menu bar entry are provided for the purpose
;; to enable, disable, modify, or define the character set of any
;; language or special purpose character subset.  This not only means
;; that you can restrict the composible characters to the set used by
;; your preferred language, but also that you can enable a particular
;; language occasionally, just to write a foreign word, for example.

;; Installation

;; Put this file somewhere in your load path and byte compile it (use
;; M-x byte-compile-file).  Insert an auto-load entry into your .emacs
;; file:
;;
;;	(autoload 'latin1-mode "latin1-mode" "Enable floating accents" t)
;;
;; Also bind some key to this function.	 Example:
;;
;;	(global-set-key [?\C-c ?l] 'latin1-mode)
;;	(define-key-after menu-bar-edit-menu [latin1-mode]
;;	   '("Toggle Floating Accents" . latin1-mode) 'spell)
;;
;; The command `turn-on-latin1-mode' can be used to enable
;; `latin1-mode' automatically in some major mode.  Example:
;;
;;	(autoload 'turn-on-latin1-mode "latin1-mode" "" t)
;;	(add-hook 'mail-mode-hook 'turn-on-latin1-mode)
;;
;; `latin1-mode' tries to find out a suitable default language based
;; on the domain name of your system.  If that fails -- or if that is
;; not what you want -- fix the default character set of your choice
;; in your .emacs file.  Example:
;;
;;	(setq latin1-language-selection
;;	      '(latin1-French latin1-Guillemot-Quotes))
;;
;; See the customization section below to determine which languages
;; are defined.  Don't put languages on this list that you need only
;; occasionally.  The menu bar entry and the commands
;; `latin1-add-language' and `latin1-remove-language' provide a
;; convenient method to change this list temporarily.  It is also
;; possible to define your own private character set; see the
;; documentation of the variable `latin1-language-selection' for
;; details.  The command `latin1-new-language' can be used to do this
;; conveniently in the case `latin1-mode' has already been loaded.

;; After installation

;; Ensure that your Emacs is set up to display 8 bit characters.
;; You may need to type M-x standard-display-european first.

;; Invoke `latin1-mode' in some buffer.  Use C-h f latin1-mode to read
;; the documentation.  Use M-x latin1-describe-translations or the
;; appropriate menu bar entry to see which two-character-sequences
;; form the ISO Latin 1 characters you wish to type.  The command
;; `latin1-list-languages' may be used to display the character sets
;; of all defined languages.

;; Normally there is no need to invoke `latin1-mode' in the minibuffer
;; explicitly, because the minibuffer is automatically put into
;; `latin1-mode' whenever it is visited from a buffer which has
;; `latin1-mode' enabled.

;;; Code:

;; Customization

(defvar latin1-language-selection
  ;; Try to find out a good default value from the domain part of the
  ;; system name.  The domain name list is stolen from "mail-extr.el".
  (if (string-match "\\.\\([a-zA-Z]+\\)$" (system-name))
      (cdr-safe (assoc (substring (system-name) (match-beginning 1))
		       '(("ar" '(latin1-Spanish)) ; Argentina
			 ("at" '(latin1-German)) ; Austria
			 ("be" '(latin1-French latin1-Flemish)) ; Belgium
			 ("bo" '(latin1-Spanish)) ; Bolivia
			 ("br" '(latin1-Portuguese)) ; Brazil
			 ("ca" '(latin1-French)) ; Canada
			 ("ch" '(latin1-Swiss-German
				 latin1-French
				 latin1-Italian
				 latin1-Rhaeto-Romanic)) ; Switzerland
			 ("cl" '(latin1-Spanish)) ; Chile
			 ("co" '(latin1-Spanish)) ; Columbia
			 ("cr" '(latin1-Spanish)) ; Costa Rica
			 ("de" '(latin1-German)) ; Germany
			 ("dk" '(latin1-Danish)) ; Denmark
			 ("do" '(latin1-Spanish)) ; Dominican Republic
			 ("ec" '(latin1-Spanish)) ; Ecuador
			 ("es" '(latin1-Spanish)) ; Spain
			 ("fi" '(latin1-Finnish)) ; Finland
			 ("fr" '(latin1-French)) ; France
			 ("ie" '(latin1-Irish)) ; Ireland
			 ("is" '(latin1-Icelandic)) ; Iceland
			 ("it" '(latin1-Italian)) ; Italy
			 ("mx" '(latin1-Spanish)) ; Mexico
			 ("ni" '(latin1-Spanish)) ; Nicaragua
			 ("nl" '(latin1-Dutch)) ; Netherlands
			 ("no" '(latin1-Norwegian)) ; Norway
			 ("pe" '(latin1-Spanish)) ; Peru
			 ("pt" '(latin1-Portuguese)) ; Portugal
			 ("py" '(latin1-Spanish)) ; Paraguay
			 ("se" '(latin1-Swedish)) ; Sweden
			 ("uk" '(latin1-Irish
				 latin1-Scottish
				 latin1-Welsh)) ; United Kingdom
			 ("uy" '(latin1-Portuguese ; ???
				 latin1-Spanish)) ; Uruguay
			 ("ve" '(latin1-Spanish)) ; Venezuela
			 ))))
  
  "A list of language specific character sets to be used in `latin1-mode'.

This list should contain variable symbols that refer to the strings
which describe the ISO 8859/1 characters that should be composible in
`latin1-mode'. Example:

	(setq latin1-language-selection
	      '(latin1-French latin1-Guillemot-Quotes))

The variable symbols should either be chosen from the ones provided
with `latin1-mode', or they should be named `latin1-<Name>', where
<Name> starts with an uppercase ASCII character in the range A..Z.
Character set description variables that retain this naming convention
will show up in the menu bar menu and can be managed by the
`latin1-mode' functions that add or remove character sets to or from
this list.  Example:

	(defvar latin1-Math \"\367\267\327\254\261\271\262\263\")

If you want to define a character set after `latin1-mode' has already
been loaded, do that with \\[latin1-new-language], because this
command also updates the menu entries and the completion list which is
used by some commands in a mandatory manner.")

(defvar latin1-ignore-space-accents '(?\" ?\' ?^ ?` ?~ ?_ ?< ?>)
  "List of accents swallowing a blank character typed after them.

If a currently used translation sequence starts with one of these
characters, you can press the space bar to abort the translation and
to let the character stand for itself.	This feature is especially
useful at the beginning of quoted string constants and the like.")

(defvar latin1-mode-activate-hook nil
  "Run after `latin1-mode' has been enabled.")

(defvar latin1-mode-deactivate-hook nil
  "Run after `latin1-mode' has been disabled.")

;; The following variables describe the character subsets for specific
;; languages or groups of characters.  For some languages, the given
;; values may be not quite correct.  I would appreciate any hints
;; for improvements.

;; Please note the special naming convention:  Each of these variables
;; starts with the prefix "latin1-" followed by an *uppercase* ASCII
;; letter.  All symbols with such names will appear in the menu bar
;; menu for character set selection.  This means, you can provide your
;; own variables and they will show up in the menu, as long as you
;; retain the same naming convention.

(defvar latin1-Albanian "\313\353\307\347"
  "`latin1-mode' ISO 8859/1 character subset used in Albanian.")

(defvar latin1-Breton
  "\310\350\311\351\312\352\317\357\321\361\331\371\334\374"
  "`latin1-mode' ISO 8859/1 character subset used in Breton.")

(defvar latin1-Catalan
  "\300\340\307\347\311\351\315\355\317\357\322\362\323\363\332\372\334\374"
  "`latin1-mode' ISO 8859/1 character subset used in Catalan.")

(defvar latin1-Danish "\305\345\306\346\330\370"
  "`latin1-mode' ISO 8859/1 character subset used in Danish.")

(defvar latin1-Dutch
  "\301\341\311\351\313\353\315\355\317\357\323\363\326\366\332\372"
  "`latin1-mode' ISO 8859/1 character subset used in Dutch.")

(defvar latin1-Estonian "\304\344\326\366\325\365\334\374"
  "`latin1-mode' ISO 8859/1 character subset used in Estonian.")

(defvar latin1-Faroese
  "\301\341\315\355\323\363\332\372\335\375\306\346\320\360\330\370"
  "`latin1-mode' ISO 8859/1 character subset used in Faroese.")

(defvar latin1-Finnish "\304\344\326\366"
  "`latin1-mode' ISO 8859/1 character subset used in Finnish.")

(defvar latin1-Flemish
  "\301\341\311\351\313\353\315\355\317\357\323\363\326\366\332\372"
  "`latin1-mode' ISO 8859/1 character subset used in Flemish.")

(defvar latin1-French
  (concat "\300\340\302\342\307\347\310\350\311\351\312\352\313"
	  "\353\316\356\317\357\324\364\331\371\333\373\334\374")
  "`latin1-mode' ISO 8859/1 character subset used in French.")

(defvar latin1-Frisian "\302\342\312\352\316\356\324\364\333\373"
  "`latin1-mode' ISO 8859/1 character subset used in Frisian.")

(defvar latin1-German "\304\344\326\366\334\374\337"
  "`latin1-mode' ISO 8859/1 character subset used in German.")

(defvar latin1-Swiss-German "\304\344\326\366\334\374"
  "`latin1-mode' ISO 8859/1 character subset used in Swiss German.")

(defvar latin1-Icelandic
  (concat "\301\341\311\351\315\355\323\363\326\366"
	  "\332\372\335\375\306\346\320\360\336\376")
  "`latin1-mode' ISO 8859/1 character subset used in Icelandic.")

(defvar latin1-Irish "\301\341\311\351\315\355\323\363\332\372"
  "`latin1-mode' ISO 8859/1 character subset used in Irish.")

(defvar latin1-Italian "\300\340\310\350\314\354\322\362\331\371"
  "`latin1-mode' ISO 8859/1 character subset used in Italian.")

(defvar latin1-Luxembourgian
  "\302\342\311\351\312\352\316\356\324\364\333\373"
  "`latin1-mode' ISO 8859/1 character subset used in Luxembourgian.")

(defvar latin1-Norwegian "\305\345\306\346\330\370"
  "`latin1-mode' ISO 8859/1 character subset used in Norwegian.")

(defvar latin1-Portuguese
  (concat "\300\340\301\341\302\342\303\343\307\347\310\350"
	  "\311\351\312\352\313\353\315\355\317\357\322\362"
	  "\323\363\324\364\325\365\332\372\334\374")
  "`latin1-mode' ISO 8859/1 character subset used in Portuguese.")

(defvar latin1-Provencal "\300\340\307\347\310\350\311\351"
  "`latin1-mode' ISO 8859/1 character subset used in Provencal.")

(defvar latin1-Rhaeto-Romanic
  (concat "\300\340\302\342\310\350\311\351\312\352\316\356"
	  "\317\357\322\362\324\364\326\366\331\371\334\374")
  "`latin1-mode' ISO 8859/1 character subset used in Rhaeto-Romanic.")

(defvar latin1-Scottish "\301\341\311\351\314\354\322\362\323\363\331\371"
  "`latin1-mode' ISO 8859/1 character subset used in Scottish.")

(defvar latin1-Spanish
  "\301\341\311\351\315\355\321\361\323\363\332\372\334\374\241\277"
  "`latin1-mode' ISO 8859/1 character subset used in Spanish.")

(defvar latin1-Swedish "\304\344\326\366\305\345"
  "`latin1-mode' ISO 8859/1 character subset used in Swedish.")

(defvar latin1-Welsh
  (concat "\301\341\302\342\304\344\310\350\311\351\312\352\313\353\316"
	  "\356\317\357\324\364\326\366\331\371\333\373\334\374\377")
  "`latin1-mode' ISO 8859/1 character subset used in Welsh.")

(defvar latin1-Guillemot-Quotes "\253\273"
  "`latin1-mode' ISO 8859/1 character subset for guillemot quotes.")

(defvar latin1-Symbol
  (concat "\240\241\242\243\244\245\246\247\250\251\252\253\254"
	  "\255\256\257\260\261\262\263\264\265\266\267\270\271"
	  "\272\273\274\275\276\277\327\367")
  "`latin1-mode' ISO 8859/1 symbol character subset.")

(defvar latin1-Letter
  (concat "\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317"
	  "\320\321\322\323\324\325\326\330\331\332\333\334\335\336\337\340"
	  "\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360"
	  "\361\362\363\364\365\366\370\371\372\373\374\375\376\377")
  "`latin1-mode' ISO 8859/1 alphabetic character subset.")

;; This sparse keymap defines the two character translation sequences
;; for all ISO 8859/1 characters.  The actually used keymap is a copy
;; of a subset of these bindings.

(defvar latin1-mother-map nil
  "Sparse keymap that defines translations for all 8 bit ISO 8859/1 chars.
Translations are two character ASCII key sequences, bindings are the
composible characters themselves as integers.

Use `eval-after-load' if you want to modify this map.  Example:

  	(defun my-latin1-mode-translations ()
  	  (define-key latin1-mother-map \"sz\" 223) ; s-sharp
  	  (define-key latin1-mother-map \"\\\"s\" nil))
      
  	(and (boundp 'latin1-mother-map) (keymapp latin1-mother-map)
  	     (my-latin1-mode-translations))
	(eval-after-load \"latin1-mode\" '(my-latin1-mode-translations))

This works even in the case `latin1-mode' is pre-loaded.
Multiple entries for one character are allowed.")

(defvar latin1-mode-map nil
  "Sparse minor mode keymap used in `latin1-mode'.
Merely used to hold the menu bar entry.")

(defvar latin1-mode-menu nil
  "Sparse menu keymap used for the menu bar entry of `latin1-mode'.")

;;;; End of customization section

(defvar latin1-add-language-menu nil
  "Sub-menu of `latin1-mode-menu' used to enable particular languages.
Don't modify this map.  Its entries are automatically generated.")

(defvar latin1-remove-language-menu nil
  "Sub-menu of `latin1-mode-menu' used to disable particular languages.
Don't modify this map.  Its entries are automatically generated.")


;; Initialize translation keymap

(if (keymapp latin1-mother-map)
    ()
  (setq latin1-mother-map (make-sparse-keymap))
  (define-key latin1-mother-map "//" 160) ; nobreakspace
  (define-key latin1-mother-map "!!" 161) ; exclamdown
  (define-key latin1-mother-map "/c" 162) ; cent
  (define-key latin1-mother-map "/L" 163) ; sterling
  (define-key latin1-mother-map "/$" 164) ; currency
  (define-key latin1-mother-map "/Y" 165) ; yen
  (define-key latin1-mother-map "/|" 166) ; brokenbar
  (define-key latin1-mother-map "/S" 167) ; section
  (define-key latin1-mother-map "\"\"" 168) ; diaeresis
  (define-key latin1-mother-map "/C" 169) ; copyright
  (define-key latin1-mother-map "_a" 170) ; ordfeminine
  (define-key latin1-mother-map "<<" 171) ; guillemotleft
  (define-key latin1-mother-map "/~" 172) ; notsign
  (define-key latin1-mother-map "/-" 173) ; hyphen
  (define-key latin1-mother-map "/R" 174) ; registered
  (define-key latin1-mother-map "^-" 175) ; macron
  (define-key latin1-mother-map "^^" 176) ; degree
  (define-key latin1-mother-map "+-" 177) ; plusminus
  (define-key latin1-mother-map "^2" 178) ; twosuperior
  (define-key latin1-mother-map "^3" 179) ; threesuperior
  (define-key latin1-mother-map "''" 180) ; acute
  (define-key latin1-mother-map "/u" 181) ; mu
  (define-key latin1-mother-map "/P" 182) ; paragraph
  (define-key latin1-mother-map "^." 183) ; periodcentered
  (define-key latin1-mother-map ",," 184) ; cedilla
  (define-key latin1-mother-map "^1" 185) ; onesuperior
  (define-key latin1-mother-map "_o" 186) ; masculine
  (define-key latin1-mother-map ">>" 187) ; guillemotright
  (define-key latin1-mother-map "/4" 188) ; onequarter
  (define-key latin1-mother-map "/2" 189) ; onehalf
  (define-key latin1-mother-map "/3" 190) ; threequarters
  (define-key latin1-mother-map "??" 191) ; questiondown
  (define-key latin1-mother-map "`A" 192) ; Agrave
  (define-key latin1-mother-map "'A" 193) ; Aacute
  (define-key latin1-mother-map "^A" 194) ; Acircumflex
  (define-key latin1-mother-map "~A" 195) ; Atilde
  (define-key latin1-mother-map "\"A" 196) ; Adiaeresis
  (define-key latin1-mother-map "AA" 197) ; Aring
  (define-key latin1-mother-map "Aa" 197) ; Aring
  (define-key latin1-mother-map "AE" 198) ; AE
  (define-key latin1-mother-map "Ae" 198) ; AE
  (define-key latin1-mother-map ",C" 199) ; Ccedilla
  (define-key latin1-mother-map "`E" 200) ; Egrave
  (define-key latin1-mother-map "'E" 201) ; Eacute
  (define-key latin1-mother-map "^E" 202) ; Ecircumflex
  (define-key latin1-mother-map "\"E" 203) ; Ediaeresis
  (define-key latin1-mother-map "`I" 204) ; Igrave
  (define-key latin1-mother-map "'I" 205) ; Iacute
  (define-key latin1-mother-map "^I" 206) ; Icircumflex
  (define-key latin1-mother-map "\"I" 207) ; Idiaeresis
  (define-key latin1-mother-map "DH" 208) ; ETH
  (define-key latin1-mother-map "Dh" 208) ; ETH
  (define-key latin1-mother-map "~N" 209) ; Ntilde
  (define-key latin1-mother-map "`O" 210) ; Ograve
  (define-key latin1-mother-map "'O" 211) ; Oacute
  (define-key latin1-mother-map "^O" 212) ; Ocircumflex
  (define-key latin1-mother-map "~O" 213) ; Otilde
  (define-key latin1-mother-map "\"O" 214) ; Odiaeresis
  (define-key latin1-mother-map "/\\" 215) ; multiply
  (define-key latin1-mother-map "/O" 216) ; Ooblique
  (define-key latin1-mother-map "`U" 217) ; Ugrave
  (define-key latin1-mother-map "'U" 218) ; Uacute
  (define-key latin1-mother-map "^U" 219) ; Ucircumflex
  (define-key latin1-mother-map "\"U" 220) ; Udiaeresis
  (define-key latin1-mother-map "'Y" 221) ; Yacute
  (define-key latin1-mother-map "TH" 222) ; THORN
  (define-key latin1-mother-map "Th" 222) ; THORN
  (define-key latin1-mother-map "\"s" 223) ; ssharp
  (define-key latin1-mother-map "`a" 224) ; agrave
  (define-key latin1-mother-map "'a" 225) ; aacute
  (define-key latin1-mother-map "^a" 226) ; acircumflex
  (define-key latin1-mother-map "~a" 227) ; atilde
  (define-key latin1-mother-map "\"a" 228) ; adiaeresis
  (define-key latin1-mother-map "aa" 229) ; aring
  (define-key latin1-mother-map "ae" 230) ; ae
  (define-key latin1-mother-map ",c" 231) ; ccedilla
  (define-key latin1-mother-map "`e" 232) ; egrave
  (define-key latin1-mother-map "'e" 233) ; eacute
  (define-key latin1-mother-map "^e" 234) ; ecircumflex
  (define-key latin1-mother-map "\"e" 235) ; ediaeresis
  (define-key latin1-mother-map "`i" 236) ; igrave
  (define-key latin1-mother-map "'i" 237) ; iacute
  (define-key latin1-mother-map "^i" 238) ; icircumflex
  (define-key latin1-mother-map "\"i" 239) ; idiaeresis
  (define-key latin1-mother-map "dh" 240) ; eth
  (define-key latin1-mother-map "~n" 241) ; ntilde
  (define-key latin1-mother-map "`o" 242) ; ograve
  (define-key latin1-mother-map "'o" 243) ; oacute
  (define-key latin1-mother-map "^o" 244) ; ocircumflex
  (define-key latin1-mother-map "~o" 245) ; otilde
  (define-key latin1-mother-map "\"o" 246) ; odiaeresis
  (define-key latin1-mother-map "/:" 247) ; division
  (define-key latin1-mother-map "/o" 248) ; oslash
  (define-key latin1-mother-map "`u" 249) ; ugrave
  (define-key latin1-mother-map "'u" 250) ; uacute
  (define-key latin1-mother-map "^u" 251) ; ucircumflex
  (define-key latin1-mother-map "\"u" 252) ; udiaeresis
  (define-key latin1-mother-map "'y" 253) ; yacute
  (define-key latin1-mother-map "th" 254) ; thorn
  (define-key latin1-mother-map "\"y" 255) ; ydiaeresis
  )

;; An alist of the form ((LANGUAGE-NAME . LANGUAGE-SYMBOL)...) follows
;; here that describes all known language symbols and which is mainly
;; used for completion.  It is created when it is needed, or when
;; `latin1-mode' is invoked the first time, by the function with the
;; same name.  This kind of deferred generation allows `latin1-mode'
;; to be pre-loaded even if the user defines new language symbols in
;; his or her initialization file.

(defvar latin1-language-symbols nil)

(defun latin1-language-symbols ()
  (or latin1-language-symbols
      (let (list case-fold-search)
	(mapatoms
	 (function
	  (lambda (sym)
	    (if (and (boundp sym)
		     (string-match "^latin1-\\([A-Z]\\)" (symbol-name sym)))
		(let ((name (substring (symbol-name sym) (match-beginning 1))))
		  (setq list (cons (cons name sym) list)))))))
	(setq latin1-language-symbols
	      (sort list (function (lambda (a b) (string< (car a) (car b))))))
	;; Also make the menu entries.  `latin1-add-menu-item' is
	;; defined below.
	(mapcar 'latin1-add-menu-item (reverse (latin1-language-symbols))))))
	
;; Commands to add and remove language symbols from the current
;; selection 

;; This history list is only used when adding languages.  It is made
;; up from the elements removed from `latin1-language-selection'.
(defvar latin1-remove-language-history nil)

;;;###autoload
(defun latin1-add-language (lang)
  "Add a `latin1-mode' language symbol to the current language selection.

The language name is read from the minibuffer (with completion and
history).  You must specify a language for which a `latin1-mode'
language symbol exists and you cannot specify a language which is
already part of the current language selection.

Called from a program, LANG must be a valid `latin1-mode' language
symbol."

  (interactive
   (let* ((completion-ignore-case t)
	  ;; Remove non-selectable items from history list
	  (history (apply 'append
			  '("")		; avoid empty list
			  (mapcar
			   (function
			    (lambda (str)
			      (let ((cell (assoc str
						 (latin1-language-symbols))))
				(and cell
				     (not (memq (cdr cell)
						latin1-language-selection))
				     (list str)))))
			   latin1-remove-language-history)))
	  (input (completing-read "Add language: "
				  (latin1-language-symbols)
				  (function
				   (lambda (cell)
				     (not (memq (cdr cell)
						latin1-language-selection))))
				  'exact
				  (and (>= (length history) 2)
				       (cons (car (cdr history)) 0))
				  '(history . 2)))) ; this is the first entry
     (list (cdr-safe (assoc input (latin1-language-symbols))))))
  (if lang
      (let ((name (car-safe (rassq lang (latin1-language-symbols)))))
	(setq latin1-language-selection
	      (cons lang (delq lang latin1-language-selection)))
	(latin1-update-maps)
	(message "%s characters added to language selection" name))))

;;;###autoload
(defun latin1-remove-language (lang)
  "Remove a `latin1-mode' language symbol from the current language selection.

The language name is read from the minibuffer (with completion and
history).  You must specify a language for which a `latin1-mode'
language symbol exists and you cannot specify a language which is not
part of the current language selection.

Called from a program, LANG must be a valid `latin1-mode' language
symbol."

  (interactive
   (let* ((completion-ignore-case t)
	  ;; Make up a history list of the currently used languages
	  (history (apply 'append
			  '("")		; avoid empty list
			  (mapcar
			   (function
			    (lambda (sym)
			      (let ((cell (rassq sym
						 (latin1-language-symbols))))
				(and cell (list (car cell))))))
			   latin1-language-selection)))
	  (input (completing-read "Remove language: "
				  (latin1-language-symbols)
				  (function
				   (lambda (cell)
				     (memq (cdr cell)
					   latin1-language-selection)))
				  'exact
				  (and (>= (length history) 2)
				       (cons (car (cdr history)) 0))
				  '(history . 2)))) ; this is the first entry
     (list (cdr-safe (assoc input (latin1-language-symbols))))))
  (if lang
      (let ((name (car-safe (rassq lang (latin1-language-symbols)))))
	(setq latin1-language-selection
	      (delq lang latin1-language-selection)
	      ;; Put this language on top of the history list for
	      ;; `latin1-add-language'.
	      latin1-remove-language-history
	      (cons name (delete name latin1-remove-language-history)))
	(latin1-update-maps)
	(message "%s characters removed from language selection" name))))

;; Create a menu bar menu for language selection.  It consists of a
;; top-level menu with two sub menus to enable or to disable a
;; specific language.

(defun latin1-add-menu-item (lang)
  ;; LANG is a (NAME . SYMBOL) cell as in `latin1-language-symbols'.
  ;; Add entries in the add- and remove-language menus for this language.
  ;; The commands to select languages are artificially created.
  ;; Their names begin with large letters in order to avoid overloaded
  ;; completion help buffers.
  (let ((symbol (cdr lang))
	(name (car lang))
	command)
    ;; Create a symbol `Latin1-add-<name>' with the function value of
    ;; a command that adds language <name> to
    ;; `latin1-language-selection'.  Give the function symbol a
    ;; menu-enable property that evaluates to t, if sym is not part of
    ;; `latin1-language-selection'.
    
    (setq command (intern (concat "Latin1-add-" name)))
    (fset command (list 'lambda '() '(interactive)
			(list 'latin1-add-language (list 'quote symbol))))
    (put command 'menu-enable
	 (list 'not (list 'memq (list 'quote symbol)
			  'latin1-language-selection)))
    (define-key latin1-add-language-menu (vector command) (cons name command))
    
    ;; Create a symbol `Latin1-remove-<name>' with the function value
    ;; of a command that removes language <name> from
    ;; `latin1-language-selection'.  Give the function symbol a
    ;; menu-enable property that evaluates to t, if sym is already
    ;; part of `latin1-language-selection'.
    
    (setq command (intern (concat "Latin1-remove-" name)))
    (fset command (list 'lambda '() '(interactive)
			(list 'latin1-remove-language (list 'quote symbol))))
    (put command 'menu-enable
	 (list 'memq (list 'quote symbol) 'latin1-language-selection))
    (define-key latin1-remove-language-menu
      (vector command) (cons name command))))

;; The items of these menus are defined by the function
;; `latin1-language-symbols':
(or (keymapp latin1-add-language-menu)
    (setq latin1-add-language-menu (make-sparse-keymap "Add Language")))
(or (keymapp latin1-remove-language-menu)
    (setq latin1-remove-language-menu (make-sparse-keymap "Remove Language")))

;; The menu itself
(if (keymapp latin1-mode-menu)
    ()
  (setq latin1-mode-menu (make-sparse-keymap "Latin1"))
  (define-key latin1-mode-menu [new-language]
    '("Define or Modify Language..." . latin1-new-language))
  (define-key latin1-mode-menu [separator-new]
    '("--"))
  (define-key latin1-mode-menu [remove-language]
    (cons "Remove Language" latin1-remove-language-menu))
  (define-key latin1-mode-menu [add-language]
    (cons "Add Language" latin1-add-language-menu))
  (define-key latin1-mode-menu [separator-enable]
    '("--"))
  (define-key latin1-mode-menu [describe-function]
    '("Help on This Mode" . (lambda () (interactive)
			      (describe-function 'latin1-mode))))
  (define-key latin1-mode-menu [list-languages]
    '("List Language Character Sets" . latin1-list-languages))
  (define-key latin1-mode-menu [describe-translations]
    '("Describe Accent Translations" . latin1-describe-translations)))

;; The minor mode map
(if (keymapp latin1-mode-map)
    ()
  (setq latin1-mode-map (make-sparse-keymap))
  (define-key latin1-mode-map [menu-bar latin1]
    (cons "Latin1" latin1-mode-menu))
  (setq minor-mode-map-alist
	(cons (cons 'latin1-mode latin1-mode-map)
	      (delq (assq 'latin1-mode minor-mode-map-alist)
		    minor-mode-map-alist))))

;; This is the buffer local variable that holds the current state of
;; `latin-mode'.
(defvar latin1-mode nil)
(make-variable-buffer-local 'latin1-mode)
(setq-default latin1-mode nil)

;; This is the currently used translation keymap
;; (a subset of `latin1-mother-map')
(defvar latin1-current-translations nil)

(defun latin1-traverse-map (function keymap)
  ;; Traverse the translation sequence definitions in KEYMAP and call
  ;; FUNCTION on each two-key-binding with the three arguments PREFIX,
  ;; SUFFIX, and BINDING.
  (mapcar
   (function
    (lambda (map)
      ;; Only singleton vector prefixes are of interest here
      (if (= 1 (length (car map)))
	  (let ((prefix (aref (car map) 0)))
	    (mapcar
	     (function
	      (lambda (entry)
		(let ((suffix (car entry))
		      (binding (cdr entry)))
		  (if (integerp binding)
		      (funcall function prefix suffix binding)))))
	     ;; Rest comes after [prefix] and 'keymap
	     (nthcdr 2 map))))))
   ;; The first accessible keymap is the map itself and uninteresting
   (and (keymapp keymap) (cdr (accessible-keymaps keymap)))))

;; This function translates input events to ISO 8859/1 characters.  It
;; is bound in `key-translation-map' to the prefixes of the actually
;; used translation sequences.  The suffixes are read with
;; `read-event'.  If the input matches a translation sequence, a
;; vector of the translation is returned.  If not, the suffix is put
;; back for later processing and a vector of the prefix is returned.

(defun latin1-compose-keys (&optional prompt)
  (if latin1-mode
      (let* ((prefix last-input-event)	; should be a character
	     ;; set these values for `command-execute'
	     (last-command-event prefix)
	     (last-command-char	 prefix)
	     (last-input-char	 prefix)
	     (prefix-arg nil)
	     (opos (point))
	     (insertion (memq (key-binding "a" t)
			      '(self-insert-command picture-self-insert)))
	     (suffix (if (or prompt (null insertion))
			 (progn
			   (message "%s%c" (or prompt "Compose with ") prefix)
			   (read-event))
		       ;; Display prefix first but put point back in
		       ;; front of it such that the user is notified
		       ;; that she has typed the first character of a
		       ;; translation sequence.
		       ;; Currently we cannot support `picture-mode'
		       ;; because this mode fails to bind 8 bit
		       ;; characters to `picture-self-insert'.
		       (command-execute 'self-insert-command)
		       (goto-char opos)
		       (prog1
			   (read-event)
			 ;; Delete the character again, but in insert
			 ;; mode only.
			 (or overwrite-mode (delete-char 1)))))
	     (keys (vector prefix suffix))
	     (binding (lookup-key latin1-current-translations keys)))
	(if (and (integerp binding) (>= binding 160))
	    (vector binding)
	  ;; Force to evaluate the events normally
	  (or (and (eq suffix ?\ ) (memq prefix latin1-ignore-space-accents))
	      (setq unread-command-events
		    (cons suffix unread-command-events)))
	  (vector prefix)))
    ;; `latin1-mode' is turned off
    (vector last-input-event)))

;; The following function updates `latin1-current-translations' and
;; `latin1-mode-map' in the case `latin1-language-selection' has been
;; changed.

(defun latin1-update-maps ()
  ;; Remove all existing bindings to `latin1-compose-keys' in
  ;; `key-translation-map'
  (if (keymapp key-translation-map)
      (substitute-key-definition 'latin1-compose-keys nil key-translation-map)
    (setq key-translation-map (make-sparse-keymap)))
  ;; Create the keymap for the subset of ISO 8859/1 characters according
  ;; to the description in `latin1-language-selection'
  (let* ((current-map (make-sparse-keymap))
	 (char-list
	  (mapcar 'identity
		  (mapconcat (function
			      (lambda (sym)
				(and (symbolp sym) (boundp sym)
				     (stringp (eval sym)) (eval sym))))
			     latin1-language-selection
			     "")))
	 (prefix-list nil)
	 ;; Traverse all accessible keymaps of `latin1-mother-map'.
	 ;; For each two-character key sequence test whether the
	 ;; binding is a member of the wished character list.  If so,
	 ;; define these keys in `latin1-current-translations' and
	 ;; bind them to this character.
	 (mapper (function
		  (lambda (prefix suffix binding)
		    (if (memq binding char-list)
			(progn
			  (define-key
			    current-map (vector prefix suffix) binding)
			  (setq prefix-list
				(cons prefix (delq prefix prefix-list)))))))))
    ;; Create the subset keymap.
    (latin1-traverse-map mapper latin1-mother-map)
    (setq latin1-current-translations current-map)
    ;; Bind all found prefixes to `latin1-compose-keys' in
    ;; `key-translation-map'
    (mapcar
     (function
      (lambda (char)
	(define-key key-translation-map (vector char) 'latin1-compose-keys)))
     prefix-list)))

;; The mode itself

;;;###autoload
(defun latin1-mode (&optional force)
  "Use floating accents (\"dead typewriter keys\") for ISO Latin 1 characters.
This is a buffer local minor mode.

In `latin1-mode', the keys for characters like ' ` ^ ~ \" behave
similar to the \"dead\" accent keys of a typewriter.  Pressing one of
them causes the corresponding character to show up, but the cursor
does not move.  Then, if you type a character that can have such an
accent, the already displayed accent is replaced by an appropriate ISO
Latin 1 character.  For example, characters like \344, \341, or \361
result from key sequences like \" a, ' a, or ~ n, respectively.

Generally, the following pseudo accent characters are recognized:

	' gives acute accents;
	` gives grave accents;
	\" gives diaeresis accents;
	^ gives circumflex accents;
	~ gives tilde accents;
	, gives cedillas;
	/ gives slash accents and most symbol characters;
	aa and ae give a-ring characters and ae-ligatures, respectively.

Besides the menu bar entries, you can use the following commands:

\\[latin1-describe-translations]
	gives precise information how the currently available ISO
	Latin 1 characters can be composed.
\\[latin1-add-language]
	enables the accent characters of a specific language.
\\[latin1-remove-language]
	disables them again.
\\[latin1-new-language]
	creates or modifies a particular character set.
\\[latin1-list-languages]
	gives precise information which accent characters are part of
	a specific language.

The command `turn-on-latin1-mode' can be put on major mode hooks like
`text-mode-hook' to enable `latin1-mode' unconditionally in these
modes.

For customization, refer to the documentations of the following
variables (Rename this buffer and type \\[describe-variable] on their names):

	latin1-language-selection
	latin1-ignore-space-accents
	latin1-mother-map
	latin1-mode-activate-hook
	latin1-mode-deactivate-hook

Notes on usage:

For convenience, most pseudo accents swallow a blank character typed
afterwards.  Therefore you can press the space bar to terminate the
active state of a floating accent.  See the documentation of
`latin1-ignore-space-accents' for details.

The minibuffer is automatically put into `latin1-mode' whenever it is
consulted from a buffer with `latin1-mode' already turned on.  This
behavior is mostly useful for commands like `query-replace'.

If you invoke `latin1-mode' with a prefix argument, arguments > 0
enable `latin1-mode', arguments <= 0 disable it unconditionally."

  (interactive "P")
  (setq latin1-mode (if (null force)
			(not latin1-mode)
		      (> (prefix-numeric-value force) 0)))
  (if latin1-mode
      (progn
	(if (not (keymapp latin1-current-translations))
	    (latin1-update-maps))
	(latin1-language-symbols)
	(run-hooks 'latin1-mode-activate-hook))
    (run-hooks 'latin1-mode-deactivate-hook))
  (force-mode-line-update))

;;;###autoload
(defun turn-on-latin1-mode ()
  "Unconditionally turn on `latin1-mode' in the current buffer.
See the function documentation of `latin1-mode' for details.
This command is intended to be put on major mode hooks like
`text-mode-hook' etc."
  (interactive)
  (latin1-mode 1))

;; The following function enables `latin1-mode' in the minibuffer
;; whenever it is consulted from a buffer that has `latin1-mode'
;; turned on.  The binding of the `latin1-mode' variable local to the
;; minibuffer is always lost after the minibuffer has been exited.

(defun latin1-mode-minibuffer-setup ()
  (setq latin1-mode (save-excursion
		      (set-buffer (other-buffer nil t))
		      latin1-mode)))

(add-hook 'minibuffer-setup-hook 'latin1-mode-minibuffer-setup)

;; The mode line indicator
(or (assq 'latin1-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(latin1-mode " Lat") minor-mode-alist)))
			 
;; Display a summary of the currently available ISO 8859/1 characters
;; and there translation sequences in the help buffer.

;;;###autoload
(defun latin1-describe-translations (&optional all)
  "Describe floating accent translation sequences for ISO Latin 1 characters.

With optional prefix argument, describe all translation sequences
independent of whether they are currently active or not.  Without
argument, display only those which are currently available.
The description pops up in the *Help* buffer."

  (interactive "P")
  (let* ((alist nil)
	 ;; Create an alist ((ISO-CHAR PREFIX SUFFIX)...) for all
	 ;; defined translation sequences and sort it.
	 (mapper (function
		  (lambda (prefix suffix binding)
		    (setq alist (cons (list binding prefix suffix) alist))))))
    (if all
	(latin1-traverse-map mapper latin1-mother-map)
      (latin1-traverse-map mapper latin1-current-translations))
    (setq alist (sort alist (function (lambda (a b) (< (car a) (car b))))))
    ;; Display a description similar to the ones created by
    ;; `describe-bindings'.
    (with-output-to-temp-buffer "*Help*"
      (princ "`latin1-mode' composition key sequences:\n")
      (princ "keys\tdefinition\n")
      (princ "----\t----------\n\n")
      (mapcar (function
	       (lambda (desc)
		 (let ((prefix (car (cdr desc)))
		       (suffix (car (nthcdr 2 desc)))
		       (binding (car desc)))
		   (princ (format "%c %c\t%c\n" prefix suffix binding)))))
	      alist)
      (print-help-return-message))))

;; For backward compatibility
;;;###autoload
(defalias 'latin1-summary 'latin1-describe-translations)

;;;###autoload
(defun latin1-list-languages ()
  "Describe the ISO 8859/1 characters associated with language symbols.
The description pops up in the *Help* buffer.  Currently selected
languages are marked with a `+' sign."
  (interactive)
  (let ((format "%c %-20s %s\n"))
    (with-output-to-temp-buffer "*Help*"
      (princ "`latin1-mode' language specific character sets:\n")
      (princ (format format ?* "language" "character set"))
      (princ (format format ?- "--------" "-------------\n"))
      (mapcar
       (function
	(lambda (cell)
	  (let ((sym (cdr cell))
		(name (car cell)))
	    (if (and (boundp sym) (stringp (eval sym)))
		(princ (format format
			       (if (memq sym latin1-language-selection) ?+ ?\ )
			       name
			       (eval sym)))))))
       (latin1-language-symbols))
      (print-help-return-message))))

;; History of the following command
(defvar latin1-new-language-history nil)

;;;###autoload
(defun latin1-new-language (symbol charset save)
  "Define a new or modify an existing `latin1-mode' language character set.
Prompts for the language name and for the character set.
`latin1-mode' is enabled in the minibuffer for all ISO Latin1
characters during the input of this character set.

The language is added to the current `latin1-language-selection'.  In
the case a new language symbol is defined, the menu bar menus are
updated to reflect the existence of this new symbol.  In addition, you
are asked whether the definition should be saved in your .emacs file.

Requires three arguments when called from a program: SYMBOL must be a
valid `latin1-mode' language symbol of the form latin1-<Name>, CHARSET
a string that describes the character set, and non-nil SAVE indicates
that the definition should be saved in the user's init file."

  (interactive
   (let ((completion-ignore-case t)
	 (latin1-mode nil)
	 name sym set)
     ;; First read the name
     (setq name (completing-read "Name of requested character set: "
				 (latin1-language-symbols)
				 nil
				 nil
				 nil
				 'latin1-new-language-history))
     ;; Strip off the prefix in the case the user has typed it.
     (if (string-match "^latin1-" name)
	 (setq name (substring name (match-end 0))))
     ;; Name must start with ASCII letter
     (if (string-match "^[^a-zA-Z]" name)
	 (error "Name does not start with an ASCII letter."))
     (setq name (capitalize name))
     ;; Create the symbol and read the character set
     (setq sym (intern (concat "latin1-" name)))
     ;; Enable composition of all characters temporarily
     (let ((latin1-mode t)
	   (latin1-language-selection '(latin1-Letter latin1-Symbol)))
       (latin1-update-maps)
       (setq set (read-from-minibuffer
		  (format "%S characters should be: " name)
		  (and (boundp sym) (stringp (eval sym)) (eval sym)))))
     ;; The arguments
     (list sym set
	   (y-or-n-p "Save this definition for future editing sessions? "))))
   
  (let (name)
    (if (string-match "^latin1-" (symbol-name symbol))
	(progn
	  ;; Extract the name
	  (setq name (substring (symbol-name symbol) (match-end 0)))
	  ;; Update language symbol list
	  (or (assoc name (latin1-language-symbols))
	      (setq latin1-language-symbols
		    (cons (cons name symbol) latin1-language-symbols)))
	  (set symbol charset)
	  ;; Save definition
	  (unwind-protect
	      (if save
		  (save-excursion
		    (set-buffer (find-file-noselect
				 (substitute-in-file-name user-init-file)))
		    ;; Make buffer writable.  Note that the init file
		    ;; may be under version control and then has to be
		    ;; checked out.
		    (and buffer-read-only (vc-toggle-read-only))
		    ;; Look for an already existing definition and
		    ;; replace it, if appropriate.
		    (goto-char (point-min))
		    (if (re-search-forward
			 (format "\\<%s\\>[ \t\n]+\"[\240-\377]+\""
				 (regexp-quote (symbol-name symbol)))
			 nil t)
			(replace-match (format "%S %S" symbol charset) t t)
		      ;; Insert new definition at end of file
		      (goto-char (point-max))
		      (insert (format "\n(setq %S %S)\n" symbol charset)))
		    (save-buffer)
		    ;; If we checked out the file, we ought to check
		    ;; it in again here.  However, that seems to cause
		    ;; problems, so it's better to let the buffer be
		    ;; writable.
		    ))
	    ;; Add menu entries
	    (latin1-add-menu-item (cons name symbol))
	    ;; Enable this character set.  This function also updates
	    ;; the keymaps.
	    (latin1-add-language symbol))))))

(provide 'latin1-mode)

;;; latin1-mode.el ends here
