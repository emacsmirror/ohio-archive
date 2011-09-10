;; @(#) tinypair.el -- self insert character pairs () "" '' <> etc.

;; @(#) $Id: tinypair.el,v 1.18 1995/12/12 07:02:04 jaalto Release_3 jaalto $
;; @(#) $Keywords: char, inserting, pairing $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Nov 8 1995
;;
;; To get information on this program use ident(1) or do M-x tipi-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinypair|Jari Aalto|jari.aalto@ntc.nokia.com|
;; self insert character pairs () "" '' <> etc. User configurable|
;; 12-Dec-1995|1.18|~/misc/tinypair.el.Z|

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
;;	(require 'tinypair)
;;	(tipi-control 'us)		-- select this for US style
;;	(tipi-control 'european)	-- or select this for EUROPEAN style
;;
;; If you want to turn the pairing off, use this:
;;
;;	(tipi-control)

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o  When you hit eg. ", package will double the character. If you
;;    insertion point was on whitespace, the pair is inserted "as is",
;;    but if point was in front of word, the word is surrounded with pair,
;;    provided that there we no pair already.
;; o  Every pair beginning character may have it's own function
;;    to handle the pairing.
;; o  Smart pairing, more efective than 19.29 paired-insert.el


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tipi-" in front of
;;   every function & variable. It stands for '(ti)ny (p)aired char (i)nsert
;; - variable names contain letters 'tipi-:', excecpt version/mode vars.

;; PREFACE
;; ========================================
;; - I saw paired-insert.el posted to gnu.emacs.help group, and
;;   the code was so badly documented that I got frusrated. And when I
;;   finally got it going, it was so dum, that I discarded it right away
;; - The code seemed lot of promises, but in order to get it into level
;;   of "smart" pairing, I would have had to reprogram it.
;; - Instead I started developing my own pairing function, which would
;;   had all the functionality I wanted "buil-in"
;; - Hope you enjoy this another 'tiny tool' package.
;;
;;
;; How it works?
;; ==========================================
;; - When you call function
;;
;;	(tipi-control 'us)
;;
;;   it does some global-set-key bindings automatically, so that
;;   special tipi-self-insert-command is assigned to  beg-pair
;;   keys. You can see this yorself after the command is executed by
;;   calling C-h k <pair-beg-key> (or M-x describe-key <pair-beg-key> )
;;
;; - When you call function without pareameters
;;
;;	(tipi-control)
;;
;;   The bindings are set back to function self-insert-command.
;;
;; Pair control
;; ========================================
;; - REMEMBER: Always ask youself "Does this character the cursor is on,
;;	       belong to _word_ class?",
;;	       when you wonder why the pairing does not take in effect
;;	       around the current character block.
;;
;; - The pair control is turned off for lisp mode, because it makes
;;   things worse if the pairing is on.
;; - The paring in US style includes
;;
;;	`        '
;;
;;   But European people don't use this at all, instead they use
;;
;;	'	'
;;
;; - The pairing is done according to assoc lists in the following way:
;;   o  if there is whitespace in front of char, then pair is inserted
;;   o  if next character is pair-end, no pairing takes effect.
;;      Like if you press opening paren when you're sitting on the
;;      closing paren:
;;
;;	  ()
;;	   *	<-- cursor here, pressing another ( does not pair.
;;
;;	but this behavior can be controlled through variable
;;
;;   o  if the cursor is at the beginning of the word (see syntax-table):
;;      -- if there is no pairs around the word, the whole word is paired.
;;      -- if there is pair, no pairing takes effect. The char acts as
;;         self-insert-command.
;;   o if character is repeated with prefix arg, the pairing isn't done,
;;     instead the character is repeated as in self-insert-command.
;;     -- This can be changed through variable, so that when you give
;;        prefix args, it will pair as follows:
;;
;;        beg-word word word word word. word word word word
;;        *   ,now pressing " would do
;;
;;        "beg-word" word word word word word word word word
;;
;;            ,but giving prefix arg 2 (M-2)
;;        "beg-word word" word word word. word word word word
;;
;;        OR
;;
;;        M-2 ( gives you	-->    (())
;;
;;   o  If The C-u modifier variable is set (by default it is)
;;      it has a special meaning. Suppose you press C-u " , you get:
;;
;;	  "beg-word word word word word". word word word word
;;	                               ^^
;;      The pairing is done until character which is given in C-u
;;      modifier variable is met. See the documentation of the
;;      variable for more.
;;
;; Cursor positioning
;; ========================================
;; - By default the cursor is positioned in the "middle" of the
;;   inserted pair chars. But for words, this is impossible, because
;;   there is no middle position.
;; - Please see the variables
;;
;;	tipi-:word-positioning
;;	tipi-:word-positioning-func
;;
;;   which allow you to customize cursor positioning after word pairing.
;;
;; Word about syntax tables
;; ========================================
;; - Syntax table play a major part in pairing, especially pairing words
;;   correctly. Suppose you're writing in text mode:
;;
;;        ...txt txt... (help is the key)
;;                       *			<-- cursor
;;
;;   If you now press " to have the word HELP paired, you don't get it,
;;   because normally text mode's syntax table says that "(" belongs
;;   to group "w" (word) too. So the actual word is seen as "(help" and
;;   the program determines that you're inside a word, thus not
;;   allowing the pairing.
;;
;;   In the other hand, if you were in any other mode, say in C++, the
;;   "(" is defined as open parenthesis syntax and it that case the
;;   seen word seen would have been "help" and the " character would have
;;   been added around the HELP string. Like this:
;;
;;	...txt txt... ("help" is the key)
;;			*			<-- cursor
;;
;;   You may propably want to quicly see the syntax definition of
;;   characters, so use the included function from my f'coming
;;   tinylibXX.el lisp libraries
;;
;;	(defalias 'syntax-info 'ti::s-syntax-info)
;;
;;   To return to this syntax problem in text mode, you could do the
;;   following, to make certain characters out of "w" class. The code can
;;   be extracted with function tinylib.el/ti::m-pkg-rip-magic
;;
;;* _
;;* (defun my-syntax-default (table )
;;*   "My syntax table settings."
;;*   (modify-syntax-entry ?[ "_" table)
;;*   (modify-syntax-entry ?] "_" table)
;;*   (modify-syntax-entry ?{ "_" table)
;;*   (modify-syntax-entry ?} "_" table)
;;*   (modify-syntax-entry ?( "_" table)
;;*   (modify-syntax-entry ?) "_" table)
;;*   (modify-syntax-entry ?/ "." table)
;;*   (modify-syntax-entry ?\' "\"" table)
;;*   (modify-syntax-entry ?\" "\"" table)
;;*   (modify-syntax-entry ?_ "w" table)
;;*   )
;;
;;
;;    Then you just change the definitions of syntax table in hook:
;;
;;* (setq text-mode-hook 'my-text-mode-hook)
;;* _
;;* _
;;* (defun my-text-mode-hook ()
;;*   (my-syntax-default  text-mode-syntax-table)
;;*   )
;;
;;
;;    Do you wonder why I put {}()[] into "_" class and not in
;;    corresponding "(" or ")" classes? Well, my stig-paren just went
;;    beserk and started beeping the bell like hell whenever I was nearby
;;    ")" class... The "_" shut it down, so I just chose it. You can
;;    of course put the chars into any class you like.
;;
;; How do I control the pairing in different modes, buffers ?
;; =================================================
;; - Since the tipi-:alist is global to all buffers, you have several
;;   several choices:
;;
;;   Brute force way
;;   o  To totally disable pairing for ceratain
;;      major mode, go and add the mode name to variable
;;      tipi-:disable-mode-list
;;
;;
;;  For more finer control
;;   o  make variable local to buffer, and then change the
;;      pair list with your definitions.
;;      (make-local-variable 'tipi-:alist)
;;      (setq 'tipi-:alist my-perl-tipi-:alist)
;;
;;   o  You can turn on/off the pairings of certain chars easily.
;;	(make-local-variable 'tipi-:alist)
;;      (tipi-pair-func-set ?\' 'tipi-f-off)
;;      (tipi-pair-func-set ?\" 'tipi-f-on)
;;
;;   o  If mode has it's own map (usually has) then you can redefine
;;      the paired char to mode's map so that the global definition
;;      (pairing) is hidden. You can do this:
;;
;;      	(define-key c++-mode-map "{" 'electric-c++-semi)
;;
;;      or for text mode
;;
;;		(define-key text-mode-map "{" 'self-insert-command)
;;
;;      And the pairing isn't done, because the closest keymap is
;;      scanned first, in this case modes' maps are seen first before
;;      the global-map.
;;
;;
;; Development notes
;; ========================================
;; - Let's see scenario first. Suppose I wrote some text and the I decide
;;   to put parens around it:
;;
;;	txt-beg txt txt txt txt txt txt txt txt-end
;;	*
;;
;;   Later I park my cursor to text-beg and my intention is to quote
;;   whole sentence until text-end. But instead I get this:
;;
;;	"txt-beg" txt txt txt txt txt txt txt txt-end
;;	  *
;;
;;   Now I have to go and delete that extra ending quote character....
;;   Unfortunately there is no way to prevent this, because the program
;;   can't guess what the writer intends to do. I considered adding
;;   variable that would have value like:
;;
;;	'ask
;;
;;   which would have asked from user how to put the pair. But I decided
;;   not to implement it, because:
;;   o  it's very easy to delete the extra quote when you have bound
;;      [C-right] to forward-word
;;   o  Asking from user is not this program's "way of doing things",
;;      it would just slow down typing if all kinds of questions
;;      were asked in the middle of writing text.
;;
;;   So, this or similar "more intelligent" features will not be programmed
;;   to this package.
;;
;; BUGS, special notes
;; ========================================
;; - Suppose you have following situation, where  " and ( belong to
;;   non-word syntax classes:
;;
;;	"txt-beg" txt txt txt. txt txt
;;      *
;;
;;   And you want to pair the first word including the quotes. You try
;;   to hit ( but you only get;
;;
;;      ("txt-beg" txt txt txt. txt txt
;;       *
;;
;;   Then you try M-1 (, but you still get the same results.
;;   This is just obvious, bcause the " char is not word, and thus
;;   program cannot do pairing.
;;
;;   I have thought, that it may be nice to say M-3 ( to get first 3 words,
;;   including that quoted one paired, but I haven't found any good
;;   solution. The program structure does not allow this kind of things,
;;   and it would create quite complex situation, when there is multiple
;;   pair chars involved.
;;
;;   I won't add support to these kind of things, because this package is
;;   is only a "companion". Someone else may want to write full minor mode
;;   to handle all kinds of special handlings.
;;
;;   Just do C-right to get the extra ) at the 3rd word, it's lot quicker.
;;
;; - Giving args are in user's responsibility. If you give M-10 and try
;;   to pair 10 words, when there is only 5, the forward-word function
;;   obviously goes to the end of buffer (maybe newlines at the end)
;;   and the end-char pair is inserted there.


;;}}}
;;{{{ history

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Dec	12	1995	[jari]		19.28	v1.18		Release_3
;; - Tiny bug, when typing double quote character in empty buffer caused
;;   error.
;;   tipi-c-\"			:! corrected
;;
;; Dec	11	1995	[jari]		19.28	v1.17		Release_3
;; - changed defconst --> defvar. Some documentation of variables fine tuned.
;;
;; Dec	8	1995	[jari]		19.28	v1.16		NotReleased
;; - There was bug in the main program that efectively disabled user
;;   variable tipi-:pairing-end-allow.
;;   tipi-self-insert-command	:!	corrected
;;
;; Nov	29	1995	[jari]		19.28	v1.15		NotReleased
;; - petersen@kurims.kyoto-u.ac.jp suggested that giving the prefix arg
;;   would pair ARG words. Also the C-u region pairing seemed to have some
;;   troubles.
;; - If cursor is sitting before the word, only pair-beg is now inserted,
;;   previously user got both pair-beg and pair-end. The idea is that user
;;   doesn't normally want double chars when quoting in front of word.
;; - Personally I'm starting to think this pairing stuff is getting too
;;   complicated...
;;   tipi-:word-pairing		-> renamed to tipi-:automatic-word-pairing
;;   tipi-:prefix-arg-nbr	:! option 'word-pair now added
;;   tipi-self-insert-command	:! Added more options
;;
;; Nov	28	1995	[jari]		19.28	v1.14		NotReleased
;; - Corrected byte compiler errors, there was functions called like
;;   this: ((funcall 1 2)), extra parens around the statement.
;;
;; Nov	28	1995	[jari]		19.28	v1.13		NotReleased
;; - Added The C-u region case.
;;   tipi-self-insert-command	:! added some code
;;
;; Nov	28	1995	[jari]		19.28	v1.12		NotReleased
;; - petersen@kurims.kyoto-u.ac.jp,  Jens, proposed some suggestions
;;   again. Thanks! Now the C-u modifier pairs selected region and the
;;   word can now be logical group of syntax classes.
;;   tipi-:word-pairing		:+ can now be turned on/off
;;   tipi-:word-syntax-classes	:+ logical word
;;   tipi-:prefix-arg-cu	:! parameter 'region
;;   tipi-word-class-p		:+
;;   tipi-word-class-skip	:+
;;   tipi-move-logic-word	:+
;;   tipi-self-insert-command	:! C-u arg, word handling
;;   tipi-word-pair		:! logical words
;;
;;
;; Nov	16	1995	[jari]		19.28	v1.11		Release 2
;; - Seems working pretty well right now. No major changes foreseen, so
;;   let's make this alplha. Converted all defconst vars to defvar, when
;;   needed
;;
;; Nov	15	1995	[jari]		19.28	v1.10		NotReleased
;; - Version variables' documentation strings corrected.
;;   tipi-c-\"			:+ Smarter quote handling
;;   tipi-c-\<			:! HTML tags taken cared of better
;;
;; Nov	12	1995	[jari]		19.28	v1.8		NotReleased
;; - Small cursor positioning error in "word" function: when insering
;;   inside pair, the cursor was not left in "middle"
;;
;;	""	-->		"()"
;;	 *	--> (	           *		;; wrong position after insert
;;
;;   tipi-word-pair		:!	corrected
;;
;; Nov	12	1995	[jari]		19.28	v1.8		NotReleased
;; - Forget to handle the positioning of cursor. Added text section
;;   'Cursor positioning'.
;;   tipi-:word-positioning-func	:+
;;   tipi-:word-positioning		:+
;;   tipi-move				:+
;;   tipi-word-pos-func			:+
;;   tipi-word-pair			:! word pos func handling
;;   tipi-self-insert-command		:! positioning handling
;;
;; Nov	12	1995	[jari]		19.28	v1.7		Release_1
;; - Added control function for <, because html end tag delimiter /,
;;   should not pair. Big: pairing didn't take effect on whitespace.
;;   tipi-c-\<			:+ new
;;   tipi-c-\'			:! renamed
;;   tipi-self-insert-command	:! bugs
;;
;; Nov	10	1995	[jari]		19.28	v1.6		NotReleased
;; - Still bugs. Now if I tried:
;;	 []
;;	  *	,inserting { didn't pair, while it certainly should
;;
;;
;; Nov	10	1995	[jari]		19.28	v1.5		NotReleased
;; - Minor bug corrected, when iserting "  in case like this
;;	txt" txt txt	-->	"txt"" txt txt
;;	*			     *
;;
;;   it mistakenly didn't look at the possible end-pair. Now the end-pair
;;   is not inserted if it exists already.
;; - Added text section 'How it works?', 'Pair control:REMEMBER'
;;   'BUGS, special notes'
;;   tipi-multi-pair		:!  corrected
;;
;; Nov	10	1995	[jari]		19.28	v1.4		NotReleased
;; - Radical changes, 40% of the engine code rewritten.
;;   Now the meaning of C-u arg and nbr args are controlled by variables.
;;   tipi-:prefix-arg-nbr	:+
;;   tipi-:prefix-arg-cu	:+
;;   inc, inci			:+ from my libs
;;   ti::s-repeat		:+ from my libs
;;   ti::b-read-char		:+ from my libs
;;   tipi-multi-pair		:+
;;   tipi-word-pair		:+
;;   tipi-self-insert-command	:! rewritten
;;   ti::b-read-syntax-word	:- deleted, not needed
;;
;; Nov	9	1995	[jari]		19.28	v1.3		NotReleased
;; - More explanation added to 'How do I control the pairing...' section
;;   tipi-:disable-mode-list	:+ new variable
;;
;; Nov	9	1995	[jari]		19.28	v1.2		NotReleased
;; - Improvements. The thingatpt library wasn't suitable for
;;   reading words, because it gave false results.
;; - Now it is possible to handle each pair chararcter individually with
;;   associated function.
;; - Now it's possible to have individual settings for different modes,
;;   added text section 'How do I control the pairing in different modes'
;;   ti::b-read-syntax-word	:+ from my libs
;;   tipi-e-beg			:+ "macro"
;;   tipi-e-end			:+ "macro"
;;   tipi-e-funcs		:+ "macro"
;;   tipi-f-on			:+ default "on" function
;;   tipi-f-off			:+ default "off" function
;;   tipi-pair-func-set		:+ changinf the func cell easily
;;   tipi-c-tick		:+ special tick ' check function
;;   tipi-self-insert-command	:! many changes
;;   tipi-:alist		:! docs rewritten.

;;
;; Nov	8	1995	[jari]		19.28	v1.1		Created


;; To do list:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.


;;}}}

;;; Code:

;;{{{ setup: variables

;;; ......................................................... &require ...

;; (require 'tinylib)
(require 'backquote)


;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar tipi-:load-hook nil
  "Hook that is run when package is loaded.")


;;; ... private variables ................................. &v-private ...


(defvar tipi-:us-alist
  '(
    (?(  ?))
    (?[  ?])
    (?{  ?})
    (?<  ?>    tipi-c-\<)
    (?\`  ?\'  tipi-c-\')
    (?\"  ?\"  tipi-c-\")
    )
  "*Default US pairing alist.")


(defvar tipi-:european-alist
  '(
    (?(  ?))
    (?[  ?])
    (?{  ?})
    (?<  ?>    tipi-c-\<)
    (?\'  ?\'  tipi-c-\')
    (?\`  ?\`)				;in perl, or shell you need backticks
    (?\"  ?\"  tipi-c-\")
    )
  "*Default European pairing alist.")

;;; please use function tipi-pair-info instead of touching/reading this.
;;;
(defvar tipi-:alist tipi-:european-alist
  "*The pairing alist '((?BEG-CHAR  ?END-CHAR FUNC-SYM) ..)
The FUNC-SYM element is optional. FUNC definition should have form,

accepted args:
  BEG-CHAR
  END-CHAR

return values:
   t	,force immediate pairing
   nil	,pairing prohibited, main should insert char \"as is\"
   nbr  ,return control to main program.
   sym  ,func handled pairing, main program should terminate.

If the func element is missing, pairing is done always according to main
function's decision.
")

;;; ... user configurable .................................. &v-config ...

;;  You can use your own function if this doesn't satisfy you
;;
(defvar tipi-:pair-function 'tipi-self-insert-command
  "*The function that takes care of pairing.")

;;  - Since not all people program with perl-mode when coding perl
;;    (I don't use it), the default function here is not always
;;    the best choice.
;;  - For detecting buffer contents in more robust way that just
;;    relying on the major-mode variable, keep on eye on my
;;    f'coming package
;;
;;        tinybufid.el -- Identifying buffer regardless of mode
;;
;;    which will be released soon.
;;
(defvar tipi-:pair-allow-check-function 'tipi-pair-allowed
  "*Funtion that takes no args, and return t or non-nil if
pairing is allowed for current buffer

If the value is nil, pairing is allowed for all buffers.
")



;;  Eg. (setq tipi-:pairing-end-allow  '( ?\) ?\} )  )
;;
(defvar tipi-:pairing-end-allow nil
  "*List of end-pair characters that enable pairing, while cursor is
is sitting on it. Eg, normally if user's cursor is over the pair-end,
inserrting a pair character causes no doubling:

    ()
     *         <-- cursor position

- If you now press \"(\" the pairing isn't done if the list is nil
- if list contains \")\", th double pairing is allowed while cursor
  is sitting on \")\"
- If you press any other pairing character eg. \"{\", it will be doubled
  normally, because the char under cursor wasn't pair-end --> \"}\"

")



(defvar tipi-:disable-mode-list
  '(
    lisp-interaction-mode
    lisp-mode
    emacs-lisp-mode
    )
  "*List of major-mode symbols, where the pairing is fully
prohibited. If you want to allow some pairing, see documentation
how to control pairing through individual funtions in tipi-:alist")



;; Sorry, but you can't have M-4 ( to act like (((
;; AND M-4 " act like pairing first 4 words...
;; --> just go and type 4 ((( if you need 4 parens and in other cases
;;     use the M-4 "  for pairing many words.
;;
(defvar tipi-:prefix-arg-nbr 'word-pair
  "*How the prefix arg is handled for paired chars. Possible values are:
  nil	     ,act as self-insert-command, M-4 ( gives 4 characters ((((
  'word-pair ,word pairing. If prefix is 1, pair current word, if prefix
              is 2, pair next two wrds and so on.
")

;;;  eg. value can be "^.\n\)"  if you want to pair sentences
;;;
(defvar tipi-:prefix-arg-cu  'region
  "*How the prefix arg is handled for paired chars. possible values:
  nil	    ,C-u is not handled
  str	    ,character set that denotes the last character that is used
             for pairing word. The set is used as [charset] regexp for
             skip-chars-forward, please check right syntax from the
             emacs info pages.

	     Eg. if you have set this variable to \"^.\\n\", then
	     all the characters up to period or newline are paired.

	     txt txt txt txt txt txt txt txt. Txt txt txt txt txt txt
	     *    ,pressing C-u \"   gives you
	     \"txt txt txt txt txt txt txt txt\". Txt txt txt txt txt txt

	     NOTE: the C-u is only valid when point is sitting on word
	           beginning. Multiple C-u's are not recognized.

   'region   The marked region is paired.
")


(defvar tipi-:automatic-word-pairing t
"*If non-nil, then the word pairing is allowed. Eg when your cursor is
at the beginning of word, pressing pair-beg char will pair the whole word.

   txt		-->		   (txt)
   *		--> pressing (	    *		;; cursor
")


(defvar tipi-:word-positioning-func 'tipi-word-pos-func
"*The value can also be a function symbol, which takes care of positioning
the cursor. Passed parameters are:

  BEG-POINT	,point+1 where the beg-char were inserted
  BEG-CHAR	,character

If function returns, non-nil it is assumed that function handled the
positioning. If it returns nil, then the control is returned to calling
program and the positioning is done according to variable
tipi-:word-positioning")


(defvar tipi-:word-positioning 'end
  "*How the cursor should be positioned after word pairing.
  'beg          ,leave point after beg pair char
  'end		,leave point after end pair char

")




(defvar tipi-:word-syntax-classes  '(?w ?$ ?. )
"*List of syntax classes that are treated like WORD, when the pairing is in effect. Eg if you have following text in LaTeX mode:

    $x^2+$
	 *	<-- cursor here, now you want to pair it with (

You would normally get

    $x^2+()$
	  *

Because the character $ is in class $. (You can check the class with
function tipi-syntax-info). But when the is defined into this variable's
list, it is seen as \"word\", and the pairing is done like for word,
so that you get this:

     $x^2+($)
	   *
")




;;}}}
;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tipi-version
  "$Revision: 1.18 $"
  "Latest version number.")


(defconst tipi-version-id
  "$Id: tinypair.el,v 1.18 1995/12/12 07:02:04 jaalto Release_3 jaalto $"
  "Latest modification time and version number.")

(defconst tipi-version-doc
  "tinypair.el -- self insert character pairs () \"\" '' <> etc.

First created: Nov 8 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tipi-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tipi-version-doc
       "\n\ncurrent version:\n" tipi-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}
;;{{{ lib

;;; ############################################################# &lib ###
;;; ** Do not copy these, they will be in tinylibXX.el distribution
;;;    released later.
;;; ** This section will be removed from this file when the libs are out.


(defmacro inc (var)
  "Increments by 1. See GNU Lisp manual sec. 12 MACROS"
  (` (setq (, var) (1+ (, var)))))

(defmacro inci (var i)
  "Increments by i."
  (` (setq (, var) (+ (, var) (, i)))))



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

(defmacro PMIN ()
  "goes to point-min"
  (` (goto-char (point-min))))

(defmacro PMAX ()
  "goes to point-max"
  (` (goto-char (point-max))))


(defmacro NEXTP (list)
  "Advances list pointer with cdr."
  (` (setq (, list) (cdr (, list)))))



(defconst ti:-syntax-info
 '(
   (?\ "Whitespace")
   (?- "Whitespace")

   (?w "Word")
   (?_ "Symbol, variables and commands")
   (?. "Punctuation, separate symbols from one another")
   (?( "Open parenthesis")
   (?) "Close parenthesis")
   (?\"  "String quote, string as a single token")
   (?\\ "Escape")
   (?/ "Character quote, only the character immediately following.")
   (?$ "Paired delimiter, like string quote, chars between are not suppressed")
   (?< "Comment starter")
   (?> "Comment ender")
   (?@ "Inherit from standard syntax table")
   )
 "Short syntax definition table ((CLASS . DESC) ..)")



;;; ----------------------------------------------------------------------
;;;
(defun ti::s-syntax-info (char &optional verb)
  "Return brief syntax definition string for CHAR"
  (interactive "cShow syntax of char: ")
  (let* ((syntax (char-syntax char ))
	 (elt    (assq syntax ti:-syntax-info))
	 (verb   (or verb (interactive-p)))
	 ret
	 )
    (setq ret
	  (concat
	   (char-to-string syntax)
	   " "
	   (if elt  (nth 1 elt) "")))
    (if verb
	(message ret))
    ret
    ))



;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'string-repeat 'ti::s-repeat)
;;;
(defun ti::s-repeat (count char-or-string)
  "Returns repeater char or string."
  (let* ((i 0)
	 ret
	 )
    (if (integerp char-or-string)
	(setq ret (make-string count char-or-string))
      (setq ret "")
      (while (< i count)
	(setq ret (concat ret char-or-string))
	(inc i)))
    ret
    ))


;;; ----------------------------------------------------------------------
;;;
(defun ti::b-read-char (&optional direction distance)
  "Reads character towards the direction from current point:
nil = forward, non-nil backward. DISTANCE 0/nil means reading from
current position.

Returns:
  nbr   ,read char value
  nil   ,if the position is not within point-min-marker and
         point-max-marker.
"
  (let* (
         (beg  (point-min-marker))
         (end  (point-max-marker))
         (pos  (or distance 0))
         (dest (if direction (- (point) (1+ pos)) (+ (point) pos)))
         (read (if (or (< dest beg) (> dest end)) nil t))
         )
    (if (null read) nil                 ;allowed to read ?
      (char-after dest))
    ))







;;}}}


;;; ########################################################### &Funcs ###

;;{{{ misc

(defalias 'tipi-syntax-info 'ti::s-syntax-info)


;;; ----------------------------------------------------------------------
;;;
(defun tipi-word-class-p (class)
  "Checks if class of part of logical word classes."
  (memq class tipi-:word-syntax-classes))


;;; ----------------------------------------------------------------------
;;;
(defun tipi-word-class-skip (&optional back)
  "Skips forward all tipi-:word-syntax-classes chars."
  (let* ((ptr		tipi-:word-syntax-classes)
	 (func		(if back 'skip-syntax-backward 'skip-syntax-forward))
	 (point		(point))
	 )
    (while ptr
      (funcall func (char-to-string (car ptr)))
      (if (eq (point) point)
	  (NEXTP ptr)
	;; moved, start over.
	(setq point (point))
	(setq ptr tipi-:word-syntax-classes)
	))))


;;; ----------------------------------------------------------------------
;;; "e"  refers to elt
;;;
(defun tipi-e-beg (elt)
  "Return BEGIN element."
  (nth 0  elt))

(defun tipi-e-end (elt)
  "Return BEGIN element."
  (nth 1 elt))

(defun tipi-e-func (elt)
  "Return FUNC element."
  (if (= (length elt) 3)
      (nth 2 elt)
      nil
      ))

;;; ----------------------------------------------------------------------
;;; "f" refers to (f)unc cell function in pair alist
;;;
(defun tipi-f-on (&rest args)
  "Return integer, allow pairing."
  1)


(defun tipi-f-off (&rest args)
  "Return nil, prohibit pairing."
  nil)


;;; ----------------------------------------------------------------------
;;; "c"  refers to "checking func"
;;; - Yes I know, the function name seems a little ugly with \'
;;;   in the name, but that was the most logical name for
;;;   different characters. Please remember, that english char names are
;;;   not that "descriptive" for non-english people.
;;;
(defun tipi-c-\' (ch1 ch2)
  "Checks if tick '  character can be paired."
  (if (memq major-mode
	    '(mail-mode
	      news-mode
	      news-reply-mode
	      perl-mode
	      cperl-mode))
      nil
    1
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tipi-c-\< (ch1 ch2)
  "Checks if <  character can be paired. In HTML mode when there
is tag end,\"slash\", it's not desirable to have <>. Several other HTML
cases are checked too.
"
  (let* ((ret 1)
	 prev
	 )
    (cond
     ((memq (following-char) '(?/ ))
      (setq ret nil))
     ((or (looking-at "a[ \t]+href")
	  (looking-at "hr[ \t]\\(size\\|wid\\)")	;1.1N <hr size=..>
	  (looking-at "\\(th\\|tr\\)[ \t]align")	;1.1N tables
	  (looking-at "p[ \t]+align")	                ;1.1N <p align=..>
	  (looking-at "\\(link\\|img\\|form\\) ")
	  )
      ;;  The word pairing isn't good in sgml/html mode.
      ;;
      ;;  If we have
      ;;     <A HREF="http://www.interplay.com">Interplay</a>
      ;;     <LINK REV="company"  HREF="http://www.interplay.com">
      ;;
      (setq ret nil)
      ))
    ret
    ))


;;; ----------------------------------------------------------------------
;;;
;;;  It's like you have opened ne quote
;;;   "txt txt txt
;;;       *               ,point here, and you want to end the quote..
;;;
;;;  In this case the pairing isn't desiredable
;;;
(defun tipi-c-\" (ch1 ch2)
  "Checks if \"  character can be paired. Looks backward if previous word
has starting pair.
"
  (let* ((ret 1)			;default is main handling
	 prev				;char
	 point
	 )
    ;;  The prev is nil if point is in BOB
    ;;
    (if (not (and (setq  prev (char-syntax (or (ti::b-read-char nil -1) ?\ )))
		  (tipi-word-class-p prev)
		  ))
	(save-excursion
	  (setq point (point))
	  (skip-syntax-backward "w")

	  ;;  point must move, because the skip-syntax will skip
	  ;;        "txt"
	  ;;         2  1          ,1= before  2, after
	  ;;  and reading that first " require backward char
	  ;;
	  (if (and (not (= point (point)));require movement
		   (not (bobp))
		   (prog1 t (forward-char -1))	;now we can move
		   (eq (following-char) ch1))
	      (setq ret nil)		;disallow pairing
	    nil
	    )))
  ret
  ))


;;; ----------------------------------------------------------------------
;;;
(defun tipi-pair-func-set (beg-char &optional func-sym)
  "Sets the function cell of active pairing list's BEG-CHAR to FUNC-SYM.
Default functions to disable or enable pairing are:

  tipi-f-off
  tipi-f-on

If the character is not defined in pair list, this function act as no-op.
FUNC-SYM is by default 'tipi-f-on
"
  (let* ((elt   (assq beg-char tipi-:alist))
	 (func  (or func-sym 'tipi-f-on))
	 )
    (if (null elt)
	nil
      (setcdr (cdr elt) (list func))
      )))

;;; ----------------------------------------------------------------------
;;;
(defun tipi-pair-allowed ()
  "Function to determine if pairing is allowed.
Returns t, when pairing is allowed for buffer.
"
  (not (memq major-mode tipi-:disable-mode-list))
  )



;;; ----------------------------------------------------------------------
;;;
(defun tipi-pair-info (mode &optional arg)
  "Mode can be 'set 'get. Sets or returns the efective pair list."
  (cond
   ((eq mode 'get)
    tipi-:alist)
   ((eq mode 'set)
    (setq tipi-:alist arg))
   (t
    (error "What you had in mind?")
    )))


;;; ----------------------------------------------------------------------
;;;
(defun tipi-bind (key-func func alist)
  "Bind FUNC to char in car of alist."
  (let* (ch
	 )
  (while alist
    (setq ch (tipi-e-beg (car alist)))
    (funcall key-func (char-to-string ch) func)
    (NEXTP alist)
    )))

;;; ----------------------------------------------------------------------
;;; - I used this before, may use it again...
;;;
(defun tipi-pair2re (mode alist)
  "Returns set Regexp out of pairs. Mode can be 'beg 'end referring
to left and right items in alist."
  (let* ((func (if (eq mode 'beg)
		   'tipi-e-beg 'tipi-e-end))
	 ch
	 set
	 ret
	 )
    (while alist
      (setq ch (funcall func (car alist)))
      (setq set (concat (or set "")
			(regexp-quote (char-to-string ch))))
      (NEXTP alist)
      )
    (if set
	(setq ret (concat "[" set "]")))
    ret
    ))


;;; ----------------------------------------------------------------------
;;; - I used this before, may use it again...
;;;
(defun tipi-move-logic-word (&optional count)
  "Moves forward, skipping tipi-:word-syntax-classes."
  (let* ((i		0)
	 (count		(or count 1))
	 (back	        (if (< count 0) 'back nil))
	 (func		(if back 'skip-chars-backward 'skip-chars-forward))
	 )
    (while (< i count)
      (funcall func " \t\n")		;ignore whitespace
      (tipi-word-class-skip back)
      (inc i)
      )
    ))






;;; ----------------------------------------------------------------------
;;; - I used this before, may use it again...
;;;
(defun tipi-multi-pair (count str-beg str-end)
  "Insert multiple pairs."
  (let* ((i 0)
	 (str-beg (if (integerp str-beg)
		      (char-to-string str-beg)
		    str-beg))
	 (str-end (if (integerp str-end)
		      (char-to-string str-end)
		    str-end))
	 (s1 "")
	 (s2 "")
	 )
    (if (not (integerp count))
	nil
      (while (< i count)
	(setq s1 (concat s1 str-beg))
	(setq s2 (concat s2 str-end))
	(inc i)
	)
      (insert s1 s2))
    ))


;;; ----------------------------------------------------------------------
;;; - I used this before, may use it again...
;;;
(defun tipi-word-pair (arg ch-beg ch-end)
  "Insert pair around word(s)."
  (let* ((cu-str    tipi-:prefix-arg-cu)
	 (arg-flag  tipi-:prefix-arg-nbr)
	 (pos-flag  tipi-:word-positioning)
	 (pos-func  tipi-:word-positioning-func)
	 direction
	 count
	 syntax-prev
	 syntax-now
	 tmp
	 beg end			;points
	 )
    (setq direction
	  (cond
	   ((integerp arg)
	    (if (> arg -1) nil 'back))
	   (t				;C-u forward
	    nil)))

    (setq syntax-prev (char-syntax
		       (or
			(ti::b-read-char direction -1)
			?\
			)))
    (setq syntax-now  (char-syntax
		       (or
			(ti::b-read-char direction 0)
			?\
			)))

;;;    (d! arg arg-flag (char-to-string syntax-now) (char-to-string syntax-prev))



    (cond
     ((and (or (null arg)
	       (integerp arg))
	   (tipi-word-class-p syntax-now)
	   (null (tipi-word-class-p syntax-prev)))

      (setq count (if (null arg) 1 arg))
      (if (< count 0)			;switch the values
	  (setq tmp ch-beg   ch-beg ch-end   ch-end tmp))

;;;      (d! "word" count)
      (insert ch-beg)
      (setq beg (point))

      (tipi-move-logic-word count)

    ;;; (forward-word count)

      ;;  There may already be pair-end ...
      (if (eq (ti::b-read-char direction 0) ch-end)
	  nil
	(insert ch-end)
	(setq end (point)))
      )

     ((and (integerp arg) arg-flag)
      (insert (tipi-multi-pair arg ch-beg ch-end))
      (tipi-move (* 2 arg))
      (setq pos-flag nil)
      )

     ((and (integerp arg) (null arg-flag))
      (insert (ti::s-repeat arg ch-beg))
      )

     ((and (not (null arg))
		(listp arg)
	   (stringp cu-str))
      (cond
       ((and
	 (tipi-word-class-p syntax-now)	;word beginnning
	 (not (tipi-word-class-p syntax-prev)))

	(insert ch-beg)
	(setq beg (point))
	(skip-chars-forward cu-str)
	(insert ch-end)
	)
       (t				;in the middle of the word
	(insert ch-beg)
	)))

     (t					;defualt case
;;;      (d! "default")
      (insert ch-beg ch-end)
      (backward-char 1)
      (setq pos-flag nil)
      ))


    ;; ............................................ cursor positioning ...
    (setq tmp nil)			;"status" of call
    (and (fboundp pos-func)
	 (integerp beg)
	 (setq tmp (funcall pos-func beg ch-beg)))

;;;    (d! "stat>>" beg (fboundp pos-func) tmp)

    (cond
     ((not (null tmp))			;function handled this.
      nil)

     ((eq 'beg pos-flag)
      (and (integerp beg)
	   (goto-char beg)))
     (t
      nil))

    ))


;;; ----------------------------------------------------------------------
;;;
(defun tipi-move (count)
  (cond
   ((or (not (integerp count))
	(<= count 1))
    nil)				;do nothing
   (t
    (backward-char (/ count 2))
    )))


;;; ----------------------------------------------------------------------
;;;
(defun tipi-word-pos-func (beg char)
  "Special cursor positioning func for word pairing."
  (cond
   ((eq char ?\( )
    ;;  Mostly in minibuffer and for lisp'ish things I want cursor
    ;;  after starting paren.
    (goto-char beg))
   (t
    nil)))


;;}}}

;;{{{ main

;;; ............................................................ &main ...




;;; ----------------------------------------------------------------------
;;;
(defun tipi-control (&optional arg key-func)
  "Pairing control center.
Input:
 arg	    nil     ,remove pairing, set chars to self-insert-command
	    1       ,use US pair alist
	    2       ,use European pair alist
 key-func	    ,default is global-set-key
"
  (interactive "P")
  (let* ((func  (if (null arg)
		    'self-insert-command
		  tipi-:pair-function))
	 (key-func (or key-func
		       'global-set-key))
	 )
    (cond
     ((memq arg '(nil off))
      (tipi-bind key-func func (tipi-pair-info 'get))
      )

     ((memq arg '(1 us))
      (tipi-pair-info 'set tipi-:us-alist)
      (tipi-bind key-func func tipi-:us-alist)
      )

     ((memq arg '(2 european))
      (tipi-pair-info 'set tipi-:european-alist)
      (tipi-bind key-func func tipi-:european-alist)
      ))

    ))




;;; ----------------------------------------------------------------------
;;; - Original idea in 19.29+ package paired-insert.el. Unfortunately the
;;;   package didn't satisfy my needs, so here is better pairing func.
;;;
;;; - the 'pair' variable in this function is purposively set
;;;   many times, although it is not always necessary. It is just eases
;;;   following the program flow.
;;;
(defun tipi-self-insert-command (arg)
  "Smart pairing."
  (interactive "*P")
  (let*  ((nbr		(prefix-numeric-value arg))
	  (table	(tipi-pair-info 'get))
	  (pair-end     tipi-:pairing-end-allow)
	  (word-pair    tipi-:automatic-word-pairing)
	  (multi-pair   tipi-:prefix-arg-nbr)
	  (cu-arg       tipi-:prefix-arg-cu)
	  (arg-flag     tipi-:prefix-arg-nbr)
	  (ch		last-command-char)
	  (elt          (assq ch table))


	  (pair-allow   (if (fboundp  tipi-:pair-allow-check-function)
			    (funcall  tipi-:pair-allow-check-function)
			  t))

	  (pair         nil)		;pair control
	  (status	1)		;see user configuration CHAR-FUNC

	  direction			;character looking at cmd
	  word
	  ch-func			;character function
	  ch-beg      ch-end
	  syntax-prev syntax-now  syntax-next
	  ch-prev     ch-now	  ch-next
	  beg end			;point beg end
	  )


    (if (or (null elt)			;Not defined for pairing
	    (null pair-allow))
	(self-insert-command nbr)

      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. do pairing . .
      (setq ch-beg  (tipi-e-beg elt))
      (setq ch-end  (tipi-e-end elt))
      (setq ch-func (tipi-e-func elt))


      ;;  The buffer end is a special case, we just use some dummy
      ;;  value by default
      (setq syntax-prev (char-syntax
			 (setq ch-prev
			       (or
				(ti::b-read-char direction -1)
				?\
				))))
      (setq syntax-now  (char-syntax
			 (setq ch-now
			       (or
				(ti::b-read-char direction 0)
				?\
				))))

      (if (fboundp ch-func)
	  (setq status (funcall  ch-func ch-beg ch-end)))

;;;      (d! ch-now ch-end)


      (cond
       ((integerp status)
	(setq direction
	      (cond
	       ((integerp arg)
		(if (> arg -1) nil 'back))
	       (t				;C-u forward
		nil)))


;;;  (d!  "dir" direction (char-to-string ch-now) (char-to-string ch-prev))




	(cond
	 ((and (equal 'region cu-arg)
	       (equal '(4) arg))


	  ;; Hmmm, let's use dirty hack to test if region is marked
	  (condition-case nil
	      (save-excursion
		(region-end)		;breaks, if not set
		;;  User may draw tthe region up-down, or down-up
		;;  Let's include right pairs in right spots
		;;
		;;  The order of insertion is important here, if we had
		;;  inserted to the beginning, then the end point would
		;;  have moved after ch-beg insert...
		;;
		(setq end (max (region-beginning)
			       (region-end)))
		(setq beg (min (region-beginning)
			       (region-end)))
		(goto-char end) (insert ch-end)
		(goto-char beg) (insert ch-beg)
		)
	    (error
	     (setq status 'nothing pair nil)
	     ;;  I think message is okay here, error is so beepy...
	     (message "Mark inactive. Region not defined.")
	     ))
	  )

	 ((equal  ch-now ch-end)		;already pair visible


	  ;;  does user want to allow this pairing ?
	  ;;  is the end character in "allow" list"
	  (setq pair t)
	  (if (and pair-end
		   (memq (cdr elt) pair-end))
	      (setq multi-pair t))
;;;	  (d! "found end, multi" pair multi-pair )
	  )

	 ((equal syntax-now ?\ )	;whitespace at point
	  (setq multi-pair 1 arg 1 pair t))	;ok, do pairing

	 (word-pair
	  ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  words  ..
	  ;; the default case
	  ;;  handle smart pairing.
	  (tipi-word-pair arg ch-beg ch-end)
;;;	  (d! "word ok")
	  (setq pair nil)
	  )

	 ((and (integerp arg)
	       (equal arg-flag 'word-pair))
	  ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  words  ..
	  (tipi-word-pair arg ch-beg ch-end)
;;;	  (d! "word ok")
	  (setq pair nil)
	  )

	 (t
;;;	  (d! "def")
	  (setq multi-pair 1 arg 1 pair t)
;;;	  (setq pair t)
	  ))				;main COND

        ;; ... ... ... ... ... ... ... ... ... ... ...  insert chars ? ...
;;;	(d! "pair"  arg pair  multi-pair)

	(if (null pair)
	    nil
	  (cond
	   ((and (null multi-pair) (null arg))
	    (insert ch-beg))

	   ((not (null multi-pair))
;;;	    (d! "DO" nbr ch-beg ch-end)
	    (tipi-multi-pair nbr ch-beg ch-end)
	    (tipi-move (* 2 nbr))
	    )

	   (t
	    (insert (ti::s-repeat nbr ch-beg))
	    )))
	)

       ;; ... ... ... ... ... ... ... ... ... ... ... ... other status ..
       ((eq nil status)
	(insert ch-beg))

       ((eq t status)
	(insert ch-beg ch-end)
	(backward-char 1))

       ((symbolp status)
	nil
	))				;this cond
      )

    ))

;;}}}


(provide 'tinypair)
(run-hooks 'tipi-:load-hook)

;;; ............................................................. &eof ...
