;;; madlib.el -- Mad Lib game with examples

;; Copyright 1991-1994 Michael D. Ernst

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Created: 5/17/91
;; Last modified: 2/7/94
;; Version: 1.0.1
;; Keywords: games

;; LCD Archive Entry:
;; madlib|Michael Ernst|mernst@theory.lcs.mit.edu|
;; Mad Lib game|
;; 7-Feb-1994|1.0.1|~/games/madlib.el|

;; This file is distributed under the same conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Remember the Mad Lib game which you played when you were a child?
;; Some of us haven't grown up yet and are still playing them.

;; Mad Libs are stories with some key words excised; for each missing word,
;; the players are asked to specify a replacement, but aren't given any
;; context, only the part of speech (such as noun, adjective, etc.).  The
;; resulting filled-in stories can be quite humorous.

;; Mad Libs were invented by Roger Price and Leonard Stern.  Thanks to Ed
;; Reingold <reingold@cs.uiuc.edu> for supplying five Mad Lib files from
;; one of their original books.

;;; Usage:

;; * Save this file, madlib.el, somewhere on your load path.
;; * Save some madlib skeleton files with extension ".mad" in some directory.
;;   (See the end of this file for samples.)
;; * Put the following lines in your .emacs file:
;;     (autoload 'madlib "madlib" "Mad Lib game" t)
;;     (setq madlib-directories '("~/random/mad/"))
;;   specifying the directory containing your own madlib skeleton files.
;;   A skeleton file is chosen at random from one of those directories.
;; * The next time you run Emacs, you can play a Mad Lib by doing 
;;   M-x madlib RET and responding to the promts.

;; To try it right now,
;; * Extract the sample skeleton files from the end of this file (directions
;;   for extracting them while viewing this file in Emacs appear there).
;; * M-x madlib RET

;;; Skeleton files:

;; A Mad Lib skeleton file is a story with certain words or phrases left
;; out and replaced by descriptions (called bones, because they're part of
;; the skeleton).  The user is prompted by the description, and the
;; response replaces the bone in the final output.

;; Two sample skeleton files appear at the end of this file, uuencoded (so
;; that their content is a surprise to you).  

;; A bone starts with an asterisk, gives a description, optionally contains
;; an equal sign and a key with which to associate the value, and ends with
;; a second asterisk.  Typical bones are "*noun*" and "*animal one would
;; hunt=biggamename*".

;; After a key is specified, future bones may simply give the key as a
;; bone, like so:  "*biggamename*".  The previously-specified value is
;; inserted.  This permits a response to be multiply inserted in the Mad Lib.

;; The bone's capitalization is respected.  That is, if the bone appears
;; entirely in upper case, the user's input is upcased; similarly with
;; capitalized bones, which cause the user input to be capitalized.

;; If a bone is preceded by the words "a" or "an", that preceding word is
;; transformed to make sense with the first character of the replacement
;; text.

;; In order to specify a literal asterisk in the skeleton text, precede it
;; by a backslash.  There is no way to indicate that an asterisk
;; immediately following a backslash is actually part of a bone.

;; Variable `madlib-bone-prompt-alist' permits abbreviations (such as *adj*
;; for *adjective* or *nouns* for *plural noun*) to appear in the skeleton
;; file.  See its documentation.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar madlib-directories nil
  "*List of directories in which to look for Mad Lib skeletons.")

(defvar madlib-dont-repeat-p t
  "*Non-nil if a story skeleton should not be re-used until all others have been.")

(defvar madlib-prev-directories nil
  "Previous value for `madlib-directories'.
This is used when the value of `madlib-directories' has changed.")

(defvar madlib-files nil
  "List of madlib skeleton files.")

(defvar madlib-assoc-list nil
  "Assoc list for \"variables\" used in the Mad Lib skeleton file.
These variables allow a response to be multiply inserted into the skeleton.")

(defun madlib-add-key (key replacement)
  (setq madlib-assoc-list
	(cons (cons key replacement) madlib-assoc-list)))
(defun madlib-lookup-key (key)
  (cdr (assoc key madlib-assoc-list)))

(defvar madlib-bone-prompt-alist
  '(("adj" . "adjective")
    ("nouns" . "plural noun")
    ("verbed" . "verb, \"ed\" form")
    ("verbing" . "verb, \"ing\" form")
    ("adv" . "adverb"))
  "Assoc list of Mad Lib skeleton forms and prompts.
The default is for the prompt to be the bone, sans asterisks;
use this alist only when that is inappropriate.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find madlib files
;;;

(defun madlib-select-file ()
  "Choose a Mad Lib skeleton file."
  (if (stringp madlib-directories)
      (setq madlib-directories (list madlib-directories)))
  (while (not madlib-directories)
    (setq madlib-directories
	  (read-file-name "Directory containing Mad Lib files: " nil nil t)
	  madlib-directories (if (file-directory-p madlib-directories)
				 (list madlib-directories)
			       nil)))
  (if (not (eq madlib-prev-directories madlib-directories))
      (setq madlib-files (apply (function append)
				(mapcar (function madlib-directory-madlib-files)
					madlib-directories))
	    madlib-prev-directories madlib-directories))
  (if (null madlib-files)
      (progn
	(setq ;; madlib-directories nil
	      madlib-prev-directories nil)
	(error "I can't find any Mad Lib skeleton files.")))	       
  (let ((r (random t))
	result)
    (if (< r 0) (setq r (- r)))
    (setq result (nth (mod r (length madlib-files)) madlib-files))
    (if madlib-dont-repeat-p
	(setq madlib-files (delq result madlib-files)))
    result))
	

(defun madlib-directory-madlib-files (directory)
  "Return a list of full pathnames of .mad files in DIRECTORY."
  (mapcar
   (function (lambda (filename)
	       (concat (file-name-as-directory directory) filename)))
   (madlib-filter 
    (directory-files directory)
    (function (lambda (filename)
		(and (> (length filename) 4)
		     (string-equal (upcase (substring filename -4)) 
				   ".MAD")))))))

(defun madlib-filter (list predicate)
  "Return the sublist of LIST answering true to PREDICATE."
  (let (result)
    (while list
      (if (funcall predicate (car list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main procedures
;;;

(defun madlib ()
  "Prompt the user to supply words to complete a Mad Lib, then display it."
  (interactive)

  (set-buffer (get-buffer-create "*madlib*"))
  (replace-buffer-in-windows "*madlib*")
  (setq buffer-read-only nil)

  (erase-buffer)
  (insert-file (madlib-select-file))
  (let ((fill-column (1+ (point-max))))
    (fill-region (point-min) (point-max)))

  (goto-char (point-min))
  (setq madlib-assoc-list nil)

  (madlib-process-bones)

  (goto-char (point-min))
  (replace-string "\\*" "*")
  (fill-region (point-min) (point-max))
  (goto-char (point-min))
  (switch-to-buffer "*madlib*")
  (setq buffer-read-only t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get words from user and do replacement
;;;

(defvar madlib-match-re
  (concat
   "\\(\\`\\|[^\\]\\|\\<an? \\)"	; match 1
   "\\*"
   "\\([^*=]+\\)"			; match 2
   "\\(=\\([^*]+\\)\\)?"		; match 3, 4
   "\\*"
   ;; "\\(s\\|ed\\|ing\\)?"		; match 5
   ))

(defun madlib-match-string (n)
  "Return the string matched by parentheses number N, or nil.
Not general!  For the use of Mad Lib only."
  (and (match-beginning n)
       (buffer-substring (match-beginning n) (match-end n)))) 

;; This is a hack.
(defun madlib-a-or-an (following-character capitalizep)
  (let* ((fc (downcase following-character))
	 (result (if (or (= ?a fc)
			 (= ?e fc)
			 (= ?i fc)
			 (= ?o fc)
			 (= ?u fc))
		     "an "
		   "a ")))
    (if capitalizep (capitalize result) result)))

;; This is called in the *madlib* buffer.
(defun madlib-process-bones ()
  (let (prefix text key replacement)
    (while (re-search-forward madlib-match-re nil t)
      (setq prefix (madlib-match-string 1)
	    text (madlib-match-string 2)
	    key (madlib-match-string 4)
	    replacement (madlib-bone-replacement text))
      ;; This is a hack
      (if (not (= 1 (length prefix)))
	  (setq prefix (madlib-a-or-an (aref replacement 0)
				       ;; depends on ASCII ordering
				       (= (aref prefix 0) ?A))))
      (replace-match (concat prefix replacement))
      (if key
	  (madlib-add-key key replacement)))))

(defun madlib-bone-replacement (bone-text)
  (let* ((case (cond ((string-equal bone-text (upcase bone-text))
		      'upcase)
		     ((string-equal bone-text (capitalize bone-text))
		      'capitalize)))
	 (text (if case (downcase bone-text) bone-text))
n	 (result (or (madlib-lookup-key text)
		     (let ((prompt (or (cdr (assoc text madlib-bone-prompt-alist))
				       text)))
		       (read-input (concat prompt ":  "))))))
    (if case (funcall case result) result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample skeleton files
;;;

;; One simple way to extract and uudecode these files is to, in Emacs, put
;; the cursor before the ";; ***" and type C-x C-e.

(if nil
    (let ((directory (read-file-name "Directory for Mad Lib skeleton files: "
				     nil "~/" t))
	  begin)
      (if (not (file-directory-p directory))
	  (error "Must specify a directory for Mad Lib skeleton files."))
      (save-excursion
	(save-restriction
	  (widen)
	  (while (re-search-forward ";; begin 6[46]4 " nil t)
	    (setq begin (match-beginning 0))
	    (re-search-forward ";; end")
	    (let ((commented-uuencode (buffer-substring begin (point))))
	      (save-excursion
		(set-buffer (get-buffer-create
			     "temp Mad Lib skeleton extraction"))
		(setq default-directory (file-name-as-directory directory))
		(erase-buffer)
		;; Need a newline after "end" so the end line is recognized.
		(insert commented-uuencode "\n;; ")
		;; Now in column 3; delete all leading semicolons.
		(delete-rectangle (point-min) (point))
		
		;; In Emacs 19, shell-command-on-region works; but Emacs
		;; 18 requires this monkey business.
		(write-file "madlbtmp.uue")
		(shell-command
		 (concat "cd " directory "; uudecode madlbtmp.uue"))
		(kill-buffer (current-buffer))
		(delete-file (expand-file-name "madlbtmp.uue" directory))))
	  (setq madlib-directories (list directory))
	  (eval-current-buffer))))
      "Now you're ready to type M-x madlib RET to try a Mad Lib.")
  ;; *** Put cursor at BEGINNING of THIS line (before semicolons), type C-x C-e
  )

;; begin 644 advice.mad
;; M(" @(" @(" @(" @(" @($%D=FEC92!T;R!P<F]S<&5C=&EV92!P87)E;G1S
;; M"@I#;VYG<F%T=6QA=&EO;G,@=&\@86QL('EO=2 J861J*B!M;W1H97)S(&%N
;; M9" J861J*B!F871H97)S+B @66]U(&%R92!A8F]U="!T;PIG:79E(&)I<G1H
;; M('1O(&$@*FYO=6XJ+B @4F5M96UB97(L(&$@:&%P<'D@8VAI;&0@8V]M97,@
;; M9G)O;2!A(&AA<'!Y("IN;W5N*B N"E5N9&]U8G1E9&QY('1H92 J861J*B!A
;; M<G)I=F%L('=I;&P@8V%U<V4@;6%N>2 J861J*B!C:&%N9V5S(&EN('EO=7(@
;; M;&EF92X*66]U)VQL(&AA=F4@=&\@9V5T('5P(&%T(&9O=7(@02Y-+B!T;R!G
;; M:79E('1H92!L:71T;&4@*FYO=6XJ(&ET<R!B;W1T;&4@;V8**F%D:BH@3&%T
;; M97(@=VAE;B!S:&4G<R J;G5M8F5R*B!Y96%R<R!O;&0@<VAE)VQL(&QE87)N
;; M('1O('=A;&L@86YD('EO=2=L;"!H96%R"G1H92!P871T97(@;V8@;&ET=&QE
;; M("IP;'5R86P@;F]U;BH@87)O=6YD('1H92!H;W5S92X@($%N9"!I;B!N;R!T
;; M:6UE('-H92=L;"!B90IT86QK:6YG("IA9'8J(&%N9"!C86QL:6YG('EO=2!H
;; M97(@(BIN;W5N*B B(&%N9" B*FYO=6XJ+B B("!)="=S(&YO('=O;F1E<@IT
;; M:&5Y(&%R92!C86QL960@;&ET=&QE(&)U;F1L97,@;V8@*G!L=7)A;"!N;W5N
;; #*BX*
;;  
;; end
;;
;; begin 644 fmcsex.mad
;; M5&AE(&)E<W0@<W1O<GD@=V%S("IV97)B960J(&)Y("IF=6QL(&9E;6%L92!N
;; M86UE*BP@=VAO;2!)(&AA=F4@86QR96%D>0IC:&%R86-T97)I>F5D(&%S(&5X
;; M=')E;65L>2`J861J*BX@($]N92!D87D@=VAE;B!S:&4@=V]R:V5D(&]N($9/
;; M4DU!0PHH1D]2;75L82!-06YI<'5L871I;VX@*DYO=6X@0F5G:6YN:6YG(%=I
;; M=&@@0SUW8RHI(&%T("I#;VUP86YY($YA;64]8RH*:6X@*FEN=&5G97(J(%1E
;; M8VAN;VQO9WD@*DYO=6XJ+"!S:&4@<F5C96EV960@82!V:7-I="!F<F]M(&$@
;; M:&EG:&5R+75P"F%T("IC*BX@($%F=&5R('-P96YD:6YG(&$@*G5N:70@;V8@
;; M=&EM92H@=&%L:VEN9R!W:71H(&UE;6)E<G,@;V8@:&5R"BIN;W5N*BP@:&4@
;; M8V%M92!T;R!H97(@;V9F:6-E(&%N9"!G:6YG97)L>2!T;VQD(&AE<B!T:&5R
;; M92!W87,@82`J861J*@IM871T97(@=VAI8V@@:&4@:&%T960@=&\@:&%V92!T
;; M;R`J=F5R8BH@=&\@:&5R+B`@4VAE('1O;&0@:&EM('1O(&=O"F%H96%D+"!A
;; M;F0L(&%F=&5R("IV97)B:6YG*B!A9V%I;BP@:&4@87-K960@:68@<VAE('1H
;; M;W5G:'0@:70@=V%S"BIA9&H]82H@=&AA="!H97(@=V]R:V5R<R!W97)E('-P
;; M96YD:6YG('-O(&UU8V@@;V8@=&AE:7(@=&EM92`J=F5R8FEN9RH*86)O=70@
;; M<V5X+B`@17-P96-I86QL>2!B96-A=7-E('1H:7,@=V%S("IC*BP@<VAE(')E
;; M<&QI960@*F%D=BH@=&AA=`IS:&4@9&ED(&YO="!T:&EN:R!I="!W87,@*F$J
;; M(&%N9"!A<VME9"!W:&EC:"`J;F]U;G,J('=E<F4@9&]I;F<@=&AI<RX*069T
;; M97(@=&5L;&EN9R!H97(@=&AE(&YA;65S+"!H92!W87,@*F%D:BH@=VAE;B!S
;; M:&4@8G5R<W0@;W5T"BIV97)B:6YG*BP@<VEN8V4@<VAE('1H96X@=6YD97)S
;; M=&]O9"!W:&%T('=A<R!M96%N="X@($]N92!O9B!T:&4@;6%J;W(**FYO=6YS
;; M/6XJ(&EN($9/4DU!0R!W87,@(D9-0U-%6"(N("!3:6YC92!A;&P@;V8@=&AE
;; M("IN*B!B96=A;B!W:71H"B)&34,B+"!N;V)O9'D@<V%I9"!I="X@(")315@B
;; M('-T;V]D(&9O<B`B*D%D:F5C=&EV92!"96=I;FYI;F<@5VET:"!3*@I%6'!R
;; M97-S:6]N(BX@(%-I;F-E('1H:7,@=V%S(&]N92!O9B!T:&4@*FEN=&5G97(J
;; M(&UO<W0@:6UP;W)T86YT("IN*@II;B!T:&4@*G=C*BP@<VAE(&5X<&QA:6YE
;; M9"P@;V8@8V]U<G-E('!E;W!L92!W97)E(&1I<V-U<W-I;F<@:&]W('1O"BIT
;; M<F%N<VET:79E('9E<F(J(&ET(&%N9"!M86ME(&ET("IC;VUP87)A=&EV92!A
;; (9'9E<F(J(0IE
;; `
;; end
;; 
;; begin 644 group.mad
;; M(" @(" @(" @(" @(" @(" @1&5S8W)I<'1I;VX@;V8@=&AE($QO=F5L>2!'
;; M<F]U<"!T:&%T($D@86T@:6X*"E=E(&%R92!H879I;F<@82!P97)F96-T;'D@
;; M*F%D:BH@=&EM92!T:&ES(&5V96YI;F<@:6X@=&AE("IA9&HJ(&AO;64@;V8@
;; M*FYA;64*;V8@:&]S="!O<B!H;W-T97-S*BX@(%1H92!R;V]M<R!A<F4@9&5C
;; M;W)A=&5D("IA9'8J('=I=&@@;6%N>2!S='EL:7-H("IP;'5R86P*;F]U;BH@
;; M=&AA="!M=7-T(&AA=F4@8V]S="!A="!L96%S=" J;G5M8F5R*B!D;VQL87)S
;; M+B @5&AE(&=U97-T<R!A<F4@86QL("IA9&HJ"F-O;G9E<G-A=&EO;F%L:7-T
;; M<R!A;F0@87)E(&%L;" J861V*B!D<F5S<V5D+B @*FYA;64@;V8@<&5R<V]N
;; M(&EN(')O;VTJ(&AA<PIB965N(&5N=&5R=&%I;FEN9R!U<R!B>2!T96QL:6YG
;; M(&%B;W5T('1H92!T:6UE(&AE('-H;W=E9"!H:7,@*F%D:BH@*FYO=6XJ('1O
;; M"BIN86UE(&]F(&]T:&5R('!E<G-O;B!I;B!R;V]M*B!W:&\@;6ES=&]O:R!I
;; M="!F;W(@86X@96%R;'D@06UE<FEC86X@*FYO=6XJ+@I4:&4@<F5F<F5S:&UE
;; M;G1S(&%R92 J861V*B!A;F0@=&AE(&ED96$@;V8@<V5R=FEN9R J82!L:7%U
;; M:60J(&]N('1H92!R;V-K<PIS:&]W960@*F%D:BH@:6UA9VEN871I;VXN("!6
;; K:7-I=&EN9R!H97)E(&ES(&%L=V%Y<R!A("IA9&HJ(&5X<&5R:65N8V4N"B!6
;;  
;; end
;;
;; begin 644 newspaper.mad
;; M(" @(" @(" @(" @(" @(" @(" @(" @(" @3F5W<W!A<&5R($%R=&EC;&4*
;; M"DUR<RX@1FEF:2!686YD97)B;VQD+"!T:&4@*F%D:BH@86YD("IA9&HJ(&AE
;; M:7)E<W,@:&%S(&9I;&5D(&9O<B!D:79O<F-E(&9R;VT*:&5R(&AU<V)A;F0L
;; M(%!E<F-Y(%9A;F1E<F)O;&0L('1H92!F;W)M97(@*F%D:BH@*FYO=6XJ(&]F
;; M($AA<G9A<F0L(&-L87-S(&]F"B<S."P@;F]W(&EN('1H92 J;F]U;BH@8G5S
;; M:6YE<W,N("!-<G,N(%9A;F1E<F)O;&0@8VQA:6UE9"!T:&%T(&AE<B!H=7-B
;; M86YD(&AA9 HJ861V*B!G:79E;B!H97(@82 J;F]U;BH@:6X@=&AE(&5Y92!A
;; M;F0@:&%D(&MI8VME9"!H97(@='=I8V4@:6X@=&AE("IN;W5N*B!A;F0*=&AE
;; M("IN;W5N*BX@($UR+B @5F%N9&5R8F]L9"P@=VAE;B!A<VME9"!T;R!C;VUM
;; M96YT+"!S86ED("(J97AC;&%M871I;VXJ(2$*5&AI<R!I<R!A("IA9&HJ(&QI
;; G92X@($D@;VYL>2!P:6YC:&5D(&AE<B!I;B!T:&4@*FYO=6XJ+B(*
;;  
;; end
;;
;; begin 644 pond.mad
;; M"0D)("!!($1A>2!O;B!T:&4@4&]N9`H*3VYC92!U<&]N(&$@=&EM92!T:&5R
;; M92!W87,@82!F<F]G(&YA;65D("IN86UE/69R;V<J('-I='1I;F<@;VX@82`J
;; M;F%M92!O9@IP;&%N=#UH;VUE*B!P860L('1R>6EN9R!T;R!C871C:"!A("IA
;; M9&HJ(&1I;FYE<B!O9B`J86YI;6%L("AP;'5R86P*9F]R;2D]<')E>2HN("`J
;; M9G)O9RH@=V%S(&AA=FEN9R`J861J*B!L=6-K(&%N9"!A9G1E<B`J<&5R:6]D
;; M(&]F('1I;64J"FAA9&XG="!C875G:'0@82!S:6YG;&4@;VYE+B`@07)O=6YD
;; M('1H870@=&EM92`J;F%M93UT<F]U="HL("IF<F]G*B=S(&9R:65N9`IT:&4@
;; M*F9I<V@J+"!S=V%M('5P+B`@(D-A=6=H="!A;GD_(B`@*G1R;W5T*B!A<VME
;; M9"X@("))(&-A=6=H="!O;F4L(&)U="!I=`IW87,@=&]O("IA9&HJ(&%N9"!)
;; M(&AA9"!T;R`J=F5R8BH@:70@8F%C:RP@<V\@22=M(&YO=VAE<F4@;F5A<B!T
;; M:&4@;&EM:70L(@IS86ED("IF<F]G*B`J861V*BX@(")097)H87!S('1H92`J
;; M<')E>2H@8V%N('-E92!Y;W4L(B!S86ED("IT<F]U="HN("`B66]U"FYE960@
;; M=&\@8V%M;W5F;&%G92!Y;W5R<V5L9BXB("`B5VAA="!A("IA9&HJ(&ED96$A
;; M(B!E>&-L86EM960@*F9R;V<J+"!W:&\*=V5N="!L;V]K:6YG(&EN("IP;&%C
;; M92H@9F]R('-O;65T:&EN9R!S=6ET86)L92X@("IF<F]G*B!F:7)S="!T<FEE
;; M9`ID:7-G=6ES:6YG(&ET<V5L9B!W:71H("IN;W5N<RHN("!"=70@=&AA="!W
;; M87,@=&]O("IA9&HJ+B`@*F9R;V<J('1H96X@=')I960*8V]V97)I;F<@:71S
;; M("IB;V1Y('!A<G0J('=I=&@@<V]M92`J;F]U;G,J(&)U="!T:&5Y("IV97)B
;; M960J+B`@1FEN86QL>2P**F9R;V<J(')U8F)E9"!S;VUE("IN;W5N*B!O;B!I
;; M=',@*F)O9'D@<&%R="H@86YD(')E='5R;F5D('1O(&ET<R`J:&]M92H*<&%D
;; M+B`@5&AE("IP<F5Y*B!W97)E("IV97)B960J(&%N9"!C86UE("IV97)B:6YG
;; M*B!A;&]N9R!W:&5R975P;VX@*F9R;V<J"BIA9'8J(&%T92!E=F5R>2!L87-T
;; &(&]N92X*
;; `
;; end
;; 
;; begin 644 quiz.mad
;; M(" @(" @(" @(" @(" @(" @(" @(" @(" @475I8VL@475I>@H*5VAO(&%M
;; M($D_("!)(&%M(&$@*F%D:BH@06UE<FEC86XN("!)('=A<R!B;W)N("IN=6UB
;; M97(J('EE87)S(&%G;R!I;@HJ9V5O9W)A<&AI8V%L(&QO8V%T:6]N*BX@(%=H
;; M96X@;7D@9F%T:&5R(&9I<G-T('-A=R!M92!H92!S86ED+ HB*F5X8VQA;6%T
;; M:6]N*BXB("!)(&%M("IN=6UB97(J(&9E970@=&%L;"P@:&%V92 J861J*B!B
;; M<F]W;B!E>65S(&%N9"!A("IA9&HJ"F-O;7!L97AI;VXN("!->2!H;V)B>2!I
;; M<R!C;VQL96-T:6YG("IP;"X@;F]U;BHN("!)(&%L=V%Y<R!S<&5A:R J861V
;; M*B!A;F0@20IH879E(&UA9&4@<V5V97)A;" J861J*B!M;W1I;VX@<&EC='5R
;; M97,N("!)(&%M(&UA<G)I960@=&\@*G!E<G-O;B=S(&YA;64J+"!T:&4*=V5L
;; M;"UK;F]W;B!(;VQL>7=O;V0@*FYO=6XJ+B @22!H879E(&=I=F5N(&%W87D@
;; M=&AO=7-A;F0@;V8@*G!L+B!N;W5N*B!T;PIC:&%R:71Y+B @37D@;6]S="!P
;; M<F]M:6YE;G0@<&AY<VEC86P@8VAA<F%T97)I<W1I8W,@87)E(&UY("IA9&HJ
;; M(&YO<V4@86YD(&UY"FQA<F=E("IN;W5N*BX@(%=H;R!A;2!)/PH*04Y35T52
;; A.B!)(&%M("IN86UE(&]F('!E<G-O;B!I;B!R;V]M*BX*
;;  
;; end
;;
;; begin 644 safari.mad
;; M"0D)("!!(%-A9F%R:2!!9'9E;G1U<F4*"BIM86QE(&YA;64];6%L92H@86YD
;; M("IF96UA;&4@;F%M93UF96UA;&4J('=E<F4@;VX@<V%F87)I(&1E97`@:6X@
;; M=&AE(&AE87)T"F]F(&$@*F%D:BH@*FYO=6XJ+B`@5&AE>2!W97)E(')I9&EN
;; M9R!I;B!A(&QA<F=E("IC;VQO<BH@*F%D:BH**FYO=6X]=F5H:6-L92HN("!!
;; M<R!T:&5Y(')O=6YD960@=&AE(&)E;F0@=&AE>2!S87<@86X@96YO<FUO=7,@
;; M*F-O;&]R*@HJ86YI;6%L/7!R97DJ+B`@*FUA;&4J('1O;VL@;W5T(&AI<R!B
;; M:6<@*FYO=6XJ(&%N9"`J=F5R8F5D*B!I="X@($AE(&UI<W-E9`IT:&4@*G!R
;; M97DJ+"!A;F0@=&AE(&YA=&EV92`J;V-C=7!A=&EO;BH@*G9E<F)E9"HN("!4
;; M:&4@8G5L;&5T(&5N=&5R960@80HJ861J*B!R;W1T96X@=')E92!T<G5N:R!A
;; M;F0@=&AE("IP<F5Y*B!R86X@87=A>2P@*G9E<F)I;F<J("IA9'8J+B`@*F9E
;; M;6%L92H*;V9F97)E9"`J;6%L92H@;VYE(&]F(&AE<B!T87-T>2!H;VUE+6)A
;; M:V5D("IN;W5N<RH@:6X@8V]N<V]L871I;VXL(&)U="!J=7-T"G1H96X@=&AE
;; M('1R964@8F5G86X@=&\@9F%L;"X@($ET(&9E;&P@=VET:"!A(&QO=60@*FYO
;; M:7-E*BP@;F%R<F]W;'D@;6ES<VEN9PIT:&4@*G9E:&EC;&4J(&%N9"!R979E
;; M86QI;F<@82!C879E<FYO=7,@*FYO=6XJ+B`@*FUA;&4J(&=O="!O=70@;V8@
;; M=&AE"BIV96AI8VQE*B!T;R`J=F5R8BHN("!(92`J=F5R8F5D*B!I;G1O('1H
;; M92`J;F]U;BHL(&%N9"!R96%L:7IE9"!I="!W87,@80IT=6YN96PA("!2971R
;; M:65V:6YG(&$@*FYO=6XJ(&%N9"!A("IN;W5N*BP@:&4@=F5N='5R960@:6XN
;; M("!7:&5N(&AE(&AA9"!N;W0*>65T(')E='5R;F5D(&%F=&5R("ID969I;FET
;; M92!P97)I;V0@;V8@=&EM92HL("IF96UA;&4J(&)E9V%N('1O("IV97)B*BP@
;; M8G5T"G1H96X@:&4@87!P96%R960@8V%R<GEI;F<@<V5V97)A;"!B86=S(&]F
;; M("IN;W5N*B!A;F0@82`J861J*B!B=7-T(&]F"BIF86UO=7,@<&5R<V]N86QI
;; M='DJ+B`@(DDG=F4@9F]U;F0@82!T<F5A<W5R92!T<F]V92!O9B`J;F]U;G,J
;; M(2(@:&4*97AC;&%I;65D+B`@(BIF86UO=7,@<&5R<V]N86QI='DJ('=I;&P@
;; M87-K('5S('1O(&)E(&]N("I45B!S:&]W*BP@86YD('=E)VQL"F)E("IA9&HJ
;; -(&%N9"`J861J*B$B"F]N
;; `
;; end
;;
;; begin 644 wine.mad
;; M(" @(" @(" @(" @(" @(" @(" @("!(;W<@=&\@4V5R=F4@5VEN90H*02!G
;; M;V]D('=I;F4L('-E<G9E9" J861V*BP@8V%N(&UA:V4@86YY(&UE86P@82!T
;; M<G5L>2 J861J*B!O8V-A<VEO;BX@(%1H92!R960*=VEN97,@:&%V92!A("IA
;; M9&HJ(&9L879O<B!T:&%T(&)L96YD<R!W:71H(&)O:6QE9" J<&QU<F%L(&YO
;; M=6XJ(&]R('-M;VME9 HJ;F]U;BH@+B @5VAI=&4@=VEN97,@<F%N9V4@:6X@
;; M9FQA=F]R(&9R;VT@*F%D:BH@=&\@*F%D:BHN("!4:&4@8F5S="!W:6YE<R!A
;; M<F4*;6%D92!B>2!P96%S86YT<R!I;B J1V5O9W)A<&AI8V%L(&QO8V%T:6]N
;; M*B!F<F]M('1H92!J=6EC92!O9B!R:7!E("IP;'5R86P*;F]U;BH@8GD@<'5T
;; M=&EN9R!T:&5M(&EN('9A=',@86YD('-Q=6%S:&EN9R!T:&5M('=I=&@@=&AE
;; M:7(@*F%D:BH@9F5E="X@(%1H:7,*:7,@=VAA="!G:79E<R!W:6YE('1H870@
;; M*F%D:BH@87)O;6$N"@I(97)E(&%R92!A(&9E=R!R=6QE<SH*"B @("@Q*2!!
;; M;'=A>7,@<V5R=F4@=VAI=&4@=VEN92!I;B!A("IA9&HJ(&=L87-S(&%T("IN
;; M;W5N*B!T96UP97)A='5R92X*(" @*#(I($YE=F5R('-E<G9E($)U<F=U;F1Y
;; M('=I=&@@9G)I960@*G!L=7)A;"!N;W5N*BX*(" @*#,I(%=I;F5S('-H;W5L
;; M9"!A;'=A>7,@8F4@9')U;FL@*F%D=BH@;W(@>6]U)W)E(&QI86)L92!T;R!E
;; C;F0@=7 @=VET: H@(" @(" @82 J861J*B!S=&]M86-H+@II
;;  
;; end

;;; madlib.el ends here
