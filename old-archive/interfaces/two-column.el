; Path: dg-rtp!dgcad!amdcad!apple!mips!zaphod.mps.ohio-state.edu!wuarchive!uunet!mcsun!corton!cix!irit!pfeiffer
; From: pfeiffer@irit.fr (Daniel Pfeiffer)
; Newsgroups: comp.emacs,gnu.emacs.sources,soc.culture.esperanto
; Subject: latest enhanced two-column editing support
; Date: 31 May 1991 11:55:55 GMT
; Organization: Instituto de Reser^cado pri Informadiko de Tuluzo (IRIT)
; 	      Universitato Paul Sabatier
; 
; Here is the latest version, which is more robust against major mode
; changes, and which can handle any column widths.  The two columns now
; remain aligned if you use the minor mode commands for scrolling.  I
; was repeatedly asked to do this.  At first reluctant, since I thought
; that I didn't want it to work that way, I've since became a convert.
; 
; In addition to two-column editing of text, for example for writing a
; bilingual text side-by-side as shown below in the file's prolog, other
; interesting uses have been found for this minor mode:
; 
; 
; You can separate the columns with    {+} C-x 6 u  or  <f2> u  if you prefer
; any string that pleases you, by      {+} handles these with a prefix argument
; setting two-column:separator.  For   {+} that enables you to declare the
; example "{+}  " if you like to       {+}  desired length of such a string.
; amuse yourself.
; 
; 
; keyword You can write any text corresponding to a
; 	  given keyword in a filled paragraph next to
; 	  it.  Note that the width of the first column
; 	  may be less than window-min-width in the
; 	  result, but will be displayed at that width.
; 
; another This is not a three- or multi-column mode.
; 	  The example in the file's prolog required
; 	  working on two columns and then treating the
; 	  result as one column in order to add the
; 	  third.
; 
; 
; Programmers might like the ability to split off the comment column of
; a file that looks like the following.  The advantage is that with
; (setq fill-prefix "-- ") you can run M-q (fill-paragraph) on the
; comment.  The problem is, code quickly gets rather wide, so you need
; to use a narrower comment column, which is less interesting, unless
; you have a 132-column screen.  Code lines that reach beyond
; comment-column are no problem, except that you won't always see their
; end during editing.
; 
; BEGIN					-- This is just some meaningless
;     FOR i IN 1..10 LOOP		-- code in Ada, that runs foobar
; 	foobar( i );			-- once for each argument from one
;     END LOOP;				-- to ten, and then we're already
; END;					-- through with it.
; 
; Better yet, you can put the point before "This", type M-3 C-x 6 u
; which makes "-- " the separator between a no-comments Ada buffer, and
; a plain text comment buffer.  When you put them back together, every
; non-empty line of the 2nd column will again be preceded by "-- ".
; 
; 
; The <f2> function key hack (which is one of the rare times when
; function keys are mnemonic) at the end of the file's prolog requires
; that the lisp/term/*.el for your terminal use the standard
; conventions.  Too bad that some don't (at least not in version 18.55).
; The Sun one is hopelessly non-standard, and vt2[024]0 somehow forgot
; to define <f1> thru <f5>.  (It defines <pf1> thru <pf4> instead, but
; that is not what we need on an X terminal.)  If you want to use those,
; you'll need another hack something like:
; 
;       (if (string= (system-name) "cix")
; 	  (progn
; 	    (load-library "term/vt200.el")
; 	    (define-key CSI-map "12~" (cons function-keymap ?\^b)))
; 	(global-unset-key "\e[")
; 	(define-key esc-map "[225z" (cons function-keymap ?\^b)))
; 
; where "cix" is the non-sun machine I use.  Actually I use the same X
; terminal to connect to both machines, and I want to keep my ~/.emacs
; identical on both.  Bother, the two Emacses don't recognize the same
; keys and assign different sequences to those they do!  I sure hope all
; this nonsense will stop with version 19 (or preferably soon) where I'd
; like to be able to say (define-key some-map '<f2> some-cmd), and see
; <f2> rather than some unintelligible ESC-sequence in command key
; sequences.
; 
; Enjoy and keep me informed!
; 
; --
; -- Daniel Pfeiffer				<pfeiffer@cix.cict.fr>
; -- Tolosa (Toulouse), Midi-Pyrenees, Europe	<pfeiffer@irit.fr>
; -- "Beware - polyglot esperantist"		<pfeiffer@frcict81.bitnet>
; --
; 
; --8<---- two-column.el ----8<--------8<--------8<--------8<--------8<--------
; Esperanto:				English:

; Minora modalo por samtempa dukolumna	Minor mode for simultaneous
; tajpado				two-column editing

; Daniel Pfeiffer <pfeiffer@cix.cict.fr, @irit.fr>, 1991-05-14
; Copyright (C) 1991 Free Software Foundation, Inc.

; ^Ci dataro estas ero de GNU Emacs.	This file is part of GNU Emacs.

; GNU  Emacs  estas  disdonata   en la	GNU Emacs is distributed in the hope
; espero  ke ^gi estos utila,  sed SEN	that it will  be useful, but WITHOUT
; IA  GARANTIO.   Neniu   a^utoro  a^u	ANY    WARRANTY.     No  author   or
; disdonanto  akceptas respondecon  al	distributor   accepts responsibility
; iu ajn  por la sekvoj de ^gia uzado,	to  anyone for the   consequences of
; a^u  ^cu  ^gi serveblas al  iu celo,	using it or  for  whether it  serves
; a^u e^c  entute funkcias,  se  li ni	any particular purpose   or works at
; estas skribinta  tion.  Vidu la  GNU	all,  unless he says so  in writing.
; Emacs ^Generala Publika  Licenco por	Refer  to  the   GNU  Emacs  General
; plenaj detaloj.			Public License for full details.

; ^Ciu rajtas  kopii,  modifi kaj  ree	Everyone  is  granted permission  to
; disdoni  GNU Emacs,  sed nur  sub la	copy,  modify and  redistribute  GNU
; condi^coj  priskribitaj  en  la  GNU	Emacs, but only under the conditions
; Emacs  ^Generala  Publika   Licenco.	described in  the  GNU Emacs General
; Kopio de  tiu licenso estas supozata	Public  License.   A copy   of  this
; donita al vi kune kun GNU Emacs, por	license  is supposed to have    been
; ke   vi  sciu   viajn  rajtojn   kaj	given to you along with GNU Emacs so
; respondecojn.   ^Gi  devus  esti  en	you   can   know   your   rights and
; dataro    nomata    COPYING.   Inter	responsibilities.  It should be in a
; alia^joj,  la notico  pri  kopirajto	file named    COPYING.   Among other
; kaj  ^ci  notico devas esti  gardata	things, the  copyright   notice  and
; sur ^ciuj kopioj.			this notice must be preserved on all
;					copies.


; Tiu minora  modalo  ebligas  al   vi	This     minor mode  allows  you  to
; tajpi   sendepende  en   du   apudaj	independently    edit two   adjacent
; bufroj.  Vi  havas tri eblecojn  por	buffers.    You have three  ways  to
; eki    ^gin.   ^Ciu  donas  al    vi	start it  up.   Each  gives   you  a
; horizontale   disigatan   fenestron,	horizontally split window similar to
; simila  al  fina   apareco  de   via	the final outcome of your text:
; teksto:

; C-x 6 2  asocias  novan  bufron  nomatan  associates a new  buffer called
;	   same, sed kun 2C/ anta^u.	    the   same,    but   with   2C/
;					    prepended.

; C-x 6 b  asocias alian bufron.  Vi povas  associates    another   buffer.
;	   anka^u asocii  dataron,   se vi  This can be used to associate a
;	   ^jus anta^ue faris C-x C-f.	    file if you just did C-x C-f.

; C-x 6 u  disigas  jam dukolumnan tekston  unmerges a two-column text into
;	   en  du   bufroj  ekde  la  nuna  two  buffers from  the  current
;	   linio,  kaj je la nuna kolumno.  line and at the current column.
;	   La    anta^uaj   signoj   (ofte  The preceding characters (often
;	   tabeligilo  a^u  |)  estas   la  tab   or  |)  are   the  column
;	   kolumna disiganto.  Linioj kiuj  separator.   Lines  that  don't
;	   ne   enhavas   ilin   ne  estas  have them  won't  be separated.
;	   disigitaj.   Kiel  la kvara kaj  Like the  fourth and fifth line
;	   la   kvina  linio se vi disigas  if  you unmerge this  file from
;	   ^ci dataron ekde la unua  angla  the first english word.
;	   vorto.

; Je ^cia  flanko  estas  bufro,   kiu	On each side is a buffer  that knows
; konas la  alian.  Kun la ordonoj C-x	about the other.  With the  commands
; 6 SPC, C-x 6 DEL  kaj  C-x 6 RET oni	C-x 6 SPC,  C-x 6 DEL  and C-x 6 RET
; povas   suben-   a^u  supreniri  unu	you can  simultaneously scroll up or
; ekranon,    kaj   subeniri   linion,	down by  a screenfull  and by a line
; samtempe en la du bufroj. Al la alia	in both buffers.   Empty lines   are
; bufro  estas   aldonataj  linioj  se	added to  the  other    buffer    if
; necesas,  por  ke  vi vidu la  saman	necessary, so that  you see the same
; parton.  Per  C-x  6  C-l vi   povas	part.   With   C-x 6  C-l    you can
; recentrigi la linion.    Kiam vi nur	recenter the line.   When  you  only
; plu  havas    unu el   la du  bufroj	have one of the two buffers onscreen
; surekrane vi  revidos la alian   per	you can get the other back  with C-x
; denove C-x 6 2.			6 2 once more.

; Se  vi  volas  meti  longajn liniojn	If you include long lines, i.e which
; (ekz. programerojn) en la  kunigotan	will span both columns  (eg.  source
; tekston,   ili  devas  esti  en   la	code), they should  be  in what will
; estonte unua kolumno.  La alia devas	be the    first column,    with  the
; havi malplenajn linion apud ili.	associated buffer having empty lines
;					next to them.

; Averto: en Emacs kiam vi ^san^gas la	Attention:  in Emacs when you change
; ma^joran modalon, la minoraj modaloj	the major mode,  the minor modes are
; estas  anka^u  elmemorigitaj.   Tiu-	also  purged  from  memory.  In that
; okaze  vi devas religi la du bufrojn	case you   must  reassociate the two
; per iu  C-x 6-ordono,  ekz. C-x 6 b.	buffers with any C-x 6-command, e.g.
;					C-x 6 b.

; Kiam   vi   estos  kontenta   de  la	When you have edited both buffers to
; rezulto, vi kunmetos la du kolumnojn	your  content,  you merge them  with
; per  C-x 6 1.   Se  vi  poste  vidas	C-x 6 1.  If you then see a problem,
; problemon, vi  neniigu   la kunmeton	you undo the  merge with  C-x u  and
; per C-x u  kaj  plue  modifu  la  du	continue   to  edit the two buffers.
; bufrojn.  Kiam vi ne plu volas tajpi	When you  no longer  want to edit in
; dukolumne,  vi  eliru el  la  minora	two  columns, you turn off the minor
; modalo per C-x 6 k.			mode with C-x 6 k.


; An^stata^u tri `autoload' kaj tri  |  Instead  of  three  `autoload' and
; `global-set-key'  vi povas uzi la  |  three `global-set-key' you can use
; jenon en via dataro ~/.emacs, por  |  the    following   in  your   file
; memstare ^car^gi la modalon:	     |  ~/.emacs,  to  automatically  load
;				     |  the mode:

;	(global-set-key "\C-x6"
;			'(lambda () (interactive)
;			   (load-library "two-column")
;			   (call-interactively
;			    (cdr (assq (read-char) two-column:mode-map)))))

; Se vi ^satus  havi la dukolumnajn  |  If     you'd like   to  have   the
; ordonojn je funkciklavo <f2>,  vi  |  two-column  commands   on function
; povas  uzi la jenon en via dataro  |  key   <f2>,  you  can     use  the
; ~/.emacs:			     |  following in your file ~/.emacs:

;	(define-key function-keymap "\^b"
;	  '(lambda () (interactive)
;	     (load-library "two-column")
;	     (define-key function-keymap "\^b" two-column:mode-map)
;	     (call-interactively
;	      (cdr (assq (read-char) two-column:mode-map)))))

;;;;; variable declarations ;;;;;


(provide 'two-column)


(defvar two-column:prefix "\C-x6"
  "Prefix two-column:mode-map gets bound to.
If you'd like to bind it to function key <f2>, see the prolog of the
source file, lisp/two-column.el")

(defvar two-column:mode-map nil
  "Keymap that contains all commands useful with two-column minor mode.
This gets bound globally to `two-column:prefix' since minor modes have
no local keymap.")

(if two-column:mode-map
    ()
  (setq two-column:mode-map (make-sparse-keymap))
  (define-key two-column:mode-map "1" 'two-column:merge)
  (define-key two-column:mode-map "2" 'two-column:split)
  (define-key two-column:mode-map "b" 'two-column:associate-buffer)
  (define-key two-column:mode-map "k" 'two-column:kill-association)
  (define-key two-column:mode-map "\C-l" 'two-column:recenter)
  (define-key two-column:mode-map "o" 'two-column:associated-buffer)
  (define-key two-column:mode-map "u" 'two-column:unmerge)
  (define-key two-column:mode-map "{" 'shrink-window-horizontally)
  (define-key two-column:mode-map "}" 'enlarge-window-horizontally)
  (define-key two-column:mode-map " " 'two-column:scroll-up)
  (define-key two-column:mode-map "\^?" 'two-column:scroll-down)
  (define-key two-column:mode-map "\C-m" 'two-column:scroll-line))

(global-set-key two-column:prefix two-column:mode-map)


; markers seem to be the only buffer-id not affected by renaming
; a buffer.  This nevertheless loses when a buffer is killed.
(defvar two-column:other nil
  "Marker to the associated buffer, if non-nil.")
(make-variable-buffer-local 'two-column:other)


(defvar two-column:buffer-list ()
  "An alist of markers to associated buffers.  (Backs up `two-column:other')")


(setq minor-mode-alist (cons '(two-column:other " 2C") minor-mode-alist))


; rearranged, so that the pertinent info will show in 40 columns
(defvar two-column:mode-line-format
	'("-%*- %15b --"  (-3 . "%p")  "--%[("  mode-name
	  minor-mode-alist  "%n"  mode-line-process  ")%]%-")
  "*Value of mode-line-format for a buffer in two-column minor mode.")


(defvar two-column:separator ""
  "*A string inserted between the two columns when merging.
This gets set locally by \\[two-column:unmerge].")


(defvar two-column:window-width 40
  "*The width of the first column.  (Must be at least `window-min-width')
This value is local for every buffer that sets it.")
(make-variable-buffer-local 'two-column:window-width)


(defvar two-column:beyond-fill-column 4
  "*Base for calculating `fill-column' for a buffer in two-column minor mode.
The value of `fill-column' becomes `two-column:window-width' for this buffer
minus this value.")


(defvar two-column:mode-hook nil
  "Function called, if non-nil, whenever turning on two-column minor mode.
It can get called by \\[two-column:split] (two-column:split), \\[two-column:unmerge] (two-column:unmerge)
and \\[two-column:associate-buffer] (two-column:associate-buffer), on both buffers.")

;;;;; base functions ;;;;;


; the access method for the other buffer.  this tries to remedy against
; lost local variables and lost buffers.
(defun two-column:other ()
  (if (or two-column:other
	  (setq two-column:other
		; assoc with a different predicate, since we don't know
		; which marker points to this buffer
		(let ((bl two-column:buffer-list))
		  (while (and bl (not (eq (current-buffer)
					  (marker-buffer (car (car bl))))))
		    (setq bl (cdr bl)))
		  (cdr (car bl)))))
      (or (prog1
	      (marker-buffer two-column:other)
	    (setq mode-line-format two-column:mode-line-format ))
	  ; The associated buffer somehow got killed.
	  (progn
	    ; The other variables may later be useful if the user
	    ; reestablishes the association.
	    (kill-local-variable 'two-column:other)
	    (kill-local-variable 'mode-line-format)
	    nil))))


(defun two-column:split (&optional buffer)
  "Split current window vertically for two-column editing.

When called the first time, associates a buffer with the current
buffer.  Both buffers are put in two-column minor mode and
two-column:mode-hook gets called on both.  These buffers remember
about one another, even when renamed.

When called again, restores the screen layout with the current buffer
first and the associated buffer to it's right.

If you include long lines, i.e which will span both columns (eg.
source code), they should be in what will be the first column, with
the associated buffer having empty lines next to them.

You have the following commands at your disposal:

\\[two-column:split]   Rearrange screen
\\[two-column:associate-buffer]   Reassociate buffer after changing major mode
\\[two-column:scroll-up]   Scroll both buffers up by a screenfull
\\[two-column:scroll-down]   Scroll both buffers down by a screenful
\\[two-column:scroll-line]   Scroll both buffers up by one or more lines
\\[two-column:recenter]   Recenter and realign other buffer
\\[shrink-window-horizontally], \\[enlarge-window-horizontally]   Shrink, enlarge current column
\\[two-column:associated-buffer]   Switch to associated buffer
\\[two-column:merge]   Merge both buffers

These keybindings can be customized in your ~/.emacs by `two-column:prefix'
and `two-column:mode-map'.

The appearance of the screen can be customized by the variables
`two-column:window-width', `two-column:beyond-fill-column',
`two-column:mode-line-format' and `truncate-partial-width-windows'."

  (interactive "P")
  ; first go to full width, so that we can certainly split into
  ; two windows
  (if (< (window-width) (screen-width))
      (enlarge-window 99999 t))
  (split-window-horizontally
   (max window-min-width (min two-column:window-width
			      (- (screen-width) window-min-width))))

  (if (two-column:other)
      (progn
	(other-window 1)
	(switch-to-buffer (two-column:other))
	(other-window -1)
	; align buffers if necessary
	(two-column:scroll-line 0))

    ; set up minor mode linking two buffers
    (setq fill-column (- two-column:window-width
			 two-column:beyond-fill-column)
	  mode-line-format two-column:mode-line-format)
    (run-hooks two-column:mode-hook)
    (let ((other (point-marker)))
      (other-window 1)
      (switch-to-buffer
       (or buffer
	   (generate-new-buffer
	    (concat "2C/" (buffer-name)))))
      (or buffer
	  (text-mode))
      (setq fill-column (- two-column:window-width
			   two-column:beyond-fill-column)
	    mode-line-format two-column:mode-line-format
	    two-column:other other
	    other (point-marker))
      (setq two-column:buffer-list (cons (cons two-column:other other)
					 two-column:buffer-list))
      (run-hooks two-column:mode-hook)
      (other-window -1)
      (setq two-column:buffer-list
	    (cons (cons other
			(save-excursion
			  (set-buffer (two-column:other))
			  two-column:other))
		  two-column:buffer-list))
      (setq two-column:other other))))

(fset 'two-column:mode 'two-column:split)


(defun two-column:associate-buffer ()
  "Associate another buffer with this one in two-column minor mode.
Can also be used to associate a just previously visited file, by
accepting the proposed default buffer.

See  \\[two-column:split]  and  `lisp/two-column.el'  for further details."
  (interactive)
  (let ((b1 (current-buffer))
	(b2 (or (two-column:other)
		(read-buffer "Associate buffer: " (other-buffer)))))
    (save-excursion
      (setq two-column:other nil)
      (set-buffer b2)
      (and (two-column:other)
	   (not (eq b1 (two-column:other)))
	   (error "Buffer already associated with buffer `%s'."
		  (buffer-name (two-column:other))))
      (setq b1 (and (assq 'two-column:window-width (buffer-local-variables))
		    two-column:window-width)))
    ; if other buffer has a local width, adjust here too
    (if b1 (setq two-column:window-width (- (screen-width) b1)))
    (two-column:split b2)))


(defun two-column:unmerge (arg)
  "Unmerge a two-column text into two buffers in two-column minor mode.
The text is unmerged at the cursor's column which becomes the local
value of two-column:window-width.  Only lines that have the ARG same
preceding characters at that column get split.  The ARG preceding
characters without any leading whitespace become the local value for
`two-column:separator'.  This way lines that continue across both
columns remain untouched in the first buffer.

This function can be used with a prototype line, to set up things as
you like them.  You write the first line of each column with the
separator you like and then unmerge that line.  E.g.:

First column's text    sSs  Second columns text
		       \\___/\\
			/    \\
   5 character Separator      You type  M-5 \\[two-column:unmerge]  with the point here

See  \\[two-column:split]  and  `lisp/two-column.el'  for further details."
  (interactive "p")
  (and (two-column:other)
       (if (y-or-n-p (concat "Overwrite associated buffer `"
			     (buffer-name (two-column:other))
			     "'? "))
	   (save-excursion
	     (set-buffer (two-column:other))
	     (erase-buffer))
	 (signal 'quit nil)))
  (let ((point (point))
	; make next-line always come back to same column
	(goal-column (current-column))
	; a counter for empty lines in other buffer
	(n (1- (count-lines (point-min) (point))))
	chars other)
    (save-excursion
      (backward-char arg)
      (setq chars (buffer-substring (point) point))
      (skip-chars-forward " \t" point)
      (make-variable-buffer-local 'two-column:separator)
      (setq two-column:separator (buffer-substring (point) point)
	    two-column:window-width (current-column)))
    (two-column:split)
    (setq other (two-column:other))
    ; now we're ready to actually unmerge
    (save-excursion
      (while (not (eobp))
	(if (not (and (= (current-column) goal-column)
		      (string= chars
			       (buffer-substring (point)
						 (save-excursion
						   (backward-char arg)
						   (point))))))
	    (setq n (1+ n))
	  (setq point (point))
	  (backward-char arg)
	  (skip-chars-backward " \t")
	  (delete-region point (point))
	  (setq point (point))
	  (insert-char ?\n n)
	  (append-to-buffer other point (progn (end-of-line)
					       (if (eobp)
						   (point)
						 (1+ (point)))))
	  (delete-region point (point))
	  (setq n 0))
	(next-line 1)))))


(defun two-column:kill-association ()
  "Turn off two-column minor mode in current and associated buffer.
If the associated buffer is unmodified and empty, it is killed."
  (interactive)
  (let ((buffer (current-buffer)))
    (save-excursion
      (and (two-column:other)
	   (prog2
	    (setq two-column:buffer-list
		  (delq (assq two-column:other two-column:buffer-list)
			two-column:buffer-list))
	    (set-buffer (two-column:other))
	    (setq two-column:buffer-list
		  (delq (assq two-column:other two-column:buffer-list)
			two-column:buffer-list)))
	   (or (not (two-column:other))
	       (eq buffer (two-column:other)))
	   (if (and (not (buffer-modified-p))
		    (eobp) (bobp))
	       (kill-buffer nil)
	     (kill-local-variable 'two-column:other)
	     (kill-local-variable 'two-column:window-width)
	     (kill-local-variable 'two-column:separator)
	     (kill-local-variable 'mode-line-format)
	     (kill-local-variable 'fill-column))))
    (kill-local-variable 'two-column:other)
    (kill-local-variable 'two-column:window-width)
    (kill-local-variable 'two-column:separator)
    (kill-local-variable 'mode-line-format)
    (kill-local-variable 'fill-column)))


; this doesn't use yank-rectangle, so that the first column can
; contain long lines
(defun two-column:merge ()
  "Merges the associated buffer with the current buffer.
They get merged at the column, which is the value of
`two-column:window-width', i.e. usually at the vertical window
separator.  This separator gets replaced with white space.  Beyond
that the value of gets inserted on merged lines.  The two columns are
thus pasted side by side, in a single text.  If the other buffer is
not displayed to the left of this one, then this one becomes the left
column.

If you want `two-column:separator' on empty lines in the second column,
you should put just one space in them.  In the final result, you can strip
off trailing spaces with \\[beginning-of-buffer] \\[replace-regexp] [ SPC TAB ] + $ RET RET"

  (interactive)
  (or (two-column:other)
      (error "You must first set two-column minor mode."))
  (and (> (car (window-edges)) 0)	; not touching left edge of screen
       (eq (window-buffer (previous-window))
	   (two-column:other))
       (other-window -1))
  (save-excursion
    (let ((b1 (current-buffer))
	  (b2 (two-column:other))
	  string)
      (goto-char (point-min))
      (set-buffer b2)
      (goto-char (point-min))
      (while (not (eobp))
	(setq string (buffer-substring (point)
				       (progn (end-of-line) (point))))
	(or (eobp)
	    (forward-char))		; next line
	(set-buffer b1)
	(if (string= string "")
	    ()
	  (end-of-line)
	  (indent-to-column two-column:window-width)
	  (insert two-column:separator string))
	(next-line 1)			; add one if necessary
	(set-buffer b2))))
  (if (< (window-width) (screen-width))
      (enlarge-window 99999 t)))

;;;;; utility functions ;;;;;


(defun two-column:associated-buffer ()
  "Switch to associated buffer."
  (interactive)
  (or (two-column:other)
      (error "You must set two-column minor mode."))
  (if (get-buffer-window (two-column:other))
      (select-window (get-buffer-window (two-column:other)))
    (switch-to-buffer (two-column:other))))


; It would be desirable to intercept anything that causes the current
; window to scroll.  Maybe a `scroll-hook'?
(defun two-column:scroll-line (arg)
  "Scroll current window upward by ARG lines.
The associated window gets scrolled to the same line."
  (interactive "p")
  (or (two-column:other)
      (error "You must set two-column minor mode."))
  ; scroll-up has a bug on arg 0 at end of buffer
  (or (zerop arg)
      (scroll-up arg))
  (setq arg (count-lines (point-min) (window-start)))
  ; too bad that pre 18.57 Emacs makes save-window-excursion restore
  ; the point.  When it becomes extinct, we can simplify this.
  (if (get-buffer-window (two-column:other))
      (let ((window (selected-window)))
	(select-window (get-buffer-window (two-column:other)))
	(setq arg (- arg (count-lines (point-min) (window-start))))
	; make sure that other buffer has enough lines
	(save-excursion
	  (goto-char (point-max))
	  (insert-char ?\n
		       (- arg (count-lines (window-start) (point-max)) -1)))
	(or (zerop arg)
	    (scroll-up arg))
	(select-window window))))


(defun two-column:scroll-up (arg)
  "Scroll current window upward by ARG screens.
The associated window gets scrolled to the same line."
  (interactive "p")
  (two-column:scroll-line (* arg (- (window-height)
				    next-screen-context-lines 1))))


(defun two-column:scroll-down (arg)
  "Scroll current window downward by ARG screens.
The associated window gets scrolled to the same line."
  (interactive "p")
  (two-column:scroll-line (* arg (- next-screen-context-lines
				    (window-height) -1))))


(defun two-column:recenter (arg)
  "Center point in window.  With ARG, put point on line ARG.
This counts from bottom if ARG is negative.  The associated window
gets scrolled to the same line."
  (interactive "P")
  (setq arg (and arg (prefix-numeric-value arg)))
  (two-column:scroll-line (- (count-lines (window-start) (point))
			     (cond ((null arg)  (/ (window-height) 2))
				   ((< arg 0)  (+ (window-height) arg))
				   (  arg)))))


(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t)
  (and (two-column:other)
       (setq two-column:window-width (+ two-column:window-width arg))
       (set-buffer (two-column:other))
       (setq two-column:window-width (- two-column:window-width arg))))


(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (enlarge-window-horizontally (- arg)))

