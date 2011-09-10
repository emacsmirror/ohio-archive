;
Date: Sun, 5 Jul 92 11:49:45 EDT
From: klm@cme.nist.gov (Ken Manheimer)
Subject: 'allout.el' submission for LCD elisp archive directory

Around a half year ago i posted a package of extensions to emacs
outline mode, which i called 'outext.el'.  In the interim i've
developed some much more substantial extensions and revisions of the
base outline mode, an essentially self contained package called
'allout.el'.  The code is attached below.

I released v 2.0 to the Usenet newsgroup gnu.emacs.sources a while
back, and have gotten some bug fixes, which i've applied along with my
own fixes developed in the interim.  I think i've got most if not all
of the significant bugs, but i'm inundated at work and home, and doubt
i'll have time to write up anything like an info file or even a
detailed outline of the new features, so you'll have to make do for
now with the 'outline-mode' and other functions' documentation
strings, if you're interested in giving it a whirl.

Included below is, first, some exposition on the functionality of the
package, then a patch to Kyle Jone's filladapt package, which must be
applied in order to capitalize on filladapts features within the
allout outline mode.  Below that (delimited by a "Cut Here" type line)
is the allout.el code for the Emacs lisp archive.


My original revisions included functions to create new topic templates
relative to the current topic, shift entire subtrees to greater or
lesser nesting depths, and use an alternate, more cogent bulleting
scheme.  All of the functionality of the original outline mode was
supported, including the old topic bulleting scheme.  (In fact, the
code for the original outline version was used together with the new
'outext' code.)

This new release supports all those features in a more fleshed out
way, plus:

  - Copy, cut, yank, and yank-pop of outline subtrees as units, with
    automatic adjustment of the subtree depth to the depth of the new
    location (if pasted into a blank topic template);

  - Adjustment of the bullet chars when you shift the depth of
    subtrees, according to level;

  - "distinctive bullets", a specific user-configurable set of
    characters which are immune to the level-change adjustments, so
    special topic flags are retained across such shifts.  (See the
    documentation string for 'outline-stylish-prefixes' for an
    rudimentary example of the outline bulleting scheme with this and
    the next elaborations.)

  - "numbered bullets", which automatically get numbers that indicate
    how many siblings come between the topic and its parent, which
    numbers are automatically reconciled upon the inclusion of new and
    deletion of intervening siblings;

  - A substantial definition of outline terms and functions, in the
    documentation string for the 'outline-mode' function;

  - A complete, self contained package, loaded instead of rather than
    in addition to the distributed emacs outline code;

  - Clearer opportunities for outline code developement - rationalized
    and reconciled function names, plus more thoroughly documented
    code, for all outline functions.

There are two other, exceptionally nifty features, but they require
use of other custom elisp software (both available from the lisp
archives) in conjunction with allout:

  - Incremental search which dynamically exposes hidden outline bodies
    when the search hits inside them, reconceals them if the search is
    then continued past, or reveals the subtopics of all hidden
    ancestors down to the target topic if the search is quit there.
    You need to be using dan laliberte's 'isearch-mode' package for
    this feature.

  - Automatically maintained hanging indentation on topic bodies,
    which is accordingly adjusted when the depth of the topic is
    changed.  You need to be using kyle jones 'filladapt' package to
    get this feature.

I heartily recommend both of these packages in their own right, so i
don't think you'll be losing anything to give them a try, and see what
you it gets you in allout outline mode.

This is all the explanation i have time for now.  Alas, i doubt i'll
get much time in the near future to write things up more thoroughly.
However, if you find yourself really interested but you're perplexed
by some feature or behavior, drop me an email-line and i'll see what i
can do to help.

Cheers, 

Ken Manheimer
klm@cme.nist.gov, 301 975-3539

8<-------------------------- Cut Here -------------------------->8

As seems inevitable, i neglected to include some customizations you
need to apply to the distributed (lisp archives) version of kyle
jones' filladapt package, if you want allout to use it for hanging
topic bodies.  I'm attaching below a diff of the distributed version
(from the elisp archive - i believe it's as of Sep 89) to my modified
one.

If i am recalling correctly, my changes amount to parameterizing
hanging list prefixes so individual modes can define their own special
ones, which is what allout does.  (I also extend the default hanging
list regexps to include ':' colons.)

To apply the diffs,

	patch filladapt.el < thisMessage

I hope people find allout as useful as i have.  It's more than
returned the time i invested, enabling me to keep track of a lot more
than i previously could.  This can be a lifesaver in system
management/support type jobs, and probably would be in a lot of
others...

Ken again.
klm@cme.nist.gov, 301 975-3539

40a41,50
> ;;; klm 20-Mar-1992 Parameterize the hanging-list prefixes so we can add to
> ;;;		    it from, eg, outline mode...
> (defvar filladapt-hanging-list-prefixes
>   '(" *(?\\([0-9]+[a-z]?\\|[a-z]\\)) +"
>     " *\\([0-9]+[a-z]?\\|[a-z]\\)\\. +"
>     " *[?!"~*+---]+ +")
>   "A list of regular expression strings which can head hanging lists.")
> 
> ;;;klm My 'outext.el' adds a 'filladapt-hanging-list' entry for the fancy
> ;;;    outline prefixes.
57,61c67,71
<     ;; 1. xxxxx   or   *   xxxxx   etc.
<     ;;    xxxxx            xxx
<     (" *(?\\([0-9]+[a-z]?\\|[a-z]\\)) +" . filladapt-hanging-list)
<     (" *\\([0-9]+[a-z]?\\|[a-z]\\)\\. +" . filladapt-hanging-list)
<     ("[?!~*+--- ]+ " . filladapt-hanging-list)
---
>     ;; 1. xxxxx	  or   *   xxxxx   etc.
>     ;;	  xxxxx		   xxx
>     (" *(?\\([0-9:]+[a-z]?\\|[a-z]\\)) +" . filladapt-hanging-list)
>     (" *\\([0-9:]+[a-z]?\\|[a-z]\\)\\. +" . filladapt-hanging-list)
>     ("[?!~*+:--- ]+ " . filladapt-hanging-list)
72c82
< is found the crorrespoding FUNCTION is called.  FUNCTION is called with
---
> is found the corresponding FUNCTION is called.	FUNCTION is called with
157a168,173
> (defun filladapt-looking-at-prefixes (prefixes)
>   "Recursively check point for prefixes, until we find one."
>   (cond ((null prefixes) nil)
> 	((looking-at (car prefixes)))
> 	(t (filladapt-looking-at-prefixes (cdr prefixes)))))
> 
165a182
> 		;; Get to the beginning of the indented stuff:
168,173c185,192
< 		(cond ((or (looking-at " *(?\\([0-9]+[a-z]?\\|[a-z]\\)) +")
< 			   (looking-at " *\\([0-9]+[a-z]?\\|[a-z]\\)\\. +")
< 			   (looking-at " *[?!~*+---]+ +"))
< 		       (setq beg (point)))
< 		      (t (setq beg (progn (forward-line 1) (point))))))
< 	    (setq beg (point)))
---
> 		;; Check the line prior to the indented stuff for the prefix:
> 		(if (filladapt-looking-at-prefixes
> 		     filladapt-hanging-list-prefixes)
> 		    ;; setting the beginning to the match if we found it:
> 		    (setq beg (match-end 0))
> 		  ;; else beginning of starting line if not:
> 		  (setq beg (progn (forward-line 1) (point)))))
> 	    (setq beg (match-end 0)))


8<-------------------------- Cut Here -------------------------->8
;; LCD Archive Entry:
;; allout|Ken Manheimer|klm@cme.nist.gov|
;; A more thorough outline-mode|
;; 07-05-92|V 2.1|~/modes/allout.el.Z|

;; A full-fledged outline mode, based on the original rudimentary
;; GNU emacs outline functionality.  This version supercedes the
;; original GNU outline.el and my outext.el (v 1.1, which is the only
;; one i officially released).  klm 6-Apr-1992
;;
;; Ken Manheimer		 	Nat'l Inst of Standards and Technology
;; klm@cme.nist.gov (301)975-3539	(Formerly Nat'l Bureau of Standards)
;;    Factory Automation Systems Division Unix Systems Support Manager

;; Copyright (C) 1991 Free Software Foundation, Inc.

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

(provide 'outline)

;=======================================================================
;                     Outline Variables

;-----------------------------------------------------------------------
; Basic configuration vars:

(defconst outline-primary-bullet "*") ;; This var should probably not be
                                      ;; changed - backwards compatability
                                      ;; and convention depend on it.

(defvar outline-plain-bullets-string ""
  "   The bullets normally used in outline topic prefixes.  See
   'outline-distinctive-bullets-string' for the other kind of
   bullets.

   You must run 'set-outline-regexp' in order for changes to the
   value of this var to effect outline-mode operation.")
(setq outline-plain-bullets-string (concat outline-primary-bullet
                                           "+-.:,;=%"))
(make-variable-buffer-local 'outline-plain-bullets-string)

(defvar outline-distinctive-bullets-string ""
  "   The bullets used for distinguishing outline topics.  These
   bullets are not offered among the regular rotation, and are not
   changed when automatically rebulleting, as when shifting the
   level of a topic.  See 'outline-plain-bullets-string' for the
   other kind of bullets.

   You must run 'set-outline-regexp' in order for changes
   to the value of this var to effect outline-mode operation.")
(setq outline-distinctive-bullets-string "&!?(#\"X@$~")
(make-variable-buffer-local 'outline-distinctive-bullets-string)

(defvar outline-numbered-bullet ()
  "   Bullet signifying outline prefixes which are to be numbered.
   Leave it nil if you don't want any numbering, or set it to a
   string with the bullet you want to be used.")
(setq outline-numbered-bullet "#")
(make-variable-buffer-local 'outline-numbered-bullet)

;-----------------------------------------------------------------------
;                  Topic Header Style Configuration
;
; The following vars affect the basic behavior of outline topic
; creation and manipulation.

(defvar outline-stylish-prefixes t
  "*A true value for this var makes the topic-prefix creation and modification
   functions vary the prefix bullet char according to level.  Otherwise, only
   asterisks ('*') and distinctive bullets are used.

   This is how an outline can look with stylish prefixes:

   * Top level
   .* A topic
   . + One level 3 subtopic
   .  . One level 4 subtopic
   . + Another level 3 subtopic
   .  . A level 4 subtopic
   .  #2 A distinguished, numbered level 4 subtopic
   .  ! A distinguished ('!') level 4 subtopic
   .  #4 Another numbered level 4 subtopic
   
   This would be an outline with stylish prefixes inhibited:

   * Top level
   .* A topic
   .! A distinctive (but measly) subtopic
   . * A sub-subtopic - no bullets from outline-plain-bullets-string but '*'

   Stylish and constant prefixes (as well as old-style prefixes) are
   always respected by the topic maneuvering functions, regardless of
   this variable setting.

   The setting of this var is not relevant when outline-old-style-prefixes
   is t.")
(make-variable-buffer-local 'outline-stylish-prefixes)

(defvar outline-old-style-prefixes nil
  "*Setting this var causes the topic-prefix creation and modification
   functions to make only asterix-padded prefixes, so they look exactly
   like the old style prefixes.

   Both old and new style prefixes are always respected by the topic
   maneuvering functions.")
(make-variable-buffer-local 'outline-old-style-prefixes)

;-----------------------------------------------------------------------
; Incidental configuration vars (incidental subsystems interaction with
; outline):

                   ;;; Currently only works with Dan LaLiberte's isearch-mode:
(defvar outline-enwrap-isearch-mode t
  "*  Set this var non-nil if you have Dan LaLiberte's 'isearch-mode'
   stuff, and want isearches to reveal hidden stuff encountered in the
   course of a search (and reconceal it if you go past).")

(defvar outline-use-hanging-indents t
  "*  Set this var non-nil if you have Kyle E Jones' filladapt stuff,
  and you want outline to fill topics as hanging indents to the
  bullets.")
(make-local-variable 'outline-use-hanging-indents)

(defvar outline-reindent-bodies t
  "*  Set this var non-nil if you want topic depth adjustments to
  reindent hanging bodies (ie, bodies lines indented to beginning of
  heading text).  The performance hit is small.")
(make-local-variable 'outline-reindent-bodies)

;-----------------------------------------------------------------------
; Internal configuration variables

(defvar outline-mode-map nil "Keybindings for outline mode.")

(defvar outline-regexp ""
  "*   Regular expression to match the beginning of a heading line.
   Any line whose beginning matches this regexp is considered a
   heading.  This var is set according to the user configuration vars
   by set-outline-regexp.")

(defvar outline-bullets-string ""
  "   A string dictating the valid set of outline topic bullets.  This
   var should *not* be set by the user - it is set by 'set-outline-regexp',
   and is composed from the elements of 'outline-plain-bullets-string'
   and 'outline-distinctive-bullets-string'.")

(defvar outline-line-boundary-regexp ()
  "   outline-regexp with outline-style beginning of line anchor (ie,
   C-j, *or* C-m, for prefixes of hidden topics, *or* beginning of the
   buffer).  This is properly set when outline-regexp is produced by
   'set-outline-regexp', so that (match-beginning 2) and (match-end 2)
   delimit the prefix.")

;;; Recent-topic-search-state data:

;;; All basic outline functions which directly do string matches to
;;; evaluate heading prefix location set the variables
;;; outline-recent-prefix-beginning and outline-recent-prefix-end when
;;; successful.  Functions starting with 'outline-recent-' all use
;;; this state, providing the means to avoid redundant searches for
;;; just established data.  This optimization can be significant but
;;; must be employed carefully.

(defvar outline-recent-prefix-beginning 0
  "   Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-beginning)
(defvar outline-recent-prefix-end 0
  "   Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-end)

;=======================================================================
;                         Outline Initializations

(if outline-mode-map
    nil
  (setq outline-mode-map (copy-keymap text-mode-map))
  (define-key outline-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-map "\C-c\C-i" 'outline-show-current-children)
  (define-key outline-mode-map "\C-c\C-s" 'outline-show-current-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'outline-hide-current-subtree)
  (define-key outline-mode-map "\C-c\C-u" 'outline-up-current-level)
  (define-key outline-mode-map "\C-c\C-f" 'outline-forward-current-level)
  (define-key outline-mode-map "\C-c\C-b" 'outline-backward-current-level)
  ;; klm extensions:
  (define-key outline-mode-map "\C-cc" 'outline-copy-exposed)
  (define-key outline-mode-map "\C-c\C-a" 'outline-show-current-branches)
  (define-key outline-mode-map "\C-c\C-e" 'outline-show-current-entry)
  (define-key outline-mode-map "\C-c " 'open-sibtopic)
  (define-key outline-mode-map "\C-c." 'open-subtopic)
  (define-key outline-mode-map "\C-c," 'open-supertopic)
  (define-key outline-mode-map "\C-c'" 'outline-shift-in)
  (define-key outline-mode-map "\C-c>" 'outline-shift-in)
  (define-key outline-mode-map "\C-c<" 'outline-shift-out)
  (define-key outline-mode-map "\C-c\C-m" 'outline-rebullet-topic)
  (define-key outline-mode-map "\C-cb" 'outline-rebullet-current-heading)
  (define-key outline-mode-map "\C-c#" 'outline-number-siblings)
  (define-key outline-mode-map "\C-k" 'outline-kill-line)
  (define-key outline-mode-map "\C-y" 'outline-yank)
  (define-key outline-mode-map "\M-y" 'outline-yank-pop)
  (define-key outline-mode-map "\C-c\C-k" 'outline-kill-topic))


(defun outline-mode ()
  "  Set major mode for editing outlines with selective display.

   Below the description of the bindings is explanation of the outline
   mode terminology.

Exposure Commands		      Movement Commands
C-c C-h	outline-hide-current-subtree  C-c C-n outline-next-visible-heading
C-c C-i	outline-show-current-children C-c C-p outline-previous-visible-heading
C-c C-s	outline-show-current-subtree  C-c C-u outline-up-current-level
C-c C-a	outline-show-current-branches C-c C-f outline-forward-current-level
C-c C-e	outline-show-current-entry    C-c C-b outline-backward-current-level
        outline-show-all - expose entire buffer
	outline-hide-current-leaves

Topic Header Generation Commands
C-c 	open-sibtopic		Create a header for a sibling of current topic
C-c .	open-subtopic		... for an offspring of current topic
C-c ,	open-supertopic		... for a sibling of the current topic's parent

Level and Prefix Adjustment Commands
C-c >	outline-shift-in	Shift current topic and all offspring deeper
C-c <	outline-shift-out	... less deep
C-c C-m	outline-rebullet-topic	Reconcile bullets of topic and offspring
C-c b	outline-rebullet-current-heading Prompt for alternate bullet for
					 current topic (no CR necessary)
C-c #	outline-number-siblings	Number bullets of topic and siblings - the
				offspring are not affect.  With repeat count,
				revoke numbering.
C-k	outline-kill-line	Kill line, reconciling subsequent numbering
C-y	outline-yank		Yank, reconciling numbering, and adjusting
				yanked topics to depth of heading if yanking
				into bare topic prefix.
M-y	outline-yank-pop	
C-c C-k	outline-kill-topic	Kill current topic, including offspring.

Misc commands
C-cc	outline-copy-exposed	Copy outline sans all hidden stuff to
				another buffer whose name is derived
				from the current one - \"XXX exposed\"

                             Terminology

Topic: A basic cohesive component of an emacs outline, which can
       be closed (made hidden), opened (revealed), generated,
       traversed, and shifted as units, using outline-mode functions.
       A topic is composed of a HEADER, a BODY, and SUBTOPICs (see below).

Exposure: Hidden (~closed~) topics are represented by ellipses ('...')
          at the end of the visible SUPERTOPIC which contains them,
          rather than by their actual text.  Hidden topics are still
          susceptable to editing and regular movement functions, they
          just are not displayed normally, effectively collapsed into
          the ellipses which represent them.  Outline mode provides
          the means to selectively expose topics based on their
          NESTING.

          SUBTOPICS of a topic can be hidden and subsequently revealed
          based on their DEPTH relative to the supertopic from which
          the exposure is being done.

          The BODIES of a topic do not generally become visible except
          during exposure of entire subtrees (see documentation for
          '-current-subtreesubtree'), or when the entry is explicitly exposed
          with the 'outline-show-entry' function, or (if you have a special
          version of isearch installed) when encountered by
          incremental searches.

          The CURRENT topic is the more recent visible one before or
          including the text cursor.

Header: The initial portion of an outline topic.  It is composed of a
        topic header PREFIX at the beginning of the line, followed by
        text to the end of the EFFECTIVE LINE.

Body: Any subsequent lines of text following a topic header and preceeding
      the next one.  This is also referred to as the entry for a topic.

Prefix: The text which distinguishes topic headers from normal text
        lines.  There are two forms, both of which start at the beginning
        of the topic header (EFFECTIVE) line.  The length of the prefix
        represents the DEPTH of the topic.  The fundamental sort begins
        either with solely an asterisk ('*') or else dot ('.')  followed
        by zero or more spaces and then an outline BULLET.  The second
        form is for backwards compatability with the original emacs
        outline mode, and consists solely of asterisks.  Both sorts are
        recognized by all outline commands.  The first sort is generated
        by outline topic production commands if the emacs variable
        outline-old-style-prefixes is nil, otherwise the second style is
        used.

Bullet: An outline prefix bullet is one of the characters on either
        of the outline bullet string vars, 'outline-plain-bullets-string'
        and 'outline-distinctive-bullets-string'.  (See their
        documentation for more details.)  The default choice of bullet
        for any prefix depends on the DEPTH of the topic.

Depth and Nesting:
       The length of a topic header prefix, from the initial
       character to the bullet (inclusive), represents the depth of
       the topic.  A topic is considered to contain the subsequent
       topics of greater depth up to the next topic of the same
       depth, and the contained topics are recursively considered to
       be nested within all containing topics.  Contained topics are
       called subtopics.  Immediate subtopics are called 'children'.
       Containing topics are supertopicsimmediate supertopics are
       'parents'.  Contained topics of the same depth are called
       siblings.

Effective line: The regular ascii text in which form outlines are
                saved are manipulated in outline-mode to engage emacs'
                selective-display faculty.  The upshot is that the
                effective end of an outline line can be terminated by
                either a normal Unix newline char, \n, or the special
                outline-mode eol, ^M.  This only matters at the user
                level when you're doing searches which key on the end of
                line character."

  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  ;; klm extension:
  (set-outline-regexp)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|^\\("
				outline-regexp "\\)"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|^\\("
				   outline-regexp "\\)"))

  ;; klm extension:
  (if outline-enwrap-isearch-mode
      (outline-enwrap-isearch))
  ;; klm extension:
  (if (and outline-use-hanging-indents
           (boundp 'filladapt-prefix-table))
      ;; Add outline-prefix recognition to filladapt - not standard:
      (progn (setq filladapt-prefix-table
                   (cons (cons (concat "\\(" outline-regexp "\\) ")
                               'filladapt-hanging-list)
                         filladapt-prefix-table))
             (setq filladapt-hanging-list-prefixes
                   (cons outline-regexp filladapt-hanging-list-prefixes))))


  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defun set-outline-regexp ()
  "   Generate proper topic-header regexp form for outline functions, from
   outline-plain-bullets-string and outline-distinctive-bullets-string."

  (interactive)
  ;; Derive outline-bullets-string from user configured components:
  (setq outline-bullets-string (concat outline-plain-bullets-string
                                       outline-distinctive-bullets-string))
  ;; Derive next for repeated use in outline-pending-bullet:
  (setq outline-plain-bullets-string-len (length outline-plain-bullets-string))
  ;; Produce the new outline-regexp:
  (set-outline-regexp-grunt (mapcar '(lambda (x) x) outline-bullets-string))
  (setq outline-line-boundary-regexp
        (concat "\\(\\`\\|[\C-j\C-m]\\)\\(" outline-regexp "\\)"))
  (make-variable-buffer-local 'outline-regexp)
  (make-variable-buffer-local 'outline-bullets-string)
  (make-variable-buffer-local 'outline-line-boundary-regexp)
  )

(defun set-outline-regexp-grunt (raw &optional cooked)
  "   Do the real work for set-outline-regexp.  Requires a raw string of
   bullets, RAW.  (Internally uses a second arg, COOKED, to recursively
   accumulate the final form.)"
  (cond

   ;; Base case, we've transformed all of 'raw' to 'cooked' - finish up:
   ((null raw)
    (setq outline-regexp (concat "\\(\\. *[" cooked "]\\)\\|\\*+\\|\^l")))

   ;; Initialization case - no vals - prime function:
   ((null cooked) (set-outline-regexp-grunt raw ""))

   ;; Recurse with leading char in raw form transformed to proper escaped
   ;; string on end of cooked form:
   ((let* ((this-char (car raw))
	   (this-bullet (char-to-string this-char)))
      ;; The recursive invocation:
      (set-outline-regexp-grunt
       ;; The remaining raw string:
       (cdr raw)
       ;; The burgeoning cooked string:
       (concat cooked
               ;; Do the right thing for various chars:
               (cond ((eq this-char ?-) "--")
                     ((eq this-char ?\() (char-to-string this-char))
                     (t (concat  "\\" (char-to-string this-char)))))
       )
      ))
   )
  )

;-----------------------------------------------------------------------
;                      Outline State Functions

(defun outline-recent-depth ()
  "   Return depth of last heading encountered by an outline maneuvering
   function.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."

  (- outline-recent-prefix-end outline-recent-prefix-beginning))

(defun outline-recent-prefix ()
  "   Like outline-recent-depth, but returns text of last encountered prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."
  (buffer-substring outline-recent-prefix-beginning outline-recent-prefix-end))

(defun outline-recent-bullet ()
  "   Like outline-recent-prefix, but returns bullet of last encountered
   prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth of the most recently matched topic."
  (buffer-substring (1- outline-recent-prefix-end) outline-recent-prefix-end))

;--------------------------------------------------------------------
;                   Fundamental Location Assement

(defun outline-on-current-heading-p ()
  "   Return prefix beginning point if point is on same line as current
   visible topic's header line."
  (save-excursion
    (beginning-of-line)
    (and (looking-at outline-regexp)
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning (match-beginning 0)))))

(defun outline-hidden-p ()
  "True if point is in hidden text."
  (interactive)
  (save-excursion
    (and (re-search-backward "[\C-j\C-m]" (point-min) t)
         (looking-at "\C-m"))))

(defun outline-current-depth ()
  "   Return the depth to which the current containing visible topic is
   nested in the outline."
  (save-excursion
    (if (outline-back-to-current-heading)
        (- outline-recent-prefix-end
           outline-recent-prefix-beginning)
      0)))

(defun outline-depth ()
  "   Like outline-current-depth, but respects hidden as well as visible
   topics."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-depth)
      (progn
        (setq outline-recent-prefix-end (point)
              outline-recent-prefix-beginning (point))
        0))))

;--------------------------------------------------------------------
;                 Fundamental Topic Prefix Assement

(defun outline-get-current-prefix ()
  "   Topic prefix of the current topic."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-prefix))))
(defun outline-get-bullet ()
  "   Return bullet of current topic."
  (save-excursion
    (and (outline-goto-prefix)
         (outline-recent-bullet))))

(defun outline-get-prefix-bullet (prefix)
  "   Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match outline-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))

;-----------------------------------------------------------------------
;                         Fundamental Motion

(defun outline-back-to-current-heading ()
  "   Move to heading line of current visible topic, or beginning of heading
   if already on visible heading line."
  (beginning-of-line)
  (or (outline-on-current-heading-p)
      (and (re-search-backward (concat "^\\(" outline-regexp "\\)") nil 'move)
           (setq outline-recent-prefix-end (match-end 1)
                 outline-recent-prefix-beginning (match-beginning 1)))))

(defun outline-goto-prefix ()
  "  Put point at beginning of outline prefix for current topic, visible
   or not.

   Returns a list of char address of the beginning of the prefix and the
   end of it, or nil if none."

  (cond ((and (or (bobp)
                  (memq (preceding-char) '(?\n ?\^M)))
              (looking-at outline-regexp))
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 0))))
        ((re-search-backward outline-line-boundary-regexp
                             ;; unbounded search,
                             ;; stay at limit and return nil if failed:
                             nil 1)
         (setq outline-recent-prefix-end (match-end 2)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 2)))))
 )

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.

   Returns that character position."

  (if (re-search-forward outline-line-boundary-regexp nil 'move)
      (progn (goto-char (match-beginning 0))
             (setq outline-recent-prefix-end (match-end 2)
                   outline-recent-prefix-beginning (match-beginning 2)))))


(defun outline-ascend-to-depth (depth)
  "   Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (outline-depth)))
      (let ((last-good (point)))
        (while (and (< depth (outline-depth))
                    (setq last-good (point))
                    (outline-beginning-of-level)
                    (outline-previous-heading)))
        (if (= (outline-recent-depth) depth)
            (progn (goto-char outline-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))))

(defun outline-descend-to-depth (depth)
  "   Descend to depth DEPTH within current topic, returning depth if
   successful, nil if not."
  (let ((start-point (point))
        (start-depth (outline-depth)))
    (while (and (> (outline-depth) 0)
                (not (= depth (outline-recent-depth)))	 ; ... not there yet
                (outline-next-heading)            ; ... go further
                (< start-depth (outline-recent-depth)))) ; ... still in topic
    (if (and (> (outline-depth) 0)
             (= (outline-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )

(defun outline-end-of-current-subtree ()
  (outline-back-to-current-heading)
  (let ((opoint (point))
	(level (outline-recent-depth)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (outline-recent-depth) level))
      (outline-next-heading))
    (if (not (eobp)) (forward-char -1))
    (if (memq (preceding-char) '(?\n ?\^M)) (forward-char -1))))

(defun outline-next-visible-heading (arg)
  "   Move to the next visible heading line.

   With argument, repeats or can move backward if negative.
   A heading line is one that starts with a `*' (or that outline-regexp
   matches)."
  (interactive "p")
  (if (< arg 0) (beginning-of-line) (end-of-line))
  (if (re-search-forward (concat "^\\(" outline-regexp "\\)") nil 'go arg)
      (progn (beginning-of-line)
             (setq outline-recent-prefix-end (match-end 1)
                   outline-recent-prefix-beginning (match-beginning 1)))))

(defun outline-previous-visible-heading (arg)
  "   Move to the previous heading line.

   With argument, repeats or can move forward if negative.
   A heading line is one that starts with a `*' (or that outline-regexp
   matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-next-heading (&optional backward)
  "   Move to the heading for the topic (possibly invisible) before this one.

   Optional arg BACKWARD means search for most recent prior heading.

   Returns the location of the heading, or nil if none found."

  (if backward (outline-goto-prefix)
    (if (bobp) (forward-char 1)))

  (if (if backward
          ;; searches are unbounded and return nil if failed:
          (re-search-backward outline-line-boundary-regexp
                              nil
                              0)
        (re-search-forward outline-line-boundary-regexp
                           nil
                           0))
      (progn;; Got some valid location state - set vars:
        (setq outline-recent-prefix-end (match-end 2))
        (goto-char (setq outline-recent-prefix-beginning
                         (match-beginning 2))))
    )
  )
(defun outline-previous-heading ()
  "   Move to the next (possibly invisible) heading line.

   Optional repeat-count arg means go that number of headings.

   Return the location of the beginning of the heading, or nil if not found."

  (outline-next-heading t))

(defun outline-next-sibling (&optional backward)
  "   Like outline-forward-current-level, but respects invisible topics.

   Go backward if optional arg BACKWARD is non-nil.

   Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (outline-depth))
          (start-point (point))
          last-good)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (outline-previous-heading)
                    (outline-next-heading))
                  (> (outline-recent-depth) start-depth)))
      (if (and (not (eobp))
               (and (> (outline-depth) 0)
                    (= (outline-recent-depth) start-depth)))
          outline-recent-prefix-beginning
        (goto-char start-point)
        nil)
      )
    )
  )
(defun outline-previous-sibling (&optional arg)
  "   Like outline-forward-current-level, but goes backwards and respects
   invisible topics.

   Optional repeat count means go number backward.

   Note that the beginning of a level is (currently) defined by this
   implementation to be the first of previous successor topics of
   equal or greater depth.

   Return depth if successful, nil otherwise."
  (outline-next-sibling t))

(defun outline-beginning-of-level ()
  "   Go back to the first sibling at this level, visible or not."
  (outline-end-of-level 'backward))
;klm transferred
(defun outline-end-of-level (&optional backward)
  "   Go to the last sibling at this level, visible or not."

  (while (outline-previous-sibling))
  (outline-recent-depth))

(defun outline-up-current-level (arg)
  "   Move to the heading line of which the present line is a subheading.
   With argument, move up ARG levels."
  (interactive "p")
  (outline-back-to-current-heading)
  (let ((present-level (outline-recent-depth)))
    ;; Loop for iterating arg:
    (while (and (> (outline-recent-depth) 1)
                (> arg 0)
                (not (bobp)))
      ;; Loop for going back over current or greater depth:
      (while (not (< (outline-recent-depth) present-level))
        (outline-previous-visible-heading 1))
      (setq present-level (outline-recent-depth))
      (setq arg (- arg 1)))
    )
  (if (> arg 0)
      (error "Can't ascend past level 1.")
    outline-recent-prefix-beginning)
  )

(defun outline-forward-current-level (arg &optional backward)
  "   Position the point at the next heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (outline-back-to-current-heading)
  (let ((amt (if arg (if (< arg 0)
                         ;; Negative arg - invert direction.
                         (progn (setq backward (not backward))
                                (abs arg))
                       arg)          ;; Positive arg - just use it.
               1)))              ;; No arg - use 1:
    (while (and (> amt 0)
                (outline-next-sibling backward))
      (setq amt (1- amt)))
    (if (> amt 0)
        (if (and arg (= (abs arg) amt))
          (error "This is the %s topic on this level."
                 (if backward "first" "last"))
          (error "There are no more topics on this level %s this"
                 (if backward "before" "after")))
      t)
    )
  )
(defun outline-backward-current-level (arg)
  "   Position the point at the previous heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (outline-forward-current-level arg t))

(defun outline-next-visible-sibling ()
  "   Position the point at the next heading of the same level, 
   and return that position, else nil if is not found."
  (let ((level (outline-current-depth)))
    (if (outline-next-visible-heading 1)
        (progn (while (and (> (outline-recent-depth) level) (not (eobp)))
                 (outline-next-visible-heading 1))
               (if (< (outline-recent-depth) level) nil (point))))))
(defun outline-previous-visible-sibling ()
  "   Position the point at the previous heading of the same level, 
   and return that position or nil if it cannot be found."
  (let ((level (outline-current-depth)))
    (if (outline-previous-visible-heading 1)
        (progn (while (and (> (outline-recent-depth) level) (not (bobp)))
                 (outline-previous-visible-heading 1))
               (if (< (outline-recent-depth) level) nil (point))))))

;-----------------------------------------------------------------------
;            Fundamental Outline Exposure Control Function

(defun outline-flag-region (from to flag)
  "   Hides or shows lines from FROM to TO, according to FLAG.
   Uses emacs selective-display, where text is show if FLAG put at
   beginning of line is `\\n' (newline character), while text is
   hidden if FLAG is `\\^M' (control-M)."
  (let ((modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
     (set-buffer-modified-p modp))))

;-----------------------------------------------------------------------
;                  Topic Component Exposure Control

(defun outline-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-current-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\^M)))

(defun outline-show-current-entry ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\n)))

; outline-show-entry basically for isearch dynamic exposure, as is...
(defun outline-show-entry ()
  "   Like outline-show-current-entry, but reveals an entry that is nested
   within hidden topics."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-next-preface) (point)) ?\n)))

; ... outline-hide-current-entry-completely also for isearch dynamic exposure:
(defun outline-hide-current-entry-completely ()
  "Like outline-hide-current-entry, but conceal topic completely."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-next-preface) (point)) ?\C-m)))

(defun outline-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (outline-hide-region-body (point-min) (point-max)))

;-----------------------------------------------------------------------
;                 Composite Topics Exposure Control

(defun outline-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(outline-flag-region (point)
                             (progn (outline-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\^M][\n\^M]")
		 2 1)))))))

(defun outline-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))

(defun outline-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (outline-show-current-children 1000))

(defun outline-flag-current-subtree (flag)
  (save-excursion
    (outline-back-to-current-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-current-subtree) (point))
			  flag)))

(defun outline-hide-current-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-current-subtree ?\^M))

(defun outline-hide-current-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-current-heading)
  (outline-hide-region-body (point) (progn (outline-end-of-current-subtree)
                                           (point))))

(defun outline-show-current-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-current-subtree ?\n))

(defun outline-show-current-children (&optional level)
  "  Show all direct subheadings of this heading.  Optional LEVEL specifies
   how many levels below the current level should be shown."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (progn (outline-back-to-current-heading)
                                (outline-recent-depth))))
    (narrow-to-region (point)
		      (progn (outline-end-of-current-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
                (outline-next-heading))
      (if (<= (outline-recent-depth) level)
	  (save-excursion
	   (let ((end (1+ (point))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (outline-flag-region (point) end ?\n))))))))

;-------------------------------------------------------------------
; Something added solely for use by a "smart menu" package someone got
; off the net.  I have no idea whether this is appropriate code.

(defun outline-to-entry-end (&optional include-sub-entries curr-entry-level)
  "   Go to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
   CURR-ENTRY-LEVEL is an integer representing the length of the current level
   string which matched to 'outline-regexp'.  If INCLUDE-SUB-ENTRIES is nil,
   CURR-ENTRY-LEVEL is not needed."
  (while (and (setq next-entry-exists
		    (re-search-forward outline-regexp nil t))
	      include-sub-entries
	      (save-excursion
		(beginning-of-line)
		(> (outline-depth) curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

;;;-------------------------------------------------------------------------
;;; Outline topic prefix and level adjustment funcs:

(defun outline-bullet-for-depth (&optional depth)
  "   Return outline topic bullet suited to DEPTH, or for current depth if none
   specified."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if (null depth) (setq depth (outline-visible-depth)))
  (if outline-stylish-prefixes
      (char-to-string (aref outline-plain-bullets-string
                            (% (max 0 (- depth 2))
                               outline-plain-bullets-string-len)))
    outline-primary-bullet)
  )


(defun outline-sibling-index (&optional depth)
  "   Item number of this prospective topic among it's siblings.

   If optional arg depth is greater than current depth, then we're
   opening a new level, and return 0.

   If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (outline-depth)))
           (let ((index 1))
             (while (outline-previous-sibling) (setq index (1+ index)))
             index))
          ((< depth (outline-recent-depth))
           (outline-ascend-to-depth depth)
           (outline-sibling-index))
          (0))))

(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "   Solicit (with first arg PROMPT) choice of a character from string STRING.

   Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for '?' character.  (Might oughta change minibuffer
      ;; keymap instead, oh well.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area t)) (read-char))))

      (if (null (string-match got string))
          (if (and do-defaulting (string= got "\^M"))
              ;; We're defaulting, return null string to indicate that:
              (setq got "")
            ;; Failed match and not defaulting,
            ;; set the prompt to give feedback,
            (setq new-prompt (concat prompt
                                     got
                                     " ...pick from: "
                                     string
                                     ""))
            ;; and set loop to try again:
            (setq got nil))
        ;; Got a match - give feedback:
        (message "")))
    ;; got something out of loop - return it:
    got)
  )

(defun outline-solicit-alternate-bullet (depth &optional current-bullet)

  "   Prompt for and return a bullet char as an alternative to the
   current one, but offer one suitable for current depth DEPTH
   as default."

  (let* ((default-bullet (or current-bullet
                             (outline-bullet-for-depth depth)))
	 (choice (solicit-char-in-string
                  (format "Choose a bullet from '%s' [%s]: "
                          outline-bullets-string
                          default-bullet)
                  outline-bullets-string
                  t)))
    (if (string= choice "") default-bullet choice))
  )

(defun outline-distinctive-bullet (bullet)
  "   True if bullet is one of those on outline-distinctive-bullets-string."
  (string-match (regexp-quote bullet) outline-distinctive-bullets-string))

(defun outline-numbered-type-prefix (&optional prefix)
  "   True if current header prefix bullet is numbered bullet."
  (and outline-numbered-bullet
        (string= outline-numbered-bullet
                 (if prefix
                     (outline-get-prefix-bullet prefix)
                   (outline-get-bullet)))))

(defun outline-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            solicit
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Solicit dominates specified bullet-char.
  "   Generate a topic prefix suitable for optional arg DEPTH, or current
   depth if not specified.

   All the arguments are optional.

   PRIOR-BULLET indicates the bullet of the prefix being changed, or
   nil if none.  This bullet may be preserved (other options
   notwithstanding) if it is on the outline-distinctive-bullets-string,
   for instance.

   Second arg NEW indicates that a new topic is being opened after the
   topic at point, if non-nil.  Default bullet for new topics, eg, may
   be set (contingent to other args) to numbered bullets if previous
   sibling is one.  The implication otherwise is that the current topic
   is being adjusted - shifted or rebulleted - and we don't consider
   bullet or previous sibling.

   Third arg DEPTH forces the topic prefix to that depth, regardless of
   the current topics' depth.

   Fourth arg SOLICIT non-nil provokes solicitation from the user of a
   choice among the valid bullets.  (This overrides other all the
   options, including, eg, a distinctive PRIOR-BULLET.)

   Fifth arg, NUMBER-CONTROL, matters only if 'outline-numbered-bullet'
   is non-nil *and* soliciting was not explicitly invoked.  Then
   NUMBER-CONTROL non-nil forces prefix to either numbered or
   denumbered format, depending on the value of the sixth arg, INDEX.

   (Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

   If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
   the prefix of the topic is forced to be numbered.  Non-nil
   NUMBER-CONTROL and nil INDEX forces non-numbered format on the
   bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
   that the index for the numbered prefix will be derived, by counting
   siblings back to start of level.  If INDEX is a number, then that
   number is used as the index for the numbered prefix (allowing, eg,
   sequential renumbering to not requre this function counting back the
   index for each successive sibling)."

  ;; The options are ordered in likely frequence of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (outline-depth)))
         (lead-char ".")
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq lead-char "") outline-primary-bullet)
                                        ; Simple, too: all asterisks:
           (outline-old-style-prefixes
            ;; Cheat - make body the whole thing, null out lead-char and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char outline-primary-bullet)))
            (setq lead-char "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (outline-solicit-alternate-bullet depth)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and outline-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got outline-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and outline-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                outline-numbered-bullet
              (if (and current-bullet
                       (not (string= outline-numbered-bullet
                                     current-bullet)))
                  current-bullet
                (outline-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (outline-depth))
                 outline-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (outline-depth))
                              (outline-ascend-to-depth depth))
                          (outline-get-bullet))))
                   (if (and sibling-bullet
                            (string= outline-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (outline-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and outline-numbered-bullet
                               (string= prior-bullet outline-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((outline-bullet-for-depth depth)))))

    (concat lead-char
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (outline-sibling-index depth)))
                                   ((outline-sibling-index))))))
    )
  )

(defun outline-reindent-body (old-depth new-depth)
  "  Reindent body lines which were indented at old-depth to new-depth.

  Note that refill of indented paragraphs is not done, and tabs are
  not accomodated.  ('untabify' your outline if you want to preserve
  hanging body indents.)"

  (save-excursion
    (save-restriction
      (outline-goto-prefix)
      (forward-char 1)
      (let* ((old-spaces-expr (make-string (1+ old-depth) ?\ ))
             (new-spaces-expr (concat (make-string (1+ new-depth) ?\ )
                                      ;; spaces followed by non-space:
                                      "\\1")))
        (while (and (re-search-forward "[\C-j\C-m]" nil t)
                    (not (looking-at outline-regexp)))
          (if (looking-at old-spaces-expr)
              (replace-match new-spaces-expr)))))))

(defun outline-rebullet-current-heading (arg)
  "   Like non-interactive version 'outline-rebullet-heading', but work on
   (only) visible heading containing point.

   With repeat count, solicit for bullet."
  (interactive "P")
  (save-excursion (outline-back-to-current-heading)
                  (outline-rebullet-heading (not arg)	;;; solicit
                                            nil		;;; depth
                                            nil		;;; number-control
                                            nil		;;; index
                                            t)		;;; do-successors
                  )
  )
(defun outline-rebullet-heading (&optional solicit
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "   Adjust bullet of current topic prefix.

   All args are optional.

   If SOLICIT is non-nil then the choice of bullet is solicited from
   user.  Otherwise the distinctiveness of the bullet or the topic
   depth determines it.

   Second arg DEPTH forces the topic prefix to that depth, regardless
   of the topic's current depth.

   Third arg NUMBER-CONTROL can force the prefix to or away from
   numbered form.  It has effect only if 'outline-numbered-bullet' is
   non-nil and soliciting was not explicitly invoked (via first arg).
   Its effect, numbering or denumbering, then depends on the setting
   of the forth arg, INDEX.

   If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
   prefix of the topic is forced to be non-numbered.  Null index and
   non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
   non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
   INDEX is a number, then that number is used for the numbered
   prefix.  Non-nil and non-number means that the index for the
   numbered prefix will be derived by outline-make-topic-prefix.

   Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
   siblings.

   Cf vars 'outline-stylish-prefixes', 'outline-old-style-prefixes',
   and 'outline-numbered-bullet', which all affect the behavior of
   this function."

  (let* ((current-depth (outline-depth))
         (new-depth (or new-depth current-depth))
         (mb outline-recent-prefix-beginning)
         (me outline-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outline-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                solicit
                                                number-control
                                                index)))

    ;; Don't need to reinsert identical one:
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
        t

      ;; New prefix probably different from old:
      ;; get rid of old one:
      (delete-region mb me)
      (goto-char mb)
      ;; Dispense with number if numbered-bullet prefix:
      (if (and outline-numbered-bullet
               (string= outline-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
          (delete-region (match-beginning 0)(match-end 0)))

      ;; Put in new prefix:
      (insert-string new-prefix)
      )

    ;; Reindent the body if elected and depth changed:
    (if (and outline-reindent-bodies
             (not (= new-depth current-depth)))
        (outline-reindent-body current-depth new-depth))

    ;; Recursively rectify successive siblings if selected:
    (if do-successors
        (save-excursion
          (while (outline-next-sibling)
            (setq index
                  (cond ((numberp index) (1+ index))
                        ((not number-control)  (outline-sibling-index))))
            (if (outline-numbered-type-prefix)
                (outline-rebullet-heading nil		;;; solicit
                                          new-depth	;;; new-depth
                                          number-control;;; number-control
                                          index		;;; index
                                          nil)))))	;;;(dont!)do-successors
      )
  )

(defun outline-rebullet-topic (arg)
  "   Like outline-rebullet-topic-grunt, but start from visible one at point.
   Descends into invisible as well as visible topics, however.

   With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column))
        (was-eol (eolp)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (outline-back-to-current-heading)
      (if (<= (+ (outline-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (outline-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (+ start-col arg)))
  )
(defun outline-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)

  "   Rebullet the topic at point, visible or invisible, and all
   contained subtopics.  See outline-rebullet-heading for rebulleting
   behavior.

   All arguments are optional.

   First arg RELATIVE-DEPTH means to shift the depth of the entire
   topic that amount.

   The rest of the args are for internal recursive use by the function
   itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (outline-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (outline-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1."))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (outline-rebullet-heading nil		;;; solicit
                                     (+ starting-depth	;;; starting-depth
                                        relative-depth)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (outline-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (outline-recent-depth)))
             (setq index (1+ index))
             (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (outline-recent-depth) starting-depth)
                           (= (outline-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (outline-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (outline-rebullet-heading nil nil nil nil t)))))
    )
  )
(defun outline-number-siblings (&optional denumber)
  "   Assign numbered topic prefix to this topic and its siblings.

   With universal argument, denumber - assign default bullet to this
   topic and its siblings.

   With repeated universal argument (`^U^U'), solicit bullet for each
   rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (outline-back-to-current-heading)
    (outline-beginning-of-level)
    (let ((index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (outline-rebullet-heading use-bullet		;;; solicit
                                  nil			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (outline-next-sibling)))
      )
    )
  )


(defun outline-shift-in (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic arg))
(defun outline-shift-out (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic (* arg -1)))

;;;-------------------------------------------------------------------------
;;; Outline topic creation forms:

(defun open-topic (depth &optional open-prior get-alternate-bullet)
  "   Open new topic header at DEPTH, after the end of the current topic
   at that depth (including any subtopics).  The new topic gets a leading
   blank line if the initial topic at the depth has one before it.

   If optional arg PRIOR is non-nil, open just prior to the current
   topic at specified depth.

   Non-nil optional third argument, GET-ALTERNATE-BULLET, causes
   solicitation from the user of the choice of bullet.

   The first line of a topic is distinguished by a topic prefix at the
   beginning of the line.  A normal topic prefix consists of either
   the first character on 'outline-bullets-string' (typically an
   asterisk - '*'), or else a period ('.') followed by zero or more
   spaces and then one of the characters in 'outline-bullets-string'.
   Thus the topic depth for a given prefix is equal to the number of
   spaces + 2 (for the asterisk and period).

   The form and content of the topic prefix produced depends on a few
   things:

   - The top level prefix is always an asterisk ('*')
   - 'outline-old-style-prefixes' setting - if nil (default) then
     non-top-level prefixes are 'space-padded', nil means all
     asterisks.
   - Space padded prefixes for level n start with a dot ('.'), followed
     by n-2 spaces, followed by a bullet from among the characters on
     'outline-bullets-string'.  The choice of bullets for space padded
     prefixes depends on some additional factors:

     - With an exception (see next item), the default bullet produced
       depends on the value of 'outline-stylish-prefixes' - if nil
       then an asterisk is used.  If t (default) then the bullet is
       selected from 'outline-plain-bullets -string' modulo the topic
       depth.
     - If 'outline-numbered-bullets' has a string value, then any
       topic which uses that bullet is numbered by open-topic, it is
       preserved through adjustment by 'outline-rebullet-heading' and
       'outline-rebullet-topic', and topics opened immediately after
       and at the same level of a numbered heading are created as
       numbered headings.

   All supported topic prefix styles are respected by the maneuvering
   functions regardless of the configuration variable settings.

   The '...-old-style-...' setting provides complete backwards
   compatability with the original outline-mode, permitting use of the
   new topic minting, reforming, and depth-adjusting functions while
   maintaining consistency with the old format."

  (let ((open-on-blank (if (looking-at "^$") (point)))
        (this-depth (outline-depth)))
    (if (> this-depth depth)
        (outline-ascend-to-depth depth)
      (outline-back-to-current-heading))
    (let* ((index (save-excursion
                    (if open-on-blank (goto-char open-on-blank))
                    (+ (outline-sibling-index depth) (if open-prior 0 1))))
           (new-prefix
            (concat (outline-make-topic-prefix nil ;prior-bullet
                                               t ;new
                                               depth ;depth
                                               get-alternate-bullet ;solicit
                                               nil ;number-control
                                               index) ;index
                    " ")))

      ;; We're at topic header, position cursor for opening:
      (cond (open-prior (if (bobp) (open-line 1) (forward-char -1)))
            (open-on-blank (goto-char open-on-blank)) ;; open on this line.
            ;; open just after any topics at depth greater than requested:
            (t (if (> depth (outline-depth))
                   ;; Embedded new topics are snugged up to subtree:
                   (progn (outline-next-preface) (end-of-line))
                 ;; ... while sibling and greater get whitespace
                 ;; before if there's already some there:
                 (outline-end-of-current-subtree)
                 (end-of-line))))

      (cond (open-on-blank)
            ((< this-depth depth)
             (if (bolp) (open-line 1) (newline 1)))
            ;; If current topic at depth has a blank line before and
            ;; after it, provide blank lines around the new one, too.
            ;; Next two cases for this.
            ((and open-prior
                  (save-excursion
                    (beginning-of-line)
                    (if (not (bobp)) (outline-ascend-to-depth depth))
                    (looking-at "[\C-j ]*$")))
             (newline 1)(open-line 1))
            ;; Blank lines around current topic, make blanks around new one:
            ((save-excursion
               (beginning-of-line)
               (and (outline-ascend-to-depth depth)
                    (or (bobp)
                        (and (forward-char -1)
                             (re-search-backward "^" nil t)
                             (looking-at "[\C-j ]*$")))
                    (looking-at "\C-j[\C-i ]*$")))
             (newline 2))
            ((newline 1)))

      ;; Put the new topic header prefix in place:
      (insert-string new-prefix)

      ;; Show the new entry, to give quick feedback even if we're
      ;; really still working on the subsequent renumbering:
      ;; (bet peoply mostly don't notice the lag once they get the response!)
      (sit-for 0)

      ;; And renumber the new topic, and successive siblings if any,
      ;; if the new topic was at a different level than topic from
      ;; which we opened:
      (save-excursion
        (if (or (> this-depth depth)
                ;; Not going outwards, don't have to redo current one:
                (outline-next-sibling))
            (outline-rebullet-heading nil		;;; solicit
                                      depth 		;;; depth
                                      nil 		;;; number-control
                                      nil		;;; index
                                      t))		;;; do-successors
        )
      )
    )
  )

(defun open-subtopic (arg)
  "   Open new topic header at deeper level than the current one.

  Negative universal arg means to open deeper, but place the new topic
  prior to the current one."
  (interactive "p")
  (open-topic (+ (outline-current-depth) 1)
               (> 0 arg)
               (not (= (if (< arg 0) (- arg) arg) 1))))
(defun open-sibtopic (arg)
  "   Open new topic header at same level as the current one.  Negative
  universal arg means to place the new topic prior to the current
  one."
  (interactive "p")
  (open-topic (outline-current-depth)
              (> 0 arg)
              (not (= (if (< arg 0) (- arg) arg) 1))))
(defun open-supertopic (arg)
  "   Open new topic header at shallower level than the current one.
  Negative universal arg means to open shallower, but place the new
  topic prior to the current one."

  (interactive "p")
  (open-topic (- (outline-current-depth) 1)
              (> 0 arg)
              (not (= (if (< arg 0) (- arg) arg) 1))))

;;;-------------------------------------------------------------------------
;;; Surgery (kill-ring) functions with special provisions for outlines:

(defun outline-kill-line (&optional arg)
  "   Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")
  (if (not (and outline-numbered-bullet (bolp) (looking-at outline-regexp)))
      (kill-line arg)
    (let* ((depth (outline-depth))
           (ascender depth))
      (kill-line arg)
      (sit-for 0)
      (if (> (outline-depth) depth)
          ;; An intervening parent was removed from after a subtree:
          (setq depth (outline-recent-depth)))
      (save-excursion
        (while (and (> (outline-depth) 0)
                    (> (outline-recent-depth) ascender)
                    (outline-ascend-to-depth (setq ascender (1- ascender)))))
            ;; Have to try going forward until we find another at
            ;; desired depth:
        (if (and outline-numbered-bullet
                 (outline-descend-to-depth depth))
            (outline-rebullet-heading nil		;;; solicit
                                      depth		;;; depth
                                      nil 		;;; number-control
                                      nil		;;; index
                                      t)		;;; do-successors
          )
        )
      )
    )
  )
(defun outline-kill-topic ()
  "   Kill topic together with subtopics."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is disposed of, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* the kill has been performed.

  (interactive)
  (let* ((beg (outline-back-to-current-heading))
         (depth (outline-recent-depth)))
    (outline-end-of-current-subtree)
    (if (not (eobp))
        (forward-char 1))
    (kill-region beg (point))
    (sit-for 0)
    (save-excursion
      (if (and outline-numbered-bullet
               (outline-descend-to-depth depth))
          (outline-rebullet-heading nil		;;; solicit
                                    depth	;;; depth
                                    nil		;;; number-control
                                    nil		;;; index
                                    t)		;;; do-successors
        )
      )
    )
  )

(defun outline-yank (&optional arg)
  "   Like regular yank, except does depth adjustment of yanked topics, when:

   1 the stuff being yanked starts with a valid outline header prefix, and
   2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

   If these two conditions hold then the depth of the yanked topics
   are all adjusted the amount it takes to make the first one at the
   depth of the header into which it's being yanked.

   The point is left in from of yanked, adjusted topics, rather than
   at the end (and vice-versa with the mark).  Non-adjusted yanks,
   however, (ones that don't qualify for adjustment) are handled
   exactly like normal yanks.

   Outline-yank-pop is used with outline-yank just as normal yank-pop
   is used with normal yank in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (let ((beginning (point))
        established-depth)      ; Depth of the prefix into which we're yanking.
    ;; Get current depth and numbering ... Oops, not doing anything
    ;; with the number just yet...
    (if (and (eolp)
             (save-excursion (beginning-of-line)
                             (looking-at outline-regexp)))
        (setq established-depth (- (match-end 0) (match-beginning 0))))
    (yank arg)
    (exchange-dot-and-mark)
    (if (and established-depth          ; the established stuff qualifies.
             ;; The yanked stuff also qualfies - is topic(s):
             (looking-at (concat "\\(" outline-regexp "\\)")))
        ;; Ok, adjust the depth of the yanked stuff.  Note that the
        ;; stuff may have more than a single root, so we have to
        ;; iterate over all the top level ones yanked, and do them in
        ;; such a way that the adjustment of one new one won't affect
        ;; any of the other new ones.  We use the focus of the
        ;; narrowed region to successively exclude processed siblings.
        (let* ((yanked-beg (match-beginning 1))
               (yanked-end (match-end 1))
               (yanked-depth (- yanked-end yanked-beg))
               (depth-diff (- established-depth yanked-depth))
               done
               (more t))
          (save-excursion
            (save-restriction
              (narrow-to-region yanked-beg (mark))
              ;; First trim off excessive blank line at end, if any:
              (goto-char (point-max))
              (if (looking-at "^$") (delete-char -1))
              (goto-char (point-min))
              ;; Work backwards, with each shallowest level,
              ;; successively excluding the last processed topic
              ;; from the narrow region:
              (goto-char (point-max))
              (while more
                (outline-back-to-current-heading)
                ;; go as high as we can in each bunch:
                (while (outline-ascend-to-depth
                        (1- (outline-depth))))
                (save-excursion
                  (outline-rebullet-topic-grunt depth-diff
                                                (outline-depth)
                                                (point)))
                (if (setq more (not (bobp)))
                    (progn (widen)
                           (forward-char -1)
                           (narrow-to-region yanked-beg (point)))))))
          ;; Now dispose of old prefix...
          (delete-region yanked-beg
                         (+ yanked-beg established-depth))
          ;; and extraneous digits and a space:
          (while (looking-at "[0-9]") (delete-char 1))
          (if (looking-at " ") (delete-char 1))
          )
      (exchange-dot-and-mark))
    (if outline-numbered-bullet
        (progn
          ;; Renumber, in case necessary:
          (sit-for 0)
          (save-excursion
            (goto-char beginning)
            (if (outline-goto-prefix)
                (outline-rebullet-heading nil		;;; solicit
                                          (outline-depth) ;;; depth
                                          nil		;;; number-control
                                          nil		;;; index
                                          t)		;;; do-successors
                  )
            )
          )
      )
    )
  )
(defun outline-yank-pop (&optional arg)
  "   Just like yank-pop, but works like outline-yank when popping
  topics just after fresh outline prefixes.  Adapts level of popped
  stuff to level of fresh prefix."

  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (delete-region (point) (mark))
  (rotate-yank-pointer arg)
  (outline-yank)
  )

;;;-------------------------------------------------------------------------
;;; isearch wrappers for special outline provisions:

(defvar outline-search-reconceal nil
  "Used for outline isearch provisions, to track whether current search
match was concealed outside of search.  The value is the location of the
match, if it was concealed, regular if the entire topic was concealed, in
a list if the entry was concealed.")
(defun outline-enwrap-isearch ()
  "   Impose isearch-mode wrappers so isearch progressively exposes and
   reconceals hidden topics when working in outline mode, but works
   elsewhere.

   The function checks to ensure that the rebindings are done only once"
  ;; Make sure isearch-mode is loaded,
  (if (or (not outline-enwrap-isearch-mode)
          (fboundp 'real-isearch-quit))
      nil
    (if (not (and (fboundp 'isearch-mode)
                  (fboundp 'isearch-quit)))
        (load-library "isearch-mode.el"))
    ;; stash the crux-point functions so they're in known places, then
    ;; register the wrapper functions under their old names, instead:
    (fset 'real-isearch-quit (symbol-function 'isearch-quit))
    (fset 'isearch-quit 'isearch-quit/outline-provisions)
    (fset 'real-isearch-done (symbol-function 'isearch-done))
    (fset 'isearch-done 'isearch-done/outline-provisions)
    (fset 'real-isearch-update (symbol-function 'isearch-update))
    (fset 'isearch-update 'isearch-update/outline-provisions)
    (make-local-variable 'outline-search-reconceal))
  )
(defun outline-isearch-arrival-business ()
  "   Do outline business like exposing current point, if necessary,
   registering reconceal state accordingly."
  (setq outline-search-reconceal
        (if (outline-hidden-p)
            ;; set to just point if the entire topic is hidden, or is
            ;; supposed to be hidden (according to already pending
            ;; setting of outline-search-reconceal), or point in a list
            ;; if just part of the entry is hidden:
            (save-excursion (outline-goto-prefix)
                            (prog1 (if (outline-hidden-p)
                                        (point)
                                      (list (point)))
                              (outline-show-entry)))
            )
    )
  )
(defun outline-isearch-advancing-business ()
  "   Do outline business like deexposing current point, if necessary,
   according to reconceal state registration."
  (if outline-search-reconceal
      (save-excursion
        (if (listp outline-search-reconceal)
            ;; Leave the topic visible:
            (progn (goto-char (car outline-search-reconceal))
                   (outline-hide-current-entry))
          ;; Rehide the entire topic:
          (goto-char outline-search-reconceal)
          (outline-hide-current-entry-completely))))
  )

(defun isearch-quit/outline-provisions ()
  (interactive)
  (if (and outline-enwrap-isearch-mode
           (string= mode-name "Outline"))
      (outline-isearch-advancing-business))
  (real-isearch-quit))
(defun isearch-done/outline-provisions ()
  (interactive)
  (if (and outline-enwrap-isearch-mode
           (string= mode-name "Outline"))
      (save-excursion
        (if (and outline-search-reconceal
                 (not (listp outline-search-reconceal)))
            ;; The topic was concealed - reveal it, its siblings,
            ;; and any ancestors that are still concealed:
            (progn (message "(exposing destination)")(sit-for 0)
                   (outline-ascend-to-depth (1- (outline-depth)))
                   ;; Ensure that the target topic's ancestors are exposed
                   (while (outline-hidden-p) (outline-show-current-children))
                   ;; Ensure target topic's siblings are exposed:
                   (outline-show-current-children)))
        (outline-isearch-arrival-business)))
  (real-isearch-done)
  )
(defun isearch-update/outline-provisions ()
  "    Wrapper around isearch which exposes and conceals hidden outline
   portions encountered in the course of searching."
  (if (not (and outline-enwrap-isearch-mode
                (string= mode-name "Outline")))
      ;; Just do the plain business:
      (real-isearch-update)

    ;; Ah - provide for outline conditions:
    (outline-isearch-advancing-business)
    (real-isearch-update)
    (cond (isearch-success (outline-isearch-arrival-business))
          ((not isearch-success) (outline-isearch-advancing-business)))
    )
  )



;;;-------------------------------------------------------------------------
;;; Scattered and Sundries

(defun outline-copy-exposed (&optional workbuf)
  "   Duplicate buffer to other buffer, sans hidden stuff.

   Without repeat count, this simple-minded function just generates
   the new buffer by concatenating the current buffer name with \"
   exposed\", and doing a 'get-buffer' on it."

  (interactive)
  (if (not workbuf) (setq workbuf (concat (buffer-name) " exposed")))
  (let ((buf (current-buffer)))
    (if (not (get-buffer workbuf))
	(generate-new-buffer workbuf))
    (pop-to-buffer workbuf)
    (erase-buffer)
    (insert-buffer buf)
    (replace-regexp "\^M[^\^M\^J]*" "")
    (goto-char (point-min))
    )
  )

(defun outlineify-sticky ()
  "   Activate outline mode and establish an -*-outline-*- explicit mode
   trigger in buffer."
  (interactive)
  (outline-mode)
  (save-excursion
    (goto-char 0)
    (if (not (looking-at ".*-\*-outline-*-"))
	(insert "* -*-outline-*-\n" ))))
                                        
;; Change history
;;; Dec 91	V 1.1 released to usenet
;;; later in dec, 91 eliminated reference to 'cl.el' macro, 'case'
;;;		     (thanks to XXX for pointing out the dependency).
;;;  8-Jan-1992 Eliminated reference to 'cl.el' func, 'abs' (thanks to
;;;		jmm@king.econ.lsa.umich.edu for pointing out this dependency).
;;;  8-Jan-1992 Put in "?\(" instead of "40" (thanks to liberte@cs.uiuc.edu
;;;		for filling me in on this way to accomodate emacs lisp syntax).
;;;  9-Jan-1992 V1.2
;;;  9-Mar-1992 V1.3
;;; 11-Apr-1992 V2.0 'allout.el'
;;; 11-Apr-1992 V2.1 Several bug fixes.  Released to LCD.
