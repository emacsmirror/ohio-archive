;; todo-mode.el -- Major mode for editing TODO list files

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Oliver.Seidel@cl.cam.ac.uk (was valid on Aug 2, 1997)
;; Created: 2 Aug 1997
;; Version: $Id: todo-mode.el,v 1.34 1998/01/12 11:43:22 os10000 Exp $
;; Keywords: Categorised TODO list editor, todo-mode

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ---------------------------------------------------------------------------

;;; Commentary:

;;  Mode Description
;;
;;	TODO is a major mode for EMACS which offers functionality to
;;	treat most lines in one buffer as a list of items one has to
;;	do.  There are facilities to add new items, which are
;;	categorised, to edit or even delete items from the buffer.
;;	The buffer contents are currently compatible with the diary,
;;	so that the list of todo-items will show up in the FANCY diary
;;	mode.
;;
;;	Notice: Besides the major mode, this file also exports the
;;	function `todo-show' which will change to the one specific
;;	TODO file that has been specified in the todo-file-do
;;	variable.  If this file does not conform to the TODO mode
;;	conventions, the todo-show function will add the appropriate
;;	header and footer.  I don't anticipate this to cause much
;;	grief, but be warned, in case you attempt to read a plain text
;;	file.
;;
;;  Preface, Quickstart Installation
;;
;;      To get this to work, make emacs execute the line
;;
;;          (autoload 'todo-mode "todo-mode"
;;                    "Major mode for editing TODO lists." t)
;;          (autoload 'todo-show "todo-mode"
;;                    "Show TODO items." t)
;;          (autoload 'todo-insert-item "todo-mode"
;;                    "Add TODO item." t)
;;
;;      You may now enter new items by typing "M-x todo-insert-item",
;;      or enter your TODO list file by typing "M-x todo-show".
;;
;;      The TODO list file has a special format and some auxiliary
;;      information, which will be added by the todo-show function if
;;      it attempts to visit an un-initialised file.  Hence it is
;;      recommended to use the todo-show function for the first time,
;;      in order to initialise the file, but it is not necessary
;;      afterwards.
;;
;;      As these commands are quite long to type, I would recommend
;;      the addition of two bindings to your to your global keymap.  I
;;      personally have the following in my initialisation file:
;;
;;          (global-set-key "\C-ct" 'todo-show) ;; switch to TODO buffer
;;	    (global-set-key "\C-ci" 'todo-insert-item) ;; insert new item
;;
;;      Note, however, that this recommendation has prompted some
;;      criticism, since the keys C-c LETTER are reserved for user
;;      functions.  I believe my recommendation is acceptable, since
;;      the Emacs Lisp Manual *Tips* section also details that the
;;      mode itself should not bind any functions to those keys.  The
;;      express aim of the above two bindings is to work outside the
;;      mode, which doesn't need the show function and offers a
;;      different binding for the insert function.  They serve as
;;      shortcuts and are not even needed (since the TODO mode will be
;;      entered by visiting the TODO file, and later by switching to
;;      its buffer).
;;
;;      If you are an advanced user of this package, please consult
;;      the whole source code for autoloads, because there are several
;;      extensions that are not explicitly listed in the above quick
;;      installation.
;;
;;  Version
;;
;;      Which version of todo-mode.el does this documentation refer to?
;;
;;      $Id: todo-mode.el,v 1.34 1998/01/12 11:43:22 os10000 Exp $
;;
;;  Pre-Requisites
;;
;;      This package will require the following packages to be
;;      available on the load-path:
;;
;;          time-stamp
;;          easymenu
;;
;;  Operation
;;
;;	You will have the following facilities available:
;;
;;	    M-x todo-show   will enter the todo list screen, here type
;;
;;	    +  to go to next category
;;          -  to go to previous category
;;          d  to file the current entry, including a
;;            			    comment and timestamp
;;          e  to edit the current entry
;;          E  to edit a multi-line entry
;;          f  to file the current entry, including a
;;            			    comment and timestamp
;;          i  to insert a new entry, with prefix, omit category
;;          I  to insert a new entry at current cursor position
;;	    j  jump to category
;;          k  to kill the current entry
;;          l  to lower the current entry's priority
;;          n  for the next entry
;;          p  for the previous entry
;;	    P  print
;;          q  to save the list and exit the buffer
;;          r  to raise the current entry's priority
;;          s  to save the list
;;          S  to save the list of top priorities
;;	    t  show top priority items for each category
;;
;;	When you add a new entry, you are asked for the text and then
;;	for the category.  I for example have categories for things
;;	that I want to do in the office (like mail my mum), that I
;;	want to do in town (like buy cornflakes) and things I want to
;;	do at home (move my suitcases).  The categories can be
;;	selected with the cursor keys and if you type in the name of a
;;	category which didn't exist before, an empty category of the
;;	desired name will be added and filled with the new entry.
;;
;;  Configuration
;;
;;  Variable todo-prefix
;;
;;	I would like to recommend that you use the prefix "*/*" (by
;;	leaving the variable 'todo-prefix' untouched) so that the
;;	diary displays each entry every day.
;;
;;	To understand what I mean, please read the documentation that
;;	goes with the calendar since that will tell you how you can
;;	set up the fancy diary display and use the #include command to
;;	include your todo list file as part of your diary.
;;
;;	If you have the diary package set up to usually display more
;;	than one day's entries at once, consider using
;;
;;	    "&%%(equal (calendar-current-date) date)"
;;
;;	as the value of `todo-prefix'.  Please note that this may slow
;;	down the processing of your diary file some.
;;
;;      Carsten Dominik <dominik@strw.LeidenUniv.nl> suggested that
;;
;;          "&%%(todo-cp)"
;;
;;      might be nicer and to that effect a function has been declared
;;      further down in the code.  You may wish to auto-load this.
;;
;;      Carsten also writes that that *changing* the prefix after the
;;      todo list is already established is not as simple as changing
;;      the variable - the todo files have to be changed by hand.
;;
;;  Variable todo-file-do
;;
;;	This variable is fairly self-explanatory.  You have to store
;;	your TODO list somewhere.  This variable tells the package
;;	where to go and find this file.
;;
;;  Variable todo-file-done
;;
;;	Even when you're done, you may wish to retain the entries.
;;	Given that they're timestamped and you are offered to add a
;;	comment, this can make a useful diary of past events.  It will
;;	even blend in with the EMACS diary package.  So anyway, this
;;	variable holds the name of the file for the filed todo-items.
;;
;;  Variable todo-file-top
;;
;;      File storing the top priorities of your TODO list when
;;      todo-save-top-priorities is non-nil.  Nice to include in your
;;      diary instead of the complete TODO list.
;;
;;  Variable todo-mode-hook
;;
;;	Just like other modes, too, this mode offers to call your
;;	functions before it goes about its business.  This variable
;;	will be inspected for any functions you may wish to have
;;	called once the other TODO mode preparations have been
;;	completed.
;;
;;  Variable todo-insert-threshold
;;
;;     	Another nifty feature is the insertion accuracy.  If you have
;;     	8 items in your TODO list, then you may get asked 4 questions
;;     	by the binary insertion algorithm.  However, you may not
;;     	really have a need for such accurate priorities amongst your
;;     	TODO items.  If you now think about the binary insertion
;;     	halfing the size of the window each time, then the threshhold
;;     	is the window size at which it will stop.  If you set the
;;     	threshhold to zero, the upper and lower bound will coincide at
;;     	the end of the loop and you will insert your item just before
;;     	that point.  If you set the threshhold to, e.g. 8, it will stop
;;     	as soon as the window size drops below that amount and will
;;     	insert the item in the approximate centre of that window.  I
;;     	got the idea for this feature after reading a very helpful
;;     	e-mail reply from Trey Jackson <trey@cs.berkeley.edu> who
;;     	corrected some of my awful coding and pointed me towards some
;;     	good reading.  Thanks Trey!
;;
;;  Things to do
;;
;;      These originally were my ideas, but now also include all the
;;      suggestions that I included before forgetting them:
;;
;;      o   Fancy fonts for todo/top-priority buffer
;;      o   Remove todo-prefix option in todo-top-priorities
;;      o   Rename category
;;      o   Move entry from one category to another one
;;      o   Entries which both have the generic */* prefix and a
;;          "deadline" entry which are understood by diary, indicating
;;          an event (unless marked by &)
;;      o   The optional COUNT variable of todo-forward-item should be
;;          applied to the other functions performing similar tasks
;;      o   Modularization could be done for repeaded elements of
;;          the code, like the completing-read lines of code.  
;;	o   license / version function
;;	o   export to diary file
;;	o   todo-report-bug
;;	o   GNATS support
;;	o   elide multiline (as in bbdb, or, to a lesser degree, in
;;          outline mode)
;;	o   rewrite complete package to store data as lisp objects
;;          and have display modes for display, for diary export,
;;          etc. (Richard Stallman pointed out this is a bad idea)
;;      o   so base todo-mode.el on generic-mode.el instead
;;
;;  History and Gossip
;;
;;	Many thanks to all the ones who have contributed to the
;;	evolution of this package!  I hope I have listed all of you
;;	somewhere in the documentation or at least in the RCS history!
;;
;;	Enjoy this package and express your gratitude by sending nice
;;	things to my parents' address!
;;
;;	Oliver Seidel
;;	(Lessingstr. 8, 65760 Eschborn, Federal Republic of Germany)
;;

;; ---------------------------------------------------------------------------

;;; Change Log:

;; $Log: todo-mode.el,v $
;; Revision 1.35  2000/07/22 11:31:21  walters
;; Added small font lock capabilities, ability to use C-c C-c to exit
;; from a multiline edit.
;;
;; Revision 1.34  1998/01/12 11:43:22  os10000
;; Added patch from Don Hejna <djhejna@oasis.ambit.com>.
;;
;; Revision 1.33  1997/12/04 17:45:22  os10000
;; Another patch by Michael Cook to fix annotation.
;;
;; Revision 1.32  1997/12/03  12:18:20  os10000
;; Added category patch by Michael R Cook <mcook@cognex.com>.
;;
;; Revision 1.31  1997/10/28  22:16:24  os10000
;; Three insertion options:
;; i without prefix: ask for category, do binary insertion
;; i with prefix: do binary insertion in current category
;; uppercase I: insert directly under cursor
;;
;; Revision 1.30  1997/10/28 21:59:48  os10000
;; Improved documentation, fixed insertion with prefix.
;;
;; Revision 1.29  1997/10/28 21:47:12  os10000
;; Implemented "insert-under-cursor" as suggested by
;; Kai Grossjohann <grossjohann@ls6.cs.uni-dortmund.de>.
;;
;; Revision 1.28  1997/10/28 21:37:05  os10000
;; Incorporated simplifying suggestions from
;; Carsten Dominik <dominik@strw.LeidenUniv.nl>.
;;
;; Revision 1.27  1997/10/28 21:26:55  os10000
;; Patch from Paul Stodghill <stodghil@CS.Cornell.EDU>:
;; The patch below fixes todo-insert-item so that it will
;; insert the item in place, instead of at the top of the
;; buffer, when invoked with a prefix argument.
;;
;; Revision 1.26  1997/10/28 21:14:51  os10000
;; Improvements sent in by Dave Love <d.love@dl.ac.uk>:
;; todo-mode.el: Doc fixes.  Customization.
;; (todo-add-item-non-interactively): New arg -- don't dynamically bind ARG.
;; (todo-insert-item): Use it.
;;
;; Revision 1.25  1997/10/28 20:03:27  os10000
;; Harald Backer <harald.backer@fou.telenor.no> sent the following:
;; Added `todo-save-top-priorities' and option to automatically save top
;; priorities file when saving todo-file.  Changed some default values.
;; Bug fixes.
;;
;; Revision 1.24  1997/10/28 19:41:53  os10000
;; Added fix from Frank Ridderbusch <ridderbusch.pad@sni.de>,
;; an apostrophe was missing.
;;
;; Revision 1.23  1997/10/24  17:30:54  os10000
;; Added three suggestions from Carsten
;; Dominik <dominik@strw.LeidenUniv.nl>:
;;
;; - recommend autoloading instead of require
;; - inserting from different buffer didn't work
;;   (now fixed -- I pray)
;; - provided public entry point to insert items
;;   from normal lisp code
;;
;; Revision 1.22  1997/10/24  16:53:20  os10000
;; Paul Stodghill <stodghil@CS.Cornell.EDU> writes:
;;
;; When invoked with a prefix, todo-insert-item
;; should not prompt for a category.  (He adds:
;; At least that's what I think.)
;;
;; Revision 1.21  1997/10/24  16:51:02  os10000
;; Rafael Laboissiere <rafael@icp.inpg.fr> writes:
;;
;; I was just annoyed with the fact that there is no way
;; to dynamically control the insertion accuracy.  I mean:
;; the variable `todo-insert-threshold' does the job, but
;; it is not very handy if one wants to mix the two
;; behaviors (bisection and "insert right here under the
;; cursor").
;;
;; Therefore I did a quick hack in the function
;; `todo-insert-item'.  Now by giving a prefix argument to
;; the insert command (i.e. by typing "C-u i"), entries
;; are inserted exactly at the line where the cursor is.
;; It would be better to give the value of
;; `todo-insert-threshold' as a numeric argument of
;; `todo-insert-item' (like "M-8 i"), but it's too late
;; now for continuing to hack.
;;
;; Revision 1.20  1997/10/17  15:41:57  os10000
;; Thanks to Harald Backer <harald.backer@fou.telenor.no>, we now have
;; the following facilities available:
;;
;; Added todo-print, todo-top-priorities and todo-jump with matching
;; variables; Parameterized todo-header, todo-category-beg,
;; todo-category-end and todo-category-sep; Added autoload comments;
;; todo-category-select: Modified regexp to make category names unique;
;; todo-forward-item: Added optional COUNT vaiable; todo-insert-item:
;; Rewrote completing read entry.
;;
;; Also, check out the extended list of things left to be done to this
;; package at the end of the documentation!
;;
;; Revision 1.19  1997/10/16  21:21:16  os10000
;; Jari Aalto <jari.aalto@poboxes.com> writes:
;;
;;     I just downloaded your package and after reading the docs I
;;     decided to do some reformatting.  Hope you don't mind.  Now
;;     they are in such a format that the html page can be
;;     automatically generated from the source file.  As an example, I
;;     generated the attached page using the following command:
;;     ripdoc.pls < todo-mode.el | t2html.pls -a "Oliver.Seidel" -e \
;;     Oliver.Seidel@cl.cam.ac.uk -simple -base
;;
;; And of course I appreciate it.  Jari's stuff can be found at:
;; ftp://cs.uta.fi/pub/ssjaaa/, while I'm making the rev 1.18 page
;; available at http://www.cl.cam.ac.uk/users/os10000/doc/todo-mode.html
;; (That link will be valid until 10/1998 or slightly longer.)
;;
;; Revision 1.18  1997/10/15  17:18:11  os10000
;; Everything seems to work in Harald Melands Emacs 20.02 and
;; my Emacs 19.34.  Beware of the spelling in some of the
;; variable names.  I looked up "threshold" in a dictionary
;; and here in Britain this appears to be the way to spell it.
;;
;; Revision 1.17  1997/10/15  14:30:41  os10000
;; Attempted to reconcile Harald's changes with mine since 1.15.
;;
;; Revision 1.16  1997/10/15  14:00:12  os10000
;; Fixed 'file-item' and added 20.02 split-string function.
;;
;; Revision 1.15  1997/10/14  22:22:35  os10000
;; Added string-split (which I stole from ediff-util), changed
;; pop-to-buffer to switch-to-buffer and added message on how
;; to exit the multi-line-edit mode.
;;
;; Revision 1.14  1997/10/09  09:24:50  os10000
;; Harald Meland <harald.meland@usit.uio.no> asked for
;; the latest version, got 1.13, and returned this.
;; He writes:
;;
;; Thanks a lot for the new version of todo-mode.el.  As you will see I
;; have messed it up a bit, hopefully for the better -- I don't like
;; short, cryptic names for variables and functions, so I renamed most of
;; them, and `defalias'ed the old function names.  I hope you don't mind
;; too much, I just kinda couldn't stop myself.
;;
;; Additionally, I included some support for multiline entries, cleaned
;; up (IMHO :) a lot of the code, included completion-support for which
;; category to install a new entry in, and possibly some other changes I
;; can't remember :)
;;
;; It's getting rather late, and I have just done some preliminary
;; testing on whether all of this really works, but so far it looks
;; good.
;;
;; Revision 1.13  1997/08/19  14:00:36  seidel
;; - changed name to todo-mode
;; - fixed menu descriptions
;; - fixed "pressing abort while filing"
;; - attempted Emacs Lisp Manual *Tips* section compliance
;;
;; Revision 1.12  1997/08/06  10:56:15  os10000
;; Fixed header, typos, layout, documentation.
;;
;; Revision 1.11  1997/08/06  09:14:25  os10000
;; Applied patch from Istvan Marko <istvan@cmdmail.amd.com>
;; to make menus work anywhere.
;;
;; Revision 1.10  1997/08/06  08:56:03  os10000
;; Acted upon suggestion from Shane Holder <holder@rsn.hp.com>:
;; Cancelling the editing of an entry will not delete it any more.
;;
;; Revision 1.9  1997/08/06 08:12:03  os10000
;; Improved documentation.  Broke some lines to comply with
;; Richard Stallman's email to please keep in sync with the
;; rest of the Emacs distribution files.
;;
;; Revision 1.8  1997/08/05 22:39:04  os10000
;; Made todo-mode.el available under GPL.
;;
;; Revision 1.7  1997/08/05 22:34:14  os10000
;; Fixed insertion routine with help from Trey Jackson
;; <trey@cs.berkeley.edu>; added todo-inst-tresh;
;; fixed keyboard layout to remove unwanted keys.
;;
;; Revision 1.6  1997/08/05 16:47:01  os10000
;; Incorporated menus for XEmacs from Allan.Cochrane@soton.sc.philips.com,
;; fixed TYPO, fixed todo-file-cmd, cleaned up rcs history.
;;
;; Revision 1.5  1997/08/05  14:43:39  os10000
;; Added improvements from Ron Gut <rgut@aware.com>.
;; Added category management.
;;
;; Revision 1.4  1997/08/04  16:18:45  os10000
;; Added Raise/Lower item.
;;
;; Revision 1.3  1997/08/03  12:47:26  os10000
;; Cleaned up variables, prefix and cursor position.
;;
;; Revision 1.2  1997/08/03 12:15:28  os10000
;; It appears to work.
;;
;; Revision 1.1  1997/08/03 12:15:13  os10000
;; Initial revision
;;

;; ---------------------------------------------------------------------------

;;; Code:

(eval-and-compile                       ; Removable for installation in
                                        ; Emacs 20.
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

;; User-configurable variables:

(defgroup todo nil
  "Maintain a list of todo items."
  :group 'calendar)

(defconst todo-prefix     "*/*"
  "*TODO mode prefix for entries.")

; This is useful in conjunction with `calendar' and `diary' if you use

; #include \"~/.todo-do\"

; in your diary file to include your todo list file as part of your
; diary.  With the default value \"*/*\" the diary displays each entry
; every day and it may also be marked on every day of the calendar.
; Using \"&%%(equal (calendar-current-date) date)\" instead will only
; show and mark todo entreis for today, but may slow down processing of
; the diary file somewhat."
;   :type 'string
;   :group 'todo)

(defcustom todo-file-do    "~/.todo-do"
  "*TODO mode list file."
  :type 'file
  :group 'todo)
(defcustom todo-file-done  "~/.todo-done"
  "*TODO mode archive file."
  :type 'file
  :group 'todo)
(defcustom todo-mode-hook  nil
  "*TODO mode hooks."
  :type 'hook
  :group 'todo)
(defcustom todo-edit-mode-hook nil
  "*TODO Edit mode hooks."
  :type 'hook
  :group 'todo)
(defcustom todo-insert-threshold 0
  "*TODO mode insertion accuracy.

If you have 8 items in your TODO list, then you may get asked 4
questions by the binary insertion algorithm.  However, you may not
really have a need for such accurate priorities amongst your TODO
items.  If you now think about the binary insertion halfing the size
of the window each time, then the threshhold is the window size at
which it will stop.  If you set the threshhold to zero, the upper and
lower bound will coincide at the end of the loop and you will insert
your item just before that point.  If you set the threshhold to,
e.g. 8, it will stop as soon as the window size drops below that
amount and will insert the item in the approximate centre of that
window."
  :type 'integer
  :group 'todo)
(defvar todo-edit-buffer " *TODO Edit*" "TODO Edit buffer name.")
(defcustom todo-file-top "~/.todo-top"
  "*TODO mode top priorities file.

Not in TODO format, but diary compatible.
Automatically generated when `todo-save-top-priorities' is non-nil."
  :type 'string
  :group 'todo)

(defcustom todo-print-function 'ps-print-buffer-with-faces
  "*Function to print the current buffer."
  :type 'symbol
  :group 'todo)
(defcustom todo-show-priorities 1
  "*Default number of priorities to show by \\[todo-top-priorities].
0 means show all entries."
  :type 'integer
  :group 'todo)
(defcustom todo-print-priorities 0
  "*Default number of priorities to print by \\[todo-print].
0 means print all entries."
  :type 'integer
  :group 'todo)
(defcustom todo-remove-separator t
  "*Non-nil to remove category separators in\
\\[todo-top-priorities] and \\[todo-print]."
  :type 'boolean
  :group 'todo)
(defcustom todo-save-top-priorities-too t
  "*Non-nil makes todo-save automatically save top-priorities in
`todo-file-top'."
  :type 'boolean
  :group 'todo)

;; Thanks for the ISO time stamp format go to Karl Eichwalder <ke@suse.de>
;; My format string for the appt.el package is "%3b %2d, %y, %02I:%02M%p".
;;
(defconst todo-time-string-format
  "%:y-%02m-%02d %02H:%02M"
  "*TODO mode time string format for done entries.
For details see the variable `time-stamp-format'.")
;  :type 'string
;  :group 'todo)

(defconst todo-entry-prefix-function 'todo-entry-timestamp-initials
  "*Function producing text to insert at start of todo entry.")
;  :type 'symbol
;  :group 'todo)
(defcustom todo-initials (or (getenv "INITIALS") (user-login-name))
  "*Initials of todo item author."
  :type 'string
  :group 'todo)

(defun todo-entry-timestamp-initials ()
  "Prepend timestamp and your initials to the head of a TODO entry."
  (let ((time-stamp-format todo-time-string-format))
    (concat (time-stamp-string) " " todo-initials ": ")))

;; ---------------------------------------------------------------------------

;; Get some outside help ...

(require 'time-stamp)
(require 'easymenu)

;; ---------------------------------------------------------------------------

;; Set up some helpful context ...

(defvar todo-categories         nil     "TODO categories.")
(defvar todo-cats               nil
  "Old variable for holding the TODO categories.
Use `todo-categories' instead.")
(defvar todo-previous-line      0       "Previous line that I asked about.")
(defvar todo-previous-answer    0       "Previous answer that I got.")
(defvar todo-mode-map           nil     "TODO mode keymap.")
(defvar todo-category-number    0       "TODO category number.")

(defvar todo-tmp-buffer-name "*Tmp*")

(defvar todo-category-sep (make-string 75 ?-)
  "Category separator.")
(defvar todo-category-beg " --- "
  "Category start separator to be prepended onto category name.")
(defvar todo-category-end "--- End"
  "Separator after a category.")
(defvar todo-header "-*- mode: todo; "
  "Header of todo files.")

(defvar todo-font-lock-keywords
  (list (cons (concat
	       "^"
	       (regexp-quote todo-prefix)
	       ".+\\:\\s-*\\(.+\\)$")
	      1)))

(defconst todo-font-lock-defaults
  '(todo-font-lock-keywords t))


;; ---------------------------------------------------------------------------

(if todo-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'todo-forward-category)
    (define-key map "-" 'todo-backward-category)
    (define-key map "d" 'todo-file-item) ;done/delete
    (define-key map "e" 'todo-edit-item)
    (define-key map "E" 'todo-edit-multiline)
    (define-key map "f" 'todo-file-item)
    (define-key map "i" 'todo-insert-item)
    (define-key map "I" 'todo-insert-item-here)
    (define-key map "j" 'todo-jump-to-category)
    (define-key map "k" 'todo-delete-item)
    (define-key map "l" 'todo-lower-item)
    (define-key map "n" 'todo-forward-item)
    (define-key map "p" 'todo-backward-item)
    (define-key map "P" 'todo-print)
    (define-key map "q" 'todo-quit)
    (define-key map "r" 'todo-raise-item)
    (define-key map "s" 'todo-save)
    (define-key map "S" 'todo-save-top-priorities)
    (define-key map "t" 'todo-top-priorities)
    (setq todo-mode-map map)))

(defun todo-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (nth todo-category-number todo-categories)))
    (setq mode-line-buffer-identification
;;          (concat "Category: " name))
          (concat "Category: " (format "%18s" name)))
    (widen)
    (goto-char (point-min))
    (search-forward-regexp
     (concat "^"
             (regexp-quote (concat todo-prefix todo-category-beg name))
             "$"))
    (let ((begin (1+ (point-at-eol))))
      (search-forward-regexp (concat "^" todo-category-end))
      (narrow-to-region begin (point-at-bol))
      (goto-char (point-min)))))
(defalias 'todo-cat-slct 'todo-category-select)

(defun todo-forward-category () "Go forward to TODO list of next category."
  (interactive)
  (setq todo-category-number
        (mod (1+ todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-forw 'todo-forward-category)

(defun todo-backward-category () "Go back to TODO list of previous category."
  (interactive)
  (setq todo-category-number
        (mod (1- todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-back 'todo-backward-category)

(defun todo-backward-item () "Select previous entry of TODO list."
  (interactive)
  (search-backward-regexp (concat "^" (regexp-quote todo-prefix)) nil t)
  (message ""))
(defalias 'todo-cmd-prev 'todo-backward-item)

(defun todo-forward-item (&optional count)
  "Select COUNT-th next entry of TODO list."
  (interactive "P")
  (if (listp count) (setq count (car count)))
  (end-of-line)
  (search-forward-regexp (concat "^" (regexp-quote todo-prefix))
                         nil 'goto-end count)
  (beginning-of-line)
  (message ""))
(defalias 'todo-cmd-next 'todo-forward-item)

(defun todo-save () "Save the TODO list."
  (interactive)
  (save-buffer)
  (if todo-save-top-priorities-too (todo-save-top-priorities))
  )
(defalias 'todo-cmd-save 'todo-save)

(defun todo-quit () "Done with TODO list for now."
  (interactive)
  (widen)
  (todo-save)
  (message "")
  (bury-buffer))
(defalias 'todo-cmd-done 'todo-quit)

(defun todo-edit-item () "Edit current TODO list entry."
  (interactive)
  (let ((item (todo-item-string)))
    (if (todo-string-multiline-p item)
        (todo-edit-multiline)
      (let ((new (read-from-minibuffer "Edit: " item)))
        (todo-remove-item)
        (insert new "\n")
        (todo-backward-item)
        (message "")))))
(defalias 'todo-cmd-edit 'todo-edit-item)

(defun todo-edit-multiline ()
  "Set up a buffer for editing a multiline TODO list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todo-edit-buffer)))
    (switch-to-buffer
     (make-indirect-buffer
      (file-name-nondirectory todo-file-do) buffer-name))
    (message "To finish editing, type C-c C-c.")
    (todo-edit-mode)
    (narrow-to-region (todo-item-start) (todo-item-end))))

;;;### autoload
(defun todo-add-category (cat) 
  "Add new category CAT to the TODO list."
  (interactive "sCategory: ")
  (save-window-excursion
    (setq todo-categories (cons cat todo-categories))
    (find-file todo-file-do)
    (widen)
    (goto-char (point-min))
    (let ((posn (search-forward "-*- mode: todo; " 17 t)))
      (if (not (null posn)) (goto-char posn))
      (if (equal posn nil)
          (progn
            (insert "-*- mode: todo; \n")
            (forward-char -1))
        (kill-line)))
    (insert (format "todo-categories: %S; -*-" todo-categories))
    (forward-char 1)
    (insert (format "%s%s%s\n%s\n%s %s\n"
                    todo-prefix todo-category-beg cat
                    todo-category-end 
                    todo-prefix todo-category-sep)))
  0)

;;;### autoload
(defun todo-add-item-non-interactively (new-item category)
  "Insert NEW-ITEM in TODO list as a new entry in CATEGORY."
  (save-excursion
    (todo-show))
  (save-excursion
    (if (string= "" category)
        (setq category (nth todo-category-number todo-categories)))
    (let ((cat-exists (member category todo-categories)))
      (setq todo-category-number
            (if cat-exists
                (- (length todo-categories) (length cat-exists))
              (todo-add-category category))))
    (todo-show)
    (setq todo-previous-line 0)
    (let ((top 1)
	  (bottom (1+ (count-lines (point-min) (point-max)))))
      (while (> (- bottom top) todo-insert-threshold)
	(let* ((current (/ (+ top bottom) 2))
	       (answer (if (< current bottom)
			   (todo-more-important-p current) nil)))
	  (if answer
	      (setq bottom current)
	    (setq top (1+ current)))))
      (setq top (/ (+ top bottom) 2))
      ;; goto-line doesn't have the desired behavior in a narrowed buffer
      (goto-char (point-min))
      (forward-line (1- top)))
    (insert new-item "\n")
    (todo-backward-item)
    (todo-save)
    (message "")))

;;;### autoload
(defun todo-insert-item (ARG)
  "Insert new TODO list entry.
With a prefix argument solicit the category, otherwise use the current
category."
  (interactive "P")
  (save-excursion
    (if (not (string-equal mode-name "TODO")) (todo-show))
    (let* ((new-item (concat todo-prefix " "
			     (read-from-minibuffer
			      "New TODO entry: "
			      (if todo-entry-prefix-function
				  (funcall todo-entry-prefix-function)))))
	   (categories todo-categories)
	   (history (cons 'categories (1+ todo-category-number)))
	   (current-category (nth todo-category-number todo-categories))
	   (category 
	    (if ARG
		current-category
	      (completing-read 
	       (concat "Category ["
		       current-category "]: ")
	       (todo-category-alist) nil nil nil history))))
      (todo-add-item-non-interactively new-item category))))

(defalias 'todo-cmd-inst 'todo-insert-item)

;;;### autoload
(defun todo-insert-item-here ()
  "Insert new TODO list entry under the cursor."
  (interactive "")
  (save-excursion
    (if (not (string-equal mode-name "TODO")) (todo-show))
    (let* ((new-item (concat todo-prefix " "
			     (read-from-minibuffer
			      "New TODO entry: "
			      (if todo-entry-prefix-function
				  (funcall todo-entry-prefix-function))))))
      (insert (concat new-item "\n")))))

(defun todo-more-important-p (line)
  "Ask whether entry is more important than the one at LINE."
  (if (not (equal todo-previous-line line))
      (progn
        (setq todo-previous-line line)
        (goto-char (point-min))
        (forward-line (1- todo-previous-line))
        (let ((item (todo-item-string-start)))
          (setq todo-previous-answer
                (y-or-n-p (concat "More important than '" item "'? "))))))
  todo-previous-answer)
(defalias 'todo-ask-p 'todo-more-important-p)

(defun todo-delete-item () "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((todo-entry (todo-item-string-start))
             (todo-answer (y-or-n-p (concat "Permanently remove '"
                                            todo-entry "'? "))))
        (if todo-answer
            (progn
              (todo-remove-item)
              (todo-backward-item)))
        (message ""))
    (error "No TODO list entry to delete")))
(defalias 'todo-cmd-kill 'todo-delete-item)

(defun todo-raise-item () "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point)) 0)
      (let ((item (todo-item-string)))
        (todo-remove-item)
        (todo-backward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to raise")))
(defalias 'todo-cmd-rais 'todo-raise-item)

(defun todo-lower-item () "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point) (point-max)) 1)
      ;; Assume there is a final newline
      (let ((item (todo-item-string)))
        (todo-remove-item)
        (todo-forward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to lower")))
(defalias 'todo-cmd-lowr 'todo-lower-item)

(defun todo-file-item (&optional comment)
  "File the current TODO list entry away,
annotated with an optional COMMENT."
  (interactive "sComment: ")
  (or (> (count-lines (point-min) (point-max)) 0)
      (error "No TODO list entry to file away"))
  (let ((time-stamp-format todo-time-string-format))
    (if (and comment (> (length comment) 0))
	(progn
	  (goto-char (todo-item-end))
	  (insert
	   (if (save-excursion (beginning-of-line)
			       (looking-at (regexp-quote todo-prefix)))
	       " "
	     "\n\t")
	   "(" comment ")")))
    (goto-char (todo-item-end))
    (insert " [" (nth todo-category-number todo-categories) "]")
    (goto-char (todo-item-start))
    (let ((temp-point (point)))
      (if (looking-at (regexp-quote todo-prefix))
	  (replace-match (time-stamp-string))
	;; Standard prefix -> timestamp
	;; Else prefix non-standard item start with timestamp
	(insert (time-stamp-string)))
      (append-to-file temp-point (1+ (todo-item-end)) todo-file-done)
      (delete-region temp-point (1+ (todo-item-end))))
    (todo-backward-item)
    (message "")))

;; ---------------------------------------------------------------------------

;; Utility functions:


;;;###autoload
(defun todo-top-priorities (&optional nof-priorities category-pr-page)
  "List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to \'todo-show-priorities\'.

If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category."

  (interactive "P")
  (or nof-priorities (setq nof-priorities todo-show-priorities))
  (if (listp nof-priorities)            ;universal argument
      (setq nof-priorities (car nof-priorities)))
  (let ((todo-print-buffer-name "*Tmp*")
        ;;(todo-print-category-number 0)
        (todo-category-break (if category-pr-page "" ""))
        (cat-end
         (concat
          (if todo-remove-separator
              (concat todo-category-end "\n"
                      (regexp-quote todo-prefix) " " todo-category-sep "\n")
            (concat todo-category-end "\n"))))
        beg end)
    (todo-show)
    (save-excursion
      (save-restriction
        (widen)
        (copy-to-buffer todo-print-buffer-name (point-min) (point-max))
        (set-buffer todo-print-buffer-name)
        (goto-char (point-min))
        (if (re-search-forward (regexp-quote todo-header) nil t)
            (progn
              (beginning-of-line 1)
              (kill-line)))             ;Remove mode line
        (while (re-search-forward       ;Find category start
                (regexp-quote (concat todo-prefix todo-category-beg))
                nil t)
          (setq beg (+ (point-at-eol) 1)) ;Start of first entry.
          (re-search-forward cat-end nil t)
          (setq end (match-beginning 0))
          (replace-match todo-category-break)
          (narrow-to-region beg end)    ;In case we have too few entries.
          (goto-char (point-min))
          (if (= 0 nof-priorities)      ;Traverse entries.
              (goto-char end)            ;All entries
            (todo-forward-item nof-priorities))
          (setq beg (point))
          (delete-region beg end)
          (widen))
        (and (looking-at "") (replace-match "")) ;Remove trailing form-feed.
        (goto-char (point-min))         ;Due to display buffer
        ))
    ;; Could have used switch-to-buffer as it has a norecord argument,
    ;; which is nice when we are called from e.g. todo-print.
    ;; Else we could have used pop-to-buffer.
    (display-buffer todo-print-buffer-name)
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             todo-print-buffer-name)
    ))

;;;###autoload
(defun todo-save-top-priorities (&optional nof-priorities)
  "Save top priorities for each category in `todo-file-top'.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to `todo-show-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (todo-top-priorities nof-priorities)
        (set-buffer todo-tmp-buffer-name)
        (write-file todo-file-top)
        (kill-this-buffer)
        ))))

;;;###autoload
(defun todo-print (&optional category-pr-page)
  "Print todo summary using \\\[todo-print-function].
If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category.

Number of entries for each category is given by
\'todo-print-priorities\'."
  (interactive "P")
  (if todo-print-function
      (progn
  (save-window-excursion
  (save-excursion
    (save-restriction
      (todo-top-priorities todo-print-priorities
                                    category-pr-page)
              (set-buffer todo-tmp-buffer-name)
              (and (funcall todo-print-function)
                   (kill-this-buffer))
            (message "Todo printing done."))
            )))
    (message "todo-print-function undefinded")
    ))

(defun todo-jump-to-category ()
  "Jump to a category.  Default is previous category."
  (interactive)
  (let* ((categories todo-categories)
         (history (cons 'categories (1+ todo-category-number)))
	 (category (completing-read 
                    (concat "Category ["
                            (nth todo-category-number todo-categories) "]: ")
                    (todo-category-alist) nil nil nil history)))
    (if (string= "" category)
        (setq category (nth todo-category-number todo-categories)))
    (setq todo-category-number
          (if (member category todo-categories)
              (- (length todo-categories)
                 (length (member category todo-categories)))
            (todo-add-category category)))
    (todo-show)))

(defun todo-line-string () "Return current line in buffer as a string."
  (buffer-substring (point-at-bol) (point-at-eol)))

(defun todo-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todo-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defun todo-item-start () "Return point at start of current TODO list item."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at (regexp-quote todo-prefix)))
        (search-backward-regexp
         (concat "^" (regexp-quote todo-prefix)) nil t))
    (point)))

(defun todo-item-end () "Return point at end of current TODO list item."
  (save-excursion
    (end-of-line)
    (search-forward-regexp
     (concat "^" (regexp-quote todo-prefix)) nil 'goto-end)
    (1- (point-at-bol))))

(defun todo-remove-item () "Delete the current entry from the TODO list."
  (delete-region (todo-item-start) (1+ (todo-item-end))))

(defun todo-item-string () "Return current TODO list entry as a string."
  (buffer-substring (todo-item-start) (todo-item-end)))

(defun todo-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todo-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todo-string-count-lines string) 1))

(defun todo-category-alist ()
  "Generate an alist for use in `completing-read' from `todo-categories'."
  (mapcar (lambda (cat) (cons cat nil))
          todo-categories))

;; utility functions:  These are available in XEmacs, but not in Emacs 19.34

(if (not (fboundp 'point-at-bol))
    (defun point-at-bol () "Return value of point at beginning of line."
      (save-excursion
        (beginning-of-line)
        (point))))

(if (not (fboundp 'point-at-eol))
    (defun point-at-eol () "Return value of point at end of line."
      (save-excursion
        (end-of-line)
        (point))))

;; splits at a white space, returns a list
(if (not (fboundp 'split-string))
    (defun split-string (string &optional separators)
      "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
      (let ((rexp (or separators "[ \f\t\n\r\v]+"))
            (start 0)
            (list nil))
        (while (string-match rexp string start)
          (or (eq (match-beginning 0) 0)
              (setq list
                    (cons (substring string start (match-beginning 0))
                          list)))
          (setq start (match-end 0)))
        (or (eq start (length string))
            (setq list
                  (cons (substring string start)
                        list)))
        (nreverse list))))

;; ---------------------------------------------------------------------------

(easy-menu-define todo-menu todo-mode-map "Todo Menu"
                  '("Todo"
                    ["Next category"        todo-forward-category t]
                    ["Previous category"    todo-backward-category t]
                    ["Jump to category"     todo-jump-to-category t]
                    ["Show top priority items" todo-top-priorities t]
                    ["Print categories"     todo-print t]
                    "---"
                    ["Edit item"            todo-edit-item t]
                    ["File item"            todo-file-item t]
                    ["Insert new item"      todo-insert-item t]
                    ["Insert item here"     todo-insert-item-here t]
                    ["Kill item"            todo-delete-item t]
                    "---"
                    ["Lower item priority"  todo-lower-item t]
                    ["Raise item priority"  todo-raise-item t]
                    "---"
                    ["Next item"            todo-forward-item t]
                    ["Previous item"        todo-backward-item t]
                    "---"
                    ["Save"                 todo-save t]
                    ["Save Top Priorities"  todo-save-top-priorities t]
                    "---"
                    ["Quit"                 todo-quit t]
                    ))

;; As calendar reads .todo-do before todo-mode is loaded.
;;;### autoload
(defun todo-mode () "Major mode for editing TODO lists.\n\n\\{todo-mode-map}"
  (interactive)
  (setq major-mode 'todo-mode)
  (setq mode-name "TODO")
  (use-local-map todo-mode-map)
  (set (make-local-variable 'font-lock-defaults) todo-font-lock-defaults)p
  (easy-menu-add todo-menu)
  (run-hooks 'todo-mode-hook))

;; Read about this function in the setup instructions above!
;;;### autoload
(defun todo-cp ()
  "Make a diary entry appear only in the current date's diary"
  (if (equal (calendar-current-date) date)
      entry
    nil))

(defun todo-edit-mode ()
  "Major mode for editing items in the TODO list\n\n\\{todo-edit-mode-map}"
  (text-mode)
  (setq major-mode 'todo-edit-mode)
  (setq mode-name "TODO Edit")
  (local-set-key (kbd "C-c C-c") 'todo-edit-done)
  (run-hooks 'todo-edit-mode-hook))

(defun todo-edit-done ()
  "Finish editing the current TODO item."
  (interactive)
  (kill-buffer (current-buffer))
  (todo-show))

;;;### autoload
(defun todo-show () "Show TODO list."
  (interactive)
  (if (file-exists-p todo-file-do)
      (find-file todo-file-do)
    (todo-initial-setup))
  (if (null todo-categories)
      (if (null todo-cats)
          (error "Error in %s: No categories in list `todo-categories'"
                 todo-file-do)
        (goto-char (point-min))
        (and (search-forward "todo-cats:" nil t)
             (replace-match "todo-categories:"))
        (make-local-variable 'todo-categories)
        (setq todo-categories todo-cats)))
  (beginning-of-line)
  (todo-category-select))
(defalias 'todo 'todo-show)

(defun todo-initial-setup () "Set up things to work properly in TODO mode."
  (find-file todo-file-do)
  (erase-buffer)
  (todo-mode)
  (todo-add-category "Todo"))

(provide 'todo-mode)

;; ---------------------------------------------------------------------------
;;; todo-mode.el ends here
;; ---------------------------------------------------------------------------
