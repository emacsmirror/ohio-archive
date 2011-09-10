From ark1!nap1!ames!mailrus!cornell!kowalski Wed Dec  6 08:00:21 1989
Article 986 of comp.emacs:
Path: ark1!nap1!ames!mailrus!cornell!kowalski
From kowalski@svax.cs.cornell.edu (Jeffrey Kowalski)
Newsgroups: comp.emacs
Subject: EDT/TPU Emulator for GNU Emacs (tpu.el)
Message-ID: <34798@cornell.UUCP>
Date: 3 Dec 89 00:02:29 GMT
Sender: nobody@cornell.UUCP
Reply-To: kowalski@svax.cs.cornell.edu (Jeffrey Kowalski)
Organization: Cornell Univ. CS Dept, Ithaca NY
Lines: 2109


Hack, hack, hack.
This represents a serious series of hacks to edt.el (as
supplied with GNU Emacs) in order to more closely simulate
the edt editor.  I was wrong when I called it a TPU emulator.
TPU would be tough to emulate in Emacs since TPU is a language.
What I meant was that tpu.el simualtes the EDT interface to TPU.

Well, with a few extensions.

There's a truly nice new style of abbrevs that uses alists.
Try typing the partial word "Heu", then GOLD-H a few times.
See where the cursor goes?

I don't have up to date docs for my toy, but I'll include
what I gave the students last semester, along with tpu.el

Have fun, mail me bugs, questions, chocolate chip cookies.

Jeff

CADIF Swamp
172 Hollister Hall
Cornell University
Ithaca, NY 14853

----------
GUIDE
---------
The long-heralded emulation of the highly acclaimed BOBTPU is here!

What is bobtpu?  It's simply the most extended editor ever.  Written by
system programmer par-excellence Bob Covey, it was formerly available
only on VMS systems supporting tpu.  Now, through the miracle of modern
pharmaceuticals, Jeff Kowalski has just completed a complete emulation
of the incredible editor under Unix.  That's right, UNIX!  For the first
time ever in the history of CADIF, the Unix user, previously hindered by
such evil tortures as vi and ed, will finally be able to complete work!

"How can I, so simple an undeserving a student, gain access to the
fruits of Jeff's laudable accomplishment?"  Well, I will tell you.  But
first, you must suffer through the incredible mind-expanding description
of what the bobtpu emulator can do for you (or you must turn the page).

Firstly, the bobtpu emulator (hereafter called bobtpu) is written in
elisp, which is the emacs native language.  This means that the
behind-the-scenes editor is GNU Emacs, not VAX TPU.  There are,
therefore, a few subtle differences between VMS bobtpu and Unix bobtpu.
For the most part, however, you'll probably never notice.  If you have
questions, you should refer to both the EDT and Emacs guides.  Ask the
room monitor for them.

At the very least, bobtpu is an EDT emulator.  That means that the
arrows work and the keypad at the right has special meaning, among other
things.  If you are not familiar with EDT, ask the room monitor for
information (a manual or online CAI on VMS).  In addition to the basics
of EDT, bobtpu provides all of these extras:

				TERMINOLOGY

   The GOLD key is the PF1 key on DEC-style keyboards, F9 on SGI style
keyboards.  It is not a modifier in the same sense as the shift or
control keys: you don't hold it down while striking another key, you
simply strike it first.  Thus GOLD-a means strike PF1, then the letter
"a."  On the VMS version, GOLD-a and GOLD-A are equivalent.  On my unix
version, the letters are different, so we have twice the functionality.
In general, the lowercase version is the same as the VMS version, while
the uppercase version, since it is harder to type, performs an extended
operation, or an operation with no query.  For example, GOLD-q quits,
but asks about saving unsaved files first; GOLD-Q quits without asking.
In the cases where case is insignificant, I will represent the sequence
as GOLD-[Aa] which means use either case.
   If you get stuck in some crazy mode, you can type CTRL-G (cancel) to
get out.  So if you're being prompted for a filename and you don't know
why (ie you're lost), just hit a CTRL-G.

				MULTIPLE FILES

   It is possible to edit many files at a time.  They may be entered
using wildcards or lists on the command line. Any files which are not
found and do not contain wildcards will be created and marked as
modified. You are placed in the first file read/created (if no files are
found and all specifications have wildcards, you will be placed in a
default buffer).  The files are marked for update on exit.  A status
line shows information about the current buffer, just as in Emacs.

    The easiest way to move between files is by hitting GOLD-W to "walk"
to the next one.  GOLD-w will "walk" to the previous one.  GOLD-f will
ask you for the name of a file to jump to, it will try to find one it
has that best matches the name you give.  GOLD-F will do the same, but
will show the selected buffer in another window.

    You can add more files to the list (just as if you had included them
on the original edit command) by hitting GOLD-e.  It will prompt for the
file(s), which may contain wildcards/lists.  The new files will be
marked for update on exit.  There is and equivalent line-mode "find-file
filespec(s)".  GOLD-E will read the file into another window.

    CTRL-F allows you to specify a new filename to be attached to the
current buffer.

    Hitting GOLD-[Uu] will change the update mode of a buffer.  Only
those buffers marked with the double star in the status line will be
written out on exit.
 
    GOLD-[Bb] will generate a list of the current buffers.  The list
will show a dot in the first column to indicate the current buffer, a
star in the second for each buffer that has been modified, and a "%" in
the third if the buffer is marked readonly.  It also shows the number of
characters in each buffer and the filename and mode associated with it.

    GOLD-[Ii] is a handy shorthand for the line mode "insert-file
filename" command.  It will prompt for the name of a file, which is
included into the current buffer immediately after the current cursor
location.  The file name specified may contain wildcards and/or lists.

    GOLD-CTRL-W allows you to have the current versions of all buffers
written out (saved), without exiting the editor.

    GOLD-k will throw away the current buffer and move onto the next,
but will ask for confirmation first.  GOLD-K doesn't ask.  Be really
careful...


				 SHELL COMMANDS

    It is possible to execute shell commands from within the editor, and
get the results.  Hitting GOLD-[Dd] will prompt you for a shell command,
then place you in the shell buffer.  The command is executed in a
subprocess in that other buffer.  Commands which try to take over the
screen (such as talk) may not work.


				DEFINING KEYS

    Defining keys is almost the same as in EDT, the only major
difference is that it executes the keys as you build the definition --
you don't have to try to visualize what the sequence will do.  The
definitions properly handle repeat counts (even if you use a repeat
count on a defined key containing a repeat on some other keys), and
execution of a key definition halts immediately if it encounters any
error (string not found, move past end of buffer, etc.).


				SUBSTITUTIONS

    GOLD-s or GOLD-/ allows you to easily specify a substitution.  It
will prompt for the old string you want to replace, and the new
replacement string.  Then, the function will pause at each occurence of
the original and wait for a response.  Typical responses are "Y"
(replace), "N" (don't replace), "Q" (stop replacing).  For more
information, see the Emacs manual under query-replace.  GOLD-S will
replace all the old strings with the new ones, without asking for any
confirmation.


				   AUTO-TAB

    When enabled (disabled by default), hitting RETURN will act as if
you hit RETURN, followed by appropriate spacing if the resulting line is
blank.  TAB at the beginning of a line (still in the whitespace) will
re-indent the line according to the language you are using.  TAB
anywhere in the text is still a tab.  GOLD-[Aa] is provided as an easy
way to toggle autotab mode on and off.


				    OTHER

    GOLD-DEL (GOLD-BS on some) will remove the current window, if it's
not the only one.  CTRL-X 1 will make the current window the only one,
by removing all the others.  Remember, buffers and windows are not the
same thing.  Don't worry: removing a window doesn't erase that buffer!

    You can get help at any time by pressing the HELP key on VT200
keyboards, or using "GOLD-KP7 info RETURN"

    You can get the description of any key's function by using the PF2
key, then pressing a key sequence.  Neat, huh?

    As with normal EDT at CADIF, GOLD-q and GOLD-x provide any easy way
to quit without saving any changes or exit & write out all buffers
marked update which have been modified.  The lowercase versions ask for
confirmation if necessary; GOLD-Q and GOLD-X do not.  Be careful...

    GOLD-[Yy] is defined to copy the selected region into the paste
buffer.  It is similar to the CUT command, but you don't have to paste
the text back in, and it doesn't mark the buffer as changing.

    GOLD-CTRL-L and GOLD-CTRL-U act the same as CHANGE CASE (GOLD-KP1),
except that all the text is forced to be lower or upper case instead of
switching to the opposite case.

    GOLD-UP and GOLD-DOWN will move up or down a full screen.  GOLD-LEFT
and GOLD-RIGHT will ask you for a new screen size, in case your terminal
is screwed up.

    GOLD-SPACE is an undo.  You can typically undo about 500 major
changes.  After that, you're on your own.

    GOLD-~ swaps the current location with the other end of the selected
region, allowing you to check where it is or easily adjust both ends.



			      ADVANCED FUNCTIONS

    And now some more advanced (pronounced "bizarre") definitions...


				   MARKERS

    GOLD-. (keyboard period, not the one on the keypad) will set a
marker that can be returned to later using GOLD-, (also not the one on
the keypad).  You may use a repeat count to specify which marker (1 is
normally the default).  On DEC-style keyboards, the keys F17-F20 retrieve
markers 2 through 5 automatically, and GOLD-F17 through GOLD-F20
insert them.  Pretty convenient, huh?

			       SEARCH PATTERNS

    GOLD-' or GOLD-" will allow you to search for a regular expression
instead of just a simple string.  The pattern matching is relatively
powerful, but takes a little practice.  See a good Unix manual (ha!) for
more details on patterns.  After a pattern search is entered, FIND NEXT
(PF3) may be used to search with the same pattern.

			       MATCH DELIMITERS

    The VMS version of bobtpu has special keys for matching delimiters.
The unix version does it constantly, by blinking on the matching open
delimiter briefly.  For example, if you type a closing brace (}) in
c-mode, the editor rests the cursor briefly on the mathing opening
brace, then returns the cursor to its proper place.

				   COUNTERS

    There is an internal counter that may be useful.  GOLD-= will set
the counter to the repeat count, zero if not specified.  GOLD-> and
GOLD-< will increment and decrement the counter by the repeat count (1
by default).  GOLD-# will copy the value of the counter into the buffer
using the current format, which is C-like format that may be changed
using GOLD-! (the default is %d).  GOLD-* inserts the ASCII character
corresponding to the counter value (mod 256).

				TEXT EXPANSION

    GOLD-h (also DO on VT200-style keyboards or F1 on SGI-styles) will
invoke an automatic text expander.  This will search a special list for
the word which is under the cursor.  If it finds a line containing the
word (called the key), the word is replaced by a "tag".  Repeating will
find successive occurrences, replacing the text each time.  If no more
matches are found, it will restore the original word.  For instance, if
you enter "open<GOLD-h>", it might first replace "open" with
"HC_Open_Segment ();", then "HC_KOpen_Segment ();", and then return to
"open" if requested a third time.  Gold-DO or GOLD-H will cause the
original text to be restored.  Currently, all the HOOPS commands are in
the list.  If you'd like to add more, I'll provide a facility for doing
so.

				SPELLING CHECKER

    If spell has been installed on the system it can be invoked to check
all or part of a file/buffer.  The simplest means of using it is to use
GOLD-?.  This will cause it to generate Spell as a subprocess and have
it check the selected region, or the whole buffer if no region was
selected.  If any changes are made in Spell and then saved, the changes
will be incorporated back into the editor.

				  NUMERIC KEYPAD

    GOLD-$ toggles between numeric keypad mode (the digits and
punctuation characters on the keypad merely insert the corresponding
text into the buffer) and function mode (the keypad keys have their
normal editing functions).

 				    LINE MODE

    You can enter commands directly to the emulator with GOLD-KP7.  You
can evaluate elisp s-espressions with GOLD-PF2.


** If for some obscure reason you want plain emacs, 
** you can type "GOLD-KP7 cancel-tpu"
** Use "ESC-X use-tpu" to get it back.


---------
tpu.el
--------
;;;
;;;
;;;
;;;  E D T / B O B T P U
;;;  Editor Definitions
;;;  Jeff Kowalski
;;;  November, 1988
;;;
;;;  Based on the edt.el 
;;;
;;;
;; Copyright (C) 1989 Free Software Foundation, Inc.

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
;;; 


;;;
;;;
;;;  G L O B A L
;;;  K E Y M A P S
;;;
;;;
(defvar CSI-map nil
  "Maps the CSI function keys on the VT100 keyboard.
CSI is DEC's name for the sequence <ESC>[.")
(setq CSI-map (make-sparse-keymap))

(defvar APPLE-map nil
  "Maps the Apple Extended Keyboard function keys.
The sequence begins with CTRL-A.")
(setq APPLE-map (make-sparse-keymap))

(defvar DCS-map nil
  "Maps the DCS function keys on the iris-ansi keyboard.
DCS is SGI's name for the sequence <ESC>P.")
(setq DCS-map (make-sparse-keymap))

(defvar SS3-map nil
  "Maps the SS3 function keys on the VT100 keyboard.
SS3 is DEC's name for the sequence <ESC>O.")
(setq SS3-map (make-sparse-keymap))

(defvar GOLD-map nil
   "Maps the function keys on the VT100 keyboard preceeded by PF1.
GOLD is the ASCII 7-bit escape sequence <ESC>OP.")
(setq GOLD-map (make-sparse-keymap))

(defvar GOLD-APPLE-map nil
  "Maps the function keys on the Apple Keyboard preceeded by F9 CTRL-A.")
(setq GOLD-APPLE-map (make-sparse-keymap))

(defvar GOLD-CSI-map nil
   "Maps the function keys on the VT100 keyboard preceeded by GOLD-CSI.")
(setq GOLD-CSI-map (make-sparse-keymap))

(defvar GOLD-SS3-map nil
   "Maps the function keys on the VT100 keyboard preceeded by GOLD-SS3.")
(setq GOLD-SS3-map (make-sparse-keymap))


;;;
;;;
;;;  G L O B A L
;;;  V A R I A B L E S
;;;
;;;
(defvar tpu-global-key-plist nil
  "Original pre-tpu definitions of global keys, so they may be restored.")
(defvar tpu-breadcrumb-plist nil
  "The set of user-defined markers (breadcrumbs), as a plist.")
(defvar tpu-current-expansion nil
  "The expansion that was last performed.")
(defvar tpu-last-expand-point 0
  "The position at which the last bob-expand was performed.")
(defvar tpu-expand-key ""
  "The key for which the last expansion was performed.")
(defvar tpu-counter 0
  "A general-purpose counter available for the user.")
(defvar tpu-counter-format "%d"
  "The format to use to when displaying the tpu-counter.")
(defvar tpu-last-replaced-text ""
  "Last text deleted by an TPU emulation replace command.")
(defvar tpu-last-deleted-lines ""
  "Last text deleted by an TPU emulation line-delete command.")
(defvar tpu-last-deleted-words ""
  "Last text deleted by an TPU emulation word-delete command.")
(defvar tpu-last-deleted-chars ""
  "Last text deleted by an TPU emulation character-delete command.")
(defvar tpu-search-last-string ""
  "Last text searched by the tpu-search commands.")
(defvar newline-and-indent-p nil
  "Predicate: return produces a newline and indent.")


;;;
;;;
;;;  U T I L I T I E S
;;;
;;;
(defun caar (thingy) (car (car thingy)))
(defun cadr (thingy) (car (cdr thingy)))
(defun cadar (thingy) (car (cdr (car thingy))))
(defun caddar (thingy) (car (cdr (cdr (car thingy)))))


;;;
;;;  Bob-expand
;;;
(defun bob-unexpand nil
  "Cancels current expansion."
  (interactive)
    (cond ((and (= (point) tpu-last-expand-point)
                (string= (cadar tpu-current-expansion)
                         (buffer-substring
                          (- (point)
                             (length (cadar tpu-current-expansion))
                             (caddar tpu-current-expansion))
                          (- (point)
                             (caddar tpu-current-expansion)))))
         (backward-char (caddar tpu-current-expansion))
         (backward-delete-char (length (cadar tpu-current-expansion)))
         (insert tpu-expand-key)
         (message "Cancelled.")
         (setq tpu-last-expand-point 0))
        (t (message "No expansion to cancel."))))

(defun bob-expand nil
  "Expand the word before the point."
  (interactive)
  (let ((beg (point))
        (old-syntax (char-syntax 95))
        (not-found t))
    (cond ((and (= (point) tpu-last-expand-point)
                (string= (cadar tpu-current-expansion)
                         (buffer-substring
                          (- (point)
                             (length (cadar tpu-current-expansion))
                             (caddar tpu-current-expansion))
                          (- (point)
                             (caddar tpu-current-expansion)))))
           (backward-char (caddar tpu-current-expansion))
           (backward-delete-char (length (cadar tpu-current-expansion)))
           (setq tpu-current-expansion (cdr tpu-current-expansion)))
          (t (setq tpu-current-expansion tpu-expansions)
             (modify-syntax-entry 95 "w")
             (forward-word -1)
             (modify-syntax-entry 95 (char-to-string old-syntax))
             (setq tpu-expand-key (buffer-substring (point) beg))
             (delete-region beg (point))))
    (while (and tpu-current-expansion not-found)
      (if (string-match tpu-expand-key (caar tpu-current-expansion))
          (setq not-found nil)
        (setq tpu-current-expansion (cdr tpu-current-expansion))))
    (cond (tpu-current-expansion
           (insert (cadar tpu-current-expansion))
           (forward-char (caddar tpu-current-expansion))
           (setq tpu-last-expand-point (point)))
          (t
           (insert tpu-expand-key)
           (message "Not found.")
           (setq tpu-last-expand-point 0)))))

;;;
;;;  Breadcrumbs
;;;
(defun drop-breadcrumb (num)
  "Drops a breadcrumb that can be returned to later with goto-breadcrumb."
  (interactive "p")
  (put tpu-breadcrumb-plist num (list (current-buffer) (point)))
  (message "Mark %d set." num))
  
(defun goto-breadcrumb (num)
  "Returns to a breadcrumb set with goto-breadcrumb."
  (interactive "p")
  (cond ((get tpu-breadcrumb-plist num)
	 (switch-to-buffer (car (get tpu-breadcrumb-plist num)))
	 (goto-char (cadr (get tpu-breadcrumb-plist num)))
	 (message "mark %d found." num))
	(t
	 (message "mark %d not found." num))))

(defun drop-breadcrumb-2 nil
  "drops breadcrumb number 2 at the point."
  (interactive)
  (drop-breadcrumb 2))

(defun drop-breadcrumb-3 nil
  "drops breadcrumb number 3 at the point."
  (interactive)
  (drop-breadcrumb 3))

(defun drop-breadcrumb-4 nil
  "drops breadcrumb number 4 at the point."
  (interactive)
  (drop-breadcrumb 4))

(defun drop-breadcrumb-5 nil
  "drops breadcrumb number 5 at the point."
  (interactive)
  (drop-breadcrumb 5))

(defun goto-breadcrumb-2 nil
  "goto breadcrumb number 2."
  (interactive)
  (goto-breadcrumb 2))

(defun goto-breadcrumb-3 nil
  "goto breadcrumb number 3."
  (interactive)
  (goto-breadcrumb 3))

(defun goto-breadcrumb-4 nil
  "goto breadcrumb number 4."
  (interactive)
  (goto-breadcrumb 4))

(defun goto-breadcrumb-5 nil
  "goto breadcrumb number 5."
  (interactive)
  (goto-breadcrumb 5))


;;;
;;;  miscellaneous
;;;
(defun replace-global-key (key func)
  "Saves the current global key definition and replaces it with a new one."
  (interactive)
  (if (not (get tpu-global-key-plist key))
      (put tpu-global-key-plist key func))
  (define-key global-map key func))

(defun restore-global-key (key)
  "Restores the original definition of a global key."
  (interactive)
  (define-key global-map key (get tpu-global-key-plist key)))

(defun case-flip (num)
  "Change the case of the character under the cursor or region.
accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (if (mark)
      (let ((end (max (mark) (point)))
            (point-save (point)))
        (goto-char (min (point) (mark)))
        (while (not (eq (point) end))
          (funcall (if (<= ?a (following-char))
                       'upcase-region 'downcase-region)
                   (point) (1+ (point)))
          (forward-char 1))
        (goto-char point-save))
    (progn
      (if (string= tpu-direction-string " backup")
          (backward-char num))
      (while (> num 0)
        (funcall (if (<= ?a (following-char))
                     'upcase-region 'downcase-region)
                 (point) (1+ (point)))
        (forward-char 1)
        (setq num (1- num))))))

(defun indent-or-fill-region nil
  "fill region in text modes, indent region in programming language modes."
  (interactive)
  (if (string= paragraph-start "^$\\|^")
      (indent-region (point) (mark) nil)
    (fill-region (point) (mark))))

(defun mark-section-wisely nil
  "mark the section in a manner consistent with the major-mode.
uses mark-defun for emacs-lisp, lisp,
mark-c-function for c,
and mark-paragraph for other modes."
  (interactive)
  (cond  ((eq major-mode 'emacs-lisp-mode)
          (mark-defun))
         ((eq major-mode 'lisp-mode)
          (mark-defun))
         ((eq major-mode 'c-mode)
          (mark-c-function))
         (t (mark-paragraph))))

(defun update-mode-line nil
  "make sure mode-line in the current buffer reflects all changes."
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun reset-screen-size (height width)
  "sets screen size."
  (interactive "nnew screen height: \nnnew screen width: ")
  (set-screen-height height)
  (set-screen-width width))

(defun toggle-newline-and-indent nil
  "toggle between 'newline and indent' and 'simple newline'."
  (interactive)
  (cond (newline-and-indent-p
         (setq newline-and-indent-p-string "")
         (setq newline-and-indent-p nil)
         (global-set-key "\c-m" 'newline))
        (t
         (setq newline-and-indent-p-string " autoindent")
         (setq newline-and-indent-p t)
         (global-set-key "\c-m" 'newline-and-indent)))
  (update-mode-line))

(defun spell-check nil
  "checks the spelling of the region, or of the entire buffer if no
 region is selected."
  (interactive)
  (cond ((= (point) (mark)) (spell-buffer))
        (t (spell-region (point) (mark)))))
  
(defun report-position nil
  "prints a message in the minibuffer that shows the current position
 of the point in the buffer."
  (interactive)
  (message "char %d   line %d   %d%%"
           (point)
           (1+ (count-lines 1 (point)))
           (/ (* 100 (point)) (point-max))))

(defun unset-mark-command nil
  "Remove current mark."
  (interactive)
  (setq mark-ring nil)
  (set-mark nil)
  (message "Mark unset."))

(defun quit-emacs-now nil
  "Go away.  Now.  Just  g o  a w a y."
  (interactive)
  (kill-emacs t))


;;;
;;;  Auto-insert
;;;
(defun insert-escape nil
  "Inserts an escape character, and so becomes the escape-key alias."
  (interactive)
  (insert "\e"))

(defun insert-formfeed nil
  "Inserts a formfeed character."
  (interactive)
  (insert "\C-L"))


;;;
;;;  Define key
;;;
(defun end-define-macro-key nil
  "Ends the current macro definition"
  (interactive)
  (end-kbd-macro nil)
  (global-set-key defining-key last-kbd-macro)
  (global-set-key "\eOM" 'newline))

(defun define-macro-key (key)
  "Bind a set of keystrokes to a single key."
  (interactive "kKey to define: ")
  (setq defining-key key)
  (global-set-key "\eOM" 'end-define-macro-key)
  (start-kbd-macro nil))


;;;
;;;  Counter
;;;
(defun set-counter nil
  "Set the internal counter to the repeat count."
  (interactive)
  (message "Counter set to %d."
	   (setq tpu-counter current-prefix-arg)))

(defun region-length-to-counter nil
  "Sets the counter to the length of the region."
  (interactive)
  (cond ((mark)
	 (message "Counter set to %d."
		  (setq tpu-counter
			(- (max (point) (mark)) (min (point) (mark))))))
	(t (message "The mark is not set now; no select range is active."))))

(defun decr-counter nil
  "Decrement the internal counter by the repeat count,
 or by one if no repeat count is defined."
 (interactive)
 (message "Counter set to %d."
	  (setq tpu-counter (- tpu-counter (or current-prefix-arg 1)))))

(defun incr-counter nil
  "Increment the internal counter by the repeat count,
 or by one if no repeat count is defined."
 (interactive)
 (message "Counter set to %d."
	  (setq tpu-counter (+ tpu-counter (or current-prefix-arg 1)))))

(defun change-counter-format (fmt)
  "Changes the display format for use when inserting the internal tpu counter.
 You may use standard C format expressions."
  (interactive "sFormat (C-like):")
  (setq tpu-counter-format fmt))

(defun insert-counter nil
  "Inserts the internal tpu counter using the tpu counter format."
  (interactive)
  (insert (format tpu-counter-format tpu-counter)))

(defun insert-counter-as-char nil
  "Inserts the character value (mod 256) of the internal tpu counter."
  (interactive)
  (insert (mod tpu-counter 256)))


;;;
;;;  Buffer
;;;
(defun set-buffer-clean nil
  "Toggles the update mode of the buffer."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun kill-this-buffer nil
  "Kills the current buffer, but ask first."
  (interactive)
  (if (y-or-n-p (format "Really kill %s? " (current-buffer)))
      (kill-buffer (current-buffer))))

(defun kill-this-buffer-now nil
  "Kills the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun save-all-buffers-kill-emacs nil
  "Save all buffers and exit emacs."
  (interactive)
  (setq trim-versions-without-asking t)
  (save-buffers-kill-emacs t))

(defun write-current-buffers nil
  "Save all modified buffers without exiting."
  (interactive)
  (save-some-buffers t))

(defun next-buffer nil
  "Go to next buffer in ring."
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

(defun switch-to-last-buffer nil
 "Go to most recent buffer in ring."
  (interactive)
  (switch-to-buffer (cadr (buffer-list))))


;;;
;;;  Repeat count
;;;
(defun repeat-command-0 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 48 nil nil))

(defun repeat-command-1 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 49 nil nil))

(defun repeat-command-2 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 50 nil nil))

(defun repeat-command-3 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 51 nil nil))

(defun repeat-command-4 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 52 nil nil))

(defun repeat-command-5 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 53 nil nil))

(defun repeat-command-6 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 54 nil nil))

(defun repeat-command-7 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 55 nil nil))

(defun repeat-command-8 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 56 nil nil))

(defun repeat-command-9 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 57 nil nil))

(defun repeat-command-- nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal 45 nil nil))


;;;
;;;  Search
;;;
(defun tpu-search-forward (pat)
  "Search for a string."
  (interactive "sSearch: ") 
  (setq tpu-search-last-string
        (if (not (string= "" pat)) pat (read-string "Search: ")))
  (search-forward tpu-search-last-string))

(defun tpu-search-backward (pat)
  "Search for a string."
  (interactive "sSearch: ")
  (setq tpu-search-last-string
        (if (not (string= "" pat)) pat (read-string "Search: ")))
  (search-backward tpu-search-last-string))

(defun search-again-forward nil
  "Search for the same string as last time."
  (interactive)
  (tpu-search-forward tpu-search-last-string))

(defun search-again-backward nil
  "Search for the same string as last time."
  (interactive)
  (tpu-search-backward tpu-search-last-string))


;;;
;;;  Delete
;;;
(defun delete-current-line (num)
  "Delete one or specified number of lines after point.
This includes the newline character at the end of each line.
They are saved for the TPU undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
        (insert "\n"))
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-eol (num)
  "Delete text up to end of line.
With argument, delete up to to Nth line-end past point.
They are saved for the TPU undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-bol (num)
  "Delete text back to beginning of line.
With argument, delete up to to Nth line-end past point.
They are saved for the TPU undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (backward-char 1)
    (beginning-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring (point) beg))
    (delete-region (point) beg)))

(defun delete-current-word (num)
  "Delete one or specified number of words after point.
They are saved for the TPU undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word num)
    (setq tpu-last-deleted-words
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-previous-word (num)
  "Delete one or specified number of words before point.
They are saved for the TPU undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word (- num))
    (setq tpu-last-deleted-words
          (buffer-substring (point) beg))
    (delete-region beg (point))))

(defun delete-current-char (num)
  "Delete one or specified number of characters after point.
They are saved for the TPU undelete-chars command."
  (interactive "p")
  (setq tpu-last-deleted-chars
        (buffer-substring (point) (min (point-max) (+ (point) num))))
  (delete-region (point) (min (point-max) (+ (point) num))))

(defun delete-previous-char (num)
  "Delete one or specified number of characters before point.
They are saved for the TPU undelete-chars command."
  (interactive "p")
  (setq tpu-last-deleted-chars
        (buffer-substring (max (point-min) (- (point) num)) (point)))
  (delete-region (max (point-min) (- (point) num)) (point)))

(defun append-region nil
  "Deletes selected region and appends it to the current kill ring entry."
  (interactive)
  (append-next-kill)
  (kill-region (mark) (point)))


;;;
;;;  Undelete
;;;
(defun replace nil
  "Deletes text in selected range and replaces it with contents of kill ring."
  (interactive)
  (setq tpu-last-replaced-text
        (buffer-substring (point) (mark)))
  (delete-region (point) (mark))
  (yank))

(defun undelete-lines nil
  "Yank lines deleted by last TPU line-deletion command."
  (interactive)
  (insert tpu-last-deleted-lines))

(defun undelete-words nil
  "Yank words deleted by last TPU word-deletion command."
  (interactive)
  (insert tpu-last-deleted-words))

(defun undelete-chars nil
  "Yank characters deleted by last TPU character-deletion command."
  (interactive)
  (insert tpu-last-deleted-chars))


;;;
;;;  Position
;;;
(defun next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (forward-char)
  (end-of-line num))

(defun next-beginning-of-line (num)
  "Move to beginning of line; if at beginning, move to beginning of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (backward-char)
  (beginning-of-line num))

(defun previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num)))

(defun forward-to-word (num)
  "Move to next word-beginning, or to Nth following word-beginning."
  (interactive "p")
  (forward-word (1+ num))
  (forward-word -1))

(defun backward-to-word (num)
  "Move back to word-end, or to Nth word-end seen."
  (interactive "p")
  (forward-word (- (1+ num)))
  (forward-word 1))

(defun backward-line (num)
  "Move point to start of previous line.
Prefix argument serves as repeat-count."
  (interactive "p")
  (forward-line (- num)))

(defun next-paragraph (num)
  "Move to beginning of the next indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (next-line 1)
    (forward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun previous-paragraph (num)
  "Move to beginning of previous indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (backward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun move-to-beginning nil
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min)))

(defun move-to-end nil
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max)))

(defun goto-percent (perc)
  "Move point to ARG percentage of the buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> perc 100) (< perc 0))
      (error "Percentage %d out of range 0 < percent < 100" perc)
    (goto-char (/ (* (point-max) perc) 100))))


;;;
;;;  Window
;;;
(defun scroll-window-down (num)
  "Scroll the display down a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-down (- (* (window-height) num) 2)))

(defun scroll-window-up (num)
  "Scroll the display up a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-up (- (* (window-height) num) 2)))

(defun beginning-of-window nil
  "Home cursor to top of window."
  (interactive)
  (move-to-window-line 0))

(defun line-to-bottom-of-window nil
  "Move the current line to the bottom of the window."
  (interactive)
  (recenter -1))

(defun line-to-top-of-window nil
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))

;;;
;;;  Keyboard-style specific
;;;
(defun DEC-mode nil
  "Defines the DEC style keyboard."
  (interactive)
  (define-key global-map "\C-h" 'next-beginning-of-line)        ; ^H (BS)
  (define-key CSI-map "C" 'backward-char)                       ; right
  (define-key CSI-map "D" 'forward-char)                        ; left
  )
(defun SGI-mode nil
  "Defines the SGI style keyboard."
  (interactive)
  (define-key global-map "\C-h" 'backward-delete-char)          ; ^H (BS)
  (define-key CSI-map "C" 'forward-char)                        ; left
  (define-key CSI-map "D" 'backward-char)                       ; right
  (define-key CSI-map "001q" 'bob-expand)                       ; F1 IRIS4D
  )

;;;
;;;  Direction
;;;
(defun advance-direction nil
  "Set TPU Advance mode so keypad commands move forward."
  (interactive)
  (setq tpu-direction-string " advance")
  (define-key CSI-map "1~" 'isearch-forward)                    ; Find
  (define-key SS3-map "R" 'search-again-forward)                ; PF3
  (define-key APPLE-map "J\C-M" 'search-again-forward)          ; Apple F11
  (define-key SS3-map "p" 'forward-line)                        ; KP0
  (define-key SS3-map "q" 'forward-to-word)                     ; KP1
  (define-key SS3-map "r" 'next-end-of-line)                    ; KP2
  (define-key SS3-map "s" 'forward-char)                        ; KP3
  (define-key SS3-map "w" 'next-paragraph)                      ; KP7
  (define-key SS3-map "x" 'scroll-window-up)                    ; KP8
  (define-key GOLD-map "'" 'isearch-forward-regexp)             ; '
  (define-key GOLD-map "\"" 'isearch-forward-regexp)            ; "
  (define-key GOLD-SS3-map "R" 'tpu-search-forward)             ; PF3
  (define-key GOLD-APPLE-map "J\C-M" 'tpu-search-forward)       ; Apple F11
  (update-mode-line))

(defun backup-direction nil
  "Set TPU Backup mode so keypad commands move backward."
  (interactive)
  (setq tpu-direction-string " backup")
  (define-key CSI-map "1~" 'isearch-backward)                   ; Find
  (define-key SS3-map "R" 'search-again-backward)               ; PF3
  (define-key APPLE-map "J\C-M" 'search-again-backward)         ; Apple F11
  (define-key SS3-map "p" 'backward-line)                       ; KP0
  (define-key SS3-map "q" 'backward-to-word)                    ; KP1
  (define-key SS3-map "r" 'previous-end-of-line)                ; KP2
  (define-key SS3-map "s" 'backward-char)                       ; KP3
  (define-key SS3-map "w" 'previous-paragraph)                  ; KP7
  (define-key SS3-map "x" 'scroll-window-down)                  ; KP8
  (define-key GOLD-map "'" 'isearch-backward-regexp)            ; '
  (define-key GOLD-map "\"" 'isearch-backward-regexp)           ; "
  (define-key GOLD-SS3-map "R" 'tpu-search-backward)            ; PF3
  (define-key GOLD-APPLE-map "J\C-M" 'tpu-search-backward)      ; Apple F11
  (update-mode-line))


;;;
;;;  APPLE-map key definitions
;;;
(define-key APPLE-map "@\C-M" 'undo)                            ; APPLE F1
(define-key APPLE-map "A\C-M" 'kill-region)                     ; APPLE F2
(define-key APPLE-map "B\C-M" 'copy-region-as-kill)             ; APPLE F3
(define-key APPLE-map "C\C-M" 'yank)                            ; APPLE F4
(define-key APPLE-map "D\C-M" 'goto-breadcrumb-2)               ; APPLE F5
(define-key APPLE-map "E\C-M" 'goto-breadcrumb-3)               ; APPLE F6
(define-key APPLE-map "F\C-M" 'goto-breadcrumb-4)               ; APPLE F7
(define-key APPLE-map "G\C-M" 'goto-breadcrumb-5)               ; APPLE F8
;                                                               ; APPLE F9
(define-key APPLE-map "I\C-M" 'describe-key)                    ; APPLE F10
;                                                               ; APPLE F11
(define-key APPLE-map "K\C-M" 'delete-current-line)             ; APPLE F12
(define-key APPLE-map "L\C-M" nil)                              ; APPLE F13
(define-key APPLE-map "M\C-M" nil)                              ; APPLE F14
(define-key APPLE-map "N\C-M" nil)                              ; APPLE F15
(define-key APPLE-map "O\C-M" 'backward-delete-char)            ; APPLE Del
(define-key APPLE-map "P\C-M" 'move-to-end)                     ; APPLE End
(define-key APPLE-map "Q\C-M" 'scroll-window-up)                ; APPLE Page Down
(define-key APPLE-map "R\C-M" 'apropos)                         ; APPLE Help
(define-key APPLE-map "S\C-M" 'move-to-beginning)               ; APPLE Home
(define-key APPLE-map "T\C-M" 'scroll-window-down)              ; APPLE Page Up


;;;
;;;  GOLD-APPLE-map key definitions
;;;
(define-key GOLD-APPLE-map "@\C-M" nil)                         ; APPLE F1
(define-key GOLD-APPLE-map "A\C-M" nil)                         ; APPLE F2
(define-key GOLD-APPLE-map "B\C-M" nil)                         ; APPLE F3
(define-key GOLD-APPLE-map "C\C-M" nil)                         ; APPLE F4
(define-key GOLD-APPLE-map "D\C-M" 'drop-breadcrumb-2)          ; APPLE F5
(define-key GOLD-APPLE-map "E\C-M" 'drop-breadcrumb-3)          ; APPLE F6
(define-key GOLD-APPLE-map "F\C-M" 'drop-breadcrumb-4)          ; APPLE F7
(define-key GOLD-APPLE-map "G\C-M" 'drop-breadcrumb-5)          ; APPLE F8
(define-key GOLD-APPLE-map "H\C-M" 'mark-section-wisely)        ; APPLE F9
(define-key GOLD-APPLE-map "I\C-M" 'eval-expression)            ; APPLE F10
;                                                               ; APPLE F11
(define-key GOLD-APPLE-map "K\C-M" 'undelete-lines)             ; APPLE F12
(define-key GOLD-APPLE-map "L\C-M" nil)                         ; APPLE F13
(define-key GOLD-APPLE-map "M\C-M" nil)                         ; APPLE F14
(define-key GOLD-APPLE-map "N\C-M" nil)                         ; APPLE F15
(define-key GOLD-APPLE-map "O\C-M" 'backward-delete-char)       ; APPLE Del
(define-key GOLD-APPLE-map "P\C-M" 'move-to-end)                ; APPLE End
(define-key GOLD-APPLE-map "Q\C-M" 'scroll-window-up)           ; APPLE Page Down
(define-key GOLD-APPLE-map "R\C-M" 'apropos)                    ; APPLE Help
(define-key GOLD-APPLE-map "S\C-M" 'move-to-beginning)          ; APPLE Home
(define-key GOLD-APPLE-map "T\C-M" 'scroll-window-down)         ; APPLE Page Up


;;;
;;;  CSI-map key definitions
;;;
(define-key CSI-map "A" 'previous-line)                         ; up
(define-key CSI-map "B" 'next-line)                             ; down
;; left and right are defined in DEC-mode and SGI-mode
(define-key CSI-map "H" 'move-to-beginning)                     ; Home IRIS4D
(define-key CSI-map "P" 'backward-delete-char)                  ; Delete IRIS4D
(define-key CSI-map "2~" 'yank)                                 ; Insert Here
(define-key CSI-map "3~" 'kill-region)                          ; Remove
(define-key CSI-map "4~" 'set-mark-command)                     ; Select
(define-key CSI-map "5~" 'scroll-window-down)                   ; Prev Screen
(define-key CSI-map "6~" 'scroll-window-up)                     ; Next Screen
(define-key CSI-map "11~" 'nil)                                 ; F1
(define-key CSI-map "12~" 'nil)                                 ; F2
(define-key CSI-map "13~" 'nil)                                 ; F3
(define-key CSI-map "14~" 'nil)                                 ; F4
(define-key CSI-map "15~" 'nil)                                 ; F5
(define-key CSI-map "17~" 'nil)                                 ; F6
(define-key CSI-map "18~" 'nil)                                 ; F7
(define-key CSI-map "19~" 'nil)                                 ; F8
(define-key CSI-map "20~" 'nil)                                 ; F9
(define-key CSI-map "21~" 'nil)                                 ; F10
(define-key CSI-map "23~" 'insert-escape)                       ; F11 (ESC)
(define-key CSI-map "24~" 'next-beginning-of-line)              ; F12 (BS)
(define-key CSI-map "25~" 'delete-previous-word)                ; F13 (LF)
(define-key CSI-map "26~" 'nil)                                 ; F14
(define-key CSI-map "28~" 'apropos)                             ; HELP
(define-key CSI-map "29~" 'bob-expand)                          ; DO
(define-key CSI-map "31~" 'goto-breadcrumb-2)                   ; F17
(define-key CSI-map "32~" 'goto-breadcrumb-3)                   ; F18
(define-key CSI-map "33~" 'goto-breadcrumb-4)                   ; F19
(define-key CSI-map "34~" 'goto-breadcrumb-5)                   ; F20
(define-key CSI-map "001q" 'SGI-mode)                           ; F1 IRIS4D
(define-key CSI-map "002q" nil)                                 ; F2 IRIS4D
(define-key CSI-map "003q" nil)                                 ; F3 IRIS4D
(define-key CSI-map "004q" nil)                                 ; F4 IRIS4D
(define-key CSI-map "005q" nil)                                 ; F5 IRIS4D
(define-key CSI-map "006q" nil)                                 ; F6 IRIS4D
(define-key CSI-map "007q" nil)                                 ; F7 IRIS4D
(define-key CSI-map "008q" nil)                                 ; F8 IRIS4D
(define-key CSI-map "009q" nil)                                 ; F9 IRIS4D*
(define-key CSI-map "010q" nil)                                 ; F10 IRIS4D*
(define-key CSI-map "011q" nil)                                 ; F11 IRIS4D*
(define-key CSI-map "012q" nil)                                 ; F12 IRIS4D*
(define-key CSI-map "139q" 'yank)                               ; Insert IRIS4D
(define-key CSI-map "146q" 'move-to-end)                        ; End IRIS4D
(define-key CSI-map "150q" 'scroll-window-down)                 ; PgUp IRIS4D
(define-key CSI-map "154q" 'scroll-window-up)                   ; PgDn IRIS4D
(define-key CSI-map "209q" nil)                                 ; PrScr IRIS4D
(define-key CSI-map "213q" nil)                                 ; ScrLck IRIS4D
(define-key CSI-map "217q" nil)                                 ; Pause IRIS4D


;;;
;;;  SS3-map key definitions
;;;
(define-key SS3-map "A" 'previous-line)                         ; up
(define-key SS3-map "B" 'next-line)                             ; down
(define-key SS3-map "C" 'forward-char)                          ; right
(define-key SS3-map "D" 'backward-char)                         ; left
(define-key SS3-map "Q" 'describe-key)                          ; PF2
(define-key SS3-map "S" 'delete-current-line)                   ; PF4
(define-key SS3-map "X" 'describe-key)                          ; Apple KP=
(define-key SS3-map "j" 'delete-current-line)                   ; Apple KP*
(define-key SS3-map "k" 'delete-current-char)                   ; Apple KP+
(define-key SS3-map "l" 'delete-current-char)                   ; KP,
(define-key SS3-map "m" 'delete-current-word)                   ; KP-
(define-key SS3-map "n" 'set-mark-command)                      ; KP.
(define-key SS3-map "t" 'advance-direction)                     ; KP4
(define-key SS3-map "u" 'backup-direction)                      ; KP5
(define-key SS3-map "v" 'kill-region)                           ; KP6
(define-key SS3-map "y" 'append-region)                         ; KP9


;;;
;;;  GOLD-map key definitions
;;;
;(define-key GOLD-map "\C-A" 'nil)                               ; ^A
(define-key GOLD-map "\C-B" 'nil)                               ; ^B
(define-key GOLD-map "\C-C" 'nil)                               ; ^C
(define-key GOLD-map "\C-D" 'nil)                               ; ^D
(define-key GOLD-map "\C-E" 'nil)                               ; ^E
(define-key GOLD-map "\C-F" 'nil)                               ; ^F
(define-key GOLD-map "\C-g" 'keyboard-quit)                     ; safety first
(define-key GOLD-map "\C-h" 'delete-other-windows)              ; BS
(define-key GOLD-map "\C-i" 'other-window)                      ; TAB
;(define-key GOLD-map "\C-J" 'nil)                               ; ^J
(define-key GOLD-map "\C-K" 'nil)                               ; ^K
(define-key GOLD-map "\C-l" 'downcase-region)                   ; ^L
(define-key GOLD-map "\C-M" 'nil)                               ; ^M
(define-key GOLD-map "\C-N" 'nil)                               ; ^N
(define-key GOLD-map "\C-O" 'nil)                               ; ^O
(define-key GOLD-map "\C-P" 'nil)                               ; ^P
(define-key GOLD-map "\C-Q" 'nil)                               ; ^Q
(define-key GOLD-map "\C-R" 'nil)                               ; ^R
(define-key GOLD-map "\C-S" 'nil)                               ; ^S
(define-key GOLD-map "\C-T" 'nil)                               ; ^T
(define-key GOLD-map "\C-u" 'upcase-region)                     ; ^U
(define-key GOLD-map "\C-V" 'nil)                               ; ^V
(define-key GOLD-map "\C-w" 'write-current-buffers)             ; ^W
(define-key GOLD-map "\C-X" 'nil)                               ; ^X
(define-key GOLD-map "\C-Y" 'nil)                               ; ^Y
(define-key GOLD-map "\C-Z" 'nil)                               ; ^Z
(define-key GOLD-map " " 'undo)                                 ; SPC
(define-key GOLD-map "!" 'change-counter-format)                ; !
(define-key GOLD-map "#" 'insert-counter)                       ; #
(define-key GOLD-map "$" nil)                                   ; $
(define-key GOLD-map "%" 'goto-percent)                         ; %
(define-key GOLD-map "&" nil)                                   ; &
(define-key GOLD-map "(" nil)                                   ; (
(define-key GOLD-map ")" nil)                                   ; )
(define-key GOLD-map "*" 'insert-counter-as-char)               ; *
(define-key GOLD-map "+" 'region-length-to-counter)             ; +
(define-key GOLD-map "," 'goto-breadcrumb)                      ; ,
(define-key GOLD-map "-" 'repeat-command--)                     ; -
(define-key GOLD-map "." 'drop-breadcrumb)                      ; .
(define-key GOLD-map "/" 'query-replace)                        ; /
(define-key GOLD-map "0" 'repeat-command-0)                     ; 0
(define-key GOLD-map "1" 'repeat-command-1)                     ; 1
(define-key GOLD-map "2" 'repeat-command-2)                     ; 2
(define-key GOLD-map "3" 'repeat-command-3)                     ; 3
(define-key GOLD-map "4" 'repeat-command-4)                     ; 4
(define-key GOLD-map "5" 'repeat-command-5)                     ; 5
(define-key GOLD-map "6" 'repeat-command-6)                     ; 6
(define-key GOLD-map "7" 'repeat-command-7)                     ; 7
(define-key GOLD-map "8" 'repeat-command-8)                     ; 8
(define-key GOLD-map "9" 'repeat-command-9)                     ; 9
(define-key GOLD-map ":" nil)                                   ; :
(define-key GOLD-map ";" nil)                                   ; ;
(define-key GOLD-map "<" 'decr-counter)                         ; <
(define-key GOLD-map "=" 'set-counter)                          ; =
(define-key GOLD-map ">" 'incr-counter)                         ; >
(define-key GOLD-map "?" 'spell-check)                          ; ?
(define-key GOLD-map "A" 'toggle-newline-and-indent)            ; A
(define-key GOLD-map "B" 'buffer-menu)                          ; B
(define-key GOLD-map "D" 'shell-command)                        ; D
(define-key GOLD-map "E" 'find-file-other-window)               ; E
(define-key GOLD-map "F" 'switch-to-buffer-other-window)        ; F
(define-key GOLD-map "H" 'bob-unexpand)                         ; H
(define-key GOLD-map "I" 'insert-file)                          ; I
(define-key GOLD-map "K" 'kill-this-buffer-now)                 ; K
(define-key GOLD-map "L" 'report-position)                      ; L
(define-key GOLD-map "M" 'switch-to-last-buffer)                ; M
(define-key GOLD-map "N" 'next-error)                           ; N
(define-key GOLD-map "O" 'shell-command)                        ; O
(define-key GOLD-map "P" 'lpr-buffer)                           ; P
(define-key GOLD-map "Q" 'quit-emacs-now)                       ; Q
(define-key GOLD-map "R" 'revert-file)                          ; R
(define-key GOLD-map "S" 'replace-string)                       ; S
(define-key GOLD-map "U" 'set-buffer-clean)                     ; U
(define-key GOLD-map "V" 'find-file-other-window)               ; V
(define-key GOLD-map "W" 'other-window)                         ; W
(define-key GOLD-map "X" 'save-buffers-kill-emacs)              ; X
(define-key GOLD-map "Y" 'copy-region-as-kill)                  ; Y
(define-key GOLD-map "Z" 'suspend-emacs)                        ; Z
(define-key GOLD-map "[" 'blink-matching-open)                  ; [
(define-key GOLD-map "\\" nil)                                  ; \
(define-key GOLD-map "]" 'blink-matching-open)                  ; ]
(define-key GOLD-map "^" nil)                                   ; ^
(define-key GOLD-map "_" 'split-window-vertically)              ; -
(define-key GOLD-map "`" 'what-line)                            ; `
(define-key GOLD-map "a" 'toggle-newline-and-indent)            ; a
(define-key GOLD-map "b" 'buffer-menu)                          ; b
(define-key GOLD-map "d" 'shell-command)                        ; d
(define-key GOLD-map "e" 'find-file)                            ; e
(define-key GOLD-map "f" 'switch-to-buffer)                     ; f
(define-key GOLD-map "h" 'bob-expand)                           ; h
(define-key GOLD-map "i" 'insert-file)                          ; i
(define-key GOLD-map "k" 'kill-this-buffer)                     ; k
(define-key GOLD-map "l" 'goto-line)                            ; l
(define-key GOLD-map "m" 'switch-to-last-buffer)                ; m
(define-key GOLD-map "n" 'next-error)                           ; n
(define-key GOLD-map "o" 'shell-command)                        ; o
(define-key GOLD-map "p" 'lpr-region)                           ; p
(define-key GOLD-map "q" 'kill-emacs)                           ; q
(define-key GOLD-map "r" 'revert-file)                          ; r
(define-key GOLD-map "s" 'query-replace)                        ; s
(define-key GOLD-map "u" 'set-buffer-clean)                     ; u
(define-key GOLD-map "v" 'find-file-other-window)               ; v
(define-key GOLD-map "w" 'next-buffer)                          ; w
(define-key GOLD-map "x" 'save-all-buffers-kill-emacs)          ; x
(define-key GOLD-map "y" 'copy-region-as-kill)                  ; y
(define-key GOLD-map "z" 'suspend-emacs)                        ; z
(define-key GOLD-map "{" 'nil)                                  ; {
(define-key GOLD-map "|" 'split-window-horizontally)            ; |
(define-key GOLD-map "}" 'nil)                                  ; }
(define-key GOLD-map "~" 'exchange-point-and-mark)              ; ~
(define-key GOLD-map "\177" 'delete-window)                     ; <X]

;;;
;;;  GOLD-CSI-map key definitions
;;;
(define-key GOLD-CSI-map "11~" 'nil)                            ; F1
(define-key GOLD-CSI-map "12~" 'nil)                            ; F2
(define-key GOLD-CSI-map "13~" 'nil)                            ; F3
(define-key GOLD-CSI-map "14~" 'nil)                            ; F4
(define-key GOLD-CSI-map "16~" 'nil)                            ; F5
(define-key GOLD-CSI-map "17~" 'nil)                            ; F6
(define-key GOLD-CSI-map "18~" 'nil)                            ; F7
(define-key GOLD-CSI-map "19~" 'nil)                            ; F8
(define-key GOLD-CSI-map "20~" 'nil)                            ; F9
(define-key GOLD-CSI-map "21~" 'nil)                            ; F10
(define-key GOLD-CSI-map "23~" 'nil)                            ; F11
(define-key GOLD-CSI-map "24~" 'nil)                            ; F12
(define-key GOLD-CSI-map "25~" 'nil)                            ; F13
(define-key GOLD-CSI-map "26~" 'nil)                            ; F14
(define-key GOLD-CSI-map "28~" 'nil)                            ; HELP
(define-key GOLD-CSI-map "29~" 'bob-unexpand)                   ; DO
(define-key GOLD-CSI-map "31~" 'drop-breadcrumb-2)              ; F17
(define-key GOLD-CSI-map "32~" 'drop-breadcrumb-3)              ; F18
(define-key GOLD-CSI-map "33~" 'drop-breadcrumb-4)              ; F19
(define-key GOLD-CSI-map "34~" 'drop-breadcrumb-5)              ; F20
(define-key GOLD-CSI-map "A" 'scroll-window-down)               ; up-arrow
(define-key GOLD-CSI-map "B" 'scroll-window-up)                 ; down-arrow
(define-key GOLD-CSI-map "C" 'reset-screen-size)                ; right-arrow
(define-key GOLD-CSI-map "D" 'reset-screen-size)                ; left-arrow
(define-key GOLD-CSI-map "H" 'move-to-beginning)                ; Home IRIS4D
(define-key GOLD-CSI-map "P" 'backward-delete-char)             ; Delete IRIS4D
(define-key GOLD-CSI-map "001q" 'bob-unexpand)                  ; F1 IRIS4D
(define-key GOLD-CSI-map "002q" nil)                            ; F2 IRIS4D
(define-key GOLD-CSI-map "003q" nil)                            ; F3 IRIS4D
(define-key GOLD-CSI-map "004q" nil)                            ; F4 IRIS4D
(define-key GOLD-CSI-map "005q" nil)                            ; F5 IRIS4D
(define-key GOLD-CSI-map "006q" nil)                            ; F6 IRIS4D
(define-key GOLD-CSI-map "007q" nil)                            ; F7 IRIS4D
(define-key GOLD-CSI-map "008q" nil)                            ; F8 IRIS4D
(define-key GOLD-CSI-map "009q" nil)                            ; F9 IRIS4D*
(define-key GOLD-CSI-map "010q" nil)                            ; F10 IRIS4D*
(define-key GOLD-CSI-map "011q" nil)                            ; F11 IRIS4D*
(define-key GOLD-CSI-map "012q" nil)                            ; F12 IRIS4D*
(define-key GOLD-CSI-map "139q" 'yank)                          ; Insert IRIS4D
(define-key GOLD-CSI-map "146q" 'move-to-end)                   ; End IRIS4D
(define-key GOLD-CSI-map "150q" 'scroll-window-down)            ; PgUp IRIS4D
(define-key GOLD-CSI-map "154q" 'scroll-window-up)              ; PgDn IRIS4D
(define-key GOLD-CSI-map "209q" nil)                            ; PrScr IRIS4D
(define-key GOLD-CSI-map "213q" nil)                            ; ScrLck IRIS4D
(define-key GOLD-CSI-map "217q" nil)                            ; Pause IRIS4D

;;;
;;;  GOLD-SS3-map key definitions
;;;
(define-key GOLD-SS3-map "A" 'scroll-window-down)               ; up-arrow
(define-key GOLD-SS3-map "B" 'scroll-window-up)                 ; down-arrow
(define-key GOLD-SS3-map "C" 'reset-screen-size)                ; right-arrow
(define-key GOLD-SS3-map "D" 'reset-screen-size)                ; left-arrow
(define-key GOLD-SS3-map "P" 'mark-section-wisely)              ; PF1
(define-key GOLD-SS3-map "Q" 'eval-expression)                  ; PF2
(define-key GOLD-SS3-map "X" 'eval-expression)                  ; PF2
(define-key GOLD-SS3-map "S" 'undelete-lines)                   ; PF4
(define-key GOLD-SS3-map "j" 'undelete-lines)                   ; PF4
(define-key GOLD-SS3-map "p" 'open-line)                        ; KP0
(define-key GOLD-SS3-map "q" 'case-flip)                        ; KP1
(define-key GOLD-SS3-map "r" 'delete-to-eol)                    ; KP2
(define-key GOLD-SS3-map "s" 'copy-region-as-kill)              ; KP3
(define-key GOLD-SS3-map "t" 'move-to-end)                      ; KP4
(define-key GOLD-SS3-map "u" 'move-to-beginning)                ; KP5
(define-key GOLD-SS3-map "v" 'yank)                             ; KP6
(define-key GOLD-SS3-map "w" 'execute-extended-command)         ; KP7
(define-key GOLD-SS3-map "x" 'indent-or-fill-region)            ; KP8
(define-key GOLD-SS3-map "y" 'replace)                          ; KP9
(define-key GOLD-SS3-map "m" 'undelete-words)                   ; KP-
(define-key GOLD-SS3-map "l" 'undelete-chars)                   ; KP,
(define-key GOLD-SS3-map "k" 'undelete-chars)                   ; KP,
(define-key GOLD-SS3-map "n" 'unset-mark-command)               ; KP.

(defun use-tpu nil
  "Sets bob tpu emulation on."
  (interactive)
  (replace-global-key "\e[" CSI-map)
  (replace-global-key "\eP" DCS-map)
  (replace-global-key "\eO" SS3-map)
  (replace-global-key "\C-a" APPLE-map)
  (replace-global-key "\C-\\" 'quoted-insert)                  ; ^\
  (replace-global-key "\177" 'delete-previous-char)            ; <X]
  (replace-global-key "\C-f" 'rename-buffer)                   ; ^F
  (replace-global-key "\C-j" 'delete-previous-word)            ; ^J (LF)
  (replace-global-key "\C-k" 'define-macro-key)                ; ^K
  (replace-global-key "\C-l" 'insert-formfeed)                 ; ^L (FF)
  (replace-global-key "\C-r" 'recenter)                        ; ^R
  (replace-global-key "\C-u" 'delete-to-bol)                   ; ^U
  (define-key SS3-map "P" GOLD-map)
  (define-key APPLE-map "H\C-M" GOLD-map)
  (define-key GOLD-map "\e[" GOLD-CSI-map)
  (define-key GOLD-map "\eO" GOLD-SS3-map)
  (define-key GOLD-map "\C-A" GOLD-APPLE-map)
  (DEC-mode)
  (advance-direction)
  ;; Make direction of motion show in mode line
  ;; while TPU emulation is turned on.
  ;; Note that the keypad is always turned on when in Emacs.
  (or (assq 'tpu-direction-string minor-mode-alist)
      (setq minor-mode-alist (cons '(tpu-direction-string tpu-direction-string)
				   minor-mode-alist)))
  (or (assq 'newline-and-indent-p minor-mode-alist)
      (setq minor-mode-alist 
	    (cons '(newline-and-indent-p newline-and-indent-p-string)
		  minor-mode-alist))))

(defun cancel-tpu nil
  "Sets bob tpu emulation off."
  (interactive)
  (restore-global-key "\e[")
  (restore-global-key "\eP")
  (restore-global-key "\eO")
  (restore-global-key "\C-a")
  (restore-global-key "\C-\\")
  (restore-global-key "\177")
  (restore-global-key "\C-f")
  (restore-global-key "\C-h")
  (restore-global-key "\C-j")
  (restore-global-key "\C-k")
  (restore-global-key "\C-l")
  (restore-global-key "\C-r")
  (restore-global-key "\C-u")
  (setq tpu-direction-string nil))

(use-tpu)

;;;
;;;  Expansions for Bob-expand
;;;
;;;;
;;;;  Expansions for HOOPS
;;;;
(setq tpu-expansions 
      (list
       (list "HC_Abort_Program" "HC_Abort_Program();" -2)
       (list "HC_Append_Modelling_Matrix" "HC_Append_Modelling_Matrix();" -2)
       (list "HC_Await_Event" "HC_Await_Event();" -2)
       (list "HC_Bring_To_Front" "HC_Bring_To_Front();" -2)
       (list "HC_Bring_To_Front_By_Key" "HC_Bring_To_Front_By_Key();" -2)
       (list "HC_Check_For_Events" "HC_Check_For_Events();" -2)
       (list "HC_Clear_Display" "HC_Clear_Display();" -2)
       (list "HC_Close_Segment" "HC_Close_Segment();" -2)
       (list "HC_Copy_Segment" "HC_Copy_Segment(, );" -4)
       (list "HC_Create_Segment" "HC_Create_Segment();" -2)
       (list "HC_Define_Alias" "HC_Define_Alias(, );" -4)
       (list "HC_Define_Color" "HC_Define_Color(, , , );" -8)
       (list "HC_Define_Color_Name" "HC_Define_Color_Name(, , , );" -8)
       (list "HC_Define_Error_Handler" "HC_Define_Error_Handler();" -2)
       (list "HC_Define_Exit_Handler" "HC_Define_Exit_Handler();" -2)
       (list "HC_Delete_By_Key" "HC_Delete_By_Key();" -2)
       (list "HC_Delete_Segment" "HC_Delete_Segment();" -2)
       (list "HC_Disable_Button_Events" "HC_Disable_Button_Events(, );" -4)
       (list "HC_Disable_Location_Events" "HC_Disable_Location_Events(, , );" -6)
       (list "HC_Disable_Selection_Events" "HC_Disable_Selection_Events(, );" -4)
       (list "HC_Disable_String_Events" "HC_Disable_String_Events(, );" -4)
       (list "HC_Disable_Wakeup_Events" "HC_Disable_Wakeup_Events();" -2)
       (list "HC_Dolly_Camera" "HC_Dolly_Camera(, , );" -6)
       (list "HC_Edit_Pixel_Array" "HC_Edit_Pixel_Array(, , , , , );" -12)
       (list "HC_Edit_Polygon" "HC_Edit_Polygon(, , , , );" -10)
       (list "HC_Edit_Polyline" "HC_Edit_Polyline(, , , , );" -10)
       (list "HC_Edit_Text" "HC_Edit_Text(, , , , , );" -12)
       (list "HC_Enable_Button_Events" "HC_Enable_Button_Events(, );" -4)
       (list "HC_Enable_Location_Events" "HC_Enable_Location_Events(, , );" -6)
       (list "HC_Enable_Selection_Events" "HC_Enable_Selection_Events(, );" -4)
       (list "HC_Enable_String_Events" "HC_Enable_String_Events(, );" -4)
       (list "HC_Enable_Wakeup_Events" "HC_Enable_Wakeup_Events();" -2)
       (list "HC_Exit_Program" "HC_Exit_Program();" -2)
       (list "HC_Flush_All_Events" "HC_Flush_All_Events();" -2)
       (list "HC_Flush_By_Key" "HC_Flush_By_Key();" -2)
       (list "HC_Flush_Segment" "HC_Flush_Segment();" -2)
       (list "HC_Get_Button" "HC_Get_Button();" -2)
       (list "HC_Get_Location" "HC_Get_Location(, );" -4)
       (list "HC_Get_Selection" "HC_Get_Selection();" -2)
       (list "HC_Get_String" "HC_Get_String(, );" -4)
       (list "HC_Get_Wakeup" "HC_Get_Wakeup();" -2)
       (list "HC_Include_Segment" "HC_Include_Segment();" -2)
       (list "HC_Insert_Ink" "HC_Insert_Ink(, , );" -6)
       (list "HC_Insert_Line" "HC_Insert_Line(, , , , , );" -12)
       (list "HC_Insert_Marker" "HC_Insert_Marker(, , );" -6)
       (list "HC_Insert_Polygon" "HC_Insert_Polygon(, );" -4)
       (list "HC_Insert_Polyline" "HC_Insert_Polyline(, );" -4)
       (list "HC_Insert_Polygon_With_Normals" "HC_Insert_Polygon_With_Normals(, , );" -6)
       (list "HC_Insert_Text" "HC_Insert_Text(, , , );" -8)
       (list "HC_Insert_Distant_Light" "HC_Insert_Distant_Light(, , );" -6)
       (list "HC_Insert_Pixel_Array" "HC_Insert_Pixel_Array(, , , , , );" -12)
       (list "HC_Insert_Pixel_Array_By_Ref" "HC_Insert_Pixel_Array_By_Ref(, , , , , );" -12)
       (list "HC_KCopy_Segment" "HC_KCopy_Segment(, );" -4)
       (list "HC_KCreate_Segment" "HC_KCreate_Segment();" -2)
       (list "HC_KInclude_Segment" "HC_KInclude_Segment();" -2)
       (list "HC_KInsert_Ink" "HC_KInsert_Ink(, , );" -6)
       (list "HC_KInsert_Line" "HC_KInsert_Line(, , , , , );" -12)
       (list "HC_KInsert_Marker" "HC_KInsert_Marker(, , );" -6)
       (list "HC_KInsert_Polygon" "HC_KInsert_Polygon(, );" -4)
       (list "HC_KInsert_Polyline" "HC_KInsert_Polyline(, );" -4)
       (list "HC_KInsert_Polygon_With_Normals" "HC_KInsert_Polygon_With_Normals(, , );" -6)
       (list "HC_KInsert_String_Cursor" "HC_KInsert_String_Cursor(, , );" -6)
       (list "HC_KInsert_Text" "HC_KInsert_Text(, , , );" -8)
       (list "HC_KInsert_Block_Text" "HC_KInsert_Block_Text(, , , , );" -10)
       (list "HC_KInsert_Distant_Light" "HC_KInsert_Distant_Light(, , );" -6)
       (list "HC_KInsert_Pixel_Array" "HC_KInsert_Pixel_Array(, , , , , );" -12)
       (list "HC_KInsert_Pixel_Array_By_Ref" "HC_KInsert_Pixel_Array_By_Ref(, , , , , );" -12)
       (list "HC_KShow_Owner_By_Key" "HC_KShow_Owner_By_Key();" -2)
       (list "HC_Modify_Color_Map" "HC_Modify_Color_Map(, );" -4)
       (list "HC_Modify_Color_Map_By_Value" "HC_Modify_Color_Map_By_Value(, , , );" -8)
       (list "HC_Move_By_Key" "HC_Move_By_Key(, );" -4)
       (list "HC_Move_Distant_Light" "HC_Move_Distant_Light(, , , );" -8)
       (list "HC_Move_Pixel_Array" "HC_Move_Pixel_Array(, , , );" -8)
       (list "HC_Move_Segment" "HC_Move_Segment(, );" -4)
       (list "HC_Move_String_Cursor" "HC_Move_String_Cursor(, , );" -6)
       (list "HC_Open_Segment" "HC_Open_Segment();" -2)
       (list "HC_KOpen_Segment" "HC_KOpen_Segment();" -2)
       (list "HC_Open_Segment_By_Key" "HC_Open_Segment_By_Key();" -2)
       (list "HC_Orbit_Camera" "HC_Orbit_Camera(, );" -4)
       (list "HC_Pan_Camera" "HC_Pan_Camera(, );" -4)
       (list "HC_Parse_String" "HC_Parse_String(, , , );" -8)
       (list "HC_Pause" "HC_Pause();" -2)
       (list "HC_Print_Version" "HC_Print_Version();" -2)
       (list "HC_Queue_Special_Event" "HC_Queue_Special_Event(, );" -4)
       (list "HC_Rename_Segment" "HC_Rename_Segment(, );" -4)
       (list "HC_Render_Text" "HC_Render_Text(, , , );" -8)
       (list "HC_Report_Error" "HC_Report_Error(, , , , );" -10)
       (list "HC_Request_Location" "HC_Request_Location(, );" -4)
       (list "HC_Requeue_Event" "HC_Requeue_Event();" -2)
       (list "HC_Reset_System" "HC_Reset_System();" -2)
       (list "HC_Restart_Ink" "HC_Restart_Ink();" -2)
       (list "HC_Roll_Camera" "HC_Roll_Camera();" -2)
       (list "HC_Rotate_Object" "HC_Rotate_Object(, , );" -6)
       (list "HC_Rotate_Object_Offaxis" "HC_Rotate_Object_Offaxis(, , , );" -8)
       (list "HC_Scale_Object" "HC_Scale_Object(, , );" -6)
       (list "HC_Scroll_Text" "HC_Scroll_Text(, , );" -6)
       (list "HC_Set_Camera" "HC_Set_Camera(, , , , , );" -12)
       (list "HC_Set_Camera_By_Volume" "HC_Set_Camera_By_Volume(, , , , );" -10)
       (list "HC_Set_Camera_Field" "HC_Set_Camera_Field(, );" -4)
       (list "HC_Set_Camera_Position" "HC_Set_Camera_Position(, , );" -6)
       (list "HC_Set_Camera_Projection" "HC_Set_Camera_Projection();" -2)
       (list "HC_Set_Camera_Target" "HC_Set_Camera_Target(, , );" -6)
       (list "HC_Set_Camera_Up_Vector" "HC_Set_Camera_Up_Vector(, , );" -6)
       (list "HC_Set_Color" "HC_Set_Color();" -2)
       (list "HC_Set_Color_By_Index" "HC_Set_Color_By_Index(, );" -4)
       (list "HC_Set_Color_By_Value" "HC_Set_Color_By_Value(, , , , );" -10)
       (list "HC_Set_Color_Map" "HC_Set_Color_Map();" -2)
       (list "HC_Set_Color_Map_By_Value" "HC_Set_Color_Map_By_Value(, , );" -6)
       (list "HC_Set_Driver_Options" "HC_Set_Driver_Options();" -2)
       (list "HC_Set_Edge_Pattern" "HC_Set_Edge_Pattern();" -2)
       (list "HC_Set_Edge_Weight" "HC_Set_Edge_Weight();" -2)
       (list "HC_Set_Face_Pattern" "HC_Set_Face_Pattern();" -2)
       (list "HC_Set_Handedness" "HC_Set_Handedness();" -2)
       (list "HC_Set_Heuristics" "HC_Set_Heuristics();" -2)
       (list "HC_Set_Line_Pattern" "HC_Set_Line_Pattern();" -2)
       (list "HC_Set_Line_Weight" "HC_Set_Line_Weight();" -2)
       (list "HC_Set_Marker_Size" "HC_Set_Marker_Size();" -2)
       (list "HC_Set_Marker_Symbol" "HC_Set_Marker_Symbol();" -2)
       (list "HC_Set_Metafile" "HC_Set_Metafile();" -2)
       (list "HC_Set_Modelling_Matrix" "HC_Set_Modelling_Matrix();" -2)
       (list "HC_Set_Selectability" "HC_Set_Selectability();" -2)
       (list "HC_Set_Streaming_Mode" "HC_Set_Streaming_Mode();" -2)
       (list "HC_Set_Text_Alignment" "HC_Set_Text_Alignment();" -2)
       (list "HC_Set_Text_Font" "HC_Set_Text_Font();" -2)
       (list "HC_Set_Text_Path" "HC_Set_Text_Path(, , );" -6)
       (list "HC_Set_Text_Size" "HC_Set_Text_Size();" -2)
       (list "HC_Set_Text_Spacing" "HC_Set_Text_Spacing();" -2)
       (list "HC_Set_User_Options" "HC_Set_User_Options();" -2)
       (list "HC_Set_User_Value" "HC_Set_User_Value();" -2)
       (list "HC_Set_Visibility" "HC_Set_Visibility();" -2)
       (list "HC_Set_Window" "HC_Set_Window(, , , );" -8)
       (list "HC_Set_Window_Frame" "HC_Set_Window_Frame();" -2)
       (list "HC_Set_Window_Pattern" "HC_Set_Window_Pattern();" -2)
       (list "HC_Show_Alias" "HC_Show_Alias(, );" -4)
       (list "HC_Show_Alias_Count" "HC_Show_Alias_Count();" -2)
       (list "HC_Show_Button" "HC_Show_Button();" -2)
       (list "HC_Show_Button_Source" "HC_Show_Button_Source(, , );" -6)
       (list "HC_Show_Camera" "HC_Show_Camera(, , , , , );" -12)
       (list "HC_Show_Camera_By_Volume" "HC_Show_Camera_By_Volume(, , , , );" -10)
       (list "HC_Show_Camera_Field" "HC_Show_Camera_Field(, );" -4)
       (list "HC_Show_Camera_Position" "HC_Show_Camera_Position(, , );" -6)
       (list "HC_Show_Camera_Projection" "HC_Show_Camera_Projection();" -2)
       (list "HC_Show_Camera_Target" "HC_Show_Camera_Target(, , );" -6)
       (list "HC_Show_Camera_Up_Vector" "HC_Show_Camera_Up_Vector(, , );" -6)
       (list "HC_Show_Color" "HC_Show_Color();" -2)
       (list "HC_Show_Color_By_Index" "HC_Show_Color_By_Index(, );" -4)
       (list "HC_Show_Color_By_Value" "HC_Show_Color_By_Value(, , , , );" -10)
       (list "HC_Show_Color_Map" "HC_Show_Color_Map();" -2)
       (list "HC_Show_Color_Map_By_Value" "HC_Show_Color_Map_By_Value(, , );" -6)
       (list "HC_Show_Color_Map_Count" "HC_Show_Color_Map_Count();" -2)
       (list "HC_Show_Color_Name" "HC_Show_Color_Name(, );" -4)
       (list "HC_Show_Color_Name_Count" "HC_Show_Color_Name_Count();" -2)
       (list "HC_Show_Contents_Count" "HC_Show_Contents_Count();" -2)
       (list "HC_Show_Device_Info" "HC_Show_Device_Info(, , );" -6)
       (list "HC_Show_Distant_Light" "HC_Show_Distant_Light(, , , );" -8)
       (list "HC_Show_Driver_Options" "HC_Show_Driver_Options();" -2)
       (list "HC_Show_Edge_Pattern" "HC_Show_Edge_Pattern();" -2)
       (list "HC_Show_Edge_Weight" "HC_Show_Edge_Weight();" -2)
       (list "HC_Show_Environment" "HC_Show_Environment(, );" -4)
       (list "HC_Show_Face_Pattern" "HC_Show_Face_Pattern();" -2)
       (list "HC_Show_Handedness" "HC_Show_Handedness();" -2)
       (list "HC_Show_Heuristics" "HC_Show_Heuristics();" -2)
       (list "HC_Show_Include_Segment" "HC_Show_Include_Segment(, );" -4)
       (list "HC_Show_Key_Type" "HC_Show_Key_Type(, );" -4)
       (list "HC_Show_Line" "HC_Show_Line(, , , , , , );" -14)
       (list "HC_Show_Line_Pattern" "HC_Show_Line_Pattern();" -2)
       (list "HC_Show_Line_Weight" "HC_Show_Line_Weight();" -2)
       (list "HC_Show_Location" "HC_Show_Location(, );" -4)
       (list "HC_Show_Location_Source" "HC_Show_Location_Source(, , , );" -8)
       (list "HC_Show_Location_Status" "HC_Show_Location_Status();" -2)
       (list "HC_Show_Marker" "HC_Show_Marker(, , , );" -8)
       (list "HC_Show_Marker_Size" "HC_Show_Marker_Size();" -2)
       (list "HC_Show_Marker_Symbol" "HC_Show_Marker_Symbol();" -2)
       (list "HC_Show_Memory_Usage" "HC_Show_Memory_Usage(, );" -4)
       (list "HC_Show_Metafile" "HC_Show_Metafile();" -2)
       (list "HC_Show_Modelling_Matrix" "HC_Show_Modelling_Matrix();" -2)
       (list "HC_Show_Net_Camera" "HC_Show_Net_Camera(, , , , , );" -12)
       (list "HC_Show_Net_Camera_By_Volume" "HC_Show_Net_Camera_By_Volume(, , , , );" -10)
       (list "HC_Show_Net_Camera_Field" "HC_Show_Net_Camera_Field(, );" -4)
       (list "HC_Show_Net_Camera_Position" "HC_Show_Net_Camera_Position(, , );" -6)
       (list "HC_Show_Net_Camera_Projection" "HC_Show_Net_Camera_Projection();" -2)
       (list "HC_Show_Net_Camera_Target" "HC_Show_Net_Camera_Target(, , );" -6)
       (list "HC_Show_Net_Camera_Up_Vector" "HC_Show_Net_Camera_Up_Vector(, , );" -6)
       (list "HC_Show_Net_Color" "HC_Show_Net_Color();" -2)
       (list "HC_Show_Net_Color_By_Index" "HC_Show_Net_Color_By_Index(, );" -4)
       (list "HC_Show_Net_Color_By_Value" "HC_Show_Net_Color_By_Value(, , , , );" -10)
       (list "HC_Show_Net_Color_Map" "HC_Show_Net_Color_Map();" -2)
       (list "HC_Show_Net_Color_Map_By_Value" "HC_Show_Net_Color_Map_By_Value(, , );" -6)
       (list "HC_Show_Net_Color_Map_Count" "HC_Show_Net_Color_Map_Count();" -2)
       (list "HC_Show_Net_Driver_Options" "HC_Show_Net_Driver_Options();" -2)
       (list "HC_Show_Net_Edge_Pattern" "HC_Show_Net_Edge_Pattern();" -2)
       (list "HC_Show_Net_Edge_Weight" "HC_Show_Net_Edge_Weight();" -2)
       (list "HC_Show_Net_Face_Pattern" "HC_Show_Net_Face_Pattern();" -2)
       (list "HC_Show_Net_Handedness" "HC_Show_Net_Handedness();" -2)
       (list "HC_Show_Net_Heuristics" "HC_Show_Net_Heuristics();" -2)
       (list "HC_Show_Net_Line_Pattern" "HC_Show_Net_Line_Pattern();" -2)
       (list "HC_Show_Net_Line_Weight" "HC_Show_Net_Line_Weight();" -2)
       (list "HC_Show_Net_Marker_Size" "HC_Show_Net_Marker_Size();" -2)
       (list "HC_Show_Net_Marker_Symbol" "HC_Show_Net_Marker_Symbol();" -2)
       (list "HC_Show_Net_Metafile" "HC_Show_Net_Metafile();" -2)
       (list "HC_Show_Net_Modelling_Matrix" "HC_Show_Net_Modelling_Matrix();" -2)
       (list "HC_Show_Net_Selectability" "HC_Show_Net_Selectability();" -2)
       (list "HC_Show_Net_Streaming_Mode" "HC_Show_Net_Streaming_Mode();" -2)
       (list "HC_Show_Net_Text_Alignment" "HC_Show_Net_Text_Alignment();" -2)
       (list "HC_Show_Net_Text_Font" "HC_Show_Net_Text_Font();" -2)
       (list "HC_Show_Net_Text_Path" "HC_Show_Net_Text_Path(, , );" -6)
       (list "HC_Show_Net_Text_Size" "HC_Show_Net_Text_Size();" -2)
       (list "HC_Show_Net_Text_Spacing" "HC_Show_Net_Text_Spacing();" -2)
       (list "HC_Show_Net_User_Options" "HC_Show_Net_User_Options();" -2)
       (list "HC_Show_Net_User_Value" "HC_Show_Net_User_Value();" -2)
       (list "HC_Show_Net_Visibility" "HC_Show_Net_Visibility();" -2)
       (list "HC_Show_Net_Window" "HC_Show_Net_Window(, , , );" -8)
       (list "HC_Show_Net_Window_Frame" "HC_Show_Net_Window_Frame();" -2)
       (list "HC_Show_Net_Window_Pattern" "HC_Show_Net_Window_Pattern();" -2)
       (list "HC_Show_One_Color" "HC_Show_One_Color(, );" -4)
       (list "HC_Show_One_Color_By_Index" "HC_Show_One_Color_By_Index(, );" -4)
       (list "HC_Show_One_Color_By_Value" "HC_Show_One_Color_By_Value(, , , , );" -10)
       (list "HC_Show_One_Color_Map" "HC_Show_One_Color_Map(, );" -4)
       (list "HC_Show_One_Color_Map_By_Value" "HC_Show_One_Color_Map_By_Value(, , , , );" -10)
       (list "HC_Show_One_Driver_Option" "HC_Show_One_Driver_Option(, );" -4)
       (list "HC_Show_One_Heuristic" "HC_Show_One_Heuristic(, );" -4)
       (list "HC_Show_One_Net_Color" "HC_Show_One_Net_Color(, );" -4)
       (list "HC_Show_One_Net_Color_By_Index" "HC_Show_One_Net_Color_By_Index(, );" -4)
       (list "HC_Show_One_Net_Color_By_Value" "HC_Show_One_Net_Color_By_Value(, , , , );" -10)
       (list "HC_Show_One_Net_Color_Map" "HC_Show_One_Net_Color_Map(, );" -4)
       (list "HC_Show_One_Net_Color_Map_By_V" "HC_Show_One_Net_Color_Map_By_V(, , , , );" -10)
       (list "HC_Show_One_Net_Driver_Option" "HC_Show_One_Net_Driver_Option(, );" -4)
       (list "HC_Show_One_Net_Heuristic" "HC_Show_One_Net_Heuristic(, );" -4)
       (list "HC_Show_One_Net_Selectability" "HC_Show_One_Net_Selectability(, );" -4)
       (list "HC_Show_One_Net_Visibility" "HC_Show_One_Net_Visibility(, );" -4)
       (list "HC_Show_One_Selectability" "HC_Show_One_Selectability(, );" -4)
       (list "HC_Show_One_Visibility" "HC_Show_One_Visibility(, );" -4)
       (list "HC_Show_Open_Segment_Count" "HC_Show_Open_Segment_Count();" -2)
       (list "HC_Show_Owner" "HC_Show_Owner(, );" -4)
       (list "HC_Show_Owner_By_Key" "HC_Show_Owner_By_Key(, );" -4)
       (list "HC_Show_Partial_Pixel_Array" "HC_Show_Partial_Pixel_Array(, , , , , );" -12)
       (list "HC_Show_Partial_Polygon" "HC_Show_Partial_Polygon(, , , );" -8)
       (list "HC_Show_Partial_Polyline" "HC_Show_Partial_Polyline(, , , );" -8)
       (list "HC_Show_Pathname_Expansion" "HC_Show_Pathname_Expansion(, );" -4)
       (list "HC_Show_Pixel_Array" "HC_Show_Pixel_Array(, , , , , , );" -14)
       (list "HC_Show_Pixel_Array_Size" "HC_Show_Pixel_Array_Size(, , , , , );" -12)
       (list "HC_Show_Polygon" "HC_Show_Polygon(, , );" -6)
       (list "HC_Show_Polygon_Count" "HC_Show_Polygon_Count(, );" -4)
       (list "HC_Show_Polyline" "HC_Show_Polyline(, , );" -6)
       (list "HC_Show_Polyline_Count" "HC_Show_Polyline_Count(, );" -4)
       (list "HC_Show_Segment" "HC_Show_Segment(, );" -4)
       (list "HC_Show_Segment_Count" "HC_Show_Segment_Count();" -2)
       (list "HC_Show_Selectability" "HC_Show_Selectability();" -2)
       (list "HC_Show_Selection" "HC_Show_Selection();" -2)
       (list "HC_Show_Selection_Item" "HC_Show_Selection_Item(, , );" -6)
       (list "HC_Show_Selection_Location" "HC_Show_Selection_Location(, , , , );" -10)
       (list "HC_Show_Selection_Pathname" "HC_Show_Selection_Pathname();" -2)
       (list "HC_Show_Selection_Source" "HC_Show_Selection_Source(, , , );" -8)
       (list "HC_Show_Special_Event" "HC_Show_Special_Event(, );" -4)
       (list "HC_Show_Streaming_Mode" "HC_Show_Streaming_Mode();" -2)
       (list "HC_Show_String" "HC_Show_String();" -2)
       (list "HC_Show_String_Source" "HC_Show_String_Source(, );" -4)
       (list "HC_Show_Style_Segment" "HC_Show_Style_Segment(, );" -4)
       (list "HC_Show_Text" "HC_Show_Text(, , , , );" -10)
       (list "HC_Show_Text_Alignment" "HC_Show_Text_Alignment();" -2)
       (list "HC_Show_Text_Font" "HC_Show_Text_Font();" -2)
       (list "HC_Show_Text_Length" "HC_Show_Text_Length(, );" -4)
       (list "HC_Show_Text_Path" "HC_Show_Text_Path(, , );" -6)
       (list "HC_Show_Text_Size" "HC_Show_Text_Size();" -2)
       (list "HC_Show_Text_Spacing" "HC_Show_Text_Spacing();" -2)
       (list "HC_Show_Time" "HC_Show_Time();" -2)
       (list "HC_Show_User_Options" "HC_Show_User_Options();" -2)
       (list "HC_Show_User_Value" "HC_Show_User_Value();" -2)
       (list "HC_Show_Visibility" "HC_Show_Visibility();" -2)
       (list "HC_Show_Wakeup" "HC_Show_Wakeup();" -2)
       (list "HC_Show_Window" "HC_Show_Window(, , , );" -8)
       (list "HC_Show_Window_Frame" "HC_Show_Window_Frame();" -2)
       (list "HC_Show_Window_Pattern" "HC_Show_Window_Pattern();" -2)
       (list "HC_Style_Segment" "HC_Style_Segment();" -2)
       (list "HC_KStyle_Segment" "HC_KStyle_Segment();" -2)
       (list "HC_Translate_Object" "HC_Translate_Object(, , );" -6)
       (list "HC_UnDefine_Alias" "HC_UnDefine_Alias();" -2)
       (list "HC_UnDefine_Color" "HC_UnDefine_Color();" -2)
       (list "HC_UnDefine_Color_Name" "HC_UnDefine_Color_Name();" -2)
       (list "HC_UnDefine_Error_Handler" "HC_UnDefine_Error_Handler();" -2)
       (list "HC_UnDefine_Exit_Handler" "HC_UnDefine_Exit_Handler();" -2)
       (list "HC_UnSet_Camera" "HC_UnSet_Camera();" -2)
       (list "HC_UnSet_Color" "HC_UnSet_Color();" -2)
       (list "HC_UnSet_Color_Map" "HC_UnSet_Color_Map();" -2)
       (list "HC_UnSet_Driver_Options" "HC_UnSet_Driver_Options();" -2)
       (list "HC_UnSet_Edge_Pattern" "HC_UnSet_Edge_Pattern();" -2)
       (list "HC_UnSet_Edge_Weight" "HC_UnSet_Edge_Weight();" -2)
       (list "HC_UnSet_Face_Pattern" "HC_UnSet_Face_Pattern();" -2)
       (list "HC_UnSet_Handedness" "HC_UnSet_Handedness();" -2)
       (list "HC_UnSet_Heuristics" "HC_UnSet_Heuristics();" -2)
       (list "HC_UnSet_Line_Pattern" "HC_UnSet_Line_Pattern();" -2)
       (list "HC_UnSet_Line_Weight" "HC_UnSet_Line_Weight();" -2)
       (list "HC_UnSet_Marker_Size" "HC_UnSet_Marker_Size();" -2)
       (list "HC_UnSet_Marker_Symbol" "HC_UnSet_Marker_Symbol();" -2)
       (list "HC_UnSet_Metafile" "HC_UnSet_Metafile();" -2)
       (list "HC_UnSet_Modelling_Matrix" "HC_UnSet_Modelling_Matrix();" -2)
       (list "HC_UnSet_Selectability" "HC_UnSet_Selectability();" -2)
       (list "HC_UnSet_Streaming_Mode" "HC_UnSet_Streaming_Mode();" -2)
       (list "HC_UnSet_Text_Alignment" "HC_UnSet_Text_Alignment();" -2)
       (list "HC_UnSet_Text_Font" "HC_UnSet_Text_Font();" -2)
       (list "HC_UnSet_Text_Path" "HC_UnSet_Text_Path();" -2)
       (list "HC_UnSet_Text_Size" "HC_UnSet_Text_Size();" -2)
       (list "HC_UnSet_Text_Spacing" "HC_UnSet_Text_Spacing();" -2)
       (list "HC_UnSet_User_Options" "HC_UnSet_User_Options();" -2)
       (list "HC_UnSet_User_Value" "HC_UnSet_User_Value();" -2)
       (list "HC_UnSet_Visibility" "HC_UnSet_Visibility();" -2)
       (list "HC_UnSet_Window" "HC_UnSet_Window();" -2)
       (list "HC_UnSet_Window_Frame" "HC_UnSet_Window_Frame();" -2)
       (list "HC_UnSet_Window_Pattern" "HC_UnSet_Window_Pattern();" -2)
       (list "HC_Update_Display" "HC_Update_Display();" -2)
       (list "HC_Zoom_Camera" "HC_Zoom_Camera();" -2)
       (list "HC_QAppend_Modelling_Matrix" "HC_QAppend_Modelling_Matrix(, );" -4)
       (list "HC_QDolly_Camera" "HC_QDolly_Camera(, , , );" -8)
       (list "HC_QModify_Color_Map" "HC_QModify_Color_Map(, , );" -6)
       (list "HC_QModify_Color_Map_By_Value" "HC_QModify_Color_Map_By_Value(, , , , );" -10)
       (list "HC_QOrbit_Camera" "HC_QOrbit_Camera(, , );" -6)
       (list "HC_QPan_Camera" "HC_QPan_Camera(, , );" -6)
       (list "HC_QRoll_Camera" "HC_QRoll_Camera(, );" -4)
       (list "HC_QRotate_Object" "HC_QRotate_Object(, , , );" -8)
       (list "HC_QRotate_Object_Offaxis" "HC_QRotate_Object_Offaxis(, , , , );" -10)
       (list "HC_QScale_Object" "HC_QScale_Object(, , , );" -8)
       (list "HC_QSet_Camera" "HC_QSet_Camera(, , , , , , );" -14)
       (list "HC_QSet_Camera_By_Volume" "HC_QSet_Camera_By_Volume(, , , , , );" -12)
       (list "HC_QSet_Camera_Field" "HC_QSet_Camera_Field(, , );" -6)
       (list "HC_QSet_Camera_Position" "HC_QSet_Camera_Position(, , , );" -8)
       (list "HC_QSet_Camera_Projection" "HC_QSet_Camera_Projection(, );" -4)
       (list "HC_QSet_Camera_Target" "HC_QSet_Camera_Target(, , , );" -8)
       (list "HC_QSet_Camera_Up_Vector" "HC_QSet_Camera_Up_Vector(, , , );" -8)
       (list "HC_QSet_Color" "HC_QSet_Color(, );" -4)
       (list "HC_QSet_Color_By_Index" "HC_QSet_Color_By_Index(, , );" -6)
       (list "HC_QSet_Color_By_Value" "HC_QSet_Color_By_Value(, , , , , );" -12)
       (list "HC_QSet_Color_Map" "HC_QSet_Color_Map(, );" -4)
       (list "HC_QSet_Color_Map_By_Value" "HC_QSet_Color_Map_By_Value(, , , );" -8)
       (list "HC_QSet_Driver_Options" "HC_QSet_Driver_Options(, );" -4)
       (list "HC_QSet_Edge_Pattern" "HC_QSet_Edge_Pattern(, );" -4)
       (list "HC_QSet_Edge_Weight" "HC_QSet_Edge_Weight(, );" -4)
       (list "HC_QSet_Face_Pattern" "HC_QSet_Face_Pattern(, );" -4)
       (list "HC_QSet_Handedness" "HC_QSet_Handedness(, );" -4)
       (list "HC_QSet_Heuristics" "HC_QSet_Heuristics(, );" -4)
       (list "HC_QSet_Line_Pattern" "HC_QSet_Line_Pattern(, );" -4)
       (list "HC_QSet_Line_Weight" "HC_QSet_Line_Weight(, );" -4)
       (list "HC_QSet_Marker_Size" "HC_QSet_Marker_Size(, );" -4)
       (list "HC_QSet_Marker_Symbol" "HC_QSet_Marker_Symbol(, );" -4)
       (list "HC_QSet_Metafile" "HC_QSet_Metafile(, );" -4)
       (list "HC_QSet_Modelling_Matrix" "HC_QSet_Modelling_Matrix(, );" -4)
       (list "HC_QSet_Selectability" "HC_QSet_Selectability(, );" -4)
       (list "HC_QSet_Streaming_Mode" "HC_QSet_Streaming_Mode(, );" -4)
       (list "HC_QSet_Text_Alignment" "HC_QSet_Text_Alignment(, );" -4)
       (list "HC_QSet_Text_Font" "HC_QSet_Text_Font(, );" -4)
       (list "HC_QSet_Text_Path" "HC_QSet_Text_Path(, , , );" -8)
       (list "HC_QSet_Text_Size" "HC_QSet_Text_Size(, );" -4)
       (list "HC_QSet_Text_Spacing" "HC_QSet_Text_Spacing(, );" -4)
       (list "HC_QSet_User_Options" "HC_QSet_User_Options(, );" -4)
       (list "HC_QSet_User_Value" "HC_QSet_User_Value(, );" -4)
       (list "HC_QSet_Visibility" "HC_QSet_Visibility(, );" -4)
       (list "HC_QSet_Window" "HC_QSet_Window(, , , , );" -10)
       (list "HC_QSet_Window_Frame" "HC_QSet_Window_Frame(, );" -4)
       (list "HC_QSet_Window_Pattern" "HC_QSet_Window_Pattern(, );" -4)
       (list "HC_QShow_Camera" "HC_QShow_Camera(, , , , , , );" -14)
       (list "HC_QShow_Camera_By_Volume" "HC_QShow_Camera_By_Volume(, , , , , );" -12)
       (list "HC_QShow_Camera_Field" "HC_QShow_Camera_Field(, , );" -6)
       (list "HC_QShow_Camera_Position" "HC_QShow_Camera_Position(, , , );" -8)
       (list "HC_QShow_Camera_Projection" "HC_QShow_Camera_Projection(, );" -4)
       (list "HC_QShow_Camera_Target" "HC_QShow_Camera_Target(, , , );" -8)
       (list "HC_QShow_Camera_Up_Vector" "HC_QShow_Camera_Up_Vector(, , , );" -8)
       (list "HC_QShow_Color" "HC_QShow_Color(, );" -4)
       (list "HC_QShow_Color_By_Index" "HC_QShow_Color_By_Index(, , );" -6)
       (list "HC_QShow_Color_By_Value" "HC_QShow_Color_By_Value(, , , , , );" -12)
       (list "HC_QShow_Color_Map" "HC_QShow_Color_Map(, );" -4)
       (list "HC_QShow_Color_Map_By_Value" "HC_QShow_Color_Map_By_Value(, , , );" -8)
       (list "HC_QShow_Color_Map_Count" "HC_QShow_Color_Map_Count(, );" -4)
       (list "HC_QShow_Driver_Options" "HC_QShow_Driver_Options(, );" -4)
       (list "HC_QShow_Edge_Pattern" "HC_QShow_Edge_Pattern(, );" -4)
       (list "HC_QShow_Edge_Weight" "HC_QShow_Edge_Weight(, );" -4)
       (list "HC_QShow_Face_Pattern" "HC_QShow_Face_Pattern(, );" -4)
       (list "HC_QShow_Handedness" "HC_QShow_Handedness(, );" -4)
       (list "HC_QShow_Heuristics" "HC_QShow_Heuristics(, );" -4)
       (list "HC_QShow_Line_Pattern" "HC_QShow_Line_Pattern(, );" -4)
       (list "HC_QShow_Line_Weight" "HC_QShow_Line_Weight(, );" -4)
       (list "HC_QShow_Marker_Size" "HC_QShow_Marker_Size(, );" -4)
       (list "HC_QShow_Marker_Symbol" "HC_QShow_Marker_Symbol(, );" -4)
       (list "HC_QShow_Metafile" "HC_QShow_Metafile(, );" -4)
       (list "HC_QShow_Modelling_Matrix" "HC_QShow_Modelling_Matrix(, );" -4)
       (list "HC_QShow_Net_Camera" "HC_QShow_Net_Camera(, , , , , , );" -14)
       (list "HC_QShow_Net_Camera_By_Volume" "HC_QShow_Net_Camera_By_Volume(, , , , , );" -12)
       (list "HC_QShow_Net_Camera_Field" "HC_QShow_Net_Camera_Field(, , );" -6)
       (list "HC_QShow_Net_Camera_Position" "HC_QShow_Net_Camera_Position(, , , );" -8)
       (list "HC_QShow_Net_Camera_Projection" "HC_QShow_Net_Camera_Projection(, );" -4)
       (list "HC_QShow_Net_Camera_Target" "HC_QShow_Net_Camera_Target(, , , );" -8)
       (list "HC_QShow_Net_Camera_Up_Vector" "HC_QShow_Net_Camera_Up_Vector(, , , );" -8)
       (list "HC_QShow_Net_Color" "HC_QShow_Net_Color(, );" -4)
       (list "HC_QShow_Net_Color_By_Index" "HC_QShow_Net_Color_By_Index(, , );" -6)
       (list "HC_QShow_Net_Color_By_Value" "HC_QShow_Net_Color_By_Value(, , , , , );" -12)
       (list "HC_QShow_Net_Color_Map" "HC_QShow_Net_Color_Map(, );" -4)
       (list "HC_QShow_Net_Color_Map_By_Value" "HC_QShow_Net_Color_Map_By_Value(, , , );" -8)
       (list "HC_QShow_Net_Color_Map_Count" "HC_QShow_Net_Color_Map_Count(, );" -4)
       (list "HC_QShow_Net_Driver_Options" "HC_QShow_Net_Driver_Options(, );" -4)
       (list "HC_QShow_Net_Edge_Pattern" "HC_QShow_Net_Edge_Pattern(, );" -4)
       (list "HC_QShow_Net_Edge_Weight" "HC_QShow_Net_Edge_Weight(, );" -4)
       (list "HC_QShow_Net_Face_Pattern" "HC_QShow_Net_Face_Pattern(, );" -4)
       (list "HC_QShow_Net_Handedness" "HC_QShow_Net_Handedness(, );" -4)
       (list "HC_QShow_Net_Heuristics" "HC_QShow_Net_Heuristics(, );" -4)
       (list "HC_QShow_Net_Line_Pattern" "HC_QShow_Net_Line_Pattern(, );" -4)
       (list "HC_QShow_Net_Line_Weight" "HC_QShow_Net_Line_Weight(, );" -4)
       (list "HC_QShow_Net_Marker_Size" "HC_QShow_Net_Marker_Size(, );" -4)
       (list "HC_QShow_Net_Marker_Symbol" "HC_QShow_Net_Marker_Symbol(, );" -4)
       (list "HC_QShow_Net_Metafile" "HC_QShow_Net_Metafile(, );" -4)
       (list "HC_QShow_Net_Modelling_Matrix" "HC_QShow_Net_Modelling_Matrix(, );" -4)
       (list "HC_QShow_Net_Selectability" "HC_QShow_Net_Selectability(, );" -4)
       (list "HC_QShow_Net_Streaming_Mode" "HC_QShow_Net_Streaming_Mode(, );" -4)
       (list "HC_QShow_Net_Text_Alignment" "HC_QShow_Net_Text_Alignment(, );" -4)
       (list "HC_QShow_Net_Text_Font" "HC_QShow_Net_Text_Font(, );" -4)
       (list "HC_QShow_Net_Text_Path" "HC_QShow_Net_Text_Path(, , , );" -8)
       (list "HC_QShow_Net_Text_Size" "HC_QShow_Net_Text_Size(, );" -4)
       (list "HC_QShow_Net_Text_Spacing" "HC_QShow_Net_Text_Spacing(, );" -4)
       (list "HC_QShow_Net_User_Options" "HC_QShow_Net_User_Options(, );" -4)
       (list "HC_QShow_Net_User_Value" "HC_QShow_Net_User_Value(, );" -4)
       (list "HC_QShow_Net_Visibility" "HC_QShow_Net_Visibility(, );" -4)
       (list "HC_QShow_Net_Window" "HC_QShow_Net_Window(, , , , );" -10)
       (list "HC_QShow_Net_Window_Frame" "HC_QShow_Net_Window_Frame(, );" -4)
       (list "HC_QShow_Net_Window_Pattern" "HC_QShow_Net_Window_Pattern(, );" -4)
       (list "HC_QShow_One_Color" "HC_QShow_One_Color(, , );" -6)
       (list "HC_QShow_One_Color_By_Index" "HC_QShow_One_Color_By_Index(, , );" -6)
       (list "HC_QShow_One_Color_By_Value" "HC_QShow_One_Color_By_Value(, , , , , );" -12)
       (list "HC_QShow_One_Color_Map" "HC_QShow_One_Color_Map(, , );" -6)
       (list "HC_QShow_One_Color_Map_By_Value" "HC_QShow_One_Color_Map_By_Value(, , , , , );" -12)
       (list "HC_QShow_One_Driver_Option" "HC_QShow_One_Driver_Option(, , );" -6)
       (list "HC_QShow_One_Heuristic" "HC_QShow_One_Heuristic(, , );" -6)
       (list "HC_QShow_One_Net_Color" "HC_QShow_One_Net_Color(, , );" -6)
       (list "HC_QShow_One_Net_Color_By_Index" "HC_QShow_One_Net_Color_By_Index(, , );" -6)
       (list "HC_QShow_One_Net_Color_By_Value" "HC_QShow_One_Net_Color_By_Value(, , , , , );" -12)
       (list "HC_QShow_One_Net_Color_Map" "HC_QShow_One_Net_Color_Map(, , );" -6)
       (list "HC_QShow_One_Net_Color_Map_By_V" "HC_QShow_One_Net_Color_Map_By_V(, , , , , );" -12)
       (list "HC_QShow_One_Net_Driver_Option" "HC_QShow_One_Net_Driver_Option(, , );" -6)
       (list "HC_QShow_One_Net_Heuristic" "HC_QShow_One_Net_Heuristic(, , );" -6)
       (list "HC_QShow_One_Net_Selectability" "HC_QShow_One_Net_Selectability(, , );" -6)
       (list "HC_QShow_One_Net_Visibility" "HC_QShow_One_Net_Visibility(, , );" -6)
       (list "HC_QShow_One_Selectability" "HC_QShow_One_Selectability(, , );" -6)
       (list "HC_QShow_One_Visibility" "HC_QShow_One_Visibility(, , );" -6)
       (list "HC_QShow_Selectability" "HC_QShow_Selectability(, );" -4)
       (list "HC_QShow_Streaming_Mode" "HC_QShow_Streaming_Mode(, );" -4)
       (list "HC_QShow_Text_Alignment" "HC_QShow_Text_Alignment(, );" -4)
       (list "HC_QShow_Text_Font" "HC_QShow_Text_Font(, );" -4)
       (list "HC_QShow_Text_Path" "HC_QShow_Text_Path(, , , );" -8)
       (list "HC_QShow_Text_Size" "HC_QShow_Text_Size(, );" -4)
       (list "HC_QShow_Text_Spacing" "HC_QShow_Text_Spacing(, );" -4)
       (list "HC_QShow_User_Options" "HC_QShow_User_Options(, );" -4)
       (list "HC_QShow_User_Value" "HC_QShow_User_Value(, );" -4)
       (list "HC_QShow_Visibility" "HC_QShow_Visibility(, );" -4)
       (list "HC_QShow_Window" "HC_QShow_Window(, , , , );" -10)
       (list "HC_QShow_Window_Frame" "HC_QShow_Window_Frame(, );" -4)
       (list "HC_QShow_Window_Pattern" "HC_QShow_Window_Pattern(, );" -4)
       (list "HC_QTranslate_Object" "HC_QTranslate_Object(, , , );" -8)
       (list "HC_QUnSet_Camera" "HC_QUnSet_Camera();" -2)
       (list "HC_QUnSet_Color" "HC_QUnSet_Color();" -2)
       (list "HC_QUnSet_Color_Map" "HC_QUnSet_Color_Map();" -2)
       (list "HC_QUnSet_Driver_Options" "HC_QUnSet_Driver_Options();" -2)
       (list "HC_QUnSet_Edge_Pattern" "HC_QUnSet_Edge_Pattern();" -2)
       (list "HC_QUnSet_Edge_Weight" "HC_QUnSet_Edge_Weight();" -2)
       (list "HC_QUnSet_Face_Pattern" "HC_QUnSet_Face_Pattern();" -2)
       (list "HC_QUnSet_Handedness" "HC_QUnSet_Handedness();" -2)
       (list "HC_QUnSet_Heuristics" "HC_QUnSet_Heuristics();" -2)
       (list "HC_QUnSet_Line_Pattern" "HC_QUnSet_Line_Pattern();" -2)
       (list "HC_QUnSet_Line_Weight" "HC_QUnSet_Line_Weight();" -2)
       (list "HC_QUnSet_Marker_Size" "HC_QUnSet_Marker_Size();" -2)
       (list "HC_QUnSet_Marker_Symbol" "HC_QUnSet_Marker_Symbol();" -2)
       (list "HC_QUnSet_Metafile" "HC_QUnSet_Metafile();" -2)
       (list "HC_QUnSet_Modelling_Matrix" "HC_QUnSet_Modelling_Matrix();" -2)
       (list "HC_QUnSet_Selectability" "HC_QUnSet_Selectability();" -2)
       (list "HC_QUnSet_Streaming_Mode" "HC_QUnSet_Streaming_Mode();" -2)
       (list "HC_QUnSet_Text_Alignment" "HC_QUnSet_Text_Alignment();" -2)
       (list "HC_QUnSet_Text_Font" "HC_QUnSet_Text_Font();" -2)
       (list "HC_QUnSet_Text_Path" "HC_QUnSet_Text_Path();" -2)
       (list "HC_QUnSet_Text_Size" "HC_QUnSet_Text_Size();" -2)
       (list "HC_QUnSet_Text_Spacing" "HC_QUnSet_Text_Spacing();" -2)
       (list "HC_QUnSet_User_Options" "HC_QUnSet_User_Options();" -2)
       (list "HC_QUnSet_User_Value" "HC_QUnSet_User_Value();" -2)
       (list "HC_QUnSet_Visibility" "HC_QUnSet_Visibility();" -2)
       (list "HC_QUnSet_Window" "HC_QUnSet_Window();" -2)
       (list "HC_QUnSet_Window_Frame" "HC_QUnSet_Window_Frame();" -2)
       (list "HC_QUnSet_Window_Pattern" "HC_QUnSet_Window_Pattern();" -2)
       (list "HC_QZoom_Camera" "HC_QZoom_Camera(, );" -4)
       (list "HC_Set_Driver" "HC_Set_Driver();" -2)
       (list "HC_Show_Driver" "HC_Show_Driver();" -2)
       (list "HC_Show_Net_Driver" "HC_Show_Net_Driver();" -2)
       (list "HC_UnSet_Driver" "HC_UnSet_Driver();" -2)
       (list "HC_QSet_Driver" "HC_QSet_Driver(, );" -4)
       (list "HC_QShow_Driver" "HC_QShow_Driver(, );" -4)
       (list "HC_QShow_Net_Driver" "HC_QShow_Net_Driver(, );" -4)
       (list "HC_QUnSet_Driver" "HC_QUnSet_Driver();" -2)
       (list "HC_Begin_Alias_Search" "HC_Begin_Alias_Search();" -2)
       (list "HC_Begin_Color_Name_Search" "HC_Begin_Color_Name_Search();" -2)
       (list "HC_Begin_Contents_Search" "HC_Begin_Contents_Search(, );" -4)
       (list "HC_Begin_Open_Segment_Search" "HC_Begin_Open_Segment_Search();" -2)
       (list "HC_Begin_Segment_Search" "HC_Begin_Segment_Search();" -2)
       (list "HC_Find_Alias" "HC_Find_Alias();" -2)
       (list "HC_Find_Color_Name" "HC_Find_Color_Name(, , );" -6)
       (list "HC_Find_Contents" "HC_Find_Contents(, );" -4)
       (list "HC_Find_Open_Segment" "HC_Find_Open_Segment();" -2)
       (list "HC_Find_Segment" "HC_Find_Segment();" -2)
       (list "HC_End_Alias_Search" "HC_End_Alias_Search();" -2)
       (list "HC_End_Color_Name_Search" "HC_End_Color_Name_Search();" -2)
       (list "HC_End_Contents_Search" "HC_End_Contents_Search();" -2)
       (list "HC_End_Open_Segment_Search" "HC_End_Open_Segment_Search();" -2)
       (list "HC_End_Segment_Search" "HC_End_Segment_Search();" -2)
;;;;
;;;; Expansions for BobGadgets
;;;;
       (list "GB_Set_Defaults" "GB_Set_Defaults();" -2)
       (list "GB_Set_Colors" "GB_Set_Colors(, , , , , , , );" -16)
       (list "GB_Push_Colors" "GB_Push_Colors();" -2)
       (list "GB_Pop_Colors" "GB_Pop_Colors();" -2)
       (list "GB_Set_Bar_Size" "GB_Set_Bar_Size(, );" -4)
       (list "GB_Push_Bar_Size" "GB_Push_Bar_Size();" -2)
       (list "GB_Pop_Bar_Size" "GB_Pop_Bar_Size();" -2)
       (list "GB_Draw_Edged_Window" "GB_Draw_Edged_Window();" -2)
       (list "GB_Hilite_Edged_Window" "GB_Hilite_Edged_Window();" -2)
       (list "GB_Draw_Plain_Window" "GB_Draw_Plain_Window();" -2)
       (list "GB_Hilite_Plain_Window" "GB_Hilite_Plain_Window();" -2)
       (list "GB_Make_Button" "GB_Make_Button(, , , , , , , , , );" -20)
       (list "GB_Make_Slider" "GB_Make_Slider(, , , , , , , , );" -18)
       (list "GB_Make_Icon" "GB_Make_Icon(, , , , , , , );" -16)
       (list "GB_Make_Sizable_Window" "GB_Make_Sizable_Window(, , , , , );" -12)
       (list "GB_Make_Closable_Sizable_Window" "GB_Make_Closable_Sizable_Window(, , , , , , );" -14)
       (list "GB_Make_Titled_Window" "GB_Make_Titled_Window(, , , , , , );" -14)
       (list "GB_Make_Weird_Window" "GB_Make_Weird_Window(, , , , , , );" -14)
       (list "GB_Draw_Edged_Scroll_Up" "GB_Draw_Edged_Scroll_Up();" -2)
       (list "GB_Draw_Edged_Scroll_Down" "GB_Draw_Edged_Scroll_Down();" -2)
       (list "GB_Scroll_Up" "GB_Scroll_Up(, );" -4)
       (list "GB_Scroll_Down" "GB_Scroll_Down(, );" -4)
       (list "GB_Add_To_Spreadsheet" "GB_Add_To_Spreadsheet(, , );" -6)
       (list "GB_Clear_Spreadsheet" "GB_Clear_Spreadsheet();" -2)
       (list "GB_Update_Spreadsheet" "GB_Update_Spreadsheet();" -2)
       (list "GB_Update_Base_Spreadsheet" "GB_Update_Base_Spreadsheet();" -2)
       (list "GB_Resize_Spreadsheet" "GB_Resize_Spreadsheet(, );" -4)
       (list "GB_Resize_Base_Spreadsheet" "GB_Resize_Base_Spreadsheet(, );" -4)
       (list "GB_Update_Input_And_Send" "GB_Update_Input_And_Send(, );" -4)
       (list "GB_Make_Spreadsheet" "GB_Make_Spreadsheet(, , , , , , , , , , , , , , , , , );" -36)
       (list "GB_Make_Selector" "GB_Make_Selector(, , , , , , , );" -16)
       (list "GB_Make_Circular_Selector" "GB_Make_Circular_Selector(, , , , , , , );" -16)
       (list "GB_Add_To_Text_List" "GB_Add_To_Text_List(, , );" -6)
       (list "GB_Clear_Text_List" "GB_Clear_Text_List();" -2)
       (list "GB_Update_Selector_List" "GB_Update_Selector_List();" -2)
       (list "GB_Resize_Selector_List" "GB_Resize_Selector_List(, );" -4)
       (list "GB_Draw_Slider" "GB_Draw_Slider();" -2)
       (list "GB_Make_Titled_Slider_Window" "GB_Make_Titled_Slider_Window(, , , , , , , , , );" -20)
       (list "GS_Toggle" "GS_Toggle(, );" -4)
       (list "GS_Action" "GS_Action(, );" -4)
       (list "GS_Confirm" "GS_Confirm(, );" -4)
       (list "GS_Blender" "GS_Blender(, );" -4)
       (list "GS_Drag" "GS_Drag(, );" -4)
       (list "GS_Resize" "GS_Resize(, );" -4)
       (list "GS_Icon" "GS_Icon(, );" -4)
       (list "GS_Icon_Drag" "GS_Icon_Drag(, );" -4)
       (list "GS_Open_Close" "GS_Open_Close(, );" -4)
       (list "GS_Delete_Single" "GS_Delete_Single(, );" -4)
       (list "GS_Delete_Multiple" "GS_Delete_Multiple(, );" -4)
       (list "GS_Edit_Text" "GS_Edit_Text(, );" -4)
       (list "GS_Send_Text" "GS_Send_Text(, );" -4)
       (list "GS_Continuous_Action" "GS_Continuous_Action(, );" -4)
       (list "GS_Change_Action" "GS_Change_Action(, );" -4)
       (list "GS_Slider" "GS_Slider(, );" -4)
       (list "GI_Free_Gadget" "GI_Free_Gadget(, );" -4)
       (list "GI_Free_Gadget_Now" "GI_Free_Gadget_Now(, );" -4)
       (list "GI_Free_Deferred_Gadgets" "GI_Free_Deferred_Gadgets();" -2)
       (list "GI_Set_Draw_Routines" "GI_Set_Draw_Routines(, );" -4)
       (list "GI_Push_Draw_Routines" "GI_Push_Draw_Routines();" -2)
       (list "GI_Pop_Draw_Routines" "GI_Pop_Draw_Routines();" -2)
       (list "GI_Enable_Updates" "GI_Enable_Updates();" -2)
       (list "GI_Disable_Updates" "GI_Disable_Updates();" -2)
       (list "GI_Make_Gadget" "GI_Make_Gadget(, , , , , , , , );" -18)
       (list "GI_Show_Gadget" "GI_Show_Gadget();" -2)
       (list "GI_Show_Gadget_Data" "GI_Show_Gadget_Data();" -2)
       (list "GI_Set_Gadget_Data" "GI_Set_Gadget_Data(, );" -4)
       (list "GI_Set_Gadget_Default_Color" "GI_Set_Gadget_Default_Color(, );" -4)
       (list "GI_Set_Gadget_Active_Color" "GI_Set_Gadget_Active_Color(, );" -4)
       (list "GI_Set_Gadget_Default_Text" "GI_Set_Gadget_Default_Text(, );" -4)
       (list "GI_Set_Gadget_Active_Text" "GI_Set_Gadget_Active_Text(, );" -4)
       (list "GI_Set_Gadget_Internal_Routine" "GI_Set_Gadget_Internal_Routine(, );" -4)
       (list "GI_Set_Gadget_Service_Routine" "GI_Set_Gadget_Service_Routine(, );" -4)
       (list "GI_Set_Button_Routine" "GI_Set_Button_Routine();" -2)
       (list "GI_Set_Idle_Routine" "GI_Set_Idle_Routine();" -2)
       (list "Go_Go_Gadget" "Go_Go_Gadget();" -2)
       (list "Go_Gadget" "Go_Gadget();" -2)
       (list "Go_Idle" "Go_Idle();" -2)
       (list "GU_strchr" "GU_strchr(, );" -4)
       (list "GU_strrchr" "GU_strrchr(, );" -4)
       (list "GU_Load_String" "GU_Load_String(, );" -4)
       (list "GU_Set_Color" "GU_Set_Color(, );" -4)
       (list "GU_Remap_Coordinates" "GU_Remap_Coordinates(, , , , , );" -12)
       (list "GU_Generate_Generic_Names" "GU_Generate_Generic_Names(, , );" -6)
       (list "GU_Find_Gadget_Set_Extent" "GU_Find_Gadget_Set_Extent(, , , , );" -10)
       (list "GU_HC_Outline_Rectangle" "GU_HC_Outline_Rectangle(, , , );" -8)
       (list "GU_Show_Limited_Selection_Loc" "GU_Show_Limited_Selection_Loc(, , );" -6)
       (list "GU_Show_Limited_Location" "GU_Show_Limited_Location(, , );" -6)
       (list "GU_Delete_Gadget" "GU_Delete_Gadget();" -2)
       (list "GU_Delete_Gadget_Now" "GU_Delete_Gadget_Now();" -2)
       (list "GU_Resize_Gadget" "GU_Resize_Gadget(, , , , );" -10)
       (list "GU_Bring_Gadget_To_Front" "GU_Bring_Gadget_To_Front();" -2)
       (list "GU_Update_Slider" "GU_Update_Slider(, );" -4)
       (list "GU_File_List" "GU_File_List(, );" -4)
       (list "GU_Fetch_Text" "GU_Fetch_Text(, , );" -6)))


