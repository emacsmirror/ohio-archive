;;; vi-dot.el --- Implementation of vi's dot command for Emacs

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Mo 02 Mar 98
;; Version: 0.51, We 13 May 98
;; Keywords: abbrev, vi, dot, ., universal argument, typematic, repeat

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Sometimes the fastest way to get something done is just to lean on a key;
;; moving forward through a series of words by leaning on M-f is an example.
;; But 'forward-page is orthodoxily bound to C-x ], so moving forward through
;; several pages requires
;;   Loop until desired page is reached:
;;     Hold down control key with left pinkie.
;;     Tap <x>.
;;     Lift left pinkie off control key.
;;     Tap <]>.
;; This is a pain in the ass.

;; This package defines a command that repeats the preceding command,
;; whatever that was.  The command is called `vi-dot' because the vi editor,
;; Emacs's arch-rival among the Great Unwashed, does that when "." is pressed
;; in its command mode.

;; Starting with GNU Emacs 20.3, this package is part of Emacs, and the
;; `vi-dot' command is bound to the key sequence C-x z.  (You can actually
;; keep repeating the most recent command by just repeating the z after the
;; first C-x z.)  However, you can use this package with older versions of
;; GNU Emacs by installing it yourself.  Here's how:
;; [1] Copy this file to a directory that appears in your load-path.
;;     `load-path' is the name of a variable that contains a list of
;;     directories Emacs searches for files to load.  To prepend another
;;     directory to load-path, put a line like
;;     (add-to-list 'load-path "c:/My_Directory") in your .emacs file.
;; [2] Then, put the line
;;       (require 'vi-dot)
;;     in your .emacs file.  (Don't try to use an autoload, because then the
;;     first time vi-dot is invoked, some data the command may need to know
;;     about the preceding command won't have been set.)  Then put the line
;;       (global-set-key "\C-xz" 'vi-dot)
;;     in your .emacs to give the command its orthodox binding of C-x z.
;; [3] Those lines in your .emacs will be evaluated every time you start a
;;     new Emacs session, & will cause this package to be available in that
;;     session.  To make it available in the session in which you typed the
;;     lines into your .emacs file, move the cursor to the (require) line,
;;     type C-SPC to set the mark, move it to the beginning of the line after
;;     the `global-set-key', and type M-x eval-region.  You should then see
;;     "nil" in the echo area; that's the value of the final form evaluated.
;;     If you see an error message, there's a typo.  You won't need to do
;;     this `eval-region' again; in the future, when you load Emacs, that
;;     region will be evaluated along with the rest of your .emacs at load
;;     time.  This is just for adding lines to .emacs during a running
;;     session.
;; That's it; vi-dot is installed & ready for use.

;; Since the whole point of vi-dot is to let you repeat commands that are
;; bound to multiple keystrokes by leaning on a *single* key, it seems not to
;; make sense to bind vi-dot itself to a multiple-character key sequence, but
;; there aren't any appropriate single characters left in the orthodox global
;; map.  (Meta characters don't count because they require two keystrokes if
;; you don't have a real meta key, and things like function keys can't be
;; relied on to be available to all users.  We considered rebinding C-z,
;; since C-x C-z is also bound to the same command, but RMS decided too many
;; users were accustomed to the orthodox meaning of C-z.)  So the vi-dot
;; command checks what key sequence it was invoked by, and allows you to
;; repeat the final key in that sequence to keep repeating the command.
;; For example, C-x ] C-x z z z will move forward 4 pages.

;; This works correctly inside a keyboard macro as far as recording and
;; playback go, but `edit-kbd-macro' gets it wrong.  That shouldn't really
;; matter; if you need to edit something like
;;   C-x ]              ;; forward-page
;;   C-x z              ;; vi-dot
;;   zz                 ;; self-insert-command * 2
;;   C-x                ;; Control-X-prefix
;; you can just kill the bogus final 2 lines, then duplicate the vi-dot line
;; as many times as it's really needed.  Also, `edit-kbd-macro' works
;; correctly if `vi-dot' is invoked through a rebinding to a single keystroke
;; and the global variable vi-dot-repeat-on-final-keystroke is set to a value
;; that doesn't include that keystroke.  For example, the lines
;;   (global-set-key "\C-z" 'vi-dot)
;;   (setq vi-dot-repeat-on-final-keystroke "z")
;; in your .emacs would allow `edit-kbd-macro' to work correctly when C-z was
;; used in a keyboard macro to invoke `vi-dot', but would still allow C-x z
;; to be used for `vi-dot' elsewhere.  The real reason for documenting this
;; isn't that anybody would need it for the `edit-kbd-macro' problem, but
;; that there might be other unexpected ramifications of re-executing on
;; repetitions of the final keystroke, and this shows how to do workarounds.

;; If the preceding command had a prefix argument, that argument is applied
;; to the vi-dot command, unless the vi-dot command is given a new prefix
;; argument, in which case it applies that new prefix argument to the
;; preceding command.  This means a key sequence like C-u - C-x C-t can be
;; repeated.  (It shoves the preceding line upward in the buffer.)

;; Here are some other key sequences with which vi-dot might be useful:
;;   C-u - C-t      [shove preceding character backward in line]
;;   C-u - M-t      [shove preceding word backward in sentence]
;;         C-x ^    enlarge-window [one line] (assuming frame has > 1 window)
;;   C-u - C-x ^    [shrink window one line]
;;         C-x `    next-error
;;   C-u - C-x `    [previous error]
;;         C-x DEL  backward-kill-sentence
;;         C-x e    call-last-kbd-macro
;;         C-x r i  insert-register
;;         C-x r t  string-rectangle
;;         C-x TAB  indent-rigidly [one character]
;;   C-u - C-x TAB  [outdent rigidly one character]
;;         C-x {    shrink-window-horizontally
;;         C-x }    enlarge-window-horizontally

;; Using vi-dot.el doesn't entail a performance hit.  There's a
;; straightforward way to implement a package like this that would save some
;; data about each command as it was executed, but that Lisp would need to be
;; interpreted on every keystroke, which is Bad.  This implementation doesn't
;; do it that way; the peformance impact on almost all keystrokes is 0.

;; Buried in the implementation is a reference to a function in my
;; typematic.el package, which isn't part of GNU Emacs.  However, that
;; package is *not* required by vi-dot; the reference allows it to be used,
;; but doesn't require it.

;; I've been told that this version of vi-dot.el is incompatible with XEmacs
;; because XEmacs lacks a num-input-keys variable.

;;; Code:

(eval-when-compile (require 'cl))

;;;;; ************************* USER OPTIONS ************************** ;;;;;

(defvar vi-dot-too-dangerous '(kill-this-buffer)
  "Commands too dangerous to repeat with vi-dot.")

;; If the last command was self-insert-command, the char to be inserted was
;; obtained by that command from last-command-char, which has now been
;; clobbered by the command sequence that invoked vi-dot.  We could get it
;; from (recent-keys) & set last-command-char to that, "unclobbering" it, but
;; this has the disadvantage that if the user types a sequence of different
;; chars then invokes vi-dot, only the final char will be inserted.  In vi,
;; the dot command can reinsert the entire most-recently-inserted sequence.
;; To do the same thing here, we need to extract the string to insert from
;; the undo information, then insert a new copy in the buffer.  However, the
;; built-in 'insert, which takes a string as an arg, is a little different
;; from 'self-insert-command, which takes only a prefix arg; 'insert ignores
;; 'overwrite-mode.  GNU Emacs 19.34 has no self-insert-string.  But there's
;; one in my dotemacs.el (on the web), so if you want to, you can define that
;; in your .emacs, & it'll Just Work, as it will in any future Emaecse that
;; have self-insert-string.  Or users can code their own
;; insert-string-with-trumpet-fanfare and use that by customizing this:

(defvar vi-dot-insert-function
  (catch t (mapcar (lambda (f) (if (fboundp f) (throw t f)))
                   [self-insert-string
                    insert]))
  "Function used by vi-dot command to re-insert a string of characters.
In a vanilla GNU Emacs this will default to 'insert, which doesn't respect
overwrite-mode; customize with your own insertion function, taking a single
string as an argument, if you have one.")

(defvar vi-dot-message-function nil
  "If non-nil, function used by vi-dot command to say what it's doing.
Message is something like \"Repeating command glorp\".
To disable such messages, assign 'ignore to this variable.  To customize
display, assign a function that takes one string as an arg and displays
it however you want.")

(defvar vi-dot-repeat-on-final-keystroke t
  "Allow `vi-dot' to re-execute every time a single keystroke is hit
even if that keystroke isn't a complete key sequence to which `vi-dot'
is bound.  If this variable is t, `vi-dot' determines what key sequence
it was invoked by, extracts the final character of that sequence, and
re-executes as many times as that final character is hit; so for example
if `vi-dot' is bound to C-x z, typing C-x z z z repeats the previous command
3 times.  If this variable is a sequence of characters, then re-execution
only occurs if the final character by which `vi-dot' was invoked is a
member of that sequence.  If this variable is nil, no re-execution occurs.")
  
;;;;; ****************** HACKS TO THE REST OF EMACS ******************* ;;;;;

;; The basic strategy is to use last-command, a variable built in to Emacs.
;; There are 2 issues that complicate this strategy.  The first is that
;; last-command is given a bogus value when any kill command is executed;
;; this is done to make it easy for 'yank-pop to know that it's being invoked
;; after a kill command.  The second is that the meaning of the command is
;; often altered by the prefix arg, but although Emacs (GNU 19.34) has a
;; builtin prefix-arg specifying the arg for the next command, as well as a
;; builtin current-prefix-arg, it has no builtin last-prefix-arg.

;; There's a builtin (this-command-keys), the return value of which could be
;; executed with (command-execute), but there's no (last-command-keys).
;; Using (last-command-keys) if it existed wouldn't be optimal, however,
;; since it would complicate checking membership in vi-dot-too-dangerous.

;; It would of course be trivial to implement last-prefix-arg &
;; true-last-command by putting something in post-command-hook, but that
;; entails a performance hit; the approach taken below avoids that.

;; First cope with (kill-region).  It's straightforward to advise it to save
;; the true value of this-command before clobbering it.

(require 'advice)

(defvar vi-dot-last-kill-command nil
  "True value of this-command before (kill-region) clobbered it.")

(defadvice kill-region (before vi-dot-save-last-kill-command act)
  "Remember true value of this-command before (kill-region) clobbers it."
  (setq vi-dot-last-kill-command this-command))

;; Next cope with the prefix arg.  I can advise the various functions that
;; create prefix args to save the arg in a variable ...

(defvar vi-dot-prefix-arg nil
  "Prefix arg created as most recent universal argument.")

;; ... but alone that's not enough, because if last-command's prefix arg was
;; nil, none of those functions were ever called, so whatever command before
;; last-command did have a prefix arg has left it in vi-dot-prefix-arg, & I
;; need a way to tell whether whatever's in there applies to last-command.

;; From Info|ELisp|Command Loop|Reading Input|Key Sequence Input:
;;  - Variable: num-input-keys
;;      This variable's value is the number of key sequences processed so far
;;      in this Emacs session.  This includes key sequences read from the
;;      terminal and key sequences read from keyboard macros being executed.
;; num-input-keys counts key *sequences*, not key *strokes*; it's only
;; incremented after reading a complete key sequence mapping to a command.

(defvar vi-dot-num-input-keys-at-prefix -1
  "# of key sequences read in Emacs session when prefix-arg defined.")

(mapcar (lambda (f)
          (eval
           `(defadvice ,f (after vi-dot-save-universal-arg act)
              (setq vi-dot-prefix-arg                  current-prefix-arg
                    vi-dot-num-input-keys-at-prefix    num-input-keys))))
        [universal-argument-more
         universal-argument-other-key
         typematic-universal-argument-more-or-less])

;; Coping with strings of self-insert commands gets hairy when they interact
;; with auto-filling.  Most problems are eliminated by remembering what we're
;; self-inserting, so we only need to get it from the undo information once.

(defvar vi-dot-last-self-insert nil
  "If last repeated command was self-insert-command, it inserted this.")

;; That'll require another keystroke count so we know we're in a string of
;; repetitions of self-insert commands:

(defvar vi-dot-num-input-keys-at-self-insert -1
  "# key sequences read in Emacs session when self-insert-command repeated.")

;;;;; *************** ANALOGOUS HACKS TO VI-DOT ITSELF **************** ;;;;;

;; That mechanism of checking num-input-keys to figure out what's really
;; going on can be useful to other commands that need to fine-tune their
;; interaction with vi-dot.  Instead of requiring them to advise vi-dot, we
;; can just defvar the value they need here, & setq it in the vi-dot command:

(defvar vi-dot-num-input-keys-at-vi-dot -1
  "# key sequences read in Emacs session when vi-dot last invoked.")

;; Also, we can assign a name to the test for which that variable is
;; intended, which thereby documents here how to use it, & makes code that
;; uses it self-documenting:

(defsubst vi-dot-is-really-this-command ()
  "Return t if this command is happening because user invoked `vi-dot'.
Usually, when a command is executing, the Emacs builtin variable
`this-command' identifies the command the user invoked.  Some commands modify
that variable on the theory they're doing more good than harm; `vi-dot' does
that, and usually does do more good than harm.  However, like all do-gooders,
sometimes `vi-dot' gets surprising results from its altruism.  The value of
this function is always whether the value of `this-command' would've been
'vi-dot if `vi-dot' hadn't modified it."
  (= vi-dot-num-input-keys-at-vi-dot num-input-keys))

;; An example of the use of (vi-dot-is-really-this-command) may still be
;; available in <http://www.eskimo.com/~seldon/dotemacs.el>; search for
;; "defun wm-switch-buffer".

;;;;; ******************* THE VI-DOT COMMAND ITSELF ******************* ;;;;;

(defun vi-dot (vi-dot-arg)
  "Repeat most recently executed command.
With prefix arg, apply new prefix arg to that command; otherwise, maintain
prefix arg of most recently executed command if it had one.
This command is named after the `.' command in the Unix vi editor.

If this command is invoked by a multi-character key sequence, it can then
be repeated by repeating the final character of that sequence.  This behavior
can be modified by the global variable `vi-dot-repeat-on-final-keystroke'."
  ;; The most recently executed command could be anything, so surprises could
  ;; result if it were re-executed in a context where new dynamically
  ;; localized variables were shadowing global variables in a `let' clause in
  ;; here.  (Remember that GNU Emacs 19 is dynamically localized.)
  ;; To avoid that, I tried the `lexical-let' of the Common Lisp extensions,
  ;; but that entails a very noticeable performance hit, so instead I use the
  ;; "vi-dot-" prefix, reserved by this package, for *local* variables that
  ;; might be visible to re-executed commands, including this function's arg.
  (interactive "P")
  (when (eq last-command 'kill-region)
    (setq last-command vi-dot-last-kill-command))
  (setq this-command                      last-command
        vi-dot-num-input-keys-at-vi-dot   num-input-keys)
  (when (eq last-command 'mode-exit)
    (error "last-command is mode-exit & can't be repeated"))
  (when (memq last-command vi-dot-too-dangerous)
    (error "Command %S too dangerous to repeat automatically" last-command))
  (when (and (null vi-dot-arg)
             (<= (- num-input-keys vi-dot-num-input-keys-at-prefix) 2))
    (setq vi-dot-arg vi-dot-prefix-arg))
  ;; Now determine whether to loop on repeated taps of the final character
  ;; of the key sequence that invoked vi-dot.  The Emacs global
  ;; last-command-char contains the final character now, but may not still
  ;; contain it after the previous command is repeated, so the character
  ;; needs to be saved.
  (let ((vi-dot-repeat-char
         (if (eq vi-dot-repeat-on-final-keystroke t)
             ;; allow any final input event that was a character
             (when (eq last-command-char
                       last-command-event)
               last-command-char)
           ;; allow only specified final keystrokes
           (car (memq last-command-char
                      (listify-key-sequence
                       vi-dot-repeat-on-final-keystroke))))))
    (if (memq last-command '(exit-minibuffer
                             minibuffer-complete-and-exit
                             self-insert-and-exit))
        (let ((vi-dot-command (car command-history)))
          (vi-dot-message "Repeating %S" vi-dot-command)
          (eval vi-dot-command))
      (if (null vi-dot-arg)
          (vi-dot-message "Repeating command %S" last-command)
        (setq vi-dot-num-input-keys-at-prefix      num-input-keys
              current-prefix-arg                   vi-dot-arg)
        (vi-dot-message "Repeating command %S %S" vi-dot-arg last-command))
      (if (eq last-command 'self-insert-command)
          (let ((insertion
                 (if (<= (- num-input-keys
                            vi-dot-num-input-keys-at-self-insert)
                         1)
                     vi-dot-last-self-insert
                   (let ((range (nth 1 buffer-undo-list)))
                     (condition-case nil
                         (setq vi-dot-last-self-insert
                               (buffer-substring (car range)
                                                 (cdr range)))
                       (error (error "%s %s %s" ;Danger, Will Robinson! 
                                     "vi-dot can't intuit what you"
                                     "inserted before auto-fill"
                                     "clobbered it, sorry")))))))
            (setq vi-dot-num-input-keys-at-self-insert num-input-keys)
            (loop repeat (prefix-numeric-value vi-dot-arg) do
                  (funcall vi-dot-insert-function insertion)))
        (call-interactively last-command)))
    (when vi-dot-repeat-char
      ;; A simple recursion here gets into trouble with max-lisp-eval-depth
      ;; on long sequences of repetitions of a command like `forward-word'
      ;; (only 32 repetitions are possible given the default value of 200 for
      ;; max-lisp-eval-depth), but if I now locally disable the repeat char I
      ;; can iterate indefinitely here around a single level of recursion.
      (let (vi-dot-repeat-on-final-keystroke)
        (while (eq (read-event) vi-dot-repeat-char)
          (vi-dot vi-dot-arg))
        (setq unread-command-events (list last-input-event))))))

(defun vi-dot-message (format &rest args)
  "Like `message' but displays with vi-dot-message-function if non-nil."
  (let ((message (apply 'format format args)))
    (if vi-dot-message-function
        (funcall vi-dot-message-function message)
      (message "%s" message))))

;; OK, there's one situation left where that doesn't work correctly: when the
;; most recent self-insertion provoked an auto-fill.  The problem is that
;; unravelling the undo information after an auto-fill is too hard, since all
;; kinds of stuff can get in there as a result of comment prefixes etc.  It'd
;; be possible to advise do-auto-fill to record the most recent
;; self-insertion before it does its thing, but that's a performance hit on
;; auto-fill, which already has performance problems; so it's better to just
;; leave it like this.  If text didn't provoke an auto-fill when the user
;; typed it, this'll correctly repeat its self-insertion, even if the
;; repetition does cause auto-fill.

;; If you wanted perfection, probably it'd be necessary to hack do-auto-fill
;; into 2 functions, maybe-do-auto-fill & really-do-auto-fill, because only
;; really-do-auto-fill should be advised.  As things are, either the undo
;; information would need to be scanned on every do-auto-fill invocation, or
;; the code at the top of do-auto-fill deciding whether filling is necessary
;; would need to be duplicated in the advice, wasting execution time when
;; filling does turn out to be necessary.

;; I thought maybe this story had a moral, something about functional
;; decomposition; but now I'm not even sure of that, since a function
;; call per se is a performance hit, & even the code that would
;; correspond to really-do-auto-fill has performance problems that
;; can make it necessary to stop typing while Emacs catches up.
;; Maybe the real moral is that perfection is a chimera.

;; Ah, hell, it's all going to fall into a black hole someday anyway.

;;;;; ************************* EMACS CONTROL ************************* ;;;;;

;; Local Variables:
;; mode: outline-minor
;; fill-column: 77
;; outline-regexp: ";;;;+"
;; page-delimiter: "^;;;;"
;; End:

(provide 'vi-dot)

;;; vi-dot.el ends here