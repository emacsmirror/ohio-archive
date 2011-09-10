;;; typematic.el --- Optimize 'universal-argument for typematic keyboards

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Mo 12 Jan 98
;; Version: 0.65, Mo 04 May 98
;; Keywords: abbrev, universal argument, typematic, hardware, typm

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

;; Standard Emacs interprets a single C-u as giving the following command an
;; argument of 4, & each subsequent C-u multiplies that argument by 4.  That
;; allows generating large numbers quickly, but with low precision; high
;; precision requires explicitly typing digits.  This means that, for
;; example, to give an argument of 2 to a command that's bound to a sequence
;; of control characters, the following actions are necessary:
;;   Hold down control key with left pinkie.
;;   Tap <u>.
;;   Lift left pinkie off control key.
;;   Tap <2> (typically a stretch).
;;   Hold down control key again (stretching back in the other direction).
;;   Tap the sequence of control characters that invokes the command.
;; I find that small arguments are useful much more often than large ones (a
;; good example is 'set-selective-display), & that I want to be able to input
;; them without a lot of pinkie gymnastics.  So instead I want this to be the
;; keystroke sequence for providing that argument of 2:
;;   Hold down control key with left pinkie.
;;   Tap <u>.
;;   Tap <u>.
;;   Tap the sequence of control characters that invokes the command.
;; In other words, C-u should start at 1 & increment by 1, instead of
;; starting at 4 & multiplying by 4.

;; Understanding why this is a good idea requires understanding why the
;; orthodox definition was a good idea in its time.  Computer interaction was
;; then typically by teletype or by CRT terminals that ran @ 300 .. 1200 bps.
;; (I remember first seeing a 2400 bps terminal & thinking "Wow!".)  In such
;; an environment, if you wanted, for example, to move forward 4 screenfuls,
;; pressing C-v 4 times would be bad because it would refresh the screen
;; after each move, & you didn't want to waste the I/O time; instead, you
;; wanted to specify how far to go & get a refresh only when you were there.
;; The cost of screen I/O in modern computing environments is practically 0;
;; what we care about now is minimizing finger motion, especially stretching.

;; What makes typematic (the rapid repetition of keystroke generation when a
;; key is held down on an electronic keyboard) relevant is that on a modern
;; keyboard it can be made fast enough to allow generating most realistically
;; desirable large arguments just by holding down a key that increments the
;; universal argument by 1 rather than one that multiplies it by 4.  If you
;; do this for a *large* number, though, you'll probably overshoot, so
;; there's an additional key defined that decrements the magnitude by 1.

;; Standard GNU Emacs 19 provides no automatic generation of large negative
;; arguments; e.g., C-u C-u generates an argument of 16, but C-- C-u C-u
;; doesn't generate an argument of -16.  (I think it should.)  In this
;; package, C-u (or whatever key you choose) keeps going in whatever
;; direction has been selected, so if you still have C-- mapped to
;; 'negative-argument (as it is orthodoxily), C-- C-u C-u generates -2.
;; The "decrement" character goes back in the other direction, toward 0.

;; To use this package, first you'll need to copy this file to a directory
;; that appears in your load-path.  `load-path' is the name of a variable
;; that contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; Then, put
;;   (require 'typematic)
;; in your .emacs file.  The effect will be to take whatever key you have
;; mapped to 'universal-argument, orthodoxily C-u, & remap it so it has the
;; new behavior described above.  Its behavior for explicit arguments will
;; remain unchanged, so for example C-u 6 6 6 will still enter an explicit
;; 666.  The decrement character will be C-d unless you change it;
;;   (setq typematic-decrement-char "\C-l")
;; would, for example, change it to C-l.

;; The one time in your life 10 years from now that you want to pass a
;; particular numeric argument (take 42 as an example) to 'delete-char
;; (orthodoxily C-d) instead of just visually deleting the chars you no
;; longer want, you will still be able to do so by typing `C-u 4 2 C-d'.

;; Here's an example of how useful this change can be.  Many people have
;; mapped a keystroke to (switch-to-buffer (other-buffer)), allowing them to
;; toggle buffers.  This package defines a function typematic-other-buffer
;; that, after loading typematic, allows *troggling* buffers; if you've
;; mapped it to C-c C-o, for example, then just typing C-c C-o will toggle,
;; C-u C-c C-o will troggle, etc.  (A toggle cycles between 2 values, a
;; troggle between three.  I didn't make that word up, but what C-u C-u C-c
;; C-o does would, I guess, have to be called a "quaggle"; next comes a
;; "quiggle", & I'll stop there.)  If you want to use that function, put
;; something like
;;   (global-set-key "\C-c\C-o"
;;           (lambda (P)
;;             (interactive "P")
;;             (switch-to-buffer (typematic-other-buffer P))))
;; in your .emacs file.

;; Bill Zvonar <zvonar@nortel.ca> reports that this "almost" works in XEmacs
;; 20.3; the only problem, he says, is the use in the definition of
;; typematic-universal-argument-mapping of '(keymap), which may be
;; GNU-specific.  I don't have the hardware to run XEmacs so I can't hack
;; this myself, but Bill says that "by just hard-coding
;; typematic-universal-argument-mapping to C-u, typematic.el works fine", so
;; I assume you should be able to put
;;   (setq typematic-universal-argument-mapping "\C-u")
;; in your .emacs file to get this to work with XEmacs.  Let me know.

;;; Code:

(defvar typematic-decrement-char "\C-d"
  "Char that decrements magnitude of universal argument in typematic.el.")

;; I decided against ;;;###autoload for this because loading typematic.el
;; changes the definition of the binding of universal-argument, which is a
;; very radical thing to do; it should only happen by explicit request.

(defun typematic-other-buffer (P)
  "Select which other buffer according to position in buffer-list.
The arg is expected to be passed from an (interactive \"P\") function.
If nil, returns (other-buffer (current-buffer) 'visible-ok);
if '-, returns (other-buffer); otherwise, is construed as a number (which may
require extracting the number from the car of the arg) and accesses the
nth-recently-accessed buffer other than this one, counting from 0,
excluding hidden buffers such as minibuffers."
  (cond ((null P)
         (other-buffer (current-buffer) 'visible-ok))
        ((eq P '-)
         (other-buffer));so a M-- prefix means (not visible-ok)
        (t
         (setq P (prefix-numeric-value P))
         (or (numberp P) (error "typematic-other-buffer internal failure"))
         (setq P (1+ P))
         (or (> P 0) (error "Negative args are meaningless here"))
         (let ((l (buffer-list)))
           (while (and (> P 0) l)
             (setq l (cdr l))
             (and (buffer-live-p (car l))
                  (not (string-match "^ " (buffer-name (car l))))
                  (setq P (1- P))))
           (or (car l) (error "Not that many buffers exist"))))))

;; Simple.el defines universal-argument-map & universal-argument.
;; You need to understand that code before trying to understand this.

(defvar typematic-universal-argument-mapping
  (or (where-is-internal 'universal-argument '(keymap) t)
      ;; Now what?  I think it's more likely that where-is-internal is
      ;; failing (probably because this is an Emacs incompatible with GNU
      ;; Emacs 19.34) rather than that universal-argument has no mapping.
      ;; So I'll assume C-u.
      (progn
        (message "%s %s"
                 "Warning: typematic.el couldn't find"
                 "'universal-argument; assuming C-u.")
        [?\C-u]))
  "The user's key binding of 'universal-argument before loading typematic.
This is orthodoxily bound to C-u.")

(define-key   global-map               typematic-universal-argument-mapping
  'typematic-universal-argument)
(define-key   universal-argument-map   typematic-universal-argument-mapping
  'typematic-universal-argument-more)
(define-key   universal-argument-map   typematic-decrement-char
  'typematic-universal-argument-less)
;;Debug code (use with C-x C-e): (global-set-key "\C-u" 'universal-argument)

(defun typematic-universal-argument ()
  "Begin a numeric argument for the following command.  Unlike standard Emacs,
interpret subsequent \\[typematic-universal-argument] taps as incrementing
the magnitude of the argument by 1 while preserving sign.  The starting value
is 1 by default, or -1 if \\[negative-argument] was tapped first.
Decrement the magnitude of the argument (i.e. move toward 0) in response to
\\<universal-argument-map>\\[typematic-universal-argument-less].
As in standard Emacs, allow allow digits or minus sign to explicitly specify
the argument, after which \\[typematic-universal-argument-more] ends the
argument rather than incrementing it (e.g. to allow inserting 256 \"0\"s)."
  (interactive)
  (setq prefix-arg (list 1))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

(defun typematic-universal-argument-more (P)
  (interactive "P")
  (typematic-universal-argument-more-or-less 1 P))

(defun typematic-universal-argument-less (P)
  (interactive "P")
  (typematic-universal-argument-more-or-less -1 P))

(defun typematic-universal-argument-more-or-less (increment P)
  (cond ((eq P '-)
         (setq prefix-arg (list -1)))
        ((consp P)
         (setq prefix-arg
               (list (+ (* (if (> (car P) 0) 1 -1) increment) (car P)))))
        (t
         (setq prefix-arg P)
         (if (eq increment -1)
             (setq unread-command-events
                   (listify-key-sequence typematic-decrement-char)))
         (setq overriding-terminal-local-map nil)))
  (let (message-log-max) (message "Universal argument: %S" prefix-arg))
  (setq universal-argument-num-events (length (this-command-keys))))

;; C-u is sometimes used as an ersatz prefix key by checking the arg values
;; returned by (interactive "P") for '(4).  typematic can mess that up.
;; If all you want is to check whether C-u was tapped, use (consp P).
;; (Not (listp P), which is also t if (null P).)  If you need to know what
;; you get from n taps on C-u, you could copy this to your package:

(or (fboundp 'universal-arg-nth)
    (defun universal-arg-nth (n)
      "Value to which prefix-arg is set when C-u is pressed N times."
      (prog2
          (command-execute
           (make-vector n
                        (aref (or (where-is-internal
                                   'typematic-universal-argument '(keymap) t)
                                  (where-is-internal
                                   'universal-argument '(keymap) t)
                                  [?\C-u])
                              0)))
          prefix-arg
        (setq prefix-arg nil
              overriding-terminal-local-map nil))))

;;Test code executable with C-x C-e:
;;  (universal-arg-nth 0)
;;  (universal-arg-nth 1)
;;  (universal-arg-nth 2)
;;  (universal-arg-nth 3)

;; It'd be nice for typematic.el to provide a real equivalent to that
;; universal-arg-nth function, but the reason I didn't do that is that
;; right now (require 'typematic), which would be necessary to access that
;; universal-arg-nth function, actually changes the definition of C-u.
;; Instead, typematic-universal-argument-mode should be a minor mode.

;; There are also functions now part of standard Emacs that assume universal
;; arguments are successive squares from 4.  I grepped the library for "(4)",
;; but every instance I found by that method will still work after you
;; (require 'typematic).
;;   bytecomp.el:1297 calls byte-compile-file on '(4), but byte-compile-file
;;     just uses its arg as a boolean.
;;   compile.el:983: calls (next-error '(4)), but next-error tests whether
;;     the arg is a consp, & if so, uses it as a boolean.  Because
;;     typematic.el generates cons cells in exactly the circumstances when
;;     standard Emacs does, this works.
;;   dired-aux.el tests in 2 places with integerp; that works, same reason.
;;   edmacro.el, edt.el, & mouse.el all call (recenter '(4))).
;;     That's OK because it's equivalent to (recenter '(1)).
;;   vc.el:528: (compile-reinitialize-errors '(4) compilation-parsing-end)
;;     That works because 'compile-reinitialize-errors tests its reparse arg
;;     as a boolean.

;; That was the good news.  The bad news is that (save-buffer) in files.el
;; tests its args for numeric values of 4, 16, or 64.  Fixing this is work.

;; Grepping for "memq.*(16|64)" found no function other than (save-buffer)
;; with this problem.

;; Since (save-buffer) only allows 3 different args, it could be
;; around-advised to accept (1) == (4), (2) == (16), & (3) == (64).
;; That's actually a rather nice idea & could be done even without making
;; typematic a minor mode.  When it becomes a mode, turning the mode off
;; would also need to disable the advice.

;; Then there'll still be a problem with (save-buffer)'s docstring.
;; advice.el already advises (documentation); another advice could be used
;; for this hack, first getting the raw docstring, then editing any
;; references to 'universal-argument, then running it thru
;; (substitute-command-keys).  This docstring problem might just go away if
;; typematic-universal-argument-mode were a minor mode, because the docstring
;; doesn't explicitly identify the keymap, which I assume therefore defaults
;; to the global map, & as a minor mode, typematic-universal-argument-mode
;; would use its own modal keymap.  If (substitute-command-keys) insists on
;; accessing the modal mappings, it'd suffice to prepend "\\<global-map>" to
;; the docstring, but test first.  The likelihood that the docstring problem
;; will be solved by mode-ification means mode-ification is the first
;; improvement that should be made.  After that, the around advice is easy.

;; Yeah, but the thing is, the around advice *is* easy, whereas
;; mode-ification is hard.  I might have time for the advice first, & it's
;; orthogonal to mode-ification.

;; If there existed an Emacs function that referenced a prefix-arg of '(256)
;; explicitly, the around-advice would need to check whether
;; typematic-universal-argument-mode was in effect.  No problem, & it's
;; probably better to do it that way anyway even tho it's not necessary, just
;; because it might become necessary in the future.

;; It'd be nice if combining this package with my orthodox.el at least
;; allowed a way to still invoke the old universal-argument, but it doesn't,
;; because of all the keymapping games the universal-argument mechanism
;; plays.  Also, by design, vi-dot.el doesn't repeat a preceding
;; universal-argument invocation, incrementing the arg; it considers the arg
;; complete & applies it to the command that preceded universal-argument.
;; So if you're using typematic now in its current state & you want the
;; special features of (save-buffer), the only way to get them is by passing
;; explicit args of 4, 16, or 64.  You can do that with C-u 1 6 C-x C-s, for
;; example.  (This works because (save-buffer) declares itself (interactive
;; "p"), not "P".)  I know, it sucks, but I've never used those C-u prefixes
;; to C-x C-s; have you?  And if you do use them regularly, you could always
;; define your own functions for calling (save-buffer) with its various args,
;; & give each function its own name & key-binding.

;; This would all be a lot easier if, in simple.el, customization functions
;; universal-arg-first & universal-arg-next were provided.  These would be
;; invoked by universal-arg & universal-arg-more.  They would orthodoxily
;; return '(4) & (list (* (prefix-numeric-value ARG) 4)), &
;; typematic-universal-argument-mode would hack them appropriately.
;; Or (a matter of style) there could be variables
;; universal-arg-{first,next}-function that were orthodoxily nil, &
;; universal-arg{,-more} would test whether they contain values, & if so call
;; those values as functions, otherwise calculate directly.
;; There should also be a universal-arg-nth function built-in, & it'd be
;; easier to code than my version (altho my version would suffice) if the
;; customization functions universal-arg-{first,next} were provided.

(provide 'typematic)

;;; typematic.el ends here