;;; orwell.el --- Exchange [C-h] & [DEL] without affecting prefix mappings.

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini  <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Fr 05 Sep 97
;; Version: 0.37, Mo 11 May 98
;; Keywords: abbrev, emulations, terminals, del, delete, swap, exchange,
;;	keymap, key map, binding, orthodox,
;;	war, peace, freedom, slavery, erase, backspace

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

;; To use orwell, just put the line
;;   (require 'orwell)
;; in your .emacs, after copying orwell.el to a directory on your load-path.
;; `load-path' is the name of a variable that contains a list of directories
;; Emacs searches for files to load.  To prepend another directory to
;; load-path, put a line like (add-to-list 'load-path "c:/My_Directory") in
;; your .emacs file.

(message "WAR IS PEACE FREEDOM IS SLAVERY ERASE IS BACKSPACE")

;; I want C-h to delete the previous character.  I could use <F1> for help,
;; but it's harder to stretch to than <Backspace>, which is transmitted as
;; DEL.  So I want to use <Backspace> as the equivalent of orthodox C-h,
;; freeing my C-h for things like (backward-delete-char-untabify).

;; I'm not the first human to ever want this, & let me tell you a story: when
;; I first wanted to learn Emacs, the only access I had to it was on a Unix
;; shell on a dial-up ISP that I reached via an 80286 emulating a VT100, & I
;; had absolutely no documentation whatsoever; the ISP's staff were having a
;; bad-hair year, the installation didn't have a man page, & I didn't know
;; about Info yet; but I remembered being told that C-h was help.  So I tried
;; that, & C-h had been helpfully improved to backward-delete-char in the
;; site's default.el.  DEL, transmitted by <Backspace>, was also
;; backward-delete-char.  <F1> didn't mean help (the reason why not is
;; addressed below).  The only reason I was even able to EXIT THE PROGRAM,
;; the only reason I didn't need to generate a SIGHUP by hanging up the modem
;; just to get out of Emacs, was that I knew about C-x C-c from some guy who
;; used the Epsilon editor.  It was SEVERAL DAYS before I learned how to run
;; M-x help-on-help.  And that of course sucks, so I didn't learn much Emacs
;; until an upgrade by a new sysop eliminated this "improved" keymapping.
;; NEVER DO THIS AT THE SITE LEVEL!  Wanting to swap <Ctrl-h> and <Backspace>
;; is perfectly reasonable, but it should be implemented in personal .emacs
;; files, never in default.el.

;; However, as shall be revealed forthwith, this mapping is <cough>
;; nontrivial, and that's even when Emacs itself is working; the Windows port
;; of 19.34.1 at <ftp://ftp.cs.washington.edu/pub/ntemacs> can't do
;; keyboard-translate on DEL any more at all.  So anybody crazy enough to
;; figure all this out really should publish it, & here it is.

;; The most common way to solve this problem is with
;;   (keyboard-translate ?\C-h ?\C-?)
;;   (keyboard-translate ?\C-? ?\C-h)
;; which simply reverses the meanings of the 2 keys.  However, this causes
;; mappings like M-h & C-M-h to get messed up.  What I really want is for
;; the keymappings of [C-h] & [DEL] to be exchanged, but for every sequence
;; of [prefix C-h] or [prefix DEL] to remain orthodox.  Emacs is easier to
;; learn when it conforms to documentation.

;; A 26 Mar 97 Erik Naggum Usenet post suggests the character syntax has been
;; "fixed" so now ?\C-? will refer to <Ctrl-QuestionMark> & DEL will be
;; needed (no examples given; how does it denote a character?) in code like
;; that above.  But I digress.

;; So I don't want to use (keyboard-translate) to change C-h to something
;; else; I want to leave the char alone, & remap it with (global-set-key).
;; Since I can't use the first (keyboard-translate) above, I also can't use
;; the second, since that'd just make <Backspace> & <Ctrl-h> synonyms for the
;; same command.

;; But I also can't just remap DEL with (global-set-key), because
;; many modes have local mappings for it which will mask the
;; global mapping.  So what I do is translate DEL into a third
;; key that is otherwise unused, & remap that.

;; I used to do this with 'keyboard-translate, but that only affects 8-bit
;; chars, so modifiers like H- don't work.  I used "\377", aka [255].
;; This worked with Unix GNU Emacs 19.34:
;;   (keyboard-translate ?\C-? 255)
;; However, it failed mysteriously under Voelker's 19.34.1, & no fooling
;; around with notation, such as
;;   (keyboard-translate 127 255)
;; would get it to work.  All (keyboard-translate)s pertaining to ?\C-? are
;; NOPs on that NT/95 port (probably something to do with real
;; <Ctrl-QuestionMark> & <Ctrl-Virgule>).  I had to use key-translation-map
;; (see Info|Elisp|System Interface|Terminal Input|Translating Input), but
;; when I did, I discovered it doesn't have the 8-bit limitation, so I
;; switched the translated help character from [255] to [H-h]:

(or key-translation-map (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map [127] [H-h])

;; Here is code executable with C-x C-e
;;   (keyboard-translate 127 255)
;;   (keyboard-translate ?\C-? ?\C-?)
;;   (keyboard-translate ?\b 255)
;;   (keyboard-translate ?b 255)
;;   (keyboard-translate 127 127)
;;   (keyboard-translate 255 255)
;;   (keyboard-translate ?\b ?\b)
;;   (keyboard-translate ?b ?b)
;; for experimenting with 'keyboard-translate.

;; If you experiment with keyboard-translate on an ISP Unix shell, watch out
;; for a gotcha.  On a PC running MS-DOS it's possible to generate any
;; arbitrary 8-bit char using the <Alt> key & the numeric keypad.  You can,
;; for example, generate [255] by just typing
;; <DownAlt><kp-2><kp-5><kp-5><UpAlt>.  If you're using a keyboard-translate
;; of 127 to 255 (for example because my use of key-translation-map didn't
;; work for you), & try transmitting a [255] to your ISP this way, it'll
;; *seem* to work; you'll get a default display of \377, & if you try it in
;; response to a (describe-key-briefly) prompt you'll see that the mapping of
;; \377 is being displayed.  However, what can actually happen is for the
;; comm program to strip the high bit before sending the char, which will
;; therefore be received as [127] & (keyboard-translate)d back to [255],
;; giving the false impression of 8-bit transmission.

;; The mapping for [H-h] is help-command, which is (fboundp) but isn't a
;; function; it's a keymap, because the help character is a prefix key:

(global-set-key [H-h] 'help-command)

;; To get the effect that C-h C-h has in Emacs, we need to insert [H-h] into
;; help-map.  (Note that (eq help-map (symbol-function 'help-command)).)
;; However, that seems to require a mysterious conditional that could be
;; related to the inoperability of keyboard-translate of DEL on NT:

(if (eq system-type 'windows-nt)
    (define-key help-map [backspace] 'help-for-help)
  (define-key help-map [H-h] 'help-for-help))

;; It's been my experience that mysteries that weird usually result from
;; programmer error, but hey, I've spent *hours* on this, so if you know
;; what's wrong, you fix it.  You need to test on both Unix & Windows.

;; This global variable is used in Emacs documentation & some Emacs code:

(setq help-char ?\H-h)

;; In loaddefs.el there's a help-event-list variable that doesn't include
;; C-h, just f1 & help.  Why isn't C-h in there?  Bafflement.  Anyway,
;; putting the new event into that list causes the message printed to be more
;; meaningful: "H-h (Type ? for further options)" instead of just "H-h-".

(setq help-event-list (cons 'H-h help-event-list))

;; Note that consing 'H-h onto the front of help-event-list causes it to be
;; the preferred char in some messages that you'd think would use help-char
;; but seem to actually use (car help-event-list).  This can become a pain
;; when more help chars need to be added, because sequence matters.
;; I handle this below by using nconc instead of cons.

;; ?? For some reason, I still can't get ?\H-h to work when I set
;; minibuffer-help-form to non-nil, whereas <F1> works in that case, even
;; though help-char is definitely ?\H-h.

;; Finally, C-h can be remapped.  Remember I didn't want to do
;;   (keyboard-translate ?\C-h ?\C-?)
;; because it messes up other mappings that use C-h, like C-M-h.

;; Another conventional approach would be
;;   (global-set-key "\C-h" 'backward-delete-char-untabify)
;; but we have another problem to think about: at this point there's no way
;; left to type a DEL & have Emacs receive it, so there's no way to run
;; whatever is mapped to DEL.  I can't usefully put a reciprocal mapping into
;; key-translation-map, because most personal-computer keyboards don't have
;; a Hyper key.  (There is, by the way, theoretically, a way to generate
;; Hyper characters on any terminal: C-x @ h is equivalent to holding down
;; the Hyper key, so C-x @ h h should be equivalent to H-h.  Well, I've found
;; that it doesn't work for generating this help char, possibly because the
;; mapping is a keymap rather than a function.  The C-x @ <whatever>
;; sequences do work in most cases.  I confess to being a bit curious why
;; event-apply-{control,shift}-modifier are mapped to keys that can only
;; be generated by using both control & shift modifiers; well, as Zathras
;; would say, "very sad life...probably have very sad death...but, at least
;; there is symmetry".  (Put them on the menu bar?)  But again I digress.)

;; What I really want is to map C-h to do whatever DEL does, not just
;; hardwired as (backward-delete-char-untabify), but *whatever* any local
;; buffer's major mode has currently mapped it to; but I want to do this
;; without using key-translation-map, so other sequences containing C-h
;; are unaffected.  This accomplishes that for most cases:

(global-set-key "\C-h" (lambda () (interactive)
  (call-interactively (key-binding "\C-?"))))

;; Unfortunately, that's not good enough for isearch.el v19.34.1, which needs
;; to make a special distinction between control characters that affect the
;; buffer & those that affect the search string.  For example, if you type
;; your (delete-char) binding inside an isearch, & it's a control char,
;; isearch will delete the char under point *in the buffer* you're searching;
;; but if you haven't messed with C-h & DEL & you type DEL (normally mapped
;; to (backward-delete-char)) in an isearch, isearch will delete the char
;; before point *in the search string* & backtrack the search to wherever
;; it'd be if the char had never been typed.  To do this isearch needs to
;; create its own mapping invoking this special function, & it hardwires it
;; to DEL; & now after my above translations there's no way to type
;; DEL at all.  It's necessary to hack the map in a hook.  Since isearch has
;; some cool features users may need to review the mappings of, the hook
;; should also ensure that the new help char works in isearch:

(add-hook 'isearch-mode-hook '(lambda ()
  (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
  (define-key isearch-mode-map [H-h] 'isearch-mode-help)))

;; <sigh> Looking at the latest code, I see that RMS turned off the original
;; mapping of C-h to the help char in isearch-mode-map, so it may be
;; necessary to make some special accommodation here.  It doesn't work
;; to dynamically examine the map to see whether the user has turned C-h
;; back on, because this code will be executed before isearch is loaded.
;; Probably isearch.el itself should be hacked to deal with this; it should
;; have a defvar toggling whether help-char calls global help or
;; isearch-mode-help, & a straightforward way to specify some other char to
;; call isearch-mode-help.  (You know, if you want global help, it's pretty
;; straightforward to just terminate the isearch first with C-m, & most users
;; will never find out about the special help isearch offers unless the
;; default is for that to be what they get pressing help-char in isearch.
;; That help could include mention of the variable to toggle if you just
;; want global help instead.  If none of that ever happens you can still live
;; a happy life with (require 'orwell), because the code above overrides
;; isearch's map, whatever that's been hacked to.  While we're at it, if
;; you want this to be a user option, you don't even need a defvar, just
;; a remapping in isearch-hook.  If you're RMS-like, do this:
;; (add-hook 'isearch-mode-hook '(lambda ()
;;   (define-key isearch-mode-map "\C-h" 'help-command)))
;; If you're Orwellian but want global instead of isearch help, do this:
;; (add-hook 'isearch-mode-hook '(lambda ()
;;   (define-key isearch-mode-map [H-h] 'help-command)))
;; <sigh>.)

;; The next problem is that on a remote 7-bit terminal where the only way to
;; type M-DEL is <Esc><Backspace>, that mapping has now become inaccessible
;; because of the translation of DEL.  So we need to use a similar dynamic
;; access to the key binding:

(global-set-key [?\e H-h] (lambda () (interactive)
  (call-interactively (key-binding "\e\C-?"))))

;; C-x DEL is orthodoxily mapped to 'backward-kill-sentence:

(global-set-key [?\C-x H-h] (lambda () (interactive)
  (call-interactively (key-binding "\C-x\C-?"))))

;; Any other prefix sequences involving DEL can be hacked accordingly.
;; One that needs to be for remote 7-bit access to Emacs from a PC is
;; C-M-DEL, which normally maps to (backward-kill-sexp).  (See the GNU
;; Emacs manual 21.3, since there's some confusion over whether DEL &
;; delete are the same thing; the manual says C-M-DEL, but all the functions
;; concerned with keymapping use C-M-delete.)  This isn't a problem caused
;; by the translation of DEL above; there simply is no way to type
;; that character on this kind of terminal.  There isn't even a difference
;; between what <Backspace> and <Ctrl-Backspace> transmit; they're both DEL.
;; (Why isn't there an event-apply-control-meta-modifier function?)
;; The best I can do is think that since ESC is a M- prefix, ESC ESC might
;; as well be a C-M- prefix.  <Esc><Esc><Backspace> can become C-M-delete:

(global-set-key [?\e ?\e H-h] (lambda () (interactive)
  (call-interactively (key-binding [C-M-delete]))))

;; On the Windows port, C-M-DEL Just Works, for a change.  Note that this
;; sounds scarily like the Vulcan Nerve Pinch, but that's only because the
;; Unix world often uses "DEL" to mean <Backspace>, not <Delete>.

;; THE RESULT SO FAR: At this point the effect has been to make DEL the help
;; character overriding the local keymaps that would supercede a
;; (global-set-key) approach, but to leave [prefix DEL] with its orthodox
;; mappings; & to make C-h do what DEL used to do regardless of what that
;; was, respecting local as well as global keymaps, while leaving all
;; [prefix C-h] sequences unmodified.

;; I'd still like a dynamic way to automatically cope with *all* sequences
;; involving DEL, but I haven't thought of one.  The best I can do is make it
;; possible to toggle whether the translation of DEL actually occurs; toggle
;; it off, & <Backspace> is back to being DEL.

(defvar translation-of-127-is-toggled-off nil
  "Whether orwell.el's hack to DEL is temporarily toggled off.
This variable should go away when orwell becomes a minor mode.")

(defun del ()
  "Toggle whether <Backspace> generates [DEL] or [H-h]."
  (interactive)
  (if translation-of-127-is-toggled-off
      ;; Toggle it back on
      (progn
        (define-key key-translation-map [127] [H-h])
        (message "<Backspace> is now H-h, the help char; use C-h to erase.")
        (setq translation-of-127-is-toggled-off nil))
    ;; Toggle it off
    (define-key key-translation-map [127] [127])
    (message "<Backspace> is now DEL; use <F1> for help.")
    (setq translation-of-127-is-toggled-off t)))

;; You can see that's obviously an afterthought; really, now that the tech
;; has been worked out, this whole file should be recoded, with all the
;; improvements that it makes toggleable.  It could even be a minor mode, but
;; I think that most of the time having it take space on the mode line would
;; be undesirable, since most of the time its users will have it always on.
;; (For an example of the opposite situation, I think toggle-rot13-mode errs
;; in *not* being a full-fledged minor mode; if you're temporarily using a
;; weird display table, you want the modeline to warn you about it.)
;; A better idea would be for every press of C-h to get a message saying
;; which button to press to get help (NOT "[H-h]", which would be meaningless
;; to most users who might need that message; the message needs to be
;; selected at load time based on the kind of system running orwell, so
;; reasonable guesses can be made about what the keyboard looks like).
;; That can be done without cluttering up *Messages* by using a form like
;; (let (message-log-max) (message "Hit %s for help." whatever)).

;; One more thing, pertaining to the <F1> key.

;; Notation: by "[f1]" I mean whatever keyboard event Emacs construes
;; as [f1].  By "<F1>" I mean pressing the so-labeled key on a terminal.
;; These are not the same things.

;; I want the <F1> key on a PC, when talking to an ISP Unix shell by way of a
;; comm program running a VT100 emulation, to have the standard meaning it has
;; in the PC world: pressing the key gets help.  19.34.1 already so maps [f1].
;; However, via a VT100 emulation, an <F1> is transmitted to Emacs not as [f1]
;; but as [kp-f1].  Because of the importance of help, I think it's a bug in
;; help.el that that keymapping isn't included along with [f1] by default.

(global-set-key [kp-f1] 'help-command)

;; And that requires this:

(nconc help-event-list (list 'kp-f1))

;; Note that consing onto the front would be bad, because it changes the car.
;; It's also necessary to put [kp-f1] into help-map as help-for-help.

(define-key help-map [kp-f1] 'help-for-help)

;; Well, this has been fun.

(provide 'orwell)

;;; orwell.el ends here