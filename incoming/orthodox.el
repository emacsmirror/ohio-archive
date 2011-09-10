;;; orthodox.el --- Save the orthodox keymap before hacking it

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Mo 08 Sep 97
;; Version: 0.04, Mo 04 May 98
;; Keywords: abbrev, keymap, key map, binding, orthodox

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

;; This package is so simple that it's adequately documented by
;; the documentation string of the file's first definition:

;;; Code:

;;;###autoload
(defvar orthodox-prefix-keys '("\M-o")
  "One or more key sequences you can press followed by any key sequence that
Emacs orthodoxily (i.e. before user customization) maps to a command in its
global keymap, thereby executing that command; the effect is to let you
retain access to the entire orthodox Emacs global keymap, no matter how
radically your own customizations overwrite it.  For example, the orthodox
mapping of M-f is forward-word, but if you do a lot of C++ programming with
StudlyCapsIdentifiersLikeThis, & need to write documentation of your code in
text files not in c++-mode, you might decide to globally remap M-f to
c-forward-into-nomenclature (defined in cc-mode.el).  With this package,
you can still execute forward-word by typing M-o M-f.  This kind of thing can
be really useful if you're at somebody else's workstation & they've wildly
remapped Emacs, so you don't know where anything is; you can still execute
commands by using the orthodox prefix.

To make this work, put the line
  (require 'orthodox)
near the beginning of your .emacs file; in particular, it must be *before*
you customize any keys.

The value of the variable orthodox-prefix-keys is a list of key sequences,
each of which can be a sequence of one or more keys; each sequence becomes
a prefix by which the orthodox global keymap can be accessed.  The reason
for allowing multiple sequences is that you might need them to work around
various modal keymappings.

However, as of version 19.34.1, the default value of orthodox-prefix-keys is
a list containing just one key sequence, which contains just one key, M-o.
This is because M-o has no orthodox Emacs global mapping, so it's very
convenient to mean \"orthodox\".

To include other key sequence to orthodox-prefix-keys, put a line like
  (setq orthodox-prefix-keys '(\"\\M-o\" \"\\C-c\\C-o\"))
in your .emacs before (require 'orthodox).")

;; That's all you need to read to use this file.  All the documentation after
;; this is for programmers who want to understand how the code works.

;; The way to do this is to construct a complete copy of the global keymap
;; as it exists when (require 'orthodox) is executed, then make that copy
;; the mapping for each key sequence in orthodox-prefix-keys.
;; Here's a start at constructing the copy:

(defvar orthodox-global-map (copy-keymap (current-global-map))
  "Deeply-copied (symbolically-referenced submaps too) copy of global map.")

;; At this point, however, what that documentation string says isn't true;
;; the 'copy-keymap function did *not* copy the symbolically-referenced
;; submaps, so if a change were made now to, for example, the mapping of
;; "\M-f", aka "\ef", it'd change not only in (current-global-map) but in
;; orthodox-global-map, because it's actually part of the esc-map submap.

;; What we really need is a (defun deep-copy-keymap), but I don't have
;; time for that now.  If anybody writes one, I'd be interested in
;; incorporating it into orthodox.el; it would allow doing things
;; like capturing any global keymap at any time, including after
;; customization, & toggling between orthodox & heterodox keymaps.
;; It could also be useful for saving orthodox local keymaps before
;; mode hooks munge them.  But for now, I'll copy "by hand".

;; Here are the symbolic submaps whose values are in variables:

(define-key orthodox-global-map "\C-x"  (copy-keymap ctl-x-map))
(define-key orthodox-global-map "\C-x4" (copy-keymap ctl-x-4-map))
(define-key orthodox-global-map "\C-x5" (copy-keymap ctl-x-5-map))
(define-key orthodox-global-map "\e"    (copy-keymap esc-map))
(define-key orthodox-global-map "\C-h"  (copy-keymap help-map))
(define-key orthodox-global-map [help]  (copy-keymap help-map))
(define-key orthodox-global-map [f1]    (copy-keymap help-map))

;; Some symbolic submaps are only function values:

(define-key orthodox-global-map "\C-x6" (symbol-function '2C-command))
(define-key orthodox-global-map [f2]    (symbol-function '2C-command))

;; And in case this one might not have a definition on startup, I'll test:

(if (keymapp (symbol-function 'mode-specific-command-prefix))
    (define-key orthodox-global-map "\C-c"
      (copy-keymap (symbol-function 'mode-specific-command-prefix)))
  (message "\\C-c was undefined on startup.")
  ;; ?? Next line generates compilation warning & is sloppy anyway:
  (setq orthodox-^C-submap-undefined-on-startup t))

;; Finally we can make orthodox-global-map the definition of each element
;; of the list orthodox-prefix-keys.

(mapcar (lambda (keys) (global-set-key keys orthodox-global-map) nil)
        orthodox-prefix-keys)

;; I'd like to put mappings like [M-o C-h] into help-event-list, but I
;; couldn't find a way that would change the prompt you get from pressing
;; M-o C-h to the same one you get from pressing orthodox C-h alone.  These
;;   (nconc help-event-list '("\M-o\C-h"))
;;   (nconc help-event-list '("\eo\C-h"))
;; don't work, & neither do the more simplistic approaches like [M-o C-h].
;; (Neither 'M-o nor 'C-h normally has the plist of an input event.)

(provide 'orthodox)

;;; orthodox.el ends here