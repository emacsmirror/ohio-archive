<HEAD><TITLE>Index of /people/ttn/software/hideshow/</TITLE></HEAD><BODY>
<H1>Index of /people/ttn/software/hideshow/</H1>
<PRE>
[README extracted from hideshow.el 5.19 at 2000/06/23 13:40:04]

* Commands provided

This file provides Hideshow Minor Mode.  When active, nine commands
are available, implementing block hiding and showing.  They (and their
keybindings) are:

  hs-hide-block                      C-c h
  hs-show-block                      C-c s
  hs-hide-all                        C-c H
  hs-show-all                        C-c S
  hs-show-region                     C-c R
  hs-hide-level                      C-c L
  hs-toggle-hiding
  hs-mouse-toggle-hiding             [(shift button-2)]
  hs-hide-initial-comment-block

Blocks are defined per mode.  In c-mode, c++-mode and java-mode, they
are simply text between curly braces, while in Lisp-ish modes parens
are used.  Multi-line comment blocks can also be hidden.  Read-only
buffers are not a problem, since hideshow doesn't modify the text.

The command `M-x hs-minor-mode' toggles the minor mode or sets it
(similar to other minor modes).

* Customization

You can use `M-x customize-variable' on the following variables:

- hs-hide-comments-when-hiding-all -- self-explanatory!
- hs-hide-all-non-comment-function -- if non-nil, when doing a
                                      `hs-hide-all', this function
                                      is called w/ no arguments
- hs-isearch-open                  -- what kind of hidden blocks to
                                      open when doing isearch

Some languages (e.g., Java) are deeply nested, so the normal behavior
of `hs-hide-all' (hiding all but top-level blocks) results in very
little information shown, which is not very useful.  You can use the
variable `hs-hide-all-non-comment-function' to implement your idea of
what is more useful.  For example, the following code shows the next
nested level in addition to the top-level:

  (defun ttn-hs-hide-level-1 ()
    (hs-hide-level 1)
    (forward-sexp 1))
  (setq hs-hide-all-non-comment-function 'ttn-hs-hide-level-1)

Hideshow works w/ incremental search (isearch) by setting the variable
`hs-headline', which is the line of text at the beginning of a hidden
block that contains a match for the search.  You can have this show up
in the mode line by modifying the variable `mode-line-format'.  For
example, the following code prepends this info to the mode line:

  (unless (memq 'hs-headline mode-line-format)
    (setq mode-line-format
          (append '("-" hs-headline) mode-line-format)))

See documentation for `mode-line-format' for more info.

Hooks are run after some commands:

  hs-hide-hook     in      hs-hide-block, hs-hide-all, hs-hide-level
  hs-show-hook             hs-show-block, hs-show-all, hs-show-region

All hooks are run w/ `run-hooks'.  See docs for each variable or hook
for more info.

Normally, hideshow tries to determine appropriate values for block
and comment definitions by examining the buffer's major mode.  If
there are problems, hideshow will not activate and in that case you
may wish to override hideshow's heuristics by adding an entry to
variable `hs-special-modes-alist'.  Packages that use hideshow should
do something like:

  (let ((my-mode-hs-info '(my-mode "{{" "}}" ...)))
    (if (not (member my-mode-hs-info hs-special-modes-alist))
        (setq hs-special-modes-alist
              (cons my-mode-hs-info hs-special-modes-alist))))

If you have an entry that works particularly well, consider
submitting it for inclusion in hideshow.el.  See docstring for
`hs-special-modes-alist' for more info on the entry format.

* Suggested usage

First make sure hideshow.el is in a directory in your `load-path'.
You can optionally byte-compile it using `M-x byte-compile-file'.
Then, add the following to your ~/.emacs:

(load-library "hideshow")
(add-hook 'X-mode-hook               ; other modes similarly
          '(lambda () (hs-minor-mode 1)))

where X = {emacs-lisp,c,c++,perl,...}.  You can also manually toggle
hideshow minor mode by typing `M-x hs-minor-mode'.  After hideshow is
activated or deactivated, `hs-minor-mode-hook' is run w/ `run-hooks'.

* Bugs

(1) Hideshow does not work w/ emacs 18 because emacs 18 lacks the
    function `forward-comment' (among other things).  If someone
    writes this, please send me a copy.

(2) Sometimes `hs-headline' can become out of sync.  To reset, type
    `M-x hs-minor-mode' twice (that is, deactivate then activate
    hideshow).

(3) Hideshow 5.x is developed and tested on GNU Emacs 20.4.
    XEmacs compatibility may have bitrotted since 4.29.

(4) Some buffers can't be `byte-compile-file'd properly.  This is because
    `byte-compile-file' inserts the file to be compiled in a temporary
    buffer and switches `normal-mode' on.  In the case where you have
    `hs-hide-initial-comment-block' in `hs-minor-mode-hook', the hiding of
    the initial comment sometimes hides parts of the first statement (seems
    to be only in `normal-mode'), so there are unbalanced "(" and ")".

    The workaround is to clear `hs-minor-mode-hook' when byte-compiling:

    (defadvice byte-compile-file (around
                                  byte-compile-file-hideshow-off
                                  act)
      (let ((hs-minor-mode-hook nil))
        ad-do-it))

Correspondance welcome; please indicate version number.  Send bug
reports and inquiries to <ttn@gnu.org>.

* Thanks

Thanks go to the following people for valuable ideas, code and
bug reports.

    Dean Andrews, Alf-Ivar Holm, Holger Bauer, Christoph Conrad, Dave
    Love, Dirk Herrmann, Gael Marziou, Jan Djarv, Guillaume Leray,
    Moody Ahmad, Preston F. Crow, Lars Lindberg, Reto Zimmermann,
    Keith Sheffield, Chew Meng Kuan, Tony Lam, Pete Ware

Special thanks go to Dan Nicolaescu, who reimplemented hideshow using
overlays (rather than selective display), added isearch magic, folded
in custom.el compatibility, generalized comment handling, incorporated
mouse support, and maintained the code in general.  Version 4.0 is
largely due to his efforts.

* History

Hideshow was inspired when I learned about selective display.  It was
reimplemented to use overlays for 4.0 (see above).  WRT older history,
entries in the masterfile corresponding to versions 1.x and 2.x have
been lost.  XEmacs support is reliable as of 4.29.  State save and
restore was added in 3.5 (not widely distributed), and reliable as of
4.30.  Otherwise, the code seems stable.  Passes checkdoc as of 4.32.
Version 5.x uses new algorithms for block selection and traversal,
unbundles state save and restore, and includes more isearch support.

[README ends here]
</PRE>
<PRE>
<HR>
<A HREF="README">README</A>                        size   6571
<A HREF="hideshow.el">hideshow.el</A>                   size  35282
<A HREF="README.txt">README.txt</A>                    size   6571
<A HREF="RevLog.txt">RevLog.txt</A>                    size  31140
<A HREF="hideshow-testing.tar.gz">hideshow-testing.tar.gz</A>       size  23516
</PRE>
</BODY>
