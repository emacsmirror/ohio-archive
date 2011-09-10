;;; swiftex.el --- major modes for LaTeX and LaTeX doc.sty documents

;;;; COPYRIGHT NOTICE AND PRELIMINARIES
;;;
;;; Copyright (C) 1995 Matt Swift <swift@bu.edu>
;;;
;; Author:  Matt Swift <swift@bu.edu>
;;	Johanes Braams
;;	Frank Mittelbach
;; Maintainer:  Matt Swift <swift@bu.edu>
;; Version:  $Revision: 1.23 $
;; Keywords: tex, latex, doc, doc.sty, ltxdoc
;; LCD Archive Entry:
;; swifTeX|Matt Swift|swift@bu.edu|
;; Major modes for LaTeX and LaTeX doc.sty documents|
;; 21-Apr-1995|1.23|~/modes/swiftex.el.gz|
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; Commentary:
;;;
;;;; INTRODUCTION
;;;
;;; This file defines major modes swifTeX and docTeX.  
;;;
;;; swifTeX mode is for editing buffers containing normal LaTeX files, and
;;; provides an alternative to the LateX modes in the standard Emacs
;;; distribution and the AUC TeX package.
;;;
;;; docTeX mode is for editing buffers containing self-documenting LaTeX code
;;; that uses the package "doc.sty", including the document class "ltxdoc".
;;; For these buffers, docTeX mode is significantly more useful than the
;;; alternatives provided by standard Emacs and AUC TeX.
;;;
;;; These modes are derived from the LaTeX mode in "tex-mode.el" (distributed
;;; with GNU Emacs) using the autoloaded function `define-derived-mode' in
;;; "derived.el" (also part of the standard distribution).
;;;
;;; The principal additions allow convenient, precise control over filling and
;;; outlining.  There are also a few improvements to help save typing.  In
;;; docTeX mode, functions are provided to help manage LaTeX file versions and
;;; dates.  These major modes are fully documented and well-tested.
;;;
;;; ************************************************************
;;; * WARNING:  BYTE-COMPILE THIS FILE ONLY WITH A FRESH EMACS *
;;; * started with the "-q" command line option, UNLESS you    *
;;; * have GNU Emacs 19.29 or higher, or you apply the patch   *
;;; * to bytecomp.el given below.  The patch is not guaranteed *
;;; * to work, so I recommend always compiling with a fresh    *
;;; * Emacs.                                                   *
;;; *                                                          *
;;; * (At the time of this release, 19.29 does not exist.)     *
;;; ************************************************************
;;;
;;;
;;;; INSTALLATION
;;;
;;; Please see the warning about byte-compiling above.
;;;
;;; You will probably want to put lines like the following into your .emacs.
;;; (1) The autoload declarations will cause this file to be loaded whenever
;;;     `doctex-mode' or `swiftex-mode' is called.  
;;; (2) The entries in `auto-mode-alist' will cause either `swiftex-mode' or
;;;     `doctex-mode' to be called when a file with one of the named extensions
;;;      is loaded.  Each entry is independent and can be omitted. 
;;; (3) The key bindings are the ones I prefer.  They are not made the default
;;;     bindings because it is against convention for a major mode to bind
;;;     `C-c <letter>'.  You will probably want to bind the last three useful
;;;     functions to something, if not what I suggest below, since they are not
;;;     initially bound to anything at all.
;;; 
;;; (autoload 'doctex-mode "swiftex" "Major mode for LaTeX doc documents.")
;;; (autoload 'swiftex-mode "swiftex" "Major mode for LaTeX documents.")
;;;
;;; (defun stx-merge-list (old new)
;;;   "Modify an list OLD to include all NEW's elements not in OLD.
;;; Compare elements with `equal'.  New elements are added at
;;; the end of OLD.  NEW and OLD can both be lists or both
;;; alists.  Returns OLD.
;;;
;;; For alists, add NEW keys to end of OLD if OLD does not
;;; have them.  If OLD does have a matching key, change its
;;; value to NEW's value for that key.  The key is the car of
;;; each element, the value is the cdr."
;;; (let ((p (listp (car old))))
;;;   ;; ensure by first element that args are either both lists or both
;;;   ;; alists
;;;   (if (not (eq p (listp (car new))))
;;;       (error "Args to `stx-merge-list' must be both lists or both alists!")
;;;     (if p
;;;      ;; `old' and `new' are alists
;;;      (while new
;;;        (let* ((new-cons (car new))
;;;               (old-cons (assoc (car new-cons) old)))
;;;          (if old-cons
;;;              (setcdr old-cons (cdr new-cons))
;;;            (nconc old (list new-cons)))
;;;          (setq new (cdr new))))
;;;       ;; `old' and `new' are regular lists
;;;       (while new
;;;      (let ((x (car new)))
;;;        (or (member x old)
;;;            (nconc old (list x)))
;;;        (setq new (cdr new)))))
;;;     old)))
;;;
;;; ;; These will only replace entries in `auto-mode-alist' with identical
;;; ;; first parts; watch out for ending up with two entries that match the
;;; ;; same file names, such as "\\.tex$" and "\\.tex\\'".
;;; (stx-merge-list auto-mode-alist '(
;;;                 ("\\.bbl\\'" . swiftex-mode) ("\\.aux\\'" . swiftex-mode)
;;;                 ("\\.tex\\'" . swiftex-mode) ("\\.tmpl\\'" . swiftex-mode)
;;;                 ("\\.ltx\\'" . swiftex-mode) ("\\.notes\\'" . swiftex-mode)
;;;                 ("\\.ins\\'" . swiftex-mode) ("\\.cfg\\'" . swiftex-mode)
;;;                 ("\\.cls\\'" . doctex-mode) ("\\.sty\\'" . doctex-mode) 
;;;                 ("\\.dtx\\'" . doctex-mode) ("\\.fdd\\'" . doctex-mode)
;;;
;;; (eval-after-load "swiftex" 'swiftex-startup)
;;; (defun swiftex-startup ()
;;;   "Onetime customization to be executed after loading \"swiftex.el\"."
;;;   (define-key swiftex-mode-map     "\C-cm"    'stx-emphasize)
;;;   (define-key swiftex-mode-map     "\C-ce"    'stx-close-block-from-inside)
;;;   (define-key swiftex-mode-map     "\C-cB"    'stx-insert-block)
;;;   (define-key swiftex-mode-map     "\C-cb"    'stx-begin-block)
;;;   ;; These next three are the way I like to use braces, but this is best
;;;   ;; left as a personal choice.
;;;   (define-key swiftex-mode-map     "\M-{"     'self-insert-command)
;;;   (define-key swiftex-mode-map     "\M-}"     'self-insert-command)
;;;   (define-key swiftex-mode-map     "{"        'tex-insert-braces))
;;;
;;;
;;;;; PATCH:
;;; 
;;; If you are using GNU Emacs 19.28 or earlier, I recommand byte-compiling
;;; this file only with a fresh Emacs process started with the "-q" command
;;; line option.  Sometimes byte-compiling this file when it is itself loaded
;;; leads to faulty compilation.  This bug has been reported and will be fixed
;;; in versions of GNU Emacs > 19.28.
;;;
;;; Alternatively, you can try the following patch to bytecomp.el.  I'm not
;;; sure that it will completely get rid of the problem, so if you notice
;;; strange errors, please reproduce them on an uncompiled version before
;;; sending them to me.
;;;
;;; To apply the patch, make a backup of your bytecomp.el, and then modify
;;; bytecomp.el by adding the definition for the function
;;; `get-buffer-create-protect' given below.  Then change the four calls to
;;; `get-buffer-create' to calls to `get-buffer-create-protect'.  Recompile
;;; bytecomp.el.  
;;;
;;; (defun get-buffer-create-protect (arg)
;;;   "A wrapper for `get-buffer-create' in a protected environment.
;;; This ensures the value of `default-major-mode' is 'emacs-lisp-mode."
;;;   (let ((default-major-mode 'emacs-lisp-mode))
;;;     (get-buffer-create arg)))
;;; 
;;;
;;;; USER DOCUMENTATION
;;;
;;; The user functions are described below.  Next to each one is the initial
;;; key binding, or a suggested one in parentheses, or both.  A major mode
;;; should by convention not bind `C-c <letter>'; where this is my preferred
;;; binding, I have put the binding in the comments in the installation
;;; section above, and left the function unbound, or in one case bound to a
;;; similar key. 
;;;
;;;;;   SWIFTEX MODE
;;; 
;;; swifTeX mode provides all the functionality of the standard LaTeX mode,
;;; plus the following extensions.  In the case of tab and C-c C-u, the
;;; standard functions have been replaced by improved ones that act similarly.
;;; 
;;;
;;;****************
;;;* swiftex-mode *
;;;****************
;;;
;;; Select swifTeX major mode.  See the installation section for useful ways to
;;; have swifTeX mode come up automatically.
;;;
;;;*****************
;;;* stx-tab:  TAB *
;;;*****************
;;;
;;; If you have tabs in a LaTeX buffer, you will be in for some surprises when
;;; you use a verbatim environment, so we define a tabbing command that only
;;; inserts spaces.  I can't imagine that the savings of file-size is at all
;;; significant using tabs instead of spaces.  If you decide to reinstate
;;; normal tabbing for swifTeX mode, I strongly recommend you keep the new
;;; space-tabbing for docTeX mode because every macrocode environment is a
;;; verbatim environment.
;;;
;;;***************************************
;;;* stx-next-braces:  M-TAB and C-c TAB *
;;;***************************************
;;;
;;; Move point out of the current set of braces and just into the next. 
;;;
;;;**************************
;;;* stx-up-block:  C-c C-] *
;;;**************************
;;;
;;; Move point out of the current \begin{}-\end{} environment. 
;;;
;;;*****************************
;;;* stx-begin-block:  (C-c b) *
;;;*****************************
;;;
;;; Insert \begin{*} and leave point where the * is.
;;;
;;;******************************
;;;* stx-insert-block:  (C-c B) *
;;;******************************
;;;
;;; Alias for standard `tex-latex-block'.  Create a matching pair of
;;; \begin{}-\end{} lines and leave point on a blank line between them.
;;;
;;;*****************************************
;;;* stx-close-block-from-inside:  (C-c e) *
;;;*****************************************
;;;
;;; Let * be the point.
;;; Initial buffer contents:
;;;
;;;   \begin{text*}
;;;
;;; Final buffer contents:
;;;
;;;   \begin{text}
;;;   *
;;;   \end{text}
;;;
;;;*****************************
;;;* stx-close-block:  C-c C-e *
;;;*****************************
;;;
;;; Close the last unclosed \begin{}.  Duplicate any legal prefix to the
;;; \begin{}.
;;;
;;;*****************************************
;;;* stx-goto-last-unended-begin:  C-c C-u *
;;;*****************************************
;;;
;;; Move point to the last unended \begin{} above. 
;;;
;;;***************************
;;;* stx-emphasize:  (C-c m) *
;;;***************************
;;;
;;; Insert \emph{*} and leave point at *.
;;;
;;;********************************
;;;* stx-emphasize-word:  C-c C-m *
;;;********************************
;;;
;;; Surround word at point with "\emph{" and "}".
;;;
;;;*********************************************
;;;* dtx-insert-change:              C-c C-d g *
;;;* add-change-log-entry:           C-c C-d l *
;;;* dtx-get-fileinfo:               C-c C-d v *
;;;* dtx-update-minor-version:       C-c C-d u *
;;;* dtx-update-major-version:       C-c C-d U *
;;;* dtx-update-documentation-date:  C-c C-d D *
;;;*********************************************
;;;
;;; All the docTeX mode commands that make sense to use in a swifTeX buffer are
;;; available on the same keys as in docTeX mode.  See below for what these
;;; functions do.
;;;
;;;;;   DOCTEX MODE
;;;
;;; docTeX mode is intended to behave as you would want swifTeX mode to behave
;;; in doc.sty buffers.  
;;;
;;;
;;;***************
;;;* doctex-mode *
;;;***************
;;;
;;; Select docTeX major mode.  See the installation section for useful ways to
;;; have docTeX mode come up automatically.
;;;
;;;*********************************************
;;;* dtx-update-minor-version:       C-c C-d u *
;;;* dtx-update-major-version:       C-c C-d U *
;;;* dtx-update-documentation-date:  C-c C-d d *
;;;*********************************************
;;;
;;; A <version> is any number of integers separated by dots with an optional
;;; prefix "v" ;and an optional suffix of a lowercase letter.  When the version
;;; is updated, a "v" will always be added.
;;;
;;; The major and minor version strings will be the last two elements of the
;;; <version>, counting the possible suffix as a separate element if it exists.
;;;
;;; Updates are done by looking for the first line that defines \fileversion in
;;; the buffer (with \def or \newcommand) as a <version>. If no such line
;;; exists, then the line with \ProvidesPackage or \ProvidesClass is updated.
;;;
;;; When the major or minor version is updated, the date is updated.  Once
;;; again, this is either the line defining \filedate, or the right part of the
;;; \ProvidesPackage (or Class) optional argument.
;;;
;;; The documentation date is similarly found on the line defining \docdate.
;;;
;;;********************************
;;;* dtx-get-fileinfo:  C-c C-d v *
;;;********************************
;;;
;;; Display current file version in the mini-buffer.
;;;
;;;******************
;;;* dtx-begin-FOO  *        
;;;* dtx-insert-FOO *
;;;******************
;;;
;;;   FOO           begin        insert      
;;; =======================================
;;; macrocode:    C-c C-d c    C-c C-d C
;;; macro:        C-c C-d m    C-c C-d M
;;; environment:  C-c C-d e    C-c C-d E
;;; bibfunction:  C-c C-d f    C-c C-d F
;;;
;;; The `insert' commands (on uppercase keys) begin and end the relevant
;;; environment, and leave point in between\; `begin' commands (on lowercase
;;; keys) just begin the environment.  All prompt for the name of the thing in
;;; question.
;;;
;;;***************************************
;;;* dtx-interrupt-macrocode:  C-c C-d i *
;;;***************************************
;;;
;;; Interrupt a macrocode environment to add some commentary.
;;;
;;;************************************
;;;* add-change-log-entry:  C-c C-d l *
;;;************************************
;;;
;;; Make a ChangeLog entry.  
;;;
;;;*********************************
;;;* dtx-insert-change:  C-c C-d g *
;;;*********************************
;;;
;;; Insert a LaTeX changes entry, prompting for the description of the change.
;;; 
;;;
;;;; SYMBOL NAMES:
;;;
;;; The prefixes `stx' or `dtx' begin all new symbol names, the latter being
;;; for symbols unique to docTeX mode.
;;;
;;; There are some exceptions that begin with `swiftex', `doctex', or `rx'. 
;;; The actual mode functions, the mode maps, and variables holding the prefix
;;; keys are:
;;;   `swiftex-mode'            `doctex-mode'
;;;   `swiftex-mode-map'        `doctex-mode-map'
;;;   `swiftex-dtx-mode-map'    `doctex-dtx-mode-map'
;;;   `swiftex-dtx-prefix-key'  `doctex-dtx-prefix-key'.
;;;
;;; A group of constants have names beginning with `rx', short for "regexp".
;;;
;;;
;;;; HISTORY AND CREDITS
;;;
;;;;;   OVERVIEW
;;;
;;; These major modes are the result of my own hacking over the years, adding
;;; to the standard LaTeX major mode.
;;;
;;; Once I got used to my developments, switching to AUC TeX seemed like too
;;; much of a pain in the neck.  Perhaps someone will add this functionality to
;;; AUC-TeX's one day.  I don't expect many people besides me will want to use
;;; swifTeX mode except in its extension of docTeX mode.  Do let me know if you
;;; like swifTeX mode.
;;;
;;; DocTeX mode began its life on 14 November 1994 when David Carlisle of the
;;; LaTeX 3 team gave me some Emacs lisp code written by fellow LaTeX 3 team
;;; members Johanes Brahms and Frank Mittelbach.  That file contained functions
;;; for handling version control, inserting changes, and inserting the basic
;;; doc.sty environments.  I used these functions to build a new major mode in
;;; parallel with swifTeX mode.
;;;
;;; The original authors have given me permission to incorporate their code in
;;; mine, but they do not otherwise endorse or support swiftex.el.  I, Matt
;;; Swift <swift@bu.edu>, am the sole maintainer.
;;; 
;;;;;   1.20 - 1.23
;;;
;;; + Fixed `stx-enclose-word' which was screwing up the undo list in certain
;;;   situations and causing Emacs to hang entirely.  Oops.
;;; + omitted making `paragraph-' vars local since tex-mode does this
;;; + changed where `dtx-interrupt-macrocode' leaves point
;;; + removed "whitespace" from `stx-command-par{starts,seps}" -- nonstandard
;;;   
;;;;;   1.19 - 1.20
;;;
;;; + improved the suggested .emacs lines -- the nconc was not a good idea
;;; + returned default brace behavior to standard -- let users ask for my
;;;   peculiar way if they want it
;;; + added frontmatter mainmatter backmatter begin{document} and documentclass
;;;   to `stx-sectioning-commands'
;;; + changed the definitions of `foo-dtx-mode-map' to use defvar not setq
;;; + moved define-keys out of the mode definitions -- was inefficient
;;; + added sentence that GNU Emacs 19.29 is not yet out
;;; + suggested compiling with fresh Emacs as alternative to applying the patch
;;; + improved docstring for `stx-set-outlinevars' and distinguished its two
;;;   error messages
;;; + mentioned `stx-local-' variables in the two mode docstrings
;;; + altered obsolete reference to `stx-dtx-prefix-key' in swifTeX docstring
;;; + altered top comment to read "swiftex.el" instead of "swiftex-mode.el"
;;;
;;;;;   ORIGINAL HISTORY
;;; This is the original history from the file I was given:
;;;   ; doc-mode.el
;;;   ;
;;;   ; Support for doc.sty style of (La)TeX programing.
;;;   ;
;;;   ; The code was started by Frank Mittelbach
;;;   ; modified and extended by Johanes Braams.
;;;   ; date 1994/05/10
;;;   ; version 1.0g
;;;   ; author Johanes Braams
;;;   ;
;;;   ; changes
;;;   ; 1.0g 1994/05/05 Made arg of doc-mode optional to allow the
;;;   ;                 following code in a .emacs file:
;;;   ;                 (autoload 'doc-mode "doc-mode")
;;;   ;                 (setq auto-mode-alist 
;;;   ;                   (cons '("\\.dtx$" . doc-mode) auto-mode-alist))
;;;   ;                 Thanks to Rainer for the suggestion.
;;;   ; 1.0f 1994/04/30 Now also possible to get and update the version 
;;;   ;                 information from the \Provides... macros;
;;;   ;                 Always put a 'v' in front of the version number.
;;;   ; 1.0e 1994/03/16 Allow for leading 'v' in version numbers
;;;   ; 1.0d 1994/03/02 Added keybinding \C-ce and \C-cC
;;;   ; 1.0c 1994/02/23 reverted change to doc-insert-change; I got mixed up.
;;;   ; 1.0b 1994/02/22 made version variables buffer-local;
;;;   ;                 doc-insert-change inserted arguments of
;;;   ;                 \changes in the wrong order. 
;;;   ; 1.0a 1994/02/18 original version
;;;

;;;; TO DO
;;;
;;; Undoing `dtx-interrupt-macrocode' leaves point in the wrong place.  I
;;; should check and correct the undoing of all the functions.
;;;
;;; Someday I should pore over AUC TeX, do what it takes to get the
;;; functionality I want out of it, and submit improvements to it, so there
;;; won't be a reason to have swifTeX and docTeX lying around.  I hope one day
;;; I will, or that someone else will get AUC TeX up to speed with regard to
;;; LaTeX2e.
;;;
;;; This mode is about as developed as it will get before a major overhaul.
;;; Using the paragraph variables to control filling has worked up until now,
;;; but the next stage should be a rewriting of the filling functions.
;;;
;;; We could make a alist variable `stx-outline-heading' whose entries would be
;;; an outline heading and a function to compute the level for it.  This would
;;; work roughly like a more general `stx-sectioning-commands'.  We could add
;;; local counterparts to both those variables, for setting as file variables.
;;; If anyone sees a good reason to do this I'll do it.
;;;
;;; We could avoid the extra call to `stx-set-vars' in the execution of
;;; `doctex-mode'.  Life is short.
;;;

;;;; IMPLEMENTATION
;;; CODE:

;;;;; GENERAL ACTIONS
(provide 'swiftex)
(require 'tex-mode)
(require 'outline)
(defconst stx-version "$Revision: 1.23 $"
  "RCS version of Matt's master swiftex.el.")

;;;;; REGEXP CONSTANTS

;;; Using the following constants makes regular expressions more readable in
;;; Lisp code.

(defconst rx-whitespace "\\s-*"
  "Matches optional characters with whitespace syntax.")
(defconst rx-normal-boln "^"
  "Matches unhidden beginning of line.")
(defconst rx-hidden-boln "[\^M]"
  "Matches beginning of line when hidden in outline mode.")
(defconst rx-eoln "$")
(defconst rx-normal-whiteln (concat rx-normal-boln rx-whitespace rx-eoln)
  "Matches empty line.")
(defconst rx-dollar "\\$"
  "Matches a dollar sign.")
(defconst rx-backslash "\\\\"
  "Matches a backslash.")
(defconst rx-white-back (concat rx-whitespace rx-backslash)
  "Matches optional whitespace plus backslash." )
(defconst rx-or "\\|"
  "Regexp string for logical OR." )
(defconst rx-brace-pair "{[^}\n]*}"
  "Matches one balanced set of braces.")
(defconst rx-tex-grop "{"
  "Matches TeX group open character -- a left brace.
Also (ab)used as a regular string for insertions.")
(defconst rx-tex-grcl "}"
  "Matches TeX group close character -- a right brace.
Also (ab)used as a regular string for insertions.")
(defconst rx-tex-def-cmd "\\(def\\|\\(re\\)?newcommand\\)"
  "Matches a (La)TeX defining command without its argument.")
(defconst rx-dtx-version "\\(v?\\)\\([0-9.]*\\)\\([a-z]?\\)"
  "Matches a LaTeX version string.")
(defconst rx-dtx-date "[0-9]+/[0-9]+/[0-9]+"
  "Matches a LaTeX date string.")

;;;;; SWIFTEX MODE

;;; Much of what is defined below takes docTeX mode into account.  Only the
;;; code forms unique to docTeX mode appear in its section below.

;;;;;;        USER VARIABLES

(defvar swiftex-dtx-prefix-key '"\C-c\C-d"
  "*Prefix key for certain docTeX mode commands available in swifTeX mode.
A change in this variable takes effect the next time
`swiftex-mode' is called.")

(defvar stx-sectioning-offset 1
  "*Outline level offset for all LaTeX sectioning commands in a swifTeX buffer.
See the documentation for the variable
`stx-sectioning-commands'.")

(defvar stx-sectioning-commands '(
				  ("documentclass" . -2)
				  ("begin{document}" . -2)
				  ("end{document}" . -2)
				  ("endinput" . -2)
				  ("part" . -2)
				  ("frontmatter" . -1)
				  ("mainmatter" . -1)
				  ("backmatter" . -1)
				  ("chapter" . -1)
				  ("section".  0)
				  ("subsection" . 1)
				  ("paragraph" . 2)
				  ("subparagraph" . 3)
				  )
  "*An alist of LaTeX sectioning commands with level offsets for swifTeX mode.
The list is in the form (NAME . NUMBER) where NAME is a
string containing a LaTeX sectioning command with no
backslash, and NUMBER is an integer which will be added to
`stx-sectioning-offset' to get the actual outline level.

The normal top-level outline heading of `%*' is level 1.
The default offset for \"\\section\" is 0, so its default
level is 1.  For documents with chapters, you might want to
set `stx-sectioning-offset' to 2, so that \"\\chapter\" and
\"%*\" are the same level.

An \"endinput\" entry, if it exists, is treated specially in
docTeX mode.  See `stx-set-outlinevars'. 

Changes to `stx-sectioning-offset' and
`stx-sectioning-commands' will take effect the next time
`stx-set-vars' is called.  This is normally called only by
`swiftex-mode' and `doctex-mode'.")

(defvar stx-local-command-parstarts nil
  "*Partial list of LaTeX commands that start or separate Emacs paragraphs.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'

When these regexp strings occur after optional whitespace
and a backslash, those not in `paragraph-separate' start
Emacs paragraphs, and receive text on their line during
fills.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or'), precedes them by optional
whitespace and a backslash, and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

See `stx-local-parstarts' for a way to add starters that are
not LaTeX commands.

See also `stx-local-parseps'.")
(make-variable-buffer-local 'stx-local-command-parstarts)

(defvar stx-local-command-parseps nil
  "*Partial list of LaTeX commands that separate Emacs paragraphs.
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

When these regexp strings occur after optional whitespace
and a backslash, they separate Emacs paragraphs, and resist
being filled.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or'), precedes them by optional
whitespace and a backslash, and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not add strings to this list without also adding them to
`stx-local-command-parstarts', or otherwise ensuring they
make it into `paragraph-start'.

See `stx-local-parseps' for a way to add separators that are
not LaTeX commands.")
(make-variable-buffer-local 'stx-local-command-parseps)

(defvar stx-local-parstarts nil
  "*A part of the variable `paragraph-start' in swifTeX and docTeX modes. 
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

The regexp strings in this list that are not in
`paragraph-separate' start Emacs paragraphs, and receive
text on their line during a fill.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-start' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they get into
`paragraph-separate' via `outline-regexp'.

See `stx-local-command-parstarts' for an easy way to add
starters that are LaTeX commands.

See also `stx-local-parseps'.")
(make-variable-buffer-local 'stx-local-parstarts)

(defvar stx-local-parseps nil
  "*A part of the variable `parargraph-separate' in swifTeX and docTeX modes. 
This variable exists so that it can be set as a file
variable.  After the line that sets it, include a line with
`eval: stx-set-vars'.

The regexp strings in this list separate Emacs paragraphs,
and resist being filled.

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not add strings to this list without also adding them to
`stx-local-parstarts', or otherwise ensuring they make it
into `paragraph-start'.

See `stx-local-command-parseps' for an easy way to add
separators that are LaTeX commands.")
(make-variable-buffer-local 'stx-local-parseps)

(defvar stx-command-parseps '(
			;;; below ought to match first section of
			;;; `stx-command-parstarts' exactly:
				"begin" "end" 
				"par"
				"opening" "closing"
				"\\[" "caption" "label"	"usepackage"
				"documentclass" "date" "author" "title"
				"tableofcontents" "maketitle"
				"listoffigures" "listoftables"
				"bibliographystyle" "bibliography"
				"include" "input" "includeonly"
				"frontmatter" "mainmatter" "backmatter"
				)
  "*A list of LaTeX commands which separate Emacs paragraphs.

When these regexp strings occur after optional whitespace
and a backslash, they separate Emacs paragraphs, and resist
being filled.  

More specifically, `stx-set-vars' combines these strings
with logical OR (`rx-or') and tacks them on to
`paragraph-separate' with a logical OR.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not make this list nil. 

Do not add strings to this variable without also adding them
to `stx-command-parstarts', or otherwise ensuring they make
it into `paragraph-start'.")
(make-variable-buffer-local 'stx-command-parseps)

(defvar stx-command-parstarts '(
			;;; below do not receive fill text:
				  "begin" "end" 
				  "par"
				  "opening" "closing"
				  "\\[" "caption" "label" "usepackage"
				  "documentclass" "date" "author" "title"
				  "lastchange" "copystyle" "usepackage"
				  "tableofcontents" "maketitle"
				  "listoffigures" "listoftables"
				  "bibliographystyle" "bibliography"
				  "include" "input" "includeonly"
				  "frontmatter" "mainmatter" "backmatter"
			;;; below receive fill text:
				  "indent" "noindent"
				  "bibitem" "item"
				  )

  "*A list of LaTeX commands which start or separate Emacs paragraphs.

When these regexps occur after a backslash at the beginning
of a line, those that are not in `paragraph-separate' start
Emacs paragraphs, and receive text on their line during
fills.

Sectioning commands included in `stx-sectioning-commands'
don't need to be given here because they will get into
`paragraph-separate' via `outline-regexp'.

Do not make this list nil.

See also `stx-command-parseps'.")
(make-variable-buffer-local 'stx-command-parstarts)
;;;;;;        INTERNAL VARIABLES AND CONSTANTS

(defvar stx-comment-start "%"
  "A regexp that matches the start of a comment.
It is \"%\" in swifTeX mode and \"\\\\^\\\\^A\" in docTeX mode.")
(make-variable-buffer-local 'stx-comment-start)

(defvar stx-line-start ""
  "A regexp for the beginning of all significant lines.
It is the empty string in swifTeX mode and \"%*\" in docTeX mode.")
(make-variable-buffer-local 'stx-line-start)

(defconst stx-str-backslash "\\")
;;;;;;        INTERNAL FUNCTION VARIABLES

;;; Without these abbreviations, other code would be very hard to read.

;; Return regexp for the beginning of a line in swifTeX or docTeX modes.  It
;; matches lines hidden in an outline, and depends on the value of
;; `stx-line-start'.
(defsubst stx-boln ()
  (concat "\\(" rx-normal-boln rx-or rx-hidden-boln "\\)"
	  stx-line-start))

;; Return a regexp boln + w-space + b-slash, even if hidden in an outline.
(defsubst stx-bwb ()
  (concat (stx-boln) rx-whitespace rx-backslash))

;; Return a regexp for a blank line, even if hidden in an outline.
(defsubst stx-whiteln ()
  (concat (stx-boln) rx-whitespace rx-eoln))
;;;;;;        MODE INITIALIZATION FUNCTIONS

(defun stx-set-outlinevars (sectioning-commands sectioning-offset doctex)
  "Set `outline-regexp' and define `outline-function' for LaTeX buffers.

Headings of the form `stx-comment-start' followed by at
least one star (\"*\"), and elements of SECTIONING-COMMANDS
are included.  The variable SECTIONING-OFFSET controls the
relative depths of these two kinds of headings.  DOCTEX
non-nil means the buffer is a docTeX buffer.

In default use, the first two arguments will always be the
variables of the same name with either `stx-' or `dtx-'
prefixed, e.g., `dtx-sectioning-offset'.

If an entry for \"endinput\" exists in SECTIONING-COMMANDS
it is treated specially.  In swifTeX mode (i.e., DOCTEX
non-nil), it is treated just like the other sectioning
commands.  In docTeX mode, \"\\endinput\" will be an
outline heading only if it begins in the first column of the
buffer.  This is because 1) there is no reason for it to
occur as an executed macro behind a comment in the
documentation text, and 2) when it occurs with preceding
whitespace in a macrocode environment, doc.sty considers it
to be part of a macro being defined, and does not obey it,
so neither will docTeX."
  (let (;; modifications to `sectioning-commands' below are local
	(sectioning-commands (copy-sequence sectioning-commands))
	;; `endinput-entry' will be nil if \endinput is not there, i.e., not
	;; supposed to be an outline heading; or will be the cons cell if there
	(endinput-entry (assoc "endinput" sectioning-commands)))
    ;; STEP ONE -- set `outline-regexp'
    (make-local-variable 'outline-regexp)
    (setq outline-regexp
	  ;;`outline-regexp' is prefixed by "^" when it is used by the outline
	  ;;commands, and enclosed in regexp parentheses, so we don't need to
	  ;;use "^" or parentheses here.
	  (rx-catenate-or
	   nil
	   (concat stx-line-start stx-comment-start "[*]+")
	   (if (and doctex endinput-entry)
	       (progn
		 ;; Remove the endinput entry from `stx-sectioning-commands' so
		 ;; it doesn't get included below in the wrong way.
		 (delete endinput-entry sectioning-commands)
		 ;; include it in the right way here
		 (concat rx-backslash "endinput")))
	   (concat stx-line-start
		   rx-white-back
		   "\\(" 
		   ;; These are the actual sectioning commands.
		   (rx-catenate-or (mapcar 'car sectioning-commands))
		   "\\)"
		   "\\>")))
    ;; STEP TWO -- define `stx-outline-level' and `byte-compile' it.  We want
    ;; to make `stx-outline-level' as fast as possible, so we avoid evaluating
    ;; conditionals and variable references which are going to be the same
    ;; throughout a buffer's life.  The tradeoff is requiring the byte
    ;; compiling code to be in memory.
    (byte-compile
     (eval 
      (` 
       (defun stx-outline-level ()
	 (, (concat "Return the outline level for heading at point.

For a sectioning command, return the value of
`" (if doctex
       "d"
     "s") "tx-sectioning-offset' plus the number associated with the
particular heading in `" (if doctex
     "d"
     "s") "tx-sectioning-commands'. 

For a normal outline heading `stx-comment-start' followed by
at least one star (\"*\"), return the number of stars.

Like the standard function `outline-level', this function
must be called at the beginning of a header line."))
	 ;; First check if we're on a normal outline heading.
	 (if (looking-at (concat stx-line-start ;    ""  or "%*"
				 stx-comment-start ; "%" or "\\^\\^A"
				 "[*]+"))
	     ;; Return the number of stars.  The formula is different in docTeX
	     ;; mode and swifTeX mode.
	     (, (if doctex
		    ;; length of match minus length of "%^^A"
		    '(- (- (match-end 0) (match-beginning 0)) 4)
		  ;; length of match minus length of "%"
		  '(1- (- (match-end 0) (match-beginning 0)))))
	   ;; Still don't know where we are.
	   ;; Check if we're on a sectioning command.
	   (let ((count 0)
		 (max (, (length sectioning-commands))))
	     (while (and (< count max)
			 (not (looking-at 
			       (concat stx-line-start
				       rx-white-back 
				       (car (elt '(, sectioning-commands) 
						 count))
				       "\\>"))))
	       (setq count (1+ count)))
	     (if (equal count max)
		 ;; Still haven't figured where we are.  
		 ;; If docTeX mode and \endinput is a heading, check for it.
		 (, (if (and doctex endinput-entry)
			(` (if (looking-at (concat rx-backslash
						   "endinput"))
			       (, (cdr endinput-entry))
			     ;; We really don't know where we are.  (In docTeX
			     ;; mode and looking for \endinput.)
			     (error 
			      "`stx-outline-level' falling through! (a)")))
		      ;; We really don't know where we are. (In swifTeX mode,
		      ;; or docTeX mode and not looking for \endinput.)
		      '(error "`stx-outline-level' falling through! (b)")))
	       (+ (, sectioning-offset)
		  (cdr (elt '(, sectioning-commands) count))))))))))))

(defun stx-set-parvars ()
  "Set `paragraph-' variables for swifTeX or docTeX mode.

Set `paragraph-start' to a string containing each of the
following strings separated by logical OR (`rx-or'):

    BOLN + whitespace + EOLN
    BOLN + whitespace + `$$'
    `$$' + whitespace + EOLN    
    `\\\\' + whitespace + EOLN
    BOLN + `outline-regexp'
    BOLN + significant text + single comment
    BOLN + whitespace + double comment
    `stx-local-parstarts'
    BOLN + whitespace + `\\' + `command' + `\\>' 
        where `command' is an element of either
          `stx-command-parstarts'   or
          `stx-local-command-parstarts'.

Set `paragaph-separate' to the same string, with the
substitution of `parseps' for `parstarts' in the obvious
places.

The variable `rx-whitespace' determines what whitespace is\;
the variable `rx-eoln' determines EOLN\; the function
`stx-boln' determines BOLN.  Significant text is one or more
non-newlines followed by an even number of backslashes
\(including zero backslashes).

The regexps are actually defined in a more efficient but
logically equivalent way.

The function `stx-merge-list' provides a convenient way to
add items to list variables."
  (let ((common
	 (rx-catenate-or nil
			 (stx-whiteln)
			 (concat (stx-boln) rx-whitespace rx-dollar rx-dollar)
			 (concat "\\(" 
				 rx-backslash rx-backslash      rx-or
				 rx-dollar rx-dollar
				 "\\)"
				 rx-whitespace
				 rx-eoln)
			 (concat (stx-boln) outline-regexp)
			 (concat (stx-boln)
				 ".*[^\n\\]" 
				 "\\(" rx-backslash rx-backslash "\\)*" 
				 comment-start)
			 (concat (stx-boln)
				 rx-whitespace
				 stx-comment-start 
				 "\\(" stx-comment-start "\\)+"))))
    (setq paragraph-start 
	  (rx-catenate-or nil
			  common
			  (concat (stx-boln)
				  rx-white-back
				  "\\("
				  (rx-catenate-or stx-command-parstarts)
				  (rx-catenate-or stx-local-command-parstarts)
				  "\\)"
				  "\\>"
				  (rx-catenate-or stx-local-parstarts))))
    (setq paragraph-separate
	  (rx-catenate-or nil
			  common
			  (concat (stx-boln)
				  rx-white-back
				  "\\("			
				  (rx-catenate-or stx-command-parseps)
				  (rx-catenate-or stx-local-command-parseps)
				  "\\)"
				  "\\>"
				  (rx-catenate-or stx-local-parseps))))))

(defun stx-set-vars ()
  "Call `stx-set-outlinevars' with appropriate args, then `stx-set-parvars'."
  (interactive)
  (if (equal major-mode 'doctex-mode)
      (stx-set-outlinevars dtx-sectioning-commands dtx-sectioning-offset t)
    (stx-set-outlinevars stx-sectioning-commands stx-sectioning-offset nil))
  (stx-set-parvars))
;;;;;;        DEFINE MODE
(define-derived-mode swiftex-mode latex-mode "swifTeX"
  "Major mode for LaTeX documents derived from the standard LaTeX mode.

The variables `paragraph-separate' and `paragraph-start' are
carefully chosen to make filling and movement convenient.
See `stx-set-parvars'.

One consequence of their values is that single-commented
paragraphs fill normally, and double-or-more commented lines
resist filling (they separate Emacs paragraphs).  Comments
with non-whitespace before a \"%\" also resist filling.

Outline headings are either a single \"%\" in the first
column plus at least one star (\"*\"), or the standard LaTeX
sectioning commands.  See `stx-set-outlinevars'.

The TAB key seems to behave as usual but inserts spaces only
\(that is, no `\\t' characters).  It is confusing when `\\t'
characters occur inside verbatim environments (including
macrocode environments) because the typeset appearance will
not match the appearance of the source file, and
code-indenting conventions will often get mangled.

A number of commands facilitate movement and save typing.

\\[stx-up-block]	moves point past the next \"\\end{name}\".
\\[stx-next-braces]	moves point out of enclosing braces and just into the next set.  
\\[stx-begin-block]	inserts \"\\begin{}\" and prompts for the argument.
\\[stx-insert-block]	inserts a \"\\begin{} \\end{}\" pair and prompts for the argument. 
\\[stx-close-block]	creates an \"\\end{...}\" to match the last unclosed \"\\begin{...}\".
\\[stx-close-block-from-inside]	closes a LaTeX block after the user has typed the argument to \"\\begin\".
\\[stx-emphasize]	inserts \"\\emph{}\" and leaves point between braces.
\\[stx-emphasize-word]	surrounds word at point with \"\\emph{\" and \"}\".

Certain docTeX mode commands are available with the prefix
`swiftex-dtx-prefix-key':  

The following commands maintain the version information in
the LaTeX buffer.  They act on lines that define
\\fileversion, \\filedate, or \\docdate, or the equivalent
components of the \\ProvidePackage or \\ProvideClass
declarations if the first are absent.

\\[dtx-get-fileinfo]	displays the buffer's version in the minibuffer. 
\\[dtx-update-minor-version]	updates the minor version.
\\[dtx-update-major-version]	updates the major version.

The last two also update the file date to the current date.  

\\[dtx-update-documentation-date]	makes the documentation date current.
\\[dtx-insert-change]	inserts a LaTeX \\changes entry.
\\[add-change-log-entry]	adds a ChangeLog entry. 

Some convenient ways to customize behavior in individual
files are provided by the `stx-local-' variables.

\\{swiftex-mode-map}"
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp 
	;; whitespace + optional stx-comment-start + whitespace
	;; Notice that a line with more than one "%" is a paragraph separator
	;; and therefore never part of a paragraph anyway.
	(concat rx-whitespace
		stx-comment-start "?"
		rx-whitespace))
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (outline-minor-mode 1)
  (make-local-variable 'outline-level)
  (setq outline-level (function stx-outline-level))
  (stx-set-vars))
;;;;;;        DEFINE KEYMAP
(defvar swiftex-dtx-mode-map (make-sparse-keymap)
  "Keymap for docTeX mode commands in swifTeX mode.")

(define-key swiftex-mode-map swiftex-dtx-prefix-key swiftex-dtx-mode-map)
(define-key swiftex-dtx-mode-map "g" 'dtx-insert-change)
(define-key swiftex-dtx-mode-map "l" 'add-change-log-entry)
(define-key swiftex-dtx-mode-map "v" 'dtx-get-fileinfo)
(define-key swiftex-dtx-mode-map "u" 'dtx-update-minor-version)
(define-key swiftex-dtx-mode-map "U" 'dtx-update-major-version)
(define-key swiftex-dtx-mode-map "D" 'dtx-update-documentation-date)

(define-key swiftex-mode-map     "\t"       'stx-tab)
(define-key swiftex-mode-map     "\M-\t"    'stx-next-braces)
(define-key swiftex-mode-map     "\C-c\t"   'stx-next-braces)
(define-key swiftex-mode-map     "\C-c\C-]" 'stx-up-block)
(define-key swiftex-mode-map     "\C-]\C-]" 'up-list)
(define-key swiftex-mode-map     "\""       'stx-insert-quote)
(define-key swiftex-mode-map     "\C-c\C-u" 'stx-goto-last-unended-begin)
(define-key swiftex-mode-map     "\C-c\C-e" 'stx-close-block)
(define-key swiftex-mode-map     "\C-c\C-m" 'stx-emphasize-word)
;;;;;;        FUNCTIONS AND VARIABLES TAKEN FROM AUC TEX
;;; The following are taken from AUC TeX. 
;;;
;;; I have mirrored these variables with my own variables.  This way, if a
;;; user's .emacs sets things up for AUC TeX, this mode will do the right
;;; thing.  And a properly-named version exists, for someone who is looking for
;;; things by completion.  I have changed documentation strings conform to
;;; proper convention.

(defvar TeX-open-quote "``"
  "*String inserted by typing \\[stx-insert-quote] to open a quotation.")
(defvar rx-tex-open-quote TeX-open-quote
  "*String inserted by typing \\[stx-insert-quote] to open a quotation.")

(defvar TeX-close-quote "''"
  "*String inserted by typing \\[stx-insert-quote] to close a quotation.")
(defvar rx-tex-close-quote TeX-close-quote
  "*String inserted by typing \\[stx-insert-quote] to close a quotation.")

(defvar TeX-quote-after-quote nil
  "*See documentation for `stx-insert-quote'.")
(defvar stx-quote-after-quote TeX-quote-after-quote
  "*See documentation for `stx-insert-quote'.")

(defun stx-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of `rx-tex-open-quote' (normally ``) or
`rx-tex-close-quote' (normally '') depending on the
context.

`stx-quote-after-quote' non-nil means this insertion works
only after \".  A prefix argument means always insert \"
characters."
  (interactive "*P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (if stx-quote-after-quote
	(insert (cond ((bobp)
		       ?\")
		      ((not (= (preceding-char) ?\"))
		       ?\")
		      ((save-excursion
			 (forward-char -1)
			 (bobp))
		       (delete-backward-char 1)
		       rx-tex-open-quote)
		      ((save-excursion
			 (forward-char -2) ;;; at -1 there is double quote
			 (looking-at "[ \t\n]\\|\\s("))
		       (delete-backward-char 1)
		       rx-tex-open-quote)
		      (t
		       (delete-backward-char 1)
		       rx-tex-close-quote)))
      (insert (cond ((bobp)
		     rx-tex-open-quote)
		    ((= (preceding-char) (string-to-char stx-str-backslash))
		     ?\")
		    ((= (preceding-char) ?\")
		     ?\")
		    ((save-excursion
		       (forward-char (- (length rx-tex-open-quote)))
		       (looking-at (regexp-quote rx-tex-open-quote)))
		     (delete-backward-char (length rx-tex-open-quote))
		     ?\")
		    ((save-excursion
		       (forward-char (- (length rx-tex-close-quote)))
		       (looking-at (regexp-quote rx-tex-close-quote)))
		     (delete-backward-char (length rx-tex-close-quote))
		     ?\")
		    ((save-excursion
		       (forward-char -1)
		       (looking-at "[ \t\n]\\|\\s("))
		     rx-tex-open-quote)
		    (t
		     rx-tex-close-quote))))))
;;;;;;        MISCELLANEOUS FUNCTIONS

(defun stx-merge-list (old new)
  "Modify an list OLD to include all NEW's elements not in OLD.
Compare elements with `equal'.  New elements are added at
the end of OLD.  NEW and OLD can both be lists or both
alists.  Returns OLD.

For alists, add NEW keys to end of OLD if OLD does not
have them.  If OLD does have a matching key, change its
value to NEW's value for that key.  The key is the car of
each element, the value is the cdr."
(let ((p (listp (car old))))
  ;; ensure by first element that args are either both lists or both
  ;; alists
  (if (not (eq p (listp (car new))))
      (error "Args to `stx-merge-list' must be both lists or both alists!")
    (if p
	;; `old' and `new' are alists
	(while new
	  (let* ((new-cons (car new))
		 (old-cons (assoc (car new-cons) old)))
	    (if old-cons
		(setcdr old-cons (cdr new-cons))
	      (nconc old (list new-cons)))
	    (setq new (cdr new))))
      ;; `old' and `new' are regular lists
      (while new
	(let ((x (car new)))
	  (or (member x old)
	      (nconc old (list x)))
	  (setq new (cdr new)))))
    old)))

;;; If you have tabs in a LaTeX buffer, you will be in for some surprises when
;;; you use a verbatim environment.  We define a version of `tab-to-tab-stop'
;;; that only will insert spaces.  I can't imagine that the savings of
;;; file-size is at all significant using tabs instead of spaces, but you can
;;; always debind this from tab if you want usual Emacs tabbing.  I strongly
;;; recommend you keep the new space-tabbing for `doctex-mode' because every
;;; macrocode environment is a verbatim environment.

;; This function's code is based on the standard `tab-to-tab-stop' in
;; the standard "indent.el".
(defun stx-tab ()
  "Insert spaces (no tabs) to next defined tab-stop column.
The variable `tab-stop-list' is a list of columns at which
there are tab stops.  Use \\[edit-tab-stops] to edit them
interactively."
  (interactive "*")
  (if abbrev-mode (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(insert (make-string (- (car tabs) (current-column)) ?\ ))
      (insert ?\ ))))

;; This function can cause screwups if byte-compiled, and if calling it is part
;; of a hook that gets called by `get-buffer-create', for example if it's part
;; of the hook belonging to your `default-major-mode'.
(defun rx-catenate-or (strlist &rest strings)
  "Catenate strings with `rx-or' between them.

Return catenation with `rx-or' of the elements of STRLIST
and optional STRINGS which follow.  Ignore STRINGS that are
nil and elements of STRLIST that are nil.  Return nil if
passed no strings at all.

Empty strings will get catenated, and should probably not be
passed to this function.

STRINGS and the elements of STRLIST are expected to be
strings or nil, but can be other types.  See the source for
full details. 

CAUTION: if you are experiencing strange bugs with this
function, see the warning in \"swiftex.el\"."
  (if (not (listp strlist))
      (error "First arg %S to `rx-catenate-or' must be a list!" strlist))
  (let* ((strlist (mapconcat (function identity) (delq nil strlist) rx-or))
	 (strings (mapconcat (function identity) (delq nil strings) rx-or))
         (islist (not (string-equal "" strlist)))
	 (arestrings (not (string-equal "" strings))))
    (if (and arestrings islist)
	(concat strlist rx-or strings)
      (if arestrings
	  strings
	(if islist
	    strlist)))))

(defun stx-up-block ()
  "Move point past the next \"\\end{name}\".
Let * = point before calling this function, 
** = point afterward, and \\n = a new line:

* [possible text here] 
  [more possible text]
\\end{name}
\\n 
**"
  (interactive)
  (goto-char (re-search-forward (concat rx-backslash "end" rx-brace-pair)))
  (forward-line 1)
  (newline))

(defun stx-next-braces ()
  "Move point out of enclosing braces and just into the next set.  
That is, call `up-list' and then go to position of next
\"{\" plus one."
  (interactive)
  (up-list 1)
  (goto-char (search-forward rx-tex-grop)))

;; Leave point at the beginning of the last \\begin{...} that is unended.  The
;; \\begin{...} must follow `stx-bwb'.
(defun stx-last-unended-begin ()
  (while (and (re-search-backward 
	       (concat (stx-bwb) 
		       "\\(begin" rx-or "end\\)" rx-whitespace
		       rx-tex-grop))
              (looking-at (concat (stx-bwb) "end" rx-whitespace rx-tex-grop)))
    (stx-last-unended-begin))
  (re-search-forward (concat rx-backslash "begin"))
  (goto-char (match-beginning 0)))

;; This is identical to the standard `tex-goto-last-unclosed-latex-block'
;; except for this calls `stx-last-unended-begin' instead of
;; `tex-last-unended-begin'. 
(defun stx-goto-last-unended-begin ()
  "Move point to beginning of the last unclosed \"\\begin{...}\" above.
Leave the mark at the original location.

The \"\\begin{...}\" must follow `stx-bwb'."
  (interactive)
  (let ((spot))
    (save-excursion
      (condition-case nil
	  (stx-last-unended-begin)
	(error (error "Couldn't find unended \\begin!  (stx-goto-last-unended-begin)")))
      (setq spot (point)))
    (push-mark)
    (goto-char spot)))

(defun stx-close-block ()
  "Create an \"\\end{...}\" to match the last unclosed \"\\begin{...}\".
The \"\\begin{...}\" must follow `stx-bwb', which is
duplicated in front of the \"\\end{...}\"."
  (interactive "*")
  (let ((new-line-needed (bolp))
	text indentation)
    (save-excursion
      (condition-case nil
          (stx-last-unended-begin)
        (error (error "Couldn't find unended \\begin!  (stx-close-block)")))
      (setq indentation (buffer-substring 
			 (save-excursion (beginning-of-line) (point))
			 (point)))
      (re-search-forward (concat rx-backslash "begin" 
				 "\\(" rx-whitespace rx-brace-pair
				 "\\)"))
      (setq text (buffer-substring (match-beginning 1) (match-end 1))))
    (insert indentation "\\end" text)
    (if new-line-needed (insert ?\n))))

(defun stx-close-block-from-inside ()
  "Close LaTeX block after typing the argument to \"\\begin\".
Let * be point.
Initial buffer contents:

 \\begin{foo*}

Final buffer contents:

    \\begin{foo}
    *
    \\end{foo}"
  (interactive "*")
  (forward-line 1)
  (stx-close-block)
  (forward-line  -1)
  (open-line 1))

(defun stx-command (command &optional arg)
  "Insert a one-argument LaTeX command \"\\COMMAND{}\".  
ARG non-nil means place ARG between braces following COMMAND
and leave point (*) after both:

\\COMMAND{ARG} *

If ARG is nil, leave point between the braces:
  \\COMMAND{*}"
  (interactive "*")
  (insert "\\" command "{")
  (if arg
      (insert arg "} ")
    (insert "}")
    (backward-char)))

;; This code is based on `current-word' in the standard 19.28 simple.el.
;;
;; It seemed wisest to imitate its "strict" behavior.
;;
;; We explicitly add the original point to undo list, because we have to
;; explicitly move point during the normal execution of the function to keep it
;; on the same text on which it was called.
(defun stx-enclose-word (before after)
  "Insert string BEFORE before word at point and AFTER after.
Keep point over the same text as when the function is called."
  (let* ((oldpoint (point)) 
	 (start oldpoint) 
	 (end oldpoint))
    ;; Since the following movement commands do not alter
    ;; `buffer-undo-list', we add point manually.  We could do this
    ;; any time before the first command that modified it, which is
    ;; the `insert' below.
    (setq buffer-undo-list (cons oldpoint buffer-undo-list))
    (skip-syntax-backward "w")
    (setq start (point))
    (goto-char oldpoint)
    (skip-syntax-forward "w") 
    (setq end (point))
    (if (and (eq start oldpoint)
	     (eq end oldpoint))
	(error "Point is neither in nor adjacent to a word!")
      ;; because the head of `buffer-undo-list' is non-nil, `insert'
      ;; is going to add a boundary to it before adding its element.
      (insert after)
      ;; We remove the boundary now.
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list)))
      (goto-char start)
      (insert before)
      ;; We remove the boundary as before.
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list)))
      (goto-char (+ oldpoint (length before))))))

(defun stx-emphasize-word ()
  "Surround word at point with \"\\emph{\" and \"}\"."
  (interactive "*")
  (stx-enclose-word "\\emph{" "}"))

(defun stx-emphasize ()
  "Call `(stx-command \"emph\")'."
  (interactive "*")
  (stx-command "emph"))

(defun stx-begin-block ()
  "Call `(stx-command \"begin\")'."
  (interactive "*")
  (stx-command "begin"))

(defalias 'stx-insert-block 'tex-latex-block)

(defun stx-block-comment ()
  "Call `(stx-insert-block \"comment\")'.)"
  (interactive "*")
  (stx-insert-block "comment"))

(defun stx-block-quotation ()
  "Call `(stx-insert-block \"quotation\")'."
  (interactive "*")
  (stx-insert-block "quotation"))

;;;;; DOCTEX MODE

;;; The code forms below are unique to docTeX mode, but rely on many of the
;;; forms in the swifTeX mode section above.

;;;;;;        USER VARIABLES

(defvar dtx-sectioning-offset 1
  "*Outline level offset for all LaTeX sectioning commands in a docTeX buffer.
See the documentation for the variable
`stx-sectioning-commands'.")

(defvar dtx-sectioning-commands '(
				  ("documentclass" . -2)
				  ("begin{document}" . -2)
				  ("end{document}" . -2)
				  ("endinput" . -2)
				  ("part" . -2)
				  ("chapter" . -1)
				  ("section".  0)
				  ("subsection" . 1)
				  ("paragraph" . 2)
				  ("subparagraph" . 3)
				  )
  "Equivalent of `stx-sectioning-commands' for docTeX buffers.
See documentation for that variable.")

(defvar doctex-dtx-prefix-key '"\C-c\C-d"
  "*Prefix key for all commands unique to docTeX mode.
A change in this variable takes effect the next time `doctex-mode' is
called.")
;;;;;;        INTERNAL VARIABLES AND CONSTANTS

(defvar dtx-file-version nil
  "The version string of the file in the current buffer.")
(make-variable-buffer-local 'dtx-file-version)

(defvar dtx-prefix-version nil
  "The leading part of the version string up until the major version.

This is the optional \"v\" plus any following elements that
will still leave two elements to the right, which are the
major and minor versions.")
(make-variable-buffer-local 'dtx-prefix-version)

(defvar dtx-major-version nil
  "The major version part of the version string.")
(make-variable-buffer-local 'dtx-major-version)

(defvar dtx-minor-version nil
  "The minor version part of the version string.")
(make-variable-buffer-local 'dtx-minor-version)

(defconst dtx-month-numbers '(("Jan"          "01")
			      ("Feb"          "02")
			      ("Mar"          "03")
			      ("Apr"          "04")
			      ("May"          "05")
			      ("Jun"          "06")
			      ("Jul"          "07")
			      ("Aug"          "08")
			      ("Sep"          "09")
			      ("Oct"          "10")
			      ("Nov"          "11")
			      ("Dec"          "12"))
  "List of lists of pairs of month abbreviations and numbers.")
;;;;;;        INTERNAL FUNCTION VARIABLES

(defun stx-tex-defn (name)
  "Return regexp string matching a line that defines NAME.
The match begins at the beginning of the line, and goes through the
\"{\" that begins the definition."
  (concat (stx-bwb) rx-tex-def-cmd rx-tex-grop "?"
	  rx-backslash name rx-tex-grcl "?" rx-whitespace rx-tex-grop))
;;;;;;        DEFINE MODE

(define-derived-mode doctex-mode swiftex-mode "docTeX"
  "Major mode for LaTeX doc documents derived from swifTeX mode.

DocTeX mode preserves the filling and outlining behavior of
swifTeX mode behind the wall of of \"%\" characters in the
first column of a LaTeX doc.sty buffer.

You won't want to fill inside a \"macrocode\" environment.

The TAB key seems to behave as usual but inserts spaces only
\(that is, no `\\t' characters).  It is confusing when `\\t'
characters occur inside verbatim environments \(including
macrocode environments) because the typeset appearance will
not match the appearance of the source file, and
code-indenting conventions will often get mangled.

Outline headings all begin with a \"%\" in the first column.
LaTeX sectioning commands are outline headings, and so is
\"^^A\" followed by at least one star \(\"*\").  (The ltxdoc
document class defines the \"^^A\" character as a LaTeX
comment.)  See `stx-set-outlinevars'.

The following commands maintain the version information in
the LaTeX buffer.  They act on lines that define
\\fileversion, \\filedate, or \\docdate, or the equivalent
components of the \\ProvidePackage or \\ProvideClass
declarations if the first are absent.

\\[dtx-get-fileinfo]	displays the buffer's version in the minibuffer. 
\\[dtx-update-minor-version]	updates the minor version.
\\[dtx-update-major-version]	updates the major version.

The last two also update the file date to the current date.  

\\[dtx-update-documentation-date]	makes the documentation date current.
\\[dtx-insert-change]	inserts a LaTeX \\changes entry.
\\[add-change-log-entry]	adds a ChangeLog entry. 

A number of commands save typing.  `insert' commands (on
uppercase keys) begin and end the relevant environment and
leave point in between\; `begin' commands \(on lowercase
keys) just begin the environment.  All prompt for the name
of the thing in question.

\\[dtx-interrupt-macrocode]	interrupts a \"macrocode\" environment for more documentation.

Some convenient ways to customize behavior in individual
files are provided by the `stx-local-' variables.

Some function and variable names begin with `stx-' and some
with `dtx-'.  The latter are unique to docTeX mode\; the
former are in common with swifTeX mode.  The difference is
unimportant if you just want to use docTeX mode.

\\{doctex-mode-map}"
  (setq stx-comment-start "\\^\\^A")
  (setq stx-line-start "%*")
  (setq stx-command-parstarts 
	(append stx-command-parstarts '("DescribeMacro" "DescribeEnv")))
  (setq stx-command-parseps
	(append stx-command-parseps '("DescribeMacro" "DescribeEnv")))
  (stx-set-vars))
;;;;;;        DEFINE KEYMAP

(defvar doctex-dtx-mode-map (make-sparse-keymap)
  "Keymap for all new commands in docTeX mode.")
(define-key doctex-mode-map doctex-dtx-prefix-key doctex-dtx-mode-map)
(define-key doctex-dtx-mode-map "g" 'dtx-insert-change)
(define-key doctex-dtx-mode-map "l" 'add-change-log-entry)
(define-key doctex-dtx-mode-map "e" 'dtx-begin-environment)
(define-key doctex-dtx-mode-map "E" 'dtx-insert-environment)
(define-key doctex-dtx-mode-map "f" 'dtx-begin-bibfunction)
(define-key doctex-dtx-mode-map "F" 'dtx-insert-bibfunction)
(define-key doctex-dtx-mode-map "m" 'dtx-begin-macro)
(define-key doctex-dtx-mode-map "M" 'dtx-insert-macro)
(define-key doctex-dtx-mode-map "i" 'dtx-interrupt-macrocode)
(define-key doctex-dtx-mode-map "v" 'dtx-get-fileinfo)
(define-key doctex-dtx-mode-map "c" 'dtx-begin-macrocode)
(define-key doctex-dtx-mode-map "C" 'dtx-insert-macrocode)
(define-key doctex-dtx-mode-map "u" 'dtx-update-minor-version)
(define-key doctex-dtx-mode-map "U" 'dtx-update-major-version)
(define-key doctex-dtx-mode-map "d" 'dtx-update-documentation-date)
;;;;;;        FUNCTIONS TO SAVE TYPING

(defun dtx-begin-macrocode ()
  "Begin a \"macrocode\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert "%    \\begin{macrocode}"))

(defun dtx-insert-macro ()
  "Insert a \"macro\" environment and matching \"macrocode\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
%  \\begin{macro}
%    
%    \\begin{macrocode}
%    \\end{macrocode}
%  \\end{macro}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "Macro: " stx-str-backslash) rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-begin-macro ()
  "Begin a \"macro\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "%  \\begin{macro}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "Macro: " stx-str-backslash) rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-insert-environment ()
  "Insert an \"environment\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
%  \\begin{environment}
%    
%    \\begin{macrocode}
%    \\end{macrocode}
%  \\end{environment}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "Environment: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-begin-environment ()
  "Begin an \"environment\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "%  \\begin{environment}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "Environment: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-insert-bibfunction ()
  "Insert a \"bibfunction\" environment."
  (interactive "*")
  (end-of-line)
  (insert-string "
%  \\begin{bibfunction}
%    
%    \\begin{macrocode}
%    \\end{macrocode}
%  \\end{bibfunction}
%")
  (forward-line -5)
  (end-of-line)
  (insert rx-tex-grop (read-string "FUNCTION: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-begin-bibfunction ()
  "Begin a \"bibfunction\" environment."
  (interactive "*")
  (beginning-of-line)
  (insert-string "%  \\begin{bibfunction}
")
  (forward-line -1)
  (end-of-line)
  (insert rx-tex-grop (read-string "FUNCTION: ") rx-tex-grcl)
  (forward-line 1)
  (end-of-line))

(defun dtx-insert-change ()
  "Insert a LaTeX change log entry with automatic date."
  (interactive "*")
  (if dtx-file-version nil (dtx-get-fileinfo))
  (end-of-line)
  (insert-string "
% \\changes")
  (insert rx-tex-grop dtx-file-version rx-tex-grcl)
  (insert rx-tex-grop (dtx-current-date) rx-tex-grcl)
  (insert rx-tex-grop (read-string "Change: ") rx-tex-grcl))

(defun dtx-insert-macrocode ()
  "Insert a \"macrocode\" environment."
  (interactive "*")
  (forward-line -1)
  (insert-string "%    \\begin{macrocode}

%    \\end{macrocode}
")
  (forward-line -2))

(defun dtx-interrupt-macrocode ()
  "Interrupt a \"macrocode\" environment for more documentation."
  (interactive "*")
  (backward-char 1)
  (end-of-line)
  (insert-string "
%    \\end{macrocode}
% 
%    \\begin{macrocode}")
  (forward-line -1)
  (end-of-line))
;;;;;;        FUNCTIONS TO HANDLE VERSIONS, DATES, ETC.

(defun dtx-get-fileinfo ()
  "Set `dtx-' version-control variables according to lines in the buffer.

That is, set `dtx-file-version', `dtx-prefix-version',
`dtx-major-version', and `dtx-minor-version'.

To set them, look for a line that defines
\"\\fileversion{<version>}\" (see `stx-tex-defn').
<version> is any number of integers separated by dots with
an optional prefix \"v\" and an optional suffix of a
lowercase letter.  If no such line exists, then use the line
with \"\\ProvidesPackage\" or \"\\ProvidesClass\".

The major and minor version strings will be the last two
elements of the <version>, counting the possible suffix as a
separate element if it exists."
  (interactive)
  (save-excursion
    (goto-char (point-min)) 
    (if (or
	 ;; Look for definition of \fileversion.
	 (and (re-search-forward 
	       (stx-tex-defn "fileversion") 
	       nil t)
	      ;; we search twice because we want to remain independent
	      ;; of how many \\(...\\) pairs are in `stx-tex-defn'.
	     (re-search-forward 
	      (concat rx-whitespace rx-dtx-version 
		      rx-whitespace rx-tex-grcl)       
	      nil t))
	 ;; If the first search failed, point is still at point-min.
	 ;; Look for an optional argument to \ProvidesFoo.
	 (and (re-search-forward 
	       (concat (stx-bwb) "Provides" 
		       "\\(Package" rx-or "Class\\)"
		       rx-tex-grop "\\w+" rx-tex-grcl)
	       nil t) 
	      (re-search-forward 
	       (concat rx-whitespace "\\[" 
		       rx-whitespace rx-dtx-date
		       rx-whitespace rx-dtx-version ".*\\]") 
	       nil t)))
	;; We've found what we need. 
	(let* ((ver-match-data (match-data))
	       (the-list (dtx-massage-version
			  (buffer-substring (nth (* 2 2)
						 ver-match-data)
					    (nth (1+ (* 2 3)) 
						 ver-match-data))))
	       (prefix (buffer-substring (nth (* 2 1) ver-match-data)
					 (nth (1+ (* 2 1)) ver-match-data)))
	       (n (length the-list))
	       (k 0))
	  (cond
	   ((= n 0)
	    (setq dtx-major-version "")
	    (setq dtx-minor-version ""))
	   ((= n 1)
	    (setq dtx-major-version (nth 0 the-list))
	    (setq dtx-minor-version ""))
	   (t
	    (setq dtx-major-version (nth (- n 2) the-list))
	    (setq dtx-minor-version (nth (- n 1) the-list))))
	  (setq dtx-prefix-version prefix)
	  ;; now add onto the prefix whatever is right
	  (while (< k (- n 2))
	    (setq dtx-prefix-version
		  (concat dtx-prefix-version
			  (if (or (string-equal dtx-prefix-version "")
				  (string-equal dtx-prefix-version "v"))
			      "" ".")
			  (nth k the-list)))
	    (setq k (1+ k)))
	  (dtx-set-version))
      ;; We have not found what we need. 
      (error "Can't find file info!  (dtx-get-fileinfo)"))))

(defun dtx-update-documentation-date ()
  "Update the \"\\docdate\" to the current date."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (if (and (re-search-forward (stx-tex-defn "docdate") nil t)
	     (re-search-forward rx-dtx-date nil t))
	  (replace-match (dtx-current-date))
      (error 
       "Can't find documenation date!  (dtx-update-documentation-date)"))))

;; Update the \fileversion and \filedate.
;;
;; Find and update the version and date in the line that defines them (see
;; `stx-tex-defn'); if no such line exists, use \"\\ProvidesPackage\" or
;; \"\\ProvidesClass\" line."
(defun dtx-update-fileinfo ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (stx-tex-defn "fileversion") nil t)
        (progn
	  ;; The argument can contain matching braces.
	  (backward-char)
	  (delete-region (point) (forward-list))
          (insert rx-tex-grop dtx-file-version rx-tex-grcl))
      ;; If first search failed, point is still point-min.
      (if (and 
	   (re-search-forward 
	    (concat (stx-bwb) "Provides\\(Package" rx-or "Class\\)"
		    rx-whitespace rx-brace-pair
		    rx-whitespace "\\["
		    rx-whitespace rx-dtx-date rx-whitespace)
	    nil t) 
           (re-search-forward rx-dtx-version nil t))
	  (replace-match dtx-file-version)
	(error "Can't find file version!  (dtx-get-fileinfo)")))
    ;; The date is only changed if a change above succeeds. 
    (goto-char (point-min))
    (if (re-search-forward (stx-tex-defn "filedate") nil t)
	(progn
	  (backward-char)
	  (delete-region (point) (forward-list))
	  (insert rx-tex-grop (dtx-current-date) rx-tex-grcl))
      (if (and (re-search-forward 
		(concat (stx-bwb) 
			"Provides\\(Package" rx-or 
			           "Class\\)"
			rx-whitespace rx-brace-pair
			rx-whitespace "\\[")
		nil t) 
	       (re-search-forward rx-dtx-date nil t))
	  (replace-match (dtx-current-date))
	(error "Can't find file date!  (dtx-get-fileinfo)")))))

(defun dtx-update-minor-version ()
  "Increment the minor version."
  (interactive "*")
  (if dtx-file-version nil (dtx-get-fileinfo))
  (if (string-equal dtx-minor-version "")
      (setq dtx-minor-version (read-string "Minor version: "))
    (if (string-match "[a-z]" dtx-minor-version)
	(setq dtx-minor-version
	      (char-to-string (1+ (string-to-char dtx-minor-version))))
      (setq dtx-minor-version
	    (int-to-string (1+ (string-to-int dtx-minor-version))))))
  (dtx-set-version)
  (dtx-update-fileinfo))

(defun dtx-update-major-version ()
  "Increment the major version."
  (interactive "*")
  (if dtx-file-version nil (dtx-get-fileinfo))
  (setq dtx-minor-version "")
  (setq dtx-major-version
        (int-to-string (1+ (string-to-int dtx-major-version))))
  (dtx-set-version)
  (dtx-update-fileinfo))

;; Assemble `dtx-file-version' from its constituent parts.
(defun dtx-set-version ()
  (setq dtx-file-version
        (concat dtx-prefix-version
		(if (or (string-equal dtx-prefix-version "")
			(string-equal dtx-prefix-version "v"))
		    "" ".")
                dtx-major-version
                (if (not (string-equal dtx-minor-version ""))
                    (progn
                      (concat
                       (if (or (string-equal dtx-major-version "")
                               (string-match "[a-z]" dtx-minor-version))
                           "" ".")
                       dtx-minor-version)))))
  (message "The version of this file is now <%s>" dtx-file-version))

;; Massage a version string into a list of strings. 
;;
;; Given a string S matching a series of unsigned integers separated by dots,
;; return a list of those integers as strings.
;;
;; If a lowercase letter ends S, it will be tacked on to the returned list.
(defun dtx-massage-version (s)
  (let ((k 0) 
	(n (length s)) 
	(element nil) 
	(kth-char)
	(number-list nil))
    (while (< k n)
      (setq kth-char (char-to-string (aref s k)))
      (cond
       ((string-match "[0-9]" kth-char)
        (setq element (concat element kth-char)))
       ((string-match "\\." kth-char)
        (setq number-list (append number-list (list element))
	      element nil))
       ((string-match "[a-zA-Z]" kth-char)
	(if (or (not (equal k (1- n)))
		(string-match "[A-Z]" kth-char))
	    (error "Bad letter in version string \"%s\"!  The last character may be a lowercase letter.  (dtx-massage-version)" s)
	  (setq number-list (append number-list (list element))
		element kth-char)))
       (t
        (error "Can't parse version string \"%s\"!  (dtx-massage-version)" s)))
      (setq k (1+ k)))
    (setq number-list (append number-list (list element)))
    (if (equal number-list (list nil))
        (setq number-list nil))
    number-list))

(defun dtx-current-date ()
  "Return the current date as a string in the form \"1991/01/23\"."
  (let ((cts (current-time-string)))
    (concat
     ;; get "yyyy"
     (substring cts 20 24)
     "/"
     (nth 1 (assoc (substring cts 4 7)
                   dtx-month-numbers))	;expand "mon" to number
     "/"
     (if (equal (substring cts 8 9) " ")
         "0"
       (substring cts 8 9))
     ;; get "dd"
     (substring cts 9 10))))

;;;; EMACS FILE VARIABLES 
;;;
;;; Local Variables:
;;; outline-regexp: ";;;[;]+"
;;; End:
;;;
;;; swiftex.el ends here
