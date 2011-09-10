;;; x-sb-mouse.el : extended mouse support for emacs
;;; Copyright (C) 1992 Sullivan Beck
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; This is a new and hopefully better mouse package for emacs running
;;; under X.  Some of its features are:
;;;  1) It combines the features of x-mouse-drag and alt-mouse.
;;;  2) It allows you to use control, shift, and meta modifiers with the
;;;     mouse (in any combination).
;;;  3) It distinguishes between click and drag events.  A drag is where
;;;     the mouse button is moved with a button held down).
;;;  4) It recognizes 5 different parts of the window:
;;;       window : the window itself
;;;       mode   : the mode line of a window
;;;       border : the vertical border separating two windows split side
;;;                by side
;;;       inter  : the intersection of a mode line and the vertical border
;;;       mini   : the minibuffer
;;;     This allows a great deal of extra functionality not available in
;;;     the other two mouse packages such as both horizontal AND vertical
;;;     resizing of the windows.  You can even resize the minibuffer (as
;;;     long as you give it at least 1 line).
;;;  5) 120 click events and 24 drag events can be bound simply to emacs
;;;     functions by simply setting the appropriate variable (see
;;;     installation instructions below).
;;;  6) It is fully x-mouse compatible.  All packages that run on top
;;;     of x-mouse should also run on top of this one.

;;; Although I have rewritten almost everything, segments of this package
;;; have been borrowed from both of the packages mentioned above.  Thanks
;;; to the authors (Eirik Fuller for alt-mouse and Peter Moore for
;;; x-mouse-drag) for their work.

;;; The 120 valid click events are defined by where they occured (one of
;;; the 5 different recognized parts of the window) and what keyboard
;;; modifiers are used (none OR control, shift, meta, or any combination
;;; of the three modifiers).  Drag events are defined by where they
;;; start, where they end, and what keyboard modifiers are used.  Obviously
;;; there are a lot of them, but only a small number of them actually do
;;; anything.  Dragging mode lines around can resize windows vertically.
;;; Dragging borders around resize windows horizontally.  Dragging an
;;; intersection resizes horizontally and vertically simoultaneously.
;;; Keyboard modifiers are ignored for these drags.  The other type of
;;; drag that is recognized are those that begin and end in the same
;;; window.  All 24 of these (with keyboard modifiers) can be set by the
;;; user.

;;; To examine the default bindings, either look at the 'setq' lines at
;;; the bottom of the file or run the command x-mouse-help (bound to
;;; shift-button-1 in the mode line.

;;; HISTORY:
;;;
;;;   1.6        Fixed documentation.  Thanks to Bill Horne.
;;;
;;;              Fixed .texi file.  Thanks to George Ferguson, Robert
;;;              Chassell, Tonny Madsen, and many others for smaller
;;;              fixes.
;;;
;;;              Fixed x-help.  Thanks to Gnanasekaran Swaminathan.
;;;
;;;              Added Makefile written by George Ferguson.  Thanks!
;;;
;;;              Created 2 new files: xsbm-funs.el and xsbm-keys.el.
;;;
;;;              Added mode specific bindings.  Thanks to Dag Wanvik
;;;              and Shekhar Bapat.  Added x-mouse-global-set-key,
;;;              x-mouse-local-set-key, x-mouse-define-key,
;;;              x-mouse-global-unset-key, x-mouse-local-unset-key,
;;;              and x-mouse-undefine-key functions.  Added
;;;              x-mouse-bind-hook.
;;;
;;;              Added x-mouse-describe-event function and
;;;              x-mouse-describe-only variable.  Thanks to Lars Huttar.
;;;              x-mouse-describe-event is bound to C-h RET.
;;;
;;;              Modified x-mouse a lot to make it easier to extend.
;;;              Thanks to Lars Huttar.
;;;
;;;              Added the x-mouse-get-file command (written by Ik Su Yoo)
;;;              and the x-mouse-file-ignore-regexp variable.
;;;
;;;              Added x-mouse-copy-thing-to-point and
;;;              x-mouse-cut-thing-to-point.  Thanks to Nitin More.

;;; The history of previous versions has been moved to the info file.
;;;
;;; I have tried to acknowledge all people who have contributed something,
;;; but occasionally I forget.  I apologize if I forgot you.  Mail me and
;;; I'll correct it.

;;; COMMON QUESTIONS:
;;;
;;; Highlighting: I do not support highlighting (yet) in x-sb-mouse.
;;; The reason for this is that there is no standard way to do highlighting
;;; inside of emacs without first applying an unofficial patch to the
;;; source code and recompiling emacs.  Because it is unofficial, I do
;;; not support it.  Emacs 19 will have highlighting.  So does Epoch, but
;;; I don't use Epoch.  If anyone happens to fix this to highlight under
;;; Epoch, I'd appreciate it if you'd send me the diffs so that I can
;;; add it to the standard distribution.
;;;
;;; Popup menus:  Popup menus only work if emacs was compiled with
;;; HAVE_X_MENUS defined in the config.h file.  This does not work on
;;; all computers, but is part of the standard source code so I use it.

;;; INSTALLING AND CUSTOMIZING
;;;
;;; Installing x-sb-mouse is very easy with the Makefile included.  It
;;; requires that you have the makeinfo program.  If you do not have it,
;;; you can get it by anonymous ftp from prep.ai.mit.edu:/pub/gnu/.
;;; It's in the file texinfo-2.15.tar.Z (or whatever the most recent
;;; version happens to be).  This is only necessary if you want to use
;;; the info file.  The actual mouse package can be compiled and installed
;;; without the makeinfo program.
;;;
;;; Edit the Makefile and decide where you want to install the program
;;; and set ELISPDIR to that directory.  If you are going to create an
;;; info file, set the INFODIR variable to the directory where it should
;;; be installed.
;;;
;;; Then just type one of the following:
;;;
;;;    make install       # to make and install everything
;;;    make install.src   # to make and install just the program
;;;    make install.info  # to make and install just the info file
;;;    make               # to make (but not install) everything
;;;    make src           # to make (but not install) just the program
;;;    make info          # to make (but not install) just the info file
;;;
;;; To try this out once (but NOT have it load every time you start up
;;; emacs), just load it with the 'M-x load-file' command to load the
;;; file x-sb-mouse.elc.
;;;
;;; To have it automatically start up each time you use emacs, just add
;;; the following to your .emacs file:
;;;
;;;   (setq term-setup-hook '(lambda () (load-library "x-sb-mouse")))
;;;
;;; This assumes that ELISPDIR is in the default load path (i.e. where
;;; all of the other .el and .elc files are kept).  If they are stored
;;; somewhere else, you will need to add two lines to your .emacs file:
;;;
;;;   (setq x-mouse-dir "DIR")
;;;   (setq term-setup-hook
;;;         '(lambda () (load "DIR/x-sb-mouse")))
;;;
;;; where you must replace DIR in the above lines with whatever you
;;; defined ELISPDIR to be in the Makefile.  For example, I keep a copy
;;; of the package in my "~/lisp/xsbm/" directory that I am working
;;; on and that's the one I want loaded, so in my .emacs file, I have:
;;;
;;;   (setq x-mouse-dir "~/lisp/xsbm")
;;;   (setq term-setup-hook
;;;         '(lambda () (load "~/lisp/xsbm/x-sb-mouse")))
;;;
;;; If you use any packages that require x-mouse (such as hyperbole or
;;; gdbsrc), they should be loaded AFTER x-sb-mouse.  They will probably
;;; redefine some of the default key bindings.  The function
;;; x-mouse-init-mouse-map will redefine these keys to whatever functions
;;; I have defined or that you have defined in your .emacs file.  The
;;; problem is that this will remove the bindings used by the other
;;; packages.  Hyperbole does NOT have this problem.  It allows you to
;;; toggle between it's bindings and your personal mouse bindings.  I
;;; am planning on adding a simple toggle function to x-sb-mouse which
;;; will hopefully solve this problem.
;;; 
;;; To change the bindings, simply add a line to your .emacs file putting
;;; in a new command.  For example, if you want clicking the first button
;;; inside a window with a meta modifier to insert the letter "a" when in
;;; text-mode and to run the command recenter when in lisp-mode rather
;;; than the default bindings and you want to change the default binding
;;; for clicking button 2 in the mode line to select that window, add the
;;; following to your .emacs file:
;;;
;;;   (setq x-mouse-bind-hook '(lambda ()
;;;     (x-mouse-define-key "x-mouse-m1-window-click" t
;;;      'text-mode '(lambda () (insert "a"))
;;;      'lisp-mode 'recenter))
;;;     (x-mouse-define-key "x-mouse-2-mode-click" t
;;;      'default   'x-mouse-select))
;;;
;;; As many mode specific bindings can be added as desired.  The first
;;; argument is a string of the form given in the example.  They all start
;;; with "x-mouse-" followed by any or all of the three keyboard modifiers
;;; "c" (control), "m" (meta), and "s" (shift) which must be in alphabetical
;;; order in the string.  This is followed by the button number (1, 2, or 3),
;;; where it occurs ("window", "mode", "border", "inter", or "mini"), and
;;; the type of event ("click" or "drag" - NOTE: only "window" drags are
;;; currently defined).
;;;
;;; More information on customization is included in the info file.  For
;;; interactively setting a binding, use x-mouse-global-set-key and
;;; x-mouse-local-set-key.

;;; Please send any comments or suggestions to me at beck@qtp.ufl.edu.


;;;*************************************************************************
;;; User definable variables.  Set these in your .emacs file.

;;(defvar x-mouse-dir "~/lisp")
(defvar x-mouse-dir nil 
  "*The directory where x-sb-mouse lives.
If it is set to nil, then it looks in the load path.")

(defvar x-mouse-blink-cursor nil
  "*If this is t, the cursor will blink temporarily to where a mouse event is.
Pressing a mouse button will cause the cursor to appear there for a second.
Releasing the mouse will cause the cursor to appear there for a second and
then appear at the position of the press (if it was different) for a second.
These 'blinks' do not interfere with executing commands during the delay.")

(defvar x-mouse-auto-set-mark t
  "*If this is non-nil, the mark will be set every time the point is set.
With this set, clicking in two spots defines a region (which you can
kill, copy, etc.)")

(defvar x-mouse-duplicate-cut t
  "*If non-nil, saving to the x-cut-buffer saves to the kill ring too.
In other words, every time a region is saved/appended to the x-cut-buffer,
it is also saved/appended to the kill ring.")

(defvar x-mouse-init-map t
  "*If non-nil, initializes the mouse map to include all my default bindings.
If it is t, all of the functionality still exists but the buttons are not
defined and you'll have to do it yourself.")

(defvar x-mouse-help-to-menu t
  "*If nil, sends help to a buffer rather than popup menus.
Nice if you want to be able to refer back to the help while you type.
Rather then set this manually, there is now an option in the menu to
do this as a one time only command.")

(defvar x-mouse-describe-only nil
  "*If non-nil, mouse events are not executed, just described.
They are evaluated to see what function would be called; that function
name is printed.  Used by describe-mouse-event.")

(defvar x-mouse-file-ignore-regexp ".o$\\|~$\\|#$\\|/$\\|@\\|*\\|.elc"
  "*File endings to ignore in the x-mouse-get-file command.")


;;;*************************************************************************
;;; First we'll define the mouse map and some other necessary things.  This
;;; section should probably not be altered.
(provide 'x-mouse)
(provide 'x-sb-mouse)

(define-key help-map "\C-m" 'x-mouse-describe-event)

(defconst x-button-right (char-to-string 0))
(defconst x-button-right-up (char-to-string 4))
(defconst x-button-middle (char-to-string 1))
(defconst x-button-middle-up (char-to-string 5))
(defconst x-button-left (char-to-string 2))
(defconst x-button-left-up (char-to-string 6))

(defconst x-button-s-right (char-to-string 16))
(defconst x-button-s-right-up (char-to-string 20))
(defconst x-button-s-middle (char-to-string 17))
(defconst x-button-s-middle-up (char-to-string 21))
(defconst x-button-s-left (char-to-string 18))
(defconst x-button-s-left-up (char-to-string 22))

(defconst x-button-m-right (char-to-string 32))
(defconst x-button-m-right-up (char-to-string 36))
(defconst x-button-m-middle (char-to-string 33))
(defconst x-button-m-middle-up (char-to-string 37))
(defconst x-button-m-left (char-to-string 34))
(defconst x-button-m-left-up (char-to-string 38))

(defconst x-button-c-right (char-to-string 64))
(defconst x-button-c-right-up (char-to-string 68))
(defconst x-button-c-middle (char-to-string 65))
(defconst x-button-c-middle-up (char-to-string 69))
(defconst x-button-c-left (char-to-string 66))
(defconst x-button-c-left-up (char-to-string 70))

(defconst x-button-m-s-right (char-to-string 48))
(defconst x-button-m-s-right-up (char-to-string 52))
(defconst x-button-m-s-middle (char-to-string 49))
(defconst x-button-m-s-middle-up (char-to-string 53))
(defconst x-button-m-s-left (char-to-string 50))
(defconst x-button-m-s-left-up (char-to-string 54))

(defconst x-button-c-s-right (char-to-string 80))
(defconst x-button-c-s-right-up (char-to-string 84))
(defconst x-button-c-s-middle (char-to-string 81))
(defconst x-button-c-s-middle-up (char-to-string 85))
(defconst x-button-c-s-left (char-to-string 82))
(defconst x-button-c-s-left-up (char-to-string 86))

(defconst x-button-c-m-right (char-to-string 96))
(defconst x-button-c-m-right-up (char-to-string 100))
(defconst x-button-c-m-middle (char-to-string 97))
(defconst x-button-c-m-middle-up (char-to-string 101))
(defconst x-button-c-m-left (char-to-string 98))
(defconst x-button-c-m-left-up (char-to-string 102))

(defconst x-button-c-m-s-right (char-to-string 112))
(defconst x-button-c-m-s-right-up (char-to-string 116))
(defconst x-button-c-m-s-middle (char-to-string 113))
(defconst x-button-c-m-s-middle-up (char-to-string 117))
(defconst x-button-c-m-s-left (char-to-string 114))
(defconst x-button-c-m-s-left-up (char-to-string 118))

(defvar x-process-mouse-hook nil
  "Hook to run after each mouse event is processed.  Should take two
arguments; the first being a list (XPOS YPOS) corresponding to character
offset from top left of screen and the second being a specifier for the
buttons/keys.  This will normally be set on a per-buffer basis.")
(defun x-flush-mouse-queue ()
  "Process all queued mouse events."
  ;; A mouse event causes a special character sequence to be given
  ;; as keyboard input.  That runs this function, which process all
  ;; queued mouse events and returns.
  (interactive)
  (while (> (x-mouse-events) 0)
    (x-proc-mouse-event))
  (and (boundp 'x-process-mouse-hook)
       (symbol-value 'x-process-mouse-hook)
       (funcall x-process-mouse-hook x-mouse-pos x-mouse-item)))
(define-key global-map "\C-c\C-m" 'x-flush-mouse-queue)
(define-key global-map "\C-x\C-@" 'x-flush-mouse-queue)


;;;*************************************************************************
;;; The following three functions (and the progn statement after) are what
;;; actually decide what to do when a press or release event occurs.  This
;;; section may be modified eventually as I add minibuffer functions and
;;; dragging from one window to another.

;;; All a mouse press does is set some variables.
(defun x-mouse-press (arg)
  "Sets up all the parameters describing the environment of a button press.
The following variables are set:
  x-mouse-point-0     The current point
  x-mouse-win-0       The current selected window
  x-mouse-mode-0      The current major mode
  x-mouse-pos-d       The position (relative to the screen) of the mouse
  x-mouse-win-d       The window the mouse is in (nil if minibuffer)
  x-mouse-mode-d      The major mode in the window where the mouse was pressed
  x-mouse-coords-d    The position (relative to the window) of the mouse
  x-mouse-point-d     The (point) of the mouse position in the window
  x-mouse-type-d      The \"type\" of position where the press occured.
      'window refers to a position actually in the window
      'mode   refers to a position on the mode line of a window
      'border refers to the column separating two horizontally split windows
      'inter  refers to the intersection of a mode line and a border
      'mini   refers to the minibuffer
  x-mouse-press        This function was called for the press."
  (save-window-excursion
    (setq x-mouse-point-0 (point)
          x-mouse-win-0 (selected-window)
	  x-mouse-mode-0 major-mode
          x-mouse-press t
          x-mouse-pos-d arg
          x-mouse-win-d (x-mouse-window arg)
	  x-mouse-mode-d (save-window-excursion
			  (select-window x-mouse-win-d)
			  major-mode)
          x-mouse-coords-d (x-mouse-coords x-mouse-pos-d x-mouse-win-d)
          x-mouse-point-d (x-mouse-point arg x-mouse-win-d)
          x-mouse-type-d (cond
                          ((x-mouse-mini-p arg)
                           'mini)
                          ((x-mouse-inter-p arg x-mouse-win-d)
                           'inter)
                          ((x-mouse-mode-p arg x-mouse-win-d)
                           'mode)
                          ((x-mouse-border-p arg x-mouse-win-d)
                           'border)
                          (t 'window)))
    (if x-mouse-blink-cursor
	(progn
	  (select-window x-mouse-win-d)
	  (save-excursion
	    (goto-char x-mouse-point-d)
	    (sit-for 1))))))

(defvar x-mouse-press nil)
;;; This runction is called on a mouse release event and it sets up the
;;; remaining variables used to perform the desired action.
(defun x-mouse-release (arg)
  "Sets up all the parameters describing the environment of a button press.
The following variables are set:
  x-mouse-pos-u       The position (relative to the screen) of the mouse
  x-mouse-win-u       The window the mouse is in (nil if minibuffer)
  x-mouse-mode-u      The major mode in the window where the mouse was released
  x-mouse-coords-u    The position (relative to the window) of the mouse
  x-mouse-point-u     The (point) of the mouse position in the window
  x-mouse-type-u      The \"type\" of position where the press occured.
      'window refers to a position actually in the window
      'mode   refers to a position on the mode line of a window
      'border refers to the column separating two horizontally split windows
      'inter  refers to the intersection of a mode line and a border
      'mini   refers to the minibuffer
  x-mouse-click       't if the down and up events occurred at the same place."
  (save-window-excursion
    (setq x-mouse-press ()
          x-mouse-pos-u arg
          x-mouse-click (equal x-mouse-pos-d x-mouse-pos-u)
          x-mouse-win-u (x-mouse-window arg)
	  x-mouse-mode-u (save-window-excursion
			  (select-window x-mouse-win-u)
			  major-mode)
          x-mouse-coords-u (x-mouse-coords x-mouse-pos-u x-mouse-win-u)
          x-mouse-point-u (x-mouse-point arg x-mouse-win-u)
          x-mouse-type-u (cond
                          ((x-mouse-mini-p arg)
                           'mini)
                          ((x-mouse-inter-p arg x-mouse-win-u)
                           'inter)
                          ((x-mouse-mode-p arg x-mouse-win-u)
                           'mode)
                          ((x-mouse-border-p arg x-mouse-win-u)
                           'border)
                          (t 'window)))
    (if x-mouse-blink-cursor
	(progn
	  (select-window x-mouse-win-u)
	  (save-excursion
	    (goto-char x-mouse-point-u)
	    (sit-for 1))
	  (if (not x-mouse-click)
	      (progn
		(select-window x-mouse-win-d)
		(save-excursion
		  (goto-char x-mouse-point-d)
		  (sit-for 1))))))))

;;; This function is called when a button is released.  The three arguments
;;; are the position of the mouse, the button number (1, 2, or 3), and
;;; a list of modifiers containing 'c, 's, and 'm in any combination.
(defun x-mouse (arg button modifier)
  (if x-mouse-press
      (progn
        (x-mouse-release arg)
        ;; base is a string "x-mouse-"
        ;; mods is one of the following: 
        ;;    "", "c", "m", "s", "cm", "cs", "ms", "cms"
        ;; but  is "1", "2", or "3"
        ;; op   contains base mods, but, and "-"
        ;; type contains "window", "mode", "border", "inter", or "mini"
        ;; function contains the whole string
        ;; x-mouse-windows contains info used in the resize commands
        (let* ((base "x-mouse-")
               (mods (if (memq 'c modifier) "c" ""))
               (mods (if (memq 'm modifier) (concat mods "m") mods))
               (mods (if (memq 's modifier) (concat mods "s") mods))
               (but  (int-to-string button))
               (op   (concat base mods but "-"))
               (type (symbol-name x-mouse-type-d))
               (function "x-mouse-ignore"))
          ;; list ALL the combinations under which the function is evaluated
          (cond
           ;; any click event
           (x-mouse-click
	    (setq function (concat op type "-click")))
           ;; drag in a single window
           ((and (eq x-mouse-type-d 'window) (eq x-mouse-type-u 'window)
                 (eq x-mouse-win-d x-mouse-win-u))
	    (setq function (concat op "window-drag")))
           ;; drag from mode to adjacent window
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'window))
            (setq function "x-mouse-resize"))
           ;; drag in a single mode line
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'mode)
                 (eq x-mouse-win-d x-mouse-win-u))
            (setq function (concat op "mode-click")))
           ;; drag from mode to adjacent minibuffer
           ((and (eq x-mouse-type-d 'mode) (eq x-mouse-type-u 'mini))
            (setq function "x-mouse-resize"))
           ;; drag from border to adjacent window
           ((and (eq x-mouse-type-d 'border) (eq x-mouse-type-u 'window))
            (setq function "x-mouse-resize"))
           ;; drag in a single border
           ((and (eq x-mouse-type-d 'border) (eq x-mouse-type-u 'border)
                 (eq x-mouse-win-d x-mouse-win-u))
            (setq function (concat op "border-click")))
           ;; drag from inter to adjacent window
           ((and (eq x-mouse-type-d 'inter) (eq x-mouse-type-u 'window))
            (setq function "x-mouse-resize"))
           ;; drag from inter to adjacent mini
           ((and (eq x-mouse-type-d 'inter) (eq x-mouse-type-u 'mini))
            (setq function "x-mouse-resize"))
           ;; drag in mini
           ((and (eq x-mouse-type-d 'mini) (eq x-mouse-type-u 'mini))
            (setq function (concat op "mini-click")))
           )
          (setq x-mouse-press nil)
	  (if x-mouse-describe-only
              (message "%s runs the function %s" function
		       (x-mouse-get-function-name function x-mouse-mode-u))
	    (funcall (x-mouse-get-function function x-mouse-mode-u)))))))

(defun x-mouse-init-mouse-map ()
  "Sets up the mouse map (or resets it if it has gotten altered).
This is useful for when some package alters the key bindings and
you want to recover the default ones."
  (interactive)
  (define-key mouse-map x-button-left 'x-mouse-press)
  (define-key mouse-map x-button-left-up
    '(lambda (arg) (x-mouse arg 1 '())))
  (define-key mouse-map x-button-middle 'x-mouse-press)
  (define-key mouse-map x-button-middle-up
    '(lambda (arg) (x-mouse arg 2 '())))
  (define-key mouse-map x-button-right 'x-mouse-press)
  (define-key mouse-map x-button-right-up
    '(lambda (arg) (x-mouse arg 3 '())))

  (define-key mouse-map x-button-c-left 'x-mouse-press)
  (define-key mouse-map x-button-c-left-up
    '(lambda (arg) (x-mouse arg 1 '(c))))
  (define-key mouse-map x-button-c-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c))))
  (define-key mouse-map x-button-c-right 'x-mouse-press)
  (define-key mouse-map x-button-c-right-up
    '(lambda (arg) (x-mouse arg 3 '(c))))

  (define-key mouse-map x-button-m-left 'x-mouse-press)
  (define-key mouse-map x-button-m-left-up
    '(lambda (arg) (x-mouse arg 1 '(m))))
  (define-key mouse-map x-button-m-middle 'x-mouse-press)
  (define-key mouse-map x-button-m-middle-up
    '(lambda (arg) (x-mouse arg 2 '(m))))
  (define-key mouse-map x-button-m-right 'x-mouse-press)
  (define-key mouse-map x-button-m-right-up
    '(lambda (arg) (x-mouse arg 3 '(m))))

  (define-key mouse-map x-button-s-left 'x-mouse-press)
  (define-key mouse-map x-button-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(s))))
  (define-key mouse-map x-button-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(s))))
  (define-key mouse-map x-button-s-right 'x-mouse-press)
  (define-key mouse-map x-button-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(s))))

  (define-key mouse-map x-button-c-m-left 'x-mouse-press)
  (define-key mouse-map x-button-c-m-left-up
    '(lambda (arg) (x-mouse arg 1 '(c m))))
  (define-key mouse-map x-button-c-m-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-m-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c m))))
  (define-key mouse-map x-button-c-m-right 'x-mouse-press)
  (define-key mouse-map x-button-c-m-right-up
    '(lambda (arg) (x-mouse arg 3 '(c m))))

  (define-key mouse-map x-button-c-s-left 'x-mouse-press)
  (define-key mouse-map x-button-c-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(c s))))
  (define-key mouse-map x-button-c-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c s))))
  (define-key mouse-map x-button-c-s-right 'x-mouse-press)
  (define-key mouse-map x-button-c-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(c s))))

  (define-key mouse-map x-button-m-s-left 'x-mouse-press)
  (define-key mouse-map x-button-m-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(m s))))
  (define-key mouse-map x-button-m-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-m-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(m s))))
  (define-key mouse-map x-button-m-s-right 'x-mouse-press)
  (define-key mouse-map x-button-m-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(m s))))

  (define-key mouse-map x-button-c-m-s-left 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-left-up
    '(lambda (arg) (x-mouse arg 1 '(c m s))))
  (define-key mouse-map x-button-c-m-s-middle 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-middle-up
    '(lambda (arg) (x-mouse arg 2 '(c m s))))
  (define-key mouse-map x-button-c-m-s-right 'x-mouse-press)
  (define-key mouse-map x-button-c-m-s-right-up
    '(lambda (arg) (x-mouse arg 3 '(c m s)))))

;;;*************************************************************************

(if x-mouse-dir
    (progn
      (load (concat (file-name-as-directory x-mouse-dir) "xsbm-funs"))
      (load (concat (file-name-as-directory x-mouse-dir) "thing"))
      (load (concat (file-name-as-directory x-mouse-dir) "xsbm-userfuns"))
      (load (concat (file-name-as-directory x-mouse-dir) "xsbm-keys")))
  (load "xsbm-funs")
  (load "thing")
  (load "xsbm-userfuns")
  (load "xsbm-keys"))

(if x-mouse-init-map
    (x-mouse-init-mouse-map))
