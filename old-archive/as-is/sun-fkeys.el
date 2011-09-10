;From bug-gnu-emacs-request@prep.ai.mit.edu Fri Apr  7 12:23:14 1989
;Received: by life.ai.mit.edu; Fri, 7 Apr 89 12:10:34 EDT
;Return-Path: <@relay.cs.net:root@scrsu1.sdr.slb.com>
;Received: from prep.ai.mit.edu by life.ai.mit.edu; Fri, 7 Apr 89 12:07:30 EDT
;Received: from RELAY.CS.NET by prep.ai.mit.edu; Fri, 7 Apr 89 10:40:23 EST
;Message-Id: <8904071540.AA00270@prep.ai.mit.edu>
;Received: from relay2.cs.net by RELAY.CS.NET id aa25995; 7 Apr 89 11:56 EDT
;Received: from sdr.slb.com by RELAY.CS.NET id ah29684; 7 Apr 89 11:44 EDT
;Date: Fri, 7 Apr 89 11:27 EST
;From: Operator <root@scrsu1.sdr.slb.com>
;Subject: Function keys in Sunview Emacs
;To: INFO-GNU-EMACS@prep.ai.mit.edu
;X-Vms-To: aaet.csc.ti.com::wilensky, prep.ai.mit.edu::info-gnu-emacs
;Status: R
;
;
;
;Harold -
;
;Simple:
;------
;
;You can't without doing
;
;	M-x set-var sun-esc-bracket t
;	M-x load-lib term/sun
;
;Then you can just press `F1' when global-set-key asks for a key.
;
;Complex
;-------
;
;I spent a long time trying set up sun function keys sensibly: here's
;the results (not very sensible !).  I actually load sun-fns.el (below)
;and sun-mouse.el into emacs at build time, so that they do not slow
;down startup (this is done by modifying EMACS/src/ymakefile, amongst
;other things.)
;
;The full spec for a Sun function key in a shelltool run with no .ttyswrc 
;is:
;
;	ESC [ <keystring> z  (where <keystring> is something like "208")
;
;In an emacstool it's:
;
;	Ctrl-x * <code>      (where <code> is something like "ar" for R1)
;
;You cannot specify the first for `M-x global-set-key' unless ESC-[ is
;rebound to a new keymap.  However, worry not. Emacs loads a file called
;EMACS/lisp/term/sun.el when its starts up (by default, if your TERM
;variable is "sun"). This sets up the keymap, but does not enable it.
;You need to set the variable sun-esc-bracket to "t" before the keymap
;is used. You would normally do this in your .emacs file:
;
;		(setq sun-esc-bracket t)
;
;I have enclosed the sun.el I use - put this into the lisp/term
;directory and all will work by default (since this file sets
;sun-esc-bracket to t).  This sets up keys for both shelltools and
;emacstools. If you want LaTeX keyboard maps, shout.
;
;You will need to make sure you replace the standard sun-fns.el by the
;one below, since all non-key related functions that were in sun.el
;have been moved into this instead.
;
;If you would like more help, shout: but do it quickly - I'm moving to
;your side of the pond at the end of the month. Its a real mess how
;Emacs handles function keys, and I suspect it will not get any better
;in the foreseeable future.
;
;Paul
;                             Paul Davis at Schlumberger Cambridge Research
;                                <davis%scrsu1%sdr.slb.com@relay.cs.net>
;
;                              "to shatter tradition makes us feel free ..."
;
;====> lisp/ours/term/sun.el  <====

;; keybinding for standard default sunterm keys
;; Copyright (C) 1987 Free Software Foundation, Inc.

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

;;  Jeff Peck, Sun Microsystems Inc  <peck@sun.com>

;; SCR LOCAL CHANGES

;; this file only does key bindings - extra functions and emacstool
;; setup funcs are now in the local version of sun-fns.el.

;; -- keymaps expanded to cover all available keys for both sun-raw-map
;;         and suntool-map (*BUT* no shift sequences used here for emacstool,
;;         on the basis that plain, control and meta is ENOUGH!!).

;; -- Vee-eM-eSsy-like access to Emacs command history on L3 and L4.

;; -- suntool-map contains bindings to some functions to be found in
;;         site-init.el

;; -- sun-esc-bracket set to t to always setup arrow keys
;;    [use (setq sun-esc-bracket nil) in .emacs to unset this].

;; Paul Davis, Schlumberger Cambridge Research <davis@blue.sdr.slb.com>

;;;
;;; handle sun's extra function keys
;;; this version for those who run with standard .ttyswrc and no emacstool
;;;
;;; sunview picks up L1, L5, L6, L7, L8, L9, L10 and F1 on the way up
;;; so we ignore them on the way down
;;;

(defvar sun-raw-map (make-sparse-keymap) 
  "*Keymap for ESC-[ encoded keyboard")

(define-key sun-raw-map "208z" 'beginning-of-line)  ;; R1
(define-key sun-raw-map "209z" 'goto-line)
(define-key sun-raw-map "210z" 'end-of-line)
(define-key sun-raw-map "211z" 'backward-paragraph)
(define-key sun-raw-map "212z" 'exchange-point-and-mark)
(define-key sun-raw-map "213z" 'forward-paragraph)
(define-key sun-raw-map "214z" 'backward-word)
(define-key sun-raw-map "215z" 'previous-line)    ;; UP
(define-key sun-raw-map "216z" 'forward-word)     ;; R9
(define-key sun-raw-map "217z" 'backward-char)    ;; LEFT
(define-key sun-raw-map "218z" 'recenter)         ;; R11
(define-key sun-raw-map "219z" 'forward-char)     ;; RIGHT
(define-key sun-raw-map "220z" 'scroll-down)      ;; R13
(define-key sun-raw-map "221z" 'next-line)        ;; DOWN
(define-key sun-raw-map "222z" 'scroll-up)        ;; R15
(define-key sun-raw-map "193z" 'execute-extended-command)  	;; L2
(define-key sun-raw-map "194z" 'repeat-complex-command)    	;; L3
(define-key sun-raw-map "195z" 'shell)			   	;; L4
(define-key sun-raw-map "197z" 'lpr-buffer)			;; L6
(define-key sun-raw-map "199z" 'auto-fill-mode)			;; L8
(define-key sun-raw-map "200z" 'yank)				;; L9
(define-key sun-raw-map "201z" 'undo)				;; L10
(define-key sun-raw-map "225z" 'enlarge-window)			;; F2
(define-key sun-raw-map "226z" 'split-window-horizontally)	;; F3
(define-key sun-raw-map "227z" 'other-window)			;; F4
(define-key sun-raw-map "228z" 'delete-other-windows)		;; F5
(define-key sun-raw-map "229z" 'beginning-of-buffer)		;; F6
(define-key sun-raw-map "230z" 'end-of-buffer)			;; F7
(define-key sun-raw-map "231z" 'find-file)			;; F8
(define-key sun-raw-map "232z" 'save-buffer)			;; F9

;; access to command history on two keys - here
;; L3 (used to call repeat-complex-command) and L4.
;; Notice how these bindings are only in effect after
;; repeat-complex-command has been used, and only until
;; we exit from the minibuffer (either with C-g or RET/LFD)

(define-key repeat-complex-command-map "\e[194z" 'previous-complex-command)
(define-key repeat-complex-command-map "\e[195z" 'next-complex-command)

(defvar sun-esc-bracket t
  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")

(if sun-esc-bracket
    (progn
      (define-key esc-map "[" sun-raw-map)		; Install sun-raw-map
      (define-key esc-map "[A" 'previous-line )		; R8
      (define-key esc-map "[B" 'next-line)		; R14
      (define-key esc-map "[C" 'forward-char)		; R12
      (define-key esc-map "[D" 'backward-char)		; R10
      (define-key esc-map "[[" 'backward-paragraph)	; the original esc-[
      ))

;;; Since .emacs gets loaded before this file, a hook is supplied
;;; for you to put your own bindings in.

(defvar sun-raw-map-hooks nil
  "List of forms to evaluate after setting sun-raw-map.")

(let ((hooks sun-raw-map-hooks))
  (while hooks
    (eval (car hooks))
    (setq hooks (cdr hooks))
    ))


;;; This section adds defintions for the emacstool users
;;; emacstool event filter converts function keys to C-x*{c}{lrt}
;;;
;;; for example the Open key (L7) would be encoded as "\C-x*gl"
;;; the control, meta, and shift keys modify the character {lrt}
;;; note that (unshifted) C-l is ",",  C-r is "2", and C-t is "4"
;;;
;;; {c} is [a-j] for LEFT, [a-i] for TOP, [a-o] for RIGHT.
;;; A higher level insists on encoding {h,j,l,n}{r} (the arrow keys)
;;; as ANSI escape sequences.  Use the shell command 
;;; % setkeys noarrows
;;; if you want these to come through for emacstool.
;;;
;;; If you are not using EmacsTool, 
;;; you can also use this by creating a .ttyswrc file to do the conversion.
;;; but it won't include the CONTROL, META, or SHIFT keys!
;;;
;;; Important to define SHIFTed sequence before matching unshifted sequence.
;;; (talk about bletcherous old uppercase terminal conventions!*$#@&%*&#$%)
;;;  this is worse than C-S/C-Q flow control anyday!
;;;  Do *YOU* run in capslock mode?
;;;

;;; Note:  L1 (al), L5 (el), L7 (gl) and F1 (at) are trapped by
;;; EmacsTool, so they never make it here.

(defvar meta-flag t)

(defvar suntool-map (make-sparse-keymap)
  "*Keymap for Emacstool bindings.")

(define-key suntool-map "ar" 'beginning-of-line)	;; same order
(define-key suntool-map "br" 'goto-line)		;; as above,
(define-key suntool-map "cr" 'end-of-line)		;; with
(define-key suntool-map "dr" 'backward-paragraph)	;; each block of
(define-key suntool-map "er" 'exchange-point-and-mark)	;; keys followed
(define-key suntool-map "fr" 'forward-paragraph)	;; by Ctrl and
(define-key suntool-map "gr" 'backward-word)		;; meta variants
(define-key suntool-map "ir" 'forward-word)
(define-key suntool-map "kr" 'recenter)
(define-key suntool-map "mr" 'scroll-down)
(define-key suntool-map "or" 'scroll-up)
(define-key suntool-map "a2" 'search-backward)
(define-key suntool-map "b2" 'just-one-space)
(define-key suntool-map "c2" 'search-forward)
(define-key suntool-map "d2" 'isearch-backward-regexp)
(define-key suntool-map "e2" 'occur)
(define-key suntool-map "f2" 'isearch-forward-regexp)
(define-key suntool-map "g2" 'replace-string)
(define-key suntool-map "i2" 'replace-regexp)
(define-key suntool-map "k2" 'query-replace)
(define-key suntool-map "m2" 'delete-matching-lines)
(define-key suntool-map "o2" 'delete-non-matching-lines)
(define-key suntool-map "a\M-r" 'backward-kill-line)
(define-key suntool-map "b\M-r" 'delete-blank-lines)
(define-key suntool-map "c\M-r" 'kill-line)
(define-key suntool-map "d\M-r" 'kill-to-top)
(define-key suntool-map "e\M-r" 'zap-to-char)
(define-key suntool-map "f\M-r" 'kill-to-bottom)
(define-key suntool-map "g\M-r" 'backward-kill-word)
(define-key suntool-map "i\M-r" 'kill-word)
(define-key suntool-map "k\M-r" 'append-next-kill)
(define-key suntool-map "m\M-r" 'kill-region)
(define-key suntool-map "o\M-r" 'copy-region-as-kill)
(define-key suntool-map "bl" 'execute-extended-command)
(define-key suntool-map "cl" 'repeat-complex-command)
(define-key suntool-map "dl" 'shell)
(define-key suntool-map "fl" 'lpr-buffer)
(define-key suntool-map "hl" 'auto-fill-mode)
(define-key suntool-map "il" 'yank)
(define-key suntool-map "jl" 'undo)
(define-key suntool-map "b," 'compile)
(define-key suntool-map "c," 'repeat-matching-complex-command)
(define-key suntool-map "d," 'rmail)
(define-key suntool-map "f," 'print-buffer)
(define-key suntool-map "h," 'overwrite-mode)
(define-key suntool-map "i," 'show-output-from-shell)
(define-key suntool-map "j," 'shell-command)
(define-key suntool-map "b\M-l" 'dired)
(define-key suntool-map "c\M-l" 'list-command-history)
(define-key suntool-map "d\M-l" 'grep)
(define-key suntool-map "f\M-l" 'lpr-region)
(define-key suntool-map "h\M-l" 'toggle-case-sensitive)
(define-key suntool-map "i\M-l" 'manual-entry)
(define-key suntool-map "j\M-l" 'shell-command-on-region)
(define-key suntool-map "bt" 'enlarge-window)
(define-key suntool-map "ct" 'split-window-horizontally)
(define-key suntool-map "dt" 'other-window)
(define-key suntool-map "et" 'delete-other-windows)
(define-key suntool-map "ft" 'beginning-of-buffer)
(define-key suntool-map "gt" 'end-of-buffer)
(define-key suntool-map "ht" 'find-file)
(define-key suntool-map "it" 'save-buffer)
(define-key suntool-map "b4" 'insert-file)
(define-key suntool-map "c4" 'switch-to-buffer-other-window)
(define-key suntool-map "d4" 'switch-to-buffer)
(define-key suntool-map "e4" 'buffer-menu)
(define-key suntool-map "f4" 'list-buffers)
(define-key suntool-map "g4" 'rename-buffer)
(define-key suntool-map "h4" 'find-file-other-window)
(define-key suntool-map "i4" 'save-some-buffers)
(define-key suntool-map "b\M-t" 'insert-buffer)
(define-key suntool-map "c\M-t" 'split-window-vertically)
(define-key suntool-map "d\M-t" 'revert-buffer)
(define-key suntool-map "e\M-t" 'kill-some-buffers)
(define-key suntool-map "f\M-t" 'recover-file)
(define-key suntool-map "g\M-t" 'set-visited-file-name)
(define-key suntool-map "h\M-t" 'find-alternate-file)
(define-key suntool-map "i\M-t" 'write-file)

;; command history as for sun-raw-map, still on L3 and L4 (see above)

(define-key repeat-complex-command-map "\C-x*cl" 'previous-complex-command)
(define-key repeat-complex-command-map "\C-x*dl" 'next-complex-command)

(define-key ctl-x-map "*" suntool-map)

;;; Since .emacs gets loaded before this file, a hook is supplied
;;; for you to put your own bindings in.

(defvar suntool-map-hooks nil
  "List of forms to evaluate after setting suntool-map.")

(let ((hooks suntool-map-hooks))
  (while hooks
    (eval (car hooks))
    (setq hooks (cdr hooks))
    ))

===> lisp/ours/sun-fns.el <====

;; Subroutines of Mouse handling for Sun windows
;; Copyright (C) 1987 Free Software Foundation, Inc.

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

;;; Submitted Mar. 1987, Jeff Peck
;;;		 	 Sun Microsystems Inc. <peck@sun.com>
;;; Conceived Nov. 1986, Stan Jefferson,
;;;                      Computer Science Lab, SRI International.
;;; GoodIdeas Feb. 1987, Steve Greenbaum
;;; & UpClicks           Reasoning Systems, Inc.

(provide 'sun-fns)
(require 'sun-mouse)

;;;
;;; Functions for manipulating via the mouse and mouse-map definitions
;;; for accessing them.  Also definitons of mouse menus.
;;; This file you should freely modify to reflect you personal tastes.
;;;
;;; First half of file defines functions to implement mouse commands,
;;; Don't delete any of those, just add what ever else you need.
;;; Second half of file defines mouse bindings, do whatever you want there.

;;;
;;;         Mouse Functions.
;;;
;;; These functions follow the sun-mouse-handler convention of being called
;;; with three arguements: (window x-pos y-pos)
;;; This makes it easy for a mouse executed command to know where the mouse is.
;;; Use the macro "eval-in-window" to execute a function 
;;; in a temporarily selected window.
;;;
;;; If you have a function that must be called with other arguments
;;; bind the mouse button to an s-exp that contains the necessary parameters.
;;; See "minibuffer" bindings for examples.
;;;
(defconst cursor-pause-milliseconds 300
  "*Number of milliseconds to display alternate cursor (usually the mark)")

(defun indicate-region (&optional pause)
  "Bounce cursor to mark for cursor-pause-milliseconds and back again"
  (or pause (setq pause cursor-pause-milliseconds))
  (let ((point (point)))
    (goto-char (mark))
    (sit-for-millisecs pause)
    ;(update-display)
    ;(sleep-for-millisecs pause)
    (goto-char point)))


;;;
;;; Text buffer operations
;;;
(defun mouse-move-point (window x y)
  "Move point to mouse cursor."
  (select-window window)
  (move-to-loc x y)
  (if (memq last-command	; support the mouse-copy/delete/yank
	    '(mouse-copy mouse-delete mouse-yank-move))
      (setq this-command 'mouse-yank-move))
  )

(defun mouse-set-mark (window x y)
  "Set mark at mouse cursor."
  (eval-in-window window	;; use this to get the unwind protect
    (let ((point (point)))
      (move-to-loc x y)
      (set-mark (point))
      (goto-char point)
      (indicate-region)))
  )

(defun mouse-set-mark-and-select (window x y)
  "Set mark at mouse cursor, and select that window."
  (select-window window)
  (mouse-set-mark window x y)
  )

(defun mouse-set-mark-and-stuff (w x y)
  "Set mark at mouse cursor, and put region in stuff buffer."
  (mouse-set-mark-and-select w x y)
  (sun-select-region (region-beginning) (region-end)))

;;;
;;; Simple mouse dragging stuff: marking with button up
;;;

(defvar *mouse-drag-window* nil)
(defvar *mouse-drag-x* -1)
(defvar *mouse-drag-y* -1)

(defun mouse-drag-move-point (window x y)
  "Move point to mouse cursor, and allow dragging."
  (mouse-move-point window x y)
  (setq *mouse-drag-window* window
	*mouse-drag-x* x
	*mouse-drag-y* y))

(defun mouse-drag-set-mark-stuff (window x y)
  "The up click handler that goes with mouse-drag-move-point.
If mouse is in same WINDOW but at different X or Y than when
mouse-drag-move-point was last executed, set the mark at mouse
and put the region in the stuff buffer."
  (if (and (eq *mouse-drag-window* window)
	   (not (and (equal *mouse-drag-x* x)
		     (equal *mouse-drag-y* y))))
      (mouse-set-mark-and-stuff window x y)
    (setq this-command last-command))	; this was just an upclick no-op.
  )

(defun mouse-select-or-drag-move-point (window x y)
  "Select window if not selected, otherwise do mouse-drag-move-point."
  (if (eq (selected-window) window)
      (mouse-drag-move-point window x y)
    (mouse-select-window window x y)))

;;;
;;; esoteria:
;;;
(defun mouse-exch-pt-and-mark (window x y)
  "Exchange point and mark."
  (select-window window)
  (exchange-point-and-mark)
  )

(defun mouse-call-kbd-macro (window x y)
  "Invokes last keyboard macro at mouse cursor."
  (mouse-move-point window x y)
  (call-last-kbd-macro)
  )

(defun mouse-mark-thing (window x y)
  "Set point and mark to text object using syntax table.
The resulting region is put in the sun-window stuff buffer.
Left or right Paren syntax marks an s-expression.  
Clicking at the end of a line marks the line including a trailing newline.  
If it doesn't recognize one of these it marks the character at point."
  (mouse-move-point window x y)
  (if (eobp) (open-line 1))
  (let* ((char (char-after (point)))
         (syntax (char-syntax char)))
    (cond
     ((eq syntax ?w)			; word.
      (forward-word 1)
      (set-mark (point))
      (forward-word -1))
     ;; try to include a single following whitespace (is this a good idea?)
     ;; No, not a good idea since inconsistent.
     ;;(if (eq (char-syntax (char-after (mark))) ?\ )
     ;;    (set-mark (1+ (mark))))
     ((eq syntax ?\( )			; open paren.
      (mark-sexp 1))
     ((eq syntax ?\) )			; close paren.
      (forward-char 1)
      (mark-sexp -1)
      (exchange-point-and-mark))
     ((eolp)				; mark line if at end.
      (set-mark (1+ (point)))
      (beginning-of-line 1))
     (t					; mark character
      (set-mark (1+ (point)))))
    (indicate-region))			; display region boundary.
  (sun-select-region (region-beginning) (region-end))
  )

(defun mouse-kill-thing (window x y)
  "Kill thing at mouse, and put point there."
  (mouse-mark-thing window x y)
  (kill-region-and-unmark (region-beginning) (region-end))
  )

(defun mouse-kill-thing-there (window x y)
  "Kill thing at mouse, leave point where it was.
See mouse-mark-thing for a description of the objects recognized."
  (eval-in-window window 
    (save-excursion
      (mouse-mark-thing window x y)
      (kill-region (region-beginning) (region-end))))
  )

(defun mouse-save-thing (window x y &optional quiet)
  "Put thing at mouse in kill ring.
See mouse-mark-thing for a description of the objects recognized."
  (mouse-mark-thing window x y)
  (copy-region-as-kill (region-beginning) (region-end))
  (if (not quiet) (message "Thing saved"))
  )

(defun mouse-save-thing-there (window x y &optional quiet)
  "Put thing at mouse in kill ring, leave point as is.
See mouse-mark-thing for a description of the objects recognized."
  (eval-in-window window
    (save-excursion
      (mouse-save-thing window x y quiet))))

;;;
;;; Mouse yanking...
;;;
(defun mouse-copy-thing (window x y)
  "Put thing at mouse in kill ring, yank to point.
See mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)	 ;Avoids appending to previous kills.
  (mouse-save-thing-there window x y t)
  (yank)
  (setq this-command 'yank))

(defun mouse-move-thing (window x y)
  "Kill thing at mouse, yank it to point.
See mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)	 ;Avoids appending to previous kills.
  (mouse-kill-thing-there window x y)
  (yank)
  (setq this-command 'yank))

(defun mouse-yank-at-point (&optional window x y)
  "Yank from kill-ring at point; then cycle thru kill ring."
  (if (eq last-command 'yank)
      (let ((before (< (point) (mark))))
	(delete-region (point) (mark))
	(rotate-yank-pointer 1)
	(insert (car kill-ring-yank-pointer))
	(if before (exchange-point-and-mark)))
    (yank))
  (setq this-command 'yank))

(defun mouse-yank-at-mouse (window x y)
  "Yank from kill-ring at mouse; then cycle thru kill ring."
  (mouse-move-point window x y)
  (mouse-yank-at-point window x y))

(defun mouse-save/delete/yank (&optional window x y)
  "Context sensitive save/delete/yank.
Consecutive clicks perform as follows:
    * first click saves region to kill ring,
    * second click kills region,
    * third click yanks from kill ring,
    * subsequent clicks cycle thru kill ring.
If mouse-move-point is performed after the first or second click,
the next click will do a yank, etc.  Except for a possible mouse-move-point,
this command is insensitive to mouse location."
  (cond
   ((memq last-command '(mouse-delete yank mouse-yank-move))	; third+ click
    (mouse-yank-at-point))
   ((eq last-command 'mouse-copy)	; second click
    (kill-region (region-beginning) (region-end))
    (setq this-command 'mouse-delete))
   (t					; first click
    (copy-region-as-kill (region-beginning) (region-end))
    (message "Region saved")
    (setq this-command 'mouse-copy))
   ))


(defun mouse-split-horizontally (window x y)
  "Splits the window horizontally at mouse cursor."
  (eval-in-window window (split-window-horizontally (1+ x))))

(defun mouse-split-vertically (window x y)
  "Split the window vertically at the mouse cursor."
  (eval-in-window window (split-window-vertically (1+ y))))

(defun mouse-select-window (window x y)
  "Selects the window, restoring point."
  (select-window window))

(defun mouse-delete-other-windows (window x y)
  "Deletes all windows except the one mouse is in."
  (delete-other-windows window))

(defun mouse-delete-window (window x y)
  "Deletes the window mouse is in."
  (delete-window window))

(defun mouse-undo (window x y)
  "Invokes undo in the window mouse is in."
  (eval-in-window window (undo)))

;;;
;;; Scroll operations
;;;

;;; The move-to-window-line is used below because otherwise
;;; scrolling a non-selected process window with the mouse, after
;;; the process has written text past the bottom of the window,
;;; gives an "End of buffer" error, and then scrolls.  The
;;; move-to-window-line seems to force recomputing where things are.
(defun mouse-scroll-up (window x y)
  "Scrolls the window upward."
  (eval-in-window window (move-to-window-line 1) (scroll-up nil)))

(defun mouse-scroll-down (window x y)
  "Scrolls the window downward."
  (eval-in-window window (scroll-down nil)))

(defun mouse-scroll-proportional (window x y)
  "Scrolls the window proportionally corresponding to window
relative X divided by window width."
  (eval-in-window window 
    (if (>= x (1- (window-width)))
	;; When x is maximun (equal to or 1 less than window width),
	;; goto end of buffer.  We check for this special case
	;; becuase the calculated goto-char often goes short of the
	;; end due to roundoff error, and we often really want to go
	;; to the end.
	(goto-char (point-max))
      (progn
	(goto-char (+ (point-min)	; For narrowed regions.
		      (* x (/ (- (point-max) (point-min))
			      (1- (window-width))))))
	(beginning-of-line))
      )
    (what-cursor-position)		; Report position.
    ))

(defun mouse-line-to-top (window x y)
  "Scrolls the line at the mouse cursor up to the top."
  (eval-in-window window (scroll-up y)))

(defun mouse-top-to-line (window x y)
  "Scrolls the top line down to the mouse cursor."
  (eval-in-window window (scroll-down y)))

(defun mouse-line-to-bottom (window x y)
  "Scrolls the line at the mouse cursor to the bottom."
  (eval-in-window window (scroll-up (+ y (- 2 (window-height))))))

(defun mouse-bottom-to-line (window x y)
  "Scrolls the bottom line up to the mouse cursor."
  (eval-in-window window (scroll-down (+ y (- 2 (window-height))))))

(defun mouse-line-to-middle (window x y)
  "Scrolls the line at the mouse cursor to the middle."
  (eval-in-window window (scroll-up (- y -1 (/ (window-height) 2)))))

(defun mouse-middle-to-line (window x y)
  "Scrolls the line at the middle to the mouse cursor."
  (eval-in-window window (scroll-up (- (/ (window-height) 2) y 1))))


;;;
;;; main emacs menu.
;;;
(defmenu expand-menu
  ("Vertically" mouse-expand-vertically *menu-window*)
  ("Horizontally" mouse-expand-horizontally *menu-window*))

(defmenu delete-window-menu
  ("This One" delete-window *menu-window*)
  ("All Others" delete-other-windows *menu-window*))

(defmenu mouse-help-menu
  ("Text Region"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'text)
  ("Scrollbar"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'scrollbar)
  ("Modeline"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'modeline)
  ("Minibuffer"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'minibuffer)
  )
  
(defmenu emacs-quit-menu
  ("Suspend" suspend-emacstool)
  ("Quit" save-buffers-kill-emacs))

(defmenu emacs-menu
  ("Emacs Menu")
  ("Stuff Selection" sun-yank-selection)
  ("Expand" . expand-menu)
  ("Delete Window" . delete-window-menu)
  ("Previous Buffer" mouse-select-previous-buffer *menu-window*)
  ("Save Buffers" save-some-buffers)
  ("List Directory" list-directory nil)
  ("Dired" dired nil)
  ("Mouse Help" . mouse-help-menu)
  ("Quit" . emacs-quit-menu))

(defun emacs-menu-eval (window x y)
  "Pop-up menu of editor commands."
  (sun-menu-evaluate window (1+ x) (1- y) 'emacs-menu))

(defun mouse-expand-horizontally (window)
  (eval-in-window window
    (enlarge-window 4 t)
    (update-display)		; Try to redisplay, since can get confused.
    ))

(defun mouse-expand-vertically (window)
  (eval-in-window window (enlarge-window 4)))

(defun mouse-select-previous-buffer (window)
  "Switch buffer in mouse window to most recently selected buffer."
  (eval-in-window window (switch-to-buffer (other-buffer))))

;;;
;;; minibuffer menu
;;;
(defmenu minibuffer-menu 
  ("Minibuffer" message "Just some miscellanous minibuffer commands")
  ("Stuff" sun-yank-selection)
  ("Do-It" exit-minibuffer)
  ("Abort" abort-recursive-edit)
  ("Suspend" suspend-emacs))

(defun minibuffer-menu-eval (window x y)
  "Pop-up menu of commands."
  (sun-menu-evaluate window x (1- y) 'minibuffer-menu))

(defun mini-move-point (window x y)
  ;; -6 is good for most common cases
  (mouse-move-point window (- x 6) 0))

(defun mini-set-mark-and-stuff (window x y)
  ;; -6 is good for most common cases
  (mouse-set-mark-and-stuff window (- x 6) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Buffer-mode Mouse commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun Buffer-at-mouse (w x y)
  "Calls Buffer-menu-buffer from mouse click."
  (save-window-excursion 
    (mouse-move-point w x y)
    (beginning-of-line)
    (Buffer-menu-buffer t)))

(defun mouse-buffer-bury (w x y)
  "Bury the indicated buffer."
  (bury-buffer (Buffer-at-mouse w x y))
  )

(defun mouse-buffer-select (w x y)
  "Put the indicated buffer in selected window."
  (switch-to-buffer (Buffer-at-mouse w x y))
  (list-buffers)
  )

(defun mouse-buffer-delete (w x y)
  "mark indicated buffer for delete"
  (save-window-excursion
    (mouse-move-point w x y)
    (Buffer-menu-delete)
    ))

(defun mouse-buffer-execute (w x y)
  "execute buffer-menu selections"
  (save-window-excursion
    (mouse-move-point w x y)
    (Buffer-menu-execute)
    ))
  
(defun enable-mouse-in-buffer-list ()
  "Call this to enable mouse selections in *Buffer List*
    LEFT puts the indicated buffer in the selected window.
    MIDDLE buries the indicated buffer.
    RIGHT marks the indicated buffer for deletion.
    MIDDLE-RIGHT deletes the marked buffers.
To unmark a buffer marked for deletion, select it with LEFT."
  (save-window-excursion
    (list-buffers)			; Initialize *Buffer List*
    (set-buffer "*Buffer List*")
    (local-set-mouse '(text middle) 'mouse-buffer-bury)
    (local-set-mouse '(text left) 'mouse-buffer-select)	    
    (local-set-mouse '(text right) 'mouse-buffer-delete)
    (local-set-mouse '(text middle right) 'mouse-buffer-execute)
    )
  )


;;; Functions from $EMACS/lisp/term/sun.el
;;; copied over so that the latter only does key bindings

(defun ignore-key ()
  "interactive version of ignore"
  (interactive)
  (ignore))

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(defun kill-region-and-unmark (beg end)
  "Like kill-region, but pops the mark [which equals point, anyway.]"
  (interactive "r")
  (kill-region beg end)
  (setq this-command 'kill-region-and-unmark)
  (set-mark-command t))

(defun prev-complex-command ()
  "Select Previous-complex-command"
  (interactive)
  (if (zerop (minibuffer-depth))
      (repeat-complex-command 1)
    (previous-complex-command 1)))

(defun rerun-prev-command ()
  "Repeat Previous-complex-command."
  (interactive)
  (eval (nth 0 command-history)))

(defvar grep-arg nil "Default arg for RE-search")
(defun grep-arg ()
  (if (memq last-command '(research-forward research-backward)) grep-arg
    (let* ((command (car command-history))
	   (command-name (symbol-name (car command)))
	   (search-arg (car (cdr command)))
	   (search-command 
	    (and command-name (string-match "search" command-name)))
	   )
      (if (and search-command (stringp search-arg)) (setq grep-arg search-arg)
	(setq search-command this-command 
	      grep-arg (read-string "REsearch: " grep-arg)
	      this-command search-command)
	grep-arg))))

(defun research-forward ()
  "Repeat RE search forward."
  (interactive)
  (re-search-forward (grep-arg)))

(defun research-backward ()
  "Repeat RE search backward."
  (interactive)
  (re-search-backward (grep-arg)))

;;;
;;; If running under emacstool, arrange to call suspend-emacstool
;;; instead of suspend-emacs.
;;;


(defun emacstool-init ()
  "Set up Emacstool window, if you know you are in an emacstool."
  ;; Make sure sun-mouse and sun-fns are loaded, and try for
  ;; emacstool-menus, as well as local modeline goodies.
  (require 'sun-fns)
  (load "emacstool-menus" t t)
  (load "modeline" t t)
  (define-key ctl-x-map "\C-@" 'sun-mouse-handler)

  (if (< (sun-window-init) 0)
      (message "Not a Sun Window")
    (progn
      (substitute-key-definition 'suspend-emacs 'suspend-emacstool global-map)
      (substitute-key-definition 'suspend-emacs 'suspend-emacstool esc-map)
      (substitute-key-definition 'suspend-emacs 'suspend-emacstool ctl-x-map))
      (send-string-to-terminal
       (concat "\033]lEmacstool - GNU Emacs " emacs-version "\033\\"))
    ))

(defun sun-mouse-once ()
  "Converts to emacstool and sun-mouse-handler on first mouse hit."
  (interactive)
  (emacstool-init))
  
(define-key ctl-x-map "\C-@" 'sun-mouse-once)



;;;*******************************************************************
;;;
;;;           Global Mouse Bindings.
;;;
;;; There is some sense to this mouse binding madness:
;;; LEFT and RIGHT scrolls are inverses.
;;; SHIFT makes an opposite meaning in the scroll bar.
;;; SHIFT is an alternative to DOUBLE (but double chords do not exist).
;;; META makes the scrollbar functions work in the text region.
;;; MIDDLE operates the mark
;;; LEFT operates at point

;;; META commands are generally non-destructive,
;;; SHIFT is a little more dangerous.
;;; CONTROL is for the really complicated ones.

;;; CONTROL-META-SHIFT-RIGHT gives help on that region.

;;;
;;; Text Region mousemap
;;;
;;; The basics: Point, Mark, Menu, Sun-Select:

(global-set-mouse '(text        left)	'mouse-drag-move-point)
(global-set-mouse '(text     up left)	'mouse-drag-set-mark-stuff)
(global-set-mouse '(text shift  left)	'mouse-exch-pt-and-mark)
(global-set-mouse '(text double left)	'mouse-exch-pt-and-mark)

(global-set-mouse '(text	middle)	'mouse-set-mark-and-stuff)

(global-set-mouse '(text	right)	'emacs-menu-eval)
(global-set-mouse '(text shift	right)	'(sun-yank-selection))
(global-set-mouse '(text double	right)	'(sun-yank-selection))

;; The Slymoblics multi-command for Save, Kill, Copy, Move:
(global-set-mouse '(text shift	middle)	'mouse-save/delete/yank)
(global-set-mouse '(text double	middle)	'mouse-save/delete/yank)

;; Save, Kill, Copy, Move Things:
;; control-left composes with control middle/right to produce copy/move
(global-set-mouse '(text control middle	    )	'mouse-save-thing-there)
(global-set-mouse '(text control right      )	'mouse-kill-thing-there)
(global-set-mouse '(text control 	left)	'mouse-yank-at-point)
(global-set-mouse '(text control middle	left)	'mouse-copy-thing)
(global-set-mouse '(text control right	left)	'mouse-move-thing)
(global-set-mouse '(text control right middle)	'mouse-mark-thing)

;; The Universal mouse help command (press all buttons):

(global-set-mouse '(text shift  control meta right)	'mouse-help-region)
(global-set-mouse '(text double control meta right)	'mouse-help-region)

;;; Meta in Text Region is like meta version in scrollbar:

(global-set-mouse '(text meta        left)	'mouse-line-to-top)
(global-set-mouse '(text meta shift  left)	'mouse-line-to-bottom)
(global-set-mouse '(text meta double left)	'mouse-line-to-bottom)
(global-set-mouse '(text meta         middle)	'mouse-line-to-middle)
(global-set-mouse '(text meta shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(text meta double  middle)	'mouse-middle-to-line)
(global-set-mouse '(text meta control middle)	'mouse-split-vertically)
(global-set-mouse '(text meta        right)	'mouse-top-to-line)
(global-set-mouse '(text meta shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(text meta double right)	'mouse-bottom-to-line)

;; Miscellaneous:

(global-set-mouse '(text meta control left)	'mouse-call-kbd-macro)
(global-set-mouse '(text meta control right)	'mouse-undo)

;;;
;;; Scrollbar mousemap.
;;; Are available in the Scrollbar Region, or with Meta Text (or Meta Scrollbar)
;;;
(global-set-mouse '(scrollbar        left)	'mouse-line-to-top)
(global-set-mouse '(scrollbar shift  left)	'mouse-line-to-bottom)
(global-set-mouse '(scrollbar double left)	'mouse-line-to-bottom)

(global-set-mouse '(scrollbar         middle)	'mouse-line-to-middle)
(global-set-mouse '(scrollbar shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar double  middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar control middle)	'mouse-split-vertically)

(global-set-mouse '(scrollbar        right)	'mouse-top-to-line)
(global-set-mouse '(scrollbar shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(scrollbar double right)	'mouse-bottom-to-line)

(global-set-mouse '(scrollbar meta        left)		'mouse-line-to-top)
(global-set-mouse '(scrollbar meta shift  left)		'mouse-line-to-bottom)
(global-set-mouse '(scrollbar meta double left)		'mouse-line-to-bottom)
(global-set-mouse '(scrollbar meta         middle)	'mouse-line-to-middle)
(global-set-mouse '(scrollbar meta shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar meta double  middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar meta control middle)	'mouse-split-vertically)
(global-set-mouse '(scrollbar meta        right)	'mouse-top-to-line)
(global-set-mouse '(scrollbar meta shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(scrollbar meta double right)	'mouse-bottom-to-line)

;; And the help menu:

(global-set-mouse '(scrollbar shift  control meta right) 'mouse-help-region)
(global-set-mouse '(scrollbar double control meta right) 'mouse-help-region)

;;;
;;; Modeline mousemap.
;;;
;;; Note: meta of any single button selects window.

(global-set-mouse '(modeline      left)	'mouse-scroll-up)
(global-set-mouse '(modeline meta left)	'mouse-select-window)

(global-set-mouse '(modeline         middle)	'mouse-drag-modeline)
(global-set-mouse '(modeline up      middle)	'mouse-set-modeline)

(global-set-mouse '(modeline meta    middle)	'mouse-split-vertically)
(global-set-mouse '(modeline control middle)	'mouse-split-horizontally)

(global-set-mouse '(modeline      right)	'mouse-scroll-down)
(global-set-mouse '(modeline meta right)	'mouse-select-window)

;;; control-left selects this window, control-right deletes it.

(global-set-mouse '(modeline control left)	'mouse-delete-other-windows)
(global-set-mouse '(modeline control right)	'mouse-delete-window)

;; in case of confusion, just select it:

(global-set-mouse '(modeline control left right) 'mouse-select-window)

;; even without confusion (and without the keyboard) select it:

(global-set-mouse '(modeline left right)	'mouse-select-window)

;; And the help menu:
(global-set-mouse '(modeline shift  control meta right)	'mouse-help-region)
(global-set-mouse '(modeline double control meta right)	'mouse-help-region)

;;;
;;; Minibuffer Mousemap
;;; Demonstrating some variety:
;;;
(global-set-mouse '(minibuffer left)		'mini-move-point)

(global-set-mouse '(minibuffer         middle)	'mini-set-mark-and-stuff)

(global-set-mouse '(minibuffer shift   middle) '(prev-complex-command))
(global-set-mouse '(minibuffer double  middle) '(prev-complex-command))
(global-set-mouse '(minibuffer control middle) '(next-complex-command 1))
(global-set-mouse '(minibuffer meta    middle) '(previous-complex-command 1))

(global-set-mouse '(minibuffer right)	'minibuffer-menu-eval)

(global-set-mouse '(minibuffer shift  control meta right)  'mouse-help-region)
(global-set-mouse '(minibuffer double control meta right)  'mouse-help-region)


