;------------------------------------------------------------;
; line+.el
;
; version 1.1
;
; This has not (yet) been accepted by the Emacs Lisp archive,
; but if it is the archive entry will probably be something like this:

;; LCD Archive Entry:
;; line+|Neil Jerram|nj104@cus.cam.ac.uk|
;; Line numbering and interrupt driven actions.|
;; 1993-02-18|1.1|~/misc/line+.el.Z|

; Mished and mashed by Neil Jerram <nj104@cus.cam.ac.uk>,
; Monday 21 December 1992.
; Copyright (C) 1993 Neil Jerram.
;
; Horizontal scrolling code is by Wayne Mesard <WMesard@cs.stanford.edu>,
; Copyright (C) 1992 Wayne Mesard.

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
;;; The GNU General Public License is available by anonymous ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.

; A mix'n'match of linenumbers.el by Ajay Shekhawat
;              and hscroll.el by Wayne Mesard
;
; OR...
;
; ``If it's acceptable to have 1 second polling for horizontal scroll
;   checking, then why not do line numbers the easy way ?''
;
; USAGE:
;
; `M-x linenumbers' to toggle line numbering in the current buffer.
;
; `M-x linenumbers-shutdown' to kill the external process and turn off
;                            line numbering in all buffers.
;
; `M-x ln-start-process' to start/restart the external process
;                        (perhaps with a new polling period).
;
; `M-x linenumbers-all-buffers' to switch on line numbering in all
;                               future buffers.
;
; `C-u M-x linenumbers-all-buffers' to switch on line numbering in all
;                                   future and existing buffers.
;                                   
; `M-x linenumbers-in-certain-modes' to switch on line numbering in all
;                                    future buffers whose major mode is
;                                    represented in the variable
;                                    `ln-mode-hook-list'
;
; INSTALLATION EXAMPLES (in your `.emacs' file):
;
; (1) Loading code upon starting Emacs:
;
; ; either
; (load "line+")
; ; if the file `line+.el' lies in your `load-path',
; ; or, for example,
; (load "/home/neil/emacs-lisp/line+")
; ; followed by
; ; any changes to the default values of `ln-format',
; ; `ln-format-options', `ln-mode-hook-list',
; ; followed by
; ; either
; (linenumbers-all-buffers t)
; ; or
; (linenumbers-in-certain-modes)
; ; according to your preference.
;
; (2) Autoloading the code upon invocation of `M-x linenumbers':
;
; ; either
; (autoload 'linenumbers "line+"
;           "Switch on line numbers in the current buffer." t)
; ; or
; (autoload 'linenumbers "/home/neil/emacs-lisp/line+"
;           "Switch on line numbers in the current buffer." t)
; ; depending on your `load-path' as in (1).

(provide 'line+)

; ----------------
; Public variables
; ----------------

(defvar ln-format " l.%l%h"
  "*A string describing what information is presented in the mode line.
Percentage constructs in this string cause substitution of the
relevant information.  The currently understood options are:

	%l	insert current line number (within current restriction)
	%m	insert current line number (after widening any restrictions)
	%c	insert current column
	%t	insert the 24hr clock time in format hh:mm:ss
	%h	scrolls window horizontally when necessary, inserts nothing
	%H	scrolls window horizontally when necessary and
		inserts the left margin offset
	%%	insert `%'

Further options may be defined by adding to the variable
`ln-format-options'.")

; My thanks to John M. Klassa for spurring me on to extend `ln-format'
; in this way.

; As an example of alternative formats, you might write
;  (setq ln-format "-= line %l column %c =-")
; in your `.emacs' file after the instruction to load in the lisp code.
; The code as it stands does not allow the format to be different in
; different buffers, but you can achieve this if you want it by adding
;  (make-variable-buffer-local 'ln-format)
; in your `.emacs' file.
;   Changes to ln-format will not carry into other buffers until they
; become active (even 'though they may be visible in another window).
; This could be remedied, by making ln-filter cycle through all existing
; buffers every time it gets a signal from "Wakeup!", but I don't
; really think it's worth it.

(defvar ln-format-options (list (list "%l" "%d" '(ln-which-line))
				(list "%m" "%d" '(save-restriction
						   (widen)
						   (ln-which-line)))
				(list "%c" "%d" '(1+ (current-column)))
				(list "%t" "%s" '(ln-time))
				(list "%h" "" '(hscroll-window-maybe))
				(list "%H" "%d" '(hscroll-window-maybe)))
  "This variable is a list, each of whose elements looks like
(list FORMAT-LN-TYPE FORMAT-C-TYPE LISP-FORM).
  FORMAT-LN-TYPE is the percentage string that should appear
in `ln-format', e.g. \"%l\" in `ln-format' means include the
current line number.
  FORMAT-C-TYPE describes the way that the information should
be inserted into the printed string, e.g. as a decimal number \"%d\"
or as a string \"%s\" (the %'s here are as understood by `format').
  LISP-FORM is the lisp expression that should be evaluated to
produce the relevant information.")

(defvar ln-poll-period "1"
  "*Interval between checking the current line number (in seconds).
If nil, it will test continuously (but this is not recommended, 
since it will slow down your machine and annoy Emacs).")

(defvar ln-mode nil 
  "*Whether ln-mode is enabled for the current buffer.
Ordinarily set indirectly (via \\[linenumbers]).")
(make-variable-buffer-local 'ln-mode)

; ----------------
; Public functions
; ----------------

(defun linenumbers (&optional onoff)
  "Toggle line+ mode in the current buffer.
With arg, turn line+ mode on if arg is positive, off otherwise."
  (interactive "P")
  (setq ln-mode (if onoff
		    (> (prefix-numeric-value onoff) 0)
		  (not ln-mode)))
  (or ln-mode
      (setq ln-string ""))
  (and ln-mode
       (null ln-process)
       (ln-start-process)))

(defun linenumbers-shutdown ()
  "Disable line+ mode in all buffers, and terminate 
the line+ subprocess.  This command is an \"emergency switch\"
for use if the subprocess starts hogging up too many system resources."
  (interactive)
  (ln-kill-process-quietly ln-process)
  (setq ln-process nil)
  (let* ((buffers (buffer-list))
	 (i (length buffers)))
    (while (>= (setq i (1- i)) 0)
      (set-buffer (nth i buffers))
      (linenumbers -1))))

(defun linenumbers-all-buffers (arg)
  "Set up for line numbering to come on in all buffers.
Explicitly sets ln-mode to t everywhere if ARG is given."
  (interactive "P")
  (setq-default ln-mode t)
  (if arg
      (let* ((buffers (buffer-list))
	     (i (length buffers)))
	(while (>= (setq i (1- i)) 0)
	  (set-buffer (nth i buffers))
	  (setq ln-mode t))))
  (ln-start-process))

(defun linenumbers-in-certain-modes ()
  "Set up for line numbering in certain major modes."
  (interactive)
  (ln-modify-mode-hooks ln-mode-hook-list))

(defun ln-start-process ()
  "Starts (or restarts) the \"wakeup\" process for line+ mode."
  (interactive)
  (ln-kill-process-quietly ln-process)
  (let ((process-connection-type nil))
    (setq ln-process (start-process "line+" nil
				    (concat exec-directory "wakeup")
				    (or ln-poll-period "0"))))
  (set-process-sentinel ln-process 'ln-sentinel)
  (set-process-filter ln-process 'ln-filter)
  (process-kill-without-query ln-process))

; ---------
; Internals
; ---------

(defvar ln-string nil
  "String printed in the mode line describing the current line number.")
(make-variable-buffer-local 'ln-string)

(defvar ln-process nil
  "Variable holding the line+ process object.")

(defun ln-modify-mode-line-format ()
  "Modify the mode-line-format to insert the line number after
mode-line-buffer-identification."
  (or (memq 'ln-string mode-line-format)
      (let ((mlbi (memq 'mode-line-buffer-identification 
			mode-line-format)))
	(if mlbi
	    (setq mode-line-format
		  (append (reverse (memq 'mode-line-buffer-identification
					 (reverse mode-line-format)))
			  '(ln-string)
			  (cdr mlbi)))))))


(defun ln-kill-process-quietly (proc)
  "Turns off filter and sentinel before killing the process PROC."
  (and (processp proc)
       (eq (process-status proc) 'run)
       (progn
	 (set-process-filter proc nil)
	 (set-process-sentinel proc nil)
	 (kill-process proc))))

(defun ln-which-line ()
  "Returns the current line number, counting from 1."
  (1+ (count-lines (point-min)
		   (save-excursion (beginning-of-line) (point)))))

(defun ln-time ()
  "Returns a string describing the time."
  (let* ((cts (current-time-string))
	 (col (string-match ":" cts)))
    (substring cts (- col 2) (+ col 6))))
  
(defun ln-make-ln-string (format)
  (let (ppos
	format-ln-type
	format-option
	(string nil))
    (while (setq ppos (string-match "%" format))
      (setq string (concat string (substring format 0 ppos))
	    format (substring format ppos)
	    format-ln-type (substring format 0 2)
	    format (substring format 2))
      (cond
       ((string= format-ln-type "%%")
	(setq string (concat string "%")))
       ((setq format-option (assoc format-ln-type ln-format-options))
	(setq string (concat string (format (nth 1 format-option)
					    (save-restriction
					      (eval (nth 2 format-option)))))))
       (t
	(setq string (concat string format-ln-type)))))
    (concat string format)))

(defun ln-filter (ignore ignore)
  ;; Don't even bother if we're not in the mode.
  (if ln-mode
      (progn
	(setq ln-string (ln-make-ln-string ln-format))
	(ln-modify-mode-line-format))))

(defun ln-sentinel (ignore reason)
  (linenumbers-shutdown)
  (error "Whoa: the line+ process died unexpectedly: %s." reason))

; ------------------------------------------------------------------
; Additions to persuade line numbers to appear only in certain modes
; ------------------------------------------------------------------

(defun linenumbers-1 ()
  (linenumbers 1))

(defun ln-existify (sym)
  "Give symbol SYM a value of nil if it isn't already bound."
  (or (boundp sym)
      (set sym nil)))

(defun ln-modify-mode-hooks (hooklist)
  "Add a function to turn on line numbering to each of the
major mode hooks (symbols) listed in HOOKLIST."
  (let ((i (length hooklist))
	hooksym
	hookval)
    (while (>= (setq i (1- i)) 0)
      (setq hooksym (nth i hooklist))
      (ln-existify hooksym)
      (setq hookval (symbol-value hooksym))
      (or (listp hookval)
	  (setq hookval (list hookval)))
      (or (memq 'linenumbers-1 hookval)
	  (set hooksym (cons 'linenumbers-1 hookval))))))

(defvar ln-mode-hook-list (list 'fortran-mode-hook
				'c-mode-hook
				'emacs-lisp-mode-hook
				'TeX-mode-hook
				'lisp-mode-hook)
  "List of mode hooks which should be modified to insert
a command for switching line numbers on.")

; --------------------------------------------------
; Horizontal scrolling code, written by Wayne Mesard
; Copyright (C) 1992 Wayne Mesard
; --------------------------------------------------

;;; DESCRIPTION
;;    Automatically scroll horizontally when the point moves off the
;;    left or right edge of the window.  Include "%h" in the value of
;;    `ln-format' to enable automatic horizontal scrolling.
;;    This only has effect when the current line is truncated by Emacs.
;;    Say "Control-h f hscroll-truncate-lines" for details.
;;
;;    HScroll's sensitivity is controlled by the variable hscroll-margin.
;;    How much HScroll adjusts the window is determined by hscroll-step.
;;
;;    Most users won't have to mess with the other variables and functions 
;;    defined here.  But they're all documented, and they all start with 
;;    "hscroll-" if you're curious.
;;
;;    Oh, you should also know that if you set the hscroll-margin and
;;    hscroll-step large enough, you can get an interesting, but
;;    undesired ping-pong effect as the point bounces from one edge to
;;    the other.
;;
;;    WMesard@cs.stanford.edu

(defvar hscroll-margin 5 
  "*How many columns away from the edge of the window point is allowed to get
before HScroll will horizontally scroll the window.")

(defvar hscroll-step 25
  "*How far away to place the point from the window's edge when scrolling.
Expressed as a percentage of the window's width.")

(defun hscroll-truncate-lines (&optional onoff)
  "Toggle the value of the Emacs variable truncate-lines in the current buffer.  
With arg, set to t if arg is positive, nil otherwise.  This is just a
convenience function and not really part of HScroll.  Without it, you'd
have to use set-variable to change the value of truncate-lines.

Say \\[describe-variable] truncate-lines and \\[describe-variable] \
truncate-partial-width-windows for details."
  (interactive "P")
  (setq truncate-lines (if onoff
			   (> (if (numberp onoff) onoff 
				(prefix-numeric-value onoff))
			      0)
			 (not truncate-lines))
	))

(defun hscroll-window-maybe ()
  "Scroll horizontally if point is off or nearly off the edge of the window.
This is called automatically when \"%h\" or \"%H\" is included in the variable
`ln-format', but it can be explicitly invoked as well.  This function
returns the left margin offset, which will be inserted in the string
displayed in the mode line if invoked via \"%H\"."
  (interactive)
  ;; Only consider scrolling if truncate-lines is true, 
  ;; the window is already scrolled or partial-widths is true and this is
  ;; a partial width window.  See display_text_line() in xdisp.c.
  (if (or truncate-lines
	  (not (zerop (window-hscroll)))
	  (and truncate-partial-width-windows
	       (< (window-width) (screen-width))))
      (let ((linelen (save-excursion (end-of-line) (current-column)))
	    (rightmost-char (+ (window-width) (window-hscroll)))
	    )
	(if (>= (current-column)
		(- rightmost-char hscroll-margin
		   ;; Off-by-one if the left edge is scrolled
		   (if (not (zerop (window-hscroll))) 1 0)
		   ;; Off by one if the right edge is scrolled
		   (if (> linelen rightmost-char) 1 0)))
	    ;; Scroll to the left a proportion of the window's width.
	    (set-window-hscroll 
	     (selected-window) 
	     (- (+ (current-column) 
		   (/ (* (window-width) hscroll-step) 100))
		(window-width)))
	  (if (< (current-column) (+ (window-hscroll) hscroll-margin))
	      ;; Scroll to the right a proportion of the window's width.
	      (set-window-hscroll
	       (selected-window)
	       (- (current-column) (/ (* (window-width) hscroll-step) 100)))
	    ))
	))
  (window-hscroll))

; ---
; End
; ---
;------------------------------------------------------------;
