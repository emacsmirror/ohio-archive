;;; -*-Emacs-Lisp-*-
;;; %Header
;;; Shrink-wrapped temporary windows for GNU Emacs V2.11
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.
;;; Thanks to Ken Laprade for suggestions and patches.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; DESCRIPTION: This module provides a single shrink-wrapped window
;;; for displaying temporary text called the popper window.  At any
;;; time there is only one popper window on the screen.  If there is
;;; an entry for the buffer being displayed on popper-min-heights, the
;;; size of the window will be from that entry.  If the buffer is
;;; empty, the size of the window will be popper-empty-min.  Otherwise
;;; its size will be the minimum of the number of lines of text being
;;; displayed and half the number of lines in the currently selected
;;; window when the popper window was created.  It will work with any
;;; function that uses temporary windows or that has been wrapped with
;;; popper-wrap.  The window can be scrolled or buried from any other
;;; window.
;;;
;;; When a buffer is displayed using the function
;;; with-output-to-temp-buffer, the text will be displayed in the
;;; popper window if the name of the buffer is in popper-pop-buffers
;;; or popper-pop-buffers is set to T and the name is not in
;;; popper-no-pop-buffers.  Many kinds of completion and help
;;; information are displayed this way.  In general any buffer with
;;; *'s around its name will be a temporary buffer.  Some commands
;;; like shell-command do not use with-output-to-temp-buffer even
;;; though you might like to have their output be temporary.  For
;;; commands like this, you can define a wrapper like this using the
;;; function popper-wrap.
;;;
;;; The default binding for C-x o is changed so that when a buffer is
;;; displayed in a popper window, it will be skipped if it is in
;;; popper-buffers-to-skip or popper-buffers-to-skip is T and it is
;;; not in popper-buffers-no-skip.

;;; USAGE: Load this file, preferably after byte-compiling it.  If you
;;; do not define key bindings using popper-load-hook, the bindings
;;; will be:
;;; 
;;;  C-z 1   popper-bury-output
;;;  C-z v   popper-scroll-output
;;;  C-z g   popper-grow-output
;;;  C-z b   popper-switch
;;;  C-x o   popper-other-window (C-u to select popper window)

;;; See %%User variables below for possible options.  Here is a sample
;;; load hook for your .emacs:
;;;
;;; (setq popper-load-hook 
;;;      '(lambda ()
;;;        ;; Define key bindings
;;;        (define-key global-map "\C-c1" 'popper-bury-output)
;;;        (define-key global-map "\C-cv" 'popper-scroll-output)
;;;        (define-key global-map "\C-cg" 'popper-grow-output)
;;;        (define-key global-map "\C-cb" 'popper-switch)
;;;        ;; Make *Manual windows default to 10 lines
;;;        (setq popper-min-heights
;;;              (cons (cons "^\\*Manual" 10) popper-min-heights)
;;;              ;; Don't skip over *Buffer List*
;;;              popper-buffers-no-skip (cons "*Buffer List*" 
;;;                                             popper-buffers-no-skip))))
;;; This is an example that will not work in your .emacs
;;;        ;; Make command's output buffer a popper window even though
;;;        ;; it does not use with-output-to-temp-buffer
;;;        (popper-wrap 'command "*Command Output*")))
;;; (require 'popper)

;;; WARNING: This package redefines the function split-window and
;;; pop-to-buffer so that the popper window is buried before calling
;;; the old definition.

;;;%Globals
;;;%%User variables
(defvar popper-load-hook nil
  "List of functions to run when the popper module is loaded.")

;;;
(defvar popper-pop-buffers t
  "*List of buffers to put in the shrink-wrapped pop-up window.  
If it is T, all temporary buffers will be put in the pop-up window.")

(defvar popper-no-pop-buffers nil
  "*If popper-pop-buffers is T, these buffers will not be put into the
pop-up window.")

(defvar popper-buffers-to-skip popper-pop-buffers
  "*\\[popper-other-window] will skip over these buffers when they are
used in a temporary window.  If it is T, all popper windows will be
skipped except those in popper-buffers-no-skip.")

(defvar popper-buffers-no-skip nil
  "*\\[popper-other-window] will not skip these buffers when they are
used in a popper window if popper-buffers-to-skip is T.")

;;; By default, this is set to 2 so that a window can be one line big.
;;; The number of lines in the popper window plus the mode line will
;;; never be less than this value.  (In fact no window will be less
;;; than this value.)
(setq window-min-height 2)

;;;
(defvar popper-empty-min '(50) 
  "*Minimum number of lines to display for an empty popper buffer.  If
it is a list, it is an integer percentage 0-100 of the available space.")

;;;
(defvar popper-min-heights '(("^\\*compilation\\*" . (50)))
  "*List of cons where each the car is a regular expression pattern to
match a buffer name and the cdr is the minimum number of lines to
allow when popping buffers that match the regular expression.  If the
number is a list, it is interpreted as the percentage of available
space 0-100 to use for the window.")

(defvar popper-mode-line-text nil
  "*Minor mode text for mode line of popper buffers.  If nil, it will
be set to a short help message on first use of popper.")

;;;%%Internal variables
(defvar popper-output-buffers nil
  "LIFO list of buffers displayed in the popper window.")
(defvar popper-last-output-window nil
  "The window that last popped up an output window.")

(defvar popper-buffer nil
  "Indicates buffer is a popper for minor-mode-alist.")
(make-variable-buffer-local 'popper-buffer)
(or (assq 'popper-buffer minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(popper-buffer popper-mode-line-text) minor-mode-alist)))

;;;%Utils
;;; This should be in emacs, but it isn't.
(defun popper-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Option comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
	  (setq list (cdr list))))
    done))

;;;
(defun popper-select (&optional window)
  "Select WINDOW and its buffer.  WINDOW defaults to selected-window."
  (setq window (or window (selected-window)))
  (select-window window)
  (set-buffer (window-buffer window)))

;;;
(defun popper-first-buffer ()
  "Remove killed buffers and return the first buffer on
popper-output-buffers."
  (while (and popper-output-buffers
	      (null (buffer-name (car popper-output-buffers))))
    (setq popper-output-buffers (cdr popper-output-buffers)))
   (let ((buffers popper-output-buffers))
     (while (cdr buffers)
       (if (buffer-name (car (cdr buffers)))
	   (setq buffers (cdr buffers))
	   (rplacd buffers (cdr (cdr buffers))))))
   (car popper-output-buffers))

;;;
(defun popper-output-buffer ()
  "Return the buffer being displayed in the popper window."
  (popper-first-buffer)
  (if (not (eq (selected-window) (next-window)))
      (let ((buffers popper-output-buffers)
	    (done nil))
	(while (and buffers (not done))
	  (let* ((buffer (car buffers))
		 (window (if (buffer-name buffer) (get-buffer-window buffer))))
	    (if window 
		(setq done buffer)
		(save-excursion
		  (set-buffer (car buffers)) (setq popper-buffer nil))
		(setq buffers (cdr buffers)))))
	(if done (setq popper-output-buffers buffers))
	done)))

;;;
(defun popper-parent ()
  "Return the parent of the popper window."
  (let ((output (popper-output-buffer)))
    (if output (next-window (get-buffer-window output) 'no))))

;;;
(defun popper-window-heights (window)
  "Return a list of the heights of all of the windows following WINDOW."
  (let ((heights nil))
    (select-window window)
    (while (progn 
	     (select-window (next-window (selected-window) 'no))
	     (not (eq (selected-window) window)))
      (setq heights (cons (window-height (selected-window)) heights)))
    (reverse heights)))

;;;
(defun popper-min-height ()
  "Return the minimum height to use for the buffer in the current
window.  This is either an entry from popper-min-heights,
popper-empty-min if the buffer is empty or window-min-height."
  (let ((buffer (buffer-name))
	(pat popper-min-heights)
	min)
    (while pat
      (if (string-match (car (car pat)) buffer)
	  (setq min (cdr (car pat))
		pat nil)
	(setq pat (cdr pat))))
    (if (not min)
	(setq min
	      (if (= (point-min) (point-max))
		  popper-empty-min
		  window-min-height)))
    (if (consp min)
	;; Floating percentage
	(/ (* (+ (window-height) (window-height (next-window)))
	      (car min))
	   100)
	min)))

;;;
(defun popper-show-output (&optional buffer size)
  "Bring the output window up showing optional BUFFER in window of
SIZE.  If SIZE is not specified, then shrink the window.  Finally
select the original window."
  (let* ((window (selected-window))
	 (old-buffer (window-buffer window))
	 (buffer (get-buffer-create
		  (or buffer (popper-first-buffer) 
		      (error "No popper buffers"))))
	 start parent
	 (min-height (+ window-min-height (or size window-min-height))))
    (setq popper-last-output-window window)
    (if (eq buffer old-buffer)
	(popper-shrink-window)
	(if (eq window (minibuffer-window)) 
	    (let* ((parent (popper-parent)))
	      (popper-bury-output t)
	      (select-window (setq window (or parent (previous-window)))))
	    (if (not (eq old-buffer (popper-output-buffer)))
		(popper-bury-output t)))
	(if (< (window-height window) min-height)
	    (enlarge-window (- min-height (window-height window))))
	(setq start (window-start window))
	(split-window nil size)
	(set-window-buffer window buffer)
	(set-buffer buffer)
	(setq popper-buffer t)
	(or popper-mode-line-text
	    (setq popper-mode-line-text
		  (list (format " %s bury, %s scroll" 
				(where-is-internal 'popper-bury-output nil t)
				(where-is-internal 'popper-scroll-output nil t)))))
	(setq popper-output-buffers
	      (cons buffer (delq buffer popper-output-buffers)))
	(if (not size) (popper-shrink-window))
	(setq parent (next-window window 'no))
	(popper-select parent)
	;; Move the window so that top lines get covered unless it would
	;; cover point in which case point is at top of window
	(save-excursion
	  (set-window-start parent start)
	  (move-to-window-line (window-height window))
	  (set-window-start parent (point)))
	(let ((point (save-excursion (beginning-of-line) (point))))
	  (if (not (pos-visible-in-window-p point))
	      (set-window-start (selected-window) point)))
	(if (eq popper-last-output-window (minibuffer-window))
	    (select-window (minibuffer-window)))
	(set-buffer old-buffer))))

;;;
(defun popper-shrink-window ()
  "Shrink the current window if larger than its buffer unless it has
an entry in popper-min-heights or it is empty in which case
popper-empty-min is used."
  (let* ((window (selected-window))
	 (window-lines (1- (window-height window))))
    (set-buffer (window-buffer window))
    (let ((buffer-read-only nil)
	  (buffer-modified-p (buffer-modified-p)))
      (save-excursion
	;; Delete trailing blank lines
	(goto-char (point-max))
	(skip-chars-backward "\n")
	(if (< (point) (point-max)) (delete-region (1+ (point)) (point-max)))
	(goto-char (point-min))
	;; Delete leading blank lines
	(if (looking-at "\n+") (replace-match ""))
	(set-buffer-modified-p buffer-modified-p)))
    (enlarge-window (- (max (1+ (save-excursion 
				  (goto-char (point-min))
				  (vertical-motion window-lines)))
			    (1- (popper-min-height)))
		       window-lines))))

;;;
(defun popper-show (buffer)
  "Function to display BUFFER in a popper window if it is in
popper-pop-buffers or popper-pop-buffers is T and it is not in
popper-no-pop-buffers."
  (let ((name (if (bufferp buffer) (buffer-name buffer) buffer)))
    (if (eq popper-pop-buffers t)
	(if (not (popper-mem name popper-no-pop-buffers))
	    (popper-show-output buffer)
	    (display-buffer buffer))
	(if (popper-mem name popper-pop-buffers)
	    (popper-show-output buffer)
	    (display-buffer buffer))))
  (setq minibuffer-scroll-window (get-buffer-window buffer)))

;;;%Commands
(defun popper-bury-output (&optional no-error)
  "Bury the popper output signalling an error if not there unless
optional NO-ERROR is T."
  (interactive)
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let* ((old (current-buffer))
	       (old-window (selected-window))
	       (output (get-buffer-window buffer))
	       (start (window-height output))
	       (parent (next-window output 'no))
	       (height (window-height output))
	       (heights (popper-window-heights output)))
	  (bury-buffer buffer)
	  (delete-window output)
	  (popper-select parent)
	  (while heights
	    (enlarge-window (- (+ height (car heights)) (window-height)))
	    (if start
		(condition-case () (scroll-down start) (error nil)))
	    (select-window (next-window (selected-window) 'no))
	    (setq height 0 start nil)
	    (setq heights (cdr heights)))
	  (set-buffer old)
	  (if (not (eq old-window output))
	      (select-window old-window)))
	(if (not no-error) (popper-show-output)))))

;;;
(defun popper-scroll-output (&optional n)
  "Scroll text of the popper window upward ARG lines ; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "P")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let ((window (selected-window)))
	  (unwind-protect
	       (progn (select-window (get-buffer-window buffer))
		      (condition-case ()
			  (scroll-up n)
			(error
			 (if (or (null n) (and (numberp n) (> n 0)))
			     (goto-char (point-min))
			     (goto-char (point-max))))))
	    (select-window window)))
	(popper-show-output))))

;;;
(defun popper-grow-output (&optional n)
  "Grow the popper window by ARG (default 1) lines.  If the popper
window is not being shown, it will be brought up."
  (interactive "p")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let ((old-buffer (current-buffer))
	      (window (selected-window)))
	  (select-window (get-buffer-window buffer))
	  (enlarge-window n)
	  (popper-select (next-window (selected-window) 'no))
	  (save-excursion
	    (if (< n 0)
		(condition-case () (scroll-up n) (error nil))
		(move-to-window-line n)
		(set-window-start (selected-window) (point))))
	  (select-window window)
	  (set-buffer old-buffer))
	(popper-show-output))))

;;;
(defun popper-switch (buffer)
  "Switch the popper window to BUFFER."
  (interactive
   (list (read-buffer
	  "Popper buffer " 
	  (car (if (popper-output-buffer)
		   (cdr popper-output-buffers)
		   popper-output-buffers))
	  t)))
  (if buffer (popper-show buffer)))
	 
;;;%Redefinitions
;;;***Redefine split-window to bury popper window first***
(defvar popper-split-window (symbol-function 'split-window) 
  "Original definition of split-window.")
(defun split-window (&optional window size hor-flag)
  "Split WINDOW, putting SIZE lines in the first of the pair.
WINDOW defaults to selected one and SIZE to half its size.
If optional third arg HOR-FLAG is non-nil, split side by side
and put SIZE columns in the first of the pair."
  (let ((parent (popper-parent)))
    (if (eq parent (selected-window))
	(let* ((pop-size (window-height
			  (get-buffer-window (popper-output-buffer))))
	       (size (if size (+ size pop-size))))
	  (popper-bury-output)
	  (prog1
	      (funcall popper-split-window window size hor-flag)
	    (popper-show-output nil pop-size)))
	(funcall popper-split-window window size hor-flag))))

;;; ***Redefine pop-to-buffer to skip output windows***
(defvar popper-pop-to-buffer (symbol-function 'pop-to-buffer)
  "Original pop to buffer function.")
(defun pop-to-buffer (buffer &optional other-window)
  "Select buffer BUFFER in some window, preferably a different one.
If pop-up-windows is non-nil, windows can be split to do this.
If second arg OTHER-WINDOW is non-nil, insist on finding another
window even if BUFFER is already visible in the selected window."
  (let ((parent (popper-parent)))
    (if (and parent 
	     (not (eq (get-buffer buffer) (popper-output-buffer))))
	(progn
	  (popper-bury-output)
	  (funcall popper-pop-to-buffer buffer other-window)
	  (select-window parent)
	  (popper-show-output)
	  (sit-for 0)			;Allow display update
	  (funcall popper-pop-to-buffer buffer))
	(funcall popper-pop-to-buffer buffer other-window))))

;;;***Redefine other-window to skip popper buffers***
(defun popper-other-window (arg)
  "Select the arg'th other window.  If arg is a C-u prefix, the popper
window will be selected.  Otherwise, windows that contain buffers in
popper-buffers-to-skip will be skipped or if popper-buffers-to-skip is
T those that are not in popper-buffers-no-skip."
  (interactive "P")
  (if (consp arg) 
      (let* ((buffer (popper-output-buffer))
	     (window (and buffer (get-buffer-window buffer))))
	(if window (select-window window)))
      (setq arg (if (eq arg '-) -1 (or arg 1)))
      (other-window arg)
      (if (eq popper-buffers-to-skip t)
	  (if (and (not (popper-mem (buffer-name (current-buffer))
				    popper-buffers-no-skip))
		   (eq (popper-output-buffer) (current-buffer)))
	      (other-window arg))
	  (if (popper-mem (buffer-name (current-buffer))
			  popper-buffers-to-skip)
	      (other-window arg)))))
(define-key ctl-x-map "o" 'popper-other-window)

;;; %Wrappers
(defun popper-unwrap (function)
  "Remove the popper wrapper for NAME."
  (let ((var (car (read-from-string (format "popper-%s" function)))))
    (if (boundp var)
	(progn (fset function (symbol-value var))
	       (makunbound var)))))

;;;
(defun popper-wrap (function buffer)
  "Define a wrapper on FUNCTION so that BUFFER will be a pop up window."
  (popper-unwrap function)
  (let* ((var (car (read-from-string (format "popper-%s" function))))
	 (defn (symbol-function function))
	 arg-spec doc int)
    (set var defn)
    (if (consp defn)
	(setq arg-spec (elt defn 1)
	      doc (elt defn 2)
	      int (elt defn 3))
	(setq arg-spec (aref defn 0)
	      doc (and (> (length defn) 4) (aref defn 4))
	      int (and (> (length defn) 5) (list 'interactive (aref defn 5)))))
    (fset function 
	  (append 
	   (list 'lambda arg-spec)
	   (if (numberp doc) (list (documentation function)))
	   (if (stringp doc) (list doc))
	   (if (eq (car int) 'interactive) (list int))
	   (list 
	    (list
	     'let '((shown nil))
	     (list 'save-window-excursion 
		   (cons 'funcall 
			 (cons 
			  var
			  (let ((args nil))
			    (while arg-spec
			      (if (not (eq (car arg-spec) '&optional))
				  (setq args (cons (car arg-spec)
						   args)))
			      (setq arg-spec (cdr arg-spec)))
			    (reverse args))))
		   (list 'setq 'shown (list 'get-buffer-window buffer)))
	     (list 'if 'shown
		   (list 'funcall 'temp-buffer-show-hook buffer))))))
    (if (not (eq popper-pop-buffers t))
	(let ((elt popper-pop-buffers))
	  (while (consp elt)
	    (if (string= (car elt) buffer) 
		(setq elt t)
		(setq elt (cdr elt))))
	  (if (not elt)
	      (setq popper-pop-buffers (cons buffer popper-pop-buffers)))))))

;;; 
(popper-wrap 'shell-command "*Shell Command Output*")
(popper-wrap 'shell-command-on-region "*Shell Command Output*")

;;;
(setq temp-buffer-show-hook 'popper-show)
(run-hooks 'popper-load-hook)

;;; Default key bindings
(if (not (where-is-internal 'popper-bury-output nil t))
    (progn
      (if (not (keymapp (lookup-key global-map "\C-z")))
	  (define-key global-map "\C-z" (make-keymap)))
      (define-key global-map "\C-z1" 'popper-bury-output)
      (define-key global-map "\C-zv" 'popper-scroll-output)
      (define-key global-map "\C-zg" 'popper-grow-output)
      (define-key global-map "\C-zb" 'popper-switch)))

(provide 'popper)
