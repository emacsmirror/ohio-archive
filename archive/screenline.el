;;; screenline.el
;; Copyright (C) 2000 Joshua E. Buhl

;; Emacs Lisp Archive Entry
;; Filename: screenline.el
;; Version: 2.03
;; Author: Joshua E. Buhl <josh@math.uni-bonn.de>
;; Description: edit and move cursor independently of line wrapping.
;; Keywords: line wrapping, screen line, cursor

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary

;; This package provides Screen Line Minor Mode.  When activated, it
;; rebinds the keys C-n, C-p, C-k, C-a, C-e, up, down, which are
;; usually bound to next-line, previous-line, kill-line,
;; beginning-of-line, end-of-line, (and again) next-line, and
;; previous-line, respectively.  The keys are rebound to replacement
;; functions sl-next-screen-line, sl-previous-screen-line,
;; sl-kill-screen-line, sl-beginning-of-screen-line, and
;; sl-end-of-screen-line. Evaluating this buffer alone does not rebind
;; these keys.  Rebinding is buffer local.

;; Note that for lines which are not wrapped, the new functions should
;; behave exactly like the standard functions, i.e. it doesn't hurt to
;; have screen-line-mode turned on all the time.

;; The code for sl-next-screen-line and sl-previous-screen-line is now
;; identical to that from next- and previous-line (except for the fix
;; for XEmacs' zmacs-region-stays).  sl-line-move is the line-move
;; source from simple.el made sl-screen-line-mode aware, i.e. you
;; could use sl-line-move as a drop-in replacement for line-move by
;; eliminating the sl- prefix.  This would make next- and
;; previous-line sl-screen-line-mode aware and you would need neither
;; to rebind keys with the minor-mode-map-alist, nor the sl-next- and
;; sl-previous-screen-line functions.

;; In order to use this package, put screenline.el somewhere in your
;; load-path and byte-compile it. Put (require 'screenline) in your
;; .emacs file. Then do a M-x sl-screen-line-mode to activate it for
;; the current buffer. "ScrLn" appears in the mode line, to indicate
;; that the mode is active.  Alternatively to putting the package in
;; your load-path, you could just paste the whole thing into your
;; .emacs file.  If you want Screen Line Minor mode to be on by
;; default, change the "(defcustom sl-screen-line-mode nil" below to
;; "(defcustom sl-screen-line-mode t" and re byte-compile.

;; You can do a `M-x eval-buffer' and a `M-x sl-screen-line-mode' if
;; you just want to test it. There's a couple of test lines at the end
;; of this file.

;;; Version history: 

;; - 1.0 original implementation and posting to gnu.emacs.sources;
;; - 1.1 clean-up based on suggestions received from original posting;
;; - 1.2 added sl-kill-screen-line.  Changed name from screenmotion.el
;; to screenline.el to comply with thirteen character name limit (see
;; coding conventions node in elisp ref.)
;; - 2.0 Made into minor-mode. Ripped out the insides and integrated
;; it with the source from simple.el.
;; - 2.02 Fixed some bugs which got introduced during the conversion
;; to a minor mode.
;; - 2.03 Made the keymap compatible with XEmacs. Added code from
;; `bmonkey' to deactive sl-screen-line-mode automatically for certain
;; buffers (e.g. dired)

;;; ToDo 

;; - sl-kill-screen-line is a hack. The correct way to implement this
;; is to make forward-visible-line and end-of-visible-line in
;; simple.el sl-screen-line-mode aware and leave the kill-line code
alone.  
;; - Maybe implement a screen-line-global-mode.  Is this desirable? 

;;; Known Bugs 

;; - I've not adequately tested it with intangible and invisible text
;; (people who use outline-mode are encouraged to beta test.)  Bug
;; reports are welcome!
;; - sl-kill-screen-line kills one character too few on a wrapped line
;; (see ToDo above.)

;;; Thanks

;; This package now contains a large chunk of code from simple.el, GNU
;; Emacs version 20.7.2. I also copied code and ideas from the GNU
;; ELisp Ref. Manual and by looking at paren.el from GNU Emacs.  All
;; are GPL'd software.

;; I've included a piece of code from `bmonkey' (Yuji Minejima), which
;; deactives screen-line-mode automatically for modes like dired, tar,
;; and the mini-buffer.  Suggestions on other modes for which this is
;; good are welcome.

;; Thanks to the following people for their helpful suggestions: Kalle
;; Olavi Niemitalo, Gareth Owen, Kai Grossjohann, Guillaume Conjat,
;; Nick Selwyn, Colin Walters.

;;; Code:

(defgroup sl-screenline nil
  "Cursor motion independent of line-wrapping"
  :group 'editing-basics)

(defcustom sl-screen-line-mode t
  "*Non-nil means perform cursor motion in terms of screen lines."
  :set (lambda (symbol value)
 	 (sl-screen-line-mode (or value 0)))
  :initialize 'custom-initialize-default
  :group 'sl-screenline
  :type 'boolean
  :require 'screenline)

(defcustom sl-screen-line-mode-name " ScrLn"
  "*Screen line mode line indicator.
Set this to nil to conserve mode line space."
  :group 'sl-screenline
  :type 'string)

(eval-when-compile
  (defvar goal-column)
  (defvar last-command)
  (defvar minor-mode-alist)
  (defvar minor-mode-map-alist)
  (defvar next-line-add-newlines)
  (defvar temporary-goal-column)
  (defvar track-eol)
  (defvar zmacs-region-stays))   ; maintains region in XEmacs

;;;###autoload
(defun sl-screen-line-mode (arg)
  "Toggle screen line mode.
With arg, turn screen line mode on iff arg is positive.

When Screen Line mode is enabled, cursor motion is based on 
screen lines, independent of line-wrapping.

Screen line mode rebinds the keys C-n, C-p, C-k, C-a, C-e, up, down to
`sl-next-screen-line', `sl-previous-screen-line',
`sl-kill-screen-line', `sl-beginning-of-screen-line',
`sl-end-of-screen-line' and again `sl-previous-screen-line', and
`sl-next-screen-line' respectively.  Rebinding is buffer local."
  (interactive "P")
  (set (make-variable-buffer-local 'sl-screen-line-mode)
	(if (null arg)
	    (not sl-screen-line-mode)
	  (> (prefix-numeric-value arg) 0)))
    (if (interactive-p)
      (if sl-screen-line-mode
	  (message "screen line mode enabled")
	(message "screen line mode disabled")))
    (or (assq 'sl-screen-line-mode minor-mode-alist)
	(progn (setq minor-mode-alist
		     (cons '(sl-screen-line-mode sl-screen-line-mode-name)
			   minor-mode-alist))
	       (let ((keymap (make-sparse-keymap)))
		 (define-key keymap [(control k)] 'sl-kill-screen-line)
		 (define-key keymap [(control p)] 'sl-previous-screen-line)
		 (define-key keymap [(control n)] 'sl-next-screen-line)
		 (define-key keymap [(control a)] 'sl-beginning-of-screen-line)
		 (define-key keymap [(control e)] 'sl-end-of-screen-line)
		 (define-key keymap [(up)] 'sl-previous-screen-line)
		 (define-key keymap [(down)] 'sl-next-screen-line)
		 (add-to-list 'minor-mode-map-alist 
			      (cons sl-screen-line-mode keymap)))
	       (force-mode-line-update))))

;;; from bmonkey's screen-lines.el
;; Turn on/off automatically for specific buffers and major modes.
(defvar sl-screen-line-mode-auto-alist
  '((dired-mode-hook . nil)             ; not needed for these modes
    (tar-mode-hook . nil)
    (minibuffer-setup-hook . nil))
  "Alist of hook variable and the value of screen-line-mode for it.")
(mapcar #'(lambda (assoc)
            (add-hook (car assoc)
                      `(lambda () (setq sl-screen-line-mode ,(cdr
assoc)))))
        sl-screen-line-mode-auto-alist)
;;; end bmonkey code

(defun sl-beginning-of-screen-line (&optional arg)
  "Move point to the start of the current screen line.

The cursor is moved to the beginning of the current screen line, even
if the current line is broken across multiple screen lines
(line wrapping). With optional argument ARG not nil, move cursor forward
ARG - 1 screen lines first, similar to `beginning-of-line'."
  (interactive "P")
  (if (boundp 'zmacs-region-stays)
      (setq zmacs-region-stays t))
  (vertical-motion (- (or (prefix-numeric-value arg) 1) 1))
  nil)

(defun sl-end-of-screen-line (&optional arg)
  "Move point to the end of the current screen line. 

The cursor is moved to the end of the current screen line even if the
current line is broken across multiple screen lines
(line wrapping). With optional argument ARG not nil, move cursor forward
ARG - 1 screen lines first, similar to `end-of-line'."
  (interactive "P")
  (if (boundp 'zmacs-region-stays)
      (setq zmacs-region-stays t))
  (vertical-motion (or (prefix-numeric-value arg) 1))
  (backward-char))

(defun sl-next-screen-line (arg)
  "Move point vertically down ARG screen lines of text.  

The cursor is moved down ARG screen lines of text, even if the lines
are broken across multiple screen lines (line wrapping).  If there is
no character in the target column, the cursor is positioned at the end
of the line similar to `next-line'. Adheres to next-line's use of the
variables `goal-column', `track-eol', and `next-line-add-newlines'."
  (interactive "p")
  (if (boundp 'zmacs-region-stays)
      (setq zmacs-region-stays t))
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
	(end-of-line)
	(if (eobp)
	    (newline 1)
	  (goto-char opoint)
	  (sl-line-move arg)))
    (if (interactive-p)
	(condition-case nil
	    (sl-line-move arg)
	  ((beginning-of-buffer end-of-buffer) (ding)))
      (sl-line-move arg)))
 nil)

(defun sl-previous-screen-line (arg)
  "Move point vertically up ARG screen lines of text.

The cursor is moved up ARG screen lines of text, even if the current
line is broken across multiple screen lines (line wrapping).  If there
is no character in the target column, the cursor is positioned at the
end of the line similar to `previous-line'. Adheres to previous-line's
use of the variables `goal-column' and `track-eol'."
  (interactive "p")
  (if (boundp 'zmacs-region-stays)
      (setq zmacs-region-stays t))
  (if (interactive-p)
      (condition-case nil
	  (sl-line-move (- arg))
	((beginning-of-buffer end-of-buffer) (ding)))
    (sl-line-move (- arg)))
  nil)

(defun sl-move-to-screen-column (column)
    (let ((inhibit-point-motion-hooks t)
	(line-beg-mod (save-excursion (sl-beginning-of-screen-line) 
				       (% (current-column) (- (frame-width) 1))))
	(line-end (save-excursion (sl-end-of-screen-line) (point)))
	(column-mod (% column (- (frame-width) 1)))
      	new)
      ;;This is kind of a hack.  The problem is intangible text at the
      ;;end of the previous screen line that's wrapped onto the
      ;;current screen line.  This means that
      ;;sl-beginning-of-screen-line doesn't move onto the first
      ;;visible column, but rather to the end of the intangible text.
      ;;We subtract this amount from column.
      (setq new (save-excursion (sl-beginning-of-screen-line)
		  (move-to-column (+ (current-column) (- column-mod line-beg-mod)))
		  (point)))
      (setq inhibit-point-motion-hooks nil)	     
      (goto-char new)
      ;;This is also a hack.  Here, the problem is intangible text
      ;;that's wrapped onto the next screen line.  If we try to move
      ;;into it, we overshoot and end up on the next screen line.  The
      ;;backward-char command moves back to the beginning of the
      ;;intangible text.  Anybody got any better ideas?
      (if (and (not (eobp)) (> (point) line-end)) 
	  (backward-char))))

; sl-line-move is the line-move function from simple.el, modified
; to be sl-screen-line-mode aware.
(defun sl-line-move (arg)
  ;; Don't run any point-motion hooks, and disregard intangibility,
  ;; for intermediate positions.
  (let ((inhibit-point-motion-hooks t)
	(opoint (point))
	new line-end line-beg)
    (unwind-protect
	(progn
	  (unless (memq last-command '(sl-next-screen-line
sl-previous-screen-line next-line previous-line))
	      (setq temporary-goal-column
		    (if (and track-eol (or (eolp)
					   (eq (% (current-column) 
						      (- (frame-width) 1)) 
					       (- (frame-width) 2)))
			     ;; Don't count beg of empty line as end of line
			     ;; unless we just did explicit end-of-line.
			     (or (not (bolp)) 
				 (memq last-command 
				       '(end-of-line sl-end-of-screen-line))))
			(if sl-screen-line-mode
			    (- (frame-width) 2)
			  9999)
			(current-column))))
	  (if (and (not (integerp selective-display))
		   (not line-move-ignore-invisible))
	      ;; Use just newline characters.
	      (if sl-screen-line-mode
		  (or (eq arg (vertical-motion arg))
		      (signal (if (< arg 0)
				  'beginning-of-buffer
				'end-of-buffer)
			      nil))
	      (or (if (> arg 0)
		      (progn (if (> arg 1) (forward-line (1- arg)))
			     ;; This way of moving forward ARG lines
			     ;; verifies that we have a newline after
			     ;; the last one.  It doesn't get confused
			     ;; by intangible text.
			     (end-of-line)
			     (zerop (forward-line 1)))
		    (and (zerop (forward-line arg))
			 (bolp)))
		  (signal (if (< arg 0)
				  'beginning-of-buffer
				'end-of-buffer)
			      nil)))
	      	    ;; Move by arg lines, but ignore invisible ones.
	    (while (> arg 0)
	      (if (not sl-screen-line-mode)
		  (end-of-line))
	      (and (zerop (vertical-motion 1))
		   (signal 'end-of-buffer nil))
	      ;; If the following character is currently invisible,
	      ;; skip all characters with that same `invisible'
	      ;; property value.
	      (while (and (not (eobp))
			  (let ((prop
				 (get-char-property (point) 'invisible)))
			    (if (eq buffer-invisibility-spec t)
				prop
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))))
		(if (get-text-property (point) 'invisible)
		    (goto-char (next-single-property-change (point) 'invisible))
		  (goto-char (next-overlay-change (point)))))
	      (setq arg (1- arg)))
	    (while (< arg 0)
	      (if (not sl-screen-line-mode)
		  (beginning-of-line))
	      (and (zerop (vertical-motion -1))
		   (signal 'beginning-of-buffer nil))
	      (while (and (not (bobp))
			  (let ((prop
				 (get-char-property (1- (point)) 'invisible)))
			    (if (eq buffer-invisibility-spec t)
				prop
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))))
		(if (get-text-property (1- (point)) 'invisible)
		    (goto-char (previous-single-property-change (point) 'invisible))
		  (goto-char (previous-overlay-change (point)))))
	      (setq arg (1+ arg)))) ; matches (if (and (not intergerp...
	  (let ((buffer-invisibility-spec nil))
	    (if sl-screen-line-mode
		  (sl-move-to-screen-column (or goal-column temporary-goal-column))
	      (move-to-column (or goal-column temporary-goal-column)))));(progn
      ;; begin unwind-form
      (setq new (point))
      ;; If we are moving into some intangible text,
      ;; look for following text on the same line which isn't intangible
      ;; and move there.
      (setq line-end (save-excursion (if sl-screen-line-mode 
					 (sl-end-of-screen-line)
				       (end-of-line)) 
				     (point)))
      (setq line-beg (save-excursion (if sl-screen-line-mode 
					 (sl-beginning-of-screen-line) 
				       (beginning-of-line))
				     (point)))
      (let ((after (and (< new (point-max))
			(get-char-property new 'intangible)))
	    (before (and (> new (point-min))
			 (get-char-property (1- new) 'intangible))))
	(when (and before (eq before after)
		   (not (bolp)))
	  (goto-char (point-min))
	  (let ((inhibit-point-motion-hooks nil))
	    (goto-char new))
	  (if (<= new line-end)
	      (setq new (point)))))
      ;; NEW is where we want to move to.
      ;; LINE-BEG and LINE-END are the beginning and end of the line.
      ;; Move there in just one step, from our starting position,
      ;; with intangibility and point-motion hooks enabled this time.
      (goto-char opoint)
      (setq inhibit-point-motion-hooks nil)
      (goto-char new)
      ;; If intangibility processing moved us to a different line,
      ;; readjust the horizontal position within the line we ended up
at.
      (when (or (< (point) line-beg) (> (point) line-end))
	(setq new (point))
	(setq inhibit-point-motion-hooks t)
	(setq line-end (save-excursion (end-of-line) (point)))
	(if sl-screen-line-mode 
	    (sl-beginning-of-screen-line) 
	  (beginning-of-line))
	(setq line-beg (point))
	(let ((buffer-invisibility-spec nil))
	  (if sl-screen-line-mode
	      (sl-move-to-screen-column (or goal-column temporary-goal-column))
	    (move-to-column (or goal-column temporary-goal-column))))
	(if (<= (point) line-end)
	    (setq new (point)))
	(goto-char (point-min))
	(setq inhibit-point-motion-hooks nil)
	(goto-char new))
      ) ; matches (unwind-protect
    ) ; matches (let ((inhibit-point-motion-hooks t)...
  nil)

(defun sl-kill-screen-line (&optional arg)
  "Kill the rest of the current screen line.

Killing is independent of line wrapping; i.e. kills to where line is
wrapped or to end of line if not wrapped.  With prefix argument, kill
that many screen lines from point.  Negative arguments kill screen
lines backward.  Adheres to `kill-line's use of `kill-whole-line'."
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (vertical-motion (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (vertical-motion 1)
		     (sl-end-of-screen-line)))
 		  (point))))

(provide 'screenline)

(if sl-screen-line-mode
    (sl-screen-line-mode t))

;; screenline.el ends here
