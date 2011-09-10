;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Fri Jan  6 12:07:49 1989
;;;; Subroutines for Mouse operations under X windows, based on the file
;;;; "sun-fns.el" from the standard distribution.

(require 'x-mouse)
(require 'utilities)			; For indicate-region
(provide 'x-fns)

(defun x-mouse-select-item (arg header items)
  "Pop up an X Menu at ARG with HEADER and ITEMS, return selected ITEM or nil.
ARG is a list (x-pos y-pos).
HEADER is a string.
If ITEMS is a list of strings or symbols, the selected string or
symbol is returned. If it is a list of lists in which case the car of
the list (which must be a string) is displayed as the selection item
and the cdr of the list is returned when it is selected.
See also x-mouse-query which is a more robust version of this." 
  (x-popup-menu
   arg
   (list
    "Selection Menu"
    (cons header
	  (if (consp (car items))
	      items
	    (mapcar (function (lambda (x) (cons (format "%s" x) x))) items))))))

(defun x-mouse-move-point (arg)
  "Move point to X mouse cursor."
  (select-window x-mouse-window)
  (move-to-loc (car arg) (car (cdr arg)))
  (if (memq last-command		; support the mouse-copy/delete/yank
	    '(mouse-copy mouse-delete mouse-yank-move))
      (setq this-command 'x-mouse-yank-move)))

(defun x-mouse-set-mark (arg)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (eval-in-window x-mouse-window	; use this to get the unwind protect
    (let ((point (point)))
      (move-to-loc (car arg) (car (cdr arg)))
      (set-mark (point))
      (goto-char point)
      (indicate-region))))

(defun x-mouse-set-mark-and-stuff (arg)
  "Set mark at mouse cursor, and put region in window system cut buffer."
  (x-mouse-set-mark arg)
  (x-store-cut-buffer (buffer-substring (region-beginning) (region-end))))

;;;; Simple mouse dragging stuff: marking with button up

(defvar *mouse-drag-window* nil
  "The window the last mouse drag action started in, bound on mouse down.")
(defvar *mouse-drag-coordinates* nil
  "The (x y) start location of the last mouse drag action, bound on mouse down.")

(defun x-mouse-drag-move-point (arg)
  "Move point to mouse cursor, and allow dragging."
  (x-mouse-move-point arg)
  (setq *mouse-drag-window* x-mouse-window 
 	*mouse-drag-coordinates* arg))

(defun x-mouse-drag-set-mark-stuff (arg)
  "The up click handler that goes with mouse-drag-move-point.
 If mouse is in same window but at different x or y than when
 mouse-drag-move-point was last executed, set the mark at mouse
 and put the region in the window system cut buffer."
  (if (and (eq *mouse-drag-window* x-mouse-window)
	   (not (equal *mouse-drag-coordinates* arg)))
      (x-mouse-set-mark-and-stuff arg)
    (setq this-command last-command)))	; this was just an upclick no-op.
 
(defun x-mouse-select-or-drag-move-point (arg)
  "Select window if not selected, otherwise do mouse-drag-move-point."
  (if (eq (selected-window) x-mouse-window)
      (x-mouse-drag-move-point arg)
    (select-window x-mouse-window)))
 
(defun x-mouse-exch-pt-and-mark (arg)
  "Exchange point and mark."
  (select-window x-mouse-window)
  (exchange-point-and-mark))

(defun x-mouse-call-kbd-macro (arg)
  "Invokes last keyboard macro at mouse cursor."
  (x-mouse-move-point arg)
  (call-last-kbd-macro))

(defun x-mouse-mark-thing (arg)
  "Set point and mark to text object using syntax table.
 The resulting region is put in the window system cut buffer.
 Left or right Paren syntax marks an s-expression.  
 Clicking at the end of a line marks the line including a trailing newline.  
 If it doesn't recognize one of these it marks the character at point."
  (x-mouse-move-point arg)
  (if (eobp) (open-line 1))
  (let* ((char (char-after (point)))
	 (syntax (char-syntax char)))
    (cond
     ((eq syntax ?w)			; word.
      (forward-word 1)
      (set-mark (point))
      (forward-word -1))
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
  (x-store-cut-buffer (buffer-substring (region-beginning) (region-end))))
 
(defun x-mouse-kill-thing (arg)
  "Kill thing at mouse, and put point there."
  (x-mouse-mark-thing arg)
  (kill-region-and-unmark (region-beginning) (region-end)))

(defun x-mouse-kill-thing-there (arg)
  "Kill thing at mouse, leave point where it was.
See x-mouse-mark-thing for a description of the objects recognized."
  (eval-in-window x-mouse-window
    (save-excursion
      (x-mouse-mark-thing arg)
      (kill-region (region-beginning) (region-end)))))
 
(defun x-mouse-save-thing (arg &optional quiet)
  "Put thing at mouse in kill ring.
See x-mouse-mark-thing for a description of the objects recognized."
  (x-mouse-mark-thing arg)
  (copy-region-as-kill (region-beginning) (region-end))
  (if (not quiet) (message "Thing saved")))

(defun x-mouse-save-thing-there (arg &optional quiet)
  "Put thing at mouse in kill ring, leave point as is.
See x-mouse-mark-thing for a description of the objects recognized."
  (eval-in-window x-mouse-window
    (save-excursion
      (x-mouse-save-thing arg quiet))))

(defun x-mouse-copy-thing (arg)
  "Put thing at mouse in kill ring, yank to point.
See x-mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)		; Avoids appending to previous kills.
  (x-mouse-save-thing-there arg t)
  (yank)
  (setq this-command 'yank))
 
(defun x-mouse-move-thing (arg)
  "Kill thing at mouse, yank it to point.
See mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)		; Avoids appending to previous kills.
  (x-mouse-kill-thing-there arg)
  (yank)
  (setq this-command 'yank))
 
(defun x-mouse-yank-at-point (&optional arg)
  "Yank from kill-ring at point; then cycle thru kill ring."
  (if (eq last-command 'yank)
      (let ((before (< (point) (mark))))
 	(delete-region (point) (mark))
 	(rotate-yank-pointer 1)
 	(insert (car kill-ring-yank-pointer))
 	(if before (exchange-point-and-mark)))
    (yank))
  (setq this-command 'yank))
 
(defun x-mouse-yank-at-mouse (arg)
  "Yank from kill-ring at mouse; then cycle thru kill ring."
  (x-mouse-move-point arg)
  (x-mouse-yank-at-point arg))
  
(defun x-mouse-save/delete/yank (&optional arg)
  "Context sensitive save/delete/yank.
Consecutive clicks perform as follows:
     * first click saves region to kill ring,
     * second click kills region,
     * third click yanks from kill ring,
     * subsequent clicks cycle thru kill ring.
If x-mouse-set-point is performed after the first or second click,
the next click will do a yank, etc.  
Except for a possible x-mouse-set-point, this command is insensitive
to mouse location."  
  (cond
   ((memq last-command '(x-mouse-delete yank x-mouse-yank-move)) ; third+ click
    (x-mouse-yank-at-point))
   ((eq last-command 'x-mouse-copy)	; second click
    (kill-region (region-beginning) (region-end))
    (setq this-command 'x-mouse-delete))
   (t					; first click
    (copy-region-as-kill (region-beginning) (region-end))
    (message "Region saved")
    (setq this-command 'x-mouse-copy))))

(defun x-mouse-save/delete/yank-no-op (arg)
  "Percolate last-command through a mouse-event."
  (setq this-command last-command))

(defun x-mouse-split-horizontally (arg)
  "Splits the window horizontally at mouse cursor."
  (eval-in-window
      x-mouse-window
    (split-window-horizontally (1+ (car arg)))))
 
(defun x-mouse-split-vertically (arg)
  "Split the window vertically at the mouse cursor."
  (eval-in-window
      x-mouse-window
    (split-window-vertically (1+ (car (cdr arg))))))
 
(fset 'x-mouse-delete-other-windows 'x-mouse-keep-one-window)
 
(defun x-mouse-delete-window (arg)
  "Deletes the window mouse is in."
  (delete-window x-mouse-window))
 
(defun x-mouse-select-emacs-buffer (arg &optional buffers header)
  "Pop up an X menu at position ARG of BUFFERS (defaults to (buffer-list)).
If optional 3rd arg HEADER is non-nil use that instead of 
\"Select a buffer\" as the namestripe of the menu to be popped up.
Return selected buffer or nil."   
  (x-mouse-select-item 
   arg (or header "Select a buffer")
   (let ((buffers (or buffers (buffer-list)))
	 buffer-a-list)
     (while buffers
       (let ((elt (car buffers)))
	 (if (not (string-match "^ " (buffer-name elt)))
	     (setq buffer-a-list 
		   (cons (cons (format "%14s   %s"
				       (buffer-name elt)
				       (or (buffer-file-name elt) ""))
			       elt)
			 buffer-a-list))))
       (setq buffers (cdr buffers)))
     (reverse buffer-a-list))))

(defun x-mouse-switch-to-buffer (arg)
  "Switch to a buffer selected via an X menu."
  (eval-in-window
      x-mouse-window
    (switch-to-buffer 
     (or (x-mouse-select-emacs-buffer x-mouse-pos nil "Switch to buffer:")
	 (current-buffer)))))

(defun x-mouse-switch-to-buffer-other-window (arg)
  "Switch to a buffer selected via an X menu."
  (eval-in-window
      x-mouse-window
    (switch-to-buffer-other-window 
     (or (x-mouse-select-emacs-buffer
	  x-mouse-pos nil "Switch to buffer other window:")
	 (current-buffer)))))

(defvar *mouse-resizing* nil
  "Non-nil if we are in the middle of a window resize.")

(defun x-mouse-resize-window-mouse-down (arg)
  "Shrink/enlarge window by dragging the modeline.
This function is bound to the desired mouse-down event."
  (setq *mouse-drag-window* x-mouse-window
	*mouse-drag-coordinates* arg
	*mouse-resizing* t))

(defun x-mouse-resize-window-mouse-up (arg)
  "Shrink/enlarge window by dragging the modeline.
This function must bound to the desired mouse-up event in ALL mousemaps."
  (eval-in-window x-mouse-window  
    (if *mouse-resizing*
	(let ((old-y (car (cdr *mouse-drag-coordinates*)))
	      (new-y (car (cdr arg)))
	      (old-y-top (car (cdr (window-edges *mouse-drag-window*))))
	      (new-y-top (car (cdr (window-edges x-mouse-window)))))
	  (select-window *mouse-drag-window*)
	  (if (> new-y old-y)
	      (enlarge-window (- (+ new-y new-y-top) (+ old-y old-y-top)))
	    (shrink-window (- (+ old-y old-y-top) (+ new-y new-y-top))))
	  (setq *mouse-resizing* nil)))))

(defun x-mouse-undo (arg)
  "Invokes undo in the window mouse is in."
  (eval-in-window x-mouse-window (undo)))

;;; The move-to-window-line is used below because otherwise
;;; scrolling a non-selected process window with the mouse, after
;;; the process has written text past the bottom of the window,
;;; gives an "End of buffer" error, and then scrolls.  The
;;; move-to-window-line seems to force recomputing where things are.
	
(defun x-mouse-scroll-up (arg)
  "Scroll the window whose modeline the mouse is in up a page."
  (eval-in-window x-mouse-window (move-to-window-line 1) (scroll-up nil)))

(defun x-mouse-scroll-down (arg)
  "Scroll the window whose modeline the mouse is in down a page."
  (eval-in-window x-mouse-window (scroll-down nil)))
 
(defun x-mouse-scroll-proportional (arg)
  "Scrolls the window the mouse is in proportionally,
corresponding to window-relative X divided by window width."
  (let ((x (car arg)))
    (eval-in-window
	x-mouse-window
      (if (>= x (1- (window-width)))
	  ;; When x is maximum (equal to or 1 less than window width),
	  ;; goto end of buffer.  We check for this special case
	  ;; because the calculated goto-char often goes short of the
	  ;; end due to roundoff error, and we often really want to go
	  ;; to the end.
	  (goto-char (point-max))
	(progn
	  (goto-char (+ (point-min)	; For narrowed regions.
			(* x (/ (- (point-max) (point-min))
				(1- (window-width))))))
	  (beginning-of-line)))
      (what-cursor-position))))

(defun x-mouse-line-to-top (arg)
  "Scrolls the line at the mouse cursor up to the top."
  (eval-in-window x-mouse-window (scroll-up (car (cdr arg)))))
 
(defun x-mouse-top-to-line (arg)
  "Scrolls the top line down to the mouse cursor."
  (eval-in-window x-mouse-window (scroll-down (car (cdr arg)))))
 
(defun x-mouse-line-to-bottom (arg)
  "Scrolls the line at the mouse cursor to the bottom."
  (eval-in-window
      x-mouse-window
    (scroll-up (+ (car (cdr arg)) (- 2 (window-height))))))

(defun x-mouse-bottom-to-line (arg)
  "Scrolls the bottom line up to the mouse cursor."
  (eval-in-window
      x-mouse-window
    (scroll-down (+ (car (cdr arg)) (- 2 (window-height))))))

(defun x-mouse-line-to-middle (arg)
  "Scrolls the line at the mouse cursor to the middle."
  (eval-in-window
      x-mouse-window
    (scroll-up (- (car (cdr arg)) -1 (/ (window-height) 2)))))

(defun x-mouse-middle-to-line (arg)
  "Scrolls the line at the middle to the mouse cursor."
  (eval-in-window
      x-mouse-window
    (scroll-up (- (/ (window-height) 2) (car (cdr arg)) 1))))

(defun x-mouse-expand-horizontally (arg)
  (eval-in-window x-mouse-window (enlarge-window 4 t)))
 
(defun x-mouse-expand-vertically (arg)
  (eval-in-window x-mouse-window (enlarge-window 4)))
 
(defun x-mouse-select-previous-buffer (arg)
  "Switch buffer in mouse window to most recently selected buffer."
  (eval-in-window x-mouse-window (switch-to-buffer (other-buffer))))

(defun x-mouse-prev-complex-command (arg)
  "Perform a previous-complex-command from a mouse click."
  (if (eq (current-local-map) repeat-complex-command-map)
      (previous-complex-command 1)
    (error "Not in command history minibuffer.")))

(defun x-mouse-next-complex-command (arg)
  "Perform a next-complex-command from a mouse click."  
  (if (eq (current-local-map) repeat-complex-command-map)
      (next-complex-command 1)
    (error "Not in command history minibuffer.")))

(defun x-mouse-eval-expression (arg)
  "Allow evaluation of an arbitrary Lisp Expression from a mouse click."
  (call-interactively 'eval-expression))

(defun x-mouse-mini-move-point (arg)
  ;; -6 is good for most common cases
  (x-mouse-move-point (list (- (car arg) 6) 0)))

(defun x-mouse-mini-set-mark-and-stuff (arg)
  ;; -6 is good for most common cases
  (x-mouse-set-mark-and-stuff (list (- (car arg) 6) 0)))

;;;           Global Mouse Bindings.
;;;
;;; There is some sense to this mouse binding madness:
;;; left and right scrolls are inverses.
;;; Shift makes an opposite meaning in the scroll bar.
;;; Meta makes the scrollbar functions work in the text region. [See below -- Russell]
;;; middle operates the mark
;;; left operates at point

;;; Meta commands are generally non-destructive,
;;; Shift is a little more dangerous.
;;; Control is for the really complicated ones.

;;; Control-Meta-Shift-right gives help on that region.

;;; Text Region mousemap

;;; The basics: Point, Mark, Menu, Cut:
(global-set-mouse 'text x-button-left 'x-mouse-drag-move-point)
(global-set-mouse 'text x-button-left-up 'x-mouse-drag-set-mark-stuff)
(global-set-mouse 'text x-button-s-left 'x-mouse-exch-pt-and-mark)
(global-set-mouse 'text x-button-middle 'x-mouse-set-mark-and-stuff)
(global-set-mouse 'text x-button-s-right 'x-paste-text)
(global-set-mouse 'text x-button-s-right-up ; See modeline map for explanation.
		  'x-mouse-resize-window-mouse-up)
;;; The Slymoblics multi-command for Save, Kill, Copy, Move:
;;; What is this supposed to do? We don't have a Symbolics. -- Russell
(global-set-mouse 'text x-button-s-middle 'x-mouse-save/delete/yank)
(global-set-mouse 'text x-button-s-middle-up 'x-mouse-save/delete/yank-no-op)
;;; Save, Kill, Copy, Move Things:
;;; Control-left combines with Control middle/right to produce copy/move
(global-set-mouse 'text x-button-c-middle 'x-mouse-save-thing-there)
(global-set-mouse 'text x-button-c-right 'x-mouse-kill-thing-there)
(global-set-mouse 'text x-button-c-left 'x-mouse-yank-at-point)
(global-set-mouse 'text x-button-c-s-left 'x-mouse-copy-thing)
; (global-set-mouse '(text control middle left)	'mouse-copy-thing)
(global-set-mouse 'text x-button-c-s-middle 'x-mouse-move-thing)
; (global-set-mouse '(text control right left)	'mouse-move-thing)
(global-set-mouse 'text x-button-c-s-right 'x-mouse-mark-thing)
; (global-set-mouse '(text control right middle)	'mouse-mark-thing)
;;; The Universal mouse help command (press all keys and right button):
(global-set-mouse 'text x-button-c-m-s-right 'x-mouse-help-region)
;;; Meta in Text Region is like Meta version in scrollbar:
;;; Oh no it's not -- Russell, Not all the time anyway, in this world
;;; we still some key bindings for window manager operations...
;;; For now we'll make do with:
;;; 	Meta-Shift-Left in text-map = mouse line to top of window
;;; 	Meta-Shift-Right in text-map = mouse line to bottom of window.
;;; They're the ones I want the most.
(global-set-mouse 'text x-button-m-s-left 'x-mouse-line-to-top)
(global-set-mouse 'text x-button-m-s-right 'x-mouse-line-to-bottom)
; (global-set-mouse '(text meta shift  left)	'mouse-line-to-bottom)
; (global-set-mouse '(text meta double left)	'mouse-line-to-bottom)
; (global-set-mouse '(text meta         middle)	'mouse-line-to-middle)
; (global-set-mouse '(text meta shift   middle)	'mouse-middle-to-line)
; (global-set-mouse '(text meta double  middle)	'mouse-middle-to-line)
; (global-set-mouse '(text meta control middle)	'mouse-split-vertically)
; (global-set-mouse '(text meta        right)	'mouse-top-to-line)
; (global-set-mouse '(text meta shift  right)	'mouse-bottom-to-line)
; (global-set-mouse '(text meta double right)	'mouse-bottom-to-line)
;;; Miscellaneous:
(global-set-mouse 'text x-button-c-m-left 'x-mouse-call-kbd-macro)
(global-set-mouse 'text x-button-c-m-right 'x-mouse-undo)

;;; Scrollbar mousemap.

(global-set-mouse 'scrollbar x-button-left 'x-mouse-line-to-top)
(global-set-mouse 'scrollbar x-button-s-left 'x-mouse-line-to-bottom)
(global-set-mouse 'scrollbar x-button-middle 'x-mouse-line-to-middle)
(global-set-mouse 'scrollbar x-button-s-middle 'x-mouse-middle-to-line)
(global-set-mouse 'scrollbar x-button-c-middle 'x-mouse-split-vertically)
(global-set-mouse 'scrollbar x-button-right 'x-mouse-top-to-line)
(global-set-mouse 'scrollbar x-button-s-right 'x-mouse-bottom-to-line)
(global-set-mouse 'scrollbar x-button-s-right-up ; See below 
		  'x-mouse-resize-window-mouse-up)
(global-set-mouse 'scrollbar x-button-m-left 'x-mouse-line-to-top)
(global-set-mouse 'scrollbar x-button-m-s-left 'x-mouse-line-to-bottom)
(global-set-mouse 'scrollbar x-button-m-middle 'x-mouse-line-to-middle)
(global-set-mouse 'scrollbar x-button-m-s-middle 'x-mouse-middle-to-line)
(global-set-mouse 'scrollbar x-button-c-m-middle 'x-mouse-split-vertically)
(global-set-mouse 'scrollbar x-button-m-right 'x-mouse-top-to-line)
(global-set-mouse 'scrollbar x-button-m-s-right 'x-mouse-bottom-to-line)
;;; And the help menu:
(global-set-mouse 'scrollbar x-button-c-m-s-right 'x-mouse-help-region)

;;; Modeline mousemap.

(global-set-mouse 'modeline x-button-left 'x-mouse-scroll-up)
(global-set-mouse 'modeline x-button-middle 'x-mouse-scroll-proportional)
(global-set-mouse 'modeline x-button-right 'x-mouse-scroll-down)
;;; Shift-right starts enlarge/shrink, shift-right-up ends it.
(global-set-mouse 'modeline x-button-s-right 'x-mouse-resize-window-mouse-down)
(global-set-mouse 'modeline x-button-s-right-up
		  ;; This needs to go on every shift-right-up event since it is
		  ;; unlikely that the user will drag a window onto another
		  ;; window's modeline.
		  'x-mouse-resize-window-mouse-up) 
;;; Control-left selects this window, Control-right deletes it.
(global-set-mouse 'modeline x-button-c-left 'x-mouse-delete-other-windows)
(global-set-mouse 'modeline x-button-c-middle 'x-mouse-split-horizontally)
(global-set-mouse 'modeline x-button-c-right 'x-mouse-delete-window)
;;; Shift-left lists buffers and switch-to-buffer-other-window's the selection.
(global-set-mouse 'modeline x-button-s-left 'x-mouse-switch-to-buffer-other-window)
;;; Meta-Shift-left lists buffers and switch-to-buffer's the selection.
(global-set-mouse 'modeline x-button-m-s-left 'x-mouse-switch-to-buffer)
;;; And the help menu:
(global-set-mouse 'modeline x-button-c-m-s-right 'x-mouse-help-region)

;;; Minibuffer Mousemap

(global-set-mouse 'minibuffer x-button-left 'x-mouse-prev-complex-command)
(global-set-mouse 'minibuffer x-button-middle 'x-mouse-eval-expression)
(global-set-mouse 'minibuffer x-button-right 'x-mouse-next-complex-command)
(global-set-mouse 'minibuffer x-button-s-right-up ; See above...
		  'x-mouse-resize-window-mouse-up)
(global-set-mouse 'minibuffer x-button-c-m-s-right 'x-mouse-help-region)
