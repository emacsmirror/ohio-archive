;;; xsbm-userfuns.el : bindable functions for x-sb-mouse
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

;;; This file contains the user functions (i.e. functions designed
;;; specifically to be bound to mouse events).

;;;**************************************************************************
;;;  MISC FUNCTIONS

;; For SOME reason which I don't understand, when hyperbole requires
;; x-mouse, it redefines x-mouse-ignore to the old function in x-mouse.el
;; which requires a single argument.  This was goofing me up, so I have
;; made two functions: x-mouse-ignore which must have an argument (to
;; be compatible with the old version) and x--mouse-ignore which may
;; have an argument but doesn't need one.  I'll try to fix this sometime
;; if for no other reason then to get rid of the double dash (which I think
;; is ugly).
(setq x-mouse-ignore
      '((default . x--mouse-ignore)))
(defun x--mouse-ignore (&optional arg)
  "Do nothing."
  ())
(defun x-mouse-ignore (arg)
  "Do nothing."
  ())

(defun x-mouse-set-mark (&optional arg)
  "Select the window that the mouse is on and set the mark at the mouse pos.
The optional argument is to provide compatibility with x-mouse."
  (let ((win ())
        (mpos ())
        (p0 ()))
    (if arg
        (setq win (x-mouse-window arg)
              mpos (x-mouse-point arg))
      (setq win x-mouse-win-u
            mpos x-mouse-point-u))
    (while (not (eq win (selected-window)))
      (other-window 1))
    (push-mark mpos t)))

(defun x-mouse-call-last-kbd-macro ()
  "Moves the point to where the mouse cursor is and calls last keyboard macro.
Uses x-mouse-set-point to set point."
  (x-mouse-set-point)
  (call-last-kbd-macro))

(defun x-mouse-execute-extended-command ()
  "Executes a mouse command by name."
  (setq func (read-minibuffer "Enter command: "))
  (if (symbolp func)
      (funcall (symbol-function func))
    (message "Not a valid mouse function.")))

(defun x-mouse-describe-event (keyseq)
  "Describes what function would be called by a mouse event (both down and up).
Analogous to describe-key-briefly.  Lars Huttar 6/92."
  (interactive "k(Describe what mouse event?)")
               ; This key sequence should be C-c C-m or C-x C-@
  (let ((defn (key-binding keyseq)))
    (if (not (eq defn 'x-flush-mouse-queue)) ;; or maybe symbol-function ...
        (error "Not a mouse event."))
    (x-proc-mouse-event)) ;; handle the down event
  (let ((defn (key-binding (read-key-sequence nil)))
        (tmp x-mouse-describe-only)) ; save old value
    (if (not (eq defn 'x-flush-mouse-queue)) ;; or maybe symbol-function ...
        (error "Not a mouse event."))
    (setq x-mouse-describe-only t)
    (x-proc-mouse-event) ;; handle the up event, but don't actually do it.
    (setq x-mouse-describe-only tmp)))


;;;**************************************************************************
;;;  SCROLLING/MOVEMENT COMMANDS

(defun x-mouse-set-point (&optional arg)
  "This moves the point to where the mouse cursor is.
It also stores the old point as the mark if x-mouse-auto-set-mark is
non-nil unless arg is passed.  If arg is passed, it is using the old
style x-mouse function which did not set the mark.  The optional argument
is to provide compatibility with x-mouse.  Can be used as a drag in which
case the position when you release the mouse is used."
  (let ((win ())
        (pos ())
        (p0  ()))
    (if arg
        (setq win (x-mouse-window arg)
              pos (x-mouse-point arg)
              p0  (point))
      (setq win x-mouse-win-u
            pos x-mouse-point-u
            p0  x-mouse-point-0))
    (select-window win)
    (goto-char pos)
    (if (and (not arg) x-mouse-auto-set-mark (eq x-mouse-type-u 'window))
        (push-mark p0))))

(defun x-mouse-scroll-down ()
  "This scrolls the window down without selecting it."
  (select-window x-mouse-win-u)
  (condition-case nil
      (scroll-down)
    (beginning-of-buffer (message "Beginning of buffer")))
  (select-window x-mouse-win-0))

(defun x-mouse-scroll-up ()
  "This scrolls the window up without selecting it."
  (select-window x-mouse-win-u)
  (condition-case nil
      (scroll-up)
    (end-of-buffer (message "End of buffer")))
  (select-window x-mouse-win-0))

(defun x-mouse-scroll-to-top ()
  "Positions the mouse point at the top of the window.
Does not select the window."
  (x-mouse-set-point)
  (recenter 0)
  (select-window x-mouse-win-0))

(defun x-mouse-scroll-to-center ()
  "Positions the mouse point at the center of the window.
Does not select the window."
  (x-mouse-set-point)
  (recenter (1- (/ (window-height) 2)))
  (select-window x-mouse-win-0))

(defun x-mouse-scroll-to-bottom ()
  "Positions the mouse point at the bottom of the window.
Does not select the window."
  (x-mouse-set-point)
  (recenter (- (window-height) 2))
  (select-window x-mouse-win-0))

(defun x-mouse-scroll-to-proportion ()
  "Goes to a point N% of the way from the top of the buffer.
If the mouse release occurs at a point 4/5 of the way down the window or
border, or 4/5 of the way along the mode line, it will go to the line 80%
of the way through the file BY THE NUMBER OF LINES in the buffer, NOT the
number of characters.  In other words, it will go to line 80 of a 100 line
buffer regardless of how much of the actual text is above or below line 80.
Can be used as a buffer, mode, or window click.  Does NOT select the window."
  (let (nline line hei wid
        (x   (car x-mouse-pos-u))
        (y   (car (cdr x-mouse-pos-u))))
    (x-mouse-select)
    (setq hei (1- (window-height))
          wid (window-width)
          nline (count-lines (point-min) (point-max)))
    (if (x-mouse-mode-p x-mouse-pos-u x-mouse-win-u)
        (setq x (- x (nth 0 (window-edges x-mouse-win-u)))
              line (/ (* x nline) (1- wid)))
      (setq y (- y (nth 1 (window-edges x-mouse-win-u)))
            line (/ (* y nline) (1- hei))))
    (goto-line line)
    (select-window x-mouse-win-0)))

(defun x-mouse-scroll-line ()
  "Scroll line where mouse was pressed to where mouse was released.
If a click is used, scrolls the line with the point to the position
under the mouse point."
  (let (y0 y1 pos)
    (if x-mouse-click
	(setq pos (x-mouse-pos x-mouse-win-u
			       (save-window-excursion
				 (select-window x-mouse-win-u)
				 (point)))
	      y0 (nth 1 pos))
      (setq y0 (nth 1 x-mouse-pos-d)))
    (setq y1 (nth 1 x-mouse-pos-u))
    (select-window x-mouse-win-u)
    (goto-char (window-start))
    (vertical-motion (- y0 y1))
    (set-window-start x-mouse-win-u (point) t)
    (select-window x-mouse-win-0)))


;;;**************************************************************************
;;;  WINDOW FUNCTIONS

(defun x-mouse-select (&optional arg)
  "This selects the window clicked on.
The optional argument is to provide compatibility with x-mouse."
  (let ((coords ())
        (win ()))
    (if arg
        (setq coords (x-mouse-coords arg)
              win    (x-mouse-window arg))
      (setq coords (x-mouse-coords x-mouse-pos-u)
            win    x-mouse-win-u))
    (select-window win)
    coords))

(defun x-mouse-select-and-split (&optional arg)
  "This selects the window under the mouse and splits it.
The optional argument is to provide compatibility with x-mouse."
  (if arg
      (x-mouse-select arg)
    (select-window x-mouse-win-u))
  (split-window-vertically nil))

(defun x-mouse-keep-one-window (&optional arg)
  "Selects the window under the mouse and kills all others.
The optional argument is to provide compatibility with x-mouse."
  (if arg
      (x-mouse-select arg)
    (select-window x-mouse-win-u))
  (delete-other-windows))

(defun x-mouse-split-vertically ()
  "Splits the window vertically placing the mode line here.
This function should either be bound to a border or window click.  It
will split this window (without selecting it) and put the mode line at
this height."
  (let* ((top (nth 1 (window-edges x-mouse-win-u)))
	 (y (car (cdr x-mouse-pos-u))))
  (select-window x-mouse-win-u)
  (split-window-vertically (1+ (- y top)))
  (select-window x-mouse-win-0)))

(defun x-mouse-split-horizontally ()
  "Splits the window horizontally placing the border line here.
This function should either be bound to a mode or window click.  It
will split this window (without selecting it) and put the border line at
this position from the left."
  (let* ((left (nth 0 (window-edges x-mouse-win-u)))
	 (x (car x-mouse-pos-u)))
  (select-window x-mouse-win-u)
  (split-window-horizontally (1+ (- x left)))
  (select-window x-mouse-win-0)))

(defun x-mouse-delete-this-window ()
  "Kills the window under the mouse."
  (if (eq x-mouse-win-u x-mouse-win-0)
      (delete-window)
    (select-window x-mouse-win-u)
    (delete-window)
    (select-window x-mouse-win-0)))


;;;**************************************************************************
;;;  COPYING/CUTTING TEXT

(defun x-cut-text (arg &optional kill)
  "This is just to be compatible with x-mouse.el."
  (x-mouse-copy-text arg kill))

(defun x-mouse-copy-text (&optional arg kill)
  "Copies the appropriate region to the X cut buffer.
The optional arguments are to provide compatibility with x-mouse.  This
function is appropriate either for a drag (arg=nil, x-mouse-click=nil,
region defined by mouse drag), click (arg=nil, x-mouse-click=t, region
defined by the mouse position and the point, both of which MUST be in
the same window), and a call to the traditional x-cut-text function
where arg=(x,y) and the region is defined the same way as a click.
If x-mouse-duplicate-cut is non-nil, the region is also stored in the kill
ring."
  (let ((test t)
        (win ())
        (p0 ())
        (p1 ()))
    (if arg
        (setq win (selected-window)
              test (coordinates-in-window-p arg win)
              p0 (point)
              p1 (x-mouse-point arg))
      (if x-mouse-click
          (setq win (selected-window)
                test (coordinates-in-window-p x-mouse-pos-u win)
                p0 (point)
                p1 x-mouse-point-u)
        (setq win x-mouse-win-u
              p0 x-mouse-point-d
              p1 x-mouse-point-u)))
    (if test
        (save-excursion
          (set-buffer (window-buffer win))
          (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1))
          (x-store-cut-buffer (buffer-substring p0 p1))
          (if kill (delete-region p0 p1)))
      (message "Mouse not in selected window"))))

(defun x-paste-text (arg)
  "This is to be compatible with x-mouse.el."
  (x-mouse-paste-text arg))

(defun x-mouse-paste-text (&optional arg)
  "This moves the point to the cursor position and pastes the X cut buffer.
It also saves the old point as the mark if arg is not present (it does NOT
save it if arg is present because x-mouse didn't save it).  The optional
argument is to provide compatibility with x-mouse."
  (if arg
      (x-mouse-set-point arg)
    (x-mouse-set-point)
    (push-mark x-mouse-point-0))
  (insert (x-get-cut-buffer)))

(defun x-cut-and-wipe-text (arg)
  "This is to be compatible with x-mouse.el."
  (x-mouse-cut-text arg))

(defun x-mouse-cut-text (&optional arg)
  "This cuts the window drag section to the X cut buffer.
The optional argument is to provide compatibility with x-mouse.  If
x-mouse-duplicate-cut is non-nil, the section is also stored in the
emacs kill ring."
  (let ((p0 ())
        (p1 ()))
    (if arg
        (x-cut-text arg t)
      (save-excursion
        (set-buffer (window-buffer x-mouse-win-u))
        (if x-mouse-click
            (setq p0 (point)
                  p1 x-mouse-point-u)
          (setq p0 x-mouse-point-d
                p1 x-mouse-point-u))
        (x-store-cut-buffer (buffer-substring p0 p1))
        (if x-mouse-duplicate-cut
            (kill-region p0 p1)
          (delete-region p0 p1))))))

(defun x-mouse-paste-there ()
  "This pastes the X cut buffer at the point (NOT the mouse position).
It does not select the window."
  (save-window-excursion
    (x-mouse-select)
    (save-excursion
      (insert (x-get-cut-buffer)))))

(defun x-mouse-append-drag ()
  "This appends the drag to the X cut buffer.
If x-mouse-duplicate-cut is non-nil, it is also appended to the kill ring."
  (save-excursion
    (set-buffer (window-buffer x-mouse-win-u))
    (x-store-cut-buffer
     (concat (x-get-cut-buffer)
             (buffer-substring x-mouse-point-d x-mouse-point-u)))
    (if x-mouse-duplicate-cut
        (progn
          (append-next-kill)
          (copy-region-as-kill x-mouse-point-d x-mouse-point-u)))))

(defun x-mouse-copy-text-to-point ()
  "This copies the drag to the point of the current window.
When called as a click, it will copy the text between the mark and the
mouse point."
  (let ((p0 ())
        (p1 ()))
    (insert
     (save-excursion
       (set-buffer (window-buffer x-mouse-win-u))
       (if x-mouse-click
           (setq p0 (mark)
                 p1 x-mouse-point-u)
         (setq p0 x-mouse-point-d
               p1 x-mouse-point-u))
       (buffer-substring p0 p1)))))

(defun x-mouse-cut-text-to-point ()
  "This cuts the drag to the point of the current window.
When called as a click, it will cut the text between the mark and the
mouse point."
  (let ((p0 ())
        (p1 ()))
    (insert
     (save-excursion
       (set-buffer (window-buffer x-mouse-win-u))
       (if x-mouse-click
           (setq p0 (mark)
                 p1 x-mouse-point-u)
         (setq p0 x-mouse-point-d
               p1 x-mouse-point-u))
       (let ((tmp (buffer-substring p0 p1)))
         (delete-region p0 p1)
         tmp)))))

(defun x-mouse-yank-here ()
  "This inserts the kill ring at the mouse point.
It saves the old point as the mark."
  (select-window x-mouse-win-u)
  (goto-char x-mouse-point-u)
  (push-mark x-mouse-point-0)
  (yank))

(defun x-mouse-yank-there ()
  "This inserts the kill ring at the point (NOT mouse position)."
  (yank))

(defun x-mouse-copy-kill-to-x ()
  "This copies the emacs kill buffer to the x kill buffer."
  (x-store-cut-buffer (car kill-ring-yank-pointer)))

(defun x-mouse-copy-bol-to-x ()
  "Copies from the mouse point to the beginning of the line to X cut-buffer."
  (let (p0 p1)
    (x-mouse-select)
    (save-excursion
      (x-mouse-set-point)
      (setq p0 (save-excursion (beginning-of-line) (point))
            p1 x-mouse-point-u)
      (x-store-cut-buffer (buffer-substring p0 p1))
      (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1))
      (select-window x-mouse-win-0))))

(defun x-mouse-copy-line-to-x ()
  "Copies the line under the mouse to the X cut-buffer."
  (let (p0 p1)
    (x-mouse-select)
    (save-excursion
      (x-mouse-set-point)
      (setq p0 (save-excursion (beginning-of-line) (point))
            p1 (save-excursion (end-of-line) (point)))
      (x-store-cut-buffer (buffer-substring p0 p1))
      (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1))
      (select-window x-mouse-win-0))))

(defun x-mouse-copy-eol-to-x ()
  "Copies from the mouse point to the end of the line to X cut-buffer."
  (let (p0 p1)
    (x-mouse-select)
    (save-excursion
      (x-mouse-set-point)
      (setq p0 x-mouse-point-u
            p1 (save-excursion (end-of-line) (point)))
      (x-store-cut-buffer (buffer-substring p0 p1))
      (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1))
      (select-window x-mouse-win-0))))

(defun x-mouse-copy-rect-to-x ()
  "This copies a rectangle to the x-cut-buffer."
  (let (lines tmp j (i 0) (line "") p0 p1)
    (if x-mouse-click
        (setq p0 (point)
              p1 x-mouse-point-u)
      (setq p0 x-mouse-point-d
            p1 x-mouse-point-u))
    (if (< p1 p0)
        (setq tmp p1
              p1 p0
              p0 tmp))
    (setq j (operate-on-rectangle 'extract-rectangle-line p0 p1 nil)
          lines (nreverse lines))
    (while (< i j)
      (setq line (concat line (nth i lines) "\n")
            i (1+ i)))
    (setq line (concat line (nth i lines)))
    (x-store-cut-buffer line)))

(defun x-mouse-cut-rect-to-x ()
  "This kills a rectangle to the x-cut-buffer."
  (let (lines tmp j (i 0) (line "") p0 p1)
    (if x-mouse-click
        (setq p0 (point)
              p1 x-mouse-point-u)
      (setq p0 x-mouse-point-d
            p1 x-mouse-point-u))
    (if (< p1 p0)
        (setq tmp p1
              p1 p0
              p0 tmp))
    (setq j (operate-on-rectangle 'delete-extract-rectangle-line p0 p1 t)
          lines (nreverse lines))
    (while (<= i j)
      (setq line (concat line (nth i lines) "\n")
            i (1+ i)))
    (x-store-cut-buffer line)))

(defun x-mouse-copy-rect-to-000 ()
  "This copies the rectangle to register 0."
  (let (p0 p1)
    (if x-mouse-click
        (setq p0 (point)
              p1 x-mouse-point-u)
      (setq p0 x-mouse-point-d
            p1 x-mouse-point-u))
    (if (< p1 p0)
        (setq tmp p1
              p1 p0
              p0 tmp))
    (copy-rectangle-to-register ?\000 p0 p1 nil)))

(defun x-mouse-cut-rect-to-000 ()
  "This kills the rectangle to register 0."
  (let (p0 p1)
    (if x-mouse-click
        (setq p0 (point)
              p1 x-mouse-point-u)
      (setq p0 x-mouse-point-d
            p1 x-mouse-point-u))
    (if (< p1 p0)
        (setq tmp p1
              p1 p0
              p0 tmp))
    (copy-rectangle-to-register ?\000 p0 p1 t)))

(defun x-mouse-open-rect ()
  "This opens the rectangle."
  (let (p0 p1)
    (if x-mouse-click
        (setq p0 (point)
              p1 x-mouse-point-u)
      (setq p0 x-mouse-point-d
            p1 x-mouse-point-u))
    (if (< p1 p0)
        (setq tmp p1
              p1 p0
              p0 tmp))
    (open-rectangle p0 p1)))

(defun x-mouse-insert-rect-000-here ()
  "This inserts the rectangle stored in register 0 at mouse point."
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point)
      (insert-register ?\000))))

(defun x-mouse-insert-rect-000 ()
  "Moves point here and inserts the rectangle stored in register 0."
  (x-mouse-set-point)
  (insert-register ?\000))

(defun x-mouse-insert-rect-000-there ()
  "Inserts the rectangle stored in register 0 at the point."
  (insert-register ?\000))

(defun x-mouse-copy-thing ()
  "Uses the thing package to copy the thing under the mouse.
The thing is either a word or an s-expression of some kind.
A left or right bracket, parenthese, or brace marks an expression.
An opening double copies everything up to the next set of double quotes.
The end of a line matches the whole line (excluding the newline)."
  (select-window x-mouse-win-u)
  (let* ((boundaries (thing-boundaries x-mouse-point-u))
	 (p0 (car boundaries))
	 (p1 (cdr boundaries)))
    (x-store-cut-buffer (buffer-substring p0 p1))
    (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1)))
  (select-window x-mouse-win-0))

(defun x-mouse-cut-thing ()
  "Uses the thing package to cut the thing under the mouse.
The thing is either a word or sexp.  See x-mouse-copy-thing for a
description of valid things."
  (select-window x-mouse-win-u)
  (let* ((boundaries (thing-boundaries x-mouse-point-u))
	 (p0 (car boundaries))
	 (p1 (cdr boundaries)))
    (x-store-cut-buffer (buffer-substring p0 p1))
    (if x-mouse-duplicate-cut (copy-region-as-kill p0 p1))
    (delete-region p0 p1))
  (select-window x-mouse-win-0))

(defun x-mouse-copy-thing-to-point ()
  "Uses the thing package to copy the thing under the mouse to the point.
The thing is either a word or sexp.  See x-mouse-copy-thing for a
description of valid things."
  (select-window x-mouse-win-u)
  (let* ((boundaries (thing-boundaries x-mouse-point-u))
	 (p0 (car boundaries))
	 (p1 (cdr boundaries))
	 (tmp (buffer-substring p0 p1)))
    (select-window x-mouse-win-0)
    (insert tmp)))

(defun x-mouse-cut-thing-to-point ()
  "Uses the thing package to cut the thing under the mouse to the point.
The thing is either a word or sexp.  See x-mouse-copy-thing for a
description of valid things."
  (select-window x-mouse-win-u)
  (let* ((boundaries (thing-boundaries x-mouse-point-u))
	 (p0 (car boundaries))
	 (p1 (cdr boundaries))
	 (tmp (buffer-substring p0 p1)))
    (delete-region p0 p1)
    (select-window x-mouse-win-0)
    (insert tmp)))


;;;**************************************************************************
;;;  PUPUP MENU COMMANDS

(defun x-buffer-menu (arg)
  "For compatibility with x-mouse.el."
  (x-mouse-buffer-menu arg))

(defun x-mouse-buffer-menu (&optional arg)
  "Pop up a menu of buffers for selection with the mouse.
The optional argument is to provide compatibility with x-mouse."
  (if (fboundp 'x-popup-menu)
      (let* ((pos (if arg arg x-mouse-pos-u))
             (menu ())
             (title "Buffer Menu")
             (menu-label "Select Buffer")
             (bufflist (buffer-list))
             (i 0)
             (len (length bufflist))
             (buff ())
             (buff-name ()))
        (while (< i len)
          (setq buff (nth i bufflist)
                buff-name (buffer-name buff)
                i (1+ i))
          (if (equal (char-to-string (elt buff-name 0)) " ")
              ()
            (setq buff-name (format " %25s   %s " buff-name
                                    (or (buffer-file-name buff) ""))
                  menu (append menu (list (cons buff-name buff))))))
        (setq menu (list title (append (list menu-label) menu)))
        (switch-to-buffer (or (x-popup-menu pos menu)
                              (current-buffer))))
    (buffer-menu nil)))

(defun x-help (arg)
  "For compatibility with x-mouse.el."
  (x-mouse-menu-help arg))

(defun x-mouse-menu-help (&optional arg)
  "Enter a menu-based help system."
  (if (fboundp 'x-popup-menu)
      (let* ((pos (if arg arg x-mouse-pos-u))
             (selection
              (x-popup-menu
               pos
               '("Help" 
                 ("Is there a command that..."
                  ("Command apropos" . command-apropos)
                  ("Apropos" . apropos))
                 ("Key Commands <==> Functions"
                  ("List all keystroke commands" . describe-bindings)
                  ("Describe key briefly" . describe-key-briefly)
                  ("Describe key verbose" . describe-key)
                  ("Describe Lisp function" . describe-function)
                  ("Where is this command" . where-is))
                 ("Manual and tutorial"
                  ("Info system" . info)
                  ("Invoke Emacs tutorial" . help-with-tutorial))
                 ("Odds and ends"
                  ("Last 100 Keystrokes" . view-lossage)
                  ("Describe syntax table" . describe-syntax))
                 ("Modes"
                  ("Describe current major mode" . describe-mode)
                  ("List all keystroke commands" . describe-bindings))
                 ("Administrivia"
                  ("View Emacs news" . view-emacs-news)
                  ("View the GNU Emacs license" . describe-copying)
                  ("Describe distribution" . describe-distribution)
                  ("Describe (non)warranty" . describe-no-warranty))))))
        (and selection (call-interactively selection)))))

(defun x-mouse-help ()
  "Pops up a menu or a *Mouse Help* buffer showing all mouse bindings.
If menus are unavailable, it'll put the text in a *Help* buffer."
  (let* ((menubase
	  (list
	   (list
	    "Keyboard Modifiers"
	    (cons "*     Default function    " ())
	    (cons "-     No keyboard modifier" ())
	    (cons "c     Control key         " ())
	    (cons "m     Meta (ESC) key      " ())
	    (cons "s     Shift key           " ())
	    (cons "" ())
	    (cons "" ())
	    (cons "A drag region is defines as the region between where" ())
	    (cons "the button was pressed and where it was released." ())
	    (cons "" ())
	    (cons "The X cut buffer is where X stores text in.  It is NOT" ())
	    (cons "the same as the kill ring where emacs stores cut and" ())
	    (cons "killed text." ())
	    (cons "" ())
	    (cons "" ())
	    (cons "Note:  Menu commands are only available if emacs was" ())
	    (cons "       compiled with HAVE_X_MENUS defined.  This is" ())
	    (cons "       NOT defined by default so you may have to" ())
	    (cons "       recompile emacs if you wish to use menus." ())
	    (cons "" ())
	    (cons "" ())
	    (cons "" ())
	    (cons "" ())
	    (cons "" ())
	    (cons "" ())
	    (cons "Select this to send this text to *Mouse Help* buffer."
		  "HelpBuffer"))))
	 (text '(lambda (var)
		  ;; Returns the first line of the function documentation
		  ;; for VAR of type "x-mouse-c1-window-click".
		  (x-mouse-get-function-doc var x-mouse-mode-u)))
	 (line '(lambda (func mods but where type)
		  ;; Returns a cons containing the description of function
		  ;; "x-mouse-MODS BUT-WHERE-TYPE" if it is not the
		  ;; same as the default function FUNC.
		  (let* ((but-var (concat "x-mouse-" mods but "-" where "-"
					  type))
			 (but-func (x-mouse-get-function but-var
							 x-mouse-mode-u))
			 spaces txt)
		    (if (eq but-func func) ()
		      (setq spaces (make-string (- 4 (length mods)) ?\ )
			    txt (funcall text but-var))
		      (cons (concat spaces mods " " txt) ())))))
	 (typel '(lambda (but where type)
		   ;; Returns a list of cons cells, one for each button
		   ;; event (of TYPE) occuring in WHERE (ex. "border").
		   ;; The first one is the default function (no keyboard
		   ;; modifiers) and the remaining members are the
		   ;; events that are different then the default one.
		   (let* ((mods '["c" "m" "s" "cm" "cs" "ms" "cms"])
			  (i 0)
			  (but-var (concat "x-mouse-" but "-" where "-" type))
			  (but-func (x-mouse-get-function but-var
							  x-mouse-mode-u))
			  aline list)
		     (while (< i 7)
		       (setq aline (funcall line but-func (aref mods i)
					    but where type)
			     i (1+ i))
		       (if aline
			   (setq list (append list (list aline)))))
		     (append (list (cons
				    (concat "   * " (funcall text but-var))
				    ()))
			     list))))
	 (button '(lambda (but)
		    ;; Returns a list of cons cells, one for each button
		    ;; click event in each of the locations on the screen.
		    (let* ((where '["window" "mode" "border" "inter"
				    "mini"])
			   (i 0)
			   list wh)
		      (while (< i 5)
			(setq wh (aref where i)
			      list (append
				    list
				    (list (cons " " ()))
				    (list (cons (concat
						 (capitalize wh)
						 " Click") ()))
				    (funcall typel but wh "click"))
			      i (1+ i)))
		      (append (list (concat "*** BUTTON " but " ***"))
			      (list (cons " " ()))
			      list))))
	 (write '(lambda (list &optional spaces)
		   ;; Takes a list of lists and returns a string containing
		   ;; all the lists.
		   (let ((i 0) string len)
		     (if (listp list)
			 (if (listp (cdr list))
			     (if (= (setq len (length list)) 1)
				 (setq string
				       (funcall write (nth 0 list) spaces))
			       (if spaces (setq spaces (concat spaces "  "))
				 (setq spaces "  "))
			       (while (< i len)
				 (setq string
				       (concat string
					       (funcall write (nth i list)
							spaces))
				       i (1+ i))))
			   (setq string (concat spaces (nth 0 list))))
		       (setq string (concat spaces list "\n")))
		     string)))
	 (drag '(lambda ()
		  ;; Returns a list describing all drag functions.
		  (let* (list)
		    (setq list (append (list (cons "Button 1" ()))
				       (list (cons " " ()))
				       (funcall typel 1 "window" "drag")
				       (list (cons " " ()))
				       (list (cons "Button 2" ()))
				       (list (cons " " ()))
				       (funcall typel 2 "window" "drag")
				       (list (cons " " ()))
				       (list (cons "Button 3" ()))
				       (list (cons " " ()))
				       (funcall typel 3 "window" "drag")))
		    (append (list (concat "*** Window Drags ***"))
			    list))))
	 menu tobuff)
    (setq a0 menubase a1 text a2 line a3 typel a4 button a5 write a6 drag)
    (if (and x-mouse-help-to-menu (fboundp 'x-popup-menu))
	(progn
	  (setq menu
		(append (list "Mouse Help")
			menubase
			(list (funcall button 1))
			(list (funcall button 2))
			(list (funcall button 3))
			(list (funcall drag)))
		tobuff
		(x-popup-menu x-mouse-pos-u menu))
	  (if (string= tobuff "HelpBuffer")
	      (let ((x-mouse-help-to-menu nil))
		(x-mouse-help))))
      (with-output-to-temp-buffer "*Mouse-Help*"
	(princ "Mouse Help\n\n")
	(setq aaa menubase bbb write)
	(princ (funcall write menubase))
	(princ "\n")
	(princ "\n")
	(princ (funcall write (funcall button 1)))
	(princ "\n")
	(princ (funcall write (funcall button 2)))
	(princ "\n")
	(princ (funcall write (funcall button 3)))
	(princ "\n")
	(princ (funcall write (funcall drag)))))))

(defun x-mouse-files ()
  (let* ((buffer (get-buffer-create "*TEMP*"))
	 (file-name (buffer-file-name (current-buffer)))
	 (dir-name (and file-name (file-name-directory file-name))))
    (if (null dir-name)
	(ding)
      (save-excursion
	(set-buffer buffer)
	;;
	;; Use directory-files?!?
	;;
	(call-process "ls" nil buffer nil "-F" dir-name)
	(prog1
	    (x-mouse-pick-files (string-to-strings (buffer-string)))
	  (erase-buffer)
	  (set-buffer-modified-p nil))))))
(defun x-mouse-pick-files (files)
  (let (filtered-files)
    (while files
      (if (not (string-match x-mouse-file-ignore-regexp (car files)))
	  (setq filtered-files
		(cons (cons (car files) (car files)) filtered-files)))
      (setq files (cdr files)))
    (reverse filtered-files)))
(defun string-to-strings (string &optional delimiter-regexp)
  (or delimiter-regexp
      (setq delimiter-regexp "[^ \n\t]+"))
  (let ((start 0)
	end
	strings)
    (while (setq start (string-match delimiter-regexp string start))
      (setq end (match-end 0))
      (setq strings (cons (substring string start end) strings))
      (setq start end))
    (reverse strings)))
(defun x-mouse-get-file ()
  "Pop up a menu of files in cwd for selection with the mouse.
The optional argument is to provide compatibility with x-mouse."
  (if (fboundp 'x-popup-menu)
      (let* ((files (x-mouse-files))
	     (file (and files
			(x-popup-menu x-mouse-pos-u
				      (list "File Menu"
					    (append (list "Select File")
						    files))))))
	(if file
	    (find-file file)))
    (find-file "")))


