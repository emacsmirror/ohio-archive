;From ark1!uakari!uflorida!novavax!hcx1!tom Mon Feb 26 14:51:25 1990
;Article 1489 of comp.emacs:
;Path: ark1!uakari!uflorida!novavax!hcx1!tom
;>From tom@ssd.csd.harris.com (Tom Horsley)
;Newsgroups: comp.emacs
;Subject: Additional X mouse functions (was Scrolling emacs windows under X...)
;Message-ID: <TOM.90Feb23070642@hcx2.ssd.csd.harris.com>
;Date: 23 Feb 90 12:06:42 GMT
;References: <52442@bbn.COM>
;Sender: news@hcx1.SSD.CSD.HARRIS.COM
;Organization: Harris Computer Systems Division
;Lines: 717
;In-reply-to: AMANDEL%BRUSP.ANSP.BR@UICVM.UIC.EDU's message of 21 Feb 90 12:29:38 GMT
;
;I have been thinking about posting this since I have not made any changes
;for a while and it seems to be pretty useful. Now that someone has asked
;about scrolling windows using a mouse, this is a good time. Note that the
;documentation refers to several other .el files. I don't believe that any of
;these are required to make this package work, they are merely other packages
;I have which use the mouse.
;
;The one package that *is* required is nuscroll.el, this is the scroll up and
;down in place code that was written by Joe Wells and posted quite some time
;ago. If you don't have it, or don't like it, all you have to do is modify
;the defuns for the x-mouse scrolling commands here to use the ordinary
;scroll functions rather than scroll up or down in place.

; mighty-mouse.el:
;
; Additional mouse functions for gnuemacs under X
; Revision 2.4 Jan 5, 1990
;
; written by Tom Horsley (tahorsley@ssd.csd.harris.com)
;
; Additional functions by Bill Leonard - Jan 2, 1990
;
; source is in ~tom/gnuemacs/mighty-mouse.el
;
; Several routines are modifications of the distributed code in x-mouse.el
;
; Functions for positioning and scrolling point and windows.
;
; x-mouse-set-window-or-point
;   A replacement for x-mouse-set-point, this function will simply switch
;   windows if the mouse is in a different window, but not change point. To
;   change window and point, click twice.
;
; x-mouse-set-window-and-point
;   A replacement for the standard x-mouse-set-point routine which does not
;   work correctly in horizontally scrolled windows.
;
; x-mouse-remember
;   Bind this to button down on a key that you will bind something else to
;   button up on.
;
; x-mouse-drag-window
;   Bind this to button up on the same button that x-mouse-remember is bound
;   to button down on. The net effect is this. You push the button down
;   while sitting on a line in the window, move the mouse, then release the
;   button and the text in the window moves so the line you started on
;   scrolls to where the mouse is now. If you start the mouse on a mode line
;   or a boundary between vertically split windows it changes the size of
;   the windows rather than moving the text.
;
; x-mouse-scroll-down-in-place
;   Scrolls the window where the mouse is, using the scroll-down-in-place
;   function.
;
; x-mouse-scroll-up-in-place
;   Guess!
;
; x-mouse-copy-region
;   Copy region between mark and mouse.
;
; x-mouse-cut-region
;   Cut region between mark and mouse.
;
; x-mouse-insert-space
;   Insert a rectangle of space starting at the point the button was pushed
;   down and ending at the point the button was released (bind
;   x-mouse-remember to button down and x-mouse-insert-space to button up).
; 
; Font changing functions - you may want to set some variables before
; loading this file and picking up the defaults:
;   x-mouse-font-list - list of font names in ascending font size
;   x-default-font-in-list - index of the default font in this list
;
; x-mouse-set-default-font
;   Set font to a specified default value.
;
; x-mouse-set-smaller-font
;   Set font to next smaller in a list of fonts
;
; x-mouse-set-bigger-font
;   Set font to next bigger in a list of fonts
;
; Local/global mouse map support:
;
; x-split-mouse-map
;   Split the mouse map into local and global mouse maps. After calling this
;   all define-key calls should be done with global-mouse-map.  If mouse-map
;   is modified, all hell will break loose.
;
; x-use-local-mouse-map
;   Setup a local mouse map, then pass it to this function to be used as the
;   local mouse map in the current buffer (typically this call is made in a
;   mode hook). Mouse buttons left un-defined will default to the
;   global-mouse-map function.
;
; Other functions:
;
; x-mouse-kill-buffer
;   Kill the buffer the mouse is pointing at. (Best to bind this to something
;   complicated like a Control Shift Meta combination.)
;
; x-mouse-kill-window
;   Kill the buffer the mouse is pointing at, and delete that window.
;
; The sample bindings I use are:
;
;   (define-key global-mouse-map x-button-left 'x-mouse-set-window-or-point)
;   (define-key global-mouse-map x-button-m-middle 'x-mouse-remember)
;   (define-key global-mouse-map x-button-m-middle-up 'x-mouse-drag-window)
;   (define-key global-mouse-map x-button-m-right 'x-mouse-scroll-down-in-place)
;   (define-key global-mouse-map x-button-m-left 'x-mouse-scroll-up-in-place)
;   (define-key global-mouse-map x-button-c-s-left 'x-mouse-set-smaller-font)
;   (define-key global-mouse-map x-button-c-s-middle 'x-mouse-set-default-font)
;   (define-key global-mouse-map x-button-c-s-right 'x-mouse-set-bigger-font)
;   (define-key global-mouse-map x-button-c-m-s-right 'x-mouse-kill-buffer)
;   (define-key global-mouse-map x-button-c-right 'x-mouse-remember)
;   (define-key global-mouse-map x-button-c-right-up 'x-mouse-insert-space)
;
; Other files of interest:
;
; mouse-help.el
;   Print reasonable description of the mouse map.
;
; buff-setup.el, buff-mouse.el
;   Provide functions for use in binding keys to the mouse in buffer menu mode.
;
; nuscroll.el
;   Contains the scroll-down/up-in-place functions used by the mouse window
;   scroller functions.
;
; info-mouse.el, info-setup.el
;   These files contain functions that make use of the local mouse map to
;   setup an info browser controlled by the mouse.

(provide 'mighty-mouse)

(require 'x-mouse)

(defun test-position-in-window (abspos winedge)
"Test the position ABSPOS against the window edge list WINEDGE.
Return nil if not in window, or ((col row) status) if in window, where status
is 'on-mode-line if on mode line of window, 'on-right-edge if on the right
edge of a vertically split window or 'in-window if inside window somewhere."
   (let
      (
         (abscol (nth 0 abspos))
         (absrow (nth 1 abspos))
         (winleftcol (nth 0 winedge))
         (wintoprow (nth 1 winedge))
         (winrightcol (nth 2 winedge))
         (winbottomrow (nth 3 winedge))
         (status nil)
      )
      (if (and
             (< absrow winbottomrow)
             (>= absrow wintoprow)
             (< abscol winrightcol)
             (>= abscol winleftcol)
          )
         ; If the window only has one line, it cannot have a mode line so
         ; don't check for mode line in that case. (This really only happens
         ; in the minibuffer, but this test should always work.)
         (if (= wintoprow (1- winbottomrow))
            (setq status 'in-window)
         ; else
            (if (= absrow (1- winbottomrow))
               (setq status 'on-mode-line)
            ; else
               ; if the window does not extend all the way to the screen
               ; edge then check for being on the right edge divider.
               (if (and (= abscol (1- winrightcol))
                        (not (= (screen-width) winrightcol))
                   )
                  (setq status 'on-right-edge)
               ; else
                  (setq status 'in-window)
               )
            )
         )
      )
      (if status
         (list (list (- abscol winleftcol) (- absrow wintoprow)) status)
      ; else
         nil
      )
   )
)

(defun get-window-position-config (arg minibuf)
"Return relative window position of absolute screen position.
Checks ARG, a list that looks like (col row) against all the windows and
returns (window (rel-col rel-row) status) giving relative position.  Arg
MINIBUF t means always count the minibuffer window, nil means count
minibuffer only if active, and anything else means never count the
minibuffer."
   (let
      (
         cwin
         (swin (selected-window))
         curstat
      )
      (setq cwin swin)
      (setq curstat (test-position-in-window arg (window-edges cwin)))
      (while (not (or curstat (eq (setq cwin (next-window cwin minibuf)) swin)))
         (setq curstat (test-position-in-window arg (window-edges cwin)))
      )
      (if curstat
         (cons cwin curstat)
      ; else
         nil
      )
   )
)

(defun x-get-mouse-window-and-pos (arg)
"Return a list (window (window-x window-y)) giving the mouse window
and relative position in window."
   (get-window-position-config arg nil)
)

(defun x-mouse-set-window-or-point (arg)
"Select Emacs window mouse is on, if same as current window move point to
mouse location, otherwise just select the window."
   (let*
      (
         (mouse-pos-info (x-get-mouse-window-and-pos arg))
         (mouse-win-info (car mouse-pos-info))
         (cur-win-info (selected-window))
         (relative-coordinate (car (cdr mouse-pos-info)))
	 (rel-x (car relative-coordinate))
	 (rel-y (car (cdr relative-coordinate)))
      )
      (if (eq mouse-win-info cur-win-info)
         (if relative-coordinate
            (progn
               (move-to-window-line rel-y)
               (move-to-column
                  (+ rel-x
                     ; Adding in current-column fixes things for lines
                     ; that wrap.
                     (current-column)
                     ; Adding in windwo-hscroll fixes things for
                     ; horizontally scrolled windows.
                     (if (> (window-hscroll) 0) (1- (window-hscroll)) 0)
                     ; Scrolled and wrapped lines probably don't work
                  )
               )
            )
         )
      ; else
         (if mouse-win-info
            (select-window mouse-win-info)
         )
      )
   )
)

(defun x-mouse-set-window-and-point (arg)
"Select Emacs window mouse is on, and move cursor to mouse position."
   (let*
      (
         (mouse-pos-info (x-get-mouse-window-and-pos arg))
         (mouse-win-info (car mouse-pos-info))
         (cur-win-info (selected-window))
         (relative-coordinate (car (cdr mouse-pos-info)))
	 (rel-x (car relative-coordinate))
	 (rel-y (car (cdr relative-coordinate)))
      )
      (if relative-coordinate
         (progn
            (select-window mouse-win-info)
            (move-to-window-line rel-y)
            (move-to-column
               (+ rel-x
                  ; Adding in current-column fixes things for lines
                  ; that wrap.
                  (current-column)
                  ; Adding in windwo-hscroll fixes things for
                  ; horizontally scrolled windows.
                  (if (> (window-hscroll) 0) (1- (window-hscroll)) 0)
                  ; Scrolled and wrapped lines probably don't work
               )
            )
         )
      )
   )
)

(defvar x-mouse-remember-original-window nil
"Variable recording window where x-mouse-remember happened."
)

(defvar x-mouse-remember-original-x-pos nil
"Variable recording window X position where x-mouse-remember happened."
)

(defvar x-mouse-remember-original-y-pos nil
"Variable recording window Y position where x-mouse-remember happened."
)

(defvar x-mouse-remember-original-status nil
"Variable that remembers original mouse position status."
)

(defun x-mouse-remember (arg)
"This function is designed to be bound to button down events, so another
function (bound to button up) can then do something depending on the
amount the mouse has moved."
   (let
      (
         (mpos (x-get-mouse-window-and-pos arg))
      )
      (setq x-mouse-remember-original-x-pos (car arg))
      (setq x-mouse-remember-original-y-pos (car (cdr arg)))
      (setq x-mouse-remember-original-window (nth 0 mpos))
      (setq x-mouse-remember-original-status (nth 2 mpos))
   )
)

(defun x-mouse-drag-window (arg)
"Scroll x-mouse-remember-original-window by the number of lines
between the current mouse position and x-mouse-remember-original-y-pos.
If the mouse starts on a mode like or vertical boundary, simply change
the window size."
   (let
      (
         (initial-window (selected-window))
         (current-y-pos (nth 1 arg))
         (current-x-pos (nth 0 arg))
         lines
      )
      (cond
         ((eq x-mouse-remember-original-status 'in-window)
            (if (and
                    x-mouse-remember-original-y-pos
                    current-y-pos
                    x-mouse-remember-original-window
                )
               (unwind-protect
                  (progn
                     (setq lines
                        (- x-mouse-remember-original-y-pos current-y-pos)
                     )
                     (select-window x-mouse-remember-original-window)
                     (cond
                        ((> lines 0)
                           (scroll-up lines)
                        )
                        ((< lines 0)
                           (scroll-down (- lines))
                        )
                     )
                  )
                  (select-window initial-window)
               )
            )
         )
         ((eq x-mouse-remember-original-status 'on-mode-line)
            (if (and
                    x-mouse-remember-original-y-pos
                    current-y-pos
                    x-mouse-remember-original-window
                )
               (unwind-protect
                  (progn
                     (setq lines
                        (- current-y-pos x-mouse-remember-original-y-pos)
                     )
                     (select-window x-mouse-remember-original-window)
                     (enlarge-window lines)
                  )
                  (select-window initial-window)
               )
            )
         )
         ((eq x-mouse-remember-original-status 'on-right-edge)
            (if (and
                    x-mouse-remember-original-x-pos
                    current-x-pos
                    x-mouse-remember-original-window
                )
               (unwind-protect
                  (progn
                     (setq lines
                        (- current-x-pos x-mouse-remember-original-x-pos)
                     )
                     (select-window x-mouse-remember-original-window)
                     (enlarge-window lines t)
                  )
                  (select-window initial-window)
               )
            )
         )
      )
   )
)

(defun x-mouse-scroll-down-in-place (arg)
"Run scroll-down-in-place on the window the mouse is in."
   (let
      (
         (mouse-window (car (x-get-mouse-window-and-pos arg)))
         (initial-window (selected-window))
      )
      (if mouse-window
         (unwind-protect
            (progn
               (select-window mouse-window)
               (scroll-down-in-place nil)
            )
            (select-window initial-window)
         )
      )
   )
)

(defun x-mouse-scroll-up-in-place (arg)
"Run scroll-up-in-place on the window the mouse is in."
   (let
      (
         (mouse-window (car (x-get-mouse-window-and-pos arg)))
         (initial-window (selected-window))
      )
      (if mouse-window
         (unwind-protect
            (progn
               (select-window mouse-window)
               (scroll-up-in-place nil)
            )
            (select-window initial-window)
         )
      )
   )
)

(defun x-mouse-copy-region (arg)
"Copy text between mark and mouse position into window system cut buffer.
Save in Emacs kill ring also.  Cursor is displayed at mouse position
for one second."
   (let*
      (
         (mouse-pos-info (x-get-mouse-window-and-pos arg))
         (mouse-window (car mouse-pos-info))
      )
      (if mouse-window
         (unwind-protect
            (progn
               (save-excursion
                  (x-mouse-set-point arg)
                  (x-store-cut-buffer (buffer-substring (mark) (point)))
                  (copy-region-as-kill (mark) (point))
                  (sit-for 1)
               )
            )
         )
      )
   )
)

(defun x-mouse-cut-region (arg)
"Cut text between mark and mouse position into window system cut buffer.
Save in Emacs kill ring also.  Cursor is displayed at mouse position
for one second."
   (let*
      (
         (mouse-pos-info (x-get-mouse-window-and-pos arg))
         (mouse-window (car mouse-pos-info))
      )
      (if mouse-window
         (unwind-protect
            (progn
               (save-excursion
                  (x-mouse-set-point arg)
                  (x-store-cut-buffer (buffer-substring (mark) (point)))
                  (kill-region (mark) (point))
                  (sit-for 1)
               )
            )
         )
      )
   )
)

(defun x-mouse-insert-space (arg)
"Insert a rectangle of space starting at the button down position
and ending at the button up positon."
   (let
      (
         (cur-mouse-col (nth 0 arg))
         (cur-mouse-row (nth 1 arg))
         left-col
         right-col
         bottom-row
         top-row
         mouse-tl-pos
         mouse-br-pos
      )
      (setq left-col (min cur-mouse-col x-mouse-remember-original-x-pos))
      (setq right-col (max cur-mouse-col x-mouse-remember-original-x-pos))
      (setq top-row (min cur-mouse-row x-mouse-remember-original-y-pos))
      (setq bottom-row (max cur-mouse-row x-mouse-remember-original-y-pos))
      (setq mouse-tl-pos
         (x-get-mouse-window-and-pos (list left-col top-row))
      )
      (setq mouse-br-pos
         (x-get-mouse-window-and-pos (list right-col bottom-row))
      )
      (if (equal (nth 0 mouse-tl-pos) (nth 0 mouse-br-pos))
         (progn
            (setq save-window (selected-window))
            (save-excursion
               (unwind-protect
                  (let
                     (
                        (rel-lc (nth 0 (nth 1 mouse-tl-pos)))
                        (rel-rc (nth 0 (nth 1 mouse-br-pos)))
                        (rel-tr (nth 1 (nth 1 mouse-tl-pos)))
                        (rel-br (nth 1 (nth 1 mouse-br-pos)))
                     )
                     (select-window (nth 0 mouse-tl-pos))
                     (while (<= rel-tr rel-br)
                        (move-to-window-line rel-tr)
                        (move-to-column (+ rel-lc (current-column)))
                        (if (= (current-column) rel-lc)
                           (indent-to-column rel-rc)
                        )
                        (setq rel-tr (1+ rel-tr))
                     )
                  )
                  (select-window save-window)
               )
            )
         )
      )
   )
)

(defvar x-mouse-font-list
   (list
      "-adobe-courier-medium-o-normal--8-80-75-75-m-50-iso8859-1"
      "-adobe-courier-bold-o-normal--10-100-75-75-m-60-iso8859-1"
      "-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1"
      "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1"
      "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1"
      "-adobe-courier-medium-r-normal--24-240-75-75-m-150-iso8859-1"
   )
"*List of fonts to cycle through, should be arranged smallest to largest."
)

(defvar x-default-font-in-list 4
"*Index in x-mouse-font-list of default font to use."
)

(defvar x-current-font-in-list x-default-font-in-list)

(defun x-mouse-set-default-font (arg)
"Set the font to the normal default."
   (x-set-font (nth x-default-font-in-list x-mouse-font-list))
   (setq x-current-font-in-list x-default-font-in-list)
)

(defun x-mouse-set-smaller-font (arg)
"Pick a smaller font from list of defaults."
   (if (equal x-current-font-in-list 0)
      (error "The font is as small as it gets!")
      (setq x-current-font-in-list (1- x-current-font-in-list))
      (x-set-font (nth x-current-font-in-list x-mouse-font-list))
   )
)

(defun x-mouse-set-bigger-font (arg)
"Pick a biger font from the list of defaults."
   (if (equal x-current-font-in-list (1- (length x-mouse-font-list)))
      (error "The font is as big as it gets!")
      (setq x-current-font-in-list (1+ x-current-font-in-list))
      (x-set-font (nth x-current-font-in-list x-mouse-font-list))
   )
)

(defun x-mouse-kill-buffer (arg)
"Kill the buffer the mouse is pointing at."
   (let
      (
         (mouse-window (car (x-get-mouse-window-and-pos arg)))
      )
      (if mouse-window
         (kill-buffer (window-buffer mouse-window))
      )
   )
)

(defun x-mouse-kill-window (arg)
"Kill the buffer the mouse is pointing at and delete that window (unless it
is the only window)."
   (let*
      (
         (mouse-window (car (x-get-mouse-window-and-pos arg)))
         (mouse-buffer (window-buffer mouse-window))
      )
      (if mouse-window
         (progn
            (if (not (one-window-p t))
               (delete-window mouse-window)
            )
            (kill-buffer mouse-buffer)
         )
      )
   )
)

; The following functions provide support for separate local and global
; mouse maps. Run x-split-mouse-maps to activate the functionality.

(defvar x-mouse-maps-already-split nil
"Used so x-split-mouse-maps will run only once"
)
(defvar local-mouse-map (make-keymap)
"Keymap used once the mouse maps are split to hold local mouse bindings."
)
(make-variable-buffer-local 'local-mouse-map)

;; The x-mouse-route-click function is used to route mouse clicks to the
;; appropriate local/global map depending on which one is non-nil.
;;
;; NOTE: Possibly the route function should determine which window the mouse
;; is in, and look at its local-mouse-map, but currently it does not do
;; that. It should not be too hard to do, I am just not sure if it is really
;; what I want.

(defun x-mouse-route-click (arg)
"Invoke function in local-mouse-map if that is defined, else invoke function
in global-mouse-map. The local-mouse-map should be a buffer local variable
set when the mode is set."
   (let
      (
         (last-click x-mouse-item)
         (mouse-click-function nil)
      )
      (if (and (boundp 'local-mouse-map) (vectorp local-mouse-map))
         (setq mouse-click-function (aref local-mouse-map last-click))
      )
      (if (and (not mouse-click-function)
               (boundp 'global-mouse-map)
               (vectorp global-mouse-map))
         (setq mouse-click-function (aref global-mouse-map last-click))
      )
      (if mouse-click-function
         (if (eq mouse-click-function 'x-mouse-route-click)
            (error "Recursive mouse function loop!")
            (funcall mouse-click-function arg)
         )
         (error "")
      )
   )
)

(defun x-split-mouse-maps ()
"x-split-mouse-maps provides support for separate local and global mouse
maps.  It works by copying the current mouse-map bindings to
global-mouse-map and making local-mouse-map a buffer local variable. It then
replaces mouse-map with a vector in which each entry simply invokes
x-mouse-route-click to route the last mouse action to the local-mouse-map,
or if it is nil to the global-mouse-map. Once this function is called
you MUST NOT modify mouse-map, do your define key calls on global-mouse-map."
   (interactive)
   (if x-mouse-maps-already-split
      nil ; ignore multiple calls
   ; else first call
      (setq x-mouse-maps-already-split t)
      (setq global-mouse-map (copy-keymap mouse-map))
      (let
         (
            (key 0)
         )
         (while (< key 128)
            (define-key mouse-map (char-to-string key) 'x-mouse-route-click)
            (setq key (1+ key))
         )
      )
   )
)

(defun x-use-local-mouse-map (map)
"Used to define values for local mouse map. Sets the local-mouse-map to
the input keymap. If the input keymap is a sparse map, transforms it into
a vector which the routing function expects."
   (let
      (
         new-map
         one-key
         keys
      )
      ; first make sure we have split the mouse maps
      (x-split-mouse-maps)
      ; then define the local map
      (if (and (not (vectorp map)) (keymapp map))
         (progn
            ; This must be a sparse map, go through it and vectorize it
            (setq new-map (make-vector 128 nil))
            (setq keys (cdr map))
            (while keys
               (setq one-key (car keys))
               (setq keys (cdr keys))
               (aset new-map (car one-key) (cdr one-key))
            )
         )
         (setq new-map map)
      )
      (setq local-mouse-map (copy-keymap new-map))
   )
)
;--
;=====================================================================
;domain: tahorsley@ssd.csd.harris.com  USMail: Tom Horsley
;  uucp: ...!novavax!hcx1!tahorsley            511 Kingbird Circle
;      or  ...!uunet!hcx1!tahorsley            Delray Beach, FL  33444
;======================== Aging: Just say no! ========================
