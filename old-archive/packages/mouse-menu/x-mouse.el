;; Mouse support for X window system.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Fri Jan 13 15:13:40 1989

;;;; Created 4 conceptual screen regions, with a separate mouse-maps
;;;; for each. Patterned after "sun-mouse.el", which purports to be
;;;; modelled after the GNU Emacs keymap interface.

;;;; To facilitate determination of what mouse-map to use, based on 
;;;; pointer position, added function x-mouse-window, which returns the 
;;;; window the pointer is in (a modeline considered part of a window), 
;;;; setting the (new) Lisp variable x-mouse-map to be the name of the 
;;;; appropriate map as part of that process. 
;;;; Which map is "the appropriate map" is determined as follows: 
;;;; 	mouse location		map 
;;;;  	minibuffer 		x-mouse-minibuffer-map 
;;;;  	modeline		x-mouse-modeline-map 
;;;;  	text region		x-mouse-text-map 
;;;;  	scrollbar region	x-mouse-scrollbar-map  
;;;; The "text region" is that part of the screen where a buffers 
;;;; contents are displayed. 
;;;; The "scrollbar region" is the N rightmost columns of the text 
;;;; region where N is defined by the value of x-mouse-scrollbar-width. 

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

(provide 'x-mouse)

(defvar x-mouse-map nil
  "The name of the x-mouse-map should be used for the place the mouse
is currently pointing at according to the following algorithm:
	mouse location		map
  	minibuffer 		x-mouse-minibuffer-map
  	modeline		x-mouse-modeline-map
  	text region		x-mouse-text-map
  	scrollbar region	x-mouse-scrollbar-map
The \"text region\" is that part of the screen where a buffer
contents are displayed.
The \"scrollbar region\" is the N rightmost columns of the text
region where N is defined by the value of x-mouse-scrollbar-width.
x-mouse-map will be set (in passing) by x-mouse-window, which returns
the window the mouse is pointing at.
Of course it's an ugly thing to do, but the code to figures out which
window the mouse is in, is virtually the same as that required to work
out what map to use, so it's pointless doing it twice.")

(defvar local-x-mouse-map nil
  "The local x-mouse-map to be used for the place the mouse is currently pointing at.")

(defvar global-x-mouse-map nil
  "The global x-mouse-map to be used for the place the mouse is currently pointing at.")

(defvar x-mouse-text-map nil
  "The mouse map to be used in the text region for the X window system.")

(defvar x-mouse-modeline-map nil
  "The mouse map to be used in modelines for the X window system.")

(defvar x-mouse-minibuffer-map nil
  "The mouse map to be used in the minibuffer for the X window system.")

(defvar x-mouse-scrollbar-map nil
  "The mouse map to be used in the scrollbar region for the X window system.")

(defvar x-mouse-scrollbar-width 5
  "*Number of rightmost columns to be the width of the 'scrollbar' region.")

(defvar x-mouse-window nil
  "The window the last mouse event occured in.")

(defconst x-button-right (char-to-string 0))
(defconst x-button-middle (char-to-string 1))
(defconst x-button-left (char-to-string 2))

(defconst x-button-right-up (char-to-string 4))
(defconst x-button-middle-up (char-to-string 5))
(defconst x-button-left-up (char-to-string 6))

(defconst x-button-s-right (char-to-string 16))
(defconst x-button-s-middle (char-to-string 17))
(defconst x-button-s-left (char-to-string 18))

(defconst x-button-s-right-up (char-to-string 20))
(defconst x-button-s-middle-up (char-to-string 21))
(defconst x-button-s-left-up (char-to-string 22))

(defconst x-button-m-right (char-to-string 32))
(defconst x-button-m-middle (char-to-string 33))
(defconst x-button-m-left (char-to-string 34))

(defconst x-button-m-right-up (char-to-string 36))
(defconst x-button-m-middle-up (char-to-string 37))
(defconst x-button-m-left-up (char-to-string 38))

(defconst x-button-c-right (char-to-string 64))
(defconst x-button-c-middle (char-to-string 65))
(defconst x-button-c-left (char-to-string 66))

(defconst x-button-c-right-up (char-to-string 68))
(defconst x-button-c-middle-up (char-to-string 69))
(defconst x-button-c-left-up (char-to-string 70))

(defconst x-button-m-s-right (char-to-string 48))
(defconst x-button-m-s-middle (char-to-string 49))
(defconst x-button-m-s-left (char-to-string 50))

(defconst x-button-m-s-right-up (char-to-string 52))
(defconst x-button-m-s-middle-up (char-to-string 53))
(defconst x-button-m-s-left-up (char-to-string 54))

(defconst x-button-c-s-right (char-to-string 80))
(defconst x-button-c-s-middle (char-to-string 81))
(defconst x-button-c-s-left (char-to-string 82))

(defconst x-button-c-s-right-up (char-to-string 84))
(defconst x-button-c-s-middle-up (char-to-string 85))
(defconst x-button-c-s-left-up (char-to-string 86))

(defconst x-button-c-m-right (char-to-string 96))
(defconst x-button-c-m-middle (char-to-string 97))
(defconst x-button-c-m-left (char-to-string 98))

(defconst x-button-c-m-right-up (char-to-string 100))
(defconst x-button-c-m-middle-up (char-to-string 101))
(defconst x-button-c-m-left-up (char-to-string 102))

(defconst x-button-c-m-s-right (char-to-string 112))
(defconst x-button-c-m-s-middle (char-to-string 113))
(defconst x-button-c-m-s-left (char-to-string 114))

(defconst x-button-c-m-s-right-up (char-to-string 116))
(defconst x-button-c-m-s-middle-up (char-to-string 117))
(defconst x-button-c-m-s-left-up (char-to-string 118))

(defvar x-button-help-alist
  '((0 . right) 		  (1 . middle)		   	 (2 . left)
    (4 . right-up)		  (5 . middle-up)		 (6 . left-up)
    (16 . shift-right) 	   	  (17 . shift-middle) 	   	 (18 . shift-left)
    (20 . shift-right-up)	  (21 . shift-middle-up)	 (22 . shift-left-up)
    (32 . meta-right) 		  (33 . meta-middle) 		 (34 . meta-left)
    (36 . meta-right-up)	  (37 . meta-middle-up)	         (38 . meta-left-up)
    (48 . meta-shift-right)	  (49 . meta-shift-middle) 	 (50 . meta-shift-left)
    (52 . meta-shift-right-up)	  (53 . meta-shift-middle-up)	 (54 . meta-shift-left-up)
    (64 . control-right)	  (65 . control-middle)	         (66 . control-left)
    (68 . control-right-up)	  (69 . control-middle-up)	 (70 . control-left-up)
    (80 . control-shift-right)	  (81 . control-shift-middle)	 (82 . control-shift-left)
    (84 . control-shift-right-up) (85 . control-shift-middle-up) (86 . control-shift-left-up)
    (96 . meta-control-right) 	  (97 . meta-control-middle) 	 (98 . meta-control-left)
    (100 . meta-control-right-up) (101 . meta-control-middle-up) (102 . meta-control-left-up)
    (112 . meta-control-shift-right)
    (113 . meta-control-shift-middle)
    (114 . meta-control-shift-left)
    (116 . meta-control-shift-right-up)
    (117 . meta-control-shift-middle-up)
    (118 . meta-control-shift-left-up))
  "Alist of the descriptions of what keychord and button combinations
generate which mouse events. If you change any of the x-button-... constants 
you must change the entries in this list to update the online documentation.")

(defvar x-process-mouse-hook nil
  "Hook to run after each mouse event is processed.  Should take two
arguments; the first being a list (XPOS YPOS) corresponding to character
offset from top left of screen and the second being a specifier for the
buttons/keys.

This will normally be set on a per-buffer basis.")

(defmacro eval-in-window (window &rest forms)
  "Switch to WINDOW, evaluate FORMS, return to original window."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect
	   (progn
	     (select-window (, window))
	     (,@ forms))
	 (select-window OriginallySelectedWindow)))))
(put 'eval-in-window 'lisp-indent-hook 1)

(defmacro eval-in-windows (form &optional yesmini)
  "Switches to each window and evaluates FORM.  Optional argument
YESMINI says to include the minibuffer as a window.
This is a macro, and does not evaluate its arguments."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect 
	   (while (progn
		    (, form)
		    (not (eq OriginallySelectedWindow
			     (select-window
			      (next-window nil (, yesmini)))))))
	 (select-window OriginallySelectedWindow)))))
(put 'eval-in-windows 'lisp-indent-hook 0)

(defun move-to-loc (x y)
  "Move cursor to window location X, Y.
Handles wrapped and horizontally scrolled lines correctly."
  (move-to-window-line y)
  ;; window-line-end expects this to return the window column it moved to.
  (let ((cc (current-column))
	(nc (move-to-column
	     (if (zerop (window-hscroll))
		 (+ (current-column)
		    (min (- (window-width) 2)	; To stay on the line.
			 x))
	       (+ (window-hscroll) -1
		  (min (1- (window-width))	; To stay on the line.
		       x))))))
    (- nc cc)))

(defmacro x-mouse-loc-win (loc)
  "Return the window of LOC, a (window x y) 3 tuple."
  (list 'nth 0 loc))

(defmacro x-mouse-loc-x (loc)
  "Return the x coordinate of LOC, a (window x y) 3 tuple."
  (list 'nth 1 loc))

(defmacro x-mouse-loc-y (loc)
  "Return the y coordinate of LOC, a (window x y) 3 tuple."
  (list 'nth 2 loc))

(defun minibuffer-window-p (window)
  "True iff this WINDOW is minibuffer."
  (= (screen-height) (nth 3 (window-edges window))))	; The bottom edge.

(defun x-mouse-window-xy (mousepos)
  ;; Minor mutilation of the sun mouse function which performs the same task.
  "Find window containing MOUSEPOS (screen coordinates X and Y).
Returns list (window x y) where x and y are relative to window."
  (let ((x (car mousepos))
	(y (car (cdr mousepos))))
    (or
     (catch 'found
       (eval-in-windows 
	(let ((we (window-edges (selected-window))))
	  (let ((le (nth 0 we))
		(te (nth 1 we))
		(re (nth 2 we))
		(be (nth 3 we)))
	    (if (= re (screen-width))
		;; include the continuation column with this window
		(setq re (1+ re)))
	    (if (= be (screen-height))
		;; include partial line at bottom of screen with this window
		;; id est, if window is not multple of char size.
		(setq be (1+ be)))
	    (if (and (>= x le) (< x re)
		     (>= y te) (< y be))
		(throw 'found 
		       (list (selected-window) (- x le) (- y te))))))
	t))				; include minibuffer in eval-in-windows
     ;;If x,y from a real mouse click, we shouldn't get here.
     (list nil x y))))

(defun x-mouse-window-region (loc)
  "Parse (window x y) into a region symbol.
Returns one of (text scrollbar modeline minibuffer)"
  (let ((w (x-mouse-loc-win loc))
	(x (x-mouse-loc-x loc))
	(y (x-mouse-loc-y loc)))
    (let ((right (1- (window-width w)))
	  (bottom (1- (window-height w))))
      (cond ((minibuffer-window-p w) 'minibuffer)
	    ((>= y bottom) 'modeline)
	    ((>= x right) 'scrollbar)
	    ;; far right column (window seperator) is always a scrollbar
	    ((and x-mouse-scrollbar-width
		  ;; mouse within scrollbar-width of edge.
		  (>= x (- right x-mouse-scrollbar-width))
		  ;; mouse a few chars past the end of line.
		  (>= x (+ 2 (x-mouse-window-line-end w x y))))
	     'scrollbar)
	    (t 'text)))))

(defun x-mouse-window-line-end (w x y)
  "Return WINDOW column (ignore X) containing end of line Y"
  (eval-in-window w (save-excursion (move-to-loc (screen-width) y))))

(defun x-mouse-lookup (mouse-hit)
  "Return the binding of MOUSE-HIT in local-x-mouse-map (then global-x-mouse-map), or nil."
  (let ((local-mouse-map (symbol-value (intern-soft (format "local-%s" x-mouse-map)))))
    (or (if local-mouse-map
	    (lookup-key local-mouse-map mouse-hit))
	;; global maps are vector keymaps
	(lookup-key (symbol-value (intern-soft (format "global-%s" x-mouse-map))) mouse-hit))))

(defun x-multiple-map-flush-mouse-queue ()
  "Process all queued mouse events using multiple mouse maps."
  (interactive)
  (while (> (x-mouse-events) 0)
    (let ((mouse-event (x-get-mouse-event nil)) )
      (let ((mouse-com-letter (car mouse-event))
	    (mouse-pos (car (cdr mouse-event))))
	(let* ((mouse-loc (x-mouse-window-xy mouse-pos))
	       (mouse-pos (list (x-mouse-loc-x mouse-loc) ; Rebind mouse-pos to 
				(x-mouse-loc-y mouse-loc)))) ; be window-relative.
	  (setq x-mouse-window (x-mouse-loc-win mouse-loc)) ; Remember the mouse window.
	  (setq x-mouse-map (format "x-mouse-%s-map" (x-mouse-window-region mouse-loc)))
	  (if (integerp mouse-com-letter)
	      ;; x-get-mouse-event sometimes screws up and returns N
	      ;; instead of (char-to-string N), so...
	      (setq mouse-com-letter (char-to-string mouse-com-letter)))
	  (let ((mouse-cmd (save-excursion
			     ;; Look up mouse binding in the mouse window buffer's maps.
			     (set-buffer (window-buffer x-mouse-window))
			     (x-mouse-lookup mouse-com-letter))))
	    (if mouse-cmd
		(funcall mouse-cmd mouse-pos)
	      (ding))
	    (and (boundp 'x-process-mouse-hook)
		 (symbol-value 'x-process-mouse-hook)
		 (funcall x-process-mouse-hook x-mouse-pos x-mouse-item))))))))

(define-key global-map "\C-c\C-m" 'x-multiple-map-flush-mouse-queue)
(define-key global-map "\C-x\C-@" 'x-multiple-map-flush-mouse-queue)

(defun local-set-mouse (region hit def)
  "Args REGION, MOUSE-HIT and DEF.
Define mouse-event MOUSE-HIT, in mouse region REGION, as DEF. 
REGION is the quoted name of a mouse region, e.g. 'modeline
MOUSE-HIT is a symbol denoting a mouse-event.
DEF is an x-mouse function taking 1 arg: a list of the mouse (x y) position.
Valid MOUSE-HIT symbols are:
 x-button-BUTTON
 x-button-c-BUTTON
 x-button-m-BUTTON
 x-button-s-BUTTON
 x-button-c-m-BUTTON
 x-button-c-s-BUTTON
 x-button-m-s-BUTTON
 x-button-c-m-s-BUTTON
where BUTTON is one of left, middle or right.
c, m, and s denote the control, meta and shift keys respectively (for chords).
Additionally -up may be appended, indicating that the function is to be
run on an up-click, i.e. when the mouse button is released.

The definition goes in the current buffer's local version of MOUSEMAP,
which is shared with other buffers in the same major mode."
  (funcall 'define-key 
	   (let ((mapname (format "local-x-mouse-%s-map" region)))
	     (or (symbol-value (intern-soft mapname)) ; Create a sparse local map
		 (set (intern mapname) (make-sparse-keymap)) ; if none currently exists.
		 (intern-soft mapname)))
	   hit 
	   def))

(defun global-set-mouse (region hit def)
  "Args REGION, MOUSE-HIT and DEF.
Define mouse-event MOUSE-HIT, in mouse region REGION, as DEF. 
REGION is the quoted name of a mouse region, e.g. 'modeline
MOUSE-HIT is an unquoted symbol denoting a mouse-event.
DEF is the quoted name of an x-mouse function of 1 arg: the mouse (x y) position.
Valid MOUSE-HIT symbols are:
 x-button-BUTTON
 x-button-c-BUTTON
 x-button-m-BUTTON
 x-button-s-BUTTON
 x-button-c-m-BUTTON
 x-button-c-s-BUTTON
 x-button-m-s-BUTTON
 x-button-c-m-s-BUTTON
where BUTTON is one of left, middle or right.
c, m, and s denote the control, meta and shift keys respectively (for chords).
Additionally -up may be appended, indicating that the function is to be
run on an up-click, i.e. when the mouse button is released.

The definition goes in the global version of MOUSEMAP,
which is shared by all buffers by default."
  (funcall 'define-key 
	   ;; Global maps are initialised next, thus they are always interned.
	   (symbol-value (intern-soft (format "global-x-mouse-%s-map" region)))
	   hit 
	   def))

(defun x-mouse-select (arg)
  "Select Emacs window the mouse is on."
  (select-window x-mouse-window))

(defun x-mouse-keep-one-window (arg)
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (delete-other-windows x-mouse-window))

(defun x-mouse-select-and-split (arg)
  "Select Emacs window mouse is on, then split it vertically in half."
  (select-window x-mouse-window)
  (split-window-vertically nil))

(defun x-mouse-set-point (arg)
  "Select Emacs window mouse is on, and move point to mouse position."
  (select-window x-mouse-window)
  (move-to-window-line (car (cdr arg)))
  (move-to-column (+ (car arg) (current-column))))

(defun x-cut-text (arg &optional kill)
  "Copy text between point and mouse position into window system cut buffer.
Save in Emacs kill ring also."
  (if (eq x-mouse-window (selected-window))
      (save-excursion
	(let ((opoint (point))
	      beg end)
	  (x-mouse-set-point arg)
	  (setq beg (min opoint (point))
		end (max opoint (point)))
	  (x-store-cut-buffer (buffer-substring beg end))
	  (copy-region-as-kill beg end)
	  (if kill (delete-region beg end))))
    (message "Mouse not in selected window")))

(defun x-paste-text (arg)
  "Move point to mouse position and insert window system cut buffer contents."
  (x-mouse-set-point arg)
  (insert (x-get-cut-buffer)))

(defun x-cut-and-wipe-text (arg)
  "Kill text between point and mouse; also copy to window system cut buffer."
  (x-cut-text arg t))

(defun x-mouse-ignore (arg)
  "Don't do anything.")

(defun x-mouse-help-region (arg)
  "Describe the mouse bindings in current region."
  (x-mouse-report-bindings x-mouse-map))

(defun x-mouse-where-is (function map)
  (let ((map-name (symbol-name map))
	(map-contents (symbol-value map)))
    (if (consp map-contents)
	;; It's a sparse keymap
	(apply 'append
	       (mapcar
		(function 
		 (lambda (x)
		   (if (eq (cdr x) function)
		       (cons (string-to-int (car x)) map-name))))
		(cdr map-contents))) 
      (let ((index 0)			; For remembering where we are in the map....
	    (result '())
	    (map-size (length map-contents)))
	(while (< index map-size)
	  (let ((map-el (aref map-contents index)))
	    (if (and map-el (eq map-el function))
		;; We have a non-nil binding...
		(setq result (cons (cons index map-name) result))))
	  (setq index (1+ index)))
	result))))

(defun x-mouse-binding (x-mouse-fn &optional pretty)
  "Describe the mouse binding of X-MOUSE-FN.
If called interactively or if optional 2nd arg PRETTY is non-nil, 
make it a user-friendly string description."
  (interactive "aFunction name: \nP")
  (let ((bindings 
	 (apply 'append 
		(mapcar (function (lambda (x) (x-mouse-where-is x-mouse-fn x)))
			(list 'local-x-mouse-text-map
			      'local-x-mouse-scrollbar-map
			      'local-x-mouse-modeline-map
			      'local-x-mouse-minibuffer-map
			      'global-x-mouse-text-map
			      'global-x-mouse-scrollbar-map
			      'global-x-mouse-modeline-map
			      'global-x-mouse-minibuffer-map)))))
    (if bindings
	(if (interactive-p)
	    (message
	     (format
	      "%s is on %s." 
	      x-mouse-fn
	      (mapconcat
	       (function
		(lambda (x)
		  (format "%s in the %s region"
			  (cdr (assq (car x) x-button-help-alist))
			  (let ((map-id (substring (cdr x) -6 -5)))
			    (cond ((string= map-id "x") "text")
				  ((string= map-id "a") "scrollbar")
				  ((string= map-id "n") "modeline")
				  ((string= map-id "e") "minibuffer"))))))
	       bindings " or ")))
	  (if pretty
	      (mapconcat
	       (function
		(lambda (x)
		  (format "%s in the %s region"
			  (cdr (assq (car x) x-button-help-alist)) 
			  (let ((map-id (substring (cdr x) -6 -5)))
			    (cond ((string= map-id "x") "text")
				  ((string= map-id "a") "scrollbar")
				  ((string= map-id "n") "modeline")
				  ((string= map-id "e") "minibuffer"))))))
	       bindings " or ")
	    bindings))
      (if (interactive-p)
	  (message (format "%s does not have a mouse-binding." x-mouse-fn))
	(format "%s does not have a mouse-binding." x-mouse-fn)))))

(defun x-mouse-report-bindings (map)
  "Describe the contents of MAP, a mouse map."
  (eval-in-window
    x-mouse-window
    (with-output-to-temp-buffer "*Help*"
      (let ((local-map (intern-soft (format "local-%s" map)))
	    (global-map (intern-soft (format "global-%s" map))))
	(let ((l-map (eval local-map))
	      (region (substring map 8 -4)))
	  (if l-map
	      ;; The Local mouse map has some bindings in it...
	      (progn 
		(princ "Local Mouse bindings in ")
		(princ region)
		(princ " region are:\n")
		(princ (x-mouse-describe-map l-map))))
	  (princ "Global Mouse bindings in ")
	  (princ region)
	  (princ " region are:\n")
	  (princ (x-mouse-describe-map (eval global-map) l-map)))))))

(defun x-mouse-describe-map (map &optional local-map)
  "Return a documentation string describing the bindings in MAP, an x-mouse map."
  (if (consp map)
      ;; It's a sparse keymap
      (apply 'concat
	     (mapcar
	      (function (lambda (x)
			  (let ((fn (cdr x)))
			    (if (eq fn 'x-mouse-ignore)	; Ignore x-mouse-ignore
				""
			      (format "%s:			%s\n"
				      (cdr (assoc (car x) x-button-help-alist)) fn)))))
	      (cdr map))) 
    (let ((count 0));; For remembering where we are in the map....
      (mapconcat
       (function (lambda (x)
		   (setq count (1+ count))
		   (if x
		       ;; We have a non-nil binding...
		       (let ((keynum (1- count)))
			 (if (or (and local-map (assoc keynum local-map))
				 ;; We have already printed the local
				 ;; binding for this mouse event
				 (eq x 'x-mouse-ignore)) ; Ignore x-mouse-ignore
			     ""
			   (format "%s:			%s\n"
				   (cdr (assoc keynum x-button-help-alist)) x)))
		     "")))
       map "")))) 

;;;; Initialise sensible (?) settings for maps whether they are being used or not.

(setq global-x-mouse-modeline-map (make-keymap))
(setq global-x-mouse-minibuffer-map (make-keymap))
(setq global-x-mouse-scrollbar-map (make-keymap))
(setq global-x-mouse-text-map (make-keymap))
(make-variable-buffer-local 'local-x-mouse-modeline-map)
(make-variable-buffer-local 'local-x-mouse-minibuffer-map)
(make-variable-buffer-local 'local-x-mouse-scrollbar-map)
(make-variable-buffer-local 'local-x-mouse-text-map)

;; Prevent beeps on button-up.  If the button isn't bound to anything, it
;; will beep on button-down.

(global-set-mouse 'modeline	x-button-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-s-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'text		x-button-c-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-middle-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-left-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-s-right-up	 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-s-middle-up 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-s-middle-up 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-m-s-middle-up 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-s-middle-up 'x-mouse-ignore)
(global-set-mouse 'modeline	x-button-c-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'minibuffer	x-button-c-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'text	 	x-button-c-m-s-left-up	 'x-mouse-ignore)
(global-set-mouse 'scrollbar	x-button-c-m-s-left-up	 'x-mouse-ignore)
  
