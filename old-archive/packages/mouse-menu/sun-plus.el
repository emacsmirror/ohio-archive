;;; Additional Sun mouse & menu functionality.
;;; Augments that provided by the distribution 'sun-fns.el.'

(require 'sun-fns)
(require 'sun-menus)
(require 'gensym)
(provide 'sun-plus)

(defun sun-mouse-select-item (window x y header items)
  "Pop up a Menu at in window WINDOW at X, Y with HEADER and ITEMS.
If HEADER is nil Menu should have no namestripe.
Return member of ITEMS that is selected or nil."
  (let ((menu-sym (gensym))
	(make-menus-silently t))
    (eval (append (if header
		      (list 'defHCImenu menu-sym (list header))
		    (list 'defHCImenu menu-sym))
		  (mapcar (function (lambda (x)
				       (list (format "%s" x) 'identity x)))
			  items)))
    (put menu-sym 'internal t)
    (prog1 
	(sun-menu-other-menu-display menu-sym window x y)
      (setplist menu-sym nil)
      (makunbound menu-sym))))

(defun sun-mouse-select-emacs-buffer (window x y &optional buffers header)
  "Pop up a menu at in WINDOW at X,Y of BUFFERS (defaults to (buffer-list)).
If optional 5th arg HEADER is non-nil use that instead of 
\"Select a buffer\" as the namestripe of the menu to be popped up.
Return selected buffer or nil."
  (let ((buf-list (or buffers (buffer-list)))
	 buf-a-list)
    (while buf-list
      (let ((elt (car buf-list)))
	(if (not (string-match "^ " (buffer-name elt)))
	    (setq buf-a-list	 
		  (cons (cons (format "%14s   %s"
				      (buffer-name elt)
				      (or (buffer-file-name elt) ""))
			      elt)
			buf-a-list))))
      (setq buf-list (cdr buf-list)))
    (setq buffers (reverse buf-a-list))
    (cdr (assoc
	  (sun-mouse-select-item
	   window x y (or header "Select a buffer") (mapcar 'car buffers))
	  buffers))))

(defun sun-mouse-switch-to-buffer (window x y)
  "Switch to a buffer selected via a menu."
  (eval-in-window
    window
    (switch-to-buffer 
     (or (sun-mouse-select-emacs-buffer window x y nil "Switch to buffer: ")
	 (current-buffer)))))

(defun sun-mouse-switch-to-buffer-other-window (window x y)
  "Switch to a buffer selected via a menu."
  (eval-in-window
    window
    (switch-to-buffer-other-window 
     (or (sun-mouse-select-emacs-buffer
	  window x y nil "Switch to buffer in other window: ")
	 (current-buffer)))))

(global-set-mouse '(shift right modeline) 'sun-mouse-switch-to-buffer-other-window)
(global-set-mouse '(meta shift right modeline) 'sun-mouse-switch-to-buffer)

(defvar nil-synonym (gensym)
  "Synonym for nil when it is desired that this value be explicitly selected.")

(defun sun-mouse-query (window x y question answers &optional must-select)
  "In WINDOW at position X Y, ask QUESTION. Return selected item from ANSWERS.
ARG is a list (x-pos y-pos).
QUESTION is a string.
ANSWERS is a list of strings or symbols or lists. If strings or
symbols, the selected string or symbol is displayed and returned when
selected. If lists, the car (which must be a string) is displayed and
the cdr returned when it is selected.

If optional third arg MUST-SELECT is non-nil one of ANSWERS
must be selected; querying will continue until a selection is made.
See also sun-mouse-select-item."
  (let* ((make-menus-silently t)
	 (query-menu (gensym))
	 (menu-items
	  (if (consp (car answers))
	      (if must-select
		  ;; The user may desire some options to return nil, as in the
		  ;; yes-or-no-p example below. Cater for this with a synonym.
		  (mapcar (function (lambda (x)
				      (if (null (cdr x))
					  (list (car x) 'identity 'nil-synonym)
					(list (car x) 'quote (cdr x)))))
			  answers)
		(mapcar (function (lambda (x) (list (car x) 'quote (cdr x))))
			answers))
	    (mapcar (function (lambda (x) (list (format "%s" x) 'quote x)))
		    answers))))
    (eval (append (cons 'defHCImenu (list query-menu (list question))) menu-items))
    (put query-menu 'internal t)	; Make it invisible.
    (let ((selection (sun-menu-other-menu-display query-menu window x y)))
      (if must-select
	  (if (null selection)
	      (let* ((all-rev (reverse (mapcar 'car menu-items)))
		     (but-last (cdr all-rev))
		     (last (car all-rev))
		     (all-but-last (nreverse but-last))
		     (mesg (format "You must select one of: %s or %s."
				   (mapconcat 'identity all-but-last ", ") last)))
		(while (null selection)
		  (message mesg)
		  (ding)
		  (setq selection (sun-menu-other-menu-display query-menu window x y)))
		(if (eq selection nil-synonym)
		    nil
		  selection))
	    (setplist query-menu nil)	; Throw away it's internals.
	    (makunbound query-menu)	; Throw it away in a vain attempt not to waste memory.
	    (if (eq selection nil-synonym)
		nil
	      selection))
	selection))))

(defun sun-mouse-yes-or-no-p (question &optional window x y)
  "Ask a yes or no QUESTION in WINDOW at X, Y. Force user to explicitly select yes or no.
Position args WINDOW, X and Y are optional, 
defaulting to *mouse-window*, *mouse-x* and *mouse-y*."
  (sun-mouse-query
   (or window *mouse-window*) (or x *mouse-x*) (or y *mouse-y*)
   question '(("yes" . t) ("no")) t))

(defun sun-mouse-y-or-n-p (question &optional window x y)
  "Ask user a yes or no QUESTION in WINDOW at X, Y. No selection is a synonym for no.
Position args WINDOW, X and Y are optional, 
defaulting to *mouse-window*, *mouse-x* and *mouse-y*."
 (sun-mouse-query
  (or window *mouse-window*) (or x *mouse-x*) (or y *mouse-y*)
  question '(("yes". t) ("no"))))

(defun sun-mouse-completing-select (prompt table predicate &optional require-match initial-input pos)
  "Offer a list of possibilities for selection using the mouse under SunView.
Args are PROMPT, TABLE, PREDICATE and optional args REQUIRE-MATCH, INITIAL-INPUT, WIN, X, Y.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray (see try-completion).
PREDICATE limits completion to a subset of TABLE; see try-completion for details.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
a selection from TABLE is made.
If INITIAL-INPUT is non-nil, make this the default selection (the one
the mouse pointer is warped to).
If WIN, X and Y are non-nil, they are the WINDOW and X,Y coordinates for menu on screen.
The default is *mouse-window* at *mouse-x*, *mouse-y*.
Don't try to use this as a replacement for completing-read: it's too slow and 
your screen is probably not big enough to list all possibilities. Think about C-h f."
  (let (possibilities)
    (message "Making completion list...")
    (if (or (vectorp table) (arrayp table))
	;; What is the canonical test for obarray-ness?
	(mapatoms (function (lambda (x)
			      (if (funcall predicate x)
				  (setq possibilities (append possibilities (list x))))))
		  table)
      (if (consp table)
	  (mapcar (function (lambda (x)
			      (if (funcall predicate x)
				  (setq possibilities (append possibilities (list x))))))
		  table)))
    (message "Making completion list...done")
    (let ((selections (if initial-input
			  (cons initial-input (delq initial-input possibilities))
			possibilities)))
      (sun-mouse-query (or win *mouse-window*) (or x *mouse-x*) (or y *mouse-y*)
		       prompt selections require-match))))