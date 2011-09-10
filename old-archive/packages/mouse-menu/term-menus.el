;;; Pop-up menus for use on arbitrary terminal types.
;;; Similar in spirit (and functionality) to the mode-specific
;;; SUN menu interface described in 'sun-menus.el'.
;;; Russell A. Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;; Wed May  4 11:33:23 1988

(require 'electric)
(require 'gensym)
(require 'hci-menus)
(provide 'term-menus)

;;;; This part of the file deals with menu definition/creation, and
;;;; would be identical to that of the SUN menu functions, except that
;;;; we don't actually have a mouse...  Therefore it is necessary to
;;;; specify a key binding to be used to for the menu-pop-up, the
;;;; default is Meta-Control-m.  This finger-twister is the default
;;;; because the obvious choice (to me at any rate...), Meta-m, is
;;;; already bound by default.  I suppose you could use Control-c-m,
;;;; but Control-c is the user-prefix map, supposedly reserved for
;;;; user customisation, so it should be left alone.

(defvar term-menu-key "\e\C-m"
  "*The keystroke that pop-up menus are to be bound to.")

(defvar term-menu-buffer-switch nil
  "*If non-nil (default nil) replace key bindings of switch-to-buffer
and switch-to-buffer-other-window with menu-based equivalents.")

;;; The main function is set-mode-menu, e.g.
;;; (set-mode-menu MENU-NAME)
;;; 	MENU-NAME is the function that defines the top-level menu,

;;; To use this facility, insert lines like the following in your
;;; '.emacs' init file.
;;; 	(setq lisp-mode-hook
;;; 	      (function (lambda ()
;;; 			  (require 'term-menus) 
;;; 			  (set-mode-menu 'lisp-menu))))

;;; If you always load 'term-menus', by putting (require 'term-menus)
;;; in your '.emacs' init file, then this can be simplified into:
;;;	(setq lisp-mode-hook
;;; 	      (function (lambda ()
;;;			  (set-mode-menu 'lisp-menu))))

;;; To define your own menus, for customisation or to provide some
;;; that do not already exist, try:
;;;  	(setq any-mode-hook
;;;   	      (function
;;; 		(lambda ()
;;; 		  (defHCImenu Any-menu
;;; 		    ("Any Mode Menu name string")
;;; 		    ("Menu item label" fn-name arg1 arg2) ; This fn takes 2 args
;;; 		    ("Another Menu item label" another-fn-name) ; No args..
;;; 		    ("Walking Menu item" . Any-other-menu) ; A pull-right menu
;;; 		    ("Last Menu item" last-fn arg)) ; A fn of 1 arg.
;;; 		  (defHCImenu Any-other-menu
;;; 		    ;; Any-other-name has no name stripe...
;;; 		    ("Any other Menu item label" fn-name arg)
;;; 		    ("Any other Menu next item label" next-fn-name)
;;; 		    ("Any other Menu last item label" last-fn-name arg1 arg2 arg3))
;;; 		  (set-mode-menu 'Any-menu))))

;;;; The first thing we need is a better interface for creating
;;;; interfaces, so...

(defun create-mode-term-menu-eval-function (mode-name mode-menu)
  "Create an appropriate mode-menu-eval function for MODE-NAME with MODE-MENU.
This is the function that is usually is called when a menu is requested in
a MODE-NAME window. See also set-mode-terminal-menu."
  (eval
    (` (defun (, (make-symbol (format "%s-term-menu" mode-name))) ()
	 (, (format "Terminal Menu handler for %s." mode-name))
	 (interactive)
	 (term-menu-evaluate (selected-window) '(, mode-menu))))))

(defun set-mode-menu (menu)
  "Set MENU to be the menu that pops up in the current major mode when 
term-menu-key (default \"ESC RET\") is detected in the text-region."
  (local-set-key term-menu-key
		 (create-mode-term-menu-eval-function major-mode menu))) 

(defun term-menu-init ()
  "Initialise the top level term-menu, i.e. bind it to the value of term-menu-key."
  (global-set-key term-menu-key 'emacs-term-menu))

(defun emacs-term-menu ()
  "Standard vanilla term menu function for vanilla menu pop-up."
  (interactive)
  (term-menu-evaluate (selected-window) 'emacs-menu))

;; Copied from 'sun-mouse.el', Fri May  6 16:47:53 1988
	
(defmacro eval-in-window (window &rest forms)
  "Switch to WINDOW, evaluate FORMS, return to original window."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect
	   (progn
	     (select-window (, window))
	     (,@ forms))
	 (select-window OriginallySelectedWindow)))))

(defun term-menu-evaluate (window menu)
  "Create (or display) a pop-up menu from MENU menu description, 
get a selection, and evaluate that in the context of WINDOW."
  ;; This pops up a menu and sets term-menu-selected-item to be the choice.
  (term-menu-selection (term-menu-buffer-create menu)) 
  (and term-menu-selected-item	; Only do this if there's something to do!
       (if (eq term-menu-no-selection term-menu-selected-item)
	   ;; We want out...
	   term-menu-no-selection  
	 (eval				; Do selection in window
	   (` (eval-in-window window (, term-menu-selected-item))))
	 (setq term-menu-selected-item nil)))) ; Flush selection

(defun term-menu-buffer-create (menu-name)
  "Create a menu buffer for MENU-NAME."
  (let ((menu-buffer (get-buffer-create (format "*%s*" menu-name))))
    (set-buffer menu-buffer)
    (term-menu-mode menu-name)
    menu-buffer))
      
(defun term-menu-select-item (header items)
  "Pop up a Menu with HEADER and ITEMS, return selected item or nil."
  (let ((menu-symbol (gensym)))
    (eval (append (if header
		      (list 'defHCImenu menu-symbol (list header))
		    (list 'defHCImenu menu-symbol))
		  (mapcar (function (lambda (x)
				       (list (format "%s" x) 'identity x)))
			  items)))
    (term-menu-selection (term-menu-buffer-create menu-symbol))
    (makunbound menu-symbol)		; Destroy the Temp Menu.
    (if term-menu-selected-item	; Only do this if there's something to do!
	(if (eq term-menu-no-selection term-menu-selected-item)
	     ;; We want out...
	    (setq term-menu-selected-item nil)
	  (prog1			; Flush selection-slot but eval and 
	      (eval term-menu-selected-item)	; return what was selected.
	    (setq term-menu-selected-item nil))))))

(defun term-menu-select-emacs-buffer (&optional buffers header)
  "Pop up a menu of BUFFERS (defaults to (buffer-list)).
If optional 2nd arg HEADER is non-nil use that instead of 
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
	  (term-menu-select-item
	   (or header "Select a buffer") (mapcar 'car buffers))
	  buffers))))

(defun term-menu-switch-to-buffer ()
  "Switch to a buffer selected via a menu."
  (interactive)
  (switch-to-buffer 
   (or (term-menu-select-emacs-buffer nil "Switch to buffer:")
       (current-buffer))))

(defun term-menu-switch-to-buffer-other-window ()
  "Switch to a buffer in another window selected via a menu."
  (interactive)
  (switch-to-buffer-other-window
   (or (term-menu-select-emacs-buffer nil "Switch to buffer in other window:")
       (current-buffer))))

(if term-menu-buffer-switch
    (progn
      (mapcar
       (function (lambda (key)
		   (global-set-key key 'term-menu-switch-to-buffer)))
       (where-is-internal 'switch-to-buffer))
      (mapcar
       (function (lambda (key)
		   (global-set-key key 'term-menu-switch-to-buffer-other-window)))
       (where-is-internal 'switch-to-buffer-other-window))))

;;; Term menu mode

(defvar term-menu-mode-map nil
  "The mode map for term-menu buffers.")

(if term-menu-mode-map
    nil
  (setq term-menu-mode-map (make-keymap))
  (setq term-menu-mode-esc-map (make-keymap))
  (suppress-keymap term-menu-mode-map t)
  (suppress-keymap term-menu-mode-esc-map t)
  (fillarray term-menu-mode-map 'term-menu-quit) ; Back to previous menu.
  (fillarray term-menu-mode-esc-map 'term-menu-quit)
  (define-key term-menu-mode-map "\e" term-menu-mode-esc-map)
  (define-key term-menu-mode-map " " 'term-menu-select)
  (define-key term-menu-mode-map "q" 'term-menu-noselect) ; Quit from submenus.
  (define-key term-menu-mode-map "\C-g" 'term-menu-noselect)
  (define-key term-menu-mode-map "\C-z" 'suspend-emacs)
  (define-key term-menu-mode-map "\C-h" 'Helper-help)
  (define-key term-menu-mode-map "?" 'Helper-describe-bindings)
  (define-key term-menu-mode-map "l" 'term-menu-scroll-down)
  (define-key term-menu-mode-map "" 'term-menu-scroll-down)
  (define-key term-menu-mode-esc-map "v" 'term-menu-scroll-down)
  (define-key term-menu-mode-map "n" 'term-menu-scroll-up)
  (define-key term-menu-mode-map "\C-v" 'term-menu-scroll-up)
  (define-key term-menu-mode-map "u" 'term-menu-previous-line)
  (define-key term-menu-mode-map "\C-p" 'term-menu-previous-line)
  (define-key term-menu-mode-map "d" 'term-menu-next-line)
  (define-key term-menu-mode-map "\C-n" 'term-menu-next-line))

(defvar term-menu-items ()
  "Where the menu-option-label . functions-to-be-called pairs are stashed.")

(defvar term-menu-selected-item ()
  "The form to be evaluated from the selected menu item")

(defvar term-menu-no-selection (make-temp-name "No-selection")
  "A unique name for the no-selection selection.")

;; Term menu mode is only suitable for specially formatted data.
(put 'term-menu-mode 'mode-class 'special)

(defmacro pullrightp (menu-item)
  (` (atom (cdr (, menu-item)))))

(defun term-menu-mode (name)
  "Major mode for term menus.
Arg NAME is the menu to be displayed."
  (kill-all-local-variables)
  (use-local-map term-menu-mode-map)
  (let* ((menu-body (symbol-value name))
	 (menu-name-suppliedp (null (cdr (car menu-body))))
	 (menu-name (if menu-name-suppliedp (car (car menu-body)) ""))
	 (menu-items (if menu-name-suppliedp (cdr menu-body) menu-body))
	 (menu-item-counter 1)
	 (menu-key-reminders (or (get name 'pretty-key-reminders)
				 (term-menu-pretty-key-reminders
				  name
				  (if menu-name-suppliedp
				      (cdr (get name 'key-reminders))
				    (get name 'key-reminders))))))
    ;; Record the commands for this buffer..
    (make-local-variable 'term-menu-items)
    (setq term-menu-items menu-items)
    (setq mode-line-buffer-identification menu-name)
    (setq mode-name "Menu Mode")
    ;; Set up distinctive mode line.
    (setq mode-line-format
	  (list "        "
		mode-line-buffer-identification
		"       [%[" mode-name "]%]    [" global-mode-string "]        "
		(cons -3 "%p")))
    (while menu-items
      ;; Insert the options in the buffer.
      (insert (format "%d:	%s" menu-item-counter (car (car menu-items))))
      (if (not inhibit-key-reminders)
	  (let ((key-reminder (car menu-key-reminders)))
	    (if key-reminder
		(insert (format "	[%s]" key-reminder)))))
      (if (pullrightp (car menu-items))
	  (insert "	=>"))
      (insert "\n")
      (setq menu-item-counter (1+ menu-item-counter))
      (setq menu-items (cdr menu-items))
      (setq menu-key-reminders (cdr menu-key-reminders)))
    (make-local-variable 'Helper-return-blurb)
    (setq Helper-return-blurb
	  (format "return to the %s." (if menu-name-suppliedp
					  menu-name
					"previous menu")))
    (setq truncate-lines t)
    (delete-char -1)			; remove final newline.
    (goto-char (point-min))
    (delete-matching-lines "^$")	; delete any spurious blank ones.
    (setq buffer-read-only t)
    (setq major-mode 'term-menu-mode)
    (run-hooks 'term-menu-mode-hook)))

(defun term-menu-pretty-key-reminders (menu reminders)
  "Make all key REMINDERS 'pretty' descriptions."
  (put menu 'pretty-key-reminders
       (mapcar (function (lambda (keys)
			   (if keys
			       (mapconcat 'key-description keys " or "))))
	       reminders)))

(defun term-menu-selection (menu-buffer)
  "Pop up the MENU-BUFFER electric term-menu buffer.
Return the function associated with the menu option selected (nil if none)."
  (save-excursion
    (save-window-excursion
      (let ((buffer (window-buffer (Electric-pop-up-window menu-buffer))))
	(unwind-protect
	    (progn
	      (set-buffer buffer)
	      ;; When term-menu-exit is thrown, term-menu-selected-item
	      ;; will have been set to the s-expression to be evaluated, or to
	      ;; the special value term-menu-no-selection, indicating that
	      ;; immediate exit from submenus is desired.
	      (catch 'term-menu-exit
		(Electric-command-loop
		  'term-menu-exit
		  "u-up d-down n-next page l-last page SPC-select q-quit others-prev menu (if any)"
		  t))))
	(kill-buffer buffer)
	(message ""))))
  term-menu-selected-item)		; which should now be set...

(defun term-menu-index ()
  "Return the index of the term menu item listed by this line of the term menu."
  (1+ (count-lines 1 (point))))

(defun term-menu-get-selection ()
  "Set term-menu-selected-item to be the function associated with the menu selection at point."
  (if (not (looking-at "$"))
      (let ((menu-item (nth (1- (term-menu-index)) term-menu-items)))
	(setq term-menu-selected-item
	      (if (pullrightp menu-item)
		  ;; a pullright menu - pull it and get a selection from it
		  (term-menu-selection
		   (eval (` (term-menu-buffer-create '(, (cdr menu-item))))))
		(cdr menu-item))))
    ;; On a blank line, there is no selection here
    (error "There is no menu option on this line.")))

(defun term-menu-select ()
  "If term-menu-selected-item is set, throw out of Electric-command-loop
Kill buffer if quit given."
  (interactive)
  (setq unread-command-char -1)
  (condition-case ()
      (term-menu-get-selection)	; Get a selection.
    (error 
      (setq term-menu-selected-item nil) ; Flush any dummy selections.
      (goto-char (point-min))))
  (condition-case ()
      ;; Only quit when a selection is made.
      (and term-menu-selected-item (throw 'term-menu-exit nil))
    (error
      (setq term-menu-selected-item nil) ; Flush last selection
      (kill-buffer (current-buffer)))))

(defun term-menu-quit ()
  "Throw out of Electric-command-loop, kill buffer if quit given."
  (interactive)
  (setq unread-command-char -1)
  (setq term-menu-selected-item nil) ; Flush last selection
  (condition-case ()
      (throw 'term-menu-exit nil)
    (error
      (kill-buffer (current-buffer)))))
  
(defun term-menu-noselect ()
  "Signal direct quit from submenus."
  (interactive)
  (setq unread-command-char -1)
  (setq term-menu-selected-item term-menu-no-selection)
  (condition-case ()
      (throw 'term-menu-exit nil)
    (error
      (kill-buffer (current-buffer)))))

(defun term-menu-next-line ()
  "Move the beginning of the next line, if already at eob go to beginning of current line."
  (interactive)
  (end-of-line 2)
  (beginning-of-line))

(defun term-menu-previous-line ()
  "Move to previous line, if already at bob go to beginning of current line."
  (interactive)
  (beginning-of-line 0))

(defun term-menu-scroll-down ()
  "Scroll the menu up a page if it's not all visible."
  (interactive)
  (if (not (pos-visible-in-window-p (point-min)))
      (scroll-down nil)))

(defun term-menu-scroll-up ()
  "Scroll the menu down a page if it's not already all visible."
  (interactive)
  (if (not (pos-visible-in-window-p (point-max)))
      (scroll-up nil)))
