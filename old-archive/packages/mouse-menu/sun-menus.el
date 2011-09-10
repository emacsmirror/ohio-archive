;;; SunView interface to the hci-menus package; provides pop-up and
;;; pull-right menus for the SunView window system.

;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;; Tue Mar 21 17:43:14 1989 

(require 'sun-fns)
(require 'hci-menus)
(provide 'sun-menus)

;;; The main function is set-mode-menu, e.g.
;;; 	(set-mode-menu MENU-NAME)
;;; MENU-NAME is the top-level menu.  To use this facility, insert
;;; lines like the following in your '.emacs' init.  file.
;;; 	(setq lisp-mode-hook
;;; 	      (function (lambda ()
;;;			  (require 'lisp-menus)
;;;			  (set-mode-menu 'lisp-menu))))

;;; To define your own menus, for customisation or to provide some
;;; that do not already exist, try:
;;;  	(setq any-mode-hook
;;;   	      (function
;;; 		(lambda ()
;;; 		  (defHCImenu Any-menu
;;; 		    ("Any Mode Menu name string")
;;; 		    ("Menu item label" fn-name arg1 arg2)
;;; 		    ("Another Menu item label" another-fn-name)
;;; 		    ("Walking Menu item" . Any-other-menu)
;;; 		    ("Last Menu item" last-fn arg))
;;; 		  (defHCImenu Any-other-menu
;;; 		    ;; Other-name has no name stripe...
;;; 		    ("Other Menu item label" fn-name arg)
;;; 		    ("Other Menu next label" next-fn-name)
;;; 		    ("Other Menu last label" last-fn-name arg1 arg2 arg3))
;;; 	          (set-mode-menu 'Any-menu))))
  
(defvar sun-menu-mouse-binding '(right text)
  "*The mouse binding that the pop-up menu function is bound to.")

(defvar make-menus-silently nil
  "*If non-nil (default nil) no messages will be printed as menus are made.")

(defvar sun-keyboard-prefix-binding "C-x *"
  "The 'pretty' description of the key sequence the sun key map is bound to.")

(defvar sun-keyboard-translate-table
  '(("a" . "1") ("b" . "2") ("c" . "3") ("d" . "4") ("e" . "5") ("f" . "6")
    ("g" . "7") ("h" . "8") ("i" . "9") ("j" . "10") ("k" . "11")
    ("l" . "12") ("m" . "13") ("n" . "14") ("o" . "15"))
  "Maps key sequences of the form {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o} to the
appropriate 'pretty' key description, e.g. \"c\" => \"3\".
The remaining characters determine which keypad and what modifiers were used.")

(defvar sun-keyboard-modifier-translate-table
  '(("l" . "L") 	    ("t" . "F") 	    ("r" . "R")
    ("L" . "Shift-L")	    ("T" . "Shift-F")	    ("R" . "Shift-R")
    ("," . "C-L")	    ("4" . "C-F")	    ("2" . "C-R")
    ("M-l" . "M-L")	    ("M-t" . "M-F")	    ("M-r" . "M-R")
    ("C-l" . "C-Shift-l")   ("C-t" . "C-Shift-F")   ("C-r" . "C-Shift-R")
    ("M-L" . "M-Shift-L")   ("M-T" . "M-Shift-F")   ("M-R" . "M-Shift-R")
    ("M-," . "M-C-L") 	    ("M-4" . "M-C-F")	    ("M-2" . "M-C-R")
    ("M-" . "M-C-Shift-L")("M-" . "M-C-Shift-F")("M-" . "M-C-Shift-R"))
  "Maps final substrings of a keypad keybinding to the appropriate modifiers.")

(defun sun-init ()
  "Set up Emacstool window, if you know you are in an emacstool."
  (define-key ctl-x-map "\C-@" 'sun-mouse-handler)
  (if (< (sun-window-init) 0)
      (message "Not running under Emacstool")
    (mapcar (function (lambda (key) (global-set-key key 'suspend-emacstool)))
	    (where-is-internal 'suspend-emacs))
    (substitute-key-definition 'suspend-emacs 'suspend-emacstool current-global-mousemap)
    (substitute-key-definition 'suspend-emacs 'suspend-emacstool current-local-mousemap)
    (if (not (or (equal sun-menu-mouse-binding '(right text))
		 (equal sun-menu-mouse-binding '(text right))))
	;; Remove default binding of emacs-menu-eval, and insert preferred binding.
	(progn
	  (substitute-key-definition 'emacs-menu-eval 'ignore current-global-mousemap)
	  (global-set-mouse sun-menu-mouse-binding 'emacs-menu-eval)
	  ;; Selection is done using right mouse button, this is the
	  ;; default for popping the menu up. We only need say this if
	  ;; the default  sun-menu-mouse-binding  has been overridden.  
	  (message "Select menu options using the right mouse button.")))))

(defun create-mode-menu-eval-function (mode-name mode-menu)
  "Create an appropriate mode-menu-eval function for MODE-NAME with
MODE-MENU. This is the function that is usually is called when  
sun-menu-mouse-binding  occurs in a MODE-NAME window. 
See also set-mode-menu.  
This function is used by set-mode-menu, and not intended to be user-callable.  

If you have more than 1 window on the screen, in differing modes, a potential
problem exists since most functions operate on the buffer point is in, 
not the buffer the mouse is in. To avoid real consistency problems (and confusion) 
if point and mouse are not in the same window, point is put into the same window 
as the mouse before the menu pops up." 
  (eval
   (` (defun
	(, (intern (format "%s-menu-eval" mode-name)))
	(window x y)
	(, (format "Menu evaluation handler for %s." mode-name))
	(interactive)
	(if (not (eq (selected-window) window))
	    (progn			; Ensure cursor movement seen.
	      (message "Moving to mouse window.")
	      (select-window window)
	      (message "")))
	(sun-menu-evaluate
	 (selected-window) (1+ x) (1- y) (sun-make-menu '(, mode-menu)))))))

(defun sun-make-menu (menu)
  "Make an internal SUN menu from MENU."
  (let ((internal-representation
	 (get menu (if inhibit-key-reminders 'sun-menu 'sun-menu-reminding))))
    (or internal-representation
	(prog2
	 ;; Explain why the machine appears to have tripped up (or not)...
	 (if (not make-menus-silently)
	     (message "Making SUN Menu %s" menu))
	 (if inhibit-key-reminders
	     (put menu 'sun-menu
		  (eval
		   (cons 'defmenu
			 (cons (intern (format "%s-internal" menu))
			       (mapcar (function
					(lambda (item)
					  (let ((thing (cdr item)))
					    (if (and thing 
						     (symbolp thing)
						     (menu-p thing))
						(cons (car item) 
						      (sun-make-menu thing))
					      item))))
				       (symbol-value menu))))))
	   (put menu 'sun-menu-reminding
		(eval (cons 'defmenu
			    (cons (intern (format "%s-reminding-internal" menu))
				  (sun-display-keybindings menu))))))
	 (if (not make-menus-silently)
	     (message "Making SUN Menu %s...done" menu))))))

(defun sun-display-keybindings (menu)
  "Return MENU with key bindings for commands added to the
end of the menu item if the command is bound to a key."
  (or (get menu 'reminding-menu-items)
      (let ((keybindings (cons 'dummy (sun-translate-keybindings menu)))
	    (body (symbol-value menu)))
	(put menu 'reminding-menu-items
	     (mapcar 
	      (function 
	       (lambda (menu-item)
		 ;; Wish mapcar would take an arbitrary number of sequences.
		 (setq keybindings (cdr keybindings))
		 (cons (let ((bindings (car keybindings)))
			 (if bindings
			     (format "%s [%s]" (car menu-item) bindings)
			   (car menu-item)))
		       (let ((action (cdr menu-item)))
			 (if (and action (symbolp action) (menu-p action))
			     (sun-make-menu action)
			   action)))))
	      body)))))

(defun sun-translate-keybindings (menu)
  "Return a list of the correct pretty key description for all keybindings in
MENU, using sun-keyboard-translate-table."
  (or (get menu 'sun-keyboard-translated)
      (let ((key-reminders (get menu 'key-reminders)))
	(put menu 'sun-keyboard-translated
	     (mapcar
	      (function
	       (lambda (keys)
		 (if keys
		     (mapconcat 'sun-key-description keys " or "))))
	      key-reminders)))))

(defun sun-key-description (keystroke)
  "Return a 'pretty' description of a SUN keyboard keystroke."
  (let* ((prefix-length (length sun-keyboard-prefix-binding))
	 (pretty-key (key-description keystroke))
	 (sun-keyboard-prefixp
	  (and (> (length pretty-key) prefix-length)
	       (string= sun-keyboard-prefix-binding
			(substring pretty-key 0 prefix-length)))))
    (if sun-keyboard-prefixp
	(let ((modifier	(cdr (assoc (substring pretty-key (+ prefix-length 3))
				    sun-keyboard-modifier-translate-table)))
	      (number (cdr (assoc (substring
				   pretty-key (1+ prefix-length) (+ prefix-length 2))
				  sun-keyboard-translate-table))))
	  (if (and modifier number)
	      (concat modifier number)
	    pretty-key))
      pretty-key)))

(defun set-mode-menu (menu)
  "Set the menu that pops up in the current major mode, when the mouse
event specified by sun-menu-mouse-binding, occurs to be MENU. 

If the first item in a pull-right menu is NOT a name stripe, then
selecting the pull-right menu item in the parent will execute it
without having to explicitly pull the menu, i.e. the default action of
pull-right menus without name stripes is the action performed by the
first item in the menu. Actually this is true of menus with
namestripes as well, but namestripes are indicated to the evaluator by
having a null action, so a null action is what is performed..."
  (interactive)
  (local-set-mouse
    sun-menu-mouse-binding (create-mode-menu-eval-function major-mode menu)))

(defun emacs-menu-eval (window x y)
  "Replaces standard supplied function with one that changes the 
selected window to be the one the mouse is in before menu pop-up."
  (interactive)
  (if (not (eq (selected-window) window))
      (progn				; Ensure cursor movement is seen.
	(message "Moving to mouse window.")
	(select-window window)
	(message "")))
  (sun-menu-evaluate
   (selected-window) (1+ x) (1- y) (sun-make-menu 'emacs-menu)))

(defun minibuffer-menu-eval (window x y)
  "Pop-up menu of minibuffer commands."
  (sun-menu-evaluate window x (1- y) (sun-make-menu 'minibuffer-menu)))

(defun sun-menu-other-menu-display (menu &optional w x y)
  "Display MENU in window W at position (X,Y).  W, X and Y are optional,
defaulting to *menu-window* (1+ *menu-x*) (1+ *menu-y*)

   This to allow menus to pop up menus as an alternative to
   pull-rights.  It also enables recursive, multiply-inheritant menu
   structures to be created, and as such should be used with extreme
   care (or with a Path Algebra Tool)."
  (let ((w (or w *menu-window*))
	(x (or x *menu-x*))
	(y (or y *menu-y*)))
    (sun-menu-evaluate w x y (sun-make-menu menu))))

(defun sun-set-namestripe (string)
  "Set the namestripe of the current 'suntools' window to be STRING."
  (send-string-to-terminal (format "\033]l%s\033\\" string)))

(defun sun-menu-banner ()
  "Display how to get menus in the namestripe."
  (let* ((menu-binding (copy-alist sun-menu-mouse-binding))
	 (region (cond ((member 'text menu-binding)
			(setq menu-binding (delq 'text menu-binding))
			'text)
		       ((member 'modeline menu-binding)
			(setq menu-binding (delq 'modeline menu-binding))
			'modeline)
		       ((member 'scrollbar menu-binding)
			(setq menu-binding (delq 'scrollbar menu-binding))
			'scrollbar)
		       ((member 'minibuffer menu-binding)
			(setq menu-binding (delq 'minibuffer menu-binding))
			'minibuffer)
		       (t (error
			   "Menu pop-up function not bound to an existing region: %s"
			   sun-menu-mouse-binding))))
	 (button (cond ((member 'left menu-binding)
			(setq menu-binding (delq 'left menu-binding))
			'Left)
		       ((member 'middle menu-binding)
			(setq menu-binding (delq 'middle menu-binding))
			'Middle)
		       ((member 'right menu-binding)
			(setq menu-binding (delq 'right menu-binding))
			'Right)
		       (t (error
			   "Menu pop-up function not bound to an existing button: %s"
			   sun-menu-mouse-binding))))
	 (keys menu-binding))		; Key modifiers (if any) are what's left.
  (sun-set-namestripe
   (format
    "Scottish HCI Centre Emacs Listener [Hit %s%s-mouse in the %s region for Menus]"
    (if keys
	(concat (mapconcat
		 (function (lambda (key) (capitalize (symbol-name key))))
		 keys "-")
		"-")
      "")
    button
    region))))

;;; SUNs minibuffer menu is bugged, it calls SUSPEND-EMACS not
;;; SUSPEND-EMACSTOOL, so... Might as well chrome it now too.
;;; Russell, Wed Jun 29 16:02:03 1988.

(defHCImenu minibuffer-menu
  ("Minibuffer" message "Some miscellanous minibuffer commands.")
  ("Stuff" sun-yank-selection)
  ("Do-It"
   if (minibuffer-window-p (selected-window))
   (exit-minibuffer)
   (error "There is no minibuffer command to do."))
  ("Close Emacstool" suspend-emacstool))

;;; Redefine the default and help menus. 

(defHCImenu help-describe-menu
  ("bindings" describe-bindings)
  ("mouse bindings" describe-mouse-bindings)
  ("mouse briefly" call-interactively 'describe-mouse-briefly)
  ("key" call-interactively 'describe-key)
  ("key briefly" call-interactively 'describe-key-briefly)
  ("function" call-interactively 'describe-function)
  ("variable" call-interactively 'describe-variable)
  ("mode" describe-mode)
  ("syntax" describe-syntax))

;; Mouse-help-menu is defined using the standard ``defmenu'' macro.
;; This does not save its contents effectively, so we recreate it
;; using ``defHCImenu''.

(defHCImenu mouse-help-menu
  ("Text Region"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'text)
  ("Scrollbar"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'scrollbar)
  ("Modeline"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'modeline)
  ("Minibuffer"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'minibuffer))

(defHCImenu help-menu
  ("Help Menu")
  ("Command apropos" call-interactively 'command-apropos)
  ("Describe" . help-describe-menu)
  ("Tutorial help" help-with-tutorial)
  ("Mouse help" . mouse-help-menu)
  ("View lossage" view-lossage)
  ("Info" info))

(defHCImenu emacs-quit-menu
  ("Close Emacstool" suspend-emacstool)
  ("Quit" save-buffers-kill-emacs))

(defHCImenu emacs-menu
  ("Emacs Menu")
  ("Undo" call-interactively 'undo)
  ("Stuff selection" sun-yank-selection)
  ("Select region" call-interactively 'sun-select-region)
  ("Find file" . find-file-menu)
  ("New buffer" . new-buffer-mode-menu)
  ("Help menu" . help-menu)
  ("Quit" . emacs-quit-menu))
