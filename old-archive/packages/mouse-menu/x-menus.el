;;; Crude, primitive X Menu facility. But it works...
;;; Russell A. Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;; Mon Feb 13 16:33:20 1989 

(require 'x-mouse)
(require 'x-fns)
(provide 'x-menus)

;;; The extant X menu facilities don't provide pull-right (or walking)
;;; menus, so this code cannot parse specifications as defined in
;;; 'hci-menus.el', and used by 'sun-menus.el' and 'term-menus.el'.

;;; Thus 'inhibit-key-reminders' is undefined when this file is loaded:

(defvar inhibit-key-reminders nil
  "*If non-nil (default is nil) menus will not attempt to display key
bindings for options.")

(defvar x-menu-mouse-binding x-button-right
  "*The mouse binding that the pop-up menu function is bound to.")

(defun prefix-arg-supplied (command)
  "Declare in a menu entry that COMMAND is called as if typed with a C-u prefix."
  (let ((current-prefix-arg (list 4)))
    (call-interactively command)))

(defun defXmenu (menu-name menu-spec)
  "Make MENU-NAME an X menu with MENU-SPEC as it's contents.
If  inhibit-key-reminders  is non-nil display the keystroke that 
will do the same thing as the menu option on the menu, if it exists."  
  (set menu-name menu-spec)	; For X a menu is just it's item-list.
  (put menu-name 'key-reminders (x-menu-keybindings menu-spec))
  menu-spec)

(defun x-menu-keybindings (menu-spec-list)
  "Return MENU-SPEC-LIST with key bindings for commands added to the
end of the menu item if the command is bound to a key."
  (let ((name (car menu-spec-list))
	(menus (cdr menu-spec-list))
	;; Declare these outside loop for speed 
	new-menu-items result)
    (while menus
      (let* ((menu (car menus))
	     (menu-name (car menu))
	     (menu-items (cdr menu)))
	(setq new-menu-items nil)
	(while menu-items
	  (let* ((menu-form (cdr (car menu-items)))
		 (menu-form-car (car menu-form))
		 (prefix-arg-supplied-p nil)
		 (command 
		  (cond ((eq menu-form-car 'call-interactively)
			 (eval (car (cdr menu-form))))
			((eq menu-form-car 'prefix-arg-supplied)
			 (setq prefix-arg-supplied-p t)
			 (eval (car (cdr menu-form))))
			(t (and (null (cdr menu-form)) ; No arguments supplied
				menu-form-car))))
		 (keystrokes 
		  (if (commandp command)
		      (if prefix-arg-supplied-p
			  (mapcar
			   '(lambda (x) (concat "" x))
			   (where-is-internal command (current-local-map)))
			(where-is-internal command (current-local-map))))))
	    (setq new-menu-items
		  (append new-menu-items
			  (list
			   (if keystrokes
			       (cons 
				(format "%s [%s]"
					(car (car menu-items))
					(mapconcat
					 'key-description keystrokes " or "))
				menu-form)
			     ;; No keybinding
			     (car menu-items))))	  
		  menu-items (cdr menu-items))))
	  (setq result (append result (list (cons menu-name new-menu-items)))
		menus (cdr menus))))
    (cons name result)))

;;; defvars are used for defining these menus to provide documentation
;;; strings, and also to make sure loading this file after ".emacs"
;;; does not redefine any preferences that the luser may set there,
;;; rather than using term-setup-hook.

(defXmenu 'x-mouse-default-menu
  '("Emacs-X Menu" 
    ("Command Menu"
     ("Undo last edit" call-interactively 'undo)
     ("Quit from complex command or unknown state"
      call-interactively 'keyboard-quit)
     ("Exit from Emacs" x-mouse-save-buffers-kill-emacs))
    ("File and Buffer Menu" 
     ("Find File" call-interactively 'find-file)
     ("Find File in other window" call-interactively 'find-file-other-window)
     ("Save File" call-interactively 'save-buffer)
     ("Save File (prompting for a name)" call-interactively 'write-file)
     ("List Buffers" call-interactively 'electric-buffer-list))
    ("Word Processing Menu"
     ("LaTeX" new-buffer-other-window 'LaTeX-mode ".tex" t)
     ("TeX" new-buffer-other-window 'TeX-mode ".tex")
     ("Nroff" new-buffer-other-window 'nroff-mode ".roff")
     ("Scribe" new-buffer-other-window 'scribe-mode ".mss")
     ("Text" new-buffer-other-window 'text-mode ".text"))
    ("Programming Menu"
     ("Lisp" new-buffer-other-window 'lisp-mode ".lisp")
     ("Common Lisp" new-buffer-other-window 'common-lisp-mode ".cl")
     ;; ("Kyoto Common Lisp" new-buffer-other-window 'kyoto-lisp-mode ".lsp")
     ("Franz Lisp" new-buffer-other-window 'franz-lisp-mode ".l")
     ("Emacs Lisp" new-buffer-other-window 'emacs-lisp-mode ".el")
     ("Scheme" new-buffer-other-window 'scheme-mode ".scm")
     ("PROLOG" new-buffer-other-window 'prolog-mode ".pl")
     ("Ada" new-buffer-other-window 'ada-mode ".ada") ; see below
     ("C" new-buffer-other-window 'c-mode ".c")
     ("FORTRAN" new-buffer-other-window 'fortran-mode ".f")
     ("Modula-2" new-buffer-other-window 'modula-2-mode ".mod")) 
    ("Tool Menu"
     ("Read Mail" rmail)
     ("Send Mail" mail)
     ("Dired" call-interactively 'dired)
     ("Shell" shell)
     ("Telnet" telnet)
     ("Terminal Emulator" call-interactively 'terminal-emulator))
    ("Help Menu"
     ("Enter Tutorial introduction to Emacs" help-with-tutorial)
     ("Enter \"Info\", the documentation browser" info)
     ("Command apropos" call-interactively 'command-apropos)
     ("Where is command" call-interactively 'where-is))
    ("Describe Menu"
     ("Describe Mode" describe-mode)
     ("Describe mouse bindings" x-mouse-help)
     ("Describe single key binding" call-interactively 'describe-key)
     ("Describe all key bindings" describe-bindings)
     ("Describe mode" describe-mode)
     ("Describe function" call-interactively 'describe-function)
     ("Describe variable" call-interactively 'describe-variable)
     ("Describe syntax" describe-syntax))
    ("Miscellaneous Menu"
     ("Emacs ordering information" describe-distribution)
     ("Emacs copying information" describe-copying)
     ("Emacs recent changes" view-emacs-news)
     ("Emacs [absence of] warranty information" describe-no-warranty))))

(autoload 'ada-mode "ada")

(defvar global-x-mouse-menu
  'x-mouse-default-menu
  "*The default global X mouse menu.")

(defvar x-mouse-help-menu
  (defXmenu 'x-mouse-help-menu
    '("Mouse regions"
      ("Choose a mouse region:"
       ("Text" x-mouse-report-bindings "x-mouse-text-map")
       ("Scrollbar" x-mouse-report-bindings "x-mouse-scrollbar-map")
       ("Modeline" x-mouse-report-bindings "x-mouse-modeline-map")
       ("Minibuffer" x-mouse-report-bindings "x-mouse-minibuffer-map"))))
  "*The default mouse help menu for X.")

(defvar local-x-mouse-menu nil
  "*The local X menu for the current major mode.")
(make-variable-buffer-local 'local-x-mouse-menu)

(defun x-mouse-menu (arg &optional menu)
  "Make a (mode-sensitive) menu pop up, offering various command
options. If optional arg MENU is supplied, use as menu to pop up."
  (eval-in-window
    x-mouse-window
    (let ((menu (or menu local-x-mouse-menu global-x-mouse-menu)))
      (let ((selection (x-popup-menu x-mouse-pos
				     (if inhibit-key-reminders
					 (symbol-value menu)
				       (get menu 'key-reminders)))))
	(if selection
	    (eval selection))))))

(defun x-mouse-other-menus ()
  "Pop up menu of things that should be available from all menus, but not at top-level."
  (x-mouse-menu x-mouse-pos global-x-mouse-menu))

(defun x-mouse-help ()
  "Pop up a menu of the available regions and describe the mouse bindings in the selection."
  (interactive)
  (x-mouse-menu x-mouse-pos 'x-mouse-help-menu))

(defun set-mode-menu (menu)
  "Make the current local X menu be MENU."
  (setq local-x-mouse-menu menu))

(global-set-mouse 'text x-menu-mouse-binding 'x-mouse-menu) 

(defvar x-mouse-exit-menu
  (defXmenu 'x-mouse-exit-menu
    '("Exit Menu"
      ("Confirm exit from Emacs?"
       ("Yes" save-buffers-kill-emacs)
       ("No" error "Emacs exit aborted..."))))
  "*The default mouse exit query menu for X.")

(defun x-mouse-save-buffers-kill-emacs ()
  "Use the mouse to confirm whether or not to call the Emacs exit function."
  (interactive)
  (x-mouse-menu x-mouse-pos 'x-mouse-exit-menu))
