;;;; Implement a mode-specific menu definition schema. Allow it 
;;;; to show what (if any) keystrokes invoke the menu options.
;;;; Russell A. Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Wed May 4 11:26:16 1988

(require 'utilities)			; For new-buffer-other-window
(provide 'hci-menus)

;;; INHIBIT-KEY-REMINDERS is a buffer-local variable set by the user
;;; to indicate whether or not the keystrokes that would invoke a menu
;;; option should be shown (if any exist).  Clearly this should be
;;; dynamically alterable, so the function that displays the menu can
;;; determine whether to recreate it or not.

(defvar inhibit-key-reminders nil
  "*If non-nil (default is nil) menus will not attempt to display key
bindings for options.")

(defun menu-p (lisp-object)
  "Return non-nil iff LISP-OBJECT is a menu."
  (and (listp (symbol-value lisp-object)) 	; A menu *might* have no items...
       (memq 'key-reminders (symbol-plist lisp-object))))

(defun prefix-arg-supplied (command)
  "Declare in a menu entry that COMMAND is called as if typed with a C-u prefix."
  (let ((current-prefix-arg (list 4)))
    (call-interactively command)))

(defun key-reminder (menu-item)
  "Return a list of the keybindings that would invoke MENU-ITEM,
if it could be called interactively, or the null string."
  (let ((option-string (car menu-item))
	(menu-form (cdr menu-item)))
    (if (and menu-form (listp menu-form))
	(let* ((prefix-arg-supplied-p nil)
	       (menu-form-car (car menu-form))
	       (command (cond ((eq menu-form-car 'call-interactively)
			       ;; function name will be quoted, so
			       (car (cdr (car (cdr menu-form)))))
			      ((eq menu-form-car 'prefix-arg-supplied)
			       (setq prefix-arg-supplied-p t)
			       (car (cdr (car (cdr menu-form)))))
			      (t (and (null (cdr menu-form))
				      menu-form-car)))))
	  (if (commandp command)
	      (if prefix-arg-supplied-p
		  (mapcar '(lambda (x) (concat "" x))
			  (where-is-internal command (current-local-map)))
		(where-is-internal command (current-local-map))))))))

(defun key-reminders (menu-items)
 "Apply key-reminder to each item in MENU-ITEMS and make a list of the result."
 (mapcar 'key-reminder menu-items))

(defmacro defHCImenu (menu &rest menu-items)
  "Define a new menu called MENU with (&rest) options MENU-ITEMS. 
Returns a symbol called MENU, which evaluates to the menu.
A MENU-ITEM is either
  (OPTION-NAME) : make this the name-stripe
  (OPTION-NAME command arg1 .. argn) : run command on args 1..n
  (OPTION-NAME . other-menu) : use other-menu as a pull-right menu

Puts a list of the keystrokes that will invoke each option on the 'key-reminders 
property of the menu name."
  (` (prog1
	 (setq (, menu) '(, menu-items))
       (put '(, menu) 'key-reminders (key-reminders '(, menu-items))))))

;;; No matter where we are, we have to be able to quit...

(defHCImenu emacs-quit-menu
  ("Suspend" suspend-emacs)
  ("Quit" save-buffers-kill-emacs))

;;; The ``...-description-menu'' menus give a one-line description
;;; of the type of buffer they will create, if selected.

(defHCImenu mail-mode-description-menu
  ("Mode for composing and sending electronic mail messages"
   mail))

(defHCImenu rmail-mode-description-menu
  ("Mode for reading and replying to electronic mail messages"
   rmail))

(defHCImenu dired-mode-description-menu
  ("Mode for directory organisation and housekeeping."
   call-interactively 'dired))

(defHCImenu fundamental-mode-description-menu
  ("Mode not specialized for anything in particular."
   new-buffer-other-window
   'fundamental-mode))

(defHCImenu text-mode-description-menu
  ("Mode for editing text intended for humans to read."
   new-buffer-other-window
   'text-mode
   ".text"))

(defHCImenu LaTeX-mode-description-menu
  ("Mode for editing files of input for LaTeX."
   new-buffer-other-window
   'LaTeX-mode
   ".tex"
   t))

(defHCImenu TeX-mode-description-menu
  ("Mode for editing files of input for TeX."
   new-buffer-other-window
   'TeX-mode
   ".tex"))

(defHCImenu nroff-mode-description-menu
  ("Mode for editing text intended for nroff to format."
   new-buffer-other-window
   'nroff-mode
   ".roff"))

(defHCImenu scribe-mode-description-menu
  ("Mode for editing text intended for Scribe to format."
   'scribe-mode
   ".mss"))

(defHCImenu emacs-lisp-mode-description-menu
  ("Mode for editing Lisp code to run in Emacs."
   new-buffer-other-window
   'emacs-lisp-mode
   ".el"))

(defHCImenu lisp-interaction-mode-description-menu
  ("Mode for typing and evaluating Emacs Lisp forms."
   new-buffer-other-window
   'lisp-interaction-mode))

(defHCImenu lisp-mode-description-menu
  ("Mode for editing code for Lisps other than Emacs Lisp."
   new-buffer-other-window
   'lisp-mode
   ".lisp"))

(defHCImenu modula2-mode-description-menu
  ("Mode for editing Modula-2 code."
   new-buffer-other-window
   'modula-2-mode
   ".mod"))

(defHCImenu scheme-mode-description-menu 
   ("Mode for editing scheme code."
   new-buffer-other-window
   'scheme-mode
   ".scm"))

(defHCImenu prolog-mode-description-menu
  ("Mode for editing Prolog code for Prologs."
   new-buffer-other-window
   'prolog-mode
   ".pl"))

(defHCImenu c-mode-description-menu
  ("Mode for editing C code."
   new-buffer-other-window
   'c-mode
   ".c"))

(defHCImenu fortran-mode-description-menu
  ("Mode for editing fortran code."
   new-buffer-other-window
   'fortran-mode
   ".f"))


;;; Emulator and subprocess description menus.

(defHCImenu shell-mode-description-menu
  ("Create an interactive UNIX shell buffer."
   shell))

(defHCImenu telnet-mode-description-menu
  ("Start a Telnet connection to some other machine."
   telnet))

(defHCImenu terminal-emulator-mode-description-menu
  ("Start a display terminal emulator."
   call-interactively 'terminal-emulator))

(defHCImenu vi-mode-description-menu
  ("Major mode that acts like the ``vi'' editor."
   vi-mode))


;;; Various ``amusement'' menus.

(defHCImenu doctor-description-menu
  ("Start the psychotherapist" doctor))

(defHCImenu hanoi-description-menu
  ("Solve the ``Towers of Hanoi'' problem visually"
   call-interactively 'hanoi))

(defHCImenu yow-description-menu
  ("Display a Zippy quotation" call-interactively 'yow))


;;; Help menus

(defHCImenu help-describe-menu
  ("bindings" describe-bindings)
  ("key" call-interactively 'describe-key)
  ("key briefly" call-interactively 'describe-key-briefly)
  ("function" call-interactively 'describe-function)
  ("variable" call-interactively 'describe-variable)
  ("mode" describe-mode)
  ("syntax" describe-syntax))

(defHCImenu help-menu
  ("Help Menu")
  ("Command apropos" call-interactively 'command-apropos)
  ("Describe" . help-describe-menu)
  ("View lossage" view-lossage)
  ("Info" info)
  ("Tutorial help" help-with-tutorial))


;;; Various submenus used in a variety of places.

(defHCImenu hyper-buffer-menu
  ("Emacs buffer menu" buffer-menu ())
  ("Electric buffer menu" electric-buffer-list ()))

(defHCImenu find-file-menu
  ("This window" call-interactively 'find-file)
  ("Other window" call-interactively 'find-file-other-window))

(defHCImenu file-menu
  ("Find file" . find-file-menu)
  ("Write file" call-interactively 'write-file))


;;; Top level heirarchy menus

(defHCImenu amusements-menu
  ("Doctor" . doctor-description-menu)
  ("Hanoi". hanoi-description-menu)
  ("Yow" . yow-description-menu))

(defHCImenu programming-language-modes-menu
  ("Lisp" . lisp-mode-description-menu)
  ("Emacs Lisp" . emacs-lisp-mode-description-menu)
  ("Scheme" . scheme-mode-description-menu) 
  ("PROLOG" . prolog-mode-description-menu)
  ("C" . c-mode-description-menu)
  ("FORTRAN" . fortran-mode-description-menu)
  ("Modula-2". modula2-mode-description-menu))  

(defHCImenu word-processing-modes-menu
  ("LaTeX" . LaTeX-mode-description-menu)
  ("TeX" . TeX-mode-description-menu)
  ("Nroff" . nroff-mode-description-menu)
  ("Scribe" . scribe-mode-description-menu)
  ("Text" . text-mode-description-menu))

(defHCImenu miscellaneous-modes-menu
  ("Rmail" . rmail-mode-description-menu)
  ("Mail" . mail-mode-description-menu)
  ("Dired" . dired-mode-description-menu)
  ("Shell" . shell-mode-description-menu)
  ("Telnet" . telnet-mode-description-menu)
  ("Terminal Emulator" . terminal-emulator-mode-description-menu)
  ("Vi Emulator" . vi-mode-description-menu)
  ("Amusements" . amusements-menu)
  ("Lisp Interaction" . lisp-interaction-mode-description-menu)
  ("Fundamental" . fundamental-mode-description-menu))

(defHCImenu new-buffer-mode-menu
  ;; This is a menu of all possible available major modes for a new
  ;; buffer.  It allows the user to see a description of each one, and
  ;; creates a new buffer in the mode eventually chosen.
  ("Available Major Modes for")
  ("programming languages".  programming-language-modes-menu)
  ("word processing" . word-processing-modes-menu)
  ("miscellaneous operations" . miscellaneous-modes-menu))

(defHCImenu emacs-menu
  ("Emacs Menu")
  ("Undo" call-interactively 'undo)
  ("Help menu" . help-menu)
  ("Find file" . find-file-menu)
  ("New buffer" . new-buffer-mode-menu)
  ("Quit" . emacs-quit-menu))

;; This is the ``Other menus'' menu that allows access to all the
;; menus from the mode-specific menus installed in various major
;; modes.

(defHCImenu other-menus-menu	
  ("Emacs menu" . emacs-menu)
  ("Help menu" . help-menu)
  ("File menu" . file-menu)
  ("Buffer menu" . hyper-buffer-menu))

 

