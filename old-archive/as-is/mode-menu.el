;Return-Path: <@relay.cs.net:davis@scr.slb.com>
;Date: Tue, 25 Apr 89 09:31 EST
;From: Paul Davis <davis@scr.slb.com>
;Subject: modeline menus for Emacstool
;To: INFO-GNU-EMACS@prep.ai.mit.edu
;X-Vms-To: prep.ai.mit.edu::info-gnu-emacs
;
;
;
;This one is a real ugly brute. Frustrated by the speed of popup menus
;in Emacstool, and wanting to provide some Mac-like aspects to some of
;Emacs' major modes, I set about writing some stuff to provide `menus'
;not unlike the Mac's frame menu, but appearing on the modeline.
;There's a necessary evil here in that you end up losing the regular
;information shown here, but for certain things its proved a godsend
;(particularly LaTeX mode).
;
;I remain unconvinced that this is a good thing to do, let alone a good
;way to do it, but other people may have fun or be able to improve it.
;Perhaps some fool may even port it to version 19's X interface :-( 
;Generally, I prefer a command-key interface for speed, but menus can
;be a good source of education, and can help novice users get going.
;
;The set-up for a particular mode is a bit ugly, and regrettably, most
;menu defs have to be of one of two dreadful forms:
;
;	(button-name (eval-in-window window (button-function)))
;or
;	(sub-menu-name (sun-menu-evaluate window x y sub-menu))
;
;The first defines a `button' that executes `button function' directly;
;the second pops up sub-menu to select other functions or sub-sub-menus etc.
;
;Anyway, I enclose a few files: the elisp code itself, followed by *some*
;bits of a LaTeX interface, and a hook for LaTeX mode at the end.
;
;Don't flame me on this - I'm posting it in the event that other people
;might find something useful in it. If people want the rest of the
;LaTeX stuff, I'll mail it to them individually.
;
;Paul
;                             Paul Davis at Schlumberger Cambridge Research
;                                <davis%scrsu1%sdr.slb.com@relay.cs.net>
;
;                              "to shatter tradition makes us feel free ..."
;
;=====> modeline-menus.el
;; Modeline menus for Emacstool
;; Paul Davis <davis@blue.sdr.slb.com> April 1988

;; This file is a subject to the conditions of the GNU Emacs general
;; public license.

;; A modeline menu is displayed as a series of labels in the
;; modeline of a buffer. Clicking with a mouse button
;; bound to the function mouse-eval-modeline-event will result
;; in the evaluation of the expression associated with that
;; label.

;; A Modeline menu is simply specified as a list where each
;; item of the list has a label (a string) as its car, and
;; a list which eval's to a function as its cdr.
;; For example,

;; (setq foobar-modeline-menu '(("FOO" (do-my-foo-function t 3 nil))
;;                              ("BAR" (save-buffer 1))))

;; defines a two button menu with labels "FOO"
;; and "BAR". Clicking the appropriately bound mouse button
;; on the label "FOO" will result in the evaluation of the
;; the cdr of this item (do-my-foo...... etc).

;; The general form of a mode-setup hook to use a modeline
;; menu is like this,
;;
;;        (setq foo-mode-hook '(lambda ()
;;                                (require 'modeline-menus)
;;                                (setup-modeline-menu foo-mode-menu)
;;                                (setup-foo-modeline)
;;                                (local-set-mouse '(modeline right) 
;;                                                 'mouse-eval-modeline-event)
;;                   
;; the modeline menu should be defined before hand, as should the function
;; setup-foo-modeline, which might look something like this:
;;
;;       (defun setup-foo-modeline ()
;;          (setq-mode-line-format (concat "my foo stuff " 
;;                                          modeline-menu-string))
;;          (setq modeline-menu-offset (+ 13 (length modeline-menu-string))))
;;
;; where the magic number 13 is simply the number of chars
;; in the string "my foo stuff " (ie; the right shift of the
;; start of the modeline menu from the left edge of the modeline in chars).
;;
;; This is basically a kludge. If I was using X right now, it would be
;; possible to have *real* buttons, and thats probably true of SunView
;; too, but in the latter case, I can't be bothered to learn how. It all
;; started because I wanted a mailtool-like interface for Rmail. Have Fun....

;; ----------------------------------------------------------------------------------


(defvar modeline-menu nil
  "List structure for buffer modeline menu. Buffer
local.")

(defvar modeline-menu-string nil
  "String showing menu \"buttons\" in modeline. Buffer
local.")

(defvar modeline-menu-offset nil
  "Initial rightwards shift (chars) in modeline caused by
adding extra information to the left of the modeline-menu-string.
Should be set by any mode-setup hook so that the mouse-event can
be properly located. Buffer local")

(make-variable-buffer-local 'modeline-menu-string)
(make-variable-buffer-local 'modeline-menu-offset)
(make-variable-buffer-local 'modeline-menu)

(defun mouse-eval-modeline-event (window x y)
  "Mouse driven eval-modeline-event - the only form possible."
  (eval-in-window window (eval-modeline-event modeline-menu window x)))

(defun eval-modeline-event (menu window mouse-event-x)
  "
Identify and evaluate in WINDOW the item of MENU labelled by string in
the current modeline located near MOUSE-EVENT-X."

;; the double eval is a horrible kludge - (eval-in-window window (cadar menu)
;; seems to do nothing at all ... I don't understand lisp macros well enough
;; to figure out whats going on, so it remains ...

  (cond ((null menu) (message "No modeline menu defined here")) 
	((< mouse-event-x (+ (caar menu) modeline-menu-offset))
	 (eval (cadar menu)))
	(t
	 (eval-modeline-event (cdr menu) window mouse-event-x))))

(defun setup-modeline-menu (menu)
  "Process a modeline menu and set the buffer-local
value of modeline-menu-string"
  (make-modeline-string menu)
  (define-and-index-menu menu))

(defun make-modeline-string (menu)
  "Create a string containing the label for each item in MENU"
  (setq modeline-menu-string nil)
    (while (not (null menu))
      (setq modeline-menu-string (concat modeline-menu-string (car (car menu)) " "))
      (setq menu (cdr menu))))

(defun define-and-index-menu (menu)
  "Set the car of each menu item to be the offset
of its label in the modeline-menu-string. We have to copy
the menu, rather than process it directly because make-modeline-string
requires a menu with strings as the car's not integers. Messy."
  (setq local-menu (copy-alist menu))            ;; copy the string-based menu
  (setq modeline-menu local-menu)                ;; top of new menu
  (setq running-offset 0)                        
  (while (not (null local-menu))
    (setcar (car local-menu) (setq running-offset 
				   (1+ (+ running-offset 
					       (length (caar local-menu))))))
    (setq local-menu (cdr local-menu))))
  
;; Common Lisp utilities

(defun caar (list)
  (car (car list)))

(defun cadr (list)
  (car (cdr list)))

(defun cddr (list)
  (cdr (cdr list)))

(defun cdar (list)
  (cdr (car list)))

(defun cadar (list)
  (car (cdr (car list))))

(provide 'modeline-menus)

=====> a simple hint on using modeline menus
=====> from "electric-latex.el"

(require 'modeline-menus)

;; the modeline menu itself.

(setq latex-modeline-menu 
'(("Process" (sun-menu-evaluate window x y latex-process-menu))
  ("Preview" (sun-menu-evaluate window x y latex-preview-menu))
  ("Print" (sun-menu-evaluate window x y latex-print-menu))
  ("documentstyle" (sun-menu-evaluate window x y latex-docstyle-menu))
  ("Environments" (sun-menu-evaluate window x y latex-environment-menu))
  ("Fonts" (sun-menu-evaluate window x y latex-font-menu))
  ("Parameters" (sun-menu-evaluate window x y latex-parameters-menu))
  ("Check" (call-interactively 'validate-TeX-buffer))
  ("Help" (sun-menu-evaluate window x y latex-help-menu))))

;; set up function for the modeline

(defun setup-latex-modeline ()
  (local-set-mouse '(modeline right) 'mouse-eval-modeline-event)
  (setq mode-line-format 
	(list (concat "LaTeX: [%b] " modeline-menu-string)))
  (setq modeline-menu-offset (+ 10 (length (buffer-name)))))

====> my latex-mode hook

(setq LaTeX-mode-hook '(lambda ()
			 (require 'electric-latex)
			 (setup-modeline-menu latex-modeline-menu)
			 (setup-latex-modeline)))

