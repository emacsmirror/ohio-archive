;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : simple-menu.el
;;;; Author          : Frank Ritter
;;;; Created On      : Mon Oct 28 12:28:03 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sat Nov 21 23:19:33 1992
;;;; Update Count    : 54
;;;; 
;;;; PURPOSE
;;;; Simple Menu Enhancements for GNU Emacs
;;;; I've completely rewritten the Chris Ward's menu system to suit my
;;;; needs.  It is a simple tty based menu system for providing a limited
;;;; number of choices in an extensible way.  I use it daily (well, not
;;;; really, I now use the keystroke equivalents it teaches), but the point
;;;; is that it is robust enough to put out.  I have cut most of Chris's 
;;;; emacs commands from the menus, the present package is offered more for
;;;; applications, but I would be happy to paste stuff people send me.  At
;;;; the bottom of this file I provide a sample set of menus for emacs.
;;;; 	
;;;; TABLE OF CONTENTS
;;;; 	i.	LCD archive entry
;;;;	ii.	COPYRIGHT and WARNINGS
;;;;	iii.	Update information and how to get copies
;;;;	iv.	OVERVIEW/INTRODUCTION
;;;;	v.	Requires/provides/compile info
;;;;
;;;; 	I.	Variables and constants 
;;;; 	II.	Creating functions
;;;;	III.	Running functions
;;;; 	IV. 	Helper functions 
;;;; 	V.	Utilities
;;;;	VI.	Example menus for emacs
;;;; 
;;;; Copyright 1991, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Status          : Released version 1.2.
;;; HISTORY
;;; 8-10-93 - FER  fixed sm-read-char-from-minibuffer to read a return f/ 19.
;;; 18-Nov-92 -DMS Simplified (and generalized) sm-find-binding, which
;;;           now gives bindings in the same form as C-h w. Item selection
;;;           is now done with the minibuffer instead of 'read-char, so
;;;           you can scroll the help window, for example. Help pop-ups
;;;           use 'with-output-to-temp-buffer, so works well with the
;;;           popper package. If no default is specified, the last selection
;;;           is provided as the default. def-menu now works like defvar,
;;;           i.e. does nothing if the symbol is already bound. sm-clear-menu 
;;;           also makes its symbol unbound (so def-menu will then work).
;;; Version 1.3 (not yet released)
;;;  9-Sep-92 -FER no longer saves menu help, found to be buffer sensative.
;;;  9-Sep-92 -FER fixed bug if given nil commands.
;;; 13-Jul-92 -FER replaced CR with \n
;;; 18-Mar-92 -FER fixed default usage, added bytecomp information.
;;; 12-Feb-92 -FER more robust in allowing user to move in pop-up help.
;;; 11-Feb-92 -FER added optional default to running a menu.
;;; 13-Jan-92 -FER added sm-clear-menu, and now run-menu returns values
;;; 19-Nov-91 -FER added variable prompts
;;;                f/ Christopher fernand@SPUNKY.CS.NYU.EDU
;;; 28-Oct-91 release 1.2 to elisp archive  -FER
;;;  3-oct-91 -FER TAB and M- replace "   " and ^[ in full help descriptions.
;;; 16-Sep-91 -FER better help display
;;; 6-12-91 - unbelievably better key search in sm-find-binding
;;; 6-11-91 - even more robust key search in sm-find-binding
;;; 6-10-91 - more robust key search in sm-find-binding
;;; Version 1.1
;;; 6-5-91 - added ability to show esc-x commands in help
;;; 5-27-91 - added ability to show esc-x commands after command completion
;;; 2 may 91 added (require 'cl) reported by dfreuden@govt.shearson.com,
;;;   ne201ph@prism.gatech.edu (Halvorson,Peter J), rayv@revenge.sps.mot.com 
;;;   (Ray Voith), & Sara.Kalvala@computer-lab.cambridge.ac.uk
;;; 30 may 91 - posted to gnu.emacs.sources version 1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TO DO:
;;;  * Select menu items by the first *capital* letter in the menu
;;;    label (e.g. select "About" with `a' but "aTtach" with `t')
;;;  * Introduce some means of coping with long prompts, e.g. make
;;;    minibuffer taller, or display options in another window with
;;;    a short prompt in minibuffer



;;;
;;; 	i.	LCD archive entry
;;;

;; LCD Archive Entry:
;; simple-menu|Frank Ritter|ritter@cs.cmu.edu
;; |Command-line menus made declaratively (rev of Ward's procedural version)
;; |91-10-28|1.2|~/interfaces/simple-menu2.el.Z



;;;
;;;	ii.	 COPYRIGHT and WARNINGS
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.


;;;
;;;	iii.	Update information and how to get copies
;;;
;;; Updated versions (if any) are available from the author or via ftp:
;;; from the elisp archive on archive.cis.ohio-state.edu as file
;;;  pub/gnu/emacs/elisp-archive/interfaces/simple-menu2.el.Z
;;; Iff you post me mail that you use it, I'll post you updates when they 
;;; come out.
;;;
;;; Initially based on code posted by Chris Ward.
;;;        Texas Instruments 
;;;        (cward@houston.sc.ti.com)       (214) 575-3128
;;;        (X.400: /ADMD=MCI/PRMD=TI/C=US/G=Chris/S=Ward MCI_Mail_ID #4418566)
;;; and posted comments on Chris's code by Erik Hennum (Erik@informix.com)


;;;
;;;	iv.	OVERVIEW/INTRODUCTION
;;; 
;;; Simple-menu is a way to provide simple menus, rather reminiscent of
;;; the menus provided by the PDP software of McClellend & Rumelhart.  With
;;; the simple menus defined here for gnu-emacs, an initial menu of
;;; commands is displayed in the message line by calling run-menu on a
;;; previously defined menu.  The user types the first letter of an item to 
;;; select it, and a command gets executed, or a sub-menu is entered.
;;; Often you will bind the top menu call to a key.
;;;
;;; The prompt that is displayed includes a reminder that help is available  
;;; by typing ``?''.  (Help is also available by typing ^h or SPC.)
;;; The prompt can be a string (which will get a ":" tacked on to it),
;;; a list that will get evaled, a variable that will get evaled, or a
;;; function that will get funcalled.
;;;
;;; Simple menus are defined with def-menu.  This takes a menu-name, an
;;; title, an intro help comment (ie.: "Pick a command"), and a list of
;;; items to be put on the menu.  Each  menu item is a list with 2 
;;; components: 1) a display string, and 2) the command corresponding 
;;; to the string.  The first word is put in the menu, the first letter in
;;; the string is used as the selector for the item (case insensitive),
;;; and the whole string is used in the help display.  
;;; Def-menu and sm-add-to-menu allow you add commands to menus after they have
;;; been created, and sm-clear-menu lets you start from scratch.
;;;
;;; For example, the menu item:
;;; 
;;; ("Redraw         Redraw the screen."   recenter)
;;; 
;;; would create the item Redraw in the menu, and the letter R would
;;; select it.  In the help display, the full string would appear, along
;;; with any keybindings for the command in the local buffer, in this case
;;; the help line would look like 
;;; 
;;; Redraw         Redraw the screen. (C-l)
;;; 
;;; The command given as the second argument can be either: 1) a simple
;;; function name, 2) a list to eval, or 3) a menu name (symbol).  If you
;;; want two commands there, wrap them in a progn because the internals of
;;; the program use each list position.  The command should not display 
;;; a value with message as its result.
;;;
;;; If there is only one menu item, it is executed when the menu is run.
;;; After an item is selected and sucessfully completed, a possible keybinding
;;; or call via meta-X is displayed if possible.
;;;
;;;  Here's an example:
;;; 
;;; (def-menu 'simple-menu
;;;   "Choose a simple command"
;;;   "Here are some simple commands to choose from"
;;; '(("Add 2 + 2      Add 2+2 and print it out for me."
;;;    (progn (message (format "The Answer is %s." (+ 2 2)))
;;;           (sleep-for 2)))
;;;   ("Redraw         Redraw the screen." recenter)
;;;   ("Simple menu    Recurse and run this darn menu again." simple-menu)))
;;; 
;;; Run-menu will start up the menu.  ^g will abort the menu.
;;; e.g.,  (run-menu 'simple-menu)
;;; Binding this to a key makes the menu more usable.
;;; Run-menu also takes a default, a string or symbol.  If the user 
;;; types a CR, the first letter of the string or symbol's name is 
;;; used to make the choice.
;;; 
;;; (add-to-menu 

;;; I will NOT maintain it in the traditional sense (mostly a note to myself to
;;; get back to the thesis), but I will 1) incorporate changes that are
;;; useful to me, 2) fix bugs that you notice that would bother my
;;; application, and 3) incorporate good stuff you post me.
;;; 
;;; I am willing to answer questions if things aren't clear on how to get
;;; started.  
;;; 
;;; possible bugs/misfeatures:
;;; * The command should not display a value with message as its result.


;;;
;;;	v.	Requires/provides/compile info
;;;

(require 'cl)
(provide 'simple-menu)

;; Compiler info for JWZ's byte compiler.
;; they add about 2k to the .elc file.
(if (fboundp 'proclaim-inline)
  (proclaim-inline
    sm-menu-p
    sm-eval-single-menu
    sm-make-help
    sm-find-binding
    sm-menu-ized-items
    sm-setup-menu-item
    sm-note-function-key
    first-word
    first-letter
))

(defmacro mapc (function alist)
 (` (let ((blist (, alist)))
     (while blist
      (funcall (, function) (car blist))
      (setq blist (cdr blist))    ))))


;;; 
;;; 	I.	Variables and constants 
;;;

(defvar sm-default-function 'sm-cant-do-this
  "*Default function to call if a menu item doesn't have a function 
assigned to it.")

;; uses main help buffer, used to be *MENU Help*
(defconst help-buffer "*Simple-Menu-Help*")

(defconst simple-menu-help-string "? ")
(defconst simple-menu-default-string "[%s]:")

(defconst sm-default-help-header "Commands in the")
(defconst sm-default-help-for-help 
  "? or C-h or SPC to display this text at the first prompt.")

(defconst sm-default-help-footer
" First letter of the line to choose a command.  
 RET selects the item in [] (if any).
 * - menu item not functional yet.   . - more information pompted for.
 / - item leads to another menu.     ESC to quit this menu now.")

(defconst simple-menu-version "1.3+")


; menus have the following fields:
;  prompt - the string used as the prompt before the choices
;  full-prompt - the string put in the message line
;  items - the list of items
;  prompt-header  - header (leading string) for the command line
;  help-header - header for the help buffer


;;;
;;; 	II.	Creating functions
;;;
;; menus are symbols, 
;; the raw items are stored under the plist 'items
;; the list that is displayed is stored in their value, 
;;    it is made by calling sm-menu-ized-items on the items, 
;; the prompt-header is under the 'prompt-header property
;; the help-header   is under the 'help-header prop.

(defun sm-menu-p (poss-menu)
 "Return t if item is a simple-menu."
 (and (boundp poss-menu)
      poss-menu
      (get poss-menu 'items)
      (get poss-menu 'prompt-header)
      (get poss-menu 'help-header)
      t))

(defun sm-def-menu (name prompt help-header items)
 "Define a menu object"
 ;; check for errors on the way in and massage args
 (if (not (symbolp name)) 
     (error (format "%s, the first arg must be a symbol." name)))
 (if (boundp name) nil
   (put name 'items items)               ; doit
   (set name (sm-menu-ized-items items))
   (put name 'prompt-header prompt)
   (put name 'help-header help-header)))

(fset 'def-menu 'sm-def-menu)

;; Could set here whether items go on front or back.
(defun sm-add-to-menu (menu items)
 "Add to NAME the list of ITEMS."
 (mapc (function
           (lambda (item)
             (let ( (old-items (get menu 'items)) )
               (cond ( (member item old-items) )
                     (t (put menu 'items (append old-items items))
                        (set menu (sm-menu-ized-items (get menu 'items)))
                        (put menu 'full-prompt nil)))  )))
          items))

(fset 'add-to-menu 'sm-add-to-menu)


;;;
;;;	III.	Running functions
;;;
;;; The cursor-in-echo-area doesn't work on pmaxen with X windows,
;;; we don't know why.

(defun sm-for (from to fun)
  "for x=TO to FROM (FUN x). TO and FROM are ints, FUN is a symbol"
  (let ((x (1- from)))
    (while (<= (setq x (1+ x)) to)
      (funcall fun x))))

(setq sm-select-item-map (make-keymap))
(suppress-keymap sm-select-item-map)
;; Set letters and digits to return from minibuffer
(let ((set-key '(lambda (k) 
		 (define-key
		   sm-select-item-map
		   (char-to-string k)
		   'exit-minibuffer))))
  (sm-for ?a ?z set-key)
  (sm-for ?A ?Z set-key)
  (sm-for ?0 ?9 set-key)
  (funcall set-key ?\e)			; should abort
  (funcall set-key ?\C-g)		; should abort
  (funcall set-key ?\r)			; should accept default
  (funcall set-key ?\n)			; should accept default
  (funcall set-key ?\t)			; should accept default
  (define-key sm-select-item-map "?" 'sm-pop-up-help)
  (define-key sm-select-item-map " " 'sm-pop-up-help)
  (define-key sm-select-item-map "\C-h" 'sm-pop-up-help))

(defun sm-read-char-from-minibuffer (prompt)
  (read-from-minibuffer prompt nil sm-select-item-map)
  (if (or (eq last-input-char ?\e) (eq last-input-char ?\C-g))
      'abort
    (if (or ;; for emacs 18.*
            (eq last-input-char ?\r) (eq last-input-char ?\n)
	    (eq last-input-char ?\t)
            (eq last-input-char 'return))
	'default
      last-input-char)))

(defvar sm-current-menu nil
  ;; Needed so sm-pop-up-help can find the right documentation
  "The current menu being run.")

(defvar sm-current-buffer nil
  ;; Needed so sm-find-binding can use the right keymap
  "Name of the buffer from which run-menu was called.")

;; prompt is the initial prompt
;; full prompt is what is actually shown to the user, includes choices
(defun sm-run-menu (amenu &optional default)
 "Present AMENU.  DEFAULT will be selected on a CR."
 ;; get & present the prompt
 (if (= (length (eval amenu)) 1)
     (sm-eval-single-menu amenu)
 (let* ((raw-prompt (get amenu 'prompt-header))
       (full-prompt (get amenu 'full-prompt))
       (old-window (selected-window))
       (items (eval amenu))
       (the-prompt)
       (last-selection (get amenu 'last-selection))
       (string-default (cond ((stringp default) default)
                             ((and default (symbolp default))
                              (prin1-to-string default))
			     (last-selection
			      (setq default t)
			      (char-to-string last-selection))
                             (t "")))    )
  (if full-prompt
      (setq the-prompt (format full-prompt string-default))
      (progn
        ;; this makes a full prompt, & saves it for later use
       (setq prompt (cond ((listp raw-prompt)
                           (eval raw-prompt))
                          ((and (symbolp raw-prompt) (fboundp raw-prompt))
                           (funcall raw-prompt))
                          ((and (symbolp raw-prompt) (boundp raw-prompt))
                           (eval raw-prompt))
                          ((stringp raw-prompt)
                           (if (not (string= raw-prompt ""))
                               (concat raw-prompt ": ")
                             raw-prompt))))
        (mapc (function (lambda (x) (setq prompt (concat prompt x " "))))
              (mapcar 'first-word items))
        (setq prompt (concat prompt simple-menu-help-string
                             simple-menu-default-string))
        (if (stringp raw-prompt)
            (put amenu 'full-prompt prompt))
        (setq the-prompt (format prompt string-default))))
  ;; read it in & process char choice
  (if sm-current-menu
      (error "A menu is already running!"))
  (unwind-protect
      (progn
	(setq sm-current-menu amenu)
	(setq sm-current-buffer (current-buffer))
	(setq opt (sm-read-char-from-minibuffer the-prompt)))
    (setq sm-current-menu nil))
  (if (eq opt 'abort) 
      (message "Aborted.")
    (if (eq opt 'default)
	(if default
	    (setq opt (string-to-char string-default))
	  (message "No default - no action taken.")))
    (if (eq opt 'default) nil		; Message was given
      (setq opt (downcase opt))
      (sm-eval-menu amenu opt))))))

(fset 'run-menu 'sm-run-menu)


;;;
;;; 	IV. 	Helper function s
;;; 

(defun sm-clear-menu (name)
 "Completely clears out a menu."
 (put name 'items nil)
 (set name nil)
 (put name 'prompt-header nil)
 (put name 'raw-prompt nil)
 (put name 'full-prompt nil)
 (put name 'help-prompt nil)
 (put name 'last-selection nil)
 (makunbound name))

(defun sm-eval-menu (amenu opt)
 "Find in AMENU the command corresponding to OPT."
 (let ( (items (eval amenu)) results
        (current-key-map (current-local-map))
        (command nil) )
  (while items
     (setq item (pop items))
     (cond ( (and (null (third item))
                  (= opt (second item)))
             (setq command t)
             (error "Menu item \"%c\" not implemented yet." opt))
           ( (and (third item) (= opt (third item)))
             (setq items nil)
             (setq command (second item))
	     (put amenu 'last-selection opt)
             (setq results 
               (cond ;; something to be returned
                     ((or (stringp command) (numberp command))
                      command)
                     ;; its a command
                     ((and (not (listp command)) (fboundp command))
                      (call-interactively command)
                      (sm-note-function-key command current-key-map))
                     ;; it is something to eval
                     ((listp command)
                      (eval command))
                     ((or (not (boundp command)) (not (eval command))
                          (not (listp (eval command))))
                      command)
                     ;; it is another menu, you hope...
                     (t (sm-run-menu command)))))))
  (if (not command) ; no match
      (progn (message (format "%c did not match a menu name" opt))
             (beep))) ; note that we lost
  results))

(defun sm-eval-single-menu (amenu)
 "Run in AMENU the single only command."
 (let* ( (item (first (eval amenu)))
         (command (second item)) 
         (current-key-map (current-local-map)) )
   (cond ;; its a command
        ((and (not (listp command)) 
              (fboundp command))
         (call-interactively command)
         (sm-note-function-key command current-key-map))
        ;; it is something to eval
        ((listp command)
         (eval command))
        ;; it is another menu, you hope...
        (t (sm-run-menu command)))
   (if (not command) ; no match
       (progn (message (format "%c did not match a menu name" opt))
              (beep)))     ;note we lost
))

(defun sm-make-help (help-header name items)
 "Make a help string for a simple menu."
 ;; this is a bit sloppy about how to make it....
 (let ((header nil) (result ""))
  (setq result
        (concat result
               (cond ((string= "" help-header)
                      (format "%s %s:%s\n\n" sm-default-help-header name))
                     (t (concat help-header ":\n\n")))))
  (mapc
     (function 
       (lambda (x)
          (let ((bind-thing (sm-find-binding (car (cdr x))))
                (help-string (car x)) )
           (setq result (format "%s %s " result help-string))
           (if bind-thing
               (if (> (+ (length bind-thing) (length help-string)) fill-column)
                   (setq result 
                         (format "%s\n\t\t\t (%s)" result bind-thing))
                   (setq result 
                         (format "%s (%s)" result bind-thing))))
           (setq result (concat result "\n"))           )))
      items)
  (setq result (concat result "\n " sm-default-help-for-help ))
  (setq result (concat result "\n " sm-default-help-footer))
  result))

(defun sm-find-binding (function &optional map)
  (if (and function (not (symbolp function)))
      nil
    (if (sm-menu-p function) nil
      (if (not map)
	  (setq map 
		(save-excursion
		  (set-buffer (or sm-current-buffer (current-buffer)))
		  (current-local-map))))
      (substitute-command-keys
       (concat "\\<map>\\[" (symbol-name function) "\]")))))

(defun sm-menu-ized-items (items)
 "Strips the first letter off and makes it the third item for ease and speed."
 (mapcar (function (lambda (x)
            (append (sm-setup-menu-item x)
                    (list (string-to-char (first-letter x))))))
         items))

(defun sm-setup-menu-item (x)
 "Setup the menu item X, which should have a string and symbol or listp.
If it doesn't, add a dummy function call."
 (let ((value (car (cdr x))))
 (cond ( (and (listp x)
              (stringp (car x))
              (or (symbolp value)
                  (stringp value)
                  (listp value)))
          x)
       ( (and (listp x)         ;given a null function
              (stringp (car x))
              (null (car (cdr x))))
         (append x (list sm-default-function)))
       (t (error "Bad menu item: %s" x)))
))

(defun sm-pop-up-help ()
  "Display help for the current menu."
  (interactive)
  (let ((menu sm-current-menu))
    (if menu nil
      (error "No menu running!"))
    (with-output-to-temp-buffer help-buffer
      (princ (cond ((put menu 'help 
			 (sm-make-help (get menu 'help-header)
				       menu
				       (get menu 'items))))
		   ((get menu 'help))
		   ;; these had been switched to save cycles, but
		   ;; it looks like some menus are buffer sensative
		   (t "not documented"))))))

(defun sm-note-function-key (command keymap)
 "Note to the user any keybindings for Command"
 (let ( (key-binding (sm-find-binding command keymap)) )
  (if key-binding
      (message (format "%s is also bound to \"%s\"."
	                command key-binding))) ))


;;;
;;; 	V.	Utilities
;;; 

;; (first-word '("asdf" fun1))
;; (first-letter '("Asdf" fun1))

(defun sm-cant-do-this ()
  (message "No function to do this menu item yet."))

(defun first-word (menu-item)
 "Return the first word of the first part (a string) of MENU-ITEM."
 (let ((string  (car menu-item)))
  (substring string 0 (string-match " " string))))

(defun first-letter (menu-item)
 "Return the first letter of the first part (a string) of MENU-ITEM."
 (let ((string  (first-word menu-item)))
    (downcase (substring string 0 1))))


;;;
;;;	VI.	Example menus for emacs
;;;
(get 'emacs-menu 'prompt-header)

(concat "prompt" simple-menu-help-string
                             simple-menu-default-string)

(def-menu 'emacs-menu
  "Emacs commands"
  "Menu of plain Emacs commands"
 '(("Windows      Manipulate multiple window settings."   emacs-windows-menu)
   ("Modify       Change your editing environment."       emacs-modify-menu)
   ("Block menu   Perform operations on blocks (regions) of text." emacs-block-menu)
))

(def-menu  'emacs-block-menu
  "Block Option"
  "Displays menu of block commands to chose from"
 '(("Align    Adjust all lines in region Left, Right, or Centered." 
        emacs-align-menu)
  ("Eval     Evaluate region as a Lisp expression."           eval-region)
  ("Fill     Fill each paragraph in the region."              fill-region)
  ("Indent   Indent region according to major mode."          indent-region)
  ("Lower    Convert all characters in region to lowercase."  downcase-region)
  ("Narrow   Narrow scope of edit to region."                 narrow-to-region)
  ("Spell    Check spelling of all words in region."          spell-region)
  ("Upcase   Convert all characters in region to uppercase."  upcase-region)
  ))

(def-menu 'emacs-modify-menu
  "Modify Option"
  "Modify editing environment options are"
 '(("Keys     Locally rebind a key to a function."      local-set-key)
   ("Mode     Change current major/minor mode."         emacs-mode-menu)
   ("Options  Change environmental variable values."    (edit-options))
   ("Save     Save current options settings to a file."
             (message "Modify Save not implemented yet."))
   ("Tabs     Modify tab stop locations."               edit-tab-stops))  )

(def-menu 'emacs-windows-menu
  ""
  "Displays menu of window commands to chose from"
 '(("Buffers  Change to buffers menu."                       emacs-buffer-menu) 
  ("Compare  Compare text in current window with text in next window."
    compare-windows)  
  ("Delete   Remove current window from the display."               delete-window)
  ("Find     Find another buffer and change current window to it."  select-window)
  ("Split    Divide current window Vertically or Horizontally."
   (progn
    (while (not (or (= opt ?h) (= opt ?v)))
      (message "Split window: Horizontally Vertically ")
      (setq opt (downcase (read-char))))
    (if (= opt ?h) 
        (call-interactively 'split-window-horizontally)
        (call-interactively 'split-window-vertically))   ))
  ("Other    Change to next window."                      other-window)
  ("1        Make current window the only one visible."   (delete-other-windows))
  ("+        Increase lines in current window."       (do-window-sizing ?+))
  ("-        Decrease lines in current window."       (do-window-sizing ?-))
  ("<        Increase columns in current window."     (do-window-sizing ?<))
  (">        Decrease columns in current window."     (do-window-sizing ?>))))

(defun do-window-sizing (opt)
 (while (or (= opt ?+) (= opt ?-) (= opt ?>) (= opt ?<))
   (message "Change window size press '+', '-', '<', '>', or space to quit.")
   (if (= opt ?+) (enlarge-window 1))
   (if (= opt ?-) (shrink-window 1))
   (if (= opt ?>) (enlarge-window-horizontally 1))
   (if (= opt ?<) (shrink-window-horizontally 1))
   (setq opt (read-char))))

(def-menu 'emacs-buffer-menu
  ""
  "Displays menu of buffer commands to chose from"
 '(("Delete   Kill current buffer."               kill-buffer)
  ("Edit     Edit another buffer."               switch-to-buffer)
  ("File     Change to use File menu."           files-menu)
  ("List     List current buffers and status."   list-buffers)
  ("Other    Switch to buffer in other window."  switch-to-buffer-other-window)
  ("Spell    Check spelling for current buffer." ispell-buffer)
  ("Toggle   Toggle current buffer read-only status." toggle-read-only)
  ("Window   Change to Windows menu."                 windows-menu)))

(def-menu 'emacs-mode-menu
  "Mode"
  "Displays menu of known major and minor modes to chose from"
 '(("1  [pfe-mode] Use PFE emulation and keyboard layout."   (pfe-mode))
  ("A  [edit-abbrevs-mode] Major mode for editing list of abbrev definitions."
     (edit-abbrevs-mode))
  ("C  [c-mode] Major mode for editing C language source files."   (c-mode))
  ("D  [normal-mode] Default to normal mode for current file."  (normal-mode))
  ("F  [fortran-mode] Major mode for editing FORTRAN source files."  
    (fortran-mode))
  ("G  [emacs-lisp-mode] Major mode for editing GNU Emacs lisp source files."
     (emacs-lisp-mode))
  ("I  [lisp-interaction-mode] Major mode for typing/evaluating Lisp forms."
     (lisp-interaction-mode))
  ("L  [lisp-mode] Major mode for editing LISP code other than Emacs Lisp."
    (lisp-mode))
  ("O  [outline-mode] Major mode for editing outlines with selective display."
     (outline-mode))
  ("P  [picture-mode] Use quarter-plane screen model to edit."  (picture-mode))
  ("T  [text-mode] Major mode for editing regular text files." (text-mode))
  ("X  [tex-mode] Major mode for editing files of input for TeX or LaTeX."
     (tex-mode))
  ("Z  [fundamental-mode] Major mode not specialized for anything."
    (fundamental-mode))))

(def-menu 'emacs-align-menu
  "Align Option"
  "Displays menu of region alignment commands to chose from:"
 '(("Center   Center all lines in region between left margin and fill column."
     center-region)
  ("Justify  Fill each paragraph between left margin and fill column."
     (fill-region (point) (mark) t))
  ("Left     Adjust lines to start in a specific column."
    (progn (setq opt 
                 (read-input "Align left at column: " (int-to-string left-margin)))
           (setq opt (string-to-int opt))
           (message (format "Align left at column %d." opt))
           (indent-rigidly (point) (mark) opt)))
  ("Right    Ajdust lines to end in a specific column if possible."
     (progn (setq opt (read-input "Align right at column: " 
                                  (int-to-string left-margin)))
            (setq opt (string-to-int opt))
            (message (format "Align right at column %d." opt))
            (right-flush-region (point) (mark) opt)))
  ("Tab      Indent each line in region relative to line above it." indent-region)
  ))
