; Path: hal.com!olivea!uunet!mcsun!news.funet.fi!ousrvr.oulu.fi!ousrvr!suopanki
; From: suopanki@stekt1.oulu.fi (Heikki T. Suopanki)
; Newsgroups: gnu.emacs.sources
; Subject: screens.el
; Date: 7 Apr 92 16:52:55 GMT
; Reply-To: suopanki@stekt.oulu.fi
; Organization: Unixverstas Olutensin, Finlandia
; 
; 
;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;
;;; screens.el
;;;
;;; LCD Archive Entry:
;;; screens|Heikki T. Suopanki|suopanki@stekt.oulu.fi|
;;; Save and restore multiple window configurations within Emacs.|
;;; 92-04-07|1.3|~/misc/screens.el.Z|
;;;
;;; v1.3 ; 7 Apr 1992
;;; Written by Heikki Suopanki
;;; email: suopanki@stekt.oulu.fi, suopanki@phoenix.oulu.fi
;;;
;;; This is free software; you can redistribute and/or modify it.
;;;
;;;
;;; You can have multiple screens inside your emacs if you use screens.el.
;;; If you use applications which have many windows (like gnus) it's
;;; easy to switch to a different screen and back and find the old
;;; screen and its windows unchanged because each application can have its
;;; own screen.  
;;; You can create and kill screens, jump to a specific screen etc.
;;; 
;;; Bytecompile and put (load 'screens) in your .emacs
;;;
;;; 'M-o c' creates a new screen, 'M-o k' kills one. Use 'M-o p' (previous)
;;; and 'M-o n' (next) or 'M-o g' (goto) to switch screens. 
;;; 'M-o 1' goes directly to screen 1 and so on.
;;;
;;; 'M-o m' shows a screen menu, it is useful if you have many screens open.
;;;
;;; There are other functions, 'M-o ?' gives help. 
;;; All screens functions use bindings 'M-o + key'. 
;;; You can bind M-o to something else, look at the variable screen-prefix-key.
;;;
;;; Screen # is shown in the mode line (like '<0>'), if you want to get
;;; rid of it put: 
;;; (screen-number-mode-line 0)
;;; in your .emacs. 'M-o t' toggles this function too.
;;;

(provide 'screens)

;; the key which the all screens functions use
(defvar screen-prefix-key "\eo")  ; M-o,  I had to bind it to something....
                                  ; change it to whatever you want

(global-unset-key screen-prefix-key)

;;  show the screen #  in the mode line
(defvar screen-show-screen-number t)

(setq global-mode-string '("" screen-mode-line display-time-string))

;;  An example how to get your favorite emacs applications in their
;;  own screens automatically.  
;;  You could also put something like this in your .emacs and they are
;;  there everytime you start your emacs session.
(defun screen-open-useful ()
  "Open some useful screens."
  (interactive)
  (screen-create-new)
  (shell)       ; screen #1  is shell
  (screen-create-new)
  (dired "~")   ; screen #2  is dired
  (screen-goto 0) ; back to screen #0
  )

(defvar screen-mode-line "<0> " ;; 
"Shows the screen number in mode line.") 


(defvar screen-not-used t 
"Nil if any of the screen functions has been used.")

(defconst screen-max 10 
"Maximum number of screens.")

(defvar screen-confs (make-vector screen-max '0) 
"Vector that contains the information about screen configurations." )

(defvar screen-list (make-vector screen-max '0)
"List of screens.")

(defvar screen-mode-list (make-vector screen-max '0)
"List of modes.")

(defvar screen-type (make-vector screen-max '0)
"List of screen types (GNUS, VM etc).")

(defvar screen-open-screens 1
"How many screens are used.")

(defvar screen-scratch-buffer "*scratch*")

(defvar screen-killed-screen (current-window-configuration))

;;
(defvar screen-last (current-window-configuration)
"Configuration which has beens saved last.")

(aset screen-confs 0 (current-window-configuration))

(defvar screen-this-screen 0 
"Number of the current screen.")

(defvar screen-map (make-sparse-keymap)
  "Keymap for screens.")

(define-key global-map screen-prefix-key 'screen-command)
(fset 'screen-command screen-map)

(define-key screen-map  "c" 'screen-create-new)
(define-key screen-map  "k" 'screen-kill)
(define-key screen-map  "p" 'screen-previous)
(define-key screen-map  "n" 'screen-next)
(define-key screen-map  "g" 'screen-goto)
(define-key screen-map  "f" 'screen-find-file-new)
(define-key screen-map  "v" 'screen-show-number)
(define-key screen-map  "t" 'screen-number-mode-line)
(define-key screen-map  "?" 'screen-help)
(define-key screen-map  "0" 'screen-jump-0)
(define-key screen-map  "1" 'screen-jump-1)
(define-key screen-map  "2" 'screen-jump-2)
(define-key screen-map  "3" 'screen-jump-3)
(define-key screen-map  "4" 'screen-jump-4)
(define-key screen-map  "5" 'screen-jump-5)
(define-key screen-map  "6" 'screen-jump-6)
(define-key screen-map  "7" 'screen-jump-7)
(define-key screen-map  "8" 'screen-jump-8)
(define-key screen-map  "9" 'screen-jump-9)
(define-key screen-map  "s" 'screen-save-current-configuration)
(define-key screen-map  "l" 'screen-change-last-configuration)
(define-key screen-map  "a" 'screen-open-useful)
(define-key screen-map  "m" 'screen-menu)
(define-key screen-map  "r" 'screen-restore)

;;  not really a mode, used only to show the help
(defun screen-help-mode () 
      "Screens keys:
       \\[screen-create-new]    create a new screen         
       \\[screen-kill]    kill current screen
       \\[screen-previous]    previous screen
       \\[screen-next]    next screen
       \\[screen-menu]    screen menu  
       \\[screen-number-mode-line]    show/hide the screen number in the mode line
       \\[screen-jump-0]  
       .......    jump to screen #
       \\[screen-jump-9]      
       
       \\[screen-show-number]    show the number of the current screen
       \\[screen-goto]    goto screen #
       \\[screen-help]    show this help
       \\[screen-save-current-configuration]    save current configuration 
       \\[screen-change-last-configuration]    switch to the last saved configuration
       \\[screen-open-useful]    open some useful screens
       \\[screen-find-file-new]    find file in a new screen
       \\[screen-restore]    restore a killed screen"
      )

(defconst screen-menu-mode-map nil)
(if screen-menu-mode-map
    nil
  (setq screen-menu-mode-map (make-keymap))
  (suppress-keymap screen-menu-mode-map)
  (define-key screen-menu-mode-map " " 'scroll-up)
  (define-key screen-menu-mode-map "\177" 'scroll-down)
  (define-key screen-menu-mode-map "?" 'screen-menu-help)  
  (define-key screen-menu-mode-map "s" 'screen-menu-select)
  (define-key screen-menu-mode-map "h" 'describe-mode)
  (define-key screen-menu-mode-map "n" 'screen-menu-new)
  (define-key screen-menu-mode-map "k" 'screen-menu-kill)
  (define-key screen-menu-mode-map "q" 'screen-menu-quit))

(defun screen-next () 
"Switch to the next screen."
  (interactive)
  (if (not (screen-check))
      (progn   
	(aset screen-confs screen-this-screen (current-window-configuration))
	(delete-other-windows)
	(switch-to-buffer screen-scratch-buffer)
	(if (= screen-this-screen (- screen-open-screens 1)) (setq screen-this-screen 0)
	  (setq screen-this-screen (+ screen-this-screen 1)))
	(set-window-configuration (aref screen-confs screen-this-screen))
	(screen-update))))
  
(defun screen-previous () 
"Switch to previous screen."
  (interactive)                                                      
  (if (not (screen-check))
      (progn
       (aset screen-confs screen-this-screen (current-window-configuration))  
       (delete-other-windows)
       (switch-to-buffer screen-scratch-buffer)
       (if (= screen-this-screen 0) (setq screen-this-screen (- screen-open-screens 1))
	 (setq screen-this-screen (- screen-this-screen 1)))                      
       (set-window-configuration (aref screen-confs screen-this-screen))      
       (screen-update))))

(defun screen-save-current-configuration () 
"Save the current configuration."
  (interactive)
  (if (not (screen-check))
      (setq screen-last (current-window-configuration))))

(defun screen-change-last-configuration () 
"Switch to the configuration which has been saved last."
  (interactive)
  (if (not (screen-check))
      (progn
       (delete-other-windows)
       (switch-to-buffer screen-scratch-buffer)
       (set-window-configuration screen-last))))

(defun screen-create-new () 
"Open a new screen."
  (interactive)
  (if (not (screen-check))
      (if (>= screen-open-screens screen-max)  (message "No more screens")
	(progn 
	  (aset screen-confs screen-this-screen (current-window-configuration))
	  (delete-other-windows) 
	  (switch-to-buffer screen-scratch-buffer)
	  (setq screen-open-screens (+ 1 screen-open-screens))
	  (setq screen-this-screen (- screen-open-screens 1))
	  (screen-update)
	  (aset screen-confs screen-this-screen (current-window-configuration))))))

(defun screen-kill () 
"Kill the current screen."
  (interactive)
  (if (not (screen-check))
      (cond
       ((= screen-open-screens 1) (message "Only one screen, can't kill"))
       ( t 
	 (let ((i screen-this-screen))
	   (while (< i (- screen-open-screens 1))
	     (aset screen-confs i (aref screen-confs (+ i 1)))
	     (setq i (+ i 1))) 
	   (setq screen-killed-screen (current-window-configuration))
	   (setq screen-open-screens (- screen-open-screens 1))
	   (delete-other-windows)
	   (switch-to-buffer screen-scratch-buffer)
	   (if (= screen-this-screen screen-open-screens ) 
	       (setq screen-this-screen (- screen-this-screen 1)))
	   (set-window-configuration (aref screen-confs screen-this-screen))
	   (screen-update))))))

(defun screen-restore()
  (interactive)
  (screen-create-new)
  (set-window-configuration screen-killed-screen))

;;
(defun screen-goto (arg) 
"Switch to a specific screen."
  (interactive "nGoto screen number:")
  (if (not (screen-check))
      (if (>= arg 0)
	  (if (< arg screen-open-screens)
	      (progn
		(aset screen-confs screen-this-screen (current-window-configuration))
		(setq screen-this-screen arg)
		(delete-other-windows)
		(switch-to-buffer screen-scratch-buffer)
		(set-window-configuration (aref screen-confs screen-this-screen))  
		(screen-update))
	    (message "No screen %d" arg))
	(message "No screen %d" arg))))

(defun screen-show-number () 
"Show the number of the current screen."
  (interactive)
  (message "screen: %d" screen-this-screen)
  )

(defun screen-help () 
"Help about screen functions."
 (interactive)
 (with-output-to-temp-buffer "*Screens Help*"
   (princ (documentation 'screen-help-mode))
   (print-help-return-message)))

(defun screen-jump-0 ()
  (interactive)
  (screen-goto 0))

(defun screen-jump-1 ()
   (interactive)
   (screen-goto 1))

(defun screen-jump-2 ()
  (interactive)
  (screen-goto 2))

(defun screen-jump-3 ()
  (interactive)
  (screen-goto 3))

(defun screen-jump-4 ()
  (interactive)
  (screen-goto 4))

(defun screen-jump-5 ()
  (interactive)
  (screen-goto 5))

(defun screen-jump-6 ()
  (interactive)
  (screen-goto 6))

(defun screen-jump-7 ()
  (interactive)
  (screen-goto 7))

(defun screen-jump-8 ()
  (interactive)
  (screen-goto 8))

(defun screen-jump-9 ()
  (interactive)
  (screen-goto 9))

(defun screen-number-mode-line (&optional arg)
  "If no ARG toggles the screen number in the mode line.
If ARG is 0 hides the screen number,
any other ARG shows the screen number."
  (interactive)
  (if arg
      (if (equal arg 0)
	  (setq screen-show-screen-number nil)
	(setq screen-show-screen-number t))
    (if screen-show-screen-number
	(setq screen-show-screen-number nil)
      (setq screen-show-screen-number t)))
  (screen-update)
  (switch-to-buffer (current-buffer)))

(defun screen-update () 
  "Update screen variables 
    Most screen functions call this function."
  (interactive)
  ;; update the screen number in the mode line
  (if screen-show-screen-number
      (setq screen-mode-line (concat "<" screen-this-screen "> "))
    (setq screen-mode-line ""))
  (if screen-not-used (screen-first-time)) 
  )

(defun screen-check()
  "Check if the screen functions can be used,
    returns nil if everything is okay"
  ;;
  ;; if there's a minibuffer open, do nothing
  (if (= (minibuffer-depth) 0) nil 
    (message "Minibuffer open, can't do that!"))
  )

;; is this really needed?
(defun screen-first-time ()
  "This function is called when the screen functions 
    are used the first time."
  (setq screen-not-used nil)
  (run-hooks 'screen-hook)
  )

;; one example how to use screen functions
;; saves couple of key strokes ...   :)
(defun screen-find-file-new (filename)
  (interactive "FFind file in new screen: ")
  (screen-create-new)
  (find-file filename)
  )
 
(defun screen-show-list ()
  (interactive)
  (let ((i 0)(screen-list-orig-screen screen-this-screen))
    (while (< i screen-open-screens)
      (screen-goto i)
      (aset screen-list i (screen-get-window-list))
      (aset screen-mode-list i (screen-get-mode-list))
      (cond 
       ((string-match "INBOX\\|reply to\\|VM" (aref screen-list i))
	(aset screen-type i "VM     "))
       ((string-match "Article\\|Subject\\|Newsgroup" (aref screen-list i))
	(aset screen-type i "GNUS   "))
       ((string-match "[Ss]hell" (aref screen-list i))
	(aset screen-type i "SHELL  "))
       ((string-match "compilation" (aref screen-list i))
	(aset screen-type i "COMPILE"))
       ((string-match "-telnet" (aref screen-list i))
	(aset screen-type i "TELNET "))
       ((string-match "IRC" (aref screen-list i))
	(aset screen-type i "IRC    "))
       ((string-match "RMAIL" (aref screen-list i))
	(aset screen-type i "RMAIL  "))
       ;; buffer-names didn't give it so let's try modes
       ((string-match "Dired" (aref screen-mode-list i))
	(aset screen-type i "DIRED  "))
       ((string-match "Info" (aref screen-mode-list i))
	(aset screen-type i "INFO   "))
       (t 
	(aset screen-type i "       ")))
      (setq i (+ i 1)))
    (screen-goto screen-list-orig-screen)
    (with-output-to-temp-buffer "*screen-list*"
      (setq i 0)
      (while (< i screen-open-screens)
	(princ (concat " + " i " +   " (aref screen-type i)
		       (aref screen-list i) "\n"))
	(setq i (+ i 1))))))

(defun screen-menu ()
  (interactive)
  (if (not (screen-check))
      (progn
	(screen-show-list)
	(pop-to-buffer "*screen-list*")
	(screen-menu-mode))))

(defun screen-menu-mode ()
"Shows screen numbers and the name of the buffers in each screen, also
tries to guess which application is run in each screen.

Move to the line where desired screen is and type \\[screen-menu-select]
to select that screen.

\\{screen-menu-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'screen-menu-mode)
  (setq mode-name "Screen-menu")
  (setq buffer-auto-save-file-name nil)
  (use-local-map screen-menu-mode-map)
  (setq buffer-read-only t)
  (beginning-of-line)
  (message "s: select, n: new, k: kill, q: quit, ?: this help, h: more help"))

(defun screen-menu-quit ()
  (interactive)
  (bury-buffer)
  (set-window-configuration (aref screen-confs screen-this-screen))
  (if (string-equal (buffer-name) "*screen-list*")
      (if (one-window-p t)
	  (bury-buffer)
	(delete-window))))

(defun screen-menu-select ()
  (interactive)
  (if (not (screen-check))
      (let (string start)
	(beginning-of-line)
	(search-forward "+")
	(forward-char 1)
	(setq start (point))
	(search-forward "+")
	(backward-char 1)
	(setq string (buffer-substring start (point)))
	(bury-buffer)
	(set-window-configuration (aref screen-confs screen-this-screen))
	(screen-goto (string-to-int string)))))

(defun screen-menu-new ()
  (interactive)
  (if (not (screen-check))
      (progn
	(bury-buffer)
	(set-window-configuration (aref screen-confs screen-this-screen))
	(screen-create-new))))
				

(defun screen-menu-help ()
  (interactive)
  (message "s: select, n: new, k: kill, q: quit, ?: this help, h : more help"))

(defun screen-menu-kill ()
  (interactive)
  (let (string start screen-to-kill)
    (beginning-of-line)
    (search-forward "+")
    (forward-char 1)
    (setq start (point))
    (search-forward "+")
    (backward-char 1)
    (setq string (buffer-substring start (point)))
    (setq screen-to-kill (string-to-int string))
      (cond
       ((< screen-open-screens 2) (message "Can't kill, only one screen!"))
       ((< screen-to-kill 0) (message "No such screen"))
       ((>= screen-to-kill screen-open-screens) (message "No such screen"))
       (t 
	(setq screen-killed-screen (aref screen-confs screen-to-kill))
	(let ((i screen-to-kill))
	  (while (< i (- screen-open-screens 1))
	    (aset screen-confs i (aref screen-confs (+ i 1)))
	    (setq i (+ i 1)))
	  (setq screen-open-screens (1- screen-open-screens))
	  (if (or
	       (> screen-this-screen screen-to-kill) 
	       (= screen-this-screen screen-open-screens))
	      (setq screen-this-screen (1- screen-this-screen)))
	  (set-window-configuration (aref screen-confs screen-this-screen))
	  (screen-menu)
	  (if (= screen-to-kill screen-open-screens)
	      (goto-line screen-to-kill)
	    (goto-line (1+ screen-to-kill))))))))

(defun screen-get-window-list ()
  (let ((org (selected-window))
	(window-list (buffer-name)))
    (while (progn (other-window 1)
		  (not (eq org (selected-window))))
      (setq window-list (concat (buffer-name) "  " window-list)))
    window-list))

(defun screen-get-mode-list ()
  (let ((org (selected-window))
	(mode-list mode-name))
    (while (progn (other-window 1)
		  (not (eq org (selected-window))))
      (setq mode-list (concat mode-name "  " mode-list)))
    mode-list))
