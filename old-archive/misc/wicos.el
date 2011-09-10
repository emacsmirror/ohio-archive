;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wicos.el
;; Save and restore multiple window configurations (wicos) within emacs.
;; 
;; v1.43;  23 Apr 1993
;;
;; Copyright 1993 Heikki Suopanki
;;
;; email: suopanki@phoenix.oulu.fi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LCD Archive Entry:
;; wicos|Heikki T. Suopanki|suopanki@phoenix.oulu.fi|
;; Save and restore multiple window configurations within Emacs.|
;; 23-Apr-1993|1.43|~/misc/wicos.el.Z|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; You can have multiple window configurations ('wico screens' or
;;; 'wicos') within your emacs if you use wicos.el. 
;;; If you use applications which use many windows (like gnus) it's
;;; easy to switch to a different wico screen and back and find the old
;;; wico screen and its windows unchanged because each application can have its
;;; own wico.  
;;; You can create and kill wicos, jump to a specific wico etc.
;;;
;;; Bytecompile this file and put (require 'wicos) in your .emacs
;;;
;;; 'M-o c' creates a new wico, 'M-o k' kills one. Use 'M-o p' (previous)
;;; and 'M-o n' (next) or 'M-o g' (go to) to switch wicos. 
;;; 'M-o 1' goes directly to wico 1 (this works with numbers 0-9).
;;;
;;; 'M-o m' shows a wico menu, it is useful if you have many wicos open.
;;; 
;;; There are other functions, 'M-o ?' gives help. 
;;; All wicos functions use bindings 'M-o + key'. 
;;; You can bind M-o to something else, look at the variable wico-prefix-key.
;;;
;;; Wico name is shown in the mode line (like '<Elisp>') if you have a line: 
;;; (setq wico-show-ml t)
;;; in your .emacs. 
;;; Use 'M-o t' to toggle it interactively.
;;; The name is however updated only when you use some wico function.
;;; If you only want to update it use 'M-o v'
;;;
;;; Wicos are named automatically if possible or you can give them
;;; your own names. If wicos don't have a real name they get a name 'scratch'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; X, Epoch (Emacs-19, Lucid Emacs) :
;;;
;;; If your Emacs supports x-popup-menu you can use functions:
;;; wico-x-menu and wico-x-show-list.
;;;
;;; There is now some _minimal_ Epoch support (it may work with
;;; Emacs-19 and Lucid Emacs too...???). It doesn't get confused if
;;; there are many screens open. It doesn't necessarily work properly if you
;;; change the screen size or do something "strange". Someone who has
;;; more Epoch experience should fix this....(volunteers?)
;;; If you use Epoch with multiple screens add:
;;; (setq wico-multi-screens t) 
;;; in your .emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hints and tips:
;;;
;;; Many wicos are automatically named (gnus, mail, elisp, C etc.) and
;;; it's easy to switch to them. For example just type M-o g g RET
;;; (that is automatically completed to gnus, you can also use TAB or
;;; SPC to complete) and you should be in your gnus wico. But if
;;; you tend to have C files in many wicos you will have wicos called:
;;; C, C<2>, C<3> , and it's hard to remember what is what. Now it's
;;; time to give names.  If your driver project files are in wico C<2>,
;;; just go there and type 'M-o w driver RET' and that wico is now
;;; called 'driver' instead of 'C<2>'.  
;;; 
;;;
;;; You can copy wicos. Type 'M-o s' and current configuration is
;;; saved, go to another wico and type 'M-o r' and configuration is
;;; restored. That can be used inside one wico to temporarily do
;;; something that alters the normal configuration.
;;;
;;;
;;; If your screen gets really mangled (lot of useles windows) 'M-o z'
;;; may help. It resets the wico back to the configuration it had when
;;; you switched to it.
;;;
;;;
;;; 'M-o u' unkills the wico you last killed, it can be used to move wicos.
;;;

;;; Here are things you can put in your .emacs:
;;;
;;;
;;; add a new mode to type-alist
;(setq wico-type-alist (cons '("texinfo" . "Texinfo ") wico-type-alist))
;
;;;
;;;
;;; 'M-o a' runs this hook. You can put something like this without
;;; the hook in your .emacs and whenever you run emacs you have 
;;; always automatically the same wicos, of course that slows
;;; down your emacs startup.
;(setq wico-open-useful-hook 
;  '(lambda()
;     (wico-create-new)
;     (shell)       ; wico #1  is shell
;     (wico-create-new)
;     (dired "~")   ; wico #2  is dired
;     (wico-jump-to 0))) ; back to wico #0
;
;;;
;;;
;;; wico-hook and wico-update-hook are the two most important hooks.
;;; wico-hook is run when the wico functions are used the first
;;; time, so it's run only once maximun during each emacs session.
;;; wico-update-hook is run everytime you use any of the wico functions
;;;
;;;
;;; If any wico function is used start to show the wico name in the
;;; mode line. 
;(setq wico-hook
;  '(lambda()
;     (setq wico-show-ml t)))
;;;
;;; If you want to show it always just add the bare line:
;;;       (setq wico-show-ml t)
;;; before (!) the line '(require 'wicos)' in your .emacs
;;;
;;; You can use 'M-o t' to toggle the value interactively.
;;;
;;;
;;; An example how to use wico-update-hook.
;;; Sets bg, fg and cursor colors for every wico.
;;; First try to check if wico type or wico number has preferred
;;; colors, otherwise use default colors.
;;; You can easily change this to modify other things.
;;; This example works only under X!
;(defvar wico-colors-alist 
;      (mapcar 'purecopy  
;	      '(("gnus" . ("green" "blue" "red"))
;               ("tex" .  ("white" "blue" "black"))
;		("0" . ("cyan" "black" "blue"))
;		("1" . ("red" "black" "white"))
;		("2" .  ("yellow" "red" "blue"))
;		)))
;(defvar wico-update-hook
;  '(lambda() 
;     (wico-set-type)
;     (let ((mode (aref wico-get-mode))
;	    (alist wico-colors-alist) 
;           (colors nil))
;       (while (and (not colors) alist)
;	 (if (and mode (string-match (downcase (car (car alist))) mode))
;	     (setq colors (cdr (car alist)))
;	   (if (string-match (car (car alist)) (int-to-string wico-this-wico))
;	       (setq colors (cdr (car alist)))
;	     (setq alist (cdr alist)))))
;       (if (not colors) (setq colors (list '"white" '"black" '"blue")))
;       (x-set-background-color (car colors))
;       (x-set-foreground-color (car (cdr colors)))
;	(x-set-cursor-color (car (cdr (cdr colors)))))))
;;;
;;The next variable and two hooks are used to enable new features.
;;For experienced users only....
;;
;;(defvar wico-user-vectors)
;;(defvar wico-current-configuration-hook)
;;(defvar wico-set-configuration-hook)
;
;;;
;;; change the prefix key.
;(setq wico-prefix-key "\C-c\C-o"
;;;  If wico-prefix-key is set _before_ wicos.el is loaded it replaces 
;;;  the normal binding (M-o) which can now have different function binded to it.
;;;  If it is set after loading wicos.el it doesn't have any effect,
;;;  use global-set-key method instead.
;;;
;;; and finally load the beast
;(require 'wicos)
;;;
;;; Add a new wico prefix key. Now you have two prefix keys.
;(global-set-key "\e[[L" 'wico-command-prefix); F12 in my terminal
;
;
;;;; end .emacs

(provide 'wicos)

;; the key which the all wicos functions use
(defvar wico-prefix-key "\eo")  ; M-o,  I had to bind it to something....
                                  ; change it to whatever you want

(global-unset-key wico-prefix-key)

(defvar wico-show-ml nil
"*Shows the wico name or number in the mode line if t.")

(defvar wico-show-numeric-ml nil
"*Shows the wico number in the mode line instead of the name, if t,
may be useful if your mode-line is short....")

(defvar wico-multi-screens nil
"*Better support in multi screen environment if t.")

;; set the mode line
;(if (not (listp (car global-mode-string)))
;    (setq global-mode-string (list global-mode-string)))
(or (assoc 'wico-show-ml global-mode-string)
     (setq global-mode-string
	   (cons '(wico-show-ml wico-mode-line)
		 global-mode-string)))

(if (not (memq 'wico-update-mode-line find-file-hooks))
    (setq find-file-hooks (cons 'wico-update-mode-line find-file-hooks)))


(defvar wico-mode-line "Scratch"  
"*String that is shown in the mode line.") 

(defvar wico-type-alist 
      (mapcar 'purecopy
	      '(("inbox\\|reply to\\|vm\\|mail" . "Mail")
		("article\\|subject\\|newsgroup" . "Gnus")
		("dired" . "Dired")
		("shell" .  "Shell")
		("^c \\|^c$\\| c \\| c$" . "C") ;; have to be a bit tricky
                                                ;; so we won't get unexpected
						;; results....
                ("^tex \\|^tex$\\| tex \\| tex$" . "TeX")
		("emacs-lisp" . "Elisp")
		("info" .  "Info")
		("calc" . "Calc")
		("telnet" . "Telnet ")
		("gopher" . "Gopher")
		("wais" . "Wais")
		("compil" . "Compile")
		("calendar" . "Calendar")
		("gomoku" . "Gomoku")
		("manual" . "Man")
		("text" . "Text")
		("texinfo" . "Texinfo")
		("irc" . "IRC")
		("mud" . "Mud")
		("lisp interaction" . "Scratch")))) ;; only *scratch*
						   ;; buffer use that
						   ;; mode, right?


(defvar wico-buf-count 10  ;; this was unlimited, but I prefer it this way
"*How many buffers are associated with each wico")

(defvar wico-scratch-buffer "*scratch*")

;;  you probably need not to change anything after this
;;  but go ahead if you want

(defvar wico-not-used t 
"*Nil if any of the wico functions has been used.")

(defvar wico-confs (vector nil)
"*Vector that contains the information about wico configurations." )

(defvar wico-types (vector nil)
"*List of wico types (Gnus, Mail, Shell etc).")

(defvar wico-names (vector nil)
"*Names user has given")

(defvar wico-windows (vector "")
"*Names of windows in each wico")

(defvar wico-open-wicos 1
"*How many wicos are used.")

(defvar wico-this-wico 0 
"*Current wico.")

(defvar wico-user-vectors nil
"*List of vectors user wants to use in hooks.")

(defvar wico-map (make-sparse-keymap)
  "*Keymap for wicos.")

(define-key global-map wico-prefix-key 'wico-command-prefix)
(fset 'wico-command-prefix wico-map)
(define-key wico-map  "c" 'wico-create-new)
(define-key wico-map  "k" 'wico-kill)
(define-key wico-map  "p" 'wico-previous)
(define-key wico-map  "n" 'wico-next)
(define-key wico-map  "g" 'wico-goto)
(define-key wico-map  "w" 'wico-name-wico)
(define-key wico-map  "f" 'wico-find-file-new)
(define-key wico-map  "v" 'wico-show-number)
(define-key wico-map  "t" 'wico-toggle-mode-line)
(define-key wico-map  "?" 'wico-help)
(define-key wico-map  "0" 'wico-jump-0)
(define-key wico-map  "1" 'wico-jump-1)
(define-key wico-map  "2" 'wico-jump-2)
(define-key wico-map  "3" 'wico-jump-3)
(define-key wico-map  "4" 'wico-jump-4)
(define-key wico-map  "5" 'wico-jump-5)
(define-key wico-map  "6" 'wico-jump-6)
(define-key wico-map  "7" 'wico-jump-7)
(define-key wico-map  "8" 'wico-jump-8)
(define-key wico-map  "9" 'wico-jump-9)
(define-key wico-map  "s" 'wico-save-current-wico)
(define-key wico-map  "r" 'wico-restore-wico)
(define-key wico-map  "a" 'wico-open-useful)
(define-key wico-map  "m" 'wico-menu)
(define-key wico-map  "z" 'wico-zap)
(define-key wico-map  "u" 'wico-unkill)

;;  not really a mode, used only to show the help
(defun wico-help-mode () 
      "Wico keys:
       \\[wico-create-new]    create a new wico         
       \\[wico-kill]    kill current wico
       \\[wico-previous]    previous wico
       \\[wico-next]    next wico
       \\[wico-menu]    wico menu  
       \\[wico-name-wico]    give wico a name
       \\[wico-toggle-mode-line]    toggle the wico number in the mode line
       \\[wico-jump-0]  
       .......     }  jump to wico #
       \\[wico-jump-9]      
       \\[wico-show-number]    show the number of the current wico
       \\[wico-goto]    switch to a named wico
       \\[wico-help]    show this help
       \\[wico-save-current-wico]    save current configuration 
       \\[wico-restore-wico]    restore last saved configuration
       \\[wico-open-useful]    open some useful wicos
       \\[wico-find-file-new]    find file in a new wico
       \\[wico-unkill]    unkill a killed wico
       \\[wico-zap]    resets the original config
"
      (interactive)
      nil)

(defconst wico-menu-mode-map nil)
(if wico-menu-mode-map
    nil
  (setq wico-menu-mode-map (make-keymap))
  (suppress-keymap wico-menu-mode-map)
  (define-key wico-menu-mode-map " " 'scroll-up)
  (define-key wico-menu-mode-map "\177" 'scroll-down)
  (define-key wico-menu-mode-map "h" 'wico-menu-help)  
  (define-key wico-menu-mode-map "q" 'wico-menu-select)
  (define-key wico-menu-mode-map "x" 'wico-menu-execute)
  (define-key wico-menu-mode-map "?" 'describe-mode)
  (define-key wico-menu-mode-map "n" 'wico-menu-new)
  (define-key wico-menu-mode-map "u" 'wico-menu-unmark)
  (define-key wico-menu-mode-map "d" 'wico-menu-delete))

(defun wico-current-window-configuration ()
  (run-hooks 'wico-current-configuration-hook)
  (if wico-multi-screens
      (let ((origin (get-screen))
	    (conf-list (list (current-window-configuration))))
	(while (progn (switch-screen)
		      (not (eq origin (get-screen))))
	  (setq conf-list (cons (current-window-configuration) conf-list)))
	conf-list)
    (let ((count-buf 0)
	  (buffer-list (list (buffer-name)))
	  (point-list (list (point)))
	  (win-config (current-window-configuration))
	  (origin-buffer (buffer-name))
	  (origin-window (selected-window)))
      (while (progn (other-window 1)
		    (not (eq origin-window (selected-window))))
	(setq point-list (cons (point) point-list)))
      (while (progn (bury-buffer)
		    (and (not (eq origin-buffer (buffer-name)))
			 (< (setq count-buf (1+ count-buf)) wico-buf-count)))
	(setq buffer-list (cons (buffer-name) buffer-list)))
      (set-window-configuration win-config) ;; back where we started
      (aset wico-windows wico-this-wico (wico-get-windows))
      (wico-set-type)
      (list win-config point-list buffer-list))))
  
(defun wico-set-window-configuration (config-and-points)
  (if wico-multi-screens
      (let ((origin (get-screen)))
	(set-window-configuration (car config-and-points))
	(setq config-and-points (cdr config-and-points))
	(switch-screen)
	(redisplay-screen)
	(while (and (not (eq origin (get-screen)))
		    config-and-points)
	  (set-window-configuration (car config-and-points))
	  (setq config-and-points (cdr config-and-points))
	  (switch-screen))
	(redisplay-screen))
    (let ((point-list	(car (cdr config-and-points)))
	  (buffer-list    (car (cdr (cdr config-and-points))))
	  (origin	nil))
      (mapcar '(lambda(arg)
		 (if (get-buffer arg)
		     (switch-to-buffer arg)))
	      buffer-list)
      (delete-other-windows)
      (set-window-configuration (car config-and-points))
      (setq origin (selected-window))
      (while (progn (other-window -1)
		    (not (eq origin (selected-window))))
	(goto-char (car point-list))
	(setq point-list (cdr point-list)))
      (goto-char (car point-list))))
  (run-hooks 'wico-set-configuration-hook))

(defun wico-open-useful ()
  (interactive)
  (run-hooks 'wico-open-useful-hook))

(defun wico-name-wico(name)
  "Ask a name, if only RET removes name"
  (interactive "sGive name :")
  (if (string-match name "")
      (setq name nil))
  (aset wico-names wico-this-wico name)
  (aset wico-types wico-this-wico name)
  (wico-update))
 
(defun wico-zap ()
  "Set window config to the original config of current wico."
  (interactive)
  (if (not (wico-check))
      (progn
       (wico-set-window-configuration (aref wico-confs
					      wico-this-wico))
       (wico-update))))

(defun wico-next () 
"Switch to next wico."
  (interactive)
  (if (not (wico-check))
      (progn   
	(aset wico-confs wico-this-wico (wico-current-window-configuration))
	(if (= wico-this-wico (- wico-open-wicos 1)) (setq wico-this-wico 0)
	  (setq wico-this-wico (+ wico-this-wico 1)))
	(wico-set-window-configuration (aref wico-confs wico-this-wico))
	(wico-update))))
  
(defun wico-previous () 
"Switch to previous wico."
  (interactive)                                                      
  (if (not (wico-check))
      (progn
       (aset wico-confs wico-this-wico (wico-current-window-configuration))  
       (if (= wico-this-wico 0) (setq wico-this-wico (- wico-open-wicos 1))
	 (setq wico-this-wico (- wico-this-wico 1)))                      
       (wico-set-window-configuration (aref wico-confs wico-this-wico))      
       (wico-update))))

(defun wico-save-current-wico () 
"Save the current configuration."
  (interactive)
  (if (not (wico-check))
      (progn
	(setq wico-stored (wico-current-window-configuration))
	(setq wico-stored-user 
	      (mapcar '(lambda(x) 
			 (if (vectorp (eval (intern-soft x)))
			     (aref (eval (intern x)) wico-this-wico)
			   nil))
		      wico-user-vectors)))))

(defun wico-restore-wico () 
"Switch to the configuration which has been saved last."
  (interactive)
  (if (not (wico-check))
      (progn
	(let ((list wico-stored-user))
	  (mapcar '(lambda(x) 
		     (progn
		       (aset (eval (intern x)) wico-this-wico
			     (car list))
		       (setq list (cdr list))))
		  wico-user-vectors))
	(wico-set-window-configuration wico-stored))))

(defun wico-create-new () 
"Open a new wico."
  (interactive)
  (if (not (wico-check))
      (progn 
	(if (>= wico-open-wicos (length wico-confs))
	    (wico-enlarge-vectors)) ;; make the vectors bigger
	(aset wico-confs wico-this-wico (wico-current-window-configuration))
	(delete-other-windows) 
	(switch-to-buffer wico-scratch-buffer)
	(setq wico-open-wicos (+ 1 wico-open-wicos))
	(setq wico-this-wico (- wico-open-wicos 1))
	(aset wico-names wico-this-wico nil)
	(aset wico-confs wico-this-wico (wico-current-window-configuration))
	(wico-update))))

(defun wico-kill () 
"Kill the current wico."
  (interactive)
  (if (not (wico-check))
      (cond
       ((= wico-open-wicos 1) (message "Only one wico, can't kill"))
       ( t 
	 (let ((i wico-this-wico))
	   (while (< i (- wico-open-wicos 1))
	     (aset wico-confs i (aref wico-confs (+ i 1)))
	     (aset wico-types i (aref wico-types (+ i 1)))
	     (aset wico-names i (aref wico-names (+ i 1)))
	     (aset wico-windows i (aref wico-windows (+ i 1)))
	     (let ((list wico-user-vectors))  ;; update the user's vectors too
	       (while list
		 (let ((name (car list)))
		   (aset (eval (intern name)) i 
			 (aref (eval (intern name)) (+ i 1)))
		   (setq list (cdr list)))))
	     (setq i (+ i 1)))
	   (setq wico-killed-wico (wico-current-window-configuration))
	   (aset wico-names (1- wico-open-wicos) nil)
	   (aset wico-types (1- wico-open-wicos) nil)
	   (setq wico-open-wicos (- wico-open-wicos 1))
	   (if (= wico-this-wico wico-open-wicos) 
	       (setq wico-this-wico (- wico-this-wico 1)))
	   (wico-set-window-configuration (aref wico-confs wico-this-wico))
	   (wico-update))))))

(defun wico-unkill()
  (interactive)
  (wico-create-new)
  (wico-set-window-configuration wico-killed-wico))

;;
(defun wico-jump-to (arg &optional non-ia) 
  "Go to a specific wico."
  (if (not (wico-check))
      (if (>= arg 0)
	  (if (< arg wico-open-wicos)
	      (progn
		(aset wico-confs wico-this-wico (wico-current-window-configuration))
		(setq wico-this-wico arg)
		(wico-set-window-configuration (aref wico-confs wico-this-wico))
		(if (not non-ia) (wico-update)))
	    (message "No wico %d" arg))
	(message "No wico %d" arg))))

(defun wico-goto (switch-var)
  "Switch to a named wico."
  (interactive (let ((switch-var (wico-set-type)) 
		     (completion-ignore-case t)
		     (type-alist
		      (let ((i 0)
			    (types wico-types)
			    (types-alist (list)))
			(while (< i wico-open-wicos)
			  (setq types-alist 
				(cons (list (aref types i)) types-alist))
			  (setq i (1+ i)))
			types-alist)))
		 ;; if I was the author of 'terminal.el'
		 ;; there would be some dirty words here....
		 (setq switch-var 
		       (completing-read	
			"Switch to wico: "
			type-alist
			nil
			t
			nil
			))
		 (list switch-var)))
  (let ((i 0)
	(new-wico nil))
    (while (and (< i wico-open-wicos) (not new-wico))
      (if (string-equal (downcase (aref wico-types i)) (downcase switch-var))
	  (setq new-wico i)
	(setq i (1+ i))))
    (if new-wico
	(wico-jump-to new-wico)
      (message "No wico called %s!" switch-var))))

(defun wico-show-number () 
"Show the number of the current wico."
  (interactive)
  (message "wico: %d" wico-this-wico)
  (wico-update)
  (switch-to-buffer (current-buffer)))

(defun wico-help () 
"Help about wico functions."
 (interactive)
 (with-output-to-temp-buffer "*Wico Help*"
   (princ (documentation 'wico-help-mode))
   (print-help-return-message)))

(defun wico-jump-0 ()
  (interactive)
  (wico-jump-to 0))

(defun wico-jump-1 () 
   (interactive)
   (wico-jump-to 1))

(defun wico-jump-2 ()
  (interactive)
  (wico-jump-to 2))

(defun wico-jump-3 ()
  (interactive)
  (wico-jump-to 3))

(defun wico-jump-4 ()
  (interactive)
  (wico-jump-to 4))

(defun wico-jump-5 ()
  (interactive)
  (wico-jump-to 5))

(defun wico-jump-6 ()
  (interactive)
  (wico-jump-to 6))

(defun wico-jump-7 ()
  (interactive)
  (wico-jump-to 7))

(defun wico-jump-8 ()
  (interactive)
  (wico-jump-to 8))

(defun wico-jump-9 ()
  (interactive)
  (wico-jump-to 9))

(defun wico-toggle-mode-line (&optional arg)
"If no ARG toggles the wico info in the mode line.
if ARG 1 turns on
if ARG 0 turns off."
  (interactive)
  (if (not arg)
      (setq wico-show-ml (not wico-show-ml))
    (if (= arg 0) 	    
	(setq wico-show-ml nil)
      (setq wico-show-ml t)))
  (wico-update)
  (switch-to-buffer (current-buffer)))

(defun wico-update () 
  "Update wico variables 
    Most wico functions call this function."
  (interactive)
  (if wico-not-used 
      (progn
	(setq wico-not-used nil)
	(run-hooks 'wico-hook)))
  (run-hooks 'wico-update-hook)
  ;; update the wico name in the mode line
  (if wico-show-ml
      (wico-update-mode-line)))

(defun wico-update-mode-line ()
  (let ((name ""))
    (wico-set-type)
    (setq name (if wico-show-numeric-ml
		   wico-this-wico
		 (aref wico-types wico-this-wico)))
    (setq wico-mode-line (concat "<" name "> "))))

(defun wico-check()
  "Check if the wico functions can be used,
    returns nil if everything is okay"
  ;;
  ;; if there's a minibuffer open, do nothing
  (if (= (minibuffer-depth) 0) nil 
    (message "Minibuffer open, close it first!")))

;; an example how to use wico functions
;; saves couple of key strokes ...   :)
(defun wico-find-file-new (filename)
  (interactive "FFind file in new wico: ")
  (wico-create-new)
  (find-file filename)
  )

(defun wico-show-list ()
  (interactive)
  (let ((i 0))
    (with-output-to-temp-buffer "*wico-list*"
      (while (< i wico-open-wicos)
	;; isn't lisp beautiful?
	(princ (concat " +" i "+   " 
		       (if (< i 10)
			   (eval " ")
			 (eval ""))
		       (aref wico-types i)
		       (let ((count (- 12 (length (aref wico-types i))))
			     (spaces ""))
			 (while (> count 0)
			   (setq count (1- count))
			   (setq spaces (concat " " spaces)))
			 (eval spaces))
		       (aref wico-windows i) "\n"))
	(setq i (+ i 1)))))
  (wico-update))

(defun wico-menu ()
  (interactive)
  (aset wico-confs wico-this-wico (wico-current-window-configuration))
  (if (not (wico-check))
      (progn
	(wico-show-list)
	(pop-to-buffer "*wico-list*")
	(wico-menu-mode))))

(defun wico-menu-mode ()
"Shows wico numbers and the name of the buffers in each wico, also
tries to guess which application is run in each wico.

Move to the line where desired wico is and type \\[wico-menu-select] to select that wico.
\\[wico-menu-delete] marks the wicos you want to delete, 
\\[wico-menu-execute] deletes the marked wicos.

\\{wico-menu-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wico-menu-mode)
  (setq mode-name "Wico-menu")
  (setq truncate-lines t)
  (setq buffer-auto-save-file-name nil)
  (use-local-map wico-menu-mode-map)
  (setq buffer-read-only t)
  (beginning-of-line)
  (forward-line wico-this-wico)
  (message "q:select,n:new,d:delete,u:unmark,x:execute,h:help,?:more help"))

(defun wico-menu-select ()
  (interactive)
  (if (not (wico-check))
      (let (string start)
	(beginning-of-line)
	(search-forward "+")
	(setq start (point))
	(search-forward "+")
	(setq string (buffer-substring start (point)))
	(bury-buffer)
	(wico-set-window-configuration (aref wico-confs wico-this-wico))
	(wico-jump-to (string-to-int string)))))

(defun wico-menu-new ()
  "Create a new wico."
  (interactive)
  (if (not (wico-check))
      (progn
	(bury-buffer)
	(wico-set-window-configuration (aref wico-confs wico-this-wico))
	(wico-create-new))))

(defun wico-menu-help ()
  (interactive)
  (message "q:select,n:new,d:delete,u:unmark,x:execute,h:help,?:more help"))

(defun wico-menu-unmark ()
  "Unmark wico on this line."
  (interactive)
  (beginning-of-line)
  (let ((buffer-read-only nil))
    (delete-char 1)
    (insert ? )
    (forward-line 1)))

(defun wico-menu-delete ()
  "Mark wico on this line to be deleted by \\[wico-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (let ((buffer-read-only nil))
    (delete-char 1)
    (insert ?D)
    (forward-line 1)))

(defun wico-menu-execute ()
  (interactive)
  (goto-char (point-min))
  (let ((kill-list nil))
    (while (re-search-forward "^D" nil t)
      (let (string start wico-to-kill)
	(search-forward "+" nil t)
	(setq start (point))
	(search-forward "+" nil t)
	(setq string (buffer-substring start (point)))
	(setq wico-to-kill (string-to-int string))
	(cond
	 ((< wico-to-kill 0) nil)
	 ((>= wico-to-kill wico-open-wicos) nil)
	 (t 
	  (setq kill-list (cons wico-to-kill kill-list))))))
    (while kill-list
      (let* ((wico-to-kill (car kill-list))
	     (i wico-to-kill))
	(setq kill-list (cdr kill-list))
	(setq wico-killed-wico (aref wico-confs wico-to-kill))
	(while (< i (- wico-open-wicos 1))
	  (aset wico-confs i (aref wico-confs (+ i 1)))
	  (aset wico-types i (aref wico-types (+ i 1)))
	  (aset wico-names i (aref wico-names (+ i 1)))
	  (aset wico-windows i (aref wico-windows (+ i 1)))
	  (let ((list wico-user-vectors))  ;; update the user's vectors too
	    (while list
	      (let ((name (car list)))
		(aset (eval (intern name)) i 
		      (aref (eval (intern name)) (+ i 1)))
		(setq list (cdr list)))))
	  (setq i (+ i 1)))
	(aset wico-names (1- wico-open-wicos) nil)
	(aset wico-types (1- wico-open-wicos) nil)
	(setq wico-open-wicos (1- wico-open-wicos))
	(if (or
	     (> wico-this-wico wico-to-kill) 
	     (= wico-this-wico wico-open-wicos))
	    (setq wico-this-wico (1- wico-this-wico)))))
    (if (< wico-open-wicos 1)
	(progn
	  (setq wico-open-wicos 1)
	  (setq wico-this-wico 0)
	  (delete-other-windows)
	  (switch-to-buffer wico-scratch-buffer)
	  (aset wico-confs wico-this-wico 
		(wico-current-window-configuration))))
    (wico-set-window-configuration (aref wico-confs wico-this-wico))
    (wico-show-list)  
    (pop-to-buffer "*wico-list*")
    (forward-line wico-this-wico)
    (wico-update)))

(defun wico-set-type ()
  (let ((mode (wico-get-mode)))
   (if (aref wico-names wico-this-wico) 
	 (aset wico-types wico-this-wico (aref wico-names wico-this-wico))
     (if (or (not (stringp (aref wico-types wico-this-wico)))
	     (not (string-match mode (aref wico-types wico-this-wico))))
	 (let ((i 0) 
	       (k 2)
	       (new-type mode))
	   (while (< i wico-open-wicos)
	     (if (and (stringp (aref wico-types i))
		      (string-equal new-type (aref wico-types i)))
		 (progn
		   (setq new-type (concat mode "<" (int-to-string k) ">")) 
		   (setq k (1+ k))
		   (setq i 0)))
	     (setq i (1+ i)))
	   (aset wico-types wico-this-wico new-type))))))


(defun wico-get-mode()
  (let ((alist wico-type-alist)
	(type nil)
	(org (selected-window))
	(modes mode-name))
    (while (progn (other-window 1)
		  (not (eq org (selected-window))))
      (setq modes (concat mode-name "  " modes)))
    (while (and (not type) alist)
      (if (string-match (downcase (car (car alist)))   
			(downcase  modes )) 
	  (setq type (cdr (car alist)))
	(setq alist (cdr alist))))
    (if type 
	type
      "Scratch")))
  
(defun wico-get-windows ()
  (let ((org (selected-window))
	(window-list (buffer-name)))
    (while (progn (other-window 1)
		  (not (eq org (selected-window))))
      (setq window-list (concat (buffer-name) "  " window-list)))
    window-list))

;;  couple of functions which can be used with X
(defun wico-x-menu (&optional arg)
  (interactive)
  (if (null arg)
      (setq arg x-mouse-pos))
  (let ((selection
	 (x-popup-menu
	  arg
	  '("wico menu"
	    ("wico menu"
	     ("x : x wico list"           . wico-x-show-list)
	     ("n : next"                    . wico-next)
	     ("p : previous"                . wico-previous)
	     ("c : create new"              . wico-create-new)
             ("k : kill wico"             . wico-kill)
	     ("g : goto "                   . wico-goto)
	     ("f : find file"               . wico-find-file-new)
	     ("t : toggle mode line"        . wico-toggle-mode-line)
	     ("m : wico menu"             . wico-menu)
	     ("z : reset orig config"          . wico-zap)
	     ("s : wico save config"      . wico-save-current-wico)
	     ("r : restore saved config"      . wico-restore-wico)
	     ("t : toggle wico # mode line" . wico-toggle-mode-line)
	     ("v : show number"             . wico-show-number)
	     ("u : unkill"                  . wico-unkill)
	     ("? : help"                    . wico-help))))))
    (and selection (call-interactively selection))))

(defun wico-x-show-list (&optional arg)
  (interactive)
  (if (null arg)
      (setq arg x-mouse-pos))
  (let ((i 0)
	(slist))
    ;;
    ;; build a x-menu...
    ;;
    (setq slist
	  (list "wico list"
		(cons "wico list"
		       (let (j (tlist '()))
			 (setq i 0)
			 (while (< i wico-open-wicos)
			   (progn
			     (setq j i)
			     (setq i (+ i 1))
			     (setq tlist
				   (cons
				    (cons
				     (format " %d:  %s  %s"
					     j
					     (aref wico-types j))
				     (int-to-string j))
				    tlist))))
			 (reverse tlist)))))
    (setq i (x-popup-menu arg slist))
    (if (and
	 (stringp i)
	 (integerp (setq i (string-to-int i))))
	(wico-jump-to i)
      (message "dont change wico anyway."))))


;; is this really a good way to do it???
(defun wico-enlarge-vectors () 
  (setq wico-confs (vconcat wico-confs (vector nil)))
  (setq wico-names (vconcat wico-names (vector nil)))
  (setq wico-windows (vconcat wico-windows (vector "")))
  (setq wico-types (vconcat wico-types (vector nil)))
  (let ((list wico-user-vectors))  ;; enlarge the user's vectors too
    (while list
      (let ((name (car list)))
	(if (vectorp (eval (intern-soft name)))
	    (set (intern name) (vconcat (eval (intern name)) (vector nil)))
	  (set (intern name) (make-vector (length wico-confs) nil))))
      (setq list (cdr list))))
)

;; and finally define some things

(defvar wico-killed-wico (wico-current-window-configuration))

(defvar wico-stored (wico-current-window-configuration)
"*Configuration which has beens saved last.")

(defvar wico-stored-user 
    (mapcar '(lambda(x) 
	       (if (vectorp (eval (intern-soft x)))
		   (aref (eval (intern x)) wico-this-wico)
	       nil))
	       wico-user-vectors))

(aset wico-confs wico-this-wico (wico-current-window-configuration))
(aset wico-names wico-this-wico nil)

(wico-update-mode-line)

;; end
