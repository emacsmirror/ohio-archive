;;-------------------------------------------------------------------------
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|     Unused      |    What Line    |    Goto Line    |   End of Line   |
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|-----------------------------------------------------------------------|
;;|                 |                 |                 |                 |
;;|                 |       ^         |                 |                 |
;;|  Begin of File  |      / \        |  Previous Page  |                 |
;;|                 |      | |        |                 |                 |
;;|                 |                 |                 |   Beginning     |
;;|-----------------------------------------------------|      of         |
;;|                 |                 |                 |     Line        |
;;|       /__       |                 |     __\         |                 |
;;|      < __       |     Center      |     __ >        |                 |
;;|       \         |                 |       /         |                 |
;;|                 |                 |                 |                 |
;;|-----------------------------------------------------------------------|
;;|                 |                 |                 |                 |
;;|                 |      | |        |                 |                 |
;;|   End of File   |      \ /        |    Next Page    |                 |
;;|                 |       v         |                 |                 |
;;|                 |                 |                 |     Query       |
;;|------------------------------------------------------    Replace      |
;;|                                   |                 |  Reg Express    |
;;|                                   |                 |                 |
;;|             Over Write            |     Delete      |                 |
;;|                                   |                 |                 |
;;|                                   |                 |                 |
;;-------------------------------------------------------------------------
;;-------------------------------------------------------------------------
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|      Undo       |    Directory    |    Help Info    |       FTP       |
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|-----------------------------------------------------------------------|
;;-------------------------------------------------------------------------
;;|                 |                 |                 |                 |
;;|                 |                 |   Fundamental   |   Hexadecimal   |
;;|      Make       |                 |      Mode       |      Mode       |
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|-----------------------------------------------------------------------|
;;-------------------------------------------------------------------------
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|                 |                 |      Hanoi      |       YOW       |
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;-------------------------------------------------------------------------
;;|                 |                 |                 |                 |
;;|                 |                 |                 |                 |
;;|      Prt Sc     |   Scroll Lock   |      Pause      |                 |
;;|     (unused)    |     (unused)    |     (unused)    |                 |
;;|                 |                 |                 |                 |
;;|-----------------------------------------------------------------------|
;;
;; .emacs
;;
;; Tom's GNU Emacs initialization file - v1.0 - 1/2/96.
;; Emacs init file for 101-key keyboards (and X Windows).
;; Linux 2.0.29, XF86Free X11R6, GNU Emacs 19.28.
;; Copy this file to your home directory, customize it, smile.
;;
;; Feel free to use any or all parts of this file - standard GNU Public
;; License - restrictions none.  Pass it on.
;;
;; Comments to: Tom Herman (Internet: mail11325@pop.net)
;;                         (  Packet: N4OOJ @ WR4AGC.#DUR.NC.USA.NA)
;;
;; Last Changed:
;:   1/2/96 - TWH - New file.
;:   7/6/96 - TWH - Chged function key commands.
;;   3/21/97 - TWH - Many changes to colors, features, and template.
;;
;; Look in the following files for more info:
;;     o  lisp/loaddefs.el
;;     o  The emacs FAQ.
;;          /usr/lib/emacs/19.28/etc/FAQ
;;                   or
;;          anonymous ftp to ftp://rtfm.mit.edu/pub/usenet-by-group/comp.emacs/
;;     o  xev  (x events)
;;     o  xmodmap -pk  and   xmodmap -pm
;;     o  /usr/X11R6/include/X11/keysymdef.h
;;

;;(setq emacs-base-dir "/usr/lib/emacs/")
;;(setq emacs-dir (concat emacs-base-dir "19.28/"))

;; add to load path
;;(setq load-path (cons (concat emacs-dir "i486-slackware-linux/") load-path))


;;
;; Uncomment this lisp form if you want to try using the packages
;; that are not distributed with the standard emacs distribution
;; They reside in the directory emacs-dir/i486-slackware-linux.
;;
;;(setq exec-directory (concat emacs-dir "lisp/"))   ; for load-file
;;(setq load-path (cons (concat emacs-dir "i486-slackware-linux/") load-path))
;;(setq exec-directory (concat emacs-dir "i486-slackware-linux/"))

;;
;; This causes the vt220 file to be loaded after all of the setup files
;; have been run
;;
(setq term-setup-hook
  '(lambda ()
;;    (load "loaddefs")
;;    (load "spell")
;;    (load "term/vt220")
)) 


;;
;; Uncomment the following code to bind the keypad/function keys under X11.
;; See the file lisp/loaddefs.el for further details.
;; Note: .emacs is case sensitive to kp-x.  Compare to xmodmap -pk.
;;
;; Num Lock doesn't work.  Neither does Prt Sc or Scroll Lock.
;; I wonder why?
;;
;; Keypad
;;
(global-set-key '[num_lock] 'unused)               ; Num Lock (unused)
(global-set-key '[kp-divide] 'what-line)           ; / 
(global-set-key '[kp-multiply] 'goto-line)         ; * 
(global-set-key '[kp-subtract] 'beginning-of-line) ; - (BOL)
(global-set-key '[kp-7] 'beginning-of-buffer)      ; KP7 (BOF)
(global-set-key '[kp-8] 'previous-line)            ; KP8 (up-arrow)
(global-set-key '[kp-9] 'scroll-down)              ; KP9 (pg-up)
(global-set-key '[kp-add] 'end-of-line)            ; + (EOL)
(global-set-key '[kp-4] 'backward-char)            ; KP4 (lf-arrow)
(global-set-key '[kp-5] 'recenter)                 ; KP5
(global-set-key '[kp-6] 'forward-char)             ; KP6 (rt-arrow)
(global-set-key '[kp-1] 'end-of-buffer)            ; KP1 (EOF)
(global-set-key '[kp-2] 'next-line)                ; KP2 (dn-arrow)
(global-set-key '[kp-3] 'scroll-up)                ; KP3 (pg-down)
(global-set-key '[kp-enter] 'query-replace-regexp) ; Enter
(global-set-key '[kp-0] 'overwrite-mode)           ; KP0 (Over write)
(global-set-key '[kp-decimal] 'delete-char)        ; .
;;
;; Function Keys
;;
(global-set-key '[f1] 'undo)                       ; F1
(global-set-key '[f2] 'dired)                      ; F2
(global-set-key '[f3] 'info)                       ; F3
(global-set-key '[f4] 'find-file)                  ; F4
(global-set-key '[f5] 'compile)                    ; F5
(global-set-key '[f6] 'unused)                     ; F6
(global-set-key '[f7] 'fundamental-mode)           ; F7
(global-set-key '[f8] 'hexl-mode)                  ; F8
(global-set-key '[f9] 'unused)                     ; F9
(global-set-key '[f10] 'unused)                    ; F10
(global-set-key '[f11] 'hanoi)                     ; F11
(global-set-key '[f12] 'yow)                       ; F12
;; Other Function Key Bindings (kinda like WordPerfect 4.2)
;; (Use Control-h l (that's L) for key codes
(global-set-key '[C-f9] 'yow)                      ; Control-F9
(global-set-key '[M-f9] 'hanoi)                    ; Alt-F9
(global-set-key '[S-f19] 'dired)                   ; Shift-F9
(global-set-key '[C-S-f19] 'c-mode)                ; Control-Shift-F9
(global-set-key '[M-S-f19] 'perl-mode)             ; Alt-Shift-F9
;; Control-Alt-F9 not recommented unless you want to change virtual consoles.
;; Use Control-Alt-F7 to get back to your current virtual console.

;; Use Alt_R as a Meta (escape) key (or type it in xterm).
(shell-command "xmodmap -e 'add mod1 = Mode_switch'")

;;
;; Miscellaneous keys
;;
(global-set-key '[delete] 'delete-backward-char)   ; Delete
(global-set-key '[nosymbol] 'unused)               ; Prt Sc
(global-set-key '[multi_key] 'unused)              ; Scroll Lock
(global-set-key '[pause] 'unused)                  ; Pause

;;
;; Other tricks and goodies:
;; Meta is Esc or Alt keys, Ctrl is Control key.
;;
;; To view the X keymap:
;;   xmodmap -pk -pm
;;
;; Ctrl-g to quit a command.
;; Ctrl-x K to kill a buffer.
;; Ctrl-z to iconize.
;; 'f' to read a file in dired.
;; ! to skip the yes-no question and do all on 'Query replacing'.
;; Ctrl-u 7 Ctrl-x hanoi to specify an argument
;;
;; To iconize to GNU icon, add the following line into .env:
;;   alias emacs='emacs -i '
;; You may have to add the following line to .profile to run .env:
;;   . $HOME/.env
;;
;; *.el files are Lisp source code files; *.elc are faster compiled files.
;;

;;
;; My special stuff
;;
(setq default-tab-width 4) ; tabs to 4
(setq c-tab-always-indent nil)
(setq c-indent-level 4)
(setq c-continued-statement-offset 4)
(setq c-auto-newline 1)
(setq c-brace-offset -4)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(require 'paren)             ; highlight matching parenthesis

;;
;; Mike's Stuff
;; Ctrl-N to split windows vertically
;; Shift-Ctrl-N to recombine vertical windows
;;
(defun grab-paste-and-pad (click)
  (interactive "e")
  (grab-and-paste click)
  (insert " "))

(defun grab-and-paste (click)
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((orig-window (selected-window))
		 (orig-point (point)))
	(unwind-protect
		(progn
		  (mouse-set-point click)
		  (forward-sexp 1)
		  (let ((end (point)))
			(forward-sexp -1)
			(kill-new (buffer-substring (point) end) t)))
	  (select-window orig-window)
	  (goto-char orig-point)
	  (yank)
	  )))

(global-set-key [C-backspace] 'undo)

(global-set-key [S-mouse-2] 'split-window-vertically)
(global-set-key [S-mouse-3] 'split-window-vertically)
(global-set-key [C-S-mouse-2] 'mouse-delete-other-windows)
(global-set-key [C-S-mouse-3] 'mouse-delete-other-windows)

(global-set-key [A-mouse-2] 'grab-paste-and-pad)
(global-set-key [M-mouse-2] 'grab-and-paste)
(global-set-key [M-mouse-3] 'grab-paste-and-pad)
(global-set-key [A-mouse-3] 'grab-and-paste)

(global-set-key [A-M-mouse-3] 'isearch-yank-kill) 

(global-set-key "\C-c." 'find-tag-other-window)

;;
;; Add color
;;
(add-hook 'font-lock-mode-hook
  '(lambda ()
    (set-face-foreground font-lock-comment-face "DarkSalmon")
    (set-face-foreground font-lock-type-face "MintCream")
    (set-face-foreground font-lock-string-face "grey40")
    (set-face-foreground font-lock-keyword-face "LightBlue")))
;;(add-hook 'emacs-lisp-mode-hook '(lambda () (font-lock-mode 1)))

;;
;; Cool Colors for C Programs.
;; See /usr/share/emacs/19.30/lisp/hilit19.el
;;
(cond (window-system
       (setq hilit-mode-enable-list  '(not text-mode)
             hilit-background-mode   'dark
            hilit-inhibit-hooks     nil
            hilit-inhibit-rebinding t
            hilit-quietly t    
            hilit-auto-highlight-maxout 100000) 
       (require 'hilit19)
       ))

;; Customize colors
(cond (window-system
       (hilit-translate type     'yellow)      ;; char, int
       (hilit-translate comment  'yellow4)     ;; /* */
       (hilit-translate decl     'blue)        ;; struct
       (hilit-translate defun    'red)         ;; func()
       (hilit-translate include  'RoyalBlue)   ;; #include
       (hilit-translate keyword  'cyan)        ;; if, for, while
       (hilit-translate string   'blue)        ;; strings
       (hilit-translate dired-directory 'blue) ;; subdirectory
       (hilit-translate dired-link 'firebrick) ;; link
;       (hilit-translate crossref 'yellow)
;       (hilit-translate define   'AntiqueWhite)
;       (hilit-translate formula  'seagreen1)
;       (hilit-translate label    'AntiqueWhite)
;       (hilit-translate type     'yellow3)
        (add-hook 'recenter-hook 'hilit-rehighlight-buffer)
;       (add-hook 'auto-save-hook 'hilit-rehighlight-buffer)
        (global-set-key "\C-l"  'hilit-recenter)
))

;;
;; Miscellaneous
;;
(display-time)
(setq visible-bell t)        ; turn bell off
(setq line-number-mode t)    ; display the current line #
(setq column-number-mode t)  ; see mode-line-format, %c for column in v19.29
;; (global-set-key [menubar tools ange-ftp] 'internal-ange-ftp-mode)
;;
;; Ready to Roll
;;

;; end
