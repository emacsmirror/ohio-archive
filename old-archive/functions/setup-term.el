;From: jbw@USWEST.COM (Joe Wells)
;Newsgroups: gnu.emacs.bug
;Subject: binding keys consistently under different interfaces
;Message-ID: <8907191758.AA01264@ketchum.uswest.com>
;Date: 19 Jul 89 17:58:17 GMT
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 291
;
;jsol@BU-IT.BU.EDU writes:
; > One of our users has extensively configured EMACS to work with
; > suntools using the sun-raw-map feature. He wants to make this
; > work with X11R3. Does anyone have a non-painful way for this to
; > happen? Specifically he has bound all of his keyboard characters
; > (the ones on the top of the keyboard, labelled F1 through Fsomething)
; > to useful functions within emacs and wants to keep the "look and feel"
; > of the emacs configuration he used to However.
;
;There may be a simple solution.  If your user is running Emacs in an xterm
;window, there is a option to xterm that tells it to use the same character
;sequences for function keys that are used on the Sun console or under
;suntools in a tty subwindow.  Another alternative is to run Emacs as a
;separate X client, in which case I believe the function key character
;sequences also follow the Sun standard.
;
;However, in case neither of those solutions work, I am including a
;function that I use, along with a demonstration of its use.  I have
;written a function that will bind the Sun 3 function keys consistently in
;all of the environments with which I am familiar.  You'll will have to
;look at it and see what you can use.  I hope this helps.
;
;--
;Joe Wells <jbw@uswest.com>
;jbw%ketchum.uswest.com@boulder.colorado.edu
;----------------------------------------------------------------------
;; Last changed: Wed Jul 19 11:49:22 1989 by jbw ( Joe Wells #360 x2392) on ketchum
;; Sun specific stuff

;; Xterm:
;; L1-4: "\e[23~" through "\e[26~"
;; L5-6: "\e[28~" through "\e[29~"
;; L7-10: "\e[31~" through "\e[34~"
;; F1-5: "\e[11~" through "\e[15~"
;; F6-9: "\e[17~" through "\e[20~"
;; Setup Sun 3 function key bindings for most environments
;; normal S C M C-S M-S C-M C-M-S
(defun define-sun3-function-key (key &rest binding-list)
  "Bind Sun 3 function key KEY to BINDING.  Under the Emacstool
environment, additional BINDINGS may be specified after the first.  In
that case the first BINDING affects the unshifted function key, and the
subsequent bindings affect these modifications of the key, in this order:
shift-key, control-key, meta-key, control-shift-key, meta-shift-key,
control-meta-key, control-meta-shift-key.  If all eight bindings are not
specified, the last binding specified is used for the rest.  Under other
environments, all BINDINGS after the first are ignored.  If no binding is
specified, the function undefined is used."
  (if (symbolp key)
      (setq key (symbol-name key))
    (if (not (stringp key))
	(error "Key must be a string or symbol")))
  (or binding-list
      (setq binding-list '(undefined)))
  (let* ((key-group (aref key 0))
	 (key-number (car (read-from-string key 1)))
	 (emacstool-number-letter (+ key-number ?a -1))
	 (emacstool-group-letter (cond ((memq key-group '(?L ?l)) ?l)
				       ((memq key-group '(?R ?r)) ?r)
				       ((memq key-group '(?F ?f ?T ?t)) ?t)))
	 (raw-group-number (cdr (assq emacstool-group-letter
				      '((?l . 191) (?r . 207) (?t . 223)))))
	 (xterm-group-number (cdr (assq emacstool-group-letter
					'((?l . 22) (?t . 10) (?r . nil)))))
	 (xterm-number-offset
	  (cond ((eq emacstool-group-letter ?l)
		 (cond ((>= key-number 7) 2)
		       ((>= key-number 5) 1)
		       (t 0)))
		((eq emacstool-group-letter ?t)
		 (if (>= key-number 6) 1 0))
		(t nil)))
	 (emacstool-shift-values '(0 -32 -64 128 -96 96 64 32))
	 (raw-key-string
	  (concat (prin1-to-string (+ raw-group-number key-number)) "z"))
	 (xterm-key-string
	  (if xterm-group-number
	      (concat (prin1-to-string
		       (+ xterm-group-number key-number xterm-number-offset))
		      "~")))
	 (arrow-key-string
	  (cdr (assoc key '(("R8" . "A") ("R10" . "D")
			    ("R12" . "C") ("R14" . "B")))))
	 key-string)
    ;; Raw Sun key binding
    (define-key sun-raw-map raw-key-string (car binding-list))
    ;; Xterm key binding
    (if xterm-key-string
	(define-key sun-raw-map xterm-key-string (car binding-list)))
    ;; Arrow key binding
    (if arrow-key-string
	(define-key sun-raw-map arrow-key-string (car binding-list)))
    ;; Emacstool key bindings
    (while (consp emacstool-shift-values)
      (setq key-string (concat (list emacstool-number-letter)
			       (list (+ emacstool-group-letter
					(car emacstool-shift-values)))))
      (define-key suntool-map key-string (or (car binding-list) 'undefined))
      (if (cdr binding-list)
	  (setq binding-list (cdr binding-list)))
      (setq emacstool-shift-values (cdr emacstool-shift-values)))))

;; Setup necessary keymaps, variables, and functions
(or (and (boundp 'sun-raw-map) sun-raw-map)
    (setq sun-raw-map (make-sparse-keymap)))
(or (and (boundp 'suntool-map) suntool-map)
    (setq suntool-map (make-sparse-keymap)))
(or (not sun-esc-bracket)
    (eq (lookup-key esc-map "[") sun-raw-map)
    (eq (lookup-key esc-map "[") 'sun-raw-map)
    (define-key esc-map "[" 'sun-raw-map))
(or (fboundp 'undefined)
    (fset 'undefined
	  '(lambda nil
	     (interactive)
	     (byte-code "\300\210\301 \210\302\303!\207"
			[nil beep message "Undefined key sequence."] 3))))
(setq meta-flag t)

(provide 'sun-stuff)
;----------------------------------------------------------------------
;; This is in my .emacs file.
;; You will have to delete references to some of the stuff that I use to
;; make all of this work.

(autoload 'define-sun3-function-key "sun-stuff")
(setq term-setup-hook 'setup-term)

(defun term-name-prefix (name prefix)
  "Returns true if NAME begins with PREFIX.  Both must be strings."
  (and (>= (length name) (length prefix))
       (string-equal (substring name 0 (length prefix)) prefix)))

(defun setup-term ()
  "Setup keymaps based on the terminal type.  This should be run after
the file in lisp/term/<type>.el has been run.  It can be run as the
value of term-setup-hook."
  (interactive)
  (cond ((term-name-prefix term "vt2")
	 (enable-arrow-keys)
	 (setup-arrow-keys)
	 (setup-terminal-keymap CSI-map
	   '(("A" . previous-line)      ; cursor up (normal)
	     ("B" . next-line)          ; cursor down (normal)
	     ("C" . forward-char)       ; cursor forward (normal)
	     ("D" . backward-char)      ; cursor backward (normal)
	     ;; VT220 keys
	     ("28~" . help-for-help)    ; "Help" key
	     ("29~" . execute-extended-command) ; "Do" key
	     ("1~" . re-search-forward) ; "Find" key
	     ("2~" . yank)              ; "Insert Here" key
	     ("3~" . kill-region)       ; "Remove" key
	     ("4~" . set-mark-command)  ; "Select" key
	     ("5~" . scroll-down-in-place) ; "Prev Screen" key
	     ("6~" . scroll-up-in-place) ; "Next Screen" key
	     ("17~" . delete-other-windows) ; F6
	     ("18~" . delete-window)    ; F7
	     ("19~" . enlarge-window)   ; F8
	     ("20~" . split-window-vertically) ; F9
	     ("21~" . other-window)     ; F10
	     ("23~" . ESC-prefix)       ; F11 (ESC)
	     ("24~" . bury-buffer)      ; F12 (BS)
	     ("25~" . buffer-menu)      ; F13 (LF)
	     ("26~" . switch-to-buffer) ; F14
	     ("31~" . delete-indentation) ; F17
	     ("32~" . delete-horizontal-space) ; F18
	     ("33~" . loop-word)        ; F19
	     ("34~" . loop-grab)))      ; F20
	 ;; VT100 and VT220 keypad keys
	 (setup-terminal-keymap SS3-map
	   '(("A" . previous-line)      ; cursor up (application)
	     ("B" . next-line)          ; cursor down (application)
	     ("C" . forward-char)       ; cursor fwd (application)
	     ("D" . backward-char)      ; cursor bwd (application)
	     ("p" . overwrite-mode)     ; numpad 0 (Ins)
	     ("q" . end-of-line)        ; numpad 1 (End)
	     ("r" . next-line)          ; numpad 2 (Crsr Dn)
	     ("s" . scroll-up-in-place) ; numpad 3 (Pg Dn)
	     ("t" . backward-char)      ; numpad 4 (Crsr Lf)
	     ("u" . recenter)           ; numpad 5 (nothing)
	     ("v" . forward-char)       ; numpad 6 (Crsr Rt)
	     ("w" . beginning-of-line)  ; numpad 7 (Home)
	     ("x" . previous-line)      ; numpad 8 (Crsr Up)
	     ("y" . scroll-down-in-place) ; numpad 9 (Pg Up)
	     ("m" . undefined)          ; numpad -
	     ("l" . undefined)          ; numpad ,
	     ("M" . newline)            ; numpad Enter
	     ("n" . delete-char)        ; numpad . (Del)
	     ("P" . undefined)          ; PF1
	     ("Q" . find-tag-at-point)  ; PF2
	     ("R" . describe-function-called-at-point) ; PF3
	     ("S" . describe-variable-at-point)))) ; PF4 (Sys Request)
	((term-name-prefix term "vt1")  ; working at home?
	 (setq search-slow-speed 9600)) ; ugly hack to fix emacs bug
	((term-name-prefix term "sun")
	 (mapcar (function
		  (lambda (x)
		    (apply 'define-sun3-function-key x)))
		 '((R1 find-tag-at-point)
		   (R2 describe-function-called-at-point)
		   (R3 describe-variable-at-point)
		   (R4 set-mark-command exchange-point-and-mark)
		   (R5 yank yank-pop)
		   (R6 kill-region copy-region-as-kill)
		   (R7 beginning-of-line beginning-of-buffer)
		   (R8 previous-line)
		   (R9 scroll-down-in-place backward-page)
		   (R10 backward-char)
		   (R11 recenter)
		   (R12 forward-char)
		   (R13 end-of-line end-of-buffer)
		   (R14 next-line)
		   (R15 scroll-up-in-place forward-page)
		   (L1)
		   (L2 redraw-display)	; Again
		   ;; M-S-Again: rerun-prev-command
		   ;; M-Again: prev-complex-command
		   (L3 buffer-menu)	; Props
		   (L4 undo)		; Undo
		   (L5)	                ; Expose
		   (L6 sun-select-region) ; Put
		   ;; C-Put: copy-region-as-kill
		   (L7)	                ; Open
		   (L8 sun-yank-selection) ; Get
		   ;; C-Get: yank
		   (L9 delete-horizontal-space) ; Find
		   ;; Find: research-forward (sun.el)
		   ;; C-Find: re-search-forward
		   ;; M-Find: research-backward (sun.el)
		   ;; C-M-Find: re-search-backward
		   (L10 delete-indentation) ; Delete
		   ;; Delete: kill-region-and-unmark
		   ;; S-Delete: yank
		   ;; M-Delete: exchange-point-and-mark
		   ;; C-Delete: interactive-pop-mark
		   (F1 re-search-forward)
		   (F2 switch-to-buffer)
		   (F3 bury-buffer)
		   ;; F3: scroll-down-in-place (sun.el)
		   ;; S-F3: scroll-down
		   (F4 split-window-vertically)
		   ;; F4: scroll-up-in-place (sun.el)
		   ;; S-F4: scroll-up
		   (F5 other-window)
		   (F6 shrink-window shrink-window-horizontally)
		   (F7 enlarge-window enlarge-window-horizontally)
		   (F8 delete-window)
		   (F9 delete-other-windows)))
	 (define-key ctl-x-map "\C-z" 'hide-emacstool)
	 (define-key ctl-x-map "z" 'hide-emacstool)
	 (define-key esc-map "z" 'hide-emacstool)
	 (setup-arrow-keys)
	 ;; Fix name conflict with lisp/term/sun.el
	 (fmakunbound 'scroll-down-in-place)
	 (fmakunbound 'scroll-up-in-place)
	 (autoload 'scroll-down-in-place "scroll-fix" nil t)
	 (autoload 'scroll-up-in-place "scroll-fix" nil t))
	((term-name-prefix term "xterm")
	 (setq SS3-map (make-keymap))
	 (setup-terminal-keymap SS3-map
	   '(("A" . previous-line)      ; up arrow
	     ("B" . next-line)          ; down-arrow
	     ("C" . forward-char)       ; right-arrow
	     ("D" . backward-char)      ; left-arrow
	     ("Q" . tab-to-tab-stop)    ; L8
	     ("R" . save-some-buffers)  ; L9
	     ("S" . other-window)))     ; L10
	 (define-key esc-map "O" SS3-map))
	((or (term-name-prefix term "h19")
	     (term-name-prefix term "z19"))
	 (setq SS3-map (make-keymap))
	 (setup-terminal-keymap SS3-map
	   '(("x" . previous-line)
	     ("r" . next-line)
	     ("v" . forward-char)
	     ("t" . backward-char)
	     ("p" . scroll-down)
	     ("M" . scroll-up)
	     ("n" . set-mark-command)
	     ("w" . beginning-of-buffer)
	     ("y" . end-of-buffer)
	     ("u" . other-window)))
	 (define-key esc-map "O" SS3-map))))

(defun undefined ()
  "If a key is bound to this function, that means it has been
deliberately undefined.  This function rings the bell and
prints a message saying the key sequence is undefined."
  (interactive)
  (beep)
  ;; and give us a message, since we always have the bell turned off
  (message "Undefined key sequence."))
