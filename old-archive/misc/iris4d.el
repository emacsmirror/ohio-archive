;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!elroy.jpl.nasa.gov!jpl-devvax!larryc Fri May 18 15:57:19 1990
;Article 1944 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!elroy.jpl.nasa.gov!jpl-devvax!larryc
;>From larryc@jpl-devvax.JPL.NASA.GOV (Larry E. Carroll)
;Newsgroups: comp.emacs
;Subject: Silicon Graphics F key binding
;Message-ID: <8108@jpl-devvax.JPL.NASA.GOV>
;Date: 15 May 90 23:45:54 GMT
;Reply-To: larryc@devvax.JPL.NASA.GOV (Larry E. Carroll)
;Organization: Jet Propulsion Laboratory, Pasadena, CA
;Lines: 94
;
;Recently someone asked for a Silicon Graphics init file that would map the 
;function keys.  I wasn't able to respond then because I didn't have access 
;to news posting.  Hope this isn't too late to help.
;
;This is what worked for me.  I'm CCing it to comp.emacs because I'd 
;like to hear if there is a better way.
;
;First, I had version 18.54.  I also had a problem loading the default init 
;file, which I solved by putting the following alias in my .login file. 
;(Loading the default would not over-ride the redefinition below of the 
;DEL key.)
;
;  alias em ~/bin/emacs -q -l ~/.emacsinit
;
;In the emacs init file I included the following along with such things as 
;my preferred c-indent-level.
;
;  (load "~/emacs/lisp/term/iris4d.elc")
;
;In iris4d.el (later byte-compiled to iris4d.elc) I put the following.

  ;;  Change the Help prefix character; it conflicts with the BACKSPACE key.

  (define-key global-map "\C-\\" help-map)
  (global-set-key "C-h" 'backward-delete-char)

  ;;  Define the <escape left-bracket> ANSI function key prefix, in prep.
  ;;  for defining the Silicon Graphics keymap.

  (global-unset-key "\e[")
  (global-unset-key "\e]")
  (global-seat-key "\e{" 'backward-paragraph ;;  You may want a different
  (global-seat-key "\e}" 'forward-paragraph  ;;  key binding.

  (defvar iris-reg-map (make-sparse-keymap) "*Keymap for ESC-[ keys")
  (global-set-key "\e[" iris-reg-map)

  (define-key iris-reg-map "\177" 'delete-char)          ;; The DEL key

  ;;--------------------------------------------------------------------
  ;;  Define the Silicon Graphics regular keypad keymap.

  (define-key iris-reg-map "142q" 'kill-word)            ;; Ctrl-Delete

  (define-key iris-reg-map "A" 'previous-line)           ;; Up-arrow
  (define-key iris-reg-map "B" 'next-line)               ;; Down-arrow
  (define-key iris-reg-map "C" 'forward-char)            ;; Right-arrow
  (define-key iris-reg-map "D" 'backward-char)           ;; Left-arrow

  (define-key iris-reg-map "168q" 'forward-word)         ;; Ctrl-right-arrow
  (define-key iris-reg-map "159q" 'forward-word)         ;; Ctrl-right-arrow)
  (define-key iris-reg-map "158q" 'beginning-of-line)    ;; Shift-left-arrow
  (define-key iris-reg-map "167q" 'end-of-line)          ;; Shift-right-arrow

  (define-key iris-reg-map "154q" 'scroll-up)            ;; Page-up
  (define-key iris-reg-map "150q" 'scroll-down)          ;; Page-down
  (define-key iris-reg-map "156q" 'scroll-other-window)  ;; Ctrl-page-down
  ;;  Unfortunately for consistency, Ctrl-page-up produces nothing useful.

  (define-key iris-reg-map "H" 'beginning-of-buffer)     ;; Home
  (define-key iris-reg-map "146q" 'end-of-buffer)        ;; End

  (define-key iris-reg-map "139q" 'overwrite-mode)       ;; Insert

  ;;---------------------------------------------------------------------
  ;;  Define the SG function-key keymap.  (Until a good reason comes along
  ;;  to change it, it is the same as the regular-keypad map.)

  (define-key iris-reg-map "001q" 'whatever)             ;; F1
                 ...
  (define-key iris-reg-map "008q" 'you-choose)           ;; F8
				   
  (define-key iris-reg-map "P" ')             ;; F9 (Also made by Shift-Delete)
  (define-key iris-reg-map "Q" ')             ;; F10
  (define-key iris-reg-map "R" ')             ;; F11
  (define-key iris-reg-map "S" ')             ;; F12

  ;;---------------------------------------------------------------------
  ;;  Define the SG numeric keypad keymap.

  (define-key iris-num-map "n" 'delete-char)

  (define-key iris-num-map "x" 'previous-line)
  (define-key iris-num-map "r" 'next-line)
  (define-key iris-num-map "v" 'forward-char)
  (define-key iris-num-map "t" 'backward-char)

  (define-key iris-num-map "y" 'scroll-up)
  (define-key iris-num-map "s" 'scroll-down)

  (define-key iris-num-map "w" 'beginning-of-buffer)
  (define-key iris-num-map "q" 'end-of-buffer)

  (define-key iris-num-map "p" 'overwrite-mode)


