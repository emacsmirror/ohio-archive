; use it if you like it delete it otherwise

; turn on flow control

(set-input-mode nil t)

; Remap ^S and ^Q to ^\ and ^^ and exchange bacspace and delete

(setq keyboard-translate-table "\000\001\002\003\004\005\006\007\177\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\023\035\021\037 !\042#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\134]^_`abcdefghijklmnopqrstuvwxyz{|}~\010")

; more vi like, deleating will get emacs behaviour

(setq scroll-step 1)

; a directory of lisp stuff that can be loaded

(setq load-path (append (list "/usr/djs/GNUmacs") load-path))

; go into vi mode when a file is read in

(setq find-file-hooks '(mvi-emacs-to-vi))

; a vi like indent fuction for fundamental mode

(setq indent-line-function 'doug-indent)

; load the indent function

(load "doug-misc")

;  Needed for proper auto indent

(global-set-key "\C-m" 'doug-newline)
