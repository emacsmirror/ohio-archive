;To: unix-emacs@bbn.com
;Date: 9 Mar 89 19:12:46 GMT
;From: Derrell Lipman <apple!vsi1!wyse!mips!prls!philabs!ttidca!retix!derrell@bbn.com>
;Subject: Re: The definitive story on VT200/VT300 keys (tell me it)
;
;In article <90@csv.viccol.edu.au> timcc@csv.viccol.edu.au writes:
;
;>I've seen a few messages in comp.emacs lately regarding the setting up of
;>VT200-series terminal keys in Emacs (including the one that mentioned that the
;>definition of ESC-[ could be a problem), so I went off and made what was about
;>my sixth attempt at (fully) understanding the Emacs manual section on defining
;>keys, and using that understanding to define some key-sequences that are
;>generated on a VT200.
;> 
;>I failed again.  Can someone who is a complete authority on making full use
;>of VT200 keys (LK201 actually), including multiple key sequences, post a full
;>rundown on how it is done.  I would also be interested in how you might
;>convince Emacs to interpret a multi-character sequence as the meta key.
;>
;
;
;following is a copy of a file which i use to bind the function keys on
;an esprit 6515 terminal running in VT220 emulation mode.  maybe this will
;help clear up the mystery.  one part of this which may not be obvious
;involves the use of the lisp function "mapcar".  i have included a brief
;explanation of that function below.
;
;note that the 'ENTER' key on the application keypad has been made into
;an additional META key.  search for 'Enter' to see how.
;
;disclaimer: i have not tried this stuff independent of all of the other
;	    lisp functions which we use, so i don't know if functions
;	    are called from here which are not in this listing.  it should,
;	    however, give the idea of how to bind function keys.
;
;--derrell



(defvar keybind-use-alt-defs t
  "Use Derrell's keybindings rather than Rob's originals")

(defvar keybind-use-swapped-delete-and-backspace t
  "If t, swap the actions of the delete key and the backspace key globally.")

(defun keybind-init ()
  (interactive)
  (keybind-application-keypad)
  (keybind-load-F-keys)
  (keybind-bind-F-keys)
  (keybind-define-key keybind-key-defs)
  (keybind-local-mods)
  (if keybind-use-alt-defs
      (progn
	(keybind-define-key keybind-alt-key-defs)
	(keybind-local-alt-mods)))
  (if keybind-use-swapped-delete-and-backspace
      (keybind-swap-delete-and-backspace)))

;; Function key definitions for VT220 function keys.
;; A key label like "S4" means "Shift F4"

(defconst keybind-key-defs '(
  ("[1~"    Find            find-file)
  ("\e[1~"   M-Find          dired-other-window)
  ("[2~"    Insert          insert-buffer)
  ("\e[2~"   M-Insert        insert-file)
  ("[3~"    Remove          kill-buffer)
  ("\e[3~"   M-Remove        delete-window)
  ("[4~"    Select          switch-to-buffer)
  ("\e[4~"   M-Select        buffer-menu)
  ("[5~"    Prev            scroll-down)
  ("\e[5~"   M-Prev          beginning-of-buffer)
  ("[6~"    Next            scroll-up)
  ("\e[6~"   M-Next          end-of-buffer)

  ("OA"     Up              previous-line)
  ("\eOA"    M-Up            top-of-screen)
  ("OB"     Down            next-line)
  ("\eOB"    M-Down          bot-of-screen)
  ("OC"     Right           forward-char)
  ("\eOC"    M-Right         end-of-line)
  ("OD"     Left            backward-char)
  ("\eOD"    M-Left          beginning-of-line)

  ("OP"     PF1             word-search-forward)
  ("\eOP"    M-PF1           word-search-backward)
  ("OQ"     PF2             isearch-forward-regexp)
  ("\eOQ"    M-PF2           isearch-backward-regexp)
  ("OR"     PF3             query-replace-regexp)
  ("\eOR"    M-PF3           global-toggle-case-fold-search)
  ("OS"     PF4             yank)
  ("\eOS"    M-PF4           yank-pop)

  ("Om"     Minus           kill-line)
  ("\eOm"    M-Minus         kill-region)
  ("Ol"     Comma           copy-region-as-kill)
  ("\eOl"    M-Comma         append-next-kill)
  ("OM"     Enter           ESC-prefix)
  ("\eOM"    M-Enter         quit-command)

  ("Ow"     7               backward-paragraph)
  ("Ox"     8               forward-paragraph)
  ("Oy"     9               none)
  ("Ot"     4               backward-sentence)
  ("\eOt"    M-4             backward-kill-sentence)
  ("Ou"     5               forward-sentence)
  ("\eOu"    M-5             kill-sentence)
  ("Ov"     6               none)
  ("Oq"     1               backward-word)
  ("\eOq"    M-1             backward-kill-word)
  ("Or"     2               forward-word)
  ("\eOr"    M-2             kill-word)
  ("Os"     3               exchange-point-and-mark)
  ("Op"     0               overwrite-or-insert)
  ("\eOp"    M-0             auto-fill-mode)
  ("On"     Dot             delete-char)
  ("\eOn"    M-Dot           bold-word)

  ("[11~"   F1              cmushell)
  ("[41~"   S1              fortune)
  ("\e[11~"  M-F1            date-and-time)
  ("\e[41~"  M-S1            yow)

  ("[14~"   F4              screen-normal)
  ("[44~"   S4              screen-wide)
  ("\e[14~"  M-F4            wide-side-by-side)
  ("\e[44~"  M-S4            wide-over-and-under)

  ("[17~"   F6              call-last-kbd-macro)
  ("[47~"   S6              label-last-kbd-macro)
  ("[18~"   F7              F7)
  ("[38~"   S7              S7)
  ("[19~"   F8              F8)
  ("[49~"   S8              S8)
  ("[20~"   F9              F9)
  ("[50~"   S9              S9)
  ("[21~"   F10             F10)
  ("[51~"   S10             S10)

  ("[23~"   F11             other-or-make-window)
  ("[53~"   S11             delete-other-windows)
  ("[24~"   F12             scroll-other-window)
  ("[54~"   S12             scroll-other-window-back)
  ("[25~"   F13             shorten-other-window)
  ("[55~"   S13             narrow-other-window)
  ("[26~"   F14             switch-to-buffer-other-window)
  ("[56~"   S14             find-file-other-window)
  ("\e[26~"   M-S14         toggle-read-only)


  ("[28~"   F15             help-for-help)
  ("[58~"   S15             help-key-help)
  ("[29~"   F16             undo)
;;  ("[59~"   S16             history)
  ("[59~"   S16             electric-command-history)
  ("\e[29~"  M-F16           repeat-complex-command)
  ("\e[59~"  M-S16           eval-expression)

  ("[31~"   F17             next-error)
  ("[61~"   S17             compile)
  ("[32~"   F18             none)
  ("[33~"   F19             rectangle)
  ("[34~"   F20             where-am-i)
  ("[64~"   S20             goto-line)
  ("\e[34~"  M-F20           skip-66-forward)
  ("\e[64~"  M-S20           skip-66-backward)
) "Rob's original VT220 special keys and bindings")

(defconst keybind-alt-key-defs
  '(
    ("OP"     PF1             beginning-of-buffer)
    ("\eOP"    M-PF1           none)
    ("OQ"     PF2             end-of-buffer)
    ("\eOQ"    M-PF2           none)
    ("Ow"     7               backward-kill-word)
    ("Ox"     8               previous-line)
    ("Oy"     9               kill-word)
    ("Ot"     4               backward-char)
    ("\eOt"    M-4            none)
    ("Ou"     5               newline-and-indent)
    ("\eOu"    M-5            none)
    ("Ov"     6               forward-char)
    ("Oq"     1               backward-word)
    ("\eOq"    M-1            none)
    ("Or"     2               next-line)
    ("\eOr"    M-2            none)
    ("Os"     3               forward-word)
    ("Op"     0               overwrite-or-insert)
    ("\eOp"    M-0             auto-fill-mode)
    ("On"     Dot             delete-char)
    ("\eOn"    M-Dot           bold-word)
    
    ("[11~"   F1              cmushell)
    ("[41~"   S1              fortune)
    ("\e[11~"  M-F1            run-dbx)
    ("\e[41~"  M-S1            yow)
    ("[32~"  F18		add-modification-tag)
    ("[62~"    S18		  add-change-log-entry)
    ("\e[32~"  M-F18		  show-modifications-toggle)
    ("\e[62~"  M-S18		  add-change-log-entry-new-project)
) "Derrell's special keys and bindings")

(defun keybind-application-keypad ()
  (send-string-to-terminal "\033[?1h\033="))  ; Force App cursor and keypad

(defun keybind-redefine-function-key (pair) 
  ; Make F-keys generate consistent escape sequences (NOT command names)
  ; See terminal reference manual under DECUDK
  (send-string-to-terminal
    (format "\033P1;1|%s/1B5B%x%x7E\033\\"
      (car pair) (elt (cdr pair) 0) (elt (cdr pair) 1))))

;;;
;;; Brief explanation of mapcar:
;;;	call the function specified by the first parameter (in this case
;;;	keybind-redefine-function-key) multiple times.  each time it is
;;;	called, the next element in the list specified by parameter two
;;;	is used as the parameter.  i.e., keybind-redefine-function-key is
;;;	called the first time with '("17" . "47"), and then with
;;;	'("18" . "48") the second time, etc.

;;;	note that the first number in each of these pairs is the value
;;;	used to identify a particular function key to be programmed
;;;	(17 refers to shifted function key 6 which we call S6) and the
;;;	second number in the pair is the variable part of what we want
;;;	the function key to send (S6 will send \e[47~).  this procedure
;;;	of actually programming what you want the function keys to send
;;;	may be particular to our esprit terminals and not generic to
;;;	VT220's.  i'm not sure.
;;;
(defun keybind-load-F-keys ()
  (mapcar 'keybind-redefine-function-key
    '(                ("17" . "47")                               ; S6
       ("18" . "48")  ("19" . "49") ("20" . "50")  ("21" . "51")  ; S7-S10
       ("23" . "53")  ("24" . "54") ("25" . "55")  ("26" . "56")  ; S11-S14
       ("28" . "58")  ("29" . "59") ("31" . "61")  ("32" . "62")  ; S15-S18
       ("33" . "63")  ("34" . "64")                               ; S19-S20
       ;; Add some normally empty keys
       ("41" . "11")  ("11" . "41")                               ; F1  S1
       ("44" . "14")  ("14" . "44")                               ; F4  S4
     )))

(defun keybind-bind-F-keys ()
  ;; Bind all Function Keys to default function none().
  (global-unset-key "\e\e")		;allow using this as a prefix!
  (global-unset-key "\e[")		;ditto!

  ;; this use of mapcar is calling global-set-key for the function key
  ;; and the function key preceeded with ESC (to make meta function key).
  ;; note that a function name is not used as the first parameter to mapcar;
  ;; lambda is used to "define a function on the fly".  it creates a function
  ;; which takes one parameter, but the function has no name and can not be
  ;; referenced from elsewhere.
  (mapcar
    '(lambda (x)
       (global-set-key (concat "\e["   x "~") 'none)
       (global-set-key (concat "\e\e[" x "~") 'none))
    '( "1"  "2"  "3"  "4"  "5"  "6"
       "11" "41" "14" "44"
       "17" "47" "18" "38" "19" "49" "20" "50" "21" "51"
       "23" "53" "24" "54" "25" "55" "26" "56"
       "28" "58" "29" "59"
       "31" "61" "32" "62" "33" "63" "34" "64")))

(defun keybind-define-key (arg-defs)
   ;; Set global bindings for Function Keys.
   (mapcar
     '(lambda (x)
        (global-set-key (concat "\e" (elt x 0)) (elt x 2)))
     arg-defs))

;; Dummies for keyboard macro binding
(mapcar '(lambda (x) (fset x "\007")) '(F7 S7 F8 S8 F9 S9 F10 S10))

;;; keybind hooks

(defun keybind-edit-picture-hooker ()
  (mapcar '(lambda (x) (define-key picture-mode-map (car x) (cdr x)))
                  ; In picture-mode            ; Normally
    '(("\eOA"     . picture-move-up)           ; previous-line
      ("\eOB"     . picture-move-down)         ; next-line
      ("\eOC"     . picture-forward-column)    ; forward-char
      ("\eOD"     . picture-backward-column)   ; backward-char
      ("\eOm"     . picture-clear-line)        ; kill-line
      ("\eOn"     . picture-clear-column)      ; delete-char
      ("\e\eOC"   . picture-end-of-line)       ; end-of-line
      ("\e\eOA"   . picture-motion-reverse)    ; top-of-screen
      ("\e\eOB"   . picture-motion)            ; bot-of-screen
      )))

(defun keybind-lisp-interaction-mode-hooker ()
  (define-key lisp-interaction-mode-map "\e[3~"	; Remove
    '(lambda ()
       (interactive)
       (funcall 'erase-buffer))))


(defun keybind-cmushell-mode-hooker ()

  (define-key cmushell-mode-map "\e[11~"
    '(lambda (arg)
       (interactive "P")
       (if arg
	   (cmushell arg)
	 (switch-to-buffer (other-buffer))))) ; F1

  (define-key cmushell-mode-map "\e[3~"
    '(lambda ()
	(interactive)
	(kill-region (point-min) (point-max))
	(comint-send-input)
	(message " All lines killed in this buffer."))))   ; Remove


(defun keybind-shell-mode-hooker ()

  (define-key shell-mode-map "\e[11~"
    'switch-to-number-shell-or-buffer-previous) ; F1

  (define-key shell-mode-map "\e[3~"
    'shell-clear-buffer)   ; Remove

  (define-key shell-mode-map "\eOM\e[29~"
    'shell-repeat))    ; M-F16


;--derrell    (derrell@retix)
;
;------------------------------------------------------------------------------
;airplane:
;  (n) A hole in the air, surrounded by metal, into which you
;      continually throw money.

