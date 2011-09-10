;;; File:  edt-apollo.el, v 1.1
;;;
;;;            -----   -----------   -------   -----------------
;;;            E D T   K e y p a d   M o d e   E m u l a t i o n
;;;            -----   -----------   -------   -----------------
;;;
;;;       -----   -----------   ---------------------   ---------------
;;;       F o r   A p o l l o   L o w - P r o f i l e   K e y b o a r d
;;;       -----   -----------   ---------------------   ---------------
;;;
;;;                        BETA Distribution Version 0.5
;;;
;;; Copyright (C) 1986 Free Software Foundation, Inc.
;;; Copyright (C) 1991 Kevin Gallagher
;;;
;;;   WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!
;;;
;;;     This EDT emulation for the Apollo Low-Profile Keyboard assumes
;;;     that you have applied Leonard N. Zubkoff's (lnz@lucid.com)
;;;     Apollo Modifications for GNU Emacs, Version 18.54, or later.
;;;
;;;   WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!
;;;
;;;
;;; This file contains GNU Emacs Apollo specific functions and Apollo
;;; Low-Profile Keyboard specific EDT bindings and functions.  
;;;
;;; The function edt-bind-function-key is based upon
;;; bind-apollo-function-key found in apollo.el.
;;;
;;; Send bug reports and suggestions for improvement to Kevin Gallagher
;;; (kgallagh@digi.lonestar.org).
;;;

;;; Leonard N. Zubkoff's Apollo Modifications for Version 18.54
;;; support the function keys differently than his method used in
;;; subsequent versions.  So, we need to give version 18.54 special
;;; handling.
;;;
;;; If the 1st 15 characters of the Emacs version string has been
;;; altered in a site specific installation of GNU Emacs, the
;;; following test may fail to locate the version string correctly.
;;; If this was done at your site, it was a bad idea and should be
;;; undone.

(setq edt-old-emacs (string-equal "18.54" (substring (emacs-version) 10 15)))

;;;;
;;;; KEY TRANSLATIONS
;;;;

;; Since apollo.el does not assign names to keypad keys until Version
;; 18.55, we do this here for Version 18.54.  (We chose to use the EDT
;; Emulation keypad names for the Apollo keypad keys to avoid the need
;; for two levels of translation, in case you are wondering why we did
;; not use the 18.55 version names.)
;;
;; Note that the Apollo numeric keypad has the same physical layout as
;; the VT100 keypad, except for the markings on two keys.  The Apollo
;; "+" keypad key is located where the VT100 keyboard has its "-"
;; keypad key.  And the Apollo "-" keypad key is located where the
;; VT100 has its "," keypad key.  So, when binding the Apollo "+" and
;; "-" keypad keys, we treat them as VT100 "-" and "," keypad keys,
;; in that order.
;;
;; Version 18.54 recognizes shifted keypad keys for all keypad keys
;; except the enter key.  It does NOT recognize control keypad
;; sequences.

(defvar *apollo-old-emacs-keypad*
  '(("KP0" . "0") ("KP1" . "1") ("KP2" . "2") ("KP3" . "3") ("KP4" . "4")
    ("KP5" . "5") ("KP6" . "6") ("KP7" . "7") ("KP8" . "8") ("KP9" . "9")
    ("KP-" . "+") ("KP," . "-") ("KPE" . "\^M") ("KPP" . ".")
    ("KP0S" . ")") ("KP1S" . "!") ("KP2S" . "@") ("KP3S" . "#") ("KP4S" . "$")
    ("KP5S" . "%") ("KP6S" . "^") ("KP7S" . "&") ("KP8S" . "*") ("KP9S" . "(")
    ("KP+S" . "=") ("KP-S" . "_") ("KPPS" . ",")))


;; VT100/VT200/VT300 function key and keypad keynames are mapped to
;; corresponding Apollo keynames, below.  This is done for keys with
;; the same function but different names on the two keyboards.  When
;; there is no obvious Apollo function key to map to a VT100/VT200/VT300
;; key, we map the key to the empty string.  This informs the function
;; edt-bind-function-key, defined later in this file, to ignore any
;; request to define such a key.  This is necessary because the file
;; edt.el attempts to bind ALL standard VT100/VT200/VT300 function keys,
;; by default.
;;
;; (EDT users will appreciate the fact that we did NOT follow Apollo's
;; lead and map PF1-PF4 to Apollo's F2-F5 keys, as is done in the
;; Apollo VT100 emulator!)

(defvar *apollo-edt-keys*
  '(("PF1" . "R3") ("PF2" . "R4") ("PF3" . "R5") ("PF4" . "R6") 
    ("UP" . "L8") ("DOWN" . "LE") ("LEFT" . "LA") ("RIGHT" . "LC")
    ("KP0" . "NP0") ("KP1" . "NP1") ("KP2" . "NP2") ("KP3" . "NP3") 
    ("KP4" . "NP4") ("KP5" . "NP5") ("KP6" . "NP6") ("KP7" . "NP7") 
    ("KP8" . "NP8") ("KP9" . "NP9") ("KP-" . "NPG") ("KP," . "NPF") 
    ("KPP" . "NPP") ("KPE" . "NPE") ("FIND" . "") ("INSERT" . "")
    ("REMOVE" . "") ("SELECT" . "") ("NEXT" . "") ("PREVIOUS" . "")
    ;; VT200 F10 is mapped to F0; F20 to Shifted F0; F11-F14 to 
    ;; Shifted F1-F4; F17-F19 to Shifted F7-F9; DO (F15) to Shifted F5.
    ;; HELP (F16) to Shifted F6.
    ("F10" . "F0") ("F11" . "F1S") ("F12" . "F2S") ("F13" . "F3S")
    ("F14" . "F4S") ("DO" . "F5S") ("HELP" . "F6S") ("F17" . "F7S")
    ("F18" . "F8S") ("F19" . "F9S") ("F20" . "F0S")))

;; Combine the list of EDT key names with the list of Apollo function
;; keys found in apollo.el.

(setq *apollo-edt-function-keys* 
      (append *apollo-edt-keys* *apollo-function-keys*
              ;; The return key is called CR in 18.54 and RET in 18.55 and above.
              ;; By mapping RET to CR, when using 18.54, RET will work in all
              ;; versions.
              (if edt-old-emacs '(("RET" . "CR")))))

;;;;
;;;; BINDS VT100/VT200/VT300 FUNCTION KEYS
;;;;
;; The function edt-bind-function-key is keyboard/terminal dependent.

;; The version below is somewhat messy because of the special handling
;; added to it to support the Apollo mods in Version 18.54.

(defun edt-bind-function-key 
    (function-key binding gold-binding &optional default)
  "Bind commands to a function key in the EDT Emulator."
  (catch 'edt-key-not-supported
    (let ((numeric-code (cdr (assoc function-key  *apollo-edt-function-keys*)))
          (key-sequence))
      (if (null numeric-code)
          (error "%s is not a legal function key name" function-key)
          (if (stringp numeric-code)
              (if (string-equal "" numeric-code)
                  (throw 'edt-key-not-supported t)
                  (setq numeric-code 
                        (cdr (assoc numeric-code *apollo-edt-function-keys*))))))
      ;; If numeric-code is null at this point and this is Emacs
      ;; Version 18.54, then function-key should be a keypad key,
      ;; which requires special handling.
      (if (null numeric-code)
          (if (not edt-old-emacs) 
              (error "%s is not set up properly in edt-apollo.el" function-key)
              (progn
                ;; Keypad keys require special handling in Emacs, Version 18.54.
                (setq numeric-code 
                      (cdr (assoc function-key *apollo-old-emacs-keypad*)))
                (if (null numeric-code)
                    (error "%s is not a legal function key name" function-key)
                    (progn
                      ;;One code for entire keypad.
                      (enable-apollo-function-key 158) 
                      (setq key-sequence (concat "\M-*\M-\C-~" numeric-code))))))
          (progn
            (enable-apollo-function-key numeric-code)
            (setq key-sequence 
                  (if edt-old-emacs
                      (concat "\M-*" (char-to-string numeric-code))
                      (concat "\C-^" 
                              (char-to-string 
                                (logior 72 (lsh numeric-code -6)))
                              (char-to-string 
                                (logior 64 (logand numeric-code 63))))))))
      (if default
          (progn
            (define-key edt-default-global-map key-sequence binding)
            (define-key 'edt-default-gold-map key-sequence gold-binding))
          (progn
            (define-key edt-user-global-map key-sequence binding)
            (define-key 'edt-user-gold-map key-sequence gold-binding))))))


;;;;
;;;; Setup extra default EDT key bindings.
;;;;

;;; The following function is optional.  If defined, it is called
;;; during EDT emulation setup.  It permits the binding of
;;; extra function keys not found on VT100/VT200/VT300 terminals.

(defun edt-setup-extra-default-bindings ()
  "Setup EDT default bindings for extra terminal keys."

  ;; Shifted PF Keys - Make some Apollo-like assignments.
  (edt-bind-function-key "R3S" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "R4S" 'save-buffer 'edt-key-not-assigned t)
  (edt-bind-function-key "R5S" 'apollo-abort 'edt-key-not-assigned t)
  (edt-bind-function-key "R6S" 'manual-entry 'edt-key-not-assigned t)

  ;; AGAIN Key
  (edt-bind-function-key "R2" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "R2S" 'toggle-read-only 'edt-key-not-assigned t)

  ;; Function Keys
  ;; These Apollo function keys are not bound in edt.el because VT200
  ;; series function keys F1-F5 do not send signals to the host.  So
  ;; we bind the Apollo keys here.
  (edt-bind-function-key "F1" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F2" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F3" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F4" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F5" 'edt-key-not-assigned 'edt-key-not-assigned t)

  ;; Other Keys
  ;;
  ;; Apollo mods, by default, do not support re-defining the following
  ;; keys, except when Emacs source changes are made to enable
  ;; modification of these keys.  So, the following key re-definitions
  ;; may be ignored at your site.  (See APOLLO.README that comes with
  ;; the Apollo distribution.)  In addition, Version 18.54 Apollo mods
  ;; do not support re-defining the DELETE key under any
  ;; circumstances!

  (edt-bind-function-key "RET" 'newline 'newline-and-indent t)
  (edt-bind-function-key "BS" 'edt-beginning-of-line 'delete-other-windows t)
  (if (not edt-old-emacs)
    (edt-bind-function-key "DEL" 'backward-delete-char-untabify 'delete-window t))
)

;;;;
;;;; SET UP ADDITIONAL PREFIX MAPS.
;;;;

;;; The following functions are also optional.  If defined, they are
;;; called during EDT emulation setup.  If a terminal generates more
;;; than one character sequence prefix for its function keys, then
;;; separate keymaps must be maintained for each additional prefix.
;;; For example, the VT200/VT300 series terminals have "\eO" and "\e["
;;; as function key prefixes.  These maps must be created and assigned
;;; to the prefix character sequences in the appropriate global map.

;;; Although the Apollo function keys in Emacs versions 18.55, and
;;; above, all have "\C-^" as a prefix, eight external "sub-prefix"
;;; maps are defined as part of the implementation.  In order to keep
;;; the EDT default bindings, the EDT user bindings, and the initial
;;; Emacs bindings independent, separate versions of these
;;; "sub-prefix" maps must be maintained for the three sets of
;;; bindings.  (See apollo.el.)
;;;
;;; Apollo Emacs version 18.54 has no additional prefix maps.
;;;

(if (not edt-old-emacs)
    (progn
      (defun edt-additional-default-prefix-map-setup ()
        "Setup additional terminal specific prefix maps."
        (fset 'edt-default-prefix (copy-keymap 'apollo-prefix))
        (define-key edt-default-global-map "\C-^" 'edt-default-prefix)
        (fset 'edt-default-prefix-1 (copy-keymap 'apollo-prefix-1))
        (fset 'edt-default-prefix-2 (copy-keymap 'apollo-prefix-2))
        (fset 'edt-default-prefix-3 (copy-keymap 'apollo-prefix-3))
        (fset 'edt-default-prefix-4 (copy-keymap 'apollo-prefix-4))
        (fset 'edt-default-prefix-5 (copy-keymap 'apollo-prefix-5))
        (fset 'edt-default-prefix-6 (copy-keymap 'apollo-prefix-6))
        (fset 'edt-default-prefix-7 (copy-keymap 'apollo-prefix-7))
        (fset 'edt-default-prefix-8 (copy-keymap 'apollo-prefix-8))
        (define-key 'edt-default-prefix "H" 'edt-default-prefix-1)
        (define-key 'edt-default-prefix "I" 'edt-default-prefix-2)
        (define-key 'edt-default-prefix "J" 'edt-default-prefix-3)
        (define-key 'edt-default-prefix "K" 'edt-default-prefix-4)
        (define-key 'edt-default-prefix "L" 'edt-default-prefix-5)
        (define-key 'edt-default-prefix "M" 'edt-default-prefix-6)
        (define-key 'edt-default-prefix "N" 'edt-default-prefix-7)
        (define-key 'edt-default-prefix "O" 'edt-default-prefix-8)
        )
      (defun edt-additional-user-prefix-map-setup ()
        "Setup additional terminal specific prefix maps."
        (fset 'edt-user-prefix (copy-keymap 'edt-default-prefix))
        (define-key edt-user-global-map "\C-^" 'edt-user-prefix)
        (fset 'edt-user-prefix-1 (copy-keymap 'edt-default-prefix-1))
        (fset 'edt-user-prefix-2 (copy-keymap 'edt-default-prefix-2))
        (fset 'edt-user-prefix-3 (copy-keymap 'edt-default-prefix-3))
        (fset 'edt-user-prefix-4 (copy-keymap 'edt-default-prefix-4))
        (fset 'edt-user-prefix-5 (copy-keymap 'edt-default-prefix-5))
        (fset 'edt-user-prefix-6 (copy-keymap 'edt-default-prefix-6))
        (fset 'edt-user-prefix-7 (copy-keymap 'edt-default-prefix-7))
        (fset 'edt-user-prefix-8 (copy-keymap 'edt-default-prefix-8))
        (define-key 'edt-user-prefix "H" 'edt-user-prefix-1)
        (define-key 'edt-user-prefix "I" 'edt-user-prefix-2)
        (define-key 'edt-user-prefix "J" 'edt-user-prefix-3)
        (define-key 'edt-user-prefix "K" 'edt-user-prefix-4)
        (define-key 'edt-user-prefix "L" 'edt-user-prefix-5)
        (define-key 'edt-user-prefix "M" 'edt-user-prefix-6)
        (define-key 'edt-user-prefix "N" 'edt-user-prefix-7)
        (define-key 'edt-user-prefix "O" 'edt-user-prefix-8)
        )
      )
    )

;;;;
;;;; TERMINAL SPECIFIC VERSIONS OF COMMANDS
;;;;

;;; Screen width commands often need terminal specific enhancements.
;;; The following two commands are specific to an Apollo 19-inch
;;; Landscape 1280x1024 Monitor.  The "execute-dm-command" lines
;;; produce windows of sizes 80 and 132 characters wide only if a
;;; fix-length font, such as f7x13, or equivalent, is in use.  These
;;; place the windows in a fixed location on the screen, which may or
;;; may not be acceptable to individual people.  Such users should
;;; write their own versions of these two commands and place them in
;;; edt-user.el. 
;;;
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; The following Apollo DM command will create an Emacs window to
;;; match the 80 column mode setting:
;;;
;;;   (230,0)dr;(980,996)cp -n Emacs -c 'e' /gnuemacs/emacs; wc -a
;;;
;;; You may also find the following DM key definitions of interest:
;;;
;;;   kd f2 wi Emacs -w; icon Emacs -W; wp Emacs -t ke
;;;   kd f2s (230,0)dr;(980,996)cp -n Emacs -c 'e' /gnuemacs/emacs; wc -a ke
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;
;;; SET APOLLO WINDOW 80
;;;

(defun edt-set-screen-width-80 ()
  "Set Apollo Emacs pad size to 80 columns."
  (interactive)
  (execute-dm-command "twb -l;dr;(230,0)wg;twb -r;twb -b;dr;(980,996)wg;tl")
  (set-screen-width 80))

;;;
;;; SET APOLLO WINDOW 132
;;;

(defun edt-set-screen-width-132 ()
  "Set Apollo Emacs pad size to 132 columns."
  (interactive)
  (execute-dm-command "twb -l;dr;(0,0)wg;twb -r;twb -b;dr;(1279,996)wg;tl")
  (set-screen-width 132))


;;;
;;; APOLLO MOUSE MARK
;;;

(defun edt-apollo-mouse-mark ()
  "Select the point at which to begin mouse editing functions."
  (interactive)
  (apollo-mouse-move-point)
  (set-mark-command nil)
  (point-to-register 1))

;;;
;;; APOLLO MOUSE COPY
;;;

(defun edt-apollo-mouse-copy ()
  "Copy text between mark and mouse cursor."
  (interactive)
  (apollo-mouse-move-point)
  (set-mark-command nil)
  (point-to-register 2)
  (if (eq (register-to-point 1) (register-to-point 2))
      nil 
      (kill-ring-save (register-to-point 1) (register-to-point 2))))

;;;
;;; APOLLO MOUSE CUT
;;;

(defun edt-apollo-mouse-cut ()
  "Cut text between mark and mouse cursor."
  (interactive)
  (apollo-mouse-move-point)
  (set-mark-command nil)
  (point-to-register 2)
  (if (eq (register-to-point 1) (register-to-point 2)) 
      nil 
      (kill-region (register-to-point 1) (register-to-point 2))))

;;;
;;; APOLLO MOUSE PASTE
;;;

(defun edt-apollo-mouse-paste ()
  "Paste text previously cut or copied."
  (interactive)
  (apollo-mouse-move-point)
  (set-mark-command nil)
  (point-to-register 1)
  (yank)
  (register-to-point 1))


;;;
;;; DEFAULT EDT KEYPAD HELP
;;;
;; This version is essentially identical to that defined in edt.el,
;; except that it shows the Apollo specific markings found on some of
;; the keys.
;;
;; Upper case commands in the keypad diagram below indicate that the
;; emulation should look and feel very much like EDT.  Lower case
;; commands are enhancements and/or additions to the EDT keypad
;; commands or are native Emacs commands.

(defun edt-keypad-help ()
  "
                              DEFAULT EDT Keypad Active

                                  +----------+----------+----------+----------+
   F7: Copy Rectangle             |Prev Line |Next Line |Bkwd Char |Frwd Char |
   F8: Cut Rect Overstrike        |   (UP)   |  (DOWN)  |  (LEFT)  | (RIGHT)  |
 G-F8: Paste Rect Overstrike      |Window Top|Window Bot|Bkwd Sent |Frwd Sent |
   F9: Cut Rect Insert            +----------+----------+----------+----------+
 G-F9: Paste Rect Insert         
  F11: ESC                            PF1        PF2        PF3        PF4
                       +----------+----------+----------+----------+----------+
                       |          |   GOLD   |   HELP   |  FNDNXT  |  DEL L   |
                       |  (AGAIN) |  (READ)  |  (EDIT)  |  (EXIT)  |  (HOLD)  |
                       |          |Mark Wisel|Desc Funct|   FIND   |  UND L   |
                       +----------+----------+----------+----------+----------+
  F12: Begining of Line           +----------+----------+----------+----------+
G-F12: Delete Other Windows       |   PAGE   |   SECT   |  APPEND  |  DEL W   |
  F13: Delete to Begin of Word    |    (7)   |    (8)   |    (9)   |   (+)    |
 HELP: Emacs Help                 |Ex Ext Cmd|Fill Regio| REPLACE  |  UND W   |
   DO: Execute extended command   +----------+----------+----------+----------+
                                  |  ADVANCE |  BACKUP  |   CUT    |  DEL C   |
  C-g: Keyboard Quit              |    (4)   |    (5)   |    (6)   |   (-)    |
G-C-g: Keyboard Quit              |   BOTTOM |    TOP   |   Yank   |  UND C   |
  C-h: Beginning of Line          +----------+----------+----------+----------+
G-C-h: Emacs Help                 |   WORD   |    EOL   |   CHAR   |   Next   |
  C-i: Tab Insert                 |    (1)   |    (2)   |    (3)   |  Window  |
  C-j: Delete to Begin of Word    | CHNGCASE |  DEL EOL |Quoted Ins|          |
  C-k: Define Key                 +---------------------+----------+  (ENTER) |
G-C-k: Restore Key                |         LINE        |SELECT/RES|          |
  C-l: Form Feed Insert           |         (0)         |    (.)   |   Query  |
  C-n: Set Screen Width 80        |      Open Line      |Center Lin|  Replace |
  C-r: Isearch Backward           +---------------------+----------+----------+
  C-s: Isearch Forward         
  C-t: Display the Time        
  C-u: Delete to Begin of Line   
  C-v: Redraw Display            
  C-w: Set Screen Width 132      
  C-z: Suspend Emacs             
G-C-\\: Split Window             
                                 
  G-b: Buffer Menu               
  G-c: Compile                   
  G-d: Delete Window             
  G-e: Exit                      
  G-f: Find File                 
  G-g: Find File Other Window    
  G-h: Keypad Help               
  G-i: Insert File               
  G-k: Toggle Capitalization Word
  G-l: Downcase Region           
  G-m: Save Some Buffers         
  G-n: Next Error                
  G-o: Switch to Next Window     
  G-q: Quit                      
  G-r: Revert File               
  G-s: Save Buffer               
  G-u: Upcase Region             
  G-v: Find File Other Window    
  G-w: Write file                
  G-y: EDT Emulation OFF         
  G-z: Switch to User EDT Key Bindings
  G-1: Delete Other Windows      
  G-2: Split Window              
  G-%: Go to Percentage          
  G- : Undo  (GOLD Spacebar)     
  G-=: Go to Line                
  G-`: What line"                

  (interactive)
  (describe-function 'edt-keypad-help))


;;;
;;; CUSTOMIZE DEFAULT EMACS BINDINGS
;;;
;; Some site-specific and/or computer-specific Emacs installations may
;; contain some built-in support for function keys on a given terminal
;; and may provide some default bindings to those keys.  Sometimes
;; these bindings are made by using Emacs macros to equate the
;; function key to control key, for example.  This means that when the
;; EDT emulation re-defines that control key, the function key default
;; definition is re-defined, as well, often causing undesired results.
;; To avoid this problem, when EDT emulation begins initialization, it
;; looks to see if the function edt-customize-default-emacs-bindings
;; has been defined.  If so, it calls this function before starting
;; the EDT Emulation initialization.

;; The Apollo default assignments to several Apollo function keys do,
;; indeed, have this problem, so we modify the bindings of those keys
;; to eliminate the problem. 

(defun edt-customize-default-emacs-bindings ()
  "Change some emacs defaults before EDT emulation initialization."
  (bind-apollo-function-key "LINE_DEL" 'edt-delete-entire-line)
  (bind-apollo-function-key "CHAR_DEL" 'edt-delete-character)
  (bind-apollo-function-key "L_BAR_ARROW" 'beginning-of-line)
  (bind-apollo-function-key "R_BAR_ARROW" 'end-of-line)
  (bind-apollo-function-key "R3S" 'apollo-find-file) ;Shift READ
  (bind-apollo-function-key "L2S" 'edt-undelete-line) ;Shift LINE DEL
  (bind-apollo-function-key "L3S" 'edt-undelete-character) ;Shift CHAR DEL
  (bind-apollo-function-key "LAS" 'edt-one-word-backward) ;Shift LEFT ARROW
  (bind-apollo-function-key "LCS" 'edt-one-word-forward) ;Shift RIGHT ARROW
  (bind-apollo-function-key "L8S" 
                            'edt-scroll-window-backward-line) ;Shift UP ARROW
  (bind-apollo-function-key "LES" 
                            'edt-scroll-window-forward-line) ;Shift DOWN ARROW
  (bind-apollo-function-key "LF" 'scroll-up) ;DOWN BOX ARROW
)

;; A user version of this function is available: 
;;
;;          edt-user-customize-default-emacs-bindings
;;
;; If defined in edt-user.el, it will be called immediately after 
;; edt-customize-default-emacs-bindings and before EDT emulation
;; initialization begins.
