;;; File:  edt-vt100.el, v 1.0
;;;
;;;            -----   -----------   -------   -----------------
;;;            E D T   K e y p a d   M o d e   E m u l a t i o n
;;;            -----   -----------   -------   -----------------
;;;
;;;       -----   ----------  ----------  ---------   -----------------
;;;       F o r   V T 1 0 0,  V T 2 0 0,  V T 3 0 0   T e r m i n a l s
;;;       -----   ----------  ----------  ---------   -----------------
;;;
;;;                        BETA Distribution Version 0.5
;;;
;;; Copyright (C) 1986 Free Software Foundation, Inc.
;;; Copyright (C) 1991 Kevin Gallagher
;;;
;;;
;;; This file contains GNU Emacs VT100, VT200, and VT300 series terminal
;;; specific EDT bindings and functions.
;;;
;;; Send bug reports and suggestions for improvement to Kevin Gallagher
;;; (kgallagh@digi.lonestar.org).
;;;

;;;;
;;;; KEY TRANSLATIONS
;;;;

(defvar *vt100-keys*
  '(("KP0" . "p") ("KP1" . "q") ("KP2" . "r") ("KP3" . "s") ("KP4" . "t")
    ("KP5" . "u") ("KP6" . "v") ("KP7" . "w") ("KP8" . "x") ("KP9" . "y")
    ("KP," . "l") ("KP-" . "m") ("KPP" . "n") ("KPE" . "M") ("PF1" . "P")
    ("PF2" . "Q") ("PF3" . "R") ("PF4" . "S") ("UP" . "A") ("DOWN" . "B")
    ("RIGHT" . "C") ("LEFT" . "D")))

(defvar *vt200-keys*
  '(("FIND" . "1~") ("INSERT" . "2~") ("REMOVE" . "3~") ("SELECT" . "4~")
    ("PREVIOUS" . "5~") ("NEXT" . "6~") ("F6" . "17~") ("F7" . "18~")
    ("F8" . "19~") ("F9" . "20~") ("F10" . "21~") ("F11" . "23~")
    ("F12" . "24~") ("F13" . "25~") ("F14" . "26~") ("HELP" . "28~")
    ("DO" . "29~") ("F17" . "31~") ("F18" . "32~") ("F19" . "33~")
    ("F20" . "34~") ("UP" . "A") ("DOWN" . "B") ("RIGHT" . "C")
    ("LEFT" . "D")))

;;;;
;;;; BINDS VT100/VT200/VT300 FUNCTION KEYS
;;;;
;; The function edt-bind-function-key is keyboard/terminal dependent.

;; The arrow keys appear in both maps, so special logic has been
;; added to ensure that they get defined in both maps.

(defun edt-bind-function-key 
    (function-key binding gold-binding &optional default)
  "Terminal specific function which binds commands to keyboard function keys in
the EDT Emulator."
  (let ((key-code (cdr (assoc function-key *vt100-keys*)))
        (key-sequence)
        (key-found nil))
    (if (not (null key-code))
        (progn
          (setq key-found t)
          (setq key-sequence (concat "\eO" key-code))
          (if default
              (progn
                (define-key edt-default-global-map key-sequence binding)
                (define-key 'edt-default-gold-map key-sequence gold-binding))
              (progn
                (define-key edt-user-global-map key-sequence binding)
                (define-key 'edt-user-gold-map key-sequence gold-binding)))))
    (setq key-code (cdr (assoc function-key *vt200-keys*)))
    (if (not (null key-code))
        (progn
          (setq key-sequence (concat "\e[" key-code))
          (if default
              (progn
                (define-key edt-default-global-map key-sequence binding)
                (define-key 'edt-default-gold-map key-sequence gold-binding))
              (progn
                (define-key edt-user-global-map key-sequence binding)
                (define-key 'edt-user-gold-map key-sequence gold-binding))))
        (if (not key-found)
            (error "%s is not a legal function key name" function-key)))))

;;;;
;;;; Setup extra default EDT key bindings.
;;;;

;;; The following function, edt-setup-extra-default-bindings, is
;;; optional.  If defined, it is called during EDT emulation setup.
;;; It permits the EDT emulation binding of extra function keys not
;;; found on VT100/VT200/VT300 terminals.
;;;
;;; Since this file IS the VT100/VT200/VT300 terminal file, there are no
;;; extra function keys to bind.  All the default EDT bindings for the
;;; standard VT100/VT200/VT300 keys are made in edt.el, so no bindings are
;;; made here.  See one of the other terminal specific files for an example
;;; of how this function is set up (edt-apollo.el, for example).


;;;;
;;;; SET UP ADDITIONAL PREFIX MAPS.
;;;;

;;; The following functions are also optional.  If defined, they are
;;; called during EDT emulation setup.  If a terminal generates more
;;; than one character sequence prefix for its function keys, then
;;; separate keymaps must be maintained for each additional prefix.
;;; The VT200/VT300 series terminals have "\eO" and "\e[" as function
;;; key prefixes.  These maps must be created and assigned to the
;;; prefix character sequences in the appropriate global map.
;;; 
;;; By default, edt.el sets up the initial set of prefix maps for a
;;; single prefix.  edt-bind-function-key, above, assigns the prefix
;;; "\eO" to those initial maps and "\e[" to the maps defined here.

(defun edt-additional-default-prefix-map-setup ()
  "Setup additional terminal specific default prefix maps."
  (define-prefix-command 'edt-vt200-default-prefix-map)
  (define-key edt-default-global-map "\e[" 'edt-vt200-default-prefix-map))

(defun edt-additional-user-prefix-map-setup ()
  "Setup additional terminal specific user prefix maps."
  (fset 'edt-vt200-user-prefix-map (copy-keymap 'edt-vt200-default-prefix-map))
  (define-key edt-user-global-map "\e[" 'edt-vt200-user-prefix-map))


;;;;
;;;; TERMINAL SPECIFIC VERSIONS OF COMMANDS
;;;;

;;; Screen width commands often need terminal specific enhancements.
;;;
;;; Need to tell VT100/VT200/VT300 terminals to use the appropriate
;;; character set.
;;;

(defun edt-set-screen-width-80 ()
  "Set screen width to 80 columns."
  (interactive)
  (send-string-to-terminal "\e[?3l")
  (set-screen-width 80)
  (message "%sScreen width 80"))

(defun edt-set-screen-width-132 ()
  "Set screen width to 132 columns."
  (interactive)
  (send-string-to-terminal "\e[?3h")
  (set-screen-width 132)
  (message "%sScreen width 132"))

;;;
;;; DEFAULT EDT KEYPAD HELP
;;;
;; The default keypad help function is defined in edt.el, so there is
;; no need to define on here. 

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
;; the EDT Emulation initialization.  (See edt-apollo.el for an example.)

;; A user version of this function is available: 
;;
;;          edt-user-customize-default-emacs-bindings
;;
;; If defined in edt-user.el, it will be called immediately after 
;; edt-customize-default-emacs-bindings and before EDT emulation
;; initialization begins.
