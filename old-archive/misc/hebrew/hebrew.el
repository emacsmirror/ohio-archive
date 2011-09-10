;;;
;;; hebrew.el -- Hebrew editing mode.
;;; For more information, see documentation of `hebrew-mode'.
;;;
;;; $Id: hebrew.el,v 1.4 1992/08/22 22:29:57 yossi Exp $
;;; Copyright (c) Joseph Friedman, April 1990
;;; E-mail: yossi@DEShaw.COM
;;;
;;; LCD Archive Entry:
;;; hebrew|Joseph Friedman|yossi@DEShaw.COM|
;;; Support for editing Hebrew files in Emacs.|
;;; 92-08-22|$Revision: 1.4 $|~/misc/hebrew.tar.Z|


(defvar running-epoch nil "*Non-nil means Epoch is running.")

(defvar hebrew-mode-hook () "*Called when entering hebrew-mode")

(defvar hebrew-mode nil "*Non nil when Hebrew mode is turned on.")
(make-variable-buffer-local 'hebrew-mode)

(or (assoc 'hebrew-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(hebrew-mode " Hebrew") minor-mode-alist)))

(defun hebrew-mode ()
"\
Toggles hebrew-mode on/off.  Hebrew-mode is a minor mode which allows
reversing the editor display and changing the keyboard in order to
enter and edit text in a semitic language, such as Hebrew.

By default, hebrew-mode stays in \"normal\" state.  The \"reversed\"
state is activated with \"\\[hebrew-toggle-reversed]\".  (In Epoch, this
is also bound to \"F3\".)  In the \"reversed\" state, hebrew-mode emulates
a Hebrew keyboard both during direct insertion and during searches.

Hebrew-mode calls hebrew-mode-hook upon entering."

  (interactive)
  (if (null hebrew-mode)
      (let (char)

	; signal hebrew minor mode in the mode-line
	(setq hebrew-mode t)

	; activate the hebrew insertion function
	(setq char ? )
	(local-set-key " " 'hebrew-insert-space) ; SPC is a special case,
	(setq char (1+ char))			  ;  see below.
	(while (<= char ?~)
	  (let ((s (format "%c" char)))
	    (if (eq (key-binding s) 'self-insert-command)
		(local-set-key s 'hebrew-insert)))
	  (setq char (1+ char)))
	; for showmatch:
	(local-unset-key "{")
	(local-unset-key "[")
	(local-unset-key "(")
	(local-unset-key ")")
	(local-unset-key "]")
	(local-unset-key "}")

	; display-literal, display-reversed
	(setq display-literal t)
	(local-set-key "\C-cR" 'hebrew-toggle-reversed)
	(local-set-key "\e[13~" 'hebrew-toggle-reversed) ; F3

	; syntax table stuff
	(setq char ?\340)	; aleph
	(while (<= char ?\372)	; taf
	  (modify-syntax-entry char "w")
	  (setq char (1+ char)))

	; search stuff
	(local-set-key "\C-s" 'hebrew-ctrl-s)
	(local-set-key "\C-r" 'hebrew-ctrl-r)

	; hooks
	(run-hooks 'hebrew-mode-hook))

    (let (char)

      ; turn off hebrew minor mode in the mode-line
      (setq hebrew-mode nil)

      ; deactivate the hebrew insertion function
      (setq char ? )
      (while (<= char ?~)
	(let ((s (format "%c" char)))
	  (if (eq (key-binding s) 'hebrew-insert)
	      (local-unset-key s)))
	(setq char (1+ char)))

      ; deactivate display-literal and display-reversed
      (setq display-literal nil)
      (local-unset-key "\C-cR")
      (local-unset-key "\e[13~") ; F3

      ; search stuff
      (local-unset-key "\C-s")
      (local-unset-key "\C-r")

      ; restore syntax-table
      (setq char ?\340)		; aleph
      (while (<= char ?\372)	; taf
	(modify-syntax-entry char " ")
	(setq char (1+ char))))))

(if running-epoch
    (epoch::rebind-key "F3" 0 "\C-cR"))
(defun hebrew-toggle-reversed ()
  "Toggle whether or not the display is laterally reversed."
  (interactive)
  (setq display-reversed (null display-reversed))
  (if running-epoch
      (epoch::redisplay-screen)
    (x-smart-redisplay)))

(defun hebrew-insert (&optional arg)
  "\
If display-reversed is nil, behaves like self-insert-command.
If display-reversed is t, simulates a Hebrew typewriter keyboard."

  (interactive "p")
  (or (numberp arg) (setq arg 1))
  (let ((char (if display-reversed
		  (aref hebrew-keyboard last-command-char)
		(format "%c" last-command-char))))
    (while (> arg 0)
      (insert char)
      (setq arg (1- arg)))))

; *** KLUDGE ALERT ***
; When last-command-char is SPC and display-reversed is t,
; self-insert-command does not update the display after insertion, and I
; couldn't figure out why this is so.  To get around this bug, I call
; self-insert-command from a user routine, and this takes care of updating
; the display.
(defun hebrew-insert-space (&optional arg)
"This is a kludge to get around an insertion bug.  Bind to SPC."
  (interactive)
  (cond
   ((null arg) (self-insert-command 1))
   ((numberp arg) (self-insert-command arg))
   (t (ding))))

(defun hebrew-ctrl-s () "The Hebrew-mode version of \C-s"
  (interactive)
  (let ((old-read-char (symbol-function 'read-char))
	(old-text-char-description (symbol-function 'text-char-description))
	(search-exit-option nil)
	(res nil)
	ERR)
    (fset 'read-char (symbol-function 'hebrew-read-char))
    (fset 'text-char-description
	  (symbol-function 'hebrew-text-char-description))
    (condition-case ERR
      (setq res (funcall (global-key-binding "\C-s")))
      (error  nil)
      (quit nil))
    (fset 'read-char old-read-char)
    (fset 'text-char-description old-text-char-description)
    res))

(defun hebrew-ctrl-r () "The Hebrew-mode version of \C-r"
  (interactive)
  (let ((old-read-char (symbol-function 'read-char))
	(old-text-char-description (symbol-function 'text-char-description))
	(search-exit-option nil)
	(res nil)
	ERR)
    (fset 'read-char (symbol-function 'hebrew-read-char))
    (fset 'text-char-description
	  (symbol-function 'hebrew-text-char-description))
    (condition-case ERR
      (setq res (funcall (global-key-binding "\C-r")))
      (error nil)
      (quit nil))
    (fset 'read-char old-read-char)
    (fset 'text-char-description old-text-char-description)
    res))

(defun hebrew-read-char ()
  "The hebrew-mode version of read-char."
  (let ((char (funcall old-read-char)))
    (if (and display-reversed (>= char ? ) (<= char ?~))
	(string-to-char (aref hebrew-keyboard char))
      char)))

(defun hebrew-text-char-description (char)
  "The hebrew-mode version of text-char-description."
  (if display-reversed
      (char-to-string char)
    (funcall old-text-char-description char)))

; for the search function to work, we can't use a byte-compiled version of
; isearch.el, so load it back:
(load-library "isearch.el")

(setq hebrew-keyboard
      [
       ""     ""     ""     ""     ""     ""     ""     ""
       ""     ""     ""     ""     ""     ""     ""     ""
       ""     ""     ""     ""     ""     ""     ""     ""
       ""     ""     ""     ""     ""     ""     ""     ""
       " "    "!"    "\""   "#"    "$"    "%"    "&"    "'"
       "("    ")"    "*"    "+"    "\372" "-"    "\365" "."
       "0"    "1"    "2"    "3"    "4"    "5"    "6"    "7"
       "8"    "9"    ":"    "\363" "<"    "="    ">"    "?"
       "@"    "A"    "B"    "C"    "D"    "E"    "F"    "G"
       "H"    "I"    "J"    "K"    "L"    "M"    "N"    "O"
       "P"    "Q"    "R"    "S"    "T"    "U"    "V"    "W"
       "X"    "Y"    "Z"    "["    "\\"   "]"    "^"    "_"
       ";"    "\371" "\360" "\341" "\342" "\367" "\353" "\362"
       "\351" "\357" "\347" "\354" "\352" "\366" "\356" "\355"
       "\364" "\\"   "\370" "\343" "\340" "\345" "\344" ","
       "\361" "\350" "\346" "{"    "|"    "}"    "~"    ""])
