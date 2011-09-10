; Date: Tue, 18 Sep 90 16:51:27 EDT
; From: Ken Laprade <laprade@trantor.harris-atd.com>
; Subject: Generic function key binding
; 
; Here is a function I came up with to generically setup function key
; definitions.  There is a different definition of the function for epoch, X,
; or emacstool.  The same invocations of the function can be used in all
; cases.  Thus these bindings work for me no matter which emacs I run:
; 
;	(define-function-key "F1" 'describe-function)
;	(define-function-key "M-F1" 'describe-variable)
;	(define-function-key "R5" "\C-V")
;	(define-function-key "M-R5" 'scroll-other-window)
;	(define-function-key "R6" "\M->")
;	(define-function-key "M-R6" 'end-of-buffer-other-window)
;	(define-function-key "S-R5" 'scroll-other-screen-up)
;	(define-function-key "S-R6" 'end-of-buffer-other-screen)
;
; These functions are written with a Sun3 keyboard in mind, but they could be
; easily adapted to anything.
;
; This is the epoch version:
;
; -- 
; Ken Laprade			INTERNET: laprade@trantor.harris-atd.com
; Harris Corporation 		Usenet:  ...!uunet!x102a!trantor!laprade
; PO Box 37, MS 3A/1912		Voice: (407)727-4433
; Melbourne, FL 32902		FAX: (407)729-2537
;
; ----------

;;; Support for generic function key binding (by Ken Laprade):
;;;
(setq with-shift (epoch::mod-to-shiftmask 0))
(setq with-control (epoch::mod-to-shiftmask 2))
(setq with-meta (epoch::mod-to-shiftmask 3))

(defvar function-key-prefix-string "\C-X*"
  "Prefix for function key definitions created by define-function-key.")

(defun define-function-key (key def &optional epoch-def)
  "Globally define a generic function key for epoch.  KEY is a string naming
the key (as known to X) with optional `M', `S', or `C' prefixes specifying
meta, shift and control qualifiers.  Prefixes are separated from the key
name by a hyphen.  DEF is the key definition.  It may be any valid
ddefinition for define-key.  If it is a string, it will be directly bound
to the key using rebind-key.  If the optional EPOCH-DEF is supplied, it
is used rather than def."
  (interactive "sFunction key name: \nxDefinition: ")
  (if epoch-def (setq def epoch-def))
  (let* ((i (string-match "-" key))
	 (keyname (let ((name (substring key (if i (1+ i) 0))))
		    (cond ((string-match "R8" name) "Up")
			  ((string-match "R10" name) "Left")
			  ((string-match "R12" name) "Right")
			  ((string-match "R14" name) "Down")
			  (t name))))
	 (meta (string-match "M.*-" key))
	 (shift (string-match "S.*-" key))
	 (control (string-match "C.*-" key))
	 (quals (logior (if meta with-meta 0)
			(if shift with-shift 0)
			(if control with-control 0))))
    (if (stringp def)
	(rebind-key keyname quals def)
      ;; Make an intermediate binding for the definition.
      (let* ((keytype (cond ((string-match "L\\([1-9]\\|10\\)$" key) "L")
			    ((string-match "F\\([1-9]\\)$" key) "F")
			    ((string-match "R\\([1-9]\\|1[0-5]\\)$" key) "R")
			    (t keyname)))
	     (keynum (and (match-beginning 1)
			  (string-to-int (substring key (match-beginning 1) (match-end 1)))))
	     (binding (concat function-key-prefix-string
			      (if meta "M") (if shift "S") (if control "C")
			      keytype
			      (if keynum (format "%02d" keynum)))))
	(rebind-key keyname quals binding)
	(global-set-key binding def)))))
----------
This is the version for X emacs:
----------
;;; Support for generic function key binding (by Ken Laprade):
;;;
(defun define-function-key (key def &optional ignored)
  "Globally define a generic function key for Sun3 keyboards.  KEY is a
string naming the key (L1-L10, F1-F9, R1-15) with optional `M', `S', or `C'
prefixes specifying meta, control, and shift qualifiers.  Prefixes are
separated from the key name by a hyphen.  Raw keys (as from the normal X
version) do not provide the shift or control prefixes, but the emacstool
version does.  Both maps are setup by this function.  DEF is the key
definition.  It may be any valid definition for define-key."
  (interactive "sFunction key name: \nxDefinition: ")
  (let* ((i (string-match "-" key))
	 (keyname (let ((name (substring key (if i (1+ i) 0))))
		    (cond ((string-match "Up" name) "R8")
			  ((string-match "Left" name) "R10")
			  ((string-match "Right" name) "R12")
			  ((string-match "Down" name) "R14")
			  (t name))))
	 (meta (string-match "M.*-" key))
	 (shift (string-match "S.*-" key))
	 (control (string-match "C.*-" key))
	 (keybase (cond ((string-match "L\\([1-9]\\|10\\)$" keyname) 191)
			  ((string-match "F\\([1-9]\\)$" keyname) 223)
			  ((string-match "R\\([1-9]\\|1[0-5]\\)$" keyname) 207)
			  (t 0)))	; Unknown key.
	 (keynum (if (match-beginning 1)
		      (string-to-int (substring keyname (match-beginning 1) (match-end 1)))
		   0)))
    ;; The raw key version:
    (or shift control (not sun-esc-bracket)
	(global-set-key (format "%s[%03dz"
				(if meta "\M-\e" "\e")
				(+ keybase keynum))
			def))))

(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")
(defvar sun-meta-raw-map (make-sparse-keymap) "*Keymap for Meta-ESC-[ encoded keyboard")

(defvar sun-esc-bracket t
  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")

;;; Hook in the function key maps:
(if sun-esc-bracket
    (progn
      (define-key esc-map "[" sun-raw-map)		; Install sun-raw-map
      (global-set-key "\M-\e[" sun-meta-raw-map)	; Install sun-meta-raw-map
      (define-key esc-map "[A" 'previous-line )		; R8
      (define-key esc-map "[B" 'next-line)		; R14
      (define-key esc-map "[C" 'forward-char)		; R12
      (define-key esc-map "[D" 'backward-char)		; R10
      (define-key esc-map "[[" 'backward-paragraph)	; the original esc-[
      ))
----------
And this is what I threw into our local copy of term/sun.el:
----------
;;; Support for generic function key binding (by Ken Laprade):
;;;
(defun define-function-key (key def &optional ignored)
  "Globally define a generic function key for Sun3 keyboards.  KEY is a
string naming the key (L1-L10, F1-F9, R1-15) with optional `M', `S', or `C'
prefixes specifying meta, control, and shift qualifiers.  Prefixes are
separated from the key name by a hyphen.  Raw keys (as from the normal X
version) do not provide the shift or control prefixes, but the emacstool
version does.  Both maps are setup by this function.  DEF is the key
definition.  It may be any valid definition for define-key."
  (interactive "sFunction key name: \nxDefinition: ")
  (let* ((i (string-match "-" key))
	 (keyname (let ((name (substring key (if i (1+ i) 0))))
		    (cond ((string-match "Up" name) "R8")
			  ((string-match "Left" name) "R10")
			  ((string-match "Right" name) "R12")
			  ((string-match "Down" name) "R14")
			  (t name))))
	 (meta (string-match "M.*-" key))
	 (shift (string-match "S.*-" key))
	 (control (string-match "C.*-" key))
	 (keybase (cond ((string-match "L\\([1-9]\\|10\\)$" keyname) 191)
			  ((string-match "F\\([1-9]\\)$" keyname) 223)
			  ((string-match "R\\([1-9]\\|1[0-5]\\)$" keyname) 207)
			  (t 0)))	; Unknown key.
	 (keynum (if (match-beginning 1)
		      (string-to-int (substring keyname (match-beginning 1) (match-end 1)))
		   0)))
    ;; The raw key version:
    (or shift control (not sun-esc-bracket)
	(global-set-key (format "%s[%03dz"
				(if meta "\M-\e" "\e")
				(+ keybase keynum))
			def))
    ;; The emacstool version:
    (global-set-key (format "\C-X*%c%c"
			    (+ keynum ?`)
			    (+ (cond ((= keybase 191) ?l)
				     ((= keybase 223) ?t)
				     ((= keybase 207) ?r)
				     ((= keybase 0) ??))
			       (if control -64 0)
			       (if shift -32 0)
			       (if meta 128 0)))
		    def)))

(defvar sup-map (make-keymap) "Default keymap for SUP (meta-meta) commands.")
(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")
(defvar sun-meta-raw-map (make-sparse-keymap) "*Keymap for Meta-ESC-[ encoded keyboard")
(defvar suntool-map (make-sparse-keymap) "*Keymap for Emacstool bindings.")

;;; Hook in the function key maps:
(define-key ctl-x-map "*" suntool-map)
(if sun-esc-bracket
    (progn
      (define-key esc-map "[" sun-raw-map)		; Install sun-raw-map
      (define-key esc-map "\e" sup-map)			; Install sup-map
      (define-key sup-map "\e" sun-meta-raw-map)	; Install sun-meta-raw-map
      (define-key esc-map "[A" 'previous-line )		; R8
      (define-key esc-map "[B" 'next-line)		; R14
      (define-key esc-map "[C" 'forward-char)		; R12
      (define-key esc-map "[D" 'backward-char)		; R10
      (define-key esc-map "[[" 'backward-paragraph)	; the original esc-[
      ))
