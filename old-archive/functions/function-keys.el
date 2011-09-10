;;; $Id: function-keys.el,v 1.3 1992/10/28 21:42:04 rwhitby Exp $ */
;;; $File: ~elib/functions/function-keys.el $ */

;; LCD Archive Entry:
;; function-keys|Rod Whitby|rwhitby@research.canon.oz.au|
;; Provides define-function-key for Epoch and X Emacs.|
;; 1992-10-28|1.3|~/functions/function-keys.el.Z|

;;; Check if we are running under epoch
(if (boundp 'epoch::version)
(progn

;;; Turn off function key mapping
(setq epoch::function-key-mapping nil)

;;; Support for generic function key binding (originally by Ken Laprade):
;;;
(defvar function-key-prefix-string "\C-^*"
  "Prefix for function key definitions created by define-function-key.")

(defun define-function-key (keymap key def)
  "\
Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.
KEYMAP is a keymap. KEY is a string naming the key (as known to X) with
optional `M', `S', or `C' prefixes specifying meta, shift and control
qualifiers. Prefixes are separated from the key name by a hyphen.
DEF is the key definition.  It may be any valid definition for define-key.
If it is a string, it will be directly bound to the key using rebind-key."
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
	 quals)
    (if meta (setq quals (append '(meta) quals)))
    (if control (setq quals (append '(control) quals)))
    (if shift (setq quals (append '(shift) quals)))
    (or quals (setq quals 0))
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
	(define-key keymap binding def)))))

)) ;;; End test for epoch

;;; Check if we are running under X emacs
(if (and (not (boundp 'epoch::version))
	 (equal window-system 'x))
(progn

;;; Support for generic function key binding (originally by Ken Laprade):
;;;
(defun define-function-key (keymap key def)
  "\
Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.
KEYMAP is a keymap. KEY is a string naming the key (as known to X) with
optional `M', `S', or `C' prefixes specifying meta, shift and control
qualifiers. Prefixes are separated from the key name by a hyphen.
Raw keys (as from the normal X version) do not provide the shift or
control prefixes, but the emacstool version does.
Both maps are setup by this function.
DEF is the key definition.  It may be any valid definition for define-key."
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
			  ((string-match "F1\\([1-2]\\)$" keyname) 191)
			  ((string-match "R\\([1-9]\\|1[0-5]\\)$" keyname) 207)
			  (t 0)))	; Unknown key.
	 (keynum (if (match-beginning 1)
		     (string-to-int (substring keyname (match-beginning 1)
					       (match-end 1)))
		   0)))
    ;; The raw key version:
    (or shift control (not sun-esc-bracket)
	(define-key keymap (format "\e[%03dz" (+ keybase keynum)) def)
	)))

(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")

(defvar sun-esc-bracket t
  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")

;;; Hook in the function key maps:
(if sun-esc-bracket
    (progn
      (define-key esc-map "[" sun-raw-map)		; Install sun-raw-map
      (define-key esc-map "[-1z" nil)			; Undefined keys
      (define-key esc-map "[A" 'previous-line )		; R8
      (define-key esc-map "[B" 'next-line)		; R14
      (define-key esc-map "[C" 'forward-char)		; R12
      (define-key esc-map "[D" 'backward-char)		; R10
      (define-key esc-map "[[" 'backward-paragraph)	; the original esc-\[
      ))

)) ;;; End test for X emacs

;;; Check if we are running under a character based emacs
(if (and (not (boundp 'epoch::version))
	 (not (equal window-system 'x)))
(progn

;;; Support for generic function key binding (originally by Ken Laprade):
;;;
(defun define-function-key (keymap key def)
  "\
Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.
KEYMAP is a keymap. KEY is a string naming the key (as known to X) with
optional `M', `S', or `C' prefixes specifying meta, shift and control
qualifiers. Prefixes are separated from the key name by a hyphen.
Raw keys (as from the normal X version) do not provide the shift or
control prefixes, but the emacstool version does.
Both maps are setup by this function.
DEF is the key definition.  It may be any valid definition for define-key."
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
	 (keybase (cond ((string-match "L\\([1-4]\\)$" keyname) 22)
			((string-match "L\\([5-6]\\)$" keyname) 23)
			((string-match "L\\([7-9]\\|10\\)$" keyname) 24)
			((string-match "F\\([1-5]\\)$" keyname) 10)
			((string-match "F\\([6-9]\\|10\\)$" keyname) 11)
			((string-match "F1\\([1-2]\\)$" keyname) 12)
			(t 0)))	; Unknown key.
	 (keynum (if (match-beginning 1)
		     (string-to-int (substring keyname (match-beginning 1)
					       (match-end 1)))
		   0)))
    ;; The raw key version:
    (or shift control (not sun-esc-bracket)
	(define-key keymap (format "\e[%03d~" (+ keybase keynum)) def)
	)))

(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")

(defvar sun-esc-bracket t
  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")

;;; Hook in the function key maps:
(if sun-esc-bracket
    (progn
      (define-key esc-map "[" sun-raw-map)		; Install sun-raw-map
      (define-key esc-map "[-1z" nil)			; Undefined keys
      (define-key esc-map "[A" 'previous-line )		; R8
      (define-key esc-map "[B" 'next-line)		; R14
      (define-key esc-map "[C" 'forward-char)		; R12
      (define-key esc-map "[D" 'backward-char)		; R10
      (define-key esc-map "[[" 'backward-paragraph)	; the original esc-\[
      ))

)) ;;; End test for character based emacs

(defun global-set-function-key (keys function)
  "\
Give KEY a definition of COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a string naming the key (as known to X) with
optional `M', `S', or `C' prefixes specifying meta, shift and control
qualifiers. Prefixes are separated from the key name by a hyphen.
Note that if KEY has a local definition in the current buffer
that local definition will continue to shadow any global definition."
  (interactive "sSet key globally: \nCSet key %s to command: ")
  (define-function-key global-map keys function)
  nil)

(defun local-set-function-key (keys function)
  "\
Give KEY a local definition of COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a string naming the key (as known to X) with
optional `M', `S', or `C' prefixes specifying meta, shift and control
qualifiers. Prefixes are separated from the key name by a hyphen.
The definition goes in the current buffer's local map,
which is shared with other buffers in the same major mode."
  (interactive "sSet key locally: \nCSet key %s locally to command: ")
  (let ((local-map (current-local-map)))
    (or local-map
	(progn
	  (setq local-map (make-sparse-keymap))
	  (use-local-map local-map)))
    (define-function-key local-map keys function)
    nil))

(provide 'function-keys)
