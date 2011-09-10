;; LCD Archive Entry:
;; pk-dc|Pete Krautscheid|krauts@icd.teradyne.com|
;; Mode to interface emacs with dc, the desk calculator.|
;; 19-Sep-1995|0.07|~/packages/pk-dc.el.Z|
;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  pk-dc.el - a dc interface for emacs.
;;
;;  Revision: 0.07
;;
;;  Pete Krautscheid
;;
;;  Please send bug reports to krauts@icd.teradyne.com
;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; M-x dc  to invoke
;;
;;
;;{{{ Installation

;; Simply add a line like
;;
;;     (load-file "~/lisp/pk-dc.el")
;;
;; to your .emacs file.  Then simply use M-x dc to invoke.

;;}}}
;;{{{ Basic info

;; pk-dc is an interface to the unix dc command, which implements an
;; arbitrary precision arithmetic package.  As such, it is used in the
;; same manner as dc, with the same commands.  The dc man page should
;; provide a good basis for using this package.
;;
;; Hopefully this package provides some value added features to
;; vanilla dc.  Some features include:
;;
;;     Constantly display the stack contents
;;     Displays a command summary if needed
;;     Use the keypad without the Num Lock key (at least on Sun and Linux)
;;     Ummm... another reason to not leave emacs and use "something else".
;;
;;
;; All key commands which affect the stack assume that the immediatly
;; entered number is at the top of the stack.  Key commands take
;; effect immediatly - enter is not needed.

;;}}}
;;{{{ Customizing

;; To set the default number of decimal places that dc uses when it is
;; brought up, use
;;
;; (setq dc-default-scale-factor 4)
;;
;; To set the default keypad mapping (which seems to be very different
;; on different machines)
;;
;; (setq dc-default-keypad-map 'sun)
;;
;; Currently supported are 'sun and 'linux.  The default is 'sun If
;; neither of these work for you, let me know.  Particularly if you
;; can send me a keymap!
;;
;; To make dc come up in a window other than the current one by
;; default, use
;;
;; (setq dc-other-window t)
;;
;; To make emacs use a pipe rather than a pty to talk with the dc
;; process, use
;;
;; (setq dc-use-pty-connect nil)
;;
;; To make dc not show the key bindings when brought up, use
;;
;; (setq dc-display-keys nil)
;;
;; If for some reason you need a hook, dc-mode-hook is called upon
;; entering dc-mode.

;;}}}
;;{{{ Note on pipes and pty's

;; Note: Pipes will not work with the current version of GNU dc
;; because of output buffering; when sent to a pipe, GNU dc uses block
;; buffering rather than line buffering.  This means that Emacs can't
;; read output from dc that is stuck in the buffer.  The default
;; connection has been set to pty's to prevent this problem.  Use the
;; variable dc-use-pty-connect to change this.

;;}}}
;;{{{ Revision History

;; Revision History:
;;
;;
;; v0.07  09-19-95  PWK   Cleaned up documentation for release.
;;
;; v0.06  09-14-95  PWK   Added default setting for scaling
;;                        Added support for multiple keypad maps
;;
;; v0.05  08-31-95  PWK   Added support for GNU dc
;;
;; v0.03  08-30-95  PWK   Added register support
;;                        Added setting of scale factor
;;                        Display key bindings (if var set)
;;
;; v0.02  08-29-95  PWK   Added some bindings, fixed minor bugs
;;
;; v0.01  08-28-95  PWK   Created

;;}}}
;;{{{ Wish list

;; Wish list:
;;
;;    Display of registers - dc won't tell me what's in them!
;;    More features (swap top elements, delete top element, etc.)
;;    Use filter instead of buffer for output from dc

;;}}}
;;{{{ Bugs

;; Bugs:
;;
;;    dc errors are not reported
;;    Multiple invocations do not bring up the existing dc buffer
;;    Multiple . creates wierd hanging problem - seems to be dc bug
;;    If user changes key bindings display is not updated

;;}}}
;;{{{ Copyright

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;    Copyright (C) 1995  Pete Krautschied
;;
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License as
;;    published by the Free Software Foundation; either version 2 of
;;    the License, or (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public
;;    License along with GNU emacs; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;}}}
;;{{{ Note on {{{

;; The triple braces you see are used by `folding-mode', a minor mode
;; by Jamie Lokier, available from the elisp archive.

;;}}}

;;{{{ Variable definitions

(defvar dc-default-scale-factor 2
  "Default variable to determine how many decimal places of accuracy
to use when giving the results of calculations.  The default accuracy
is 2.")

(defvar dc-default-keypad-map nil
  "Default mapping to use for the keypad.  These allow the keypad to
be used without worrying about the Num Lock key.  Currently supported
are 'sun, 'linux, and 'none.  If this variable is not set, the default
is currently 'sun.")

(defvar dc-other-window nil
  "If non nil, start dc in a window other than the current one.
Otherwise use the current window.")

(defvar dc-use-pty-connect t
  "If t, the connection with dc will be made using a pty.  If nil, the
connection will use a pipe. The default is to use a pty, since GNU dc
does not seem to support line buffered output to a pipe and other
versions may have the same problem.")

(defvar dc-display-keys t
  "If non nil, show which keys do what.")

(defvar dc-wait-loop 1000
  "Initial loop counter for hacked delay")

(defvar dc-mode-hook nil
  "If non nil, this hook is called when entering dc mode.")

;;}}}

;;{{{ dc and dc-mode function definitions

(defun dc ()
  (interactive)
  ;; Start the dc process
  (let ((process-connection-type dc-use-pty-connect))
	(start-process "dc-process" "*dc-buffer*" "dc"))
  ;; Set up the display buffer
  (let ((dc-buffer (generate-new-buffer "desktop-calculator")))
	(set-buffer dc-buffer)
	(dc-mode)
	(dc-set-scale dc-default-scale-factor)
	(dc-update)
	(if dc-other-window
		;; Use another window
		(progn
		  (if (one-window-p)
			  ;; One window - split and use top one.
			  (progn
				(split-window)
				(select-window (next-window (frame-first-window)))))
		  (pop-to-buffer dc-buffer))
	  (switch-to-buffer dc-buffer))))


(defun dc-mode ()
  "RPN calculator mode using dc"
  (interactive)
  (setq major-mode 'dc-mode)
  (setq mode-name "Desktop Calculator")
  (use-local-map dc-mode-map)
  ;; Specific bindings for machine type
  (dc-set-keypad-keymap dc-default-keypad-map)
  (run-hooks 'dc-mode-hook))

;;}}}

;;{{{ Basic keymap definition

(defvar dc-mode-map nil
  "Local keymap for dc mode.")

(if dc-mode-map nil
  (progn
	(setq dc-mode-map (make-sparse-keymap))

	;; Basic mathematical operations, entering, and numbers

	(define-key dc-mode-map "+" '(lambda () (interactive) (dc-command "+")))
	(define-key dc-mode-map "-" '(lambda () (interactive) (dc-command "-")))
	(define-key dc-mode-map "*" '(lambda () (interactive) (dc-command "*")))
	(define-key dc-mode-map "/" '(lambda () (interactive) (dc-command "/")))
	(define-key dc-mode-map "%" '(lambda () (interactive) (dc-command "%")))
	(define-key dc-mode-map "^" '(lambda () (interactive) (dc-command "^")))
	(define-key dc-mode-map "v" '(lambda () (interactive) (dc-command "v")))

	(define-key dc-mode-map "=" 'dc-enter)
	(define-key dc-mode-map [kp-enter] 'dc-enter)
	(define-key dc-mode-map [return] 'dc-enter)
	(define-key dc-mode-map " " 'dc-enter)

	;; Duplicate top item in stack
	(define-key dc-mode-map "d" '(lambda () (interactive) (dc-command "d")))

	;; Register operations
	(define-key dc-mode-map "s" '(lambda () (interactive) (dc-register-command "s")))
	(define-key dc-mode-map "S" '(lambda () (interactive) (dc-register-command "S")))
	(define-key dc-mode-map "l" '(lambda () (interactive) (dc-register-command "l")))
	(define-key dc-mode-map "L" '(lambda () (interactive) (dc-register-command "L")))

	;; Clear
	(define-key dc-mode-map "c" 'dc-clear)

	;; Set scale factor
	(define-key dc-mode-map "k" 'dc-set-scale)

	;; Toggle key display
	(define-key dc-mode-map "t" 'dc-toggle-keys)

	;; Quit
	(define-key dc-mode-map "q" 'dc-quit)))

;;}}}

;;{{{ Code to set various keymaps for the keypad

(defun dc-set-keypad-keymap (system-type)
  (cond ((equal system-type 'sun)
		 ;; Sun
		 (define-key dc-mode-map [f24] '(lambda () (interactive) (dc-command "-")))
		 (define-key dc-mode-map [f25] '(lambda () (interactive) (dc-command "/")))
		 (define-key dc-mode-map [f26] '(lambda () (interactive) (dc-command "*")))
		 (define-key dc-mode-map [delete] (lambda () (interactive) (insert ".")))
		 (define-key dc-mode-map [f33] (lambda () (interactive) (insert "1")))
		 (define-key dc-mode-map [down] (lambda () (interactive) (insert "2")))
		 (define-key dc-mode-map [f35] (lambda () (interactive) (insert "3")))
		 (define-key dc-mode-map [left] (lambda () (interactive) (insert "4")))
		 (define-key dc-mode-map [f31] (lambda () (interactive) (insert "5")))
		 (define-key dc-mode-map [right] (lambda () (interactive) (insert "6")))
		 (define-key dc-mode-map [f27] (lambda () (interactive) (insert "7")))
		 (define-key dc-mode-map [up] (lambda () (interactive) (insert "8")))
		 (define-key dc-mode-map [f29] (lambda () (interactive) (insert "9")))
		 (define-key dc-mode-map [insert] (lambda () (interactive) (insert "0")))
		 ;; Bind pause to "clear"
		 (define-key dc-mode-map [f21] 'dc-clear))

		((equal system-type 'linux)
		 ;; Linux
		 (define-key dc-mode-map "\C-m" 'dc-enter)
		 (define-key dc-mode-map [kp-1] (lambda () (interactive) (insert "1")))
		 (define-key dc-mode-map [kp-2] (lambda () (interactive) (insert "2")))
		 (define-key dc-mode-map [next] (lambda () (interactive) (insert "3")))
		 (define-key dc-mode-map [kp-4] (lambda () (interactive) (insert "4")))
		 (define-key dc-mode-map "\M-[G" (lambda () (interactive) (insert "5")))
		 (define-key dc-mode-map [kp-6] (lambda () (interactive) (insert "6")))
		 (define-key dc-mode-map [kp-7] (lambda () (interactive) (insert "7")))
		 (define-key dc-mode-map [kp-8] (lambda () (interactive) (insert "8")))
		 (define-key dc-mode-map [prior] (lambda () (interactive) (insert "9")))
		 (define-key dc-mode-map [insertchar] (lambda () (interactive) (insert "0"))))
		((equal system-type 'none)
		 ;; None - don't bind any...
		 nil)
		(t
		 ;; Don't know... use Sun as a basic default until more info available...
		 (dc-set-keypad-keymap 'sun))))

;;}}}

;;{{{ Code for... well... doing stuff

(defun dc-clear ()
  (interactive)
  ;; Delete the input line
  (save-excursion
	(goto-char (point-min))
	(delete-region (point) (save-excursion (end-of-line) (point))))
  ;; Update dc
  (dc-send-string "c")
  (dc-update)
  (message "Cleared"))

(defun dc-enter ()
  (interactive)
  ;; Send whatever is on the input line to the dc process
  (goto-char (point-min))
  (search-forward-regexp "^.*$" nil t)
  (dc-send-string (buffer-substring (match-beginning 0) (match-end 0)))
  (replace-match "")
  (dc-update))

(defun dc-command (symbol)
  (interactive)
  (dc-enter)
  (dc-send-string symbol)
  ;; (message symbol)  ; This gacks on printing %
  (dc-update))

(defun dc-get-register (key)
  (interactive "cRegister: ")
  (char-to-string key))

(defun dc-register-command (command)
  (interactive)
  (dc-command (concat command (call-interactively 'dc-get-register))))

(defun dc-set-scale (scale-string)
  (interactive "sNumber of decimal places: ")
  (dc-send-string (concat scale-string "k")))

(defun dc-quit ()
  (interactive)
  (if (y-or-n-p "Exit dc mode? ")
	  (progn
		;; Kill process (politely) - send q to dc
		(dc-send-string "q")
		(accept-process-output (get-process "dc-process") 1)
		(kill-buffer "desktop-calculator")
		;; Not sure if this should be done - most people seem to leave it around.  Ah well!
		(kill-buffer "*dc-buffer*")
		(message ""))))

;;}}}

;;{{{ Code for updating the dc buffer

(defun dc-update ()
  ;; Store what is on the first line
  (goto-char (point-min))
  (let ((first (buffer-substring (point) (progn (end-of-line) (point)))))
	(delete-region (point-min) (point-max))
	(insert first)
	(insert "\n\n===================================================================")
	(insert "\n\nStack:")
	(insert "\n\n")
	(insert (dc-get-stack))
	(if dc-display-keys
		(dc-print-keys)))
  (goto-char (point-min))
  (end-of-line))


(defun dc-print-keys ()
  ;; Print the keys
  (insert "\n\n===================================================================")
  (insert "\n
Math               Registers                 Misc
----               ---------                 ----
% - mod            s - move to register      c - clear stack
^ - exponentiate   S - push onto register    d - duplicate
v - sqrt           l - copy from register    t - toggle key display
k - set scaling    L - pop from register     q - quit"))


(defun dc-toggle-keys ()
  (interactive)
  (if dc-display-keys
	  (setq dc-display-keys nil)
	(setq dc-display-keys t))
  (dc-update))

;;}}}

;;{{{ Code for communicating with the dc process

(defun dc-send-string (string)
  ;; Need \n for GNU dc
  (process-send-string "dc-process" (concat string "\n"))
  ;; Flush all remaining output
  (accept-process-output "dc-process" 0 1))


;; Return string composed of the current stack
(defun dc-get-stack ()
  (let ((process (get-process "dc-process")))

	(save-excursion

	  ;; Clear the contents of the dc buffer
	  (set-buffer "*dc-buffer*")
	  (erase-buffer)

	  ;; Print the stack
	  (dc-send-string "f")

	  ;; If we go too fast here dc doesn't always respond completely.  Do
	  ;; something silly to waste time.  sleep-for does not let me wait less
	  ;; than a second under SunOS...

	  (let ((n dc-wait-loop)) (while (> n 1) (setq n (- n 1))))

	  ;; Wait for something to be in the buffer.  Timeout in one microsecond.
	  (accept-process-output process 0 1)

	  ;; The dc buffer now contains the contents of the stack
	  (if (equal (point-min) (point-max))
		  "empty stack\n"  ; No contents - probably GNU dc - return empty stack
		(buffer-substring (point-min) (point-max))))))

;;}}}

;;{{{ Emacs local variables for folding mode

;; Local variables:
;; folded-file: t
;; end:

;;}}}
