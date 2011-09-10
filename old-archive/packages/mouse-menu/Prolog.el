;; Major mode for editing Prolog, and for running Prolog under Emacs
;; Copyright (C) 1986 Free Software Foundation, Inc.
;; Author Masanobu UMEDA (umerin@flab.fujitsu.junet)

;; Modified, Fri Mar 4 16:29:47 1988, by Russell Ritchie, <russell@uk.ac.strath.hci>
;; to use the ``write-temp-file and load'' method now employed by lisp-mode
;; and to allow differing Prolog programs to be run.

;; Modified, Thu Apr 21 14:15:06 1988, by Russell Ritchie, <russell@uk.ac.strath.hci>,
;; to allow multiple, differing Prolog processes (for using Quintus saved states).
;; Set inferior-prolog-program to the name of the saved state you want to run.
;; Do this either with a Local variables comment in Prolog source file, or in your
;; .emacs initialisation file. If you set it to nil in your .emacs file, you will be
;; prompted for the name of the prolog program you want to run every time you visit
;; a[nother] Prolog file.

;; Added inferior-prolog-buffer, Wed Nov 9 11:57:33 1988, <russell@uk.ac.strath.hci>.
;; Added (provide 'Prolog), Wed Nov 9 11:58:44 1988, <russell@uk.ac.strath.hci>.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(require 'shell)
(provide 'Prolog)

(defvar prolog-mode-syntax-table nil)
(defvar prolog-mode-abbrev-table nil)
(defvar prolog-mode-map nil)

(defvar inferior-prolog-program "prolog"
  "*Program that is run by 'run-prolog' in the Prolog buffer.
If this is set to nil a program to be executed will be prompted for.")

(defvar inferior-prolog-buffer nil
  "The buffer the inferior Prolog process is running in.
Do NOT set this variable, it is bound automatically after determination of which program to run.")

(defvar prolog-consult-string "reconsult('%s').\n"
  "*(Re)Consult mode (for C-Prolog and Quintus Prolog). ")

(defvar prolog-compile-string "compile('%s').\n"
  "*Compile mode (for Quintus Prolog).")

(defvar prolog-eof-string "end_of_file.\n"
  "*String that represents end of file for prolog.
nil means send actual operating system end of file.")

(defvar prolog-indent-width 4)

(if prolog-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq prolog-mode-syntax-table table)))

(define-abbrev-table 'prolog-mode-abbrev-table ())

(defun prolog-mode-variables ()
  (set-syntax-table prolog-mode-syntax-table)
  (setq local-abbrev-table prolog-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^%%\\|^$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'prolog-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'prolog-comment-indent))

(defun prolog-mode-commands (map)
  (define-key map "\t" 'prolog-indent-line)
  (define-key map "\C-c\C-p" 'prolog-consult-predicate-and-go)
  (define-key map "\C-cp"    'prolog-consult-predicate)
  (define-key map "\C-c\C-r" 'prolog-consult-region-and-go)
  (define-key map "\C-cr"    'prolog-consult-region)
  (define-key map "\C-c\C-b" 'prolog-consult-buffer-and-go)
  (define-key map "\C-cb"    'prolog-consult-buffer))

(if prolog-mode-map
    nil
  (setq prolog-mode-map (make-sparse-keymap))
  (prolog-mode-commands prolog-mode-map))

(defun prolog-mode ()
  "Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of prolog-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map prolog-mode-map)
  (setq major-mode 'prolog-mode)
  (setq mode-name "Prolog")
  (prolog-mode-variables)
  (run-hooks 'prolog-mode-hook))

(defun c-prolog-mode ()
  "Major mode for editing C-Prolog code.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of prolog-mode-hook then c-prolog-mode-hook."
  (interactive)
  (prolog-mode)
  (setq mode-name "C-Prolog")
  (make-local-variable 'inferior-prolog-program)
  (setq inferior-prolog-program "c-prolog1.5")
  (make-local-variable 'inferior-prolog-buffer)	; Set by #'run-prolog when the buffer is created.
  (run-hooks 'c-prolog-mode-hook))

(defun quintus-prolog-mode ()
  "Major mode for editing Quintus Prolog code.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of prolog-mode-hook then quintus-prolog-mode-hook."
  (interactive)
  (prolog-mode)
  (setq mode-name "Quintus Prolog")
  (make-local-variable 'inferior-prolog-program)
  (setq inferior-prolog-program "qprolog2.0")
  (make-local-variable 'inferior-prolog-process-name)
  (setq inferior-prolog-process-name inferior-prolog-program)
  (make-local-variable 'inferior-prolog-buffer)	; Set by #'run-prolog when the buffer is created.
  (run-hooks 'quintus-prolog-mode-hook))

(defun prolog-indent-line (&optional whole-exp)
  "Indent current line as Prolog code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (prolog-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ))

(defun prolog-indent-level ()
  "Compute prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%%%") 0)		;Large comment starts
     ((looking-at "%[^%]") comment-column) ;Small comment starts
     ((bobp) 0)				;Beginning of buffer
     (t
      (let ((empty t) ind more less)
	(if (looking-at ")")
	    (setq less t)		;Find close
	  (setq less nil))
	;; See previous indentation
	(while empty
	  (forward-line -1)
	  (beginning-of-line)
	  (if (bobp) (setq empty nil))
	  (skip-chars-forward " \t")
	  (if (not (or (looking-at "%[^%]") (looking-at "\n")))
	      (setq empty nil)))
	(setq ind (current-column))	;Beginning of clause
	;; See its beginning
	(if (looking-at "%%[^%]")
	    ind
	  ;; Real prolog code
	  (if (looking-at "(")
	      (setq more t)		;Find open
	    (setq more nil))
	  ;; See its tail
	  (end-of-prolog-clause)
	  (or (bobp) (forward-char -1))
	  (cond ((looking-at "[,(;>]")
		 (if (and more (looking-at "[^,]"))
		     (+ ind prolog-indent-width) ;More indentation
		   (max tab-width ind))) ;Same indentation
		((looking-at "-") tab-width) ;TAB
		((or less (looking-at "[^.]"))
		 (max (- ind prolog-indent-width) 0)) ;Less indentation
		(t 0))			;No indentation
	  )))
     )))

(defun end-of-prolog-clause ()
  "Go to end of clause in this line."
  (beginning-of-line 1)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

(defun prolog-comment-indent ()
  "Compute prolog comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (prolog-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))
	))


;;;
;;; Inferior prolog mode
;;;
(defvar inferior-prolog-mode-map nil)

(defun inferior-prolog-mode ()
  "Major mode for interacting with an inferior Prolog process.

Runs the value of inferior-prolog-program in the Prolog buffer.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of prolog-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
prolog-mode-hook is called after shell-mode-hook.

You can send text to the inferior Prolog from other buffers
using the commands send-region, send-string, \\[prolog-consult-predicate], 
\\[prolog-consult-region and \\[prolog-consult-buffer].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'. '%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[shell-send-eof] sends end-of-file as input.
\\[kill-shell-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[interrupt-shell-subjob] interrupts the shell or its current subjob if any.
\\[stop-shell-subjob] stops, likewise. \\[quit-shell-subjob] sends quit signal, likewise."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-prolog-mode)
  (setq mode-name "Inferior Prolog")
  (setq mode-line-process '(": %s"))
  (prolog-mode-variables)
  (require 'shell)
  (if inferior-prolog-mode-map
      nil
    (setq inferior-prolog-mode-map (copy-alist shell-mode-map))
    (prolog-mode-commands inferior-prolog-mode-map))
  (use-local-map inferior-prolog-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-variable-buffer-local 'shell-prompt-pattern)
  (setq shell-prompt-pattern "^| [ ?][- ] *") ;Set prolog prompt pattern
  (run-hooks 'shell-mode-hook 'prolog-mode-hook))

(defun run-prolog ()
  "Run an inferior Prolog process, use 
inferior-prolog-progam for the name of the Prolog buffer."
  (interactive)
  (let ((inferior-prolog-program
	 (or inferior-prolog-program (read-existing-program-name))))
    (setq inferior-prolog-buffer
	  (make-shell (file-name-nondirectory inferior-prolog-program)
		      (if (string-match "~" inferior-prolog-program)
			  ;; User has specified a ~-relative program, so...
			  (expand-file-name inferior-prolog-program)
			inferior-prolog-program)))
    (switch-to-buffer inferior-prolog-buffer)
    (inferior-prolog-mode)))

(defun prolog-consult-region (compile beg end)
  "Send the region to the Prolog process made by M-x run-prolog.
 If COMPILE (prefix arg) is not nil,
 use compile mode rather than consult mode.

Bound to \\<prolog-mode-map>\\[prolog-consult-region]"
  (interactive "P\nr")
  (message "Sending region...")
  (save-excursion
    (let* ((inferior-prolog-program (file-name-nondirectory inferior-prolog-program))
	   (filename
	    (format
	     "/tmp/emPROLOG%d" (process-id (get-process inferior-prolog-program)))))
      (write-region beg end filename nil 'nomessage)
      (process-send-string
	inferior-prolog-program
	(format (if compile prolog-compile-string prolog-consult-string) filename))))
  (message "Sent region!"))

(defun prolog-consult-region-and-go (compile beg end)
  "Send the region to the inferior Prolog, and switch to Prolog buffer.
 If COMPILE (prefix arg) is not nil,
 use compile mode rather than consult mode.

Bound to \\<prolog-mode-map>\\[prolog-consult-region]"
  (interactive "P\nr")
  (prolog-consult-region compile beg end)
  (switch-to-buffer-other-window inferior-prolog-buffer)
  (end-of-buffer))

(defun start-of-prolog-predicate (predicate)
  "Attempt to find the start of prolog PREDICATE.
Simple minded: searches from start of buffer for PREDICATE at beginning of line."
  (interactive "sWhich predicate?: ")
  (goto-char (point-min))
  (re-search-forward (concat "^" predicate))
  (beginning-of-line))

(defun end-of-prolog-predicate (predicate)
  "Attempt to find the end of prolog PREDICATE.
Simple minded: searches from end of buffer for last PREDICATE clause at beginning of line, 
then searches for \".\" at end of line."
  (interactive "sWhich predicate?: ")
  (goto-char (point-max))
  (re-search-backward (concat "^" predicate))
  (re-search-forward "\\.$"))

(defun prolog-consult-predicate (compile predicate)
  "Send the prompted-for predicate to the PROLOG process made by M-x run-prolog.
If interactive arg COMPILE is non-nil, then try to compile predicate using
the value of prolog-compile-string, rather than 'reconsult'ing.

Bound to \\<prolog-mode-map>\\[prolog-consult-predicate]"
  (interactive "P\nsWhich predicate?: ")
  (save-excursion
    (end-of-prolog-predicate predicate)
    (let ((end (point)))
      (start-of-prolog-predicate predicate)
      (prolog-consult-region compile (point) end))))

(defun prolog-consult-predicate-and-go (compile predicate)
  "Send the prompted-for predicate to Prolog, and switch to Prolog buffer.
If interactive arg COMPILE is non-nil, then try to compile predicate using
the value of prolog-compile-string, rather than 'reconsult'ing.

Bound to \\<prolog-mode-map>\\[prolog-consult-predicate-and-go]"
  (interactive "P\nsWhich predicate?: ")
  (prolog-consult-predicate compile predicate)
  (switch-to-buffer-other-window inferior-prolog-buffer)
  (end-of-buffer))

(defun prolog-consult-buffer (compile)
  "Send the entire current buffer to the Prolog process.
 If prefix arg COMPILE is non-nil then use
 compile mode rather than consult mode.

Bound to \\<prolog-mode-map>\\[prolog-consult-buffer]."
  (interactive "P")
  (message (concat "Sending contents of " (buffer-name) "..."))
  (save-excursion
    (mark-whole-buffer)
    (prolog-consult-region compile (point) (mark)))
  (message (concat "Sent " (buffer-name) "!")))

(defun prolog-consult-buffer-and-go (compile)
  "Send the entire current buffer to the Prolog buffer and go to it.
 If prefix arg COMPILE is non-nil then use
 compile mode rather than consult mode.

Bound to \\<prolog-mode-map>\\[prolog-consult-buffer-and-go]."
  (interactive "P")
  (prolog-consult-buffer compile)
  (switch-to-buffer-other-window inferior-prolog-buffer)
  (end-of-buffer))

(defun prolog-compile-region (start end)
  "Compile PROLOG buffer from START of region to END."
  (interactive "r")
  (prolog-consult-region t start end))

(defun prolog-compile-buffer ()
  "Compile all of current PROLOG buffer."
  (interactive)
  (prolog-consult-buffer t))

(defun start-prolog ()
  "Called when a file ending in \".pl\" is visited. Starts a prolog process 
 (using run-prolog) in another window and displays that and the file, leaving 
the cursor at the top of the file buffer."
  (interactive)
  (let ((this (current-buffer)))
    (prolog-mode)			; Set prolog-mode in the ".pl" file.
    (run-prolog)			; Start up a prolog process.
    (switch-to-buffer this)))
