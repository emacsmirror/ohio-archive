;;;
;;; FILE
;;;	rexx-debug.el	V1.0
;;;
;;;	Copyright (C) 1993 by Anders Lindgren.
;;;
;;;	This file is NOT part of GNU Emacs (yet).
;;;
;;; DISTRIBUTION
;;;	REXX-debug is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published 
;;;	by the Free Software Foundation; either version 1, or (at your 
;;;	option) any later version.
;;;
;;;	GNU Emacs is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;
;;;	You should have received a copy of the GNU General Public
;;;	License along with GNU Emacs; see the file COPYING.  If not,
;;;	write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;	MA 02139, USA.
;;;
;;;
;;; AUTHOR
;;;	Anders Lindgren, d91ali@csd.uu.se
;;;
;;; USAGE
;;;	To use this program, call "rexx-debug", enter a filename,
;;;	or press return if you would like to debug the current file.
;;;	Enter the arguments to the rexx program and press return.
;;;	The output from the program and debuginformation will be
;;;	shown in "*rexx-<name>*".
;;;
;;;	Very simple REXX source level debugger. Currently, the only thing
;;;	it reallt does is reads the debug info and places the arrow
;;;	on the correct line.
;;;
;;;	To use this program, the rexx script must be run in interactive
;;;	debug mode. This is controlled by the '?' trace flag. You can
;;;	for example place this line in the beginning of the script:
;;;		trace ?r
;;;
;;; HISTORY
;;;	93-01-11 ALi Start of codeing based on comint-gdb
;;;	93-01-15     Works very well, thank you!
;;;	93-03-16     rxdb-command-name removed as local function.
;;;

(require 'comint)
(provide 'rexx-debug)

(defvar rxdb-lineno-regexp "^ +[0-9]+ +\\*\\-\\* "
  "A regexp to recognize a linenumber in the rexx debugger output stream.")

(defvar rxdb-prompt-pattern "^>\\+> "
  "A regexp to recognize the rexx debugger prompt.")

(defvar rxdb-command-name "rx"
  "Pathname for REXX interpreter.")

(defvar rxdb-mode-map nil
  "Keymap for rexx-debug-mode.")

(if rxdb-mode-map
    nil
  (setq rxdb-mode-map (full-copy-sparse-keymap comint-mode-map))
  (define-key rxdb-mode-map "\C-l" 'rxdb-refresh))

(defun rxdb-mode ()
  "Major mode for interacting with an inferior rexx process.

Commands:

\\{rxdb-mode-map}

Variable:
    rxdb-command-name  contains the name of the REXX interpretator.
		       Default is \"RX\", which is used by ARexx.

Customisation: Entry to this mode runs the hooks comint-mode-hook and
rxdb-mode-hook (in that order).

For example:
(setq rxdb-mode-hook '(lambda ()
		        (setq rxdb-command-name \"/usr/local/bin/rexx\")
			(define-local-key \"key\" 'favorite-command)
			))

will set the command so it can be used in many UNIX environments."
  (interactive)
  (comint-mode)
  (setq major-mode 'rxdb-mode)
  (setq mode-name "Inferior REXX")
  (setq mode-line-process '(": %s"))
  (use-local-map rxdb-mode-map)
  (setq comint-prompt-regexp rxdb-prompt-pattern)
  (make-local-variable 'rxdb-last-frame)
  (setq rxdb-last-frame nil)
  (make-local-variable 'rxdb-last-frame-displayed-p)
  (setq rxdb-last-frame-displayed-p t)
  (make-local-variable 'rxdb-delete-prompt-marker)
  (setq rxdb-delete-prompt-marker nil)
  (run-hooks 'rxdb-mode-hook))

(defun rexx-debug (path args)
  "Run a rexx program FILE in buffer *rexx-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive "FDebug file (return for current buffer): \nsArguments:")
  (setq path (expand-file-name path))
  (let* ((file (file-name-nondirectory path))
	 (rxdb-buffer (concat "*rexx-" file "*"))
	 (rxdb-window (get-buffer-window rxdb-buffer)))
    (if rxdb-window 
	(select-window rxdb-window)
	(switch-to-buffer rxdb-buffer))
    (if (comint-check-proc rxdb-buffer) nil
      (setq default-directory (file-name-directory path))
      (or (bolp) (newline))
      (insert "Current directory is " default-directory "\n")
      (make-comint (concat "rexx-" file) rxdb-command-name nil
		   (concat file " " args))
      (rxdb-mode)
      (set-process-filter (get-buffer-process (current-buffer)) 'rxdb-filter)
      (set-process-sentinel (get-buffer-process (current-buffer)) 
			    'rxdb-sentinel))
    (rxdb-set-buffer path)))

(defun rxdb-set-buffer (&optional path)
  (cond ((eq major-mode 'rxdb-mode)
	 (setq current-rxdb-buffer (current-buffer))
	 (if path
	     (setq current-rxdb-file path)))))

;; This function is responsible for inserting output from the rexx 
;; debugger into the buffer. It records the linenumber for the
;; placement of the arrow.
(defun rxdb-filter (proc string)
  (let ((inhibit-quit t))
    (rxdb-filter-accumulate-marker proc string)))

(defun rxdb-filter-accumulate-marker (proc string)
  (let ((end t))
    (while end
      (setq end (string-match "\012" string))
      (if end
	  (progn
	    (if (string-match rxdb-lineno-regexp string)
		(progn
		  (setq rxdb-last-frame 
			(string-to-int (substring string 0 
						  (string-match 
						   "\\*\\-\\*" string 1))))
		  (setq rxdb-last-frame-displayed-p nil)))
	    (rxdb-filter-insert proc (substring string 0 (1+ end)))
	    (setq string (substring string (1+ end))))))
    (if (equal string "") nil
      (rxdb-filter-insert proc string))))

(defun rxdb-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (rxdb-maybe-delete-prompt)
	  ;; Check for new linenumber.
	  (rxdb-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new linenumber appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun rxdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the rxdb buffer.
	     (set-buffer obuf))))))


(defun rxdb-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (rxdb-display-frame))

(defun rxdb-display-frame (&optional nodisplay noauto)
  "Display the last line executed in another window."
  (interactive)
  (rxdb-set-buffer)
  (and rxdb-last-frame (not nodisplay)
       (or (not rxdb-last-frame-displayed-p) (not noauto))
       (progn (rxdb-display-line current-rxdb-file rxdb-last-frame)
	      (setq rxdb-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun rxdb-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))


(defun rxdb-call (command)
  "Invoke rexx debug COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq rxdb-delete-prompt-marker (point-marker))
  (rxdb-set-buffer)
  (send-string (get-buffer-process current-rxdb-buffer)
	       (concat command "\n")))

(defun rxdb-maybe-delete-prompt ()
  (if (and rxdb-delete-prompt-marker
	   (> (point-max) (marker-position rxdb-delete-prompt-marker)))
      (let (start)
	(goto-char rxdb-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq rxdb-delete-prompt-marker nil))))


(defvar rxdb-commands nil
  "List of strings or functions used by send-rxdb-command.
It is for customization by you.")

(defun send-rxdb-command (arg)
  "This command reads the core-address where the cursor is positioned.  It
 then inserts this ADDR at the end of the rxdb buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list rxdb-commands.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of rxdb-commands.  "
  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg rxdb-commands)))
    (setq addr (rxdb-read-address))
    (if (eq (current-buffer) current-rxdb-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-rxdb-buffer)
    (goto-char (dot-max))
    (insert-string comm)))
