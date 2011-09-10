;Return-Path: <info-gnu-emacs-request@prep.ai.mit.edu>
;Date: 28 Apr 89 20:43:48 GMT
;From: uhccux!lupton@humu.nosc.mil  (Robert Lupton)
;Organization: University of Hawaii
;Subject: dbx.el
;Sender: info-gnu-emacs-request@prep.ai.mit.edu
;To: info-gnu-emacs@prep.ai.mit.edu
;
;Here is my occasional posting on dbx-mode. I have added
;
;  Support for multiple directories (the use command)
;  Understands `where', `up', and `down'
;  Understands segmentation violations etc. (thank you, whoever suggested
;this, I'm afraid that I lost your name)
;  Added dbx-step, dbx-next, and dbx-cont to step through the source without
;a *dbx-proc* window on the screen (and without scribbling on one)
;  An alias `dbx' for `run-dbx' (trivial, but annoying!)
;
;It's shorter to repost than to post diffs, so here it is:
;
;			Robert Lupton
;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- Cut Here -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Run dbx under Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Main author Masanobu UMEDA (umerin@flab.fujitsu.junet)

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
;
;   Added dbx-use to follow `use' commands.
;      RHL March 1989
;   If tracing is enabled, it now understands up/down/where and
; bus errors, segmentation violations, etc.
;   Added dbx-command which can be bound to keys, to allow stepping
; through source without having a *dbx-proc* window
;      RHL April 1989

(require 'shell)

(defvar dbx-trace-flag nil
  "Dbx trace switch.")

(defvar dbx-process nil
  "Dbx process name.")

(defvar dbx-dir-list nil
  "List of directories searched by dbx-where, set by dbx-use")

(defvar dbx-break-point
  "\\( in .* at\\|),\\) line \\([0-9]*\\) in \\(file \\)?\"\\([^\"]*\\)\""
  "Regexp of pattern that dbx writes at break points, etc.")

(defvar inferior-dbx-mode-map nil)
(if inferior-dbx-mode-map
    nil
  (setq inferior-dbx-mode-map (copy-keymap shell-mode-map))
  (define-key inferior-dbx-mode-map "\C-cu" 'dbx-use)
  (define-key inferior-dbx-mode-map "\C-cw" 'dbx-where)
  (define-key inferior-dbx-mode-map "\C-c\C-t" 'dbx-trace-mode)
  (define-key ctl-x-map "\C-@" 'dbx-stop-at))

(defun inferior-dbx-mode ()
  "Major mode for interacting with an inferior Dbx process.
(Note that dbx is an alias for run-dbx)

The following commands are available:
\\{inferior-dbx-mode-map}

Entry to this mode calls the value of dbx-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
dbx-mode-hook is called after shell-mode-hook.

You can display the debugging program in other window and point out
where you are looking at using the command \\[dbx-where].

\\[dbx-trace-mode] toggles dbx-trace mode. In dbx-trace mode,
debugging program is automatically traced using output from dbx.
The dbx commands `down', `up' and `where' are also traced. Using
the command dbx-command (e.g. M-x dbx-command \"step\") allows you to
step through the source without using the *dbx-proc* buffer. The
commands dbx-cont, dbx-next, and dbx-step use this.

The command \\[dbx-stop-at] sets break point at current line of the
program in the buffer. Major mode name of the buffer must be in
dbx-language-mode-list.

You should use the command `dbx-use' (in the mini-buffer, or \\[dbx-use])
rather than `use' so dbx-mode can find your source directories. (Using
use, then dbx-use with no directories specified also works).

Commands:

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[shell-send-eof] sends end-of-file as input.
\\[kill-shell-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[interrupt-shell-subjob] interrupts the shell or its current subjob if any.
\\[stop-shell-subjob] stops, likewise. \\[quit-shell-subjob] sends quit signal, likewise.
\\[dbx-use] tells dbx mode about any use commands you have issued
\\[dbx-where] displays debugging program in other window and
 points out where you are looking at.
\\[dbx-trace-mode] toggles dbx-trace mode.
\\[dbx-stop-at] sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-dbx-mode)
  (setq mode-name "Inferior Dbx")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-dbx-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'dbx-trace-flag)
  (setq dbx-trace-flag nil)
  (make-variable-buffer-local 'shell-prompt-pattern)
  (setq shell-prompt-pattern "^[^)]*dbx) *") ;Set dbx prompt pattern
  (or (assq 'dbx-trace-flag minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(dbx-trace-flag " Trace") minor-mode-alist)))
  (run-hooks 'shell-mode-hook 'dbx-mode-hook))

(defun dbx (path) "An alias for run-dbx"
  (interactive "fProgram to debug: ")
  (run-dbx path))

(defun run-dbx (path)
  "Run inferior Dbx process on PROGRAM, with I/O via buffer *dbx-PROGRAM*."
  (interactive "fProgram to debug: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*dbx-" file "*"))
    (setq default-directory (file-name-directory path))
    (switch-to-buffer (make-shell (concat "dbx-" file) "dbx" nil file)))
  (setq dbx-process (get-buffer-process (current-buffer)))
  (setq dbx-dir-list (list "."))
  (set-process-filter dbx-process 'dbx-filter)
  (set-process-sentinel dbx-process 'dbx-sentinel)
  (inferior-dbx-mode))

(defun dbx-trace-mode (arg)
  "Toggle dbx-trace mode.
With arg, turn dbx-trace mode on iff arg is positive.
In dbx-trace mode, user program is automatically traced."
  (interactive "P")
  (if (not (eql major-mode 'inferior-dbx-mode))
      (error "Dbx-trace mode is effective in inferior-dbx mode only."))
  (setq dbx-trace-flag
	(if (null arg)
	    (not dbx-trace-flag)
	  (> (prefix-numeric-value arg) 0)))
  (if (not dbx-trace-flag)
      (set-marker overlay-arrow-position nil))
  ;; Force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun dbx-filter (process string)
  "Trace debugging program automatically if dbx-trace-flag is not nil.
Checks two flags: dbx-no-output suppresses output to *dbx-proc* if it exists
and is non-nil, and dbx-wait is set to nil (if it exists) when the dbx prompt
is received."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string)
    (if (process-mark process)
	(set-marker (process-mark process) (point-max)))
    (goto-char (point-max))
    (if (string-match "(dbx)" string)	; the next prompt
	(progn
	  (if (boundp 'dbx-wait) (setq dbx-wait nil))
	  (if dbx-trace-flag
	      (dbx-where
	       (let ((end (point))
		     (beg (progn
			    (re-search-backward "^(dbx)" 0 t 2) (match-end 0)))
		     (str))
		 (setq str (buffer-substring beg end))
		 (if (and (boundp 'dbx-no-output) dbx-no-output)
		     (progn
		       (delete-region (+ 1 beg) end)
		       (goto-char (point-max))))
		 str))))))
  (if (eq (process-buffer process) (current-buffer))
      (goto-char (point-max))))
  
(defun dbx-sentinel (process string)
  "Catch dbx exiting and clean up"
  (if (string-equal string "finished\n")
      (progn
	(insert "\nProcess has finished\n")
	(if overlay-arrow-position
	    (set-marker overlay-arrow-position nil)))))

(defun dbx-where (&optional string)
 "Display dbx'ed program in other window and point out what you are looking at.
STRING contains dbx's output."
  (interactive)
  (let (file line)
    (if (string-match dbx-break-point string)
	(progn
	  (setq line (substring string (match-beginning 2) (match-end 2)))
	  (setq file (substring string (match-beginning 4) (match-end 4)))
    (if (and file line)			;Find break point?
	(progn
; Look for the file in each of the dbx-dir-list directories
	  (let ((dir dbx-dir-list))
	    (while (and dir (not (file-readable-p
		    (expand-file-name (concat (car dir) "/" file) nil))))
	      (setq dir (cdr dir))
	      )
	    (if (not dir)
		(error (format "Can't find file %s" file)))
	    (setq file (expand-file-name (concat (car dir) "/" file) nil)))
	  (set-buffer (window-buffer))
	  (let ( (looking-at-file (and (buffer-file-name)
		  (string-equal (buffer-file-name) file))) )
	    (if (not looking-at-file) (find-file-other-window file))
	    (goto-line (string-to-int line))
	    (beginning-of-line)
	    (setq overlay-arrow-string "=>")
	    (or overlay-arrow-position 
		(setq overlay-arrow-position (make-marker)))
	    (set-marker overlay-arrow-position (point) (current-buffer))
	    (if (not looking-at-file) (other-window 1))
      )))))))

(defun dbx-stop-at ()
  "Set break point at current line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines
		     1 (save-excursion (beginning-of-line) (point)))))))
    (send-string dbx-process
		 (concat "stop at \"" file-name "\":" line "\n")))
    (accept-process-output dbx-process)
    ; now move point to end of buffer:
    (set-buffer (get-buffer (process-buffer dbx-process)))
    (save-window-excursion
      (switch-to-buffer-other-window (current-buffer))
      (goto-char (point-max)))
    (if (process-mark dbx-process)
	(set-marker (process-mark dbx-process) (point-max))))

(defun dbx-use (dirs)
  "Run dbx's `use' command and parse the output to make a list of
directories for the use of dbx-where. If no directories are specified,
get the current list and parse that"
  (interactive "suse: ")
  (setq dbx-dir-list nil)
  (save-window-excursion
    (if (string-match "[^ \t]" dirs)
	(progn
	  (send-string dbx-process (concat "use " dirs "\n"))
	  (delete-region (save-excursion (beginning-of-line) (dot)) (dot))))
    (send-string dbx-process "use\n")
    (accept-process-output dbx-process)
    (switch-to-buffer (process-buffer dbx-process))
    (end-of-buffer)
    (previous-line 1)
    (beginning-of-line)
    (if (looking-at shell-prompt-pattern)
	(goto-char (match-end 0)))
    (while (looking-at "[ \t]*[^ \t\n]+")
      (progn 
      (setq dbx-dir-list
	    (append dbx-dir-list (list (buffer-substring
					(match-beginning 0) (match-end 0)))))
      (goto-char (match-end 0))
      (skip-chars-forward " \t"))))
      (end-of-buffer))

(defun dbx-command (command)
  "Run a dbx COMMAND from the current buffer, suppressing output to *dbx-proc*.
If you have enabled tracing this enables you to step through the source.
This is probably most useful for `next', `step', and `cont'."
  (interactive "sCommand: ")
  (let ( (dbx-no-output t)		; used by dbx-filter to kill output
	 (dbx-wait t))			; set to nil when next prompt is ready
    (send-string dbx-process (concat command "\n"))
    (while dbx-wait
      (accept-process-output dbx-process))
    )
  (let ( (mark-buffer (marker-buffer overlay-arrow-position))
	 (bw))
    (setq bw (get-buffer-window mark-buffer))
    (if bw (select-window bw)
      (switch-to-buffer mark-buffer)))
  (goto-char (marker-position overlay-arrow-position)))

(defun dbx-cont ()
  "Key-bindable interface to dbx-command, for `cont' command"
  (interactive)
  (dbx-command "cont"))

(defun dbx-next ()
  "Key-bindable interface to dbx-command, for `next' command"
  (interactive)
  (dbx-command "next"))

(defun dbx-step ()
  "Key-bindable interface to dbx-command, for `step' command"
  (interactive)
  (dbx-command "step"))

