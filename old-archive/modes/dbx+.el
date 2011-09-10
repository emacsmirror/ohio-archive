;From: lupton@uhccux.uhcc.hawaii.edu (Robert Lupton)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: dbx.el
;Message-ID: <4432@uhccux.uhcc.hawaii.edu>
;Date: 26 Jul 89 20:51:27 GMT
;Organization: University of Hawaii
;Lines: 530
;Keywords: dbx gnuemacs
;
;
;Here's (yet another) repost of dbx.el. I've been doing some debugging
;(of C that is, not lisp) and improved it somewhat. The diffs are
;bigger than the patches, so it's all here. Thanks to Brian for alpha
;testing some of it.
;
;The main changes are:
;		(In my previous posting)

;   Added dbx-use to follow `use' commands.
;   If tracing is enabled, it now understands up/down/where and
; bus errors, segmentation violations, etc. (providing that no-one has
; messed with dbx-error messages. E.g. Sun).
;   Added dbx-command which can be bound to keys, to allow stepping
; through source without having a *dbx* window
;   Now runs dbx in current directory, but adds source directory to default
; dbx-dir-list. This allows post-mortem debugging.
;   dbx-command takes an optional repeat-count; this may be provided as
; a prefix argument to dbx-step etc.

;		(new in this posting)

;   dbx-print will pass a string or the region to dbx's `print' command,
; and provide the answer in the minibuffer; dbx-print-word will print the
; value of the word under the cursor
;   Only one dbx process is now permitted
;   A prefix argument to dbx-where will try to show the stack frame under the
; cursor (good for going through `where' commands)
;   Added dbx-compile which is the same as compile (q.v.), except that
; it first saves dbx's status (break etc.), then runs compile, and then
; restarts dbx

;   Plus sundry other changes (some to the documentation).

;Bug fixes/Flames (surely not!)/Improvements to lupton@uhccux.uhcc.hawaii.edu


;				Robert
;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Run dbx under Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Main author Masanobu UMEDA (umerin@flab.fujitsu.junet)
;; Secondary Author Robert Lupton (lupton@uhifa.ifa.hawaii.edu)

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
; bus errors, segmentation violations, etc. (providing that no-one has
; messed with dbx-error messages. E.g. Sun).
;   Added dbx-command which can be bound to keys, to allow stepping
; through source without having a *dbx* window
;      RHL April 1989
;   Now runs dbx in current directory, but adds source directory to default
; dbx-dir-list. This allows post-mortem debugging.
;   dbx-command takes an optional repeat-count; this may be provided as
; a prefix argument to dbx-step etc.
;   dbx-print will pass a string or the region to dbx's `print' command,
; and provide the answer in the minibuffer; dbx-print-word will print the
; value of the word under the cursor
;   Only one dbx process is now permitted
;      RHL June 1989
;   A prefix argument to dbx-where will try to show the stack frame under the
; cursor (good for going through `where' commands)
;   Added dbx-compile which is the same as compile (q.v.), except that
; it first saves dbx's status (break etc.), then runs compile, and then
; restarts dbx
;
(require 'shell)

(defvar dbx-trace-flag nil
  "Dbx trace switch.")

(defvar dbx-process nil
  "Dbx process name.")

(defvar dbx-dir-list nil
  "List of directories searched by dbx-where, set by dbx-use")

(defvar dbx-break-point
  "\\( in .* at\\|),\\) line \\([0-9]*\\)[^\"]*\"?\\([^\"]*\\)?\"?"
  "Regexp of pattern that dbx writes at break points, etc.
the second regular expression in \\\\(\\\\) should give the line number,
the third the file name")

(defvar dbx-last-output
  "The last output from dbx that was suppressed by setting dbx-no-output")

(defvar dbx-downcase-for-print nil
  "*If set, convert strings to lowercase before submitting to dbx's `print'")

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

You can display the debugging program in other window and point out
where you are looking at using the command \\[dbx-where]. With a
prefix argument, it'll try to display the position described by the
text under the cursor (this is meant to be used to examine a stack
trace produced by the `where' command; put the cursor on the level you
want to see, and type ^U\\[dbx-where]).

\\[dbx-trace-mode] toggles dbx-trace mode. In dbx-trace mode, the
debugging program is automatically traced using output from dbx.  The
dbx commands `down', `up' and `where' are also traced (If no-one has
messed with error formats, e.g. Sun)

Using the command dbx-command (e.g. M-x dbx-command \"step\") allows
you to step through the source without using the *dbx* buffer.
The commands dbx-cont, dbx-next, and dbx-step use this. They take an
optional prefix argument to indicate how many steps to take. If no
dbx-prompt is emitted (e.g. control is returned to your code) these
may appear to hang: either concatenate your prompt onto the
shell-prompt-pattern string, or hit ^G.

The command dbx-print passes a string to dbx's print command, and
displays the answer in the mini-buffer. If the string is either nil or
empty the current region will be used (i.e. from dot to mark).
dbx-print-word uses this command to print the value of the word under
the cursor. If the variable dbx-downcase-for-print is set then the
string will be converted to lowercase before use.

All of the commands based on dbx-command assume that you have a dbx prompt
in the *dbx* buffer, as they use the dbx process to do their work.

The command \\[dbx-stop-at] sets a break point at the current line of
the program in the buffer. Major mode name of the buffer must be in
dbx-language-mode-list.

You should use the command `dbx-use' (in the mini-buffer, or \\[dbx-use])
rather than `use' so dbx-mode can find your source directories. (Using
use, then dbx-use with no directories specified also works).

If you need to recompile, dbx-compile will first save dbx's environment
(using the status command), then run compile (q.v.), then restart dbx
restoring breakpoints, tracing, etc.

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
  (setq mode-name "Dbx")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-dbx-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (setq dbx-trace-flag nil)
  (make-variable-buffer-local 'shell-prompt-pattern)
  (setq dbx-prompt ".*(dbx) *")
  (setq shell-prompt-pattern dbx-prompt) ;Set dbx prompt pattern
  (or (assq 'dbx-trace-flag minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(dbx-trace-flag " Trace") minor-mode-alist))))

(defun dbx () "An alias for run-dbx"
  (interactive)
  (call-interactively 'run-dbx))

(defun run-dbx (path)
  "Run inferior Dbx process on PROGRAM, with I/O via buffer *dbx*.

   Running this function calls the value of dbx-mode-hook with no
arguments, if that value is non-nil. Likewise with the value of
shell-mode-hook. dbx-mode-hook is called after shell-mode-hook.  When
the hooks are called, the variable path contains the full path name of
the programme being debugged."

  (interactive "fProgram to debug: ")
  (if (and dbx-process
	   (let ( (status (process-status dbx-process)) )
	     (not (or (equal status 'exit) (equal status 'signal)))))
      (progn
	(let ( (pbuff (process-buffer dbx-process)) pwind )
	  (if (setq pwind (get-buffer-window pbuff))
	      (select-window pwind)
	    (switch-to-buffer pbuff)))
	(if (yes-or-no-p "You already running dbx: kill it? ")
	    (progn
	      (delete-region (point-min) (point-max))
	      (delete-process dbx-process))
	  (error ""))))
  (setq path (expand-file-name path))
  (switch-to-buffer "*dbx*")
  (switch-to-buffer (make-shell "dbx" "dbx" nil path))
  (setq dbx-process (get-buffer-process (current-buffer)))
  (set-process-filter dbx-process 'dbx-filter)
  (set-process-sentinel dbx-process 'dbx-sentinel)
  (inferior-dbx-mode)
  (let ( (dbx-wait t) )
    (while dbx-wait
      (accept-process-output dbx-process)))
  (goto-char (point-max))
  (dbx-use (concat ". " (file-name-directory path)))
  (let ( ( tracing dbx-trace-flag) )
    (dbx-trace-mode 1)
    (if (and (boundp 'dbx-restart-file) (file-readable-p dbx-restart-file))
	(dbx-command (concat "source " dbx-restart-file) 1 t))
    (if (not tracing)
	(dbx-trace-mode -1)))
  (run-hooks 'shell-mode-hook 'dbx-mode-hook))

(defun dbx-trace-mode (arg)
  "Toggle dbx-trace mode.
With arg, turn dbx-trace mode on iff arg is positive.
In dbx-trace mode, user program is automatically traced."
  (interactive "P")
  (setq dbx-trace-flag
	(if (null arg)
	    (not dbx-trace-flag)
	  (> (prefix-numeric-value arg) 0)))
  (if (and (not dbx-trace-flag) overlay-arrow-position)
      (set-marker overlay-arrow-position nil))
  ;; Force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun dbx-filter (process string)
  "Trace debugging program automatically if dbx-trace-flag is not nil.
Checks two flags: dbx-no-output suppresses output to buffer *dbx* if it exists
and is non-nil, and dbx-wait is set to nil (if it exists) when the dbx prompt
is received. If your programme has some prompt of its own then you should
concatenate it with dbx-prompt, i.e.
 (setq shell-prompt-pattern (concat dbx-prompt \"\\\\|my_prompt\")
if you don't want dbx-no-output to throw away all output until the next
dbx-prompt appears."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string)
    (if (process-mark process)
	(set-marker (process-mark process) (point-max)))
    (goto-char (point-max))
    (if (string-match shell-prompt-pattern string)	; the next prompt
	(progn
	  (if (and (boundp 'dbx-no-output)
		   (not (string-match dbx-prompt string)))
	      (setq dbx-no-output nil))	; we do want output at programme prompt
	  (if (boundp 'dbx-wait) (setq dbx-wait nil))
	  (let ((end (point))
		(beg)
		(search-start (save-excursion (forward-line -20) (point)))
		(str))
	    (re-search-backward shell-prompt-pattern search-start t)
	    (if (re-search-backward shell-prompt-pattern search-start t)
		(setq beg (match-end 0))
	      (setq beg search-start))
	    (setq str (buffer-substring beg end))
	    (if (and (boundp 'dbx-no-output) dbx-no-output)
		(progn
		  (setq dbx-last-output str)
		  (delete-region beg end)
		  (goto-char (point-max))))
	    (if dbx-trace-flag (dbx-show-where str))))))
  (if (eq (process-buffer process) (current-buffer))
      (goto-char (point-max))))
  
(defun dbx-sentinel (process string)
  "Catch dbx exiting and clean up"
  (if (equal string "finished\n")
      (progn
	(insert "\nProcess has finished\n")
	(dbx-trace-mode -1)
	(if overlay-arrow-position
	    (set-marker overlay-arrow-position nil))
	(if (and (boundp 'dbx-restart-file)
		 (file-readable-p dbx-restart-file))
	    (delete-file dbx-restart-file)))))

(defun dbx-where (arg)
  "Display dbx'ed program in other window and point out what you are
looking at.  With a prefix argument, try to interpret the line under
the cursor as part of a stack trace as produced by the `where'
command) and display it."
  (interactive "p")
  (if (= arg 1)				; just run where
      (dbx-command "where" 1 t)
    (let ( (beg (save-excursion (beginning-of-line) (point))) )
      (dbx-show-where (buffer-substring beg (point-max))))))

(defun dbx-show-where (string)
  "Display dbx'ed program in other window and point out what you are
looking at.  STRING contains dbx's output."
;
; This function can be recursive via dbx-filter, hence dbx-file is not 
; inside a (let ...)
;
  (let (file line)
    (if (string-match dbx-break-point string)
	(progn
	  (setq line (substring string (match-beginning 2) (match-end 2)))
	  (setq file (substring string (match-beginning 3) (match-end 3)))
	  (if (equal file "")		; don't know where we are
	      (if (boundp 'dbx-loop)
		  (error "Sorry, I can't figure out which file you are in")
		(let ((dbx-loop nil))	; no file name: try a `where'
		  (dbx-command "where" 1 t)
		  (string-match dbx-break-point dbx-last-output)))
	    (if (string-match "/[^/]*$" file)
		(setq file (substring file (+ 1 (match-beginning 0)) nil)))
	    (setq dbx-file file))
	  (if (and dbx-file line)			;Found break point?
	      (progn
		;; Look for the file in each of the dbx-dir-list directories
		(let ((dir dbx-dir-list))
		  (while (and dir (not (file-readable-p
		      (expand-file-name (concat (car dir) "/" dbx-file) nil))))
		    (setq dir (cdr dir)))
		  (if (not dir)
		      (error (format "Can't find file %s" dbx-file)))
		  (setq dbx-file
		       (expand-file-name (concat (car dir) "/" dbx-file) nil)))
		(set-buffer (window-buffer))
		(let ( (looking-at-file
			(and (buffer-file-name)
			     (equal (buffer-file-name) dbx-file))) )
		  (if (not looking-at-file)
		      (find-file-other-window dbx-file))
		  (goto-line (string-to-int line))
		  (beginning-of-line)
		  (setq overlay-arrow-string "=>")
		  (or overlay-arrow-position 
		      (setq overlay-arrow-position (make-marker)))
		  (set-marker overlay-arrow-position (point) (current-buffer))
		  (if (not looking-at-file) (other-window 1)))))))))

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
    (let ( (dbx-wait t) )		; set to nil when next prompt is ready
      (if (string-match "[^ \t]" dirs)
	  (let ( (dbx-no-output t) )	; used by dbx-filter to kill output
	    (send-string dbx-process (concat "use " dirs "\n"))
	    (while dbx-wait (accept-process-output dbx-process))))
      (setq dbx-wait t)
      (send-string dbx-process "use\n")
      (while dbx-wait (accept-process-output dbx-process)))
    (switch-to-buffer (process-buffer dbx-process))
    (end-of-buffer)
    (previous-line 1)
    (beginning-of-line)
    (while (looking-at shell-prompt-pattern)
	(goto-char (match-end 0)))
    (while (looking-at "[ \t]*[^ \t\n]+")
      (progn 
	(goto-char (match-end 0))
	(skip-chars-forward " \t")
	(setq dbx-dir-list
	      (append dbx-dir-list
		      (list
		       (let ((dir (buffer-substring
				   (match-beginning 0) (match-end 0))))
			 (if (string-match "/$" dir)
			     (setq dir (substring dir 0 -1))
			   dir))))))))
  (end-of-buffer)
  (kill-region (progn (previous-line 1) (dot)) (point-max)))

(defun dbx-command (command &optional num no-trace)
  "Run a dbx COMMAND from the current buffer, suppressing output to *dbx*.
If you have enabled tracing this enables you to step through the source.
This is probably most useful for `next', `step', and `cont'.
Optionally, provide a repeat count NUM. If NO-TRACE is t, don't trace
position in source"
  (interactive "sCommand: ")
  (if (not dbx-trace-flag)
      (error "This command only works if tracing is turned on"))
  (if (not num) (setq num 1))
  (while (>= (setq num (- num 1)) 0)
    (let ( (dbx-no-output t)		; used by dbx-filter to kill output
	   (dbx-wait t))		; set to nil when next prompt is ready
      (send-string dbx-process (concat command "\n"))
      (while dbx-wait
	(accept-process-output dbx-process))
      )
    (if (not no-trace)
    (let ( (mark-buffer) (bw) )
      (if (and overlay-arrow-position
	       (setq mark-buffer (marker-buffer overlay-arrow-position)))
	  (progn
	    (setq bw (get-buffer-window mark-buffer))
	    (if bw (select-window bw)
	      (switch-to-buffer mark-buffer))
	    (goto-char (marker-position overlay-arrow-position))))))))

(defun dbx-cont (num)
  "Key-bindable interface to dbx-command, for `cont' command"
  (interactive "p")
  (dbx-command "cont" num))

(defun dbx-next (num)
  "Key-bindable interface to dbx-command, for `next' command"
  (interactive "p")
  (dbx-command "next" num))

(defun dbx-step (num)
  "Key-bindable interface to dbx-command, for `step' command"
  (interactive "p")
  (dbx-command "step" num))

(defun dbx-print ( &optional expr)
  "Feed an expression to dbx as the subject of a `print' command, and
output the answer in the minibuffer. If the expression is nil or empty
print the region in the current buffer. If the variable dbx-downcase-for-print
is set, convert the expression to lowercase before using it (this may
be useful for languages like fortran). See dbx-print-word for printing
the word under the cursor."
  (interactive "sExpression: ")
  (if (or (not expr) (equal expr ""))
      (setq expr (buffer-substring (region-beginning) (region-end))))
  (if dbx-downcase-for-print
      (setq expr (downcase expr)))
  (dbx-command (concat "print " expr) 1 t)
  (string-match dbx-prompt dbx-last-output)
  (let ( (val (substring dbx-last-output 0 (- (match-beginning 0) 1))) )
    (if (string-match "= *" val)
	(setq val (substring val (match-end 0) nil)))
    (message (format "%s : %s" expr val))))

(defun dbx-print-word (num)
  "print the value of the word under the cursor in the mini-buffer,
with a prefix-argument simply call dbx-print"
  (interactive "p")
  (if (not (= num 1))			; there's a prefix argument
      (call-interactively 'dbx-print)
    (save-excursion
      (let ( (beg (progn (forward-word 1) (dot)))
	     (end (progn (forward-word -1) (dot))) )
	(dbx-print (buffer-substring beg end))))))

(defun dbx-compile ()
  "Compile a programme. Just like compile, except that it first saves
the state of the current dbx session, then compiles, then restarts
dbx. If the compilation fails the status should be recovered when a
dbx-compile finally succeeds. If you exit dbx (`quit'), then the
status information is lost, but dbx-compile will still attempt to
restart the same programme."
  (interactive)
  (if (and (boundp 'dbx-process) (equal (process-status dbx-process) 'run))
      (progn
	(if (and (boundp 'dbx-restart-file)
		 (file-readable-p dbx-restart-file))
	    (delete-file dbx-restart-file))
	(setq dbx-restart-file (make-temp-name "/tmp/dbx_"))
	(let ( (dbx-wait t)
	       (pt (point)))
	  (send-string dbx-process (concat "status > " dbx-restart-file "\n"))
	  (while dbx-wait
	    (accept-process-output dbx-process))
	  (set-process-sentinel dbx-process nil) ; don't delete restart file
	  (kill-process dbx-process)
	  (delete-region (point) pt))))
  (call-interactively 'compile)
  (set-process-sentinel compilation-process 'compilation-sentinel2))
;
; Replace the compilation sentinel by one that restarts dbx
;
(defun compilation-sentinel2 (proc msg)
  (compilation-sentinel proc msg)
  (if (or (equal msg "exit\n") (equal msg "finished\n"))
      (if (y-or-n-p "restart dbx? ")
	  (let ( (pgm (car (reverse (process-command dbx-process)))))
	    (set-buffer (process-buffer dbx-process))
	    (goto-char (point-max))
	    (insert "\nIf line numbers have changed, break points may be wrong\n")
	    (run-dbx pgm)))))
