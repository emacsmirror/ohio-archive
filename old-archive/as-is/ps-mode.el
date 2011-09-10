;To: unix-emacs@bbn.com
;Date: 19 Dec 88 18:21:00 GMT
;From: mailrus!csd4.milw.wisc.edu!uxc!uxc.cso.uiuc.edu!m.cs.uiuc.edu!grunwald@bbn.com
;Subject: ps-mode.el
;
;
;If this is a double submission, please pardon me -- we've recently discovered
;that our local news interface (notes) appears to not forward local submissions
;in gnu.* to the rest of the world.
;
;The following is a handy interface to the unix ``ps'' command. I've been using
;this for several weeks, and have found it very useful.
;
;Dirk Grunwald
;Univ. of Illinois
;grunwald@m.cs.uiuc.edu

;; ps-mode commands for Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc,
;; or could be if they want it.
;;
;; Written by Dirk Grunwald, grunwald@m.cs.uiuc.edu
;; University of Illinois

;; This file is part of GNU Emacs, or could be if they want it.

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

(defvar ps-mode-buffer "*Process-Mode Info*" 
   "Buffer name of ps-mode information")

(defvar ps-mode-buffer-all "*All Process-Mode Info*" 
   "Buffer name of ps-mode information")

(defvar ps-mode-uptime-program "uptime"
  "Program to call to generate uptime information")

(defvar ps-mode-program "ps"
  "Program to call to generate process information")

(defvar ps-mode-kill-program "kill"
  "Program to call to kill a process")

(defvar ps-mode-nice-program "/etc/renice"
  "Program to call to nice a process")

(defvar ps-mode-nice-args "+4"
  "Default niceness")

(defvar ps-mode-program-args "ugx"
  "Arguments passed to ps-mode-program")

(defvar ps-mode-bogus-lines 3
  "Number of non-process lines at the top of the display")

(defvar ps-mode-pid-array nil
  "Array of process id's. Array index corresponds to line number in
   current ps-mode-buffer")

(defvar ps-mode-signal-array nil
  "Array of signals to be sent to individual processes. Each signal is
either a signal number or a signal name")

(defvar ps-mode-pid-position nil
  "Position of the PID field in the ps-mode buffer")

(defvar ps-mode-lines nil
  "Number of lines in the current ps-mode buffer")

;;
;; Issue a ps-mode-command to the current buffer, then build the array
;; of process id's and signal numbers.
;;
(defun ps-mode-build-process-list ()
  "kill processes from status display"
  (interactive)
  (let
      ((buffer-read-only nil))
					;
					; call ps-mode-program
					;
    (delete-region (point-min) (point-max))
    (call-process ps-mode-uptime-program nil t nil)
    (insert "\n")
    (call-process ps-mode-program nil t nil ps-mode-program-args)
    ;
    ; signals have four spaces for their symbols
    ;
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert "     ")
      (forward-line 1))
					;
					; find the word position of the
					; string ``PID'' in the header, since
					; this varies with different
					; versions of ps
					;
    (save-excursion
      (let ( bol eol (i 1) )
	(goto-char (point-min))
	(search-forward "PID")		; get to the ps output
	(beginning-of-line)
	(setq bol (point))
	(end-of-line)
	(setq eol (point))
	(narrow-to-region bol eol)
	(goto-char bol)
	(setq ps-mode-pid-position nil)
	(while (or (not (eolp)) (not (numberp ps-mode-pid-position)))
	  (if (looking-at " *PID")
	      (setq ps-mode-pid-position i))
	  (setq i (+ i 1))
	  (forward-word 1))
	(widen)))
    (setq ps-mode-lines (count-lines (point-min) (point-max)))
    (setq ps-mode-pid-array (make-vector (+ ps-mode-lines 1) nil))
    (setq ps-mode-signal-array (make-vector (+ ps-mode-lines 1) nil))
    (goto-char (point-min))
    (let
	( (i ps-mode-bogus-lines)
	  (to-skip (- ps-mode-pid-position 1))
	  pid-start pid-end this-pid)
      (forward-line i)
      (while (not (eobp))
	(beginning-of-line)
	(forward-word to-skip)	; skip to beginning of PID field
	(setq pid-start (point))
	(forward-word 1)
	(setq pid-end (point))
	(setq this-pid
	      (string-to-int (buffer-substring pid-start pid-end)))
	(aset ps-mode-pid-array i
	      (if (> this-pid 0) this-pid nil))
	(setq i (+ i 1))
	(forward-line 1))))
  (goto-char (point-min))
  (forward-line ps-mode-bogus-lines))

(defun ps-mode ()
  "A major-mode for sending signals to processes.
In ps-mode, you indicate signals to send to UNIX processes.
Signals are marked on the left hand side of the display using
an abbreviated name.

The following signals may be sent. To mark a process, move to the line
corresponding to that process and hit one of the captialized letters
in the list below (the lower case letters work as well).

Hup	-- SIGHUP
Int	-- SIGINT
Quit	-- SIGQUIT
Kill	-- SIGKILL
Bus	-- SIGBUS
segV	-- SIGSEGV
Alrm	-- SIGALRM
Term	-- SIGTERM
Stop	-- SIGSTOP
Cont	-- SIGCONT
Nice	-- Nice that sucker
U	-- clear a previously marked signal
?	-- Update the process list

To issue these signals, type \\C-c\\C-c or `x'.
To avoid silly mistake, the `q' key is not a synonym for `Q',
it exits ps-mode."

  (interactive)
  (pop-to-buffer (get-buffer-create ps-mode-buffer))
;  (kill-all-local-variables)
  (setq major-mode 'ps-mode)
  (setq mode-name "Process Mode")
  ;;
  ;; local bindings
  ;;
  (local-set-key "\C-c\C-c" 'ps-mode-issue-signals)
  (local-set-key "x" 'ps-mode-issue-signals)
  (local-set-key "X" 'ps-mode-issue-signals)
  (local-set-key "q" 'ps-mode-quit)
  (local-set-key " " 'next-line)
  (local-set-key "H" 'ps-mode-mark-hangup)
  (local-set-key "h" 'ps-mode-mark-hangup)
  (local-set-key "I" 'ps-mode-mark-int)
  (local-set-key "i" 'ps-mode-mark-int)
  (local-set-key "Q" 'ps-mode-mark-quit)
  (local-set-key "K" 'ps-mode-mark-kill)
  (local-set-key "k" 'ps-mode-mark-kill)
  (local-set-key "B" 'ps-mode-mark-bus)
  (local-set-key "b" 'ps-mode-mark-bus)
  (local-set-key "V" 'ps-mode-mark-segv)
  (local-set-key "v" 'ps-mode-mark-segv)
  (local-set-key "A" 'ps-mode-mark-alarm)
  (local-set-key "a" 'ps-mode-mark-alarm)
  (local-set-key "T" 'ps-mode-mark-term)
  (local-set-key "t" 'ps-mode-mark-term)
  (local-set-key "S" 'ps-mode-mark-stop)
  (local-set-key "s" 'ps-mode-mark-stop)
  (local-set-key "C" 'ps-mode-mark-cont)
  (local-set-key "c" 'ps-mode-mark-cont)
  (local-set-key "N" 'ps-mode-mark-nice)
  (local-set-key "n" 'ps-mode-mark-nice)
  (local-set-key "u" 'ps-mode-mark-unmark)
  (local-set-key "U" 'ps-mode-mark-unmark)
  (local-set-key "?" 'ps-mode-build-process-list)
  ;;
  (setq truncate-lines t)
  (set-syntax-table text-mode-syntax-table)
  (setq buffer-read-only t)
  (ps-mode-build-process-list)
  (run-hooks 'ps-mode-hook))

;;
;; like ps-mode, but do it for everyone by appending "a" to the args list
;;
(defun ps-mode-all ()
  (interactive)
  (let
      ((global-ps-mode-program-args (concat ps-mode-program-args "a")))
    (pop-to-buffer ps-mode-buffer-all)
    (make-variable-buffer-local 'ps-mode-buffer)
    (setq ps-mode-buffer ps-mode-buffer-all)
    (make-variable-buffer-local 'ps-mode-program-args)
    (setq ps-mode-program-args global-ps-mode-program-args)
    (ps-mode)))

(defun ps-mode-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;;
;;	mark line and go forward signal line. Don't allow them to
;;	mark the first line (line 0)
;;
(defun ps-mode-mark-line (signal-symbol this-signal)
  (let
      ((this-line (count-lines (point-min) (point))))
    (if (not (bolp))
	(setq this-line (- this-line 1)))
    (save-excursion
      (if (> this-line 0)
	  (if (aref ps-mode-pid-array this-line)
	      (let
		  ((buffer-read-only nil))
		(beginning-of-line)
		(delete-char (length signal-symbol))
		(insert signal-symbol)
		(aset ps-mode-signal-array this-line this-signal)))))
    (next-line 1)))

(defun ps-mode-mark-unmark ()
  (interactive)
  (ps-mode-mark-line "     " nil))

(defun ps-mode-mark-hangup ()
  (interactive)
  (ps-mode-mark-line "Hup" "HUP"))

(defun ps-mode-mark-int ()
  (interactive)
  (ps-mode-mark-line "Int" "INT"))

(defun ps-mode-mark-quit ()
  (interactive)
  (ps-mode-mark-line "Quit" "QUIT"))

(defun ps-mode-mark-kill ()
  (interactive)
  (ps-mode-mark-line "Kill" "KILL"))

(defun ps-mode-mark-bus ()
  (interactive)
  (ps-mode-mark-line "Bus" "BUS"))

(defun ps-mode-mark-segv ()
  (interactive)
  (ps-mode-mark-line "segV" "SEGV"))

(defun ps-mode-mark-alarm ()
  (interactive)
  (ps-mode-mark-line "Alrm" "ALRM"))

(defun ps-mode-mark-term ()
  (interactive)
  (ps-mode-mark-line "Term" "TERM"))

(defun ps-mode-mark-stop ()
  (interactive)
  (ps-mode-mark-line "Stop" "STOP"))

(defun ps-mode-mark-cont ()
  (interactive)
  (ps-mode-mark-line "Cont" "CONT"))

(defun ps-mode-mark-nice ()
  (interactive)
  (ps-mode-mark-line "Nice" "NICE"))

(defun ps-mode-signal-process (this-pid this-signal)
  (if (string-equal this-signal "NICE")
      (call-process ps-mode-nice-program nil nil nil
		      ps-mode-nice-args (int-to-string this-pid) )
    (call-process ps-mode-kill-program nil nil nil
		  (concat "-" 
			  (if (numberp this-signal)
			      (int-to-string this-signal)
			    this-signal))
		  (int-to-string this-pid))))

(defun ps-mode-issue-signals ()
  (interactive)
  (let
      ((i 0))
    (beginning-of-buffer)
    (while (< i ps-mode-lines)
      (if (aref ps-mode-signal-array i)
	    (ps-mode-signal-process (aref ps-mode-pid-array i)
				    (aref ps-mode-signal-array i)))
      (setq i (+ i 1))
      ))
  (ps-mode-build-process-list))

