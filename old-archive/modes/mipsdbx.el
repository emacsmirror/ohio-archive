; Newsgroups: gnu.emacs.help,gnu.emacs.sources
; Path: hal.com!decwrl!uunet!mcsun!sunic!aun.uninett.no!nuug!nntp.uio.no!egilp
; From: egilp@ulrik.uio.no (Egil Pedersen)
; Subject: Re: Is there a dbx-mode for DEC's Ultrix dbx?
; In-Reply-To: whaley@kpc.com's message of Mon, 16 Nov 1992 03:20:27 GMT
; Organization: University of Oslo, Norway
; Date: Mon, 16 Nov 1992 10:28:14 GMT
; 
; In article <whaley.721884027@kpc.com> whaley@kpc.com (Kenneth Whaley) writes:
; 
;  Unfortunately, DEC's Ultrix dbx outputs different strings  for breakpoints,
;  etc. than the dbx that all the "dbx.el"s I've found on the net are
;  written to work with.  If it were simply a matter of changing the 
;  regexp of breakpoints, then I could get this to work, but
;  unfortunately the DEC dbx does not print out the file name at every
;  breakpoint, only the line number (and the breakpoint number, the function
;  name, and the PC).
; 
; You may try this one. It works with 18.58. I do not remember where I got it.
;

 
;; Run MIPS dbx under Emacs
;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.
;; Some ideas are due to Masanobu.
;; Converted from gdb.el to mipsdbx.el by Kung and Killian.

;; LCD Archive Entry:
;; mipsdbx.el|Egil Pedersen|egilp@ulrik.uio.no|
;; Run MIPS dbx under Emacs.|
;; 92-11-16||~/modes/mipsdbx.el.el.Z|

;; This file is part of GNU Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; Description of DBX interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using dbx to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command DBX to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Dbx will be invoked on this file, in a
;; window named *dbx-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the dbx command next on \M-n
;; (def-dbx next "\M-n")

;; This causes the emacs command dbx-next to be defined, and runs
;; dbx-display-frame after the command.

;; dbx-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the dbx window.  For example after a dbx-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the dbx buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; dbx-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'shell)

(defvar dbx-prompt-pattern "^(.*dbx[+]?) *"
  "A regexp to recognize the prompt for dbx or dbx+.")

(defvar dbx-mode-map nil
  "Keymap for dbx-mode.")

(if dbx-mode-map
   nil
  (setq dbx-mode-map (copy-keymap shell-mode-map))
  (define-key dbx-mode-map "\C-l" 'dbx-refresh))

(define-key ctl-x-map " " 'dbx-break)
(define-key ctl-x-map "&" 'send-dbx-command)

;;Of course you may use `def-dbx' with any other dbx command, including
;;user defined ones.

(defmacro def-dbx (name key &optional doc)
  (let* ((fun (intern (format "dbx-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'dbx-call cstr))
	  (list 'define-key 'dbx-mode-map key  (list 'quote fun)))))

(def-dbx "step"   "\M-s" "Step one source line with display")
(def-dbx "stepi"  "\M-i" "Step one instruction with display")
(def-dbx "next"   "\M-n" "Step one source line (skip functions)")
(def-dbx "cont"   "\M-c" "Continue with display")

(def-dbx "quit!" "\C-c\C-f" "Finish executing current function")
(def-dbx "up"     "\M-u"   "Go up N stack frames (numeric arg) with display")
(def-dbx "down"   "\M-d"   "Go down N stack frames (numeric arg) with display")

(defun dbx-mode ()
  "Major mode for interacting with an child Dbx process.
The following commands are available:

\\{dbx-mode-map}

\\[dbx-display-frame] displays in the other window
the last line referred to in the dbx buffer.

\\[dbx-step],\\[dbx-next], and \\[dbx-nexti] in the dbx window,
call dbx to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[dbx-break].

Commands:
Many commands are inherited from shell mode.
Additionally we have:

\\[dbx-display-frame] display frames file in other window
\\[dbx-step] advance one line in program
\\[dbx-next] advance one line in program (skip over calls).
\\[send-dbx-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dbx-mode)
  (setq mode-name "Child Dbx")
  (setq mode-line-process '(": %s"))
  (use-local-map dbx-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'dbx-last-frame)
  (setq dbx-last-frame nil)
  (make-local-variable 'dbx-last-frame-displayed-p)
  (setq dbx-last-frame-displayed-p t)
  (make-local-variable 'dbx-delete-prompt-marker)
  (setq dbx-delete-prompt-marker nil)
  (make-local-variable 'dbx-filter-accumulator)
  (setq dbx-filter-accumulator nil)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern dbx-prompt-pattern)
  (run-hooks 'shell-mode-hook 'dbx-mode-hook))

(defvar current-dbx-buffer nil)

(defvar dbx-command-name "dbx"
  "Pathname for executing dbx.")

(defun dbx (path &optional dbxargs)
  "Run dbx on program FILE in buffer *dbx-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for DBX.  If you wish to change this, use
the DBX commands `cd DIR' and `directory'."
  (interactive "FRun dbx on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*dbx-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (if dbxargs
	(make-shell (concat "dbx-" file) dbx-command-name nil "-emacs" dbxargs file)
      (make-shell (concat "dbx-" file) dbx-command-name nil "-emacs" file))
    (dbx-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'dbx-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'dbx-sentinel)
    (dbx-set-buffer)))

(defun dbx-set-buffer ()
  (cond ((eq major-mode 'dbx-mode)
	(setq current-dbx-buffer (current-buffer)))))

;; This function is responsible for inserting output from DBX
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that DBX prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun dbx-filter (proc string)
  (let ((inhibit-quit t))
    (if dbx-filter-accumulator
	(dbx-filter-accumulate-marker proc
				      (concat dbx-filter-accumulator string))
	(dbx-filter-scan-input proc string))))

(defun dbx-filter-accumulate-marker (proc string)
  (setq dbx-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq dbx-last-frame
			  (cons (substring string 2 first-colon)
				(cons (string-to-int
				 	(substring string (1+ first-colon)
					    second-colon))
				      (string-to-int
				 	(substring string (1+ second-colon)
					    end))))))
		  (setq dbx-last-frame-displayed-p nil)
		  (dbx-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq dbx-filter-accumulator string)))
	(dbx-filter-insert proc "\032")
	(dbx-filter-scan-input proc (substring string 1)))
    (setq dbx-filter-accumulator string)))

(defun dbx-filter-scan-input (proc string)
  (if (equal string "")
      (setq dbx-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (dbx-filter-insert proc (substring string 0 start))
		   (dbx-filter-accumulate-marker proc
						 (substring string start)))
	    (dbx-filter-insert proc string)))))

(defun dbx-filter-insert (proc string)
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
	  (dbx-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (dbx-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun dbx-sentinel (proc msg)
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
	     ;; if obuf is the dbx buffer.
	     (set-buffer obuf))))))


(defun dbx-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (dbx-display-frame))

(defun dbx-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from DBX.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (dbx-set-buffer)
  (and dbx-last-frame (not nodisplay)
       (or (not dbx-last-frame-displayed-p) (not noauto))
       (progn (dbx-display-line (car dbx-last-frame)
       				(car (cdr dbx-last-frame))
				(cdr (cdr dbx-last-frame)))
	      (setq dbx-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun dbx-display-line (true-file line chpos)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(cond ((not (= chpos 0)) (setq overlay-arrow-string "=>"))
	      (t (setq overlay-arrow-string "")))
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun dbx-call (command)
  "Invoke dbx COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq dbx-delete-prompt-marker (point-marker))
  (dbx-set-buffer)
  (send-string (get-buffer-process current-dbx-buffer)
	       (concat command "\n")))

(defun dbx-maybe-delete-prompt ()
  (if (and dbx-delete-prompt-marker
	   (> (point-max) (marker-position dbx-delete-prompt-marker)))
      (let (start)
	(goto-char dbx-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq dbx-delete-prompt-marker nil))))

(defun dbx-break ()
  "Set DBX breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (send-string (get-buffer-process current-dbx-buffer)
		 (concat "stop at \"" file-name "\":" line "\n"))))

(defun dbx-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar dbx-commands nil
  "List of strings or functions used by send-dbx-command.
It is for customization by you.")

(defun send-dbx-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the dbx buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list dbx-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of dbx-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg dbx-commands)))
    (setq addr (dbx-read-address))
    (if (eq (current-buffer) current-dbx-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-dbx-buffer)
    (goto-char (dot-max))
    (insert-string comm)))
