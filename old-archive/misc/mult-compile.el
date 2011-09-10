; Path: dg-rtp!rock.concert.net!mcnc!stanford.edu!bu.edu!dimacs.rutgers.edu!mips!zaphod.mps.ohio-state.edu!wuarchive!uunet!wsrcc.com!wolfgang
; From: wolfgang@wsrcc.com (Wolfgang S. Rupprecht)
; Newsgroups: comp.emacs,comp.unix.aix,gnu.emacs.sources
; Subject: Re: GNU Emacs with xlc on AIX3.1 Was: Shutdown: EMACS vs. vi
; Date: 6 Jun 91 15:39:08 GMT
; References: <2995.9106031725@seq.hull.ac.uk> <1991Jun04.063328.21379@lynx.CS.ORST.EDU> <Q=4_#5@uzi-9mm.fulcrum.bt.co.uk>
; Organization: Wolfgang S Rupprecht Computer Consulting, Washington DC.
; 
; igb@fulcrum.bt.co.uk (Ian G Batten) writes:
; >> Speaking of line number and related stuff, is there a way to let the GNU
; >> Emacs 18.57 to understand the error messages from cc on AIX 3.1? 
; 
; >I'm on the point of writing the code to do this.  Has anyone else
; >started?
; 
; Ok, I'll bite.  
; 
; Here is a general error message parser that I have been using in
; various forms since 1987.  It is all table driven and adding another
; parser template is as trivial as adding one regexp to describe the
; error output.
; 
; It already knows about most errors that I have needed to parse.  In
; addition it does 3-window hacks for lint's inconsistent usage
; messages.  It can also hunt for files (as in make's "Make: error on
; line 31" type errors, or the brain damaged multi-line System V lint
; output.)
; 
; Unlike the normal emacs function, this next error is cursor-driven,
; and doesn't pre-parse any errors.  One can select an error message by
; moving the cursor to the line *above* the desired message and invoking
; next-error.
; 
; This file also contains a multiple *compilation* buffer hack.  This
; allows one to compile in one buffer, grep in another, and watch
; something else in a third.  One selects the current compilation buffer
; (for next-error use) by typing "C-u C-x `" from inside the buffer one
; wants to select.  (Anyone have any good ideas on how to input &optional
; args from (interactive)?  Perhaps a "M-x command<space>option" syntax?)
; 
; I normally start off a compilation and then rename the *compilation*
; buffer to be something more descriptive (eg. *make-dirname* ,
; *grep-dirname*, *lint-dirname*, etc.).  This is just the thing when
; one has greps of several source trees, and a few compiles active and
; needs to switch between them.
; 
; One can also have wrappers to start up compilations in different
; buffers as in:
; 
;     (defun gidbuild (command)
;       (interactive (list (read-input "Run gid (with args): "
; 				     (symbol-around-point))))
;       (let ((default-directory (substitute-in-file-name "$BUILD")))
;       (compile1 (concat "gid " command)
; 		"No more gidbuild hits" "gidbuild" "*gidbuild*")))
; 
; To install it just put the the rest of the message into a single file
; called 'compile.el' somewhere in your emacs load-path.
; 
; -wolfgang
; ---
; Wolfgang Rupprecht    wolfgang@wsrcc.com (or) uunet!wsrcc!wolfgang
; Snail Mail Address:   Box 6524, Alexandria, VA 22306-0524
; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;      File:     mult-compile.el                                            ;;
;;      Author:   Wolfgang S Rupprecht <wolfgang@wsrcc.com>                  ;;
;;      Created:  Wed Jan 30 15:51:48 EST 1991                               ;;
;;      Contents: Gnu compile.el with multiple process interface             ;;
;;                                                                           ;;
;;      Copyright (c) 1991 Wolfgang S Rupprecht.                             ;;
;;                                                                           ;;
;;      $Header$                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run compiler as inferior of Emacs, and parse its error messages.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

(provide 'compile)

;; make a more general purpose compilation interface.
;; 1. make compilation-process be buffer-local. (for buffer->process mapping)
;; 2. use the (process-buffer) for process->buffer mapping.
;; 3. make compilation-error-message be a buffer local variable.

;; user preference configurations

(defvar compile-sets-error-buf t
  "*A compile command will set the error buf.")

;; end user configurations

(defvar compilation-process nil
  "Process created by compile command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

;; this isn't used in any meaningful way - kept here for
;; reasons of minimum change.

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.
The value may be t instead of a list;
this means that the buffer of error messages should be reparsed
the next time the list of errors is wanted.")

;; not really used.
(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages parsed.")

;; make this buffer local
(defvar compilation-error-message nil
  "Message to print when no more matches for compilation-error-regexp are found")

;; The filename excludes colons to avoid confusion when error message
;; starts with digits.
(defvar compilation-error-regexp
  "\\([^ :\n]+\\(: *\\|, line \\|(\\)[0-9]+\\)\\|\\([0-9]+ *of *[^ \n]+\\)"
  "Regular expression for filename/linenumber in error in compilation log.")

(defvar last-compilation-buffer "*compilation*")

(defun compile (command &optional buf)

  "NEW COMPILE:Compile the program including the current buffer.
Default: run `make'.  Runs COMMAND, a shell command, in a separate
process asynchronously with output going a buffer (*compilation* by
default).  You can then use the command \\[next-error] to find the
next error message and move to the source code that caused it.

Optional ARG is the buffer or name of the buffer to use for output. If
interactive, C-u will cause command to prompt for filename.  Default
is the last compilation buffer's name."

  (interactive (list (read-string "Compile command: " compile-command)
                     (if current-prefix-arg
                         (read-string "Compilation buffer: "
                                      last-compilation-buffer))))
  (setq compile-command command)
  (if buf
      (setq last-compilation-buffer buf)
    (setq buf last-compilation-buffer))
   (compile1 compile-command "No more errors" nil buf))

; (defun grep (command)
;   "Run grep, with user-specified args, and collect output in a buffer.
; While grep runs asynchronously, you can use the \\[next-error] command
; to find the text that grep hits refer to."
;   (interactive "sRun grep (with args): ")
;   (compile1 (concat "grep -n " command " /dev/null")
;           "No more grep hits" "grep")))

(defun compile1 (command error-message &optional name-of-mode comp-buf)
  "Multi-Buffer version of compile1"
  (save-some-buffers)
  (setq comp-buf (get-buffer-create (or comp-buf "*compilation*")))
  (let ((cwd default-directory)         ; catch that slipery animal
        (regexp compilation-error-regexp)
        (comp-buf-name (buffer-name comp-buf))
	(watch-flag (eq comp-buf (current-buffer))))
    (save-excursion
      (set-buffer comp-buf)
      (setq default-directory cwd)      ; and restore...
      (if compilation-process
          (if (or (not (eq (process-status compilation-process) 'run))
                  (yes-or-no-p
                   "A compilation process is running in this buffer; kill it? "
                   ))
              (condition-case ()
                  (let ((comp-proc compilation-process))
                    (interrupt-process comp-proc)
                    (sit-for 1)
                    (delete-process comp-proc))
                (error nil))
            (error "Cannot have two processes in one buffer!")))

      (if compile-sets-error-buf
	  (setq last-error-buf comp-buf-name))

      (fundamental-mode)
      (buffer-flush-undo comp-buf)

      (setq mode-name (or name-of-mode "Compilation"))
      ;; Make log buffer's mode line show process state
      (setq mode-line-process '(": %s"))

      (make-local-variable 'compilation-process)
      (setq compilation-process nil)

      (make-local-variable 'compilation-error-message)
      (setq compilation-error-message error-message)

      (make-local-variable 'compilation-error-regexp)
      (setq compilation-error-regexp regexp)

      (compilation-forget-errors)
      (setq compilation-error-list t)

      (setq compilation-process
            (start-process "compilation" comp-buf
                           shell-file-name
                           "-c" (concat "exec " command)))
      ;; side effects: erase buffer, pop up buffer in other window 
      (with-output-to-temp-buffer comp-buf-name
        (princ mode-name)
        (princ " started at ")
        (princ (substring (current-time-string) 0 -5))
        (terpri)
        (princ "cd ")
        (princ default-directory)
        (terpri)
        (princ command)
        (terpri))
      (set-process-sentinel compilation-process 'compilation-sentinel))
    (if watch-flag (goto-char (point-max)))))

;; Called when compilation process changes state.

(defun compilation-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         (let* ((obuf (current-buffer))
                omax opoint)
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *compilation* and hack its mode line,
                 (set-buffer (process-buffer proc))
                 (setq omax (point-max) opoint (point))
                 (goto-char (point-max))
                 (insert ?\n mode-name " " msg)
                 (forward-char -1)
                 (insert " at "
                         (substring (current-time-string) 0 -5))
                 (forward-char 1)
                 (setq mode-line-process
                       (concat ": "
                               (symbol-name (process-status proc))))
                 ;; If buffer and mode line will show that the process
                 ;; is dead, we can delete it now.  Otherwise it
                 ;; will stay around until M-x list-processes.
                 (delete-process proc))
             (setq compilation-process nil)
             ;; Force mode line redisplay soon
             (set-buffer-modified-p (buffer-modified-p)))
           (if (and opoint (< opoint omax))
               (goto-char opoint))
           (set-buffer obuf)))))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (if compilation-process
      (interrupt-process compilation-process)
    (error "This buffer doesn't have a compilation process!")))

(defun kill-grep ()
  "Kill the process made by the \\[grep] command."
  (interactive)
  (if compilation-process
      (interrupt-process compilation-process)
    (error "This buffer doesn't have a compilation process!")))

(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  (interactive "P")
  (if (or (eq compilation-error-list t)
          argp)
      (progn (compilation-forget-errors)
             (setq compilation-parsing-end 1)))
  (if compilation-error-list
      nil
    (save-excursion
      (switch-to-buffer "*compilation*")
      (set-buffer-modified-p nil)
      (compilation-parse-errors)))
  (let ((next-error (car compilation-error-list)))
    (if (null next-error)
        (error (concat compilation-error-message
                       (if (and compilation-process
                                (eq (process-status compilation-process)
                                    'run))
                           " yet" ""))))
    (setq compilation-error-list (cdr compilation-error-list))
    (if (null (car (cdr next-error)))
        nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let* ((pop-up-windows t)
           (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)))

;; Set compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun compilation-forget-errors ()
  (if (eq compilation-error-list t)
      (setq compilation-error-list nil))
  (while compilation-error-list
    (let ((next-error (car compilation-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
          (set-marker (car (cdr next-error)) nil)))
    (setq compilation-error-list (cdr compilation-error-list))))

(defun compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in compilation-error-list.
The function next-error, assigned to \\[next-error], takes the next error off the list
and visits its location."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
        last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
        (forward-line 2))
    (while (re-search-forward compilation-error-regexp nil t)
      (let (linenum filename
            error-marker text-marker)
        ;; Extract file name and line number from error message.
        (save-restriction
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-max))
          (skip-chars-backward "[0-9]")
          ;; If it's a lint message, use the last file(linenum) on the line.
          ;; Normally we use the first on the line.
          (if (= (preceding-char) ?\()
              (progn
                (narrow-to-region (point-min) (1+ (buffer-size)))
                (end-of-line)
                (re-search-backward compilation-error-regexp)
                (skip-chars-backward "^ \t\n")
                (narrow-to-region (point) (match-end 0))
                (goto-char (point-max))
                (skip-chars-backward "[0-9]")))
          ;; Are we looking at a "filename-first" or "line-number-first" form?
          (if (looking-at "[0-9]")
              (progn
                (setq linenum (read (current-buffer)))
                (goto-char (point-min)))
            ;; Line number at start, file name at end.
            (progn
              (goto-char (point-min))
              (setq linenum (read (current-buffer)))
              (goto-char (point-max))
              (skip-chars-backward "^ \t\n")))
          (setq filename (compilation-grab-filename)))
        ;; Locate the erring file and line.
        (if (and (equal filename last-filename)
                 (= linenum last-linenum))
            nil
          (beginning-of-line 1)
          (setq error-marker (point-marker))
          ;; text-buffer gets the buffer containing this error's file.
          (if (not (equal filename last-filename))
              (setq text-buffer
                    (and (file-exists-p (setq last-filename filename))
                         (find-file-noselect filename))
                    last-linenum 0))
          (if text-buffer
              ;; Go to that buffer and find the erring line.
              (save-excursion
                (set-buffer text-buffer)
                (if (zerop last-linenum)
                    (progn
                      (goto-char 1)
                      (setq last-linenum 1)))
                (forward-line (- linenum last-linenum))
                (setq last-linenum linenum)
                (setq text-marker (point-marker))
                (setq compilation-error-list
                      (cons (list error-marker text-marker)
                            compilation-error-list)))))
        (forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

(defun compilation-grab-filename ()
  "Return a string which is a filename, starting at point.
Ignore quotes and parentheses around it, as well as trailing colons."
  (if (eq (following-char) ?\")
      (save-restriction
        (narrow-to-region (point)
                          (progn (forward-sexp 1) (point)))
        (goto-char (point-min))
        (read (current-buffer)))
    (buffer-substring (point)
                      (progn
                        (skip-chars-forward "^ :,\n\t(")
                        (point)))))

;; (define-key ctl-x-map "`" 'next-error)
(define-key ctl-x-map "`" 'wsr:next-error)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;      File:     compile.el                                                 ;;
;;      Author:   Wolfgang S. Rupprecht <wolfgang@wsrcc.com>                 ;;
;;      Created:  March 1987                                                 ;;
;;      Contents: I did significant hacking to the error parser              ;;
;;              for next-error.  The parser now has a table that             ;;
;;              it scans for applicable rexexp templates.  If                ;;
;;              one of them fits, it uses that one to parse the              ;;
;;              line. If it doesn't fit, the scanner tries the               ;;
;;              next template. If all templates fail, the line               ;;
;;              is deemed a useless line and discarded.                      ;;
;;                                                                           ;;
;;      Copyright (c) 1989, 1987 Wolfgang Rupprecht.                         ;;
;;                                                                           ;;
;;      $Header$                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is not really part of GNU Emacs, but is (hopefully)
;; useful in conjunction with it.  It is meant as a patch to the
;; distributed GnuEmacs lisp file by the same name.

;; GNU Emacs and this compile.el is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General
;; Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;; First we load the real lib.  The original load-path should be stored
;; in the variable old-load-path.

;(let ((load-path old-load-path))
;  (require 'compile))

;; Now we patch it.

(define-key ctl-x-map "`" 'wsr:next-error)

;; let's hope that nobody is stupid enough to put a colon or
;; parenthesis in their filenames, (these regexps desperately need to
;; cue off of them) -wsr

;; for forced updating of the defvar definitions
;; (defmacro defvar-f (name val doc) (list 'setq name val))

(defvar error-parsing-regexp-list
  ;; rule 0: 4.3bsd grep, cc, lint(part1 warnings)
  ;; /users/wolfgang/foo.c(8): warning: w may be used before set
  '(("^\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2)
    ;; rule 1: 4.3bsd lint part2: inconsistant type warnings
    ;; strcmp: variable # of args.      llib-lc(359)  ::  /users/wolfgang/foo.c(8)
    ;; also sysV lint: from kamat@uceng.uc.edu
    ;;     seekdir      llib-lc(345) :: uuq.c?(73)
    ("^[^\n]*[ \t]+\\([^:( \t\n]+\\)[:(]+[ \t]*\\([0-9]+\\)[:) \t]+\\([^:?( \t\n]+\\)\\??[:(]+[ \t]*\\([0-9]+\\)[:) \t]+$"
     3 4 1 2)

;;;    ("[^\n]*[ \t:]+\\([^:( \t\n]+\\)[ \t]*[:(]+[ \t]*\\([0-9]+\\)[:) \t]*$"     1 2)
    ;; rule 2: 4.3bsd lint part3: defined, but unused messages
    ;; linthemorrhoids defined( /users/wolfgang/foo.c(4) ), but never used
    ;; foo used( file.c(144) ), but not defined
    ("[^\n]*\\(defined\\|used\\)[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+"
     2 3)
    ;; rule 3: 4.3bsd compiler
    ;; "foo.c", line 18: cc_cardiac_arrest undefined
    ("^[\* \t]*[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+of[ \t]+\"\\([^\"\n]+\\)\":" 2 1)
    ;; rule 4: apollo cc warnings, yuk -wsr
    ("^[\* \t]*\"\\([^\"\n]+\\)\",?[ \t]+[Ll]ine[ \t]+\\([0-9]+\\):" 1 2)
    ;; rule 5: as on a sun 3 under sunos 3.4
    ;;(as) "spl.i", line 23:  Error:  syntax error.
    ("^(.+)[ \t]+\"\\([^\"\n]+\\)\",[ \t]+line[ \t]+\\([0-9]+\\):" 1 2)
    ;; rule 6: m88kcc
    ;; "./foo.h" , line 128: redeclaration of bar
    ;; note the extra space before the comma (after filename) : grotty
    ("^\\((.+)[ \t]+\\)?\"\\([^\"\n]+\\)\" ?,[ \t]+line[ \t]+\\([0-9]+\\):"
     2 3)
    ;; rule 7: Make
    ;; Make: line 20: syntax error.  Stop.
    ;; Make: Must be a separator on rules line 84.  Stop.
    ("^[\* \t]*[Mm]ake: [^\n]*[Ll]ine[ \t]+\\([0-9]+\\)[.:]"
     scan-make 1)
    ;; rule 8: /bin/sh 
    ;; filename can only be parsed correctly if it is a full pathname, or
    ;; is relative to this directory.
    ;; ./binshscript: syntax error at line 5: `newline or ;' unexpected
    ("^\\([^:\n]+\\):.*line[ \t]+\\([0-9]+\\):" 1 2)
    ;; rule 9: sysV woes
    ;;     rcmd         cico.c?(243)
    ("^    [^: \t\n]+ +\t\\([^:?( \t\n]+\\)\\??(\\([0-9]+\\))$" 1 2)
    ;; rule 10: sysV lint - "Reach out and confuse someone."
    ;; cico.c
    ;; ==============
    ;; (88)  warning: alias unused in function main
    ;; (656)  warning: main() returns random value to invocation environment
    ;; cntrl.c:
    ;;  
    ;; uucpd.c
    ;; ==============
    ;; warning: argument unused in function:
    ;;     (48)  argc in main
    ;; warning: possible pointer alignment problem
    ;;     (145)            (246)           (329)  
    ;;     (367)        
    ;; note: This regexp has to be incredibally weak.  There just isn't much
    ;; to get a toe-hold on here.  Better keep this one on the end. -wsr
    ("^[ \t]*(\\([0-9]+\\))[ \t]" scan-s5lint 1)
    ;; rule 11: there is no rule 11
    ;; (add other rules and explanations here)
    )
  "a list of lists consisting of:
\((rexexp filename-match-index linenum-match-index)(...)(...))
for parsing error messages")

(defun test-parse (pos)
  "Test the line parsing code, attempts to parse the current line for
filename and line number. Answer is returned in minibuffer."
  (interactive "d")
  (forward-line 0)
  (let (filename linenum filename-2 linenum-2) ; set by compilation-parse-line
    (let ((parserule (compilation-parse-line)))
      (if parserule
          (if filename-2
              (message "Parses as: '%s(%d)' and '%s(%d)' [rule %d]"
                       filename linenum
                       filename-2 linenum-2 parserule)
            (message "Parses as: '%s(%d)' [rule %d]"
                     filename linenum parserule))
        (message "Couldn't parse that line")))))

(defvar compilations-window-height 4 "*Height of compilations buffer window.")

;; parse error msgs, find file or files, and position cursor on the
;; appropriate lines.
;; The "primary" file is always at the bottom of the screen.
;; The *compilations* buffer is always at the top, and reduced to
;;  a smaller height.

(defvar last-error-buf nil
  "The last buffer that next error used.")

(defun wsr:next-error (&optional flag)
  "This is the *new* NEXT ERROR: Visit next compilation error message
and corresponding source code.  This operates on the output from the
\\[compile] command.  This command uses the line after current point
as the starting point of the next error search.

If optional FLAG is set (C-u for interactive), cause the current
buffer to be the new compilation buffer."
  (interactive "P")
  (if flag
      (setq last-error-buf (current-buffer)))
  (pop-to-buffer (or last-error-buf last-compilation-buffer
		     "*compilation*"))
  (let ((opoint (point))
        (pwd default-directory)
        filename linenum filename-2 linenum-2)
    ;; note: compilation-parse-line will set the above 4 variables
    ;; by side effect.
    (while (and (zerop (forward-line 1))
                (null (compilation-parse-line))))
    (if (null filename)
	;; this error will leave on in the compilation buffer.
	;; usually this is of benefit - one can now move the up
	;; point back up to bet back to the last error of interest.
        (error (concat compilation-error-message
                       (if (and compilation-process
                                (eq (process-status compilation-process)
                                    'run))
                           " yet" "")))
      (recenter 0)
      (if (file-exists-p filename)
          (progn
            (delete-other-windows)
            (let ((wh (window-height)))
              (find-file-other-window filename)
              (shrink-window (- compilations-window-height (/ wh 2)))
              )
            (goto-line linenum)
            (recenter (/ (window-height) 2))
            (if filename-2              ; a two file match
                (let ((default-directory pwd)) ; get the pwd right!
                  (if (file-exists-p filename-2)
                      (progn
                        (split-window-vertically nil)
                        (find-file filename-2)
                        (goto-line linenum-2)
                        (recenter (/ (window-height) 2))
                        ;; now back to file # 1
                        (other-window 1)
                        ;; needed if both windows are on the same file
                        (recenter (/ (window-height) 2)))
                    (message "Can't find file '%s(%d)'"
                             filename-2 linenum-2)))))
        ;; try filename-2 ... suggested by kamat
        (if filename-2
            (if (file-exists-p filename-2)
                (progn
                  (message "Can't find file '%s(%d)'" filename linenum)
                  (delete-other-windows)
                  (let ((wh (window-height)))
                    (find-file-other-window filename-2)
                    (shrink-window (- compilations-window-height (/ wh 2))))
                  (goto-line linenum-2)
                  (recenter (/ (window-height) 2)))
              (error "Can't find files '%s(%d)' or '%s(%d)'" 
                     filename linenum filename-2 linenum-2))
          (error "Can't find file '%s(%d)'" filename linenum))))))

(defun compilation-parse-line ()
  "Parse this line, setq-ing filename and linenum."
  (let ((parse-list error-parsing-regexp-list)
        (rule-num 0))
    (while parse-list
      (let ((rule-list (car parse-list)))
        (if (looking-at (car rule-list))
            (let ((file-index (nth 1 rule-list))
                  (line-index (nth 2 rule-list))
                  (file-2-index (nth 3 rule-list))
                  (line-2-index (nth 4 rule-list)))
              (setq linenum (string-to-int
                             (buffer-substring (match-beginning line-index)
                                               (match-end line-index))))
              (if file-2-index
                  (progn
                    (setq filename-2 (buffer-substring
                                      (match-beginning file-2-index)
                                      (match-end file-2-index)))
                    (setq linenum-2 (string-to-int
                                     (buffer-substring
                                      (match-beginning line-2-index)
                                      (match-end line-2-index))))))
              (setq filename
                    (cond ((integerp file-index)
                           (buffer-substring (match-beginning file-index)
                                             (match-end file-index)))
                          ;; careful! this next funcall may mash
                          ;; the match-data, so it must be done
                          ;; after all the line numbers and names have been
                          ;; extracted
                          ((symbolp file-index) (funcall file-index))
                          ((stringp file-index) file-index)
                          (t (error "Parsing error: unknown action type: %s"
                                    file-index))))
              (setq parse-list nil))    ;we're done
          (setq parse-list (cdr parse-list)
                rule-num (1+ rule-num)))))
    (and linenum filename rule-num)))   ; return matching rule number

(defun scan-make ()
  "Attempt to find the name of the Makefile used by this make run.
This routine shouln't be used for anything drastic, since it just isn't
that robust."
  (cond ((save-excursion
           (re-search-backward "make[^\n]+-f[ \t]+\\(\\sw\\|\\s_\\)+" nil t))
         (buffer-substring (match-beginning 1)(match-end 1)))
        ((file-exists-p "makefile") "makefile")
        ((file-exists-p "Makefile") "Makefile")
        (t nil)
      ))

(defun scan-s5lint ()
  "Attempt to find the name of the file that lint was griping about on
this line.  This routine also has the side-effect of modifying the current
buffer.  The current line will have the first gripe of a multi-gripe line 
broken off onto a separate line."
  (let (retval)
    (if (save-excursion
          (re-search-backward "^\\(\\sw\\|\\s_\\|\\s.\\)+\n======+$" nil t))
        (progn
          (setq retval (buffer-substring (match-beginning 1)(match-end 1)))
          (save-excursion
            (if (re-search-forward ")[ \t]*("
                                   (save-excursion (end-of-line) (point)) t)
                (replace-match ")\n(")))))
  retval))


-- 
Wolfgang Rupprecht    wolfgang@wsrcc.com (or) uunet!wsrcc!wolfgang
Snail Mail Address:   Box 6524, Alexandria, VA 22306-0524
