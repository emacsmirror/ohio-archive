;$modified: Fri Apr  9 19:10:05 1993 by rshouman $
(provide 'rs-compile)
(provide 'compile)

;; LCD Archive Entry:
;; rs-compile|Radey Shouman|rshouman@chpc.utexas.edu|
;; More convenient compiler interface; replaces compile.el.|
;; 09-Apr-1993|1.0|~/misc/rs-compile.el.Z|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rs-compile--Run possibly remote compiler process.  Parse its error messages
;;             using an extensible table.  Find selected error locations, 
;;             searching for files in tags table and/or search path.
;; author:  Radey Shouman         rshouman@chpc.utexas.edu
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of GNU emacs, but it is based in part on the 18.58
;; distribution compile.el, and is released on the same terms as GNU emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This package is largely based on on mult-compile.el, by Wolfgang Rupprecht.
;; the following is taken from the header to that package:
;;           -------------------------------------------
;; Make a more general purpose compilation interface.
;;
;; 1. Make compilation-process be buffer-local. (for buffer->process mapping)
;; 2. Use the (process-buffer) for process->buffer mapping.
;; 3. Make compilation-error-message be a buffer local variable.
;;           --------------------------------------------
;;
;; The mult-compile list of regexps and error message parser survive
;; more or less intact here, as does the idea of following the
;; error message point is on in the compilation buffer.  However I've
;; included a few more features to make it more general, and potentially
;; faster:
;;
;; 4.  Follow the error message on the line point is in, in the 
;;     compilation buffer, so you can pick and choose which errors
;;     you want to see.  (mult-compile uses the line after point.)
;; 5.  Use markers to save locations in target files, so editing a file
;;     doesn't throw later error locations off. 
;;     Only visit and mark files when they are actually needed, instead of 
;;     visiting them all at once, as the original compile.el did.
;; 6.  If a file is not found in the current directory, look in the 
;;     tags-table for it, if that doesn't work, search through the
;;     directories in the local variable compilation-vpath, This approach
;;     doesn't require grubbing around in make output for pwd's and can 
;;     work with any make.
;; 7.  Make the compilation buffer scroll in its window, whether point is
;;     there or not.  If the compilation buffer window isn't visible when
;;     its process exits, pop it up.
;; 8.  If the default-directory is an ange-ftp style remote directory,
;;     use rsh to run the compilation process remotely.
;; 9.  Provide a symbolic name for each rule, and a compile-hook so that
;;     rules can be selected on the fly, minimizing unnecessary regexp
;;     searches -- no need to look for 3 kinds of lint error messages in
;;     a grep buffer.  Provide an interactive command to add rules by name
;;     in the compilation buffer.
;; 10. Provide next-error-find-file-hook so that file opening by next-error
;;     may be customized.
;; 11. Add a keymap for the compilation buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Installation:
;;    byte-compile rs-compile.el and put it somewhere in your load path,
;;    in your .emacs, use:
;;
;;(autoload (fmakunbound 'compile) "rs-compile" 
;;	  "Run compiler as inferior process" t)
;;(autoload (fmakunbound 'grep) "rs-compile" "Run grep as inferior process" t)
;;(autoload 'find-grep "rs-compile"
;;	  "Execute grep with find command as inferior process" t)
;;
;;     This is meant as a drop-in replacement for compile.el, except for
;;     the behavior of next-error with a prefix arg, the new next-error
;;     goes to the first error message in a different file from the one
;;     pointed to in the compilation buffer.  It should not be necessary
;;     to force reparsing of the compilation buffer, which is what the
;;     old next-error did when given a prefix arg.  If you really want to
;;     force reparsing, you can call compilation-forget-errors interactively.
;;
;;;;;;;;;;;;;; user preference configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In order to use only a subset of the regexp rules available in the list
;; call the function compilation-regexp-list with a symbol in the argument
;; list for each rule you want to use.
;;       for example:
;;(setq compilation-regexp-list
;;      (compilation-regexp-list 'rule0 'grep 'cray 'convex 'bsdcc 'sh 'make))
;;
;; If you do this somewhere other than a compilation buffer, the effect will
;; be global.
;;
;; compile-hook is run whenever a new compilation process is started, so you
;; can customize local variables according to the process.  Whenever a file
;; is visited by next-error, it temporarily appends the functions in 
;; next-error-find-file-hook to those already in find-file-hook.

;; You could use different patterns on different remote hosts this way,
;;      for example:
;;(defun compilation-by-host-hook ()
;;  "Map compilation-regexp-list to remote hostnames. "
;;  (let ((host (nth 0 (ange-ftp-ftp-path default-directory))))
;;    (cond ((its-a-cray-p host)
;;	   (compilation-regexp-list 'cray))
;;	  ((its-a-convex-p host)
;;	   (compilation-regexp-list 'convex))
;;	  (t ))))
;;
;;  To control window configuration and display, see the variables:
;;  compilation-window-height, compilation-context-lines,
;;  compilation-scroll-window, and compilation-select-window.
;;
;;  Variables controlling how error files are found are:
;;  compilation-use-tags-table, and compilation-vpath.
;;
;;  Normally, error messages will be parsed by the compilation filter
;;  as they arrive, if this causes too many delays for other editing,
;;  this behavior can be disabled by setting the variable
;;  compilation-background-parse to nil.
;;
;;  If next-error cannot parse error messages from a compiler, it should
;;  probably be sufficient to change compilation-regexp-list, as long as
;;  each error message has the file name and line number both on one line
;;  and can be recognized by a regexp.
;;
;;  CHANGES:
;;   9-4-93 Applied patch from Rod Whitby <rwhitby@research.canon.oz.au>
;;          to fix test-parse, changed name to compilation-test-parse.
;;          Added find-grep, miscellaneous documentation.
;;   6-4-93 Fixed compilation-window-height so nil values worked correctly,
;;          suggestion from Curtis Bingham <bingh@sibelius.cs.byu.edu>
;;   21-3-93 Added compilation-select-window, fixed compile so that a
;;           buffer local compile-command would work properly, suggestions by
;;           Harald Fuchs <hf@telematik.informatik.uni-karlsruhe.de>



(defvar compile-command "make -k"
  "*Default command for compilation, this variable is local in compilation
buffers, if no local value is defined, the global value will be used, the
global value is changed by each new compilation.

If the default directory of the compilation buffer is an ange-ftp style 
pathname, then the compile-command will be run on the indicate remote host
using compilation-remote-shell-file-name, (normally rsh).  If compile-command
is of the form user@host:command or host:command, then the command will
liekwise be run on `host'. ")

(defvar compilation-use-tags-table 'maybe
  "*If t, next-error will search the current tags table for any files not
found in the default directory.  If not nil and not t, next-error will ask
once in each compilation buffer whether to use a tags table when a file is
not found. ")

(defvar compilation-vpath nil
  "*If a list, list of directories next-error will search for any
files not found in the default directory; if a string, a directory to
search.  This variable is buffer local in compilation buffers.

If compilation-use-tags is non-nil, next-error will search
the tags table first. ")

(defvar compilation-window-height 8 
  "*Height of compilation buffer window.  If nil, don't change window sizes. ")

(defvar compilation-context-lines 4
  "*Number of lines of continuity when scrolling compilation window with
next-error. ")

(defvar compilation-scroll-window t
  "*If non-nil, scroll the compilation buffer window as new output arrives. ")

(defvar compilation-select-window t
  "*If non-nil, starting the compilation process will select the compilation
window, otherwise, it will display the window, but not select it. ")

(defvar compilation-sets-error-buf t
  "*A compile command will set the error buf.")

(defvar grep-command "egrep -n "
  "*The default grep command.")

(defvar find-grep-format "find . -type f %s -exec egrep -n %s {} /dev/null \\;"
  "*Format for producing the find-grep command. ")

(defvar compile-hook nil
  "*Function or list of functions to be called before starting a compilation
process with \\[compile].  This might be a good place to set
compilation-regexp-list or compilation-vpath. ")

(defvar next-error-find-file-hook nil
  "*Function or list of functions to be added to the end of find-file-hooks
when finding a file with \\[next-error]. ")

(defvar compilation-expert nil
  "*If non-nil, don't print fancy error messages. ")

(defvar compilation-background-parse t
  "*If non-nil, parse error messages while they are being received, this
may cause slow response if you are editing another buffer at the same time. ")

(defvar compilation-mode-map nil
  "Local keymap for compilation buffer. ")
(if compilation-mode-map
    nil
  (setq compilation-mode-map (make-sparse-keymap))
  (define-key compilation-mode-map "\C-c\C-b" 'rename-buffer)
  (define-key compilation-mode-map "\C-c\C-c" 'compile-again)
  (define-key compilation-mode-map "\C-c\C-d" 'compilation-cd)
  (define-key compilation-mode-map "\C-c\C-f" 'this-error)
  (define-key compilation-mode-map "\C-c\C-k" 'kill-compilation)
  (define-key compilation-mode-map "\C-c\C-n" 'next-error-next-line)
  (define-key compilation-mode-map "\C-c\C-p" 'next-error-previous-line)
  (define-key compilation-mode-map "\C-c\C-r" 'compilation-add-rule)
  (define-key compilation-mode-map "\C-c\C-t" 'compilation-use-tags-table)
  (define-key compilation-mode-map "\C-c\C-v" 'compilation-add-vpath)
  (define-key compilation-mode-map "\C-^" 'compilation-enlarge-window)
  (define-key ctl-x-map "`" 'next-error)
  (define-key ctl-x-map "~" 'next-error-backward))

;; let's hope that nobody is stupid enough to put a colon or
;; parenthesis in their filenames, (these regexps desperately need to
;; cue off of them) -wsr

(defvar compilation-regexp-list
'(
  ;; rule 0: if it doesn't have any numbers, it isn't an error message.
  (rule0
   "\\[^0-9]*$" nil nil)

  ;; rule 1: 4.3bsd grep, cc, lint(part1 warnings)
  ;; /users/wolfgang/foo.c(8): warning: w may be used before set
  (grep
   "\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2)

  ;; added rule 2 for CRAY compiler messages  --ars
  (cray
   "[^\n:]*:[^\n]*File = \\([^,]+\\), *Line = \\([0-9]+\\)" 1 2)

  ;; added rule 3 for ConvexOS compiler messages --ars
  (convex
   "[^\n:]*:[^\n]*on line \\([0-9]+\\)[.0-9]* of \\([^:\n]+\\):" 2 1)

  ;; Thanks to Richard Everson <rme@cfm.brown.edu>
  ;; sgi C compiler.
  (sgi
   "[^\n:]*: [^\n:]*: \\([^,\n]+\\), line \\([0-9]+\\):.*" 1 2 )

  ;; rule 4: 4.3bsd lint part2: inconsistant type warnings
  ;; strcmp: variable # of args.     llib-lc(359)  ::  /users/wolfgang/foo.c(8)
  ;; also sysV lint: from kamat@uceng.uc.edu
  ;;     seekdir      llib-lc(345) :: uuq.c?(73)
  (lint2
   "[^\n]*[ \t]+\\([^:( \t\n]+\\)[:(]+[ \t]*\\([0-9]+\\)[:) \t]+\\([^:?( \t\n]+\\)\\??[:(]+[ \t]*\\([0-9]+\\)[:) \t]+$"
   3 4 1 2)

  ;; rule 5: 4.3bsd lint part3: defined, but unused messages
  ;; linthemorrhoids defined( /users/wolfgang/foo.c(4) ), but never used
  ;; foo used( file.c(144) ), but not defined
  (lint3
   "[^\n]*\\(defined\\|used\\)[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+"
   2 3)

  ;; rule 6: 4.3bsd compiler
  ;; "foo.c", line 18: cc_cardiac_arrest undefined
  (bsdcc
   "[\* \t]*[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+of[ \t]+\"\\([^\"\n]+\\)\":"
   2 1)

  ;; rule 7: apollo cc warnings, yuk -wsr
  (apollo
   "[\* \t]*\"\\([^\"\n]+\\)\",?[ \t]+[Ll]ine[ \t]+\\([0-9]+\\):"
   1 2)

  ;; rule 8: as on a sun 3 under sunos 3.4
  ;;(as) "spl.i", line 23:  Error:  syntax error.
  (sun3
   "(.+)[ \t]+\"\\([^\"\n]+\\)\",[ \t]+line[ \t]+\\([0-9]+\\):"
   1 2)

  ;; rule 9: m88kcc
  ;; "./foo.h" , line 128: redeclaration of bar
  ;; note the extra space before the comma (after filename) : grotty
  (m88kcc
   "\\((.+)[ \t]+\\)?\"\\([^\"\n]+\\)\" ?,[ \t]+line[ \t]+\\([0-9]+\\):"
   2 3)

  ;; rule 10: Make
  ;; Make: line 20: syntax error.  Stop.
  ;; Make: Must be a separator on rules line 84.  Stop.
  (make 
   "[\* \t]*[Mm]ake: [^\n]*[Ll]ine[ \t]+\\([0-9]+\\)[.:]"
   scan-make 1)

  ;; rule 11: /bin/sh 
  ;; filename can only be parsed correctly if it is a full pathname, or
  ;; is relative to this directory.
  ;; ./binshscript: syntax error at line 5: `newline or ;' unexpected
  (sh
   "\\([^:\n]+\\):.*line[ \t]+\\([0-9]+\\):"
   1 2)

  ;; rule 12: sysV woes
  ;;     rcmd         cico.c?(243)
  (sysv
   "     [^: \t\n]+ +\t\\([^:?( \t\n]+\\)\\??(\\([0-9]+\\))$"
   1 2)

  ;; rule 13: AIX
  (aix
   "\"\\([^\"]+\\)\", line \\([0-9]+\\)\\.[0-9]+:" 
   1 2)

  ;; rule 14: sysV lint - "Reach out and confuse someone."
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
  ;; note: This regexp has to be incredibly weak.  There just isn't much
  ;; to get a toe-hold on here.  Better keep this one on the end. -wsr
  (lintv
   "[ \t]*(\\([0-9]+\\))[ \t]" scan-s5lint 1)

  ;; rule 15: there is no rule 15
  ;; \(add other rules and explanations here\)
  )
  "List of rules for parsing compilation buffer error messages.  Each
rule is a list:
\(symbol regexp filename-index-1 linenum-index-1 
                 filename-index-2 linenum-index-2\)
symbol is the name of the rule, used in making the local variable
compilation-regexp-list, regexp is a regular expression matching
the error message, filename-index-1 is the subexpression of the regexp
matching the first filename mentioned, and similarly for filename-index-2,
and the linenum indexes. ")

(defvar compilation-remote-shell-file-name "rsh"
  "The name of the program used to run a program on a remote machine,
should be the same as ange-ftp-remote-shell-file-name if you are using
ange-ftp.")

;;;;;;;;;;;;;;;;;;;;;;;; end user configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar compilation-process nil
  "Process created by compile command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar compilation-error-type nil
  "*What to call error messages when we report how many there are. This
is local in compilation buffers. ")

(defvar compilation-regexp-list nil
  "A subset of compilation-master-regexp-list, but without symbols in car
of each sublist, used by next-error for parsing error-messages.  This is
be local in compilation buffers.  See compilation-master-regexp-list. ")

(defvar compile-last-compilation-buffer "*compilation*"
  "Last buffer used for compilation. ")

(defvar compilation-last-error-buf nil
  "Last compilation buffer from which the \\[next-error] command was 
executed. ")

(defvar compilation-last-command nil
  "Last compile command used in this buffer. ")

(defvar compilation-parsing-end-marker nil
  "Marks the end of the part of the compilation buffer that has been parsed.")

(defvar compilation-error-list  nil
  "Alist of error location alists for each file, the car is a file name, the
cdr is an alist, the car of which is a line number and the cdr is a marker if
the file has been visited, or nil if it has not.

((filename1 (num1 . mark1) (num2 . mark2)) (filename2 (num1 . mark1)) ... )")

;; This function is here to make it easy to drop rules before parsing the 
;; buffer, if it is clear that some of them aren't going to be necessary.
(defun compilation-regexp-list (&rest args)
  "For each ARG, define a rule in the local compilation-regexp-list.
If ARG is a symbol, it is the car of a rule in the global 
compilation-regexp-list.
If it is a list, copy it into the local list, each list should look like:

\(symbol regexp filename-match-index-1 linenum-match-index-1 
	filename-match-index-2 linenum-match-index-2\)

symbol is the name of the rule, for a local rule, it can be anything,
regexp is a regular expression matching the error message, filename-index-1
is the subexpression of the regexp matching the first filename mentioned, 
and similarly for filename-index-2, and the linenum indexes. 

With no args, or nil, use all the rules in the global compilation-regexp-list."
  (setq compilation-regexp-list (default-value 'compilation-regexp-list))
  (if (null args)
      compilation-regexp-list
    (setq compilation-regexp-list
	  (mapcar '(lambda (arg) 
		     (cond ((symbolp arg)
			    (or 
			     (assq arg compilation-regexp-list)
			     (error "rule %s not in compilation-regexp-list"
				    arg)))
			   ((listp arg)
			    (if (stringp (car (cdr arg)))
				arg
			      (error "%s is not a regexp" (car (cdr arg)))))
			   (t nil)))
		  args))))

(defun compilation-add-rule (rule &optional last interactive)
  "Add a parsing rule from the global definition of compilation-regexp-list
to the local definition, the rule will be the one given by the list whose
car is the symbol RULE.  With optional LAST or interactive prefix arg, put
rule at the end of the current list, instead of the beginning. "
  (interactive
   (let (alist)
     (mapcar '(lambda (arg) 
		(or (assq (car arg) compilation-regexp-list)
		    (setq alist
			  (cons (list (symbol-name (car arg))) alist))))
	     (default-value 'compilation-regexp-list))
     (list
      (if alist
	  (intern (completing-read "rule? " alist nil t))
	(error "All rules already used."))
      current-prefix-arg t)))

  (setq compilation-regexp-list
	(if last
	    (append compilation-regexp-list
		    (list
		     (assq rule (default-value 'compilation-regexp-list))))
	  (cons (assq rule (default-value 'compilation-regexp-list))
		compilation-regexp-list)))
  (if interactive
      (message "rule list: %s" (mapcar 'car compilation-regexp-list))))

(defun compile (command &optional buf)
  "Run COMMAND asynchronously, putting output in the buffer named BUF 
(default *compilation*).  When invoked interactively, command will be
prompted for (default is \"make -k\"); if invoked with prefix arg, the
buffer name is prompted for as well.

\\<compilation-mode-map>\\
The output from COMMAND is scanned for error messages from compilers;
you can then use the command \\[next-error] to jump to the location in 
a source file indicated by an first error message.  The error message will
be the first one on or after the line point is in in the compilation
buffer, so you can choose which messages you want to follow.

Commands:

These are available anywhere, and use the last active compilation buffer:
  \\[next-error] finds the error indicated by point in the compilation buffer
         with prefix arg, finds an error in another file.

  \\[next-error-backward\\] finds the first error before the one that \\[next-error\\]
         would jump to.

These are available only in the compilation buffer:

  \\[this-error] finds the source code corresponding to the error message
         on the line point is in.

  \\[next-error-next-line] skips to the next error message, but doesn't find the 
         file, with prefix arg, skips to an error message about another file.

  \\[next-error-previous-line] skips to the previous error message, with prefix
         arg, the previous file.

  \\[compilation-add-vpath]  adds a directory to the beginning of the search 
         path for error files, with prefix arg adds it to the end or the path.

  \\[compilation-use-tags-table]  tells next-error to use the current tags table
         to find files, prompting for it if necessary.

  \\[rename-buffer]  renames the current buffer.

  \\[compilation-cd]  changes the default directory. 

  \\[compilation-add-rule]  adds an error message parsing rule from the global
         list to the local one. "
  (interactive (let ((dir default-directory)
		     (buffer-command (if (string= compile-command 
					     (default-value 'compile-command))
					 nil
				       compile-command))
		     command buf)
		 (setq buf
		       (if current-prefix-arg
			   (read-buffer "Compilation buffer: "
					compile-last-compilation-buffer)
			 ;; Use current buffer if it is a compilation buffer
			 (if (eq major-mode 'compile)
			     (buffer-name (current-buffer))
			   compile-last-compilation-buffer)))
		 (set-buffer (get-buffer-create buf))
		 (setq default-directory dir)
		 (setq command
		       (let ((host 
			      (and (fboundp 'ange-ftp-ftp-path)
				   (nth 0 (ange-ftp-ftp-path 
					   default-directory)))))
			 (read-string 
			  (concat (if host (concat "(" host ") "))
				  "Compile command: ")
			  (or buffer-command
			      compilation-last-command
			      compile-command))))
		 (list command buf)))

  (setq-default compilation-last-command command)
  (setq compile-last-compilation-buffer buf)
  (compile1 command "error" nil buf 'compile))

(defun grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive 
   (let ((dir default-directory))
     (set-buffer (get-buffer-create "*grep*"))
     (setq default-directory dir)
     (let ((host (and (fboundp 'ange-ftp-ftp-path)
		      (nth 0 (ange-ftp-ftp-path default-directory))))
	   cmd)
       (list (read-string 
	      (concat (if host (concat "(" host ") ")) "Grep command: ")
	      grep-command)))))
  (setq grep-command command)
  (compile1 (concat command " /dev/null")
	    "grep hit" "grep" "*grep*" 'grep '(grep)))

(defun find-grep (command)
  "Run find, executing egrep, with a user-specified expression, and collect
output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive 
   (let ((dir default-directory))
     (set-buffer (get-buffer-create "*grep*"))
     (setq default-directory dir)
     (let ((find-args (read-string "find arguments: "))
	   (grep-args (read-string "Grep arguments: ")))
       (list (format find-grep-format find-args grep-args)))))
  (compile1 command "grep hit" "find-grep" "*find-grep*" 'find-grep '(grep)))

(defun compile-again ()
  "Runs compile or grep again from a compilation buffer. "
  (interactive)
  (call-interactively
   (if compilation-revert-function
       compilation-revert-function
     'compile)))

;; error-type is a string describing the errors, or grep hits, or whatever.
;; revert-function is the function to call when compile-again is executed.
(defun compile1 (command error-type &optional name-of-mode comp-buf 
			 revert-function regexp-list)
  "All-singing, all-dancing, multi-buffer, ange-ftpified, ever-scrolling,
smart-file-finding, customizable version of compile1.  It slices, it dices,
it crawls on its belly like a reptile and rubs up against you leg ...
but wait, there's MORE ... "
  (save-some-buffers)
  (setq comp-buf (get-buffer-create (or comp-buf "*compilation*")))
  (let ((cwd default-directory)         ; catch that slippery animal
        (comp-buf-name (buffer-name comp-buf)))
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

    (if compilation-sets-error-buf
	(setq compilation-last-error-buf comp-buf-name))
    (fundamental-mode)
    (use-local-map compilation-mode-map)
    (buffer-flush-undo comp-buf)
    (setq mode-name (or name-of-mode "Compilation"))
    (setq major-mode 'compile)

    ;; Make log buffer's mode line show process state
    (setq mode-line-process '(": %s"))

    (make-local-variable 'compilation-regexp-list)
    (make-local-variable 'compilation-last-command)
    (make-local-variable 'compilation-process)
    (make-local-variable 'compilation-error-type)
    (make-local-variable 'compilation-parsing-end-marker)
    (make-local-variable 'compilation-error-list)
    (make-local-variable 'compilation-use-tags-table)
    (make-local-variable 'compilation-vpath)
    (make-local-variable 'next-error-find-file-hook)
    (make-local-variable 'compilation-revert-function)
    (setq compilation-last-command command
	  compilation-error-type error-type
	  compilation-parsing-end-marker (make-marker)
	  compilation-revert-function (symbol-function revert-function)
	  compilation-regexp-list (if regexp-list
				      (apply 'compilation-regexp-list
					     regexp-list)
				    (default-value 'compilation-regexp-list)))

    (run-hooks 'compile-hook)
    (compilation-forget-errors)
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
    (if compilation-select-window
	(pop-to-buffer (current-buffer) nil)
      (set-window-point (get-buffer-window (current-buffer))
			(point-max)))

    ;; I've altered this bit to run remote processes if that seems to 
    ;; be necessary.  --ars
    (let ((arglst (or (compilation-rsh-arglst command t)
		      (list shell-file-name "-c" command))))
      (setq compilation-process
	    (apply 'start-process "compilation" comp-buf
		       (car arglst) (cdr arglst))))
    (set-process-sentinel compilation-process 'compilation-sentinel)
    (set-process-filter compilation-process 'compilation-filter)
    (and compilation-scroll-window (goto-char (point-max)))))

(defun compilation-filter (process string)
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (let* ((p-mark (process-mark process))
	     (win (get-buffer-window (current-buffer)))
	     (move-point (and compilation-scroll-window
			      win
			      (equal 
			       (window-point win) (marker-position p-mark))))
	     (buffer-read-only nil))
	(or (marker-position p-mark)
	    (set-marker p-mark (point-max)))
	(goto-char (marker-position p-mark))
	(insert-before-markers string)
	(if compilation-background-parse
	    (compilation-parse-buffer))
	(and move-point
	     (goto-char (marker-position p-mark))
	     (set-window-point (get-buffer-window (current-buffer)) (point)))))
    ))

;; Called when compilation process changes state.
(defun compilation-sentinel (process message)
  (let ((buf (process-buffer process))
	move-point)
    (save-excursion
      (set-buffer buf)
      (let ((buffer-read-only nil))
	;; If point is at the end of the buffer, we'll keep it there and
	;; scroll the buffer.
	(setq move-point (and (get-buffer-window buf)
			      (eobp)))
	(save-excursion
	  (cond ((null (buffer-name buf))
	       ;; buffer killed
		 (set-process-buffer process nil))
		((memq (process-status process) '(signal exit))
		 ;; Write something in *compilation* and hack its mode line,
		 (goto-char (point-max))
		 (or (= (process-exit-status process) 0)
		     (setq message "failed "))
		 (insert ?\n mode-name " " message)
		 (forward-char -1)
		 (insert " at "
			 (substring (current-time-string) 0 -5))
		 (setq mode-line-process
		       (concat ": "
			       (symbol-name (process-status process))))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process process)
		 (setq compilation-process nil)
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if compilation-background-parse
		     ;; Parse error messages in buffer, and report.
		     (let ((data (match-data))
			   (n 0))
		       (unwind-protect 
			   (compilation-parse-buffer)
			 (store-match-data data))
		       (mapcar '(lambda (arg)
				  (setq n (+ n (length (cdr arg)))))
			       compilation-error-list)
		       (cond ((= n 0)
			      (insert "  no " 
				      compilation-error-type "s found."))
			     ((= n 1)
			      (insert "  one "
				      compilation-error-type " found."))
			     (t 
			      (insert (format "  %d %ss found." n
					      compilation-error-type)))))))))))
    (let ((comp-win (get-buffer-window buf)))
      (if comp-win
	  (if move-point
	      (if (eq comp-win (selected-window))
		  (goto-char (point-max))
		(save-excursion
		  (set-buffer buf)
		  (set-window-point comp-win (point-max)))))
	(let* ((win (get-largest-window))
	       (height (- (window-height win)
			  (if (numberp compilation-window-height)
			      compilation-window-height
			    5))))
	  (split-window win (if (> height 2) height nil)))
	(display-buffer buf)
	(set-window-point (get-buffer-window buf) (point-max)))
      (message "compilation in buffer %s finished" (buffer-name buf)))))

(defun compilation-cd (dir)
  "Set the default directory to DIR for the current buffer. "
  (interactive (list (expand-file-name
		      (compilation-read-dir-name "Directory? " 
						 default-directory
						 default-directory t))))
  (setq default-directory dir)
  (message "default directory now %s ." default-directory))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command. "
  (interactive)
  (if compilation-process
      (if (eq last-command 'kill-compilation)
	  (kill-process compilation-process)
	(interrupt-process compilation-process))
    (error "This buffer doesn't have a compilation process!")))

;; Set compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
(defun compilation-forget-errors ()
  (interactive)
  (set-marker compilation-parsing-end-marker (point-min))
  (while compilation-error-list
    (let ((file-error-list (car compilation-error-list)))
      (setq file-error-list (cdr file-error-list))
      (while file-error-list
	(set-marker (cdr (car file-error-list)) nil)
	(setq file-error-list (cdr file-error-list))))
    (setq compilation-error-list (cdr compilation-error-list))))
      
;; Set all markers in file-error-list to point at the appropriate lines.
(defun compilation-set-markers (file-error-list)
  (let (linenumber this-marker)
    (while (setq file-error-list (cdr file-error-list))
      (setq linenumber (car (car file-error-list)))
      (setq this-marker (cdr (car file-error-list)))
      (goto-line linenumber)
      (set-marker this-marker (point)))))
      
;; Find all error messages in this buffer after compilation-parsing-end-marker,
;; and build  compilation-error-list with the filenames and line numbers,
;; but do not visit any of the files, we don't know whether the user
;; wants to see them or not.
(defun compilation-parse-buffer ()
  (save-excursion
    (goto-char (marker-position compilation-parsing-end-marker))
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(progn
	  (setq compilation-error-list nil)
	  (forward-line 2)
	  (set-marker compilation-parsing-end-marker (point))))
    (let ((n-errors 0)
	  filename linenumber errinfo file-error-list)
      (while (looking-at ".*\n")
	(setq errinfo (compilation-parse-line))
	(if errinfo
	    (progn
	      (setq n-errors (1+ n-errors))
	      (setq filename (nth 0 errinfo))
	      (setq linenumber (nth 1 errinfo))
	      (setq file-error-list
		    (or (assoc filename compilation-error-list)
			(progn
			  (setq compilation-error-list
				(cons (list filename) compilation-error-list))
			  (car compilation-error-list))))
	      
	      (if (assoc linenumber file-error-list)
		  nil
		(setcdr file-error-list
			(cons (cons linenumber (make-marker)) 
			      (cdr file-error-list))))))
	(set-marker compilation-parsing-end-marker (point))
	(beginning-of-line 2))
      n-errors)))

;; Parse current line for error messages, return a list like:
;; (filename linenumber filename-2 linenumber-2)
(defun compilation-parse-line ()
  (let ((parse-list compilation-regexp-list)
	filename linenum filename-2 linenum-2 rule)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "[ ]")
      (while parse-list
	(let ((rule-list (car parse-list)))
	  (if (looking-at (car (cdr rule-list)))
	      (let ((file-index (nth 2 rule-list))
		    (line-index (nth 3 rule-list))
		    (file-2-index (nth 4 rule-list))
		    (line-2-index (nth 5 rule-list)))
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
		(setq rule (car rule-list))
		(setq parse-list nil))    ;we're done
	    (setq parse-list (cdr parse-list)))))
      (and filename linenum
	   (list filename linenum filename-2 linenum-2 rule)))))

(defun compilation-test-parse (pos)
  "Test the line parsing code, attempts to parse the current line for
filename and line number. Answer is returned in minibuffer."
  (interactive "d")
  (forward-line 0)
  (let (filename linenum filename-2 linenum-2 rule)
    ;; (filename linenum filename-2 linenum-2 rule) is returned by
    ;; compilation-parse-line
    (let ((parselist (compilation-parse-line)))
      (if parselist
          (if (nth 2 parselist)
              (message "Parses as: '%s(%d)' and '%s(%d)' [rule %s]"
                       (nth 0 parselist) (nth 1 parselist)
                       (nth 2 parselist) (nth 3 parselist)
		       (princ (nth 4 parselist)))
            (message "Parses as: '%s(%d)' [rule %s]"
                     (nth 0 parselist) (nth 1 parselist)
		     (princ (nth 4 parselist))))
	(message "Couldn't parse that line")))))

(defun compilation-use-tags-table ()
  "*Set the variable compilation-use-tags-table to t, so that next-error
will use a tags table.  If tags-file-name is nil, then set it.  If the 
function visit-tags-table-locally is bound, use it, otherwise call
visit-tags-table.  (harris-tags.el defines visit-tags-table-locally.) "
  (interactive)
  (setq compilation-use-tags-table t)
  (or tags-file-name
      (call-interactively (if (fboundp 'visit-tags-table-locally)
			      'visit-tags-table-locally
			    'visit-tags-table))))

(defun compilation-add-vpath (dir &optional last interactive)
  "*Add DIR to the front of vpath used by next-error, if in a compilation 
buffer, this will be buffer local, otherwise it will be global.  With
optional LAST or interactive prefix argument add dir to end of vpath.
Return dir. "
  (interactive
   (list (expand-file-name
	  (compilation-read-dir-name "Add directory: "
				     default-directory
				     default-directory t))
	 current-prefix-arg t))

  (if (member dir compilation-vpath)
      (if (y-or-n-p (format "%s already in vpath, continue? " dir))
	  (setq dir (compilation-read-dir-name "Directory? "
					       default-directory
					       dir t))
	(error)))
  (setq compilation-vpath
	(cond ((listp compilation-vpath)
	       (if last (append compilation-vpath (list dir))
		 (cons dir compilation-vpath)))
	      ((stringp compilation-vpath)
	       (list dir compilation-vpath))
	      (t dir)))
  (if interactive 
      (message "compilation-vpath: %s" compilation-vpath))
  dir)

(defun compilation-enlarge-window (&optional arg)
  "Enlarge the compilation buffer window, remembering its size in a local
variable. "
  (interactive "p")
  (let ((change (and (numberp compilation-window-height)
		     (= (window-height) compilation-window-height))))
  (enlarge-window (or arg 1))
  (if change
      (setq compilation-window-height (window-height)))))

(defun next-error-next-line (&optional next-file)
  "In a compilation buffer, go to the first line after the one point
is in that contains an error message, and move forward one line.
Returns a list containing the filename and linenumber according to the
error message. "
  (interactive "P")
  (next-error-line 1 next-file))

(defun next-error-previous-line (&optional next-file)
  "In a compilation buffer, go to the first line before the one point
is in that contains an error message, and move backward one more line.
Returns a list containing the filename and linenumber according to the
error message. "
  (interactive "P")
  (next-error-line -1 next-file))

;; If we're looking at an error message, skip forward until we find an
;; error message with different filename and linenumber, otherwise search
;; forward until we find any error message.  If we're at bob or eob, wrap.
(defun next-error-line (skip &optional next-file)
  (let ((opoint (point))
	oldinfo errinfo)
    (save-excursion
      (setq oldinfo (compilation-parse-line))
      (cond ((and (eobp) (> skip 0))
	     (goto-char (point-min)))
	    ((and (bobp) (< skip 0))
	     (goto-char (point-max))
	     (beginning-of-line))
	    (t nil))
      (while (and (or (null (setq errinfo (compilation-parse-line)))
		      (if next-file
			  (and (equal (nth 0 errinfo) (nth 0 oldinfo))
			       (equal (nth 2 errinfo) (nth 2 oldinfo)))
			(equal errinfo oldinfo)))
		  (zerop (forward-line skip)))
	(setq opoint (point))))
    (goto-char opoint)
    (and (boundp 'compilation-debug) compilation-debug (message "%s" errinfo))
    errinfo))

(defun next-error (&optional next-file)
  "Visit source code corresponding to the next error message, i.e.
the first one after the line point is on in the compilation buffer.  
The compilation buffer is produced by the \\[compile] command.
With prefix arg, skip to the next file. "
  (interactive "P")
  (setq compilation-last-error-buf
	(or (if (eq major-mode 'compile) (current-buffer))
	    compilation-last-error-buf
	    compile-last-compilation-buffer
	    "*compilation*"))
  (set-buffer (get-buffer compilation-last-error-buf))
  (let ((win (get-buffer-window compilation-last-error-buf)))
    (if win
	(select-window win)))
  (next-error-next-line next-file)
  (this-error))

(defun next-error-backward (&optional next-file)
  "Visit source code corresponding to the previous error message, i.e.
the first one before the line point is on in the compilation buffer. 
The compilation buffer is produced by the \\[compile] command.
With prefix arg, skip to the previous file. "
  (interactive "P")
  (setq compilation-last-error-buf
	(or (if (eq major-mode 'compile) (current-buffer))
	    compilation-last-error-buf
	    compile-last-compilation-buffer
	    "*compilation*"))
  (set-buffer (get-buffer compilation-last-error-buf))
  (let ((win (get-buffer-window compilation-last-error-buf)))
    (if win
	(select-window win)))
  (next-error-previous-line next-file)
  (this-error))

;; Parse error msgs, find file or files, and position cursor on the
;; appropriate lines.
;; The "primary" file is always at the top of the screen.
;; The *compilation* buffer is at the bottom, and reduced to
;; a smaller height, unless compilation-window-height is nil,
;; in which case existing windows are used without resizing them.
(defun this-error ()
  "Visit the source code corresponding to the line point is in in the 
compilation buffer.  This operates on the output from the
\\[compile] command, and should be invoked from the compilation 
buffer. "
  (interactive)
  (setq compilation-last-error-buf
	(or (if (eq major-mode 'compile) (current-buffer))
	    compilation-last-error-buf
	    compile-last-compilation-buffer
	    "*compilation*"))
  (let ((errinfo (compilation-parse-line)))
    (if (null errinfo)
	;; this error will leave one in the compilation buffer.
	;; usually this is of benefit - one can now move the up
	;; point back up to get back to the last error of interest.
	(error (concat "No more " compilation-error-type "s"
			 (if (and compilation-process
				  (eq (process-status compilation-process)
				      'run))
			     " yet" "")))

      (let* ((filename (nth 0 errinfo))
	     (linenumber (nth 1 errinfo))
	     (filename-2 (nth 2 errinfo))
	     (linenumber-2 (nth 3 errinfo)))
	(if filename
	    (let ((error-marker (compilation-get-marker filename linenumber)))
	      (if error-marker
		  (progn
		    (if compilation-window-height
			(progn
			  (pop-to-buffer
			   (get-buffer compilation-last-error-buf))
			  (delete-other-windows)
			  (split-window-vertically
			   (- (window-height) 
			      compilation-window-height))
			  (other-window 1)))
			  ;; recenter refreshes screen, very annoying on slow
			  ;; terminals, so do it ourselves.
		    (if (get-buffer-window compilation-last-error-buf)
			(progn
			  (save-excursion
			    (beginning-of-line 
			     (if compilation-context-lines
				 (- 1 compilation-context-lines)
			       (- 2 (/ (window-height) 2))))
			    (set-window-start (selected-window) (point) t))
			  (pop-to-buffer (marker-buffer error-marker)))
		      (switch-to-buffer (marker-buffer error-marker)))
		    (goto-char (marker-position error-marker))))))

	(if filename-2                          ; a two file match
	    (progn
	      (set-buffer compilation-last-error-buf)
	      (let ((error-marker
		     (compilation-get-marker filename-2 linenumber-2)))
		(if error-marker
		    (progn
		      (if (null compilation-window-height)
			  (pop-to-buffer (marker-buffer error-marker))
			(split-window-vertically nil)
			(switch-to-buffer-other-window
			 (marker-buffer error-marker)))
		      (goto-char (marker-position error-marker))
		      (other-window 1))))))))))

;; Return a marker to the beginning of linenumber in file filename,
;; visiting the file with compilation-find-file if necessary.
;; Call compilation-find-file-help and return nil if the file can't be found.
(defun compilation-get-marker (filename linenumber &optional list-hosed)
  (let* ((file-error-list (assoc filename compilation-error-list))
	 (error-marker (cdr (assoc linenumber file-error-list))))

    (if (not (and file-error-list error-marker))
	(cond ((not list-hosed)
	       (message "Parsing error messages...")
	       (compilation-parse-buffer)
	       (message "Parsing error messages...done")
	       (compilation-get-marker filename linenumber 'maybe))
	      ((eq list-hosed 'maybe)
	       (message "Parsing error messages again...")
	       (compilation-forget-errors)
	       (compilation-parse-buffer)
	       (message "Parsing error messages again...done")
	       (compilation-get-marker filename linenumber t))
	      (t (error "Can't parse buffer ")))
      (if (marker-buffer error-marker)
	  error-marker
	(save-excursion
	  (if (compilation-find-file filename)
	      (progn
		(setq file-error-list (assoc filename compilation-error-list))
		(cdr (assoc linenumber file-error-list)))
	    (if compilation-expert
		(error "File %s not found." filename))
	    (ding)
	    (compilation-find-file-help filename)
	    nil))))))

;; Try to find and open file filename, searching the tags table and/or 
;; the path in compilation-vpath if necessary.  Set the markers to errors
;; in the file by calling compilation-set-markers.  Returns t if the file
;; was found, nil if not (no error).
(defun compilation-find-file (filename)
  (catch 'not-found
    (setq filename
	  (or (if (file-exists-p filename)
		  filename
		nil)
	      (and compilation-use-tags-table
		   (if (eq compilation-use-tags-table t)
		       t
		     (yes-or-no-p (format "File %s not found, use tags table? "
					  filename)))
		   (let ((flist (tag-table-files))
			 (fregexp (concat ".*/" (regexp-quote filename) "$")))
		     (while (and flist
				 (not (string-match fregexp (car flist))))
		       (setq flist (cdr flist)))
		     (car flist)))
	      (if compilation-vpath
		  (let ((dirlist (cond ((listp compilation-vpath)
					compilation-vpath)
				     ((stringp compilation-vpath)
				      (list compilation-vpath))
				     (t nil))))
		  (while (and dirlist
			      (not (file-exists-p 
				    (expand-file-name filename 
						      (car dirlist)))))
		    (setq dirlist (cdr dirlist)))
		  (if dirlist
		      (expand-file-name filename (car dirlist))
		    nil)))
		(throw 'not-found nil)))

  (let* ((tags (and compilation-use-tags-table
		    (fboundp 'visit-tags-table-locally)
		    tags-file-name))
	 (hook next-error-find-file-hook)
	 (find-file-hooks (cond ((listp hook)
				 (append find-file-hooks hook))
				((symbolp hook)
				 (append find-file-hooks
					 (list hook)))
				(t find-file-hooks))))
    (save-excursion
      (set-buffer (find-file-noselect filename))
					; If we're using a local tags-table,
					; propagate it to all files we visit.
      (if tags (visit-tags-table-locally tags))
      (compilation-set-markers file-error-list)
      (setq error-marker (cdr (assoc linenumber file-error-list))))
  t)))

;; Pop up a *Help* buffer explaining the file-not-found situation.
(defun compilation-find-file-help (filename)
  (save-excursion
    (set-buffer compilation-last-error-buf)
    (with-output-to-temp-buffer "*Help*"
      (princ "************** next-error **************************")
      (terpri) (terpri)
      (princ (format "   File %s was not found." filename))
      (terpri) (terpri)
      (if compilation-use-tags-table
	  (progn
	    (princ (format
		    "   The current tags file is %s" tags-file-name))
	    (terpri)
	    (princ (substitute-command-keys 
		    "   Use the command \\[visit-tags-table] to change it. ")))
	(princ "   Searching the tags table is not enabled,")
	(terpri)
	(princ (substitute-command-keys "   Use the command \\[compilation-use-tags-table] from the compilation buffer to enable. ")))
      (terpri) (terpri)
      (princ (format "   The current search path is %s" compilation-vpath))
      (terpri)
      (princ (substitute-command-keys "   Use the command \\[compilation-add-vpath] from the compilation buffer
   to add directories to this path."))
      (terpri) (terpri)
      (princ "   If you don't want to see this message again:  ") (terpri)
      (princ "      (setq compilation-expert t)") (terpri)
      (princ "*****************************************************")
      (print-help-return-message 'message))))
    
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

(defun compilation-rsh-arglst (command &optional directory &rest args)
  "Return a list of arguments to call process, including the program
name as the first element, to run COMMAND in a subshell on a remote
host.  If optional DIRECTORY is present, add a \"cd\" command to that
directory, t means use default directory.

If COMMAND is of the form \"user@host:command\" then the command
is run on \"host\" as \"user\"; otherwise, if the buffer file name is
an ange-ftp style remote pathname then the indicated user and host
will be used; if neither of these is true then nil is returned. "
  (let* ((path (and (fboundp 'ange-ftp-ftp-path)
		    (ange-ftp-ftp-path default-directory)))
	 (remote-host (nth 0 path))
	 (logname (nth 1 path)))

    (and (eq directory t)
	 (setq directory (or (nth 2 path) default-directory)))
      
    (if (string-match
	 "^\\(\\([^@: \t\n]*\\)@\\)?\\([^@: \t\n]+\\):\\([^ ]+.*\\)"
	 command)
	(setq
	 remote-host (substring command (match-beginning 3) (match-end 3))
	 logname (if  (equal (match-beginning 2) (match-end 2))
		     nil
		   (substring command (match-beginning 2) (match-end 2)))
	 command (substring command (match-beginning 4) (match-end 4))))
    
    (if remote-host
	(let ((arglst (list (concat 
			     (and directory (concat "cd " directory "; "))
			     command))))
	  ;; Ange says remsh doesn't like -l, so we'll try to avoid it.
	  (and logname
	       (not (string-equal logname (user-login-name)))
	       (setq arglst (cons "-l" (cons logname arglst))))
	  (setq arglst (cons remote-host arglst))
	  (append (cons compilation-remote-shell-file-name arglst)
		  args))
      nil)))

(defun compilation-read-dir-name (prompt &optional dir default mustmatch)
  "Read directory name, prompting with PROMPT and completing in directory
DIR (optional, default is default-directory).  Optional DEFAULT is the
default in case user enters the null string.  Fourth arg MUSTMATCH non-nil
means require existing directory's name. "
  (completing-read prompt 'compilation-read-dir-name-internal
		   (or dir default-directory) 
		   mustmatch
		   (or default "")))

(defun compilation-read-dir-name-internal (string dir action)
  (if (eq action 'lambda)
      (and (stringp string)
	   (> (length string) 0)
	   (file-directory-p (expand-file-name string dir)))
    (let* ((name (file-name-nondirectory string))
	   (subdir (or (file-name-directory string) "."))
	   (realdir (expand-file-name subdir dir))
	   accepted)
      (setq accepted 
	    (delq nil
		  (mapcar (function 
			   (lambda (arg)
			     (if (string= (file-name-directory arg) arg)
				 arg)))
			  (file-name-all-completions name realdir))))
      (if action
	  accepted
	(if (null accepted)
	    nil
	  (if (and (subset '("./" "../") accepted)
		   (subset accepted '("./" "../")))
	      string
	    (let ((comp-name
		   (try-completion name (mapcar 'list accepted))))
	      (concat (file-name-as-directory subdir) comp-name))))))))

;; Stolen from emacs-19, from the tree-dired distribution.
(or (fboundp 'member)
    (defun member (x y)
      "Like memq, but uses `equal' for comparison.
This is a subr in Emacs 19."
      (while (and y (not (equal x (car y))))
	(setq y (cdr y)))
      y))

(defun subset (set1 set2)
  "Returns t if list SET1 is a subset of list SET2, nil otherwise. 
Membership is tested with member. "
  (catch 'exit
    (mapcar (function (lambda (arg)
			(or (member arg set2)
			    (throw 'exit nil))))
	    set1)
    t))
