;;                 Maple editing support package
;;    (borrows heavily from modula2.el, ada.el and tex-mode.el)
;;
;;               Bruno Salvy (Bruno.Salvy@inria.fr)
;;
;; Major mode for Maple editing. It provides functions for editing
;; Maple code and interacting with Maple and Mint. See the documentation
;; of maple-mode.
;;
;; LCD Archive Entry:
;; maple-mode|Bruno Salvy|Bruno.Salvy@inria.fr|
;; Maple editing support|
;; 29-Sep-94|1.1|~/modes/maple.el.Z|
;;
;; modified 03/05/90: C-p and C-n in maple-shell. B.S.
;; modified 29/05/90: maple-prompt-regexp B.S.
;; modified 21/08/90: mint-level, M-p and M-n in maple-shell B.S.
;; modified 16/11/90: C-cT bound to maple-toggle-previous-next-behavior B.S.
;; modified 01/01/91: C-cC-l available also in maple-shell B.S.
;; modified 30/07/91: maple-help adapted to MapleV. B.S.
;; modified 09/03/92: nicer tabulation handling B.S.
;; modified 09/06/92: comment-start regexp B.S.
;; 
;; 13-Jan-99: C. Schwieters (Charles.Schwieters@hec.utah.edu)
;;            did something to Bruno Salvy's elisp code.
;;            What works for me under Maple V R 3,4 for Irix and Linux:
;;            - running maple on the current buffer
;;              with graphical data appearing separate windows (under X).
;;              Also, the Help facility (C-c h) works.
;;            - running an interactive Maple shell (maple command)
;;              this gets kinda wierd sometimes, though.
;;
;; 2-Sept-99: CDS
;;            - removed extra slashes from maple-cmd in maple-help.
;;            - new version of Maple (5.1) does not have -f flag (and doesn't
;;              need it.
;;
;; 5-Oct-99:  CDS
;;            - added errorcursor=false to interface. This fixes some 
;;              interactive mode wierdness under R5.1.

(provide 'maple)

(defvar maple-mode-syntax-table nil
  "Syntax table in use in maple-mode buffers.")

(if maple-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\   " table)
    (modify-syntax-entry ?\n ">   "  table)
    (modify-syntax-entry ?\f ">   "  table)
    (modify-syntax-entry ?#  "<   "  table)
    (modify-syntax-entry ?\t "    "  table)
    (modify-syntax-entry ?*  ".   "  table)
    (modify-syntax-entry ?+  ".   "  table)
    (modify-syntax-entry ?-  ".   "  table)
    (modify-syntax-entry ?/  ".   "  table)
    (modify-syntax-entry ?=  ".   "  table)
    (modify-syntax-entry ?<  ".   "  table)
    (modify-syntax-entry ?>  ".   "  table)
    (modify-syntax-entry ?\' "\"   " table)
    (modify-syntax-entry ?\` "\"   " table)
    (modify-syntax-entry ?\" ".   "  table)
    (modify-syntax-entry ?\{ "(}  "  table)
    (modify-syntax-entry ?\[ "(]  "  table)
    (modify-syntax-entry ?\( "()  "  table)
    (modify-syntax-entry ?\} "){  "  table)
    (modify-syntax-entry ?\] ")[  "  table)
    (modify-syntax-entry ?\) ")(  "  table)
    (setq maple-mode-syntax-table table)))

(defvar maple-mode-map nil
  "Keymap used in Maple mode.")

(if maple-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'maple-newline)
    (define-key map "\C-?" 'maple-untab)
    (define-key map "\C-i" 'maple-tab)
    (define-key map "\C-c<" 'maple-backward-to-same-indent)
    (define-key map "\C-c>" 'maple-forward-to-same-indent)
    (define-key map "\C-c#" 'maple-inline-comment)
    (define-key map "\C-ce" 'maple-else)
    (define-key map "\C-cf" 'maple-for)
    (define-key map "\C-ch" 'maple-help)
    (define-key map "\C-ci" 'maple-if)
    (define-key map "\C-cl" 'maple-local)
    (define-key map "\C-cm" 'maple-modify)
    (define-key map "\C-cp" 'maple-procedure)
    (define-key map "\C-ct" 'maple-title)
    (define-key map "\C-cw" 'maple-while)
    (define-key map "\C-c\C-i" 'maple-make-library)
    (define-key map "\C-c\C-r" 'maple-region)
    (define-key map "\C-c\C-b" 'maple-buffer)
    (define-key map "\C-c\C-m" 'mint-buffer)
    (define-key map "\C-c\C-z" 'suspend-emacs)
    (setq maple-mode-map map)))

(defvar maple-indent 4 "*This variable gives the indentation in Maple-Mode")

(defun maple-mode ()
  "This is a mode intended to support program development in Maple.
All control constructs of Maple can be reached by typing
Control-C followed by the first character of the construct.
Use \\[maple-region] to run Maple on the current region under
a special subshell.  \\[maple-buffer] does the whole buffer,
\\[maple-make-library] doeis it with maple -s option.
\\[mint-buffer] runs mint on the buffer.

  Control-c f for           Control-c e else
  Control-c i if            Control-c l local
  Control-c w while         Control-c p proc
  Control-c # comment       Control-c h help
  Control-c m modify        Control-c t title
  Control-c ( paired parens
  Control-c Control-z suspend-emacs

  C-c < and C-c > move backward and forward respectively to the next line
  having the same (or lesser) level of indentation.

   maple-indent controls the number of spaces for each indentation.

   Note that in a Maple subshell buffer, C-p and C-n are bound to
   maple-previous-command and maple-next-commmand, while M-p and M-n
   are bound to previous-line and next-line. Use C-cT to toggle this
   behavior.

\\{maple-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (maple-define-common-keys maple-mode-map)
  (use-local-map maple-mode-map)
  (setq major-mode 'maple-mode)
  (setq mode-name "Maple")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 72)
  (set-syntax-table maple-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
;;  (make-local-variable 'indent-line-function)
;;  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'maple-mode-hook))

(defun maple-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (let ((hpos (current-indentation)))
    (newline)
    (indent-to hpos)))

(defun maple-tab ()
  "Indent to next tab stop."
  (interactive)
  (if (> (current-column) (current-indentation))
      (insert "\t")
    (back-to-indentation)
    (let ((ci (current-indentation)))
      (backward-delete-char-untabify ci)
      (indent-to (* (1+ (/ ci maple-indent)) maple-indent)))))

(defun maple-tabsize (s)
  "changes spacing used for indentation. Reads spacing from minibuffer."
  (interactive "new indentation spacing: ")
  (setq maple-indent s))

(defun maple-untab ()
  "Delete backwards to previous tab stop."
  (interactive)
  (backward-delete-char-untabify
   (let ((ind (current-indentation)))
     (if (and (= ind (current-column)) (>= ind maple-indent))
         maple-indent 1))))

(defun maple-go-to-this-indent (step indent-level)
  "Move point repeatedly by <step> lines till the current line
has given indent-level or less, or the start/end of the buffer is hit.
Ignore blank lines and comments."
  (while (and
          (zerop (forward-line step))
          (or (looking-at "^[   ]*$")
              (looking-at "^[   ]*#")
              (looking-at "^<<[A-Za-z0-9_]+>>")
              (looking-at "^[A-Za-z0-9_]+:")
              (> (current-indentation) indent-level)))
    nil))

(defun maple-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer."
  (interactive)
  (maple-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun maple-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer."
  (interactive)
  (maple-go-to-this-indent 1 (current-indentation))
  (back-to-indentation))

(defun maple-for ()
  "Build skeleton for-loop statment, prompting for the loop parameters."
  (interactive)
  (let ((for (read-string "var: ")))
    (if (string-equal for "")
        (let ((to (read-string "to: ")))
          (if (not (string-equal to ""))
              (insert " to " to)))
      (insert "for " for)
      (let ((in (read-string "in: ")))
        (if (not (string-equal in ""))
            (insert " in " in)
          (let ((from (read-string "from: ")))
            (if (not (string-equal from ""))
                (insert " from " from)))
          (let ((by (read-string "by: ")))
            (if (not (string-equal by ""))
                (insert " by " by)))
          (let ((to (read-string "to: ")))
            (if (not (string-equal to ""))
                (insert " to " to)))))))
  (let ((while (read-string "while: ")))
    (if (not (string-equal while ""))
        (insert " while " while)))
  (insert " do")
  (maple-newline)
  (maple-newline)
  (insert "od;")
  (end-of-line 0)
  (maple-tab))

(defun maple-while ()
  "Build skeleton while-loop statment, prompting for the loop parameters."
  (interactive)
  (insert "while " (read-string "cond: "))
  (insert " do")
  (maple-newline)
  (maple-newline)
  (insert "od;")
  (end-of-line 0)
  (maple-tab))

(defun maple-title ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (if (eq (point) (point-min))
      nil
    (set-mark (point))
    (goto-line 1))
  (insert "\n\n")
  (previous-line 2)
  (insert "##    -*-Maple-*-\n")
  (insert "##\n##    Title: \t")
  (insert (read-string "Title: "))
  (insert "\n##    Created:\t")
  (insert (current-time-string))
  (insert "\n##    Author: \t")
  (insert (user-full-name))
  (insert (concat "\n##\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "##\n## Description: ")
  (end-of-line nil))

(defun maple-modify ()
  "Insert a comment block containing the modification, author, etc."
  (interactive)
  (set-mark (point))
  (goto-line 1)
  (while (char-equal (char-after (point)) 35)
    (forward-line 1))                                        
  (insert "##\n##    Modified: \t")
  (insert (current-time-string))
  (insert "\n##    Author: \t")
  (insert (user-full-name))
  (insert "\n##    Modification: ")
  (insert (read-string "Modification: "))
  (insert "\n##\n"))

(defun maple-if ()
  "Insert skeleton if statment, prompting for <boolean-expression>."
  (interactive)
  (insert "if " (read-string "cond: ") " then")
  (maple-newline)
  (maple-newline)
  (insert "fi;")
  (end-of-line 0)
  (maple-tab))

(defun maple-else ()
  "Add an elif clause to an if statement, prompting for the condition.
   When no condition is given, put an else."
  (interactive)
  (maple-untab)
  (let ((condition (read-string "elif: ")))
    (if (not (string-equal condition ""))
        (insert "elif " condition " then")
      (insert "else")))
  (maple-newline)
  (maple-tab))

(defun maple-local ()
  "Add a new local variable, inserting the word local if necessary."
  (interactive)
  (save-excursion
    (set-mark (point))
    (while (or (> (current-indentation) 0)
               (looking-at "#")
               (looking-at "end")
               (looking-at "option"))
      (forward-line -1))
    (let ((first-time))
      (if (looking-at "local")
          (setq first-time nil)
        (forward-line 1)
        (insert "local ;\n")
        (forward-line -1)
        (setq first-time t))
      (search-forward ";")
      (backward-char)
      (let ((newvar (read-string "New variable: ")))
        (if first-time (insert newvar)
          (insert ", " newvar))))))

(defun maple-procedure ()
  (interactive)
  (let ((name (read-string "Name: " ))
        args)
    (insert name ":=proc (")
    (insert (read-string "Arguments: ") ")")
    (let ((options (read-string "Options: ")))
      (if (not (string-equal options ""))
          (progn (maple-newline)
                 (insert "options " options ";"))))
    (maple-newline)
    (maple-newline)
    (insert "end: # ")
    (insert name)
    (end-of-line 0)
    (maple-tab)))

(defun maple-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun maple-inline-comment ()
  "Start a comment after the end of the line, indented at least COMMENT-COLUMN.
If starting after END-COMMENT-COLUMN, start a new line."
  (interactive)
  (end-of-line)
  (if (> (current-column) end-comment-column) (newline))
  (if (< (current-column) comment-column) (indent-to comment-column))
  (insert "#  "))

(defun maple-display-comment ()
  "Inserts 3 comment lines, making a display comment."
  (interactive)
  (insert "#\n# \n#")
  (end-of-line 0))

;;(defvar maple-lib-directory "/usr/local/maple/lib")
;;
;;(defun maple-help ()
;;  "Like describe-function in lisp-mode, tries to guess which function is
;;interesting for the user and prompts for confirmation, then displays help.
;;This could be much better if we called Maple to find the file, but I do not
;;see how it could be as fast. This will only work with version 5 or higher."
;;  (interactive)
;;  (save-excursion
;;    (let ((orig (point))
;;	    eow bow)
;;	(forward-word -1)
;;	(setq bow (point))
;;	(forward-word 1)
;;	(setq eow (point))
;;	(let* ((posfuncname (buffer-substring bow eow))
;;	       (funcname (read-string
;;			  (if (string-equal posfuncname "")
;;			      "Help about: "
;;			    (concat "Help about [" posfuncname "]: "))))
;;	       filename)
;;	  (and (string-equal funcname "")
;;	       (setq funcname posfuncname))
;;	  (if (string-match "[a-zA-Z]*\\[" funcname)
;;	      (setq filename
;;		    (concat maple-lib-directory "/help/"
;;			    (substring funcname 0 (- (match-end 0) 1))
;;			    "/text/"
;;			    (substring funcname (match-end 0)
;;				       (- (length funcname) 1))
;;			    ".m"))
;;	    (setq filename (concat maple-lib-directory "/help/text/"
;;				   funcname ".m")))
;;	  (or (file-exists-p filename)
;;	      (error "Cannot find help for this function"))
;;	  (with-output-to-temp-buffer "*Maple Help*"
;;	    (buffer-flush-undo standard-output)
;;	    (save-excursion
;;	      (set-buffer standard-output)
;;	      (insert-file-contents filename)
;;	      (goto-char (point-min))
;;	      (kill-line 4)
;;	      (replace-regexp "\230\226\214" "\n")
;;	      (goto-char (point-min))
;;	      (replace-regexp "[^\n -~]" "")))))))
;;

(defun maple-help ()
  "Like describe-function in lisp-mode, tries to guess which function is
interesting for the user and prompts for confirmation, then displays help.
This could be much better if we called Maple to find the file, but I do not
see how it could be as fast. This will only work with version 5 or higher."
  (interactive)
  (save-excursion
    (let ((orig (point))
	    eow bow)
	(forward-word -1)
	(setq bow (point))
	(forward-word 1)
	(setq eow (point))
	(let* ((posfuncname (buffer-substring bow eow))
	       (funcname (read-string
			  (if (string-equal posfuncname "")
			      "Help about: "
			    (concat "Help about [" posfuncname "]: "))))
	       filename)
	  (and (string-equal funcname "")
	       (setq funcname posfuncname))
	  (setq help-buffer-name (concat "*" funcname "-MapleHelp*"))

	   (if (get-buffer help-buffer-name)
	       nil
	    (progn
	      (setq maple-cmd (concat "'help(\`" funcname "\`);'"))
	      (maple-run-help help-buffer-name maple-cmd)))
	  (display-buffer (get-buffer help-buffer-name))))))
	     

; 
; tried to pipe output from current maple process to another buffer. Didn't
; know how to wait for the output to complete before switching back to the 
; main buffer
;
	  
;	   (if (get-buffer help-buffer-name)
;	       (set-buffer (get-buffer help-buffer-name))
;	     (let ((main-buffer-name (concat "*" (buffer-name) "-Maple*")))
;	       (or (get-buffer main-buffer-name)
;		   (maple-start-shell main-buffer-name))
;	       (setq maple-cmd (concat "?" funcname "\n"))
;	       (setq process-name (get-buffer-process main-buffer-name))
;		(set-process-buffer process-name 
;				    (get-buffer-create help-buffer-name))
;	       (switch-to-buffer-other-window help-buffer-name)
;	       (send-string process-name maple-cmd)
;	       ;wait for help command to be processed
;	       (set-process-buffer process-name 
;				   (get-buffer main-buffer-name))
;	       ))))))
	    
	  


;;; Invoking Maple in an inferior shell.

(defvar maple-command "maple"
  "The command to run maple on a file.")

(defvar maple-filter-command (concat maple-command " -q")
  "The command to run maple as a filter. Older versions require a -f.")

(defvar maple-args ""
  "*Arguments passed to maple. For old versions of Maple, -q could be useful")

;;(defvar maple-prompt-regexp "^[^-> \n]*>+ +" "\
;;*Regexp defining the prompt in Maple sessions.")

(defvar maple-prompt-regexp "^[^>]> " "\
*Regexp defining the prompt in Maple sessions.")

(defvar mint-command "mint"
  "The command to run mint on a file.  The name of the file will be appended
to this string, separated by <.")

(defvar mint-level "2"
  "Sets the verbosity of Mint")

(defun maple-define-common-keys (keymap)
  "Define the keys that we want defined both in maple-mode
and in the maple-shell."
  (define-key keymap "\C-c\C-k" 'maple-kill-job)
  (define-key keymap "\C-c\C-l" 'maple-recenter-output-buffer)
  (define-key keymap "\C-ch" 'maple-help))

(defvar maple-shell-map nil
  "Keymap for the maple shell.  A shell-mode-map with a few additions")

(defvar maple-temp-directory "/tmp/"
  "*Directory in which to create temporary files.")

(defvar zap-file nil
  "Temporary file name used for text being sent as input to Maple.")

(defun maple ()
  "Run maple in a buffer, without shell. Exiting maple will kill the buffer.
The buffer is called *Maple*, and the program used comes from variable
maple-command (default maple). The buffer is put in shell-mode with
a maple-syntax-table."
  (interactive)
  (if (get-buffer "*Maple*")
      nil
    (maple-start-shell "Maple"))
  (switch-to-buffer "*Maple*"))

;(defun maple-strip-cr (inbuf)
;  "Strip off carriage-return character."
;;;  (setq a "howdy"))
;  (setq chars (string-to-list inbuf))
;  (setq newstring "")
;  (while chars
;    (setq char (car chars))
;    (setq chars (cdr chars))
;    (concat newstring char)))
;;    (if (not (equal (char-to-string char) ""))


(defun maple-start-shell (name)
  (require 'shell)
  (save-excursion
;    (set-process-sentinel
     (make-comint name (concat maple-command maple-args))
;     'maple-process-sentinel)
;    (set-buffer name)
    (set-buffer (concat "*" name "*"))

    (setq comint-process-echoes t)
;;    (setq comint-preoutput-filter-functions '(maple-strip-cr))
;;    (setq comint-output-filter-function '(maple-strip-cr))
    (setq comint-prompt-regexp maple-prompt-regexp)

    (make-local-variable 'maple-startup-cmds)
    (setq maple-startup-cmds 
    "interface(errorcursor=false,ansi=false,screenheight=1000,echo=1);plotsetup(x11);\n")
;    (send-string (get-buffer-process name) maple-startup-cmds)
    (sit-for 1)
    (send-string  name maple-startup-cmds)

    (set-syntax-table maple-mode-syntax-table)
    (setq maple-shell-map (copy-keymap comint-mode-map))
    (maple-define-common-keys maple-shell-map)
    (define-key maple-shell-map "\C-ch" 'maple-help)
    (use-local-map maple-shell-map)))

;;(defun maple-start-shell (name)
;;  (require 'shell)
;;  (save-excursion
;;    (set-process-sentinel
;;     (if (not (string-equal maple-args ""))
;;	   (start-process "maple" name
;;;			 (concat exec-directory "env")
;;;			 "TERM=emacs"  ; so that the process may know it 
;;;					 ; is called by emacs
;;;			 "PAGER=cat"   ; because of 4.3 help
;;;					 ;                  
;;;                        "PAGER=emacsclient"   ; because of 4.3 help
;;;			 "-"
;;			  maple-command
;;			  maple-args)
;;	 (start-process "maple" name
;;;		       (concat exec-directory "env")
;;;		       "TERM=emacs"  ; so that the process may know it 
;;;					 ; is called by emacs
;;;		       "PAGER=cat"   ; because of 4.3 help
;;;                      "PAGER=emacsclient"   ; because of 4.3 help
;;;		       "-"
;;			maple-command))
;;     'maple-process-sentinel)
;;    (set-buffer name)
;;    (shell-mode)
;;    (make-local-variable 'shell-prompt-pattern)
;;    (setq shell-prompt-pattern maple-prompt-regexp)
;;
;;    (make-local-variable 'maple-startup-cmds)
;;    (setq maple-startup-cmds 
;;	    "interface(ansi=false,screenheight=1000,echo=0);plotsetup(x11);\n")
;;    (send-string (get-buffer-process name) maple-startup-cmds)
;;
;;    (set-syntax-table maple-mode-syntax-table)
;;    (setq maple-shell-map (copy-keymap shell-mode-map))
;;    (maple-define-common-keys maple-shell-map)
;;    (define-key maple-shell-map "\M-p" 'previous-line)
;;    (define-key maple-shell-map "\M-n" 'next-line)
;;    (define-key maple-shell-map "\C-p" 'maple-previous-command)
;;    (define-key maple-shell-map "\C-n" 'maple-next-command)
;;    (define-key maple-shell-map "\C-cT" 'maple-toggle-previous-next-behavior)
;;    (use-local-map maple-shell-map)))

(defun maple-run-help (buffer-name help-command)
  (save-excursion
    (setq maple-help-command (concat "echo 'interface(ansi=false):'"
				     help-command "|"
				     maple-filter-command))
    (shell-command maple-help-command  (get-buffer-create buffer-name))
    (set-buffer buffer-name)
;    (insert-buffer "*Shell Command Output*")
    (goto-line 1)))


(defun maple-process-sentinel (proc mesg)
  (let ((stat (process-status proc))
        (name (process-buffer proc)))
    (cond ((eq stat 'run) nil)
          ((eq stat 'stop)
           (continue-process proc)
           (if (and (not (eq (process-status proc) 'run))
                    name
                    (save-excursion
                      (kill-buffer name)))))
          (t (if (not name) nil
               (save-excursion
                 (kill-buffer name)))))))

(defun maple-region (beg end)
  "Run Maple on the current region.  A temporary file (zap-file) is
written in directory maple-temp-directory, but Maple is run in the current
directory."
  (interactive "r")
  (let ((name (concat (buffer-name) "-Maple")))
    (or (get-buffer (concat "*" name "*"))
        (maple-start-shell name))
    (if zap-file
        (if (file-exists-p zap-file)
            (delete-file zap-file))
      (setq zap-file
            (expand-file-name
             (concat maple-temp-directory
                     (make-temp-name "#mz")))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (write-region beg end zap-file t nil))
      (send-string (get-buffer-process (concat "*" name "*"))
                   (concat "read \`" zap-file "\`;\n")))
    (sit-for 1)
    (maple-recenter-output-buffer nil)))

(defun mint-shell (name)
  (require 'shell)
  (save-excursion
    (set-process-sentinel
     (start-process "mint" "*Mint*" mint-command "-i" mint-level name)
     'mint-process-sentinel)
    (set-buffer "*Mint*")
    (shell-mode)))

(defun mint-process-sentinel (proc mesg)
  (let ((stat (process-status proc))
        (name (process-buffer proc)))
    (cond ((eq stat 'run) nil)
          ((eq stat 'stop)
           (continue-process proc))
          (t nil))))

(defun mint-region (beg end)
  "Run Mint on the current region.  A temporary file (zap-file) is
written in directory maple-temp-directory, but Mint is run in the current
directory."
  (interactive "r")
  (let ((name "*Mint*"))
    (if (get-buffer name)
        (save-excursion (kill-buffer name)))
    (if zap-file
        (if (file-exists-p zap-file)
            (delete-file zap-file))
      (setq zap-file
            (expand-file-name
             (concat maple-temp-directory
                     (make-temp-name "#mz")))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (write-region beg end zap-file t nil))
      (mint-shell zap-file))
    (pop-to-buffer "*Mint*")))

(defun maple-buffer ()
  "Run Maple on current buffer.  See \\[maple-region] for more information."
  (interactive)
  (maple-region (point-min) (point-max)))

(defun maple-make-library ()
  "Run Maple on current buffer, with option -s."
  (interactive)
  (let ((maple-args "-s"))
    (maple-region (point-min) (point-max))))

(defun mint-buffer ()
  "Run Mint on current buffer.  See \\[mint-region] for more information."
  (interactive)
  (mint-region (point-min) (point-max)))

(defun maple-kill-job ()
  "Kill the currently running Maple job."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (kill-buffer (current-buffer))
    (let ((name (concat "*" (buffer-name) "-Maple*")))
      (and (get-buffer name)
           (kill-buffer name)))))

(defun maple-recenter-output-buffer (linenum)
  "Redisplay buffer of Maple job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((old-buffer (current-buffer))
        (maple-shell(if (get-buffer-process (current-buffer))
                        (current-buffer)
                      (or (get-buffer (concat "*" (buffer-name) "-Maple*"))
                          (get-buffer "*Maple*")))))
    (if (null maple-shell)
        (message "No Maple output buffer")
      (pop-to-buffer maple-shell)
      (goto-char (point-max))
      (recenter (if linenum
                    (prefix-numeric-value linenum)
                  (/(window-height) 2)))
      (pop-to-buffer old-buffer))))

(defun maple-previous-command ()
  "Recall previous maple command."
  (interactive)
  (maple-relative-command -1))

(defun maple-next-command ()
  "Step to maple next command line."
  (interactive)
  (maple-relative-command 1))

(defun maple-relative-command (dir)
  "Step to previous or next command line according to the first argument
being 1 or -1."
  (while (and (zerop (forward-line dir))
              (not (looking-at maple-prompt-regexp))
              (looking-at "^"))); forward-line at the end of a buffer
  (end-of-line))

(defun maple-toggle-previous-next-behavior ()
  "Change C-p/M-p C-n/M-n from previous-line and next-line to
maple-previous-command and maple-next-command and reciprocally"
  (interactive)
  (if (equal (key-binding "\C-p") 'previous-line)
      (progn
        (define-key maple-shell-map "\M-p" 'previous-line)
        (define-key maple-shell-map "\M-n" 'next-line)
        (define-key maple-shell-map "\C-p" 'maple-previous-command)
        (define-key maple-shell-map "\C-n" 'maple-next-command))
    (define-key maple-shell-map "\C-p" 'previous-line)
    (define-key maple-shell-map "\C-n" 'next-line)
    (define-key maple-shell-map "\M-p" 'maple-previous-command)
    (define-key maple-shell-map "\M-n" 'maple-next-command)))

