; $Id: pdb.el,v 1.6 1998/11/18 08:58:52 wwe Exp $

; This is a first attempt to add support for Python's pdb to xemacs
; using the Grand Unified Debugger mode gud.el by Eric S. Raymond

; requires 'pdb' to be an executable command which runs the pdb.py from 
; Python 1.4b3 or later (e.g. a symbolic link to /usr/local/lib/python1.4/pdb.py)

; On Windows platforms, you can place a "pdb.bat" file somewhere along
; your path, containing something like 

; "C:\Program Files\Python\python.exe" -u "C:\Program Files\Python\Lib\pdb.py" %1 %2 %3 %4 %5 %6 %7 %8 %9

; (modify the paths according to where your local Python installation lives)

; - copy pdb.el to your xemacs site-lisp directory (or somewhere else
;   on your xemacs' load path)  Note - for XEmacs 21 this is not necessary
; - add (load-library "pdb.el") to your ~/.emacs file (not needed for Xemacs 21)
; - open some python file and start pdb from Emacs with M-x pdb
; - type in pdb commands or use the gud commands from your source buffer
;   (C-x C-a C-...see the 'gud-def's below and gud.el for more information) 

; This version includes experimental toolbar support for many pdb 
; commands (see below). 

; Tested with xemacs 19.13, 19.14, 19.15 / gud.el 1.3 / python 1.4b3, 1.5.1

; Wolfgang Weitz <wwe@aifb.uni-karlsruhe.de>

; Charles G Waldman <cgw@pgt.com> 1998-11-18
; Fixed gud-pdb-marker filter so this works on Windows, as well as
; directories with a hyphen in the name (like site-packages).  Also
; use completing read to read the command line at startup.
;--------------------------------------------------------------------------

;;
;; UNCOMMENT THE FOLLOWING LINES IF
;; you want the default toolbar 'debug' button start pdb instead of gdb
;;

;; (defun toolbar-debug ()
;;   (interactive)
;;   (call-interactively 'pdb)
;; )

;--------------------------------------------------------------------------

(require 'gud)

(defvar gud-pdb-history nil)

(defun gud-pdb-massage-args (file args) (cons file args))

(defvar gud-pdb-marker-acc "")

(defun gud-pdb-marker-filter (string)
  (if (string-match
       "> \\([-/\\\\: a-zA-Z0-9_\\.]+\\)(\\([0-9]+\\))[?a-zA-Z0-9_]+().*"
         string)
      (setq gud-last-frame
	    (cons
	     (substring string (match-beginning 1) (match-end 1))
	     (string-to-int 
	      (substring string (match-beginning 2) (match-end 2))))))
  string)

(defun gud-pdb-find-file (f)
  (find-file-noselect f))

;;;###autoload

(defun pdb (command-line)
     "Run pdb on program FILE in buffer *gud-FILE*.
      The directory containing FILE becomes the initial working directory
      and source-file directory for your debugger."
  (interactive
  (list (read-shell-command "Run pdb (like this): "
			       (if (consp gud-pdb-history)
				   (car gud-pdb-history) "pdb ") 
			       '(gud-pdb-history . 1))))
  (gud-overload-functions '((gud-massage-args . gud-pdb-massage-args)
			    (gud-marker-filter . gud-pdb-marker-filter)
			    (gud-find-file . gud-pdb-find-file)
			    ))

  (gud-common-init command-line "pdb")

  (gud-def gud-break  "break %l"    "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clear %l"    "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"        "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next"        "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "continue"    "\C-r" "Continue with display.")
  (gud-def gud-finish "return"      "\C-f" "Finish executing current function.")
  (gud-def gud-up     "up %p"       "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"     ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "p %e"        "\C-p" "Evaluate Python expression at point.")
 
  (gud-def gud-where "where" "?" "where")

  (setq comint-prompt-regexp "^(Pdb) ")
  (run-hooks 'pdb-mode-hook)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Toolbar button support for xemacs 19.13 (and better)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar toolbar-where-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"X	c #CCCC9999FFFF\",
\"o	c #99996666CCCC\",
\"O	c #FFFFFFFF0000\",
\"+	c #FFFFCCCC3333\",
\"@	c #0000FFFF0000\",
\"#	c #000077770000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      ........              \",
\"      .XXXXXX.              \",
\"      .Xooooo.     ....     \",
\"      .Xooooo.    ......    \",
\"      .Xooooo.   ...  ...   \",
\"      .Xooooo.  ...    ...  \",
\"      .O+++++.   ..    ...  \",
\"      .O+++++.        ...   \",
\"      .O+++++.       ...    \",
\"      .O+++++.      ...     \",
\"      .O+++++.     ...      \",
\"      .@#####.     ...      \",
\"      .@#####.              \",
\"      .@#####.     ...      \",
\"      .@#####.     ...      \",
\"      .@#####.     ...      \",
\"      ........              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-intro.xbm" 
          (file-name-as-directory (expand-file-name "eos" data-directory))))
 
    )
  "A 'where' icon pair.")

(defvar pdb-toolbar
  '(
    [eos::toolbar-run-icon
     gud-finish
     t
     "Finish executing current function"]
    [eos::toolbar-cont-icon
     gud-cont
     t
     "Continue current program"]
    [eos::toolbar-step-into-icon
     gud-step
     t
     "Step into (aka step)"]
    [eos::toolbar-step-over-icon
     gud-next
     t
     "Step over (aka next)"]
    [eos::toolbar-up-icon
     gud-up
     t
     "Stack Up (towards \"cooler\" - less recently visited - frames)"]
    [eos::toolbar-down-icon
     gud-down
     t
     "Stack Down (towards \"warmer\" - more recently visited - frames)"]
    [ toolbar-where-icon
     gud-where
     t
     "Show stack (aka where)" ]
    ))

(defun pdb-install-toolbar ()
  (interactive)
      (require 'eos-toolbar "sun-eos-toolbar")
      (set-specifier default-toolbar (cons (current-buffer) pdb-toolbar)))
 
(add-hook 'pdb-mode-hook 'pdb-install-toolbar)

