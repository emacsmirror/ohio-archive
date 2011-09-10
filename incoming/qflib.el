;;; qflib.el version 0.97.1

;;; A collection of little utilities that come in handy when writing
;;; java code, some especially tuned for cooperation with the
;;; de.qfs.lib.log package of our free Java library qflib.

;;; Many of the functions are intended to help programmers use a good
;;; coding style by relieving them of most of the typing burden
;;; associated with things like complete javadoc comments or not using
;;; import *;

;;; An up-to-date version of qflib.el is always available at
;;; http://www.qfs.de/en/projects/elisp/index.html
;;; or straight from
;;; http://www.qfs.de/en/download.html#elisp

;;; For more information about qflib take a look at
;;; http://www.qfs.de/en/projects/qflib/index.html

;;; Quality First Software's home page is at
;;; http://www.qfs.de

;;; To contact the author send an email to gs@qfs.de.

;;; Contributor(s):
;;; Oliver Brandt, ATecoM GmbH <oliver@atecom.com>:
;;;     XEmacs menus, bugfixes

;;{{{ license

;;; Copyright (C) 2000 Gregor Schmid, Quality First Software

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{ install

;;; To install qflib put it anywhere on your load path, byte-compile
;;; it and edit your .emacs file as follows:

;;; If you want to use a more convenient prefix key for qflib
;;; functions, set the following variable like:
;;; (setq qflib-prefix-key "\C-x\C-j")

;;; Make sure qflib is loaded on demand
;;; (autoload 'qflib-mode "qflib"
;;;   "Minor mode that offers some utilities for editing Java code")

;;; Add qflib-mode to the mode hook of your favourite java-mode
;;; (add-hook 'java-mode-hook 'qflib-mode)

;;; The method qflib-find-stacktrace-error is typically used
;;; outside a java buffer, like a shell or compilation buffer, so you
;;; must define a global keybinding if you want to, e.g.
;;; (global-set-key "\C-x\C-j\C-m" 'qflib-find-stacktrace-error)
;;; to bind it to ^x-^j-return.

;;; Also qflib-find-stacktrace-error needs to know where to look for
;;; your Java source files. Set the variable qflib-source-directories
;;; accordingly, e.g.
;;; (setq qflib-source-directories
;;;   (list (expand-file-name "~/src/Java")))

;;}}}

;;{{{ history

;;; Version 0.97.1
;;; ==============
;;;
;;; - XEmacs menus contributed by Oliver Brandt
;;; - Optional argument for qflib-find-stacktrace-error
;;; - Indentation fixes for qflib-make-doc and qflib-make-method-doc
;;; - allow whitespace before closing brace in qflib-defun-regexp

;;; Version 0.97.0
;;; ==============
;;;
;;; First public release.

;;}}}

;;{{{ requirements

(require 'easymenu)

;;}}}

;;----------------------------------------------------------------------
;; Customizable variables
;;----------------------------------------------------------------------
;;{{{ qflib-prefix-key

(defvar qflib-prefix-key "\C-x\C-j"
  "*Prefix key for qflib functions.
This can either be a string like \"\\C-x\\C-j\" or a symbol for a
modifier like 'super")

;;}}}
;;{{{ qflib-logger-comment

(defvar qflib-logger-comment "The Logger used for logging."
  "*Comment used when creating a Logger with `qflib-make-logger'.")

;;}}}
;;{{{ qflib-source-directories

(defvar qflib-source-directories nil
  "*List of directories to search for java source files.
You must define these in order to use `qflib-find-stacktrace-error'.")

;;}}}

;;----------------------------------------------------------------------
;; You probably don't want to mess with the following variables
;;----------------------------------------------------------------------
;;{{{ qflib-mode

(defvar qflib-mode nil
  "Flag that indicates whether qflib-mode is turned on in the
current buffer.")
(make-variable-buffer-local 'qflib-mode)

;;}}}
;;{{{ qflib-mode-map

(defvar qflib-mode-map nil
  "Keymap used for qflib mode.")

;;}}}
;;{{{ qflib-xemacsp

(defconst qflib-xemacsp (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")

;;}}}
;;{{{ qflib-qualifiers

(defvar qflib-qualifiers
  '("public" "protected" "private"
    "static" "final" "volatile" "transient" "synchronized")
  "List of possible qualifiers for Java methods.")

;;}}}
;;{{{ qflib-defun-regexp

(defconst qflib-defun-regexp
  (let* ((qual "")
	 (dummy (mapcar
		 '(lambda (x)
		    (setq qual
			  (concat qual (if (> (length qual) 0) "\\|" "") x)))
		 qflib-qualifiers))
	 (qual (concat "\\(" qual "\\)"))
	 (word "\\(\\w\\|\\s_\\)+")
	 (type "\\(\\w\\|\\s_\\|\\.\\)+[][ \t]*")
	 (ws "[ \t\r\n]"))
    (concat
     "^[ \t]*"				; whitespace
     "\\(\\(" qual "[ \t]+\\)*\\)"	; qualifiers (1)
     "\\(\\(" type "\\)" ws"+\\)?"	; return type (5)
     "\\(" word "\\)" ws"*" "("		; method name (7)
     "\\("				; optional arguments (9)
     ws"*" type ws"+" word		; first arg
     "\\(" ws"*" "," ws"*" type ws"+" word "\\)*" ; rest args
     "\\)?" ws"*" ")"
     "\\(" ws"*" "throws" ws"+"		; optional throws clause (15)
     "\\(" type				; first exception
     "\\(" ws"*," ws"*" type "\\)*\\)"	; other exceptions
     "\\)?" ws"*{"
     ))
  "Regular expression that matches the beginning of a Java method.
It has the following limitations:
- no comments inside method declaration
- only supports `String[] args', not `String args[]'")

(defconst qflib-reindex-qualifiers 1)
(defconst qflib-reindex-return 5)
(defconst qflib-reindex-name 7)
(defconst qflib-reindex-args 9)
(defconst qflib-reindex-throws 16)

;;}}}
;;{{{ qflib-class-regexp

(defconst qflib-class-regexp
  (let* ((qual "")
	 (dummy (mapcar
		 '(lambda (x)
		    (setq qual
			  (concat qual (if (> (length qual) 0) "\\|" "") x)))
		 qflib-qualifiers))
	 (qual (concat "\\(" qual "\\)"))
	 (word "\\(\\w\\|\\s_\\)+")
	 (type "\\(\\w\\|\\s_\\|\\.\\)+")
	 (ws "[ \t\r\n]"))
    (concat
     "^[ \t]*"				; whitespace
     "\\(\\(" qual "[ \t]+\\)*\\)"	; qualifiers (1)
     "\\(class\\|interface\\)" ws"+"	; type (4)
     "\\(" word "\\)" ws"*" 		; class name (5)
     "\\(extends" ws"+"			; optional extends (8)
     type ws"*\\)?"			; baseclass (9)
     "\\(implements" ws"+"		; optional interfaces (10)
     "\\(" type				; first interface
     "\\(" ws"*," ws"*" type "\\)*\\)"	; other interfaces
     "\\)?" ws"*{"
     ))
  "Regular expression that matches a Java class or interface definition.")

(defconst qflib-reindex-type 4)
(defconst qflib-reindex-cname 5)
(defconst qflib-reindex-base 9)
(defconst qflib-reindex-inter 10)

;;}}}
;;{{{ qflib-stacktrace-error-regexp

(defconst qflib-stacktrace-error-regexp
  (concat "^\t\\(at \\)?"
	  "\\([^(\\$]+\\)"		; package
	  "\\.\\([^\\$\\.]+\\)"		; class
	  "\\(\\$[^\\.]+\\)?"		; optional inner class
	  "\\.\\([^\\.(]+\\)"		; method
	  "\\((\\(.*, \\)?Compiled Code)\\|(\\([^:]+\\):\\([0-9]+\\))\\)") ; line (maybe)
  "Regexp that matches errors in a Java stacktrace")

;;}}}

;;----------------------------------------------------------------------
;; The minor mode
;;----------------------------------------------------------------------
;;{{{ qflib-mode

(defun qflib-mode (&optional arg)
  "Toggle qflib mode.

With arg turn qflib mode on iff arg is positive.

qflib mode is a collection of little utilities that come in
handy when editing Java code. It binds the following keys:

\\[qflib-make-doc]	Insert an empty javadoc block comment.
\\[qflib-make-method-doc]	Create a javadoc skeleton for the current method.
\\[qflib-make-link]	Create a {@link ...} in a javadoc comment.

\\[qflib-make-log]	Insert a statement skeleton to output a log message.
\\[qflib-make-method-log]	Insert a statement that logs the call to the current method.
\\[qflib-make-exception-log]	Insert a statement that logs an exception.
\\[qflib-make-logger]	Insert a Logger for the current class.

\\[qflib-make-try-wrapper]	Wrap the region from BEG to END into a try/catch block.
\\[qflib-copy-import]	Copy an import statement to the following line and remove the class.
\\[qflib-make-new]	Complete a variable definition with a new clause.
\\[qflib-make-this-assign]	Complete an assignment of a parameter to an identically named member.
"
  (interactive "P")
  (if (not arg)
      (setq arg (not qflib-mode))
    (setq arg (> (prefix-numeric-value arg) 0)))

  (cond
   ((and arg (not qflib-mode))
    ;; Turn on qflib mode
    ;; Make qflib known as a minor mode
    (or (assq 'qflib-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons '(qflib-mode " qflib") minor-mode-alist)))
    ;; Setup `qflib-mode-map'
    (qflib-setup-keymap)
    ;; Add `qflib-mode-map' to `minor-mode-map-alist'
    (if (assq 'qflib-mode minor-mode-map-alist)
	(setcdr (assq 'qflib-mode minor-mode-map-alist) qflib-mode-map)
      (setq minor-mode-map-alist
	    (cons (cons 'qflib-mode qflib-mode-map)
		  minor-mode-map-alist)))
    ;; Start qflib and run qflib-hook
    (setq qflib-mode t)
    (run-hooks 'qflib-mode-hook))
   ((and (not arg) qflib-mode)
    ;; Turn off qflib mode
    (setq qflib-mode nil))))

;;}}}

;;----------------------------------------------------------------------
;; Moving around
;;----------------------------------------------------------------------
;;{{{ qflib-beginning-of-defun

(defun qflib-beginning-of-defun (arg)
"Move backward to the beginning of a Java method definition.
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of method.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "p")
  (let ((pos (point)))
    (while (and (> arg 0)
		(qflib-find-regex qflib-defun-regexp arg "{"))
      (setq arg (1- arg)))
    (while (and (< arg 0)
		(or (end-of-line) t)
		(qflib-find-regex qflib-defun-regexp arg "{"))
      (setq arg (1+ arg)))
    (if (= arg 0)
	(goto-char (match-beginning 0))
      (goto-char pos)
      nil)))

;;}}}
;;{{{ qflib-end-of-defun

(defun qflib-end-of-defun (arg)
"Move forward to next end of defun.
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of defun.

This method may be confused by methods in anonymous inner classes."
(interactive "p")
  (or
   (= arg 0)
   (let ((pos (point))
	 (blp (save-excursion (beginning-of-line) (point)))
	 inside single)
     (save-excursion
       (end-of-line)
       (or (eobp) (forward-char 1))
       (if (qflib-beginning-of-defun 1)
	   (progn
	     (goto-char (1- (match-end 0)))
	     (forward-sexp)
	     (beginning-of-line)
	     (setq single (point))
	     (setq inside (> single blp)))))
     (cond
      ((and inside
	    (= arg 1))
       (goto-char single))
      (t;(> arg 0)
       (setq arg (- (if inside 1 0) arg))
       (end-of-line)
       (or (eobp) (forward-char 1))
       (if (qflib-beginning-of-defun arg)
	   (progn
	     (forward-sexp)
	     (beginning-of-line)
	     (point))
	 (goto-char pos)
	 nil))))))

;;}}}

;;----------------------------------------------------------------------
;; Javadoc utilities
;;----------------------------------------------------------------------
;;{{{ qflib-make-doc

(defun qflib-make-doc ()
  "Insert an empty javadoc block comment."
  (interactive)
  (let (start
	indent-region-function)
    (beginning-of-line)
    (indent-according-to-mode)
    (setq start (point))
    (insert "/**\n* \n*/\n")
    (forward-line 1)
    (indent-region start (point) nil)
    (goto-char start)
    (forward-line 1)
    (end-of-line)))

;;}}}
;;{{{ qflib-make-method-doc

(defun qflib-make-method-doc ()
  "Insert a javadoc comment skeleton before the current Java method.

The skeleton contains a @param line for every argument, a @return line
if the method has a return value and a @throws line for every
exception thrown."
  (interactive)
  (let* ((sig (qflib-get-method-signature))
	 (args (nth 4 sig))
	 (ret (nth 2 sig))
	 (ex (nth 5 sig))
	 indent-region-function ; avoid problems with cc-mode's java-mode
	 )
    (goto-char (car (car sig)))
    (save-excursion
      (insert "\n")
      (forward-char -1)
      (indent-according-to-mode)
      (insert "/**\n* \n")
      (if args
	  (insert "* \n"))
      (while args
	(insert "* @param\t" (cdr (car args)) "\t\n")
	(setq args (cdr args)))
      (if (and ret
	       (not (string= ret "void")))
	  (insert "* \n* @return\t\n"))
      (if ex
	  (insert "* \n"))
      (while ex
	(insert "* @throws\t" (car ex) "\t\n")
	(setq ex (cdr ex)))
      (insert "*/")
      (indent-region (car (car sig)) (point) nil)
      (untabify (car (car sig)) (point)))
    (forward-line 1)
    (end-of-line)))

;;}}}
;;{{{ qflib-make-link

(defun qflib-make-link ()
  "Create a {@link ...} in a javadoc comment.
Point must be after the class or method you want to link to. When
linking to a method you must include the `#' mark and the fully
qualified classname if the method is outside the current class."
  (interactive)
  (let (pos
	var)
    (skip-chars-backward " \t")
    (save-excursion
      (setq pos (point))
      (skip-chars-backward "^ \t")
      (setq var (buffer-substring-no-properties pos (point)))
      (insert "{@link "))
    (insert " ")
    (cond
     ((string-match "^#" var)
      (insert (substring var 1)))
     ((string-match "\\(.*\\.\\)?\\([^\\.]+\\)#" var)
      (insert (substring var (match-beginning 2)
			 (match-end 2))
	      "."
	      (substring var (match-end 0))))
     ((string-match "\\.[^\\.]+$" var)
      (insert (substring var (1+ (match-beginning 0)))))
     (t
      (insert var)))
    (insert "}")))

;;}}}

;;----------------------------------------------------------------------
;; logging (with the de.qfs.lib.log package of qflib)
;;----------------------------------------------------------------------
;;{{{ qflib-make-log

(defun qflib-make-log (level)
  "Insert a statement skeleton to output a log message.
First arg LEVEL is the level to log at, one of the level constants
defined in the de.qfs.lib.log.Log class (e.g. ERR, DBG, etc.)."
  (interactive "sLog level: ")
  (let* ((sig (qflib-get-method-signature))
	 (name (nth 3 sig))
	 (args (nth 4 sig))
	 pos
	 edpos)
    (beginning-of-line)
    (setq pos (point))
    (insert "if (logger.level >= Log." level ") {\n"
	    "logger.log(Log." level ", \"" name "(")
    (while args
      (insert (car (car args)))
      (setq args (cdr args))
      (if args
	  (insert ",")))
    (insert ")\",\n\"")
    (indent-region pos (point) nil)
    (setq edpos (point))
    (insert "\");\n}\n")
    (indent-region pos (point) nil)
    (goto-char edpos)))


;;}}}
;;{{{ qflib-make-method-log

(defun qflib-make-method-log ()
  "Put a statement that logs the call to the current method at the
beginning of the method."
  (interactive)
  (let* ((sig (qflib-get-method-signature))
	 (name (nth 3 sig))
	 (args (nth 4 sig))
	 pos)
    (goto-char (cdr (car sig)))
    (end-of-line)
    (setq pos (point))
    (insert "\nif (logger.level >= Log.MTD) {"
	    "\nlogger.log(Log.MTD, \"" name "(")
    (while args
      (insert (car (car args)))
      (setq args (cdr args))
      (if args
	  (insert ",")))
    (insert ")\",")
    (setq args (nth 4 sig))
    (if (null args)
	(insert " \"\"")
      (insert "\nlogger.level < Log.MTDDETAIL ? \"\" :\n")
      (while args
	(insert "\""
		 (cdr (car args))
		 ": \" + "
		 (cdr (car args)))
	(setq args (cdr args))
	(if args
	    (insert " + \", \" +\n"))))
    (insert ");\n}" )
    (indent-region pos (point) nil)
    (forward-char 1)
    (indent-for-tab-command)))

;;}}}
;;{{{ qflib-make-exception-log

(defun qflib-make-exception-log (level)
  "Insert a statement that logs an exception.
Prefix argument LEVEL is the level to log at.
The name of the exception is determined by searching backward for a
catch clause."
  (interactive "sLog level (ERR): ")
  (let* ((sig (qflib-get-method-signature))
	 (name (nth 3 sig))
	 (args (nth 4 sig))
	 (ex "ex")
	 pos indent-region-function)
    (save-excursion
      (if (re-search-backward
	   (concat
	    "catch[ \t\r\n]*([ \t\r\n]*"
	    "\\(\\w\\|\\s_\\|\\.\\)+[ \t\r\n]+"	; exception class
	    "\\(\\(\\w\\|\\s_\\)+\\)")	; exception name
	    nil t)
	  (setq ex (buffer-substring-no-properties
		    (match-beginning 2)
		    (match-end 2)))))
    (if (string= level "")
	(setq level "ERR"))
    (beginning-of-line)
    (setq pos (point))
    (insert "if (logger.level >= Log." level ") {\n"
	    "logger.log(Log." level ", \"" name "(")
    (while args
      (insert (car (car args)))
      (setq args (cdr args))
      (if args
	  (insert ",")))
    (insert ")\", " ex ");\n}\n")
    (indent-region pos (point) nil)))

;;}}}
;;{{{ qflib-make-logger

(defun qflib-make-logger ()
  "Insert a Logger for the current class.
You can customize the variable `qflib-logger-comment' to get a
different javadoc comment for the logger."
  (interactive)
  (beginning-of-line)
  (let ((pos (point))
	(class (nth 3 (qflib-get-class-signature)))
	indent-region-function)
    (insert
     "/**\n* "
     qflib-logger-comment
     "\n*/\n"
     "private final static Logger logger = new Logger ("
     class
     ".class);\n")
    (indent-region pos (point) nil)))

;;}}}

;;----------------------------------------------------------------------
;; Other utilities
;;----------------------------------------------------------------------
;;{{{ qflib-find-stacktrace-error

(defun qflib-find-stacktrace-error (&optional arg)
  "Locate the source code for a method in a Java stacktrace.
To use this function, the list of source directories to search must be
defined in `qflib-source-directories'.

Non-nil optional prefix argument ARG means diplay the source file in
another window."
  (interactive "P")
  (or qflib-source-directories
      (error "You must define qflib-source-directories first."))
  (save-excursion
    (beginning-of-line)
    (or (looking-at qflib-stacktrace-error-regexp)
	(error "cannot parse stacktrace line")))
  (let* ((package (match-string 2))
	 (class (match-string 3))
	 (file (concat class ".java"))
	 (method (match-string 5))
	 (line (match-string 9))
	 pos dirs tmp)
    (setq pos (string-match "\\." package))
    (while pos
      (setq package
	    (concat (substring package 0 pos) "/"
		    (substring package (1+ pos)))
	    pos (string-match "\\." package)))
    (setq dirs qflib-source-directories)
    (if (catch 'done
	  (while dirs
	    (setq tmp (concat (car dirs) "/" package "/" file))
	    (if (file-exists-p tmp)
		(progn
		  (if arg
		      (find-file-other-window tmp)
		    (find-file tmp))
		  (if line
		      (goto-line (string-to-number line))
		    (if (string= method "<init>")
			(setq method class))
		    (goto-char (point-min))
		    (while (re-search-forward
			    (concat method "[ \t\r\n]*(") nil t)
		      (setq pos (point))
		      (if (and (qflib-beginning-of-defun 1)
			       (> (match-end 0) pos))
			  (throw 'done nil))))
		  (throw 'done nil)))
	    (setq dirs (cdr dirs)))
	  t)
	(error "cannot locate %s/%s" package file))))

;;}}}
;;{{{ qflib-make-try-wrapper

(defun qflib-make-try-wrapper (beg end)
  "Wrap the region from BEG to END into a try/catch block.
BEG and END are modified so the region only contains complete lines."
  (interactive "r")
  (let ((to (make-marker))
	indent-region-function)
    (set-marker to
		(save-excursion
		  (goto-char end)
		  (if (and (bolp)
			   (not (= beg end)))
		      (point)
		    (end-of-line)
		    (1+ (point)))))
    (goto-char beg)
    (beginning-of-line)
    (insert "try {\n")
    (forward-char -1)
    (indent-for-tab-command)
    (indent-region (point) to nil)
    (goto-char to)
    (insert "} catch ( ex) {\n}\n")
    (indent-region (marker-position to) (point) nil)
    (goto-char to)
    (search-forward "(")))

;;}}}
;;{{{ qflib-copy-import

(defun qflib-copy-import ()
  "Copy an import statement to the following line and remove the class.
Point must be placed on a line with an import statement. If you use this
function on a line like
import java.awt.event.KeyEvent;
you'll get the following new line just below it
import java.awt.event.;
with point before the `;' character so you can enter the class name."
  (interactive)
  (let (start tmp)
    (beginning-of-line)
    (setq start (point))
    (forward-line 1)
    (insert (buffer-substring-no-properties start (point)))
    (forward-char -2)
    (setq start (point))
    (backward-word 1)
    (delete-region (point) start)))

;;}}}
;;{{{ qflib-make-new

(defun qflib-make-new ()
  "Complete a variable definition with a new clause.
Point must be placed after the variable name. As an example if you
start a line with
javax.swing.JLabel label
and then call `qflib-make-new', it will be expanded to
javax.swing.JLabel label = new java.awt.JLabel();
with point in between the braces."
  (interactive)
  (let (pos
	type)
  (save-excursion
    (skip-chars-backward " \t")
    (skip-chars-backward "^ \t")
    (skip-chars-backward " \t")
    (setq pos (point))
    (skip-chars-backward "^ \t")
    (setq type (buffer-substring-no-properties pos (point))))
  (if (= (save-excursion (skip-chars-backward " ")) 0)
      (insert " "))
  (insert "= new " type " ();")
  (forward-char -2)))

;;}}}
;;{{{ qflib-make-this-assign

(defun qflib-make-this-assign ()
  "Complete an assignment of a parameter to an identically named member.
Point must be placed after the variable name. This is useful in the
common situation in constructors or setter methods, when you need
assignments in the style
this.name = name;
In such a case simply enter
name
and call `qflib-make-this-assign'."
  (interactive)
  (let (pos
	var)
    (skip-chars-backward " \t")
    (save-excursion
      (setq pos (point))
      (skip-chars-backward "^ \t")
      (setq var (buffer-substring-no-properties pos (point)))
      (insert "this."))
    (insert " = " var ";")))

;;}}}

;;----------------------------------------------------------------------
;; Helper functions
;;----------------------------------------------------------------------
;;{{{ qflib-get-method-signature

(defun qflib-get-method-signature ()
  "Get the signature of a Java method.
When inside a method, get that method's signature, when outside, look
at the following method.

The result is a list of the form
  ((START . END) (QUALIFIERS) RESULT NAME (ARGS) (EXCEPTIONS)), where
- START and END are the start and end positions of the method header
- QUALIFIERS are the method qualifiers like public, static, etc.
- RESULT is the method's result type (nil for a constructor)
- ARGS are the arguments, where each argument is a cons cell with the
  argument's type and name
- EXCEPTIONS are the exceptions thrown by the method"
  (let ((start nil)
	(pos (point))
	found lastpos
	qual lqual ret name args largs throws lthrows
	start end)
    (save-excursion
      (setq found
	    ;; find the matching method header
	    (catch 'found
	      ;; first try: search backwards for method header and see if the
	      ;; method contains point. This may fail to find the correct
	      ;; header if an anonymous inner class with a method definition
	      ;; is located inside the current method before point.
	      (end-of-line)
	      (if (qflib-beginning-of-defun 1)
		  ;; go backwards to next match
		  (save-match-data
		    (save-excursion
		      (goto-char (match-end 0))
		      (backward-char 1)
		      ;; check if the method contains point
		      (forward-sexp 1)
		      (if (>= (point) pos)
			  (throw 'found t)))))

	      ;; Next try: move outwards and then search backwards
	      (condition-case err
		  (progn
		    (goto-char pos)
		    (beginning-of-line)
		    (setq lastpos (point))
		    (while t
		      (goto-char lastpos)
		      (backward-up-list 1)
		      (if (= (char-after) ?{)
			  (progn
			    (setq lastpos (point))
			    (if (and (qflib-beginning-of-defun 1)
				     (= (1- (match-end 0)) lastpos))
				(throw 'found t))))))
		(error nil))

	      ;; finally search forward for the next method
	      (goto-char pos)
	      (or (bobp)
		  (forward-char -1))
	      (qflib-beginning-of-defun -1)))
      (if (not found)
	  (progn
	    (goto-char pos)
	    (error "Cannot find method definition.")))

      ;; get all necessary data from the regexp matches
      (setq start (match-beginning 0)
	    end (match-end 0))
      (if (match-beginning qflib-reindex-qualifiers)
	  (setq qual (buffer-substring-no-properties
		      (match-beginning qflib-reindex-qualifiers)
		      (match-end qflib-reindex-qualifiers))))
      (if (match-beginning qflib-reindex-return)
	  (setq ret (buffer-substring-no-properties
		     (match-beginning qflib-reindex-return)
		     (match-end qflib-reindex-return))))
      (setq name (buffer-substring-no-properties
		  (match-beginning qflib-reindex-name)
		  (match-end qflib-reindex-name)))
      (if (match-beginning qflib-reindex-args)
	  (setq args (buffer-substring-no-properties
		      (match-beginning qflib-reindex-args)
		      (match-end qflib-reindex-args))))
      (if (match-beginning qflib-reindex-throws)
	  (setq throws (buffer-substring-no-properties
			(match-beginning qflib-reindex-throws)
			(match-end qflib-reindex-throws))))

      ;; split qualifiers into tokens
      (while (and qual
		  (string-match "\\w+" qual))
	(setq lqual
	      (cons (substring qual (match-beginning 0) (match-end 0))
		    lqual))
	(setq qual (substring qual (match-end 0))))

      ;; split args into (type . name) pairs
      (while (and args
		  (string-match
		   (concat
		    "\\(\\(\\w\\|\\s_\\|\\.\\)+\\([][ \t]*]\\)?\\)" ;type
		    "[ \t\r\n]+"	; whitespace
		    "\\(\\(\\w\\|\\s_\\)+\\)" ;name
		    ) args))
	(setq largs
	      (cons
	       (cons (substring args (match-beginning 1) (match-end 1))
		     (substring args (match-beginning 4) (match-end 4)))
	       largs))
	(setq args (substring args (match-end 0))))

      ;; split exceptions into tokens
      (while (and throws
		  (string-match "\\(\\w\\|\\s_\\|\\.\\)+" throws))
	(setq lthrows
	      (cons (substring throws (match-beginning 0) (match-end 0))
		    lthrows))
	(setq throws (substring throws (match-end 0))))

      ;; return everything
      (list (cons start end)
	    (nreverse lqual) ret name (nreverse largs)
	    (nreverse lthrows)))))

;;}}}
;;{{{ qflib-get-class-signature

(defun qflib-get-class-signature ()
  "Get the signature of a Java class or interface.
When inside a class, get that class' signature, when outside, look
at the following class.

The result is a list of the form
  ((START . END) (QUALIFIERS) TYPE NAME BASE (INTERFACES)), where
- START and END are the start and end positions of the class header
- QUALIFIERS are the class qualifiers like public, static, etc.
- TYPE is either \"class\" or \"interface\"
- NAME is the name of the class
- BASE is the optional baseclass
- INTERFACES are the optional interfaces"
  (let ((start nil)
	(pos (point))
	found
	qual lqual type name base inter linter
	start end)
    (save-excursion
      ;; find the matching class header
      (end-of-line)
      (if (qflib-find-regex qflib-class-regexp 1 "{")
	  (save-match-data
	    (save-excursion
	      (goto-char (match-end 0))
	      (backward-char 1)
	      (forward-sexp 1)
	      (if (>= (point) pos)
		  (setq found t)))))
      (if (not found)
	  (progn (goto-char pos)
		 (or (bobp)
		     (forward-char -1))
		 (setq found
		       (qflib-find-regex qflib-class-regexp -1 "{"))))
      (if (not found)
	  (progn
	    (goto-char pos)
	    (error "Cannot find class definition.")))
      ;; get all necessary data from the regexp matches
      (setq start (match-beginning 0)
	    end (match-end 0))
      (if (match-beginning qflib-reindex-qualifiers)
	  (setq qual (buffer-substring-no-properties
		      (match-beginning qflib-reindex-qualifiers)
		      (match-end qflib-reindex-qualifiers))))
      (setq type (buffer-substring-no-properties
		  (match-beginning qflib-reindex-type)
		  (match-end qflib-reindex-type)))
      (setq name (buffer-substring-no-properties
		  (match-beginning qflib-reindex-cname)
		  (match-end qflib-reindex-cname)))
      (if (match-beginning qflib-reindex-base)
	  (setq base (buffer-substring-no-properties
		      (match-beginning qflib-reindex-base)
		      (match-end qflib-reindex-base))))
      (if (match-beginning qflib-reindex-inter)
	  (setq inter (buffer-substring-no-properties
			(match-beginning qflib-reindex-inter)
			(match-end qflib-reindex-inter))))

      ;; split qualifiers into tokens
      (while (and qual
		  (string-match "\\w+" qual))
	(setq lqual
	      (cons (substring qual (match-beginning 0) (match-end 0))
		    lqual))
	(setq qual (substring qual (match-end 0))))

      ;; split interfaces into tokens
      (while (and inter
		  (string-match "\\(\\w\\|\\s_\\|\\.\\)+" inter))
	(setq linter
	      (cons (substring inter (match-beginning 0) (match-end 0))
		    linter))
	(setq inter (substring inter (match-end 0))))

      ;; return everything
      (list (cons start end)
	    (nreverse lqual) type name (nreverse linter)))))

;;}}}
;;{{{ qflib-find-regex

(defun qflib-find-regex (regex dir &optional skip)
  "Find the following or previous occurance of REGEX.

If argument DIR is positive, search backward, if it is negative,
search forward. Return non-nil iff REGEX is found.

If optional arg SKIP is non-nil it must be a regular expression
matching text at the end of REGEX. When searching backward, point is
first moved to after the following occurance of SKIP, so that REGEX is
found even if point is currently inside it."
  (let ((oldpos (point))
	end done pos)
    (cond
     ((> dir 0)
      ;; first move outside of a possible method header
      (if (and skip
	       (re-search-forward skip nil t)
	       (not (eobp)))
	  (progn
	    (forward-char 1)
	    ;; search without moving
	    (qflib-find-regex regex dir nil)
	    (if (>= (point) oldpos)
		;; no good, moved too far
		(progn
		  (goto-char oldpos)
		  (qflib-find-regex regex dir nil))
	      t))
	(if (re-search-backward regex nil t)
	    ;; backward search doesn't necessarily match the true beginning,
	    ;; so we have to go back until we find the true start
	    (progn
	      (beginning-of-line)
	      (setq end (match-end 0)
		    pos (point))
	      (while (not done)
		(forward-line -1)
		(if (and (looking-at regex)
			 (= (match-end 0) end))
		    ;; still inside the same method definition
		    (setq pos (point))
		  ;; moved too far
		  (setq done t)))
	      (goto-char pos)))))
     ((< dir 0)
      (re-search-forward regex nil t))
     (t
      t))))

;;}}}

;;{{{ qflib-setup-keymap

(defun qflib-setup-keymap ()
  "Setup key-bindings for Generic Interpreter Mode in keymap `qflib-mode-map'.

If `qflib-mode-map' is non-nil, it is left unchanged"
  (if (or t (not qflib-mode-map)) ;;XXX
      (progn
	(setq qflib-mode-map (make-sparse-keymap))
	(cond
	 ((stringp qflib-prefix-key)
	  (define-key qflib-mode-map (concat qflib-prefix-key ".")
	    'qflib-copy-import)
	  (define-key qflib-mode-map (concat qflib-prefix-key "\C-d")
	    'qflib-make-doc)
	  (define-key qflib-mode-map (concat qflib-prefix-key "d")
	    'qflib-make-method-doc)
	  (define-key qflib-mode-map (concat qflib-prefix-key "e")
	    'qflib-make-exception-log)
	  (define-key qflib-mode-map (concat qflib-prefix-key "g")
	    'qflib-make-logger)
	  (define-key qflib-mode-map (concat qflib-prefix-key "h")
	    'qflib-make-this-assign)
	  (define-key qflib-mode-map (concat qflib-prefix-key "k")
	    'qflib-make-link)
	  (define-key qflib-mode-map (concat qflib-prefix-key "l")
	    'qflib-make-log)
	  (define-key qflib-mode-map (concat qflib-prefix-key "m")
	    'qflib-make-method-log)
	  (define-key qflib-mode-map (concat qflib-prefix-key "n")
	    'qflib-make-new)
	  (define-key qflib-mode-map (concat qflib-prefix-key "t")
	    'qflib-make-try-wrapper)
	  )
	 ((and qflib-prefix-key
	       (symbolp qflib-prefix-key))
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key
					      'control ?d)))
	    'qflib-make-doc)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?.)))
	    'qflib-copy-import)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?d)))
	    'qflib-make-method-doc)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?e)))
	    'qflib-make-exception-log)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?g)))
	    'qflib-make-logger)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?h)))
	    'qflib-make-this-assign)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?k)))
	    'qflib-make-link)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?l)))
	    'qflib-make-log)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?m)))
	    'qflib-make-method-log)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?n)))
	    'qflib-make-new)
	  (define-key qflib-mode-map
	    (vector (event-convert-list (list qflib-prefix-key ?t)))
	    'qflib-make-try-wrapper)
	 ))
	;; Define a menu for the menu bar
	(if window-system
	    (let ((menu (make-sparse-keymap  "qflib")))
	      ;; Set up menu commands bottom up
	      (define-key menu [this-assign]
		'("Complete this assign" . qflib-make-this-assign))
	      (define-key menu [new-assign]
		'("Complete new assign" . qflib-make-new))
	      (define-key menu [copy-import]
		'("Copy import statement" . qflib-copy-import))
	      (define-key menu [try-wrapper]
		'("Surround with try/catch" . qflib-make-try-wrapper))
	      (define-key menu [sep2] '("--"))
	      (define-key menu [logger]
		'("Insert Logger" . qflib-make-logger))
	      (define-key menu [exception-log]
		'("Insert exception log" . qflib-make-exception-log))
	      (define-key menu [method-log]
		'("Insert method log" . qflib-make-method-log))
	      (define-key menu [log]
		'("Insert log" . qflib-make-log))
	      (define-key menu [sep] '("--"))
	      (define-key menu [link]
		'("Insert javadoc link" . qflib-make-link))
	      (define-key menu [method-doc]
		'("Insert method doc" . qflib-make-method-doc))
	      (define-key menu [doc]
		'("Insert javadoc block" . qflib-make-doc))
	      ;; Put the menu keymap into `qflib-mode-map'
	      (define-key qflib-mode-map [menu-bar qflib]
		(cons "qflib" menu)))))))

;;}}}

;;{{{ XEmacs menu

(defvar qflib-menu
  (list "qflib"
	["Insert javadoc block"     qflib-make-doc t]
	["Insert method doc"        qflib-make-method-doc t]
	["Insert javadoc link"      qflib-make-link t]
	"--"
	["Insert log"     qflib-make-log  t]
	["Insert method log"     qflib-make-method-log  t]
	["Insert exception log"     qflib-make-exception-log  t]
	["Insert logger"     qflib-make-logger  t]
	"--"
	["Surround with try/catch"     qflib-make-try-wrapper  t]
	["Copy import statement"     qflib-copy-import  t]
	["Complete new assign"     qflib-make-new  t]
	["Complete this assign"     qflib-make-this-assign  t]
	)
  "Menu for qflib.")

(defun qflib-insert-menu-in-XEmacs-menubar ()
  "Insert qflib menu in the XEmacs menu bar."
  (if (and
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil qflib-menu)
	(add-menu nil "qflib" (cdr qflib-menu)))))

;;}}}

(provide 'qflib)

;;; qflib.el ends here -------------------------------------------------