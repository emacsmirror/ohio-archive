;;; jde.el -- Integrated Development Environment for Java.
;; $Revision: 1.129 $ $Date: 2000/07/29 03:18:41 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 1999 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:


;;;###autoload
(defconst jde-version "2.2.0"
  "JDE version number.")


(defconst jde-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defconst jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))

(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cus-edit)
(require 'jde-compile)
(require 'jde-db)
(require 'jde-run)
(require 'jde-make)
(require 'jde-gen)
(require 'compile)
(require 'imenu)
(require 'speedbar)
(require 'browse-url)
(require 'beanshell)
(require 'jde-wiz)
(require 'jde-parse)
(require 'jde-help)
(require 'jde-bug)
(require 'jde-complete)
(require 'jde-javadoc)
(require 'jde-stat)

;; This is copied straight out of andersl-java-font-lock.el
;; Necessary to set here because andersl assumes that the
;; buffer is in java-mode (it is actually in jde-mode).
(defun setup-fontlock()
  (if (not (assq 'jde-mode font-lock-defaults-alist))
      (setq font-lock-defaults-alist
	    (cons
	     (cons 'jde-mode
		   ;; jde-mode-defaults
		   '((java-font-lock-keywords java-font-lock-keywords-1
		      java-font-lock-keywords-2 java-font-lock-keywords-3)
		     nil nil ((?_ . "w") (?$ . "w")) nil
		     (font-lock-mark-block-function . mark-defun)))
	     font-lock-defaults-alist))))

(cond ((not jde-xemacsp)
       (if (< emacs-major-version 20)
	   (require 'andersl-java-font-lock))
       (setup-fontlock)))

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup jde nil
  "Java Development Environment"
  :group 'tools
  :prefix "jde-")

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-")


;; (makunbound 'jde-key-bindings)
(defcustom jde-key-bindings
  (list 
   (cons "[?\C-c ?\C-v ?\C-a]" 'jde-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'jde-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'jde-compile)
   (cons "[?\C-c ?\C-v ?\C-d]" 'jde-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'jde-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"     'jde-javadoc-generate-javadoc-template)
   (cons "[?\C-c ?\C-v ?\C-k]" 'bsh)
   (cons "[?\C-c ?\C-v ?\C-l]" 'jde-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'jde-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'jde-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'jde-wiz-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'jde-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'jde-db-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'jde-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-y]" 'jde-show-class-source)
   (cons "[?\C-c ?\C-v ?\C-z]" 'jde-wiz-find-and-import)
   (cons "[(control c) (control v) (control ?.)]" 'jde-complete-at-point-menu)
   (cons "[(control c) (control v) ?.]" 'jde-complete-at-point)
   )
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
		(string :tag "Key")
		(function :tag "Command")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (when (boundp 'jde-mode-map)
		 ;; Unmap existing key bindings
		 (if (and (boundp 'jde-key-bindings)
			  jde-key-bindings)
		     (mapc 
		      (lambda (binding)
			(let ((key (car binding)))
			  (if (string-match "\\[.+]"key)
			      (setq key (car (read-from-string key))))
			  (local-unset-key key)))
		      jde-key-bindings))
		 ;; Map new key bindings.
		 (mapc 
		  (lambda (binding)
		    (let ((key (car binding))
			  (fcn (cdr binding)))
		      (if (string-match "\\[.+]" key)
			  (setq key (car (read-from-string key))))
		      (define-key (current-local-map) key fcn)))
		  val))))
	   (jde-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jde-project-context-switching-enabled-p t
  "*Enable project context switching.
If non-nil, the JDE reloads a buffer's project file when you switch to the buffer from
another buffer belonging to another project. You can disable this feature if you prefer
to load project files manually. The debugger uses this variable to disable context-switching
temporarily when stepping through code."
  :group 'jde-project
  :type 'boolean
)

;; Used by debugger to disable context-switching temporarily while 
;; stepping through code.
(setq jde-project-cs-enabled-p t)

(defcustom jde-jdk-doc-url "http://www.javasoft.com/products/jdk/1.1/docs/index.html"
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. By
default, this variable points to the copy stored at JavaSoft's
website."
  :group 'jde-project
  :type 'string)

(makunbound 'jde-global-classpath)
(defcustom jde-global-classpath nil
  "Specify a common classpath for compile, run, and debug commands.
Use this variable if you want to the JDE to use the same classpath for
compiling, running,and debugging an application. If you want to use
different classpaths for these operations, use
`jde-compile-option-classpath' to specify the compilation classpath,
`jde-run-option-classpath' to specify the run classpath, and/or
`jde-db-option-classpath' to specify the debug classpath. You can use
these variables together. For example, suppose that you need to use
one classpath for compilation and other for running and debugging. You
could do this by setting `jde-compile-option-classpath' to the compile
classpath and `jde-global-classpath' to the run and debug
classpath. If you set `jde-global-classpath', the JDE uses it to
construct the classpath for any operation for which you do not set the
operation-specific classpath variable (e.g.,
`jde-compile-option-classpath'). If you do not set
`jde-global-classpath', the JDE uses the operation-specific classpath
if it is set. If neither the global nor the operation-specific
classpath is set, the JDE does not generate a -classpath argument for
the operation. Note: the JDE does not support use of environment variables
in classpaths."
  :group 'jde-project
  :type '(repeat (string :tag "Path")))

(defcustom jde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jde-project
  :type 'boolean)

(defvar jde-project-name "default"
"Specifies name of project to which the current buffer belongs.")

(defcustom jde-project-file-name "prj.el"
  "*Specify name of JDE project file.
When it loads a Java source file, the JDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDE options on a
project-by-project basis."
  :group 'jde-project
  :type 'string)

(defcustom jde-use-font-lock t
  "*Turn on font-locking if on.
	Set to nil to disable the use of font-locking."
  :group 'jde-project
  :type 'boolean)


(defcustom jde-compiler "javac"
  "*Java compiler.
Specifies the path to the compiler to be used to compile the source
in the current buffer. The default is the JDK compiler (javac)."
  :group 'jde-project
  :type 'string)

(defcustom jde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-project
  :type 'boolean
)

(defvar jde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")


(defcustom jde-build-use-make nil
"*If true, use make to build JDE projects."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jde-mode.
See `jde-mode-abbreviations' for more information."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "throw" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Java keywords.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
   :group 'jde-project
  :type '(repeat 
	  (cons :tag "jde-mode abbreviation"
		(string :tag "Abbreviation")
		(string :tag "Expansion"))))

(defvar jde-mode-abbrev-table nil
  "Abbrev table for use in JDE-mode buffers.")


(defun jde-init-abbrev-table ()
  "Load the abbrev table with a set of abbrevs that invoke an anonymous
function that  does the expansion only if point is not in a quoted string 
or a comment."

  ;; Note the use of lexical-let - must have the common lisp packages
  ;; around, since the anonymous function needs the closure provided by
  ;; lexical-let.
  (interactive)
  (setq local-abbrev-table (make-abbrev-table))
  (mapc 
   (lambda (x)
     (lexical-let
	 ((abbrev (car x))		; this is the abbrev, lexically scoped
	  (expansion (cdr x)))		; this is the expansion
       (define-abbrev 
	 local-abbrev-table
	 abbrev
	 ""
	 (lambda ()
	   (if (jde-parse-comment-or-quoted-p)
	       (insert abbrev)		; insert the abbrev in quote/comment
	     (insert expansion)))       ; proceed with expansion elsewhere
	 0)))
   jde-mode-abbreviations)
  (jde-gen-load-cflow-abbrevs)
  (setq abbrevs-changed nil))

;;;###autoload
(defun jde-set-compiler (compiler)
  "Specify the pathname of the compiler to be used to compile the
current buffer. Default is javac."
  (interactive
   "sEnter compiler (javac): ")
   (if (string= compiler "")
       (setq jde-compiler "javac")
     (setq jde-compiler compiler)))

(defvar jde-classpath-separator (if (eq system-type 'cygwin32) 
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")

(defun jde-path-string-to-list (paths)
 "Converts a string of paths to a list of paths.
It is assumed that the default path separator for the
current platform (e.g., semicolon on Win32) separates
the paths."
 (let ((path-list (list))
       (m 0)
       (n (string-match jde-classpath-separator paths)))
   (while n
     (let ((path (substring paths m n)))
       (if path
	   (setq path-list
		 (cons path path-list)))
       (setq m (+ n 1))
       (setq n (string-match jde-classpath-separator paths m))))
   (setq n (length paths))
   (if (and (> n 0) (< m n))
       (let ((path (substring paths m n)))
	 (if path
	     (setq path-list
		   (cons path path-list)))))
   (setq path-list (nreverse path-list))))

;;;###autoload
(defun jde-set-global-classpath (classpath)
  "Specify the value of the -classpath argument for the Java compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (setq jde-global-classpath (jde-path-string-to-list classpath)))


(defun jde-build-compile-vm-args ()
  (let ((args " ")
	(len (length jde-compile-option-vm-args))
	(n 0))
    (while (< n len)
      (setq args (concat " -J"
			 (elt jde-compile-option-vm-args n)))
      (setq n (1+ n)))
    args))

;;;###autoload
(defun jde-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (if (file-exists-p jde-jdk-doc-url)
      (browse-url jde-jdk-doc-url browse-url-new-window-p)
    (error "The JDK documentation file, %s, does not exist." jde-jdk-doc-url)))

(defun jde-make-compile-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-compiler " " 
	  (jde-get-compile-options) 
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "
	  (file-name-nondirectory buffer-file-name)))


(defun jde-show-compile-options ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))

(defun jde-show-run-options ()
  "Show the JDE Run Options panel."
  (interactive)
  (customize-apropos "jde-run-options" 'groups))

(defun jde-show-debug-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-db-options" 'groups))

(defun jde-show-project-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-project" 'groups))

(defun jde-show-autocode-options ()
  "Show the JDE Autocode panel."
  (interactive)
  (customize-apropos "jde-gen" 'groups))


;;;###autoload
(defun jde-java-build ()
  "Use javac -depend to build the application whose main class is
specified by `jde-run-application-class'."
  (interactive)
  (cond 
   ((string= jde-run-application-class "")
    (message "No application main class specified."))
   (t
    (string-match "\\(\\(\\w*\\.\\)*\\)\\(\\w*\\b\\)"
		jde-run-application-class)
    (let* ((b1 (match-beginning 1))
	   (e1 (match-end 1))
	   (b2 (match-beginning 3))
	   (e2 (match-end 3))
	   (file (concat
		  (substring jde-run-application-class b2 e2)
		  ".java"))
	   (package (if e1
			(substring jde-run-application-class b1 e1)))
	   (directory (jde-db-search-src-dirs file package)))
      (cond
       (directory
	(let ((file-path 
	       (concat directory 
		       file))
	      (save-depend jde-compile-option-depend))
	  (find-file file-path)
	  (setq jde-compile-option-depend t)
	  (jde-compile)
	  (setq jde-compile-option-depend save-depend)))
       (t
	(message (concat "Could not find source for "
			 jde-run-application-class))))))))
    
;;;###autoload
(defun jde-build ()
  "Rebuild the entire project.
This command has two operating modes: java and make. In java mode,
this command uses javac's built-in make facility to rebuild a
project. In make mode, this command uses a user-specified make program
to rebuild the project. JDE configuration variables control which mode
is used.  In particular, if the variable `jde-build-use-make' is
non-nil, this command invokes the make program specified by the
variable `jde-make-program'. If the variable `jde-make-args' is a
non-empty string, this function uses its contents to invoke make;
otherwise, it prompts you to enter command-line arguments for make. If
`jde-build-use-make' is nil, this function invokes javac on the source
file specified by `jde-run-app-class', with the -depend option. This
causes javac to recompile all missing or out-of-date files required
to run the application's main class."
  (interactive)
  (if jde-build-use-make
      (jde-make)
    (jde-java-build)))

(defun jde-mode-internal () 
 ;; Define buffer-local variables.
  (make-local-variable 'jde-project-name)
  (make-local-variable 'jde-run-applet-document)

  ;; Load the project file for this buffer. The project file
  ;; defines JDE options for a project.
  (if (not (jde-debugger-running-p))
	   (jde-load-project-file))

;;  (make-local-variable 'jde-force-project-file-reload-p)
;;  (setq jde-force-project-file-reload-p nil)

  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Java buffer belonging to one project
  ;; to a buffer belonging to another.
  (make-local-hook 'post-command-hook)
  (unless (find 'jde-detect-java-buffer-activation post-command-hook)
    (add-hook 'post-command-hook 'jde-detect-java-buffer-activation nil t))

  (if jde-xemacsp
      (jde-insert-menu-in-XEmacs-menubar))

  (if jde-use-font-lock
      (jde-setup-syntax-coloring))

  ;; Define underscore as a word constituent. This is needed
  ;; to support coding styles the begin fields with an underscore.
  (modify-syntax-entry ?_ "w")

  (setq jde-current-project jde-project-name)

  (when jde-enable-abbrev-mode
     ;; Define abbreviations.
    (jde-init-abbrev-table)
    (abbrev-mode 1))

  ;; Reset the key bindings in case jde-mode-keymap
  ;; was not bound at startup.
  (custom-initialize-reset 'jde-key-bindings nil)

  (if (and
       jde-setnu-mode-enable
       (< (point-max) jde-setnu-mode-threshold))
      (setnu-mode 1))

  (if (string= (car jde-db-debugger) "JDEbug")
      (jde-bug-install-jdebug-menu))
  
)

;; This is actually a no-op to get jde auto-loaded.
;;;###autoload
(defun jde-mode ()
  "Major mode for developing Java applications and applets."
  nil)

(define-derived-mode 
  jde-mode java-mode "JDE"
  "Major mode for developing Java applications and applets.
  \\{jde-mode-map}"

  (jde-mode-internal)
)


(defun jde-setup-syntax-coloring() 
  ;; Set up syntax coloring.
  (cond (window-system

	 ;; If not XEmacs 20.1 turn on font lock.
	 ;; (XEmacs 21 has font-lock on by default.)
	 (if (or
	      (not jde-xemacsp)
	      (not
	       (and
		(eq emacs-major-version 21)
		(eq emacs-minor-version 0))))
	     (turn-on-font-lock))

	 (setq font-lock-maximum-decoration t)

	 (if (not jde-xemacsp)
	     (global-font-lock-mode 1))
	 )))

;; Setup jde-mode for font locking.
(if jde-xemacsp
    (put 'jde-mode 'font-lock-defaults
	 '((java-font-lock-keywords
	    java-font-lock-keywords-1 java-font-lock-keywords-2
	    java-font-lock-keywords-3)
	   nil nil ((?_ . "w")) beginning-of-defun)))

;; Make jde-mode the default mode for Java source code buffers.
;; Prepend the jde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(setq auto-mode-alist
  (append
   '(("\\.java\\'" . jde-mode))
	auto-mode-alist))

(defvar jde-menu 
  (list "JDE"
	["Compile"           jde-compile t]
	;; ["Run App"           jde-run (not (jde-run-application-running-p))]
	["Run App"           jde-run t]
	["Debug App"         jde-debug t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        jde-run-menu-run-applet t]
	["Debug Applet"      jde-db-menu-debug-applet t]
	"-"  
	["Build"             jde-build t]
	["Interpret"         bsh t]
	["Document"          jde-javadoc-generate-javadoc-template t]
        "-" 
	(list "Templates"
	      ["Get/Set Pair..."  jde-gen-get-set t]
	      ["Println..."       jde-gen-println t]
	      (list "Listener"
		    ["Action"          jde-gen-action-listener t]
		    ["Window"          jde-gen-window-listener t]
		    ["Mouse"           jde-gen-mouse-listener t]
		    )
	      ["Other..."        jde-gen-code t]
	      )
	(list "Wizards"
	      ["Import class"        jde-wiz-find-and-import t]
	      ["Override Method"     jde-wiz-override-method t]
	      ["Implement Interface" jde-wiz-implement-interface t]
	      ["Delegate Methods"    jde-wiz-delegate t]
              "-"
	      ["Update Class List"   jde-wiz-update-class-list t]
	      )
	["Speedbar"          speedbar-frame-mode t]
	(list "Project"
	      (list "Options"
		    ["General"         jde-show-project-options t]
		    ["Compile"         jde-show-compile-options t]
		    ["Run"             jde-show-run-options t]
		    ["Debug"           jde-show-debug-options t]
		    ["Autocode"        jde-show-autocode-options t]
		    ["Javadoc"         jde-javadoc-customize t]
		    )
	      (list "Project File"
		    ["Save"     jde-save-project t]
		    ["Load"     jde-load-project-file t]
		    ["Load All" jde-load-all-project-files t]
		    )
	      )
	(list "Help"
	      ["JDE Users Guide"       jde-show-help t]
	      ["JDK"                   jde-browse-jdk-doc t]
	      ["Symbol at point"       jde-help-symbol t]
	      "-"
	      ["Submit problem report" jde-submit-problem-report t]
	      "-"
	      (concat "JDE " jde-version)
	      )
	)
  "Menu for JDE.")

;; Define JDE menu for FSF Emacs.
(if (or (not jde-xemacsp) (featurep 'infodock))
    (easy-menu-do-define 'jde-menu1 
			 jde-mode-map
			 "Menu for JDE."
			 jde-menu))

(defun jde-insert-menu-in-XEmacs-menubar ()
  "Insert JDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil jde-menu)
	(add-menu nil "JDE" (cdr jde-menu)))))


(defvar jde-new-buffer-menu
  (list
   "JDE New"
   ["Class..."         jde-gen-class-buffer t]
   ["Console..."       jde-gen-console-buffer t]
   ["Other..."         jde-gen-buffer t]
   )
  "Menu for creating new Java buffers.")

;; Add JDE New menu to Emacs Files menu.
(if (not jde-xemacsp)
    (let* ((mb (assq 'menu-bar global-map))
	   (files (assq 'files mb))
	   (menu (if (fboundp 'easy-menu-create-menu)
		     (easy-menu-create-menu 
		      (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))
		   (easy-menu-create-keymaps 
		    (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))))     
	   (menu-name (car jde-new-buffer-menu)))
      (define-key-after (cdr (cdr files)) [jde-new]
	(cons menu-name menu)
	'open-file))
  (unless (featurep 'infodock)
    (add-submenu '("File") jde-new-buffer-menu "Insert File...")))



;; Project File Functions


(defun jde-convert-cygwin-path (path &optional separator)
  "Convert cygwin style PATH to a form acceptable to java vm.  Basiclally
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (let* ((path-re "/\\(cygdrive\\)?/\\([a-zA-Z]\\)/")
         (subexpr 2)
         (index1 (* 2 subexpr))
         (index2 (1+ index1)))
    (setq path 
          (if (string-match (concat "^" path-re) path)
              (concat (substring path 
                                 (nth index1 (match-data)) 
                                 (nth index2 (match-data)))
                      ":/" 
                      (substring path (match-end 0)))
            path))
    (if separator
        (while (string-match (concat separator path-re) path)
          (setq path 
                (concat (substring path 0 (match-beginning 0))
                        separator
                        (substring path 
                                 (nth index1 (match-data)) 
                                 (nth index2 (match-data)))
                        ":/" 
                        (substring path (match-end 0))))))
    path))


(defun jde-root-dir-p (dir)
  (let ((parent (concat dir "../")))
    (cond ((eq system-type 'windows-nt)
           (not (file-exists-p parent))
           )
          ((eq system-type 'cygwin32)
           (or (string= (file-truename dir) "/") 
               (not (file-exists-p (file-truename dir))))
           )
          (t
           (and 
            (string= (file-truename dir) "/")
            (string= (file-truename parent) "/"))))))



(defun jde-find-project-file (dir)
  "Finds the project file for the Java source file in the current
buffer. Returns nil if it cannot find a project file in the
source file directory or an ascendant directory."
  (let ((file (find jde-project-file-name
		    (directory-files dir) :test 'string=)))
    (if file
	(concat dir file)
      (if (not (jde-root-dir-p dir))
	  (jde-find-project-file (concat dir "../"))))))

(defun jde-load-project-file ()
  "Loads the project file for the Java source file in the current
buffer. Searches for the project file first in the buffer directory,
then in ascendant directories. Uses the first file that it encounters.
If no project file is found, set each JDE variable to the value defined
in your .emacs file or, if your .emacs file does not define a value, to
the value defined by the JDE."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(load-file prj-file)
      (jde-set-variables-init-value))))


(defun jde-load-all-project-files ()
  (interactive)
  "Loads the project file associated with each Java source buffer."
  (mapc
   (lambda (java-buffer)
     (save-excursion
       (set-buffer java-buffer)
       (message "Loading project file for %s ..." 
		(buffer-file-name java-buffer))
       (jde-load-project-file)))
   (jde-get-java-source-buffers)))

;;;###autoload
(defun jde-open-project-file ()
  "Opens the project file for the Java source file in the
current buffer."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun jde-save-delete (symbol)
  "Delete the call to SYMBOL from project file.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (let ((project-file (or
			 (jde-find-project-file default-directory)
			 (concat "./" jde-project-file-name))))
      (set-buffer (find-file-noselect project-file)))

    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))

(defvar jde-symbol-list nil
  "A list of jde variables which are processed by jde-save-variables")

(defun jde-symbol-list ()
  "Return a list of variables processed by jde-save-variables.
The first time this is called, the list is saved in jde-symbol-list"
  (or jde-symbol-list
      (mapatoms
       (lambda (symbol)
         (if (and (string-match "jde-" (symbol-name symbol))
                  (get symbol 'custom-type))
             (setq jde-symbol-list (cons symbol jde-symbol-list))))))
  jde-symbol-list)


(defun jde-save-variables ()
  "Save all JDE variables in project file."
  (jde-save-delete 'jde-set-project-name)
  (jde-save-delete 'jde-set-variables)
  (let ((standard-output (get-buffer jde-project-file-name)))
    (unless (bolp)
      (princ "\n"))

    (princ "(jde-set-project-name ")
    (prin1 jde-project-name)
    (princ ")\n")

    (princ "(jde-set-variables ")
    (mapcar
     (lambda (symbol)
       (when 
	   (and (string-match "jde-" (symbol-name symbol))
		(get symbol 'custom-type))
	 (let ((value (symbol-value symbol)))	   
	     (princ "\n '(")
	     (princ symbol)
	     (princ " ")
	     (prin1 (custom-quote value))
	     ;; Check whether the user has changed the value of this
	     ;; variable in a customization buffer. If so, save flag
	     ;; so that custom knows that this value differs from
             ;; standard value.
	     (if (get symbol 'customized-value)
		 (princ " t)")
	       (princ ")"))		 
	     )))
	(jde-symbol-list))
      (princ ")")
      (save-excursion
	(set-buffer (get-buffer jde-project-file-name))
	(unless (looking-at "\n")
	  (princ "\n"))
	(save-buffer))
      (kill-buffer (get-buffer jde-project-file-name))))

(defun jde-set-project-name (name)
  (setq jde-project-name name))

(defun jde-set-variables (&rest args)
  "Initialize JDE customization variables.  

Takes a variable number of arguments. Each argument 
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
"
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (get symbol 'custom-set) 'set-default)))
	    (if customized
		(put symbol 'customized-value (list value)))
	    (when (default-boundp symbol)
		   ;; Something already set this, overwrite it
		   (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defun jde-set-variables-init-value ()
  "Set each JDE variable to the value it has at Emacs startup."
  (interactive)
  (message "Setting JDE variables to startup values...")
  (mapcar 
   (lambda (symbol) 
     (when 
	 (and (string-match "jde-" (symbol-name symbol))
	      (get symbol 'custom-type))
       (let ((saved-val (get symbol 'saved-value))
	     (std-val (get symbol 'standard-value))
	     (set (or (get symbol 'custom-set) 'set-default)))
	 (if saved-val
	     (funcall set symbol (eval (car saved-val)))
	   (funcall set symbol (eval (car std-val)))))))
	(jde-symbol-list)))
 
;;;###autoload
(defun jde-save-project (proj-name)
  "Saves local source file buffer options in project file.
This command provides an easy way to create and update a
project file for a Java project. Simply open a source file,
set the desired options, using the JDE Options menu, then
save the settings in the project file, using this command.
Now, whenever you open a source file from the same directory
tree, the saved settings will be restored for that file."
  (interactive
   (list 
    (let (prompt)
      (if (string= jde-project-name "")
	  (setq prompt "Enter project name: ")
	(setq prompt
	      (format "Enter project name (%s): " 
		      jde-project-name)))
      (read-string prompt))))
  (unless (string= proj-name "")
      (setq jde-project-name proj-name))
  (jde-save-variables))

(defun jde-convert-prj-file (file) 
"Converts a pre-JDE-2.0.7 project file to JDE-2.0.7 format.
Note: old project files did not preserve information about 
whether a saved value differed from the standard (JDE-defined)
value of a variable. Thus, all values are saved in the
converted file as though they were standard values. This means
that when JDE reloads the file, a custom buffer will customized
values as though they were standard. If you want to restore
a customized value to a standard value, simply make some
innocuous edit to the customized value and choose 
'Set for current session' from the customization buffer's
Set menu. Custom will then enable the Set menu option that
allows you to restore the value to its default value."
  (interactive "F")
  (let ((olddef (symbol-function 'jde-set-variables))
	(newdef 
	 (lambda (&rest args)
	   (while args 
	     (let ((entry (car args)))
	       (if (listp entry)
		   (let* ((symbol (nth 0 entry))
			  (value (nth 1 entry))
			  (set (or (get symbol 'custom-set) 'set-default)))
		     (when (default-boundp symbol)
		       ;; Something already set this, overwrite it
		       (funcall set symbol value))
		     (setq args (cdr args)))))))))
    (defalias 'jde-set-variables newdef)
    (require 'cus-edit)
    (load-file file)
    (jde-save-project jde-project-name)
    (defalias 'jde-set-variables olddef)))

;; Code to update JDE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(setq jde-current-project "")

(defun jde-reload-project-file ()
  "If project context-switching is enabled 
(see `jde-project-context-switching-enabled-p'),
reloads the project file for a newly activated Java buffer when
the new buffer's project differs from the old buffer's."
  (interactive)
  (if  (and
	jde-project-context-switching-enabled-p
        (not (jde-debugger-running-p))
	(not (string= jde-current-project jde-project-name)))
      (progn
	(setq jde-current-project jde-project-name)
	(jde-load-project-file))))

(defun jde-debugger-running-p () 
  (and 
   (jde-dbs-debugger-running-p)
   (jde-dbs-get-target-process)))

(defcustom jde-entering-java-buffer-hooks '(jde-reload-project-file)
"*Lists functions to run when entering a Java source buffer"
  :group 'jde-project
  :type 'hook)


(setq jde-current-buffer (current-buffer))

(defun jde-detect-java-buffer-activation ()
"Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the 
`jde-entering-java-buffer' hooks."
  (let ((curr-buff (current-buffer)))
    (if (not
	 (equal curr-buff jde-current-buffer))
	(progn
	  (setq jde-current-buffer curr-buff)
	  (if (eq major-mode 'jde-mode)
		(run-hooks 'jde-entering-java-buffer-hooks))))))


(defun jde-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (count 
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (string-match file-type file-name))))))
	 

(defun jde-remove-jde-hook ()
  "Removes `jde-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (unless (> (jde-count-open-java-buffers) 1)
  (remove-hook 'post-command-hook 'jde-detect-java-buffer-activation)))

(add-hook 'kill-buffer-hook 'jde-remove-jde-hook)


;; JDE help

(defun jde-find-jde-data-directory ()
  "Return the path of the JDE data directory.
Returns the path of the directory containing the
JDE java and documentation directories;  nil if the 
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions, 
the JDE expects to find the documentation and Java class directories
in the same directory that contains the JDE lisp directory."
  (let (dir)
    (if jde-xemacsp
	(progn
	  (setq dir (locate-data-directory "jde"))
	  (when (not dir)
	      (setq dir (file-name-directory (locate-library "jde")))
	      (setq dir (substring dir 0 (- (length dir) 5)))))
      (setq dir (file-name-directory (locate-library "jde"))))
    (if dir
	(nsubstitute ?/ ?\\ dir))
    (if (not jde-xemacsp)
	(setq dir (substring dir 0 (- (length dir) 5))))
    dir))

(defun jde-find-jde-doc-directory ()
  "Return the path of the JDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
jde.el."
  (jde-find-jde-data-directory))

;;;###autoload
(defun jde-show-help ()
  "Displays the JDE User's Guide in a browser."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
         (jde-help
          (if jde-dir
              (if (and jde-xemacsp
                       (locate-data-directory "jde"))
                  (expand-file-name "jde-ug.html" jde-dir)
                (expand-file-name "doc/ug/jde-ug.html" jde-dir)))))       
    (if (and
         jde-help
         (file-exists-p jde-help))

        (browse-url (concat "file://" (jde-convert-cygwin-path jde-help))
                    browse-url-new-window-p)
      (signal 'error '("Cannot find JDE help file.")))))

(defun jde-debug ()
"*Runs the debugger specified by `jde-db-debugger'."
  (interactive)
  (if (string= (car jde-db-debugger) "JDEbug")
      (jde-bug-debug-app)
    (jde-db)))

;;
;; Problem reporting functions contributed by Phillip Lord <plord@hgmp.mrc.ac.uk>.
;;
(defvar jde-problem-report-mail-address "pkinnucan@mediaone.net" )

(defun jde-submit-problem-report()
  "Submit a problem report for the JDE" 
  (interactive)
  (require 'reporter)
  (and 
   (y-or-n-p "Do you want to submit a problem report on the JDE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      jde-problem-report-mail-address
      (concat "JDE version " jde-version)
      (jde-problem-report-list-all-variables)
      nil
      'jde-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))


(defun jde-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a JDEBug buffer and inserts the contents of that, and then prompts 
for insertion of the .emacs file"
  (save-excursion 
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*JDEbug*"))
	  (messages-buffer 
	   (get-buffer
	    (if jde-xemacsp " *Message-Log*" "*Messages*")))
	  (backtrace-buffer (get-buffer "*Backtrace*"))
	  (process 
	   (let ((proc (jde-dbs-get-target-process)))
	     (if (not proc)
		 (let ((dead-proc-alist 
			(oref jde-dbs-the-process-morgue proc-alist)))
		   (if dead-proc-alist
		       (setq proc (cdr (car dead-proc-alist))))))
	     proc))
	  (cli-buffer (if (and process (slot-boundp process 'cli-buf))
			(oref process cli-buf)))
	  (locals-buffer (if (and process (slot-boundp process 'locals-buf))
			(oref process locals-buf)))
	  )

      ;;insert the contents of the debug buffer if it is there. 
      (if debug-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *JDEBug* buffer were\n\n")
	    (insert-buffer debug-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *JDEbug* buffer" ))
	(insert-string "\n\n\nThere was no *JDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the CLI buffer are\n\n")
	    (insert-buffer cli-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert CLI buffer" ))
	(insert-string "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the locals buffer are\n\n")
	    (insert-buffer locals-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert locals buffer" ))
	(insert-string "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there. 
      (if backtrace-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer backtrace-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Backtrace* buffer" ))
	(insert-string "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there. 
      (if messages-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Messages* buffer were\n\n")
	    (insert-buffer messages-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Messages* buffer" ))
	(insert-string "\n\n\nThere was no *Messages* buffer" )))

    (when process-environment
      (insert-string "\n\n\nProcess environment: \n\n")
      (insert-string (mapconcat (lambda (var) var) process-environment "\n")))

    (let ((buf (get-buffer-create "*Insert .emacs*"))
	  (mail-buf (current-buffer)))
      
      (set-buffer buf)
      (widget-insert "It is requested that you send the entire contents of your .emacs file.\n")
      (widget-insert "This is because it has been found that those parts of the .emacs file\n" )
      (widget-insert "which appear not to be JDE related often do in fact contain the cause of\n")
      (widget-insert "reported bugs.\n\n")
      (widget-insert "If you do not want to send the contents of your .emacs or you load a large\n" )
      (widget-insert "large number of files from your full .emacs file, then please attempt to\n" )
      (widget-insert "replicate the bug using the minimal .emacs file suggested in the JDE\n" )
      (widget-insert "documentation, and note that you have done this in this bug report\n" )
      (widget-insert "If you choose to do neither of these things we may not be able to\n" )
      (widget-insert "or necessarily want to help determine the cause of the problem!\n" )
      (switch-to-buffer "*Insert .emacs*")
      
      (set-buffer mail-buf)
      (goto-char (point-max))
      (if (y-or-n-p "Insert your .emacs file into the problem report? " )
	  (progn
	    (insert-string "\n\n\nThe contents of the .emacs file was\n\n\n")
	    (insert-file "~/.emacs")
	    (goto-char (point-max))
	    (insert-string "\n\n\n=====end inserted .emacs file"))
	(insert-string "\n\n\nThe user choose not to insert their .emacs file\n" ))
      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun jde-problem-report-list-all-variables()
  "List all variables starting with `jde' or `bsh'."
  (let ((vars))
    (mapatoms
     (lambda (symbol)
       (when 
	   (and (or 
		 (string-match "bsh-" (symbol-name symbol))
		 (string-match "jde-" (symbol-name symbol)))
		(get symbol 'custom-type))
	 (setq vars (cons symbol vars)))))
    vars))


;; Line numbering support.

(defvar jde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'jde-setnu-deletion-check)

(defun jde-setnu-after-change (start end length)
 "When in setnu-mode, toggles setnu-mode off and on."
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     jde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq jde-setnu-deletion-check nil)))

(defun jde-setnu-before-change (start end) 
  "Determines whether any newlines were deleted."
   (if setnu-mode
       (if (> end start) 
	   (setq jde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end))))))


(defcustom jde-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering."
 :group 'jde-project
 :type 'integer)

(defcustom jde-setnu-mode-enable nil
 "Enable numbering of lines in Java source buffers."
 :group 'jde-project
 :type 'boolean
 :set '(lambda (sym val)
	 (if val
	     (progn
	       (require 'setnu)
	       (add-hook 
		'after-change-functions 
		'jde-setnu-after-change)
	       (add-hook 
		'before-change-functions 
		'jde-setnu-before-change)
	       (mapc
		(lambda (buf)
		  (save-excursion
		    (set-buffer buf)
		    (if (and
			 (not setnu-mode)
			 (< (point-max) jde-setnu-mode-threshold))
			(setnu-mode 1))))
		  (jde-get-java-source-buffers)))
	   (progn
	     (mapc 
	      (lambda (buf)
		(save-excursion
		  (set-buffer buf)
		  (if (and (boundp 'setnu-mode)
			   setnu-mode)
		      (setnu-mode))))
	      (jde-get-java-source-buffers))))	 
	 (set-default sym val)))


(provide 'jde)

;; Change History
;;
;; $Log: jde.el,v $
;; Revision 1.129  2000/07/29 03:18:41  paulk
;; Add support for line numbering via the setnu package.
;;
;; Revision 1.128  2000/07/28 06:27:47  paulk
;; Committing all modified files.
;;
;; Revision 1.127  2000/07/13 05:22:49  paulk
;; *** empty log message ***
;;
;; Revision 1.126  2000/06/12 08:20:19  paulk
;; Integrated David Ponce's jdok package.
;;
;; Revision 1.125  2000/05/10 05:38:41  paulk
;; The JDEbug menu now appears or disappears when you select or deselect JDEbug as the current debugger.
;;
;; Revision 1.124  2000/04/20 04:32:09  paulk
;; User can now supply customized imenu regular expressions. See `jde-imenu-regex-function'.
;;
;; Revision 1.123  2000/04/14 07:23:30  paulk
;; Added option jde-imenu-recognize-tag-comments-p. When on, this option causes the imenu symbol declaration indexer to recognize variables and method declarations witn prefixed tag comments.
;;
;; Revision 1.122  2000/03/16 05:08:25  paulk
;; Added JDEbug option to jde-db-debugger.
;;
;; Revision 1.121  2000/03/03 06:52:20  paulk
;; Moved Browse JDK doc item to Help menu. Other cosmetic changes to the
;; Help menu.
;;
;; Revision 1.120  2000/02/17 06:40:15  paulk
;; Fixed key bindings to show function keys on menu labels.
;;
;; Revision 1.119  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.118  2000/02/16 03:11:50  paulk
;; *** empty log message ***
;;
;; Revision 1.117  2000/02/14 06:19:38  paulk
;; Implemented up and down stack commands.
;;
;; Revision 1.116  2000/01/29 02:25:15  paulk
;; You can now use the notation [f1], [f2], etc., to specify function
;; keys when customizing jde-key-bindings.
;;
;; Revision 1.115  2000/01/18 07:11:26  paulk
;; Added jde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.114  2000/01/02 08:07:55  paulk
;; Added attach process commands.
;;
;; Revision 1.113  1999/12/23 04:04:02  paulk
;; * jde-mode now defines underscore as a word constituent to allow
;; variable names to start with underscores.
;; * Added Project menu
;; * Added Project Files->Load and Load All commands
;; * Added jde-project-context-switching-enabled-p variable.
;; * Context-switching now suspended when JDEbug is running a debuggee process.
;;
;; Revision 1.112  1999/12/19 06:57:43  paulk
;; Made buffer activation hook truly buffer local. Thanks to "Phillip
;; Lord" <plord@hgmp.mrc.ac.uk> for this fix.
;;
;; Revision 1.111  1999/12/14 05:16:13  paulk
;; Changed key binding for jde-help-class from C-c C-v C-h to C-c C-v C-w
;; because C-h is a reserve binding. Thanks to Richard Y. Kim <ryk@coho.net> for suggesing this change.
;;
;; Revision 1.110  1999/12/13 05:54:09  paulk
;; Added jde-bug-vm-executable and jde-bug-jre-home variables.
;; Fixed jde-dbs-launch-process command so that it fails gracefully.
;;
;; Revision 1.109  1999/11/28 07:01:23  paulk
;; Changed key binding for jde-complete-at-point to C-c C-v C-.
;;
;; Revision 1.108  1999/11/27 05:13:50  paulk
;; Added commands for tracing classes.
;;
;; Revision 1.107  1999/11/01 03:08:05  paulk
;; Added jde-submit-bug-report contributed by Phillip Lord.
;;
;; Revision 1.106  1999/10/01 05:59:25  paulk
;; Added jde-wiz-update-class-list function to the Wizards menu.
;; Added jde-help-class to the Help menu.
;;
;; Revision 1.105  1999/09/30 04:48:53  paulk
;; Cache jde variables to speed switching between projects. Enhancement
;; provide by David Biesack.
;;
;; Revision 1.104  1999/09/17 06:51:07  paulk
;; Fixed Wizards->Import Class menu item to invoke
;; jde-wiz-find-and-import instead of jde-wiz-import.
;;
;; Revision 1.103  1999/09/08 05:40:47  paulk
;; Updated debugger code to take advantage of new unbound slot capability
;; of eieio.
;;
;; Revision 1.102  1999/09/05 04:37:24  paulk
;; Fixed bug in jde-show-help function that prevented it from working
;; with Internet Explorer on Windows/NT.
;;
;; Revision 1.101  1999/08/27 05:27:53  paulk
;; Provided initial support for multiple processes.
;; Fixed jde-find-data-directory to work on XEmacs with a standard
;; JDE distribution.
;; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
;; Now includes jde-db-option options on vm command-line for process.
;;
;; Revision 1.100  1999/08/24 06:29:43  paulk
;; Reimplemented the constructor for jde-dbs-proc the right way. Renamed
;; jde-bug-counter to jde-bug-breakpoint-counter.
;;
;; Revision 1.99  1999/08/23 03:52:01  paulk
;; Updated jde-show-help function to reflect new location of user's guide
;; in the JDE help hierarchy.
;;
;; Revision 1.98  1999/08/23 01:44:25  paulk
;; Updated to use Eric Ludlam's eieio object system.
;;
;; Revision 1.97  1999/08/15 23:48:32  paulk
;; Provided support for initial implementation of JDEbug.
;;
;; Revision 1.96  1999/06/27 01:04:14  paulk
;; Changed release number to 2.1.6beta4.
;;
;; Revision 1.95  1999/06/26 00:15:09  paulk
;; Kill project file buffer after saving to prevent cross-contamination of simultaneously open project files.
;;
;; Revision 1.94  1999/06/22 21:16:24  paulk
;; Added key binding for `jde-help-class'.
;; Updated revision to 2.1.6beta3.
;;
;; Revision 1.93  1999/05/10 13:23:07  paulk
;; Added require statement for jde-parse.
;;
;; Revision 1.92  1999/05/07 23:25:13  paulk
;; Changed version number to 2.1.6beta1.
;;
;; Revision 1.91  1999/05/07 21:13:45  paulk
;; Changed key binding C-c C-v C-z to invoke
;; jde-wiz-find-and-import instead of jde-wiz-import.
;;
;; Changed jde-build to support interactive entry of make arguments.
;;
;; Revision 1.90  1999/03/10 18:56:43  paulk
;; Fixed in bug in jde-find-jde-data-directory
;;
;; Revision 1.89  1999/03/10 17:00:09  paulk
;; Changed version to 2.1.5.
;;
;; Revision 1.88  1999/02/26 15:56:38  paulk
;; Version 2.1.5b4
;;
;; Revision 1.87  1999/02/17 19:17:11  paulk
;; 2.1.5b3 version number.
;;
;; Revision 1.86  1999/02/15 01:15:09  paulk
;; Updated version number.
;;
;; Revision 1.85  1999/02/12 15:26:09  paulk
;; Added menu item (Wizards->Import Class) for generating import statements.
;;
;; Revision 1.84  1999/02/08 18:02:57  paulk
;; *** empty log message ***
;;
;; Revision 1.83  1999/02/04 01:38:37  paulk
;; Provided a fix for ensuring that key bindings are always set. The fix is
;; to do a custom-initialize-reset on the jde-key-bindings variable in the jde-mode
;; function. The jde-mode-map is updated with the key bindings as a side effect of
;; resetting the variable.
;;
;; Revision 1.82  1999/02/04 01:22:23  paulk
;; Fixed some keybindings. Also backed out Matthew Moore's fix for ensuring
;; that jde-mode-keymap gets set at startup since it seems to break
;; java-mode. I'll try to come up with another fix later.
;;
;; Revision 1.81  1999/02/03 22:57:31  paulk
;; *** empty log message ***
;;
;; Revision 1.80  1999/02/03 01:54:46  paulk
;; Minor fix to debug applet item.
;;
;; Revision 1.79  1999/02/03 01:12:11  paulk
;; Fixed a bug in the initialization code for jde-key-bindings.
;; Thanks to Matthew Moore <matthew.moore@Schwab.COM> for this fix.
;;
;; Added a menu item for debugging applets.
;;
;; Revision 1.78  1999/01/15 22:11:04  paulk
;; Some XEmacs patches that I missed.
;;
;; Revision 1.77  1999/01/15 21:57:48  paulk
;; Added XEmacs compatibility changes from Andy Piper.
;;
;; Revision 1.76  1998/12/09 00:56:31  paulk
;; Put jde-compile variables in a new file jde-compile.el.
;;
;; Revision 1.75  1998/11/27 10:10:03  paulk
;; Updated JDE version number to 2.1.3.
;;
;; Revision 1.74  1998/11/22 18:13:57  paulk
;; Added menu items for the BeanShell and method override and interface wizards.
;;
;; Revision 1.73  1998/09/13 02:01:53  paulk
;; Fixed a small bug in key binding code.
;;
;; Revision 1.72  1998/09/13 01:49:35  paulk
;; Added support for customization of JDE key bindings via the
;; variable jde-key-bindings.
;;
;; Revision 1.71  1998/09/13 00:32:48  paulk
;; Added System.out.println template to the Generate menu.
;;
;; Revision 1.70  1998/09/07 02:50:31  paulk
;; This version includes the latest version of jde-gen.el, which was inadvertently
;; replaced by an older version in the last release. This version also includes
;; a newer version of speedbar.el that seems to work better with NT/Emacs 20.3.1
;; than the one that comes with the 20.3.1 distribution.
;;
;; Revision 1.69  1998/08/28 12:56:06  paulk
;; *** empty log message ***
;;
;; Revision 1.68  1998/08/28 12:52:52  paulk
;; Updated version number.
;;
;; Revision 1.67  1998/07/28 03:15:33  paulk
;; Removed a diagnostic message.
;;
;; Revision 1.66  1998/07/28 03:12:40  paulk
;; Updated version number to 2.0.9.
;;
;; Revision 1.65  1998/07/28 03:12:05  paulk
;; Fixed the following project file bugs:
;;
;;   * JDE does not store the project name in the project file.
;;   * JDE does not save variables whose value is nil.
;;   * JDE does not reset variables to initial values when
;;     switching to a buffer that is not part of a project.
;;
;; Revision 1.64  1998/07/22 00:10:07  paulk
;; Now requires cus-edit. This fixes custom-quote is void bug.
;;
;; Fixed bug in jde-set-variables that prevented loading of
;; project files in the new format.
;;
;; Revision 1.63  1998/07/10 00:49:24  paulk
;; Changed jde-save-variables to mark variables that have been customized\n in the current session. Changed jde-set-variables to store the value\n of a customized variable in the customized-value property of the\n variable. This enables Custom to recognize the variable as customized.\n\n  Added jde-convert-prj-file, a function that converts old project files to \n \
;; JDE-2.0.7 format.\n\n Fixed a bug in the function that finds the JDE documentation.
;;
;; Revision 1.62  1998/07/09 04:33:57  paulk
;; Change the way that the JDE saves and restores project-specific values of
;; customization variables to be compatible with custom. This fixes the bug
;; that caused errors when loading customized JDE variables from a .emacs file.
;; .
;; .
;; .
;; Old entries deleted to save space.
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;