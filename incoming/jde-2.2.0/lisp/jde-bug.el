;;; jde-bug.el -- JDEbug Interface
;; $Revision: 1.52 $ $Date: 2000/07/28 06:27:44 $ 

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

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'jde-parse)
(require 'jde-dbs)

(when jde-xemacsp
  (defun line-beginning-position ()
    (point-at-bol))
  (defun line-end-position ()
    (point-at-eol)))

(defgroup jde-bug nil
  "JDEbug General Options"
  :group 'jde
  :prefix "jde-bug")


(defcustom jde-bug-debug nil
"*IMPORTANT!!!! Leave this switch in its default state (off) unless
you need to modify the *JDEbug* Java source code. Setting this switch
on causes the JDE to load *JDEbug* from its java/classes directory
instead of from jde.jar. It also causes the JDE to run the debugger in
debug server mode. This allows you to use *JDEbug* to debug itself."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-jpda-directory ""
  "*Pathname of the directory containing Sun's Java Platform Debug Architecture
distribution. You need to set this variable only if this project uses a JDK 1.2 vm."
  :group 'jde-bug
  :type 'file)

(defcustom jde-bug-vm-includes-jpda-p nil
  "*On (non-nil) indicates that the vm used by this project includes JDPA. 
Turn this option on if this project uses a JDK 1.3 (or later) vm."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-jdk-directory "e:/jdk1.3/"
  "*Specifies the location of the JDK used to debug this application. 
The tools.jar file distributed with JDK 1.3beta (in the jdk1.3/lib directory) contains the JPDA classes. However, the vm's boot classpath does not include this jar. Thus the JDE must include tools.jar in the application classpath when invoking JDEbug."
  :group 'jde-bug
  :type 'string)


(defcustom jde-bug-jre-home "" 
"*Home directory of the JRE containing the executable used to 
run debuggee processes.   
This variable specifies the home directory of the Java runtime
environment containing the executable, e.g., java, to be used to
launch processes (see `jde-bug-vm-executable'). If you do not specify
a home directory, the home directory is the same as that of the
executable used to run the debugger itself."
  :group 'jde-bug :type 'string)


(defcustom jde-bug-vm-executable (list (if (eq system-type 'windows-nt) "javaw" "java"))
  "*Name of the executable used to launch target processes.
This defaults to java on Unix platforms and javaw on Windows platforms"
  :group 'jde-bug
  :type '(list
	  (radio-button-choice 
	  (const "java")
	  (const "javaw")
	  (const "java_g"))))

(defcustom jde-bug-raise-frame-p t
  "*Raise frame when a breakpoint is hit."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-server-socket (cons t "2112") 
  "*Socket where debugger listens for apps needing debugger services.
You can arrange for a vm to connect to JDEbug via a socket by starting it with the
options -Xdebug and -Xrunjdwp:transport=dt_socket,address=MYHOST:NNNN,
where MYHOST is the name of the machine running the debugger and
NNNN is the socket specified by this variable. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE." 
  :group 'jde-bug 
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Address")))

(defcustom jde-bug-server-shmem-name (cons t "JDEbug") 
  "*Shared memory name under which the debugger listens for apps 
needing debugger services. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE." 
  :group 'jde-bug 
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Name")))


(defcustom jde-bug-debugger-host-address (if jde-xemacsp (system-name) system-name)
  "*Address of system on which JDEbug is running.
The default value is the value of the standard Emacs variable `system-name'."
  :group 'jde-bug
  :type 'string)


(defcustom jde-bug-debugger-command-timeout 10
  "*Length of time the JDE waits for a response from the debugger to a command."
  :group 'jde-bug
  :type 'integer)

(defcustom jde-bug-saved-breakpoints nil
"*Breakpoints to be set for the current project."
  :group 'jde-bug
  :type '(repeat
	  (cons :tag "Break at"
	   (string :tag "File Name")
	   (integer :tag "Line Number"))))


(defcustom jde-bug-breakpoint-cursor-colors (cons "cyan" "brown")
"*Specifies the foreground and background colors of the debugger's
breakpoint cursor."
  :group 'jde-bug
  :type '(cons  
	  (string :tag "Foreground Color") 
	  (string :tag "Background Color"))
  :set '(lambda (sym val)
	  (make-face 'jde-bug-breakpoint-cursor)
	  (set-face-foreground 'jde-bug-breakpoint-cursor (car val))
	  (set-face-background 'jde-bug-breakpoint-cursor (cdr val))
	  (set-default sym val)))


(defcustom jde-bug-breakpoint-marker-colors (cons "red" "yellow")
"*Specifies the foreground and background colors of the debugger's
breakpoint marker."
  :group 'jde-bug
  :type '(cons :tag "Colors"
	  (string :tag "Foreground") 
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-bug-breakpoint-marker)
	  (set-face-foreground 'jde-bug-breakpoint-marker (car val))
	  (set-face-background 'jde-bug-breakpoint-marker (cdr val))
	  (set-default sym val)))

(defgroup jde-bug-window nil
  "JDEbug Window Preferences"
  :group 'jde-bug
  :prefix "jde-bug-window")

(defcustom jde-bug-window-message nil
  "Message buffer window preferences."
  :group 'jde-bug-window
  :type 'list)

(defvar jde-bug-menu-spec
  (list "JDEbug"

	["Step Over"                  jde-bug-step-over 
                                      (jde-dbs-target-process-steppable-p)]

	["Step Into"                  jde-bug-step-into 
	                              (jde-dbs-target-process-steppable-p)]

	["Step Out"                   jde-bug-step-out 
	                              (jde-dbs-target-process-steppable-p)]

	["Continue"                   jde-bug-continue 
	                              (jde-dbs-target-process-runnable-p)]

	["Exit Debugger"              jde-bug-exit 
	                              (jde-dbs-debugger-running-p)]
	"-"
	["Set Breakpoint"             jde-bug-set-breakpoint t]
	["Set Conditional Breakpoint" jde-bug-set-conditional-breakpoint nil]
	["Clear Breakpoint"           jde-bug-clear-breakpoint t]
	["Save Breakpoints"           jde-bug-save-breakpoints nil]

	(list
	 "Watch for Field"

	 ["Access"                   jde-bug-watch-field-access 
	                             :style    nil
				     :active   (and 
						(jde-dbs-debugger-running-p)
						(jde-dbs-get-target-process))]


	 ["Modification"             jde-bug-watch-field-modification
	                             :style   nil
	                             :active  (and 
						(jde-dbs-debugger-running-p)
						(jde-dbs-get-target-process))]

	 ["Cancel"                   jde-bug-cancel-watch
	                             :style     nil
				     :active    (and
						 (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process)
						  (slot-boundp
						   (jde-dbs-get-target-process)
						   'watch-req))]

	)

	(list
	 "Trace"
	
	 ["Class Prep..."             jde-bug-trace-class-prep
	                              :style    nil
				      :active	 (and 
					          (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process))]

	 ["Class Unload..."           jde-bug-trace-class-unload
	                              :style    nil
				      :active	 (and 
					          (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process))]


	 ["Method Entry..."           jde-bug-trace-method-entry
	                              :style    nil
				      :active	 (and 
					          (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process))]

	 ["Method Exit..."            jde-bug-trace-method-exit
	                              :style    nil
				      :active	 (and 
					          (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process))]

	 ["Exceptions..."            jde-bug-trace-exceptions
	                              :style    nil
				      :active	 (and 
					          (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process))]


	 ["Cancel..."                jde-bug-cancel-trace
	                             :style     nil
				     :active    (and
						 (jde-dbs-debugger-running-p)
						  (jde-dbs-get-target-process)
						  (slot-boundp
						   (jde-dbs-get-target-process)
						   'trace-req))]

	 )

	"-"


	(list
	 "Display"

	 ["Variable"                  jde-bug-display-variable 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]


	 ["Array"                     jde-bug-display-array
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Object"                    jde-bug-display-object
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))
				      ]

	 ["String"                    jde-bug-display-string
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))
				      ]

	 ["Local Variables"           jde-bug-display-local-variables 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Loaded Classes"            jde-bug-display-loaded-classes 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Threads"                   jde-bug-show-threads
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Object Monitors"           jde-bug-show-object-monitors
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]


	 ["Path Info"                 jde-bug-display-path-info
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]



	)

	["Evaluate Expression"        jde-bug-evaluate-expression 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	(list
	 "Stack"
	 ["Up"                        jde-bug-up-stack 
	                              (and
                                       (jde-dbs-target-process-steppable-p)
				       (let* ((process (jde-dbs-get-target-process))
					      (stack-max 
					       (if (slot-boundp process 'stack)
						   (1- (length (oref process stack)))
						 0))
					      (stack-ptr (oref process stack-ptr)))
					 (< stack-ptr stack-max)))]

	 ["Down"                      jde-bug-down-stack 
	                              (and 
				       (jde-dbs-target-process-steppable-p)
				       (let* ((process (jde-dbs-get-target-process))
					      (stack-ptr (oref process stack-ptr)))
					 (> stack-ptr 0)))]
	 
	 )
	(list
	 "Thread"

	 ["Suspend"                   jde-bug-suspend-thread
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]


	 ["Resume"                    jde-bug-resume-thread 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Interrupt"                 jde-bug-interrupt-thread 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Stop"                      jde-bug-stop-thread 
	                              (and 
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

;	 ["Show Thread Info"          jde-bug-thread-show-thread-info nil]
	 )

	(list
	 "Processes"
	 ["Start Debugger"            jde-bug-start-debugger 
	                              (not (jde-dbs-debugger-running-p))]

	 ["Launch Process"            jde-bug-launch-process
	                              (jde-dbs-debugger-running-p)]

	 ["Suspend Process"           jde-bug-suspend-process
                                      (let ((process (jde-dbs-get-target-process)))
					(and 
					 (jde-dbs-debugger-running-p)
					 process
					 (not (oref process suspendedp))))]

	 ["Resume Process"           jde-bug-resume-process
	                              (let ((process (jde-dbs-get-target-process)))
					(and 
					 (jde-dbs-debugger-running-p)
					 process
					 (oref process suspendedp)))] 

	 ["Finish Process"            jde-bug-finish-process 
	                               (let ((process (jde-dbs-get-target-process)))
					 (and
					  (jde-dbs-debugger-running-p)
					  process
					  (not (oref process attachedp))))]

	 "-"

	 (list
	  "Attach Process"
	  ["Via Shared Memory"        jde-bug-attach-via-shared-memory
	                              (and 
                                       (eq system-type 'windows-nt)
				       (jde-dbs-debugger-running-p))]

	  ["On Local Host"             jde-bug-attach-local-host
	                               (jde-dbs-debugger-running-p)]
 
	  ["On Remote Host"             jde-bug-attach-remote-host
	                                (jde-dbs-debugger-running-p)] 
          )

	 (list
	  "Listen on"
	  ["Shared Memory"             jde-bug-listen-shmem
	                               (and
					(eq system-type 'windows-nt)
					(jde-dbs-debugger-running-p))]
	  
	  ["Socket"                     jde-bug-listen-socket
					(jde-dbs-debugger-running-p)]
	  )

	 ["Detach Process"            jde-bug-detach-process 
	                               (let ((process (jde-dbs-get-target-process)))
					 (and
					  (jde-dbs-debugger-running-p)
					  process
					  (oref process attachedp)))]


	 "-"

	 ["Set Target Process"        jde-bug-set-target-process 
	                              (> (jde-dbs-proc-set-get-size
					  jde-dbs-the-process-registry)
					  0)]

	 ["Show Processes"            jde-bug-set-show-processes nil]

	 ["Remove Dead Processes"     jde-bug-remove-dead-processes 
	                              (oref jde-dbs-the-process-morgue proc-alist)]

	 )
	(list
	 "Show Buffer"

	 ["Locals"                    jde-bug-show-locals-buf 
	                              (and
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["CLI"                       jde-bug-show-cli-buf
                                      (and    
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]

	 ["Threads"                   jde-bug-show-threads-buf
	                               (and    
				       (jde-dbs-debugger-running-p)
				       (jde-dbs-get-target-process))]
	 )
	"-"
	["Preferences"                jde-bug-show-preferences t]
	)
"Defines the JDE's debug menu.")


(defvar jde-bug-keymap (make-sparse-keymap)
  "Debugger keymap.")

(easy-menu-define  jde-bug-menu jde-bug-keymap "JDEbug menu" jde-bug-menu-spec)

(defvar jde-bug-minor-mode-p nil
  "If non-nil, show jdebug menu.")
(make-variable-buffer-local 'jde-bug-minor-mode-p)


; (let ((a (assoc 'jde-bug-minor-mode-p minor-mode-map-alist)))
;   (if a
;       (setcdr a jde-bug-keymap)
;     (add-to-list 'minor-mode-map-alist
; 		 (cons 'jde-bug-minor-mode-p
; 		       jde-bug-keymap))))

(defun jde-bug-install-jdebug-menu ()
  "Installs the jdebug menu in the current Java source buffer
if the user has selected jdebug as the debugger for the current
project."
  (if (and 
       (or 
	(not jde-xemacsp) 
	(featurep 'infodock)))
      (progn
	(setq jde-bug-minor-mode-p t)
	(let ((a (assoc 'jde-bug-minor-mode-p minor-mode-map-alist)))
	  (if a
	      (setcdr a jde-bug-keymap)
	    (add-to-list 'minor-mode-map-alist
			 (cons 'jde-bug-minor-mode-p
			       jde-bug-keymap)))))
    (if (and 
	 (not (featurep 'infodock))
	 (not (memq 'infodock c-emacs-features))
	 (boundp 'current-menubar)
	 current-menubar
	 (not (car (find-menu-item current-menubar '("JDEbug")))))
	(if (fboundp 'add-submenu)
	    (add-submenu nil jde-bug-menu)
	  (add-menu nil "JDEbug" (cdr jde-bug-menu))))))

(defun jde-bug-remove-jdebug-menu ()
  "Removes the jdebug menu from the menubar of the current Java source buffer."
  (if (and 
       (or 
	(not jde-xemacsp) 
	(featurep 'infodock)))
      (setq jde-bug-minor-mode-p nil)
    (if (and 
	 (not (featurep 'infodock))
	 (not (memq 'infodock c-emacs-features))
	 (boundp 'current-menubar)
	 current-menubar)
      (if (fboundp 'delete-menu-item)
	  (delete-menu-item '("JDEbug"))))))

;; (fmakunbound 'jde-bug-key-bindings)
(defcustom jde-bug-key-bindings
  (list (cons "[?\C-c ?\C-z ?\C-s]" 'jde-bug-step-over)
	(cons "[?\C-c ?\C-z ?\C-x]" 'jde-bug-step-into)
	(cons "[?\C-c ?\C-z ?\C-w]" 'jde-bug-step-out)
	(cons "[?\C-c ?\C-z ?\C-c]" 'jde-bug-continue)
	(cons "[?\C-c ?\C-z ?\C-b]" 'jde-bug-set-breakpoint))
  "*Specifies key bindings for JDEbug.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-bug
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'jde-bug-key-bindings)
	       jde-bug-key-bindings)
	      (mapc 
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]"key)
		       (setq key (car (read-from-string key))))
		   (define-key jde-bug-keymap key nil)))
	       jde-bug-key-bindings))
	  ;; Map new key bindings.
	  (mapc 
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]"key)
		   (setq key (car (read-from-string key))))
	       (define-key jde-bug-keymap key fcn)))
	   val)
	  (set-default sym val)))

(defvar jde-bug-breakpoint-marker-overlays nil 
  "List of breakpoint marker overlay positions for this buffer.")

(setq-default jde-bug-breakpoint-marker-overlays nil)
(make-variable-buffer-local 'jde-bug-breakpoint-marker-overlays)

(defvar jde-bug-breakpoint-cursor-overlay nil 
  "Breakpoint cursor overlay position for this buffer.")

(setq-default jde-bug-breakpoint-cursor-overlay nil)
(make-variable-buffer-local 'jde-bug-breakpoint-cursor-overlay)


(defvar jde-bug-breakpoints nil
"Current breakpoints.")

(defun jde-bug-breakpoints-add (bp)
  "Adds this breakpoints to the list of breakpoints."
  (setq jde-bug-breakpoints 
	(cons (cons (oref bp id) bp) 
	      jde-bug-breakpoints)))

(defun jde-bug-breakpoints-remove (bp)
  "Removes BP from list of breakpoints."
  (setq jde-bug-breakpoints
	(remove-if
	 (lambda (assoc)
	   (let* ((xbp (cdr assoc))
		  (xid (oref xbp id))
		  (id (oref bp id)))
	     (equal xid id)))
	 jde-bug-breakpoints)))

(defun jde-bug-breakpoints-find (file line)
  "Finds the breakpoint object for the breakpoint at FILE and LINE."
  (cdr (find-if 
	(lambda (assoc)
	  (let ((bp (cdr assoc)))
	       (and (string= (oref bp file) file)
		    (equal (oref bp line) line))))
	jde-bug-breakpoints)))

(defun jde-bug-make-breakpoint-overlay ()
"Makes a breakpoint overlay at the current line in the current buffer."
  (let ((marker-overlay
	 (make-overlay
	  (line-beginning-position)
	  (line-end-position)
	  (current-buffer) nil t)))
    (overlay-put marker-overlay  'face 'jde-bug-breakpoint-marker)
    (overlay-put marker-overlay 'priority 99) 
    marker-overlay))


(defun jde-bug-highlight-breakpoint (&optional line)
  (save-excursion
    (if line (goto-line line))
    (if jde-xemacsp
	(if (or (not (extent-at (line-beginning-position)))
		(not (eq 'jde-bug-breakpoint-marker
			 (extent-property (extent-at (line-beginning-position))
					  'face nil))))
	    (let ((highlight-extent
		   (make-extent
		    (line-beginning-position)
		    (line-end-position))))
	      (set-extent-face highlight-extent 'jde-bug-breakpoint-marker)
	      (set-extent-priority highlight-extent 99)))
      (let ((marker-overlay
	     (make-overlay
	      (line-beginning-position)
	      (line-end-position)
	      (current-buffer) nil t)))
	(overlay-put marker-overlay  'face 'jde-bug-breakpoint-marker)
	(overlay-put marker-overlay 'priority 99) 
	marker-overlay))))

(defvar jde-bug-breakpoint-id-counter 0
"Counter for generating breakpoint ids")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-bug-breakpoint ()
  ((id   :initarg :id
	 :type integer
	 :documentation
	 "Indentifies this breakpoint.")
   (file :initarg :file
	 :initform ""
	 :type string
	 :documentation
	 "Pathname of file containing this breakpoint.")
   (line :initarg :line
	 :type integer
	 :documentation
         "Line number of this breakpoint."))
  (:allow-nil-initform t)
  "Class of breakpoints.")


(defmethod initialize-instance ((this jde-bug-breakpoint) &rest fields)
  "Constructor for a breakpoint specification."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this id))
  (assert (oref this file))
  (assert (oref this line)))

(defun jde-bug-step-over () 
  "Advances the process to the next line in the current method."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step over" :process process)))
    (jde-dbs-cmd-exec cmd)))


(defun jde-bug-step-into () 
  "Advances the process into the function invoked at point."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step into" :process process :step-type "into")))
    (jde-dbs-cmd-exec cmd)))

(defun jde-bug-step-out () 
  "Advances the process to the next line in the invoking method."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step into" :process process :step-type "out")))
    (jde-dbs-cmd-exec cmd)))


(defun jde-bug-continue () 
  "Runs the target process. Execution continues from the current breakpoint."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (run (jde-dbs-run-process
	       (format "run %d" (oref process id))
		  :process process)))
    (oset process startupp nil)
    (oset process suspendedp nil)
    (oset process steppablep nil)
    (jde-dbs-cmd-exec run)))



(defun jde-bug-exit () 
  (interactive)
  (if (jde-dbs-debugger-running-p)
      (progn
	(mapc
	 (lambda (assoc)
	   (let* ((process (cdr assoc))
		  (finish-cmd (jde-dbs-finish-process
			       (format "finish %d" (oref process id))
			       :process process))
		  (result (jde-dbs-cmd-exec finish-cmd)))
	     (jde-dbs-proc-move-to-morgue process)))
	 (oref jde-dbs-the-process-registry proc-alist))
	(slot-makeunbound jde-dbs-the-process-registry :target-process)
	(jde-dbs-debugger-quit jde-dbs-the-debugger))
    (error "Debugger is not running.")))

(defun jde-bug-get-line-at-point ()
  (let ((ln (count-lines (point-min) (point))))
    (if (eq (char-before) ?\n)
	(1+ ln)
      ln)))


(add-hook 
 'jde-mode-hook 
 (lambda ()
   (if (buffer-file-name)
       (let ((this-file (file-name-nondirectory (buffer-file-name))))
	 (mapc
	  (lambda (spec)
	    (let* ((file (car spec))
		   (line (cdr spec))
		   (bp (jde-bug-breakpoints-find file line)))
	      (when (not bp)
		(setq jde-bug-breakpoint-id-counter (1+ jde-bug-breakpoint-id-counter))
		(setq bp 
		      (jde-bug-breakpoint 
		       (format "breakpoint%d" jde-bug-breakpoint-id-counter)
		       :id jde-bug-breakpoint-id-counter
		       :file file 
		       :line line))
		(jde-bug-breakpoints-add bp))
	      (if (string-match file this-file)
		  (jde-bug-highlight-breakpoint line))))
	  jde-bug-saved-breakpoints)))))

(defun jde-bug-set-breakpoint()
"Sets a breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (line (jde-bug-get-line-at-point))
         (bp (jde-bug-breakpoints-find file line))
         (proc (jde-dbs-get-target-process)))
    (when (not bp)
      (setq jde-bug-breakpoint-id-counter (1+ jde-bug-breakpoint-id-counter))
      (setq bp 
            (jde-bug-breakpoint 
             (format "breakpoint%d" jde-bug-breakpoint-id-counter)
             :id jde-bug-breakpoint-id-counter
             :file file 
             :line line))
      (jde-bug-breakpoints-add bp))
    (jde-bug-highlight-breakpoint)
    (if (and bp proc)
        (let* ((set-breakpoint (jde-dbs-set-breakpoint 
                                "set breakpoint" 
                                :process proc
                                :breakpoint bp))
              (result (jde-dbs-cmd-exec set-breakpoint)))
          (message "Breakpoint set at line %d in class %s." line file)))))

(defun jde-bug-set-conditional-breakpoint () 
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (line (count-lines (point-min) (point)))
	 (bp (jde-bug-breakpoints-find file line))
	 (proc (jde-dbs-get-target-process)))
    (if (and bp proc)
	(let* ((clear-breakpoint
		(jde-dbs-clear-breakpoint 
		 "clear breakpoint"
		 :process proc
		 :breakpoint bp))
	       (result (jde-dbs-cmd-exec clear-breakpoint)))))
    (if bp
	(jde-bug-breakpoints-remove bp))
    (jde-bug-remove-breakpoint-highlight)))

(defun jde-bug-remove-breakpoint-highlight ()
  (if jde-xemacsp
      (let ((highlight (extent-at (line-beginning-position))))
	(when highlight
	  (delete-extent highlight)))
    (let ((highlight (car (overlays-at (line-beginning-position)))))
      (when highlight
	(delete-overlay highlight)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Save Breakpoints Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-save-breakpoints ()
  "Save breakpoints in project file." 
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-bug-trace-methods-dialog (jde-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "entry"
			     :documentation
			     "Values may be entry or exit.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace methods dialogs."
)

(defmethod initialize-instance ((this jde-bug-trace-methods-dialog) &rest fields)
  "Constructor for trace methods dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "entry") 
	      (string= (oref this trace-type) "exit")))
)


(defmethod jde-dialog-create ((this jde-bug-trace-methods-dialog))

  (widget-insert (concat "Trace method " 
			 (oref this trace-type)
			 "\n\n"))

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on method entry or exit."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose methods should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose methods should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod jde-dialog-ok ((this jde-bug-trace-methods-dialog))
  (let* ((thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-exclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-methods-request "trace methods request" 
						 :trace-type (oref this trace-type)))
	 (cmd  (jde-dbs-trace-methods 
		"trace methods command" 
		:process process :trace-request request)))
    
    (if (and thread-restriction (not (string= thread-restriction "")))
        (oset request :thread-restriction thread-restriction))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun jde-bug-trace-method-entry ()
  "Displays the trace method entry dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-methods-dialog "trace method entry dialog")))
    (jde-dialog-show dialog)))

(defun jde-bug-trace-method-exit ()
  "Displays the trace method exit dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-methods-dialog 
		 "trace method exit dialog" :trace-type "exit")))
    (jde-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jde-bug-trace-classes-dialog (jde-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "preparation"
			     :documentation
			     "Values may be preparation or unloading.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace classes dialogs."
)

(defmethod initialize-instance ((this jde-bug-trace-classes-dialog) &rest fields)
  "Constructor for trace classes dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "preparation") 
	      (string= (oref this trace-type) "unloading")))
)


(defmethod jde-dialog-create ((this jde-bug-trace-classes-dialog))

  (widget-insert (concat "Trace class " 
			 (oref this trace-type)
			 "\n\n"))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod jde-dialog-ok ((this jde-bug-trace-classes-dialog))
  (let* ((thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-classes-request "trace classes request" 
						 :trace-type (oref this trace-type)))
	 (cmd  (jde-dbs-trace-classes 
		"trace classes command" 
		:process process :trace-request request)))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun jde-bug-trace-class-prep ()
  "Displays the trace class preparation dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-classes-dialog "trace class prep dialog")))
    (jde-dialog-show dialog)))

(defun jde-bug-trace-class-unload ()
  "Displays the trace class unloading dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-classes-dialog 
		 "trace class unloading dialog" :trace-type "unloading")))
    (jde-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jde-bug-trace-exceptions-dialog (jde-dialog)
  ((exception-class-field    :initarg :exception-class
			     :documentation
			     "Class of exception to trace.")
   (trace-type-field         :initarg :trace-type
			     :documentation
			     "Values may be caught, uncaught, or both.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a trace exception dialog."
)

(defmethod initialize-instance ((this jde-bug-trace-exceptions-dialog) &rest fields)
  "Constructor for trace exceptions dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod jde-dialog-create ((this jde-bug-trace-exceptions-dialog))

  (widget-insert (concat "Trace exception\n\n"))

  (oset this exception-class-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Exception class"
	 :doc "Name of class of exception to trace. May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to trace occurences of java.io.IOException, specify *.IOException."))

  (oset this trace-type-field
	(widget-create
	 '(choice
	   :tag "Exception type"
	   :value "both"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify the type of exception to trace."
	   (const "caught")
	   (const "uncaught")
	   (const "both"))))   

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod jde-dialog-ok ((this jde-bug-trace-exceptions-dialog))
  (let* ((exception-class (widget-value (oref this exception-class-field)))
	 (trace-type (widget-value (oref this trace-type-field)))
	 (thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-exceptions-request 
		   "trace exceptions request" 
		   :exception-class exception-class
		   :trace-type trace-type))
	 (cmd  (jde-dbs-trace-exceptions
		"trace exceptions command" 
		:process process :trace-request request)))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun jde-bug-trace-exceptions ()
  "Displays the trace exceptions dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-exceptions-dialog "trace exceptions dialog")))
    (jde-dialog-show dialog)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-cancel-trace-request (process request)
  "Cancels a specified trace request on a specified process."
  (let ((cmd (jde-dbs-cancel-trace "cancel trace" :process process
				  :trace-request request)))
    (jde-dbs-cmd-exec cmd)))

(defclass jde-bug-cancel-trace-dialog (jde-dialog)
  ((process          :initarg :process
		     :type jde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod jde-dialog-create ((this jde-bug-cancel-trace-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc 
	       (concat
		(if (typep request 'jde-dbs-trace-methods-request)
		    (progn
		      (concat
		      (format "Trace method %s." (oref request trace-type))
		      (if (slot-boundp request 'thread-restriction)
			  (format " Thread restriction: %s." 
				  (oref request thread-restriction)))))
		  (format "Trace class %s." (oref request trace-type)))
		(if (slot-boundp request 'suspend-policy)
		    (format " Suspend policy: %s." (oref request suspend-policy)))
		(if (slot-boundp request 'inclusion-filters)
		    (format " Inclusion filters: %s." (oref request inclusion-filters)))
		(if (slot-boundp request 'exclusion-filters)
		    (format " Exclusion filters: %s." (oref request exclusion-filters)))
		)
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the trace requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list 
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod jde-dialog-ok ((this jde-bug-cancel-trace-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr 
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jde-bug-cancel-trace-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jde-bug-cancel-trace ()
  "Cancels method and class trace requests for the target process.
If only one trace request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive) 
 (let* ((process (jde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'trace-req)
	   (let ((trace-requests (oref process :trace-req)))
	     (if (= (length trace-requests) 1)
		 (jde-bug-cancel-trace-request process (cdr (car trace-requests)))
	       (let ((dialog
		      (jde-bug-cancel-trace-dialog "cancel trace dialog"
						   :process process
						   :requests trace-requests)))
		 (jde-dialog-show dialog))))
	 (error "The target process has no outstanding trace requests"))
     (error "There is no active process."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Command                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jde-bug-watch-field-dialog (jde-dialog)
  ((watch-type                :initarg :watch-type
			      :type string
			      :initform "access"
			      :documentation
			      "Watch type: field access or modification.")
   (object-class-widget       :initarg :object-class-widget
			      :documentation
			      "Widget specifying class of objects to watch.")
   (field-name-widget         :initarg :field-name-widget
			      :documentation
			      "Widget specify name of field to watch.")
   (expression-widget         :initarg :expression-widget
			      :documentation
			      "Widget specify watch restriction expression.")
   (object-id-widget          :initarg :object-id-widget
			      :documentation
			      "Widget specify id of object to watch.")
   (thread-restriction-widget :initarg :thread-restriction-widget
			      :documentation
			      "Text field that contains thread restriction.")
   (suspend-policy-widget     :initarg :suspend-policy-widget
			      :documentation
			      "Text field that specifies the thread suspension policy.")
   (class-inclusion-widget    :initarg :class-inclusion-widget
			      :documentation
			     "Specifies class inclusion filters.") 
   (class-exclusion-widget    :initarg :class-exclusion-widget
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a watch field dialog."
)

(defmethod initialize-instance ((this jde-bug-watch-field-dialog) &rest fields)
  "Constructor for watch field dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod jde-dialog-create ((this jde-bug-watch-field-dialog))

  (widget-insert (format "Watch for field %s\n\n" (oref this watch-type)))

  (oset this object-class-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Class"
	 :doc "Class of object or objects to watch.
May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to watch a field of java.io.IOException, specify *.IOException."))


  (oset this field-name-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Field name"
	 :doc "Name of field to watch.")) 
 
  (oset this expression-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Watch expression"
	 :doc "A boolean expression. 
Execution of the process is suspended only if the expression is true. The expression can contain any variable that is in scope when a field changes."))

  (oset this object-id-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Object ID"
	 :doc "ID of the object to watch."))

  (oset this thread-restriction-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict watch to the specified thread."))

  (oset this suspend-policy-widget
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which threads to suspend on field access or modification."
	   (const "all")
	   (const "thread")
	   (const "none"))))   


  (oset this class-inclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose field should be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 

    (oset this class-exclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose fields should not be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))) 
  )

(defmethod jde-dialog-ok ((this jde-bug-watch-field-dialog))
  (let* ((obj-class (widget-value (oref this object-class-widget)))
	 (field-name (widget-value (oref this field-name-widget)))
	 (expression (widget-value (oref this expression-widget)))
	 (object-id (widget-value (oref this object-id-widget)))
	 (thread-restriction (widget-value (oref this thread-restriction-widget)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-widget)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-watch-field-request 
		   "watch field request" 
		   :watch-type (oref this watch-type)
		   :object-class obj-class
		   :field-name field-name))
	 (cmd  (jde-dbs-watch-field
		"watch field command" 
		:process process :watch-request request)))

    (if (and expression (not (string= expression "")))
	(oset request :expression expression))

    (if (and object-id (not (string= object-id "")))
	(oset request :object-id object-id))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))
    
    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))
    
    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))
		       

(defun jde-bug-watch-field-access ()
  "Request that the debugger watch for access of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (jde-bug-watch-field-dialog "watch field dialog")))
    (jde-dialog-show dialog)))

(defun jde-bug-watch-field-modification ()
  "Request that the debugger watch for modifiction of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (jde-bug-watch-field-dialog "watch field dialog" 
					    :watch-type "modification")))
    (jde-dialog-show dialog)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-cancel-watch-request (process request)
  "Cancels a specified watch field request on a specified process."
  (let ((cmd (jde-dbs-cancel-watch "cancel watch" :process process
				  :watch-request request)))
    (jde-dbs-cmd-exec cmd)))

(defclass jde-bug-cancel-watch-dialog (jde-dialog)
  ((process          :initarg :process
		     :type jde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod jde-dialog-create ((this jde-bug-cancel-watch-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc 
	       (concat
		(format "Watch type: %s. Class: %s. Field: %s."
		       (oref request watch-type)
		       (oref request object-class)
		       (oref request field-name))
		(if (slot-boundp request 'object-id)
		    (concat " Object id: " (oref request object-id) "."))
		(if (slot-boundp request 'expression)
		    (concat " Expression: " (oref request expression) ".")))
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the watch requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list 
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod jde-dialog-ok ((this jde-bug-cancel-watch-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr 
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jde-bug-cancel-watch-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jde-bug-cancel-watch ()
  "Cancels watch requests for the target process.
If only one watch request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive) 
 (let* ((process (jde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'watch-req)
	   (let ((watch-requests (oref process :watch-req)))
	     (if (= (length watch-requests) 1)
		 (jde-bug-cancel-watch-request process (cdr (car watch-requests)))
	       (let ((dialog
		      (jde-bug-cancel-watch-dialog "cancel watch dialog"
						   :process process
						   :requests watch-requests)))
		 (jde-dialog-show dialog))))
	 (error "The target process has no outstanding watch requests"))
     (error "There is no active process."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Variable Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-variable () 
  (interactive)
  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
  (jde-bug-evaluate-expression 
   (if (fboundp 'find-c-expr) (find-c-expr) (gud-find-c-expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Expression Command                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-evaluate-expression (expression) 
"Evaluates a Java expression. The Java expression may include
any variables in scope in the program being debugged."
  (interactive
   "sExpression: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
	   
  (if  (string= expression "")
      (error "Empty expression."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (evaluate-command
	  (jde-dbs-evaluate 
	   (format "Evaluate %s" expression)
	   :process process
	   :expression expression
	   :thread-id thread-id))
	 (result
	  (jde-dbs-cmd-exec evaluate-command)))
    (if result
	(let* ((object-p (equal (length result) 3))
	       (type  (nth 0 result))
	       (value (nth 1 result))
	       (gc    (if object-p 
			  (if (nth 2 result)
			      "(garbage collected)"
			    "")))
	       (formatted-result 
		(if object-p
		    (format "<%s:%s> %s" type value gc)
		  (format "%s (%s)" value type))))
	  (jde-dbs-proc-display-debug-message
	   process
	   (format "\"%s\" = %s" expression formatted-result))
	  (message formatted-result))
      (message "Error: could not evaluate \"%s\"." expression))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Array Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-get-array-length (array)
  (let* ((process (jde-dbs-get-target-process))
	 (get-array-command
	  (jde-dbs-get-array 
	   (format "get_array_length %d" (oref array id))
	   :process process
	   :array array)))
	 (jde-dbs-cmd-exec get-array-command)
	 (if (slot-boundp array 'length)
	     (oref array length))))

(defun jde-bug-get-array-elements (array length)
  (let* ((process (jde-dbs-get-target-process))
	 (get-array-command
	  (jde-dbs-get-array 
	   (format "get_array_elements %d" (oref array id))
	   :process process
	   :array array
	   :index 0
	   :length length)))
	 (jde-dbs-cmd-exec get-array-command)))


(defun jde-bug-display-array (array-id)
  (interactive
   "nArray ID: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((array
	  (jde-dbs-java-array 
	   (format "array %d" array-id) :id array-id))
	 (array-length
	  (jde-bug-get-array-length array)))
    (if array-length
	(if (> array-length 0)
	    (progn
	      (jde-bug-get-array-elements array array-length)
	      (jde-dbs-proc-display-debug-message
	       (jde-dbs-get-target-process) 
	       (jde-dbs-java-obj-to-string array)))
	  (jde-dbs-proc-display-debug-message
	   (jde-dbs-get-target-process)
	   (format "Array %d has no elements." array-id)))      
      (error "Could not get array elements."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Object Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-object (object-id)
  (interactive
   "nObject ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (get-object-command
	  (jde-dbs-get-object
	   (format "get_object %d" object-id)
	   :process process
	   :object-id object-id))
	 (object
	  (jde-dbs-cmd-exec get-object-command)))
    (if object
	(jde-dbs-proc-display-debug-message
	 (jde-dbs-get-target-process)
	 (jde-dbs-java-obj-to-string object))
      (error "Could not get object"))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display String Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-string (object-id) 
  (interactive
   "nObject ID: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (get-string-command
	  (jde-dbs-get-string
	   (format "get_string %d" object-id)
	   :process process
	   :object-id object-id))
	 (string
	  (jde-dbs-cmd-exec get-string-command)))
    (if string
	(jde-dbs-proc-display-debug-message
	 (jde-dbs-get-target-process)
	 (format "string %d = %s" object-id string))
      (error "Could not get object"))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Local Variables Command                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-local-variables () 
  (interactive)

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (get-locals-command
	  (jde-dbs-get-locals
	   (format "get_locals t%d" thread-id)
	   :process process
	   :thread-id thread-id))
	 (result
	  (jde-dbs-cmd-exec get-locals-command)))
    (if result
	(mapc
	 (lambda (var) 
	   (jde-dbs-proc-display-debug-message
	    (jde-dbs-get-target-process)
	    (jde-dbs-java-variable-to-string var)))
	 result)	    
      (error "Could not get locals"))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Loaded Classes Command                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-loaded-classes () 
  "Displays the classes currently loaded by the target process."
  (interactive)

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-get-loaded-classes
	   "get_loaded_classes"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec cmd)))
    (if (not result)	    
      (error "Could not get loaded classes."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Threads Command                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-show-threads () 
"Shows all threads and thread-groups running in the target process.
This command displays the threads as a tree structure. To expand
a node of the tree, click the + sign next to the node, using mouse
button 2."
  (interactive)

  (if (not (jde-dbs-get-target-process))
      (error "No target process."))

  (let* ((process (jde-dbs-get-target-process))
	 (get-threads-command
	  (jde-dbs-get-threads
	   "get_threads"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec get-threads-command)))
    (if (not result)	    
      (error "Could not get threads"))))

(defun jde-bug-thread-show-thread-info () 
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Object Monitors                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-show-object-monitors (object-id) 
"Shows the threads that are monitoring a specified object, including the thread
that currently owns the object and threads that are waiting to access the object."
  (interactive
   "nObject ID: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (get-monitors-command
	  (jde-dbs-get-object-monitors
	   "get_object_monitors"
	   :process process :object-id object-id))
	 (result
	  (jde-dbs-cmd-exec get-monitors-command)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Path Info Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-path-info () 
  "Displays the base directory, boot classpath, and classpath of the target process."
  (interactive)

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-get-path-info
	   "get_path_info"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec cmd)))
    (if (not result)	    
      (error "Could not get path info."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Watchpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-clear-watchpoint () 
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Up Stack Command                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-up-stack () 
  "Moves the source cursor up one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not 
       (let* ((process (jde-dbs-get-target-process))
	      (stack-max (1- (length (oref process stack))))
	      (stack-ptr (oref process stack-ptr)))
	 (< stack-ptr stack-max)))
      (error "The debugger is displaying the top of the stack."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1+ (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))
	 
    (oset process :stack-ptr stack-ptr)
    (jde-dbo-show-line class file line)
    (jde-dbo-update-locals-buf process thread-id stack-ptr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Down Stack Command                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-down-stack () 
  "Moves the source cursor down one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not (let* ((process (jde-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))
      (error "The debugger is displaying the bottom of the stack."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1- (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))
	 
    (oset process :stack-ptr stack-ptr)
    (jde-dbo-show-line class file line)
    (jde-dbo-update-locals-buf process thread-id stack-ptr)))



(defun jde-bug-suspend-thread (thread-id) 
"Suspends the thread or group of threads specified by THREAD-ID.
If the thread or group is already suspended, this command increments
the thread's suspend count. Use JDEBug->Threads->Show Threads (`jde-bug-thread-show-threads')
to display the IDs of all threads and thread groups running in the
target process. Use JDEBug->Processes->Suspend Process 
(`jde-bug-suspend-process') to suspend the entire process. Use
Threads->Resume Thread (`jde-bug-resume-thread') to resume the thread."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (suspend-command
	  (jde-dbs-suspend-thread
	       (format "suspend thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec suspend-command)))
 

(defun jde-bug-resume-thread (thread-id) 
"Resumes the previously suspended thread or group of threads specified
by THREAD-ID.  This command has no effect if the specified thread or
thread-group is running or was not suspended by you, using the
JDEBug->Threads->Suspend Thread command (`jde-bug-suspend-thread').
If you suspended the thread more than once, this command reduces the
suspend count by 1. The thread resumes only when the suspend count
reaches 0. Use JDEBug->Threads->Show Threads
(`jde-bug-thread-show-threads') to display the IDs of all threads and
thread groups running in the target process. Use
JDEBug->Processes->Resume Process (`jde-bug-resume-process') to resume
the entire process."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (resume-command
	  (jde-dbs-resume-thread
	       (format "resume thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec resume-command)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-interrupt-thread (thread-id) 
"Interrupts the thread specified by THREAD-ID. The thread cannot be
resumed. Use JDEBug->Threads->Show Threads
(`jde-bug-thread-show-threads') to display the IDs of all threads
running in the target process. Use Threads->Suspend Thread
(`jde-bug-suspend-thread') to suspend a thread temporarily."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (interrupt-command
	  (jde-dbs-interrupt-thread
	       (format "interrupt thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec interrupt-command)))
 

(defun jde-bug-stop-thread (thread-id exception-id) 
"Stops a thread and throws an exception. THREAD-ID is the id of the thread you want 
to stop. EXCEPTION-ID is the id of the exception object you want to throw. Use 
JDEBug->Threads->Show Threads (`jde-bug-thread-show-threads') to display the IDs of 
all threads and thread groups running in the target process. Use JDEBug->Evaluate Expression
to creae the exception object."
 (interactive
   "nThread ID: \nnException Id: ")
  (let* ((process (jde-dbs-get-target-process))
	 (stop-command
	  (jde-dbs-stop-thread
	       (format "stop thread %d" thread-id)
	       :process process
	       :thread-id thread-id
	       :exception-id exception-id)))
    (jde-dbs-cmd-exec stop-command)))

(defun jde-bug-jpda-installed-p ()
  "Returns t if the jpda is installed."
  (interactive)
  (cond
   (jde-bug-vm-includes-jpda-p
    t)
   ((string= jde-bug-jpda-directory "")
    (error "jde-bug-jpda-directory variable is not set.")
    nil)
   ((not (file-exists-p 
	  (expand-file-name "lib/jpda.jar" jde-bug-jpda-directory)))
    (error "Cannot find JPDA jar file at %s"
	     (expand-file-name "lib/jpda.jar" jde-bug-jpda-directory))
    nil)
   (t
    t)))


(defun jde-bug-start-debugger ()
  "Starts the debugger."
  (interactive)
  (if (and (jde-bug-jpda-installed-p)
	   (jde-dbs-debugger-start jde-dbs-the-debugger))
      (message "Debugger started successfully." )
    (message "Could not start debugger.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-launch-process () 
  "Starts a virtual machine to run the application
in the current source buffer. Halts at the beginning
of the process to let you set breakpoints. The started
process becomes the target process for debugger 
commands. Select Processes->Set Target Process from the JDEBug
menu or run the `jde-bug-set-target-process' command
to set another process as the target process."
  (interactive)
  (let* ((main-class (jde-run-get-main-class)))
    (unless (and
	     (jde-dbs-proc-set-find jde-dbs-the-process-registry 
				    :main-class main-class)
	     (not (yes-or-no-p 
		   (format "An instance of %s is already running. Continue?" main-class))))	 
      (let* ((process 
	      (jde-dbs-proc (format "process%d" 
				    (setq jde-dbs-proc-counter 
					  (1+ jde-dbs-proc-counter))) 
			    :id jde-dbs-proc-counter :main-class main-class))
	     (old-target (jde-dbs-get-target-process))
	     (launch (jde-dbs-launch-process 
		      (format "Launch %s" main-class) 
		      :process process
		      :vmexec (car jde-bug-vm-executable)
		      ;; :vmexec "xyz"
		      ))
	     (succeededp t))
	(jde-dbs-proc-set-add jde-dbs-the-process-registry process)
	(if (not (string= jde-bug-jre-home ""))
	    (oset launch :jre-home jde-bug-jre-home))
	(oset jde-dbs-the-process-registry :target-process process)
	(when (not (jde-dbs-cmd-exec launch))	    
	  (jde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jde-dbs-the-process-registry :target-process old-target))
	  (jde-dbs-proc-set-state process "unknown")
	  (jde-dbs-proc-set-state-reason process "Error launching process.")
	  (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
	  (setq succeededp nil))
	succeededp))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Local Process Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-attach-via-shared-memory (process-name) 
  "Attaches the debugger to a process running on the same machine via shared
memory. This command works only on Windows."
  (interactive
   "sProcess name: ")
  (let* ((process 
	  (jde-dbs-proc (format "process%d" 
				(setq jde-dbs-proc-counter 
				      (1+ jde-dbs-proc-counter))) 
			:id jde-dbs-proc-counter :main-class process-name))
	     (old-target (jde-dbs-get-target-process))
	     (attach (jde-dbs-attach-shmem
		      (format "Attach %s" process-name) 
		      :process process 
		      :process-name process-name)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec attach))	    
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error launching process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Local Host Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-attach-local-host (process-port) 
  "Attaches the debugger to a process running on local host. This command connects 
to the process via a socket."
  (interactive
   "sProcess Port: ")
  (let* ((process 
	  (jde-dbs-proc (format "process%d" 
				(setq jde-dbs-proc-counter 
				      (1+ jde-dbs-proc-counter))) 
			:id jde-dbs-proc-counter :main-class process-port))
	     (old-target (jde-dbs-get-target-process))
	     (attach (jde-dbs-attach-socket
		      (format "Attach %s" process-port) 
		      :process process 
		      :port process-port)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec attach))	    
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error launching process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Remote Host Command                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-attach-remote-host (process-host process-port) 
  "Attaches the debugger to a process running on a remote host. This command connects 
to the process via a socket."
 (interactive
   "sHost: \nsProcess Port: ")
  (let* ((process 
	  (jde-dbs-proc (format "process%d" 
				(setq jde-dbs-proc-counter 
				      (1+ jde-dbs-proc-counter))) 
			:id jde-dbs-proc-counter :main-class process-port))
	     (old-target (jde-dbs-get-target-process))
	     (attach (jde-dbs-attach-socket
		      (format "Attach %s" process-port) 
		      :process process 
		      :host process-host
		      :port process-port)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec attach))	    
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error launching process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Shared Memory Command                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jde-bug-server-shmem-name-history nil
  "History of shared memory names for debugger.")

(defun jde-bug-listen-shmem (shmem-name)
  "Listens on shared memory for a vm requiring debugging services."
  (interactive
   (list
    (if (car jde-bug-server-shmem-name)
	(read-from-minibuffer "Name: " 
			      (car jde-bug-server-shmem-name-history)
			      nil nil 
			      'jde-bug-server-shmem-name-history)
      (cdr jde-bug-server-shmem-name))))
  (let* ((process 
	  (jde-dbs-proc (format "process%d" 
				(setq jde-dbs-proc-counter 
				      (1+ jde-dbs-proc-counter))) 
			:id jde-dbs-proc-counter :main-class shmem-name))
	     (old-target (jde-dbs-get-target-process))
	     (listen (jde-dbs-listen-for-process
		      (format "Listen %s" shmem-name) 
		      :process process 
		      :address shmem-name)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec listen))	    
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error listening for process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Socket Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jde-bug-server-socket-history nil
  "History of sockets used by debugger to listen for debuggee vms.")

(defun jde-bug-listen-socket (socket)
  "Listens on socket for a vm requiring debugging services.
If `jde-bug-server-socket' is set to \"prompt for,\" this command
prompts you to enter the socket on which to listen. Otherwise, it 
listens on the socket specified by `jde-bug-server-socket'."
  (interactive
   (list
    (if (car jde-bug-server-socket)
	(read-from-minibuffer "Socket: " 
			      (car jde-bug-server-socket-history)
			      nil nil 
			      'jde-bug-server-socket-history)
      (cdr jde-bug-server-socket))))
  (let* ((process 
	  (jde-dbs-proc (format "process%d" 
				(setq jde-dbs-proc-counter 
				      (1+ jde-dbs-proc-counter))) 
			:id jde-dbs-proc-counter :main-class socket))
	     (old-target (jde-dbs-get-target-process))
	     (listen (jde-dbs-listen-for-process
		      (format "Listen %s" socket) 
		      :process process 
		      :address socket
		      :transport "socket")))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec listen))	    
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error listening for process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Detach Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-detach-process () 
  "Detaches the debugger from the target process. The target process continues
to run."
  (interactive)
  (jde-bug-finish-process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Process Command                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-suspend-process () 
"Suspends the target process. To suspend a particular thread or thread group,
use JDEbug->Threads->Suspend Thread (`jde-bug-suspend-thread')."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (suspend-command
	  (jde-dbs-suspend-thread "suspend process" :process process)))
    (jde-dbs-cmd-exec suspend-command)))


(defun jde-bug-resume-process () 
"Resumes the target process. To resume a particular thread or thread group,
use JDEbug->Threads->Resume Thread (`jde-bug-resume-thread')."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (resume-command
	  (jde-dbs-resume-thread "resume process" :process process)))
    (jde-dbs-cmd-exec resume-command)))


(defun jde-bug-finish-process () 
  "Terminates the target process."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (finish (jde-dbs-finish-process 
		  (format "finish %d" (oref process id))
		  :process process))
	 (result (jde-dbs-cmd-exec finish)))
    (jde-dbs-proc-move-to-morgue process)
    (slot-makeunbound jde-dbs-the-process-registry :target-process)
    (jde-dbs-proc-set-state-reason process "finish")))

(defun jde-bug-set-target-process (process-id) 
  "Sets the process whose process-id is PROCESS-ID to be
the focus of debugger commands."
  (interactive
   "nEnter process id: ")
  (jde-dbs-proc-registry-set-target-proc 
   jde-dbs-the-process-registry process-id))
  
	  
(defun jde-bug-show-processes () 
  (interactive)
  (message "not implemented"))


(defun jde-bug-remove-dead-processes () 
  "Remove dead processes and their associated buffers from the Emacs environment."
  (interactive)
  (if (oref jde-dbs-the-process-morgue proc-alist)
      (jde-dbs-proc-morgue-bury-the-dead jde-dbs-the-process-morgue)))


(defun jde-bug-show-locals-buf ()
  "Show the local variables buffer of the target process.
This command shows the locals buffer in the middle pane of
JDEBug's three-pane frame configuration."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (locals-buf (oref process locals-buf))
	 (source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) locals-buf)
    (select-window source-window)))

(defun jde-bug-show-cli-buf ()
  "Show the command-line interface (CLI) buffer of the target process.
This command shows the CLI buffer in the middle pane of JDEBug's
three-pane window configuration."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	(cli-buf (oref process cli-buf))
	(source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) cli-buf)
    (select-window source-window)))


(defun jde-bug-show-threads-buf ()
  "Show the threads buffer of the target process.
This command shows the threads buffer in the middle
pane of the JDEBug's three-pane window configuration."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	(threads-buf (oref process threads-buf))
	(source-window (selected-window)))
    (set-window-configuration (oref process win-cfg))
    (set-window-buffer (next-window source-window) threads-buf)
    (select-window source-window)))


(defun jde-bug-show-preferences () 
  (interactive)
  (customize-apropos "jde-bug" 'groups))


(defun jde-bug-set-breakpoints (process breakpoints)
  "Sets BREAKPOINTS in PROCESS."
  (mapc
   (lambda (assoc)
     (let* ((breakpoint (cdr assoc))
	    (set-breakpoint (jde-dbs-set-breakpoint
			     (format "set breakpoint%d" 
				     (oref breakpoint id))
			     :process process
			     :breakpoint breakpoint))
	    (result (jde-dbs-cmd-exec set-breakpoint)))))
   breakpoints))

;;;###autoload
(defun jde-bug-debug-app ()
  "*Runs the debugger on the application in the current source buffer."
  (interactive)
  (if (and 
       (jde-bug-jpda-installed-p)
       (not (jde-dbs-debugger-running-p)))
      (jde-dbs-debugger-start jde-dbs-the-debugger))
  (if (jde-dbs-debugger-running-p)
      (let ((result (jde-bug-launch-process)))
	(if result
	    (let ((process (oref jde-dbs-the-process-registry :target-process)))
	      (jde-bug-set-breakpoints process jde-bug-breakpoints)
	      (setq result (jde-bug-continue)))))))

(provide 'jde-bug)

;; $Log: jde-bug.el,v $
;; Revision 1.52  2000/07/28 06:27:44  paulk
;; Committing all modified files.
;;
;; Revision 1.51  2000/07/13 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.50  2000/06/12 08:29:32  paulk
;; Restored jde-bug-install-jdebug-menu for XEmacs compatibility.
;; Added jde-bug-debugger-host-address variable.
;;
;; Revision 1.49  2000/05/26 09:18:20  paulk
;; Added variable jde-bug-raise-frame-p to allow a user to specify
;; whether the Emacs window should pop up when a breakpoint is hit.
;;
;; Revision 1.48  2000/05/10 05:36:49  paulk
;; The JDEbug menu now appears or disappears when you select or deselect JDEbug as the current debugger.
;;
;; Revision 1.47  2000/03/16 05:08:25  paulk
;; Added JDEbug option to jde-db-debugger.
;;
;; Revision 1.46  2000/03/03 07:03:21  paulk
;; Fixed bug where jde-bug-launch-process was returning t even when it failed.
;;
;; Revision 1.45  2000/02/17 06:41:09  paulk
;; Added key bindings for debugger.
;;
;; Revision 1.44  2000/02/16 04:59:48  paulk
;; Implemented color customization for breakpoint marker.
;; Implemented persistent breakpoints.
;;
;; Revision 1.43  2000/02/14 06:19:37  paulk
;; Implemented up and down stack commands.
;;
;; Revision 1.42  2000/02/10 02:53:37  paulk
;; Fixed bug where Display->Threads command was not enabled when debugger
;; was attached to a process.
;;
;; Revision 1.41  2000/02/02 05:49:51  paulk
;; Fixed bug in socket listen command.
;;
;; Revision 1.40  2000/02/01 05:58:44  paulk
;; Added commands for listening for applications needing debug services.
;;
;; Revision 1.39  2000/02/01 04:11:54  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.38  2000/01/15 08:04:07  paulk
;; Added show buffer commands.
;;
;; Revision 1.37  2000/01/02 08:07:55  paulk
;; Added attach process commands.
;;
;; Revision 1.36  1999/12/27 08:01:17  paulk
;; Added show object monitors command.
;;
;; Revision 1.35  1999/12/20 07:52:06  paulk
;; Added cancel watchpoint command.
;;
;; Revision 1.34  1999/12/19 06:54:21  paulk
;; Added watch field command.
;;
;; Revision 1.33  1999/12/14 04:46:02  paulk
;; Added JDEbug->Processes->Remove Dead Processes command.
;;
;; Revision 1.32  1999/12/13 05:54:08  paulk
;; Added jde-bug-vm-executable and jde-bug-jre-home variables.
;; Fixed jde-dbs-launch-process command so that it fails gracefully.
;;
;; Revision 1.31  1999/12/03 08:22:00  paulk
;; Updated JDEbug to run under JDK 1.3beta.
;;
;; Revision 1.30  1999/11/30 05:46:21  paulk
;; Added JDEbug->Display->Path Info command.
;;
;; Revision 1.29  1999/11/29 06:58:41  paulk
;; Added JDEbug->Display->Loaded Classes Command.
;;
;; Revision 1.28  1999/11/27 05:13:49  paulk
;; Added commands for tracing classes.
;;
;; Revision 1.27  1999/11/23 06:37:03  paulk
;; Added Trace->Cancel command.
;;
;; Revision 1.26  1999/11/16 05:58:17  paulk
;; Added trace method commands and skeletons for trace class and cancel
;; trace commands.
;;
;; Revision 1.25  1999/11/04 05:54:07  paulk
;; Added class and method tracing command skeletons.
;;
;; Revision 1.24  1999/10/28 04:18:09  paulk
;; Added interrupt and stop thread commands.
;;
;; Revision 1.23  1999/10/15 05:16:58  paulk
;; Fixed bugs in JDEbug->Exit Debugger.
;;
;; Revision 1.22  1999/10/14 04:59:23  paulk
;; Added Resume Process and Resume Thread commands.
;;
;; Revision 1.21  1999/10/13 08:16:43  paulk
;; Added suspend process and suspend thread commands.
;;
;; Revision 1.20  1999/10/13 06:19:56  paulk
;; Add JDEBug->Threads->Show Threads command
;;
;; Revision 1.19  1999/09/28 04:01:57  paulk
;; Patched to use either gud-find-c-expr or find-c-expr.
;;
;; Revision 1.18  1999/09/16 05:36:59  paulk
;; Added get locals command.
;;
;; Revision 1.17  1999/09/13 05:37:33  paulk
;; Enhanced get array command.
;;
;; Revision 1.16  1999/09/10 06:41:50  paulk
;; Finished first cut at get_object command.
;;
;; Revision 1.15  1999/09/08 05:40:45  paulk
;; Updated debugger code to take advantage of new unbound slot capability
;; of eieio.
;;
;; Revision 1.14  1999/09/07 05:12:35  paulk
;; Added get array command.
;;
;; Revision 1.13  1999/09/05 04:35:34  paulk
;; Added initial implementation of evaluate and display variable commands.
;;
;; Revision 1.12  1999/08/30 07:10:41  paulk
;; Converted clear breakpoint command to OOPS.
;;
;; Revision 1.11  1999/08/28 05:34:19  paulk
;; Improved multiple process handling, window configuration.
;;
;; Revision 1.10  1999/08/27 05:27:52  paulk
;; Provided initial support for multiple processes.
;; Fixed jde-find-data-directory to work on XEmacs with a standard
;; JDE distribution.
;; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
;; Now includes jde-db-option options on vm command-line for process.
;;
;; Revision 1.9  1999/08/24 06:29:43  paulk
;; Reimplemented the constructor for jde-dbs-proc the right way. Renamed
;; jde-bug-counter to jde-bug-breakpoint-counter.
;;
;; Revision 1.8  1999/08/23 06:17:45  paulk
;; Minor bug.
;;
;; Revision 1.7  1999/08/23 05:33:37  paulk
;; Added customization variable jde-bug-jpda-directory. Also check to
;; ensure that this variable specifies a valid path.
;;
;; Revision 1.6  1999/08/23 01:44:25  paulk
;; Updated to use Eric Ludlam's eieio object system.
;;
;; Revision 1.5  1999/08/19 10:18:27  paulk
;; *** empty log message ***
;;
;; Revision 1.4  1999/08/18 01:18:41  paulk
;; Continuing implementation
;;
;; Revision 1.3  1999/08/17 01:06:03  paulk
;; *** empty log message ***
;;
;; Revision 1.2  1999/08/15 23:46:46  paulk
;; Implemented more functionality.
;;
;; Revision 1.1  1999/08/10 09:59:59  paulk
;; Initial revision
;;