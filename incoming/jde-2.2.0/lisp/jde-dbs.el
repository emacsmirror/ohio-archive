;;; jde-dbs.el -- JDEbug Session Interface Functions
;; $Revision: 1.56 $ $Date: 2000/07/28 06:27:45 $ 

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

(require 'jde-dbo) 
(require 'eieio)
(require 'jde-widgets)

(defvar jde-dbs-comint-filter nil
  "Standard comint filter for debugger buffer.")

(defvar jde-dbs-debugger-process-name "jdebug"
"Name of debugger process.")

(defun jde-dbs-get-debugger-process ()
  (get-process jde-dbs-debugger-process-name))


(defvar jde-dbs-debugger-output-buffer-name "*JDEbug Messages*"
"Name of buffer used to display messages from the debugger.")

(defvar jde-dbs-debugger-socket-process-name "jdebug-socket"
"Name of debugger socket process.")

(defun jde-dbs-get-debugger-socket-process ()
  (get-process jde-dbs-debugger-socket-process-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Set                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc-set ()
  ((proc-alist     :initarg :proc-alist
		   :type list
		   :initform nil
		   :documentation
		   "List of active processes"))
  "Class of process sets.")

(defmethod jde-dbs-proc-set-add ((this jde-dbs-proc-set) process)
  (oset this :proc-alist
	(cons 
	 (cons (oref process :id) process)
	 (oref this :proc-alist))))

(defmethod jde-dbs-proc-set-remove ((this jde-dbs-proc-set) process)
  (oset this :proc-alist
	(remove-if
	 (lambda (assoc)
	   (let* ((xproc (cdr assoc))
		  (xid (oref xproc id))
		  (id (oref process id)))
	     (equal xid id)))
	 (oref this proc-alist))))

(defmethod jde-dbs-proc-set-get-proc ((this jde-dbs-proc-set) id)
  (cdr (assq id (oref this :proc-alist))))

(defmethod jde-dbs-proc-set-find ((this jde-dbs-proc-set) field value)
  "Finds the process in the set whose FIELD is equal to VALUE."
  (if (slot-boundp this :proc-alist)
      (cdr (find-if
	(lambda (assoc-x)
	  (let ((process-x (cdr assoc-x)))
	    (equal (oref-engine process-x field) value)))
	(oref this :proc-alist)))))

(defmethod jde-dbs-proc-set-contains-p ((this jde-dbs-proc-set) process)
  (assq (oref process :id) (oref this :proc-alist)))

(defmethod jde-dbs-proc-set-get-size ((this jde-dbs-proc-set))
  "Gets the number of processes in this set."
  (if (slot-boundp this 'proc-alist)
      (length (oref this proc-alist))
    0))
	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Registry                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc-registry (jde-dbs-proc-set)
  ((target-process :initarg :target-process
		   :type jde-dbs-proc
		   :documentation
		   "Process that currently has the debugger command focus."))
  "Class of process registries.")


(defmethod jde-dbs-proc-registry-set-target-proc ((this jde-dbs-proc-registry) &optional id)
  "Sets process specified by ID to be the target process. If ID is not specified, the first
registered process becomes the target process"
  (let ((target-process
	  (if id
	      (let ((process (jde-dbs-proc-set-get-proc this id)))
		(if process
		    (if (jde-dbs-proc-set-contains-p this process)
			process
		      (message "Error: process %s is dead." process-id)
		      nil)
		  (message "Error: process %s does not exist." process-id)
		  nil))
	    (let ((existing-processes 
		   (oref jde-dbs-the-process-registry :proc-alist)))
	      (if existing-processes (cdr (nth 0 existing-processes)))))))
    (when target-process
      (oset this target-process target-process)
      (set-window-configuration (oref target-process win-cfg)))
    target-process))
  

(defvar jde-dbs-the-process-registry 
  (jde-dbs-proc-registry "Process Registry")
  "The debuggee process registry.")

(defun jde-dbs-get-target-process ()
  (and jde-dbs-the-process-registry
       (slot-boundp jde-dbs-the-process-registry :target-process)
       (oref jde-dbs-the-process-registry :target-process)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Morgue                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc-morgue (jde-dbs-proc-set) ()
  "Class of process morgues. A process morgue contains dead or dying processes. 
Their carcasses must be kept around until the debugger stops sending messages
concerning them." )

(defmethod jde-dbs-proc-morgue-bury-the-dead ((this jde-dbs-proc-morgue))
  (mapc 
   (lambda (dead-proc-assoc)
     (let* ((dead-proc (cdr dead-proc-assoc))
	    (cli-buffer (if (slot-boundp dead-proc 'cli-buf) (oref dead-proc cli-buf)))
	    (msg-buffer (if (slot-boundp dead-proc 'msg-buf) (oref dead-proc msg-buf)))
	    (locals-buffer (if (slot-boundp dead-proc 'locals-buf) (oref dead-proc locals-buf)))
	    (threads-buffer (if (slot-boundp dead-proc 'threads-buf) (oref dead-proc threads-buf))))
       (if cli-buffer (kill-buffer cli-buffer))
       (if msg-buffer (kill-buffer msg-buffer))
       (if locals-buffer (kill-buffer locals-buffer))
       (if threads-buffer (kill-buffer threads-buffer))))
   (oref this proc-alist))
  (oset this proc-alist nil))


(defvar jde-dbs-the-process-morgue (jde-dbs-proc-morgue "The JDE Process Morgue")
  "The JDE process morgue. This morgue contains processes that are dead or
dying, for example, because they have been terminated by the user or the
debugger. Their corpses must be kept around until it is clear they are dead and
the debugger ceases sending messages concerning them.")


(defun jde-dbs-get-process (id)
"Get the process whose id is ID. This function looks first in the process registry
and then in the process morgue for the process."
  (let ((process
	 (jde-dbs-proc-set-get-proc jde-dbs-the-process-registry id)))
    (if (not process)
	(setq process (jde-dbs-proc-set-get-proc jde-dbs-the-process-morgue id)))
    process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process State Info                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc-state-info ()
  ((state       :initarg :state)
   (reason      :initarg :reason)
   (thread-id   :initarg :thread-id)
   (thread-name :initarg :thread-name))
  "Class of process state information objects.")


(defmethod jde-dbs-proc-state-info-set ((this jde-dbs-proc-state-info)
					state reason thread-id thread-name)
  (oset this reason reason)
  (oset this state state)
  (oset this thread-id thread-id)
  (oset this thread-name thread-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Breakpoint Specification                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc-bpspec ()
  ((id         :initarg :id
	       :type integer
	       :documentation 
	       "Id assigned to this breakpoint by the debugger.")
   (breakpoint :initarg :breakpoint
	       :type jde-bug-breakpoint
	       :documentation
	       "Instance of `jde-bug-breakpoint'.")
   (resolved   :initarg :resolved))
  (:allow-nil-initform t)
  "Class of breakpoint specifications. A breakpoint specification contains 
process-specific information about a breakpoint")


;; Defines a class of containers for breakpoint specs.
;; Each container lists the process specs for breakpoints set in a
;; particular process.

(defun jde-dbs-proc-bpspecs-add (bpspecs bpspec)
  "Adds BPSPEC to BPSPECS, a process's breakpoint spec list."
  (cons 
   (cons (oref bpspec id) bpspec)
   bpspecs))

(defun jde-dbs-proc-bpspecs-remove (bpspecs bpspec)
  "Removes BPSPEC from BPSPECS"
  (remove-if (lambda (x) 
	       (equal (car x) (oref bpspec id) ))
	     bpspecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Request Class                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-request ()
  ((id                  :initarg :id
	                :type integer
		        :documentation
		        "Trace request id")
   (suspend-policy      :initarg :suspend-policy
		        :type string
		        :initform "none"
		        :documentation
		        "Valid values are all (all threads), thread (current thread), or none")
   (inclusion-filters   :initarg :inclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to include in trace.")
   (exclusion-filters   :initarg :exclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to exclude from trace.")
   (cancel-command      :initarg :cancel-command
			:type string
			:documentation
			"Name of command used to cancel this request.")
   )
"Super class of trace requests."
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Method Request Class                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-methods-request (jde-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "entry"
			:documentation 
			"Entry or exit.")
   (thread-restriction  :initarg :thread-restriction
	                :type string
			:documentation
			"Thread to trace."))
   "Trace methods request."
)

(defmethod initialize-instance ((this jde-dbs-trace-methods-request) &rest fields)
  "Constructor for objects of `jde-dbs-trace-methods-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_methods"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Request Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-classes-request (jde-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "preparation"
			:documentation 
			"Valid values are preparation or unloading."))
   "Trace classes request."
)

(defmethod initialize-instance ((this jde-dbs-trace-classes-request) &rest fields)
  "Constructor for objects of `jde-dbs-trace-classes-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_classes"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Request Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-exceptions-request (jde-dbs-trace-request)
  ((exception-class    :initarg :exception-class
		       :type string
		       :documentation
		       "Class of exceptions to trace. Can be a wild card pattern.")
   (trace-type         :initarg :trace-type
		       :type string
		       :initform "both"
		       :documentation 
			"Valid values are caught, uncaught, or both."))
   "Trace exceptions request."
)

(defmethod initialize-instance ((this jde-dbs-trace-exceptions-request) &rest fields)
  "Constructor for objects of `jde-dbs-trace-exceptions-request' class."
  (call-next-method)
  (oset this cancel-command "clear"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Request Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-watch-field-request (jde-dbs-trace-request)
  ((watch-type         :initarg :watch-type
		       :type string
		       :documentation
		       "Valid values are \"access\" and \"modification\".")
   (object-class       :initarg :object-class
		       :type string
		       :documentation
		       "Class of object to watch. Can be a wild card pattern.")
   (field-name         :initarg :field-name
		       :type string
		       :documentation 
			"Name of field to watch.")
   (expression         :initarg :expression
		       :type string
		       :documentation 
		       "Boolean expression that must be satisfied to suspend execution.")
   (object-id          :initarg :object-id
		       :type string
		       :documentation 
		       "Id of object to watch."))
   "Watch field request."
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Application Process Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-proc ()
  ((id            :initarg :id
                  :type integer
                  :documentation
                  "Id assigned by the JDE.")
   (main-class    :initarg :main-class
                  :type string
                  :documentation
                  "Main class for this process.")
   (cli-socket    :initarg :cli-socket
                  :type integer
	          :documentation
                  "Number of socket used by the process's command line interface.")
   (cli-buf       :initarg :cli-buf
                  :type buffer
	          :documentation
	          "Buffer for the process's command-line interface.")
   (msg-buf       :initarg :msf-buf
	          :type buffer
	          :documentation
	          "Buffer used to display debugger output for this process")
   (threads-buf   :initarg :threads-buf
		  :type buffer
		  :documentation
		  "Buffer used to display threads.")
   (locals-buf    :initarg :locals-buf
		  :type buffer
		  :documentation
		  "Buffer used to display local variables.")
   (startupp       :initarg :startupp
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process is in the startup state.")
   (suspendedp    :initarg :suspendedp
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process has been suspended by the debugger.")
   (steppablep    :initarg :steppablep
                  :type boolean
                  :initform nil
                  :documentation
		  "non-nil if this process can be single-stepped.")
   (state-info    :initarg :state-info
	          :type jde-dbs-proc-state-info
	          :documentation
	          "Process state information.")
   (stack         :initarg :stack
		  :type list
		  :documentation
		  "Lists stack frames for thread of current step or breakpoint.")
   (stack-ptr     :initarg :stack-ptr
		  :type integer
		  :initform 0
		  :documentation
		  "Points to the current frame on the stack.")
   (trace-req     :initarg :trace-req
		  :type list
		  :documentation
                  "List of outstanding trace requests.")
   (watch-req     :initarg :watch-req
		  :type list
		  :documentation
                  "List of outstanding watch field requests.")
   (object-refs   :initarg :object-refs
		  :type list
		  :initform nil
		  :documentation
		  "IDs of debuggee objects currently referenced by the debugger.")
   (bpspecs       :initarg :bpspecs
	          :type list
	          :documentation
                  "Breakpoints set in this process.")
   (last-cmd      :initarg :last-cmd
	          :type jde-dbs-cmd
	          :documentation
                  "Most recent command targeting this process.")
   (win-cfg       :initarg :win-cfg
	          :type window-configuration
	          :documentation
	          "Desired window configuration for this process.")
   (attachedp     :initarg :attachedp
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if the debugger was attached to this process."))
  (:allow-nil-initform t)
  "Class of debuggee processes.")

(defmethod initialize-instance ((this jde-dbs-proc) &rest fields)
  "Constructor for objects of `jde-dbs-proc' class."
  (call-next-method)

  (if (not (slot-boundp this 'state-info))
      (oset this state-info 
	    (jde-dbs-proc-state-info 
	     (format "State Info %d" (oref this id)))))

  (assert (slot-boundp this 'main-class))
  (assert (slot-boundp this 'id))
  
  (oset this msg-buf (get-buffer-create 
		      (format "Process %s(%d)" 
			      (oref this main-class)
			      (oref this id))))
  (save-excursion
    (set-buffer (oref this msg-buf))
    (erase-buffer)	
    (goto-char (point-min))
    (insert 
       (format "*** Debugger Output for Process %s(%d) ***\n\n" 
	       (oref this main-class)
	       (oref this id))))

  (oset this locals-buf (get-buffer-create
			(format "%s(%d) Local Variables"
				(oref this main-class)
				(oref this id))))

  (oset this threads-buf (get-buffer-create
			  (format "%s(%d) Threads"
				  (oref this main-class)
				  (oref this id)))))


(defmethod jde-dbs-proc-set-state ((this jde-dbs-proc) state)
  (let ((state-info (oref this state-info)))
    (oset state-info state state)))

(defmethod jde-dbs-proc-set-state-reason ((this jde-dbs-proc) reason)
  (let ((state-info (oref this state-info)))
    (oset state-info reason reason)))

(defmethod jde-dbs-proc-get-state ((this jde-dbs-proc))
  (oref (oref this state-info) state))

(defmethod jde-dbs-proc-get-state-reason ((this jde-dbs-proc))
  (oref (oref this state-info) reason))

; (defmethod jde-dbs-proc-display-debug-message ((this jde-dbs-proc) message)
;   (let ((buffer
; 	 (oref this msg-buf)))
;     (if buffer
; 	(save-excursion
; 	  (set-buffer buffer)
; 	  (goto-char (point-max))
; 	  (insert (concat message "\n"))))))

(defmethod jde-dbs-proc-display-debug-message ((this jde-dbs-proc) message)
  (let ((buffer
  (oref this msg-buf)))
    (if buffer
 (save-excursion
   (let ((currbuffp (equal buffer (current-buffer))))
     (if (not currbuffp) (other-window -1))
     (set-buffer buffer)
     (goto-char (point-max))
     (insert (concat message "\n"))
     (goto-char (point-max))
     (if (not currbuffp) (other-window 1)))))))


(defmethod jde-dbs-proc-move-to-morgue ((this jde-dbs-proc))
  "Moves this process from the process registry to the process morgue."
  (jde-dbs-proc-set-remove jde-dbs-the-process-registry this)
  (jde-dbs-proc-set-add jde-dbs-the-process-morgue this))

(defmethod jde-dbs-proc-move-to-registry ((this jde-dbs-proc))
  "Moves this process from the registry to the morgue."
  (jde-dbs-proc-set-remove jde-dbs-the-process-morgue this)
  (jde-dbs-proc-set-add jde-dbs-the-process-registry this))


(defmethod jde-dbs-proc-get-bpspec ((this jde-dbs-proc) bp)
  "Gets the process specification for a breakpoint. BP may be either
an instance of `jde-bug-breakpoint' or the debugger-assigned id
for the breakpoint."
  (let ((bpspecs (oref this bpspecs)))
    (if (and (object-p bp) (jde-bug-breakpoint-p bp))
	(let* ((jde-id (oref bp id)))
	  (cdr
	   (find-if
	    (lambda (assoc)
	      (let ((spec (cdr assoc)))
		(equal (oref (oref spec breakpoint) id) jde-id)))
	    bpspecs)))
      (cdr (assoc bp bpspecs)))))

(defmethod jde-dbs-proc-runnable-p ((this jde-dbs-proc))
  (or
   (oref this startupp)
   (oref this suspendedp)
   (oref this steppablep)))

(defun jde-dbs-target-process-runnable-p ()
  (interactive)
  (let ((target (jde-dbs-get-target-process)))
    (and target (jde-dbs-proc-runnable-p target))))

(defun jde-dbs-target-process-steppable-p ()
  (interactive)
  (let ((target (jde-dbs-get-target-process)))
    (and target (oref target steppablep))))

(defun jde-dbs-display-debug-message (proc-id message)
  (let ((process (jde-dbs-get-process proc-id)))
    (if process 
	(jde-dbs-proc-display-debug-message process message)
      (message message))))

(defvar jde-dbs-proc-counter 0
  "Process counter. Used to generate process IDs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java Object                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-java-obj ()
  ((jtype  :initarg :jtype
	   :type string
	   :documentation
	  "Type of this object."))
  "Superclass of Java objects.")

(defmethod jde-dbs-java-obj-to-string ((this jde-dbs-java-obj))
  "")


(defclass jde-dbs-java-primitive (jde-dbs-java-obj)
  ((value :initarg :value
	  :type (or string number)
	  :documentation
	  "Value of this primitive object."))
  "Class of Java primitives.")

(defmethod jde-dbs-java-obj-to-string ((this jde-dbs-java-primitive))
  (format "%s" (oref this value)))

(defclass jde-dbs-java-null (jde-dbs-java-obj) ()
  "Java null object.")

(defmethod initialize-instance ((this jde-dbs-java-null) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (oset this jtype "null"))


(defmethod jde-dbs-java-obj-to-string ((this jde-dbs-java-null))
  "null")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java Variable                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-java-variable ()
  ((name         :initarg :name
		 :type string
		 :documentation
		 "Name of this variable")
   (jtype        :initarg :jtype
		 :type string
		 :documentation
		 "Type of this variable.")
   (value        :initarg :value
                 :type jde-dbs-java-obj
		 :documentation
		 "Value of this variable."))
  "Class that defines the JDE's representation of a Java variable.")

(defmethod jde-dbs-java-variable-to-string ((this jde-dbs-java-variable))
  (format "%s %s = %s"
	  (oref this jtype)
	  (oref this name)
	  (jde-dbs-java-obj-to-string (oref this value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java Class Instance                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-java-class-instance (jde-dbs-java-obj)
  ((id           :initarg :id
		 :type integer
		 :documentation
		 "Id assigned to this object by the debugger.")
   (gc-flag      :initarg :gc-flag
		 :type boolean
		 :documentation
		 "t if this object has been garbage collected."))
  "Instance of a Java class accessed via the debugger.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java Array                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-java-array (jde-dbs-java-class-instance)
  ((length     :initarg :length
	       :type integer
	       :documentation
	       "Length of this array.")
   (elements   :initarg :elements
	       :type list
	       :initform nil
	       :documentation
	       "Elements of this array."))
  "Class of Lisp objects representing instances of Java arrays.")



(defmethod jde-dbs-java-obj-to-string ((this jde-dbs-java-array))
  (let ((str (format "<%s:%d%s> %d" 
		     (oref this jtype)
		     (oref this id)
		     (if (oref this gc-flag) ":gc" "")
		     (oref this length)))
	(elements (oref this elements)))
    (if elements
	(let ((sep "\n |- "))
	  (concat 
	   str
	   sep
	   (mapconcat
	    (lambda (element) 
	      (jde-dbs-java-obj-to-string element))
	    elements sep)))
      str)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java User-Defined Class Instance                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-java-udci (jde-dbs-java-class-instance)
  ((fields       :initarg :fields
		 :type list
		 :initform nil
		 :documentation
		 "Fields of this object."))
  "Class of Lisp objects representing instances of user-defined Java classes.")


(defmethod jde-dbs-java-udci-add-field ((this jde-dbs-java-udci) field)
  (oset this fields
	(nconc (oref this fields) (list (cons (oref field name) field)))))


(defmethod jde-dbs-java-obj-to-string ((this jde-dbs-java-udci))
  (let ((str (format "<%s:%d%s>" 
		     (oref this jtype)
		     (oref this id)
		     (if (oref this gc-flag) ":gc" "")))
	(fields (oref this fields)))
    (if fields
	(let ((sep "\n |- "))
	  (concat 
	   str
	   sep
	   (mapconcat
	    (lambda (assoc) 
	      (jde-dbs-java-variable-to-string (cdr assoc)))
	    fields sep)))
      str)))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Debugger Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-debugger ()
  ((name          :initarg :name
	          :initform "JDEbug"
		  :type string
		  :documentation
		  "Name of debugger.")
   (buffer-name   :initarg :buffer-name
                  :initform "*JDEbug*"
		  :type string
		  :documentation
		  "Name of buffer used to interact with debugger.")
   (buffer        :initarg :buffer
		  :type buffer
		  :documentation
		  "Buffer used to interact with debugger.")
   (process       :initarg :process)
   (comint-filter :initarg :comint-filter)
   (started-p     :initarg :started-p
		  :initform nil
		  :type boolean
		  :documentation
		  "True if debugger started successfully."))
  "Class of debuggers.")

(defmethod jde-dbs-debugger-register-process-filter ((debugger jde-dbs-debugger) filter)
  "Set the process filter for the debugger to FILTER."
  (set-process-filter  (oref debugger process) filter))


(defmethod jde-dbs-debugger-display-message ((debugger jde-dbs-debugger) message)
  "Displays message in the debugger process buffer."
 (let ((buffer
	 (oref debugger buffer)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point))
	  (insert-before-markers (concat message "\n"))))))



(defmethod jde-dbs-debugger-start((this jde-dbs-debugger))
  "Starts the debugger."
  (if (not (jde-dbs-debugger-running-p))
      (let* ((debugger-buffer-name 
	      (oref this buffer-name))
	     (debugger-buffer 
	      (let ((old-buf (get-buffer debugger-buffer-name)))
		    (if old-buf (kill-buffer old-buf))
		    (get-buffer-create debugger-buffer-name)))
	     (win32-p (eq system-type 'windows-nt))
	     (source-directory default-directory)
	     (working-directory
	      (if (and 
		   jde-run-working-directory
		   (not (string= jde-run-working-directory "")))
		  jde-run-working-directory
		source-directory))	     
	     (vm (jde-dbs-choose-vm))
	     (jde-java-directory
	      (concat
	       (jde-find-jde-data-directory)
	       "java/"))
	     (vm-args 
		(let (args)
		  (setq args 
			(append 
			 args
			 (list
			  "-classpath"
			  (jde-convert-cygwin-path
                           (if jde-bug-vm-includes-jpda-p
                               (format
                                (if jde-bug-debug
                                    "%sclasses%s%s"   
                                  "%slib/jde.jar%s%s")
                                jde-java-directory
                                jde-classpath-separator
                                (expand-file-name 
                                 "lib/tools.jar" jde-bug-jdk-directory))
                             (format 
                              (if jde-bug-debug
                                  "%sclasses%s%s"   
                                "%ssrc%s%slib/jde.jar%s%s" )
                              jde-java-directory 
                              jde-classpath-separator
                              jde-java-directory 
                              jde-classpath-separator
                              (expand-file-name 
                               "lib/jpda.jar" jde-bug-jpda-directory)))
                           jde-classpath-separator))))
		  (if jde-bug-debug
		      (setq args 
			    (append args
			     (list "-Xdebug"
				   "-Xnoagent"   
				   "-Xrunjdwp:transport=dt_socket,address=2112,server=y,suspend=n"))))
		  (setq args (append args (list "jde.debugger.Main")))
		  args))		  
	     (command-string 
	      (concat 
	       vm " " 
	       (jde-run-make-arg-string
		vm-args)
	       "\n\n"))
	     debugger-process)
	(oset this started-p nil)
	(setq jde-dbs-debugger-output nil)


	(save-excursion
	  (set-buffer debugger-buffer)
	  (erase-buffer)
	  ;; Set working directory
	  (if (and
	       (file-exists-p working-directory)
	       (file-directory-p working-directory))
	      (cd working-directory)
	    (error "Invalid working directory: %s" working-directory))
	  (insert (concat "cd " working-directory "\n"))
	  (insert command-string)
	  (jde-run-mode))

	(save-w32-show-window
	 (comint-exec debugger-buffer debugger-buffer-name vm nil vm-args)
	 (setq debugger-process (get-process debugger-buffer-name))
	 (oset this process debugger-process)
	 (oset this buffer debugger-buffer)
	 (oset this comint-filter (process-filter debugger-process))
	 (jde-dbs-debugger-register-process-filter this 'jde-dbs-asynch-output-listener)
	 )

	(cd source-directory)

	(bury-buffer debugger-buffer)
	
	;; Wait for response from debugger
	(if (not (accept-process-output debugger-process jde-bug-debugger-command-timeout 0))
	    (progn
	      (message "Error: debugger failed to start.")
	      nil)
	  (oref this started-p)))
    (message "An instance of the debugger is running.")
    (pop-to-buffer (jde-dbs-get-app-buffer-name))
    nil))
    
(defmethod jde-dbs-debugger-quit ((debugger jde-dbs-debugger)) 
  (jde-dbs-do-command -1 "quit")
  (slot-makeunbound debugger :process)
  (slot-makeunbound debugger :buffer)
  (slot-makeunbound debugger :comint-filter))

(defun jde-dbs-debugger-running-p ()
  "*Returns t if the debugger is running."
  (and (slot-boundp jde-dbs-the-debugger 'buffer)
       (oref jde-dbs-the-debugger started-p)
       (comint-check-proc (oref jde-dbs-the-debugger buffer))))

(defvar jde-dbs-the-debugger (jde-dbs-debugger "JDEbug")
  "The debugger.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debugger Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-cmd ()
  ((process    :initarg :process
	       :type jde-dbs-proc
	       :documentation
	       "Process that this command targets.")
   (id         :initarg :id
	       :type integer
	       "Command id.")
   (name       :initarg :name
               :type string
	       :documentation
	       "Name of command.")
   (result     :initarg :result
	       "Result of executing command.")
   (data       :initarg :data
	       "Data returned by command.")
   (msg        :initarg :msg
	       :type string
	       "Message to display to user in debug buffer.")
   )
  "Super class of debugger commands.")
 

(defmethod initialize-instance ((this jde-dbs-cmd) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."
  (call-next-method)
  (setq jde-dbs-cmd-counter (+ jde-dbs-cmd-counter 1))
  (oset this id jde-dbs-cmd-counter))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-cmd))
  "Creates the command line for this command by concatentating
the process id, command id, and command name. If there is no
process, specifies the process id as -1. Derived classes can
extend this method to specify command arguments."
  (let* ((process (oref this process))
	 (process-id (if process (oref process id) -1))
	 (command-id (oref this id))
	 (command-name (oref this name)))
    (format "%s %s %s" process-id command-id command-name)))
    
(defvar jde-dbs-debugger-output nil
  "Contains output from the debugger.")

(defvar jde-dbs-command-reply nil
  "Contains reply to a debugger command.")

(defvar jde-dbs-pending-command 0
"Number of the current command.")

(defun jde-dbs-eval-debugger-output (lisp-form)
  (condition-case error-desc
      (eval (read lisp-form))
    (error 
     (let* ((process (jde-dbs-get-target-process)))
       (if process
	   (jde-dbs-proc-display-debug-message 
	    process 
	    (concat
	     "Error: evaluating debugger output caused a Lisp error.\n"
	     "  See *messages* buffer for details.")))
       (message "Error: evaluating output from the debugger caused a Lisp error.")
       (message "Debugger output: %s." lisp-form)
       (message "Lisp error: %s" error-desc)))))

(defun jde-dbs-extract-exception (debugger-output)
  (let ((lisp-form "")
	(remainder "")
	(output-length (length debugger-output))
	(re "\\(.*Exception:.*[\n]\\)+\\(.*at[^\n]*[\n]\\)+"))
    (if (string-match re debugger-output)
	(let ((start (match-beginning 0))
	      (end (match-end 0)))
	  (setq lisp-form (format "(jde-dbo-unknown-exception \"%s\")" 
				  (substring debugger-output 0 end)))
	  (if (< end output-length)
	      (setq remainder (substring debugger-output end output-length))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun jde-dbs-extract-lisp-form (debugger-output)
"Extract first complete Lisp form from debugger output.
Returns (FORM . REMAINDER) where FORM is the Lisp form
or the null string and REMAINDER is the remainder of the
debugger output following the Lisp form."
  (let ((lisp-form "")
	(remainder "")
	(level 0)
	in-string-p
	in-escape-p
	(curr-pos 1)
	(output-length (length debugger-output))
	command-end
	lisp-form-end)
    (setq 
     lisp-form-end
     (catch 'found-lisp-form
       ;; skip over any inital white space.
       (string-match "^[\n\t ]*(" debugger-output)
       (setq curr-pos (match-end 0))

       (while (< curr-pos output-length)

	 (cond 

	  ;; Current character = left slash (escape)
	  ((equal (aref debugger-output curr-pos) ?\\)
	   (if in-string-p
	       (setq in-escape-p (not in-escape-p))))
	  
	  ;; Current character = quotation mark
	  ((equal (aref debugger-output curr-pos) ?\")
	   (if in-string-p
	       (if in-escape-p
		   (setq in-escape-p nil)
		 (setq in-string-p nil))
	     (setq in-string-p t)))

	  ;; Current character = right paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\)))
	     (if (= level 0)
		 (throw 'found-lisp-form curr-pos)
	       (setq level (1- level))
	       (if (< level 0)
		   (error "Error parsing debugger output."))))

	  ;; Current character = left paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\()
	       (setq level (1+ level))))
	  (t
	   (if in-escape-p
	       (setq in-escape-p nil))))

	 (setq curr-pos (1+ curr-pos)))

       -1))
    (if (> lisp-form-end 1)
	(progn
	  (setq lisp-form (substring debugger-output 0 (1+ lisp-form-end)))
	  (when (< lisp-form-end (1- output-length))
	    (setq remainder (substring debugger-output (1+ lisp-form-end) output-length))
	    (if (string-match "(" remainder)
		(setq remainder (substring remainder (string-match "(" remainder)))
	      (setq remainder ""))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun jde-dbs-reply-p (form)
  "Returns t if FORM is a command response form."
  (or
   (string-match "jde-dbo-command-result" form)
   (string-match "jde-dbo-command-error" form)))

(defvar jde-dbs-pending-event-queue nil
"Queue of events that occurred before receiving a reply to the last command.")

(defun jde-dbs-command-reply-listener (process output)
  "Listens at the jdebug socket for a reply to the command specified by
`jde-dbs-pending-command'."
  ;; (message "entering command reply listener")
  (let* ((combined-output (concat jde-dbs-debugger-output output))
	 (parsed-output 
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (jde-dbs-extract-lisp-form combined-output)
	    (jde-dbs-extract-exception combined-output)))			 
	 (form (car parsed-output))
	 (remainder (cdr parsed-output))
	 reply-received)
    ;; (message "form: %s" form)
    ;; (message "remainder: %s" remainder)
    (funcall (oref jde-dbs-the-debugger  comint-filter)
	 process output)
    (while (not (string= form ""))
      (if (jde-dbs-reply-p form) 
	  (progn 
	    (setq jde-dbs-command-reply form)
	    (setq reply-received t))
	;; (message "   appending %s to pending event queue" form)
	;; (jde-dbs-eval-debugger-output form)
	(if (not reply-received)
	    (setq jde-dbs-pending-event-queue
		  (append jde-dbs-pending-event-queue (list form)))
	  (jde-dbs-eval-debugger-output form)))
      (setq parsed-output
	    (jde-dbs-extract-lisp-form remainder))
      (setq form (car parsed-output))
      (setq remainder (cdr parsed-output)))
    (setq jde-dbs-debugger-output remainder)
    (if (not reply-received)
	(when (not (accept-process-output process jde-bug-debugger-command-timeout 0))
	    (message "No response to command %d. (process = %s; timeout = %s sec.)"
		     jde-dbs-pending-command
		     (if (jde-dbs-get-target-process)
			 (oref (jde-dbs-get-target-process) id)
		       "?")
		     jde-bug-debugger-command-timeout)
		    (setq jde-dbs-command-reply nil)))))
	
(defun jde-dbs-asynch-output-listener (process output)
  "Listens at the jdebug socket for asynchronous debugger output."
  (let* ((combined-output (concat jde-dbs-debugger-output output))
	 (parsed-output 
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (jde-dbs-extract-lisp-form combined-output)
	    (jde-dbs-extract-exception combined-output)))		
	 (lisp-form (car parsed-output))
	 (remainder (cdr parsed-output))
	 events)

    ;; (message "asynch form: %s" lisp-form)
    ;; (message "asynch remainder: %s" remainder)

    (funcall (oref  jde-dbs-the-debugger comint-filter)
	     process output)
    ;; Extract events from debugger output.
    (while (not (string= lisp-form ""))
      ;; (message "   evaluating %s" lisp-form)
      ;; (jde-dbs-eval-debugger-output lisp-form)
      (setq events (append events (list lisp-form)))
      (setq parsed-output
	    (jde-dbs-extract-lisp-form remainder))
      (setq lisp-form (car parsed-output))
      (setq remainder (cdr parsed-output)))
    (setq jde-dbs-debugger-output remainder)
    (if events
	(mapc (lambda (event) (jde-dbs-eval-debugger-output event))
	      events))))

(defun jde-dbs-do-command (vm command)
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process 
	  (oref jde-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 cmd)	
    (setq jde-dbs-debugger-output "")
    (setq jde-dbs-command-reply "")
    (setq jde-dbs-pending-events nil)
    (setq jde-dbs-cmd-counter (+ jde-dbs-cmd-counter 1))
    (setq jde-dbs-pending-command (number-to-string jde-dbs-cmd-counter))
    (setq cmd (concat (number-to-string vm) " " jde-dbs-pending-command " " command "\n\n"))
    (jde-dbs-debugger-display-message jde-dbs-the-debugger (concat "JDE> " cmd))
    (set-process-filter debugger-process 'jde-dbs-command-reply-listener)
    (process-send-string debugger-process cmd)
    (when (not (accept-process-output debugger-process jde-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" cmd)
		(setq jde-dbs-command-reply nil))
    (set-process-filter debugger-process previous-listener)
    (if jde-dbs-command-reply
	(let ((result (jde-dbs-eval-debugger-output jde-dbs-command-reply)))
	  ;; evaluate any events that occurred between issuance and 
	  ;; acknowledgement of this command
	  (mapc (lambda (event) (jde-dbs-eval-debugger-output event))
		jde-dbs-pending-event-queue)
	  (setq jde-dbs-pending-event-queue nil)
	  result))))



(defvar jde-dbs-debugger-socket-number nil
"Number of socket used to communicate with debugger.")


(defun jde-dbs-listen-for-debugger-socket (debugger-process output)
  (set-process-filter debugger-process jde-dbs-app-buffer-filter)
  (eval (read output)))

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-cmd)))


(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-cmd)))


(defmethod jde-dbs-cmd-display-response ((this jde-dbs-cmd))
  (if (slot-boundp this 'msg)
      (jde-dbs-proc-display-debug-message 
       (oref this process)
       (oref this msg))))

(defmethod jde-dbs-cmd-execute-pending-events ((this jde-dbs-cmd))
  "Evaluate any events that occurred between issuance and 
   acknowledgement of this command"
  (let ((events jde-dbs-pending-event-queue))
    ;; Empty queue to avoid recursion if commands are executed
    ;; as a result of processing these events.
    (setq jde-dbs-pending-event-queue nil)
    (mapc (lambda (event) (jde-dbs-eval-debugger-output event))
		events)))


(defmethod jde-dbs-cmd-exec ((this jde-dbs-cmd))
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process 
	  (oref jde-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 (target-process (oref this process))
	 (command-line (format "%s\n" (jde-dbs-cmd-make-command-line this))))	
    (setq jde-dbs-debugger-output "")
    (setq jde-dbs-command-reply "")
    (setq jde-dbs-pending-events nil)
    (setq jde-dbs-pending-command (oref this id))
    (if target-process (oset target-process last-cmd this))
    (jde-dbs-debugger-display-message jde-dbs-the-debugger (concat "JDE> " command-line))
    (set-process-filter debugger-process 'jde-dbs-command-reply-listener)
    (process-send-string debugger-process command-line)
    (process-send-string debugger-process "\n")
    (when (not (accept-process-output debugger-process jde-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" command-line)
		(setq jde-dbs-command-reply nil))
 
   (set-process-filter debugger-process previous-listener)

    (if jde-dbs-command-reply
	(let ((result (jde-dbs-eval-debugger-output jde-dbs-command-reply)))

	  (oset this :result result)

	  (oset this :data (car (jde-dbo-command-result-data (oref this result))))

	  (if (jde-dbo-command-succeeded-p result)
	      (jde-dbs-cmd-success-action this)
	    (jde-dbs-cmd-failure-action this))

	  (jde-dbs-cmd-display-response this)

	  (jde-dbs-cmd-execute-pending-events this)
	  (oref this :result)))))

(defvar jde-dbs-cmd-counter 0
 "Count of the number of commands issued in this session.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-launch-process (jde-dbs-cmd)
  ((main-class  :initarg :main-class
		:type string
		:documentation
		"Class containing this process's main method.")
   (jre-home    :initarg :jre-home
		:type string
		:documentation
		"Home directory of JRE used to launch this process.")
   (vmexec     :initarg :vmexec
	        :type string
		:initform "java"
	        :documentation
	        "Name of vm executable used to run process.")
   (vm-args     :initarg :args
	        :type string
		:initform ""
	        :documentation
		"Command line arguments to be passed to vm's main method.")
   (app-args    :initarg :app-args
		:type string
		:initform ""
		:documentation
		"Command line arguments to be passed to app's main method."))
  "Class of launch-process commands.")

(defun jde-dbs-choose-vm ()
  (if (and
       (eq system-type 'windows-nt)
       (string= jde-run-java-vm "java"))
      jde-run-java-vm-w
    jde-run-java-vm))


(defun jde-dbs-get-app-buffer-name ()
  (concat "*" (jde-run-get-main-class) "*"))
    
(defmethod initialize-instance ((this jde-dbs-launch-process) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "launch")

  ;; You must specify a process to launch when constructing a launch command."
  (assert (slot-boundp this :process))

  ;; Set main class.
  (if (not (slot-boundp this :main-class))
    (oset this :main-class
	  (oref (oref this :process) :main-class)))

  ;; Set vm.
  ;; (oset this vm (jde-dbs-choose-vm))

  ;; Set vm args
  (oset this vm-args 
 	(concat (mapconcat (lambda (s) s) (jde-db-get-vm-args) " ")
		" "
 		(mapconcat (lambda (s) s) (jde-db-get-vm-args-from-user) " ")))


  ;; Set application arguments.
  (oset this app-args
 	(concat 
	 (if jde-db-option-application-args
	     (mapconcat (lambda (s) s) jde-db-option-application-args " ") 
	   "")
	 " "
	 (mapconcat (lambda (s) s) (jde-db-get-app-args-from-user) " "))))
  


(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-launch-process))
  "Creates the command line for the launch command."
  (let ((cmd (format "-1 %s %s %s -vmexec %s"
		     (oref this id)                    ;; cid
		     (oref this name)                  ;; launch
		     (oref (oref this process) id)     ;; pid
		     (oref this vmexec ))))  

    (if (slot-boundp this 'jre-home)
	(setq cmd (concat cmd " -home " (oref this jre-home))))
		     
    (setq cmd 
	  (format "%s %s %s %s" 
		  cmd
		  (oref this vm-args)            ;; vm args
		  (oref this main-class)         ;; main class
		  (oref this app-args)))         ;; command line args
    ))    

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-launch-process))
  (call-next-method)
  (delete-other-windows)
  (let* ((source-buffer (current-buffer))
	 (cli-socket
	  (car (jde-dbo-command-result-data result)))
	 (cli-buffer-name 
	  (format "%s(%d) CLI" main-class (oref process id)))
	 (cli-buffer
	  (get-buffer-create cli-buffer-name)))

    (oset (oref this process) cli-socket cli-socket)

    ;; Connect to socket used by debugger to transport the
    ;; standard I/O of the debuggee process.
    (oset 
     (oref this process)
     cli-buf
     (make-comint 
      cli-buffer-name 
      (cons jde-bug-debugger-host-address cli-socket)))
	  
    (oset this msg
	  (format "Emacs connected to standard IO port %d for process %s." 
		  cli-socket
		  (oref this main-class)))

    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (split-window-vertically)
    (pop-to-buffer (oref process locals-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-launch-process))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
	  (format "Error: debugger unable to launch %s.\n  Reason: %s" 
		  (oref this main-class)
		  (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Shared Memory                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-attach-shmem (jde-dbs-cmd)
  ((process-name  :initarg :process-name
		  :type string
		  :documentation
		  "Name of process to attach."))
  "Attach debugger to a running process via shared memory.")

(defmethod initialize-instance ((this jde-dbs-attach-shmem) &rest fields)
  "Constructor for attach_shmem command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'process-name))

  ;; Set command name.
  (oset this name "attach_shmem"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-attach-shmem))
  "Creates the command line for the attach_shmem command."
  (format "-1 %s %s %s %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this process-name)))       ;; process name   

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-attach-shmem))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset (oref this process) :attachedp t)
    (oset (oref this process) :startupp t)
    (oset this msg  (format "Attached to process %s." 
			    (oref this process-name)))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-attach-shmem))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach process %s.\n Reason: %s." 
		    (oref this process-name)
		    (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process via Socket                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-attach-socket (jde-dbs-cmd)
  ((port  :initarg :port
	  :type string
	  :documentation
	  "Name of port on which existing process is listening.")
   (host  :initarg :host
	  :type string
	  :documentation
	  "Name of host on which existing process is listening."))
  "Attach debugger to a running process via a socket connection.")

(defmethod initialize-instance ((this jde-dbs-attach-socket) &rest fields)
  "Constructor for attach_socket command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'port))

  ;; Set command name.
  (oset this name "attach_socket"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-attach-socket))
  "Creates the command line for the attach_socket command."
  (let ((cmd
	 (format "-1 %s %s %s -port %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this port))))              ;; process name
    (if (slot-boundp this 'host)
	(setq cmd (format "%s -host %s" cmd (oref this host))))
    cmd))

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-attach-socket))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset (oref this process) attachedp t)
    (oset (oref this process) startupp t)
    (oset this msg  (format "Attached to process on port %s of %s." 
			    (oref this port)
			    (if (slot-boundp this 'host)
				(oref this host)
			      "local host")))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-attach-socket))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach to process on port %s of %s.\n Reason: %s." 
	     (oref this port)
	     (if (slot-boundp this 'host)
		 (oref this host)
	       "local host")
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-listen-for-process (jde-dbs-cmd)
  ((address   :initarg :address
	      :type string
	      :documentation
	      "Address at which to listen for a debuggee process.")
   (transport :initarg :transport
	      :type string
	      :initform "shmem"
	      :documentation
	      "Transport mechanism used to interact with debuggee process."))
  "Listen for a process requesting debugger services.")

(defmethod initialize-instance ((this jde-dbs-listen-for-process) &rest fields)
  "Constructor for listen command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'address))

  (assert (not 
	   (and
	    (not (eq system-type 'windows-nt))
	    (string= (oref this transport) "shmem"))))

  ;; Set command name.
  (oset this name 
	(concat "listen_"
		(oref this transport))))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-listen-for-process))
  "Creates the command line for the listen command."
  (format "-1 %s %s %s %s" 
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this address)))            ;; process address

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-listen-for-process))
  (call-next-method)
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this msg  (format "Listening for process at %s address: %s." 
			    (if (string= (oref this transport) "shmem")
				"shared memory" "socket")
			    (oref this address)))
    (oset process startupp t)
    (split-window-vertically)
    (pop-to-buffer (oref process locals-buf))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-listen-for-process))
  (delete-other-windows)
  (let ((source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot listen for process at %s address: %s.\n Reason: %s." 
	     (if (string= (oref this transport) "shmem")
		 "shared memory" "socket")
	     (oref this address)
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Run Process Command Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-run-process (jde-dbs-cmd) ()
  "Run process command.")

(defmethod initialize-instance ((this jde-dbs-run-process) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "run"))


(defmethod jde-dbs-cmd-success-action ((this jde-dbs-run-process))
  (call-next-method)
  (oset this msg (format "Running %s." 
			 (oref (oref this process)  main-class))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-run-process))
  (oset this msg 
	(format "Error: unable to run %s..\n  Reason: %s."
		(oref (oref this process) main-class)
		(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Finish Process Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-finish-process (jde-dbs-cmd) ()
  "Finish process command.")

(defmethod initialize-instance ((this jde-dbs-finish-process) &rest fields)
  "Constructor for finish process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))

  ;; Set command name.
  (oset this name "finish"))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-finish-process))
  "Executes the finish process command."
  (let* ((process (oref this :process))
	 (main-class (oref process :main-class))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(progn
	  (jde-dbs-proc-display-debug-message process
	   (concat "Terminating " main-class)))
      (jde-dbs-proc-display-debug-message process
       (concat "Error: debugger unable to terminate: "
	       main-class
	       ".\n  Reason: " 
	       (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Set Breakpoint Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-set-breakpoint (jde-dbs-cmd) 
  ((breakpoint    :initarg :breakpoint
	          ;; :type jde-bug-breakpoint
	          :documentation
	          "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this jde-dbs-set-breakpoint) &rest fields)
  "Constructor for set breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "break absolute"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-set-breakpoint))
  "Creates the command line for the set breakpoint command."
  (let* (
	 (bp-spec (oref this breakpoint))
	 (file (oref bp-spec file))
	 (line (oref bp-spec line)))
    (format "%s %s %s" 
	    (call-next-method)
	    file     ;; File
	    line)))  ;; Line number    

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-set-breakpoint))
  (call-next-method)
  (let*  ((process (oref this process))
	  (bp-procid (oref this data))
	  (bp-spec (oref this breakpoint))
	  (file (oref bp-spec file))
	  (line (oref bp-spec line))
	  (bpspec (jde-dbs-proc-bpspec "spec" :id bp-procid :breakpoint bp-spec))
	  (bpspecs (if (slot-boundp process :bpspecs) (oref process :bpspecs))))
    (if bpspecs
	(oset process bpspecs (jde-dbs-proc-bpspecs-add bpspecs bpspec))
      (oset process bpspecs (jde-dbs-proc-bpspecs-add nil bpspec)))
    (oset this msg (format "Setting breakpoint at line %s in %s." line file))))


(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-set-breakpoint))
  (let* ((bp-spec (oref this breakpoint))
	 (file (oref bp-spec file))
	 (line (oref bp-spec line)))
    (oset this msg  (format "Error: cannot set breakpoint at line %s in file %s.\n  Reason:" 
			    file line (oref this data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-clear-breakpoint (jde-dbs-cmd) 
  ((breakpoint    :initarg :breakpoint
	          ;; :type jde-bug-breakpoint
	          :documentation
	          "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this jde-dbs-clear-breakpoint) &rest fields)
  "Constructor for clear breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "clear"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-clear-breakpoint))
  "Creates the command line for the clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))
	 (bpspec (jde-dbs-proc-get-bpspec process breakpoint))
	 (bp-procid (oref bpspec id)))
    (format "%s %s"              ;; PID CID clear BPID
	    (call-next-method)
	    bp-procid)))         ;; Id assigned by debugger to this breakpoint
 

(defmethod jde-dbs-cmd-exec ((this jde-dbs-clear-breakpoint))
  "Execute clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))	
	 (file (oref breakpoint file))
	 (line (oref breakpoint line))
	 (proc-id (oref process id))
	 (bpspec (jde-dbs-proc-get-bpspec process breakpoint))
	 (bp-procid (oref bpspec id))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let ((bpspecs (oref process bpspecs)))
	  (oset process bpspecs 
		(jde-dbs-proc-bpspecs-remove bpspecs bpspec))
	  (jde-dbs-proc-display-debug-message  
	   process
	   (format "Cleared breakpoint at line %s in file %s" line file)))
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot clear breakpoint at line %s in file %s.\n Reason: %s." 
	       line file (car (jde-dbo-command-result-data result))))
      nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Over/Into/Out Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-dbs-step (jde-dbs-cmd)
  ((step-type :initarg :step-type
	      :type string
	      :initform "over"
	      :documentation
	      "Type of step operation: over, into, out"))
  "Step command.")

(defmethod initialize-instance ((this jde-dbs-step) &rest fields)
  "Constructor for step command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))


  ;; Set command name.
  (oset this name (concat "step " (oref this step-type))))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-step))
  "Creates the command line for the step command."
  (format "%s %d" (call-next-method) 
	  (oref (oref (oref this process) state-info) thread-id)))  


(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-step))
  (oset this msg
	(format "Error: unable to step %s.\n Reason: %s"
		(oref this step-type) (oref this data))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Into Command Class                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod jde-dbs-proc-step-into ((this jde-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id 
	  (oref (oref this state-info) thread-id))	
	 (result (jde-dbs-do-command proc-id  (format "step into %s" thread-id))))
    (when (not (jde-dbo-command-succeeded-p result))
      (jde-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (jde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Out Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod jde-dbs-proc-step-out ((this jde-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id 
	  (oref (oref this state-info) thread-id))	
	 (result (jde-dbs-do-command proc-id  (format "step out %s" thread-id))))
    (when (not (jde-dbo-command-succeeded-p result))
      (jde-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (jde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-evaluate (jde-dbs-cmd) 
  ((expression    :initarg :expression
	          ;; :type string
	          :documentation
	          "Expression to be evaluate. Required.")
   (thread-id     :initarg :thread-id
		  ;; :type integer
		  :documentation
		  "Id of thread that scopes this expression. Required."))
  "Evaluate expression command.")

(defmethod initialize-instance ((this jde-dbs-evaluate) &rest fields)
  "Constructor for evaluate command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this expression))
  (assert (oref this thread-id))
 
  ;; Set command name.
  (oset this name "evaluate"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-evaluate))
  "Creates the command line for the clear breakpoint command."
    (format "%s %s 0 \"%s\""         ;; PID CID evaluate THREAD-ID 0 "EXPRESSION"
	    (call-next-method)       ;; PID CID evaluate
	    (oref this thread-id)    ;; thread id
	    (oref this expression))) ;; expression to be evaluated.
 

(defmethod jde-dbs-cmd-exec ((this jde-dbs-evaluate))
  "Execute evaluate expression command. Returns
(TYPE VALUE GCFLAG) where TYPE is the type of the result,
VALUE is the value, and GCFLAG is t if the result has been
garbage collected."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(car (jde-dbo-command-result-data result))
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot evaluate \"%s\".\n Reason: %s." 
	       (oref this expression)
	       (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Array                                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-array (jde-dbs-cmd) 
  ((array    :initarg :array
	     :type jde-dbs-java-array
	     :documentation
	     "Object to represent the array. Required.")
   (index    :initarg :index
	     :type integer
	     :documentation
	     "Index of array slice to be returned.")
   (length   :initarg :length
	     :type integer
	     :documentation "Length of slice to be returned."))
  "Get a slice of the array object specified by ARRAY. INDEX and LENGTH are
the index and length of the slice to be returned. If omitted, this command returns
the length of the first slice of the array. Note that each element of this array
can be another array or some other object.")


(defmethod initialize-instance ((this jde-dbs-get-array) &rest fields)
  "Constructor for get array command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :array))

  (if (slot-boundp this :index)
      (assert (slot-boundp this :length)))

  ;; Set command name.
  (oset this name "get_array"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-get-array))
  "Creates the command line for the get-object command."
  (let ((cl
	 (format "%s %d" (call-next-method) (oref (oref this array) id)))
	(index (if (slot-boundp this :index) (oref this :index))))
    (if index
	(setq cl 
	      (format "%s %d %d"                ;; PID CID get_array OBJ-ID INDEX LENGTH
		      cl
		      index                     ;; index of slice to be returned.
		      (oref this length))))    ;; length of slice to be returned.
    cl))
 

(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-array))
  "Executes the get-array command. If a slice is specified, 
returns the slice as a list of elements. Otherwise, return
the length of the array."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let* ((array (oref this array))
	       (data   (nth 0 (jde-dbo-command-result-data result)))
	       (type (nth 0 data))
	       (id (nth 1 data))
	       (gc-flag (nth 2 data))
	       (length (nth 3 data))
	       (elements (if (> (length data) 4)
			     (cdr (cdr (cdr (cdr data)))))))
	  (or elements length)
	  (oset array jtype type)
	  (oset array id id)
	  (oset array gc-flag gc-flag)
	  (oset array length length)
	  (oset array elements 
		(mapcar 
		 (lambda (element)
		   (jde-dbs-objectify-value element))
		 elements))
	  array)      
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get array %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Abstract Get Object                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-abstract-get-object (jde-dbs-cmd) 
  ((object-id     :initarg :object-id
	          :type integer
	          :documentation
	          "Id of object. Required."))
  "Parent class of get object commands.")


(defmethod initialize-instance ((this jde-dbs-abstract-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :object-id)))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-abstract-get-object))
  "Creates the command line for the get-object command."

  (format "%s %d" (call-next-method) (oref this object-id)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-object (jde-dbs-abstract-get-object) ()
  "Class of generic get-object commands. These commands return the fields of
the object.")


(defmethod initialize-instance ((this jde-dbs-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_object"))

(defun jde-dbs-objectify-value (value-form)
  (let ((lvf        (length value-form))
	(value-type (car value-form)))
    (cond
     ((and (= lvf 1) (string= value-type "null"))
      (jde-dbs-java-null "null"))
     ((= lvf 2)
      (jde-dbs-java-primitive
       "primitive" 
       :jtype  value-type
       :value  (nth 1 value-form)))
     ((= lvf 3)
      (if (string-match "\\[\\]" value-type)
	  (jde-dbs-java-array
	   (format "array %d" (nth 1 value-form))
	   :jtype value-type
	   :id (nth 1 value-form)
	   :gc-flag (nth 2 value-form))
	(jde-dbs-java-udci
	 (format "obj %d" (nth 1 value-form))
	 :jtype    value-type
	 :id       (nth 1 value-form)
	 :gc-flag  (nth 2 value-form)))))))

(defun jde-dbs-objectify-variable (variable-form)
  (let* ((var-name   (car (car variable-form)))
	 (var-type   (cdr (car variable-form)))
	 (value-form (cdr variable-form))
	 (value      (jde-dbs-objectify-value 
		      value-form)))				
    (jde-dbs-java-variable
     (format "variable %s" var-name)
     :name var-name
     :jtype (mapconcat (lambda (x) x) (nreverse var-type) " ")
     :value value)))
 
(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-object))
  "Executes the get-object command. Returns a Lisp object of type
`jde-dbs-java-class-instance' that represents the Java object."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let* ((obj     (car (jde-dbo-command-result-data result)))
	       (type    (nth 0 obj))
	       (id      (nth 1 obj))
	       (gc-flag (nth 2 obj))
	       (fields  (if (> (length obj) 3)
			    (nth 3 obj)))
	       (object  (jde-dbs-java-udci
			 (format "obj %d" id)
			 :jtype type
			 :id id
			 :gc-flag gc-flag)))
	  (if fields
	      (mapc
	       (lambda (variable-form)
		 (let ((field
			(jde-dbs-objectify-variable variable-form)))
		   (jde-dbs-java-udci-add-field object field)))
	       fields))
	  object)	    
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get object %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (jde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get String                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-string (jde-dbs-abstract-get-object) ()
  "Get the value of a string object.")


(defmethod initialize-instance ((this jde-dbs-get-string) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_string"))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-string))
  "Executes the get_string command. Returns the string."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(nth 3 (car (jde-dbo-command-result-data result)))	    
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get string %d.\n Reason: %s." 
	       (oref this object-id)
	       (car (jde-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Locals                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-locals (jde-dbs-cmd) 
  ((thread-id         :initarg :thread-id
	              :type integer
	              :documentation
	              "ID of thread whose local variables are being queried.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame containing requested local variables."))
  "Get variables local to a specified thread and stack frame.")


(defmethod initialize-instance ((this jde-dbs-get-locals) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_locals"))


(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-get-locals))
  "Creates the command line for the get-locals command."
  (format "%s %d %d" 
	  (call-next-method) 
	  (oref this thread-id)
	  (oref this stack-frame-index)))
 

(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-locals))
  "Executes the get-locals command. Returns a list of Lisp objects of type
`jde-dbs-java-variable' that represents the local variables."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let* ((variable-forms (car (jde-dbo-command-result-data result)))
	       (variables      (if variable-forms
				   (mapcar
				    (lambda (variable-form)
			                (jde-dbs-objectify-variable variable-form))
				    variable-forms))))
	  variables)	    
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get local variables.\n Reason: %s." 
	       (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get This                                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-this (jde-dbs-cmd)
  ((thread-id         :initarg :thread-id
	              :type integer
	              :documentation
	              "ID of thread of stack frame whose this object is required.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame whose this object is required."))
  "Get this object of a specified stack frame.")


(defmethod initialize-instance ((this jde-dbs-get-this) &rest fields)
  "Constructor for get_this command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))
  (assert (slot-boundp this 'stack-frame-index))

  ;; Set command name.
  (oset this name "get_this"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-get-this))
  "Creates the command line for the get_this command."
  (format "%s %d %d" 
	  (call-next-method) 
	  (oref this thread-id)
	  (oref this stack-frame-index)))
 
(defmethod jde-dbs-cmd-success-action ((this jde-dbs-get-this))
  (call-next-method)
  (let ((this-obj (oref this :data)))
    (oset 
     this 
     :result 
     (if (string= (nth 0 this-obj) "null")
	 (jde-dbs-java-null "null")
       (jde-dbs-java-udci
	  "this object"
	  :jtype (nth 0 this-obj)
	  :id (nth 1 this-obj))))))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-get-this))
 (oset 
  this 
  msg 
  (format 
   "Error: unable to get this object for stack frame %s on thread %d.\n Reason: %s." 
   (oref this stack-frame-index)
   (oref this thread-id)
   (oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Loaded Classes Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-loaded-classes (jde-dbs-cmd) ()
  "Gets the classes loaded by a specified process.")

(defmethod initialize-instance ((this jde-dbs-get-loaded-classes) &rest fields)
  "Constructor for get_loaded_classes command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_loaded_classes"))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-loaded-classes))
  "Executes the get_loaded_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let ((classes (car (jde-dbo-command-result-data result))))
	  (jde-dbs-proc-display-debug-message 
	   process
	   (format "Loaded classes:\n  %s." 
		   (mapconcat (lambda (x) x) classes "\n  ")))
	  t)
      (jde-dbs-proc-display-debug-message process
             (format "Error: unable to list loaded classes.\n  Reason: %s."
		     (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Path Info Command Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-path-info (jde-dbs-cmd) ()
  "Gets the base directory, boot classpath, and classpath of the specified process.")

(defmethod initialize-instance ((this jde-dbs-get-path-info) &rest fields)
  "Constructor for get_path_information command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_path_information"))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-path-info))
  "Executes the get_path_info command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let* ((data (jde-dbo-command-result-data result))
	       (base-dir (nth 0 data))
	       (boot-classpath (nth 1 data))
	       (classpath (nth 2 data)))
	  (jde-dbs-proc-display-debug-message 
	   process
	   (format (concat
		    "\nPath information\n\n  Base directory:\n    %s\n\n  "
		    "Boot classpath:\n    %s\n\n  Application Classpath:\n    %s\n")
		   base-dir
		   (mapconcat (lambda (x) x) boot-classpath "\n    ")
		   (mapconcat (lambda (x) x) classpath "\n    ")))
	  t)
      (jde-dbs-proc-display-debug-message process
             (format "Error: unable to display path information.\n  Reason: %s."
		     (car (jde-dbo-command-result-data result))))
      nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Threads                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-threads (jde-dbs-cmd) ()
  "Get all the threads for this process.")


(defmethod initialize-instance ((this jde-dbs-get-threads) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_threads"))

(defun jde-dbs-map-thread-to-tree (thread)
  (list (quote jde-widget-tree) :tag (concat (nth 2 thread) " thread")
	:value nil
	(list (quote jde-widget-tree) :tag (concat "id: " (number-to-string (nth 1 thread))))
	(list (quote jde-widget-tree) :tag (concat "status: " (nth 3 thread)))
	(list (quote jde-widget-tree) :tag (concat "state: " (nth 4 thread)))
	(jde-dbs-map-stack-to-tree (nth 5 thread))))


(defun jde-dbs-map-threadgroup-to-tree (threadgroup)
  (nconc
   (list (quote jde-widget-tree) :tag (concat (nth 2 threadgroup) " thread group")
	:value nil)
   (mapcar
    (lambda (x)
      (jde-dbs-map-thread-to-tree x))
    (nth 3 threadgroup))
   (mapcar
    (lambda (x)
      (jde-dbs-map-threadgroup-to-tree x))
    (nth 4 threadgroup))))

(defun jde-dbs-map-stack-to-tree (stack)
  (nconc
   (list (quote jde-widget-tree) :tag "Stack")
   (if (listp stack)
       (mapcar
	(lambda (x)
	  (list (quote jde-widget-tree) :tag
		(format "%s.%s(%s:%s)" (nth 1 x) (nth 4 x) (nth 2 x)
			(nth 3 x))))
	stack))))

(defun jde-dbs-map-threads-to-tree (threads)
  (nconc
   (list (quote jde-widget-tree) :tag "Threads")
	(mapcar
	 (lambda (x)
	   (if (string= (nth 0 x) "Thread")
	       (jde-dbs-map-thread-to-tree x)
	     (if (string= (nth 0 x) "ThreadGroup")
		 (jde-dbs-map-threadgroup-to-tree x))))
	 threads)))


(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-threads))
  "Executes the get-threads command. Returns a list of thread information."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jde-dbo-command-succeeded-p result)
	(let* ((thread-list (car (jde-dbo-command-result-data result)))
	       (buf (oref process threads-buf)))
	  (set-window-configuration (oref process win-cfg))
	  (set-window-buffer 
	   (next-window (frame-first-window)) buf)
	  (set-buffer buf)
	  (let ((inhibit-read-only t))
	    (erase-buffer)) 
	  (if (not jde-xemacsp)
	      (let ((all (overlay-lists)))  
		(mapcar 'delete-overlay (car all))    
		(mapcar 'delete-overlay (cdr all))))
	  (apply 'widget-create (jde-dbs-map-threads-to-tree thread-list))
	  (use-local-map widget-keymap)
	  (widget-setup))	    
      (jde-dbs-proc-display-debug-message 
       process
       (format "Error: cannot get local variables.\n Reason: %s." 
	       (car (jde-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-thread (jde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be queried."))
  "Gets information about a thread, including the method call stack.")


(defmethod initialize-instance ((this jde-dbs-get-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_thread"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-get-thread))
  "Creates the command line for the get_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-get-thread))
  (call-next-method)
  (oset this :result (oref this :data)))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-get-thread))
 (oset this msg (format "Error: unable to get info for thread %d.\n Reason: %s." 
			(oref this thread-id)
			(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object Monitors                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-get-object-monitors (jde-dbs-cmd) 
  ((object-id     :initarg :object-id
	          :type integer
	          :documentation
	          "Id of object. Required."))
  "Get threads that are monitoring the specified object.")


(defmethod initialize-instance ((this jde-dbs-get-object-monitors) &rest fields)
  "Constructor for get_object_monitors command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :object-id))

  ;; Set command name.
  (oset this name "get_object_monitors"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-get-object-monitors))
  "Creates the command line for the get_object_monitors command."

  (format "%s %d" (call-next-method) (oref this object-id)))
 
(defmethod jde-dbs-cmd-exec ((this jde-dbs-get-object-monitors))
  "Executes the get_object_monitors command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 msg)
    (if (jde-dbo-command-succeeded-p result)
	(let* ((data (car (jde-dbo-command-result-data result)))
	       (obj-id (nth 0 data))
	       (obj-type (nth 1 data))
	       (obj-gc (nth 2 data))
	       (owner (nth 3 data))
	       (waiting (nth 4 data)))

	  (setq msg (format "\nThe following threads are monitoring <%s:%s>:\n"
			    obj-type obj-id))

	  (setq 
	   msg 
	   (concat 
	    msg   
	    "  Current owner:"
	    (if (listp owner)
		(concat
		 "\n"
		 "    Name:   " (nth 1 owner) "\n" 
		 "    Id:     " (nth 2 owner) "\n"
		 "    Status: " (nth 3 owner) "\n"
		 "    State:  " (nth 4 owner) "\n")
	      (if (stringp owner)
		  (concat " " owner)))))

	  (if waiting
	      (setq 
	       msg 
	       (concat 
		msg 
		"\n  Waiting threads:"
		(if (listp waiting)
		    (progn
		      "\n"
		      (mapconcat
		      (lambda (thread)
		        (concat 			
			 "    Name:   " (nth 1 thread) "\n" 
			 "    Id:     " (nth 2 thread) "\n"
			 "    Status: " (nth 3 thread) "\n"
			 "    State:  " (nth 4 thread) "\n"))
		      waiting "\n"))
		  (if (stringp waiting) (concat " " waiting "\n")))))))		
      (setq msg
	    (format "Error: cannot get object monitors for  %d.\n Reason: %s." 
		    (oref this object-id)
		    (car (jde-dbo-command-result-data result)))))
    (jde-dbs-proc-display-debug-message process msg)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-suspend-thread (jde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread or thread-group to be suspended. If omitted, all threads are suspended."))
  "Suspend a thread of this process.")


(defmethod initialize-instance ((this jde-dbs-suspend-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "suspend"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-suspend-thread))
  "Creates the command line for the suspend_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-suspend-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d suspended." (oref this thread-id)))
    (oset this msg "All threads suspended.")
    (oset (oref this process) suspendedp t)))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-suspend-thread))
 (oset this msg (format "Error: unable to suspend thread.\n Reason: %s." 
	       (oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Resume Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-resume-thread (jde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread or thread-group to be resumed. If omitted, all threads are resumed."))
  "Resume a thread of this process.")


(defmethod initialize-instance ((this jde-dbs-resume-thread) &rest fields)
  "Constructor for resume-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "resume"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-resume-thread))
  "Creates the command line for the resume_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod jde-dbs-cmd-success-action ((this jde-dbs-resume-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d resumed." (oref this thread-id)))
    (oset this msg "All threads resumed.")
    (oset (oref this process) suspendedp nil)))

(defmethod jde-dbs-cmd-failure-action ((this jde-dbs-resume-thread))
  (oset this msg 
	(format "Error: unable to resume thread.\n Reason: %s." 
		(oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Stop Thread                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-stop-thread (jde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be stopped.")
   (exception-id  :initarg :exception-id
	          :type integer
	          :documentation
	          "Id of thread to be stopped."))
  "Stops the specified thread in the target process and throw the specified
exception. You can use the evaluate expression command to create the exception
object.")


(defmethod initialize-instance ((this jde-dbs-stop-thread) &rest fields)
  "Constructor for stop-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))
 (assert (slot-boundp this 'exception-id))

  ;; Set command name.
  (oset this name "stop"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-stop-thread))
  "Creates the command line for the resume_thread command."
  
  (format "%s %d %d" (call-next-method) (oref this thread-id) 
	  (oref this exception-id)))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-stop-thread))
  "Executes the stop_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result)))
    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d stopped." (oref this thread-id))
	   (format "Error: unable to stop thread %d.\n Reason: %s." 
		   (oref this thread-id)
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-interrupt-thread (jde-dbs-cmd)
  ((thread-id     :initarg :thread-id
	          :type integer
	          :documentation
	          "Id of thread to be interrupted."))
  "Interrupt a thread of this process. An interrupted thread cannot be resumed.")


(defmethod initialize-instance ((this jde-dbs-interrupt-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "interrupt"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-interrupt-thread))
  "Creates the command line for the interrupt_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-interrupt-thread))
  "Executes the interrupt_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result)))
    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d interrupted." (oref this thread-id))
	   (format "Error: unable to interrupt thread %d.\n Reason: %s." 
		   (oref this thread-id)
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-methods (jde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jde-dbs-trace-methods-request
		   :documentation 
		   "Trace method request."))
  "Trace method entries or exits.")


(defmethod initialize-instance ((this jde-dbs-trace-methods) &rest fields)
  "Constructor for trace_methods command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "entry")
	  (string= (oref (oref this trace-request) trace-type) "exit")))

  ;; Set command name.
  (oset this name "trace_methods"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-trace-methods))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'thread-restriction)
	(setq cmd (format "%s -tname %s" cmd (oref request thread-restriction))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-trace-methods))
  "Executes the trace_methods command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace method %s enabled. Use request id %s to cancel." 
                     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-classes (jde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jde-dbs-trace-classes-request
		   :documentation 
		   "Trace classes request."))
  "Trace class preparations or unloadings.")


(defmethod initialize-instance ((this jde-dbs-trace-classes) &rest fields)
  "Constructor for trace_classes command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "preparation")
	  (string= (oref (oref this trace-request) trace-type) "unloading")))

  ;; Set command name.
  (oset this name "trace_classes"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-trace-classes))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-trace-classes))
  "Executes the trace_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace class %s enabled. Use request id %s to cancel." 
                     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-trace-exceptions (jde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jde-dbs-trace-exceptions-request
		   :documentation 
		   "Trace exceptions request."))
  "Trace exceptions.")


(defmethod initialize-instance ((this jde-dbs-trace-exceptions) &rest fields)
  "Constructor for trace_exceptions command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "both")
	  (string= (oref (oref this trace-request) trace-type) "caught")
	  (string= (oref (oref this trace-request) trace-type) "uncaught")))

  ;; Set command name.
  (oset this name "trace_exceptions"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-trace-exceptions))
  "Creates the command line for the trace_exceptions command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s %s" 
		      (call-next-method) 
		      (oref request exception-class)
		      (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-trace-exceptions))
  "Executes the trace_exceptions command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset 
	   process 
	   trace-req 
	   (nconc (oref process trace-req) 
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace exception %s enabled. Use request id %s to cancel." 
                     (oref request exception-class) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s." 
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-cancel-trace (jde-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jde-dbs-trace-request
		   :documentation 
		   "Trace request."))
  "Cancel a trace request.")


(defmethod initialize-instance ((this jde-dbs-cancel-trace) &rest fields)
  "Constructor for cancel_trace command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'trace-request))

  ;; Set command name.
 (oset this name (oref (oref this trace-request) cancel-command)))


(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-cancel-trace))
  "Creates the command line for the cancel_trace command."
  (format "%s %s" (call-next-method) (oref (oref this trace-request) id)))


(defmethod jde-dbs-cmd-exec ((this jde-dbs-cancel-trace))
  "Executes the cancel_trace command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this trace-request) id))
	       (requests 
		(remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process trace-req))))
	  (if requests
	      (oset process trace-req requests)
	    (slot-makeunbound process 'trace-req))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled trace request %s." 
                     (oref (oref this trace-request) id))
	   (format "Error: unable to cancel trace %s.\n Reason: %s." 
		   (oref (oref this trace-request) id)
		   (car (jde-dbo-command-result-data result)))))

    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-watch-field (jde-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type jde-dbs-watch-field-request
		   :documentation 
		   "Watch field request."))
  "Watch a field of an object or a specified class of objects.")


(defmethod initialize-instance ((this jde-dbs-watch-field) &rest fields)
  "Constructor for watch field command."

  ;; Call parent initializer.
  (call-next-method)

  (let ((request (oref this watch-request)))

    (assert (or
	     (string= (oref request watch-type) "access")
	     (string= (oref request watch-type) "modification")))

    (assert (slot-boundp request 'object-class))
    (assert (slot-boundp request 'field-name)))
 
  ;; Set command name.
  (oset this name "watch"))

(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-watch-field))
  "Creates the command line for the watch-field command."
  (let* ((request (oref this watch-request))
	 (cmd (format 
	       "%s %s %s %s" 
	       (call-next-method) 
	       (oref request object-class)
	       (oref request field-name)
	       (concat "for_" (oref request watch-type)))))

    (if (slot-boundp request 'object-id)
	(setq cmd (format "%s -oid %s" cmd (oref request object-id))))

    (if (slot-boundp request 'expression)
	(setq cmd (format "%s -if %s" cmd (oref request expression))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cf \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd 
	      (format 
	       "%s -cef \"%s\"" 
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jde-dbs-cmd-exec ((this jde-dbs-watch-field))
  "Executes the watch-field command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result))
	 (request (oref this watch-request))
	 (request-id (car (jde-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'watch-req)
	  (oset 
	   process 
	   watch-req 
	   (nconc (oref process watch-req) 
		  (list (cons request-id request))))
	(oset process watch-req (list (cons request-id request)))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Watch request field for field %s of %s instance of class %s is enabled. Use request id %s to cancel." 
                     (oref request field-name) 
		     (if (slot-boundp request 'object-id)
			 (oref request object-id)
		       "any")
		     (oref request object-class)
		     request-id)
	   (format "Error: unable to enable watch request.\n Reason: %s." 
		   (car (jde-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-dbs-cancel-watch (jde-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type jde-dbs-watch-field-request
		   :documentation 
		   "Watch request."))
  "Cancel a watch request.")


(defmethod initialize-instance ((this jde-dbs-cancel-watch) &rest fields)
  "Constructor for cancel_watch command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'watch-request))

  ;; Set command name.
 (oset this name "clear"))


(defmethod jde-dbs-cmd-make-command-line ((this jde-dbs-cancel-watch))
  "Creates the command line for the clear command."
  (format "%s %s" (call-next-method) (oref (oref this watch-request) id)))


(defmethod jde-dbs-cmd-exec ((this jde-dbs-cancel-watch))
  "Executes the cancel watch command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jde-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this watch-request) id))
	       (requests 
		(remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process watch-req))))
	  (if requests
	      (oset process watch-req requests)
	    (slot-makeunbound process 'watch-req))))

    (jde-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled watch request %s." 
                     (oref (oref this watch-request) id))
	   (format "Error: unable to cancel watch request %s.\n Reason: %s." 
		   (oref (oref this watch-request) id)
		   (car (jde-dbo-command-result-data result)))))

    command-succeeded-p))


(provide 'jde-dbs)

; $Log: jde-dbs.el,v $
; Revision 1.56  2000/07/28 06:27:45  paulk
; Committing all modified files.
;
; Revision 1.55  2000/06/12 08:35:38  paulk
; Now uses the value of jde-bug-debugger-host-address as the address of the socket for the CLI channel.
;
; Revision 1.54  2000/04/23 06:19:29  paulk
; Fixed some problems with the process launch command. Among others,
; the command now uses `system-name', instead of an absolute
; address (127.0.0.1) to refer to the local host when connecting to
; the port used to transport the debuggee process's standard I/O.
; This fixes the failure to start processes that occurs on some
; Windows networks.
;
; Revision 1.53  2000/04/18 01:20:52  paulk
; Fixes a bug in the jde-dbs-cmd-failure-action method for jde-dbs-get-this and a bug in the jde-dbs-cmd-success-action for jde-dbs-listen-for process.
;
; Revision 1.52  2000/04/13 09:20:54  paulk
; Removed one stray reference to deleted function jde-proc-steppable-p.
;
; Revision 1.51  2000/04/13 09:00:09  paulk
; Added steppablep field to process object. Set whenever process hits a breakpoint or step event.
; Modified jde-dbs-target-process-steppable-p to test steppablep field.
; This fixes menu enabling bug.
; Debugger lisp output parser now suspends paren balancing in strings. This fixes Lisp eval error when a Java variable includes unbalanced parentheses.
;
; Revision 1.50  2000/04/10 05:22:55  paulk
; Added command to get the this object for a specified stack frame.
;
; Revision 1.49  2000/04/05 05:00:02  paulk
; Fixed thread-tree code to ignore No information Available threads.
;
; Revision 1.48  2000/03/27 07:31:54  paulk
; Now sets the working directory to jde-run-working-directory (if not null) before starting the debugger.
;
; Revision 1.47  2000/03/17 04:19:02  paulk
; Display threads now includes the stack for each thread. Thanks to Paul Michael Reilly <pmr@pajato.com> for implementing this.
;
; Revision 1.46  2000/03/16 05:05:06  paulk
; Enabled interactive reading of vm and application arguments for JDEbug sessions. Thanks to Steve Haflich <smh@franz.com> for this enhancement.
;
; Revision 1.45  2000/02/17 06:36:31  paulk
; Fixed scrolling in process debug message window. Thanks to "Martin
; Dickau" <mdickau@byallaccounts.com> for this fix.
;
; Revision 1.44  2000/02/17 06:23:44  paulk
; jde-dbs-cmd now copies and then empties the pending event queue before
; processing the events. This fixes an infinite recursion bug that can
; occur when stepping through code.
;
; Revision 1.43  2000/02/16 04:41:41  paulk
; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
; <cfhart@Z-TEL.com>.
;
; Revision 1.42  2000/02/14 06:19:37  paulk
; Implemented up and down stack commands.
;
; Revision 1.41  2000/02/10 02:53:38  paulk
; Fixed bug where Display->Threads command was not enabled when debugger
; was attached to a process.
;
; Revision 1.40  2000/02/01 05:59:54  paulk
; Added commands for listening for applications needing debug services.
;
; Revision 1.39  2000/02/01 04:11:55  paulk
; ReleaseNotes.txt
;
; Revision 1.38  2000/01/17 09:36:39  paulk
; Implemented array and object inspectors.
;
; Revision 1.37  2000/01/15 08:04:08  paulk
; Added show buffer commands.
;
; Revision 1.36  2000/01/02 08:07:55  paulk
; Added attach process commands.
;
; Revision 1.35  1999/12/27 08:01:17  paulk
; Added show object monitors command.
;
; Revision 1.34  1999/12/20 07:52:06  paulk
; Added cancel watchpoint command.
;
; Revision 1.33  1999/12/19 06:54:21  paulk
; Added watch field command.
;
; Revision 1.32  1999/12/14 04:46:02  paulk
; Added JDEbug->Processes->Remove Dead Processes command.
;
; Revision 1.31  1999/12/13 05:54:08  paulk
; Added jde-bug-vm-executable and jde-bug-jre-home variables.
; Fixed jde-dbs-launch-process command so that it fails gracefully.
;
; Revision 1.30  1999/12/03 08:22:00  paulk
; Updated JDEbug to run under JDK 1.3beta.
;
; Revision 1.29  1999/11/30 05:46:22  paulk
; Added JDEbug->Display->Path Info command.
;
; Revision 1.28  1999/11/29 06:58:41  paulk
; Added JDEbug->Display->Loaded Classes Command.
;
; Revision 1.27  1999/11/27 05:13:49  paulk
; Added commands for tracing classes.
;
; Revision 1.26  1999/11/23 06:37:04  paulk
; Added Trace->Cancel command.
;
; Revision 1.25  1999/11/16 05:58:17  paulk
; Added trace method commands and skeletons for trace class and cancel
; trace commands.
;
; Revision 1.24  1999/11/04 05:52:42  paulk
; Added trace-mode fields to jde-dbs-proc class. Needed to support trace mode.
; Added object-refs field to jde-dbs-proc class. Needed to support object reference management.
;
; Revision 1.23  1999/10/28 04:18:09  paulk
; Added interrupt and stop thread commands.
;
; Revision 1.22  1999/10/14 04:59:23  paulk
; Added Resume Process and Resume Thread commands.
;
; Revision 1.21  1999/10/13 08:16:43  paulk
; Added suspend process and suspend thread commands.
;
; Revision 1.20  1999/10/13 06:19:00  paulk
; Add JDEBug->Show Threads command
;
; Revision 1.19  1999/09/28 04:12:50  paulk
; start debugger method now checks whether debugger actually started
; and returns nil if the debugger did not start.
;
; Revision 1.18  1999/09/18 03:55:58  paulk
; Fixed bug in the launch-process command where the command was failing
; to convert the application arguments from a list of arguments to a
; string of arguments. Thanks to "Matthew
; Weymar"<mweymar@hamilton-partners.com> for reporting the bug.
;
; Revision 1.17  1999/09/16 05:36:59  paulk
; Added get locals command.
;
; Revision 1.16  1999/09/13 05:37:33  paulk
; Enhanced get array command.
;
; Revision 1.15  1999/09/10 06:41:50  paulk
; Finished first cut at get_object command.
;
; Revision 1.14  1999/09/08 05:40:46  paulk
; Updated debugger code to take advantage of new unbound slot capability
; of eieio.
;
; Revision 1.13  1999/09/07 05:12:36  paulk
; Added get array command.
;
; Revision 1.12  1999/09/05 04:35:34  paulk
; Added initial implementation of evaluate and display variable commands.
;
; Revision 1.11  1999/08/30 07:10:41  paulk
; Converted clear breakpoint command to OOPS.
;
; Revision 1.10  1999/08/28 05:34:20  paulk
; Improved multiple process handling, window configuration.
;
; Revision 1.9  1999/08/27 05:27:53  paulk
; Provided initial support for multiple processes.
; Fixed jde-find-data-directory to work on XEmacs with a standard
; JDE distribution.
; Ported breakpoint highlighting code to XEmacs. Still has bugs though.
; Now includes jde-db-option options on vm command-line for process.
;
; Revision 1.8  1999/08/24 06:29:43  paulk
; Reimplemented the constructor for jde-dbs-proc the right way. Renamed
; jde-bug-counter to jde-bug-breakpoint-counter.
;
; Revision 1.7  1999/08/24 03:26:39  paulk
; Fixed a couple of NT-related problems. In particular, add an extra
; line feed after debugger commands to force flushing of debugger output
; buffer and modified jde-dbs-process-runnable-p to recognize an
; "unknown" state as runnable if the process is suspended.
;

;; End of jde-dbs.el