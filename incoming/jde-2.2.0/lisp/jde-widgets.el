;;; jde-widgets.el -- Custom-style widgets used by the JDE
;; $Revision: 1.6 $ $Date: 2000/02/01 04:11:57 $ 

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
;; <URL:http://sunsite.auc.dk/jde/>
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'wid-edit)
(require 'eieio)


;; ----------------------------------------------------------------------
;; The Tree Widget Code:


;;; The `tree' Widget.

(define-widget 'jde-widget-tree-open-button 'item
  "Open node in `jde-tree' widget."
  :button-prefix "["
  :button-suffix "]"
  :tag "+"
  :action 'jde-widget-tree-open-button-callback
  :help-echo "Show subtree."
  :format "%[%t%]")

(defun jde-widget-tree-open-button-callback (widget &optional event)
  ;; Set parent state to open.
  (widget-value-set (widget-get widget :parent) t))

(define-widget 'jde-widget-tree-close-button 'item
  "Close node in `tree' widget."
  :button-prefix "["
  :button-suffix "]"
  :tag "-"
  :action 'jde-widget-tree-close-button-callback
  :help-echo "Hide subtree."
  :format "%[%t%]")

(defun jde-widget-tree-close-button-callback (widget &optional event)
  ;; Set parent state to closed.
  (let* ((parent (widget-get widget :parent))
	(entries (widget-get parent :args))
	(children (widget-get parent :children)))
    (while (and entries children)
      (widget-put (car entries) :value (widget-value (car children)))
      (setq entries (cdr entries)
	    children (cdr children)))
    (widget-value-set parent nil)))

(define-widget 'jde-widget-tree 'default
  "A tree structure widget."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :prefix ""
  :prefix-extra "   "
  :prefix-empty " |--- "
  :value-get 'widget-value-value-get
  :value-create 'jde-widget-tree-value-create-callback
  :value-delete 'widget-children-value-delete)

(defun jde-widget-tree-value-create-callback (widget)
  ;; Insert all values
  (let ((open (widget-value widget))
	(tag (widget-get widget :tag))
	(entries (widget-get widget :args))
	children buttons)
    (cond ((null entries)
	   ;; Empty node.
	   (insert (widget-get widget :prefix-empty) tag "\n"))
	  (open
	   ;; Open node.
	   (push (widget-create-child-and-convert widget 'jde-widget-tree-close-button)
		buttons)
	   (insert "-\\ " tag "\n")
	   (let ((prefix (concat (widget-get widget :prefix)
				(widget-get widget :prefix-extra)))
		entry)
	     (while entries 
	       (setq entry (car entries)
		     entries (cdr entries))
	       (insert prefix)
	       (push (if entries
			(widget-create-child-and-convert widget entry 
							  :prefix prefix
							  :prefix-extra " | ")
		       ;; Last entry uses a different prefix.
		       (widget-create-child-and-convert 
			widget entry 
			:prefix prefix
			:prefix-empty " `--- "))
		     children))))
	  (t
	   ;; Closed node.
	   (push (widget-create-child-and-convert widget 'jde-widget-tree-open-button)
		buttons)
	   (insert "-- " tag "\n")))
    (widget-put widget :children children)
    (widget-put widget :buttons buttons)))

;;----------------------------------------------------------------------
;; Eval this to create a small tree.

(defun test-tree ()
  (interactive)
  (switch-to-buffer "*Tree Example*")  
  (kill-all-local-variables)
  ;; (make-local-variable 'widget-example-repeat)  
  (let ((inhibit-read-only t))
    (erase-buffer))  
  (let ((all (overlay-lists)))  
    (mapcar 'delete-overlay (car all))    (mapcar 'delete-overlay (cdr all))) 

  (widget-insert "Test tree widget. \n\n")

;   (setq tree (widget-create 'tree
; 			:tag "Foo"
; 			'(tree :tag "First")
; 			'(tree :tag "Second"
; 			       :value nil
; 			       (tree :tag "Nested"))
; 			'(tree :tag "Third")))

  (setq tree (widget-create 'jde-widget-tree
		  :tag "<test.Foo:139>"
		  '(jde-widget-tree :tag "n  int  0")
		  ;; '(jde-widget-tree :tag '(jde-widget-tree :tag "n  int  0"))
		  '(jde-widget-tree :tag "a  double 5.5")
		  '(jde-widget-tree :tag "s  S      <test.S:145>"
			 (jde-widget-tree :tag "b   boolean  true"))))
	 
;   (let*  ((threads 
; 	  (list 
; 	   (list "ThreadGroup" 189 "system"
; 		 (list
; 		  (list "Thread" 190 "Signal dispatcher" "runnable" "suspended by debugger")
; 		  (list "Thread" 191 "Reference Handler" "waiting" "suspended by debugger")
; 		  (list "Thread" 192 "Finalizer" "waiting" "suspended by debugger")))
; 	   (list "ThreadGroup" 193 "main" 
; 		 (list
; 		  (list "Thread" 1 "main" "runnable" "suspended at breakpoint")) 
; 		 nil)))
; 	 (tree (jde-dbs-map-threads-to-tree threads)))
		 
			  
;  (apply 'widget-create tree))
    
  (use-local-map widget-keymap)
  (widget-setup))


(defclass jde-dialog ()
  ((title     :initarg :title
	      :type string
	      :initform "JDE Dialog"
	      :documentation
	      "Title of dialog")
   (buf       :initarg :buf
	      :type buffer
	      :documentation
	      "Dialog buffer")
   )
  "Super class of JDE dialogs."
  )

(defmethod initialize-instance ((this jde-dialog) &rest fields)
  "Constructor for trace methods dialog."

  ;; Call parent initializer.
  (call-next-method)

  (oset this buf (get-buffer-create (oref this title)))
  (set-buffer (oref this buf))

  (jde-dialog-create this)

  (widget-put
   (widget-create 
    'push-button
    :notify 
    (lambda (button &rest ignore) (jde-dialog-ok (widget-get button :dialog)))
    "Ok")
   :dialog this)

  (widget-put
   (widget-create 
    'push-button
    :notify (lambda (button &rest ignore) (jde-dialog-cancel (widget-get button :dialog)))
    "Cancel")
   :dialog this)

  (use-local-map widget-keymap)
  (widget-setup)
  )

(defmethod jde-dialog-create ((this jde-dialog)))

(defmethod jde-dialog-ok ((this jde-dialog))
  "Invoked when the user clicks the dialog's okay button. The
default method kills the dialog buffer."
  (kill-buffer (current-buffer)))

(defmethod jde-dialog-cancel ((this jde-dialog))
  "Invoked when the user clicks the dialog's Cancel button. The
default method kills the dialog buffer."
  (kill-buffer (current-buffer)))

(defmethod jde-dialog-show ((this jde-dialog))
   (pop-to-buffer (oref this buf)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Dynamic tree widget                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-widget 'jde-widget-dtree 'default
  "A widget whose nodes are generated on demand.
The first time the user expands the tree, the tree invokes a function that 
generates the nodes. The tree then caches the nodes. 
Thereafter, the node uses the cached nodes when the
user closes and then reopens the tree. Use the syntax 
(widget-create 'jde-widget-dtree :tag NAME :node-fcn NODE-FUNCTION) 
to create the widget where NAME is the tree name and NODE-FUNCTION
is a function that takes one argument, the tree itself, and
returns a list of widgets that are the nodes of the expanded
tree."
  :format "%v"
  :prefix-extra "    "
  :value-get 'widget-value-value-get
  :value-create 'jde-widget-dtree-create-callback
  :value-delete 'widget-children-value-delete
  :has-nodes t)

(defun jde-widget-dtree-create-callback (widget)
  (let ((open (widget-value widget))
	(tag (widget-get widget :tag))
	children buttons)
    (cond
     (open
      (push (widget-create-child-and-convert widget 'jde-widget-tree-close-button)
		buttons)
      (insert "-\\ " tag "\n")
      (let ((prefix (concat (widget-get widget :prefix)
				(widget-get widget :prefix-extra)))
	    (nodes (widget-get widget :nodes))
	    node)

	(when (and (widget-get widget :has-nodes)
		   (not nodes))
	  (setq nodes
		(funcall (widget-get widget :node-fcn) widget))
	  (if nodes
	      (widget-put widget :nodes nodes)
	    (widget-put widget :has-nodes nil)))
	
	(while nodes
	  (setq node (car nodes)
		nodes (cdr nodes))
	  (insert prefix)
	  (push
	   (if nodes
	       (widget-create-child-and-convert widget node
						:prefix prefix
						:prefix-extra " | ")
	     (widget-create-child-and-convert
	      widget node
	      :prefix prefix
	      :prefix-empty " `--- "))
	   children))))
     (t
      ;; Closed node.
      (push (widget-create-child-and-convert widget 'jde-widget-tree-open-button)
	    buttons)
      (insert "-- " tag "\n")))
    (widget-put widget :children children)
    (widget-put widget :buttons buttons)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Java object widget                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-test-get-fields (process object-id)
  (list 
   (cons (list "sum" "double") (list "double" 0.0))
   (cons (list "r" "int") (list "int" 1))
   (cons (list "z" "java.lang.String") (list "java.lang.String" 229 nil))
   (cons (list "B" "double[]") (list "double[]" 228 nil))
   (cons (list "A" "double[][]") (list "double[][]" 227 nil))
   (cons (list "args" "java.lang.String[]") (list "java.lang.String[]" 226 nil))))

(defun jde-widget-java-var-to-tree (process var)
  (let* ((var-name (oref var name))
	 (var-type (oref var jtype))
	 (var-value (oref var value))
	 (var-tag (format "%s %s" var-type var-name)))
    (cond
     ((typep var-value 'jde-dbs-java-udci)
      (if (string= (oref var-value :jtype) "java.lang.String")
	  (let* ((cmd (jde-dbs-get-string 
		       "get string"
		       :process process
		       :object-id (oref var-value id)))
		 (str-val (jde-dbs-cmd-exec cmd)))
	    (list 'jde-widget-tree :tag var-tag :value t
		  (list 'jde-widget-tree :tag str-val)))
	(list 'jde-widget-java-obj :tag var-tag
	      :process process :object-id (oref var-value :id))))
     ((typep var-value 'jde-dbs-java-array)
      (list 'jde-widget-java-array :tag var-tag
	    :process process :object var-value))
     ((typep var-value 'jde-dbs-java-primitive)
      (list 'jde-widget-tree :tag var-tag :value t
	    (list 'jde-widget-tree 
		  :tag (format "%s" (oref var-value value)))))
     ((typep var-value 'jde-dbs-java-null)
      (list 'jde-widget-tree :tag var-tag :value t
	    (list 'jde-widget-tree :tag "null")))
     (t
      (error "Unidentified type of local variable: %s" var-tag)))))

(defun jde-widget-java-obj-get-fields (tree-widget)
  (let* ((process (widget-get tree-widget :process))
	 (object-id (widget-get tree-widget :object-id))
	 (cmd
	  (jde-dbs-get-object
	   (format "get_object %d" object-id)
	   :process process
	   :object-id object-id))
	 (object
	  (jde-dbs-cmd-exec cmd))
	 (fields (oref object fields))
	 field
	 nodes)
    (while fields
      (setq field (car fields) fields (cdr fields)) 
      (setq field (cdr field))
      (push 
       (jde-widget-java-var-to-tree process field)
       nodes)) 
    nodes))

(define-widget 'jde-widget-java-obj 'jde-widget-dtree
  "A widget that represents a Java object.
This widget is essentially a tree node whose entries are the fields
of the corresponding object. The first time the user expands the node,
the node retrieves the fields of the object from the debugger and
caches them. Thereafter, the node uses the cached values when the
user closes and then reopens the node. Use the syntax 
(widget-create 'jde-widget-java-obj 
:tag NAME :process PROCESS :object-id OBJ-ID) to create the widget where
NAME is the object's name, PROCESS is the process in which
the object exists, and  ID is the debugger id for the object."
  :node-fcn 'jde-widget-java-obj-get-fields)


(defun jde-widget-java-array-element-to-tree (process element index)
  (cond
     ((typep element 'jde-dbs-java-udci)
      (if (string= (oref element :jtype) "java.lang.String")
	  (let* ((cmd (jde-dbs-get-string 
		       "get string"
		       :process process
		       :object-id (oref element id)))
		 (str-val (jde-dbs-cmd-exec cmd)))
	    (list 'jde-widget-tree :tag (format "[%d] %s" index str-val)))
	(list 'jde-widget-java-obj 
	      :tag (format "[%d] %s" index (oref element jtype))
	      :process process :object-id (oref element id))))
     ((typep element 'jde-dbs-java-array)
      (list 'jde-widget-java-array 
	    :tag (format "[%d] %s" index (oref element jtype))
	    :process process :object element))
     ((typep element 'jde-dbs-java-primitive)
      (list 'jde-widget-tree :tag (format "[%d] %s"  index (oref element value))))
     ((typep element 'jde-dbs-java-null)
      (list 'jde-widget-tree :tag (format "[%d] null" index)))
     (t
      (error "Unidentified type of object: <%s|%s>" (oref element jtype) 
	     (oref element id)))))

(defun jde-widget-java-array-get-elements (tree-widget)
  (let* ((process (widget-get tree-widget :process))
	 (array (widget-get tree-widget :object))
	 cmd array-length)

    (setq cmd
	 (jde-dbs-get-array 
	   (format "get_array_length %d" (oref array id))
	   :process process
	   :array array))
    (jde-dbs-cmd-exec cmd) 

    (setq array-length
	  (if (slot-boundp array 'length)
	     (oref array length)
	    0))
    
    (when (> array-length 0)
      (setq cmd
	  (jde-dbs-get-array 
	   (format "get_array_elements %d" (oref array id))
	   :process process
	   :array array
	   :index 0
	   :length array-length))
      (jde-dbs-cmd-exec cmd)
      (let ((elements (oref array elements))
	    element
	    nodes 
	    (index 0))
	(while elements
	  (setq element (car elements) elements (cdr elements)) 
	  (setq nodes 
		(append nodes 
			(list (jde-widget-java-array-element-to-tree process element index))))
	  (setq index (1+ index))) 
	nodes))))

(define-widget 'jde-widget-java-array 'jde-widget-dtree
  "A widget that represents a Java array. Clicking on the widget's
expand button causes the widget to display the values of the array."
  :node-fcn 'jde-widget-java-array-get-elements)

(defun test-obj ()
  (interactive)
  (switch-to-buffer "*Java Object Example*")  
  (kill-all-local-variables)
  ;; (make-local-variable 'widget-example-repeat)  
  (let ((inhibit-read-only t))
    (erase-buffer))  
  (let ((all (overlay-lists)))  
    (mapcar 'delete-overlay (car all))    (mapcar 'delete-overlay (cdr all))) 

  (widget-insert "Test object tree. \n\n")

  (widget-create 'jde-widget-java-obj :tag "jmath.System s"   :process "process" :object-id 1)
  (widget-create 'jde-widget-java-obj :tag "java.awt.Frame frame1" :process "process" :object-id 1)
    
  (use-local-map widget-keymap)
  (widget-setup))


(provide 'jde-widgets)

;; $Log: jde-widgets.el,v $
;; Revision 1.6  2000/02/01 04:11:57  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.5  2000/01/17 09:36:41  paulk
;; Implemented array and object inspectors.
;;
;; Revision 1.4  2000/01/15 08:05:10  paulk
;; Implemented dynamic tree widget.
;;
;; Revision 1.3  1999/11/16 05:58:18  paulk
;; Added trace method commands and skeletons for trace class and cancel
;; trace commands.
;;
;; Revision 1.2  1999/10/13 06:19:57  paulk
;; Add JDEBug->Threads->Show Threads command
;;
;; Revision 1.1  1999/09/28 04:39:42  paulk
;; Initial revision.
;;

;; End of jde-widgets.el
    