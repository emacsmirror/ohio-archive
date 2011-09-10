;;; jde-wiz.el;; $Revision: 1.2;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998 Paul Kinnucan.

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

(require 'beanshell)
(require 'jde-complete)

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defcustom jde-wiz-import-excluded-packages '("bsh.*")
  "*Specifies classes that should not be imported into a source file.
The value of this variable should be a regular expression. The
`jde-wiz-find-and-import' command does not import any classes whose
fully qualified names match the regular expression. If more than one
fully qualified class name matches the unqualified name that you specify,
the command prompts you to select only the classes that do not match the
regular expression."
  :group 'jde-project
  :type '(repeat (string :tag "Package")))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>


(defun jde-wiz-update-class-list()
  "Update the class list used to resolve class names.
The first time you invoke a JDE wizard, the JDE builds a list of all classes on the classpath
defined by jde-global-classpath. Wizards use this list to resolve unqualified class names. If you add any classes to the classpath after invoking a wizard, you should update the class list."
  (interactive)
  (message "Rescanning classes..." )
  (bsh-eval "jde.util.JdeUtilities.buildClassList();" )
  (message "Rescanning classes...Complete"))


(defun jde-wiz-get-imports()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))    
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports 
			     (list (buffer-substring-no-properties 
				    (match-beginning 1) 
				    (match-end 1)))))))
    imports))

(defun jde-wiz-get-package-name ()
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
		       (match-beginning 1)
		       (match-end 1))))))

(defun jde-wiz-get-import-insertion-point ()
  (let ((ip-re
	 (list (cons "import[ ]+\\(.*\\)[ ]*;" 'backward)
	       (cons "package[ \t]+\\(.*\\)[ \t]*;" 'backward)
	       (cons "^$" 'forward)))
	insertion-point n)
    (save-excursion
      (let ((i 0)
	    (n (length ip-re)))
	(while (and
		(not insertion-point)
		(< i n))
	  (let ((re (car (nth i ip-re)))
		(direction (cdr (nth i ip-re))))
	    (if (eq direction 'forward)
		(progn
		  (goto-char (point-min))
		  (setq insertion-point (re-search-forward re (point-max) t)))
	      (goto-char (point-max))
	      (setq insertion-point (re-search-backward re (point-min) t)))
	    (when insertion-point
	      (forward-line 1)
	      (setq insertion-point (point))))
	  (setq i (+ i 1)))))
    insertion-point))


(defun jde-wiz-import (class) 
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an import
statement for the specified class alrady exists."
  (interactive
   "sClass: ")
  (jde-wiz-insert-imports (list class)))

;; Contributed by David Ponce <david_ponce@mail.schneider.fr>
(defun jde-sort-imports (&optional reverse)
  "Sort Java import statements alphabetically. In reverse order if
REVERSE is non-nil.

Usage:
  \\[jde-sort-imports] sort import statements ascending.
  \\[universal-argument] \\[jde-sort-imports] sort descending.

The the current buffer must be in `jde-mode'. This command uses the
semantic Java parser and requires JDE 2.1.6-beta24 and above."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found. Must be 'jde-mode'."))
  (or (and (local-variable-p 'semantic-toplevel-bovine-table (current-buffer))
           (symbol-value 'semantic-toplevel-bovine-table))
      (error "Semantic Java parser not found. JDE 2.1.6-beta24+
needed."))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq reverse t))
  (let* ((tokens  (semantic-bovinate-toplevel nil nil t))
         (depends (semantic-find-nonterminal-by-token 'include tokens)))
    (if depends
        (let* ((first-import-token (car depends))
               (last-import-token  (nth (1- (length depends)) depends))
               (start (semantic-token-start first-import-token))
               (end   (semantic-token-end   last-import-token)))
          (if (and start end)
              (let (sort-fold-case)
                (sort-lines reverse start end)
                (goto-char start)))))))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-wiz-find-and-import (class)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can use
the variable `jde-wiz-import-excluded-packages' to prevent
specified classes from being imported or consider for import. This command uses
the JDE's BeanShell interpreter. It starts the interpreter if it is not
already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, including jars implicitly included by the jvm, e.g.,
rt.jar."
  (interactive
   (list (read-from-minibuffer "Class: "
			       (thing-at-point 'symbol))))
  (let (existing-import)
    (setq existing-import (jde-wiz-get-existing-import class))
    (if (not (null existing-import))
	(message "Skipping: already imported %s" existing-import)
      (let ((imports
             (bsh-eval-r
              (concat "jde.util.JdeUtilities.getQualifiedName(\""
                      class "\");"))))
        (if imports
            (jde-wiz-insert-imports imports)
          (message "Error: could not find %s." class))))))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-wiz-insert-imports (new-imports)
  (let* ((imports
	  (mapcar 'jde-wiz-strip-excluded-imports
	   (jde-wiz-strip-existing-imports new-imports 
					   (jde-wiz-get-imports)))))
    ;;Delete the nil which result from the excluded ones
    (setq imports (delq nil imports))
    ;; If more than one class matches the specified class name, 
    ;; prompt the user to select a class for import.
    (if (> (length imports) 1 )
	(jde-wiz-choose-imports imports)
      (jde-wiz-insert-imports-into-buffer imports))))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

(defun jde-wiz-strip-excluded-imports (new-import)
  "Removes excluded imports from the list"
  ;;if the string matchs the regexp we want to ignore it.
  (if jde-wiz-import-excluded-packages
      (let (i n result)
        (setq i 0)
        (message "exclude-regexp=%s"
		 jde-wiz-import-excluded-packages)
        (setq n (length jde-wiz-import-excluded-packages))
        (setq result new-import)
        (while (< i n)
          (let ((exclude-regexp
                 (nth i jde-wiz-import-excluded-packages)))
            (message "exclude-regexp=%s" exclude-regexp)
            (message "new-import=%s" new-import)
	    (if (or (not (string-match "[.]" new-import))
		    (string-match exclude-regexp new-import))
                (progn
                  (message "Excluding import: %s" new-import)
                  (setq result nil)))
            (setq i (+ i 1))))
        result)
    new-import))

(defun jde-wiz-strip-excluded-import (exclude-regexp new-import)
  "Removes excluded imports from the list"
  ;;if the string matchs the regexp we want to ignore it.
  (if (string-match exclude-regexp (concat " " new-import))      
      (progn (message "Excluding import: %s" new-import)
             ())
    new-import))

(defun jde-wiz-insert-imports-into-buffer (new-imports)
  "Inserts imports into the correct place in the buffer."
  (let (i n)
    (save-excursion
      (goto-char (jde-wiz-get-import-insertion-point))
      (setq i 0)
      (setq n (length new-imports))
      (while (< i n)
	(let ((new-import 
	       (nth i new-imports)))
	  (progn
	    (insert
	     (concat "import " new-import ";\n"))
	    (message "Imported %s" new-import))
	  (setq i (+ i 1)))))))


(defun jde-wiz-strip-existing-imports (new-imports existing-imports)
  "Exclude classes that have already been imported."
  (let (i n return-imports)
    (setq i 0)
    (setq n (length new-imports))
    (while (< i n)
      ;;iterate through the new imports
      (let((new-import
	    (nth i new-imports)))
	;;Strip out those alreay there
	(when (not (find new-import existing-imports :test 'string=))
	  (setq return-imports (nconc (list new-import)
				      return-imports))))
      (setq i(+ i 1)))
    ;;Return any that still exist
    return-imports))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-wiz-get-existing-import (class-name)
  ""
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil)
        (existing-import)
        (result nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
        (setq existing-import (buffer-substring-no-properties
                               (match-beginning 1)
                               (match-end 1)))
        (if (string-equal class-name
                          (jde-wiz-strip-package-from-class
			   existing-import))
            (setq result existing-import))))
    result))

(defun jde-wiz-already-imports-class (class-name)
  "Determine if a class is already being imported (ignoring packages)"
  (find class-name (jde-wiz-get-imports-no-package) :test 'string=))

(defun jde-wiz-strip-package-from-class (class-name)
  "Strips the package name from fully qualified java class"
  (let (i return-name)
    (setq return-name class-name)
    (setq i (string-match "[^.]*$" class-name))
    (if i
        (setq return-name (substring class-name i)))
    return-name))

(defun jde-wiz-get-imports-no-package()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports
			     (list (jde-wiz-strip-package-from-class
				    (buffer-substring-no-properties
				     (match-beginning 1)
				     (match-end 1))))))))
    imports))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>


(defun jde-wiz-choose-imports (new-imports)
  "Prompts the user to select a class to import from a list of similarly
named candidates."
  (let ((buf (get-buffer-create "*Select Import Clases*" )))
    (setq jde-wiz-import-possibles new-imports)
    (setq jde-wiz-selected-imports new-imports)
    (set-buffer buf)
    (widget-insert "Several classes match the name you specified.\n")
    (widget-insert "Select the ones you want to import.\n")
    (widget-insert "Then click the OK button.\n" )
    (let (i n)				;Iteration vars
      (setq i 0)
      (setq n (length new-imports))
      (while (< i n)
	(let* ((new-import
		(nth i new-imports))
	       (args (list
		      ;;widget of type checkbox
		      'checkbox
		      :value new-import
		      ;;with a value equal to the importable file
		      :format "\n %[%v%]  %t"
		      :tag new-import
		      ;;When we activate this button we need to add to the
		      ;;selected imports
		      :notify (lambda (widget &rest ignore)
				(let ((value (widget-get widget ':tag)))
				  ;;if the widgets value is already in the selected imports
				  (if (find value jde-wiz-selected-imports)
				      ;;then we need to remove it
				      (progn(setq jde-wiz-selected-imports
						  (delq value jde-wiz-selected-imports))
					    (message "You have deselected: %s" value))
				    ;;else we need to add it
				    (progn(setq jde-wiz-selected-imports
						(nconc (list value)
						       jde-wiz-selected-imports))
					  (message "You have selected: %s" value))))))))
	  (apply 'widget-create args)
	  (setq i (+ i 1)))))
    ;;Then insert the okay button
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let ((dialog-buffer
				    (current-buffer)))
			       (if pop-up-windows (delete-window))
			       (kill-buffer dialog-buffer)
			       (if jde-wiz-selected-imports
				   (progn (jde-wiz-insert-imports-into-buffer
					   jde-wiz-selected-imports)
					  (message "Imports complete"))
				 (message "No imports selected."))))
		   "Ok")
    (use-local-map widget-keymap)
    (widget-setup)
    (pop-to-buffer buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Interface Implementation wizard                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-wiz-get-unqualified-name (name)
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun jde-wiz-update-implements-clause (interface-name)
   (interactive
    "sEnter interface: ")
  (let ((interface 
	(jde-wiz-get-unqualified-name interface-name)))
    (save-excursion
      (let* ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*")
	     (open-brace-pos
	      (scan-lists (point) -1 1))
	     (class-name-end-pos
	      (when open-brace-pos
		(goto-char open-brace-pos)
		(when (re-search-backward class-re (point-min) t)
		  (looking-at class-re)
		  (match-end 1))))
	     (implements-keyword-end-pos
	      (when (and open-brace-pos class-name-end-pos)
		(goto-char open-brace-pos)
		(if (re-search-backward "implements" class-name-end-pos t)
		    (match-end 0)))))
	(if implements-keyword-end-pos
	    (progn
	      (goto-char implements-keyword-end-pos)
	      (insert (concat " " interface ", ")))
	  (when class-name-end-pos
	    (goto-char (- open-brace-pos 1))
	      (insert (concat " implements " interface " "))))))))


(defun jde-wiz-implement-interface (interface-name)
  "*Generate a skeleton implementation of a specified interface."
  (interactive
   "sInterface name: ")
  (condition-case err
      (let* ((nl-brace-p
	      (find 'before 
		    (cdr (assoc 'defun-open c-hanging-braces-alist))))
	     (code
	      (bsh-eval-r
	       (concat
		"jde.wizards.InterfaceFactory.makeInterface(\""
		interface-name "\", true, true, "
		(if nl-brace-p "true" "false") ");"))))
	(if code 
	    (let ((required-imports
		   (bsh-eval-r
		    "jde.wizards.InterfaceFactory.getImportedClasses();")))
	      (insert code)
	      (if required-imports
		  (jde-wiz-insert-imports-into-buffer required-imports))
	      (jde-wiz-update-implements-clause interface-name))))	  
    (error
     (message "%s" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override wizard                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jde-wiz-get-method-class ()
  (let ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (buffer-substring-no-properties
		     (match-beginning 1)
		     (match-end 1))))))))


(defun jde-wiz-override-method (method-name)
  "Overrides a method whose name you specify.
This command creates a skeleton implementation of the
overridden method at point. This command infers the
qualified name of the class of the overriden method by 
prepending the package name of the current buffer to
the class containing point. If the class defines
more than one method of the same name, this command
prompts you to select the desired method from a list
of method prototypes.

This command also generates import statements for 
the parameter and return types of the overridden method.
The import statements are inserted after the last 
existing import statement or the package statement
or the first blank line in the source file. Import
statements are generated only for types for which an
import statement does not already exist in the file.

NOTE: this command works only if the overriding class 
      has been previously compiled."
  (interactive
   "sMethod name: ")
  (condition-case err
      (let* ((package-name (jde-wiz-get-package-name))
	     (class-name (jde-wiz-get-method-class))
	     (qualified-class-name 
	      (if (and package-name class-name)
		  (concat package-name "." class-name)
		class-name)))
	(if qualified-class-name
	    (let ((signatures
		   (bsh-eval
		    (concat 
		     "jde.wizards.MethodOverrideFactory.getCandidateSignatures(\""
		     qualified-class-name "\",\"" method-name "\");") t)))
	      (if signatures
		  (if (> (length signatures) 1)
		      (jde-wiz-override-variant-method signatures)
		    (jde-wiz-override-method-internal (car signatures)  signatures))))))
    (error
     (message "%s" (error-message-string err)))))

(defun jde-wiz-override-method-internal (selected-method methods)
  (let* ((variant
	(position selected-method methods :test 'string=))
	(nl-brace-p
	  (find 'before 
		(cdr (assoc 'defun-open c-hanging-braces-alist))))
	(skeleton
	  (bsh-eval-r
	   (concat
	    "jde.wizards.MethodOverrideFactory.getMethodSkeleton("
	    (int-to-string variant) 
	    (if nl-brace-p
		", true"
	      ", false")
	    ");")))
	(required-imports
	  (bsh-eval-r
	   "jde.wizards.MethodOverrideFactory.getImportedClasses();")))
    (insert skeleton)
    (if required-imports
	(jde-wiz-insert-imports required-imports))))


(defun jde-wiz-override-variant-method (methods) 
  (let ((buf (get-buffer-create "*Choose Method*")))
    (setq jde-wiz-source-buffer (current-buffer))
    (setq jde-wiz-method-variants methods)
    (setq jde-wiz-selected-method (car methods))
    (set-buffer buf)
    (widget-insert "Select the method you want to override.\n")
    (widget-insert "Then click the Ok button.\n\n")
    (let ((args (list
		'radio-button-choice
		:value (car methods)
		:notify (lambda (widget &rest ignore)
			   (setq jde-wiz-selected-method (widget-value widget))
			   (message "You selected: %s"
				    (widget-value widget))))))
	  (setq args (nconc
		      args
		       (mapcar (lambda (x) (list 'item x)) methods)))
	  (apply 'widget-create args)
	  )
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let ((dialog-buffer
				    (current-buffer)))
			       (set-buffer jde-wiz-source-buffer)
			       (delete-window)
			       (kill-buffer dialog-buffer)
			       (jde-wiz-override-method-internal
				jde-wiz-selected-method 
				jde-wiz-method-variants)
			       (message "Method inserted.")
			     ))
     		"Ok")
    (use-local-map widget-keymap)
    (widget-setup)
    (pop-to-buffer buf)))


;;
;; Contributed by Rohit Namjoshi <Rohit_Namjoshi@trilogy.com>
;;
(defun jde-browse-class ()
  "Displays class at point in BeanShell class browser."
  (interactive)
  (condition-case err
      (let* ((unqualified-name (thing-at-point 'symbol))
             (class-names
              ;;expand the names into full names, or a list of names
              (bsh-eval-r
               (concat "jde.util.JdeUtilities.getQualifiedName(\""
		       unqualified-name "\");")))
             ;; Pick first match
             (cmd (concat "browseClass(\"" (car class-names) "\");")))
        (if (eq nil class-names)
            (error "Cannot find %s" unqualified-name))
        (message cmd)
        (bsh-eval cmd))
    (error
     (message "%s" (error-message-string err)))))


;; Required (at least for me) because indent-region does not seem to
;; indent javadoc comments properly.  This function is used to indent
;; the code inserted by jde-wiz-delegate.  (It might also be useful to
;; indent the output of jde-wiz-implement-interface)
(defun jde-indent-java-region (start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (setq end (set-marker (make-marker) end))
    (c-indent-line)
    (while (< (point) end)
      (c-indent-line)
      (forward-line 1))))


(defun jde-wiz-delegate (attribute-name)
  "*Generate delegation methods for a given attribute.
This method generates methods in the current buffer that delegate
calls to methods of the given ATTRIBUTE of the current class.  For
example, if the current buffer contains Java class A and there is
an attribute in A named b of class type B, this method will
generate in A, all the public methods of class B and delegate
handling of those methods via attribute B."
  (interactive
   "sAttribute name: ")
  (condition-case err
      (let* ((start nil)
             (class-name (or (jde-complete-get-qualified-name
                              (jde-parse-declared-type-of attribute-name))
                             (read-string (concat "Enter fully qualified class name of " 
                                                  attribute-name ": "))))
             (nl-brace-p
	      (find 'before 
		    (cdr (assoc 'defun-open c-hanging-braces-alist))))
	     (code
	      (bsh-eval-r
	       (concat
		"jde.wizards.DelegateFactory.makeDelegates(\""
		attribute-name "\", \"" class-name "\", true, true, "
		(if nl-brace-p "true" "false") ");"))))
	(if code 
	    (let ((required-imports
		   (bsh-eval-r
		    "jde.wizards.DelegateFactory.getImportedClasses();")))
              (font-lock-mode -1)
              (setq start (point))
	      (insert code)
              (jde-indent-java-region start (point))
	      (if required-imports
		  (jde-wiz-insert-imports-into-buffer required-imports))
              (font-lock-mode)
              )))	  
    (error
     (message "%s" (error-message-string err)))))

  
(provide 'jde-wiz)

;; $Log: jde-wiz.el,v $
;; Revision 1.27  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.26  2000/07/14 05:22:57  paulk
;; Adds a delegation wizard contributed by Charles Hart.
;;
;; Revision 1.25  2000/07/13 07:18:24  paulk
;; * You can now specify a list of packages to exclude from import
;;   into a source file. See jde-wiz-import-excluded-packages for
;;   more information. Thanks to "Jim Loverde" <loverde@str.com>
;;   for this enhancement.
;;
;; * Changed name of jde-wiz-insert-excluded-packages-regexp to
;;   jde-wiz-import-excluded-packages.
;;
;; Revision 1.23  2000/06/22 02:50:25  paulk
;; The import wizard dialog now uses radio buttons rather than check boxes to select
;;  the class to import. Thanks to Mark Gibson for this enhancement.
;;
;; Revision 1.22  2000/06/01 06:01:14  paulk
;; Added jde-sort-imports command. Thanks to David Ponce <david_ponce@mail.schneider.fr>.
;;
;; Revision 1.21  2000/01/18 07:11:26  paulk
;; Added jde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.20  1999/12/19 07:02:30  paulk
;; Changed import wizard to use jde.util.JdeUtilities.getQualifiedName
;; eliminated redundancy. Thanks to Len Trigg <len@intelligenesis.net>
;; for this improvement.
;;
;; Revision 1.19  1999/11/01 03:11:42  paulk
;; Added jde-browse-class contributed by Rohit Namjoshi <Rohit_Namjoshi@trilogy.com>.
;;
;; Revision 1.18  1999/10/17 04:35:05  paulk
;; Fixed a line in jde-wiz.el, where an int is concat'd with some
;; strings.  This is not allowed by XEmacs 21.1.
;;
;; Revision 1.17  1999/10/01 05:58:14  paulk
;; Added jde-wiz-update-class-list function contributed by Phillip Lord.
;;
;; Revision 1.16  1999/09/17 06:52:50  paulk
;; Fixed regression error where the interface wizard was putting quotes
;; around the code inserted into the source buffer.
;;
;; Revision 1.15  1999/08/29 04:29:18  paulk
;; Patches provided by Michael Ernst <mernst@alum.mit.edu>
;;
;; Revision 1.14  1999/08/23 02:13:43  paulk
;; Fixed regression bug in jde-wiz-implement-interface caused by recent
;; change in jde-wiz-insert-imports.
;;
;; Revision 1.13  1999/06/22 21:12:20  paulk
;; Added variable to filter out unwanted classes from the list of classes being
;; considered for import command by jde-find-and-import. The jde-find-and-import
;; command now prompts the user if more than one class matches the specified
;; import name. Thanks to Phillip Lord <plord@hgmp.mrc.ac.uk> for these enhancements.
;;
;; Revision 1.12  1999/05/07 20:42:25  paulk
;; Cosmetic change.
;;
;; Revision 1.11  1999/05/07 20:40:49  paulk
;; Added new command, jde-wiz-find-and-import, that, given an unqualified class
;; name, generates and inserts an import statement for that class.
;;
;; Revision 1.10  1999/02/17 19:16:07  paulk
;; Provided more robust error handling for the interface wizard. The wizard
;; no longer kills the bsh when it cannot create an interface and provides
;; meaningfull error messages.
;;
;; Revision 1.9  1999/02/15 01:12:54  paulk
;; Fixed bug in jde-wiz-get-method-class that caused it to fail when the open bracket
;; for the class was not on the same line as the class keyworkd. Thanks to
;; P.Lord@mdx.ac.uk (Phillip Lord) for diagnosing this bug.
;;
;; Revision 1.8  1999/02/12 15:13:00  paulk
;; Added jde-wiz-import function.
;;
;; Revision 1.7  1999/02/11 19:14:50  paulk
;; Fixed bug in jde-wiz-update-implements-clause.
;;
;; Revision 1.6  1999/02/11 18:28:40  paulk
;; Corrected missing parentheses.
;;
;; Revision 1.5  1998/11/22 22:03:43  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.4  1998/11/22 21:55:33  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.3  1998/11/21 02:41:34  paulk
;; Fixed bug.
;; Added implements clause update function to interface implementation wizard.
;;
;; Revision 1.2  1998/11/10 00:46:39  paulk
;; Added smart import insertion to interface wizard.
;;
;; Revision 1.1  1998/11/08 00:39:24  paulk
;; Initial revision
;;


;; End of jde-wiz.el
