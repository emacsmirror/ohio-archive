;; jde-javadoc.el --- Javadoc template generator;; Copyright (C) 1998, 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>, Paul Kinnucan <paulk@mathworks.com>
;; Created: 8 Oct 1998
;; Version: 1.2 beta2
;; Keywords: tools
;; VC: $Id: jde-javadoc.el,v 1.5 2000/07/13 05:22:48 paulk Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library helps to document Java classes and methods. The
;; command `jde-javadoc-generate-javadoc-template' automatically
;; inserts a javadoc block comment template above the field, method,
;; class or interface declaration at point.
;;
;; Starting with version 1.2 jde-javadoc uses the semantic bovinator
;; parser table to find declaration tokens.
;;
;; The command `jde-javadoc-customize' (or customize-group
;; jde-javadoc) allows you to customize each category of javadoc
;; comment line (description, @version, @return, ...).

;;; Code:
(require 'tempo)
(require 'semantic)

(defconst jde-javadoc-version "1.2 beta2 $Date: 2000/07/13 05:22:48 $"
  "jde-javadoc version information.")

;;;
;;; Customization
;;;

(defgroup jde-javadoc nil
  "Javadoc template generator"
  :group 'tools
  :prefix "jde-javadoc-")

;; IMPORTANT: This function must be defined before the following defcustoms
;; because it is used in their :set clause.
(defun jde-javadoc-define-template (sym val)
  "Define a template (see `tempo-define-template'). The template name
is the `symbol-name' of SYM from which the '-template' suffix has been
removed, prefixed by 'tempo-template-'. VAL is the template value."
  (let* ((name (symbol-name sym))
         (template-name
          (if (string-match "\\(.*\\)-template$" name)
              (match-string 1 name)
            (error "Invalid template variable name: %S" name))))
    (tempo-define-template template-name val nil name)
    (set-default sym val)))

(defcustom jde-javadoc-describe-class-template
  '("* Describe class " (jde-javadoc-code name) " here.")
  "*Line template used to describe a class.
If nil the line is not inserted.
The variable 'name' is set to the class name.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-describe-class'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-interface-template
  '("* Describe interface " (jde-javadoc-code name) " here.")
  "*Line template used to describe an interface.
If nil the line is not inserted.
The variable 'name' is set to the interface name.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-describe-interface'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-constructor-template
  '("* Creates a new " (jde-javadoc-code name) " instance.")
  "*Line template used to describe a constructor.
If nil the line is not inserted.
The variable 'name' is set to the constructor name (that is the class name).
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-describe-constructor'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-method-template
  '("* Describe " (jde-javadoc-code name) " method here.")
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the method name.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-describe-method'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-field-template
  '("* Describe " (jde-javadoc-field-type modifiers) " " (jde-javadoc-code name) " here.")
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the field name.
The variable 'type' is set to the field type.
The variable 'modifiers' is set to the field modifiers.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-describe-field'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-param-tag-template
  '("* @param " name " " (jde-javadoc-a type) " " (jde-javadoc-code type) " value")
  "*Line template used to describe a parameter.
If nil the line is not inserted.
The variable 'name' is set to the parameter name.
The variable 'type' is set to the parameter type.
A line is inserted for each parameter.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-param-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-return-tag-template
  '("* @return " (jde-javadoc-a type) " " (jde-javadoc-code type) " value")
  "*Line template used to describe a returned value.
If nil the line is not inserted.
The variable 'type' is set to the returned type.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-return-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-exception-tag-template
  '("* @exception " type " if an error occurs")
  "*Line template used to describe an exception.
If nil the line is not inserted.
The variable 'type' is set to the exception type.
A line is inserted for each exception in the 'throws' clause.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-exception-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-author-tag-template
  '("* @author <a href=\"mailto:" user-mail-address "\">" user-full-name "</a>")
  "*Line template used to give an author.
If nil the line is not inserted.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-author-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-version-tag-template
  '("* @version 1.0")
  "*Line template used to give a version.
If nil the line is not inserted.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-version-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-see-tag-template
  '("* @see " ref)
  "*Line template used to give a reference.
If nil the line is not inserted.
The variable 'ref' is set to the class or interface name.
A line is inserted for each name in the 'extends' then 'implements' clauses.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-see-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-since-tag-template
  '("* @since 1.0")
  "*Line template used to give a since reference.
If nil the line is not inserted.
See `jde-javadoc-generate-javadoc-template' for usage.
Define the template `tempo-template-jde-javadoc-since-tag'."
  :group 'jde-javadoc
  :type '(repeat sexp)
  :set 'jde-javadoc-define-template)

;;;
;;; semantic bovinator parser table lookup
;;;

(defun jde-javadoc-lookup-class (token pos prev)
  (let (start parts)
    (setq start (semantic-token-start token))
    (if (>= start pos)
        (throw 'found
               (if (= start pos) token prev)))
    (setq prev token)
    (setq parts (semantic-token-type-parts token))
    (while parts
      (setq token (car parts))
      (if (eq (semantic-token-token token) 'type)
          (setq prev (jde-javadoc-lookup-class token pos prev))
        (setq start (semantic-token-start token))
        (if (>= start pos)
            (throw 'found
                   (if (= start pos) token prev)))
        (setq prev token))
        (setq parts (cdr parts)))
    prev))

(defun jde-javadoc-lookup-classes (classes pos prev)
  (while classes
    (setq prev    (jde-javadoc-lookup-class (car classes) pos prev))
    (setq classes (cdr classes)))
  prev)

(defun jde-javadoc-lookup-bovine-table ()
  "Search the bovine table for the token at the current line."
  (save-excursion
    ;; Moves the point to the first non-blank character found on the
    ;; current line. Blank lines are skipped.
    (beginning-of-line)
    (while (looking-at "[ \n\r\t]")
      (forward-char))
    (let* ((p       (point))
           (tokens  (semantic-bovinate-toplevel nil nil t))
           (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (catch 'found (jde-javadoc-lookup-classes classes p nil)))))

;;;
;;; Documentation helpers
;;;

(defun jde-javadoc-field-type (modifiers)
  "Return \"constant\" if field MODIFIERS contains \"static\" and
\"final\" otherwise return \"variable\".
Useful to generate field description."
  (if (and (member "static" modifiers) (member "final" modifiers))
      "constant"
    "variable"))

(defun jde-javadoc-a (word)
  "Return \"an\" if WORD begin with a vowel or \"a\" otherwise.
Useful to generate description like \"an int value\" or \"a long value\"."
  (if (string-match "^[aeiouyAEIOUY]" word)
      "an" "a"))

(defun jde-javadoc-code (text)
  "Return \"<code>TEXT</code>\".
Useful to generate HTML code style."
  (concat "<code>" text "</code>"))

;;;
;;; Javadoc template insertion
;;;

(defun jde-javadoc-insert-start-block ()
  "Insert a javadoc start comment block '/**'."
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (indent-according-to-mode)
  (insert "/**")
  (reindent-then-newline-and-indent))
  
(defun jde-javadoc-insert-empty-line ()
  "Insert an empty javadoc line '*'."
  (insert "*")
  (reindent-then-newline-and-indent))
  
(defun jde-javadoc-insert-end-block ()
  "Insert a javadoc end comment block '*/'."
  (insert "*/")
  (reindent-then-newline-and-indent))
  
(defun jde-javadoc-insert-template (template-name)
  "Insert the `tempo' line template TEMPLATE-NAME. Does nothing if
TEMPLATE is nil."
  (when (symbol-value template-name)
    (tempo-insert-template template-name nil)
    (reindent-then-newline-and-indent)))

(defun jde-javadoc-insert-see-tag (ref)
  "Insert a javadoc @see tag referencing REF."
  (and ref (not (string= ref ""))
       (jde-javadoc-insert-template 'tempo-template-jde-javadoc-see-tag)))

(defun jde-javadoc-insert-param-tag (type name)
  "Insert a javadoc @param tag for a TYPE NAME parameter."
  (and type (not (string= type ""))
       name (not (string= name ""))
       (jde-javadoc-insert-template 'tempo-template-jde-javadoc-param-tag)))

(defun jde-javadoc-insert-exception-tag (type)
  "Insert a javadoc @exception tag for the exception TYPE."
  (and type (not (string= type ""))
       (jde-javadoc-insert-template 'tempo-template-jde-javadoc-exception-tag)))

(defun jde-javadoc-insert-return-tag (type)
  "Insert a javadoc @return tag of type TYPE."
  (and type (not (string= type "void"))
       (jde-javadoc-insert-template 'tempo-template-jde-javadoc-return-tag)))

(defun jde-javadoc-insert-field-desc (modifiers type name)
  "Insert a description for the field NAME of type TYPE.
MODIFIERS contains the list of field modifiers."
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-describe-field))

(defun jde-javadoc-insert-method-desc (type name)
  "Insert a description for the method NAME of type TYPE.
If TYPE is nil insert a constructor description."
  (and name (not (string= name ""))
       (jde-javadoc-insert-template
        (if (and type (not (string= type "")))
            'tempo-template-jde-javadoc-describe-method
          'tempo-template-jde-javadoc-describe-constructor))))

(defun jde-javadoc-insert-class-desc (name)
  "Insert a description for the class NAME."
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-describe-class))

(defun jde-javadoc-insert-interface-desc (name)
  "Insert a description for the interface NAME."
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-describe-interface))

;;;
;;; Javadoc specialized generators
;;;

(defun jde-javadoc-type-doc (modifiers type name parents) ; semantic 1.2
  "Document a class or interface.

- - MODIFIERS  list of class or interface modifiers
- - TYPE       \"class\" or \"interface\"
- - NAME       class or interface name
- - PARENTS    (EXTENTS . IMPLEMENTS) where:
- - EXTENDS    list of super class or super interface names
- - IMPLEMENTS list of implemented interface names"
  (if (string-equal type "interface")
      (jde-javadoc-insert-interface-desc name)
    (jde-javadoc-insert-class-desc name))
  (jde-javadoc-insert-empty-line)
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-author-tag)
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-version-tag)
  (jde-javadoc-insert-template 'tempo-template-jde-javadoc-since-tag)
  (mapcar 'jde-javadoc-insert-see-tag parents)) ; semantic 1.2

(defun jde-javadoc-function-arg-doc (modifiers type name)
  "Document a method argument.

- - MODIFIERS list of argument modifiers
- - TYPE      argument type
- - NAME      argument name."
  (jde-javadoc-insert-param-tag type name))

(defun jde-javadoc-function-arg (token)
  "Document method argument TOKEN."
  (let ((modifiers (semantic-token-variable-modifiers token))
        (type      (semantic-token-type               token))
        (name      (semantic-token-name               token)))
    (jde-javadoc-function-arg-doc modifiers type name)))

(defun jde-javadoc-function-doc (modifiers type name args throws)
  "Document a method or constructor.

- - MODIFIERS list of method or constructor modifiers
- - TYPE      method or constructor type
- - NAME      method or constructor name
- - ARGS      list of argument tokens
- - THROWS    list of thrown exception names"
  (jde-javadoc-insert-method-desc type name)
  (jde-javadoc-insert-empty-line)
  (mapcar 'jde-javadoc-function-arg args)
  (jde-javadoc-insert-return-tag type)
  (mapcar 'jde-javadoc-insert-exception-tag throws))

(defun jde-javadoc-variable-doc (modifiers type name)
  "Document a field.

- - MODIFIERS list of field modifiers
- - TYPE      field type
- - NAME      field name"
  (jde-javadoc-insert-field-desc modifiers type name))

;;;
;;; Main generators based on semantic token types
;;;

(defun jde-javadoc-type (token)
  "Document a 'type' (class or interface) TOKEN."
  (let ((modifiers  (semantic-token-type-modifiers        token))  ; semantic 1.2
        (type       (semantic-token-type                  token))
        (name       (semantic-token-name                  token))
        (parents    (semantic-token-type-parent           token))) ; semantic 1.2
    (jde-javadoc-type-doc modifiers type name parents)))

(defun jde-javadoc-function (token)
  "Document a 'function' (method or constructor) TOKEN."
  (let ((modifiers  (semantic-token-function-modifiers    token))  ; semantic 1.2
        (type       (semantic-token-type                  token))
        (name       (semantic-token-name                  token))
        (args       (semantic-token-function-args         token))
        (throws     (semantic-token-function-throws       token))) ; semantic 1.2
    (jde-javadoc-function-doc modifiers type name args throws)))

(defun jde-javadoc-variable (token)
  "Document a 'variable' (field) TOKEN."
  (let ((modifiers  (semantic-token-variable-modifiers    token))
        (type       (semantic-token-type                  token))
        (name       (semantic-token-name                  token)))
    (jde-javadoc-variable-doc modifiers type name)))

;;;
;;; The generic javadoc generator
;;;

(defun jde-javadoc-generator (token)
  "Call the javadoc generator associated to TOKEN."
  (let* ((type      (semantic-token-token token))
         (generator (intern (concat "jde-javadoc-"
                                    (symbol-name type)))))
    (cond ((fboundp generator)
           (goto-char (semantic-token-start token))
           (beginning-of-line)
           (jde-javadoc-insert-start-block)
           (funcall generator token)
           (jde-javadoc-insert-end-block))
          (t
           (error "No generator found to process '%S' token" type)))))

;;;
;;; Commands
;;;

;;;###autoload
(defun jde-javadoc-customize ()
  "Show the jde-javadoc customization options panel."
  (interactive)
  (customize-group "jde-javadoc"))

;;;###autoload
(defun jde-javadoc-generate-javadoc-template ()
  "Insert a javadoc block comment template above the class, method or
field declaration at point. Uses the semantic bovinator parser table
to find declarations (see `jde-javadoc-lookup-bovine-table').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION. IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jde-javadoc-generate-javadoc-template] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jde-javadoc-generate-javadoc-template] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[jde-javadoc-generate-javadoc-template] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of javadoc line. The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jde-javadoc-author-tag-template'
- - `jde-javadoc-describe-class-template'
- - `jde-javadoc-describe-constructor-template'
- - `jde-javadoc-describe-interface-template'
- - `jde-javadoc-describe-method-template'
- - `jde-javadoc-describe-field-template'
- - `jde-javadoc-exception-tag-template'
- - `jde-javadoc-param-tag-template'
- - `jde-javadoc-return-tag-template'
- - `jde-javadoc-see-tag-template'
- - `jde-javadoc-since-tag-template'
- - `jde-javadoc-version-tag-template'

For example if you customize `jde-javadoc-describe-class-template' with the
following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the
minibuffer. See also the `jde-javadoc-field-type', `jde-javadoc-a' and
`jde-javadoc-code' helper functions."
  (interactive)
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found. Must be 'jde-mode'."))
  (let ((found (jde-javadoc-lookup-bovine-table)))
    (if found
        (jde-javadoc-generator found)
      (error "No token found at point"))))

(provide 'jde-javadoc)


;;
;; $Log: jde-javadoc.el,v $
;; Revision 1.5  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.4  2000/07/08 07:15:41  paulk
;; Latest updates from David.
;;
;; Revision 1.2  2000/06/22 03:36:40  paulk
;; Fix submitted by David Ponce.
;;
;; Revision 1.1  2000/06/12 08:19:03  paulk
;; Initial revision.
;;
;;

;;; jde-javadoc.el ends here
