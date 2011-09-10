;;; jde-parse.el
;; $Revision: 1.17 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
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

(require 'semantic)
(require 'semantic-sb)
(require 'semantic-bnf)

(defcustom jde-imenu-enable t
  "Enables creation of a classes index menu in the Emacs menubar."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-imenu-include-signature t
  "If non-nil the \"Classes\" imenu displays full method signatures
and field types. Use \"*rescan*\" to update the imenu when this option 
is changed."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-imenu-include-classdef t
  "If non-nil `jde-imenu-index-class' adds *class def* items in imenu index
to go to class definition."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-imenu-sort nil
  "If non-nil sorts items in the classes index menu.
You can choose:

- - 'asc   to sort by token name ascending (ignoring case).
- - 'desc  to sort by token name descending (ignoring case).

Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'jde-project
  :type '(choice (const :tag "No sort"    nil )
                 (const :tag "Ascending"  asc )
                 (const :tag "Descending" desc)))

(defvar jde-parse-bovine-java-grammar
  `((bovine-toplevel
     ( package_declaration)
     ( import_declaration)
     ( type_declaration)
     ) ; end bovine-toplevel
    (FLOAT
     ( symbol "[0-9]" punctuation "\\." symbol "[0-9Ee]" punctuation "[-+]" symbol "[0-9fFdD]")
     ( symbol "[0-9]" punctuation "\\." symbol "[0-9EefFdD]")
     ( symbol "[0-9fFdD]")
     ) ; end FLOAT
    (literal
     ( FLOAT)
     ( qualified_name)
     ( string)
     ) ; end literal
    (type
     ( reference_type
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( primitive_type
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end type
    (primitive_type
     ( symbol "\\(boolean\\|byte\\|short\\|int\\|long\\|char\\|float\\|double\\)"
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals))
			  (list start end))))
     ) ; end primitive_type
    (reference_type
     ( array_type
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( qualified_name
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end reference_type
    (array_type
     ( primitive_type dims
		      ,(lambda (vals start end)
			 (append  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))
				  (list start end))))
     ( qualified_name dims
		      ,(lambda (vals start end)
			 (append  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))
				  (list start end))))
     ) ; end array_type
    (qualified_name
     ( symbol punctuation "\\." qualified_name
	      ,(lambda (vals start end)
		 (append  (list ( concat (nth 0 vals) (nth 1 vals) ( car (nth 2 vals))))
			  (list start end))))
     ( symbol
       ,(lambda (vals start end)
	  (append  (list (nth 0 vals))
		   (list start end))))
     ) ; end qualified_name
    (package_declaration
     ( symbol "package" qualified_name punctuation ";"
	      ,(lambda (vals start end)
		 (append  (nth 1 vals) (list 'package nil)
			  (list start end))))
     ) ; end package_declaration
    (import_declaration
     ( symbol "import" qualified_name punctuation ";"
	      ,(lambda (vals start end)
		 (append  (nth 1 vals) (list 'include nil)
			  (list start end))))
     ( symbol "import" qualified_name punctuation "\\." punctuation "*" punctuation ";"
	      ,(lambda (vals start end)
		 (append  (list ( concat ( car (nth 1 vals)) (nth 2 vals) (nth 3 vals)) 'include nil)
			  (list start end))))
     ) ; end import_declaration
    (type_declaration
     ( symbol ";")
     ( class_declaration)
     ( interface_declaration)
     ) ; end type_declaration
    (modifiers_opt
     ( modifiers
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ()
     ) ; end modifiers_opt
    (modifiers
     ( modifier modifiers
		,(lambda (vals start end)
		   (append  ( cons ( car (nth 0 vals)) (nth 1 vals))
			    (list start end))))
     ( modifier
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end modifiers
    (modifier
     ( symbol "\\(public\\|protected\\|private\\|static\\|abstract\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\)")
     ) ; end modifier
    (class_declaration
     ( modifiers_opt symbol "class" qualified_name class_parents class_body
		     ,(lambda (vals start end)
			(append  (nth 2 vals) (list 'type "class" (nth 4 vals) (nth 3 vals) (nth 0 vals) nil)
				 (list start end))))
     ) ; end class_declaration
    (class_parents
     ( super interfaces
	     ,(lambda (vals start end)
		(append  ( append (nth 0 vals) (nth 1 vals))
			 (list start end))))
     ( interfaces super
		  ,(lambda (vals start end)
		     (append  ( append (nth 1 vals) (nth 0 vals))
			      (list start end))))
     ( super
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( interfaces
       ,(lambda (vals start end)
	  (append  ( cons nil (nth 0 vals))
		   (list start end))))
     ()
     ) ; end class_parents
    (super
     ( symbol "extends" qualified_name
	      ,(lambda (vals start end)
		 (append  (nth 1 vals)
			  (list start end))))
     ) ; end super
    (interfaces
     ( symbol "implements" qualified_name_list
	      ,(lambda (vals start end)
		 (append  (nth 1 vals)
			  (list start end))))
     ) ; end interfaces
    (qualified_name_list
     ( qualified_name punctuation "," qualified_name_list
		      ,(lambda (vals start end)
			 (append  ( cons ( car (nth 0 vals)) (nth 2 vals))
				  (list start end))))
     ( qualified_name
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end qualified_name_list
    (class_body
     ( semantic-list
       ,(lambda (vals start end)
	  (append 
	   (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'class_body_declarations)
	   
	   (list start end))))
     ) ; end class_body
    (class_body_declarations
     ( class_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( interface_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( field_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( method_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( constructor_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end class_body_declarations
    (field_declaration
     ( modifiers_opt type variable_declarators punctuation ";"
		     ,(lambda (vals start end)
			(append  (nth 2 vals) (list 'variable) (nth 1 vals) (list nil nil (nth 0 vals) nil)
				 (list start end))))
     ) ; end field_declaration
    (variable_declarators
     ( variable_declarator variable_declarators_opt
			   ,(lambda (vals start end)
			      (append  (nth 0 vals)
				       (list start end))))
     ) ; end variable_declarators
    (variable_declarators_opt
     ( punctuation "," variable_declarators)
     ()
     ) ; end variable_declarators_opt
    (variable_declarator
     ( variable_declarator_id variable_assign_opt
			      ,(lambda (vals start end)
				 (append  (nth 0 vals)
					  (list start end))))
     ) ; end variable_declarator
    (variable_assign_opt
     ( punctuation "=" expression)
     ()
     ) ; end variable_assign_opt
    (variable_declarator_id
     ( symbol dims
	      ,(lambda (vals start end)
		 (append  (list ( concat (nth 0 vals) ( car (nth 1 vals))))
			  (list start end))))
     ( symbol
       ,(lambda (vals start end)
	  (append  (list (nth 0 vals))
		   (list start end))))
     ) ; end variable_declarator_id
    (method_declaration
     ( method_header method_body
		     ,(lambda (vals start end)
			(append  (nth 0 vals)
				 (list start end))))
     ) ; end method_declaration
    (method_header
     ( modifiers_opt method_type symbol formal_parameter_list_opt throws_opt
		     ,(lambda (vals start end)
			(append  (list (nth 2 vals) 'function) (nth 1 vals) (list (nth 3 vals) (nth 0 vals) (nth 4 vals) nil)
				 (list start end))))
     ) ; end method_header
    (method_type
     ( symbol "void"
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals))
			  (list start end))))
     ( type
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end method_type
    (formal_parameter_list_opt
     ( semantic-list
       ,(lambda (vals start end)
	  
	  (semantic-bovinate-from-nonterminal (car (nth 0 vals)) (cdr (nth 0 vals)) 'formal_parameter_list)
	  ))
     ) ; end formal_parameter_list_opt
    (formal_parameter_list
     ( open-paren "(" close-paren ")"
		  ,(lambda (vals start end)
		     (append  (list nil)
			      (list start end))))
     ( open-paren "(" formal_parameter_list
		  ,(lambda (vals start end)
		     (append  (nth 1 vals)
			      (list start end))))
     ( formal_parameter punctuation "," formal_parameter_list
			,(lambda (vals start end)
			   (append  ( cons (nth 0 vals) (nth 2 vals))
				    (list start end))))
     ( formal_parameter close-paren ")"
			,(lambda (vals start end)
			   (append  (list (nth 0 vals))
				    (list start end))))
     ) ; end formal_parameter_list
    (formal_parameter-modifier
     ( symbol "final")
     ()
     ) ; end formal_parameter-modifier
    (formal_parameter
     ( formal_parameter-modifier type variable_declarator_id
				 ,(lambda (vals start end)
				    (append  (list ( car (nth 2 vals)) 'variable ( car (nth 1 vals)) nil nil (nth 0 vals) nil)
					     (list start end))))
     ) ; end formal_parameter
    (throws_opt
     ( throws
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ()
     ) ; end throws_opt
    (throws
     ( symbol "throws" qualified_name_list
	      ,(lambda (vals start end)
		 (append  (nth 1 vals)
			  (list start end))))
     ) ; end throws
    (method_body
     ( punctuation ";"
		   ,(lambda (vals start end)
		      (append  (list nil)
			       (list start end))))
     ( block
	 ,(lambda (vals start end)
	    (append  (list nil)
		     (list start end))))
     ) ; end method_body
    (constructor_declaration
     ( modifiers_opt symbol formal_parameter_list_opt throws_opt constructor_body
		     ,(lambda (vals start end)
			(append  (list (nth 1 vals) 'function nil (nth 2 vals) (nth 0 vals) (nth 3 vals) nil)
				 (list start end))))
     ) ; end constructor_declaration
    (constructor_body
     ( semantic-list
       ,(lambda (vals start end)
	  (append  (list nil)
		   (list start end))))
     ) ; end constructor_body
    (interface_declaration
     ( modifiers_opt symbol "interface" symbol interface_parents interface_body
		     ,(lambda (vals start end)
			(append  (list (nth 2 vals) 'type "interface" (nth 4 vals) (nth 3 vals) (nth 0 vals) nil)
				 (list start end))))
     ) ; end interface_declaration
    (interface_parents
     ( symbol "extends" qualified_name_list
	      ,(lambda (vals start end)
		 (append  (nth 1 vals)
			  (list start end))))
     ()
     ) ; end interface_parents
    (interface_body
     ( semantic-list
       ,(lambda (vals start end)
	  (append 
	   (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'interface_body_declarations)
	   
	   (list start end))))
     ) ; end interface_body
    (interface_body_declarations
     ( class_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( interface_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( method_header punctuation ";"
		     ,(lambda (vals start end)
			(append  (nth 0 vals)
				 (list start end))))
     ( field_declaration
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ) ; end interface_body_declarations
    (array_initializer
     ( semantic-list)
     ) ; end array_initializer
    (block
	( semantic-list)
      ) ; end block
    (primary
     ( array_creation_expression)
     ( primary_no_new_array primary_dim_opt)
     ) ; end primary
    (primary_dim_opt
     ( semantic-list "\\[.*]")
     ()
     ) ; end primary_dim_opt
    (primary_no_new_array
     ( qualified_name semantic-list "\\(.*\\)")
     ( class_instance_creation_expression)
     ( semantic-list "\\(.*\\)")
     ( array_type punctuation "\\." symbol "class")
     ( literal)
     ) ; end primary_no_new_array
    (class_instance_creation_expression
     ( symbol "new" qualified_name semantic-list "\\(.*\\)" semantic-list)
     ( symbol "new" qualified_name semantic-list "\\(.*\\)")
     ) ; end class_instance_creation_expression
    (array_creation_expression
     ( symbol "new" qualified_name dims array_initializer)
     ( symbol "new" qualified_name dims)
     ) ; end array_creation_expression
    (dims_opt
     ( dims
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end dims_opt
    (dims
     ( semantic-list "\\[.*]" dims_opt
		     ,(lambda (vals start end)
			(append  (list ( concat "[]" ( car (nth 1 vals))))
				 (list start end))))
     ) ; end dims
    (field_access
     ( primary punctuation "\\." symbol)
     ( qualified_name)
     ) ; end field_access
    (postfix_expression
     ( primary postfix_operator_opt)
     ) ; end postfix_expression
    (postfix_operator_opt
     ( punctuation "[-+]" punctuation "[-+]")
     ()
     ) ; end postfix_operator_opt
    (unary_expression
     ( punctuation "[-+^!]" unary_expression)
     ( punctuation "[-+]" punctuation "[-+]" unary_expression)
     ( semantic-list "\\(.*\\)" unary_expression)
     ( postfix_expression)
     ) ; end unary_expression
    (operator
     ( punctuation "[-+*/%=<>^~&|!?:.]")
     ( symbol "instanceof")
     ) ; end operator
    (operators
     ( operator operators)
     ( operator)
     ) ; end operators
    (operators_expression_opt
     ( operators expression)
     ()
     ) ; end operators_expression_opt
    (expression
     ( unary_expression operators_expression_opt)
     ) ; end expression
    )
"Grammar used by the semantic library to parse Java source
buffers. This grammar is a Lisp version of the BNF grammar stored in
java.bnf. The Lisp grammar is generated automatically from the BNF
source. For this reason, you should never modify this variable
directly.  See `bovinate' for more information.")


(defun jde-sort-tokens (tokens)
  "Sorts the token list TOKENS depending on `jde-imenu-sort' value."
  (cond ((eq jde-imenu-sort 'asc)
         (sort tokens
               (function
                (lambda (token1 token2)
                  (string-lessp (upcase (semantic-token-name token1))
                                (upcase (semantic-token-name token2)))))))
        ((eq jde-imenu-sort 'desc)
         (sort tokens
               (function
                (lambda (token1 token2)
                  (string-lessp (upcase (semantic-token-name token2))
                                (upcase (semantic-token-name token1)))))))
        (t
         tokens)))

(defun jde-imenu-index-class  (class-token)
  "Creates an imenu index for a class in CLASS-TOKEN."
  (let* ((class-name  (semantic-token-name       class-token))
         (class-type  (semantic-token-type       class-token))
         (class-start (semantic-token-start      class-token))
         (class-parts (semantic-token-type-parts class-token))
         (class-index (jde-imenu-index-class-parts class-parts)))

    (if jde-imenu-include-classdef
        ;; If requested adds a *class def* item to go to the class def.
        (setq class-index (cons (cons "*class def*" class-start)
                                class-index))
      ;; Else adds an *empty* item to go to the class def. only
      ;; when there is not parts
      (or class-index
          (setq class-index
                (list (cons "*empty*"
                            class-start)))))

    (list (cons (format "%s %s" class-type class-name)
                class-index))))

(defun jde-imenu-index-class-parts (tokens)
  "Creates an imenu index for class parts in TOKENS.
When`jde-imenu-include-signature' is non-nil the
index menu displays full method signatures and field types."
  (let ((methods (semantic-find-nonterminal-by-token 'function tokens))
        (fields  (semantic-find-nonterminal-by-token 'variable tokens))
        (classes (semantic-find-nonterminal-by-token 'type     tokens))
        index)

    (setq methods (jde-sort-tokens methods))
    (while methods
      (let* ((method-token (car methods))
             (method-name  (semantic-token-name method-token))
             (method-pos   (semantic-token-start method-token))
             method-sig)
        (if jde-imenu-include-signature
            (let ((method-type  (semantic-token-type method-token))
                  (method-args  (semantic-token-function-args method-token)))
              (setq method-sig (if method-type
                                   (format "%s %s(" method-type method-name)
                                 (format "%s(" method-name)))
              (while method-args
                (let ((method-arg-token (car method-args))
                      method-arg-type)
                  (when method-arg-token
                    (setq method-arg-type (semantic-token-type method-arg-token))
                    (setq method-sig (concat method-sig method-arg-type))))
                (setq method-args (cdr method-args))
                (if method-args (setq method-sig (concat method-sig ","))))
              (setq method-sig (concat method-sig ")")))
          (setq method-sig (format "%s()" method-name)))
        (setq index
              (append
               index (list (cons method-sig method-pos)))))
      (setq methods (cdr methods)))

    ;; Add a separator between method and field index
    (if fields
        (setq index (append index '(("-"))))) 
    
    (setq fields (jde-sort-tokens fields))
    (while fields
      (let* ((field-token (car fields))
             (field-name  (semantic-token-name  field-token))
             (field-pos   (semantic-token-start field-token)))
        (if jde-imenu-include-signature
            (setq field-name (concat (semantic-token-type field-token)
                                     " " field-name)))
        (setq index 
              (append 
               index (list (cons field-name field-pos)))))
      (setq fields (cdr fields)))

    (setq classes (jde-sort-tokens classes))
    (while classes
      (let* ((class-token  (car classes))
             (class-index  (jde-imenu-index-class class-token)))
        (setq index (append index class-index)))
      (setq classes (cdr classes)))
    index))

(defun jde-create-imenu-index ()
  "Creates an imenu index for a Java source buffer.
This function uses the semantic bovinator to index the buffer."

  ;; The following clears the toplevel bovine cache for the current
  ;; buffer and forces *rescan* to work even if the current buffer has
  ;; not been modifified. This is needed to rebuild the "Classes"
  ;; imenu when customization has been changed.
  (semantic-clear-toplevel-cache)
  
  (let* ((tokens   (semantic-bovinate-toplevel))
         (packages (semantic-find-nonterminal-by-token 'package tokens))
         (depends  (semantic-find-nonterminal-by-token 'include tokens))
         (classes  (semantic-find-nonterminal-by-token 'type tokens))
         depend-index
         index)

    (setq classes (jde-sort-tokens classes))
    (while classes
      (let* ((class-token  (car classes))
             (class-index  (jde-imenu-index-class class-token)))
        (setq index (append index class-index)))
      (setq classes (cdr classes)))

    (setq depends (jde-sort-tokens depends))
    (while depends
      (let* ((depend-token (car depends))
             (depend-name  (semantic-token-name  depend-token))
             (depend-pos   (semantic-token-start depend-token)))
        (setq depend-index (append depend-index (list (cons depend-name depend-pos)))))
      (setq depends (cdr depends)))
    (if depend-index
        (setq index (append index (list (cons "imports" depend-index)))))

    (setq packages (jde-sort-tokens packages))
    (while packages
      (let* ((package-token (car packages))
             (package-name  (semantic-token-name  package-token))
             (package-pos   (semantic-token-start package-token)))
        (setq index 
              (append 
               index 
               (list (cons (concat "package " package-name) package-pos)))))
      (setq packages (cdr packages)))
    index))

(add-hook 
 'jde-mode-hook
 (lambda ()
   (make-local-variable 'semantic-toplevel-bovine-table)
   (setq semantic-toplevel-bovine-table jde-parse-bovine-java-grammar)
   (when jde-imenu-enable
     (setq imenu-create-index-function 'jde-create-imenu-index)
     (imenu-add-to-menubar "Classes"))))


(defun jde-get-java-source-buffers ()
  "Get a list of the Java source buffers open in the
current session."
  (mapcan (lambda (buffer)
	  (save-excursion
	    (set-buffer buffer)
	    (if (string= mode-name "JDE")
		(list buffer))))
	  (buffer-list)))

(defun jde-parse-get-package-name ()
  "Gets the name of the package in which the Java source file in the
current buffer resides."
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
		       (match-beginning 1)
		       (match-end 1))))))

(defun jde-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring 
   class-name 0
   (let ((pos  (position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun jde-parse-get-unqualified-name (name)
"Gets the last name in a qualified name." 
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun jde-parse-get-class-at-point ()
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


(defun jde-parse-get-super-class-at-point ()
  (setq superClass "Object")
  (let ((class-re "extends[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (setq superClass (buffer-substring-no-properties
		     (match-beginning 1)
		     (match-end 1))))))))
  superClass
 )

(defun jde-parse-in-comment-p () 
  
)

(defun jde-parse-get-innermost-class-at-point ()
"Get the innermost class containing point.
If point is in a class, this function returns 
(CLASS_NAME . CLASS_POSITION), where CLASS_NAME is the 
name of the class and CLASS_POSITION is the position
of the first character of the class keyword. Otherwise,
this function returns nil."
  ;; (interactive)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (if (looking-at "{")
			(let* ((search-end-pos
			       (if (< left-paren-index (1- left-paren-count))
				   (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				     (if (consp pos)
					 (cdr pos)
				       pos))
				 (point-min)))
                              (case-fold-search nil)
                              (class-re "^[ \t]*\\(\\(public\\|abstract\\|final\\|static\\|strictfp\\|protected\\)[ \t]+\\)*[ \t]*class[ \t]+\\([^ \t\n{]*\\).*")
                              (class-pos (re-search-backward class-re search-end-pos t)))      
                           (if class-pos
			      (progn
				(looking-at class-re)
				(throw
				 'class-found
				 (cons
				  (buffer-substring-no-properties
				   (match-beginning 3)
				   (match-end 3))
				  class-pos))))))))
		  (setq left-paren-index (1+ left-paren-index)))))))))

(defun jde-parse-test ()
  (interactive)
  (message (car (jde-parse-get-innermost-class-at-point))))
      
 

(defun jde-parse-get-class-at-point () 
  (let ((class-info (jde-parse-get-innermost-class-at-point))
	class-name)
    (while class-info
      (let ((name (car class-info))
	    (pos (cdr class-info)))
	(if (not class-name)
	    (setq class-name name)
	  (setq class-name (concat name "." class-name)))
	(save-excursion
	  (goto-char pos)
	  (setq class-info (jde-parse-get-innermost-class-at-point)))))
    class-name)) 


(defun jde-parse-get-classes-at-point ()
  (interactive)
  (let ((class (jde-parse-get-innermost-class-at-point)))
    (if class (message "%s %s" (car class) (cdr class) ) (message "no class")))
;; (goto-char (aref (c-search-uplist-for-classkey (c-parse-state)) 0))
)


(defun jde-parse-qualified-name-at-point ()
  "Returns (cons QUALIFIER NAME) where NAME is the symbol at point and
QUALIFIER is the symbol's qualifier. For example, suppose the name at
point is

     int i = error.msg.length()
                   ^
In this case, this function returns (cons \"error.msg\" \"length\").
This function works only for qualified names that do not contain
white space. It returns null if there is no qualified name at point."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (thing-at-point-looking-at "[^ \n\t();,:+]+")
      (let ((qualified-name 
	      (buffer-substring-no-properties
	       (match-beginning 0)
	       (match-end 0))))
	(string-match "\\(.+[.]\\)*\\([^.]+\\)" qualified-name)
	(let ((qualifier (if (match-beginning 1)
			     (substring qualified-name 
					(match-beginning 1) (match-end 1))))
	      (name (substring qualified-name 
				    (match-beginning 2) (match-end 2))))
	  (if qualifier
	      (setq qualifier (substring qualifier 0 (1- (length qualifier)))))
	  (cons qualifier name))))))


(defun jde-parse-double-backslashes (name)
  (mapconcat (lambda (x) (if (eq x ?\\)
		 "\\\\"
	       (string x)))
	     name ""))

(defun jde-parse-valid-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
			    (jde-parse-double-backslashes varname) 
			    "[ \t\n\r]*[;=]"))
	(match-string 1) 
      nil)))

(defun jde-parse-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (setq orgpt (point))
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
      
      (goto-char orgpt)

      (while (and (not found)
		  (search-forward name nil t))
	(setq pos (point))
	(backward-word 2)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(goto-char pos)
	(forward-char 1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
      res)))


(defun jde-display-parse-error (error)
  (let* ((parser-buffer-name "*Java Parser*")
	 (buf (get-buffer parser-buffer-name))) 
    (if (not buf)
	(setq buf (get-buffer-create parser-buffer-name)))
    (set-buffer buf)
    (erase-buffer)
    (insert error)
    (pop-to-buffer buf)))

(defun jde-parse ()
"*Parses the Java source file displayed in the current buffer.
If the source file parses successfully, this command displays
a success message in the minibuffer. Otherwise, it displays an error
message in the Java Parser buffer. If the Java Parser buffer does
not exist, this command creates it.

Note. This command uses an external Java parser implemented in
Java to parse Java source files. This command uses the JDE's integrated
Java source interpreter, the BeanShell, to invoke the parser. If the
BeanShell is not running, this command starts the BeanShell. Thus,
the first time you invoke the parser you may notice a slight delay
before getting a response. Thereafter, the response should be very
fast."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((parse-error
	 (bsh-eval-r (concat "jde.parser.ParserMain.parseFile(\"" (buffer-file-name) "\");"))))
    (if parse-error
	(jde-display-parse-error parse-error)
      (message "Parsed %s successfully" (buffer-name)))))


;; Thanks to Eric D. Friedman <friedman@lmi.net> for this function.
(defun jde-parse-comment-or-quoted-p ()
  "Returns t if point is in a comment or a quoted string. nil otherwise"
  (interactive "p")
  ;; limit our analysis to the current line.
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (if
        (or
         ;; are we in a javadoc comment?
         (save-excursion
           (re-search-backward
            "^[ \t]*/?\\*"
            beg t))
         ;; are we in a '//' or a '/*' style comment?
         ;; note that /* or /** on a line with only leading whitespace
         ;; will have matched in the previous regex.  We check again here
         ;; because the above case needs to allow for a line with
         ;; the continuation of a comment (using only a '*') whereas this
         ;; requires the presence of a '/' in front of the '*' so as to
         ;; distinguish a comment from a '*' operator.
         ;; To make a long story short,
         ;; the first regex matches
         ;;   /* a comment */
         ;; and
         ;; /**
         ;;  * a comment
         ;;  */
         ;; while the second one matches
         ;; System.out.println(foo); /* a comment */
         ;; but not
         ;; i = 3 * 5;
         ;; if you do something like following, you're on your own:
         ;; i = 3
         ;;       * 5; 
         ;; Suggestions for improving the robustness of this algorithm
         ;; gratefully accepted.
         (save-excursion
           (re-search-backward
            "\\(//\\|/\\*\\)"
            beg t))
         ;; are we in a quoted string?
         (save-excursion
           (re-search-backward
            "\"" ;;
            beg t)))
        t ;; return true if we had any matches; nil otherwise
      nil)))

(provide 'jde-parse)

;; $Log: jde-parse.el,v $
;; Revision 1.17  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.16  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.15  2000/06/29 02:33:42  paulk
;; Added sort option to Classes index menu. Thanks to David Ponce for this contribution.
;;
;; Revision 1.14  2000/06/22 03:40:16  paulk
;; Index menu now shows variable types and class definitions. Thanks to David Ponce for these enhancments. Changed the name of jde-enable-index-menu to jde-imenu-enable and jde-enable-full-method-signatures-index-menu to jde-imenu-include signature.
;;
;; Revision 1.13  2000/06/09 05:07:06  paulk
;; Classes index menu now shows full signatures of methods. Thanks to Ittay Freiman <ittay@vigiltech.com> for suggesting this enhancement and to David Ponce <david@dponce.com> for implementing it.
;;
;; Revision 1.12  2000/05/26 09:14:10  paulk
;; Updated grammar to handle argument variables with modifiers and array arguments.
;;
;; Revision 1.11  2000/05/16 04:08:55  paulk
;; Adds a Classes index menu to the Emacs menubar.
;;
;; Revision 1.10  2000/05/11 03:07:17  paulk
;; Updated bovinator grammar.
;;
;; Revision 1.9  2000/05/11 01:24:40  paulk
;; Added support for Eric Ludlam's semantic bovinator. Moved regular expression-based imenu indexer to this file.
;;
;; Revision 1.8  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.7  2000/03/08 03:47:02  paulk
;; Fixed regular expression in jde-parse-get-innermost-class-at-point to handle more cases. Thanks to Steve Haflich <smh@franz.com> for reporting the problem and providing a starting point for the fix.
;;
;; Revision 1.6  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.5  2000/02/14 06:21:32  paulk
;; Fixed find innermost class regular expression.
;;
;; Revision 1.4  2000/02/09 05:18:10  paulk
;; Added methods for parsing symbol at point.
;;
;; Revision 1.3  2000/02/01 04:10:48  paulk
;; Fixed regular expression for classes to handle case where point is in
;; a constructor. Thanks to Francois Cogne <cogne@col.bsf.alcatel.fr>.
;;
;; Revision 1.2  1999/08/20 00:52:14  paulk
;; Added jde-parse-get-package-from-name function.
;;
;; Revision 1.1  1999/04/27 16:25:46  paulk
;; Initial revision
;;