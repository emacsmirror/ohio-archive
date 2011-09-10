;;; sccsinfo: @(#)xmr-mode.el	2.3 4/2/92 isy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:     xmr-mode.el
;;; Author:   Ik Su Yoo <ik@ctt.bellcore.com>
;;; Date:     11/03/91
;;; Contents: Mode definition for editing X/Motif/WCL resource files.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (C) 1992, 1991 Ik Su Yoo
;;;
;;; This file currently is not part of the GNU Emacs distribution.
;;;
;;; This file is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY. No author or distributor accepts responsibility
;;; to anyone for the consequences of using it or for whether it serves any
;;; particular purpose or works at all, unless he says so in writing.
;;; Refer to the GNU Emacs General Public License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute this
;;; file, but only under the conditions described in the GNU Emacs General
;;; Public License. A copy of this license is supposed to have been given
;;; to you along with GNU Emacs so you can know your rights and
;;; responsibilities. It should be in a file named COPYING. Among other
;;; things, the copyright notice and this notice must be preserved on all
;;; copies.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History:
;;;
;;;   11/20/91
;;;     - full name completion extended to regular Motif resource files
;;;     - bug fixed so that non-unique completion always gets reported
;;;     - bug fixed which didn't do full completion when the resource
;;;       name matched exactly
;;;   11/21/91
;;;     - bug fixed so that when TAB is pressed on a blank line, a line
;;;       containing comment, or a line that is the continuation of the
;;;       previous line, it does indentation rather than completion
;;;     - bug fixed so that the relative completion work when a line is
;;;       the continuation of the previous line
;;;     - added complete resource names as found in Xm.h
;;;   11/25/91
;;;     - added a function to go to a newline and automatically insert the
;;;       full object name
;;;   12/03/91
;;;     - added a function to display the widget hierarchy
;;;     - defined an abbrev table of widget class names
;;;   12/10/91
;;;     - added abbrev definitions for Motif constructors
;;;     - added commands to jump to next/previous object
;;;   12/12/91
;;;     - modified to assume that each object's specification is delimited
;;;       by blank lines at the beginning and the end
;;;     - as a consequence, XRM-PARSE-CONTEXT has been changed;
;;;       XMR-FULL-COMPLETION-RELATIVE-P no longer applies
;;;     - fixed a bug which assumed that there is no white space to the
;;;       left of the delimiting character `:'
;;;   12/17/91
;;;     - modified FIND-TAG feature so that it would look up the correct
;;;       callback function name from a file when figuring out the default
;;;       tag
;;;   01/18/92
;;;     - integrated with a manual browser
;;;     - added context sensitive resource name completion
;;;     - fixed a bug which didn't do name completion when the name was empty
;;;     - fixed xmr-run-program to work with Mri distributed with WCL 1.06
;;;     - enhanced xmr-display-widget-hierarchy indentation
;;;   01/31/92
;;;     - fixed a bug which inserted partially matched resource name into
;;;       the kill ring
;;;   03/20/92
;;;     - defined XMR mode Dmacros
;;;     - added a function to indent the current resource set or a region
;;;       containing resource sets
;;;     - the display name is made optional
;;;     - fixed xmr-uncomment-region to remove only the first comment
;;;       character
;;;     - added a function to rename the current object
;;;   03/21/92
;;;     - added a function to search forward/backward objects by name
;;;     - resource completion extended to include `.wc*' resources and
;;;       constraint resources
;;;   03/29/92
;;;     - fixed a bug in the hierarchy traversal mechanism which didn't
;;;       follow the `wcPopups' links
;;;     - added functions to traverse (up, down, left, or right) the widget
;;;       hierarchy
;;;     - cleaned up comments
;;;
;;; Bugs/limitations:
;;;
;;;   - Space after the value of wcClass, wcClassName, or wcConstructor is
;;;     not handled.
;;;
;;;   - The widget hierarchy traversal code is a bit slow.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; XMR mode is an emacs extension that is useful for editing X/Motif/WCL
;;; resource files. Basically, it helps you to edit large resource files
;;; which can become tedious and repetitive. It offers the following
;;; features:
;;;
;;;   - Dmacros and name completion to simplify resource specification.
;;;
;;;   - On-line assistance and on-line help during resource specification.
;;;
;;;   - Automatic indentation of the resource specification.
;;;
;;;   - Navigational aid to traverse the widget hierarchy.
;;;
;;;   - Visually displaying the widget hierarchy in another buffer.
;;;
;;;   - Limited run-time integration.
;;;
;;;   - Commenting/uncommenting regions.
;;;
;;; Consult the README file for more details on these features.
;;;
;;; To autoload, add the following lines to your .emacs file:
;;;
;;;   (setq auto-mode-alist (cons '("\\.ad$" . xmr-mode) auto-mode-alist))
;;;   (autoload 'xmr-mode "xmr-mode")
;;;
;;; The following commands are available:
;;;
;;;   xmr-mode				
;;;
;;;     Selects the current major mode to be XMR mode.
;;;
;;;   xmr-complete-resource-name
;;;
;;;     Completes the (partial) resource name that is typed before the point.
;;;     Beeps when no completion is possible; displays the possibilities
;;;     when more than 1 completions are possible.
;;;
;;;   xmr-display-resource-name-completions	Ctrl-c-h
;;;
;;;     Displays the possible resource name completions in a temporary buffer.
;;;
;;;   xmr-display-resource-name-completions*	Ctrl-c-Ctrl-h
;;;
;;;   xmr-indent-or-complete			TAB
;;;
;;;     If the point is on or to the left of `:', it does name completion.
;;;     Otherwise, it does relative tabbing.
;;;
;;;   xmr-indent-or-complete			Meta-TAB
;;;
;;;   xmr-yank-previous-object-name		Meta-Ctrl-y
;;;
;;;     Yanks the previous object name found in the buffer and inserts it
;;;     at the current point. Consecutive calls to it will search further
;;;     back in the buffer.
;;;
;;;   xmr-newline-relative			Ctrl-j
;;;
;;;     Inserts a newline and automatically inserts the previous line's
;;;     full object name.
;;;
;;;   xmr-next-object				Meta-n
;;;
;;;     Moves the point to the beginning of the next object.
;;;
;;;   xmr-previous-object			Meta-p
;;;
;;;     Moves the point to the beginning of the previous object.
;;;
;;;   xmr-goto-parent				Ctrl-c-<
;;;
;;;     Moves the point to the beginning of the current object's parent.
;;;
;;;   xmr-goto-child				Ctrl-c->
;;;
;;;     Moves the point to the beginning of the current object's Nth child,
;;;     depending on the numeric argument.
;;;
;;;   xmr-goto-next-sibling			Ctrl-c-n
;;;
;;;     Moves the point to the beginning of the current object's right sibling.
;;;
;;;   xmr-goto-previous-sibling			Ctrl-c-p
;;;
;;;     Moves the point to the beginning of the current object's left sibling.
;;;
;;;   xmr-search-object-forward			Ctrl-c-s
;;;
;;;     Searches forward for the object of the given name.
;;;
;;;   xmr-search-object-backward		Ctrl-c-r
;;;
;;;     Searches backward for the object of the given name.
;;;
;;;   xmr-display-widget-hierarchy		Ctrl-c-t
;;;
;;;     Displays the widget hierarchy that can be parsed from the current
;;;     point in a temporary buffer.
;;;
;;;   xmr-comment-region			Ctrl-c-c
;;;
;;;     Inserts a comment character (`!') to the beginning of every line
;;;     contained in the region.
;;;
;;;   xmr-uncomment-region			Ctrl-c-u
;;;
;;;     Removes the comment characters from the beginning of every line
;;;     contained in the region.
;;;
;;;   xmr-run-program				Ctrl-c-Ctrl-r
;;;
;;;     Runs the program which makes use of the resource file.
;;;
;;;   xmr-man					Ctrl-c-m
;;;
;;;     Display the man page for a Motif widget.
;;;
;;;   xmr-rename-object				Ctrl-c-s
;;;
;;;     Renames the current object.
;;;
;;;   xmr-indent-region				Meta-Ctrl-\
;;;
;;;     Formats the resource specification contained in the given region.
;;;
;;;   xmr-fill-paragraph			Meta-q
;;;
;;;     Formats the current object's resource specification.
;;;
;;; You may set the following variables according to your environment:
;;;
;;;   xmr-wcl-p
;;;
;;;     Set this to T (the default) if you are editing WCL resource files,
;;;     NIL if editing regular Motif resource files. If you edit both WCL
;;;     resource files and regular Motif resource files, setting this to T
;;;     globally won't effect the way the completion is done for the Motif
;;;     resource files.
;;;
;;;   xmr-full-completion-p
;;;
;;;     Set this to T (the default), if you want a full name completion.
;;;     What this means is that when you simply have a (partial) resource
;;;     name on a line by itself, the name completion process may figure
;;;     out the object name you intended and insert it for you
;;;     automatically, while at the same time doing the name completion.
;;;     For example:
;;;
;;;       *main.wcClass:	xmMainWindowWidgetClass
;;;       wi
;;;
;;;     When you type TAB after the "wi" above, it will result in the
;;;     following:
;;;
;;;       *main.wcClass:	xmMainWindowWidgetClass
;;;       *main.width
;;;
;;;   xmr-complete-resource-name-display-p
;;;
;;;     Set this to T, if you want to display the possible completions in a
;;;     temporary buffer. Set it to NIL (the default), if you simply want
;;;     to be told that there are multiple completions.
;;;
;;;   xmr-program-name
;;;
;;;     Set this to the name of the program to run using the XMR-RUN-PROGRAM
;;;     command. It defaults to "Mri".
;;;
;;;   xmr-display-name
;;;
;;;     Set this to the name of the display where your X server is running.
;;;     It defaults to NIL.
;;;
;;;   xmr-tree-indent
;;;
;;;     Set this to the string used to recursively indent when
;;;     displaying the widget hierarchy. The default is "| ".
;;;
;;;   xmr-rhs-column
;;;
;;;     Set this to the column where the resource values should be aligned
;;;     when indenting a region. The default is 40.
;;;
;;; The customization for each resource file is conveniently done by
;;; putting something like the following lines near the bottom of the file:
;;; 
;;;   !
;;;   ! Local Variables:
;;;   ! mode: xmr
;;;   ! comment-column: 0
;;;   ! comment-start: "!"
;;;   ! xmr-wcl-p: t
;;;   ! xmr-full-completion-p: t
;;;   ! xmr-complete-resource-name-display-p: nil
;;;   ! xmr-program-name: "/user/spock/xvulcan"
;;;   ! xmr-display-name: "enterprise:0"
;;;   ! xmr-tree-indent: "  "
;;;   ! xmr-rhs-column: 60
;;;   ! End:
;;;   !
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helping functions and macros
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro skip-forward-until-regexp (regexp)
  (list 'while
	(list 'and
	      (list 'not (list 'eobp))
	      (list 'not (list 'looking-at regexp)))
	(list 'next-line 1)))

(defmacro skip-forward-while-regexp (regexp)
  (list 'while
	(list 'and
	      (list 'not (list 'eobp))
	      (list 'looking-at regexp))
	(list 'next-line 1)))

(defmacro skip-backward-until-regexp (regexp)
  (list 'while
	(list 'and
	      (list 'not (list 'bobp))
	      (list 'not (list 'looking-at regexp)))
	(list 'previous-line 1)))

(defmacro skip-backward-while-regexp (regexp)
  (list 'while
	(list 'and
	      (list 'not (list 'bobp))
	      (list 'looking-at regexp))
	(list 'previous-line 1)))

;;;
;;; True iff CH is an alphabet character.
;;;
(defmacro alphabet-p (ch)
  (list 'and
	(list '>= (list 'downcase ch) ?a)
	(list '<= (list 'downcase ch) ?z)))

;;;
;;; Like MEMQ, but it works for strings.
;;;
(defun string-member (string string-list)
  (cond ((null string-list)
	 nil)
	((string= string (car string-list))
	 string-list)
	(t
	 (string-member string (cdr string-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; internal functions and variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xmr-comment-prefix "!")
(defvar xmr-delimiter ":")
(defvar xmr-continue "\\")
(defvar xmr-name-delimiter ".")
(defvar xmr-name-delimiter* "*")
(defvar xmr-list-delimiters ", \t\n\\")

(defvar xmr-wcclass-regexp "wcclass\\|wcconstructor\\|wcclassname")
(defvar xmr-wcchildren-regexp "wcChildren")
(defvar xmr-wcpopups-regexp "wcPopups")
(defvar xmr-class-name-regexp "[a-zA-Z]+")
(defvar xmr-object-name-regexp "[a-zA-Z0-9_]+")
(defvar xmr-full-object-name-regexp "^[a-zA-Z0-9\*\._]+")
(defvar xmr-resource-name-regexp "[a-zA-Z]+")
(defvar xmr-name-delimiter-regexp "[\*\.]")

(defvar xmr-blank-line-regexp "^[ \t]*$")

;;;
;;; Context information and the stack used to save and restore them.
;;;
(defvar xmr-context-stack nil)
(defvar xmr-current-class-name nil)
(defvar xmr-current-object-name nil)
(defvar xmr-parent-class-name nil)
(defvar xmr-parent-object-name nil)
(defvar xmr-children-names nil)

(defvar xmr-lhs-regexp (concat xmr-full-object-name-regexp
			       xmr-name-delimiter-regexp
			       xmr-resource-name-regexp))

(defvar xmr-resource-name-closure nil
  "Controls the scope of the resource completion to include all resources or
only non-inherited resources.")

;;;
;;; Returns the largest suffix of name not containing a name delimiter.
;;;
(defun xmr-clean-name (name)
  (if (string-match (format ".*%s" xmr-name-delimiter-regexp) name)
      (substring name (match-end 0))
    name))

;;;
;;; This function takes a constructor name, class name, or a class pointer
;;; name and returns the corresponding class name.
;;;
(defun xmr-convert-class-name (thing)
  (let (class-symbol
	pos)
    (cond ((string-match "^XmCreate" thing)
	   ;;
	   ;; constructor: XmCreate???
	   ;;
	   (setq class-symbol (intern (concat "Xm" (substring thing 8)))))
	  ((setq pos (string-match "WidgetClass" thing))
	   ;;
	   ;; class pointer: xm???WidgetClass
	   ;;
	   (setq class-symbol
		 (intern (concat (char-to-string (upcase (string-to-char thing)))
				 (substring thing 1 pos)))))
	  ((setq pos (string-match "GadgetClass" thing))
	   (setq class-symbol
		 (intern (concat (char-to-string (upcase (string-to-char thing)))
				 (substring thing 1 (+ pos (length "Gadget")))))))
	  (t
	   ;;
	   ;; class: Xm???
	   ;;
	   (setq class-symbol (intern thing))))
    (and (setq class-symbol (car (memq class-symbol xmr-class-symbols)))
	 (symbol-name class-symbol))))

;;;
;;; This function returns the class pointer name associated with the
;;; class name.
;;;
(defun xmr-class-pointer (name)
  (concat (char-to-string (downcase (string-to-char name)))
	  (substring name 1)
	  "WidgetClass"))

;;;
;;; This function returns the constructor name associated with the class
;;; name.
;;;
(defun xmr-class-constructor (name)
  (if (char-equal (string-to-char name) ?X)
      (concat "XmCreate" (substring name 2))))

;;;
;;; This function returns the man page name associated with the class name.
;;;
(defun xmr-class-manpage (name)
  (get (and name (intern name)) 'manpage))

;;;
;;; This function returns the list of resource names, excluding inherited
;;; resources, associated with the class name.
;;;
(defun xmr-class-resources (name)
  (let ((class-symbol (and name (intern name))))
    (or (get class-symbol 'resources)
	(get (get class-symbol 'resources-shadow) 'resources))))

;;;
;;; This function returns the list of all resource names associated with the
;;; class name.
;;;
(defun xmr-class-resources* (name)
  (let ((class-symbol (and name (intern name))))
    (or (get class-symbol 'resources*)
	(get (get class-symbol 'resources-shadow) 'resources*))))

;;;
;;; This function returns the list of all constraint resource names
;;; associated with the class name.
;;;
(defun xmr-class-resources-constraint (name)
  (let ((class-symbol (and name (intern name))))
    (get class-symbol 'resources-constraint)))

;;;
;;; This function returns T iff the point is currently on or to the left of
;;; the resource name/value delimiter.
;;;
(defun xmr-left-of-delimiter-p ()
  (save-excursion
    (save-restriction
      (let ((end (point)))
	(beginning-of-line)
	(narrow-to-region (point) end)
	(not (search-forward xmr-delimiter nil t nil))))))

;;;
;;; This function returns T iff the current line is the continuation of the
;;; previous line.
;;;
(defun xmr-line-continued-p ()
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	nil
      (let ((limit (point)))
	(backward-char 2)
	(search-forward xmr-continue limit t)))))

;;;
;;; This function returns T iff the current line "needs" name completion.
;;;
(defun xmr-need-completion-p ()
  (and (save-excursion
	 (beginning-of-line)
	 (and (not (looking-at "^[ \t]*$"))
	      (not (looking-at xmr-comment-prefix))))
       (xmr-left-of-delimiter-p)
       (not (xmr-line-continued-p))))

;;;
;;; This function returns T iff the current line contains an object name.
;;;
(defun xmr-line-contains-object-name-p ()
  (save-excursion
    (save-restriction
      (let ((end (point)))
	(beginning-of-line)
	(narrow-to-region (point) end)
	(re-search-forward xmr-name-delimiter-regexp nil t nil)))))

;;;
;;; This function returns the completion alist to be used in completing
;;; resource names, depending on the current context. The context
;;; determines which resource names are available.
;;;
(defun xmr-get-completion-alist ()
  (append xmr-resources-permanent
	  (if xmr-resource-name-closure
	      (xmr-class-resources* xmr-current-class-name)
	    (xmr-class-resources xmr-current-class-name))
	  (xmr-class-resources-constraint xmr-parent-class-name)))

;;;
;;; This function saves the current context into the stack.
;;;
(defun xmr-push-context ()
  (setq xmr-context-stack
	(cons (list xmr-current-class-name
		    xmr-current-object-name
		    xmr-children-names)
	      xmr-context-stack)))

;;;
;;; This function restores the most recently saved context from the stack.
;;;
(defun xmr-pop-context ()
  (let ((context (car xmr-context-stack)))
    (setq xmr-context-stack (cdr xmr-context-stack))
    (setq xmr-current-class-name (nth 0 context))
    (setq xmr-current-object-name (nth 1 context))
    (setq xmr-children-names (nth 2 context))))

;;;
;;; Parse for the object name.
;;;
(defun xmr-parse-object-name ()
  (goto-char (point-min))
  (if (not (re-search-forward (concat xmr-lhs-regexp
				      "[ \t]*"
				      xmr-delimiter)
			      nil t nil))
      ()
    (backward-word 1)
    (backward-char 1)
    (set-mark (point))
    (beginning-of-line)
    (buffer-substring (point) (mark))))

;;;
;;; Parse for the class name.
;;;
(defun xmr-parse-object-class-name ()
  (goto-char (point-min))
  (if (not (re-search-forward xmr-wcclass-regexp nil t nil))
      ()
    (end-of-line)
    (set-mark (point))
    (backward-word 1)
    (if (looking-at xmr-class-name-regexp)
	(xmr-convert-class-name (buffer-substring (point) (mark))))))

(defun xmr-parse-parent-name ()
  )

(defun xmr-parse-parent-class-name ()
  )

;;;
;;; Parse for the children names.
;;;
(defun xmr-parse-children-names ()
  (goto-char (point-min))
  (cdr (xmr-parse-widget-hierarchy nil)))

;;;
;;; This function parses the current context that includes the following
;;; information:
;;;
;;;   - object name
;;;   - object class name
;;;   - parent name
;;;   - parent class name
;;;   - children names
;;;
;;; If FULL-P is NIL, only the first two are looked for. Otherwise, all of
;;; the above are looked for.
;;;
(defun xmr-parse-context (&optional full-p)
  (save-excursion
    (xmr-narrow-to-object)
    (setq xmr-current-class-name (xmr-parse-object-class-name))
    (setq xmr-current-object-name (xmr-parse-object-name))
    (if full-p
	(setq xmr-children-names (xmr-parse-children-names)))
    ;;
    ;; In order to parse the parent's information, we need to get out of
    ;; the narrowed region.
    ;;
    (widen)
    (let ((object-name (and xmr-current-object-name
			    (xmr-clean-name xmr-current-object-name)))
	  pclass-name
	  children-names
	  found-p)
      (if (null object-name)
	  ()
	(while (and (not found-p)
		    (re-search-backward (format "%s\\|%s"
						xmr-wcchildren-regexp
						xmr-wcpopups-regexp)
					nil t nil))
	  (save-excursion
	    (save-restriction
	      (xmr-narrow-to-object)
	      (setq children-names (xmr-parse-children-names))
	      (if (not (string-member object-name children-names))
		  ()
		(setq found-p t)
		(setq xmr-parent-class-name (xmr-parse-object-class-name))
		(setq xmr-parent-object-name (xmr-parse-object-name))))))))))

;;;
;;; This function takes a (partial) resource name and displays the possible
;;; completions in a temporary buffer.
;;;
(defun xmr-list-completions-in-buffer (name)
  (with-output-to-temp-buffer "*XMR Completion*"
      (display-completion-list (all-completions name
						(xmr-get-completion-alist)))))

;;;
;;; This function narrows the buffer to contain only the current object
;;; specification. The object specification is assumed to be delimited at
;;; it beginning and end by at least 1 blank line.
;;;
(defun xmr-narrow-to-object ()
  (let ((point-save (point))
	point-begin
	point-end)
    (beginning-of-line)
    (skip-backward-until-regexp xmr-blank-line-regexp)
    (if (looking-at xmr-blank-line-regexp)
	(next-line 1))
    (setq point-begin (point))
    (goto-char point-save)
    (skip-forward-until-regexp xmr-blank-line-regexp)
    (setq point-end (point))
    (narrow-to-region point-begin point-end)
    (goto-char point-save)))

;;;
;;; This function narrows the buffer to contain only the current word
;;; before the point.
;;;
(defun xmr-narrow-to-word ()
  (let ((point-save (point)))
    ;;
    ;; Narrow the region to the current line so that we don't back over
    ;; to the previous line.
    ;;
    (beginning-of-line)
    (save-restriction
      (narrow-to-region (point) point-save)
      (goto-char point-save)
      (while (and (not (bobp))
		  (alphabet-p (preceding-char)))
	(backward-char 1)))
    ;;
    ;; Narrow the region to the current word to be completed.
    ;;
    (narrow-to-region (point) point-save)))

;;;
;;; This function calls an auxiliary function to display the given tree
;;; specification to a temporary buffer.
;;;
(defun xmr-list-tree-in-buffer (tree)
  (with-output-to-temp-buffer "*XMR Widget Hierarchy*"
    (xmr-list-tree-in-buffer-aux tree 0)))

;;;
;;; This function recursively prints the given tree specification. It first
;;; prints the root of TREE is printed indented. Then, the children
;;; subtrees are recursively printed with the indentation incremented
;;; appropriately.
;;;
(defun xmr-list-tree-in-buffer-aux (tree indent)
  (cond ((null tree))
	((not (listp tree))
	 ;;
	 ;; The TREE is a leaf node.
	 ;;
	 (while (> indent 0)
	   (princ xmr-tree-indent)
	   (setq indent (1- indent)))
	 (princ tree)
	 (terpri))
	(t
	 ;;
	 ;; Print the root followed by the subtrees.
	 ;;
	 (xmr-list-tree-in-buffer-aux (car tree) indent)
	 (setq indent (1+ indent))
	 (mapcar '(lambda (subtree)
		    (xmr-list-tree-in-buffer-aux subtree indent))
		 (cdr tree)))))

;;;
;;; This function parses the widget hierarchy specification in a WCL file,
;;; starting at the current point, and returns the hierarchy. It follows
;;; the `wcChildren' and `wcPopups' links and collects all the names
;;; contained in the process. The tree specification returned has the
;;; following syntax: 
;;;
;;;   - If it is a string, it is a leaf node.
;;;   - If it is a list, its head is the root and the tail is a list of the
;;;     children subtree specifications.
;;;
;;; For example, the following shows a tree and its corresponding tree
;;; specification:
;;;
;;;     A
;;;    / \
;;;   B  C    ->  (A B (C D E))
;;;     / \
;;;    D   E
;;;
;;; If RECURSIVE-P is non-NIL, the entire widget hierarchy is collected.
;;; Otherwise, only the current node and its children are collected.
;;;
(defun xmr-parse-widget-hierarchy (&optional recursive-p)
  (save-excursion
    (let ((object-name (save-restriction
			 (xmr-narrow-to-object)
			 (xmr-parse-object-name)))
	  subtree)
      (if (null object-name)
	  ()
	;;
	;; Start from the beginning of the object specification.
	;;
	(skip-backward-until-regexp xmr-blank-line-regexp)
	(setq subtree (xmr-parse-widget-hierarchy-aux object-name recursive-p))
	(if (listp subtree)
	    subtree
	  (list subtree))))))
  
(defun xmr-parse-widget-hierarchy-aux (object-name recursive-p)
  ;;
  ;; Need to scan twice: once to follow the `wcChildren' link and
  ;; another to follow the `wcPopups' link.
  ;;
  (let ((children-names (xmr-parse-widget-children
			 (concat object-name
				 xmr-name-delimiter-regexp
				 xmr-wcchildren-regexp)))
	(popup-names (xmr-parse-widget-children
		      (concat object-name
			      xmr-name-delimiter-regexp
			      xmr-wcpopups-regexp))))
    (setq children-names (append children-names popup-names))
    (cond ((null children-names)
	   object-name)
	  ((not recursive-p)
	   (cons object-name children-names))
	  (t
	   (cons object-name
		 (mapcar '(lambda (name)
			    (xmr-parse-widget-hierarchy-aux name recursive-p))
			 children-names))))))

(defun xmr-parse-widget-children (regexp)
  (save-excursion
    (if (not (re-search-forward regexp nil t nil))
	()
      (let (start
	    end)
	(save-restriction
	  ;;
	  ;; Narrow the working region to all lines that constitute the
	  ;; current children entry.
	  ;;
	  (beginning-of-line)
	  (setq start (point))
	  (end-of-line)
	  (backward-char 1)
	  (while (and (not (eobp))
		      (looking-at (regexp-quote xmr-continue)))
	    (next-line 1)
	    (end-of-line)
	    (backward-char))
	  (end-of-line)
	  (setq end (point))
	  (narrow-to-region start end)
	  (beginning-of-buffer)
	  ;;
	  ;; Start from the right of the "wcChildren" (or "wcPopups") entry and
	  ;; collect all the child names.
	  ;;
	  (re-search-forward (regexp-quote xmr-delimiter))
	  (xmr-parse-object-names (point) end))))))

;;;
;;; This function returns the object names contained in the given region as
;;; a list of strings.
;;;
(defun xmr-parse-object-names (start end)
  (save-excursion
    (if (>= start end)
	()
      (goto-char start)
      (re-search-forward (concat "[" xmr-list-delimiters "]*") end)
      (set-mark (point))
      (re-search-forward (concat "[^" xmr-list-delimiters "]+") end t nil)
      (if (= (mark) (point))
	  ()
	(cons (buffer-substring (mark) (point))
	      (xmr-parse-object-names (point) end))))))

;;;
;;; This function prompts the user for a class name, which defaults to the
;;; current object's class name, and returns it in a list.
;;;
(defun xmr-read-class-name (prompt)
  (let* ((class-name (progn
		       (xmr-parse-context)
		       xmr-current-class-name))
	 (thing (read-string (if class-name
				 (format "%s(default %s) " prompt class-name)
			       prompt))))
    (if (not (equal thing ""))
	(setq class-name (or (xmr-convert-class-name thing) thing)))
    (list class-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; external (interactive) functions and variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xmr-wcl-p t
  "T if editing WCL resource file; NIL if editing regular Motif resource file.")

(defvar xmr-full-completion-p t
  "T if you want object name completion as well.")

(defvar xmr-complete-resource-name-display-p nil
  "T if all matching names should be displayed during name completion.")

(defvar xmr-program-name "Mri"
  "Name of the program to run to test the resource file.")

(defvar xmr-display-name nil
  "Name of the X display. NIL means use the DISPLAY variable.")

(defvar xmr-tree-indent "| "
  "String used to indent when displaying the widget tree.")

(defvar xmr-rhs-column 40
  "Used for automatic indentation of the resource values.")

(defvar xmr-cbtable-filename nil
  "?")

;(defvar xmr-mode-syntax-table nil
;  "Syntax table used while in XMR mode.")

(defvar xmr-mode-abbrev-table nil
  "Abbrev table used while in XMR mode.")

(defvar xmr-mode-map nil)
(if xmr-mode-map
    ()
  (setq xmr-mode-map (make-sparse-keymap))
  (define-key xmr-mode-map "\t" 'xmr-indent-or-complete)
  (define-key xmr-mode-map "\C-c\t" 'xmr-indent-or-complete*)
  (define-key xmr-mode-map "\C-ch" 'xmr-display-resource-name-completions)
  (define-key xmr-mode-map "\C-c\C-h" 'xmr-display-resource-name-completions*)
  (define-key xmr-mode-map "\C-cc" 'xmr-comment-region)
  (define-key xmr-mode-map "\C-cu" 'xmr-uncomment-region)
  (define-key xmr-mode-map "\C-c\C-r" 'xmr-run-program)
  (define-key xmr-mode-map "\C-ct" 'xmr-display-widget-hierarchy)
  (define-key xmr-mode-map "\C-j" 'xmr-newline-relative)
  (define-key xmr-mode-map "\C-cm" 'xmr-man)
  (define-key xmr-mode-map "\C-c\C-s" 'xmr-rename-object)
  (define-key xmr-mode-map "\e\C-\\" 'xmr-indent-region)
  (define-key xmr-mode-map "\eq" 'xmr-fill-paragraph)
  (define-key xmr-mode-map "\en" 'xmr-next-object)
  (define-key xmr-mode-map "\ep" 'xmr-previous-object)
  (define-key xmr-mode-map "\C-c>" 'xmr-goto-child)
  (define-key xmr-mode-map "\C-c<" 'xmr-goto-parent)
  (define-key xmr-mode-map "\C-cn" 'xmr-goto-next-sibling)
  (define-key xmr-mode-map "\C-cp" 'xmr-goto-previous-sibling)
  (define-key xmr-mode-map "\C-cs" 'xmr-search-object-forward)
  (define-key xmr-mode-map "\C-cr" 'xmr-search-object-backward)
  )

;;;
;;; 03/17/92 isy
;;;
;;; The abbrev stuff has been removed in favor of Dmacro.
;;;
(define-abbrev-table 'xmr-mode-abbrev-table nil)
; (define-abbrev-table 'xmr-mode-abbrev-table
;   '(
;     ;;
;     ;; primitives
;     ;;
;     ("ab" "xmArrowButtonWidgetClass"    nil 0)
;     ("cb" "xmCascadeButtonWidgetClass"  nil 0)
;     ("pb" "xmPushButtonWidgetClass"     nil 0)
;     ("te" "xmTextWidgetClass"           nil 0)
;     ("li" "xmListWidgetClass"           nil 0)
;     ("tb" "xmToggleButtonWidgetClass"   nil 0)
;     ("se" "xmSeparatorWidgetClass"      nil 0)
;     ("la" "xmLabelWidgetClass"          nil 0)
;     ("sc" "xmScaleWidgetClass"          nil 0)
;     ("sb" "xmScrollBarWidgetClass"      nil 0)
;     ;;
;     ;; composites
;     ;;
;     ("bb" "xmBulletinBoardWidgetClass"  nil 0)
;     ("mw" "xmMainWindowWidgetClass"     nil 0)
;     ("fr" "xmFrameWidgetClass"          nil 0)
;     ("fo" "xmFormWidgetClass"           nil 0)
;     ("sw" "xmScrolledWindowWidgetClass" nil 0)
;     ("da" "xmDrawingAreaWidgetClass"    nil 0)
;     ("pw" "xmPanedWindowWidgetClass"    nil 0)
;     ("rc" "xmRowColumnWidgetClass"      nil 0)
;     ;;
;     ;; constructors
;     ;;
;     ("csli" "XmCreateScrolledList"        nil 0)
;     ("cste" "XmCreateScrolledText"        nil 0)
;     ("cmb"  "XmCreateMenuBar"             nil 0)
;     ("copm" "XmCreateOptionMenu"          nil 0)
;     ("cpom" "XmCreatePopupMenu"           nil 0)
;     ("cpum" "XmCreatePulldownMenu"        nil 0)
;     ("cfsb" "XmCreateFileSelectionBox"    nil 0)
;     ("cmeb" "XmCreateMessageBox"          nil 0)
;     ("crab" "XmCreateRadioBox"            nil 0)
;     ("cseb" "XmCreateSelectionBox"        nil 0)
;     ("cbbd" "XmCreateBulletinBoardDialog" nil 0)
;     ("cerd" "XmCreateErrorDialog"         nil 0)
;     ("cind" "XmCreateInformationDialog"   nil 0)
;     ("cfsd" "XmCreateFileSelectionDialog" nil 0)
;     ("cfod" "XmCreateFormDialog"          nil 0)
;     ("cmed" "XmCreateMessageDialog"       nil 0)
;     ("cprd" "XmCreatePromptDialog"        nil 0)
;     ("cqud" "XmCreateQuestionDialog"      nil 0)
;     ("csed" "XmCreateSelectionDialog"     nil 0)
;     ("cwad" "XmCreateWarningDialog"       nil 0)
;     ("cwod" "XmCreateWorkingDialog"       nil 0)
;     ))

(defun xmr-mode ()
  "Major mode for editing X/Motif/WCL resource files.\\{xmr-mode-map}
Turning on xmr-mode calls the value of the variable xmr-mode-hook,
if that value is non-NIL."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (use-local-map indented-text-mode-map)
  (use-local-map xmr-mode-map)
  (setq local-abbrev-table xmr-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (setq mode-name "XMR")
  (setq major-mode 'xmr-mode)
  (run-hooks 'xmr-mode-hook))

(defun xmr-complete-resource-name ()
  "Completes the (partial) resource name that is typed before the point.
If no completion is possible, it beeps; if more than 1 completion is
possible, it displays the possibilities in a temporary buffer."
  (interactive)
  (save-restriction
    (let ((point-save (point))
	  (line-contains-object-name-p (xmr-line-contains-object-name-p)))
      (xmr-parse-context)
      (xmr-narrow-to-word)
      (let* ((xmr-alist (xmr-get-completion-alist))
	     (part-name (buffer-string))
	     (full-name (try-completion part-name xmr-alist)))
	(cond ((eq full-name t)
	       ;;
	       ;; There is an exact match. See if we still need to do a
	       ;; full completion.
	       ;;
	       (delete-region (point-min) (point-max))
	       (cond ((and xmr-current-object-name
			   xmr-full-completion-p
			   (not line-contains-object-name-p))
		      (insert xmr-current-object-name)
		      (insert xmr-name-delimiter)))
	       (insert part-name)
	       )
	      ((null full-name)
	       ;;
	       ;; There is no completion. Inform the user and restore the
	       ;; cursor to its original position.
	       ;;
	       (ding)
	       (goto-char point-save))
	      (t
	       ;;
	       ;; There is a completion. Use it to substitute and position
	       ;; the cursor so that the user may go on typing.
	       ;;
	       (beginning-of-line)
	       (kill-line)
	       (cond ((and xmr-current-object-name
			   xmr-full-completion-p
			   (not line-contains-object-name-p))
		      (insert xmr-current-object-name)
		      (insert xmr-name-delimiter)))
	       (insert full-name)
	       ;;
	       ;; Try the completion again to see whether the completion
	       ;; was unique or not.
	       ;;
	       (setq full-name (try-completion full-name xmr-alist))
	       (if (not (eq full-name t))
		   (if (not xmr-complete-resource-name-display-p)
		       (message "Resource name not unique.")
		     (xmr-list-completions-in-buffer part-name)))))))))

(defun xmr-display-resource-name-completions ()
  "Displays the possible resource name completions in a temporary buffer."
  (interactive)
  (if (xmr-need-completion-p)
      (save-excursion
	(save-restriction
	  (xmr-narrow-to-word)
	  (xmr-list-completions-in-buffer (buffer-string))))))

(defun xmr-display-resource-name-completions* ()
  "Displays the possible resource name completions (including the inherited
ones) in a temporary buffer."
  (interactive)
  (let ((xmr-resource-name-closure t))
    (xmr-display-resource-name-completions)))

(defun xmr-indent-or-complete ()
  "If the resource name is currently being typed, does the name completion.
Else, it does indentation."
  (interactive)
  (if (xmr-need-completion-p)
      (xmr-complete-resource-name)
    (indent-relative)))

(defun xmr-indent-or-complete* ()
  (interactive)
  (let ((xmr-resource-name-closure t))
    (xmr-indent-or-complete)))

(defun xmr-newline-relative ()
  "Inserts a newline and automatically inserts the previous line's
full object name. Need to be at the end of the current line."
  (interactive)
  (if (or (not (eolp)) (bolp))
      (ding)
    (cond ((string= (char-to-string (preceding-char)) xmr-continue)
	   (newline)
	   (indent-relative))
	  (t
	   (xmr-parse-context)
	   (if (not xmr-current-object-name)
	       (ding)
	     (newline)
	     (insert xmr-current-object-name))))))

(defun xmr-next-object ()
  "Moves the point to the beginning of the next object."
  (interactive)
  (beginning-of-line)
  (skip-forward-until-regexp xmr-blank-line-regexp)
  (skip-forward-while-regexp xmr-blank-line-regexp))

(defun xmr-previous-object ()
  "Moves the point to the beginning of the previous object."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at xmr-blank-line-regexp))
      (skip-backward-until-regexp xmr-blank-line-regexp))
  (skip-backward-while-regexp xmr-blank-line-regexp)
  (skip-backward-until-regexp xmr-blank-line-regexp)
  (skip-forward-while-regexp xmr-blank-line-regexp))

(defun xmr-goto-parent ()
  "Moves the point to the beginning of the current object's parent."
  (interactive)
  (xmr-parse-context t)
  (if xmr-parent-object-name
      (xmr-search-object-backward (xmr-clean-name xmr-parent-object-name))))

(defun xmr-goto-child (count)
  "Moves the point to the beginning of the current object's COUNT'th child."
  (interactive "P")
  (if (null count)
      (setq count 0)
    (setq count (- count 1)))
  (xmr-parse-context t)
  (xmr-search-object-forward (nth count xmr-children-names)))

(defun xmr-goto-next-sibling ()
  "Moves the point to the beginning of the current object's right sibling."
  (interactive)
  (xmr-parse-context t)
  (if (null xmr-parent-object-name)
      ()
    (let ((object-name (xmr-clean-name xmr-current-object-name))
	  (point-save (point))
	  subtree
	  sibling-name)
      ;;
      ;; Determine the right sibling by looking at the object's parent's
      ;; children.
      ;;
      (xmr-search-object-backward (xmr-clean-name xmr-parent-object-name))
      (setq subtree (xmr-parse-widget-hierarchy nil))
      (setq sibling-name (car (cdr (string-member object-name
						  (cdr subtree)))))
      (if sibling-name
	  (xmr-search-object-forward sibling-name)
	(goto-char point-save)))))

(defun xmr-goto-previous-sibling ()
  "Moves the point to the beginning of the current object's left sibling."
  (interactive)
  (xmr-parse-context t)
  (if (null xmr-parent-object-name)
      ()
    (let ((object-name (xmr-clean-name xmr-current-object-name))
	  (point-save (point))
	  subtree
	  sibling-name)
      ;;
      ;; Determine the left sibling by looking at the object's parent's
      ;; children.
      ;;
      (xmr-search-object-backward (xmr-clean-name xmr-parent-object-name))
      (setq subtree (xmr-parse-widget-hierarchy nil))
      (setq sibling-name (car (cdr (string-member object-name
						  (reverse (cdr subtree))))))
      (if sibling-name
	  (xmr-search-object-forward sibling-name)
	(goto-char point-save)))))

(defun xmr-search-object-forward (name)
  "Searches forward for the object of the given name."
  (interactive "sName: ")
  (if (re-search-forward (format "%s.\\(%s\\)" name xmr-wcclass-regexp)
			 nil t nil)
      (beginning-of-line)
    (ding)))

(defun xmr-search-object-backward (name)
  "Searches backward for the object of the given name."
  (interactive "sName: ")
  (if (re-search-backward (format "%s.\\(%s\\)" name xmr-wcclass-regexp)
			  nil t nil)
      (beginning-of-line)
    (ding)))

(defun xmr-display-widget-hierarchy ()
  "Displays the widget hierarchy that can be parsed (by following the
`wcChildren' links) from the current point in a temporary buffer."
  (interactive)
  (if xmr-wcl-p
      (xmr-list-tree-in-buffer (xmr-parse-widget-hierarchy t))))

;;;
;;; 01/27/92 courtesy of Ernst Lippe (lippe@serc.nl)
;;;
(defun xmr-comment-region (start end)
   "Comments out the lines in the region."
  (interactive "r")
  (save-excursion
    (save-restriction 
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-regexp "^."
		      (concat (regexp-quote xmr-comment-prefix) "\\&")))))

(defun xmr-uncomment-region (start end)
   "Uncomments commented lines in the region."
  (interactive "r")
  (save-excursion
    (save-restriction 
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (eq (following-char) (string-to-char xmr-comment-prefix))
	  (delete-char 1))
	(end-of-line)
	(if (< (point) (point-max))
	    (forward-char 1))))))

(defun xmr-run-program ()
  "Runs the XMR program which uses the resource file."
  (interactive)
  (let ((shell "sh")
	(process-name "XMR Shell")
	(buffer-name "*XMR Shell Output*")
	(display-name (or xmr-display-name (getenv "DISPLAY"))))
    (cond ((null xmr-program-name)
	   (error "XMR program name not set."))
	  ((null display-name)
	   (error "XMR display name not set."))
	  (t
	   ;;
	   ;; Can have at most 1 process running at a time.
	   ;;
	   (if (or (not (eq (process-status process-name) 'run))
		   (y-or-n-p "XMR program running, kill it? "))
	     (condition-case ()
		 (progn
		   (interrupt-process process-name)
		   (sit-for 1)
		   (delete-process process-name))
	       (error nil))
	     (error "XMR program already running."))
	   (start-process process-name buffer-name shell)
	   ;;
	   ;; set DISPLAY
	   ;;
	   (process-send-string process-name
				(format "DISPLAY=%s;export DISPLAY\n"
					display-name))
	   ;;
	   ;; set XENVIRONMENT
	   ;;
	   (process-send-string process-name
				(format "XENVIRONMENT=%s; export XENVIRONMENT\n"
					(buffer-file-name)))
	   (if (string= "Mri" xmr-program-name) 
	       (process-send-string process-name
				    (format "exec %s %s\n"
					    xmr-program-name
					    (file-name-nondirectory (buffer-file-name))))
	     (process-send-string process-name
				  (format "exec %s\n" xmr-program-name)))))))

(defun xmr-man (class-name)
  "Display the man page for a Motif widget."
  (interactive (xmr-read-class-name "Man page for widget: "))
  (let ((manpage-name (and class-name (xmr-class-manpage class-name))))
    (cond ((null manpage-name)
	   (message (format "Man page for %s not found." class-name))
	   (ding))
	  (t
	   (save-excursion
	     (man (format "%s(3X)" manpage-name)))
	   (let ((pop-up-windows t))
	     (display-buffer manual-buffer-name))))))

(defun xmr-rename-object ()
  "Renames the current object's name."
  (interactive)
  (xmr-parse-context)
  (if (null xmr-current-object-name)
      (ding)
    (let ((new-name (read-string "New name: ")))
      (if (equal new-name "")
	  ()
	(save-excursion
	  (save-restriction
	    (xmr-narrow-to-object)
	    (goto-char (point-min))
	    (replace-regexp (concat "^" xmr-current-object-name)
			    new-name)))))))

(defun xmr-indent-region (start end)
  "Formats the resource specification contained in the given region."
 (interactive "r")
 (save-excursion
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward (concat "^[^"
				      xmr-comment-prefix
				      " \t"
				      "].*"
				      xmr-delimiter)
			      nil t nil)
      (delete-horizontal-space)
      (while (< (current-column) xmr-rhs-column)
	(tab-to-tab-stop))))))

(defun xmr-fill-paragraph ()
  "Formats the current object's resource specification."
  (interactive)
  (save-excursion
    (save-restriction
      (xmr-narrow-to-object)
      (xmr-indent-region (point-min) (point-max)))))

;;;
;;; packages required
;;;

(require 'manual "man")
(require 'xmr-database "xmr-database")
(if (memq 'dmacro features)
    (require 'xmr-dmacro "dm-xmr"))
(provide 'xmr-mode)
