;;; jde-complete.el -- Smart completion for the JDE
;; $Revision: 1.10 $ 

;; Author: Rodrigo Reyes <reyes@chez.com>
;; Maintainers: Rodrigo Reyes, Paul Kinnucan, Howard Spector, 
;;              Stephane Nicolas <s.nicolas@videotron.ca>
;; Keywords: java, intellisense, completion

;; Copyright (C) 1999 Rodrigo Reyes

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Java is a registered trademark of Sun Microsystem, Inc.
;;
;;; Commentary:

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;;
;; This package adds smart completion to the JDE. How it works is
;; simple : put the cursor at the end of a statement "under
;; construction", eg. "myVariable.rem<CURSOR HERE> and call the
;; jde-complete-at-point emacs-lisp function (this is by default
;; C-.). A completion is then inserted. If multiple completions are
;; possible, calling the completion function again will cycle through
;; all the possibilities (as dabbrev-mode does).

;; To retrieve all the possible completions, it uses the java code in
;; jde.util.Completion.getClassInfo(), called by beanshell. That
;; need the class to be compiled (but that's not worst than an etag
;; call).

;; Known bugs/problems :

;; - Due to the way the JVM works, it is not possible to explicitly
;; unload a class. So, if major changes are done in a class, the
;; beanshell must be restarted in order to reload the class.

;;
;; TODO :
;;
;; - [EASY] Check for the variables,
;; - [NOT THAT EASY] Keep the completion information in the minibuffer
;; (it is currently erased after the user presses a key).
;; - [AVERAGE] Add a cache for the class informations.
;; - Check for fields declared at the end of the class.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

(defvar jde-complete-current-list nil
  "The list of all the completion. Each element of the list is a list
which car is the possible completion, and the cdr is an additional
information about this completion.")

(defvar jde-complete-current-list-index nil
  "An index to an element in jde-complete-current-list. This is used to
cycle the list.")

(defvar jde-complete-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar jde-complete-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

(defun jde-complete-import-list ()
  "Build the list of java package declared in the current buffer.
It mostly scans the buffer for 'import' statements, and return the
resulting list. It impliciyly adds the java.lang.* package."
  (save-excursion
    (beginning-of-buffer)
    (let (lst first second)
      (if (re-search-forward "package[ \t\n\r]+\\([a-zA-Z0-9.]+\\)[ \t\n\r]*;" nil t)
	  (setq lst (append lst (list (list (concat (match-string 1) ".") "*")))))
      (while (not (null 		 
		   (re-search-forward "import[ \t\n\r]+\\(\\([a-zA-Z0-9]+[.]\\)+\\)\\([*]\\|[a-zA-Z0-9]+\\)" nil t) ))
	(setq first (match-string 1))
	(setq second (match-string 3))
	(if (string= "*" second)
	    (setq lst (append lst
			      (list (list first second))))
	  (setq lst (append (list (list first second))
			    lst))))
      (if (not (member "java.lang.*" lst))
	  (setq lst (append lst (list (list "java.lang." "*")))))
      lst)))



(defun jde-complete-valid-java-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
(save-excursion
  (goto-char point)
  (if (looking-at (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" (jde-complete-double-backquotes varname) "[ \t\n\r]*[;=]"))
      (match-string 1)
    nil)))
  
(defun jde-complete-double-backquotes (varname)
  "Build a new string identical to VARNAME, except that every backquote
`\' is doubled, so that it can be used in a regex expression"
  (let (result (idx 0) (len (length varname)) curcar)
    (while (< idx len)
      (setq curcar (elt varname idx))
      (setq result (concat result (if (eq curcar ?\\)
				      "\\\\"
				    (make-string 1 curcar))))
      (setq idx (+ 1 idx)))
    result))

(defun jde-complete-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jde-complete-valid-java-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
    res)))

(defun jde-complete-filter-fqn (importlist)
"Filter all the fully-qualified classnames in the import list. It uses
the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if (not (null importlist))
      (if (string= "*" (car (cdr (car importlist))))
	  importlist
	(jde-complete-filter-fqn (cdr importlist)))))



(defun jde-complete-guess-type-of (name)
"Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or a list of possible
packages otherwise."
  (let ((importlist (jde-complete-import-list)) shortname fullname tmp result)
    (while (and (not (null importlist)) (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond 
       ((string= "*" shortname)
	(setq result importlist))
       ((string= name shortname)
	(setq result fullname))
       (t 
	(setq importlist (cdr importlist)))))
    result))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Returns t if the fully qualified class name can be found in the
;; classpath, nil otherwise
(defun jde-complete-class-exists (name)
  (bsh-eval-r (concat "try { "
                      "Class.forName(\"" name "\"); "
                      "System.out.println(\"t\"); "
                      "} catch (Exception e) { "
                      "System.out.println(\"nil\");}\n")))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Get the fully qualified name of the class NAME, using the import
;; list. It returns a string if the fqn was found, or null otherwise.
;; This is more capable than jde-complete-guess-type-of because it
;; uses the beanshell to determine if an import statement with a
;; wildcard contains the unqualified class name passed to this
;; function.
(defun jde-complete-get-qualified-name (name)
"Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or null otherwise."
  (let ((importlist (jde-complete-import-list)) shortname fullname tmp result)
    (while (and (not (null importlist)) (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond 
       ((and (string= "*" shortname) (jde-complete-class-exists fullname))
        (setq result fullname))
       ((string= name shortname)
	(setq result fullname))
       (t 
	(setq importlist (cdr importlist)))))
    result))

(defun jde-complete-get-classinfo (name &optional qualifiedp)
  "Return the class info list for the class NAME.  If QUALIFIEDP is nil, this function tries todetermine the package to which class NAME belongs. Each element of the list returned by this function is itself a list whose car is a possible completion and whose cdr gives additional informations on the completion."
  (let* ((qualified-name
	  (if qualifiedp name (jde-complete-guess-type-of name)))
	 (result 
	  (bsh-eval
	   (if (stringp qualified-name)
	       (concat "jde.util.Completion.getClassInfo(\"" qualified-name "\");")
	     (jde-complete-get-classinfo-javacode name qualified-name)))))
    (if result (eval (read result)))))

(defun jde-complete-get-classinfo-javacode (name import)
"Return the java code that calls the
jde.util.Completion.getClassInfo function with the short java class
name NAME and the package list IMPORT where to look at."
  (save-excursion
    (concat "{ " 
	      "String[] lst = new String[" (number-to-string (length import)) "];\n"
	      (let ((count -1))
		(mapconcat (function (lambda (x) 
				       (setq count (+ 1 count))
					   (concat "lst[" (int-to-string count) "]=\"" 
						   (car (nth count import)) "\";\n")))
			   import
			   " "))
	      "jde.util.Completion.getClassInfo(\"" name "\",lst);\n"
	      "}")))

(defun t1 ()
  (interactive)
  (let ((r (jde-complete-java-variable-at-point)))
  (message "(%s, %s)" (car r) (car (cdr r)))))

(defun jde-complete-isolate-to-complete (s)
"Returns the right expression that needs completion in S." 
 (let* ((index (length s)) stop (paren 0) curcar)
  (while (and (> index 0)
              (not stop)
         )     
   (setq index (- index 1))
   (setq curcar (aref s index))
   (if (eq ?\) curcar)
    (setq paren (+ 1 paren))
   )
   (if (eq ?\( curcar)
    (setq paren (- paren 1))
   )

   (if (or (< paren 0)
           (and (eq curcar ?\,) (<= paren 0)
           )
       )
    (setq stop t)
   ) 
  )
  (if stop
   (setq index (+ 1 index))
  )
  (substring s index)
 )
)

;;(jde-complete-isolate-to-complete "this.read().read( toto,tata( this.")

(defun jde-complete-java-variable-at-point ()
  "Returns a list (VAR PARTIAL) where VAR.PARTIAL is the partially completed method or field
name at point. For example, suppose obj.f1.ge were the name at point. This function would return
the list (obj.f1 ge)."
  (save-excursion
    (let (start 
	  varname 
	  curcar 
	  found 
	  (original-point (point)) 
	  intermediate-point 
	  beg-point
	  first-part
	  second-part)
      (setq curcar (char-before))
      (while (null found)
	(cond 
	 ((or (and (>= curcar ?a) (<= curcar ?z))
	      (and (>= curcar ?A) (<= curcar ?Z))
	      (and (>= curcar ?0) (<= curcar ?9))
	      (>= curcar 127)
	      (member curcar '(?_ ?\\ )))
	  (forward-char -1))
	 ((eq ?. curcar)
	  (setq found (point)))
	 (t
	  (setq found t)))
	(setq curcar (char-before)))
      ;;
      (setq intermediate-point (point))
      (if (not (eq t found))
	  (progn 
	    (setq curcar (char-before))
	    (while (or (and (>= curcar ?a) (<= curcar ?z))
		       (and (>= curcar ?A) (<= curcar ?Z))
		       (and (>= curcar ?0) (<= curcar ?9))
		       (>= curcar 127)
		       (member curcar '(?. ?_ ?\\ ?\( ?\) ? ?\, )))
	      (forward-char -1)
	      (setq curcar (char-before)))
	    (setq beg-point (point))
	    (set-marker jde-complete-current-beginning intermediate-point)
	    (set-marker jde-complete-current-end original-point)
            (setq first-part (buffer-substring-no-properties beg-point (- intermediate-point 1)))
            (setq first-part (jde-complete-isolate-to-complete first-part))
            (string-match " *\\(.*\\)" first-part)
            (setq first-part (substring first-part (match-beginning 1) (match-end 1)))
            (setq second-part (buffer-substring-no-properties intermediate-point original-point))
	    (list first-part second-part))
	nil))))

(defun jde-complete-build-completion-list (classinfo)
"Build a completion list from the CLASSINFO list, as returned by the
jde.util.Completion.getClassInfo function."
  (let ((result nil) (tmp nil))
    ;; get the variable fields
    (setq tmp (car classinfo))
    (while (not (null tmp))
      (setq result (append (list (list (car (car tmp)) (concat (nth 1 (car tmp)) " "(car (car tmp)) ) )) result))
      (setq tmp (cdr tmp)))
    ;; get the methods 
    (setq tmp (nth 2 classinfo))
    (while (not (null tmp))
      (setq result (append (list (list (concat (car (car tmp))"(")
				       (jde-complete-build-information-for-completion (car tmp))
				       ;; (car tmp)
				       )) result))
      (setq tmp (cdr tmp)))
    result)

)

(defun jde-complete-build-information-for-completion (lst)
  (let ((result (concat (car (cdr lst)) " " (car lst) "(")))
    (setq lst (cdr (cdr lst)))
    (while (not (null lst))
      (setq result (concat result (car lst)))
      (setq lst (cdr lst))
      (if (not (null lst))
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    result))

(defun jde-complete-complete-cycle ()
  "Replace the previous completion by the next one in the list."
  ;(debug)
  (let (elem)
    (setq jde-complete-current-list-index (+ 1 jde-complete-current-list-index))
    (if (>= jde-complete-current-list-index (length jde-complete-current-list))
	(setq jde-complete-current-list-index 0))
    (setq elem (nth jde-complete-current-list-index jde-complete-current-list))
    (if (not (null (car elem)))
	(progn
	  (delete-region jde-complete-current-beginning jde-complete-current-end)
	  (insert (car elem))
	  (set-marker jde-complete-current-end 
		      (+ (marker-position jde-complete-current-beginning) (length (car elem))))
	  (message (car (cdr elem))))
      (message (format "No completion at this point!(cycle)")))
    ;;  (goto-char (marker-position jde-complete-current-end))
    ))

(defun jde-complete-insert-completion (item)
  (if item 
      (let* ((chop-point
	      (if (string-match " : " item)
		  (string-match " : " item)
		(length item)))
	     (completion (substring item 0 chop-point)))
	(delete-region jde-complete-current-beginning jde-complete-current-end)
	(insert completion)
	(set-marker jde-complete-current-end 
		    (+ (marker-position jde-complete-current-beginning) 
		       (length completion))))))

(defun jde-complete-popup-xemacs-completion-menu (completion-list)
 (let* ((items
	 (sort
	  ;; Change each item in the completion list from the form
	  ;;   return-value method-name(args)
	  ;; to the form
	  ;;   method-name(args) : return-value
	  (mapcar
	   (lambda (completion)
	     (let ((completion-short (nth 0 completion))
		   (completion-long (nth 1 completion)))
	       (if completion-long
		   (let ((chop-pos (string-match " " completion-long)))
		     (concat 
		      (substring completion-long (+ 1 chop-pos)
				 (length completion-long)) 
		      " : " 
		      (substring completion-long 0 chop-pos)))
		 completion-short)))
	   completion-list)
	  'string<))
	(menu	
	 (cons
	  "Completions"
	  (mapcar
	   (lambda (item)
	     (vector item (list 'jde-complete-insert-completion item)))
	   items))))
   (popup-menu-and-execute-in-window menu (selected-window))))

;; Howard Spector provided the first version of this function.
(defun jde-complete-popup-emacs-completion-menu (completion-list)
  "Builds a popup menu which displays all of the possible completions for the
   object it was invoked on."
  (let* ((menu 
	  (cons 
	   "Completions"
	   (list 
	    (cons ""
		  (mapcar 
		   (lambda (elt) 
		     (cons  elt elt))
		   ;; Sort completion list alphabetically by method name
		   (sort
		    ;; Change each item in the completion list from the form
		    ;;   return-value method-name(args)
		    ;; to the form
		    ;;   method-name(args) : return-value
		    (mapcar
		     (lambda (completion)
		       (let ((completion-short (nth 0 completion))
			     (completion-long (nth 1 completion)))
			 (if completion-long
			     (let ((chop-pos (string-match " " completion-long)))
			       (concat 
				(substring completion-long (+ 1 chop-pos)
					   (length completion-long)) 
				" : " 
				(substring completion-long 0 chop-pos)))
			   completion-short)))
		     completion-list)
		    'string<))))))
	 (mouse-pos (if (nth 1 (mouse-position))
			(mouse-position)
		      (set-mouse-position 
		       ;;(car (mouse-position))   ;; Frame
		       (if jde-xemacsp (selected-window) (selected-frame))
		       (/ (frame-width) 2);; x position
		       2);; y position
		      (mouse-position)))
	 (pos (list 
	       (list 
		(car (cdr mouse-pos));; XOFFSET
		(1+ (cdr (cdr mouse-pos))));; YOFFSET
	       (car mouse-pos)));; WINDOW
	 (pos
	  (list
	   (list 1 1)
	   (selected-window)))
	 (name (x-popup-menu pos menu)))
    (jde-complete-insert-completion name)))


(defun jde-complete-find-all-completions (pat lst)
  (let ((result nil))
    (while (not (null lst))
      (if (equal 0 (string-match pat (car (car lst))))
	  (setq result (append (list (car lst)) result)))
      (setq lst (cdr lst)))
    result))

(defun jde-complete-split-by-dots (s)
"Return a list containing the longest substring of S that ends with a dot, and the rest.
 For now we here get rid of parenthesis.This allows to get read of arguments methods,
 so that we can get only the name of methods.
 But removes the intermediate(=last) dot"
 (let (result)
  ;;;For now we here get rid of parenthesis.This allows to get read of arguments methods,
  ;;;so that we can get only the name of methods."
  (while (string-match "\\(([^()]*)\\)" s)
   (setq s (replace-match "" nil t s))
  )
  ;;we now isolate the last atom after a dot and the beginning
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
   (progn
    (setq first (substring s (match-beginning 1) (match-end 1)))
    (setq second (substring s (match-beginning 2) (match-end 2)))
    (setq result (list first second))
   )
  )
  result
 ))

(defun jde-complete-find-completion-for-pair (pair)
"Fills jde-complete-current-list with the completion possibilities of PAIR.
 PAIR is a list whose car is the class to complete and nth 1 is radix of desired completion."
   ;(debug)
   (let* (
	   classinfo fulllist to-complete name-is-qualified nth1-split method-name)
       (setq jde-complete-current-list nil)
       ;;if pair is not null
       (if (not (null pair))
        (progn
          ;;we try to complete its field/method
          ;;we first check to see if the expression if iterated or not (iterated means x.y(.z...)) 
          (setq split (jde-complete-split-by-dots (car pair)))
          ;;if so , we must enter in a recusive mode to find the type
          ;;of each atomic expression x,y,...
          ;;if our expression is atomik
          (setq to-complete nil)
            (cond
             ((not (null split))
              (progn 
               (setq nth1-split (nth 1 split))
               (if (eq (aref nth1-split (- (length nth1-split) 1)) ?\) )
                 (progn 
                  (string-match "\\(.*\\)(.*)" nth1-split)
                  (setcar (cdr split) (substring nth1-split (match-beginning 1) (match-end 1)))
                 )
                )
                ;;we find the type of the two last iterations (eg y.z) (recursive)
    		(jde-complete-find-completion-for-pair split)
                ;;we so check for the result type
                (setq to-complete (nth 1 (car jde-complete-current-list)))
                (string-match "\\(.*?\\) " to-complete)
                (setq to-complete (substring to-complete (match-beginning 1) (match-end 1)))
                ;;and we re-use this type in a new completion
                ;;the type is fully qualified as there is no need to import
                ;;itermediate classes in iterations
                ;;this is the recursive core of the completion
                ;(setq to-complete vtype)
                (setq name-is-qualified t)
               )
             ) 
             ;;a powerfull feature for beautifull code
             ;; ".x" <-complete --> "this.x<completion>"
             ((string= "" (car pair))
              (progn
               (goto-char (- jde-complete-current-beginning 1)) 
               (insert "this")
               (goto-char jde-complete-current-beginning ) 
               (setq to-complete  (jde-parse-get-class-at-point))
              )
             )
             ;;if we are trying to complete super if it begins with super
             ((string= "super" (car pair))
              (setq to-complete  (jde-parse-get-super-class-at-point))
             )
             ;;else if we are trying to complete this if it begins with this
             ((string= "this" (car pair))
              (setq to-complete  (jde-parse-get-class-at-point))
             )
             ;;this atom is a this.method
             ((string= ")" (substring (car pair) (- (length (car pair)) 1)))
              (progn 
               (setq to-complete (jde-parse-get-class-at-point))
               (string-match "\\(.*\\)(.*)" (car pair))
               (setq method-name (substring (car pair) (match-beginning 1) (match-end 1)))
               (jde-complete-find-completion-for-pair (list to-complete method-name))
               ;; after the recursive call ends, jde-complete-current-list
               ;;has been filled
               (setq to-complete (nth 1 (car jde-complete-current-list)))
               (string-match "\\(.*?\\) " to-complete)
               (setq to-complete (substring to-complete (match-beginning 1) (match-end 1)))
               ;;and we re-use this type in a new completion
               ;;the type is fully qualified as there is no need to import
               ;;itermediate classes in iterations
               ;(setq to-complete vtype)
               (setq name-is-qualified t)
              )
             )
             (t
              (progn
               ;;else we look if the variable is declared in the buffer
               (setq to-complete (jde-parse-declared-type-of (car pair)))
               ;; if not, we try the value as is, it's an attempt for a static method
               (if (null to-complete)
                (setq to-complete (car pair))
               )
              )
             )
            )
            (setq classinfo (jde-complete-get-classinfo to-complete name-is-qualified))
            (setq fulllist (jde-complete-build-completion-list classinfo))
            (setq jde-complete-current-list 
             (jde-complete-find-all-completions (car (cdr pair)) fulllist))
        )
       )))

(defun jde-complete-at-point ()
  "Completes the method or field name at point.
Repeating the command cycles through all potential completions for the name.
This function displays the signature of a method completion in the minibuffer.
This command uses the Beanshell to run Java code that in turn uses Java
reflection to determine the methods and fields defined by the class of the
object at point. This command starts the Beanshell if necessary. Hence, you
may experience a slight delay when using this command for the first time in
a session or when completing a field or method of an object that has many
methods and fields. See `jde-complete-at-point-menu' for a version of this 
command that lets you select the desired completion from a popup menu."
  (interactive)
  (if (and
       (not (null jde-complete-current-list))
       (markerp jde-complete-current-beginning)
       (markerp jde-complete-current-end)
       (marker-position jde-complete-current-beginning)
       (marker-position jde-complete-current-end)
       (>= (point) (marker-position jde-complete-current-beginning))
       (<= (point) (marker-position jde-complete-current-end))
       (eq last-command this-command)
      )
      (jde-complete-complete-cycle) 
      ;;else
      (progn 
       (jde-complete-find-completion-for-pair (jde-complete-java-variable-at-point))
       (setq jde-complete-current-list-index -1)
       (jde-complete-complete-cycle))))

(defun jde-complete-at-point-menu()
  "Completes the method or field name at point.
This command displays a popup menu listing the potential completions for the name
at point. Selecting a completion causes the command to use the completion to complete
the name at point. See `jde-complete-at-point' for a version of this 
command that lets you cycle throught the potential completions at point."
  (interactive)
  (let* ((pair (jde-complete-java-variable-at-point))
	 vtype classinfo fulllist completion-list)
                (jde-complete-find-completion-for-pair pair)
		(setq completion-list jde-complete-current-list)
;;		      (jde-complete-find-all-completions (car (cdr pair)) fulllist))
		(if completion-list
		    (if jde-xemacsp
			(jde-complete-popup-xemacs-completion-menu completion-list)
		      (jde-complete-popup-emacs-completion-menu completion-list))
		  (message "No completion at this point."))))

(provide 'jde-complete)

;; $Log: jde-complete.el,v $
;; Revision 1.10  2000/07/30 20:06:12  paulk
;; Updated doc for jde-complete-at-point and jde-complete-at-point-menu commands.
;;
;; Revision 1.9  2000/07/27 04:54:00  paulk
;; Now completes object fields to any depth and completes variables declared in method argument lists. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
;;
;; Revision 1.8  2000/07/26 14:42:23  paulk
;; Adds support for static fields and methods and completion of fields and methods of this
;; and super objects. Thanks to  Stephane Nicolas <s.nicolas@videotron.ca> for this enhancement.
;;
;; Revision 1.7  2000/06/01 05:52:25  paulk
;; Completion menu now works on XEmacs.
;;
;; Revision 1.6  2000/05/16 04:41:28  paulk
;; *** empty log message ***
;;

;; end of jde-complete.el
