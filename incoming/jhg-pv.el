;;; jhg-pv.el --- Additional commands for editing perl code.
;;
;; $Id: jhg-pv.el,v 1.14 1998/12/22 16:10:42 harley Exp $
;;

;; Author:        J. Harley Gorrell  <harley@bcm.tmc.edu>
;; Keywords:      perl

;; LCD Archive Entry:
;; jhg-pv|Harley Gorrell|harley@bcm.tmc.edu|
;; Commands for 'my' perl variables|
;; 1996-07-12| (not yet submitted)

;;; Commentary:

;; Declaring all those 'my' variables can be time consuming
;; and I seem to overlook one or two now and then.  'jhg-pv'
;; helps me catch some the stragglers.

;; Bind 'jhg-pv' to a function key and go wild.

;; This package expects 'properly' indented code.  That is
;; not a big problem when using perl-mode, so jhg-pv expects
;; it.

;; Global variables are variables which start in the first
;; column anywhere in the buffer.  Variables declared with
;; 'my' or 'local' should have only whitespace in front of
;; them.  You may use '#global' to document global
;; variables.  '#ignore' may be used to work around over
;; zealous declerations on the part of this code.

;; If font-lock-mode is on, it will not pull variables out
;; of strings.  If not, it will think "%s" and the like is
;; fair game.

;; Enjoy!

;;; Code:

(defvar jhg-pv-are-special '( "%ENV" "@ARGV" )
  "*List of variables which are special and should not be declared.")

(defvar jhg-pv-are-local '( "$a" "$b" )
  "*List of variables which should be local.
These are typically the variables used in a sort() expression.")

(defvar jhg-pv-decl-global t
  "*Should jhg-pv declare globals?")

(defvar jhg-pv-decl-local t
  "*Should jhg-pv declare locals?")

(defvar jhg-pv-tag-string "#" ;"#pv"
  "*String to tag inserted lines with.")

(defvar jhg-pv-always-update-global-vars t
  "*Always scan the buffer for global vars before inserting
declarations.  Set to nil if you have a big buffer.  The
global variable list may be manually updated with
'jhg-pv-set-global-list'." )

(defvar jhg-pv-buffer-global-vars nil 
  "Global perl variables used in this buffer."  )

;; Make 'em all buffer local
(make-variable-buffer-local 'jhg-pv-are-special)
(make-variable-buffer-local 'jhg-pv-are-local)
(make-variable-buffer-local 'jhg-pv-decl-global)
(make-variable-buffer-local 'jhg-pv-decl-local)
(make-variable-buffer-local 'jhg-pv-tag-string)
(make-variable-buffer-local 'jhg-pv-always-update-global-vars)
(make-variable-buffer-local 'jhg-pv-buffer-global-vars)




;;
(defun jhg-pv-find-vars (r-start r-end)
  "Returns a list of variables in the region with their declaration.

The list is formatted like: ((var1 . decl) (var2 . decl) ... )
Where var is a string and decl is one of
   'special
   'global-decl     'global-use 
   'local-decl      'local-use
   'my-decl         'my-use
   'ignore-decl               
"
  (interactive "r")
  (let ((v-list nil) v-name v-loc v-pos v-type v-face)
    (save-excursion
      ;; top
      (goto-char r-start)

      ;; find the next
      (while (search-forward-regexp "[$@%][A-Za-z][A-Za-z0-9_:]*[[{]?" r-end t)

	;; get its name
	(setq v-name (buffer-substring-no-properties 
		      (match-beginning 0) (match-end 0)))
	(setq v-name (jhg-pv-proper-var v-name))

	;; get its face	
	(setq v-face (get-text-property (match-beginning 0) 'face))
	  
	;; How is this variable defined?
	(setq v-pos (match-end 0))
	(beginning-of-line)
	(setq v-type 
	      (cond
	       ;; special, global
	       ((member v-name jhg-pv-are-special) 'special)
	       ((assoc v-name jhg-pv-buffer-global-vars) 'global-decl)
	       ;; declarations
	       ((looking-at "\\s-*#global(") 'global-decl)
	       ((looking-at "\\s-*local(") 'local-decl)
	       ((looking-at "\\s-*my(") 'my-decl)
	       ((looking-at "\\s-*#ignore(") 'ignore-decl)
	       ;; in a string or comment?
	       ((and 
		 (boundp 'font-lock-string-face)
		 (or (eq v-face font-lock-string-face)
		     (eq v-face font-lock-comment-face))) nil) ; forget
	       ;; Used in some way 
	       (t (funcall jhg-pv-declarer v-name))))
	(goto-char v-pos)

	;; add to list
	(if (and v-type (not (assoc v-name v-list)))
	    (setq v-list (cons (cons v-name v-type) v-list)))
	)
      v-list
      )))

(defun jhg-pv-find-global-vars (r-start r-end)
  "Return a list of global varibles in the region."
  (interactive "r")
  (save-excursion 
    (let (v-name v-list)
      ;; top
      (goto-char r-start)
      ;; find
      (while (search-forward-regexp "^[$@%][A-z][A-Za-z0-9_:]*" r-end t)
	(setq v-name (buffer-substring (match-beginning 0) (match-end 0)))
	(set-text-properties 0 (length v-name) nil v-name)
	(setq v-list (cons (cons v-name 'global) v-list)))
      ;;
      v-list
      )))

(defvar jhg-pv-declarer 'jhg-pv-declarer-default
  "The function to determine the decleraton type")
(make-variable-buffer-local 'jhg-pv-declarer)

(defun jhg-pv-declarer-my (v)
  "Always declare variables as 'my'."
  'my)

(defun jhg-pv-declarer-default (v)
  "Deterimine what kind of declaration the variable should receive.
Return value is one of special, global, local, or my.
"
  (let ((case-fold-search nil))
    (cond
     ((member v-name jhg-pv-are-local) 'local-use)
     ((string-match "^%ENV$" v) 'special)
     ((string-match "^[$@%][A-Z]+$" v) 'global-use)
     ((string-match "::" v) 'global-use)
     ((string-match "^[$@%]opt_" v) 'global-use)
     ((string-match "^\\$[ab]$" v) 'local-use) ;; for sort({$a<=>$b})
     (t 'my-use))))


;(let ((l '("@ARGV" "@ARGVaaa" "$ARGVaaa" "$ENV" 
;	   "$opt_a" "$a" "$aa" "$aa" "$foo")))
;  (insert "\n")
;  (while l
;    (insert (format "%s:%s\n" (car l) (funcall jhg-pv-declarer (car l))))
;    (setq l (cdr l))))

(defun jhg-pv-filter-vars (v-type v-list)
  "Filter a list of variables by their type."
  (let ((v-out ()))
    (while v-list
      (if (eq (cdr (car v-list)) v-type)
	  (setq v-out (cons (car (car v-list)) v-out))
	)
      (setq v-list (cdr v-list))
      )
    v-out
    ))
      
(defun jhg-pv-proper-var (v-name)
  "Convert a perl variable to its 'proper' form.
The expression '$foo{...}' is a use of the hash 'foo', 
while '$bar[...]' is a use of the list 'bar'.
"
  (let (v-type v-name-len)
    ;;
    (setq v-name-len (1- (length v-name)))
    (setq v-type (aref v-name v-name-len))
    ;;
    (cond
     ((= v-type   91 ) ; [
      (concat "@" (substring v-name 1 v-name-len)))
      ((= v-type 123 ) ; {
       (concat "%" (substring v-name 1  v-name-len)))
      (t v-name)
      )))


;; (jhg-pv-proper-var "$foo{")
;; (jhg-pv-proper-var "$foo[")
;; (jhg-pv-proper-var "$foo")



(defun jhg-pv-insert (v-decl v-list)
  "Insert the declaration of a list of variables."
  (beginning-of-line)
  (insert " " v-decl "(") ; " " to move it off col 0 for tabbing
  (while v-list
    (insert (car v-list))
    (setq v-list (cdr v-list))
    (if v-list 
	(insert ","))
    )
  ;; The "#" is to mark the lines this function inserts.
  (insert "); " jhg-pv-tag-string) 
  (indent-according-to-mode)
  (insert "\n")
  )

;; (jhg-pv-insert-my '( "aaa" "bbb"))

(defun jhg-pv-set-global-list ()
  "Set the jhg-pv cache of global variables."
  (interactive)
  (setq jhg-pv-buffer-global-vars 
	(jhg-pv-find-global-vars (point-min) (point-max))
	))

(defun jhg-pv-clear-global-list ()
  "Clear the jhg-pv cache of global variables."
  (interactive)
  (setq jhg-pv-buffer-global-vars nil))


;;;###autoload
(defun jhg-pv ()
  "Examine the current perl subroutine and insert a declaration.
Variables are considered 'declared' if they appear as:

     my($myvar);
     local($localvar);
or
     #global($globalvar);
     #ignore(%d);

Used but undeclared variables are inserted as a 'my' declaration.

If font-lock mode is active, the faces are used to do a better job.
"
  (interactive)
  (let (r-s r-e v-list g-list l-list m-list)

    ;; Select the block
    (save-excursion
      (mark-perl-function)
      (setq r-s (point)
	    r-e (mark)))

    ;; Inside a sub?
    (if (and r-s r-e (< r-s (point)) (> r-e (point)))
	(progn

	  ;; Refresh globals?
	  (if (or jhg-pv-always-update-global-vars
		  (not jhg-pv-buffer-global-vars))
	      (jhg-pv-set-global-list))

	  ;; Make a list of vars in the region
	  (setq v-list (jhg-pv-find-vars r-s r-e))

	  ;; global?
	  (if jhg-pv-decl-global 
	      (progn
		(setq g-list (jhg-pv-filter-vars 'global-use v-list))
		(if g-list (jhg-pv-insert "#global" (sort g-list 'string<)))))

	  ;; local?
	  (if jhg-pv-decl-local
	      (progn 
		(setq l-list (jhg-pv-filter-vars 'local-use v-list))
		(if l-list (jhg-pv-insert "local" (sort l-list 'string<)))))

	  ;; my
	  (setq m-list (jhg-pv-filter-vars 'my-use v-list))
	  (if m-list (jhg-pv-insert "my" (sort m-list 'string<)))
	  
	  ))))

;;
(provide 'jhg-pv)

;;; jhg-pv.el ends here
