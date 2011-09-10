;;; elder.el; ELDER, the ELisp Document writER/formattER/unitER. 
;;; UNITES LISP WITH OTHER LANGUAGES.
;;; See comments at the end.

;;;---Deepak Goel deego@glue.umd.edu
;;; UNRESTRICTIONS: see README 
;;;
;;;Global variables that should *NOT* be messed around with if you are
;;;the end-user: *elder-aliases*, *elder-delimiter-list*. 
;;;
;;;
;;;

;;;====================================================

(defmacro elder-detailed-error (arg)
  "Wrap this around a code to generate detailed error-message if any.
The code is executed normally, except that in case any error occurs,
the error-message is more detailed. The emacs's error is indicated,
along with the current location of the elder-document being
processed."
  (list 'condition-case 'err arg
	(list 'error (list 'error
			   (list 'concat 
				 "THIS ERROR OCCURED:\n"
				 (list 'error-message-string 'err)
				 "\nWHILE WE ARE HERE:"
				 (list 'show-position-my
				       (list 'point)))))))


;;;====================================================

(require 'cl)
(setq *elder-trace* nil)

(defun elder-parse-csh-list-my (string)
 "Given a string  as a csh-list, makes it a lisp-list. Basically
copied from .emacs.macros on 9/23/00.." 
 (let 
     ((ll (length string)))
   (if (<= ll 0)
       nil
     (with-temp-buffer
       (insert string)
       (let ((firstpos
	      (progn
		(goto-char (point-min))
		(search-forward ":" nil t))))
	 (if (null firstpos)
	     (list string)
	   (cons (buffer-substring (point-min) (- firstpos 1))
		 (elder-parse-csh-list-my
		  (buffer-substring firstpos
				    (point-max)))))))))
)

(defun elder-getenv-my (var)
  "Gets the variable as a list of strings rather than single string.
You get the idea.. The : is perhaps always assumed to be a delimiter.. even
when preceded by a // . Might someday get to look into
this.. Basically copied from .emacs.macros on 9/23/00"
   (elder-parse-csh-list-my (getenv var))
)

(defun elder-insert-file-contents (file path)
"Finds file from within elder-path and inserts it here. 
After posting to comp.emacs.help, realize that could have simply used
the emacs-provided function locate-library."
  (if 
      (null path)
      (progn
	(error  
	 (concat "ELDER Error: File to be inserted " file " not found")
	 (show-position-my (point))))
    (let ((this-file (concat (car path) "/" file)))
      (if (file-exists-p this-file)
	  (elder-insert-stripped-file-contents this-file)
	(elder-insert-file-contents file (cdr path)))))
)

(defun elder-insert-stripped-file-contents (file)
  "Strips all occurences of comments in the file and inserts it.
The file, stripped of its comments is inserted at the current
\(point\)."
  (let 
      ((content 
	(with-temp-buffer
	  (insert-file-contents-literally file)
	  (el-strip-comments (buffer-name) *elder-comment*)
	  (buffer-substring (point-min) (point-max)))))
    (insert content))
)




(defun estyle (&rest args)
  "Adds .est to each filename, and inserts them at current position. 
Returns a string \"\". "
  (mapcar 
   (lambda (arg)
     ;;; AM i supposed to use insert-file-contents-literally here???
     (elder-insert-file-contents (concat arg ".est") (cons "." elder-path)))
   args)
  ""
)



(defun elder-message-my (&rest args)
  (shell-command (apply 'concat "echo " 
			(mapcar 
			 (lambda (arg)			    
			   (format "%S" (concat 
					 (if (stringp arg)
					     arg
					   (format "%S" arg)) " ")))
			 args))))

(defun space-p-my ()
  "Tells if the following character is a whitespace."
  (member (following-char) '(9 10 32)))

(defun elder-defaults ()
"Basic defaults for elder."

;   probably not needed any more since have made elder iterative now.
;  (if (< max-lisp-eval-depth 10000)
;      (setq max-lisp-eval-depth 10000)) ;;;increase it even more for
;				       ;;;longer documents. 
;  (if (< max-specpdl-size 20000) 
;      (setq max-specpdl-size 20000))
  (setq *elder-comment* "")
  (setq *elder-noticep* nil)
  (setq *elder-commentstripp* nil)
  (setq *elder-elder-commentstripp* nil)
  (setq *elder-begin* "([ebeg]")
  (setq *elder-end* "[eend])")
  (setq *elder-roughbuffer* "skjghj-elder-roughbuffer")
  (setq *elder-aliases* '() )
  (setq *elder-elbf* "([ebf]")
  (setq *elder-elef* "[eef])")
  (setq *elder-elff* "[eff]")
  (setq elder-path nil)
  (mapcar (lambda (element)
	    (add-to-list 'elder-path element))
	  (reverse (elder-getenv-my "ELDERPATH")))
  (ealias "[bck]"
	'(progn
	   (delete-backward-char 1)
	   "")
	"[del]"
	'(progn
	   (delete-backward-char -1) 
	   "")
	)

)

;;;====================================================
(defun ealias (&rest args)
  (elderaliaslist args))

(defun elderaliaslist (args)
  (unless (null args) 
    (progn
      (let ((aa (elder-replace-already-defined (list (car args) (cadr args))
					 *elder-aliases*)))
	(if aa (setq *elder-aliases* aa)
	  (setq *elder-aliases*
		(cons (list (car args) (cadr args)) *elder-aliases*))))
      (elderaliaslist (cddr args))))
)

;;;====================================================
(defun eunalias (&rest args)
  "Undefines all the arguments"
  (elderunalias args))

(defun elderunalias (args)
  (if (null args) nil
    (progn
      (setq *elder-aliases*
	    (apply 'append
		    (mapcar 
		     (lambda (element)
		       (if (string= (car args) (car element))
			   nil (list element)))
		     *elder-aliases*)))
      (elderunalias (cdr args)))))



;;;====================================================
(defun elder-replace-already-defined (newalias aliaslist)
"If already defined, returns a substituted aliases, else gives nil"
 (if (null aliaslist) nil
   (if (string= (car newalias) (caar aliaslist))
       (cons newalias (cdr aliaslist))
     (let ((testothers (elder-replace-already-defined
			newalias (cdr aliaslist))))
       (if (null testothers) nil
	 (cons (car aliaslist) testothers)))))
)


;;;====================================================
(defun el-strip-comments (workbuffer olcomment)
  "The strip-comments routine..
Compare to el-strip-comments-all though.."
  (set-buffer workbuffer)
  (let* ((comment (concat "
" olcomment))
	 (cl (length comment))
	 (eldone nil)
	 (curpos nil))
    (while (not eldone)
      (progn
	(goto-char (point-min))
	(setq curpos (search-forward comment nil t))
	(if (null curpos)
	    (setq eldone t)
	  (progn 
	    (setq curpos (- curpos cl))
	    (goto-char curpos)
	    (kill-line)
	    (kill-line)
	    ))))))

;;;====================================================
(defun el-strip-comments-all (workbuffer comment)
  "Strips ALL occurences of comments..
As opposed to el-strip-comments which strips only the lines that
start with a comment. This one strips everything until the end-of-line
after it sees a comment, even if the comment doesnot start at the
beginning of a line.."
 (set-buffer workbuffer) 
 (goto-char (point-min))
 (let ((cl (length comment)) (eldone nil) (curpos nil))
   (if (> cl 0)
       (while (not eldone)
	 (progn
	   (goto-char (point-min))
	   (setq curpos (search-forward comment nil t))
	   (if (null curpos)
	       (setq eldone t)
	     (progn
	       (setq curpos (- curpos cl))
	       (goto-char curpos)
	       (if (string= (buffer-substring (- curpos 1) curpos)
			    "
")
		   (progn (kill-line) (kill-line))
		 (kill-line))


	       ))))))
)
;;;====================================================

(defun elder-setnthcar-my (n ls expr)
  (if (> n 0) (cons (car ls) (elder-setnthcar-my (- n 1) (cdr ls) expr))
    (setcar ls expr) ls)
)

;;;====================================================

(defun etex (initbuf)
"Elder's Latex-specific invocation"
  (elder-defaults)
  (let* ((buffers (el-bufnames initbuf ".tex.e"))
	 (finalbuf (second buffers)))
    (setq *elder-comment* "%")
    (setq *elder-noticep* t)
    (setq *elder-commentstripp* t)
    (eldergeneral (car buffers) finalbuf)
))

(defun emat (initbuf)
"Elder's matlab-specific invocation"

  (elder-defaults)
  (let ((buffers (el-bufnames initbuf ".m.e"))
	(finalbuf (second buffers)))
    (setq *elder-comment* "%")
    (setq *elder-noticep* t)
    (setq *elder-commentstripp* t)
    (eldergeneral (car buffers) finalbuf)
))

(defun elder (initbuf)
 "ELDER for a general language. Use etex etc. for specific languages.
Use etex for Latex, emat for Matlab etc."
 (elder-defaults)
 (let ((buffers (el-bufnames initbuf ".e"))
       (finalbuf (second buffers)))
   (setq *elder-comment* "")
   (setq *elder-noticep* nil)
   (setq *elder-commentstripp* nil)
   (eldergeneral (car buffers) finalbuf)
   ))
;;;====================================================
(defun el-bufnames (initbuf ext)
"Internal to ELDER, sorts out proper buffer-names. 
Assumes that the argument ext ends in .e ."
  (if 
      (and
       (> (length initbuf) (length ext))
       (string= ext
		(substring initbuf (- (length initbuf) (length ext))
			   (length initbuf))))
      (list initbuf (substring initbuf 0 (- (length initbuf) 2)))
    (let ((newinit (concat initbuf ext)))
      (list newinit
	    (substring newinit 0 (- (length newinit) 2)))))
 )

;;;====================================================

;(defmacro elder-detailed-error (arg)
;  arg)


;;;===============================================================
(defun eldergeneral(initbuf finalbuf)

"Not recommended for end-user. Works on any language.  
End-user: Use elder instead
"
;;;(let ((el-tmp-buffer (current-buffer)))
  (save-excursion
   (let ((workbuffer (concat initbuf ".log")))
     (get-buffer-create workbuffer)
     (set-buffer workbuffer)
     (kill-region (point-min) (point-max))
     (get-buffer-create initbuf)
     (kill-buffer initbuf)
     (if (file-exists-p (concat default-directory initbuf))
       (find-file (concat default-directory initbuf))
       (error (concat "ELDER Error: Can't find your initial file " default-directory 
	      initbuf))
       )
     (if (file-exists-p (concat default-directory finalbuf))
	 (delete-file (concat default-directory finalbuf)))
     (copy-region-as-kill (point-min) (point-max))
     (set-buffer workbuffer)
     (auto-fill-mode -1) ; turns it off
     (insert "                                                    ")
     (insert "
")
     (insert (car kill-ring))
     (insert "                                                    ")
     (insert "
")
    ;;;these inserts of spaces help my search-es return no errors.

     (if *elder-commentstripp* 
	 (el-strip-comments workbuffer *elder-comment*))
     (write-file 
      (concat default-directory workbuffer) nil)


     (let ((processedp t))
       (while processedp
	 (setq processedp (el-substitute workbuffer))))


     (goto-char (point-min))
     (if *elder-noticep*
	 (progn
	   (insert *elder-comment*)
	   (insert "DO NOT EDIT THIS FILE! Your changes will be")
	   (insert " overwritten next\n")
	   (insert *elder-comment*)
	   (insert "time this file is automatically generated from")
	   (insert " its source-file \n")
	   (insert *elder-comment*)
	   (insert initbuf)
	   (insert " via ELDER. ")
	   (insert "Please go edit that file instead\n")
	   (insert *elder-comment*)
	   (insert "This File: ")
	   (insert finalbuf)
	   (insert " \n")
	   ))
   (write-file 
    (concat default-directory workbuffer) nil)
   (write-file 
    (concat default-directory finalbuf) nil)
   (kill-buffer finalbuf)
   )
  )
)


;;;====================================================

(defun stringize-my (arg)
  "Easy: Stringizes the argument."
 (format "%S" arg))
;;;====================================================

(defun el-findfirstelbf (workbuffer)
  (set-buffer workbuffer)
  (goto-char (point-min))
  (list
   (search-forward *elder-elbf* nil t)
   (list *elder-elbf* ""))
)
			       

;;;====================================================
(defun el-findfirstelderbegin (workbuffer)
  (set-buffer workbuffer)
  (goto-char (point-min))
  (list
   (search-forward *elder-begin* nil t) 
   (list *elder-begin* "")))
;;;====================================================
(defun el-findfirstalias (workbuffer)
"Returns the first elder-alias, found, along with the 2 matching
aliases." 
  (set-buffer workbuffer)
  (let* ((aliaslist (el-findaliaslist workbuffer *elder-aliases*))
	 (bestyet '(nil ("" ""))))
    (while (not (null aliaslist))
      (if (not (null (caar aliaslist)))
	  (if (or (null (car bestyet)) (> (car bestyet) (caar aliaslist)))
	      (setq bestyet (car aliaslist))))
      (setq aliaslist (cdr aliaslist)))
    bestyet)
)
;;;====================================================
(defun el-findaliaslist (workbuffer aliases)
  (if (null aliases) nil
    (cons 
     (list
      (progn
	(goto-char (point-min))
	(search-forward (caar aliases) nil t))
      (car aliases))
     (el-findaliaslist workbuffer (cdr aliases))))
)


;;;====================================================
(defun el-substitute (workbuffer)
"Internal to ELDER. "
  (set-buffer workbuffer) 
  (let ((change-made nil)		
	(firstalias (el-findfirstalias workbuffer))
	(firstelbegin (el-findfirstelderbegin workbuffer))
	(firstelbf (el-findfirstelbf workbuffer))
	firstfound cutregionend cutregionbegin)
  
    (unless (and (null (car firstalias)) (null (car firstelbegin))
		 (null (car firstelbf)))

;;;====================================================
      (setq change-made t)
      (setq firstfound
	    (if (not (null (car firstalias)))
		(if (or (null (car firstelbegin)) 
			(< (car firstalias) (car firstelbegin)))
		    (if (or (null (car firstelbf))
			    (< (car firstalias) (car firstelbf)))
			"alias"
		      "elbf")
		  (if (or (null (car firstelbf))
			  (< (car firstelbegin) (car firstelbf)))
		      "elbegin"
		    "elbf" ))
	      (if (null (car firstelbegin)) 
		  "elbf"
		(if (or (null (car firstelbf))
			(< (car firstelbegin) (car firstelbf)))
		    "elbegin" "elbf" ))))
      (if (string= firstfound "elbegin")
	  (progn 
;;;	    (elder-message-my "\nProcessing " (buffer-substring-my
;;;					 (car firstelbegin)
;;;					 (+ (car firstelbegin) 10)))
	    (goto-char (point-min))
	    (setq cutregionend
		  (search-matching-end-my 
		   *elder-begin* 1 *elder-end* 0
		   (search-forward *elder-begin* nil t)))
	    (if (null cutregionend)
		(error (concat "\nELDER Error: Missing " *elder-end*
			       " for the " *elder-begin* " found at "
			       "point " 
			       (format "%S" (car firstelbegin))
			       "-----\n"
			       "You might have missed some other"
"[eef], not necessarily this one.\n"
   "Look for dangling [eef]s with no ) \n"
			       "-----\n"
			       (show-position-my (car firstelbegin))
			     )))
	    (setq cutregionbegin (- (car firstelbegin) (length
							*elder-begin*)))



	    (goto-char cutregionbegin)
	    (let ((expressions (buffer-substring 
				(car firstelbegin)
				(- cutregionend (length *elder-end*))))
		  tobeevaluated)
	      (kill-region cutregionbegin cutregionend)
	      (elder-detailed-error
	       (with-temp-buffer
		 (insert "(setq tobeevaluated (quote (progn " 
			 expressions " )))")
		 (eval-buffer)))
	      (elder-detailed-error 
	       (let ((aa (eval tobeevaluated)))
		 (if (stringp aa)
		     (insert aa)
		   (if (numberp aa)
		       (insert (format "%S" aa)))))))

;          the stuff before i modified it:
; 	    (goto-char cutregionend)
; 	    (insert (el-execute workbuffer (car firstelbegin) 
; 				(- cutregionend (length *elder-end*))
; 				*elder-roughbuffer*))
; 	    (kill-region cutregionbegin cutregionend)

	    ))

      (if (string= firstfound "alias")
	  (progn
;	    (elder-message-my "\nProcessing " (buffer-substring-my
; 					 (car firstalias)
; 					 (+ (car firstalias) 10)))
	    (setq cutregionend (car firstalias))
	    (setq cutregionbegin (- cutregionend 
				    (length (caadr firstalias))))
	    (let ((aliasname
		   (buffer-substring cutregionbegin cutregionend)))
	      (kill-region cutregionbegin cutregionend)
	      (goto-char cutregionbegin)

	      (elder-detailed-error (insert 
			       (eval (cadadr firstalias))))
; 	        (condition-case err (eval (cadadr firstalias))
;		  (error (error
;			  (error-message-string err)
;			  "while evaluating alias"
;			  (show-position-my (point))))
	      )
	))
      (if (string= firstfound "elbf") 
	  (progn
;	    (elder-message-my "\nProcessing " (buffer-substring-my
;					 (car firstelbf)
;					 (+ (car firstelbf) 10)))
	  (goto-char (point-min))
	  (goto-char (search-forward *elder-elbf* nil t))
	  (setq cutregionend (search-matching-end-my *elder-elbf* 1 
						     *elder-elef* 0 (point)))

	  (if (null cutregionend)
	      (error (concat "\nELDER Error: Missing " *elder-elef* " for the "
			     *elder-elbf* " at point " 
			     (format "%S" (car firstelbf)) 
			     "-----\n"
			     "If not, some other [eef]) is missing.\n"
			     "Look for dangling [eef]s with no ) \n"
			     "-----\n"
			     (show-position-my (car firstelbf))
			     )))
	  (setq cutregionbegin (- (car firstelbf) (length
						  *elder-elef*)))
	  (goto-char (+ cutregionbegin (length *elder-elbf*))) 
	  (insert " "); now cutregion has increased by 1...
	  (setq cutregionend (+ 1 cutregionend))
	  (goto-char (+ cutregionbegin (length *elder-elbf*))) 
	  (forward-sexp 1)
	  (let ((function-string 
		 (buffer-substring 
		  (+ cutregionbegin (length *elder-elbf*)) (point)))
		(remainder-string 
		 (progn
		   (while (space-p-my)
		     (forward-char))
		   (if (string= 
			*elder-elff*
			(buffer-substring 
			 (point)
			 (+ (point) (length *elder-elff*))))
		       (goto-char (+ (point) (length *elder-elff*))))
		   (buffer-substring (point) 
				     (- cutregionend 
					(length *elder-elef*))))))
	    (goto-char cutregionend )
	    (insert " ")
	    (insert *elder-begin*)
	    (insert " ")
	    (insert "(apply")
	    (insert (concat "(quote " function-string ") "))
	    (insert "(quote (")
	    (insert (format "%S" remainder-string))
	    (insert " ))")
	    (insert ")")
	    (insert *elder-end*)
	    (kill-region cutregionbegin (+ cutregionend 1)) )))

      (if *elder-trace* 
	  (write-file 
	   (concat default-directory workbuffer) nil))
 
;      (set-buffer "tmpfile")
;      (goto-char (point-max))
;      (insert "\naliases:\n\n")
;      (goto-char (point-max))
;      (insert (format "%S" *elder-aliases*))
;      (goto-char (point-max))
;      (insert "\n\nworkbuffer:\n")
;      (goto-char (point-max))
;      (insert-buffer workbuffer)
;      (goto-char (point-max))
;      (insert "\n")
;      (set-buffer workbuffer)
;       the above is a debugging tool..				       

)

      change-made
))

;;;====================================================
; Not needed as of elder21.
; (defun el-execute (workbuffer startregion endregion roughbuffer)
;   (set-buffer workbuffer)
;   (let ((expressions (buffer-substring startregion endregion))
; 	(elder-tmp-result "") tmpstring)
;     (get-buffer-create roughbuffer)
;     (set-buffer roughbuffer)
;     (setq tmpstring (buffer-substring (point-min) (point-max)))
;     (kill-region (point-min) (point-max))
;     (goto-char (point-min))
;     (insert " (setq elder-tmp-result (progn ")
;     (insert expressions)
;     (insert " )) ")
;     (eval-buffer roughbuffer)
;     (kill-region (point-min) (point-max))
;     (goto-char (point-min))
;     (insert tmpstring)
;     (set-buffer workbuffer)
;     (if (integerp elder-tmp-result) 
; 	(setq elder-tmp-result (format "%S" elder-tmp-result)))
;     (if (numberp elder-tmp-result)
; 	(setq elder-tmp-result (format "%d" elder-tmp-result)))
;     (if (not (stringp elder-tmp-result))
; 	(setq elder-tmp-result ""))
;    (set-buffer workbuffer)
;     elder-tmp-result )
; )


;;;====================================================
(defun show-position-my (position)
 "Returns a string indicating buffer-contents near position.
Very useful as a error-position-indicator."
  (concat "\n"
   (buffer-substring-my (- position 60)
			(- position 0))
   "\n<--- ELDER WAS AT THIS POINT (Error) *********\n"
   (buffer-substring-my (+ position 0)
			(+ position 60)) "\n" )
)

;;;====================================================
(defun search-matching-end-my (beginstring bnumber endstring enumber startpt)
  "Tries to search for matching end.
Tries to mimic search-forward in that returns nil if not found."
  (if (<= bnumber enumber) 
      startpt
    (let ((nextbegin
	   (progn
	     (goto-char startpt)
	     (search-forward beginstring nil t)))
	  (nextend
	   (progn
	     (goto-char startpt)
	     (search-forward endstring nil t))))
      (if (null nextend) nil
	(if (or (null nextbegin) (> nextbegin nextend))
	    (search-matching-end-my beginstring bnumber endstring 
				    (+ enumber 1) nextend)
	  (search-matching-end-my beginstring (+ bnumber 1) endstring 
				  enumber nextbegin))))))


;;;====================================================
(defun buffer-substring-my (a b)
  (buffer-substring
   (if (< a (point-min)) (point-min) a)
   (if (> b (point-max)) (point-max) b)))

;;;====================================================
(defun search-matching-begin-my (beginstring bnumber endstring enumber startpt)
  "Tries to search for matching end.
Tries to mimic search-backward in that returns nil if not found.
Provided just for completeness though elder doesn't use it."
  (if (<= enumber bnumber) 
      startpt
    (let ((nextbegin
	   (progn
	     (goto-char startpt)
	     (search-backward beginstring nil t)))
	  (nextend
	   (progn
	     (goto-char startpt)
	     (search-backward endstring nil t))))
      (if (null nextbegin) nil
	(if (or (null nextend) (> nextbegin nextend))
	    (search-matching-begin-my beginstring (+ 1 bnumber) endstring 
				    enumber nextbegin)
	  (search-matching-begin-my beginstring bnumber endstring 
				  (+ 1 enumber) nextend))))))




;;;====================================================

(defun generaterandomstring (length)
  (if (zerop length)
      ""
    (concat "*&*"
     (format "%c" (+ 97 (random* 26)))
        ; we use random* since that seems to be truly random..
     (generaterandomstring (- length 1)))))

;;;====================================================



(defun eregionalias (string1 string2 pos1 pos2)
  "Optional extension. Allows aliasing within the 2 positions.
Achieves the effect that aliasing takes place only in certain regions
of the document.  Is not a necessary part of ELDER. Just uses ELDER to
build on it.  pos2 and pos1 are unique strings the user places
somewhere in the buffer. pos2 should occur *after* pos1.  If u decide
to use this function, please do not have any other aliases or keywords
named *&* or substrings thereof. If you do, there is a 1 in 26^(length
of yr alias) chance that yr alias might conflict with the random alias
being used by eregionalias.

"
  (let ((string3 (generaterandomstring 10)) 
	(pos3 (generaterandomstring 10)))
    (ealias string1 string3)
    (ealias pos2
	    (list 'progn
		  (list 'eunalias string1)
		  (list 'ealias string3 string1)
		  (list 'ealias pos1
			(list 'quote 
			      (list 'progn
				    (list 'eunalias string3)
				    "")))
		 pos3))
    (ealias pos3 
	    (list 'progn
		  (list 'ealias string3 string2)
		  ""))
    )
)
    


;;;====================================================
; 	        (condition-case err (eval (cadadr firstalias))
;		  (error (error
;			  (error-message-string err)
;			  "while evaluating alias"
;			  (show-position-my (point))))

;;;===============================================================
;;; ELDER allows user to write documents in any lanaguage/package they
;;; desire.  What's new about this, you ask? ELDER also allows them to
;;; use ELisp simultaneously. For instance, if you want Latex to
;;; calculate the average and mean-deviation of the grades you are
;;; entering for your class, you can't. But, an ELDER document uses
;;; Emacs's abilities to write in the answers before generating the
;;; <file>.tex..

;;; The procedure is simple. Just generate an ELDER file ending in e,
;;; for instance sample.tex.e, enclose everything you want Emacs to
;;; handle within <elderbegin> and <elderend>. Emacs will not touch
;;; anything enclosed within these demarkers. The remainder is a
;;; series of SEXP's for Emacs to handle. Each sexp returns a string
;;; which Emacs inserts right there. Given this much, you can now use
;;; Emacs' awesome power to do neat tricks.

;;; What's the need for this, you ask. An Emacs programmer, if they
;;; want can easily write a program that can generate a ".tex"
;;; document for him. My answer is that ELDER documents look much more
;;; like .tex documents, (my Emacs can colorcode them as .tex
;;; documents --- a program on the other hand, would, well, be an
;;; Emacs program, and atleast one of the disasdvantages is that all
;;; Latex commands will be encoded within inverted commmas and my
;;; Emacs will screw up their color-coding while dispalying. ). Coming
;;; back to the main point, a .tex file needs very little changes to
;;; convert it to a .tex.e file, in fact, it is a .tex.e file already!

;;; Thus, if you want Latex to print the square-root of a number, say
;;; r, then do it as follows:
;;;<elderbegin> 
;;;   (setq r 1.2)
;;;   (format "%d" (sqrt r))
;;;<elderend>


;;; Within each Emacs-region (the region enclosed by <elderbegin> and
;;; <elderend>, one expression is returned, the string returned by the
;;; last expression.

;;; The user may even define new functions.. etc. 

;;; There is one useful exception to the above rule. One utility ELDER
;;; provides is aliasing. This aliasing works not inside, but outside
;;; the EMACS region. Thus, if you want to use "\beq" instead of
;;; "\begin{equation}" in Latex (which does not work in Latex --->
;;; as you may see for yourself by trying
;;; \newcommand{\beq}{\begin{equation} and seeing that it does not
;;; work). 
;;; But it is very easy in
;;; ELDER---->

;;;Just use:
;;;<elderbegin>
;;;(ealias '("\beq" "\begin{equation}" "\eeq" "\end{equation"))
;;;""
;;;<elderend>

;;; The above aliases beq to begin{equation} and \eeq to
;;; \end{equation}. Emacs will now blindly substitute for \beq and
;;; \eeq wherever it sees them. There's no checking for whitespace
;;; etc. because we don't want to specialize to Latex. The user can
;;; also call functions via these aliases. For instance, 
;;; <elderbegin>
;;; (ealias "/number" '(concat "4" "4") "numm" (concat "4" "4"))
;;; 
;;; "" 
;;; <elderbegin>

;;;Henceforth, whenever Emacs sees /number, it will call the function
;;;(concat 4 4). But now look at the second one numm . This one leads
;;;to emacs *calling* concat *while* defining the alias "numm".The
;;;function, of course needs to return a string.  Of course, there is
;;;also provided an "eunalias" function, which unaliases all its
;;;arguments. See README..  You see, so far, Elder is completely
;;;independent of what document it wants to write. ELDER can be used
;;;to write documents for absolutely any language!!!
;;; 
;;; By the way, ELDER works recursively. It keeps making passes
;;; through the document starting from the beginning, until it finds
;;; no more passes to make. Each successful pass yields only one
;;; change.  Thus, if you EMACS expressions generate "\beq", they will
;;; be subsequently changes to "\begin{equation}". I shall try to
;;; guarantee that ELDER always executes the first matching
;;; expression. If you run ELDER through an EMACS environment, you
;;; might actually be able to see ELDER at work if you are dealing
;;; with a large enough document.

;;; The user does not need anything more than this. He now has
;;; abbgreviations and can write any functions s/he wants using
;;; elderbegin etc. But if s/he insists on ability to provide new
;;; functions *outside* the Emacs regions (so far, we are just
;;; providing aliasing), he can do that very easily using aliasing and
;;; buffer-reading functions. His function woulld read and delete from
;;; (current-buffer) the expression following the function-name. 

;;; Thus, a clever user might be able to define a funciton 
;;; \doublefrac{}{} which doubles its first argument before printing
;;; it out. Thus \doublefrac{3.2}{2} would print what would be printed
;;; by \frac{6.4}{2}. [Needless to say, LATEX cannot do that.]

;;; Let me illustrate the need for ELDER via more examples:


;;; Many packages do not contain things we like. Even if they do
;;; provide such things, we emacs users don't have to use their
;;; roundabout ways. 

;;; Consider what happens if you ask Matlab to numerically solve this
;;; 5th order differential equation:
;;; s=1; solve('x^5 + s*x - x/s=0');

;;;the stupid Matlab says: 's' unknown. Fortunately, matlab provides a
;;;workaround through eval (there may be better workarounds that i
;;;don't know of, but that's not the point)--->
;;; So, a workaround is: 
;;; eval(concat(2,'solve(''x^5+',num2str(s),'*x-x/',num2str(s),'=0'')'))

;;;Even so, i don't want to figure out and write this huge expression
;;;every time. The user can just define and then use his own new
;;;elfunction esubstitute.
;;;solve(<elderbegin>
;;;            (esubstitute 'x^5-s*x-x/s=0' s)
;;;       <elderend>)

;;; This esubstitute would sunstitute the value of "s" into the
;;; expression everywhere in the resulting .m file. This would work
;;; even if matlab didn't provide for the eval workaround.

;;; Or the user can define a function esolve which converts its
;;; argument to the above above huge eval expression..

;;;The idea is to use emacs's awesome text-manipulation powers to
;;;provide such things as variables in latex. This would be a
;;;front-end to *any* package. The user creates a <file>.<ext>.e and
;;;Latex converts it to <file>.<ext>.

;;; Finally, if you are sure there will be no conflicts, be sure to
;;; shorten the <elderbegin> and <elderend> expressions to whatever
;;; you like, for instance <elb> and <ele> in the following example of
;;; a valid ELDER file sample.my.e

;;;; <elderbegin>
;;;; (setq *elder-begin* "<elb>")
;;;; (stq *elder-end* "<ele>")
;;;; (ealias vs "velocity-shear")
;;;; ""
;;;; <elderend>
;;;; The value of vs is <elb> (* 2 3 4) <ele>

;;; The above file, when processed by ELDER yields a file sample.my
;;; that says:
;;;; The value of velocity-shear is 24.

;;; An ELDER expression expects a string as the result. If it gets an
;;; integer or real, it converts it to string. If it gets something
;;; else, it ignores it (by returning ""). You may often use ELDER
;;; expressions to initiate aliases etc. in which case you want to
;;; return "". While omission of "" will often work, it is a good
;;; ELDER etiquette to always end such ELDER expressions with "".

;;; Notice the importance of returning "" at the end of the first
;;; ELDER call above. Also, the second ELDER call doesnot return a
;;; string, but ELDER tries to live with it, and converts it to string.

;;; See LOGFILE for more details..