;;;Date: 6 Dec 88 14:19:36 GMT
;;;From: bbn.com!sboisen@bbn.com (Sean Boisen)
;;;Subject: Lisp-syntax structure hiding
;;;Reply-To: sboisen@bbn.com

;;;A number of people wanted the code, so i figured i'd best post it.
;;;Again, credit where due: i didn't write this, i got it from Dan
;;;LaLiberte, along with the following caveat: since the end-of-comment
;;;terminator is \n and this replaces those with \r, you can get into
;;;trouble with code like this if you evaluate it while hidden

;;;(defun fn ()
;;;	;; this comment causes problems
;;;	(do-something))

;;;because the (do-something) gets commented out! 

;;;One other problem stems from the fact that subst-char-in-region
;;;modifies the buffer (despite its documentation): i added a workaround
;;;for this but i haven't made this work with read-only buffers (though
;;;it should be trivial). Other than that :-) ....


;;;Yours for software sharing,
;;;Sean Boisen

;;;........................................
;;;Sean Boisen -- sboisen@bbn.com
;;;BBN Systems and Technologies Corporation, Cambridge MA
;;;Disclaimer: these opinions void where prohibited by lawyers.



;;;------------------------------ cut here ------------------------------
;;Date: Sat, 3 Dec 88 14:08:15 cst
;;From: Daniel LaLiberte <liberte@M.CS.UIUC.EDU>
;;To: sboisen@IZAR.BBN.COM
;;Subject: Re:  elisp structure editing
;;
;;There are two ways to use selective display.  It can be used to
;;hide everything indented past some number of leading whitespaces,
;;or to hide only those lines that are preceded by CR.  My functions,
;;below, only use the latter.  They need some work, but maybe you
;;will be inspired to do that.  Also, keep in mind the problem with
;;comments.  You may suggest to RMS that comments should end with
;;either \n or \r; that would solve the problem.
;;
;;dan
;;===
;;I called the file hide-sexp.el.
;;===

(defun hide-sexp ()
  "Hide the lines within the current sexp."
  (interactive)
  (setq selective-display t)
  (save-excursion
    (backward-up-list 1)
    (hide-or-show-region
      (save-excursion
	(beginning-of-line)
	(point))
      (progn
	(forward-sexp 1)
	(point))
      ?\r)))

;; i added this one: sboisen 12/5/88
(defun hide-this-sexp ()
  "Hide the lines within the sexp starting at point."
  (interactive)
  (setq selective-display t)
  (save-excursion
    (forward-sexp 1)
    (forward-sexp -1)
    (hide-or-show-region
      (save-excursion
	(beginning-of-line)
	(point))
      (progn
	(forward-sexp 1)
	(point))
      ?\r)))

(defun show-sexp ()
  "Show the lines within the current sexp."
  (interactive)
  (setq selective-display t)
  (save-excursion
    (backward-up-list 1)
    (hide-or-show-region
     (save-excursion
       (beginning-of-line)
       (point))
     (progn
       (forward-sexp 1)
       (point))
     ?\n)))

(defun hide-sub-sexps ()
  "Hide all the sub sexps in the current form."
  (interactive)
  (save-excursion
    (condition-case err
	(progn
	  (backward-up-list 1)
	  (down-list 1)
	  (forward-sexp 1)		; after function name
	  (while t			; run til end of list
	    (forward-sexp 1)
	    (hide-or-show-region 
	      (progn
		(forward-sexp -1) (point))
	      (progn
		(forward-sexp 1) (point))
	      ?\r))
	  )
      (error nil))))
	  

(defun show-all ()
  "Show any hidden text in buffer."
  (interactive)
  (save-excursion
    (hide-or-show-region (progn (goto-char (point-min)))
			 (progn (goto-char (point-max)))
			 ?\n)))

(defun hide-or-show-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is \\n (newline character) then text is shown, while if FLAG is \\r
\(control-M) the text is hidden. Also twiddles the buffer-modified
flag to make up for a deficiency (in the documentation at least) of
subst-char-in-region."
  (let ((modifiedp (buffer-modified-p)))
    (subst-char-in-region from to
			  (if (= flag ?\n) ?\r ?\n)
			  flag 'noundo)
    ;; added by sboisen 12/6/88: might also want to try to handle
    ;; read-only buffers...
    (set-buffer-modified-p modifiedp))
  )
