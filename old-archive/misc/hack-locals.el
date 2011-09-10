; Newsgroups: dg.ml.emacs.vm
; From: mike-w@cs.aukuni.ac.nz
; Subject: Re: inhibit-local-variables on mailboxes again
; Organization: University of Auckland, New Zealand.
; Date: 24 Jun 91 10:10:07
; 
;     Here's a modified version of Carl Witty's modified version of
;   hack-local-variables (Dan suggested I pass it on).  It supports two
;   additional values for inhibit-local-variables:
; 
;       'always	means never obey "Local Variables" section, and don't
;                 bother querying the user.  (I thought that setting this
;                 value in vm-mode-hooks would solve Dan's problem, but
;                 unfortunately vm explicitly let-binds
;                 inhibit-local-variables to t.)
; 
;       'query-other
;             	means only bother querying the user if the file is owned by
;             	someone else (helps prevent Trojan Horses without getting
;             	in the way too much).
; 
; --- CUT HERE --------------------------------------------------------------
;; LCD Archive Entry:
;; hack-locals|Carl Witty,Mike Williams
;; |cwitty@csli.stanford.edu,mike-w@cs14b.cs.aukuni.ac.nz
;; |Show local variables when present, more options for inhibit-local-variables
;; |91-04-26||~/misc/hack-locals.el.Z

;; Modified by cwitty@csli.Stanford.EDU (Carl Witty) on 12/4/88
;; to show "Local Variables" section of file on screen before
;; asking whether to use them (when inhibit-local-variables set).

;; Mike Williams (mike-w@cs14b.cs.aukuni.ac.nz) - Fri Apr 26 15:08:12 1991 
;; inhibit-local-variables can have the values
;;    nil 		... always set
;;    'query, t		... always query
;;    'query-other	... query if not file's owner
;;    'always		... always inhibit

(provide 'hack-locals)

(defun hack-local-variables (&optional force)
  "Parse, and bind or evaluate as appropriate, any local variables
for current buffer."
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (and 
	 (let ((case-fold-search t)) (search-forward "Local Variables:" nil t))
	 (or force (not inhibit-local-variables)
	     (and (eq inhibit-local-variables 'query-other) 
		  buffer-file-name
		  (eq (nth 2 (file-attributes (buffer-file-name))) 
		      (user-uid)))
	     (if (eq inhibit-local-variables 'always) nil
	       (save-window-excursion
		 (switch-to-buffer (current-buffer) t)
		 (recenter 0)
		 (y-or-n-p 
		  (format 
		   "Set local variables as specified at end of %s? "
		   (file-name-nondirectory buffer-file-name)))))))
	(let ((continue t)
	      prefix prefixlen suffix beg)
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix (buffer-substring (point)
					     (progn (end-of-line) (point)))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))
	  (if prefix (setq prefixlen (length prefix)
			   prefix (regexp-quote prefix)))
	  (if suffix (setq suffix (regexp-quote suffix)))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display (re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (error "Local variables entry is missing the prefix")))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (setq beg (point))
	    (skip-chars-forward "^:\n")
	    (if (eolp) (error "Missing colon in local variables entry"))
	    (skip-chars-backward " \t")
	    (let* ((str (buffer-substring beg (point)))
		   (var (read str))
		  val)
	      ;; Setting variable named "end" means end of list.
	      (if (string-equal (downcase str) "end")
		  (setq continue nil)
		;; Otherwise read the variable value.
		(skip-chars-forward "^:")
		(forward-char 1)
		(setq val (read (current-buffer)))
		(skip-chars-backward "\n")
		(skip-chars-forward " \t")
		(or (if suffix (looking-at suffix) (eolp))
		    (error "Local variables entry is terminated incorrectly"))
		;; Set the variable.  "Variables" mode and eval are funny.
		(cond ((eq var 'mode)
		       (funcall (intern (concat (downcase (symbol-name val))
						"-mode"))))
		      ((eq var 'eval)
		       (if (string= (user-login-name) "root")
			   (message "Ignoring `eval:' in file's local variables")
			 (eval val)))
		      (t (make-local-variable var)
			 (set var val))))))))))

;;=== END of hack-locals.el ===============================================
