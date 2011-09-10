;To: unix-emacs@bbn.com
;Date: 17 Dec 88 00:33:48 GMT
;From: Jerry Jackson <esosun!jackson%freyja.css.gov@seismo.css.gov>
;Subject: lisp editing commands
;

;;;Here's some code I've been using for a long time for editing lisp.  It has
;;;completely replaced the mouse for cutting and pasting.  It also has the
;;;advantage of being useful on a terminal.
;;;
;;;There are two main functions:
;;;
;;;loop-word and loop-grab -- I have them bound to C-o and M-o
;;;
;;;
;;;They work sort of like an in-place incremental search.  As you type,
;;;atoms (if you are using loop-word) or lists (loop-grab) that match what
;;;you have typed get copied to point.  As long as the current atom or list
;;;continues to match, it stays there, but as soon as it fails to match,
;;;the next matching item comes down.  If you hit <esc> at any time, you
;;;automatically go to the next item.  If you hit <space>, <lf>, <cr>, or ')'
;;;(as well as C-g :-), you leave the mode.  When working with lists
;;;in loop-grab, if you want to keep matching past a space, hit a ';' to 
;;;match the space.  
;;;
;;;Besides being useful for cut & paste, these functions make a nice 
;;;redo/fix/history mechanism for use within a lisp interpreter.  Also,
;;;you never have to type a long variable name more than once.  It is
;;;much faster than mouse action, particularly if you want to grab something
;;;that is not currently visible on the screen. 


(defun lisp-word (name)
  (let ((here (point)))
    (setq *redo-start* here)
    (setq *word-wrapped* nil)
    (setq *tried-list* nil)
    (setq *failed* nil)
    (setq *last-redo* (cons (point) (concat "\\( \\|\t\\|\n\\|(\\|)\\)" name)))
    (setq *word-prefix-length* (- (length (cdr *last-redo*))
				  (length name)))
    (setq *old-redo* (car *last-redo*))
    (re-search-backward (cdr *last-redo*) nil t)
    (if (not (eql (point) (car *last-redo*)))
	(move-text here)
      (setq *word-wrapped* t)
      (rplaca *last-redo* (point-max))
      (goto-char (car *last-redo*))
      (re-search-backward (cdr *last-redo*) nil t)
      (if (or (eql (point) *old-redo*)
	      (< (point) *redo-start*))
	  (word-redo-fail here)
	(move-text here)))))
  

(defun next-lisp-word ()
  (set-mark (point))
  (backward-sexp)
  (delete-region (point) (mark))
  (let ((here (point)))
    (goto-char (car *last-redo*))
    (re-search-backward (cdr *last-redo*) nil t)
    (while (and (memstring (get-sexp) *tried-list*)
		(not (eql (point) (car *last-redo*))))
      (rplaca *last-redo* (point))
      (re-search-backward (cdr *last-redo*) nil t))
    (if (eql (point) (car *last-redo*))
	(if *word-wrapped*
	    (word-redo-fail here)
	  (setq *word-wrapped* t)
	  (rplaca *last-redo* (point-max))
	  (goto-char here)
	  (insert "()")
	  (next-lisp-word))
      (if (and *word-wrapped* (< (point) *redo-start*))
	  (word-redo-fail here)
	(move-text here)))))


(defun move-text (position)
  (let (temp)
    (setq *old-redo* (car *last-redo*))
    (setq *last-redo* (cons (point) (cdr *last-redo*)))
    (forward-char 1)
    (mark-sexp 1)
    (setq temp (buffer-substring (mark) (point)))
    (goto-char position)
    (insert temp)
    (setq *tried-list* (cons temp *tried-list*))))


(defun word-redo-fail (position)
  (message "no more.")
  (setq *failed* t)
  (goto-char position)
  (insert (substring (cdr *last-redo*)
		     *word-prefix-length*
		     (length (cdr *last-redo*))))
  (end-of-line))

(defun get-sexp ()
  (if (>= (point) (point-max))
      ""
    (forward-char 1)
    (mark-sexp 1)
    (prog1 (buffer-substring (point) (mark))
      (backward-char 1))))

(defun memstring (x l)
  (cond ((null l) nil)
	((string-equal (car l) x) t)
	(t (memstring x (cdr l)))))

(defun loop-word ()
  (interactive)
  (let ((key (read-char)))
    (if (eq key ?\*)
	(lisp-word "\\*")
      (lisp-word (char-to-string key)))
    (while (and (not *failed*)
		(not (eq (setq key (read-char)) ?\ ))
		(not (eq key ?\n))
		(not (eq key ?\r))
		(not (eq key ?\))))
      (cond ((eq key ?\*)
	     (rplacd *last-redo* (concat (cdr *last-redo*)
					 "\\" (char-to-string key)))
	     (setq *tried-list* (cdr *tried-list*)))
	    ((eq key ?\e))
	    (t
	     (setq *last-redo* (cons *old-redo*
				    (concat (cdr *last-redo*)
					    (char-to-string key))))
	     (setq *tried-list* (cdr *tried-list*))))
      (next-lisp-word))
    (cond ((or (eq key ?\n) (eq key ?\r))
	   (funcall (key-binding (char-to-string key))))
	  ((eq key ?\ )
	   (insert " "))
	  ((eq key ?\))
	   (insert ")")
	   (backward-char 1)
	   (balance-pars)
	   (forward-char 1)))))


(defun grab-sexp-as-string ()
  (let ((here (point)))
    (backward-up-list 1)
    (set-mark (point))
    (forward-list)
    (prog1 (buffer-substring (mark) (point))
      (goto-char here))))


(defun lisp-grab (name)
  (let ((here (point)))
    (setq *redo-start* here)
    (setq *grab-wrapped* nil)
    (setq *failed* nil)
    (setq *tried-list* nil)
    (setq *last-redo* (cons (point) (concat "(" name)))
    (setq *old-redo* (car *last-redo*))
    (search-backward (cdr *last-redo*) nil t)
    (if (not (eql (point) (car *last-redo*)))
	(move-grab here)
      (setq *grab-wrapped* t)
      (rplaca *last-redo* (point-max))
      (goto-char (car *last-redo*))
      (search-backward (cdr *last-redo*) nil t)
      (if (or (eql (point) *old-redo*)
	      (< (point) *redo-start*))
	  (redo-fail here)
	(move-grab here)))))
  

(defun next-lisp-grab ()
  (set-mark (point))
  (backward-sexp)
  (delete-region (point) (mark))
  (let ((here (point)))
    (goto-char (car *last-redo*))
    (search-backward (cdr *last-redo*) nil t)
    (while (and (memstring (get-non-atomic-sexp) *tried-list*)
		(not (eql (point) (car *last-redo*))))
      (rplaca *last-redo* (point))
      (search-backward (cdr *last-redo*) nil t))
    (if (eql (point) (car *last-redo*))
	(if *grab-wrapped*
	    (redo-fail here)
	  (setq *grab-wrapped* t)
	  (rplaca *last-redo* (point-max))
	  (goto-char here)
	  (insert "()")
	  (next-lisp-grab))
      (if (and *grab-wrapped* (< (point) *redo-start*))
	  (redo-fail here)
	(move-grab here)))))



(defun move-grab (position)
  (let (temp)
    (setq *old-redo* (car *last-redo*))
    (setq *last-redo* (cons (point) (cdr *last-redo*)))
    (forward-char 1)
    (setq temp (grab-sexp-as-string))
    (goto-char position)
    (insert temp)
    (setq *tried-list* (cons temp *tried-list*))))

(defun redo-fail (position)
  (message "no more.")
  (setq *failed* t)
  (goto-char position)
  (insert (cdr *last-redo*))
  (end-of-line))


(defun get-non-atomic-sexp ()
  (mark-sexp 1)
  (buffer-substring (point) (mark)))


(defun loop-grab ()
  (interactive)
  (let (key)
    (lisp-grab (char-to-string (read-char)))
    (while (and (not *failed*)
		(not (eq (setq key (read-char)) ?\n))
		(not (eq key ?\r))
		(not (eq key ?\ ))
		(not (eq key ?\))))
      (cond ((eq key ?\;)
	     (setq *last-redo* (cons *old-redo*
				    (concat (cdr *last-redo*) " ")))
	     (setq *tried-list* (cdr *tried-list*)))
	    ((eq key ?\e))
	    (t
	     (setq *last-redo* (cons *old-redo*
				    (concat (cdr *last-redo*)
					    (char-to-string key))))
	     (setq *tried-list* (cdr *tried-list*))))
      (next-lisp-grab))
    (cond ((or (eq key ?\n) (eq key ?\r))
	   (funcall (key-binding (char-to-string key))))
	  ((eq key ?\ )
	   (insert " "))
	  ((eq key ?\))
	   (insert ")")
	   (backward-char 1)
	   (balance-pars)
	   (forward-char 1)))))



;+-----------------------------------------------------------------------------+
;|   Jerry Jackson                       UUCP:  seismo!esosun!jackson          |
;|   Geophysics Division, MS/22          ARPA:  esosun!jackson@seismo.css.gov  |
;|   SAIC                                SOUND: (619)458-4924                  |
;|   10210 Campus Point Drive                                                  |
;|   San Diego, CA  92121                                                      |
;+-----------------------------------------------------------------------------+
;
