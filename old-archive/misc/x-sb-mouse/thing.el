;; Thing.el
;; adapted from sun-fns.el by Joshua Guttman, MITRE.  
;; Comments appreciated: guttman@mitre.org


(provide 'thing)

(defun thing-boundaries (here)
  "Return start and end of text object at HERE using syntax table and thing-boundary-alist.  
Thing-boundary-alist is a list of pairs of the form (SYNTAX-CHAR FUNCTION)
where FUNCTION takes a single position argument and returns a cons of places
 (start end) representing boundaries of the thing at that position.  
Typically:
 Left or right Paren syntax indicates an s-expression.	
 The end of a line marks the line including a trailing newline. 
 Word syntax indicates current word. 
 Symbol syntax indicates symbol.
 If it doesn't recognize one of these it selects just the character HERE."
  (interactive "d")
  (if (save-excursion
	(goto-char here)
	(eolp))
      (thing-get-line here)
    (let* ((syntax
	    (char-syntax (char-after here)))
	   (pair
	    (assq syntax thing-boundary-alist)))
    
      (if pair
	  (funcall (car (cdr pair)) here)
	(cons here (1+ here))))))  


(defvar thing-boundary-alist
  '((?w thing-word)
    (?_ thing-symbol)
    (?\( thing-sexp-start)
    (?\$ thing-sexp-start)
    (?' thing-sexp-start)
    (?\" thing-sexp-start)
    (?\) thing-sexp-end)
    (?  thing-whitespace))
  "*List of pairs of the form (SYNTAX-CHAR FUNCTION) used by THING-BOUNDARIES.")
  
(defun thing-get-line (here)
  "Return whole of line HERE is in, with newline unless at eob."
  (save-excursion
    (goto-char here)
    (let* ((start (progn (beginning-of-line 1)
			 (point)))
	   (end (progn (forward-line 1)
		       (point))))
      (cons start end))))

(defun thing-word (here)
  "Return start and end of word at HERE."
  (save-excursion
    (goto-char here)
    (forward-word 1)
    (let ((end (point)))
      (forward-word -1)
      (cons (point) end))))

(defun thing-symbol (here)
  "Return start and end of symbol at HERE."
  (let ((end (scan-sexps here 1)))
    (cons (min here (scan-sexps end -1))
	  end)))

(defun thing-sexp-start (here)
  "Return start and end of sexp starting HERE."
  (cons here (scan-sexps here 1)))

(defun thing-sexp-end (here)
  "Return start and end of sexp ending HERE."
  (cons (scan-sexps (1+ here) -1)
	(1+ here)))

(defun thing-whitespace (here)
  "Return start to end of all but one char of whitespace HERE, unless 
there's only one char of whitespace.  Then return start to end of it."
  (save-excursion
    (let ((start (progn
		   (skip-chars-backward " \t") (1+ (point))))
	  (end (progn 
		 (skip-chars-forward " \t") (point))))
      (if (= start end)
	  (cons (1- start) end)
	(cons start end)))))





(defun mark-thing-at-point (here)
  "Set point at beginning and mark at end of text object using syntax table.
See thing-boundaries for definition of text objects"
  (interactive "d")
  (let ((bounds (thing-boundaries here)))
    (goto-char (cdr bounds))
    (set-mark-command nil)
    (goto-char (car bounds))))

(defun kill-thing-at-point (here)
  "Kill text object using syntax table.
See thing-boundaries for definition of text objects"
  (interactive "d")
  (let ((bounds (thing-boundaries here)))
    (kill-region (car bounds) (cdr bounds))))


(defun copy-thing-at-point (here)
  "Copy text object using syntax table.
See thing-boundaries for definition of text objects"
  (interactive "d")
  (let ((bounds (thing-boundaries here)))
    (copy-region-as-kill (car bounds) (cdr bounds))))

;;; Two X-related fns.  	
;;;	    
;;;(defun x-mouse-kill-thing (arg)
;;;  "Kill text object at point or mouse position and insert into window system cut buffer.
;;;Save in Emacs kill ring also."
;;;  (interactive "d")
;;;  (setq last-command nil)
;;;  (x-mouse-set-point arg)
;;;  (let* ((bounds (thing-boundaries (point)))
;;;	 (start (car bounds))
;;;	 (end (cdr bounds)))
;;;    (x-store-cut-buffer (buffer-substring start end))
;;;    (kill-region start end)))
;;;
;;;(defun x-mouse-copy-thing (arg)
;;;  "Copy text object at point or mouse position into window system cut buffer.
;;;Save in Emacs kill ring also."
;;;  (save-excursion
;;;    (save-window-excursion
;;;      (setq last-command nil)
;;;      (x-mouse-set-point arg)
;;;      (let* ((bounds (thing-boundaries (point)))
;;;	     (start (car bounds))
;;;	     (end (cdr bounds)))
;;;	(x-store-cut-buffer (buffer-substring start end))
;;;	(copy-region-as-kill start end)))))

