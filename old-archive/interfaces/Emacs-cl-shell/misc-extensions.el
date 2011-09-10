;;;; This is a file of useful miscellaneous extensions to emacs.
;;;; NOTE: it does not bind any keys -- it only provides function
;;;; definitions.  To use it, you should put lines into your .emacs
;;;; file which will load the file and then setup the desired
;;;; keybindings.  See example.emacs.

;;;; ---------------- Miscellaneous Command definitions -----------------

;;; Nice little function which rebinds keys which call old-function to
;;; call new-function.
(defun rebind-keys-which-call (old-function new-function &optional keymap)
  (if keymap
      (mapcar '(lambda (key) (define-key keymap key new-function))
	      (where-is-internal old-function keymap))
      (mapcar '(lambda (key) (global-set-key key new-function))
	      (where-is-internal old-function))))

;;; Have emacs call the right kind of fill command (i.e.
;;; fill-paragraph or indent-sexp), depending on whether it is on a
;;; comment line or a code line.  To do make this work properly, you
;;; must set the paragraph-start and paragraph-separate variables
;;; properly (see example.emacs).
(defun lisp-fill-paragraph (&optional justify-p)
  "Fill-paragraph command for lisp-mode that knows how to fill
both comments and code.  To use this, you need to set up the
regular expressions contained in the buffer-variables paragraph-start 
and paragraph-separate correctly."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "[ \t]*;")          ;if comment
	   (let ((old-fill-prefix fill-prefix)
		 (start (point)))
	     (re-search-forward ";+ ?")    ;count semicolons
;;	     (untabify start (point))	;convert tabs to spaces.
	     (setq fill-prefix (buffer-substring start (point)))
	     (unwind-protect
		  (fill-paragraph justify-p)
	       (setq fill-prefix old-fill-prefix))))
	  (t (forward-char 1)		;if on defun, don't go backward
	     (beginning-of-defun)
	     (indent-sexp)))))

;;; Expand all symlinks in the filename.  Useful for lisp source file
;;; names, since Lucid Common Lisp expands all symlinks when recording
;;; source files.
(defun expand-symlinks (file &optional position prev-position)
  (if (null position)			;first call
      (setq file (expand-file-name file)
	    position (or (string-match "/" file 1) (length file))
	    prev-position 0))
  (let ((e-file (file-symlink-p (substring file 0 position))))
    (cond (e-file
	   (setq e-file (directory-file-name e-file)) ;strip trailing "/"
	   (expand-symlinks (concat (substring file 0 (1+ prev-position))
				    e-file
				    (substring file position))))
	  ((> (length file) position)
	   (expand-symlinks file
			    (or (string-match "/" file (1+ position)) (length file))
			    position))
	  (t file))))

;;; Redefine this (from simple.el) to get rid of newlines along with
;;; tabs and spaces.
(defun my-just-one-space ()
  "Delete all spaces, tabs and newlines around point, leaving one space."
  (interactive "*")
  (skip-chars-backward " \t\n")
  (if (= (following-char) ? )		; space?
      (forward-char 1) 
      (insert ? ))
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(defun delete-forward-whitespace ()
  "Delete all spaces, tabs and newlines after cursor."
  (interactive)
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

;;; Non-nil optional third arg to set-window-start would inhibit point motion.
(defun reposition-defun-at-top ()
  "Put current defun at top of window."
  (interactive)
  (set-window-start
   (get-buffer-window (current-buffer))
   (save-excursion
     (end-of-defun)
     (beginning-of-defun)
     (point)))) 

(defun reposition-point-at-top ()
  "Scroll contents of current window so that point is at top."
  (interactive)
  (recenter 0))

;;; Grep for symbol nearest point in files *.lisp.  Should modify this
;;; to remember the directory you last grepped in...
(defun cl-grep-for-symbol (grep-args)
  (interactive 
   (list (read-string "Run grep: "
		      (let ((default (find-tag-default)))
			(if default
			    (concat "\"" default "\"" " *.lisp")
			    "")))))
   (grep grep-args))

;;; The most useful mouse binding I've seen!  Copies the sexp under
;;; the mouse to the point.  This is from the TMC emacs extensions.
(defun x-mouse-paste-sexp (arg)
  "Copies sexp containing mouse position to point."
  (let ((current-window (selected-window))
	(current-buffer (current-buffer)))
    (if (x-mouse-select arg)
	(let ((temp
	       (save-excursion 
		 (x-mouse-set-point arg)
                 ;; get the point to the beginning of this symbol if
                 ;; we are sitting in the middle of one
		 (let ((syntax-of-buffer-char 
			(if (= (point) (point-min)) 0
			    (char-syntax
			     (string-to-char
			      (buffer-substring (1- (point)) (point)))))))
		   (if (or (= syntax-of-buffer-char (char-syntax ?x))
			   (= syntax-of-buffer-char (char-syntax ?:)))
		       (forward-sexp -1)))
		 (let ((old-point (point)))
		   (forward-sexp 1)
		   (buffer-substring old-point (point))))))
	  ;; This code doesn't work for the minibuffer.  It'd be
	  ;; really nice if it did.
	  (select-window current-window)
	  (switch-to-buffer current-buffer)
	  (let ((before-inserted-text (point)))
	    ;; maybe put in a blank space
	    (let* ((buffer-char
		    (if (= (point) (point-min))
			nil
			(string-to-char (buffer-substring (1- (point)) (point)))))
		   (syntax-of-buffer-char (if buffer-char (char-syntax buffer-char))))
	      (if (and buffer-char (or (= syntax-of-buffer-char (char-syntax ?x))
				       (= syntax-of-buffer-char (char-syntax ?:))
				       (= buffer-char ?\))))
		  (insert " ")))
	    (let ((temp1 (point)) temp2)
	      (insert temp)
	      (setq temp2 (point-marker)) ;use a marker since indent-sexp will add chars
	      (goto-char temp1)
	      (indent-sexp)
	      (goto-char temp2))
	    (set-mark before-inserted-text))))))

;;; Useful debugging macro for emacs-lisp code.
(defmacro print-db (form &optional pop-up)
  "Insert FORM and its evaluated value at the end of the *debug* buffer.
If optional second arg POP-UP is non-nil, display the buffer.  Returns 
evaluated value of FORM." 
  (list 'let (list (list 'val form)
		   '(buf (current-buffer))
		   '(db-buf (get-buffer-create "*debug*")))
	(list 'if pop-up '(pop-to-buffer db-buf) '(set-buffer db-buf))
	'(goto-char (point-max))
	(list 'insert (list 'format "%s  %s\n" (list 'quote form) 'val))
	'(set-buffer buf)
	'val))

;;;; ---------------- Mouse commands -----------------

;;; For back-compatibility: this was the old TMC name.
(defun x-mouse-paste-sexp (arg)
  (x-copy-sexp arg))

;;; This one is the most useful mouse binding I've seen!  Copies the
;;; sexp under the mouse to the point.  This is modified slightly from
;;; the TMC emacs extensions.  If the sexp is followed by a newline
;;; character, it is included in the copy.
(defun x-copy-sexp (arg)
  "Copy sexp (and trailing newline) containing the mouse to the point."
  (let ((current-window (selected-window))
	(current-buffer (current-buffer)))
    (if (x-mouse-select arg)
	(let ((sexp (save-excursion
		      (x-mouse-set-point arg)
		      (get-surrounding-sexp))))
	  (select-window current-window)
	  (switch-to-buffer current-buffer)
	  (insert-indented-sexp sexp)))))

(defun x-move-sexp (arg)
  "Move sexp (and trailing newline) containing the mouse to the point."
  (let ((current-window (selected-window))
	(current-buffer (current-buffer)))
    (if (x-mouse-select arg)
	(let ((sexp (save-excursion
		      (x-mouse-set-point arg)
		      (get-surrounding-sexp 'kill))))
	  (select-window current-window)
	  (switch-to-buffer current-buffer)
	  (insert-indented-sexp sexp)))))

;;; *** Should we also have an x-swap-sexps functions?  No.  Too hairy.
(defun x-replace-sexp (arg)
  "Replace sexp under point with sexp containing mouse."
  (let ((current-window (selected-window))
	(current-buffer (current-buffer)))
    (if (x-mouse-select arg)
	(let ((sexp (save-excursion
		      (x-mouse-set-point arg)
		      (get-surrounding-sexp))))
	  (select-window current-window)
	  (switch-to-buffer current-buffer)
	  (get-surrounding-sexp 'kill)
	  (insert-indented-sexp sexp)))))

(defun x-zap-sexp (arg)
  "Cut and wipe the sexp (and trailing newline) containing the mouse."
  (let ((current-window (selected-window))
	(current-buffer (current-buffer)))
    (if (x-mouse-select arg)
	(let ((sexp (save-excursion
		      (x-mouse-set-point arg)
		      (get-surrounding-sexp 'kill))))
	  (select-window current-window)
	  (switch-to-buffer current-buffer)))))

(defun get-surrounding-sexp (&optional kill)
  "Returns a string containing the surrounding sexp, including the
trailing carriage return (if there is one).  If kill is non-nil, delete
the string and indent the remaining line according to major mode."
  (save-excursion 
    ;; get the point to the beginning of this symbol if
    ;; we are sitting in the middle of one
    (let ((syntax-of-buffer-char 
	   (if (= (point) (point-min)) 0
	       (char-syntax
		(string-to-char
		 (buffer-substring (1- (point)) (point)))))))
      (if (or (= syntax-of-buffer-char (char-syntax ?x))
	      (= syntax-of-buffer-char (char-syntax ?:)))
	  (forward-sexp -1)))
    (let ((old-point (point))
	  sexp)
      (forward-sexp 1)
      ;(if (looking-at "\n") (forward-char 1))
      (setq sexp (buffer-substring old-point (point)))
      (if (not kill)
	  nil
	(delete-region old-point (point))
	(indent-according-to-mode))
      sexp)))

;;; This code doesn't work for the minibuffer.  It'd be really nice if
;;; it did.
(defun insert-indented-sexp (sexp)
  "Insert sexp at point, indenting according to major mode.  Set mark 
before inserted text, and leave point after."
  (let ((before-inserted-text (point)))
    ;; maybe put in a blank space
    (let* ((buffer-char
	    (if (= (point) (point-min))
		nil
		(string-to-char (buffer-substring (1- (point)) (point)))))
	   (syntax-of-buffer-char (if buffer-char (char-syntax buffer-char))))
      (if (and buffer-char (or (= syntax-of-buffer-char (char-syntax ?x))
			       (= syntax-of-buffer-char (char-syntax ?:))
			       (= buffer-char ?\))))
	  (insert " ")))
    (let ((temp1 (point)) temp2)
      (insert sexp)
      (setq temp2 (point-marker))	;use a marker since indent-sexp will add chars
      (goto-char temp1)
      (indent-sexp)
      (goto-char temp2)
      ;(indent-according-to-mode)
      )
    (set-mark before-inserted-text)))

(defun x-line-to-top (arg)
  "Scrolls the line at the mouse to the top of the window."
  (let* ((current-window (selected-window))
	 (current-buffer (current-buffer))
	 relative-coordinate)
    (save-excursion
      (setq relative-coordinate (x-mouse-select arg)))
    (if relative-coordinate
	(scroll-up (car (cdr relative-coordinate))))))

(defun x-line-to-middle (arg)
  "Scrolls the line at the mouse to the middle of the window."
  (let* ((current-window (selected-window))
	 (current-buffer (current-buffer))
	 relative-coordinate)
    (save-excursion
      (setq relative-coordinate (x-mouse-select arg)))
    (if relative-coordinate
	(scroll-down (- (/ (window-height) 2)
			(car (cdr relative-coordinate))
			1)))))

(defun x-line-to-bottom (arg)
  "Scrolls the line at the mouse to the bottom of the window."
  (let* ((current-window (selected-window))
	 (current-buffer (current-buffer))
	 relative-coordinate)
    (save-excursion
      (setq relative-coordinate (x-mouse-select arg)))
    (if relative-coordinate
	(scroll-down (- (window-height)
			(car (cdr relative-coordinate))
			2)))))


