;To: unix-emacs@BBN.COM
;Date: 13 Jun 89 19:18:53 GMT
;From: CS.ROCHESTER.EDU!koomen@eddie.mit.edu
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: electric-minibuffer.el
;Source-Info:  From (or Sender) name not authenticated.
;
;Here's a nice utility I came across recently:

;;; electric-minibuffer.el

;;; Add -yank-next- and -yank-previous- commands to completing reads in
;;; minibuffer.

;;; This is an extension to the completing-read commands in the
;;; minibuffer.  Hitting M-n (M-p) will yank the next (previous) default
;;; value into the buffer, from the current list of completions.  For
;;; example, suppose you execute the switch-to-buffer command, but the
;;; default "other buffer" is not the one you desire.  Instead of typing
;;; the desired buffer's name, just hit M-n until the desired buffer's
;;; name appears in the minibuffer, and hit return.  This also works
;;; with any command which reads a function or variable name, or a
;;; programmer specified completion list.  It doesn't work, however,
;;; with functions which read a file name.

(provide 'electric-minibuffer)

(define-key minibuffer-local-completion-map "\en"
  'minibuffer-yank-next-completion)
(define-key minibuffer-local-completion-map "\ep"
  'minibuffer-yank-previous-completion)
(define-key minibuffer-local-must-match-map "\en"
  'minibuffer-yank-next-completion)
(define-key minibuffer-local-must-match-map "\ep"
  'minibuffer-yank-previous-completion)

(defvar minibuffer-last-default nil)
; If minibuffer-completion-table is an obarray, then the index in that
; array of the default value to be read.  If it is an assoc list, then
; the string value of the default value.
; For optimal performance, any function which reads from the
; minibuffer and accepts a default value should stuff that value in
; minibuffer-last-default before reading.
; E.g., (read-buffer prompt default) should stuff default into this
; variable.  But even if this is not done these functions work well enough.

(defun assoc-tail (element list)
  "returns tail of list whose caar matches element"
  (let ((continue t))
    (while continue
      (cond
       ((null list) (setq continue nil))
       ((equal element (car (car list)))
	(setq continue nil))
       (t (setq list (cdr list)))))
    list))

(defun mod+ (x y mod)
  "add X and Y and take MOD"
  (setq x (+ y x))
  (if (>= x mod) (setq x (- x mod)))
  (if (< x 0) (setq x (+ x mod)))
  x)

(defun minibuffer-yank-next-completion (&optional incr)
  "Replace input by next possible default input"
  (interactive)
  (or incr (setq incr 1))
  (erase-buffer)
  (if (arrayp minibuffer-completion-table)
      (let (next orig (len (length minibuffer-completion-table)))
	(if (not (and (numberp minibuffer-last-default)
		      (>= minibuffer-last-default 0)
		      (< minibuffer-last-default len)))
	    (progn
	      (setq next 0)
	      (setq orig (- len 1)))
	  (progn
	    (setq orig minibuffer-last-default)
	    (setq next (mod+ orig incr len))))
	(while (and minibuffer-completion-predicate
		    (not (= next orig))
		    (not
		     (condition-case err
			 (funcall minibuffer-completion-predicate
				  (aref minibuffer-completion-table next))
		       (error nil))))
	  (setq next (mod+ next incr len)))
	(setq minibuffer-last-default next)
	(insert-string
	 (prin1-to-string (aref minibuffer-completion-table next))))
    (let ((table (if (< incr 0)
		     (reverse minibuffer-completion-table)
		   minibuffer-completion-table))
	  next orig remainder)
      (setq orig (assoc-tail minibuffer-last-default table))
      (setq next (cdr orig))
      (if (null next) (setq next table))
      (while (and minibuffer-completion-predicate
		  (not (eq next orig))
		  (not (condition-case err
			   (funcall minibuffer-completion-predicate (car next))
			 (error nil))))
	(setq next (cdr next))
	(if (null next) (setq next table)))
      (setq minibuffer-last-default (car (car next)))
      (insert-string minibuffer-last-default))))

(defun minibuffer-yank-previous-completion ()
  "Replace input by previous possible default input"
  (interactive)
  (minibuffer-yank-next-completion -1))

