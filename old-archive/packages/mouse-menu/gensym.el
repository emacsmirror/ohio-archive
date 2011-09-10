;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Tue Aug 23 17:02:57 1988
;;;; Gensym for Emacs Lisp. If you don't know what it is, you don't need it.

(provide 'gensym)

;;; Oh, for the want of closures.

(defvar gensym-string-index-alist nil "Where gensym hides its dirty deeds.")

(defun gensym (&optional prefix)
  "Return a unique symbol with (optional) string or symbol prefix, e.g.
	(gensym) => a02886-1
	(gensym) => a02886-2
	(gensym \"foo\") => foo-1
	(progn (setq foo-2 nil) (gensym \"foo\")) => foo-3
	(gensym 'bar) => bar-1
Will not clobber any existing symbol. 
Functional? Of course it's not functional."
  (let* ((prefix (or prefix (make-temp-name "")))
	 (last-index (assoc prefix gensym-string-index-alist))
	 (gensym-index (if last-index (cdr last-index) 1))
	 (symbol (format "%s-%d" prefix gensym-index)))
    (while (intern-soft symbol)
      (setq gensym-index (1+ gensym-index)
	    symbol (format "%s-%d" prefix gensym-index)))
    (setq gensym-string-index-alist
	  (cons (cons prefix gensym-index)
		(delq last-index gensym-string-index-alist)))
    (intern symbol)))


