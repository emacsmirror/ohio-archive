;;; $Header: /home/user3/miles/src/elisp/RCS/gensym.el,v 1.2 1992/04/16 13:43:43 miles Exp $

(provide 'gensym)

(defvar gensym-counter 0)

(defun gensym (&optional pfx)
  (prog1 (make-symbol
	  (concat (if pfx
		      (if (symbolp pfx) (symbol-name pfx) pfx)
		      "G")
		  (int-to-string gensym-counter)))
    (setq gensym-counter (1+  gensym-counter))))
