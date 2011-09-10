;;; $Header: /home/user3/miles/src/elisp/RCS/setf-autoloads.el,v 1.1 1992/04/16 13:37:35 miles Exp $

(defun %autoloads (file macrop funcs)
  (while funcs
    (autoload (car funcs) file nil nil macrop)
    (setq funcs (cdr funcs))))

(%autoloads "setf" nil
	    '(has-setf-method-p get-setf-method))
(%autoloads "setf" t
	    '(setf psetf shiftf rotatef
	      swap incf decf push pop
	      defsetf define-setf-metthod define-modify-macro
	      with-setf-method setf-update-form))

(autoload 'gensym "gensym")

(%autoloads "letopt" nil
	    '(letopt-maybe-optimize-let letopt-maybe-progn))

(autoload 'getf "plist" nil nil 'also-setf)
(autoload '%putf "plist")

(%autoloads "strhash" t '(strhash-get strhash-map make-strhash-table))
