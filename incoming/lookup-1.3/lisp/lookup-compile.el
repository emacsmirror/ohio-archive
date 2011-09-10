;;; lookup-compile.el --- byte-compile elisp files

(if (string< emacs-version "19.29")
    (setq command-line-args-left (cdr command-line-args-left)))

(defvar lookup-optional-packages '("bitmap" "browse-url"))

(let ((load-path (cons "." load-path))
      (byte-compile-warnings nil)
      (lookup-byte-compile t))
  (mapcar 'load command-line-args-left)
  (mapcar (lambda (file) (load file t)) lookup-optional-packages)
  (if (not (boundp 'lookup-compile-directory))
      (batch-byte-compile)
    (mapcar (lambda (file)
	      (byte-compile-file
	       (expand-file-name file lookup-compile-directory)))
	    command-line-args-left)))

;;; lookup-compile.el ends here
