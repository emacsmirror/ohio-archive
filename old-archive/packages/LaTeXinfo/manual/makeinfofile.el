(if (null (getenv "LATEXINFO"))
  (error "You must define the environment variable LATEXINFO first."))

(defun compile-if-necessary (file)
  (let ((filename (expand-file-name
		   (concat (file-name-as-directory (getenv "LATEXINFO"))
			   file))))
    (if (file-newer-than-file-p filename (concat filename "c"))
	(byte-compile-file filename)
      (message "%s is up to date." filename))
    ))

(compile-if-necessary "latexinfo.el")
(load-file "$LATEXINFO/latexinfo.elc")
(mapcar 'compile-if-necessary '("latexinfo-mode.el"
				"get-node.el"
				"macsyma-fmt.el"
				"head-fmt.el"
				"scheme-fmt.el"
				"nodify.el"))

(find-file "MANUAL.tex")
(latexinfo-format-buffer t)
(save-some-buffers t)
(kill-emacs 0)
