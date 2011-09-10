(defun latexinfo-format-dots ()
  (latexinfo-discard-command)
  (insert "..."))

(put 'Macsyma 'latexinfo-format 'latexinfo-format-Macsyma)
(defun latexinfo-format-Macsyma ()
  (latexinfo-parse-noarg)
  (insert "Macsyma"))

(put 'ExampleInFile 'latexinfo-format 'latexinfo-ExampleInFile)
(defun latexinfo-ExampleInFile ()
  (let ((arg (latexinfo-parse-arg-discard)))
    (insert
     "There is an example in the file: \"" arg "\".")))

(put 'DemoInFile 'latexinfo-format 'latexinfo-DemoInFile)
(defun latexinfo-DemoInFile ()
  (let ((arg (latexinfo-parse-arg-discard)))
      (insert
       "There is a demo in the file: \"" arg "\".")))

(put 'UsageInFile 'latexinfo-format 'latexinfo-UsageInFile)
(defun latexinfo-UsageInFile ()
  (let ((arg (latexinfo-parse-arg-discard)))
    (insert
     "There are some usage notes in the file: \"" arg "\".")))


(put 'f 'latexinfo-format 'latexinfo-format-noop)
(put 'fb 'latexinfo-format 'latexinfo-format-findex-plus)
(defun latexinfo-format-findex-plus ()
  (latexinfo-index-plus 'latexinfo-findex))

(put 'v 'latexinfo-format 'latexinfo-format-noop)
(put 'vb 'latexinfo-format 'latexinfo-format-vindex-plus)
(defun latexinfo-format-vindex-plus ()
  (latexinfo-index-plus 'latexinfo-vindex))

(defun latexinfo-index-plus (indexvar)
  (let ((arg (latexinfo-parse-expanded-arg)))
    (latexinfo-discard-command)
    (insert arg)
    (set indexvar
	 (cons (list arg latexinfo-last-node)
	       (symbol-value indexvar)))))


(put 'o 'latexinfo-format 'latexinfo-format-noop)
(put 'concept  'latexinfo-format 'latexinfo-format-noop)
(put 'filen 'latexinfo-format 'latexinfo-format-code)
(put 'flushr 'latexinfo-format 'latexinfo-format-noop)

(put 'pcp 'latexinfo-format 'latexinfo-discard-line-with-args)
