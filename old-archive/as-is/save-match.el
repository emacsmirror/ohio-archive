;;;From: tale@pawl.rpi.edu (David C Lawrence)
;;;From: quiroz@cs.rochester.edu (Cesar Quiroz)
;;;From: kjones@talos.UUCP (Kyle Jones)

(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list
     'let (list (list original '(match-data)))
     (list 'unwind-protect
           (cons 'progn body)
           (list 'store-match-data original)))))
(put 'save-match-data 'lisp-indent-hook 0)
