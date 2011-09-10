;;;From: nz@wucs.UUCP
;;;Subject: useful GNU command (for hackers)
;;;Date: 8 Apr 86 15:25:57 GMT
;;;Keywords: lisp gnu useful?

;;;	Here are a pair of GNU Elist routines that I find very useful.
;;;I keep them in my .emacs (byte-compiled, of course) but I may put them
;;;into lisp/site-init.el next time I recompile a GNU.

;;;--------------
;; install tops-20 type control-meta calling  ( ^Z = C-M- )
(defun C-M-command (char)
 "execute control-meta command as in old emacs 
	take argument char, controlify it, and execute it
	from esc-map"
    (interactive "cC-M-")
    (if (< ?! char)   (setq char (- (upcase char) 64)))
    (let ((def (lookup-key esc-map (char-to-string char))))
      (if def
        (command-execute def))))

;; Command to evaluate the sexp you just typed and replace it by its value.
;; for instance, 
;;      BEFORE       the total cost is $ (+ (* 67 210) (* 12 500)) _
;;
;;      AFTER        the total cost is $ 20070 _
;; nz@wucs.UUCP 4/2/86
(defun reduce ()
  "Evaluate elisp sexp before point and replace it with its value"
  (interactive)
  (backward-sexp 1)
  (save-excursion
    (let (sfront sback)
      (setq sfront (point))
      (forward-sexp 1)
      (setq sback (point))
      (eval-print-last-sexp nil)
      (delete-backward-char 1) (backward-sexp 1) (delete-backward-char 1)
      (delete-region sfront sback)
    ))
    (forward-sexp 1)
  )
;;;-----------
;;;-- 
;;;...nz (Neal Ziring at WU ECL  -  we're here to provide superior computing.)

;;;	{seismo,ihnp4,cbosgd}!wucs!nz   OR   nz@wucs.UUCP

;;;    "You could get an infinite number of wires into this !*$$#!?! junction 
;;;                         box, but we usually don't go that far in practice"
;;;				--   Employee of London Electricity Board, 1959
