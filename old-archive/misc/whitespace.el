;; whitespace.el --- strip trailing whitespace from buffers
;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Created: 1995-02-11
;; Public domain

;; $Id: whitespace.el,v 1.2 1995/03/08 18:36:32 friedman Exp $
;; LCD Archive Entry:
;; whitespace|Noah Friedman|friedman@gnu.ai.mit.edu|
;; strip trailing whitespace from buffers|
;; 11-Feb-1995|1.2|~/misc/whitespace.el.gz|

(defvar nuke-trailing-whitespace-p
  (function
   (lambda ()
     (cond
      ((memq major-mode '(ada-mode
                          c++-mode
                          c-mode
                          change-log-mode
                          emacs-lisp-mode
                          fortran-mode
                          latex-mode
                          lisp-interaction-mode
                          lisp-mode
                          mail-mode
                          nroff-mode
                          perl-mode
                          plain-tex-mode
                          prolog-mode
                          scheme-mode
                          sgml-mode
                          tcl-mode
                          slitex-mode
                          texinfo-mode))
       t)
      ((memq major-mode '(rmail-mode vm-mode))
       nil)
      (t 'query))))
  "*Specify when stripping whitespace should be done.
This variable affects how the function `nuke-trailing-whitespace' behaves.
If `t', unreservedly strip trailing whitespace.
If `nil', do nothing.
If a symbol, query for each instance.

If a function, call it to decide what to do.
This function is called once and should return `t', `nil', or the symbol
`query' to decide what to do.  Note that this function must be placed on
the hook directly; you cannot specify the name of the function since
that is just a symbol.

This variable is made buffer-local when set in any fashion.")

(make-variable-buffer-local 'nuke-trailing-whitespace-p)


(defun nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

This function uses `nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is `t', this function will query for
replacement if the buffer is read-only."
  (interactive)
  (let ((buffer-orig-read-only buffer-read-only)
        (buffer-read-only nil)
        (flag nuke-trailing-whitespace-p))
    (and nuke-trailing-whitespace-p
         (functionp nuke-trailing-whitespace-p)
         (setq flag (funcall nuke-trailing-whitespace-p)))
    (and flag
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (save-match-data
               (cond ((and (eq flag t)
                           (not buffer-orig-read-only))
                      (while (re-search-forward "[ \t]+$" (point-max) t)
                        (delete-region (match-beginning 0) (match-end 0))))
                     (t
                      (query-replace-regexp "[ \t]+$" ""))))))))
  ;; always return nil, in case this is on write-file-hooks.
  nil)

;(add-hook 'mail-send-hook 'nuke-trailing-whitespace)
;(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

(provide 'whitespace)

;; whitespace.el ends here
