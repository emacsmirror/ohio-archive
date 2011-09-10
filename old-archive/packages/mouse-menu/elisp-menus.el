;;;; The menus for emacs-lisp-mode.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 13:56:49 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'elisp-menus)

(if (eq window-system 'x)
    ;; Set up the single menu usable under X at the moment...

(defXmenu 'emacs-lisp-menu
  '("Emacs Lisp Menu"
    ("Emacs Lisp Menu"
     ("Eval defun" call-interactively 'eval-defun)
     ("Eval region" call-interactively 'eval-region)
     ("Eval buffer" eval-current-buffer)
     ("Eval last s-expression" call-interactively 'eval-last-sexp)
     ("Eval expression" call-interactively 'eval-expression)
     ("Print buffer" lpr-buffer)
     ("Print region" call-interactively 'lpr-region)
     ("Describe function" call-interactively 'describe-function)
     ("Describe variable" call-interactively 'describe-variable)
     ("Other Menus" x-mouse-other-menus))))

(defHCImenu emacs-lisp-eval-menu
  ("defun" eval-defun nil)
  ("region" call-interactively 'eval-region)
  ("buffer" eval-current-buffer)
  ("last s-expression" call-interactively 'eval-last-sexp)
  ("expression" call-interactively 'eval-expression))

(defHCImenu emacs-lisp-describe-menu
  ("function" call-interactively 'describe-function)
  ("variable" call-interactively 'describe-variable))

(defHCImenu emacs-lisp-menu
  ("Emacs Lisp Menu")
  ("Eval" . emacs-lisp-eval-menu)
  ("Print buffer" lpr-buffer)
  ("Print region" call-interactively 'lpr-region)
  ("Describe" . emacs-lisp-describe-menu)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )
