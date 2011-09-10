;;;; Menu definitions for Allegro Common Lisp.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Wed May  9 18:01:39 1990 

(if (eq window-system 'x)
    (progn
      (require 'x-menus)
      (require 'x-cl-fns))
  (require 'hci-menus))
(provide 'cl-menus)

(if (eq window-system 'x)

    (progn

(defXmenu 'cl-menu
  '("Allegro Common Lisp Menu"
    ("Eval Menu"
     ("Eval defun at point" call-interactively 'fi:lisp-eval-defun)
     ("Eval region" call-interactively 'fi:lisp-eval-region)
     ("Eval buffer" call-interactively 'fi:lisp-eval-current-buffer)
     ("Compile defun at point" prefix-arg-supplied 'fi:lisp-eval-defun)
     ("Compile region" prefix-arg-supplied 'fi:lisp-eval-region)
     ("Compile buffer" prefix-arg-supplied 'fi:lisp-eval-current-buffer))
    ("Help Menu"
     ("Complete symbol at point" call-interactively 'fi:lisp-complete-symbol)
     ("Describe symbol" call-interactively 'fi:lisp-describe)
     ("Display CLtL manual entry" fi:clman)
     ("Display source code" call-interactively 'fi:lisp-find-tag)
     ("Display function documentation"
      call-interactively 'fi:lisp-function-documentation)
     ("Display argument list" call-interactively 'fi:lisp-arglist)
     ("Display all callers" call-interactively 'fi:lisp-who-calls)
     ("Display macroexpansion" fi:lisp-macroexpand)
     ("Display full macroexpansion" call-interactively 'fi:lisp-walk)
     ("Display full macroexpansion (compiler style)"
      prefix-arg-supplied 'fi:lisp-walk))
    ("Print Menu"
     ("Print defun"
      save-excursion
      (mark-defun) (enscript-region (region-beginning) (region-end)))
     ("Print region" enscript-region (region-beginning) (region-end))
     ("Print buffer" enscript-buffer)
     ("Show Printer Queue" lpq))
    ("Miscellaneous Menu"
     ("Justify current defun" indent-defform)
     ("Describe Common Lisp mode" describe-mode)
     ("Describe Key Bindings" describe-bindings)
     ("Other Menus" x-mouse-other-menus))))

(defXmenu 'cl-process-menu
  '("Allegro Inferior Common Lisp Menu"
    ("Command Menu"
     ("Previous command" call-interactively 'fi:pop-input)
     ("Next command" call-interactively 'fi:push-input)
     ("List command ring" call-interactively 'fi:list-input-ring)
     ("Search command ring forwards"
      call-interactively 'fi:re-search-forward-input)
     ("Search command input backwards"
      call-interactively 'fi:re-search-backward-input)     
     ("Flush command" fi:subprocess-kill-input)
     ("Move to start of last output" fi:subprocess-show-output)
     ("Flush last output" fi:subprocess-kill-output))
    ("Signal Menu"
     ("Send End-Of-File signal" fi:subprocess-send-eof)
     ("Send Suspend signal" fi:subprocess-suspend)
     ("Send Interrupt signal" fi:subprocess-quit)
     ("Send Quit signal" fi:subprocess-quit)
     ("Send Kill signal" fi:subprocess-kill))
    ("Help Menu"
     ("Complete symbol at point" call-interactively 'fi:lisp-complete-symbol)
     ("Describe symbol" call-interactively 'fi:lisp-describe)
     ("Display CLtL manual entry" fi:clman)
     ("Display source code" call-interactively 'fi:lisp-find-tag)
     ("Display function documentation"
      call-interactively 'fi:lisp-function-documentation)
     ("Display argument list" call-interactively 'fi:lisp-arglist)
     ("Display all callers" call-interactively 'fi:lisp-who-calls)
     ("Display macroexpansion" fi:lisp-macroexpand)
     ("Display full macroexpansion" call-interactively 'fi:lisp-walk)
     ("Display full macroexpansion (compiler style)"
      prefix-arg-supplied 'fi:lisp-walk))
    ("Print Menu"
     ("Print defun" save-excursion (mark-defun) (enscript-region))
     ("Print region" enscript-region (region-beginning) (region-end))
     ("Print buffer" enscript-buffer)
     ("Show Printer Queue" lpq))
    ("Miscellaneous Menu"
     ("Describe Inferior Common Lisp mode" describe-mode)
     ("Describe Key Bindings" describe-bindings)
     ("Other Menus" x-mouse-other-menus)))))
	    
(defHCImenu cl-eval-menu
  ("Eval Menu")
  ("Eval defun at point" call-interactively 'fi:lisp-eval-defun)
  ("Eval region" call-interactively 'fi:lisp-eval-region)
  ("Eval buffer" call-interactively 'fi:lisp-eval-current-buffer)
  ("Compile defun at point" fi:lisp-eval-defun t)
  ("Compile region" fi:lisp-eval-region t)
  ("Compile buffer" fi:lisp-eval-current-buffer t))

(defHCImenu cl-help-menu
  ("Help Menu")
  ("Complete symbol at point" call-interactively 'fi:lisp-complete-symbol)
  ("Describe symbol" call-interactively 'fi:lisp-describe)
  ("Display CLtL manual entry" fi:clman)
  ("Display source code" call-interactively 'fi:lisp-find-tag)
  ("Display function documentation"
   call-interactively 'fi:lisp-function-documentation)
  ("Display argument list" call-interactively 'fi:lisp-arglist)
  ("Display all callers" call-interactively 'fi:lisp-who-calls)
  ("Display macroexpansion" fi:lisp-macroexpand)
  ("Display full macroexpansion" call-interactively 'fi:lisp-walk)
  ("Display full macroexpansion (compiler style)" fi:lisp-walk t))

(defHCImenu cl-print-menu
  ("Print Menu")
  ("Print defun" progn (mark-defun) (lpr-region))
  ("Print region" call-interactively 'lpr-region)
  ("Print buffer" lpr-buffer)
  ("Show Printer Queue" lpq))

(defHCImenu cl-menu
  ("Allegro Common Lisp Menu")
  ("Eval" . cl-eval-menu)
  ("Help" . cl-help-menu)
  ("Print". cl-print-menu)
  ("Justify current defun" indent-defform)
  ("Describe Common Lisp mode" describe-mode)
  ("Describe Key Bindings" describe-bindings)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )

