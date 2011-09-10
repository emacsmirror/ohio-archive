;;;; The menus for lisp-mode.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 13:58:12 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'lisp-menus)

(if (eq window-system 'x)

(defXmenu 'lisp-menu
  '("Lisp Menu"
    ("Lisp Menu"
     ("Send defun" call-interactively 'lisp-send-defun)
     ("Send region" call-interactively 'lisp-send-region)
     ("Send buffer" call-interactively 'lisp-send-buffer)
     ("Print region" call-interactively 'lpr-region)
     ("Print buffer" lpr-buffer)
     ("Justify current defun" indent-defform)
     ("Describe Lisp mode" describe-mode)
     ("Other Menus" x-mouse-other-menus))))
	    
(defHCImenu lisp-send-menu
  ("defun" call-interactively 'lisp-send-defun)
  ("region" call-interactively 'lisp-send-region)
  ("buffer" call-interactively 'lisp-send-buffer))

(defHCImenu lisp-menu
  ("Lisp Menu")
  ("Send" . lisp-send-menu)
  ("Justify current defun" indent-defform)
  ("Print buffer" lpr-buffer)
  ("Print region" call-interactively 'lpr-region)
  ("Describe Lisp mode" describe-mode)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )

