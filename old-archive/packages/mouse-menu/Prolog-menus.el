;;;; The Prolog mode menus.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 14:00:43 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'Prolog-menus)

(if (eq window-system 'x)
    
(defXmenu 'prolog-menu
  '("Prolog Menu"
    ("Prolog Menu"
     ("Consult region" call-interactively 'prolog-consult-region)
     ("Consult buffer" call-interactively 'prolog-consult-buffer)
     ("Compile region" call-interactively 'prolog-compile-region)
     ("Compile buffer" prolog-compile-buffer)
     ("Print region" call-interactively 'lpr-region)
     ("Print buffer" lpr-buffer)
     ("Describe Prolog mode" describe-mode)
     ("Other Menus" x-mouse-other-menus))))
	  
(defHCImenu prolog-consult-menu
  ("region" call-interactively 'prolog-consult-region)
  ("buffer" call-interactively 'prolog-consult-buffer))

(defHCImenu prolog-compile-menu
  ("region" call-interactively 'prolog-compile-region)
  ("buffer" prolog-compile-buffer))

(defHCImenu prolog-menu
  ("Prolog Menu")
  ("Consult" . prolog-consult-menu)
  ("Compile" . prolog-compile-menu)
  ("Describe Prolog mode" describe-mode)
  ("Print buffer" lpr-buffer)
  ("Print region" call-interactively 'lpr-region)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )
