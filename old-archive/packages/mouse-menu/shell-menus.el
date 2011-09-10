;;;; The Shell mode menus.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 13:59:35 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'shell-menus)

(if (eq window-system 'x)

(defXmenu 'shell-menu
  '("Process Menu"
    ("Process Menu"
     ("Copy previous command" copy-last-shell-input)
     ("Show last output" show-output-from-shell)
     ("Flush last output" kill-output-from-shell)
     ("Suspend process" stop-shell-subjob)
     ("Restart process" continue-shell-subjob)
     ("Interrupt process" interrupt-shell-subjob)
     ("Quit process" quit-shell-subjob)
     ("Describe this mode" describe-mode)
     ("Other Menus" x-mouse-other-menus))))

(defHCImenu shell-signal-menu
  ("interrupt signal" interrupt-shell-subjob)
  ("stop signal" stop-shell-subjob)
  ("quit signal" quit-shell-subjob))

(defHCImenu shell-menu
  ;; This menu is inherited by all shell-mode buffers, e.g.
  ;; the inferior lisps and prologs, and the TeX shell.
  ("Process Menu")
  ("Copy previous command" copy-last-shell-input)
  ("Show last output" show-output-from-shell)
  ("Flush last output" kill-output-from-shell)
  ("Send process" . shell-signal-menu)
  ("Describe this mode" describe-mode)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )
