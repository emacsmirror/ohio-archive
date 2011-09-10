;;;; Text mode menus
;;;; George R. S. Weir, Scottish HCI Centre, <george@uk.ac.strath.hci>.
;;;; Thu May 12 14:06:12 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'text-menus)

(if (eq window-system 'x)

(defXmenu 'text-menu
  '("Text Menu"
    ("Text Menu"
     ("Transpose chars" call-interactively 'transpose-chars)
     ("Transpose words" call-interactively 'transpose-words)
     ("Kill line" kill-line)
     ("Yank" yank)
     ("Delete horizontal space" delete-horizontal-space)
     ("Upcase region" call-interactively 'upcase-region)
     ("Capitalise region" call-interactively 'capitalize-region)
     ("Justify paragraph" justify-paragraph)
     ("Centre paragraph" center-paragraph)
     ("Centre line" center-line)
     ("Spell buffer" spell-buffer)
     ("Print region" call-interactively 'lpr-region)
     ("Print buffer" lpr-buffer)
     ("Printer Queue" lpq)
     ("Undo" undo)
     ("Other Menus" x-mouse-other-menus))))

(defHCImenu edit-transpose-menu
  ("chars" call-interactively 'transpose-chars)
  ("words" call-interactively 'transpose-words))

(defHCImenu edit-menu
  ("Transpose" . edit-transpose-menu)
  ("Kill line" kill-line)
  ("Yank" yank)
  ("Start keyboard macro" call-interactively 'start-kbd-macro)
  ("End keyboard macro" end-kbd-macro))

(defHCImenu print-menu
  ("Print buffer" lpr-buffer)
  ("Print region" call-interactively 'lpr-region)
  ("Printer queue" lpq))

(defHCImenu text-menu
  ("Text Menu")
  ("Editing commands" . edit-menu)
  ("Delete horizontal space" delete-horizontal-space)
  ("Upcase region" call-interactively 'upcase-region)
  ("Capitalise region" call-interactively 'capitalize-region)
  ("Justify paragraph" justify-paragraph)
  ("Centre paragraph" center-paragraph)
  ("Centre line" center-line)
  ("Spell buffer" spell-buffer)
  ("Print" . print-menu)
  ("Undo" undo)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))
  
)
