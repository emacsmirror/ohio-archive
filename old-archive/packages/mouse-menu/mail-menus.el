;;;; Mail mode menus
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 14:10:01 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'mail-menus)

(if (eq window-system 'x)

(defXmenu 'mail-menu
  '("Mail Menu"
    ("Mail Menu"
     ("Send" mail-send)
     ("Send and exit mail" call-interactively 'mail-send-and-exit)
     ("Move to \"To:\" field" mail-to)
     ("Move to \"Subject:\" field" mail-subject)
     ("Move to \"Cc:\" field" mail-cc)
     ("Move to \"Bcc:\" field" mail-bcc)
     ("Insert contents of Rmail message being replied to (if any)" 
      call-interactively 'mail-yank-original)
     ("Fill yanked original message (if any)"
      call-interactively 'mail-fill-yanked-message)
     ("Fill yanked original message (if any) and justify"
      prefix-arg-supplied 'mail-fill-yanked-message)
     ("Append signature file" 
      if (file-exists-p "~/.signature")
      (mail-signature)
      (error "~/.signature does not exist."))
     ("Other Menus" x-mouse-other-menus))))

(defHCImenu mail-move-menu
  ("To: field" mail-to)
  ("Subject: field" mail-subject)
  ("CC: field" mail-cc)
  ("BCC: field" mail-bcc))

(defHCImenu mail-yank-menu
  ("being replied to (if any)." call-interactively 'mail-yank-original)
  ("Fill yanked original." call-interactively 'mail-fill-yanked-message)
  ("Fill yanked original and justify."
   prefix-arg-supplied 'mail-fill-yanked-message))

(defHCImenu mail-insert-menu
  ("Contents of Rmail message" . mail-yank-menu)
  ("Append signature file" 
   if (file-exists-p "~/.signature")
   (mail-signature)
   (error "You don't have a ~/.signature file")))

(defHCImenu mail-quit-menu
  ("Mail without sending" kill-buffer (current-buffer))
  ("Emacs" . emacs-quit-menu))

(defHCImenu mail-menu
  ("Mail Menu")
  ("Send" mail-send)
  ("Send and exit mail" call-interactively 'mail-send-and-exit)
  ("Move to" . mail-move-menu)
  ("Insert" . mail-insert-menu)
  ("Other menus" . other-menus-menu)
  ("Quit" . mail-quit-menu))

  )