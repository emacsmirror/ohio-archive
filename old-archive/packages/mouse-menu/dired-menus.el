;;;; Dired mode menus
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 14:03:46 1988

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'dired-menus)

(if (eq window-system 'x)

(defXmenu 'dired-menu
  '("Dired Menu"
    ("Execute Menu"
     ("Update Dired Buffer" revert-buffer)
     ("Purge Dired Buffer" dired-purge)
     ("Describe Dired Mode" describe-mode)
     ("Describe Mouse Bindings" x-mouse-help)
     ("Other Menus" x-mouse-other-menus))
    ("Inspect Menu"
     ("Find file in this window" dired-find-file)
     ("Find file in other window" dired-find-file-other-window)
     ("View file" dired-view-file)
     ("Look for instances of something" dired-grep))
    ("Delete Menu"
     ("Mark file for deletion" call-interactively 'dired-flag-file-deleted)
     ("Remove deletion mark for file"  call-interactively 'dired-unflag)
     ("Mark backup and auto-save files for deletion"
      dired-flag-backup-and-auto-save-files)
     ("Mark auto-save files for deletion" dired-flag-auto-save-files)
     ("Mark backup files for deletion" dired-flag-backup-files)
     ("Mark files matching REGEXP for deletion" dired-flag-regexp-files)
     ("Perform requested deletions" dired-do-deletions))
    ("Modify Menu"
     ("Make new directory" call-interactively 'dired-make-directory)
     ("Make symbolic link" call-interactively 'dired-make-symbolic-link)
     ("Copy file" call-interactively 'dired-copy-file)
     ("Rename file" call-interactively 'dired-rename-file)
     ("Compress file" dired-compress)
     ("Uncompress file" dired-uncompress)
     ("Change file mode" call-interactively 'dired-chmod)
     ("Change file owner" call-interactively 'dired-chown)
     ("Change file group" call-interactively 'dired-chgrp)
     ("Byte recompile file" dired-byte-recompile))))

(defHCImenu dired-find-file-menu
  ("This window" dired-find-file)
  ("Other window" dired-find-file-other-window))

(defHCImenu dired-inspect-menu
  ("Find file" . dired-find-file-menu)
  ("View file" dired-view-file)
  ("Look for instances of something" dired-grep))

(defHCImenu dired-delete-menu
  ("Mark file for deletion" call-interactively 'dired-flag-file-deleted)
  ("Remove deletion mark for file"  call-interactively 'dired-unflag)
  ("Mark backup and auto-save files for deletion" dired-flag-backup-and-auto-save-files)
  ("Mark auto-save files for deletion"  dired-flag-auto-save-files)
  ("Mark backup files for deletion" dired-flag-backup-files)
  ("Mark files matching REGEXP for deletion" dired-flag-regexp-files)
  ("Perform requested deletions" dired-do-deletions))
	
(defHCImenu dired-ch-menu
  ("mode" call-interactively 'dired-chmod)
  ("owner" call-interactively 'dired-chown)
  ("group" call-interactively 'dired-chgrp))

(defHCImenu dired-alteration-menu
  ("Make new directory" call-interactively 'dired-make-directory)
  ("Make symbolic link" call-interactively 'dired-make-symbolic-link)
  ("Copy file" call-interactively 'dired-copy-file)
  ("Rename file" call-interactively 'dired-rename-file)
  ("Compress file" dired-compress)
  ("Uncompress file" dired-uncompress)
  ("Change file" . dired-ch-menu)
  ("Byte recompile file" dired-byte-recompile))

(defHCImenu dired-menu
  ("Dired Menu")
  ("Update Dired buffer" revert-buffer)
  ("Purge Dired buffer" dired-purge)
  ("Inspect" . dired-inspect-menu)
  ("Delete" . dired-delete-menu)
  ("Modify" . dired-alteration-menu)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )