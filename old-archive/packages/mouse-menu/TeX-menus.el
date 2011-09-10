;;;; TeX and LaTeX mode menus.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 14:02:32 1988 

;; Conditionalise support for window-system internals.
;; Eventually need to support 'defHCImenu' macro in both worlds, made
;; difficult by lack of 'pull-right menu' concept in X at present, and
;; the heavy use made of this in the existing menu package.
(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'TeX-menus)

(if (eq window-system 'x)
    ;; Set up single menus for X...
    (progn

(defXmenu 'TeX-menu
  '("TeX Menu" 
    ("TeX Menu"
     ("Run TeX on current buffer" TeX-buffer)
     ("Run TeX on current region" call-interactively 'TeX-region)
     ("Run BibTeX on current buffer" TeX-BibTeX-buffer)
     ("Show the .dvi file on screen" call-interactively 'TeX-screen-print)
     ("Show the printer queue" TeX-show-print-queue)
     ("Show last output from the TeX process" 
      call-interactively 'TeX-recenter-output-buffer)
     ("Print the .dvi file" TeX-print)
     ("Spell the current TeX buffer" TeX-spell-buffer)
     ("Spell all TeX buffers" TeX-spell-all-TeX-buffers)
     ("Kill the current TeX process" TeX-kill-job)
     ("Describe TeX mode" describe-mode)
     ("Other Menus" x-mouse-other-menus))))

(defXmenu 'LaTeX-menu
  '("LaTeX Menu"
    ("LaTeX Menu"
     ("Run LaTeX on buffer" TeX-buffer)
     ("Run LaTeX on region" call-interactively 'TeX-region)
     ("Run BibTeX on current buffer" TeX-BibTeX-buffer))
    ("Preview Menu"
     ("Preview the .dvi file on screen" call-interactively 'TeX-screen-print)
     ("Suspend previewer (assuming its running)" TeX-suspend-job)
     ("Restart previewer (assuming its running)" TeX-continue-job))
    ("Print Menu"
     ("Print the .dvi file" TeX-print)
     ("Show the printer queue" TeX-show-print-queue)
     ("Remove job from the printer queue"
      call-interactively 'TeX-remove-from-print-queue))
    ("Spell Menu"
     ("Spell the current LaTeX buffer" TeX-spell-buffer)
     ("Spell all LaTeX buffers" TeX-spell-all-TeX-buffers))
    ("Edit Menu"
     ("New Chapter" TeX-new-heading "chapter")
     ("New Section" TeX-new-heading "section")
     ("New Subsection" TeX-new-heading "subsection")
     ("New Subsubsection" TeX-new-heading "subsubsection"))
    ("Typeface Menu"
     ("Bold" TeX-typeface-region "bf")
     ("Emphasise" TeX-typeface-region "em")
     ("Italicise" TeX-typeface-region "it")
     ("Slant" TeX-typeface-region "sl")
     ("Sans-serif" TeX-typeface-region "sf")
     ("Small Caps" TeX-typeface-region "sc")
     ("Teletype" TeX-typeface-region "tt"))
    ("Miscellaneous"
     ("Show last output from the LaTeX process" 
      call-interactively 'TeX-recenter-output-buffer)     
     ("Kill the current LaTeX process" TeX-kill-job)
     ("Describe LaTeX mode" describe-mode)
     ("Other Menus" x-mouse-other-menus)))))

(defHCImenu TeX-menu
  ("TeX Menu")
  ("Run TeX on current buffer" TeX-buffer)
  ("Run TeX on current region" call-interactively 'TeX-region)
  ("Run BibTeX on current buffer" TeX-BibTeX-buffer)
  ("Show the .dvi file on screen" call-interactively 'TeX-screen-print)
  ("Show the printer queue" TeX-show-print-queue)
  ("Show last output from the LaTeX process" 
   call-interactively 'TeX-recenter-output-buffer)
  ("Print the .dvi file" TeX-print)
  ("Spell the current TeX buffer" TeX-spell-buffer)
  ("Spell all TeX buffers" TeX-spell-all-TeX-buffers)
  ("Kill the current TeX process" TeX-kill-job)
  ("Describe TeX mode" describe-mode)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

(defHCImenu LaTeX-run-menu
  ("LaTeX on current buffer" TeX-buffer)
  ("LaTeX on current region" call-interactively 'TeX-region)
  ("BibTeX on current buffer" TeX-BibTeX-buffer))

(defHCImenu LaTeX-show-menu
  ("the .dvi file on screen" call-interactively 'TeX-screen-print)
  ("the printer queue" TeX-show-print-queue)
  ("last output from the TeX process" 
   call-interactively 'TeX-recenter-output-buffer))

(defHCImenu LaTeX-spell-menu
  ("the current LaTeX buffer" TeX-spell-buffer)
  ("all LaTeX buffers" TeX-spell-all-TeX-buffers))

(defHCImenu LaTeX-menu
  ("LaTeX Menu")
  ("Run" . LaTeX-run-menu)
  ("Show" . LaTeX-show-menu)
  ("Spell" . LaTeX-spell-menu)
  ("Print the .dvi file" TeX-print)
  ("Kill the current LaTeX process" TeX-kill-job)
  ("Describe LaTeX mode" describe-mode)
  ("Other menus" . other-menus-menu)
  ("Quit" . emacs-quit-menu))

  )
