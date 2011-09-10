; My bindings for buffer menu mode mouse map

(require 'buff-setup)

(defvar buffer-menu-mode-mouse-map (make-keymap)
"Local mouse map for use in buffer menu mode."
)

(define-key buffer-menu-mode-mouse-map x-button-left 'x-Buffer-menu-1-window)
(define-key buffer-menu-mode-mouse-map x-button-right 'x-Buffer-menu-this-window)
(define-key buffer-menu-mode-mouse-map x-button-middle 'x-Buffer-menu-other-window)
(define-key buffer-menu-mode-mouse-map x-button-c-left 'x-Buffer-menu-delete)
(define-key buffer-menu-mode-mouse-map x-button-c-right 'x-Buffer-menu-unmark)
(define-key buffer-menu-mode-mouse-map x-button-c-middle 'x-Buffer-menu-execute)

(defun x-add-buffer-menu-mouse ()
"Add mouse bindings for buffer menu mode to local-mouse-map"
   (x-split-mouse-maps)
   (x-use-local-mouse-map buffer-menu-mode-mouse-map)
)

(setq buffer-menu-mode-hook (list 'x-add-buffer-menu-mouse))
