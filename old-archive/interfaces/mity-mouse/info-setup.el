;; X-window specific setup code for info-mouse.el This is just a small file
;; of glue to paste together the info-mouse package and the mighty-mouse
;; package.
;;
;; tahorsley@ssd.csd.harris.com (Tom Horsley)
;; Dec 26, 1989

(require 'info-mouse)
(require 'mighty-mouse)

; Setup the keys for bopping around info tree. Since middle button is
; normally paste, and you are not likely to be pasting into an info buffer,
; re-use it for the info browser.

(defvar Info-mode-mouse-map (make-keymap)
"map to be used as local mouse map when in info mode."
)
(define-key Info-mode-mouse-map x-button-middle 'x-Info-mouse)
(define-key Info-mode-mouse-map x-button-c-middle 'x-Info-mouse-meta)

(defun Info-mouse-setup-mode ()
   (x-use-local-mouse-map Info-mode-mouse-map)
)

(defun x-mouse-info-set-point ()
"Digs up last mouse position and calls x-mouse-set-point with it."
   (x-mouse-set-point x-mouse-pos)
)

(defun x-Info-mouse (arg)
"Move forward through the info network by clicking the mouse. In a header
or menu item, moves to that item. At the end of the screen pages to the
next screen or the next item if at end of item."
   (Info-mouse)
)

(defun x-Info-mouse-meta (arg)
"Move backward through the info network by clicking the mouse. At end of
screen will go to previous page of note or previous item."
   (Info-mouse-meta)
)
