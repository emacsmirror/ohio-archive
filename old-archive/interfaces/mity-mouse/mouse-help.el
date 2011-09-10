; Module to get a buffer with a readable description of the mouse key
; bindings.
;
; tahorsley@ssd.csd.harris.com (Tom Horsley)
; Dec 26, 1989
;
; To actually bind a function to some mouse key use:
; (define-key mouse-map x-button-right 'func)
; ...
;
; Where x-button-right can be replaced with any of the names below:

(require 'x-mouse)

(defvar x-button-description
   '(
      (x-button-right           . "right button down")
      (x-button-right-up        . "right button up")
      (x-button-middle          . "middle button down")
      (x-button-middle-up       . "middle button up")
      (x-button-left            . "left button down")
      (x-button-left-up         . "left button up")

      (x-button-s-right         . "<Shift> right button down")
      (x-button-s-right-up      . "<Shift> right button up")
      (x-button-s-middle        . "<Shift> middle button down")
      (x-button-s-middle-up     . "<Shift> middle button up")
      (x-button-s-left          . "<Shift> left button down")
      (x-button-s-left-up       . "<Shift> left button up")

      (x-button-m-right         . "<Meta> right button down")
      (x-button-m-right-up      . "<Meta> right button up")
      (x-button-m-middle        . "<Meta> middle button down")
      (x-button-m-middle-up     . "<Meta> middle button up")
      (x-button-m-left          . "<Meta> left button down")
      (x-button-m-left-up       . "<Meta> left button up")

      (x-button-c-right         . "<Ctrl> right button down")
      (x-button-c-right-up      . "<Ctrl> right button up")
      (x-button-c-middle        . "<Ctrl> middle button down")
      (x-button-c-middle-up     . "<Ctrl> middle button up")
      (x-button-c-left          . "<Ctrl> left button down")
      (x-button-c-left-up       . "<Ctrl> left button up")

      (x-button-m-s-right       . "<Meta-Shift> right button down")
      (x-button-m-s-right-up    . "<Meta-Shift> right button up")
      (x-button-m-s-middle      . "<Meta-Shift> middle button down")
      (x-button-m-s-middle-up   . "<Meta-Shift> middle button up")
      (x-button-m-s-left        . "<Meta-Shift> left button down")
      (x-button-m-s-left-up     . "<Meta-Shift> left button up")

      (x-button-c-s-right       . "<Ctrl-Shift> right button down")
      (x-button-c-s-right-up    . "<Ctrl-Shift> right button up")
      (x-button-c-s-middle      . "<Ctrl-Shift> middle button down")
      (x-button-c-s-middle-up   . "<Ctrl-Shift> middle button up")
      (x-button-c-s-left        . "<Ctrl-Shift> left button down")
      (x-button-c-s-left-up     . "<Ctrl-Shift> left button up")

      (x-button-c-m-right       . "<Ctrl-Meta> right button down")
      (x-button-c-m-right-up    . "<Ctrl-Meta> right button up")
      (x-button-c-m-middle      . "<Ctrl-Meta> middle button down")
      (x-button-c-m-middle-up   . "<Ctrl-Meta> middle button up")
      (x-button-c-m-left        . "<Ctrl-Meta> left button down")
      (x-button-c-m-left-up     . "<Ctrl-Meta> left button up")

      (x-button-c-m-s-right     . "<Ctrl-Meta-Shift> right button down")
      (x-button-c-m-s-right-up  . "<Ctrl-Meta-Shift> right button up")
      (x-button-c-m-s-middle    . "<Ctrl-Meta-Shift> middle button down")
      (x-button-c-m-s-middle-up . "<Ctrl-Meta-Shift> middle button up")
      (x-button-c-m-s-left      . "<Ctrl-Meta-Shift> left button down")
      (x-button-c-m-s-left-up   . "<Ctrl-Meta-Shift> left button up")
   )
)

; Add to help-map under the "x" key (for x-windows, of course)
(define-key help-map "x" 'mouse-help)

(defun mouse-help ()
"Display reasonable description of mouse-map."
   (interactive)
   (let
      (
         (b-desc x-button-description)
         m-fun
         k-desc
         (h-buf (get-buffer-create "*Help*"))
         p-buf
         m-ndx
         (local-map-buffer (current-buffer))
      )
      (save-excursion
         (set-buffer h-buf)
         (erase-buffer)
         (while b-desc
            (setq m-ndx (string-to-char (symbol-value (car (car b-desc)))))
            (setq m-fun (aref mouse-map m-ndx))
            (if (eq m-fun 'x-mouse-route-click)
               ; This is a split mouse map, go look at local-mouse-map then
               ; global-mouse-map if local was nil.
               (progn
                  (save-excursion
                     (set-buffer local-map-buffer)
                     (setq m-fun (aref local-mouse-map m-ndx))
                  )
                  (if m-fun
                     nil
                     (setq m-fun (aref global-mouse-map m-ndx))
                  )
               )
            )
            (setq k-desc (cdr (car b-desc)))
            (setq b-desc (cdr b-desc))
            (if (and m-fun (not (eq m-fun 'x-mouse-ignore)))
               (let
                  (
                     (doc (documentation m-fun))
                  )
                  (if doc
                     (progn
                        (insert "Function ")
                        (princ m-fun h-buf)
                        (insert " bound to " k-desc ":\n" doc "\n\n")
                     )
                  )
               )
            )
         )
         (goto-char (point-min))
      )
      (display-buffer h-buf)
   )
)
