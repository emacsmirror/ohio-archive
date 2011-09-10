;;;; X Emacs functions and menus for electric buffer mode.
;;;; Allow mark and shoot with middle and left buttons, menu on right.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Fri May 11 12:10:05 1990 

(require 'x-menus)
(provide 'ebuff-menus)

(defXmenu 'electric-buffer-menu
  '("Electric Buffer Menu"
    ("Buffer Menu"
     ("Mark buffer on cursor line to be shown" x-mouse-ebuff-select)
     ("Mark buffer on cursor line to be saved" x-mouse-ebuff-save)
     ("Mark buffer on cursor line to be killed" x-mouse-ebuff-kill)
     ("Show (and save or kill) marked buffers" x-mouse-ebuff-exit)
     ("View buffer on line cursor is on" x-mouse-ebuff-view)
     ("Quit, restoring previous window configuration"  x-mouse-ebuff-quit))))

(defun x-mouse-ebuff-show (arg)
  "Exit Electric Buffer mode and show the buffer under the mouse."
  (eval-in-window x-mouse-window
    (x-mouse-move-point arg)
    (Electric-buffer-menu-select)))

(defun x-mouse-ebuff-select ()
  "Mark the buffer on cursor line to be shown when Electric Buffer Mode exits."
  (eval-in-window x-mouse-window (Buffer-menu-mark)))

(defun x-mouse-ebuff-save ()
  "Mark the buffer on cursor line to be saved when Electric Buffer Mode exits."
  (eval-in-window x-mouse-window (Buffer-menu-save)))

(defun x-mouse-ebuff-kill ()
  "Mark the buffer on cursor line to be killed when Electric Buffer Mode exits."
  (eval-in-window x-mouse-window (Buffer-menu-delete)))

(defun x-mouse-ebuff-exit ()
  "Exit Electric Buffer Mode saving, killing and showing buffers as marked."
  (eval-in-window x-mouse-window (Electric-buffer-menu-exit)))

(defun x-mouse-ebuff-view ()
  "View the buffer on the line the cursor is on."
  (eval-in-window x-mouse-window (Electric-buffer-menu-mode-view-buffer)))

(defun x-mouse-ebuff-quit ()
 "Quit from Electric Buffer Mode, restoring previous window configuration."
 (eval-in-window x-mouse-window (Electric-buffer-menu-quit)))