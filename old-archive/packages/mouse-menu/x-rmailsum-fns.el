;;;; Rmail summary X window system mouse functions.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Wed May 16 11:02:06 1990

(require 'x-fns)
(provide 'x-rmailsum-fns)

(defun x-mouse-rmailsum-goto-msg (arg)
  "Jump to the message under the mouse (and move the cursor there too)."
  (eval-in-window x-mouse-window
    (x-mouse-set-point arg)
    (rmail-summary-goto-msg)))

(defun x-mouse-rmailsum-exit (arg)
  "Exit Rmail Summary mode, but remain in Rmail." 
  (select-window x-mouse-window)
  (bury-buffer (current-buffer))
  (if (get-buffer-window rmail-buffer)
      ;; Select the window with rmail in it, then delete this window.
      (select-window (prog1
			 (get-buffer-window rmail-buffer)
		       (delete-window (selected-window))))
    ;; Switch to the rmail buffer in this window.
    (switch-to-buffer rmail-buffer)))
