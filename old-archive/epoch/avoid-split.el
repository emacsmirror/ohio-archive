; From: tmb@AI.MIT.EDU (Thomas M. Breuel)
; Date: Mon, 30 Jul 90 20:44:56 EDT
; Subject: integrating epoch functions with standard emacs window functions
; 
; The following code will override some of the default Emacs code and
; avoid splitting a window if some other X window (screen) already
; displays the target buffer.
; 
; 						Thomas.


(if (fboundp 'old-switch-to-buffer-other-window)
    (fset 'old-switch-to-buffer-other-window
	  (symbol-function 'switch-to-buffer-other-window)))

(if (fboundp 'old-get-buffer-window) nil
    (fset 'old-get-buffer-window (symbol-function 'get-buffer-window)))

(defun get-buffer-window (buffer)
  (let ((window (epoch::get-buffer-window buffer)))
    (if (null window) nil
	(let* ((screen (epoch::screen-of-window window))
	       (info (epoch::screen-information))
	       (mapped (nth 5 info)))
	  (if mapped window nil)))))


(if (fboundp 'old-pop-to-buffer) nil
    (fset 'old-pop-to-buffer (symbol-function 'pop-to-buffer)))

(defun pop-to-buffer (buffer &optional split)
  (let* ((window (get-buffer-window buffer))
	 (screen (and window (epoch::screen-of-window window))))
    (cond
      (window
       (epoch::map-screen screen)
       (epoch::raise-screen screen)
       (epoch::select-screen screen)
       (epoch::warp-pointer 10 10 screen)
       (switch-to-buffer buffer)
       window)
      (t
       (old-pop-to-buffer buffer split)))))

(if (fboundp 'old-display-buffer) nil
    (fset 'old-display-buffer (symbol-function 'display-buffer)))

(defun display-buffer (buffer &optional split)
  (let* ((window (get-buffer-window buffer))
	 (screen (and window (epoch::screen-of-window window))))
    (cond
      (window
       (epoch::map-screen screen)
       (epoch::raise-screen screen)
       window)
      (t
       (old-display-buffer buffer split)))))

(setq temp-buffer-show-hook '(lambda (x) (display-buffer x)))
