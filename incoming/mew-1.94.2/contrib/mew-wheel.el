;;
;; mew-wheel.el by Yuuichi Teranishi 寺西裕一 <teranisi@isl.ntt.co.jp>
;; Read the mew messages by spinning the mouse wheel.

;; [How to Use]
;; 
;;(require 'mew-wheel)
;;(add-hook 
;; 'mew-summary-mode-hook
;; (lambda () 
;;   (if mew-xemacs-p
;;       (progn
;;	 (define-key mew-summary-mode-map 'button4 'mew-summary-wheel-up)
;;	 (define-key mew-summary-mode-map 'button5 'mew-summary-wheel-down)
;;	 (define-key mew-summary-mode-map [(shift button4)] 
;;	   'mew-summary-wheel-up)
;;	 (define-key mew-summary-mode-map [(shift button5)] 
;;	   'mew-summary-wheel-down)
;;	 )
;;     (define-key mew-summary-mode-map [mouse-4] 'mew-summary-wheel-up)
;;     (define-key mew-summary-mode-map [mouse-5] 'mew-summary-wheel-down)
;;     (define-key mew-summary-mode-map [S-mouse-4] 'mew-summary-wheel-up)
;;     (define-key mew-summary-mode-map [S-mouse-5] 'mew-summary-wheel-down))))

;;(add-hook
;; 'mew-message-mode-hook
;; (lambda () 
;;   (if mew-xemacs-p
;;       (progn
;;	 (define-key mew-message-mode-map 'button4 'mew-message-wheel-up)
;;	 (define-key mew-message-mode-map 'button5 'mew-message-wheel-down)
;;	 (define-key mew-message-mode-map [(shift button4)] 
;;	   'mew-message-wheel-up)
;;	 (define-key mew-message-mode-map [(shift button5)] 
;;	   'mew-message-wheel-down)
;;	 )
;;     (define-key mew-message-mode-map [mouse-4] 'mew-message-wheel-up)
;;     (define-key mew-message-mode-map [mouse-5] 'mew-message-wheel-down)
;;     (define-key mew-message-mode-map [S-mouse-4] 'mew-message-wheel-up)
;;     (define-key mew-message-mode-map [S-mouse-5] 'mew-message-wheel-down))))
;;

(defvar mew-wheel-scroll-amount '(5 . 1)
  "Amount to scroll messages by spinning the mouse wheel.
This is actually a cons cell, where the first item is the amount to scroll
on a normal wheel event, and the second is the amount to scroll when the
wheel is moved with the shift key depressed.")

(defun mew-summary-wheel-down (event)
  "Make this message scroll down by spinning the mouse wheel."
  (interactive "e")
  (let ((amount (if (memq 'shift (event-modifiers event))
		    (cdr mew-wheel-scroll-amount)
		  (car mew-wheel-scroll-amount))))
    (if mew-summary-buffer-disp-msg 
	  (let ((buf (current-buffer))
		(msg (mew-summary-message-number))
		(ofld-msg (mew-current-get 'message))
		(part (mew-syntax-nums))
		(opart (mew-current-get 'part)))
	    (cond ((or (and msg (null part) (string= msg (cdr ofld-msg)))
		       (and part (equal part opart)))
		   (unwind-protect
		       (progn
			 (mew-window-configure buf 'message)
			 (if (mew-message-next-page (if amount amount 1))
			     (mew-message-next-msg))
			 )
		     (pop-to-buffer buf)))
		  ((or msg part)
		   (mew-summary-show))
		  (t
		   (message "No message or part here"))))
      (scroll-up amount))))

(defun mew-summary-wheel-up (event)
  "Make this message scroll up by spinning the mouse wheel."
  (interactive "e")
  (let ((amount (if (memq 'shift (event-modifiers event))
		    (cdr mew-wheel-scroll-amount)
		  (car mew-wheel-scroll-amount))))
    (if mew-summary-buffer-disp-msg 
	(if (or (mew-summary-message-number) (mew-syntax-number))
	    (let ((buf (current-buffer)))
	      (unwind-protect
		  (progn
		    (mew-window-configure buf 'message)
		    (condition-case ()
			(mew-message-prev-page (if amount amount 1))
		      (error 
		       (mew-message-next-msg -1))))
		(pop-to-buffer buf))
	      )
	  (mew-summary-display-up))
      (scroll-down amount))))

(defun mew-message-wheel-down (event)
  "Make this message scroll down by spinning the mouse wheel."
  (interactive "e")
  (save-selected-window
    (select-window (if (fboundp 'event-window)
		       (event-window event)
		     (posn-window (event-start event)))))
  (mew-summary-wheel-down event))

(defun mew-message-wheel-up (event)
  "Make this message scroll up by spinning the mouse wheel."
  (interactive "e")
  (save-selected-window
    (select-window (if (fboundp 'event-window)
		       (event-window event)
		     (posn-window (event-start event)))))
  (mew-summary-wheel-up event))
  

(provide 'mew-wheel)