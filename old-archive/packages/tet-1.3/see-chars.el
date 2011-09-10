;;
;; From the xemacs FAQ.
;;
;; 54: How do I tell what characters my function keys emit?
;;  
;; Use this function by Randal L. Schwartz <merlyn@iwarp.intel.com>:
;;
;; To Run:
;;    Meta-x load-file see-chars.el   (Meta = Esc)
;;    Meta-x see-chars
;;    Press any key and wait for answer
;;

(defun see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
        (inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
            quit-flag nil))         ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))
  
;;  Alternatively, use the "C-h l" view-lossage command, which will display    +
;;  the last 100 characters Emacs has seen in its input stream.                +
