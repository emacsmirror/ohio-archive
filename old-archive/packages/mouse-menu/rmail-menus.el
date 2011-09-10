;;;; Rmail mode menus
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Thu May 12 14:08:42 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'rmail-menus)

(defun rmail-search-backward ()
  "Does the same thing as rmail-search, but searches backwards."
  (let ((regexp
	 (read-string
	  (concat "Reverse Rmail search (regexp): "
		  (if rmail-search-last-regexp
		      (concat "(default " rmail-search-last-regexp ") ")
		    ""))))
	(omin (point-min))
	(omax (point-max))
	(opoint (point))
	(win nil)
	(msg rmail-current-message))
    ;; Trash the search regexp memory if necessary
    (cond ((string= regexp "")
	   (setq regexp rmail-search-last-regexp))
	  ((not (string= regexp ""))
	   (setq rmail-search-last-regexp regexp))
	  ((not rmail-search-last-regexp)
	   (error "No previous Rmail search string")))
    ;; Say what's going on for slow things.
    (message "Reverse Rmail search for %s..." regexp)
    (unwind-protect
	(progn
	  (widen)
	  ;; Check messages one by one, decrementing message number
	  ;; but searching forward through each message.
	  (while (and (null win) (> msg 1))
	    (goto-char (rmail-msgbeg (setq msg (1- msg))))
	    (setq win (re-search-forward
		       regexp (rmail-msgend msg) t))))
      (if win
	  (progn
	    ;; This is a reverse search and we found a message,
	    ;; search backward thru this message to position point.
	    (goto-char (rmail-msgend msg))
	    (re-search-backward regexp (rmail-msgbeg msg) t)
	    (setq win (point))
	    (rmail-show-message msg)
	    (message "Reverse Rmail search for %s...done" regexp)
	    (goto-char win))
	(goto-char opoint)
	(narrow-to-region omin omax)
	(ding)
	(message "Reverse Searched failed: %s" regexp)))))

(if (eq window-system 'x)

    (progn

(defXmenu 'rmail-menu
  '("Rmail Menu"
    ("Read Menu"
     ("next non-deleted message"
      call-interactively 'rmail-next-undeleted-message)
     ("previous non-deleted message"
      call-interactively 'rmail-previous-undeleted-message)
     ("next message (deleted or not)"
      call-interactively 'rmail-next-message)
     ("previous message (deleted or not)"
      call-interactively 'rmail-previous-message)
     ("last message" rmail-last-message))
    ("Search Menu"
     ("for message by number?" call-interactively 'rmail-show-message)
     ("for next message containing string?" call-interactively 'rmail-search)
     ("previous message containing string?"
      call-interactively 'rmail-search-backward))
    ("Save Menu"
     ("to Rmail file" call-interactively 'rmail-output-to-rmail-file)
     ("to UNIX mail file" call-interactively 'rmail-output)
     ("to Rmail file then delete"
      progn
      (call-interactively 'rmail-output-to-rmail-file)
      (rmail-delete-forward))
     ("to UNIX mail file then delete"
      progn
      (call-interactively 'rmail-output)
      (rmail-delete-forward)))
    ("Delete Menu"
     ("this message then move forwards" rmail-delete-forward)
     ("this message then move backwards"
      prefix-arg-supplied 'rmail-delete-forward)
     ("Undelete preceding message" rmail-undelete-previous-message))
    ("Reply Menu"
     ("Forward this message" rmail-forward)
     ("Reply to the sender of this message" prefix-arg-supplied 'rmail-reply)
     ("Reply to all recipients of this message"
      call-interactively 'rmail-reply))
    ("Miscellaneous Menu"
     ("Give a one-line summary of all messages" rmail-summary)
     ("Get new mail" rmail-get-new-mail)
     ("Other Menus" x-mouse-other-menus)
     ("Quit Rmail" rmail-quit))))

 (defXmenu 'rmailsum-menu
  '("Rmail Summary Menu"
    ("Rmail Summary Menu"
     ("Move to next undeleted message after the cursor location"
      call-interactively 'rmail-summary-next-msg)
     ("Move to previous undeleted message after the cursor location"
      rmail-summary-previous-msg)
     ("Move to next message after the cursor location"
      rmail-summary-next-all)
     ("Move to previous message after the cursor location"
      rmail-summary-previous-all)
     ("Jump to the message at the cursor location"
      rmail-summary-goto-msg)
     ("Delete the message at the cursor location and move to next message"
      rmail-summary-delete-forward)
     ("Undelete this or previous deleted message"
      rmail-summary-undelete)
     ("Scroll displayed Rmail message forward"
      rmail-summary-scroll-msg-up)
     ("Scroll displayed Rmail message backward"
      rmail-summary-scroll-msg-down)
     ("Exit and kill the Summary window"
      rmail-summary-exit)
     ("Other Menus" x-mouse-other-menus)
     ("Quit Rmail" rmail-summary-quit))))

)

(defHCImenu rmail-quit-menu
  ("Rmail" rmail-quit)
  ("Emacs" . emacs-quit-menu))

(defHCImenu rmail-search-menu
  ("Search forwards" call-interactively 'rmail-search)
  ("Search backwards" call-interactively 'rmail-search-backward))

(defHCImenu rmail-specific-message-menu
  ("Message number?" call-interactively 'rmail-show-message)
  ("Message containing string?" . rmail-search-menu))

(defHCImenu rmail-motion-menu
  ("Next non-deleted message"
   call-interactively 'rmail-next-undeleted-message)
  ("Previous non-deleted message"
   call-interactively 'rmail-previous-undeleted-message)
  ("Next message (deleted or not)"
   call-interactively 'rmail-next-message)
  ("Previous message (deleted or not)"
   call-interactively 'rmail-previous-message)
  ("Last message" rmail-last-message)
  ("Specific message:" . rmail-specific-message-menu))

(defHCImenu rmail-delete-menu
  ("then move forwards" rmail-delete-forward)
  ("then move backwards" prefix-arg-supplied 'rmail-delete-forward))

(defHCImenu rmail-reply-menu
  ("Only to the sender" call-interactively 'rmail-reply)
  ("To all recipients of original message" prefix-arg-supplied 'rmail-reply))

(defHCImenu rmail-menu
  ("Rmail Menu")
  ("Move to" . rmail-motion-menu)
  ("Delete this message" . rmail-delete-menu)
  ("Undelete preceding message" rmail-undelete-previous-message)
  ("Forward this message" rmail-forward)
  ("Reply to this message" . rmail-reply-menu)
  ("Get new mail" rmail-get-new-mail)
  ("Other menus" . other-menus-menu)
  ("Quit" . rmail-quit-menu))



  )
