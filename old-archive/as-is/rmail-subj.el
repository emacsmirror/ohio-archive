;; Subject search mode for rmail.
;; (c) Ashwin Ram, 11/23/87.
;; This file is not part of the GNU Emacs distribution (yet)
;; but is distributed under the same conditions.

;; The basic function is one that searches for a nondeleted message with a given
;; SUBJECT (a regular expression):

(defun rmail-subject-search (subject &optional reverse)
   "Find next message whose subject matches the regular expression SUBJECT.
If optional argument REVERSE is non-nil, find previous message.
Returns number of message if found, or nil."
   (let ((current rmail-current-message)
         (found nil))
      (while (and (not found) (< 0 current) (< current rmail-total-messages))
         (setq current (if reverse (1- current) (1+ current)))
         (if (and (not (rmail-message-deleted-p current))
                  (let ((current-subject (save-excursion
                                            (save-restriction
                                               (widen)
                                               (narrow-to-region (rmail-msgbeg current) (rmail-msgend current))
                                               (mail-fetch-field "Subject")))))
                     (and current-subject (string-match subject current-subject))))
             (setq found current)))
      found))

;; Using this, I wrote functions to find the next (or previous) message with the
;; same subject as the current one (like "subject search mode (^N)" in rn(1)),
;; and to kill all messages with the same subject as the current one (like the
;; "k" command in rn(1)).  The match ignores a leading "Re: ", if any.

(defun rmail-next-message-with-subject (subject)
   "Show following message whose subject matches the regular expression SUBJECT,
which defaults to the subject of the current message.  (This is like the '^N' command
of rn(1).)"
   (interactive (list (concat "^\\(Re: *\\|\\)"
                              (regexp-quote (rmail-deReify (or (mail-fetch-field "Subject")
                                                               (error "Current message has no subject"))))
                              "$")))
   (let ((next (rmail-subject-search subject)))
      (if next
          (rmail-show-message next)
          (message "No following message with subject %s" subject))))

(defun rmail-previous-message-with-subject (subject)
   "Show previous message whose subject matches the regular expression SUBJECT,
which defaults to the subject of the current message.  (This is like the '^N' command
of rn(1).)"
   (interactive (list (concat "^\\(Re: *\\|\\)"
                              (regexp-quote (rmail-deReify (or (mail-fetch-field "Subject")
                                                               (error "Current message has no subject"))))
                              "$")))
   (let ((previous (rmail-subject-search subject 'reverse)))
      (if previous
          (rmail-show-message previous)
          (message "No previous message with subject %s" subject))))

(defun rmail-kill-subject (subject)
   "Kills all following messages matching the regular expression SUBJECT, which
defaults to the subject of the current message.  (This is like the 'k' command of rn(1).)"
   (interactive (list (concat "^\\(Re: *\\|\\)"
                              (regexp-quote (rmail-deReify (or (mail-fetch-field "Subject")
                                                               (error "Current message has no subject"))))
                              "$")))
   (rmail-delete-message)
   (let ((start rmail-current-message)
         (deleted (list rmail-current-message))
         (next (rmail-subject-search subject)))
      (while next
         (rmail-show-message next)
         (rmail-delete-message)
         (setq deleted (cons rmail-current-message deleted))
         (setq next (rmail-subject-search subject)))
      (rmail-show-message start)
      (rmail-next-undeleted-message 1)
      (if deleted
          (message "Deleted %s messages %s" (length deleted) (reverse deleted))
          (message "No nondeleted messages with subject %s" subject))))

;; This is a utility useful in itself (I use it to set up the subject line
;; correctly in rmail-reply, for example):

;; Utility.  Couldn't resist the pun.
(defun rmail-deReify (subject)
   "Strips a leading \"Re:\", if any, from SUBJECT."
   (if (and subject (string-match "^Re: *" subject))
       (substring subject (match-end 0))
       subject))

;; I usually bind the above functions as follows:

(define-key rmail-mode-map "k"   'rmail-kill-subject)                  ;; K for Kill (as in RN(1)).
(define-key rmail-mode-map "\en" 'rmail-next-message-with-subject)     ;; N for Next
(define-key rmail-mode-map "\ep" 'rmail-previous-message-with-subject) ;; P for Previous
