;;; mscroll.el --- scroll messages in the echo area.

;;; Copyright (C) 1999 Matthew P. Hodges

;; Author: Matthew P. Hodges <pczmph@unix.ccc.nottingham.ac.uk>
;; Version: $Id: mscroll.el,v 1.12 2000/03/28 10:05:07 matt Exp $

;; mscroll.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mscroll.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;; 
;; The library provides two interactive functions, mscroll-activate
;; and mscroll-deactivate. These can be used to toggle the state of
;; the mscroll package. When enabled, the functions listed in
;; mscroll-advised-functions are advised such that the
;; messages/prompts which appear in the echo area scroll across it if
;; they are too wide.
;;
;; The variables mscroll-initial-delay and mscroll-scroll-delay can be
;; used to alter how the scrolling occurs.
;;
;; It is desirable for mscroll to ignore some frequently occurring
;; messages which convey little (or no) useful information. The
;; variable mscroll-ignored-messages contains a list of regular
;; expressions which won't be considered. You can modify it like this:
;;
;; (add-to-list 'mscroll-ignored-messages "^Compiling .*\\.el")
;;
;; Note that in GNU Emacs 21, it will be possible to resize the echo
;; area for long messages.

;;; Code:

(require 'advice)

;;; User options

(defvar mscroll-initial-delay 1000
  "The number of milliseconds to delay before scrolling.")

(defvar mscroll-scroll-delay 100
  "The number of milliseconds to delay between scroll steps.")

(defvar mscroll-ignored-messages
  '("^Fontifying" "^Continuing spelling check" "^Opening nn" "^Reading active" "^Retrieving newsgroup" "^Fetching headers" "\\.\\.\\.$")
  "A list of regular expressions for `mscroll-message' to ignore.")

;;; Constants

(defconst mscroll-advised-functions
  '((message . around)
    (y-or-n-p . before)
    (yes-or-no-p . before)
    (read-from-minibuffer . before)
    (error . before))
  "An alist of functions which can be advised by the mscroll package.
The car of each element is the function and the cdr the class of
advice.")

;;; Functions

(defun mscroll-message (message width log-message)
  "Scroll MESSAGE in the echo area.
The scrolling continues until the message is WIDTH characters long.
Log the message only if LOG-MESSAGE is t."
  (if log-message (message "%s" message))
  ;; Check to see if this message should be ignored
  (let ((ignored mscroll-ignored-messages)
        match-found)
    (while (and ignored (not match-found))
      (setq match-found (string-match (car ignored) message))
      (setq ignored (cdr ignored)))
    (if (not match-found)
        (if (> (length message) width)
            (let ((message-log-max nil)) ; Don't clutter up the log
              (message "%s" message)
              (sit-for 0 mscroll-initial-delay)
              (while (and (> (length message) width)
                          (not (input-pending-p)))
                (setq message (substring message 1))
                (message "%s" message)
                (sit-for 0 mscroll-scroll-delay))
              ;; In the event of pending input, the string may not have been
              ;; fully truncated
              (if (> (length message) width)
                  (setq message (substring message (- width))))))))
  message)                              ; Return the truncated message

(defun mscroll-activate ()
  "Activate the functions listed in `mscroll-advised-functions'."
  (interactive)
  (let ((list mscroll-advised-functions))
    (while list
      (ad-enable-advice (car (car list)) (cdr (car list)) 'mscroll)
      (ad-activate (car (car list)))
      (setq list (cdr list))))
  (message "mscroll enabled"))

(defun mscroll-deactivate ()
  "Deactivate the functions listed in `mscroll-advised-functions'."
  (interactive)
  (let ((list mscroll-advised-functions))
    (while list
      (ad-disable-advice (car (car list)) (cdr (car list)) 'mscroll)
      (ad-activate (car (car list)))
      (setq list (cdr list))))
  (message "mscroll disabled"))

;;; Advised functions

(defadvice message (around mscroll)
  (ad-with-originals 'message
    (let ((args (ad-get-args 0)))
      (if (equal '(nil) args)
          ad-do-it
        (setq ad-return-value
              (mscroll-message (apply 'format args) (frame-width) t))))))

(defadvice y-or-n-p (before mscroll)
  "Scroll y-or-n prompts if they are too wide for the frame."
  (let* ((message (ad-get-arg 0))
         (pad-string "(y or n) ")
         (pad-width (length pad-string)))
    (ad-with-originals 'message
      (setq message (mscroll-message (concat message pad-string)
                                     (frame-width) nil))
      (ad-set-arg 0 (substring message 0 (- (length message) pad-width))))))

(defadvice yes-or-no-p (before mscroll)
  "Scroll yes-or-no prompts if they are too wide for the frame."
  (let* ((message (ad-get-arg 0))
         (pad-string "(yes or no)     ")  ; Padding for width of response
         (pad-width (length pad-string)))
    (ad-with-originals 'message
      (setq message (mscroll-message (concat message pad-string)
                                     (frame-width) nil))
      (ad-set-arg 0 (substring message 0 (- pad-width))))))

(defadvice read-from-minibuffer (before mscroll)
  "Scroll minibuffer read prompts."
  (let* ((message (ad-get-arg 0))
         (pad-string "           ")       ; Padding for width of response
         (pad-width (length pad-string)))
    (ad-with-originals 'message
      (setq message (mscroll-message (concat message pad-string)
                                     (frame-width) nil))
      (ad-set-arg 0 (substring message 0 (- pad-width))))))

(defadvice error (before mscroll)
  "Scroll error messages."
  (let (message)
    (ad-with-originals 'message
      (setq message (mscroll-message (apply 'format (ad-get-args 0))
                                     (frame-width) t))
      (ad-set-arg 0 (substring message 0 (length message))))))

(provide 'mscroll)

;;; mscroll.el ends here
