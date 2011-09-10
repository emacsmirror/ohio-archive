;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp-Interaction -*- ;;;;;;;;;;;;;;;;;;;;;;;
;; gnusclear.el -- Clear all the unread news
;; Copyright (C) 1991 Dylan Kaufman
;; Author          : Dylan Kaufman
;; Created On      : Fri Apr 19 13:48:39 1991
;; Last Modified By: Dylan Kaufman
;; Last Modified On: Fri Apr 19 13:59:00 1991
;; Update Count    : 1
;; Status          : Functional (1 Caveat)
;; 
;; HISTORY
;; 19-Apr-1991		Dylan Kaufman	
;;    Preliminary version.  Single problem is that if one tries to
;;    use it on too many news groups, you'll surpass the limit on the
;;    nesting depth (I did), and that is why it does a redisplay to
;;    get rid of all unsubscribed groups and all groups which are
;;    already down to 0.
;; PURPOSE
;;    Clear all unread news in the *Newsgroup* buffer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; USAGE
;; 
;;    To use this function, just call clear-gnus within emacs.  I have
;;    a key set up as follows:
;;    (global-set-key "\C-cg" 'clear-gnus)
;;
;;    It is a good idea to either load or autoload the code into emacs
;;    when you start up:
;;    (autoload 'clear-gnus "gnusclear" "Clear unread news" t)
;;

(defun clear-gnus ()
  "Use gnus-Group-catch-up to catch up all the news groups quietly and
automatically.  Only operates on subscribed news groups with at least
one unread article."
  (interactive)
  (gnus-Group-list-groups nil)
  (end-of-buffer)
  (let ((temp (count-lines 1 (point))))
    (beginning-of-buffer)
    (do-clear temp)))

(defun do-clear (number)
  (if (> number 0)
      (progn
	(gnus-Group-catch-up t t)
	(do-clear (- number 1)))))
