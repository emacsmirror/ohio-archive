From gatech!uflorida!novavax!weiner@bbn.com Fri May 19 14:36:54 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 21:35:27 GMT
From: Bob Weiner <gatech!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: wconfig.el, window-config ring for GNU Emacs
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; SUMMARY:      wconfig.el - maintains save ring of window configurations
;; USAGE:        {C-x 4 w} to save config; {C-x 4 y} to restore successive
;;                 saves; {C-x 4 DEL} to delete successive saves
;;
;; AUTHOR:       weiner@novavax.UUCP     (Bob Weiner, Motorola Inc.)
;;               Based on kill-ring code from simple.el.
;;               Copyright (C) 1989, Bob Weiner and Free Software Foundation
;;
;; ORIG-DATE:    15-Mar-89
;; LAST-MOD:     28-Apr-89 at 00:39:18 by Bob Weiner
;; DESCRIPTION:  
;; DESCRIP-END.

;; Recommended key bindings
;;
(global-set-key "\C-x4w" 'wconfig-ring-save)
(global-set-key "\C-x4y" 'wconfig-yank-pop)
(global-set-key "\C-x4\177" 'wconfig-delete-pop)

(defvar wconfig-ring nil
  "List of saved window configurations.")

(defconst wconfig-ring-max 10
  "*Maximum length of window configuration ring before oldest elements are
thrown away.")

(defvar wconfig-ring-yank-pointer nil
  "The tail of the window configuration ring whose car is the last thing yanked.")

(defun wconfig-ring-save ()
  "Save the current window configuration onto the save ring.  Use
{\\[wconfig-yank-pop]} to restore it at a later time."
  (interactive)
  (setq wconfig-ring (cons (current-window-configuration) wconfig-ring))
  (if (> (length wconfig-ring) wconfig-ring-max)
      (setcdr (nthcdr (1- wconfig-ring-max) wconfig-ring) nil))
  (setq wconfig-ring-yank-pointer wconfig-ring)
  (wconfig-rotate-yank-pointer (1- (length wconfig-ring-yank-pointer)))
  (message "Window configuration saved.  Use 'wconfig-yank-pop' to restore."))

(defun wconfig-yank-pop (arg)
  "Replace the current window configuration with the next one in the window
configuration save ring.

With no argument, the last saved window configuration is displayed.
With argument n, the n'th previous window configuration is displayed.

The sequence of window configurations wraps around, so that after the oldest
one comes the newest one."
  (interactive "p")
  (wconfig-rotate-yank-pointer arg)
  (set-window-configuration (car wconfig-ring-yank-pointer)))

(defun wconfig-delete-pop ()
  "Replace the current window configuration with the most recently saved window
configuration in the save ring.  Then delete this new configuration from the
ring."
  (interactive)
  (if (not wconfig-ring)
      (error "Window configuration save ring is empty")
    (set-window-configuration (car wconfig-ring))
    (and (eq wconfig-ring wconfig-ring-yank-pointer)
	 (setq wconfig-ring-yank-pointer (cdr wconfig-ring)))
    (setq wconfig-ring (cdr wconfig-ring))))


(defun wconfig-rotate-yank-pointer (arg)
  "Rotate the yanking point in the window configuration save ring."
  (interactive "p")
  (let ((length (length wconfig-ring)))
    (if (zerop length)
	(error "Window configuration save ring is empty")
      (setq wconfig-ring-yank-pointer
	    (nthcdr (% (+ arg (- length (length wconfig-ring-yank-pointer)))
		       length)
		    wconfig-ring)))))

(provide 'wconfig)
-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


