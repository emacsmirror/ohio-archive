From gatech!uflorida!novavax!weiner@bbn.com Fri May 19 14:37:05 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 21:32:12 GMT
From: Bob Weiner <gatech!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: smart-menu.el, menu interface for smart-key.el, GNU Emacs Subsystems
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; FILE:         smart-menu.el
;; SUMMARY:      Display subsystem menu for use with 'smart-key.el' package.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc.
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    20-Apr-89
;; LAST-MOD:     28-Apr-89 at 00:38:00 by Bob Weiner
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not part of GNU Emacs.
;;
;; DESCRIPTION:  
;;;
;;; This code is machine independent.
;;;
;;; To install:
;;;
;;;   See smart-key.el
;;;
;; DESCRIP-END.

(defconst smart-menu-at-bottom t
  "Non-nil means display menu below current window, nil means above.")

(defconst smart-menu-buffer "*Smart Menu*"
  "Name of the buffer used for the smart subsystem menu.")

(defvar *smart-menu-window-config* nil
  "Window configuration prior to entry of smart menu mode.")

(defvar *smart-menu-prev-mode* nil
  "Records major-mode prior to entry of smart-menu.")

(defvar smart-menu-mode-map nil
  "Keymap containing smart-menu commands.")
(if smart-menu-mode-map
    nil
  (setq smart-menu-mode-map (copy-keymap text-mode-map))
  (define-key smart-menu-mode-map "h" 'smart-menu-help)
  (define-key smart-menu-mode-map "q" 'smart-menu-quit)
  (define-key smart-menu-mode-map " " 'smart-menu-select)
  (define-key smart-menu-mode-map "\C-m" 'smart-key)
  (define-key smart-menu-mode-map "\M-\C-m" 'smart-key-meta))

;; Smart menu mode is suitable only for specially formatted data.
(put 'smart-menu-mode 'mode-class 'special)

(defun smart-menu-mode ()
  "Smart-menu mode provides a menu of commands for entering subsystems.
See also the documentation for 'smart-menu'.

\\[smart-menu-select] or \\[smart-key] selects entries in the menu.
\\[smart-menu-quit] or \\[smart-key-meta] quits from the menu."
  (kill-all-local-variables)
  (use-local-map smart-menu-mode-map)
  (setq major-mode 'smart-menu-mode)
  (setq mode-name "smart-menu")
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
;;  (smart-menu-set-mode-line)
  (run-hooks 'smart-menu-mode-hook))

(defun smart-menu ()
  "Smart-menu pops up a window with a menu of commands for entering subsystems.
\\[smart-menu-select] or \\[smart-key] selects entries in the menu.
\\[smart-menu-quit] or \\[smart-key-meta] quits from the menu."
  (interactive)
  (setq *smart-menu-window-config* (current-window-configuration))
  (let ((buf (get-buffer-create smart-menu-buffer))
	(pop-up-windows t))
    (setq *smart-menu-prev-mode* major-mode)
    (set-buffer buf)
    (or (eq major-mode 'smart-menu-mode)
	(smart-menu-mode))
    (split-window-vertically)
    (if smart-menu-at-bottom
	(pop-to-buffer smart-menu-buffer)
      (switch-to-buffer smart-menu-buffer))
    ;; Smallest size you can shrink a window
    (shrink-window (- (window-height) 4))
    ;; If empty buffer
    (if (= (save-excursion (end-of-buffer) (point)) 1)
	(let ((buffer-read-only nil))
	  (insert
	    "\nBuffer-Menu  Calendar  Dired-Home-Dir  <Help>  Info  Rmail  Mail  Shell  <Quit>")
	  (center-line)
	  (set-buffer-modified-p nil)
	  (back-to-indentation)))))

(defun smart-menu-select (&optional arg)
  "Select smart menu item that point is within and execute associated command.
Also quits from smart menu mode.  With prefix ARG, quits only."
  (interactive "P")
  (if arg
      (smart-menu-quit)
    (let ((cmd (extract-item-around-point)))
      (if (string= cmd "")
	  (error "No command selected.")
	(let ((exec-cmd
		(cond ((string= cmd "Buffer-Menu") '(buffer-menu nil))
		      ((string= cmd "Calendar") '(calendar))
		      ((string= cmd "Dired-Home-Dir") '(dired "~"))
		      ((string= cmd "<Help>") '(smart-menu-help))
		      ((string= cmd "Info") '(info))
		      ((string= cmd "Rmail") '(rmail))
		      ((string= cmd "Mail") '(mail))
		      ((string= cmd "Shell") '(shell))
		      ((string= cmd "<Quit>")))))
	  (if exec-cmd
	      (progn (smart-menu-quit)
		     (eval exec-cmd))
	    (beep)
	    (message (format "Unimplemented command selected, '%s'." cmd))))))))

(defun smart-menu-help ()
  "Displays description of major mode prior to smart-menu invocation."
  (interactive)
  (describe-function *smart-menu-prev-mode*))

(defun smart-menu-quit (&optional arg)
  "Quit from smart menu.  Restore previous window configuration.
Optional ARG kills the smart menu buffer rather than just burying it."
  (interactive "P")
  (let ((buf (get-buffer smart-menu-buffer)))
    (and buf (if arg (kill-buffer buf) (bury-buffer buf)))
    ;; Restore previous window configuration before entering smart menu.
    (if (and buf *smart-menu-window-config*)
	(progn
	  (set-window-configuration *smart-menu-window-config*)
	  (setq *smart-menu-window-config* nil)))))

(defun extract-item-around-point ()
  "Return whitespace separated menu item that point is within or before."
  (interactive)
  (save-excursion
    (skip-chars-backward "^ \t\n")
    (let ((start (point)))
      (skip-chars-forward "^ \t\n")
      (buffer-substring start (point)))))

(provide 'smart-menu)
-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


