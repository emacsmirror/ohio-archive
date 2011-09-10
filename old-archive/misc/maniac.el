;;; maniac.el --- fill paragraphs like a maniac.

;; Copyright (C) 1994 Per Abrahamsen.
;; Copyright abandoned.  This file is donated to the public domain.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: wp
;; Version: 0.0
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;; LCD Archive Entry:
;; maniac|Per Abrahamsen|abraham@iesd.auc.dk|
;; fill paragraphs like a maniac|
;; 01-Jan-1994|0.0|~/misc/maniac.el.Z|

;;; Commentary:

;; Save this file in your load-path, insert
;;   (autoload 'maniac-fill-mode "maniac" nil t)
;; in your `.emacs' file, restart FSF Emacs 19 then activate with 
;;   M-x maniac-fill-mode RET

;; Should really use after-change-function, but I can not get that to
;; work.

;;; Change Log:
;;
;; Sat Jan 01 00:00:00 1994
;;      * Created.

;;; Code:

;; Fill this buffer like a maniac?
(defvar maniac-fill-mode nil)
(make-variable-buffer-local ' maniac-fill-mode) 

(defun do-maniac-fill ()
  ;; Do an auto fill if maniac fill mode is on.
  (if (or (not maniac-fill-mode)
	  (and (eolp)
	       (memq (preceding-char)
		     '(0 ?\n ?\t ? ))))
      ()
    (fill-paragraph nil)))

;; Call maniac fill after each command.
(add-hook 'post-command-hook 'do-maniac-fill)

;; Add to mode line.
(or (assq 'maniac-fill-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(maniac-fill-mode (" Maniac"))
		minor-mode-alist)))

;;;###autoload
(defun maniac-fill-mode (&optional arg)
  "Toggle maniac fill mode.
With prefix arg, turn maniac fill mode on iff arg is positive.

When maniac fill mode is on, the current paragraph will be formatted
after each command."
  (interactive "P")
  (setq maniac-fill-mode (not (or (and (null arg) maniac-fill-mode)
				  (<= (prefix-numeric-value arg) 0))))
  (set-buffer-modified-p (buffer-modified-p)))

(provide 'maniac)

;;; maniac.el ends here
