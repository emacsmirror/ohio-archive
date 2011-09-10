;;; zenirc-color.el --- color messages in zenirc

;; Copyright (C) 1996 John Wiegley
;; Copyright (C) 1996, 1998 Per Persson

;; Author: John Wiegley <johnw@borland.com>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc,extensions
;; Created: 1996-05-22

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; This ZenIRC extensions allows you to colorize input from specific
;;; sources.  Use the "/color #victim <COLOR>" command to start
;;; colorizing a certain victim's output.

;;; Code:

(require 'zenirc)

(defun zenirc-color-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((color-lame-args . "[info] %s: not enough arguments.")
     (color-nonexistant . "[info] %s is not an allowed color.")
     (color-not-found . "[info] %s: not found in color list."))))

(defvar zenirc-color-mode nil
  "*If non-nil, then color messages fitting `zenirc-color-message-categories'.
This is buffer-local.")
(make-variable-buffer-local 'zenirc-color-mode)

(defvar zenirc-color-region-function 'zenirc-colorize-region
  "*Function to use for coloring.")

(defvar zenirc-facename-index 1
  "Used for creating new zenirc face names")

(defvar zenirc-color-alist
  '()
  "*A list of elements, each of which is (REGEXP FACE), where both are
strings.  Any string containing REGEXP in the output will have it's face
property set to FACE.")
(make-variable-buffer-local 'zenirc-color-alist)

(defvar zenirc-color-message-categories
  '(privmsg privmsg_you notice notice_you ctcp_action)
  "*ZenIRC message categories to color.  This should be a list
consisting of symbols corresponding to the type of messages in the
message catalog which should be colored.  For example, private
messages (`privmsg') and notices (`notice') are good choices.

If this variable is set to `t', then all messages are colored.
If this variable is set to `nil', then no messages are colored.")
(make-variable-buffer-local 'zenirc-color-message-categories)

(defvar zenirc-color-nonstandard-message-categories-p nil
  "If non-nil, then color messages that are not in a standard category.
That is, color messages which did not originate from the message catalog,
and thus have no category symbol.")
(make-variable-buffer-local 'zenirc-color-nonstandard-message-categories-p)

;; Check whether a given color really exists as a color.
(defun zenirc-color-name-p (color)
  (let ((version (emacs-version)))
    (cond ((string-match "XEmacs" version)
	   (valid-color-name-p color))
	  ((string-match "GNU" version)
	   (x-color-defined-p color))
	  (t
	   nil))))

(defun zenirc-color-mode (&optional prefix)
  "Enable or disable colorization of irc messages.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may also enable or disable this mode simply by setting the
variable of the same name."
  (interactive "P")
  (cond
   ((null prefix)
    (setq zenirc-color-mode (not zenirc-color-mode)))
   ((>= (prefix-numeric-value prefix) 0)
    (setq zenirc-color-mode t))
   (t
    (setq zenirc-color-mode nil)))
  (cond ((not (interactive-p)))
        (zenirc-color-mode
         (message "zenirc-color-mode is enabled"))
        (t
         (message "zenirc-color-mode is disabled")))
  zenirc-color-mode)

(defvar zenirc-face nil)

(defun zenirc-colorize-region (beg end)
  (interactive "r")
  (save-match-data
    (save-excursion
      (goto-char beg)
      (mapcar
       (function
        (lambda (elem)
          (if (re-search-forward (car elem) end t)
              (put-text-property beg end 'face (car (cdr elem))))))
       zenirc-color-alist))))

(defun zenirc-color-message (proc sym string)
  (and zenirc-color-mode
       (cond ((eq zenirc-color-message-categories t))
             ((null sym)
              zenirc-color-nonstandard-message-categories-p)
             ((memq sym zenirc-color-message-categories))
             (t nil))
       (funcall zenirc-color-region-function (point-min) (- (point-max) 1))))


(defvar zenirc-command-color-hook '(zenirc-command-color))

;; /color #victim <color>
(defun zenirc-command-color (proc cmd)
  (let* ((arg (zenirc-parse-firstword (cdr parsedcmd)))
         (victim (format "%s" (car arg)))
         (color (cdr arg)))
    (if (or (string= "" victim)
            (string= "" color))
        (zenirc-message proc 'color-lame-args "/color")
      (if (zenirc-color-name-p color)
	  (let ((newface (make-symbol
			  (concat "zenirc-color-face-"
				  (number-to-string zenirc-facename-index)))))
	    (setq zenirc-facename-index (1+ zenirc-facename-index))
	    (copy-face 'default newface)
	    (set-face-foreground newface color)
	    (setq zenirc-color-alist
		  (cons (list victim newface)
			zenirc-color-alist)))
	(zenirc-message proc 'color-nonexistant color)))))

(defvar zenirc-command-uncolor-hook '(zenirc-command-uncolor))

;; /uncolor #victim
(defun zenirc-command-uncolor (proc cmd)
  (let* ((arg (zenirc-parse-firstword (cdr parsedcmd)))
         (victim (format "%s" (car arg))))
    (if (string= "" victim)
        (zenirc-message proc 'color-lame-args "/uncolor"))
    (let ((pointer zenirc-color-alist) last found)
      (while pointer
        (if (string= (car (car pointer)) victim)
            (progn
              (setq found t)
              (if (= (length zenirc-color-alist) 1)
                  (setq zenirc-color-alist nil)
                (if last
                    (setcdr last (cdr pointer))
                  (setq zenirc-color-alist (cdr pointer))))))
        (setq last pointer)
        (setq pointer (cdr pointer)))
      (if (not found)
          (zenirc-message proc 'color-not-found victim)))))

(provide 'zenirc-color)

(zenirc-add-hook 'zenirc-message-hook 'zenirc-color-message)

(or (assq 'zenirc-color-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list 'zenirc-color-mode " Zcolor") minor-mode-alist)))

(zenirc-color-install-message-catalogs)

;;; zenirc-color.el ends here
