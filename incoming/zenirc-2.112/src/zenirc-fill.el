;;; zenirc-fill.el --- fill messages in zenirc

;; Copyright (C) 1995 Noah S. Friedman
;; Copyright (C) 1995, 1996, 1997, 1998 Per Persson

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1995-03-16

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
;;; Code:

(require 'zenirc)

(defvar zenirc-fill-mode nil
  "*If non-nil, then fill messages fitting `zenirc-fill-message-categories'.
This is buffer-local.")
(make-variable-buffer-local 'zenirc-fill-mode)

(defvar zenirc-fill-region-function 'zenirc-wrap-region
  "*Function to use for filling.")

(defvar zenirc-fill-prefix " | "
  "*String for filling to insert at front of new lines, or nil for none.")

(defvar zenirc-fill-static 12
  "*How many chars into first the line the first word should end.
This will look totally ridicolous if you don't strip away !user@host from
PRIVMSG and NOTICE")

(defvar zenirc-fill-column (- (window-width) 2)
  "*Column beyond which line-wrapping should happen in zenirc buffers.")

(defvar zenirc-fill-message-categories
  '(privmsg privmsg_you notice notice_you privmsg_nochannel 
	    notice_nochannel ctcp_action)
  "*ZenIRC message categories to fill as paragraphs.
This should be a list consisting of symbols corresponding to the type of
messages in the message catalog which should be filled as paragraphs.
For example, private messages (`privmsg') and notices (`notice') are good
choices.

If this variable is set to `t', then all messages are filled.
If this variable is set to `nil', then no messages are filled.")

(defvar zenirc-fill-nonstandard-message-categories-p nil
  "If non-nil, then fill messages that are not in a standard category.
That is, fill messages which did not originate from the message catalog,
and thus have no category symbol.")


(defun zenirc-fill-mode (&optional prefix)
  "Enable or disable line wrapping of irc messages.

A negative prefix argument disables this mode.
A non-negative prefix argument enables it.
If no prefix argument is given, toggle the current state of the mode.

The user may also enable or disable this mode simply by setting the
variable of the same name."
  (interactive "P")
  (cond
   ((null prefix)
    (setq zenirc-fill-mode (not zenirc-fill-mode)))
   ((>= (prefix-numeric-value prefix) 0)
    (setq zenirc-fill-mode t))
   (t
    (setq zenirc-fill-mode nil)))
  (cond ((not (interactive-p)))
        (zenirc-fill-mode
         (message "zenirc-fill-mode is enabled"))
        (t
         (message "zenirc-fill-mode is disabled")))
  zenirc-fill-mode)

;; "normal" filling function.
(defun zenirc-fill-region (beg end win-width)
  (let* ((fill-prefix zenirc-fill-prefix)
         (fill-column zenirc-fill-column))
    (fill-region-as-paragraph (point-min) (1- (point-max))))
    ;; this filling function adds an unnecessary newline.
    (goto-char (point-max))
    (delete-char -1))

;; This is like the normal filling routines except that it doesn't squash
;; whitespace (except at line breaks).  This will make ascii barphics and
;; other random spaced crap easier to see.
(defun zenirc-wrap-region (beg end win-width)
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (let* ((ws "[ \t]")
               (prefix-length (length zenirc-fill-prefix))
               (fill-column (max (or zenirc-fill-column
                                     (- win-width 2))
                                 prefix-length))
               line-beg)
          (goto-char beg)
          (while (< (point) (point-max))
            (beginning-of-line)
            (setq line-beg (+ (point) prefix-length))
            (cond ((< fill-column (- (point-max) (point)))
                   (forward-char fill-column)
                   (cond ((or (memq (char-after (point)) '(32 ?\t))
                              (re-search-backward ws line-beg t)
                              (re-search-forward ws (point-max) t))
                          (just-one-space)
                          (delete-char -1)
                          (insert "\n" (or zenirc-fill-prefix "")))
                         (t
                          (goto-char (point-max)))))
                  (t
                   (goto-char (point-max))))))))))

;; This adds whitespaces before the first word on every line but the first
;; one to match up with the length of the first word on the first line.
(defun zenirc-wrap-region-dynamic (beg end win-width)
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
	;; trim trailing whitespace
	(goto-char (- end 1))
	(just-one-space)
	(if (< (+ beg 1) (point))
	    (delete-char -1))
	(setq end (point))

        (narrow-to-region beg end)
	(goto-char beg)
        (let* ((ws "[ \t]")
	       ; set the length of the prefix
               (prefix-length 
		(or (and (search-forward " " end t)
			 (- (match-end 0) beg))
		    0))
		
	       ; how many columns zenirc-wrap-region should keep inside
	       (fill-column (max (or zenirc-fill-column
				     (- win-width 2))
                                 (or prefix-length 0)))
               line-beg)
          (goto-char beg)
	  ; start wrapping of the actual message, after first word
          (while (< (point) (point-max))
            (beginning-of-line)
            (setq line-beg (+ (point) prefix-length))
            (cond ((< fill-column (- (point-max) (point)))
                   (forward-char fill-column)
                   (cond ((or (memq (char-after (point)) '(32 ?\t))
                              (re-search-backward ws line-beg t)
                              (re-search-forward ws (point-max) t))
			  ; remove all spaces before adding newline
			  (just-one-space)
			  (delete-char -1)
			  (insert 
			   "\n"
			   (make-string prefix-length
					(string-to-char " "))))
			 (t
                          (goto-char (point-max)))))
                  (t
                   (goto-char (point-max))))))))))

;; This adds whitespaces before the first word on every line so that the
;; second word on the first line and the first word on every other line
;; match up with all the other lines (not just in this region).
(defun zenirc-wrap-region-static (beg end win-width)
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
	(goto-char beg)
        (let* ((ws "[ \t]")
	       ; set the length of the prefix
               (prefix-length 
		(and (search-forward " " end t)
		     (- (match-end 0) beg)))
	       ; how many columns zenirc-wrap-region should keep inside
	       (fill-column (max (or zenirc-fill-column
                                     (- win-width 2))
                                 prefix-length))
               line-beg)
          (goto-char beg)
	  (if (<= prefix-length zenirc-fill-static)
	      (progn
		(insert (make-string 
			 (- zenirc-fill-static prefix-length)
			 (string-to-char " ")))
		; update the length of the prefix, as it is static
		(setq prefix-length zenirc-fill-static)))
	  ; start wrapping of the actual message, after first word
          (while (< (point) (point-max))
            (beginning-of-line)
            (setq line-beg (+ (point) prefix-length))
            (cond ((< fill-column (- (point-max) (point)))
                   (forward-char fill-column)
                   (cond ((or (memq (char-after (point)) '(32 ?\t))
                              (re-search-backward ws line-beg t)
                              (re-search-forward ws (point-max) t))
			  ; remove all spaces before adding newline
			  (just-one-space)
			  (delete-char -1)
			  (insert 
			   "\n"
			   (make-string zenirc-fill-static
					  (string-to-char " "))))
			 (t
                          (goto-char (point-max)))))
                  (t
                   (goto-char (point-max))))))))))


(defvar zenirc-window-last-width 80)
(defun zenirc-fill-message (proc sym string)
  (let* ((w (and proc
		 (get-buffer-window (process-buffer proc) t)))
	 (win-width (if w
			(setq zenirc-window-last-width (window-width w))
		      zenirc-window-last-width)))
    (and zenirc-fill-mode
	 (cond ((eq zenirc-fill-message-categories t))
	       ((null sym)
		zenirc-fill-nonstandard-message-categories-p)
	       ((memq sym zenirc-fill-message-categories))
	       (t nil))
	 (funcall zenirc-fill-region-function 
		  (point-min) (point-max) win-width))))

(defvar zenirc-fill-outgoing-mode nil
  "*If non-nil, then fill outgoing PRIVMSG/NOTICEs.
This is buffer-local.")
(make-variable-buffer-local 'zenirc-fill-outgoing-mode)

(defvar zenirc-fill-outgoing-prefix "-> "
  "*What do add before outgoing lines.")

(defun zenirc-fill-outgoing-mode (&optional prefix)
  "Enable or disable line wrapping of outgoing PRIVMSG/NOTICEs.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may also enable or disable this mode simply by setting the
variable of the same name."
  (interactive "P")
  (cond
   ((null prefix)
    (setq zenirc-fill-outgoing-mode (not zenirc-fill-outgoing-mode)))
   ((>= (prefix-numeric-value prefix) 0)
    (setq zenirc-fill-outgoing-mode t))
   (t
    (setq zenirc-fill-outgoing-mode nil)))
  (cond ((not (interactive-p)))
        (zenirc-fill-outgoing-mode
         (message "zenirc-fill-outgoing-mode is enabled"))
        (t
         (message "zenirc-fill-outgoing-mode is disabled")))
  zenirc-fill-outgoing-mode)

(defun zenirc-fill-outgoing (beg end str)
  (if zenirc-fill-outgoing-mode
      (save-excursion
	(goto-char beg)
	; if the outgoing line isn't a command, just insert the prefix
	(if (not (= (aref str 0) zenirc-command-char))
	    (progn
	      (insert zenirc-fill-outgoing-prefix)
	      (save-restriction
		(narrow-to-region beg end)
		(funcall zenirc-fill-region-function
			 beg end (window-width))))
	  (let 
	      ; if the outgoing line was a command, parse out the
	      ; actual command and the argument/text.
	      ((command (car (zenirc-parse-firstword 
			      (substring str 1))))
	       (text (cdr (zenirc-parse-firstword str))))
	    (cond 
	     ; if the command was one of 'm', 'msg' or 'privmsg',
	     ; do some fanzy parsing and change the line a bit.
	     ((string-match "\\(^m$\\|^msg$\\|^privmsg$\\)" command)
	      (delete-region beg end)
	      (insert zenirc-fill-outgoing-prefix
		      "*" (car (zenirc-parse-firstword text)) "* "
		      (cdr (zenirc-parse-firstword text)))
	      (save-restriction
		(narrow-to-region beg end)
		(funcall zenirc-fill-region-function 
			 beg end (window-width)))))))
)))


(defvar zenirc-command-resize-hook '(zenirc-command-resize))

;; /resize [width]
(defun zenirc-command-resize (proc cmd)
  (if (string= (cdr cmd) "")
      (setq zenirc-fill-column (- (window-width) 2))
    (setq zenirc-fill-column (cdr cmd))))

(provide 'zenirc-fill)

(zenirc-add-hook 'zenirc-message-hook 'zenirc-fill-message)
(zenirc-add-hook 'zenirc-send-line-hook 'zenirc-fill-outgoing)

(or (assq 'zenirc-fill-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list 'zenirc-fill-mode " Zfill") minor-mode-alist)))

;;; zenirc-fill.el ends here

