;Date: Fri, 17 Feb 89 12:09:17 EST
;From: mlittman@wind.bellcore.com (Michael S. Littman)
;To: bug-gnu-emacs@prep.ai.mit.edu
;Subject: mail-plus mode
;
;Hi,
;
;	Someone suggested a fews days ago (in one of these Emacs
;lists...) that mail mode allow the possibility of multiple outgoing
;mail messages.  I implemented something which does the same thing by
;using widening-narrowing in the *mail* buffer.  It changes the
;existing package only minimally and actually comes in handy quite
;often.
;
;-Michael
;------------------
;;;
;;; mail-plus.el: Allows multiple active outgoing mail messages.
;;;

; Use, copy, and alter this program at will but don't blame me if it
;  doesn't work or erase this notice.

(require 'sendmail) ; This allows this file to augment sendmail.el.

; The main idea is that narrowing in on a single mail message allows
;   multiple messages in *mail*.  Extra bindings are used to move
;   around in the narrowed buffer.  Should work as usual for people
;   who don't use it. (Michael S. Littman 11/88)

; C-c C-a (mail+append)
; C-c C-d (mail+delete)
; C-c C-p (mail+previous)
; C-c C-n (mail+next)
; C-c < (mail+first)
; C-c > (mail+last)
; C-c C-c (mail+send-and-exit)

(defvar mail+marker "--new message follows this line--"
  "Separates mail messages within the *mail* buffer.")

(defun mail+narrow ()
  "Narrows the current buffer on a single mail message."
  (interactive)
  (let (top bottom)
    (widen) ; Just in case.
    (save-excursion
      (if (search-backward mail+marker (point-min) 1)
	  (setq top (+ 1 (length mail+marker) (point)))
	(setq top (point))))
    (save-excursion
      (if (search-forward mail+marker (point-max) 1)
	  (setq bottom (+ (- (length mail+marker)) (point)))
	(setq bottom (point))))
    (narrow-to-region top bottom)))

(defun mail+next ()
  "Moves to the next message."
  (interactive)
  (widen)
  (if (not (search-forward mail+marker (point-max) 1))
      (message "No next message."))
  (mail+narrow)
  (goto-char (point-max)))

(defun mail+previous ()
  "Moves to the previous message."
  (interactive)
  (widen)
  (if (not (search-backward mail+marker (point-min) 1))
      (message "No previous message."))
  (mail+narrow)
  (goto-char (point-max)))

(defun mail+append ()
  "Add a new message to the end."
  (interactive)
  (widen)
  (goto-char (point-max))
  (insert-string mail+marker)
  (insert-string "\n")
  (mail+narrow)
  (mail-setup nil nil nil nil nil))

(defun mail+first ()
  "Moves to the first message."
  (interactive)
  (widen)
  (goto-char (point-min))
  (mail+narrow)
  (goto-char (point-max)))

(defun mail+last ()
  "Moves to the last message."
  (interactive)
  (widen)
  (goto-char (point-max))
  (mail+narrow)
  (goto-char (point-max)))

(defun mail+delete (&optional no-message)
  "Delete the current message."
  (interactive)
  (let (top bottom last first)
    (widen)
    (setq first nil)
    (save-excursion
      (if (search-backward mail+marker (point-min) 1)
	  (setq top (+ 1 (length mail+marker) (point)))
	(progn
	  (setq top (point))
	  (setq first t))))
    (setq last nil)
    (save-excursion
      (if (search-forward mail+marker (point-max) 1)
	  (setq bottom (+ 1 (point)))
	(progn
	  (setq bottom (point))
	  (setq last t))))
    (if (and last (not first))
	(setq top (+ top (- (length mail+marker)) -1)))
    (if (and first last)
	(if (not no-message) (message "Last message."))
      (delete-region top bottom))
    (mail+narrow)))

(defun mail+send-and-exit (arg)
  "Send message like mail-send, then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window.  Like mail-send-and-exit
except does a mail+delete."
  (interactive "P")
  (mail-send)
  (mail+delete t)
  (bury-buffer (current-buffer))
  (if (and (not arg)
	   (not (one-window-p))
	   (save-excursion
	     (set-buffer (window-buffer (next-window (selected-window) 'not)))
	     (eq major-mode 'rmail-mode)))
      (delete-window)
    (switch-to-buffer (other-buffer (current-buffer)))))

; Key bindings
(define-key mail-mode-map "\C-c\C-a" 'mail+append)
(define-key mail-mode-map "\C-c\C-d" 'mail+delete)
(define-key mail-mode-map "\C-c\C-p" 'mail+previous)
(define-key mail-mode-map "\C-c\C-n" 'mail+next)
(define-key mail-mode-map "\C-c<" 'mail+first)
(define-key mail-mode-map "\C-c>" 'mail+last)
(define-key mail-mode-map "\C-c\C-c" 'mail+send-and-exit)

