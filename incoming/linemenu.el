;       $Id: linemenu.el 1.8 1995/04/07 12:32:58 brodie Exp $

;;; linemenu.el - Hook function to highlight current line in buffer

;; Author:   Bill Brodie <wbrodie@panix.com>
;; Version:  $Revision: 1.8 $
;; Date:     $Date: 1995/04/07 12:32:58 $
;;
;; To try it out, put this in your .emacs file:
;;  Turn on highlighting for Dired (C-x d) and list-buffers (C-x C-b)
;;  (require 'linemenu)
;;  (defadvice list-buffers (after highlight-line activate)
;;    (save-excursion
;;      (set-buffer "*Buffer List*")
;;      (linemenu-initialize)))
;;  (add-hook 'dired-after-readin-hook 'linemenu-initialize)

(defun linemenu-initialize ()
  "Turn on highlighting of the current line in this buffer."
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'linemenu-highlight-current-line)
  (linemenu-highlight-current-line))

(defvar linemenu-overlay nil "Highlight for current line")
(make-variable-buffer-local 'linemenu-overlay)

(defun linemenu-init-overlay ()
  ;; Create a highlighting overlay for the current line
  (setq linemenu-overlay (make-overlay 1 1))   ; Hide it for now
  (overlay-put linemenu-overlay 'hilit t)
  (overlay-put linemenu-overlay 'face 'region)
  (overlay-put linemenu-overlay 'priority 20))

(defun linemenu-highlight-current-line ()
  ;; Move highlight to the current line
  (or linemenu-overlay (linemenu-init-overlay))
  (let ((start (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (forward-line 1) (point))))
    (delete-overlay linemenu-overlay)
    (move-overlay linemenu-overlay start end)
    (overlay-put linemenu-overlay 'hilit t)))

(provide 'linemenu)

;;; End of linemenu.el
