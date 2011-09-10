;;; mew-toolbar-frame.el	-*-Emacs-Lisp-*-
;;;
;;; Commentary:
;;; Run Mew from toolbar in a separate frame.
;;;
;;; Keywords:
;;; Mew Xemacs ToolBar Frame
;;;
;;; Time-stamp: <99/07/02 16:37:17 jado@sophia>

;;; How to use:
;;; Require it when initialize.

;;; Code:

(provide 'mew-toolbar-frame)

;;;

(setq toolbar-mail-reader 'Mew)
(setq toolbar-mail-commands-alist
      (cons '(Mew . toolbar-mew) toolbar-mail-commands-alist))

(defvar toolbar-mail-use-separate-frame t
  "*Whether Mew is invoked in a separate frame.")
(defvar toolbar-mail-frame nil
  "The frame in which Mew is displayed.")
(defvar toolbar-mail-frame-plist nil
  "*The properties of the frame in which mail is displayed.")
(define-obsolete-variable-alias 'toolbar-mail-frame-properties
  'toolbar-mail-frame-plist)

(defun toolbar-mew ()
  "Run Mew in a separate frame."
  (interactive)
  (if (not toolbar-mail-use-separate-frame)
      (mew)
    (unless (frame-live-p toolbar-mail-frame)
      (setq toolbar-mail-frame (make-frame toolbar-mail-frame-plist))
      (add-hook 'mew-suspend-hook
	(lambda ()
	  (when (frame-live-p toolbar-mail-frame)
	    (if (cdr (frame-list))
		(delete-frame toolbar-mail-frame))
	    (setq toolbar-mail-frame nil))))
      (add-hook 'mew-quit-hook
	(lambda ()
	  (when (frame-live-p toolbar-mail-frame)
	    (if (cdr (frame-list))
		(delete-frame toolbar-mail-frame))
	    (setq toolbar-mail-frame nil))))
      (select-frame toolbar-mail-frame)
      (mew))
    (when (framep toolbar-mail-frame)
      (when (frame-iconified-p toolbar-mail-frame)
        (deiconify-frame toolbar-mail-frame))
      (select-frame toolbar-mail-frame)
      (raise-frame toolbar-mail-frame))))

;;; mew-toolbar-frame.el ends here
