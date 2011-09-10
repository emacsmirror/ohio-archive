;;; pascal.el -- Pascal mode for Gnuemacs
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'pascal)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar pascal-mode-map nil
  "Keymap for Pascal Mode."
  ) ; pascal-mode-map

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun pascal-mode ()
  "Fundamental mode plus Pascal templates:
\\{pascal-mode-map}
"
  (interactive)
					; Local Variables
  (let ()
					; Body
    (setq major-mode 'pascal-mode)
    (setq mode-name "Pascal")
    (template-mode)
    (setq pascal-mode-map (current-local-map))
    ) ; let
  ) ; defun pascal-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of pascal.el
