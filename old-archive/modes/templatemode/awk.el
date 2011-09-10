;;; awk.el -- Awk mode for Gnuemacs
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'awk)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar awk-mode-map nil
  "Key map for Awk mode."
  ) ; awk-mode-map

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun awk-mode ()
  "Fundamental mode plus Awk templates:
\\{awk-mode-map}
"
  (interactive)
					; Local Variables
  (let ()
					; Body
    (setq major-mode 'awk-mode)
    (setq mode-name "Awk")
    (template-mode)
    (setq awk-mode-map (current-local-map))
    ) ; let
  ) ; defun awk-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of awk.el
