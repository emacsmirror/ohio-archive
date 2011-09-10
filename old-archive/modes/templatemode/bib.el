;;; bib.el -- Bib mode for Gnuemacs
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'bib)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar bib-mode-map nil
  "Keymap for Bib Mode."
  ) ; bib-mode-map

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun bib-mode ()
  "Fundamental mode plus Bib templates:
\\{bib-mode-map}
"
  (interactive)
					; Local Variables
  (let ()
					; Body
    (setq major-mode 'bib-mode)
    (setq mode-name "Bib")
    (template-mode)
    (setq bib-mode-map (current-local-map))
    ) ; let
  ) ; defun bib-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of bib.el
