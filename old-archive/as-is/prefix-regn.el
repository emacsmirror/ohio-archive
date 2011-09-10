(defun prefix-region (begin end prefix)
   "Prefix every line in the region between BEGIN and END with the string PREFIX."
   (interactive "r\nsPrefix: ")
   (save-excursion
      (save-restriction
         (narrow-to-region begin end)
         (goto-char (point-min))
         (while (re-search-forward "^" nil t)
            (replace-match prefix t t)))))
