(load-library "hooks")		; Mark Weissman's hooks library

(defun save-to-message-buffer (string &rest args)
  "Before-hook for (message) and (error).
Appends a copy of each minibuffer message to the buffer *messages*."
  (interactive)
  (save-window-excursion
    (set-buffer (get-buffer-create "*messages*"))
    (goto-char (point-max))
    (insert-string (format string args))
    (insert-char ?\n 1)))

(mdw:add-hooks message)
(setq mdw:message-before-hooks '(save-to-message-buffer))

(mdw:add-hooks error)
(setq mdw:error-before-hooks '(save-to-message-buffer))
