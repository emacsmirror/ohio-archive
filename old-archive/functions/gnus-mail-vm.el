;; LCD Archive Entry:
;; gnus-mail-vm|Joe Wells|jbw@cs.bu.edu|
;; Allow using VM's mail sending instead of the default version from GNUS.|
;; 1992-08-16||~/functions/gnus-mail-vm.el.Z|

;;(setq gnus-mail-reply-method 'gnus-mail-reply-using-mail)
(setq gnus-mail-reply-method 'gnus-mail-reply-using-vm)

(defun gnus-mail-reply-using-vm (&optional yank)
  "Compose reply mail using VM.
Optional argument YANK means yank original article."
  ;; Ugly hack to work around fact that vm-mail-internal always uses
  ;; switch-to-buffer.  Duplicate old GNUS functionality.
  (save-excursion
    (pop-to-buffer " *dummy buffer*"))
  (save-restriction
    (narrow-to-region (point-min) (progn (goto-line (point-min))
					 (search-forward "\n\n")
					 (- (point) 1)))
    (vm-mail-internal
     (format "reply to %s:%d" gnus-newsgroup-name gnus-current-article)     
     (or (mail-fetch-field "reply-to")
	 (mail-fetch-field "from"))
     (mail-fetch-field "subject")
     nil nil
     (concat (mail-fetch-field "references") " "
	     (mail-fetch-field "message-id"))
     (mail-fetch-field "newsgroups")))
  (gnus-handle-references)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer gnus-Article-buffer)
  (if yank
      (let ((last (point)))
	(goto-char (point-max))
	;; This will delete window on *Article* buffer
	(mail-yank-original nil)
	(goto-char last)
	)))

;; The references filling functionality should be moved to vm-mail-internal
(defun gnus-handle-references (&rest additional-references)
  (save-excursion
    (mail-position-on-field "References")
    (insert " " (mapconcat 'identity additional-references " "))
    (save-restriction
      (narrow-to-region (1+ (point))
			(progn (re-search-backward "^[^ \t]")
			       (point)))
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]+" nil 'move)
	(replace-match " "))
      (delete-char -1)
      (insert ?\n)
      (let ((fill-column 79)
	    (fill-prefix " "))		; save space on screen
	;; Fold long references line to follow RFC1036.
	;; The region must end with a newline to fill the region
	;; without inserting extra newline.
	(fill-region-as-paragraph (point-min) (point-max))))))
