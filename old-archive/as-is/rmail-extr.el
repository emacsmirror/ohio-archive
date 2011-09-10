;To: unix-emacs@bbn.com
;Date: 29 Nov 88 03:06:18 GMT
;From: "Eric L. Raible" <EW09.NAS.NASA.GOV!raible@eddie.mit.edu>
;Subject: rmail-extract-rejected-message
;
;
;I think that anyone who uses rmail will find this to be a welcome addition.
;
(defvar mail-unsent-separator "^   ----- Unsent message follows -----$")

(setq rmail-mode-hook
      '((lambda ()
	  (define-key rmail-mode-map "e" 'rmail-extract-rejected-message))))

(defun rmail-extract-rejected-message ()
  "Edit a mail message which is based on the contents of the current message.

For a message rejected by the mail system, extract the interesting headers and
the body of the original message; otherwise copy the current message."
  (interactive)
  (rmail-beginning-of-message)
  (re-search-forward mail-unsent-separator nil t)
  (let* ((to   (rmail-get-field "To"))
	 (subj (rmail-get-field "Subject"))
	 (irp2 (rmail-get-field "In-reply-to"))
	 (cc   (rmail-get-field "Cc"))
	 (orig-message (buffer-substring
			(progn (search-forward "\n\n") (point))
			(point-max)))
	 (mail-setup-hook (function (lambda ()
				      (goto-char (point-max))
				      (insert orig-message)
				      (mail-to)))))
    (rmail-beginning-of-message)
    (mail-other-window nil to subj irp2 cc (current-buffer))))

(defun rmail-get-field (field-name)
  (save-excursion
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^" field-name ": ") nil t)
	  (buffer-substring (point) (progn (end-of-line) (point)))))))

