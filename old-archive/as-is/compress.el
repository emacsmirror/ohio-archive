;;;Date: 20 Jul 88 17:34:23 GMT
;;;From: Stephen Gildea <ALEXANDER.BBN.COM!gildea%eddie.mit.edu.uucp@BBN.COM>
;;;Subject: Emacs on compressed files

;;;Here's my handling of compressed files.  It works such that if you try
;;;to find a file, and it exists only in compressed form, it is
;;;uncompressed into the buffer.  No facility for saving the file is
;;;provided since the file on disk is never uncompressed.

;;; < Stephen

;;; Read in compressed files automatically.
;;; By gildea@bbn.com 20 Jul 88

(defun read-file-compressed ()
  "Called by find-file-not-found-hooks to try reading the file
by adding `.Z' to the name and uncompressing it.  Note that if you do
completion on the filename in the minibuffer this will not work,
because then Emacs will find the existing `filename.Z' instead of
failing to find `filename' and being forced to uncompress it.  By gildea."
  (if (file-exists-p (concat buffer-file-name ".Z"))
      (progn
	(call-process "zcat" nil t nil buffer-file-name)
	(goto-char (point-min))
	(setq error nil)
	(set-buffer-modified-p nil)
	(message "uncompressed %s" buffer-file-name))))

;;; I don't have a mechanism for writing compressed files,
;;; so the buffer is set read-only.
(defun after-find-file-compressed ()
  "If we had to uncompress a file, set it read only"
  (if (and (not (file-exists-p buffer-file-name))
	   (file-exists-p (concat buffer-file-name ".Z")))
      (setq buffer-read-only t)))

(setq find-file-not-found-hooks
      (cons 'read-file-compressed find-file-not-found-hooks))

(setq find-file-hooks
      (cons 'after-find-file-compressed find-file-hooks))

;;; And make it work in dired, too.

(defun dired-find-file-compressed ()
  "In dired, find the uncompressed version of this file."
  (interactive)
  (let* ((dired-name (dired-get-filename))
	 (filename (substring dired-name 0 (string-match "\\.Z$" dired-name))))
    (find-file filename)))

(defun dired-hook ()
  (define-key dired-mode-map "z" 'dired-find-file-compressed))

(setq dired-mode-hook 'dired-hook)
