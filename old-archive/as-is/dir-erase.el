;To: unix-emacs@bbn.com
;Date: 15 Feb 89 06:52:00 GMT
;From: David Gudeman <gudeman@arizona.edu>
;Subject: Re: Selective Dired
;
;In article  <5318@lifia.imag.fr> phs@lifia.imag.fr (Philippe Schnoebelen) writes:
;>
;>Is there any hook, regexp, alist or whatever trick available to tell Dired
;>not to list files such and such ? ... Surely,
;>there must exist something like that !
;
;I don't think there was.  But here is something you can use:
;----------------------------------------------------------------
;; David Gudeman gudeman@arizona.edu

(define-key dired-mode-map "l" 'dired-erase-extensions)

;; Uncomment this if you want dired-erase-extensions to work
;; automatically when you call dired.
;(fset 'dired-readin
;      (append (symbol-function 'dired-readin)
;	      (cdr (cdr (cdr (symbol-function 'dired-erase-extensions))))))

(defun dired-erase-extensions ()
  "Remove all files that match completion-ignored-extensions.  If
dired-ignored-extensions is defined, then use that instead."
  (interactive)
  (let ((ls (if (boundp 'dired-ignored-extensions)
		dired-ignored-extensions
	      completion-ignored-extensions))
	(buffer-read-only))
    (save-excursion
      (goto-char (point-min))
      (while ls
	(delete-matching-lines
	 (concat (regexp-quote (car ls)) "[*|=/]?$"))
	(setq ls (cdr ls))))))

