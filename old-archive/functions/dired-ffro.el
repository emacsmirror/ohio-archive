;From: sra@lcs.mit.edu (Rob Austein)
;Newsgroups: gnu.emacs.bug
;Subject: Suggested addition: dired-find-file-read-only
;Message-ID: <SRA.89Aug31105224@mintaka.lcs.mit.edu>
;Date: 31 Aug 89 14:52:26 GMT
;Distribution: gnu
;Organization: ITS Preservation Society
;Lines: 18
;
;Philippe Schnoebelen pointed out that the following would be useful.
;It defines a function that can be used in Dired mode to find a file
;read-only.  Since "e" and "f" run the same function (dired-find-file)
;in the vanilla dired keymap, "e" ("examine") might be a reasonable
;place to hang this.

(defun dired-find-file-read-only ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file-read-only (dired-get-filename)))

;; Be careful here if you already have a dired-mode-hook set....
(setq dired-mode-hook
      (function
       (lambda ()
	 (define-key dired-mode-map "e" 'dired-find-file-read-only))))

;--Rob Austein, MIT
