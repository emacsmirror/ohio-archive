;From: sra@lcs.mit.edu (Rob Austein)
;Newsgroups: gnu.emacs.bug
;Subject: Suggested addition: Buffer-menu-toggle-read-only
;Message-ID: <SRA.89Aug30223544@mintaka.lcs.mit.edu>
;Date: 31 Aug 89 02:35:46 GMT
;Distribution: gnu
;Organization: ITS Preservation Society
;Lines: 23
;
;Here's a function I wanted for a while and finally got to writing.  It
;defines a command to be used in Buffer Menu mode to toggle the
;read-only status of the buffer named on the current line.

(defun Buffer-menu-toggle-read-only ()
  "Toggle read-only status of buffer on this line."
  (interactive)
  (let (char)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (toggle-read-only)
      (setq char (if buffer-read-only ?% ? )))
    (save-excursion
      (beginning-of-line)
      (forward-char 2)
      (if (/= (following-char) char)
	  (let (buffer-read-only)
	    (delete-char 1)
	    (insert char))))))

(define-key Buffer-menu-mode-map "%" 'Buffer-menu-toggle-read-only)

;--Rob Austein, MIT
