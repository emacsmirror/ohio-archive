;From: montnaro@copernicus.crd.ge.com (Skip Montanaro)
;Newsgroups: gnu.emacs
;Subject: Re: BABYL to unix mail format converter?
;Message-ID: <MONTNARO.89Aug16222628@copernicus.crd.ge.com>
;Date: 17 Aug 89 02:26:28 GMT
;References: <31142@cornell.UUCP>
;Reply-To: <montanaro@sprite.crd.ge.com> (Skip Montanaro)
;Distribution: gnu
;Organization: /home/kreskin/u0/montnaro/.organization
;Lines: 91
;
;The following ain't perfect, but it got me over the hump. It's just a
;throwaway. Don't send me bugs, since I don't expect to reuse it. After
;running it, proofread the generated mail files!
;
;---------- cut here ---------- cut here ---------- cut here ---------- 

;; Babyl-to-mail.el (written late (Late!) at night.

(defun convert-babyl-buffer-to-mail ()
  "Quick-n-dirty function to convert current buffer back to Mail format."
  (interactive)
  (delete-babyl-header)
  (goto-char (point-min))
  (while (looking-at "\C-l")
    (convert-babyl-message-to-mail)))

(defun delete-babyl-header ()
  "Nuke \"BABYL OPTIONS:\" to first \C-_."
  (goto-char (point-min))
  (cond
   ((looking-at "^BABYL OPTIONS:")
    (let ((p (point)))
      (search-forward "\C-_" nil t)
      (delete-region p (point))))
   (t (error "Non a BABYL format buffer!"))))

(defun convert-babyl-message-to-mail ()
  "Convert the babyl message immediately following (point) to Mail format."
  (let ((start (point))
	(end (progn (search-forward "\C-_") (point))))
    ; restrict view to the current message
    (narrow-to-region start end)

    ; nuke the starting delimiter and the attribute and summary lines
    (goto-char (point-min))
    (kill-line 3)

    ; zap the *** EOOH *** line, the blank line preceding it, and the header
    ; after it.
    (goto-char (point-min))
    (search-forward "*** EOOH ***")
    (let (start-hdr)
      (save-excursion
	(beginning-of-line 0)
	(setq start-hdr (point)))
      (search-forward "\C-j\C-j")
      (beginning-of-line 0)
      (delete-region start-hdr (point)))

    ; convert multi-line headers into a single line so they
    ; stay together when we move them around.
    (goto-char (point-min))
    (narrow-to-region (point-min) (save-excursion
				    (search-forward "\C-j\C-j")
				    (beginning-of-line 0)
				    (point)))
    (replace-string "\C-j\C-i" "<NL>")

    (goto-char (point-min))
    ; Copy From: and delete :, placing result at start
    (goto-char (point-min))
    (cond
     ((re-search-forward "^\\(From:.*\\)$" (point-max) t)
      (let ((from-hdr   (buffer-substring (match-beginning 1) (match-end 1))))
	(goto-char (point-min))
	(insert from-hdr "\n"))
      (goto-char (point-min))
      (search-forward "From")
      (delete-char 1)
      (if (not (looking-at " ")) (insert " "))
      (beginning-of-line 2))
     (t				; should look for Apparently-From: ...
      (goto-char (point-min))
      (insert "From (Unknown Person)\n")))

    ; restore the multi-line headers
    (goto-char (point-min))
    (replace-string "<NL>" "\C-j\C-i"))

  ; remove restriction, and zap trailing \C-_
  (widen)
  (goto-char (point-min))
  (search-forward "\C-_")
  (delete-backward-char 1))


;;; watch for the signature!!!


;--
;Skip Montanaro (montanaro@sprite.crd.ge.com)
