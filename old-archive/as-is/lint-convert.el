;To: unix-emacs@bbn.com
;Date: 19 Apr 89 08:31:32 GMT
;From: Lars Pensj| <mcvax!kth!draken!chalmers!myab!lars@uunet.uu.net>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: Parsing lint error messages doesn't seem to work right
;Reply-To: Lars Pensj| <lars%myab.uucp@CHIPS.BBN.COM>
;References: <849@uceng.UC.EDU>
;Organization: Myab Gothenburg, Sweden
;Source-Info:  From (or Sender) name not authenticated.

; This is a simple procedure that converts the output of lint to a format
; that next-error can understand. Run it before you do the first next-error.
; Some cases are not taken cared of.
;
; This is probably Sys V dependent.
;

(defun lint-convert ()
  "Modify *compilation* buffer from lint format."
  (interactive)
  (save-excursion
    (switch-to-buffer "*compilation*" t)
    (goto-line 1)
    (while (re-search-forward "^=")
      (let (loc filename)
	(forward-line -1)
	(beginning-of-line)
	(setq loc (point)) (end-of-line)
	(setq filename (buffer-substring loc (point)))
	(message filename)
	(sit-for 1)
	(beginning-of-line 3)
	(while (equal (buffer-substring (point) (1+ (point))) "(")
	  (insert (concat "\"" filename "\", line "))
	  (delete-char 1)
	  (search-forward ")")
	  (delete-char -1)
	  (insert ":")
	  (beginning-of-line 2)
	  )
	)
      )
    )
  )
;-- 
;    Lars Pensj|
;    lars@myab.se

