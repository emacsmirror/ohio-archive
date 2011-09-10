;
; GNUS things: Mail and news in one incantation.
; Definitely not for the weak of heart.
;
(setq gnus-nntp-server "")
(setq gnus-nntp-service nil)
(load "gnus") ; I don't see any point in autoloading this.
(setq gnus-ignored-headers
      (concat gnus-ignored-headers
	      "\\|^Errors-To:\\|^Precedence:\\|^Received:\\|^UNIX-From:"))
(setq nnspool-spool-directory "~/Memos/") ; note trailing `/'
(defvar karl-active-file-maker "~/bin/make-active"
  "*Personalized active file generator")
(defvar karl-active-file "~/Memos/active"
  "*Personalized active file")
(defun nnspool-personal-active-file ()
  "Returns a filename string for a personal active file."
  (call-process karl-active-file-maker nil nil nil
		; remaining stuff is args to the active file maker.
		nnspool-spool-directory
		"personal/general" "personal/bob" "personal/romig"
		"personal/george" "personal/paul" "personal/zwicky"
		"personal/northrup" "personal/uucp" "personal/karl"
		"personal/amanda"
		"list/general" "list/firearms/politics" "list/firearms"
		"mailer-daemon")
  karl-active-file
  )
