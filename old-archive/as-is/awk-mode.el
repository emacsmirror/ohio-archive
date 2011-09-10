;Date: Tue 25 Oct 88 15:10:36-MDT
;From: "Nelson H.F. Beebe" <Beebe@science.utah.edu>
;Subject: preliminary awk mode for GNU Emacs
;To: unix-emacs%bbn.com%WASATCH@cs.utah.edu
;Cc: Beebe@science.utah.edu, dsill@ee00
;
;The posting today
;
;>> Date: Tue, 25 Oct 88 14:44:30 edt
;>> From: Dave Sill <dsill@nswc-oas.ARPA>
;>> To: unix-emacs@bbn.com
;>> Subject: GNU Emacs Lisp Code Directory
;
;reminded me that I hadn't got around to contributing my awk
;mode for GNU Emacs.  awk is very similar to C, differing (as
;far as editing goes) mostly in the comment syntax.  Thus,
;the awk-mode.el file is quite small.  I augment it with the
;following in my .emacs file, to get automatic selection of
;the mode on visiting a file with extension .awk:
;
;(setq auto-mode-alist
;      (cons (cons "\\.awk$" 'awk-mode) auto-mode-alist))
;(autoload 'awk-mode  "awk-mode"
;  "Enter awk mode." t nil)
;
;Here is awk-mode.el:
;
;/u/sy/beebe/emacs/awk-mode.el, Mon Oct 17 14:29:27 1988
;Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
(defun awk-mode ()
  "Set up for editing awk code (a minor variation on C mode)."
  (interactive)
  (c-mode)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "# *")
  (setq mode-name "awk")
)


