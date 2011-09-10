; Path: hal.com!olivea!pagesat!netsys!ukma!psuvax1!atlantis.psu.edu!howland.reston.ans.net!paladin.american.edu!europa.asd.contel.com!uunet!Cadence.COM!daled
; From: daled@cadence.com (Dale DePriest)
; Newsgroups: gnu.emacs.sources
; Subject: mail aliases
; Date: 20 Jan 93 19:56:05 GMT
; Organization: Cadence Design Systems
; 
; The routine permits you to see the expansion of the aliases at any time
; during the composition of your mail.  I have bound it to C-c C-a with
; the meaning alias expansion.  The aliases themselves are already supported
; is sent.  I only provided a hook to permit you to see them ahead of time.
; 
; ---Cut here-------------

;; LCD Archive Entry:
;; my-mail-aliases|Dale DePriest|daled@cadence.com|
;; Expand aliases in outgoung mail headers.|
;; 93-01-20||~/functions/my-mail-aliases.el.Z|

(setq mail-mode-hook '(lambda ()
	(define-key mail-mode-map "\C-c\C-a" 'my-mail-aliases)))

(defun my-mail-aliases ()
"Expand local mail aliases so user can view them."
  (interactive)
(if mail-aliases
  (save-excursion
  (goto-char (point-min))
  (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
     (replace-match "\n")
     (backward-char 1)
  (setq delimline (point-marker))
  (expand-mail-aliases (point-min) delimline)
  (goto-char delimline)
  (insert mail-header-separator ""))))













--
    _      _     Dale DePriest  		San Jose, California
   /`) _  //     daled@Cadence.COM		voice: (408) 428-5249
 o/_/ (_(_X_(`	 
