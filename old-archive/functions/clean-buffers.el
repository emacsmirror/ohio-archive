; Newsgroups: gnu.emacs.sources
; Path: hal.com!halaus!halaus!petonic
; From: petonic@hal.com (Michael A. Petonic)
; Subject: Deleting buffers in a regular manner
; Reply-To: petonic@hal.com (Michael A. Petonic)
; Organization: Henry's Laughing Gas Co., Inc.
; Date: 12 Nov 92 01:25:40 GMT
; 
; Ever want to delete all those buffers whose names start with
; "sent \(mail\|reply\) to ..." and such?  Here's a snippet of my
; .emacs so that this may be done in a regular manner.  Just
; put the following in your .emacs and call M-x clean-buffers whenever
; your buffer list gets too big.  It works off of the clean-buffer-list
; variable which is a list of regular expressions to delete.  
; 
; If called with an argument, clean-buffers will delete those buffers
; that are found with clean-buffer-extended-list, as well as what's
; in clean-buffer-list.
; 
; -pet-
; 
; -----------
;;;;;;;;;;;;;;;;;
;;
;; LCD Archive Entry:
;; clean-buffers|Michael A. Petonic|petonic@hal.com|
;; Buffer list cleanup stuff.|
;; 92-11-12||~/functions/clean-buffers.el.Z|
;;
;; -pet-  petonic@hal.com
;; Date: 92/11/11
;; 
;; Buffer list cleanup stuff.
;;
;; Standard GNU copyleft applies but I'm too harried to include it here.
;;  - not that anyone would "steal" this code, anyway.
;;

(defvar clean-buffer-list '("sent \\(reply\\)\\|\\(mail\\) to" "*gif parts*")
  "List of regexs of buffers to clean up")
(defvar clean-buffer-extended-list '("*Manual-")
  "List of extended regexs of buffers to clean up when M-x clean buffers
is called with an argument")

(defun clean-buffers (arg)
  "Cleans temporary and old buffers as defined by the regexs in 
clean-buffer-list.  If called with an ARG, then cleans up buffers specified
by the regexs in clean-buffers-extended-list."
  (interactive "P")
  (mapcar '(lambda (buffer)
	     (let ((buffer (buffer-name buffer)))
	       (mapcar '(lambda (re)
			  (if (string-match re buffer)
			      (progn
				(message "Deleting %s" buffer)
				(kill-buffer buffer))))
		       (append clean-buffer-list
			       (if arg
				   clean-buffer-extended-list
				 nil)))))
	  (buffer-list)))
