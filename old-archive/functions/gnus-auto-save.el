; From: montnaro@sprite.crd.ge.com (Skip Montanaro)
; Newsgroups: gnu.emacs.gnus
; Subject: Saving ~/.newsrc every now and again
; Date: 17 Oct 89 14:25:39 GMT
; Reply-To: <montanaro@crdgw1.ge.com> (Skip Montanaro)
; 
; I was a annoyed that GNUS didn't save my .newsrc periodically (automatically
; - I know I can do it manually), so I added the following code to my .emacs
; file. All changes are made using hooks.
; 
(setq gnus-newsrc-save-frequency 20)
(setq gnus-read-articles 0)
(setq gnus-Select-group-hook
      '(lambda ()
	 (if (> gnus-read-articles gnus-newsrc-save-frequency)
	     (progn
	       (gnus-save-newsrc-file)
	       (setq gnus-read-articles 0)))))

(setq gnus-Article-prepare-hook
      '(lambda ()
	 (setq gnus-read-articles (1+ gnus-read-articles))))

; Now, every time a new group is selected, if more than 20 articles have been
; read since the last time .newsrc was saved, it's saved, and the counter is
; reset. 
