;From: allegra!tk@RESEARCH.ATT.COM
;Newsgroups: gnu.emacs
;Subject: Re: Multi-Buffer search?
;Message-ID: <8908260520.AA12914@life.ai.mit.edu>
;Date: 26 Aug 89 05:05:44 GMT
;References: <68700011@sts>
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 26
;
;
; how bout:
 (defun search-buffers (s &optional fn)
   (interactive "sString: ")
   (if (null fn) (setq fn (symbol-function 'recursive-edit)))
   (save-excursion
     (catch 'search-buffers-abort
       (mapcar (function
		(lambda (x)
		  (message "searching %s..." (buffer-name x))
		  (set-buffer x)
		  (save-excursion
		    (save-restriction
		      (widen)
		      (goto-char (point-min))
		      (if (search-forward s nil t)
			  (progn
			    (switch-to-buffer x)
			    (funcall fn)
			    (while (search-forward s nil t)
			      (funcall fn))))))))
	       (buffer-list)))))
 (defun screwit ()
   "quick way out of search-buffers"
   (interactive)
   (throw 'search-buffers-abort nil))
