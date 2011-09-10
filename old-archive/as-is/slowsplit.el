;From arpa-unix-emacs-request@CHIPS.BBN.COM Wed Apr  5 10:27:40 1989
;Received: from chips by CHIPS.BBN.COM id aa19769; 5 Apr 89 11:10 EDT
;Received: from BBN.COM by CHIPS.BBN.COM id aa19765; 5 Apr 89 11:09 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 5 Apr 89 15:12:34 GMT
;From: John Robinson <jr@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: slowsplit.el again
;Reply-To: John Robinson <jr@bbn.com>
;Message-Id: <38288@bbn.COM>
;Organization: BBN Systems and Technologies Corporation, Cambridge MA
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;With help and encouragement from assorted anonymous souls, the
;slowsplit package seen here earlier has improved.  If no other bugs or
;missing features show up, I might try to integrate this with the
;standard split-window and delete-other-windows.  No one has stepped
;forward to work on the split-vertically version, but that is just a
;job for someone in Emacs 101 (:-).
;
;Despite the advertisement at the top, this behavior is pretty nice in
;general.  Your eyes will thank you.
;
;Without further ado:
;--------
;; slowsplit.el
;; Response to a challenge to build a split-window with minimum redisplay.
;; In the interests of really doing this, it may move point (the case where
;; the new modeline would cover point).  Replacement for
;; split-window-vertically and delete-other-windows.
;; John Robinson, <jr@bbn.com>, 16 Mar 89
;; updated 20 Mar 89, to use window lines so narrowing/wrapping works

(provide 'slowsplit)

(global-set-key "\C-x2" 'split-window-quietly)
(global-set-key "\C-x1" 'delete-other-windows-quietly)

(defun split-window-quietly (&optional arg)
  "Split the window vertically with minimum redisplay.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (interactive "P")
  (let* (
	  (num-arg (and arg (prefix-numeric-value arg)))
	  (oldpt (point))		
	  (oldstart (window-start))	
	  (scroll (or num-arg (/ (window-height) 2)))
	  (scrollpoint (progn (move-to-window-line scroll)
			 (point)))
	  (barstart (progn 
		      (move-to-window-line (- scroll 1))
		      (point)))
	  )
    (split-window nil num-arg)	   
    (goto-char oldstart)
    (recenter 0)
    (other-window 1)
    (goto-char scrollpoint)
    (recenter 0)
    (if (< oldpt scrollpoint )
      (if (>= oldpt barstart)
	(progn
	  (other-window -1)
	  (move-to-window-line (- scroll 2))
	  )
	(progn
	  (other-window -1)
	  (goto-char oldpt)
	  ))
      (progn
	(goto-char oldpt)
	))
    ))


(defun delete-other-windows-quietly ()
  "Delete other windows with minimum redisplay"
  (interactive)
  (let* ((oldpt (point))
	  (oldtopchar (window-start))
	  (oldtop (car (cdr (window-edges))))
	  )
    (delete-other-windows (selected-window))
    (goto-char oldtopchar)
    (recenter oldtop)
    (goto-char oldpt)))
;--------
;/jr
;jr@bbn.com or bbn!jr
;C'mon big money!
