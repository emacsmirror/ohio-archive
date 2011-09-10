;From arpa-unix-emacs-request@CHIPS.BBN.COM Wed Mar 29 16:22:47 1989
;Received: from chips by CHIPS.BBN.COM id aa24982; 29 Mar 89 15:49 EST
;Received: from BBN.COM by CHIPS.BBN.COM id aa24978; 29 Mar 89 15:47 EST
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 28 Mar 89 23:30:03 GMT
;From: Bob Weiner <ukma!uflorida!novavax!weiner@husc6.harvard.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Note on info-mouse.el missing 'last-line-p'
;Message-Id: <1135@novavax.UUCP>
;Organization: Nova University, Fort Lauderdale, FL
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;Hucka@caen.engin.umich.edu pointed out that he had no 'last-line-p'
;function which is called in the info-mouse.el package that I posted
;awhile back.  I checked and the function is in cal.el which is now
;distributed with GNU Emacs but may not have been when you got your
;version.  It is a very simple function, so the code follows.
;
(defun last-line-p ()
  "Return T if point is on the last line of the buffer.
If the buffer is narrowed, this means the last line of the narrowed
part."
  (save-excursion
     (end-of-line)
     (eobp)))
;-- 
;Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
;(407) 738-2087

