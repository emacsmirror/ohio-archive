;From POP2-Server@k30b Thu Oct 20 07:12:13 1988
;Received: from pizza by PIZZA.BBN.COM id aa12993; 20 Oct 88 6:22 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa12988; 20 Oct 88 6:20 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 17 Oct 88 19:21:47 GMT
;From: Dale Worley <compass.UUCP!worley%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Function for saving the results of a help request
;Message-Id: <8810171921.AA04440@galaxy.compass.com>
;Source-Info:  From (or Sender) name not authenticated.
;
;Sometimes you want to save the *Help* buffer for later examination,
;e.g., when you do an apropos.  save-help will rename the *Help* buffer
;*Help<1>*, *Help<2>*, etc., so the information won't get clobbered by
;further help requests.
;
;Dale
;
(defun save-help ()
  (interactive)
  (save-excursion
    (let ((i 1) 
	  (buffer (get-buffer "*Help*"))
	  name)
      (if (not buffer)
	  (ding)
	(while
	    (progn
	      (setq name (concat "*Help<" (int-to-string i) ">*"))
	      (get-buffer name))
	  (setq i (1+ i)))
	(set-buffer buffer)
	(rename-buffer name)
	(message (concat "Help buffer renamed " name))))))
