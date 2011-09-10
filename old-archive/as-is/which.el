;From arpa-unix-emacs-request@PIZZA.BBN.COM Wed Aug 31 22:24:58 1988
;Received: from pizza by pizza.BBN.COM id aa19978; 31 Aug 88 20:50 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa19974; 31 Aug 88 20:49 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 31 Aug 88 22:51:49 GMT
;From: Bill Janssen <bloom-beacon!tut.cis.ohio-state.edu!cs.utexas.edu!milano!titan!janssen%titan.sw.mcc.com%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: Is a file loadable?
;Reply-To: Bill Janssen <janssen@titan.sw.mcc.com>
;Message-Id: <1140@titan.SW.MCC.COM>
;References: <5852@columbia.edu>, <36722@yale-celray.yale.UUCP>
;Organization: MCC Software Technology
;Source-Info:  From (or Sender) name not authenticated.
;
(defun remove (item list)
  (let ((tmp list)
	(new-list nil)
	)
    (while (consp tmp)
      (if (not (eq item (car tmp)))
	  (setq new-list (append new-list (list (car tmp)))))
      (setq tmp (cdr tmp))
      )
    new-list
    ))

(defun make-which-list (file)
  "Find out what FILE would be loaded with the current load-path."
  (remove nil (mapcar '(lambda (dir)
			 (let ((filename (concat dir "/" file))
			       )
			   (if (file-exists-p (expand-file-name filename))
			       (expand-file-name filename)
			     (if (file-exists-p (expand-file-name (concat
								   filename
								   ".elc")))
				 (expand-file-name (concat filename ".elc"))
			       (if (file-exists-p (expand-file-name (concat
								     filename
								     ".el")))
				   (expand-file-name (concat filename ".el"))
				 nil
				 )))
			   ))
		      load-path))
  )

(defun which (file)
  (interactive "sFile:  ")
  (message (or (car (make-which-list file))
	       "No file found."))
  )

(defun show-loads (file)
  
  "Show all possible loadings for file FILE in a buffer"

  (interactive "sFile:  ")
  (let ((whiches (make-which-list file))
	(b (get-buffer-create (concat "*Load Files for " file "*")))
	)
      (if whiches
	  (progn
	    (set-buffer b)
	    (erase-buffer)
	    (mapcar '(lambda (string)
		       (goto-char (point-max))
		       (insert string "\n")
		       )
		    whiches)
	    (goto-char (point-min))
	    (display-buffer b)
	    )
	(message "No files found."))
      ))


