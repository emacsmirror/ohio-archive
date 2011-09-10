;From POP2-Server@k30b Thu Oct 20 07:12:11 1988
;Received: from pizza by PIZZA.BBN.COM id aa12747; 20 Oct 88 4:23 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa12743; 20 Oct 88 4:20 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 17 Oct 88 19:21:07 GMT
;From: Dale Worley <compass.UUCP!worley%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: shell-command-on-buffer
;Message-Id: <8810171921.AA04437@galaxy.compass.com>
;Source-Info:  From (or Sender) name not authenticated.
;
;This is to go along with shell-command and shell-command-on-region:
;
(defun shell-command-on-buffer (command &optional flag)
  "Execute string COMMAND in inferior shell with buffer as input;
display output (if any) in temp buffer;
Prefix arg means replace the buffer with it."
  (interactive "sShell command (on buffer): \nP")
  (shell-command-on-region (point-min) (point-max) command flag t))
