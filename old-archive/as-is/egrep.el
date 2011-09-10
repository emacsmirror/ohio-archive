;To: unix-emacs@bbn.com
;Date: 26 Apr 89 15:12:16 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: EGREP -- Addition to COMPILE.EL for GNU Emacs
;Reply-To: Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;Organization: Computer Science, Yale University, New Haven, CT 06520-2158
;Source-Info:  From (or Sender) name not authenticated.
;
;GNU Emacs 18.52.26 of Sun Apr 23 1989 on leo.ring.cs.yale.edu (Domain/OS)
;
;The following function, analogous to GREP, is a useful addition to COMPILE.EL:

;; Ashwin Ram, 4/11/89.
(defun egrep (command)
  "Run egrep, with user-specified args, and collect output in a buffer.
While egrep runs asynchronously, you can use the \\[next-error] command
to find the text that egrep hits refer to."
  (interactive "sRun egrep (with args): ")
  (compile1 (concat "egrep -n " command " /dev/null")
	    "No more grep hits" "egrep"))


;-- Ashwin.
;
;ARPA:    Ram-Ashwin@cs.yale.edu
;UUCP:    {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;BITNET:  Ram@yalecs

