;From POP2-Server@k30b Thu Oct 20 07:12:18 1988
;Received: from pizza by PIZZA.BBN.COM id aa13004; 20 Oct 88 6:22 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa13000; 20 Oct 88 6:20 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 17 Oct 88 19:23:14 GMT
;From: Dale Worley <compass.UUCP!worley%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: "Improvements" to find-alternate-file
;Message-Id: <8810171923.AA04446@galaxy.compass.com>
;Source-Info:  From (or Sender) name not authenticated.
;
;I find that most of the times that I want to use find-alternate-file
;are when I have attempted to find a file in the wrong directory.
;find-alternate-file is annoying, because I then have to type the
;filename over again.  This change to find-alternate-file fixes that --
;if you gave the right filename, type C-u C-x C-v and just give the
;correct directory.  The current filename of the buffer will be
;appended.
;
;Dale
;


(defun find-alternate-file (filename &optional prefix)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want.
If second argument is non-nil (or prefix argument is given interactively),
the current filename is appended to FILENAME (with a / if necessary) before
it is selected.  If you visited a file but gave the wrong directory, use this
command with a prefix argument to visit the right file, but you only have to
give the correct directory"
  (interactive "FFind alternate file: \nP")
  (and (buffer-modified-p)
       (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (if prefix
      (progn
	(if (not (string-equal (substring filename -1) "/"))
	    (setq filename (concat filename "/")))
	(setq filename (concat filename
			       (file-name-nondirectory buffer-file-name)))))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(oname (buffer-name)))
    (rename-buffer " **lose**")
    (setq buffer-file-name nil)
    (unwind-protect
	(progn
	  (unlock-buffer)
	  (find-file filename))
      (cond ((eq obuf (current-buffer))
	     (setq buffer-file-name ofile)
	     (lock-buffer)
	     (rename-buffer oname))))
    (kill-buffer obuf)))
