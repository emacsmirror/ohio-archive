;From arpa-unix-emacs-request@PIZZA.BBN.COM Wed Jul 20 15:01:14 1988
;Received: from pizza by PIZZA.BBN.COM id aa16265; 19 Jul 88 22:15 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa16261; 19 Jul 88 22:13 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 19 Jul 88 23:15:28 GMT
;From: Alan Stebbens <HUB.UCSB.EDU!aks%nowhere%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: z-mode.el
;Message-Id: <8807200126.AA15162@EDDIE.MIT.EDU>
;Source-Info:  From (or Sender) name not authenticated.
;
;Speaking of auto-uncompressing ".Z" files when visiting them, here
;is my little Emacs hack to do this.  It is invoked automatically
;by including the function in the auto-mode-alist in my .emacs
;startup file.
;
;My original idea was to auto-compress it upon saving, but there
;are good reasons not to do this.  Generally, compressing a file is
;a way of archiving it for later (usually much later) retrieval.
;However, if you visit a file and make changes to it, more often
;than not, you are entering a modification cycle, and will make
;subsequent changes to the same file.  So, compressing a file
;automatically is usually counter-productive and wastes time, since
;it will likely be accessed again until the modification cycle is
;complete.  Once the changes are done, though, it is easy enough to
;compress the file, and you need not visit the file to do this:
;"M-! compress FILE". Thus, auto-compressing the visited file was
;not considered useful.
;
;Alan Stebbens <aks@hub.ucsb.edu>
;
;========================= z-mode.el =========================
(defun z-mode () "\
Temporary major mode triggered by the \".Z\" suffix on a file,
used to automatically uncompress the file when visiting.  After
running the buffer contents through \"uncompress\", the buffer
name is changed by truncating the \".Z\" (as well as the visited
file name).  Also, the buffer is marked as read-only.  Finally,
normal-mode is invoked to process the buffer for its normal mode."
  (if (and (not (null buffer-file-name))
	   (string-match "^\\(.*\\)\\.Z$" buffer-file-name))
      (let ((new (substring buffer-file-name
			    (match-beginning 1) (match-end 1))))
	(setq buffer-read-only nil)
	(message "Uncompressing text...")
	(shell-command-on-region (point-min) (point-max)
				 "uncompress" t)
	(message "Uncompressing text...done")
	(goto-char (point-min))
	(set-visited-file-name new)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
  (normal-mode))
;============================================================
;
;To use it, place the following (or equivalent) in your ~/.emacs
;startup.
;
;========================== .emacs ==========================
;
;; Define z-mode to auto-load when visiting a ".Z" file

(autoload 'z-mode "z-mode" "\
Mode triggered by \".Z\" suffixed files, which then get automatically
uncompressed with appropriate buffer and visited file name changes.  The
buffer containing the uncompressed source is set to read-only."
	  t)

;; Establish '.Z' as a valid mode

(setq auto-mode-alist (nconc '(("\\.Z$" . z-mode)) auto-mode-alist))

