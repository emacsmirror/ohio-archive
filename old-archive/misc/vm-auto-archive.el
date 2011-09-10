;;
;; LCD Archive Entry:
;; vm-auto-archive|Neal Young|ney@research.att.com|
;; Two ways to automatically archive outgoing mail into appropriate VM folders.  Supersedes vm-auto-fcc.el|
;; 20-Oct-1994|version 5.72a|~/misc/vm-auto-archive.el|
;;
;; PURPOSE: Automatically archive outgoing mail into appropriate VM folders.
;;
;;   This works with vm 5.72.
;;
;; USAGE: 
;;   This package provides two methods for automatically archiving outgoing
;;   mail messages using the VM mailer.  [Also it provides a utility function
;;   "return-match" which is useful in vm-auto-folder-alist -- e.g.,
;;   after matching the regexp "a\(.*\)b" to "axb", (return-match "\\1 \\1") 
;;   will return "x x".]
;;
;;   The first method is implemented by an interactive function vm-auto-fcc.
;;   This function, invoked by "" when composing a message, prompts for a
;;   folder name and inserts an "FCC: <folder-name>" field into the msg.  When
;;   prompting for the folder name, the function provides a default determined
;;   by pattern-matching the contents of the current message, similarly to VM's
;;   use of vm-auto-folder-alist, as described below.  When the message is
;;   sent, the "FCC:" field causes a copy to be appended to the chosen folder.
;;
;;   The second method is implemented by a function vm-auto-archive.  This
;;   function, invoked by "" when composing a message, prompts for a
;;   folder name and inserts two fields into the message.  When prompting for
;;   the folder name, the function provides a default determined as in the
;;   first method.  The first field is a "BCC: " field with your
;;   user-login-name.  The second field is a "X-VM-folder: " field with the
;;   chosen folder name.  When the message is sent, the "BCC:" field causes a
;;   copy to be mailed to you.  This file modifies VM (using "advice") so that
;;   when you save a message that appears to be from you and has an
;;   "X-VM-folder:" field, that folder will be offered as the default folder
;;   for saving.
;;
;;   The second method has the advantage that all of the fields added to the
;;   message by send-mail are present in the archived message too.  It has
;;   the disadvantages that the mail is not immediately archived and a
;;   malicious mail sender could screw it up.
;;
;;   In each case, the default folder is determined by pattern-matching the
;;   contents of the message being composed to the variable
;;   vm-auto-archive-alist.  This variable has the same format and semantics
;;   as vm-auto-folder-alist, except that it is used to classify a message
;;   that is being composed for sending, rather than one in a VM folder.
;;
;;   (VM is an alternative to rmail.  It has some nice features like using the
;;    standard mailbox format, automatically filing mail messages by pattern
;;    matching, easily customized summary buffer display, threads...
;;    If you don't have VM, try /ftp.uu.net:networking/mail/vm.
;;
;; INSTALLATION:
;;    Probably best not to compile this file, but if you do, recompile it
;;    whenever you install a new version of vm.
;;
;;    PUT this file in a directory on your e-lisp search path
;;    under the name "vm-auto-archive.el".
;;
;;    ADD these lines to your .vm (or .emacs) file:

(defvar vm-auto-archive-alist nil "\
Like vm-auto-folder-alist, but used by vm-auto-fcc and vm-auto-archive
to categorize a message being composed for sending.")

(defvar vm-auto-archive-sender-regexp nil "\
Regexp.  If the sender of a mail message matches this, and the message
 has an X-VM-folder field, then vm-auto-select-folder will return the folder
 named in the field.  Defaults to vm-reply-ignored-addresses.")

(autoload (quote vm-auto-fcc) "vm-auto-archive" "\
Add a new FCC field, with file name guessed by vm-auto-archive-alist.
Return file name." t nil)

(autoload (quote vm-auto-archive) "vm-auto-archive" "\
Add BCC and X-VM-folder fields to message being composed,
 guessing folder name based on vm-auto-archive-alist.  Return file name." t nil)

(autoload (quote vm-auto-archive-enable) "vm-auto-archive" "\
Enable vm-auto-archive." t nil)

(autoload (quote vm-auto-archive-disable) "vm-auto-archive" "\
Disable vm-auto-archive." t nil)

(autoload (quote return-match) "vm-auto-archive" "\
Like replace-match except return the string that would be
substituted for the match, instead of replacing the match with it." nil nil)

(add-hook 'mail-setup-hook 
	     '(lambda () 
		(local-set-key "" 'vm-auto-fcc)
		(local-set-key "" 'vm-auto-archive)
		))

(vm-auto-archive-enable)

;;    (continued...)
;;
;;    SET variable vm-auto-archive-alist (in your .vm (or .emacs) file)
;; 	as you would set vm-auto-folder-alist, 
;; 	except set it to recognize fields of outgoing messages.
;;      You probably want to reverse the sense of "from" and "to" headers,
;;	but you could just (setq vm-auto-archive-alist vm-auto-folder-alist).
;;      
;; BUGS:
;;    The method is a hack.  It advises vm-get-header-contents to work in
;;    arbitrary buffers, so that vm-auto-select-folder can too.  It also
;;    advises vm-auto-select-folder to catch the "X-VM-folder:" field if
;;    appropriate.  If vm-auto-select-folder or vm-get-header-contents is
;;    changed, it might cease to work.
;;
;;    For these reasons, I added commands to disable/enable vm-auto-archive
;;    at will.
;;
;; Copyright (C) 1993,1994  Neal Young
;;
;;    "X-VM-folder:" field functionality based on code
;;    provided by gec@Mti.Sgi.Com (Gardner Cohen).
;; 
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;; 
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;; 

(require 'advice)

(defvar vm-auto-archive-enabled nil "vm-auto-archive private variable")

;;;###autoload
(defvar vm-auto-archive-alist nil 
  "Like vm-auto-folder-alist, but used by vm-auto-fcc and vm-auto-archive
to categorize a message being composed for sending.")

(defvar vm-auto-archive-alist nil "\
Like vm-auto-folder-alist, but used by vm-auto-fcc and vm-auto-archive
to categorize a message being composed for sending.")

(defvar vm-auto-archive-alist nil "\
Like vm-auto-folder-alist, but used by vm-auto-fcc and vm-auto-archive
to categorize a message being composed for sending.")

;;;###autoload
(defvar vm-auto-archive-sender-regexp nil
  "Regexp.  If the sender of a mail message matches this, and the message
 has an X-VM-folder field, then vm-auto-select-folder will return the folder
 named in the field.  Defaults to vm-reply-ignored-addresses.")

(defvar vm-auto-archive-sender-regexp nil "\
Regexp.  If the sender of a mail message matches this, and the message
 has an X-VM-folder field, then vm-auto-select-folder will return the folder
 named in the field.  Defaults to vm-reply-ignored-addresses.")

(defvar vm-auto-archive-sender-regexp nil "\
Regexp.  If the sender of a mail message matches this, and the message
 has an X-VM-folder field, then vm-auto-select-folder will return the folder
 named in the field.  Defaults to vm-reply-ignored-addresses.")

;;;###autoload
(defun vm-auto-fcc (&optional dont-ask-if-default-available)
  "Add a new FCC field, with file name guessed by vm-auto-archive-alist.
Return file name."
  (interactive)
  (if (not vm-auto-archive-enabled)
      (message "execute vm-auto-archive-enable first!")
    (let (file-name)
      (save-excursion
	(expand-abbrev)
	(or (mail-position-on-field "fcc" t) ;Put new field after existing FCC.
	    (mail-position-on-field "to"))
	(let ((default (vm-auto-select-folder-for-buffer 
			vm-auto-archive-alist)))
	  (if (or (not (stringp default)) (equal default ""))
	      (setq default vm-last-save-folder))
	  (setq file-name
		(if (or (not (stringp default)) (equal default ""))
		    (read-file-name "FCC: " vm-folder-directory)
		  (if (not (file-name-absolute-p default))
		      (setq default (concat vm-folder-directory default)))
		  (if dont-ask-if-default-available
		      default
		    (read-file-name (concat "FCC: (default " default ") ")
				    vm-folder-directory
				    default)))))
	(insert "\nFCC: " file-name))
      file-name)))

;;;###autoload
(defun vm-auto-archive (&optional dont-ask-if-default-available)
  "Add BCC and X-VM-folder fields to message being composed,
 guessing folder name based on vm-auto-archive-alist.  Return file name."
  (interactive)
  (if (not vm-auto-archive-enabled)
      (message "execute vm-auto-archive-enable first!")
    (save-excursion
      (expand-abbrev)
      (or (mail-position-on-field "bcc" t) ;Put new field after existing BCC.
	  (mail-position-on-field "to"))
      (insert "\nbcc: " (user-login-name))
      (let ((default (vm-auto-select-folder-for-buffer vm-auto-archive-alist)))
	(if (or (not (stringp default)) (equal default ""))
	    (setq default vm-last-save-folder))
	(let ((file-name
	       (if (or (not (stringp default)) (equal default ""))
		   (read-file-name "folder: " vm-folder-directory)
		 (if (not (file-name-absolute-p default))
		     (setq default (concat vm-folder-directory default)))
		 (if dont-ask-if-default-available
		     default
		   (read-file-name (concat "folder: (default " default ") ")
				   vm-folder-directory
				   default)))))
	  (insert "\nX-VM-folder: " file-name)
	  file-name)))))

(defun vm-auto-select-folder-for-buffer (auto-folder-alist)
  (vm-auto-select-folder '(CURRENT-BUFFER) auto-folder-alist))

;;
(defadvice vm-get-header-contents;; args: message header-name-regexp
  (around vm-auto-archive-get-header-contents disable)
  "Advised so as to work in current buffer (instead of vm msg)
   when (eq MESSAGE 'CURRENT-BUFFER).  For vm-5.65."
  (if (not (eq message 'CURRENT-BUFFER))
      ad-do-it
    (vm-get-header-contents-current-buffer header-name-regexp)))

;; logic to use X-VM-folder field to set the default when saving message.
(defadvice vm-auto-select-folder
  (around vm-auto-archive-auto-select (MP AUTO-FOLDER-ALIST) disable)
  "If you sent the message, use the `X-VM-folder' field, if any."
  (let ((header-folder (vm-get-header-contents (car MP) "x-vm-folder"))
	(sender-regexp (or vm-auto-archive-sender-regexp
			   (if vm-reply-ignored-addresses
			       (mapconcat 'identity
					  vm-reply-ignored-addresses
					  "\\|"))
			   (user-login-name)))
	(sender (vm-get-header-contents (car MP) "from:")))
    (or
     (and sender 
	  (string-match sender-regexp sender)
	  (setq ad-return-value header-folder))
     ad-do-it)))

(defun vm-get-header-contents-current-buffer (header-name-regexp)
  "like vm-get-header-contents but within a buffer holding text of message"
  (condition-case nil
      (let ((contents nil) 
	    (regexp (concat "^\\(" header-name-regexp "\\)")))
	(save-excursion
	  (save-restriction
	    (let ((end
		   (progn (goto-char (point-min))
			  (re-search-forward "\n\n\\|\\'")
			  (point))))
	      (goto-char (point-min))
	      (let ((case-fold-search t))
		(while (and (re-search-forward regexp end t)
			    (save-excursion (goto-char (match-beginning 0))
					    (vm-match-header)))
		  (if contents
		      (setq contents
			    (concat contents ", " (vm-matched-header-contents)))
		    (setq contents (vm-matched-header-contents)))))))
	  (setq ad-return-value contents)))
    (error
     (error 
      "vm-auto-archive incompatible with vm version or needs recompilation"
      ))))

;;; use the next two functions to enable/disable vm-auto-archive once loaded

;;;###autoload
(defun vm-auto-archive-enable ()
  "Enable vm-auto-archive."
  (interactive)
  (if vm-auto-archive-enabled
      nil
    (setq ad-activate-on-definition t)
    (ad-start-advice)
    (ad-enable-advice 'vm-get-header-contents 'around
		     'vm-auto-archive-get-header-contents)
    (ad-activate 'vm-get-header-contents)
    (ad-enable-advice 'vm-auto-select-folder 'around 
		      'vm-auto-archive-auto-select)
    (ad-activate 'vm-auto-select-folder)
    (setq vm-auto-archive-enabled t)
    ))

;;;###autoload
(defun vm-auto-archive-disable ()
  "Disable vm-auto-archive."
  (interactive)
  (if (not vm-auto-archive-enabled)
      nil
    (ad-disable-advice 'vm-get-header-contents 'around
		       'vm-auto-archive-get-header-contents)
    (ad-activate 'vm-get-header-contents)
    (ad-disable-advice 'vm-auto-select-folder 'around 
		       'vm-auto-archive-auto-select)
    (ad-activate 'vm-auto-select-folder)
    (setq vm-auto-archive-enabled nil)
    ))

;; the following function is useful in vm-auto-folder-alist,
;; e.g. after matching the regular expression "\(.*\)aa" to "bbaa"
;; (return-match "\\1 x \\1") yields "bb x bb".  So, for instance, 
;; the following would return the username of the first person in the
;; from, to, sender, or cc fields, whichever comes first.
;; (setq 
;;      vm-auto-folder-alist
;;      '(
;;	("From\\|To\\|Sender\\|CC"
;; 	 ("^\\(.*<\\)?\\([^@%> 	
;; ]+\\)" . (return-match "\\2"))
;; 	 )
;; 	)

;;;###autoload
(defun return-match (new-text &optional fixed-case &optional literal)
  "Like replace-match except return the string that would be
substituted for the match, instead of replacing the match with it."
  (let* ((maxi (- (length new-text) 1))
	 (submatches (make-vector 10 ""))
	 (substrings nil)
	 (this (substring new-text 0 1))
	 (next (if (<= 1 maxi)
		   (substring new-text 1 2)
		 (char-to-string 0)))
	 (i 0))

    (while (< i 10)
      (if (match-beginning i)
	  (aset submatches i
		(buffer-substring (match-beginning i) (match-end i))))
      (setq i (+ 1 i)))

    (setq i 0)
    (while (<= i maxi)
      (setq substrings 
	    (cons 
	     (if (string= this "\\")
		 (progn 
		   (setq i (+ 1 i))
		   (setq this next)
		   (setq next (if (< i maxi)
				  (substring new-text (+ 1 i) (+ 2 i))
				(char-to-string 0)))
		   (cond
		    ((string= this "&") (aref submatches 0))
		    ((and (string< "0" this)
			  (or (string= this "9")
			      (string< this "9")))
		     (aref submatches (string-to-int this)))
		    (t this)))
	       this)
	     substrings))
      (setq i (+ 1 i))
      (setq this next)
      (setq next (if (< i maxi)
		     (substring new-text (+ 1 i) (+ 2 i))
		   (char-to-string 0))))
    (apply 'concat (nreverse substrings))))

(provide 'vm-auto-archive)
