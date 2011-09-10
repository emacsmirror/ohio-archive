;; LCD Archive Entry:
;; vm-auto-fcc|Neal Young|ney@orie.cornell.edu|
;; Insert fcc field according to vm-auto-folder-alist to file outgoing mail|
;; 06-Jun-1994|5.72|~/misc/vm-auto-fcc.el|
;;
;; vm-auto-fcc.el
;;
;; PURPOSE: Automatically file outgoing mail using VM.
;;
;;   This works with vm 5.72.
;;
;;   (VM is an alternative to rmail.  It has some nice features like using the
;;    standard mailbox format, automatically filing mail messages by pattern
;;    matching, easily customized summary buffer display, threads...
;;    If you don't have VM, try /ftp.uu.net:networking/mail/vm.)
;;
;; INSTALLATION:
;;    LOAD THIS FILE each session (via .emacs, .vm, autoload or whatever).
;;
;;    SET VARIABLE vm-auto-out-folder-alist
;; 	as you would set vm-auto-folder-alist, 
;; 	except set it to recognize fields of OUTGOING messages.
;;      You probably want to reverse the sense of "from" and "to" headers.
;;      
;;    Best not to compile this file (because I have no experience
;;    compiling a file that "advises" other functions, and because
;;    you'd have to recompile it with each new version of vm).
;;
;; USAGE:
;;    When composing a message in a vm mail buffer, 
;;    if you want the message to be filed when it is sent,
;;    type \C-c\C-f\C-f (vm-vmail-fcc) to insert FCC: field to folder.
;;    The default folder is determined by vm-auto-out-folder-alist, 
;;    just as it is determined by vm-auto-folder-alist in vm-save.
;;    The message will be filed to that folder when sent because of the fcc.
;;
;; BUGS:
;;    The method uses "advice" to hack VM.  It advises vm-get-header-contents
;;    to work in arbitrary buffers, so that vm-auto-select-folder
;;    can too.  If vm-auto-select-folder or vm-get-header-contents 
;;    is changed, it might cease to work.
;;
;;    The message is archived before being passed through sendmail,
;;    so the many headers that are inserted by sendmail into the
;;    outgoing message are not in the archived copy.
;;
;;
;; Copyright (C) 1993,1994  Neal Young
;; 
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;; 
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;; 

(setq ad-activate-on-definition t)
(require 'advice)
(ad-start-advice)

(defvar vm-auto-out-folder-alist nil 
  "Like vm-auto-folder-alist, but used by vm-mail-fcc 
to recognize fields of outgoing mail.  See vm-mail-fcc.")

(add-hook 'mail-setup-hook 
	   '(lambda () (local-set-key "" 'vm-mail-fcc))
	   t)

(defun vm-mail-fcc ()
  "Add a new FCC field, with file name guessed by vm-auto-out-folder-alist."
  (interactive)
  (save-excursion
    (expand-abbrev)
    (or (mail-position-on-field "fcc" t) ;Put new field after exiting FCC.
	(mail-position-on-field "to"))
    (let ((default (vm-auto-select-folder-for-buffer 
		    vm-auto-out-folder-alist)))
      (if (or (not (stringp default)) (equal default ""))
	  (setq default vm-last-save-folder))
      (insert "\nFCC: "
	      (if (or (not (stringp default)) (equal default ""))
		  (read-file-name "FCC: " vm-folder-directory)
		(if (not (file-name-absolute-p default))
		    (setq default (concat vm-folder-directory default)))
		(read-file-name (concat "FCC: (default " default ") ")
				vm-folder-directory
				default))))))

(defun vm-auto-select-folder-for-buffer (auto-folder-alist)
  (vm-auto-select-folder '(CURRENT-BUFFER) auto-folder-alist))

;; make sure library vm-summary is loaded
;; so advice will have effect
;; must be a better way to do this (?)
(if (or (not (fboundp 'vm-get-header-contents))
	(let ((fbinding (symbol-function 'vm-get-header-contents)))
	  (and (listp fbinding))
	      (not (null fbinding))
	      (eq (car fbinding) 'autoload)))
    (load-library "vm-summary"))

(defadvice vm-get-header-contents ;; args: message header-name-regexp
  (around vm-auto-fcc-get-header-contents activate)
  "Advised so as to work in current buffer (instead of vm msg)
   when (eq MESSAGE 'CURRENT-BUFFER).  For vm-5.65."
  (if (not (eq message 'CURRENT-BUFFER))
      ad-do-it
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
	"vm-auto-fcc incompatible with vm version or needs recompilation")))))

(provide 'vm-auto-fcc)
