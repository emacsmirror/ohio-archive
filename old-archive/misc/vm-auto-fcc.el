; Here's e-lisp code to file outgoing mail into folders that are chosen just as
; vm-save chooses a folder by consulting vm-auto-folder-alist.
; 
; After loading this code, when you are composing a message that you wish to
; save in a folder, type ^C^F^F.  You will be prompted for a folder name, with
; the default being determined by consulting vm-auto-out-folder-alist, just as
; the default folder for vm-save is determined from vm-auto-folder-alist.
; A 'fcc' field will be inserted into your message, so that when you send the
; message, it will be archived in that folder.
; 
; Neal Young <ney@cs.princeton.edu>
; 
;;
;; vm-auto-fcc.el, by Neal Young <ney@cs.princeton.edu> 24-Nov-93
;;
;; PURPOSE: Automatically file outgoing mail using mailer vm.
;;
;;   (VM is an alternative to rmail.  It has some nice features like using the
;;    standard mailbox format, automatically filing mail messages by pattern
;;    matching, easily customized summary buffer display, a threads package...
;;    If you don't have VM, try `archie vm` (avoid www stuff) or (as of 11/93)
;;    /cs.columbia.edu:/pub/archives/mirror2/uunet/networking/mail/vm/.)
;;
;; INSTALLATION:
;;    LOAD THIS FILE (either in .emacs, through autoload or whatever)
;;      _after_ loading the original vm* files.
;;      If you have installed your own vm, just append the .elc file to vm.elc 
;;
;;    SET VARIABLE vm-auto-out-folder-alist (in your .emacs)
;; 	as you would set vm-auto-folder-alist, 
;; 	except set it to recognize fields of outgoing messages.
;;      You probably want to reverse the sense of "from" and "to" headers.
;; 	If you want you can just change each "from" to "from\\|to"
;; 	in your vm-auto-folder-alist and then
;;	(setq vm-auto-out-folder-alist vm-auto-folder-alist).
;;    
;;      This file modifies VM to allow patterns in the header description
;;      in vm-auto-folder-alist and (now) vm-auto-out-folder-alist.
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
;;    Redefines function vm-get-header-contents
;;    to work in arbitrary buffers, so that vm-auto-select-folder
;;    will too.  If vm-get-header-contents is changed,
;;    might cease to work.  Works in my vm version 5.35.L (beta).
;;    (The vm-auto-select-folder function as it is defined there is appended.)
;;
;; LCD Archive Entry:
;; vm-auto-fcc|Neal Young|ney@cs.princeton.edu|
;; Insert fcc field according to vm-auto-folder-alist to file outgoing mail|
;; 24-Nov-1993|version 1 (beta)|~/misc/vm-auto-fcc.el.Z|
;;
;;     Copyright (C) 1993  Neal Young
;; 
;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2 of the License, or
;;     (at your option) any later version.
;; 
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;; 

; (require 'vm)

(defvar vm-auto-out-folder-alist nil 
  "Set like vm-auto-folder-alist, but to recognize fields of outgoing mail.")

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
		  (read-file-name "FCC: ")
		(if (not (file-name-absolute-p default))
		    (setq default (concat vm-folder-directory default)))
		(read-file-name (concat "FCC: (default " default ") ")
				nil
				default))))))

;;
;; patch vm-header-contents to work for ordinary (ie, not vm format) buffer
;; containing mail message.  This allows vm-auto-select-folder
;; to work in ordinary buffers too.
;; 
;; relies on specific version of vm-auto-select-folder
;; (as in vm 5.35.L (beta)).  (appended to this file).

(defun vm-auto-select-folder-for-buffer (auto-folder-alist)
  (vm-auto-select-folder '(CURRENT-BUFFER) auto-folder-alist))

;; overwrites definition in vm-vars.el
(defconst vm-header-regexp-format "^\\(%s\\):[ \t]*\\(.*\\(\n[ \t].*\\)*\\)")

;; overwrites definition in vm-summary.el
(defun vm-get-header-contents (message header-name)
  (let (contents regexp)
    (setq regexp (format vm-header-regexp-format header-name))
    (save-excursion
      (let (start end)
	(if (eq message 'CURRENT-BUFFER)
	     (setq start (point-min)
		   end (progn
			 (goto-char start)
			 (re-search-forward "\n\n\\|\\'")
			 (point)))
	  (setq start (vm-start-of message))
	  (set-buffer (marker-buffer start))
	  (setq end (vm-text-of message)))
	(save-restriction
	  (widen)
	  (goto-char start)
	  (while (re-search-forward regexp end t)
	    (if contents
		(setq contents
		      (concat
		       contents ", "
		       (buffer-substring (match-beginning 2) (match-end 2))))
	      (setq contents
		    (buffer-substring (match-beginning 2) (match-end 2)))))
	  contents)))))

; (provide 'vm-auto-fcc)

;; vm-auto-select-folder as it is defined in vm version 5.35.L (beta):
;
; (defun vm-auto-select-folder (mp auto-folder-alist)
;   (condition-case ()
;       (catch 'match
; 	(let (header alist tuple-list)
; 	  (setq alist auto-folder-alist)
; 	  (while alist
; 	    (setq header (vm-get-header-contents (car mp) (car (car alist))))
; 	    (if (null header)
; 		()
; 	      (setq tuple-list (cdr (car alist)))
; 	      (while tuple-list
; 		(if (let ((case-fold-search vm-auto-folder-case-fold-search))
; 		      (string-match (car (car tuple-list)) header))
; 		    ; Don't waste time eval'ing an atom.
; 		    (if (atom (cdr (car tuple-list)))
; 			(throw 'match (cdr (car tuple-list)))
; 		      (let* ((match-data (vm-match-data))
; 			     (buf (get-buffer-create " *VM scratch*"))
; 			     (result))
; 			; Set up a buffer that matches our cached
; 			; match data.
; 			(save-excursion
; 			  (set-buffer buf)
; 			  (widen)
; 			  (erase-buffer)
; 			  (insert header)
; 			  ; It appears that get-buffer-create clobbers the
; 			  ; match-data.
; 			  ;
; 			  ; The match data is off by one because we matched
; 			  ; a string and Emacs indexes strings from 0 and
; 			  ; buffers from 1.
; 			  ;
; 			  ; Also store-match-data only accepts MARKERS!!
; 			  ; AUGHGHGH!!
; 			  (store-match-data
; 			   (mapcar
; 			    (function (lambda (n) (and n (vm-marker n))))
; 			    (mapcar
; 			     (function (lambda (n) (and n (1+ n))))
; 			     match-data)))
; 			  (setq result (eval (cdr (car tuple-list))))
; 			  (throw 'match (if (listp result)
; 					    (vm-auto-select-folder mp result)
; 					  result ))))))
; 		(setq tuple-list (cdr tuple-list))))
; 	    (setq alist (cdr alist)))
; 	  nil ))
;     (error nil)))
; 
