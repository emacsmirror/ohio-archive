;;;; GNU-EMACS PCMAIL mail reader

;;  Written by Mark L. Lambert
;;
;;  Internet: markl@us.oracle.com 
;;  USMail:   Oracle Corporation
;; 	      500 Oracle Parkway, box 659410
;;	      Redwood Shores CA 94065
;;  voice:    (415) 506 2912
;;  FAX:      (415) 506 7226

;; Copyright (C) 1989, 1993 Mark L. Lambert

;; This file is not officially part of GNU Emacs, but is being
;; donated to the Free Software Foundation.  As such, it is
;; subject to the standard GNU-Emacs General Public License,
;; referred to below.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;; system-dependent things

;;; mail environment.  For new environments, simply add a system-type switch
;;; to the cond and put whatever properties you desire into the cond clause
;;; examples for VMS and UNIX follow.  Currently all UNIXes are treated the
;;; same.  This can change as required.  A fair amount of this code is
;;; VMS-specific.  If you need to save space and you don't use VMS,
;;; cut where indicated and throw the remainder of the file away.

(cond ((eq system-type 'vax-vms)

       ;; VMS system mail drop

       (put 'vms-default-mail-drop 'header-processing-function 
	    'pcmail-process-vms-message)
       (put 'vms-default-mail-drop 'msg-start-regexp
	    (concat "^\^L\nFrom:[ \t]+.+[0-9]+-[a-zA-Z]+-"
		    "19[0-9]+[ \t]*[0-9]+:[0-9]+"))
       (put 'vms-default-mail-drop 'insert-function 'pcmail-do-vms-movemail)

       ;; VMS file mail drop, used to perform an initial import.  Extract
       ;; your messages into a file and use this mail drop to import the 
       ;; file

       (put 'vms-file-mail-drop 'header-processing-function 
	    'pcmail-process-vms-message)
       (put 'vms-file-mail-drop 'msg-start-regexp 
	    (concat "^\^L\nFrom:[ \t]+.+[0-9]+-[a-zA-Z]+-"
		    "19[0-9]+[ \t]*[0-9]+:[0-9]+"))
       (put 'vms-file-mail-drop 'insert-function 'pcmail-rename-mail-drop)
       (put 'vms-file-mail-drop 'name-input-func
	    '(lambda () (pcmail-narrow-read-file-name "maildrop.log")))

       ;; other environment stuff

       (put 'pcmail-mail-environment 'printer "SYS$PRINT")
       (put 'pcmail-mail-environment 'print-function 'pcmail-vms-print-message)
       (put 'pcmail-mail-environment 'mail-directory
	    (concat (substring (getenv "HOME") 0 -1) ".pcmail]"))
       (put 'pcmail-mail-environment 'time-zone "PST")
       (put 'pcmail-mail-environment 'legal-folder-regexp 
	    "[0-9A-Za-z][-0-9A-Za-z_$+.]+")
       (put 'pcmail-mail-environment 'send-mail-function 'pcmail-vms-send-mail)
       (put 'pcmail-mail-environment 'create-mail-directory-fn
	    'pcmail-vms-create-mail-directory)
       (put 'pcmail-mail-environment 'folder-to-file-function 
	    'pcmail-vms-folder-name-to-file)
       (put 'pcmail-mail-environment 'default-mail-drop-list 
	    '(vms-default-mail-drop)))

      ;;; UNIX systems

      (t

       ;; NNTP mail drop
       
       (put 'nntp-mail-drop 'header-processing-function 
	    'pcmail-process-nntp-message)
       (put 'nntp-mail-drop 'msg-start-regexp 
	    "^\^L\n\\(Organization\\|Newsgroups\\|Path\\|From\\|Xref\\|Date\\):")
       (put 'nntp-mail-drop 'insert-function 'pcmail-load-nntp-mail)
       (put 'nntp-mail-drop 'display-errors-p t)
       (put 'nntp-mail-drop 'folder-delete-hook 'pcmail-delete-nntp-folder)
       
       ;; NNTP file mail drop -- this is a file of NNTP messages that have been
       ;; assembled by the nntp-slave program.  An indirect variant of the 
       ;; above
       
       (put 'nntp-file-mail-drop 'header-processing-function 
	    'pcmail-process-nntp-message)
       (put 'nntp-file-mail-drop 'msg-start-regexp 
	    "^\^L\n\\(Organization\\|Newsgroups\\|Path\\|From\\|Xref\\|Date\\):")
       (put 'nntp-file-mail-drop 'insert-function 'pcmail-rename-mail-drop)
       (put 'nntp-file-mail-drop 'folder-delete-hook 
	    'pcmail-delete-nntp-folder)
       (put 'nntp-file-mail-drop 'name-input-func
	    '(lambda () (pcmail-narrow-read-file-name "mail-log")))
       
       ;; Berkeley-mail mail drops
       (put 'spool-mail-drop 'header-processing-function 
	    'pcmail-process-unix-message)
       (put 'spool-mail-drop 'insert-function 'pcmail-do-unix-movemail)
       (put 'spool-mail-drop 'msg-start-regexp
	    (concat "^From [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
		    "[0-9]* [0-9:]* "		   ; time of day
		    "\\([A-Z]?[A-Z][A-Z]T \\|"        ; 3-char time zone
		    "[-+][0-9][0-9][0-9][0-9] \\|\\)" ; numeric offset
		    "19[0-9]*$"))

       (put 'berkeley-mail-drop 'header-processing-function 
	    'pcmail-process-unix-message)
       (put 'berkeley-mail-drop 'msg-start-regexp
	    (concat "^From [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
		    "[0-9]* [0-9:]* "		   ; time of day
		    "\\([A-Z]?[A-Z][A-Z]T \\|"        ; 3-char time zone
		    "[-+][0-9][0-9][0-9][0-9] \\|\\)" ; numeric offset
		    "19[0-9]*$"))
       (put 'berkeley-mail-drop 'insert-function 'pcmail-rename-mail-drop)
       (put 'berkeley-mail-drop 'name-input-func
	    '(lambda () (pcmail-narrow-read-file-name "~/mbox")))

       ;; MH "mail drop"
       
       (put 'mh-mail-drop 'header-processing-function 
	    'pcmail-process-mh-message)
       (put 'mh-mail-drop 'msg-start-regexp "^\^Lbegin-message\^L\n")
       (put 'mh-mail-drop 'insert-function 'pcmail-do-mh-movemail)
       (put 'mh-mail-drop 'display-errors-p t)

       ;; other environment stuff

       (put 'pcmail-mail-environment 'time-zone "PST")
       (put 'pcmail-mail-environment 'legal-folder-regexp
	    "[-0-9A-Za-z_$.+%#&!]+")
       (put 'pcmail-mail-environment 'printer (or (getenv "PRINTER") "lp"))
       (put 'pcmail-mail-environment 'print-function 'pcmail-unix-lpr-message)
       (put 'pcmail-mail-environment 'print-command "lpr")
       (put 'pcmail-mail-environment 'printer-errors-p t)
       (put 'pcmail-mail-environment 'mail-directory "~/.pcmail/")
       (put 'pcmail-mail-environment 'create-mail-directory-fn
	    'pcmail-unix-create-mail-directory)
       (put 'pcmail-mail-environment 'folder-to-file-function 'identity)
       (put 'pcmail-mail-environment 'default-mail-drop-list 
	    '(spool-mail-drop))))
     
;;;; UNIX functions

;;; message print function

(defun pcmail-unix-lpr-message (printer-name folder-name)
  "Send current message to printer
Args: (printer-name folder-name)
  Send the current message to the printer using LPR.  Call-process-region
on the current region.  Add job/title arguments so burst page looks nice."
  (let ((errorbuf (get 'pcmail-mail-environment 'printer-errors-p))
	(prtcmd (or (get 'pcmail-mail-environment 'print-command) "lpr")))
    (and errorbuf
	 (setq errorbuf (generate-new-buffer (concat " *lpr lossage*"))))
    (unwind-protect
	(save-excursion
	  (and errorbuf (buffer-disable-undo errorbuf))
	  (call-process-region (point-min) (point-max) prtcmd nil errorbuf nil
			       (concat "-P" printer-name))
	  (cond ((and errorbuf (buffer-modified-p errorbuf))
		 (set-buffer errorbuf)
		 (subst-char-in-region (point-min) (point-max) ?\n ?\  )
		 (goto-char (point-max))
		 (skip-chars-backward " \t")
		 (delete-region (point) (point-max))
		 (goto-char (point-min))
		 (and (looking-at (concat prtcmd ": "))
		      (delete-region (point-min) (match-end 0)))
		 (signal 'file-error
			 (list prtcmd
			       (buffer-substring (point-min) (point-max)))))))
      (and errorbuf (kill-buffer errorbuf)))))

;;; maildrop to folder move routines

;; UNIX mail-drop transfer routine

(defun pcmail-do-unix-movemail (mail-drop)
  "UNIX mail-drop transfer function.
Args: (mail-drop)
Call the emacs movemail utility to transfer <spool-directory>/foo to a 
temporary file, returning the temporary file's name to the caller.  If
MAIL-DROP has a 'display-errors-p property, signal any errors from movemail 
by formatting the movemail output in process output buffer."
  (let ((fromfile (substitute-in-file-name 
		   (concat (if (boundp 'rmail-spool-directory)
			       rmail-spool-directory
			     "/usr/spool/mail/")
			     "$USER")))
	(lossbuffer)
	(errors (get mail-drop 'display-errors-p))
	(tofile (expand-file-name (concat pcmail-directory ".newmail"))))

    ;; On some systems, <spool-directory>/foo is a directory
    ;; and the actual mail drop is <spool-directory>/foo/foo.
    (and (file-directory-p fromfile)
	 (setq fromfile
	       (substitute-in-file-name (expand-file-name "$USER" fromfile))))

    ;; if TOFILE exists, a mail get was interrupted for some reason.
    (cond ((file-exists-p tofile)
	   (unwind-protect
	       (progn
		 (setq lossbuffer (generate-new-buffer "**lose**"))
		 (switch-to-buffer lossbuffer t)
		 (insert-file-contents tofile)
		 (or (yes-or-no-p 
		      "Previous mail fetch was interrupted.  Finish it before new fetch? ")
		     (error "Aborted")))
	     (kill-buffer lossbuffer)))
	  ((file-exists-p fromfile)
	   (pcmail-generic-unix-movemail "movemail" exec-directory errors
					 fromfile tofile)))
    tofile))

;; UNIX NNTP mail transfer routine

(defun pcmail-load-nntp-mail (mail-drop)
  "UNIX NNTP mail-drop transfer function.
Args: (mail-drop)
Call the nntp_slave program to transfer netnews messages from a newgroup
with the same name as the current folder to a temporary file. If MAIL-DROP 
has a 'display-errors-p property, signal any errors from movemail by 
formatting the movemail output in process output buffer."
  (let ((errors (get mail-drop 'display-errors-p))
	(tofile (expand-file-name (concat pcmail-folder-name ".newnews")))
	(controlfile (concat pcmail-folder-name ".ctl")))
    (pcmail-generic-unix-movemail "nntp_slave" exec-directory errors
				  pcmail-nntp-host-name pcmail-folder-name
				  tofile controlfile)
    tofile))

(defun pcmail-delete-nntp-folder (folder-name)
  "NNTP-mail-drop-specific folder delete processing
Args: (foler_name)
  Run on delete of FOLDER_NAME with an attached nntp mail drop.  Deletes the
nntp_slave news control file associated with FOLDER_NAME."
  (condition-case nil
      (delete-file (expand-file-name (concat folder-name ".ctl") 
				     pcmail-directory))
    (file-error nil)))

;; Unix MH load

(defun pcmail-do-mh-movemail (mail-drop)
  "UNIX MH mail-drop transfer function.
Args: (mail-drop)
  Read an MH folder name from the minibuffer and use an export utility to
move all messages in the MH folder into a temporary file, returning
the temporary file's name to the caller.  If MAIL-DROP has a 
'display-errors-p property, signal any errors from the shell script by 
formatting the shell script output in the process output buffer."
  (let* ((folder (pcmail-mh-read-folder-name))
	 (errors (get mail-drop 'display-errors-p))
	 (tofile (expand-file-name (concat "~/Mail/" folder "/" folder 
					   ".mhexport"))))
    (pcmail-generic-unix-movemail "mh-to-pcmail-export" exec-directory
				  errors folder tofile)
    tofile))

(defun pcmail-mh-read-folder-name ()
  "Read a folder name from the minibuffer, using completion.
Args: none
  Use pcmail-completing-read to read an MH folder name from the minibuffer.
Completion directory is the standard MH mail directory ~/Mail/.  
Pcmail-completing-read takes an alist, so we need to convert the output of
file-name-all-completions to alist form.  In the process, remove trailing
slashes from any directory names in the completion set.  Completion set
is filtered through a lambda expression that passes only directories and
eliminates the special directories \".\" and \"..\"."
  (let ((mhdir (expand-file-name "~/Mail/")))
    (or (file-directory-p mhdir) 
	(error "Default MH mail directory \"%s\" does not exist." mhdir))
    (pcmail-completing-read 
     "Folder name: " 
     (mapcar '(lambda (s) (list (if (string-match ".*/$" s)
				    (substring s 0 -1)
				  s)))
	     (file-name-all-completions "" mhdir))
     nil
     '(lambda (s) (and (file-directory-p (expand-file-name (car s) mhdir))
		       (not (string= (car s) ".."))
		       (not (string= (car s) ".")))))))


;; generic call-process and error-handling part of the above three routines

(defun pcmail-generic-unix-movemail (progname dir errorbuf &rest args)
  "Generic mail mover.  Calls a program, formatting and signalling errors.
Args: (progname dir tofile fromfile errorbuf &rest args)
  If ERRORBUF is non-nil, generate an error buffer.  Call PROGNAME in
directory DIR, passing it arguments ARGS, and routing output to ERRORBUF
if present.  If errors occur, format the output in ERRORBUF and use it as
an argument to a file-error signal."
  (and errorbuf
       (setq errorbuf 
	     (generate-new-buffer (concat " *" progname " lossage*"))))
  (unwind-protect
      (save-excursion
	(and errorbuf (buffer-disable-undo errorbuf))
	(apply 'call-process (expand-file-name progname dir) nil errorbuf nil 
	       args)
	(cond ((and errorbuf (buffer-modified-p errorbuf))
	       (set-buffer errorbuf)
	       (subst-char-in-region (point-min) (point-max) ?\n ?\  )
	       (goto-char (point-max))
	       (skip-chars-backward " \t")
	       (delete-region (point) (point-max))
	       (goto-char (point-min))
	       (and (looking-at (concat progname ": "))
		    (delete-region (point-min) (match-end 0)))
	       (signal 'file-error
		       (list progname
			     (buffer-substring (point-min) (point-max)))))))
    (and errorbuf (kill-buffer errorbuf))))

;; Babyl mail drop

(put 'babyl-mail-drop 'header-processing-function
     'pcmail-process-babyl-message)
(put 'babyl-mail-drop 'msg-start-regexp "BABYL OPTIONS:\\|\^L")
(put 'babyl-mail-drop 'insert-function 'pcmail-rename-mail-drop)
(put 'babyl-mail-drop 'name-input-func
     '(lambda () (pcmail-narrow-read-file-name rmail-file-name)))

;;; Generic mail drop insert function

(defun pcmail-rename-mail-drop (mail-drop)
  "A generic mail drop insert function
Args: (mail-drop)
  Read a mail drop file name from the minibuffer, then rename it to a
temporary file, returning the name of the temporary file to the caller."
  (or (get mail-drop 'name-input-func)
      (error "Missing mail drop name input property in mail drop %s"
	     mail-drop))
  (let ((tofile) 
	(fromfile (funcall (get mail-drop 'name-input-func))))
    (and (file-directory-p fromfile)
	 (error "Cannot load, %s is a directory!" fromfile))
    (cond ((file-exists-p fromfile)
	   (setq tofile 
		 (concat (file-name-directory fromfile) "new-" 
			 (file-name-nondirectory fromfile)))
	   (rename-file fromfile tofile nil)
	   tofile))))

;;; message header processing routines.  These functions look from point
;;; forward for a message-begin regexp (end of current message, beginning of 
;;; next message).  They narrow to that region and convert the message 
;;; to RFC822 format.  This means stripping mail-drop-specific message
;;; delimiting text and cleaning up headers

;;; Babyl processing routine

(defun pcmail-process-babyl-message (mail-drop)
  "Process a Babyl message header
Args: (mail-drop)
  Process a Babyl message.  If looking at Babyl header, nuke
it.  If looking at Babyl message, remove summary-line field if present.  
Assume the current buffer is narrowed from point to end-of-buffer."
  (cond ((looking-at "BABYL OPTIONS:")
	 (setq newmsgs (1- newmsgs)) ;not a real message
	 (re-search-forward "\n\^_" nil 'move)
	 (delete-region (point-min) (point)))
	((looking-at "\^L")
	 (let ((end) (case-fold-search t))
	   (re-search-forward "\n\^_" nil 'move)
	   (narrow-to-region (point-min) (point))
	   (goto-char (point-min))
	   (save-excursion
	     (cond ((search-forward pcmail-header-delim nil t)
		    (and (re-search-backward
			  "^summary-line:.*\n\\([ \t]+.*\n\\)*" nil 'move)
			 (replace-match "")))))
	   (pcmail-convert-message-to-folder-format mail-drop)))))
	   
;;; NNTP message conversion routine

(defun pcmail-process-nntp-message (mail-drop)
  "Process an NNTP slave message.  See pcmail-process-unix-message.
Args: (mail-drop)"
  (let ((start (point))
	(msgseparator (get mail-drop 'msg-start-regexp)))
    ;point must be at this regexp; see convert-region-to-babyl-format
    (and (looking-at msgseparator)
	 (delete-char 2))
    (and (re-search-forward msgseparator nil 'move)
	 (goto-char (match-beginning 0)))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (let ((hdrend (progn
		    (or (re-search-forward pcmail-header-delim nil 'move)
			(insert pcmail-header-delim))
		    (point)))
	  (case-fold-search t))
      (save-excursion
	(save-restriction
	  (narrow-to-region (point-min) hdrend)
	  (and (re-search-backward "^Newsgroups:" nil t)
	       (replace-match "To:")))))
    (pcmail-convert-message-to-folder-format mail-drop)))

;; MH conversion routine

(defun pcmail-process-mh-message (mail-drop)
  "Process an exported MH message.  See pcmail-process-unix-message.
Args: none"
  (let ((start (point))
	(msgseparator (get mail-drop 'msg-start-regexp)))
    ;point must be at this regexp; see convert-region-to-babyl-format
    (and (looking-at msgseparator)
	 (replace-match ""))
    (and (re-search-forward msgseparator nil 'move)
	 (goto-char (match-beginning 0)))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (pcmail-convert-message-to-folder-format mail-drop)))

;;; Berkeley MAIL conversion routine

(defun pcmail-process-unix-message (mail-drop)
  "Process a Berkeley Mail message.
Args: none
  Process a UNIX-style Mail header.  From point, search forward for next
message start regexp (which is end of current message).  Narrow to this
region and perform any standard header processing.  Call the folder-specific
conversion routine, which will add format-specific headers and trailers as
necessary.  Assume the current buffer is narrowed from point to end of buffer."
  (let ((start (point))
	(msgseparator (get mail-drop 'msg-start-regexp)))
    (forward-line 1)				        ;over From line
    (and (re-search-forward msgseparator nil 'move)
	 (goto-char (match-beginning 0)))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (pcmail-convert-message-to-folder-format mail-drop)))

(defun pcmail-bash-unix-header ()
  "Turn a Berkeley Mail header into an RFC822 header
Args: none"
  (let ((hdrend (progn
		  (or (re-search-forward pcmail-header-delim nil 'move)
		      (insert pcmail-header-delim))
		  (point)))
	(case-fold-search t))
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) hdrend)
	(goto-char (point-min))
	(pcmail-maybe-gronk-unix-header)))))

(defun pcmail-maybe-gronk-unix-header ()
  "Transform unix mail header.
Args: none
  If there is a righteous from or date field, nuke the non-standard Berkeley
from field, otherwise extract from and date field info from it and create
righteous fields before nuking the Berkeley from field.  Assume buffer is
narrowed to the message header."
  (let ((case-fold-search t) (has-from) (has-date))
    (goto-char (point-min))
    (and (re-search-forward "^Date:[ \t]+.*\n\\([\t ]+.*\n\\)*" nil t)
	 (setq has-date t))
    (goto-char (point-min))
    (and (re-search-forward "^From:[ \t]+.*\n\\([\t ]+.*\n\\)*" nil t)
	 (setq has-from t))
    (goto-char (point-min))

    ; if the header has neither a from nor a date field, create them using
    ; the Berkeley from field
    (let ((case-fold-search nil))
      (and (re-search-forward	;The Pinhead Header
	    "^From \\([^ ]*\\(\\|\".*\"[^ ]*\\)\\)  ?\\([^ ]*\\) \\([^ ]*\\) *\\([0-9]*\\) \\([0-9:]*\\)\\( [A-Z]?[A-Z][A-Z]T\\|[-+][0-9][0-9][0-9][0-9]\\|\\) 19\\([0-9]*\\)\n" nil t)
	   (replace-match
	    (concat
	     (cond (has-date
		    "")
		   ((= (match-beginning 7) (match-end 7))
		    (concat "Date: \\3, \\5 \\4 \\8 \\6 " pcmail-time-zone
			    "\n"))
		   (t
		    "Date: \\3, \\5 \\4 \\8 \\6\\7\n"))
	     (cond (has-from
		    "")
		   (t
		    "From: \\1\n"))))))))

;;; initial mail directory create

(defun pcmail-unix-create-mail-directory ()
  "Create UNIX local mail directory.
Args: none"
  (call-process "mkdir" nil nil nil 
		(directory-file-name (expand-file-name pcmail-directory))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; VMS SYSTEM-SPECIFIC FUNCTIONS.  IF YOU DON'T RUN VMS AND WANT TO SAVE 
;;;; SPACE, CUT HERE, BEGING CAREFUL TO PRESERVE THE (PROVIDE 'PCMAILSYSDEP)
;;;; FORM ON THE LAST LINE OF THE FILE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; message print function

(defun pcmail-vms-print-message (printer-name &ignore)
  "Send the current message to printer queue.
Args: (printer-name &ignore)
  Send the current message to printer queue PRINTER-NAME using a canned
COM file.  First write message to a file.  COM file prints the file and may 
delete it."
  (let ((temp-file (expand-file-name "pcmail-msg.txt" pcmail-directory))
	(com-file (expand-file-name "vms-doprint.com" exec-directory))
	(bname (buffer-name)))
    (write-region (point-min) (point-max) temp-file)
    (pcmail-vms-command (format "@%s %s %s" com-file printer-name 
				temp-file "delete"))
    (set-buffer bname)))		;pcmail-vms-command lossage

;;; directory create function

(defun pcmail-vms-create-mail-directory ()
  "Create VMS local mail directory.
Args: none"
  (pcmail-vms-command (concat "create/dir " pcmail-directory))
  (while (not (file-directory-p pcmail-directory))))


;;; mail-drop move function

(defun pcmail-do-vms-movemail (mail-drop)
  "VMS mail-drop transfer function.
Args: (mail-drop)
  Call a COM file to transfer a file named newmail into a temporary file
named newmail.  Return the file name to the caller.  Assumes existence of 
a function called pcmail-vms-command which does a non-blocking exexute of a 
DCL command in an kept inferior process."
  (let ((bname (buffer-name))
	(fromfile "newmail")
	(tofile "mail.temp"))
    (condition-case nil
	(delete-file tofile)		;in case of previous lossage
      (file-error nil))
    (pcmail-vms-command (concat "@" 
			 (expand-file-name "vms-movemail.com" exec-directory)
			 " "
			 fromfile
			 " "
			 tofile
			 " "
			 (file-name-directory (buffer-file-name))))
    (set-buffer bname)		   	         ;pcmail-vms-command lossage
    (while (not (file-exists-p tofile)))         ;gag, choke, nonblocking call
    tofile))


;;; process a VMS VAX-MAIL message.  This is pretty 
;;; horrific, but works well enough.

(defun pcmail-process-vms-message (mail-drop)
  "Process a VMS-style message.  See pcmail-process-unix-message.
Args: mail-drop"
  (let ((start (point))
	(msgseparator (get 'vms-default-mail-drop 'msg-start-regexp)))

    ;point must be at this regexp; see convert-region-to-babyl-format
    (and (looking-at msgseparator)
	 (delete-char 2))
    (and (re-search-forward msgseparator nil 'move)
	 (goto-char (match-beginning 0)))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (pcmail-bash-vms-header)
    (goto-char (point-min))
    (pcmail-convert-message-to-folder-format mail-drop)))

(defun pcmail-bash-vms-header ()
  "Convert a VMS message header to at least minimally resemble an RFC822 header
Args: none
  Assume the region is narrowed to the current message."
  (let ((hdrend (progn
		  (or (re-search-forward pcmail-header-delim nil 'move)
		      (insert pcmail-header-delim))
		  (point)))
	(case-fold-search t))
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) hdrend)
	(pcmail-maybe-gronk-vms-header)))))

(defun pcmail-maybe-gronk-vms-header ()
  "Reformat or nuke VMS fields as necessary.  Not too bad.
Args: none"
  (goto-char (point-min))
  (cond ((re-search-forward
	  (concat "^From:\t"			;Anatomy of a VMS From: field
		  "\\(\\(\\w+::\\)?"	        ;optional host name
		  "\\(\\w+\\)"	                ;user name
		  "[ \t]*\\(\".*\"\\)?[ \t]*\\)";comment
		  "\\([0-9]+\\)-" 		;day
		  "\\([a-zA-Z]+\\)-"    	;month
		  "19\\([0-9]+\\)[ \t]*"	;year
		  "\\([0-9]+:[0-9]+\\)\\(:[0-9]+\\)?.*\n")      ;time
	  nil t)
	 (replace-match 
	  (concat "Date: \\5 "
		  (capitalize 
		   (buffer-substring (match-beginning 6) (match-end 6)))
		  " \\7 \\8 " pcmail-time-zone "\n"
		  "From: \\1\n")
	  t)))

  ; find subject and reformat if it exists, punt if blank
  (goto-char (point-min))
  (let ((no-subject) (has-cc) (has-to) (no-cc))
    (setq no-subject 
	  (let ((temp-subj (mail-fetch-field "subj")))
	    (zerop (length temp-subj))))
    (and (re-search-forward "^Subj:\t\\(.*\n\\([ \t]+.*\n\\)?\\)" nil t)
	 (replace-match (if no-subject "" "Subject: \\1") t))
  
    ; find CC and reformat if it exists, punt if blank
    (goto-char (point-min))
    (setq no-cc 
	  (let ((temp-cc (mail-fetch-field "cc")))
	    (zerop (length temp-cc))))
    (and (re-search-forward "^CC:\t\\(.*\n\\([ \t]+.*\n\\)?\\)" nil t)
	 (if no-cc
	     (replace-match "")
	   (replace-match "Cc: \\1" t)))

    ; find to: field and reformat if no other to fields exists
    (goto-char (point-min))
    (and (re-search-forward "^To:\t" nil t)
	 (replace-match "To: "))))

;;; folder-to-file translate function

(defun pcmail-vms-folder-name-to-file (folder-name)
  "Return a copy of FOLDER-NAME that has been translated into a valid VMS
file name.  The translation converts \".\" characters into \"_\" characters
and \"+\" characters into \"$\" characters."
  (let ((i 0) (outbox (copy-sequence folder-name)))
    (while (< i (length outbox))
      (and (= (aref outbox i) ?.) (aset outbox i ?_))
      (and (= (aref outbox i) ?+) (aset outbox i ?$))
      (setq i (1+ i)))
    outbox))

;;; mail transmission function

(defconst pcmail-vms-mailcopy "SYS$SCRATCH:MAIL.CPY"
  "File used to store body of message when using VMS mail utility.
Deleted on mail transmit.")

(defun pcmail-vms-send-mail ()
  "Send a message using VMS Mail.
Args: none
  Copy message body to a file.  Using message header, create TO and SUBJECT
arguments after converting addresses from RFC822 format to VMS format.  
Call VMS MAIL with to, subject, and body file arguments.  Note that this is
a hack, and may break down from time to time."
  (let ((tembuf) (case-fold-search t) (to) (cc) (subj) (delimline)
	(mailbuf (current-buffer)))
    (and (file-exists-p pcmail-vms-mailcopy) (delete-file pcmail-vms-mailcopy))
    (find-file pcmail-vms-mailcopy)
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (setq tembuf (current-buffer))
	    (erase-buffer)
	    (insert-buffer-substring mailbuf)
	    ;; Find end of header and narrow to it.
	    (goto-char (point-min))
	    (or (re-search-forward
		 (concat "^" (regexp-quote mail-header-separator)))
		(error "Improperly formatted mail buffer."))
	    (setq delimline (point-marker))
	    (replace-match "")
	    (narrow-to-region (point-min) delimline)
	    
	    ;; Find and handle any aliases.
	    (and mail-aliases 
		 (expand-mail-aliases (point-min) delimline))

	    ;; Remove any blank lines in the header.
	    (goto-char (point-min))
	    (while (re-search-forward "^[ \t\n]*\n" delimline t)
	      (replace-match ""))

	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (and (re-search-forward "^FCC:" delimline t)
		 (mail-do-fcc delimline))

	    ;; don't send out a blank subject line
	    (goto-char (point-min))
	    (and (re-search-forward "^Subject:[ \t]*\n" delimline t)
		 (replace-match ""))

	    (goto-char (point-min))
	    (pcmail-vms-mail-convert-text-field "subject")
	    (setq to (or (mail-fetch-field "to" nil t)
			 (error "Message must have a to: recipient.")))
	    (setq cc (mail-fetch-field "cc" nil t))
	    (and cc (setq to (concat to "," cc)))
	    (setq to (pcmail-vms-mail-nl-to-space to))
	    (if (setq subj (mail-fetch-field "subject" t))
		(setq subj (pcmail-vms-mail-nl-to-space
			    (concat "/SUBJECT=\"" subj "\"")))
	      (setq subj "")))
	  (delete-region (point-min) delimline)
	  (write-file (buffer-file-name))
	  ;; Make call to VMS Mail.
	  (pcmail-vms-command (concat "MAIL" subj " -"))
	  (pcmail-vms-command (concat pcmail-vms-mailcopy " -"))
	  (pcmail-vms-command (concat "\"" to "\""))
	  (pcmail-vms-command " "))    ; to clear out any prompts due to errors
      (set-buffer tembuf)
      (set-buffer-modified-p nil)
      (kill-buffer tembuf))))

(defun pcmail-vms-mail-convert-text-field (field)
  "Convert RFC822 text fields to VMS format.
Args: (field)"
  (let ((start)
	(case-fold-search t))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(cond ((re-search-forward (concat "^" (regexp-quote field) ":[ \t]*")
				  nil t)
	       (setq start (point))
	       (while (progn (forward-line 1)
			     (looking-at "[ \t]")))
	       (narrow-to-region start (point))
	       (goto-char start)
	       (while (re-search-forward "\"" nil t)
		 (replace-match "\"\""))))))))
  
(defun pcmail-vms-mail-nl-to-space (s)
  "Convert all whitespace in S to spaces and return the result.  Modifies S.
Args: (s)"
  (let ((i 0))
    (while (< i (length s))
      (and (or (= (aref s i) ?\n)
	       (= (aref s i) ?\t))
	   (aset s i ? ))
      (setq i (1+ i))))
  s)


;;; subprocess capability.  NOTE THAT THIS IS A HACK.  A GRUNGY HACK.

(defvar pcmail-vms-process-id nil
  "Process ID of inferior VMS process used by pcmail-vms-command.")

(defvar pcmail-vms-process-buffer "*DCL Output*"
  "Name of buffer where output from VMS process goes.")

(defun pcmail-vms-command (s)
  "Send a string S to a kept inferior VMS process.
Args: (s)
  If variable PCMAIL-VMS-PROCESS-ID is unbound, spawn a process using the
SPAWN-PROCESS function.  Then send S to the process using the
SEND-COMMAND-TO-SUBPROCESS function."
  (cond ((not pcmail-vms-process-id)
	 (setq pcmail-vms-process-id (random))
	 (spawn-subprocess pcmail-vms-process-id 'pcmail-vms-process-input)))
  (send-command-to-subprocess pcmail-vms-process-id s))

(defun pcmail-vms-process-input (id s)
  "Called when input string S arrives from VMS process with handle ID
Args: (id s)
  Place input in buffer PCMAIL-VMS-PROCESS-BUFFER and display that buffer in
another window."
  (pop-to-buffer pcmail-vms-process-buffer)
  (goto-char (point-max))
  (insert s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; STOP CUTTING HERE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'pcmailsysdep)
