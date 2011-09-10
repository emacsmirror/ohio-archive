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

;;;; global variables

;;; defaults

(defvar pcmail-last-file (expand-file-name "~/pcmail-archive")
  "The name of the last file given to an archive command.")

;;;; Pcmail output and copy commands by single message and subset

;;; archive a message or current subset

(defun pcmail-archive-subset (file dont-delete)
  "Archive the current message subset.
Args: (file dont-delete)
  Append the current message subset to the file named by pcmail-last-file.
If the variable pcmail-delete-on-archive is non-NIL, set the deleted attribute
on all messages in the subset after archiving.  If called interactively, a 
prefix argument means do not delete after archiving no matter what the
setting of pcmail-delete-on-archive.  Archived messages have their archived 
attribute set upon archiving."
  (interactive
   (list
    (setq pcmail-last-file 
	  (pcmail-read-file-name "Archive subset to file: " pcmail-last-file))
    current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (pcmail-archive-message-1 file dont-delete 1 (pcmail-current-subset-length)))

(defun pcmail-archive-message (file dont-delete)
  "Archive the current message.
Args: (file dont-delete)
  Append this message to the file named by pcmail-last-file.  If the variable 
pcmail-delete-on-archive is non-NIL, set this message's deleted attribute 
after archiving, and move to the next interesting message in the folder.  
If called interactively, a prefix argument means do not delete after archiving
no matter what the setting of pcmail-delete-on-archive.  Set the message's 
archived attribute."
  (interactive
   (list
    (setq pcmail-last-file
	  (pcmail-read-file-name "Archive message to file: " pcmail-last-file))
    current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (pcmail-archive-message-1 file dont-delete pcmail-current-subset-message 1))

(defun pcmail-archive-message-1 (file-name dont-delete start len)
  "Archive a portion of the current message subset.
Args: (file-name dont-delete start len)
  Append to FILE-NAME all messages in the current subset, from message START 
for LEN messages.  Set the archived attribute on all archived messages.  Set 
the deleted attribute on all archived messages if pcmail-delete-on-archive 
is non-NIL and DONT-DELETE is NIL.  If any messages were deleted, move to 
the next interesting message in the subset after archiving."
  (let ((cbuf (current-buffer)) (i start))
    (message "Archiving...")
    (unwind-protect
	(while (< i (+ start len))
	  (pcmail-narrow-to-message (pcmail-make-absolute i))
	  (let ((beg (point-min))
		(end (point-max)))
	    (save-excursion
	      (find-file file-name)
	      (goto-char (point-max))
	      (and pcmail-archive-hook
		   (funcall pcmail-archive-hook i))
	      (insert-buffer-substring cbuf beg end)
	      (insert "\n\n"))
	    (pcmail-set-attribute (pcmail-make-absolute i) "filed" t)
	    (and pcmail-delete-on-archive 
		 (not dont-delete)
		 (pcmail-set-attribute (pcmail-make-absolute i) "deleted" 
				       t)))
	  (and (zerop (% (- (setq i (1+ i)) start) pcmail-progress-interval))
	       (message "Archiving...%d" (- i start))))
      (and pcmail-delete-on-archive
	   (not dont-delete)
	   (pcmail-next-message))
      (pcmail-update-folder-mode-line pcmail-current-subset-message)
      (save-excursion
	(find-file file-name)
	(save-buffer)
	(bury-buffer (current-buffer))))
    (message "Archiving...done (%d message%s)" (- i start) 
	     (pcmail-s-ending (- i start)))))

;;; print a message or current subset

(defun pcmail-print-subset (printer dont-delete)
  "Print each message in the current subset.
Args: (printer dont-delete)
  Send the current message subset to a named printer.  The default printer 
is specified by the variable pcmail-printer-name.  A system-dependent print
routine set up in pcmail-mail-environment prints the message.  If the 
variable pcmail-delete-on-print is non-NIL, set the deleted attribute of all 
messages in the subset after printing.  If called interactively,
a prefix argument means do not delete after printing no matter what the
setting of pcmail-delete-on-print.  Printed messages have their printed 
attribute set upon printing."
  (interactive
      (list (setq pcmail-printer-name 
		  (pcmail-read-string-default "Send subset to printer: " 
					      pcmail-printer-name
					      t))
	    current-prefix-arg))  
  (pcmail-barf-if-empty-folder)
  (pcmail-print-message-1 printer dont-delete 1 
			  (pcmail-current-subset-length)))

(defun pcmail-print-message (printer dont-delete)
  "Print the current message.
Args: (printer dont-delete)
  Send the current message to a named printer.  The default printer is 
specified by the variable pcmail-printer-name.  A system-dependent print
routine set up in pcmail-mail-environment prints the message.  If the 
variable pcmail-delete-on-print is non-NIL, set the message's deleted 
attribute after printing, and move to the next interesting message in the 
folder.  If called interactively, a prefix argument means do not delete 
after printing no matter what the setting of pcmail-delete-on-print.
Set the message's printed attribute."
  (interactive
      (list (setq pcmail-printer-name 
		  (pcmail-read-string-default "Send message to printer: " 
					      pcmail-printer-name t))
	    current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (pcmail-print-message-1 printer dont-delete pcmail-current-subset-message 1))

(defun pcmail-print-message-1 (printer-name dont-delete start len)
  "Print a portion of the current subset.
Args: (printer-name dont-delete start len)
  Send to PRINTER-NAME all messages in the current subset, from message START 
for LEN messages.  Set the printed attribute on all printed messages.  Set 
the deleted attribute on all printed messages if pcmail-delete-on-print is 
non-NIL and DONT-DELETE is NIL.  If any messages were deleted, move to the 
next interesting message in the subset after printing."
  (let ((folder-name pcmail-folder-name) (i start))
    (message "Printing...")
    (unwind-protect
	(while (< i (+ start len))
	  (pcmail-narrow-to-message (pcmail-make-absolute i))
	  (save-excursion
	    (funcall (get 'pcmail-mail-environment 'print-function) 
		     printer-name folder-name)
	    (pcmail-set-attribute (pcmail-make-absolute i) "printed" t)
	    (and pcmail-delete-on-print 
		 (not dont-delete)
		 (pcmail-set-attribute (pcmail-make-absolute i) "deleted" t)))
	  (and (zerop (% (- (setq i (1+ i)) start) pcmail-progress-interval))
	       (message "Printing...%d" (- i start))))
      (and pcmail-delete-on-print
	   (not dont-delete)
	   (pcmail-next-message))
      (pcmail-update-folder-mode-line pcmail-current-subset-message))
    (message "Printing...done (%d message%s)" (- i start)
	     (pcmail-s-ending (- i start)))))
   
;;; copy a message or current subset

(defun pcmail-copy-subset (folder-name dont-delete)
  "Copy to a named folder each message in the current subset.
Args: (folder-name dont-delete)
  Copy the current message subset to a named folder.  If called interactively,
request a folder name from the minibuffer.  Completion of input is permitted;
input defaults to the name of the last folder given to a folder command.
If the variable pcmail-delete-on-copy is non-NIL, set the deleted attribute 
of all messages in the subset after copying.  If called interactively,
a prefix argument means do not delete after copying no matter what the
setting of pcmail-delete-on-copy.  Copied messages have their copied
attribute set upon copying."
  (interactive
      (list (pcmail-read-folder "Copy subset to folder: ") 
	    current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (pcmail-copy-message-1 folder-name dont-delete 1 
			 (pcmail-current-subset-length))
  (pcmail-update-folder-mode-line pcmail-current-subset-message))

(defun pcmail-copy-message (folder-name dont-delete)
  "Copy the current message to a named folder.
Args: (folder-name dont-delete)
  Copy the current message to a named folder.  If called interactively,
request a folder name from the minibuffer.  Completion of input is permitted;
input defaults to the name of the last folder given to a folder command.
If the variable pcmail-delete-on-copy is non-NIL, set the message's deleted 
attribute after copying, and move to the next interesting message in the 
folder.  If called interactively, a prefix argument means do not delete 
after copying no matter what the setting of pcmail-delete-on-copy.  Set the
message's copied attribute."
  (interactive
      (list (pcmail-read-folder "Copy message to folder: ")
	    current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (pcmail-copy-message-1 folder-name dont-delete pcmail-current-subset-message
			 1)
  (and pcmail-delete-on-copy
       (not dont-delete)
       (pcmail-next-message))
  (pcmail-update-folder-mode-line pcmail-current-subset-message))

(defun pcmail-copy-message-1 (target-folder-name dont-delete start len)
  "Copy a portion of the current subset.
Args: (target-folder-name dont-delete start len)
  Copy to FOLDER-NAME all messages in the current subset, from message START 
for LEN messages.  Set the copied attribute on all copied messages.  Set 
the deleted attribute on all copied messages if pcmail-delete-on-copy is 
non-NIL and DONT-DELETE is NIL.  If any messages were deleted, move to the 
next interesting message in the subset after copying."
  (let ((source-folder-name pcmail-folder-name) (eom) (i start))
    (and (string= target-folder-name source-folder-name) 
	 (error "Cannot copy message into itself"))
    (or (pcmail-find-folder target-folder-name)
	(error "Target folder %s not found" target-folder-name))
    (save-excursion
      (pcmail-open-folder target-folder-name)
      (save-restriction
	(widen)
	(setq eom (point-max))))
    (message "Copying to %s..." target-folder-name)
    (unwind-protect
	(while (< i (+ start len))
	  (pcmail-perform-copy source-folder-name target-folder-name i)
	  (pcmail-set-attribute (pcmail-make-absolute i) "copied" t)
	  (and pcmail-delete-on-copy
	       (not dont-delete)
	       (pcmail-set-attribute (pcmail-make-absolute i) "deleted" t))
	  (and (zerop (% (- (setq i (1+ i)) start) pcmail-progress-interval))
	       (message "Copying to %s...%d" target-folder-name 
			(- i start))))

      ; and now update target folder
      (save-excursion
	(set-buffer (pcmail-folder-buffer-name target-folder-name))
	(pcmail-save-buffer)
	(pcmail-set-message-vectors eom)
	(pcmail-narrow-to-message 
	 (pcmail-make-absolute pcmail-current-subset-message))
	(pcmail-set-nmessages target-folder-name pcmail-total-messages)
	(pcmail-change-in-folder-list target-folder-name 
				       pcmail-total-messages)))
    (bury-buffer (get-buffer (pcmail-folder-buffer-name target-folder-name)))
    (message "Copying to %s...done (%d message%s)" target-folder-name
	     (- i start) (pcmail-s-ending (- i start)))))

(defun pcmail-perform-copy (source target n)
  "Append message absolute-numbered N in source folder to target folder.
Args: (source target n)"
  (let ((msg (pcmail-message-contents (pcmail-make-absolute n))))
    (save-excursion
      (save-restriction
	(set-buffer (pcmail-folder-buffer-name target))
	(widen)
	(goto-char (point-max))
	(let ((buffer-read-only nil))
	  (insert msg))))))

(defun pcmail-wastebasket-message (start len)
  "Copy a portion of the current message subset to the wastebasket folder.
Args: (start len)
  If the current folder is not the wastebasket, copy part of the current
folder to the wastebasket.  Ask to create pcmail-wastebasket-folder if it 
does not exist.  Then call pcmail-copy-message-1 to perform the copy."
  (cond ((not (string= pcmail-folder-name pcmail-wastebasket-folder))
	 (cond ((not (pcmail-find-folder pcmail-wastebasket-folder))
		(or (yes-or-no-p (concat "Wastebasket folder \"" 
					 pcmail-wastebasket-folder
					 "\" does not exist.  Create? "))
		    (error "Aborted."))
		(pcmail-create-folder pcmail-wastebasket-folder)))
	 (save-excursion
	   (save-restriction
	     (pcmail-copy-message-1 pcmail-wastebasket-folder nil start
				    len))))))

(provide 'pcmailout)
