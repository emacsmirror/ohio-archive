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

;;;; folder commands and utilities

;;;; global variables

;;; system-defined globals

(defconst pcmail-folder-regexp
  (get 'pcmail-mail-environment 'legal-folder-regexp)
  "Regexp describing a legal folder name.")

(defconst pcmail-folder-line-regexp
  (concat "Folder[ \t]+\\(" pcmail-folder-regexp "\\):[ \t]*\\([0-9]+\\)")
  "Regexp that finds a folder entry in the folder list buffer and binds its
name to \\1 and its message count to \\2.")

(defvar pcmail-folder-mode-map nil
  "Key map for pcmail mode.")

(defvar pcmail-valid-folder-formats '(babyl-format)
  "A list of valid folder formats.  Used by pcmail-determine-folder-format")

;;; defaults

(defvar pcmail-last-folder nil
  "The last folder name given to a folder command.")

;;;; folder mode definition

(if pcmail-folder-mode-map
    nil
  (suppress-keymap (setq pcmail-folder-mode-map (make-keymap)))
  (define-key pcmail-folder-mode-map "?" 'describe-mode)
  (define-key pcmail-folder-mode-map "." 'pcmail-beginning-of-message)
  (define-key pcmail-folder-mode-map " " 'scroll-up)
  (define-key pcmail-folder-mode-map ">" 'pcmail-last-message)
  (define-key pcmail-folder-mode-map "<" 'pcmail-goto-message)
  (define-key pcmail-folder-mode-map "a" 'pcmail-archive-message)
  (define-key pcmail-folder-mode-map "b" 'pcmail-sort-folder)
  (define-key pcmail-folder-mode-map "c" 'pcmail-copy-message)
  (define-key pcmail-folder-mode-map "d" 'pcmail-delete-message)
  (define-key pcmail-folder-mode-map "e" 'pcmail-expunge-folder)
  (define-key pcmail-folder-mode-map "f" 'pcmail-forward-message)
  (define-key pcmail-folder-mode-map "g" 'pcmail-get-mail)
  (define-key pcmail-folder-mode-map "h" 'pcmail-summarize-folder)
  (define-key pcmail-folder-mode-map "i" 'pcmail-change-message-priority)
  (define-key pcmail-folder-mode-map "j" 'pcmail-goto-message)
  (define-key pcmail-folder-mode-map "k" 'pcmail-kill-message-later)
  (define-key pcmail-folder-mode-map "l" 'pcmail-load-mail-drop)
  (define-key pcmail-folder-mode-map "m" 'pcmail-mail)
  (define-key pcmail-folder-mode-map "n" 'pcmail-next-message)
  (define-key pcmail-folder-mode-map "o" 'pcmail-print-message)
  (define-key pcmail-folder-mode-map "p" 'pcmail-previous-message)
  (define-key pcmail-folder-mode-map "q" 'pcmail-quit)
  (define-key pcmail-folder-mode-map "r" 'pcmail-answer-message)
  (define-key pcmail-folder-mode-map "s" 'pcmail-save-folder)
  (define-key pcmail-folder-mode-map "t" 'pcmail-toggle-message-header)
  (define-key pcmail-folder-mode-map "u" 'pcmail-undelete-previous-message)
  (define-key pcmail-folder-mode-map "v" 'pcmail-version-information)
  (define-key pcmail-folder-mode-map "w" 'pcmail-edit-message)
  (define-key pcmail-folder-mode-map "x" 'pcmail-expand-subset)
  (define-key pcmail-folder-mode-map "y" 'pcmail-change-message-attr)
  (define-key pcmail-folder-mode-map "z" 'pcmail-zap-to-message)
  (define-key pcmail-folder-mode-map "\C-d" 'pcmail-delete-message-backward)
  (define-key pcmail-folder-mode-map "\C-m" 'pcmail-next-message)
  (define-key pcmail-folder-mode-map "\C-xm" 'pcmail-mail)
  (define-key pcmail-folder-mode-map "\177" 'scroll-down)
  (define-key pcmail-folder-mode-map "\ec" 'pcmail-create-folder)
  (define-key pcmail-folder-mode-map "\ed" 'pcmail-delete-folder)
  (define-key pcmail-folder-mode-map "\em" 'pcmail-folder-list-folders)
  (define-key pcmail-folder-mode-map "\en" 'pcmail-next-message-of-type)
  (define-key pcmail-folder-mode-map "\ep" 'pcmail-previous-message-of-type)
  (define-key pcmail-folder-mode-map "\er" 'pcmail-rename-folder)
  (define-key pcmail-folder-mode-map "\e\C-a" 'pcmail-archive-subset)
  (define-key pcmail-folder-mode-map "\e\C-c" 'pcmail-copy-subset)
  (define-key pcmail-folder-mode-map "\e\C-d" 'pcmail-delete-subset)
  (define-key pcmail-folder-mode-map "\e\C-f" 'pcmail-filter-folder)
  (define-key pcmail-folder-mode-map "\e\C-i" 'pcmail-change-priority-subset)
  (define-key pcmail-folder-mode-map "\e\C-n" 'pcmail-get-next-folder-mail)
  (define-key pcmail-folder-mode-map "\e\C-o" 'pcmail-print-subset)
  (define-key pcmail-folder-mode-map "\e\C-u" 'pcmail-undelete-subset)
  (define-key pcmail-folder-mode-map "\e\C-y" 'pcmail-change-attr-subset))


;; Emacs V19 SUBSET menu bar
(define-key pcmail-folder-mode-map [menu-bar subsets]
  (cons "Subsets" (make-sparse-keymap "Subsets")))
(define-key pcmail-folder-mode-map [menu-bar subsets priority]
  '("Change Priority..." . pcmail-change-priority-subset))
(define-key pcmail-folder-mode-map [menu-bar subsets clear-attribute]
  '("Clear Attribute..." . pcmail-clear-attr-subset-menu))
(define-key pcmail-folder-mode-map [menu-bar subsets set-attribute]
  '("Set Attribute..." . pcmail-set-attr-subset-menu))
(define-key pcmail-folder-mode-map [menu-bar subsets undelete]
  '("Undelete" . pcmail-undelete-subset))
(define-key pcmail-folder-mode-map [menu-bar subsets delete]
  '("Delete" . pcmail-delete-subset))
(define-key pcmail-folder-mode-map [menu-bar subsets archive]
  '("Archive to File..." . pcmail-archive-subset))
(define-key pcmail-folder-mode-map [menu-bar subsets print]
  '("Print..." . pcmail-print-subset))
(define-key pcmail-folder-mode-map [menu-bar subsets copy]
  '("Copy to Folder..." . pcmail-copy-subset))


;; Emacs V19 MAIL menu bar
(define-key pcmail-folder-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))
(define-key pcmail-folder-mode-map [menu-bar mail mail-subset]
  '("Mail Subset" . pcmail-mail-subset))
(define-key pcmail-folder-mode-map [menu-bar mail forward]
  '("Forward" . pcmail-forward-message))
(define-key pcmail-folder-mode-map [menu-bar mail mail]
  '("Mail" . pcmail-mail))
(define-key pcmail-folder-mode-map [menu-bar mail reply]
  '("Reply" . pcmail-answer-message))

;; Emacs V19 FOLDER menu bar
(define-key pcmail-folder-mode-map [menu-bar folders]
  (cons "Folders" (make-sparse-keymap "Folders")))
(define-key pcmail-folder-mode-map [menu-bar folders rename]
  '("Rename..." . pcmail-rename-folder))
(define-key pcmail-folder-mode-map [menu-bar folders delete]
  '("Delete..." . pcmail-delete-folder))
(define-key pcmail-folder-mode-map [menu-bar folders create]
  '("Create New..." . pcmail-create-folder))
(define-key pcmail-folder-mode-map [menu-bar folders sort]
  '("Sort by Key..." . pcmail-sort-folder))
(define-key pcmail-folder-mode-map [menu-bar folders list]
  '("Folder List" . pcmail-folder-list-folders))
(define-key pcmail-folder-mode-map [menu-bar folders unfilter]
  '("Unfilter" . pcmail-expand-subset))
(define-key pcmail-folder-mode-map [menu-bar folders filter]
  '("Filter..." . pcmail-filter-folder))
(define-key pcmail-folder-mode-map [menu-bar folders open-folder]
  '("Open Folder..." . pcmail-open-folder-menu))
(define-key pcmail-folder-mode-map [menu-bar folders summarize]
  '("Summarize" . pcmail-summarize-folder))
(define-key pcmail-folder-mode-map [menu-bar folders expunge]
  '("Expunge" . pcmail-expunge-folder))
(define-key pcmail-folder-mode-map [menu-bar folders save]
  '("Save" . pcmail-save-folder))
(define-key pcmail-folder-mode-map [menu-bar folders get-mail]
  '("Get New Mail" . pcmail-get-mail))

;; Emacs V19 Message menu bar
(define-key pcmail-folder-mode-map [menu-bar messages]
  (cons "Messages" (make-sparse-keymap "Messages")))
(define-key pcmail-folder-mode-map [menu-bar messages undigestify]
  '("Undigestify" . pcmail-undigestify-message))
(define-key pcmail-folder-mode-map [menu-bar messages previous-of-type]
  '("Previous of Type..." . pcmail-previous-message-of-type))
(define-key pcmail-folder-mode-map [menu-bar messages next-of-type]
  '("Next of Type..." . pcmail-next-message-of-type))
(define-key pcmail-folder-mode-map [menu-bar messages edit]
  '("Edit" . pcmail-edit-message))
(define-key pcmail-folder-mode-map [menu-bar messages auto-kill]
  '("Timed Delete..." . pcmail-kill-message-later))
(define-key pcmail-folder-mode-map [menu-bar messages zap]
  '("Delete Rest" . pcmail-zap-to-message))
(define-key pcmail-folder-mode-map [menu-bar messages archive]
  '("Archive to File..." . pcmail-archive-message))
(define-key pcmail-folder-mode-map [menu-bar messages print]
  '("Print..." . pcmail-print-message))
(define-key pcmail-folder-mode-map [menu-bar messages copy]
  '("Copy to Folder..." . pcmail-copy-message))
(define-key pcmail-folder-mode-map [menu-bar messages change-priority]
  '("Change Priority..." . pcmail-change-message-priority))
(define-key pcmail-folder-mode-map [menu-bar messages clear-attribute]
  '("Clear Attribute..." . pcmail-clear-message-attr-menu))
(define-key pcmail-folder-mode-map [menu-bar messages set-attribute]
  '("Set Attribute..." . pcmail-set-message-attr-menu))
(define-key pcmail-folder-mode-map [menu-bar messages undelete]
  '("Undelete" . pcmail-undelete-previous-message))
(define-key pcmail-folder-mode-map [menu-bar messages delete]
  '("Delete" . pcmail-delete-message))


;; EMACS V19 scroll bar bindings, for message movement.  So far (emacs 19.19)
;; it seems like you cannot control the size and position of the scroll bar
;; handle from lisp, so even though you can use the scroll bar to move, it
;; won't reflect the current message's position in the folder.

(define-key pcmail-folder-mode-map [vertical-scroll-bar mouse-1] 
  'pcmail-previous-message)
(define-key pcmail-folder-mode-map [vertical-scroll-bar down-mouse-2] 
  'pcmail-scroll-goto-message)
(define-key pcmail-folder-mode-map [vertical-scroll-bar mouse-2] 
  'pcmail-scroll-goto-message)
(define-key pcmail-folder-mode-map [vertical-scroll-bar mouse-3]
  'pcmail-next-message)

;;; pcmail-folder mode -- used in folders

(defun pcmail-folder-mode ()
  "Pcmail Folder Mode is used by \\[pcmail] for examining mail messages.
The following commands are available:

\\{pcmail-folder-mode-map}"
  (pcmail-mode-setup 'pcmail-folder-mode "Folder" pcmail-folder-mode-map)
  (pcmail-make-folder-local-variables)
  (setq mode-line-format
	(list "" 'pcmail-display-info " " 'global-mode-string))
  (run-hooks 'pcmail-folder-mode-hook))

(defun pcmail-make-folder-local-variables ()
  "Create and initialize per-folder local variables.
Args: none."
  (make-local-variable 'pcmail-total-messages)
  (make-local-variable 'pcmail-current-subset-message)
  (make-local-variable 'pcmail-message-vector)
  (make-local-variable 'pcmail-current-subset-vector)
  (make-local-variable 'pcmail-attr-vector)
  (make-local-variable 'pcmail-summary-vector)
  (make-local-variable 'pcmail-date-vector)
  (make-local-variable 'pcmail-priority-vector)
  (make-local-variable 'pcmail-current-filter-description)
  (make-local-variable 'pcmail-display-info)
  (make-local-variable 'pcmail-summary-buffer)
  (make-local-variable 'pcmail-folder-name)
  (make-local-variable 'pcmail-folder-format)
  (make-local-variable 'pcmail-folder-needs-expunge-p)

  (setq pcmail-total-messages nil
	pcmail-current-subset-message nil
	pcmail-message-vector nil
	pcmail-current-subset-vector nil
	pcmail-attr-vector nil
	pcmail-summary-vector nil
	pcmail-date-vector nil
	pcmail-priority-vector nil
	pcmail-current-filter-description t
	pcmail-display-info nil
	pcmail-summary-buffer nil
	pcmail-folder-name nil
	pcmail-folder-format nil
	pcmail-folder-needs-expunge-p t))

;;;; folder operations and associated utility routines

(defun pcmail-create-folder (folder-name &optional mail-drop-list 
					 folder-format)
  "Create a new folder and maybe attach a mail drop to it.
Args: (folder-name &optional mail-drop-list folder-format)
  If called as a function, supply a folder name, an optional list of
mail drop symbols, and an optional folder format; if called interactively, 
read the folder name and format from the minibuffer and read a single mail 
drop symbol if a prefix argument was supplied, turning the symbol into
a list of length 1 containing the symbol.  The folder created will have a 
folder header field containing the mail drop symbol or symbols; mail will be 
transferred from these mail drops when the \\[pcmail-get-mail] command is 
issued."
  (interactive
   (list (pcmail-read-folder "Create folder named: ")
	 (and current-prefix-arg
	      (list (intern-soft 
		     (pcmail-completing-read 
		      "Attach mail drop of type: "
		      obarray pcmail-last-mail-drop-type
		      '(lambda (s) (get s 'insert-function)) t))))
	 (intern-soft
	  (pcmail-completing-read 
	   "Folder format: "
	   obarray (prin1-to-string pcmail-default-folder-format)
	   '(lambda (s) (get s 'format-determination-fn)) t))))

  (and (pcmail-find-folder folder-name)
       (error "A folder named %s already exists." folder-name))
  (message "Creating %s..." folder-name)
  (pcmail-insert-into-folder-list folder-name 0)
  (pcmail-create-folder-file folder-name mail-drop-list 
			     (or folder-format pcmail-default-folder-format))
  (message "Creating %s...done" folder-name))

(defun pcmail-delete-folder (&optional folder-name)
  "Delete a specified folder.
Args: (folder-name)
  If called interactively with a prefix argument, read a folder name from 
the minibuffer and delete that folder, otherwise delete the current folder.
If called as a function, supply a folder name or NIL to delete the current 
folder.  Delete FOLDER-NAME, provided it is not pcmail-primary-folder-name.
Delete the folder file, remove its entry in the folder info list, remove 
its line in the folder list file, kill its message buffer, and kill its 
summary buffer.  If FOLDER-NAME has an attached mail drop, get that
mail drop's folder-delete-hook property and run the hook."
  (interactive 
   (list (and current-prefix-arg (pcmail-read-folder "Delete folder: "))))
  (or folder-name
      (setq folder-name pcmail-folder-name))
  (and (string= folder-name pcmail-primary-folder-name)
       (error "You may not delete your primary folder."))
  (or (pcmail-find-folder folder-name)
      (error "No folder named %s." folder-name))
  (or (yes-or-no-p "Deletion is permanent; are you sure? ")
      (error "Delete aborted."))
  (message "Deleting %s..." folder-name)
  (pcmail-open-folder folder-name)
  (let ((droplist (pcmail-get-mail-drop-list folder-name))
	(drop-delete))
    (and droplist
	 (while droplist
	   (and (setq drop-delete (get (car droplist) 'folder-delete-hook))
		(funcall drop-delete folder-name))
	   (setq droplist (cdr droplist)))))
  (pcmail-remove-from-folder-list folder-name)
  (pcmail-delete-folder-file folder-name)
  (message "Deleting %s...done" folder-name))

(defun pcmail-rename-folder (from to)
  "Change the name of the current folder.  
Args: (from to)
  Rename buffer, folder file, and summary buffer.  Update folder list buffer
to reflect new name.  If called interactively, request new name.  With prefix
argument, request name of folder to rename, otherwise rename current folder.
If called as a function, supply folder to be renamed, NIL for current
folder, together with its new name.  You may not rename 
pcmail-primary-folder-name"
  (interactive 
   (list (and current-prefix-arg (pcmail-read-folder "Rename folder: "))
	 (pcmail-read-folder "Rename to new name: ")))
  (or from
      (setq from pcmail-folder-name))
  (and (string= from pcmail-primary-folder-name)
       (error "You may not rename your primary folder."))
  (or (pcmail-find-folder from)
      (error "No folder named %s." from))
  (and (pcmail-find-folder to)
       (error "A folder named %s already exists." to))
  (message "Renaming %s to %s..." from to)
  (save-excursion
    (pcmail-open-folder from)
    ; make target buffer, write new file to disk, delete old, rename buffer
    (let ((tobuf to))
      (and (get-buffer tobuf)
	   (let ((count 1))
	     (while (get-buffer (setq tobuf (format "%s<%d>" to count)))
	       (setq count (1+ count)))))
      (write-file (pcmail-folder-file-name to))
      (condition-case nil
	  (delete-file (pcmail-folder-file-name from))
	(file-error nil))
      (setq pcmail-folder-name to)
      (pcmail-add-folder-entry to (pcmail-nmessages from) tobuf
				 (pcmail-mail-drop-list from))
      (pcmail-remove-folder-entry from)
      (pcmail-update-folder-mode-line pcmail-current-subset-message))
    ; generate target summary buffer and rename to it
    (and pcmail-summary-buffer
	 (buffer-name pcmail-summary-buffer)
	 (let ((tobuf (concat to "-summary"))
	       (owner-name pcmail-folder-name))
	   (and (get-buffer tobuf)
		(let ((count 1))
		  (while (get-buffer (setq tobuf (format "%s<%d>" to count)))
		    (setq count (1+ count)))))
	   (save-excursion
	     (set-buffer pcmail-summary-buffer)
	     (rename-buffer tobuf)
	     (setq pcmail-summary-owner to)
	     (pcmail-set-summary-mode-line-format owner-name))
	   (setq pcmail-summary-buffer (get-buffer tobuf))))
    ; update folder list buffer
    (pcmail-insert-into-folder-list to (pcmail-nmessages to))
    (pcmail-remove-from-folder-list from))
  (message "Renaming %s to %s...done" from to))

(defun pcmail-save-folder (&optional folder)
  "Save a folder buffer to disk.
Args: (&optional folder)
  If called interactively, a prefix argument means ask for the name of a 
folder to save, otherwise save the current folder.  If called as a function,
supply the name of the folder to save, or NIL to save the current folder.  
If pcmail-expunge-on-save is non-nil, expunge the folder before saving."
  (interactive 
   (list (and current-prefix-arg (pcmail-read-folder "Save folder: "))))
  (or folder
      (setq folder pcmail-folder-name))
  (or (pcmail-find-folder folder)
      (error "No folder named %s." folder))
  (and pcmail-expunge-on-save
       (pcmail-expunge-folder folder))
  (message "Saving %s..." folder)
  (pcmail-open-folder folder)
  (pcmail-save-buffer)
  (message "Saving %s...done" folder))

(defun pcmail-expunge-folder (&optional folder)
  "Expunge all deleted messages in a specified folder.
Args: (&optional folder)
  If called interactively, a prefix argument means ask for the name of a 
folder to expunge, otherwise expunge the current folder.  If called as 
a function, supply the name of the folder to expunge, or NIL to expunge 
the current folder."
  (interactive 
   (list (and current-prefix-arg (pcmail-read-folder "Expunge folder: "))))
  (or folder
      (setq folder pcmail-folder-name))
  (or (pcmail-find-folder folder)
      (error "No folder named %s." folder))
  (pcmail-open-folder folder)
  (if (not pcmail-folder-needs-expunge-p)
      (message "No messages need expunging")
    (message "Expunging %s..." folder)
    (let* ((current-message 1)
	   (new-messages (list (aref pcmail-message-vector 0)))
	   (new-summary (list nil))
	   (new-date (list nil))
	   (new-priority (list nil))
	   (new-subset-map (make-vector (1+ pcmail-total-messages) 0))
	   (new-attr (list nil))
	   (new-current pcmail-current-subset-message)
	   (ndeleted-messages 0)
	   (buffer-read-only nil))
      (unwind-protect
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (pcmail-expunge-loop)
	    (setq pcmail-total-messages 
		  (- pcmail-total-messages ndeleted-messages)
		  pcmail-message-vector 
		  (apply 'vector 
			 (nreverse
			  (cons (aref pcmail-message-vector current-message)
				new-messages)))
		  pcmail-attr-vector (apply 'vector (nreverse new-attr))
		  pcmail-summary-vector (apply 'vector (nreverse new-summary))
		  pcmail-date-vector (apply 'vector (nreverse new-date))
		  pcmail-priority-vector (apply
					  'vector (nreverse new-priority)))
	    (pcmail-fix-expunged-subset new-subset-map)
	    (pcmail-fix-current-message new-subset-map)
	    (pcmail-set-nmessages folder pcmail-total-messages)
	    (pcmail-change-in-folder-list folder pcmail-total-messages)
	    (setq pcmail-folder-needs-expunge-p nil))
	(or (zerop ndeleted-messages)
	    (pcmail-maybe-resummarize-folder))
	(pcmail-goto-message pcmail-current-subset-message)))
    (message "Expunging %s...done (%d message%s)" folder pcmail-total-messages
	     (pcmail-s-ending pcmail-total-messages))))

(defun pcmail-expunge-loop ()
  "Scan folder, erasing deleted messages.
Args: none
  Iterate through messages in current folder, erasing those with their
deleted attribute set.  Modify inherited variables current-message, 
ndeleted-messages, new-subset-map, new-messages, new-summary, new-date,
new-priority and new-attr.  Reset message counters on quit signal."
  (condition-case nil
      (while (<= current-message pcmail-total-messages)
	(cond ((pcmail-has-attribute-p current-message "deleted")
	       (and pcmail-wastebasket-on-expunge
		    (pcmail-wastebasket-message current-message 1))
	       (delete-region
		 (marker-position (aref pcmail-message-vector current-message))
		 (marker-position (aref pcmail-message-vector 
					(1+ current-message))))
	       (move-marker (aref pcmail-message-vector current-message) nil)
	       (setq ndeleted-messages (1+ ndeleted-messages))
	       (aset new-subset-map current-message nil))
	      (t
		(aset new-subset-map current-message
		      (- current-message ndeleted-messages))
		(setq new-messages 
		      (cons (aref pcmail-message-vector current-message) 
			    new-messages)
		      new-summary 
		      (cons (aref pcmail-summary-vector current-message) 
			    new-summary)
		      new-date
		      (cons (aref pcmail-date-vector current-message) 
			    new-date)
		      new-priority
		      (cons (aref pcmail-priority-vector current-message) 
			    new-priority)
		      new-attr 
		      (cons (aref pcmail-attr-vector current-message) 
			    new-attr))))
	(and (zerop (% (setq current-message (1+ current-message)) 
		       pcmail-progress-interval))
	     (message "Expunging %s...%d" folder current-message)))
    (quit
      (pcmail-set-message-vectors))))

(defun pcmail-fix-current-message (map)
  "Adjusts the current subset message number after expunging a folder.
Args: (map)
  MAP is a vector pcmail-total-messages long, with entries that are either
a message's post-expunge message number, or NIL if the message was expunged.
This function decrements pcmail-current-subset-message by the number of
NIL entries in slots numbered less than pcmail-current-subset-message."
  (cond ((zerop (pcmail-current-subset-length))
	 (setq pcmail-current-subset-message 0))
	(t
	 (let ((i 0) (count 0))
	   (while (<= i pcmail-current-subset-message)
	     (or (aref map i) (setq count (1+ count)))
	     (setq i (1+ i)))
	   (setq pcmail-current-subset-message
		 (max (- pcmail-current-subset-message count) 1))))))

(defun pcmail-open-folder-menu (event)
  "Open FOLDER and display its current message.
Args: (event)
  If FOLDER has an attached mail drop, read mail from the mail drop and
append it to FOLDER.  If FOLDER is already open and there us no new mail, 
don't change the current message.  If FOLDER is being opened for the first
time now, then after new mail has been read, go to either the last message
or the first unseen and interesting message, whatever is first."
  (interactive "e")
  (pcmail-get-mail (pcmail-read-folder "Open folder: ")))

(defun pcmail-get-mail (&optional folder)
  "Open FOLDER and display its current message.  
Args: (&optional folder)
  If called interactively, a prefix argument means ask for the name of a 
folder to open, otherwise open the current folder.  If called as a function,
supply the name of the folder to open, or NIL to open the current folder.  
If pcmail-new-frame-on-open is non-NIL, put it up in a new frame.  If FOLDER
has an attached mail drop, read mail from the mail drop and append it to
FOLDER.  If FOLDER is already open and there us no new mail, don't change the
current message.  If FOLDER is being opened for the first time now, then after
new mail has been read, go to either the last message or the first unseen and
interesting message, whatever is first."
  (interactive
    (list (if current-prefix-arg (pcmail-read-folder "Open folder: "))))
  (or folder
      (setq folder pcmail-folder-name))
  (or (pcmail-find-folder folder)
      (error "No folder named %s." folder))
  (let ((was-openp (pcmail-open-folder folder t))
	(newmsgs 0))
    (unwind-protect
	(let ((dl (pcmail-get-mail-drop-list folder)))
	  (and dl (setq newmsgs (pcmail-read-mail-drop folder dl))))
      (if (and was-openp (zerop newmsgs))
	  (pcmail-goto-message pcmail-current-subset-message)
	(let ((first (pcmail-next-subset-message-of-type 
		      'forward nil t
		      '(lambda (n)
			 (and (pcmail-interesting-p n)
			      (pcmail-has-attribute-p n "unseen"))))))
	  (if first
	      (pcmail-goto-message first)
	    (pcmail-last-message)))
	(pcmail-maybe-resummarize-folder)))))

(defun pcmail-load-mail-drop (mail-drop-sym)
  "Load a file with a particular mail drop format into the current folder.
Args: (mail-drop-sym)
If called interactively, read the mail drop type symbol from the minibuffer.  
Completion on mail drop symbol is permitted and defaults to last mail 
drop symbol supplied to this command."
  (interactive
   (let ((msym))
     (setq msym
	   (intern-soft 
	    (setq pcmail-last-mail-drop-type
		  (pcmail-completing-read "Load mail drop of type: "
					  obarray pcmail-last-mail-drop-type
					  '(lambda (s) 
					     (get s 
						  'header-processing-function))
					  t))))
     (list msym)))
  (let ((folder pcmail-folder-name) (nmsgs pcmail-total-messages))
    (or (pcmail-find-folder folder)
	(error "No folder named %s." folder))
    (unwind-protect
	(pcmail-read-mail-drop folder (list mail-drop-sym))
      (let ((first (pcmail-next-subset-message-of-type 
		    'forward nil nil 
		    '(lambda (n)
		       (and (pcmail-interesting-p n)
			    (pcmail-has-attribute-p n "unseen"))))))
	(if first
	    (pcmail-goto-message first)
	  (pcmail-last-message)))
      (or (= nmsgs pcmail-total-messages) ;resummarize if new msgs
	  (pcmail-maybe-resummarize-folder)))))

(defun pcmail-get-next-folder-mail ()
  "Offer to read new mail for the next folder in the folder list.
Args: none
Look for the first folder after this one with an attached mail drop and offer
to read mail from that folder.  Wrap around the folder list if necessary."
  (interactive)
  (let ((done) (next-folder-name pcmail-folder-name))
    (while (not done)
      (setq next-folder-name (pcmail-next-folder-entry next-folder-name))
      (cond ((string= next-folder-name pcmail-folder-name)
	     (error "No other folders with mail drops"))
	    ((and
	      (save-excursion
		(or (pcmail-folder-buffer-name next-folder-name)
		    (pcmail-open-folder next-folder-name))
		t)
	      (pcmail-get-mail-drop-list next-folder-name)
	      (y-or-n-p (concat "Get mail from folder " next-folder-name
				"? ")))
	     (and (y-or-n-p "Kill current folder? ") 
		  (kill-buffer (current-buffer)))
	     (pcmail-get-mail next-folder-name)
	     (setq done t))))))

;;; folder utility routines

(defun pcmail-create-folder-file (folder mail-drop-list folder-format)
  "Create a new folder file.
Args: (folder mail-drop-list folder-format)
  Create a folder file in pcmail-directory with name FOLDER and format
FOLDER-FORMAT.  Place a folder header in it.  If MAIL-DROP-LIST is non-NIL,
put the printed representation of each of its elts in the folder header's 
mail-drop list field.  Put the folder name, message count, buffer name, and 
mail-drop list in the folder info list.  Leave buffer narrowed to the folder
header."
  (save-excursion
    (find-file (pcmail-folder-file-name folder))
    (pcmail-folder-mode)
    (pcmail-folder-format-setup folder-format)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (pcmail-insert-folder-header mail-drop-list))
    (narrow-to-region (point-min) (1- (point-max)))
    (pcmail-save-buffer)
    (setq pcmail-folder-name folder)
    (pcmail-add-folder-entry folder 0 (buffer-name) mail-drop-list)))

(defun pcmail-delete-folder-file (folder)
  "Delete a folder buffer, summary (if present), and file.
Args: (folder)
  Delete FOLDER's file, and kill its corresponding folder and summary
buffers if they exist.  Remove FOLDER's name from folder info list."
  (and (file-exists-p (pcmail-folder-file-name folder))
       (condition-case nil
	   (delete-file (pcmail-folder-file-name folder))
	 (file-error nil)))
  (let ((buf (pcmail-folder-buffer-name folder)))
    (and buf
	 (get-buffer buf)
	 (save-excursion
	   (set-buffer buf)
	   (and pcmail-summary-buffer 
		(get-buffer pcmail-summary-buffer)
		(kill-buffer pcmail-summary-buffer))
	   (set-buffer-modified-p nil)
	   (kill-buffer buf))))
  (pcmail-remove-folder-entry folder))

(defun pcmail-maybe-resummarize-folder ()
  "If pcmail-resummarize-folder is non-NIL, resummarize the current folder.
Args: (none)"
  (cond ((and pcmail-resummarize-folder-on-change
	      pcmail-summary-buffer)
	 (pcmail-summarize-folder)
	 (pop-to-buffer pcmail-summary-owner))))

(defun pcmail-barf-if-empty-folder ()
  "Barf if a folder is empty.
Args: none
  Signal an error if the current folder's current subset is zero-length.
Set message counters first, if necessary."
  (or pcmail-folder-name
      (error "You are not in a Pcmail Folder buffer."))
  (pcmail-maybe-set-message-vectors)
  (cond ((zerop pcmail-total-messages)
	 (pcmail-display-subset-message 0)
	 (error "%s is empty!" pcmail-folder-name))
	((zerop (pcmail-current-subset-length))
	 (pcmail-display-subset-message 0)
	 (error "Current message subset is empty!"))))

(defun pcmail-read-folder (prompt)
  "Read a folder name form the minibuffer
Args: (prompt)
  Provide PROMPT, then read a folder name from the minibuffer, completing 
off of folder info list.  If pcmail-last-folder is non-NIL, use it as
a default.  Set pcmail-last-folder to input value.  See
pcmail-completing-read."

  (let* ((s) (rk (recent-keys)) (event (aref rk (- (length rk) 2)))
	 (folders (mapcar '(lambda (x) (list x x)) 
			  (pcmail-all-folders))))
    ;; I *strongly* suspect there is a better way to get the most recent
    ;; mouse event than getting the next-most-recent event.
    (or (pcmail-find-folder pcmail-last-folder)
	(setq pcmail-last-folder nil))
    (setq s
	  (if (listp event)
	      (or 
	       (car (x-popup-menu (if (listp event) event 
				    (cons '(0 0) (selected-frame)))
				  (list "Select Folder"
					(cons "Select Folder"
					      (sort folders
						    '(lambda (a b) 
						       (string< (car a) 
								(car b))))
					      ))))
	       (keyboard-quit))
	    (while (not (pcmail-legal-folder-name-p
			 (setq s (pcmail-completing-read
				  prompt obarray pcmail-last-folder
				  '(lambda (s) (get s 'folder-name)))))))
	    s))
    (setq pcmail-last-folder s)))

(defun pcmail-legal-folder-name-p (s)
  "Is specified string a legal Pcmail folder name?
Args: (s)
  Return t if S is a legal folder name, NIL else.  A legal folder name 
satisfies the regexp pcmail-folder-regexp, which is operating-system
dependent."
  (and (string-match pcmail-folder-regexp s)
       (= (length (substring s (match-beginning 0) (match-end 0)))
	  (length s))))

(defun pcmail-load-folder-information ()
  "Open the folder list file and construct information for each folder
Args: none
  Using the folder list file, add information for each folder.  The
information consists of folder name, buffer name, number of messages,
and mail drop list."
  (let ((mbname))
    (save-excursion
      (pcmail-open-folder-list)
      (goto-char (point-min))
      (while (re-search-forward pcmail-folder-line-regexp nil t)
	(setq mbname (buffer-substring (match-beginning 1) (match-end 1)))
	(cond ((not (pcmail-find-folder mbname))
	       (pcmail-set-folder-name mbname mbname)
	       (pcmail-set-nmessages mbname 
				     (string-to-int (buffer-substring
						     (match-beginning 2) 
						     (match-end 2))))))))
    (bury-buffer pcmail-folder-list)))

(defun pcmail-add-folder-entry (folder nmsgs buf droplist)
  "Add a folder entry; give it a name, count, buffer, and drop list.
Args: (folder nmsgs buf droplist)"
  (pcmail-set-folder-name folder folder)
  (pcmail-set-nmessages folder nmsgs)
  (pcmail-set-folder-buffer-name folder buf)
  (pcmail-set-mail-drop-list folder droplist))

(defun pcmail-remove-folder-entry (folder)
  "Remove a folder entry by setting its name to NIL.
Args: (folder)"
  (pcmail-set-folder-name folder nil))

(defun pcmail-get-mail-drop-list (folder-name)
  "Get this folder's mail-drop list.  Assume folder is current buffer.
Args: folder-name
  If folder has not yet been opened (i.e. buffer name is nil), read list 
from mail: field of folder file header, turn it into a lisp list, and return."
  (cond ((not (pcmail-folder-buffer-name folder-name))
	 (pcmail-get-folder-mail-drop-list))
	(t
	 (pcmail-mail-drop-list folder-name))))

(defun pcmail-open-folder (folder-name &optional select)
  "Find specified folder's folder file and place it in pcmail mode.
  Args: (folder-name &optional select)
Find FOLDER-NAME's folder file, making it the current buffer if SELECT is non-
NIL.  If it did not exist before finding, place it in pcmail mode.  Replace
old buffer value in folder info with current buffer name.  Set the folder's
message counters if necessary.  Load the folder's user-defined attributes into
the attribute completion obarray.  Turn folder's mail drops (as specified in
the folder's mail-drop: field) into a list and add to folder entry in info
list.  Return T if FOLDER-NAME's folder file was already open, NIL else."
  (let* ((file-name (pcmail-folder-file-name folder-name))
	 (folderbuf)
	 (existed (get-file-buffer file-name)))
    (or (pcmail-find-folder folder-name)
	(error "%s is not a Pcmail folder." folder-name))
    (if select
	(if (and (not existed) pcmail-new-frame-on-open)
	    (find-file-other-frame file-name)
	  (find-file file-name))
      (setq folderbuf (find-file-noselect file-name))
      (set-buffer folderbuf))
    (cond ((not existed)
	   (pcmail-folder-mode)
	   (pcmail-determine-folder-format)
	   (setq pcmail-folder-name folder-name)
	   (pcmail-set-mail-drop-list folder-name
				      (pcmail-get-mail-drop-list folder-name))
	   (pcmail-set-folder-buffer-name folder-name (buffer-name))
	   (pcmail-load-user-defined-attrs)))
    (pcmail-maybe-set-message-vectors)
    existed))

(defun pcmail-determine-folder-format ()
  "Determine folder format and set up folder-format-specific functions.
Args: none
  Determine the folder format by iterating through a list of valid folder 
formats, executing an attached format-determining routine, and using the 
first non-NIL return value as the format.  Once format has been determined, 
set up the generic folder-format functions by calling another format-specific 
setup routine."
  (let ((format) (tmp pcmail-valid-folder-formats))
    (while (and tmp (not format))
      (setq format
	    (funcall 
	     (or (get (car tmp) 'format-determination-fn)
		 (error "Missing format determination function for format %s"
			(car tmp)))))
      (setq tmp (cdr tmp)))
    (or format
	(error "Folder is in an unknown format."))
    (pcmail-folder-format-setup format)
    format))

(defun pcmail-folder-format-setup (folder-format)
  "Call FOLDER-FORMAT's associated setup function
Args: (folder-format)
  Set pcmail-folder-format to FOLDER-FORMAT, run any setup hooks attached to
the folder format."
  (setq pcmail-folder-format folder-format)
  (funcall (get folder-format 'folder-setup-hook)))

(defun pcmail-folder-file-name (folder-name)
  "Expand FOLDER-NAME into an absolute path, translating it as necessary
if it contains characters that are illegal file name characters."
  (expand-file-name (funcall (get 'pcmail-mail-environment
				  'folder-to-file-function) folder-name)
		    pcmail-directory))

;;; the following functions implement folder-format-specific functions

(defun pcmail-prune-header (n state)
  "Call buffer-local-variable hook.
Args: (n state)"
  (funcall (or (get pcmail-folder-format 'prune-header-fn)
	       (error "Missing prune-header function in folder format %s"
		      pcmail-folder-format))
	   n state))

(defun pcmail-header-pruned-p (n)
  "Call buffer-local-variable hook.
Args: (n)"
  (funcall (or (get pcmail-folder-format 'header-pruned-p-fn)
	       (error "Missing header-pruned-p function in folder format %s"
		      pcmail-folder-format))
	   n))

(defun pcmail-narrow-to-unpruned-header (n)
  "Call buffer-local-variable hook.
Args: (n)"
  (funcall
   (or (get pcmail-folder-format 'narrow-to-unpruned-header-fn)
       (error "Missing narrow-to-unpruned-header function in folder format %s"
	      pcmail-folder-format))
   n))

(defun pcmail-narrow-to-message (n)
  "Call buffer-local-variable hook.
Args: (n)"
  (funcall (or (get pcmail-folder-format 'narrow-to-message-fn)
	       (error "Missing narrow-to-message function in folder format %s"
		      pcmail-folder-format)) n))

(defun pcmail-insert-folder-header (drop-list)
  "Call buffer-local-variable hook.
Args: (drop-list)"
  (funcall
   (or (get pcmail-folder-format 'insert-folder-header-fn)
       (error "Missing insert-folder-header function in folder format %s"
	      pcmail-folder-format))
   drop-list))

(defun pcmail-get-folder-mail-drop-list ()
  "Call buffer-local-variable hook.
Args: none"
  (funcall
   (or (get pcmail-folder-format 'get-folder-mail-drop-list-fn)
       (error "Missing get-folder-mail-drop-list function in folder format %s"
	      pcmail-folder-format))))

(defun pcmail-scan-folder-messages (&optional start)
  "Call buffer-local-variable hook.
Args: (start)"
  (funcall
   (or (get pcmail-folder-format 'scan-folder-messages-fn)
       (error "Missing scan-folder-messages function in folder format %s"
	      pcmail-folder-format))
   start))

(defun pcmail-convert-message-to-folder-format (mail-drop &optional attrlist)
  "Call buffer-local-variable hook.
Args: (start)"
  (funcall
   (or (get pcmail-folder-format 'convert-message-to-folder-format-fn)
       (error "Missing convert-message-to-folder-format function in folder format %s"
		      pcmail-folder-format))
   mail-drop attrlist))

(defun pcmail-change-folder-message-attr (n attr state)
  "Call buffer-local-variable hook.
Args: (n attr state)"
  (funcall
   (or (get pcmail-folder-format 'change-folder-message-attr-fn)
       (error "Missing change-folder-message-attr function in folder format %s"
	      pcmail-folder-format))
   n attr state))

(defun pcmail-folder-message-has-attr-p (n attr)
  "Call buffer-local-variable hook.
Args: (n attr)"
  (funcall
   (or (get pcmail-folder-format 'folder-message-has-attr-p-fn)
       (error "Missing folder-message-has-attr-p function in folder format %s"
	      pcmail-folder-format))
   n attr))

(defun pcmail-install-user-defined-attr (attr)
  "Call buffer-local-variable hook.
Args: (attr)"
  (funcall
   (or (get pcmail-folder-format 'install-user-defined-attr-fn)
       (error "Missing install-user-defined-attr function in folder format %s"
	      pcmail-folder-format))
   attr))

(defun pcmail-user-defined-attr-list ()
  "Call buffer-local-variable hook.
Args: ()"
  (funcall
   (or (get pcmail-folder-format 'user-defined-attr-list-fn)
       (error "Missing user-defined-attr-list function in folder format %s"
	      pcmail-folder-format))))

;;; the following functions are the only ones which know about the storage 
;;; and access method for folder information.  Current method is a folder 
;;; symbol for each folder, with properties containing number of messages, 
;;; buffer name, and mail drop list
  
(defun pcmail-all-folders (&optional fun)
  "Return a list of all valid folder names.
Args: &optional fun
If FUN is present, use it as the completion filter, otherwise use a filter
that will return all folder names"
  (all-completions "" obarray (or fun '(lambda (s) (get s 'folder-name)))))

(defun pcmail-find-folder (folder-name)
  "Return non-NIL if specified folder exists, NIL else.
Args: (folder-name)
  Search folder info list for an entry associated with FOLDER-NAME,
returning the entry if it exists, NIL else."
  (and (stringp folder-name)
       (setq folder-name (intern-soft folder-name))
       (get folder-name 'folder-name)
       folder-name))

(defun pcmail-set-folder-name (folder-name name)
  "Set FOLDER-NAME's symbol's 'folder-name property to NAME
Args: (folder-name name).
  Note that FOLDER-NAME need not be a valid folder name, since the test
for validity will fail until this routine is called to insert a valid name."
  (put (intern folder-name) 'folder-name name))

(defun pcmail-set-folder-buffer-name (folder-name bname)
  "Set FOLDER-NAME's symbol's 'folder-buffer-name property to BNAME
Args: (folder-name bname)."
  (put (pcmail-find-folder folder-name) 'folder-buffer-name bname))

(defun pcmail-set-mail-drop-list (folder-name droplist)
  "Set FOLDER-NAME's symbol's 'mail-drop-list property to DROPLIST
Args: (folder-name droplist)."
  (put (pcmail-find-folder folder-name) 'mail-drop-list droplist))

(defun pcmail-set-nmessages (folder-name nmsgs)
  "Set FOLDER-NAME's symbol's 'nmessages property to NMSGS
Args: (folder-name nmsgs)."
  (put (pcmail-find-folder folder-name) 'nmessages nmsgs))

(defun pcmail-nmessages (folder-name)
  "Return the number of messages contained in the specified folder.
Args: (folder-name)"
  (and (setq folder-name (pcmail-find-folder folder-name))
       (get folder-name 'nmessages)))

(defun pcmail-mail-drop-list (folder-name)
  "Return the mail drop list attached to the specified folder.
Args: (folder-name)"
  (and (setq folder-name (pcmail-find-folder folder-name))
       (get folder-name 'mail-drop-list)))

(defun pcmail-folder-buffer-name (folder-name)
  "Return the buffer name associated with the specified folder.
Args: (folder-name)"
  (and (setq folder-name (pcmail-find-folder folder-name))
       (get folder-name 'folder-buffer-name)))

(provide 'pcmailfolder)
