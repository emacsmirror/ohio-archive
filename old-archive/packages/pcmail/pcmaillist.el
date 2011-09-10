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


;;;; folder list commands and utilities

;;;; global variables

;;; system-defined globals

(defconst pcmail-folder-list "folders"
  "The file under pcmail-directory that contains the pcmail folder list.")

(defvar pcmail-folder-list-mode-map nil
  "Key map for pcmail-folder-list mode.")

;;;; key map and definitions

(if pcmail-folder-list-mode-map
    nil
  (suppress-keymap (setq pcmail-folder-list-mode-map (make-keymap)))
  (define-key pcmail-folder-list-mode-map "." 
    'pcmail-folder-list-beginning-of-message)
  (define-key pcmail-folder-list-mode-map "?" 'describe-mode)
  (define-key pcmail-folder-list-mode-map "c" 'pcmail-create-folder)
  (define-key pcmail-folder-list-mode-map "d" 
    'pcmail-folder-list-delete-folder)
  (define-key pcmail-folder-list-mode-map "e" 
    'pcmail-folder-list-expunge-folder)
  (define-key pcmail-folder-list-mode-map "g" 'pcmail-folder-list-get-mail)
  (define-key pcmail-folder-list-mode-map "h" 
    'pcmail-folder-list-summarize-folder)
  (define-key pcmail-folder-list-mode-map "i" 'pcmail-folder-list-get-mail)
  (define-key pcmail-folder-list-mode-map "q" 'pcmail-quit)
  (define-key pcmail-folder-list-mode-map "r" 
    'pcmail-folder-list-rename-folder)
  (define-key pcmail-folder-list-mode-map "s" 'pcmail-folder-list-save-folder)
  (define-key pcmail-folder-list-mode-map "x" 'pcmail-folder-list-exit))

(define-key pcmail-folder-list-mode-map [menu-bar folders]
  (cons "Folders" (make-sparse-keymap "Folders")))
(define-key pcmail-folder-list-mode-map [menu-bar folders rename]
  '("Rename" . pcmail-folder-list-rename-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders delete]
  '("Delete" . pcmail-folder-list-delete-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders create]
  '("Create New" . pcmail-create-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders summarize]
  '("Summarize" . pcmail-folder-list-summarize-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders expunge]
  '("Expunge" . pcmail-folder-list-expunge-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders exit]
  '("Current Folder" . pcmail-folder-list-exit))
(define-key pcmail-folder-list-mode-map [menu-bar folders save]
  '("Save" . pcmail-folder-list-save-folder))
(define-key pcmail-folder-list-mode-map [menu-bar folders get-mail]
  '("Get New Mail" . pcmail-folder-list-get-mail))

;;; pcmail-folder-list mode -- used in folder list buffer

(defun pcmail-folder-list-mode () 
  "Pcmail Folder List Mode is used by \\[pcmail] for manipulating Pcmail
folders.  The following commands are available:
\\{pcmail-folder-list-mode-map}"
  (interactive)
  (pcmail-mode-setup 'pcmail-folder-list-mode "Folder List" 
		     pcmail-folder-list-mode-map)
  (let ((fill-pre (cond (mode-line-inverse-video "") (t "-----")))
	(fill-post (cond (mode-line-inverse-video " ") (t "%-"))))
    (setq mode-line-format (list fill-pre "Folder List            "
				 'global-mode-string fill-post)))
  (run-hooks 'pcmail-folder-list-mode-hook))

;;;; folder-list mode commands

(defun pcmail-folder-list-folders ()
  "Open and display the folder list file in the other window.
Args: none"
  (interactive)
  (let ((b))
    (save-excursion
      (pcmail-open-folder-list)
      (setq b (current-buffer)))
    (pop-to-buffer b)
    (goto-char (point-min))))

(defun pcmail-folder-list-exit ()
  "Exit the folder list, returning to the current folder.
Args: none"
  (interactive)
  (pop-to-buffer (or (and (pcmail-folder-buffer-name (pcmail-folder-at-point))
			  (get-buffer (pcmail-folder-buffer-name 
				       (pcmail-folder-at-point))))
		     (pcmail-folder-buffer-name pcmail-primary-folder-name)))
  (delete-other-windows))

(defun pcmail-folder-list-beginning-of-message ()
  "Display the current message in the folder next to the cursor.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-open-folder mb t)
    (pcmail-beginning-of-message)))

(defun pcmail-folder-list-rename-folder ()
  "Change the name of the next to the cursor.  See pcmail-rename-folder-1.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-rename-folder mb (pcmail-read-folder "Rename to new name: "))))

(defun pcmail-folder-list-delete-folder ()
  "Delete the folder next to the cursor.  See pcmail-delete-folder.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-delete-folder mb)))

(defun pcmail-folder-list-save-folder ()
  "Save the folder next to the cursor.  See pcmail-save-folder.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-save-folder mb)))

(defun pcmail-folder-list-summarize-folder ()
  "Summarize the folder next to the cursor.  See pcmail-summarize-folder.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-summarize-folder mb)))

(defun pcmail-folder-list-expunge-folder ()
  "Expunge the folder next to the cursor.  See pcmail-expunge-1.
Args: none"
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-expunge-folder mb)))
    
(defun pcmail-folder-list-get-mail ()
  "Open the folder next to the cursor and transfer any new mail into it.
Args: none
  Open the current folder.  If the folder has an attached mail drop list,
transfer mail from the mail drops into the folder.  See pcmail-get-mail."
  (interactive)
  (let ((mb (or (pcmail-folder-at-point) (error "No current folder."))))
    (other-window 1)
    (pcmail-get-mail mb)))

;;; folder list utility routines

(defun pcmail-create-folder-list-file ()
  "Create a folder list file in the mail directory
Args: none"
  (pcmail-open-folder-list)
  (set-buffer-modified-p t)
  (pcmail-save-buffer))
  
(defun pcmail-open-folder-list ()
  "Find and display the folder list file in pcmail-folder-list mode.
Args: none"
  (let ((existed (get-buffer pcmail-folder-list))
	(listbuf))
    (setq listbuf
	  (find-file-noselect (expand-file-name pcmail-folder-list 
						pcmail-directory)))
    (set-buffer listbuf)
    (or existed
	(pcmail-folder-list-mode))))

(defun pcmail-folder-at-point ()
  "Return name of folder where cursor is in folder list buffer.
Args: none
  Jump to folder list buffer pcmail-folder-list and return the name of the 
folder on the buffer's current line.  Return NIL if the buffer is empty, 
improperly formatted, or if no folder exists on the current line."
  (and (get-buffer pcmail-folder-list)
       (save-excursion
	 (set-buffer pcmail-folder-list)
	 (save-excursion
	   (end-of-line)
	   (and (re-search-forward pcmail-folder-line-regexp 
				   (prog1 (point) (beginning-of-line)) t)
		(buffer-substring (match-beginning 1) (match-end 1)))))))

(defun pcmail-next-folder-entry (folder-name)
  "Return the name of the folder following FOLDER-NAME in the folder list.
Args: (folder-list)"
  (let ((nextname))
    (and (pcmail-find-folder folder-name)
	 (save-excursion
	   (pcmail-open-folder-list)
	   (goto-char (point-min))
	   (re-search-forward (concat "^folder " folder-name ":.*\n") nil t)
	   (and (eq (point) (point-max))	;wrap if at end of buffer
		(goto-char (point-min)))
	   (setq nextname (pcmail-folder-at-point))))
    (bury-buffer pcmail-folder-list)
    nextname))
    
(defun pcmail-change-in-folder-list (folder-name nmessages)
  "Update a specified folder's entry in the folder list buffer.
Args: (folder-name nmessages)"
  (save-excursion
    (pcmail-open-folder-list)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (cond ((re-search-forward (format "Folder %s:.*\n" folder-name) nil t)
	     (replace-match (format "Folder %s: %d message%s\n" folder-name
				    nmessages (pcmail-s-ending nmessages))
			    t t)
	     (goto-char (match-beginning 0)))))
    (pcmail-save-buffer pcmail-folder-list)
    (bury-buffer pcmail-folder-list)))

(defun pcmail-insert-into-folder-list (folder-name nmessages)
  "Add a new folder line to the folder list buffer.
Args: (folder-name nmessages)
  Open the folder list file, go to the end of the buffer, and append an
entry for FOLDER-NAME with a message count of NMESSAGES.  Save and bury
the list buffer after insertion."
  (save-excursion
    (pcmail-open-folder-list)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (or (= (buffer-size) 0)		;add newline if already text in the
	  (eq (preceding-char) ?\n)	;buffer with no trailing newline
	  (insert ?\n))
      (insert "Folder " folder-name ": " (int-to-string nmessages)
	      " message" (pcmail-s-ending nmessages) "\n")))
  (bury-buffer pcmail-folder-list))

(defun pcmail-remove-from-folder-list (folder-name)
  "Remove a specified folder's entry from the folder list buffer.
Args: (folder-name)"
  (save-excursion
    (pcmail-open-folder-list)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (cond ((re-search-forward (format "Folder %s:" folder-name) nil t)
	     (beginning-of-line)
	     (delete-region (point) (progn (forward-line 1) (point)))))))
  (bury-buffer pcmail-folder-list))

(provide 'pcmaillist)
