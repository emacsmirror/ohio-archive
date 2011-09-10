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

;;;; miscellaneous mail reader commands

;;;; global variables

;;; system-defined

(defvar pcmail-key-alist
  '(("date" pcmail-date-less-than-p) 
    ("from" pcmail-from-field-less-than-p)
    ("priority" pcmail-priority-less-than-p)
    ("subject" pcmail-subject-field-less-than-p)
    ("to" pcmail-to-field-less-than-p))
  "Completion list of sort types.")

(defvar pcmail-edit-mode-map nil
  "Key map used to edit Pcmail messages.")

;;; defaults

(defvar pcmail-last-priority nil
  "The last priority assigned a message.")

(defvar pcmail-last-key nil
  "The last key name given a sort command.")

(if pcmail-edit-mode-map
    nil
  (setq pcmail-edit-mode-map (copy-keymap text-mode-map))
  (define-key pcmail-edit-mode-map "\C-c\C-c" 'pcmail-cease-edit)
  (define-key pcmail-edit-mode-map "\C-c\C-]" 'pcmail-abort-edit))

(define-key pcmail-edit-mode-map [menu-bar edit save]
  '("Save Edit" . pcmail-cease-edit))

(define-key pcmail-edit-mode-map [menu-bar edit abort]
  '("Abort Edit" . pcmail-abort-edit))

;;; editing

(defun pcmail-edit-mode ()
  "Pcmail Edit Mode is used when editing Pcmail messages.
Pcmail Edit mode is identical to text mode with the addition of two commands:
\\[pcmail-cease-edit], which saves an edit, and 
\\[pcmail-abort-edit], which aborts an edit."
  (put 'pcmail-edit-mode 'mode-class 'special)
  (use-local-map pcmail-edit-mode-map)
  (setq major-mode 'pcmail-edit-mode
	mode-name "Edit")
  (make-local-variable 'pcmail-old-text)
  (run-hooks 'pcmail-edit-mode-hook))

(defun pcmail-edit-message ()
  "Edit the contents of the current message.
Args: none
  Allow the body of the current message to be edited.  On save, changes
are made permanent.  On abort, old body is restored. Type
\\[pcmail-cease-edit\\] to make changes permanent, \\[pcmail-abort-edit\\] to 
abort changes."
  (interactive)
  (pcmail-barf-if-empty-folder)
  (let ((n (pcmail-make-absolute pcmail-current-subset-message)))
    (pcmail-edit-mode)
    (goto-char (point-min))
    (pcmail-prune-header n nil)		;display unpruned header
    (pcmail-narrow-to-message n)
    (setq pcmail-old-text (buffer-substring (point-min) (point-max)))
    (setq buffer-read-only nil)
    (pcmail-update-folder-mode-line pcmail-current-subset-message)
    (message (substitute-command-keys
	      (concat "Message edit: Type \\[pcmail-cease-edit] "
		      "to save changes, \\[pcmail-abort-edit] to abort")))))

(defun pcmail-cease-edit ()
  "Make changes to current message permanent.  Switch back to pcmail keymap.
Args: none"
  (interactive)
  (unwind-protect
      (cond ((and (= (length pcmail-old-text) (- (point-max) (point-min)))
		  (string= pcmail-old-text
			   (buffer-substring (point-min) (point-max))))
	     (message "Edit complete; no changes"))
	    (t
	     (let ((n (pcmail-make-absolute pcmail-current-subset-message)))
	       ;; gronk summary-line since it's probably no good now
	       (save-excursion
		 (pcmail-narrow-to-unpruned-header n)
		 (pcmail-delete-field "summary-line"))
	       (pcmail-set-attribute n "edited" t))
	     (message "Edit complete.")))

    ;; note -- cannot call pcmail-folder-mode because it gronks all local
    ;; variables.  That would be Bad.
    (use-local-map pcmail-folder-mode-map)
    (setq major-mode 'pcmail-folder-mode
	  mode-name "Folder")
    (pcmail-goto-message pcmail-current-subset-message)
    (setq buffer-read-only t)))

(defun pcmail-abort-edit ()
  "Abort edit of current message; restore original message body.
Args: none"
  (interactive)
  (delete-region (point-min) (point-max))
  (insert pcmail-old-text)
  (pcmail-cease-edit))
  
;;; sorting

(defun pcmail-read-sort-key (&optional pr)
  "Read a filter name from the minibuffer.
Args: (&optional PROMPT)
Read a filter name from the minibuffer.  Completion is permitted; input
defaults to pcmail-last-filter-name.  Signal an error if supplied filter
name is invalid."
  (let* ((s) (key-entry) (rk (recent-keys)) 
	 (event (aref rk (- (length rk) 2))))

    ;; I *strongly* suspect there is a better way to get the most recent
    ;; mouse event than getting the next-most-recent event.
    (setq s
	  (if (listp event)
	      (or 
	       (car (x-popup-menu event 
				  (list "Select Sort Key"
					(cons "Select Sort Key" 
					      (mapcar '(lambda (l)
							 (list (car l) 
							       (car l)))
						      pcmail-key-alist)))))
	       (keyboard-quit))
	    (pcmail-completing-read (or pr "Sort key: ") pcmail-key-alist
				    pcmail-last-key)))
    (setq key-entry (pcmail-search-entry-list s pcmail-key-alist))
    (or key-entry
	(error "Unknown sort key."))
    (setq pcmail-last-key s)
    key-entry))


(defun pcmail-sort-folder (key-entry)
  "Sort the current subset by one of several keys.
Args: (key-entry)
  Sort the current subset by one of several keys.  If called interactively,
specifiy a key in the minibuffer.  Completion on input is permitted; input
defaults to last key given this command."
  (interactive
   (list (pcmail-read-sort-key)))
  (let ((subset) (i 0))
    (message "Sorting %s by %s..." pcmail-folder-name (nth 0 key-entry))

    ;; convert subset vector to a list since sort works only on lists
    (while (< i (length pcmail-current-subset-vector))
      (setq subset (cons (aref pcmail-current-subset-vector i) subset))
      (setq i (1+ i)))
    (setq pcmail-current-subset-vector
	  (apply 'vector (sort (nreverse subset) (nth 1 key-entry))))
    (pcmail-maybe-resummarize-folder)
    (message "Sorting %s by %s...done" pcmail-folder-name (nth 0 key-entry))))

;;;  mail reader information

(defun pcmail-version-information ()
  "Show useful information about this incarnation of the mail reader.
Args: none"
  (interactive)
  (with-output-to-temp-buffer "*pcmail-information*"
    (princ "Mail reader version:\t") (princ pcmail-version) (terpri)
    (princ "Mail directory:\t\t") (princ pcmail-directory) (terpri)
    (princ "AutoPigeonholing:\t") 
    (princ (if pcmail-pigeonhole-hook "enabled\n" "disabled\n"))
    (princ "Ignored header fields:\t") 
    (save-excursion
      (set-buffer "*pcmail-information*")
      (let ((fill-prefix "\t\t\t") (foo (point)))
	(princ (mapconcat 'identity pcmail-uninteresting-fields-list ", ")) 
	(terpri)
	(fill-region-as-paragraph foo (point))))
    (princ "Summary format:\t\t") (princ pcmail-summary-format) (terpri)
    (princ "Date format:\t\t") (princ pcmail-date-format) (terpri)
    (princ "Folder format:\t\t") 
    (princ pcmail-folder-mode-line-format) (terpri)
    (princ "Startup filter name:\t") (princ pcmail-default-filter-name)
    (terpri)
    (and (boundp 'pcmail-last-file)
	 (princ "Default archive file:\t") (princ pcmail-last-file) (terpri))
    (princ "Default wastebasket:\t") 
    (princ pcmail-wastebasket-folder) (terpri)
    (princ "Default printer:\t") (princ pcmail-printer-name) (terpri)
    (princ "Default attribute:\t") (princ (or pcmail-last-attr "[none]"))
    (terpri)
    (princ "Default folder:\t\t") (princ (or pcmail-last-folder "[none]"))
    (terpri)
    (princ "Default filter name:\t") (princ (or pcmail-last-filter-name
						"[none]"))
    (terpri)
    (princ "Default search regexp:\t") 
    (princ (or pcmail-last-search "[none]")) (terpri)
    (princ "Default addresses:\t") 
    (princ (or pcmail-last-addresses "[none]")) (terpri) 
    (princ "Default numeric range:\t") 
    (princ (cond (pcmail-last-numeric-range 
		  (format "[%d - %d]\n" (nth 0 pcmail-last-numeric-range)
			  (nth 1 pcmail-last-numeric-range)))
		 (t "[none]\n")))
    (princ "Default date range:\t") 
    (princ (cond (pcmail-last-date-range 
		  (format "[%s - %s]\n" 
			  (pcmail-date-triple-to-string
			   (nth 0 pcmail-last-date-range))
			  (pcmail-date-triple-to-string
			   (nth 1 pcmail-last-date-range))))
		 (t "[none]\n")))
    (princ "Yanked-message prefix:\t")
    (princ (if (stringp pcmail-yank-prefix) pcmail-yank-prefix "[none]"))
    (terpri)
    (princ "Highlight forwarded:\t") 
    (princ (if pcmail-highlight-forwarded-message "yes" "no")) (terpri)
    (princ "Yank message on reply:\t") 
    (princ (if pcmail-yank-message-on-reply "yes" "no")) (terpri)
    (princ "Expunge on save:\t") 
    (princ (if pcmail-expunge-on-save "yes" "no")) (terpri)
    (princ "Wastebasket on expunge:\t") 
    (princ (if pcmail-wastebasket-on-expunge "yes" "no")) (terpri)
    (princ "Save on quit:\t\t") 
    (princ (if pcmail-save-on-quit "yes" "no")) (terpri)
    (princ "Delete on archive:\t") 
    (princ (if pcmail-delete-on-archive "yes" "no")) (terpri)
    (princ "Delete on copy:\t\t") 
    (princ (if pcmail-delete-on-copy "yes" "no")) (terpri)
    (princ "Delete on print:\t") 
    (princ (if pcmail-delete-on-print "yes" "no")) (terpri)))

;;; undigestify

(defun pcmail-undigestify-message ()
  "Separate a digest message into its constituent messages.  
Args: none
  See pcmail-undigestify-message-1. After undigestifying, move to the next 
interesting message in the folder."
  (interactive)
  (pcmail-undigestify-message-1 
   (pcmail-make-absolute pcmail-current-subset-message))
  (pcmail-next-message))

(defun pcmail-undigestify-message-1 (n)
  "Separate a digest message into its constituent messages.
Args: N
  If message absolute-numbered N is a UNIX digest, break the digest into its
constituent messages, appending the messages to the current folder.
Each undigestified message shares the digest's attribute list."
  (let* ((buffer-read-only nil)
	 (done)
	 (digest-name)
	 (start) (end)
	 (msg-string (pcmail-message-contents n)))
    (save-restriction
      (pcmail-narrow-to-unpruned-header n)
      (setq digest-name
	    (mail-strip-quoted-names 
	     (or (mail-fetch-field "to")
		 (mail-fetch-field "reply-to")
		 (error "Message is not a digest that I understand.")))))
    (widen)
    (goto-char (point-max))
    (setq start (point-marker))
    (insert msg-string)
    (setq end (point-max-marker))
    (unwind-protect
	(let ((short-dashes (make-string 27 ?-))
	      (case-fold-search t))
	  (put 'digest-mail-drop 'header-processing-function 
	       'pcmail-process-digest-message)
	  (put 'digest-mail-drop 'msg-start-regexp
	       (concat "^" short-dashes "-*\n\n*"))
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char (point-min))
	    (cond ((re-search-forward (concat "^" short-dashes "-*\n*"
					      "End of.*digest.*\n"
					      "\\**\\(\n------*\\)*"
					      "\\(\n*\^_\\)")
				      nil t)
		   (replace-match ""))
		  (t
		   (error "Message is not a digest that I understand.")))
	    (goto-char (point-min))
	    (cond ((re-search-forward (get 'digest-mail-drop 'msg-start-regexp)
				      nil t)
		   (goto-char (match-beginning 0))
		   (delete-region (point-min) (point)))
		  (t
		   (error "Message is not a digest that I understand.")))
	    (pcmail-convert-region-to-folder-format 
	     'digest-mail-drop start end)
	    (message "Message successfully undigestified"))
	  (pcmail-set-message-vectors start)
	  (pcmail-set-attribute n "undigestified" t)
	  (pcmail-set-attribute n "deleted" t)
	  (setq done t))
      (or done
	  (delete-region start end))
      (move-marker start nil)
      (move-marker end nil)
      (pcmail-narrow-to-message n))))

;;; unix digest conversion routine

(defun pcmail-process-digest-message (mail-drop)
  "Process a UNIX digest message header.  
Args: (mail-drop)
  Uses inherited variable digest-name.  See pcmail-process-unix-message
for other header processing details."
  (let ((start (point))
	(msgseparator (get mail-drop 'msg-start-regexp)))
    (and (looking-at msgseparator)
	 (replace-match ""))
    (cond ((re-search-forward msgseparator nil t)
	   (goto-char (match-beginning 0)))
	  (t
	   (goto-char (point-max))))
    (narrow-to-region start (point))
    (goto-char (point-min))
    (save-excursion
      (save-restriction
	(narrow-to-region (point)
			  (progn (search-forward pcmail-header-delim)
				 (point)))
	(cond ((not (mail-fetch-field "to"))
	       (goto-char (point-min))
	       (re-search-forward "^from:[ \t]*.*\n" nil t)
	       (insert "To: " digest-name "\n")))))
    (pcmail-convert-message-to-folder-format mail-drop)))

;;; header pruning

(defun pcmail-toggle-message-header ()
  "Show original message header of current message if pruned header is now
shown, or vice versa.
Args: none"
  (interactive)
  (pcmail-barf-if-empty-folder)
  (let ((n (pcmail-make-absolute pcmail-current-subset-message)))
    (cond ((pcmail-header-pruned-p n)
	   (pcmail-prune-header n nil))
	  (t
	    (pcmail-prune-header n t)))
    (pcmail-narrow-to-message n)))

;;; timely messages

(defun pcmail-kill-message-later (n date)
  "Arrange for something to happen to a message some time in the future.
Args: (n date)
  If called interactively, read a date of the form dd-mmm-yy from the 
minibuffer.  N is current message.  If called as a function, supply an 
absolute message number and a date string in the form dd-mmm-yy.  Set 
message N's \"timely\" attribute.  Insert an expires: field in the message 
header.  When the current date is greater than a message's expiration date, 
apply the hook pcmail-expiration-hook to the message.  With a prefix argument 
(called interactively) or a DATE value of NIL (called as a function), remove 
the expired field and clear the message's \"timely\" attribute, effectively 
unexpiring the message."
  (interactive
   (list (pcmail-make-absolute pcmail-current-subset-message)
	 (if current-prefix-arg
	     nil
	   (let ((expiration))
	     (while 
                 ; string-to-date triple validates date format
  		 (not (pcmail-string-to-date-triple
		       (setq expiration 
			     (pcmail-read-string-default 
			      "Expiration date (dd-mmm-yy): " nil t))))
	       (message "Date not dd-mmm-yy.") (ding) (sit-for 2))
	     expiration))))
  (pcmail-set-message-expiration n date)
  (pcmail-set-attribute n "timely" (and date t))
  (pcmail-update-folder-mode-line pcmail-current-subset-message))
			     
;;; priority setting

(defun pcmail-change-message-priority (priority)
  "Change the current message's priority.
Args: (priority)
  Change the current message's priority.  A priority is represented by a 
non-zero number, the lower the number the higher the priority; messages can 
be sorted by priority using the \\[pcmail-sort-folder\\] command.
Input defaults to last priority given a message."
  (interactive 
   (if current-prefix-arg
       '(nil)
     (let ((p (pcmail-read-string-default "Message priority: " 
					  pcmail-last-priority t)))
       (or (> (string-to-int p) 0)
	   (error "Priority must be a number greater than zero."))
       (list (string-to-int (setq pcmail-last-priority p))))))
  (pcmail-change-message-priority-1 priority pcmail-current-subset-message 1)
  (pcmail-update-folder-mode-line pcmail-current-subset-message))

(defun pcmail-change-priority-subset (priority)
  "Change the current message subset's priority.
Args: (priority)
  Change the current subset's priority.  A priority is represented by a 
non-zero number, the lower the number the higher the priority; messages can 
be sorted by priority using the \\[pcmail-sort-folder\\] command.
Input defaults to last priority given a message."
  (interactive 
   (if current-prefix-arg
       '(nil)
     (let ((p (pcmail-read-string-default "Message priority: " 
					  pcmail-last-priority t)))
       (or (> (string-to-int p) 0)
	   (error "Priority must be a number greater than zero."))
       (list (string-to-int (setq pcmail-last-priority p))))))
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-priority-1 priority 1 (pcmail-current-subset-length))
  (pcmail-update-folder-mode-line pcmail-current-subset-message))

(defun pcmail-change-message-priority-1 (p start len)
  "Change message priorities to P starting with START for LEN subset messages.
Args: (p start len)"
  (pcmail-barf-if-empty-folder)
  (let ((i start))
    (unwind-protect
	(while (< i (+ start len))
	  (pcmail-set-priority (pcmail-make-absolute i) p)
	  (and (zerop (% (- (setq i (1+ i)) start) pcmail-progress-interval))
	       (message "Setting priorities...%d" (- i start))))
      (pcmail-update-folder-mode-line pcmail-current-subset-message))
    (and (>= (- i start) pcmail-progress-interval)
	 (message "Setting priorities...done (%d message%s)"
		  (- i start) (pcmail-s-ending (- i start))))))

;;;; utility routines for above commands

;;; priority support

(defun pcmail-priority-less-than-p (a b)
  "Args: (a b)
Return T is message A's priority is higher (less than) B's, NIL else."
  (< (pcmail-message-priority a) (pcmail-message-priority b)))

(defun pcmail-message-priority (n)
  "Return specified message's Priority: field contents as a number.
Args: (n)
  First search the pcmail-priority-vector cache for a priority number.  If 
none is found, get message N's Priority: field and turn it into a number.  
If no priority exists, return the highest priority, 1."
  (or (aref pcmail-priority-vector n)
      (aset pcmail-priority-vector n
	    (cond ((zerop n)
		   1)
		  (t
		    (save-excursion
		      (save-restriction
			(let ((case-fold-search t))
			  (pcmail-narrow-to-unpruned-header n)
			  (let ((p (mail-fetch-field "priority")))
			    (if p (string-to-int p) 1))))))))))

(defun pcmail-set-priority (n p)
  "Set message absolute-numbered N's priority to P.  Kill priority if P is NIL.
Args: (n p)"
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header n)
      (goto-char (point-min))
      (let ((buffer-read-only nil)
	    (case-fold-search t))
	(and (re-search-forward "^priority:.*\n\\([ \t]+.*\n\\)*" nil t)
	     (replace-match ""))
	(and p
	     (insert "Priority: " (int-to-string p) "\n"))
	(aset pcmail-priority-vector n (or p 1))))))

;;; sort routines for sort by from, to, or subject fields.

(defun pcmail-from-field-less-than-p (a b)
  "Return t if message A's from field is lexicographically less than B's
Args: (a b)"
  (let ((afrom) (bfrom))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header a)
	(setq afrom (mail-strip-quoted-names
		     (or (mail-fetch-field "resent-from")
			 (mail-fetch-field "resent-sender")
			 (mail-fetch-field "from")
			 (mail-fetch-field "sender")
			 "")))))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header b)
	(setq bfrom (mail-strip-quoted-names
		     (or (mail-fetch-field "resent-from")
			 (mail-fetch-field "resent-sender")
			 (mail-fetch-field "from")
			 (mail-fetch-field "sender")
			 "")))))
    (string< (downcase afrom) (downcase bfrom))))

(defun pcmail-subject-field-less-than-p (a b)
  "Return t if message A's subject field is lexicographically less than B's
Args: (a b)"
  (let ((asubj) (bsubj))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header a)
	(setq asubj (or (mail-fetch-field "subject") ""))))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header b)
	(setq bsubj (or (mail-fetch-field "subject") ""))))
    (string< (downcase asubj) (downcase bsubj))))

(defun pcmail-to-field-less-than-p (a b)
  "Return t if message A's to field is lexicographically less than B's
Args: (a b)"
  (let ((ato) (bto))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header a)
	(setq ato (mail-strip-quoted-names
		   (or (mail-fetch-field "resent-to")
		       (mail-fetch-field "resent-apparently-to")
		       (mail-fetch-field "to")
		       (mail-fetch-field "apparently-to") ;uck
		       "")))))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header b)
	(setq bto (mail-strip-quoted-names
		   (or (mail-fetch-field "resent-to")
		       (mail-fetch-field "resent-apparently-to")
		       (mail-fetch-field "to")
		       (mail-fetch-field "apparently-to") ;uck
		       "")))))
    (string< (downcase ato) (downcase bto))))

(provide 'pcmailmisc)
