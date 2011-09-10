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

;;; system-defined globals

(defvar pcmail-summary-mode-map nil
  "Keymap for pcmail summary mode.")

;;;; mode definitions 

(if pcmail-summary-mode-map
    nil
  (suppress-keymap (setq pcmail-summary-mode-map (make-keymap)))
  (define-key pcmail-summary-mode-map "?" 'describe-mode)
  (define-key pcmail-summary-mode-map ">" 'pcmail-summary-last-message)
  (define-key pcmail-summary-mode-map "<" 
    '(lambda () (interactive) (pcmail-summary-goto-message 1)))
  (define-key pcmail-summary-mode-map "." 'pcmail-summary-beginning-of-message)
  (define-key pcmail-summary-mode-map " " 'pcmail-summary-scroll-message-up)
  (define-key pcmail-summary-mode-map "a" 'pcmail-summary-archive-message)
  (define-key pcmail-summary-mode-map "b" 'pcmail-summary-sort-folder)
  (define-key pcmail-summary-mode-map "c" 'pcmail-summary-copy-message)
  (define-key pcmail-summary-mode-map "d" 'pcmail-summary-delete-message)
  (define-key pcmail-summary-mode-map "e" 'pcmail-summary-expunge-folder)
  (define-key pcmail-summary-mode-map "f" 'pcmail-summary-forward-message)
  (define-key pcmail-summary-mode-map "g" 'pcmail-summary-get-mail)
  (define-key pcmail-summary-mode-map "i" 
    'pcmail-summary-change-message-priority)
  (define-key pcmail-summary-mode-map "j" 'pcmail-summary-goto-message)
  (define-key pcmail-summary-mode-map "k" 'pcmail-summary-kill-message-later)
  (define-key pcmail-summary-mode-map "m" 'pcmail-summary-mail)
  (define-key pcmail-summary-mode-map "n" 'pcmail-summary-next-message)
  (define-key pcmail-summary-mode-map "o" 'pcmail-summary-print-message)
  (define-key pcmail-summary-mode-map "p" 'pcmail-summary-previous-message)
  (define-key pcmail-summary-mode-map "q" 'pcmail-summary-quit)
  (define-key pcmail-summary-mode-map "r" 'pcmail-summary-answer-message)
  (define-key pcmail-summary-mode-map "s" 'pcmail-summary-save-folder)
  (define-key pcmail-summary-mode-map "u" 'pcmail-summary-undelete-message)
  (define-key pcmail-summary-mode-map "w" 'pcmail-summary-edit-message)
  (define-key pcmail-summary-mode-map "x" 'pcmail-summary-expand-subset)
  (define-key pcmail-summary-mode-map "y" 'pcmail-summary-change-message-attr)
  (define-key pcmail-summary-mode-map "z" 'pcmail-summary-zap-to-message)
  (define-key pcmail-summary-mode-map "\em" 'pcmail-folder-list-folders)
  (define-key pcmail-summary-mode-map "\C-d" 
    'pcmail-summary-delete-message-backward)
  (define-key pcmail-summary-mode-map "\C-m" 'pcmail-summary-next-message)
  (define-key pcmail-summary-mode-map "\en" 
    'pcmail-summary-next-message-of-type)
  (define-key pcmail-summary-mode-map "\ep" 
    'pcmail-summary-previous-message-of-type)
  (define-key pcmail-summary-mode-map "\e\C-a" 'pcmail-summary-archive-subset)
  (define-key pcmail-summary-mode-map "\e\C-c" 'pcmail-summary-copy-subset)
  (define-key pcmail-summary-mode-map "\e\C-d" 'pcmail-summary-delete-subset)
  (define-key pcmail-summary-mode-map "\e\C-f" 'pcmail-summary-filter-folder)
  (define-key pcmail-summary-mode-map "\e\C-i" 
    'pcmail-summary-change-priority-subset)
  (define-key pcmail-summary-mode-map "\e\C-o" 'pcmail-summary-print-subset)
  (define-key pcmail-summary-mode-map "\e\C-u" 'pcmail-summary-undelete-subset)
  (define-key pcmail-summary-mode-map "\e\C-y" 
    'pcmail-summary-change-attr-subset)
  (define-key pcmail-summary-mode-map "\177" 
    'pcmail-summary-scroll-message-down))

;; Emacs V19 SUBSET menu bar
(define-key pcmail-summary-mode-map [menu-bar subsets]
  (cons "Subsets" (make-sparse-keymap "Subsets")))
(define-key pcmail-summary-mode-map [menu-bar subsets priority]
  '("Change Priority..." . pcmail-summary-change-priority-subset))
(define-key pcmail-summary-mode-map [menu-bar subsets clear-attribute]
  '("Clear Attribute..." . pcmail-summary-clear-attr-subset-menu))
(define-key pcmail-summary-mode-map [menu-bar subsets set-attribute]
  '("Set Attribute..." . pcmail-summary-set-attr-subset-menu))
(define-key pcmail-summary-mode-map [menu-bar subsets undelete]
  '("Undelete" . pcmail-summary-undelete-subset))
(define-key pcmail-summary-mode-map [menu-bar subsets delete]
  '("Delete" . pcmail-summary-delete-subset))
(define-key pcmail-summary-mode-map [menu-bar subsets archive]
  '("Archive to File..." . pcmail-summary-archive-subset))
(define-key pcmail-summary-mode-map [menu-bar subsets print]
  '("Print..." . pcmail-summary-print-subset))
(define-key pcmail-summary-mode-map [menu-bar subsets copy]
  '("Copy to Folder..." . pcmail-summary-copy-subset))

;; Emacs V19 MAIL menu bar
(define-key pcmail-summary-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))
(define-key pcmail-summary-mode-map [menu-bar mail mail-subset]
  '("Mail Subset" . pcmail-summary-mail-subset))
(define-key pcmail-summary-mode-map [menu-bar mail mail]
  '("Mail" . pcmail-summary-mail))
(define-key pcmail-summary-mode-map [menu-bar mail forward]
  '("Forward" . pcmail-summary-forward-message))
(define-key pcmail-summary-mode-map [menu-bar mail reply]
  '("Reply" . pcmail-summary-answer-message))


;; Emacs V19 FOLDER menu bar
(define-key pcmail-summary-mode-map [menu-bar folders]
  (cons "Folders" (make-sparse-keymap "Folders")))
(define-key pcmail-summary-mode-map [menu-bar folders sort]
  '("Sort by Key..." . pcmail-summary-sort-folder))
(define-key pcmail-summary-mode-map [menu-bar folders list]
  '("Folder List" . pcmail-folder-list-folders))
(define-key pcmail-summary-mode-map [menu-bar folders unfilter]
  '("Unfilter" . pcmail-summary-expand-subset))
(define-key pcmail-summary-mode-map [menu-bar folders filter]
  '("Filter..." . pcmail-summary-filter-folder))
(define-key pcmail-summary-mode-map [menu-bar folders open-folder]
  '("Open Folder..." . pcmail-summary-open-folder-menu))
(define-key pcmail-summary-mode-map [menu-bar folders expunge]
  '("Expunge" . pcmail-summary-expunge-folder))
(define-key pcmail-summary-mode-map [menu-bar folders save]
  '("Save" . pcmail-summary-save-folder))
(define-key pcmail-summary-mode-map [menu-bar folders get-mail]
  '("Get New Mail" . pcmail-summary-get-mail))

;; Emacs V19 Message menu bar
(define-key pcmail-summary-mode-map [menu-bar messages]
  (cons "Messages" (make-sparse-keymap "Messages")))
(define-key pcmail-summary-mode-map [menu-bar messages previous-type]
  '("Previous of Type..." . pcmail-summary-previous-message-of-type))
(define-key pcmail-summary-mode-map [menu-bar messages next-type]
  '("Next of Type..." . pcmail-summary-next-message-of-type))
(define-key pcmail-summary-mode-map [menu-bar messages edit]
  '("Edit" . pcmail-summary-edit-message))
(define-key pcmail-summary-mode-map [menu-bar messages auto-kill]
  '("Timed Delete..." . pcmail-summary-kill-message-later))
(define-key pcmail-summary-mode-map [menu-bar messages zap]
  '("Delete Rest" . pcmail-summary-zap-to-message))
(define-key pcmail-summary-mode-map [menu-bar messages archive]
  '("Archive to File..." . pcmail-summary-archive-message))
(define-key pcmail-summary-mode-map [menu-bar messages print]
  '("Print..." . pcmail-summary-print-message))
(define-key pcmail-summary-mode-map [menu-bar messages copy]
  '("Copy to Folder..." . pcmail-summary-copy-message))
(define-key pcmail-summary-mode-map [menu-bar messages change-priority]
  '("Change Priority..." . pcmail-summary-change-message-priority))
(define-key pcmail-summary-mode-map [menu-bar messages clear-attribute]
  '("Clear Attribute..." . pcmail-summary-clear-message-attr-menu))
(define-key pcmail-summary-mode-map [menu-bar messages set-attribute]
  '("Set Attribute..." . pcmail-summary-set-message-attr-menu))
(define-key pcmail-summary-mode-map [menu-bar messages undelete]
  '("Undelete" . pcmail-summary-undelete-message))
(define-key pcmail-summary-mode-map [menu-bar messages delete]
  '("Delete" . pcmail-summary-delete-message))


;; EMACS V19 scroll bar bindings, for message movement.  So far (emacs 19.19)
;; it seems like you cannot control the size and position of the scroll bar
;; handle from lisp, so even though you can use the scroll bar to move, it
;; won't reflect the current message's position in the folder.

(define-key pcmail-summary-mode-map [vertical-scroll-bar mouse-1] 
  'pcmail-summary-previous-message)
(define-key pcmail-summary-mode-map [vertical-scroll-bar down-mouse-2] 
  'pcmail-summary-scroll-goto-message)
(define-key pcmail-summary-mode-map [vertical-scroll-bar mouse-2] 
  'pcmail-summary-scroll-goto-message)
(define-key pcmail-summary-mode-map [vertical-scroll-bar mouse-3]
  'pcmail-summary-next-message)

(defun pcmail-summary-mode (owner-name)
  "Pcmail Summary Mode is used by the summarization commands to manipulate
messages in a summary window.  A subset of the commands available in 
Folder Mode are supported in this mode. As commands are issued in the 
summary buffer the corresponding mail message (if any) is manipulated
and displayed in the owning folder buffer.

\\{pcmail-summary-mode-map}

Entering this mode causes hook variable pcmail-summary-mode-hook to 
be evaluated."
  (pcmail-mode-setup 'pcmail-summary-mode "Summary" pcmail-summary-mode-map)
  (put 'pcmail-summary-mode 'mode-class 'special)
  (make-local-variable 'pcmail-summary-owner)
  (setq pcmail-summary-owner "[unknown]")
  (make-local-variable 'pcmail-summary-size)
  (setq pcmail-summary-size 0)
  (setq truncate-lines t)
  (pcmail-set-summary-mode-line-format owner-name)
  (run-hooks 'pcmail-summary-mode-hook))

(defun pcmail-set-summary-mode-line-format (owner-name)
  "Set summary buffer's mode line format."
  (let ((fill-pre (cond (mode-line-inverse-video "") (t "-----")))
	(fill-post (cond (mode-line-inverse-video " ") (t "%-"))))
    (setq mode-line-format (list fill-pre "Summary: " 
				 owner-name
				 (make-string 
				  (max 0 (- 18 (length owner-name))) ? )
				 'global-mode-string fill-post))))

;;; routines to create a message summary

;; create a summary and place it in the summary buffer

(defun pcmail-summarize-folder (&optional folder-name)
  "Summarize the messages in a folder.  
Args: none
  If called interactively, a prefix argument means ask for the name of a 
folder to summarize, otherwise summarize the current folder.  If called 
as a function, supply the name of the folder to summarize, or NIL to 
summarize the current folder."
  (interactive 
   (list (and current-prefix-arg (pcmail-read-folder "Summarize folder: "))))
  (or folder-name
      (setq folder-name pcmail-folder-name))
  (or (pcmail-find-folder folder-name)
      (error "No folder named %s." folder-name))
  (pcmail-open-folder folder-name t)
  (pcmail-barf-if-empty-folder)
  (message "Summarizing %s..." folder-name)
  (or (and pcmail-summary-buffer
	   (buffer-name pcmail-summary-buffer))
      (setq pcmail-summary-buffer
	    (generate-new-buffer (format "%s-summary" folder-name))))
  (let ((i 1)
	(owner-buffer-name (pcmail-folder-buffer-name folder-name))
	(owner-folder-name pcmail-folder-name)
	(cmsg pcmail-current-subset-message)
	(lines)
	(curr-height)
	(nlines (pcmail-current-subset-length)))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (widen)
	    (while (<= i (pcmail-current-subset-length))
	      (setq lines 
		    (cons (concat (pcmail-get-summary-line i) "\n") lines))
	      (and (zerop (% (setq i (1+ i)) pcmail-progress-interval))
		   (message "Summarizing %s...%d" folder-name i)))))
      (pcmail-goto-message cmsg))
    (setq lines (nreverse lines))
    (setq curr-height (pcmail-summary-record-height))
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-size-window curr-height)
    (pcmail-summary-mode owner-folder-name)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapcar 'insert lines))
    (goto-char (point-min))
    (setq pcmail-summary-owner owner-buffer-name
	  pcmail-summary-size nlines)
    (pcmail-summary-goto-message cmsg)
    (message "Summarizing %s...done (%d message%s)" folder-name
	     nlines (pcmail-s-ending nlines))))

(defun pcmail-get-summary-line (n)
  "Return the Nth subset message's summary information.
Args: (N)
  Get message with relative number N's summary line and add to it volatile
information like message number and D (deleted) or U (unseen) notes.  
Assumes current buffer is folder buffer that owns this summary."
  (let* ((abs (pcmail-make-absolute n))
	 (s (or (aref pcmail-summary-vector abs)
		(aset pcmail-summary-vector abs
		      (pcmail-get-summary-line-1 abs)))))
    (format "%4d%s %s" 
	    n
	    (cond ((pcmail-has-attribute-p abs "deleted") "D")
		  ((pcmail-has-attribute-p abs "unseen") "U")
		  (t " "))
	    s)))

(defun pcmail-get-summary-line-1 (n)
  "Get message absolute-numbered N's summary information.
Args: (n)
  Get summary information from message absolute-numbered N's summary-line 
field.  If the field does not exist, create it.  Assume that the current 
buffer is the folder that owns this summary.  Assume current buffer has been 
widened."
  (let ((line))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header n)
	(cond ((not (setq line (mail-fetch-field "summary-line" nil)))
	       (setq line (pcmail-create-summary-line-field n))
	       (goto-char (point-min))
	       (let ((buffer-read-only nil))
		 (insert "Summary-line: " line "\n"))))))
    line))

(defun pcmail-create-summary-line-field (n)
  "Create a summary-line field using the header of message absolute-numbered N.
Args: (n)
  Use the format string pcmail-summary-format to format the summary line.
The format string can request to, from, subject, cc, bcc, date, or message
ID fields in any combination."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header n)
      (pcmail-format-string 
       pcmail-summary-format
       (list (list "b" '(lambda ()
			  (pcmail-summary-make-field "bcc")))
	     (list "c" '(lambda ()
			  (pcmail-summary-make-field "cc")))
	     (list "C" '(lambda (n)
			  (pcmail-message-char-count n))
		   n)
	     (list "d" '(lambda (n) 
			  (let ((arg))
			    (cond ((setq arg (pcmail-message-date n))
				   (setq arg 
					 (pcmail-date-triple-to-string arg)))
				  (t
				   (setq arg "[unknown]")))
			    arg)) 
		   n)
	     (list "f" '(lambda ()
			  (pcmail-summary-make-from)))
	     (list "l" '(lambda (n)
			  (pcmail-message-line-count n))
		   n)
	     (list "m" '(lambda ()
			  (pcmail-summary-make-field "message-id")))
	     (list "s" '(lambda ()
			  (pcmail-summary-make-field "subject")))
	     (list "t" '(lambda ()
			  (or (pcmail-summary-make-field "to")
			      (pcmail-summary-make-field "apparently-to")
			      "") 
			  )))))))

(defun pcmail-summary-concat (s field len neg)
  "Concatenate S and the first LEN bytes of FIELD.
Args: (s field len neg)
  If LEN is zero use all of FIELD.  If LEN is greater than the length of 
FIELD, blank-pad on the right, unless NEG is non-nil, in which case blank
pad on the left"
  (let ((blanks (make-string len ? )))
    (concat s 
	    (if (zerop len) 
		field			;all of the field
	      (if (not neg)		;left-justify
		  (concat (substring field 0 (min len (length field)))
			  (substring blanks 0 
				     (max 0 (- len (length field)))))
		(concat (substring blanks 0 (max 0 (- len (length field))))
			(substring field 0 (min len (length field)))))))))
  
(defun pcmail-summary-make-from ()
  "Return current message's from: summary information .
Args: none
  Create current message's From: summary information.  Assume buffer is
narrowed to desired message's unpruned header.  If the From: field is the 
same as pcmail-primary-folder-name, return the To: field.  Otherwise return 
either the Reply-to: field or the From: field in that order."
  (let ((from) (to))
    (setq from (mail-strip-quoted-names 
		(or (mail-fetch-field "reply-to")
		    (mail-fetch-field "from")
		    " ")))
    (and (string= (or (and (string-match "\\([^%@]+\\)\\([%@].*\\)?" from)
			   (substring from (match-beginning 1) (match-end 1)))
		      from)
		  pcmail-primary-folder-name)
	 (not (zerop (length (setq to (mail-strip-quoted-names 
				       (or (mail-fetch-field "to" t)
					   (mail-fetch-field "apparently-to" t)
					   ""))))))
	 (setq from (concat "To: " to)))
    (and (> (length from) 25)
	 (setq from (substring from 0 25)))
    from))

(defun pcmail-summary-make-field (field)
  "Create a summary-line field given field name FIELD.  
Args: (field)
  Assume buffer narrowed to msg header.  If the field spans multiple lines,
truncate it before the first newline."
  (let ((s (mail-fetch-field field)))
    (cond (s
	   (and (string-match "^[ \t\n]*" s)
		(setq s (substring s (match-end 0))))
	   (and (string-match "\n" s)
		(setq s (substring s 0 (match-beginning 0))))
	   s)
	  (t
	   (format "[no %s field]" field)))))

(defun pcmail-summary-record-height ()
  (window-height (selected-window)))

(defun pcmail-summary-size-window (mail-w-height)
  "Grow or shrink the current window based on pcmail-summary-window-percentage.
Args: (mail-w-height)
  MAIL-W-HEIGHT is the height of the mail window before splitting on entry
into summary.  Find desired height by using recorded height and 
pcmail-summary-window-percentage, then grow or shrink window to that size"
  (and (or (> pcmail-summary-window-percentage 100)
	   (< pcmail-summary-window-percentage 10))
       (setq pcmail-summary-window-percentage 50))
  (let ((desired (/ (* mail-w-height pcmail-summary-window-percentage)
		    100)))
    (and (> desired window-min-height)
	 (cond ((> desired (window-height))
		(enlarge-window (- desired (window-height))))
	       (t
		(shrink-window (- (window-height) desired)))))))

(defun pcmail-summary-pop-to-owner ()
  "Resummarize current folder if current summary is out of date.
Args: (none)"
  (or (get-buffer pcmail-summary-owner)
      (error "Owning folder has disappeared!"))
  (let ((cbuf (current-buffer)))
    (pop-to-buffer pcmail-summary-owner)
    (if pcmail-summary-buffer
	nil
      (pcmail-summarize-folder)
      (kill-buffer cbuf)				     ;kill old summary
      (pop-to-buffer pcmail-summary-owner))))

;;;; summary commands

;;; movement commands: as in folder mode 

(defun pcmail-summary-last-message ()
  "Move to the last interesting message in the summary and display it.
Args: none
  Call pcmail-last-message interactively.  See pcmail-last-message."
  (interactive)
  (let ((curr))
    (pcmail-summary-pop-to-owner)
    (call-interactively 'pcmail-last-message)
    (setq curr pcmail-current-subset-message)
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-next-message ()
  "Move to the next interesting message in the summary and display it.
Args: none
  Call pcmail-next-message interactively.  See pcmail-next-message."
  (interactive)
  (let ((curr))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (call-interactively 'pcmail-next-message)
    (setq curr pcmail-current-subset-message)
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-next-message-of-type ()
  "Move to the next message in the current subset that satisfies a predicate.
Args: none
  Call pcmail-next-message-of-type interactively.  See 
pcmail-next-message-of-type."
  (interactive)
  (let ((curr))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (unwind-protect
	(call-interactively 'pcmail-next-message-of-type)
      (setq curr pcmail-current-subset-message)
      (pop-to-buffer pcmail-summary-buffer)
      (pcmail-summary-goto-message curr))))

(defun pcmail-summary-previous-message ()
  "Move to the previous interesting message in the summary and display it.
Args: none
  Call pcmail-previous-message interactively.  See pcmail-previous-message."
  (interactive)
  (let ((curr))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (call-interactively 'pcmail-previous-message)
    (setq curr pcmail-current-subset-message)
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-previous-message-of-type ()
  "Move to the previous message in the summary that satisfies a predicate.
Args: none
  Call pcmail-previous-message-of-type interactively.  See 
pcmail-previous-message-of-type."
  (interactive)
  (let ((curr))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (unwind-protect
	(call-interactively 'pcmail-previous-message-of-type)
      (setq curr pcmail-current-subset-message)
      (pop-to-buffer pcmail-summary-buffer)
      (pcmail-summary-goto-message curr))))

(defun pcmail-summary-beginning-of-message ()
  "Move to the beginning of the current message.
Args: none"
  (interactive)
  (pcmail-summary-goto-message)
  (pcmail-summary-pop-to-owner)
  (pcmail-beginning-of-message)
  (pop-to-buffer pcmail-summary-buffer))

(defun pcmail-summary-scroll-goto-message (event)
  "Jump to a message based on the cursor's position in the scroll bar.
Args: (event)
Accept EVENT and move to the appropriate message in the folder."
  (interactive "e")
  (let ((window) (portion-whole) (position) 
	(modifier (car-safe (event-modifiers (car-safe event)))))
    (cond ((eq modifier 'click)
	   (setq position (event-start event)
		 window (posn-window position)
		 portion-whole (posn-col-row position))
	   (pcmail-summary-scroll-goto-message-1 window portion-whole)))))

(defun pcmail-summary-scroll-goto-message-1 (window portion-whole)
  (let ((n) (total) (bufmode))
    (set-buffer (window-buffer window))
    (if (eq major-mode 'pcmail-summary-mode)
	(progn
	  (pcmail-summary-pop-to-owner)
	  (setq total (pcmail-current-subset-length))
	  (pop-to-buffer pcmail-summary-buffer)
	  (setq n (scroll-bar-scale portion-whole total))
	  (pcmail-summary-goto-message n)
	  (beginning-of-line)))))

(defun pcmail-summary-goto-message (&optional n)
  "Move to message number N of the summary and display it.
Args: (&optional n)
Display message N in the summary.  If called interactively, N is specified 
by a numeric prefix argument.  If not specified, N defaults to the first 
message in the subset."
  (interactive "P")

  ;; following skullduggery is here in case the user gets silly and either
  ;; (1) kills the folder which owns this summary, or (2) kills and
  ;; then re-gets the folder owning this summary.  A folder with no summary
  ;; will have a pcmail-summary-buffer local variable value of NIL.  If
  ;; we think we are owned by a folder, and its summary-buffer is NIL, its
  ;; time to resummarize and kill ourselves (die bravely for Amber...)

  (or pcmail-summary-owner
      (error "You are not in a Pcmail Summary buffer."))
  (or (get-buffer pcmail-summary-owner)
      (error "Owning folder has disappeared!"))
  (save-excursion
    (let ((cbuf (current-buffer)))
      (set-buffer pcmail-summary-owner)
      (if pcmail-summary-buffer
	  nil
	(pcmail-summarize-folder)
	(kill-buffer cbuf))))
  (and (eobp)
       (forward-line -1))
  (beginning-of-line)
  (let ((msg) (deleted-p) (unseen-p))
    (if n
	(setq n (prefix-numeric-value n))
      (setq n (string-to-int (buffer-substring (point) (+ 5 (point))))))
    (cond ((< n 1)
	   (setq msg "Beginning of summary"
		 n 1))
	  ((> n pcmail-summary-size)
	   (setq msg "End of summary"
		 n pcmail-summary-size)))
    (goto-char (point-min))
    (forward-line (1- n))
    (save-excursion
      (set-buffer pcmail-summary-owner)
      (setq deleted-p (pcmail-has-attribute-p (pcmail-make-absolute n)
					      "deleted")
	    unseen-p (pcmail-has-attribute-p (pcmail-make-absolute n)
					     "unseen")))
    (cond ((/= n 
	       (save-excursion
		 (set-buffer pcmail-summary-owner)
		 pcmail-current-subset-message))
	   (pop-to-buffer pcmail-summary-owner)
	   (pcmail-goto-message n)
	   (pop-to-buffer pcmail-summary-buffer)))
    (pcmail-summary-set-attr ?U unseen-p)
    (pcmail-summary-set-attr ?D deleted-p)
    (and msg
	 (message msg))))

(defun pcmail-summary-set-attr (attr state)
  "In the summary buffer, set the current message's attribute ATTR to STATE.
Args: (attr state)"
  (let ((buffer-read-only nil))
    (save-excursion
      (skip-chars-forward " ")
      (skip-chars-forward "0-9")
      (cond ((and (= (following-char) attr)
		  (not state))
	     (delete-char 1)
	     (insert " "))
	    ((and (= (following-char) ? )
		  state)
	     (delete-char 1)
	     (insert attr))))))

(defun pcmail-summary-scroll-message-up (&optional n)
  "Scroll the current message forward in the other window.
Args: none
  Scroll the current message up in the other window N lines.  If called 
interactively, prefix arg gives number of lines to scroll.  Can't use
scroll-other-window because there might be more than two windows on the 
screen."
  (interactive "P")
  (pcmail-summary-goto-message)    
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (scroll-up n)
    (pop-to-buffer pcmail-summary-buffer)))

(defun pcmail-summary-scroll-message-down (&optional n)
  "Scroll the current message backward in the other window
Args: n
  Scroll the current message backward in the other window N lines.  If called 
interactively, prefix arg gives number of lines to scroll.  Can't use
scroll-other-window because there might be more than two windows on the 
screen."
  (interactive "P")
  (pcmail-summary-goto-message)    
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (scroll-down n)
    (pop-to-buffer pcmail-summary-buffer)))

;;; attribute-setting commands.  As in folder mode.

(defun pcmail-summary-delete-message ()
  "Delete this message and move to the next interesting message.
Args: none
  Call pcmail-delete-message interactively.  See pcmail-delete-message."
  (interactive)
  (let ((curr) (old) (d))
    (pcmail-summary-goto-message)    
    (pcmail-summary-pop-to-owner)
    (setq old pcmail-current-subset-message)
    (call-interactively 'pcmail-delete-message)
    (setq curr pcmail-current-subset-message)
    (setq d (pcmail-has-attribute-p (pcmail-make-absolute old) "deleted"))
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-set-attr ?D d)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-delete-message-backward ()
  "Delete this message and move to the previous interesting message.
Args: none
  Call pcmail-delete-message-backward interactively.  See 
pcmail-delete-message-backward."
  (interactive)
  (let ((curr) (old) (d))
    (pcmail-summary-goto-message)    
    (pcmail-summary-pop-to-owner)
    (setq old pcmail-current-subset-message)
    (call-interactively 'pcmail-delete-message-backward)
    (setq curr pcmail-current-subset-message)
    (setq d (pcmail-has-attribute-p (pcmail-make-absolute old) "deleted"))
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-set-attr ?D d)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-kill-message-later ()
  "Cause a message to expire at a future date.
Args: none
  Call pcmail-kill-message-later interactivel.. See pcmail-kill-message-later."
  (interactive)
  (pcmail-summary-goto-message)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-kill-message-later)
    (pop-to-buffer pcmail-summary-buffer)))

(defun pcmail-summary-undelete-message ()
  "Looking backward from the current message, clear the first deleted 
message's delete attribute.
Args: none"
  (interactive)
  (let ((curr))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (pcmail-undelete-previous-message)
    (setq curr pcmail-current-subset-message)
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message curr)))

(defun pcmail-summary-set-message-attr-menu (event)
  "Set a named attribute of the current message.  
Args: (event)
  Pop up a window containing valid attributes, then set the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Set attribute: ")))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (and choice (pcmail-change-message-attr choice t))
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message)))

(defun pcmail-summary-clear-message-attr-menu (event)
  "Clear a named attribute of the current message.  
Args: (event)
  Pop up a window containing valid attributes, then clear the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Clear attribute: ")))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (and choice (pcmail-change-message-attr choice nil))
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message)))

(defun pcmail-summary-change-message-attr ()
  "Toggle a named attribute of the current message.  
Args: none
  Call pcmail-change-message-attr interactively.  See 
pcmail-change-message-attr."
  (interactive)
  (pcmail-summary-goto-message)
  (pcmail-summary-pop-to-owner)
  (unwind-protect			;so C-G leaves you in summary
      (call-interactively 'pcmail-change-message-attr)
    (pop-to-buffer pcmail-summary-buffer)
    (pcmail-summary-goto-message)))	;to get U/D settings right

(defun pcmail-summary-zap-to-message ()
  "Delete all messages in the current subset from the current message forward.
Args: none
  Call pcmail-zap-to-message interactively.  See 
pcmail-zap-to-message."
  (interactive)
  (pcmail-summary-goto-message)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-zap-to-message)
  (pop-to-buffer pcmail-summary-buffer)

  ; now loop from current message forward, setting "D" markers in the summary
  ; buffer
  (while (not (eobp))
    (pcmail-summary-set-attr ?D t)
    (forward-line 1))

  ; and pop back to the message
  (pcmail-summary-goto-message))

;;; output commands: copy, file, print

(defun pcmail-summary-copy-message ()
  "Copy the current message to a named folder.
Args: none
  Call pcmail-copy-message interactively.  See pcmail-copy-message."
  (interactive)
  (let ((curr) (d) (old))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (setq old pcmail-current-subset-message)
    (unwind-protect
	(call-interactively 'pcmail-copy-message)
      (setq curr pcmail-current-subset-message)
      (setq d (pcmail-has-attribute-p (pcmail-make-absolute old) "deleted"))
      (pop-to-buffer pcmail-summary-buffer)
      (pcmail-summary-set-attr ?D d)
      (pcmail-summary-goto-message curr))))

(defun pcmail-summary-print-message ()
  "Print the current message.
Args: none
  Call pcmail-print-message interactively.  See pcmail-print-message."
  (interactive)
  (let ((curr) (d) (old))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (setq old pcmail-current-subset-message)
    (unwind-protect
	(call-interactively 'pcmail-print-message)
      (setq curr pcmail-current-subset-message)
      (setq d (pcmail-has-attribute-p (pcmail-make-absolute old) "deleted"))
      (pop-to-buffer pcmail-summary-buffer)
      (pcmail-summary-set-attr ?D d)
      (pcmail-summary-goto-message curr))))

(defun pcmail-summary-archive-message ()
  "Archive the current message.  
Args: none
  Call pcmail-archive-message interactively.  See pcmail-archive-message."
  (interactive)
  (let ((curr) (d) (old))
    (pcmail-summary-goto-message)
    (pcmail-summary-pop-to-owner)
    (setq old pcmail-current-subset-message)
    (unwind-protect
	(call-interactively 'pcmail-archive-message)
      (setq curr pcmail-current-subset-message)
      (setq d (pcmail-has-attribute-p (pcmail-make-absolute old) "deleted"))
      (pop-to-buffer pcmail-summary-buffer)
      (pcmail-summary-set-attr ?D d)
      (pcmail-summary-goto-message curr))))

;;; subset commands

(defun pcmail-summary-archive-subset ()
  "Archive the current subset from the summary window.
Args: none
  Call pcmail-archive-subset interactively.  See pcmail-archive-subset.  Note
that the deleted bits which may get set by a mass archive are reflected into
the summary by resummarizing.  That is much cheaper than updating the changed
records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-archive-subset)
    (pcmail-summarize-folder)))

(defun pcmail-summary-copy-subset ()
  "Copy the current subset from the summary window.
Args: none
  Call pcmail-copy-subset interactively.  See pcmail-copy-subset.  Note
that the deleted bits which may get set by a mass copy are reflected into
the summary by resummarizing.  That is much cheaper than updating the changed
records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-copy-subset)
    (pcmail-summarize-folder)))

(defun pcmail-summary-delete-subset ()
  "Delete the current subset from the summary window.
Args: none
  Call pcmail-delete-subset interactively.  See pcmail-delete-subset.  Note
that the deleted bits which may get set by a mass delete are reflected into
the summary by resummarizing.  That is much cheaper than updating the changed
records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-delete-subset)
    (pcmail-summarize-folder)))

(defun pcmail-summary-filter-folder ()
  "Filter and re-summarize the current folder.
Args: none
  Call pcmail-filter-folder interactively.  See pcmail-filter-folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-filter-folder)
    (pcmail-summarize-folder)))

(defun pcmail-summary-change-priority-subset ()
  "Change the priority of the current subset from the summary window.
Args: none
  Call pcmail-change-priority-subset interactively.  See
pcmail-change-priority-subset."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-change-priority-subset))

(defun pcmail-summary-print-subset ()
  "Print the current subset from the summary window.
Args: none
  Call pcmail-print-subset interactively.  See pcmail-print-subset.  Note
that the deleted bits which may get set by a mass print are reflected into
the summary by resummarizing.  That is much cheaper than updating the changed
records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-print-subset)
    (pcmail-summarize-folder)))

(defun pcmail-summary-undelete-subset ()
  "Undelete the current subset from the summary window.
Args: none
  Call pcmail-undelete-subset interactively.  See pcmail-undelete-subset.  Note
that the deleted bits which may get cleared by a mass undelete are reflected 
into the summary by resummarizing.  That is much cheaper than updating the 
changed records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-undelete-subset)
    (pcmail-summarize-folder)))

(defun pcmail-summary-set-attr-subset-menu (event)
  "Set a named attribute in each message of the current message subset.
Args: none
  Call pcmail-change-attr-subset to set the attributes.  See
pcmail-change-attr-subset.  Note that the deleted bits which may get cleared 
by a mass change are reflected into the summary by resummarizing.  That is 
much cheaper than updating the changed records individually."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Set attribute: ")))
    (pcmail-summary-pop-to-owner)
    (and choice (pcmail-change-attr-subset choice t))
    (pcmail-summarize-folder)))

(defun pcmail-summary-clear-attr-subset-menu (event)
  "Clear a named attribute in each message of the current message subset.
Args: none
  Call pcmail-change-attr-subset to clear the attributes.  See
pcmail-change-attr-subset.  Note that the deleted bits which may get cleared 
by a mass change are reflected into the summary by resummarizing.  That is 
much cheaper than updating the changed records individually."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Clear attribute: ")))
    (pcmail-summary-pop-to-owner)
    (and choice (pcmail-change-attr-subset choice nil))
    (pcmail-summarize-folder)))

(defun pcmail-summary-change-attr-subset ()
  "Toggle a named attribute in each message of the current message subset.
Args: none
  Call pcmail-change-attr-subset interactively.  See pcmail-change-attr-subset.
Note that the deleted bits which may get cleared by a mass change are 
reflected into the summary by resummarizing.  That is much cheaper than 
updating the changed records individually."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-change-attr-subset)
    (pcmail-summarize-folder)))

;;; random other commands

(defun pcmail-summary-open-folder-menu (event)
  "Open a named folder and re-summarize it.
Args: (event)
  Call pcmail-get-mail interactively.  See pcmail-get-mail."
  (interactive "e")
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (pcmail-get-mail (pcmail-read-folder "Open Folder: "))
    (pcmail-summarize-folder)))

(defun pcmail-summary-get-mail ()
  "Open a named folder and re-summarize it.
Args: none
  Call pcmail-get-mail interactively.  See pcmail-get-mail."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-get-mail)
    (pcmail-summarize-folder)))

(defun pcmail-summary-edit-message ()
  "Edit the current message in the other window.
Args: none
Call pcmail-edit-message interactively.  See pcmail-edit-message."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-edit-message))

(defun pcmail-summary-sort-folder ()
  "Sort the current folder by a key and re-summarize the current folder.
Args: none
  Call pcmail-sort-folder interactively.  See pcmail-sort-folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-sort-folder)
    (pcmail-summarize-folder)))

(defun pcmail-summary-expand-subset ()
  "Expand the current subset to include all messages in the current folder
and re-summarize the folder
Args: none
  Call pcmail-expand-subset.  See pcmail-expand-subset."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (call-interactively 'pcmail-expand-subset)
    (pcmail-summarize-folder)))
  

(defun pcmail-summary-exit ()
  "Exit the current summary, returning to the owning folder.
Args: none
  A little skullduggery here since pcmail-summary-owner is a local variable
and we need our hands on it after nuking the summary buffer in order to
pop back to the owner."
  (interactive)
  (pop-to-buffer (prog1 (or (get-buffer pcmail-summary-owner)
			    (other-buffer))
		   (bury-buffer (current-buffer))))
  (delete-other-windows))

(defun pcmail-summary-quit ()
  "Exit the mail reader.  See pcmail-quit.
Args: none"
  (interactive)
  (pcmail-summary-exit)
  (call-interactively 'pcmail-quit))

(defun pcmail-summary-expunge-folder ()
  "Expunge and re-summarize the current folder.
Args: none
  Call pcmail-expunge-folder.  See pcmail-expunge-folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (pcmail-expunge-folder)
    (cond ((zerop pcmail-total-messages)
	   (kill-buffer pcmail-summary-buffer)
	   (delete-other-windows)))
    (pcmail-summarize-folder)))		;in case expunge changed #messages


(defun pcmail-summary-save-folder ()
  "Save and re-summarize the current folder.
Args: none
  Call pcmail-save-folder.  See pcmail-save-folder"
  (interactive)
  (pcmail-summary-pop-to-owner)
  (unwind-protect
      (pcmail-save-folder)
    (pcmail-summarize-folder)))	;in case expunge changed #messages

(defun pcmail-summary-answer-message ()
  "Reply to the current message.
Args: none
  Call pcmail-answer-message interactively.  See pcmail-answer-message.  
Note that sending the message does not return you to the summary window, 
but instead leaves you at the current message in the folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-answer-message))

(defun pcmail-summary-mail ()
  "Compose mail in another window.
Args: none
  Call pcmail-mail interactively.  See pcmail-mail.  Note that sending 
the message does not return you to the summary window, but instead
leaves you at the current message in the folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-mail))

(defun pcmail-summary-mail-subset ()
  "Compose mail in another window.
Args: none
  Call pcmail-mail-subset interactively.  See pcmail-mail-subset.  Note that
sending the message does not return you to the summary window, but instead
leaves you at the current message in the folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-mail-subset))

(defun pcmail-summary-forward-message ()
  "Forward the current message.
Args: none
  Call pcmail-forward-message interactively.  See pcmail-forward-message.  
Note that sending the message does not return you to the summary window, 
but instead leaves you at the current message in the folder."
  (interactive)
  (pcmail-summary-pop-to-owner)
  (call-interactively 'pcmail-forward-message))

(provide 'pcmailsum)
