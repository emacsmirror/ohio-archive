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

;; NOTE: this is the only code in the pcmail mail reader implementation 
;;       that understands Babyl.  The message format therefore does not
;;       matter provided the public entry points all do the expected thing

;;;; folder format definition.  Defining a new folder format is fairly simple.
;;;; A folder format is a symbol with a number of required properties.
;;;; One of these is a function which, when applied to a file, will return
;;;; the folder format symbol if the file is in that folder format.  The
;;;; buffer local variable pcmail-folder-format is set to the format symbol.
;;;; The format symbol must be a member of the listpcmail-valid-folder-formats
;;;; in order for folders in that format to be recognized as such.
;;;; The mail system uses generic folder operations which actually funcall
;;;; properties in the symbol pointed at by pcmail-folder-format.  All of
;;;; these properties must be defined for the format to work.

;; define folder format list if not already defined
(defvar pcmail-valid-folder-formats nil
  "List of valid Pcmail folder formats.  
Each folder format definition file -- pcmailbabyl.el, pcmailberk.el, for
example -- add their format symbol, together with its properties, to the list.
If the list has not been bound, the folder definition routine must do so.")

;; add format to list if not already in list
(or (memq 'babyl-format pcmail-valid-folder-formats)
    (setq pcmail-valid-folder-formats
	  (cons 'babyl-format pcmail-valid-folder-formats)))

;; and install folder operation properties.  Each of the property values
;; must be a function defined in this file.

(put 'babyl-format 'folder-setup-hook 'pcmail-babyl-setup-hook)
(put 'babyl-format 'format-determination-fn 'pcmail-folder-in-babyl-format-p)

(put 'babyl-format 'prune-header-fn 'pcmail-babyl-prune-header)
(put 'babyl-format 'header-pruned-p-fn 'pcmail-babyl-header-pruned-p)
(put 'babyl-format 'narrow-to-unpruned-header-fn 
     'pcmail-babyl-narrow-to-unpruned-header)
(put 'babyl-format 'narrow-to-message-fn 'pcmail-babyl-narrow-to-message)

(put 'babyl-format 'insert-folder-header-fn 'pcmail-babyl-insert-header)
(put 'babyl-format 'get-folder-mail-drop-list-fn 
     'pcmail-get-babyl-mail-drop-list)
(put 'babyl-format 'scan-folder-messages-fn 'pcmail-scan-babyl-messages)
(put 'babyl-format 'convert-message-to-folder-format-fn
     'pcmail-babyl-convert-message-to-folder-format)

(put 'babyl-format 'change-folder-message-attr-fn 'pcmail-change-babyl-attr)
(put 'babyl-format 'folder-message-has-attr-p-fn 'pcmail-babyl-attr-present-p)
(put 'babyl-format 'user-defined-attr-list-fn 
     'pcmail-babyl-user-defined-attr-list)
(put 'babyl-format 'install-user-defined-attr-fn 
     'pcmail-babyl-install-user-defined-attr)

;;;; global variables

(defconst pcmail-babyl-begin "\^L"
  "Character sequence that begins a Babyl message.")

(defconst pcmail-babyl-end "\n\^_"
  "Character sequence that ends a Babyl message.")

(defconst pcmail-babyl-exploded-end "^_"
  "The character sequence to which we change pcmail-babyl-end when we encounter
it in the body of a mail message")

(defconst pcmail-babyl-old-header-delim "*** EOOH ***\n"
  "Babyl old header delimiting string.")

(defconst pcmail-babyl-header
  (concat pcmail-babyl-begin "\n0, unseen,,\n" pcmail-babyl-old-header-delim)
  "Initial Babyl message header string.")

(defconst pcmail-babyl-defined-attributes
  '("deleted" "forwarded" "filed" "answered" "unseen" "recent" "badheader")
  "A list of attributes defined in the Babyl specification")

(defvar pcmail-uninteresting-fields-regexp nil
  "Regexp of headers derived from pcmail-interesting-fields-list.")

;;;; folder format functions

(defun pcmail-folder-in-babyl-format-p ()
  "Return 'babyl-format if current folder is babyl format, NIL else.
Args: none"
  (save-excursion
    (goto-char (point-min))
    (and (looking-at "^BABYL OPTIONS:\n")
	 'babyl-format)))

(defun pcmail-babyl-setup-hook ()
  "Babyl-specific folder post-processing.
Args: none"

  ; require-final-newline hosed us? Punt trailing whitespace but don't
  ; change buffer-modified-p
  (save-excursion    			
    (save-restriction
      (widen)
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (let ((buffer-read-only nil)
	    (modp (buffer-modified-p)))
	(delete-region (point) (point-max))
	(set-buffer-modified-p modp)))))

;;; header-pruning operations
  
(defun pcmail-babyl-prune-header (n state)
  "Either prune (STATE = T) or expand (STATE = NIL) message N's header.
Args: (n state)
  If STATE is non-NIL, prune message absolute-numbered N's message header by
copying N's header, passing it through pcmail-uninteresting-fields-regexp, 
and placing the copy after the original header line so it will be displayed by 
pcmail-narrow-to-message.  If STATE is NIL, remove the pruned header copy,
leaving only the unpruned header. 

Save the state of buffer-modified-p before pruning or unpruning and restore 
afterwards.  This saves on disk writes and doesn't hurt us since all messages 
are automatically pruned on display as necessary anyway."
  (or (zerop n)
      (and state (pcmail-header-pruned-p n))
      (and (not state) (not (pcmail-header-pruned-p n)))
      (let ((buffer-read-only nil)
	    (mod (buffer-modified-p)))
	(widen)
	(goto-char (pcmail-msgbeg n))
	(forward-line 1)
	(delete-char 1)
	(insert (if state ?1 ?0))
	(forward-line 1) 
	(cond (state
	       (if (looking-at (regexp-quote pcmail-babyl-old-header-delim))
		   (delete-region (point) (progn (forward-line 1) (point))))
	       (insert-buffer-substring 
		(current-buffer) 
		(point)
		(save-excursion 
		  (search-forward pcmail-header-delim (pcmail-msgend n) 'move)
		  (point)))
	       (insert pcmail-babyl-old-header-delim)
	       (narrow-to-region (point)
				 (progn
				   (search-forward pcmail-header-delim
						   (pcmail-msgend n) t)
				   (point)))
	       (pcmail-nuke-uninteresting-fields n))
	      (t
	       (insert pcmail-babyl-old-header-delim)
	       (search-forward pcmail-babyl-old-header-delim)
	       (forward-line -1)
	       (let ((temp (point)))
		 (and (search-forward pcmail-header-delim nil t)
		      (delete-region temp (point))))))
	(set-buffer-modified-p mod))))

(defun pcmail-babyl-header-pruned-p (n)
  "Return T if message absolute-numbered N's header is pruned, NIL else.
Args: (n)"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (pcmail-msgbeg n))
      (forward-line 1)
      (= (following-char) ?1))))

(defun pcmail-babyl-narrow-to-unpruned-header (n)
  "Narrow to the region around message absolute-numbered N's unpruned header.
Args: (n)"
  (widen)
  (goto-char (pcmail-msgbeg n))
  (cond ((pcmail-header-pruned-p n)
	 (forward-line 2)		;skip over attrs
	 (narrow-to-region (point)
			   (progn (search-forward 
				   pcmail-babyl-old-header-delim)
				  (forward-line -1) (point))))
	(t
	 (forward-line 3)		;skip over attrs, EOOH
	  (narrow-to-region
	    (point)
	    (progn (search-forward pcmail-header-delim
				   (pcmail-msgend n) 'move)
		   (point))))))

(defun pcmail-babyl-narrow-to-message (n)
  "Narrow the current buffer to message absolute-numbered N.
Args: (n)
  Narrowed region is message N in a displayable form, i.e. starts with the
message's pruned header and does not include Babyl delimiters, Babyl
header information, or unpruned header.  If N is 0, narrow to the current
folder's Babyl header."
  (widen)
  (goto-char (pcmail-msgbeg n))
  (cond ((zerop n)
	  (narrow-to-region (pcmail-msgbeg n) (1- (pcmail-msgend n))))
	(t
	 (narrow-to-region
	  (progn (search-forward pcmail-babyl-old-header-delim nil t) (point))
	  (- (pcmail-msgend n) 2)))))

;;; folder-hacking operations

(defun pcmail-babyl-insert-header (mail-drop-list)
  "Insert a folder Babyl header at the start of the current buffer.
Args: (mail-drop-list)"
  (goto-char (point-min))
  (insert "BABYL OPTIONS:\nVersion: 5\nLabels: printed, copied, edited, "
	  "timely, expired, undigestified, precious\n"
	  (if mail-drop-list
	      (concat "Mail: "
		      (mapconcat '(lambda (s) (prin1-to-string s)) 
				 mail-drop-list ", ")
		      "\n")
	    "")
	  "Note: if you can see this, this folder is empty." 
	  pcmail-babyl-end))

(defun pcmail-get-babyl-mail-drop-list ()
  "Return a list of interned mail drop symbols derived from the names in
the current mail file's mail: option field."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-babyl-header)
      (let ((s))
	(setq s (mail-fetch-field "mail"))
	(and s
	     (mapcar '(lambda (x) (intern x)) (pcmail-parse-space-list s)))))))

(defun pcmail-scan-babyl-messages (&optional start)
  "Scan the current folder buffer, setting up message information.
Args: (start)
  Create attribute list and message marker list for current folder.  Put 
message-delimiting markers in messages-list, message attributes in
attr-list, timely message numbers in timely-list, and bump message total 
total-messages.  Scan from buffer position START to end of buffer."
  (let ((msg-attr-list))
    (or start
	(setq total-messages -1))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (or start (point-min)))
	(while (re-search-forward pcmail-babyl-end nil t)
	  (setq msg-attr-list (pcmail-make-babyl-attr-list)
		attr-list (cons msg-attr-list attr-list)
		messages-list (cons (point-marker) messages-list))
	  (and (pcmail-in-sequence-p "timely" msg-attr-list)
	       (setq timely-list (cons (1+ total-messages) timely-list)))
	  (and (zerop (% (setq total-messages (1+ total-messages)) 
			     pcmail-progress-interval))
	       (not (zerop total-messages))
	       (message "Counting messages in %s...%d" pcmail-folder-name
			total-messages)))
	(setq messages-list (nreverse messages-list)
	      attr-list (nreverse attr-list))))))


(defun pcmail-babyl-convert-message-to-folder-format (mail-drop 
						      &optional attrlist)
  "Perform all message header processing necessary for babyl format.
Args: (mail-drop &optional attrlist)
  This routine is called from the inbox-format-to-folder-format conversion
routines.  It assumes the buffer has been narrowed to the current message.
If attribute list ATTRLIST is non-NIL, permanently attach those attributes
to the message.  Leave the buffer widened, with point at the end of the 
message"
  (goto-char (point-min))
  (or (eq mail-drop 'babyl-mail-drop)
      (if attrlist
	  (insert pcmail-babyl-begin "\n0," 
		  (pcmail-format-babyl-attrs attrlist)
		  "\n" pcmail-babyl-old-header-delim)
	(insert pcmail-babyl-header)))

  ; processing is different depending on mail drop format
  (cond ((eq mail-drop 'babyl-mail-drop)
	 (save-excursion
	   (cond ((re-search-forward pcmail-babyl-end nil 'move)
		  (replace-match "")
		  (delete-region (point)
				 (progn (skip-chars-forward " \t\n")
					(point)))))))
	((or (eq mail-drop 'vms-file-mail-drop)
	     (eq mail-drop 'vms-default-mail-drop)))
	(t
	 (pcmail-bash-unix-header)))
  (goto-char (point-min))
  (while (search-forward pcmail-babyl-end nil 'move)
    (replace-match (concat "\n" pcmail-babyl-exploded-end)))
  (widen)
  (insert pcmail-babyl-end))

;;; attribute-hacking functions

(defun pcmail-change-babyl-attr (n attr state)
  "Add (STATE = T) or remove (STATE = NIL) a label to message N's label list.
Args: (n attr state)
If N is NIL, search backward for the nearest Babyl message, otherwise use
message absolute-numbered N.  If STATE is NIL, remove ATTR from N's label
list.  Otherwise, If ATTR is babyl-defined (i.e. a member of the list 
pcmail-babyl-defined-attributes), place it at the end of the attribute list
according to the Babyl specification.  If user-defined, put it at the 
beginning of the list."
  (save-excursion
    (save-restriction
      (let ((buffer-read-only))
	(pcmail-narrow-to-babyl-attrs n)
	(cond (state
	       (if (pcmail-babyl-defined-attribute-p attr)
		   (goto-char (point-min))
		 (goto-char (point-max)))
	       (insert " " attr ","))
	      (t
	       (and (re-search-forward (concat " ?" attr ",") nil t)
		    (delete-region (match-beginning 0) (match-end 0)))))))))

(defun pcmail-babyl-attr-present-p (n attr)
  "Return non-NIL if ATTR is present in message absolute-numbered N, NIL else.
Args: (n attr)
If N is NIL, narrow to most recent babyl message's babyl label list, 
otherwise narrow to message absolute-numbered N's list.  Return non-NIL if
ATTR is there, NIL else."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-babyl-attrs n)
      (let ((case-fold-search t))
	(re-search-forward (concat " ?" attr ",") nil t)))))

(defun pcmail-babyl-install-user-defined-attr (attr)
  "Install the user-defined attr ATTR in the current folder's folder header.
Args: (args)
This could be made a lot simpler by looking for a labels line and appending
to it or creating it if it doesn't exist.  Instead, we fill labels lines
with no more than default-fill-column worth of labels, then create a new
label line.  Also, labels are comma-separated with no leading commas."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-babyl-header)
      (let ((buffer-read-only)
	    (case-fold-search t)
	    (labelist)
	    (label-line-exists)
	    (label-exists)
	    (label-null)
	    (label-start))
	
	; skullduggery to use multiple labels: lines rather than
	; overflow the line.  Search for last labels: line
	(while (re-search-forward "Labels:[ \t]*" nil t)
	  (setq label-start (point))
	  (and (looking-at "\n") (setq label-null t)) ;no labels here!
	  (and (re-search-backward attr (prog1 (point) (end-of-line)) t)
	       (setq label-exists t))
	  (setq label-line-exists t))
	
	(cond ((not label-exists)	;don't bother if label is already there
	       
	; if there is a label line, get the last line's contents
	       (and label-line-exists
		    (setq labelist
			  (buffer-substring label-start (point))))
	       
		; if there is no label line, or the current one will be too 
		; long with the newly-added label, make a new label line, 
		; otherwise use the current one
	       (cond ((or (not label-line-exists)
			  (> (+ 10 (length attr) (length labelist))
			     default-fill-column))
		      (goto-char (point-max))
		      (insert "Labels: " attr "\n"))
		     (t
		      (insert (if label-null "" ",") " " attr)))))))))
    
(defun pcmail-babyl-user-defined-attr-list ()
  "Return the list of non-Babyl-defined attributes in the Labels option
at the beginning of the current mail file."
  (let ((l) (k))
    (save-excursion
      (save-restriction)
      (pcmail-narrow-to-babyl-header)
      (setq l (mail-fetch-field "labels" nil t)
	    k (mail-fetch-field "keywords" nil t)) ;alt form
      (if k (setq l (if l (concat l " " k) k)))
      (and l (pcmail-parse-space-list l)))))

;;;; private utility routines used only by the above routines

(defun pcmail-nuke-uninteresting-fields (n)
  "Delete all of message N's header fields in pcmail-uninteresting-fields-list.
Args: (n)
  Search for matches for pcmail-uninteresting-fields-regexp, deleting region
from matched text start through possible field continuation lines.  Assume 
region is narrowed to message absolute-numbered N's header."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward pcmail-uninteresting-fields-regexp nil t)
      (replace-match ""))))

(defun pcmail-narrow-to-babyl-attrs (&optional n)
  "Narrow the current buffer to a message's Babyl attribute list.
Args: (&optional N)
  If N is non-NIL, narrow the current buffer to message absolute-numbered N's 
Babyl attribute list, otherwise narrow to the nearest Babyl attribute list 
in the current buffer.  Nearest Babyl message is found by searching backward
through the current buffer for the previous message's Babyl message-end 
delimiter and parsing the attribute list following.  If no delimiter 
exists (i.e. point is at the Babyl folder header), return NIL."
  (widen)
  (let ((found (if n
		   (goto-char (pcmail-msgbeg n))
		 (re-search-backward 
		  (concat pcmail-babyl-end pcmail-babyl-begin) nil t))))
    (and found
	 (cond ((re-search-forward "[01]," (and n (pcmail-msgend n)) t)
		(narrow-to-region (point) (progn (end-of-line) (point)))
		(goto-char (point-min)))
	       (t
		(error "Region not Babyl format (narrow-to-babyl-attrs)"))))))

(defun pcmail-narrow-to-babyl-header ()
  "Narrow the current buffer to its folder Babyl header.
Args: none"
  (widen)
  (goto-char (point-min))
  (narrow-to-region 
   (point)
   (progn (or (re-search-forward pcmail-babyl-end nil t)
	      (error "Region not Babyl format (narrow-to-babyl-header)"))
	  (1- (point))))
  (goto-char (point-min)))

(defun pcmail-make-babyl-attr-list ()
  "Return a list of attribute strings from a message's Babyl label list.
Args: none
Create an attribute string list from the closest message's Babyl attribute
information.  Assume point is at the start of a message's Babyl information.
This will work on both RMAIL-style and true Babyl-style attribute lists."
  (save-excursion
    (save-restriction
      (and (pcmail-narrow-to-babyl-attrs)
	   (pcmail-parse-space-list 
	    (buffer-substring (point-min) (point-max)))))))

(defun pcmail-babyl-defined-attribute-p (attr)
  "Return non-NIL if ATTR is a babyl-defined attribute, NIL else."
  (pcmail-in-sequence-p attr pcmail-babyl-defined-attributes))

(defun pcmail-format-babyl-attrs (l)
  "Turn a list of attributes into a Babyl-format label string.
Args: (attrlist)
Given a list of attributes, turns it into a Babyl-compatible label string.
The string is of the form \"( <attr>,)*,( <user-defined-label>,)*\"."
  (let ((sys-attrs) (user-attrs))
    (while l
      (if (pcmail-babyl-defined-attribute-p (car l))
	  (setq sys-attrs (cons (car l) sys-attrs))
	(setq user-attrs (cons (car l) user-attrs)))
      (setq l (cdr l)))

    ; now have 2 lists: system attributes and user attributes
    ; put them together according to the Babyl specification
    (concat 
     (mapconcat '(lambda (s) (concat " " s ",")) sys-attrs "")
     ","
     (mapconcat '(lambda (s) (concat " " s ",")) user-attrs ""))))

(provide 'pcmailbabyl)
