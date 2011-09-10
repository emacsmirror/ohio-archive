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

;;;; required pcmail elisp files.  Order is significant.  In particular,
;;;; pcmailsysdep.el must be first, and all folder-format definition files
;;;; (in this case pcmailbabyl.el) must immediately follow

(require 'pcmailsysdep)
(require 'pcmailbabyl)
(require 'pcmaildrop)
(require 'pcmailfolder)
(require 'pcmaillist)
(require 'pcmailattr)
(require 'pcmailmove)
(require 'pcmaildate)
(require 'pcmailsub)
(require 'mail-utils)

;;;; global variables

;;; system-defined variables

(defconst pcmail-version "4.3"
  "Mail reader version.")

(defvar pcmail-directory (get 'pcmail-mail-environment 'mail-directory)
  "The directory in which all folders are stored.")

(defvar pcmail-time-zone (get 'pcmail-mail-environment 'time-zone)
  "The local time zone, in three character format, i.e. \"PST\".")

(defconst pcmail-header-delim "\n\n"
  "Mail header delimiter.")
  
;;; user-defined config parameters
  
;;;###autoload
(defvar pcmail-archive-hook
  nil
  "*Hook expression which is applied before each message is inserted
into an archive file with the pcmail-archive-message or pcmail-archive-subset
commands.")

;;;###autoload
(defvar pcmail-nntp-host-name 
  "newshost"
  "*The name of your local NNTP server.  Only interesting if you are using
The nntp-mail-drop or nntp-file-mail-drop mail drop types, in which case it 
must be defined.")

;;;###autoload
(defvar pcmail-expiration-hook
  '(lambda (n) (pcmail-set-attribute n "deleted" t))
  "*A hook expression that is applied to messages with the \"timely\"
attribute when current date is later than message's \"expires:\" field.")

;;;###autoload
(defvar pcmail-read-mail-drop-hook
  nil
  "*A hook expression that is run after new mail has been stored in its
folder.  The hook can be used to clear local \"you-have-mail\" indicators.")

;;;###autoload
(defvar pcmail-progress-interval 10
  "*When performing time-intensive tasks like message counting or filtering,
a progress message is displayed every pcmail-progress-interval messages, with
the total number of messages processed displayed upon completion of the task.")

;;;###autoload
(defvar pcmail-uninteresting-fields-list
  '("via" "mail-from" "origin" "status" "received" "message-id" "expires"
	  "resent-message-id" "summary-line" "return-path" "priority"
	  "header-pruned" "attributes")
  "*Non-nil means prune from the message header all fields in this list.
View the unpruned header with the \\[pcmail-toggle-message-header] command.")

;;;###autoload
(defvar pcmail-wastebasket-folder "wastebasket"
  "*The wastebasket folder name.  The wastebasket folder is a useful place 
to copy messages that aren't really wanted anymore.  
If pcmail-wastebasket-on-expunge is non-NIL, expunged messages are placed
here before removal from their folder.")

;;;###autoload
(defvar pcmail-wastebasket-on-expunge nil
  "*Non-nil means copy deleted messages to the wastebasket before expunging.
This can be very time-consuming.")

;;;###autoload
(defvar pcmail-expunge-on-save t
  "*Non-nil means expunge folders before saving them.")

;;;###autoload
(defvar pcmail-save-on-quit t
  "*Non-nil means save all folders upon exit from the mail reader.")

;;;###autoload
(defvar pcmail-delete-on-archive nil
  "*Non-nil means automatically delete a message that is archived to a file.")

;;;###autoload
(defvar pcmail-delete-on-copy nil
  "*Non-nil means automatically delete a message that is copied to another 
folder.")

;;;###autoload
(defvar pcmail-delete-on-print nil
  "*Non-nil means automatically delete a message that is sent to a printer.")

;;;###autoload
(defvar pcmail-printer-name (get 'pcmail-mail-environment 'printer)
  "*The printer that the \\[pcmail-print-message] command sends messages to.")

;;;###autoload
(defvar pcmail-pigeonhole-hook nil
  "*If non-NIL, a hook expression applied to each new message in a mail drop.
The hook expression is passed the new message's absolute message number.")

;;;###autoload
(defvar pcmail-interesting-hook 
  '(lambda (n) (not (pcmail-has-attribute-p n "deleted")))
  "*If non-NIL, a lambda expression which is applied to a message number.  
If the expression returns non-NIL, the message is interesting, otherwise it
is not.")

;;;###autoload
(defvar pcmail-yank-message-on-reply nil
  "*If non-NIL, the \\[pcmail-answer-message\\] command will automatically
insert a copy of the replied-to message in the message reply.")

;;;###autoload
(defvar pcmail-yank-prefix nil
  "*If a string, any message inserted into the message composition buffer
will have that string placed at the beginning of each non-blank line.")

;;;###autoload
(defvar pcmail-highlight-forwarded-message nil
  "*Non-NIL means place a \"begin forwarded message\" line before the 
forwarded message and a \"end forwarded message\" line after it.  If
the variable value is a string, use that string as begin and end delimiter.
Messages are forwarded using the \\[pcmail-forward-message\\] command.")

;;;###autoload
(defvar pcmail-default-filter-name "all"
  "*Name of the filter to be used when you first enter a folder.
Default value is the filter named \"all\", which contains all messages
in the folder.")

;;;###autoload
(defvar pcmail-summary-window-percentage 50
  "*Percentage of mail window which should be taken by summary window")

;;;###autoload
(defvar pcmail-summary-format "%d  %25f  %s"
  "*The format string used to format summary lines.  
The following percent-constructs are recognized:

   %b:  replace with the contents of the bcc: field
   %c:  replace with the contents of the cc: field
   %C:  replace with the message's character count
   %d:  replace with the contents of the date: field, dd-mon-yy
   %f:  replace with the contents of the from: field
   %l:  replace with the message's line count
   %m:  replace with the contents of the message-id: field
   %s:  replace with the contents of the subject: field
   %t:  replace with the contents of the to: field

All directive modifications (field width, justification, etc) are recognized 
and work as the emacs-lisp format function.
Default value of pcmail-summary-format places date, followed by from and 
subject fields, in a summary line.")

;;;###autoload
(defvar pcmail-date-format "%d-%m-%y"
  "*The format string used to format dates.  
The following percent-constructs are recognized:

   %d:  replace with the day of the month
   %n:  replace with the number of the month
   %m:  replace with the first three letters of the month
   %M:  replace with the full name of the month
   %y:  replace with the last two digits of the year
   %Y:  replace with the full year

All directive modifications (field width, justification, etc) are recognized 
and work as the emacs-lisp format function.
Default value of pcmail-date-format is \"%d-%m-%y\", which creates a
date of the form dd-mmm-yy.")

;;;###autoload
(defvar pcmail-folder-mode-line-format 
  "Folder: %-18f (%eMessage %s/%S%n %a)"
  "*The format string used to format a folder's mode line.
The following percent-constructs are recognized:

   %a:  replace with the current message's attribute list, or \"[none]\",
        if the message has no attributes
   %c:  replace with the current message's character count
   %e:  if current message is being edited, replace with \"Editing\"
   %E:  if current message is timely, replace with expiration date
   %f:  replace with the current folder name
   %l:  replace with the current message's line count
   %n:  if the current subset does not comprise the entire folder, or the
        current message's number os not the same as its absolute number,
        replace with the message's absolute number and the total number of
        messages in the folder in the form \"[<curr>/<total>]\"
   %p:  replace with the current message's priority if the priority
        number is greater than 1.
   %s:  replace with the current message's number
   %S:  replace with the number of messages in the current subset

All directive modifications (field width, justification, etc) are recognized 
and work as the emacs-lisp format function.")

;;;###autoload
(defvar pcmail-resummarize-folder-on-change nil
  "*If non-NIL, resummarize a folder with an existing summary every time
the folder changes.  Changes are defined as a change in the number of messages
in the folder or a change in their order.  Default value is NIL.")

;;;###autoload
(defvar pcmail-summarize-on-entry nil
  "*If non-NIL, place primary folder in summary mode on entry to Pcmail.  
Default value is NIL.")

;;;###autoload
(defvar pcmail-default-folder-format 'babyl-format
  "*The default folder format.  When a folder is created and no format is
specified, use this format")

;;;###autoload
(defvar pcmail-new-frame-on-mail nil
  "*If non-nil, compose outgoing mail in a different frame.")

;;;###autoload
(defvar pcmail-new-frame-on-open nil
  "*If non-nil, open new folders in a different frame.")

;;;; mail reader entry point

;;;###autoload
(defun pcmail (&optional folder-name) 
  "Read and edit mail using the Pcmail mail reader.

Pcmail operates on folder files in Babyl format.  User defined formats
are fairly easy to install; an experimental implementation of standard 
Berkeley mail format is included in the file pcmailberk.el, although the
format is not enabled by default.  Folder format is automatically determined 
whenever a folder is opened.

Notes on Pcmail's Babyl implementation: Pcmail (as does RMAIL)
treats the \"unseen\" label as a Babyl-defined attribute rather than
the user-defined attribute it should be.  Pcmail conforms to the Babyl
specification in all other respects.

There are a number of configuration variables that you can set to
customize the mail reader.  A list of them follows this documentation.
Use a lambda-expression set to \"pcmail-hook\" in order to set these
variables upon entry into the mail reader.  Type \\[describe-mode]
after the mail reader has started; this will get you a list of mode
commands.  Typing \\[describe-function] with one of the function names
listed in the mode documentation will give more detailed documentation
on what the particular function does.

Following is a list of user-settable configuration variables.  Type
 \\[describe-variable] to get a particular variable's description.

pcmail-date-format		  pcmail-default-filter-name		
pcmail-default-folder-format	  pcmail-delete-on-archive		
pcmail-delete-on-copy 		  pcmail-delete-on-print
pcmail-expiration-hook		  pcmail-expunge-on-save
pcmail-folder-mode-line-format	  pcmail-highlight-forwarded-message
pcmail-interesting-hook		  pcmail-new-frame-on-mail
pcmail-new-frame-on-open          pcmail-nntp-host-name		  
pcmail-pigeonhole-hook		  pcmail-printer-name
pcmail-progess-interval		  pcmail-resummarize-folder-on-change
pcmail-read-mail-drop-hook        pcmail-summarize-on-entry
pcmail-summary-format		  pcmail-summary-window-percentage
pcmail-uninteresting-fields-list  pcmail-wastebasket-folder
pcmail-wastebasket-on-expunge	  pcmail-yank-message-on-reply
pcmail-yank-prefix		        

mail-aliases		          mail-archive-file-name
mail-default-reply-to		  mail-header-separator
mail-self-blind		          mail-setup-hook
mail-use-rfc822		          mail-yank-ignored-headers
rmail-dont-reply-to-names

Typing \\[pcmail] causes the hook variable \"pcmail-hook\" to be evaluated. 
The hook variable \"pcmail-exit-hook\" is evaluated upon exit from the mail 
reader via the \\[pcmail-quit] command.  With a prefix argument, Pcmail 
will ask for the name of a folder to open on startup, otherwise it will 
open the primary folder."
  (interactive
   (list (and current-prefix-arg
	      (pcmail-read-folder "Folder name: "))))
  (and (< (string-to-int (substring emacs-version 0 2)) 19)
       (error "Version %s of Pcmail requires Emacs version 19 or later."
	      pcmail-version))
  (run-hooks 'pcmail-hook)
  (pcmail-maybe-init)
  (pcmail-get-mail (or folder-name pcmail-primary-folder-name))
  (and pcmail-summarize-on-entry
       (pcmail-summarize-folder)))

;;; maybe create mail directory, folder list, and primary folder.
(defun pcmail-maybe-init ()
  "Create mail directory and primary folder as necessary.
Args: none"
  (and (get 'pcmail-mail-environment 'send-mail-function)
       (setq send-mail-function 
	     (get 'pcmail-mail-environment 'send-mail-function)))
  (pcmail-make-uninteresting-fields-regexp)
  (cond ((not (file-directory-p pcmail-directory))
	 (or (yes-or-no-p 
	      (format "Pcmail mail directory \"%s\" not found.  Create? "
		      pcmail-directory))
	   (error "Aborted."))
	 (funcall (get 'pcmail-mail-environment 'create-mail-directory-fn))
	 (pcmail-create-folder-list-file)))
  (pcmail-load-folder-information)
  (cond ((not (pcmail-find-folder pcmail-primary-folder-name))
	 (or (yes-or-no-p 
	      (format "Pcmail primary folder \"%s\" not found.  Create? "
		      pcmail-primary-folder-name))
	     (error "Aborted."))
	 (pcmail-create-folder pcmail-primary-folder-name
			       (get 'pcmail-mail-environment 
				    'default-mail-drop-list)))))

;;; mail reader exit point

(defun pcmail-quit (no-hooks)
  "Exit the mail reader.  
Args: none.
  Exit the mail reader in an orderly manner.  If pcmail-save-on-quit is 
non-NIL, save all folders first.  Evaluate the hook variable 
pcmail-exit-hook unless a prefix argument was supplied."
  (interactive "P")
  (let ((cb (current-buffer)))

    ; if the wastebasket exists, open it up so its messages get counted,
    ; expired, and expunged correctly in the following save code
    (and (pcmail-find-folder pcmail-wastebasket-folder)
	 (save-excursion
	   (pcmail-open-folder pcmail-wastebasket-folder)))
    (and pcmail-save-on-quit
	 (mapcar
	  '(lambda (m)
	     (cond ((and (pcmail-folder-buffer-name m)
			 (get-buffer (pcmail-folder-buffer-name m)))
		    (pcmail-save-folder m))))
	  (pcmail-all-folders)))
    (let ((nmsgs))
      (and (pcmail-find-folder pcmail-wastebasket-folder)
	   (> (setq nmsgs (pcmail-nmessages pcmail-wastebasket-folder)) 0)
	   (progn (message "%d message%s in the wastebasket"
			   nmsgs (pcmail-s-ending nmsgs))
		  (sit-for 2))))
    (save-excursion
      (pcmail-open-folder-list)
      (pcmail-save-buffer)
      (bury-buffer (current-buffer)))
    (or no-hooks (run-hooks 'pcmail-exit-hook))
    
    ;and make sure the folder current at quit time is displayed now,
    ;and use pcmail-beginning-of-message to init an uninitted selected
    ;mail buffer
    (switch-to-buffer cb)

    ; bury all mail buffers other than the current mail buffer.  Iterate
    ; over all folder names, skipping those without a buffer
    (mapcar
     '(lambda (m)
	(let ((name (pcmail-folder-buffer-name m)))
	  (and name
	       (get-buffer name)
	       (or (eq (get-buffer name) cb)
		   (bury-buffer name)))))
     (pcmail-all-folders))

    (call-interactively 'switch-to-buffer)
    (and (eq major-mode 'pcmail-mode)
	 (pcmail-beginning-of-message))))

;;;; autoloads for edit, mail, summary, and output commands

(autoload 'pcmail-mail "pcmailmail" nil t)
(autoload 'pcmail-answer-message "pcmailmail" nil t)
(autoload 'pcmail-forward-message "pcmailmail" nil t)
(autoload 'pcmail-archive-message "pcmailout" nil t)
(autoload 'pcmail-print-message "pcmailout" nil t)
(autoload 'pcmail-copy-message "pcmailout" nil t)
(autoload 'pcmail-copy-message-menu "pcmailout" nil t)
(autoload 'pcmail-copy-message-1 "pcmailout" nil t)
(autoload 'pcmail-archive-subset "pcmailout" nil t)
(autoload 'pcmail-print-subset "pcmailout" nil t)
(autoload 'pcmail-copy-subset "pcmailout" nil t)
(autoload 'pcmail-copy-subset-menu "pcmailout" nil t)
(autoload 'pcmail-wastebasket-message "pcmailout" nil t)
(autoload 'pcmail-summarize-folder "pcmailsum" nil t)
(autoload 'pcmail-edit-message "pcmailmisc" nil t)
(autoload 'pcmail-sort-folder "pcmailmisc" nil t)
(autoload 'pcmail-sort-folder-menu "pcmailmisc" nil t)
(autoload 'pcmail-version-information "pcmailmisc" nil t)
(autoload 'pcmail-undigestify-message "pcmailmisc" nil t)
(autoload 'pcmail-undigestify-message-1 "pcmailmisc" nil t)
(autoload 'pcmail-toggle-message-header "pcmailmisc" nil t)
(autoload 'pcmail-kill-message-later "pcmailmisc" nil t)
(autoload 'pcmail-change-message-priority "pcmailmisc" nil t)
(autoload 'pcmail-change-priority-subset "pcmailmisc" nil t)
(autoload 'pcmail-message-priority "pcmailmisc" nil t)

;;;; random routines, used by all pc*.el files

;;; minibuffer input utilities

(defun pcmail-read-file-name (prompt fname &optional must-exist)
  "Read a file name from the minibuffer.
Args: (prompt fname &optional must-exist)
  Read a file name from the minibuffer, prompting with PROMPT and using
the file portion of FNAME as default file, the directory portion as default
directory.  If optional MUST-EXIST is non-NIL, input must be an existing
file name."
  (expand-file-name
   (read-file-name (concat prompt
			   (if fname (concat "(default " fname ") ") ""))
		   (and fname (file-name-directory fname))
		   fname
		   must-exist)))

;; a simple read routine to grab a file name from the minibuffer.  The function
;; is only called when the folder buffer has been widened, so it narrows to
;; the current message before getting input, then restores

(defun pcmail-narrow-read-file-name (fname)
  "Read a file name, narrowing the current buffer to the current message.
Args: (fname)
  FNAME is the default to present to the user.  If NIL, no default is 
presented.  Note this is a more restrictive version of pcmail-read-file-name,
that assumes a standard prompt and required file existence."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-message 
       (pcmail-make-absolute pcmail-current-subset-message))
      (pcmail-read-file-name "File name: " (expand-file-name fname) t))))

(defun pcmail-read-string-default (prompt &optional default no-blanks)
  "Read from minibuffer with optional default input.
Args: (prompt &optional default no-blanks)
  Read from minibuffer, prompting with PROMPT, plus DEFAULT (if present).
If a default is supplied, allow input of \"\", which causes the default 
value to be returned.  If \"\" is not input, the input is returned.  If 
optional NO-BLANKS is non-NIL, do not allow blanks in input."
  (let ((res))
    (cond ((and (stringp default)
		(> (length default) 0))
	   (setq prompt (concat prompt "(default " default ") ")))
	  (t
	    (setq default nil)))
    (cond (no-blanks
	   (setq res (read-no-blanks-input prompt "")))
	  (t
	   (setq res (read-string prompt))))
    (cond ((string= res "")
	    (cond (default)
		  (t
		    (error "No default has been set."))))
	  (t
	    res))))

(defun pcmail-completing-read (prompt table &optional default pred force-p)
  "Completing read from minibuffer with optional default input.
Args: (prompt table &optional default pred force-p)
  Read from the minibuffer using prompt PROMPT and completion list or
obarray TABLE.  If pred is non-NIL, input is valid only if PRED when
applied to input returns non-NIL.  If FORCE-P is non-NIL, require a 
match with an elt of TABLE.
If DEFAULT is a string, a blank string can be input, in which case the
returned value will be the default.  A non-blank input will be returned as 
a new default."
  (let ((res) (completion-ignore-case t))
    (cond ((and (stringp default)
		(> (length default) 0))
	   (setq prompt (concat prompt "(default " default ") ")))
	  (t
	    (setq default nil)))
    (cond ((string= (setq res
			  (completing-read prompt table pred force-p nil)) "")
	   (or default
	       (error "No default has been specified.")))
	  (t
	    res))))

;;; random routines

(defun pcmail-mode-setup (mode name keymap)
  "Generic routine for setting up pcmail modes.
Args: (mode name keymap)
  All pcmail buffers have common features, which this routine sets up.  MODE 
is the buffer's mode symbol, NAME is the buffer's mode name, and KEYMAP is
the buffer's key map."
  (kill-all-local-variables)
  (put mode 'mode-class 'special)
  (use-local-map keymap)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'version-control)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'file-precious-flag)
  (make-local-variable 'make-backup-files)
  (setq major-mode mode
	mode-name name
	local-abbrev-table text-mode-abbrev-table
	buffer-read-only t
	buffer-auto-save-file-name nil
	make-backup-files t		;rename, guarantees backup if disk full
	file-precious-flag t		;other I/O errors
	require-final-newline nil
	version-control 'never))

(defun pcmail-s-ending (n)
  "If N is 1, return the empty string, otherwise return \"s\".
Args: (n)"
  (if (= n 1) "" "s"))

(defun pcmail-save-buffer (&optional buf)
  "Save buffer object BUF, or current buffer if BUF is NIL.
Args: (&optional buf)
The only reason this function exists is that the normal save-buffer call
displays messages in the minibuffer like \"(no changes need to be saved)\",
which are distracting in the mail reader."
  (save-excursion
    (and buf 
	 (get-buffer buf) 
	 (set-buffer buf))
    (save-restriction
      (cond ((buffer-modified-p)
	     (widen)
	     (write-region (point-min) (point-max) buffer-file-name nil 'nomsg)
	     (clear-visited-file-modtime)
	     (set-buffer-modified-p nil))))
    t))

(defun pcmail-force-mode-line-update ()
  "Force a mode line update.
Args: none"
  (force-mode-line-update))

(defun pcmail-search-entry-list (name alist)
  "Return the list associated with NAME in ALIST.
Args: (name alist)"
  (assoc name alist))

(defun pcmail-in-sequence-p (thing seq)
  "Determine if a thing is in a sequence of such things.
Args: (thing seq)
  Return index of THING in SEQ if THING is EQUAL to an element in SEQ, nil
else.  SEQ may be either a list or a vector."
  (let ((i 0)
	(found))
    (while (and (not found) (< i (length seq)))
      (and (equal thing (cond ((vectorp seq) (aref seq i))
			      ((listp seq) (nth i seq))))
	   (setq found i))
      (setq i (1+ i)))
    found))

(defun pcmail-parse-space-list (s)
  "Turn a string of words separated by whitespace or commas into a list.
Args: s"
  (let ((l) (i 0))
    (while (string-match "\\([^ \t,]+\\)" s i)
      (setq l (cons (substring s (match-beginning 1) (match-end 1)) l)
	    i (match-end 1)))
    l))

(defun pcmail-format-string (format alist)
  "Format an arbitrary format string FORMAT using directive functions in ALIST.
Args: (format alist)
  FORMAT is a format string with embedded printf-style format directives.
ALIST is an association list.  Each alist element's car is a format character.
Each alist element's cadr is a function to call when the character is 
encountered following a percent sign.  The function is passed any length or
justification modifiers, together with a list of arguments which are the
alist element's caddr, if present.  The function may return a string or 
a number, which is concatenated appropriately onto the formatted output 
string.  The output string is returned."
  (let ((arglist) (directive) (start) (arg) (len)
	(outformat (copy-sequence format)) (charstart))
    (while (string-match "%\\(-?[0-9]*\\)\\([a-zA-Z]\\)" outformat start)
      (or (setq directive (pcmail-search-entry-list 
			   (substring outformat (match-beginning 2)
				      (match-end 2))
			   alist))
	  (error "Unknown format directive in \"%s\"" outformat))
      (setq len 
	    (string-to-int (substring outformat (match-beginning 1) 
				      (match-end 1)))
	    start (match-end 0)
	    charstart (match-beginning 2)
	    arg (apply (nth 1 directive) (nthcdr 2 directive)))
      (cond ((numberp arg)
	     (aset outformat charstart ?d))
	    (t
	     (setq arg (pcmail-justify-string arg len))
	     (aset outformat charstart ?s)))
      (setq arglist (append arglist (list arg))))
    (apply 'format outformat arglist)))

(defun pcmail-justify-string (s len)
  "Justify string S to LEN spaces, left if LEN is negative, right else.
Args: (s len)"
  (let ((abslen (if (> len 0) len (- len))))
    (cond ((zerop abslen)
	   s)
	  ((> (length s) abslen)
	   (substring s 0 abslen))
	  ((< len 0)
	   (concat s (make-string (- abslen (length s)) ? )))
	  ((> len 0)
	   (concat (make-string (- abslen (length s)) ? ) s)))))

(defun pcmail-make-uninteresting-fields-regexp ()
  "Make a pruning regexp from the fields in pcmail-uninteresting-fields-list.
Args: none"
  (and pcmail-uninteresting-fields-list
       (setq pcmail-uninteresting-fields-regexp
	     (concat "^"
		     (mapconcat 'identity pcmail-uninteresting-fields-list
				":.*\n\\([ \t]+.*\n\\)*\\|^")
		     ":.*\n\\([ \t]+.*\n\\)*"))))

(provide 'pcmail)
