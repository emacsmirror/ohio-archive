;From arpa-unix-emacs-request@ALEXANDER.BBN.COM Tue Nov 17 06:28:40 1987
;Received: from alexander by ALEXANDER.BBN.COM id aa01139; 16 Nov 87 22:55 EST
;Received: from [128.89.0.122] by ALEXANDER.BBN.COM id aa01135;
;          16 Nov 87 22:55 EST
;Received: from ucbvax.berkeley.edu by BBN.COM id aa11618; 16 Nov 87 22:54 EST
;Received: by ucbvax.Berkeley.EDU (5.58/1.26)
;	id AA02834; Mon, 16 Nov 87 19:41:12 PST
;Received: from USENET by ucbvax.Berkeley.EDU with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com)
;	(contact usenet@ucbvax.Berkeley.EDU if you have questions)
;Date: 17 Nov 87 01:20:35 GMT
;From: David Gudeman <gudeman@ARIZONA.EDU>
;Organization: U of Arizona CS Dept, Tucson
;Subject: completion with backup for GNU Emacs
;Message-Id: <2862@megaron.arizona.edu>
;Sender: unix-emacs-request@BBN.COM
;To: unix-emacs@BBN.COM
;
;This package is sort of based on a function that came over the net a
;couple of years ago.  It is pretty much unrecognizable now though,
;since I've speeded it up and ehanced it considerably.  It basically
;causes GNU Emacs to do command line completion like Unipress Emacs: it
;backs up if there are no completions, and exits if there is only one
;unique completion.
;
;I was planning to enhance it further to get it to use Unix-like ~name
;sequences where appropriate and to add a variable
;unexpanded-shell-variables, a list of shell variables (as in $xxx)
;that would remain unexpanded in displayed file names.  This is
;non-trivial, and I've put it off so long that I thought I might as
;well post this code as it is.  If anyone else implements these
;features, please let me know.
;------------------------------------------------------------
;;;;Bruce Israel <israel@tove.umd.edu>
;;;changed for version 17 by  Greg Earle
;;;			      ia-sun2!smeagol!earle@cit-vax.arpa (ARPA)
;;;David Gudeman <gudeman@arizona.edu> June 1987:
;;;  rewrote minibuffer-complete-exit-backup, fixed file-name completion
;;;  to work for directories, implemented completion within words, added
;;;  completion-auto-exit, minibuffer-complete-backup-undo, and the various
;;;  auxiliary functions.

(defvar completion-auto-exit nil
  "*If non-nil, (minibuffer-complete-exit-backup) exits automatically
when the current contents are a prefix of a unique solution.  For filename
completion, if this variable is t then ignored extensions are considered
solutions.  If this variable is not t or nil, then ignored extensions are
ignored completely by (minibuffer-complete-exit-backup).")

(defvar minibuffer-complete-backup-undo ""
  "String deleted in last minibuffer backup.")

(defun minibuffer-complete-exit-backup ()
  "Complete the item in the minibuffer backing up on failure.
If the item is unmatchable, then backup to the longest matchable prefix.
If point is not at the end of the minibuffer, try to complete only the part
of the minibuffer before point.  If the item in the minibuffer is an exact
unique match and completion-auto-exit is not nil, exit.  If completion is
ambiguous and completion-auto-help is non-nil, then pop up a help window.
Otherwise, complete the item as far as possible.  If this produces a unique
match and completion-auto-exit is non-nil, exit the minibuffer."
  (interactive)
  (let* ((p-min (point-min))
	 (file-comp (eq minibuffer-completion-table
			'read-file-name-internal))
	 (backed-up (if file-comp
			;; Filename completion.  Back up to a pathname.
			(minibuffer-check-directory p-min (point))))
	 (p-max (point))
	 (comp (try-completion
			     (buffer-substring p-min (point))
			     minibuffer-completion-table
			     minibuffer-completion-predicate)))
    (while (null comp)
      ;; There are no legal completions of the string.  Back up one char
      ;; at a time until there is a legal completion.
      (forward-char -1)
      (setq comp (try-completion
		  (buffer-substring p-min (point))
		  minibuffer-completion-table
		  minibuffer-completion-predicate)
	    backed-up t))
    (cond (backed-up
	   ;; The string was backed up.  Save the unmatched suffix for undo
	   ;; and delete it from the minibuffer.  Do not exit.
	   (setq minibuffer-complete-backup-undo
		 (buffer-substring (point) p-max))
	   (delete-region (point) p-max)
	   (and (eq comp t)
		(eobp)
		(temp-eob-message " [Confirm]")))
	  ((not (eobp))
	   ;; The completion was inside the string, not at the end.  Try to
	   ;; complete without exiting.
	   (if (eq comp t)
	       (temp-eob-message " [Complete prefix]")
	     (delete-region p-min p-max)
	     (insert comp)
	     (delete-double p-min)))
	  ((eq comp t)
	   ;; The string is matched exactly, exit or print a message.
	   (and completion-auto-exit
		(not (and file-comp
			  (eq t completion-auto-exit)
			  (cdr (all-completions
				(buffer-substring p-min p-max)
				minibuffer-completion-table
				minibuffer-completion-predicate))))
		(exit-minibuffer))
	   (temp-eob-message " [Complete]"))
	  ((= (length comp) (- p-max p-min))
	   ;; The string is a non-unique prefix, print a message.
	   (minibuffer-complete))
	  ((eq t (try-completion comp
				 minibuffer-completion-table
				 minibuffer-completion-predicate))
	   ;; The string is expanded to a unique completion (stored in
	   ;;  comp), insert it and either exit or print a message.
	   (delete-region p-min p-max)
	   (insert comp)
	   (and completion-auto-exit
		(not minibuffer-completion-confirm)
		(exit-minibuffer))
	   (temp-eob-message " [Confirm]"))
	  (t
	   ;; The expanded string is not a completion or not unique, just
	   ;;  insert the expansion.
	   (delete-region p-min p-max)
	   (insert comp)))))

(defun temp-eob-message (s)
  "Print a temporary message at the end of the current buffer.
After 2 seconds or when a key is typed, erase it."
  (let (p)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char (point-max))
	    (setq p (point))
	    (insert s))
	  (sit-for 2))
      (delete-region p (point-max)))))

(defun minibuffer-complete-backup-undo ()
  "Insert the last string deleted in a minibuffer backup."
  (interactive)
  (insert minibuffer-complete-backup-undo))

(defun minibuffer-check-directory (p1 p2)
  "Do file directory completion on the string between P1 and P2."
  (interactive "r")
  (let* ((s (expand-file-name (buffer-substring p1 p2)))
	 (d (or (file-name-directory s) ""))
	 (f (file-name-nondirectory s))
	 (h (getenv "HOME"))
	 backed-up)
    (while (not (or (string= d "") (file-exists-p d)))
      (setq s (substring d 0 -1)
	    d (or (file-name-directory s) "")
	    f (file-name-nondirectory s)
	    backed-up t))
    (delete-region p1 p2)
    (if (and (not (string= h "/"))
	     (eq 0 (string-match h d)))
	(insert "~" (substring d (match-end 0)) f)
      (insert d f))
    backed-up))

(defun delete-double (p-start)
  "Delete any buffer substring following point that is also a suffix of
the buffer substring between P-START and point."
  (let* ((p-center (point))
	 (p-end (+ p-center (- p-center p-start))))
    (while (and (< p-start p-center)
		(not (search-forward
		      (buffer-substring p-start p-center) p-end t)))
      (setq p-start (1+ p-start)
	    p-end (1- p-end)))
    (delete-region p-center p-end)))

(define-key minibuffer-local-must-match-map " "
  'minibuffer-complete-exit-backup)
(define-key minibuffer-local-completion-map " "
  'minibuffer-complete-exit-backup)
(define-key minibuffer-local-must-match-map "\C-_"
  'minibuffer-complete-backup-undo)
(define-key minibuffer-local-completion-map "\C-_"
  'minibuffer-complete-backup-undo)

