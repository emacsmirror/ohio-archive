;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maintain-lcd.el -- Useful functions for LCD maintaince work
;; RCSID           : $Header$
;; Author          : David Brennan (brennan@hal.com)
;; Created On      : Sat Dec 15 16:31:41 1990
;; Last Modified By: Dave Brennan
;; Last Modified On: Mon Mar 23 23:35:32 1992
;; Update Count    : 84
;; Status          : Works...mostly.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LCD Archive Entry:
;; maintain-lcd|David Brennan|brennan@hal.com
;; |Useful functions for LCD maintaince work
;; |92-03-23|2.13|~/misc/maintain-lcd.el.Z|

;;
;; Generally all you need to use is M-x lcd-process-buffer.

;; You may need to refill some paragraphs in beginning of mail messages
;; before finalizing the entry.

;; Recent Changes:

;; Wed Jul 24, 1991 (DJB)
;;
;; * Added parsing and handling of "LCD Archive Entry" info provided in
;;   files being procedded.  There may be problems with strangely or
;;   incorrectly formatted entries, but the format is rather flexible as
;;   suggested by Joe Wells.
;; * Give archiver a chance to add "LCD Archive Entry" to buffers before
;;   archiving if one doesn't exist.  If one does it is updated with the
;;   entry in the "*LCD Entry*" buffer and the archived is ask to OK it.

;; Tue June 4, 1991 (DJB)
;;
;; * Added ability to archive from a buffer not visiting a file
;;   (like a GNUS *Article* buffer!) or a read-only buffer
;; * Improved ability to process files on a remote host
;; * lcd-guess-date won't fail with ange-ftp if the date can't be
;;   parsed out of the article.
;; * Improved lcd-guess-name and lcd-guess-file-name heuristics
;;   by searching buffer for possible package names
;; * Added lcd comment regions, because lcd-comment-header often
;;   stops prematurely
;; * Added lcd-shadow-directory variable - its setting determines where or
;;   if archived files are shadowed locally


;; Check out the variables below for customizations.

;; TODO:
;;
;; * If item being archived is last entry in LCD-changes don't
;;   ask replacement question, and don't add replace LCD-changes
;;   entry instead of adding a new entry.
;; * Sometimes shar files contain multiple lisp files, but only on
;;   lisp file can contain the "LCD Archive Entry"  Maybe add some
;;   way to indicate in the entry that it is only part of a package
;;   so that people don't get ahold of the single file and not realize
;;   that something is missing.
;; * don't comment out headers if file contains .sh or .shar suffix
;;   (or maybe use a '#' comment)  (Can be done with pfx arg currently)
;; * some code to add an entry (with duplicate verifcation) without
;;   a corresponding file
;; * a nicer interface for editing lcd entries??
;; * some kind of save hook on the LCD-datafile so it isn't sorted for
;;   each addition (since it is sloooow)
;; * is there any kind of nice way to get the create date of a remove file
;;   with ange-ftp?  file-attributes seems useless

(require 'lispdir)
(require 'ange-ftp)
(require 'crypt)     ;; Expects Kyle Jones' crypt.el

(define-key emacs-lisp-mode-map "\C-c\C-p" 'lcd-process-buffer)

(defvar lcd-insert-marker-key "\C-cm"
  "Key temporarily bound to lcd-insert-formatted-entry in lcd-finalize-entry
if a marker isn't present in the lcd-working-buffer.  Look at the code below
or try it if this isn't clear.")

(defvar lcd-header-max 1000
  "Number of characters at the start of a buffer that will be
searched by the lcd-* functions for information.")

(defvar lcd-month-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" .10) ("Nov" .11) ("Dec" .12))
  "Association list of month strings and their month number.")

(defconst lcd-date-format "%02d-%02d-%02d"
  "Format string for LCD dates.  The order of arguments is currently fixed
at year, month, day, and are integers.")

(defvar lcd-archive-directories
  '("as-is" "epoch" "functions" "games" "interfaces" "misc" "modes"
    "packages" "terms" "patches")
"List of directories in the LCD archive.  They are used for completion
when an archive directory name is needed.")

(defconst lcd-suffixes-regexp "\\.el$\\|\\.el\\.Z$"
 "Regexp of suffixes to strip from filnames when generating the package name.")

(defconst lcd-useless-headers-regexp
  "^From \\|^Article \\|^Message-ID: \\|^Sender: \\|^Distribution: \\|^Lines: \\|^CC: \\|^Posted-Date: \\|^To: \\|^Originator: \\|^Xref:"
"Regexp of headers that don't need to be kept in archived files.")

(defconst lcd-name-regexp
  "\\([a-zA-Z0-9_-]+\\)\\.el"
  "Regexp for matching possible package names.  \1 should be the name without
any suffix.")

(defvar lcd-target-buffer nil
  "*Buffer in which lcd-generate-and-insert-entry will insert an LCD entry.")

(defconst lcd-entry-edit-buffer "*LCD Entry*"
  "Buffer used for displaying and editing new LCD entries.")

(defconst lcd-archive-file-buffer "*LCD Archive File*"
  "Buffer used to process buffers which are read-only.")

(defvar lcd-archive-path
  "/brennan@python.cis.ohio-state.edu:"
  "*Path identifing the Emacs lisp archive.")

(defvar lcd-working-directory "/brennan@python.cis.ohio-state.edu:"
  "*Directory prefix of the local LCD maintain files.")

(defvar lcd-shadow-directory nil
  "*Directory prefix of the local archive shadow.  If nil working files
will be temporarily written in lcd-temp-directory.")

(defvar lcd-temp-directory "/tmp/"
  "*Directory to which temporary files are written.")

(defvar lcd-datafile (concat lcd-working-directory "LCD-datafile.Z")
  "*LCD datafile used by maintain functions.")

(defvar lcd-update-file (concat lcd-working-directory "LCD-changes")
  "*Name of the file used to record LCD updates.")

(defconst lcd-entry-marker "^\\(.*\\)LCD Archive Entry:[ \t]*"
  "Regexp marker tag for archive entry in archived code. \1 should be the
comment leader after the search.")

(defvar lcd-entry-marker-pos nil
  "Marker set to before the next LCD field in the buffer of the file being
archived.")

(defconst lcd-entry-text "LCD Archive Entry:"
  "Text version (ie: no regexp stuff) of lcd-entry marker")

(defconst lcd-update-marker "---- next updated entry ----"
  "String in lcd-update-file at which entries changed in lcd-datafile will
be listed.")

(defconst lcd-new-marker "---- next new entry ----"
  "String in lcd-update-file at which entries added to lcd-datafile will
be listed.")

(defvar lcd-raw-apropos-buffer "*LCD Raw Apropos*"
  "*Buffer in which lcd-raw-apropos will place it's output.")

(defvar lcd-keep-buffers nil
  "*Set to non-nil to keep working file and lcd temporary buffers after
an entry is finalized.  If nil they will all be killed on finalize.
The lcd-datafile and lcd-update-file buffers will never be killed.")

(defvar lcd-working-buffer nil
  "The buffer currently being worked on.")

(defvar lcd-working-file nil
  "File visited by buffer lcd-process-buffer is run in.")

(defvar lcd-last-entry ""
  "String containing the last lcd entry generated.")

(defvar lcd-saved-window-configuration nil
  "Window configuration when lcd-process-buffer is called.  Used to restore
the window configuration after lcd-finalize-entry is run.")

(defvar lcd-format-function 'lcd-format-for-mail
  "Function used by lcd-format-region to format raw LCD entries.")

(defvar lcd-mail-intro
"Here is the lisp formatted LCD (Lisp Code Directory) entry and archive path
for the code which has been added to the Emacs lisp archive on
archive.cis.ohio-state.edu:
"
"Introduction for mail message to author of E-lisp code.")

(defvar lcd-mail-closing
"You will make the job of the archive maintainers easier if you include the
above entry in future revisions of your code.  Also, you can help users
distinguish between different version of your code by providing a version
string in the entry if you do not already do so.  For more information,
please see the file \"guidelines\" in the elisp-archive directory on archive.
I can mail you a copy if you cannot ftp.

Finally, if you have a more up-to-date version of this code or have any
corrections for the entry please let me know.  If there are no changes now
I'd appreciate being notified of future revisions.

Thanks,"
  "Closing for mail message to author of E-lisp code.")

;;;
;;; End of variables section
;;;

(defun match-substring (&optional count)
  "Return the text matched by the last regexp search.  Optional ARG, a
number, specifies that the ARGth parenthesized expression should be
returned.  If ARG is missing or zero the entire text matched is returned.
If there are less than ARG expressions nil is returned."
  (if (not count)
      (setq count 0))
  (buffer-substring (match-beginning count) (match-end count)))

(defun lcd-generate-names ()
  "Returns a list of possible names for elisp in the current buffer.
The cdr of each list element is the number of times it appeared in
the buffer.  The list is sorted from most frequent occurrence to
least frequent.  Elements which occured the same number of times
are in order of occurence."
  (save-excursion
    (goto-char 1)
    (let (names)
      (while
	  (re-search-forward lcd-name-regexp nil t)
	(let* ((name (match-substring 1))
	      (element (assoc name names)))
	  (if element
	      (setcdr element (1+ (cdr element)))
	    (setq names (cons (cons name 1) names)))))
      ;; do a special search for "provide" with special weighting
      (goto-char 1)
      (if (re-search-forward "(provide '\\([a-zA-Z0-9_-]+\\))" nil t)
	  (let* ((name (match-substring 1))
		 (element (assoc name names)))
	    (if element
		(setcdr element (+ 1000 (cdr element)))
	      (setq names (cons (cons name 1000) names)))))
      ;; reverse list to occurence order
      (setq names (nreverse names))
      ;; sort by number of occurrences
      (sort names '(lambda (el1 el2) (> (cdr el1) (cdr el2)))))))


(defun lcd-get-next-field ()
  "Get the next author provided field in the region indicated by the
lcd-entry-marker regexp."
  (if lcd-entry-marker-pos
      (save-excursion
	(goto-char lcd-entry-marker-pos)
	;; grab from point to next '|' or end of line and move pos forward
	(if (re-search-forward "[^|\n]*" nil t)
	    (progn
	      (if (= (char-after (point)) ?|)
		  (forward-char 1))
	      (setq lcd-entry-marker-pos (point))
	      (if (string-equal (match-substring 0) "")
		  nil
		(match-substring 0)))))))


(defun lcd-guess-name ()
  "Try to figure out the package name of the current buffer.  Base guess
on number of occurences of lcd-name-regexp in buffer and the name of the
file.  If the name of the file without suffixes matches one of the names
from lcd-generate-names it is used, and no prompting is done.  Otherwise
the user is prompted for a name with completing-read, using the list from
lcd-generate-names.  The first name in the list is used as the default
response."
  (or (lcd-get-next-field)
      (let* ((name (lcd-guess-file-name))
	     (pos (string-match lcd-suffixes-regexp name))
	     (name (if pos (substring name 0 pos) name))
	     (names (lcd-generate-names))
	     (match (assoc name names)))
	(if match
	    (car match)
	  (let* ((default (car (car names)))
		 (pick (completing-read
			(if default
			    (format "Package name [%s]: " default)
			  "Package name: ")
			names)))
	    (if (equal pick "")
		default
	      pick))))))


(defun lcd-guess-author ()
  "Try to figure out the name of the author of the elisp in the current buffer.
Currently this function only knows about mail \"From:\" headers."
  (or (lcd-get-next-field)
      (save-excursion
	(goto-char 1)
	(cond
	 ;; for address of the form "contact@foobar (Author's Name)"
	 ((re-search-forward "From:.*(\\(.*\\))" lcd-header-max t)
	  (match-substring 1))
	 ;; for address of the form "Author's Name <contact@foobar>"
	 ((re-search-forward "From: \\(.*\\) <.*>" lcd-header-max t)
	  (match-substring 1))
	 ;; for non-parsable addresses
	 (t "")))))

(defun lcd-guess-contact ()
  "Try to figure out the email address of the author of the elisp in the
current buffer.  Currently this function only knows about mail \"From:\"
headers."
  (or (lcd-get-next-field)
      (save-excursion
	(goto-char 1)
	(cond
	 ;; for address of the form "contact@foobar (Author's Name)"
	 ((re-search-forward ";? ?From: \\(.*\\) (.*)" lcd-header-max t)
	  (match-substring 1))
	 ;; for addresses of the form "Author's Name <contact@foobar>"
	 ((re-search-forward "From: .* <\\(.*\\)>" lcd-header-max t)
	  (match-substring 1))
	 (t "")))))

(defun lcd-uncompressed-file-name ()
  "Return the file name of the current buffer without the \".Z\" suffix,
if present."
  (let* ((name (lcd-guess-file-name))
	(pos (string-match "\\.Z$" name)))
    (if pos
	(substring name 0 pos)
      name)))

(defun lcd-guess-description ()
  "Try to figure out a description string for the elisp in the current buffer.
The following strategies are used:

 * Search for \"name -- <description>\"
 * Search for \"Description: <description>\"
"
  (or (lcd-get-next-field)
      (save-excursion
	(goto-char 1)
	(cond
	 ((re-search-forward (concat ";;;? "
				    (regexp-quote (lcd-uncompressed-file-name))
				    " ---? \\(.*\\)")
			     (point-max) t)
	  (match-substring 1))
	 ((re-search-forward "Description:[ \t]*\\(.*\\)" (point-max) t)
	  (match-substring 1))
	 ( t "")))))

(defun lcd-guess-date ()
  "Try to figure out the mailed or last changed date of the elisp in the
current buffer.  This function first looks for a \"Date:\" header, then
the creation date of the file, if available."
  (or (lcd-get-next-field)
      (save-excursion
	(goto-char 1)
	(cond
	 ((re-search-forward
	   "Date: \\([A-Z][a-z][a-z], \\)?\\([0-9][0-9]?\\) \\([A-Z][a-z][a-z]\\) \\([0-9][0-9]\\)"
	   lcd-header-max t)
	  (format lcd-date-format
		  (string-to-int (match-substring 4))                ; year
		  (cdr (assoc (match-substring 3) lcd-month-alist))  ; month
		  (string-to-int (match-substring 2))                ; day
		  ))
	 
	 ;; only try this on the local system - won't work with ange-ftp
	 ((not (string-match (car ange-ftp-path-format)
			     (buffer-file-name)))
	  (let ((file-name (buffer-file-name)))
	    (set-buffer (get-buffer-create " *LCD Capture Date*"))
	    (erase-buffer)
	    (shell-command (concat "sls -p '%c\"%y-%m-%d\"' " file-name) t)
	    (setq result (buffer-substring (point) (- (mark) 1)))
	    (kill-buffer (current-buffer))
	    result))
	 (t "")))))

(defun lcd-guess-version ()
  "Try to figure out a version string for the elisp in the current buffer.
Currently this functions returns an empty string."
  (or (lcd-get-next-field)
      ""))

(defun lcd-guess-file-name ()
  "Try to figure out the file name of the elisp in the current buffer.
This function assumes the file name of the buffer with a \".Z\"
appended if necessary.  If the buffer is not associated with a file
the package name will be used if this function was called from
lcd-build-entry."
  (let ((full-name (buffer-file-name)))
    (if full-name
	(file-name-nondirectory
	 (if (string-match "\\.Z$" full-name)
	     full-name
	   (concat full-name ".Z")))
      ;; name is set in lcd-build-entry
      (if (boundp 'name)
	  (concat name ".el.Z")
	""))))

(defun lcd-guess-archive ()
  "Try to guess the archive name of the elisp in the current buffer."
  (or (lcd-get-next-field)
      (concat
       "~/"
       (completing-read "Classification: "
			(mapcar 'list lcd-archive-directories))
       "/"
       (lcd-guess-file-name))))

(defun lcd-one-lineify ()
  "Make the LCD entry in the current buffer one line.  Returns nil if the
buffer does not contain an entry, the comment leader otherwise.  Leaves
the point at the start of the line containing the entry."
  (let (leader old-syntax (end-marker (make-marker)))
  (goto-char 1)
  (if (re-search-forward lcd-entry-marker nil t)
      (progn
	(setq leader (match-substring 1))
	;; go to start of next line if the rest of this one is blank
	(if (looking-at "\n")
	    (goto-char (match-end 0))
	  (insert ?\n))
	(setq old-syntax (char-to-string (char-syntax ?|)))
	(modify-syntax-entry ?| "w")
	(insert ?\n)
	;; get rid of comments and concat lines
	;; Find the next line after the 7th "|" or the first
	;; line with a different leader - whichever comes first.
	(save-excursion
	  (while (looking-at "^;; .")
	    (forward-line 1))
	  (set-marker end-marker (point)))
	(save-excursion
	  (if (search-forward "|" nil 'no-error 7)
	      (progn
		(forward-line 1)
		(beginning-of-line)
		(if (< (point) (marker-position end-marker))
		    (set-marker end-marker (point))))))
	(while (re-search-forward (concat "^" leader "\\<") end-marker t)
	  ;; delete comment leader and merge with previous line
	  (delete-region (1- (match-beginning 0)) (match-end 0))
	  (forward-line 1))
	(forward-line -1)
	(modify-syntax-entry ?| old-syntax)
	(insert leader)
	leader))))

(defun lcd-build-entry ()
  "Generate an LCD datafile entry for the current buffer, filling in as
many fields as possible.  The format this function generates is
Name|Author|Contact (Email)|Description|Date|Version|Archive."

  ;; Look for author supplied entry and set position and comment leader
  ;; if found.
  (setq lcd-entry-marker-pos (if (lcd-one-lineify) (point)))
      
  (let* ((name         (lcd-guess-name))
	 (author       (lcd-guess-author))
	 (contact      (lcd-guess-contact))
	 (description  (lcd-guess-description))
	 (date         (lcd-guess-date))
	 (version      (lcd-guess-version))
	 (archive      (lcd-guess-archive)))
    (format "%s|%s|%s|%s|%s|%s|%s"
	    name
	    author
	    contact
	    description
	    date
	    version
	    archive)))

(defun lcd-generate-and-insert-entry ()
  "Generate a best guess LCD datafile entry for the current buffer, and
insert it in the buffer lcd-target-buffer.  The entry is inserted at the
current line if it is empty, or after the current line if it contains text."
  (interactive)
  (let ((lcd-entry (lcd-build-entry)))
    (progn
      (set-buffer lcd-target-buffer)
      ;; if the point is not at the start of a blank line make a new line
      (if (not (and (= (current-column) 0)
		    (or (= (following-char) ?\n) (= (following-char) 0))))
	  (progn (end-of-line) (insert ?\n)))
      (insert lcd-entry)
      (if (get-buffer-window lcd-target-buffer)
	  (set-window-point (get-buffer-window lcd-target-buffer) (point))
	))))

(defun lcd-raw-apropos (topic)
  "Find raw lisp code directory entries in lcd-file matching TOPIC."
  (interactive (list
		(read-string
		 (concat "Raw apropos (" (current-word) "): "))))
  (if (equal "" topic) (setq topic (current-word)))
  (save-excursion
    (set-buffer (get-buffer-create lcd-raw-apropos-buffer))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (lisp-dir-insert-datafile)
    (delete-non-matching-lines topic)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t))
  (display-buffer lcd-raw-apropos-buffer))

(defun lcd-comment-header ()
  "Comment out any header information in a Emacs lisp file."
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "^[ \t]*\\(;\\|(\\)"))
      (insert-string "; ")
      (forward-line 1)
      (beginning-of-line))))

(defun lcd-comment-region (start end)
  "Comment out the region with \"; \".  Called from a program takes
arguments START and END."
  (interactive "r")
  (let ((end-marker (make-marker)))
    (set-marker end-marker end)
    (goto-char start)
    (beginning-of-line)
    (insert-before-markers "; ")
    (while (search-forward "\n" (marker-position end-marker) 1)
      (insert-before-markers "; "))))

(defun lcd-prune-headers ()
  "Remove lines between the beginning of the buffer and lcd-header-max
that match lcd-useless-headers-regexp."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward lcd-useless-headers-regexp lcd-header-max t)
      (beginning-of-line)
      (kill-line 1))))

;; this really should be interactive (data available in lcd-last-entry)

(defun lcd-mail-author (author package)
  (let ((saved-mail-archive-file-name mail-archive-file-name)
	(region-start))
    ;; Don't want message "FCC"ed to some file...
    (setq mail-archive-file-name nil)
    (mail nil author (concat "Your E-lisp code (" package
			     ") has been added to the archive."
    "\nFrom: elisp-archive@cis.ohio-state.edu (Elisp Archive Maintainer)"))
    (end-of-buffer)
    (insert lcd-mail-intro "\n")
    (setq region-start (point))
    (insert lcd-last-entry)
    (lcd-format-region region-start (point))
    (insert "\n\n" lcd-mail-closing "\n")
    ;; (mail-signature)
    (setq mail-archive-file-name saved-mail-archive-file-name)))

(defun lcd-log-entry (name-and-author entry)
  "Log the specified entry in the lcd-datafile and the lcd-update-file."
  (let ((entry-marker lcd-new-marker))
    (save-excursion
      ;; look for a duplicate in lcd-datafile
      (set-buffer (find-file-noselect lcd-datafile))
      (goto-char (point-min))
      ;; try the search, if it works ask the question
      ;; if either one fails add it and sort
      (or (if (search-forward name-and-author (point-max) t)
	      (save-excursion
		(find-file lcd-datafile)
		(beginning-of-line)
		(set-window-start (selected-window) (point))
		(if (yes-or-no-p "Replace this entry? ")
		    (progn
		      (setq entry-marker lcd-update-marker)
		      (kill-line 1)
		      (insert entry)
		      t))))
	  ;; no match or "no" answer, so just add it and sort
	  (save-excursion
	    (set-buffer (find-file-noselect lcd-datafile))
	    (goto-line 2)
	    (insert entry)
	    (previous-line 1)
	    (sort-lines nil (point) (point-max)))))
    ;; add the entry to the lcd-update-file
    (save-excursion
      (set-buffer (find-file-noselect lcd-update-file))
      (goto-char (point-min))
      (if (search-forward entry-marker (point-max) t)
	  (progn
	    (beginning-of-line)
	    (insert entry))
	(progn
	  (message "Couldn't find marker string %s in %s." entry-marker
		   (file-name-nondirectory lcd-update-file))
	  (sit-for 2))))))

;; this routine needs to update the marker if it already exists!

(defun lcd-entry-marker-check ()
  "Check the lcd-working-buffer for an \"LCD Archive Entry:\" marker, asking
user for verification if it isn't there."
  (save-excursion
    (let (result comment)
      (set-buffer lcd-working-buffer)
      (beginning-of-buffer)
      ;; replace it if found
      (if (setq comment (lcd-one-lineify))
	  (progn
	    (forward-line -1)    ; move back to "LCD Archive Entry" line
	    (kill-line 2)
 ;;	    (insert ?\n)
	    (lcd-insert-formatted-entry comment)
	    (set-window-point (display-buffer (current-buffer)) (point))
	    (yes-or-no-p "Does this look OK? "))
	;; else let the user insert
	(let ((saved-binding (key-binding lcd-insert-marker-key)))
	  ;; switch binding for a while
	  (local-set-key lcd-insert-marker-key 'lcd-insert-formatted-entry)
	  (setq result
		(yes-or-no-p
		 "No entry marker found (insert with C-c m).  Contunue? "))
	  ;; restore previous binding
	  (local-set-key lcd-insert-marker-key saved-binding)
	  result)))))


(defun lcd-finalize-entry (arg)
  "Merge the LCD entry in the buffer \"*LCD Entry*\" into lcd-datefile and
make an appropriate entry in lcd-update-file-name."
  (interactive "P")
  (if (lcd-entry-marker-check)
  (progn
  (let (name name-and-author new-file-name contact archive-path)
    (save-excursion
      (set-buffer lcd-entry-edit-buffer)
      (goto-line 2)
      ;; grab some info
      (re-search-forward "^\\(.*\\)|\\(.*\\)|\\(.*\\)|.*|.*|.*|\\(.*\\)")
      (setq lcd-last-entry (concat (match-substring 0) "\n")
	    name (match-substring 1)
	    name-and-author (concat name "|" (match-substring 2))
	    contact (match-substring 3)
	    archive-path (match-substring 4))
      (setq new-file-name
	    ;; drop file in the shadow directory if specified
	    (if lcd-shadow-directory
		(concat lcd-shadow-directory (substring archive-path 2))
	      ;; otherwise use a temp file
	      (concat lcd-temp-directory "lcd-"
		      (file-name-nondirectory archive-path)))))
    
    ;; now log the entry to the appropriate files
    (lcd-log-entry name-and-author lcd-last-entry)

    ;; write out the file and move it to the proper place
    (set-buffer lcd-working-buffer)

    ;; put buffer in compress mode if it isn't already and should be
    (if (string-match "\\.Z$" new-file-name)
	(compress-mode 1))

    ;; finally save it
    ;; (this will probably leave an auto-save file, but that's ok)
    ;; buffer will also now visit the new file
    
    (write-file new-file-name)
    (if (and lcd-working-file (file-readable-p lcd-working-file))
	(delete-file lcd-working-file))
    
    ;; update dired as well
    (if (and lcd-working-file (fboundp 'dired-remove-entry-all-buffers))
	(dired-remove-entry-all-buffers lcd-working-file))

    ;; send the file to the archive (this depends on having ange-ftp!)
    ;; confirmation is requsted if the file already exists

    ;; this code should probably abort nicely if the user doesn't confirm
    ;; for now it doesn't hurt to abort here
    ;; the confirmation request is usually impossible to read because of
    ;; the line length, so a specific check would be better
    ;; however, the check does slow things down, because we need to do
    ;; an ls of the dir twice, so I'll leave it alone for now

    (if lcd-archive-path
	(let ((archive-name (concat lcd-archive-path
				    (substring archive-path 2))))
	(copy-file new-file-name archive-name 1 t)))

    ;; if new-file is a temp file delete it, else update dired
    (if (equal lcd-temp-directory (file-name-directory new-file-name))
	(delete-file new-file-name)      
      (if (fboundp 'dired-add-entry-all-buffers)
	  (dired-add-entry-all-buffers (file-name-directory new-file-name)
				      (file-name-nondirectory new-file-name))))

    ;; make it so dired buffer appears after mail is sent
    ;; this assumes that we were using dired
    (if lcd-working-file
	(set-buffer (find-file-noselect
		     (file-name-directory lcd-working-file))))
		     
    ;; send mail to the author
    (lcd-mail-author contact name)

    ;; finally back to the way things were
    (set-window-configuration lcd-saved-window-configuration)

    (if lcd-keep-buffers
	nil
      (kill-buffer lcd-working-buffer)
      (kill-buffer lcd-entry-edit-buffer)
      (kill-buffer lcd-raw-apropos-buffer))))))

(defun lcd-process-buffer (&optional arg)
  "Process the current buffer for submission into the Emacs lisp archive
and create and LCD entry for the LCD data file.  Prefix arg means do not
try to comment out the header.  [more docs needed]"
  (interactive "P")
  (setq lcd-saved-window-configuration (current-window-configuration))
  (if (equal "*Subject*" (buffer-name))
      (switch-to-buffer "*Article*"))
  (if (equal "*Article*" (buffer-name))
      (let ((gnus-Article-prepare-hook nil))
	(gnus-Article-show-all-headers)
	(delete-other-windows)))

  ;; copy buffer to editable buffer if original is read-only
  
  (if buffer-read-only
      (let ((edit-buffer (get-buffer-create lcd-archive-file-buffer)))
	(save-excursion
	  (set-buffer edit-buffer)
	  (erase-buffer))
	(save-restriction
	  (widen)
	  (copy-to-buffer edit-buffer (point-min) (point-max)))
	(buffer-flush-undo edit-buffer)
	(buffer-enable-undo edit-buffer)
	(switch-to-buffer edit-buffer)
	(if buffer-read-only (toggle-read-only))))

  ;; go to work on the entry buffer

  (let ((entry (lcd-build-entry))
	pattern)
    (lcd-prune-headers)
    (if (not arg)
	(lcd-comment-header))
    (setq lcd-working-file (buffer-file-name)
	  lcd-working-buffer (current-buffer))

    ;; set up the buffer to edit the new lcd entry

    (split-window-vertically 12)
    (switch-to-buffer lcd-entry-edit-buffer)
    (fundamental-mode)
    (local-set-key "\C-c\C-c" 'lcd-finalize-entry)
    (local-set-key "\t" 'lcd-edit-next-field)
    (local-set-key "\e\t" 'lcd-edit-previous-field)
    (modify-syntax-entry ?| " ")
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (buffer-enable-undo (current-buffer))
    (insert
     "Name | Author | Contact | Description | Date | Version | Archive\n"
     entry)

    ;; get the first two elements of ENTRY and search for them in current LCD

    (beginning-of-line)
    (re-search-forward "^[^|]*")   ; can't fail
    (setq pattern (match-substring 0))
    (if (equal pattern "")
	(setq pattern nil))
    (forward-char 1)			; skip over '|'
    (re-search-forward "[^|]*")
    ;; if there's no author name, leave it out
    (if (not (equal (match-substring 0) ""))
	(setq pattern (concat pattern (if pattern "\\|") (match-substring 0))))
    (beginning-of-line)

    ;; display any matches in another window
    ;; this helps the user avoid duplicat names

    (split-window-vertically 5)
    (if pattern
	(lcd-raw-apropos pattern))
    (beginning-of-line)
    (message "Edit lisp file and/or LCD entry.  Press C-c C-c when done.")
    ))

(defun lcd-edit-next-field ()
  "Goto the start of the next field in the lcd edit buffer."
  (interactive)
  (search-forward "|" nil 'goto-end))

(defun lcd-edit-previous-field ()
  "Goto the start of the next field in the lcd edit buffer."
  (interactive)
  (backward-char 1)
  (if (re-search-backward "|\\|\n" nil 'goto-start)
      (forward-char 1)))

(defun lcd-format-region (start end)
  "Format all LCD entries in region to a human readable format.  When used
from a program the arguments START and END are required.  Adapted to work
on non-empty starting buffers."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char start)
    (beginning-of-line)
    (while (re-search-forward
       "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (match-substring 1))
	    (author (match-substring 2))
	    (contact (match-substring 3))
	    (description (match-substring 4))
	    (date (match-substring 5))
	    (version (match-substring 6))
	    (archive (match-substring 7)))
	(delete-region
	 (progn (beginning-of-line) (point))
	 (progn (end-of-line) (point)))
	(funcall lcd-format-function
		 name author contact description date version archive)))))

(defun lcd-insert-formatted-entry (&optional comment-leader)
  "Insert at the point the formatted version of the LCD entry in
the lcd-entry-edit-buffer."
  (interactive)
  (let ((lcd-format-function 'lcd-format-for-lisp-code)
	entry start)
    (save-excursion
      (set-buffer lcd-entry-edit-buffer)
      (goto-line 2)
      (setq start (point))
      (end-of-line)
      (setq entry (concat (buffer-substring start (point)) "\n")))
    (beginning-of-line)
    (setq start (point))
    (insert entry)
    (lcd-format-region start (point))))

(defun lcd-format-for-change-buf (start end)
  "Format all LCD entires in the region using the lcd-format-lcd-line
function.  This will move to where the change entry actually gets done.
This function exists for manual testing."
  (interactive "r")
  (let ((lcd-format-function 'lcd-format-lcd-line))
    (lcd-format-region start end)))

(defun lcd-format-lcd-line
  (name author contact description date version archive)
  "Multi-line non-columnar line formatter for LCD maintainer.
Adapted from Dave Sill's version."
  (insert-at-column 0 name)
  (if (not (equal version ""))
      (insert "  (Version " version ")")
    (insert "  (version unknown)"))
  (insert "  " date)
  (insert "\n")
  (if (and (string-match "[0-9]+\.[0-9]+ dist" contact)
	   (equal author "FSF"))
      (insert-at-column 2 contact)
    (progn
      (insert-at-column 2 author)
      (insert ", <" contact ">\n")
      (if (not (equal archive ""))
	  (progn
	    (if (string-match "~" archive)
		(setq archive (concat "Archived in: " elisp-archive-directory
				      (substring archive 2))))
	    (insert-at-column 2 archive)))))
  (insert-at-column 2 description))

(defun lcd-build-entry-for-lisp-code
  (prefix name author contact description date version archive)
  "Formatter for entry suitable for inclusion in an Emacs lisp source
code file."
  (concat 
   prefix lcd-entry-text "\n"
   prefix name "|" author "|" contact "|" "\n"
   prefix description "|" "\n"
   prefix date "|"
   (if version version "(no version)") "|" archive "|"))
  
(defun lcd-format-for-mail
  (name author contact description date version archive)
  "Formatter for entry suitable for inclusion in mail to the author
of the code."
  (insert (lcd-build-entry-for-lisp-code
	   ";; " name author contact description date version archive))
  (if (not (equal archive ""))
      (progn
	(if (string-match "~" archive)
	    (insert (concat "\n\nArchived in: " elisp-archive-directory
			    (substring archive 2)) ?\n)))))

(defun lcd-format-for-lisp-code
  (name author contact description date version archive)
  "Formatter for entry suitable for inclusion in an Emacs lisp source
code file."
  (insert (lcd-build-entry-for-lisp-code
	   (if (and (boundp 'comment-leader) comment-leader)
	       comment-leader ";; ")
	   name author contact description date version archive)))

(provide 'maintain-lcd)

;; Local Variables:
;; kept-old-versions: 0
;; kept-new-versions: 4
;; End:
