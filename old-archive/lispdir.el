;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lispdir.el --- Lisp Code Directory formatter and apropos
;; Authors         : Ashwin Ram (Ram-Ashwin@cs.yale.edu)
;;                 ; Dave Sill (de5@ornl.gov)
;;                 ; David Lawrence (tale@uunet.uu.net)
;;		   ; Noah Friedman (friedman@ai.mit.edu)
;;		   ; Joe Wells (jbw@cs.bu.edu)
;;                 ; Dave Brennan (brennan@gnu.ai.mit.edu)
;;		   ; Eric Raymond (eric@snark.thyrsus.com)
;;		   ; Daniel Quinlan (quinlan@gnu.ai.mit.edu)
;; Created On      : Wed Jan 25, 1989
;; Last Modified By: Dave Brennan
;; Last Modified On: Mon Mar 24 1997
;; Update Count    : 58
;; Status          : No known bugs.
;; Version         : 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;- Add the following lines to ~/.emacs or an equivalent  (w/o  ";;-"  !):
;;
;;-  (autoload 'format-lisp-code-directory "lispdir" nil t)
;;-  (autoload 'lisp-dir-apropos "lispdir" nil t)
;;-  (autoload 'lisp-dir-retrieve "lispdir" nil t)
;;-  (autoload 'lisp-dir-verify "lispdir" nil t)
;;
;; Other routines of interest to programmers:
;;
;;	insert-lcd-headers
;;	submit-lcd-entry
;;
;;See the doc strings of the individual functions for more documentation.

;; History

;; 24-Mar-1997		Dave Brennan
;;    Wrapped definition of current-word so that it is not defined in
;;    newer version of Emacs, which already have it.
;; 05-Jan-1996		Dave Brennan
;;    Update to use .gz on LCD-datafile.  Also need to update to use .gz
;;    on files in the archive so that I can convert them all.
;; 24-Sep-1995          Daniel Quinlan
;;    Backquote use of " inside of a string.  Don't delete original
;;    tar file buffers.  Keep 'em around.

;; 09-Oct-1994          Daniel Quinlan
;;    Don't require crypt if jka-compr is already in use.  jka-compr.el
;;    became a standard part of Emacs as of version 19.23 so we also
;;    test the version number of Emacs.

;; 09-Mar-1993          Joe Wells <jbw@cs.bu.edu>, Dave Brennan
;;    Added code to break long archive name lines between the hostname and
;;    the filename to avoid exceeding lisp-dir-fill-column characters.

;; 23-Jun-1992		Dave Brennan
;;    Added strategic "(require 'crypt)" statements as suggested by
;;    Martin Boyer.

;; 23-Mar-1992		Dave Brennan
;;    Just use the file name in the variable lisp-code-directory instead
;;    of screwy " *LCD-datafile*" name.  Merged in Eric's changes from
;;    November.  (See next entry.)

;; 26-Nov-1992		Eric Raymond
;;    Added insert-lcd-headers, submit-lcd-entry.  Isolated access-method code.
;;    Introduced lcd-locations variable.  Added progress indication to format
;;    so the hapless luser doesn't just see a frozen screen for 10 minutes.
;;    Added entry point list to header for the enlightenment of newbies.

;; 22-Aug-1991		Dave Brennan
;;    Only load LCD-datafile when it is first referenced to make it faster.
;;    To force a reload kill the buffer " *LCD-datafile*" (note the space).

;; 03-Aug-1991		Dave Brennan
;;    If no matching entry is found print message in minibuffer instead of
;;    displaying an empty apropos buffer.  If the apropos buffer exists
;;    it is not modified.

;; 19-Jul-1991		Dave Sill
;;    Added lisp-code-retrieve and lisp-code-verify using ange-ftp.

;; 18-Jul-1991		Dave Sill
;;    Added "Archive" to Ram/Tale header. (Joe Wells <jbw@maverick.uswest.com>)

;; 12-Jul-1991		Noah Friedman (friedman@ai.mit.edu)
;;    Modified format-lisp-code-directory to use find-file-noselect in
;;    another buffer, then insert-buffer.  The reason for this is that
;;    insert-file has no hooks, and so cannot (for example) uncompress
;;    a compressed file.  This loses if I want to grab LCD-datafile.gz
;;    using ange-ftp.

;; 20-Jun-1991		de5
;;    Mostly cosmetic changes in prompts and buffer names.

;; 27-Jun-1989		dsill
;;    Added support for "archive" field containing anonymous FTP location.

;; 28-Feb-1989		dsill
;;    Changed format-lcd-line-Sill to be smart about GNU-distributed code.
;;    Changed format-lcd-line-Sill to take advantage of 12-char max name.

;; 22-Feb-1989		dsill
;;    Changed format-lisp-code-directory and lisp-dir-apropos to call the
;;      line formatter indirectly.  The variable
;;      format-lisp-code-directory-line contains a function to format a single
;;      line, and format-lcd-line-Ram, format-lcd-line-tale, and
;;      format-lcd-line-Sill are the three possibilities at this time.

;; 20-Feb-1989		tale
;;    changed file's name to lispdir.el
;;    format-lisp-code-directory makes separate buffer
;;    removed lisp-dir-apropos-buffer -- why use more space in memory?
;;    added lisp-dir-[apropos-]hook
;;      (I like (setq lisp-dir-hook 'delete-other-windows))
;;    other aesthetic changes

;; 16-Feb-1989		dsill
;;    Added lisp-dir-apropos function

(require 'picture)			;provides move-to-column-force

;; *** WARNING *** WARNING *** WARNING *** WARNING *** WARNING ***
;; You'll need ange-ftp and crypt (for uncompressing) to use the name
;; below.  For better performance put the LCD-datafile on a local sytem.
;; Do not use uncompress.el because it (incorrectly) renames a buffer
;; after uncompressing it, which means that each apropos will wind up
;; ftping the LCD-datafile again.  Use Kyle Jones' crypt code which
;; is also available from the archive.
;;
;; If you have Emacs version >= 19.23, then crypt is not required
;; and this should work "out-of-the-box".
;; *** WARNING *** WARNING *** WARNING *** WARNING *** WARNING ***

;; Some users don't read warnings...
(if (and (featurep 'ange-ftp) (fboundp 'find-compressed-version))
    (progn
      (message
       "Don't use uncompress.el with lispdir.el.  See warning in lispdir.el.")
      (sit-for 5)))

(defvar lisp-code-directory
 "/anonymous@archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive/LCD-datafile.gz"
  "*Database of free lisp code.  Entries are in the form:
Name|Author|Contact|Description|Date|Version|Archive")

(defvar format-lisp-code-directory-line 'format-lcd-line-Sill
  "*Function that formats one line of GNU Emacs Lisp Code Directory.\n
Provided as a variable for customizability.  Should not insert
final newline.")

(defvar lisp-code-directory-header 'lcd-header-Sill
  "*Function that inserts header appropriate for
format-lisp-code-directory-line.")

(defvar elisp-archive-host "archive.cis.ohio-state.edu"
  "*Site with elisp archive available via anonymous ftp.")

(defvar elisp-archive-directory "/pub/gnu/emacs/elisp-archive/"
  "*Root directory of elisp archives on elisp-archive-host.")

(defvar elisp-submission-address "elisp-archive@cis.ohio-state.edu"
  "*Submission mail address for elisp archive.")

(defvar lisp-dir-expand-filename nil
  "*If nil do not expand \"~\" to the elisp archive.  Non-nil means expand.")

(defvar lisp-dir-fill-column 76
  "*Column beyond which formatted LCD entries are wrapped.  A value of
nil means don't wrap.")

;; Access-method-dependent code begins here
;;
;; Theory: some day soon we'll have a couple of different remote-access
;; methods --- at least, one through FTP and one through an FTP request server
;; accessed by mail (I'd have implemented this already if I could remember
;; the address of the one at Princeton!).  Isolate all that stuff as cases
;; of the implementation of these functions.

(defun remote-exists-p (file)
  ;; Check for the existence of a file at a remote ftp site
  (require 'ange-ftp)
  (if (not (or (featurep 'jka-compr)
	       (featurep 'crypt)))
      (if (and (>= emacs-major-version 19)
	       (>= emacs-minor-version 23))
	  (require 'jka-compr)
	(require 'crypt)))
  (ange-ftp-file-exists-p (concat "/anonymous@" file)))

(defun remote-find-file (file)
  ;; Remote-fetch a file
  (require 'ange-ftp)
  (if (not (or (featurep 'jka-compr)
	       (featurep 'crypt)))
      (if (and (>= emacs-major-version 19)
	       (>= emacs-minor-version 23))
	  (require 'jka-compr)
	(require 'crypt)))
  (find-file-noselect (concat "/anonymous@" file)))

;; Access-method-dependent code ends here

(defun format-lisp-code-directory ()
   "Convert GNU Emacs Lisp Code Directory into something a human could read.
Calls value of lisp-dir-hook with no args if that value is non-nil."
   (interactive)
   (pop-to-buffer "*GNU Emacs Lisp Code Directory*")
   (fundamental-mode)
   (setq buffer-read-only nil)
   (erase-buffer)
   (buffer-flush-undo (current-buffer))
   (lisp-dir-insert-datafile)
   (insert " GNU Emacs Lisp code directory.  " (current-time-string) ".\n\n")
   (message "Formatting %s ..." lisp-code-directory)
   (delete-region (progn (beginning-of-line) (point))
		  (progn (end-of-line) (point)))
   (funcall lisp-code-directory-header)
   (while (re-search-forward
	   "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
            (author (buffer-substring (match-beginning 2) (match-end 2)))
            (contact (buffer-substring (match-beginning 3) (match-end 3)))
            (description (buffer-substring (match-beginning 4) (match-end 4)))
            (date (buffer-substring (match-beginning 5) (match-end 5)))
            (version (buffer-substring (match-beginning 6) (match-end 6)))
	    (archive (buffer-substring (match-beginning 7) (match-end 7))))
       (delete-region (progn (beginning-of-line) (point))
	(progn (end-of-line) (point)))
       (funcall format-lisp-code-directory-line
	name author contact description date version archive)))
   (goto-char (point-min))
   (center-line)
   (message "Formatting %s ... done" lisp-code-directory)
   (set-buffer-modified-p nil)
   (run-hooks 'lisp-dir-hook))

(defun lisp-dir-apropos (topic)
  "Display entries in Lisp Code Directory for TOPIC in separate window.
Calls value of lisp-dir-apropos-hook with no args if that value is non-nil."
  (interactive (list
		(read-string
		 (concat "GELCD apropos regexp (" (current-word) "): "))))
  (if (equal "" topic) (setq topic (current-word)))
  (save-excursion
    (let ((lisp-code-directory-tmp-buffer
	   (get-buffer-create "*lcd-working*")))
      (message "Searching for %s ..." topic)
      (set-buffer lisp-code-directory-tmp-buffer)
      (lisp-dir-insert-datafile)
      (delete-non-matching-lines topic)
      (set-buffer-modified-p nil)
      (if (= (point-min) (point-max))
	  (progn
	    (kill-buffer lisp-code-directory-tmp-buffer)
	    (message "No entries matching `%s' were found." topic))
	(set-buffer
	 (get-buffer-create "*GNU Emacs Lisp Code Directory Apropos*"))
	(fundamental-mode)
	(setq buffer-read-only nil)
	(erase-buffer)
	(buffer-flush-undo (current-buffer))
	(insert-buffer lisp-code-directory-tmp-buffer)
	(kill-buffer lisp-code-directory-tmp-buffer)
	(insert "GNU Emacs Lisp Code Directory Apropos -- \"" topic "\"\n")
	(if (not lisp-dir-expand-filename)
	    (insert "\"~/\" refers to "
		    elisp-archive-host ":" elisp-archive-directory))
	(insert "\n\n")
	(backward-char 1)
	(funcall lisp-code-directory-header)
	(while (re-search-forward
	"\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
	  (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
		(author (buffer-substring (match-beginning 2) (match-end 2)))
		(contact (buffer-substring (match-beginning 3) (match-end 3)))
		(description (buffer-substring (match-beginning 4) (match-end 4)))
		(date (buffer-substring (match-beginning 5) (match-end 5)))
		(version (buffer-substring (match-beginning 6) (match-end 6)))
		(archive (buffer-substring (match-beginning 7) (match-end 7))))
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (end-of-line) (point)))
	    (funcall format-lisp-code-directory-line
		     name author contact description date version archive)))
	(goto-char (point-min))
	(center-line)
	(message "Searching for %s ... done" topic)
	(set-buffer-modified-p nil)
	(display-buffer "*GNU Emacs Lisp Code Directory Apropos*")
	(run-hooks 'lisp-dir-apropos-hook)))))

;; Read in lisp code directory file in another buffer (using
;; find-file-noselect, so that usual find-file-hooks will be run,
;; like find-crypt-file-hook).

(defun lisp-dir-insert-datafile ()
  "Insert the LCD-database in the current buffer.  The datebase is found
in the file named by the lisp-code-directory variable."
  (if (string-match "\\.gz$" lisp-code-directory)
      (if (not (or (featurep 'jka-compr)
		   (featurep 'crypt)))
	  (if (and (>= emacs-major-version 19)
		   (>= emacs-minor-version 23))
	      (require 'jka-compr)
	    (require 'crypt))))
  (insert-buffer (find-file-noselect lisp-code-directory))
  (setq buffer-read-only nil))

(defun format-lcd-line-Ram
  (name author contact description date version archive)
  "Columnar formatter for Lisp code directory that tries to use as few lines
as possible.  Doesn't fit Contact within first 80 columns."
   (insert-at-column 1  name)
   (insert-at-column 17 description)
   (insert-at-column 49 author)
   (insert-at-column 65 date)
   (insert-at-column 74 "/")
   (insert-at-column 75 version)
   (insert-at-column 84 contact))

(defun format-lcd-line-tale
  (name author contact description date version archive)
  "Multi-line columnar formatter for Lisp Code Directory that tries not
to write anything past column 79."
   (insert-at-column 0  name)
   (insert-at-column 17 description)
   (insert-at-column 56 author)
   (insert-at-column 4  contact)
   (insert-at-column 56 date)
   (insert-at-column 72 version))

(defun format-lcd-line-Sill
  (name author contact description date version archive)
  "Multi-line non-columnar line formatter for Lisp Code Directory."
  (insert-at-column 0 name)
  (if (not (equal version ""))
      (insert " (" version ")"))
  (insert-at-column 22 date)
  (insert "\n")
  (if (and (string-match "[0-9]+\.[0-9]+ dist" contact)
	   (equal author "FSF"))
      (insert-at-column 5 contact)
    (progn
      (insert-at-column 5 author)
      (insert ", <" contact ">\n")
      (if (not (equal archive ""))
	  (progn
	    (if lisp-dir-expand-filename
		(progn
		  (if (and (string-match "~" archive)
			   (= 0 (string-match "~" archive)))
		      (setq archive
			    (concat elisp-archive-host ":"
				    elisp-archive-directory
				    (substring archive 2))))))
	    (insert-at-column 5 archive)
	    ;; jbw: Added code to break long archive name lines between
	    ;; jbw: the hostname and the filename to avoid exceeding 80
	    ;; jbw: characters.
	    (cond ((and lisp-dir-fill-column
			(> (current-column) lisp-dir-fill-column))
		   (move-to-column 5)
		   (cond ((looking-at "[-A-Za-z0-9]+\\(\\.[-A-Za-z0-9]+\\)+:")
			  (goto-char (match-end 0))
			  (insert "\n       ")
			  (end-of-line 1)))))
	    ))))
  (insert-at-column 5 description))

(defun lcd-header-Ram/tale ()
  "Inserts header for column-formatted Lisp Code Directory."
  (funcall format-lisp-code-directory-line
    "Name" "Author" "Contact" "Description" "Date" "Version" "Archive")
  (insert "\n")
  (insert-char ?- 79)
)

(defun lcd-header-Sill ()
  "Inserts empty header for non-columnar Lisp Code Directory"
)

(defun insert-at-column (col string)
   (if (> (current-column) col) (insert "\n"))
   (move-to-column-force col)
   (insert string))

(defun lisp-dir-retrieve (name)
  "Retrieves a copy of the NAMEd package.  The NAME must be an exact match.
Calls value of lisp-dir-retrieve-hook with no args if that value is non-nil."
  (interactive (list
		(read-string
		 (concat "GELCD retrieve (" (current-word) "): "))))
  (if (equal "" name) (setq name (current-word)))
  (save-excursion
    (set-buffer (get-buffer-create (concat "GELCD-" name)))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (lisp-dir-insert-datafile)
    (message "Searching for %s ..." name)
    (delete-non-matching-lines (concat "^" (regexp-quote name) "|"))
    (let ((matches (count-lines (point-min) (point-max))))
      (cond ((= matches 0)
             (progn
               (message "No match found for %s" name)
               nil))
            ((> matches 1)
             (progn
               (message "Multiple matches found for %s, should be unique" name)
               nil))
            (t
             (re-search-forward
              "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
             (let ((archive (buffer-substring (match-beginning 7) (match-end 7))))
               (if (not (equal archive ""))
                   (progn
                     (if (and (string-match "~" archive)
                              (= 0 (string-match "~" archive)))
                         (setq archive (concat elisp-archive-host ":"
                                               elisp-archive-directory
                                               (substring archive 2))))))
               (erase-buffer)
               (let ((lisp-code-directory-tmp-buffer
		      (remote-find-file archive)))
                 (insert-buffer lisp-code-directory-tmp-buffer)
		 (if (not (string-match
			   "\\(\\.tar\\|\\.tgz\\|\\.tar\\.gz\\)$"
			   (buffer-name lisp-code-directory-tmp-buffer)))
		     (kill-buffer lisp-code-directory-tmp-buffer))))
             (goto-char (point-min))
             (display-buffer (concat "GELCD-" name))
             (run-hooks 'lisp-dir-retrieve-hook))))))

(defun lisp-dir-verify (name)
  "Verifies the archive location of the NAMEd package using ange-ftp."
  (interactive (list
		(read-string
		 (concat "GELCD verify (" (current-word) "): "))))
  (if (equal "" name) (setq name (current-word)))
  (save-excursion
    (set-buffer (get-buffer-create "GELCD-verify"))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (lisp-dir-insert-datafile)
    (message "Searching for %s ..." name)
    (delete-non-matching-lines (concat "^" name "|"))
    (let ((matches (count-lines (point-min) (point-max))))
      (cond ((= matches 0)
             (progn
               (message "No match found for %s" name)
               nil))
            ((> matches 1)
             (progn
               (message "Multiple matches found for %s, should be unique" name)
               nil))
            (t
             (re-search-forward
              "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
             (let ((archive (buffer-substring (match-beginning 7) (match-end 7))))
               (if (not (equal archive ""))
                   (progn
                     (if (and (string-match "~" archive)
                              (= 0 (string-match "~" archive)))
                         (setq archive (concat elisp-archive-host ":"
                                               elisp-archive-directory
                                               (substring archive 2))))
                     (if (remote-exists-p archive)
                         (message "Package %s is available from: %s" name archive)
                       (message "Package %s is supposed to be available but isn't." name)))
                 (message "Package %s is not archived." name))))))))

;; Snatched from unix-apropos by Henry Kautz
(if (not (fboundp 'current-word))
    (defun current-word ()
      "Word cursor is over, as a string."
      (save-excursion
	(let (beg end)
	  (re-search-backward "\\w" nil 2)
	  (re-search-backward "\\b" nil 2)
	  (setq beg (point))
	  (re-search-forward "\\w*\\b" nil 2)
	  (setq end (point))
	  (buffer-substring beg end)))))

(defun insert-lcd-headers (name version description)
  "Query user for contents of LCD entry.  Insert them in the current buffer"
  (interactive "sPackage name: \nsVersion: \nsDescription: ")
  (beginning-of-line)
  (insert
   ";;\n"
   ";; Purpose of this package:\n;;\n"
   ";; Installation instructions\n;;\n"
   ";; Usage instructions:\n;;\n"
   ";; Known bugs:\n;;\n"
   (format
    ";; LCD Archive Entry:\n;; \n;; %s|%s|%s@%s\n;; |%s\n;; %s|%s||\n;;\n"
    name
    (user-full-name)
    (user-login-name) (system-name)
    description
    (current-time-string)
    version)))

(defun submit-lcd-entry (name)
  "Submit code for the LCD archive.  Prompts for package name, version, and  a
one-line description of the package.  Leaves you in a mail buffer."
  (interactive "sPackage name: ")
  (require 'sendmail)
  (mail nil elisp-submission-address name)
  (message (substitute-command-keys "Type \\[mail-send] to send bug report.")))

(provide 'lispdir)
