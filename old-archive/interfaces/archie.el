;; Questions about this version to Jack Repenning <jackr@sgi.com>
;;
;; archie.el
;;   A mock-interface to Archie for Emacs.
;;
;;   -- original version by Brendan Kehoe (brendan@cs.widener.edu)
;;   ange-ftp extensions by Sanjay Mathur (mathur@nas.nasa.gov)
;;   ----- async support by Andy Norman (ange@hplb.hpl.hp.com)
;;   ----- convert-to-dired by (drw@bourbaki.mit.edu)
;;   ----- archie-server-preference-list by Jack Repenning (jackr@sgi.com)
;;   ----- merge with original archie mode by Piet van Oostrum <piet@cs.ruu.nl>
;;   ----- many enhancements thanks to the ange-ftp-lovers list
;;   ----- further archie-mode functions, cleanup, by Rob Austein
;;   ----- support for tree-dired "i"
;;   ----- use reporter.el to report bugs
;;   ----- protect against missing date fields
;;   ----- better default behavior by Michelangelo Grigni <mic@Princeton.EDU>
;;   ----- archie-delete-matching from Sanjay Mathur via David N. Blank-Edelman
;;   ----- Xemacs compatibility (keymap type for keymaps)
;;	   Version: 3.0.2
;;         $ClearCase: archie.el@@/main/66 $
;;         sites:     /ftp@sgigate.sgi.com:/pub/archie-aux/archie.el
;;                    /ftp@alpha.gnu.ai.mit.edu:ange-ftp/archie.el
;;		      /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive 
;; 
;; This file is not part of GNU Emacs but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;

;; Usage:
;;
;; M-x archie creates a separate buffer from which you can find, copy
;; or run dired on any of the entries (using efs/ange-ftp) and redo the search
;; with modified string and/or search-type.
;; alternatively M-x archie creates a separate buffer in dired mode (q.v).
;;
;; Bugs:
;; If you have trouble with this mode, execute the command M-x
;; archie-submit-bug from an archie buffer, describe your problem in
;; the window provided, and mail it off.
;; In order for this to work, you will need Barry Warsaw's reporter.el
;; available in your load-path.

;;

;; Installation instructions:
;;
;; Install this file as archie.el somewhere in your load-path and add the
;; following two lines to ~/.emacs. (without the semicolon's, of course)
;;
;; (autoload 'archie "archie" "Archie interface" t)
;;
;; You may have to change the value of archie-program and archie-server
;; as appropriate for your site.
;; archie-search-type and archie-download-directory can be modified
;; to suit personal preferences.
;;
;; For use with this package, it is also convenient to set
;;  (setq efs-generate-anonymous-password t) or
;;  (setq ange-ftp-generate-anonymous-password t)
;;
;;  The crypt package (available in the LCD archives as
;;  ~/misc/crypt.el.Z) is useful with archie-find-file, since most
;;  archive sites store their files in a compressed form.
;;
;;  The built-in bug reporting system depends upon reporter.el, which
;;  you should have received along with this file.  Or, you can get it
;;  from the LCD archives, in ~/misc/reporter.el.Z.

;; Getting archie.el
;; ===================
;; The latest public release version of this file should always be
;; available for anonymous ftp on the Emacs lisp archive machine.  The
;; path to the file is:
;;
;; archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archiveinterfaces/archie.el.Z 
;; 
;; For those of you without anon-ftp access, you can use the DEC's
;; ftpmail'er at the address ftpmail@decwrl.dec.com.  Send the
;; following message in the body of your mail to that address to get
;; c++-mode:
;;
;; reply <a valid net address back to you>
;; connect archive.cis.ohio-state.edu
;; binary
;; uuencode
;; chdir pub/gnu/emacs/elisp-archiveinterfaces
;; get archie.el.Z
;;
;; or just send the message "help" for more information on ftpmail.
;; Response times will vary with the number of requests in the queue.
;;
;; Similar directions apply for crypt and reporter.

;;
;; This package is available in the LCD archive at
;; archive.cis.ohio-stat.edu:/pub/gnu/emacs/elisp-archive 
;;
;; LCD Archive Entry:
;; archie|Jack Repenning|jackr@dblues.wpd.sgi.com|
;; An Emacs interface to the archie program.|
;; 11-Oct-1994|3.0.2|~/interfaces/archie.el.Z|
;;

;; Customization variables

(defvar archie-program "archie"
  "Program that queries archie servers.")

(defvar archie-args nil "*\
Extra flags passed to archie-program, default is nil.
May be a string or list of strings.  Try \"-t\".")

(defvar archie-server-list
  '(
    ("archie.ans.net"	   .  "147.225.1.10     (ANS server, NY (USA))")
    ("archie.au"	   .  "139.130.4.6      (Austrailian Server)")
    ("archie.doc.ic.ac.uk" .  "146.169.11.3     (United Kingdom Server)")
    ("archie.edvz.uni-linz.ac.at" . "140.78.3.8 (Austrian Server)")
    ("archie.funet.fi"	   .  "128.214.6.102    (Finnish Server)")
    ("archie.internic.net" .  "198.49.45.10     (AT&T server, NY (USA))")
    ("archie.kr"	   .  "128.134.1.1      (Korean Server)")
    ("archie.kuis.kyoto-u.ac.jp" . "130.54.20.1 (Japanese Server)")
    ("archie.luth.se"	   .  "130.240.18.4     (Swedish Server)")
    ("archie.ncu.edu.tw"   .  "140.115.19.24    (Taiwanese server)")
    ("archie.nz"	   .  "130.195.9.4      (New Zeland server)")
    ("archie.rediris.es"   .  "130.206.1.2      (Spanish Server)")
    ("archie.rutgers.edu"  .  "128.6.18.15      (Rutgers University (USA))")
    ("archie.sogang.ac.kr" .  "163.239.1.11     (Korean Server)")
    ("archie.sura.net"	   .  "(1526) 128.167.254.195  (SURAnet alt. MD (USA))")
    ("archie.sura.net"	   .  "128.167.254.195  (SURAnet server MD (USA))")
    ("archie.switch.ch"	   .  "130.59.1.40      (Swiss Server)")
    ("archie.th-darmstadt.de" . "130.83.22.60   (German Server)")
    ("archie.unipi.it"	   .  "131.114.21.10    (Italian Server)")
    ("archie.univie.ac.at" .  "131.130.1.23     (Austrian Server)")
    ("archie.unl.edu"	   .  "129.93.1.14      (U. of Nebraska, Lincoln (USA))")
    ("archie.uqam.ca"	   .  "132.208.250.10   (Canadian Server)")
    ("archie.wide.ad.jp"   .  "133.4.3.6        (Japanese Server)")
    )
  "List of known archie servers.")

(defvar archie-server nil "*\
Server for \\[archie] searches.  If nil (the default), uses whatever
default is built into archie-program.  If t, the first time you use
archie you are asked to choose a server from archie-server-list.")

(defvar archie-download-directory nil
  "*Default directory into which any files copied by archie-copy are
copied. nil means to use /usr/tmp.")

(defvar archie-search-type "exact"
  "*Search type for \\[archie] searches.  (Used to set command-line
argument for archie program.)  See also archie-search-type-sticky.

Can be one of:
        exact                   for exact matches (-e) (default)
        regexp                  for a regexp (-r)
        substring               for substring searches (-c) 
        case-insensitive        for a case-insensitive substring search (-s)
        exact-regexp            for an exact regexp (-er)
        exact-substring         for an exact substring search (-es)
        exact-case-insensitive  for exact case-insensitive search (-ec)
        nil                     to ask every time")

(defvar archie-search-type-sticky t
  "*Once you specify a search type, should it be made the new default
(new value of archie-search-type)?")

(defvar archie-search-type-alist
  ;; This is left as a defvar instead of defconst in case you don't like
  ;; the keyword choice here, eg, you want "substring" to mean
  ;; "case-insensitive-substring" (-s) as Allah clearly intended.
  '(("substring" . "-c")
    ("exact" . "-e")
    ("regexp" . "-r")
    ("case-insensitive" . "-s")
    ("exact-substring" . "-ec")
    ("exact-case-insensitive" . "-es")
    ("exact-regexp" . "-er"))
  "*Alist of search types for \\[archie] searches.")

(defvar archie-internal-search-type-alist nil
  "Internal version of archie-search-type-alist (includes switches, as
well as keywords).")

(defun archie-search-type-alist ()
  "Returns value of archie-internal-search-type-alist, updating it if
necessary."
  (if (eq archie-search-type-alist
          (nthcdr (length archie-search-type-alist)
                  archie-internal-search-type-alist))
      archie-internal-search-type-alist
    (setq archie-internal-search-type-alist
          (nconc (mapcar (function (lambda (x) (cons (cdr x) (cdr x))))
                         archie-search-type-alist)
                 archie-search-type-alist))))

(defvar archie-do-convert-to-dired nil
  "*If t archie buffers are converted to dired-mode, otherwise archie-mode
is used.")

(defvar archie-search-hits nil "*\
Maximum number of hits to ask for in search, a number or string.
If nil, uses whatever default is built into archie-program.")

(defvar archie-window-management 'at-end
  "*When should \\[archie] display the window with the answer?
        'at-start       When the search is initiated
        'at-end         When the result is ready
        'both           Both
        otherwise       Never")

(defvar archie-server-preference-list nil
  "*List of regexps for ordering archie results by server.  May be
right-anchored with \"$\", e.g.:
        '(\"erlangen\\.de$\"
          \"tu-muenchen\\.de$\"
          \"\\.de$\")")

(defvar archie-mode-hook nil
  "Hooks to run after entering archie (non-dired) mode.")

(defvar archie-dired-mode-hook nil
  "Hooks to run after entering archie-dired-mode.")

(defvar archie-anonymous-ftp-username "anonymous"
  "Username to use for \"anonymous\" FTP connections.
Set to \"anonymous\" by default, since more sites accept that than any
other username (even \"ftp\", and no, not all machines in the world
think they're synonyms).  For dired-mode archie, this only matters for
hosts where you've got a non-anonymous username set.")

(defvar archie-display-hook nil
  "Hook run after displaying the results buffer.")

(defvar archie-load-hook nil
  "Hooks run after loading archie.el")


;; Variables you shouldn't have to customize

(defvar archie-file-version "$ClearCase: archie.el@@/main/66 $"
  "Version string for this copy of archie.el")

(defvar archie-release "3.0.1"
  "Release number")

(defvar archie-internal-search-type-alist nil
  "Internal version of archie-search-type-alist (includes switches, as
well as keywords).")

(defvar archie-dired-unusable-functions
  (list
   ;; Classic dired functions
   'dired-backup-unflag
   'dired-byte-recompile
   'dired-chgrp
   'dired-chmod
   'dired-chown
   'dired-clean-directory
   'dired-compress
   'dired-do-deletions
   'dired-flag-auto-save-files
   'dired-flag-backup-files
   'dired-flag-file-deleted
   'dired-rename-file
   'dired-uncompress

   ;;; Tree-dired functions
   'dired-backup-diff
   ;; 'dired-backup-unflag
   'dired-clean-directory
   ;; 'dired-create-directory
   ;; 'dired-diff
   'dired-do-byte-compile
   'dired-do-chgrp
   ;; 'dired-do-chmod
   'dired-do-chown
   'dired-do-compress
   ;; 'dired-do-copy
   ;; 'dired-do-copy-regexp
   'dired-do-delete
   'dired-do-flagged-delete
   'dired-do-hardlink
   'dired-do-hardlink-regexp
   ;; 'dired-do-kill
   'dired-do-load
   ;; 'dired-do-move ; amounts to dired-do-copy
   'dired-do-print
   ;; 'dired-do-redisplay
   'dired-do-rename-regexp
   ;; 'dired-do-shell-command   ; not likely the command knows what to
                                ; do with such a name, but what the hey
   'dired-do-symlink
   'dired-do-symlink-regexp
   'dired-do-uncompress
   'dired-downcase
   ;; 'dired-find-file
   ;; 'dired-find-file-other-window
   'dired-flag-auto-save-files
   'dired-flag-backup-files
   'dired-flag-file-deleted
   'dired-flag-regexp-files
   'dired-hide-all              ; when ``i'' works ...
   'dired-hide-subdir           ; when ``i'' works ...
   ;; 'dired-kill-line-or-subdir
   ;; 'dired-mark-directories
   ;; 'dired-mark-executables
   ;; 'dired-mark-files-regexp
   ;; 'dired-mark-subdir-or-file
   ;; 'dired-mark-symlinks
   ;; 'dired-maybe-insert-subdir
   ;; 'dired-next-dirline
   ;; 'dired-next-line
   ;; 'dired-next-marked-file
   ;; 'dired-next-subdir
   ;; 'dired-prev-dirline
   ;; 'dired-prev-marked-file
   ;; 'dired-prev-subdir
   ;; 'dired-previous-line
   ;; 'dired-quit
   'dired-sort-toggle-or-edit
   ;; 'dired-summary
   ;; 'dired-tree-down
   ;; 'dired-tree-up
   ;; 'dired-undo
   ;; 'dired-unflag-all-files
   ;; 'dired-unmark-subdir-or-file
   ;; 'dired-up-directory
   'dired-upcase
   ;; 'dired-view-file
   ;; 'dired-why
   ;; 'revert-buffer            ; replaced with archie-modify-query
   )
  "*List of dired functions that should be removed from the
archie-dired-mode keymap.")

(defvar archie-l-output "[0-9]*Z *[0-9]* *\\([^ ]*\\) *\\(.*$\\)"
  "Regular expression matching the results of archie -l query. The
   two subexpressions match the host-name and the path respectively.")

(defvar archie-last-query nil)
(defvar archie-last-type nil)

(defvar archie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'archie-next-line)
    (define-key map "\C-?" 'archie-previous-line)
    (define-key map "\C-n" 'archie-next-line)
    (define-key map "\C-p" 'archie-previous-line)
    (define-key map "a" 'archie-modify-query)
    (define-key map "c" 'archie-copy)
    (define-key map "d" 'archie-dired)
    (define-key map "f" 'archie-find-file)
    (define-key map "k" 'archie-delete-matching) ; dNb from SM 10/28/93
    (define-key map "n" 'archie-next-line)
    (define-key map "p" 'archie-previous-line)
    (define-key map "s" 'archie-change-server)
    (define-key map "v" 'archie-view-file)
    (define-key map "x" 'convert-archie-to-dired)
    map)
  "Local keymap used when in archie (non-dired) mode.")

(defvar archie-dired-mode-map nil
  "Local keymap used when in archie-dired-mode.  Normally cloned from
dired-mode-map, after dired-mode-hook is run.")

(defvar archie-msg nil
  "Holds the message posted when a search was begun, to redisplay when
done.")

(defun archie-nyi (arg)
  (interactive "P")
  (message "Not yet implemented."))

(defun archie-deep-copy-keymap (map)
  "Perform an arbitrarily deep copy of a keymap, so it may be changed
without affecting the original."
  (cond
   ((listp map)
    (let (m)
      (cond
       ((and (setq m (cdr-safe map))
	     (symbolp m))
	(cons (nth 0 map) m))
       ((and (setq m (cdr-safe m))
	  (symbolp m))
	(cons (nth 0 map) (cons (nth 1 map) m)))
       ((and (setq m (cdr-safe m))
	     (symbolp m))
	(cons (nth 0 map) (cons (nth 1 map) (cons (nth 2 map) m))))
       (t
	(mapcar 'archie-deep-copy-keymap map)))))

   ((or (symbolp map)			; 'keymap, menu handle, or
	(stringp map))			; menu name
    map)

   ((vectorp map)
    (nth 1 (copy-keymap (list 'keymap map))))
   ((keymapp map)			; ange -- catchall for XEmacs
    (copy-keymap map))))

(require 'dired)

(if archie-dired-mode-map
    nil
  (setq archie-dired-mode-map
	(archie-deep-copy-keymap dired-mode-map))
  (mapcar
   (function (lambda (fn)
	       (substitute-key-definition
		fn
		'archie-nyi
		archie-dired-mode-map)))
   archie-dired-unusable-functions)
  (substitute-key-definition 'revert-buffer
			     'archie-modify-query archie-dired-mode-map)
  (define-key archie-dired-mode-map "s" 'archie-change-server))

(defun archie (type string)
  "Search (with style TYPE, or prompt if arg) for STRING on an Archie
server.

TYPE is the type of search to make -- by default, it's
`archie-search-type'.  Possible values are exact, substring (case
sensitive), case-insensitive and regexp (a regular expression).
Interactively, a prefix arg will make it prompt for this. If
archie-search-type is NIL, always prompts.  If
archie-search-type-sticky is non-nil, each specified value is used as
the next default; otherwise it reverts to archie-search-type.

STRING is the string (or regexp) for which to search.

If archie-do-convert-to-dired is non-NIL, the buffer is converted to a
dired buffer.

The total number of search hits will be limited to (approximately)
archie-search-hits.  If the prefix arg is >= 16 (e.g., ^U ^U
\\[archie]), then you will be prompted for a new value for
archie-search-hits."
  (interactive (archie-get-query-args archie-search-type nil))
  (let ((buf (generate-new-buffer string))
        (flags (concat (or (cdr (assoc type (archie-search-type-alist)))
                           (cdr (assoc archie-search-type
                                       (archie-search-type-alist)))
                           "-e"))))
    (save-window-excursion
      (set-buffer buf)
      (setq archie-last-query string)
      (setq archie-last-type type)
      (setq buffer-read-only nil)
      (erase-buffer)
      (archie-mode)
      (set
       (make-local-variable 'archie-msg)
       (message "Asking archie for %s match for \"%s\" ..." type string)))
    (if (or (eq archie-window-management 'at-start)
            (eq archie-window-management 'both))
        (progn
          (display-buffer buf)
	  (save-window-excursion
	    (set-buffer buf)
	    (run-hooks 'archie-display-hook))))
    (let ((proc (apply
		 'start-process
		 "archie" buf archie-program "-l"
		 (nconc
		  (if (consp archie-args) 
		      (copy-sequence archie-args) ;; protect from nconc
		    (if (stringp archie-args) (list archie-args)))
		  (if archie-server (list "-h" (archie-server)))
		  (if archie-search-hits
		      ;; "%s" allows either a number or string:
		      (list (format "-m%s" archie-search-hits)))
		  ;; why do we have a second "-l" again here?
		  (list flags "-l" string)))))
      (process-kill-without-query proc)
      (set-process-sentinel proc (function archie-process-sentinel)))))

(defun archie-load-some-dfs ()
  "Ensure some distributed file system code is loaded (EFS, or if not
available, ange-ftp)." 
  (interactive)
  (if (and (not (featurep 'efs))
	   (not (featurep 'ange-ftp)))
      (condition-case nil
	  (require 'efs)
	(error (require 'ange-ftp)))))

(defun archie-process-sentinel (proc string)
  (if (buffer-name (process-buffer proc))
      (unwind-protect
          (save-window-excursion
            (set-buffer (process-buffer proc))
            (let ((am archie-msg))
              (message "%s converting." am)
              (goto-char (point-min))
              (archie-order-results)
	      (archie-load-some-dfs)
              (if archie-do-convert-to-dired (convert-archie-to-dired))
              (setq buffer-read-only t)
              (message "%s done." am)))
        (if (or (eq archie-window-management 'at-end)
                (eq archie-window-management 'both))
            (progn
              (display-buffer (process-buffer proc))
	      (save-window-excursion
		(set-buffer (process-buffer proc))
		(run-hooks 'archie-display-hook)))))))

(defun archie-order-results ()
  "Order archie results by archie-server-preference-list."
  (goto-char (point-min))
  (mapcar
   (function
    (lambda (server-re)
      (let (match)
        (if (string-match "\\$$" server-re)
            (setq server-re
                  (concat (substring server-re 0 -1) " ")))
        (while
            (save-excursion
              (re-search-forward (concat "^[0-9Z]+\\s +[0-9]+ \\S *"
                                         server-re
                                         ".*")
                                 nil t))
          (setq match (buffer-substring (match-beginning 0) (1+ (match-end 0))))
          (delete-region  (match-beginning 0) (1+ (match-end 0)))
          (insert match)))))
   archie-server-preference-list))

(defun convert-archie-to-dired ()
  "Convert a buffer containing output in 'archie -l' format into a Dired-mode
buffer in which the usual Dired commands can be used, via efs/ange-ftp."
  (interactive)
  (archie-load-some-dfs)
  (let (lines b s date size host file type year)
    (setq year (substring (current-time-string) -4))
    (setq lines (count-lines (point-min) (point-max)))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (insert "  total " (int-to-string lines) ?\n)
    (while (not (eobp))
      (condition-case error
          (progn
            (setq b (point))
            (beginning-of-line 2)
            (setq s (buffer-substring b (point)))
	    (cond
	     ((string-match
                 "^\\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\)Z +\\([0-9]+\\) \\([-_.a-zA-Z0-9]+\\) \\([^ \n]+\\)$"
                 s)
	      t)
	     ((string-match
                 "^ +\\([0-9]+\\) \\([-_.a-zA-Z0-9]+\\) \\([^ \n]+\\)$"
                 s)
	      ;; date field missing (happens sometimes) - provide a
	      ;; nonsense one, so at least the entry is usable
	      (setq s (concat "19700101000000Z" s))
	      (string-match
                 "^\\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\)Z +\\([0-9]+\\) \\([-_.a-zA-Z0-9]+\\) \\([^ \n]+\\)$"
                 s))
	     (t
                (error "Line not from 'archie -l'")))
            (setq date (substring s (match-beginning 1) (match-end 1)))
            (setq size (substring s (match-beginning 2) (match-end 2)))
            (setq host (substring s (match-beginning 3) (match-end 3)))
            (setq file (substring s (match-beginning 4) (match-end 4)))
            (if (string-equal (substring file -1) "/")
                (progn
                  (if (not (string-match "F" dired-listing-switches))
                      (setq file (substring file 0 -1)))
                  (setq type "d"))
              (setq type "-"))
            (save-excursion
              (insert "  "
                      ;; - or d, depending on whether it's a file or a directory
                      type
                      "r--r--r--  1 ftp"
                      ;; file size
                      (make-string (- 8 (length size)) ? )
                      size
                      " "
                      ;; creation date
                      (condition-case error
                          (aref
                           ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
                           (1- (string-to-int (substring date 4 6))))
                        (error "Jan"))
                      " "
                      (if (= (aref date 6) ?0)
                          (concat " " (substring date 7 8))
                        (substring date 6 8))
                      (if (string-equal (substring date 0 4) year)
                          (concat " " (substring date 8 10) ":"
				  (substring date 10 12))
                        (concat "  " (substring date 0 4)))
                      ;; file name, in efs/ange-FTP format
                      (archie-get-user-prefix host) host ":" file
                      ?\n))
            (delete-region b (point))
            (forward-line 1))
        (error nil)))
    (goto-char (point-min))
    (save-excursion (insert "  /:\n"))
    (setq default-directory "/")
    (set-buffer-modified-p nil)
    (dired-mode)
    (use-local-map archie-dired-mode-map)
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-build-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v 1.15
        ;; and later)
	(progn
	  (make-local-variable 'dired-subdir-alist)
	  (dired-build-subdir-alist)
	  (dired-initial-position "/"))
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm) 
      (archie-dired-mode)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))))

(defun archie-get-user-prefix (host)
  "Return a suitable string to affix to the archie filename for this HOST."
  (archie-load-some-dfs)
  (let ((prefix (concat " /" archie-anonymous-ftp-username "@"))
	(default-user (if (boundp 'efs-default-user)
			  efs-default-user
			ange-ftp-default-user)))
    (if (or (not default-user)
            (stringp default-user))
        (let ((user (apply
		     (if (fboundp 'efs-get-user)
			 'efs-get-user
		       'ange-ftp-get-user)
		     (list host))))
          (if (or (string-equal user "anonymous")
                  (string-equal user "ftp"))
              (setq prefix " /"))))
    prefix))

(defun archie-dired-mode ()
  "Mode for handling archie output as a dired buffer.  Uses your own
dired mode, as customized by any hooks.  Also runs your own
archie-dired-mode-hook, if any, and uses this modified keymap:
\\{archie-dired-mode-map}."
  (interactive)
  (if (not (fboundp 'dired-mode)) (load "dired"))
  (dired-mode (concat "archie " (buffer-name)))
  (setq default-directory "/usr/tmp/")
  (if archie-dired-mode-map
      nil
    (setq archie-dired-mode-map
          (copy-keymap (current-local-map)))
    (mapcar
     (function (lambda (fn)
                 (substitute-key-definition fn nil archie-dired-mode-map)))
     archie-dired-unusable-functions)
    (substitute-key-definition 'revert-buffer
                               'archie-modify-query archie-dired-mode-map)
    (define-key archie-dired-mode-map "s" 'archie-change-server))
  (use-local-map archie-dired-mode-map)
  (setq major-mode 'archie-dired-mode)
  (setq mode-name "Archie Dired")
  (run-hooks 'archie-dired-mode-hook))

(defun archie-get-filename ()
    (beginning-of-line)
    (if (looking-at archie-l-output)
        (concat "/" archie-anonymous-ftp-username "@"
                (buffer-substring (match-beginning 1) (match-end 1))
                ":"
                (buffer-substring (match-beginning 2) (match-end 2)))
      (error "Not archie -l output")))

(defun archie-next-line (arg)
  (interactive "p")
  (next-line arg)
  (if (looking-at archie-l-output)
      (goto-char (match-beginning 1))))

(defun archie-previous-line (arg)
  (interactive "p")
  (previous-line arg)
  (if (looking-at archie-l-output)
      (goto-char (match-beginning 1))))

(defun archie-find-file ()
  "Find the file mentioned on the current line of archie -l output.
Runs dired if the file is a directory and find-file-run-dired is
non-nil."
  (interactive)
  (find-file (archie-get-filename)))

(defun archie-view-file ()
  "View the file mentioned on the current line of archie -l output."
  (interactive)
  (view-file (archie-get-filename)))

(defun archie-copy ()
  "Copy the file mentioned on the current line of archie -l output.
   Prompts with the value implied by archie-download-directory
   as the default directory in which to copy. The file-name part can be
   empty, in which case the original name is used."
  (interactive)
  (let* ((from (archie-get-filename))
         (from-nondir (file-name-nondirectory from))
         (to nil))
    (if (string-equal "" from-nondir)
        (error "%s is a directory" from))
    (setq to (read-file-name
              (format "Copy %s to: " from-nondir)
              (or archie-download-directory "/usr/tmp")))
    (if (file-directory-p to)
        (setq to (concat (file-name-as-directory to) from-nondir)))
    (copy-file from to 1)))

(defun archie-delete-matching (regex) ; dNb from SM 10/28/93
  (interactive "sDelete entries matching regex ? ")
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (delete-matching-lines regex))))

(defun archie-dired ()
  "Run dired on the file or directory mentioned on the current line
   of archie -l output."
  (interactive)
  (dired (file-name-directory (archie-get-filename))))

(defun archie-get-query-args (type-defl string-defl)
  "Queries user for search type (default: TYPE-DEFL) and string
 (default: STRING-DEFL).  Use to prepare args for (interactive)."
  (let* ((tmp-type (or (if (or current-prefix-arg (null archie-search-type))
                           (completing-read
                            "Search type: "
                            (archie-search-type-alist)
                            nil
                            t
                            type-defl))
                       archie-search-type))
         (tmp-string (read-string
                      (concat "Ask Archie for " tmp-type  " match for: ")
                      string-defl)))
    (if archie-search-type-sticky
        (setq archie-search-type tmp-type))
    (if (and current-prefix-arg (<= 16 (car current-prefix-arg)))
        (let (tstr)
          (setq tstr (read-from-minibuffer "Reset archie-search-hits to: "))
          (while (>= 0 (string-to-int tstr))
            (setq tstr
                  (read-from-minibuffer
                   "Must be a number greater than zero.  Reset archie-search-hits to: ")))
          (setq archie-search-hits tstr)))
    (list tmp-type tmp-string)))

(defun archie-modify-query (type string)
  "Re-do the last archie search, with modification of the string
and/or search type."
  (interactive (archie-get-query-args archie-last-type archie-last-query))
  (archie type string))

(defun archie-server ()
  "Return current server, or prompt for new one."
  (interactive)
  (if (stringp archie-server)
      archie-server
    (call-interactively 'archie-change-server)))

(defun archie-change-server (new-server)
  "Change the current archie server to be NEW-SERVER."
  (interactive (list
                (completing-read
		 (if (stringp archie-server)
		     (format "Change Archie server (current: %s): "
			     archie-server)
		   "Choose Archie server: ")
                 archie-server-list
                 nil
		 ;; Why do we require a match here?  Maybe the user wants
		 ;; to try out a new experimental server ... Mic
                 t)))
  (setq archie-server new-server))

(defun archie-mode ()
  "Major mode for interacting with the archie program.
Type: \\[archie-find-file]  to find the file on the current line,
or:  \\[archie-copy] to copy it
or:  \\[archie-dired] to run dired.
or:  \\[convert-archie-to-dired] to convert the buffer to dired.

To redo the last search with modification of the string and/or
switches, type: \\[archie-modify-query].

If archie-download-directory is set to non-nil then its value is used
as the default directory while prompting for the target file by the
archie-copy command; otherwise, /usr/tmp.

\\{archie-mode-map}

Runs archie-mode-hook, if defined.

For problems, do \\[archie-submit-bug]."
  (kill-all-local-variables)
  (setq mode-name "Archie")
  (setq major-mode 'archie-mode)
  (use-local-map archie-mode-map)
  (setq mode-line-process '(": %s"))
  (run-hooks 'archie-mode-hook))


;;

(defvar archie-mode-help-address "archie@dblues.wpd.sgi.com"
  "Address to receive archie.el bug reports.")

(autoload 'reporter-submit-bug-report "reporter")

(defun archie-submit-bug ()
  "Submit a bug report via email."
  (interactive)
  (if (y-or-n-p "Do you want to submit a bug report on archie mode? ")
      (reporter-submit-bug-report
       archie-mode-help-address
       (concat "archie.el " archie-release
	       " (" archie-file-version ")")
       (list
	'archie-program
	'archie-args
	'archie-server
	'archie-download-directory
	'archie-search-type
	'archie-search-type-sticky
	'archie-do-convert-to-dired
	'archie-search-hits
	'archie-window-management
	'archie-anonymous-ftp-username
	'archie-l-output
	'archie-last-query
	'archie-last-type
	'archie-server-preference-list
	'archie-mode-hook
	'archie-dired-mode-hook
	'archie-display-hook
	'archie-load-hook
	'archie-server-list
	'archie-search-type-alist
	'archie-internal-search-type-alist
	'archie-dired-unusable-functions
	'archie-mode-map
	)
       nil
       nil
       "Yo, Jack!")))


(run-hooks 'archie-load-hook)
(provide 'archie)
