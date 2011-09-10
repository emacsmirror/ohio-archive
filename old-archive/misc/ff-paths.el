;;; ff-paths.el - find-file-using-paths searches certain paths to find files.

;; Copyright (C) 1994, 1995, 1996, 1997 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Created:   16 Sep 1994
;; Version:   3.06 (18 January 97)
;; Keywords:  find-file, ffap, paths, search

;; RCS $Id: ff-paths.el,v 1.7 1997/01/21 16:34:20 rhogee Exp $
;; Note: RCS version number does not correspond to release number.

;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;; LCD Archive Entry:
;; ff-paths|Peter Galbraith|galbraith@mixing.qc.dfo.ca|
;; find-file-using-paths searches certain paths to find files|
;; 20-March-1996|3.02|~/misc/ff-paths.el.gz|
;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;   ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/ff-paths.el

;; This code allows you to use C-x C-f normally most of the time, except that 
;; if the requested file doesn't exist, it is checked against a list of 
;; patterns for special paths to search for a file of the same name.
;;
;; Examples:
;;  - a file extension of .bib will cause to search the path defined in 
;;    $BSTINPUTS or $BIBINPUTS for the file you requested.
;;  - a file extension of .h will cause the /usr/include/ and
;;    /usr/local/include/ directory trees to be searched.
;;  - a file extension of .sty causes a search of TEXINPUTS and of all 
;;    directories below /usr/lib/texmf/tex/
;;  - a file extension of .el causes a search of the path set in the
;;    emacs variable load-path.
;;  - If the aboves searches don't return a match, the filename is searched
;;    for using the `locate' command (if available on your system).  
;;
;; If one file is found, or many files of the same name are found, then the
;; *completions* buffer is displayed with all possibilities, including the
;; non-existing path you first provided.  Selecting it creates the new
;; file.
;;
;; This package runs as a find-file-not-found-hooks hook, and so will
;; happily live alongside other such file-finding mechanisms (e.g.
;; PC-look-for-include-file PC-try-load-many-files vc-file-not-found-hook)

;; The patterns to test against filenames and the associated paths to search
;; for these files can be modified by the user by editing the variable 
;; ff-paths-list defined below.

;; I suggest that you use ffap.el by Michelangelo Grigni <mic@cs.ucsd.edu>,
;; now part of GNU Emacs.  His package will guess the filename from the
;; text under the editing point.  It will search for an existing file in
;; various places before you even get the "File: " prompt.  ff-paths will
;; provide itself to ffap as an additional tool to locate the file before
;; you ever see a prompt.  ff-paths behaves slightly differently with ffap
;; than it does with find-file: if the file path selected under point by
;; ffap does not exist, it is not shown in the completions buffer along
;; with existing paths.  If only one existing path is found for said file,
;; it is placed in the minibuffer at the ffap prompt.  Also, since using
;; the `locate' command is fairly aggressive, it is not used in the ffap
;; toolkit.

;;; Installation:
;;
;;  ff-paths installs itself as a hook in find-file-not-found-hooks for
;;  find-file.  If ffap is installed, ff-paths installs itself as a toolbox
;;  hook in ffap-alist (so load ff-paths after ffap).
;;
;;  All you need to do is load it:
;;   (require 'ff-paths)
;;
;;  You may alter the value of ff-paths-display-non-existant-filename.
;;  Do: C-h v ff-paths-display-non-existant-filename for its documentation
;;
;;  You may alter the value of ff-paths-prompt-for-only-one-match
;;  Do:  C-h v ff-paths-prompt-for-only-one-match for its documentation

;; ----------------------------------------------------------------------------
;;; Change log:
;;
;; V1.01  16sep94 - created by Peter S. Galbraith, 
;;                             rhogee@bathybius.meteo.mcgill.ca
;; V1.02  20sep94 - by Peter S. Galbraith 
;;      Change TeX-split-string to dired-split (thanks to Michelangelo Grigni)
;;      Change variable name psg-ff-list to ff-paths-list
;;      Added find-file-noselect-using-paths for ffap.el
;;      Added ff-paths-prompt variable
;; V1.03  12oct94 - by Peter S. Galbraith
;;      Fixed:
;;      - error when nil appeared in ff-paths-list translation 
;;        (meaning current default)
;;      - find-file-at-point would switch buffer if new file were not created.
;; V1.04  24oct94 - by Peter S. Galbraith
;;      Added patch from Ziv Gigus <ziv@sgi.com> to let environment variables
;;       have trailing directory paths:
;;            ("^foo_.*\\.[ch]$" "$FOO1:$FOO/bar:$FOO/barnone")
;; V2.00  05Jul95 - by Peter S. Galbraith
;;       Reworked interface
;;   Tremendous thanks to Bill Brodie <wbrodie@panix.com> for telling me how 
;;   to make completing-read start off with the completions buffer displayed.
;;   It made this version possible without a kludge.  Thanks Bill!
;; V2.01  05Jul95 - by Peter S. Galbraith
;;      - Followed Bill Brodie's suggestions to make ff-paths-list not
;;        necessarilly a colon-separated string, but rather usually a list
;;        of strings:    ("\\.bib$" "$BSTINPUTS:$BIBINPUTS")
;;                    -> ("\\.bib$" "$BSTINPUTS" "$BIBINPUTS")
;;      - Also his suggestion to not quote symbols.
;;      - Also his suggestion to include leftmost matches as initial string
;;        to completing-read.
;;      - Also, I substitute ~/ for the home directory if possible in the 
;;        matches displayed in the completions buffer. 
;; V2.02  Jul 19 95 - Peter Galbraith 
;;   - Had introduced bug in search-directory-tree. synced with bib-cite.el.
;; V3.00  Jul 26 95 - Peter Galbraith
;;   - Now a hook to find-file and ffap.  Removed `create buffer?' prompt. 
;; V3.01  Sep 13 95 
;;   - dired-aux may not be loaded - Yoichi Konno <itokon@ssel.toshiba.co.jp>
;;   - added ff-paths-display-non-existant-filename 
;;      Jason Hatch <jhatch@matra.demon.co.uk>
;;   - psg-translate-ff-list was reversing directory order 
;;      Juergen Vollmer <vollmer@ipd.info.uni-karlsruhe.de>
;; V3.02  March 20 96
;;     dired-aux not in XEmacs - Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; V3.03  August 19 96 
;;     ff-paths-prompt-for-only-one-match added.
;;     Havard Fosseng <havardf@usit.uio.no>
;; V3.04  August 26 96 Sudish Joseph <sudish@MindSpring.COM>  (RCS 1.4)
;;   - Use unread-command-events instead of unread-command-char.
;; V3.05  December 31 96 - Christoph Wedler <wedler@fmi.uni-passau.de> 
;    (RCS 1.5)
;;   - Use minibuffer-setup-hook instead of unread-command-events.
;;   - Better minibuffer-quit.
;;   - New variable `ff-paths-prompt'
;;   - New variable `ff-paths-require-match'
;;   - Changed from `dired-split' to copying AUCTeX's code.  
;; V3.06  Janury 18 97  (RCS 1.6)
;;   - Added the `locate' command functionality.
;; ----------------------------------------------------------------------------
;;; Code:

;; The following variable may be edited to suit your site: 
;; Send me your interesting add-ons too!

(defvar ff-paths-list
  '(("\\.awk$" "$AWKPATH")              ; awk files in AWKPATH env variable.
    ("\\.bib$" "$BSTINPUTS" "$BIBINPUTS") ; bibtex files.
    ("\\.\\(sty\\|cls\\)$" "$TEXINPUTS" "/usr/lib/texmf/tex//") ;LaTeX files
    ("\\.[h]+$" "/usr/local/include//" "/usr/include//")
    ("^\\." "~/")                       ; .* (dot) files in user's home
    ("\\.el$" load-path))               ; el extension in load-path elisp var
  "*List of paths to search for given file extension regexp's.
The directories can be:
  - colon-separated directories and ENVIRONMENT variables 
    (which may also translate to colon-separated directories)
  - list of strings representing directories or environment variables.
  - a symbol object evaluating to a list of strings (e.g. load-path)

You may mix environment variables and directory paths together.
You may add trailing directoty paths to environment variables, e.g. $HOME/bin
You may not mix strings with elisp lists (like load-path).
You may terminate a directory name with double slashes // indicating that
 all subdirectories beneath it should also be searched.")

(defvar ff-paths-display-non-existant-filename t
  "*find-file-using-paths-hook displays the prompted-for non-existant filename
If you use \"C-x C-f article.sty\" in a path where it does not exists, 
find-file-using-paths-hook will presumably find it for you. If this variable
is set, then this non-existant filename will be displayed in the completions 
buffer along with the existing found file.  This makes it easier in case
you really wanted to create the new file.")

(defvar ff-paths-prompt-for-only-one-match t
  "*If non-nil, prompt the user for filename even if there is only one match.
If nil and ff-paths-display-non-existant-filename is also nil, then dispense 
with confirmation prompt when a single match is found for a non-existant file
and edit that single matched file immediately.")

(defvar ff-paths-require-match nil
  "*Whether user has to choose one of the listed files.
This is the argument REQUIRE-MATCH of `completing-read'.")

;; See variable `ff-paths-use-locate' near end of file

;; Other variables

(defvar ff-paths-prompt "Find File: "
  "Prompt used by ff-paths.")

;; ----------------------------------------------------------------------------
;;; Installs itself as hooks at the end of the file 
;;  (so it won't if error in byte-compiling)

;; ----------------------------------------------------------------------------
;;  Notes about ffap
;;
;;  This defines two hooks: 
;;  - ff-paths-in-ffap used by ffap if it found a filename around point
;;    which doesn't exist in the specified path or default directory.
;;  - find-file-using-paths-hook used by find-file when the specific file
;;    path does not exist.
;;
;;  If ffap doesn't find a filename around point and prompts the user for a
;;  filename and that file doesn't exist, ffap will not use its bag of
;;  tricks to find the file (which would include ff-paths-in-ffap), but
;;  will rather pass the filename directly to find-file, which will call
;;  find-file-using-paths-hook.  So both hooks are actually used. This is
;;  ok, but I'll have to change things if ffap changes this behaviour.
;;
;;  If ffap finds a filename around point but said file does not exit, ffap
;;  will use ff-paths-in-ffap (as part of its toolbox) to locate the file.
;;  I do not include the non-existent file as a possible completion because
;;  ffap cannot readily deal with this.  If only one file is found it is
;;  returned to ffap, which will prompt the user using it as an initial
;;  string.  If no files are found, ff-paths-in-ffap recurses through
;;  directory paths ending in // to try again. If two or more files are
;;  found, ff-paths-in-ffap will use the completions buffer to ask which
;;  the user wants, and returns it to ffap.  Unfortunately, ffap doesn't
;;  know any better than to prompt the user again with this filename.

;;  If ffap and ff-paths-in-ffap both fail, ffap will pass the argument to
;;  vanilla find-file and find-file-using-paths-hook will be called down
;;  the line because the file does not exist.  find-file-using-paths-hook
;;  checks if called with same filename (which will also be same as
;;  ffap-string-at-point) and doesn't do anything if it is. This handles
;;  the case where the user actually wanted to create this new file.

;;  ff-paths-in-ffap can't let the user edit completions to some
;;  non-existing file because ffap will check for existence, crush the
;;  choice and display a fresh prompt.

(defun find-file-using-paths-hook ()
  "Search for file not found in path specified by the variable ff-paths-list."
  ;; This is called by find-file after it fails.
  ;; find-file can itself be called by ffap if no string was under point.
  (if (string-equal buffer-file-name ff-paths-in-ffap-name)
      nil
    (let* ((the-buffer (current-buffer))
           (the-name (file-name-nondirectory buffer-file-name))
           (matches 
            (or (psg-filename-in-directory-list 
                 the-name (psg-translate-ff-list the-name))
                (if ff-paths-use-locate (ff-paths-locate the-name))))
           (bufname (buffer-name buf)) ; compute before uniquify hits!
           newbuf)
      (if (null matches)
          nil                             ;Return nil
        (if ff-paths-display-non-existant-filename
            (setq matches (psg-convert-homedir-to-tilde 
                           (cons (expand-file-name buffer-file-name) matches)))
          (setq matches (psg-convert-homedir-to-tilde matches))) 

;;From: Christoph Wedler <wedler@fmi.uni-passau.de>
;; * The code of automatically displaying the *Completion* Buffer doesn't work
;;   in XEmacs 19.13 (this is fixed in the patch below, ffap did something
;;   similar--but I prefer `cons'ing to `minibuffer-setup-hook' instead of
;;   setting this hook)

;; Replace this:
;;        (let ((unread-command-char ??))
;;          (setq the-name
;;                (if (and (not ff-paths-prompt-for-only-one-match)
;;                         (null (cdr matches)))
;;                    (car matches)
;;                  (or (and (string-equal "18" (substring emacs-version 0 2))
;;                           (completing-read "Find file: " 
;;                                            (create-alist-from-list matches) 
;;                                            nil nil
;;                                            (psg-common-in-list matches)))
;;                      (completing-read "Find file: " 
;;                                       (create-alist-from-list matches) 
;;                                       nil nil
;;                                       (psg-common-in-list matches)
;;                                       'file-name-history)))))
;;

;; With this:
        (condition-case nil
            (let ((minibuffer-setup-hook (cons 'minibuffer-completion-help
                                               minibuffer-setup-hook)))
              (setq the-name
                    (or (and (string-equal "18" (substring emacs-version 0 2))
                             (completing-read ff-paths-prompt
                                              (create-alist-from-list matches)
                                              nil ff-paths-require-match
                                              (psg-common-in-list matches)))
                        (completing-read ff-paths-prompt
                                         (create-alist-from-list matches)
                                         nil ff-paths-require-match
                                         (psg-common-in-list matches)
                                         'file-name-history))))
          (quit (setq the-name nil)))
;; End of Christoph Wedler's change.

        (if (or (not the-name)
                (string-equal "" the-name)
                (not (file-exists-p the-name)))
            nil                           ;Return nil
          (let ((find-file-hooks))        ;Don't call hooks twice
            (setq newbuf (set-buffer (find-file-noselect the-name))))
          (kill-buffer buf)
          (rename-buffer bufname)
          ;; Side-effect variables of parent find-file-noselect
          (setq buf newbuf
                filename buffer-file-name
                truename buffer-file-truename
                number buffer-file-number)
          t)))))

(defun ff-paths-in-ffap (name)
  "Search for ffap-string-at-point in path specified in ff-paths-list."
  ;; This is called by ffap before it prompts.
  (setq ff-paths-in-ffap-name (expand-file-name name))
  (let* ((the-name (file-name-nondirectory name))
         (matches (psg-filename-in-directory-list 
                   the-name (psg-translate-ff-list the-name))))
    (cond
     ((null matches)                    ; No match, Return nil
      nil)
     ((null (cdr matches))              ; Single matche
      (car matches))
     (t
      (setq matches (psg-convert-homedir-to-tilde matches))
      (condition-case nil
	  (let ((minibuffer-setup-hook (cons 'minibuffer-completion-help
					     minibuffer-setup-hook)))
	    (setq the-name 
		  (or (and (string-equal "18" (substring emacs-version 0 2))
			   (completing-read ff-paths-prompt 
					    (create-alist-from-list matches) 
					    nil t
					    (psg-common-in-list matches)))
		      (completing-read ff-paths-prompt 
				       (create-alist-from-list matches) 
				       nil t
				       (psg-common-in-list matches)
				       'file-name-history))))
	(quit (setq the-name nil)))
      (if (and the-name
               (not (string-equal "" the-name)))
          the-name
        nil)))))

(defvar ff-paths-in-ffap-name "" 
  "Filename used when ff-paths-in-ffap called. 
Find-file-using-paths-hook does nothing if called with this same name to avoid
searching twice for a non-existing file the user actually wants to create")

(defun ff-paths-in-ffap-install ()
  "Install ff-paths in ffap toolbox to find files from name under point"
  (cond
   ((and (boundp 'ffap-alist)
         (not (member 
               (cons "\\(^\\.\\)\\|\\.\\(awk\\|bib\\|sty\\|cls\\|[h]+\\|el\\)$"
                     'ff-paths-in-ffap)
               ffap-alist)))
    (setq ffap-alist
	  (nconc
	   ffap-alist
	   (list
	    (cons "\\(^\\.\\)\\|\\.\\(awk\\|bib\\|sty\\|cls\\|[h]+\\|el\\)$"
		  'ff-paths-in-ffap)))))))

;; There must be a command to do this!
(defun psg-common-in-list (list)
  "returns STRING with same beginnings in all strings in LIST"
  (let* ((first-string (car list)) 
         (work-list (cdr list))
         (match-len (length first-string)))
    (while work-list
      (let ((i 1))
        (while (and (<= i match-len)
                    (<= i (length (car work-list)))
                    (string-equal (substring first-string 0 i) 
                                  (substring (car work-list) 0 i))
                    (setq i (1+ i))))
        (setq match-len (1- i)))
      (setq work-list (cdr work-list)))
    (substring first-string 0 match-len)))

(defun psg-convert-homedir-to-tilde (list)
  (let* ((work-list list)(result-list)
         (homedir (concat "^" (file-name-as-directory 
                               (expand-file-name "~"))))
         (the-length (1- (length homedir))))
    (while work-list
      (if (string-match homedir (car work-list))
          (setq result-list 
                (cons (concat "~/" (substring (car work-list) the-length)) 
                      result-list))
        (setq result-list (cons (car work-list) result-list)))
      (setq work-list (cdr work-list)))
    (nreverse result-list)))
    
;; Defined in bib-cite.el !
(defun create-alist-from-list (the-list)
  (let ((work-list the-list)(the-alist))
    (setq the-alist (list (list (car work-list))))
    (setq work-list (cdr work-list))
    (while work-list
      (setq the-alist (append the-alist (list (list (car work-list)))))
      (setq work-list (cdr work-list)))
    the-alist))

(defun psg-filename-in-directory-list (filename list)
  "Check for presence of FILENAME in directory LIST. Return all found.
If none found, recurse through directory tree of directories ending in //
and return all matches."
  ;;USAGE: (psg-filename-in-directory-list "emacs" (psg-list-env "PATH"))
  ;;USAGE: (psg-filename-in-directory-list "ff-paths.el" load-path)
  ;;USAGE: (psg-filename-in-directory-list "ff-paths.el" (psg-translate-ff-list "ff-paths.el"))
  (let ((the-list list) (filespec-list))
    (while the-list
      (let* ((directory (or (and (not (car the-list)) ; list item is nil -> ~/
                                 "~/")
                            (substring (car the-list) 
                                       0 
                                       (string-match "//$" (car the-list)))))
             ;; This removed trailing // if any
             (filespec (expand-file-name filename directory)))
        (if (file-exists-p filespec)
            (setq filespec-list (cons filespec filespec-list))))
      (setq the-list (cdr the-list)))
    (if filespec-list
        filespec-list
      ;; If I have not found a file yet, then check if some directories
      ;; ended in // and recurse through them.
      (let ((the-list list))
        (while the-list
          (if (or (not (car the-list))  ; `nil' case
                  (not (string-match "//$" (car the-list)))) nil
            (message "Searching directories %s for %s" (car the-list) filename)
            (setq filespec-list 
                  (append
                   filespec-list
                   (search-directory-tree 
                    (substring (car the-list) 0 (match-beginning 0)) 
                    (concat "^" filename "$") 
                    t
                    nil))))
          (setq the-list (cdr the-list))))
      filespec-list)))
      
;;; search-directory-tree is heavily based on TeX-search-files
;;  which recursively searches a list of directories for files
;;  matching a list of extensions.  This simplified version should
;;  be a wee bit faster and will suit my purposes (for bib-cite's
;;  need to search directories listed in BIBINPUTS recursively 
;;  if they end in //).
;;  TeX-search-files is part of auc-tex:
;;    Maintainer: Per Abrahamsen <auc-tex@iesd.auc.dk>
;;    Version: $Id: ff-paths.el,v 1.7 1997/01/21 16:34:20 rhogee Exp $
;;    Keywords: wp
     
;;    Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;;    Copyright (C) 1987 Lars Peter Fischer
;;    Copyright (C) 1991 Kresten Krab Thorup
;;    Copyright (C) 1993, 1994 Per Abrahamsen 

;; Also defined in bib-cite.el !
(defun search-directory-tree (directories extension-regexp recurse first-file)
  "Return a list of all reachable files in DIRECTORIES ending with EXTENSION.
DIRECTORIES is a list or a single-directory string
EXTENSION is actually (any) regexp, usually \\\\.bib$
If RECURSE is t, then we will recurse into the directory tree, 
              nil, we will only search the list given.
If FIRST-FILE is t, stop after first file is found."
  (or (listp directories)
      (setq directories (list directories)))
    
  (let (match)
    (while directories
      (let* ((directory (file-name-as-directory  (car directories)))
             (content (and directory
			   (file-readable-p directory)
			   (file-directory-p directory)
			   (directory-files directory))))
        (setq directories (cdr directories))
        (while content
          (let ((file (expand-file-name (car content) directory)))
            (cond ((string-match "[.]+$" (car content))) ;This or parent dir
                  ((not (file-readable-p file)))
                  ((and recurse
                        (file-directory-p file))
                   (setq directories
                         (cons (file-name-as-directory file) directories)))
                  ((string-match extension-regexp 
                                 (file-name-nondirectory file))
                   (and first-file
                        (setq content nil
                              directories nil))
                   (setq match (cons file match)))))
          (setq content (cdr content)))))
    
    match))

;; copied from auctex's TeX-split-string
(defun ff-paths-split-string (regexp string)
  "Returns a list of strings. given REGEXP the STRING is split into 
sections which in string was seperated by REGEXP.

Examples:

      (ff-paths-split-string \"\:\" \"abc:def:ghi\")
          -> (\"abc\" \"def\" \"ghi\")

      (ff-paths-split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

          -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If REGEXP is nil, or \"\", an error will occur."

  (let ((start 0)
        (result '()))
    (while (string-match regexp string start)
      (let ((match (string-match regexp string start)))
        (setq result (cons (substring string start match) result))
        (setq start (match-end 0))))
    (setq result (cons (substring string start nil) result))
    (nreverse result)))

(defun psg-translate-ff-list (filename)
  "Given a file name, return corresponding directory list from ff-paths-list
or nil if file name extension is not listed in ff-paths-list.
So translate the cdr of the ff-paths-list entry to a directory list.
NOTE: returned nil means no match, but nil as an element of the returned list
      is valid, meaning current-directory!"
  (let ((local-ff-list ff-paths-list)(unexpanded-path))
    (while local-ff-list
      (let ((the-pair (car local-ff-list)))
        (cond ((string-match (car the-pair) filename)
               (setq unexpanded-path (cdr the-pair))
               (setq local-ff-list nil)))
        (setq local-ff-list (cdr local-ff-list))))
    ;; `unexpanded-path' holds a list of:
    ;;    no match          ->  nil
    ;;    symbol            ->  (load-path) 
    ;;    stringed PATH     ->  ("/usr/local/include//:/usr/include//")
    ;;    many such strings ->  ("/usr/local/include//" "/usr/include//")
    ;;    appended env var  ->  ("$FOO/bar")
    (cond 
     ((not unexpanded-path)             ; nil case, and we're done.
      nil)
     ((symbolp (car unexpanded-path))   ; load-path type symbol
      (eval (car unexpanded-path)))     ; ->Return it, and we're done.
     (t                                 ;string case, expand each element
      (let ((the-list))
        (while unexpanded-path
          (let ((the-elements (ff-paths-split-string ":" (car unexpanded-path)))
		(path-list) (element))
            (while the-elements
              (setq element (car the-elements))
              (setq the-elements (cdr the-elements))
              (if (string-match "^\\$" element) ; an ENVIRONMENT var?
                  (setq path-list 
                        (nconc path-list (psg-list-env (substring element 1))))
                (if (file-directory-p element) ;  Add only if it exists
                    (setq path-list (cons element path-list)))))
            (if path-list
                (setq the-list (append the-list path-list))))
          (setq unexpanded-path (cdr unexpanded-path)))
        the-list)))))                   ; return full path list. Done...

(defun psg-list-env (env)
  "Return a list of directory elements in ENVIRONMENT variable (w/o leading $)
argument may consist of environment variable plus a trailing directory, e.g.
HOME or HOME/bin"
  (let* ((slash-pos (string-match "/" env))
         (value (if (not slash-pos)
                    (getenv env)
                  (concat (getenv (substring env 0 slash-pos))
                          (substring env slash-pos))))
         ;;(value (getenv env))
	 (entries (and value (ff-paths-split-string ":" value)))
	 entry
	 answers) 
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (if (file-directory-p entry)
	  (setq answers (cons entry answers))))
    (nreverse answers)))

;;; `locate' stuff

(defun ff-paths-have-locate ()
  "Determine if the `locate' command exists on this system.
Based on the ability of locate to find itself; but a bullet-proof test."
  (if (not (condition-case nil
               (not (call-process "sh" nil 0 nil))
             (error)))
      nil                               ;No `sh' command on system
    (if (ff-paths-locate "locate")
        t
      nil)))

(defun ff-paths-locate (filename)
  "Try finding FILENAME using the locate command.
Return a string if a single match, or a list if many matches."
  (let ((ff-buffer (get-buffer-create "*ff-paths-locate*"))
        status matches)
    (save-excursion
      (set-buffer ff-buffer)
      (setq status 
            (call-process "sh" nil t nil "-c" (concat "locate " filename)))
      (goto-char 1)
      (if (eq status 1)
          nil                           ;Not found...
        (while (re-search-forward (concat "/" filename "$") nil t)
          (let ((the-file (buffer-substring (progn (beginning-of-line)(point))
                                            (progn (end-of-line)(point)))))
            (setq matches (cond ((not matches)
                                 (list the-file))
                                (t            
                                 (cons the-file matches)))))))
      (kill-buffer ff-buffer)
      matches)))

(defvar ff-paths-use-locate (ff-paths-have-locate)
  "*Determines whether the `locate' command is used by ff-paths.
If nil, don't.  If t, use it but only if other ff-paths methods have failed.
By default, this is set to `t' if it can be determined that your system has
the locate command.
Using locate is fairly aggressive, and so is *not* added to the ffap toolkit.")

;;; Installs itself
(add-hook 'find-file-not-found-hooks 'find-file-using-paths-hook t)
(ff-paths-in-ffap-install)

(provide 'ff-paths)
;;; ff-paths.el ends here
