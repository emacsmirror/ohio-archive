; Path: dg-rtp!rock!mcnc!stanford.edu!agate!usenet.ins.cwru.edu!magnus.acs.ohio-state.edu!cis.ohio-state.edu!ifi.uio.no!hallvard
; From: hallvard@ifi.uio.no (Hallvard B Furuseth)
; Newsgroups: gnu.emacs.sources
; Subject: shadow-load.el
; Date: 11 Jul 91 23:11:05 GMT
; 
; Here is a library I have forgotten to post for some time.
; 
; If your private ~/elisp/foo.el loads the standard library foo.el and then
; redefines some parts of it, you can now put
; 
;    (load "foo" nil 'shadow)
; 
; in ~/elisp/foo.el to load the standard library.  It does the same as
; normal load, but skips the first ~/elisp/foo.el in load-path.
; 
; This can be convenient if the "standard" load-path is undetermined when
; you write your mod to foo.el, or if "foo" loads other libraries which are
; also modified in ~/elisp.  Otherwise ~/elisp/foo.el could simply say
; 
;   (let ((load-path '("/usr/lib/gnu/emacs/lisp")))
;      (load "foo" nil t))
; 
; Problems: It will not always work in autoloads.  Read the top of the file
; to see.
; 
; 
; Hallvard Furuseth
; hallvard@ifi.uio.no

;; shadow-load.el version 1.5 for GNU Emacs.
;; Created by Hallvard B Furuseth (hallvard@ifi.uio.no).
;; Last modified Tue 18/04-1991.
;; This code is in the public domain.

;; LCD Archive Entry:
;; shadow-load|Hallvard B Furuseth|hallvard@ifi.uio.no
;; |Load "file" from first dir in load-path from which "file" is not loading
;; |91-04-18|1.5|~/functions/shadow-load.el.Z

;; In file "foo.el", put
;;
;;   (load "foo" nil 'shadow)
;;
;; to load "foo" from the first dir in load-path from which "foo" not
;; currently loading.
;; It first searches load-path once for library "foo", the first occurrence
;; which is not already shadowed is assumed to be the current file.
;;
;; To let others (who do not have ~you/elisp in load-path) load your foo, the
;; form below is currently necessary. It will probably be abolished if
;; shadowing is implemeted in C:
;;
;;   (require 'shadow-load "~you/elisp/shadow-load")
;;   (load "foo" nil "~you/elisp/foo")
;;
;; This informs load that the your "foo" resides in ~you/elisp/.
;; But in many cases you could probably just as well say
;;    (load "/local/lib/gnu/emacs/lisp/foo").
;;
;; Extended require:
;;   Arg 3 is NOERROR as 2nd arg to load,
;;   Arg 4 is SHADOW ('shadow or filename of self, as with load).
;; So you may prefer to use require instead of load:
;;
;;   (require 'shadow-load "~you/elisp/shadow-load")
;;   (require 'foo nil nil "~you/elisp/foo")
;;
;; Modified eval-current-buffer and eval-region so shadowing can be
;; used inside them.
;;
;; Also includes an extension to my where-is-file (locate a file in a pathlist)
;; which obeys the variable load-ignore-directories.

;; Bugs and caveats:
;;
;;   If you don't tell Load which file is currently loading, it can guess
;;   wrong.
;;
;;   When you give 3rd or 4th arg to Require, defuns in the loaded file are
;;   not undone if the feature was not provided.
;;
;;   File-id has several problems.  See comments in the code.
;;   It is used to (try to) detect directories with several names, but
;;   if you don't like it, replace it with (fset 'file-id 'identity).
;;
;;   These problems can be fixed by writing this in C.
;;   The code should be smaller in C, too.  Load, f.ex, would simply bind
;;   current-load-info to (cons full-filename load-ignore-directories) during
;;   readevalloop().  When shadowing, it would bind load-ignore-directories
;;   to (cons (file-id (car current-load-info)) (cdr current-load-info))
;;   during openp() and when computing the current-load-info above.

(provide 'shadow-load)


(defvar current-load-info nil
  "Info about the currently loading file.  Set by function load.
Value: (full_filename . load-ignore-directories for (load ... 'shadow)).")

(defvar load-ignore-directories nil
  "If (list (file-id DIR)) is in this list, where-is-file will not return
files in DIR.  However, files in subdirs of DIR may still be returned.
Used in (load ... 'shadow).
Don't modify this variable unless you know exactly what you are doing.")


(or (fboundp 'shadow-old-load)
    (fset 'shadow-old-load (symbol-function 'load)))

(defun load (shadow-lib &optional shadow-noerr shadow-self shadow-nosuff)
  "Execute a file of Lisp code named FILE.
First tries FILE with .elc appended, then tries with .el,
 then tries FILE unmodified.  Searches directories in  load-path.
If optional second arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional third arg NOMESSAGE is non-nil.
If optional fourth arg NOSUFFIX is non-nil, don't try adding
 suffixes .elc or .el to the specified name FILE.
Return t if file exists.

Extension:
If NOMESSAGE is 'shadow, load FILE from the first dir in load-path from which
 FILE is not currently loading.  If FILE is an absolute pathname, just fail if
 it's already loading.  The calling file must have the same basename as FILE.
Until this is written in C, NOMESSAGE may also be the full pathname of the file
 which calls load.  If not, load assumes this is the first (not shadowed) FILE
 in load-path."
  (if (not (or (eq shadow-self 'shadow) (stringp shadow-self)))
      (let ((current-load-info (list shadow-lib)))
	(shadow-old-load shadow-lib shadow-noerr shadow-self shadow-nosuff))
    (or current-load-info load-in-progress
	(error "Attempt to shadow-load while no load in progress"))
    (let ((current-load-info
	   (let* ((load-ignore-directories load-ignore-directories)
		  (case-fold-search (eq system-type 'vax-vms))
		  self key tmp)
	     ;; Get or guess current library name and load-ignore-directories
	     (setq self (cond
			 ((not (stringp shadow-self)) shadow-lib)
			 ((not (file-name-absolute-p shadow-self))
			  (error "load: non-absolute shadow %s" shadow-self))
			 ((setq shadow-self (expand-file-name shadow-self))))
		   self (substring self 0 (string-match "\\.elc?\\'" self))
		   key (file-name-nondirectory self))
	     (and current-load-info
		  (string-match
		   (concat "\\`" (regexp-quote key) "\\(\\.elc?\\)?\\'")
		   (file-name-nondirectory (car current-load-info)))
		  (progn (setq load-ignore-directories (cdr current-load-info))
			 (or (stringp shadow-self)
			     (setq shadow-self (car current-load-info)))))
	     ;; load-ignore-directories is set. Find currently loading file
	     (or (stringp shadow-self)
		 (setq shadow-self (if (file-name-absolute-p self) key self)))
	     (setq self
		   (cond ((file-name-absolute-p shadow-self)
			  (expand-file-name shadow-self))
			 ((where-is-file load-path shadow-self ".elc:.el:"))
			 ((error "load: unknown shadow file %s" shadow-self))))
	     ;; Find shadowed library
	     (setq load-ignore-directories
		   (cons (list (file-id (file-name-directory self)))
			 load-ignore-directories))
	     (or (setq shadow-lib
		       (where-is-file load-path (setq tmp shadow-lib)
				      (if (not shadow-nosuff) ".elc:.el:")))
		 shadow-noerr
		 (error "Cannot open shadow-load file: %s" tmp))
	     (cons shadow-lib load-ignore-directories))))
      (and shadow-lib
	   (shadow-old-load shadow-lib shadow-noerr t t)))))



(or (fboundp 'shadow-old-eval-current-buffer)
    (fset 'shadow-old-eval-current-buffer (symbol-function 'eval-current-buffer)))
(defun eval-current-buffer (&optional shadow-printflag)
  (interactive)
  (let ((current-load-info (if buffer-file-name
			       (list buffer-file-name))))
    (shadow-old-eval-current-buffer shadow-printflag)))

(or (fboundp 'shadow-old-eval-region)
    (fset 'shadow-old-eval-region (symbol-function 'eval-region)))
(defun eval-region (shadow-beginning shadow-end &optional shadow-printflag)
  (interactive "r")
  (let ((current-load-info (if buffer-file-name
			       (list buffer-file-name))))
    (shadow-old-eval-region shadow-beginning shadow-end shadow-printflag)))



(or (fboundp 'shadow-old-require)
    (fset 'shadow-old-require (symbol-function 'require)))

(defun require (rq-feature &optional rq-file rq-noerror rq-shadow)
  "If FEATURE is not present in Emacs (ie (featurep FEATURE) is false),
 load FILENAME.  FILENAME is optional and defaults to FEATURE.
Extension:
If NOERROR is non-nil, report no error if no FILENAME is found.
If SHADOW is 'shadow (or, for now, a string), it is used as 3rd arg to load."
  (cond ((featurep rq-feature) rq-feature)
	((not (or rq-noerror rq-shadow))
	 (shadow-old-require rq-feature rq-file))
	((not (condition-case error
		  (load (or rq-file (symbol-name rq-feature))
			rq-noerror (or rq-shadow t))
		(error
		 (setq features (delq rq-feature features))
		 (signal (car error) (cdr error)))))
	 nil)
	((featurep rq-feature) rq-feature)
	((error "Required feature %s was not provided" rq-feature))))


;; This is a lisp version of openp() in src/lread.c,
;; ***extended to obey the load-ignore-directories variable***.
;;  Instead of using the exec_only argument of openp(), it returns the
;;  name of a *readable* file.  Should include an optional prefix arg
;;  ACCESS as well, but that can't be done correctly in Elisp.
(defun where-is-file (path file &optional suffixes)
  "Search through PATH (list) for a readable FILENAME, expanded by one of the
optional SUFFIXES (string of suffixes separated by \":\"s).  Interactively,
SUFFIXES (default \".elc:.el:\") is prompted when there is a prefix arg.
Does not return files in load-ignore-directories, see doc for that variable."
  (interactive
   (list (let ((path (read-minibuffer "Search path: " "load-path")))
	   (if (and (consp path) (or (stringp (car path)) (null (car path))))
	       path
	     (eval path)))
	 (read-string "Locate file: ")
	 (if current-prefix-arg
	     (read-string "Suffixes: " ".elc:.el:")
	   ".elc:.el:")))
  (if (not (equal file ""))
      (let ((filelist nil) pos temp templist ignore)
	;; Make list of possible file names
	(setq filelist
	      (if suffixes
		  (progn
		    (while (setq pos (string-match ":[^:]*\\'" suffixes))
		      (setq filelist (cons (concat file (substring suffixes
								   (1+ pos)))
					   filelist))
		      (setq suffixes (substring suffixes 0 pos)))
		    (cons (concat file suffixes) filelist))
		(list file)))
	;; Search PATH for a readable file in filelist
	(catch 'bar
	  (if (file-name-absolute-p file) (setq path '(nil)))
	  (while path
	    (setq ignore (cons '(nil) load-ignore-directories))
	    (setq templist filelist)
	    (while
		(progn
		  (setq temp (expand-file-name (car templist) (car path)))
		  (cond ((and ignore
			      (prog1
				  (assoc (file-id (file-name-directory temp))
					 ignore)
				(setq ignore nil)))
			 nil)
			((file-readable-p temp)
			 (if (interactive-p)
			     (message "%s" temp))
			 (throw 'bar temp))
			((setq templist (cdr templist))))))
	    (setq path (cdr path)))
	  (if (interactive-p)
	      (message "(File %s not found)" file))
	  nil))))


(defun file-id (file)
  "Attempt to return an i.d. for FILE which is unique (by EQUAL)."
  ;; Problems:
  ;;    Does not dereference if FILE is a symlink (because expand-file-name
  ;;	  handles "foo/.." incorrectly if foo is a symlink).
  ;;	I have no idea of how this works on non-UNIX systems.
  ;;	Can't get FILE's device, so two files with the same
  ;;	  inode and owner will be considered equal.
  ;;  If you don't trust it, replace it with (fset 'file-id 'identity).
  (if (setq file (file-attributes file))
      ;; Should be (inode . device), but I suppose this is safe enough
      ;; for the use of this package.
      (cons (nth 10 file) (nth 2 file))))
