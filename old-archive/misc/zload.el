; zload.el was written by Kresten Krab Thorup in Feb 1993, but last
; updated by him in March 93.
; 
; Since then package has been reworked by Alon Albert
; alon@milcse.rtsg.mot.com in Aug 93 to use the file-name-handler-alist
; technique (which enables autoloads which have been dumped with emacs to
; work).
; 
; Unfortunately there were several problems with this new version, and
; since it hasn't been updated for so long, I decided to fix them!
; 
; Here are some of the problems which have been fixed:-
; 
; cl.el would NOT load.
; 
; An uncompressed file could not cause another compressed file to be loaded.
; 
; Ange-ftp magic name handling was broken.
; 
; eval-after-load was broken for compressed files.
; 
; The package went unnecessarily recursive while searching the load path.
; 
; 
; 
; The version I have appended to this email has been used by several
; people without problem.
; 
; Here is a brief list of changes I have made since version 1.16 to get to version 1.20:-
;; Revision 1.20
;; Added variables zload:cache-hits zload:cache-misses and zload:want-menus
;; Added function zload:cache-report
;;
;; Revision 1.19
;; Modified to handle after-load-alist members which are a list of forms.
;;
;; Revision 1.18 
;; 
;; Attempted to fix the bug that after-load-alist didn't work.
;; 
;; (eval-after-alist was handled by "load" but we don't use load....)
;; 
;; This fix would be much simpler if it was done in the handler function.....
;; 
;; It is currently done in zload::load-file.
;; 
;; This does have the advantage that path and extensions are stripped
;; before the lookup in the alist.

;; zload.el,v
;; Revision 1.17 1995/01/31 msp
;; 
;; zload::handler-function
;; Modified to disable recursion using inhibit-file-name-handlers and
;; inhibit-file-name-operation.
;; This means that zload will correctly co-exist with ange-ftp
;;
;; Modified zload to temporarily remove our handler from 
;; file-name-handler-alist so that the search code runs faster.
;; The handler is re-added before eval-buffer is called (in case it
;; does a require).
;;
;; Changed to load uncompressed files by evaling a buffer. This means
;; uncompressed files can directly load compressed ones with out
;; infinite recursion happening.
;; (zload::load-file now handles uncompressed files).
;;
;; Changed installation code to only install if our HANDLER is not
;; present.
;;
;; Changed to NOT cache misses (because this makes adding libraries
;; and the interaction between zload::find-compressed-path and
;; zload::find-normal-path fail).
;;
;; Re-ordered the contents of zload:suffix-alist to speed operation up.
;;



---------------------------------------cut here------------------------------------
;; ;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;; zload.el -- load and execute compressed elisp files
;; 
;; Copyright (C) 1992 by Kresten Krab Thorup
;;
;; Author          : Kresten Krab Thorup
;; Created On      : Tue Feb 16 17:24:41 1993
;; Last Modified By: Mark Phillips msp@bnr.co.uk
;; Last Modified On: $Id: zload.el,v 1.20 1995/02/03 11:23:50 msp Exp $
;; 
;; LCD Archive Entry:
;; zload.el|Kresten Krab Thorup|krab@iesd.auc.dk|
;; Load and execute compressed elisp files|
;; 13-Feb-1995||~/misc/zload.el.el.Z|

;; Revision 1.20
;; Added variables zload:cache-hits zload:cache-misses and zload:want-menus
;; Added function zload:cache-report
;;
;; Revision 1.19
;; Modified to handle after-load-alist members which are a list of forms.
;;
;; Revision 1.18 
;; 
;; Attempted to fix the bug that after-load-alist didn't work.
;; 
;; (eval-after-alist was handled by "load" but we don't use load....)
;; 
;; This fix would be much simpler if it was done in the handler function.....
;; 
;; It is currently done in zload::load-file.
;; 
;; This does have the advantage that path and extensions are stripped
;; before the lookup in the alist.

;; zload.el,v
;; Revision 1.17 1995/01/31 msp
;; 
;; zload::handler-function
;; Modified to disable recursion using inhibit-file-name-handlers and
;; inhibit-file-name-operation.
;; This means that zload will correctly co-exist with ange-ftp
;;
;; Modified zload to temporarily remove our handler from 
;; file-name-handler-alist so that the search code runs faster.
;; The handler is re-added before eval-buffer is called (in case it
;; does a require).
;;
;; Changed to load uncompressed files by evaling a buffer. This means
;; uncompressed files can directly load compressed ones with out
;; infinite recursion happening.
;; (zload::load-file now handles uncompressed files).
;;
;; Changed installation code to only install if our HANDLER is not
;; present.
;;
;; Changed to NOT cache misses (because this makes adding libraries
;; and the interaction between zload::find-compressed-path and
;; zload::find-normal-path fail).
;;
;; Re-ordered the contents of zload:suffix-alist to speed operation up.
;;

;; Revision 1.16  1993/08/10 20:29:21   alon
;; Added support for file-name-handler-alist so that zload gets called instead
;; of load allways (don't need to replace the function definition)
;; This provides better suport for autoloading since functions that are
;; declared autoload during dump are declared using the real autoload
;; function and not zload's replacement.
;; Because of this the functions zload::autoload and zload::require are not
;; needed.
;; Also added `.gz' support.
;;
;; Revision 1.15  1993/03/03  23:52:31  krab
;; Fixed bug in recursive loading.  Thanks to numme@itk.unit.no.
;;
;; Revision 1.14  1993/02/25  14:23:56  krab
;; Incorporated bugfixes and features suggested by Jay Adams.  These
;; are fixes to autoload stuff from his jka-load package.
;;
;; Revision 1.13  1993/02/23  21:24:34  krab
;; Incorporated cleanups from Alon Ziv
;;
;; Revision 1.11  1993/02/19  00:48:31  krab
;; Fixed up documentation, so it is presentable to the public.
;; released to gnu.emacs.sources.
;;
;; Revision 1.10  1993/02/18  22:53:44  krab
;; Incorporated new edbm facilities for the cache
;;
;; Revision 1.9  1993/02/18  19:26:11  krab
;; Implemented a caching mechanism, which allows users to avoid
;; waiting for the long seek times when looking for compressed files.
;;
;; Revision 1.8  1993/02/18  16:15:24  krab
;; Incorporated ideas of Alon Ziv (s2861785@techst02.technion.ac.il)
;; So that zload now handles autoload and require.  Autoload should
;; work in most cases including loading of macros and interactive
;; functions.
;;
;; Revision 1.7  1993/02/18  12:31:23  krab
;; Changed scheme to handle multiple decompression
;; programs much like crypt.
;;
;; Revision 1.6  1993/02/17  01:35:20  krab
;; Changed default values to ".Z" and "zcat" in place of gnuzip specifics
;;
;; Revision 1.5  1993/02/17  01:21:15  krab
;; Fixed zload::find-path to handle argument nosuffix correctly
;; Changed errors signaled, to look like those of the original load
;;
;; Revision 1.4  1993/02/16  17:00:21  krab
;; First public release to comp.sources.emacs
;;
;; Revision 1.3  1993/02/16  16:58:18  krab
;; Optimized for bytecompiled files
;;
;; Revision 1.2  1993/02/16  16:46:49  krab
;; Added documentation
;;
;; Revision 1.1  1993/02/16  16:36:17  krab
;; ** initial revision **
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ** BRIEF INTRO **
;;
;; zload provides facilities to load and execute compressed elisp files.
;; When loaded it will hook itself onto the standard library functions
;; load (which is used by load-file and load-library), autoload and require.
;;
;; To use it, simply compress some of your elisp files, and zload will
;; recognize the files as compressed and decompress them when they are
;; loaded.  The decompression is done in memory, so you won't have any
;; files laying around.  If a non-compressed version of a file can be
;; found, and the .Z extension is not given explicitly, this version
;; will be loaded.  The code is optimized for finding byte compiled
;; compressed files.
;;
;; Since looking for all versions of a file takes a long time, zload
;; can utilize a cache to store the full pathnames of previously used
;; files.  The cache is on by default; to disable it, set
;; zload:use-cache to nil.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NAMING CONVENTIONS:  zload:<name> are public, zload::<name> are
;; private and zload:::<name> are local variables.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defconst zload:version "$Revision: 1.20 $"
  "The revision number of zload.el -- code to load and execute
compressed Elisp files.   Complete RCS identity is

	$Id: zload.el,v 1.20 1995/02/03 11:23:50 msp Exp $")

(defvar zload:suffix-alist
  '((".gz" . "gzcat")
    (".Z" . "gzcat")
    (".z" . "gzcat"))

  "*Alist of (SUFFIX . PROGRAM) pairs. PROGRAM may be either the name
of a program that can decompress files with SUFFIX, or it may be a
list with the program name as the first element and 
of which the first is the name of the program, and the rest is
arguments to supply.  PROGRAM will recieve compressed input on stdin,
and should print the decompressed file on stdout")

(defvar zload:use-cache t
  "*If non-nil zload will cache the location of compressed elisp
files. This will speed up loading a lot if your load-path is long.")
(defvar zload:want-menus zload:use-cache
  "Do you want a menu item to display the cache statistics?")
(defvar zload:cache-hits 0
  "Keeps a count of cache hits (which return a usable name).")
(defvar zload:cache-misses 0
  "Keeps a count of the number of new entries made in the cache.")

(defvar zload:cache-file "~/.zload-cache"
  "*Name of file holding cache information for zload.
If this is not an absolute file name \(see file-name-absolute-p\),
the caching mechanism will be disabled. ")

(defvar zload:search-compressed-first t
  "*If non-nil search for compressed files before non-compressed files.
When enabled, you should use the cache for performance reasons which
will be obvious if you try!  When enabled, the cache will get filled
with entries for files which are *not* found in a compressed version.
It will probably be rather slow until the cache is warmed up...")

(defvar zload::cache nil
  "An EDBM structure used for keeping the cache.")

(defvar zload::loadlevel 0
  "Used to handle recursive loads, ie. an autoloaded file that requires 
another file")

;; Load edbm if it is needed

(if zload:use-cache (require 'edbm))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving information on compressed files
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zload::foreach (var list &rest body) 
  "Set VAR to each element of LIST in turn, evaluating BODY for each
value."

  (` (let ((zload:::list (, list))
	   ((, var) nil))
       (while (setq (, var) (car zload:::list))
	 (setq zload:::list (cdr zload:::list))
	 (,@ body)))))

(defun zload::compressed-p (file)
  "*Determine if FILE is compressed in a method known to zload.
Returns an element of zload:suffix-alist or nil."

  (and (string-match "\\.[^.][^.]?$" file)
       (assoc (substring file (match-beginning 0)) zload:suffix-alist)))

(defun zload::decompression-program (file)
  "Return the program needed to decompress FILE, as a list of which
the first element is the program, and the rest are arguments.

If the compression type is not known, nil is returned."

  (let ((program (zload::compressed-p file)))
    (if program
	(if (listp (cdr program))
	    (cdr program)
	  (list (cdr program)))
      nil)))
;;      (error "zload doesn't know how to decompress %s" file))))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::handler-function -- magic handler for compressed files
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::handler-function(operation &rest args)
  (let ((inhibit-file-name-handlers
	 (cons 'zload::handler-function
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
  (if (equal operation 'load)
      (zload (car args) (nth 1 args) (nth 2 args) (nth 3 args))
    (apply operation args))))
 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload -- `load' substitute that can handle compressed files
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload (file &optional noerror nomessage nosuffix)
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

Modified to work with compressed elisp files using zload"

  ;; Remove our handler from the handler list because there is no need to go recursive.
  ;; This will speed up all file calls.
  ;; This will be undone when we return because it is done via a let.
  ;; We re-install ourself before evaling the file.
  (let ((file-name-handler-alist (delq (rassq 'zload::handler-function file-name-handler-alist) file-name-handler-alist))
	(inhibit-file-name-handlers nil)
	(inhibit-file-name-operation nil))

    ;; if the file ends with `zfile:compressed-suffix', try loading it
    ;; right away...
    (if (zload::compressed-p file)
	(let ((full-path (zload::find-path-exact file)))
	  (if full-path
	      (zload::load-file full-path nomessage)
	    (if noerror
		nil
	      (signal 'file-error (list "Cannot open load file" file)))))
      
      ;; Else - Does not end with a compression suffix

      ;; It is tempting to try a normal (load) call if the compressed
      ;; file is NOT found BUT If we enable our handler we go
      ;; infinitly recursive, and if we do not we do not handle
      ;; embedded load, require etc. commands....

      ;; Do we want to search for compressed files first?
      (if zload:search-compressed-first
	  (let ((full-path (or (zload::find-compressed-path file nosuffix nomessage)
			       (zload::find-normal-path file nosuffix nomessage))))
	    (if full-path
		(zload::load-file full-path nomessage)
	      (if noerror
		  nil
		(signal 'file-error (list "Cannot open load file" file)))
		))
	
	;; Look for normal before compressed
	(let ((full-path (or (zload::find-normal-path file nosuffix nomessage)
			     (zload::find-compressed-path file nosuffix nomessage))))
	  (if full-path
	      (zload::load-file full-path nomessage)

	    (if noerror
		nil
	      (signal 'file-error (list "Cannot open load file" file)))
	    nil))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::find-compressed-path -- find absolute path of a compressed file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::find-compressed-path (file nosuffix nomessage)
  "Find the absolute path of FILE which is compressed.
FILE as given is supposed *not* to end in a `compressed suffix.'
If NOSUFFIX is non-nil, don't try appending .el and .elc.
If NOMESSAGE is non-nil, don't print error messages."

  (if (or nosuffix (string-match "\\.elc?$" file))
      (setq nosuffix t))

  (let ((elfile (concat file ".el"))
	(elcfile (concat file ".elc"))
	(resfile))

    (let* ((dont-cache-it nil)
	   (result
	    (catch 'return

	    (if zload:use-cache
		(let ((cfile
		       (if nosuffix
			   (edbm:get zload::cache (concat "!" file "!"))
			 (edbm:get zload::cache file))))

		  ;; Note the following code does NOT return nil if
		  ;; the cache says the file does not exist (the entry
		  ;; may have been made during a search for a
		  ;; normal file).
		  (if (and cfile
			   (cdr cfile)
			   (file-exists-p (cdr cfile)))
		      (progn
			(setq zload:cache-hits (1+ zload:cache-hits))
			(setq dont-cache-it t) ; Already in the cache.
			(throw 'return (cdr cfile)))

		    ;; the cache is out of date -- remove that entry
		    (if nosuffix
			(edbm:remove zload::cache (concat "!" file "!"))
		      (edbm:remove zload::cache file)))))

	    ;; if file is absolute, it is simple... (and fast)
	    (if (file-name-absolute-p file)
		(progn
		  (setq dont-cache-it t)
		  (zload::foreach encoding zload:suffix-alist
	            (let ((sfx (car encoding)))

		      ;; don't check .el & .elc versions if NOSUFFIX
		      (or nosuffix
			  (cond
			   ((file-exists-p (concat elcfile sfx))
			    (throw 'return (concat elcfile sfx)))
			   ((file-exists-p (concat elfile sfx))
			    (throw 'return (concat elfile sfx)))))
		      
		      ;; the bare-bone .z version
		      (if (file-exists-p (concat file sfx))
			  (throw 'return (concat file sfx))
			(throw 'return nil))))))

	    ;; otherwise, we must try all possibilities.  This is a bit
	    ;; clumsy, but fast in the case of byte compiled files...

	    (zload::foreach encoding zload:suffix-alist
	      (let ((elczfile (concat file ".elc" (car encoding)))
		    (elzfile (concat file ".el" (car encoding)))
		    (zfile (concat file (car encoding))))

		(or nosuffix		; id .elc/.el is wanted
		    
		    (zload::foreach path load-path	; find .elc.Z file
		      (if (file-exists-p (setq resfile 
					       (expand-file-name elczfile path)))
			  (throw 'return resfile)))
	      
		    (zload::foreach path load-path	; find .el.Z file
		      (if (file-exists-p (setq resfile 
					       (expand-file-name elzfile path)))
			  (throw 'return resfile))))
	
		(zload::foreach path load-path	; append only .Z
		  (if (file-exists-p (setq resfile 
					   (expand-file-name zfile path)))
		      (throw 'return resfile)))))
	
	    nil)))

      ;; Save the newly found file to the cache
      ;; BUT do not cache misses
      (if (and zload:use-cache
	       (not dont-cache-it)
	       result)
	  (progn
	    (setq zload:cache-misses (1+ zload:cache-misses))
	    (if nosuffix
		(edbm:set zload::cache (concat "!" file "!") result)
	      (edbm:set zload::cache file result))))

      result)))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::find-normal-path -- find absolute path of an uncompressed file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::find-normal-path (file nosuffix nomessage)
  "Find the absolute path of library FILE which is NOT compressed.
If NOSUFFIX is non-nil, don't try appending .el and .elc.
If NOMESSAGE is non-nil, don't print error messages."

  (if (or nosuffix (string-match "\\.elc?$" file))
      (setq nosuffix t))

  (let ((elfile (concat file ".el"))
	(elcfile (concat file ".elc"))
	(resfile))

    (let* ((dont-cache-it nil)
	   (result
	    (catch 'return

	    (if zload:use-cache
		(let ((cfile
		       (if nosuffix
			   (edbm:get zload::cache (concat "!" file "!"))
			 (edbm:get zload::cache file))))

		  ;; Note the following code does NOT return nil if
		  ;; the cache says the file does not exist (the entry
		  ;; may have been made during a search for a
		  ;; compressed file).
		  (if (and cfile
			   (cdr cfile)
			   (file-exists-p (cdr cfile)))
		      (progn
			(setq zload:cache-hits (1+ zload:cache-hits))
			(setq dont-cache-it t) ; Already in the cache.
			(throw 'return (cdr cfile)))

		    ;; the cache is out of date -- remove that entry
		    (if nosuffix
			(edbm:remove zload::cache (concat "!" file "!"))
		      (edbm:remove zload::cache file)))))

	    ;; if file is absolute, it is simple... (and fast)
	    (if (file-name-absolute-p file)
		(progn
		  (setq dont-cache-it t)

		      ;; don't check .el & .elc versions if NOSUFFIX
		      (or nosuffix
			  (cond
			   ((file-exists-p elcfile)
			    (throw 'return elcfile))
			   ((file-exists-p elfile)
			    (throw 'return elfile))))
		      
		      ;; the bare-bone .z version
		      (if (file-exists-p file)
			  (throw 'return file)
			(throw 'return nil))))

	    ;; otherwise, we must try all possibilities.
	    (or nosuffix		; id .elc/.el is wanted
		
		(zload::foreach path load-path	; find .elc file
		
				(if (file-exists-p (setq resfile 
							 (expand-file-name elcfile path)))
				    (throw 'return resfile)))
		
		(zload::foreach path load-path	; find .el file
				(if (file-exists-p (setq resfile 
							 (expand-file-name elfile path)))
				    (throw 'return resfile))))
	    
	    (zload::foreach path load-path	; bare file
			    (if (file-exists-p (setq resfile 
						     (expand-file-name file path)))
				(throw 'return resfile)))
	
	    nil)))

      ;; Save the newly found file to the cache
      ;; BUT do not cache misses
      (if (and zload:use-cache
	       (not dont-cache-it)
	       result)
	  (progn
	    (setq zload:cache-misses (1+ zload:cache-misses))
	    (if nosuffix
		(edbm:set zload::cache (concat "!" file "!") result)
	      (edbm:set zload::cache file result))))
      
      result)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::find-path-exact -- find absolute path of file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::find-path-exact (file)
  "Find the absolute path of FILE which is compressed.
FILE is supposed to end in a `compressed suffix'."

  (let ((resfile))  

    (catch 'return

      ;; first check if the file is absolute
      (if (file-name-absolute-p file)
	  (if (file-exists-p file)
	      (throw 'return file)
	    (throw 'return nil)))

      ;; else loop through load-path...
      (zload::foreach path load-path
	
	(if (file-exists-p (setq resfile (expand-file-name file path)))
	    (throw 'return resfile)))
      
      nil)))
    

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::load-file -- uncompress and execute a file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::load-file (file nomessage)
  "Execute a lisp FILE.
If second argument NOMESSAGE is absent or nil, writes a message in the echo area.
FILE is supposed to exist, and it must be the full pathname including the suffix.

This will also handle the loading of an uncompressed file without
using a 'load' call. 

Now also checks the load-after-alist (file is looked up after path,
el, elc and compression extensions have been removed).  "

  (let* ((tmp-buf (get-buffer-create (format "*zload<%d>*" zload::loadlevel)))
	 (full-prog (zload::decompression-program file))
	 (program (car-safe full-prog))
	 (args (cdr-safe full-prog)))

    (save-excursion
      (or nomessage
	  (message "Loading %s..." (file-name-nondirectory file)))
      (set-buffer tmp-buf)

      ;; Only process the buffer if zload::decompression-program found a de-compressor.
      (if program
	  ;; We'll have to `apply' if there are arguments
	  (progn
	    (delete-region (point-min) (point-max))
	    (if args
		(apply 'call-process program file tmp-buf nil args)
	      (call-process program file tmp-buf nil))
	    )
	
	;; program is not set therefore just load the file into the buffer,
	;; replacing all its existing contents
	(insert-file-contents file nil nil nil t))

      ;; Execute it
      (setq zload::loadlevel (1+ zload::loadlevel))
      ;; Re-install our handler in case the file does a require, load etc.
      (zload::install)
      (eval-current-buffer)

      ;;
      ;; Massage the file name so we can use it to check the load-after-alist
      ;;

      ;; Remove any compression extension
      (and
       ;; Find the extension (if any)
       (string-match "\\.[^.][^.]?$" file)
       ;; Check it is in our list
       (assoc (substring file (match-beginning 0)) zload:suffix-alist)
       ;; remove the extension
       (setq file (substring file 0 (match-beginning 0))))

      ;; Remove any trailing .el or .elc
      (and
       (string-match "\\.elc+$" file)
       (setq file (substring file 0 (match-beginning 0))))

      ;; Extract the last path component
      (and
       (string-match "[^/]*$" file)
       (setq file (substring file (match-beginning 0))))

      ;; Now check the eval-after-alist
      (let ((form  (cdr-safe (assoc file after-load-alist))))
	(if form
	    (eval (cons 'progn form))
	  ))


      (setq zload::loadlevel (1- zload::loadlevel))

      ;; Cleanup
      (kill-buffer tmp-buf)

      ;; notify user
      (or nomessage
	  (message "Loading %s...done" (file-name-nondirectory file)))))
  t)




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Report cache hit info.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zload:cache-report()
  "Report the cache hits, misses and ratio."
  (interactive)
  (let* ((total (+ 0.0 zload:cache-hits zload:cache-misses))
	 (ratio (if (= total 0)
		    0
		  (* 100 (/ zload:cache-hits total)))))
    (message "Zload cache Hits %d Misses %d Ratio %d%%" zload:cache-hits zload:cache-misses ratio)))

;; If we are running under X add a menu item.
(and window-system 
     zload:want-menus 
     (cond 
      ((string-match "GNU" (emacs-version))
       ;; GNU Emacs menu
       (defvar menu-bar-zload-menu (make-sparse-keymap "Zload"))
       (define-key global-map [menu-bar ZLoad] (cons "Zload" menu-bar-zload-menu))
       (define-key menu-bar-zload-menu [cache-info] 
	 '("Cache Info" . zload:cache-report)))
      ((string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
       ;; Lucid
       nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install zload 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Changed to check for the presence of our handler instead of our file pattern
(defun zload::install ()
  "Add the zload handler to file-name-handler-alist if it is NOT already present."

  (or (rassq 'zload::handler-function file-name-handler-alist)
      (setq file-name-handler-alist
	    (cons '("^.*$" . zload::handler-function)
;; The following change will NOT work because files are often loaded with NO extension...
;; Changed so the handler only gets called for files with 1 or 2 character extensions
;; ie. contains a . followed by two non-dot non-/ chars in the section after the last /
;;          (cons '( "\\.[^\\./][^\\./]?$" . zload::handler-function)
		  file-name-handler-alist))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the cache if the user wants it
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and zload:use-cache
	 (file-name-absolute-p zload:cache-file))
    (setq zload::cache (edbm:init zload:cache-file))
  (setq zload:use-cache nil))



(zload::install)

(provide 'zload)
;; End of file
---------------------------------------cut here------------------------------------


-- 
Mark Phillips - BNR Europe Ltd. 			(msp@bnr.co.uk)
ESN 742 2461						Tel (+44) 279 402461
