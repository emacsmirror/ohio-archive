;; ;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; zload.el -- load and execute compressed elisp files
;; 
;; Copyright (C) 1992 by Kresten Krab Thorup
;;
;; Author          : Kresten Krab Thorup
;; Created On      : Tue Feb 16 17:24:41 1993
;; Last Modified By: Kresten Krab Thorup
;; Last Modified On: Fri Feb 26 01:23:27 1993
;; 
;; Update Count    : 341
;; Buffer Position : 12591
;; Minor Modes     : ( Fill)
;;
;; LCD Archive Entry:
;; zload.el|Kresten Krab Thorup|krab@iesd.auc.dk
;; |Load and execute compressed elisp files
;; |1993/03/03 23:52:31|1.15|iesd.auc.dk:zload.el
;; 
;; zload.el,v
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


(provide 'zload)

(defconst zload:version "1.15"
  "The revision number of zload.el -- code to load and execute
compressed Elisp files.   Complete RCS identity is

	zload.el,v 1.15 1993/03/03 23:52:31 krab Exp")

(defvar zload:suffix-alist
  '((".z" . ("gunzip" "-c"))
    (".Z" . "zcat"))

  "*Alist of (SUFFIX . PROGRAM) pairs. PROGRAM may be either the name
of a program that can decompress files with SUFFIX, or it may be a
list with the program name as the first element and 
of which the first is the name of the program, and the rest is
arguments to supply.  PROGRAM will recieve compressed input on stdin,
and should print the decompressed file on stdout")

(defvar zload:use-cache t
  "*If non-nil zload will cache the location of compressed elisp
files. This will speed up loading a lot if your load-path is long.")

(defvar zload:cache-file "~/.zload-cache"
  "*Name of file holding cache information for zload.
If this is not an absolute file name \(see file-name-absolute-p\),
the caching mechanism will be disabled. ")

(defvar zload:search-compressed-first nil
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

  (and (string-match "\\.[^.]$" file)
       (assoc (substring file (match-beginning 0)) zload:suffix-alist)))

(defun zload::decompression-program (file)
  "Return the program needed to decompress FILE, as a list of which
the first elemnt is the program, and the rest are arguments."

  (let ((program (zload::compressed-p file)))
    (if program
	(if (listp (cdr program))
	    (cdr program)
	  (list (cdr program)))
      (error "zload doesn't know how to decompress %s" file))))

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

  ;; if the file ends with `zfile:compressed-suffix', try loading it
  ;; right away...
  (if (zload::compressed-p file)
      (let ((full-path (zload::find-path-exact file)))
	(if full-path
	    (zload::load-file full-path nomessage)
	  (if noerror
	      nil
	    (signal 'file-error (list "Cannot open load file" file)))))

    (if zload:search-compressed-first
	(let ((full-path (zload::find-path file nosuffix nomessage)))
	  (if full-path
	      (zload::load-file full-path nomessage)
	    (zload::orig-load file noerror nomessage nosuffix)))

      ;; Else, see if the original (emacs) load can handle it...
      (let ((noloaderror (zload::orig-load file t nomessage nosuffix)))
	(if noloaderror
	    (progn
	      (if zload:use-cache
		  (if nosuffix
		      (edbm:remove zload::cache (concat "!" file "!"))
		    (edbm:remove zload::cache file)))
	      noloaderror)
	  
	  ;; Finally, go and have a look for it...
	  (let ((full-path (zload::find-path file nosuffix nomessage)))
	    (if full-path
		(zload::load-file full-path nomessage)
	      (if noerror
		  nil
		(signal 'file-error (list "Cannot open load file" file))))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::find-path -- find absolute path of file appending .Z
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zload::find-path (file nosuffix nomessage)
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

		  (if (and cfile
			   (or (null (cdr cfile))
			       (file-exists-p (cdr cfile))))
		      (throw 'return (cdr cfile))

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
      (if (and zload:use-cache
	       (not dont-cache-it))
	  (if nosuffix
	      (edbm:set zload::cache (concat "!" file "!") result)
	    (edbm:set zload::cache file result)))

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
  "Execute a compressed lisp FILE.
If second argument NOMESSAGE is non-nil, writes a message in the echo area.
FILE is supposed to exist, and it must be the full name including the suffix"

  (let* ((tmp-buf (get-buffer-create (format "*zload<%d>*" zload::loadlevel)))
	 (orig-buf (current-buffer))
	 (full-prog (zload::decompression-program file))
	 (program (car full-prog))
	 (args (cdr full-prog)))

    (save-excursion
      (or nomessage
	  (message "Loading %s..." (file-name-nondirectory file)))
      (set-buffer tmp-buf)
      (delete-region (point-min) (point-max))

      ;; We'll have to `apply' if there are arguments
      (if args
	  (apply 'call-process program file tmp-buf nil args)
	(call-process program file tmp-buf nil))

      ;; Execute it
      (setq zload::loadlevel (1+ zload::loadlevel))
      (eval-current-buffer)
      (setq zload::loadlevel (1- zload::loadlevel))

      ;; go home again..
      (set-buffer orig-buf)
      (kill-buffer tmp-buf)

      ;; notify user
      (or nomessage
	  (message "Loading %s...done" (file-name-nondirectory file)))))
  t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zrequire -- load an option package
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zrequire (feature &optional file)
  "If FEATURE is not present in Emacs (ie (featurep FEATURE) is false),
load FILENAME.  FILENAME is optional and defaults to FEATURE.

Modified to work with compressed files using zload."

  (or (featurep feature)
      (progn
	(load (or file (symbol-name feature)) nil t nil)
	(or (featurep feature)
	    (error "Required feature %s was not provided." feature))))
  feature)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zload::autoload -- load a package when function is invoked
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zautoload (__zload_fun __zload_file 
		  &optional __zload_doc __zload_int __zload_macro)
  "Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to  load.
Third arg DOCSTRING is documentation for the function.
Fourth arg INTERACTIVE if non-nil says function can be called interactively.
Fifth arg MACRO if non-nil says the function is really a macro.
Third through fifth args give info about the real definition.
They default to nil.
If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil.

Modified to work with compressed files using zload."

  (or (and (fboundp __zload_fun)
	   (not (zload::autoload-p __zload_fun)))
      (fset __zload_fun
	    (append (and __zload_macro '(macro))
		    '(lambda (&rest __autoload_args__))
		    (list __zload_doc)
		    (and __zload_int '((interactive)))
		    (list (list 'zload::do-autoload
				(list 'quote __zload_fun)
				__zload_file
				(and __zload_int '(interactive-p))
				__zload_macro
				'__autoload_args__))))))

(defun zload::autoload-p (__zload_fun)
  "Determine if FUNCTION is an autoload function"
  (and (fboundp __zload_fun)
       (let ((def (symbol-function __zload_fun)))
	 (if (listp def)
	     (if (eq (car def) 'autoload) 
		 ;; it's an ordinary autoload
		 t 	
	       
	       ;; strip 'macro if present
	       (if (eq (car def) 'macro)
		   (setq def (cdr def)))
	       
	       (let ((args (car (cdr def))))
		 (eq (car-safe (cdr-safe args))
		     '__autoload_args__)))))))

(defun zload::do-autoload (__zload_fun __zload_file __zload_int 
				       __zload_macro __zload_args)
  "Function called to actually evaluate an autoload function"

  ;; load it
  (load __zload_file nil (not __zload_int) nil)

  (and (or (not (fboundp __zload_fun))
	   (zload::autoload-p __zload_fun))
       (error "Autoloading file %s failed to define %s"
	      __zload_file __zload_fun))

  (if __zload_int
      (call-interactively __zload_fun)
    (if __zload_macro
	(macroexpand (cons __zload_fun __zload_args))
      (apply __zload_fun __zload_args))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install zload in place of load
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'zload::orig-load)
    nil
  (fset 'zload::orig-load (symbol-function 'load)))

(if (fboundp 'zload::orig-autoload)
    nil
  (fset 'zload::orig-autoload (symbol-function 'autoload)))

(fset 'load (symbol-function 'zload))
(fset 'require (symbol-function 'zrequire))
(fset 'autoload (symbol-function 'zautoload))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the cache if the user wants it
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and zload:use-cache
	 (file-name-absolute-p zload:cache-file))
    (setq zload::cache (edbm:init zload:cache-file))
  (setq zload:use-cache nil))

