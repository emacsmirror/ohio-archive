;;; jka-compr.el - low level support for reading/writing compressed files (V18)
;;; bugs/comments to jka@ece.cmu.edu
;;; Version: 0.5
;;; Last modified: 11/30/93


;;; Copyright (C) 1993  Jay K. Adams
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; LCD Archive Entry:
;;; jka-compr|Jay Adams|jka@ece.cmu.edu|
;;; Low level support for reading/writing compressed files|
;;; 30-Nov-1993|0.5|~/misc/jka-compr.el.Z|


;;; This package implements low-level support for reading and writing
;;; compressed files.  It overwrites several low-level file I/O
;;; functions (including write-region and insert-file-contents) so
;;; that they automatically compress or uncompress a file if the file
;;; appears to need it (based on the extension of the file name).
;;; Although its a little presumptious of a low-level function to make
;;; a decision about whether a file should be compressed or
;;; uncompressed, doing so offers the benefit of allowing packages
;;; like Rmail, Vm, Gnus, and Info to work with compressed files
;;; without modification.


;;; INSTRUCTIONS:
;;;
;;; To use jka-compr, simply load this package, and edit as usual.
;;; Its operation should be transparent to the user (except for
;;; messages appearing when a file is being compressed or
;;; uncompressed).
;;;
;;; *** THIS PACKAGE SHOULD BE LOADED BEFORE ANGE-FTP ***
;;;
;;; *** DO NOT USE THIS PACKAGE WITH EMACS V19 ***
;;;
;;; The variable, jka-compr-compression-info-list can be used to
;;; customize jka-compr to work with other compression programs.  The
;;; default value of this variable allows jka-compr to work with Unix
;;; compress and gzip.
;;; 
;;; If you are concerned about the stderr output of gzip and other
;;; compression/decompression programs showing up in your buffers you
;;; should set the discard-error flag in the compression-info-list.
;;; This will cause the stderr of all programs to be discarded.
;;; However, it also causes emacs to call compression/uncompression
;;; programs through a shell (which is specified by jka-compr-shell).
;;; This may be a drag if, on your system, starting up a shell is
;;; slow.
;;;
;;; If you don't want messages about compressing and decompressing
;;; to show up in the echo area, you can set the compress-name and
;;; decompress-name fields of the jka-compr-compression-info-list to
;;; nil.


;;; APPLICATION NOTES:
;;; 
;;; loading compressed elisp files
;;;   The jka-load package gives you the ability to load, autoload,
;;;   and require compressed elisp files when jka-compr is installed.
;;;   jka-load is probably available wherever you got jka-compr.
;;;
;;; dired
;;;   Normally jka-compr works fine with dired.  However, one case
;;;   where it doesn't work so well is when you use the dired 'B'
;;;   command (byte compile file).  The .z on the file name makes
;;;   dired think the file is not compilable.  Changing the
;;;   dired-byte-recompile function to the one shown below will get
;;;   around this problem.  It makes dired recognize a file as being
;;;   an emacs lisp file even if it has a .z or .Z extension after the
;;;   .el. 
;;;
;;;   (defun dired-byte-recompile ()
;;;     "Byte recompile this file."
;;;     (interactive)
;;;     (let* ((buffer-read-only nil)
;;;            (from-file (dired-get-filename))
;;;            (to-file (substring from-file 0 -3)))
;;;       (if (string-match "\\.el\\(\\.[zZ]\\|\\.gz\\)?$" from-file) nil
;;;           (error "%s is uncompilable!" from-file))
;;;       (byte-compile-file from-file)))
;;;
;;;   The dired supplied with Lucid Emacs 19.6 is a little different
;;;   and requires the following version of dired-byte-compile (from
;;;   David Hughes, Apr 20, 1993).
;;;       
;;;   (defun dired-byte-compile ()
;;;     ;; Return nil for success, offending file name else.
;;;     (let* ((filename (dired-get-filename))
;;;            (elc-file
;;;             (if (eq system-type 'vax-vms)
;;;                 (concat (substring filename 0
;;;                                    (string-match ";" filename)) "c")
;;;               (if (string-match "\\.el\\(\\.[zZ]\\|\\.gz\\)?$" filename)
;;;                   (concat (substring filename 0
;;;                                      (match-beginning 0)) ".elc")
;;;                 (error "%s is uncompilable!" filename))))
;;;            buffer-read-only failure)
;;;       (condition-case err
;;;           (save-excursion (byte-compile-file filename))
;;;         (error
;;;          (setq failure err)))
;;;       (if failure
;;;           (progn
;;;             (dired-log "Byte compile error for %s:\n%s\n"
;;;                        filename failure)
;;;             (dired-make-relative filename))
;;;         (dired-remove-file elc-file)
;;;         (forward-line)                    ; insert .elc after its .el file
;;;         (dired-add-file elc-file)
;;;         nil)))
;;;
;;;
;;; rmail, vm, gnus, etc.
;;;   To use compressed mail folders, .newsrc files, etc., you need
;;;   only compress the file.  Since jka-compr searches for .z
;;;   versions of the files it's finding, you need not change
;;;   variables within rmail, gnus, etc.  
;;;
;;; crypt++
;;;   jka-compr can coexist with crypt++ if you take all the
;;;   compression entries out of the crypt-encoding-alist (i.e. set it
;;;   to nil).  Clearly, problems will arise if you have two programs
;;;   trying to compress/decompress the same file.  Using jka-compr
;;;   along with crypt++ will not enable you to read encrypted
;;;   compressed files--that is, files that have been compressed then
;;;   encrypted (in that order).  With the crypt++ decompression
;;;   features turned off, crypt++ and jka-compr should be able to
;;;   decode the same files that crypt++ could decode on its own.
;;;


;;; TO DO
;;;
;;; Note:  as far as I'm concerned, the whole idea of dealing with
;;; compressed files in this way is still experimental.  Still, I and
;;; others have been using this code for some time and have found it
;;; useful.
;;;
;;; Also, I consider this code to be in beta-test.  The bug rate has
;;; been pretty low, however, so after the few remaining issues
;;; (listed below) are addressed, I'll release Version 1.0 (maybe
;;; sometime this summer).
;;;
;;; To do list:
;;;
;;; 1. Make jka-compr work whether or not ange-ftp is loaded first.
;;;    I'm not sure if this is practical.  Forcing jka-compr to be
;;;    loaded first seems like a good compromise for now.
;;;
;;; 2. Fix it so that the compression extension (.Z or .z) does not
;;;    appear in the buffer name.
;;;
;;; 3. Add the ability to configure translations on the file name to
;;;    be applied before mode-specification.  For instance, .taz files
;;;    should be recognized as being compressed but should be treated
;;;    as .tar files for mode-specification.
;;;
;;; 4. Consider making file name completion less concerned with .Z suffixes.
;;; 
;;; 5. Figure out how to do error checking and handling.
;;;    Unfortunately, there doesn't seem to be any way to check the
;;;    error status of programs called by emacs through call-process.
;;;
;;; 6. Encrypted files.  It would be nice to be able to handle
;;;    encrypted compressed files.


;;; History:
;;;
;;;   Apr 20, 1993    Started keeping a history of changes.
;;;
;;;   Apr 20, 1993    Fixed problems caused by expand-file-name
;;;                   mysteriously returning nil.
;;;
;;;                   0.1 released
;;;
;;;   Apr 30, 1993    Added error handler to jka-compr-insert-file-contents
;;;                   to catch and properly handle errors caused by the 
;;;                   file not being readable.
;;;
;;;   May 25, 1993    Fixed jka-compr-file-name-sans-versions to comply
;;;                   with Version 19.
;;;
;;;                   0.2 released
;;;
;;;   June 9, 1993    Removed all Version 19 support, the version 19
;;;                   variant is now called jka-compr19.
;;;
;;;                   Changed jka-compr-compression-info-list to
;;;                   understand the .gz extension. 
;;;
;;;                   0.3 released
;;;
;;;   June 14, 1993   Added support for file-name-completion.
;;;
;;;                   0.4 released
;;;
;;;   June 23, 1993   Modified the comment regarding crypt++.
;;;
;;;   Nov. 30, 1993   Changed default values of the verify flags to nil.
;;;
;;;                   0.5 released
;;;


(defvar jka-compr-enabled t
  "*Non-nil means that the jka-compr package is enabled.")


(defvar jka-compr-verify-append-file-change nil
  "Non-nil means ask the user before changing the name of an append file.")


(defvar jka-compr-verify-overwrite-file-change nil
  "Non-nil means ask the user before changing the name of a file being written.")


(defvar jka-compr-verify-visited-file-change nil
  "Non-nil means ask the user before changing the visited file name of a buffer.")


(defvar jka-compr-verify-delete-file-change nil
  "Non-nil means ask the user before changing the name of a file being deleted.")


(defvar jka-compr-shell "sh"
  "*Shell to be used for calling compression programs.
The value of this variable only matters if you want to discard the
stderr of a compression/decompression program (see the documentation
for jka-compr-compression-info-list).")


;;; I have this defined so that .Z files are assumed to be in unix
;;; compress format; and .z files, in gzip format.
(defvar jka-compr-compression-info-list
  ;;[regexp  magic 
  ;; compr-name  compr-prog  compr-discard  compr-args
  ;; uncomp-name uncomp-prog uncomp-discard uncomp-args
  ;; can-append extension]
  '(["\\.Z~?$"     "\037\235"
     "compress"    "compress"     nil  nil
     "uncompress"  "uncompress"   nil  nil
     nil           ".Z"]
    ["\\.z~?$"     "\037\213"
     "zip"         "gzip"         nil  ("--suffix" ".z")
     "unzip"       "gzip"         nil  ("-d")
     t             ".z"]
    ["\\.gz~?$"     "\037\213"
     "zip"         "gzip"         nil  nil
     "unzip"       "gzip"         nil  ("-d")
     t             ".gz"])
    

  "List of vectors that describe available compression techniques.
Each element, which describes a compression technique, is a vector of
the form [regexp magic compress-name compress-program compress-discard-err
compress-args uncompress-name uncompress-program uncompress-discard-err
uncompress-args append-flag extension] where:

   regexp                is a regexp that matches filenames that are
                         compressed with this format

   magic                 is a two-byte magic number that identifies
                         files that are compressed with this format

   compress-name         is an English name of the compression (nil
                         means don't show message when compressing)

   compress-program      is a program that performs this compression

   compress-discard-err  is non-nil if the stderr output of the compress
                         program should be discarded.  Setting this flag to 
                         non-nil also causes jka-compr to call compression
                         programs using a shell rather than directly.

   compress-args         is a list of args to pass to the compress program

   uncompress-name       is an English name of the uncompression (nil
                         means don't show message when decompressing)

   uncompress-program    is a program that performs this compression

   uncompress-discard-err  is non-nil if the stderr output of the uncompress
                         program should be discarded.  Setting this flag to 
                         non-nil also causes jka-compr to call decompression
                         programs using a shell rather than directly.

   uncompress-args       is a list of args to pass to the uncompress program

   append-flag           is non-nil if this compression technique can be
                         appended

   extension             string to add to end of filename when looking for
                         files compressed with this technique.

Because of the way call-process is defined, discarding the stderr output of
a program adds the overhead of starting a shell each time the program is
invoked.")


;;; Functions for accessing the return value of jka-get-compression-info
(defun jka-compr-info-fname-match-beg      (info)  (car (car info)))
(defun jka-compr-info-fname-match-end      (info)  (cdr (car info)))
(defun jka-compr-info-fname-regexp         (info)  (aref (cdr info) 0))
(defun jka-compr-info-magic                (info)  (aref (cdr info) 1))
(defun jka-compr-info-compress-message     (info)  (aref (cdr info) 2))
(defun jka-compr-info-compress-program     (info)  (aref (cdr info) 3))
(defun jka-compr-info-compress-discard-err (info)  (aref (cdr info) 4))
(defun jka-compr-info-compress-args        (info)  (aref (cdr info) 5))
(defun jka-compr-info-uncompress-message   (info)  (aref (cdr info) 6))
(defun jka-compr-info-uncompress-program   (info)  (aref (cdr info) 7))
(defun jka-compr-info-uncompress-discard-err (info)  (aref (cdr info) 8))
(defun jka-compr-info-uncompress-args      (info)  (aref (cdr info) 9))
(defun jka-compr-info-can-append           (info)  (aref (cdr info) 10))


(defun jka-compr-get-compression-info-mapper (x)
  "Function used by jka-compr-get-compression-info
to map across the jka-compr-compression-info-list."
  (let ((case-fold-search nil))
    (if (string-match (aref x 0) filename)
	(throw 'compression-info
	       (cons (cons (match-beginning 0) (match-end 0))
		     x)))))


(defvar jka-compr-mktemp-regexp 
  "[A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9]"
  "A regexp that matches the return value of mktemp(3).")


(defun jka-compr-get-compression-info (filename)
  "Return information about the compression scheme of FILENAME.
The determination as to which compression scheme, if any, to use is
based on the filename itself and jka-compr-compression-info-list."
  (and
   (boundp 'ange-ftp-tmp-name-template)
   (boundp 'path)
   (or
    ;; See if it looks like an ange-ftp temp file.
    (string-match (concat "^" (regexp-quote ange-ftp-tmp-name-template)
			  jka-compr-mktemp-regexp "$")
		  filename)
    (string-match (concat "^" (regexp-quote ange-ftp-gateway-tmp-name-template)
			  jka-compr-mktemp-regexp "$")
		  filename))
   ;; If so, use the path variable (dynamically bound by
   ;; ange-ftp-insert-file-contents and ange-ftp-write-region) as the file
   ;; name.
   (setq filename path))

  (catch 'compression-info
    (mapcar 'jka-compr-get-compression-info-mapper
	    jka-compr-compression-info-list)
    nil))



(defvar jka-compr-temp-name-template
  "/usr/tmp/jka-compr")


(defun jka-compr-write-region (start end filename &optional append visit)
  "Documented as original."
  (interactive "r\nFWrite region to file: ")
  (cond
   (jka-compr-enabled
    (let (zfile)
      (setq filename (expand-file-name filename))
      (setq zfile (or (jka-compr-find-compressed-version filename)
		      filename))
      (or
       (string= zfile filename)
       (if append

	   (and
	    (or (not jka-compr-verify-append-file-change)
		(yes-or-no-p (format "Append to file %s? " zfile)))
	    (setq filename zfile))

	 (and
	  (or (not jka-compr-verify-overwrite-file-change)
	      (yes-or-no-p (format "Overwrite file %s? " zfile)))
	  (setq filename zfile)))))

    (let ((info (jka-compr-get-compression-info filename)))

      (if info

	  (let ((can-append (jka-compr-info-can-append info))
		(compress-program (jka-compr-info-compress-program info))
		(compress-message (jka-compr-info-compress-message info))
		(uncompress-program (jka-compr-info-uncompress-program info))
		(uncompress-message (jka-compr-info-uncompress-message info))
		(compress-args (jka-compr-info-compress-args info))
		(uncompress-args (jka-compr-info-uncompress-args info))
		(discard-err (jka-compr-info-compress-discard-err info))
		(temp-file (make-temp-name jka-compr-temp-name-template))
		cbuf temp-buffer)

	    (or
	     discard-err
	     (progn
	       (setq cbuf (current-buffer)
		     temp-buffer (get-buffer-create " *jka-compr-temp*"))
	       (set-buffer temp-buffer)
	       (widen) (erase-buffer)
	       (set-buffer cbuf)))

	    (and append
		 (not can-append)
		 (jka-compr-real-file-exists-p filename)
		 (progn
		      
		   (and
		    uncompress-message
		    (message "%sing %s..." uncompress-message
			     (file-name-nondirectory filename)))

		   (if discard-err

		       (call-process
			jka-compr-shell filename nil nil
			"-c" (format "%s -c %s 2> /dev/null > %s"
				     uncompress-program
				     (mapconcat (function (lambda (x) x))
						uncompress-args
						" ")
				     temp-file))
			   
		     (apply 'call-process
			    uncompress-program filename temp-buffer nil
			    uncompress-args)
		     (set-buffer temp-buffer)
		     (jka-compr-real-write-region (point-min) (point-max)
						  temp-file)
		     (erase-buffer)
		     (set-buffer cbuf))

		   (and
		    uncompress-message
		    (message "%sing %s...done" uncompress-message
			     (file-name-nondirectory filename)))))

	    (and 
	     compress-message
	     (message "%sing %s..." compress-message
		      (file-name-nondirectory filename)))

	    (jka-compr-real-write-region start end temp-file t 'dont)

	    (if discard-err

		;; this seems a little dangerous
		(call-process
		 jka-compr-shell temp-file nil nil
		 "-c" (format "%s -c %s 2> /dev/null %s %s"
			      compress-program
			      (mapconcat (function (lambda (x) x))
					 compress-args " ")
			      (if (and append can-append) ">>" ">")
			      filename))

	      (apply 'call-process
		     compress-program temp-file temp-buffer nil
		     compress-args)
	      (set-buffer temp-buffer)
	      (jka-compr-real-write-region (point-min) (point-max)
					   filename (and append can-append))
	      (erase-buffer)
	      (set-buffer cbuf))

	    (jka-compr-real-delete-file temp-file)

	    (and
	     compress-message
	     (message "%sing %s...done" compress-message
		      (file-name-nondirectory filename)))

	    (and
	     (eq visit t)
	     (progn
	       ;; set visited file name and buffer file modtime
	       (clear-visited-file-modtime)
	       (jka-compr-real-write-region start start filename t t)))

	    (if (and visit (not (eq visit t)))
		nil
	      (message "Wrote %s" filename)
	      nil))
	      
	(jka-compr-real-write-region start end filename append visit))))
   (t
    (jka-compr-real-write-region start end filename append visit))))
    


(defun jka-compr-insert-file-contents (filename &optional visit)
  "Documented as original."

  (cond
   (jka-compr-enabled

    (barf-if-buffer-read-only)

    (setq filename (or (jka-compr-find-compressed-version filename)
		       filename))

    (let ((info (jka-compr-get-compression-info filename)))

      (if info

	  (let ((magic (jka-compr-info-magic info))
		(uncompress-message (jka-compr-info-uncompress-message info))
		(uncompress-program (jka-compr-info-uncompress-program info))
		(discard-err (jka-compr-info-uncompress-discard-err info))
		(uncompress-args (jka-compr-info-uncompress-args info))
		(temp-file (make-temp-name jka-compr-temp-name-template))
		(notfound nil)
		size start)

	    (and
	     uncompress-message
	     (message "%sing %s..." uncompress-message
		      (file-name-nondirectory filename)))

	    (condition-case error-code

		(if discard-err

		    (progn
		      (call-process
		       jka-compr-shell filename nil nil 
		       "-c" (format "%s -c %s 2> /dev/null > %s"
				    uncompress-program
				    (mapconcat (function (lambda (x) x))
					       uncompress-args
					       " ")
				    temp-file))
		      (setq size (nth 1 (jka-compr-real-insert-file-contents
					 temp-file visit)))
		      (jka-compr-real-delete-file temp-file))

		  (setq start (point))
		  (apply 'call-process
			 uncompress-program filename t nil
			 uncompress-args)
		  (setq size (- (point) start))
		  (goto-char start))

	      (error
	       (if (and (eq (car error-code) 'file-error)
			(eq (nth 3 error-code) filename))
		   (if visit
		       (setq notfound error-code)
		       (signal 'file-error 
			       (cons "Openning input file"
				     (nthcdr 2 error-code))))
		 (signal (car error-code) (cdr error-code)))))
		   

	    (and
	     visit
	     (setq buffer-file-name filename)
	     ;; this sets the save_modified field of the buffer
	     (set-buffer-modified-p nil)
	     ;; this sets the auto_save_modified and save_length of the buffer
	     (set-buffer-auto-saved)
	     ;; attempt to set the modtime of the buffer by doing a
	     ;; dummy write to the file.
	     (jka-compr-real-write-region (point) (point) filename t t))
	    
	    (and
	     uncompress-message
	     (message "%sing %s...done" uncompress-message
		      (file-name-nondirectory filename)))

	    (and
	     visit
	     notfound
	     (signal 'file-error
		     (cons "Openning input file" (nth 2 notfound))))

	    (list filename size))

	(jka-compr-real-insert-file-contents filename visit))))
   (t
    (jka-compr-real-insert-file-contents filename visit))))
    


;;; This originally came from uncompress.el
(defun jka-compr-find-compressed-version (filename)
  "If FILENAME does not exist, try to find a compressed version.
Return the version (extended filename) that is found.  If the file
does not exist and no compressed versions are found, return nil."
  (if (jka-compr-real-file-exists-p filename)
      filename
    (catch 'found-compressed-version
      (mapcar
       (function (lambda (cinfo)
		   (let ((extended-fname (concat filename (aref cinfo 11))))
		     (if (jka-compr-real-file-exists-p extended-fname)
			 (throw 'found-compressed-version extended-fname)))))
       jka-compr-compression-info-list)
      nil)))



;;; There are probably many little functions like this that need to
;;; be defined.  These seem to to the job for info and rmail.


;;; expand-file-name will be overwritten when/if ange-ftp is loaded.
;;; This step makes sure we get a virgin copy of the expand file-name
;;; routine (without actually assuming that ange-ftp will be loaded).
(fset 'jka-compr-real-expand-file-name
      (symbol-function 'expand-file-name))


(defun jka-compr-file-readable-p (file)
  "Documented as original."
  (if jka-compr-enabled
      (and 
       (setq file (jka-compr-real-expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (jka-compr-real-file-readable-p file))
    (jka-compr-real-file-readable-p file)))


(defun jka-compr-file-writable-p (file)
  "Documented as original."
  (if jka-compr-enabled 
      (let* ((efile (jka-compr-real-expand-file-name file))
	     (zfile (and efile (jka-compr-find-compressed-version efile))))
	(if zfile
	    (jka-compr-real-file-writable-p zfile)
	  (file-directory-p (file-name-directory file))))
    (jka-compr-real-file-writable-p file)))


(defun jka-compr-verify-visited-file-modtime (buf)
  "Documented as original."
  (and
   jka-compr-enabled
   (not (jka-compr-real-file-exists-p (buffer-file-name buf)))
   (let* ((bfile (buffer-file-name buf))
	  (efile (jka-compr-real-expand-file-name bfile))
	  (file  (and efile (jka-compr-find-compressed-version efile)))
	  (cbuf (current-buffer)))
     
     (and
      (stringp file)
      (not (string= file bfile))
      (or (not jka-compr-verify-visited-file-change)
	  (yes-or-no-p
	   (format "Change visited file of buffer %s to %s? "
		   (buffer-name buf) file)))
      (set-buffer buf)
      (set-visited-file-name file)
      (set-buffer cbuf))))
  (jka-compr-real-verify-visited-file-modtime buf))


(defun jka-compr-file-symlink-p (file)
  "Documented as original."
  (if jka-compr-enabled
      (and
       (setq file (jka-compr-real-expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (jka-compr-real-file-symlink-p file))
    (jka-compr-real-file-symlink-p file)))


(defun jka-compr-file-attributes (file)
  "Documented as original."
  (if jka-compr-enabled
      (and
       (setq file (jka-compr-real-expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (jka-compr-real-file-attributes file))
    (jka-compr-real-file-attributes file)))


(defun jka-compr-file-exists-p (file)
  "Documented as original."
  (if jka-compr-enabled
      (and
       (setq file (jka-compr-real-expand-file-name file))
       (jka-compr-find-compressed-version file)
       t)
    (jka-compr-real-file-exists-p file)))


(defun jka-compr-delete-file (file)
  "Documented as original."
  (if jka-compr-enabled
      (let (zfile)
	(setq file  (jka-compr-real-expand-file-name file)
	      zfile (or (and file (jka-compr-find-compressed-version file))
			file))
	(and
	 (or (string= zfile file)
	     (not jka-compr-verify-delete-file-change)
	     (yes-or-no-p (format "Delete file %s? "zfile)))
	 (jka-compr-real-delete-file zfile)))

    (jka-compr-real-delete-file file)))


;;; Ange-ftp doesn't overwrite get-file-buffer so it should call the
;;; ange-ftp version of expand-file-name (in case the file is on a
;;; remote system).
(defun jka-compr-get-file-buffer (file)
  "Documented as original."
  (if jka-compr-enabled
      (or
       (jka-compr-real-get-file-buffer file)
       (and
	(setq file (expand-file-name file))
	(setq file (jka-compr-find-compressed-version file))
	(jka-compr-real-get-file-buffer file)))
    (jka-compr-real-get-file-buffer file)))


(defun jka-compr-file-name-sans-versions (name)
  "Documented as original."
  ;; this function assumes that the regexps in jka-compr-compression-info-list
  ;; will find .z extensions even if there are backup flags or version numbers
  ;; attached.
  (jka-compr-real-file-name-sans-versions
   (if jka-compr-enabled
       (jka-compr-remove-compression-extension name)
     name)))


(defun jka-compr-remove-compression-extension (name)
  (catch 'name-sans-compress-extension
    (mapcar
     (function (lambda (x)
		 (if (string-match (aref x 0) name)
		     (throw 'name-sans-compress-extension 
			    (substring name 0 (match-beginning 0))))))
     jka-compr-compression-info-list)
    name))


(defun jka-compr-file-name-completion (file dir)
  "Documented as original."
  (let ((completions (file-name-all-completions file dir)))
    (or
     (try-completion
      file
      (mapcar
       (function (lambda (x)
		   (list (jka-compr-remove-compression-extension x))))
       completions))
     (jka-compr-real-file-name-completion file dir))))


;;; This function was lifted from ange-ftp.  I added some args to make
;;; it a little more general. - jka
(defun jka-compr-overwrite-fn (fun saved-prefix new-prefix overwrite-msg)
  "Replace FUN's function definition with NEW-PREFIX-FUN's, saving the
original definition as SAVED-PREFIX-FUN.  The original documentation is
placed on the new definition suitably augmented.  Third arg, OVERWRITE-MSG,
is tacked on to the doc string of the new fun."

  (let* ((name (symbol-name fun))
	 (saved (intern (concat saved-prefix name)))
	 (new (intern (concat new-prefix name)))
	 (nfun (symbol-function new))
	 (exec-directory (if (or (equal (nth 3 command-line-args) "dump")
				 (equal (nth 4 command-line-args) "dump"))
			     "../etc/"
			   exec-directory)))			 
    
    (while (symbolp nfun)
      (setq nfun (symbol-function nfun)))
    
    (or (fboundp saved)
	(progn
	  (fset saved (symbol-function fun))
	  (fset fun new)))
    
    (let* ((doc-str (jka-compr-safe-documentation saved))
	   (ndoc-str (concat doc-str (and doc-str "\n")
			     overwrite-msg)))
      
      (cond ((listp nfun)
	     ;; Probe to test whether function is in preloaded read-only
	     ;; memory, and if so make writable copy:
	     (condition-case nil
		 (setcar nfun (car nfun))
	       (error
		(setq nfun (copy-sequence nfun)) ; shallow copy only
		(fset new nfun)))
	     (let ((ndoc-cdr (nthcdr 2 nfun)))
	       (if (stringp (car ndoc-cdr))
		   ;; Replace the existing docstring.
		   (setcar ndoc-cdr ndoc-str)
		 ;; There is no docstring.  Insert the overwrite msg.
		 (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
		 (setcar ndoc-cdr overwrite-msg))))
	    (t
	     ;; it's an emacs19 compiled-code object
	     (let ((new-code (append nfun nil))) ; turn it into a list
	       (if (nthcdr 4 new-code)
		   (setcar (nthcdr 4 new-code) ndoc-str)
		 (setcdr (nthcdr 3 new-code) (cons ndoc-str nil)))
	       (fset new (apply 'make-byte-code new-code))))))))


(defun jka-compr-safe-documentation (fun)
  "A documentation function that isn't quite as fragile."
  (condition-case ()
      (documentation fun)
    (error nil)))


(defvar jka-compr-overwrite-list
  '(
    write-region
    insert-file-contents
    file-readable-p
    file-writable-p
    file-symlink-p
    file-attributes
    file-exists-p
    delete-file
    get-file-buffer
    file-name-sans-versions
    file-name-completion
    verify-visited-file-modtime
    )
  "List of functions overwritten by jka-compr-install.")


(mapcar
 (function
  (lambda (fn)
    (jka-compr-overwrite-fn
     fn
     "jka-compr-real-"
     "jka-compr-"
     "Note: This function has been modified to work with jka-compr.")
    )
  )
 jka-compr-overwrite-list)


(provide 'jka-compr)

