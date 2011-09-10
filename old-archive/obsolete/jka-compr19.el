;;; jka-compr19.el - low level support for reading/writing/loading compressed
;;;                  files in GNU Emacs Version 19.
;;; bugs/comments to jka@ece.cmu.edu
;;; Version: 0.7
;;; Last modified: 12/21/93


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
;;; jka-compr19|Jay Adams|jka@ece.cmu.edu|
;;; Low level support for reading/writing/loading compressed files in GNU Emacs V19|
;;; 21-Dec-1993|0.7|~/misc/jka-compr19.el.Z|


;;; This package implements low-level support for reading, writing,
;;; and loading compressed files.  It hooks into the low-level file I/O
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
;;; dired (note: I don't know if thie still applies in Version 19 of Emacs)
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
;;;       (if (string-match "\\.el\\(\\.[zZ]\\)?$" from-file) nil
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
;;;               (if (string-match "\\.el\\(\\.[zZ]\\)?$" filename)
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
;;;   jka-compr can coexist with crpyt++ if you take all the decryption
;;;   entries out of the crypt-encoding-list.  Clearly problems will
;;;   arise if you have two programs trying to compress/decompress
;;;   files.  jka-compr will not "work with" crypt++: you won't be
;;;   able to decode encrypted compressed files--that is, files that
;;;   have been compressed then encrypted (in that order).
;;;   Theoretically, crypt++ and jka-compr could properly handle a
;;;   file that has been encrypted then compressed, but there is little
;;;   point in trying to compress an encrypted file.



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
;;; 1. Fix it so that the compression extension (.Z or .z) does not
;;;    appear in the buffer name.
;;;
;;; 2. Add the ability to configure translations on the file name to
;;;    be applied before mode-specification.  For instance, .taz files
;;;    should be recognized as being compressed but should be treated
;;;    as .tar files for mode-specification.
;;;
;;; 3. Figure out a good way to handle the beg and end args to
;;;    insert-file-contents.
;;;


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
;;;   June 6, 1993    jka-compr for Version 19 created.  
;;;                   New file name: jka-compr19.el
;;;
;;;                   Added support for the visit argument of
;;;                   write-region being a string.
;;;
;;;                   Changed dummy writes in insert-file-contents and
;;;                   write-region to calls to set-visited-file-name
;;;                   and set-visited-file-modtime.
;;;
;;;                   Modified the compression-info-list to recognize
;;;                   the .gz extension for gzip.
;;;
;;;                   Added checks for the exit status of calls to
;;;                   compression and uncompression programs.
;;;
;;;                   Changed the way the package hooks into file I/O
;;;                   functions (using the file-name-handler-alist
;;;                   instead of overwriting function definitions).
;;;
;;;                   Added a rewrite of ange-ftp-file-name-directory
;;;                   to work around a bug in set-visited-file-modtime.
;;;
;;;    June 7, 1993   Created jka-compr-call-process to cut down on
;;;                   the complexity of jka-compr-write-region and
;;;                   jka-compr-insert-file-contents.
;;;
;;;                   0.3 of jka-compr19 released.
;;;
;;;                   Added a workaround for a bug in rename-file (in 19.12).
;;;
;;;                   0.3a released.
;;;
;;;    June 8, 1993   Added jka-compr-file-local-copy.
;;;
;;;                   Added support for loading compressed files (folded
;;;                   in code from jka-load.el).
;;;
;;;                   Change default directory for
;;;                   jka-compr-temp-name-template to /tmp (was /usr/tmp).
;;;
;;;                   0.4 released.
;;;
;;;    June 9, 1993   Made jka-compr-write-region understand the meaning
;;;                   of a string for the visit argument.
;;;
;;;                   Added a workaround for a bug in copy-file (in 19.13).
;;;
;;;    June 14, 1993  Added support for file-name-completion.
;;;
;;;    June 17, 1993  Changed default values of jka-compr-verify- variables.
;;;
;;;    June 18, 1993  Implement new args BEG and END for
;;;                   jka-compr-insert-file-contents.
;;;
;;;                   Removed bug workarounds for rename-file and
;;;                   copy-file.  This version no longer works for
;;;                   versions of emacs earlier than 19.14.
;;;
;;;    June 21, 1993  Removed the bug workaround for set-vistited-file-modtime.
;;;                   This will no longer work for versions of emacs earlier
;;;                   than 19.15.
;;;
;;;                   Version 0.6
;;;    
;;;    Sept. 17, 1993 Disabled file-name-completion (until the problem
;;;                   with basic-save-buffer can be fixed).
;;;
;;;                   Version 0.6a
;;;
;;;    Oct. 25, 1993  Modified jka-compr-file-name-sans-versions so
;;;                   that compression extension would not be removed.
;;;
;;;                   Added (set-buffer-modified-p nil) to write
;;;                   region so that modified flag will be cleared
;;;                   when a file is saved.
;;;                   
;;;                   Added permanent-local variables,
;;;                   jka-compr-buffer-file-name and
;;;                   jka-compr-original-file-name, to help
;;;                   write-region detect when it compress the file
;;;                   (and where it should write it).
;;;
;;;                   Added a workaround for a bug in write-region.
;;;
;;;                   Version 0.6b
;;;
;;;    Oct. 27, 1993  Added a workaround for a bug in
;;;                   insert-file-contents (similar to the one in
;;;                   write-region).
;;;
;;;    Nov. 10, 1993  Fixed a bug in write-region (filename should have
;;;                   been file).
;;;
;;;                   Version 0.6d
;;;
;;;    Nov. 29, 1993  Fixed the insert-file-contents bug workaround so
;;;                   that buffer-file-name would be correctly expanded.
;;;
;;;                   Version 0.6e
;;;
;;;    Dec. 8, 1993   Changed messages in load so that entire pathname
;;;                   of file is not printed.
;;;
;;;                   Version 0.6f
;;;
;;;    Dec. 16, 1993  Added a workaround to the auto-save bug
;;;                   (diagnosed by Chris Ross, cross@eng.umd.edu).
;;;
;;;                   Added "-q" flag (supress warning messages) to the
;;;                   gzip entries in jka-compr-comprssion-info-list.
;;;
;;;    Dec. 21, 1993  Reenabled file-name-completion.
;;;                   
;;;                   Version 0.7


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
     "zip"         "gzip"         nil  ("-q")
     "unzip"       "gzip"         nil  ("-q" "-d")
     t             ".z"]
    ["\\.gz~?$"     "\037\213"
     "zip"         "gzip"         nil  ("-q")
     "unzip"       "gzip"         nil  ("-q" "-d")
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
(defun jka-compr-info-extension            (info)  (aref (cdr info) 11))


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
  (catch 'compression-info
    (mapcar 'jka-compr-get-compression-info-mapper
	    jka-compr-compression-info-list)
    nil))


(defun jka-compr-call-process (prog infile output temp discard-err args)
  (if discard-err

      (or (zerop
	   (call-process jka-compr-shell infile
			 (if (stringp output) nil output)
			 nil
			 "-c"
			 (format "%s -c %s 2> /dev/null %s"
				 prog
				 (mapconcat (function (lambda (x) x))
					    args
					    " ")
				 (if (stringp output)
				     (concat "> " output)
				   ""))))

	  (error "Non-zero exit status for %s." prog))

    (or (zerop
	 (apply 'call-process
		prog
		infile
		(if (stringp output) temp output)
		nil
		args))
	(error "Non-zero exit status for %s." prog))

    (and (stringp output)
	 (let ((cbuf (current-buffer)))
	   (set-buffer temp)
	   (jka-compr-real-write-region (point-min) (point-max) output)
	   (erase-buffer)
	   (set-buffer cbuf)))))


(defvar jka-compr-temp-name-template
  "/tmp/jka-compr")


(defun jka-compr-write-region (start end file &optional append visit)
  "Documented as original."
  (interactive "r\nFWrite region to file: ")
  (unwind-protect
      (progn
	(setq file-name-handler-alist
	      (delq jka-compr-file-name-handler-entry
		    file-name-handler-alist))
  (cond
   (jka-compr-enabled
    (let ((filename (expand-file-name file))
	  (visited  (and (stringp visit) (expand-file-name visit)))
	  zfile)
      
      (if visited
	  (progn
	    (setq zfile (or (jka-compr-find-compressed-version visited)
			    visited))
	    (or
	     (string= zfile visited)
	     (and
	      (or (not jka-compr-verify-visited-file-change)
		  (yes-or-no-p (format "Change visited file to %s? " zfile)))
	      (setq visit zfile))))

	(setq zfile (or (jka-compr-find-compressed-version filename)
			filename))
	(or
	 (string= zfile filename)
	 (if visit

	     (and (or (not jka-compr-verify-visited-file-change)
		      (yes-or-no-p (format "Change visited file to %s? " zfile)))
		  (setq filename zfile))

	   (if append

	       (and (or (not jka-compr-verify-append-file-change)
			(yes-or-no-p (format "Append to file %s? " zfile)))
		    (setq filename zfile))

	     (and (or (not jka-compr-verify-overwrite-file-change)
		      (yes-or-no-p (format "Overwrite file %s? " zfile)))
		  (setq filename zfile))))))

      (let ((info (jka-compr-get-compression-info zfile)))

	(if info

	    (let ((can-append (jka-compr-info-can-append info))
		  (compress-program (jka-compr-info-compress-program info))
		  (compress-message (jka-compr-info-compress-message info))
		  (uncompress-program (jka-compr-info-uncompress-program info))
		  (uncompress-message (jka-compr-info-uncompress-message info))
		  (compress-args (jka-compr-info-compress-args info))
		  (uncompress-args (jka-compr-info-uncompress-args info))
		  (must-discard-stderr (jka-compr-info-compress-discard-err info))
		  (temp-file (make-temp-name jka-compr-temp-name-template))
		  (visit-file (if (stringp visit) visit filename))
		  cbuf temp-buffer)

	      (setq cbuf (current-buffer)
		    temp-buffer (get-buffer-create " *jka-compr-temp*"))
	      (set-buffer temp-buffer)
	      (widen) (erase-buffer)
	      (set-buffer cbuf)

	      (and append
		   (not can-append)
		   (file-exists-p filename)
		   (let* ((local-copy (file-local-copy filename))
			  (local-file (or local-copy filename)))

		     (unwind-protect

			 (progn
		      
			   (and
			    uncompress-message
			    (message "%sing %s..." uncompress-message
				     (file-name-nondirectory visit-file)))

			   (jka-compr-call-process uncompress-program
						   local-file
						   temp-file
						   temp-buffer
						   must-discard-stderr
						   uncompress-args)
			   (and
			    uncompress-message
			    (message "%sing %s...done" uncompress-message
				     (file-name-nondirectory visit-file))))
		     
		       (and
			local-copy
			(file-exists-p local-copy)
			(delete-file local-copy)))))

	      (and 
	       compress-message
	       (message "%sing %s..." compress-message
			(file-name-nondirectory visit-file)))

	      (jka-compr-real-write-region start end temp-file t 'dont)

	      (jka-compr-call-process compress-program
				      temp-file
				      temp-buffer
				      nil
				      must-discard-stderr
				      compress-args)

	      (set-buffer temp-buffer)
	      (jka-compr-real-write-region (point-min) (point-max)
					   filename (and append can-append))
	      (erase-buffer)
	      (set-buffer cbuf)

	      (delete-file temp-file)

	      (and
	       compress-message
	       (message "%sing %s...done" compress-message
			(file-name-nondirectory visit-file)))

	      (cond
	       ((eq visit t)
		(setq buffer-file-name zfile)
		(set-buffer-modified-p nil)
		(set-buffer-auto-saved)
		(set-visited-file-modtime))
	       ((stringp visit)
		(set-visited-file-name visit)
		(set-buffer-modified-p nil)
		(set-buffer-auto-saved)
		(set-visited-file-modtime)))

	      (and (or (eq visit t)
		       (eq visit nil)
		       (stringp visit))
		   (message "Wrote %s" visit-file))

	      nil)
	      
	  (jka-compr-real-write-region start end filename append visit)))))
   (t
    (jka-compr-real-write-region start end file append visit))))

    (setq file-name-handler-alist
	  (cons jka-compr-file-name-handler-entry
		file-name-handler-alist))))


(defun jka-compr-insert-file-contents (filename &optional visit beg end)
  "Documented as original."

  (unwind-protect
      (progn
	(setq file-name-handler-alist
	      (delq jka-compr-file-name-handler-entry
		    file-name-handler-alist))
  (cond
   (jka-compr-enabled

    (barf-if-buffer-read-only)

    (and (or beg end)
	 visit
	 (error "Attempt to visit less than an entire file"))

    (let* ((zfile (expand-file-name
		   (or (jka-compr-find-compressed-version filename)
		       filename)))
	   (info (jka-compr-get-compression-info zfile)))

      (if info

	  (let ((magic (jka-compr-info-magic info))
		(uncompress-message (jka-compr-info-uncompress-message info))
		(uncompress-program (jka-compr-info-uncompress-program info))
		(must-discard-stderr (jka-compr-info-uncompress-discard-err info))
		(uncompress-args (jka-compr-info-uncompress-args info))
		(temp-file (make-temp-name jka-compr-temp-name-template))
		(notfound nil)
		(local-copy (file-local-copy zfile))
		local-file
		size start)

	    (setq local-file (or local-copy zfile))

	    (unwind-protect		; to make sure local-copy gets deleted

		(progn
		  
		  (and
		   uncompress-message
		   (message "%sing %s..." uncompress-message
			    (file-name-nondirectory zfile)))

		  (condition-case error-code

		      (progn
			(setq start (point))
			(jka-compr-call-process uncompress-program
						local-file
						t
						nil
						must-discard-stderr
						uncompress-args)
			(if end
			    (delete-region (+ start end) (point)))
			(if beg
			    (delete-region start (+ start beg)))
			(setq size (- (point) start))
			(goto-char start))
		    
		    (error
		     (if (and (eq (car error-code) 'file-error)
			      (eq (nth 3 error-code) local-file))
			 (if visit
			     (setq notfound error-code)
			   (signal 'file-error 
				   (cons "Openning input file"
					 (nthcdr 2 error-code))))
		       (signal (car error-code) (cdr error-code))))))

	      (and
	       local-copy
	       (file-exists-p local-copy)
	       (delete-file local-copy))

	      (and
	       (file-exists-p temp-file)
	       (delete-file temp-file)))

			      
	    (and
	     visit
	     (progn
	       (setq buffer-file-name zfile)
	       ;;
	       (make-local-variable 'jka-compr-buffer-file-name)
	       (put 'jka-compr-buffer-file-name 'permanent-local t)
	       (setq jka-compr-buffer-file-name zfile)
	       ;;
	       (make-local-variable 'jka-compr-original-file-name)
	       (put 'jka-compr-original-file-name 'permanent-local t)
	       (setq jka-compr-original-fie-name (expand-file-name filename))
	       ;;
	       ;; this sets the save_modified field of the buffer
	       (set-buffer-modified-p nil)
	       ;; this sets the auto_save_modified and save_length of the buffer
	       (set-buffer-auto-saved)
	       ;; set the modtime of the buffer
	       (set-visited-file-modtime)))
	    
	    (and
	     uncompress-message
	     (message "%sing %s...done" uncompress-message
		      (file-name-nondirectory zfile)))

	    (and
	     visit
	     notfound
	     (signal 'file-error
		     (cons "Openning input file" (nth 2 notfound))))

	    (list zfile size))

	(jka-compr-real-insert-file-contents filename visit beg end))))
   (t
    (jka-compr-real-insert-file-contents filename visit beg end))))

    (setq file-name-handler-alist
	  (cons jka-compr-file-name-handler-entry
		file-name-handler-alist))))
    
    


;;; This originally came from uncompress.el
(defun jka-compr-find-compressed-version (filename)
  "If FILENAME does not exist, try to find a compressed version.
Return the version (extended filename) that is found.  If the file
does not exist and no compressed versions are found, return nil."
  (if (file-exists-p filename)
      filename
    (catch 'found-compressed-version
      (mapcar
       (function (lambda (cinfo)
		   (let ((extended-fname (concat filename (aref cinfo 11))))
		     (if (file-exists-p extended-fname)
			 (throw 'found-compressed-version extended-fname)))))
       jka-compr-compression-info-list)
      nil)))



;;; There are probably many little functions like this that need to
;;; be defined.  These seem to to the job for info and rmail.

(defun jka-compr-file-readable-p (file)
  "Documented as original."
  (if jka-compr-enabled
      (and 
       (setq file (expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (file-readable-p file))
    (file-readable-p file)))


(defun jka-compr-file-writable-p (file)
  "Documented as original."
  (if jka-compr-enabled 
      (let* ((efile (expand-file-name file))
	     (zfile (and efile (jka-compr-find-compressed-version efile))))
	(if zfile
	    (file-writable-p zfile)
	  (file-directory-p (file-name-directory file))))
    (file-writable-p file)))


(defun jka-compr-verify-visited-file-modtime (buf)
  "Documented as original."
  (and
   jka-compr-enabled
   (not (file-exists-p (buffer-file-name buf)))
   (let* ((bfile (buffer-file-name buf))
	  (efile (expand-file-name bfile))
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
      (setq buffer-file-name file)
      (set-buffer cbuf))))
  (verify-visited-file-modtime buf))


(defun jka-compr-file-symlink-p (file)
  "Documented as original."
  (if jka-compr-enabled
      (and
       (setq file (expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (file-symlink-p file))
    (file-symlink-p file)))


(defun jka-compr-file-attributes (file)
  "Documented as original."
  (if jka-compr-enabled
      (and
       (setq file (expand-file-name file))
       (setq file (jka-compr-find-compressed-version file))
       (file-attributes file))
    (file-attributes file)))


(defun jka-compr-file-exists-p (file)
  "Documented as original."
  (and jka-compr-enabled
       (let ((efile (expand-file-name file)))
	 (or (and efile (jka-compr-find-compressed-version efile))
	     (file-exists-p file)))))


(defun jka-compr-delete-file (file)
  "Documented as original."
  (if jka-compr-enabled
      (let (zfile)
	(setq file  (expand-file-name file)
	      zfile (or (and file (jka-compr-find-compressed-version file))
			file))
	(and
	 (or (string= zfile file)
	     (not jka-compr-verify-delete-file-change)
	     (yes-or-no-p (format "Delete file %s? "zfile)))
	 (delete-file zfile)))

    (delete-file file)))


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


(defun jka-compr-file-name-sans-versions (name &optional keep-backup-version)
  "Documented as original."
  ;; this function assumes that the regexps in jka-compr-compression-info-list
  ;; will find .z extensions even if there are backup flags or version numbers
  ;; attached.
  (if jka-compr-enabled
      (file-name-sans-versions name keep-backup-version)
    (file-name-sans-versions name keep-backup-version)))


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
  (let ((completions (file-name-all-completions file dir)))
    (or
     (try-completion
      file
      (mapcar
       (function (lambda (x)
		   (list (jka-compr-remove-compression-extension x))))
       completions))
     (file-name-completion file dir))))


(defun jka-compr-rename-file (file newname &optional noerror)
  (let* ((efile (expand-file-name file))
	 (zfile (or (jka-compr-find-compressed-version efile)
		    efile))
	 info ext new)
    (cond
     ((string= efile zfile)
      (rename-file zfile newname noerror))
     (t
      (setq info (jka-compr-get-compression-info zfile))
      (setq ext (jka-compr-info-extension info))
      (setq new (jka-compr-remove-compression-extension newname))
      (rename-file zfile (concat new ext) noerror)))))


(defun jka-compr-file-local-copy (filename)
  "Documented as original."

  (cond
   (jka-compr-enabled

    (setq filename (or (jka-compr-find-compressed-version filename)
		       filename))

    (let ((info (jka-compr-get-compression-info filename)))

      (if info

	  (let ((magic (jka-compr-info-magic info))
		(uncompress-message (jka-compr-info-uncompress-message info))
		(uncompress-program (jka-compr-info-uncompress-program info))
		(must-discard-stderr (jka-compr-info-uncompress-discard-err info))
		(uncompress-args (jka-compr-info-uncompress-args info))
		(local-copy (file-local-copy filename))
		(temp-file (make-temp-name jka-compr-temp-name-template))
		(temp-buffer (get-buffer-create " *jka-compr-temp*"))
		(notfound nil)
		(cbuf (current-buffer))
		local-file)

	    (setq local-file (or local-copy filename))

	    (unwind-protect

		(progn
		  
		  (and
		   uncompress-message
		   (message "%sing %s..." uncompress-message
			    (file-name-nondirectory filename)))

		  (set-buffer temp-buffer)
		  
		  (jka-compr-call-process uncompress-program
					  local-file
					  t
					  nil
					  must-discard-stderr
					  uncompress-args)

		  (and
		   uncompress-message
		   (message "%sing %s...done" uncompress-message
			    (file-name-nondirectory filename)))

		  (jka-compr-real-write-region
		   (point-min) (point-max) temp-file nil 'dont))

	      (and
	       local-copy
	       (file-exists-p local-copy)
	       (delete-file local-copy))

	      (set-buffer cbuf)
	      (kill-buffer temp-buffer))

	    temp-file)
	    
	(file-local-copy filename))))

   (t
    (file-local-copy filename))))



;;; Support for loading compressed files.  This was originally part of
;;; jka-compr.el.

(defvar jka-compr-lisp-file-extensions '(".elc" ".el" "")
  "List of extensions to try adding to emacs lisp load files.")


;;; This is sort of like the openp routine in lread.c except there is
;;; no exec_only arg and the suffix arg is a list instead of a string.
;;; In fact, if the lisp code looks a little strange here its because
;;; I pretty much transliterated the C version.
(defun jka-compr-openp (path str suffix)
  "Duplicate the function of the openp routing in lread.c."
  (catch 'result
    (let ((absolute (file-name-absolute-p str))
	  filename suf try)
      (while path
	(catch 'continue
	  (setq filename (expand-file-name str (car path)))
	  (if (not (file-name-absolute-p filename))
	      (progn
		(setq filename (expand-file-name str default-directory))
		(if (not (file-name-absolute-p filename))
		    (throw 'continue nil))))

	  (setq suf suffix)
	  (while suf
	    (setq try (concat filename (car suf)))
	    (and (file-readable-p try)
		 (not (file-directory-p try))
		 (throw 'result try))
	    (setq suf (cdr suf))))

	(if absolute
	    (throw 'result nil)
	  (setq path (cdr path)))))

    nil))
      
   
(defun jka-compr-load (file &optional noerror nomessage nosuffix)
  "Documented as original."
  (unwind-protect
      (progn

	(setq file-name-handler-alist
	      (cons jka-compr-file-name-handler-entry
		    file-name-handler-alist))

	(let ((filename
	       (jka-compr-openp load-path file 
				(if nosuffix
				    (cons "" nil)
				  jka-compr-lisp-file-extensions))))
	  (if (not filename)

	      (if noerror
		  nil
		(error "Cannot open load file %s" file))

	    (let ((cbuf (current-buffer))
		  (lbufname (concat " *jka-load-temp:" filename))
		  lbuf)

	      (or nomessage
		  (message "Loading %s..." file))

	      (unwind-protect
		  (progn
		    (setq lbuf (get-buffer lbufname))
		    (if lbuf
			(set-buffer lbuf)
		      (setq lbuf (get-buffer-create lbufname))
		      (set-buffer lbuf)
		      (insert-file-contents filename))
		    (set-buffer cbuf)
		    (eval-buffer lbuf)
		    (let ((after (assoc file after-load-alist)))
		      (and
		       after
		       (apply 'progn (cdr after)))))
		(and
		 lbuf
		 (kill-buffer lbuf)))

	      (or nomessage
		  (message "Loading %s...done." file))))))

    (setq file-name-handler-alist
	  (delq jka-compr-file-name-handler-entry
		file-name-handler-alist))))


(defun jka-compr-file-truename (filename)
  (let ((zfile (or (jka-compr-find-compressed-version filename)
		   filename)))
    (file-truename zfile)))


(defvar jka-compr-file-name-handler-entry
  (cons "[^#]\\'" 'jka-compr-handler)
  "The entry in file-name-handler-alist used by the jka-compr I/O functions.")


(defun jka-compr-handler (operation &rest args)

  (let ((jka-op (intern-soft (symbol-name operation) jka-compr-op-table)))

    (unwind-protect
	(progn
	  (setq file-name-handler-alist
		(delq jka-compr-file-name-handler-entry
		      file-name-handler-alist))
	  (if jka-op
	      (apply jka-op args)
	    (apply operation args)))

      (setq file-name-handler-alist
	    (cons jka-compr-file-name-handler-entry
		  file-name-handler-alist)))))

  
(defvar jka-compr-op-table
  (make-vector 127 0))


(defun jka-compr-intern-operation (op)
  (let ((opsym (intern (symbol-name op) jka-compr-op-table))
	(jka-fn (intern (concat "jka-compr-" (symbol-name op)))))
    (fset opsym jka-fn)))


(defvar jka-compr-operation-list
  '(
;;; write-region
;;; insert-file-contents
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
    file-local-copy
    rename-file
    file-truename
    load
    )
  "List of file operations implemented by jka-compr.")


(mapcar
 (function
  (lambda (fn)
    (jka-compr-intern-operation fn)))
 jka-compr-operation-list)


(defun jka-compr-handler-installed ()
  (let ((fnha file-name-handler-alist))
    (catch 'installed
      (while fnha
	(and (eq (cdr (car fnha)) 'jka-compr-handler)
	     (throw 'installed (car fnha)))
	(setq fnha (cdr fnha)))
      nil)))

      
;;; Add the file I/O hook if it does not already exist.
;;; Make sure that jka-compr-file-name-handler-entry is eq to the
;;; entry for jka-compr in file-name-handler-alist.
(let ((alist-entry (jka-compr-handler-installed)))
  (if alist-entry
      (setq jka-compr-file-name-handler-entry alist-entry)
    (setq file-name-handler-alist
	  (cons jka-compr-file-name-handler-entry
		file-name-handler-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs bug workarounds.
;;;

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


;;; Still have to do get-file-buffer the old way...
(jka-compr-overwrite-fn
 'get-file-buffer
 "jka-compr-real-" "jka-compr-"
 "Note: This function has been modified to work with jka-compr.")

(jka-compr-overwrite-fn
 'write-region
 "jka-compr-real-" "jka-compr-"
 "Note: This function has been modified by jka-compr to work around an emacs bug.")

(jka-compr-overwrite-fn
 'insert-file-contents
 "jka-compr-real-" "jka-compr-"
 "Note: This function has been modified by jka-compr to work around an emacsb ug.")


(provide 'jka-compr)
