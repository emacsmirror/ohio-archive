;Author: Eyvind Ness (eyvind) 
;Date:   Wednesday, July 22 1992 13:38 GMT
;File:   /usr/local/gnu/emacs/elisp/rpc-hm-1.0/init.el

;;;
;;; See lisp/bytecomp.el
;;; (require 'byte-compile)
;;; Ref. batch-byte-compile-file byte-recompile-directory
  
;;;
;;; Makes sure the load-path is extend (to be able autoload the extra
;;; snacks like ilisp while compiling):

(defun _init ()
  (setq load-path
	(append
	 '(nil)
	 (list (expand-file-name
		(concat exec-directory "../../elisp/ilisp-4.11")))
	 (list
	  (expand-file-name
	   (concat exec-directory "../../elisp/site-extensions")))
	 load-path)))


(defun _batch-byte-compile-file (file)
  (condition-case err
      (prog1 t (byte-compile-file file))
    (error
     (message (if (cdr err)
		  "===> Error processing %s: %s \"%s\""
		  "===> Error processing %s: %s")
	      file
	      (get (car err) 'error-message)
	      (prin1-to-string (cdr err)))
     nil)))


(defun _makedir (directory)
  "Byte compiles all .el files that needs recompiling in DIRECTORY.
Returns non-nil if one or more compilations failed."
  
  (setq directory (expand-file-name directory))
  (let ((files (directory-files directory nil "\\.el$"))
	(count 0)
	(errorp nil)
	(inputfile nil)
	(outputbuffer (get-buffer-create "*Grep Output*"))
	(displaybufferp nil)
	(missingokp nil)
	(nomessagep nil)
	(nosuffixp t)
	source dest)
    (while files
      (if (and (not (auto-save-file-name-p (car files)))
	       (setq source (expand-file-name (car files) directory))
	       (setq dest (concat (file-name-sans-versions source) "c"))
	       (or (not (file-exists-p dest))
		   (file-newer-than-file-p source dest)))
	  ;; Source needs recompiling.
	  (progn
	    (setq count (1+ count))
	    (and
	     ;; Check if there are macro defs in source.
	     (save-excursion
	       (set-buffer outputbuffer)
	       (erase-buffer)
	       (call-process
		"/bin/sh"
		inputfile outputbuffer displaybufferp 
		"-c"
		(concat
		 "(grep '^(defmacro ' " source 
		 " >/dev/null ) && echo found-some-macros"))
	       (or 
		(< (point-max) 2)	; OK - No defmacros.
		;; Else, there are macro defs in source. Make sure these
		;; are loaded before trying to byte-compile it. 
		(condition-case err
		    (prog1 t
		      (load source missingokp nomessagep nosuffixp))
		  (error
		   (prog1 nil
		     (setq errorp t)
		     (message
		      (if (cdr err)
			  "===> Error processing %s: %s \"%s\""
			"===> Error processing %s: %s")
		      source
		      (get (car err) 'error-message)
		      (prin1-to-string (cdr err))))))))
	     ;; We don't want to byte-compile anything that didn't load.
	     (or (_batch-byte-compile-file source) (setq errorp t)))))
      (setq files (cdr files)))
    (message "Done (Total of %d file%s compiled)"
	     count (if (= count 1) "" "s"))
    errorp))


(defun _make ()
  (message "%s\n" (emacs-version))
  (kill-emacs (if (_makedir ".") 1 0)))

;;; End of file.
