;;; mew-mime.el --- MIME launcher for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-mime-version "mew-mime.el version 0.13")

(require 'mew)

;;
;;
;;

(defmacro mew-attr-by-ct (ct)
  (` (mew-assoc-match2 (, ct) mew-mime-content-type 0)))

(defmacro mew-attr-by-file (ct)
  (` (mew-assoc-match2 (, ct) mew-mime-content-type 1)))

(defmacro mew-attr-get-ct (attr)
  (` (nth 0 (, attr))))

(defmacro mew-attr-get-cte (attr)
  (` (symbol-value (nth 2 (, attr)))))

(defun mew-attr-get-prog (attr)
  (let ((val (symbol-value (nth 3 attr))))
    (if (and (listp val) (equal 'if (car val)))
	(setq val (eval val)))
    (nth 0 val)))

(defun mew-attr-get-opt (attr)
  (let ((val (symbol-value (nth 3 attr))))
    (if (and (listp val) (equal 'if (car val)))
	(setq val (eval val)))
    (nth 1 val)))

(defun mew-attr-get-async (attr)
  (let ((val (symbol-value (nth 3 attr))))
    (if (and (listp val) (equal 'if (car val)))
	(setq val (eval val)))
    (nth 2 val)))

(defmacro mew-attr-get-icon (attr)
  (` (symbol-value (nth 4 (, attr)))))

(defvar mew-process-file-alist nil)

;;
;;
;;

(defun mew-mime-start-process (program options file)
  (let ((process-connection-type mew-connection-type1) pro)
    (message "Starting %s ..." program)
    (setq pro (apply (function start-process)
		     (format "*mew %s*" program)
		     mew-buffer-tmp
		     program
		     (append options (list file))))
    (set-process-sentinel pro 'mew-mime-start-process-sentinel)
    (message "Starting %s ... done" program)
    (setq mew-process-file-alist (cons (cons pro file) mew-process-file-alist))
    )
  t ;; to next part
  )

(defun mew-mime-start-process-sentinel (process event)
  (let* ((al (assoc process mew-process-file-alist))
	 (file (cdr al)))
    (if (and mew-delete-temp-file file) (delete-file file))
    (setq mew-process-file-alist (delete al mew-process-file-alist))))

(defun mew-mime-call-process (program options file)
  (message "Calling %s ..." program)
  (apply (function call-process) program file nil nil options)
  (message "Calling %s ... done" program)
  t ;; to next part
  )

;;
;;
;;

(defun mew-mime-part (fullpart nums &optional execute)
  ;; called in message buffer
  ;; if nums is nil, it means singlepart.
  (let* ((syntax  (mew-syntax-get-entry fullpart nums))
	 (begin   (mew-syntax-get-begin syntax))
	 (end     (mew-syntax-get-end   syntax))
	 (ctl     (mew-syntax-get-ct    syntax))
	 (cte     (mew-syntax-get-cte   syntax))
	 (ct      (mew-syntax-get-value  ctl))
	 (cdpl    (mew-syntax-get-cdp syntax))
	 (fname   (and cdpl (mew-syntax-get-param cdpl "filename")))
	 (cd      (mew-syntax-get-cd syntax))
	 (params  (mew-syntax-get-params ctl))
	 (attr    (mew-attr-by-ct ct))
	 (program (mew-attr-get-prog attr))
	 (options (mew-attr-get-opt attr))
	 (async   (mew-attr-get-async attr)))
    (if (symbolp program)
	(if (fboundp program)
	    (cond
	     ((eq program 'mew-mime-message/rfc822)
	      (funcall program syntax)) ;; for recursive MIME
	     ((eq program 'mew-mime-application/octet-stream)
	      (funcall program begin end params ct cte fname))
	     (t
	      (funcall program begin end params execute))))
      (insert " ######  ######  #######  #####  ######     #    #     #\n"
	      " #     # #     # #     # #     # #     #   # #   ##   ##\n"
	      " #     # #     # #     # #       #     #  #   #  # # # #\n"
	      " ######  ######  #     # #  #### ######  #     # #  #  #\n"
	      " #       #   #   #     # #     # #   #   ####### #     #\n"
	      " #       #    #  #     # #     # #    #  #     # #     #\n"
	      " #       #     # #######  #####  #     # #     # #     #\n"
	      "\n\n")
      (mew-insert "Content-Type:\t%s\n" ct)
      (mew-insert "Encoding: \t%s\n" cte)
      (mew-insert "Size:\t\t%d bytes\n"
		  (mew-region-bytes begin end (mew-current-get 'cache)))
      (mew-insert "Filename:\t%s\n" fname)
      (mew-insert "Description: \t%s\n" cd)
      (mew-insert "Program:\t%s\n" program)
      (if (not execute)
	  (insert "\nTo execute this external command, type "
		  (substitute-command-keys
		   "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.")
		  "\nTo save this part, type "
		  (substitute-command-keys
		   "'\\<mew-summary-mode-map>\\[mew-summary-save]'.")
		  "\nTo display this part in Message mode, type "
		  (substitute-command-keys
		   "'\\<mew-summary-mode-map>\\[mew-summary-insert]'."))
	(if (mew-which program exec-path)
	    (let ((file (mew-make-temp-name fname)))
	      (save-excursion
		(set-buffer (mew-current-get 'cache))
		;; NEVER use call-process-region for privacy reasons
		(mew-frwlet 
		 mew-cs-dummy
		 (if (mew-ct-linebasep ct) mew-cs-outfile mew-cs-binary)
		 (write-region begin end file nil 'no-msg))
		(if async
		    (mew-mime-start-process program options file)
		  (mew-mime-call-process program options file))))
	  (message "Program %s is not found" program))))))
;;
;;
;;

(defun mew-mime-image (begin end format)
  (message "Loading image...")
  (cond 
   ((eq format 'xbm) ;; use temporary file.
    (let ((temp-file-name (mew-make-temp-name))
	  glyph)
      (save-excursion
	(set-buffer (mew-current-get 'cache))
	(write-region begin end temp-file-name nil 'no-msg)
	(set-buffer (mew-buffer-message))
	(mew-elet
	 (setq glyph (make-glyph (vector 
				  'xbm
				  :file
				  temp-file-name)))
	 (set-glyph-property glyph 'face 'x-face)
	 (set-extent-end-glyph
	  (mew-overlay-make (point-min) (point-min)) glyph)
	 (if (file-exists-p temp-file-name)
	     (delete-file temp-file-name))))))
   (t
    (set-buffer (mew-buffer-message))
    (mew-elet
     (set-extent-end-glyph (mew-overlay-make (point-min) (point-min))
			   (make-glyph (vector 
					format
					:data
					(buffer-substring 
					 begin end
					 (mew-current-get 'cache))))))))
  (message "Loading image...done"))

(defun mew-mime-image/jpeg (begin end &optional params execute)
  (mew-mime-image begin end 'jpeg))

(defun mew-mime-image/gif (begin end &optional params execute)
  (mew-mime-image begin end 'gif))

(defun mew-mime-image/xbm (begin end &optional params execute)
  (mew-mime-image begin end 'xbm))

(defun mew-mime-image/xpm (begin end &optional params execute)
  (mew-mime-image begin end 'xpm))

(defun mew-mime-image/png (begin end &optional params execute)
  (mew-mime-image begin end 'png))

(defun mew-mime-text/plain (begin end &optional params execute)
  (if (> end begin)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(mew-elet
	 (insert-buffer-substring (mew-current-get 'cache) begin end)
	 ;; Highlight
	 (mew-highlight-url)
	 (mew-highlight-body))
	;; Page breaks
	(if mew-break-pages
	    (progn
	      (goto-char (point-min))
	      (mew-message-narrow-to-page))))))

(defun mew-mime-text/enriched (begin end &optional params execute)
  (if (> end begin)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(mew-elet
	 (insert-buffer-substring (mew-current-get 'cache) begin end)
	 ;; Highlight
	 (if mew-use-text/enriched
	     (progn
	       (format-decode-buffer 'text/enriched)
	       (enriched-mode nil)))
	 (mew-highlight-url)
	 (mew-highlight-body))
	;; Page breaks
	(if mew-break-pages
	    (progn
	      (goto-char (point-min))
	      (mew-message-narrow-to-page))))))

(defun mew-prog-text/html-netscape-remote ()
  (list "-remote" (format mew-prog-text/html-netscape-remote-format file)))

(defun mew-mime-text/html (begin end &optional params execute)
  (mew-elet
   (insert " #     # ####### #     # #\n"
	   " #     #    #    ##   ## #\n"
	   " #     #    #    # # # # #\n"
	   " #######    #    #  #  # #\n"
	   " #     #    #    #     # #\n"
	   " #     #    #    #     # #\n"
	   " #     #    #    #     # #######\n"
	   "\n\n")
   (mew-insert "Size:\t\t%d bytes\n"
	       (mew-region-bytes begin end (mew-current-get 'cache)))
   (insert (format "Browser:\t%s\n"
		   (cond ((and (symbolp mew-prog-text/html)
			       (fboundp mew-prog-text/html))
			  (symbol-name mew-prog-text/html))
			 ((stringp mew-prog-text/html) mew-prog-text/html)
			 (t "none")))
	   "\nTo save this part, type "
	   (substitute-command-keys
	    "'\\<mew-summary-mode-map>\\[mew-summary-save]'.")
	   "\nTo display this part in Message mode, type "
	   (substitute-command-keys
	    "'\\<mew-summary-mode-map>\\[mew-summary-insert]'."))
   (if (null execute)
       (insert "\nTo display this text/html contents with browser, type "
	       (substitute-command-keys
		"'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'."))
     (cond
      ((and (symbolp mew-prog-text/html) (fboundp mew-prog-text/html))
       (let (source)
	 (set-buffer (mew-current-get 'cache))
	 (setq source (buffer-substring begin end))
	 (set-buffer (mew-buffer-message))
	 (mew-erase-buffer)
	 (insert source)
	 (funcall mew-prog-text/html (point-min) (point-max))))
      ((stringp mew-prog-text/html)
       (if (> end begin)
	   (let ((file (format "%s.html" (mew-make-temp-name))) arg)
	     (save-excursion
	       (set-buffer (mew-current-get 'cache))
	       (if mew-prog-text/html-arg-hack
		   (setq arg (funcall mew-prog-text/html-arg-hack))
		 (setq arg (append mew-prog-text/html-arg (list file))))
	       (mew-frwlet
		mew-cs-dummy mew-cs-outfile
		(write-region begin end file nil 'no-msg)
		(apply (function start-process)
		       mew-prog-text/html mew-buffer-tmp mew-prog-text/html
		       arg))))))))))

(defun mew-mime-message/rfc822 (part)
  (mew-elet
   (let* ((hbeg (mew-syntax-get-begin part))
	  (hend (mew-syntax-get-end   part))
	  (cache (mew-current-get 'cache))
	  (body (mew-syntax-get-part part)))
     (insert-buffer-substring cache hbeg hend)
     (mew-header-arrange (point-min) (point-max))
     (cond
      ;; Displaying the text/plain body or the first part of 
      ;; top level multipart if it is text/plain.
      ;; see also mew-syntax-singlepart
      ((mew-syntax-singlepart-p body)
       (mew-mime-part body nil)) ;; nil is single
      ((mew-syntax-multipart-p body)
       (let* ((first (mew-syntax-get-part body))
	      (ct (mew-syntax-get-value (mew-syntax-get-ct first) 'cap)))
	 (if (mew-case-equal ct mew-ct-txt)
	     (let* ((syntax (mew-syntax-get-entry body '(1)))
		    (begin   (mew-syntax-get-begin syntax))
		    (end     (mew-syntax-get-end   syntax)))
	       (mew-mime-text/plain begin end)))))))))

(defun mew-mime-application/octet-stream (begin end &optional params ct cte fl)
  (mew-elet
   (insert " ######    ###   #     #    #    ######  #     #\n"
	   " #     #    #    ##    #   # #   #     #  #   #\n"
	   " #     #    #    # #   #  #   #  #     #   # #\n"
	   " ######     #    #  #  # #     # ######     #\n"
	   " #     #    #    #   # # ####### #   #      #\n"
	   " #     #    #    #    ## #     # #    #     #\n"
	   " ######    ###   #     # #     # #     #    #\n"
	   "\n\n")
   (mew-insert "Content-Type:\t%s\n" ct)
   (mew-insert "Encoding: \t%s\n" cte)
   (and params
	(insert
	 (format
	  "Parameters:\t%s\n"
	  (mapconcat
	   (function mew-header-sanity-check-string)
	   (mapcar (function (lambda (x) (concat (nth 0 x) "=" (nth 1 x))))
		   params)
	   ", "))))
   (mew-insert "Size:\t\t%d bytes\n"
	       (mew-region-bytes begin end (mew-current-get 'cache)))
   (mew-insert "Filename:\t%s\n" fl)
   (insert "\nTo save this part, type "
	   (substitute-command-keys
	    "'\\<mew-summary-mode-map>\\[mew-summary-save]'.")
	   "\nTo display this part in Message mode, type "
	   (substitute-command-keys
	    "'\\<mew-summary-mode-map>\\[mew-summary-insert]'."))))

(defun mew-summary-insert ()
  "Insert row message or part into Message mode."
  (interactive)
  (let* ((ofld-msg (mew-current-get 'message))
	 (msg (mew-summary-message-number))
	 (part (mew-syntax-nums))
	 (buf (buffer-name)))
    (if (or msg (not part))
	(mew-summary-display 'force)
      (unwind-protect
	  (progn
	    (mew-summary-toggle-disp-msg 'on)
	    (mew-window-configure buf 'message)
	    (set-buffer (mew-buffer-message))
	    (mew-elet
	     (let* ((syntax (mew-cache-decode-syntax (mew-cache-hit ofld-msg)))
		    (stx (mew-syntax-get-entry syntax part))
		    (begin (mew-syntax-get-begin stx))
		    (end (mew-syntax-get-end stx)))
	       (mew-erase-buffer)
	       (insert-buffer-substring (mew-current-get 'cache) begin end)
	       (run-hooks 'mew-message-hook)
	       (mew-message-set-end-of)
	       (goto-char (point-min))))
	    (mew-pop-to-buffer buf))))))

(provide 'mew-mime)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-mime.el ends here
