;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header.el -- Support for emacs update of headers
;; Author          : Lynn Slater
;; Created On      : Tue Aug  4 17:06:46 1987
;; Last Modified By: Lynn Slater
;; Last Modified On: Tue Sep  6 07:57:22 1988
;; Update Count    : 77
;; Status          : OK to use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1988 Lynn Randolph Slater, Jr.
;; This file might become part of GNU Emacs.
;;
;; This file is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; this file so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.

;; Make this file header.el, byte-compile it in your path

;; History 		
;; 31-Aug-1988		Lynn Slater	
;;    Made the make-revision work in most modes
;;    Added the update-file-name command
;; 1-Mar-1988		Lynn Slater
;;   made the headers be as sensitive as possible to the proper
;;   comment chars.
;; 1-Mar-1988		Lynn Slater
;;   Made the mode be declared in each header
;; 26-Feb-1988		Lynn Slater
;;   added the make-revision call
(provide 'header)
(defvar file-header-update-alist ()
  "list of sets of cons cells of strings and fcn to call")

(defun update-file-header ()
  "If the file has been modified, Scans the values in
   file-header-update-alist applying the fcns immediately after finding the
   matching strings" 
  (interactive)
  (if (and (> (buffer-size) 100) (buffer-modified-p))
      (save-excursion
	(let ((the-list file-header-update-alist)
	      (header 	(buffer-substring
			 1
			 (min 1000 (- (buffer-size) 1))))
	      pt)
	  (setq last-command nil)
	  (while the-list
	    (setq pt (string-match (car (car the-list)) header))
	    (if pt (progn
		     (goto-char (+ 1 (match-end 0)))
		     ;;(update-display)
		     ;;(message "doing %s" the-list)
		     ;;(sit-for 2)
		     (funcall (cdr (car the-list)))))
	    (setq the-list (cdr the-list))
	    )))))

;;(update-file-header)

(defun delete-and-forget-line ()
  (let* ((start (point))
	 (stop  (progn (end-of-line) (point)))
	 (str   (buffer-substring start stop))
	 )
  (delete-region start stop)
  str))

(defun update-write-count ()
  (if (not buffer-read-only)
  (let ((num)
	(str  (delete-and-forget-line)))
    (setq num (car (read-from-string str)))
    (if (not (numberp num))
	(progn
	  (insert str)
	  (error "invalid number for update count '%s'" str))
      (progn
	;;(message "New write count=%s" num)
	(insert (format "%s" (+ 1 num)))))
    )))


(defun update-last-modifier ()
  (if (not buffer-read-only)
      (progn
	(delete-and-forget-line)
	(insert (format "%s" (user-full-name)))
	)))

(defun update-last-modified-date ()
  (if (not buffer-read-only)
      (progn
	(delete-and-forget-line)
	(insert (format "%s" (current-time-string)))
	)))

(setq file-header-update-alist nil)
(setq file-header-update-alist
      (append '(("[ \t]Update Count[ \t]*: " . update-write-count)
		("[ \t]Last Modified On: " . update-last-modified-date)
		("[ \t]Last Modified By: " . update-last-modifier)
		)
	    file-header-update-alist))

;;(setq write-file-hooks nil)
(if (not (memq 'update-file-header write-file-hooks))
    (setq write-file-hooks (cons 'update-file-header write-file-hooks)))

(defun true-mode-name ()
  "Returns the name of the mode in such a form that the mode may be
  re-established by calling the function named by appending '-name' to
  this string.  This differs from the variable called mode-name in that
  this is guaranteed to work while the values held by the variable may
  have embedded spaces or other junk.

  THIS MODE NAME IS GUARANTEED OK TO USE IN THE MODE LINE."
  ;;(interactive)
  (let ((major-mode-name (symbol-name major-mode)))
    (capitalize (substring major-mode-name 0
			   (or   (string-match "-mode" major-mode-name)
				 (length major-mode-name))))))

(defun generate-top-line-declarations ()
  "Makes the string that declares the mode in the top line"
  (concat " -*- Mode: " (true-mode-name) " -*- "))

(defun header-prefix-string ()
  (let ((comment-end-p (and comment-end
			    (not (string-equal comment-end "")))))

    (cond
     ((and comment-start (= (length comment-start) 1))
      (concat comment-start comment-start " "))
     ;; Special case, three letter comment starts
     ((and comment-start (= (length comment-start) 3)
	   (equal (aref comment-start 1) (aref comment-start 0)))
      (concat comment-start))
     ((and comment-start (= (length comment-start) 3))
      (concat " " (list (aref comment-start 1)) " "))
     ((and comment-start (not comment-end-p))
      ;; I must start the comment every time
      (concat comment-start))
     (t;; I now presume that the comment carries over, but I
      ;; have no idea what is a good block start character.
      ;; I will use lisp as a default.
      ";; "))
    ))

(defun make-header ()
  "Makes a standard file header at the top of the buffer"
  (interactive)
  (beginning-of-buffer)
  (let* ((mode-declaration (generate-top-line-declarations))
	 (md-length (length mode-declaration))
	 (comment-start-p (and comment-start
			       (not (string-equal comment-start ""))))
	 (comment-end-p (and comment-end
			     (not (string-equal comment-end ""))))
	 (start-line
	  (cond
	   ((and comment-start (= (length comment-start) 1))
	    ;; I will presume that the comment start character is
	    ;; the filler character.
	    (concat comment-start comment-start
		    (make-string (/ (- 77 md-length) 2)
				 (aref comment-start 0))
		    mode-declaration
		    (make-string (/ (- 78 md-length) 2)
				 (aref comment-start 0))
		    ))
	   (comment-start-p
	    ;; I will presume that spaces will fill the gaps
	    (concat comment-start
		    (make-string (/ (- 79 md-length (length comment-start))
				    2) ?\ )
		    mode-declaration))
	   (t;; there is no known comment-start. Presume lisp
	    (concat ";;"
		    (make-string (/ (- 77 md-length) 2) ?\;)
		    mode-declaration
		    (make-string (/ (- 78 md-length) 2) ?\;)))))
	 (end-line
	  (cond
	   (comment-end-p comment-end)
	   ((and comment-start (= (length comment-start) 1))
	    (make-string 79 (aref comment-start 0)))
	   (comment-start-p comment-start)
	   (t       (make-string 79 ?\;))))

	 (prefix-str;; used in each line of the body of the header
	  (concat "\n" (header-prefix-string)))
	 )
    (insert (concat
	      start-line
	      prefix-str
	      ;;"Buffer          : "
	      (buffer-name)
	      " --- "
	      ;;prefix-str
	      ;;"Description     : "
	      prefix-str
	      ;;prefix-str
	      "Author          : " (user-full-name)
	      prefix-str
	      "Created On      : "  (current-time-string)
	      prefix-str
	      "Last Modified By: "
	      prefix-str
	      "Last Modified On: "
	      prefix-str
	      "Update Count    : 0"
	      prefix-str
	      "Status          : Unknown, Use with caution!"

	      ;;prefix-str
	      ;;prefix-str
	      ;;"$Locker$"			;for rcs use
	      ;;prefix-str
	      ;;"$Log$"
	      "\n"
	      end-line
	      "\n\n"
	      ))
    (goto-line 2)
    (end-of-line)))

(defun current-d-m-y-string ()
  (let ((str (current-time-string)))
    (concat (if (equal ?\  (aref str 8))
		       (substring str 9 10)
		       (substring str 8 10))
	    "-"
	    (substring str 4 7)
	    "-"
	    (substring str 20 24))))

(defun make-revision ()
  "Makes a revision marker"
  (interactive)
  (let ((logical-comment-start
	 (if (= (length comment-start) 1)
	     (concat comment-start comment-start " ")
	   comment-start)))
    (beginning-of-buffer)
    (if (re-search-forward (concat "^"
				   (regexp-quote (header-prefix-string))
				   " *History") () t)
	(progn (end-of-line))
      (progn
	(beginning-of-buffer)
	;; find the status line
	(if (re-search-forward (concat "^"
				       (regexp-quote (header-prefix-string))
				       " *Status.*\n") () t)
	    (progn
	      (next-line 1)
	      (insert "\n"
		      logical-comment-start
		      "History \t\t"
		      comment-end))
	  (error "Cannot figure out where to insert the revision!"))))
    (insert 		    "\n"
			    logical-comment-start
			    (current-d-m-y-string)
			    "\t\t"
			    (user-full-name)
			    "\t"
			    comment-end
			    "\n"
			    logical-comment-start
			    "   "
			    )
    (save-excursion (insert "\n"))))


(defun make-divisor (&optional end-col)
  "A divisor line is the comment start, filler, and the comment end"
  (interactive)
  (insert comment-start)
  (if (= 1 (length comment-start))
      (insert comment-start))
  (insert " ")
  (insert (make-string (max 2
			    (- (or end-col (- fill-column 2)) (length
							       comment-end)
			       2 (current-column)))
                       (aref comment-start
			     (if (= 1 (length comment-start)) 0 1))
		       ))
  (insert " " comment-end)
  )

(defun update-file-name ()
  (if (not buffer-read-only)
  ;; look for the file name in the first three lines
  (save-excursion
    (beginning-of-buffer)
    (forward-line 4)
    (let ((stop (point))
	  b e)
      (beginning-of-buffer)
      (if (re-search-forward (concat "^"
				     (regexp-quote (header-prefix-string))
				     " *\\(.*\\) *\\-\\-") () t)
	  (progn
	    (setq b (match-beginning 1))
	    (setq e (match-end 1))
	    (kill-region b e)
	    (goto-char b)
	    (insert (file-name-nondirectory (buffer-file-name))" ")
	    ))))))


;; need a make-revision
;; need ez access to values in the header bos
;; need a header-presentp fcn

;;(setq write-file-hooks ())
;;(setq file-header-update-alist ())
;;(load "header")

