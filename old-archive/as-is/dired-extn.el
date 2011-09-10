;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired-extn.el --- extensions to dired
;; Author          : Lynn Slater
;; Created On      : Wed Dec  2 14:08:43 1987
;; Last Modified By: Lynn Slater
;; Last Modified On: Mon Jul 11 15:33:23 1988
;; Update Count    : 27
;; Status          : Safe
;; 
;; $Locker$
;; $Log$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (fboundp 'dired-get-filename))
    (load "dired"))

(defun dired-flag-arbitrary-files (fcn &optional args)
  "Takes fcn and optional args as parameters. Passes each filename
   and args to the fcn.  If fcn returns T, the file is marked for deletion.
   Directories are not passed or ever flaged for deletion."
  (let ((count 0)
	(unique 0))
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (and (not (looking-at "  d"));; directories do not delete
	       ;; like other files, ignore them
	       (not (eolp))
	       ;; note that args will not be a list to the called fcn
	       (apply fcn (cons (dired-get-filename t t) args))
	       (progn (setq count (1+ count))
		      (if (not (looking-at "D"))
			  (setq unique (1+ unique)))
		      (beginning-of-line)
		      (delete-char 1)
		      (insert "D")))
	  (forward-line 1))))
    (message "%s Files flagged for deletion, %s were not previously flagged"
	     count unique)
    ))

(defun filename-matches-string-p (filename string)
  (and (stringp string) (stringp filename)
       (string-match string  filename)))

(defun dired-flag-matching-files (string)
  (interactive "sDelete those files whose names contain the string: ")
  (dired-flag-arbitrary-files 'filename-matches-string-p
			      (list string))
  )

(define-key dired-mode-map "D" 'dired-flag-matching-files)

(defun dired-unflag-all-files ()
  "Removes all flags from all files"
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(delete-char 1)
	(insert " ")
	(forward-line 1))))
  (message "All Flags Removed!")
  )

(define-key dired-mode-map "U" 'dired-unflag-all-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define recursive directory walking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dired-map-dired-dir-lines (fn)
  "Perform fn with point at the end of each directory line except for . and ..:
Arguments to fcn  are the short and long filename"
  (save-excursion
    (let (filename longfilename (buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (looking-at "  d")
	       (not (eolp))
	       (setq filename (dired-get-filename t t)
		     longfilename (dired-get-filename nil t))
	       (if (not (or (equal filename ".") (equal filename "..")))
		   (progn (end-of-line)
			  (funcall fn filename longfilename)))))
	(forward-line 1)))))

(defun dired-directory-linep ()
  "returns t if the current dired line is a directory"
  (save-excursion
    (beginning-of-line)
    (forward-char 2)
    (looking-at "d")))

(defun dired-summarize-hierarchy ()
  "Walks the hierarchy of the current directory and prints each subdirectory
   Gives a single buffer representation of a tree hierarchy."       
  (interactive)
  (let ((summary-indent-level 0))
    (with-output-to-temp-buffer "*Dired Hierarchy*"
      (dired-map-dired-dir-lines 'dired-walk-dir))))

(defun dired-walk-dir (sn ln)
  "Calls dired-summarize-hierarchy on each dir in this dir"
  (princ (format "%s%s" (make-string (* 4 summary-indent-level) 32) sn))
  (message "Looking in %s" ln)
  (if (> 1 summary-indent-level) (sit-for 0))
  (setq summary-indent-level (1+ summary-indent-level))
  (save-excursion
    (dired-find-file)
    (princ (format " %s\n" (or (dired-collect-suffixes) "")))
    (dired-map-dired-dir-lines 'dired-walk-dir))
  (setq summary-indent-level (1- summary-indent-level))
  )

(defun dired-summarize-source-files ()
  "Walks the hierarchy of the current directory and prints each subdirectory
   that contains .c or .h files
   
   You probably want to remove the -a option on the first line
   and to add aliase and # !/bin/csh lines."
  (interactive)
    (with-output-to-temp-buffer "*Dired Hierarchy*"
      (dired-map-dired-dir-lines 'dired-walk-source-dir)))

(defun dired-walk-source-dir (sn ln)
  (message "Looking in %s" ln)
  (save-excursion
    (dired-find-file)
    (let ((suffixes (dired-collect-suffixes)))
      (if (or (assoc "c" suffixes)(assoc "h" suffixes))
	  (progn
	    (princ (format "\netags -tea %s/*.[hc]" ln))))
      (dired-map-dired-dir-lines 'dired-walk-source-dir)))
  )

(defun dired-recursive-dirs ()
  "Prints the long name of all dirdctories under the current directory"
  (interactive)
  (let ((summary-indent-level 0))
    (with-output-to-temp-buffer "*Dired Hierarchy*"
      (dired-map-dired-dir-lines 'dired-print-dirs))))

(defun dired-print-dirs (sn ln)
  (princ (format "%s\n" ln))
  (message "Looking in %s" ln)
  (if (> 1 summary-indent-level) (sit-for 0))
  (setq summary-indent-level (1+ summary-indent-level))
  (save-excursion
    (dired-find-file)
    ;;(princ (format " %s\n" (or (dired-collect-suffixes) "")))
    (dired-map-dired-dir-lines 'dired-print-dirs))
  (setq summary-indent-level (1- summary-indent-level))
  )


(defun file-name-suffix (filename)
  (let ((start (string-match "\\.[^\\.]*$" filename)))
    (if start
	(substring filename (1+ start) (match-end 0)))))

(defun dired-collect-suffixes ()
  "Makes a list of all the suffixes present on non-dirs in this dir"
  (interactive)
  (let ((collected-suffixes nil)
	current-suffix)
    (dired-map-dired-file-lines
      '(lambda (sn ln)
	 (setq current-suffix (file-name-suffix sn))
	 (if (and current-suffix
		  (not (member current-suffix collected-suffixes)))
	     (setq collected-suffixes
		   (cons current-suffix collected-suffixes)))))
    collected-suffixes))

(defun dired-collect-suffixes ()
  "Makes a list of all the suffixes present on non-dirs in this dir"
  (interactive)
  (let ((collected-suffixes nil)
	current-suffix
	key-cell)
    (dired-map-dired-file-lines
      '(lambda (sn ln)
	 (setq current-suffix (file-name-suffix sn))
	 (if (dired-counted-suffixp current-suffix)
	     (progn
	       (setq key-cell (assoc current-suffix collected-suffixes))
	       (if key-cell
		   (setcdr key-cell (1+ (cdr key-cell)))
		 (setq collected-suffixes
		       (cons (cons current-suffix 1) collected-suffixes)))))))
    collected-suffixes))

(defun dired-counted-suffixp (suffix)
  (or (not suffix)
      (and
	(not (char-equal ?~ (aref suffix (1- (length suffix)))))
	(not (char-equal ?# (aref suffix (1- (length suffix)))))
	)))

(defun member (elem list)
  (and list (or (equal elem (car list)) (member elem (cdr list)))))

;; need date sensitive fcns
;; need regular expression matching


