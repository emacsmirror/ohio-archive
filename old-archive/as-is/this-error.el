;Date: Sun, 19 Feb 89 22:29:35 cst
;From: liberte@m.cs.uiuc.edu (Daniel LaLiberte)
;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: Re: grep-command in compile.el
;
;Like Skip, I was also fed up with the linear order of error finding.
;My solution, find-this-error, allows you to move point to whatever
;error you are interested in and the corresponding line in the source
;file will be found.  There are probably better ways to do this;
;I'll leave that as an exercise for the reader.
;
;Dan LaLiberte
;uiucdcs!liberte
;liberte@cs.uiuc.edu
;liberte%a.cs.uiuc.edu@uiucvmd.bitnet
;===

;; find-this-error.el -  Enhancement to compile or grep error processing.
;; Also new grep command uses grep-command constant.

;; Copyright (C) 1989 Dan LaLiberte
;; uiucdcs!liberte
;; liberte@cs.uiuc.edu
;; liberte%a.cs.uiuc.edu@uiucvmd.bitnet

;; find-this-error will find the error that point is on.
;; So do a compile or grep, then call next-error,
;; or move point to some error message and call find-this-error.

;; If you find an error before the last one found, all
;; error messages must be reparsed, which takes some time. 
;; It does this because next-error throws out previous error positions
;; and removes the corresponding marks so that editing is not slowed.

;; load this file and bind some global key to find-this-error
;; (global-set-key "\C-x~" 'find-this-error)

(defun find-this-error ()
  "Find the error that point is on and position to that
skipping all in between.  If error is before the previous error found,
then reparse the errors and try again."
  (interactive)
  (if (eq compilation-error-list t)
      (progn (compilation-forget-errors)
	     (setq compilation-parsing-end 1)))
  (if (not (string= (buffer-name (current-buffer)) "*compilation*"))
      (error "Execute from the *compilation* buffer"))
  (beginning-of-line)
  (let ((error-list compilation-error-list))

    ;; look for point in the error list
    (setq error-list
	  (memq (assoc (point-marker) error-list) error-list))
    
    (if error-list
	;; found, so use it
	(progn
	  (setq compilation-error-list error-list)
	  (select-window (next-window))
	  (find-next-error)
	  )
      ;; not found so restart
      ;; should save error positions as they are found so they may be reused.
      (compilation-forget-errors)
      (setq compilation-parsing-end 1)
      (if compilation-error-list
	  nil
	(save-excursion
	  (switch-to-buffer "*compilation*")
	  (set-buffer-modified-p nil)
	  (compilation-parse-errors)))
      ;; try to find it again
      (let ((error-list compilation-error-list))
	
	;; look again for point in the error list
	(setq error-list
	      (memq (assoc (point-marker) error-list) error-list))
    
	(if error-list
	    ;; found, so use it
	    (progn
	      (setq compilation-error-list error-list)
	      (select-window (next-window))
	      (find-next-error)
	      )
	  (error "Is point on an error message?")
	  )))))


(defun find-next-error ()
  "Do the next-error stuff that finds the next error."
  ;; this is extracted from next-error and could be used by it.
  (let ((next-error (car compilation-error-list)))
    (if (null next-error)
	(error (concat compilation-error-message
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" ""))))
    (setq compilation-error-list (cdr compilation-error-list))
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)
    ))


;; redefine grep command to use grep-command constant

(defconst grep-command "grep -n "
  "*Command name to use for the grep command.")


(defun grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive "sRun grep (with args): ")
  (compile1 (concat grep-command command " /dev/null")
	    "No more grep hits" "grep"))

