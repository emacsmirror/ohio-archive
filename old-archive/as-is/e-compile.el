Date: 27 Feb 89 22:21:02 GMT
From: jr%bbn.com@bbn.com  (John Robinson)
Subject: Re: grep-command in compile.el
To: info-gnu-emacs@prep.ai.mit.edu

In article <8902200429.AA30185@m.cs.uiuc.edu>, liberte@M.CS (Daniel LaLiberte) writes:
>My solution, find-this-error, allows you to move point to whatever
>error you are interested in and the corresponding line in the source
>file will be found.  There are probably better ways to do this;
>I'll leave that as an exercise for the reader.

This reader decided the thing to do was to merge Dan's and Skip's
improvements.  I now have the following defuns different from what
Skip posted.

My exercise to the reader is to edit this into your local copy of
compile.el.  I will send it all to by mail if you insist.
--------
;; Well, I finally got fed up enough to do something about it. How many
;; times have you ran a compilation or a grep and wanted to skip over some
;; of the errors (finds) for whatever reason. Well, I wrote a new command
;; that accepts a prefix argument to say how many errors (finds) to skip
;; over.  As the documentation string says, give it a positive prefix arg
;; and it will skip over that many errors. Give it zero as a prefix arg, and
;; it will reparse the errors and start again. I blew off negative prefix
;; args... maybe later.
;; 
;; Enjoy
;; 
;; Charlie Fineman
;; cef@h.cs.cmu.edu!seismo
;; 

(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.  If all
preparsed error messages have been processed, the error message buffer
is checked for new ones.  If the prefix arg is 0, it means reparse the
error message buffer and start at the first error. A positive prefix
arg means move forward that many errors. A negative prefix arg means
move backward that many errors.  Attempting to backup before the first
error or advance past the last error is an error."
  (interactive "p")
  (let ((this-error
	  (if (null argp)
	      (1+ compile-last-error)
	    (+ compile-last-error argp))))
    (if (or (eq compilation-error-list t)
	    (eq argp 0))
	(progn (compilation-forget-errors)
	       (setq compile-last-error -1)
	       (setq this-error 0)
	       (setq compilation-parsing-end 1)))
    (if compilation-error-list
	nil
      (save-excursion
	(switch-to-buffer "*compilation*")
	(set-buffer-modified-p nil)
	(compilation-parse-errors)))
    (show-nth-error this-error)))

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

;; Modified 2/27/89, John Robinson (jr@bbn.com) to work with the
;; nth-error version from Skip Montanaro.

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
  (let ((error-list compilation-error-list)
	(n 0))

    ;; look for point in the error list
    (while (and error-list (not (equal (point-marker) (car (car error-list)))))
      (setq error-list (cdr error-list))
      (setq n (1+ n)))
    
    (if error-list
	;; found, so use it
	(progn
	  (select-window (next-window))
	  (show-nth-error n))
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
      (setq error-list compilation-error-list n 0)
	
      ;; look again for point in the error list
      (while (and error-list (not (eq (point-marker) (car (car error-list)))))
	(setq error-list (cdr error-list))
	(setq n (1+ n)))

      (if error-list
	  ;; found, so use it
	  (progn
	    (select-window (next-window))
	    (show-nth-error n))

	(error "Is point on an error message?")))))

;;; nth-error part of Skip Montanaro's revised compile.el:next-error,
;;; extracted to be of use to Dan LaLiberte's find-this-error.
;;; John Robinson, 2/27/89

(defun show-nth-error (n)
  "Display error N (counting from 0) in the *compilation* buffer."
  (let ((next-error (nth n compilation-error-list)))
    (if (null next-error)
	(error (concat compilation-error-message
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" ""))))
    (if (or (< n 0)
	    (>= n (length compilation-error-list)))
	(error "Error number is out of bounds!"))
    (setq compile-last-error n)
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error))))
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))))
--
/jr
jr@bbn.com or bbn!jr

