;; rcp.el
;; Module to do remote editing via rcp.  Assume .rhosts files are
;; set up properly on both machines. 
;; Modeled after ftp.el by MLY.PREP.AI.MIT.EDU
;;
;; Nick Tran
;; University of Minnesota
;; Summer 87
;;

(make-variable-buffer-local 'default-remote-directory)
(make-variable-buffer-local 'buffer-remote-file-name)
(setq-default default-remote-directory "sb:")

(defun find-remote-file ()
  (interactive)
  (let (proc temp s host file time default (olddir default-directory))
    (setq s "")
    (while (not (string-match "\\`[ \t]*\\([^ \t:]+\\)[ \t]*:\\(.+\\)\\'" s))
      (setq s (read-string "Find remote file: " default-remote-directory)))
    (setq host (substring s (match-beginning 1) (match-end 1)))
    (setq file (substring s (match-beginning 2) (match-end 2)))
    
    (if (string-match "[^\t /]+$" file)
	(and (setq temp (substring file (match-beginning 0) nil))
	     (setq default (substring file 0 (match-beginning 0))))
      (and (setq temp file) (setq default "")))
    (setq temp (concat "/tmp/" temp))
    (setq proc (start-process temp nil "/bin/rcp"
	  (concat host ":" file) temp))
    (setq default-remote-directory (concat host ":" default))
    (message "Finding %s:%s ..." host file)
    (setq time 0)
    (while (eq (process-status proc) 'run)
      (setq time (1+ time))
      (sleep-for 1)
	)
    (if (and (eq (process-status proc) 'exit)
	     (eq (process-exit-status proc) 0))
	(progn
	  (find-file-read-only temp)
	  (message "%d bytes in %d seconds (~ %d Kb/s)"
		   (buffer-size) time (/ (buffer-size) (* time 1024)))
	  (delete-file temp)
	  (setq buffer-file-name "")
	  (setq buffer-remote-file-name (concat host ":" file))
	  (setq default-remote-directory (concat host ":" default))
	  (setq default-directory olddir)
	  )
      (message "Find-remote-file failed (exit status %d)"
	       (process-exit-status proc)))
          ))

(defun save-remote-file()
  (interactive)
  (cond ((eq 0 (length buffer-remote-file-name)) (write-remote-file))
	( t (do-write-remote-file buffer-remote-file-name))))

(defun write-remote-file ()
  (interactive)
  (let ((s ""))
    (while (not (string-match "\\`[ \t]*\\([^ \t:]+\\)[ \t]*:\\(.+\\)\\'" s))
	  (or (not (and (string= s "")
		   (eq 0 (length (setq s buffer-remote-file-name)))))
	      (setq s default-remote-directory))
	  (setq s (read-string "Write remote file: " s)))

    (do-write-remote-file s)))

(defun do-write-remote-file (s)
  (if (buffer-modified-p)
      (let (host file temp proc time (size (buffer-size))
		 (olddir default-directory))
	(string-match "\\`[ \t]*\\([^ \t:]+\\)[ \t]*:\\(.+\\)\\'" s)
	(setq host (substring s (match-beginning 1) (match-end 1)))
	(setq file (substring s (match-beginning 2) (match-end 2)))
	(setq temp (concat "/tmp/" (buffer-name)))
	(if (string-match "[^\t /]+$" file)
	    (setq default-remote-directory
		  (concat host ":" (substring file 0 (match-beginning 0))))
	  (setq default (concat host ":")))
	(setq buffer-remote-file-name (concat host ":" file))
	(write-file temp)
	(message "Writing %s:%s ..." host file)
	(setq proc (start-process temp nil "/bin/rcp" temp (concat host ":" file)))
	(setq time 0)
	(while (eq (process-status proc) 'run)
	  (setq time (1+ time))
	  (sleep-for 1)
	  )
	(if (and (eq (process-status proc) 'exit)
		 (eq (process-exit-status proc) 0))
	    (message "%d bytes in %d seconds (~ %d Kb/s)" size time (/ size (* time 1024)))
	  
	  (and (set-buffer-modified-p t)
	       (message "Write-remote-file failed (exit status %d)"
			(process-exit-status proc))))
	(delete-file temp)
	(set-visited-file-name "")
	(setq default-directory olddir)
	)
    (message "(No changes need to be saved)")
    ))
