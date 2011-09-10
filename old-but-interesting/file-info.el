;;;From: daveemme@amdahl.uts.amdahl.com (Dave Emme)
;;;Subject: A find-file Hack
;;;Date: 24 Oct 88 22:51:01 GMT
;;;Organization: Amdahl Corporation, Sunnyvale CA

;;;Here's a little bit of code to be run as a "find-file-hook".  It runs the
;;;UNIX command "ls -l" on the filename being visited and places the result in
;;;the message line.  This gives you the file size, date last modified,
;;;file permissions, etc. automatically.

;;;Enjoy.

;;;-- Dave

;;; Place file information in the message line when a file is visited.

(defvar display-file-info-buffer "*display-file-info-buffer*")
 
(defun display-file-info ()
  "Run `ls -l <filename>' whenever a file is visited, placing the
output in the message line."
  (let ((temp-buffer-show-hook 'display-file-info-hook)
	(file (cdr (assq 'buffer-file-name (buffer-local-variables)))))
    (with-output-to-temp-buffer display-file-info-buffer
      (call-process "/bin/ls" nil standard-output nil
		    "-l" (expand-file-name file))
      )))

(defun display-file-info-hook (buffer)
  ;; This hook prevents the temporary buffer created by display-file-info
  ;; from being displayed.  Instead it `messages' the information.
  (save-excursion
    (set-buffer display-file-info-buffer)
    (let ((str (buffer-substring (point-min) (1- (point-max)))))
      (message str))
    (kill-buffer buffer)))

(if (not (memq 'display-file-info find-file-hooks))
    (setq find-file-hooks (cons 'display-file-info find-file-hooks)))


-- 
------------------------------------------------------------------------------
	(Insert disclaimer of your choice here)

;;;Dave Emme  (M/S 316)                    daveemme@uts.amdahl.com
;;;Amdahl Corporation            {ames, decwrl, sun, uunet}!amdahl!daveemme
;;;1250 E. Arques Ave.
;;;Sunnyvale, CA 94088
