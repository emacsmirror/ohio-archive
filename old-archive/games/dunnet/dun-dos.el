;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DOS
;;;;
;;;;;;;;;;;;;;;;;;;

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

(defun dos-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg)
    (setq beg (+ (point) 3))
    (end-of-line)
    (if (not (= beg (point)))
	(let (line)
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (parse2 nil dos-verbs line) -1)
	      (progn
		(sleep-for 1)
		(mprincl "Bad command or file name"))))
      (goto-char (point-max))
      (mprinc "\n"))
    (if (eq dungeon-mode 'dos)
	(progn
	  (fix-screen)
	  (dos-prompt)))))

(defun dos-interface ()
  (dos-boot-msg)
  (setq dungeon-mode 'dos)
  (define-key dungeon-mode-map "\r" 'dos-parse)
  (dos-prompt))

(defun dos-type (args)
  (sleep-for 2)
  (if (setq args (car args))
      (if (string= args "foo.txt")
	  (dos-show-combination)
	(if (string= args "command.com")
	    (mprincl "Cannot type binary files")
	  (mprinc "File not found - ")
	  (mprincl (upcase args))))
    (mprincl "Must supply file name")))

(defun dos-invd (args)
  (sleep-for 1)
  (mprincl "Invalid drive specification"))

(defun dos-dir (args)
  (sleep-for 1)
  (if (or (not (setq args (car args))) (string= args "\\"))
      (mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

COMMAND  COM     47845 04-09-91   2:00a
FOO      TXT        40 01-20-93   1:01a
        2 file(s)      47845 bytes
                     1065280 bytes free
")
    (mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

File not found")))


(defun dos-prompt ()
  (mprinc "A> "))

(defun dos-boot-msg ()
  (sleep-for 3)
  (mprinc "Current time is ")
  (mprincl (substring (current-time-string) 12 20))
  (mprinc "Enter new time: ")
  (read-line)
  (if (not batch-mode)
      (mprinc "\n")))

(defun dos-spawn (args)
  (sleep-for 1)
  (mprincl "Cannot spawn subshell"))

(defun dos-exit (args)
  (setq dungeon-mode 'dungeon)
  (mprincl "\nYou power down the machine and step back.")
  (define-key dungeon-mode-map "\r" 'dungeon-parse)
  (if (not batch-mode)
      (dungeon-messages)))

(defun dos-no-disk ()
  (sleep-for 3)
  (mprincl "Boot sector not found"))


(defun dos-show-combination ()
  (sleep-for 2)
  (mprinc "\nThe combination is ")
  (mprinc combination)
  (mprinc ".\n"))

(defun dos-nil (args))
