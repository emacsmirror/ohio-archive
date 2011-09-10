;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;


; These are functions, and function re-definitions so that dungeon can
; be run in batch mode.

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

(defun mprinc (arg)
   (if (stringp arg)
       (send-string-to-terminal arg)
     (send-string-to-terminal (prin1-to-string arg))))

(defun mprincl (arg)
   (if (stringp arg)
       (progn
           (send-string-to-terminal arg)
           (send-string-to-terminal "\n"))
     (send-string-to-terminal (prin1-to-string arg))
     (send-string-to-terminal "\n")))

(defun parse (ignore verblist line)
  (setq line-list (listify-string (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun parse2 (ignore verblist line)
  (setq line-list (listify-string2 (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun read-line ()
  (read-from-minibuffer "" nil dungeon-batch-map))

(setq batch-mode t)

(defun dungeon-batch-loop ()
  (setq dead nil)
  (setq room 0)
  (while (not dead)
    (if (eq dungeon-mode 'dungeon)
	(progn
	  (if (not (= room current-room))
	      (progn
		(describe-room current-room)
		(setq room current-room)))
	  (mprinc ">")
	  (setq line (downcase (read-line)))
	  (if (eq (parse ignore verblist line) -1)
	      (mprinc "I don't understand that.\n"))))))

(defun dos-interface ()
  (dos-boot-msg)
  (setq dungeon-mode 'dos)
  (while (eq dungeon-mode 'dos)
    (dos-prompt)
    (setq line (downcase (read-line)))
    (if (eq (parse2 nil dos-verbs line) -1)
	(progn
	  (sleep-for 1)
	  (mprincl "Bad command or file name"))))
  (goto-char (point-max))
  (mprinc "\n"))

(defun unix-interface ()
    (login)
    (if logged-in
	(progn
	  (setq dungeon-mode 'unix)
	  (while (eq dungeon-mode 'unix)
	    (mprinc "$ ")
	    (setq line (downcase (read-line)))
	    (if (eq (parse2 nil unix-verbs line) -1)
		(let (esign)
		  (if (setq esign (string-match "=" line))
		      (doassign line esign)		
		    (mprinc (car line-list))
		    (mprincl ": not found.")))))
	  (goto-char (point-max))
	  (mprinc "\n"))))

(defun dungeon-nil (arg)
  "noop"
  (interactive "*p"))
