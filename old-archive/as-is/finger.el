;Date: 16 Jan 89 21:52:17 GMT
;From: ucsdhub!isg100!elgar!ag@sdcsvax.ucsd.edu  (Keith Gabryelski)
;Subject: finger.el; finger program for emacs.
;To: info-gnu-emacs@prep.ai.mit.edu
;
;Bill Trost posted users.el that listed the users in a `who' format.
;Here is that program hacked for system V with /etc/passwd lookup.
;
;The fuctions get-passwd-entry (return a /etc/passwd entry for
;USERNAME) and get-group-name (return group name of GID) may be useful
;to someone.
;
;I don't plan working on this code any longer.

;;; finger.el
;;; Keith Gabryelski (ag@wheaties.ai.mit.edu)
;;;
;;; Basic part of this code (read utmp file) snarfed from:
;;;     users.el
;;;     Bill Trost (trost@reed.UUCP)
;;;     MCMLXXXVIII
;;;
;;; Emacs lisp code to look up who is on the system.
;;;

(defvar finger-list nil "Who is on the system?")

(defun finger (prefix-arg)
  "Show who is on the system"
  (interactive "P")
  (let ((u (snarf-users prefix-arg)))
    (with-output-to-temp-buffer "*finger*"
      (save-excursion
	(set-buffer standard-output)
	(insert "User      Group     Fullname                TTY     TTYLOC\n")
	(while u
	  (setq username (car (car u)))
	  (setq ttyname (car (cdr (car u))))
	  (insert username)
	  (indent-to 10)
	  (let ((pass (save-excursion (get-passwd-entry username))))
	    (setq uid (car (cdr (car pass))))
	    (setq gid (car (cdr (cdr (car pass)))))
	    (setq fullname (car (cdr (cdr (cdr (car pass))))))
	    (setq homedir  (car (cdr (cdr (cdr (cdr (car pass)))))))
	    (setq shell (car (cdr (cdr (cdr (cdr (cdr (car pass))))))))
	    (insert
	     (save-excursion (get-group-name (car (cdr (cdr (car pass)))))))
	    (indent-to 20)
	    (insert (car (cdr (cdr (cdr (car pass))))))
	    (indent-to 43)
	    (setq device-name (concat "/dev/" (car (cdr (car u)))))
	    (if (file-writable-p device-name)
		(insert " ")
	      (insert "*"))
	    (insert ttyname)
	    (indent-to 52)
;; do ttyloc thing
	    (setq ttyloc-filename (concat homedir "/.ttyloc"))
	    (if (file-readable-p ttyloc-filename)
		(goto-char (+ (point) (car (cdr (insert-file-contents ttyloc-filename nil))))))
	    (insert ?\n))
	  (setq u (cdr u))))))
  finger-list)

(defun snarf-users (with-sort-p)
  "Get list of who's on"
  (setq finger-list nil)
  (let ((buf (generate-new-buffer " finger"))
	(cursor-in-echo-area t)
	name user)
    (message "Scrounging utmp...")
    (set-buffer buf)
    (insert-file-contents "/etc/utmp")
    (while (not (eobp))
      (if (looking-at "\0")
	  (forward-char 36)		; null entry -- skip
	(let ((start (point)))
	  (search-forward "\0")
	  (setq name (buffer-substring start (1- (point))))
	  (goto-char (+ 12 start))
	  (let ((i 12) (begin (point)))
	    (while (and (> i 0) (not (looking-at "\0")))
	      (setq i (1- i))
	      (forward-char 1))
	    (setq tty (buffer-substring begin (point))))
	  (goto-char (+ start 36))
	  (if (or (equal tty "") (equal name "LOGIN") (equal name "LOGIN1")
		  (equal name "LOGIN2"))
	      nil
	    (setq finger-list
		  (cons (list name tty) finger-list))))))
    (kill-buffer buf))
  (message "")
  (if with-sort-p
      (sort finger-list
	    (function (lambda (l m) (string< (car l) (car m)))))
    (reverse finger-list)))

(defun get-passwd-entry (username)
  "Get first entry from /etc/passwd for username USERNAME and return a
list in the form: (uname uid gid fullname homedir shell)."
  (setq passwd-entry nil)
  (let ((buf (generate-new-buffer " passwd"))
	uname uid gid fullname homedir shell)
    (set-buffer buf)
    (insert-file-contents "/etc/passwd")
    (while (not (eobp))
      (let ((start (point)))
	(search-forward "\n" (1+ (buffer-size)) 'move)
;;	(end-of-line)
	(setq eol (point))
	(goto-char start)
	(search-forward ":" eol 'move)
	(setq uname (buffer-substring start (1- (point))))
	(if (equal username uname)
	    (progn 
	      (search-forward ":" eol 'move)  ;; skip password
	      (if (not (equal (point) eol))
		  (let ((start (point)))
		    (search-forward ":" eol 'move)
		    (setq uid (buffer-substring start (1- (point))))
		(if (not (equal (point) eol))
		    (let ((start (point)))
		      (search-forward ":" eol 'move)
		      (setq gid (buffer-substring start (1- (point))))
		  (if (not (equal (point) eol))
		      (let ((start (point)))
			(search-forward ":" eol 'move)
			(setq fullname (buffer-substring start (1- (point))))
		    (if (not (equal (point) eol))
			(let ((start (point)))
			  (search-forward ":" eol 'move)
			  (setq homedir (buffer-substring start (1- (point))))
		      (if (not (equal (point) eol))
			  (let ((start (point)))
			    (search-forward "\n" eol 'move)
			    (setq shell
				  (buffer-substring start (1- (point))))))))))))))
		   (goto-char (point-max)))
	  (forward-line 1)
	  (setq uname nil))))
    (if (equal uname nil)
	nil
      (setq
       passwd-entry
       (cons (list uname uid gid fullname homedir shell) passwd-entry)))
    (kill-buffer buf))
  passwd-entry)

(defun get-group-name (gid)
  "Get group name for GID"
  (setq group-name nil)
  (let ((buf (generate-new-buffer " group"))
	group)
    (set-buffer buf)
    (insert-file-contents "/etc/group")

    (while (not (eobp))
      (let ((start (point))
	    group-id)
	(search-forward "\n" (1+ (buffer-size)) 'move)
	(setq eol (point))
	(goto-char start)
	(search-forward ":" eol 'move)
	(setq group (buffer-substring start (1- (point))))
	(search-forward ":" eol 'move)
	(if (not (equal (point) eol))
	    (let ((start (point)))
	      (search-forward ":" eol 'move)
	      (setq group-id (buffer-substring start (1- (point))))))

	(if (equal group-id gid)
	    (goto-char (point-max))
	  (forward-line 1)
	  (setq group nil))))

    (setq group-name group)
    (kill-buffer buf))
  group-name)
;-- 
;ag@elgar.CTS.COM         Keith Gabryelski          ...!{ucsd, jack}!elgar!ag

