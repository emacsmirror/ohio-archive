;;;
;;; users.el
;;; Bill Trost (trost@reed.UUCP)
;;; MCMLXXXVIII
;;;
;;; Emacs lisp code to look up who is on the system.
;;;

(defvar user-list nil "Who was on the system last we looked?")

(defun users (prefix-arg)
  "Show who's on and where"
  (interactive "P")
  (let ((u (snarf-users prefix-arg)))
    (with-output-to-temp-buffer "*users*"
      (save-excursion
	(set-buffer standard-output)
	(while u
	  (insert (car (car u)))
	  (indent-to 10)
	  (insert (car (cdr (car u))))
	  (indent-to 20)
	  (if (equal (car (cdr (cdr (car u)))) "")
	      nil
	    (insert ?( (car (cdr (cdr (car u)))) ?)))
	  (insert ?\n)
	  (setq u (cdr u))))))
  user-list)

(defun snarf-users (with-sort-p)
  "Get list of who's on"
  (setq user-list nil)
  (let ((buf (generate-new-buffer " users"))
	(cursor-in-echo-area t)
	tty user host)
    (message "Scrounging utmp...")
    (set-buffer buf)
    (insert-file-contents "/etc/utmp")
    (while (not (eobp))
      (if (looking-at "\0")
	  (forward-char 36)		; null entry -- skip
	(let ((start (point)))
	  (search-forward "\0")
	  (setq tty (buffer-substring start (1- (point))))
	  (goto-char (+ 8 start))
	  (let ((i 8) (begin (point)))
	    (while (and (> i 0) (not (looking-at "\0")))
	      (setq i (1- i))
	      (forward-char 1))
	    (setq name (buffer-substring begin (point)))
	    (goto-char (+ 8 begin))
	    (setq begin (point))
	    (search-forward "\0")
	    (setq host (if (equal (point) begin)
			   nil
			 (buffer-substring begin (1- (point))))))
	  (goto-char (+ start 36))
	  (if (equal name "")
	      nil
	    (setq user-list (cons (list name tty host) user-list))))))
    (kill-buffer buf))
  (message "")
  (if with-sort-p
      (sort user-list
	    (function (lambda (l m) (string< (car l) (car m)))))
    (reverse user-list)))
