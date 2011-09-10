(require 'cl)
(require 'rnews)

;;;;;;;;;;;;;;;;;;;;; Utility functions

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

;;; Function which takes a verb and a list of other words.  Calls proper
;;; function associated with the verb, and passes along the other words.

(defun doverb (ignore verblist verb rest)
  (if (not verb)
      nil
    (if (member (intern verb) ignore)
	(if (not (car rest)) -1
	  (doverb ignore verblist (car rest) (cdr rest)))
      (if (not (cdr (assq (intern verb) verblist))) -1
	(setq numcmds (1+ numcmds))
	(eval (list (cdr (assq (intern verb) verblist)) (quote rest)))))))


;;; Function to take a string and change it into a list of lowercase words.

(defun listify-string (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match "[ ,:;]" (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun listify-string2 (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match " " (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun replace (list n number)
  (rplaca (nthcdr n list) number))


;;; Get the first non-ignored word from a list.

(defun firstword (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) ignore))
      (setq list (cdr list)))
    (car list)))

(defun firstwordl (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) ignore))
      (setq list (cdr list)))
    list))

;; parse a line passed in as a string  Call the proper verb with the
;; rest of the line passed in as a list.

(defun parse (ignore verblist line)
  (mprinc "\n")
  (setq line-list (listify-string (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun parse2 (ignore verblist line)
  (mprinc "\n")
  (setq line-list (listify-string2 (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

;; Read a line, in window mode

(defun read-line ()
  (let (line)
    (setq line (read-string ""))
    (mprinc line) line))

;; Insert something into the window buffer

(defun minsert (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;; Print something out, in window mode

(defun mprinc (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;; In window mode, keep screen from jumping by keeping last line at
;; the bottom of the screen.

(defun fix-screen ()
  (interactive)
  (forward-line (- 0 (- (window-height) 2 )))
  (set-window-start (selected-window) (point))
  (end-of-buffer))

;; Insert something into the buffer, followed by newline.

(defun minsertl (string)
  (minsert string)
  (minsert "\n"))

;; Print something, followed by a newline.

(defun mprincl (string)
  (mprinc string)
  (mprinc "\n"))

;;;; Function which will get an object number given the list of
;;;; words in the command, except for the verb.

(defun objnum-from-args (obj)
  (let (objnum)
    (setq obj (firstword obj))
    (if (not obj)
	obj-special
      (setq objnum (cdr (assq (intern obj) objnames))))))

(defun objnum-from-args-std (obj)
  (let (result)
  (if (eq (setq result (objnum-from-args obj)) obj-special)
      (mprincl "You must supply an object."))
  (if (eq result nil)
      (mprincl "I don't know what that is."))
  (if (eq result obj-special)
      nil
    result)))

;; Take a short room description, and change spaces and slashes to dashes.

(defun space-to-hyphen (string)
  (let (space)
    (if (setq space (string-match "[ /]" string))
	(progn
	  (setq string (concat (substring string 0 space) "-"
			       (substring string (1+ space))))
	  (space-to-hyphen string))
      string)))

;; Given a unix style pathname, build a list of path components (recursive)

(defun get-path (dirstring startlist)
  (let (slash pos)
    (if (= (length dirstring) 0)
	startlist
      (if (string= (substring dirstring 0 1) "/")
	  (get-path (substring dirstring 1) (append startlist (list "/")))
	(if (not (setq slash (string-match "/" dirstring)))
	    (append startlist (list dirstring))
	  (get-path (substring dirstring (1+ slash))
		    (append startlist
			    (list (substring dirstring 0 slash)))))))))


;; Is a string a member of a string list?

(defun members (string string-list)
  (let (found)
    (setq found nil)
    (dolist (x string-list)
      (if (string= x string)
	  (setq found t))) found))

;; Function to put objects in the treasure room.  Also prints current
;; score to let user know he has scored.

(defun put-objs-in-treas (objlist)
  (let (oscore newscore)
    (setq oscore (reg-score))
    (replace room-objects 0 (append (nth 0 room-objects) objlist))
    (setq newscore (reg-score))
    (if (not (= oscore newscore))
	(score nil))))

;; Load an encrypted file, and eval it.

(defun load-d (filename)
  (let (old-buffer result)
    (setq result t)
    (setq old-buffer (current-buffer))
    (switch-to-buffer (get-buffer-create "*loadc*"))
    (erase-buffer)
    (condition-case nil
	(insert-file-contents filename)
      (error (setq result nil)))
    (unless (not result)
      (condition-case nil
	  (dun-rot13)
	(error (yank)))
      (eval-current-buffer)
      (kill-buffer (current-buffer))
      (switch-to-buffer old-buffer))
    result))

;; Rotate the globals file, and save it for later loading.

(defun compile-globals ()
  (let
    (switch-to-buffer (get-buffer-create "*compd*"))
    (erase-buffer)
    (insert-file-contents "dun-globals.el")
    (dun-rot13)
    (goto-char (point-min))
    (write-region 1 (point-max) "dun-globals.dat")
    (kill-buffer (current-buffer))))

;; Functions to remove an object either from a room, or from inventory.

(defun remove-obj-from-room (room objnum)
  (let (newroom)
    (setq newroom nil)
    (dolist (x (nth room room-objects))
      (if (not (= x objnum))
	  (setq newroom (append newroom (list x)))))
    (rplaca (nthcdr room room-objects) newroom)))

(defun remove-obj-from-inven (objnum)
  (let (new-inven)
    (setq new-inven nil)
    (dolist (x inventory)
      (if (not (= x objnum))
	  (setq new-inven (append new-inven (list x)))))
    (setq inventory new-inven)))

;; Find the global data file.

(defun get-glob-dat ()
  (let (result)
    (setq result nil)
    (dolist (x load-path)
	    (if (file-exists-p (concat x "/dun-globals.dat"))
		(setq result (concat x "/dun-globals.dat"))))
    result))

;; rotate current buffer 13 characters
(let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
  (setq translate-table (make-vector 256 0))
  (while (< i 256)
    (aset translate-table i i)
    (setq i (1+ i)))
  (setq lower (concat lower lower))
  (setq upper (upcase lower))
  (setq i 0)
  (while (< i 26)
    (aset translate-table (+ ?a i) (aref lower (+ i 13)))
    (aset translate-table (+ ?A i) (aref upper (+ i 13)))
      (setq i (1+ i))))
  
(defun dun-rot13 ()
  (let (str len (i 0))
    (setq str (buffer-substring (point-min) (point-max)))
    (setq len (length str))
    (while (< i len)
      (aset str i (aref translate-table (aref str i)))
      (setq i (1+ i)))
    (erase-buffer)
    (insert str)))
