;;
;; Stupid waste of time trying to find a use for x-popup-menu..
;;
;; Use: (find-elisp-file) or M-x find-file-using-menu or something..
;;
;; Steven Grady
;; grady@postgres.berkeley.edu
;; ...!ucbvax!grady
;;

(defun find-elisp-file ()
  "Find some elisp file.  Warning - Slow!"
  (interactive)
  (find-file-using-menu "/usr/new/lib/emacs/lisp" ".el$"))

(defun find-file-using-menu (directory &optional match)
  "Find file from DIRECTORY, with optional MATCH regexp."
  (interactive "DFrom directory? 
sRegexp match? ")
  (let ((menu (make-file-menu directory match)))
    (message "Creating menu...")
    (setq choice (car (x-popup-menu '(20 20) menu)))
    (if choice
	(find-file (concat directory choice)))))

(defun make-file-menu (directory &optional match)
  (let
      ((files (directory-files directory nil match))
       (split-files)
       (one-list)
       (pane)
       (menu '("Files")))
    (setq split-files (split-big-list files 10))
    (while (not (null split-files))
      (setq one-list (car split-files))
      (setq pane (list (make-pane-name one-list)))
      (while (not (null one-list))
	(setq pane (append pane (list (list (car one-list) (car one-list)))))
	(setq one-list (cdr one-list)))
      (setq split-files (cdr split-files))
      (setq menu (append menu (list pane))))
    menu))

(defun make-pane-name (l)
  (concat (car l) "..."))

(defun split-big-list (l n)
  (cond
   ((null l) l)
   (t (let
       ((i 0)
	(s nil))
       (while (< i n)
	 (setq i (+ 1 i))
	 (setq s (append s (list (car l))))
	 (setq l (cdr l))
	 (if (null l) (setq i (+ n 1))))
       (append (list s) (split-big-list l n))))))
