;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-indent.el - functions to comment and uncomment SQL code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Author:        Peter D. Pezaris <pez@atlantic2.sbi.com>
;;  Maintainer:    sql-mode-help@atlantic2.sbi.com
;;  Version:       0.922 (beta)
;;  Last Modified: Mon Aug  7 19:48:31 1995
;;  Keywords:      isql fsql sql editing major-mode languages
;;
;;  Copyright © 1995 Peter D. Pezaris
;;
;;  This file is part of the SQL Mode package.  Refer to the sql-mode.el
;;  file for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sql-comment-line ()
  "Comment out one line of SQL code.
Insert sql-comment-start-string at the beginning of the line, 
and a sql-comment-end-string at the end."
  (save-excursion
    (back-to-indentation)
    (if (and (not (looking-at sql-comment-start-regexp))
	     (looking-at "."))
	(progn
	  (insert sql-comment-start-string)
	  (end-of-line)
	  (insert sql-comment-end-string)))))

(defun sql-uncomment-line ()
  "Uncomment out one line of SQL code.
Remove sql-comment-start-string and sql-comment-end-string from the line."
  (save-excursion
    (back-to-indentation)
    (if (looking-at sql-commented-line-regexp)
	(progn
	  (delete-char 3)
	  (end-of-line)
	  (backward-delete-char 3)))))
  
(defun sql-comment-line-toggle ()
  "Comment out a line of SQL code, or un-comment it if it is commented."
  (interactive)
  (if (and (interactive-p) (mark))
      (sql-comment-region-toggle)
    (save-excursion
      (back-to-indentation)
      (if (looking-at sql-commented-line-regexp)
	  (sql-uncomment-line)
	(sql-comment-line)))))

(defun sql-comment-buffer ()
  "Comment out each line of SQL code in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line sql-comment-buffer-ignore-lines)
    (sql-comment-line)
    (while (eq 0 (forward-line 1))
      (sql-comment-line))))

(defun sql-uncomment-buffer ()
  "Remove comment from each line of SQL code in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line sql-comment-buffer-ignore-lines)
    (sql-uncomment-line)
    (while (eq 0 (forward-line 1))
      (sql-uncomment-line))))

(defun sql-comment-buffer-toggle ()
  "Toggle the comment state of each line in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line sql-comment-buffer-ignore-lines)
    (sql-comment-line-toggle)
    (while (eq 0 (forward-line 1))
      (sql-comment-line-toggle)
      (end-of-line))))

(defun sql-comment-region ()
  "Comment out each line of SQL code in the current region."
  (interactive)
  (save-excursion
    (if sql-comment-regions-by-line
	(progn
	  (let ((beginning (sql-find-beginning))
		(end (sql-find-end)))
	    (narrow-to-region beginning end)
	    (goto-char (point-min))
	    (sql-comment-line)
	    (while (eq 0 (forward-line 1))
	      (sql-comment-line))
	    (widen)))
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (if (looking-at sql-comment-start-regexp)
	  ()
	(insert sql-comment-start-string)
	(goto-char (point-max))
	(insert sql-comment-end-string))
      (widen)))
  (if sql-deactivate-region
      (zmacs-deactivate-region)))
		   
(defun sql-uncomment-region ()
  "Uncomment each line of SQL code in the current region."
  (interactive)
  (save-excursion
    (if sql-comment-regions-by-line
	(progn
	  (let ((beginning (sql-find-beginning))
		(end (sql-find-end)))
	    (narrow-to-region beginning end)
	    (goto-char (point-min))
	    (sql-uncomment-line)
	    (while (eq 0 (forward-line 1))
	      (sql-uncomment-line))
	    (widen)))
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (if (not (looking-at sql-comment-start-regexp))
	  ()
	(delete-char 3)
	(goto-char (point-max))
	(backward-delete-char 3))
      (widen)))
  (if sql-deactivate-region
      (zmacs-deactivate-region)))

(defun sql-comment-region-toggle ()
  "Toggle comment state of each line in the current region."
  (interactive)
  (save-excursion
    (if sql-comment-regions-by-line
	(progn
	  (let ((beginning (sql-find-beginning))
		(end (sql-find-end)))
	    (narrow-to-region beginning end)
	    (goto-char (point-min))
	    (sql-comment-line-toggle)
	    (while (eq 0 (forward-line 1))
	      (sql-comment-line-toggle))
	    (widen)
	    (if sql-deactivate-region
		(zmacs-deactivate-region))))
      (error "This function is disabled when sql-comment-regions-by-line's value is nil."))))

(defun sql-find-beginning ()
  "Find the location of the first character on the current line."
  (if (> (point) (mark))
      (exchange-point-and-mark))
  (beginning-of-line)
  (point))

(defun sql-find-end ()
  "Find the location of the last character on the current line."
  (if (< (point) (mark))
      (exchange-point-and-mark))
  (end-of-line)
  (point))

