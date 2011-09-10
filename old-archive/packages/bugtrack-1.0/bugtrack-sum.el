;;; bugtrack.el --- Summarize bug reports.

;; Copyright (C) 1992, 1993 Per Cederqvist.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;; Created: 1992-07-29
;; Version: Id: bugtrack-sum.el,v 1.11 1993/12/13 17:21:53 ceder Exp 
;; Keywords: tools, bugtracking, rolodex, bug, track

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
     
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See bugtrack.el and the doc strings.  This file requires cookie.el
;; from Elib.  The commentary in bugtrack.el tells you where to get
;; that file.


;;; Code:

(provide 'bugtrack-sum)
(require 'cookie)
(require 'mail-utils)

;;;; The 'sumlin' data type

(defun bugtrack-create-sumlin (id summary severity priority difficulty
				  edit-user handled-user
				  closed-user last-date)
  "Create a sumlin.
Arguments:
ID        -- numerical bug id.
SUMMARY   -- a line of text.
SEVERITY  -- a number in the interval 0-9.
PRIORITY  -- a number in the interval 0-9.
DIFFICULTY-- a number in the interval 0-9.
EDIT-USER -- login-id of the one who last modified this entry.
HANDLED-USER -- login-id of the one who is handling this bug.
CLOSED-USER -- login-id of the one who closed this bug, or nil if it is open.
LAST-DATE -- last date something happened with this bug."
  (cons 'SUMLIN
	(vector id summary severity priority difficulty edit-user
		handled-user closed-user last-date)))

(defun bugtrack-sumlin-p (object)
  "Return t if OBJECT is a sumlin."
  (eq (car-safe object) 'SUMLIN))

;;; Selectors of sumlin:

(defun bugtrack-sumlin->id (sumlin)
  "Get id from SUMLIN."
  (elt (cdr sumlin) 0))

(defun bugtrack-sumlin->summary (sumlin)
  "Get summary from SUMLIN."
  (elt (cdr sumlin) 1))

(defun bugtrack-sumlin->severity (sumlin)
  "Get severity from SUMLIN."
  (elt (cdr sumlin) 2))

(defun bugtrack-sumlin->priority (sumlin)
  "Get priority from SUMLIN."
  (elt (cdr sumlin) 3))

(defun bugtrack-sumlin->difficulty (sumlin)
  "Get difficulty from SUMLIN."
  (elt (cdr sumlin) 4))

(defun bugtrack-sumlin->edit-user (sumlin)
  "Get edit-user from SUMLIN."
  (elt (cdr sumlin) 5))

(defun bugtrack-sumlin->handled-user (sumlin)
  "Get handled-user from SUMLIN."
  (elt (cdr sumlin) 6))

(defun bugtrack-sumlin->closed-user (sumlin)
  "Get closed-user from SUMLIN."
  (elt (cdr sumlin) 7))

(defun bugtrack-sumlin->last-date (sumlin)
  "Get last-date from SUMLIN."
  (elt (cdr sumlin) 8))


;;; Modifiers:

(defun bugtrack-set-sumlin->id (sumlin newval)
  "Set id in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 0 newval))

(defun bugtrack-set-sumlin->summary (sumlin newval)
  "Set summary in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 1 newval))

(defun bugtrack-set-sumlin->severity (sumlin newval)
  "Set severity in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 2 newval))

(defun bugtrack-set-sumlin->priority (sumlin newval)
  "Set priority in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 3 newval))

(defun bugtrack-set-sumlin->difficulty (sumlin newval)
  "Set difficulty in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 4 newval))

(defun bugtrack-set-sumlin->edit-user (sumlin newval)
  "Set edit-user in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 5 newval))

(defun bugtrack-set-sumlin->handled-user (sumlin newval)
  "Set handled-user in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 6 newval))

(defun bugtrack-set-sumlin->closed-user (sumlin newval)
  "Set closed-user in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 7 newval))

(defun bugtrack-set-sumlin->last-date (sumlin newval)
  "Set last-date in SUMLIN to NEWVAL."
  (aset (cdr sumlin) 8 newval))


;;;; Cookie support functions

(defun bugtrack-sum-pp (cookie)
  "Format COOKIE as a bugtrack summary line."
  (insert
   (format "%c %d %d %d %4d %s %8s %s"
	   (if (bugtrack-sumlin->closed-user cookie) ?C ? )
	   (bugtrack-sumlin->priority cookie)
	   (bugtrack-sumlin->severity cookie)
	   (bugtrack-sumlin->difficulty cookie)
	   (bugtrack-sumlin->id cookie)
	   (mail-string-delete (bugtrack-sumlin->last-date cookie) 11 20)
	   (substring (concat (bugtrack-sumlin->edit-user cookie) "        ")
		      0 8)
	   (bugtrack-sumlin->summary cookie))))


;;;; Summary creation

;;;###autoload
(defun bugtrack-summary-by-id (&optional include-closed)
  "In Bugtrack mode, create a summary.
If optional prefix argument INCLUDE-CLOSED is non-nil, closed
bug reports are also included in the summary."
  (interactive "P")
  (bugtrack-basic-summary include-closed))

;;;###autoload
(defun bugtrack-summary-by-priority (&optional include-closed)
  "In Bugtrack mode, create a summary sorted by priority.
If optional prefix argument INCLUDE-CLOSED is non-nil, closed
bug reports are also included in the summary."
  (interactive "P")
  (bugtrack-basic-summary
   include-closed
   (function (lambda (x1 x2)
	       (> (bugtrack-sumlin->priority x1)
		  (bugtrack-sumlin->priority x2))))))

;;;###autoload
(defun bugtrack-summary-by-severity (&optional include-closed)
  "In Bugtrack mode, create a summary sorted by severity.
If optional prefix argument INCLUDE-CLOSED is non-nil, closed
bug reports are also included in the summary."
  (interactive "P")
  (bugtrack-basic-summary
   include-closed
   (function (lambda (x1 x2)
	       (> (bugtrack-sumlin->severity x1)
		  (bugtrack-sumlin->severity x2))))))

;;;###autoload
(defun bugtrack-summary-by-difficulty (&optional include-closed)
  "In Bugtrack mode, create a summary sorted by difficulty.
If optional prefix argument INCLUDE-CLOSED is non-nil, closed
bug reports are also included in the summary."
  (interactive "P")
  (bugtrack-basic-summary
   include-closed
   (function (lambda (x1 x2)
	       (< (bugtrack-sumlin->difficulty x1)
		  (bugtrack-sumlin->difficulty x2))))))

(defun bugtrack-create-summary-cookie ()
  "Create a cookie for the summary of the current bug."
  (let ((sumlin (bugtrack-create-sumlin
		 nil nil nil nil nil nil nil nil nil)))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Id: ")
      (bugtrack-set-sumlin->id sumlin (read (current-buffer)))
      (search-forward "Summary: ")
      (bugtrack-set-sumlin->summary
       sumlin
       (buffer-substring (point)
			 (save-excursion (end-of-line) (point))))
      (search-forward "\nCurrently handled by: ")
      (bugtrack-set-sumlin->handled-user
       sumlin
       (buffer-substring (point)
			 (save-excursion (end-of-line) (point))))
      (search-forward "\nSeverity: ")
      (bugtrack-set-sumlin->severity sumlin (read (current-buffer)))
      (search-forward "Priority: ")
      (bugtrack-set-sumlin->priority sumlin (read (current-buffer)))
      (search-forward "Difficulty: ")
      (bugtrack-set-sumlin->difficulty sumlin (read (current-buffer)))
      (beginning-of-line 2)
      (if (looking-at "Closed: \\([^:]\\)")
	  (bugtrack-set-sumlin->closed-user
	   sumlin
	   (buffer-substring (match-beginning 1) (match-end 1))))
      (search-forward "Last edit: ")
      (bugtrack-set-sumlin->edit-user
       sumlin
       (buffer-substring
	(point)
	(save-excursion (search-forward ":") (backward-char 1) (point))))
      (search-forward ": ")
      (bugtrack-set-sumlin->last-date
       sumlin
       (buffer-substring (point)
			 (save-excursion (end-of-line) (point)))))
    sumlin))

		 
(defun bugtrack-basic-summary (include-closed &optional sort-function)
  "Bugtrack internal: make summary.
Args: INCLUDE-CLOSED &optional SORT-FUNCTION.
When finished, the bugtrack-summary-handle will be filled with cookies.
If SORT-FUNCTION is given, the summary lines are sorted according
to it. It is given two sumlins and should return t if the first one
should appear first."

  ;; Find or create a buffer for the summary and
  ;; Clear or create the cookie structure.
  (let ((sum-buffer nil))
    (cond
     ((or (not bugtrack-summary-handle)
	  (null (collection-buffer bugtrack-summary-handle)))
      (setq sum-buffer (generate-new-buffer
			(concat (buffer-name (current-buffer)) "-summary")))
      (setq bugtrack-summary-handle
	    (collection-create sum-buffer 'bugtrack-sum-pp
			   "C P S D   Id    Edit date    Edit by   Summary"
			   "")))
     (t
      (setq sum-buffer (collection-buffer bugtrack-summary-handle))
      (collection-clear bugtrack-summary-handle)))

    ;; Set up the buffer in summary mode.
    (let ((basic (current-buffer))
	  (handle bugtrack-summary-handle))
      (save-excursion
	(set-buffer sum-buffer)
	(bugtrack-summary-mode)
	(setq bugtrack-summary-handle handle)
	(setq bugtrack-basic-buffer basic)))

    ;; Create all the summary cookies.
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (point-min))
	(narrow-to-page 1)
	(while (not (= (point-min) (point-max)))
	  (if (or include-closed
		  (not (bugtrack-bug-closed-p)))
	      (cookie-enter-last
	       bugtrack-summary-handle
	       (bugtrack-create-summary-cookie)))
	  (narrow-to-page 1))
	(widen)))

    ;; Sort the cookies, if necessary.
    (if sort-function
	(cookie-sort bugtrack-summary-handle sort-function))
    (set-window-start (display-buffer sum-buffer) 0)))


;;;; The bugtrack summary mode

(defvar bugtrack-summary-mode-map nil
  "Keymap for the Bugtrack summary mode")

(defvar bugtrack-basic-buffer nil
  "In bugtrack summary mode, the buffer that this summary was made from.")

(if bugtrack-summary-mode-map
    nil
  (setq bugtrack-summary-mode-map (make-keymap))
  (suppress-keymap bugtrack-summary-mode-map)
  (define-key bugtrack-summary-mode-map "\C-p" 'bugtrack-sum-previous-line)
  (define-key bugtrack-summary-mode-map "\C-n" 'bugtrack-sum-next-line)
  (define-key bugtrack-summary-mode-map "p" 'bugtrack-summary-goto-previous)
  (define-key bugtrack-summary-mode-map "n" 'bugtrack-summary-goto-next)
  (define-key bugtrack-summary-mode-map "j" 'bugtrack-summary-goto-bug))

(defun bugtrack-summary-mode ()
  "/// Document me! ///"
  (interactive)
  (kill-all-local-variables)
  ;; The following two variables are set in bugtrack-basic-summary.
  (make-variable-buffer-local 'bugtrack-summary-handle)
  (make-variable-buffer-local 'bugtrack-basic-buffer)
  (setq truncate-lines t)
  (use-local-map bugtrack-summary-mode-map)
  (setq mode-name "Bugtrack summary")
  (setq major-mode 'bugtrack-summary-mode))


;;;; Commands in the summary buffer

(defun bugtrack-summary-goto-bug (id)
  "In Bugtrack summary mode, display the current bug.
If a prefix argument is given, display the bug with that Id."
  (interactive "P")
  (let ((old-buffer (current-buffer)))
    (if (null id)
	(let ((tin (tin-locate bugtrack-summary-handle (point))))
	  (if tin
	      (setq id (bugtrack-sumlin->id
			(tin-cookie bugtrack-summary-handle tin)))
	    (error "There is no bug report to show.")))
      (setq id (prefix-numeric-value id)))
    (pop-to-buffer bugtrack-basic-buffer)
    (bugtrack-show-bug id)
    (pop-to-buffer old-buffer)))
  
		
(defun bugtrack-summary-goto-previous (count)
  "In Bugtrack summary mode, display the previous bug.
If a prefix argument is given, move up that many bugs first."
  (interactive "p")
  (tin-goto-previous bugtrack-summary-handle (point) count)
  (bugtrack-summary-goto-bug nil))

(defun bugtrack-summary-goto-next (count)
  "In Bugtrack summary mode, display the next bug.
If a prefix argument is given, move down that many bugs first."
  (interactive "p")
  (tin-goto-next bugtrack-summary-handle (point) count)
  (bugtrack-summary-goto-bug nil))

(defun bugtrack-sum-next-line (arg)
  "Go to the next line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (tin-goto-next bugtrack-summary-handle (point) arg))

(defun bugtrack-sum-previous-line (arg)
  "Go to the previous line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (tin-goto-previous bugtrack-summary-handle (point) arg))

;;; Local Variables:
;;; generated-autoload-file: "bugtrack-init.el"
;;; End:

;;; bugtrack-sum.el ends here
