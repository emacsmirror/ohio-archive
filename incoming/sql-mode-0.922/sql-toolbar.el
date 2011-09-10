;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-toolbar.el - toolbar support for SQL Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Author:        Peter D. Pezaris <pez@atlantic2.sbi.com>
;;  Maintainer:    sql-mode-help@atlantic2.sbi.com
;;  Version:       0.922 (beta)
;;  Last Modified: Wed Oct 18 13:34:12 1995
;;  Keywords:      isql fsql sql editing major-mode languages
;;
;;  Copyright © 1995 Peter D. Pezaris
;;
;;  This file is part of the SQL Mode package.  Refer to the sql-mode.el
;;  file for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sql-icons)

(defvar sql-batch-toolbar
  '([sql-go-icon
     sql-toolbar-go
     t
     "Execute the current query"]
    [sql-abort-icon
     sql-abort
     t
     "Halt execution (WARNING: do not do this if you are performing an UPDATE)"]
;    [sql-sp-lock-icon
;     sql-toolbar-sp-lock
;     t
;     "Execute the sp_lock stored procedure"]
;    [sql-sp-who-icon
;     sql-toolbar-sp-who
;     t
;     "Execute the sp_who stored procedure"]
    [sql-previous-icon
     sql-toolbar-previous-history
     t
     "Go to the previous query"]
    [sql-next-icon
     sql-toolbar-next-history
     t
     "Go to the next query"]
    [sql-previous-global-icon
     sql-toolbar-previous-global-history
     t
     "Go to the previous global query"]
    [sql-next-global-icon
     sql-toolbar-next-global-history
     t
     "Go to the next global query"]
    [sql-new-query-icon
     sql-new-query
     t
     "Clear the query and results windows"]
    [sql-bcp-in-icon
     sql-bcp-in-menu
     t
     "BCP IN"]
    [sql-bcp-out-icon
     sql-bcp-out-menu
     t
     "BCP OUT"]
    [sql-cut-icon
     sql-cut-data
     t
     "Cut the rows at point or in the region"]
    [sql-copy-icon
     sql-copy-data
     t
     "Copy the rows at point or in the region"]
    [sql-paste-icon
     sql-paste-data
     t
     "Paste the rows that are in the clipboard into the database"]
    [sql-delete-icon
     sql-delete-data
     t
     "Delete the rows at point or in the region"]
    [sql-insert-icon
     sql-insert-row
     t
     "Insert a row into the database"]
    [sql-update-row-icon
     sql-edit-row
     t
     "Update rows"]
    [sql-printer-icon
     sql-print-buffer
     t
     "Print the current buffer"]

    nil

    [sql-help-icon
     sql-help
     t
     "Help"])
  "The default toolbar for a sql-batch-mode buffer.")

(defvar sql-results-toolbar
  '([sql-go-icon
     sql-toolbar-go
     t
     "Execute the current query"]
    [sql-abort-icon
     sql-abort
     t
     "Halt execution (WARNING: do not do this if you are performing an UPDATE)"]
    [sql-cut-icon
     sql-cut-data
     t
     "Cut the rows at point or in the region"]
    [sql-copy-icon
     sql-copy-data
     t
     "Copy the rows at point or in the region"]
    [sql-paste-icon
     sql-paste-data
     t
     "Paste the rows that are in the clipboard into the database"]
    [sql-delete-icon
     sql-delete-data
     t
     "Delete the rows at point or in the region"]
    [sql-insert-icon
     sql-insert-row
     t
     "Insert a row into the database"]
    [sql-update-row-icon
     sql-edit-row
     t
     "Update rows"]
    [sql-printer-icon
     sql-print-buffer
     t
     "Print the current buffer"]

    nil

    [sql-help-icon
     sql-help
     t
     "Help"])
  "The default toolbar for a sql-results-mode buffer.")

;(defvar sql-interactive-toolbar nil)

(defvar sql-top-ten-toolbar
  (list
   (vector
    'sql-top-ten-1-icon
    'sql-insert-top-ten-1
    '(aref sql-top-ten 1)
    (or (aref sql-top-ten-help 1)
	"Insert the Top Ten Item #1"))
   (vector
    'sql-top-ten-2-icon
    'sql-insert-top-ten-2
    '(aref sql-top-ten 2)
    (or (aref sql-top-ten-help 2)
	"Insert the Top Ten Item #2"))
   (vector
    'sql-top-ten-3-icon
    'sql-insert-top-ten-3
    '(aref sql-top-ten 3)
    (or (aref sql-top-ten-help 3)
	"Insert the Top Ten Item #3"))
   (vector
    'sql-top-ten-4-icon
    'sql-insert-top-ten-4
    '(aref sql-top-ten 4)
    (or (aref sql-top-ten-help 4)
	"Insert the Top Ten Item #4"))
   (vector
    'sql-top-ten-5-icon
    'sql-insert-top-ten-5
    '(aref sql-top-ten 5)
    (or (aref sql-top-ten-help 5)
	"Insert the Top Ten Item #5"))
   (vector
    'sql-top-ten-6-icon
    'sql-insert-top-ten-6
    '(aref sql-top-ten 6)
    (or (aref sql-top-ten-help 6)
	"Insert the Top Ten Item #6"))
   (vector
    'sql-top-ten-7-icon
    'sql-insert-top-ten-7
    '(aref sql-top-ten 7)
    (or (aref sql-top-ten-help 7)
	"Insert the Top Ten Item #7"))
   (vector
    'sql-top-ten-8-icon
    'sql-insert-top-ten-8
    '(aref sql-top-ten 8)
    (or (aref sql-top-ten-help 8)
	"Insert the Top Ten Item #8"))
   (vector
    'sql-top-ten-9-icon
    'sql-insert-top-ten-9
    '(aref sql-top-ten 9)
    (or (aref sql-top-ten-help 9)
	"Insert the Top Ten Item #9"))
   (vector
    'sql-top-ten-0-icon
    'sql-insert-top-ten-0
    '(aref sql-top-ten 0)
    (or (aref sql-top-ten-help 0)
	"Insert the Top Ten Item #0"))

   nil

   (vector
    'sql-top-ten-icon
    'sql-add-top-ten
    t
    "Make the current buffer's contents into a Top Ten item.")
    )
  "The defailt toolbar for the left hand side in sql-batch-mode buffers.")

(defun sql-top-ten-toolbar (&optional action)
  "Add a top ten toolbar if it doesn't exist.  Remove it if it does.
This feature requires XEmacs."
  (interactive)
  (if (not sql-xemacs-19-12)
      (error "Toolbars only work in XEmacs version 19.12 or later."))
  (if action
      (progn
	(if (eq action 1)
	    (progn
	      (set-specifier left-toolbar (cons (current-buffer) sql-top-ten-toolbar))
	      (set-specifier left-toolbar-width (cons (selected-frame) 32)))
	  (remove-specifier left-toolbar (current-buffer))
	  (remove-specifier left-toolbar-width (selected-frame)))
	(setq sql-use-top-ten-toolbar (if (eq action 1) t nil)))
    (if sql-use-top-ten-toolbar
	(progn
	  (remove-specifier left-toolbar (current-buffer))
	  (remove-specifier left-toolbar-width (selected-frame)))
      (set-specifier left-toolbar (cons (current-buffer) sql-top-ten-toolbar))
      (set-specifier left-toolbar-width (cons (selected-frame) 32)))
    (setq sql-use-top-ten-toolbar (not sql-use-top-ten-toolbar))))

(defun sql-turn-on-top-ten-toolbar ()
  "Add a top ten toolbar if it doesn't exist.
This feature requires XEmacs version 19.12 or later."
  (interactive)
  (sql-top-ten-toolbar 1))

(defun sql-turn-off-top-ten-toolbar ()
  "Delete a top ten toolbar if it exists.
This feature requires XEmacs version 19.12 or later."
  (interactive)
  (sql-top-ten-toolbar 0))

(defun sql-toolbar (&optional action)
  "Add a toolbar if it doesn't exist.  Remove it if it does.
This feature requires XEmacs version 19.12 or later."
  (interactive)
  (if (not sql-xemacs-19-12)
      (error "Toolbars only work in XEmacs version 19.12 or later."))
  (let ((the-toolbar (cond
		      ((eq major-mode 'sql-batch-mode)
		       sql-batch-toolbar)
		      ((eq major-mode 'sql-results-mode)
		       sql-batch-toolbar))))
    (if action
	(progn
	  (if (eq action 1)
	      (set-specifier default-toolbar (cons (current-buffer)
						   the-toolbar))
	    (remove-specifier default-toolbar (current-buffer)))
	  (setq sql-use-toolbar (if (eq action 1) t nil)))
      (if sql-use-toolbar
	  (remove-specifier default-toolbar (current-buffer))
	(set-specifier default-toolbar (cons (current-buffer) the-toolbar)))
      (setq sql-use-toolbar (not sql-use-toolbar)))))

(defun sql-turn-on-toolbar ()
  "Add a toolbar if it doesn't exist.
This feature requires XEmacs version 19.12 or later."
  (interactive)
  (sql-toolbar 1))

(defun sql-turn-off-toolbar ()
  "Delete a toolbar if it exists.
This feature requires XEmacs version 19.12 or later."
  (interactive)
  (sql-toolbar 0))

(defun sql-toolbar-go ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-evaluate-buffer nil))

(defun sql-toolbar-previous-history ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-previous-history nil))

(defun sql-toolbar-next-history ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-next-history nil))

(defun sql-toolbar-previous-global-history ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-previous-global-history))

(defun sql-toolbar-next-global-history ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-next-global-history))

(defun sql-toolbar-sp-lock ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-goto-history 0 t)
  (insert "sp_lock\n")
  (sql-evaluate-buffer nil))

(defun sql-toolbar-sp-who ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-goto-history 0 t)
  (insert "sp_who\n")
  (sql-evaluate-buffer nil))

(defun sql-toolbar-sp-what ()
  (interactive)
  (sql-goto-batch-buffer)
  (sql-goto-history 0 t)
  (insert "sp_what\n")
  (sql-evaluate-buffer nil))
  
(defun sql-toolbar-insert ()
  "Insert a row into the current database."
  (interactive)
  (call-interactively 'sql-insert-row))
  
(provide 'sql-toolbar)
