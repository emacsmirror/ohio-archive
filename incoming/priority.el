;;;; priority.el - prioritized to-do list
;;;; Jim Blandy <jimb@cyclic.com> --- November 1996
;;;; Copyright (C) 1996 Jim Blandy

;; Author: Jim Blandy <jimb@cyclic.com>
;; Maintainer: Jim Blandy <jimb@cyclic.com>
;; Created: Mon 18 Nov 1996
;; Updated: Tue 19 Nov 1996
;; Version: $Id: priority.el,v 1.5 1996/11/20 00:42:30 jimb Exp $
;; Keywords: calendar


;;;; finding the ``now'' marker

(defconst pri-now-string "----- now -----")
(defconst pri-now-regexp (concat "^" pri-now-string "$"))

(defun pri-now-bounds ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward pri-now-regexp)
    (let ((start (match-beginning 0)))
      (while (looking-at "[ \t]*\n")
	(forward-line 1))
      (let ((end (point)))
	(if (re-search-forward pri-now-regexp nil t)
	    (error "found more than one ``now'' marker"))
	(list start end)))))


;;;; finding tasks

(defconst pri-task-start-regexp
  (let ((float "\\([0-9]+\\.?\\|[0-9]*\\.[0-9]+\\)"))
    (concat "^\\[" float "\\]")))
(defconst pri-task-boundary-regexp "^[^ \t\n]")

(defun pri-find-task-start ()
  (end-of-line)
  (let ((limit (save-excursion
		 (if (re-search-backward pri-task-boundary-regexp nil t)
		     (point)
		   (point-min)))))
    (re-search-backward pri-task-start-regexp limit t)))

(defun pri-find-task-end ()
  (end-of-line)
  (re-search-forward pri-task-boundary-regexp)
  (goto-char (match-beginning 0)))

(defun pri-task-bounds ()
  "Return the start and end buffer positions of the task containing point.
If point doesn't seem to be in a task, return nil.
The endpoint of the range is not considered part of the task, so you
can walk all tasks by moving point to just after the ``now'' marker,
and then repeatedly calling pri-task-bounds, and moving point to the
end of the task."
  (save-excursion
    (let ((start (pri-find-task-start)))
      (if start
	  (progn (pri-find-task-end)
		 (list start (point)))
	nil))))

(defun pri-task-priority ()
  "Return the priority of the task starting at point, as a string."
  (or (looking-at pri-task-start-regexp)
      (error "couldn't parse priority of task"))
  (buffer-substring (match-beginning 1)
		    (match-end 1)))


;;;; Sorting tasks by decreasing priority

(defun pri-sort-tasks ()
  "Sort all uncompleted tasks in order of decreasing priority.
See the documentation for ``priority-mode'' for the task syntax."
  (interactive)
  (let ((original-point (point))
	(start (nth 1 (pri-now-bounds))))
    (goto-char start)
    (let* ((tasks (pri-build-task-list original-point))
	   (sorted (sort tasks 'pri-sort-function))
	   (new-point (pri-reorder-tasks start sorted)))
      (goto-char new-point))))

(defun pri-build-task-list (saved-point)
  "Construct a list of all tasks after point in the current buffer.
Each element of the list has the form
   (PRIORITY START-MARKER END-MARKER POINT-OFFSET)
where PRIORITY is the priority of the task, and START- and END-MARKER
are markers bounding the task's text in the buffer.  If the task
contains the buffer position SAVED-POINT, then POINT-OFFSET is
present, and gives the distance from START-MARKER to SAVED-POINT;
otherwise, POINT-OFFSET is nil.

Tasks appear in the list in the same order they appear in the buffer.
Thus, a stable sort will leave an already ordered task list unchanged."
  (let (tasks bounds)
    (while (setq bounds (pri-task-bounds))
      (setq tasks (cons (list (progn (goto-char (car bounds))
				     (string-to-number (pri-task-priority)))
			      (copy-marker (nth 0 bounds))
			      (copy-marker (nth 1 bounds))
			      (if (and (<= (nth 0 bounds) saved-point)
				       (< saved-point (nth 1 bounds)))
				  (- saved-point (nth 0 bounds))))
			tasks))
      (goto-char (nth 1 bounds)))
    (nreverse tasks)))

(defun pri-sort-function (a b)
  (> (car a) (car b)))

(defun pri-reorder-tasks (start sorted)
  "Move the tasks listed in SORTED to START, and put them in order.
Return the new location of the original point in the relocated text."
  
  ;; Make sure all insertions relocate the markers in the sorted-list.
  (goto-char start)
  (insert-before-markers "*")
  (goto-char start)

  ;; Walk the sorted list, inserting each task at point, and deleting
  ;; the old text.  Zap the list markers too, so they don't slow down
  ;; editing any more than necessary.
  (let (new-point)
    (while sorted
      (let* ((task (car sorted))
	     (start (nth 1 task))
	     (end (nth 2 task))
	     (point-offset (nth 3 task)))
	(if point-offset (setq new-point (+ (point) point-offset)))
	(insert-buffer-substring (current-buffer) start end)
	(delete-region start end)
	(set-marker start nil)
	(set-marker end nil))
      (setq sorted (cdr sorted)))

    ;; Remove the character we inserted earlier.
    (delete-char 1)

    ;; Collapse all blank lines at the end to a single blank line.
    (skip-chars-backward " \t\n")
    (forward-line 1)
    (let ((start (point)))
      (while (looking-at "[ \t]*\n")
	(forward-line 1))
      (delete-region start (point))
      (insert "\n"))

    new-point))


;;;; marking tasks as done

(defun pri-done ()
  "Mark the task containing point as complete, by moving it before ``now''."
  (interactive)
  (let* ((task (or (pri-task-bounds)
		   (error "point is not in a task")))		   
	 (start (copy-marker (nth 0 task)))
	 (end (copy-marker (nth 1 task))))
    (goto-char (nth 0 (pri-now-bounds)))
    (insert-buffer-substring (current-buffer) start end)
    (delete-region start end)
    (set-marker start nil)
    (set-marker end nil))
  (goto-char (nth 1 (pri-now-bounds))))


;;;; adding and deleting tasks

(defun pri-add-task ()
  "Insert a new task, above the one containing point."
  (interactive)
  (let* ((task (pri-task-bounds))
	 (pri (if task
		  (progn (goto-char (nth 0 task))
			 (pri-task-priority))
		".")))
    (if task (goto-char (nth 1 task))
      (beginning-of-line))
    (insert "[" pri "] \n\n")
    (forward-char -4)))

(defun pri-kill-task ()
  "Delete the task containing point.  Copy its text to the kill ring."
  (interactive)
  (let ((task (pri-task-bounds)))
    (or task
	(error "point is not in a task"))
    (kill-region (nth 0 task) (nth 1 task))))
  
(defun pri-now ()
  "Move point to the first task after the ``now'' marker."
  (interactive)
  (goto-char (nth 1 (pri-now-bounds))))


;;;; keymap

(defvar priority-mode-map (make-sparse-keymap))
(define-key priority-mode-map "\C-c\C-s" 'pri-sort-tasks)
(define-key priority-mode-map "\C-c\C-c" 'pri-done)
(define-key priority-mode-map "\C-c\C-a" 'pri-add-task)
(define-key priority-mode-map "\C-c\C-k" 'pri-kill-task)
(define-key priority-mode-map "\C-c\C-n" 'pri-now)


;;;; mode function

(defun priority-mode ()
  "Mode for editing prioritized task lists.
\\<priority-mode-map>
Lines starting with the text ``[P]'', where P is a number (possibly
containing a decimal point), begin a task.  All indented lines
following the marker are considered part of the task's description.  A
line of the form ``----- now -----'' separates completed tasks (above
it) from uncompleted tasks (below it).

Commands of priority mode:
\\[pri-sort-tasks]	  Sort tasks in order of decreasing priority.  This affects only
	  uncompleted tasks: those between the ``now'' marker and the
	  end of the buffer, or the end of the page.
\\[pri-done]	  Mark the task containing point as completed, by moving it
	  above the ``now'' marker.
\\[pri-add-task]	  Insert a new task below the current one.  The new task's
	  priority is the same as the current task's.
\\[pri-kill-task]	  Delete the task containing point, and copy its text
	  to the kill ring.
\\[pri-now]	  Move point to the first uncompleted task."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'priority-mode)
  (setq mode-name "Priorities")
  (use-local-map priority-mode-map)
  (run-hooks 'priority-mode))

