;;;;;
;; LCD Archive Entry:
;; gnus-speedups|Felix Lee|flee@cs.psu.edu|
;; Simple performance enhancements for GNUS|
;; 1992-13-11|1.6: stable|~/misc/gnus-speedups.Z|

;; This package contains two plug-compatible performance enhancements
;; for GNUS 3.13 and 3.14:

;; 1. Faster subject and date sorting in the *Subject* buffer.

;; 2. Functions that implement faster set operations on group lists,
;; which make startup and saving faster.

;; This file must be loaded after "gnus.el".  Loading this instead of
;; "gnus.el" will also work fine.  A GNUS maintainer will want to add
;; this at the end of the local gnus wrapper.

(require 'gnus)
(provide 'gnus-speedups)

;;;;;
;; 1. Faster subject and date sorting in the *Subject* buffer.

;; The basic idea is explained in 'gnus-keyed-sort.

" Sample hook usage:

To automatically sort a newsgroup by subject:
(setq gnus-Select-group-hook 'gnus-sort-headers-by-subject)

To automatically sort by subject and date:
(setq gnus-Select-group-hook
      (function
       (lambda ()
	 (gnus-sort-headers-by-date)
	 (gnus-sort-headers-by-subject)
	 )))
"
;; XXX These examples should be put in the doc for gnus-Select-group-hook

(defun gnus-keyed-sort (list compare extract)
  "Sort LIST stably and return the sorted list.  Does not modify LIST.
Arguments are (LIST COMPARE EXTRACT).  Elements in the list are
compared as if the predicate were:
	(COMPARE (EXTRACT a) (EXTRACT b))
but EXTRACT is run over each element of the list in a preprocessing
stage for efficiency.  This reduces the number of EXTRACT calls from
O(N log N) to O(N).

Example: (gnus-keyed-sort load-path 'string< 'downcase)
"
  (let ( (keyed-list
	  (mapcar
	   (function (lambda (it) (cons (funcall extract it) it)))
	   list)) )
    (setq keyed-list
	  (sort keyed-list
		(function
		 (lambda (a b) (funcall compare (car a) (car b))))))
    (mapcar (function (lambda (it) (cdr it)))
	    keyed-list)
    ))

(defun gnus-keyed-sort-headers (compare extract &optional reverse)
  "Sort current group's headers by COMPARE and EXTRACT.  Sorting is
done as if the predicate were
	(COMPARE (EXTRACT a) (EXTRACT b))
See 'gnus-keyed-sort for more detailed description.
Optional argument REVERSE means reverse the sense of the sort.
Note: interrupting the sort leaves the headers unsorted.
"
  (setq gnus-newsgroup-headers
	(gnus-keyed-sort gnus-newsgroup-headers compare extract))
  (if reverse
      (setq gnus-newsgroup-headers (nreverse gnus-newsgroup-headers)))
  )

;; For backward compatibility with GNUS 3.13
;; XXX (eval-and-compile ...)
(if (not (fboundp 'gnus-sortable-date))
    (fset 'gnus-sortable-date 'gnus-comparable-date))

(defun gnus-sort-headers-by-date (&optional reverse)
  "Sort the current group by date.  Optional argument REVERSE means
reverse order.  Does not redisplay the *Subject* buffer.  See also
'gnus-Subject-sort-by-date.
"
  (gnus-keyed-sort-headers
   (function string<)
   (function
    (lambda (it)
      (gnus-sortable-date (nntp-header-date it))))
   reverse))

;; XXX It should be 'gnus-sort-fold-case, not 'case-fold-search
(defun gnus-sort-headers-by-subject (&optional reverse)
  "Sort the current group by subject.  \"Re:\"s are ignored.  If
'case-fold-search is non-nil, then capitalization is ignored.
Optional argument REVERSE means reverse order.  Does not redisplay the
*Subject* buffer.  See also 'gnus-Subject-sort-by-subject.
"
  ;; The main complication here is we try to speed up the sort process
  ;; by hoisting conditions outside the sort.
  (gnus-keyed-sort-headers
   (function string<)
   (if case-fold-search
       (function
	(lambda (it)
	  (downcase (gnus-simplify-subject (nntp-header-subject it) 're-only))))
     (function
      (lambda (it)
	(gnus-simplify-subject (nntp-header-subject it) 're-only)))
     )
   reverse)
  )

(defun gnus-Subject-sort-by-subject (reverse)
  "Sort the *Subject* buffer by subject.  With a prefix argument,
reverse the order of the sort.  Uses 'gnus-sort-headers-by-subject.
"
  (interactive "P")
  (let ( (current (gnus-Subject-article-number)) )
    (gnus-sort-headers-by-subject reverse)
    (gnus-Subject-prepare)
    (gnus-Subject-goto-subject current)
    ))

(defun gnus-Subject-sort-by-date (&optional reverse)
  "Sort the *Subject* buffer by date.  With a prefix argument, reverse
the order of the sort.  Uses 'gnus-sort-headers-by-date.
"
  (interactive "P")
  (let ( (current (gnus-Subject-article-number)) )
    (gnus-sort-headers-by-date reverse)
    (gnus-Subject-prepare)
    (gnus-Subject-goto-subject current)
    ))


;;;;;
;; 2. Functions that implement faster set operations on group lists,
;; which make startup and saving faster.

;; We reduce an O(N**2) process to O(N) by building a hash table for
;; the list of known newsgroups.  (It's a little strange that GNUS
;; doesn't already have a hash table for this.)

;; XXX hash table size should be self-adjusting.

(defun gnus-option-n-filter (group-name)
  "Does GROUP-NAME fit the constraints imposed by an -n option in
the newsrc file?"
  (or (null gnus-newsrc-options-n-no)
      (not (string-match gnus-newsrc-options-n-no group-name))
      (and gnus-newsrc-options-n-yes
	   (string-match gnus-newsrc-options-n-yes group-name)))
  )

(defun gnus-find-new-newsgroups ()
  "Look for new newsgroups and return names.
`-n' option of options line in .newsrc file is recognized."
  (let ( (group-name nil)
	 (new-groups nil)
	 (known-groups (make-vector 1031 0)) )
    ;; Build a table of known newsgroups.
    (mapcar
     (function (lambda (group) (gnus-sethash (car group) t known-groups)))
     gnus-killed-assoc)
    (mapcar
     (function (lambda (group) (gnus-sethash (car group) t known-groups)))
     gnus-newsrc-assoc)
    ;; Compare the active file against what's known.
    (mapatoms
     (function
      (lambda (sym)
	(setq group-name (symbol-name sym))
	(and (gnus-option-n-filter group-name)
	     (null (gnus-gethash group-name known-groups))
	     (setq new-groups (cons group-name new-groups)))
	))
     gnus-active-hashtb)
    new-groups
    ))

(defun gnus-check-killed-newsgroups ()
  "Check consistency between gnus-newsrc-assoc and gnus-killed-assoc."
  (let ( (active-groups (make-vector 1031 0))
	 (old-killed gnus-killed-assoc)
	 (new-killed nil)
	 (group-name nil) )
    ;; Build a table of active newsgroups.
    (mapcar
     (function (lambda (group) (gnus-sethash (car group) t active-groups)))
     gnus-newsrc-assoc)
    ;; Filter the list.
    (while old-killed
      (setq group-name (car (car old-killed)))
      (and (gnus-option-n-filter group-name)
	   (null (gnus-gethash group-name active-groups))
	   (setq new-killed (cons (car old-killed) new-killed)))
      (setq old-killed (cdr old-killed))
      )
    (setq gnus-killed-assoc (nreverse new-killed))
    ))

