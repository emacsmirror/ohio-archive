;;; Autor: cdwilli@kochab.cs.umbc.edu
;;; Organization: University of Maryland Baltimore County
;;
;; LCD Archive Entry:
;; comm-regn|cdwilli|cdwilli@kochab.cs.umbc.edu|
;; Comment-out a region of C or Lisp code.|
;; 02-Feb-1994||~/functions/comm-regn.el.Z|
;;;
;;; 30-Jun-1993 Heiko Muenkel, 
;;;		I've added support for the c++-c-mode.
;;;
;;;Have you ever wanted to comment out a block of C or Lisp code
;;;while using GNU EMACS?  Two functions, (comment-region) and
;;;(uncomment-region), make this easy.  In C, they check for nesting
;;;of comments as well as unbalanced delimiters.  Try them out by
;;;marking a region of code, then typing M-x comment-region, M-x
;;;uncomment-region.
;;;
;;; COMMENT-REGION -- The user sets the mark (CTRL-@), then moves the
;;; point (cursor) to another place in the buffer.  He then types M-x
;;; comment-region.  The area between the mark and the cursor is
;;; commented in the appropriate style depending on what mode the
;;; buffer is in (currently Lisp, EMACS Lisp, and C modes are working).
;;; UNCOMMENT-REGION -- Does the reverse of comment-region.
;;;
;;; HOW TO USE THESE FUNCTIONS:
;;; Type M-x load-file <return>, give the name of this file and
;;; <return>.  Now, if you want to comment some code, move the cursor
;;; to a point in the buffer, then set a mark (CTRL-@).  Move the
;;; cursor again to specify a region to comment, then type M-x
;;; comment-region <return>.
;;; To uncomment code, specify a region as described above, then type
;;; M-x uncomment-region <return>.

;;; Lisp commenting works line-by-line, commenting out whole lines at
;;; a time.  C commenting, on ;the other hand, looks for uncommented
;;; regions within the whole region to be commented (the C compiler
;;; doesn't allow nesting of comments, so we have to search for any
;;; comments already in the region).

;;; TO DO:  This program can be extended to include GNU's
;;; fortran-comment-region.  One needs to write a
;;; fortran-uncomment-region as well.

(defun comment-region ()
  "Insert the proper comment characters into the region of a program.
Used to comment blocks of Lisp or C code."
  (interactive)
  (let ((start-position (make-marker))
	(end-position (make-marker)))
    (set-marker start-position (mark))
    (set-marker end-position (point))
    (cond ((or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
	   (comment-lines start-position end-position 'comment-lisp-line)
	   (goto-char (marker-position end-position)))
	  ((or (eq major-mode 'c-mode)
	       (eq major-mode 'c++-mode)
	       (eq major-mode 'c++-c-mode))
	   (c-comment-region start-position end-position))
	  (t (error "Don't know how to comment %s mode." major-mode)))))

(defun uncomment-region ()
  "Remove comment delimiters from a region of code.  Works with
comments created by function comment-region.  Lisp or C."
  (interactive)
    (let ((start-position (make-marker))
	  (end-position (make-marker))
	  (final-position (make-marker)))
      (set-marker start-position (mark))
      (set-marker end-position (point))
      (set-marker final-position end-position)
      (cond ((or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
	     (comment-lines start-position end-position 'uncomment-lisp-line)
	     (goto-char (marker-position end-position)))
	    ((or (eq major-mode 'c-mode)
		 (eq major-mode 'c++-mode)
		 (eq major-mode 'c++-c-mode))
	     (c-uncomment-region start-position end-position))
	    (t (error "Don't know how to uncomment %s mode." major-mode)))))

;;; ****************************************************************
;;; LISP COMMENTING AND UNCOMMENTING FUNCTIONS
;;; ****************************************************************

;;; Loop through, commenting each line.
(defun comment-lines (start-position end-position comment-function)
  (save-excursion
  (let ((number-of-lines-to-comment (count-lines start-position end-position)))
    (goto-char start-position)
    (while (> number-of-lines-to-comment 0)
      (beginning-of-line)
      (funcall comment-function)
      (setq number-of-lines-to-comment
	    (- number-of-lines-to-comment 1))
      (go-to-next-line start-position end-position)))))

;;; Go forward or backward one line, depending on values of start and end.
(defun go-to-next-line (start end)
  (cond ((< start end) (next-line 1))
	((> start end) (next-line -1))))

;;; Comment the current-line using Lisp commenting rules.
 (defun comment-lisp-line ()
   (beginning-of-line)
   (insert ";;; "))

(defun uncomment-lines (start-position end-position uncomment-function)
  (save-excursion
    (let ((number-of-lines-to-uncomment
	   (count-lines start-position end-position)))
      (goto-char start-position)
      (while (> number-of-lines-to-uncomment 0)
	(beginning-of-line)
	(funcall uncomment-function)
	(setq number-of-lines-to-uncomment
	      (- number-of-lines-to-uncomment 1))
	(go-to-next-line start-position end-position)))))


;;; Replace all semicolons at the beginning of the line up to a space.
;;; If a space is left after the point, delete that, too.
(defun uncomment-lisp-line ()
  (beginning-of-line)
  (delete-semicolons (point))
  (if (string-equal (char-to-string (char-after (point)))
		    " ")
      (delete-char 1)))

(defun delete-semicolons (point)
  (while (string-equal (char-to-string (char-after (point)))
		       ";")
    (delete-char 1)))

;;;****************************************************************
;;; C COMMENTING AND UNCOMMENTING FUNCTIONS.
;;;****************************************************************
(defun c-comment-region (start end)
  ;; If region has been marked from the bottom to the top of the buffer,
  ;; switch to start and end points.
  (if (> start end)
      (let ((temp start))
	(setq start end)
	(setq end temp)))
  ;; Check for nothing marked.  If something marked, check whether the
  ;; region contains any comments already.  If it doesn't, do a simple
  ;; comment of a region.  If it does, check whether it has any
  ;; unbalanced or unnested comments.
  (if (not (= start end))		;No commenting an empty region.
      (cond ((not (c-comments-present-p start end))
	     (simple-c-comment start end))
	    ((and (balanced-unnested-c-comments-p "/*$" "$*/" start end)
		  (balanced-unnested-c-comments-p "/*" "*/" start end))
	     (c-comment-uncommented-regions start end)))))

;;; This is the mate to c-comment-region.  It first checks for
;;; unbalanced and nested comment delimiters, then, if all is well, it
;;; deletes all occurrences of "/*$" and "$*/".
(defun c-uncomment-region (start end)
  ;; If region has been marked from the bottom to the top of the
  ;; buffer, switch start and end points.
  (if (> start end)
      (let ((temp start))
	(setq start end)
	(setq end temp)))
  (if (not (= start end))
      (cond ((not (c-comments-present-p start end))
	     (message "No comments in region."))
	    ((and (balanced-unnested-c-comments-p
		   "/*$" "$*/" start end)
		  (balanced-unnested-c-comments-p
		   "/*" "*/" start end))
	     (c-uncomment-commented-regions start end)))))

(defun c-uncomment-commented-regions (start end)
  "Takes two markers that delimit a region and removes any C comment
   delimiters involving $."
  (let ((original-point (point-marker)))
    (goto-char (marker-position start))
    (while (search-forward "/*$" end t)
      (goto-char (- (point) 3))
      (delete-char 3)
      (cond
       ((search-forward "$*/" (marker-position end) t)
	(goto-char (- (point) 3))
	(delete-char 3))
       (t (error "Unbalanced C delimiters.  Missing %s." "$*/"))))
    (goto-char (marker-position original-point))))
  
		  
(defun c-comments-present-p (start end)
  (or (first-string-occurrence "/*" start end)
      (first-string-occurrence "*/" start end)))

(defun first-string-occurrence (string start end)
  (let (string-present)
    (if (markerp start)
	(goto-char (marker-position start))
      (goto-char start))
    (if (markerp end)
	(setq string-present (search-forward string (marker-position
						     end) t))
      (setq string-present (search-forward string end t)))
    (if string-present
	(setq string-present (point)))
    string-present))
      
(defun c-comment-uncommented-regions (start end)
  (let ((m1 (make-marker))
	(m2 (make-marker)))
    (set-marker m1 start)
    (set-marker m2 end)
    (while (not (= m1 m2))
      (let ((m3 (make-marker)))
	;; Find the beginning of a comment.
	(set-marker m3 (first-string-occurrence "/*" m1 m2))
	(cond ((marker-position m3)
	       (set-marker m3 (- m3 2))
	       (simple-c-comment m1 m3)
	       (set-marker m1 (first-string-occurrence "*/" m3 m2))
	       (if (not (marker-position m1))
		   (setq m1 m2)))
	      (t (simple-c-comment m1 m2)
		 (setq m1 m2)))))))

;;; Place "/*$" at the beginning of region and "$*/" at the end.
;;; Leave the point as it was.
(defun simple-c-comment (start end)
  "Takes two markers that delimit region.  Comments a region with C
comment delimiters.  Assumes no comments already in region."
  (let ((original-point (point-marker))
	(begin-comment "/*$")
	(end-comment "$*/"))
    (goto-char (marker-position start))
    (skip-chars-forward "[\t\n ]*" (marker-position end))
    (set-marker start (point))
    (cond ((not (= start end))
	   (insert begin-comment)
	   (goto-char (marker-position end))
	   (skip-chars-backward "[\t\n ]*")
	   (insert end-comment)
	   (goto-char (point)))
	  (t nil))))

(defun balanced-unnested-c-comments-p (left right start end)
  (balanced-but-unnested-aux left right start end))

(defun balanced-but-unnested-aux (first second start end)
  (let* ((first-delimiter-list
	  (find-closest-string start end first second))
	 (first-delimiter (car first-delimiter-list))
	 (first-delimiter-location (car (cdr first-delimiter-list))))
    (if first-delimiter
	(if (string-equal first-delimiter first)
	    (let* ((next-delimiter-list
		    (find-closest-string
		     first-delimiter-location end first second))
		   (next-delimiter (car next-delimiter-list))
		   (next-delimiter-location (car (cdr next-delimiter-list))))
	      (if next-delimiter-list
		  (if (string-equal next-delimiter second)
		      (balanced-but-unnested-aux
		       first second next-delimiter-location end)
		    (error "Unbalanced delimiters.  Extra %s." first))
	      (error "Unbalanced delimiters. %s missing." second)))
	  (error "Unbalanced delimiters. %s missing." first))	      
      t)))
	      
(defun find-closest-string (start end &rest string-list)
  (let ((closest (+ end 1))
	(current-string ""))
    (while string-list
      (let ((distance (first-string-occurrence (car string-list)
					       start end)))
	(if distance
	    (if (< distance closest)
		(progn
		  (setq closest distance)
		  (setq current-string (car string-list)))))
	(setq string-list (cdr string-list))))
    (if (null-string current-string)
	nil
      (list current-string closest))))

(defun null-string (s) (string-equal "" s))
