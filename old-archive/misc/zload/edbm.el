;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; edbm.el -- simple minded dbm-like facility for emacs
;; 
;; edbm.el,v 1.2 1993/02/19 00:46:53 krab Exp
;; 
;; Author          : Kresten Krab Thorup
;; Created On      : Thu Feb 18 23:27:07 1993
;; Last Modified By: Kresten Krab Thorup
;; Last Modified On: Fri Feb 19 01:46:00 1993
;; 
;; Update Count    : 33
;; Buffer Position : 4102
;; Minor Modes     : ( DEBUG Fill)
;; 
;; edbm.el,v
;; Revision 1.2  1993/02/19  00:46:53  krab
;; Changed to fast version of edbm::read which doesnt use load.
;;
;; Revision 1.1  1993/02/18  22:54:10  krab
;; Initial revision
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'edbm)
(require 'zload)

(defconst edbm:version "1.2"
  "The revision number of edbm.el -- Simple code to provide edbm like
facilities for elisp.   Complete RCS identity is

	edbm.el,v 1.2 1993/02/19 00:46:53 krab Exp")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edbm:init (file)
  "Initialize a edbm table from FILE.  Returns a table data structure,
which is passed as the first argument of the other functions"
  (let ((edbm::date (and (file-exists-p file)
			 (nth 5 (file-attributes file)))))
    
    (if edbm::date
	(cons (cons file edbm::date) (edbm::read file))
      (cons (cons file edbm::date) nil))))

(defun edbm:get (table key)
  "From TABLE get the value of KEY"
  (if table
      (let* ((edbm::file (car (car table)))
	     (edbm::date (cdr (car table)))
	     (file::date (and (file-exists-p (car (car table)))
			      (nth 5 (file-attributes (car (car table)))))))

	(if (equal edbm::date file::date)
	    nil
	  (setcdr table (edbm::read edbm::file))
	  (setcar table (cons edbm::file file::date)))
	
	(assoc key (cdr table)))))

(defun edbm:set (table key value)
  "In TABLE set the value of KEY to VALUE"
  (edbm:remove table key)
  (edbm::append table key value))

(defun edbm:remove (table key)
  "In table, remove key"
  (if (and table (edbm:get table key))
  
      (let ((removen-entry nil))
	
	;; first, remove it from the table
	(let ((edbm::table (cdr table))
	      (edbm::entry nil))
	  (setcdr table nil)
	  (while (setq edbm::entry (car edbm::table))
	    (setq edbm::table (cdr edbm::table))
	    (if (equal (car edbm::entry) key)
		(setq removen-entry edbm::entry)
	      (setcdr table (cons edbm::entry (cdr table))))))
	
	;;next, remove it from the file
	(let ((table-buf (get-buffer-create "*edbm*"))
	      (this-buf (current-buffer)))
	  (set-buffer table-buf)
	  (kill-region (point-min) (point-max))
	  (if (file-exists-p (car (car table)))
	      (insert-file-contents (car (car table))))
	  (goto-char (point-min))
	  (delete-matching-lines 
	   (concat "^" (regexp-quote (edbm::entry-string removen-entry))))
	  (write-region (point-min) (point-max) (car (car table)) nil "glab")
	  (set-buffer this-buf)
	  (kill-buffer table-buf)))))
    

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIVATE FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun edbm::append (table key value)
  "To TABLE, append KEY as VALUE"
  (setcdr table (cons (cons key value) (cdr table)))
  (let ((table-buf (get-buffer-create "*edbm*"))
	(this-buf (current-buffer)))
    (set-buffer table-buf)
    (kill-region (point-min) (point-max))
    (if (file-exists-p (car (car table)))
	(insert-file-contents (car (car table))))
    (goto-char (point-max))
    (insert (concat (edbm::entry-string (cons key value)) "\n"))
    (write-region (point-min) (point-max) (car (car table)) nil "glab")
    (set-buffer this-buf)
    (kill-buffer table-buf)))

(defun edbm::read (file)
  "This function is used to read a edbm file"
  (let ((edbm:::list nil)
	(table-buf (get-buffer-create "*edbm*"))
	(this-buf (current-buffer)))
    (set-buffer table-buf)
    (kill-region (point-min) (point-max))
    (insert-file-contents file)
    (eval-current-buffer)
    (set-buffer this-buf)
    (kill-buffer table-buf)
    edbm:::list))

(defun edbm::entry-string (cell)
  "This function is used for printing edbm files"
  (prin1-to-string (list 'edbm:::entry (list 'quote cell)))
)

(defun edbm:::entry (cell)
  "This function is used for scanning edbm files"
  (setq edbm:::list (cons cell edbm:::list)))



