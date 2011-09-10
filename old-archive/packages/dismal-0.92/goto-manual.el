;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : goto-manual.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Jul  6 10:56:04 1990
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Tue Apr  7 00:40:07 1992
;;;; Update Count    : 33
;;;; 
;;;; 
;;;; PURPOSE
;;;; 	Provides quick way to look up manuals quickly by assigning 
;;;;  them a nickname.
;;;; HISTORY
;;;;    Originally part of hypersoar.
;;;; TABLE OF CONTENTS
;;;; 	i.   	Required packages and Global variable setup
;;;;    I.  	goto-manual
;;;;    II.     Helper functions
;;;;   III.	Provide the package
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;         i.   Required packages and  Global variable setup
;;;

;; this may be an obsolete variable with the use of simple menus... fer 1/91
;; (defvar doc-manuals nil "*Where the manuals live in an assoc list")

(defvar doc-last-manual "taql-mode" "the last manual searched for")
(defvar doc-last-manual-search (getenv "USER")
  "The last string searched for.  Default is user's name.")

(defvar doc-manual-homes '("~/manuals/"))

(if (string= doc-last-manual-search "")
    (setq doc-last-manual-search "Newell"))


;;;
;;;         I.  	goto-manual
;;;

(defun goto-manual (manual &optional mode)
  "Get the manual out.  MANUAL is a file name, mode is the mode to put the 
buffer in to start with."
  (interactive)
  (let ((topic (read-string "Search for: " doc-last-manual-search)))
    (setq doc-last-manual manual)
    ;;; and choose what to search
    (setq doc-last-manual-search topic)
    (let ((file (doc-find-manual manual doc-manual-homes)))
    (if (not file)
        (error "Can't find manual %s" manual)
      (find-file file)
      (if mode 
          (eval (list mode)))
      (setq search-last-string topic)
      (search-forward topic) ) )))

;; users don't want the man-goto-manual version....
;; 3-Apr-92 -FER

;; (fset 'goto-manual 'doc-goto-manual)

(defun doc-find-manual (manual homes)
  "Find manual."
  (if homes
     (let ( (file (expand-file-name manual (car homes))) )
       (if (file-exists-p file)
            file
            (doc-find-manual manual (cdr homes)))  )))


;;;
;;;   	II.	 Helper functions
;;;

;;;
;;;   III.  Provide the package
;;;

(provide 'goto-manual)


