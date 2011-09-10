;;; html-helper-imenu --- imneu suport for html-helper
;;
;; ~/lib/emacs/jhg-lisp/html-helper-imenu.el ---
;;
;; $Id: html-helper-imenu.el,v 1.8 1999/05/25 17:59:15 harley Exp $
;;

;; Author:   James H Gorrell <harley@bcm.tmc.edu>
;; Keywords: html-helper, imenu, table of contents
;; URL:      http://hgsc.bcm.tmc.edu/~harley/elisp/html-helper-imenu.el

;;; Commentary:
;; * Adds an indented table of contents to the menubar
;; * The regexp only matches headers on a single line
;;   and well formed tags.

;; Put somthing like the following in your .emacs:
;; (autoload 'html-helper-imenu-setup "html-helper-imenu")
;; (add-hook 'html-helper-mode-hook 'html-helper-imenu-setup)
;;

;;; History:
;;
;; 98-Jun-25 : jhg
;; - added regexp
;;

;;; Code:

(provide 'html-helper-imenu)

(defvar html-helper-imenu-title "TOC"
  "*Title of the menu which will be added to the menubar.")

(defvar html-helper-imenu-regexp
  "\\s-*<h\\([1-9]\\)[^\n<>]*>\\(<[^\n<>]*>\\)*\\s-*\\([^\n<>]*\\)"
  "*A regular expression matching a head line to be added to the menu.
The first `match-string' should be a number from 1-9.
The second `match-string' matches extra tags and is ignored.
The third `match-string' will be the used in the menu.")

;; Make an index for imenu
(defun html-helper-imenu-index ()
  "Return an table of contents for an html buffer for use with Imenu."
  (let ((space ?\ ) ; a char
	(toc-index '())
	toc-str)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward html-helper-imenu-regexp nil t)
	(setq toc-str
	      (concat
	       (make-string
		(* 2 (- (string-to-number (match-string 1)) 1))
		space)
	       (match-string 3)))
	(beginning-of-line)
	(setq toc-index (cons (cons toc-str (point)) toc-index))
	(end-of-line)
	))
    (nreverse toc-index)))

(defun html-helper-imenu-bogus () 
  "A bougus function to make the 20.2 version of imenu happy.
Otherwise it the mode wont activate."
  (error "Imenu called html-helper-imenu-bogus"))

(defun html-helper-imenu-setup ()
  "Setup the variables to support imenu."
  (setq imenu-create-index-function 'html-helper-imenu-index)
  (setq imenu-extract-index-name-function 'html-helper-imenu-bogus)
  (setq imenu-sort-function nil) ; sorting the menu defeats the purpose
  (imenu-add-to-menubar html-helper-imenu-title)
  )

;;; html-helper-imenu ends here
