;;; u-vm-color.el --- Font-lock support for VM.
;;
;;  Copyright (C) 2001 by Ulf Jasper
;;
;;  Emacs Lisp Archive Entry
;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   u-vm-color.el
;;  Created:    January 19 2001
;;  Keywords:   Customization
;;  Time-stamp: "25. January 2001, 21:23:51 (ulf)"
;;  Version:    $Id: u-vm-color.el,v 1.7 2001/02/12 19:29:30 ulf Exp $
;;
;;
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;; Commentary:
;;  
;;  This package provides a simple way for configuring font-lock-keywords
;;  for VM.  It uses the value of `vm-summary-format' to find a "reasonable"
;;  regexp for font-locking.  The corresponding faces can be customized.
;;
;;  Font-lock can be configured and activated for vm-summary-mode as well
;;  as vm-mode and vm-presentation-mode.  All vm format-specifiers are
;;  understood (as of VM 6.88), as well as the userdefined specifier `%UB',
;;  provided by BBDB.
;;
;;  To install and use place this file somewhere in your load-path and put
;;  the following in your VM startup file (~/.emacs or ~/.vm) 
;;
;;  (require 'u-vm-color)
;;  (add-hook 'vm-mode-hook 'u-vm-color-presentation-mode)
;;  (add-hook 'vm-presentation-mode-hook 'u-vm-color-presentation-mode)
;;  (add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
;;
;;  In order to prevent Emacs from (font-)locking I strongly recommend to
;;  use lazy-lock.
;;
;;  Disclaimer: May show unexpected results, or even fail, if
;;  vm-summary-format is sufficiently complex=strange.
;;
;;  Xemacs users might want to turn off `vm-use-lucid-highlighting', if
;;  this package works...
;;
;;; History:
;;  
;;  1.1: Introduced minor modes.
;;       Should work for xemacs as well.
;;
;;  1.5: Minor bug fixes.
;;
;;  1.6: Limited headers and signatures to 5 lines to avoid regexp stack
;;       overflow.
;;       Citations now supercite-compliant.

;;; Code:
(require 'font-lock)

(defgroup u-vm-color nil
  "Font-lock support for vm."
  :group 'vm)

(defface u-vm-color-signature-face
  '((t (:bold nil :italic t :foreground "Sienna")))
  "Face for Signatures."
  :group 'u-vm-color)

(defface u-vm-color-header-face
  '((t (:bold t :italic nil :foreground "Black")))
  "General Face for headers."
  :group 'u-vm-color)

(defface u-vm-color-author-face
  '((t (:bold nil :italic nil :foreground "MidnightBlue")))
  "Face for sender names."
  :group 'u-vm-color)

(defface u-vm-color-recipient-face
  '((t (:bold nil :italic nil :foreground "DarkGreen")))
  "Face for recipient names."
  :group 'u-vm-color)

(defface u-vm-color-subject-face
  '((t (:bold t :italic nil :foreground "MediumBlue")))
  "Face for subjects."
  :group 'u-vm-color)

(defface u-vm-color-default-face
  '((t (:italic t)))
  "Default face."
  :group 'u-vm-color)

(defface u-vm-color-time-face
  '((t (:bold nil :italic nil :foreground "Maroon")))
  "Face for message time."
  :group 'u-vm-color)

(defface u-vm-color-attribute-face
  '((t (:bold t :italic nil :foreground "Red")))
  "Face for vm attributes."
  :group 'u-vm-color)

(defface u-vm-color-date-face
  '((t (:bold nil :italic nil :foreground "Maroon")))
  "Face for message date."
  :group 'u-vm-color)

(defface u-vm-color-id-face
  '((t (:bold nil :italic t)))
  "Face for message id."
  :group 'u-vm-color)

(defface u-vm-color-label-face
  '((t (:bold nil :italic nil :foreground "Red")))
  "Face for vm labels."
  :group 'u-vm-color)

(defface u-vm-color-length-face
  '((t (:bold nil :italic nil :foreground "Black")))
  "Face for message length."
  :group 'u-vm-color)

(defface u-vm-color-number-face
  '((t (:bold nil :italic nil :foreground "Black")))
  "Face for message number."
  :group 'u-vm-color)

(defface u-vm-color-user-face
  '((t (:bold nil :italic nil :foreground "ForestGreen")))
  "Face for user defined summary elements."
  :group 'u-vm-color)

(defface u-vm-color-citation-1-face
  '((t (:foreground "OrangeRed")))
  "Face for citations."
  :group 'u-vm-color)

(defface u-vm-color-citation-2-face
  '((t (:foreground "SlateBlue")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-3-face
  '((t (:foreground "DarkGreen")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-4-face
  '((t (:foreground "BlueViolet")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-5-face
  '((t (:foreground "Firebrick")))
  "Face for citation."
  :group 'u-vm-color)

(defun u-vm-color-make-summary-keywords ()
  "Parse `vm-summary-format' and return a font-lock keyword list.
List consists of one big regexp and lots of face instructions for
subexpressions."
  (let ((search-start 0)
	(length 0)
	(m-length 0)
	(rest "")
	(f-element "")
	(m-element "")
	(value "")
	(u-format "^..")
	(u-match nil)
	(count 1)
	(t-vm-summary-format vm-summary-format))
    (while (string-match
	    (concat "%-?\\([0-9]+\\)?\\(\\.[0-9]+\\)?"
		    "\\([aAcdfFhHiIlLmMnstTwyz*]\\|U.\\)\\([^%\n]*\\)")
	    t-vm-summary-format search-start)
      (setq search-start (match-end 0))
      (if (match-beginning 1)
	  (setq length (string-to-number
			(substring t-vm-summary-format (match-beginning 1)
				   (match-end 1))))
	(setq length 0))
      (if (match-beginning 2)
	  (setq m-length (string-to-number
			  (substring t-vm-summary-format
				     (+ 1 (match-beginning 2))
				     (match-end 2))))
	(setq m-length 0))
      (if (match-beginning 3)
	  (setq value (substring t-vm-summary-format (match-beginning 3)
				 (match-end 3)))
	(setq value ""))
      (if (match-beginning 4)
	  (setq rest (substring t-vm-summary-format (match-beginning 4)
				(match-end 4)))
	(setq rest ""))
      (setq rest (regexp-quote rest))
      ;;(message "--> %s, %s, %s" length m-length value)
      ;; Should use the length and m-length values for things like %5d
      ;; instead of doing [0-9 ]+ for numerics...
      ;; No!
      (cond ((string-equal value "a") ;; attributes -- make sure that all
				      ;; possible letters are given!
	     (setq f-element "\\([DNU ][FW ][RZ ][E ]\\)" )
	     (setq m-element (list count (quote 'u-vm-color-attribute-face))))
	    ((string-equal value "A") ;; attributes -- long
	     (setq f-element "\\([DNU ][r ][z ][b ][f ][w ][e ]\\)")
	     (setq m-element (list count (quote 'u-vm-color-attribute-face))))
	    ((string-equal value "c") ;; number of characters
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-length-face))))
	    ((string-equal value "d") ;; day -- numeric
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "f") ;; authors / recipients address
	     (setq f-element "\\(To: [^ \n]+\\)?\\([^ \n]+\\)?")
	     (setq m-element (list count 
				   (quote 'u-vm-color-recipient-face) t t))
	     (setq count (+ 1 count))
	     (setq u-match (append u-match (list m-element)))
	     (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
	    ((or (string-equal value "F")
		 (string-equal value "UA")  ;; IS THIS CORRECT!????????
		 (string-equal value "UB")) ;; authors / recipients full names
	     (setq f-element "\\(To:.+\\)?\\([^:\n]+\\)?")
	     (setq m-element (list count
				   (quote 'u-vm-color-recipient-face) t t))
	     (setq count (+ 1 count))
	     (setq u-match (append u-match (list m-element)))
	     (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
	    ((string-equal value "h") ;; time
	     (setq f-element "\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)")
	     (setq m-element (list count (quote 'u-vm-color-time-face))))
	    ((string-equal value "H") ;; time -- short
	     (setq f-element "\\([0-9][0-9]:[0-9][0-9]\\)")
	     (setq m-element (list count (quote 'u-vm-color-time-face))))
	    ((string-equal value "i") ;; id
	     (setq f-element "\\(<[^ \n]+>\\)")
	     (setq m-element (list count (quote 'u-vm-color-id-face))))
	    ((string-equal value "I") ;; indentation
	     (setq f-element " *")
	     (setq m-element nil))
	    ((string-equal value "l") ;; number of lines
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-length-face))))
	    ((string-equal value "L") ;; label
	     (setq f-element "\\(.*\\)")
	     (setq m-element (list count (quote 'u-vm-color-label-face))))
	    ((string-equal value "m") ;; month
	     (setq f-element "\\([A-Za-z]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "M") ;; month -- numeric
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "n") ;; message number
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element  (list count (quote 'u-vm-color-number-face))))
	    ((string-equal value "s") ;; subject
	     (setq f-element "\\(.*\\)")
	     (setq m-element (list count (quote 'u-vm-color-subject-face))))
	    ((string-equal value "t") ;; recipient addresses
	     (setq f-element "\\([^ \n]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-recipient-face))))
	    ((string-equal value "T") ;; recipient full names
	     (setq f-element "\\(.+\\)")
	     (setq m-element (list count (quote 'u-vm-color-recipient-face))))
	    ((string-equal value "w") ;; week day (is missing in some mails!)
	     (setq f-element "\\([A-Za-z ]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "y") ;; year
	     (setq f-element "\\([0-9][0-9][0-9][0-9]\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "z") ;; timezone
	     (setq f-element "\\(.+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face))))
	    ((string-equal value "*") ;; mark-marker
	     (setq f-element "\\(\\*\\| \\)")
	     (setq m-element (list count (quote 'u-vm-color-attribute-face))))
	    (t ;; user defined and everything else
	     (setq f-element ".*")
	     (setq m-element nil)))
      (setq u-format (concat u-format f-element rest))
      (if m-element
	  (progn
	    (setq count (+ 1 count))
	    (setq u-match (append u-match (list m-element)))))
      )
    (setq u-format (concat u-format "$"))
    (append (list u-format) u-match)))

(defconst u-vm-color-continued-header-contents
  "\\(\n[ \t]+.*\\)?\\(\n[ \t]+.*\\)?\\(\n[ \t]+.*\\)?\\(\n[ \t]+.*\\)?$")  

(defvar u-vm-color-presentation-keywords
  (list
   (list (concat "^\\(From\\|Sender\\): .*"
		 u-vm-color-continued-header-contents)
	 '(0 'u-vm-color-author-face t))
   (list (concat "^\\(To\\|Cc\\|Bcc\\|Fcc\\): .*"
		 u-vm-color-continued-header-contents)
	 '(0 'u-vm-color-recipient-face t))
   (list (concat "^Subject: .*" u-vm-color-continued-header-contents)
	 '(0 'u-vm-color-subject-face t))
   (list (concat "^Date: .*" u-vm-color-continued-header-contents)
	 '(0 'u-vm-color-date-face t))
   (list "^ *[-A-Za-z0-9]*> *.*$" 
	 '(0 'u-vm-color-citation-1-face t))
   (list "^ *[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *.*\\)$"   
	 '(1 'u-vm-color-citation-2-face t))
   (list "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *.*\\)$" 
	 '(1 'u-vm-color-citation-3-face t))
   (list "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *.*\\)$" 
	 '(1 'u-vm-color-citation-4-face t))
   (list "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *.*\\)$" 
	 '(1 'u-vm-color-citation-5-face t))
   (list (concat 
	  "^-- ?\n"
	  "\\(.*\n\\)?\\(.*\n\\)?\\(.*\n\\)?\\(.*\n\\)?.*\n?$")
	 '(0 'u-vm-color-signature-face t))
   (list (concat "^\\([A-Z][A-Za-z-]+\\): .*"
		 u-vm-color-continued-header-contents)
	 '(0 'u-vm-color-default-face)
	 '(1 'u-vm-color-header-face t))
   )
  "Font-lock keywords for vm presentation mode."
  )

;; Don't need it anymore:
(makunbound 'u-vm-color-continued-header-contents)


(defvar u-vm-color-summary-mode nil)
(make-variable-buffer-local 'u-vm-color-summary-mode)
(add-to-list 'minor-mode-alist '(u-vm-color-summary-mode nil))

;; FIXME: u-vm-color-summary-mode cannot be turned off
(defun u-vm-color-summary-mode (&optional arg)
  "Configure `font-lock-keywords' and add some hooks for vm-buffers."
  (interactive "P")
  (setq u-vm-color-summary-mode 
	(not (or (and (null arg) u-vm-color-summary-mode)
		 (<= (prefix-numeric-value arg) 0))))

  ;; apparently emacs expects this statement here...
  (if (not (string-match "XEmacs\\|Lucid" emacs-version))
      (font-lock-mode 1))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords (list (u-vm-color-make-summary-keywords)))
  ;; but xemacs here...
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (font-lock-mode 1))
  )

(defvar u-vm-color-presentation-mode nil)
(make-variable-buffer-local 'u-vm-color-presentation-mode)
(add-to-list 'minor-mode-alist '(u-vm-color-presentation-mode nil))
(defun u-vm-color-presentation-mode (&optional arg)
  "Configure `font-lock-keywords' and add some hooks for vm-buffers."
  (interactive "P")
  (setq u-vm-color-presentation-mode 
	(not (or (and (null arg) u-vm-color-presentation-mode)
		 (<= (prefix-numeric-value arg) 0))))

  ;; apparently emacs expects this statement here...
  (if (not (string-match "XEmacs\\|Lucid" emacs-version))
      (font-lock-mode 1))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords u-vm-color-presentation-keywords)
  ;; but xemacs here...
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (font-lock-mode 1))
  )

(provide 'u-vm-color)
;;; u-vm-color.el ends here
