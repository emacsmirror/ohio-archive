;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename        : ada-verdix.el
;; Description     :
;; Author          : Rolf EBERT
;; Created On      : Tue Nov  8 19:09:42 1994
;; Last Modified By: Rolf EBERT
;; Last Modified On: Fri Nov 11 22:06:00 1994
;; Update Count    : 3
;; $Locker:  $
;; $Log: ada-verdix.el,v $
; Revision 1.3  1994/11/11  21:07:21  re
; added C curleys  ada-make-filename-from-adaname for VADS compiler.
;
; Revision 1.2  1994/11/11  19:42:08  re
; added c. curleys ada-hook for compiling with Sun Ada.
;
; Revision 1.1  1994/11/08  18:14:09  re
; Initial revision
;
;; Status          : Unknown, Use with caution!

;;; set some functions for the VADS compiler environment.

(require 'ada-mode)

(setq ada-compiler 'vads)

;;; these are nor really compiler dependent, but often used in the
;;; VADS environment. 
(setq ada-spec-suffix "_s.a")
(setq ada-body-suffix "_b.a")

;; Delete ".a" from completion-ignored-extensions:
(let ((extensions '()))
  (while completion-ignored-extensions
    (if (not (string= (car completion-ignored-extensions) ".a"))
	(setq extensions (cons (car completion-ignored-extensions) extensions)))
    (setq completion-ignored-extensions (cdr completion-ignored-extensions)))
  (setq completion-ignored-extensions (reverse extensions)))

;; This is for compiling with the Sun Ada compiler.  It generates a
;; compile command for the current buffer and appends the
;; corresponding regexp for placing the cursor on the error.
;;
;; From Charles Curley <ccurley@atl.ge.com>
(add-hook 'ada-mode-hook
	  '(lambda ()
	     (make-local-variable 'compile-command)
	     (setq compile-command
		   (concat "ada "	;; Sun Ada compiler command
			   (cond (buffer-file-name
				  (file-name-nondirectory
				   buffer-file-name))
				 ((buffer-name)))))
	     (if (not (boundp 'compilation-error-regexp-alist))
		 (progn
		   (load-library "compile")
		   (setq compilation-error-regexp-alist
			 (append '(("\\([^ :\n]+\\), line \\([0-9]+\\), char \\([0-9]+\\):" 1 2 3))
				 compilation-error-regexp-alist))))))


(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename of a package/procedure from its Ada package name."
  ;; This is done simply by calling a.which, when we work with Sun Ada.
  (let ((which-buf (generate-new-buffer "*a.which*")))
    (save-excursion
      (set-buffer which-buf)
      (call-process "a.which" nil t nil (downcase adaname))
      (goto-char (point-min))
      (if (looking-at "^a.which: [^\n]+ not found in searched libraries")
 	  (setq adaname nil)
 	(setq adaname (buffer-substring (point-min) (progn
 						      (goto-char (point-min))
 						      (end-of-line)
 						      (point)))))
      (kill-buffer nil)
      adaname)))
