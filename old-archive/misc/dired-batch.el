;; LCD Archive Entry:
;; dired-batch|Karsten Wenger|kw@cis.uni-muenchen.de|
;; dired extension to evaluate elisp programs|
;; 11-Mar-1997|1|~/misc/dired-batch.el.gz|

;;; dired-batch.el --- dired extension to evaluate elisp programs
;; Author: Karsten Wenger
;; Created: 1997/03/11 
;; Version: 1
;; Keywords: dired, elisp calls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997 by Karsten Wenger (kw@cis.uni-muenchen.de)

;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toolbar specific function for XEmacs 19.12+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------

;  DIRED-BATCH : A SMALL DIRED EXTENSION

; Dired command "W" takes a lisp function and applies it to all marked files in dired buffer
; the result of processing all files is returned as a list
; If a file is modified, it will be safed
; Warning: there is no check on file status i.e. directory, read-only etc !
; Called with non-nil prefix argument the job is passed to a subshell
; this is useful if you are working on large files

; USAGE
; 1. mark files under dired
; 2. type W (optional prefix argument)
;    The prompt "Eval Lisp Function:" appears in Minibuffer
; 3. type in lisp function call, for example: (change "xxx" "yyy")
; 4. type Return
;


(setq dired-mode-hook '(lambda () (define-key dired-mode-map "W" 'dired-eval-on-marked-files)))



(defun dired-eval-on-marked-files (shell function)
"apply any lisp function to marked files in dired buffer"
  (interactive "P\nxEval Lisp Function:")
 (let (result)
  (if shell
     (do-shell-job (dired-get-marked-files) function)
     (progn (setq result (many-files (dired-get-marked-files) function))
            (dired-do-redisplay)
            (message "Result is: %s" result)result))))

(defun do-shell-job (files function)
"job is passed to a subshell process ... produces ugly output, but works"
  (let ((shellname (shell)))
  (send-string shellname "setenv TERM xterm\n")
  (send-string shellname "emacs -nw\n")
  (send-string shellname (format "%s%s%S%s%S%s" "(many-files " "'"files "'"function ")" ))
  (send-string shellname "\n")
  (send-string shellname "(kill-emacs)\n")))

(defun many-files (list-of-files function) 
"Return list of results of each call to function on  LIST-OF-FILES."
 (let ((c 0)(al (length list-of-files))(files list-of-files) results)
  (while files                   
     (message "processing  no. %d of %d files, working on %s" (setq c (+ c 1)) al (car files))
     (setq results (append results (one-file  (expand-file-name (car files)) function)))
     (setq files  (cdr files)))
  results))

(defun one-file (filename function)
  (save-excursion
    (let ((buffer (find-file-noselect filename))result)
      (set-buffer buffer)
      (widen)   
      (goto-char (point-min))
      (setq result (eval function))
      (if (buffer-modified-p) (save-buffer))
      (kill-buffer buffer)
      (list result))))
