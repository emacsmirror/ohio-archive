;;;From: jbw@bucsb.UUCP (Joe Wells)
;;;Subject: enhancements to uncompress.el
;;;Date: 10 Dec 88 21:38:12 GMT
;;;Reply-To: jbw@bucsf.bu.edu (Joe Wells)
;;;Organization: Boston Univ Comp. Sci.

;;;For those of you who enjoyed uncompress.el, here is an enhanced
;;;version of it.  It has the following bug fixes incorporated:

;;;1) It will work on read-only files.

;;;2) If you load it more than once, it will not keep adding its hooks to
;;;auto-mode-alist and write-file-hooks.  This version will check to see
;;;if the hooks are already in place.

;;;3) In the original uncompress.el, when you edited a compressed file,
;;;it would only uncompress the buffer, not the file on disk.  If you
;;;modified and then saved the buffer, it would save the buffer as an
;;;ordinary file and also leave the original compressed file in place.
;;;This version will uncompress the file on disk and make it the backup
;;;file.  This makes it easier to later recompress the changed file.

;;;Since my modifications depend on a buffer local variable, I am also
;;;including a fix that prevents kill-all-local-variables from nuking
;;;special variables every time the major mode of the buffer is changed.
;;;I think this behavior of kill-all-local-variables is *most* annoying.

;;;I have found this package to be incredibly helpful, since I keep most
;;;of my files compressed for lack of disk space.

;;;Have fun!

;;;--
;;;Joe Wells		INTERNET: jbw%bucsf.bu.edu@bu-it.bu.edu
;;;IP: [128.197.2.9]	UUCP: ...!harvard!bu-cs!bucsf!jbw

;;;-------------------------uncompress.fix.el----------------------------
;; Enhanced uncompression handling.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; jbw%bucsf.bu.edu@bu-it.bu.edu (school year)
;; joew%uswest@boulder.colorado.edu (summer)
;; This is based on original uncopyrighted work by an unknown (to me)
;; author.  Since it was uncopyrighted, I felt it was ok to place the
;; FSF copyright on the entire file, my work and the original authors
;; work.  If that's not ok, the FSF copyright still applies to my
;; portion of the work.

(require 'kill-fix)

(defvar buffer-compressed-file nil
  "Is the storage for this buffer on disk actually a compressed file?")
(make-variable-buffer-local 'buffer-compressed-file)
(put 'buffer-compressed-file 'preserved t)

(defun uncompress-backup-file ()
  "If the file that we are editing is a compressed file on disk,
uncompress it and make it the backup file."
  (or buffer-file-name
      (error "Buffer %s has no file-name" (current-buffer)))
  (let ((backup-name (car (find-backup-file-name buffer-file-name))))
    (cond ((and buffer-compressed-file
		make-backup-files	;what should I do here?
		(not buffer-backed-up)	;if claim backed-up, skip
		(not (file-exists-p buffer-file-name))
		(not (file-exists-p backup-name)))
	   (call-process "uncompress" nil nil nil buffer-file-name)
	   (cond ((file-exists-p buffer-file-name)
		  (condition-case ()
		      (rename-file buffer-file-name backup-name t)
		    (file-error
		     ;; If trouble writing the backup, write it in ~.
		     (setq backup-name (expand-file-name "~/%backup%~"))
		     (message "Cannot write backup file; backing up in %s"
			      backup-name)
		     (sleep-for 1)
		     (copy-file buffer-file-name backup-name 1 t)))
		  (kill-local-variable 'buffer-compressed-file)
		  (setq buffer-backed-up t))))))
  nil)

(or (memq 'uncompress-backup-file write-file-hooks)
    (setq write-file-hooks (cons 'uncompress-backup-file write-file-hooks)))

(defun uncompress-while-visiting ()
  "Temporary \"major mode\" used for .Z files, to uncompress the contents.
It then selects a major mode from the uncompressed file name and contents."
  (if (and (not (null buffer-file-name))
	   (string-match "\\.Z$" buffer-file-name))
      (set-visited-file-name
       (substring buffer-file-name 0 (match-beginning 0))))
  (message "Uncompressing...")
  (let ((buffer-read-only nil))
    (shell-command-on-region (point-min) (point-max) "uncompress" t))
  (message "Uncompressing...done")
  (set-buffer-modified-p nil)
  (setq buffer-compressed-file t)
  (normal-mode))

(or (equal (assoc "\\.Z$" auto-mode-alist)
	   '("\\.Z$" . uncompress-while-visiting))
    (setq auto-mode-alist
	  (cons '("\\.Z$" . uncompress-while-visiting) auto-mode-alist)))

(defun find-compressed-version ()
  "Hook to read and uncompress the compressed version of a file."
  ;; Just pretend we had visited the compressed file,
  ;; and uncompress-while-visiting will do the rest.
  (if (file-exists-p (concat buffer-file-name ".Z"))
      (progn
	(setq buffer-file-name (concat buffer-file-name ".Z"))
	(insert-file-contents buffer-file-name t)
	(goto-char (point-min))
	(setq error nil)
	t)))

(or (memq 'find-compressed-version find-file-not-found-hooks)
    (setq find-file-not-found-hooks
	  (cons 'find-compressed-version find-file-not-found-hooks)))

(provide 'uncompress)
---------------------------kill-fix.el--------------------------------
;; Enhancement to kill-all-local-variables
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; jbw%bucsf.bu.edu@bu-it.bu.edu (school year)
;; joew%uswest@boulder.colorado.edu (summer)

;; save the original subr function definition of kill-all-local-variables
(or (fboundp 'original-kill-all-local-variables)
    (fset 'original-kill-all-local-variables
	  (symbol-function 'kill-all-local-variables)))

(defun kill-all-local-variables ()
  "Eliminate all the buffer-local variable values of the current
buffer.  This buffer will then see the default values of all
variables.  NOTE: This function has been modified to ignore
buffer-local variables whose preserved property is non-nil."
  (let ((oldvars (buffer-local-variables)))
    (original-kill-all-local-variables)
    (while oldvars
      (let ((var (car (car oldvars))))
	(cond ((get var 'preserved)
	       (make-local-variable var)
	       (set var (cdr (car oldvars))))))
      (setq oldvars (cdr oldvars)))))

(provide 'kill-fix)
