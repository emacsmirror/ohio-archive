;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     unix.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Created:  Wed Jan 20 12:24:16 EST 1988				     ;;
;;	Contents: some useful unix interface routines for gnueamcs           ;;
;;									     ;;
;;	Copyright (c) 1988 Wolfgang Rupprecht.				     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs and this file unix.el, are distributed in the hope
;; that they will be useful, but WITHOUT ANY WARRANTY.  No author or
;; distributor accepts responsibility to anyone for the consequences of
;; using them or for whether they serve any particular purpose or work at
;; all, unless he says so in writing.  Refer to the GNU Emacs General
;; Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs and unix.el, but only under the conditions described in the
;; GNU Emacs General Public License.  A copy of this license is supposed
;; to have been given to you along with GNU Emacs so you can know your
;; rights and responsibilities.  It should be in a file named COPYING.
;; Among other things, the copyright notice and this notice must be
;; preserved on all copies.

(autoload 'compile1 "compile")

(defun lint (buffer)
  "Run lint(1) on specified buffer and collect output in a buffer.
While lint runs asynchronously, you can use the \\[next-error] command
to find the text that lint gripes refer to."
  (interactive "bbuffer to lint ")
  (save-excursion
    (switch-to-buffer buffer t)
    (save-buffer buffer)
    (compile1 (concat "lint -abchx " buffer-file-name)
	      "No more lint gripes" "lint")))

(defun cindent (buffer)
  "Reformat the specified BUFFER using the Unix indent(1) program.
Selects the specified buffer, and saves it to disk, displays new version.
M-x revert-buffer and M-x undo work as expected. User may opt not to save
the newly indented buffer."
  (interactive "bbuffer to indent ")
  (switch-to-buffer buffer)
  (let ((auto-save-file-name (make-auto-save-file-name))
	(opoint (point)) )
    (save-buffer)
    (shell-command  (concat "indent -l80 -bl -bc -v " buffer-file-name
			    " " auto-save-file-name) nil)
    (if (file-exists-p auto-save-file-name)
	(progn
	  (erase-buffer)
	  (insert-file auto-save-file-name)
	  (goto-char (min opoint (point-max))))
      (error "indent failed to produce the output file"))))

(defun diff (buf &optional nocontext) 
"Take the diff(1) of a BUFFER and its oldest backup file. With prefix
arg, (or optional flag if noninteractive) does an normal non-context
diff. This option is required for \\[show-diff]."
  (interactive "bBuffer: ")
  (switch-to-buffer buf)
  (let*
      ((file-base-name (file-name-nondirectory buffer-file-name))
       (back-list
	 (file-name-all-completions file-base-name default-directory))
       (back-name (oldest-file back-list)))
    (if (interactive-p)
	(setq nocontext current-prefix-arg))
    (if (string-match back-name file-base-name)
	(message "No older backup of %s found." file-base-name)
      (compile1 (format "diff %s %s %s"
			(if nocontext "-r" "-c")
			back-name file-base-name)
		"Use show-diff to move to diffs"
		"diff"))))

(defun oldest-file (list)
  "Return the oldest file, from the LIST of files."
  (let ((oldest (car list)))
    (if list (setq list (cdr list)))
    (while list
      (if (file-newer-than-file-p oldest (car list))
	  (setq oldest (car list)))
      (setq list (cdr list)))
    oldest))

(defun grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive (list (read-input "Run grep (with args): "
				 (concat (symbol-around-point) " "
					 (other-possibly-interesting-files)))))
  (compile1 (concat "grep -n " command " /dev/null")
	    "No more grep hits" "grep"))

(defun gid (command)
  "Run gid, with user-specified args, and collect output in a buffer.
While gid runs asynchronously, you can use the \\[next-error] command
to find the text that gid hits refer to.  Gid is Greg Mcgary's
pre-digested-grep program, like ctags, but for grep."
  (interactive (list (read-input "Run gid (with args): "
				 (symbol-around-point))))
  (compile1 (concat "gid " command)
	    "No more gid hits" "gid"))

(defun other-possibly-interesting-files ()
  "Return a sh-regexp for other files that may be of intrest for
the purpose of grep-ing"
  (if (equal major-mode 'c-mode)
      "*.h *.c"				;for .h and .c files
      (concat "*" (and buffer-file-name
		       (string-match "\.[^.]+$" buffer-file-name)
		       (substring buffer-file-name
				  (match-beginning 0) (match-end 0))))))

(defun word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (let (beg)
      (if (not (looking-at "\\<"))
	  (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (buffer-substring beg (point)))))

(defun symbol-around-point ()
  "Return the symbol around the point as a string."
  (save-excursion
    (if (not (looking-at "\\s_\\|\\sw")) ; if not in a symbol
	(re-search-backward "\\s_\\|\\sw" nil t)) ; go into prev. one
    (buffer-substring
      (progn (forward-sexp 1) (point))
      (progn (backward-sexp 1) (point)))))
