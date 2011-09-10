;;; makefile mode for GNUemacs
;;; Last edited: Thu Oct 13 15:30:01 1988 by jcgs (John Sturdy) on harlqn

(provide 'makefile)

(defvar makefile-mode-map (make-sparse-keymap)
  "Keymap used in makefile mode.")

(define-key makefile-mode-map "$" 'insert-macro-reference)
(define-key makefile-mode-map "\C-c:" 'insert-target-reference)
(define-key makefile-mode-map ":" 'insert-target)
(define-key makefile-mode-map "=" 'insert-macro)
(define-key makefile-mode-map "\C-c\C-c" 'compile)

(defun insert-macro-reference (remake-macro-table)
  "Insert a makefile macro reference. With prefix argument, scan the buffer
afresh for macro definitions."
  (interactive "P")
  (if remake-macro-table
      (make-macro-name-table))
  (insert (format "${%s}" (completing-read "Macro name: " macro-name-table))))

(defun insert-target-reference (remake-target-table)
  "Insert a makefile target reference. With prefix argument, scan the buffer
afresh for target definitions."
  (interactive "P")
  (if remake-target-table
      (make-target-name-table))
  (insert (format "%s"                  ; might change format later
                  (completing-read "Target name: " target-name-table))))

(defun makefile-mode ()
  "Major mode for editing makefiles. Similar to fundamental-mode, but with
the following commands added:
\\{makefile-mode-map}"
  (interactive)
  (setq major-mode 'makefile-mode)
  (setq mode-name "Makefile")
  (setq comment-start "# ")
  (setq comment-end "")
  (setq comment-column 0)
  (setq comment-multi-line nil)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'makefile-indent-line)
  (make-local-variable 'fill-prefix)
  (setq fill-prefix "\C-i")
  (use-local-map makefile-mode-map)
  (make-variable-buffer-local 'macro-name-table)
  (make-variable-buffer-local 'target-name-table)
  (make-macro-name-table)
  (make-target-name-table))

(defvar macro-name-table
  ; From rmail.el! I couldn't find in the documentation any way of making
  ; obarrays, so I grepped around and found a solitary example of how to
  ; set them up. I don't know whether the size really matters. BTW how do
  ; make-variable-buffer-local and defvar interact?
  (make-vector 47 0)
  "Obarray of macro names defined in this makefile.  This is a
suitable format for completing-read.")

(defvar target-name-table (make-vector 47 0)
  "Obarray of target names defined in this makefile.  This is a
suitable format for completing-read.")

(defun make-macro-name-table ()
  "Prepare the macro name table."
  (interactive)
  (make-name-table "=" macro-name-table))

(defun make-target-name-table ()
  "Prepare the target name table."
  (interactive)
  (make-name-table ":" target-name-table))

(defun make-name-table (name-end-marker array)
  "Prepare a table of names (from the beginning of a line up to
NAME-END-MARKER) in the obarray ARRAY from the present buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward name-end-marker (point-max) t)
        (let ((end-of-name (1- (point))))
          (beginning-of-line 1)
          (intern (buffer-substring (point) end-of-name) array)
          (end-of-line 1))))))

(defun insert-target (target-name)      ; target is most likely a filename
  "Insert a target, TARGET-NAME, using the appropriate \"make(1)\"
syntax. Called interactively, TARGET-NAME is read as a filename, since
many targets are files. It is better to insert targets using this
command than simply to type them in, partly because this command
provides the right syntax, tabs and all; and also because it updates
the target table that GNUemacs uses for completions on target names."
  (interactive "FTarget name: ")
  (if (string-match
       (concat "^" (expand-file-name default-directory))
       (expand-file-name target-name))
      (setq target-name
            (substring (expand-file-name target-name) (match-end 0))))
  (intern target-name target-name-table)
  (end-of-line 1)
  (insert "\n" target-name ":\C-i;")
  (backward-char 1))

(defun insert-macro (macro-name)
  "Insert a macro, MACRO-NAME, using the appropriate \"make(1)\"
syntax. It is better to insert macros using this command than simply
to type them in, partly because this command provides the right
syntax; and also because it updates the macro table that GNUemacs uses
for completions on macro names."
  (interactive "sMacro name: ")
  (intern macro-name macro-name-table)
  (end-of-line 1)
  (insert "\n" macro-name "="))

(defun makefile-indent-line ()
  "Function to indent a makefile line. It doesn't yet really do the
right thing."
  (save-excursion
    (beginning-of-line 1)
    (let ((line-start (point)))
      (re-search-forward "[^\C-i ]")
      (delete-region line-start (1- (point)))
      (goto-char line-start)
      (if (not (looking-at "#"))
          (insert "\C-i")))))

;;; end of makefile.el
