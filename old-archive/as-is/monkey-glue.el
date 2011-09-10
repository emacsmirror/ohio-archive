;;;
;;; replace dired-noselect with monkey-directory-noselect
;;; (causes every invocation of dired to try monkey instead!)
;;;

(require 'dired)

(or (fboundp 'monkey-dired-noselect)
    (fset 'monkey-dired-noselect (symbol-function 'dired-noselect)))

(defvar monkey-instead-of-dired 'ask
  "*Select `monkey' in place of `dired' invocations if t.
If not t or nil, ask.")

(defun dired-noselect (dirname)
  "Invoke monkey or dired on DIRNAME.
Select `monkey-dired-noselect' [the original `dired-noselect'] or
`monkey-directory-noselect' depending on the value of
`monkey-instead-of-dired'."
  (funcall (if (or (eq t monkey-instead-of-dired) ; yes if t
		   (and monkey-instead-of-dired	; maybe if non-nil
			(y-or-n-p "Use monkey instead of dired? ")))
	       'monkey-directory-noselect
	     'monkey-dired-noselect)
	   dirname))
