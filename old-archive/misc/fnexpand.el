;; @(#) fnexpand.el -- filename expansion anywhere

;; Copyright (C) 1991 Free Software Foundation, Inc.

;; Author: <eirik@theory.tn.cornell.edu> and <trost@reed.edu>
;; Adapted-By: Jari Aalto <jari.aalto@poboxes.com>
;; Idea by:  karl@cs.umb.edu
;; Keywords: tools

;; LCD Archive Entry:
;; fnexpand|eirik, trost, Jari Aalto|jari.aalto@poboxes.com|
;; finename expansion anywhere|
;; 19-Mar-1997|1.5|~/misc/fnexpand.el.gz|


;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Install:

;;   Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file.
;;
;;	(require 'fnexpand)
;;
;;   or use this; your .emacs loads up a bit quicker
;;
;;	(autoload 'fnexpand-complete "fnexpand" t t)
;;
;;   You should also add some keybinding to use the expansion feature
;;   The following example replaces seldom used original emacs binding.
;;
;;	(global-set-key "\e\ " 'fnexpand-complete)
;;
;;   If you want to expand executables, you should add following statement
;;   before any require command. Loading this package slows down remarkably
;;   after this though.
;;
;;	(setq fnexpand-executable-enable t)
;;

;;; Commentary:

;;  The enclosed elisp code provides completion of user names and
;;  environment variables, as well as a function which does filename
;;  completion "in place", in any buffer.  The latter function is
;;  particularly useful as the local binding of the TAB key in shell
;;  mode, but it can be used in global bindings too.
;;
;; Code to do completion of $envvar and ~username in filenames
;; Code to do completion of filenames in place (e.g. in shell buffers)
;; Sample binding:
;;
;; (setq shell-mode-hook
;;      (function
;;        (lambda () (local-set-key "\^I" 'fnexpand-complete ))))
;;
;;
;; If you want to expand
;;
;;
;; The code that does expansion on $ and ~ only works if the $ or ~ is
;; either at the beginning of the buffer, or after "/".  One result of
;; this is that file names which end in ~ are still allowed.


;;; Change Log:

;; [jari]   = <jari.aalto@poboxes.com>
;;
;; May  17       1996    [jari]          19.28   v1.2-1.5	Released
;; - Found this code lying around in my ~/elisp. I cannot recall where
;;   did I get this file.
;; - Rewrote most of the package. Added fnexpand- prefix to every function
;;   and variable. Added the executable file expanding.
;; - got rid of the redefinitions of emacs functions. Cleared the pacakge.


;;; Code:


(require 'assoc)

;;; ... private variables .................................... private ...

(defconst fnexpand-version
  "$Id: fnexpand.el,v 1.5 1997/02/14 10:26:02 jaalto Released $"
  "Latest RCS modification time and version number.")


(defvar fnexpand-envvars nil
  "Private, a list of environment variable names and values.
Format: '((ENV-VAR-NAME . ENAV-VAR-VALUE).")

(defvar fnexpand-executable-file-cache  nil
  "Private, list of executable files. The list is updated periodically.
Format: '((FILE . 1) (FILE . 2) ..).")

(defvar fnexpand-executable-file-cache-counter  nil
  "Private, incremented every time when executable file cache is asked.")



;;; ... user configurable ..................................... config ...

;;; You don't want to update cache very often...
;;;
(defvar fnexpand-executable-file-cache-update  200
  "*Counter when to update fnexpand-executable-file-cache.
Default every 200th call. See also 'fnexpand-executable-enable'.")

(defvar fnexpand-executable-cache-no-dirs  "RCS"
  "*Regexp, which directories in path not to cache. Eg looking into
RCS directory makes no sense.")

(defvar fnexpand-executable-enable nil
  "*if non-nil, then try to expand executable files too.
Beware, this may be time consuming.")

(defvar fnexpand-filename-boundary-chars "[^#$%+-9=@-Z_a-z~]"
  "*Characters used to bound filenames in 'fnexpand-find-filename'.")

(defvar fnexpand-complete-filename-look-right nil
  "*If t, consider text on both sides of point in fnexpand-complete-filename.")

(defvar fnexpand-yp-passwd-buffer "*yp passwords*"
  "Password buffer name.")

;;; .................................................... compatibility ...

(cond
 ((fboundp 'read-file-name-internal-primitive)
  (fset 'fnexpand-read-file-name-internal-primitive
	'read-file-name-internal-primitive))
 (t
  (fset 'fnexpand-read-file-name-internal-primitive
	(symbol-function 'read-file-name-internal))
  ))


;;; ............................................................ funcs ...


(defun fnexpand-getenv  (&optional var)
  "Return env VAR slot. If VAR is t, then update
global list 'fnexpand-envvars' if needed and return all variables
in format '((ENV-VAR-NAME . ENAV-VAR-VALUE) (E-NAME. E-VAL) ..)"
  (cond
   ((eq t var)
    (if fnexpand-envvars
	fnexpand-envvars		;read from cache
      (setq fnexpand-envvars
	    (mapcar
	     (function
	      (lambda (string)
		(let ((d (string-match "=" string)))
		  (cons (substring string 0 d)
			(and d (substring string (1+ d)))))))
	     process-environment))
      ))
   (t
    (getenv var)
    )))



(defun fnexpand-read-file-name-internal (name dir action)
  "Like 'read-file-name-internal' that expands partial usernames and
environment variable names.

NAME is the filename to complete; DIR is the directory to complete in.
ACTION is nil to complete, t to return list of completions, lambda to
verify final value."
    (let* ((buf (current-buffer))
	   (char (progn
		   (set-buffer (get-buffer-create " *read*"))
		   (erase-buffer)
		   (insert name)
		   (and (re-search-backward "[$~]" nil t)
			(char-after (point)))))
	   (can (and char
		     (or (eq (point) (point-min))
			 (save-excursion (backward-char 1)
					 (looking-at "/")))
		     (not (progn
			    (forward-char 1)
			    (save-excursion
			      (search-forward "/"
						 (point-max) t))))
		     (buffer-substring (point) (point-max)))))
      (set-buffer buf)
      (if (null can) (fnexpand-read-file-name-internal-primitive
		      name dir action)
	(let ((prefix (substring name 0 (- (length name) (length can) 1))))
	  (cond
	   ((eq char ?~)
	    (let ((s (fnexpand-complete-username can nil action)))
	      (cond ((stringp s)
		     (concat "~" s
			     (and
			      (eq t (fnexpand-complete-username s nil action))
			      (file-directory-p
			       (expand-file-name (concat "~" s)))
			      "/")))
		    ((eq t s) (concat name
				      (if (file-directory-p
					   (expand-file-name name))
					  "/")))
		    (t s))))
	   ((eq char ?$)
	    (let ((completion-list
		   (all-completions
		    can (fnexpand-getenv t)))
		  )
	      (cond
	       ((null action)
		(let* ((un (and (eq (length completion-list) 1)
				(car completion-list)))
		       (unv (and un (fnexpand-getenv un)))
		       (dirp (and unv (> (length unv) 0)
				  (file-directory-p unv)
				  "/")))
		  (if (and un (string-equal un can))
		      (concat prefix unv dirp)
		    (let ((s (try-completion can (fnexpand-getenv t))))
		      (cond ((stringp s) (concat prefix "$" s dirp))
			    (t s))))))
	       ((eq t action) completion-list)
	       (t (eq 1 (length completion-list)))))))))))




(defun fnexpand-find-completing-names (string predicate yp-p)
  "Looking for USERNAME completions matching PREDICATE (if non-nil) in current
buffer.  Does not do save-excursion.  If third argument YP-P is non-nil, allow
matches for individual yp entries as well."
  (let ((regexp (concat (if yp-p "^+?" "^") string "[^:]*:"))
	ret)
    (goto-char (point-min))
    (while (re-search-forward regexp () t)
      (let ((name (buffer-substring (match-beginning 0) (1- (match-end 0)))))
	(if (or (not predicate) (funcall predicate name))
	    (setq ret (cons (if (eq (string-to-char name) ?+)
				(substring name 1)
			      name)
			    ret)))
	(end-of-line)))
    ret))




(defun fnexpand-complete-username (string predicate flag)
  "Use /etc/passwd file to expand a ~.  A \"+\" at the beginning of the
line is assumed to indicate a yp entry."
  (if (string-match ":" string) nil
    (save-excursion
      (let ((pwbuf (get-file-buffer "/etc/passwd")))
	(if pwbuf
	    (progn
	      (set-buffer pwbuf)
	      (or (verify-visited-file-modtime pwbuf)
		  (revert-buffer t t)))
	  (set-buffer (create-file-buffer "/etc/passwd"))
	  (insert-file-contents "/etc/passwd" t)))

      (goto-char (point-min))
      (let ((yp-p (save-excursion (re-search-forward "^+:" nil t))))
	(if (and yp-p
		 (not (get-buffer fnexpand-yp-passwd-buffer)))
	    (call-process "ypcat" nil
			  (get-buffer-create fnexpand-yp-passwd-buffer)
			  nil "passwd"))
	(cond
	 ((eq flag t)
	  (nconc (fnexpand-find-completing-names string predicate t)
		 (if yp-p
		     (progn
		       (set-buffer (get-buffer fnexpand-yp-passwd-buffer))
		       (fnexpand-find-completing-names
			string predicate nil)))))
	 (flag				; should this be (eq flag 'lambda)?
	  (if (or (re-search-forward (concat "^+?" string ":") nil t)
		  (and yp-p
		       (progn
			 (set-buffer fnexpand-yp-passwd-buffer)
			 (re-search-forward (concat "^" string ":") nil t))))
	      t))
	 (t
	  (let ((list (mapcar 'list
			      (fnexpand-complete-username string nil t))))
	    (or (and (eq (length list) 1)
		     (fnexpand-complete-username string predicate 'lambda))
		(try-completion string list)))))))))



(defun fnexpand-path-list  ()
  "Return PATH in list format '(PATH PATH ..). Only unique paths are
returned."
  (let* ((path (or (getenv "PATH")
		   (getenv "path")))
	 list
	 elt
	 )
    (while path
      (cond
       ((string-match "^[^:]+" path)
	(setq elt (substring path 0 (match-end 0)))

	(if (> (length path) (match-end 0))
	    (setq path  (substring path (1+ (match-end 0))))
	  (setq path nil))		;no more

	;; make sure, has ending slash
	(if (not (string-match "/$" elt))
	    (setq elt (concat elt "/")))


	;; consing is faster that append.
	(if (not (member elt list))
	    (setq list (cons elt list )))
	)))
    (reverse list)			;preserve order
    ))


(defun fnexpand-executables  (&optional verb)
  "Return all unique executable files. If VERB is non-nil, print
verbose messages during updating cache. Cache is updated only
if it's nil or of cache counter reaches certain value.

References:
  'fnexpand-executable-file-cache-counter'
  'fnexpand-executable-file-cache-update'
  'fnexpand-executable-file-cache'
"
  (let* ((counter 0)
	 path-list
	 path
	 list
	 file
	 )
    (if (integerp fnexpand-executable-file-cache-counter)
	(setq fnexpand-executable-file-cache-counter
	      (1+ fnexpand-executable-file-cache-counter))
      (setq fnexpand-executable-file-cache-counter 0))

    ;; time's up? update cache if needed
    ;;
    (cond
     ((or (null fnexpand-executable-file-cache)
	  (eq 0 (% fnexpand-executable-file-cache-counter
		   fnexpand-executable-file-cache-update
		   )))

      (setq fnexpand-executable-file-cache-counter 1
	    fnexpand-executable-file-cache         nil
	    path-list				   (fnexpand-path-list)
	    )

      (while path-list
	(setq path (car path-list))

	(if verb
	    (message (format "fnexpand: cacheing executables %s" path)))

	(cond
	 ((and (not (string-match fnexpand-executable-cache-no-dirs path))
	       (file-exists-p path))		;ignore non-existing paths
	  (setq files (directory-files path))
	  (mapcar
	   '(lambda (x)
	      (setq file (concat path x))

	      (cond
	       ((and (not (file-directory-p file))
		     (file-executable-p file)
		     (not (assoc x fnexpand-executable-file-cache))
		     )

		(setq fnexpand-executable-file-cache
		      (cons
		       (list x counter)
		       fnexpand-executable-file-cache ))
		(setq counter (1+ counter))
		)))
	   files)
	   ))

	(setq path-list (cdr path-list))
	) ;; while path-list

      (if verb (message ""))
      ))

    fnexpand-executable-file-cache
    ))



(defun fnexpand-executable-completions  (name)
  "Return executable completions for NAME. If there is only one completion,
return string."
  (let* ((list (fnexpand-executables 'verb))
	 ret
	 )
    (setq ret (all-completions name list))
    (if (eq 1 (length ret))
	(setq ret (car ret)))
    ret
    ))




(defun fnexpand-find-filename ()
  "Return the largest substring to the left of point which can contain
a file name. Ignore the most recent prompt in a shell buffer"
  (let ((mark (let ((process (get-buffer-process (current-buffer))))
		(and process (process-mark process)))))
    (buffer-substring
     (save-excursion
       (if (re-search-backward fnexpand-filename-boundary-chars
			       (and mark (>= (point) mark) mark)
			       1)
	   (1+ (point))
	 (point)))
     (point))))





;;;###autoload
(defun fnexpand-complete ()
  "Expand the file name, env var or command near point"
  (interactive)
  (and fnexpand-complete-filename-look-right
       (re-search-forward fnexpand-filename-boundary-chars nil 1)
       (forward-char -1))

  (let* ((name (fnexpand-find-filename))
	 (completion
	  (fnexpand-read-file-name-internal name default-directory nil)))


    (cond
     ((eq completion t)
      (insert " "))


     ((and (null completion)			;try command name
	   fnexpand-executable-enable)

      (setq completion (fnexpand-executable-completions name))

      (cond
       ((stringp completion)
	(delete-region (- (point) (length name)) (point))
	(insert completion))

       ((and (listp completion)
	     (> (length completion) 0))
	(save-window-excursion
	  (with-output-to-temp-buffer " *Completions*"
	    (display-completion-list
	     completion))
	    (sit-for 32767))
	)
       (t
	(message "[No match]")
       )))

     (completion
      (if (equal completion name)
	  (save-window-excursion
	    (with-output-to-temp-buffer " *Completions*"
	      (display-completion-list
	       (fnexpand-read-file-name-internal name
						 default-directory t)))
	    (sit-for 32767))
	(unwind-protect
	    (if (eq t (fnexpand-read-file-name-internal
		       completion
		       default-directory
		       nil))
		(setq completion (concat completion " "))))
	(delete-region (- (point) (length name)) (point))
	(insert completion)))

     (t (message "[No match]")))))



(cond
 (fnexpand-executable-enable		;update cache immediately
  (fnexpand-executables 'verb)
  ))

(provide 'fnexpand)


;;; fnexpand.el ends here
