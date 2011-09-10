;;; mew-minibuf.el --- Minibuffer input methods for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-minibuf-version "mew-minibuf.el version 0.10")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keymap and completion switch
;;;

(defvar mew-input-map nil)

(if mew-input-map
    ()
  (setq mew-input-map (make-sparse-keymap))
  (define-key mew-input-map " "    'mew-input-complete)
  (define-key mew-input-map "\t"   'mew-input-complete)
;;  (define-key mew-input-map "\177" 'backward-delete-char)
;;  (define-key mew-input-map "\C-h" 'mew-complete-backscroll)
  (define-key mew-input-map "\r"   'exit-minibuffer)
  (define-key mew-input-map "\n"   'exit-minibuffer)
  (define-key mew-input-map "\C-g" 'abort-recursive-edit)
  (define-key mew-input-map "\M-p" 'previous-history-element)
  (define-key mew-input-map "\M-n" 'next-history-element)
  )

(defvar mew-input-complete-function nil)

(defun mew-input-complete ()
  "Do completion according to the global variable
\"mew-input-complete-function\"."
  (interactive)
  (if (and mew-input-complete-function (fboundp mew-input-complete-function))
      (funcall mew-input-complete-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew original completion
;;;

(defun mew-input-clear ()
  "A function to clean up side effects of window configuration
at completions."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    ;; mew-complete-window-config is shared by many functions
    ;; because minibuffer is just one!
    (setq mew-complete-window-config nil)))

;;;
;;; Folder
;;;

(defvar mew-input-folder-hist nil)

(defun mew-input-folder (default)
  (let ((prefix (substring default 0 1))
	 folder)
    (mew-input-clear)
    (setq mew-input-complete-function (function mew-complete-folder))
    (setq folder (read-from-minibuffer (format "Folder name (%s): " default)
				       prefix
				       mew-input-map
				       nil
				       'mew-input-folder-hist))
    (directory-file-name 
     (if (or (string= folder "") (string= folder prefix))
 	 default
       folder))))

(defun mew-input-folders (default &optional prompt)
  (let (form folders)
    (if prompt
	(progn
	  (setq form "Folder name : ")
	  (setq default prompt))
      (setq form (format "Folder name (%s): " default))
      (setq prompt "+"))
    (mew-input-clear)
    (setq mew-input-complete-function (function mew-complete-folder))
    (setq folders (read-from-minibuffer form
					prompt
					mew-input-map
					nil
					'mew-input-folder-hist))
    (if (or (string= folders "") (string= folders "+"))
	(setq folders default))
    (mapcar (function directory-file-name) 
	    (mapcar (function mew-chop)
		    (mew-split folders ?,)))))

;;;
;;; Address
;;;

(defvar mew-input-address-hist nil)

(defun mew-input-address (prompt &optional default)
  (mew-input-clear)
  (setq mew-input-complete-function (function mew-complete-address))
  (let (tmp)
    (setq tmp (read-from-minibuffer 
	       (if default (format prompt default) prompt)
	       ""
	       mew-input-map
	       nil
	       'mew-input-address-hist))
    (if (and default (string= tmp ""))
	(setq tmp default))
    (mew-addrstr-canonicalize-address tmp)))

;;;
;;; Pick pattern
;;;

(defvar mew-input-pick-pattern-hist nil)

(defun mew-input-pick-pattern ()
  (mew-input-clear)
  (setq mew-input-complete-function (function mew-complete-pick-pattern))
  (let ((keymap (copy-keymap mew-input-map)) ret)
    (define-key keymap " " nil)
    (setq ret
	  (mew-pick-canonicalize-pattern
	   (mew-pick-macro-expand-string
	    (read-from-minibuffer "pick pattern: "
				  mew-pick-default-field
				  keymap
				  nil
				  'mew-input-pick-pattern-hist))))
    (mew-decode-syntax-delete)
    ret))

;;;
;;; Sort key
;;;
;;; mew-sort-default-key-alist

(defvar mew-input-sort-key-hist nil)

(defun mew-input-sort-key (mew-sort-key)
  (mew-input-clear)
  (setq mew-input-complete-function (function mew-complete-sort-key))
  (let* ((field:mode (read-from-minibuffer
		      (format "Sort by (%s)? : " mew-sort-key)
		      ""
		      mew-input-map
		      nil 
		      'mew-input-sort-key-hist))
	 field mode)
    (if (or (null field:mode) (equal field:mode ""))
	(setq field:mode mew-sort-key))
    (setq field (car (mew-split field:mode ?:)))
    (setq mode  (or (car (cdr (mew-split field:mode ?:)))
		    (cdr (assoc field mew-sort-key-alist))
		    "text"))
    (cons field mode)))

;;;
;;; Remote file
;;;

(defvar mew-input-rfile-hist nil)

(defun mew-input-rfile (prompt) ;; prompt="To:"
  (mew-input-clear)
  (setq mew-input-complete-function (function mew-complete-rfile))
  (read-from-minibuffer
   (concat prompt " ")
   ""
   mew-input-map
   nil
   'mew-input-rfile-hist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs primitive completion
;;;

;;;
;;; Range
;;;

(defun mew-input-range (folder alist askp)
  "Return (range erase-update)."
  ;; for the case when parts are expanded in the bottom of the folder
  (let* ((pair (mew-assoc-match2 folder alist 0))
	 (default (or (cdr pair) "update")) ;; just in case
	 range ret)
    (if askp
	(setq range
	      (completing-read (format "Range (%s): " default)
			       (mapcar (function (lambda (x) (cons x x)))
				       mew-input-range-list))))
    (if (or (string= range "") (not range))
	(setq range default))
    (if (not (string-match "^[0-9a-zA-Z]" range))
	(error "Illegal range: %s" range))
    (cond
     ;; range is other than "update"
     ((not (string= range "update"))
      (setq ret (list range 'erase))) ;; non-update, erase it
     ;; update
     ((get-buffer folder)
      (setq ret (mew-update-range)))
     ;; update but folder doesn't exist in Emacs. 
     (t (setq ret (list "all" 'update)))) ;; no need to erase
    (mew-decode-syntax-delete)
    ret))

(defun mew-update-range ()
  (save-excursion
    (goto-char (point-max))
    (if (bobp)
	(list "all" 'update) ;; buffer is empty. no need to erase
      (forward-line -1)
      (mew-summary-goto-message)
      (list 
       (concat
	(int-to-string (1+ (string-to-int (mew-summary-message-number))))
	"-" 
	"last")
       'update)))) ;; this is update!

;;;
;;; File
;;;

(defun mew-input-file-name (&optional prompt default)
  (let ((msg (or prompt "File: "))
	(use-dialog-box nil)
	file)
    (cond
     ((null default)
      (setq file mew-home))
     ((or (string-match (format "^[~%s]" mew-path-separator) default)
        ;; allow drive letter -- "^\\([A-Za-z]:\\|[~%s]\\)"
        (string-match (format "^[A-Za-z]:%s.+" mew-path-separator) default))
      (setq file default))
     (t
      (setq file (concat mew-home default))))
    (expand-file-name (read-file-name msg file file))))

(defun mew-input-directory-name (&optional default)
  (let ((dir (expand-file-name (read-file-name "Directory : " default nil t))))
    (if (file-directory-p dir)
	dir
      (message "%s is not directory" dir)
      (sit-for 1)
      (mew-input-directory-name default))))

(defun mew-convert-to-home-dir (dir)
  (let* ((chome (file-name-as-directory mew-home))
	 (ehome (expand-file-name chome)))
    (if (string-match ehome dir)
	(concat chome (substring dir (match-end 0) nil))
      dir)))

(defvar mew-summary-previous-directory nil)
(defvar mew-draft-previous-directory nil)

(defmacro mew-mode-input-file-name (prompt file preservep previous modedir)
  (` (let (dir ret def)
       (if (and (, file) (file-name-absolute-p (, file)))
	   (setq def (mew-convert-to-home-dir (, file)))
	 (if (, preservep)
	     (setq dir (or (, previous) (, modedir)))
	   (setq dir (, modedir)))
	 (setq dir (and dir (file-name-as-directory dir)))
	 (setq def (concat dir (, file))))
       (setq ret (mew-input-file-name (, prompt) def))
       (if (, preservep)
	   (setq (, previous)
		 (file-name-directory (mew-convert-to-home-dir ret))))
       ret)))

(defun mew-summary-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-summary-preserve-dir
			    mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-draft-preserve-dir
			    mew-draft-previous-directory mew-copy-dir))

(defmacro mew-mode-input-directory-name (preservep previous modedir)
  (` (if (, preservep)
	 (let (dir ret)
	   (setq dir (file-name-as-directory (or (, previous) (, modedir))))
	   (setq ret (mew-input-directory-name dir))
	   (setq (, previous) (mew-convert-to-home-dir ret))
	   ret)
       (mew-input-directory-name))))

(defun mew-summary-input-directory-name ()
  (mew-mode-input-directory-name mew-summary-preserve-dir
				 mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-directory-name ()
  (mew-mode-input-directory-name mew-draft-preserve-dir
				 mew-draft-previous-directory mew-copy-dir))
;;;
;;; String
;;;

(defun mew-input-string (prompt subdir default)
  (let ((input (read-string (format prompt subdir default) "")))
    (if (string= input "") default input)))

;;;
;;; Type
;;;

(defun mew-input-type (prompt filename default type-list)
  (let ((completion-ignore-case t)
	(type))
    (setq type (completing-read
		(format prompt filename default)
		(mapcar (function (lambda (x) (cons x x))) type-list)
		nil
		t  ;; not require match
		""))
    (if (string= type "") default type)))

;;;
;;; Config
;;;

(defun mew-input-config (default)
  (let ((config))
    (setq config (completing-read
		  (format "Config value (%s): "
			  (or default mew-config-default))
		  (mapcar (function (lambda (x) (cons x x))) mew-config-list)
		  nil t nil))
    (if (string= config "")
	(or default mew-config-default)
      config)))

;;;
;;;
;;;

(defun mew-input-general (prompt alist &optional require-match initial)
  (let* ((completion-ignore-case t)
	 (question (if initial (format "%s (%s) : " prompt initial)
		     (format "(%s) : " prompt)))
	 (value (completing-read question alist nil require-match nil)))
    (if (and initial (string= value "")) initial value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; password function
;;;

(defvar mew-passwd-alist nil)
(defvar mew-passwd-timer-id nil)

(defmacro mew-passwd-get-passwd (key)
  (` (nth 1 (assoc (, key) mew-passwd-alist))))
(defmacro mew-passwd-get-counter (key)
  (` (nth 2 (assoc (, key) mew-passwd-alist))))

(defun mew-passwd-set-passwd (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 1 (assoc key mew-passwd-alist)) val)
    (setq mew-passwd-alist (cons (list key val 0) mew-passwd-alist))))
(defun mew-passwd-set-counter (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 2 (assoc key mew-passwd-alist)) val)))

(defun mew-passwd-get-keys ()
  (mapcar (function car) mew-passwd-alist))

(defmacro mew-passwd-reset ()
  '(setq mew-passwd-alist nil))

(defun mew-passwd-setup ()
  (if mew-use-timer
      (setq mew-passwd-timer-id
	    (mew-timer mew-passwd-timer-unit (function mew-passwd-timer)))))

(defun mew-passwd-clean-up ()
  (mew-passwd-reset)
  (if mew-passwd-timer-id
      (progn
	(if (not (fboundp 'disable-timeout))
	    (require 'timer))
	(cond
	 ((fboundp 'disable-timeout) (disable-timeout mew-passwd-timer-id))
	 ((fboundp 'cancel-timer) (cancel-timer mew-passwd-timer-id)))))
  (setq mew-passwd-timer-id nil))

(defun mew-passwd-timer (&optional arg) ;; for XEmacs
  (let ((keys (mew-passwd-get-keys)) key)
    (while keys
      (setq key (car keys))
      (setq keys (cdr keys))
      (if (< (mew-passwd-get-counter key) mew-passwd-lifetime)
	  (mew-passwd-set-counter key (1+ (mew-passwd-get-counter key)))
	;; time out
	(mew-passwd-set-passwd key nil)
	(mew-passwd-set-counter key 0))))
  ;; repeat every 10 minutes
  (mew-passwd-setup))

(defun mew-input-passwd (prompt &optional key)
  (if key
      (if (mew-passwd-get-passwd key)
	  (progn
	    (sit-for 0 1)  ;; timing problem, sigh
	    (if mew-passwd-reset-timer
		(mew-passwd-set-counter key 0))
	    (mew-passwd-get-passwd key))
	(let ((pass (mew-read-passwd prompt)))
	  (mew-passwd-set-passwd key pass)
	  (mew-passwd-set-counter key 0)
	  pass))
    (mew-read-passwd prompt)))

(defun mew-read-passwd (prompt)
  (let ((inhibit-input-event-recording t))
    (if (fboundp 'read-passwd)
	(read-passwd prompt)
      (let ((pass "")
	    (c 0)
	    (echo-keystrokes 0)
	    (ociea cursor-in-echo-area))
	(condition-case nil
	    (progn
	      (setq cursor-in-echo-area 1)
	      (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e) (/= c 7)) ;; ^G
		(message "%s%s"
			 prompt
			 (make-string (length pass) ?.))
		(setq c (read-char-exclusive))
		(cond
		 ((char-equal c ?\C-u)
		  (setq pass ""))
		 ((or (char-equal c ?\b) (char-equal c ?\177))  ;; BS DELL
		  ;; delete one character in the end
		  (if (not (equal pass ""))
		      (setq pass (substring pass 0 -1))))
		 ((< c 32) ()) ;; control, just ignore
		 (t
		  (setq pass (concat pass (char-to-string c))))))
	      (setq cursor-in-echo-area -1))
	  (quit
	   (setq cursor-in-echo-area ociea)
	   (signal 'quit nil)))
	(setq cursor-in-echo-area ociea)
	(message "")
	(sit-for 0)
	pass))))

(provide 'mew-minibuf)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-minibuf.el ends here
