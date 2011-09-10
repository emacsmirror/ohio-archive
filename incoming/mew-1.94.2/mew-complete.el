;;; mew-complete.el --- Completion magic for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 30, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-complete-version "mew-complete.el version 0.04")

(require 'mew)

;; hoping the functions here are free from marker problem
;; because it inserts any chars before \n. It is important
;; to use the position's color.
(fset 'mew-complete-insert (symbol-function 'insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low level functions
;;;

(defun mew-draft-on-field-p ()
  (if (bolp)
      (if (bobp) 
	  t
	(save-excursion
	  (forward-line -1)
	  (if (looking-at ".*,[ \t]?$") nil t)))
    (let ((pos (point)))
      (save-excursion
	(beginning-of-line)
	(if (looking-at mew-lwsp)
	    nil
	  (if (search-forward ":" pos t) nil t))))))
      
(defun mew-draft-on-value-p (switch)
  (save-excursion
    (beginning-of-line)
    (while (and (< (point-min) (point))	(looking-at mew-lwsp))
      (forward-line -1))
    (if (looking-at "\\([^:]*:\\)")
	(mew-assoc-match (mew-match 1) switch 0)
      nil))) ;; what a case reachs here?
  
;;
;; Window management for completion candidates
;;

(defvar mew-complete-candidates nil)

(defun mew-complete-window-delete ()
  (if (null mew-complete-window-config)
      ()
    ;; mew-complete-window-config remains when the last completion  
    ;; finished with multiple candidates.
    ;; (e.g. foo<RET> when foo and foobar are displayed.)
    ;; In this case, this function is called in another
    ;; completion thread but setting window configuration is not
    ;; desired. If we set window configuration with the old
    ;; mew-complete-window-config, the cursor jumps to mini buffer.
    ;; This was a stupid bug of Mew. So, let's see if the complete
    ;; buffer is displayed or not.
    (if (get-buffer-window mew-buffer-completions)
	(set-window-configuration mew-complete-window-config))
    (setq mew-complete-window-config nil))
  (and (get-buffer mew-buffer-completions)
       (kill-buffer mew-buffer-completions))
  (setq mew-complete-candidates nil))

(defun mew-complete-window-show (all)
  (or mew-complete-window-config
      (setq mew-complete-window-config (current-window-configuration)))
  (if (and (get-buffer-window mew-buffer-completions)
	   (equal mew-complete-candidates all))
      (let ((win (get-buffer-window mew-buffer-completions)))
	(save-excursion
	  (set-buffer mew-buffer-completions)
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win 1)
	    (scroll-other-window))))
    (setq mew-complete-candidates all)
    (with-output-to-temp-buffer
	mew-buffer-completions
      (display-completion-list all))))

(defun mew-complete-backscroll ()
  "Backscroll the *Completion* buffer."
  (interactive)
  (let* ((win (get-buffer-window mew-buffer-completions))
	 (height (and win (window-height win))))
    (and win (scroll-other-window (- 3 height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion function: C-i
;;;

(defun mew-draft-header-comp ()
  "Complete and expand address short names.
First, a short name is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
  (interactive)
  (if (mew-draft-on-field-p)
      (mew-complete-field)
    (let ((func (mew-draft-on-value-p mew-field-completion-switch)))
      (if func 
	  (funcall (cdr func))
	(tab-to-tab-stop))))) ;; default keybinding

(defun mew-complete-field ()
  "Field complete function."
  (interactive)
  (let ((word (mew-delete-key))) ;; capitalized
    (if (null word)
	(mew-complete-window-show mew-fields)
      (mew-complete
       word
       (mapcar (function (lambda (x) (list (concat (capitalize x) " "))))
	       mew-fields)
       "field"
       nil))))

(defun mew-complete-folder ()
  "Folder complete function for Fcc:."
  (interactive)
  (let ((word (mew-delete-backward-char)))
    (if (null word)
	(if mew-use-imap
	    (mew-complete-window-show (list "+" "=" "%"))
	  (mew-complete-window-show (list "+" "=")))
      (mew-complete word mew-folder-alist "folder" nil))))

(defun mew-complete-address ()
  "Complete and expand an address short name.
First alias key is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
  (interactive)
  (let ((word (mew-delete-backward-char)))
    (if (null word)
	(tab-to-tab-stop)
      (if (string-match "@." word)
	  (mew-complete-insert (or (mew-alias-next word) word))
	(mew-complete
	 word mew-addrbook-alist "alias" ?@ nil nil
	 (function mew-addrbook-alias-get) 
	 (function mew-addrbook-alias-hit))))))

(defun mew-complete-config ()
  "Complete function for Config:."
  (interactive)
  (let ((word (or (mew-delete-value ",") "")))
    (mew-complete
     word
     (mew-slide-pair mew-config-list)
     "mew-config-list"
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular completion: C-cC-i
;;;

(defun mew-draft-circular-comp ()
  "Switch function for circular complete functions."
  (interactive)
  (let ((func (mew-draft-on-value-p mew-field-circular-completion-switch)))
    (if func
	(funcall (cdr func))
      (message "No circular completion here"))))

(defun mew-circular-complete-domain ()
  "Circular completion of domains for To:, Cc:, etc.
If the @ character does not exist, the first value of
mew-mail-domain-list is inserted. If exists, the next value of 
mew-mail-domain-list concerned with the string between @ and 
the cursor is inserted."
  (interactive)
  (let ((word (mew-delete-backward-char "@"))
	(completion-ignore-case t))
    (cond
     ((equal word nil) ;; @ doesn't exist.
      (if (null mew-mail-domain-list)
	  (message "For domain circular completion, set mew-mail-domain-list")
	(mew-complete-insert "@")
	(mew-complete-insert (car mew-mail-domain-list))
	(mew-complete-window-delete)))
     ((equal word t) ;; just after @
      (if (null mew-mail-domain-list)
	  (message "For domain circular completion, set mew-mail-domain-list")
	(mew-complete-insert (car mew-mail-domain-list))
	(mew-complete-window-delete)))
     (t
      ;; can't use mew-get-next since completion is necessary sometime.
      (mew-complete
       word
       (mew-slide-pair mew-mail-domain-list)
       "domain"
       t)) ;; use cdr
     )))

(defun mew-circular-complete (msg clist cname &optional here)
  "General circular complete function to call mew-complete."
  (interactive)
  (let ((str (mew-delete-value here)))
    (if (null str)
	(if (car clist)
	    (mew-complete-insert (car clist))
	  (message "For circular completion, set %s" cname))
      (mew-complete
       str
       (mew-slide-pair clist)
       msg
       t)))) ;; use cdr

(defun mew-circular-complete-from ()
  "Circular complete function for From:."
  (interactive)
  (mew-circular-complete "from" mew-from-list "mew-from-list"))

(defun mew-circular-complete-config ()
  "Circular complete function for Config:."
  (interactive)
  (mew-circular-complete "config" mew-config-list "mew-config-list" ","))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion : M-C-i
;;;

(defun mew-draft-expand ()
  "Switch function for expand functions."
  (interactive)
  (let ((func (mew-draft-on-value-p mew-field-expansion-switch)))
    (if func
	(funcall (cdr func))
      (message "No expansion here"))))

(defun mew-expand-address ()
  "Address expansion function for To:, Cc:, etc.
'user@domain' will be expands 'name <user@domain>' if
the name exists."
  (interactive)
  (let ((word (mew-delete-backward-char)) func name)
    (if (null word)
	(message "No address here")
      (setq func (mew-addrbook-func mew-addrbook-for-address-expansion))
      (if (null func)
	  (mew-complete-insert word)
	(setq name (funcall func word))
	(mew-complete-insert (if name (format "%s <%s>" name word) word))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other completion stuff
;;;

;; dummy
(defvar mew-ext-host "")
(defvar mew-ext-user "")

(defun mew-complete-rfile ()
  "Complete a remote file."
  (interactive)
  (let* ((path-file (mew-delete-file))
	 (path (car path-file))
	 (file (cdr path-file))
	 rpath)
    (setq rpath (format "/%s@%s:%s" mew-ext-user mew-ext-host path))
    (mew-complete
     file
     rpath
     "remote file"
     nil
     (function mew-ext-file-name-completion)
     (function mew-ext-file-name-all-completions))))

(defun mew-complete-pick-pattern ()
  "Complete pick patterns."
  (interactive)
  (let* ((pat (mew-delete-pattern))
	 (clist (append '("(" "!")
			mew-pick-field-list
			(mapcar (function car) mew-pick-macro-alist))))
    (if (null pat)
	(mew-complete-window-show clist)
      (mew-complete
       pat
       (mapcar (function list) clist)
       "pick pattern"
       nil))))

(defun mew-complete-sort-key ()
  "Complete sort keys."
  (interactive)
  (let* ((word (mew-delete-line))
	 field alist)
    (if (string-match ":" word)
	(progn
	  ;; If WORD contains ':', change alist for completion.
	  (setq field (car (mew-split word ?:)))
	  (setq alist 
		(mapcar (function (lambda (str) (list (concat field ":" str))))
			mew-sort-modes)))
      ;; Otherwise, alist is mew-sort-key-alist itself.
      (setq alist mew-sort-key-alist))
    (mew-complete word alist "sort key" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hart function for completions
;;;

(fset 'mew-complete-hit (symbol-function 'assoc))

(defun mew-complete-get (key alist)
  (cdr (mew-complete-hit key alist)))

(defun mew-complete (WORD ALIST MSG EXPAND-CHAR &optional TRY ALL GET HIT)
  (let* ((ftry (or TRY (function try-completion)))
	 (fall (or ALL (function all-completions)))
	 (fget (or GET (function mew-complete-get)))
	 (fhit (or HIT (function mew-complete-hit)))
	 (cmp (funcall ftry WORD ALIST))
	 (all (funcall fall WORD ALIST))
	 (len (length WORD))
	 subkey)
    (cond
     ;; already completed
     ((eq cmp t)
      (if EXPAND-CHAR ;; may be "t"
	  (mew-complete-insert (funcall fget WORD ALIST)) ;; use cdr
	(mew-complete-insert WORD)) ;; use car
      (mew-complete-window-delete))
     ;; EXPAND
     ((and (mew-characterp EXPAND-CHAR)
	   (char-equal (aref WORD (1- len)) EXPAND-CHAR)
	   (setq subkey (substring WORD 0 (1- len)))
	   (funcall fhit subkey ALIST))
      (mew-complete-insert (funcall fget subkey ALIST)) ;; use cdr
      (mew-complete-window-delete))
     ;; just one candidate
     ((equal 1 (length all))
      (mew-complete-insert cmp)
      (mew-complete-window-delete)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message " [Sole completion]")
	(message "Sole completion")))
     ;; two or more candidates
     ((stringp cmp) ;; (length all) > 1
      (mew-complete-insert cmp)
      (mew-complete-window-show all)
      (if (and EXPAND-CHAR (funcall fhit cmp ALIST))
	  (message
	   (substitute-command-keys
	    "To expand '%s', type '%c' then '\\<mew-draft-header-map>\\[mew-draft-header-comp]'.")
	   cmp EXPAND-CHAR)))
     ;; no candidate
     (t
      (mew-complete-insert WORD)
      ;;(mew-complete-window-delete)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message (concat " No matching " MSG))
	(message "No matching %s" MSG))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuf magic
;;;

(defun mew-temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (mew-complete-insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      (if quit-flag (setq quit-flag nil	unread-command-events 7)))))

;;
;; Extracting completion key
;;

(defun mew-delete-backward-char (&optional here)
  "Delete appropriate preceding word and return it."
  (interactive)
  (let ((case-fold-search t)
        (start nil)
        (end (point))
        (regex (concat "[^" mew-address-separator "]")))
    (save-excursion
      (while (and (not (bobp))
                  (string-match regex (mew-buffer-substring
                                       (1- (point)) (point))))
        (forward-char -1))
      (if (and here (not (re-search-forward (regexp-quote here) end t)))
          nil ;; "here" doesn't exist.
          (setq start (point))
          (if (= start end)
              (if here t nil) ;; just after "here",  just after separator
            (prog1
                (mew-buffer-substring start end)
              (delete-region start end)))))))

(defun mew-delete-file ()
  (if (search-backward mew-path-separator nil t)
      (forward-char 1)
    (beginning-of-line))
  (prog1
      (cons (mew-buffer-substring (point-min) (point))
	    (mew-buffer-substring (point) (point-max)))
    (delete-region (point) (point-max))))

(defun mew-delete-pattern ()
  (let ((pos (point)))
    (if (re-search-backward " \\|(\\|&\\||\\|!" nil t)
	(forward-char 1)
      (beginning-of-line))
    (prog1
	(mew-buffer-substring (point) pos)
      (delete-region (point) pos))))

(defun mew-delete-line ()
  (let ((pos (point)))
    (beginning-of-line)
    (prog1
	(mew-buffer-substring (point) pos)
      (delete-region (point) pos))))

(defun mew-delete-key ()
  (let ((pos (point)))
    (beginning-of-line)
    (prog1
	(capitalize (mew-buffer-substring (point) pos))
      (delete-region (point) pos))))

(defun mew-delete-value (&optional here)
  (beginning-of-line)
  (if (not (looking-at "[^:]+:"))
      ()
    (goto-char (match-end 0))
    (if (looking-at "[ \t]")
	(forward-char 1)
      (mew-complete-insert " "))
    (if (eolp)
	nil
      (let ((start (point)) ret)
	(end-of-line)
	(if (and here (re-search-backward (regexp-quote here) start t))
	    (progn
	      (setq start (1+ (point)))
	      (end-of-line)))
	(setq ret (mew-buffer-substring start (point)))
	(delete-region start (point))
	ret))))

;;
;; Making alist
;;

(defun mew-slide-pair (x)
  (let ((ret nil)
	(first (car x)))
    (cond 
     ((eq x 0) nil)
     ((eq x 1) (cons first first))
     (t
      (while (cdr x)
	(setq ret (cons (cons (nth 0 x) (nth 1 x)) ret))
	(setq x (cdr x)))
      (setq ret (cons (cons (car x) first) ret))
      (nreverse ret)))))

(provide 'mew-complete)

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

;;; mew-complete.el ends here
