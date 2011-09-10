;;; mew-pick.el --- Picking up messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-pick-version "mew-pick.el version 0.17")

(require 'mew)

(defvar mew-pick-macro-alist nil)

(defun mew-summary-search ()
  "Pick messages according to a pick pattern which you input, 
then list them up."
  (interactive)
  (mew-summary-only
   (let ((folder (mew-input-folder (buffer-name)))
	 (pattern nil)
	 (range nil))
     (if (null (file-directory-p (mew-expand-folder folder)))
	 (message "No such folder %s" folder)
       (setq pattern (mew-input-pick-pattern))
       (message "Picking messages in %s ..." folder)
       (setq range (mew-summary-pick folder pattern))
       (message "Picking messages in %s ... done" folder)
       (if (get-buffer folder)
	   (switch-to-buffer folder)
	 (mew-summary-folder-create folder))
       (if range (mew-summary-scan-body mew-prog-imls
					'mew-summary-mode
					folder
					mew-cs-scan
					(list range 'erase)))))))

(defun mew-summary-search-mark (&optional arg)
  "Pick messages according to a pick pattern which you input, 
then put the '*' mark onto them. If called with \"\\[universal-argument]\", target is
the messages in the region."
  (interactive "P")
  (mew-summary-only
   (if arg
       (mew-summary-search-mark-region (region-beginning) (region-end))
     (mew-summary-search-mark-region (point-min) (point-max)))))
  
(defun mew-summary-search-mark-region (r1 r2)
  (interactive "r")
  (if (equal (point-min) (point-max))
      (message "No messages in this buffer.")
    (let ((folder (buffer-name))
	  pattern first last range)
      (setq pattern (mew-input-pick-pattern))
      (message "Picking messages in %s ..." folder)
      (goto-char r1)
      (if (eobp)
	  () ;; r1 <= r2, so if r1 = (point-max) then no messages.
	(setq first (mew-summary-message-number))
	(goto-char r2)
	(if (eobp)
	    (progn
	      (forward-line -1)
	      (setq r2 (point))))
	(setq last (mew-summary-message-number))
	(setq range (mew-summary-pick folder pattern (concat first "-" last))))
      (message "Picking messages in %s ... done" folder)
      (if (null range)
	  (message "No message to be marked.")
	(message "Marking messages ... ")
	(goto-char r1)
	(while (and range (< (point) r2))
	  (if (re-search-forward (format "^[ ]*%s[^0-9]" (car range)) nil t)
	      (if (not (mew-summary-marked-p))
		  (mew-summary-mark-as mew-mark-review)))
	  (setq range (cdr range)))
	(beginning-of-line)
	(set-buffer-modified-p nil)
	(message "Marking messages ... done")))))

(defun mew-pick-define-macro (str1 str2)
  (interactive (list
		(read-string "pick pattern: ")
		(read-string "macro body: ")))
  ;; macro-pattern is a string including no #, or
  ;; a string in a form FIELD=#1 #2 #3...#n.
  ;; #1 can be replaced by #.
  (let ((args nil) (body nil))
    (while (string-match "\\(#[0-9]*\\)[, ]*" str1)
      (setq args (cons (intern (mew-match 1 str1)) args))
      (setq str1 (concat (substring str1 0 
				    (match-beginning 0))
			 (substring str1 
				    (match-end 0)))))
    (while (string-match "#[0-9]*" str2)
      (setq body 
	    (cons (substring str2 0 (match-beginning 0)) body))
      (setq body
	    (cons (intern (mew-match 0 str2)) body))
      (setq str2
	    (substring str2 (match-end 0))))
    (setq body (cons str2 body))
    (let ((asc (assoc str1 mew-pick-macro-alist))
	  (value (cons (nreverse args) (nreverse body))))
      (if asc
	  (setcdr asc value)
	(setq mew-pick-macro-alist
	  (cons (cons str1 value) mew-pick-macro-alist))))
    ))
    
(defun mew-pick-macro-expand (name args)
  (let ((asc (assoc name mew-pick-macro-alist)))
    (if (not asc)
	name
      (let ((alist nil)
	    (args2 (nth 1 asc))
	    (body (nthcdr 2 asc))
	    (body-copy nil))
	(while (and args args2)
	  (setq alist (cons (cons (car args2) (car args)) alist))
	  (setq args (cdr args))
	  (setq args2 (cdr args2))
	  )
	(while body
	  (if (stringp (car body))
	      (setq body-copy (cons (car body) body-copy))
	    (let ((assq (assq (car body) alist)))
	      (if assq
		  (setq body-copy (cons (cdr assq) body-copy)))))
	  (setq body (cdr body)))
	(concat "("
		(mew-pick-macro-expand-string
		 (apply 'concat (nreverse body-copy)))
		")")))))

(defun mew-pick-macro-expand-string (str)
  (if (string= str "") 
      ""
    (let ((first (string-to-char str))
	  (eq-flag nil))
      (if (memq first '(?\( ?\! ?\& ?\| ?= ? ?\)))
	  (concat (char-to-string first)
		  (mew-pick-macro-expand-string (substring str 1)))
	(let ((key nil) (rest nil))
	  (if (string-match "=\\| \\|)\\|&\\||" str)
	      (if (string= (mew-match 0 str) "=")
		  (progn
		    (setq eq-flag t)
		    (setq key (substring str 0 (match-end 0)))
		    (setq rest (substring str (match-end 0))))
		(setq key (substring str 0 (match-beginning 0)))
		(setq rest (substring str (match-beginning 0))))
	    (setq key str)
	    (setq rest ""))
	  (let ((asc (assoc key mew-pick-macro-alist)))
	    (cond (asc
		   (let ((args (nth 1 asc)) (vals nil))
		     (while args
		       (if (string-match ",\\| \\|)\\|&\\||" rest)
			   (progn
			     (setq vals
				   (cons 
				    (substring rest 0 
					       (match-beginning 0))
				    vals))
			     (setq rest
				   (substring rest
					      (match-beginning
					       0))))
			 (setq vals
			       (cons rest vals))
			 (setq rest ""))
		       (setq args (cdr args)))
		     (concat
		      (mew-pick-macro-expand key (nreverse vals))
		      (mew-pick-macro-expand-string rest))))
		  (eq-flag
		   (let ((val ""))
		     (if (string-match " \\|)\\|&\\||" rest)
			 (progn
			   (setq val (substring rest 0 (match-beginning 0)))
			   (setq rest (substring rest (match-beginning
						       0))))
		       (setq val rest)
		       (setq rest ""))
		     (concat key val 
			     (mew-pick-macro-expand-string rest))))
		  (t
		   (concat key 
			   (mew-pick-macro-expand-string rest))
		   ))))))))
	
(defun mew-pick-canonicalize-pattern (string)
  (let ((i 0))
    (while (string-match "[ \t]*\\([|&]+\\)[ \t]*" string i)
      (setq string (concat (substring string 0 (match-beginning 0))
			   " "
			   (mew-match 1 string)
			   " "
			   (substring string (match-end 0) nil)))
      (setq i (+ (match-beginning 0) 3)))
    string))

(defun mew-summary-pick (folder pattern &optional range)
  (let (arg msgs)
    (setq range (or range "all"))
    (save-excursion
      (mew-set-buffer-tmp)
      ;; input(result) from pick is line-based stream...
      (cond
       ((equal pattern mew-pick-duplicate-msgid)
	(setq arg "--dupchecktarget=message-id"))
       ((equal pattern mew-pick-duplicate-subj-msgid)
	(setq arg "--dupchecktarget=message-id+subject"))
       (t
	(setq arg (format "--expression=%s" pattern))))
      (mew-pioalet
       mew-cs-autoconv mew-cs-pick mew-cs-pick
       (mew-im-call-process nil mew-prog-imgrep
			    (format "--src=%s" folder) arg range))
      (goto-char (point-min))
      (if (search-forward "imgrep: no message" nil t)
	  (progn
	    (message "No such messages")
	    nil)
	(goto-char (point-min))
	(if (search-forward "imgrep: " nil t)
	    (progn
	      (message "Illegal pattern")
	      nil))
	(while (not (eobp))
	  (if (looking-at "^[0-9]+$")
	      (setq msgs (cons (mew-match 0) msgs)))
	  (forward-line))
	(nreverse msgs)))))

(provide 'mew-pick)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-pick.el ends here
