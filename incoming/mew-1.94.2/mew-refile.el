;;; mew-refile.el --- Refile for Mew

;; Author:  Yoshinari NOMURA <nom@csce.kyushu-u.ac.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 11, 1994
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-refile-version "mew-refile.el version 0.76")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-refile-msgid-alist nil
  "Alist of message-id and folder pair")
(defvar mew-refile-msgid-file-name ".mew-refile-msgid-alist")
(defvar mew-refile-from-alist nil
  "Alist of From: address and folder pair")
(defvar mew-refile-from-file-name ".mew-refile-from-alist")

(defvar mew-refile-last-folder nil
  "Folder name previously you refiled")

(defvar mew-refile-ctrl-multi t
  "*If *non-nil*, guess functions guess multi folders.")

(defvar mew-refile-guess-alist nil
  "*If non-nil, mew guesses destination folder by using this hint.
The format is like this:

    (setq mew-refile-guess-alist
          '((\"To:\" 
              (\"wide@wide\" . \"+wide/wide\")
              (\"adam\"      . \"+labo/adam\"))
            (\"Newsgroups:\"
              (\"^nifty\\\\.\\\\([^ ]+\\\\)\" . \"+Nifty/\\\\1\"))
            (\"From:\" 
              (\"uucp\" . \"+adm/uucp\")
              (\".*\"   . \"+misc\"))
            ))
")

(defvar mew-refile-guess-key-list mew-destination:-list
  "*A list of field key used by mew-refile-guess-by-folder.")

(defvar mew-refile-guess-control
  '(mew-refile-guess-by-alist
    mew-refile-ctrl-throw
    mew-refile-guess-by-newsgroups
    mew-refile-guess-by-folder
    mew-refile-ctrl-throw
    mew-refile-ctrl-auto-boundary
    mew-refile-guess-by-thread
    mew-refile-ctrl-throw
    mew-refile-guess-by-from-folder
    mew-refile-ctrl-throw
    mew-refile-guess-by-from
    mew-refile-ctrl-throw
    mew-refile-guess-by-default))

(defvar mew-refile-auto-refile-skip-any-mark nil
  "*If *non-nil*, 'mew-summary-auto-refile' doesn't touch
any alredy marked message.")

(defvar mew-refile-auto-refile-confirm nil
  "*If *non-nil*, 'mew-summary-auto-refile' prompts the user for
confirmation before refiling.")

(defvar mew-refile-guess-strip-domainpart t
  "*If *non-nil*, 'mew-refile-guess-by-default' strips domainpart of from")

(defvar mew-refile-guess-from-me-is-special nil
  "*If *non-nil*, 'mew-refile-guess-by-from-*' think the mails from yourself
as special. They use To: or Cc: instead of From:")

;;
;; initialize function
;;
(defun mew-refile-init ()
  ;; load message id alist
  (or mew-refile-msgid-alist
      (setq mew-refile-msgid-alist (mew-lisp-load mew-refile-msgid-file-name)))
  ;; load from alist
  (or mew-refile-from-alist
      (setq mew-refile-from-alist (mew-lisp-load mew-refile-from-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess functions 
;;;

;; We have two types of functions in mew-refile-guess-control,
;; guess function and ctrl function.
;; guess function must return a folder list or folder string or nil.
;; guess function must not have a string "ctrl" in its symbol name.
;; ctrl function must have a string "ctrl" in its symbol name.

;; dispatcher returns: ((guess1 guess2 ..) info1 info2 ...) multi  guess mode
;;                     ((guess1)           info1 info2 ...) single guess mode
;;            info1:   ('guess-func-name guess1 guess2 ...)
;;
;; that is, 'car' is a list of judged  folders.
;;          'cdr' is a alist of opinions by guess functions.
;;
(defun mew-refile-guess (&optional auto show-all)
  (let ((funcs mew-refile-guess-control) ret guess info stop)
    (catch 'last
      (while funcs
	(cond
	 ;; func is control function
	 ((string-match "ctrl" (symbol-name (car funcs)))
	  (if (setq ret (funcall (car funcs) guess auto))
	      (progn
		(setq stop t)
		(or show-all (throw 'last t)))))
	 ;; func is guess function
	 (t
	  (setq ret (funcall (car funcs)))))
	;; now, ret is the return value from a function.
	;; make sure ret is a list.
	(setq ret (or (and (listp ret) ret) (and ret (list ret))))
	;; shape the guess: (guess1 guess2 ...)
	;;            info: (info1 info2 ...)
	(setq info (nconc info (list (cons (car funcs) ret))))
	(if (not stop) (setq guess (nconc (reverse ret) guess)))
	(setq funcs (cdr funcs))))
    (setq guess (nreverse guess))
    (if mew-refile-ctrl-multi
	(cons (mew-uniq-list guess) info)
      (cons (list (car guess)) info))))


;;
;; guess control functions
;;
(defun mew-refile-ctrl-auto-boundary (guess auto)
  (if auto 'stop))

(defun mew-refile-ctrl-throw (guess auto)
  (if guess 'stop))

;;
;; by alist returns: (guess1 guess2 ...) or nil
;;
(defun mew-refile-guess-by-alist ()
  (mew-refile-guess-by-alist1 mew-refile-guess-alist))

(defun mew-refile-guess-by-alist1 (alist)
  (let (name header sublist key val ent ret)
    (while alist
      (setq name (car (car alist)))
      (setq sublist (cdr (car alist)))
      (cond
       ((eq name t)
	(setq ret (cons sublist ret)))
       ((eq name nil)
	(or ret (setq ret (cons sublist ret))))
       (t
	(setq header (mew-header-get-value name))
	(if header
	    (while sublist
	      (setq key (car (car sublist)))
	      (setq val (cdr (car sublist)))
	      (if (and (stringp key) (string-match key header))
		  (cond
		   ((stringp val)
		    (setq ent (mew-refile-guess-by-alist2 key header val)))
		   ((listp val)
		    (setq ent (mew-refile-guess-by-alist1 val)))))
	      (if ent
		  (if (listp ent)
		      (setq ret (nconc ent ret) ent nil)
		    (setq ret (cons ent ret))))
	      (setq sublist (cdr sublist))))))
      (setq alist (cdr alist)))
    (mew-uniq-list (nreverse ret))))

(defun mew-refile-guess-by-alist2 (regexp field string)
  (let (match-strings match-list)
    (string-match regexp field)
    (setq match-list (cdr (cdr (match-data))))
    (while (car match-list)
      (setq match-strings
	    (cons (substring field
			     (car match-list) (car (cdr match-list)))
		  match-strings))
      (setq match-list (cdr (cdr match-list))))
    (while (string-match "\\\\\\([1-9]\\)" string)
	(setq string
	      (concat (substring string 0 (match-beginning 0))
		      (nth (- (length match-strings)
			      (string-to-int 
			       (substring string
					  (match-beginning 1)
					  (match-end 1))))
			   match-strings)
		      (substring string (match-end 0)))))
    string))

;;
;; by newsgroups returns (guess1 guess2 ...) or nil
;;
(defun mew-refile-guess-by-newsgroups ()
  (let ((newsgroups (mew-addrstr-parse-value-list2 
		     (mew-header-get-value mew-newsgroups:)))
	ent ret)
    (if (not newsgroups)
	()
      (while newsgroups
	(setq ent (mew-assoc-case-equal (car newsgroups) mew-folder-alist 1))
	(if ent (setq ret (cons (nth 0 ent) ret)))
	(setq newsgroups (cdr newsgroups)))
      (mew-uniq-list (nreverse ret)))))

;;
;; by folder returns: (guess1 guess2 ...) or nil
;;
(defun mew-refile-guess-by-folder ()
  (let ((to-cc (mew-header-parse-address-list mew-refile-guess-key-list))
	ent ret ml-name)
    (while to-cc
      (setq ml-name (mew-addrstr-extract-user (or (car to-cc) "")))
      (setq ent (mew-assoc-case-equal ml-name mew-folder-alist 1))
      (if ent (setq ret (cons (nth 0 ent) ret)))
      (setq to-cc (cdr to-cc)))
    (mew-uniq-list (nreverse ret))))

;;
;; by message-thread returns: guess1 or nil
;;
(defun mew-refile-guess-by-thread ()
  (let ((msgid (or (mew-header-get-value mew-references:)
		   (mew-header-get-value mew-in-reply-to:))))
    ;; search for msgid
    (if (and msgid 
	     (string-match "\\(<[^ \t\n]*>\\)[^>]*\0" (concat msgid "\0")))
	(nth 1 (assoc (substring msgid 
				 (match-beginning 1)
				 (match-end 1))
		      mew-refile-msgid-alist)))))

;;
;; by from-{folder,alist} returns: guess1 or (guess1 ...) or nil
;;
(defun mew-refile-guess-by-from-folder (&optional addr)
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-folder-body addr))

(defun mew-refile-guess-by-from (&optional addr)
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-body addr))

(defun mew-refile-guess-by-from-folder-body (&optional addr)
  (let* ((pfix (cond ((equal mew-folders-default-folder "" ) "+")
		     ((equal mew-folders-default-folder nil) "+")
		     ((equal mew-folders-default-folder "+") "+")
		     (t (file-name-as-directory mew-folders-default-folder))))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) "")))
	 (user (mew-addrstr-extract-user from))
	 (pfix-regex (concat "^" (regexp-quote pfix) "\\(.*/\\)?"))
	 (from-regex (concat pfix-regex (regexp-quote from) "/?$"))
	 (user-regex (concat pfix-regex (regexp-quote user) "/?$")))
    (or
     (mew-refile-match-in-list from-regex mew-folder-list)
     (mew-refile-match-in-list user-regex mew-folder-list))))

(defun mew-refile-guess-by-from-body (&optional addr)
  (let ((from   (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    (cdr (assoc from mew-refile-from-alist))))

;;
;; dispatcher to make mew-refile-guess-by-from-* consider
;; mew-refile-guess-from-me-is-special.
;;
(defun mew-refile-guess-from-dispatch (func &optional addr)
  (let ((addr (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    ;; if From: is my address, addr is the list extracted from To:, Cc:
    (if (and mew-refile-guess-from-me-is-special
	     (mew-is-my-address (mew-get-my-address-regex-list) addr))
	(let ((addr (mew-header-parse-address-list mew-refile-guess-key-list))
	      (a nil) (r nil))
	  (while addr
	    (if (setq a (funcall func (car addr)))
		(setq r (cons a r)))
	    (setq addr (cdr addr)))
	  (mew-uniq-list (nreverse r)))
      (funcall func addr))))

;;
;; by default returns: guess1
;;
(defun mew-refile-guess-by-default (&optional addr)
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-default-body addr))

(defun mew-refile-guess-by-default-body (&optional addr)
  (let* ((pfix (cond ((equal mew-folders-default-folder "" ) "+")
		     ((equal mew-folders-default-folder nil) "+")
		     ((equal mew-folders-default-folder "+") "+")
		     (t (file-name-as-directory mew-folders-default-folder))))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) "")))
	 (user (mew-addrstr-extract-user from)))
    (if mew-refile-guess-strip-domainpart
	(concat pfix user)
      (concat pfix from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Learning functions
;;;

;; dispatcher
;;
;; mew-refile-guess-learn (buf result)
;;
;; buf is message buffer.
;;
;; result is ((chosen1 chosen2 ...)
;;           (guess-func-name1 guess1 guess2...)
;;           (guess-func-name2 guess1 guess2...))
;;
;; that is, 'car' is a list of user chosen folders.
;;          'cdr' is a list of opinions by guess functions.
;;
(defun mew-refile-guess-learn (buf result)
  (let ((chosen (car result))  ;; (folder1 folder2 ...)
	(info   (cdr result))) ;; (guess-func-name guess1 guess2...)
    (save-excursion
      (set-buffer buf)
      (if (member 'mew-refile-guess-by-from mew-refile-guess-control)
	  (mew-refile-guess-by-from-learn chosen info))
      (if (member 'mew-refile-guess-by-thread mew-refile-guess-control)
	  (mew-refile-guess-by-thread-learn chosen info)))))
;;
;; learn from msgid
;;
(defun mew-refile-guess-by-thread-learn (chosen info)
  (let* ((msgid  (mew-header-get-value mew-message-id:)) 
	 (folder (car chosen))
	 ;; ohter people's honest opinion and my honest opinion.
	 (oho    info)
	 (mho    (cdr (assoc 'mew-refile-guess-by-thread info))))
    (if (and msgid (string-match "<[^ \n>]*>" msgid))
	(setq msgid (substring msgid (match-beginning 0) (match-end 0))))
    (if (or (not msgid) (not chosen))
	()
      ;; if my opninion was right, I learn it.
      ;; or a folder was not in other people's opinion,
      ;; I accept it.
      (catch 'match
	(while chosen
	  (if (or (member (car chosen) mho)
		  (not (catch 'find
		    (while oho
		      (and (member (car chosen) (car oho)) (throw 'find t))
		      (setq oho (cdr oho))))))
	      (throw 'match (setq folder (car chosen))))
	  (setq chosen (cdr chosen))))
      (setq mew-refile-msgid-alist
	    (cons (list msgid folder "??")
		  (delete (assoc msgid mew-refile-msgid-alist)
			  mew-refile-msgid-alist))))))
;;
;; learn from "From:" field
;;
(defun mew-refile-guess-by-from-learn (chosen info)
  (let* ((from  (downcase (or (mew-header-parse-address mew-from:) "")))
	 (folder nil)
	 ;; 'my honest opinion' guessed by mew-refile-guess-by-from.
	 (mho    (nth 1 (assoc 'mew-refile-guess-by-from       info))))

    (if (or (or (null from) (null chosen)) ;; if from and/or chosen is empty
	    (and mho (member mho chosen))) ;; or my opinion was right,
	()                                 ;; do nothing.

      ;; I decide which folder is most important among the user chosen
      ;; folders. 
      (catch 'match
	(while chosen
	  ;; searching a folder anyone couldn't predict.
	  (if (not (mew-refile-guess-member-p (car chosen) info))
	      (throw 'match (setq folder (car chosen))))
	  (setq chosen (cdr chosen))))

      ;; If candidate was found, I memorize it.
      (if folder
	  (setq mew-refile-from-alist
		(cons (cons from folder)
		      (delete (assoc from mew-refile-from-alist)
			      mew-refile-from-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backyard functions.
;;;

(defun mew-refile-guess-save ()
  (if (and mew-refile-from-alist
	   (member 'mew-refile-guess-by-from mew-refile-guess-control))
      (mew-lisp-save mew-refile-from-file-name mew-refile-from-alist))
  (if (and mew-refile-msgid-alist
	   (member 'mew-refile-guess-by-thread mew-refile-guess-control))
      (mew-lisp-save mew-refile-msgid-file-name mew-refile-msgid-alist)))

(defun mew-refile-guess-subfolder-p (parent child)
  (string-match 
   (concat  "^" (regexp-quote (file-name-as-directory parent)))
   child))

;;
;; search x in a nested list.
;;
(defun mew-refile-guess-member-p (x lst)
  (catch 'found
    (while lst
      (cond 
       ((listp (car lst))
	(if (mew-refile-guess-member-p x (car lst))
	    (throw 'found t)))
       (t
	(if (equal  x (car lst))
	    (throw 'found t))))
      (setq lst (cdr lst)))))

;;
;; grep with regex in a list of string. (first match)
;;
(defun mew-refile-match-in-list (regex lst)
  (let ((case-fold-search t))
    (catch 'found 
      (while lst
	(if (string-match regex (car lst))
	    (throw 'found (car lst)))
	(setq lst (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;
(defun mew-refile-set (msg folder)
  (let* ((msg-folders (assoc msg mew-summary-buffer-refile))
	 (folders (cdr msg-folders)))
    (if folders
	(if (not (mew-folder-member folder folders))
	    (setq mew-summary-buffer-refile 
		  (cons (nreverse (cons folder (nreverse (cons msg folders))))
			(delete msg-folders mew-summary-buffer-refile))))
      (setq mew-summary-buffer-refile 
	    (cons (list msg folder) mew-summary-buffer-refile)))))

(defun mew-refile-reset (msg)
  (setq mew-summary-buffer-refile
	(delete (assoc msg mew-summary-buffer-refile)
		mew-summary-buffer-refile)))

;; mew-refile-decide-folders returns: ((input1 input2...) 
;;                                     (guess-func-name1 guess1 guess2...)
;;                                     (guess-func-name2 guess1 guess2...))
;; that is, 'car' is a list of user chosen folders.
;;          'cdr' is a alist of opinions by guess functions.
;; cdr is needed for learning functions.
;;
(defun mew-refile-decide-folders (buf msg mark &optional auto)
  (let (learn-info folders ret)
    (save-excursion
      (set-buffer buf)
      (setq learn-info (mew-refile-guess auto)))
    (setq 
     folders
     (cond
      ;; if auto is set, simply use the guess.
      (auto (car learn-info))
      ;; add new folder 
      ((equal mew-mark-refile mark)
       (mew-input-folders 
	nil (mew-join "," 
		      (cdr (assoc msg mew-summary-buffer-refile)))))
      ;; multi guess
      ((nth 1 (car learn-info))
       (mew-input-folders nil (mew-join "," (car learn-info))))
      ;; single guess
      (t 
       (mew-input-folders (nth 0 (car learn-info))))))
    ;; check folder existence.
    (while folders
      (if (mew-folder-check (car folders))
	  (setq ret (cons (car folders) ret)))
      (setq folders (cdr folders)))
    (cons (nreverse ret) (cdr learn-info)))) ;; return value

(defvar mew-header-reasonable-size 5000)

(defun mew-summary-refile (&optional report)
 "Put the refile mark(default is 'o') on this message. If already
marked with 'o', it prints where this message will be refiled. This
can overlay other marks. When it overlays, the cursor stays on the
message. If it marks newly, displays the next message. If executed
with '\\[universal-argument]', it displays how the refile rules work in Message mode."
 (interactive "P")
 (if report (mew-summary-refile-report) (mew-summary-refile-body)))

(defun mew-summary-refile-body (&optional exp-flds auto)
  (mew-summary-only
   (mew-summary-msg-or-part
    (let (msg folders mark buf learn-info fld)
      (save-excursion
	;; save the cursor position anyway
	(mew-summary-goto-message)
	;; on the message
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number)) ;; msg is never nil
	(setq mark (mew-summary-get-mark))) ;; any mark
      (if exp-flds
	  (setq folders exp-flds)
	;; show message if not displayed
	(if (or auto (null mew-summary-buffer-disp-msg))
	    (save-excursion
	      (mew-set-buffer-tmp)
	      (setq buf (current-buffer))
	      (mew-insert-message fld msg mew-cs-autoconv
				  mew-header-reasonable-size)
	      (goto-char (point-min))
	      (if (and (re-search-forward (concat "^$\\|^" mew-subj:) nil t)
		       (not (looking-at "^$")))
		  (let ((med (point)))
		    (forward-line)
		    (mew-header-goto-next)
		    (mew-header-decode-region 'text med (point)))))
	  (mew-summary-display nil) ;; cursor position is important
	  (setq buf (or (mew-cache-hit (cons (buffer-name) msg))
			(mew-buffer-message)))) ;; for ","
	(setq learn-info (mew-refile-decide-folders buf msg mark auto))
	(setq folders (car learn-info)))
      ;; mark refile
      (if folders
	  (save-excursion
	    (mew-summary-goto-message)
	    (or exp-flds auto (mew-refile-guess-learn buf learn-info))
	    (mew-refile-reset msg)
	    (mapcar (function (lambda (x) (mew-refile-set msg x))) folders)
	    (mew-mark-unmark)
	    (mew-summary-mark-as mew-mark-refile)))
      ;; memorize last-folder
      (setq mew-refile-last-folder folders)
      (if (or mark auto (not folders))
	  () ;; stay here
	(mew-summary-goto-message)
	;; on the message
	(mew-decode-syntax-delete)
	;; for C-x C-x
	(beginning-of-line)
	(let ((zmacs-regions nil))
	  (push-mark (point) t t))
	(mew-summary-display-after mew-summary-mark-direction))
      (set-buffer-modified-p nil)
      folders)))) ;; return value

(defun mew-summary-refile-report ()
  (mew-summary-only
   (mew-summary-msg-or-part
    (let (fld msg guess buf
	      (customize-var '(mew-refile-ctrl-multi
			       mew-refile-guess-key-list
			       mew-refile-guess-strip-domainpart
			       mew-refile-guess-from-me-is-special)))
      (save-excursion
	;; save the cursor position anyway
	(mew-summary-goto-message)
	;; on the message
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number)) ;; msg is never nil
	(mew-set-buffer-tmp)
	(mew-insert-message fld msg mew-cs-autoconv
			    mew-header-reasonable-size)
	(setq guess (mew-refile-guess nil t)))
      (setq buf (buffer-name))
      (mew-window-configure buf 'message)
      (mew-elet
       (mew-erase-buffer)
       (save-excursion
	 ;; report result of guess.
	 (insert (format "** Guess result: %s\n" (car guess)))
	 ;; report status of customize variables.
	 (insert "\n** Current Configurations:\n\n")
	 (while customize-var
	   (insert (format "%-40s:  " (car customize-var)))
	   (insert (format "%s\n"     (eval (car customize-var))))
	   (setq customize-var (cdr customize-var)))
	 (insert "\n** Each function's opinion:\n\n")
	 ;; report how each functions guessed.
	 (setq guess (cdr guess))
	 (while guess
	   (insert (format "%-32s  " (car (car guess))))
	   (insert (format "return: %s\n" (cdr (car guess))))
	   (setq guess (cdr guess)))))
      (mew-message-clear-end-of)
      (set-buffer-modified-p nil)
      (mew-pop-to-buffer buf)))))

(defun mew-summary-refile-again ()
  "Put a refile mark on this message according to the previous 
refile folder."
  (interactive)
  (mew-summary-only
   (mew-summary-refile-body mew-refile-last-folder)))

(defun mew-summary-auto-refile (&optional mew-mark-review-only)
  "Refile each message in the folder automatically. If 
'mew-refile-auto-refile-skip-any-mark' is non-nil,
any previousely marked message will be skipped.
If '\\[universal-argument]' is specified, only messages marked with
'mew-mark-review' will be conserned."
  (interactive "P")
  (mew-summary-only
   (let ((after-change-function nil)
	 (after-change-functions nil)
	 (mew-use-highlight-x-face nil)
	 (lines (count-lines (point-min) (point-max)))
	 (line 1) (mark nil) msg)
     (cond
      (mew-mark-review-only
       (setq msg (format "Refile all messages marked with '%c'? "
			 mew-mark-review)))
      (mew-refile-auto-refile-skip-any-mark
       (setq msg (format "Refile all non-marked messages? ")))
      (t
       (setq msg (format "Refile all messages except those marked with '%c' and '%c'? " mew-mark-refile mew-mark-delete))))
     (if (and mew-refile-auto-refile-confirm (null (yes-or-no-p msg)))
	 (message "Not refiled.")
       (message "Auto refiling ...")
       (save-window-excursion
	 (goto-char (point-min))
	 (while (not (eobp))
	   (setq mark (mew-summary-get-mark))
	   (if mew-mark-review-only 
	       (and (equal mark mew-mark-review)
		    (mew-summary-refile-body nil t))
	     (or (equal mark mew-mark-refile)
		 (equal mark mew-mark-delete)
		 (and mark mew-refile-auto-refile-skip-any-mark)
		 (mew-summary-refile-body nil t)))
	   (forward-line)
	   (if (equal (% (/ (* 100 line) lines) 10) 0)
	       (message "Auto refiling ... %s%%"
			(/ (* 100 line) lines)))
	   (setq line (1+ line)))
	 (message "Auto refiling ... done"))))))

;;
;; "mx" extension
;;
(defun mew-summary-mark-refile ()
  "\\<mew-summary-mode-map>
Put the refile mark onto all messages marked with '*'.
This is very convenient to refile all messages picked by '\\[mew-summary-search-mark]'."
  (interactive)
  (mew-summary-only
   (let ((after-change-function nil)
	 (after-change-functions nil)
	 (mew-use-highlight-x-face nil)
	 (last nil)
	 (regex (concat mew-summary-message-regex
			(regexp-quote (char-to-string mew-mark-review)))))
     (message "Mark refiling ...")
     (save-excursion
       (save-window-excursion
	 (goto-char (point-min))
	 (while (re-search-forward regex nil t)
	   (setq last (mew-summary-refile-body last))
	   (forward-line))
	 (message "Mark refiling ... done"))))))

(provide 'mew-refile)

;;; Copyright Notice:

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-refile.el ends here
