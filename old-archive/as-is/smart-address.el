;; smart-address.el
;; Command for picking up an address from a signature of a replied-to
;; message and replave the address in the To: field therewith.

;; John Robinson, BBN, 21 Jun 89

;; List of search patterns for extracting addresses from the
;; replied-to message.  Used in order until one hits.  Use grouping
;; operators to delimit what is actually the address.

(defvar smart-address-search-list
  (list 
   "domain\\W*\\([^ 	([{<]+@[^] 	\n)}>]+\\)"
   "internet\\W*\\([^ 	([{<]+@[^] 	\n)}>]+\\)"
   "arpanet\\W*\\([^ 	([{<]+@[^] 	\n)}>]+\\)"
   "bitnet\\W*\\([^ 	([{<]+@[^] 	\n)}>]+\\)"
   "\\([^ 	\n([{<]+@[^] 	\n)}>]+\\)"
   "\\([^ 	\n([{<]+![^] 	\n)}>]+\\)")
  "List of search patterns for extracting as address from a signature.
Lists are used in order until one matches.  The  \\\\( and \\\\) in
the first match surround the address to pick up.")

;; Per-buffer variable for getting to buffer for replied-to message

(defvar smart-address-other-buffer nil
  "Function which returns the buffer holding the message this is a reply to")

(make-variable-buffer-local 'smart-address-other-buffer)

;; Set things up for gnews replies

(defun gnews-msg-buffer ()
  mail-reply-buffer)

(setq group-reply-hook '(lambda nil
			  (fset 'smart-address-other-buffer 
				'gnews-msg-buffer)))

;; Set things up for mh-e replies

(defun mh-msg-buffer ()
  (and mh-sent-from-msg mh-sent-from-folder
       (progn
	 (set-buffer mh-sent-from-folder)
	 mh-show-buffer)))

(setq mh-letter-mode-hook '(lambda nil 
			     (fset 'smart-address-other-buffer
				   'mh-msg-buffer)))

(defun smart-address-yank ()
  "Replace addressee with address from from signature of replied-to message.
Replace entire To: field.
Buffer local variable smart-address-other-buffer holds function to return
the name of the other buffer."
  (interactive)
  (let ((old-buffer (current-buffer))
	(search-list smart-address-search-list)
	(search-result nil)
	(case-fold-search t)
	beg end)
    (set-buffer (funcall 'smart-address-other-buffer))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "[^ 	\n]")
      (end-of-line)
      (setq end (point))
      (re-search-backward "^[ 	\n]*$\\|^--$")
      (forward-line 1)
      (setq beg (point))
      (and (> end beg)
	   (while search-list
	     (goto-char beg)
	     (if (re-search-forward (car search-list) end t)
		 (setq search-result (buffer-substring (match-beginning 1) (match-end 1))
		       search-list nil)
	       (setq search-list (cdr search-list))))))
    (set-buffer old-buffer)
    (if search-result
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward "^To:[ 	]*")
	  (end-of-line)
	  (setq end (point))
	  (beginning-of-line)
	  (re-search-forward "^To:[ 	]*")
	  (or
	   (save-excursion
	     (re-search-forward "\\(\\).*\\([ 	](.*)\\)" end t))
	   (save-excursion
	     (re-search-forward "\\(.*<\\).*\\(>\\)" end t))
	   (re-search-forward "\\(\\).*\\(\\)$"))
	  (replace-match (concat "\\1" search-result "\\2")))
      (error "No better address found"))))
