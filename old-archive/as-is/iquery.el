(defun isearch (forward &optional regexp prompt)
  (let ((search-string "")
	(search-message (or prompt ""))
	(cmds nil)
	(success t)
	(wrapped nil)
	(invalid-regexp nil)
	(slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
				 (> (window-height)
				    (* 4 search-slow-window-lines))))
	(other-end nil)    ;Start of last match if fwd, end if backwd.
	(small-window nil)		;if t, using a small window
	(window-min-height (min window-min-height
				(1+ search-slow-window-lines)))
					;so we can make small windows
	(found-point nil)		;to restore point from a small window
	;; This is the window-start value found by the search.
	(found-start nil)
	(opoint (point))
	(inhibit-quit t))  ;Prevent ^G from quitting immediately.
    (isearch-push-state)
    (save-window-excursion
     (catch 'search-done
       (while t
	 (or (>= unread-command-char 0)
	     (progn
	       (or (input-pending-p)
		   (isearch-message))
	       (if (and slow-terminal-mode
			(not (or small-window (pos-visible-in-window-p))))
		   (progn
		     (setq small-window t)
		     (setq found-point (point))
		     (move-to-window-line 0)
		     (split-window nil (- (window-height)
					  (1+ search-slow-window-lines)))
		     (other-window 1)
		     (goto-char found-point)))))
	 (let ((char (if quit-flag
			 ?\C-g
		       (read-char))))
	   (setq quit-flag nil)
	   ;; Meta character means exit search.
	   (cond ((and (>= char 128)
		       search-exit-option)
		  (setq unread-command-char char)
		  (throw 'search-done t))
		 ((eq char search-exit-char)
		  ;; Esc means exit search normally.
		  ;; Except, if first thing typed, it means do nonincremental
		  (if (= 0 (length search-string))
		      (nonincremental-search forward regexp))
		  (throw 'search-done t))
		 ((= char ?\C-g)
		  ;; ^G means the user tried to quit.
		  (ding)
		  (discard-input)
		  (if success
		      ;; If search is successful, move back to starting point
		      ;; and really do quit.
		      (progn (goto-char opoint)
			     (signal 'quit nil))
		    ;; If search is failing, rub out until it is once more
		    ;;  successful.
		    (while (not success) (isearch-pop))))
		 ((eq char search-repeat-char)
		  ;; ^S means search again, forward, for the same string.
		  ;; If failing, try again from beginning of buffer.
		  (if forward
		      (or success
			  (progn (goto-char (point-min))
				 (setq wrapped t)))
		    (setq forward t))
		  (if (null (cdr cmds))
		      ;; If the first char typed,
		      ;; it means search for the string of the previous search
		      (progn
		        (setq search-string
			        (if regexp
				    search-last-regexp search-last-string)
			      search-message
			        (mapconcat 'text-char-description
					   search-string ""))))
		  ;; Give benefit of doubt.
		  (setq success t)
		  (isearch-search)
		  (isearch-push-state))
		 ((eq char search-reverse-char)
		  ;; ^R is similar but it searches backward.
		  ;; If failing, try again from beginning of buffer.
		  (if (not forward)
		      (or success
			  (progn (goto-char (point-max))
				 (setq wrapped t)))
		    (setq forward nil))
		  (if (null (cdr cmds))
		      (progn
			(setq search-string
			        (if regexp
				    search-last-regexp search-last-string)
			      search-message
			        (mapconcat 'text-char-description
					   search-string ""))))
		  (setq success t)
		  (isearch-search)
		  (isearch-push-state))
		 ((= char search-delete-char)
		  ;; Rubout means discard last input item and move point
		  ;; back.  If buffer is empty, just beep.
		  (if (null (cdr cmds))
		      (ding)
		    (isearch-pop)))
		 (t
		  (cond ((or (eq char search-yank-word-char)
			     (eq char search-yank-line-char))
			 ;; ^W means gobble next word from buffer.
			 ;; ^Y means gobble rest of line from buffer.
			 (let ((word (save-excursion
				       (and (not forward) other-end
					    (goto-char other-end))
				       (buffer-substring
					(point)
					(save-excursion
					  (if (eq char search-yank-line-char)
					      (end-of-line)
					    (forward-word 1))
					  (point))))))
			   (setq search-string (concat search-string word)
				 search-message
				   (concat search-message
					   (mapconcat 'text-char-description
						      word "")))))
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (< char ? ) (/= char ?\t) (/= char ?\r))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => add it to the
			  ;;  search string and search.
			  (cond ((= char search-quote-char)
				 (setq char (read-quoted-char
					     (isearch-message t))))
				((= char ?\r)
				 ;; unix braindeath
				 (setq char ?\n)))
			  (setq search-string (concat search-string
						      (char-to-string char))
				search-message (concat search-message
						       (text-char-description char)))))
		  (if (and (not success)
			   ;; unsuccessful regexp search may become
			   ;;  successful by addition of characters which
			   ;;  make search-string valid
			   (not regexp))
		      nil
		    ;; In reverse regexp search, adding a character at
		    ;; the end may cause zero or many more chars to be
		    ;; matched, in the string following point.
		    ;; Allow all those possibiities without moving point as
		    ;; long as the match does not extend past search origin.
		    (if (and regexp (not forward)
			     (condition-case ()
				 (looking-at search-string)
			       (error nil))
			     (< (match-end 0) opoint))
			(setq success t invalid-regexp nil
			      other-end (match-end 0))
		      ;; Not regexp, not reverse, or no match at point.
		      (if other-end
			  (goto-char (if (or forward regexp) other-end
				       (min opoint (1+ other-end)))))
		      (isearch-search)))
		  (isearch-push-state))))))
     (setq found-start (window-start (selected-window)))
     (setq found-point (point)))
    (if regexp
	(setq search-last-regexp search-string)
      (setq search-last-string search-string))
    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) opoint) (push-mark opoint))
    (if small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers this; restore it.
      (set-window-start (selected-window) found-start t))
    (message "")
    search-last-string))

(defun query-replace (from-string to-string &optional arg)
  "\
Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
  (interactive
   (let* ((search-exit-char ?\r)
	  (point-orig (point))
	  (pattern (isearch t nil "(Query replace) ")))
     (goto-char (max point-orig (- (point) (length pattern))))
     (list pattern
	   (read-string (format "Query replace %s with: " pattern))
	   current-prefix-arg)))
  (perform-replace from-string to-string t nil arg))
