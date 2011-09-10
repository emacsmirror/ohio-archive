;; S-exp History Substitution in GNU Emacs Lisp-Interaction-Mode
;; Copyright (C) 1991 by Takeyuki Sakaguchi.

;; LCD Archive Entry:
;; sexp-history|Takeyuki Sakaguchi|saka@train.ncl.omron.co.jp
;; |S-exp history substitution in GNU Emacs lisp-interaction-mode
;; |91-12-01|1.0|~/packages/sexp-history.tar.Z|

;;   Author:            Takeyuki SAKAGUCHI
;;   Version:           1.0
;;   Last modification: Dec. 1, 1991

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 1, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU EMACS GENERAL PUBLIC
;; LICENSE along with GNU Emacs; see the file COPYING. If not,
;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;; MA 02139, USA.


(defvar sexp-history-event-max 20
  "*Maximum number of events kept in the history buffer.")

(defvar sexp-history-histchars '(?! . ?^)
  "*Pair of history substitution characters.")

(defvar sexp-history-list-order 'ascending
  "*Listing order in sexp-history-list. Either 'descending or 'ascending.")

(defvar sexp-history-quoted-too nil
  "*Whether substitute or not in quoted strings.
If non-nil substitutes as well. Otherwise doesn't.")

(defvar sexp-history-echo-only nil
  "*Whether evaluate or not after echoback in full.
If non-nil echoes back but never evaluates. Otherwise does.")

(defvar sexp-history-map nil
  "Keymap used while S-exp history substitution active.")
(if sexp-history-map
    nil
  (setq sexp-history-map
	(copy-keymap lisp-interaction-mode-map))
  (substitute-key-definition 'eval-print-last-sexp
			     'eval-print-last-sexp-with-history
			     sexp-history-map))

(defvar sexp-history-syntax-table nil
  "Syntax table used while parsing S-exp.
\`equal\' to emacs-lisp-mode-syntax-table but not \`eq\'.")
(if sexp-history-syntax-table
    nil
  (setq sexp-history-syntax-table
	(copy-syntax-table emacs-lisp-mode-syntax-table)))

(defun sexp-history (reset-p)
  "Lisp-Interaction-Mode with history substitution capability.
With no arg, this command toggles on/off S-exp history substitution on
current lisp-interaction-mode buffer. Otherwise turns on or resets.

Note: this command merely introduces several additional variables,
functions and alternative key bindings to lisp-interaction-mode. It
ever introduces NEITHER major NOR minor modes.

Additional variables:
  sexp-history-event-max    maximum number of events kept in the
                            history buffer. 20 by default.
  sexp-history-histchars    pair of history substitution characters.
                            (\`!\' . \`^\') by default.
  sexp-history-list-order   listing order in sexp-history-list. Either
                            'descending or 'ascending. 'ascending by
                            default.
  sexp-history-quoted-too   Whether substitute or not in quoted strings.
                            If non-nil substitutes as well. Otherwise
                            doesn't. nil by default.
  sexp-history-echo-only    whether evaluate or not after echoback in
                            full. If non-nil echoes back but never
                            evaluates. Otherwise does. S-exp that in-
                            vokes NO history substitution is evaluated
                            regardless of this value. nil by default.
  sexp-history-map          keymap used while S-exp history substitution
                            active. This is the alternative map that
                            invokes eval-print-last-sexp-with-history.
  sexp-history-syntax-table syntax table used while parsing S-exp.
                            \`equal\' to emacs-lisp-mode-syntax-table,
                            but not \`eq\'.

Key binding alternation:
  \\<sexp-history-map>\\[eval-print-last-sexp-with-history]    bound to command eval-print-last-sexp-with-history.

Type \\[describe-function] eval-print-last-sexp-with-history for the substitution
notation in detail; \\[describe-function] lisp-interaction-mode for original features."
  (interactive "P")
  (if (not (eq major-mode 'lisp-interaction-mode))
      (error "Not in lisp-interaction-mode.")
    (cond ((eq (current-local-map) lisp-interaction-mode-map)
	   ;; make globals to buffer local.
	   (make-local-variable 'sexp-history-event-max)
	   (make-local-variable 'sexp-history-histchars)
	   (make-local-variable 'sexp-history-list-order)
	   (make-local-variable 'sexp-history-quoted-too)
	   (make-local-variable 'sexp-history-echo-only)
	   ;; sexp-history-event-buffer history buffer itself. Consists of alists
	   ;;                           of event number and S-exp string.
	   (make-local-variable 'sexp-history-event-buffer)
	   (setq sexp-history-event-buffer nil)
	   ;; sexp-history-current-event current event number.
	   (make-local-variable 'sexp-history-current-event)
	   (setq sexp-history-current-event 1)
	   ;; sexp-history-last-event the most recently referred event no.
	   (make-local-variable 'sexp-history-last-event)
	   (setq sexp-history-last-event nil)
	   ;; sexp-history-last-subst the most recent substitution.
	   (make-local-variable 'sexp-history-last-subst)
	   (setq sexp-history-last-subst nil)
	   ;; sexp-history-last-regexp the most recent regexp in modifier.
	   (make-local-variable 'sexp-history-last-regexp)
	   (setq sexp-history-last-regexp nil)
	   ;; sexp-history-termination termination point of the last S-exp.
	   (make-local-variable 'sexp-history-termination)
	   (setq sexp-history-termination 0)
	   (use-local-map sexp-history-map)
	   (message "S-exp history substitution active."))
	  (reset-p
	   (setq sexp-history-event-buffer nil
		 sexp-history-current-event 1
		 sexp-history-last-event nil
		 sexp-history-last-subst nil
		 sexp-history-last-regexp nil
		 sexp-history-termination 0)
	   (message "S-exp history substitution reset."))
	  (t
	   ;; restore locals to global.
	   (kill-local-variable 'sexp-history-event-max)
	   (kill-local-variable 'sexp-history-histchars)
	   (kill-local-variable 'sexp-history-list-order)
	   (kill-local-variable 'sexp-history-quoted-too)
	   (kill-local-variable 'sexp-history-echo-only)
	   ;; unbound locals.
	   (kill-local-variable 'sexp-history-event-buffer)
	   (makunbound 'sexp-history-event-buffer)
	   (kill-local-variable 'sexp-history-current-event)
	   (makunbound 'sexp-history-current-event)
	   (kill-local-variable 'sexp-history-last-event)
	   (makunbound 'sexp-history-last-event)
	   (kill-local-variable 'sexp-history-last-subst)
	   (makunbound 'sexp-history-last-subst)
	   (kill-local-variable 'sexp-history-last-regexp)
	   (makunbound 'sexp-history-last-regexp)
	   (kill-local-variable 'sexp-history-termination)
	   (makunbound 'sexp-history-termination)
	   (use-local-map lisp-interaction-mode-map)
	   (message "S-exp history substitution inactive.")))))

(defun eval-print-last-sexp-with-history ()
  "Eval-Print-Last-Sexp with history substitution capability.
Notation for substitution is similar to that of C shell:

Special forms:
  !        stands for function call \`(sexp-history-list)\' that shows
           current history listings.
  ^RE^str^ abbreviates !!:s/RE/str/.

Primitive forms (event specifiers):
  !!    stands for the most recently evaluated S-exp.
  !^    stands for CAR of !! (equivalent to !!:^).
  !*    stands for CDR of !! (equivalent to !!:*).
  !$    stands for LAST of !! (equivalent to !!:$).
  !n    stands for S-exp of absolute event number n.
  !-n   stands for S-exp of relative event number n.
  !RE   stands for the most recently evaluated S-exp that matches to RE
        only at the beginning. Leading \`(\'s in S-exp are ignored. e.g.,
        !foo matches to any of \`foo\', \`(foo\', \`((foo\', \`(((foo\', and
        so on. This is equivalent to !?^\\(*foo?.
  !{RE} is escaped form for !RE in case of misinterpretation. e.g.,
        !{1} matches to S-exp beginning with \`1\' while !1 matches to
        event number one.
  !?RE? stands for the most recently evaluated S-exp that matches to RE
        anywhere. Extracts only matching word(s) if followed by \`%\'.
  !+    stands for the most recently referred event.
  !++   stands for the next event to !+.
  !&    stands for the most recent substitution.

Note: in RE, punctuation chars such as whitespace, single and double
quotation marks, \`.\', \`(\', \`)\', \`[\' and \`]\' must be escaped by \`\\\' to
mask their special meaning on emacs-lisp-mode-syntax-table. Moreover,
notation closing chars such as \` \' in !RE, \`}\' in !{RE} and \`?\' in
!?RE? also must be escaped. \`\\\' itself must be doubled like in a quoted
string. For example:

!?foo\\.*bar\\[^\\\\\\\"\\]+$?  means match for \`foo.*bar[^\\\"]+$\'.
!?\\(cons\\[\\\\\\ \\\\t\\]+foo? means match for \`(cons[\\ \\t]+foo\'.
!?\\?=?                   means match for \`?=\'.
!\\:=                     means match for \`^(*:=\'.

See The GNU Emacs Reference Manual for more details.

Additive forms (position designators/modifiers):
  :^          stands for CAR of preceding notation.
  :*          stands for CDR of preceding notation.
  :$          stands for LAST of preceding notation.
  :n          stands for NTH of preceding notation.
  :n*         stands for NTHCDR of preceding notation.
  :n-         is like n* but omitting the last.
  :-m         abbreviates 0-m.
  :n-m        stands for range in preceding notation.
  :D          stands for deletion.
  :i          stands for inside of parentheses.
  :r          stands for REVERSE of preceding notation.
  :s/RE/str/  stands for substitution of RE with str only once. If str
              preceded by \`&\', appends it instead.
  :gs/RE/str/ stands for substitution of RE with str globally. If str
              preceded by \`&\', appends it instead.

Note: additive forms can be applied recursively. For example:
  !*:*:5:$   means (LAST (nth 5 (cdr !*)))
  !16:^:2    means (nth 2 (car !16))
  !setq:$:^  means (car (LAST !setq)) 
  !{1}:*:r   means (reverse (cdr !{1}))

Type \\[describe-function] eval-print-last-sexp for original features."
  (interactive)
  (skip-chars-backward "\ \t\n")
  (setq sexp-history-termination (point))
  (if (bobp)
      nil
    (let ((original-syntax-table (syntax-table))
	  (original-match-data (match-data)))
      (set-syntax-table sexp-history-syntax-table)
      (unwind-protect
	  (if (and (sexp-history-echoback)
		   sexp-history-echo-only)
	      nil
	    (let ((end (point)))
	      (backward-sexp 1)
	      (set-syntax-table original-syntax-table)
	      (store-match-data original-match-data)
	      (sexp-history-truncate (- sexp-history-current-event
					sexp-history-event-max))
	      (setq sexp-history-event-buffer
		    (cons (cons sexp-history-current-event
				(buffer-substring (point)
						  end))
			  sexp-history-event-buffer))
	      (setq sexp-history-current-event
		    (1+ sexp-history-current-event))
	      (eval-region (point) end
			   (current-buffer))))
	(save-excursion
	  (goto-char sexp-history-termination)
	  (or (eobp)
	      (/= (char-after (point)) 0)
	      (delete-char 1)))
	(or (eq (syntax-table)
		original-syntax-table)
	    (set-syntax-table original-syntax-table))
	(store-match-data original-match-data)))))

(defun sexp-history-echoback ()
  "Subordinate function for S-exp history."
  ;; Echoes back S-exp in full after parsing the last S-exp.
  ;; Returns t if any echoback done, nil otherwise.
  (let ((replacement (sexp-history-parse-last-sexp)))
    (if (null replacement)
	nil
      (let ((beginning))
	(save-excursion
	  (backward-sexp 1)
	  (setq beginning (point)))
	(insert "\n"
		(buffer-substring beginning (point))))
      (backward-sexp 1)
      (while replacement
	(forward-char (nth 0 (car replacement)))
	(delete-char (nth 1 (car replacement)))
	(insert (nth 2 (car replacement)))
	(setq replacement (cdr replacement)))
      t)))

(defun sexp-history-parse-last-sexp ()
  "Subordinate function for S-exp history."
  ;; Parses the last S-exp and returns replacement information for echo-
  ;; back. It consists of lists of relative position, length and text
  ;; to replace with.
  (insert "\0")
  (backward-char 1)
  (backward-sexp 1)
  (let ((replacement nil) (relp (point)) (char))
    (while (/= (setq char (char-after (point))) 0)
      (cond ((= char ?\\)
	     ;; `\' escape sequence.
	     (if (= (char-after (1+ (point))) 0)
		 (forward-char 1)
	       (forward-char 2)))
	    ((= char ??)
	     ;; `?' escape sequence.
	     (setq char (char-after (1+ (point))))
	     (cond ((= char 0)
		    (forward-char 1))
		   ((= char ?\\)
		    (forward-char 3))
		   (t
		    (forward-char 2))))
	    ((= char ?\")
	     ;; beginning of `"' quoted string.
	     (forward-char 1)
	     (or sexp-history-quoted-too
		 (while (/= (prog1
				(setq char (char-after (point)))
			      (forward-char 1))
			    ?\")
		   (if (= char ?\\)
		       (forward-char 1)))))
	    ((= char (car sexp-history-histchars))
	     ;; beginning of `!' notation.
	     (let ((bor) (lor) (repl nil))
	       (setq char (char-after (1+ (point))))
	       (cond ((= char 0)
		      ;; `!' special.
		      (setq lor 1
			    repl "(sexp-history-list)"))
		     ((= char (car sexp-history-histchars))
		      ;; `!!' primitive.
		      (setq lor 2
			    repl (sexp-history-match* -1)))
		     ((= char ?^)
		      ;; `!^' primitive.
		      (setq lor 2
			    repl (sexp-history-apply
				  'car (sexp-history-match* -1))))
		     ((= char ?*)
		      ;; `!*' primitive.
		      (setq lor 2
			    repl (sexp-history-apply
				  'cdr (sexp-history-match* -1))))
		     ((= char ?$)
		      ;; `!$' primitive.
		      (setq lor 2
			    repl (sexp-history-apply
				  'last (sexp-history-match* -1))))
		     ((= char ?+)
		      (cond ((= (char-after (+ (point) 2)) ?+)
			     ;; `!++' primitive.
			     (setq lor 3
				   repl (sexp-history-match*
					 (1+ sexp-history-last-event))))
			    (t
			     ;; `!+' primitive.
			     (setq lor 2
				   repl (sexp-history-match*
					 sexp-history-last-event)))))
		     ((= char ?&)
		      ;; `!&' primitive.
		      (setq lor 2
			    repl sexp-history-last-subst))
		      ((or (= char ?-)
			  (and (>= char ?0) (<= char ?9)))
		      (if (looking-at ".\\(\\-?[0-9]+\\)")
			  ;; `!n' primitive.
			  (let ((key (sexp-history-match-substring 1)))
			    (setq lor (- (match-end 0) (point))
				  repl (sexp-history-match*
					(string-to-int key))))
			(error "Invalid notation: \`%c-\' -- missing \`n\'."
			       (car sexp-history-histchars))))
		     ((= char ??)
		      (if (looking-at ".\\?\\(\\([^\0\\?]\\|\\(\\\\.\\)\\)+\\)\\?\\%?")
			  ;; `!?RE?' or `!?RE?%' primitive.
			  (let ((key (sexp-history-match-substring 1))
				(word-only-p (= (char-after
						 (1- (match-end 0))) ?%)))
			    (setq lor (- (match-end 0) (point))
				  repl (sexp-history-match*
					(sexp-history-deesc-text key)
					nil word-only-p)))
			(error "Invalid notation: \`%c?\' -- incomplete \`RE?\'."
			       (car sexp-history-histchars))))
		     ((= char ?{)
		      (if (looking-at ".\\{\\(\\([^\0\\}]\\|\\(\\\\.\\)\\)+\\)\\}")
			  ;; `!{RE}' primitive.
			  (let ((key (sexp-history-match-substring 1)))
			    (setq lor (- (match-end 0) (point))
				  repl (sexp-history-match*
					(sexp-history-deesc-text key)
					t nil)))
			(error "Invalid notation: \`%c{\' -- incomplete \`RE}\'."
			       (car sexp-history-histchars))))
		     ((looking-at ".\\(\\([^\0\\ \\:]\\|\\(\\\\.\\)\\)+\\)")
		      ;; `!RE' primitive.
		      (let ((key (sexp-history-match-substring 1)))
			(setq lor (- (match-end 0) (point))
			      repl (sexp-history-match*
				    (sexp-history-deesc-text key)
				    t nil))))
		     (t
		      ;; otherwise: '! ' or '!:'.
		      (error "Invalid notation: \`%c%c\'."
			     (car sexp-history-histchars) char)))
	       (setq bor (- (point) relp))
	       (or repl
		   (error "Event not found: \`%s\'."
			  (buffer-substring (point)
					    (+ (point) lor))))
	       (forward-char lor)
	       (while (= (char-after (point)) ?:)
		 (let ((loa))
		   (setq char (char-after (1+ (point))))
		   (cond ((= char ?^)
			  ;; `:^' additive.
			  (setq loa 2
				repl (sexp-history-apply 'car repl)))
			 ((= char ?*)
			  ;; `:*' additive.
			  (setq loa 2
				repl (sexp-history-apply 'cdr repl)))
			 ((= char ?$)
			  ;; `:$' additive.
			  (setq loa 2
				repl (sexp-history-apply 'last repl)))
			 ((or (= char ?-)
			      (and (>= char ?0) (<= char ?9)))
			  (if (looking-at ".\\(\\([0-9]+\\*?\\)\\)?\\(\\-\\)?\\(\\([0-9]+\\)\\)?")
			      ;; `:n', `:n*', `:n-', `:-m' or `:n-m' additive.
			      (let ((n (sexp-history-match-substring 1))
				    (m (sexp-history-match-substring 4))
				    (hyphen (sexp-history-match-substring 3)))
				(setq loa (- (match-end 0)
					     (match-beginning 0)))
				(cond (hyphen
				       ;; `:n-', `:-m' or `:n-m'.
				       (and n
					    (setq n (string-to-int n)))
				       (and m
					    (setq m (string-to-int m)))
				       (if (or n m)
					   (setq repl (sexp-history-range repl n m))
					 (error "Invalid notation: \`%c-\' -- missing \`n\' or \`m\'."
						?:)))
				      (t
				       ;; `:n' or `:n*'.
				       (setq char (string-to-char (substring n -1))
					     n (string-to-int n))
				       (setq repl (sexp-history-apply
						   (if (= char ?*)
						       'nthcdr
						     'nth) repl n)))))))
			 ((= char ?D)
			  ;; `:D' additive.
			  (setq loa 2
				repl (prin1-to-string
				      (sexp-history-delete sexp-history-last-event))))
			 ((= char ?i)
			  ;; `:i' additive.
			  (setq loa 2
				repl (if (string-match "(\\(.*\\))" repl)
					 (substring repl
						    (match-beginning 1)
						    (match-end 1))
				       repl)))
			 ((= char ?r)
			  ;; `:r' additive.
			  (setq loa 2
				repl (sexp-history-apply
				      'reverse repl)))
			 ((or (= (downcase char) ?g)
			      (= (downcase char) ?s))
			  (if (looking-at ".[Gg]?[Ss]\\/\\(\\([^\0\\/]\\|\\(\\\\.\\)\\)+\\)?\\/\\&?\\(\\([^\0\\/]\\|\\(\\\\.\\)\\)*\\)\\/?")
			      ;; `:s/RE/str/', `:gs/RE/str/', `:s/RE/&str/', `:gs/RE/&str/'
			      ;; `:S/RE/str/', `:GS/RE/str/', `:S/RE/&str/' or `:GS/RE/&str/' additive.
			      (let ((RE (sexp-history-deesc-text
					 (sexp-history-match-substring 1)))
				    (str (sexp-history-deesc-text
					  (sexp-history-match-substring 4)))
				    (count (if (= (char-after (1+ (match-beginning 0)))
						  ?g)
					       -1
					     1))
				    (append-p (= (char-after (1- (match-beginning 4)))
						 ?&))
				    (case-fold-search (if (or (= char ?G)
							      (= char ?S))
							  (not case-fold-search)
							case-fold-search)))
				(setq loa (- (match-end 0)
					     (match-beginning 0)))
				(or RE
				    (setq RE sexp-history-last-regexp)
				    (error "No previous lhs."))
				(or (setq repl
					  (sexp-history-string-subst repl RE str count append-p))
				    (error "Modifier failed: \`%s\' no match."
					   RE))
				(setq sexp-history-last-regexp RE))
			    (error "Invalid notation: \`%c(g)s\' -- incomplete \`RE/str/\'."
				   ?:)))
			 (t
			  ;; otherwise.
			  (error "Invalid notation: \`%c%c\'."
				 ?: char)))
		   (setq lor (+ lor loa))
		   (forward-char loa)))
	       (setq relp (point)
		     replacement (cons (list bor lor repl)
				       replacement)
		     sexp-history-last-subst repl)))
	    ((= char (cdr sexp-history-histchars))
	     ;; beginning of `^' notation.
	     (let* ((histchar (regexp-quote (char-to-string char)))
		    (regexp (concat
			     ".\\(\\([^\0" histchar
			     "]\\|\\(\\\\.\\)\\)+\\)?" histchar
			     "\\&?\\(\\([^\0" histchar
			     "]\\|\\(\\\\.\\)\\)*\\)" histchar
			     "?")))
	       (if (looking-at regexp)
		   (let ((bor (- (point) relp))
			 (lor (- (match-end 0) (match-beginning 0)))
			 (repl)
			 (RE (sexp-history-deesc-text
			      (sexp-history-match-substring 1)))
			 (str (sexp-history-deesc-text
			       (sexp-history-match-substring 4)))
			 (append-p (= (char-after (1- (match-beginning 4)))
				      ?&)))
		     (or (setq repl
			       (sexp-history-match* -1))
			 (error "Event not found: \`%s\'."
				(buffer-substring (point)
						  (+ (point) lor))))
		     (or RE
			 (setq RE sexp-history-last-regexp)
			 (error "No previous lhs."))
		     (or (setq repl
			       (sexp-history-string-subst repl RE str 1 append-p))
			 (error "Modifier failed: \`%s\' no match."
				RE))
		     (setq sexp-history-last-regexp RE)
		     (forward-char lor)
		     (setq relp (point)
			   replacement (cons (list bor lor repl)
					     replacement)))
		 (error "Invalid notation: \`%c\' -- incomplete \`RE%cstr%c\'."
			char char char))))
	    (t
	     (forward-char 1))))
    (delete-char 1)
    (and replacement
	 (setq replacement (cons (list (- (point) relp) 0 "")
				 replacement))
	 (nreverse replacement))))

(defun sexp-history-match* (key &optional beginning-only-p word-only-p)
  "Subordinate function for S-exp history."
  ;; Extracts S-exp text of the newest event.
  (let ((events (sexp-history-match key beginning-only-p word-only-p)))
    (if (null events)
	nil
      (setq sexp-history-last-event (car (car events)))
      (cdr (car events)))))

(defun sexp-history-match (key &optional beginning-only-p word-only-p)
  "Subordinate function for S-exp history."
  ;; Returns alists for events that match to given key.
  ;; If no match returns nil. Matching policy is as follows:
  ;;
  ;; numeric key: > 0: match for absolute event number.
  ;;              < 0: match for relative event number.
  ;;
  ;; string key:  match for regexp. If beginning-only-p is non-nil it
  ;;              matches only at the beginning ignoring leading `('s.
  ;;              Otherwise, matches anywhere. If word-only-p is non-nil
  ;;              extracts only matching word(s).
  (cond ((numberp key)
	 (if (>= key 0)
	     (list (assq key
			 sexp-history-event-buffer))
	   (list (assq (+ sexp-history-current-event key)
		       sexp-history-event-buffer))))
	((stringp key)
	 (if beginning-only-p
	     (setq key (concat "^(*" key))
	   (if word-only-p
	       (setq key (concat "\\(\\sw\\|\\s_\\)*"
				 key
				 "\\(\\sw\\|\\s_\\)*"))))
	 (delq nil
	       (mapcar (function
			(lambda (event)
			  (and (string-match key (cdr event))
			       (if word-only-p
				   (cons (car event)
					 (substring (cdr event)
						    (match-beginning 0)
						    (match-end 0)))
				 event))))
		       sexp-history-event-buffer)))
	(t
	 nil)))

(defun sexp-history-apply (func text &optional n)
  "Subordinate function for S-exp history."
  ;; Applies given function on given text that stands for S-exp.
  (if (null text)
      nil
    (let ((sexp (car (read-from-string text)))
	  (print-escape-newlines t))
      (if (listp sexp)
	  (cond ((eq func 'last)
		 (setq sexp (car (nreverse sexp))))
		(n
		 (setq sexp (funcall func n sexp)))
		(t
		 (setq sexp (funcall func sexp)))))
      (prin1-to-string sexp))))

(defun sexp-history-range (text n m)
  "Subordinate function for S-exp history."
  ;; Extract from nth to mth of given text that stands for S-exp.
  ;; If n is nil taken as the first. If m is nil taken as the second
  ;; last.
  (if (null text)
      nil
    (let ((sexp (car (read-from-string text)))
	  (print-escape-newlines t))
      (if (listp sexp)
	  (let ((i 0))
	    (or n
		(setq n 0))
	    (or m
		(setq m (- (length sexp) 2)))
	    (setq sexp
		  (delq nil
			(mapcar (function
				 (lambda (element)
				   (prog1
				       (and (>= i n)
					    (<= i m)
					    element)
				     (setq i (1+ i)))))
				sexp)))))
      (prin1-to-string sexp))))

(defun sexp-history-deesc-text (text)
  "Subordinate function for S-exp history."
  ;; Eliminates '\' escape chars from given text.
  (let ((new-text nil) (si 0) (char))
    (while (< si (length text))
      (setq char (aref text si))
      (if (= char ?\\)
	  (setq si (1+ si)
		char (aref text si)))
      (setq new-text (concat
		      new-text
		      (char-to-string char)))
      (setq si (1+ si)))
    new-text))

(defun sexp-history-truncate (min)
  "Subordinate function for S-exp history."
  ;; Truncates the history buffer.
  ;; Deletes all events whose number is equal or less than given
  ;; mininum number except for the newest.
  (let ((event-buffer sexp-history-event-buffer))
    (while (cdr event-buffer)
      (if (<= (car (car (cdr event-buffer))) min)
	  (setcdr event-buffer nil)
	(setq event-buffer (cdr event-buffer))))))

(defun sexp-history-delete (event-no)
  "Subordinate function for S-exp history."
  ;; Deletes an event corresponding to given event-no.
  ;; Returns t if deletion done, nil otherwise.
  (let ((delete-event))
    (if (null (setq delete-event (assq event-no
				       sexp-history-event-buffer)))
	nil
      (setq sexp-history-event-buffer
	    (delq (assq event-no
			sexp-history-event-buffer)
		  sexp-history-event-buffer))
      t)))

(defun sexp-history-list ()
  "Subordinate function for S-exp history."
  ;; Prints S-exp history listings into current buffer.
  ;; The order is as specified by sexp-history-list-order.
  (if (not (eq major-mode 'lisp-interaction-mode))
      (error "Not in lisp-interaction-mode")
    (insert
     (mapconcat (function
		 (lambda (event)
		   (format "\n%4d  %s"
			   (car event)
			   (cdr event))))
		(if (eq sexp-history-list-order 'ascending)
		    (reverse sexp-history-event-buffer)
		  sexp-history-event-buffer)
		nil))))

(defun sexp-history-string-subst (text regexp to-string &optional count append-p)
  "Subordinate function for S-exp history."
  ;; Substitutes matches to regexp with to-string.
  ;; If count is non-nil taken as -1 that means `global' substitution.
  ;; If append-p is non-nil inserts to-string immediately after the match
  ;; instead of replacing with it. Returns nil if no match-and-replace done.
  (if (null text)
      nil
    (or count
	(setq count -1))
    (let ((result nil) (beginning 0))
      (while (and (/= count 0)
		  (string-match regexp text beginning))
	(setq result (concat
		      result
		      (substring text
				 beginning
				 (if append-p
				     (match-end 0)
				   (match-beginning 0)))
		      to-string))
	(setq count (1- count)
	      beginning (match-end 0)))
      (if (> beginning 0)
	  (concat result (substring text beginning))
	nil))))

(defun sexp-history-match-substring (level)
  "Subordinate function for S-exp history."
  ;; Returns matched buffer substring. nil if match empty.
  (and (match-beginning level)
       (buffer-substring
	(match-beginning level)
	(match-end level))))
