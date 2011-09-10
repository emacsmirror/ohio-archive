;;; Last modified on Mon May 11 09:35:32 PDT 1992 by detlefs                  ;
;;;      modified on Thu Apr 23 17:45:03 PDT 1992 by muller                   ;
;;;      modified on Wed Mar  4 10:47:23 PST 1992 by heydon                   ;
;;;      modified on Fri Feb  2 13:04:24 1990 by discolo                      ;
;;;      modified on Tue May  2 21:59:35 1989 by ellis                        ;
;;;      modified                             by Trevor Morris                ;
;;;      modified                             by Tom Perrine                  ;
;;;      modified                             by Michael Schmidt              ;
;;;      modified                             by Peter Robinson               ;
;;;      modified                             by mjordan                      ;

;; LCD Archive Entry:
;; modula3|Eric Muller|muller@src.dec.com|
;; Modula-3 mode.|
;; 92-04-17||~/modes/modula3.el.Z|

(provide 'modula3)

;
; MODE SYNTAX TABLE (Added by TEP)
;

(defvar m3-mode-syntax-table nil
  "Syntax table in use in Modula 3 mode buffers.")

(if m3-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( ". 1" table)
    (modify-syntax-entry ?\) ". 4" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?{ "(}  " table)
    (modify-syntax-entry ?} ")}  " table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq m3-mode-syntax-table table)))

;
; MODE KEY MAP (Added by TEP)
;

(defvar m3-mode-map nil
  "Keymap used in Modula 3 mode.")

(defun setup-m3-mode-map ()
  "Sets up Modula 3 mode map; this must be called after the sequence for the
keypad key \"?\\C-@\" has been setup - it uses \"function-key-sequence\" on
that key in order to bind the Modula 3 specific functions"
  (if m3-mode-map ()
    (let ((map (make-sparse-keymap)) (other-map (make-sparse-keymap)))
      (define-key map "\t" 'm3-abbrev-and-or-indent)
      (define-key map "\M-\t" 'm3-ident-complete)
      (define-key map "\C-ca" 'm3-array)
      (define-key map "\C-cb" 'm3-block)
      (define-key map "\C-cc" 'm3-case)
      (define-key map "\C-cd" 'm3-declare)
      (define-key map "\C-ce" 'm3-else)
      (define-key map "\C-cf" 'm3-for)
      (define-key map "\C-ci" 'm3-if)
      (define-key map "\C-cm" 'm3-choose-module)
      (define-key map "\C-cl" 'm3-loop-or-lock)
      (define-key map "\C-c|" 'm3-next-case)
      (define-key map "\C-co" 'm3-object)
      (define-key map "\C-c\C-o" other-map)
      (define-key map "\C-cp" 'm3-procedure)
      (define-key map "\C-cr" 'm3-record)
      (define-key map "\C-ct" 'm3-try-or-typecase)
      (define-key map "\C-cu" 'm3-until)
      (define-key map "\C-cw" 'm3-while-or-with)
      (define-key map "\C-cy" 'm3-import)
      (define-key map "\C-c{" 'm3-begin-comment)
      (define-key map "\C-c}" 'm3-end-comment)
      (define-key other-map "a" 'm3-toggle-abbrev)
      (define-key other-map "v" 'm3-path-find-file)
      (define-key other-map "b" 'm3-toggle-buffer)
      (define-key other-map "c" 'm3-compile)
      (define-key other-map "p" 'm3-convert-proc-header)
      (setq m3-mode-map map)
      )))

;
; INDENTATION
;

(defvar m3-indent 2 "*This variable gives the indentation in Modula 3 Mode")

;
; ROUTINE TO CHECK IF BUFFER CONTAINS DEF MODULE
;

(defun m3-is-def ()
  "Does current buffer's name suggest that it contains an interface?"
  (or (string-equal (m3-get-extension (buffer-name)) ".i")
      (string-equal (m3-get-extension (buffer-name)) ".i3")))

;
; THE MAIN ROUTINE - SETS UP MODULA-3 MODE
;
  
(defun modula-3-mode ()
  "This is a mode intended to support program development in Modula 3.

There are three (!) different ways of avoiding tedious entry of
constructs involving long uppercase keywords:

  1) The template mechanism.  All control constructs of Modula 3 can
     be reached by typing CNTRL C followed (usually!) by the first
     character of the construct.
  2) The 'aggressive pseudo-abbrev' mode. Typing the first letter(s)
     of a construct and then hitting TAB will cause the full construct
     to be inserted.  When there is overlap between two constructs
     (e.g. WITH and WHILE) type the smallest unique substring (e.g.
     \"wi\" for WITH) then hit TAB. If the abbreviation is not
     unique alphabetic ordering is used e.g. \"w\" gives WHILE rather than
     WITH.
  3) The 'polite pseudo-abbrev' mode.  This differs from the
     'aggressive' mode in that it does not insert full template
     constructs.  Instead, in this mode, TAB invoked at the end of a
     word completes just that current word as a keyword.  This mode
     analyzes the context to restrict the choices admitted by partial
     prefixes to as small a set as possible.  If more than 1 choice
     remain after this winnowing, they are ordered according to their
     popularity (assigned in an ad hoc manner by me, dld, and easily
     changed), and the first completion is performed, with a message
     that other completions are possible.  If the choice is wrong,
     hitting TAB immediately will cycle through the other choices.

The template mechanism is always available.  The variable
m3-abbrev-enabled controls the choice of aggressive or polite abbrev
mode.

There are also two independent mechanism for indenting/prettyprinting
text.  The main addition that I (dld) have made is adding the style of
'electric' indentation normally associated with gnuemacs language
modes.  Basically, all you need to know is that TAB, in addition to
completing keywords, also indents the current line properly.  ($I will
soon add mechanisms for indenting the current unit, indenting a
region, etc.)

The other mechanism uses a pretty printer (m3pp) that runs as a
separate process.  The command m3pp-region and m3pp-unit, and the
variable m3pp-options are used to apply m3pp to a portion of the
buffer.  These are not at present bound to specific keys.

Another new feature is END-matching and completion.  Various non-nil
values of the variable 'm3-electric-end' cause hitting TAB on a line
containing just an END to do things like fill in the name of the
procedure, module, or interface, or the keyword that starts the
construct that the END completes.  Another, independent, variable,
'm3-blink-end-matchers', temporarily blinks the curser at the
beginning of the construct that the END matches.  ($An easy thing to
add would be ESC-C-b, move-to-END-matcher)

There are a few mode specific commands which are not to do with inserting text
for language structures (e.g. compile module, toggle pseudo abbrev mode). These
can be used by typing CTRL-C CTRL-O, \"O\" (for \"Other\") and then the
command letter. See the following list for more detailed information.
\\{m3-mode-map}
The variable m3-indent controls the number of spaces for each indentation."
  (interactive)
  (kill-all-local-variables)
  (setup-m3-mode-map)
  (use-local-map m3-mode-map)
  (setq major-mode 'modula-3-mode)
  (setq mode-name "Modula 3")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 75)
  (set-syntax-table m3-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'm3-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'm3-mode-hook))

;
;  FORMATTING
;

(defun m3-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (let ((hpos (current-indentation)))
    (newline)
    (indent-to hpos)))

(defun m3-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (+ (/ (current-column) m3-indent) 1) m3-indent)))


;;;======================================================================
;;; The stuff in this section relate to indentation.

(defun m3-indent-line ()
  "Indent the current-line."
  (interactive)
  (m3-indent-line-work t))

(defun m3-indent-line-work (electric)
  ;; If in unterminated string, give an error.  If in comment and
  ;; electric, indent like previous line.
;;;  (message "indent-line-work") (sit-for 2)
  (let ((string-comment-state (m3-in-comment-or-string)))
    (cond
     ((eq string-comment-state 'string)
      (beep)
      (message "Unterminated Text literal..."))
     ((eq string-comment-state 'comment)
      (if electric
	  (let ((cur-point (point)))
	    (beginning-of-line)
	    (m3-skip-whitespace-in-line)
	    (cond
	     ;; If the current line begines with a close comment,
	     ;; indent it to the level of the matching start comment.
	     ((save-excursion
		(beginning-of-line)
		(m3-skip-whitespace-in-line)
		(looking-at "*)"))
	      (m3-indent-to
	       cur-point
	       (save-excursion
		 (beginning-of-line)
		 (m3-skip-whitespace-in-line)
		 (forward-char 2)
		 (m3-skip-comment-backward (point-min) t)
		 (current-column))))

	     ;;; If the current line begins with an open-comment, and
	     ;;; the opened comment is not nested, indent like a code line.
	     ((save-excursion
		(beginning-of-line)
		(m3-skip-whitespace-in-line)
		(and (looking-at "(*")
		     (not (m3-in-comment-or-string))))
	      (m3-indent-to cur-point (m3-indent-for-line)))

	     ;;; Otherwise, indent to same level as previous
	     ;;; non-whitespace line.
	     (t
	      (m3-indent-to
	       cur-point
	       (save-excursion
		 (forward-line -1)
		 (while (looking-at m3-whitespace-line-re)
		   (forward-line -1))
		 (m3-skip-whitespace-in-line)
		 (if (looking-at "(\\*")
		     (progn (forward-char 2)
			    (m3-skip-whitespace-in-line)))
		 (current-column))))))))

     ;; We're not in a comment or a string.  Indent the current line.
     (t
      (m3-indent-to (point) (m3-indent-for-line))
      ;; Do the appropriate thing for electric end's.
      (m3-do-electric-end)))))


(defun m3-indent-for-line ()
  (save-excursion
    (beginning-of-line)
    (let ((cur-point (point))
	  (part-start (save-excursion
			(m3-backward-to-last-part-begin)
			(point)))
	  (first-code
	   (save-excursion
	     (re-search-forward "[ \t]*"
				(save-excursion (end-of-line) (point))
				t)
	     (goto-char (match-end 0))
;;;	     (message "first-code 2") (sit-for 2)
	     (point)))
	  ;; Must do this because Modula is case-sensitive
	  (case-fold-search nil))

      ;; Find end of previous statement or last keyword-line-starter.
;;;      (message "m3-indent-for-line(A)") (sit-for 2)

      (m3-re-search-backward
       (concat "\\(;\\|^[ \t]*\\(" m3-keyword-line-starters "\\)\\)")
       part-start t)
      (while (m3-in-arg-list part-start)
	(m3-re-search-backward
	 (concat "\\(;\\|^[ \t]*\\(" m3-keyword-line-starters "\\)\\)")
	 part-start t))
      (cond
       ((and (looking-at ";")
	     (save-excursion
	       (beginning-of-line)
	       (re-search-forward
		(concat "^[ \t]*\\(" m3-keyword-line-starters "\\)")
		(save-excursion (end-of-line) (point))
		t)))
	(beginning-of-line)
	(re-search-forward "[ \t]*"))

       (t
	;; skip to the keyword;
	(re-search-forward "[ \t]*")))

;;;      (message "m3-indent-for-line(B)") (sit-for 2)

      ;; Now figure out if there is an intervening incomplete
      ;; statement between here and the original line.
      (let ((prev-statement-start (point)))
;;;	(message "Checking completeness") (sit-for 2)
	(cond
	 ;; Is it incomplete?
	 ((m3-prev-line-incomplete-p cur-point part-start)

	  ;; ...OK, the previous line *was* incomplete.
	  (goto-char cur-point)
;;;	  (message "m3-indent-for-line: incomplete") (sit-for 2)
	  (m3-incomplete-indent cur-point first-code part-start))

	 (t
	  ;; No: the previous line completed a statement, so find it's
	  ;; start and indent from that.
;;;	  (message "m3-indent-for-line: complete") (sit-for 2)

	  (let ((skip-one
		 (and (save-excursion
			(goto-char first-code)
			(looking-at m3-keyword-ssl-enders))
		      (save-excursion
			(goto-char first-code)
			(m3-re-search-backward
			 (concat "\\(" m3-keyword-endable-ssl-introducers
				 "\\|;\\)")
			 part-start t)
			(not (looking-at ";"))))))

;;;	    (message "m3-IFL complete(2): skip-one = %s" skip-one) (sit-for 2)
	    (goto-char cur-point)
	    (beginning-of-line)
	    (m3-re-search-backward
	     (concat "\\(;\\|END\\|\\("
		     m3-keyword-endable-ssl-introducers "\\|"
		     m3-part-starters "\\)\\)")
	     part-start 'move-to-limit)
;;;	    (message "m3-IFL complete(2.5-1)") (sit-for 2)
	    (while (m3-in-arg-list part-start)
;;;	      (message "m3-IFL complete(2.5-2)") (sit-for 2)
	      (m3-re-search-backward
	       (concat "\\(;\\|END\\|\\(" m3-keyword-endable-ssl-introducers
		       "\\|" m3-part-starters "\\)\\)")
	       part-start 'move-to-limit))

	    ;; Should now be at the beginning of the last
	    ;; ';', END, comment-start on left margin, or ssl-introducer.
;;;	    (message "m3-IFL complete(3)") (sit-for 2)
	    (cond
	     (skip-one
;;;	      (message "m3-IFL skip-one(1)") (sit-for 2)
	      (if (looking-at ";") (error "Bad logic."))
	      (cond
	       ((looking-at (concat "^" m3-com-start-re))
;;;		(message "m3-IFL skip-one left-margin-commment") (sit-for 2)
		0)
	       (t
		(re-search-forward m3-keyword-line-starters (point-max) t)
		(goto-char (match-end 0))
;;;		(message "m3-IFL skip-one(2)") (sit-for 2)
		(let ((eol (save-excursion (end-of-line) (point))))
		  (m3-forward-to-code first-code)
;;;		  (message "m3-IFL skip-one(3)") (sit-for 2)
		  (cond
		   ;; Is there stuff between the keyword and the current line?
		   ((and (> (point) eol) (< (point) first-code))
;;;		    (message "m3-IFL: skip-1 indentation x") (sit-for 2)
		    (m3-complete-adjust-indent (current-column) first-code
					       part-start))
		   ;; No;
		   (t
;;;		    (message "m3-IFL: skip-1 indentation y0") (sit-for 2)
		    (m3-re-search-backward
		     (concat "^[ \t]*\\(" m3-keyword-line-starters "\\)")
		     part-start t)
		    (re-search-forward m3-keyword-line-starters first-code t)
		    (goto-char (match-beginning 0))
		    (cond
		     ((save-excursion
			(beginning-of-line)
			(looking-at (concat "[ \t]*" m3-multi-keyword-lines)))
		      (beginning-of-line)
		      (re-search-forward "[ \t]*" first-code t)
		      (goto-char (match-end 0))))
;;;		    (message "m3-IFL: skip-1 indentation y") (sit-for 2)
		    (m3-after-keyword-adjust-indent
		     (current-column)
		     first-code part-start)))))))

	     (t
;;;	      (message "m3-IFL skip-two") (sit-for 2)
	      ;; First of all, are we in a procedure argument list?
	      (let ((in-arg-list (m3-in-arg-list part-start)))
		(cond
		 ;; Are we at the beginning of the file?
		 ;; If so, move current line to left margin.
		 ((eq (save-excursion
			(m3-backward-to-code (point-min))
;;;			(message "m3-IFL foo: %d" (point)) (sit-for 2)
			(point))
		      1)
		  0)

		 ;; Are we looking at a comment on the left margin?
		 ((looking-at (concat "^" m3-com-start-re))
		  0)

		 ;; Is it a keyword starting a line?
		 ((save-excursion
		    (beginning-of-line)
		    (looking-at
		     (concat "[ \t]*\\(" m3-keyword-line-starters "\\|"
			     m3-part-starters "\\)")))
;;;		  (message "m3-IFL: after complete keyword") (sit-for 2)
		  (beginning-of-line)
		  (re-search-forward
		   (concat m3-keyword-line-starters "\\|" m3-part-starters)
		   (point-max) t)
		  (goto-char (match-beginning 0))
;;;		  (message "m3-IFL: after complete keyword 2") (sit-for 2)
		  (m3-after-keyword-adjust-indent (current-column)
						  first-code part-start))

		 (t
		  ;; No; skip backwards another then forward-to-code
;;;		  (message "m3-IFL: skip-two xxx") (sit-for 2)
		  (if (not
		       (looking-at
			(concat m3-keyword-endable-ssl-introducers "\\|;")))
		      (error "Bad logic 2."))
		  (let ((last-complete (looking-at ";\\|END")))
		    (beginning-of-line)
		    (m3-re-search-backward
		     (concat "\\(;\\|END\\|\\("
			     m3-keyword-endable-ssl-introducers "\\)\\)")
		     part-start 'move-to-limit)
;;;		    (message "m3-IFL: skip-two xxx 2") (sit-for 2)
		    (while (and (not in-arg-list) (m3-in-arg-list part-start))
;;;		      (message "m3-IFL: skip-two xxx 2.2") (sit-for 2)
		      (m3-re-search-backward
		       (concat "\\(;\\|END\\|\\("
			       m3-keyword-line-starters "\\)\\)")
		       part-start t))
;;;		    (message "m3-IFL: skip-two xxx 2.5") (sit-for 2)
		    (let ((continue t) (OF-end (point)))
		      (while (and (looking-at "OF") continue)
			(if (re-search-backward
			     "SET[ \t]*\\|ARRAY[ \t]*\\[[^]]*\\][ \t]*"
			     part-start t)
			    (cond
			     ((eq (match-end 0) OF-end)
			      (m3-re-search-backward
			       (concat "\\(;\\|\\("
				       m3-keyword-line-starters "\\)\\)")
			       part-start t))
			     (t (setq continue nil)))
			  (setq continue nil))))
			  
;;;		    (message "m3-IFL: skip-two xxx 3") (sit-for 2)
		    ;; If we're at part-start, then that is the indentation
		    ;; (Since part-starts are not ssl-introducers?)
		    (if (or (not (eq (point) part-start))
			    (looking-at m3-keyword-endable-ssl-introducers))
			(progn
			  (re-search-forward
			   (concat "\\(;\\|END\\|\\("
				   m3-keyword-endable-ssl-introducers "\\)\\)")
			   (point-max) t)
			  (goto-char (match-end 0))
;;;			  (message "m3-IFL: skip-two xxx 4") (sit-for 2)
			  (m3-forward-to-code cur-point)))

;;;		    (message "m3-indent-for-line: indentation") (sit-for 2)
		    (cond
		     (last-complete
		      (m3-complete-adjust-indent (current-column) first-code
						 part-start))
		     (t
		      (m3-after-keyword-adjust-indent (current-column)
						      first-code part-start)
		      )))))))))))))))




(defun m3-in-arg-list (part-start)
  "Returns non-NIL iff the point is in a procedure or method argument
list."
;;;  (message "m3-in-arg-list(1)") (sit-for 2)
  (save-excursion
    (let ((cur-point (point)))
      (m3-re-search-backward "PROCEDURE\\|METHODS" part-start t)
      (cond
       ((looking-at "PROCEDURE")
	(forward-word 1)
	(m3-re-search-forward "([^*]" (point-max) t)
;;;	(message "m3-in-arg-list(3)") (sit-for 2)
	(and (< (point) cur-point)
	     (condition-case err
		 (progn
		   (forward-sexp 1)
;;;		   (message "m3-in-arg-list(4)") (sit-for 2)
		   (> (point) cur-point))
	       (error t))))

       ((looking-at "METHODS")
	(let ((continue t) (res nil))
	  (while (and continue (< (point) cur-point))
	    (m3-re-search-forward "([^*]\\|END" (point-max) t)
;;;	    (message "m3-in-arg-list(101)") (sit-for 2)
	    (cond
	     ((and (looking-at "([^*]") (< (point) cur-point))
;;;	      (message "m3-in-arg-list(101.5)") (sit-for 2)
	      (condition-case err
		  (progn
		    (forward-sexp 1)
;;;		    (message "m3-in-arg-list(102)") (sit-for 2)
		    (if (> (point) cur-point) (setq res t)))
		(error
		 ;; No matching right paren, so must still be in arg list.
;;;		 (message "m3-in-arg-list(103)") (sit-for 2)
		 (setq continue nil)
		 (setq res t))))
	     (t
;;;	      (message "m3-in-arg-list(104)") (sit-for 2)
	      (setq continue nil))))
	  res))

       (t nil)))))
	      


(defun m3-prev-line-incomplete-p (cur-point part-start)
;;;  (message "incomplete?") (sit-for 2)
  (or
   ;; Does the previous non-blank line end with an operator?
   (save-excursion
;;;     (message "incomplete-1") (sit-for 2)
     (goto-char cur-point)
     (m3-backward-to-code part-start)
     (or (looking-at "[+\\-*&#<,]")
	 (and (looking-at ">")
	      (save-excursion
		(beginning-of-line)
;;;		(message "incomplete-1.1") (sit-for 2)
		(not (looking-at
		      (concat "[ \t]*"
			      m3-handler-start-re
			      "[ \t]*\\($\\|(\\*\\)")))))
	 (and (looking-at "=")
	      (save-excursion
;;;		(message "incomplete-1.2") (sit-for 2)
		(beginning-of-line)
;;;		(message "incomplete-1.21") (sit-for 2)
		(and (not (looking-at
			   (concat "PROCEDURE.*=[ \t]*\\($\\|(\\*\\)")))
		     (not (m3-in-arg-list part-start)))))
		     
	 (and (> (point) 2)
	      (progn
		(forward-char -2)
		(or (looking-at
		     (concat m3-not-identifier-char-re "OR"))
		    (and
		     (> (point) 1)
		     (progn
		       (forward-char -1)
		       (looking-at
			(concat m3-not-identifier-char-re
				"\(DIV\\|MOD\\|AND\\|NOT")))))))))

   (save-excursion
     (goto-char cur-point)
     (m3-backward-to-code part-start)
     (forward-char 1)
;;;     (message "incomplete-1B1") (sit-for 2)
     (let ((last-char (point)))
       (beginning-of-line 1)
       (and (re-search-forward
	     (concat "^[ \t]*\\(" m3-statement-keywords "\\)")
	     cur-point t)
	    (= last-char (match-end 0)))))

   (save-excursion
;;;     (message "incomplete-2") (sit-for 2)
     (cond
      ((looking-at "END;")
;;;       (message "incomplete-2.01") (sit-for 2)
       (forward-char 4))
      ((looking-at
	(concat "END[ \t]*" m3-identifier-re "[ \t]*\\(;\\|\\.\\)"))
;;;       (message "incomplete-2.02") (sit-for 2)
       (re-search-forward
	(concat "END[ \t]*" m3-identifier-re "[ \t]*\\(;\\|\\.\\)")
	(point-max) t)
       (goto-char (match-end 0)))
      ((looking-at m3-multi-keyword-line-prefix)
;;;       (message "incomplete-2.1") (sit-for 2)
       (re-search-forward m3-multi-keyword-line-prefix (point-max) t)
       (goto-char (match-end 0)))

      ((looking-at "PROCEDURE")
;;;       (message "incomplete-2.15") (sit-for 2)
       (forward-word 1)
       (m3-re-search-forward "([^*]" (point-max) t)
       (let ((new-point (point)))
	 (save-excursion
	  (condition-case err
	      (forward-sexp 1)
	    (error (goto-char (point-max))))
;;;	  (message "incomplete-2.15-2") (sit-for 2)
	  (and (< (point) cur-point)
	       (m3-re-search-forward "=" (point-max) t)
	       (progn
		 (forward-char 1)
		 (and (< (point) cur-point)
;;;		      (message "incomplete-2.15-3") (sit-for 2)
		      (setq new-point (point))))))
	 (goto-char new-point)))

      ((looking-at "WITH")
;;;       (message "incomplete-2.191") (sit-for 2)
       (forward-word 1)
       (let ((new-point (point)))
	 (m3-re-search-forward "DO" first-code t)
;;;	 (message "incomplete-2.192") (sit-for 2)
	 (cond
	  ((looking-at "DO")
	   (forward-word 1)
;;;	   (message "incomplete-2.193") (sit-for 2)
	   (setq new-point (point))))
	 (goto-char new-point)))

      ((looking-at "END")
       (forward-word 1)
       (cond
	((save-excursion
	   (m3-forward-to-code (point-max))
	   (looking-at ";"))
	 (m3-forward-to-code (point-max))
	 (forward-char 1))))

      ;; If looking-at keyword-line-starter or part-starter
      ((looking-at (concat m3-keyword-line-starters "\\|" m3-part-starters))
;;;       (message "incomplete-2.2") (sit-for 2)
       (re-search-forward
	(concat m3-keyword-line-starters "\\|" m3-part-starters)
	(point-max) t)
       (goto-char (match-end 0)))

      ((looking-at ";")
       (forward-char 1)))

     ;; Go forward to code.
;;;     (message "m3-IFL: before codepoint") (sit-for 2)
     (m3-forward-to-code (point-max))
     ;; Is there something between the last ';' and the current
     ;; line?
;;;     (message "m3-IFL: codepoint") (sit-for 2)
     (and
      (< (point) cur-point)
      ;; Yes -- means that the previous statement was incomplete...

      ;; ...unless the current line is an ssl-ender, in which
      ;; case it is assumed complete...
;;;      (message "incomplete-3") (sit-for 2)
      (or (not
	   (save-excursion
	     (goto-char first-code)
;;;	     (message "incomplete-3.1") (sit-for 2)
	     (looking-at m3-keyword-ssl-enders)))
	  (save-excursion
;;;	    (message "incomplete-3.2") (sit-for 2)
	    (goto-char first-code)
	    (m3-backward-to-code part-start)
	    (forward-char 1)
;;;	    (message "incomplete-3.21") (sit-for 2)
	    (let ((after (point)))
	      (m3-re-search-backward m3-keyword-endable-ssl-introducers
				     part-start t)
	      (re-search-forward m3-keyword-endable-ssl-introducers
				 cur-point t)
	      (goto-char (match-end 0))
;;;	      (message "incomplete-3.22") (sit-for 2)
	      (= (point) after))))

      ;; ... or there is a an ssl-ender between here and first-code
      ;; that is not a semi in an argument list...
      (not (save-excursion
;;;	     (message "incomplete-3.3-0") (sit-for 2)
	     (and (m3-re-search-forward
		   (concat ";\\|" m3-keyword-ssl-enders)
		   first-code 't)
		  (let ((continue t))
		    (while (and continue (m3-in-arg-list part-start))
;;;		      (message "incomplete-3.3-1") (sit-for 2)
		      (re-search-forward
		       (concat ";\\|" m3-keyword-ssl-enders)
		       first-code 't)
		      (goto-char (match-end 0))
;;;		      (message "incomplete-3.3-2") (sit-for 2)
		      (setq continue
			    (m3-re-search-forward
			     (concat ";\\|" m3-keyword-ssl-enders)
			     first-code 't)))
		    continue)
;;;		  (message "incomplete-3.3") (sit-for 2)
		  (< (point) first-code))))

      ;; ... or the previous statement is a multi-keyword statement
      ;; and the current line is completed by a subsequent keyword...
      (not
       (save-excursion
	 (goto-char cur-point)
	 (m3-backward-to-non-comment-line-start part-start)
;;;	 (message "m3-indent-for-line: multi-keyword") (sit-for 2)
	 (looking-at m3-multi-keyword-lines)))
      ))))



;; Constants, especially helpful regexps.

(defconst m3-identifier-char-re "[a-zA-Z0-9_]")
(defconst m3-alpha-char-re "[a-zA-Z_]")
(defconst m3-not-identifier-char-re "[^a-zA-Z0-9_]")

(defconst m3-identifier-re
  (concat "\\b" m3-alpha-char-re m3-identifier-char-re "*\\b"))

(defconst m3-intlit-re "[1-9][0-9]*")

(defconst m3-poss-qual-ident-re
  (concat "\\(" "\\(" m3-identifier-re "\\.\\)?" m3-identifier-re "\\.\\)?"
	  m3-identifier-re))

(defconst m3-com-start-re "\\((\\*\\|<\\*\\)")
(defconst m3-com-end-re "\\(\\*)\\|\\*>\\)")
(defconst m3-com-start-or-end-re
  (concat "\\\(" m3-com-start-re "\\|" m3-com-end-re "\\)"))

(defconst m3-whitespace-char-re "[ \t]")
(defconst m3-poss-whitespace-re "[ \t]*")
(defconst m3-poss-whitespace-nl-re "[ \t\n]*")
(defconst m3-whitespace-line-re "^[ \t\n]*$")


(defconst m3-char-lit-re "'\\([^\\]\\|\\\\..?.?\\)'")

(defconst m3-range-re
  (concat m3-intlit-re m3-poss-whitespace-re "\\.\\."
	  m3-poss-whitespace-re m3-intlit-re))
  
  
(defconst m3-case-label-re
  (concat "\\(" m3-poss-qual-ident-re "\\|"
	  m3-char-lit-re "\\|"
	  m3-intlit-re "\\|"
	  m3-range-re
	  "\\)"))

(defconst m3-handler-start-re
  (concat "\\(|[ \t]*\\)?\\("
	  (concat "\\b" m3-poss-qual-ident-re m3-poss-whitespace-re
		  "(" m3-poss-whitespace-re m3-identifier-re
		  m3-poss-whitespace-re ")" )
	  "\\|"
	  (concat "\\b" m3-case-label-re
		  (concat "\\(" m3-poss-whitespace-re ","
			  m3-poss-whitespace-nl-re m3-case-label-re "\\)*"))
	  
	  "\\)" m3-poss-whitespace-re "=>"))

(defconst m3-object-re
  (concat "\\(" m3-identifier-re "[ \t]+\\)?\\(BRANDED[ \t]+"
	  "\\(\"[^\"]+\"\\)?[ \t]+\\)?OBJECT"))


(defconst m3-part-starters
  (concat
   "\\bINTERFACE\\b\\|\\bMODULE\\b\\|\\bIMPORT\\b\\|\\bFROM\\b\\|"
   "\\bTYPE\\b\\|\\bEXCEPTION\\b\\|\\bVAR\\b\\|"
   "\\bPROCEDURE\\b\\|\\bREVEAL\\b\\|\\bCONST\\b")
  "These are the patterns that can start lines and change the indentation
of the following line.")


(defconst m3-keyword-endable-ssl-introducers
  (concat
   "\\bTYPE\\b\\|\\bVAR\\b\\|"
   "\\bRECORD\\b\\|\\bOBJECT\\b\\|\\bMETHODS\\b\\|\\bOVERRIDES\\b\\|"
   "\\bBEGIN\\b\\|\\bTRY\\b\\|\\bEXCEPT\\b\\|"
   m3-handler-start-re "\\|"
   "\\bFINALLY\\b\\|\\bLOOP\\b\\|\\bTHEN\\b\\|\\bELSE\\b\\|\\bREPEAT\\b\\|"
   "\\bDO\\b\\|\\bOF\\b\\|\\bREVEAL\\b\\|\\bCONST\\b"))

;;; These keywords have the property that they affect the indentation if they
;;; occur at the beginning of a line.
(defconst m3-keyword-line-starters
  (concat
   "TYPE\\|\\bEND\\b\\|RECORD\\|PROCEDURE\\|OBJECT\\|METHODS\\|OVERRIDES\\|"
   "VAR\\|BEGIN\\|TRY\\|EXCEPT\\b\\|"
   m3-handler-start-re "\\|"
   "|\\|FINALLY\\|LOOP\\|THEN\\|ELSIF\\|IF\\|ELSE\\|WHILE\\|REPEAT\\|"
   "WITH\\|FOR\\b\\|DO\\|CASE\\|\\bOF\\b\\|TYPECASE\\|LOCK\\|CONST\\|FROM\\|"
   "REVEAL"))




(defconst m3-multi-keyword-line-prefix
  (concat
   "\\("
   ;; ...a PROCEDURE at the start of a line that ends
   ;; with an equals
   "^PROCEDURE[^\n]*=" "\\|"
   ;; ... or an IF or ELSEIF that ends with a THEN
   "\\(IF\\|ELSIF\\)[^\n]*THEN" "\\|"
   ;; ... or a WHILE, WITH, FOR, or LOCK that ends with a DO
   "\\(WHILE\\|WITH\\|FOR\\b\\|LOCK\\)[^\n]*DO" "\\|"
   ;; ... or a FOR that ends with a TO or BY
   "FOR[^\n]*\\(DO\\|BY\\)" "\\|"		  
   ;; ... or a CASE or TYPECASE that ends with a OF
   "\\(CASE\\|TYPECASE\\)[^\n]*OF" "\\|"
   ;; ... or at a handler-start that ends with a "=>"
   "\\(|\\|\\)[ \t]*" m3-handler-start-re
   "\\)"
   ))

(defconst m3-multi-keyword-lines
  (concat m3-multi-keyword-line-prefix 
	  "[ \t]*\\($\\|(\\*\\)"))


(defconst m3-statement-starters
  (concat
   "BEGIN\\b\\|TRY\\b\\|LOOP\\b\\|IF\\b\\|WHILE\\b\\|REPEAT\\b\\|"
   "WITH\\\b\\|FOR\\b\\|CASE\\b\\|TYPECASE\\b\\|LOCK\\b")
  
  "These are the patterns that can start lines and change the indentation
of the following line.")



(defconst m3-keyword-ssl-enders
  "|\\|EXCEPT\\|FINALLY\\|ELSIF\\|ELSE\\|UNTIL\\|END")

(defconst m3-left-parens
  "\\((\\|\\[\\|{\\)")
(defconst m3-right-parens
  "\\()\\|\\]\\|}\\)")

;;; Think of a more descriptive name for these...

(defconst m3-statement-keywords
  "RETURN\\|RAISE\\|EXCEPTION\\|IMPORT\\|WITH")


;; Variables that control indentation behavior

(defvar m3-standard-offset 2)
(defvar m3-continued-line-offset 2)
(defvar m3-case-offset 0)
;;;(setq m3-case-offset 2)
(defvar m3-open-paren-offset 4)
;;;(setq m3-open-paren-offset 2)
(defvar m3-assign-offset 4)
(defvar m3-RAISES-offset 4)

(defvar m3-follow-continued-indent t)

(defvar m3-END-undent 2)
(defvar m3-METHODS-undent 2)
(defvar m3-OVERRIDES-undent 2)
(defvar m3-EXCEPT-undent 2)
(defvar m3-VERT-undent 2)
(defvar m3-handler-start-undent 0)
(defvar m3-EXCEPT-undent 2)
(defvar m3-UNTIL-undent 2)
(defvar m3-FINALLY-undent 2)
(defvar m3-ELSIF-undent 2)
(defvar m3-ELSE-undent 2)

(defvar m3-DO-undent 1)
(defvar m3-OF-undent 1)
(defvar m3-THEN-undent 1)

(defvar m3-OBJECT-undent 1)
(defvar m3-RECORD-undent 1)



(defun m3-after-keyword-adjust-indent (indent first-code part-start)
  "Point is looking at a keyword at column INDENT; if the current line has
any code it starts at FIRST-CODE.  Return the proper indentation for the
current line."
;;;  (message "m3-after-keyword: indent = %d" indent) (sit-for 2)
  (let ((call-adjust-indent t))
    (cond
     ((looking-at "END")
;;;    (message "m3-after-keyword(END): i: %d, m3-END: %d, m3-stand: %d"
;;;	     indent m3-END-undent m3-standard-offset)
;;;    (sit-for 2)
      (setq indent (- (+ indent m3-END-undent) m3-standard-offset)))

     ((looking-at "ELSE")
      (setq indent (+ indent m3-ELSE-undent))
      (if (m3-in-case part-start)
	  (setq indent (+ indent m3-case-offset))))
    

     ((looking-at "METHODS")
      (setq indent (+ indent m3-METHODS-undent)))
     ((looking-at "OVERRIDES")
      (setq indent (+ indent m3-OVERRIDES-undent)))
     ((looking-at "EXCEPT\\b")
;;;    (message "m3-after-keyword: EXCEPT" indent) (sit-for 2)
      (setq indent (+ indent m3-EXCEPT-undent)))
     ((looking-at "|")
;;;    (message "m3-after-keyword: vert" indent) (sit-for 2)
      (setq indent (+ indent m3-VERT-undent m3-case-offset)))
     ((looking-at m3-handler-start-re)
;;;      (message "m3-after-keyword: handler-start" indent) (sit-for 2)
      (setq indent (+ indent m3-handler-start-undent m3-case-offset)))
     ((looking-at "FINALLY")
      (setq indent (+ indent m3-FINALLY-undent)))
     ((looking-at "THEN")
      (setq indent (+ indent m3-THEN-undent)))
     ((looking-at "ELSIF")
      (setq indent (+ indent m3-ELSIF-undent)))
     ((looking-at "ELSE")
      (setq indent (+ indent m3-ELSE-undent)))
     ((looking-at "DO")
      (setq indent (+ indent m3-DO-undent)))
     ((looking-at "OF")
      (setq indent (+ indent m3-OF-undent)))
     ((looking-at m3-object-re)
      (setq indent (+ indent m3-OBJECT-undent)))
     ((looking-at "RECORD")
      (setq indent (+ indent m3-RECORD-undent)))

     ;; These are the keywords that can be followed by an SSL that begins on
     ;; the same line -- if so, indent to the level of the first elem.
     ((looking-at m3-same-line-ssl-keywords)
;;;      (message "m3-after-keyword: same-line-ssl") (sit-for 2)
      (let ((eol (save-excursion (end-of-line 1) (point))))
	(save-excursion
	  (forward-word 1)
	  (m3-forward-to-code (point-max))
;;;	  (message "m3-after-keyword: SlSSL(2)") (sit-for 2)
	  (cond
	   ((and
	     m3-follow-continued-indent
	     (<= (point) eol)
	     (save-excursion
	       (goto-char first-code)
	       (not (looking-at (concat m3-part-starters "\\|BEGIN"))))
	     (save-excursion
	       (end-of-line 1)
	       (m3-backward-to-code part-start)
	       (looking-at ";")))
;;;	    (message "m3-after-keyword: SLSSL (3)") (sit-for 2)
	    (setq indent (current-column))
	    (setq call-adjust-indent nil))
	   (t
	    (setq indent (+ indent m3-standard-offset)))))))

     ;; These are all the keywords that don't affect the indentation
     ;; when they start complete lines.
     ((looking-at
       (concat "INTERFACE\\|MODULE\\|IMPORT\\|FROM\\|EXCEPTION"))
;;;    (message "m3-after-keyword: no extra") (sit-for 2)
      indent)

     ;; Otherwise, give the standard indentation.
     (t
;;;    (message "m3-after-keyword: standard") (sit-for 2)
      (setq indent (+ indent m3-standard-offset))))
	
    (cond
     (call-adjust-indent
      (save-excursion
	(goto-char first-code)
;;;	(message "m3-after-keyword: calling complete-adjust") (sit-for 2)
	(m3-complete-adjust-indent indent first-code part-start)))
     (t
;;;      (message "m3-after-keyword: not calling complete-adjust") (sit-for 2)
      indent))))


(defun m3-in-case (part-start)
;;;  (message "M3-in-case") (sit-for 2)
  (save-excursion
    (let ((cur-point (point)))
      (m3-backward-to-end-match part-start)
;;;      (message "M3-in-case(2)") (sit-for 2)
      (and
       (looking-at m3-case-starters)
       (progn
	 (cond
	  ((looking-at "TRY")
	   ;; Is it a TRY-FINALLY or a TRY-EXCEPT?
	   (let (res (continue t))
	     (while continue
	       (setq res (m3-re-search-forward "TRY\\|EXCEPT\\|FINALLY"
					     cur-point t))
	       (cond
		((looking-at "EXCEPT")
		 (setq continue nil))
		((looking-at "TRY")
		 ;; Go to matchine END and try again
		 (m3-forward-to-end-matcher cur-point))
		(t;; FINALLY or not found
		 (setq res nil)
		 (setq continue nil))))
	     res))
	  (t t)))
       ;;; We are now looking at a case starter.  Make sure there is
       ;;; at least one case arm starter.
       (progn
	 (cond
	  ((looking-at "EXCEPT") (forward-word 1))
	  ((looking-at "CASE\\|TYPECASE")
	   (forward-word 1)
	   (m3-re-search-forward "OF" cur-point 'move-to-limit)
	   (forward-word 1)))
	 (m3-forward-to-code cur-point)
;;;	 (message "M3-in-case: about to test handler") (sit-for 2)
	 (and (< (point) cur-point)
	      (looking-at m3-handler-start-re)))

;;;       (message "M3-in-case: returning t") (sit-for 2)
       ))))

	 
(defun m3-in-continued-record-def (part-start)
  (if (not (looking-at "END"))
      (error "m3-in-continued-record-def assumes looking-at END"))
  (save-excursion
    (m3-backward-to-end-match part-start)
    (let ((end-match (point)) (eol (save-excursion (end-of-line) (point))))
      (beginning-of-line)
      (or (save-excursion
	    (re-search-forward "[ \t]*" eol t)
	    (= (point) end-match))
	  (save-excursion
	    (and
	     (re-search-forward "[ \t]*BRANDED[ \t]+" eol t)
	     (= (point) end-match)
	     (save-excursion
	       (goto-char end-match)
	       (looking-at "OBJECT"))))))))

	 
(defun m3-correct-for-trailing-ends (indent part-start)
  ;; If the previous line ends in a (series of) END(s) that does
  ;; (do) not start the line, and are unmatched by the start of the line,
  ;; subtract the END-undent(s) from indent (the Eric Muller convention.)
;;;  (message "correct-for-trailing-ends in: %d" indent) (sit-for 2)
  (let ((prev-line-start
	 (save-excursion
	   (m3-backward-to-code part-start)
	   (beginning-of-line)
	   (m3-forward-to-code (point-max))
;;;	   (message "correct-for-trailing-ends (0)") (sit-for 2)
	   (point))))
    (save-excursion
      (if (save-excursion
	    (m3-backward-to-code part-start)
	    (beginning-of-line)
	    (not (looking-at "[ \t]*END")))
	  (save-excursion
	    (let ((continue t))
	      (while continue
		(m3-backward-to-code part-start)
;;;		(message "correct-for-trailing-ends (2)") (sit-for 2)
		(cond
		 ((or (and (> (point) 2)
			   (progn
			     (forward-char -2) (looking-at "END")))
		      (and (> (point) 1)
			   (progn
			     (forward-char -1) (looking-at "END;"))))
;;;		  (message "correct-for-trailing-ends (3)") (sit-for 2)
		  (if (not (looking-at "END"))
		      (error "m3-complete-adjust-indent(A)"))
		  (let ((em-point
			 (save-excursion
			   (m3-backward-to-end-match part-start)
;;;			   (message "correct-for-trailing-ends EM") (sit-for 2)
			   (point))))
;;;		    (message "xxx") (sit-for 2)
		    (cond
		      ((< em-point prev-line-start)
		       (goto-char prev-line-start)
;;;		       (message "xxx<") (sit-for 2)
		       (setq indent
			     (save-excursion (goto-char em-point)
					     (current-column))))
		      ((= em-point prev-line-start)
;;;		       (message "xxx=") (sit-for 2)
		       (setq indent (- indent m3-END-undent))
		       (setq continue nil))
		      ((> em-point prev-line-start)
		       (goto-char em-point)))))
		 (t
		  (setq continue nil))))))))
;;;    (message "m3-trailing-end returns %d" indent) (sit-for 2)
    indent))
     

(defun m3-complete-adjust-indent (indent first-code part-start)
  "Previous statement is complete and starts at column INDENT;
if the current line has any code it starts at FIRST-CODE.  Returns the
proper indentation for the current line."
;;;  (message "m3-complete-adjust(A): indent = %d, first-code = %d"
;;;	   indent first-code)
;;;  (sit-for 2)
  (save-excursion
    (goto-char first-code)
;;;    (message "m3-complete-adjust(B)") (sit-for 2)

    ;; If the previous line ends in a (series of) END(s) that does
    ;; (do) not start the line, and are unmatched before the start of the line,
    ;; the END-undent(s) (the Eric Muller convention.)
    (setq indent (m3-correct-for-trailing-ends indent part-start))
		  
;;;    (message "yyy2: indent = %d" indent) (sit-for 2)
    (cond
     ;; Some things can only start parts, and must be on the left margin.
     ((looking-at (concat "TYPE\\b\\|REVEAL\\b\\|EXCEPTION\\b\\|"
			  "FROM\\b\\|IMPORT\\b"))
      0)
      
     ;; These can start parts, but can also appear in the procedures.
     ((looking-at
       (concat "\\(PROCEDURE\\b\\|CONST\\b\\|VAR\\b\\|BEGIN\\b\\)"))
      ;; Look backwards for line-beginning-keywords that increase the
      ;; indentation, start an SSL, but don't require an END (i.e.,
      ;; TYPE, VAR, or CONST); or END's.  If the former is found first,
      ;; decrease the indentation to the same as the keyword line's.
      ;; If an END is found whose matcher is not something that can
      ;; occur in a TYPE, VAR, or CONST (i.e. RECORD or OBJECT),
      ;; indent normally.
;;;      (message "yyy7") (sit-for 2)
      (let ((new-indent indent) (continue t))
	(while continue
;;;	  (message "xxx1") (sit-for 2)
	  (m3-re-search-backward
	   (concat "\\(^[ \t]*\\(" m3-same-line-ssl-keywords "\\)\\|END\\|"
		   m3-statement-starters "\\)")
	   part-start 'move-to-limit)
;;;	  (message "xxx2") (sit-for 2)
	  (cond
	   ;; If we reached the part-start because of the move-to-limit,
	   ;; indent to here...
	   ((looking-at (concat "^" m3-part-starters))
;;;	    (message "xxx2.5") (sit-for 2)
	    (goto-char first-code)
	    ;; If its the start of a procedure def, indent normally.
	    ;; Otherwise, indent to left margin.
	    (if (not (m3-after-procedure-introducer part-start))
		(setq new-indent 0))
	    (setq continue nil))
	      
	   ((and
	     (looking-at
	      (concat "^[ \t]*\\(" m3-same-line-ssl-keywords "\\)"))
	     (not (m3-in-arg-list part-start)))
	    (setq continue nil)

	    ;;; To accomodate part-starters that establish new indentations,
	    ;;; indent to the level of the previous part-starter, unless
	    ;;; that was a BEGIN.
	    (goto-char first-code)
	    (m3-re-search-backward
	     (concat m3-part-starters "\\|BEGIN") part-start t)
	    (while (m3-in-arg-list part-start)
	      (m3-re-search-backward
	       (concat m3-part-starters "\\|BEGIN") part-start t))
;;;	    (message "xxx3") (sit-for 2)
	    (cond
	     ((looking-at "BEGIN")
	      (setq new-indent (- new-indent m3-standard-offset)))
	     (t
	      (setq new-indent (current-column)))))
	     
	   ((looking-at
	     (concat "END[ \t]*" m3-identifier-re "[ \t]*;"))
	    (setq continue nil)
	    (setq new-indent (- new-indent m3-standard-offset)))


	   ((looking-at "END")
	    (m3-backward-to-end-match part-start)
;;;	    (message "xxxEND-match") (sit-for 2)
	    (cond
	     ((looking-at "\\(RECORD\\|OBJECT\\)")
	      nil)
	     (t
	      (setq continue nil))))

	   (t
	    (setq continue nil))))
	new-indent))

     ;; If the current line is an END, add the END-undent.
     ((looking-at "END")
;;;      (message "zzz1") (sit-for 2)
      (cond
       ((m3-in-case part-start)
	(- indent m3-END-undent m3-case-offset))
       (t
	(- indent m3-END-undent))))


     ((looking-at "ELSE")
      (- indent m3-ELSE-undent
	 (if (m3-in-case part-start) m3-case-offset 0)))

     ((looking-at "METHODS")
      (- indent m3-METHODS-undent))
     ((looking-at "OVERRIDES")
      (- indent m3-OVERRIDES-undent))
     ((looking-at "EXCEPT")
      (- indent m3-EXCEPT-undent))
     ((looking-at "UNTIL")
      (- indent m3-UNTIL-undent))
     ((looking-at "|")
      (cond
       ((save-excursion
	  (m3-backward-to-code part-start)
;;;	  (message "zzz2") (sit-for 2)
	  (or
	   (save-excursion
	     (and (> (point) 1)
		  (progn (forward-char -1) (looking-at "OF"))))
	   (save-excursion
	     (and (> (point) 5)
		  (progn (forward-char -5) (looking-at "EXCEPT"))))))
	(- indent m3-VERT-undent))
       (t
	(- indent m3-VERT-undent m3-case-offset))))

     ((looking-at "FINALLY")
      (- indent m3-FINALLY-undent))
     ((looking-at "THEN")
      (- indent m3-THEN-undent))
     ((looking-at "ELSIF")
      (- indent m3-ELSIF-undent))
     ((looking-at "ELSE")
      (- indent m3-ELSE-undent))
     ((looking-at "DO")
      (- indent m3-DO-undent))
     ((looking-at "OF")
      (- indent m3-OF-undent))
     ((looking-at "RECORD")
;;;      (message "zzz-record") (sit-for 2)
      (- indent m3-RECORD-undent))
     ((looking-at m3-object-re)
;;;      (message "zzz-object") (sit-for 2)
      (- indent m3-OBJECT-undent))
     (t
;;;      (message "zzz-t: indent = %d" indent) (sit-for 2)
      indent))))
  

(defun m3-incomplete-indent (cur-point first-code part-start)
  (let* (list-indent
	 (prev-line-start
	  (save-excursion
	    (m3-backward-to-non-comment-line-start part-start)
	    (point)))
	 (last-char-prev-line
	  (save-excursion
	    (m3-backward-to-non-comment-line-start part-start)
	    (end-of-line)
	    (m3-backward-to-code
	     (save-excursion (beginning-of-line) (point)))
	    (point)))
	 (prev-line-indent
	  (save-excursion
	    (m3-backward-to-non-comment-line-start part-start)
	    (let ((pli (current-column)))
	      (cond
	       ((looking-at m3-statement-keywords)
		(forward-word 1)
		(m3-forward-to-code first-code)
		(cond
		 ((<= (point) last-char-prev-line)
		  (current-column))
		 (t pli)))
	       (t pli))))))
;;;    (message "m3-incomplete-indent(A)") (sit-for 2)
    (cond
     ;; Did the previous non-blank line end with a paren?
     ((save-excursion
	(goto-char last-char-prev-line)
	(looking-at m3-left-parens))

;;;      (message "m3-incomplete-indent(PAREN)") (sit-for 2)
      ;;   Find the indentation of the previous line,
      ;;     either add open-paren-offset, or indent of paren +
      ;;     open-paren-sep
      (goto-char last-char-prev-line)
      (cond
       (m3-open-paren-offset
;;;	(message "m3-incomplete-indent(PAREN offset)") (sit-for 2)
	(re-search-backward
	 (concat m3-identifier-re m3-poss-whitespace-re)
	 part-start t)
	(goto-char (match-beginning 0))
	;; Account for qualified names.
	(cond
	 ((save-excursion
	    (and (> (point) 1)
		 (progn
		   (forward-char -1)
		   (looking-at "\\."))))
	  (re-search-backward
	   (concat m3-identifier-re m3-poss-whitespace-re)
	   part-start t)
	  (goto-char (match-beginning 0))))

;;;	(message "m3-incomplete-indent(PAREN offset 2)") (sit-for 2)
	(+ (current-column) m3-open-paren-offset))
       (t
	(+ (current-column) m3-open-paren-sep))))
		
     ;; Did the previous line end with a ',' or ';'?:
     ((save-excursion
	(goto-char last-char-prev-line)
	(looking-at ",\\|;"))

;;;      (message "m3-incomplete-indent(COMMA)") (sit-for 2)
      ;; Skip over any matched parens; if this puts us at a line
      ;; containing an unmatched left paren, indent to that +
      ;; paren-sep.  Otherwise, indent same as beginning of that line.
      (save-excursion
	(goto-char last-char-prev-line)
	(let ((continue t) res)
	  (while continue
;;;	    (message "m3-incomplete-indent(COMMA) 0") (sit-for 2)
	    (m3-re-search-backward
	     (concat m3-left-parens "\\|" m3-right-parens)
	     (save-excursion (beginning-of-line)
			     (point)) 'move-to-limit)
;;;	    (message "m3-incomplete-indent(COMMA) 1") (sit-for 2)
	    (cond
	     ((looking-at m3-left-parens)
;;;	      (message "m3-incomplete-indent(COMMA) lp") (sit-for 2)
	      (setq continue nil)
	      (forward-char 1)
	      (re-search-forward "[ \t]*") (goto-char (match-end 0))
	      (setq list-indent (current-column)))
	     ((looking-at m3-right-parens)
;;;	      (message "m3-incomplete-indent(COMMA) rp") (sit-for 2)
	      (forward-char 1)
	      (backward-sexp 1))
	     (t
;;;	      (message "m3-incomplete-indent(COMMA) none") (sit-for 2)
	      (beginning-of-line)
	      (m3-forward-to-code last-char-prev-line)
	      (setq continue nil)
	      (setq list-indent (current-column)))))
;;;	  (message "m3-incomplete-indent(COMMA) end") (sit-for 2)
	  (cond
	   ((looking-at (concat "|[ \t]*" m3-identifier-char-re))
	    (forward-word 1) (forward-word -1)
	    (setq list-indent (current-column)))
	   ((looking-at m3-statement-keywords)
	    (forward-word 1)
	    (re-search-forward "[ \t]*" last-char-prev-line t)
	    (setq list-indent (current-column))))))
      list-indent)
	      
     ;; Did the previous non-blank line end a procedure header?
     ((m3-after-procedure-introducer part-start)
;;;      (message "m3-incomplete-indent(PROCEDURE)") (sit-for 2)
      (goto-char last-char-prev-line)
      (m3-re-search-backward "PROCEDURE" part-start t)
      (+ (current-column) m3-standard-offset))

     ;; Does the current line start a RAISES clause?
     ((looking-at "^[ \t]*RAISES")
;;;      (message "m3-incomplete-indent(RAISES)") (sit-for 2)
      (goto-char last-char-prev-line)
      (m3-re-search-backward "PROCEDURE" part-start t)
      (+ (current-column) m3-RAISES-offset))

     ;; Did the previous line end with an assignment?
     ((save-excursion
	(goto-char last-char-prev-line)
	(beginning-of-line)
;;;	(message "m3-incomplete-indent(:= 1)") (sit-for 2)
	(and (m3-re-search-forward ":=" (1+ last-char-prev-line) t)
	     (re-search-forward "[^ \t]" last-char-prev-line t)))
;;;      (message "m3-incomplete-indent(:=)") (sit-for 2)
      (goto-char last-char-prev-line)
      (beginning-of-line)
      (m3-re-search-forward ":=" last-char-prev-line t)
      (forward-char 2)
      (re-search-forward "[ \t]*[^ \t]")
      (+ (- (current-column) 1) m3-assign-offset))

     ;; Otherwise:
     (t
;;;      (message "m3-incomplete-indent(OTHER)") (sit-for 2)
      ;; Find out if the previous line begins the statement.
      (goto-char prev-line-start)
      (m3-re-search-backward
       (concat ";\\|" m3-keyword-line-starters "\\|" m3-part-starters
	       "\\|" m3-statement-keywords)
       part-start t)
      (while (m3-in-arg-list part-start)
	(m3-re-search-backward
	 (concat ";\\|" m3-keyword-line-starters "\\|" m3-part-starters
		 "\\|" m3-statement-keywords)
	 part-start t))
;;;      (message "m3-incomplete-indent(OTHER1)") (sit-for 2)
      (if (or (> (point) part-start)
	      (and (= (point) part-start)
		   (looking-at m3-keyword-endable-ssl-introducers)))
	  (progn
	    (re-search-forward
	     (concat ";\\|" m3-keyword-line-starters "\\|" m3-part-starters
		     "\\|" m3-statement-keywords)
	     cur-point t)
	    (goto-char (match-end 0))))
;;;      (message "m3-incomplete-indent(OTHER1.5)") (sit-for 2)
      (m3-forward-to-code (point-max))
;;;      (message "m3-incomplete-indent(OTHER2), prev-line-start = %d"
;;;	       prev-line-start)
;;;      (sit-for 2)
      (cond
       ;; If the previous line begins the statement, add
       ;; m3-standard-offset to indentation, unless the prev-line-indent
       ;; has already skipped over a keyword.
       ((= (point) prev-line-start)
;;;	(message "m3-incomplete-indent(START): prev-line-indent = %d"
;;;		 prev-line-indent)
;;;	(sit-for 2)
	(m3-complete-adjust-indent
	 ;; Indent further if we haven't indented already.
	 (cond
	  ((= prev-line-indent
	      (save-excursion (goto-char prev-line-start) (current-column)))
	   (+ prev-line-indent m3-continued-line-offset))
	  (t prev-line-indent))
	 first-code part-start))
       (t
;;;	(message "m3-incomplete-indent(CONT)") (sit-for 2)
	;; Otherwise, same indentation as previous, modulo adjustment
	;; for current line
	prev-line-indent))))))


(defun m3-after-procedure-introducer (part-start)
  "Returns t iff first non-blank non-comment character before point is the '='
of a procedure definition."
  (save-excursion
    (m3-backward-to-code part-start)
    (and
     (looking-at "=")
;;;     (message "m3-API(0)") (sit-for 2)
     (let ((eq-point (point)))
       (and
	;; Not that this does not allow any comments in
	;;   PROCEDURE Foo <left-paren>
	;; and all must occur on the same line.
	(m3-re-search-backward
	 (concat "PROCEDURE[ \t]*" m3-identifier-re "[ \t]*(")
	 part-start t)
;;;	(message "m3-API(1)") (sit-for 2)
	(progn
	  (re-search-forward
	   (concat "PROCEDURE[ \t]*" m3-identifier-re "[ \t]*(")
	   eq-point t)
	  (goto-char (match-end 0))
;;;	  (message "m3-API(2)") (sit-for 2)
	  (forward-char -1)
	  (and
	   (condition-case err
	       (progn (forward-sexp 1) t)
	     (error nil))
;;;	   (message "m3-API(3)") (sit-for 2)
	   ;; We should now be at the right paren of the arg-list.
	   ;; Check for a return type.
	   (progn
	     (m3-forward-to-code eq-point)
	     (and
;;;	      (message "m3-API(4)") (sit-for 2)
	      (cond
	       ((looking-at ":")
		(forward-char 1)
		(m3-forward-to-code eq-point)
		(and
		 (looking-at m3-poss-qual-ident-re)
		 (progn
		   (re-search-forward m3-poss-qual-ident-re eq-point t)
		   (goto-char (match-end 0))
		   (m3-forward-to-code eq-point)
		   t)))
	       (t t))
	      ;; Now check for RAISES clause.
;;;	      (message "m3-API(5)") (sit-for 2)
	      (cond
	       ((looking-at "RAISES")
		(forward-word 1)
		(m3-forward-to-code eq-point)
		(cond
		 ((looking-at "ANY")
		  (forward-word 1)
		  (m3-forward-to-code eq-point)
		  t)
		 ((looking-at "{")
;;;		  (message "m3-API(5.5)") (sit-for 2)
		  (and
		   (condition-case err
		       (progn (forward-sexp 1) t)
		     (error nil))
		   (progn (m3-forward-to-code eq-point) t)))
		 (t t)))
	       (t t))

	      ;; Now, we better be back to the original =!
	      (= (point) eq-point))))))))))


(defconst m3-end-matchers
  (concat
   "\\bRECORD\\b\\|\\bOBJECT\\b\\|\\bBEGIN\\b\\|\\bTRY\\b\\|\\bLOOP\\b\\|"
   "\\bIF\\b\\|\\bWHILE\\b\\|\\bWITH\\b\\|\\bFOR\\b\\|\\bCASE\\b\\|"
   "\\bTYPECASE\\b\\|\\bLOCK\\b\\|\\bINTERFACE\\b\\|\\bMODULE\\b\\|"
   "\\bGENERIC\\b"))


(defconst m3-same-line-ssl-keywords
  "\\bVAR\\b\\|\\bTYPE\\b\\|\\bCONST\\b\\|\\bEXCEPTION\\b\\|\\bREVEAL\\b"
  "These are the keywords that can be followed by an SSL that begins on
the same line -- if so, indent to the level of the first elem.")

(defconst m3-case-starters
  "TRY\\|CASE\\|TYPECASE")



(defun m3-backward-to-end-match (part-start &optional depth)
  (if (not depth) (setq depth 0))
  (let (res (continue t))
    (while continue
;;;      (message "m3-backward-to-end-match(1) [%d]" depth) (sit-for 1)
      (setq res (m3-re-search-backward
		 (concat "\\(" m3-end-matchers "\\|END\\)") part-start t))
      (cond
       ((and res (looking-at "END"))
	(m3-backward-to-end-match part-start (1+ depth)))
       (t
	(setq continue nil))))
    res))

(defun m3-forward-to-end-matcher (max-point)
  (let (res (continue t))
    (while continue
      (setq res (m3-re-search-forward
		 (concat "\\(" m3-statement-starters "\\|END\\)") max-point t))
      (cond
       ((looking-at m3-statement-starters)
	(re-search-forward m3-statement-starters max-point t)
	(goto-char (match-end 0))
	(m3-forward-to-end-matcher max-point))
       (t   ;; looking at END or reached max-point
	(setq continue nil))))
    res))


(defun m3-backward-to-non-comment-line-start (part-start)
  "Sets the point at the first non-whitespace character in a line that
contains something other than comments and/or whitespace."
  (m3-backward-to-code part-start)
  (beginning-of-line)
  (m3-skip-whitespace-in-line))


(defun m3-skip-whitespace-in-line ()
  (re-search-forward "[ \t]*"))


(defun m3-indent-to (cur-point new-column)
  "Make current line indentation NEW-COLUMN.  If the point is to the
left of the first non-blank character, move it to NEW-COLUMN.
Otherwise, maintain its relative position.  Has the side effect
of converting tabs to spaces."
  (goto-char cur-point)
  (untabify (save-excursion (beginning-of-line) (point))
	    (save-excursion (end-of-line) (point)))
  (let ((cur-column (current-column))
	(cur-point (point))
	(first-column
	 (save-excursion
	   (beginning-of-line)
	   (re-search-forward " *")
	   (current-column))))
    (let ((diff (- new-column first-column)))
      (cond
       ((> diff 0)
	(beginning-of-line)
	;; Must do this to make sure the keyword completion marker moves
	;; correctly.
	(let ((d diff))
	  (while (> d 0)
	    (insert-before-markers " ") (setq d (1- d))))
	)
       ((< diff 0)
	(save-excursion
	  (forward-char (- first-column cur-column))
	  (backward-delete-char-untabify (- diff)))))
      (cond
       ((> first-column cur-column)
	(beginning-of-line)
	(forward-char new-column))
       (t
	(goto-char (+ cur-point diff)))))))


(defun m3-in-comment-or-string ()
  "Returns 'string if point is in an unterminated string, 'comment if in
an unterminated comment, otherwise, nil."
  (save-excursion
    (beginning-of-line)
    (let ((cur-point (point))
	  (state nil))
      (save-excursion
	;; We assume the lisp-like convention that "top-level defuns,"
	;; or "parts", are the only things that occur on the left
	;; margin (we make an exception for end-comments.)
	(m3-backward-to-last-part-begin)
	(while (and (not state)
		    (re-search-forward
		     (concat "\\(" m3-com-start-re "\\|\"\\)")
		     cur-point t))
	  (goto-char (match-beginning 0))
	  (cond
	   ((looking-at m3-com-start-re)
	    (setq state 'comment)
	    (if (m3-skip-comment-forward cur-point t) (setq state nil)))
	   ((looking-at "\"\\|'")
	    (setq state 'string)
	    (if (re-search-forward "[^\\\\]\\(\"\\|'\\)" cur-point t)
		(setq state nil)))))
	state))))

(defun m3-backward-to-last-part-begin ()
  (beginning-of-line nil)
  (if (re-search-backward
       (concat "^\\(" m3-com-start-re "\\|" m3-part-starters "\\)")
       (point-min) t)
      (progn
	(goto-char (match-beginning 0)))
    (goto-char (point-min))))

(defun m3-forward-to-code (max-point)
  "Sets the point at the first non-comment, non-whitespace character
following the current point, else at max-point."
;;;  (message "m3-forward-to-code (1)") (sit-for 2)
  (let ((continue t))
    (while continue
;;;      (message "m3-forward-to-code (1.5)") (sit-for 2)
      (setq continue
	    (and (re-search-forward "[^ \t\n]" max-point 'move-to-limit)
		 (progn (goto-char (match-beginning 0))
;;;			(message "m3-forward-to-code (2)") (sit-for 2)
			(and (looking-at m3-com-start-re)
			     (m3-skip-comment-forward max-point t))))))))


(defun m3-backward-to-code (min-point)
  "Sets the point at the first non-comment, non-whitespace character
before the current point, else at end-of-file"
  (interactive "n")
  (let ((continue t))
    (while continue
      (if (re-search-backward "[^ \t\n][ \t\n]*" min-point t)
	  (goto-char (match-beginning 0)))
      (setq continue (and (save-excursion
			    (and (> (point) 1)
				 (progn
				   (forward-char -1)
				   (looking-at m3-com-end-re))))
			  (progn
			    (forward-char 1)
			    (m3-skip-comment-backward min-point t)))))

    t))

(defun m3-re-search-forward (re max-point fail)
  "Assumes we're not in a comment.  Puts point at the start of the
first occurence of RE that is not in a comment, if such an occurence
occurs before MAX-POINT, and returns non-nil.  Otherwise, returns nil
and leaves point unaffected.  Results are undefined if RE matches any
comment starter."
  (let ((continue t)
	(save-point (point))
	(res nil))
    (while continue
      (setq res (re-search-forward
		  (concat "\\(" m3-com-start-re "\\|" re "\\)")
		  max-point fail))
      (goto-char (match-beginning 0))
      (cond
       (res
	(cond
	 ((looking-at m3-com-start-re)
	  (m3-skip-comment-forward max-point fail))
	 (t
	  (setq continue nil))))
       (t
	(setq continue nil)
	(if (and (eq fail t) (not res))
	    (goto-char save-point)))))
    res))
	

(defun m3-re-search-backward (re min-point fail)
  "Assumes we're not in a comment.  Puts point the start of the
first previous occurence of RE that is not in a comment, if such an occurence
occurs before MIN-POINT, and returns non-nil.  FAIL is interpreted as is third
argument to re-search.  Results are undefined if RE matches any comment
starter." 
  (let ((continue t)
	(save-point (point))
	(res nil))
    (while continue
      (setq res (re-search-backward
		 (concat "\\(" m3-com-end-re "\\|" re "\\)") min-point fail))
      (cond
       (res
	(cond
	 ((looking-at m3-com-end-re)
	  (forward-char 2)
	  (m3-skip-comment-backward min-point fail))
	 (t
	  (setq continue nil))))
       (t
	(setq continue nil)
	(if (and (eq fail t) (not res))
	    (goto-char save-point)))))
    res))

(defun m3-skip-comment-forward (max-point fail)
  "Requires that point is at the start of a comment.  If that comment
is terminated before MAX-POINT, return t and leaves point after end of
the comment.  Otherwise, if fail is 't, returns returns nil and leaves
the point unchanged; if fail is nil raises an errer; if fail is not t or nil,
returns nil and leaves the point at max-point or (point-max), whichever is
smaller."
  (if (not (looking-at m3-com-start-re))
      (error
       "m3-skip-comment-forward should only be called when looking at
comment-starter"))
  (forward-char 2)
  (let ((save-point (point)) (continue t) res)
    (while continue
;;;      (message "m3-comment-forward (0.5)") (sit-for 2)
      (setq res (re-search-forward m3-com-start-or-end-re max-point fail))
      (cond
       (res
;;;	(message "m3-comment-forward (1)") (sit-for 2)
	(goto-char (match-beginning 0))
;;;	(message "m3-comment-forward (2)") (sit-for 2)
	(cond
	 ((looking-at m3-com-start-re)
	  (if (not (m3-skip-comment-forward max-point fail))
	      (progn (setq res nil)
		     (setq continue nil))))
	 ((looking-at m3-com-end-re)
	  (goto-char (match-end 0))
	  (setq continue nil))
	 (t
;;;	  (message "m3-comment-forward (4)") (sit-for 2)
	  (goto-char save-point)
	  (setq res nil)
	  (setq continue nil))))
       (t 
;;;	(message "m3-comment-forward (5)") (sit-for 2)
	(goto-char save-point)
	(setq res nil)
	(setq continue nil))))
    res))


(defun m3-skip-comment-backward (min-point fail)
  "Requires that point is at the end of a comment.  If that comment
is terminated before MIN-POINT, return t and leaves point at the start
the comment.  Otherwise returns nil and leaves the point in an
unspecified position."
  (forward-char -2)
  (if (not (looking-at m3-com-end-re))
      (error
       "m3-skip-comment-backward should only be called when looking at
comment-ender"))
  (let ((save-point (point)) (continue t) res)
    (while continue
      (setq res (re-search-backward m3-com-start-or-end-re min-point fail))
      (cond
       (res
	(cond
	 ((looking-at m3-com-end-re)
	  (forward-char 2)
	  (if (not (m3-skip-comment-backward min-point fail))
	      (progn
		(setq res nil)
		(setq continue nil))))
	 ((looking-at m3-com-start-re)
	  (setq continue nil))
	 (t
	  (goto-char save-point)
	  (setq res nil)
	  (setq continue nil))))
       (t
	(goto-char save-point)
	(setq res nil)
	(setq continue nil))))
    res))
     
;;;======================================================================
;;; Electric END completion

(defun m3-do-electric-end ()
;;;  (message "m3-do-electric-end") (sit-for 2)
  (let ((case-fold-search nil))
    (cond
     ((and (save-excursion
	     (beginning-of-line)
	     (looking-at "^[ \t]*END[ \t]*$"))
	   (or m3-electric-end m3-blink-end-matchers))
      (let ((insert-point
	     (save-excursion (beginning-of-line) (forward-word 1) (point)))
	    (insert-string))
;;;	(progn (message "m3-do-electric-end 2") (sit-for 2) t)
	(save-excursion
	  (beginning-of-line)
	  (and
	   (m3-backward-to-end-match (point-min))
	   (if m3-blink-end-matchers (sit-for 1) t)
;;;	   (progn (message "m3-do-electric-end 3") (sit-for 1) t)
	   (progn
	     (cond
	      ;; Do nothing if we're not supposed to...
	      ((not m3-electric-end))
	      ;; If it's a begin, what is it the begin of?
	      ((looking-at "BEGIN")
	       (cond
		;; If it's on the left margin, it must be a module.
		((looking-at "^BEGIN")
		 (goto-char (point-min))
		 (and
		  (re-search-forward "MODULE\\|INTERFACE" (point-max) t)
		  (progn
		    (goto-char (match-end 0))
		    (forward-word 1)
		    (setq insert-string
			  (concat
			   (buffer-substring
			    (save-excursion (forward-word -1) (point))
			    (point))
			   ".")))))
		;; Is it the body of a procedure?
		((and
;;;		(progn (message "m3-do-electric-end PROC 1") (sit-for 2) t)
		  (m3-re-search-backward "BEGIN\\|PROCEDURE" (point-min) t)
		  (looking-at "PROCEDURE"))
;;;	       (progn (message "m3-do-electric-end PROC 2") (sit-for 2) t)
		 (forward-word 2)
		 (setq insert-string
		       (concat
			(buffer-substring
			 (save-excursion (forward-word -1) (point))
			 (point))
			";")))
		;; Otherwise, it is just a random BEGIN, so
		;; m3-electric-end must be 'all.
		((eq m3-electric-end 'all)
		 (setq insert-string "(* BEGIN *)"))))

	      ((looking-at "INTERFACE\\|MODULE")
	       (forward-word 2)
	       (setq insert-string
		     (concat
		      (buffer-substring
		       (save-excursion (forward-word -1) (point))
		       (point))
		      ".")))

	      ;; Otherwise, m3-electric-end must be 'all.
	      ((eq m3-electric-end 'all)
;;;	       (progn (message "m3-do-electric-end non-BEGIN") (sit-for 2) t)
	       (setq insert-string
		     (concat "(* "
			     (buffer-substring
			      (point)
			      (save-excursion (forward-word 1) (point)))
			     " *)")))))))

	(and
	 insert-string
	 (progn
	   (goto-char insert-point)
	   ;; If we completed an END and then added something, include
	   ;; the something in the completion...
	   (if (and (marker-position m3-cur-keyword-completion-start)
		    (= insert-point
		       (+ m3-cur-keyword-completion-start
			  m3-cur-keyword-completion-len)))
	       (setq m3-cur-keyword-completion-len
		     (+ m3-cur-keyword-completion-len 1
			(length insert-string))))
	   (insert " " insert-string))))))))

		

;
;  COMMENTS
;

(defun m3-begin-comment ()
  "Indent to start comment column and then start Modula 3 comment."
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "(*  "))

(defun m3-end-comment ()
  "Indent to end comment column and then end Modula 3 comment."
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column))
  (insert "*)\n"))

(defun m3-banner ()
  "Insert a comment line suitable for marking the start of a big comment."
  (insert "(***************************************************************************)\n"))

;
; STATEMENTS, DECLARATIONS AND KEYWORDS
;

(defun m3-array ()
  "Insert ARRAY, prompt for index type then finish with OF."
  (interactive)
  (insert "ARRAY ")
  (insert (read-string "Index type: "))
  (insert " OF "))

(defun m3-case ()
  "Build skeleton CASE statement, prompting for the expression and first label(s)."
  (interactive)
  (insert "CASE ")
  (insert (read-string "Case expression: ") " OF")
  (m3-newline)
  (insert "| ")
  (insert (read-string "First case label(s): ") " =>")
  (m3-newline)
  (m3-newline)
  (insert "END; (* case *)")
  (end-of-line 0)
  (m3-tab)
  (m3-tab))

(defun m3-const ()
  "Insert CONST then newline and tab."
  (interactive)
  (insert "CONST")
  (m3-newline)
  (m3-tab))

(defun m3-declare ()
  "Insert a Modula 3 declaration; prompt the user for declaration type."
  (interactive)
  (message "Var (default), Const, Type, Exception or Procedure")
  (let ((choice (read-char)))
    (if (char-equal ?c choice) (m3-const)
      (if (char-equal ?t choice) (m3-type)
        (if (char-equal ?e choice) (m3-exception)
          (if (char-equal ?p choice) (m3-procedure) (m3-var)))))))

(defun m3-except ()
  "Insert EXCEPT clause of a TRY statement."
  (interactive)
  (insert "EXCEPT")
  (m3-newline)
  (m3-newline)
  (insert "END; (* try *)")
  (end-of-line -2)
  (m3-tab))

(defun m3-exception ()
  "Insert EXCEPTION then newline and tab."
  (interactive)
  (insert "EXCEPTION")
  (m3-newline)
  (m3-tab))

(defun m3-else ()
  "Insert ELSE or ELSIF keyword and indent for next line."
  (interactive)
  (m3-newline)
  (backward-delete-char-untabify m3-indent ())
  (message "elsE (default) or elsIf")
  (if (not (char-equal ?i (read-char))) (insert "ELSE")
    (insert "ELSIF ")
    (insert (read-string "Elsif expression: "))
    (insert " THEN"))
  (m3-newline)
  (m3-tab))

(defun m3-finally ()
  "Insert FINALLY clause of a TRY statement."
  (interactive)
  (insert "FINALLY")
  (m3-newline)
  (m3-tab)
  (m3-newline)
  (backward-delete-char-untabify m3-indent ())
  (insert "END; (* try *)")
  (end-of-line -2)
  (m3-tab))

(defun m3-for ()
  "Build skeleton FOR loop statement, prompting for the loop parameters."
  (interactive)
  (insert "FOR ")
  (insert (read-string "For: ") " TO ")
  (insert (read-string "To: "))
  (let ((by (read-string "By: ")))
    (if (not (string-equal by ""))
	(insert " BY " by)))
  (insert " DO")
  (m3-newline)
  (m3-newline)
  (insert "END; (* for *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-if ()
  "Insert skeleton IF statement, prompting for the expression."
  (interactive)
  (insert "IF ")
  (insert (read-string "If expression: ") " THEN")
  (m3-newline)
  (m3-newline)
  (insert "END; (* if *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-loop-or-lock ()
  "Insert LOOP or LOCK statement; prompt user to decide which."
  (interactive)
  (message "looP (default) or locK")
  (if (char-equal ?k (read-char)) (m3-lock) (m3-loop)))

(defun m3-loop ()
  "Build skeleton LOOP (with END)."
  (interactive)
  (insert "LOOP")
  (m3-newline)
  (m3-newline)
  (insert "END; (* loop *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-lock ()
  "Build skeleton LOCK (with END)."
  (interactive)
  (insert "LOCK ")
  (insert (read-string "Lock mutex: ") " DO")
  (m3-newline)
  (m3-newline)
  (insert "END; (* lock *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-module-type ()
  "Returns char describing module type deduced from buffername and user input."
  (interactive)
  (if (m3-is-def) ?i ?m))

(defun m3-choose-module ()
  "Build skeleton MODULE, decide module type from buffername and user input."
  (interactive)
  (m3-module (m3-module-type)))

(defun m3-choose-module-name ()
  "Prompt user for module name; if user returns null use buffer name."
  (let ((name (read-string "Module name (default is buffer name): ")))
    (if (string-equal name "")
      (m3-strip-extension (buffer-name))
      name)))

(defun m3-module (type)
  "Build skeleton MODULE, prompting for module name."
  (interactive)
  (if (char-equal type ?i) (insert "INTERFACE ")
    (if (char-equal type ?m) (insert "MODULE ") ()))
  (let ((name (m3-choose-module-name)) (args ""))
    (insert name)
    (if (char-equal type ?m) 
        (setq args (read-string "Exports list (default is empty): "))
        ())
    (if (not (string-equal args ""))
	(insert " EXPORTS " args))
    (insert ";\n\n")
    (m3-banner)
    (insert "(*   Author:  " (user-full-name))
    (m3-end-comment)
    (m3-banner)
    (insert "\n(* $Rev")                ; split into two so RCS can't find it!
    (insert "ision$ *)\n")
    (insert "\n\n\n")
    (if (char-equal type ?m)
        (insert "BEGIN\n\nEND " name ".\n")
        (insert "END " name ".\n"))
    (if (char-equal type ?m)
         (previous-line 5)
         (previous-line 3))
    ))

(defun m3-next-case ()
  "Move on to next arm of a CASE or TYPECASE statement."
  (interactive)
  (m3-newline)
  (backward-delete-char-untabify m3-indent)
  (backward-delete-char-untabify m3-indent)
  (let* ((label (read-string "Case label(s): "))
      (not-else (not (string-equal "ELSE" label))))
    (if not-else (insert "| "))
    (insert label)
    (if not-else (insert " =>"))
    (m3-newline)
    (m3-tab)
    (if not-else (m3-tab))))

(defun m3-object ()
  "Insert a skeleton OBJECT."
  (interactive)
  (insert "OBJECT")
  (m3-newline)
  (m3-newline)
  (insert "METHODS")
  (m3-newline)
  (insert "END; (* object *)")
  (end-of-line -1)
  (m3-tab))

(defun m3-procedure ()
  "Build a skeleton PROCEDURE declaration, prompting the user as necessary."
  (interactive)
  (insert "PROCEDURE ")
  (let ((name (read-string "Procedure name: " ))
	args)
    (insert name "(")
    (insert (read-string "Procedure arguments: ") ")")
    (setq args (read-string "Procedure result type: "))
    (if (not (string-equal args ""))
	(insert ": " args))
    (setq args (read-string "Procedure raises list (or ANY): "))
    (if (not (string-equal args "ANY"))
	(insert " RAISES {" args "}"))
    (if (m3-is-def) (insert ";") (insert "="))
    (m3-newline)
    (if (m3-is-def) ()
      (m3-tab)
      (insert "BEGIN")
      (m3-newline)
      (m3-newline)
      (insert "END ")
      (insert name)
      (insert ";\n\n")
      (end-of-line -2)
      (m3-tab))))


(defun m3-block ()
  "Insert a skeleton block"
  (interactive)
  (insert "BEGIN")
  (m3-newline)
  (m3-newline)
  (insert "END;")
  (end-of-line 0)
  (m3-tab))

(defun m3-record ()
  "Insert a skeleton RECORD."
  (interactive)
  (insert "RECORD")
  (m3-newline)
  (m3-newline)
  (insert "END; (* record *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-type ()
  "Insert TYPE then newline and tab."
  (interactive)
  (insert "TYPE")
  (m3-newline)
  (m3-tab))

(defun m3-try-or-typecase ()
  "Insert a TRY or TYPECASE statement; prompt the user to decide which."
  (interactive)
  (message  "tRy (default) or tYpecase")
  (if (char-equal ?y (read-char)) (m3-typecase) (m3-try)))

(defun m3-try ()
  "Build TRY statement, prompting to see if it is the EXCEPT or FINALLY form."
  (interactive)
  (insert "TRY")
  (m3-newline)
  (m3-newline)
  (message  "Except (default) or Finally")
  (if (char-equal ?f (read-char)) (m3-finally) (m3-except)))

(defun m3-typecase ()
  "Build skeleton TYPECASE statement, prompting for the expression and first labels."
  (interactive)
  (insert "TYPECASE ")
  (insert (read-string "Typecase expression: ") " OF")
  (m3-newline)
  (insert "| " (read-string "First typecase label(s): ") " =>")
  (m3-newline)
  (m3-newline)
  (insert "END; (* typecase *)")
  (end-of-line 0)
  (m3-tab)
  (m3-tab))

(defun m3-until ()
  "Insert a skeleton REPEAT loop, prompting the user for the final expression."
  (interactive)
  (insert "REPEAT")
  (m3-newline)
  (m3-newline)
  (insert "UNTIL ")
  (insert (read-string "Until expression: ") ";")
  (end-of-line 0)
  (m3-tab))

(defun m3-var ()
  "Insert VAR then newline and tab."
  (insert "VAR")
  (m3-newline)
  (m3-tab))

(defun m3-while-or-with ()
  "Insert WHILE or WITH statement; prompt user to decide which."
  (interactive)
  (message  "wHile (default) or wIth")
  (if (char-equal ?i (read-char)) (m3-with) (m3-while)))

(defun m3-while ()
  "Insert skeleton WHILE statement; prompt user for while expression."
  (interactive)
  (insert "WHILE ")
  (insert (read-string "While expression: "))
  (insert " DO")
  (m3-newline)
  (m3-newline)
  (insert "END; (* while *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-with ()
  "Insert skeleton WITH statement; prompt user for WITH bindings."
  (interactive)
  (insert "WITH ")
  (insert (read-string "With bindings: "))
  (insert " DO")
  (m3-newline)
  (m3-newline)
  (insert "END; (* with *)")
  (end-of-line 0)
  (m3-tab))

(defun m3-import ()
  "Insert FROM ... IMPORT statement, prompting user for module."
  (interactive)
  (insert "FROM ")
  (insert (read-string "Import from module: "))
  (insert " IMPORT "))

;
;  COMMANDS
;

(defun m3-compile ()
  "call m3c, argument is modulename derived from current buffer name."
  (interactive)
  (if (m3-is-oli)
    (compile (concat "m3c -g " 
                   (if (m3-is-def) "-i " "-m ") 
                   (m3-strip-extension (buffer-name))))
; else
    (compile (concat "m3 -c -g " (m3-strip-bufnum (buffer-name)))))
)

(defun m3-is-oli ()
  (or (string-equal (m3-get-extension (buffer-name)) ".i")
      (string-equal (m3-get-extension (buffer-name)) ".m"))) 

(defun m3-position-of-end-of-line ()
  "Returns position of end of line"
  (save-excursion
    (end-of-line)
    (point)))

(defun m3-match-regexp-here (regexp)
  "Tries to match regexp at current position and before end of line"
  (let ((save-point (point)))
    (if (and (re-search-forward regexp (m3-position-of-end-of-line) t)
        (= (match-beginning 0) save-point))
      t
      (goto-char save-point)
      nil)))

(defun m3-multi-to-single-line-proc-header ()
  "Convert multi line proc header to single line; do not call this directly"
  (backward-char)
  (let ((start-of-signature (point)))
    (forward-list)
    (re-search-forward "[=;]")
    (save-restriction
      (narrow-to-region start-of-signature (point))
      (goto-char (point-min))
      (save-excursion
        (replace-regexp "[ \t\n]+" " "))
      (save-excursion
        (replace-string "( " "("))
      (save-excursion
        (replace-string ") :" "):")))))

(defun m3-single-to-multi-line-proc-header ()
  "Convert multi line proc header to single line; do not call this directly"
  (backward-char)
  (let ((start-of-signature (point)))
    (forward-list)
    (re-search-forward "[=;]")
    (save-restriction
      (narrow-to-region start-of-signature (point))
      (goto-char (point-min))
      (save-excursion
        (replace-string " RAISES" "\n    RAISES"))
      (save-excursion
        (replace-regexp "\\([^*]\\)) ?:" "\\1)\n    :"))
      (save-excursion
        (replace-string "; " ";\n    "))
      (forward-char)
      (insert "\n    "))))

(defun m3-convert-proc-header ()
  "Convert single line <-> multi line proc header"
  (interactive)
  (beginning-of-line)
  (let ((old-cfs case-fold-search))
    (setq case-fold-search nil)
    (save-excursion
      (if (not (m3-match-regexp-here "\\(INLINE \\|\\) *PROCEDURE *[A-Za-z0-9_]+ *("))
        (message "Must be on first line of procedure header")
        (while (or (= (following-char) ? ) (= (following-char) ?\t))
          (delete-char 1))
        (if (or (= (following-char) ?\n) (= (following-char) ?\r))
          (m3-multi-to-single-line-proc-header)
          (m3-single-to-multi-line-proc-header))))
    (setq case-fold-search old-cfs)))

(defun m3-search-path-line (name)
"Appends given name to current line and sees if result is a file name.
Returns either nil or the file name."
  (let ((old-point (point)))
    (if (not (search-forward "\n" nil t))
      nil
    ; else
      (let* ((dir-name 
              (m3-filename-expand (buffer-substring old-point (- (point) 1))))
             (try (if (string= dir-name "") name (concat dir-name "/" name))))
        (if (file-exists-p try) try (m3-search-path-line name))))))

(defun m3-filename-expand (name)
 (let ((pos 0)
       (spos 0)
       (epos 0)
       (res nil)
       )
   (while (string-match "\\$(" name pos)
     (setq spos (match-beginning 0))
     (setq res (concat res (substring name pos spos)))
     (setq epos (string-match ")" name spos))
     (setq res (concat res (getenv (substring name (+ 2 spos) epos))))
     (setq pos (1+ epos))
   )
   (setq res (concat res (substring name pos)))
 )
)

(defun m3-strip-extension (name)
"Strips .ext from the given string (where ext is any extension)"
  (let ((dot-pos (string-match "\\." name)))
    (if dot-pos (substring name 0 dot-pos) name)))

(defun m3-strip-bufnum (name)
"Strips any <n> from the given string"
  (let ((dot-pos (string-match "<" name)))
    (if dot-pos (substring name dot-pos nil) name)))

(defun m3-get-extension (name)
"Gets .ext from the given string (where ext is any extension)"
  (let ((dot-pos (string-match "\\." name)))
    (if dot-pos 
      (let ((ext (substring name dot-pos nil)) ext_pos)
           (setq ext-pos (string-match "<" ext))
       (if ext-pos (substring ext 0 ext-pos) ext))
    ; else nil
    )
  )
)

(defun m3-search-path (name path-file extension)
"Uses path file to return full file name for given name and extension.
Arguments are NAME - the file name to search for. PATHFILE - a string which is
the name of a file containing newline separated directory names. The third
argument is EXTENSION - a string. Takes NAME, extends it by EXTENSION then
appends it to each directory given in PATHFILE until the name of an existing
file is obtained. Then returns the full file name."
  (save-excursion
      (set-buffer (generate-new-buffer (concat "*" path-file "*")))
      (delete-region (point-min) (point-max))
      (let ((old-point-max (point-max))
            (full-name (concat (m3-strip-extension name) "." extension)))
        (goto-char old-point-max)
        (if (file-readable-p path-file)
          (call-process "cat" path-file t))
        (if (= (point-max) old-point-max)
           (if (file-exists-p name) full-name nil)
        ; else
           (goto-char old-point-max)
           (m3-search-path-line full-name)))))

(defun m3-path-find-named-file (name path-file extension)
"Uses path file to search and open file with given name and extension.
Arguments are NAME - the file name to search for. PATHFILE - a string which is
the name of a file containing newline separated directory names. The third
argument is EXTENSION - a string. Takes NAME, extends it by EXTENSION then
appends it to each directory given in PATHFILE until the name of an existing
file is obtained.  Then opens the file in the other window."
  (let ((file-name (m3-search-path name path-file extension)))
    (if file-name (find-file-other-window file-name))))

(defun m3-whole-prev-word ()
"Return word under or previous to point as a string"
  (buffer-substring
    (save-excursion (backward-word 1) (point))
    (save-excursion (backward-word 1) (forward-word 1) (point))))

(defun m3-find-file-on-path (path-file extension)
"Uses path file to search and open file named by current word and extension.
Arguments are PATHFILE - a string which is the name of a file containing
newline separated directory names. The second argument is EXTENSION - a string.
Takes the word under point, extends it by EXTENSION then appends it to each
directory given in PATHFILE until the name of an existing file is obtained.
Then opens the file in the other window."
  (m3-path-find-named-file (m3-whole-prev-word) path-file extension))

(defun m3-path-find-file ()
"Visit interface corresponding to name currently under point. Looks down 
the m3path so it doesn't work unless all sources are on the m3path."
  (interactive)
  (if (m3-is-oli)
         (m3-find-file-on-path "m3path" "i")
; else
         (m3-find-file-on-path "m3path" "i3")))

;(defun execute-monitor-command (command)
;  (let* ((shell shell-file-name)
;	 (csh (equal (file-name-nondirectory shell) "csh")))
;    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun m3-toggle-buffer ()
  "Toggle between .i/.i3 and .m/.m3 files for the module."
  (interactive)
  (cond ((string-equal (m3-get-extension (buffer-name)) ".i")
	 (find-file-other-window
	   (concat (m3-strip-extension (buffer-name)) ".m")))
	((string-equal (m3-get-extension (buffer-name)) ".i3")
	 (find-file-other-window
	   (concat (m3-strip-extension (buffer-name)) ".m3")))
	((string-equal (m3-get-extension (buffer-name)) ".m")
	 (find-file-other-window
	   (concat (m3-strip-extension (buffer-name))  ".i")))
        ((string-equal (m3-get-extension (buffer-name)) ".m3")
	 (find-file-other-window
	   (concat (m3-strip-extension (buffer-name))  ".i3")))))

;
; PSEUDO ABBREV MODE
;

(defvar m3-abbrev-enabled 'aggressive
  "*Values are nil, 'aggressive, and 'polite, indicating no abbrev
completion, aggressive and polite abbrev mode, respectively.")

;;;(setq m3-abbrev-enabled 'polite)


(defvar m3-electric-end nil
  "*Values are nil -- do nothing; 'proc-mod -- match name of procedure or
module; 'all -- proc-mod + add comment for others.")

(defvar m3-blink-end-matchers nil)

;;;(setq m3-electric-end 'all)
;;;(setq m3-blink-end-matchers 't)


(defun m3-toggle-abbrev ()
  "Toggle the flag enabling/disabling Modula 3 pseudo abbrev mode."
  (interactive)
  (cond
   ((eq m3-abbrev-enabled 'aggressive)
    (setq m3-abbrev-enabled nil))
   ((eq m3-abbrev-enabled 'polite)
    (setq m3-abbrev-enabled 'aggressive))
   ((eq m3-abbrev-enabled nil)
    (setq m3-abbrev-enabled 'polite)))
  (message "Set m3-abbrev style to %s." m3-abbrev-enabled))

(defun m3-prev-word ()
  "returns last word in buffer."
  (buffer-substring (point) (save-excursion (backward-word 1) (point))))

(defun m3-is-abbrev (keyword word)
  "Returns non-nil if WORD is abbreviation of given KEYWORD."
  (if (> (length word) (length keyword)) ()
    (string-equal (substring keyword 0 (length word)) (upcase word))))


(defun m3-is-prefix (word prefix &optional no-upper)
  "returns non-nil if PREFIX is a (non-proper) prefix of WORD."
  (let ((uword (if no-upper word (upcase word)))
	(uprefix (if no-upper prefix (upcase prefix))))
    (if (> (length prefix) (length word)) nil
      (string-equal (substring uword 0 (length prefix)) uprefix))))


(defun m3-if-abbrev-kill-prev (keyword word)
  "checks if word is abbreviation of keyword; if so deletes last word
in buffer." 
  (if (not (m3-is-abbrev keyword word)) ()
    (forward-word -1)
    (delete-region (point) (save-excursion (forward-word 1) (point)))
    t))
		 

(defun m3-abbrev ()
  "call appropriate m3-function depending on value of last word in buffer."
  (let ((pw (m3-prev-word)))
    ;; Must split this in two because it's so big (or else elisp
    ;; can't handle it.)
    (cond 
     ((eq m3-abbrev-enabled 'aggressive)
      (or (m3-aggressive-abbrev-1 pw)
	  (m3-aggressive-abbrev-2 pw)))
     (t ;; "polite"
      (m3-polite-abbrev pw)))))
      

(defun m3-aggressive-abbrev-1 (pw)
  (cond
   ((m3-if-abbrev-kill-prev "ABS" pw) (insert "ABS") t)
   ((m3-if-abbrev-kill-prev "ADDRESS" pw) (insert "ADDRESS") t)
   ((m3-if-abbrev-kill-prev "ADR" pw) (insert "ADR") t)
   ((m3-if-abbrev-kill-prev "ADRSIZE" pw) (insert "ADRSIZE") t)
   ((m3-if-abbrev-kill-prev "AND" pw) (insert "AND ") t)
   ((m3-if-abbrev-kill-prev "BEGIN" pw) (insert "BEGIN") t)
   ((m3-if-abbrev-kill-prev "BITS" pw) (insert "BITS ") t)
   ((m3-if-abbrev-kill-prev "BITSIZE" pw) (insert "BITSIZE") t)
   ((m3-if-abbrev-kill-prev "BOOLEAN" pw) (insert "BOOLEAN") t)
   ((m3-if-abbrev-kill-prev "BRANDED" pw) (insert "BRANDED") t)
   ((m3-if-abbrev-kill-prev "BY" pw) (insert "BY") t)
   ((m3-if-abbrev-kill-prev "BYTESIZE" pw) (insert "BYTESIZE") t)
   ((m3-if-abbrev-kill-prev "CARDINAL" pw) (insert "CARDINAL") t)
   ((m3-if-abbrev-kill-prev "CEILING" pw) (insert "CEILING") t)
   ((m3-if-abbrev-kill-prev "CHAR" pw) (insert "CHAR") t)
   ((m3-if-abbrev-kill-prev "DEC" pw) (insert "DEC") t)
   ((m3-if-abbrev-kill-prev "DISPOSE" pw) (insert "DISPOSE") t)
   ((m3-if-abbrev-kill-prev "DIV" pw) (insert "DIV ") t)
   ((m3-if-abbrev-kill-prev "DO" pw) (insert "DO ") t)
   ((m3-if-abbrev-kill-prev "ELSIF" pw) (insert "ELSIF") t)
   ((m3-if-abbrev-kill-prev "END" pw) (insert "END") t)
   ((m3-if-abbrev-kill-prev "EVAL" pw) (insert "EVAL") t)
   ((m3-if-abbrev-kill-prev "EXCEPT" pw) (insert "EXCEPT") t)
   ((m3-if-abbrev-kill-prev "EXIT" pw) (insert "EXIT") t)
   ((m3-if-abbrev-kill-prev "EXPORTS" pw) (insert "EXPORTS ") t)
   ((m3-if-abbrev-kill-prev "FALSE" pw) (insert "FALSE") t)
   ((m3-if-abbrev-kill-prev "FINALLY" pw) (insert "FINALLY") t)
   ((m3-if-abbrev-kill-prev "FIRST" pw) (insert "FIRST") t)
   ((m3-if-abbrev-kill-prev "FLOAT" pw) (insert "FLOAT") t)
   ((m3-if-abbrev-kill-prev "FLOOR" pw) (insert "FLOOR") t)
   ((m3-if-abbrev-kill-prev "IMPORT" pw) (insert "IMPORT ") t)
   ((m3-if-abbrev-kill-prev "IN" pw) (insert "IN") t)
   ((m3-if-abbrev-kill-prev "INC" pw) (insert "INC") t)
   ((m3-if-abbrev-kill-prev "INLINE" pw) (insert "INLINE") t)
   ((m3-if-abbrev-kill-prev "INTEGER" pw) (insert "INTEGER") t)
   ((m3-if-abbrev-kill-prev "LAST" pw) (insert "LAST") t)
   ((m3-if-abbrev-kill-prev "LONGFLOAT" pw) (insert "LONGFLOAT") t)
   ((m3-if-abbrev-kill-prev "LONGREAL" pw) (insert "LONGREAL") t)
   ((m3-if-abbrev-kill-prev "LOOPHOLE" pw) (insert "LOOPHOLE") t)
   ((m3-if-abbrev-kill-prev "MAX" pw) (insert "MAX") t)
   ((m3-if-abbrev-kill-prev "METHODS" pw) (insert "METHODS") t)
   ((m3-if-abbrev-kill-prev "MIN" pw) (insert "MIN") t)
   ((m3-if-abbrev-kill-prev "MOD" pw) (insert "MOD") t)

   ;; These may be either "aggressive" or "polite".
   ((m3-if-abbrev-kill-prev "ARRAY" pw) (m3-array) t)
   ((m3-if-abbrev-kill-prev "CASE" pw) (m3-case) t)
   ((m3-if-abbrev-kill-prev "CONST" pw) (m3-const) t)
   ((m3-if-abbrev-kill-prev "ELSE" pw) (m3-else) t)
   ((m3-if-abbrev-kill-prev "FOR" pw) (m3-for) t)
   ((m3-if-abbrev-kill-prev "FROM" pw) (m3-import) t)
   ((m3-if-abbrev-kill-prev "IF" pw) (m3-if) t)
   ((m3-if-abbrev-kill-prev "LOCK" pw) (m3-lock) t)
   ((m3-if-abbrev-kill-prev "LOOP" pw) (m3-loop) t)
   ((m3-if-abbrev-kill-prev "INTERFACE" pw) (m3-module ?i) t)
   ((m3-if-abbrev-kill-prev "MODULE" pw) (m3-module ?m) t)
   ((m3-if-abbrev-kill-prev "EXCEPTION" pw) (m3-exception) t)
   (t nil)))


(defun m3-aggressive-abbrev-2 (pw)
  (cond
   ((m3-if-abbrev-kill-prev "NARROW" pw) (insert "NARROW") t)
   ((m3-if-abbrev-kill-prev "NEW" pw) (insert "NEW") t)
   ((m3-if-abbrev-kill-prev "NIL" pw) (insert "NIL") t)
   ((m3-if-abbrev-kill-prev "NULL" pw) (insert "NULL") t)
   ((m3-if-abbrev-kill-prev "NUMBER" pw) (insert "NUMBER") t)
   ((m3-if-abbrev-kill-prev "NOT" pw) (insert "NOT ") t)
   ((m3-if-abbrev-kill-prev "OF" pw) (insert "OF") t)
   ((m3-if-abbrev-kill-prev "OR" pw) (insert "OR ") t)
   ((m3-if-abbrev-kill-prev "ORD" pw) (insert "ORD") t)
   ((m3-if-abbrev-kill-prev "OVERRIDES" pw) (insert "OVERRIDES") t)
   ((m3-if-abbrev-kill-prev "RAISE" pw) (insert "RAISE") t)
   ((m3-if-abbrev-kill-prev "RAISES" pw) (insert "RAISES") t)
   ((m3-if-abbrev-kill-prev "READONLY" pw) (insert "READONLY") t)
   ((m3-if-abbrev-kill-prev "REAL" pw) (insert "REAL") t)
   ((m3-if-abbrev-kill-prev "REF" pw) (insert "REF ") t)
   ((m3-if-abbrev-kill-prev "REFANY" pw) (insert "REFANY") t)
   ((m3-if-abbrev-kill-prev "RETURN" pw) (insert "RETURN") t)
   ((m3-if-abbrev-kill-prev "REVEAL" pw) (insert "REVEAL") t)
   ((m3-if-abbrev-kill-prev "ROUND" pw) (insert "ROUND") t)
   ((m3-if-abbrev-kill-prev "SET" pw) (insert "SET OF ") t)
   ((m3-if-abbrev-kill-prev "SUBARRAY" pw) (insert "SUBARRAY") t)
   ((m3-if-abbrev-kill-prev "THEN" pw) (insert "THEN") t)
   ((m3-if-abbrev-kill-prev "TO" pw) (insert "TO") t)
   ((m3-if-abbrev-kill-prev "TRUE" pw) (insert "TRUE") t)
   ((m3-if-abbrev-kill-prev "TRUNC" pw) (insert "TRUNC") t)
   ((m3-if-abbrev-kill-prev "TYPECODE" pw) (insert "TYPECODE") t)
   ((m3-if-abbrev-kill-prev "UNSAFE" pw) (insert "UNSAFE") t)
   ((m3-if-abbrev-kill-prev "UNTIL" pw) (insert "UNTIL") t)
   ((m3-if-abbrev-kill-prev "UNTRACED" pw) (insert "UNTRACED") t)
   ((m3-if-abbrev-kill-prev "VAL" pw) (insert "VAL") t)
   ((m3-if-abbrev-kill-prev "VALUE" pw) (insert "VALUE") t)
   ((m3-if-abbrev-kill-prev "REPEAT" pw) (m3-until) t)
   ((m3-if-abbrev-kill-prev "OBJECT" pw) (m3-object) t)
   ((m3-if-abbrev-kill-prev "PROCEDURE" pw) (m3-procedure) t)
   ((m3-if-abbrev-kill-prev "RECORD" pw) (m3-record) t)
   ((m3-if-abbrev-kill-prev "TRY" pw) (m3-try) t)
   ((m3-if-abbrev-kill-prev "TYPE" pw) (m3-type) t)
   ((m3-if-abbrev-kill-prev "TYPECASE" pw) (m3-typecase) t)
   ((m3-if-abbrev-kill-prev "VAR" pw) (m3-var) t)
   ((m3-if-abbrev-kill-prev "WHILE" pw) (m3-while) t)
   ((m3-if-abbrev-kill-prev "WITH" pw) (m3-with) t)
   (t nil)))


;;; Here are the data structure we'll use for the "intelligent" keyword
;;; completion:
;;;
;;; We associate each keyword with a weight.  When we complete a keyword,
;;; 

(defvar m3-cur-keyword-completion-start (make-marker)
  "A marker indicating the start of the last word that was keyword-completed.")

(defvar m3-cur-keyword-completion-len nil
  "The length of the completed keyword at the time of completion, to allow
us to determine if the user has entered more text.")

(defvar m3-cur-keyword-completions nil
  "A list of the strings that matched the originally input keyword text.")

;;; This alist associates with each keyword:
;;; (<score> <left-margin> <pred>)
;;;
;;; <score> is a score for breaking ties.  Smaller numbers are
;;;    preferred to higher.
;;; <props> is a list of properties of the keyword.
;;;    Properties include:
;;;      left-margin status:  It is assumed that a keyword cannot
;;;        appear at the left-margin unless it has one of the
;;;        properties 'lm-ok or 'lm-only, which indicate that it can
;;;        or must appear at the left margin, respectively.
;;;      line-starter status:  It is assumed that a keyword cannot
;;;        appear after an ssl-introducer unless it has one of the
;;;        properties 'ls-ok or 'ls-only, which indicate that it can
;;;        or must appear after an ssl-introducer, respectively.
;;; <pred>, if non-nil, is a function that must return non-nil for the
;;;    completion to be legal

(defconst m3-keyword-completions
  '(("ABS" . (3 ()))
    ("ADDRESS" . (5 ()))
    ("ADR" . (6 ()))
    ("ADRSIZE" . (7 ()))
    ("AND" . (2 ()))
    ("ANY" . (1 () (lambda (on-lm starts-ssl)
		     (m3-keyword-before-ssl-introducer-p "RAISES"))))
    ("ARRAY" . (4 (ls-ok) (lambda (on-lm starts-ssl)
			    (or (not starts-ssl)
				(save-excursion
				  (forward-word -2)
				  (looking-at "OF"))))))

    ("BEGIN" . (1 (lm-ok ls-ok) (lambda (on-lm starts-ssl)
				    (save-excursion
				      (forward-word -1)
				      (if (not starts-ssl)
					  (m3-after-procedure-introducer
					   (point-min))
					t)))))
    ("BITS" . (6 ()))
    ("BITSIZE" . (7 ()))
    ("BOOLEAN" . (3 ()))
    ("BRANDED" . (4 ()))
    ("BY" . (2 () (lambda (on-lm starts-ssl)
		    (m3-keyword-before-ssl-introducer-p "FOR"))))
    ("BYTESIZE" . (5 ()))

    ("CARDINAL" . (4 ()))
    ("CASE" . (3 (ls-only)))
    ("CEILING" . (5 ()))
    ("CHAR" . (2 ()))
    ("CONST" . (1 (lm-ok ls-ok)))

    ("DEC" . (2 (ls-only)))
    ("DISPOSE" . (4 (ls-only)))
    ("DIV" . (3 ()))
    ("DO" . (1 () (lambda (on-lm starts-ssl)
		    (save-excursion
		      (forward-word -1)
		      (or
		       (m3-keyword-before-ssl-introducer-p "WHILE")
		       (m3-keyword-before-ssl-introducer-p "WITH")
		       (m3-keyword-before-ssl-introducer-p "FOR")
		       (m3-keyword-before-ssl-introducer-p "LOCK"))))))

    ("ELSE" . (2 (ls-ok) (lambda (on-lm starts-ssl)
			   (or (m3-end-matcher-is-p "IF")
			       (m3-end-matcher-is-p "TRY")
			       (m3-end-matcher-is-p "\\bCASE")
			       (m3-end-matcher-is-p "\\bTYPECASE")))))
    ("ELSIF" . (3 (ls-ok) (lambda (on-lm starts-ssl)
			    (m3-end-matcher-is-p "IF"))))
    ("END" . (1 (lm-ok ls-ok)))
    ("EVAL" . (7 (ls-only)))
    ("EXCEPT" . (6 (ls-ok) (lambda (on-lm starts-ssl)
			     (m3-end-matcher-is-p "TRY"))))
    ("EXCEPTION" . (5 (lm-only ls-ok)))
    ("EXIT" . (8 (ls-only)))
    ("EXPORTS"  . (4 () (lambda (on-lm starts-ssl)
			  (save-excursion
			    ;; One for prefix of EXPORTS one for module name,
			    ;; one for MODULE.
			    (forward-word -3)
			    (looking-at "MODULE")))))

    ("FALSE" . (4 ()))
    ("FINALLY" . (3 (ls-ok) (lambda (on-lm starts-ssl)
			      (m3-end-matcher-is-p "TRY"))))
    ("FIRST" . (5 ()))
    ("FLOAT" . (6 ()))
    ("FLOOR" . (7 ()))
    ("FOR" . (2 (ls-only)))
    ("FROM" . (1 (lm-only ls-ok)))

    ("GENERIC" . (1 (lm-only)))

    ("IMPORT"  . (2 (lm-ok ls-ok)
		    (lambda (on-lm starts-ssl)
		      (or on-lm
			  (save-excursion
			    (forward-word -3)
			    (looking-at "FROM"))))))
    ("IF" . (3 (ls-only)
	       (lambda (on-lm starts-ssl)
		 (or (not starts-ssl)
		     (save-excursion
		       (forward-word -3)
		       (not (looking-at "\\(\\bARRAY\\|\bSET\\)[ \t]+OF")))))))
    ("IN" . (7 ()))
    ("INC" . (4 (ls-only)
		(lambda (on-lm starts-ssl)
		  (or (not starts-ssl)
		      (save-excursion
			(forward-word -3)
			(not (looking-at
			      "\\(\\bARRAY\\|\bSET\\)[ \t]+OF")))))))
    ("INTEGER" . (5 (ls-ok)
		    (lambda (on-lm starts-ssl)
		      (or (not starts-ssl)
			  (save-excursion
			    (forward-word -2)
			    (looking-at "OF"))))))
    ("INTERFACE" . (1 (lm-ok) (lambda (on-lm starts-ssl)
				(save-excursion
				  (or on-lm
				      (progn
					(forward-word -2)
					(and
					 (m3-at-left-margin-p)
					 (looking-at "GENERIC\\|UNSAFE"))))))))
    ("ISTYPE" . (7 ()))

    ("LAST" . (3 ()))
    ("LOCK" . (1 (ls-only)))
    ("LOOP" . (2 (ls-only)))
    ("LONGFLOAT" . (4 ()))
    ("LONGREAL" . (5 ()))
    ("LOOPHOLE" . (6 ()))

    ("MAX" . (5 ()))
    ("METHODS" . (2 (ls-only)))
    ("MIN" . (4 ()))
    ("MOD" . (3 ()))
    ("MODULE" . (1 (lm-ok)
		   (lambda (on-lm starts-ssl)
		     (save-excursion
		       (forward-word -1)
		       (or (m3-at-left-margin-p)
			   (progn
			     (forward-word -1)
			     (and (m3-at-left-margin-p)
				  (looking-at "GENERIC\\|UNSAFE"))))))))

    ("NARROW" . (1 ()))
    ("NEW" . (2 ()))
    ("NIL" . (3 ()))
    ("NULL" . (6 ()))
    ("NUMBER" . (5 ()))
    ("NOT" . (4 ()))

    ("OBJECT" . (2 ()
		   (lambda (on-lm starts-ssl)
		     (save-excursion
		       (m3-re-search-backward m3-part-starters (point-min) t)
		       (looking-at "TYPE\\|REVEAL")))))
    ("OF" . (1 () (lambda (on-lm starts-ssl)
		    (or (m3-keyword-before-ssl-introducer-p
			 "\\bCASE\\|\\bTYPECASE")
			(m3-keyword-before-ssl-introducer-p
			 "\\bARRAY\\|SET\\b")))))
    ("OR" . (4 ()))
    ("ORD" . (5 ()))
    ("OVERRIDES" . (3 (ls-only)))

    ("PROCEDURE" . (1 (lm-ok ls-ok)))

    ("RAISE" . (5 (ls-only)))
    ("RAISES" . (3 () m3-raises-ok))
    ("READONLY" . (4 () (lambda (on-lm starts-ssl)
			  (m3-in-arg-list 0))))
    ("REAL" . (9 ()))
    ("RECORD" . (6 ()))
    ("REF" . (7 ()))
    ("REFANY" . (8 ()))
    ("REPEAT" . (10 (ls-only)))
    ("RETURN" . (2 (ls-only)))
    ("REVEAL" . (1 (lm-only ls-ok)))
    ("ROOT" . (11 ()))
    ("ROUND" . (12 ()))

    ("SET" . (1 ()))
    ("SUBARRAY" . (2 (ls-ok)))

    ("THEN" . (1 () (lambda (on-lm starts-ssl)
		      (or (m3-keyword-before-ssl-introducer-p "\\bIF")
			  (m3-keyword-before-ssl-introducer-p "\\bELSIF")))))
    ("TO" . (2 () (lambda (on-lm starts-ssl)
		    (m3-keyword-before-ssl-introducer-p "\\bFOR"))))
    ("TRUE" . (7 ()))
    ("TRUNC" . (8 ()))
    ("TRY" . (3 (ls-only)))
    ("TYPE" . (4 (lm-only ls-ok)))
    ("TYPECASE" . (5 (ls-only)))
    ("TYPECODE" . (6 ()))

    ("UNSAFE" . (1 (lm-only)))
    ("UNTIL" . (2 (ls-ok)))
    ("UNTRACED" . (3 ()))

    ("VAL" . (2 () (lambda (on-lm starts-ssl)
		     (and (not (save-excursion
				 (forward-word -1)
				 (m3-after-procedure-introducer 0)))
			  (not (m3-in-arg-list 0))))))

    ("VALUE" . (3 ()
		  (lambda (on-lm starts-ssl)
		    (not (save-excursion
			   (forward-word -1)
			   (m3-after-procedure-introducer 0))))))

    ("VAR" . (1 (lm-ok ls-ok)
		(lambda (on-lm starts-ssl)
		  (or on-lm starts-ssl
		      (save-excursion
			(forward-word -1)
			(m3-after-procedure-introducer 0))
		      (m3-in-arg-list 0)))))

    ("WHILE" . (1 (ls-only)))
    ("WITH" . (2 (ls-only)))))



(defun m3-at-left-margin-p () (eq (current-column) 0))

(defun m3-keyword-before-ssl-introducer-p (keyword)
  "Returns non-nil if KEYWORD occurs before an ssl-introducer (other than
KEYWORD), looking backward."
  (save-excursion
    (m3-re-search-backward
     (concat "\\(;\\|END\\|" m3-keyword-endable-ssl-introducers "\\|"
	     keyword "\\)")
     (point-min) 't)
    (looking-at keyword)))
      
(defun m3-end-matcher-is-p (keyword)
  "Returns non-nil if the keyword that would match an END inserted at
point is KEYWORD."
  (save-excursion
    (m3-backward-to-end-match (point-min))
    (looking-at keyword)))

(defun m3-raises-ok (on-lm starts-ssl)
  (save-excursion
    (forward-word -1)
    (let ((save-point (point)))
      (and
       (m3-re-search-backward "[^*])" 0 t)
       (progn
	 (forward-char 1)
	 (and
	  (m3-in-arg-list 0)
	  (progn
	    (forward-char 1)
	    (let ((retval-pat
		   (concat "[ \t\n]*:[ \t\n]*" m3-poss-qual-ident-re)))
	      (if (looking-at retval-pat)
		  (progn
		    (re-search-forward retval-pat)
		    (goto-char (match-end 0))))
	      (m3-forward-to-code (point-max))
	      (= (point) save-point)))))))))
	    




(defun m3-polite-abbrev (pw)
;;;  (message "In m3-polite-abbrev") (sit-for 2)
  (let ((case-fold-search nil))
    (cond
     ;; First, if the start of the current keyword is the same as the
     ;; start of the last keyword we completed, and the user hasn't
     ;; appended any characters, and m3-cur-keyword-completions is non-nil,
     ;; try the next completion in the list.
     ((and
;;;     (progn (message "In m3-polite-abbrev (x1)") (sit-for 2) t)
       (marker-position m3-cur-keyword-completion-start)
;;;     (progn (message "In m3-polite-abbrev (x2)") (sit-for 2) t)
       (> (point) m3-cur-keyword-completion-len)
       (= m3-cur-keyword-completion-start
	  (save-excursion
	    (forward-char (- m3-cur-keyword-completion-len))
	    (point)))
;;;     (progn (message "In m3-polite-abbrev (x3)") (sit-for 2) t)
       m3-cur-keyword-completions)
      (let ((cur-completion (car m3-cur-keyword-completions)))
	(setq m3-cur-keyword-completions
	      (append (cdr m3-cur-keyword-completions) (list cur-completion)))
;;;      (progn (message "In m3-polite-abbrev (xx1)") (sit-for 2) t)
	(forward-word -1)
	(delete-region m3-cur-keyword-completion-start
		       (+ m3-cur-keyword-completion-start
			  m3-cur-keyword-completion-len))
;;;      (progn (message "In m3-polite-abbrev (xx2)") (sit-for 2) t)
	(insert (car m3-cur-keyword-completions))
	(setq m3-cur-keyword-completion-len
	      (- (point) m3-cur-keyword-completion-start))
	(if (> (length m3-cur-keyword-completions) 1)
	    (message "Other matches: %s"
		     (mapconcat '(lambda (x) x)
				(cdr m3-cur-keyword-completions) ", ")))))

     ;; Otherwise, form the list of (<keyword> . <score>) pairs such
     ;; that pw is a prefix of <keyword>, <score> is the score
     ;; associated with <keyword> in m3-keyword-completions, and the
     ;; conditions in m3-keyword-completions are met.
     (t
;;;    (message "In m3-polite-abbrev (t)") (sit-for 2)
      (let ((keyword-list m3-keyword-completions)
	    matches
	    (on-lm
	     (and
	      (= (save-excursion (forward-word -1) (current-column))
		 0)
	      (let ((continue t) (res nil))
		(save-excursion
		  (while continue
		    (setq continue nil)
		    (m3-re-search-backward
		     (concat m3-part-starters "\\|" m3-end-matchers "\\|"
			     "\\bEND\\b")
		     (point-min) 'move-to-limit)
		    (cond
		     ((looking-at "END")
		      (m3-backward-to-end-match (point-min))
		      (setq continue t))
		     ((looking-at (concat "^\\(" m3-part-starters "\\)"))
		      (setq res t))
		     ((= (point) (point-min))
		      (setq res t)))))
;;;	      (message "After loop, res is %s" res) (sit-for 2)
		(and res
		     (save-excursion
		       (forward-word -1)
		       (or (= (point) (point-min))
			   (progn
			     (m3-backward-to-code (point-min))
;;;			   (message "xxx") (sit-for 2)
			     (looking-at ";"))))))))
	    (starts-ssl
	     (let ((first-char (save-excursion (forward-word -1) (point))))
	       (save-excursion
		 (forward-word -1)
		 (m3-re-search-backward
		  (concat
		   "\\(;\\|END\\|" m3-keyword-endable-ssl-introducers "\\)")
		  (point-min) 'move-to-limit)
		 (re-search-forward
		  (concat
		   "\\(;\\|END\\|" m3-keyword-endable-ssl-introducers "\\)")
		  first-char t)
		 (goto-char (match-end 0))
;;;	       (message "In m3-polite-abbrev (zz1)") (sit-for 2)
		 (m3-forward-to-code (point-max))
		 (= (point) first-char)))))
;;;      (message "In m3-polite-abbrev, on-lm = %s, starts-ssl = %s"
;;;	       on-lm starts-ssl)
;;;      (sit-for 2)

	(while keyword-list
	  (let* ((entry (car keyword-list))
		 (kw (car entry)))
;;;	  (message "In m3-polite-abbrev kw = %s" kw) (sit-for 2)
;;;	  (message "Foo") (sit-for 2)
	    (if (m3-is-prefix kw pw)
		(let* ((rest (cdr entry))
		       (score (car rest))
		       (props (car (cdr rest)))
		       (pred (car (cdr (cdr rest)))))
;;;		(message "In m3-polite-abbrev, found kw = %s" kw) (sit-for 1)
		  (let ((lm-status
			 (cond
			  ((and (memq 'lm-ok props) (memq 'lm-only props))
			   (error "Bad prop-list in m3-keyword-completions."))
			  ((memq 'lm-ok props) 'lm-ok)
			  ((memq 'lm-only props) 'lm-only)
			  (t 'lm-not)))
			(ls-status
			 (cond
			  ((and (memq 'ls-ok props) (memq 'ls-only props))
			   (error "Bad prop-list in m3-keyword-completions."))
			  ((memq 'ls-ok props) 'ls-ok)
			  ((memq 'ls-only props) 'ls-only)
			  (t 'ls-not))))
;;;		    (message
;;;		     "In m3-polite-abbrev, (2) lm-status = %s ls-status = %s"
;;;		     lm-status ls-status)
;;;		  (sit-for 2)
		    (and
		     (or (eq lm-status 'lm-ok)
			 (cond
			  ((eq lm-status 'lm-only) on-lm)
			  ((eq lm-status 'lm-not) (not on-lm))))
		     (or
;;;		    (progn (message "In m3-polite-abbrev, (3.2)")
;;;			   (sit-for 2) nil)
		      (eq ls-status 'ls-ok)
		      (cond
		       ((eq ls-status 'ls-only) starts-ssl)
		       ((eq ls-status 'ls-not) (not starts-ssl))))

		     (or 
;;;		    (progn (message "In m3-polite-abbrev, (5), pred = %s" pred)
;;;			   (sit-for 2) nil)
		      (not pred)
;;;		    (progn (message "In m3-polite-abbrev, (5)")
;;;			   (sit-for 2) nil)
		      (funcall pred on-lm starts-ssl))
;;;		   (message "In m3-polite abbrev, adding %s to matches" kw)
;;;		   (sit-for 2)
		     (setq matches (cons (cons kw score) matches)))))))
	  (setq keyword-list (cdr keyword-list)))

;;;   (message "In m3-polite-abbrev (after matches): %s" matches) (sit-for 4)
	;; If there are any matches, do a completion
	(and matches
	     (progn
	       ;; Now sort matches according to score.
;;;	     (message "In m3-polite-abbrev, (10)") (sit-for 2)
	       (setq matches
		     (sort matches '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
	       ;; And strip off the scores from the result.
;;;	     (message "In m3-polite-abbrev, (11)") (sit-for 2)
	       (setq matches (mapcar'(lambda (e) (car e)) matches))
;;;	     (message "In m3-polite-abbrev, (12)") (sit-for 2)
	       (setq m3-cur-keyword-completions matches)
	       (let ((first-match (car matches)))
		 (forward-word -1)
		 (delete-region (point)
				(save-excursion (forward-word 1) (point)))
;;;	       (message "In m3-polite-abbrev, (13)") (sit-for 2)
		 (set-marker m3-cur-keyword-completion-start (point))
		 (insert first-match)
		 (setq m3-cur-keyword-completion-len
		       (- (point) m3-cur-keyword-completion-start))
		 (if (> (length matches) 1)
		     (message
		      "Other matches: %s"
		      (mapconcat '(lambda (x) x) (cdr matches) ", ")))))
	     ))))))
      
		      
;;; The identifiers are the names of
;;;   1) imported interfaces
;;;   2) variables, types, constants, exceptions and procedures defined at
;;;      module scope. 
;;;   3) If we are in a procedure body:
;;;       variables, constants, and procedures defined in enclosing scopes.

(defun m3-find-id-list ()
  (interactive)
  "Returns a list of the identifiers visible at point."
  (append
   (setq m3-mod-names (m3-module-names))
   (m3-scope-names)
   ))

(defun m3-module-names ()
  (interactive)
  (let ((names nil))
    (save-excursion
      (goto-char (point-min))
      (while (m3-re-search-forward
	      (concat "^\\(" m3-part-starters "\\)")
	      (point-max) t)
;;;	(message "m3-module-names (0.1)") (sit-for 2)
	(let ((limit
	       (save-excursion
		 (forward-word 1)
;;;		 (message "m3-module-names (0.2)") (sit-for 2)
		 (if (m3-re-search-forward
		      (concat "^\\(" m3-part-starters "\\)")
		      (point-max) t)
		     (point)
		   (point-max)))))
;;;	  (message "m3-module-names (1)") (sit-for 2)
	  (cond
	   ((looking-at "INTERFACE\\|MODULE")
	    (forward-word 1))
	   ((looking-at "IMPORT")
;;;	    (message "m3-mod-names, IMPORT(1)") (sit-for 2)
	    (forward-word 1)
	    (setq names (append (m3-parse-name-list "," nil ";" limit) names)))
	   ((looking-at "FROM")
;;;	    (message "m3-mod-names, FROM(1)") (sit-for 2)
	    (forward-word 3)
	    (forward-word -1)
;;;	    (message "m3-mod-names, FROM(2)") (sit-for 2)
	    (if (looking-at "IMPORT")
		(progn (forward-word 1)
		       (setq names (append
				    (m3-parse-name-list "," nil ";" limit)
				    names)))))
	   ((looking-at "TYPE\\|REVEAL")
	    (forward-word 1)
	    (setq names (append (m3-parse-name-list
				 ";" "\\(=\\|<:\\)" nil limit)
				names)))
	   ((looking-at "EXCEPTION")
	    (forward-word 1)
	    (setq names (append (m3-parse-name-list ";" nil nil limit)
				names)))
	   ((looking-at "VAR")
	    (forward-word 1)
	    (let ((continue t))
	      (while (and (< (point) (point-max)) continue)
;;;		(message "m3-mod-names, VAR(1), limit = %d" limit) (sit-for 2)
		(setq names (append (m3-parse-name-list "," nil ":" limit)
				    names))
;;;		(message "m3-mod-names, VAR(2)") (sit-for 2)
		(setq continue
		      (and (m3-re-search-forward ";" limit t)
;;;			   (progn (message "m3-mod-names, VAR(3)")
;;;				  (sit-for 2) t)
			   (progn (forward-char 1)
;;;				  (message "m3-mod-names, VAR(4)") (sit-for 2)
				  (m3-forward-to-code limit)
;;;				  (message "m3-mod-names, VAR(5), point = %d"
;;;					   (point))
;;;				  (sit-for 2)
				  (< (point) limit)))))))
	   ((looking-at "PROCEDURE")
	    (forward-word 1)
	    (m3-forward-to-code limit)
	    (setq names (cons (buffer-substring (point)
						(save-excursion
						  (forward-word 1)
						  (point)))
			      names)))
	   ((looking-at "CONST")
	    (forward-word 1)
	    (setq names (append (m3-parse-name-list ";" "=" nil limit)
				names)))))))
;;;    (message "names are %s" names)
    names))


(defun m3-scope-names ()
  "If we are not in a procedure scope, return nil.  If we are, return a list
of all identifiers defined in the current scope."
  ;; Identifiers can be introduced by VAR, CONST, WITH, or nested PROCEDURE
  ;; declarations.
  (interactive)
;;;  (message "m3-scope-names") (sit-for 2)
  (let ((case-fold-search nil) (orig-point (point)))
    (save-excursion
      (cond
       ((save-excursion
	  (m3-re-search-backward (concat "^\\(" m3-part-starters "\\)")
				 (point-min) t)
;;;	  (message "m3-scope-names (1)") (sit-for 2)
	  (and
	   (looking-at "^PROCEDURE")
	   (progn
	     (forward-word 1)
	     (m3-re-search-forward "VAR\\|CONST\\|PROCEDURE\\|BEGIN"
				   (point-max) 'move-to-limit)
	     (while (m3-in-arg-list (point-min))
	       (forward-word 1)
	       (m3-re-search-forward "VAR\\|CONST\\|PROCEDURE\\|BEGIN"
				     (point-max) 'move-to-limit))
	     (and (looking-at "VAR\\|CONST\\|PROCEDURE\\|BEGIN")
		  (> orig-point (point))))))
	       
;;;	(message "m3-scope-names (2)") (sit-for 2)
	(let ((continue t) (names nil) (orig-point (point)))
	  ;; Set things up to make sure we start with a complete list.
	  (m3-re-search-backward
	   (concat "\\(VAR\\|CONST\\|WITH\\|PROCEDURE\\|END\\|;\\)")
	   (point-min) t)
;;;	  (message "m3-scope-names (2.2)") (sit-for 2)
	  (while continue
	    (m3-re-search-backward
	     (concat "\\(VAR\\|CONST\\|WITH\\|PROCEDURE\\|END\\)")
	     (point-min) t)
;;;	    (message "m3-scope-names (3)") (sit-for 2)
	    (cond
	     ((looking-at "^PROCEDURE")
	      (setq continue nil))

	     ((looking-at "END")
	      (m3-backward-to-end-match (point-min))
	      ;; If we're now looking at a BEGIN, skip over any
	      ;; VAR, CONST, or PROCEDURE associated with the BEGIN.
	      (cond
	       ((looking-at "BEGIN")
		(m3-re-search-backward
		 (concat "\\(BEGIN\\|WITH\\|END\\)")
		 (point-min) t)
		(forward-word 1))))

	     ((looking-at "VAR")
;;;	      (message "m3-scope-names (VAR)") (sit-for 2)
	      (let ((save-point (point))
		    (limit
		     (save-excursion
		       (forward-word 1)
		       (m3-re-search-forward
			(concat "\\(" m3-part-starters "\\|BEGIN\\)")
			orig-point 'move-to-limit)
;;;		       (message "m3-scope-names, VAR(0.5)") (sit-for 2)
		       (point))))
		(forward-word 1)
		(let ((continue t))
		  (while (and (< (point) (point-max)) continue)
;;;		  (message "m3-scope-names, VAR(1)") (sit-for 2)
		    (let ((new-names (m3-parse-name-list "," nil ":"
							 limit)))
;;;		      (message "m3-scope-names, VAR(2), new-names = %s"
;;;			       new-names)
;;;		      (sit-for 2)
		      (setq names (append names new-names)))
;;;		  (message "m3-scope-names, VAR(3)") (sit-for 2)
		    (setq continue
			  (and (m3-re-search-forward ";" limit t)
;;;			     (progn (message "m3-mod-names, VAR(4)")
;;;				    (sit-for 2) t)
			       (progn
				 (forward-char 1)
;;;			       (message "m3-mod-names, VAR(5)")
;;;			       (sit-for 2)
				 (m3-forward-to-code limit)
;;;			       (message "m3-mod-names, VAR(6), point = %d"
;;;					(point))
;;;			       (sit-for 2)
				 (< (point) limit))))))
		(goto-char save-point)))
	 
	     ((looking-at "CONST")
	      (let ((save-point (point)))
		(forward-word 1)
		(setq names (append (m3-parse-name-list ";" "=" nil orig-point)
				    names))
		(goto-char save-point)))

	     ((looking-at "WITH")
	      (let ((save-point (point)))
		(forward-word 1)
		(let ((new-names (m3-parse-name-list "," "=" "DO" orig-point)))
;;;		  (message "WITH: %s" new-names) (sit-for 2)
		  (setq names (append names new-names)))		  
		(goto-char save-point)))
	    

	     ((looking-at "PROCEDURE")
	      (let ((save-point (point)))
		(forward-word 1)
		(m3-forward-to-code orig-point)
		(setq names (cons (buffer-substring (point)
						    (save-excursion
						      (forward-word 1)
						      (point)))
				  names))
		(goto-char save-point)))))
;;;	  (message "scope-names returns %s" names) (sit-for 10)
	  names))
    
       (t nil)))))
	    
	    

(defun m3-parse-name-list (between skip end lim)
  "Assumes point is at the start of a BETWEEN-separated list.  Assumes that
SKIP, if non-nil is an regexp giving a pattern that ends each element,
and starts a string that should be ignored up to the next BETWEEN or END.
END, if non-nil gives the regexp that terminates the list.
 Alternatively, LIMIT, if non-nil is a character position that bounds the
parse.  Returns the list of names, leaves point positioned after list."
;;;  (message "m3-parse-name-list (1)") (sit-for 2)
  (let ((names nil) (continue t) (limit (if lim lim (point-max))))
;;;    (message "m3-parse-name-list, re = %s" re) (sit-for 2)
    (while (and (< (point) limit) continue)
;;;      (message "m3-parse-name-list (2)") (sit-for 2)
      (m3-forward-to-code limit)
;;;      (message "m3-parse-name-list (3)") (sit-for 2)
      (cond
       ((< (point) limit)
;;;	(message "m3-parse-name-list (3.5)") (sit-for 2)
	(let ((start (point)))
	  (setq names (cons (buffer-substring
			     (point)
			     (progn (forward-word 1) (point)))
			    names))
	  (cond
	   ((m3-re-search-forward
	     (concat "\\(" between "[ \t\n]*" m3-identifier-re
		     (if skip (concat "[ \t\n]*" skip)) "\\)"
		     (if end (concat "\\|" end)))
	     limit t)
;;;	    (message "m3-parse-name-list (4)") (sit-for 2)
	    (cond
	     ((looking-at between)
;;;	      (message "m3-parse-name-list (6.1)") (sit-for 2)
	      (re-search-forward between limit t)
	      (goto-char (match-end 0)))
	     ((and end (looking-at end))
;;;	      (message "m3-parse-name-list (6.2)") (sit-for 2)
	      (setq continue nil))))
	   (t
	    (setq continue nil)))))))
;;;    (message "Parse-name-list returns: %s" names) (sit-for 2)
    names))



(defvar m3-cur-ident-completion-start (make-marker)
  "A marker indicating the start of the last word that was
identifier-completed.")

(defvar m3-cur-ident-completion-len nil
  "The length of the completed identifier at the time of completion, to allow
us to determine if the user has entered more text.")

(defvar m3-cur-ident-completions nil
  "A list of the strings that matched the originally input identifier text.")

(defvar m3-cur-ident-completion-done nil
  "A list of the strings that matched the originally input identifier text.")

(defvar m3-ident-match-no-case-fold t
  "If non-nil, case matters in identifier matches.  If nil, case is ignored.")
;;;(setq m3-ident-match-no-case-fold nil)

(defun m3-ident-complete ()
  "Moves to the end of the current word; then checks if that word is a prefix
of any of the identifiers in the current file.  If it is a prefix of a
unique member of the list, completes the word to that prefix.  If it is a
prefix of multiple elements of the list, completes to the longest prefix
shared by all those elements, and presents the further completions to
the user in the minibuffer.  If this command is next executed from at
the end of the partially completed word, and no changes have been made
to the word, it will fill in the first element of the set of full
completions, and subsequent executions will cycle through the list."
  (interactive)
;;;  (message "In m3-ident-complete") (sit-for 2)
  (m3-ident-complete-work (m3-find-id-list)))
	   
(defun m3-ident-complete-work (names)
  "Moves to the end of the current word; then checks if that word is a prefix
of any of the strings in NAMES.  If it is a prefix of a unique member of the
list, completes the word to that prefix.  If it is a prefix of multiple
elements of the list, completes to the longest prefix shared by all those
elements, and presents the further completions to the user in the minibuffer.
If this command is next executed from at the end of the partially
completed word, and no changes have been made to the word, it will fill in
the first element of the set of full completions, and subsequent executions
will cycle through the list."
;;;  (message "In m3-ident-complete-work (1), names = %s" names) (sit-for 2)
  (let ((save-point (point)))
    (cond
     ((and
;;;       (progn (message "In m3-ident-complete (x1)") (sit-for 2) t)
       (marker-position m3-cur-ident-completion-start)
;;;     (progn (message "In m3-ident-complete (x2)") (sit-for 2) t)
       (> (point) m3-cur-ident-completion-len)
;;;     (progn (message "In m3-ident-complete (x2.5)") (sit-for 2) t)
       (= m3-cur-ident-completion-start
	  (save-excursion
	    (forward-char (- m3-cur-ident-completion-len))
;;;	  (progn (message "In m3-ident-complete (x2.75)") (sit-for 2) t)
	    (point)))
;;;     (progn (message "In m3-ident-complete (x3)") (sit-for 2) t)
       m3-cur-ident-completions)
      (let ((cur-completion (car m3-cur-ident-completions)))
	(if m3-cur-ident-completion-done
	    (setq m3-cur-ident-completions
		  (append (cdr m3-cur-ident-completions)
			  (list cur-completion))))
;;;	(progn (message "In m3-ident-complete (xx1)") (sit-for 2) t)
	(forward-word -1)
	(delete-region m3-cur-ident-completion-start
		       (+ m3-cur-ident-completion-start
			  m3-cur-ident-completion-len))
;;;      (progn (message "In m3-ident-complete (xx2)") (sit-for 2) t)
	(insert (car m3-cur-ident-completions))
	(setq m3-cur-ident-completion-len
	      (- (point) m3-cur-ident-completion-start))
	(setq m3-cur-ident-completion-done t)
	(if (> (length m3-cur-ident-completions) 1)
	    (message "Other matches: %s"
		     (mapconcat '(lambda (x) x)
				(cdr m3-cur-ident-completions) ", ")))))
   
     ;; Otherwise, find the current word, and see if it is a prefix of any
     ;; members of names.
     (t
;;;    (progn (message "In m3-ident-complete-work (1)") (sit-for 2) t)
      (cond
       ((and (not (looking-at m3-identifier-char-re))
	     (or (= (point) (point-min))
		 (save-excursion
		   (forward-char -1)
		   (not (looking-at m3-identifier-char-re)))))
;;;      (progn (message "In m3-ident-complete-work (2.1)") (sit-for 2) t)
	(beep)
	(message "Not in identifier!")
	(goto-char save-point))
       (t
;;;      (progn (message "In m3-ident-complete-work (2)") (sit-for 2) t)
	(let ((cur-word
	       (cond
		((looking-at (concat "\\b" m3-identifier-char-re))
		 (buffer-substring (point)
				   (save-excursion (forward-word 1)
						   (point))))
		(t
		 (forward-word -1)
		 (buffer-substring (point)
				   (save-excursion (forward-word 1)
						   (point))))))
	      (matches nil))
	  ;; Get the matches
	  (mapcar '(lambda (elem)
		     (if (m3-is-prefix elem cur-word
				       m3-ident-match-no-case-fold)
			 (setq matches (cons elem matches))))
		  names)

;;;	  (message "In m3-ident-complete-work (3), matches = %s" matches)
;;;	  (sit-for 2)
	  (cond
	   ((eq (length matches) 0)
	    (goto-char save-point)
	    (message "No matches of current word '%s'." cur-word))
	   ((eq (length matches) 1)
	    (delete-region (point)
			   (save-excursion (forward-word 1) (point)))
	    (insert (car matches)))
	   (t
	    ;; Multiple matches.  Sort them alphabetically.
	    (setq matches (sort matches 'string<))
	    ;; Find the longest common prefix.
	    (let ((lcp (m3-longest-common-prefix matches)))
;;;	      (message "In m3-ident-complete-work (4), lcp = %s" lcp)
;;;	      (sit-for 2)
	      (setq m3-cur-ident-completions matches)
	      (setq m3-cur-ident-completion-len (length lcp))
	      ;; This completion is only partial.
	      (setq m3-cur-ident-completion-done nil)
	      (delete-region (point)
			     (save-excursion (forward-word 1) (point)))
	      (set-marker m3-cur-ident-completion-start (point))
	      (insert lcp)
	      (if (> (length m3-cur-ident-completions) 1)
		  (message
		   "Completions: %s"
		   (mapconcat '(lambda (x) x)
			      m3-cur-ident-completions ", ")))))))))))))


(defun m3-longest-common-prefix (names)
  "Returns the longest string that is a common substring of all the strings
in NAMES"
  (m3-longest-common-prefix-work (car names) (cdr names)))

(defun m3-longest-common-prefix-work (lcp names)
  "Returns the longest string that is a common substring of lcp and the
strings in the list NAMES"
  (cond
   ((null names) lcp)
   (t
    (let ((len (length lcp))
	  (continue nil)
	  (first-name (car names)))
;;;      (message "m3-lcp, lcp: %s, fn: %s" lcp first-name) (sit-for 2)
      (while (and (> len 0) (not (m3-is-prefix first-name
					       (substring lcp 0 len))))
;;;	(message "m3-lcp, len = %d, lcp = %s" len (substring lcp 0 len))
;;;	(sit-for 2)
	(setq len (- len 1)))
;;;      (message "m3-lcp (2)") (sit-for 2)
      (cond
       ((= len 0) "")
       (t (m3-longest-common-prefix-work (substring lcp 0 len)
					 (cdr names))))))))
	      
    
        

;;;======================================================================

(defun m3-is-letter (ch)
  "checks if argument is a letter."
  (and (>= (upcase ch) ?A) (<= (upcase ch) ?Z)))

(defun m3-abbrev-or-tab ()
  "if preceding char in buffer is letter, tries to expand abbrev else tabs."
  (interactive)
  (if (and m3-abbrev-enabled (m3-is-letter (preceding-char)))
    (m3-abbrev)
    (m3-tab)))

(defun m3-abbrev-and-or-indent ()
  "If preceding char in buffer is letter, tries to expand abbrev.
Otherwise, indents the current line."
  (interactive)
;;;  (message "Foo1") (sit-for 2)
  (if (and m3-abbrev-enabled
	   (or (m3-is-letter (preceding-char))
	       (save-excursion
		 (and
		  (> (point) 2)
		  (progn
		    (forward-char -2)
		    (and
		     (looking-at "*)")
		     (progn (forward-word -1) (forward-char -3)
			    (looking-at "(*"))
		     (progn (forward-word -1) (looking-at "END"))))))
	       (save-excursion
		 (and
		  (> (point) 2)
		  (progn
		    (forward-char -1)
		    (and
		     (looking-at ";\\|.")
		     (progn (forward-word -2) (looking-at "END")))))))
	   (or (eq (point) (point-max))
	       (eq (following-char) ?\ )
	       (eq (following-char) ?\t)
	       (eq (following-char) ?\n)))
      (progn (m3-abbrev)
	     (m3-indent-line))
    (m3-indent-line)))


; Finally a function for those used to M2 style text literals. It checks for
; text literals containing single quotes and ensures that they are preceded by
; a backslash.
; BUG: If a text literal contains an embedded double quote it is ignored

(defun m3-text-literal-check ()
"Ensures that single quotes in text literals are preceded by backslash"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^\\\\]\"\\([^\"]*\\)[^\\\\]" nil t)
      (save-excursion
        (save-restriction
          (narrow-to-region (match-beginning 1) (match-end 1))
          (goto-char (point-min))
          (replace-regexp "\\([^\\\\]\\)'" "\\1\\\\'"))))))


;;;-------------------------------------------------------- pretty printing ---

(defvar m3pp-options '("-ZZ")
  "Command line options that should be passed to m3pp when it is started up.")

(defvar &m3pp-modunit "\002")
(defvar &m3pp-defunit "\005")
(defvar &m3pp-endunit "\001")

(defvar &m3pp-process nil)
(defvar &m3pp-in-progress nil)

(defvar &m3pp-unit-boundary
      (concat "^[ \t]*\nCONST\\|" 
              "^[ \t]*\nTYPE\\|"
              "^[ \t]*\nVAR\\|"
              "^[ \t]*\nPROCEDURE\\|"
              "^[ \t]*\nEXCEPTION\\|"
	      "^[ \t]*\n<\*EXTERNAL\*>|"
	      "^[ \t]*\n<\*INLINE\*>|"
              "^[ \t]*\nMODULE\\|"
	      "^[ \t]*\nINTERFACE\\|"
	      "^[ \t]*\nIMPORT\\|"
              "^[ \t]*\nBEGIN"))

(defun &m3pp-startup ()
  (if (not (and &m3pp-process
		(process-status (process-name &m3pp-process))))
      (save-excursion 
	(get-buffer-create "&m3pp")
	(set-buffer "&m3pp")
	(erase-buffer)
	(setq &m3pp-process 
	      (apply 'start-process "m3pp" nil "m3pp" m3pp-options))
	(process-kill-without-query &m3pp-process)
	(set-process-filter &m3pp-process '&m3pp-filter)
	(process-send-string &m3pp-process 
			     (concat &m3pp-modunit &m3pp-endunit "\n"))
	(accept-process-output &m3pp-process))))

(defun m3pp-unit ()
  "Pretty prints the 'unit' containing the cursor. 
   A unit starts with a blank line followed by CONST, TYPE, VAR, 
   PROCEDURE, EXCEPTION, IMPORT, FROM, MODULE, or BEGIN, and it extends 
   to the start of the next unit.  If there is no such unit around the
   cursor, the entire file is pretty printed."
  (interactive)
  (save-excursion
    (let (start)
      (&m3pp-find-format-unit)
      (setq start (point-marker))
      (m3pp-region)
      (set-mark (point))
      (goto-char start)
      (exchange-point-and-mark))))

(defun m3pp-region ()
  "Pretty prints the region. 
   The region should consist of zero or more declarations, definitions, 
   import statements, or modules."
  (interactive)
  (let* ((size (- (point-marker) (mark-marker)))
	 (a (if (< size 0) (- size) size)))
    (if (> a 32760)
	(error (concat "Sorry, region too large for emac "
		       (int-to-string a) " > 32760"))))
  (safe-m3pp-region))

(defun safe-m3pp-region ()
  (let ((m3pp-type nil)
	(m3pp-start nil))
    (&m3pp-startup)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward &m3pp-endunit (point-max) t)
	  (error "m3pp: file mustn't contain ^A"))
      (get-buffer-create "&m3pp-output")
      (set-buffer "&m3pp-output")
      (erase-buffer))
    (let* ((len (length (buffer-file-name)))
	   (tail (substring (buffer-file-name) (- len 3) len)))
      (if (string-equal tail ".m3")
	  (setq m3pp-type &m3pp-modunit))
      (if (string-equal tail ".i3")
	  (setq m3pp-type &m3pp-defunit))
      (if (and (not (string-equal tail ".m3")) (not (string-equal tail ".i3")))
	  (error "m3pp: pretty-print only .m3 or .i3 files")))
    (message "m3pp: working ...")
    (setq &m3pp-in-progress t)
    (process-send-string 
     &m3pp-process
     (concat m3pp-type 
	     (buffer-substring (min (point) (mark)) (max (point) (mark)))
	     &m3pp-endunit "\n"))
    (while &m3pp-in-progress
      (accept-process-output &m3pp-process))
    (setq m3pp-start (point-marker))
    (kill-region (point) (mark))
    (insert-buffer "&m3pp-output")
    (save-excursion
      (set-buffer "&m3pp-output")
      (if (re-search-backward "(\\* SYNTAX ERROR " (point-min) t)
	  (progn
	    (beep)
	    (message "m3pp: syntax error"))
	(progn ;else
	  (message "m3pp: done"))))
    (if (not (pos-visible-in-window-p))
	(let ((dotval (+ (point-marker))))
	  (line-to-bottom-of-window)
	  (goto-char dotval)))))

(defun &m3pp-filter (&process &str)
  (save-excursion
    (get-buffer-create "&m3pp-output")
    (set-buffer "&m3pp-output")
    (goto-char (point-max))
    (insert &str)
    (if (search-backward &m3pp-endunit (point-min) t) 
	(progn
	  (delete-char 2)
	  (setq &m3pp-in-progress nil)))))

(defun &m3pp-find-format-unit ()
  (if (not (re-search-backward &m3pp-unit-boundary (point-min) t))
      (goto-char (point-min)))
  (set-mark (point))
  (if (bobp)
      (progn 
	(goto-char (point-max))
	(if (bolp) (backward-char)))
    (progn ;else
      (forward-line)
      (beginning-of-line)
      (set-mark (point))
      (if (not (re-search-forward &m3pp-unit-boundary (point-max) t)) 
	  (progn 
	    (goto-char (point-max))
	    (if (bolp) (backward-char)))
        (progn ;else
	  (beginning-of-line)))))
  (exchange-point-and-mark)
  nil)

;;;------------------------------------------------------------------- epoch ---

(if (boundp 'epoch::version)
    (progn
	(require 'mouse)
	(require 'scr-pool)))

(defvar *m3::defpath* '("." "/proj/cra/ultrix/${CPU_TYPE}/pub/m3")
  "Search path for Modula-3 interfaces")

(if (boundp 'epoch::version)
    (progn
      (defvar *m3::poolsize* 8
	"Size of the pool of screens for Modula-3 interfaces")
      (defvar *m3::poolclass* "Modula-3"
	"Class for the Modula-3 interface screens")))

;;; stolen from lib-complete, 
;;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;;; Created On      : Sat Apr 20 17:47:21 1991
;;; Last Modified By: Mike Williams
;;; Last Modified On: Tue Jun 18 12:53:08 1991

(defun m3::locate-file (FILE SEARCH-PATH &optional SUFFIX-LIST PRED)
  "Search for FILE on SEARCH-PATH (list).  If optional SUFFIX-LIST is
provided, allow file to be followed by one of the suffixes.
Optional second argument PRED restricts the number of files which
may match.  The default is file-exists-p."
  (if (not SUFFIX-LIST) (setq SUFFIX-LIST '("")))
  (if (not PRED) (setq PRED 'file-exists-p))
  (if (file-name-absolute-p FILE) (setq SEARCH-PATH '(nil)))
  (if (equal FILE "") (error "Empty filename"))
  (let ((filelist 
	 (mapcar 
	  (function (lambda (ext) (concat FILE ext)))
	  SUFFIX-LIST)))
    ;; Search SEARCH-PATH for a readable file in filelist
    (catch 'found
      (while SEARCH-PATH
	(let ((filelist filelist))
	  (while filelist
	    (let ((filepath (substitute-in-file-name 
			     (expand-file-name (car filelist) (car SEARCH-PATH)))))
	      (if (funcall PRED filepath)
		  (throw 'found filepath)))
	    (setq filelist (cdr filelist))))
	(setq SEARCH-PATH (cdr SEARCH-PATH))))
    ))



(defun m3::show-interface (&optional arg)
  "Find a Modula-3 interface. 
   If ARG is a string, it is the name of the interface.  If ARG is nil, 
   get the name from the text around the point.  Otherwise, ARG should be 
   an epoch mouse position and the name is found around that position.
   Using *m3::defpath*, find the file that contains that interface.
   Under gnuemacs, show the interface in another window. 
   Under epoch, show the interface in a screen of the Modula-3 pool; the 
   screens in that pool are in the class *m3::poolclass*. The Modula-3 pool
   is of size *m3::poolsize*." 
  (interactive)
  (let (buffer pos interface filename)
    (if (stringp arg)
	(setq interface arg)
      (save-excursion
	(if arg
	    (progn
	      (setq buffer (nth 1 arg))
	      (setq pos (car arg)))
	  (progn
	    (setq buffer (current-buffer))
	    (setq pos (point))))
	(set-buffer buffer)
	(goto-char pos)
	(let (end)
	  (re-search-forward "[^A-Za-z0-9]" nil t)
	  (backward-char)
	  (setq end (point))
	  (re-search-backward "[^A-Za-z0-9]" nil t)
	  (forward-char)
	  (setq interface (buffer-substring (point) end)))))
    (setq filename (locate-file (concat interface ".i3") *m3::defpath*))
    (if (boundp 'epoch::version)
	(progn
	  (setq buf (find-file-noselect filename))
	  (let ((screen (pool:get-shrink-wrapped-screen 
			 m3-interfaces-pool buf '(80 80 20 40))))
	    (epoch::select-screen screen)
	    (epoch::mapraised-screen screen))
	  (switch-to-buffer buf))
      (progn 
	(find-file-other-window filename)))))


(if (boundp 'epoch::version)
    (progn
      (setq m3-interfaces-pool
	    (pool:create *m3::poolsize* 
			 '(lambda () 
			    (create-screen nil 
					   (cons (cons 'class *m3::poolclass*) 
						 nil)))))
      (global-set-mouse mouse-left mouse-meta 'm3::show-interface)))
    

