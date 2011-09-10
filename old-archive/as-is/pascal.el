Received: from pizza by PIZZA.BBN.COM id aa11914; 11 Oct 88 16:25 EDT
Received: from BBN.COM by PIZZA.BBN.COM id aa11906; 11 Oct 88 16:22 EDT
Received: from USENET by bbn.com with netnews
	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
	contact usenet@bbn.com if you have questions.
To: unix-emacs@BBN.COM
Date: 11 Oct 88 01:25:03 GMT
From: Vincent Broman <bloom-beacon!apple!bionet!agate!helios.ee.lbl.gov!nosc!trout.nosc.mil!broman%eddie.mit.edu.uucp@BBN.COM>
Sender: arpa-unix-emacs-request@BBN.COM
Subject: Re: Wanted: Pascal Minor mode for Gnu Emacs
Reply-To: broman@nosc.mil
Message-ID: <BROMAN.88Oct11102503@schroeder.nosc.mil>
References: <LAZLOR.88Oct5210131@ucscb.UCSC.EDU>, <23772@tut.cis.ohio-state.edu>c
Organization: none
Source-Info:  From (or Sender) name not authenticated.

Seeing the older pascal-mode float by reminded me that I use a significantly
rewritten descendent version now, with more features and some support for
Berkeley pc and DOMAIN Pascal compiler idiosyncracies.  The implementation
of pascal-indent-line is unfinished and unused.

If FSF wants to pick this up, fine with me.

Vincent Broman,  code 632, Naval Ocean Systems Center, San Diego, CA 92152, USA
Phone: +1 619 553 1641    Internet: broman@nosc.mil   Uucp: sdcsvax!nosc!broman
-----------------cut here----------------------------------------------------
;;; Pascal editing support package in GNU Emacs Elisp.  v2.1
;;; Author: Vincent Broman <broman@nosc.mil>  February 1988.
;;;
;;; (borrows from Mick Jordan's modula-2-mode for Emacs,
;;; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)
;;;
;;; Tries hard to do all the indenting automatically.
;;; Emphasizes correct insertion of new code, more than editing old code,
;;; although movement by indentation groups is supported.
;;;
;;; To do:   semiautomatic variable declaration.
;;;          quick duplication of subprogram specs needed for bodies or calls.
;;;          interface with abbrev-mode and/or outline-mode
;;;          finish the unused pascal-indent-line function
;;;

(let ((pascal-suffix "\\.pas$"))
  (if (null (assoc pascal-suffix auto-mode-alist))
    (setq auto-mode-alist (cons (cons pascal-suffix 'pascal-mode)
				auto-mode-alist))))
(let ((pascal-suffix "\\.p$"))
  (if (null (assoc pascal-suffix auto-mode-alist))
    (setq auto-mode-alist (cons (cons pascal-suffix 'pascal-mode)
				auto-mode-alist))))

(defvar pascal-mode-syntax-table nil
  "Syntax table in use in Pascal-mode buffers.")

(let ((pascal-tbl (make-syntax-table)))
  (modify-syntax-entry ?\_ "_" pascal-tbl)
  (modify-syntax-entry ?\$ "_" pascal-tbl)
  (modify-syntax-entry ?\# "_" pascal-tbl)
  (modify-syntax-entry ?\% "_" pascal-tbl)
  (modify-syntax-entry ?\( "()1" pascal-tbl)
  (modify-syntax-entry ?\) ")(4" pascal-tbl)
; unfortunately, (* *) comment sequences excite a bug in (blink-matching-open)
  (modify-syntax-entry ?\* ". 23" pascal-tbl)
  (modify-syntax-entry ?\/ "." pascal-tbl)
  (modify-syntax-entry ?\+ "." pascal-tbl)
  (modify-syntax-entry ?\- "." pascal-tbl)
  (modify-syntax-entry ?\= "." pascal-tbl)
  (modify-syntax-entry ?\& "." pascal-tbl)
  (modify-syntax-entry ?\| "." pascal-tbl)
  (modify-syntax-entry ?\< "." pascal-tbl)
  (modify-syntax-entry ?\> "." pascal-tbl)
  (modify-syntax-entry ?\[ "(]" pascal-tbl)
  (modify-syntax-entry ?\] ")[" pascal-tbl)
  (modify-syntax-entry ?\{ "<" pascal-tbl)
  (modify-syntax-entry ?\} ">" pascal-tbl)
  (modify-syntax-entry ?\. "." pascal-tbl)
  (modify-syntax-entry ?\\ "." pascal-tbl)
  (modify-syntax-entry ?\: "." pascal-tbl)
  (modify-syntax-entry ?\; "." pascal-tbl)
  (modify-syntax-entry ?\' "\"" pascal-tbl)
  (modify-syntax-entry ?\" "\"" pascal-tbl)
  (setq pascal-mode-syntax-table pascal-tbl))

(defvar pascal-mode-map nil
  "Keymap used in Pascal mode.")

(let ((pascal-mp (make-sparse-keymap)))
  (define-key pascal-mp "\C-m" 'pascal-newline)
  (define-key pascal-mp "\C-j" 'newline)
  (define-key pascal-mp "\C-c\C-m" 'pascal-openline)
  (define-key pascal-mp "\C-?" 'backward-delete-char-untabify)
  (define-key pascal-mp "\C-i" 'pascal-tab)
  (define-key pascal-mp "\C-c\C-i" 'pascal-untab)
  (define-key pascal-mp "\C-c<" 'pascal-backward-to-same-indent)
  (define-key pascal-mp "\C-c>" 'pascal-forward-to-same-indent)
  (define-key pascal-mp "\C-ch" 'pascal-header)
  (define-key pascal-mp "\C-c(" 'insert-parentheses)
  (define-key pascal-mp "\C-c[" 'insert-brackets)
  (define-key pascal-mp "\C-c{" 'pascal-comment)
  (define-key pascal-mp "\C-c*" 'pascal-star-display-comment)
  (define-key pascal-mp "\C-c\C-a" 'pascal-array)
  (define-key pascal-mp "\C-cb" 'pascal-begin)
  (define-key pascal-mp "\C-cc" 'pascal-case)
  (define-key pascal-mp "\C-c\C-c" 'pascal-const)
  (define-key pascal-mp "\C-c\C-e" 'pascal-elsif)
  (define-key pascal-mp "\C-ce" 'pascal-else)
  (define-key pascal-mp "\C-c\C-p" 'pascal-procedure-spec)
  (define-key pascal-mp "\C-cp" 'pascal-subprogram-body)
  (define-key pascal-mp "\C-c\C-f" 'pascal-function-spec)
  (define-key pascal-mp "\C-cf" 'pascal-for-loop)
  (define-key pascal-mp "\C-ci" 'pascal-if)
  (define-key pascal-mp "\C-c\C-r" 'pascal-record)
  (define-key pascal-mp "\C-cr" 'pascal-repeat)
  (define-key pascal-mp "\C-c\C-t" 'pascal-type)
  (define-key pascal-mp "\C-c\C-v" 'pascal-var)
  (define-key pascal-mp "\C-c\C-w" 'pascal-with)
  (define-key pascal-mp "\C-cw" 'pascal-while)
  (define-key pascal-mp "\C-c\C-x" 'pascal-external)
  (define-key pascal-mp "\C-c=" 'pascal-show-subprogram-name)
  (define-key pascal-mp "\C-cB" 'pascal-make-bind)
  (define-key pascal-mp "\C-cC" 'pascal-compile)
  (define-key pascal-mp "\C-cI" 'pascal-include-file)
  (define-key pascal-mp "\C-cK" 'pascal-togl-key-case)
  (define-key pascal-mp "\C-cM" 'pascal-main-for-bind)
  (define-key pascal-mp "\C-cO" 'dnpas-set-options)
  (define-key pascal-mp "\C-cP" 'pascal-program)
  (define-key pascal-mp "\C-cR" 'pasmat-buffer)
  (define-key pascal-mp "\C-cS" 'pascal-tabsize)
  (define-key pascal-mp "\C-cT" 'pascal-toggle-file)
  (setq pascal-mode-map pascal-mp))

(defvar pascal-edit-prefix "Last Mod: \t"
  "*String prefixed to the timestamp from the last pascal-mode edit of this file.")

(defvar pascal-openparen-style " ("
  "*The string inserted for open parens.  Spaces may precede or follow.")

(defvar pascal-closeparen-style ")"
  "*The string inserted for close parens.  Spaces may precede or follow.")

(defvar pascal-openbrack-style "["
  "*The string inserted for open brackets.  Spaces may precede or follow.")

(defvar pascal-closebrack-style "]"
  "*he string inserted for close brackets.  Spaces may precede or follow.")

(defvar pascal-indent 4
  "*Value is the number of columns to indent in Pascal Mode.")


(defun pascal-mode ()
"This is a mode intended to support program development in Pascal.
Most control constructs and declarations of Pascal can be inserted
by typing Control-C followed by a character mnemonic for the construct.
Generally, the functions expect to be invoked right after typing \\[pascal-newline],
except for array, record, and proc/func body which start at the end of a line.
Elements of the construct to insert are prompted for.  Optional elements have
prompts in square brackets[].
\\<pascal-mode-map>
C-c TAB  indent less    TAB      indent more
C-c C-a	 array          C-c b    begin end
C-c C-c  const          C-c c    case
C-c C-e	 else if        C-c e	 else
C-c C-f	 function spec  C-c f	 for loop
C-c {    comment        C-c h	 header comment section
                        C-c i	 if
C-c C-p	 procedure spec C-c p	 proc/func body
C-c C-r	 record         C-c r    repeat until
C-c C-t	 type
C-c C-v	 var
C-c C-w	 with           C-c w	 while
C-c C-x	 external       C-c =    show subprog name
C-c (	 paired parens  C-c [	 paired brackets
C-c B	 make and bind  C-c C	 compile
C-c I    include        C-c K	 keyword case
C-c M    main obj       C-c O	 comp options   
C-c P    program stmnt  C-c R    reformat source
C-c S	 set tab size   C-c T	 toggle between body and spec

\\[pascal-backward-to-same-indent] and \\[pascal-forward-to-same-indent] move backward and forward respectively to the next line having
the same (or lesser) level of indentation, passing over labels and comments.

The number of spaces for used for indenting/undenting
is controllable by doing \\[pascal-tabsize]

Other commands of potential interest are
pascal-resize-indent-whole-buffer
pascal-backward-to-less-indent
pascal-forward-to-less-indent
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pascal-mode-map)
  (set-syntax-table pascal-mode-syntax-table)
  (setq major-mode 'pascal-mode)
  (setq mode-name "Pascal")
;;;
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
;;;
  (make-local-variable 'comment-start)
  (setq comment-start "{")
  (make-local-variable 'comment-end)
  (setq comment-end "}")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "{[ \t]*\\|(\\*[ \t]*")
  (setq comment-column 35)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 60)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'pascal-comment-indent)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'pascal-end-of-subprog-hdg)
  (setq pascal-end-of-subprog-hdg nil)
  ;; value of point at end of procedure or function heading.
  ;; set by pascal-get-arg-list. made nil after pascal-newline.
  (pascal-update-timestamp)
  (run-hooks 'pascal-mode-hook))


(defun pascal-tabsize (s)
  "Changes spacing used for indentation. Reads spacing from minibuffer."
  (interactive "nnew indentation spacing: ")
  (setq pascal-indent s))

(defun pascal-prev-line-ends-heading ()
;;; Predicate indicates whether the value of point saved right after
;;; creating a subprogram heading lies on the previous line.
  (and
    pascal-end-of-subprog-hdg
    (save-excursion
      (end-of-line 0)
      (let ((pascal-point-eol (point)))
	(forward-line 0)
	(and
	  (< (point) pascal-end-of-subprog-hdg)
	  (<=        pascal-end-of-subprog-hdg pascal-point-eol))))))

(defun pascal-indent-after-heading ()
;;; Returns the amount of indentation desired
;;; on the line just after a subprogram heading with no body.
  (+ pascal-indent (cdr (pascal-get-subprogram-name))))

(defun pascal-newline ()
  "Start new line and indent to current tab stop."
  (interactive)
  (let ((pascal-cc (current-indentation)))
    (newline)
    (indent-to pascal-cc)
    (if (pascal-prev-line-ends-heading)
	(progn
	  (pascal-tab)
	  (let ((indentwanted (pascal-indent-after-heading)))
	    (delete-horizontal-space)
	    (indent-to indentwanted))))
    (setq pascal-end-of-subprog-hdg nil)))

(defun pascal-openline ()
  "Start new line ahead of this line and indent to current tab stop."
  (interactive)
  (let ((pascal-cc (current-indentation)))
    (beginning-of-line)
    (open-line 1)
    (indent-to pascal-cc)))

(defun pascal-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) pascal-indent)) pascal-indent)))

(defun pascal-untab ()
  "Delete backwards from current indentation to previous tab stop."
  (interactive)
  (let ((pascal-cc (current-indentation)))
    (if (> pascal-cc 0)
	(progn
	  (back-to-indentation)
	  (backward-delete-char-untabify
	    (1+ (mod (1- pascal-cc) pascal-indent)) nil)))))

(defun pascal-two-lines-one-indented ()
;;; Insert two newlines, the first indented again, the second not.
  (pascal-newline)
  (pascal-tab)
  (pascal-newline)
  (pascal-untab))

(defun pascal-comment-and-temp-indent ()
;;; Insert three newlines, the first before a comment line,
;;; the second temporarily indented again, the third not.
  (let ((pascal-indentation (current-indentation)))
    (insert "\n" comment-start comment-end "\n")
    (indent-to pascal-indentation))
  (pascal-tab)
  (pascal-newline)
  (pascal-untab))


;assume no nested comments {{}} or (*(**)*)

(defun looking-back-at (string)
;;;like (looking-at) but checks chars before point against string (not regexp)
  (let ((stringstart (- (point) (length string))))
    (and
      (>= stringstart (point-min))
      (string-equal string (buffer-substring stringstart (point))))))

(defun pascal-skip-forward-white ()
;;;move point forward past any white space or comments
  (skip-chars-forward "\t- " (point-max))
  (while (looking-at "{")
    (search-forward "}" (point-max) 'move)
    (skip-chars-forward "\t- " (point-max))
    (while (looking-at "(\\*")
      (forward-char 2)
      (search-forward "*)" (point-max) 'move)
      (skip-chars-forward "\t- " (point-max))))
  (< (point) (point-max)))

(defun pascal-next-code ()
;;;move point to next line and past any white space or comments preceding code.
  (end-of-line 1)
  (pascal-skip-forward-white))

(defun pascal-skip-backward-white ()
;;;move point backward past any white space or comments
  (skip-chars-backward "\t- " (point-min))
  (while (looking-back-at "}")
    (search-backward "{" (point-min) 'move)
    (skip-chars-backward "\t- " (point-min))
    (while (looking-back-at "*)")
      (backward-char 2)
      (search-backward "(*" (point-min) 'move)
      (skip-chars-backward "\t- " (point-min))))
  (> (point) (point-min)))

(defun pascal-prev-code ()
;;;move point back to nearest previous line containing code (anything besides
;;;whitespace or comments) and move to the first code found on that line.
  (if (pascal-skip-backward-white)
      (progn
	(beginning-of-line)
	(pascal-skip-forward-white))
    nil))

(defun pascal-backward-to-this-indent (indent-level)
  "Move point back one or more lines to the start of code on the line,
until the indentation is INDENT-LEVEL or less or the start of buffer is hit.
Ignore comments, blank lines, and statement labels.
Return success predicate."
  (let ((gofurther t))
    (while (and
	     (pascal-prev-code)
	     (or
	       (looking-at "^[A-Za-z0-9_$]+:[^=]")
	       (setq gofurther (> (current-column) indent-level)))))
    (not gofurther)))

(defun pascal-forward-to-this-indent (indent-level)
  "Move point foreward one or more lines to the start of code on the line,
until the indentation is INDENT-LEVEL or less or the end of buffer is hit.
Ignore comments, blank lines, and statement labels.
Return success predicate."
  (let ((gofurther t))
    (while (and
	     (pascal-next-code)
	     (or
	       (looking-at "^[A-Za-z0-9_$]+:[^=]")
	       (setq gofurther (> (current-column) indent-level)))))
    (not gofurther)))

(defun pascal-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer.
Success predicate is returned."
  (interactive)
  (pascal-backward-to-this-indent (current-indentation)))

(defun pascal-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer.
Success predicate is returned."
  (interactive)
  (pascal-forward-to-this-indent (current-indentation)))

(defun pascal-backward-to-less-indent ()
  "Move point backwards to nearest line with less indentation.
If not found, point is left at top of buffer.
Success predicate is returned."
  (interactive)
  (pascal-backward-to-this-indent (max 0 (1- (current-indentation)))))

(defun pascal-forward-to-less-indent ()
  "Move point forwards to nearest line with less indentation.
If not found, point is left at start of last line in buffer.
Success predicate is returned."
  (interactive)
  (pascal-forward-to-this-indent (max 0 (1- (current-indentation)))))


(defun pascal-start-insert-here ()
  "Remember that point is the start of a long construct being inserted.
This makes an undo after the insertion retreat to this point."
  (undo-boundary))

(defun pascal-end-insert-here ()
  "Remember point as the end of a long construct being inserted.
Also, make this point and the following line visible on screen if it is not.
This function might be called repeatedly during any one insertion,
because of all the opportunities for keyboard interrupts during read-string."
  (save-excursion
    (end-of-line 2)
    (if (not (pos-visible-in-window-p))
	(recenter -1))))

(defun pascal-end-insert-at (pascal-line-offset)
  "Remember the point at the end of pascal-line-offset lines following
the current line as being the end of a long construct being inserted."
  (save-excursion
    (end-of-line (1+ pascal-line-offset))
    (pascal-end-insert-here)))

(defun pascal-end-subproghdg-insert-here ()
;;; End insertion here and also remember point as the end of
;;; a subprogram heading.
;;; This affects the indentation of the next pascal-newline.
  (pascal-end-insert-here)
  (setq pascal-end-of-subprog-hdg (point)))


(defvar pascal-upper-keys nil
  "*Flag desire for uppercase pascal keywords")

(defun pascal-togl-key-case ()
  "Functions toggles the value of the boolean pascal-upper-keys."
  (interactive)
  (setq pascal-upper-keys (not pascal-upper-keys))
  (message (concat "key words will now be "
		   (if pascal-upper-keys "UPPER" "lower")
		   " case.")))

(defun pascal-key (s)
  "Change case of string to be appropriate for pascal keywords."
  (if pascal-upper-keys (upcase s) (downcase s)))


(defun pascal-array ()
  "Add an array type definition,
prompting for the component type and the index subtypes."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "array") pascal-openbrack-style pascal-closebrack-style)
  (pascal-end-insert-here)
  (backward-char (length pascal-closebrack-style))
  (insert (read-string "index range(s): "))
  (end-of-line)
  (insert (pascal-key " of ;"))
  (pascal-end-insert-here)
  (backward-char)
  (insert (read-string "component type: "))
  (end-of-line)
  (pascal-end-insert-here))

(defun pascal-begin ()
  "Build a begin-end statement, prompting for a comment."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "begin"))
  (pascal-newline)
  (pascal-newline)
  (insert (pascal-key "end") "; {}")
  (backward-char 1)
  (let ((pascal-comment (read-string "comment about block: ")))
    (if (string-equal pascal-comment "")
	(progn
	  (end-of-line)
	  (backward-delete-char 3))
      (progn
	(insert pascal-comment)
	(end-of-line))))
  (pascal-end-insert-here)
  (end-of-line 0))

(defun pascal-case ()
  "Build skeleton case statement, prompting for the selector expression."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "case "))
  (pascal-end-insert-here)
  (insert (read-string "selector expression: ") (pascal-key " of"))
  (pascal-two-lines-one-indented)
  (insert (pascal-key "end") "; {case}")
  (pascal-end-insert-here)
  (end-of-line 0))

(defun pascal-rm-semi-before-else ()
;;; Delete a semicolon, if present, ending the preceding code line.
  (save-excursion
    (if (pascal-prev-code-line)
	(progn
	  (pascal-goto-end-of-code)
	  (if (equal ?\; (char-after (1- (point))))
	      (backward-delete-char 1))))))


(defun pascal-else ()
  "Add an else clause after an if-then.
Tries to delete an extraneous semicolon which might precede the else."
  (interactive)
  (pascal-start-insert-here)
  (pascal-rm-semi-before-else)
  (pascal-untab)
  (insert (pascal-key "else"))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-for-loop ()
  "Build a skeleton for statement, prompting for the loop parameters."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "for "))
  (pascal-end-insert-here)
  (insert (read-string "loop variable: ") " :=  " (pascal-key "do"))
  (pascal-end-insert-here)
  (backward-char 3)			;length of " do"
  (insert (read-string "range: "))
  (end-of-line)
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-if ()
  "Insert skeleton if statement, prompting for the boolean-expression."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "if "))
  (pascal-end-insert-here)
  (insert (read-string "condition: ") (pascal-key " then"))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-elsif ()
  "Add an else if clause to an if statement,
prompting for the boolean-expression."
  (interactive)
  (pascal-start-insert-here)
  (pascal-rm-semi-before-else)
  (pascal-untab)
  (insert (pascal-key "else if "))
  (pascal-end-insert-here)
  (insert (read-string "condition: ") (pascal-key " then"))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-insert-with-semi (pascal-str)
;;; Insert string and append semicolon if not present at end thereof.
  (insert pascal-str)
  (if (not (string-match ";$" pascal-str))
      (insert ";")))

(defun pascal-get-arg-list ()
  "Read from user a procedure or function argument list.
Add parens if one or more arguments are supplied, and insert into buffer.
Individual argument specs are stacked vertically if entered one-at-a-time.
The argument list is terminated when a CR is given instead of an argument."
  (insert pascal-openparen-style)
  (pascal-end-insert-here)
  (let ((pascal-arg-indent (current-column))
	(pascal-arg (read-string "[argument and type]: ")))
    (if (string-equal pascal-arg "")
	(backward-delete-char (length pascal-openparen-style))
      (progn
	(while (not (string-equal "" pascal-arg))
	  (pascal-insert-with-semi pascal-arg)
	  (newline)
	  (indent-to pascal-arg-indent)
          (pascal-end-insert-here)
	  (setq pascal-arg (read-string "[next argument and type]: ")))
	(delete-horizontal-space)
	(backward-delete-char 2)	; NewLine and SemiColon
	(insert pascal-closeparen-style))))
  (pascal-end-subproghdg-insert-here))

(defun pascal-function-spec (pascal-nested)
  "Insert a function specification.
Prompts for name and arguments and result type.
If given a prefix arg, indentation is chosen which assumes
the function declaration is nested inside another subprogram."
  (interactive "P")
  (pascal-start-insert-here)
  (let ((pascal-prev-indent (cdr (pascal-get-subprogram-name))))
    (delete-horizontal-space)
    (indent-to (if pascal-nested
		   (+ pascal-prev-indent pascal-indent)
		 pascal-prev-indent)))
  (insert (pascal-key "function "))
  (pascal-end-insert-here)
  (insert (read-string "function name: "))
  (pascal-get-arg-list)
  (insert ": ")
  (pascal-end-insert-here)
  (insert (read-string "result type: ") ";")
  (pascal-end-subproghdg-insert-here))

(defun pascal-procedure-spec (pascal-nested)
  "Insert a procedure specification, prompting for its name and arguments.
If given a prefix arg, indentation is chosen which assumes
the procedure declaration is nested inside another subprogram."
  (interactive "P")
  (pascal-start-insert-here)
  (let ((pascal-prev-indent (cdr (pascal-get-subprogram-name))))
    (delete-horizontal-space)
    (indent-to (if pascal-nested
		   (+ pascal-prev-indent pascal-indent)
		 pascal-prev-indent)))
  (insert (pascal-key "procedure "))
  (pascal-end-insert-here)
  (insert (read-string "procedure name: " ))
  (pascal-get-arg-list)
  (insert ";")
  (pascal-end-subproghdg-insert-here))

(defun pascal-am-in-comment ()
  "Predicate indicates whether point is in the 1st line of a comment,
i.e. past the beginning of the comment introducer."
  (let ((pascal-opoint (point))
	(pascal-eol (save-excursion (end-of-line) (point))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward comment-start-skip pascal-eol 'keep-going)
	  (> pascal-opoint (match-beginning 0))
	nil))))

(defun pascal-get-subprogram-name ()
  "Return (without moving point or mark) a pair whose CAR is
the name associated with the function or procedure statement
 which immediately precedes point,
and whose CDR is the column number at which the
function/procedure keyword was found."
  (save-excursion
    (let ((keyword-regex "\\(procedure\\|function\\|program\\)\\>")
	  (case-fold-search t))		; note dynamic binding
      (while (and
	       (pascal-backward-to-less-indent)
	       (not (looking-at keyword-regex)))
	nil)
      (if (looking-at keyword-regex)
	  (let ((pascal-proc-indent (current-column)))
	    (forward-sexp 2)
	    (let ((p2 (point)))
	      (forward-sexp -1)
	      (cons (buffer-substring (point) p2) pascal-proc-indent)))
	(cons "NAME?" 0)))))

(defun pascal-subprogram-body ()
  "Insert frame for subprogram body.  Invoke right after
pascal-function-spec or pascal-procedure-spec at end of line."
  (interactive)
  (pascal-start-insert-here)
  (pascal-newline)
  (let ((pascal-subprogram-name-col (pascal-get-subprogram-name)))
    (delete-horizontal-space)
    (insert comment-start comment-end "\n")
    (indent-to (cdr pascal-subprogram-name-col))
    (pascal-tab)
    (pascal-newline)
    (insert (pascal-key "begin"))
    (pascal-two-lines-one-indented)
    (insert (pascal-key "end") "; {" (car pascal-subprogram-name-col) "}"))
  (pascal-end-insert-here)
  (end-of-line -2))

(defun pascal-show-subprogram-name ()
  "Display in the echo area the name of the subprogram
in the closest procedure or function header found before point.
Simple minded."
  (interactive)
  (let ((pascal-sub-name (pascal-get-subprogram-name)))
    (message (concat "subprogram name is " (car pascal-sub-name) "."))))

(defun pascal-program ()
  "Insert a program statement, prompting for its name and filelist."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "program "))
  (pascal-end-insert-here)
  (insert (read-string "program name: "))
  (insert pascal-openparen-style pascal-closeparen-style ";")
  (pascal-end-insert-here)
  (backward-char (1+ (length pascal-closeparen-style)))
  (let ((pascal-filelist (read-string "program header list: ")))
    (if (string-equal pascal-filelist "")
	(progn
	  (backward-char (length pascal-openparen-style))
	  (kill-line nil)
	  (insert ";"))
      (progn
	(insert pascal-filelist)
	(end-of-line))))
  (pascal-comment-and-temp-indent)
  (insert (pascal-key "begin"))
  (pascal-two-lines-one-indented)
  (insert (pascal-key "end") ". {Program}")
  (pascal-end-insert-here)
  (end-of-line -2))


(defun pascal-record ()
  "Insert a skeleton record type declaration."
  (interactive)
  (pascal-start-insert-here)
  (if (not (point-on-blank-line))
      (progn
	(end-of-line)
	(pascal-newline)
	(pascal-tab)))
  (insert (pascal-key "record"))
  (pascal-two-lines-one-indented)
  (insert (pascal-key "end") "; {record}")
  (pascal-end-insert-here)
  (end-of-line 0))

(defun pascal-repeat ()
  "Create a repeat until statement."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "repeat"))
  (pascal-two-lines-one-indented)
  (insert (pascal-key "until ;"))
  (pascal-end-insert-here)
  (backward-char 1)
  (insert (read-string "exit condition: "))
  (end-of-line)
  (pascal-end-insert-here)
  (end-of-line 0))

(defun pascal-header-section (pascal-secname)
;;; Start a const, type, or var declaration section.
  (pascal-start-insert-here)
  (pascal-tab)
  (let ((procindent (cdr (pascal-get-subprogram-name))))
    (delete-horizontal-space)
    (indent-to procindent))
  (pascal-tab)
  (insert (pascal-key pascal-secname))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-const ()
  "Start a const section."
  (interactive)
  (pascal-header-section "const"))

(defun pascal-type ()
  "Start a type section."
  (interactive)
  (pascal-header-section "type"))

(defun pascal-var ()
  "Start a var section."
  (interactive)
  (pascal-header-section "var"))

(defun pascal-with ()
  "Create a with statement."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "with "))
  (pascal-end-insert-here)
  (insert (read-string "records to with: ") (pascal-key " do"))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-while ()
  "Create a while statement."
  (interactive)
  (pascal-start-insert-here)
  (insert (pascal-key "while "))
  (pascal-end-insert-here)
  (insert (read-string "entry condition: ") (pascal-key " do"))
  (pascal-newline)
  (pascal-tab)
  (pascal-end-insert-here))

(defun pascal-external ()
  "Create an extern or external statement."
  (interactive)
  (pascal-start-insert-here)
  (if (not (point-on-blank-line))
      (progn
	(end-of-line)
	(pascal-newline)))
  (let ((indentto (cdr (pascal-get-subprogram-name))))
    (delete-horizontal-space)
    (indent-to indentto))
  (pascal-tab)
  (insert (pascal-key (if pascal-from-berkeley "external;" "extern;")))
  (pascal-newline)
  (pascal-untab)
  (pascal-end-insert-here))

(defun pascal-include-file ()
  "Create a file inclusion statement."
  (interactive)
  (pascal-start-insert-here)
  (if (point-on-blank-line)
      (delete-horizontal-space)
    (progn
      (end-of-line)
      (insert "\n")))
  (insert (pascal-key (if pascal-from-berkeley
			  "#include \"\""
			"%include '';")))
  (pascal-end-insert-here)
  (backward-sexp 1)
  (forward-char 1)
  (insert (read-string "header file to include: "))
  (end-of-line)
  (pascal-end-insert-here))

(defun insert-brackets (arg)
  "Put square brackets around next ARG sexps.  Leave point after open-brack.
No argument is equivalent to zero: just insert [] and leave point between."
  (interactive "P")
  (insert "[")
  (save-excursion
    (if arg
	(forward-sexp (prefix-numeric-value arg)))
    (insert "]")))

(defun pascal-update-timestamp ()
  "Update the Last Mod: timestamp if found near the start of the buffer."
  (if (not buffer-read-only)
      (save-excursion
	(let ((pascal-buf-was-mod (buffer-modified-p))
	      (pascal-last-edit-marker pascal-edit-prefix))
	  (goto-char (point-min))
	  (if (re-search-forward
		pascal-last-edit-marker (+ 2000 (point-min)) t)
	      (progn
		(delete-char (- (save-excursion (end-of-line) (point))
				(point)))
		(insert (current-time-string))
		(set-buffer-modified-p pascal-buf-was-mod)))))))

(defun pascal-header (pascal-note-copyright)
  "Insert a comment block containing the module title, author, etc.
If given a prefix arg, make a copyright notice instead of an Author: entry."
  (interactive "P")
  (pascal-start-insert-here)
  (if (point-on-blank-line)
      (delete-horizontal-space)
    (progn
      (end-of-line)
      (newline)))
  (insert "{\n    Title: \t\n}")
  (pascal-end-insert-here)
  (end-of-line 0)
  (insert (read-string "Title: "))
  (insert "\n    " pascal-edit-prefix (current-time-string))
  (insert "\n    ")
  (if pascal-note-copyright
      (insert "Copyright \t" (substring (current-time-string) -4) " ")
    (insert   "Author: \t"))
  (insert (user-full-name) "\n\t\t<" (user-login-name) "@" (system-name) ">")
  (end-of-line 2)
  (pascal-end-insert-here))

(defun point-on-blank-line ()
  "Tell whether point is on a blank line or not."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t\f]*$")))

(defun pascal-comment (pascal-display-comment)
  "Insert a comment at end of this line, unless a prefix argument is given,
in which case a display comment following this line is created.
Inline comments start to the right of comment-column,
unless now past end-comment-column, in which case we start on the next line."
  (interactive "P")
  (end-of-line)
  (pascal-start-insert-here)
  (if pascal-display-comment
      (progn
	(if (point-on-blank-line)
	    (progn
	      (delete-horizontal-space))
	  (progn
	    (end-of-line)
	    (newline)))
	(insert comment-start "\n    \n" comment-end))
    (progn
      (if (> (current-column) end-comment-column) (newline))
      (indent-for-comment)))
  (pascal-end-insert-here)
  (if pascal-display-comment
      (end-of-line 0)))

(defun pascal-star-display-comment ()
  "Insert a (* *) display comment following this line."
  (interactive)
  (pascal-start-insert-here)
  (end-of-line)
  (if (point-on-blank-line)
      (delete-horizontal-space)
    (progn
      (end-of-line)
      (insert "\n")))
  (insert "(*\n *")
  (pascal-tab)
  (insert "\n *)")
  (pascal-end-insert-here)
  (end-of-line 0))

(defun pascal-comment-indent ()
  "Compute indent column for comment here."
  (if (and
	(= (current-column) 0)
	(or
	  (looking-at "{")
	  (looking-at "(\\*")
	  (= comment-column 0)))
      0
    (save-excursion
      (skip-chars-backward " \t\f")
      (max comment-column
	   (1+ (current-column))))))

(defun resize-indent-one-line (otab newtab)
  "Reindent the current line, subservient to resize-indent-whole-buffer."
  (back-to-indentation)
  (let ((curindent (current-indentation)))
    (delete-backward-char
      (- (point)
	 (save-excursion (beginning-of-line) (point))))
    (indent-to
       (+
	 (* newtab (/ curindent otab))	; whole tabs
	 (min newtab (mod curindent otab)) ; partial tabs
	 ))))

(defun resize-indent-whole-buffer (old-tab-size new-tab-size)
  "Change the indentation of all lines, using a user-supplied
old tab-spacing and a new tab-spacing.
Odd leading spaces are preserved so far as they are smaller
than the new tab spacing."
  (interactive "nold tab-size: \nnnew tab-size: ")
  (if (< old-tab-size 1)
      (message "old tab-size must be positive")
    (if (< new-tab-size 0)
	(message "new tab-size must be nonnegative")
      (progn
	(goto-char (point-min))
	(resize-indent-one-line old-tab-size new-tab-size)
	(while (= (forward-line 1) 0)
	  (resize-indent-one-line old-tab-size new-tab-size))))))


(defun pascal-prev-code-line ()
  "Move back to start of nearest preceding line containing code,
i.e. is not whitespace, a label, nor a comment.
Return nil if no such line found before beginning-of-buffer,
otherwise returns point from beginning of that line."
  (let ((pascal-no-code t))
    (while (and
	     (pascal-prev-code)
	     (setq pascal-no-code (looking-at "[A-Za-z0-9_$]+:[^=]")))
      nil)
    (if pascal-no-code nil (point))))

(defun pascal-goto-end-of-code ()
  "Move point on same line to end of code,
preceding any trailing whitespace or comment on the line.
Assumes point is on the beginning-of-line already."
  (let ((pascal-eol-point (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip pascal-eol-point 'keep-going)
	(goto-char (match-beginning 0))))
  (skip-chars-backward " \t\f"))

(defun string-match-list (sm-list-of-regex sm-list-candidate)
  (if sm-list-of-regex
      (if (string-match (car sm-list-of-regex) sm-list-candidate)
	  t
	(string-match-list (cdr sm-list-of-regex sm-list-candidate)))
    nil))
		   

(defun pascal-prev-line-continued-at ()
  "Returns a column for code continuing a previous line to begin on,
or else nil, in case the previous line is complete or nonexistent."
  (save-excursion
    (if (pascal-prev-code-line)
	(let ((pascal-bol-point (point)))
	  (pascal-goto-end-of-code)
	  (if (string-match-list pascal-line-enders
				 (buffer-substring pascal-bol-point (point)))
	      nil			; line is complete
	    (+ (current-indentation) 1 (/ (1- pascal-indent) 2))))
      nil)))				; there is no prev code line

;;;
;;; unfinished, just confirms current indent for hard cases.
;;; 
(defun pascal-related-indent (pascal-nxt-pt pascal-nxt-key pascal-prev-pt pascal-prev-key)
  (save-excursion
    (goto-char pascal-nxt-pt)
    (current-indentation)))

(defun pascal-get-starter ()
  (buffer-substring (point)
		    (save-excursion
		      (skip-chars-forward "A-Za-z0-9_$"))))

(defun pascal-compute-indentation ()
  "Return column nbr appropriate for this line of Pascal code.
Assumes point is at the current indentation."
  (save-excursion
    (let ((case-fold-search t)
	  (pascal-opoint (point)))
      (cond
	((looking-at comment-start-skip)
	 (pascal-comment-indent))
	((looking-at "[a-z0-9_$]+:[^=]")
	 0)
	((pascal-prev-line-continued-at))
	(t
	  (let (pascal-prev-point pascal-prev-key)
	    (save-excursion
	      (setq pascal-prev-point (pascal-prev-code-line))
	      (if pascal-prev-point (setq pascal-prev-key (pascal-get-starter))))
	    (if pascal-prev-point
		(pascal-related-indent (point) (pascal-get-starter)
				    pascal-prev-point pascal-prev-key)
	      0)))))))

(defun pascal-indent-line ()
  "Reindent current line to column appropriate for the code.
Mainly checks the near left context."
  (interactive)
  (let* ((pascal-beyond-indent (max 0 (- (point)
				      (progn (back-to-indentation) (point)))))
	 (pascal-computed-indentation (pascal-compute-indentation)))
    (delete-backward-char
      (- (point)
	 (save-excursion (beginning-of-line) (point))))
    (indent-to (pascal-computed-indentation))
    (forward-char pascal-beyond-indent)))

(defvar pascal-line-enders
  '(
  "^.*;"
  )
  "List of regexps which match code which can belong on the end of a line,
Even if infinite room were available for long lines.")


(defun pascal-toggle-file ()
;;; assumes specification file has name of form: name.h
;;;              and body file has name of form: name.pas or name.p
  "Toggle between body and specification files for the program."
  (interactive)
  (cond
    ((string-equal (substring (buffer-file-name) -4) ".pas")
     (find-file-other-window
       (concat (substring (buffer-file-name) 0 -4) ".h")))
    ((string-equal (substring (buffer-file-name) -2) ".p")
     (find-file-other-window
       (concat (substring (buffer-file-name) 0 -2)  ".h")))
    ((string-equal (substring (buffer-file-name) -2) ".h")
     (find-file-other-window
       (let ((pascal-file (concat (substring (buffer-file-name) 0 -2)
				  ".pas")))
	 (if (file-readable-p pascal-file)
	     pascal-file
	   (substring pascal-file -2)))))
    (t
      (error "pascal-toggle-file does not know how to find the other file."))))


(defvar pascal-from-berkeley (let ((vers "(berkeley-unix)"))
			       (string-equal
				 (substring (emacs-version) (- (length vers)))
				 vers))
  "*Flag indicating pascal compiler used is berkeley pc.")


;;; compilation code for Domain Pascal and Berkeley pc compilers

(defun pascal-compile ()
  "Compile pascal program."
  (interactive)
  (compile (if pascal-from-berkeley
	       (concat "pc -c " pascal-compile-opts " " (buffer-file-name))
	     (concat "pas " (buffer-file-name) " " pascal-compile-opts))))

(defun dnpas-make-bind ()
  "Compile and link program by making."
  (interactive)
  (compile (concat "make " (pascal-main-prog))))


(defvar pascal-compile-opts ""
  "*Options to supply for Pascal compiling.")

(defun dnpas-set-options ()
  "Specify options needed for Domain Pascal compiler.
Empty responses cause no change; blank responses nullify the previous options."
  (interactive)
  (let ((pascal-options (read-string "options for pascal compile: ")))
    (if (not (string-equal pascal-options ""))
	(setq pascal-compile-opts pascal-options))))

(defvar pascal-main-prog "" "*Name of main program for binding.")
(defun pascal-main-prog ()
  "Supply name of main program unit needed for binding."
  (while (string-match "^[ \t\f]$" pascal-main-prog)
    (call-interactively 'pascal-main-for-bind))
  pascal-main-prog)

(defun pascal-main-for-bind (pascal-main)
  "Specify name of main program unit needed for binding."
  (interactive "sname of executable program to be made: ")
  (setq pascal-main-prog pascal-main))


(defun pasmat-buffer ()
  "Save buffer and replace Pascal source with reformatted version."
  (interactive)
  (let ((oldbuffer (current-buffer))
	(bufwasmod (buffer-modified-p))
	(tmpbuffer nil)
	(sourcename nil))
    (unwind-protect
	(progn
	  (if bufwasmod (save-buffer))
	  (setq sourcename (buffer-file-name))
	  (setq tmpbuffer (get-buffer-create "*Pascal Pasmat*"))
	  (set-buffer tmpbuffer)
	  (erase-buffer)
	  (message (if bufwasmod
		       "buffer saved, running pasmat..."
		     "running pasmat..."))
	  (call-process "pasmat"
			nil t nil
			"-s"
			(concat "{r"
				(if pascal-upper-keys "+" "-")
				",t="
				(int-to-string pascal-indent)
				"}")
			sourcename)
	  (goto-char (point-min))
	  (set-buffer-modified-p nil)
	  (set-buffer oldbuffer)
	  (undo-boundary)
	  (goto-char (point-min))
	  (insert-buffer-substring tmpbuffer) ;add the new
	  (delete-region (point) (point-max)) ;takeaway the old
	  (message "running pasmat... done."))
      (set-buffer oldbuffer)
      (if tmpbuffer (kill-buffer tmpbuffer)))))
