Received: from pizza by PIZZA.BBN.COM id aa09538; 19 Oct 88 5:06 EDT
Received: from BBN.COM by PIZZA.BBN.COM id aa09534; 19 Oct 88 5:04 EDT
Received: from USENET by bbn.com with netnews
	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
	contact usenet@bbn.com if you have questions.
To: unix-emacs@BBN.COM
Date: 18 Oct 88 21:21:26 GMT
From: Mosur Mohan <tektronix!sequent!mntgfx!mosurm@beaver.cs.washington.edu>
Sender: arpa-unix-emacs-request@BBN.COM
Subject: Re: Wanted: Pascal Minor mode for Gnu Emacs
Reply-To: uunet!mntgfx!mosurm@beaver.cs.washington.edu
Message-ID: <1988Oct18.142128.1638@mntgfx.mentor.com>
References: <LAZLOR.88Oct5210131@ucscb.UCSC.EDU>, <23772@tut.cis.ohio-state.edu>, <1988Oct11.110614.1154@mntgfx.mentor.com>
Organization: Mentor Graphics Corporation, Beaverton Oregon
Source-Info:  From (or Sender) name not authenticated.

Here's the pascal-mode I wrote, with capabilities to scan blocks and
do a reasonably good job of auto-indenting code.  I also modified the
etags program to build tags for Pascal code (unfortunately, not for
type-declarations, I didn't have time to get to that); I'll post the
etags program separately.

Enjoy!  -- Mohan.   {uunet!mntgfx!mosurm}
                    {Mosur Mohan, Mentor Graphics, Beaverton, OR}
================================================================================
;;; Modified by Mosur Mohan 15-Apr-88 <uunet!mntgfx!mosurm> 
;;; Pascal editing support package, based on:

;; Originally, Modula-2 editing support package
;; Author Mick Jordan
;; Amended Peter Robinson
;; Ported to GNU Michael Schmidt
;; From: "Michael Schmidt" <michael@pbinfo.UUCP>
;;; Modified by Tom Perrine <Perrin@LOGICON.ARPA> (TEP)

(defvar pas-mode-syntax-table nil
  "Syntax table in use in Pascal-mode buffers.")
(defvar pas-mode-abbrev-table nil
  "Abbrev table in use in pas-mode buffers.")
(define-abbrev-table 'pas-mode-abbrev-table ())

(if pas-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( "() 1" table)
    (modify-syntax-entry ?\) ")( 4" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?{ "<" table)
    (modify-syntax-entry ?} ">" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\' "\"" table)
    (setq pas-mode-syntax-table table)))

;;; Added by MKM
(defvar pas-mode-map nil
  "Keymap used in Pascal-mode.")

(if pas-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-i" 'pas-indent-line)
    (define-key map "\C-c\C-i" 'pas-tab-to-tab-col)
    (define-key map "\C-j" 'pas-newline)
    (define-key map "\C-cb" 'pas-begin)
    (define-key map "\C-c\C-b" 'backward-block)
    (define-key map "\C-c\C-f" 'forward-block)
    (define-key map "\C-c\C-d" 'down-block)
    (define-key map "\C-c\C-e" 'up-block)
    (define-key map "\C-c\C-u" 'back-up-block)
    (define-key map "\C-c\C-@" 'mark-block)
    (define-key map "\C-c\C-n" 'narrow-to-block)
    (define-key map "\C-c~" 'self-assign-stmt)
    (define-key map "\C-c\[" 'open-comment-box)
    (define-key map "\C-c\C-m" 'continue-comment-box)
    (define-key map "\C-c\>" 'set-end-comment-col)
    (define-key map "\C-c}" 'pas-end-comment)
    (setq pas-mode-map map)))

(defvar pas-indent 2 "*This variable gives the indentation in Pascal-mode")

(defun pas-mode ()
"Mode to support program development in Pascal.
The prefix-key for pas-mode is Ctrl-C.

  TAB            pas-indent-line       Ctrl-c TAB     pas-tab-to-tab-col
  Ctrl-j         pas-newline           Ctrl-c b       pas-begin
  Ctrl-c Ctrl-f  forward-block         Ctrl-c Ctrl-b  backward-block
  Ctrl-c Ctrl-d  down-block            Ctrl-c Ctrl-u  back-up-block
  Ctrl-c Ctrl-e  up-block              Ctrl-c Ctrl-@  mark-block
  Ctrl-c Ctrl-n  narrow-to-block       Ctrl-c ~       self-assign-stmt
  Ctrl-c Ctrl-[  open-comment-box      Ctrl-c Ctrl-m  continue-comment-box
  Ctrl-c }       pas-end-comment       Ctrl-c >       set-end-comment-column

  pas-indent controls the number of spaces for each indentation."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pas-mode-map)
  (setq major-mode 'pas-mode)
  (setq mode-name "Pascal")
  (setq local-abbrev-table pas-mode-abbrev-table)
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'box-com-col)
  (setq box-com-col 2)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 66)
  (set-syntax-table pas-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (setq indent-tabs-mode nil)
  (make-local-variable 'pas-tab-col)
  (setq pas-tab-col 20)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'pas-mode-hook))


(defun pas-indent-line ()
  "Indent the current line based on the indentation of the
surrounding Pascal block, and on whether the previous line
ended a Pascal statement."
  (interactive)
  (let (blk-ind blk-beg prev-ind prev-beg shift-amt keep-going fishy)
    (save-excursion
      (beginning-of-line)
      (setq fishy (not (backward-scan-blocks 1 nil nil)))
      (beginning-of-line)
      (setq blk-beg (point))
      (setq blk-ind (current-indentation))
      (if fishy
        (setq indent (+ blk-ind pas-indent)) );; E-O-IF
      );; E-O-SAVE EXCURSION
    (if fishy nil
      (save-excursion
        (forward-line -1)
        (setq prev-beg (point))
        (setq prev-ind (current-indentation))
        (if (<= prev-beg blk-beg)       ; prev line is containing block
          (setq indent (+ blk-ind pas-indent))
          (skip-chars-forward " \t")
          (if (looking-at "\\<if\\>\\|\\<case\\>\\|\\<with\\>\\|\\<for\\>\\|\\<while\\>\\|\\<repeat\\>")
            (setq indent (+ prev-ind pas-indent)) ; then
            (setq indent (+ blk-ind pas-indent)) ; else
            (end-of-line)
            (if (or
                  (re-search-backward ";[ \t]*\\((\\*.*\\*)\\)*$" prev-beg t 1)
                  (re-search-backward "^ *(\\*.*\\*)$" prev-beg t 1)
                  (re-search-backward "^$" prev-beg t 1)
                  (= (point) prev-beg) )
              nil                       ; then block-indent will do
              (setq indent (+ prev-ind pas-indent)) ; else use previous-line indent
              )));; E-O-3 IFs
        ));; E-O-SAVE EXCURSION & IF
    (save-excursion
      (beginning-of-line)
      (setq prev-beg (point))
      (skip-chars-forward " \t")
      (if (and (not fishy) (looking-at "end\\|until"))
        (setq indent blk-ind)
        (save-excursion
          (cond ((looking-at "then")
                 (backward-find-kwd "\\<if\\>" nil)
                 (setq indent (+ (current-indentation) pas-indent)) )
                ((looking-at "else")
                 (setq then-cnt 1)
                 (setq keep-going t)
                 (while keep-going
                   (backward-find-kwd "\\<then\\>\\|\\<else\\>" nil)
                   (if (looking-at "then")
                       (setq then-cnt (1- then-cnt))
                     (setq then-cnt (1+ then-cnt)) )
                   (if (> then-cnt 0) nil
                     (setq keep-going nil)
                     (setq indent (current-indentation)) ));; E-O-WHILE
                 )
                (t nil) );; E-O-COND
          ));; E-O-SAVE EXCURSION & IF
      ;; install the right indentation
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt) nil
        (delete-region prev-beg (point))
        (indent-to indent) )
      );; E-O-SAVE EXCURSION
    (if (bolp) (back-to-indentation))
    ));; E-O-LET & PAS-INDENT-LINE

(defun pas-tab-to-tab-col (&optional arg)
  "Insert space to force indent to specified ARG column,
or to pas-tab-col."
  (interactive "P")
  (if arg
    (if (integerp arg)
      (setq pas-tab-col arg)            ; then
      (setq pas-tab-col (current-column)) ; else
      ))
  (indent-to pas-tab-col));; E-O-PAS TAB TO TAB-COL

(defun pas-newline ()
  "Insert a newline and indent it appropriately."
  (interactive)
  (newline)
  (pas-indent-line) );; E-O-PAS-NEWLINE

(defun pas-end-comment ()
  "Finish this comment correctly right-aligned."
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column 1))
  (insert "*)"))

(defun set-end-comment-column ()
  "Set the Pascal mode local variable end-comment-column
   to the column that point is on."
  (interactive)
  (message (concat "end-comment-column set to "
    (setq end-comment-column (current-column)) )))

(defun open-comment-box (arg)
  "Open a box comment: set box-com-col to the current
column.  Now, read the char to use for the comment line,
then insert two lines and open an aligned comment box."
  (interactive "cComment-line char: ")
  (setq box-com-col (current-column))
  (insert "(*")
  (let ( (counter 1)
         (lsize (- end-comment-column box-com-col)) )
    (while (< (setq counter (1+ counter)) lsize)
      (insert arg) )
    (insert "*)\n")
    (indent-to box-com-col 0)
    (insert "(*")
    (setq counter 1)
    (while (< (setq counter (1+ counter)) lsize)
      (insert arg) )
    (insert "*)")
    (beginning-of-line)
    (open-line 1)
    (indent-to box-com-col)
    (insert "(*  ") )                   ;; E-O-LET
  )   ;; E-O-OPEN-COMMENT-BOX


(defun continue-comment-box ()
  "Close current-line comment correctly right-aligned, open a new
indented comment on the next line, and indent to pas-tab-col."
  (interactive)
  (indent-to end-comment-column 1)
  (insert "*)\n")
  (indent-to box-com-col)
  (insert "(*")
  (indent-to pas-tab-col 2) )   ;; E-O-CONTINUE-COMMENT-BOX

(defun pas-begin (&optional arg)
  "Insert a 'begin' keyword & its comment at point, and
matching 'end'.  If ARG >= 1, insert the 'end' ARG lines
elow 'begin'.  If ARG < 0, insert 'end' at mark, and indent."
  (interactive "P")
  (let ((cmnt (read-string "Comment: "))
         (cur-pt 0)
         (cur-ind (current-indentation))
         (argval (if arg
                   (if (eq arg '-) -1 arg)
                   0)) )
    (insert "begin")
    (if (string-equal cmnt "") nil
      (setq cmnt (concat "   (* " cmnt " *)"))
      (insert cmnt) )                   ;; E-O-IF
    (setq cur-pt (point))
    (cond
      ((> argval 0)
        (next-line argval)
        (end-of-line) )
      ((< argval 0)
        (exchange-point-and-mark)
        (beginning-of-line)
        (backward-char 1) ))
    (newline)
    (indent-to cur-ind)
    (insert "end;")
    (if (string-equal cmnt "") nil
      (insert cmnt) )                   ;; E-O-IF
    (goto-char cur-pt)
    (if (= argval 0) (pas-newline))
    );; E-O-LET
  );; E-O-PAS BEGIN


(defun forward-find-kwd (target lim)
  "Leave point at the end of a keyword and return the position
of the beginning of the matched keyword, skipping comments
and literal strings en route.  If TARGET is specified, find it
outside comments & strings until limit LIM is reached.  If not
found, return NIL."
  (let ( (keep-looking t)
         (reg-str
           (concat (or target "\\<begin\\>\\|\\<end\\>\\|\\<record\\>\\|\\<case\\>\\|\\<repeat\\>\\|\\<until\\>")
             "\\|(\\*\\|{\\|""\\|'"))
         found mbeg mend next-target)
    (while keep-looking
      (setq found (re-search-forward reg-str lim t 1))
      (if (not found)
        ;;; then... didn't find any of the REG-STR components
        (setq keep-looking nil)
        ;;; else... goto beginning of match, check it out
        (setq mend (match-end 0))
        (goto-char (match-beginning 0))
        (setq mbeg (point))
        (cond
          ((and target (looking-at target))
            (setq keep-looking nil) )
          ((looking-at "(\\*") (setq next-target "*)"))
          ((looking-at "{") (setq next-target "}"))
          ((looking-at "'") (setq next-target "'"))
          ((looking-at """") (setq next-target """"))
          (t  (setq keep-looking nil)) );; E-O-COND
        (goto-char mend)
        (if keep-looking (search-forward next-target nil t 1)) );; E-O-OUTER IF
      )   ;; E-O-WHILE
    (and found mbeg)                    ; return-value = match-beginning
    );; E-O-LET
  );; E-O-FORWARD-FIND-KWD

(defun backward-find-kwd (target lim)
  "Leave point at the beginning of a keyword and return the
position of the end of the matched keyword, skipping comments
and literal strings en route.  If TARGET is specified, find it
outside comments & strings until limit LIM is reached.  If not
found, return NIL."
  (let ( (keep-looking t)
         (reg-str
           (concat (or target "\\<begin\\>\\|\\<end\\>\\|\\<record\\>\\|\\<case\\>\\|\\<repeat\\>\\|\\<until\\>")
             "\\|\\*)\\|}\\|""\\|'"))
         found mbeg mend next-target)
    (while keep-looking
      (setq found (re-search-backward reg-str lim t 1))
      (if (not found)
        ;;; then... didn't find any of the REG-STR components
        (setq keep-looking nil)
        ;;; else... we're at beginning of match, check it out
        (setq mend (match-end 0))
        (setq mbeg (point))
        (cond
          ((and target (looking-at target))
            (setq keep-looking nil) )
          ((looking-at "\\*)") (setq next-target "(*"))
          ((looking-at "}") (setq next-target "{"))
          ((looking-at "'") (setq next-target "'"))
          ((looking-at """") (setq next-target """"))
          (t  (setq keep-looking nil)) );; E-O-COND
        (if keep-looking (search-backward next-target nil t 1)) );; E-O-OUTER IF
      )   ;; E-O-WHILE
    (and found mend)                    ; return-value = match-end
    );; E-O-LET
  );; E-O-BACKWARD-FIND-KWD


(defun forward-scan-blocks (depth target lim)
  "Move forward:
   down into blocks if DEPTH < 0,
   across one block if DEPTH = 0,
   up out of blocks if DEPTH > 0.
Second arg TARGET = nil initially, used internally
to distinguish between until and end.
LIM bounds the search."
  (or target (setq target ""))
  (let (mbeg mend done fishy)
    (if (not (setq mbeg (forward-find-kwd nil lim)))
      (setq fishy t)                    ; bad location
      (setq mend (point))               ; else process kwd
      (goto-char mbeg)
      (cond
        ((looking-at "begin\\|case\\|record\\|repeat")
          (setq depth (1+ depth))
          (if (= depth 0) (setq done t)
            (if (looking-at "repeat")
              (setq target "until")     ; then
              (setq target "end") ))    ; else
          (goto-char mend) )
        ((looking-at "end\\|until")
          (if (<= depth 0)
            (setq fishy t)              ; bad location
            (setq depth (1- depth))     ; else...
            (if (and (= depth 0) (looking-at target))
              (setq done t) )
            (goto-char mend)
            (setq target nil) ))
        );; E-O-COND
      (if fishy nil                     ; return bad status
        (or done (forward-scan-blocks depth target lim)) ) ; else recurse
      );; E-O-MAIN IF
    );; E-O-LET
  );; E-O-FORWARD-SCAN-BLOCKS

(defun backward-scan-blocks (depth target lim)
  "Move backward:
   down into blocks if DEPTH < 0,
   across one block if DEPTH = 0,
   up out of blocks if DEPTH > 0.
Second arg TARGET = nil initially, used internally
to distinguish between until and end.
LIM bounds the search."
  (or target (setq target ""))
  (or lim (setq lim nil))
  (let (mend done fishy)
    (if (not (setq mend (backward-find-kwd nil lim)))
      (setq fishy t)                    ; bad location
      (cond                             ; else process kwd
        ((looking-at "end\\|until")
          (setq depth (1+ depth))
          (if (= depth 0) (setq done t)
            (if (looking-at "until")
              (setq target "repeat")    ; then
              (setq target "begin\\|case\\|record\\|repeat") ; else
              )))
        ((looking-at "begin\\|case\\|record\\|repeat")
          (if (<= depth 0)
            (setq fishy t)
            (setq depth (1- depth))
            (if (and (= depth 0) (looking-at target))
              (setq done t) )
            (setq target nil) ))
        );; E-O-COND
      (if fishy nil                     ; return bad status
        (or done (backward-scan-blocks depth target lim)) ) ; else recurse
      );; E-O-MAIN IF
    );; E-O-LET
  );; E-O-BACKWARD SCAN BLOCKS


(defun forward-block (&optional numblks)
  "Move forward across NUMBLKS balanced begin-end blocks."
  (interactive "p")
  (or numblks (setq numblks 1))
  (if (< numblks 0) (backward-block (- numblks))
    (let (found-pos fishy)
      (save-excursion
        (while (> numblks 0)
          (if (forward-scan-blocks 0 nil nil)
            (setq numblks (1- numblks)) ; then... all's well
            (setq fishy t)              ; else exit
            (setq numblks 0) )
          );; E-O-WHILE
        (setq found-pos (point)) );; E-O-SAVE-EXCURSION
      (if (not fishy)
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      ));; E-O-LET & IF
  );; E-O-FORWARD-BLOCK

(defun backward-block (&optional numblks)
  "Move backward across NUMBLKS balanced begin-end block."
  (interactive "p")
  (or numblks (setq numblks 1))
  (if (< numblks 0) (forward-block (- numblks))
    (let (found-pos fishy)
      (save-excursion
        (while (> numblks 0)
          (if (backward-scan-blocks 0 nil nil)
            (setq numblks (1- numblks)) ; then... all's well
            (setq fishy t)              ; else exit
            (setq numblks 0) )
          );; E-O-WHILE
        (setq found-pos (point)) );; E-O-SAVE-EXCURSION
      (if (not fishy)
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      ));; E-O-LET & IF
  );; E-O-BACKWARD-BLOCK

(defun down-block (&optional arg)
  "Move forward down ARG levels of begin-end block.
A negative argument means move backward but still down."
  (interactive "p")
  (or arg (setq arg 1))
  (let (found-pos all-swell)
    (save-excursion
      (setq all-swell
        (if (> arg 0)
          (forward-scan-blocks (- arg) nil nil) ; then
          (backward-scan-blocks arg nil nil) ; else
          ));; E-O-IF & SETQ
      (setq found-pos (point)) );; E-O-SAVE-EXCURSION
      (if all-swell
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") );; E-O-IF
    );; E-O-LET
  );; E-O-DOWN-BLOCK

(defun back-up-block (&optional arg)
  "Move backward out of ARG levels of begin-end blocks.
   A negative argument means move forward but still up."
  (interactive "p")
  (or arg (setq arg 1))
  (up-block (- arg)))

(defun up-block (&optional arg)
  "Move forward out of ARG levels of begin-end blocks.
   A negative argument means move backward but still up."
  (interactive "p")
  (or arg (setq arg 1))
  (let (found-pos all-swell)
    (save-excursion
      (setq all-swell
        (if (> arg 0)
          (forward-scan-blocks arg nil nil) ; then
          (backward-scan-blocks (- arg) nil nil)
          ));; E-O-IF & SETQ
      (setq found-pos (point)) );; E-O-SAVE-EXCURSION
      (if all-swell
        (goto-char found-pos)           ; happy ending
        (push-mark (point) t)           ; else mark and warn
        (goto-char found-pos)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") );; E-O-IF
    );; E-O-LET
  );; E-O-UP-BLOCK


(defun mark-block (&optional arg)
  "Set mark at the end of the next block from point.
With argument, do this that many blocks away.  Leave
the cursor at top-of-region."
  (interactive "p")
  (or arg (setq arg 1))
  (let (save-loc all-swell)
    (save-excursion
      (setq all-swell
            (forward-block arg))
      (end-of-line)
      (setq save-loc (point)) );; E-O-IF & SAVE-EXCURSION
    (push-mark save-loc 1)
    (if all-swell
        (message "Block marked.")
      (send-string-to-terminal "")
      (message "Bad block structure, mark set.") )
    );; E-O-LET
  );;E-O-MARK-BLOCK

(defun narrow-to-block (&optional arg)
  "Narrow window down to the next block ahead from the cursor.
   With argument, do this that many blocks ahead (or back)."
  (interactive "p")
  (or arg (setq arg 1))
  (let ( (reg-beg (point))
         (reg-end 0)
         all-swell)
    (save-excursion
      (cond
        ((< arg 0)
          (setq all-swell (backward-block (- arg)))
          (beginning-of-line)
          (setq reg-end (point)) )
        (t
          (setq all-swell (forward-block arg))
          (end-of-line)
          (setq reg-end (point)) ));; E-O-COND
      );; E-O-SAVE-EXCURSION
    (cond
      (all-swell
        (narrow-to-region reg-beg reg-end)
        (goto-char (min reg-beg reg-end)) )
      (t
        (push-mark reg-end)
        (send-string-to-terminal "")
        (message "Bad block structure, mark set.") )
      );; E-O-COND
    );; E-O-LET
  );; E-O-NARROW-TO-BLOCK

(defun self-assign-stmt ()
  "Given variable X typed in, generate X := X."
  (interactive)
  (let (cur-pt var-end tmpstr)
    (setq cur-pt (point))
    (skip-chars-backward " \t")
    (setq var-end (point))
    (skip-chars-backward "^ \t\n")
    (setq tmpstr (buffer-substring (point) var-end))
    (goto-char cur-pt)
    (insert " := " tmpstr " ") ))
