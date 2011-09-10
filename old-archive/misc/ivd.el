;From nap1!ames!amdahl!apple!gem.mps.ohio-state.edu!uakari.primate.wisc.edu!uflorida!novavax!hcx1!jason Wed Nov 22 20:28:01 EST 1989
;Article 914 of comp.emacs:
;Xref: ark1 comp.emacs:914 gnu.emacs:769
;Path: ark1!nap1!ames!amdahl!apple!gem.mps.ohio-state.edu!uakari.primate.wisc.edu!uflorida!novavax!hcx1!jason
;>From: jason@SSD.HARRIS.COM (Jason Baietto)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: Insert Variable Dump (the poor person's debugger)
;Message-ID: <2102@hcx1.UUCP>
;Date: 21 Nov 89 17:13:06 GMT
;References: <603@uncle.UUCP>
;Sender: news@hcx1.UUCP
;Distribution: na
;Lines: 225
;
;People who write test programs a lot, or people who "hack" away at
;bugs by inserting printf statements into their code should find this
;very useful.  While editing a C file, positioning the cursor on a
;variable and typing C-ci will insert the variable into a list of
;variables.  After the variables list is complete, typing C-cp will
;insert a fully formatted printf statement at point.  Typing C-cr will
;reset the variable list.  Typing C-cv will bring up the variable list
;window which allows you to enter variables manually and also to enter
;printf format specifiers to be used in the printf format string.
;
;I've used it extensively in the last week, but there may be bugs and
;I'd appreciate feedback.  Hope somebody finds this useful.  Enjoy!
;
;-------------------------------cut-here----------------------------------

;; Insert Variable Dump
;; (the poor person's C debugger)
;; Author: Jason Baietto
;;         jason@ssd.csd.harris.com
;; Date:   November 20, 1989
;; This is not part of GNU emacs.

(define-key global-map "\C-ci" 'insert-in-variable-list)
(define-key global-map "\C-cv" 'select-variable-list)
(define-key global-map "\C-cr" 'reset-variable-list)

(defvar variable-list nil "List of variables used for insert-variable-dump")

(defun select-variable-list()
"\nSelects the *variable-list* buffer in which C variables may be entered,
one per line, optionally followed by space and a printf type specifier 
(default \"%d\").  Upon invocation of the function insert-variable-dump,
a printf statement with the variables listed in the *variable-list* window
will be generated and inserted after point in the current buffer.  It
will have the form:  printf(\"var1=%d var2=%s etc.\\n\", var1, var2, etc.);
\nA sample *variable-list* buffer might look like:
\nfoo\nvar 0x%08x\n(char)bar %c
\nSince spaces separate the variable name from its type specifier, no spaces
may exist in either the variable name or type specifier.  The function
insert-in-variable-list can be used to insert the variable around point in
the current buffer into the variable list.  Also, leading space effectively
comments out a variable in the list (which can be useful for big lists)."
   (interactive)
   (if (not (equal (buffer-name) "*variable-list*"))
      (switch-to-buffer-other-window "*variable-list*")
   )
)

(defun build-variable-list()
"Build the variable-list from the contents of the *variable-list* buffer."
   (save-excursion
      (switch-to-buffer "*variable-list*")
      (let
         (
            (lines (count-lines (point-min) (point-max)))
            (i 0)
            possible-variable
            possible-format
         )
         (goto-char (point-min))
         (while (<= i lines)
            (beginning-of-line)
            ;; Get the variable name.
            (setq possible-variable (re-around-point "[^ \t\n]" "[ \t\n]"))
            (if possible-variable
               ;; We found a variable.
               (progn
                  ;; Get any format string.
                  (if (re-search-forward "[ \t][ \t]*" (eol-location) t)
                     ;; We found space.
                     (progn
                        (setq possible-format
                           (re-around-point "[^ \t\n]" "[ \t\n]")
                        )
                        (if (not possible-format)
                           (setq possible-format "%d")
                        )
                     )
                     ;; No space, so no format string
                     (setq possible-format "%d")
                  )
                  ;; Add variable/format list to the master list.
                  (setq variable-list 
                     (append 
                        variable-list 
                        (list (list possible-variable possible-format))
                     )
                  )
               )
            )
            ;; Check the next line for variables.
            (next-line 1)
            (beginning-of-line)
            (setq i (1+ i))
         )
      )
      ;; Reset the state of the buffer to not modified
      (set-buffer-modified-p nil)
   )
)

(defun insert-variable-dump()
"Insert a printf statement composed of variables in the *variable-list*
buffer.  See select-variable-list for more details."
   (interactive)
   (let*
      (
         (variable-list-buffer (get-buffer "*variable-list*"))
         (variable-list-modified (buffer-modified-p variable-list-buffer))
      )
      (if variable-list-buffer
         ;; The buffer exists
         (if (not variable-list-modified)
            ;; The buffer exists and is not modified so there is no
            ;; reason to rebuild the list.  Just dump the old list.
            (insert-printf-statement)
            ;; The buffer exists but is modified so build and dump.
            (setq variable-list nil)
            (build-variable-list)
            (insert-printf-statement)
         )
         ;; The buffer doesn't exist
         (message "(no variable list)")
      )
   )
)

(defun insert-printf-statement()
"Does the actual insertion of the printf statement into current buffer."
   (let
      (
         (current-list (car variable-list))
         (rest-of-variable-list (cdr variable-list))
      )
      (if (not variable-list)
         ;; variable list is empty
         (message "(empty variable list)")
         ;; else, print away.
         (insert "printf(\"")
         (while current-list
            ;; Output each variable/format pair for the printf string.
            (insert (car current-list) "=" (car (cdr current-list)) " ")
            (setq current-list (car rest-of-variable-list))
            (setq rest-of-variable-list (cdr rest-of-variable-list))
         )
         (delete-backward-char 1)
         (insert "\\n\", ")
         (setq current-list (car variable-list))
         (setq rest-of-variable-list (cdr variable-list))
         (while current-list
            ;; Output each variable for the argument list.
            (insert (car current-list) ", ")
            (setq current-list (car rest-of-variable-list))
            (setq rest-of-variable-list (cdr rest-of-variable-list))
         )
         (delete-backward-char 2)
         (insert ");")
      )
   )
)

(defun insert-in-variable-list()
"Find the variable name around point and insert into *variable-list*.
A variable name here is defined as that which is composed of only
letters, digits, and underscores.  See select-variable-list."
   (interactive)
   (let
      (
         (possible-variable (re-around-point "[A-Za-z0-9_]" "[^A-Za-z0-9_]"))
      )
      (if possible-variable
         ;; We found a variable, insert it into the *variable-list* buffer.
         (save-excursion
            (switch-to-buffer "*variable-list*")
            (goto-char (point-max))
            (insert possible-variable)
            (newline)
            (message (concat "\"" possible-variable "\""))
         )
         ;; Otherwise, complain
         (message "(variable not found)")
      )
   )
)
   
(defun re-around-point(containing-re delimiting-re)
"Return a symbol composed of CONTAINING-RE delimited by DELIMITING-RE."
   (save-excursion
      (if 
         (and 
            (not (looking-at containing-re))
            (not (looking-back containing-re))
         )
         ;; Point is not on a word, so return nil.
         nil
         ;; Otherwise, get the word
         (if (re-search-backward delimiting-re (bol-location) 1)
            (forward-char 1)
         )
         (setq variable-start (point))
         (if (re-search-forward delimiting-re (eol-location) 1)
            (backward-char 1)
         )
         (setq variable-end (point))
         ;; Return the re-string found
         (buffer-substring variable-start variable-end)
      )
   )
)

(defun reset-variable-list()
"Remove all variables currently in the *variable-list* buffer"
   (interactive)
   (save-excursion
      (switch-to-buffer "*variable-list*")
      (delete-region (point-min) (point-max))
      (message "(variable list reset)")
   )
)
;=================================
;          Jason Baietto
;Harris Computer Systems Division
;     Fort Lauderdale, Florida
;jason@ssd.csd.harris.com (usenet)
;=================================


