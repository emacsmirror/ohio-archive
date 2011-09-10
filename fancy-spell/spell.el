;; Spelling correction interface for Emacs.
;; Copyright (C) 1985 Richard M. Stallman.
;; This file is part of GNU Emacs.
;;
;; Almost completely rewritten by Bengt Martensson
;; LastEditDate "Thu Aug 20 19:50:16 1987"
     
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
     
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
     
(defvar spell-options '((text-mode nil "spell")
			(mail-mode kill-mail-header "spell")
			(tex-mode detex "spell -t")
			(latex-mode delatex "spell -l")
			(emacs-lisp-mode only-double-quote-strings "spell")
			(lisp-mode only-double-quote-strings "spell")
			(c-mode only-double-quote-strings "spell")
			(pascal-mode only-single-quote-strings "spell")
			(fundamental-mode nil "spell")) 
  "*List of options for spell.  Each element is a list whose first element
is the name of a major mode, the second element is nil or the name of
a lisp-function to apply before the spell command is given, and the
third is the actual spell-command given to the shell.  MUST contain
fundamental-mode, which plays the role of default.")
(defvar spell-wait t
  "*If nil spell-region and spell-buffer will run in a separate process.")
(defvar personal-word-list "~/.mywords"
  "*Name of the personal, unsorted word list.")
(defvar spell-cautious t
  "*Setting this non-nil will give inquire the user for continuation in some 
instances when running correct-spelling.")
(defvar spell-search-first-string "egrep"
  "First part of command to search for word.  The regular expression to search
for goes after this.")
(defvar spell-search-last-string "/usr/dict/words"
  "Last part of command to search for word.  The regular expression to search
for goes before this.")
;;(defconst spell-window-height 4 "Height of the window for running spell.")
(defconst spell-directory "/tmp/" "Directory to place the temporary files in.")
(defconst cd-string "cd" "String to change the name of default directory.")
(defconst semi-colon ";" "Make this "";"" in unix, ""\\n"" otherwise.")
(defconst redirection-string ">" "To redirect output.")
(defvar spell-in-file "" "Temporary file for running spell from EMACS")
(setq spell-in-file (make-temp-name (concat spell-directory "spelli")))
(defvar spell-out-file "" "Temporary file for running spell from EMACS")
(setq spell-out-file (make-temp-name (concat spell-directory "spello")))
(defvar personal-buffer nil "Buffer to hold the personal dictionary.")
(defvar spell-last-buffer nil "Last buffer in which spell was invoked.")

(autoload 'detex "detex")
(autoload 'delatex "detex")

(defun only-double-quote-strings ()
  "Deletes everything except for strings enclosed in double quotes ("")"
  (only-strings """"))

(defun only-single-quote-strings ()
  "Deletes everything except for strings enclosed in single quotes (')"
  (only-strings "'"))

(defun only-strings (ch)
  "Deletes everything except for string delimited by CH."
  (let ((beg (point-min)))
    (goto-char (point-min))
    (while (search-forward ch (point-max) t)
      (backward-char 1)
      (delete-region beg (point))
      (forward-sexp 1)
      (insert-string "\n")
      (setq beg (point)))
    (delete-region beg (point-max))))

(defun kill-mail-header ()
  "Kills the mail header up to and including ""--text follows this line--""."
  (goto-char (point-min))
  (if (search-forward mail-header-separator (point-max)  t)
      (delete-region (point-min) (point))))

(defun cadr (l)
  (car (cdr l)))
(defun caddr (l)
  (car (cdr (cdr l))))

(defun size-of (&optional buf)
  "Returns the size of the current buffer, or BUFFER if on optional argument 
is given"
  (save-excursion
    (if buf (set-buffer buf))
    (buffer-size)))

(defun non-empty-buffer (&optional buf)
  "Returns nil iff the current buffer, or BUFFER if an argument is given,
has a size of 0."
  (> (size-of buf) 0))

(defun spell-string (string)		; (almost) the original one
  "Check spelling of string supplied as argument."
  (interactive "sSpell string: ")
  (let* ((buf (get-buffer-create " *temp*"))
	 (spell-command
	  (caddr (assoc 'fundamental-mode spell-options)))
	 (spell-program
	  (substring spell-command 0 (string-match " " spell-command)))
	 (spell-flags (if (string-match " " spell-command)
			  (substring spell-command
				     (string-match " " spell-command))
			"")))
    (save-excursion
      (set-buffer buf)
      (widen)
      (erase-buffer)
      (insert string "\n")
      (call-process-region (point-min) (point-max)
			   spell-program t t nil spell-flags)
      (if (= 0 (buffer-size))
	  (message "%s is correct" string)
	(goto-char (point-min))
	(while (search-forward "\n" nil t)
	  (replace-match " "))
	(message "%sincorrect" (buffer-substring 1 (point-max)))))))

(defun spell-word ()
  "Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling in the same way as
correct-spelling."
  (interactive)
  (let (beg end)
    (save-excursion
      (if (not (looking-at "\\<"))
	  (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (setq end (point)))
    (spell-region beg end nil (buffer-substring beg end) t)))

(defun spell-buffer (wait-code)
  "Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling,
as in correct-spelling.

If spell-wait is nil, the spell program will run in the background in
a separate window, and you will have to issue the command correct-spelling
to correct.  Calling spell-buffer with an argument larger that 1 will 
force spell-wait t, while a negative argument will make spell-wait nil.  
An argument of 0 toggles its value."
  (interactive "p")
  (spell-region (point-min) (point-max) wait-code "buffer"))

(defun spell-region (start end &optional wait-code description wait)
  "Like spell-buffer but applies only to region.
From program, applies from START to END."
  (interactive "r\np")
  (if wait-code (cond ((> wait-code 1) (setq spell-wait t))
		      ((= wait-code 0) (setq spell-wait (not spell-wait)))
		      ((< wait-code 0) (setq spell-wait nil))))
  (let ((mode (if wait			; wait is t <=> call from spell-word
		  'fundamental-mode	; use fundamental mode in that case
		major-mode))
	(oldbuffer (current-buffer))
	(buf (get-buffer-create "*spell-temp*")))
    (message "Checking spelling of %s..." (or description "region"))
    (save-excursion
      (set-buffer buf)
      (widen)
      (erase-buffer)
      (insert-buffer-substring oldbuffer start end)
      (run-spell-on-buffer buf mode description wait))
    (if (and (or wait spell-wait)
	     (non-empty-buffer buf))
	(correct-spelling buf)
      (setq spell-last-buffer (current-buffer)))
    (kill-buffer buf)))

(defun run-spell-on-buffer (buf mode description force-wait)
  "First runs the mode-specific function given in spell-options.
Then runs spell on the buffer by calling either wait-spell-buffer or
nowait-spell-buffer.  The buffer will be destroyed."
  (let (command)
    (set-buffer buf)
    (if (and (assoc mode spell-options)	; Mode specific lisp function
	     (cadr (assoc mode spell-options)))
	(progn
	  (message "Applying %s..." (cadr (assoc mode spell-options)))
	  (funcall (cadr (assoc mode spell-options)))))
    (if (or force-wait spell-wait)
	(wait-spell-buffer buf mode description)
      (nowait-spell-buffer buf mode))))

(defun nowait-spell-buffer (buf mode)
  "Starts an asynchronous spell-process in another window (provided it does
not already exist)."
  (write-region (point-min) (point-max) spell-in-file)
  (or (get-buffer "*spell-shell*")
      (progn
	(shell)
	(rename-buffer "*spell-shell*")
	(message "%s" "Creating new shell...")
	(bury-buffer)
	(sit-for 5)			; Sorry ...
	(message "%s" "")))
  (send-string "*spell-shell*"
	       (concat cd-string " " spell-directory semi-colon))
  (setq command (if (assoc mode spell-options)
		    (caddr (assoc mode spell-options))
		  (caddr (assoc 'fundamental-mode spell-options))))	    
  (send-string "*spell-shell*"
	       (concat command " " spell-in-file
		       redirection-string spell-out-file "\n"))
  (pop-to-buffer "*spell-shell*")
  ;;(enlarge-window (- spell-window-height (window-height)))
  (goto-char (point-max))
  (recenter 0)
  (other-window -1))

(defun wait-spell-buffer (buf mode &optional description)
  "Applies the mode-specific spell command to the buffer."
  (let* ((spell-command (or (caddr (assoc mode spell-options))
			    (caddr (assoc 'fundamental-mode spell-options))))
	 (spell-program
	  (substring spell-command 0 (string-match " "spell-command)))
	 (spell-flags (if (string-match " "spell-command)
			  (substring spell-command
				     (string-match " "spell-command))
			"")))
    (message "Checking spelling of %s..." (or description "region"))
    (set-buffer buf)
    (goto-char (point-max))
    (insert "\n")
    (call-process-region (point-min) (point-max)
			 spell-program t t nil spell-flags)
    (message "Checking spelling of %s...%s"
	     (or description "region")
	     (if (non-empty-buffer buf)
		 ""
	       "correct"))))

(defun spell-help ()
  "Generates a help text for correct-spelling."
  (let ((this-window (selected-window)))
    (switch-to-buffer-other-window (get-buffer-create "*Help*"))
    (erase-buffer)
    (insert
     "For each incorrect word, you are asked for one of the alternatives
(a)ccept, (e)dit, (h)elp, (i)spell, (I)nsert, (r)ec-edit, (s)earch.  
(a)ccept (or SPACE) will accept the word for this session.
(e)dit will let you edit the word in the minibuffer, and then (provided that
   the word is changed), enters a query-replace.
(h)elp (or ""?"") displays this help message.
(i)spell executes the function ispell-word, and if ispell finds close correct
   words, you can enter the character associated with the alternative to
   insert that word instead of the misspelled one.  SPACE to leave unchanged.
(I)nsert will insert the word in your personal dictionary.
(r)ec-edit will embark on a recursive edit (C-M-c to exit).
(s)earch will prompt for a regular expression and search for it in an
asynchronous process.")
    (goto-char (point-min))
    (select-window this-window)))

(defun spell-recursive-edit ()
  "Enters a recursive edit within a save excursion.  Also displays a 
hopefully helpful text."
  (message "Entering recursive edit.  C-M-c to exit.")
  (save-excursion (recursive-edit)))

(defun spell-continue (&optional str)
  "Prompts the user for options on how to continue.  Allows recursive
edits.  Returns nil if any other character than e or SPACE is entered.
Does nothing if spell-cautious is nil and no argument is supplied."
  (if (or spell-cautious str)
      (let (c)
	(if str
	    (message (concat str ".  Press SPACE to continue, - to redo, r to  recursive edit"))
	  (message "Press SPACE to continue, - to redo, r to enter recursive edit"))
	(setq c (read-char))
	(cond ((equal c ?r) (spell-recursive-edit) t)
	      ((equal c ? ))
	      (t nil)))))

(defun not-in-personal-list (word)
  "Returns non-nil if WORD is not found in the personal dictionary."
  (not (save-excursion
	 (set-buffer personal-buffer)
	 (goto-char (point-min))
	 (word-search-forward word (point-max) t))))

(defun edit-it (word &optional replacement)
  "Edits WORD in the minibuffer and, if given a different spelling,
replaces it by a query-replace.  If optional argument REPLACEMENT is given,
uses this instead of editing." 
  (let (newword
	(case-fold-search t)
	(case-replace t))
    (setq newword (or replacement
		      (read-from-minibuffer
		       (concat "Replacement for " word ": ") word)))
    (if (equal word newword)
	()
      (goto-char (point-min))
      (query-replace-regexp (concat "\\b" (regexp-quote word) "\\b") newword))
    (spell-continue)))

(defun spell-search (&optional word)
  "Issues a command in an asynchronous process.  The command is composed
by, in order, spell-search-first-string, a string supplied by the user,
and spell-search-last-string."
  (interactive)
  (let ((this-window (selected-window))
	(str (read-from-minibuffer "Enter string to search for: " word)))
    (or (get-buffer "*spell-search*")
	(progn
	  (shell)
	  (rename-buffer "*spell-search*")
	  (message "%s" "Creating new shell...")
	  (bury-buffer)
	  (sit-for 5)			; Sorry ...
	  (message "%s" "")))
    (switch-to-buffer-other-window "*spell-search*")
    (sit-for 0)
    (erase-buffer)
    (send-string "*spell-search*"
		 (concat spell-search-first-string " '" str "' "
			 spell-search-last-string "\n"))
    (goto-char (point-max))
    (recenter 0)
    (select-window this-window)))

(defun spell-insert (word)
  "Inserts WORD in the personal dictionary."
  (save-excursion
    (set-buffer personal-buffer)
    (goto-char (point-max))
    (insert word "\n")
    (message (concat "Inserted """ word """ in " personal-word-list))
    (sit-for 1)))

(defun spell-dialogue (word)
  "Implements the dialogue with the user described in correct-spelling"
  (let (command)
    (goto-char (point-min))
    (if (not (word-search-forward word (point-max) t))
	(spell-continue (concat "Cannot find " word))
      (message (concat word ": (a)ccept, (e)dit, (h)elp, (i)spell, (I)nsert, (r)ec-edit, (s)earch?"))
      (setq command (read-char))
      (if (not (cond ((equal command ?a)) ; Do nothing
		     ((equal command ? ))
		     ((equal command ?e) (edit-it word))
		     ((equal command ?i)
		      (edit-it word (spell-ispell-word word)))
		     ((equal command ?I) (spell-insert word))
		     ((equal command ?r) (spell-recursive-edit) nil)
		     ;;((equal command ?S) (scroll-other-window nil) nil)
		     ((equal command ?s) (spell-search) nil)
		     ((equal command ?h) (spell-help) nil)
		     ((equal command ??) (spell-help) nil)
		     (t (ding) nil)))
	  (spell-dialogue word)))))
		     
(defun correct-spelling (&optional buf)
  "Corrects the spelling by the file generated by spell. This is done
in an interactive dialogue.
     
For each incorrect word, you are asked for one of the alternatives
(a)ccept, (e)dit, (h)elp, (i)spell, (I)nsert, (r)ec-edit, (s)earch.  
(a)ccept (or SPACE) will accept the word for this session.
(e)dit will let you edit the word in the minibuffer, and then (provided that
   the word is changed), enters a query-replace.
(h)elp (or ""?"") displays this help message.
(i)spell executes the function ispell-word, and if ispell finds close correct
   words, you can enter the character associated with the alternative to
   insert that word instead of the misspelled one.  SPACE to leave unchanged.
(I)nsert will insert the word in your personal dictionary.
(r)ec-edit will embark on a recursive edit (C-M-c to exit).
(s)earch will prompt for a regular expression and search for it in an
   asynchronous process."
  (interactive)
  (let (word
        (old-mode-line-buffer-identification mode-line-buffer-identification)
        (errors-buffer (if buf
        		   buf
			   (if (file-readable-p spell-out-file)
			       (find-file-noselect spell-out-file)
			       (error "Can't read or find output file from spell (%s)" spell-out-file)))))
    (save-excursion
      (setq mode-line-buffer-identification (list "Spell: %17b"))
      (setq personal-buffer (find-file-noselect personal-word-list))
      (if (not buf) (switch-to-buffer spell-last-buffer))
      (unwind-protect
        (while (non-empty-buffer errors-buffer)
          (save-excursion
	    (set-buffer errors-buffer)
	    (goto-char (point-min))
	    (setq word
		  (buffer-substring (point) (progn (end-of-line) (point))))
	    (forward-char 1)
	    (delete-region (point-min) (point)))
	  (if (and (not (equal word "")) (not-in-personal-list word))
            (spell-dialogue word)))
        (setq mode-line-buffer-identification
              old-mode-line-buffer-identification)
        (set-buffer errors-buffer)
        (not-modified)
        (kill-buffer errors-buffer)
        (message "Finished!!")
        (if (not buf) (progn
	  	        (delete-file spell-in-file)
		        (delete-file spell-out-file)))))))

(autoload 'ispell-init-process "ispell")

;;; This function is modified in two respects:
;;; ispell-init-process is moved in order to autoload correctly, and
;;; the function returns nil or the selected alternative, as a string.

(defun spell-ispell-word (&optional quietly)
  "Check spelling of word at or before dot.
If word not found in dictionary, display possible corrections in a window 
and let user select."
  (interactive)
  (let* ((current-syntax (syntax-table))
	 start end word poss replace)
    (ispell-init-process)		; erases ispell output buffer
    (unwind-protect
	(save-excursion
	  ;; Ensure syntax table is reasonable 
	  (set-syntax-table ispell-syntax-table)
	  ;; Move backward for word if not already on one.
	  (if (not (looking-at "\\w"))
	      (re-search-backward "\\w" (point-min) 'stay))
	  ;; Move to start of word
	  (re-search-backward "\\W" (point-min) 'stay)
	  ;; Find start and end of word
	  (or (re-search-forward "\\w+" nil t)
	      (error "No word to check."))
	  (setq start (match-beginning 0)
		end (match-end 0)
		word (buffer-substring start end)))
      (set-syntax-table current-syntax))
    ;;(ispell-init-process)		; erases ispell output buffer
    (or quietly (message "Checking spelling of %s..." (upcase word)))
    (save-excursion
      (set-buffer ispell-out-name)
      (send-string ispell-process (concat word "\n"))
      ;; wait until we have a complete line
      (while (progn
	       (goto-char (point-max))
	       (/= (preceding-char) ?\n))
	(accept-process-output ispell-process))
      (goto-char (point-min))
      (setq poss (ispell-parse-output
		  (buffer-substring (point) 
				    (progn (end-of-line) (point))))))
    (cond ((eq poss t)
	   (or quietly (message "Found %s" (upcase word))))
	  ((stringp poss)
	   (or quietly (message "Found it because of %s" (upcase poss))))
	  ((null poss)
	   (or quietly (message "Could Not Find %s" (upcase word))))
	  (t (setq replace (ispell-choose poss word))))
    replace))
