;From ark1!nap1!ames!sun-barr!lll-winken!uunet!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs Mon Nov  6 13:04:18 1989
;Article 356 of gnu.emacs.bug
;Path: ark1!nap1!ames!sun-barr!lll-winken!uunet!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs
;>From lrs@indetech.com (Lynn Slater)
;Newsgroups: gnu.emacs.bug
;Subject: C and C++ function headers. Table of contents. Fcn synopsys extraction
;Message-ID: <m0gNXRu-0000F7C@fire.indetech.com>
;Date: 4 Nov 89 20:57:00 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 710
;
;> Lastly, it would be nice to have a way to comment function headers
;> automatically . Do you have something like this?
;
;Yep, but it is not up to general usage quality.  Emacs hackers are welcome.
;Please mail as well as post (if applicable) any replies as our gnu news feed
;may be down.
;
;===============================================================
;Lynn Slater -- lrs@indetech.com or {sun, ames, pacbell}!indetech!lrs
;42075 Lawrence Place, Fremont Ca 94538
;Office (415) 438-2048; Home (415) 796-4149; Fax (415) 438-2034
;===============================================================
;
;-*- File: ~/local/c-support.el
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-support.el -- Partial support for team C or C++ development
;; Copyright (c) 1989, Lynn R. Slater Jr.
;; Author          : Lynn Slater
;; Created On      : Tue Aug  8 12:26:17 1989
;; Last Modified By: Lynn Slater
;; Last Modified On: Sat Nov  4 12:50:16 1989
;; Update Count    : 46
;; Status          : Alpha released
;; 
;; HISTORY
;; PURPOSE
;;    This file provides:
;; 1) Function header templates for C++ or C. C++ is better supported. 
;; 2) Fcn synopsys maintenance.
;; 3) Table of contents maintenacne (lisp amd make file also).
;; 4) Help-for help like support available.
;; 5) C++ to C comment conversion
;; 6) Command line batch maintanance operations
;; 7) Main and module insertation of ident strings.
;; 8) Highlight, selection, and replacement of "templates" much like some
;;    of the dumb sun editors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warning: this file is not yet ready for insertion into just any emacs.
;; Only someone with some experience and patience should try it as I have
;; not yet seperated out local site quirks or otherwise sanitized this file.
;; It is also not as customizable as I would like. That is why this is an
;; "alpha" release and not a "beta". Improvements are welcome.

;; Save as 'c-support and byte-compile.
(provide 'c-support)
(require 'header)    ;; Package from lrs@indetech.com

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function headers.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun header-prefix-sstring ()
  "Returns the header prefix string stripped of trailing blanks"
  (let* ((hps (header-prefix-string))
	 (match (string-match "[ \t]+$" hps)))
    (if match (substring hps 0  match) hps)))

(defun document-c-function (note-copyright)
  "Insert a comment block containing the module title, author, etc.  To
   use, place the cursor on or before the line starting the function definition.

   Functions with these headers can be automatically placed into the table
   of contents by the update-table-of-contents command.

   The synopsis can be automatically constructed by the
   update-function-synopsis (\\[update-function-synopsis]) command.

   If given a prefix arg, make a copyright notice instead of an Author: entry.

   Note: this currently works much better with c++ than it does with C."
  (interactive "P")
  (let* ((start-col (current-column))
	 (fname (next-c-function-name))
	 header-prefix-string
	 new-point)
    (beginning-of-line)
    (setq header-prefix-string
	  (concat (buffer-substring (point)
				    (progn (indent-to start-col) (point)))
		  (header-prefix-sstring)))
    (make-divisor)
    (insert "\n")
    (indent-to start-col)
    (insert comment-start fname " -- \n")
    (setq new-point (1- (point)))
    (insert header-prefix-string
	    (if note-copyright
		(concat "Copyright "
			(substring (current-time-string)
				   -4)
			" ")
	      " AUTHOR:      ")
	    (user-full-name)
	    " <" (user-login-name) "@" (system-name)
	    ">\n")
    (insert
	    header-prefix-string " SYNOPSIS\n"
	    header-prefix-string " DESCRIPTION\n"
	    header-prefix-string "   |>Description of function<|\n"
	    header-prefix-string " NOTES\n"	
	    header-prefix-string "   |><|\n"
	    header-prefix-string " CAVEATS AND BUGS\n"
	    header-prefix-string "   |> describe any peculiarities <|\n"
	    )
    (if (and comment-end (not (string-equal comment-end "")))
	(progn
	  (indent-to start-col)
	  (insert comment-end "\n")))
    (forward-line -1)
    (update-function-synopsis)
    (goto-char new-point)
    ))

(defun next-c-function-name ()
  "Returns the name of the next c function.
  Should only be called from before a function as it cannot reliable  tell
  function calls from function definitions and certain c constructs such as
  for and while loops." 
  (save-excursion
    (if (re-search-forward "\\(\\sw\\|\\s_\\|::\\)+[ \t]*(" nil t)
	(buffer-substring (match-beginning 1) (match-end 1))
      "")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function synopsys extraction
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-synopsis ()
  "Deletes current synopsis, places cursor at start of next line"
  (interactive)
  (let* ((header-prefix-pat (concat "^[ \t]*" (regexp-quote (header-prefix-string))))
	 (synopsis-pat (concat header-prefix-pat "SYNOPSIS")))
    (beginning-of-line)
    ;; Move back to synopsis line
    (while (and (not (looking-at synopsis-pat)) (looking-at header-prefix-pat) (forward-line -1)))
    (if (not (looking-at synopsis-pat))
	(progn
	  (forward-line 1)
	  ;; Move forward to synopsis line
	  (while (and (not (looking-at synopsis-pat)) (looking-at header-prefix-pat) (forward-line 1)))))
    (beginning-of-line)
    (if (looking-at synopsis-pat)
	(progn
	  ;; we have the start of a synopsis
	  (forward-line 1)
	  (delete-region (point)
			 (progn
			   (while (and (looking-at (concat header-prefix-pat
							   "[\n \t]"))
				       (forward-line 1)))
			   (point)
			   ))
	  t
	  ))))

(defun update-function-synopsis ()
  "Updates the synopsis in a function header.  Grabs the function
  declaration (up to the first bracket in c++ or the first blank in c),
  converts comments, and inserts it under the synopsis line.

  Removes the old synopsis."
  (interactive)
  (if (delete-synopsis)
      (let* ((header-prefix-sstring (header-prefix-sstring))
	     (prefix (buffer-substring
		      (point)
		      (progn (back-to-indentation)
			     (if (char-equal
				  (aref header-prefix-sstring 0) 32)
				 (backward-char 1))
			     (point))))
	     (here (progn (beginning-of-line) (point)))
	     (header-prefix-pat (concat "^[ \t]*"
					(regexp-quote header-prefix-sstring)))
	     fcn-decl
	     start)
	(beginning-of-line)
	;; skip to start of defn
	(while (and (looking-at header-prefix-pat) (forward-line 1)))
	(setq start (point))
	(if (re-search-forward "\\(\\sw\\|\\s_\\|::\\|<<\\)+[ \t]*(" nil t)
	    (progn
	      (goto-char (1- (match-end 0)))
	      (forward-sexp)   ;; Skip args. In C++, we are done
	      (if (eq major-mode 'c-mode)
		  (progn
		    ;; In C, we need to find the declarations.
		    ;; Heiristic: Skip to blank or a '{'
		    (re-search-forward "\\(^[ \t]*$\\|\\{\\)")
		    (goto-char (1- (match-beginning 0)))))
	      (setq fcn-decl (buffer-substring start (point)))
	      (goto-char here)
	      (insert fcn-decl "\n")
	      (insert-box here (point)
			  (concat prefix header-prefix-sstring "   "))
	      )
	  (error "No function defn here!")))))

(defun update-all-synopsis ()
  "Updates the synopsys of all function headers."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (concat (regexp-quote (header-prefix-string)) "SYNOPSIS") nil t)
      (update-function-synopsis)
      (sit-for 0)
      )
    (message "All Function synopsis updated.")
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful things for all emacs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-box (start end text)
  "Insert a text prefix at a column in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START.
   The rough inverse of this function is kill-rectangle."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (setq cc  (current-column))
      (while (< (point) end) ;; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(move-to-column-force cc)) ;; Alternate: use move-to-column if you must.
      (move-marker end nil))))

(defun insert-end (start end text)
  "Insert a text prefix at the end in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (end-of-line)	
      (while (< (point) end);; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(end-of-line)	
	)
      (move-marker end nil))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of contents maintenance.  Works for make mode and lisp files as
;; well as C and C++.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update-table-of-contents ()
  "Updates the table of contents in the file header.
   Removes the old table of contents."
  (interactive)
  (if (header-goto-table-of-contents)
      (save-excursion
	(let ((insert-point (point))
	      (header-prefix-string (header-prefix-string))
	      str)
	  (while (setq str (get-next-function-description))
	    (save-excursion
	      (goto-char insert-point)
	      (insert "\n" header-prefix-string "  " str)
	      (setq insert-point (point))))
	  ;; Now, get rid of the old table of contents
	  (goto-char insert-point)
	  (insert "\n" header-prefix-string "  ")
	  (setq str (regexp-quote (buffer-substring (1+ insert-point) (point))))
	  (beginning-of-line)
	  (while (looking-at str)
	    (kill-line 1))
	  (message "Table of contents updated.")
	  ))
    (error "There is no table of contents!")))

(defun get-next-function-description ()
  "Returns the line defining or describing the next function.
   What a 'function' is depends upon the mode."
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    ;; All defuns, defvars, defmacros go to table of contents
    (if (re-search-forward "^(def.*$" nil t)
	(buffer-substring (match-beginning 0) (match-end 0))))
   ((eq major-mode 'make-mode)
    ;; In make mode, a "function" is a target. However, the only ones we
    ;; notice are those with not-null actions.
    (if (re-search-forward "^\\([^# \t\=\n]* *:\\).*\n\t" nil t)
	(buffer-substring (match-beginning 1) (match-end 1))))
   (t;; Presume C or C++. Only notice those fcns marked with a standard
    ;; fcn header. 
    (let ((qhps (regexp-quote (header-prefix-string)))
	  (chps (regexp-quote comment-start)))
      (if (re-search-forward (concat chps "\\(.*\\)[ \t]*\n.*" qhps "AUTHOR") nil t)
	  (buffer-substring (match-beginning 1 ) (match-end 1)))))))

(defun header-goto-table-of-contents ()
  "Moves to the table of contents in the header"
  (interactive)
  (let ((qhps (regexp-quote (header-prefix-string)))
	(here (point)))
    (beginning-of-buffer)
    (if (re-search-forward (concat qhps "TABLE OF CONTENTS") nil t)
	(point)
      (goto-char here)
      (beep)
      (message "There is no table of contents!")
      nil)))

(defun header-goto-purpose ()
  "Moves to the purpose in the header"
  (interactive)
  (let ((qhps (regexp-quote (header-prefix-string)))
	(here (point)))
    (beginning-of-buffer)
    (if (re-search-forward (concat qhps "PURPOSE") nil t)
	(progn
	  (skip-chars-forward "/ \t\n"
			      (save-excursion (forward-line 2) (1- (point))))
	  (point))
      (goto-char here)
      (beep)
      (message "There is no purpose in the header! :->")
      nil)))

(defun header-goto-end ()
  "Moves to the end of the header box"
  (interactive)
  (beginning-of-buffer)
  (forward-line 1)
  (let ((hps (regexp-quote (header-prefix-sstring))))
    (while (looking-at hps)
      (forward-line 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy to use help.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun help-for-c-templates ()
  "You have discovered 'C-ct', the C and C++ templates facility.  
   All templates start with this sequence. From there, type a templates option:

f, C-f  Document a C function
s, C-s  Update the synopsis of a C function
   C-h  Make a file header
r, C-r  Document a revision to the file
c       Goto table of contents
   C-c  Update the table of contents 
t, C-t  Move among and edit the |> <| constructs.
b, C-b  Make a box comment
d, C-d  Make a visual dividing line
e, C-e  Goto end of header
p, C-p  Goto purpose in header
m       Insert AFS c or c++ module header code line
C-m     Insert AFS c or c++ main   header code line
        (There are no special header code liens for include files.)

New Feature: \\[remake-header] -- Insert a new header, copy details from
             old header.  May still need hand cleanup afterwards.

Please use \\[describe-key] to find out more about any of these keys."
  (interactive)
  (let ((line-prompt
	 (substitute-command-keys (concat "f s t b d e p m C-m C-h C-r c C-c. Type ? for more help: "))))
    (message line-prompt)
    (let ((char (read-char)))
      (if (or (= char ??) (= char help-ch))
	  (save-window-excursion
	    (switch-to-buffer-other-window "*Help*")
	    (erase-buffer)
	    (insert (documentation 'help-for-c-templates))
	    (goto-char (point-min))
	    (while (memq char (cons help-ch '(?? ?\C-v ?\ ?\177 ?\M-v)))
	      (if (memq char '(?\C-v ?\ ))
		  (scroll-up))
	      (if (memq char '(?\177 ?\M-v))
		  (scroll-down))
	      (message "%s%s: "
		       line-prompt
		       (if (pos-visible-in-window-p (point-max))
			   "" " or Space to scroll"))
	      (let ((cursor-in-echo-area t))
		(setq char (read-char))))))
      (let ((defn (cdr (assq (downcase char) c-template-map))))
	(if defn (call-interactively defn) (ding))))))

(defvar c-template-map nil
  "Keymap used in c or c++ mode for smart template operations.")

(let ((c-mp (make-sparse-keymap)))
  (define-key c-mp "?"    'help-for-c-templates)
  (define-key c-mp "\C-h" 'help-for-c-templates)
  (define-key c-mp help-character 'help-for-c-templates)
  (define-key c-mp "\C-f" 'document-c-function)
  (define-key c-mp "f"    'document-c-function)
  (define-key c-mp "s"    'update-function-synopsis)
  (define-key c-mp "\C-s" 'update-function-synopsis)
  (define-key c-mp "\C-h" 'make-header)
  (define-key c-mp "\C-r" 'make-revision)
  (define-key c-mp "r"    'make-revision)
  (define-key c-mp "\C-t" 'enter-template-mode)
  (define-key c-mp "t"    'enter-template-mode)
  (define-key c-mp "b"    'make-box-comment)
  (define-key c-mp "\C-b" 'make-box-comment)
  (define-key c-mp "d"    'make-divisor)
  (define-key c-mp "\C-d" 'make-divisor)
  (define-key c-mp "\C-c" 'update-table-of-contents)
  (define-key c-mp "c"    'header-goto-table-of-contents)
  (define-key c-mp "e"    'header-goto-end)
  (define-key c-mp "\C-e" 'header-goto-end)
  (define-key c-mp "p"    'header-goto-purpose)
  (define-key c-mp "\C-p" 'header-goto-purpose)
  (define-key c-mp "\C-m" 'afs-main)
  (define-key c-mp "m"    'afs-module)
  (setq c-template-map c-mp))

;;(require 'c-mode)
(require 'c++-mode)
(progn
  (define-key c-mode-map "\C-ct" c-template-map)
  (define-key c-mode-map "\C-c\C-t" c-template-map)
  (define-key c++-mode-map "\C-ct" c-template-map)
  (define-key c++-mode-map "\C-c\C-t" c-template-map)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I do not like the current behavior of / or { in c++ mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun electric-c++-open-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if c-auto-newline
		     (progn ;;(c++-indent-line) (newline)
		       t)
		   nil)))
	(progn
	  (insert last-command-char)
	  (c++-indent-line)
	  (if c-auto-newline
	      (progn
		(setq insertpos (1- (point)))
		(newline)
		(c++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))


(defun electric-c++-slash (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (and (char-equal (preceding-char) ?/)
	   (save-excursion (beginning-of-line)
			   (not (looking-at "[ \t]*/"))))
      (progn
      (forward-char -1)
      (indent-to 33)
      (forward-char 1)))
    (self-insert-command (prefix-numeric-value arg)))

(progn
  (define-key c++-mode-map "{" 'electric-c++-open-brace)
  (define-key c++-mode-map "/" 'electric-c++-slash)
  )
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format conversions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun convert-to-c-comments ()
  "Conterts // stype comments to /* style */. Good for crippled
   preprocessors such as supplied with most non-gnu C++ systems. (YUCH!)."
  (interactive)
  (replace-regexp "//\\(.*\\)$" "/* \\1 */" nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line header maintenance for bi diehards or batch operations.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'do-update-table-of-contents command-line-hooks))
(defun do-update-table-of-contents ()
  (if (string= (upcase argi) "-TOC") 
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))
     
	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(update-table-of-contents)
		(write-file nil)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))

(setq command-line-hooks (cons 'do-update-all-synopsis command-line-hooks))
(defun do-update-all-synopsis ()
  (if (string= (upcase argi) "-SYNOPSIS") 
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))
     
	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(update-all-synopsis)
		(write-file nil)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))


;;(setq command-line-hooks (cons 'list-args command-line-hooks))
;;(defun list-args ()
;;  (if (string= (upcase argi) "-LIST")
;;      (message "%s" command-line-args-left)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QA and corperate legal want stings such as the following in all
;; non-copylefted code. (Apologies to rms, we engineers keep as much as we
;; can under copyleft.)
;;
;; Note: this code is NOT copyright to ITI. This would only happen if you
;; distributed fiels edited with these functions and without first
;; customizing the strings to something more suitable (such as a copyleft).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar afs-main-header-string "
static char iDeNt[] = \"@(#)$__Header$, Copyright 1989 ITI\";
static char *cOpYrIgHt[] =
        {
        \"Confidential and Proprietary to Independence\",
        \"Technologies, Unpublished and Copyrighted Work\"
        };
")

(defvar afs-module-header-string "
static char iDeNt[] = \"@(#)$__Header$, Copyright 1989 ITI\";
")

(defun afs-main ()
  "Inserts the AFS header for a main c or c++ program"
  (interactive)
  (header-goto-end)
  (insert afs-main-header-string))

(defun afs-module ()
  "Inserts the AFS header for a module (a .o file) in a  c or c++ program"
  (interactive)
  (header-goto-end)
  (insert afs-module-header-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight of templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight)

(defvar current-highlighted-template nil)

(defun highlight-template (start stop)
  ;;(sit-for 0)
  (unhighlight-template)
  ;;(sit-for 0)
  (setq current-highlighted-template (cons start stop))
    ;;(message "%s" current-highlighted-template)
  (highlight-region start stop))

(defun unhighlight-template ()
  ;;(message "%s" current-highlighted-template)
  (if (and current-highlighted-template
	   (pos-visible-in-window-p (car current-highlighted-template))
	   (pos-visible-in-window-p (cdr current-highlighted-template)))
      (unhighlight-region (car current-highlighted-template)
			  (cdr current-highlighted-template)))
  (setq current-highlighted-template nil))
  
(defun next-template ()
  (interactive)
  (if current-highlighted-template
      (goto-char (cdr current-highlighted-template)))
  (if (re-search-forward "|>.*<|" nil t)
      (progn
	(highlight-template (match-beginning 0) (match-end 0))
	)
    (beep)
    (message "No more templates")))

(defun previous-template ()
  (interactive)
  (if current-highlighted-template
      (goto-char (car current-highlighted-template)))
  (if (re-search-backward "|>.*<|" nil t)
      (progn
	(highlight-template (match-beginning 0) (match-end 0))
	)
    (beep)
        (message "No more templates")))

;; Need recursive edit for this?  arrows move, c-g quits (and unhightlights),
;; space zaps
;; Others exit and forget that they were in use?

(defun stop-template-mode ()
  (interactive)
  (beep)
  (unhighlight-template)
  (throw 'exit nil))

(defun exit-template-mode ()
  (interactive)
  (if current-highlighted-template
      (delete-region (car current-highlighted-template)
		     (cdr current-highlighted-template)))
  (setq current-highlighted-template nil)
  (throw 'exit nil))

(defvar template-mode-map (make-sparse-keymap)
  "Keymap used in template mode.")

;;(setq template-mode-map (make-sparse-keymap))

(progn
  (define-key template-mode-map "n" 'next-template)
  (define-key template-mode-map "\C-n" 'next-template)
  (define-key template-mode-map "\C-f" 'next-template)
  (define-key template-mode-map "f, \C-f" 'next-template)
  (define-key template-mode-map "\C-p" 'previous-template)
  (define-key template-mode-map "p" 'previous-template)
  (define-key template-mode-map "\C-b" 'previous-template)
  (define-key template-mode-map "b" 'previous-template)
  (define-key template-mode-map "\C-g" 'stop-template-mode)
  (define-key template-mode-map " "   'exit-template-mode)
  (define-key template-mode-map "\el"   'eval-expression)
  (define-key template-mode-map "\ex"   'execute-extended-command)
  (define-key template-mode-map help-character 'help-for-template-mode)
  (define-key template-mode-map "?" 'help-for-template-mode)
  (define-key template-mode-map "\C-h" 'help-for-template-mode)
  )

(defun help-for-template-mode ()
  "You are in the template recursive mode. All you can do is move around
   the templates or quit. You may type

n, C-n  To move to the next template
p, C-p  To move to the previous template
SPC     To kill the current template, exit template mode, and start editing.
C-g     To leave template mode"
  (interactive)
  (let ((line-prompt
	 (substitute-command-keys (concat "n p SPC C-g. Type ? for more help: "))))
    (message line-prompt)
    (let ((char (read-char)))
      (if (or (= char ??) (= char help-ch))
	  (save-window-excursion
	    (switch-to-buffer-other-window "*Help*")
	    (erase-buffer)
	    (insert (documentation 'help-for-template-mode))
	    (goto-char (point-min))
	    (while (memq char (cons help-ch '(?? ?\C-v ?\ ?\177 ?\M-v)))
	      (if (memq char '(?\C-v ?\ ))
		  (scroll-up))
	      (if (memq char '(?\177 ?\M-v))
		  (scroll-down))
	      (message "%s%s: "
		       line-prompt
		       (if (pos-visible-in-window-p (point-max))
			   "" " or Space to scroll"))
	      (let ((cursor-in-echo-area t))
		(setq char (read-char))))))
      (let ((defn (cdr (assq (downcase char)template-mode-map))))
	(if defn (call-interactively defn) (ding))))))

(defun enter-template-mode ()
  "Enter a special mode where you can move around the incomplete templates.
   Once inside, type the normal help sequences to see what you can do."
  (interactive)
  (next-template)
  (let ((previous-lkeymap (current-local-map))
	(previous-gkeymap (current-global-map)))
    (use-local-map template-mode-map)
    (use-global-map (make-keymap))
    (recursive-edit)
    (use-local-map  previous-lkeymap)
    (use-global-map previous-gkeymap)
    ))

;;(define-key c-mode-map "\C-ct" 'enter-template-mode)
;;(define-key c++-mode-map "\C-ct" 'enter-template-mode)

;; need help for arrows

-*- End File: ~/local/c-support.el


