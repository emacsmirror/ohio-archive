;;;Date: 2 Sep 87 01:36:58 GMT
;;;From: Daniel LaLiberte <B.CS.UIUC.EDU!liberte@EDDIE.MIT.EDU>
;;;Subject: c-prologue.el

;;;Here is a greatly modified version of some utilities posted recently
;;;by someone who now wishes to remain anonymous.  My main extension
;;;was to provide a prologue editor.  Since I dont plan to use this
;;;myself and since it needs some work, anyone else is invited to pick
;;;up the ball and run with it.

;;;dan

;;;-------------------
; C mode utilities
; This code was originally written by an anonymous donor.
; It was modified by Daniel LaLiberte (liberte@a.cs.uiuc.edu) to
; add edit-c-prologue.

; Functions include:
; top-of-defun-and-track : go to top of defun, stack prior points
; end-of-defun-and-track : go to bot of defun, stack prior points
; middle-of-defun        : pop stack, go back to middle of defun

; make-c-prologue   :  make (and maybe populate) a C comment block
; edit-c-prologue   :  edit the prologue made by make-c-prologue
; get-section-text  : prompt for text in window, return text string
	
;; Copyright (C) 1987 Daniel LaLiberte

;; Permission is granted to anybody to do whatever they wish
;; with this software, provided that this notice is maintained.
;; This software is provided AS IS, under the same condition
;; as GNU Emacs itself.

;; This file is, in some ways, part of GNU Emacs.

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

;; This file contains two sets of functions and variables.

;; Part 1 is several function and declarations for C and Lisp mode
;; that allow the user to jump up and down by function definitions,
;; and then go back to where he was.  This is particularly useful
;; for jumping up to the top of a defun, fixing some declarations or
;; comments, then jumping back to the middle or end and continuing 
;; interrupted work there.  It is nice to define the three functions
;; as three mnemonic key sequences defined in the same way.  A trio
;; of key definitions is provided, but commented out.  If you like
;; the definitions there, just uncomment the code and use them, or
;; change them to whatever you like.

;; Part 2 is several functions and declarations for C mode, to help a
;; user put a comment block at the top of a routine.  The code in 
;; part 2 is newer than that in part 1, and is not as polished.  For 
;; more information, see the comment block before the function 
;; make-c-prologue.  You may also edit a prologue with edit-c-prologue.


(defvar middle-of-defun-mark nil
  "list of marks left behind by top-of-defun and bottom-of-defun commands.")

(defun top-of-defun-and-track (cnt)
  "Move backward to next beginning defun, leave spot-mark behind.
With argument, do this that many times.
Return t unless search stops due to end of buffer."
  (interactive "p")
  (cond ((null cnt) (setq cnt 1)) ((eq cnt 0) (setq cnt 1)) (t t))
  (let ((herenow (point-marker)))
    (make-local-variable 'middle-of-defun-mark)
    (if (beginning-of-defun cnt) 
	(setq middle-of-defun-mark (cons herenow middle-of-defun-mark)))
))

(defun end-of-defun-and-track (cnt)
  "Move forward to next end of defun, leave spot-mark behind.
With argument, do this that many times.
Return t unless search stops due to end of buffer."
  (interactive "p")
  (cond ((null cnt) (setq cnt 1)) ((eq cnt 0) (setq cnt 1)) (t t))
  (let ((herenow (point-marker)))
    (make-local-variable 'middle-of-defun-mark)
    (end-of-defun cnt)
    (setq middle-of-defun-mark (cons herenow middle-of-defun-mark))
))

(defun middle-of-defun (arg)
  "Move back to last place where a ..defun-and-track was done.
If no defun and track was ever done, or if an argument is given,
then go to the halfway point of the current defun."
  (interactive "P")
  (make-local-variable 'middle-of-defun-mark)
  (let (top1 bot1)
    (cond ((or (null middle-of-defun-mark) arg)
	   (cond  ((null (beginning-of-defun))
		   (end-of-defun 1)
		   (setq bot1 (point))
		   (beginning-of-defun 1)
		   (setq top1 (point)))
		  (t
		   (setq top1 (point))
		   (end-of-defun 1)
		   (setq bot1 (point)))
		  )
	   (goto-char (/ (+ top1 bot1) 2))
	   (beginning-of-line)
	   )
	  (t
	   (goto-char (car middle-of-defun-mark))
	   (set-marker (car middle-of-defun-mark) nil)
	   (setq middle-of-defun-mark (cdr middle-of-defun-mark))
	   )
	  )
    )
  )

(define-key ctl-x-map "T" 'top-of-defun-and-track)    ;; top =  C-X T
(define-key ctl-x-map "B" 'end-of-defun-and-track)    ;; bottom =  C-X B
(define-key ctl-x-map "M" 'middle-of-defun)           ;; middle = C-X M


;; 
;;-----------------------------------------------------------------------
;; Make a Function comment block (specific to C, could be re-written for
;; other block-structured languages, or maybe Lisp)
;; The comment block structure given here is similar to that proposed
;; in "DACSII and DACS3 Software Standards", R. Luk, 1987.  
;;

(defvar c-prologue-section-alist
  '(
    ("PURPOSE:" full header file )
    ("INTERFACE:" header full)
    ("ALGORITHM:" function full)
    ("FUNCTIONS:" file)
    ("HISTORY:"  function full file)
    )
  "*An alist of section name strings for a C module prologue, in order of
appearance.  For each pair of the list, the first member
is the name of the section, and the second is a list of symbols.
make-c-prologue will ask for text for each field that contains the
specified kind in the corresponding list of symbols.")

(defvar c-prologue-kinds '(file full function header)
  "*List of kinds of prologues used in c-prologue-section-alist")

(defvar c-prologue-default-kind 'function
  "*Default prologue kind used in make-c-prologue")


(defvar c-defun-start-last "^[a-zA-Z_].*("
  "*Regexp that matches the last line of the header of a function declaration")

(defvar c-defun-line-start "^[a-zA-Z_]" 
  "Regexp that matches lines preceding c-defun-start-last but which are
still part of the declaration header")

(defvar c-prologue-separator
  (let ((line "")
	(index 0))
    
    (while (< index fill-column)
      (setq line (concat line "-"))
      (setq index (1+ index)))
    line
    )
  "Line separating c-prologue parts")

(defvar c-prologue-first-line (concat "/*" c-prologue-separator)
  "Unique first line of prologue")

(defvar c-prologue-last-line (concat "\\*" c-prologue-separator "*/")
  "Unique last line of prologue")

(defvar c-prologue-start "NAME:" "*leading word of a C prologue")
(defvar c-prologue-end   "END:"  "*trailer word of a C prologue")

(defvar c-comment-line-prefix "|" 
  "*String which prefixes each line of prologue")

(defvar c-prologue-section-indent "   "
  "*String to indent sections with")
(defvar c-prologue-section-text-indent "    "
  "*String to indent text within sections")
    
(defvar c-prologue-section-prefix (concat c-comment-line-prefix
					  c-prologue-section-indent)
  "Everything up to section title")


(defvar c-prologue-end-marker nil ; "----------------"
  "Marker before line before c-prologue-end")

(defvar c-prologue-include-decl nil
  "*Non-nil if function declaration should be included in the prologue")

(defvar get-header-window-hgt 7
  "*maximum height for a c-prologue-entry Text Entry window")

(defvar c-prologue-first-at-point-min  nil
  "*if non-nil, place prologue for first function at very top of file.
  Otherwise, first function gets its prologue just above declaration like
  all the other functions in the file.")

(defvar c-prologue-fill-column (- fill-column 12)
  "*maximum column for C prologues")

(defun get-prologue-kind ()
  "Get the prologue kind from the minibuffer using completion"
  (let ((kind-string
	 (completing-read (format "Prolog kind (default %s): "
				  c-prologue-default-kind)
			  (vconcat c-prologue-kinds)
			  (function (lambda (arg) 
				      (memq arg c-prologue-kinds)))
			  )))
    (if (string-equal "" kind-string)
	c-prologue-default-kind
      (intern kind-string)))
  )


(defun make-c-prologue (prologue-kind)

  "Make up nice software development-type prologue for the C routine
enclosing or after point.  If ARG is non-nil, then ask for section
text, each in its own buffer (see doc for c-prologue-section-alist).
Leaves point at top of C defun.  Leaves mark at start of prologue."

  (interactive (list (get-prologue-kind)))

  (let (prologue-top
	rev-offset
	this-1st-defun

	section-prefix
	name
	prologue)

    (save-excursion
      ; find place to insert prologue
      (if (eq prologue-kind 'file)
	  (progn
	    (goto-char (point-min))
	    (setq name (file-name-nondirectory (buffer-file-name)))
	    (setq rev-offset (- (point-max) (point)))
	    )

	; not a file prologue
	(end-of-defun 1)
	(beginning-of-defun 1)
	(if (eq (point) (point-min))
	    (error "Cannot find a C function on which to base a Prologue.")
	  (setq rev-offset (- (point-max) (point)))
	  )
	(save-excursion
	  (previous-line 1)
	  (beginning-of-defun 1)
	  (setq this-1st-defun (eq (point) (point-min))))
	
	(setq name "")
	(if (null (re-search-backward c-defun-start-last (point-min) t))
	    (error "Cannot find the declaration for this function.")
	  (save-excursion
	    (if (null (search-forward "(" (point-max) t))
		(setq name "")
	      (backward-char 1)
	      (set-mark (point))
	      (backward-sexp 1)
	      (copy-region-as-kill (point) (mark))
	      (setq name (car kill-ring))
	      (setq kill-ring (cdr kill-ring)))
	    ))
	;      (message "found declaration") (sit-for 1)
	
	(let ( (foo-temp (point)) )
	  (search-forward "(")
	  (backward-char 1)
	  (forward-sexp 1) ; match parameters
	  (set-mark (point))
	  (goto-char foo-temp))
	(beginning-of-line 1)
	;      (message "find line before c defun") (sit-for 1)
	(while (and (looking-at c-defun-line-start) (not (bobp)))
	  (previous-line 1))
	;      (message "at line before c defun") (sit-for 1)
	(next-line 1)
;	(setq defun-decl-rev-offset (- (point-max) (point)))
	(copy-region-as-kill (point) (mark))
	(setq defun-decl (car kill-ring))
	(setq kill-ring  (cdr kill-ring))
	(if (and c-prologue-first-at-point-min this-1st-defun)
	    (goto-char (point-min))
	  (open-line 1) (next-line 1))
	(open-line 2)
	(setq prologue-top (point))
	) ; if prologue-kind
      
      (setq prologue (list (cons "NAME:" name)))

      ;; now put in the various sections
      (let ((section-list-temp c-prologue-section-alist))
	(while section-list-temp
	  (if (memq prologue-kind (cdr (car section-list-temp)))
	      (setq prologue
		    (append prologue (list
				      (cons (car (car section-list-temp)) ""))))
	    )
	  (setq section-list-temp (cdr section-list-temp)))
	)
      (insert-prologue (edit-alist-text prologue))
      )

    (set-mark prologue-top)
    (goto-char (- (point-max) rev-offset))
    ))


(defun insert-prologue (prologue)
  "Insert the prologue text at point"
  
  (let (text)
    (insert c-prologue-first-line)
    
    (while prologue
	  (progn  ; insert section title and text
	    (insert "\n" c-prologue-section-prefix (car (car prologue)))
	    (setq text (cdr (car prologue)))
	    (cond ((or
		    (null text)
		    (eq (length text) 0))
		   (insert "\n" c-comment-line-prefix))
		  
		  (t ; fix up text
		   (insert "\n" text)
		   (narrow-to-region
		    (- (point) (length text))
		    (1- (point)))
		   (goto-char (point-min))
		   (replace-regexp "^"
				   (concat c-prologue-section-prefix
					   c-prologue-section-text-indent)
				   nil)
		   (widen)
		   (end-of-line)
		   (insert "\n" c-comment-line-prefix)
		   )))
      
	  (setq prologue (cdr prologue))
	  )
    )

  (insert "\n" c-prologue-last-line "\n")
  )


(defun text-entry-exit-accept ()
  "exit text entry with ok status"
  (interactive)
  (setq text-entry-exit t)
  (message "text entry accepted.")
  (throw 'exit nil))

(defun text-entry-exit-skip ()
  "exit text entry leaving text the same"
  (interactive)
  (setq text-entry-exit 'continue)
  (message "text entry skipped.")
  (throw 'exit nil))

(defun text-entry-exit-abort ()
  "exit text entry with abort status"
  (interactive)
  (setq text-entry-exit nil)
  (message "text entry abort!")
  (throw 'exit nil))


(defun edit-section-text (name text)
  "Edit some text in a buffer, the name for the text is NAME.
The starting text, if any, is TEXT.
Return the resulting text, or nil if none remains."
  (save-window-excursion
    (let (old-buffer entry-buffer entry-buffer-name
		     entry-text entry-keymap entry-window)
      (setq old-buffer (current-buffer))
      (setq entry-buffer-name (concat "*" name "*"))
      (setq entry-buffer (create-file-buffer entry-buffer-name))
      (pop-to-buffer entry-buffer)
      (if text
	  (insert text))
      (setq entry-window (get-buffer-window entry-buffer))
      (if (> (window-height entry-window) get-header-window-hgt)
	  (shrink-window (- (window-height entry-window)
			    get-header-window-hgt))

	(enlarge-window (- get-header-window-hgt
			   (window-height entry-window))))
      (text-mode)
;      (set-minor-mode 'foo "Entry" t)
      (setq fill-column c-prologue-fill-column)
      (auto-fill-mode fill-column)
      (setq text-entry-exit t)
      (define-key text-mode-map "\^c\^c" 'text-entry-exit-accept)
      (define-key text-mode-map "\^c\^s" 'text-entry-exit-skip)
      (define-key text-mode-map "\^c\^k" 'text-entry-exit-abort)
      (use-local-map text-mode-map)
      (message
       "Edit %s (C-c C-c to accept, C-c C-s to skip, C-c C-k to abort)" name)
      (recursive-edit)

      (if (null text-entry-exit)
	  (error "Editing aborted"))
      (if (eq text-entry-exit 'continue)
	  (setq entry-text text))
      (if (equal (point-min) (point-max))
	  (setq entry-text nil)
	(mark-whole-buffer)
	(exchange-dot-and-mark)
	; make sure there is exactly one newline at the end of text
	(insert "\n")
	(backward-char 1)
	(while (looking-at "\n")
	  (backward-char 1))
	(forward-char 1)
	(copy-region-as-kill (point) (mark))
	(setq entry-text (car kill-ring))
	(setq kill-ring (cdr kill-ring)))

;      (delete-window (selected-window))
      (kill-buffer entry-buffer)
;      (pop-to-buffer old-buffer)
      entry-text)))



(defun edit-c-prologue (prologue-kind)
  "Edit the prologue for the C function enclosing or after point,
or the file prologue if file prologue is specified."

  (interactive (list (get-prologue-kind)))
  (let (start-of-prologue)

    (if (eq 'file prologue-kind)
	(progn
	  (goto-char (point-min))
	  (re-search-forward (concat "^" (regexp-quote c-prologue-first-line))
			     nil t)
	  (setq start-of-prologue (point))
	  (setq prologue
		(edit-alist-text (parse-prologue (point-max))))
	  (kill-region start-of-prologue (point))
	  (insert-prologue prologue)
	  )

      (let (start-of-defun
	    found-prologue)
	(end-of-defun 1)
	(beginning-of-defun 1)
	(setq start-of-defun (point))

	(setq found-prologue
	      (re-search-backward (concat "^" (regexp-quote c-prologue-first-line))
				  (point-min) t))
	(setq start-of-prologue (point))
	(end-of-defun 1)
	(beginning-of-defun 1)

	(if (or (not found-prologue) (/= (point) start-of-defun))
	    (progn
	      (message "No prologue to edit - Let's create one instead")
	      (sit-for 1)
	      (setq prologue (make-c-prologue prologue-kind))
	      )
	  
	  (goto-char start-of-prologue)
	  (setq prologue
		(edit-alist-text (parse-prologue start-of-defun)))
	  (kill-region start-of-prologue (point))
	  (insert-prologue prologue)
	  ))))
    )


(defun parse-prologue (bound)
  "Parse the prologue which begins at point and doesnt extend beyond BOUND
and return an alist of the sections found.
Leave point after the end of the prologue"

  (let (section-prefix
	section-name
	section-text
	line-prefix
	prologue)

    (setq section-prefix (regexp-quote c-prologue-section-prefix))

    (setq line-prefix (regexp-quote c-comment-line-prefix))

    (beginning-of-line)
    ; find the next section
    (while (re-search-forward (concat "^" section-prefix) bound t)
    
      (setq section-name (buffer-substring
			  (point)
			  (progn (end-of-line) (point))))
      ; read the text
      (setq section-text "")
      (next-line 1)
      (beginning-of-line)
      (while (not (or (> (point) bound)
		      (looking-at (concat "^" line-prefix "$"))))
	(forward-char (length
		       (concat section-prefix c-prologue-section-text-indent)))
	(setq section-text
	      (concat section-text
		      (buffer-substring
		       (point)
		       (progn (end-of-line) (1+ (point))))))
	(next-line 1)
	(beginning-of-line)
	)
      
      (setq prologue (append prologue (list (cons section-name section-text))))
      )

    ; find the end of the prologue
    (if (re-search-forward
	 (concat "^" (regexp-quote c-prologue-last-line)) bound t)
	(next-line 1)
      (error "Cannot find end of prologue"))
    (beginning-of-line)
    prologue
    )
  )
  

(defun edit-alist-text (text-alist)
  "edit the TEXT-ALIST.  Each pair of the list is a label string and
  the text to edit."
  (let ((temp-alist text-alist)
	(new-alist nil))

    (while temp-alist
      (let* ((section (car temp-alist))
	     (new-text (edit-section-text (car section) (cdr section))))
	(setq new-alist (append new-alist
				(list (cons (car section) new-text))))
	)
      (setq temp-alist (cdr temp-alist))
      )
    new-alist)
  )
