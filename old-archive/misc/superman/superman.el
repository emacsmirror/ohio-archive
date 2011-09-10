;; -*- Mode: Emacs-Lisp -*-
;; File:		superman.el
;; Description: 	Background manual page formatter & mode
;; Author:		Barry A. Warsaw <bwarsaw@cen.com>
;; Last Modified:	31-Jul-1991
;; Version:		1.0
;;
;; LCD Archive Entry:
;; superman|Barry A. Warsaw|bwarsaw@cen.com
;; |Background Un*x manual page formatter and mode
;; |17-Jul-1991|1.0|

;; ========== Standard Disclaimer ==========
;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.  Refer to the GNU Emacs General Public License
;; for full details. You should consider this code to be covered under
;; the terms of the GPL.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;; ========== Credits and History ========== 
;; In mid 1991, several people posted some interesting improvements to
;; man.el from the standard emacs 18.57 distribution.  I liked many of
;; these, but wanted everthing in one single package, so I decided
;; to encorporate them into a single manual browsing mode.  While
;; much of the code here has been rewritten, and some features added,
;; these folks deserve lots of credit for providing the initial
;; excellent packages on which this one is based.

;; Nick Duffek <duffek@chaos.cs.brandeis.edu>, posted a very nice
;; improvement which retrieved and cleaned the manpages in a
;; background process, and which correctly deciphered such options as
;; man -k.

;; Eric Rose <erose@jessica.stanford.edu>, submitted manual.el which
;; provided a very nice manual browsing mode.

;; ========== Features ==========
;; + Runs "man" in the background and pipes the results through a
;;   series of sed and awk scripts so that all retrieving and cleaning
;;   is done in the background. The cleaning commands are configurable.
;; + Syntax is the same as Un*x man
;; + Functionality is the same as Un*x man, including "man -k" and
;;   "man <section>, etc.
;; + Provides a manual browsing mode with keybindings for traversing
;;   the sections of a manpage, following references in the SEE ALSO
;;   section, and more.
;; + Multiple manpages created with the same man command are put into
;;   a narrowed buffer circular list.


(require 'sc-alist)
(provide 'superman)

;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; user variables

(defvar sm-notify 'friendly
  "*Selects the behavior when manpage is ready.
This variable may have one of the following values:

'bully      -- make the manpage the current buffer and only window
'aggressive -- make the manpage the current buffer in the other window
'friendly   -- display manpage in other window but don't make current
'polite     -- don't display manpage, but prints message when ready (beeps)
'quiet      -- like 'polite, but don't beep
'meek       -- make no indication that manpage is ready

Any other value of sm-notify is equivalent to 'meek.")

(defvar sm-reuse-okay-p t
  "*Reuse a manpage buffer if possible.
When t, and a manpage buffer already exists with the same invocation,
superman just indicates the manpage is ready according to the value of
sm-notify. When nil, it always fires off a background process, putting
the results in a uniquely named buffer.")

(defvar sm-overload-p t
  "*Overload standard manual-entry command.
When t, the function manual-entry is fset to superman's manual page
function. Otherwise you have to invoke superman via sm-manual-entry.")

(defvar sm-downcase-section-letters-p t
  "*Letters in sections are converted to lower case.
Some Un*x man commands can't handle uppercase letters in sections, for
example \"man 2V chmod\", but they are often displayed in the manpage
with the upper case letter. When this variable is t, the section
letter (e.g., \"2V\") is converted to lowercase (e.g., \"2v\") before
being sent to the man background process.")

(defvar sm-circular-pages-p t
  "*If t, the manpage list is treated as circular for traversal.")

(defvar sm-auto-section-alist
  '((c-mode . ("2" "3"))
    (c++-mode . ("2" "3"))
    (shell-mode . ("1" "8"))
    (cmushell-mode . ("1" "8"))
    (text-mode . "1")
    )
  "*Association list of major modes and their default section numbers.
List is of the form: (MAJOR-MODE . [SECTION | (SECTION*)]). If current
major mode is not in list, then the default is to check for manpages
in all sections.")

(defvar sm-section-translations-alist
  '(("3C++" . "3")
    ("1-UCB" . ""))
  "*Association list of bogus sections to real section numbers.
Some manpages (e.g. the Sun C++ 2.1 manpages) have section numbers in
their references which Un*x man(1) does not recognize.  This
assocation list is used to translate those sections, when found, to
the associated section number.")

(defvar sm-filter-list
  '(("sed "
     ("-e 's/.\010//g'"
      "-e '/[Nn]o such file or directory/d'"
      "-e '/Reformatting page.  Wait... done/d'"
      "-e '/^\\([A-Z][A-Z.]*([0-9A-Za-z][-0-9A-Za-z+]*)\\).*\\1$/d'"
      "-e '/^[ \\t]*Hewlett-Packard Company[ \\t]*- [0-9]* -.*$/d'"
      "-e '/^[ \\t]*Hewlett-Packard[ \\t]*- [0-9]* -.*$/d'"
      "-e '/^ *Page [0-9]*.*(printed [0-9\\/]*)$/d'"
      "-e '/^Printed [0-9].*[0-9]$/d'"
      "-e '/^Sun Microsystems.*Last change:/d'"
      "-e '/^Sun Release [0-9].*[0-9]$/d'"
      "-e '/^\\n$/D'"
      ))
    ("awk '"
     ("BEGIN { blankline=0; anonblank=0; }"
      "/^$/ { if (anonblank==0) next; }"
      "{ anonblank=1; }"
      "/^$/ { blankline++; next; }"
      "{ if (blankline>0) { print \"\"; blankline=0; } print $0; }"
      "'"
      ))
     )
  "*Manpage cleaning filter command phrases.
This variable contains an association list of the following form:

'((command-string (phrase-string*))*)

Each phrase-string is concatenated onto the command-string to form a
command filter. The (standard) output (and standard error) of the Un*x
man command is piped through each command filter in the order the
commands appear in the association list. The final output is placed in
the manpage buffer.")

(defvar sm-mode-line-format
  '("" mode-line-modified
       mode-line-buffer-identification "   "
       global-mode-string
       sm-page-mode-string
       "    %[(" mode-name minor-mode-alist mode-line-process ")%]----"
       (-3 . "%p") "-%-")
  "*Mode line format for manual mode buffer.")

(defvar sm-mode-map nil
  "*Keymap for sm-manual-mode.")

(defvar sm-mode-hooks nil
  "*Hooks for sm-manual-mode.")

(defvar sm-section-regexp "[0-9][a-zA-Z+]*"
  "*Regular expression describing a manpage section within parentheses.")

(defvar sm-heading-regexp "^[A-Z]"
  "*Regular expression describing a manpage heading entry.")

(defvar sm-see-also-regexp "SEE ALSO"
  "*Regular expression for SEE ALSO heading (or your equivalent).
This regexp should not start with a `^' character.")

(defvar sm-first-heading-regexp "^NAME$\\|^No manual entry for .*$"
  "*Regular expression describing first heading on a manpage.
This regular expression should start with a `^' character.")

(defvar sm-reference-regexp "[-a-zA-Z0-9_.]+\\(([0-9][a-zA-Z+]*)\\)?"
  "*Regular expression describing a reference in the SEE ALSO section.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end user variables

(defconst sm-version-number "1.0"
  "Superman's version number.")


;; other variables and keymap initializations
(make-variable-buffer-local 'sm-sections-alist)
(make-variable-buffer-local 'sm-refpages-alist)
(make-variable-buffer-local 'sm-page-list)
(make-variable-buffer-local 'sm-current-page)
(make-variable-buffer-local 'sm-page-mode-string)

(setq-default sm-sections-alist nil)
(setq-default sm-refpages-alist nil)
(setq-default sm-page-list nil)
(setq-default sm-current-page 0)
(setq-default sm-page-mode-string "1 (of 1)")

(if sm-mode-map
    nil
  (setq sm-mode-map (make-keymap))
  (suppress-keymap sm-mode-map)
  (define-key sm-mode-map " "    'scroll-up)
  (define-key sm-mode-map "\177" 'scroll-down)
  (define-key sm-mode-map "n"    'sm-next-section)
  (define-key sm-mode-map "p"    'sm-previous-section)
  (define-key sm-mode-map "\en"  'sm-next-manpage)
  (define-key sm-mode-map "\ep"  'sm-previous-manpage)
  (define-key sm-mode-map ","    'beginning-of-buffer)
  (define-key sm-mode-map "."    'end-of-buffer)
  (define-key sm-mode-map "r"    'sm-follow-manual-reference)
  (define-key sm-mode-map "t"    'toggle-truncate-lines)
  (define-key sm-mode-map "g"    'sm-goto-section)
  (define-key sm-mode-map "s"    'sm-goto-see-also-section)
  (define-key sm-mode-map "q"    'sm-quit)
  (define-key sm-mode-map "m"    'sm-manual-entry)
  (define-key sm-mode-map "v"    'sm-version)
  (define-key sm-mode-map "?"    'describe-mode)
  )


;; ======================================================================
;; utilities

(defun sm-page-mode-string ()
  "Formats part of the mode line for manual mode."
  (format "%d (of %d)" sm-current-page (length sm-page-list)))

(defun sm-delete-trailing-newline (str)
  (if (string= (substring str (1- (length str))) "\n")
      (substring str 0 (1- (length str)))
    str))

(defun sm-build-man-command ()
  "Builds the entire background manpage and cleaning command."
  (let ((command "man %s 2>&1 | ")
	(flist sm-filter-list))
    (while flist
      (let ((pcom (car (car flist)))
	    (pargs (car (cdr (car flist)))))
	(setq flist (cdr flist))
	(if (or (not (stringp pcom))
		(not (listp pargs)))
	    (error "malformed sm-filter-list."))
	(setq command (concat command pcom
			      (mapconcat '(lambda (phrase) phrase)
					 pargs " "))))
      (if flist
	  (setq command (concat command " | " ))))
    command))

(defun sm-downcase (man-args)
  "Downcases section letters in MAN-ARGS."
  (let ((newargs "")
	(s 0)
	mstart mend
	(len (length man-args)))
    (while (and (< s len)
		(setq mstart (string-match sm-section-regexp man-args s)))
      (setq mend (match-end 0)
	    newargs (concat newargs (substring man-args s mstart)))
      (setq newargs (concat newargs (downcase
				     (substring man-args mstart mend)))
	    s mend))
    (concat newargs (substring man-args s len))))

(defun sm-translate-references (ref)
  "Translates REF from \"chmod(2V)\" to \"2v chmod\" style."
  (if (string-match (concat "(" sm-section-regexp ")$") ref)
      (let* ((word (progn (string-match "(" ref)
			  (substring ref 0 (1- (match-end 0)))))
	     (section-re (concat "(\\(" sm-section-regexp "\\))"))
	     (section (if (string-match section-re ref)
			  (substring ref (match-beginning 1) (match-end 1))
			""))
	     (slist sm-section-translations-alist)
	     )
	(if sm-downcase-section-letters-p
	    (setq section (sm-downcase section)))
	(while slist
	  (let ((s1 (car (car slist)))
		(s2 (cdr (car slist))))
	    (setq slist (cdr slist))
	    (if sm-downcase-section-letters-p
		(setq s1 (sm-downcase s1)))
	    (if (not (string= s1 section)) nil
	      (setq section (if sm-downcase-section-letters-p
				(sm-downcase s2)
			      s2)
		    slist nil))))
	(concat section " " word))
    ref))

(defun sm-linepos (&optional position col-p)
  "Return the character position at various line/buffer positions.
Preserves the state of point, mark, etc. Optional POSITION can be one
of the following symbols:
     bol == beginning of line
     boi == beginning of indentation
     eol == end of line [default]
     bob == beginning of buffer
     eob == end of buffer

Optional COL-P non-nil returns current-column instead of character position."
  (let ((tpnt (point))
	rval)
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bob) (goto-char (point-min)))
     ((eq position 'eob) (goto-char (point-max)))
     (t (end-of-line)))
    (setq rval (if col-p (current-column) (point)))
    (goto-char tpnt)
    rval))


;; ======================================================================
;; default man entry and get word under point

(defun sm-default-man-args (manword)
  "Build the default man args from MANWORD and major-mode."
  (let ((mode major-mode)
	(slist sm-auto-section-alist))
    (while (and slist
		(not (eq (car (car slist)) mode)))
      (setq slist (cdr slist)))
    (if (not slist)
	manword
      (let ((sections (cdr (car slist))))
	(if (not (listp sections))
	    (concat sections " " manword)
	  (let ((manarg ""))
	    (while sections
	      (setq manarg (concat manarg " " (car sections) " " manword))
	      (setq sections (cdr sections)))
	    manarg)
	  )))))

(defun sm-default-man-entry ()
  "Make a guess at a default manual entry.
This guess is based on the text surrounding the cursor, and the
default section number is selected from sm-auto-section-alist."
  (let ((default-section nil)
	default-title)
    (save-excursion
      
      ;; Default man entry title is any word the cursor is on,
      ;; or if cursor not on a word, then nearest preceding
      ;; word.
      (and (not (looking-at "[a-zA-Z_]"))
	   (skip-chars-backward "^a-zA-Z_"))
      (skip-chars-backward "(a-zA-Z_0-9")
      (and (looking-at "(") (forward-char 1))
      (setq default-title
	    (buffer-substring
	     (point)
	     (progn (skip-chars-forward "a-zA-Z0-9_") (point))))
      
      ;; If looking at something like ioctl(2) or brc(1M), include
      ;; section number in default-entry
      (if (looking-at "[ \t]*([ \t]*[0-9][a-zA-Z]?[ \t]*)")
	  (progn (skip-chars-forward "^0-9")
		 (setq default-section
		       (buffer-substring
			(point)
			(progn
			  (skip-chars-forward "0-9a-zA-Z")
			  (point)))))
	
	;; Otherwise, assume section number to be 2 if we're
	;; in C code
	(and (eq major-mode 'c-mode)
	     (setq default-section "2")))
      (if default-section
	  (format "%s %s" default-section default-title)
	default-title))))
	 

;; ======================================================================
;; top level command and background process sentinel

(defun sm-manual-entry (arg)
  "Get a Un*x manual page and put it in a buffer.
This command is the top-level command in the superman package. It runs
a Un*x command to retrieve and clean a manpage in the background and
places the results in a sm-manual-mode (manpage browsing) buffer. See
variable sm-notify for what happens when the buffer is ready.
Universal argument ARG, is passed to sm-getpage-in-background."
  (interactive "P")
  (let* ((default-entry (sm-default-man-entry))
	 (man-args
	  (read-string (format "%sman "
			(if (string= default-entry "") ""
			  (format "(default: man %s) "
				  default-entry))))))
    (and (string= man-args "")
	 (if (string= default-entry "")
	     (error "No man args given.")
	   (setq man-args default-entry)))
    (if sm-downcase-section-letters-p
	(setq man-args (sm-downcase man-args)))
    (sm-getpage-in-background man-args (consp arg))
    ))

(defun sm-getpage-in-background (man-args &optional override-reuse-p)
  "Uses MAN-ARGS to build and fire off the manpage and cleaning command.
Optional OVERRIDE-REUSE-P, when supplied non-nil forces superman to
start a background process even if a buffer already exists and
sm-reuse-okay-p is non-nil."
  (let* ((bufname (concat "*man " man-args "*"))
	 (buffer  (get-buffer bufname)))
    (if (and sm-reuse-okay-p
	     (not override-reuse-p)
	     buffer)
	(sm-notify-when-ready buffer)
      (message "Invoking man %s in background..." man-args)
      (setq buffer (generate-new-buffer bufname))
      (set-process-sentinel
       (start-process "man" buffer "sh" "-c"
		      (format (sm-build-man-command) man-args))
       'sm-bgproc-sentinel))
    ))

(defun sm-notify-when-ready (man-buffer)
  "Notify the user when MAN-BUFFER is ready.
See the variable sm-notify for the different notification behaviors."
  (cond
   ((eq sm-notify 'bully)
    (pop-to-buffer man-buffer)
    (delete-other-windows-quietly))
   ((eq sm-notify 'aggressive)
    (pop-to-buffer man-buffer))
   ((eq sm-notify 'friendly)
    (display-buffer man-buffer 'not-this-window))
   ((eq sm-notify 'polite)
    (beep)
    (message "Manual buffer %s is ready." (buffer-name man-buffer)))
   ((eq sm-notify 'quiet)
    (message "Manual buffer %s is ready." (buffer-name man-buffer)))
   ((or (eq sm-notify 'meek)
	t)
    (message ""))
   ))

(defun sm-bgproc-sentinel (process msg)
  "Manpage background process sentinel."
  (let ((man-buffer (process-buffer process))
	(delete-buff nil)
	(err-mess nil))
    (if (null (buffer-name man-buffer)) ;; deleted buffer
	(set-process-buffer process nil)
      (save-excursion
	(set-buffer man-buffer)
	(goto-char (point-min))
	(cond ((or (looking-at "No \\(manual \\)*entry for")
		   (looking-at "[^\n]*: nothing appropriate$"))
	       (setq err-mess (buffer-substring (point) (sm-linepos 'eol))
		     delete-buff t)
	       )
	      ((not (and (eq (process-status process) 'exit)
			 (= (process-exit-status process) 0)))
	       (setq err-mess
		     (concat (buffer-name man-buffer)
			     ": process "
			     (let ((eos (1- (length msg))))
			       (if (= (aref msg eos) ?\n)
				   (substring msg 0 eos) msg))))
	       (goto-char (point-max))
	       (insert (format "\nprocess %s" msg))
	       )))
      (if delete-buff
	  (kill-buffer man-buffer)
	(save-window-excursion
	  (save-excursion
	    (set-buffer man-buffer)
	    (sm-manual-mode)
	    (set-buffer-modified-p nil)))
	(sm-notify-when-ready man-buffer))

      (if err-mess
	  (error err-mess))
      )))


;; ======================================================================
;; set up manual mode in buffer and build alists

(defun sm-manual-mode ()
  "SUPERMAN 1.0: A mode for browsing Un*x manual pages.

The following superman commands are available in the buffer. Try
\"\\[describe-key] <key> RET\" for more information:

\\[sm-manual-entry]       Prompt to retrieve a new manpage.
\\[sm-follow-manual-reference]       Retrieve reference in SEE ALSO section.
\\[sm-next-manpage]   Jump to next manpage in circular list.
\\[sm-previous-manpage]   Jump to previous manpage in circular list.
\\[sm-next-section]       Jump to next manpage section.
\\[sm-previous-section]       Jump to previous manpage section.
\\[sm-goto-section]       Go to a manpage section.
\\[sm-goto-see-also-section]       Jumps to the SEE ALSO manpage section.
\\[sm-quit]       Deletes the manpage, its buffer, and window.
\\[sm-version]       Prints superman's version number.
\\[describe-mode]       Prints this help text.

The following variables may be of some use. Try
\"\\[describe-variable] <variable-name> RET\" for more information:

sm-notify                      What happens when manpage formatting is done.
sm-reuse-okay-p                Okay to reuse already formatted buffer?
sm-overload-p                  Fset manual-entry command to sm-manual-entry?
sm-downcase-section-letters-p  Force section letters to lower case?
sm-circular-pages-p            Multiple manpage list treated as circular?
sm-auto-section-alist          List of major modes and their section numbers.
sm-section-translations-alist  List of section numbers and their Un*x equiv.
sm-filter-list                 Background manpage filter command.
sm-mode-line-format            Mode line format for sm-manual-mode buffers.
sm-mode-map                    Keymap bindings for sm-manual-mode buffers.
sm-mode-hooks                  Hooks for sm-manual-mode.
sm-section-regexp              Regexp describing manpage section letters.
sm-heading-regexp              Regexp describing section headers.
sm-see-also-regexp             Regexp for SEE ALSO section (or your equiv).
sm-first-heading-regexp        Regexp for first heading on a manpage.
sm-reference-regexp            Regexp matching a references in SEE ALSO.
sm-version-number              Superman version number.

The following key bindings are currently in effect in the buffer:
\\{sm-mode-map}"
  (interactive)
  (setq major-mode 'sm-manual-mode
	mode-name "Manual"
	buffer-auto-save-file-name nil
	mode-line-format sm-mode-line-format
	truncate-lines t
	buffer-read-only t)
  (buffer-flush-undo (current-buffer))
  (auto-fill-mode -1)
  (use-local-map sm-mode-map)
  (goto-char (point-min))
  (sm-build-page-list)
  (sm-goto-page 1))

(defun sm-build-section-alist ()
  "Build the association list of manpage sections."
  (setq sm-sections-alist nil)
  (goto-char (point-min))
  (while (re-search-forward sm-heading-regexp (point-max) t)
    (aput 'sm-sections-alist
	  (buffer-substring (sm-linepos 'bol) (sm-linepos)))
    (forward-line 1)
    ))

(defun sm-build-references-alist ()
  "Build the association list of references (in the SEE ALSO section)."
  (setq sm-refpages-alist nil)
  (save-excursion
    (if (sm-find-section sm-see-also-regexp)
	(let ((start (progn (forward-line 1) (point)))
	      (end (progn
		     (sm-next-section 1)
		     (point)))
	      hyphenated
	      (runningpoint -1))
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (back-to-indentation)
	  (while (and (not (eobp)) (/= (point) runningpoint))
	    (setq runningpoint (point))
	    (let* ((bow (point))
		   (eow (re-search-forward sm-reference-regexp end t))
		   (word (buffer-substring bow (match-end 0)))
		   (len (1- (length word))))
	      (if (not eow) nil
		(if hyphenated
		    (setq word (concat hyphenated word)
			  hyphenated nil))
		(if (= (aref word len) ?-)
		    (setq hyphenated (substring word 0 len))
		  (aput 'sm-refpages-alist word))))
	    (skip-chars-forward " \t\n,"))
	  ))))

(defun sm-build-page-list ()
  "Build the list of separate manpages in the buffer."
  (setq sm-page-list nil)
  (save-excursion
    (let ((page-start (sm-linepos 'bob))
	  (page-end (sm-linepos 'eob))
	  (regexp sm-first-heading-regexp))
      (goto-char (point-min))
      (re-search-forward regexp (point-max) t)
      (while (not (eobp))
	(if (re-search-forward regexp (point-max) t)
	    (progn
	      (setq page-end (sm-linepos 'bol))
	      (end-of-line))
	  (goto-char (point-max))
	  (setq page-end (point)))
	(setq sm-page-list (append sm-page-list
				   (list (cons page-start page-end)))
	      page-start page-end)
	))))  


;; ======================================================================
;; sm-manual-mode commands

(defun sm-next-section (n)
  "Move point to Nth next section (default 1)."
  (interactive "p")
  (if (looking-at sm-heading-regexp)
      (forward-line 1))
  (if (re-search-forward sm-heading-regexp (point-max) t n)
      (beginning-of-line)
    (goto-char (point-max))))

(defun sm-previous-section (n)
  "Move point to Nth previous section (default 1)."
  (interactive "p")
  (if (looking-at sm-heading-regexp)
      (forward-line -1))
  (if (re-search-backward sm-heading-regexp (point-min) t n)
      (beginning-of-line)
    (goto-char (point-min))))

(defun sm-find-section (section)
  "Move point to SECTION if it exists, otherwise don't move point.
Returns t if section is found, nil otherwise."
  (let ((curpos (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" section) (point-max) t)
	(progn (beginning-of-line) t)
      (goto-char curpos)
      nil)
    ))

(defun sm-goto-section ()
  "Query for section to move point to."
  (interactive)
  (aput 'sm-sections-alist
	(let* ((default (aheadsym sm-sections-alist))
	       (completion-ignore-case t)
	       chosen
	       (prompt (concat "Go to section: (default " default ") ")))
	  (setq chosen (completing-read prompt sm-sections-alist))
	  (if (or (not chosen)
		  (string= chosen ""))
	      default
	    chosen)))
  (sm-find-section (aheadsym sm-sections-alist)))

(defun sm-goto-see-also-section ()
  "Move point the the \"SEE ALSO\" section.
Actually the section moved to is described by sm-see-also-regexp."
  (interactive)
  (if (not (sm-find-section sm-see-also-regexp))
      (error (concat "No " sm-see-also-regexp
		     " section found in current manpage."))))

(defun sm-follow-manual-reference (arg)
  "Get one of the manpages referred to in the \"SEE ALSO\" section.
Queries you for the page to retrieve. Of course it does this in the
background. Universal argument ARG is passed to sm-getpage-in-background."
  (interactive "P")
  (if (not sm-refpages-alist)
      (error (concat "No references found in current manpage."))
    (aput 'sm-refpages-alist
	  (let* ((default (aheadsym sm-refpages-alist))
		 chosen
		 (prompt (concat "Refer to: (default " default ") ")))
	    (setq chosen (completing-read prompt sm-refpages-alist nil t))
	    (if (or (not chosen)
		    (string= chosen ""))
		default
	      chosen)))
    (sm-getpage-in-background
     (sm-translate-references (aheadsym sm-refpages-alist))
     (consp arg))))

(defun sm-quit ()
  "Kill the buffer containing the manpage."
  (interactive)
  (let ((buff (current-buffer)))
    (delete-windows-on buff)
    (kill-buffer buff)))

(defun sm-goto-page (page)
  "Go to the manual page on page PAGE."
  (interactive
   (if (not sm-page-list)
       (error "You're looking at the only manpage in the buffer.")
     (format "nGo to manpage [1-%d]: " (length sm-page-list))))
  (if (or (< page 1)
	  (> page (length sm-page-list)))
      (error "No manpage %d found" page))
  (let* ((page-range (nth (1- page) sm-page-list))
	 (page-start (car page-range))
	 (page-end (cdr page-range)))
    (setq sm-current-page page
	  sm-page-mode-string (sm-page-mode-string))
    (widen)
    (goto-char page-start)
    (narrow-to-region page-start page-end)
    (sm-build-section-alist)
    (sm-build-references-alist)
    (widen)
    (narrow-to-region page-start page-end)
    (goto-char (point-min))))


(defun sm-next-manpage ()
  "Find the next manpage entry in the buffer."
  (interactive)
  (if (= (length sm-page-list) 1)
      (error "This is the only manpage in the buffer."))
  (if (< sm-current-page (length sm-page-list))
      (sm-goto-page (1+ sm-current-page))
    (if sm-circular-pages-p
	(sm-goto-page 1)
      (error "You're looking at the last manpage in the buffer."))))

(defun sm-previous-manpage ()
  "Find the previous manpage entry in the buffer."
  (interactive)
  (if (= (length sm-page-list) 1)
      (error "This is the only manpage in the buffer."))
  (if (> sm-current-page 1)
      (sm-goto-page (1- sm-current-page))
    (if sm-circular-pages-p
	(sm-goto-page (length sm-page-list))
      (error "You're looking at the first manpage in the buffer."))))

(defun sm-version (arg)
  "Show superman's version.
Universal argument (\\[universal-argument]) ARG inserts version
information in the current buffer instead of printing the message in
the echo area."
  (interactive "P")
  (if (consp arg)
      (insert "Using Superman version " sm-version-number ".")
    (message "Using Superman version %s." sm-version-number)))

;; ======================================================================
;; overloading

(if (and sm-overload-p
	 (fboundp 'manual-entry))
    (fset 'manual-entry (symbol-function 'sm-manual-entry)))
