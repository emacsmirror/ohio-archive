;;; -*- Mode: Emacs-Lisp -*-

;;; File:		manual.el
;;; Description:	Manual page formatter
;;; Author:		Eric Rose <erose@jessica.stanford.edu>
;;; Last Modified:	4 Apr 1991
;;; Version:		1.0

;;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; This file is a modification of man.el, in the standard distribution.
;; Most of the code is taken from it, however, various improvements have
;; been made, such as a larger number of preset headers to delete,
;; as well as the ability to follow words and man page references.
;;
;; Manual browsing under emacs.  To use, type "M-x man".  You will be
;; prompted for an entry.  To get a man page on a
;; word pointed to by the cursor, type "m", or if there is no word it
;; will query the user for a topic.  This is useful for following manual
;; reference, and by typing "s", you can jump immediately to the "SEE ALSO"
;; section.  See the help for more info.
;;
;;  Need to add:
;;    Keep a list of man pages read, so that one can back up.
;;    Support for man -k.
;;
;; To autoload:
;; (autoload 'man "manual" "Improved Manual Browsing" t)

;;;
;;; 12/19/91 isy
;;;   - modified so that the manual page buffer is "popped" up instead of
;;;     replacing the currently viewed window
;;;

(provide 'manual)

;; Manual Variables

(defvar manual-non-word-regexp "[^A-Za-z0-9()_]")
(defvar manual-buffer-name "*Manual Entry*")
(defvar manual-temp-buffer "*Manual Input*")
;;
;;  Manual keymap
;;

(defconst manual-mode-map nil)
(if manual-mode-map
    nil
  (setq manual-mode-map (make-keymap))
  (suppress-keymap manual-mode-map)
  (define-key manual-mode-map " " 'scroll-up)
  (define-key manual-mode-map "\177" 'scroll-down)
  (define-key manual-mode-map "n" 'manual-forward-line)
  (define-key manual-mode-map "p" 'manual-backward-line)
  (define-key manual-mode-map "\en" 'manual-next-section)
  (define-key manual-mode-map "\ep" 'manual-previous-section)
  (define-key manual-mode-map "\C-n" 'manual-forward-line)
  (define-key manual-mode-map "\C-p" 'manual-backward-line)
  (define-key manual-mode-map "m" 'manual-open-word)
  (define-key manual-mode-map "a" 'manual-appropos)
  (define-key manual-mode-map "g" 'manual-get-word)
  (define-key manual-mode-map "s" 'manual-see-also)
  (define-key manual-mode-map "\C-a" 'beginning-of-line)
  (define-key manual-mode-map "\C-e" 'end-of-line)
  (define-key manual-mode-map "\ea" 'backward-sentence)
  (define-key manual-mode-map "\ee" 'forward-sentence)
  (define-key manual-mode-map "\C-b" 'backward-char)
  (define-key manual-mode-map "\C-f" 'forward-char)
  (define-key manual-mode-map "b" 'manual-backward-word)
  (define-key manual-mode-map "f" 'manual-forward-word)
  (define-key manual-mode-map "\eb" 'backward-word)
  (define-key manual-mode-map "\ef" 'forward-word)
  (define-key manual-mode-map "<" 'beginning-of-buffer)
  (define-key manual-mode-map "." 'beginning-of-buffer)
  (define-key manual-mode-map ">" 'end-of-buffer)
  (define-key manual-mode-map "\e<" 'beginning-of-buffer)
  (define-key manual-mode-map "\e>" 'end-of-buffer)
  (define-key manual-mode-map "?" 'describe-mode)
  (define-key manual-mode-map "t" 'toggle-truncate-lines)
  (define-key manual-mode-map "q" 'manual-quit))

;; Stub for typing "man"
(defun man (word)
  "Displays a man page."
  (interactive "sTopic: ")
  (if (get-manual-entry word)
      (save-excursion
	(set-buffer (get-buffer-create manual-buffer-name))
	(setq buffer-read-only nil)
	(set-buffer manual-temp-buffer)
	(copy-to-buffer manual-buffer-name (point-min) (point-max))))
  (manual-mode))

;; Manual mode
(defun manual-mode ()
  "Manual Mode is used to browse through manual pages.  Normal editing commands
are turned off, and these can be used instead:

.	Move to the top of the current man page.
SPC	Scroll down one page.
DEL	Scroll up one page.
n,C-n	Move down one line.
p,C-p	Move up one line.
M-n	Move to next section of the current page.
M-p	Move to previous section of the current page.
>	Move to end of man page.
<	Move to beginning of man page.
m	Get man page on the word the cursor is on.  If the cursor is not
        pointing to any text, type in TOPIC(SECTION) or TOPIC at the prompt.
g       Gets the man page on the topic entered at the prompt.  Same format
        as above: TOPIC(SECTION).
s	Jump to the 'SEE ALSO' section.
C-a	Beginning of line.
C-e	End of line.
M-a	Previous sentence.
M-e	Next sentence.
f,M-f	Move forward one word.
b,M-b   Move backwards one word.
t       Toggle the line truncation.
?	This help screen."
  (interactive)
  (save-excursion			 ; 12/19/91 isy
    (switch-to-buffer manual-buffer-name)
    (setq major-mode 'manual-mode)
    (setq mode-name "Manual")
    (setq buffer-auto-save-file-name nil)
    (setq truncate-lines t)
    (use-local-map manual-mode-map)
    (setq buffer-read-only t)
    ;;(delete-other-windows)		 ; 12/19/91 isy
    )
  (let ((pop-up-windows t))
    (display-buffer manual-buffer-name)) ; 12/19/91 isy
  (message "Type ? for a list of commands"))

;;
;; Does the work
(defun get-manual-entry (topic)
  "Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  (let ((section nil))
    (if (null topic) nil)
    (if (and (null section)
	     (string-match "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'" topic))
	(setq section (substring topic (match-beginning 2)
				 (match-end 2))
	      topic (substring topic (match-beginning 1)
			       (match-end 1))))
    (with-output-to-temp-buffer manual-temp-buffer
      (buffer-flush-undo standard-output)
      (save-excursion
	(set-buffer standard-output)
	(message "Looking for formatted entry for %s%s..."
		 topic (if section (concat "(" section ")") ""))
	(let ((dirlist manual-formatted-dirlist)
	      (case-fold-search nil)
	      name)
	  (if (and section (or (file-exists-p
				(setq name (concat manual-formatted-dir-prefix
						   (substring section 0 1)
						   "/"
						   topic "." section)))
			       (file-exists-p
				(setq name (concat manual-formatted-dir-prefix
						   section
						   "/"
						   topic "." section)))))
	      (insert-manual-file name)
	    (while dirlist
	      (let* ((dir (car dirlist))
		     (name1 (concat dir "/" topic "."
				    (or section
					(substring
					 dir
					 (1+ (or (string-match "\\.[^./]*$" dir)
						 -2))))))
		     completions)
		(if (file-exists-p name1)
		    (insert-manual-file name1)
		  (condition-case ()
		      (progn
			(setq completions (file-name-all-completions
					   (concat topic "." (or section ""))
					   dir))
			(while completions
			  (insert-manual-file (concat dir "/" (car completions)))
			  (setq completions (cdr completions))))
		    (file-error nil)))
		(goto-char (point-max)))
	      (setq dirlist (cdr dirlist)))))
	
	(if (= (buffer-size) 0)
	    (progn
	      (message "No formatted entry, invoking man %s%s..."
		       (if section (concat section " ") "") topic)
	      (if section
		  (call-process manual-program nil t nil section topic)
	        (call-process manual-program nil t nil topic))
	      (if (< (buffer-size) 80)
		  (progn
		    (goto-char (point-min))
		    (end-of-line)
		    (error (buffer-substring 1 (point)))
		    nil))))
	
	(message "Cleaning manual entry for %s..." topic)
	(nuke-nroff-bs)
	(set-buffer-modified-p nil)
	(message ""))))
  t)

;; Hint: BS stands form more things than "back space"
(defun nuke-nroff-bs ()
  (interactive "*")
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      (cond ((= preceding following)
	     ;; x\bx
	     (delete-char -2))
	    ((= preceding ?\_)
	     ;; _\b
	     (delete-char -2))
	    ((= following ?\_)
	     ;; \b_
	     (delete-region (1- (point)) (1+ (point))))
	    ((= following ?\+)
	     ;; \b+
	     (delete-region (1- (point)) (1+ (point)))))))

  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point))

  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (goto-char (point-min))
  (forward-line 1)
  (while (re-search-forward "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$" nil t)
    (replace-match ""))
  
  ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
  ;;    Sun appear to be on drugz:
  ;;     "Sun Release 3.0B  Last change: 1 February 1985     1"
  ;;    HP are even worse!
  ;;     "     Hewlett-Packard   -1- (printed 12/31/99)"  FMHWA12ID!!
  ;;    System V (well WICATs anyway):
  ;;     "Page 1			  (printed 7/24/85)"
  ;;    Who is administering PCP to these corporate bozos?
  (goto-char (point-min))
  (while (re-search-forward
	   (cond ((eq system-type 'hpux)
		  "^[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*$")
		 ((eq system-type 'usg-unix-v)
		  "^ *Page [0-9]*.*(printed [0-9/]*)$")
		 (t
		  "^\\(Printed\\|Sun Release\\) [0-9].*[0-9]$"))
	   nil t)
    (replace-match ""))

  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n")))


(defun insert-manual-file (name)
  ;; Insert manual file (unpacked as necessary) into buffer
  (if (equal (substring name -2) ".Z")
      (call-process "zcat" nil t nil name)
    (if (equal (substring name -2) ".z")
	(call-process "pcat" nil t nil name)
      (insert-file-contents name))))
;
; Added by erose
;
(defun manual-forward-line (n)
  (interactive "p")
  (forward-line n))

(defun manual-backward-line (n)
  (interactive "p")
  (forward-line (- n)))

(defun manual-forward-word (n)
  (interactive "p")
  (forward-char 1)
  (forward-word n)
  (forward-char -1))

(defun manual-backward-word (n)
  (interactive "p")
  (backward-word n))

;  Searches for next "Section"
(defun manual-next-section ()
  (interactive)
  (beginning-of-line)
  (forward-line)
  (while (not (or (looking-at "[A-Z]")
		  (eobp)))
    (forward-line 1)))

(defun manual-previous-section ()
  (interactive)
  (beginning-of-line)
  (forward-line -1)
  (while (not (or (looking-at "[A-Z]")
		  (bobp)))
    (forward-line -1)))

;  Queries for the man page, same as manual-open-word below other than that.
(defun manual-get-word ()
  (interactive)
  (let ((word))
    (save-excursion
      (if (not (setq word (call-interactively 'manual-get-topic)))
	  nil)
      (if (get-manual-entry word)
	  (save-excursion
	    (setq buffer-read-only nil)
	    (set-buffer manual-temp-buffer)
	    (copy-to-buffer manual-buffer-name (point-min) (point-max)))))
    (setq buffer-read-only t)
    (delete-other-windows)
    (message "%s" word)))

;  Tries to find the man page for this word
(defun manual-open-word ()
  (interactive)
  (let ((word))
    (save-excursion
      (if (not (manual-mark-word))
	  (if (not (setq word (call-interactively 'manual-get-topic)))
	      nil)
	(setq word (buffer-substring (region-beginning) (region-end))))
      (if (get-manual-entry word)
	  (save-excursion
	    (setq buffer-read-only nil)
	    (set-buffer manual-temp-buffer)
	    (copy-to-buffer manual-buffer-name (point-min) (point-max)))))
    (setq buffer-read-only t)
    (delete-other-windows)
    (message "%s" word)))
;
(defun manual-get-topic (topic)
  (interactive "sManual entry (topic): ")
  topic)
;
;  Marks a word that can be a manual reference.  Returns nil if it can't
;  figure it out.
;
(defun manual-mark-word ()
  "Marks a word that should correspond to a manual refernce.
The variable 'manual-non-word-regexp' controls what is not part of a manual
refernce."
  (interactive)
  (if (looking-at manual-non-word-regexp)
      nil
    (if (not (re-search-forward manual-non-word-regexp nil t))
	nil
      (forward-char -1)
      (push-mark nil nil)
      (if (not (re-search-backward manual-non-word-regexp nil t))
	  nil
	(forward-char 1)
	t))))
	
;
(defun manual-see-also ()
  (interactive)
  (let ((opoint (point))
	(case case-fold-search))
    (goto-char (point-min))
    (setq case-fold-search nil)
    (if (not (search-forward "SEE ALSO" nil t))
	(progn
	  (goto-char opoint)
	  (message "No 'SEE ALSO' section on this manpage.")))
    (setq case-fold-search case)))
	
;
(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

;
(defun manual-quit ()
  (interactive)
  (let ((obuf (current-buffer)))
    (switch-to-buffer (other-buffer))
    (bury-buffer obuf)))
