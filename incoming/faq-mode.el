;;; faq-mode.el -- browse FAQs with ease.
;;; Copyright 1997 by Brent Burton

;; Author:       Brent Burton  <brentb@io.com>
;; Created:      July 1997
;; Version:      $Revision: 2.4 $ $Date: 1997/08/07 19:00:59 $
;; Keywords:     FAQ browser, FAQ reader, faq-mode, FAQ-mode
;; Availability: http://www.io.com/~brentb/emacs/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a FAQ browser for Emacs.  This mode makes reading and
;; navigating some FAQs easier by creating a question index into the
;; FAQ body.  While the reader peruses the questions in the index,
;; the content window displays just the answer to whichever question
;; is selected.

;; The ability to automatically discern question bodies is dependent
;; on the format of the FAQ.  Faq-mode looks for two major types: a
;; custom format designed specifically for faq-mode that includes a
;; pre-built index in the FAQ, and the simplified digest (RFC1153)
;; format used by many FAQ maintainers.  The third "format" is a
;; catch-all set of patterns, created from a survey of many FAQ
;; formats.  The bottom line is this: faq-mode is easily fooled, but
;; it performs its job surprisingly well on a wide variety of FAQs.

;;; Code:

;; Mode variables
;; ---- ---------

;; Public, user-configurable variables:
(defvar faq-mode-hook nil
  "List of functions to call when entering faq mode.")

(defvar faq-mode-map nil
  "Keymap for faq major mode.")

;; "Private" variables for internal use:
(defvar faq-re-index-start nil
  "Regexp to match the start of the FAQ index.  If the FAQ has no
index, faq-re-index-start should be set to nil which tells faq-mode
to manually build an index.
f-r-i-s is automagically set by faq-mode.")

(defvar faq-re-index-end nil
  "Regexp to match the end of the FAQ index.  If the FAQ has no
index, faq-re-index-end should be set to nil.
f-r-i-e is automagically set by faq-mode.")

(defvar faq-re-index-question nil
  "Regexp to locate a question number inside the index.
f-r-i-q is automagically set by faq-mode.")

(defvar faq-re-answer-start nil
  "Regexp to locate the beginning of a question in the FAQ body.
f-r-a-s is automagically set by faq-mode.")

(defvar faq-re-answer-end nil
  "Regexp that marks the end of a question in the FAQ body.  The
special case of locating the end of the last question will result
in using the end-of-buffer for the end-of-question.
f-r-a-e is automagically set by faq-mode.")

(defvar faq-index-start 0
  "Character position in the buffer at which the index is detected.")

(defvar faq-index-end 0
  "Character position that is determined to be the last position of
 the index.")

(defvar faq-index-buffer nil
  "The BUFFER in which faq-mode displays the question index.")

(defvar faq-content-buffer nil
  "The BUFFER in which faq-mode displays FAQ content.")

(defvar faq-index-window nil
  "The WINDOW containing the FAQ's index.")

(defvar faq-content-window nil
  "The WINDOW containing the FAQ's contents.")

(defvar faq-regions  nil
  "An associated list to track point locations for text indexing.

Each item is a triplet of the form (KEY START END) and specifies the range
in the original buffer where the question identified by KEY is located.
KEY is a string containing some number format, and needs to be unique.
START and END are buffer positions.")


;; Describe key bindings for faq mode.  Do it here so when faq-mode is
;; finally loaded, it is created.
(if faq-mode-map
    nil
  (setq faq-mode-map (make-sparse-keymap))
  (define-key faq-mode-map "n" 'forward-faq)
  (define-key faq-mode-map "p" 'backward-faq)
  (define-key faq-mode-map "q" 'quit-faq)
  (define-key faq-mode-map " " 'scroll-faq-up))

;; Faq-mode Functions
;; -------- ---------
;; The functions are preceded by a PUBLIC or PRIVATE comment to indicate
;; what is internally called versus callable by the user.

;; PUBLIC
;;;###autoload
(defun faq-mode ()
  "Major mode for reading faq files.

When invoked manually on a buffer containing a FAQ or when started
automatically via the auto-mode-alist mechanism, faq-mode creates an
index of the FAQ and displays it above the FAQ body.  Selecting a
question in the index displays the answer in the body.  For more
information, fetch the faq-mode FAQ, available at
  http://www.io.com/~brentb/emacs/

Special commands:
\\{faq-mode-map}"

  (interactive)

  ;; Create and juggle the window/buffer settings a lot.
  (setq faq-content-buffer (get-buffer (buffer-name)))
  (select-window (setq faq-content-window
		       (split-window-vertically)))
  ;; Now in bottom (content) window.  Make index buffer and display
  ;; it in top window.
  (setq faq-index-buffer
	(get-buffer-create
	 (generate-new-buffer-name
	  (concat "Index of " (buffer-name faq-content-buffer)))))
  (select-window (setq faq-index-window
		       (display-buffer faq-index-buffer)))
  (set-buffer faq-index-buffer)
  (insert "\n")

  ;; Now in the index buffer (top window), so clear the local vars, set mode.
  (kill-all-local-variables)
  (setq major-mode 'faq-mode)
  (setq mode-name "FAQ")
  (use-local-map faq-mode-map)

  ;; General settings
  (setq case-fold-search nil)		; automatically buffer-local
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    '(lambda ()			; When we get killed, clean up.
	       (delete-window)))

  ;; Identify the FAQ format and set the faq-re-* variables.
  ;; The faq-re-* variables set are global at this point, so they act
  ;; as defaults.
  (faq-identify-format)

  ;; Generate the indexing offsets for each question.  This uses the
  ;; the faq-re-* variables to locate or build the index.
  (setq faq-regions (faq-make-index))

  ;; Now that the content buffer has been parsed, set the local
  ;; variables.  Global values were used to generate the index
  ;; and local values allow many FAQs to be viewed at once.
  (select-window faq-index-window)
  (set-buffer faq-index-buffer)
  (goto-char (point-min))
  (make-local-variable 'faq-content-buffer)
  (make-local-variable 'faq-index-buffer)
  (make-local-variable 'faq-content-window)
  (make-local-variable 'faq-index-window)
  (make-local-variable 'faq-regions)
  (make-local-variable 'faq-re-index-start)
  (make-local-variable 'faq-re-index-question)
  (make-local-variable 'faq-re-index-end)
  (make-local-variable 'faq-re-answer-start)
  (make-local-variable 'faq-re-answer-end)

  (toggle-read-only 1)
  (run-hooks 'faq-mode-hook)
  (forward-faq))

;; ----
;; Faq-mode utility functions


;; PRIVATE
(defun faq-identify-format ()
  ;; "Examine the format of the content buffer for common types,
  ;;  and guess at the format."

  (let ((test-re nil))

    (set-buffer faq-content-buffer)
    (goto-char (point-min))
    (re-search-forward "^$")		; Skip past any article headers

    ;; This cond form tests various formats to set the regexp
    ;; variables correctly.
    (cond

     ;; IF... it's an easy-to-grok Official Faq-Mode(TM) format:
     ((save-excursion
	(setq test-re "^\\(.\\)\\1\\1\\1 faq-mode$")
	(re-search-forward test-re (point-max) t 1))

      ;; THEN... set this format's regexps.
      (message "FAQ-mode: I think this FAQ's in faq-mode(TM) format...")
      (setq faq-re-index-start test-re)
      (setq faq-re-index-end   test-re)
      (setq faq-re-index-question "^\\(\\[[0-9]+\\([\\.-][0-9]+\\)*\\]\\)")
      (setq faq-re-answer-start faq-re-index-question)
      (setq faq-re-answer-end   faq-re-index-question))

     ;; ELSE ----
     ;; IF... it's a minimal digest format (pseudo-RFC1153)
     ;; Subject lines are: newline, dashes(30), blank line, ^Subject:
     ;; Some FAQs surveyed do not have the blank line.
     ((save-excursion
	(setq test-re
	      (concat "^----------------+\n\\(\n\\)?"
		      "\\(From: .*\n\\)?Subject: "))
	(re-search-forward test-re (point-max) t 1))

      ;; THEN... set this format's regexps
      (message "FAQ-mode: I think this FAQ is in a simple digest format...")

      (setq faq-re-index-start nil)	; nil means no reliable index
      (setq faq-re-index-end   nil)
      ;; Read the following as "locate a number perhaps surrounded by garbage"
      (setq faq-re-index-question
	    "^\\([^ \t\n]*[0-9]+\\(\\.[0-9]+\\)*[^ \t\n]*\\)")
      ;; For the answer-start regexp, note the first word after Subject:
      (setq faq-re-answer-start  "^Subject:[ \t]+\\([^ \t]+\\).*$")
      (setq faq-re-answer-end    "^----------------+"))


     ;; ELSE ----
     ;; IF... unknown format, make a logical guess at formatting.
     (t

      ;; THEN...
      (message "FAQ-mode: unknown format.  Pray for rain.")

      (let ((prefixes "^ *\\([-Q\\[\\.]*")
	    (dot-number "[0-9]+\\(\\.[0-9]+\\)*")
	    (suffixes "[]\\.:-]+\\)"))
	;; some faqs use formats like: "-32-", "Q.61", "[21.4]" "23:" etc.
	;; prefixes matches      "-" "[" "Q" "." before the number.
	;; dotted # form matches "12" "1.2" "1.2.1" etc
	;; suffixes matches      "-" "]" "." ":" after the number
	(setq test-re
	      (concat prefixes dot-number suffixes))

	(setq faq-re-index-start nil)
	(setq faq-re-index-end   nil)
	(setq faq-re-index-question test-re)
	(setq faq-re-answer-start test-re)
	(setq faq-re-answer-end   test-re)))
     )))


;; PRIVATE
(defun faq-make-index ()
  ;; "Parses the current buffer for index delimeters.  If found, it
  ;; copies the index to the faq-index-buffer.  If not found, it parses
  ;; this buffer's contents looking for question delimeters.  It then
  ;; copies the questions and question keys to the faq-index-buffer.
  ;; At the same time, it finds the bounds of each question in the FAQ
  ;; buffer and builds/returns the assoc-list stored in faq-regions."
  (message "Indexing the FAQ...")
  (let (return-assoc
	(answer-body-start 0)
	(answer-body-end 0)
	this-key)

    (set-buffer faq-content-buffer)
    (goto-char (point-min))

    (cond

     ;; IF... the index regexp is defined,
     (faq-re-index-start

      ;; THEN... just copy the pre-made index to the faq-index-buffer.
      ;;         and quickly scan the FAQ for answer-body extents.
      (re-search-forward faq-re-index-start (point-max) t 1)
      (setq faq-index-start (1+ (match-end 0)))

      (re-search-forward faq-re-index-end   (point-max) t 1)
      (setq faq-index-end (match-beginning 0))

      (copy-to-buffer faq-index-buffer faq-index-start faq-index-end)
      (goto-char faq-index-end)
      (while (re-search-forward faq-re-answer-start (point-max) t 1)
	(setq answer-body-start (match-beginning 0))
	(setq this-key (match-string 1))
	(setq answer-body-end
	      (or (and (re-search-forward faq-re-answer-end (point-max) t 1)
		       (match-beginning 0))
		  (point-max)))
	;; update the associated list with this faq body's bounds-list.
	(setq return-assoc
	      (append return-assoc
		      (list
		       (list this-key answer-body-start answer-body-end))))
	(goto-char (1- answer-body-end))))

     ;; ELSE
     (t
      ;; Crank through the FAQ, reading the question's answer bodies and
      ;; compiling our own index.  Question text and bodies are located by
      ;; the faq-re-answer-* regexps.  Unfortunately, this code will
      ;; probably run more often than the prior case.
      (let ((start-point 0))

	;; Start search after first blank line to skip any headers.
	(re-search-forward "^$")
	(while (re-search-forward faq-re-answer-start (point-max) t 1)
	  (setq answer-body-start (match-beginning 0))
	  (setq start-point (match-beginning 1))
	  (setq this-key (match-string 1))
	  (re-search-forward "^[ \t]*$") ; find pseudo-blank line
	  (copy-region-as-kill start-point (match-beginning 0))

	  (setq answer-body-end
		(or (and
		     (re-search-forward faq-re-answer-end (point-max) t 1)
		     (1- (match-beginning 0)))
		    (point-max)))
	  ;; update the associated list with this faq body's bounds-list.
	  (setq return-assoc
		(append return-assoc
			(list
			 (list this-key answer-body-start answer-body-end))))
	  (set-buffer faq-index-buffer)
	  (goto-char (point-max))
	  (yank)
	  (set-buffer faq-content-buffer)
	  (goto-char answer-body-end)))))

    (message "Indexing the FAQ...done.")
    return-assoc))

;; PRIVATE
(defun faq-update-content (KEY)
  ;; Given the question's header tag in KEY, update the content
  ;; display area with the FAQ's answer.

  ;; Look up KEY in the faq-regions assoc list, then set the narrowed
  ;; region in faq-content-buffer.
  (let ((this-faq (assoc KEY faq-regions)))
    (if (not this-faq)
	(message "faq-mode: I didn't find the answer to this question.")
      (save-excursion			; Widen the FAQ's buffer, and ...
	(display-buffer faq-content-buffer)
	(select-window faq-content-window)
	(set-buffer faq-content-buffer)
	(widen)
	(narrow-to-region (nth 1 this-faq) (nth 2 this-faq))
	(goto-char (point-min))
	(recenter 1)))))
      
;; PUBLIC
(defun forward-faq (&optional COUNT)
  "Move forward to next FAQ in the index.  With arg, repeat, or go back
if negative.  A FAQ boundary is any line whose beginning matches the
regexp 'faq-re-index-question'."
  (interactive)
  (let ((opt (point)))
    (if (not (and (goto-char (1+ (point)))
		  (re-search-forward faq-re-index-question (point-max) t 1)))
	(goto-char opt)
      (beginning-of-line)
      (recenter (/ (window-height) 2))
      (faq-update-content (match-string 0))
      (select-window faq-index-window))))
  
;; PUBLIC
(defun backward-faq (&optional COUNT)
  "Move backward to previous FAQ.  With arg, repeat, or go forward if
negative.  A FAQ boundary is any line whose beginning matches the regexp
'faq-re-index-question'."
  (interactive)
  (let ((opt (point)))
    (if (not (and (goto-char (1- (point)))
		  (re-search-backward faq-re-index-question (point-min) t 1)))
	(goto-char opt)
      (beginning-of-line)
      (recenter (/ (window-height) 2))
      (faq-update-content (match-string 0))
      (select-window faq-index-window))))

;; PUBLIC
(defun scroll-faq-up ()
  "Scroll the FAQ's body up in the window."
  (interactive)
  (select-window faq-content-window)
  ;; When end of buffer is reached, the scroll-up fails; make sure
  ;; the cursor returns to the default window.
  (unwind-protect
      (scroll-up nil)
    (select-window faq-index-window)))

;; PUBLIC
(defun quit-faq ()
  "Stop reading in FAQ mode and kill the buffer."
  (interactive)
  (kill-buffer faq-index-buffer)
  (kill-buffer faq-content-buffer))


(provide 'faq-mode)
;; faq-mode ends here
