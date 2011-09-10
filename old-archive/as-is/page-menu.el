;;;From: eppstein@garfield.columbia.edu (David Eppstein)
;;;Newsgroups: net.emacs
;;;Subject: page menu for GNU emacs
;;;Date: 9 Apr 86 00:28:17 GMT

;;;Some of you might be interested in the following short set of GNU emacs
;;;functions to create a menu of pages (by number and first line) in a buffer. 
;;;If you're cutting this out of news, beware of my .signature file.

;;;Mail useful suggestions to me; send flames about my programming style
;;;to /dev/null.  "If you don't like it you don't have to use it."
;;;            --------------------
; Page menu function for GNU emacs
;
; We enter a menu with one line per page, giving the number
; and first line of that page.  This menu is exited by a space,
; which returns to the buffer at the start of the given page.
;
; This menu is invoked by the function page-menu, with no arguments.
;
; Edit history:
; garfield:/usr1/eppstein/gnu/page-menu.el,  8-Apr-1986 19:03:19 by eppstein
;   Create this file.

(defvar page-menu-mode-map (make-sparse-keymap) "Keymap used in page menu.")
(define-key page-menu-mode-map " " 'page-menu-exit)

(defvar page-menu-buffer-name nil
  "Name of buffer for which this is the page menu buffer.")
(make-variable-buffer-local 'page-menu-buffer-name)

(defun page-menu-mode ()
  "Mode used within \\[page-menu].  Fundamental, with space exiting.
\\{page-menu-mode-map}"
  (interactive)				;Interactive use unlikely but possible
  (setq major-mode 'page-menu-mode)
  (setq mode-name "Page Menu")
  (setq buffer-read-only t)		;We can't change the buffer
  (use-local-map page-menu-mode-map))

(defun page-menu ()
  "Enter a menu of pages from the original buffer.
Exit with space on line corresponding to desired page."
  (interactive)
  (let ((menu-buffer (concat "*pages of " (buffer-name) "*"))
	(home-buffer (current-buffer))
	(page-number 1)
	(next-page-beginning -1)
	(old-point (point))
	(old-page 0)
	(line-text nil))
    (switch-to-buffer menu-buffer)	;Make sure following kill gets no err
    (kill-buffer menu-buffer)		;Flush any previous page menu
    (switch-to-buffer home-buffer)	;Make sure we end up where we started
    (widen)				;Use whole file
    (save-excursion			;Stay in old buf pos on abort
      (goto-char (point-min))		;Beginning of buffer
      (while (not (eobp))		;For each page until eob
	(save-excursion			;Stay at bop until later
	  (forward-page)		;Find start of next page
	  (if (and (eobp) (= page-number 1)) ;Page 1 ends at eob?
	      (error "buffer only has one page")) ;Choice of 1 is no choice
	  (setq next-page-beginning (point))) ;Remember end of page
	(re-search-forward "\\w" next-page-beginning t) ;First text line
	(beginning-of-line)		;Find start of line
	(setq line-text (buffer-substring (point) ;Grab line to end
					  (progn (end-of-line) (point))))
	(switch-to-buffer menu-buffer)	;Go to menu buffer
	(insert (int-to-string page-number) " ") ;Add page number
	(while (< (current-column) 5) (insert " ")) ;Pad
	(insert line-text "\n")		;Add text in that line
	(switch-to-buffer home-buffer)	;Back to source
	(goto-char next-page-beginning)	;Move on to next page
	(setq page-number (1+ page-number)) ;Bump page counter
	(if (<= (point) old-point)	;If old point wasn't there
	    (setq old-page (1+ old-page))))) ;Then need to bump old page too
    (pop-to-buffer menu-buffer)		;Menu buffer all made, go to it
    (setq page-menu-buffer-name (buffer-name home-buffer)) ;Remember home
    (page-menu-mode)			;This is a page menu
    (goto-char (point-min))		;Back to front
    (next-line old-page)))		;Start at line of current page

(defun page-menu-exit ()
  "Return from a page menu, selecting page of current line."
  (interactive)
  (beginning-of-line)
  (let ((page-number (string-to-int (buffer-substring (point) (point-max))))
	(home-buffer page-menu-buffer-name))
    (kill-buffer (current-buffer))	;Bye bye page menu
    (delete-other-windows)		;Only keep one window
    (switch-to-buffer home-buffer)	;Find the buffer
    (widen)				;Use whole file
    (goto-char (point-min))		;Counting pages from file start
    (forward-page (1- page-number))))	;Go to the start of the right page
;;;-- 
;;;David Eppstein, eppstein@cs.columbia.edu, seismo!columbia!cs!eppstein
;;;(note that the garfield in my headers is *not* the one in the UUCP map)
