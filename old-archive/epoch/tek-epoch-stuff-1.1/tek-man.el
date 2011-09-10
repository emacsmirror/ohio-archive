;*****************************************************************************
;
; Filename:	tek-man.el
;
; This file is not part of the GNU Emacs distribution (yet).
;
; Copyright (C) 1991 Free Software Foundation, Inc.
;
; GNU Emacs is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 1, or (at your option)
; any later version.

; GNU Emacs is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with GNU Emacs; see the file COPYING.  If not, write to
; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Author:		Originally based on code by Eric Rose
;			<erose@jessica.stanford.edu>, who started with the
;			standard distribution and modified it.
;
; This version by:	Ken Wood, <kwood@austek.oz.au>
; Organisation:		Austek Microsystems Pty Ltd, Australia.
;
; Description:	Enhanced manual browser. Invoke with "M-x man".
;
;	Numerous changes from the distribution version:
;
;	- runs "man" in background mode, so you can keep editing while your
;	man page is prepared.
;
;	- renames man page buffers using the manual topic, thus supports
;	multiple man pages open at once.
;
;	- man buffers have their own major mode with special keybindings.
;	Type ? in a man buffer for details.
;
;	- provides highlighting and mouse support under epoch. Type ? for a
;	list of the mouse bindings.
;
;	Still needs: support for "man -k".
;
;*****************************************************************************

; $Id: tek-man.el,v 1.7 1991/11/21 02:56:07 kwood Exp $

(require 'epoch-running)
(provide 'tek-man)
(provide 'manual)

(defconst manual-delete-reformatting-message t
  "*t says delete the \"Reformatting entry.  Wait...\" junk at the
beginning of the manual-entry buffer.")

; Manual Variables

(defvar manual-non-entry-regexp "[^A-Za-z0-9()_]"
  "\
Regexp matching characters that are not part of a manual entry names
such as 'emacs(1)'.")

(defvar manual-entry-name-regexp "[a-zA-Z][a-zA-Z0-9._-]+\([0-9A-Z]+\)"
  "\
Regexp which matches manual entry names, such as 'emacs(1)'.")

(defvar manual-help-buffer-name "*Help*"
  "Name of help buffer for manual-mode.")


; Put all the epoch stuff inside a test, in order to get it to compile under
; emacs.
(if running-epoch
    (progn

      (require 'tek-style-utils)
      
      ;
      ; Following section sets up some attributes for highlighting.
      ;
      
      (defvar tek-manual-seealso-foreground "purple"
	"\
Foreground color used to highlight the 'See Also' section of manual
pages if no value is defined in the X11 resources and the display
device supports color.")

      (defvar tek-manual-seealso-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-manual-seealso-foreground)
	    (tek-build-style "manual-seealso" nil nil
			     tek-manual-seealso-foreground (background)
			     (background) (foreground))
	  ; Otherwise, define the style to use a different font.
	  (tek-build-style "manual-seealso" nil (or tek-bold-fixed-font
						    tek-italic-bold-fixed-font
						    tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"\
Style or attribute used to display characters in the See Also section
of man pages.")
      
      (defvar tek-manual-usersupplied-foreground "red3"
	"\
Foreground color used to highlight the user-supplied sections of
manual pages if no value is defined in the X11 resources and the
display device supports color.")
      
      (defvar tek-manual-usersupplied-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-manual-seealso-foreground)
	    (if tek-italic-fixed-font
		; Define the style to use a non-bold italic font in a different
		; color.
		(tek-build-style "manual-usersupplied" nil
				 tek-italic-fixed-font
				 tek-manual-usersupplied-foreground
				 (background) (background) (foreground))
	      (tek-build-style "manual-usersupplied" nil nil
			       tek-manual-usersupplied-foreground
			       (background) (background) (foreground)
			       tek-manual-usersupplied-foreground))
	  (if tek-italic-bold-fixed-font
	      (tek-build-style "manual-usersupplied" nil
			       tek-italic-bold-fixed-font
			       (foreground) (background)
			       (background) (foreground))
	    (tek-build-style "manual-usersupplied" nil
			     (or tek-bold-fixed-font
				 tek-italic-fixed-font)
			     (foreground) (background)
			     (background) (foreground) (foreground))))
	  "\
Style or attribute used to display characters in user supplied sections
of man pages.")
	
      (defvar tek-manual-heading-styleorattribute
	  ; Define the style to use a different font.
	(tek-build-style "manual-heading" nil (or tek-bold-fixed-font
						  tek-italic-bold-fixed-font
						  tek-italic-fixed-font)
			 (foreground) (background)
			 (background) (foreground))
	"\
Style or attribute used to display characters in heading sections
of man pages.")

      
      ; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ; Do things the old way - using attributes.

	    (defvar tek-manual-seealso-style
	      tek-manual-seealso-styleorattribute
	      "\
Style used for displaying 'See Also' sections in man pages when attributes are
used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-manual-seealso-styleorattribute (reserve-attribute))

	    ; Bind the see also style to the see also attribute.
	    (set-attribute-style tek-manual-seealso-styleorattribute
				 tek-manual-seealso-style)
      
	    (defvar tek-manual-usersupplied-style
	      tek-manual-usersupplied-styleorattribute
	      "\
Style used for displaying user-supplied sections in man pages when
attributes are used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-manual-usersupplied-styleorattribute (reserve-attribute))
      
	    ; Bind the user supplied style to the user supplied attribute.
	    (set-attribute-style tek-manual-usersupplied-styleorattribute
				 tek-manual-usersupplied-style)
      
	    (defvar tek-manual-heading-style
	      tek-manual-heading-styleorattribute
	      "\
Style used for displaying heading sections in man pages when attributes are
used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-manual-heading-styleorattribute (reserve-attribute))

	    ; Bind the heading style to the heading attribute.
	    (set-attribute-style tek-manual-heading-styleorattribute
				 tek-manual-heading-style)
	    ))
      

      ; Function to highlight SEE ALSO entries.
      (defun manual-highlight-seealso ()
	"Highlight all 'SEE ALSO' entries of man pages under epoch"
	(save-excursion
	  (goto-char (point-min))
	  (let ((case-fold-search nil)
		(seealso-endpoint (point-max)))
	    ; Find each SEE ALSO section
	    (while (re-search-forward "^\\s-*SEE ALSO.*\n" nil t)
	      ; Assume the SEE ALSO section is terminated by one or more
	      ; blank lines - find the end of the section.
	      (save-excursion
		(if (re-search-forward "\n\n" (point-max) t)
		    (setq seealso-endpoint (match-end 0))))
	      ; Find & highlight each entry in the SEE ALSO section.
	      (while (re-search-forward manual-entry-name-regexp
					seealso-endpoint t)
		(add-button (match-beginning 0) (match-end 0)
			    tek-manual-seealso-styleorattribute nil
			    (current-buffer)))))))


      ; Mouse map to be used in manual buffers.
      (defvar manual-mouse-map (create-mouse-map mouse::global-map)
	"Mousemap for manual buffers.")

      ; Bind the mouse buttons to useful functions.
      
      (define-mouse manual-mouse-map mouse-left mouse-down
	'manual-mouse-scroll-up)
      (define-mouse manual-mouse-map mouse-middle mouse-down
	'manual-mouse-select-item)
      (define-mouse manual-mouse-map mouse-right mouse-down
	'manual-mouse-scroll-down)

      ; Functions for mouse bindings.
      
      (defun manual-mouse-select-item (mouse-data)
	"Select the manual entry specified at the mouse cursor."
	(let ((orig-window (selected-window)))
	  ;(select-window (nth 2 mouse-data))
	  (mouse::set-point mouse-data)
	  (manual-find-entry-at-point)
	  (select-window orig-window)))
      
      (defun manual-mouse-scroll-up (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (scroll-up nil)
	  (select-window orig-window)))

      (defun manual-mouse-scroll-down (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (scroll-down nil)
	  (select-window orig-window)))


      )) ; end: running-epoch test


; Manual doco

(defun manual-mode-summary ()
  "Display a brief summary of all manual mode commands"
  (interactive)
  (save-window-excursion
    ; Jump into the help buffer and prepare it for display
    (switch-to-buffer manual-help-buffer-name)
    (erase-buffer)
    (insert (documentation 'manual-mode))
    (goto-char (point-min))
    ; Now, wait until the user has scrolled to the end of the help screen
    ; and then pressed space once more.
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to manual"))
		    (if (/= ?\  (setq ch (read-char)))
			(progn (setq unread-command-char ch) nil)
		      flag))
	(scroll-up)))
    ; Bury the buffer so the user is unlikely to see it outside of
    ; manual mode.
    (bury-buffer manual-help-buffer-name)))

;
;  Manual keymap
;
(defvar manual-mode-map (make-keymap)
  "Keymap used in manual page buffers.")

(suppress-keymap manual-mode-map)
(define-key manual-mode-map " " 'scroll-up)
(define-key manual-mode-map "\177" 'scroll-down)
(define-key manual-mode-map "n" 'manual-forward-line)
(define-key manual-mode-map "p" 'manual-backward-line)
(define-key manual-mode-map "\en" 'manual-next-section)
(define-key manual-mode-map "\ep" 'manual-previous-section)
(define-key manual-mode-map "\C-n" 'manual-forward-line)
(define-key manual-mode-map "\C-p" 'manual-backward-line)
(define-key manual-mode-map "m" 'man)
(define-key manual-mode-map "a" 'manual-apropos)
(define-key manual-mode-map "g" 'manual-find-entry-at-point)
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
(define-key manual-mode-map "?" 'manual-mode-summary)
(define-key manual-mode-map "t" 'toggle-truncate-lines)
(define-key manual-mode-map "q" 'manual-quit)


; Stub for typing "man"
(defun man (word)
  (interactive "sTopic: ")
  (manual-get-entry word))

; Create an alias for the man function to maintain backward compatibility.
(fset 'manual-entry (symbol-function 'man))


; Tries to find the man page for the entry near point.
(defun manual-find-entry-at-point ()
  (interactive)
  (let ((lookup-entry (manual-find-entry-name)))
    (if lookup-entry
	(manual-get-entry lookup-entry))))

; Find another manual entry, either from the text at point or by prompting
; the user
(defun manual-find-new-entry ()
  (interactive)
  (let ((lookup-entry (manual-find-entry-name)))
    (if lookup-entry
	(manual-get-entry lookup-entry)
      (call-interactively 'man))))


; Manual mode
(defun manual-mode ()
  "\
Manual Mode is used to browse through manual pages.  Normal editing
commands are turned off, and these can be used instead:

.	Move to the top of the current man page.
SPC	Scroll down one page.
DEL	Scroll up one page.
n,C-n	Move down one line.
p,C-p	Move up one line.
M-n	Move to next section of the current page.
M-p	Move to previous section of the current page.
>	Move to end of man page.
<	Move to beginning of man page.
m	Prompt for and look up a manual entry. Format is TOPIC(SECTION)
	or TOPIC.
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
mouse-left     scroll-up
mouse-middle   man-select-button
mouse-right    scroll-down
?	This help screen.
q	Quit."
  (interactive)
  (setq major-mode 'manual-mode)
  (setq mode-name "Manual")
  (setq buffer-auto-save-file-name nil)
  (setq truncate-lines t)
  ; Use the appropriate local key & mouse bindings in this buffer.
  (use-local-map manual-mode-map)
  (if running-epoch
      (use-local-mouse-map manual-mouse-map))
  (message "Type ? for a list of commands"))


; Does the work
(defun manual-get-entry (entry)
  "Display the Unix manual entry for ENTRY.
ENTRY is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  (string-match "\\`[ \t]*\\([^( \t]+\\)[ \t]*\\((.+)\\)?[ \t]*\\'" entry)
  ; Calculate the section and topic from the arg.
  (let* (
	 (manual-topic (if (match-beginning 1)
			   (substring entry
				      (match-beginning 1) (match-end 1))))
	 (temp-match-string (if (match-beginning 2)
				(substring entry
					   (match-beginning 2) (match-end 2))))
	 (manual-section (if temp-match-string
			     (and (string-match "(\\(.+\\))"
						temp-match-string)
				  (substring temp-match-string
					     (match-beginning 1)
					     (match-end 1)))))
	)
    ; First, must be sure we have a valid topic to look up.
    (if (not manual-topic)
	; Do nothing
	nil
      (let* ((manual-buffer-name (concat "*Manual-" manual-topic "*"))
	     (manual-buffer (get-buffer manual-buffer-name)))
	(if manual-buffer
	    ; Buffer for specified manual entry already exists - just
	    ; need to display it.
	    (display-buffer manual-buffer)
	  ; Otherwise, need to look it up
	  (progn
	    ; First, create the buffer
	    (setq manual-buffer (get-buffer-create manual-buffer-name))
	    ; Do all following edits in manual buffer
	    (set-buffer manual-buffer)
	    ; Turn off undo log.
	    (buffer-flush-undo manual-buffer)
	    ; Let the user know what we're up to
	    (message "Looking for formatted entry for %s%s..."
		     manual-topic
		     (if manual-section (concat "(" manual-section ")") ""))
	    ; Next section inserts the raw "man" output into the
	    ; buffer. First, look for an already formatted file.
	    (let ((dirlist manual-formatted-dirlist)
		  (currentdir nil)
		  (completions nil)
		  (case-fold-search nil)
		  (return-value nil)
		  formatted-name)
	      ; Try the dir-prefix to start with
	      (setq return-value
		    (and manual-section
			 ; First, with the section appended to the
			 ; formatted directory.
			 (or (file-exists-p
			      (setq formatted-name
				    (concat manual-formatted-dir-prefix
					    (substring manual-section 0 1)
					    "/" manual-topic "."
					    manual-section)))
			     ; Next, without the section appended
			     (file-exists-p
			      (setq formatted-name
				    (concat manual-formatted-dir-prefix
					    manual-section "/"
					    manual-topic "."
					    manual-section))))))
	      ; If that fails, then try looking through the formatted
	      ; dirlist for the file of interest.
	      (while (and (not return-value) dirlist)
		(progn
		  ; Try the current formatted directory, with a good
		  ; guess at the formatted file name.
		  (setq currentdir (car dirlist))
		  (setq formatted-name
			(concat currentdir "/" manual-topic "."
				(or manual-section
				    (substring currentdir
					       (1+ (or (string-match
							"\\.[^./]*$"
							currentdir)
						       -2))))))
		  (setq return-value (file-exists-p formatted-name))
		  (setq dirlist (cdr dirlist))))
	      ; Otherwise, try looking for the completions in each
	      ; formatted directory.
	      (setq dirlist manual-formatted-dirlist)
	      (while (and (not return-value) dirlist)
		(progn
		  (setq currentdir (car dirlist))
		  ; Set up an error handler for file-name-all-completions
		  (condition-case ()
		      (progn
			; Get the list of completions
			(setq completions (file-name-all-completions
					   (concat manual-topic "."
						   (or manual-section ""))
					   currentdir))
			; Loop through the completions until we find a file
			; that exists.
			(while (and completions
				    (setq formatted-name
					  (concat currentdir "/"
						  (car completions)))
				    (not (setq return-value
					       (file-exists-p
						formatted-name))))
			  (setq completions (cdr completions))))
		    ; Error handler quietly does nothing.
		    (file-error nil))
		  (setq dirlist (cdr dirlist))))
	      ; Now, check that we can read the formatted file. If not,
	      ; need to delete it and run man anyway.
	      (if (and return-value
		       (not (file-readable-p formatted-name)))
		  (progn
		    (delete-file formatted-name)
		    (setq return-value nil)))
	      ; If the formatted file exists and is readable, then insert it.
	      (if return-value
		  (progn
		    (manual-insert-file formatted-name)
		    (manual-clean-and-show-buffer manual-topic))
		; Else, invoke man
		(let ((manual-process nil))
		  (message "No formatted entry, invoking man %s%s in background..."
			   (if manual-section
			       (concat manual-section " ")
			     "")
			   manual-topic)
		  (if manual-section
		      (setq manual-process
			    (start-process (concat "manbg-" manual-topic
						   manual-section)
					   manual-buffer
					   "sh" "-c"
					   (format "%s - %s %s"
						   manual-program
						   manual-section
						   manual-topic)))
		    (setq manual-process
			  (start-process (concat "manbg-" manual-topic)
					 manual-buffer
					 "sh" "-c"
					 (format "%s - %s"
						 manual-program
						 manual-topic))))
		  ; Set up a sentinel to watch for "man" exiting.
		  (set-process-sentinel manual-process 'manual-sentinel)
		  ; Bury the buffer to try & prevent the user
		  ; stumbling across it.
		  (bury-buffer manual-buffer))
		))))))))

	
(defun manual-sentinel (process msg)
  "\
Waits for the asynchronous man process to exit & cleans up the
resulting output."
  (let* ((manual-buffer (process-buffer process))
	 (manual-buffer-name  (buffer-name manual-buffer))
	 (delete-buff nil)
	 (err-mess nil)
	 (initial-match-data (match-data)))
    (if (null manual-buffer)
	; man output buffer has been deleted - flag an error and set the
	; processes buffer to nil
	(progn
	  (error "Manual output buffer deleted")
	  (set-process-buffer process nil))
      (progn
	(set-buffer manual-buffer)
	(goto-char (point-min))
	(cond
	 ; Check to see if the man page wasn't found.
	 ((or (looking-at "No \\(manual \\)*entry for")
	      (looking-at "[^\n]*: nothing appropriate$"))
	  ; Print an error message and delete the buffer.
	  (setq err-mess
		(buffer-substring (point) (progn (end-of-line) (point))))
	  (setq delete-buff t))
	 ; Check to see if the process haven't exited yet, or exited with
	 ; a non-zero exit status.
	 ((not (and (eq (process-status process) 'exit)
		    (= (process-exit-status process) 0)))
	  ; Flag an error
	  (setq err-mess (format "%s: process %s" manual-buffer-name
				 (manual-delete-trailing-newline msg)))
	  ; An insert some debug info into the buffer.
	  (goto-char (point-max))
	  (insert (format "\nprocess %s" msg)))
	 ; Otherwise, must have exited normally - just delete the reformatting
	 ; message if appropriate
	 (t (and manual-delete-reformatting-message
		 (looking-at "Reformatting ")
		 (delete-region (point)
				(progn (forward-line 1) (point))))))
	; Remove the buffer if appropriate
	(if delete-buff
	    (kill-buffer manual-buffer)
	  ; Otherwise, clean it up & display it.
	  (let ((manual-topic nil))
	    (if (string-match "\\*Manual-\\(.+\\)\\*" manual-buffer-name)
		(setq manual-topic
		      (substring manual-buffer-name
				 (match-beginning 1) (match-end 1)))
	      (setq manual-topic ""))
	    (manual-clean-and-show-buffer manual-topic)))
	; Print any accumulated message
	(if err-mess
	    (error err-mess)
	  (message ""))))
    ; Restore any previous match data on exit.
    (store-match-data initial-match-data)))
       


(defun manual-delete-trailing-newline (str)
  (if (string= (substring str (1- (length str))) "\n")
      (substring str 0 (1- (length str)))
    str))



; Hint: BS stands for more things than "back space"
(defun manual-clean-and-show-buffer (manual-topic)
  "\
Works on the current buffer. First turns underlining & overstriking by
means of backspace characters into something sensible: highlighting if
we are running epoch, plain text otherwise.

Next, remove cruddy headers & whitespace.

Finally, set the buffer mode & display it."
  (message "Cleaning manual entry for %s..." manual-topic)
  (if buffer-read-only
      (toggle-read-only))
  (if running-epoch
      ;
      ; Do highlighting
      ;
      (progn
	;
	; First, change _ chars to be "underlined" - helps simplify
	; button placement algorithm later.
	;
	(goto-char (point-min))
	(replace-regexp "\\(_\b.\\)\\([_ -]\\)\\(_\b.\\)" "\\1_\b\\2\\3")
	;
	; First nuke overstriking by same character.
	;
	(goto-char (point-min))
	; Find the start of a section of overstriking - ignore
	; underlined underscores for now.
	(while (re-search-forward "\\([^_\n]\\)\\(\b\\1\\)+" nil t)
	  ; Kill backspace character & overstrike characters
	  (replace-match "\\1")
	  (let ((button-begin (1- (point)))
		(button-end (point))
		(bolded-char nil))
	    ; Search through the rest of the overstrike characters.
	    (while (and (= (char-after (1+ (point))) ?\b)
			(= (char-after (point)) (char-after (+ 2 (point)))))
	      (progn
		; Record the character which is being overstruck
		(setq bolded-char (char-after (point)))
		; Extend the button to include the new character
		(forward-char 1)
		(setq button-end (point))
		; Delete the first overstriking
		(delete-char 2)
		; Delete all the later overstrikings by the same character
		(while (= (char-after (point)) ?\b)
		  (delete-char 2))))
	    ; Add the button around the specified region
	    (add-button button-begin button-end
			tek-manual-heading-styleorattribute nil)))
	;
	; Now, nuke underlining.
	;
	(goto-char (point-min))
	; Find the start of a section of underlining
	(while (search-forward "_\b" nil t)
	  (let ((button-begin nil)
		(button-end nil))
	    ; Kill underlining characters
	    (delete-char -2)
	    ; Start a button around just this one character.
	    (setq button-begin (point))
	    (setq button-end (1+ button-begin))
	    ; Remove the rest of the backspaces in this section & extend the
	    ; button to just after the last one.
	    (while (and (= (char-after (1+ (point))) ?_)
			(not (forward-char 3))
			(= (preceding-char) ?\b))
	      (progn
		(delete-char -2)
		(setq button-end (1+ (point)))))
	    ; Insert the button
	    (add-button button-begin button-end
			tek-manual-usersupplied-styleorattribute nil)))
	; Finally, highlight any SEE ALSO sections
	(manual-highlight-seealso)
	))
  ; Now, purge any overstruck characters. If we're running epoch,
  ; most of them will already have been taken care of. Otherwise,
  ; nuke the lot of them.
  ;
  ; Underlining first
  (goto-char (point-min))
  (replace-regexp "_\b\\(.\\)" "\\1")
  ; Overstriking next
  (goto-char (point-min))
  (replace-regexp "\\(.\\)\\(\b\\1\\)+" "\\1")
  ; General overstriking last
  (replace-regexp ".\b\\(.\\)" "\\1")
  ;
  ; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (goto-char (point-min))
  (replace-regexp "^\\s-*\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$" "")
  ; Nuke headers: "MORE(1)"
  (goto-char (point-min))
  (replace-regexp "^\\s-*[A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\s-*$" "")
  ; Nuke footers: "Printed 12/3/85	27 April 1981	1"
  ;    Sun appear to be on drugz:
  ;     "Sun Release 3.0B  Last change: 1 February 1985     1"
  ;    HP are even worse!
  ;     "     Hewlett-Packard   -1- (printed 12/31/99)"  FMHWA12ID!!
  ;    System V (well WICATs anyway):
  ;     "Page 1			  (printed 7/24/85)"
  ;    Who is administering PCP to these corporate bozos?
  (goto-char (point-min))
  (replace-regexp
   (cond ((eq system-type 'hpux)
	  "^[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*$")
	 ((eq system-type 'usg-unix-v)
	  "^ *Page [0-9]*.*(printed [0-9/]*)$")
	 (t
	  "^\\(Printed\\|Sun Release\\) [0-9].*[0-9]$"))
   "")
  ; Crunch blank lines
  (goto-char (point-min))
  (replace-regexp "\n\n\n+" "\n\n")
  ; Kill first few blank lines in the buffer
  (goto-char (point-min))
  (if (re-search-forward "\\`\n+" nil t)
      (replace-match ""))
  ; Finally, make the buffer read only & unmodified.
  (toggle-read-only)
  (set-buffer-modified-p nil)
  ; Set the buffer mode & key bindings
  (manual-mode)
  ; Show the buffer in some window
  (display-buffer (current-buffer))
  )


(defun manual-insert-file (name)
  ; Insert manual file (unpacked as necessary) into buffer
  (if (equal (substring name -2) ".Z")
      (call-process "zcat" nil t nil name)
    (if (equal (substring name -2) ".z")
	(call-process "pcat" nil t nil name)
      (insert-file-contents name))))


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


; Finds a word near point that can be a manual reference.  Returns nil
; if it can't figure it out.
(defun manual-find-entry-name ()
  "\
Returns the word near point that should correspond to a manual
reference. The variable 'manual-non-entry-regexp' controls what is not
part of a manual reference."
  (let ((entry-beginning-bound nil)
	(entry-end-bound nil)
	(entry-beginning nil)
	(entry-end nil))
    (save-excursion
      ; Now, do a bit of searching to see if there is really a manual entry
      ; near point.
      ; Don't really care if we are at the beginning or end of the buffer,
      ; just let these points limit the search.
      (if (and (if (re-search-forward manual-non-entry-regexp nil t)
		   (setq entry-end-bound (match-beginning 0))
		 (setq entry-end-bound (point-max)))
	       (or (backward-char 1) t)
	       (or (re-search-backward manual-non-entry-regexp nil t) t)
	       (re-search-forward manual-entry-name-regexp
				  entry-end-bound t))
	  ; If we made it to here, then we have found a manual entry -
	  ; so return it.
	  (buffer-substring (match-beginning 0) (match-end 0))
	; Otherwise, no entry name here so return nil.
	nil))))


(defun manual-see-also ()
  (interactive)
  (let ((opoint (point))
	(case-fold-search nil))
    (goto-char (point-min))
    (if (not (re-search-forward "^\\s-*SEE ALSO" nil t))
	(progn
	  (goto-char opoint)
	  (message "No 'SEE ALSO' section on this manpage.")))))
	
(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

(defvar manual-keep-all-buffers nil
  "If t keep the manual buffer even though the user has typed q")

(defun manual-quit ()
  (interactive)
  (if manual-keep-all-buffers
      (bury-buffer (current-buffer))
    (kill-buffer (current-buffer))))

(defun manual-apropos ()
  "\
Not yet implemented.

man -k %s | pr -h Apropos")
