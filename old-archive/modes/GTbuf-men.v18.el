;;;; GTbuf-men.el - more dired-like buffer menu

(defconst GTbuf-men-version (substring "$Revision: 1.25 $" 11 -2)
  "$Id: GTbuf-men.el,v 1.25 1992/12/13 22:30:45 wurgler Exp wurgler $")

;; Copyright (C) 1991 by Bill Benedetto and Tom Wurgler

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    GTbuf-men|Tom Wurgler and Bill Benedetto|
;;    wurgler@gentire.com and benedett@gentire.com|
;;    More dired-like buffer menu|
;;    1992-12-13|1.25|~/modes/GTbuf-men.el.Z|

;; INSTALLATION ======================================================
;; 
;; Put this file into your load-path and the following in your ~/.emacs:
;; 
;;   (autoload 'buffer-menu-dired-extended "GTbuf-men")
;;   (define-key ctl-x-map "\C-b" 'buffer-menu-dired-extended)

;; OVERVIEW ==========================================================
;
; extended buff-menu functions
;
;      Global keybinding:
;          \C-c\C-j - switch to buffer list
;
;      Buffer-menu mode keybindings
;          %d - mark buffers for deletion containing regexp
;          %m - mark buffers for viewing containing regexp
;          m - mark the next ARG buffers
;          M-del - unflag all buffers
;          M-< - goto the first buffer
;          M-> - goto the last buffer
;          M-{ - goto the next marked buffer
;          M-} - goto the previous marked buffer
;          F - display marked buffers
;          L - display the buffer list based on files only, direds only, plain
;              buffers (those not associated with a file) or all buffers
;          R - list only buffers containing regexp
;	   S - sort the buffer list
;	   g - to revert a buffer list
;	   q - quit buffer-menu
;	   r - rename the buffer
;	   w - copy the marked or current buffer name(s) to the kill ring
;     C-n, n - go to the next buffer line and postion on the name
;     C-p, p - go to the previous buffer line and postion on the name
;          N - display next buffer in another window
;          P - display previous buffer in another window
;          J - display this buffer in another window
;          T - switches marked and unmarked buffers
;          X - deletes marked buffers
;
; This package is based on and requires Sebastian Kremer's dired and gmhist 
; code, which can be otained via anonymous ftp from:
;
;       ftp.thp.Uni-Koeln.DE[134.95.64.1]:/pub/gnu/emacs/diredall.tar.Z
;       ftp.thp.Uni-Koeln.DE[134.95.64.1]:/pub/gnu/emacs/gmhist.tar.Z
;
; Many thanks to Sebastian for all the clean code for dired and the start
; of this package, and his helpful comments and debugging of this code.
;
; Also, thanks to Larry Dodd for acting as a beta tester and for his many
; suggestions for improving the code.
;
; Bill Benedetto and Tom Wurgler,  wurgler@gentire.com
;

(require 'dired)
(require 'gmhist)

(defvar buffer-menu-restricted nil
  "Whether buffer-menu is restricted by regexp, sort, files or direds.")

(defvar buffer-menu-full-screen nil
  "*If t, use the full screen when displaying marked buffers using 'F'.")

(defvar buffer-menu-confirm-deletes t
  "*In buffer menu, if non-nil will ask you to confirm deletions.
If nil, you will not be asked if you want to make deletes --
they will just be done.")

(defvar buffer-marker-char ?>
  "In buffer menu, character used to mark buffers for later commands.")

(defvar buffer-delete-marker ?D
  "In buffer menu, character used to mark buffers for later deletions.")

(defvar buffer-options ""
  "*Contains the last string of buffer-menu-options such as sort, regexp etc.")

(defvar buffer-menu-regexp nil
  "*Contains the last regexp used in buffer-menu.")

(defvar buffer-menu-list nil
  "*In buffer menu, contains a list of the marked buffers.")

(defvar buffer-menu-ignore-modified-regexp "^\\*"
  "Buffers matching this regexp are marked as unmodified in the
buffer menu.")

(defvar buffer-menu-strange-buf-regexp "\
^... \\*Buffer List\\*\\|\
^... \\*Messages\\*"

"These buffers must have their modified mark cleared after the listing
has been made.")

(defvar buffer-menu-ignore-modified-modes '(ange-ftp-shell-mode)
  "Buffers in this mode will show up unmodified in the buffer menu.")

(defvar buffer-menu-mode-options nil
  "Whether buffer-menu lists by regexp or files only or etc.")

(defconst buffer-menu-modified-mark-column 1)

(defun buffer-menu-dired-extended (&optional buff-opt fromp)
  "Make a menu of buffers so you can save, delete or select them.
With \\[universal-argument] you will be allowed to choose whether you want just
files, just direds or plain buffers and whether or not you want the list 
sorted.  Type ? after invocation to get help on commands available.
Type \\[Buffer-menu-quit] immediately to make the buffer menu go away."
  (interactive)
  (if (or current-prefix-arg (and buffer-menu-restricted fromp))
      (let ((buffer-menu-opts "")
	    (buffer-menu-sort-it nil))
	(if (not buff-opt)
	    (setq buffer-menu-opts
		  (read-string "Choose direds or files or plain buffers, regexp, sort (d/f/n,r,s): "))
	  (setq buffer-menu-opts buff-opt))
	(setq buffer-options buffer-menu-opts)
	(if (not (string-equal buffer-menu-opts ""))
	    (progn
	      (setq buffer-menu-restricted t)
	      (if (string-match "s" buffer-menu-opts)
		  (setq buffer-menu-sort-it t))
	      (if (string-match "r" buffer-menu-opts)
		  (if (not buff-opt)
		      (setq buffer-menu-regexp (read-with-history-in
						'buffer-menu-regexp-history
						"Regexp: ")))
		(setq buffer-menu-regexp nil))
	      (if (string-match "f" buffer-menu-opts)
		  (buffer-menu t)
		(buffer-menu nil))
	      (if (string-match "n" buffer-menu-opts)
		  (let ((buffer-read-only nil))
		    (delete-matching-lines "[0-9]+[ \t]+Dired[ \t]*")
		    (Buffer-menu-goto-first-buffer)
		    (while (not (eobp))
		      (if (buffer-file-name (Buffer-menu-buffer nil))
			  (delete-region (progn (beginning-of-line) (point))
					 (progn (forward-line 1) (point)))
			(forward-line 1)))
		    (Buffer-menu-goto-first-buffer)
		    (delete-region (point-min) (point))
		    (insert "\
 MR Buffer         Size  Mode\n\
 -- ------         ----  ----\n")
		    (Buffer-menu-goto-first-buffer)))
	      (if (string-match "d" buffer-menu-opts)
		  (let ((buffer-read-only nil) buf dir)
		    ;; Epoch 3.2 appends the filename for dired buffers
		    (keep-lines "[0-9]+[ \t]+Dired[ \t]*")	; ugh
		    (Buffer-menu-goto-first-buffer)
		    (delete-region 1 (point))
		    (insert "\
 MR Buffer         Size  Mode           Directory\n\
 -- ------         ----  ----           ---------\n")
		    (or (string-match "^3\\.2" emacs-version)
			;; Epoch 3.2 appends the filename for dired buffers
			(save-excursion
			  (while (not (eobp))
			    (if (setq buf (Buffer-menu-buffer nil))
				(progn
				  (save-excursion
				    (set-buffer buf)
				    (setq dir (or (and (boundp
							'dired-directory)
						       dired-directory)
						  ;; 18.55 Dired does not
						  ;; have this variable
						  default-directory)))
				  (end-of-line)
				  (insert dir)))
			    (forward-line))))))
	      (if buffer-menu-sort-it 
		  (let ((buffer-read-only nil))
		    (require 'sort)
		    (Buffer-menu-goto-first-buffer)
		    (sort-subr nil 'forward-line 'end-of-line
			       'Buffer-menu-buffer-no-arg)
		    (Buffer-menu-goto-first-buffer)))
	      (if buffer-menu-regexp
		  (let ((buffer-read-only nil))
		    (Buffer-menu-goto-first-buffer)
		    (buffer-mark-files-regexp buffer-menu-regexp "_")
		    (keep-lines "^_.. ")
		    (buffer-unflag-all-files "_")
		    (message "")
		    (Buffer-menu-goto-first-buffer))))
	  (buffer-menu nil)
	  (setq buffer-menu-restricted nil)))
    (setq buffer-options "")
    (setq buffer-menu-restricted nil)
    (buffer-menu nil))
  (buffer-menu-list-options)
  (make-local-variable 'buffer-menu-restricted)
  (make-local-variable 'buffer-menu-mode-options)
  (setq buffer-menu-restricted nil)
  (run-hooks 'buffer-menu-hook)
  (buffer-menu-move-to-name)
  (message "q to quit, ? for help."))



(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (if (<= (buffer-menu-cur-line) 2)
      (error "Must point at a buffer."))
  (save-excursion
    (beginning-of-line)
    (if (eobp) (forward-line -1))
    (forward-char Buffer-menu-buffer-column)
    (let ((start (point))
	  string)
      ;; End of buffer name marked by tab or two spaces.
      (re-search-forward "\t\\|  ")
      (skip-chars-backward " \t")
      (setq string (buffer-substring start (point)))
      (or (get-buffer string)
	  (if error-if-non-existent-p
	      (error "No buffer named \"%s\"" string)
	    nil)))))

(defun Buffer-menu-buffer-no-arg ()
  "Return buffer described by this line of buffer menu.
A version of the original except this one doesn't require an arg."
  (if (<= (buffer-menu-cur-line) 2)
      (error "Must point at a buffer."))
  (save-excursion
    (beginning-of-line)
    (if (eobp) (forward-line -1))
    (forward-char Buffer-menu-buffer-column)
    (let ((start (point))
	  string)
      ;; End of buffer name marked by tab or two spaces.
      (re-search-forward "\t\\|  ")
      (skip-chars-backward " \t")
      (buffer-substring start (point)))))


;; Make the `modified' marker in buffer menu more meaningful by
;; putting buffer-menu-set-some-buffers-unmodified on buffer-menu-hook.

(defun dired-pending-marks-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward dired-re-mark nil t)))
      
(defun dired-set-buffer-modified-p ()
  "Mark all Dired buffers as modified iff there are pending marks."
  (interactive)
  (let ((blist (buffer-list)))
    (while blist
      (save-excursion
	(set-buffer (car blist))
	(setq blist (cdr blist))
	(if (eq major-mode 'dired-mode)
	    (set-buffer-modified-p (dired-pending-marks-p)))))))

(defun set-some-buffers-unmodified (name-regexp major-modes)
  "Clears the modification flag of buffers whose names match NAME-REGEXP
or whose major mode is a member of MAJOR-MODES.  Either or both of the
arguments may be nil.
Also sets dired buffer modification flags according to dired-pending-marks-p."
  (let ((blist (buffer-list)))
    (while blist
      (save-excursion
	(set-buffer (car blist))
	(setq blist (cdr blist))
	(cond ((eq major-mode 'dired-mode)
	       (set-buffer-modified-p (dired-pending-marks-p)))
	      ((buffer-modified-p)
	       ;; Don't do the work unless the buffer is marked modified.
	       (if (or (memq major-mode major-modes)
		       (and name-regexp
			    (string-match name-regexp (buffer-name))))
		   (set-buffer-modified-p nil))))))))

(defun buffer-menu-set-some-buffers-unmodified ()
  "Useful on `buffer-menu-hook' to make the modified marker in the
buffer menu more meaningful."
  (set-some-buffers-unmodified buffer-menu-ignore-modified-regexp
			       buffer-menu-ignore-modified-modes)
  (save-excursion
    (buffer-menu-home-to-tilde)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      ;; These buffers are modified during the listing, so
      ;; set-buffer-modified-p is too late.
      (while (re-search-forward buffer-menu-strange-buf-regexp nil t)
	(move-to-column buffer-menu-modified-mark-column)
	(delete-char 1)
	(insert " ")))))

;; Move to next and previous marked buffer line.

(defun buffer-menu-next-marked-buffer (arg &optional wrap opoint string)
  "Move to the next marked buffer, wrapping around the end of the buffer list."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (or string (setq string (concat "\n" (char-to-string buffer-marker-char))))
  (if (if (> arg 0)
	  (search-forward string nil t arg)
	(beginning-of-line)
	(if (search-backward string nil t (- arg))
	    (search-forward string)))
      (buffer-menu-move-to-name);; or the re-search will get stuck
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked buffer!"))
      (message "(Wraparound for next marked buffer)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (buffer-menu-next-marked-buffer arg nil opoint))))

(defun buffer-menu-prev-marked-buffer (arg &optional wrap)
  "Move to the previous marked buffer, wrapping around the end of the
buffer list."
  (interactive "p\np")
  (buffer-menu-next-marked-buffer (- arg) wrap))

(defconst buffer-menu-name-column 4)

(defun buffer-menu-move-to-name ()
  (move-to-column buffer-menu-name-column))

(defun buffer-mark-files-regexp (regexp &optional marker-char)
  "Mark all buffers matching REGEXP for use in later commands.
A prefix argument means to unmark them instead."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " buffers (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char buffer-marker-char)))
    (save-excursion
      (Buffer-menu-goto-first-buffer)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(dired-mark-if
	 (and (not (eolp))		; empty line
	      (let ((fn (buffer-name (Buffer-menu-buffer t))))
		(and fn (string-match regexp fn))))
	 "matching buffer")))))

(defun buffer-flag-regexp-files (regexp)
  "In buffer-menu, flag all buffers containing the specified REGEXP for deletion.
Use `^' and `$' to anchor matches."
  (interactive (list (dired-read-regexp "Flag for deletion (regexp): ")))
  (buffer-mark-files-regexp regexp buffer-delete-marker))

(defun buffer-unflag-all-files (flag &optional arg)
  "Remove a specific or all flags from every buffer line.
With an arg, queries for each marked buffer.
Type \\[help-command] at that time for help."
  (interactive "sRemove flag: (default: all flags) \nP")
  (let ((count 0)
	(re (if (zerop (length flag)) dired-re-mark
	      (concat "^" (regexp-quote flag)))))
    (save-excursion
      (let (buffer-read-only case-fold-search query
			     (help-form "\
Type SPC or `y' to unflag one buffer, DEL or `n' to skip to next,
`!' to unflag all remaining buffers with no more questions."))
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (if (or (not arg)
		  (dired-query 'query "Unflag buffer `%s' ? "
			       (Buffer-menu-buffer t)))
	      (progn (delete-char -1) (insert " ") (setq count (1+ count))))
	  (forward-line 1))))
    (message "%s" (format "Flags removed: %d %s" count flag) )))

;; redefines buff-menu.el's version to make a pop-up for deletions
;; like Dired does

(defun Buffer-menu-execute (&optional marked-buffers-instead)
  "Save and/or delete buffers marked with \\[Buffer-menu-save] or \\[Buffer-menu-delete] commands."
  (interactive)
  (if (not marked-buffers-instead)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^.S" nil t)
	  (let ((modp nil))
	    (save-excursion
	      (set-buffer (Buffer-menu-buffer t))
	      (save-buffer)
	      (setq modp (buffer-modified-p)))
	    (let ((buffer-read-only nil))
	      (delete-char -1)
	      (insert (if modp ?* ? )))))))
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-menu-buffer (current-buffer))
	  (dired-marker-char buffer-delete-marker)
	  (buffer-menu-do-deletes nil)
	  (buffer-read-only nil))
      (buffer-menu-list (if marked-buffers-instead
			    (char-to-string buffer-marker-char)
			  (char-to-string buffer-delete-marker)))
      (if buffer-menu-confirm-deletes
	  (if (dired-mark-pop-up
	       " *Deletions*" 'deletions buffer-menu-list
	       dired-deletion-confirmer
	       (format "Delete %s " (buffer-mark-prompt nil buffer-menu-list)))
	      (setq buffer-menu-do-deletes t)
	    (setq buffer-menu-do-deletes nil))
	(setq buffer-menu-do-deletes t))
      (if buffer-menu-do-deletes
	  (while (search-forward
		  (if marked-buffers-instead
		      (concat "\n" (char-to-string buffer-marker-char))
		    (concat "\n" (char-to-string buffer-delete-marker))) nil t)
	    (forward-char -1)
	    (let ((buf (Buffer-menu-buffer nil)))
	      (or (eq buf nil)
		  (eq buf buffer-menu-buffer)
		  (save-excursion (kill-buffer buf))))
	    (if (Buffer-menu-buffer nil)
		(progn (delete-char 1)
		       (insert ? ))
	      (delete-region (point) (progn (forward-line 1) (point)))
	      (forward-char -1)))))))

(defun buffer-mark-prompt (arg files)
  ;; Return a string for use in a prompt, either the current file
  ;; name, or the marker and a count of marked files.
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d files]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d buffers]" arg)
	(format "%c [%d buffers]" dired-marker-char count)))))

(defun Buffer-menu-select (&optional arg)
  "Visit all marked buffers at once, and display them simultaneously.
Visit just the buffer the cursor is on if no buffers are marked.
With an arg, visit arg next buffers."
  (interactive "P")
  (simultaneous-find-buffer (buffer-menu-list nil arg)))

(defun simultaneous-find-buffer (file-list)
  "Visit all buffers in BUFFER-LIST and display them simultaneously.

The current window is split across all buffers in BUFFER-LIST, as evenly
as possible.  Remaining lines go to the bottommost window.
If variable buffer-menu-full-screen is t, then use the whole screen.
The number of buffers that can be displayed this way is restricted by
the height of the current window and the variable `window-min-height'."
  (let ((size (/ (if buffer-menu-full-screen (1- (screen-height))
		   (window-height)) (length file-list))))
    (or (<= window-min-height size)
	(error "Too many buffers to visit simultaneously"))
    (if buffer-menu-full-screen (delete-other-windows))
    (switch-to-buffer (car file-list))
    (setq file-list (cdr file-list))
    (while file-list
      ;; Split off vertically a window of the desired size
      ;; The upper window will have SIZE lines.  We select the lower
      ;; (larger) window because we want to split that again.
      (select-window (split-window nil size))
      (switch-to-buffer (car file-list))
      (setq file-list (cdr file-list)))))

(defun Buffer-menu-quit ()
  "Bury the current buffer-menu."
  (interactive)
  (bury-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun Buffer-menu-goto-first-buffer ()
  (goto-line 3))


(defun buffer-copy-buffername-as-kill (&optional arg)
  "Copy names of marked buffers into the kill ring.
The names are separated by a space.
With a prefix arg, use just current buffer.
You can then feed the file name to other commands with \\[yank]."
  (interactive "P")
  (copy-string-as-kill
   (if arg
       (buffer-name (Buffer-menu-buffer t))
     (mapconcat (function identity) (buffer-menu-list) " ")))
  (message "%s" (car kill-ring)))

(defun buffer-menu-list (&optional marker-char arg)
  (let ((marker (or marker-char (char-to-string buffer-marker-char)))
	(there-are-some))
    (if arg
	(progn
	  (setq buffer-menu-list nil)
	  (while (> arg 0)
	    (setq buffer-menu-list (cons (buffer-name (Buffer-menu-buffer t))
					 buffer-menu-list))
	    (next-line 1)
	    (setq arg (1- arg)))
	  (setq buffer-menu-list (nreverse buffer-menu-list)))
      (save-excursion
	(goto-char (point-min))
	(if (search-forward (concat "\n" marker) nil t)
	    (setq there-are-some t)
	  (error "No buffers marked.")))
      (if there-are-some
	  (save-excursion
	    (setq buffer-menu-list nil)
	    (goto-char (point-min))
	    (while (search-forward (concat "\n" marker) nil t)
	      (setq buffer-menu-list
		    (cons (buffer-name (Buffer-menu-buffer t))
			  buffer-menu-list)))
	    (setq buffer-menu-list (nreverse buffer-menu-list)))
	(setq buffer-menu-list (cons (buffer-name (Buffer-menu-buffer t))
				   nil))))))

(defun Buffer-menu-revert ()
  (interactive)
  (let ((opoint (point))
	(buf (Buffer-menu-buffer-no-arg))
	(mark-alist nil)
	case-fold-search
	buffer-read-only)
    (goto-char (point-min))
    (setq mark-alist (buffer-remember-marks (point-min) (point-max)))
    (setq buffer-menu-restricted t)
    (buffer-menu-dired-extended buffer-options t)
    (buffer-mark-remembered mark-alist)
    (run-hooks 'buffer-menu-hook)
    (buffer-goto-buffer buf opoint)
    (beginning-of-line)
    (buffer-menu-move-to-name)))

(defun buffer-remember-marks (beg end)
  "Return alist of buffers and their marks, from BEG to END."
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (buffer-name (Buffer-menu-buffer nil)))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

(defun buffer-mark-remembered (alist)
  ;; Mark all files remembered in ALIST.
  (let (elt fil chr)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    fil (car elt)
	    chr (cdr elt))
      (if (buffer-goto-buffer fil)
	  (save-excursion
	    (beginning-of-line)
	    (setq buffer-read-only nil)
	    (delete-char 1)
	    (insert chr)
	    (setq buffer-read-only t))))))

(defun buffer-goto-buffer (buf &optional pos)
  (interactive)
  (let ((beg (or pos (point))))
    (goto-char (point-min))
    (if (re-search-forward (concat "\n...." buf) nil t)
	t
      (goto-char beg)
      nil)))

(defun buffer-menu-home-to-tilde ()
  "Function to convert the \"home\" to \"~\"."
  (let* ((home (regexp-quote (expand-file-name "~/")))
	 (hleng (length home))
	 (buffer-read-only nil))
    ;; Unexpand home directory:
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward home nil t)
	(if (not (= (- (current-column) hleng) 4))
	    (replace-match "~/" t t))))))

(defun buffer-menu-toggle-sort ()
  "Toggles between sorted and unsorted buffer-menu.  Maintains all other
buffer-menu options."  
  (interactive)
  (let ((opoint (point))
	(buf (Buffer-menu-buffer-no-arg))
	(mark-alist (buffer-remember-marks (point-min) (point-max))))
    (if (string-match "s" buffer-options)
	(setq buffer-options (buffer-menu-translate buffer-options "s" ""))
      (setq buffer-options (concat buffer-options "s")))
    (setq buffer-menu-restricted t)
    (buffer-menu-dired-extended buffer-options t)
    (buffer-mark-remembered mark-alist)
    (buffer-goto-buffer buf)
    (beginning-of-line)))

(defun buffer-menu-list-regexp ()
  "Lists only buffer conatining the specified regexp.  Maintains all other
buffer-menu options."
  (interactive)
  (setq buffer-menu-regexp (read-string "Regexp: "))
  (if (string-equal buffer-menu-regexp "")
      (setq buffer-options (buffer-menu-translate buffer-options "r" ""))
    (if (not (string-match "r" buffer-options))
	(setq buffer-options (concat buffer-options "r")))
    (setq buffer-menu-restricted t))
  (buffer-menu-dired-extended buffer-options t))

(defun buffer-menu-toggle-direds-or-files ()
  "Cycles listing buffers by direds, files, plain buffers and all buffers."
  (interactive)
  (if (string-match "d" buffer-options)
      (progn
	(setq buffer-options (buffer-menu-translate buffer-options "f" ""))
	(setq buffer-options (buffer-menu-translate buffer-options "n" ""))
	(setq buffer-options (buffer-menu-translate buffer-options "d" "f")))
    (if (string-match "f" buffer-options)
	(progn
	  (setq buffer-options (buffer-menu-translate buffer-options "d" ""))
	  (setq buffer-options (buffer-menu-translate buffer-options "n" ""))
	  (setq buffer-options (buffer-menu-translate buffer-options "f" "n")))
      (if (string-match "n" buffer-options)
	  (progn
	    (setq buffer-options (buffer-menu-translate buffer-options "d" ""))
	    (setq buffer-options (buffer-menu-translate buffer-options "f" ""))
	    (setq buffer-options (buffer-menu-translate buffer-options "n" "")))
	(setq buffer-options (concat buffer-options "d")))))
  (setq buffer-menu-restricted t)
  (buffer-menu-dired-extended buffer-options t))

(or (equal (assq 'buffer-menu-mode-options minor-mode-alist)
	   '(buffer-menu-mode-options buffer-menu-mode-options))
    ;; Test whether this has already been done in case dired is reloaded
    ;; There may be several elements with buffer-menu-mode-options as car.
    (setq minor-mode-alist
	  (cons '(buffer-menu-mode-options buffer-menu-mode-options)
		;; buffer-menu-mode-options is nil outside dired
		minor-mode-alist)))

(defun buffer-menu-list-options ()
  (setq buffer-menu-mode-options
	(concat "" 
		(if (string-match "r" buffer-options)
		    " regexp")
		(if (string-match "s" buffer-options)
		    " sorted")
		(if (string-match "d" buffer-options)
		    " direds only")
		(if (string-match "f" buffer-options)
		    " files only")
		(if (string-match "n" buffer-options)
		    " plain buffers only")))
  ;; update mode line:
  (set-buffer-modified-p (buffer-modified-p)))

(defun buffer-menu-translate (string1 string2 string3)
  "Change every occurence in STRING of FSTRING with RSTRING."
  (let ((case-fold-search nil))
    (while (string-match string2 string1)
      (if (not (string-equal string3 ""))
	  (aset string1
		(match-beginning 0) (string-to-char string3))
	(setq string1 (concat
		       (substring string1 0 (match-beginning 0))
		       (substring string1 (match-end 0)))))))
  string1)

(defun beginning-of-buffer-menu ()
  "Go to first buffer."
  (interactive)
  (Buffer-menu-goto-first-buffer))

(defun end-of-buffer-menu ()
  "Go to last buffer."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (buffer-menu-move-to-name))

(defun buffer-menu-rename ()
  "Rename the current buffer and optionally the associated file."
  (interactive)
  (let ((buffer (Buffer-menu-buffer nil))
	new-buffer)
    (setq new-buffer (read-buffer
		      (concat "Rename " (buffer-name buffer) " to: ")))
    (if (get-buffer new-buffer)
	(error "Buffer %s already exists" new-buffer)
      (save-excursion 
	(set-buffer buffer)
	(rename-buffer new-buffer))
      (Buffer-menu-revert)
      ;; I don't think it should do anything with filenames, this
      ;; should be done in dired.
      ;; The code below is not tested much!
      (if (and (buffer-file-name buffer)
	       (y-or-n-p "Change the filename too? "))
	  (let ((new-file (concat (file-name-directory 
				   (buffer-file-name buffer)) new-buffer)))
	    (if (file-exists-p new-file)
		(error "File already exists.")
	      (save-excursion
		(set-buffer buffer)
		(set-visited-file-name new-file))
	      (message ""))))
      )))

(defun buffer-menu-next-line (arg)
  "Move down lines then position at buffer name.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
	(forward-line 1)
	(if (eobp)
	    (goto-line 3)
	  (if (< (buffer-menu-cur-line) 3)
	      (end-of-buffer-menu)))
	(setq arg (1- arg)))
    (while (< arg 0)
      (forward-line -1)
      (if (eobp)
	  (goto-line 3)
	(if (< (buffer-menu-cur-line) 3)
	    (end-of-buffer-menu)))
      (setq arg (1+ arg))))
  (buffer-menu-move-to-name))

(defun buffer-menu-previous-line (arg)
  "Move up lines then position at buffer name.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (buffer-menu-next-line (- arg)))

(defun buffer-jump-back ()
  "Jump back to the buffer menu (*Buffer List*) if it exists, if not
do a buffer-menu."
  (interactive)
  (if (get-buffer "*Buffer List*")
      (switch-to-buffer "*Buffer List*")
    (buffer-menu-dired-extended)))

(defun Buffer-menu-mark (arg)
  "Mark next ARG buffers.  Default arg is 1."
  (interactive "p")
  (if (> arg 0) 
      (while (> arg 0)
	(beginning-of-line)
	(if (looking-at " [-M]")
	    (ding)
	  (let ((buffer-read-only nil))
	    (if (not (get-buffer (Buffer-menu-buffer-no-arg)))
		(progn
		  (buffer-menu-move-to-name)
		  (error (concat
			  "Buffer '" (Buffer-menu-buffer-no-arg)
			  "' no longer exists."))))
	    (delete-char 1)
	    (insert buffer-marker-char)
	    (buffer-menu-next-line 1)))
	(setq arg (1- arg)))
    (while (< arg 0)
      (beginning-of-line)
      (if (looking-at " [-M]")
	  (ding)
	(let ((buffer-read-only nil))
	  (delete-char 1)
	  (insert buffer-marker-char)
	  (buffer-menu-next-line -1)))
      (setq arg (1+ arg)))))

(defun Buffer-menu-unmark ()
  "Cancel all requested operations on buffer on this line.  Same as original
except this version positions point on the buffername."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
  (buffer-menu-next-line 1))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above.
Same as original except this version positions point on the buffername."
  (interactive)
  (forward-line -1)
  (Buffer-menu-unmark)
  (forward-line -1)
  (buffer-menu-move-to-name))

(defun Buffer-menu-delete (arg)
  "In buffer-menu, flag the current line's buffer for deletion."
  (interactive "p")
  (let ((buffer-marker-char buffer-delete-marker))
    (Buffer-menu-mark arg)))

(defun Buffer-menu-display-next-buffer (arg)
  "Move down ARG lines and display the buffer in another window."
  (interactive "p")
  (buffer-menu-next-line arg)
  (Buffer-menu-display-this-buffer))

(defun Buffer-menu-display-prev-buffer (arg)
  "Move up ARG lines and display the buffer in another window."
  (interactive "p")
  (Buffer-menu-display-next-buffer (- arg)))

(defun Buffer-menu-display-this-buffer ()
  "Display this buffer in another window, keeping cursor in *Buffer list*."
  (interactive)
  (let ((name-buffer (Buffer-menu-buffer-no-arg)))
    (if (get-buffer name-buffer)
	(pop-to-buffer name-buffer)
      (if (y-or-n-p (concat "Buffer '" name-buffer
			    "' no longer exists, create it? "))
	  (pop-to-buffer name-buffer)
	(message "Reverting buffer list...")
	(sit-for 1)
	(Buffer-menu-revert))))
    (pop-to-buffer "*Buffer List*"))
  
(defun buffer-menu-cur-line ()
   "Function to return the current line number.    GT"
   (save-excursion
      (beginning-of-line)
      (setq buffer-menu-cur-line (1+ (count-lines 1 (point))))))

(defun buffer-menu-do-toggle ()
  "Toggle marks.
That is, currently marked buffers become unmarked and vice versa.
Buffers marked with other flags (such as `D') are not affected."
  (interactive)
  (save-excursion
    (goto-line 3)
    (beginning-of-line)
    (let (buffer-read-only)
      (while (not (eobp))
	(apply 'subst-char-in-region
	       (point) (1+ (point))
	       (if (eq ?\040 (following-char)) ; SPC
		   (list ?\040 buffer-marker-char)
		 (list buffer-marker-char ?\040)))
	(forward-line 1)))))

(defun buffer-do-delete ()
  "Deletes the buffers marked with the buffer-mark-char.  Interactive if
buffer-menu-confirm-deletes is t."
  (interactive)
  (Buffer-menu-execute t))

(define-key global-map "\C-c\C-j" 'buffer-jump-back)

(fset 'buffer-regexp-prefix (make-sparse-keymap))
(define-key Buffer-menu-mode-map "r" 'buffer-menu-rename)
(define-key Buffer-menu-mode-map "\e<" 'beginning-of-buffer-menu)
(define-key Buffer-menu-mode-map "\e>" 'end-of-buffer-menu)
(define-key Buffer-menu-mode-map "\e{" 'buffer-menu-prev-marked-buffer)
(define-key Buffer-menu-mode-map "\e}" 'buffer-menu-next-marked-buffer)
(define-key Buffer-menu-mode-map "%" 'buffer-regexp-prefix)
(define-key Buffer-menu-mode-map "%d" 'buffer-flag-regexp-files)
(define-key Buffer-menu-mode-map "%m" 'buffer-mark-files-regexp)
(define-key Buffer-menu-mode-map "\M-\C-?" 'buffer-unflag-all-files)
(define-key Buffer-menu-mode-map "F" 'Buffer-menu-select)
(define-key Buffer-menu-mode-map "q" 'Buffer-menu-quit)
(define-key Buffer-menu-mode-map "g" 'Buffer-menu-revert)
(define-key Buffer-menu-mode-map "w" 'buffer-copy-buffername-as-kill)
(define-key Buffer-menu-mode-map "R" 'buffer-menu-list-regexp)
(define-key Buffer-menu-mode-map "S" 'buffer-menu-toggle-sort)
(define-key Buffer-menu-mode-map "L" 'buffer-menu-toggle-direds-or-files)
(define-key Buffer-menu-mode-map "n" 'buffer-menu-next-line)
(define-key Buffer-menu-mode-map "p" 'buffer-menu-previous-line)
(define-key Buffer-menu-mode-map " " 'buffer-menu-next-line)
(define-key Buffer-menu-mode-map "\C-n" 'buffer-menu-next-line)
(define-key Buffer-menu-mode-map "\C-p" 'buffer-menu-previous-line)
(define-key Buffer-menu-mode-map "N" 'Buffer-menu-display-next-buffer)
(define-key Buffer-menu-mode-map "P" 'Buffer-menu-display-prev-buffer)
(define-key Buffer-menu-mode-map "J" 'Buffer-menu-display-this-buffer)
(define-key Buffer-menu-mode-map "T" 'buffer-menu-do-toggle)
(define-key Buffer-menu-mode-map "X" 'buffer-do-delete)
