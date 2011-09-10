;;; This is a replacement for view-mode
;;; that looks a lot like less.  It also acts like a minor mode, and
;;; doesn't rebind any keys that it doesn't have to.  

;; Written by David Gudeman (gudeman@arizona.edu)
;; Gnu Emacs v18 only.

;; Mods by Bengt Martensson, to closely resemble less
;; LastEditDate "Thu Jul 23 13:23:24 1987"

;; July 87, Gudeman again: added prefix for "q",

;; to make auto-view-mode work automatically when you read a
;; write-protected file, add the following to your .emacs file.
;;(or (member 'auto-view-mode find-file-hooks)
;;    (setq find-file-hooks (cons 'auto-view-mode find-file-hooks)))

(provide 'view)

(defvar view-search-string ""
  "Last string searched for with view-search functions.")

(defvar view-search-arg 1
  "Arg to last view search.")

(defvar view-previous-values nil
  "Values of buffer variables before view-mode was called.  It's a list
of local-keymap and mode-line-buffer-identification in that order.")

(defvar view-default-lines 10		; BM
  "Default value for the ""d"" and ""u"" commands in view-mode")

(defvar view-mode-map nil)		; Keybinding changed, BM
(if view-mode-map nil
  (setq view-mode-map (make-keymap))
  (let ((i ?0))
    (while (<= i ?9)
      (define-key view-mode-map (char-to-string i) 'digit-argument)
      (setq i (1+ i))))
  (define-key view-mode-map "-" 'negative-argument)
  (define-key view-mode-map " " 'scroll-up)
  (define-key view-mode-map "f" 'scroll-up)
  (define-key view-mode-map "\C-?" 'scroll-down)
  (define-key view-mode-map "b" 'scroll-down)
  (define-key view-mode-map "\C-m" 'scroll-lines-up)
  (define-key view-mode-map "e" 'scroll-lines-up)
  (define-key view-mode-map "j" 'scroll-lines-up)
  (define-key view-mode-map "y" 'scroll-lines-down)
  (define-key view-mode-map "k" 'scroll-lines-down)
  (define-key view-mode-map "d" 'scroll-some-lines-up)
  (define-key view-mode-map "u" 'scroll-some-lines-down)
  (define-key view-mode-map "r" 'recenter)
  (define-key view-mode-map "t" 'toggle-truncate-lines)
  (define-key view-mode-map "v" 'edit-view-buffer)
  (define-key view-mode-map "N" 'view-buffer)
  (define-key view-mode-map "E" 'view-file)
  (define-key view-mode-map "P" 'view-buffer)
  (define-key view-mode-map "!" 'shell-command)
  (define-key view-mode-map "|" 'shell-command-on-region)
  (define-key view-mode-map "=" 'what-line)
  (define-key view-mode-map "?" 'view-search-backward)
  (define-key view-mode-map "h" 'describe-view-mode)
  (define-key view-mode-map "s" 'view-repeat-search)
  (define-key view-mode-map "n" 'view-repeat-search)
  (define-key view-mode-map "/" 'view-search-forward)
  (define-key view-mode-map "\\" 'view-search-backward)
  (define-key view-mode-map "g" 'view-goto-line)
  (define-key view-mode-map "G" 'view-Goto-line)
  (define-key view-mode-map "%" 'view-goto-percent)
  (define-key view-mode-map "p" 'view-goto-percent)
  (define-key view-mode-map "m" 'point-to-register)
  (define-key view-mode-map "'" 'register-to-point)
  (define-key view-mode-map "C" 'view-cleanup-backspaces)
  (define-key view-mode-map "q" 'view-quit))

(defun view-file (file &optional p)
  "Find FILE, enter view mode.  With prefix arg use other window."
  (interactive "fView File: \nP")
  (if p (find-file-other-window file)
    (find-file file))
  (view-mode))

(defun view-buffer (buf &optional p)
  "Switch to BUF, enter view mode.  With prefix arg use other window."
  (interactive "bView Buffer: \nP")
  (if p (switch-to-buffer-other-window buf)
    (switch-to-buffer buf))
  (view-mode))

(defun view-mode (&optional p)
  "Mode for viewing text.  Only the local keybindings, and buffer-read-only
are changed by view-mode.  These changes can be undone by the e command.
Commands are:
\\<view-mode-map>
0..9	prefix args
-	prefix minus
\\[scroll-up]	scroll-up
\\[scroll-down]	scroll-down
\\[scroll-lines-up]	scroll prefix-arg lines forward, default 1.
\\[scroll-some-lines-down]	scroll prefix-arg lines backward, default 10.
\\[scroll-some-lines-up]	scroll prefix-arg lines forward, default 10.
\\[what-line]	print line number
\\[describe-view-mode]	print this help message
\\[view-search-forward]	regexp search, uses previous string if you just hit RET
\\[view-search-backward]	as above but searches backward
\\[view-repeat-search]	repeat last search
\\[toggle-truncate-lines]	toggle truncate-lines
\\[view-file]	view-file
\\[view-buffer]	view-buffer
\\[view-cleanup-backspaces]	cleanup backspace constructions
\\[edit-view-buffer]	switch back to editing mode for buffer
\\[view-quit]	bury the current buffer and switch to a new one, with a prefix
	kill the current buffer.

If invoked with the optional (prefix) arg non-nil, view-mode cleans up
backspace constructions."

  (interactive "P")
  (make-local-variable 'view-previous-values)
  (make-local-variable 'view-default-lines)
  (or view-previous-values
      (setq view-previous-values
	    (list (current-local-map)
		  (assoc 'mode-line-buffer-identification
			 (buffer-local-variables)))))
  (use-local-map view-mode-map)
  (if p (cleanup-backspaces))
  (setq mode-line-buffer-identification (list "View: %17b"))
  (setq buffer-read-only t))

(defun cleanup-backspaces ()
  "Cleanup backspace constructions.
_^H and ^H_ sequences are deleted.  x^Hx sequences are turned into x for all
characters x.  ^^H| and |^H^ sequences are turned into ^.  +^Ho and o^H+ are
turned into (+)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (= (following-char) ?\C-h)
      (delete-char 1))
    (while (search-forward "\C-h" nil t)
      (forward-char -2)
      (cond ((looking-at "_\C-h\\|\\(.\\)\C-h\\1\\||\C-h\\^")
	     (delete-char 2))
	    ((looking-at ".\C-h_\\|\\^\C-h|")
	     (forward-char 1)
	     (delete-char 2))
	    ((looking-at "+\C-ho\\|o\C-h+")
	     (delete-char 3)
	     (insert "(+)"))
	    ((looking-at "|\C-h-")
	     (delete-char 3)
	     (insert "*"))
	    (t (forward-char 2))))))

(defun toggle-truncate-lines (&optional p)		; BM
  "Toggles the values of truncate-lines."
  (interactive "P")
  (setq truncate-lines
	(cond ((null p) (not truncate-lines))
	      ((= 0 (prefix-numeric-value p)) nil)
	      (t t)))
  (recenter))

(defun view-cleanup-backspaces ()
  "Cleanup backspaces and if buffer is currently unmodified, don't flag it
as a modified buffer.  This works even if the buffer is read-only."
  (interactive)
  (let ((buffer-read-only)(buf-mod (buffer-modified-p)))
    (cleanup-backspaces)
    (set-buffer-modified-p buf-mod)))

(defun scroll-lines-up (p)
  "Scroll up prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up p))

(defun scroll-lines-down (p)
  "Scroll down prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up (- p)))

(defun scroll-some-lines-down (&optional N) ; BM
  "Scroll down prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> N 1) (setq view-default-lines N))
  (scroll-down view-default-lines))

(defun scroll-some-lines-up (&optional N) ; BM
  "Scroll up prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> N 1) (setq view-default-lines N))
  (scroll-up view-default-lines))

(defun view-goto-line (&optional N)	; BM
  "Goto line prefix, default 1."
  (interactive "p")
  (goto-line N))

(defun view-Goto-line (&optional N)	; BM
  "Goto line prefix, default last line."
  (interactive "p")
  (if current-prefix-arg (goto-line N)
    (end-of-buffer)
    (recenter -1)
    (move-to-window-line 0)))

(defun view-goto-percent (&optional p)	; BM
  "Sets mark and goes to a position PERCENT percent of the file."
  (interactive "p")
  (set-mark-command nil)
  (goto-char (+ (point-min) (/ (* p (- (point-max) (point-min))) 100)))
  (beginning-of-line))

(defun edit-view-buffer ()
  "Return to buffer's previous mode, and make buffer modifiable."
  (interactive)
  (let ((map (nth 0 view-previous-values))
	(buf-id (nth 1 view-previous-values))
	(buf-mod (buffer-modified-p)))
    (use-local-map map)
    (if buf-id (setq mode-line-buffer-identification buf-id)
      (kill-local-variable 'mode-line-buffer-identification))
    (setq buffer-read-only nil)
    (kill-local-variable 'view-previous-values)
    (set-buffer-modified-p buf-mod)))	; hack to update the mode line

(defun describe-view-mode ()
  (interactive)
  (let ((mode-name "View")
	(major-mode 'view-mode))
    (describe-mode)))

(defun view-search-forward (s p)
  "Search forward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search: \np")
  (unwind-protect
      (word-search-forward
       (if (string= "" s) view-search-string s) nil nil p)
    (setq view-search-arg p)
    (or (string= "" s)
	(setq view-search-string s))))

(defun view-search-backward (s p)
  "Search backward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search backward: \np")
  (view-search-forward s (- p)))

(defun view-repeat-search (p)
  "Repeat last view search command.  If a prefix arg is given, use that
instead of the previous arg, if the prefix is just a -, then take the
negative of the last prefix arg."
  (interactive "P")
  (view-search-forward
   view-search-string
   (cond ((null p) view-search-arg)
	 ((eq p '-) (- view-search-arg))
	 (t (prefix-numeric-value p)))))

(defun view-quit (&optional p)
  "Switch to another buffer and bury this one.  With a prefix arg, kill the
current buffer."
  (interactive "P")
  (if p (kill-buffer (current-buffer))
    (bury-buffer (current-buffer))
    (switch-to-buffer nil)))

(defun auto-view-mode ()
  "If the file of the current buffer is not writable, call view-mode.
  This is meant to be added to find-file-hooks."
  (if (and buffer-file-name
	   (not (file-writable-p buffer-file-name))) (view-mode)))
