;; LCD Archive Entry:
;; brief|Thomas Huber|huber@igc.chem.ethz.ch|
;; Brief keybindings suitable for use in a .emacs file (ie: not a mode).|
;; 10-Jun-1994||~/as-is/brief.el.Z|

(global-set-key "\M-]" 'fortran-column-ruler)
(global-set-key "\M-n" 'next-error)

(global-set-key "\M-b" 'electric-buffer-list)
(global-set-key "\M-c" 'begin-mark-col)
(global-set-key "\M-d" 'delete-complete-line)
(global-set-key "\M-e" 'find-file)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-l" 'begin-mark-line)
(global-set-key "\M-r" 'insert-file)
(global-set-key "\M-w" 'save-buffer)

(global-set-key "\C-s" 'brief-search)
(global-set-key "\C-r" 'search-again-forward)
(global-set-key "\C-u" 'search-again-backward)


(global-set-key	[f1] 'enlarge-window)
(global-set-key	[f2] 'shrink-window)
(global-set-key [f3] 'kill-rectangle)
(global-set-key [f4] 'yank-rectangle)
(global-set-key [f6] 'replace-string)
(global-set-key [f7] 'bm-start-kbd-macro)       ; F7
(global-set-key [f8] 'call-last-kbd-macro)	; F8
(global-set-key [f10] 'compile)
(global-set-key [f11] 'list-matching-lines)
(global-set-key [f12] 'bookmark-jump)
(global-set-key [S-f12] 'bookmark-set)
(global-set-key [f14] 'undo)

(global-set-key '[C-up] 'scroll-down-in-place)
(global-set-key '[C-down] 'scroll-up-in-place)
(global-set-key [help] 'hippie-expand)


;; BRIEF-MODE

(defvar mark-register (string-to-char "0") "Register for MARK-MODE")
(defvar act-search-string "")
(defvar act-replace-string "")
(defvar mark-mode-string "")
(defvar mark-mode nil)
      
(or global-mode-string (setq global-mode-string '("")))
(setq global-mode-string
      (append global-mode-string '(mark-mode-string)))
       

(global-set-key  "\M-=" 'bm-copy-line-to-scrap) 
(global-set-key  "\M--" 'bm-delete-line-to-scrap)
(global-set-key  "\M-i" 'insert-mark-register)
(global-set-key  "\M-l" 'begin-mark-line)
(global-set-key  "\M-c" 'begin-mark-col)



(defun begin-mark-line ()
  ""
  (interactive)
  (setq mark-mode t)
  (setq mark-mode-string "**** MARKING LINES ****")
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0)
  (save-excursion
	(beginning-of-line)
	(setq begin-point (point)))

  (global-set-key "\M-=" 'end-mark-mode-copy) 
  (global-set-key "\M--" 'end-mark-mode-delete)
  (global-set-key "\M-i" 'insert-mark-register))


(defun end-mark-mode-copy ()
  ""
  (interactive)
  (if mark-mode
	  (save-excursion
		(setq mark-mode nil)
		(beginning-of-line)
		(forward-line 1)
		(setq end-point (point))
		(copy-to-register mark-register begin-point end-point nil)
		(message "Lines copied to scrap!")
		(unset-keys))))


(defun end-mark-mode-delete ()
  ""
  (interactive)
  (if mark-mode
	  (save-excursion
		 (setq mark-mode nil)
		 (beginning-of-line)
		 (forward-line 1)
		 (setq end-point (point))
		 (copy-to-register mark-register begin-point end-point t)
		 (message "Lines deleted to scrap!")
		 (unset-keys))))


(defun bm-copy-line-to-scrap ()
  ""
  (interactive)
  (if mark-mode
	  ()
	(save-excursion
	  (beginning-of-line)
	  (setq begin-point (point))
	  (forward-line 1)
	  (setq end-point (point))
	  (copy-to-register mark-register begin-point end-point nil)
	  (message "Line copied to scrap!"))))


(defun bm-delete-line-to-scrap ()
  ""
  (interactive)
  (if mark-mode
	  ()
	(save-excursion
	  (beginning-of-line)
	  (setq begin-point (point))
	  (forward-line 1)
	  (setq end-point (point))
	  (copy-to-register mark-register begin-point end-point t)
	  (message "Line deleted to scrap!"))))


(defun begin-mark-col ()
  ""
  (interactive)
  (setq mark-mode t)
  (setq mark-mode-string "**** MARKING RECTANGLE ****")
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0)
  (setq begin-point (point))

  (global-set-key "\M-=" 'end-mark-col-copy) 
  (global-set-key "\M-i" 'insert-mark-register-col)
  (global-set-key "\M--" 'end-mark-col-delete))



(defun end-mark-col-copy ()
  ""
  (interactive)
  (if mark-mode
	  (save-excursion
		(setq mark-mode nil)
		(setq end-point (point))
		(copy-rectangle-to-register mark-register begin-point end-point nil)
		(message "Columns copied to scrap!")
		(unset-keys))))


(defun end-mark-col-delete ()
  ""
  (interactive)
  (if mark-mode
	  (save-excursion
		(setq mark-mode nil)
		(setq end-point (point))
		(copy-rectangle-to-register mark-register begin-point end-point t)
		(message "Columns deleted to scrap!")
		(unset-keys))))


(defun insert-mark-register-col ()
  ""
  (interactive)
  (insert-register mark-register))


(defun insert-mark-register ()
  "" 
  (interactive)
  (beginning-of-line)
  (insert-register mark-register))


(defun unset-keys ()
  ""
  (interactive)
  (global-set-key "\M-=" 'bm-copy-line-to-scrap) 
  (global-set-key "\M--" 'bm-delete-line-to-scrap)
  (setq mark-mode-string "")
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))


(defun delete-complete-line ()
  ""
  (interactive)
  (beginning-of-line)
  (setq begin-point (point))
  (forward-line 1)
  (setq end-point (point))
  (kill-region begin-point end-point))


(defun next-buffer ()
  ""
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (nth 0 (buffer-list)))
  (if (string-match "*" (buffer-name))
	  (next-buffer)))


(defun brief-search ()
  ""
  (interactive)
  (setq act-search-string (read-string "Search for: " act-search-string))
  (re-search-forward act-search-string))


(defun search-again-forward ()
  ""
  (interactive)
  (re-search-forward act-search-string))


(defun search-again-backward ()
  ""
  (interactive)
  (re-search-backward act-search-string))

(defun bm-start-kbd-macro ()
  ""
  (interactive)
  (global-set-key [f7] 'bm-end-kbd-macro)
  (start-kbd-macro nil))


(defun bm-end-kbd-macro ()
  ""
  (interactive)
  (global-set-key [f7] 'bm-start-kbd-macro)
  (end-kbd-macro nil))
