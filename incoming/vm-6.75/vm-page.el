;;; Commands to move around within a VM message
;;; Copyright (C) 1989-1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-page)

(defun vm-scroll-forward (&optional arg)
  "Scroll forward a screenful of text.
If the current message is being previewed, the message body is revealed.
If at the end of the current message, moves to the next message iff the
value of vm-auto-next-message is non-nil.
Prefix argument N means scroll forward N lines."
  (interactive "P")
  (let ((mp-changed (vm-follow-summary-cursor))
	needs-decoding 
	(was-invisible nil))
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-check-for-killed-presentation)
    (vm-error-if-folder-empty)
    (setq needs-decoding (and vm-display-using-mime
			      (not vm-mime-decoded)
			      (not (vm-mime-plain-message-p
				    (car vm-message-pointer)))
			      vm-auto-decode-mime-messages
			      (eq vm-system-state 'previewing)))
    (and vm-presentation-buffer
	 (set-buffer vm-presentation-buffer))
    (let ((point (point))
	  (w (vm-get-visible-buffer-window (current-buffer))))
      (if (or (null w)
	      (not (vm-frame-totally-visible-p (vm-window-frame w))))
	  (progn
	    (vm-display (current-buffer) t
			'(vm-scroll-forward vm-scroll-backward)
			(list this-command 'reading-message))
	    ;; window start sticks to end of clip region when clip
	    ;; region moves back past it in the buffer.  fix it.
	    (setq w (vm-get-visible-buffer-window (current-buffer)))
	    (if (= (window-start w) (point-max))
		(set-window-start w (point-min)))
	    (setq was-invisible t))))
    (if (or mp-changed was-invisible needs-decoding
	    (and (eq vm-system-state 'previewing)
		 (pos-visible-in-window-p
		  (point-max)
		  (vm-get-visible-buffer-window (current-buffer)))))
	(progn
	  (if (not was-invisible)
	      (let ((w (vm-get-visible-buffer-window (current-buffer)))
		    old-w-start)
		(setq old-w-start (window-start w))
		;; save-excurison to avoid possible buffer change
		(save-excursion (vm-select-frame (window-frame w)))
		(vm-raise-frame (window-frame w))
		(vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
			    (list this-command 'reading-message))
		(setq w (vm-get-visible-buffer-window (current-buffer)))
		(and w (set-window-start w old-w-start))))
	  (if (eq vm-system-state 'previewing)
	      (vm-show-current-message))
	  (vm-howl-if-eom))
      (let ((vmp vm-message-pointer)
	    (msg-buf (current-buffer))
	    (h-diff 0)
	    w old-w old-w-height old-w-start result)
	(if (eq vm-system-state 'previewing)
	    (vm-show-current-message))
	(setq vm-system-state 'reading)
	(setq old-w (vm-get-visible-buffer-window msg-buf)
	      old-w-height (window-height old-w)
	      old-w-start (window-start old-w))
	(setq w (vm-get-visible-buffer-window msg-buf))
	(vm-select-frame (window-frame w))
	(vm-raise-frame (window-frame w))
	(vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
		    (list this-command 'reading-message))
	(setq w (vm-get-visible-buffer-window msg-buf))
	(if (null w)
	    (error "current window configuration hides the message buffer.")
	  (setq h-diff (- (window-height w) old-w-height)))
	;; must restore this since it gets clobbered by window
	;; teardown and rebuild done by the window config stuff.
	(set-window-start w old-w-start)
	(setq old-w (selected-window))
	(unwind-protect
	    (progn
	      (select-window w)
	      (let ((next-screen-context-lines
		     (+ next-screen-context-lines h-diff)))
		(while (eq (setq result (vm-scroll-forward-internal arg))
			   'tryagain))
		(cond ((and (not (eq result 'next-message))
			    vm-honor-page-delimiters)
		       (vm-narrow-to-page)
		       ;; This voodoo is required!  For some
		       ;; reason the 18.52 emacs display
		       ;; doesn't immediately reflect the
		       ;; clip region change that occurs
		       ;; above without this mantra. 
		       (scroll-up 0)))))
	  (select-window old-w))
	(set-buffer msg-buf)
	(cond ((eq result 'next-message)
	       (vm-next-message))
	      ((eq result 'end-of-message)
	       (let ((vm-message-pointer vmp))
		 (vm-emit-eom-blurb)))
	      (t
	       (and (> (prefix-numeric-value arg) 0)
		    (vm-howl-if-eom)))))))
  (if (not vm-startup-message-displayed)
      (vm-display-startup-message)))

(defun vm-scroll-forward-internal (arg)
  (let ((direction (prefix-numeric-value arg))
	(w (selected-window)))
    (condition-case error-data
	(progn (scroll-up arg) nil)
;; this looks like it should work, but doesn't because the
;; redisplay code is schizophrenic when it comes to updates.  A
;; window position may no longer be visible but
;; pos-visible-in-window-p will still say it is because it was
;; visible before some window size change happened.
;;	(progn
;;	  (if (and (> direction 0)
;;		   (pos-visible-in-window-p
;;		    (vm-text-end-of (car vm-message-pointer))))
;;	      (signal 'end-of-buffer nil)
;;	    (scroll-up arg))
;;	  nil )
      (error
       (if (or (and (< direction 0)
		    (> (point-min) (vm-text-of (car vm-message-pointer))))
	       (and (>= direction 0)
		    (/= (point-max)
			(vm-text-end-of (car vm-message-pointer)))))
	   (progn
	     (vm-widen-page)
	     (if (>= direction 0)
		 (progn
		   (forward-page 1)
		   (set-window-start w (point))
		   nil )
	       (if (or (bolp)
		       (not (save-excursion
			      (beginning-of-line)
			      (looking-at page-delimiter))))
		   (forward-page -1))
	       (beginning-of-line)
	       (set-window-start w (point))
	       'tryagain))
	 (if (eq (car error-data) 'end-of-buffer)
	     (if vm-auto-next-message
		 'next-message
	       (set-window-point w (point))
	       'end-of-message)))))))

;; exploratory scrolling, what a concept.
;;
;; we do this because pos-visible-in-window-p checks the current
;; window configuration, while this exploratory scrolling forces
;; Emacs to recompute the display, giving us an up to the moment
;; answer about where the end of the message is going to be
;; visible when redisplay finally does occur.
(defun vm-howl-if-eom ()
  (let ((w (get-buffer-window (current-buffer))))
    (and w
	 (save-excursion
	   (save-window-excursion
	     (condition-case ()
		 (let ((next-screen-context-lines 0))
		   (select-window w)
		   (save-excursion
		     (save-window-excursion
		       ;; scroll-fix.el replaces scroll-up and
		       ;; doesn't behave properly when it hits
		       ;; end of buffer.  It does this!
		       ;; (ding)
		       ;; (message (get 'beginning-of-buffer 'error-message))
		       (let ((scroll-in-place-replace-original nil))
			 (scroll-up nil))))
		   nil)
	       (error t))))
	 (= (vm-text-end-of (car vm-message-pointer)) (point-max))
	 (vm-emit-eom-blurb))))

(defun vm-emit-eom-blurb ()
  (if (vm-full-name-of (car vm-message-pointer))
      (progn
	(if (and (stringp vm-summary-uninteresting-senders)
		 (string-match vm-summary-uninteresting-senders
			       (vm-su-from (car vm-message-pointer))))
	    (message "End of message %s to %s"
		     (vm-number-of (car vm-message-pointer))
		     (vm-su-to-names (car vm-message-pointer)))
	  (message "End of message %s from %s"
		   (vm-number-of (car vm-message-pointer))
		   (vm-full-name-of (car vm-message-pointer)))))
    (message "End of message %s"
	     (vm-number-of (car vm-message-pointer)))))

(defun vm-scroll-backward (&optional arg)
  "Scroll backward a screenful of text.
Prefix N scrolls backward N lines."
  (interactive "P")
  (vm-scroll-forward (cond ((null arg) '-)
			   ((consp arg) (list (- (car arg))))
			   ((numberp arg) (- arg))
			   ((symbolp arg) nil)
			   (t arg))))

(defun vm-highlight-headers ()
  (cond
   ((and vm-xemacs-p vm-use-lucid-highlighting)
    (require 'highlight-headers)
    ;; disable the url marking stuff, since VM has its own interface.
    (let ((highlight-headers-mark-urls nil)
	  (highlight-headers-regexp (or vm-highlighted-header-regexp
					highlight-headers-regexp)))
      (highlight-headers (point-min) (point-max) t)))
   (vm-xemacs-p
    (let (e)
      (map-extents (function
		    (lambda (e ignore)
		      (if (extent-property e 'vm-highlight)
			  (delete-extent e))
		      nil))
		   (current-buffer) (point-min) (point-max))
      (goto-char (point-min))
      (while (vm-match-header)
	(cond ((vm-match-header vm-highlighted-header-regexp)
	       (setq e (make-extent (vm-matched-header-contents-start)
				    (vm-matched-header-contents-end)))
	       (set-extent-property e 'face vm-highlighted-header-face)
	       (set-extent-property e 'vm-highlight t)))
	(goto-char (vm-matched-header-end)))))
   ((fboundp 'overlay-put)
    (let (o-lists p)
      (setq o-lists (overlay-lists)
	    p (car o-lists))
      (while p
	(and (overlay-get (car p) 'vm-highlight)
	     (delete-overlay (car p)))
	(setq p (cdr p)))
      (setq p (cdr o-lists))
      (while p
	(and (overlay-get (car p) 'vm-highlight)
	     (delete-overlay (car p)))
	(setq p (cdr p)))
      (goto-char (point-min))
      (while (vm-match-header)
	(cond ((vm-match-header vm-highlighted-header-regexp)
	       (setq p (make-overlay (vm-matched-header-contents-start)
				     (vm-matched-header-contents-end)))
	       (overlay-put p 'face vm-highlighted-header-face)
	       (overlay-put p 'vm-highlight t)))
	(goto-char (vm-matched-header-end)))))))

(defun vm-energize-urls ()
  ;; Don't search too long in large regions.  If the region is
  ;; large, search just the head and the tail of the region since
  ;; they tend to contain the interesting text.
  (let ((search-limit vm-url-search-limit)
	search-pairs n)
    (if (and search-limit (> (- (point-max) (point-min)) search-limit))
	(setq search-pairs (list (cons (point-min)
				       (+ (point-min) (/ search-limit 2)))
				 (cons (- (point-max) (/ search-limit 2))
				       (point-max))))
      (setq search-pairs (list (cons (point-min) (point-max)))))
    (cond
     (vm-xemacs-p
      (let (e)
	(map-extents (function
		      (lambda (e ignore)
			(if (extent-property e 'vm-url)
			    (delete-extent e))
			nil))
		     (current-buffer) (point-min) (point-max))
	(while search-pairs
	  (goto-char (car (car search-pairs)))
	  (while (re-search-forward vm-url-regexp (cdr (car search-pairs)) t)
	    (setq n 1)
	    (while (null (match-beginning n))
	      (vm-increment n))
	    (setq e (make-extent (match-beginning n) (match-end n)))
	    (set-extent-property e 'vm-url t)
	    (if vm-highlight-url-face
		(set-extent-property e 'face vm-highlight-url-face))
	    (if vm-url-browser
		(let ((keymap (make-sparse-keymap))
		      (popup-function
		       (if (save-excursion
			     (goto-char (match-beginning n))
			     (looking-at "mailto:"))
			   'vm-menu-popup-mailto-url-browser-menu
			 'vm-menu-popup-url-browser-menu)))
		  (define-key keymap 'button2 'vm-mouse-send-url-at-event)
		  (if vm-popup-menu-on-mouse-3
		      (define-key keymap 'button3 popup-function))
		  (define-key keymap "\r"
		    (function (lambda () (interactive)
				(vm-mouse-send-url-at-position (point)))))
		  (set-extent-property e 'keymap keymap)
		  (set-extent-property e 'balloon-help 'vm-url-help)
		  (set-extent-property e 'highlight t))))
	  (setq search-pairs (cdr search-pairs)))))
     ((and vm-fsfemacs-p
	   (fboundp 'overlay-put))
      (let (o-lists o p)
	(setq o-lists (overlay-lists)
	      p (car o-lists))
	(while p
	  (and (overlay-get (car p) 'vm-url)
	       (delete-overlay (car p)))
	  (setq p (cdr p)))
	(setq p (cdr o-lists))
	(while p
	  (and (overlay-get (car p) 'vm-url)
	       (delete-overlay (car p)))
	  (setq p (cdr p)))
	(while search-pairs
	  (goto-char (car (car search-pairs)))
	  (while (re-search-forward vm-url-regexp (cdr (car search-pairs)) t)
	    (setq n 1)
	    (while (null (match-beginning n))
	      (vm-increment n))
	    (setq o (make-overlay (match-beginning n) (match-end n)))
	    (overlay-put o 'vm-url t)
	    (if vm-highlight-url-face
		(overlay-put o 'face vm-highlight-url-face))
	    (if vm-url-browser
		(let ((keymap (make-sparse-keymap))
		      (popup-function
		       (if (save-excursion
			     (goto-char (match-beginning n))
			     (looking-at "mailto:"))
			   'vm-menu-popup-mailto-url-browser-menu
			 'vm-menu-popup-url-browser-menu)))
		  (overlay-put o 'mouse-face 'highlight)
		  (setq keymap (nconc keymap (current-local-map)))
		  (if vm-popup-menu-on-mouse-3
		      (define-key keymap [mouse-3] popup-function))
		  (define-key keymap "\r"
		    (function (lambda () (interactive)
				(vm-mouse-send-url-at-position (point)))))
		  (overlay-put o 'local-map keymap))))
	  (setq search-pairs (cdr search-pairs))))))))

(defun vm-energize-headers ()
  (cond
   (vm-xemacs-p
    (let ((search-tuples '(("^From:" vm-menu-author-menu)
			   ("^Subject:" vm-menu-subject-menu)))
	  regexp menu keymap e)
      (map-extents (function
		    (lambda (e ignore)
		      (if (extent-property e 'vm-header)
			  (delete-extent e))
		      nil))
		   (current-buffer) (point-min) (point-max))
      (while search-tuples
	(goto-char (point-min))
	(setq regexp (nth 0 (car search-tuples))
	      menu (symbol-value (nth 1 (car search-tuples))))
	(while (re-search-forward regexp nil t)
	  (save-excursion (goto-char (match-beginning 0)) (vm-match-header))
	  (setq e (make-extent (vm-matched-header-contents-start)
			       (vm-matched-header-contents-end)))
	  (set-extent-property e 'vm-header t)
	  (setq keymap (make-sparse-keymap))
	  ;; Might as well make button2 do what button3 does in
	  ;; this case, since there is no default 'select'
	  ;; action.
	  (define-key keymap 'button2
	    (list 'lambda () '(interactive)
		  (list 'popup-menu (list 'quote menu))))
	  (if vm-popup-menu-on-mouse-3
	      (define-key keymap 'button3
		(list 'lambda () '(interactive)
		      (list 'popup-menu (list 'quote menu)))))
	  (set-extent-property e 'keymap keymap)
	  (set-extent-property e 'balloon-help 'vm-mouse-3-help)
	  (set-extent-property e 'highlight t))
	(setq search-tuples (cdr search-tuples)))))
   ((and vm-fsfemacs-p
	 (fboundp 'overlay-put))
    (let ((search-tuples '(("^From:" vm-menu-fsfemacs-author-menu)
			   ("^Subject:" vm-menu-fsfemacs-subject-menu)))
	  regexp menu
	  o-lists o p)
      (setq o-lists (overlay-lists)
	    p (car o-lists))
      (while p
	(and (overlay-get (car p) 'vm-header)
	     (delete-overlay (car p)))
	(setq p (cdr p)))
      (setq p (cdr o-lists))
      (while p
	(and (overlay-get (car p) 'vm-header)
	     (delete-overlay (car p)))
	(setq p (cdr p)))
      (while search-tuples
	(goto-char (point-min))
	(setq regexp (nth 0 (car search-tuples))
	      menu (symbol-value (nth 1 (car search-tuples))))
	(while (re-search-forward regexp nil t)
	  (goto-char (match-end 0))
	  (save-excursion (goto-char (match-beginning 0)) (vm-match-header))
	  (setq o (make-overlay (vm-matched-header-contents-start)
				(vm-matched-header-contents-end)))
	  (overlay-put o 'vm-header menu)
	  (overlay-put o 'mouse-face 'highlight))
	(setq search-tuples (cdr search-tuples)))))))

(defun vm-display-xface ()
  (let ((case-fold-search t) e g h)
    (if (map-extents (function
		      (lambda (e ignore)
			(if (extent-property e 'vm-xface)
			    t
			  nil)))
		     (current-buffer) (point-min) (point-max))
	nil
      (goto-char (point-min))
      (if (find-face 'vm-xface)
	  nil
	(make-face 'vm-xface)
	(set-face-background 'vm-xface "white")
	(set-face-foreground 'vm-xface "black"))
      (if (re-search-forward "^X-Face:" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (vm-match-header)
	    (setq h (concat "X-Face: " (vm-matched-header-contents)))
	    (setq g (intern h vm-xface-cache))
	    (if (boundp g)
		(setq g (symbol-value g))
	      (set g (make-glyph
		      (list
		       (list 'global (cons '(tty) [nothing]))
		       (list 'global (cons '(win) (vector 'xface ':data h))))))
	      (setq g (symbol-value g))
	      ;; XXX broken.  Gives extra pixel lines at the
	      ;; bottom of the glyph in 19.12
	      ;;(set-glyph-baseline g 100)
	      (set-glyph-face g 'vm-xface))
	    (setq e (make-extent (vm-vheaders-of (car vm-message-pointer))
				 (vm-vheaders-of (car vm-message-pointer))))
	    (set-extent-property e 'vm-xface t)
	    (set-extent-begin-glyph e g))))))

(defun vm-url-help (object)
  (format
   "Use mouse button 2 to send the URL to %s.
Use mouse button 3 to choose a Web browser for the URL."
   (cond ((stringp vm-url-browser) vm-url-browser)
	 ((eq vm-url-browser 'w3-fetch)
	  "Emacs W3")
	 ((eq vm-url-browser 'w3-fetch-other-frame)
	  "Emacs W3")
	 ((eq vm-url-browser 'vm-mouse-send-url-to-mosaic)
	  "Mosaic")
	 ((eq vm-url-browser 'vm-mouse-send-url-to-netscape)
	  "Netscape")
	 (t (symbol-name vm-url-browser)))))

(defun vm-energize-urls-in-message-region (&optional start end)
  (save-excursion
    (or start (setq start (vm-headers-of (car vm-message-pointer))))
    (or end (setq end (vm-text-end-of (car vm-message-pointer))))
    ;; energize the URLs
    (if (or vm-highlight-url-face vm-url-browser)
	(save-restriction
	  (widen)
	  (narrow-to-region start end)
	  (vm-energize-urls)))))
    
(defun vm-highlight-headers-maybe ()
  ;; highlight the headers
  (if (or vm-highlighted-header-regexp
	  (and vm-xemacs-p vm-use-lucid-highlighting))
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer)))
	(vm-highlight-headers))))

(defun vm-energize-headers-and-xfaces ()
  ;; energize certain headers
  (if (and vm-use-menus (vm-menu-support-possible-p))
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-of (car vm-message-pointer)))
	(vm-energize-headers)))
  ;; display xfaces, if we can
  (if (and vm-display-xfaces
	   vm-xemacs-p
	   (featurep 'xface))
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-of (car vm-message-pointer)))
	(vm-display-xface))))

(defun vm-narrow-for-preview ()
  (widen)
  ;; hide as much of the message body as vm-preview-lines specifies
  (narrow-to-region
   (vm-vheaders-of (car vm-message-pointer))
   (cond ((not (eq vm-preview-lines t))
	  (min
	   (vm-text-end-of (car vm-message-pointer))
	   (save-excursion
	     (goto-char (vm-text-of (car vm-message-pointer)))
	     (forward-line (if (natnump vm-preview-lines) vm-preview-lines 0))
	     (point))))
	 (t (vm-text-end-of (car vm-message-pointer))))))

(defun vm-preview-current-message ()
  (vm-save-buffer-excursion
   (setq vm-system-state 'previewing
	 vm-mime-decoded nil)
   (if vm-real-buffers
       (vm-make-virtual-copy (car vm-message-pointer)))

   ;; run the message select hooks.
   (save-excursion
     (vm-select-folder-buffer)
     (vm-run-message-hook (car vm-message-pointer) 'vm-select-message-hook)
     (and vm-select-new-message-hook (vm-new-flag (car vm-message-pointer))
	  (vm-run-message-hook (car vm-message-pointer)
			       'vm-select-new-message-hook))
     (and vm-select-unread-message-hook
	  (vm-unread-flag (car vm-message-pointer))
	  (vm-run-message-hook (car vm-message-pointer)
			       'vm-select-unread-message-hook)))

   (vm-narrow-for-preview)
   (if (or vm-mime-display-function
	   (natnump vm-fill-paragraphs-containing-long-lines)
	   (and vm-display-using-mime
		(not (vm-mime-plain-message-p (car vm-message-pointer)))))
       (let ((layout (vm-mm-layout (car vm-message-pointer))))
	 (vm-make-presentation-copy (car vm-message-pointer))
	 (vm-save-buffer-excursion
	  (vm-replace-buffer-in-windows (current-buffer)
					vm-presentation-buffer))
	 (set-buffer vm-presentation-buffer)
	 (setq vm-system-state 'previewing)
	 (vm-narrow-for-preview))
     (setq vm-presentation-buffer nil)
     (and vm-presentation-buffer-handle
	  (vm-replace-buffer-in-windows vm-presentation-buffer-handle
					(current-buffer))))

   ;; at this point the current buffer is the presentation buffer
   ;; if we're using one for this message.
   (vm-unbury-buffer (current-buffer))

   (if (and vm-display-using-mime
	    vm-auto-decode-mime-messages
	    vm-mime-decode-for-preview
	    (if vm-mail-buffer
		(not (vm-buffer-variable-value vm-mail-buffer
					       'vm-mime-decoded))
	      (not vm-mime-decoded))
	    (not (vm-mime-plain-message-p (car vm-message-pointer))))
       (progn
	 (condition-case data
	     (vm-decode-mime-message)
	   (vm-mime-error (vm-set-mime-layout-of (car vm-message-pointer)
						 (car (cdr data)))
			  (message "%s" (car (cdr data)))))
	 (vm-narrow-for-preview))
     (vm-energize-urls-in-message-region)
     (vm-highlight-headers-maybe)
     (vm-energize-headers-and-xfaces))

   (if vm-honor-page-delimiters
       (vm-narrow-to-page))
   (goto-char (vm-text-of (car vm-message-pointer)))
   ;; If we have a window, set window start appropriately.
   (let ((w (vm-get-visible-buffer-window (current-buffer))))
     (if w
	 (progn (set-window-start w (point-min))
		(set-window-point w (vm-text-of (car vm-message-pointer))))))
   (if (or (null vm-preview-lines)
	   (and (not vm-preview-read-messages)
		(not (vm-new-flag (car vm-message-pointer)))
		(not (vm-unread-flag (car vm-message-pointer)))))
       (vm-show-current-message)
     (vm-update-summary-and-mode-line))))

(defun vm-show-current-message ()
  (and vm-display-using-mime
       vm-auto-decode-mime-messages
       (if vm-mail-buffer
	   (not (vm-buffer-variable-value vm-mail-buffer 'vm-mime-decoded))
	 (not vm-mime-decoded))
       (not (vm-mime-plain-message-p (car vm-message-pointer)))
       (condition-case data
	   (vm-decode-mime-message)
	 (vm-mime-error (vm-set-mime-layout-of (car vm-message-pointer)
					       (car (cdr data)))
			(message "%s" (car (cdr data))))))
  (if (and (natnump vm-fill-paragraphs-containing-long-lines)
	   (vm-mime-plain-message-p (car vm-message-pointer)))
      (let ((needmsg (> (- (vm-text-end-of (car vm-message-pointer))
			   (vm-text-of (car vm-message-pointer)))
			12000)))
	(if needmsg
	    (message "Searching for paragraphs to fill..."))
	(vm-fill-paragraphs-containing-long-lines
	 vm-fill-paragraphs-containing-long-lines
	 (vm-text-of (car vm-message-pointer))
	 (vm-text-end-of (car vm-message-pointer)))
	(if needmsg
	    (message "Searching for paragraphs to fill... done"))))
  (vm-save-buffer-excursion
   (save-excursion
     (save-excursion
       (goto-char (point-min))
       (widen)
       (narrow-to-region (point) (vm-text-end-of (car vm-message-pointer))))
     (if vm-honor-page-delimiters
	 (progn
	   (if (looking-at page-delimiter)
	       (forward-page 1))
	   (vm-narrow-to-page))))
   ;; don't mark the message as read if the user can't see it!
   (if (vm-get-visible-buffer-window (current-buffer))
       (progn
	 (save-excursion
	   (setq vm-system-state 'showing)
	   (if vm-mail-buffer
	       (vm-set-buffer-variable vm-mail-buffer 'vm-system-state
				       'showing))
	   ;; We could be in the presentation buffer here.  Since
	   ;; the presentation buffer's message pointer and sole
	   ;; message are a mockup, they will cause trouble if
	   ;; passed into the undo/update system.  So we switch
	   ;; into the real message buffer to do attribute
	   ;; updates.
	   (vm-select-folder-buffer)
	   (cond ((vm-new-flag (car vm-message-pointer))
		  (vm-set-new-flag (car vm-message-pointer) nil)))
	   (cond ((vm-unread-flag (car vm-message-pointer))
		  (vm-set-unread-flag (car vm-message-pointer) nil))))
	 (vm-update-summary-and-mode-line)
	 (vm-howl-if-eom))
     (vm-update-summary-and-mode-line))))

(defun vm-expose-hidden-headers ()
  "Toggle exposing and hiding message headers that are normally not visible."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (vm-display (current-buffer) t '(vm-expose-hidden-headers)
	      '(vm-expose-hidden-headers reading-message))
  (let* ((exposed (= (point-min) (vm-start-of (car vm-message-pointer)))))
    (vm-widen-page)
    (goto-char (point-max))
    (widen)
    (if exposed
	(narrow-to-region (point) (vm-vheaders-of (car vm-message-pointer)))
      (narrow-to-region (point) (vm-start-of (car vm-message-pointer))))
    (goto-char (point-min))
    (let (w)
      (setq w (vm-get-visible-buffer-window (current-buffer)))
      (and w (set-window-point w (point-min)))
      (and w
	   (= (window-start w) (vm-vheaders-of (car vm-message-pointer)))
	   (not exposed)
	   (set-window-start w (vm-start-of (car vm-message-pointer)))))
    (if vm-honor-page-delimiters
	(vm-narrow-to-page))))

(defun vm-widen-page ()
  (if (or (> (point-min) (vm-text-of (car vm-message-pointer)))
	  (/= (point-max) (vm-text-end-of (car vm-message-pointer))))
      (narrow-to-region (vm-vheaders-of (car vm-message-pointer))
			(if (or (vm-new-flag (car vm-message-pointer))
				(vm-unread-flag (car vm-message-pointer)))
			    (vm-text-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer))))))

(defun vm-narrow-to-page ()
  (save-excursion
    (let (min max (omin (point-min)) (omax (point-max)))
      (if (or (bolp) (not (save-excursion
			    (beginning-of-line)
			    (looking-at page-delimiter))))
	  (forward-page -1))
      (setq min (point))
      (forward-page 1)
      (beginning-of-line)
      (setq max (point))
      (narrow-to-region min max))))

(defun vm-beginning-of-message ()
  "Moves to the beginning of the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (vm-widen-page)
  (push-mark)
  (vm-display (current-buffer) t '(vm-beginning-of-message)
	      '(vm-beginning-of-message reading-message))
  (let ((osw (selected-window)))
    (unwind-protect
	(progn
	  (select-window (vm-get-visible-buffer-window (current-buffer)))
	  (goto-char (point-min)))
      (if (not (eq osw (selected-window)))
	  (select-window osw))))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page)))

(defun vm-end-of-message ()
  "Moves to the end of the current message, exposing and flagging it read
as necessary."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (vm-widen-page)
  (push-mark)
  (vm-display (current-buffer) t '(vm-end-of-message)
	      '(vm-end-of-message reading-message))
  (let ((osw (selected-window)))
    (unwind-protect
	(progn
	  (select-window (vm-get-visible-buffer-window (current-buffer)))
	  (goto-char (point-max)))
      (if (not (eq osw (selected-window)))
	  (select-window osw))))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page)))
