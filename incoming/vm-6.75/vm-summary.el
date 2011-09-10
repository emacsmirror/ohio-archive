;;; Summary gathering and formatting routines for VM
;;; Copyright (C) 1989-1995 Kyle E. Jones
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

(provide 'vm-summary)

(defun vm-summary-mode-internal ()
  (setq mode-name "VM Summary"
	major-mode 'vm-summary-mode
	mode-line-format vm-mode-line-format
	;; must come after the setting of major-mode
	mode-popup-menu (and vm-use-menus vm-popup-menu-on-mouse-3
			     (vm-menu-support-possible-p)
			     (vm-menu-mode-menu))
	buffer-read-only t
	vm-summary-pointer nil
	vm-summary-=> (if (stringp vm-summary-arrow) vm-summary-arrow "")
	vm-summary-no-=> (make-string (length vm-summary-=>) ? )
	truncate-lines t)
  ;; horizontal scrollbar off by default
  ;; user can turn it on in summary hook if desired.
  (and vm-xemacs-p (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map vm-summary-mode-map)
  (and (vm-menu-support-possible-p)
       (vm-menu-install-menus))
;; using the 'mouse-face property gives faster highlighting than this.
;;  (and vm-mouse-track-summary
;;       (vm-mouse-support-possible-p)
;;       (vm-mouse-xemacs-mouse-p)
;;       (add-hook 'mode-motion-hook 'mode-motion-highlight-line))
  (if (and vm-mutable-frames (or vm-frame-per-folder vm-frame-per-summary))
      (vm-set-hooks-for-frame-deletion))
  (run-hooks 'vm-summary-mode-hook)
  ;; Lucid Emacs apparently used this name
  (run-hooks 'vm-summary-mode-hooks))

(fset 'vm-summary-mode 'vm-mode)
(put 'vm-summary-mode 'mode-class 'special)

(defun vm-summarize (&optional display raise)
  "Summarize the contents of the folder in a summary buffer. 
The format is as described by the variable vm-summary-format.  Generally
one line per message is most pleasing to the eye but this is not
mandatory."
  (interactive "p\np")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (if (null vm-summary-buffer)
      (let ((b (current-buffer))
	    (read-only vm-folder-read-only))
	(setq vm-summary-buffer
	      (get-buffer-create (format "%s Summary" (buffer-name))))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (setq vm-mail-buffer b
		vm-folder-read-only read-only)
	  (vm-summary-mode-internal))
	(vm-set-summary-redo-start-point t)))
  (if display
      (save-excursion
	(vm-goto-new-summary-frame-maybe)
	(vm-display vm-summary-buffer t
		    '(vm-summarize
		      vm-summarize-other-frame)
		    (list this-command) (not raise))
	;; need to do this after any frame creation because the
	;; toolbar sets frame-specific height and width specifiers.
	(set-buffer vm-summary-buffer)
	(and (vm-toolbar-support-possible-p) vm-use-toolbar
	     (vm-toolbar-install-toolbar)))
    (vm-display nil nil '(vm-summarize vm-summarize-other-frame)
		(list this-command)))
  (vm-update-summary-and-mode-line))

(defun vm-summarize-other-frame (&optional display)
  "Like vm-summarize, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'summary))
  (vm-summarize display)
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

(defun vm-do-summary (&optional start-point)
  (let ((m-list (or start-point vm-message-list))
	mp m
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 10))
	(do-mouse-track
	    (and vm-mouse-track-summary
		 (vm-mouse-support-possible-p)))
	summary)
    (setq mp m-list)
    (save-excursion
      (set-buffer vm-summary-buffer)
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p)))
	(unwind-protect
	    (progn
	      (if start-point
		  (if (vm-su-start-of (car mp))
		      (progn
			(goto-char (vm-su-start-of (car mp)))
			(delete-region (point) (point-max)))
		    (goto-char (point-max)))
		(erase-buffer)
		(setq vm-summary-pointer nil))
	      ;; avoid doing long runs down the marker chain while
	      ;; building the summary.  use integers to store positions
	      ;; and then convert them to markers after all the
	      ;; insertions are done.
	      (while mp
		(setq summary (vm-su-summary (car mp)))
		(vm-set-su-start-of (car mp) (point))
		(insert vm-summary-no-=>)
		(vm-tokenized-summary-insert (car mp) (vm-su-summary (car mp)))
		(vm-set-su-end-of (car mp) (point))
		(setq mp (cdr mp) n (1+ n))
		(if (zerop (% n modulus))
		    (message "Generating summary... %d" n)))
	      ;; now convert the ints to markers.
	      (if (>= n modulus)
		  (message "Generating summary markers... "))
	      (setq mp m-list)
	      (while mp
		(setq m (car mp))
		(and do-mouse-track
		     (vm-set-su-summary-mouse-track-overlay-of
		      m
		      (vm-mouse-set-mouse-track-highlight
		       (vm-su-start-of m)
		       (vm-su-end-of m))))
		(vm-set-su-start-of m (vm-marker (vm-su-start-of m)))
		(vm-set-su-end-of m (vm-marker (vm-su-end-of m)))
		(setq mp (cdr mp))))
	  (set-buffer-modified-p modified))
	(run-hooks 'vm-summary-redo-hook)))
    (if (>= n modulus)
	(message "Generating summary... done"))))

(defun vm-do-needed-summary-rebuild ()
  (if (and vm-summary-redo-start-point vm-summary-buffer)
      (progn
	(vm-copy-local-variables vm-summary-buffer 'vm-summary-show-threads)
	(vm-do-summary (and (consp vm-summary-redo-start-point)
			    vm-summary-redo-start-point))
	(setq vm-summary-redo-start-point nil)
	(and vm-message-pointer
	     (vm-set-summary-pointer (car vm-message-pointer)))
	(setq vm-need-summary-pointer-update nil))
    (and vm-need-summary-pointer-update
	 vm-summary-buffer
	 vm-message-pointer
	 (progn
	   (vm-set-summary-pointer (car vm-message-pointer))
	   (setq vm-need-summary-pointer-update nil)))))

(defun vm-update-message-summary (m)
  (if (and (vm-su-start-of m)
	   (marker-buffer (vm-su-start-of m)))
      (let ((modified (buffer-modified-p))
	    (do-mouse-track
	     (and vm-mouse-track-summary
		  (vm-mouse-support-possible-p)))
	    summary)
	(save-excursion
	  (setq summary (vm-su-summary m))
	  (set-buffer (marker-buffer (vm-su-start-of m)))
	  (let ((buffer-read-only nil)
		(selected nil)
		(modified (buffer-modified-p)))
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-su-start-of m))
		  (setq selected (not (looking-at vm-summary-no-=>)))
		  ;; We do a little dance to update the text in
		  ;; order to make the markers in the text do
		  ;; what we want.
		  ;;
		  ;; 1. We need to avoid having the su-start-of
		  ;;    and su-end-of markers clumping together at
		  ;;    the start position.
		  ;;
		  ;; 2. We want the window point marker (w->pointm
		  ;;    in the Emacs display code) to move to the
		  ;;    start of the summary entry if it is
		  ;;    anywhere within the su-start-of to
		  ;;    su-end-of region.
		  ;;
		  ;; We achieve (2) by deleting before inserting.
		  ;; Reversing the order of insertion/deletion
		  ;; pushes the point marker into the next
		  ;; summary entry. We achieve (1) by inserting a
		  ;; placeholder character at the end of the
		  ;; summary entry before deleting the region.
		  (goto-char (vm-su-end-of m))
		  (insert-before-markers "z")
		  (goto-char (vm-su-start-of m))
		  (delete-region (point) (1- (vm-su-end-of m)))
		  (if (not selected)
		      (insert vm-summary-no-=>)
		    (insert vm-summary-=>))
		  (vm-tokenized-summary-insert m (vm-su-summary m))
		  (delete-char 1)
		  (run-hooks 'vm-summary-update-hook)
		  (and do-mouse-track
		       (vm-mouse-set-mouse-track-highlight
			(vm-su-start-of m)
			(vm-su-end-of m)
			(vm-su-summary-mouse-track-overlay-of m)))
		  (if (and selected vm-summary-highlight-face)
		      (vm-summary-highlight-region (vm-su-start-of m) (point)
						   vm-summary-highlight-face)))
	      (set-buffer-modified-p modified)))))))

(defun vm-set-summary-pointer (m)
  (if vm-summary-buffer
      (let ((w (vm-get-visible-buffer-window vm-summary-buffer))
	    (do-mouse-track
	       (and vm-mouse-track-summary
		    (vm-mouse-support-possible-p)))
	    (old-window nil))
	(vm-save-buffer-excursion
	  (unwind-protect
	      (progn
		(set-buffer vm-summary-buffer)
		(if w
		    (progn
		      (setq old-window (selected-window))
		      (select-window w)))
		(let ((buffer-read-only nil))
		  (if (and vm-summary-pointer
			   (vm-su-start-of vm-summary-pointer))
		      (progn
			(goto-char (vm-su-start-of vm-summary-pointer))
			(insert vm-summary-no-=>)
			(delete-char (length vm-summary-=>))
			(and do-mouse-track
			     (vm-mouse-set-mouse-track-highlight
			      (vm-su-start-of vm-summary-pointer)
			      (vm-su-end-of vm-summary-pointer)
			      (vm-su-summary-mouse-track-overlay-of
			       vm-summary-pointer)))))
		  (setq vm-summary-pointer m)
		  (goto-char (vm-su-start-of m))
		  (let ((modified (buffer-modified-p)))
		    (unwind-protect
			(progn
			  (insert vm-summary-=>)
			  (delete-char (length vm-summary-=>))
			  (and do-mouse-track
			       (vm-mouse-set-mouse-track-highlight
				(vm-su-start-of m) (vm-su-end-of m)
				(vm-su-summary-mouse-track-overlay-of m))))
		      (set-buffer-modified-p modified)))
		  (forward-char (- (length vm-summary-=>)))
		  (if vm-summary-highlight-face
		      (vm-summary-highlight-region
		       (vm-su-start-of m) (vm-su-end-of m)
		       vm-summary-highlight-face))
		  (and w vm-auto-center-summary (vm-auto-center-summary))
		  (run-hooks 'vm-summary-pointer-update-hook)))
	    (and old-window (select-window old-window)))))))

(defun vm-summary-highlight-region (start end face)
  (cond (vm-fsfemacs-p
	 (if (and vm-summary-overlay (overlay-buffer vm-summary-overlay))
	     (move-overlay vm-summary-overlay start end)
	   (setq vm-summary-overlay (make-overlay start end))
	   (overlay-put vm-summary-overlay 'evaporate nil)
	   (overlay-put vm-summary-overlay 'face face)))
	(vm-xemacs-p
	 (if (and vm-summary-overlay (extent-end-position vm-summary-overlay))
	     (set-extent-endpoints vm-summary-overlay start end)
	   (setq vm-summary-overlay (make-extent start end))
	   ;; the reason this isn't needed under FSF Emacs is
	   ;; that insert-before-markers also inserts before
	   ;; overlays!  so a summary update of an entry just
	   ;; before this overlay in the summary buffer won't
	   ;; leak into the overlay, but it _will_ leak into an
	   ;; XEmacs extent.
	   (set-extent-property vm-summary-overlay 'start-open t)
	   (set-extent-property vm-summary-overlay 'detachable nil)
	   (set-extent-property vm-summary-overlay 'face face)))))

(defun vm-auto-center-summary ()
  (if vm-auto-center-summary
      (if (or (eq vm-auto-center-summary t) (not (one-window-p t)))
	  (recenter '(4)))))

(defun vm-summary-sprintf (format message &optional tokenize)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let* ((alist-var (if tokenize
			'vm-summary-tokenized-compiled-format-alist
		      'vm-summary-untokenized-compiled-format-alist))
	 (match (assoc format (symbol-value alist-var))))
    (if (null match)
	(progn
	  (vm-summary-compile-format format tokenize)
	  (setq match (assoc format (symbol-value alist-var)))))
    ;; The local variable name `vm-su-message' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-su-message message))
      (eval (cdr match)))))

(defun vm-summary-compile-format (format tokenize)
  (let ((return-value (vm-summary-compile-format-1 format tokenize)))
    (if tokenize
	(setq vm-summary-tokenized-compiled-format-alist
	      (cons (cons format return-value)
		    vm-summary-tokenized-compiled-format-alist))
      (setq vm-summary-untokenized-compiled-format-alist
	    (cons (cons format return-value)
		  vm-summary-untokenized-compiled-format-alist)))))

(defun vm-tokenized-summary-insert (message tokens)
  (if (stringp tokens)
      (insert tokens)
    (let (token)
      (while tokens
	(setq token (car tokens))
	(cond ((stringp token)
	       (if vm-display-using-mime
		   (insert (vm-decode-mime-encoded-words-in-string token))
		 (insert token)))
	      ((eq token 'number)
	       (insert (vm-padded-number-of message)))
	      ((eq token 'mark)
	       (insert (vm-su-mark message)))
	      ((eq token 'thread-indent)
	       (if (and vm-summary-show-threads
			(natnump vm-summary-thread-indent-level))
		   (insert-char ?\ (* vm-summary-thread-indent-level
				      (vm-th-thread-indentation message))))))
	(setq tokens (cdr tokens))))))

(defun vm-summary-compile-format-1 (format &optional tokenize)
  (let ((case-fold-search nil)
	(done nil)
	(list nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end 0)
	token conv-spec)
    (store-match-data nil)
    (while (not done)
      (setq token nil)
      (while
	  (and (not token)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([aAcdfFhHiIlLmMnstTwyz*%]\\|U[A-Za-z]\\)"
		format (match-end 0)))
	(setq conv-spec (aref format (match-beginning 5)))
	(if (memq conv-spec '(?a ?A ?c ?d ?f ?F ?h ?H ?i ?L ?I ?l ?M
				 ?m ?n ?s ?t ?T ?U ?w ?y ?z ?* ))
	    (progn
	      (cond ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-su-attribute-indicators
					    'vm-su-message) sexp)))
		    ((= conv-spec ?A)
		     (setq sexp (cons (list 'vm-su-attribute-indicators-long
					    'vm-su-message) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-su-byte-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-su-monthday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-su-interesting-from
					    'vm-su-message) sexp)))
		    ((= conv-spec ?F)
		     (setq sexp (cons (list 'vm-su-interesting-full-name
					    'vm-su-message) sexp)))
		    ((= conv-spec ?h)
		     (setq sexp (cons (list 'vm-su-hour
					    'vm-su-message) sexp)))
		    ((= conv-spec ?H)
		     (setq sexp (cons (list 'vm-su-hour-short
					    'vm-su-message) sexp)))
		    ((= conv-spec ?i)
		     (setq sexp (cons (list 'vm-su-message-id
					    'vm-su-message) sexp)))
		    ((= conv-spec ?I)
		     (if tokenize
			 (setq token ''thread-indent)
		       (setq sexp (cons (list 'vm-su-thread-indent
					      'vm-su-message) sexp))))
		    ((= conv-spec ?l)
		     (setq sexp (cons (list 'vm-su-line-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?L)
		     (setq sexp (cons (list 'vm-su-labels
					    'vm-su-message) sexp)))
		    ((= conv-spec ?m)
		     (setq sexp (cons (list 'vm-su-month
					    'vm-su-message) sexp)))
		    ((= conv-spec ?M)
		     (setq sexp (cons (list 'vm-su-month-number
					    'vm-su-message) sexp)))
		    ((= conv-spec ?n)
		     (if tokenize
			 (setq token ''number)
		       (setq sexp (cons (list 'vm-padded-number-of
					      'vm-su-message) sexp))))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-su-subject
					    'vm-su-message) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-su-to-names
					    'vm-su-message) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-su-to
					    'vm-su-message) sexp)))
		    ((= conv-spec ?U)
		     (setq sexp
			   (cons (list 'vm-run-user-summary-function
				       (list 'quote
					     (intern
					      (concat
					       "vm-summary-function-"
					       (substring
						format
						(1+ (match-beginning 5))
						(+ 2 (match-beginning 5))))))
				       'vm-su-message) sexp)))
		    ((= conv-spec ?w)
		     (setq sexp (cons (list 'vm-su-weekday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?y)
		     (setq sexp (cons (list 'vm-su-year
					    'vm-su-message) sexp)))
		    ((= conv-spec ?z)
		     (setq sexp (cons (list 'vm-su-zone
					    'vm-su-message) sexp)))
		    ((= conv-spec ?*)
		     (if tokenize
			 (setq token ''mark)
		       (setq sexp (cons (list 'vm-su-mark
					      'vm-su-message) sexp)))))
	      (cond ((and (not token) vm-display-using-mime)
		     (setcar sexp
			     (list 'vm-decode-mime-encoded-words-in-string
				   (car sexp)))))
	      (cond ((and (not token) (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((and (not token) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((and (not token) (match-beginning 3))
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (cond ((and (not token) vm-display-using-mime)
		     (setcar sexp
			     (list 'vm-reencode-mime-encoded-words-in-string
				   (car sexp)))))
	      (setq sexp-fmt
		    (cons (if token "" "%s")
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons "%%"
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	  (setq last-match-end (match-end 0)))
      (if (not token)
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt))
      (if tokenize
	  (setq list (nconc list (if (equal sexp "") nil (list sexp))
			    (and token (list token)))
		sexp nil
		sexp-fmt nil)))
    (if list (cons 'list list) sexp)))

(defun vm-get-header-contents (message header-name-regexp &optional clump-sep)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)")
	  message (vm-real-message-of message))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-headers-of message))
	(let ((case-fold-search t))
	  (while (and (or (null contents) clump-sep)
		      (re-search-forward regexp (vm-text-of message) t)
		      (save-excursion (goto-char (match-beginning 0))
				      (vm-match-header)))
	    (if contents
		(setq contents
		      (concat contents clump-sep (vm-matched-header-contents)))
	      (setq contents (vm-matched-header-contents))))))
      contents )))

(defun vm-left-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat string (make-string (- width (length string)) ?\ ))))

(defun vm-right-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat (make-string (- width (length string)) ?\ ) string)))

(defun vm-numeric-left-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat string (make-string (- width (length string)) ?0))))

(defun vm-numeric-right-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat (make-string (- width (length string)) ?0) string)))

(defun vm-truncate-string (string width)
  (cond
;; doesn't work because the width of wide chars such as the Kanji
;; glyphs as not even multiples of the default face's font width.
;;	((fboundp 'char-width)
;;	 (let ((i 0)
;;	       (lim (length string))
;;	       (total 0))
;;	   (while (and (< i lim) (<= total width))
;;	     (setq total (+ total (char-width (aref string i)))
;;		   i (1+ i)))
;;	   (if (<= total width)
;;	       string
;;	     (substring string 0 (1- i)))))
	((<= (length string) (vm-abs width))
	 string)
	((< width 0)
	 (substring string width))
	(t
	 (substring string 0 width))))

(defun vm-su-attribute-indicators (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (cond ((vm-filed-flag m) "F")
	 ((vm-written-flag m) "W")
	 (t " "))
   (cond ((vm-replied-flag m) "R")
	 ((vm-forwarded-flag m) "Z")
	 ((vm-redistributed-flag m) "B")
	 (t " "))
   (cond ((vm-edited-flag m) "E")
	 (t " "))))

(defun vm-su-attribute-indicators-long (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (if (vm-replied-flag m) "r" " ")
   (if (vm-forwarded-flag m) "z" " ")
   (if (vm-redistributed-flag m) "b" " ")
   (if (vm-filed-flag m) "f" " ")
   (if (vm-written-flag m) "w" " ")
   (if (vm-edited-flag m) "e" " ")))

(defun vm-su-byte-count (m)
  (or (vm-byte-count-of m)
      (vm-set-byte-count-of
       m
       (int-to-string
	(- (vm-text-end-of (vm-real-message-of m))
	   (vm-text-of (vm-real-message-of m)))))))

(defun vm-su-weekday (m)
  (or (vm-weekday-of m)
      (progn (vm-su-do-date m) (vm-weekday-of m))))

(defun vm-su-monthday (m)
  (or (vm-monthday-of m)
      (progn (vm-su-do-date m) (vm-monthday-of m))))

(defun vm-su-month (m)
  (or (vm-month-of m)
      (progn (vm-su-do-date m) (vm-month-of m))))

(defun vm-su-month-number (m)
  (or (vm-month-number-of m)
      (progn (vm-su-do-date m) (vm-month-number-of m))))

(defun vm-su-year (m)
  (or (vm-year-of m)
      (progn (vm-su-do-date m) (vm-year-of m))))

(defun vm-su-hour-short (m)
  (let ((string (vm-su-hour m)))
    (if (> (length string) 5)
	(substring string 0 5)
      string)))

(defun vm-su-hour (m)
  (or (vm-hour-of m)
      (progn (vm-su-do-date m) (vm-hour-of m))))

(defun vm-su-zone (m)
  (or (vm-zone-of m)
      (progn (vm-su-do-date m) (vm-zone-of m))))

(defun vm-su-mark (m) (if (vm-mark-of m) "*" " "))

;; Some yogurt-headed delivery agents don't provide a Date: header.
(defun vm-grok-From_-date (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(BellFrom_ From_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (or (looking-at
		     ;; special case this so that the "remote from blah"
		     ;; isn't included.
		     "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\) remote from .*")
		    (looking-at "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\)"))
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-parse-date (date)
  (let ((weekday "")
	(monthday "")
	(month "")
	(year "")
	(hour "")
	(timezone "")
	(start nil)
	string
	(case-fold-search t))
    (if (string-match "sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat" date)
	(setq weekday (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec" date)
	(setq month (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?" date)
	(setq hour (substring date (match-beginning 0) (match-end 0))))
    (if (or (string-match "[^a-z][+---][0-9][0-9][0-9][0-9]" date)
	    (string-match "e[ds]t\\|c[ds]t\\|p[ds]t\\|m[ds]t" date)
	    (string-match "ast\\|nst\\|met\\|eet\\|jst\\|bst\\|ut" date)
	    (string-match "gmt\\([+---][0-9]+\\)?" date))
	(setq timezone (substring date (match-beginning 0) (match-end 0))))
    (while (string-match "\\(\\`\\|[^:+---0-9]\\|[a-z]-\\)[0-9]+\\(\\'\\|[^:]\\)"
			 date start)
      (setq string (substring date (match-end 1) (match-beginning 2))
	    start (match-end 0))
      (cond ((string-match "\\`[4-9]." string)
	     ;; Assume that any two digits less than 40 are a date and not
	     ;; a year.  The world will surely end soon.
	     (setq year (concat "19" string)))
	    ((< (length string) 3)
	     (setq monthday string))
	    (t (setq year string))))
    
    (aset vm-parse-date-workspace 0 weekday)
    (aset vm-parse-date-workspace 1 monthday)
    (aset vm-parse-date-workspace 2 month)
    (aset vm-parse-date-workspace 3 year)
    (aset vm-parse-date-workspace 4 hour)
    (aset vm-parse-date-workspace 5 timezone)
    vm-parse-date-workspace))

(defun vm-su-do-date (m)
  (let ((case-fold-search t)
	vector date)
    (setq date (or (vm-get-header-contents m "Date:") (vm-grok-From_-date m)))
    (cond
     ((null date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m ""))
     ((string-match
;; The date format recognized here is the one specified in RFC 822.
;; Some slop is allowed e.g. dashes between the monthday, month and year
;; because such malformed headers have been observed.
"\\(\\([a-z][a-z][a-z]\\),\\)?[ \t\n]*\\([0-9][0-9]?\\)[ \t\n---]*\\([a-z][a-z][a-z]\\)[ \t\n---]*\\([0-9]*[0-9][0-9]\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+]?[0-9][0-9][0-9][0-9]\\)"
       date)
      (if (match-beginning 2)
	  (vm-su-do-weekday m (substring date (match-beginning 2)
					    (match-end 2)))
	(vm-set-weekday-of m ""))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-su-do-month m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (= 2 (length (vm-year-of m)))
	  (vm-set-year-of m (concat "19" (vm-year-of m))))
      (vm-set-hour-of m (substring date (match-beginning 6) (match-end 6)))
      (vm-set-zone-of m (substring date (match-beginning 7) (match-end 7))))
     ((string-match
;; UNIX ctime(3) format, with slop allowed in the whitespace, and we allow for
;; the possibility of a timezone at the end.
"\\([a-z][a-z][a-z]\\)[ \t\n]*\\([a-z][a-z][a-z]\\)[ \t\n]*\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([0-9][0-9][0-9][0-9]\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+][0-9][0-9][0-9][0-9]\\)?"
       date)
      (vm-su-do-weekday m (substring date (match-beginning 1)
				     (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (match-beginning 6)
	  (vm-set-zone-of m (substring date (match-beginning 6)
				       (match-end 6)))
	(vm-set-zone-of m "")))
     (t
      (setq vector (vm-parse-date date))
      (vm-su-do-weekday m (elt vector 0))
      (vm-set-monthday-of m (elt vector 1))
      (vm-su-do-month m (elt vector 2))
      (vm-set-year-of m (elt vector 3))
      (vm-set-hour-of m (elt vector 4))
      (vm-set-zone-of m (elt vector 5)))))

  ;; Normalize all hour and date specifications to avoid jagged margins.
  ;; If the hour is " 3:..." or "3:...", turn it into "03:...".
  ;; If the date is "03", turn it into " 3".
  (cond ((null (vm-hour-of m)) nil)
	((string-match "\\`[0-9]:" (vm-hour-of m))
	 (vm-set-hour-of m (concat "0" (vm-hour-of m)))))
  (cond ((null (vm-monthday-of m)) nil)
	((string-match "\\`0[0-9]\\'" (vm-monthday-of m))
	 (vm-set-monthday-of m (substring (vm-monthday-of m) 1 2))))
  )

(defun vm-su-do-month (m month-abbrev)
  (let ((val (assoc (downcase month-abbrev) vm-month-alist)))
    (if val
	(progn (vm-set-month-of m (nth 1 val))
	       (vm-set-month-number-of m (nth 2 val)))
      (vm-set-month-of m "")
      (vm-set-month-number-of m ""))))

(defun vm-su-do-weekday (m weekday-abbrev)
  (let ((val (assoc (downcase weekday-abbrev) vm-weekday-alist)))
    (if val
	(vm-set-weekday-of m (nth 1 val))
      (vm-set-weekday-of m ""))))

(defun vm-run-user-summary-function (function message)
  (let ((message (vm-real-message-of message)))
    (save-excursion
      (set-buffer (vm-buffer-of message))
      (save-restriction
	(widen)
	(save-excursion
	  (narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	  (funcall function message))))))

(defun vm-su-full-name (m)
  (or (vm-full-name-of m)
      (progn (vm-su-do-author m) (vm-full-name-of m))))

(defun vm-su-interesting-full-name (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to-names m))
	  (vm-su-full-name m)))
    (vm-su-full-name m)))

(defun vm-su-from (m)
  (or (vm-from-of m)
      (progn (vm-su-do-author m) (vm-from-of m))))

(defun vm-su-interesting-from (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to m))
	  (vm-su-from m)))
    (vm-su-from m)))

;; Some yogurt-headed delivery agents don't even provide a From: header.
(defun vm-grok-From_-author (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(From_ BellFrom_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of message))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (looking-at "From \\([^ \t\n]+\\)")
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-su-do-author (m)
  (let ((full-name (vm-get-header-contents m "Full-Name:"))
	(from (or (vm-get-header-contents m "From:" ", ")
		  (vm-grok-From_-author m)))
	pair i)
    (if (and full-name (string-match "^[ \t]*$" full-name))
	(setq full-name nil))
    (if (null from)
	(progn
	  (setq from "???")
	  (if (null full-name)
	      (setq full-name "???")))
      (setq pair (funcall vm-chop-full-name-function from)
	    from (or (nth 1 pair) from)
	    full-name (or full-name (nth 0 pair) from)))
    (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
 	(setq full-name
 	      (substring full-name (match-beginning 1) (match-end 1))))
    (while (setq i (string-match "\n" full-name i))
      (aset full-name i ?\ ))
    (vm-set-full-name-of m full-name)
    (vm-set-from-of m from)))

(defun vm-default-chop-full-name (address)
  (let ((from address)
	(full-name nil))
    (cond ((string-match
"\\`[ \t\n]*\\([^< \t\n]+\\([ \t\n]+[^< \t\n]+\\)*\\)?[ \t\n]*<\\([^>]+\\)>[ \t\n]*\\'"
			 address)
	   (if (match-beginning 1)
	       (setq full-name
		     (substring address (match-beginning 1) (match-end 1))))
	   (setq from
		 (substring address (match-beginning 3) (match-end 3))))
	  ((string-match
"\\`[ \t\n]*\\(\\(\"[^\"]+\"\\|[^\"( \t\n]\\)+\\)[ \t\n]*(\\([^ \t\n]+\\([ \t\n]+[^ \t\n]+\\)*\\)?)[ \t\n]*\\'"
			 address)
	   (if (match-beginning 3)
	       (setq full-name
		     (substring address (match-beginning 3) (match-end 3))))
	   (setq from
		 (substring address (match-beginning 1) (match-end 1)))))
    (list full-name from)))

;; test for existence and functionality of mail-extract-address-components
;; there are versions out there that don't work right, so we run
;; some test data through it to see if we can trust it.
(defun vm-choose-chop-full-name-function (address)
  (let ((test-data '(("kyle@uunet.uu.net" .
		      (nil "kyle@uunet.uu.net"))
		     ("c++std=lib@inet.research.att.com" .
		      (nil "c++std=lib@inet.research.att.com"))
		     ("\"Piet.Rypens\" <rypens@reks.uia.ac.be>" .
		      ("Piet Rypens" "rypens@reks.uia.ac.be"))
		     ("makke@wins.uia.ac.be (Marc.Gemis)" .
		      ("Marc Gemis" "makke@wins.uia.ac.be"))
		     ("" . (nil nil))))
	(failed nil)
	result)
    (while test-data
      (setq result (condition-case nil
		       (mail-extract-address-components (car (car test-data)))
		     (error nil)))
      (if (not (equal result (cdr (car test-data))))
	  ;; failed test, use default
	  (setq failed t
		test-data nil)
	(setq test-data (cdr test-data))))
    (if failed
	;; it failed, use default
	(setq vm-chop-full-name-function 'vm-default-chop-full-name)
      ;; it passed the tests
      (setq vm-chop-full-name-function 'mail-extract-address-components))
    (funcall vm-chop-full-name-function address)))

(defun vm-su-do-recipients (m)
  (let ((mail-use-rfc822 t) i names addresses to cc all list full-name)
    (setq to (or (vm-get-header-contents m "To:" ", ")
		 (vm-get-header-contents m "Apparently-To:" ", ")
		 ;; desperation....
		 (user-login-name))
	  cc (vm-get-header-contents m "Cc:" ", ")
	  all to
	  all (if all (concat all ", " cc) cc)
	  addresses (rfc822-addresses all))
    (setq list (vm-parse-addresses all))
    (while list
      ;; Just like vm-su-do-author:
      (setq full-name (or (nth 0 (funcall vm-chop-full-name-function
					  (car list)))
			  (car list)))
      ;; If double quoted are around the full name, fish the name out.
      (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
	  (setq full-name
		(substring full-name (match-beginning 1) (match-end 1))))
      (while (setq i (string-match "\n" full-name i))
	(aset full-name i ?\ ))
      (setq names (cons full-name names))
      (setq list (cdr list)))
    (setq names (nreverse names)) ; added by jwz for fixed vm-parse-addresses
    (vm-set-to-of m (mapconcat 'identity addresses ", "))
    (vm-set-to-names-of m (mapconcat 'identity names ", "))))

(defun vm-su-to (m)
  (or (vm-to-of m) (progn (vm-su-do-recipients m) (vm-to-of m))))

(defun vm-su-to-names (m)
  (or (vm-to-names-of m) (progn (vm-su-do-recipients m) (vm-to-names-of m))))
				  
(defun vm-su-message-id (m)
  (or (vm-message-id-of m)
      (vm-set-message-id-of
       m
       (or (let ((id (vm-get-header-contents m "Message-Id:")))
	     (and id (car (vm-parse id "[^<]*\\(<[^>]+>\\)"))))
	   ;; try running md5 on the message body to produce an ID
	   ;; better than nothing.
	   (save-excursion
	     (set-buffer (vm-buffer-of (vm-real-message-of m)))
	     (save-restriction
	       (widen)
	       (condition-case nil
		   (concat "<fake-VM-id."
			   (vm-pop-md5-string
			    (buffer-substring
			     (vm-text-of (vm-real-message-of m))
			     (vm-text-end-of (vm-real-message-of m))))
			   "@talos.iv>")
		 (error nil))))
	   (concat "<" (int-to-string (vm-abs (random))) "@toto.iv>")))))

(defun vm-su-line-count (m)
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (save-excursion
	 (set-buffer (vm-buffer-of (vm-real-message-of m)))
	 (save-restriction
	   (widen)
	   (int-to-string
	    (count-lines (vm-text-of (vm-real-message-of m))
			 (vm-text-end-of (vm-real-message-of m)))))))))

(defun vm-su-subject (m)
  (or (vm-subject-of m)
      (vm-set-subject-of
       m
       (let ((subject (or (vm-get-header-contents m "Subject:" " ") ""))
	     (i nil))
	 (while (setq i (string-match "\n" subject i))
	   (aset subject i ?\ ))
	 subject ))))

(defun vm-su-summary (m)
  (if (and (vm-virtual-message-p m) (not (vm-virtual-messages-of m)))
      (or (vm-virtual-summary-of m)
	  (save-excursion
	    (vm-select-folder-buffer)
	    (vm-set-virtual-summary-of m (vm-summary-sprintf
					  vm-summary-format m t))
	    (vm-virtual-summary-of m)))
    (or (vm-summary-of m)
	(save-excursion
	  (vm-select-folder-buffer)
	  (vm-set-summary-of m (vm-summary-sprintf vm-summary-format m t))
	  (vm-summary-of m)))))

(defun vm-fix-my-summary!!! ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Fixing your summary...")
  (let ((mp vm-message-list))
    (while mp
      (vm-set-summary-of (car mp) nil)
      (vm-mark-for-summary-update (car mp))
      (setq mp (cdr mp)))
    (vm-stuff-folder-attributes nil)
    (set-buffer-modified-p t)
    (vm-update-summary-and-mode-line))
  (message "Fixing your summary... done"))

(defun vm-su-thread-indent (m)
  (if (natnump vm-summary-thread-indent-level)
      (make-string (* (vm-th-thread-indentation m)
		      vm-summary-thread-indent-level)
		   ?\ )
    "" ))

(defun vm-su-labels (m)
  (or (vm-label-string-of m)
      (vm-set-label-string-of
       m
       (mapconcat 'identity (vm-labels-of m) ","))
      (vm-label-string-of m)))

(defun vm-substring (string from &optional to)
  (let ((work-buffer nil))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert string)
	  (if (null to)
	      (setq to (length string))
	    (if (< to 0)
		(setq to (+ (length string) to))))
	  ;; string indices start at 0, buffers start at 1.
	  (setq from (1+ from)
		to (1+ to))
	  (if (> from (point-min))
	      (delete-region (point-min) from))
	  (if (< to (point-max))
	      (delete-region to (point-max)))
	  (buffer-string))
      (and work-buffer (kill-buffer work-buffer)))))

				 
