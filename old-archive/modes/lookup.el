;;; lookup.el
;;; ===============================================================
;;; Small major and minor modes to facilitate translating documents
;;; from one language to another.
;;; ===============================================================
;;;
;;; Copyright (C) 1995 John Maraist  maraist@ira.uka.de
;;; Version 1.0


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
;;; A copy of the GNU General Public License can be obtained from the
;;; author of this program at the email address above; from the Free
;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA;
;;; or from most emacs implementations from the main "Help" or "Info"
;;; screens.


;; LCD Archive Entry:
;; lookup|John Maraist|maraist@ira.uka.de|
;; Small major and minor modes to facilitate manually translating
;; documents from one language to another.|
;; 30-Aug-1995|1.0|~/modes/lookup.el.gz|


;;; Purpose of this package:
;;;
;;; This package creates one major and one minor mode.  The major mode
;;; (vocabulary-mode) is for language-to-language dictionary files,
;;; which should be of the form: WORD TAB TRANSLATION, one per line,
;;; sorted alphabetically by word (see Alphabetization below).  The
;;; minor mode (lookup-mode) runs in other files, and adds commands
;;; and keybindings to interact with vocabulary buffers.


;;; Installation:
;;; 
;;; Autoloads: add the following directives to your one-time,
;;; at-startup emacs initialization file (usually ~/.emacs):
;;;
;;;   (autoload 'vocabulary-mode "lookup" nil t)
;;;   (autoload 'lookup-minor-mode "lookup" nil t)
;;;
;;; Auto mode deduction: add the following or similar to your
;;; initialization:
;;;
;;;   (setq auto-mode-alist
;;;         (cons '("\\.vocab$" . vocabulary-mode)
;;;	          auto-mode-alist))


;;; Usage:
;;; 
;;; Vocabulary files will automatically be raised in vocabulary-mode
;;; if you use the auto-mode-alist; otherwise open them and then type
;;;
;;;   M-x vocabulary-mode
;;;
;;; The minor mode must be explicitly activated; use
;;;
;;;   M-x lookup-minor-mode
;;;
;;; The minor mode has four key-bound commands:
;;;
;;;;; M-l M-s --- lookup-set-buffer
;;;;; This command sets the vocabulary-mode buffer where lookups from
;;;;; the minor mode buffer will be directed.  This setting is local
;;;;; to each buffer running the minor mode.  Note that the default
;;;;; lookup target set when first running the minor mode is the
;;;;; buffer most recently activated into vocabulary-mode.
;;;
;;;;; M-l M-l --- lookup-word
;;;;; Searches for the current region in the relevant vocabulary-mode
;;;;; buffer.  See "Lookups" below.
;;;
;;;;; M-l M-w --- mark-and-lookup-word
;;;;; Marks the word under the cursor (i.e., at the point) and looks
;;;;; it up in the relevant buffer.  See "Lookups" below.
;;;
;;;;; M-l M-a --- lookup-add
;;;;; Adds the marked word at the appropriate alphabetization point in
;;;;; the vocabulary mode buffer, and readies the cursor for the
;;;;; translation to be typed in.
;;;
;;; The keybindings are in lookup-mode-keymap for easy changing.  To
;;; give alternate bindings, either define lookup-mode-keymap before
;;; this file is loaded (with make-sparse-keymap and define-key), or
;;; set the current bindings to nil and define new ones (with
;;; define-key); see the defvar in the code.
;;; 
;;; The major mode has a single key-bound command:
;;;
;;;;; M-l M-s --- vocab-set-this-buffer
;;;;; Makes the current vocabulary-mode buffer the default target for
;;;;; subsequent activations of lookup-minor-mode.
;;;
;;; Lookups.  When a word from a minor mode buffer is looked up, the
;;; following happens:
;;; 
;;;;; First, any words in the target major mode buffer which have the
;;;;; search term as a prefix are highlighted.
;;;
;;;;; Second, the "best" (usually first) match is rendered in
;;;;; boldface.
;;;
;;;;; The cursor is moved to the beginning of the line where a new
;;;;; entry for the search term would be entered, considering
;;;;; alphabetization.  This point is given a different background
;;;;; color.
;;;
;;;;; In the minor mode buffer, the original search term is marked.
;;;
;;; 
;;; Alphabetization.  We currently handle alphabetization of German
;;; umlauted vowels and the ess-zett ligature in the "right way" for
;;; the implemented machine.  I don't know how protable this is.
;;; Obviously, this has been done only for German; such
;;; language-dependant hacks should probably be implemented as minor
;;; modes to vocabulary-mode.

;;; To do someday:
;;;
;;; 1. The major mode should have more keybound functions.
;;;
;;; 2. The layout of the major mode buffers should be overhauled and
;;; made more automatic.
;;;
;;; 3. The highlighting is done with extents.  I'm not sure that this
;;; is really portable to other emacs implementations, and so this
;;; feature should be turn off-able.
;;;
;;; 4. More things should be implemented as user options.
;;;
;;; 5. More elegantly generalize by-language treatment of special
;;; letters. 


;;; Version history:
;;;
;;;;; -- 1.0 -- 29 August 1995 --
;;;;; Initial release.


(defvar vocab-extent nil
  "The highlighted region in vocabulary mode.")
(defvar vocabulary-mode-map 
  (let ((m (make-sparse-keymap)))
    (define-key vocabulary-mode-map '[(meta l) (meta s)] 'vocab-set-this-buffer))
  "The keymap for vocabulary mode.")
(defvar vocabulary-mode-list nil
  "List of buffers in vocabulary mode, most recently opened first.")
(defvar vocab-last-lookup nil
  "The last word looked up in this vocabulary file.")

(put 'vocabulary-mode 'mode-class 'special)
(make-variable-buffer-local 'vocab-last-lookup)

(defvar lookup-minor-mode nil "Minor mode flag for looking up words in another buffer.")
(make-variable-buffer-local 'lookup-minor-mode)
(defvar lookup-source-buffer nil
  "The buffer to be used as the source in lookup minor mode.")
(defvar lookup-custom-suggest-buffer nil
  "Function to be used as first guess when finding a buffer name to suggest at prompt for setting a vocabulary source. Intended to be made into a local variable by mode hooks, etc.")

(defvar lookup-mode-keymap 
  (let ((m (make-sparse-keymap)))
    (define-key lookup-mode-keymap '[(meta l) (meta s)] 'lookup-set-buffer)
    (define-key lookup-mode-keymap '[(meta l) (meta l)] 'lookup-word)
    (define-key lookup-mode-keymap '[(meta l) (meta w)] 'mark-and-lookup-word)
    (define-key lookup-mode-keymap '[(meta l) (meta a)] 'lookup-add)
    m)
    "Keymap for lookup minor mode.")

(or (assq 'lookup-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'lookup-minor-mode 
					   lookup-mode-keymap)
				     minor-mode-map-alist)))
(or (assq 'lookup-minor-mode minor-mode-alist)
    (setq minor-mode-alist 
	  (cons '(lookup-minor-mode " Lookup") minor-mode-alist)))

(make-face 'vocab-lookup-first-match)
(set-face-foreground 'vocab-lookup-first-match "Black")
(set-face-background 'vocab-lookup-first-match "Yellow")
(set-face-font 'vocab-lookup-first-match "*-courier-bold-o-normal-*-12-*")
(make-face 'vocab-lookup-other-match)
(set-face-foreground 'vocab-lookup-other-match "Black")
(set-face-background 'vocab-lookup-other-match 'nil)
(set-face-font 'vocab-lookup-other-match "*-courier-bold-o-normal-*-12-*")
(make-face 'vocab-lookup-insertion)
(set-face-foreground 'vocab-lookup-insertion "Black")
(set-face-background 'vocab-lookup-insertion "Green")
(set-face-font 'vocab-lookup-insertion "*-courier-medium-o-normal-*-12-*")
(make-face 'vocab-inserting)
(set-face-foreground 'vocab-inserting "Black")
(set-face-background 'vocab-inserting "Yellow")
(set-face-font 'vocab-inserting "*-courier-bold-o-normal-*-12-*")
	       
(defvar un-umlautify-table
  '(("\\\"a" . "d") ("\\\"o" . "v")
    ("\\\"u" . "|") ("\\\"A" . "D")
    ("\\\"O" . "V") ("\\\"U" . "\")
    ("{\\ss}". "_"))
  "How we lookup LaTeX umlauted characters in lookup/vocabulary modes.")

(defun vocabulary-mode ()
  (interactive)
  (make-local-variable 'vocab-extent)
  (while (not (null (next-extent (current-buffer))))
    (delete-extent (next-extent (current-buffer))))
  (setq vocab-extent nil)
  (setq major-mode 'vocabulary-mode)
  (setq mode-name "Vocabulary")
  (setq vocabulary-mode-list (cons (current-buffer) vocabulary-mode-list))
  (use-local-map vocabulary-mode-map)
  (goto-char (point-min))
  (message "Vocabulary mode")
  (vocab-calculate-tab-width))

(defun vocab-mode-list-update (l)
  (if (null l)
      nil
    (if (null (buffer-name (car l)))
	(vocab-mode-list-update (cdr l))
      (cons (car l) (vocab-mode-list-update (cdr l))))))

(defun vocab-set-this-buffer ()
  "Set the current buffer to be the one processed by default when entering lookup minor mode.\n\nThis command is part of vocabulary mode."
  (interactive)
  (setq lookup-source-buffer (current-buffer))
  (message "Buffer selected as lookup minor mode source buffer."))

(defun vocab-calculate-tab-width ()
  (goto-char (point-min))
  (let ((p) (w))
    (while (not (eq (point) (point-max)))
      (setq p (point))
      (search-forward "\t" nil t nil)
      (setq w (- (point) p))
      (if (> w tab-width)
	  (setq tab-width w))
      (beginning-of-line)
      (next-line 1))))

(defun lookup-minor-mode (&optional b)
  (interactive)
  (setq lookup-minor-mode 
	(if (null b)
	    (not lookup-minor-mode)
	  (> (prefix-numeric-value b) 0)))
  (if lookup-minor-mode
      (progn (force-mode-line-update)
	     (make-local-variable 'lookup-source-buffer)
	     (setq vocabulary-mode-list (vocab-mode-list-update 
					 vocabulary-mode-list))
	     (if (null vocabulary-mode-list)
		 (message "No vocabulary list available")
	       (let ((vocab-buffer (car vocabulary-mode-list)))
		 (setq lookup-source-buffer vocab-buffer)
		 (message (concat "Taking vocabulary from buffer " 
				  (buffer-name lookup-source-buffer))))))
    (message "Exiting lookup minor mode."))
  (force-mode-line-update))

(defun lookup-set-buffer (&optional target)
  "Set a buffer as the target of word lookups from the current buffer in lookup minor mode."
  (interactive)
  (let* (;;(interacting    (null target))
	 (constr-suggest (let ((possible (concat (unsuffix-buffer-name) ".vocab")))
			   (if (bufferp (get-buffer possible))
			       possible
			     nil)))
	 (fn-suggestion  (if (not (null lookup-custom-suggest-buffer))
			     (funcall lookup-custom-suggest-buffer)
			   nil))
	 (suggest-buffer (or fn-suggestion constr-suggest)))
    (if (null target)
	(setq target (get-buffer (read-buffer "Set lookup source to what buffer? " 
					      suggest-buffer t))))
    (while (not (eq 'vocabulary-mode (major-mode-of-buffer target)))
      (beep) (beep)
      (message "Lookup source must be in vocabulary mode!")
      (sleep-for 1)
      (setq target (get-buffer (read-buffer "Set lookup source to what buffer? "
					    suggest-buffer t))))
    (setq lookup-source-buffer target)))
    
(defun lookup-word (point1 point2)
  "Lookup a word in the buffer designated by lookup-set-this-buffer.\n\nThis command is part of lookup-minor-mode."
  (interactive "r")
  (let* ((orig-word (buffer-substring point1 point2))
	 (word      (concat "\n" (un-umlautify orig-word))))
    (lookup-word-real word point1 point2 (selected-window))
    ))

(defun lookup-add ()
  "Add the last word looked up to the vocabulary file."
  (interactive)
  (switch-to-buffer-other-window lookup-source-buffer)
  (remove-text-properties (point-min) (point-max) '(face))
  (insert "\n")
  (backward-char 1)
  (put-text-property (point) (+ 1 (point)) 'face 'vocab-inserting)
  (insert vocab-last-lookup)
  (insert "\t"))
  
(defun lookup-word-real (word point1 point2 home-win)
  (if lookup-source-buffer
      (if (buffer-name lookup-source-buffer)
	  (progn (switch-to-buffer-other-window lookup-source-buffer)
		 (remove-text-properties (point-min) (point-max) '(face))
		 (let ((i (length word)))
		   (while (and (> i 1) 
			       (goto-char (point-min))
			       (not (search-forward (abs-substring word 0 i)
						    nil t nil)))
		     (setq i (- i 1)))
		   (if (equal i 1)
		       (message (concat "No match for " word))
		     (let ((first-line-start) (first-line-end) 
			   (next-line-start)  (last-line-end))
		       (recenter)
		       (beginning-of-line)
		       (setq first-line-start (point))
		       (end-of-line)
		       (setq first-line-end (point))
		       (forward-char)
		       (setq next-line-start (point))
		       (goto-char (point-max))
		       (not (search-backward (abs-substring word 0 i) nil t nil))
		       (next-line 1)
		       (end-of-line)
		       (setq last-line-end (point))
		       (put-text-property next-line-start last-line-end
					  'face 'vocab-lookup-other-match)
		       (put-text-property first-line-start first-line-end
					  'face 'vocab-lookup-first-match)
		       ;; (message (format "First line = (%d,%d); other text = (%d,%d)"
		       ;;		first-line-start first-line-end
		       ;;		next-line-start last-line-end))))
		       (message (concat "Matched " (abs-substring word 1 i))))
		     (let ((p1) (p2)
			   (dword (downcase (strip-umlauts (substring word 1)))))
		       (goto-char (point-min))
		       (while (and (string< (downcase (strip-umlauts (word-from-point)))
					    dword)
				   (not (eq (point) (point-max))))
			 (next-line 1))
		       (setq p1 (point))
		       (forward-char)
		       (setq p2 (point))
		       (backward-char)
		       (put-text-property p1 p2 'face 'vocab-lookup-insertion)
		       (recenter)
		       (message (format "(%d,%d) as insertion point for %s after %s" p1 p2 dword (downcase (word-from-point))))
		       ))
		   (setq vocab-last-lookup (substring word 1))
		   (select-window home-win)
		   (exchange-point-and-mark)
		   (exchange-point-and-mark)))
	(error "Lookup source buffer has been killed"))
    (error "No lookup source buffer selected.")))

(defun abs-substring (word pre-back &optional pre-front)
  (let* ((l     (length word))
	 (back  (max 0 pre-back))
	 (front (if pre-front
		    (min l pre-front)
		  l)))
    (if (< back front)
	(substring word back front)
      "")))
  
(defun un-umlautify (w)
  (let ((word w)
	(i    0))
    (while (> (length word) i)
      (let ((match3 (assoc (abs-substring word i (+ i 3)) un-umlautify-table))
	    (match5 (assoc (abs-substring word i (+ i 5)) un-umlautify-table)))
	(if match3
	    (setq word (concat (abs-substring word 0 i)
			       (cdr match3)
			       (abs-substring word (+ i 3))))
	  (if match5
	      (setq word (concat (abs-substring word 0 i)
				 (cdr match5)
				 (abs-substring word (+ i 5))))))
	(setq i (+ i 1))))
    word))
	
(defun mark-and-lookup-word ()
  (interactive)
  (mark-word 1)
  (let ((point1 (region-beginning))
	(point2 (region-end)))
    (lookup-word-real (concat "\n" (un-umlautify (buffer-substring point1 point2)))
		      point1
		      point2
		      (selected-window)))
  (exchange-point-and-mark))

(defun unsuffix-buffer-name ()
  (let* ((b-name (buffer-name))
	 (i      (- (length b-name) 1)))
    (while (and (>= i 0)
		(not (char-equal ?. (aref b-name i))))
      (setq i (- i 1)))
    (if (>= i 0)
	(substring b-name 0 i)
      b-name)))

(defun major-mode-of-buffer (buffer)
  (let ((mm))
    (save-excursion 
      (set-buffer buffer)
      (setq mm major-mode))
    mm))

(defun word-from-point ()
  (let ((old-point (point))
	(new-point))
    (forward-word 1)
    (setq new-point (point))
    (goto-char old-point)
    (buffer-substring old-point new-point)))

(defun strip-umlauts (word)
  (let* ((copy (substring word 0))
	 (l (length copy))
	 (i 0)
	 (y))
    (while (< i l)
      (setq y (assoc (aref copy i) strip-umlauts-table))
      (if y (aset copy i (cdr y)))
      (setq i (+ i 1)))
    copy))
(defconst strip-umlauts-table
  '((?d . ?a) (?v . ?o) (?| . ?u)
    (?D . ?A) (?V . ?O) (?\ . ?U)))
