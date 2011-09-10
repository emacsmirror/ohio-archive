;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; locate.el --
;; Copyright (C) 95: by Michael Huehne of 3-S GmbH root@technik.3-s.de
;; ITIID           : 3-S GmbH H3 :-)
;; Author          : Kai Grossjohann
;; Created On      : Sat Apr 15 21:31:33 1995
;; Last Modified By: Kai Grossjohann
;; Last Modified On: Fri Jun  2 17:11:23 1995
;; Update Count    : 42
;; Status          : ACHTUNG ! Neue Software !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HISTORY

(require 'linemenu)

(defvar locate-mode-map (make-keymap))

(defvar locate-find-file-mode-map (make-keymap))

(defvar locate-insert-file-mode-map (make-keymap))

(defun locate-mode ()
  "A few simple keybindings for locate."
  (use-local-map locate-mode-map)
  (setq major-mode 'locate-mode)
  (setq mode-name "Locate")
  (linemenu-initialize)
  (run-hooks 'locate-mode-hooks))

(defun locate-find-file-mode ()
  "A few simple keybindings for locate-find-file."
  (use-local-map locate-find-file-mode-map)
  (setq major-mode 'locate-find-file-mode)
  (setq mode-name "Locate find file")
  (linemenu-initialize)
  (run-hooks 'locate-find-file-mode-hooks))

(defun locate-insert-file-mode ()
  "A few simple keybindings for locate-find-file."
  (use-local-map locate-insert-file-mode-map)
  (setq major-mode 'locate-insert-file-mode)
  (setq mode-name "Locate insert file")
  (linemenu-initialize)
  (run-hooks 'locate-insert-file-mode-hooks))

(defun locate (pat)
  "Execute the locate command with the given pattern."
  (interactive "sEnter pattern: ")
  (let ((lbuf (get-buffer-create "*locate*")))
	(switch-to-buffer lbuf)
	(erase-buffer)
	(call-process "locate" nil t nil pat)
	(if (bobp)
		(progn
		  (message "locate.el: No files found.")
		  (kill-buffer (current-buffer)))
	  (end-of-buffer)
	  (delete-backward-char 1)
	  (beginning-of-buffer)
	  (locate-mode))))


(defun locate-find-file (pat)
  "Execute the locate command with the given pattern."
  (interactive "sEnter pattern: ")
  (let ((lbuf (get-buffer-create "*locate*")))
	(switch-to-buffer lbuf)
	(erase-buffer)
	(call-process "locate" nil t nil pat)
	(if (bobp)
		(progn
		  (message "locate.el: No files found.")
		  (kill-buffer (current-buffer)))
	  (end-of-buffer)
	  (delete-backward-char 1)
	  (beginning-of-buffer)
	  (locate-find-file-mode))))


(defun locate-insert-file (pat)
  "Execute the locate command with the given pattern."
  (interactive "sEnter pattern: ")
  (let ((lbuf (get-buffer-create "*locate*")))
	(switch-to-buffer lbuf)
	(erase-buffer)
	(call-process "locate" nil t nil pat)
	(if (bobp)
		(progn
		  (message "locate.el: No files found.")
		  (kill-buffer (current-buffer)))
	  (end-of-buffer)
	  (delete-backward-char 1)
	  (beginning-of-buffer)
	  (locate-insert-file-mode))))


(defun locate-find-this-file ()
  "Find the file on the current line."
  (interactive)
  (let* ((bolpoint (save-excursion (beginning-of-line) (point)))
		 (eolpoint (save-excursion (end-of-line) (point)))
		 (buf (current-buffer))
		 (filnam (buffer-substring bolpoint eolpoint)))
	(kill-buffer buf)
	(find-file filnam)))

(defun locate-insert-this-file ()
  "Insert the file on the current line."
  (interactive)
  (let* ((bolpoint (save-excursion (beginning-of-line) (point)))
		 (eolpoint (save-excursion (end-of-line) (point)))
		 (buf (current-buffer))
		 (filnam (buffer-substring bolpoint eolpoint)))
	(kill-buffer buf)
	(insert-file filnam)))

(defun locate-mouse-find-this-file (e)
  "Find the file under mouse point."
  (interactive "e")
  (mouse-set-point e)
  (locate-find-this-file))

(defun locate-mouse-insert-this-file (e)
  "Insert the file under the mouse point."
  (interactive "e")
  (mouse-set-point e)
  (locate-insert-this-file))

(define-key locate-mode-map " " 'locate-find-this-file)
(define-key locate-mode-map "\r" 'locate-find-this-file)
(define-key locate-mode-map "i" 'locate-insert-this-file)
(define-key locate-mode-map [mouse-2] 'locate-find-this-file)
(define-key locate-mode-map [S-mouse-2] 'locate-insert-this-file)

(define-key locate-find-file-mode-map " " 'locate-find-this-file)
(define-key locate-find-file-mode-map "\r" 'locate-find-this-file)
(define-key locate-find-file-mode-map [mouse-2] 'locate-find-this-file)

(define-key locate-insert-file-mode-map " " 'locate-insert-this-file)
(define-key locate-insert-file-mode-map "\r" 'locate-insert-this-file)
(define-key locate-insert-file-mode-map [mouse-2] 'locate-insert-this-file)

(provide 'locate)
