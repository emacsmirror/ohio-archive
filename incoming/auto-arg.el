;;; auto-arg-mode.el -- Minor mode that makes numbers prefixes by default.

;; Copyright (C) 1998 Anders Lannerbäck

;; Author:  Anders Lannerbäck <andersl@altavista.net> or <f95-ala@nada.kth.se>
;; Created: 4 Sep 1998
;; Version: 0.6 
;; Keywords: Prefix minor mode  


;; Auto-arg-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later version.

;; Auto-arg-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;;; Commentary:

;; This is a minor mode for people who uses numbers as prefixes more often than
;; to insert them in text. With this minor mode entering a number is roughly
;; equivalent to entering C-u <number>.

;; The following table probably is the best way to explain how it works. The
;; result of entering the following key sequences are equal:

;; auto-arg mode on                      auto-arg mode off
;; =======================================================

;;  "4 2 <SPC>"                            "4 2"
;;  "4 2 p"                                "C-u 4 2 p"
;;  "4 2 M-d"                              "C-u 4 2 M-d"

;; (and if auto-arg-backspace-enabled is non-nil (default))
 
;;  "4 2 <DEL> 3 p"                         "C-u 4 3 p"
;;  "4 2 <DEL> <DEL> <DEL>"                 "<DEL>"

;; (if auto-arg-backspace-enabled is nil)

;;  "4 2 <DEL>"                            "C-u 4 2 <DEL>"

;; (if auto-arg-self-insert-terminates is t)

;; "4 2 p"                                  "4 2 p"
;; "4 2 <SPC>"                              "4 2 <SPC>"
;; "4 2 M-d"                                "C-u 4 2 M-d"

;; If auto-arg-is-omnipresent is non-nil then auto-arg will will alter (a copy
;; of) the global map instead of acting as a minor mode. The original map can be
;; restored with auto-arg-restore-global-map.

;;; Bugs

;; This is not really a bug, but a limitation: <SPC> and <DEL> are hardcoded to
;; do (insert " ") and (backward-delete-char-untabify) respectively. This makes
;; auto-arg mode cooperate badly with view mode and other modes that have "non
;; standard" functions bound to <SPC> and <DEL>. I will meditate over this
;; problem. 

;; Also, it will probably not work with xemacs. I don't have xemacs so I can't
;; write an xemacs-version.

;;; Change log:
;; 13 Sep 1998 version 0.6 -- Changed from using local maps to using
;;                            minor-mode keymaps with easy-mmode.
;; 11 Sep 1998 version 0.5 -- Added auto-arg-is-omnipresent and
;;                            auto-arg-self-insert-terminates.
;; 7 Sep 1998 version 0.4 -- Major bug fixes and general improvements.
;; 5 Sep 1998 version 0.3 -- Added backspace & improved(?) docs.
;; 5 Sep 1998 version 0.2.1 -- fixed typo in version number :)
;; 5 sep 1998 version 0.2  --  total rewrite.
;; 4 sep 1998 version 0.1


;;; Code:

(defvar auto-arg-is-omnipresent nil
"If t auto-arg alters the global keymap instead of acting as a minor mode.
In fact it alters a copy of the current global map. The original map can be
resored with auto-arg-restore-global-map")

(defvar auto-arg-mode-modeline-string " auto-arg")

(defvar auto-arg-map nil "The keymap used by auto-arg-mode")

(defvar auto-arg-original-map nil 
"This is the original keymap that is restored when exiting auto-arg-mode.")

(defvar auto-arg-backspace-enabled t 
"Bind <DEL> to auto-arg-backspace.
Setting auto-arg-backspace-enabled to
\"t\" (default) makes <DEL> delete a digit 
in the number. Setting it to \"nil\"
makes <DEL> repeat number times.")

(defvar auto-arg-self-insert-terminates nil
"Makes all \"normal\" keys insert sequence and themselves.")

(defun auto-arg-make-keymap ()
  "Copies the original keymap and alters the copy."
  (let ((original-map 
	 (if auto-arg-is-omnipresent
		(current-global-map)
	   (make-sparse-keymap))))
    (setq auto-arg-original-map (copy-keymap original-map)
	auto-arg-map (copy-keymap auto-arg-original-map))
  (if auto-arg-self-insert-terminates
      (substitute-key-definition 'self-insert-command
				 'auto-arg-self-insert-and-terminate
				 auto-arg-map
				 global-map)
    (define-key auto-arg-map " " 'auto-arg-terminate))
  (define-key auto-arg-map "1" 'auto-arg-prefixer)
  (define-key auto-arg-map "2" 'auto-arg-prefixer)
  (define-key auto-arg-map "3" 'auto-arg-prefixer)
  (define-key auto-arg-map "4" 'auto-arg-prefixer)
  (define-key auto-arg-map "5" 'auto-arg-prefixer)
  (define-key auto-arg-map "6" 'auto-arg-prefixer)
  (define-key auto-arg-map "7" 'auto-arg-prefixer)
  (define-key auto-arg-map "8" 'auto-arg-prefixer)
  (define-key auto-arg-map "9" 'auto-arg-prefixer)
  (define-key auto-arg-map "0" 'auto-arg-prefixer)
  (if auto-arg-backspace-enabled
      (define-key auto-arg-map [backspace] 'auto-arg-backspace))))

(if auto-arg-is-omnipresent
    (progn
      (auto-arg-make-keymap)
      (use-global-map auto-arg-map))
  (progn 
    (auto-arg-make-keymap)
    (easy-mmode-define-minor-mode 
     auto-arg-mode
     "Toggle auto-arg mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

Auto-arg mode makes number prefixes by default."
     ;; initial value
     nil
     auto-arg-mode-modeline-string
     auto-arg-map)))
     
(defun auto-arg-prefixer (arg)
  (interactive "P")
  (let ((n (string-to-number (this-single-command-keys))))
    (if (string-match " *Minibuf" (buffer-name))
	(self-insert-command 1)
      (message "%d"
	       (setq prefix-arg
		   (if arg
		       (+ (* arg 10) n)
		     n))))))

(defun auto-arg-terminate (arg)
  (interactive "P")
  (if arg
      (insert (number-to-string arg))
    (insert (this-command-keys))))

(defun auto-arg-self-insert-and-terminate (arg)
  (interactive "P")
  (if arg
	(insert (concat 
		 (number-to-string arg)
		 (this-single-command-keys)))
    (insert (this-command-keys))))

(defun auto-arg-backspace (arg)
  (interactive "P")
  (if arg
      (progn
	(setq prefix-arg
	      (if (> arg 10)
		  (floor (/ arg 10))
		nil))
	(if prefix-arg (message "%d" prefix-arg)))
    (backward-delete-char-untabify 1)))

(defun auto-arg-restore-global-map ()
  "Restores global map."
  (if auto-arg-is-omnipresent
      (use-global-map auto-arg-original-map)))

;;; auto-arg.el ends here


