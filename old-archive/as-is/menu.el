;Date: 15 Mar 89 21:59:12 GMT
;From: encore!mist@bu-cs.bu.edu  (Dan Pierson)
;Subject: Re: Clarification of pop-up menus
;To: info-gnu-emacs@prep.ai.mit.edu
;
;In article <MGH.89Mar14082614@intelob.intel.com>, mgh@intelob (Mike Heavin) writes:
;>
;>I posted a note earlier requesting information about using pop-up menus 
;>in gnumacs.  Unfortunately, I was not clear in expressing what I want.
;>
;>In the version of Unipress Emacs (blech, ptuy, gag...) they supported 
;>"pop-up" menus for dumb terminals.  When you invoked a menu, it called a
;>temporary buffer containing the possible menu selections.  The user then
;>moved the cursor to the appropriate line and "selected" it (I don't remember
;>whether in was a RETURN or SPACE), at which point emacs would execute the
;>function associated with the line.  The menu buffer would then go away.
;>
;
;Ah, now that we know what you mean...  I think that the following was
;part of the template-mode package.
;
;--------------cut----------------cut------------------cut-----------------
;;; menu.el
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'menu)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar menu-choice nil
  "Item selected from menu."
) ; menu-choice
(make-variable-buffer-local 'menu-choice)

(defvar menu-last-input ""
  "String used for searching through menu."
) ; menu-last-input
(make-variable-buffer-local 'menu-last-input)
(setq-default menu-last-input "")

(defvar menu-mode-map nil "Key-map for Menu Mode.")

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-mode ()
  "Major mode for selecting an item from a menu.
     Like View Mode, but with only the following commands:
     Space, C-n   menu-next-item
     Del, C-p     menu-previous-item
     a,b,...,z    menu-next-match
     Return, Linefeed, ESC C-c   exit.
   Returns the line selected."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (or menu-mode-map
	(menu-set-local-keys))
    (use-local-map menu-mode-map)
    (setq major-mode 'menu-mode)
    (setq mode-name "Menu")
    (setq case-fold-search t)
    (setq menu-last-input "")
					; Prompt for selection
    (message "Position on selection and exit with Return (or ESC-^c).")
					; Wait for user's selction
    (setq menu-choice nil)
    (unwind-protect
	(recursive-edit)
    ) ; unwind-protect
					; Return selection
    menu-choice
  ) ; let
) ; defun menu-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-abort-recursive-edit ()
  "Abort recursive edit in menu-mode gracefully."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (setq menu-choice nil)
    (exit-recursive-edit)
  ) ; let
) ; defun menu-abort-recursive-edit

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-exit-recursive-edit ()
  "Pick up selection and exit Menu-mode."
  (interactive)
					; Local Variables
  (let (start stop)
					; Body
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (setq menu-choice (buffer-substring start stop))
    (exit-recursive-edit)
  ) ; let
) ; defun menu-exit-recursive-edit

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-next-item ()
  "Go to the next item in the menu (wrap around at end)."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (end-of-line)
    (if (eobp)
      (beginning-of-buffer)
      (beginning-of-line 2)
    ) ; if
    (setq menu-last-input "")
  ) ; let
) ; defun menu-next-item

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-next-match ()
  "Find the next item with last-input-char leading character."
  (interactive)
					; Local Variables
  (let (stop)
					; Body
    (setq menu-last-input
	  (concat menu-last-input (char-to-string last-input-char)))
    (beginning-of-line)
    (if (not (re-search-forward (concat "^" menu-last-input) nil t))
      (progn
	(setq stop (point))
	(beginning-of-buffer)
	(if (not (re-search-forward (concat "^" menu-last-input) stop stop))
	  (progn
	    (message "No match for that character!")
	    (ding)
	    (setq menu-last-input "")
	  ) ; progn
	) ; if
      ) ; progn
    ) ; if
  ) ; let
) ; defun menu-next-match

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-previous-item ()
  "Go to the previous item in the menu (wrap around at beginning)."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (beginning-of-line)
    (if (bobp)
      (progn
	(end-of-buffer)
	(beginning-of-line)
      ) ; progn
      (beginning-of-line 0)
    ) ; if
    (setq menu-last-input "")
  ) ; let
) ; defun menu-previous-item

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-set-local-keys ()
  "Create key-map for Menu Mode."
					; Local Variables
  (let ()
					; Body
    (setq menu-mode-map (make-keymap))
    (suppress-keymap menu-mode-map)
    (define-key menu-mode-map "\C-g" 'menu-abort-recursive-edit)
    (define-key menu-mode-map "\C-j" 'menu-exit-recursive-edit) ; LFD
    (define-key menu-mode-map "\C-m" 'menu-exit-recursive-edit) ; CR
    (define-key menu-mode-map "\e\C-c" 'menu-exit-recursive-edit)
    (define-key menu-mode-map " " 'menu-next-item)
    (define-key menu-mode-map "\C-n" 'menu-next-item)
    (define-key menu-mode-map "\C-p" 'menu-previous-item)
    (define-key menu-mode-map "\177" 'menu-previous-item) ; DEL
    (define-key menu-mode-map "a" 'menu-next-match)
    (define-key menu-mode-map "b" 'menu-next-match)
    (define-key menu-mode-map "c" 'menu-next-match)
    (define-key menu-mode-map "d" 'menu-next-match)
    (define-key menu-mode-map "e" 'menu-next-match)
    (define-key menu-mode-map "f" 'menu-next-match)
    (define-key menu-mode-map "g" 'menu-next-match)
    (define-key menu-mode-map "h" 'menu-next-match)
    (define-key menu-mode-map "i" 'menu-next-match)
    (define-key menu-mode-map "j" 'menu-next-match)
    (define-key menu-mode-map "k" 'menu-next-match)
    (define-key menu-mode-map "l" 'menu-next-match)
    (define-key menu-mode-map "m" 'menu-next-match)
    (define-key menu-mode-map "n" 'menu-next-match)
    (define-key menu-mode-map "o" 'menu-next-match)
    (define-key menu-mode-map "p" 'menu-next-match)
    (define-key menu-mode-map "q" 'menu-next-match)
    (define-key menu-mode-map "r" 'menu-next-match)
    (define-key menu-mode-map "s" 'menu-next-match)
    (define-key menu-mode-map "t" 'menu-next-match)
    (define-key menu-mode-map "u" 'menu-next-match)
    (define-key menu-mode-map "v" 'menu-next-match)
    (define-key menu-mode-map "w" 'menu-next-match)
    (define-key menu-mode-map "x" 'menu-next-match)
    (define-key menu-mode-map "y" 'menu-next-match)
    (define-key menu-mode-map "z" 'menu-next-match)
  ) ; let
) ; defun menu-set-local-keys

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun menu-undefined ()
  "Catch all undefined keys for Menu Mode."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (ding)
    (message "C-n for next, C-p for previous, Return (or ESC-^c) to quit.")
    (setq menu-last-input "")
  ) ; let
) ; defun menu-undefined

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of menu.el
;-- 
;                                            dan

;In real life: Dan Pierson, Encore Computer Corporation, Research
;UUCP: {talcott,linus,necis,decvax}!encore!pierson
;Internet: pierson@encore.com

