;;; 8bits-mode.el --- An 8 bit mode (ISO 8859-1) for gnuemacs and likes

;; Copyright (C) 1993 Cedric BEUST

;; Author: Cedric BEUST (beust@sophia.inria.fr)
;; Version: $Id: 8bits-mode.el,v 1.4 93/12/20 09:47:18 beust Exp Locker: beust $
;; Keywords: iso, 8859-1, 8bits

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; 8bits-mode|Cedric BEUST|beust@sophia.inria.fr|
;; Mode to type 8 bit characters as diacritics|
;; 20-Dec-93|1.4|~/modes/8bits-mode.el|

;; Description:
;; This package will let you toggle with \C-c8 between a normal mode
;; and an 8-bit mode. When this mode is active, the modeline will display
;; 8bits in front of the current mode name (you might have to type a
;; character before the mode line gets updated).
;; Characters are typed as diacritics, e.g. e and ' will output the
;; e-acute character. See the variable 8bits-accents-table for a list
;; of all known accents.

;; Bugs:
;; Due to gnuemacs' poor unquoting capabilities, it seems impossible to
;; write install/uninstall functions that would retrieve the accents
;; from the variable 8bits-accents-table, thus the need to write them
;; explicitly in both functions. If you want to add accents to the
;; existing ones, don't forget to update the two functions 8bits-install-accents
;; and 8bits-uninstall-accents as well.
;;
;; The mode line won't update by itself (a ^L might be necessary).
;;
;; How to use :
;; Simply load this package. Add in your .emacs :
;; (load "8bits-mode")
;; and use "\C-c8" to toggle between normal and 8-bit mode



(defvar 8bits-mode-active-p t
  "True if 8bits mode is active")

(defvar 8bits-mode-name-prefix "8bits-"
  "Prefix that is appended at the start of the mode name")

;(defvar 8bits-accents-table '(
;   (?` (?e "\350") (?a "\340") (?u "\371"))
;   (?' (?a "\341") (?e "\351"))
;   (?^ (?e "\352") (?a "\342") (?u "\373") (?o "\364") (?i "\356"))
;   (?~ (?a "\343") (?n "\361"))
;   (?" (?a "\344") (?u "\374") (?e "\353") (?i "\357"))
;   (?, (?c "\347"))
;  )
;  "This table matches a diacritic entry (e.g. e ') with the corresponding 8 bits character"
;)

(defvar 8bits-accents-table
  '((?'
     (?A "\301")
     (?E "\311")
     (?I "\315")
     (?O "\323")
     (?U "\332")
     (?Y "\335")
     (?a "\341")
     (?e "\351")
     (?i "\355")
     (?o "\363")
;;     (?u "\372")
     (?y "\375")
     (?' "\264")
     (?  "'")) ; no special code
    (?`
     (?A "\300")
     (?E "\310")
     (?I "\314")
     (?O "\322")
     (?U "\331")
     (?a "\340")
     (?e "\350")
     (?i "\354")
     (?o "\362")
     (?u "\371")
     (?  "`")) ; no special code
    (?^
     (?A "\302")
     (?E "\312")
     (?I "\316")
     (?O "\324")
     (?U "\333")
     (?a "\342")
     (?e "\352")
     (?i "\356")
     (?o "\364")
     (?u "\373")
     (?  "^")) ; no special code
    (?\"
     (?A "\304")
     (?E "\313")
     (?I "\317")
     (?O "\326")
     (?U "\334")
     (?a "\344")
     (?e "\353")
     (?i "\357")
     (?o "\366")
     (?s "\337")
     (?u "\374")
     (?y "\377")
     (?  " \"") ; no special code
     (?\" "\250"))
    (?~
     (?A "\303")
     (?C "\307")
     (?D "\320")
     (?N "\321")
     (?O "\325")
     (?a "\343")
     (?c "\347")
     (?d "\360")
     (?n "\361")
     (?o "\365")
     (?> "\273")
     (?< "\253")
     (?  "~")) ; no special code
    (?\/
     (?A "\305") ;; A-with-ring (Norwegian and Danish)
     (?E "\306") ;; AE-ligature (Norwegian and Danish)
     (?O "\330")
     (?a "\345") ;; a-with-ring (Norwegian and Danish)
     (?e "\346") ;; ae-ligature (Norwegian and Danish)
     (?o "\370")
     (?  "/")) ; no special code
    (?,
     (?c "\347")))
  "This table matches a diacritic entry (e.g. e ') with the corresponding 8 bits character")

;; "

(defun 8bits-return-accent (accent letter)
  "Given an accent and a letter, return the appropriate string to insert (e.g. an 8-bit character if it is valid, or a string made of the letter and the accent)"
  (let ((accent-entry (assoc accent 8bits-accents-table))
       (8bits-entry ())
       (result (concat (char-to-string letter) (char-to-string accent))))
   (if accent-entry (progn
       (setq 8bits-entry (assoc letter accent-entry ))
       (if 8bits-entry
	   (setq result (car (cdr 8bits-entry))))
      ) ;; progn
    );; if
    result
  )
)

(defun 8bits-accent-expand (accent)
  (progn
    (backward-char 1)
    (insert-string (8bits-return-accent accent (following-char)))
    (delete-char 1)
  )
)

(defun 8bits-install-accents ()
  (local-set-key "`" '(lambda () (interactive) (8bits-accent-expand ?`)))
  (local-set-key "'" '(lambda () (interactive) (8bits-accent-expand ?')))
  (local-set-key "^" '(lambda () (interactive) (8bits-accent-expand ?^)))
  (local-set-key "~" '(lambda () (interactive) (8bits-accent-expand ?~)))
  (local-set-key "\"" '(lambda () (interactive) (8bits-accent-expand ?")))
  (local-set-key "/" '(lambda () (interactive) (8bits-accent-expand ?/)))
  (local-set-key "," '(lambda () (interactive) (8bits-accent-expand ?,)))
  t
)

(defun 8bits-uninstall-accents ()
  (local-set-key "`" 'self-insert-command)
  (local-set-key "'" 'self-insert-command)
  (local-set-key "^" 'self-insert-command)
  (local-set-key "~" 'self-insert-command)
  (local-set-key "\"" 'self-insert-command)
  (local-set-key "/" 'self-insert-command)
  (local-set-key "," 'self-insert-command)
  t
)

;; "

(defun 8bits-mode (&optional arg)
  "Switch between 8 bits mode and normal mode. If arg given and positive, force 8 bits mode on"
  (interactive "P")
  (let 
      ((on-p (if (and arg (> (prefix-numeric-value arg) 0))
		 ()
	       (not 8bits-mode-active-p))))
    (if on-p
      (progn
	(if (string-match 8bits-mode-name-prefix mode-name)
	    (setq mode-name (substring mode-name
				       (length 8bits-mode-name-prefix)
				       (length mode-name))))
	(setq 8bits-mode-active-p t)
	(8bits-uninstall-accents)
      )
      (progn
	(setq 8bits-mode-active-p ())
	(if (not (string-match 8bits-mode-name-prefix mode-name))
	    (setq mode-name (concat 8bits-mode-name-prefix mode-name)))
	(8bits-install-accents)
	)
    ) ;; if
  ) ;; let
  t
)

(global-set-key "\C-c8" '(lambda () (interactive) (8bits-mode 0)))
