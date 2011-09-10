;;;    $Id: hebrew.el,v 1.4 1993/11/24 14:29:08 alexr Exp alexr $

;;; $RCSfile: hebrew.el,v $ -- Simple hebrew support
;;; Copyright (C) 1993 Alexander Rezinsky <alexr@msil.sps.mot.com>
;;;
;;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Created:  24 Nov 1993
;;; Version:  1.0
;;; Keywords: hebrew language
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; COMMENTARY
;;; ----------
;;;   This package provide very simple hebrew (may be any other
;;;   right-to-left language ??)  support. Unlike existing package
;;;   misc/hebrew.tar.Z (see elisp archive) it does not requires any
;;;   patches in emacs modules and it is very-very simple. This package
;;;   assumes that you already have hebrew font for your computer (see
;;;   misc/hebrew.tar.Z to X11 hebrew fonts).  This package uses only
;;;   one key that toggles hebrew mode.  Hebrew coding and keyboard
;;;   binding are Israel's standard, but you can redefine it.  Digits
;;;   and any other special characters are left-to-right.
;;;

;;; INSTALLATION
;;; ------------
;;;   Put this file in your load-path and insert the following in .emacs
;;;   
;;;   (autoload 'hebr-switch  "hebrew"  "Toggle Hebrew mode.")
;;;   
;;;   And define your hebrew-toggle key as follow:
;;;   
;;;   (global-set-key [?\A-a] 'hebr-switch)

;;; USAGE
;;; -----
;;;   Now use key which you defined to toggle hebrew mode (in our example
;;;   Alt-a). It is one additional function "hebrew-sort-lines". It works
;;;   exactly as "sort-lines", but sorting is done in right-to-left order.

;;; CUSTOMIZATION
;;; -------------
;;;   You can redefine hebrew coding by redifinition variables "alef",
;;;   "bet", "gimel" etc. in your .emacs file. For example:
;;;   (setq alef ?\260)
;;;   (setq bet  ?\261)
;;;    ....
;;;   (setq tav  ?\312)
;;;   
;;;   If you want redefine keyboard binding, define two variables
;;;   "hebrew-from-table" and "hebrew-to-table" in your .emacs
;;;   file. In "hebrew-from-table" must be list of native keyboard
;;;   codes and in "hebrew-to-table" must be list of target codes.
;;;   These lists must have same length. See definition of these
;;;   variables below for details.

;;; KNOWN BUGS
;;; ----------
;;;  Because of stupid method of Hebrew sorting, function
;;;  "hebrew-sort-lines" is slow. In case very large file (great than
;;;  ~ 1M) this function may not work properly.

;;; LCD Archive Entry:
;;; simple-hebrew|Alex Rezinsky|alexr@msil.sps.mot.com|
;;; Simple Hebrew language support.|
;;; 24-Nov-1993|1.0|~/misc/hebrew.el.Z|

;;; TODO LIST
;;; ---------
;;;   1. More intelligent "return" key processing.
;;;   2. Filling and justifying region in right-to-left style.
;;;   3. It is very stupid method of right-to-left sorting (see below).
;;;      Any ideas ?
;;;   4. Must be difference between digits (always left-to-right) and 
;;;      punctuation characters (in right-to-left mode also right-to-left).
;;;   5. Must be two modes - Hebrew insertion in English text, and English
;;;      insertion in Hebrew text (now this package works in "Hebrew 
;;;      insertion in English text").

;;; HISTORY
;;; -------
;;; v1.0 November 24 1993 Alex Rezinsky
;;;   First release.


;;; Code:

;;
;; Here default hebrew code table is defined
;;
(defvar alef   ?\200)
(defvar bet    ?\201)
(defvar gimel  ?\202)
(defvar dalet  ?\203)
(defvar hej    ?\204)
(defvar vav    ?\205)
(defvar zain   ?\206)
(defvar het    ?\207)
(defvar tet    ?\210)
(defvar jud    ?\211)
(defvar kafs   ?\212)
(defvar kaf    ?\213)
(defvar lamed  ?\214)
(defvar mems   ?\215)
(defvar mem    ?\216)
(defvar nuns   ?\217)
(defvar nun    ?\220)
(defvar sameh  ?\221)
(defvar ain    ?\222)
(defvar pejs   ?\223)
(defvar pej    ?\224)
(defvar tzadis ?\225)
(defvar tzadi  ?\226)
(defvar kuv    ?\227)
(defvar resh   ?\230)
(defvar shin   ?\231)
(defvar tav    ?\232)
(defvar kav-nt ?/)
(defvar psik   ?,)
(defvar nakuda ?.)
(defvar geresh ?')

;;
;; Here  default hebrew keyboard mapping is defined.
;;
(defvar hebrew-from-table 
  (list
    ?q     ?w     ?e     ?r     ?t     ?y     ?u     ?i     ?o     ?p
    ?a     ?s     ?d     ?f     ?g     ?h     ?j     ?k     ?l     ?;     ?'
    ?z     ?x     ?c     ?v     ?b     ?n     ?m     ?,     ?.     ?/
    ?\040
  )
)
(defvar hebrew-to-table
  (list
    kav-nt geresh kuv    resh   alef   tet    vav    nuns   mems   pej
    shin   dalet  gimel  kaf    ain    jud    het    lamed  kafs   pejs   psik
    zain   sameh  bet    hej    nun    mem    tzadi  tav    tzadis nakuda
    ?\040
  )
)


;;
;;=======================================================================
;; Tables and variables setup
;;=======================================================================
;;
(defvar hebrew-table () "Hebrew keyboard translation table.")
(if hebrew-table
    ()
  (setq hebrew-table (make-string 256 0))
  (let ((i 0))
    (while (< i 256)
      (aset hebrew-table i i)
      (setq i (1+ i))))
  (let ((from hebrew-from-table) (to hebrew-to-table))
    (while from
      (aset hebrew-table (car from) (car to))
      (setq from (cdr from))
      (setq to (cdr to)))))

(defvar hebrew-mode-map ()
  "Keymap for hebrew.")
(if hebrew-mode-map
    ()
  (setq hebrew-mode-map (make-keymap))
  (define-key hebrew-mode-map "\C-m" 'return-hebrew)
  (let ((from hebrew-from-table) (to hebrew-to-table))
    (while from
      (define-key hebrew-mode-map (char-to-string (car from)) 'cinsert-hebrew)
      (setq from (cdr from))
      (setq to (cdr to)))))

(or (assq 'hebrew-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(hebrew-mode " Hebrew") minor-mode-alist)))
(or (assq 'hebrew-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	   (cons (cons 'hebrew-mode hebrew-mode-map) minor-mode-map-alist)))


;;
;;              HEBREW MODE TOGGLE
;;
(defun hebr-switch ()
  (interactive nil)
  (if (boundp 'hebrew-mode)
      (setq hebrew-mode (not hebrew-mode))
    (make-local-variable 'hebrew-mode)
    (setq hebrew-mode t))
  (force-mode-line-update)
)

;;
;;              HEBREW INSERTION
;;
(defun cinsert-hebrew (rpt)
  (interactive "p")
  (let ((i rpt) (dec-char (aref hebrew-table last-input-char)))
    (while (> i 0)
      (if overwrite-mode
        (progn                      ;;; -- OVERWRITE MODE
          (if (and (/= (following-char) ?\n) (/= (point) (point-max)))
            (delete-char 1)
          )
          (insert-char dec-char 1)
          (backward-char 1)
          (if (or (= (preceding-char) ?\n) (= (point) 1))
            (insert-char ?  1)
          )
          (backward-char 1)
        )
        (progn                      ;;; -- INSERT MODE
          (insert-char dec-char 1)
          (backward-char 1)
        )
      )
      (setq i (1- i))
    )
  )
)


;;
;;              HEBREW RETURN
;;
(defun return-hebrew (rpt)
  (interactive "p")
  (let ((cur-point (point)))
    (beginning-of-line)
    (let ((first-part (buffer-substring (point) cur-point)))
      (delete-char (- cur-point (point)))
      (end-of-line)
      (newline rpt)
      (insert first-part)
    )
  )
)


;;--------------------------------------------------------
;;
;;              HEBREW SORTING
;;
(defun hebrew-sort-lines (reverse beg end)
  "Sort lines in region alphabetically, right-to-left; 
argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "P\nr")
  (require 'sort)
  (save-restriction
    (narrow-to-region beg end)
    (revers-buffer)
    (goto-char (point-min))
    (sort-subr reverse 'forward-line 'end-of-line)
    (goto-char (point-min))
    (revers-buffer)
  )
)

(defun revers-buffer ()
  (goto-char (point-min))
  (let ((begl 0)(endl 0)(not-end t))
    (while not-end
      (setq begl (point))
      (end-of-line)
      (setq endl (point))
      (if (/= begl endl)
        (let ((str (buffer-substring begl endl)))
          (kill-region begl endl)
          (insert (revers-string str))
        )
      )
      (setq not-end (eq (forward-line 1) 0))
    )
  )
)

(defun revers-string (str)
  (if (< (length str) 2)
    str
    (concat (revers-string (substring str 1)) (substring str 0 1))
  )
)

;;; hebrew ends here.
