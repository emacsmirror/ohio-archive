;;; File: mastermind.el
;;; Author: Anders Holst  (aho@sans.kth.se)
;;; Copyright (C) Anders Holst 1992
;;;
;;; LCD Archive Entry:
;;; mastermind|Anders Holst|aho@thalamus.sans.kth.se|
;;; The mastermind game.|
;;; 16-Mar-1993||~/games/mastermind.el.Z|
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
;;; along with your copy of Emacs; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; --------------------------------------------------------------------------

;;
;;  DESCRIPTION
;;
;;  This is a simple mastermind game in Emacs.
;;  Start with "M-x mastermind".  A numeric argument determines how
;;  many positions to guess (default is 4).  The variable
;;  `mastermind-max-color' desides the number of possible "colors", or
;;  numbers (defaulted to 6).  Maximum of allowed numbers is 10, where
;;  "0" is considered as the tenth alternative.
;;  The answer for each guessed line is presented as the number of
;;  completely correct positions, followed by the number of correct
;;  numbers at wrong positions.
;;  


(defvar mastermind-mode-map nil "Local keymap for mastermind mode")

(defvar mastermind-max-color 6
  "*The number of different possible \"colors\", i.e. numbers used (max 9).")

(if mastermind-mode-map
    ()
  (setq mastermind-mode-map (make-keymap))
  (suppress-keymap mastermind-mode-map t)
  (define-key mastermind-mode-map "\C-f" 'mm-right)
  (define-key mastermind-mode-map "\C-b" 'mm-left)
  (define-key mastermind-mode-map "\C-p" 'mm-up)
  (define-key mastermind-mode-map "\C-n" 'mm-down)
  (define-key mastermind-mode-map "\C-e" 'mm-eol)
  (define-key mastermind-mode-map "\C-a" 'mm-bol)
  (define-key mastermind-mode-map "\e<" 'mm-top)
  (define-key mastermind-mode-map "\e>" 'mm-bot)
  (define-key mastermind-mode-map " "    'mm-space)
  (define-key mastermind-mode-map "\177"   'mm-bsp)
  (define-key mastermind-mode-map "0"    'mm-set-number)
  (define-key mastermind-mode-map "1"    'mm-set-number)
  (define-key mastermind-mode-map "2"    'mm-set-number)
  (define-key mastermind-mode-map "3"    'mm-set-number)
  (define-key mastermind-mode-map "4"    'mm-set-number)
  (define-key mastermind-mode-map "5"    'mm-set-number)
  (define-key mastermind-mode-map "6"    'mm-set-number)
  (define-key mastermind-mode-map "7"    'mm-set-number)
  (define-key mastermind-mode-map "8"    'mm-set-number)
  (define-key mastermind-mode-map "9"    'mm-set-number)
  (define-key mastermind-mode-map "\C-m" 'mm-make-guess))


;; Mastermind mode is suitable only for specially formatted data.
(put 'mastermind-mode 'mode-class 'special)

(defun mastermind-mode ()
  "Major mode for playing mastermind.

Number -- set number in the position in a line
SPC    -- blank the number in the position
DEL    -- blank the number in the previous position
RET    -- make a guess on the line

The answer will be presented as the number of correctly placed numbers
followed by the number of correct numbers incorrectly placed.

Precisely,\\{mastermind-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mastermind-mode-map)
  (setq truncate-lines t)
  (setq major-mode 'mastermind-mode)
  (setq mode-name "Mastermind"))

(defun mastermind (num)
  "Play mastermind.  NUM is number of positions (default 4)."
  (interactive "P")
  (switch-to-buffer "*Mastermind*")
  (mastermind-mode)
  (setq buffer-read-only t)
  (buffer-flush-undo (current-buffer))
  (setq mm-len (or num 4))
  (setq mm-answer (mm-init-line mm-len))
  (setq mm-guess ())
  (setq mm-check (make-list mm-len ()))
  (setq mm-guesses 0)
  (setq mm-pos 0)
  (setq mm-done ())
  (mm-clear-buffer)
  (mm-blank-line)
  (mm-new-line))

(defun mm-abs (num)
  (if (>= num 0)
      num
      (- num)))

(defun mm-init-line (num)
  (random t)
  (let ((line ()))
    (while (>= (setq num (1- num)) 0)
      (setq line (cons (1+ (mod (mm-abs (random)) 
				mastermind-max-color)) line)))
    line))

(defun mm-clear-buffer ()
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun mm-new-line ()
  (let (i (buffer-read-only nil))
    (insert "\n\n   ")
    (setq i mm-len)
    (while (>= (setq i (1- i)) 0)
      (insert "- "))
    (setq mm-guess (make-list mm-len ()))
    (mm-goto 0)))

(defun mm-blank-line ()
  (let ((buffer-read-only nil))
    (insert (make-string (+ (* 2 mm-len) 3) 32))))

(defun mm-goto (pos)
  (setq mm-pos (max 0 (min mm-len pos)))
  (goto-char (point-max))
  (beginning-of-line)
  (forward-char (+ (* mm-pos 2) 3)))

(defun mm-in-pos ()
  (= (+ (point-max) (* mm-pos 2) (- (* mm-len 2))) (point)))

(defun mm-right ()
  "Move point to next position on line."
  (interactive)
  (if (= mm-pos mm-len)
      ()
    (forward-char 2)
    (setq mm-pos (1+ mm-pos))))

(defun mm-left ()
  "Move point to previous position on line."
  (interactive)
  (if (= mm-pos 0)
      ()
    (backward-char 2)
    (setq mm-pos (1- mm-pos))))

(defun mm-up ()
  "Move point up one line."
  (interactive)
  (previous-line 2))

(defun mm-down ()
  "Move point down one line."
  (interactive)
  (next-line 2))

(defun mm-eol ()
  "Move point to end of the line with the current guess."
  (interactive)
  (mm-goto mm-len))

(defun mm-bol ()
  "Move point to beginning of the line with the current guess."
  (interactive)
  (mm-goto 0))

(defun mm-top ()
  "Move to the first line in buffer"
  (interactive)
  (goto-char (+ (* mm-pos 2) 4)))

(defun mm-bot ()
  "Move to the line with the current guess"
  (interactive)
  (mm-goto mm-pos))

(defun mm-set-pos (num)
  (let ((buffer-read-only nil))
    (mm-goto mm-pos)
    (cond (mm-done
	   (message "Game over! Do `M-x mastermind' to start a new game")
	   (ding))
	  ((= mm-pos mm-len)
	   ())
	  ((null num)
	   (setcar (nthcdr mm-pos mm-guess) ())
	   (delete-char 1)
	   (insert-char 45 1)
	   (backward-char 1))
	  (t
	   (setcar (nthcdr mm-pos mm-guess) num)
	   (delete-char 1)
	   (insert-char (+ 48 num) 1)
	   (backward-char 1)))))

(defun mm-space ()
  "Blank the guess at the current position and move to the next position"
  (interactive)
  (mm-set-pos ())
  (mm-right))

(defun mm-bsp ()
  "Move to the previous position and blank the guess there"
  (interactive)
  (if (not (= mm-pos 0))
      (progn 
	(mm-left)
	(mm-set-pos ()))))

(defun mm-set-number ()
  "Set the invoking number in the current position, and move right"
  (interactive)
  (let ((num (- (string-to-char (this-command-keys)) ?0)))
    (if (and (<= num mastermind-max-color)
	     (or (= 10 mastermind-max-color)
		 (> num 0)))
	(progn
	  (mm-set-pos num)
	  (mm-right))
	(progn
	  (message (format "%d is not an allowed number. Max is %d" 
			   num mastermind-max-color))
	  (ding)))))

(defun mm-nil-pos ()
  (let ((tmp (memq () mm-guess)))
    (if tmp
	(- mm-len (length tmp)))))

(defun mm-make-guess ()
  "Make a guess on the last written line.
The number of correctly placed numbers, followed by the number of
incorrectly placed numbers are written after the line."
  (interactive)
  (let (tmp (buffer-read-only ()))
    (cond (mm-done
	   (message "Game over! Do `M-x mastermind' to start a new game")
	   (ding))
	  ((setq tmp (mm-nil-pos))
	   (mm-goto tmp))
	  ((not (mm-in-pos))
	   (mm-goto mm-pos))
	  ((setq tmp (mm-check-guess))
	   (mm-goto mm-len)
	   (setq mm-guesses (1+ mm-guesses))
	   (insert (concat "  !  " 
			   (int-to-string (car tmp))
			   "  "
			   (int-to-string (cdr tmp))))
	   (if (not (= (car tmp) mm-len))
	       (mm-new-line)
	       (progn
		 (insert "\n\n")
		 (mm-blank-line)
		 (mm-goto 0)
		 (insert (format "Correct. It took %d guesses." mm-guesses))
		 (mm-goto 0)
		 (setq mm-done t)))))))

(defun mm-check-guess ()
  (let (i j (res1 0) (res2 0))
    (setq i mm-len)
    (while (>= (setq i (1- i)) 0)
      (setcar (nthcdr i mm-check) ()))
    (setq i mm-len)
    (while (>= (setq i (1- i)) 0)
      (if (= (nth i mm-guess) (nth i mm-answer))
	  (progn
	    (setq res1 (1+ res1))
	    (setcar (nthcdr i mm-check) t))))
    (setq i mm-len)
    (while (>= (setq i (1- i)) 0)
      (if (not (= (nth i mm-guess) (nth i mm-answer)))
	  (progn
	    (setq j mm-len)
	    (while (>= (setq j (1- j)) 0)
	      (if (and (not (nth j mm-check))
		       (not (= i j))
		       (= (nth i mm-guess) (nth j mm-answer)))
		  (progn
		    (setq res2 (1+ res2))
		    (setcar (nthcdr j mm-check) t)
		    (setq j 0)))))))
    (cons res1 res2)))
