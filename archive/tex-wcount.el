;;; tex-wcount.el --- keep a running count of words in a latex buffer

;; Emacs Lisp Archive Entry
;; Filename: tex-wcount.el
;; Author: Seb James <ssj20@cam.ac.uk>
;; Maintainer: Seb James <ssj20@cam.ac.uk>
;; Created: 30 Mar 2001
;; Description: Use detex to place a word count in the modeline.
;; Version 1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `tex-wcount-mode' is a minor mode that puts into the modeline a running
;; count of the words in a latex buffer.  It uses the program detex, which must
;; be installed on the  host system. The mode runs tex-wcount-count when
;; Emacs has been idle for `tex-wcount-idleness' seconds, or when the user
;; presses C-c w (`tex-wcount-count'). detex uses the option -l so the word
;; count returned is only accurate for a latex file. This could be changed to -t
;; to modify the program to count words in a tex file.
;;
;; tex-wcount-count was modified from wcount.el, which was written by 
;; Bob Glickstein <bobg@zanshin.com>. 
;; It was a mode which did a word count using the forward-word function.

;;; Code:

(require 'timer)
(require 'assoc)

(defvar tex-wcount-mode nil)
(make-variable-buffer-local 'tex-wcount-mode)

(defvar tex-wcount-idleness 10
  "*Seconds of idle time before re-counting words in the buffer.")

(defvar tex-wcount-timer nil)
(make-variable-buffer-local 'tex-wcount-timer)

(defvar tex-wcount-mode-line-data nil)
(make-variable-buffer-local 'tex-wcount-mode-line-data)  

(aput 'minor-mode-alist 'tex-wcount-mode '(tex-wcount-mode-line-data))

(defvar tex-wcount-mode-map nil)

(if (null tex-wcount-mode-map)
    (progn
      (setq tex-wcount-mode-map (make-sparse-keymap))
      (define-key tex-wcount-mode-map "\C-cw" 'tex-wcount-count)))

(aput 'minor-mode-map-alist 'tex-wcount-mode tex-wcount-mode-map)

;; The following appends the instruction to the kill-buffer-hook that
;; if the mode is tex-wcount-mode then the tex-wcount-timer has to be
;; switched off.
(add-hook 'kill-buffer-hook
	  (lambda()
	    (if tex-wcount-mode
		(cancel-timer tex-wcount-timer))))


(defun tex-wcount-mode (&optional arg)
  "Keep a running count of this buffer's words in the mode line."
  (interactive "P")
  (setq tex-wcount-mode
        (if (null arg)
            (not tex-wcount-mode)
          (> (prefix-numeric-value arg) 0)))
  (if (timerp tex-wcount-timer)
      (cancel-timer tex-wcount-timer))
  (if tex-wcount-mode
      (setq tex-wcount-timer (run-with-idle-timer tex-wcount-idleness t 
						  'tex-wcount-count
						  (current-buffer))
            tex-wcount-mode-line-data "   Word count: ?  ")))

(defun tex-wcount-count (&optional buffer)
  "count number of words in the current buffer by detex-ing first the
wc-ing. This function requires detex to be installed on your
system. Of course, wc also needs to be installed, though this is
present on most systems"
  (interactive)
  ;; Set buffer to current buffer or the one specified:
  (set-buffer (or buffer (current-buffer)))


  ;; Now we do the counting words bit - cal detex and wc on the current buffer
  (shell-command-on-region (point-min) (point-max) "detex -l | wc -w")
  (let ((words 
	 ;; Now get the result, in buffer *Shell Command Output*
	 (save-excursion
	   (set-buffer "*Shell Command Output*")
	   (buffer-substring (+ (point-min) 1) (- (point-max) 1)))))
    ;; Remove message from minibuffer [see `To do']
    (message nil)
    ;; Finally set the buffer local varible tex-wcount-mode-line-data:
    (setq tex-wcount-mode-line-data (concat "  Word count:" words " "))))



(provide 'tex-wcount)

;;; tex-wcount.el ends here

