;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; linenumbers.el
;; Written by: Ajay Shekhawat <ajay@cs.Buffalo.EDU>
;; Written sometime in 1989.
;; Released (unleashed? :-) on Wed Sep 23 10:47:46 EDT 1992
;; Copyright (c) 1992, Ajay Shekhawat <ajay@cs.Buffalo.EDU>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General Description.
;; Load this file, and the line number that you are on gets displayed in your
;; status line. It gets updated continuously as you move around (well, almost
;; always. Sometimes it fails, and hitting C-p/C-n fixes that). Please read
;; the History and Disclaimer below, *before* you start using it.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISCLAIMER: This file is provided as-is, with no warranties whatsoever,
;; either explicit or implied. Use at your own risk. 
;; - - - - - - - - - - - - - - 
;; Everyone is granted permission to copy, modify and redistribute
;; this code, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;; - - - - - - - - - - - - - - 
;;
;; History:  During my early Emacs days, I was sometimes in the need to see
;; the current line number while editing (continuosly). I hacked out this
;; piece of Elisp code (actually , "hack" is too strong a word. There's
;; almost nothing elegant in this code), to do the job. 
;; My demands were (and still are) limited, so I'm satisified by what this
;; does.
;; Some friends of mine here have found this to be pretty useful (not that
;; they have a choice ;-) , so upon their urging I'm releasing this to
;; the wide world, with the request that
;;    - if you have gripes about the way it is written, I don't want to hear
;;      about them. I know this code is pretty shoddy.
;;    - if you find a serious bug, _and_ can come up with a fix, I'd like
;;      to hear about it.
;;    - if you improve this code, or write your own with this functionality,
;;      please drop me a line.
;;   
;; I won't be modifying this further on my own (well, most probably not :-)),
;; so please don't send me mail asking for "a newer version". There won't be
;; any, atleast from me.  I'm sorry, but...
;;
;; Some known bugs:
;;       It updates the status line of all the buffers, regardless of which 
;; buffer you are in.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LCD Archive Entry:
;; linenumbers|Ajay Shekhawat|ajay@cs.Buffalo.EDU|
;; Continuous display of current line number in the status line|
;; 92-09-23|1.0|~/misc/linenumbers.el|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar global-line-numbers t "TRUE if we want line-numbers to be shown \
in every mode")

(setq mode-line-buffer-identification '("Emacs: %1b" "--" line-number "--"))

(setq global-mode-string '(""  display-time-string))

(defun what-line* ()
  "RETURN the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (concat " " (1+ (count-lines 1 (point))) " "))))

(setq line-number (what-line*))


(cond
  (global-line-numbers
    (or
;    (global-set-key "\C-?"             'backward-delete-char-untabify*)
     (global-set-key "\C-o"             'open-line*)
     (global-set-key "\e\C-o"           'split-line*)
     (global-set-key "\C-x\C-o"         'delete-blank-lines*)
     (global-set-key "\C-j"             'newline-and-indent*)
     (global-set-key "\C-cg"            'goto-line*)
     (global-set-key "\C-k"             'kill-line*)
     (global-set-key "\C-m"             'newline*)
     (global-set-key "\C-b"             'backward-char*)
     (global-set-key "\C-w"             'kill-region*)
     (global-set-key "\C-y"             'yank*)
     (global-set-key "\C-x["            'backward-page*)
     (global-set-key "\C-p"             'previous-line*)
     (global-set-key "\eb"              'backward-word*)
     (global-set-key "\e<"              'beginning-of-buffer*)
     (global-set-key "\ev"              'scroll-down*)
     (global-set-key "\ey"              'yank-pop*)
     (global-set-key "\C-v"             'scroll-up*)
     (global-set-key "\C-n"             'next-line*)
     (global-set-key "\C-f"             'forward-char*)
     (global-set-key "\C-x]"            'forward-page*)
     (global-set-key "\e>"             'end-of-buffer*)
     (global-set-key "\ef"             'forward-word*)))
  (t
    (or
;    (local-set-key "\C-?"             'backward-delete-char-untabify*)
     (local-set-key "\C-o"             'open-line*)
     (local-set-key "\e\C-o"           'split-line*)
     (local-set-key "\C-x\C-o"         'delete-blank-lines*)
     (local-set-key "\C-j"             'newline-and-indent*)
     (local-set-key "\C-cg"            'goto-line*)
     (local-set-key "\C-k"             'kill-line*)
     (local-set-key "\C-m"             'newline*)
     (local-set-key "\C-b"             'backward-char*)
     (local-set-key "\C-w"             'kill-region*)
     (local-set-key "\C-y"             'yank*)
     (local-set-key "\C-x["            'backward-page*)
     (local-set-key "\C-p"             'previous-line*)
     (local-set-key "\eb"              'backward-word*)
     (local-set-key "\e<"              'beginning-of-buffer*)
     (local-set-key "\ev"              'scroll-down*)
     (local-set-key "\ey"              'yank-pop*)
     (local-set-key "\C-v"             'scroll-up*)
     (local-set-key "\C-n"             'next-line*)
     (local-set-key "\C-f"             'forward-char*)
     (local-set-key "\C-x]"            'forward-page*)
     (local-set-key "\e>"             'end-of-buffer*)
     (local-set-key "\ef"             'forward-word*))))


(defun backward-char* (arg)
  "same as \\[backward-char] , but display line-number also."
  (interactive "p")
  (backward-char arg)
  (setq line-number (what-line*)))

(defun backward-page* (arg)
  "See backward-page for documentation.  "
  (interactive "p")
  (backward-page arg)
  (setq line-number (what-line*)))

(defun previous-line* (arg)
  "See previous-line for documentation. "
  (interactive "p")
  (previous-line arg)
  (setq line-number (what-line*)))

(defun backward-word* (arg)
  "See backward-word for documentation. "
  (interactive "p")
  (backward-word arg)
  (setq line-number (what-line*)))

(defun beginning-of-buffer* (arg)
  "See beginning-of-buffer for documentation. "
  (interactive "P")
  (beginning-of-buffer arg)
  (setq line-number (what-line*)))

(defun scroll-down* (arg)
  "See scroll-down for documentation. "
  (interactive "P")
  (scroll-down arg)
  (setq line-number (what-line*)))

(defun scroll-up* (arg)
  "See scroll-up for documentation. "
  (interactive "P")
  (scroll-up arg)
  (setq line-number (what-line*)))

(defun next-line* (arg)
  "See next-line for documentation. "
  (interactive "p")
  (next-line arg)
  (setq line-number (what-line*)))


(defun forward-char* (arg)
  "See forward-char for documentation. "
  (interactive "p")
  (forward-char arg)
  (setq line-number (what-line*)))

(defun forward-page* (arg)
  "See forward-page for documentation. "
  (interactive "p")
  (forward-page arg)
  (setq line-number (what-line*)))

(defun end-of-buffer* (arg)
  "See end-of-buffer for documentation. "
  (interactive "P")
  (message (concat " " arg))
  (end-of-buffer arg)
  (setq line-number (what-line*)))

(defun forward-word* (arg)
  "See forward-word for documentation. "
  (interactive "p")
  (forward-word arg)
  (setq line-number (what-line*)))

;(defun newline+ (arg)
;  " Redefine \\[backward-delete-char-untabify] , and then redefine itself"
;  (interactive "p")
;(cond
;  (global-line-numbers
;    (or 
;      (global-set-key "\C-?" 'backward-delete-char-untabify*)
;      (global-set-key "\C-m" 'newline*)))
;  (t 
;    (or
;      (local-set-key "\C-?" 'backward-delete-char-untabify*)
;      (local-set-key "\C-m" 'newline*))))
;  (global-set-key "\C-m" 'newline*)
;  (newline* arg))


(defun newline* (arg)
  "See newline for documentation. "
  (interactive "p")
  (newline arg)
  (setq line-number (what-line*)))



(defun open-line* (arg)
  " See open-line for documentation "
  (interactive "*p")
  (open-line arg)
  (setq line-number (what-line*)))

(defun split-line* (arg)
  "See split-line for documentation. "
  (interactive "*")
  (split-line arg)
  (setq line-number (what-line*)))

(defun delete-blank-lines* (arg)
  "See delete-blank-lines for documentation. "
  (interactive "*" )
  (delete-blank-lines arg)
  (setq line-number (what-line*)))

(defun newline-and-indent* ()
  "See newline-and-indent for documentation. "
  (interactive "*" )
  (newline-and-indent)
  (setq line-number (what-line*)))

(defun goto-line* (arg)
  "See goto-line for documentation. "
  (interactive "Ngoto? ")
  (goto-line arg)
  (setq line-number (what-line*)))

(defun kill-line* (arg)
  "See kill-line for documentation. "
  (interactive "*P")
  (setq this-command 'kill-line)
  (kill-line arg)
  (setq line-number (what-line*)))

(defun kill-region* (beg end)
  "See kill-region for documentation. "
  (interactive "*r")
  (setq this-command 'kill-region)
  (kill-region beg end)
  (setq line-number (what-line*)))

(defun yank-pop* (arg)
  "See yank-pop for documentation. "
  (interactive "*p")
  (setq this-command 'yank-pop)
  (yank-pop arg)
  (setq line-number (what-line*)))

(defun yank* (arg)
  "See yank for documentation. "
  (interactive "*P")
  (setq this-command 'yank)
  (yank arg)
  (setq line-number (what-line*)))

;;(defun backward-delete-char-untabify* (arg &optional killp)
;;  "See backward-delete-char-untabify for documentation. "
;;  (interactive "*p\nP")
;;  (backward-delete-char-untabify arg killp)
;;  (setq line-number (what-line*)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces, and display line-number
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if prefix arg is was specified."
  (interactive "*p\nP")
  (let ((count arg))
    (save-excursion
      (while (and (> count 0) (not (bobp)))
	(if (= (preceding-char) ?\t)
	    (let ((col (current-column)))
	      (forward-char -1)
	      (setq col (- col (current-column)))
	      (insert-char ?\ col)
	      (delete-char 1)))
	(forward-char -1)
	(setq count (1- count)))))
  (delete-backward-char arg killp)
  (setq line-number (what-line*)))
