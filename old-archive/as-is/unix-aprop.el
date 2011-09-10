;;; unix-apropos command

;; USE:
;;   M-x unix-apropos
;; prompts for keywords, and simulates man -k.  Pops up window of possible
;; unix documentation.  Move cursor to desired command and hit RETURN.

;;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; MODIFICATION HISTORY:
 
;;; 9/1/88, created by  Henry Kautz
;;;                     uucp: allegra!kautz
;;; 9/2/88, H. Kautz eliminates use of man -k, added additional keywords,
;;;         added default topic based on word under cursor
;;; 9/6/88, H. Kautz added scroll-other-window, and chopped -ucb from
;;;         section numbers

(provide 'unix-apropos)

(global-set-key "\C-hu" 'unix-apropos)

(defvar unix-apropos-map nil)

(defvar whatis-file-name "/usr/lib/whatis")

(defun unix-apropos-get-man ()
   "Get the manual entry for the current line"

(interactive)
   (let (topic section)
      (interactive)
      (beginning-of-line 1)
      (looking-at "\\w*")
      (setq topic (buffer-substring (match-beginning 0) (match-end 0)))
      (re-search-forward "(\\([^)]*\\))")
      (setq section (downcase (buffer-substring
				 (match-beginning 1) (match-end 1))))
      (setq section                     ; throw out "-ucb" or "-whatever"
            (substring section 0 (string-match "\-" section)))
      (beginning-of-line 1)
      (manual-entry topic section)
      )
   )

(defun unix-apropos-expunge ()
   "kill all the Unix man buffers"
   (interactive)
   (mapcar (function (lambda (b)
			(if (string-match "^\\*Manual" (buffer-name b))
			   (kill-buffer b))))
      (buffer-list))
   (delete-window))

(defun current-word ()
   "word cursor is over as a string"
   (save-excursion
      (let (beg end)
	 (re-search-backward "\\w" nil 2)
	 (re-search-backward "\\b" nil 2)
	 (setq beg (point))
	 (re-search-forward "\\w*\\b" nil 2)
	 (setq end (point))
	 (buffer-substring beg end))))

(defun unix-apropos (topic)
   "Display apropos for TOPIC"
   (interactive (list
		   (read-string (concat "Unix apropos ("
				       (current-word) "): "))))
   (if (equal "" topic) (setq topic (current-word)))
   (pop-to-buffer "*Manual Apropos*")
   (unix-apropos-mode))

(defun unix-apropos-additional (additional)
   (interactive (list
		   (read-string (concat "Additional keyword ("
				       (current-word) "): "))))
   (if (equal "" additional) (setq additional (current-word)))
   (goto-char (point-min))
   (setq buffer-read-only nil)
   (delete-non-matching-lines additional)
   (setq buffer-read-only t))

(defun unix-apropos-mode ()
   "<return> get manual entry; n = next line; p = previous line
x = expunge man page buffers; u = unix apropos; h = describe mode
a = constrain search by an additional keyword
<space> scroll other window; <delete> reverse scroll other window"
   (setq buffer-read-only nil)
   (erase-buffer)
   (buffer-flush-undo (current-buffer))
   (if (null unix-apropos-map)
      (progn
	 (setq unix-apropos-map (make-sparse-keymap))
	 (define-key unix-apropos-map "\C-M" 'unix-apropos-get-man)
	 (define-key unix-apropos-map "n" 'next-line)
         (define-key unix-apropos-map " " 'scroll-other-window)
         (define-key unix-apropos-map "\C-?"
	    (function (lambda nil
			 (interactive)
			 (scroll-other-window '-))))
 	 (define-key unix-apropos-map "p" 'previous-line)
	 (define-key unix-apropos-map "u" 'unix-apropos)
	 (define-key unix-apropos-map "x" 'unix-apropos-expunge)
	 (define-key unix-apropos-map "a" 'unix-apropos-additional)
	 (define-key unix-apropos-map "h" 'describe-mode)
	 (define-key unix-apropos-map "?" 'describe-mode)
	 )
      )
   (use-local-map unix-apropos-map)
   (setq mode-name "Apropos")
   (setq major-mode 'unix-apropos-mode)
   (insert-file whatis-file-name)
   (goto-char (point-min))
   (delete-non-matching-lines topic)
   (setq buffer-read-only t)
   (goto-char (point-min))
   )
