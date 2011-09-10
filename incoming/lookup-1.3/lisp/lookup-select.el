;;; lookup-select.el --- lookup-select-mode
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-select.el,v 1.3 1999/05/23 17:27:21 knishida Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

;;;;;;;;;;;;;;;;;;;;
;: Construct Buffer
;;;;;;;;;;;;;;;;;;;;

(defun lookup-select-display (session)
  (with-current-buffer (lookup-open-buffer (lookup-select-buffer))
    (lookup-select-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Type `m' to select, `u' to unselect, `?' for help.\n\n")
      (lookup-table-insert
       "%c %-12t %-20t %s\n"
       (append '((?% "Identfier" "Title" "Method")
		 (?- "---------" "-----" "------"))
	       (mapcar (lambda (dic)
			 (list (if (lookup-dictionary-selected-p dic) ?* ? )
			       (lookup-dictionary-id dic)
			       (lookup-dictionary-title dic)
			       (mapconcat 'lookup-method-key
					  (lookup-dictionary-methods dic) "")))
		       (lookup-module-dictionaries
			(lookup-session-module session)))))
      (lookup-select-goto-first))
    (lookup-pop-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;:  Lookup Select mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lookup-select-mode-map nil
  "*Keymap for Lookup Select mode.")

(unless lookup-select-mode-map
  (setq lookup-select-mode-map (make-sparse-keymap))
  (define-key lookup-select-mode-map " " 'lookup-select-next-line)
  (define-key lookup-select-mode-map "n" 'lookup-select-next-line)
  (define-key lookup-select-mode-map "p" 'lookup-select-previous-line)
  (define-key lookup-select-mode-map "\en" 'lookup-history-next)
  (define-key lookup-select-mode-map "\ep" 'lookup-history-previous)
  (define-key lookup-select-mode-map "\ef" 'lookup-module-forward)
  (define-key lookup-select-mode-map "\eb" 'lookup-module-backward)
  (define-key lookup-select-mode-map "m" 'lookup-select-do-select)
  (define-key lookup-select-mode-map "u" 'lookup-select-do-unselect)
  (define-key lookup-select-mode-map "a" 'lookup-select-do-select-all)
  (define-key lookup-select-mode-map "\C-m" 'lookup-select-do-select-only)
  (define-key lookup-select-mode-map "d" 'lookup-select-mark-disable)
  (define-key lookup-select-mode-map "x" 'lookup-select-do-execute)
;  (define-key lookup-select-mode-map "i" 'lookup-select-info)
  (define-key lookup-select-mode-map "M" 'lookup-select-menu)
  (define-key lookup-select-mode-map "f" 'lookup-select-search-pattern)
  (define-key lookup-select-mode-map "o" 'delete-other-windows)
  (define-key lookup-select-mode-map "/" 'lookup-select-text-search)
  (define-key lookup-select-mode-map "g" 'lookup-select-update)
  (define-key lookup-select-mode-map "q" 'lookup-suspend)
  (define-key lookup-select-mode-map "Q" 'lookup-exit)
  (define-key lookup-select-mode-map "R" 'lookup-restart)
  (define-key lookup-select-mode-map "?" 'lookup-select-help))

(defconst lookup-select-mode-help
  "Lookup Select $B%b!<%I(B:

`n'(ext)    - $B<!$N<-=q$X(B        `p'(revios) - $BA0$N<-=q$X(B

`m'(ark)    - $B<-=q$rA*Br(B        `u'(nmark)  - $B<-=q$rHsA*Br(B
`a'(ll)     - $BA4$F$N<-=q$rA*Br(B  `RET'       - $B$=$N<-=q$@$1$rA*Br(B
`d'(isable) - $B<-=q$rL58z2=(B   (e)`x'(ecute)  - $BL58z2=$r<B9T(B

`f'(ind)    - $B8!:w$r<B9T(B        `M'(enu)    - $B<-=q$N%a%K%e!<$rI=<((B
`o'(pen)    - $B2hLL$r:GBg2=(B      `/'         - $B$=$N<-=q$+$iA4J88!:w(B

`q'    - $B%P%C%U%!$rH4$1$k(B       `g'    - $B%b%8%e!<%k$r=i4|2=$7D>$9(B
`Q'    - Lookup $B$r=*N;$9$k(B      `R'    - Lookup $B$r:F5/F0$9$k(B")

(defvar lookup-select-mode-hook nil)

(defun lookup-select-mode ()
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-select-mode)
  (setq mode-name "Select")
  (setq mode-line-buffer-identification '("Lookup:%12b"))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map lookup-select-mode-map)
  (run-hooks 'lookup-select-mode-hook))

;;;
;:: Interactive commands
;;;

(defun lookup-select-next-line ()
  "$B<!$N9T$K?J$`!#(B"
  (interactive)
  (if (eobp) (ding) (forward-line)))

(defun lookup-select-previous-line ()
  "$BA0$N9T$KLa$k!#(B"
  (interactive)
  (if (bobp) (ding) (forward-line -1)))

(defun lookup-select-do-select ()
  "$B%]%$%s%H9T$N<-=q$rA*Br$9$k!#(B"
  (interactive)
  (lookup-select-set-selected t))

(defun lookup-select-do-unselect ()
  "$B%]%$%s%H9T$N<-=q$rHsA*Br$K$9$k!#(B"
  (interactive)
  (lookup-select-set-selected nil))

(defun lookup-select-toggle-selected ()
  "$B%]%$%s%H9T$N<-=q$NA*Br>uBV$r%H%0%k$9$k!#(B"
  (interactive)
  (let ((dict (lookup-select-point-dictionary)))
    (lookup-select-set-selected
     (not (lookup-dictionary-selected-p dict)))))

(defun lookup-select-do-select-all ()
  "$BA4$F$N<-=q$rA*Br$9$k!#(B"
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (while (not (eobp))
      (lookup-select-set-selected t))))

(defun lookup-select-do-select-only ()
  "$B%]%$%s%H9T$N<-=q$N$_$rA*Br$9$k!#(B"
  (interactive)
  (if (not (lookup-select-point-dictionary))
      (error "No dictionary on current line")
    (save-excursion
      (lookup-select-goto-first)
      (while (not (eobp))
	(lookup-select-set-selected nil)))
    (lookup-select-set-selected t t)))

(defun lookup-select-mark-disable ()
  "$B%]%$%s%H9T$N<-=q$KL58z2=$N%^!<%/$rIU$1$k!#(B"
  (interactive)
  (lookup-select-mark ?D t))

(defun lookup-select-do-execute ()
  "$BL58z2=$r<B9T$9$k!#(B"
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (let* ((inhibit-read-only t)
	   (module (lookup-session-module lookup-current-session))
	   (dicts (lookup-module-dictionaries module)))
      (while (re-search-forward "^D" nil t)
	(setq dicts (delq (lookup-select-point-dictionary) dicts))
	(kill-region (progn (beginning-of-line) (point))
		     (progn (forward-line) (point))))
      (lookup-module-set-dictionaries module dicts))))

(defun lookup-select-menu ()
  "$B<-=q$,%a%K%e!<$KBP1~$7$F$$$k>l9g!"$=$l$r;2>H$9$k!#(B"
  (interactive)
  (let* ((dict (lookup-select-point-dictionary))
	 (entries (lookup-vse-get-menu dict)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (title (lookup-dictionary-title dict))
	       (query (lookup-make-query 'reference title)))
	  (lookup-display-entries module query entries))
      (error "This dictionary has no menu"))))

(defun lookup-select-search-pattern (pattern)
  "$BA*Br$5$l$?<-=q$+$i8!:w$r9T$J$&!#(B"
  (interactive (list (lookup-read-string "Look up" nil 'lookup-input-history)))
  (lookup-search-pattern (lookup-session-module lookup-last-session) pattern))

(defun lookup-select-text-search (string &optional force)
  "$B%]%$%s%H9T$N<-=q$+$iA4J88!:w$r9T$J$&!#(B"
  (interactive
   (list (let ((dictionary (lookup-select-point-dictionary)))
	   (if (memq 'text (lookup-dictionary-methods dictionary))
	       (lookup-read-string "Look up" nil 'lookup-input-history)
	     (error "This dictionary does not support text search")))
	 current-prefix-arg))
  (let ((module (lookup-session-module lookup-current-session))
	(dictionary (lookup-select-point-dictionary))
	(query (lookup-make-query 'text string)))
    (message "searcing...")
    (lookup-display-entries module query
			    (lookup-vse-search-query dictionary query))
    (message "searcing...done")))

(defun lookup-select-update ()
  "$B8=:_$N8!:w%b%8%e!<%k$r=i4|2=$7D>$9!#(B
$B$?$@$7@_Dj%U%!%$%k$rJQ99$7$?>l9g$K$O!"JQ99$rH?1G$9$k$K$O(B
\\[lookup-restart] $B$rMQ$$$kI,MW$,$"$k!#(B"
  (interactive)
  (let ((module (lookup-session-module lookup-current-session)))
    (message "Updating %s..." (lookup-module-name module))
    (lookup-module-clear module)
    (lookup-module-init module)
    (lookup-select-dictionary module)
    (message "Updating %s...done" (lookup-module-name module))))

(defun lookup-select-help ()
  "Select $B%b!<%I$N4J0W%X%k%W$rI=<($9$k!#(B"
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-select-mode-help))
    (lookup-display-help (current-buffer))))

;;;
;:: Internal functions
;;;

(defun lookup-select-goto-first ()
  (goto-char (point-min))
  (forward-line 4))

(defun lookup-select-point-dictionary ()
  (save-excursion
    (beginning-of-line)
    (forward-char 2)
    (if (looking-at "[^ ]+") (lookup-get-dictionary (match-string 0)))))

(defun lookup-select-set-selected (value &optional dont-move)
  (let ((dict (lookup-select-point-dictionary)))
    (when dict
      (lookup-dictionary-set-selected dict value)
      (lookup-select-mark (if value ?* ? ) (not dont-move)))))

(defun lookup-select-mark (mark &optional down-after)
  (save-excursion
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-char 1)
      (insert-char mark 1)))
  (if down-after (forward-line)))

(provide 'lookup-select)

;;; lookup-select.el ends here
