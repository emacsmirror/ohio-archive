;;; lookup-entry.el --- lookup-entry-mode
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-entry.el,v 1.5 2000/01/05 07:37:22 tsuchiya Exp $

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
(require 'lookup-content)

;;;
;:: Internal variables
;;;

(defvar lookup-entry-line-pattern "")
(defvar lookup-entry-line-number "0")

(make-variable-buffer-local 'lookup-entry-line-pattern)
(make-variable-buffer-local 'lookup-entry-line-number)

(defvar lookup-entry-insert-format "%-19s ")

;;;;;;;;;;;;;;;;;;;;
;: Construct Buffer
;;;;;;;;;;;;;;;;;;;;

(defun lookup-entry-display (session)
  (with-current-buffer (lookup-open-buffer (lookup-entry-buffer))
    (let ((query (lookup-session-query session))
	  (entries (lookup-session-entries session))
	  (excursion (lookup-session-excursion session)))
      ;; insert entries
      (let ((inhibit-read-only t))
	(lookup-entry-mode)
	(erase-buffer)
	(lookup-foreach 'lookup-entry-insert entries))
      ;; set mode line
      (setq lookup-entry-line-pattern (lookup-query-string query))
      (setq lookup-entry-line-number (number-to-string (length entries)))
      ;; display buffer
      (if excursion
	  (lookup-entry-set-excursion excursion)
	(lookup-pop-to-buffer (current-buffer))
	(goto-char (point-min))
	(lookup-entry-goto-link)
	(if lookup-dynamic-display (sit-for 0))
	(lookup-entry-display-content)
	(if lookup-dynamic-display (sit-for 0))))))

(defun lookup-entry-append (session entries)
  (with-current-buffer (lookup-entry-buffer)
    (goto-char (prog1 (point)
		 (let ((inhibit-read-only t))
		   (goto-char (point-max))
		   (lookup-foreach 'lookup-entry-insert entries))))
    (let ((entries (append (lookup-session-entries session) entries)))
      (lookup-session-set-entries session entries))
    (setq lookup-entry-line-number
	  (number-to-string (+ (string-to-number lookup-entry-line-number)
			       (length entries))))
    (if lookup-dynamic-display (sit-for 0))))

(defun lookup-entry-expand-reference (reference)
  (let ((entries (lookup-reference-entries reference)))
    (when entries
      ;; replace buffer
      (let ((start (progn (beginning-of-line) (point)))
	    (inhibit-read-only t))
	(delete-region start (progn (forward-line) (point)))
	(lookup-foreach 'lookup-entry-insert entries)
	(goto-char start)
	(lookup-entry-goto-link))
      ;; replace cache
      (let ((list (lookup-session-entries lookup-current-session)))
	(if (eq reference (car list))
	    (setq list (append entries (cdr list)))
	  (while (not (eq reference (cadr list))) (setq list (cdr list)))
	  (when list
	    (setcdr list (append entries (cddr list))))))
      (setq lookup-entry-line-number
	    (number-to-string (+ (string-to-number lookup-entry-line-number)
				 (1- (length entries))))))))

(defun lookup-entry-insert (entry)
  ;; $B%(%s%H%j9T$r%P%C%U%!$KA^F~$7!"%j%s%/$r%;%C%H$9$k!#(B
  (let ((dictionary (lookup-entry-dictionary entry))
	(prefix (lookup-entry-prefix entry))
	(heading (lookup-entry-heading entry))
	start end)
    (insert (format lookup-entry-insert-format (lookup-dictionary-title dictionary)))
    ;; property $B$d(B extent $B$r<!$N9T$K$^$?$,$J$$$h$&$K@h$K(B newline $B$7$F$*$/!#(B
    (newline)
    (backward-char)
    (setq start (point)
	  end (progn (if prefix (insert prefix)) (insert heading) (point)))
    (lookup-entry-set-link start end entry)
    (lookup-map-over-property start end
			      'lookup-gaiji 'lookup-gaiji-glyph-paste)
    (forward-char)))

;; Excursion:

(defun lookup-entry-excursion ()
  (let ((entry (get-buffer (lookup-entry-buffer)))
	(content (get-buffer (lookup-content-buffer))))
    (when entry
      (cons (with-current-buffer entry
	      (cons (point) (let ((window (get-buffer-window entry)))
			      (if window (window-start window)))))
	    (when (and content (with-current-buffer entry
				 (lookup-entry-current-line-entry)))
	      (with-current-buffer content
		(cons (point) (let ((window (get-buffer-window content)))
				(if window (window-start window))))))))))

(defun lookup-entry-set-excursion (excursion)
  (let ((entry-point (caar excursion)) (entry-start (cdar excursion))
	(content (cdr excursion)))
    (lookup-pop-to-buffer (lookup-entry-buffer))
    (goto-char entry-point)
    (if entry-start
	(set-window-start (selected-window) entry-start))
    (lookup-entry-display-content)
    (when content
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (car content))
	(if (cdr content)
	    (set-window-start (selected-window) (cdr content)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;:  Lookup Entry mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lookup-entry-mode-map nil
  "*Keymap for Lookup Entry mode.")

(unless lookup-entry-mode-map
  (setq lookup-entry-mode-map (make-sparse-keymap))
  (define-key lookup-entry-mode-map " " 'lookup-entry-next-page)
  (define-key lookup-entry-mode-map "\C-?" 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map [delete] 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map [backspace] 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map "\C-m" 'lookup-entry-scroll-up-content)
  (define-key lookup-entry-mode-map "<" 'lookup-entry-beginning-of-content)
  (define-key lookup-entry-mode-map ">" 'lookup-entry-end-of-content)
  (define-key lookup-entry-mode-map "n" 'lookup-entry-next-entry)
  (define-key lookup-entry-mode-map "p" 'lookup-entry-previous-entry)
  (define-key lookup-entry-mode-map "\en" 'lookup-history-next)
  (define-key lookup-entry-mode-map "\ep" 'lookup-history-previous)
  (define-key lookup-entry-mode-map "\ef" 'lookup-module-forward)
  (define-key lookup-entry-mode-map "\eb" 'lookup-module-backward)
  (define-key lookup-entry-mode-map "i" 'lookup-entry-info)
  (define-key lookup-entry-mode-map "o" 'lookup-entry-open)
  (define-key lookup-entry-mode-map "O" 'lookup-entry-open-other)
;  (define-key lookup-entry-mode-map "e" 'lookup-entry-edit)
  (define-key lookup-entry-mode-map "v" 'lookup-entry-overview-mode)
  (define-key lookup-entry-mode-map "t" 'lookup-entry-toggle-format)
  (define-key lookup-entry-mode-map "s" 'lookup-entry-isearch-content)
  (define-key lookup-entry-mode-map "w" 'lookup-entry-cite-content)
  (define-key lookup-entry-mode-map "f" 'lookup-entry-search-pattern)
;  (define-key lookup-entry-mode-map "I" 'lookup-entry-show-index)
  (define-key lookup-entry-mode-map "M" 'lookup-entry-show-menu)
  (define-key lookup-entry-mode-map "L" 'lookup-entry-list-references)
;  (define-key lookup-entry-mode-map "H" 'lookup-entry-list-history)
  (define-key lookup-entry-mode-map "S" 'lookup-entry-select-dictionary)
  (define-key lookup-entry-mode-map "r" 'lookup-entry-start-window)
  (define-key lookup-entry-mode-map "h" 'lookup-entry-content-window)
  (define-key lookup-entry-mode-map "g" 'lookup-entry-update)
  (define-key lookup-entry-mode-map "q" 'lookup-suspend)
  (define-key lookup-entry-mode-map "Q" 'lookup-exit)
  (define-key lookup-entry-mode-map "R" 'lookup-restart)
  (define-key lookup-entry-mode-map "?" 'lookup-entry-help)
  (if (featurep 'xemacs)
      (define-key lookup-entry-mode-map 'button2 'lookup-entry-mouse-follow)
    (define-key lookup-entry-mode-map [mouse-2] 'lookup-entry-mouse-follow))
  )

(defconst lookup-entry-mode-help
  "Lookup Entry $B%b!<%I(B:

`n'(ext)     - $B<!$N%(%s%H%j$X(B   `M-n' - $B8!:wMzNr$r<!$X(B
`p'(revious) - $BA0$N%(%s%H%j$X(B   `M-p' - $B8!:wMzNr$rA0$X(B

`v'(iew)   - Overview $B%b!<%I(B  	`o'(pen)  - $B2hLL$r:GBg2=(B
`s'(earch) - isearch-forward  	`i'(nfo)  - $B%(%s%H%j$N>pJsI=<((B
`t'(oggle) - $B@07A=hM}$r%H%0%k(B 	`w'(rite) - $BFbMF$r%-%k%j%s%0$KJ]B8(B
`f'(ind)   - $B8!:w8l$rF~NO(B

`M'(enu)   - $B<-=q$N%a%K%e!<I=<((B
`L'(inks)  - $B%j%U%!%l%s%9$r0lMw(B `S'(elect) - $B<-=qA*Br%b!<%I(B

`r'   - $B8!:w3+;O%P%C%U%!$K0\F0(B  `h'   - Content $B%P%C%U%!$K0\F0(B
`q'   - $B%P%C%U%!$rH4$1$k(B        `g'   - $B8!:w$r$d$jD>$9(B
`Q'   - Lookup $B$r=*N;$9$k(B       `R'   - Lookup $B$r:F5/F0$9$k(B")

(defvar lookup-entry-overview-mode nil)

(make-variable-buffer-local 'lookup-entry-overview-mode)
(or (assq 'lookup-entry-overview-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lookup-entry-overview-mode " Overview")
				 minor-mode-alist)))

(defvar lookup-entry-overview-mode-map nil
  "*Keymap for Lookup Entry Overview mode.")

(unless lookup-entry-overview-mode-map
  (setq lookup-entry-overview-mode-map (make-sparse-keymap))
  (define-key lookup-entry-overview-mode-map "n" 'next-line)
  (define-key lookup-entry-overview-mode-map "p" 'previous-line))

(or (assq 'lookup-entry-overview-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'lookup-entry-overview-mode
					   lookup-entry-overview-mode-map)
				     minor-mode-map-alist)))

(defun lookup-entry-mode ()
  "Lookup Entry $B%b!<%I!#(B
$B$3$l$O8!:w$N7k2L!"8+IU$+$C$?%(%s%H%j$r0lMw$9$k$?$a$N%b!<%I!#(B


$B%b!<%I$KF~$k$H$-$K(B `lookup-entry-mode-hook' $B$,8F$P$l$k!#(B"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-entry-mode)
  (setq mode-name "Entry")
  (setq mode-line-buffer-identification
	'("Lookup:%b {" lookup-entry-line-pattern "} ["
	  lookup-entry-line-number "]"))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map lookup-entry-mode-map)
  (if (and (featurep 'xemacs) (featurep 'scrollbar))
      (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (run-hooks 'lookup-entry-mode-hook))

(defun lookup-entry-overview-mode (&optional arg)
  "Overview $B%b!<%I$KF~$k!#(B
Cotent $B%P%C%U%!$,%/%m!<%:$5$l!"(B`n' $B$H(B `p' $B$,C1$K%]%$%s%H0\F0$@$1$r(B
$B9T$J$&$h$&$K$J$k!#(B"
  (interactive)
  (setq lookup-entry-overview-mode (if arg (> (prefix-numeric-value arg) 0)
				     (not lookup-entry-overview-mode)))
  (if lookup-entry-overview-mode
      (lookup-hide-buffer (lookup-content-buffer))
    (lookup-display-buffer (lookup-content-buffer)))
  (force-mode-line-update))

;;;
;:: Interactive commands
;;;

(defun lookup-entry-display-content ()
  "$B%]%$%s%H9T$N%(%s%H%j$NK\J8$rI=<($9$k!#(B
Overview $B%b!<%I$K$J$C$F$$$k>l9g$K$O$=$l$r2r=|$7!"(BContent $B%P%C%U%!$r(B
$B%*!<%W%s$9$k!#%(%s%H%j$,%j%U%!%l%s%9$N>l9g$K$O!"$=$l$r;2>H$9$k!#(B"
  (interactive)
  (lookup-entry-goto-link)
  (let ((entry (lookup-entry-current-line-entry)))
    (when (lookup-reference-p entry)
      (lookup-reference-refer entry)
      (lookup-entry-expand-reference entry)
      ;; dynamic reference $B$r;2>H$7$?>l9g!"%P%C%U%!$,=q$-49$o$k$3$H$,$"$k!#(B
      (setq entry (lookup-entry-current-line-entry)))
    (when entry
      (lookup-content-display entry)
      (lookup-entry-overview-mode 0))))

(defun lookup-entry-mouse-follow (event)
  "$B%^%&%9$G%/%j%C%/$7$?%(%s%H%j$NK\J8$rI=<($9$k!#(B"
  (interactive "e")
  (mouse-set-point event)
  (lookup-entry-display-content))

(defun lookup-entry-next-page ()
  "$B%(%s%H%jK\J8$NI=<($r0l%Z!<%8?J$a$k!#(B
$B%P%C%U%!$N=*$o$j$^$GC#$7$?$i!"<!$N%(%s%H%j$K0\F0$9$k!#(B"
  (interactive)
  (cond
   ((not (lookup-entry-current-line-entry)) nil)
   ((not (lookup-entry-content-visible-p)) (lookup-entry-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-max) (selected-window))))
    (lookup-entry-scroll-up-content))
   (t (lookup-entry-next-entry))))

(defun lookup-entry-previous-page ()
  "$B%(%s%H%jK\J8$NI=<($r0l%Z!<%8La$9!#(B
$B%P%C%U%!$N;O$a$^$GC#$7$?$i!"A0$N%(%s%H%j$K0\F0$9$k!#(B"
  (interactive)
  (cond
   ((not (lookup-entry-current-line-entry)) (lookup-entry-previous-entry))
   ((not (lookup-entry-content-visible-p)) (lookup-entry-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-min) (selected-window))))
    (lookup-entry-scroll-down-content))
   (t (lookup-entry-previous-entry))))

(defun lookup-entry-scroll-up-content (&optional arg)
  "$B%(%s%H%jK\J8$r%W%l%U%#%9%/$N9T?t$@$1%9%/%m!<%k!&%"%C%W$9$k!#(B"
  (interactive "p")
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-up arg))
    (lookup-entry-display-content)))

(defun lookup-entry-scroll-down-content (&optional arg)
  "$B%(%s%H%jK\J8$r%W%l%U%#%9%/$N9T?t$@$1%9%/%m!<%k!&%@%&%s$9$k!#(B"
  (interactive "p")
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-down arg))
    (lookup-entry-display-content)))

(defun lookup-entry-beginning-of-content ()
  "$B%(%s%H%jK\J8$NI=<($r@hF,$^$GLa$9!#(B"
  (interactive)
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-min)))
    (lookup-entry-display-content)))

(defun lookup-entry-end-of-content ()
  "$B%(%s%H%jK\J8$NI=<($rKvHx$^$G?J$a$k!#(B"
  (interactive)
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-max))
	(recenter -2))
    (lookup-entry-display-content)))

(defun lookup-entry-next-entry (&optional arg)
  "$B<!$N%(%s%H%j$rI=<($9$k!#%W%l%U%#%/%9$N?t$@$1?J$`!#(B"
  (interactive "p")
  (if (eobp)
      (progn (message "End of buffer") (ding))
    (forward-line arg)
    (lookup-entry-goto-link)
    (or (pos-visible-in-window-p (save-excursion (forward-line) (point)))
	(recenter -2))
    (lookup-entry-display-content)))

(defun lookup-entry-previous-entry (&optional arg)
  "$BA0$N%(%s%H%j$rI=<($9$k!#%W%l%U%#%/%9$N?t$@$1La$k!#(B"
  (interactive "p")
  (beginning-of-line)
  (if (bobp)
      (progn (message "Beginning of buffer") (ding))
    (forward-line (- (or arg 1)))
    (lookup-entry-goto-link)
    (or (pos-visible-in-window-p (save-excursion (forward-line -1) (point)))
	(recenter 1))
    (lookup-entry-display-content)))

(defun lookup-entry-info ()
  "$B%(%s%H%j$N>pJs$r=PNO$9$k!#(B"
  (interactive)
  (let ((entry (lookup-entry-current-line-entry)))
    (with-current-buffer (lookup-open-buffer "*Entry Information*")
      (help-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Entry information for `%s':\n"
			(lookup-entry-heading entry)))
	(newline)
	(insert (format "Dictionary: %s\n" (lookup-dictionary-id
					    (lookup-entry-dictionary entry))))
	(insert (format "Heading:    %s\n" (lookup-entry-heading entry)))
	(insert (format "Code:       %s\n" (lookup-entry-code entry)))
	(goto-char (point-min))
	(forward-line 2))
      (lookup-display-buffer (current-buffer)))))

(defun lookup-entry-open ()
  "$B%(%s%H%jK\J8$r2hLL0lGU$KI=<($9$k!#(B"
  (interactive)
  (delete-other-windows)
  (lookup-entry-display-content))

(defun lookup-entry-open-other ()
  "$B%(%s%H%jK\J8$rJL%W%m%0%i%`$GI=<($9$k!#(B"
  (interactive)
  (unless (lookup-vse-open-entry (lookup-entry-current-line-entry))
    (error "This entry doesn't have a open command")))

(defun lookup-entry-toggle-format ()
  "$B%(%s%H%jK\J8$N@07A=hM}$r%H%0%k$9$k!#(B"
  (interactive)
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-toggle-format)))

(defun lookup-entry-isearch-content (&optional rexexp-p)
  "Content $B%P%C%U%!$G(B isearch-forward $B$r<B9T$9$k!#(B"
  (interactive "P")
  (lookup-with-buffer-and-window (lookup-content-buffer)
    (isearch-forward rexexp-p)))

(defconst lookup-entry-default-policies
  '((asis . ((gaiji . glyph)))
    (plain . ((gaiji . alternate)))))

(defun lookup-entry-cite-content ()
  "$B%(%s%H%jK\J8$r%-%k%j%s%0$KJ]B8$9$k!#(B
See also `lookup-content-cite-region'."
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-cite-region (point-max) (point-min)))
  (when (interactive-p)
    (message "Saved text for `%s'"
	     (lookup-entry-heading (lookup-entry-current-line-entry)))))

(defun lookup-entry-search-pattern (pattern)
  "$B%_%K%P%C%U%!$+$i8!:w<0$rF~NO$7$F8!:w$9$k!#(B"
  (interactive
   (list (lookup-read-string "Look up" nil 'lookup-input-history
			     (lookup-query-string
			      (lookup-session-query lookup-last-session)))))
  (lookup-search-pattern (lookup-session-module lookup-last-session) pattern))

(defun lookup-entry-show-menu ()
  "$B<-=q$,%a%K%e!<$KBP1~$7$F$$$k>l9g!"$=$l$r;2>H$9$k!#(B"
  (interactive)
  (let* ((dict (lookup-entry-dictionary (lookup-entry-current-line-entry)))
	 (entries (lookup-vse-get-menu dict)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (title (lookup-dictionary-title dict))
	       (query (lookup-make-query 'reference title)))
	  (lookup-display-entries module query entries))
      (error "This dictionary has no menu"))))

(defun lookup-entry-list-references ()
  "$B%(%s%H%jK\J8$K4^$^$l$k%j%U%!%l%s%9$r0lMw$9$k!#(B"
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (let ((entries (lookup-content-collect-references)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (entry (lookup-entry-current-line-entry))
	       (heading (lookup-entry-heading entry))
	       (query (lookup-make-query 'reference heading)))
	  (lookup-display-entries module query entries))
      (error "No valid reference in current content"))))

(defun lookup-entry-select-dictionary ()
  "$B<-=qA*Br%P%C%U%!$K0\F0!#(B"
  (interactive)
  (lookup-select-dictionary (lookup-session-module lookup-current-session)))

(defun lookup-entry-start-window ()
  "$B8!:w$r3+;O$7$?%&%#%s%I%&$K0\F0$9$k!#(B"
  (interactive)
  (if (window-live-p lookup-start-window)
      (select-window lookup-start-window)
    (lookup-hide-buffer (current-buffer))
    (delete-other-windows)
    (let ((lookup-open-function 'lookup-other-window))
      (lookup))))

(defun lookup-entry-content-window ()
  "Content $B%P%C%U%!$K0\F0$9$k!#(B"
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (select-window (get-buffer-window (lookup-content-buffer))))

(defun lookup-entry-update (&optional module)
  "$B:#2s$HF1$88!:w8l$G!"%(%s%H%j$r:F8!:w$9$k!#(B"
  (interactive (list (if current-prefix-arg (lookup-input-module))))
  (let ((query (lookup-session-query lookup-last-session)))
    (if (eq (lookup-query-method query) 'reference)
	(error "This is reference session")
      (let ((lookup-force-update t))
	(setq module (or module (lookup-session-module lookup-last-session)))
	(lookup-search-query module query)))))

(defun lookup-entry-update-content ()
  "$B%(%s%H%jK\J8$r:FI=<($9$k!#@07A=hM}$bA4$F$d$jD>$5$l$k!#(B"
  (interactive)
  (let ((lookup-force-update t))
    (lookup-entry-display-content)))

(defun lookup-entry-help ()
  "Entry $B%b!<%I$N4J0W%X%k%W$rI=<($9$k!#(B"
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-entry-mode-help))
    (lookup-display-help (current-buffer))))

;;;
;:: Internal functions
;;;

(defun lookup-entry-set-link (start end entry)
  ;; $B%P%C%U%!$N(B START $B$+$i(B END $B$^$G$N%j!<%8%g%s$r(B ENTRY $B$X$N%j%s%/$K$9$k!#(B
  (add-text-properties start end (list 'mouse-face 'highlight
				       'lookup-entry entry)))

(defun lookup-entry-goto-link ()
  ;; $B%]%$%s%H9T$N%j%s%/0LCV$K0\F0$9$k!#(B
  (let ((p (progn (beginning-of-line) (point))))
    (if (setq p (next-single-property-change p 'lookup-entry))
	(goto-char p))))

(defun lookup-entry-current-line-entry ()
  ;; $B%]%$%s%H9T$N(B entry $B$rJV$9!#(B
  (save-excursion
    (end-of-line)
    (get-text-property (1- (point)) 'lookup-entry)))

(defun lookup-entry-content-visible-p ()
  ;; content $B$,I=<($5$l$F$$$l$P(B `t'$B!#(B
  (and (get-buffer-window (lookup-content-buffer))
       (eq (lookup-entry-current-line-entry)
	   (with-current-buffer (lookup-content-buffer)
	     lookup-content-current-entry))))

(provide 'lookup-entry)

;;; lookup-entry.el ends here
