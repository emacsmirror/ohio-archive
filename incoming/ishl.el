;;; ishl.el --- extra isearch highlighting

;; Copyright 1997 Bob Glickstein.      <http://www.zanshin.com/~bobg/>

;; Author: Bob Glickstein <bobg@zanshin.com>
;; Maintainer: Bob Glickstein <bobg@zanshin.com>
;; Version: 1.5

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

;;; Plug:

;; Check out my book, "Writing GNU Emacs Extensions," from O'Reilly
;; and Associates.  <http://www.ora.com/catalog/gnuext/>

;;; Commentary:

;; `ishl-mode' is a mode that does additional highlighting during
;; incremental searches.  Using the relatively unobtrusive "secondary
;; selection" face, ishl highlights *every* match for the current
;; search string.  (Of course, the *current* match remains highlighted
;; with the "region" face.)  It does this in a deferred fashion using
;; idle timers, so the cycles needed to highlight additional matches
;; do not rob isearch of its usual snappy response.

;; The additional highlighting makes it easier to anticipate where the
;; cursor will land each time you press C-s or C-r to repeat a pending
;; search forward or backward.

;; Each time the search string is changed, ishl-mode waits
;; `ishl-initial-delay' seconds, then starts highlighting matches one
;; every `ishl-delay' seconds.  These numbers are 1/4 and 1/16 by
;; default.

;;; To do:

;; Define an "ishl" face.

;;; Code:

(require 'timer)

(defconst ishl-version "1.5"
  "Version number of ishl.")

(defvar ishl-overlays nil)
(defvar ishl-wrapped nil)
(defvar ishl-start nil)
(defvar ishl-end nil)
(defvar ishl-timer nil)
(defvar ishl-last-string nil)

(defvar ishl-initial-delay 0.25
  "*Seconds before starting to highlight additional matches.")
(defvar ishl-delay 0.0625
  "*Seconds between highlighting additional matches.")

(defvar ishl-cleanup t
  "*Whether to remove highlighting after a search.")

(defun ishl-cleanup (&optional force)
  (interactive '(t))
  (if (or force
	  ishl-cleanup)
      (ishl-remove-overlays))
  (if ishl-timer
      (cancel-timer ishl-timer)))

(defun ishl-new-loop ()
  (if (not (equal isearch-string ishl-last-string))
      (progn
	(ishl-cleanup t)
	(if (and isearch-overlay
		 (not (overlay-get isearch-overlay 'priority)))
	    (overlay-put isearch-overlay 'priority 1))
	(setq ishl-start isearch-opoint
	      ishl-end isearch-opoint
	      ishl-last-string isearch-string
	      ishl-wrapped nil
	      ishl-timer (run-with-idle-timer ishl-initial-delay nil
					      'ishl-update)))))

(defun ishl-remove-overlays ()
  (while ishl-overlays
    (delete-overlay (car ishl-overlays))
    (setq ishl-overlays (cdr ishl-overlays))))

(defun ishl-search ()
  (let ((case-fold-search isearch-case-fold-search))
    (funcall (cond (isearch-word (if isearch-forward
				     'word-search-forward
				   'word-search-backward))
		   (isearch-regexp (if isearch-forward
				       're-search-forward
				     're-search-backward))
		   (t (if isearch-forward
			  'search-forward
			'search-backward)))
	     isearch-string
	     (if isearch-forward
		 (if ishl-wrapped ishl-start nil)
	       (if ishl-wrapped ishl-end nil))
	     t)))

(defun ishl-update ()
  (if isearch-invalid-regexp
      nil
    (save-excursion
      (save-match-data
	(goto-char (if isearch-forward ishl-end ishl-start))
	(let ((found (ishl-search)))
	  (if found
	      (let ((ov (make-overlay (match-beginning 0)
				      (match-end 0))))
		(overlay-put ov 'face 'secondary-selection)
		(overlay-put ov 'priority 0)
		(setq ishl-overlays (cons ov ishl-overlays)
		      ishl-timer (run-at-time ishl-delay nil 'ishl-update))
		(if isearch-forward
		    (setq ishl-end (point))
		  (setq ishl-start (point))))
	    (if ishl-wrapped
		nil
	      (setq ishl-wrapped t
		    ishl-timer (run-at-time ishl-delay nil 'ishl-update))
	      (if isearch-forward
		  (setq ishl-end (point-min))
		(setq ishl-start (point-max))))))))))

(defadvice isearch-update (after ishl-start-loop compile)
  (ishl-new-loop))

(defvar ishl-mode nil)
(defun ishl-mode (&optional arg)
  "Highlight additional incremental search matches."
  (interactive "P")
  (setq ishl-mode
	(if (null arg)
	    (not ishl-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if ishl-mode
      (progn
	(ad-activate 'isearch-update t)
	(add-hook 'isearch-mode-end-hook 'ishl-cleanup))
    (ad-deactivate 'isearch-update)
    (remove-hook 'isearch-mode-end-hook 'ishl-cleanup)))

(define-key global-map "\C-c\C-l" 'ishl-cleanup)

(provide 'ishl)

;;; ishl.el ends here
