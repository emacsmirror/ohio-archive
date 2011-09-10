;;; unscroll.el --- functions to restore the screen after scrolling

;; Copyright 1997 Bob Glickstein.      <http://www.zanshin.com/~bobg/>

;; Author: Bob Glickstein <bobg@zanshin.com>
;; Maintainer: Bob Glickstein <bobg@zanshin.com>
;; Version: 1.0

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

;; To use: (require 'unscroll) in your .emacs.

;; This file uses "advice" to modify the scrolling commands such that
;; the screen appearance is preserved at the start of each series of
;; scrolls.  Ever press C-v C-v C-v when you meant C-b C-b C-b?
;; That's what this is meant to remedy.

;; After scrolling (intentionally or otherwise), you can restore the
;; display with M-x unscroll RET, which I like to bind to C-M-v using
;; (define-key global-map "\C-\M-v" 'unscroll).

;; A version of this package is discussed and developed in chapter 3
;; of my book (see above).

;;; Code:

(defvar unscroll-point (make-marker)
  "Cursor position for next call to `unscroll'.")
(defvar unscroll-hscroll nil
  "Hscroll value for next call to `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to `unscroll'.")

(defun unscroll-remember ()
  (set-marker unscroll-point (point))
  (set-marker unscroll-window-start (window-start))
  (setq unscroll-hscroll (window-hscroll)))

(defun first-unscrollable-command-p ()
  (not (get last-command 'unscrollable)))

(put 'scroll-up    'unscrollable t)
(put 'scroll-down  'unscrollable t)

(defadvice scroll-up (before remember-for-unscroll
			     activate compile)
  "Before scrolling, remember where we started from, for `unscroll'."
  (if (first-unscrollable-command-p)
      (unscroll-remember)))

(defadvice scroll-down (before remember-for-unscroll
			       activate compile)
  "Before scrolling, remember where we started from, for `unscroll'."
  (if (first-unscrollable-command-p)
      (unscroll-remember)))

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (if (not unscroll-point)
      (error "Cannot unscroll yet"))
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(provide 'unscroll)

;;; unscroll.el ends here
