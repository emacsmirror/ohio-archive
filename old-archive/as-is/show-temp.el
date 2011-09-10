;Date:  Mon, 20 Mar 89 14:34:49 est
;From: jbw%bucsf.BU.EDU@bu-cs.bu.edu (Joe Wells)
;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: help windows
;
;Are you annoyed when help windows take up half of the screen, but only
;have one line of text?  Do you hate resizing them?  Here's a small
;package that will fix the problem of badly sized help windows.
;
;This package will adjust the size of help windows to fit the text,
;subject to a maximum size.  It will never accidentally enlarge the
;minibuffer, which some people were recently discussing.  If the window
;for the temp buffer is already on the screen, it will be resized.
;
;Have fun, send bugs to me.
;
;--
;Joe Wells
;INTERNET: jbw%bucsf.bu.edu@bu-it.bu.edu    IP: [128.197.10.201]
;UUCP: ...!harvard!bu-cs!bucsf!jbw
;----------------------------------------------------------------------
;; Improved help-window display.
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; jbw%bucsf.bu.edu@bu-it.bu.edu (school year)
;; joew%uswest@boulder.colorado.edu (summer)

;; The ideas for this package were derived from the C code in
;; src/window.c.

(defvar temp-buffer-max-height (/ (- (screen-height) 2) 2)
  "*This is the maximum window height (in text lines + mode line) which
show-temp-buffer will give to a temp buffer.")

(or temp-buffer-show-hook
    (setq temp-buffer-show-hook 'show-temp-buffer))

(defun show-temp-buffer (buffer)
  "Display BUFFER on the screen in a window of appropriate size.  Will
not give a window of size greater than temp-buffer-max-height."
  (let* ((current-window (selected-window))
	 (window (display-buffer buffer))
	 (window-lines (window-height window))
	 ;; Check if we can safely resize this window
	 (minibuffer-line (nth 1 (window-edges (minibuffer-window))))
	 (window-edges (window-edges window))
	 (minibuffer-safe (not (and (= (nth 1 window-edges) 0)
				    (= (nth 3 window-edges)
				       minibuffer-line))))
	 window-lines-needed)
    ;; Ensure sane initial window configuration
    (setq minibuffer-scroll-window window)
    (set-window-start window 1)
    (set-window-point window 1)
    (set-window-hscroll window 0)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      ;; Delete leading blank lines
      (if (looking-at "\n+")
	  (let ((buffer-read-only nil)
		(buffer-modified-p (buffer-modified-p)))
	    (replace-match "")
	    (set-buffer-modified-p buffer-modified-p)))
      ;; Calculate vertical lines needed by text
      (setq window-lines-needed
	    (+ 1 (vertical-motion (- temp-buffer-max-height 2))
	       (if (bolp) 0 1))))
    (cond (minibuffer-safe
	   ;; Make sure the window isn't too small
	   (setq window-lines-needed
		 (if (< window-lines-needed window-min-height)
		     window-min-height
		   window-lines-needed))
	   ;; Do the resize operation
	   (select-window window)
	   (enlarge-window (- window-lines-needed window-lines))
	   (select-window current-window)))))

(provide 'show-temp-buffer)

