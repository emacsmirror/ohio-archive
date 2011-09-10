;; Two window commands: compare-windows, two-window-command.
;; This is a modified version of compare-w.el
;; Copyright (C) 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

(defvar cw-ignore-whitespace nil
  "*Set to non-nil if you wish compare-windws to ignore all whitespace.")

(defvar cw-find-match-string-size 20
  "*The length of string to match in compare-window-find-match")
(defvar cw-find-match-distance 5
  "*The number of lines forward in which to search for a find-matchy point.")

(defvar cw-bounce-cursor t
  "*Set to nil to avoid point bouncing with compare-window")

(defvar bounce-cursor-time 1
  "*Number of seconds to sit at bounce point")


(defun compare-windows ()
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match."
  (interactive)
  (cw-find-match) ; first recover from initial difference

  (let ((opoint (point))
	p1 p2 maxp1 maxp2 b1 b2 w2
	success size done s1 s2)

    (setq p1 (point)
	  b1 (current-buffer))
    (setq w2 (next-window (selected-window)))
    (if (eq w2 (selected-window))
	(error "No other window."))
    (setq p2 (window-point w2)
	  b2 (window-buffer w2))
    (setq maxp1 (point-max))
    (save-excursion
     (set-buffer b2)
     (setq maxp2 (point-max)))

    (setq done nil)
    (while (not done)
    ;; Try advancing the pointers comparing 1024 chars at a time.
    ;; When that fails, divide by 2 and so on.
    (setq size 1024)
    (while (> size 0)
      (setq success t)
      (while success
	(setq size (min size (- maxp1 p1) (- maxp2 p2)))
	(setq s1 (buffer-substring p1 (+ size p1)))
	(save-excursion
	 (set-buffer b2)
	 (goto-char p2)
	 (setq success (search-forward s1 (+ p2 size) t)))
	(setq success (and (> size 0) success))
	(if success
	      (setq p1 (+ p1 size)
		    p2 (+ p2 size)))
	)
      ; no success - 
      (setq size (lsh size -1))  ; divide by two
      )
    ; found a difference (or eob) but is it significant?
;    (message "found a difference")

    (if (and cw-ignore-whitespace (not (eobp)))
	(let ((c2 (save-excursion
		    (set-buffer b2)
		    (goto-char p2)
		    (skip-chars-forward " \t\n\f\r")
		    (setq p2 (point))
		    (following-char)))
	      (c1 (progn
		    (goto-char p1)
		    (skip-chars-forward " \t\n\f\r")
		    (setq p1 (point))
		    (following-char))))
	  (setq done (not (char-equal c1 c2)))
;	  (message "c1=%c c2=%c" c1 c2)
	  )
      ; \n and \r are the same
      (while (and selective-display
		  (let ((c1 (progn
			      (goto-char p1)
			      (following-char)))
			(c2 (save-excursion
			      (set-buffer b2)
			      (goto-char p2)
			      (following-char))))
		    (and (or (char-equal c1 ?\n) (char-equal c2 ?\r))
			 (or (char-equal c2 ?\n) (char-equal c1 ?\r)))))
	(setq p1 (1+ p1))
	(setq p2 (1+ p2))
	)

      (setq done t))

    ) ; while
	      
    ; finish up
    (goto-char p1)
    (set-window-point w2 p2)
    (save-window-excursion
      (select-window w2)
;      (goto-char p2)
      (if cw-bounce-cursor (sit-for bounce-cursor-time)))
      
    (if (= (point) opoint)
	(ding)) ; nothing new found
    ))


(defun cw-find-match ()

  "Find the next common substring between two windows starting at
point in each window.  The substring to be found is the smaller of the
remainder of the line (or up to first whitespace if
cw-ignore-whitespace) and cw-find-match-string-size characters past point.
The maximum distance to search is cw-find-match-distance."

  (interactive)
  (let ((opoint (point))
	p1 p2
	maxp1 maxp2 b1 b2 w2 s1 s2 size success)
    (setq w2 (next-window (selected-window)))
    (if cw-ignore-whitespace
 	(skip-chars-forward " \t\n\f\r"))
    (setq p1 (point)
	  b1 (current-buffer))
    (if (eq w2 (selected-window))
 	(error "No other window."))
    (setq p2 (window-point w2)
	  b2 (window-buffer w2))
    (setq maxp1 (point-max))
    (save-excursion
      (set-buffer b2)
      (if cw-ignore-whitespace
	  (skip-chars-forward " \t\n\f\r"))
      (setq p2 (point))
      (setq maxp2 (point-max)))

    ; search for buffer 1 substring in buffer 2
    (save-excursion
      (set-buffer b1)
      (setq size (min cw-find-match-string-size (- maxp1 p1) (- maxp2 p2)
		      (- (save-excursion
			   (if cw-ignore-whitespace
			       (if (re-search-forward "[ \t\n\f\r]" maxp1 t)
				   (backward-char 1)
				 (end-of-line))
			     (end-of-line))
			   (point))
			 p1)))

      (setq s1 (buffer-substring p1 (+ size p1)))
;      (message "search 1 for \"%s\"" s1) (sit-for 1)
      (set-buffer b2)
      (goto-char p2)
      (setq success
	    (search-forward
	     s1
	     (save-excursion  ; limit search distance
	       (next-line cw-find-match-distance)
	       (point))
	     t))
      (if success
	  (setq p2 (- (point) size))) ; find beginnning of match
      )

    (if success
	(set-window-point w2 p2)

      ; else search for buffer 2 string in buffer 1
;      (message "search failed") (sit-for 1)
      (save-excursion
	(set-buffer b2)
	(setq size (min cw-find-match-string-size (- maxp1 p1) (- maxp2 p2)
			(- (save-excursion
			     (goto-char p2)
			     (if cw-ignore-whitespace
				 (if (re-search-forward "[ \t\n\f\r]" maxp2 t)
				     (backward-char 1)
				   (end-of-line))
			       (end-of-line))
			     (point))
			   p2)))
	(setq s2 (buffer-substring p2 (+ size p2)))
;	(message "search 2 for \"%s\"" s2) (sit-for 1)
	(set-buffer b1)
	(goto-char p1)
	(setq success
	      (search-forward
	       s2
	       (save-excursion
		 (next-line cw-find-match-distance)
		 (point))
	       t))
	(if success
	    (setq p1 (- (point) size)))
	))

    (if success
	(progn
	    (goto-char p1)
	    (save-window-excursion
	      (select-window w2)
	      (goto-char p2)
	      (if cw-bounce-cursor (sit-for bounce-cursor-time))
	      ))
	(error "Can't find common text"))
    ))


(defun two-window-command (keys)
  "Execute the next command in two windows.  Any arguments will be reused."
  (interactive "kEnter command for two windows: ")
  (let ((command (key-binding keys))
	b1 w2)
    (setq b1 (current-buffer))
    (if command
	(unwind-protect
	    (command-execute command t)   ; put it in the command history
	  (setq w2 (next-window (selected-window)))
	  (if (eq w2 (selected-window))
	      (error "No other window."))
	  (unwind-protect
	      (eval (car command-history)) ; any arguments given will apply
	    (sit-for bounce-cursor-time)
	    (set-buffer b1)
	    ))))
  )
	 

