;From: mad@math.keio.JUNET (MAEDA Atusi)
;Newsgroups: comp.emacs
;Subject: getris.el -- clone of a famous Russian game program.
;Message-ID: <MAD.89Sep14193115@cabbage.math.keio.JUNET>
;Date: 14 Sep 89 10:31:15 GMT
;Reply-To: mad@nakanishi.math.keio.junet
;Organization: Faculty of Sci. and Tech., Keio Univ., Yokohama, Japan.
;Lines: 521

;;; Getris -- clone of a famous Russian game program.
;; Copyright (C) 1989 by MAEDA Atusi
;; Originally written by MAEDA Atusi
;; Modified by Hideto Sazuka Thu Jun 29 12:09:36 1989
;; Modified by MAEDA Atusi Thu Jun 29 20:50:16 1989
;; Modified by MAEDA Atusi Wed Jul  5 20:21:31 1989

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

(provide 'getris)
;(require 'boss)

;;; User customizable variables.

(defvar getris-initial-delay 200
  "*Delay count to control the speed of getris game.  Bigger means slower.
You should substitute this value according to your system's performance.")

(defvar getris-min-delay 2
  "*Minimum delay count to control the maximum speed of getris game.
Smaller means faster.  The default value, 2, means `as fast as possible.'")

(defvar getris-acceleration 200
  "*Acceleration rate of getris game.
Smaller value means quicker speed-up.  This value is performance independent.")

(defvar getris-high-score-file
  (or (getenv "GETRISFILE")
      "$HOME/.getris")
  "*File name where top ten scores of getris are recorded.
Initialized from GETRISFILE environment variable.
Nil means does not record scores.")

(defvar getris-block-string
  (if (and (boundp kanji-flag) kanji-flag) "\242\243" "[]")
  "*String for getris block.  Must be width of two column.")

(defvar getris-width 10
  "*Width of getris board (number of blocks).  Each block occupies two
column width on window.")

(defvar getris-use-full-window nil
  "*Non-nil means that starting Getris game deletes other windows.")

(defun getris ()
  "Clone of a famous Russian game program."
  (interactive)
  (setq getris-previous-window-configuration
	(current-window-configuration))
  (switch-to-buffer "*Getris*")
  (getris-mode)
  (getris-startup))

;;; Internal variables.

(defvar getris-command-vector nil
  "Vector of functions which maps character to getris command.")

(defvar getris-mode-map nil)

(defvar getris-piece-data nil
  "Vector of piece data.
Each element of this vector is vector of size four, which correspond
to four directions of piece.  And each element of four size vectors is
a list of form:
	(max-y-offset (x1 . y1) (x2 . y2) (x3 . y3) (x4 . y4))
where:
	(x1 . y1) ... (x4 . y4) are offsets of dot from imaginary `origin'
				at upper-left side of the piece,
	0 <= y[1-4] <= max-y-offset.")

(defvar getris-left-margin)
(defvar getris-height)
(defvar getris-previous-window-configuration nil)
(defvar getris-blank-line)
(defvar getris-complete-line)
(defvar getris-line-length)

(defun getris-startup ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (insert (substitute-command-keys "

<<< G E T R I S >>>

Clone of a famous Russian game program.

Originally written by
MAEDA Atusi
mad@nakanishi.math.keio.junet


<Type \\[getris-mode-help] for help, \\[getris-start] to start game.>
"))
  (center-region (point-min) (point-max))
  (setq buffer-read-only t))

(defun getris-mode-help ()
  (interactive)
  (message (concat
	    (substitute-command-keys "\\[getris-mode-help]:Print this  ")
	    (substitute-command-keys "\\[getris-start]:Start new game  ")
	    (substitute-command-keys "\\[getris-help]:List action keys  ")
	    (substitute-command-keys "\\[boss-has-come]:Boss has come!  ")
	    (substitute-command-keys "\\[getris-exit]:Exit"))))

(or getris-mode-map
    (progn
      (setq getris-mode-map (make-sparse-keymap))
      (define-key getris-mode-map "?" 'getris-mode-help)
      (define-key getris-mode-map "\C-m" 'getris-start)
      (define-key getris-mode-map "h" 'getris-help)
      (define-key getris-mode-map "\e" 'boss-has-come)
      (define-key getris-mode-map "q" 'getris-exit)))

(defun getris-help ()
  (interactive)
  (message "j:Left  k:Rotate  l:Right  Space:Drop  ESC:Escape  q:Exit"))

(or getris-command-vector
    (progn
      (setq getris-command-vector (make-vector 256 'getris-help))
      (aset getris-command-vector ?j 'getris-move-left)
      (aset getris-command-vector ?k 'getris-rotate)
      (aset getris-command-vector ?l 'getris-move-right)
      (aset getris-command-vector ?  'getris-drop)
      (aset getris-command-vector ?q 'getris-quit)
      (aset getris-command-vector ?\e 'getris-boss-has-come)))

(defun getris-mode ()
  "Major mode for playing getris game.
\\{getris-mode-map}
Type \\[getris-help] for key action in the game.
Entry to this mode calls the value of getris-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'global-mode-string)
  (setq major-mode 'getris-mode)
  (setq mode-name "Getris")
  (use-local-map getris-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)
  (getris-mode-help)
  (run-hooks 'getris-mode-hook))

(defun getris-start ()
  (interactive)
  (switch-to-buffer "*Getris*")
  (if getris-use-full-window
      (delete-other-windows)
    ;; Enlarge window size if necessary.
    (progn
      (getris-get-window-size)
      (if (< getris-left-margin 5)
	  (enlarge-window (1+ (* 2 (- 5 getris-left-margin))) t))
      (if (< getris-height 20)
	  (enlarge-window (- 20 getris-height)))))
  (getris-get-window-size)		;again
  (if (or (< getris-height 20)
	  (< getris-left-margin 5))
      (error "Window size too small for getris."))
  (let ((left-margin-space (make-string (1- getris-left-margin) ? )))
    (setq getris-blank-line
	  (concat left-margin-space "||"
		  (make-string (* 2 getris-width) ? ) "||\n"))
    (setq getris-complete-line
	  (regexp-quote (concat left-margin-space "||"
				(getris-repeat-string getris-block-string
						      getris-width)
				"||")))
    (setq getris-line-length (length getris-blank-line))
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i 0))
      (while (< i getris-height)
	(insert getris-blank-line)
	(setq i (1+ i))))
    (insert (concat left-margin-space
		    (make-string (+ 4 (* 2 getris-width)) ?=))))
  (random t)				;randomize by current time
  (catch 'getris-quit-tag
    (getris-main-loop)
    (getris-mode-help)))

(defun getris-get-window-size ()
  (setq getris-height (- (window-height) 2))
  (setq getris-left-margin
	(/ (- (window-width)
	      (* 2 getris-width)
	      4)
	   2)))

(defun getris-repeat-string (string times)
  (let ((result ""))
    (while (> times 0)
      (setq result (concat string result))
      (setq times (1- times)))
    result))

(defun getris-exit ()
  (interactive)
  (set-window-configuration getris-previous-window-configuration))

(defun abs (number)
  (if (< number 0)
      (- number)
    number))

(defun getris-main-loop ()
  (let ((delay getris-initial-delay)
	(score 0)
	(loop-count 0)
	(center (+ getris-left-margin (logior getris-width 1) -2))
	(disp (/ getris-width 4))
	delay-count
	x y direction kind)
    (while (progn (setq x (+ center (ash (mod (random) disp) 1))
			y -1
			direction (mod (abs (random)) 4)
			piece-num (mod (abs (random)) 7)
			piece-vector (aref getris-piece-data piece-num)
			piece-data (aref piece-vector direction))
		  (getris-puttable-p x 0 piece-data))
      (while (getris-puttable-p x (1+ y) piece-data)
	(getris-set-piece x (setq y (1+ y)) piece-data)
	(setq delay (max getris-min-delay
			 2
			 (- getris-initial-delay
			    (/ loop-count
			       getris-acceleration))))
	(setq delay-count delay)
	(while (> (setq delay-count (1- delay-count))
		  0)
	  (setq loop-count (1+ loop-count))
	  (if (input-pending-p)
	      ;; Execute a command.
	      ;; Variable values may be modified.
	      (funcall (aref getris-command-vector (read-char)))))
	(getris-unset-piece x y piece-data))
      (getris-set-piece x y piece-data)
      (setq score (+ score (car piece-data)
		     (getris-test-delete-line y piece-data)))
      (getris-show-score))
    (end-of-line 1)
    (insert "*** GAME OVER ***")
    (setq buffer-read-only t)
    (if getris-high-score-file
	(getris-show-high-score))))

(defmacro getris-goto-x-y (x y)
  (`(goto-char (+ (* (, y) getris-line-length)
		  (, x)
		  1))))

(defmacro sit-for-getris (n)
  (`(progn (goto-char (point-min))
	   (sit-for (, n)))))

(defun getris-puttable-p (x y piece-data)
  (let ((result t))
    (while (and (setq piece-data (cdr piece-data)) result)
      (getris-goto-x-y (+ x (car (car piece-data)))
		       (+ y (cdr (car piece-data))))
      (if (not (= (following-char) ? ))
	  (setq result nil)))
    result))

(defun getris-set-piece (x y piece-data)
  (while (setq piece-data (cdr piece-data))
    (getris-goto-x-y (+ x (car (car piece-data)))
		     (+ y (cdr (car piece-data))))
    (delete-char 2)
    (insert getris-block-string)
  (sit-for-getris 0)))

(defun getris-unset-piece (x y piece-data)
  (while (setq piece-data (cdr piece-data))
    (getris-goto-x-y (+ x (car (car piece-data)))
		     (+ y (cdr (car piece-data))))
    (delete-char 2)
    (insert "  ")))

(defun getris-test-delete-line (y piece-data)
  (let ((max-y (+ y (car piece-data)))
	(lines-deleted 0))
    (while (<= y max-y)
      (getris-goto-x-y 0 y)
      (if (looking-at getris-complete-line)
	  (progn (setq lines-deleted (1+ lines-deleted))
		 (ding)
		 (delete-region (point)
				(progn (next-line 1) (point)))
		 (insert getris-blank-line)
		 (sit-for 1)
		 (delete-region (point)
				(progn (previous-line 1) (point)))
		 (goto-char (point-min))
		 (insert getris-blank-line)
		 (sit-for-getris 0)))
      (setq y (1+ y)))
    (* lines-deleted lines-deleted lines-deleted)))

(defun getris-show-score ()
  (setq global-mode-string (format "Score: %d" score))
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun getris-show-high-score ()
  (let ((file (substitute-in-file-name getris-high-score-file)))
    (find-file-other-window file)
    (goto-char (point-max))
    (insert (format "  %08d %20s at %s on %s\n"
		    score
		    (user-full-name)
		    (current-time-string)
		    (system-name)))
    (sort-fields -1 (point-min) (point-max))
    (goto-line 11)
    (move-to-column 0)
    (delete-region (point) (point-max))
    (write-file file)
    (goto-char (point-min))
    (pop-to-buffer "*Getris*")))

(defun getris-move-left ()
  (getris-unset-piece x y piece-data)
  (getris-set-piece
   (if (getris-puttable-p (- x 2) y piece-data)
       (setq x (- x 2))
     x)
   y piece-data))

(defun getris-move-right ()
  (getris-unset-piece x y piece-data)
  (getris-set-piece
   (if (getris-puttable-p (+ x 2) y piece-data)
       (setq x (+ x 2))
     x)
   y piece-data))

(defun getris-rotate ()
  (let ((new-direction (if (= direction 3)
			   0
			 (1+ direction))))
    (getris-unset-piece x y piece-data)
    (getris-set-piece
     x y
     (if (getris-puttable-p x y (aref piece-vector new-direction))
	 (setq piece-data
	       (aref piece-vector (setq direction new-direction)))
       piece-data))))

(defun getris-drop ()
  (getris-unset-piece x y piece-data)
  (while (getris-puttable-p x (1+ y) piece-data)
    (setq y (1+ y)))
  (setq delay-count delay))

(defun getris-quit ()
  (if (y-or-n-p "Are you sure to quit Getris? ")
      (progn
	(setq buffer-read-only t)
	(throw 'getris-quit-tag (getris-exit)))))

(defun getris-boss-has-come ()
  ;; Need improvement.
  (save-window-excursion
    (boss-has-come)
    (local-set-key "\C-c\C-c" 'getris-boss-goes-away)
    (recursive-edit)))

(defun getris-boss-goes-away ()
  (interactive)
  (boss-goes-away)
  (exit-recursive-edit))

(defun getris-make-piece-data (raw-piece-data)
  (setq getris-piece-data (make-vector (length raw-piece-data) nil))
  (let ((kind 0))
    (while raw-piece-data
      (let ((direction 0)
	    (four-list (car raw-piece-data))
	    (four-vector (make-vector 4 nil)))
	(while four-list
	  (let ((y 0)
		(piece-data nil)
		(max-y 0)
		(lines (car four-list)))
	    (while lines
	      (let ((x 0)
		    (line (car lines))
		    (len (length (car lines))))
		(while (< x len)
		  (if (= (aref line x) ?#)
		      (progn
			(setq piece-data
			      (cons (cons (+ x x) y) piece-data))
			(if (> y max-y)
			    (setq max-y y))))
		  (setq x (1+ x)))
		(setq lines (cdr lines)
		      y (1+ y))))
	    (aset four-vector direction (cons max-y piece-data)))
	  (setq four-list (cdr four-list)
		direction (1+ direction)))
	(aset getris-piece-data kind four-vector))
      (setq raw-piece-data (cdr raw-piece-data)
	    kind (1+ kind)))))

(or getris-piece-data
    (getris-make-piece-data
     '(
       ;; ####
       ( (""
	  ""
	  "####"
	  "")
	 (" #"
	  " #"
	  " #"
	  " #")
	 (""
	  "####"
	  ""
	  "")
	 ("  #"
	  "  #"
	  "  #"
	  "  #"))
       ;; ##
       ;; ##
       ( ("##"
	  "##")
	 ("##"
	  "##")
	 ("##"
	  "##")
	 ("##"
	  "##"))
       ;; ##
       ;;  ##
       ( ("##"
	  " ##")
	 (" #"
	  "##"
	  "#")
	 ("##"
	  " ##")
	 (" #"
	  "##"
	  "#"))
       ;;  ##
       ;; ##
       ( (" ##"
	  "##")
	 ("#"
	  "##"
	  " #")
	 (" ##"
	  "##")
	 ("#"
	  "##"
	  " #"))
       ;;  #
       ;; ###
       ( (" #"
	  "###")
	 (" #"
	  "##"
	  " #")
	 (""
	  "###"
	  " #")
	 (" #"
	  " ##"
	  " #"))
       ;; ###
       ;; #
       ( (""
	  "###"
	  "#")
	 ("#"
	  "#"
	  "##")
	 ("  #"
	  "###")
	 (" ##"
	  "  #"
	  "  #"))
       ;; #
       ;; ###
       ( (""
	  "###"
	  "  #")
	 ("##"
	  "#"
	  "#")
	 ("#"
	  "###")
	 ("  #"
	  "  #"
	  " ##"))
       )))
