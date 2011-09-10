;;; snake.el -- Snake game

;;; Copyright (C) 1994 Alexander Rezinsky <alexr@msil.sps.mot.com>
;;; $Id: snake.el,v 1.13 1994/10/13 15:25:24 alexr Exp $
;;;
;;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Created:  15 August 1994
;;; Version:  1.1
;;; Keywords: games
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; COMMENTARY
;;; ----------
;;; It is a popular snake game.
;;;
;;; NOTE that boss.el package is required. It may be found:
;;; archive.cis.ohio-state.edu: pub/gnu/emacs/elisp-archives/games/boss.el.Z

;;; INSTALLATION
;;; ------------
;;; Load this file and type \M-x snake to start game.
;;; It will be more effectively if you byte-compile it!

;;; LCD Archive Entry:
;;; snake|Alex Rezinsky|alexr@msil.sps.mot.com|
;;; Snake game|
;;; 13-Oct-1994|1.1|~/games/snake.el.Z|

;;; HISTORY
;;; -------
;;; v1.0 August 15 1994 Alex Rezinsky
;;;   First release.
;;; v1.1 October 13 1994 Alex Rezinsky
;;;   Record table added, some bugs fixed

;;; THANKS TO
;;; ---------
;;; MAEDA Atusi <mad@math.keio.JUNET>
;;;     "boss has come" code have been borrowed from his package
;;;     getris.el
;;;
;;; Eberhard Mattes <mattes@azu.informatik.uni-stuttgart.de>
;;;     Bug in eating several foods and in increasing length fixed.

;;; Variables for customization
;;; ---------------------------
;;;  
(defvar snake-use-window 'full "Method of using window:
nil      - according to snake-H and snake-W variables,
'full    - use full screen, delete all other windows,
'current - use current window.")

(defvar snake-H                 25
  "Default height of snake window.")

(defvar snake-W                 75
  "Default width of snake window.")

(defvar snake-delay             0.1
  "Delay in seconds to control the speed of snake game. Bigger means slower.")

(defvar snake-increase-freq     100
  "Number of moves after it your snake will be increased,
even without nutrition")

(defvar snake-food-prob         '(1 100)
  "Food probability. Must be pair (NUMBER-OF-FOOD NUMBER-OF-STEPS). So
every NUMBER-OF-STEPS moves, NUMBER-OF-FOOD foods will appear. May be
nil - in this case this criterion of food appearance does not work.")

(defvar snake-food-always       1
  "Number of food always present on screen. May be 0  - in this case
this criterion of food appearance does not work.")

(defvar snake-mangust-prob      '(1 1000)
  "Mangust probability. Must be pair (NUMBER-OF-MANG NUMBER-OF-STEPS). So
every NUMBER-OF-STEPS moves, NUMBER-OF-MANG mangusts will appear. May be
nil - in this case mangusts not appear.")

(defvar snake-score-single-step 1
  "Number of points for single step.")

(defvar snake-score-increase    100
  "Number of points for one snake increase.")

(defvar TURN-L ?k "Your turn left key.")

(defvar TURN-R ?l "Your turn right key.")

(defvar snake-score-file "/tmp/snake-record-table" 
  "File to record best results")

(defvar snake-score-number 10 "Number of best results to be recorded.")

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(defvar snake-body ())
(defvar snake-direction 0)
(defvar snake-food-count 0)
(defvar snake-h)
(defvar snake-head)
(defvar snake-mode-map nil)
(defvar snake-old-window-conf)
(defvar snake-score 0)
(defvar snake-score-string)
(defvar snake-step-number 0)
(defvar snake-tail-on-place 0)
(defvar snake-w)

(require 'boss)
(provide 'snake)

(or snake-mode-map
    (progn
      (setq snake-mode-map (make-sparse-keymap))
      (suppress-keymap snake-mode-map t)
      (substitute-key-definition nil        'snake-dummy snake-mode-map)
      (substitute-key-definition 'undefined 'snake-dummy snake-mode-map)
      (define-key snake-mode-map "q"        'snake-exit)
      (define-key snake-mode-map "r"        'snake-restart)
    )
)

(defun snake ()
  "Snake game."
  (interactive nil)
  (setq snake-old-window-conf (current-window-configuration))
  (switch-to-buffer "*Snake*")
  (snake-mode)
  (snake-setup)
  (snake-loop)
)

(defun snake-mode ()
  "Snake game mode."
  (interactive nil)
  (kill-all-local-variables)
  (setq major-mode 'snake-mode)
  (setq mode-name "Snake")
  (use-local-map snake-mode-map)
  (buffer-disable-undo (current-buffer))
  (make-local-variable 'global-mode-string)
  (setq global-mode-string 'snake-score-string)
)

(defun snake-dummy ()
  "Default snake mode binding."
  (interactive nil)
  (message "q - exit,  r - restart.")
)

(defun snake-exit ()
  "Exit form snake game."
  (interactive nil)
  (kill-buffer "*Snake*")
  (set-window-configuration snake-old-window-conf)
)

(defun snake-restart ()
  "Restart snake game."
  (interactive nil)
  (snake-setup)
  (snake-loop)
)

(defun snake-setup ()
  "Snake game initialization."
  (random t)
  (setq snake-direction (random 3))
  (cond 
   ((eq snake-use-window 'full)
    (delete-other-windows)
    (snake-setup-window (window-width) (window-height)))
   ((eq snake-use-window 'current)
    (snake-setup-window (window-width) (window-height)))
   (t
    (snake-setup-window snake-W snake-H))
  )
  (setq snake-head (+ (* (/ (- snake-h 3) 2) snake-w) (/ snake-w 2)))
  (setq snake-body (list snake-head))
  (setq snake-tail-on-place 0)
  (setq snake-food-count 0)
  (setq snake-score 0)
  (setq snake-step-number 0)
)

(defun snake-setup-window (w h)
  "Snake game window initialization."
  (setq snake-w w)
  (setq snake-h h)
  (erase-buffer)
  (goto-char (point-min))
  (insert "+")
  (insert-char ?- (- w 3))
  (insert "+\n")
  (let ((i (- h 3)))
    (while (> i 0)
      (insert "|")
      (insert-char ?. (- w 3))
      (insert "|\n")
      (setq i (1- i))
    )
  )
  (insert "+")
  (insert-char ?- (- w 3))
  (insert "+\n")
)

(defun snake-loop ()
  "Main snake loop."
  (message "%c - left,  %c - right,  ANY-OTHER - boss has come." TURN-L TURN-R)
  (let ((endgame t)
        (rlist '(0 "" ""))
        (rlist-tmp nil)
        (rlistnew nil)
        (i 0)
        (j 0)
        (truncate-partial-width-windows t)
       )
    ;
    ; Main snake loop
    ;
    (while endgame
      (if (sit-for snake-delay)
          ()
        (let ((rchar (read-char)))
          (if (eq rchar TURN-R)
            (setq snake-direction (mod (1+ snake-direction) 4))
            (if (eq rchar TURN-L)
              (setq snake-direction (mod (1- snake-direction) 4))
              (snake-boss-has-come)
            )
          )
        )
      )
      (setq endgame (snake-execute-step))
      (setq snake-score-string (format "Score %06d" snake-score))
      (force-mode-line-update)
    )
    ;
    ; End of main snake loop
    ;
    (ding)

    ; Read previous record table
    (and
     (file-exists-p   snake-score-file)
     (file-writable-p snake-score-file)
     (load-file       snake-score-file)
    )

    ; Insert current result to appropriate place in record table
    (setq rlist-tmp rlist)
    (setq rlistnew () i 0)
    (while (and rlist-tmp (< i snake-score-number))
      (if (> snake-score (car rlist-tmp))
          (progn
            (setq rlistnew (append rlistnew 
                                   (list snake-score 
                                         (concat 
                                          (substring (current-time-string) 11 16)
                                          (substring (current-time-string) 3 10)
                                          (substring (current-time-string) 19)
                                         )
                                         (user-full-name))))
            (setq snake-score 0)
            (setq j i)
          )
        (setq rlistnew (append rlistnew (list (car rlist-tmp)
                                              (car (cdr rlist-tmp))
                                              (car (cdr (cdr rlist-tmp))))))
        (setq rlist-tmp (cdr (cdr (cdr rlist-tmp))))
      )
      (setq i (1+ i))
    )

    ; If current result was inserted to record table, display it.
    (if (not (equal rlist rlistnew))
        (progn
          (set-buffer (get-buffer-create "*snake-temp*"))
          (insert (format "(setq rlist\n      '%s\n)\n" 
                          (prin1-to-string rlistnew)))
          (write-region (point-min) (point-max) snake-score-file)
          (call-process shell-file-name nil nil nil
                        "-c" (concat "chmod 777 " snake-score-file))
          (kill-buffer "*snake-temp*")
          (switch-to-buffer "*Snake records*")
          (setq truncate-lines t)
          (insert "\n\nPlace  Score   Time  Date   Year  Who\n\n")
          (setq i 0)
          (while (and rlistnew (/= (car rlistnew) 0) (< i snake-score-number))
            (insert (format "%s%2d   %06d  %s  %s"
                            (if (= i j) "->" "  ")
                            (1+ i)
                            (car rlistnew)
                            (car (cdr rlistnew))
                            (car (cdr (cdr rlistnew)))))
             (setq rlistnew (cdr (cdr (cdr rlistnew))))
             (if (= i j)
                 (insert "   <- You are here, congratulations !\n")
               (insert "\n")
             )
             (setq i (1+ i))
          )
          (message "Press \"q\" to exit from record table ...")
          (while (/= (read-char-exclusive) ?q)
            (message "Press \"q\" to exit from record table ...")
          )
          (kill-buffer "*Snake records*")
        )
    )
    (snake-dummy)
  )
)

(defun snake-execute-step ()
  "Execute single step."
  (if (snake-new-head-position)
    (progn

      ; Execute move
      (goto-char snake-head)
      (delete-char 1)
      (insert-char ?# 1)
      (goto-char 1)
      (setq snake-body (append snake-body (list snake-head)))
      (if (= snake-tail-on-place 0)
        (progn
          (goto-char (car snake-body))
          (delete-char 1)
          (insert-char ?. 1)
          (goto-char 1)
          (setq snake-body (cdr snake-body))
          (setq snake-score (+ snake-score snake-score-single-step))
        )
        (setq snake-tail-on-place (1- snake-tail-on-place))
        (setq snake-score (+ snake-score snake-score-increase))
      )
      (sit-for 0)
      (setq snake-step-number (1+ snake-step-number))
      
      ; Increase snake length ?
      (if (eq (mod snake-step-number snake-increase-freq) 0)
        (setq snake-tail-on-place (1+ snake-tail-on-place))
      )

      ; Add food ?
      (let ((food (- snake-food-always snake-food-count)))
        (if (< food 0) (setq food 0))
        (setq food 
              (+
               food
               (if (and snake-food-prob
                        (eq (mod snake-step-number (car (cdr snake-food-prob)))
                            0))
                 (car snake-food-prob)
                 0
               )
              )
        )
        (while (> food 0)
          (let ((food-place (snake-random snake-w (* (- snake-h 2) snake-w))))
            (if (eq (char-after food-place) ?.)
              (progn
                (goto-char food-place)
                (delete-char 1)
                (insert-char (snake-random ?1 ?9) 1)
                (goto-char 1)
                (setq snake-food-count (1+ snake-food-count))
                (setq food (1- food))
              )
            )
          )
        )
      )

      ; Add mangust ?
      (if (and snake-mangust-prob
               (eq (mod snake-step-number (car (cdr snake-mangust-prob))) 0))
        (let ((mangust (car snake-mangust-prob)))
          (while (> mangust 0)
            (let ((mangust-place (snake-random snake-w 
                                               (* (- snake-h 2) snake-w))))
              (if (eq (char-after mangust-place) ?.)
                (progn
                  (goto-char mangust-place)
                  (delete-char 1)
                  (insert-char ?M 1)
                  (goto-char 1)
                )
              )
            )
            (setq mangust (1- mangust))
          )
        )
      )

      t
    )
    nil
  )
)

(defun snake-new-head-position ()
  "Calculate new head position."
  (let ((cur-char))
    (cond
     ((= snake-direction 0)
      (setq snake-head (- snake-head snake-w))
     )
     ((= snake-direction 1)
      (setq snake-head (1+ snake-head))
     )
     ((= snake-direction 2)
      (setq snake-head (+ snake-head snake-w))
     )
     ((= snake-direction 3)
      (setq snake-head (1- snake-head))
     )
    )
    (setq cur-char (char-after snake-head))
    (if (and (>= cur-char ?0) (<= cur-char ?9))
      (progn
        (setq snake-tail-on-place (+ snake-tail-on-place (- cur-char ?0)))
        (setq snake-food-count (1- snake-food-count))
        t
      )
      (if (= cur-char ?.)
        t
        nil
      )
    )
  )
)

(defun snake-random (from to)
  "Return random number in interval [from,to]."
  (+ from (random (1+ (- to from))))
)
(defun snake-boss-has-come ()
  (save-window-excursion
    (boss-has-come)
    (local-set-key "\C-c\C-c" 'snake-boss-goes-away)
    (recursive-edit)
  )
)
(defun snake-boss-goes-away ()
  (interactive)
  (boss-goes-away)
  (exit-recursive-edit)
)

;;; end of snake.el
