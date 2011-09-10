;;;
;;; Copyright (C) 1992 Kevin A. Mocklin & Richard J. Resnick
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to mocklin@titan.ucc.umass.edu
;;;                               or rjr@titan.ucc.umass.edu
;;;                               or zappo@gnu.ai.mit.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                          TYRANT MODE GAME                            ;;;
;;;                                                                      ;;;
;;;  This program contains a program designed for use with               ;;;
;;; tyrant-mode.  This program may be used under the following           ;;;
;;; software conditions.                                                 ;;;
;;;                                                                      ;;;
;;;  Under tyrant-mode for TALK.  To run this way, use "talk"            ;;;
;;;  and once a connection is established, use the game playing          ;;;
;;;  function "talk-initiate-special-function" bound to C-c g            ;;;
;;;  to start.  Must be installed on all systems.                        ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'battleship)

(defvar battleship-map nil
  "Keymap used in playing battleship")

(if battleship-map
    ()
  (setq battleship-map (make-sparse-keymap))
  (define-key battleship-map "b" 'battleship-move-left)
  (define-key battleship-map "\C-b" 'battleship-move-left)
  (define-key battleship-map "j" 'battleship-move-left)
  (define-key battleship-map "f" 'battleship-move-right)
  (define-key battleship-map "\C-f" 'battleship-move-right)
  (define-key battleship-map "l" 'battleship-move-right)
  (define-key battleship-map "p" 'battleship-move-up)
  (define-key battleship-map "\C-p" 'battleship-move-up)
  (define-key battleship-map "i" 'battleship-move-up)
  (define-key battleship-map "n" 'battleship-move-down)
  (define-key battleship-map "\C-n" 'battleship-move-down)
  (define-key battleship-map "k" 'battleship-move-down)
  (define-key battleship-map " " 'battleship-shoot-here)
  (define-key battleship-map "\C-M" 'battleship-shoot-here)
)
(defconst current-player-home 689)
(defconst other-player-home 731)
(defconst battleship-coord2point
  [ [ 280  282  284  286  288  290  292  294  296  298  300  302  304  306  308  310  322  324  326  328  330  332  334  336  338  340  342  344  346  348  350  352 ]
    [ 359  361  363  365  367  369  371  373  375  377  379  381  383  385  387  389  401  403  405  407  409  411  413  415  417  419  421  423  425  427  429  431 ]
    [ 438  440  442  444  446  448  450  452  454  456  458  460  462  464  466  468  480  482  484  486  488  490  492  494  496  498  500  502  504  506  508  510 ] 
    [ 517  519  521  523  525  527  529  531  533  535  537  539  541  543  545  547  559  561  563  565  567  569  571  573  575  577  579  581  583  585  587  589 ]
    [ 596  598  600  602  604  606  608  610  612  614  616  618  620  622  624  626  638  640  642  644  646  648  650  652  654  656  658  660  662  664  666  668 ]
    [ 675  677  679  681  683  685  687  689  691  693  695  697  699  701  703  705  717  719  721  723  725  727  729  731  733  735  737  739  741  743  745  747 ]
    [ 754  756  758  760  762  764  766  768  770  772  774  776  778  780  782  784  796  798  800  802  804  806  808  810  812  814  816  818  820  822  824  826 ]
    [ 833  835  837  839  841  843  845  847  849  851  853  855  857  859  861  863  875  877  879  881  883  885  887  889  891  893  895  897  899  901  903  905 ]
    [ 912  914  916  918  920  922  924  926  928  930  932  934  936  938  940  942  954  956  958  960  962  964  966  968  970  972  974  976  978  980  982  984 ]
    [ 991  993  995  997  999  1001 1003 1005 1007 1009 1011 1013 1015 1017 1019 1021 1033 1035 1037 1039 1041 1043 1045 1047 1049 1051 1053 1055 1057 1059 1061 1063 ]
    [ 1070 1072 1074 1076 1078 1080 1082 1084 1086 1088 1090 1092 1094 1096 1098 1100 1112 1114 1116 1118 1120 1122 1124 1126 1128 1130 1132 1134 1136 1138 1140 1142 ] ]
  "Array to tell me where to print a piece.")

(defconst battleship-hit "#")
(defconst battleship-miss "*")
(defconst battleship-destroyer "d")
(defconst battleship-cruiser "c")
(defconst battleship-carrier "a")
(defconst battleship-battleship "b")
(defconst battleship-submarine "s")
(defconst place-ship-mode-buffer-name "Placing Ship Mode")
(defconst battleship-buffer-name "BATTLESHIP (BETA version)")
(defconst battleship-minibuffer-name "Battleship Information")

(defun battleship ()
  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create battleship-buffer-name)))
  (setq mode-name "Battleship")
  (setq major-mode 'battleship)
    ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (goto-char (point-min))
	   (delete-region (point) (save-excursion
				    (end-of-line)
				    (point)))
	   (insert (format "%28s           |%27s            " 
			   (tyrant-format "%U's territory")
			   (tyrant-format "%u's ships")))
	   (setq player 1)
	   (setq talk-tyrant-enabled-console nil)
	   (setq tyrant-call-interpreter 'get-answer)
	   (message "You are player 1")
	   (switch-to-buffer (set-buffer (get-buffer-create place-ship-mode-buffer-name)))
	   (setq mode-name "Placing Ship Mode")
	   (delete-region (point-min) (point-max))
	   (display-place-ship-buf)))
  (setq tyrant-player2-hook
	'(lambda ()
	   (goto-char (point-min))
	   (delete-region (point) (save-excursion
				    (end-of-line)
				    (point)))
	   (insert (format "%28s           |%27s            " 
			   (tyrant-format "%u's territory")
			   (tyrant-format "%U's ships")))
	   (setq player 2)
	   (setq talk-tyrant-enabled-console nil)
	   (setq tyrant-call-interpreter 'get-answer)
	   (message "Your are player 2")
	   (switch-to-buffer (set-buffer (get-buffer-create place-ship-mode-buffer-name)))
	   (setq mode-name "Placing Ship Mode")
	   (delete-region (point-min) (point-max))
	   (display-place-ship-buf)))

  (delete-region (point-min) (point-max))
  (insert "          Opponent Screen              |                Your Screen            
                                       |
   a b c d e f g h i j k l m n o p     |     a b c d e f g h i j k l m n o p
  +-------------------------------+    |    +-------------------------------+
 1|. . . . . . . . . . . . . . . .|1   |   1|. . . . . . . . . . . . . . . .|1
 2|. . . . . . . . . . . . . . . .|2   |   2|. . . . . . . . . . . . . . . .|2
 3|. . . . . . . . . . . . . . . .|3   |   3|. . . . . . . . . . . . . . . .|3
 4|. . . . . . . . . . . . . . . .|4   |   4|. . . . . . . . . . . . . . . .|4
 5|. . . . . . . . . . . . . . . .|5   |   5|. . . . . . . . . . . . . . . .|5
 6|. . . . . . . . . . . . . . . .|6   |   6|. . . . . . . . . . . . . . . .|6
 7|. . . . . . . . . . . . . . . .|7   |   7|. . . . . . . . . . . . . . . .|7
 8|. . . . . . . . . . . . . . . .|8   |   8|. . . . . . . . . . . . . . . .|8
 9|. . . . . . . . . . . . . . . .|9   |   9|. . . . . . . . . . . . . . . .|9
 a|. . . . . . . . . . . . . . . .|a   |   a|. . . . . . . . . . . . . . . .|a
 b|. . . . . . . . . . . . . . . .|b   |   b|. . . . . . . . . . . . . . . .|b
  +-------------------------------+    |    +-------------------------------+
   a b c d e f g h i j k l m n o p     |     a b c d e f g h i j k l m n o p
                                       |
  # - Hit    * - Miss   . - Unknown    |  # - Hit    * - Miss   . - Empty Water
                                       |
")
  (delete-other-windows (selected-window))
  (setq window-min-height 2)
  (split-window (selected-window) 21)
  (other-window 1)
  (switch-to-buffer (set-buffer (get-buffer-create battleship-minibuffer-name)))
  (setq mode-name "Battleship Minibuffer")
  (insert (format "%-80s"))   ;;fill the info-buffer with blank string
  (other-window 1)
  (switch-to-buffer (set-buffer battleship-buffer-name))
  (put-in-battleship-minibuffer "all" "[j,k,l,i] move [t] switches orientation [space bar] places ship"	battleship-buffer-name)
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
	"Keys: [j C-b] left [l C-f] right [i C-p] up [k C-n] down [spc] shoot")
  (setq tyrant-turn 1)
  (make-local-variable 'player1-hits)
  (setq player1-hits (copy-sequence [ () () () () () ]))
  (make-local-variable 'player2-hits)
  (setq player2-hits (copy-sequence [ () () () () () ]))
  (setq horizontal-direction t)
  (setq destroyer-place-mode-home 347)
  (setq battleship-place-mode-home 353)
  (setq submarine-place-mode-home 361)
  (setq cruiser-place-mode-home 369)
  (setq carrier-place-mode-home 379)
  (use-local-map battleship-map))
;; end of function 'battleship
;; *****************************************************


(defun battleship-move-right ()
  "Move the cursor right."

  (interactive)
  (if (= current-player-col 15)
      (error "Those are uncharted waters. You can't go there!"))
  (if talk-tyrant-enabled-console
      (goto-char (aref (aref battleship-coord2point current-player-row) (+ 1 current-player-col)))
    (goto-char (aref (aref battleship-coord2point other-player-row) (+ 1 other-player-col))))
  (setq current-player-col (+ 1 current-player-col))
  (setq other-player-col (+ 1 other-player-col)))
;; end of function 'battleship-move-right
;;******************************************************


(defun battleship-move-left ()
  "Move the cursor left."

  (interactive)
  (if (= current-player-col 0)
      (error "Those are uncharted waters. You can't go there!"))
  (if talk-tyrant-enabled-console
      (goto-char (aref (aref battleship-coord2point current-player-row) (- current-player-col 1)))
    (goto-char (aref (aref battleship-coord2point other-player-row) (- other-player-col 1))))
  (setq current-player-col (- current-player-col 1))
  (setq other-player-col (- other-player-col 1)))
;; end of function 'battleship-move-left
;;******************************************************


(defun battleship-move-up ()
  "Move the cursor up."

  (interactive)
  (if (= current-player-row 0)
      (error "Those are uncharted waters. You can't go there!"))
  (if talk-tyrant-enabled-console
      (goto-char (aref (aref battleship-coord2point (- current-player-row 1)) current-player-col))
    (goto-char (aref (aref battleship-coord2point (- other-player-row 1)) other-player-col)))
  (setq current-player-row (- current-player-row 1))
  (setq other-player-row (- other-player-row 1)))
;; end of function 'battleship-move-up
;;******************************************************


(defun battleship-move-down ()
  "Move the cursor down."

  (interactive)
  (if (= current-player-row 10)
      (error "Those are uncharted waters. You can't go there!"))
  (if talk-tyrant-enabled-console
      (goto-char (aref (aref battleship-coord2point (+ 1 current-player-row)) current-player-col))
    (goto-char (aref (aref battleship-coord2point (+ 1 other-player-row)) other-player-col)))
  (setq current-player-row (+ 1 current-player-row))
  (setq other-player-row (+ 1 other-player-row)))
;; end of function 'battleship-move-down
;;******************************************************


(defun put-cursor (row col)
  "Move cursor to col and row."

  (interactive)
  (goto-char (aref (aref battleship-coord2point row) col)))
;; end of function 'put-cursor
;;******************************************************


(defun put-cursor-place-mode (row col)
  "Move cursor to col and row in place mode."

  (interactive)
  (goto-char (aref (aref place-coord2point row) col)))
;; end of function 'put-cursor-place-mode
;;******************************************************


(defun put-in-battleship-minibuffer (where text return_to)
  (other-window 1)
  (switch-to-buffer (set-buffer battleship-minibuffer-name))
  (cond ((equal where "left")
	 (goto-char (point-min))
	 (delete-region (point) 45)
	 (insert (format "%-44s" text)))
	((equal where "right")
	 (goto-char 46)
	 (delete-region (point) (point-max))
	 (goto-char 46)
	 (insert (format "%-32s" text)))
	((equal where "all")
	 (goto-char (point-min))
	 (delete-region (point-min) (point-max))
	 (insert (format "%-78s" text))))
  (other-window 1)
  (switch-to-buffer (set-buffer return_to)))
;; end of function 'put-in-battleship-minibuffer
;;******************************************************


(defun battleship-shoot-here ()
  "Shoot at cursor location."

  (interactive)
  (if (or (looking-at "*") (looking-at "#"))
      (error "You already went there!"))
  (if talk-tyrant-enabled-console
      ()
    (progn
      (make-local-variable 'index)
      (setq index 0)
      (make-local-variable 'ship-found)
      (make-local-variable 'ship-sunk)
      (setq ship-found 0)
      (setq ship-sunk 0)
      (while (and (< index 17) (= ship-found 0))
	(if (= tyrant-turn 1)
	    (if (= (aref (aref player-two-ships index) 0) other-player-row)
		(if (= (aref (aref player-two-ships index) 1) other-player-col)
		    (progn
		      (setq ship-found 1)
		      (cond ((equal (eval (aref (aref player-two-ships index) 2)) (eval battleship-destroyer))
			     (aset player2-hits 0 (cons battleship-destroyer (aref player2-hits 0)))
			     (if (= (length (aref player2-hits 0)) 2)
				 (setq ship-sunk 1)))
			    ((equal (eval (aref (aref player-two-ships index) 2)) (eval battleship-battleship))
			     (aset player2-hits 1 (cons battleship-battleship (aref player2-hits 1)))
			     (if (= (length (aref player2-hits 1)) 3)
				 (setq ship-sunk 1)))
			    ((equal (eval (aref (aref player-two-ships index) 2)) (eval battleship-submarine))
			     (aset player2-hits 2 (cons battleship-submarine (aref player2-hits 2)))
			     (if (= (length (aref player2-hits 2)) 3)
				 (setq ship-sunk 1)))
			    ((equal (eval (aref (aref player-two-ships index) 2)) (eval battleship-cruiser))
			     (aset player2-hits 3 (cons battleship-cruiser (aref player2-hits 3)))
			     (if (= (length (aref player2-hits 3)) 4)
				 (setq ship-sunk 1)))
			    ((equal (eval (aref (aref player-two-ships index) 2)) (eval battleship-carrier))
			     (aset player2-hits 4 (cons battleship-carrier (aref player2-hits 4)))
			     (if (= (length (aref player2-hits 4)) 5)
				 (setq ship-sunk 1)))))))
	  (if (= (aref (aref player-one-ships index) 0) other-player-row)
	      (if (= (aref (aref player-one-ships index) 1) other-player-col)
		  (progn
		    (setq ship-found 1)
		    (cond ((equal (eval (aref (aref player-one-ships index) 2)) (eval battleship-destroyer))
			   (aset player1-hits 0 (cons battleship-destroyer (aref player1-hits 0)))
			   (if (= (length (aref player1-hits 0)) 2)
			       (setq ship-sunk 1)))
			  ((equal (eval (aref (aref player-one-ships index) 2)) (eval battleship-battleship))
			   (aset player1-hits 1 (cons battleship-battleship (aref player1-hits 1)))
			   (if (= (length (aref player1-hits 1)) 3)
			       (setq ship-sunk 1)))
			  ((equal (eval (aref (aref player-one-ships index) 2)) (eval battleship-submarine))
			   (aset player1-hits 2 (cons battleship-submarine (aref player1-hits 2)))
			   (if (= (length (aref player1-hits 2)) 3)
			       (setq ship-sunk 1)))
			  ((equal (eval (aref (aref player-one-ships index) 2)) (eval battleship-cruiser))
			   (aset player1-hits 3 (cons battleship-cruiser (aref player1-hits 3)))
			   (if (= (length (aref player1-hits 3)) 4)
			       (setq ship-sunk 1)))
			  ((equal (eval (aref (aref player-one-ships index) 2)) (eval battleship-carrier))
			   (aset player1-hits 4 (cons battleship-carrier (aref player1-hits 4)))
			   (if (= (length (aref player1-hits 4)) 5)
			       (setq ship-sunk 1))))))))
	(setq index (+ index 1)))
      (setq index (- index 1))
      (cond 
       ((= ship-found 1)
	(tyrant-send-message "ship found")
	(delete-char 1)
	(insert battleship-hit)
	(put-in-battleship-minibuffer "left" "Oh, no!  He hit one of your ships!!" battleship-buffer-name)
	(if (= ship-sunk 1)
	    (progn
	      (tyrant-send-message "ship sunk")
	      (put-in-battleship-minibuffer "left" "H e  S u n k  T h a t  S h i p ! ! !" battleship-buffer-name))))
       ((not (= ship-found 1))
	(tyrant-send-message "ship not found")
	(delete-char 1)
	(insert battleship-miss)
	(put-in-battleship-minibuffer "left" "That was close!  He missed!!" battleship-buffer-name)))
      (tyrant-send-message "swap turns")
      (battleship-swap-turns))))
;; end of function 'battleship-shoot-here
;;******************************************************


(defun get-answer (msg)
  (cond ((equal msg "ship found")
	 (delete-char 1)
	 (insert battleship-hit)
	 (put-in-battleship-minibuffer "left" "Alright!  You hit one of his ships!!" battleship-buffer-name))
	((equal msg "ship sunk")
	 (put-in-battleship-minibuffer "left" "Y o u  S u n k  T h a t  S h i p ! ! !" battleship-buffer-name))
	((equal msg "ship not found")
	 (delete-char 1)
	 (insert battleship-miss)
	 (put-in-battleship-minibuffer "all" "Sorry, You Missed!!" battleship-buffer-name))
	((equal msg "place-mode-quit")
	 (delete-other-windows (selected-window))
	 (kill-buffer battleship-minibuffer-name)
	 (kill-buffer place-ship-mode-buffer-name)
	 (switch-to-buffer (set-buffer battleship-buffer-name))
	 (setq talk-tyrant-enabled-console t)
	 (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
	     (talk-usurp-tyrant)
	   (kill-buffer (current-buffer))))
	((equal msg "ready-to-play")
	 (if (= player 1)
	     (put-in-battleship-minibuffer "all" (tyrant-format "%U has finished placing ships and is ready to play!!") battleship-buffer-name)
	   (put-in-battleship-minibuffer "all" (tyrant-format "%u has finished placing ships and is ready to play!!") battleship-buffer-name)))
	((equal msg "swap turns")
	 (battleship-swap-turns))
	((equal msg "game over")
	 (battleship-quit))))
;; end of function 'get-answer
;;******************************************************


(defun setup-players-home ()
  (setq current-player-col 7)
  (setq current-player-row 5)
  (setq other-player-col 23)
  (setq other-player-row 5))
;; end of function 'setup-players-home
;;******************************************************


(defun check-for-win ()
  (cond ((= tyrant-turn 1)
	 (if (and (= (length (aref player1-hits 0)) 2)
		  (and (= (length (aref player1-hits 1)) 3)
		       (and (= (length (aref player1-hits 2)) 3)
			    (and (= (length (aref player1-hits 3)) 4)
				 (= (length (aref player1-hits 4)) 5)))))
	     t))
	((= tyrant-turn 2)
	 (if (and (= (length (aref player2-hits 0)) 2)
		  (and (= (length (aref player2-hits 1)) 3)
		       (and (= (length (aref player2-hits 2)) 3)
			    (and (= (length (aref player2-hits 3)) 4)
				 (= (length (aref player2-hits 4)) 5)))))
	     t))))
;; end of function 'check-for-win
;;******************************************************


(defun battleship-swap-turns ()
  "Swap turns in battleship"

  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  (if (check-for-win)
      (progn
	(tyrant-send-message "game over")
	(battleship-quit)))
  (setup-players-home)
  (if talk-tyrant-enabled-console
      (goto-char other-player-home)
    (goto-char current-player-home))
  (setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console))
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
      (put-in-battleship-minibuffer "right" (tyrant-format "%p's turn.") battleship-buffer-name)
    (message "Player %d's move." tyrant-turn)))
;; end of function 'battleship-swap-turns
;;******************************************************


(defun battleship-quit ()
  (interactive)
  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
      (put-in-battleship-minibuffer "all" (tyrant-format "                            %p is the winner of the game!!!!!!")
				    battleship-buffer-name)
    (message "PLAYER %d WINS THE GAME!!!" tyrant-turn))
  (sleep-for 4)
  (delete-other-windows (selected-window))
  (kill-buffer battleship-minibuffer-name)
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
      (talk-usurp-tyrant)
    (kill-buffer (current-buffer))))
;; end of function 'battleship-quit
;;******************************************************

;;===========================================================
;; The remaining functions all relate to the placing of ships
;;===========================================================


(defun display-place-ship-buf ()
  (interactive)
  (defvar place-ship-map nil)
  (setq place-ship-map (make-keymap))
  (suppress-keymap place-ship-map)
  (fillarray place-ship-map 'place-mode-wrong-keypress)
  (define-key place-ship-map "b" 'place-ships-move-left)
  (define-key place-ship-map "\C-b" 'place-ships-move-left)
  (define-key place-ship-map "j" 'place-ships-move-left)
  (define-key place-ship-map "f" 'place-ships-move-right)
  (define-key place-ship-map "\C-f" 'place-ships-move-right)
  (define-key place-ship-map "l" 'place-ships-move-right)
  (define-key place-ship-map "p" 'place-ships-move-up)
  (define-key place-ship-map "\C-p" 'place-ships-move-up)
  (define-key place-ship-map "i" 'place-ships-move-up)
  (define-key place-ship-map "n" 'place-ships-move-down)
  (define-key place-ship-map "\C-n" 'place-ships-move-down)
  (define-key place-ship-map "k" 'place-ships-move-down)
  (define-key place-ship-map "t" 'place-ships-flip)
  (define-key place-ship-map " " 'place-ship-here)
  (define-key place-ship-map "\C-M" 'place-ship-here)
  (define-key place-ship-map "\C-x" nil)
  (define-key place-ship-map "\e" nil)
  (define-key place-ship-map "\C-c" nil)
  (define-key place-ship-map "\C-z" nil)
  (define-key place-ship-map "\C-l" nil)
  (if (and (boundp 'talk-tyrant-vt100-enable) (not talk-tyrant-vt100-enable))
      ()
    (define-key place-ship-map "\e[A"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\e[B"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\e[C"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\e[D"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\eOA"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\eOB"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\eOC"  'place-ships-vt100-arrow)
    (define-key place-ship-map "\eOD"  'place-ships-vt100-arrow))
  (define-key place-ship-map "\C-c\C-c" 'quit-from-placing-mode)
  (use-local-map place-ship-map)
  (defconst place-coord2point
    [ [ 103 105 107 109 111 113 115 117 119 121 123 125 127 129 131 133 ]
      [ 139 141 143 145 147 149 151 153 155 157 159 161 163 165 167 169 ]
      [ 175 177 179 181 183 185 187 189 191 193 195 197 199 201 203 205 ]
      [ 237 239 241 243 245 247 249 251 253 255 257 259 261 263 265 267 ]
      [ 315 317 319 321 323 325 327 329 331 333 335 337 339 341 343 345 347 349 351 353 355 357 359 361 363 365 367 369 371 373 375 377 379 381 383 385 387 ]
      [ 393 395 397 399 401 403 405 407 409 411 413 415 417 419 421 423 ]
      [ 471 473 475 477 479 481 483 485 487 489 491 493 495 497 499 501 ]
      [ 507 509 511 513 515 517 519 521 523 525 527 529 531 533 535 537 ]
      [ 543 545 547 549 551 553 555 557 559 561 563 565 567 569 571 573 ]
      [ 579 581 583 585 587 589 591 593 595 597 599 601 603 605 607 609 ]
      [ 615 617 619 621 623 625 627 629 631 633 635 637 639 641 643 645 ] ] )
  
  (insert "          Place your ships

   a b c d e f g h i j k l m n o p
  +-------------------------------+
 1|. . . . . . . . . . . . . . . .|
 2|. . . . . . . . . . . . . . . .|
 3|. . . . . . . . . . . . . . . .|               H a r b o r
 4|. . . . . . . . . . . . . . . .\\-----------------------------------------+
 5|. . . . . . . . . . . . . . . . d d . b b b . s s s . c c c c . a a a a a|
 6|. . . . . . . . . . . . . . . ./-----------------------------------------+
 7|. . . . . . . . . . . . . . . .|
 8|. . . . . . . . . . . . . . . .|
 9|. . . . . . . . . . . . . . . .|
 a|. . . . . . . . . . . . . . . .|
 b|. . . . . . . . . . . . . . . .|
  +-------------------------------+
   a b c d e f g h i j k l m n o p
")
  (setq player-one-ships (copy-sequence [(copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence []) ] ))
  (setq player-two-ships (copy-sequence [(copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence [])
					 (copy-sequence []) ] ))
  ;;(fillarray player-one-ships (copy-sequence []))
  ;;(fillarray player-two-ships (copy-sequence []))
  (goto-char destroyer-place-mode-home)
  (setq ship-length 2)
  (setq current-ship-type battleship-destroyer)
  (if (= player 1)
      (setq player-one-ships
	    [ [ 4 16 battleship-destroyer ]
	      [ 4 17 battleship-destroyer ]
	      [ 4 19 battleship-battleship ]
	      [ 4 20 battleship-battleship ]
	      [ 4 21 battleship-battleship ]
	      [ 4 23 battleship-submarine ]
	      [ 4 24 battleship-submarine ]
	      [ 4 25 battleship-submarine ]
	      [ 4 27 battleship-cruiser ]
	      [ 4 28 battleship-cruiser ]
	      [ 4 29 battleship-cruiser ]
	      [ 4 30 battleship-cruiser ]
	      [ 4 32 battleship-carrier ]
	      [ 4 33 battleship-carrier ]
	      [ 4 34 battleship-carrier ]
	      [ 4 35 battleship-carrier ]
	      [ 4 36 battleship-carrier ] ])  
    (setq player-two-ships
	  [ [ 4 16 battleship-destroyer ]
	    [ 4 17 battleship-destroyer ]
	    [ 4 19 battleship-battleship ]
	    [ 4 20 battleship-battleship ]
	    [ 4 21 battleship-battleship ]
	    [ 4 23 battleship-submarine ]
	    [ 4 24 battleship-submarine ]
	    [ 4 25 battleship-submarine ]
	    [ 4 27 battleship-cruiser ]
	    [ 4 28 battleship-cruiser ]
	    [ 4 29 battleship-cruiser ]
	    [ 4 30 battleship-cruiser ]
	    [ 4 32 battleship-carrier ]
	    [ 4 33 battleship-carrier ]
	    [ 4 34 battleship-carrier ]
	    [ 4 35 battleship-carrier ]
	    [ 4 36 battleship-carrier ] ])))
;; end of function 'display-place-ship-buf
;;******************************************************

(defun place-ships-vt100-arrow ()
  "Recieve a vt100 arrow key?  Turn it into CTRL-F etc!"
  (interactive)
  (cond
   ((= last-input-char ?A)
    (place-ships-move-up))
   ((= last-input-char ?B)
    (place-ships-move-down))
   ((= last-input-char ?C)
    (place-ships-move-right))
   ((= last-input-char ?D)
    (place-ships-move-left))
   ))

(defun get-ship-row (ship-type)
  (cond ((equal ship-type battleship-destroyer)
	 (get-array-entry 0 0))
	((equal ship-type battleship-battleship)
	 (get-array-entry 2 0))
	((equal ship-type battleship-submarine)
	 (get-array-entry 5 0))
	((equal ship-type battleship-cruiser)
	 (get-array-entry 8 0))
	((equal ship-type battleship-carrier)
	 (get-array-entry 12 0))))
;; end of function 'get-ship-row
;;******************************************************


(defun get-ship-col (ship-type)
  (cond ((equal ship-type battleship-destroyer)
	 (get-array-entry 0 1))
	((equal ship-type battleship-battleship)
	 (get-array-entry 2 1))
	((equal ship-type battleship-submarine)
	 (get-array-entry 5 1))
	((equal ship-type battleship-cruiser)
	 (get-array-entry 8 1))
	((equal ship-type battleship-carrier)
	 (get-array-entry 12 1))))
;; end of function 'get-ship-col
;;******************************************************


(defun which-direction (ship-type)
  (cond ((equal ship-type battleship-destroyer)
	 (if (= (get-array-entry 0 0) (get-array-entry 1 0))
	     "horizontal"
	   "vertical"))
	((equal ship-type battleship-battleship)
	 (if (= (get-array-entry 2 0) (get-array-entry 3 0))
	     "horizontal"
	   "vertical"))
	((equal ship-type battleship-submarine)
	 (if (= (get-array-entry 5 0) (get-array-entry 6 0))
	     "horizontal"
	   "vertical"))
	((equal ship-type battleship-cruiser)
	 (if (= (get-array-entry 8 0) (get-array-entry 9 0))
	     "horizontal"
	   "vertical"))
	((equal ship-type battleship-carrier)
	 (if (= (get-array-entry 12 0) (get-array-entry 13 0))
	     "horizontal"
	   "vertical"))))
;; end of function 'which-direction
;;******************************************************


(defun draw-ship (ship-type ship-length)
  (make-local-variable 'row)
  (make-local-variable 'col)
  (make-local-variable 'index)
  (setq row (get-ship-row ship-type)) 
  (setq col (get-ship-col ship-type))
  (setq index 0)
  (while (< index ship-length)
    (put-cursor-place-mode row col)
    (delete-char 1)
    (insert ship-type)
    (setq index (+ index 1))
    (if (equal (which-direction ship-type) "horizontal")
	(setq col (+ col 1))
      (setq row (+ row 1))))
  (put-cursor-place-mode (get-ship-row ship-type)
			 (get-ship-col ship-type)))
;; end of function 'draw-ship
;;******************************************************
					     

(defun delete-ship (ship-type ship-length)
  (make-local-variable 'row)
  (make-local-variable 'col)
  (make-local-variable 'index)
  (setq row (get-ship-row ship-type)) 
  (setq col (get-ship-col ship-type))
  (setq index 0)
  (while (< index ship-length)
    (put-cursor-place-mode row col)
    (delete-char 1)
    (insert ".")
    (setq index (+ index 1))
    (if horizontal-direction
	(setq col (+ col 1))
      (setq row (+ row 1)))))
;; end of function 'delete-ship
;;******************************************************


(defun place-ships-flip ()
  (interactive)
  (if (not (is-legal-flip current-ship-type ship-length))
      (error "You can not flip there!")
    (delete-ship current-ship-type ship-length)
    (make-local-variable 'index)
    (make-local-variable 'first-row)
    (make-local-variable 'first-col)
    (cond ((equal current-ship-type battleship-destroyer)
	   (setq first-row (get-array-entry 0 0))
	   (setq first-col (get-array-entry 0 1)))
	  ((equal current-ship-type battleship-battleship)
	   (setq first-row (get-array-entry 2 0))
	   (setq first-col (get-array-entry 2 1)))
	  ((equal current-ship-type battleship-submarine)
	   (setq first-row (get-array-entry 5 0))
	   (setq first-col (get-array-entry 5 1)))
	  ((equal current-ship-type battleship-cruiser)
	   (setq first-row (get-array-entry 8 0))
	   (setq first-col (get-array-entry 8 1)))
	  ((equal current-ship-type battleship-carrier)
	   (setq first-row (get-array-entry 12 0))
	   (setq first-col (get-array-entry 12 1))))
    (setq index 1)
    (if horizontal-direction
	(while (< index ship-length)
	  (cond ((equal current-ship-type battleship-destroyer)
		 (array-set index 0 (+ index first-row))
		 (array-set index 1 first-col))
		((equal current-ship-type battleship-battleship)
		 (array-set (+ index 2) 0 (+ index first-row))
		 (array-set (+ index 2) 1 first-col))
		((equal current-ship-type battleship-submarine)
		 (array-set (+ index 5) 0 (+ index first-row))
		 (array-set (+ index 5) 1 first-col))
		((equal current-ship-type battleship-cruiser)
		 (array-set (+ index 8) 0 (+ index first-row))
		 (array-set (+ index 8) 1 first-col))
		((equal current-ship-type battleship-carrier)
		 (array-set (+ index 12) 0 (+ index first-row))
		 (array-set (+ index 12) 1 first-col)))
	  (setq index (+ index 1)))
      (while (< index ship-length)
	(cond ((equal current-ship-type battleship-destroyer)
	       (array-set index 0 first-row)
	       (array-set index 1 (+ index first-col)))
	      ((equal current-ship-type battleship-battleship)
	       (array-set (+ index 2) 0 first-row)
	       (array-set (+ index 2) 1 (+ index first-col)))
	      ((equal current-ship-type battleship-submarine)
	       (array-set (+ index 5) 0 first-row)
	       (array-set (+ index 5) 1 (+ index first-col)))
	      ((equal current-ship-type battleship-cruiser)
	       (array-set (+ index 8) 0 first-row)
	       (array-set (+ index 8) 1 (+ index first-col)))
	      ((equal current-ship-type battleship-carrier)
	       (array-set (+ index 12) 0 first-row)
	       (array-set (+ index 12) 1 (+ index first-col))))
	(setq index (+ index 1))))
    (setq horizontal-direction (not horizontal-direction))
    (redraw-all current-ship-type ship-length)))
;; end of function 'place-ships-flip
;;******************************************************


(defun shift-ships ()
  (make-local-variable 'index)
  (setq index 0)
  (while (< index 17)
    (array-set index 1 (+ 16 (get-array-entry index 1)))
    (setq index (+ 1 index))))
;; end of function 'shift-ships
;;******************************************************

      
(defun get-array-entry (outer-index inner-index)
  (if (= player 1)
      (aref (aref player-one-ships outer-index) inner-index)
    (aref (aref player-two-ships outer-index) inner-index)))
;; end of function 'get-array-entry
;;******************************************************


(defun array-set (array-index row-or-col new-value)
  (if (= player 1)
      (aset (aref player-one-ships array-index) row-or-col new-value)
    (aset (aref player-two-ships array-index) row-or-col new-value)))
;; end of function 'array-set
;;******************************************************


(defun array-set-move (ref-index function set-index)
  (if (= player 1)
      (if (equal function "minus")
	  (aset (aref player-one-ships ref-index) set-index 
		(- (aref (aref player-one-ships ref-index) set-index) 1))
	(aset (aref player-one-ships ref-index) set-index 
	      (+ (aref (aref player-one-ships ref-index) set-index) 1)))
    (if (equal function "minus")
	(aset (aref player-two-ships ref-index) set-index 
	      (- (aref (aref player-two-ships ref-index) set-index) 1))
      (aset (aref player-two-ships ref-index) set-index 
	    (+ (aref (aref player-two-ships ref-index) set-index) 1)))))
;; end of function 'array-set-move
;;******************************************************


(defun place-ships-move-left ()
  (interactive)
  (if (not (is-legal-move "left" current-ship-type ship-length))
      (error "Those are uncharted waters.  You can't go there!")
    (delete-ship current-ship-type ship-length)
    (cond ((equal current-ship-type battleship-destroyer)
	   (array-set-move 0 "minus" 1)
	   (array-set-move 1 "minus" 1))
	  ((equal current-ship-type battleship-battleship)
	   (array-set-move 2 "minus" 1)
	   (array-set-move 3 "minus" 1)
	   (array-set-move 4 "minus" 1))
	  ((equal current-ship-type battleship-submarine)
	   (array-set-move 5 "minus" 1)
	   (array-set-move 6 "minus" 1)
	   (array-set-move 7 "minus" 1))
	  ((equal current-ship-type battleship-cruiser)
	   (array-set-move 8 "minus" 1)
	   (array-set-move 9 "minus" 1)
	   (array-set-move 10 "minus" 1)
	   (array-set-move 11 "minus" 1))
	  ((equal current-ship-type battleship-carrier)
	   (array-set-move 12 "minus" 1)
	   (array-set-move 13 "minus" 1)
	   (array-set-move 14 "minus" 1)
	   (array-set-move 15 "minus" 1)
	   (array-set-move 16 "minus" 1)))
    (redraw-all current-ship-type ship-length)))
;; end of function 'place-ships-move-left
;;******************************************************


(defun place-ships-move-right ()
  (interactive)
  (if (in-harbor current-ship-type)
      (error "You can't move right in the harbor!"))
  (if (not (is-legal-move "right" current-ship-type ship-length))
      (error "Those are uncharted waters.  You can't go there!")
    (delete-ship current-ship-type ship-length)
    (cond ((equal current-ship-type battleship-destroyer)
	   (array-set-move 0 "plus" 1)
	   (array-set-move 1 "plus" 1))
	  ((equal current-ship-type battleship-battleship)
	   (array-set-move 2 "plus" 1)
	   (array-set-move 3 "plus" 1)
	   (array-set-move 4 "plus" 1))
	  ((equal current-ship-type battleship-submarine)
	   (array-set-move 5 "plus" 1)
	   (array-set-move 6 "plus" 1)
	   (array-set-move 7 "plus" 1))
	  ((equal current-ship-type battleship-cruiser)
	   (array-set-move 8 "plus" 1)
	   (array-set-move 9 "plus" 1)
	   (array-set-move 10 "plus" 1)
	   (array-set-move 11 "plus" 1))
	  ((equal current-ship-type battleship-carrier)
	   (array-set-move 12 "plus" 1)
	   (array-set-move 13 "plus" 1)
	   (array-set-move 14 "plus" 1)
	   (array-set-move 15 "plus" 1)
	   (array-set-move 16 "plus" 1)))
    (redraw-all current-ship-type ship-length)))
;; end of function 'place-ships-move-right
;;******************************************************


(defun place-ships-move-up ()
  (interactive)
  (if (not (is-legal-move "up" current-ship-type ship-length))
      (error "Those are uncharted waters.  You can't go there!")
    (delete-ship current-ship-type ship-length)
    (cond ((equal current-ship-type battleship-destroyer)
	   (array-set-move 0 "minus" 0)
	   (array-set-move 1 "minus" 0))
	  ((equal current-ship-type battleship-battleship)
	   (array-set-move 2 "minus" 0)
	   (array-set-move 3 "minus" 0)
	   (array-set-move 4 "minus" 0))
	  ((equal current-ship-type battleship-submarine)
	   (array-set-move 5 "minus" 0)
	   (array-set-move 6 "minus" 0)
	   (array-set-move 7 "minus" 0))
	  ((equal current-ship-type battleship-cruiser)
	   (array-set-move 8 "minus" 0)
	   (array-set-move 9 "minus" 0)
	   (array-set-move 10 "minus" 0)
	   (array-set-move 11 "minus" 0))
	  ((equal current-ship-type battleship-carrier)
	   (array-set-move 12 "minus" 0)
	   (array-set-move 13 "minus" 0)
	   (array-set-move 14 "minus" 0)
	   (array-set-move 15 "minus" 0)
	   (array-set-move 16 "minus" 0)))
    (redraw-all current-ship-type ship-length)))
;; end of function 'place-ships-move-up
;;******************************************************
  

(defun place-ships-move-down ()
  (interactive)
  (if (not (is-legal-move "down" current-ship-type ship-length))
      (error "Those are uncharted waters.  You can't go there!")
    (delete-ship current-ship-type ship-length)
    (cond ((equal current-ship-type battleship-destroyer)
	   (array-set-move 0 "plus" 0)
	   (array-set-move 1 "plus" 0))
	  ((equal current-ship-type battleship-battleship)
	   (array-set-move 2 "plus" 0)
	   (array-set-move 3 "plus" 0)
	   (array-set-move 4 "plus" 0))
	  ((equal current-ship-type battleship-submarine)
	   (array-set-move 5 "plus" 0)
	   (array-set-move 6 "plus" 0)
	   (array-set-move 7 "plus" 0))
	  ((equal current-ship-type battleship-cruiser)
	   (array-set-move 8 "plus" 0)
	   (array-set-move 9 "plus" 0)
	   (array-set-move 10 "plus" 0)
	   (array-set-move 11 "plus" 0))
	  ((equal current-ship-type battleship-carrier)
	   (array-set-move 12 "plus" 0)
	   (array-set-move 13 "plus" 0)
	   (array-set-move 14 "plus" 0)
	   (array-set-move 15 "plus" 0)
	   (array-set-move 16 "plus" 0)))
    (redraw-all current-ship-type ship-length)))
;; end of function 'place-ships-move-down
;;******************************************************


(defun on-another-ship (ship-type ship-length)
  (make-local-variable 'row)
  (make-local-variable 'col)
  (make-local-variable 'index)
  (make-local-variable 'max-index)
  (make-local-variable 'other-ship-index)
  (make-local-variable 'on-ship)
  (cond ((equal ship-type battleship-destroyer)
	 (setq index 0))
	((equal ship-type battleship-battleship)
	 (setq index 2))
	((equal ship-type battleship-submarine)
	 (setq index 5))
	((equal ship-type battleship-cruiser)
	 (setq index 8))
	((equal ship-type battleship-carrier)
	 (setq index 12)))
  (setq max-index (+ index ship-length))
  (setq on-ship nil)
  (while (and (< index max-index) (not on-ship))
    (setq row (get-array-entry index 0))
    (setq col (get-array-entry index 1))
    (setq other-ship-index 0)
    (while (and (< other-ship-index index) (not on-ship))
      (if (and (= (get-array-entry other-ship-index 0) row)
	       (= (get-array-entry other-ship-index 1) col))
	  (setq on-ship t))
      (setq other-ship-index (+ other-ship-index 1)))
    (setq index (+ index 1)))
  on-ship)
;; end of function 'on-another-ship
;;******************************************************


(defun redraw-all (ship-type ship-length)
  (draw-ship battleship-destroyer 2)
  (draw-ship battleship-battleship 3)
  (draw-ship battleship-submarine 3)
  (draw-ship battleship-cruiser 4)
  (draw-ship battleship-carrier 5)
  (draw-ship ship-type ship-length))
;; end of function 'redraw-all
;;******************************************************


(defun in-harbor (ship-type)
  (cond ((equal current-ship-type battleship-destroyer)
	 (not (< (get-array-entry 1 1) 16)))
	((equal current-ship-type battleship-battleship)
	 (not (< (get-array-entry 4 1) 16)))
	((equal current-ship-type battleship-submarine)
	 (not (< (get-array-entry 7 1) 16)))
	((equal current-ship-type battleship-cruiser)
	 (not (< (get-array-entry 11 1) 16)))
	((equal current-ship-type battleship-carrier)
	 (not (< (get-array-entry 16 1) 16)))))
;; end of function 'in-harbor
;;******************************************************


(defun is-legal-flip (ship-type ship-length)
  (if (in-harbor ship-type)
      nil
    (if horizontal-direction
	(or (> (- 11 (get-ship-row ship-type)) ship-length)
	    (= (- 11 (get-ship-row ship-type)) ship-length))
      (or (> (- 16 (get-ship-col ship-type)) ship-length)
	  (= (- 16 (get-ship-col ship-type)) ship-length)))))
;; end of function 'is-legal-flip
;;******************************************************


(defun is-legal-move (move-direction ship-type ship-length)
  (cond ((equal move-direction "left")
	 (not (= (get-ship-col ship-type) 0)))
	((equal move-direction "up")
	 (if horizontal-direction
	     (if (> (+ (get-ship-col ship-type) ship-length) 16)
		 nil
	       (not (= (get-ship-row ship-type) 0)))
	   (not (= (get-ship-row ship-type) 0))))
	((equal move-direction "right")
	 (if horizontal-direction
	     (not (= (+ (get-ship-col ship-type) ship-length) 16))
	   (not (= (get-ship-col ship-type) 15))))
	((equal move-direction "down")
	 (if horizontal-direction
	     (progn
	       (if (> (+ (get-ship-col ship-type) ship-length) 16)
		   nil
		 (not (= (get-ship-row ship-type) 10))))
	   (not (= (+ (get-ship-row ship-type) ship-length) 11))))))
;; end of function 'is-legal-move
;;******************************************************


(defun place-ship-here ()
  (interactive)
  (if (in-harbor current-ship-type)
      (error "You can't place a ship in the harbor!"))
  (if (on-another-ship current-ship-type ship-length)
      (error "Sorry, you can't place ships on top of each other!"))
  (setq horizontal-direction t)
  (cond ((equal current-ship-type battleship-destroyer)
	 (setq ship-length 3)
	 (setq current-ship-type battleship-battleship)
	 (goto-char battleship-place-mode-home))
	((equal current-ship-type battleship-battleship)
	 (setq ship-length 3)
	 (setq current-ship-type battleship-submarine)
	 (goto-char submarine-place-mode-home))
	((equal current-ship-type battleship-submarine)
	 (setq ship-length 4)
	 (setq current-ship-type battleship-cruiser)
	 (goto-char cruiser-place-mode-home))
	((equal current-ship-type battleship-cruiser)
	 (setq ship-length 5)
	 (setq current-ship-type battleship-carrier)
	 (goto-char carrier-place-mode-home))
	((equal current-ship-type battleship-carrier)
	 (if (= player 1)
	     (progn
	       (shift-ships)
	       (kill-buffer (current-buffer))
	       (switch-to-buffer (set-buffer battleship-buffer-name))
	       (setq talk-tyrant-enabled-console t)
	       (let ((index 0))
		 (while (< index 17)
		   (put-cursor (aref (aref player-one-ships index) 0)
			       (aref (aref player-one-ships index) 1))
		   (delete-char 1)
		   (insert (eval (aref (aref player-one-ships index) 2)))
		   (setq index (+ 1 index))))
	       (goto-char current-player-home)
	       (setup-players-home)
	       (tyrant-send-message "ready-to-play"))
	   (progn
	     (shift-ships)
	     (kill-buffer (current-buffer))
	     (switch-to-buffer (set-buffer battleship-buffer-name))
	     (let ((index 0))
	       (while (< index 17)
		 (put-cursor (aref (aref player-two-ships index) 0)
			     (aref (aref player-two-ships index) 1))
		 (delete-char 1)
		 (insert (eval (aref (aref player-two-ships index) 2)))
		 (setq index (+ 1 index))))
	     (goto-char other-player-home)
	     (tyrant-send-message "ready-to-play")
	     (setup-players-home))))))
;; end of function 'place-ship-here
;;******************************************************


(defun quit-from-placing-mode ()
  (interactive)
  (tyrant-send-message "place-mode-quit")
  (delete-other-windows (selected-window))
  (kill-buffer battleship-minibuffer-name)
  (kill-buffer place-ship-mode-buffer-name)
  (switch-to-buffer (set-buffer battleship-buffer-name))
  (setq talk-tyrant-enabled-console t)
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
      (talk-usurp-tyrant)
    (kill-buffer (current-buffer))))
;; end of function 'quit-from-placing-mode
;;******************************************************


(defun place-mode-wrong-keypress ()
  (interactive)
  (message "That is an invalid keypress."))
;; end of function 'place-mode-wrong-keypress
;;******************************************************


;;==========================================================================
;;                       E N D   O F   P R O G R A M 
;;==========================================================================
