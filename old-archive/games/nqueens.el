;;; nqueens.el --- solution for the N queens problem

;; Copyright (C) 1994  Daniel Quinlan (quinlan@gnu.ai.mit.edu)
;;
;; LCD Archive Entry:
;; nqueens|Daniel Quinlan|quinlan@gnu.ai.mit.edu|
;; Produce a solution for the N queens problem.|
;; 27-Dec-94|1.2|~/games/nqueens.el|

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; To execute this program, evaluate each function, then call as follows:
;;   for an eight-by-eight chess board, placing the first queen on (1, 2)
;;     (nqueens 8 2)
;;   for a generic x-by-x chess board, placing the first queen on (1, a)
;;     (nqueens x a)

;; If there is a solution for the parameters given, nqueens will return a list
;; of pairs which represent the positioning of queens on the chess board.  If
;; there is no possible solution for the parameters, it will return nil, "nil".
;;   For instance, evaluating
;;     (nqueens 4 2)
;;   will return
;;     ((1 2) (2 4) (3 1) (4 3))
;;   meaning that four queens are placed at positions (1, 2), (2, 4), (3, 1),
;;   and (4, 3) on a four-by-four chess board.

;; Your other option is to use the graphical front-end as follows:
;;   (nqueens-graphical 4 2)
;; It uses the same calling conventions as nqueens, but has easily
;; understandable output.

;;; Code:

(defun nqueens-safe-move (move queenl)
  (cond ((null queenl) t)
	((= (car (cdr (car queenl)))
	    (car (cdr move))) nil)
	((= (- (car move) (car (car queenl)))
	    (abs (- (car (cdr move)) (car (cdr (car queenl)))))) nil)
	(t (nqueens-safe-move move (cdr queenl)))))

(defun nqueens-new-queen (n row queenl)
  (cond ((> row n) queenl)
	(t (nqueens-test-column n row 1 queenl))))

(defun nqueens-test-column (n row column queenl)
  (cond ((> column n) nil)
	((and (nqueens-safe-move (list row column) queenl)
	      (nqueens-new-queen n (1+ row) (cons (list row column) queenl))))
	(t (nqueens-test-column n row (1+ column) queenl))))

(defun nqueens (n first)
  "Solve the N queens problem where N is size of the chess board and
FIRST is the placement of the first queen on the first row of the chess
board.  Both numbers must be positive integers.

If there is a solution for the parameters given, nqueens will return a list
of pairs which represent the positioning of queens on the chess board.  If
there is no possible solution for the parameters, it will return nil.

For instance, evaluating:
  (nqueens 4 2)
will return:
  ((1 2) (2 4) (3 1) (4 3))
meaning that four queens are placed at positions (1, 2), (2, 4), (3, 1),
and (4, 3) on a four-by-four chess board.

In addition, a graphical solution is available with the
nqueens-graphical function."
  (interactive "nSize of chess board (n): \nnPlace first queen on square: ")
  (message "%s" (nreverse (nqueens-new-queen n 2 (list (list 1 first))))))

(defun nqueens-print-row (n position)
  (cond ((= position 1) (insert "  *"))
	(t (insert "  .")))
  (cond ((= n 1) (insert "\n"))
	(t (nqueens-print-row (1- n) (1- position)))))

(defun nqueens-print-board (n queenl)
  (if (null queenl) nil
    (progn
      (nqueens-print-board n (cdr queenl))
      (not (nqueens-print-row n (car (cdr (car queenl))))))))

(defun nqueens-graphical (n first)
  "Solve the N queens problem and display the solution graphically.  N
is the size of the chess board and FIRST is the placement of the first
queen on the first row of the chess board.  Both numbers must be
positive integers.

If there is a solution for the parameters given, nqueens will print a
board which represents the positioning of queens (\"*\") on the chess
board.  If there is no possible solution for the parameters, it will
print \"no solution\".

Also see the nqueens function."
  (interactive "nSize of chess board (n): \nnPlace first queen on square: ")
  (pop-to-buffer (concat "*Solution to " n " Queens Problem*"))
  (erase-buffer)
  (delete-other-windows)
  (insert (format "Placing the first queen on (1, %d)\n\n" first))
  (if (not
       (nqueens-print-board n (nqueens-new-queen n 2 (list (list 1 first)))))
      (insert "no solution\n"))
  (set-buffer-modified-p nil))

;;; nqueens.el ends here

