; 2-player chess for emacs
;
; First off, provide the chess feature.
(provide 'chess)

; Now define the Chess keymap.  Keys are as follows:
;
; 4,^b    - Move cursor one square to the left
; 6,^f    - Move cursor one square to the right
; 8,^p    - Move cursor one square up
; 2,^n    - Move cursor one square down
; 5,space - Pick up/drop a piece.
; <, >    - Castle left/right
; q,Q     - Quit.
(defvar chess-map nil "Keymap for chess")

(if chess-map
    ()
  (setq chess-map (make-sparse-keymap))
  (define-key chess-map "4" 'chess-move-left)
  (define-key chess-map "\^b" 'chess-move-left)
  (define-key chess-map "6" 'chess-move-right)
  (define-key chess-map "\^f" 'chess-move-right)
  (define-key chess-map "8" 'chess-move-up)
  (define-key chess-map "\^p" 'chess-move-up)
  (define-key chess-map "2" 'chess-move-down)
  (define-key chess-map "\^n" 'chess-move-down)
  (define-key chess-map "5" 'chess-pickup-drop)
  (define-key chess-map " " 'chess-pickup-drop)
  (define-key chess-map ">" 'chess-castle-right)
  (define-key chess-map "<" 'chess-castle-left)
  (define-key chess-map "q" 'chess-quit)
  (define-key chess-map "Q" 'chess-quit)
)

; Define some ADTs.
;
; First, the coordinate ADT.  This type represents the coordinates of a
; square on the board in the form of an x value and a y value, with ranges
; of 1-8, with 1,1 being the upper-left corner.  Internally, the type
; is represented by a dotted pair, in the form (x . y).  Its interface
; includes the following functions:
;      chess-coordinate-x    - Return the x value from a coordinate
;      chess-coordinate-y    - Return the y value from a coordinate
;      chess-make-coordinate - Make a coordinate from x and y values
(defun chess-coordinate-x (coord)
  (car coord)
)

(defun chess-coordinate-y (coord)
  (cdr coord)
)

(defun chess-make-coordinate (x y)
  (cons x y))

; The piece ADT.  This type represents a chess piece.  It's a dotted pair
; made up of two symbols: the piece type (king, queen, bishop, knight, rook,
; pawn, or none) and the piece color (black or white).  Its interface includes
; the following functions:
;        chess-make-piece    - make a piece from a type and color
;        chess-piece-type    - return the type of a piece
;        chess-piece-color   - return the color of a piece
;        chess-piece-nopiece - an empty square (none . white)
;        chess-piece-piecep  - returns t if this is a piece (and not 'none)
(defun chess-make-piece (type color)
  (cons type color)
)

(defun chess-piece-type (piece)
  (car piece)
)

(defun chess-piece-color (piece)
  (cdr piece)
)

(defconst chess-piece-nopiece (chess-make-piece 'none 'white))

(defun chess-piece-piecep (piece)
  (if (equal piece chess-piece-nopiece)
      nil
      t)
)

; The board ADT.  This type represents the game board.  Its internal
; representation is an 8x8 array of pieces.  Its interface includes:
;
;       chess-board-empty     - An empty board
;       chess-board-reset     - Reset the chess board to a new game
;       chess-board-get-piece - Get the piece at the specified coordinates
;       chess-board-set-piece - Set the piece at the specified coordinates

; We break the ADT here only because elisp doesn't like to evaluate
; expressions within array defs.
(defvar chess-board-empty
  [[(rook . black) (knight . black) (bishop . black) (queen . black) (king . black) (bishop . black) (knight . black) (rook . black)]
   [(pawn . black) (pawn . black) (pawn . black) (pawn . black) (pawn . black) (pawn . black) (pawn . black) (pawn . black)]
   [(none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white)]
   [(none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white)]
   [(none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white)]
   [(none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white) (none . white)]
   [(pawn . white) (pawn . white) (pawn . white) (pawn . white) (pawn . white) (pawn . white) (pawn . white) (pawn . white)]
   [(rook . white) (knight . white) (bishop . white) (queen . white) (king . white) (bishop . white) (knight . white) (rook . white)]])

; This is a kludge.  This is only a kludge.  Had this been an actual piece
; of code, it would have made sense.
(defun chess-board-reset nil
  (chess-board-set-piece '(1 . 1) '(rook . black))
  (chess-board-set-piece '(2 . 1) '(knight . black))
  (chess-board-set-piece '(3 . 1) '(bishop . black))
  (chess-board-set-piece '(4 . 1) '(queen . black))
  (chess-board-set-piece '(5 . 1) '(king . black))
  (chess-board-set-piece '(6 . 1) '(bishop . black))
  (chess-board-set-piece '(7 . 1) '(knight . black))
  (chess-board-set-piece '(8 . 1) '(rook . black))

  (chess-board-set-piece '(1 . 2) '(pawn . black))
  (chess-board-set-piece '(2 . 2) '(pawn . black))
  (chess-board-set-piece '(3 . 2) '(pawn . black))
  (chess-board-set-piece '(4 . 2) '(pawn . black))
  (chess-board-set-piece '(5 . 2) '(pawn . black))
  (chess-board-set-piece '(6 . 2) '(pawn . black))
  (chess-board-set-piece '(7 . 2) '(pawn . black))
  (chess-board-set-piece '(8 . 2) '(pawn . black))

  (chess-board-set-piece '(1 . 3) '(none . white))
  (chess-board-set-piece '(2 . 3) '(none . white))
  (chess-board-set-piece '(3 . 3) '(none . white))
  (chess-board-set-piece '(4 . 3) '(none . white))
  (chess-board-set-piece '(5 . 3) '(none . white))
  (chess-board-set-piece '(6 . 3) '(none . white))
  (chess-board-set-piece '(7 . 3) '(none . white))
  (chess-board-set-piece '(8 . 3) '(none . white))

  (chess-board-set-piece '(1 . 4) '(none . white))
  (chess-board-set-piece '(2 . 4) '(none . white))
  (chess-board-set-piece '(3 . 4) '(none . white))
  (chess-board-set-piece '(4 . 4) '(none . white))
  (chess-board-set-piece '(5 . 4) '(none . white))
  (chess-board-set-piece '(6 . 4) '(none . white))
  (chess-board-set-piece '(7 . 4) '(none . white))
  (chess-board-set-piece '(8 . 4) '(none . white))

  (chess-board-set-piece '(1 . 5) '(none . white))
  (chess-board-set-piece '(2 . 5) '(none . white))
  (chess-board-set-piece '(3 . 5) '(none . white))
  (chess-board-set-piece '(4 . 5) '(none . white))
  (chess-board-set-piece '(5 . 5) '(none . white))
  (chess-board-set-piece '(6 . 5) '(none . white))
  (chess-board-set-piece '(7 . 5) '(none . white))
  (chess-board-set-piece '(8 . 5) '(none . white))

  (chess-board-set-piece '(1 . 6) '(none . white))
  (chess-board-set-piece '(2 . 6) '(none . white))
  (chess-board-set-piece '(3 . 6) '(none . white))
  (chess-board-set-piece '(4 . 6) '(none . white))
  (chess-board-set-piece '(5 . 6) '(none . white))
  (chess-board-set-piece '(6 . 6) '(none . white))
  (chess-board-set-piece '(7 . 6) '(none . white))
  (chess-board-set-piece '(8 . 6) '(none . white))

  (chess-board-set-piece '(1 . 7) '(pawn . white))
  (chess-board-set-piece '(2 . 7) '(pawn . white))
  (chess-board-set-piece '(3 . 7) '(pawn . white))
  (chess-board-set-piece '(4 . 7) '(pawn . white))
  (chess-board-set-piece '(5 . 7) '(pawn . white))
  (chess-board-set-piece '(6 . 7) '(pawn . white))
  (chess-board-set-piece '(7 . 7) '(pawn . white))
  (chess-board-set-piece '(8 . 7) '(pawn . white))

  (chess-board-set-piece '(1 . 8) '(rook . white))
  (chess-board-set-piece '(2 . 8) '(knight . white))
  (chess-board-set-piece '(3 . 8) '(bishop . white))
  (chess-board-set-piece '(4 . 8) '(queen . white))
  (chess-board-set-piece '(5 . 8) '(king . white))
  (chess-board-set-piece '(6 . 8) '(bishop . white))
  (chess-board-set-piece '(7 . 8) '(knight . white))
  (chess-board-set-piece '(8 . 8) '(rook . white))
)

(defun chess-board-get-piece (coord)
  (if (not (chess-valid-squarep coord))
      chess-piece-nopiece
      (aref (aref board (- (chess-coordinate-y coord) 1)) (- (chess-coordinate-x coord) 1)))
)

(defun chess-board-set-piece (coord piece)
  (if (chess-valid-squarep coord)
      (let ((tmp (aref board (- (chess-coordinate-y coord) 1))))
	(aset tmp (- (chess-coordinate-x coord) 1) piece)
	(aset board (- (chess-coordinate-y coord) 1) tmp)))
)

; And finally the functions
;
; The main function sets things up, draws the board, and puts the
; cursor at its initial location.
(defun chess ()
  "2-player Chess mode, to be used in conjunction with Emacs Talk's
Tyrant Mode.  This mode lets you play chess against an opponent,
moving the cursor with the numeric keypad, the cursor keys, or the
standard Emacs keys, and picking up and dropping pieces with the
5 key or the space bar.  Hit the q key to quit."

  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create "CHESS")))
  (setq mode-name "Chess")
  (setq major-mode 'chess)
  (delete-region (point-min) (point-max))
  (insert "+---+---+---+---+---+---+---+---+
| r | h | b | q | k | b | h | r |
+---+---+---+---+---+---+---+---+
| p | p | p | p | p | p | p | p |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| P | P | P | P | P | P | P | P |
+---+---+---+---+---+---+---+---+
| R | H | B | Q | K | B | H | R |
+---+---+---+---+---+---+---+---+
")
  (insert "                                 ")
  (insert "
")
  (insert "                                 ")
  (insert "
")
  (delete-other-windows (selected-window))
  (use-local-map chess-map)

  ; Declare some variables for this buffer.
  ; Whose turn is it?
  (make-local-variable 'turn)
  (setq turn 'white)
  ; Tyrant mode help
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
	"4,^b-left 6,^f-right 8,^p-up 2,^n-down 5,spc-pickup/drop, <,>-castle, q-quit")
  ; The board.
  (make-local-variable 'board)
  (setq board chess-board-empty)
  (chess-board-reset)
  ; The current position of the chess cursor (relative to the board)
  (make-local-variable 'cursor-position)
  (setq cursor-position (chess-make-coordinate 8 8))
  (chess-move-cursor-to cursor-position)
  ; The original coordinates of the chess piece that's now being "carried"
  ; 0,0 (or any other out-of-range square) if no piece.
  (make-local-variable 'current-piece)
  (setq current-piece (chess-make-coordinate 0 0))
  ; Is the game over?
  (make-local-variable 'game-over)
  (setq game-over nil)
  ; The current legal moves, used in other routines
  (make-local-variable 'current-legal-moves)
  (setq current-legal-moves nil)

  ; Tyrant mode stuff
  (setq tyrant-player1-hook
	'(lambda ()
	   (message "You are white.")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq talk-tyrant-enabled-console nil)
	   (message "You are black.")))

  ; Give the user a message in the minibuffer
  (message "4,^b-left 6,^f-right 8,^p-up 2,^n-down 5,spc-pickup/drop, <,>-castle, q-quit")
)

;Key-bound functions
;
; Move the cursor one square left
(defun chess-move-left ()
  "Move the chess cursor one square left"

  (interactive)
  (if game-over (error "The game is over!  Give it up!"))
  (if (not (chess-valid-squarep
	    (chess-make-coordinate (- (chess-coordinate-x cursor-position) 1)
				   (chess-coordinate-y cursor-position))))
      (error ""))
  (setq cursor-position (chess-make-coordinate
			 (- (chess-coordinate-x cursor-position) 1)
			 (chess-coordinate-y cursor-position)))
  (chess-move-cursor-to cursor-position)
)

; Move the cursor one square right
(defun chess-move-right ()
  "Move the chess cursor one square right"

  (interactive)
  (if game-over (error "It's a done deal."))
  (if (not (chess-valid-squarep
	    (chess-make-coordinate (+ (chess-coordinate-x cursor-position) 1)
				   (chess-coordinate-y cursor-position))))
      (error ""))
  (setq cursor-position (chess-make-coordinate
			 (+ (chess-coordinate-x cursor-position) 1)
			 (chess-coordinate-y cursor-position)))
  (chess-move-cursor-to cursor-position)
)

; Move the cursor one square up
(defun chess-move-up ()
  "Move the chess cursor one square up"

  (interactive)
  (if game-over (error "This game is history."))
  (if (not (chess-valid-squarep
	    (chess-make-coordinate (chess-coordinate-x cursor-position)
				   (- (chess-coordinate-y cursor-position) 1))))
      (error ""))
  (setq cursor-position (chess-make-coordinate
			 (chess-coordinate-x cursor-position)
			 (- (chess-coordinate-y cursor-position) 1)))
  (chess-move-cursor-to cursor-position)
)

; Move the cursor one square down
(defun chess-move-down ()
  "Move the chess cursor one square down"

  (interactive)
  (if game-over (error "This game is no more."))
  (if (not (chess-valid-squarep
	    (chess-make-coordinate (chess-coordinate-x cursor-position)
				   (+ (chess-coordinate-y cursor-position) 1))))
      (error ""))
  (setq cursor-position (chess-make-coordinate
			 (chess-coordinate-x cursor-position)
			 (+ (chess-coordinate-y cursor-position) 1)))
  (chess-move-cursor-to cursor-position)
)

; Pick up/drop a piece
(defun chess-pickup-drop ()
  "Pick up or drop a chess piece"

  (interactive)
  (if game-over (error "This game is defunct."))
  (if (chess-valid-squarep current-piece)
      (progn 
	(if (equal current-piece cursor-position) ;They changed their mind
	    (progn
	      (chess-unmark-squares current-legal-moves)
	      (setq current-piece (chess-make-coordinate 0 0))
	      (message "Never mind."))
	  (if (chess-list-containsp current-legal-moves cursor-position)
	      (progn
		(chess-move-piece current-piece cursor-position) ;Drop a piece
		(chess-unmark-squares current-legal-moves)
		(setq current-piece (chess-make-coordinate 0 0))
		(message "Dropped piece.")
		(chess-swap-turns))
	    (error "Invalid move."))))
    (progn ;Pick up a piece 
      (if (not (chess-piece-piecep (chess-board-get-piece cursor-position)))
	  (error "Invalid piece."))
      (if (not (equal (chess-piece-color (chess-board-get-piece cursor-position))
		      turn))
	  (error "Not your piece!"))
      (setq current-piece cursor-position)
      (setq current-legal-moves (chess-get-legal-moves current-piece))
      (chess-mark-squares current-legal-moves)
      (message "Picked up: %s %s" 
	       (chess-piece-color (chess-board-get-piece cursor-position))
	       (chess-piece-type (chess-board-get-piece cursor-position)))))
)

; Castle right
; Currently doesn't check to see that king and rook haven't been moved.
(defun chess-castle-right()
  "Castle right in chess"

  (let
      ((tmp-king-square nil)
       (tmp-king-new-square nil)
       (tmp-rook-square nil)
       (tmp-rook-new-square nil))
    (if (equal turn 'black)
	(progn
	  (setq tmp-king-square (chess-make-coordinate 5 1))
	  (setq tmp-king-new-square (chess-make-coordinate 7 1))
	  (setq tmp-rook-square (chess-make-coordinate 8 1))
	  (setq tmp-rook-new-square (chess-make-coordinate 6 1)))
      (setq tmp-king-square (chess-make-coordinate 5 8))
      (setq tmp-king-new-square (chess-make-coordinate 7 8))
      (setq tmp-rook-square (chess-make-coordinate 8 8))
      (setq tmp-rook-new-square (chess-make-coordinate 6 8)))
    (cond
     ((chess-piece-piecep (chess-board-get-piece tmp-king-new-square))
      (error "Can't castle now"))
     ((chess-piece-piecep (chess-board-get-piece tmp-rook-new-square))
      (error "Can't castle now"))
     ((not (and (equal (chess-piece-type (chess-board-get-piece
					  tmp-king-square))
		       'king)
		(equal (chess-piece-color (chess-board-get-piece
					   tmp-king-square))
		       turn)))
      (error "Can't castle now"))
     ((not (and (equal (chess-piece-type (chess-board-get-piece
					  tmp-rook-square))
		       'rook)
		(equal (chess-piece-color (chess-board-get-piece
					   tmp-rook-square))
		       turn)))
      (error "Can't castle now"))
     (t
      (chess-move-piece tmp-king-square tmp-king-new-square)
      (chess-move-piece tmp-rook-square tmp-rook-new-square)
      (chess-swap-turns))))
)

; Castle left
; Currently doesn't check to see that king and rook haven't been moved.
(defun chess-castle-left()
  "Castle right in chess"

  (let
      ((tmp-king-square nil)
       (tmp-king-new-square nil)
       (tmp-rook-square nil)
       (tmp-rook-new-square nil)
       (tmp-extra-square nil))
    (if (equal turn 'black)
	(progn
	  (setq tmp-king-square (chess-make-coordinate 5 1))
	  (setq tmp-king-new-square (chess-make-coordinate 3 1))
	  (setq tmp-rook-square (chess-make-coordinate 1 1))
	  (setq tmp-rook-new-square (chess-make-coordinate 4 1))
	  (setq tmp-extra-square (chess-make-coordinate 2 1)))
      (setq tmp-king-square (chess-make-coordinate 5 8))
      (setq tmp-king-new-square (chess-make-coordinate 3 8))
      (setq tmp-rook-square (chess-make-coordinate 1 8))
      (setq tmp-rook-new-square (chess-make-coordinate 4 8))
      (setq tmp-extra-square (chess-make-coordinate 2 8)))
    (cond
     ((chess-piece-piecep (chess-board-get-piece tmp-king-new-square))
      (error "Can't castle now"))
     ((chess-piece-piecep (chess-board-get-piece tmp-rook-new-square))
      (error "Can't castle now"))
     ((chess-piece-piecep (chess-board-get-piece tmp-extra-square))
      (error "Can't castle now"))
     ((not (and (equal (chess-piece-type (chess-board-get-piece
					  tmp-king-square))
		       'king)
		(equal (chess-piece-color (chess-board-get-piece
					   tmp-king-square))
		       turn)))
      (error "Can't castle now"))
     ((not (and (equal (chess-piece-type (chess-board-get-piece
					  tmp-rook-square))
		       'rook)
		(equal (chess-piece-color (chess-board-get-piece
					   tmp-rook-square))
		       turn)))
      (error "Can't castle now"))
     (t
      (chess-move-piece tmp-king-square tmp-king-new-square)
      (chess-move-piece tmp-rook-square tmp-rook-new-square)
      (chess-swap-turns))))
)

; Quit the game
(defun chess-quit ()
  "Quit Chess"

  (interactive)
  (setq game-over t)
  (message (format "%s has quit." turn))
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
      (talk-usurp-tyrant)
    (kill-buffer (current-buffer)))
)

; Swap turns
(defun chess-swap-turns nil
  "Swap turns in chess."

  (cond
   ((equal turn 'white) (setq turn 'black))
   ((equal turn 'black) (setq turn 'white)))
  (if (boundp 'talk-tyrant-enabled-console)
      (progn
	(message (format "It is now %s's turn." turn))
	(setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
    (message "Player %s's move." turn))
)

; Some utility functions relating to the board
;
; Return t if coord is a valid square on the board, nil otherwise
(defun chess-valid-squarep (coord)
  "Determine whether a set of coordinates represents a valid square"

  (cond
   ((< (chess-coordinate-x coord) 1) nil)
   ((> (chess-coordinate-x coord) 8) nil)
   ((< (chess-coordinate-y coord) 1) nil)
   ((> (chess-coordinate-y coord) 8) nil)
   (t t))
)

; Move a piece from one square to another
; This function is low-level enough that we don't do much error checking.
(defun chess-move-piece (from to)
  "Move a piece on the board"

  ; First, move the piece on our board
  ; Conveniently enough, this also takes care of removing a captured
  ; piece from the board.
  (let ((tmp-piece (chess-board-get-piece from)))
    (chess-board-set-piece from chess-piece-nopiece)
    (chess-board-set-piece to tmp-piece)

    ; Now, update the screen to reflect this
    (chess-clear-square from)
    (chess-clear-square to)
    (chess-draw-piece to tmp-piece))
)

; Move the physical cursor to the board coordinates specified by coord
(defun chess-move-cursor-to (coord)
  "Move the physical cursor to a square on the board"

  (move-to-window-line (- (* (chess-coordinate-y coord) 2) 1))
  (move-to-column (- (* (chess-coordinate-x coord) 4) 2))
)

; Clear a square on the screen
(defun chess-clear-square (coord)
  "Blank a square on the chess board"

  (chess-move-cursor-to coord)
  (backward-char 1)
  (delete-char 3)
  (insert "   ")
  (chess-move-cursor-to cursor-position)
)

; Draw a chess piece at a specified board coordinate
(defun chess-draw-piece (coord piece)
  "Draw a chess piece on a square on the chess board"

  (chess-move-cursor-to coord)
  (delete-char 1)
  (let ((tmp-type (chess-piece-type piece)))
    (if (equal (chess-piece-color piece) 'black)
	(cond
	 ((equal tmp-type 'rook) (insert "r"))
	 ((equal tmp-type 'knight) (insert "h"))
	 ((equal tmp-type 'bishop) (insert "b"))
	 ((equal tmp-type 'queen) (insert "q"))
	 ((equal tmp-type 'king) (insert "k"))
	 ((equal tmp-type 'pawn) (insert "p")))
      (cond
       ((equal tmp-type 'rook) (insert "R"))
       ((equal tmp-type 'knight) (insert "H"))
       ((equal tmp-type 'bishop) (insert "B"))
       ((equal tmp-type 'queen) (insert "Q"))
       ((equal tmp-type 'king) (insert "K"))
       ((equal tmp-type 'pawn) (insert "P")))))
  (chess-move-cursor-to cursor-position)
)

; Mark all squares in the list provided
(defun chess-mark-squares (squares)
  "Mark all squares in the provided list"

  (if (not (equal squares nil))
      (progn
	(chess-move-cursor-to (car squares))
	(backward-char 1)
	(delete-char 1)
	(insert "*")
	(forward-char 1)
	(delete-char 1)
	(insert "*")
        (chess-move-cursor-to cursor-position)
	(chess-mark-squares (cdr squares))))
)

; Unmark all squares in the list provided
(defun chess-unmark-squares (squares)
  "Unmark all squares in the provided list"

  (if (not (equal squares nil))
      (progn
	(chess-move-cursor-to (car squares))
	(backward-char 1)
	(delete-char 1)
	(insert " ")
	(forward-char 1)
	(delete-char 1)
	(insert " ")
	(chess-move-cursor-to cursor-position)
	(chess-unmark-squares (cdr squares))))
)

; A utility function
(defun chess-list-containsp (l s)
  "Return t if list l contains item s"

  (cond
   ((equal l nil) nil)
   ((equal (car l) s) t)
   (t (or nil (chess-list-containsp (cdr l) s))))
)

; Return a list of coordinates representing all the legal moves
; for the chess piece at coord.
;
; This function calls a number of low-level helper functions, defined
; first.  These were not included in the function with let* because
; things got very unreadable very quickly.  Before the low-level
; helpers are some even lower level helpers used by the low-level
; helpers.  These are not included with let* because they are
; called by more than one function.
(defun chess-up-move-helper (coord color)
  "Return all possible up moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		    (chess-coordinate-x coord)
		    (- (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-up-move-helper tmp-coord color)))))
)

(defun chess-down-move-helper (coord color)
  "Return all possible down moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (chess-coordinate-x coord)
		   (+ (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-down-move-helper tmp-coord color)))))
)

(defun chess-left-move-helper (coord color)
  "Return all possible left moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (- (chess-coordinate-x coord) 1)
		   (chess-coordinate-y coord)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-left-move-helper tmp-coord color)))))
)

(defun chess-right-move-helper (coord color)
  "Return all possible right moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (+ (chess-coordinate-x coord) 1)
		   (chess-coordinate-y coord)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-right-move-helper tmp-coord color)))))
)

(defun chess-upleft-move-helper (coord color)
  "Return all possible up/left moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (- (chess-coordinate-x coord) 1)
		   (- (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-upleft-move-helper tmp-coord color)))))
)

(defun chess-upright-move-helper (coord color)
  "Return all possible up/right moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (+ (chess-coordinate-x coord) 1)
		   (- (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-upright-move-helper tmp-coord color)))))
)

(defun chess-downright-move-helper (coord color)
  "Return all possible down/right moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (+ (chess-coordinate-x coord) 1)
		   (+ (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-downright-move-helper tmp-coord color)))))
)

(defun chess-downleft-move-helper (coord color)
  "Return all possible down/left moves from the piece at coord"

  (let*
       ((tmp-coord (chess-make-coordinate
		   (- (chess-coordinate-x coord) 1)
		   (+ (chess-coordinate-y coord) 1)))
	(tmp-piece nil))
    (if (chess-valid-squarep tmp-coord)
	(setq tmp-piece (chess-board-get-piece tmp-coord)))
    (cond
     ((not (chess-valid-squarep tmp-coord)) nil)
     ((chess-piece-piecep tmp-piece)
      (if (equal (chess-piece-color tmp-piece) color)
	  nil
	(cons tmp-coord nil)))
     (t (cons tmp-coord (chess-downleft-move-helper tmp-coord color)))))
)

; This helper 'edits out' any invalid squares from a list of squares it
; is passed
(defun chess-remove-bad-squares (squares)
  "Remove invalid squares from a list of squares"

  (cond
   ((equal squares nil) nil)
   ((chess-valid-squarep (car squares)) (cons (car squares)
					      (chess-remove-bad-squares
					       (cdr squares))))
   (t (chess-remove-bad-squares (cdr squares))))
)

(defun chess-rook-moves (coord)
  "Get rook moves"

  ; This one's easy.  Just glom together the results of a bunch of helper
  ; functions.
  (let
      ((tmp-color (chess-piece-color (chess-board-get-piece coord))))
    (append
     (chess-up-move-helper coord tmp-color)
     (chess-down-move-helper coord tmp-color)
     (chess-left-move-helper coord tmp-color)
     (chess-right-move-helper coord tmp-color)))
)

; The knight moves function is a bit kludgy.  It builds a list of all
; squares the knight could possibly move to, removes any bad squares,
; then uses a helper to remove any squares that are occupied by a piece
; of the same color.
(defun chess-knight-helper (squares color)
  "Help chess-knight-moves"

  (cond
   ((equal squares nil) nil)
   ((and (chess-piece-piecep (chess-board-get-piece (car squares)))
	 (equal (chess-piece-color (chess-board-get-piece (car squares)))
		color))
    (chess-knight-helper (cdr squares) color))
   (t (cons (car squares) (chess-knight-helper (cdr squares) color))))
)

(defun chess-knight-moves (coord)
  "Get knight moves"

  (let
      ((tmp-list nil))
    (setq tmp-list
	  (list
	   (chess-make-coordinate (+ (chess-coordinate-x coord) 1)
				  (- (chess-coordinate-y coord) 2))
	   (chess-make-coordinate (+ (chess-coordinate-x coord) 2)
				  (- (chess-coordinate-y coord) 1))
	   (chess-make-coordinate (+ (chess-coordinate-x coord) 2)
				  (+ (chess-coordinate-y coord) 1))
	   (chess-make-coordinate (+ (chess-coordinate-x coord) 1)
				  (+ (chess-coordinate-y coord) 2))
	   (chess-make-coordinate (- (chess-coordinate-x coord) 1)
				  (+ (chess-coordinate-y coord) 2))
	   (chess-make-coordinate (- (chess-coordinate-x coord) 2)
				  (+ (chess-coordinate-y coord) 1))
	   (chess-make-coordinate (- (chess-coordinate-x coord) 2)
				  (- (chess-coordinate-y coord) 1))
	   (chess-make-coordinate (- (chess-coordinate-x coord) 1)
				  (- (chess-coordinate-y coord) 2))))
    (setq tmp-list (chess-remove-bad-squares tmp-list))
    (chess-knight-helper tmp-list (chess-piece-color
				   (chess-board-get-piece coord))))
)

(defun chess-bishop-moves (coord)
  "Get bishop moves"

  ; Another easy one
  (let
      ((tmp-color (chess-piece-color (chess-board-get-piece coord))))
    (append
     (chess-upleft-move-helper coord tmp-color)
     (chess-upright-move-helper coord tmp-color)
     (chess-downright-move-helper coord tmp-color)
     (chess-downleft-move-helper coord tmp-color)))
)

; The king moves function is approximately the same as the knight moves
; function.
(defun chess-king-helper (squares color)
  "Help chess-king-moves"

  (cond
   ((equal squares nil) nil)
   ((not (chess-valid-squarep (car squares)))
    (chess-king-helper (cdr squares) color))
   ((and (chess-piece-piecep (chess-board-get-piece (car squares)))
	 (equal (chess-piece-color (chess-board-get-piece (car squares)))
		color))
    (chess-king-helper (cdr squares) color))
   (t (cons (car squares) (chess-king-helper (cdr squares) color))))
)

(defun chess-king-moves (coord)
  "Get king moves"

  (let
      ((tmp-list (list (chess-make-coordinate (- (chess-coordinate-x coord) 1)
					      (- (chess-coordinate-y coord) 1))
		       (chess-make-coordinate (chess-coordinate-x coord)
					      (- (chess-coordinate-y coord) 1))
		       (chess-make-coordinate (+ (chess-coordinate-x coord) 1)
					      (- (chess-coordinate-y coord) 1))
		       (chess-make-coordinate (- (chess-coordinate-x coord) 1)
					      (chess-coordinate-y coord))
		       (chess-make-coordinate (+ (chess-coordinate-x coord) 1)
					      (chess-coordinate-y coord))
		       (chess-make-coordinate (- (chess-coordinate-x coord) 1)
					      (+ (chess-coordinate-y coord) 1))
		       (chess-make-coordinate (chess-coordinate-x coord)
					      (+ (chess-coordinate-y coord) 1))
		       (chess-make-coordinate (+ (chess-coordinate-x coord) 1)
					      (+ (chess-coordinate-y coord) 1)))))
    (setq tmp-list (chess-king-helper tmp-list 
					    (chess-piece-color
					     (chess-board-get-piece coord))))
    tmp-list)
)

(defun chess-queen-moves (coord)
  "Get queen moves"

  (let
      ((tmp-color (chess-piece-color (chess-board-get-piece coord))))
    (append
     (chess-up-move-helper coord tmp-color)
     (chess-down-move-helper coord tmp-color)
     (chess-left-move-helper coord tmp-color)
     (chess-right-move-helper coord tmp-color)
     (chess-upleft-move-helper coord tmp-color)
     (chess-upright-move-helper coord tmp-color)
     (chess-downright-move-helper coord tmp-color)
     (chess-downleft-move-helper coord tmp-color)))
)

(defun chess-pawn-moves (coord)
  "Get pawn moves"

  (let
      ((tmp-coord nil)
       (tmp-list nil))
    (if (equal (chess-piece-color tmp-piece) 'black)
	(progn
					; First check forward movement
	  (setq tmp-coord (chess-make-coordinate
			   (chess-coordinate-x coord)
			   (+ (chess-coordinate-y coord) 1)))
	  (if (equal (chess-piece-type
		      (chess-board-get-piece tmp-coord))
		     'none)
	      (progn
		(setq tmp-list (cons tmp-coord tmp-list))
		(if (equal (chess-coordinate-y coord) 2)
		    (progn
					; See if we can move two spaces
		      (setq tmp-coord (chess-make-coordinate
				       (chess-coordinate-x coord)
				       4))
		      (if (equal (chess-piece-type
				  (chess-board-get-piece tmp-coord))
				 'none)
			  (setq tmp-list (cons tmp-coord tmp-list)))))))
					; Now see if we can capture from here
	  (setq tmp-coord (chess-make-coordinate
			   (- (chess-coordinate-x coord) 1)
			   (+ (chess-coordinate-y coord) 1)))
	  (if (and (not (equal (chess-piece-type
				(chess-board-get-piece tmp-coord))
			       'none))
		   (equal (chess-piece-color
			   (chess-board-get-piece tmp-coord))
			  'white))
	      (setq tmp-list (cons tmp-coord tmp-list)))
	  (setq tmp-coord (chess-make-coordinate
			   (+ (chess-coordinate-x coord) 1)
			   (+ (chess-coordinate-y coord) 1)))
	  (if (and (not (equal (chess-piece-type
				(chess-board-get-piece tmp-coord))
			       'none))
		   (equal (chess-piece-color
			   (chess-board-get-piece tmp-coord))
			  'white))
	      (setq tmp-list (cons tmp-coord tmp-list))))
      
					; If we get here, the piece is white. (The else clause)
					; First check the forward movement
      (setq tmp-coord (chess-make-coordinate
		       (chess-coordinate-x coord)
		       (- (chess-coordinate-y coord) 1)))
      (if (equal (chess-piece-type
		  (chess-board-get-piece tmp-coord))
		 'none)
	  (progn
	    (setq tmp-list (cons tmp-coord tmp-list))
	    (if (equal (chess-coordinate-y coord) 7)
		(progn
					; See if we can move two spaces
		  (setq tmp-coord (chess-make-coordinate
				   (chess-coordinate-x coord)
				   5))
		  (if (equal (chess-piece-type
			      (chess-board-get-piece tmp-coord))
			     'none)
		      (setq tmp-list (cons tmp-coord tmp-list)))))))
					; Now see if we can capture anything
      (setq tmp-coord (chess-make-coordinate
		       (- (chess-coordinate-x coord) 1)
		       (- (chess-coordinate-y coord) 1)))
      (if (and (not (equal (chess-piece-type
			    (chess-board-get-piece tmp-coord))
			   'none))
	       (equal (chess-piece-color
		       (chess-board-get-piece tmp-coord))
		      'black))
	  (setq tmp-list (cons tmp-coord tmp-list)))
      (setq tmp-coord (chess-make-coordinate
		       (+ (chess-coordinate-x coord) 1)
		       (- (chess-coordinate-y coord) 1)))
      (if (and (not (equal (chess-piece-type
			    (chess-board-get-piece tmp-coord))
			   'none))
	       (equal (chess-piece-color
		       (chess-board-get-piece tmp-coord))
		      'black))
	  (setq tmp-list (cons tmp-coord tmp-list))))
    tmp-list))

(defun chess-get-legal-moves (coord)
  "Return the legal moves for a piece on the board"

  (let
      ((tmp-piece (chess-board-get-piece coord)))
    (cond
     ((equal (chess-piece-type tmp-piece) 'none) nil)
     ((equal (chess-piece-type tmp-piece) 'rook) (chess-rook-moves coord))
     ((equal (chess-piece-type tmp-piece) 'knight) (chess-knight-moves coord))
     ((equal (chess-piece-type tmp-piece) 'bishop) (chess-bishop-moves coord))
     ((equal (chess-piece-type tmp-piece) 'king) (chess-king-moves coord))
     ((equal (chess-piece-type tmp-piece) 'queen) (chess-queen-moves coord))
     ((equal (chess-piece-type tmp-piece) 'pawn) (chess-pawn-moves coord))))
)
