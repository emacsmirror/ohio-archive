; atc.el
;
; by Neil Jerram <nj104@cus.cam.ac.uk>
;
; version 1.0, Thu Jan 28 22:39:16 1993
;
; LCD Archive Entry:
; atc|Neil Jerram|nj104@cus.cam.ac.uk|
; Air Traffic Control|
; 1993-01-28|1.0|~/games/atc.tar.Z|
;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymous ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;
; User options.
; The following variables may be altered by the player to
; change the pace and difficulty of the game.

(defvar atc-nb-planes 26
  "*Total number of planes in the game (maximum 26).")
(defvar atc-initial-delay 5000
  "*The initial time lapse between successive plane movements.")
(defvar atc-acceleration 2
  "*If this variable is non-zero, successive plane movements will
become faster and faster as the game progresses.  The higher the 
value of the variable, the faster the speed-up.")
(defvar atc-mean-separation 10
  "*Roughly speaking, the average number of moves between new planes.")
(defvar atc-%-airports 50
  "*When a new plane is started, a provenance and a destination are
chosen randomly for it.  This variable governs how many of the
choices will be airports and how many will be areas: a value of 0
means all areas; 100 means all airports.")

; Internal variables.

(defvar atc-delay nil
  "The current time lapse between successive plane movements.")
(defvar atc-plane nil
  "Name of plane currently being/about to be commanded.")
(defvar atc-command nil
  "Name of command being/about to be executed.")
(defvar atc-arg nil
  "Argument of command under execution, where applicable.")
(defvar atc-command-2-key-list
  (list ??)				; Information.
  "List of commands requiring no argument.")
(defvar atc-command-3-key-list
  (list ?A ?L ?R)			; Altitude, Left, Right.
  "List of commands requiring an argument.")
(defvar atc-command-key-list
  (append atc-command-2-key-list atc-command-3-key-list)
  "List of all ATC commands.")
(defvar atc-last-plane -1
  "Number of last plane to be started so far (0..25).")
(defvar atc-first-plane -1
  "Number of earliest plane still in play (0..25).")
(defvar atc-moves-since-last-start 0
  "Number of moves since last plane was started.")
(defvar atc-plane-info nil
  "Vector containing all plane information.")
(defvar atc-game-in-progress nil
  "A variable which is true if a game is in progress.")

; Constants describing the nine items of plane information.

(defconst X 0)
(defconst Y 1)
(defconst U 2)
(defconst V 3)
(defconst ALTITUDE 4)
(defconst DIR-CHANGE 5)
(defconst ALT-CHANGE 6)
(defconst PROVENANCE 7)
(defconst DESTINATION 8)

; Function definitions.

(defun atc ()
  "Play Air Traffic Control: either a new game or
the resumption of a temporarily suspended game."
  (interactive)
  (if (and atc-game-in-progress
	   (y-or-n-p "Resume previously started game ? "))
      (atc-resume-game)
    (atc-new-game)))

(defun atc-new-game ()
  "Play a new game of Air Traffic Control."
  (interactive)
  (atc-prepare-game-buffer)
  (atc-zero-plane-info)
  (if (y-or-n-p "Are you ready to play ATC ? ")
      (atc-main-loop)))

(defun atc-resume-game ()
  "Resume a suspended game of Air Traffic Control."
  (interactive)
  (atc-prepare-game-buffer)
  (atc-draw-all-planes)
  (if (y-or-n-p "Are you ready to resume playing ATC ? ")
      (atc-main-loop)))

(defun atc-prepare-game-buffer ()
  (switch-to-buffer "*Air Traffic Control*")
  (erase-buffer)
  (atc-insert-board)
  (delete-other-windows)
  (goto-char (point-min))
  (sit-for 0))
  
(defun atc-zero-plane-info ()
  (setq atc-plane-info 
	(make-vector atc-nb-planes (make-vector 9 'nil)))
  ; The nine items of information for each plane are:
  ; x, y, u, v, altitude, dir-change, alt-change,
  ; provenance, destination.
  (setq atc-first-plane 0
	atc-last-plane -1
	atc-plane nil
	atc-command nil
	atc-arg nil
	atc-delay atc-initial-delay)
  (atc-start-a-plane))

(defun atc-main-loop ()
  (setq atc-game-in-progress t)
  (let ((stop-game-catch
	 (catch 'stop-game
	   (let (count)
	     (while t
	       (setq count atc-delay
		     atc-delay (- atc-delay atc-acceleration))
	       (while (> (setq count (1- count)) 0)
		 (if (input-pending-p)
		     (atc-interpret-input (read-char))))
	       (atc-move-planes)
	       (atc-check-crashes)
	       (setq atc-moves-since-last-start 
		     (1+ atc-moves-since-last-start))
	       (and (< atc-last-plane (1- atc-nb-planes))
		    (> (atc-rand atc-moves-since-last-start) 
		       atc-mean-separation)
		    (atc-start-a-plane))
	       (sit-for 0)))
	   nil)))
    (if stop-game-catch
	(cond
	 ((stringp stop-game-catch)
	  (atc-end-game stop-game-catch))
	 ((eq stop-game-catch 'quick-change)
	  (atc-quick-change))
	 ((eq stop-game-catch 'suspend)
	  (atc-suspend-game))))))

(defun atc-end-game (message)
  (ding)
  (goto-char (point-min))
  (insert message ?\n ?\n)
  (setq atc-game-in-progress nil))

(defun atc-quick-change ()
  (switch-to-buffer (other-buffer))
  (sit-for 0))

(defun atc-suspend-game ()
  (if (y-or-n-p "Game suspended.  Resume immediately ? ")
      (atc-main-loop)))

(defun atc-interpret-input (key)
  (setq key (upcase key))
  (cond
   ((= key ?\e)
    (throw 'stop-game 'quick-change))
   ((= key ?\C-z)
    (throw 'stop-game 'suspend))
   ((= key ?\C-?)
    (setq atc-plane nil
	  atc-command nil
	  atc-arg nil)
    (atc-echo-command "   "))
   (atc-command
    (and (>= key ?0)
	 (<= key ?9)
	 (setq atc-arg (- key ?0))
	 (atc-echo-command (concat (list atc-plane atc-command key)))
	 (atc-execute-command)))
   (atc-plane
    (and (memq key atc-command-key-list)
	 (setq atc-command key)
	 (atc-echo-command (concat (list atc-plane atc-command ? )))
	 (memq key atc-command-2-key-list)
	 (atc-execute-command)))
   (t
    (and (>= key ?A)
	 (<= key ?Z)
	 (setq atc-plane key)
	 (atc-echo-command (concat (list atc-plane ?  ? ))))))
  t)

(defun atc-echo-command (com)
  (goto-char 1)
  (insert "! " com)
  (zap-to-char 1 ?\n)
  (goto-char 1)
  (sit-for 0))

(defun atc-execute-command ()
  ; Check that named plane has been started.
  (setq atc-plane (- atc-plane ?A))
  (if (> atc-plane atc-last-plane)
      nil
    (let ((plane-info (aref atc-plane-info atc-plane)))
      (if (null (aref plane-info ALTITUDE))
	  nil
	(cond
	 ((= atc-command ?A)		; Change altitude.
	  (if (and (= (aref plane-info ALTITUDE) -1) ; Awaiting take-off.
		   (= atc-arg 1))
	      (progn
		(aset plane-info ALTITUDE 0)
		(aset plane-info ALT-CHANGE 1)
		(atc-update-waiting-list))
	    (aset plane-info ALT-CHANGE (int-to-string atc-arg))))
	 ((= atc-command ?L)		; Left turn.
	  (aset plane-info DIR-CHANGE atc-arg))
	 ((= atc-command ?R)		; Right turn.
	  (aset plane-info DIR-CHANGE (- atc-arg)))
	 ((= atc-command ??)		; Information.
	  (message "Aeroplane %c: from %s to %s."
		   (+ atc-plane ?A)
		   (atc-describe-place (aref plane-info PROVENANCE))
		   (atc-describe-place (aref plane-info DESTINATION)))))
	(aset atc-plane-info atc-plane plane-info))))
  (setq atc-plane nil
	atc-command nil
	atc-arg nil))

(defun atc-describe-place (place)
  (if (< place 10)
      (format "area %d/%d" place (% (1+ place) 10))
    (if (= place 10)
	"airport #"
      "airport *")))

(defun atc-choose-place ()
  (let ((m (atc-rand 100)))
    (if (< m atc-%-airports)
	(+ 10 (% m 2))
      (atc-rand 10))))

(defun atc-start-a-plane ()
  (setq atc-moves-since-last-start 0)
  (let ((prov -1)
	(dest -1))
    (while (= prov dest)
      (setq prov (atc-choose-place)
            dest (if (atc-area-p prov)
		     (+ 10 (atc-rand 2)) ; An airport: we're disallowing the
		   (atc-choose-place)))) ; possibility of area to area.
    (if (atc-area-p prov)
	(let ((symprov (% prov 5))
	      x y u v a plane-info)
	  (cond
	   ((= symprov 0)
	    (setq x (+ 1 (atc-rand 7))
		  y 0
		  u (atc-rand 2)
		  v 1))
	   ((= symprov 1)
	    (setq x (+ 9 (atc-rand 8))
		  y 0
		  u (1- (atc-rand 3))
		  v 1))
	   ((= symprov 2)
	    (setq n (atc-rand 8)
		  x (if (<= n 2) (- 20 n) 20)
		  y (if (<= n 2) 0 (- n 2))
		  u (1- (atc-rand 2))
		  v (- 1 (atc-rand (- 1 u)))))
	   ((= symprov 3)
	    (setq x 20
		  y (+ 7 (atc-rand 6))
		  u -1
		  v (1- (atc-rand 3))))
	   ((= symprov 4)
	    (setq x 20
		  y (+ 14 (atc-rand 6))
		  u -1
		  v (1- (atc-rand 2)))))
	  (if (/= prov symprov)		; Rotational symmetry, order 2.
	      (setq x (- 20 x)
		    y (- 20 y)
		    u (- u)
		    v (- v)))
	  (setq a (+ 5 (atc-rand 5))
		plane-info (vector x y u v a 'nil 'nil prov dest)
		atc-last-plane (1+ atc-last-plane))
	  (aset atc-plane-info atc-last-plane plane-info)
	  (atc-draw-plane atc-last-plane))
      (let ((plane-info (if (= prov 10)
			    (vector 10 10 -1 0 -1 'nil 'nil prov dest)
			  (vector 12 4 -1 -1 -1 'nil 'nil prov dest))))
	(setq atc-last-plane (1+ atc-last-plane))
	(aset atc-plane-info atc-last-plane plane-info)
	(atc-update-waiting-list)))))

(defun atc-draw-all-planes ()
  (let ((i atc-first-plane)
	a)
    (while (<= i atc-last-plane)
      (setq a (aref (aref atc-plane-info i) ALTITUDE))
      (if (and a (>= a 0))
	  (atc-draw-plane i))
      (setq i (1+ i)))))

(defun atc-draw-plane (plane)
  (let ((plane-info (aref atc-plane-info plane)))
    (atc-move-to (aref plane-info X) (aref plane-info Y))
    (if (looking-at (regexp-quote ". "))
	(progn
	  (delete-char 2)
	  (insert (+ ?A plane) (+ ?0 (aref plane-info ALTITUDE)))))))

(defun atc-erase-plane (plane)
  (let ((plane-info (aref atc-plane-info plane)))
    (atc-move-to (aref plane-info X) (aref plane-info Y))
    (if (looking-at "[A-Z][0-9]")
	(progn
	  (delete-char 2)
	  (insert ?. ? )))))

(defun atc-move-to (x y)
  (goto-char (point-min))
  (forward-line (+ 1 y))
  (forward-char (+ 1 (* 2 x))))
    
(defun atc-airport-p (place)
  (or (= place 10) (= place 11)))

(defun atc-area-p (place)
  (not (atc-airport-p place)))

(defun atc-rand (num)
  "Returns a random number in the range 0 to NUM - 1."
  (% (+ num (% (random) num)) num))

(defun atc-insert-board ()
  (interactive)
  (insert (format "
 0 . . . . . . . 1 . . . . . . . . 2 . . .  Air Traffic Control
 . . . . . . . . . . . . . . . . . . . . .  -------------------
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . .  atc-nb-planes = %d
 . . . . . . . . . . . . * . . . . . . . .  atc-initial-delay = %d
 . . . . . . . . . . . . . . . . . . . . .  atc-acceleration = %d
 . . . . . . . . . . . . . . . . . . . . 3  atc-%%-airports = %d
 9 . . . . . . . . . . . . . . . . . . . .  atc-mean-separation = %d
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . # . . . . . . . . . .  Awaiting Take Off (Max 5)
 . . . . . . . . . . . . . . . . . . . . .  -------------------------
 . . . $ . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . 4 
 8 . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . $ . . . . 
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . . 
 . . . . . . . . . . . . . . . . . . . . . 
 . . . 7 . . . . . . . . 6 . . . . . . . 5 "
		  atc-nb-planes
		  atc-initial-delay
		  atc-acceleration
		  atc-%-airports
		  atc-mean-separation)))

(defun atc-update-waiting-list ()
  (let ((i 0)
	(n 0)
	plane-info)
    (while (<= i atc-last-plane)
      (setq plane-info (aref atc-plane-info i)
	    a (aref plane-info ALTITUDE))
      (if (and a (= a -1))
	  (progn
	    (setq n (1+ n))
	    (atc-move-to 20 (+ n 12))
	    (forward-char 2)
	    (or (eolp)
		(kill-line nil))
	    (insert (format " %c : %s"
			    (+ i ?A)
			    (atc-describe-place (aref plane-info
						      PROVENANCE))))))
      (setq i (1+ i)))
    (atc-move-to 20 (+ n 13))
    (forward-char 2)
    (or (eolp)
	(kill-line nil))
    (if (> n 5)
	(throw 'stop-game "Airport hold-up!
The number of aeroplanes awaiting take off at any one time
must be kept at five or less."))))

(defun atc-move-planes ()
  (interactive)
  (let ((i atc-first-plane)
	plane-info nx ny u v)
    (while (<= i atc-last-plane)
      (setq plane-info (aref atc-plane-info i)
	    a (aref plane-info ALTITUDE))
      (if (or (null a) (= a -1))
	  nil
	(atc-erase-plane i)
	(setq u (aref plane-info U)
	      v (aref plane-info V)
	      nx (+ (aref plane-info X) u)
	      ny (+ (aref plane-info Y) v)
	      dch (aref plane-info DIR-CHANGE)
	      ach (aref plane-info ALT-CHANGE))
	(aset plane-info X nx)
	(aset plane-info Y ny)
	(if dch
	    (let ((nu u) (nv v))
	      (cond
	       ((> dch 0)		; Turning left.
		(setq dch (1- dch)
		      nu (sgn (+ v u))
		      nv (sgn (- v u))))
	       ((< dch 0)		; Turning right.
		(setq dch (1+ dch)
		      nu (sgn (- u v))
		      nv (sgn (+ u v)))))
	      (if (= dch 0) (setq dch nil)) ; Finished turn.
	      (aset plane-info U nu)
	      (aset plane-info V nv)
	      (aset plane-info DIR-CHANGE dch)))
	(if ach
	    (if (stringp ach)
		(aset plane-info ALT-CHANGE (string-to-int ach))
	      (let ((a (aref plane-info ALTITUDE)))
		(setq a (+ a (sgn (- ach a))))
		(aset plane-info ALTITUDE a)
		(setq ach (1+ (* 2 (/ ach 2))))
		(if (= ach a)
		    (aset plane-info ALT-CHANGE nil)
		  (aset plane-info ALT-CHANGE ach))
		(if (= a 0)
		    (progn
		      (atc-check-landing i plane-info nx ny)
		      (aset plane-info ALTITUDE nil))))))
	(if (and (>= nx 0)
		 (<= nx 20)
		 (>= ny 0)
		 (<= ny 20))
	    nil
	  (atc-check-exit i plane-info nx ny)
	  (aset plane-info ALTITUDE nil)
	  (atc-update-first-plane i))
	(aset atc-plane-info i plane-info)
	(if (aref plane-info ALTITUDE) 
	    (atc-draw-plane i)))
      (setq i (1+ i))))
  (goto-char (point-min)))

(defun atc-update-first-plane (i)
  (if (= i atc-first-plane)
      (progn
	(setq atc-first-plane (1+ atc-first-plane))
	(while (and (<= atc-first-plane atc-last-plane)
		    (null (aref (aref atc-plane-info atc-first-plane)
				ALTITUDE)))
	  (setq atc-first-plane (1+ atc-first-plane)))))
  (if (= atc-first-plane atc-nb-planes)
      (throw 'stop-game "Congratulations!
You have been a great success as an Air Traffic Controller.
If we had more people like you, air travel would be a lot safer.")))

(defun atc-check-landing (i plane-info nx ny)
  (let ((dest (aref plane-info DESTINATION)))
    (cond
     ((and (= nx 10)			; Attempt to land at #.
	   (= ny 10))
      (if (and (= (aref plane-info U) -1) ; Correct direction ?
	       (= (aref plane-info V) 0))
	  (if (= dest 10)		; Correct airport ?
	      (atc-update-first-plane i)
	    (atc-wrong-airport 10 11))
	(atc-wrong-dir-landing i
			       (aref plane-info U)
			       (aref plane-info V)
			       10)))
     ((and (= nx 12)			; Attempt to land at *.
	   (= ny 4))
      (if (and (= (aref plane-info U) -1) ; Correct direction ?
	       (= (aref plane-info V) -1))
	  (if (= dest 11)		; Correct airport ?
	      (atc-update-first-plane i)
	    (atc-wrong-airport 11 10))
	(atc-wrong-dir-landing i 
			       (aref plane-info U)
			       (aref plane-info V)
			       11)))
     (t
      (atc-bad-landing i (aref plane-info X) (aref plane-info Y))))))

(defun atc-wrong-airport (practice theory)
  (throw 'stop-game
	 (format "Wrong airport!
Aeroplane %c just landed safely at %s.
Unfortunately, it was scheduled to arrive at %s!"
		 (+ ?A i)
		 (atc-describe-place practice)
		 (atc-describe-place theory))))

(defun atc-check-exit (i plane-info nx ny)
  (let ((dest (aref plane-info DESTINATION)))
    (if (atc-airport-p dest)
	(throw 'stop-game 
	       (format "Lost aeroplane!
Aeroplane %c was meant to be landing at %s,
not leaving your airspace."
		       (+ i ?A)
		       (atc-describe-place dest)))
      (if (< (aref plane-info ALTITUDE) 5)
	  (throw 'stop-game
		 (format "Flying at dangerously low altitude!
Aeroplane %c just left your airspace at altitude %d.
All planes must achieve at least altitude 5 in open airspace."
			 (+ i ?A)
			 (aref plane-info ALTITUDE)))
	(cond
	 ((= nx -1)
	  (if (not (memq dest (list 7 8 9 0)))
	      (atc-v-bad-direction i dest)
	    (cond
	     ((= dest 7)
	      (if (or (< ny 14) (> ny 21))
		  (atc-bad-direction i dest)))
	     ((= dest 8)
	      (if (or (< ny 7) (> ny 14))
		  (atc-bad-direction i dest)))
	     ((= dest 9)
	      (if (> ny 7)
		  (atc-bad-direction i dest)))
	     ((= dest 0)
	      (if (> ny -1)
		  (atc-bad-direction i dest))))))
	 ((= ny -1)
	  (if (not (memq dest (list 9 0 1 2)))
	      (atc-v-bad-direction i dest)
	    (cond
	     ((= dest 9)
	      (if (> nx -1)
		  (atc-bad-direction i dest)))
	     ((= dest 0)
	      (if (> nx 8)
		  (atc-bad-direction i dest)))
	     ((= dest 1)
	      (if (or (< nx 8) (> nx 17))
		  (atc-bad-direction i dest)))
	     ((= dest 2)
	      (if (< nx 17)
		  (atc-bad-direction i dest))))))
	 ((= nx 21)
	  (if (not (memq dest (list 2 3 4 5)))
	      (atc-v-bad-direction i dest)
	    (cond
	     ((= dest 2)
	      (if (> ny 6)
		  (atc-bad-direction i dest)))
	     ((= dest 3)
	      (if (or (< ny 6) (> ny 13))
		  (atc-bad-direction i dest)))
	     ((= dest 4)
	      (if (< ny 13)
		  (atc-bad-direction i dest)))
	     ((= dest 5)
	      (if (< ny 21)
		  (atc-bad-direction i dest))))))
	 ((= ny 21)
	  (if (not (memq dest (list 4 5 6 7)))
	      (atc-v-bad-direction i dest)
	    (cond
	     ((= dest 4)
	      (if (< nx 21)
		  (atc-bad-direction i)))
	     ((= dest 5)
	      (if (< nx 12)
		  (atc-bad-direction i)))
	     ((= dest 6)
	      (if (or (< nx 3) (> nx 12))
		  (atc-bad-direction i)))
	     ((= dest 7)
	      (if (> nx 3)
		  (atc-bad-direction i)))))))))))

(defun atc-bad-direction (i area)
  (throw 'stop-game
	 (format "Incorrect direction!
Aeroplane %c should have left in the direction %d/%d."
		 (+ i ?A)
		 area (% (1+ area) 10))))

(defun atc-v-bad-direction (i area)
  (throw 'stop-game
	 (format "Wildly incorrect direction!
Aeroplane %c should have left in the direction %d/%d.
You have instructed it to fly miles off course!"
		 (+ i ?A)
		 area (% (1+ area) 10))))

(defun atc-bad-landing (i x y)
  (throw 'stop-game
	 (format "Crash landing!
Aeroplane %c just crashed in the middle of nowhere (%d,%d),
following your instructions to reduce altitude to zero."
		 (+ i ?A)
		 x y)))

(defun atc-wrong-dir-landing (i u v place)
  (throw 'stop-game
	 (format "Landing in the wrong direction!
Aeroplane %c just tried to land in direction (%d,%d)
and so destroyed several airport buildings.
%s is designed for landing in the direction (-1,%d)."
		 (+ i ?A)
		 u v
		 (capitalize (atc-describe-place place))
		 (if (= place 10) 0 -1))))

(defun atc-check-crashes ()
  (let ((i atc-first-plane)
	j 
	info1
	info2
	alt1
	alt2)
    (while (< i atc-last-plane)
      (setq info1 (aref atc-plane-info i))
      (if (and (setq alt1 (aref info1 ALTITUDE))
	       (> alt1 0))
	  (progn
	    (setq j (1+ i))
	    (while (<= j atc-last-plane)
	      (setq info2 (aref atc-plane-info j))
	      (and (setq alt2 (aref info2 ALTITUDE))
		   (> alt2 0)
		   (= alt1 alt2)
		   (= (aref info1 X) (aref info2 X))
		   (= (aref info1 Y) (aref info2 Y))
		   (atc-crash i j (aref info1 X) (aref info1 Y)))
	      (setq j (1+ j)))))
      (setq i (1+ i)))))

(defun atc-crash (i j x y)
  (throw 'stop-game (format "Mid-air crash!
Aeroplanes %c and %c just crashed at grid location (%d,%d)."
			    (+ i ?A)
			    (+ j ?A)
			    x y)))

(defun sgn (n)
  "Returns the sign of N, i.e. 1 if N is positive,
-1 if N is negative, 0 if N is zero."
  (cond ((> n 0) 1)
	((< n 0) -1)
	(t 0)))
