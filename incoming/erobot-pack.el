;;; erobot-pack.el --- more interesting erobots

;; Copyright (C) 2000  Alexander Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: games

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This package contains erobots to use in the erobot game.  They are
;; supposedly smarter than the sample implementations in the original
;; erobot package.

(require 'erobot)

(defun erobot-candidate-wicked-pack-hunter ()
  "The team members will form a line and eat everything.
Unfortunately it is not smart enough to actually defeat the wicked sociobots."
  (let* (;; potential foes in the vicinity
	 (victim (erobot-closest-candidate '(erobot-other-team)))
	 (dist (erobot-distance-to victim))
	 ;; orientation of the axis to form
	 (axis (ebot-pack-axis))
	 (get-coord-func (if (eq axis 'vertical) 'erobot-get-x 'erobot-get-y))
	 (team-members (erobot-get-team-members))
	 (goal-coord (round (apply '+ (mapcar get-coord-func team-members))
			    (length team-members)))
	 (coord (funcall get-coord-func))
	 ;; misc
	 (alignment-dirs '((vertical . (left right)) (horizontal . (up down))))
	 (movement-dirs '((horizontal . (left right)) (vertical . (up down)))))
    ;; bite first if immediate neighbour
    (if (= dist 1)
	(list 'erobot-move (erobot-direction-to victim))
      ;; else get in line if we are not and doing so doesn't endanger us
      (if (and (not (= goal-coord coord))
	       (setq dir (nth (if (< goal-coord coord) 0 1)
			      (cdr (assq axis alignment-dirs))))
	       (not (erobot-other-team (erobot-neighbours-dir dir))))
	  (list 'erobot-move dir)
	;; else determine preferred dir and distance we can move in that direction
	(setq dir (or (erobot-info-get 'dir)
		      (nth (random 2) (assq axis movement-dirs)))
	      dist (erobot-distance-to-non-member dir))
	;; reverse preferred dir and share the information if the way is
	;; blocked
	(if (not (erobot-free-space-dir dir dist))
	    (setq dir (erobot-opposite-dir dir)
		  dist (erobot-distance-to-non-member dir)))
	(erobot-info-share 'dir dir)
	;; 1. if member with immediately previous sort number not behind, move
	;; 2. if all members with previous sort number are in front and the position
	;;    ahead is empty, move
	(if (let ((result (if (eq (car team-members) erobot-current-candidate)
			      (list (nth (1- (length team-members)) team-members)))))
	      (if (not result)
		  (let ((candidates (reverse team-members))
			candidate)
		    (while (and candidates (not result))
		      (setq candidate (car candidates)
			    candidates (cdr candidates))
		      (if (eq candidate erobot-current-candidate)
			  (setq result candidates)))))
	      (or (not (eq (erobot-opposite-dir dir) (erobot-direction-to (car result))))
		  (and (= dist 1)
		       (not (memq nil (mapcar (function (lambda (c)
							  (eq dir (erobot-direction-to c))))
					      result))))))
	    (list 'erobot-move dir)
	  ;; else wait
	  (list 'erobot-pass))))))

(defun ebot-pack-axis ()
  "Return axis of current team.
Calculate this by taking all team members and determining
wether the max. diameter of the team is in the horizontal 
or vertical direction."
  (let ((candidates erobot-candidates)
	candidate 
	(minx (erobot-get-x)) 
	(maxx (erobot-get-x))
	(miny (erobot-get-y))
	(maxy (erobot-get-y)))
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (when (erobot-same-team candidate)
	(let ((x (erobot-get-x candidate))
	      (y (erobot-get-y candidate)))
	  (cond ((< x minx)
		 (setq minx x))
		((> x maxx)
		 (setq maxx x)))
	  (cond ((< y miny)
		 (setq miny y))
		((> y maxy)
		 (setq maxy y))))))
    (if (> (abs (- maxy miny))
	   (abs (- maxx minx)))
	'vertical
      'horizontal)))

(defun erobot-candidate-wicked-pack-hunter-test ()
  "Run a test."
  (interactive)
  (let ((erobot-map '("******************************************"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "*                                        *"
		      "******************************************"))
	(erobot-verbosity 0)
	(erobot-max-turns 500)
	bot-A bot-B bot-C bot-D bot-E bot-F bot-G bot-H)
    (fset 'bot-A 'erobot-candidate-wicked-pack-hunter)
    (fset 'bot-B 'erobot-candidate-wicked-pack-hunter)
    (fset 'bot-C 'erobot-candidate-wicked-pack-hunter)
    (fset 'bot-D 'erobot-candidate-wicked-pack-hunter)
    (fset 'bot-E 'erobot-candidate-wicked-pack-hunter)
    (fset 'bot-F 'erobot-candidate-social)
    (fset 'bot-G 'erobot-candidate-social)
    (fset 'bot-H 'erobot-candidate-social)
    (fset 'bot-I 'erobot-candidate-social)
    (fset 'bot-J 'erobot-candidate-social)
    (erobot-run-tournament 'erobot-best-of-5 
			   '(bot-A bot-B bot-C bot-D bot-E) '(bot-F bot-G bot-H bot-I bot-J))))

;;; erobot-pack.el ends here
