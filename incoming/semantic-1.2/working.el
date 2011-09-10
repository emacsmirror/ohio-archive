;;; working --- Display a "working" message in the minibuffer.

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.2
;; Keywords: status

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Working lets Emacs Lisp programmers easily display working messages.
;; These messages typically come in the form of a percentile, or generic
;; doodles if a maximum is unknown.
;;
;; The working entry points are quite simple.  If you have a loop that needs
;; to display a status as it goes along, it would look like this:
;;
;;  (working-status-forms "Doing stuff" "done"
;;    (while condition
;;  	(working-status (calc-percentile))
;;  	(my-work))
;;    (working-status t))
;;
;; If you cannot calculate a percentile, use the function
;; `working-dynamic-status' instead, and pass in what you know.  For
;; both status printing functions, the first argument is optional,
;; and you may pass in additional arguments as `format' elements
;; to the first argument of `working-status-forms'.
;;
;; See the examples at the end of the buffer.

;;; Backwards Compatibility:
;;
;; If you want to use working in your program, but don't want to force people
;; to install working, use could add this at the beginning of your program for
;; compatibility.
;;
;; (eval-and-compile
;;   (condition-case nil
;; 	 (require 'working)
;;     (error
;; 	(progn
;; 	  (defmacro working-status-forms (message donestr &rest forms)
;; 	    "Contain a block of code during which a working status is shown."
;; 	    (list 'let (list (list 'msg message) (list 'dstr donestr)
;; 			     '(ref1 0))
;; 		  (cons 'progn forms)))
;;   
;; 	  (defun working-status (&optional percent &rest args)
;; 	    "Called within the macro `working-status-forms', show the status."
;; 	    (message "%s%s" (apply 'format msg args)
;; 		     (if (eq percent t) (concat "... " dstr)
;; 		       (format "... %3d%%"
;; 			       (or percent
;; 				   (floor (* 100.0 (/ (float (point))
;; 						      (point-max)))))))))
;;   
;; 	  (defun working-dynamic-status (&optional number &rest args)
;; 	    "Called within the macro `working-status-forms', show the status."
;; 	    (message "%s%s" (apply 'format msg args)
;; 		     (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% ref1 4))))
;; 	    (setq ref1 (1+ ref1)))
;;   
;; 	  (put 'working-status-forms 'lisp-indent-function 2)))))
;;
;; Depending on what features you use, it is, of course, easy to
;; reduce the total size of the above by omitting those features you
;; do not use.

;;; History:
;; 
;; 1.0 First Version
;;
;; 1.1 Working messages are no longer logged.
;;     Added a generic animation display funciton:
;;        Convert celeron to animator
;;        Added a bounce display
;;     Made working robust under a multi-frame environment (speedbar)
;;
;; 1.2 Fix up documentation.
;;     Updated dotgrowth function for exceptionally large numbers of dots.
;;     Added the percentage bubble displays.

(require 'custom)

;;; Code:
(defgroup working nil
  "Working messages display."
  :prefix "working"
  :group 'lisp
  )

;;; User configurable variables
;;
(defcustom working-status-percentage-type 'working-bar-percent-display
  "Function used to display the percent status.
Functions provided in `working' are:
  `working-percent-display'
  `working-bar-display'
  `working-bar-percent-display'
  `working-percent-bar-display'
  `working-bubble-display'
  `working-bubble-precent-display'
  `working-celeron-percent-display'"
  :group 'working
  :type '(choice (const working-percent-display)
		 (const working-bar-display)
		 (const working-bar-percent-display)
		 (const working-percent-bar-display)
		 (const working-bubble-display)
		 (const working-bubble-percent-display)
		 (const working-celeron-percent-display)))

(defcustom working-status-dynamic-type 'working-celeron-display
  "Function used to display an animation indicating progress being made.
Dynamic working types occur when the program does not know how long
it will take ahead of time.  Functions provided in `working' are:
  `working-number-display'
  `working-spinner-display'
  `working-dotgrowth-display'
  `working-celeron-display'
  `working-bounce-display'"
  :group 'working
  :type '(choice (const working-number-display)
		 (const working-spinner-display)
		 (const working-dotgrowth-display)
		 (const working-celeron-display)
		 (const working-bounce-display)))

;;; Variables used in stages
;;
(defvar working-message nil
  "Message stored when in a status loop.")
(defvar working-donestring nil
  "Done string stored when in a status loop.")
(defvar working-ref1 nil
  "A reference number used in a status loop.")

;;; Programmer functions
;;
(defmacro working-status-forms (message donestr &rest forms)
  "Contain a block of code during which a working status is shown.
MESSAGE is the message string to use and DONESTR is the completed text
to use when the functions `working-status' is called from FORMS."
  (list 'let (list (list 'working-message message)
		   (list 'working-donestring donestr)
		   '(working-ref1 0))
	(cons 'progn forms)))
(put 'working-status-forms 'lisp-indent-function 2)

(defun working-status (&optional percent &rest args)
  "Called within the macro `working-status-forms', show the status.
If PERCENT is nil, then calculate PERCENT from the value of `point' in
the current buffer.  If it is a number or float, use it as the raw
percentile.
Additional ARGS are passed to fill on % elements of MESSAGE from the
macro `working-status-forms'."
  (let* ((p (or percent
		(floor (* 100.0 (/ (float (point)) (point-max))))))
	 (m1 (apply 'format working-message args))
	 (m2 (funcall working-status-percentage-type (length m1) p))
	 (message-log-max))
    (message "%s%s" m1 m2)))

(defun working-dynamic-status (&optional number &rest args)
  "Called within the macro `working-status-forms', show the status.
If NUMBER is nil, then increment a local NUMBER from 0 with each call.
If it is a number or float, use it as the raw percentile.
Additional ARGS are passed to fill on % elements of MESSAGE from the
macro `working-status-forms'."
  (let* ((n (or number working-ref1))
	 (m1 (apply 'format working-message args))
	 (m2 (funcall working-status-dynamic-type (length m1) n))
	 (message-log-max))
    (message "%s%s" m1 m2)
    (setq working-ref1 (1+ working-ref1))))

;;; Utilities
;;
(defun working-message-frame-width ()
  "Return the width of the frame the working message will be in."
  (let* ((mbw (cond ((fboundp 'frame-parameter)
		     (frame-parameter (selected-frame) 'minibuffer))
		    ((fboundp 'frame-property)
		     (frame-property (selected-frame) 'minibuffer))))
	 (fr (if (and mbw (not (eq mbw t)))
		 (window-frame mbw) default-minibuffer-frame)))
    (frame-width fr)))

;;; Percentage display types.
;;
(defun working-percent-display (length percent)
  "Return the percentage of the buffer that is done in a string.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (cond ((eq percent t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %3d%%" percent))))

(defun working-bar-display (length percent)
  "Return a string with a bar-graph showing percent.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let ((bs (- (working-message-frame-width) length 5)))
    (cond ((eq percent t)
	   (concat ": [" (make-string bs ?#) "] " working-donestring))
	  ((< bs 0) "")
	  (t (let ((bsl (floor (* (/ percent 100.0) bs))))
	       (concat ": ["
		       (make-string bsl ?#)
		       (make-string (- bs bsl) ?.)
		       "]"))))))

(defun working-bar-percent-display (length percent)
  "Return a string with a bar-graph and percentile showing percentage.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let* ((ps (if (eq percent t)
		 (concat "... " working-donestring)
	       (working-percent-display length percent)))
	 (psl (+ 2 length (if (eq percent t) working-ref1 (length ps)))))
    (cond ((eq percent t)
	   (concat (working-bar-display psl 100) " " ps))
	  (t
	   (setq working-ref1 (length ps))
	   (concat (working-bar-display psl percent) " " ps)))))

(defun working-percent-bar-display (length percent)
  "Return a string with a percentile and bar-graph showing percentage.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let* ((ps (if (eq percent t)
		 (concat "... " working-donestring)
	       (working-percent-display length percent)))
	 (psl (+ 1 length (if (eq percent t) working-ref1 (length ps)))))
    (cond ((eq percent t)
	   (concat ps " " (working-bar-display psl 100)))
	  (t
	   (setq working-ref1 (length ps))
	   (concat ps " " (working-bar-display psl percent))))))

(defun working-bubble-display (length percent)
  "Return a string with a bubble graph indicating the precent completed.
LENGTH is the amount of the display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (if (eq percent t)
      (concat " [@@@@@@@@@@@@@@@@@@@@] " working-donestring)
    (let ((bs " [")
	  (bubbles [ ?. ?- ?o ?O ?@ ]))
      (if (> percent 5)
	  (setq bs (concat bs (make-string (/ (floor percent) 5) ?@))))
      (setq bs (concat bs
		       (char-to-string (aref bubbles (% (floor percent) 5)))))
      (if (< (/ (floor percent) 5) 20)
	  (setq bs (concat bs (make-string (- 19 (/ (floor percent) 5)) ? ))))
      (concat bs "]"))))

(defun working-bubble-percent-display (length percent)
  "Return a string with a percentile and bubble graph showing percentage.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let* ((ps (if (eq percent t)
		 (concat " ... " working-donestring)
	       (working-percent-display length percent)))
	 (psl (+ 1 length (if (eq percent t) working-ref1 (length ps)))))
    (cond ((eq percent t)
	   (concat (working-bubble-display psl t)))
	  (t
	   (setq working-ref1 (length ps))
	   (concat (working-bubble-display psl percent) ps)))))

(defun working-celeron-percent-display (length percent)
  "Return a string with a celeron and string showing percent.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (prog1
      (cond ((eq percent t) (working-celeron-display length t))
	    ;; All the % signs because it then gets passed to message.
	    (t (format "%s %3d%%"
		       (working-celeron-display length 0)
		       percent)))
    (setq working-ref1 (1+ working-ref1))))

;;; Dynamic display types.
;;
(defun working-number-display (length number)
  "Return a string display the number of things that happened.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %d" number))))

(defun working-spinner-display (length number)
  "Return a string displaying a spinner based on a number.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% working-ref1 4))))))

(defun working-dotgrowth-display (length number)
  "Return a string displaying growing dots due to activity.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display.
This display happens to ignore NUMBER."
  (let* ((width (- (working-message-frame-width) 4 length))
	 (num-wrap (/ working-ref1 width))
	 (num-. (% working-ref1 width))
	 (dots [ ?. ?, ?o ?* ?O ?@ ?# ]))
    (concat " (" (make-string num-. (aref dots (% num-wrap (length dots)))) ")"
	    (if (eq number t) (concat " " working-donestring) ""))))

(defun working-frame-animation-display (length number frames)
  "Manage a simple frame-based animation for working functions.
LENGTH is the number of characters left.  NUMBER is a passed in
number (which happens to be ignored.).  While coders pass t into
NUMBER, functions using this should convert NUMBER into a vector
describing how to render the done message.
Argument FRAMES are the frames used in the animation."
  (cond ((vectorp number)
	 (let ((zone (- (length (aref frames 0)) (length (aref number 0))
			(length (aref number 1)))))
	   (if (< (length working-donestring) zone)
	       (concat " " (aref number 0)
		       (make-string
			(ceiling (/ (- (float zone)
				       (length working-donestring)) 2)) ? )
		       working-donestring
		       (make-string
			(floor (/ (- (float zone)
				     (length working-donestring)) 2)) ? )
		       (aref number 1))
	     (concat " " (aref frames (% working-ref1 (length frames)))
		     " " working-donestring))))
	(t (concat " " (aref frames (% working-ref1 (length frames)))))))

(defvar working-celeron-strings
  [ "[O     ]" "[oO    ]" "[-oO   ]" "[ -oO  ]" "[  -oO ]" "[   -oO]"
    "[    -O]" "[     O]" "[    Oo]" "[   Oo-]"  "[  Oo- ]" "[ Oo-  ]"
    "[Oo-   ]" "[O-    ]"]
  "Strings representing a silly celeron.")

(defun working-celeron-display (length number)
  "Return a string displaying a celeron as things happen.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t)
	 (working-frame-animation-display length [ "[" "]" ]
					  working-celeron-strings))
	;; All the % signs because it then gets passed to message.
	(t (working-frame-animation-display length number
					    working-celeron-strings))))

(defvar working-bounce-strings
  [
   "[_         ]"
   "[ -        ]"
   "[  ~       ]"
   "[   -      ]"
   "[    _     ]"
   "[     -    ]"
   "[      ~   ]"
   "[       -  ]"
   "[        _ ]"
   "[         -]"

   ]
  "Strings for the bounce animation.")
 
(defun working-bounce-display (length number)
  "Return a string displaying a celeron as things happen.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t)
	 (working-frame-animation-display length [ "[" "]" ]
					  working-bounce-strings))
	;; All the % signs because it then gets passed to message.
	(t (working-frame-animation-display length number
					    working-bounce-strings))))

;;; Some edebug hooks
;;
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec working-status-forms (form form def-body))))

;;; Example function using `working'
;;
(defun working-verify-parenthesis-a ()
  "Verify all the parenthesis in an elisp program buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (working-status-forms "Scanning" "done"
      (while (not (eobp))
	;; Use default buffer position.
	(working-status)
	(forward-sexp 1)
	(sleep-for 0.05)
	)
      (working-status t))))
 
(defun working-verify-parenthesis-b ()
  "Verify all the parenthesis in an elisp program buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (working-status-forms "Scanning" "done"
      (while (not (eobp))
	;; Use default buffer position.
	(working-dynamic-status nil)
	(forward-sexp 1)
	(sleep-for 0.05)
	)
      (working-dynamic-status t))))

(provide 'working)

;;; working.el ends here
