;;; -*- Mode: Emacs-Lisp -*-
;;; File: yank-match.el
;;; 
;;; Yanks matches for REGEXP from kill-ring.
;;; Karl Fogel
;;; (kfogel@cs.oberlin.edu)
;;;
;;; This code is distributed under the terms of the GNU General Public
;;; License. If you are not sure what that means, contact the author.
;;; If you do not have electronic mail access, write him at
;;;
;;; Karl Fogel
;;; 1123 N. Oak Park Ave.
;;; Oak Park, ILL
;;; USA      60302
;;;
;;; for more information.
;;;
;;; Please let me know what you think of it... I like messages with
;;; complaints almost as much as ones without complaints! :-)
;;;
;;; INSTALLATION:
;;; Put this stuff into your .emacs file, along with a line like:
;;; (global-set-key "\C-oy" 'yank-match)
;;;
;;; or whatever your preferred key-binding is.
;;; 
;;; USAGE:
;;; Just call yank-match ("M-x yank-match" or whatever key-binding you 
;;; have intalled for it) and supply a regular expression, and it will 
;;; yank the first item in the kill ring containing a match for that
;;; regexp.  (See documentation for the function "yank" for more details.)
;;;
;;; Yank match does set point and mark, just like "yank" does.
;;;
;;; Repeated yanks cycle through the kill-ring, taking the next match, and 
;;; the next, and go right back to the beginning and start over, until 
;;; you break the cycle with some other command.  Yank-match does not 
;;; normally move yanks to the front of the kill-ring, but see variable
;;; yank-match-modify-kill-ring to change this behaviour. 
;;;
;;;   LCD Archive Entry:
;;;   yank-match|Karl Fogel|kfogel@cs.oberlin.edu|
;;;   Yanks match(es) for REGEXP from kill-ring|
;;;   20-May-1993||~/misc/yank-match.el.el.Z|

(defvar yank-match-count 0 "Which match in kill ring yank-match will 
yank.")

(defvar yank-match-last-regexp nil "Last regexp used by yank-match.")

(defvar yank-match-modify-kill-ring nil "*Non-nil means place matches 
at the front of the kill-ring, thus making it not behave like a ring for
yank-match functions.  Instead you'd \"bounce back\" from one end of
the kill-ring to the other with repeated yank-matches.  However, each
match would then be available for immediate yanking with \\[yank].

Unless inefficiency really offends you, you should leave this set to nil.")

(defvar yank-match-inserted nil "Did we insert on last yank match?")

(defun yank-match (re)
  "Yank out the first item in the kill-ring that contains a match for
REGEXP.  Repeated execution of this command yanks you successively
through the matches in the kill-ring.  Because the kill-ring is a ring
\(or a dead ringer for one, at least\), it is okay for you to repeat
more times than the length of the kill-ring.  It just means that your
mistakes will come back to haunt you.

Matches are not put automatically at the front of the kill ring, but
if you do a \\[copy-region-as-kill] or a \\[kill-ring-save]
immediately after finding the match you wanted, it will then be put at
the front of the ring, and \\[yank] will default to that match.

See also variable yank-match-modify-kill-ring for a way to make matches
automatically be put at the front of the kill-ring \(and thus be available
for immediate yanking\)."
  (interactive (if (equal last-command 'yank-match)
		   (list yank-match-last-regexp)
		 (list (read-from-minibuffer "Yank match (regexp): "))))
  (let ((repeating (equal last-command 'yank-match)))
    (if repeating
	(progn
	  ;; if inserted on last yank, kill that region before yanking new
	  (if yank-match-inserted 
	      (progn
		(setq yank-match-inserted nil)
		(delete-region (mark) (point))))

	  ;; if repeating and successful match this time
	  (if (do-yank-match re yank-match-count)
		(setq yank-match-count (1+ yank-match-count))

	    ;; if repeating and unsuccessful match this time
	    (progn
	      (setq yank-match-count 1)
	      (do-yank-match re 0))))
		      
      ;; if not repeating
      (if (do-yank-match re 0)
	  (setq yank-match-count 1)
	(error 
	 (concat "No match found for \"" re "\" in kill-ring.")))))
  (setq yank-match-last-regexp re)
  (setq this-command 'yank-match))


(defun do-yank-match (re num)
  ;; if you can stand to read this function, then you should have your head
  ;; examined...
  (let ((found-one t))
    (catch 'done
      (let ((index 0)
	    (len (1- (length kill-ring))))
	(progn
	  (while (<= index len)
	    (let ((str (nth index kill-ring)))
	      (if (string-match re str)
		  (if (= num 0)
		      (progn (setq found-one nil)
			     (setq yank-match-inserted t)
			     (push-mark (point))
			     (insert str)
			     (if yank-match-modify-kill-ring
				 (setq 
				  kill-ring
				  (cons str (delq str kill-ring))))
			     (throw 'done nil))
		    (progn (setq found-one t)
			   (setq num (1- num))
			   (setq index (1+ index))))
		(setq index (1+ index))))))))
    (not found-one))) ; so it returns t on success! 

;;; end yank-match.el ;;;
