;From ark1!nap1!ucbvax!kosciusko.esd.3com.com!mdb Thu May 24 22:53:49 EDT 1990
;Article 1977 of comp.emacs:
;Path: ark1!nap1!ucbvax!kosciusko.esd.3com.com!mdb
;>From: mdb@kosciusko.esd.3com.com ("Mark D. Baushke")
;Newsgroups: comp.emacs
;Subject: Re: looking for "semi-greedy" next line function
;Message-ID: <9005210150.AA20761@kosciusko.ESD.3Com.COM>
;Date: 21 May 90 01:50:22 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Organization: 3Com, 2081 N. Shoreline Blvd., Mountain View, CA 94043
;Lines: 75
;
;On 20 May 90 23:56:44 GMT,
;sullivan@edison.seas.ucla.edu (Gary J. Sullivan) said:
;
;Gary>   I'm looking for a "semi-greedy" next line function.
;Gary>   Here's the explanation:
;
;[...]
;
;Gary>   I would like it to add ONE line, but no more, i.e., to add a line
;Gary>   if the last line is not empty, otherwise not to add a line.
;
;Gary>   Anyone know how?  (email responses preferred to postings)
;
;Gary>   Thanks in advance,
;Gary>   Gary Sullivan
;Gary>   (sullivan@edison.seas.ucla.edu or sullivan@icsl.ucla.edu)
;
;I use the code following my .signature. It does what you want.
;
;Enjoy!
;-- 
;Mark D. Baushke
;Internet:   mdb@ESD.3Com.COM
;UUCP:	    {3comvax,auspex,sun}!bridge2!mdb

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next-line-noextend.el --- do not let C-n add lines to a file
;; 
;; Author          : Mark D. Baushke
;; Created On      : Mon Mar  6 14:13:27 1989
;; Last Modified By: Mark D. Baushke
;; Last Modified On: Mon Mar  6 14:13:52 1989
;; Update Count    : 1
;; Status          : Works for me with GNU Emacs 18.55
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Ever feel like not adding lots of blank lines to the end of a file?
;;
(defun next-line-noextend (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one and the last line
does not end in a newline character,
a newline character is inserted to create a line
and the cursor moves to that line.
Otherwise, a warning message is generated.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (not (eq (preceding-char) ?\n))
	    (insert ?\n)
	  (if (= opoint (point))
	      (progn
		(beep)
		(message "End of buffer"))
	    (goto-char opoint)
	    (line-move arg))))
    (line-move arg))
  nil)
(substitute-key-definition 'next-line 'next-line-noextend global-map)
(substitute-key-definition 'next-line 'next-line-noextend esc-map)
(substitute-key-definition 'next-line 'next-line-noextend ctl-x-map)


