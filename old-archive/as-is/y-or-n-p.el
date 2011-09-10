;Date: Tue, 7 Feb 89 00:19:19 EST
;From: bard@theory.lcs.mit.edu
;To: jthomas@nmsu.edu
;Cc: unix-emacs@bbn.com
;Subject: y-n-or-whatever-p
;
;
;> Then I was too obtuse.  y-or-n-p reads a single character.  yes-or-no-p
;> reads characters.  I can switch buffers (C-x C-o) when the questioner uses
;> yes-or-no-p but not when it uses y-or-n-p.  The y-or-n-p routine does not
;> allow one to go check things out because C-x is taken as a "no" answer.  My
;> point is that they have two differences.  One is the length of the answer
;> and the other is what you are allowed to do while answering.
;> 
;> So, where I'm not worried about the question, y-or-n-p is fine, but I don't
;> mind having to type yes when I'm possibly in trouble.
;
;This has bothered me too.  Here's my fairly baroque proposal for a fix,
;backed up by some code.  y-or-n-p should accept answers other than y or n
;(which screws up the name, but who cares?).  Here's a draft of the function;
;it has options like:
;  r = recursive edit.  
;      This lets you go looking around the rest of the world and such,
;      and still get back to the question.
;  a = abort (almost like c-g, but can ask for confirmation 
;      -- is this complexity a good idea?)
;  * = recursive edit in a less safe manner that lets you frob the
;      position, window configuration, etc. (r restores all those things)
;      I don't know how dangerous this may be.
;  h = print a help message if y-or-n-p was called with one.
;  ? = list these commands
;
;There is a confirm option.  It can be the atom 'yes-or-no, which asks "Do you
;really mean it?" and wants a yes or no for an answer.  I thought about a
;similar use of y-or-n, but I think people will get confused about whether it
;is
;  "do you really mean it"   (usual answer is y)
;or
;  "type your answer again if you meant it".
;which are very different if you answered "n".  
;
;Alternatively, the confirm option can be a function to call with some
;appropriate arguments; see the documentation.  If you want to be *very*
;careful about confirmation, use the paranoid-confirm function I have listed.
;It generates a random number which the user has to type back at the computer
;to confirm the answer, forcing her to look at the prompt.  Good for real
;paranoids, and calls of the form:
;
;  (y-or-n-p 
;    "Destroy all your files, club you over the head with a frozen leg of
;     lamb, shoot your spouse with a poisoned crossbow bolt, spray huge
;     amounts of meta-hyper-fluorochlorocarbons into the atmosphere,
;     send the Earth spiralling into the sun, and delete this buffer?"
;    (function paranoid-confirm))
;
;
;There's a variable controlling some assorted behavior which I thought might
;be obnoxious when I was playing with it.  E.g., the command telling where 
;the exit-recursive-edit command is is too slow, so I have some ways around
;it.
;
;Comments and improvements are welcome.  Of course, if you run this y-or-n-p,
;it won't work on calls from C code.
;
;
;-- Bard the emacs gargoyle


;; Copyright (C) 1989 by Bard Bloom and the Free Software Foundation.
;; This code is distributed under the guarantee that it is an early
;; draft and needs to be thought out better before it is correct.
;; People are encouraged to adapt this code and distribute the revised 
;; version to the network.

(defvar y-or-n-p-nice '(recursive-edit slow abort)
  "Make y-or-n-p behave nicely.  It's a list of atoms.  
recursive-edit ==> y-or-n-p tells you how to get out of a recursive edit
  slow ==========> if your computer is slow about substitute-command-keys,
                   this prints a nice little message rather than doing
                   nothing for a while.
abort ===========> prompt you when you ask to abort a command.
")

(defun y-or-n-p (question &optional confirm help)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
No confirmation of the answer is requested unless CONFIRM is true;
a single character is enough.
Also accepts Space or Return to mean yes, or Delete to mean no.
Other options:
  ? for help about y-or-n-p
  h prints the HELP argument if any. (Or calls it if it is a function)
  r invokes a recursive edit
  a aborts
  * invokes a recursive edit without some safety features.
CONFIRM may be:
  'yes-or-no  -- confirmation is via yes-or-no-p.
  a function -- called (CONFIRM QUESTION HELP ANSWER)
    (where ANSWER is t or nil depending on what the user typed.)
    Should return a two-element list (answered answer):
     (nil anything) = ask the y-or-n-p question again
     (t   t)        = y-or-n-p returns t
     (t   nil)      = y-or-n-p returns nil

"
  (let ((answered nil)
        (message (concat question " [ynhra?]"))
        answer
        c
        (recursive-edit-prelude
         (function
          (lambda ()
            (if (memq 'recursive-edit y-or-n-p-nice)
                (progn
                  (if (memq 'slow y-or-n-p-nice)
                      (message "One moment please..."))
                  (message
                   (substitute-command-keys
                    "Entering recursive edit -- \\[exit-recursive-edit] to return.")))))))
        )
    (while (not answered)
      (message message)
      (setq c (downcase (read-char)))
      (cond
       ((memq c '(?y ?\  ?\n)) (setq answered t answer t))
       ((memq c '(?n ?\C-?))   (setq answered t answer nil))
       ((memq c '(?r))
        (funcall recursive-edit-prelude)
        (save-window-excursion
          (save-excursion
            (save-restriction
              (widen)
              (recursive-edit))))
        ;; no answer from the recursive edit, of course
        )
       ((memq c '(?*))
        (funcall recursive-edit-prelude)
        (recursive-edit))
       ((memq c '(?a))
        (if (if
                (memq 'abort y-or-n-p-nice)
             (let ((the-question question))
                   ;; damned dynamic binding!
                 (new-y-or-n-p "Abort this command?"
                          (function
                           (lambda () (message "Abort `%s'" the-question)))))
             t)
            (error "`%s' aborted." question)))
       ((memq c '(?h))
        (cond
         ((null help)
          (message "Sorry -- no help for this question."))
         ((stringp help)
          (message help))
         (t
          (funcall help)))
        (sit-for 2))
       ((memq c '(??))
        (message
         "Y[es], N[o], H[elp about question], R[ecursive edit], A[bort]")
        (sit-for 2))
       (t (beep)))
      ;; now confirm it (maybe)
      (cond
       ((or (not answered) (not confirm))
        ;; go on to next query
        )
       ((eq confirm 'yes-or-no)
        (setq answered (yes-or-no-p (format
                                     "Just checking: do you really mean %s? "
                                     (if answer '\"Yes\" '\"No\")))))
       (t
        (let ((conf (funcall confirm question help answer)))
          (setq answered (nth 0 conf)
                answer   (nth 1 conf)))))
      )
    answer))

(defun paranoid-confirm (q h a)
  (let* ((r (random))
         (user-r
          (string-to-int
           (read-from-minibuffer
            (format "Type %d to confirm your %s answer to `%s':"
                    r
                    (if a "\"yes\"" "\"no\"")
                    q)
            ))))
    (cond
     ((= r user-r) (list t a))
     (t (message "...not confirmed...")
        (sit-for 2)
        '(nil nil)))))

