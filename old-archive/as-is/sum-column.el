;Date: Mon, 20 Mar 89 19:40:57 EST
;From: bard@theory.lcs.mit.edu
;To: pierson%mist.lcs.mit.edu@CHIPS.BBN.COM
;Cc: unix-emacs@bbn.com
;Subject: Summing Columns
;
;
;Here's a trivially improved version of Dan Pierson's sum-column function.
;The improvements are more as advice about gnumacs-lisp coding than
;significant ones:
;
;  (1) using (interactive "r") instead of (interactive "d\nm")
;      and then figuring out which is the beginning and which the end
;  (2) eta-reducing the '(lambda (x) (string-to-int x)) into
;      (function string-to-int).  (function is like quote, but 
;      possibly better for the compiler.)
;  (3) Only printing the result when it's called interactively, so
;      that other programs can call it without cluttering up the 
;      message-line with random "The sum is 78" messages.  Ever wonder
;      why monkey-mode keeps saying "Done" when it is obviously not done?
;
;There's also a very cheap sum-region command, which adds up all the numbers
;in the region.  It's not very smart; e.g., I got lazy about treating "- 1"
;and "-1" the same.  (The second one is -1; the first is +1.)  Improvements
;are welcome.  
;
;It would be nice to have (e.g.) rational arithmetic commands, so that you
;could type 
;  4/3 + (2*3)/(8+1)
;and run m-x eval-rational-expression-insert, and it would replace that
;expression by 2. (Assuming I've done it right.)  Even adding up lists of
;numbers with decimals would be nice -- "You owe me 4.12+51.00+40.30"
;
;-- Bard the emacs gargoyle
;
;------------ snip here ------------


(defun sum-column (start end)
  "Return the sum of the integers in the rectangle delimited
by START and END. Interactively, it prints the sum as well, and uses the
region."
  (interactive "r")
  (let* ((str-nums (extract-rectangle start end))
	 (nums (mapcar (function string-to-int) str-nums))
	 (sum (apply (function +) nums)))
    (if (interactive-p) (message "%d" sum))
    sum))

(defun sum-region (start end)
  "Adds up the numbers in the region START to END.  Primitive as yet.
If called interactively, uses the region and prints the sum in a message.
Ignores things that aren't numbers or signs, so 
  $1 + $4 
will sum to 5, and
  1 -4
will sum to -3.  Of course, it's really dumb, and things like
  2*3 - 5
will sum to 2+3+5 = 10 -- as will 2 + 3.5.  Improvements welcomed."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((sum 0)
          sign number)
      (while (re-search-forward "\\(-?\\)\\([0-9]+\\)" end t)
        ;; maybe allow whitespace between sign and number?
        ;; that's why I'm parsing clumsily.
        (setq sign
                (= (match-beginning 1) (match-end 1))
              number (string-to-int
                      (buffer-substring (match-beginning 2) (match-end 2))))
        (setq sum
              (if sign (+ sum number) (- sum number))))
      (if (interactive-p)
          (message "The sum is %d" sum))
      sum)))

