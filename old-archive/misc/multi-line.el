;From ark1!uakari.primate.wisc.edu!brutus.cs.uiuc.edu!caesar.cs.montana.edu!ogicse!ucsd!ucsdhub!hp-sdd!hplabs!hpfcso!jka Fri Dec 15 12:39:47 1989
;Article 1067 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!brutus.cs.uiuc.edu!caesar.cs.montana.edu!ogicse!ucsd!ucsdhub!hp-sdd!hplabs!hpfcso!jka
;>From jka@hpfcso.HP.COM (Jay Adams)
;Newsgroups: comp.emacs
;Subject: Re: Grow and shrink minibuffer window as minibuffer grows and shrinks
;Message-ID: <8970006@hpfcso.HP.COM>
;Date: 14 Dec 89 18:06:47 GMT
;References: <8912140405.AA00554@moose.crd.Ge.Com>
;Organization: Hewlett-Packard, Fort Collins, CO, USA
;Lines: 87
;
;
;As it turns out, I was working along similar lines not too long ago.
;Here is what I came up with.  multi-line-message and
;multi-line-read-from-minibuffer could be replacements for the standard
;emacs functions message and read-from-minibuffer.
;multi-line-read-from-minibuffer, however, uses recursive-edit to read
;the input string.  The real read-from-minibuffer does some kind of
;magic to read the input string.  
;
;This code is not copyrighted in any way so feel free to mass-produce
;it, claim it as your own work, sell it for some exorbitant price, and
;make a small fortune.
;
;- Jay


(defun multi-line-message (string)
  "Sort of like message 'cept STRING can be many lines."
  (let ((index 0)
	(lines (lines-in-string string)))
    (if (= lines 1)
	(message str)
      (let (start
	    end
	    (old-window (selected-window)))
	(unwind-protect
	    (progn
	      (select-window (minibuffer-window))
	      (enlarge-window (1- lines))
	      (setq start (point))
	      (insert string)
	      (setq end (point))
	      (select-window old-window)
	      (sit-for 33554431))
	  (select-window (minibuffer-window))
	  (enlarge-window (- 1 lines))
	  (delete-region start end)
	  (select-window old-window))))))

(defun lines-in-string (s width)
  (let ((index 0)
	(last 0)
	(lines 1))
    (while (string-match "\n" s index)
      (if (>= (- (match-beginning 0) last) (1- width))
	  (setq lines (1+ lines)))
      (setq lines (1+ lines)
	    last  index
	    index (match-end 0)))
    (if (>= (- (length s) index) (1- width))
	(setq lines (1+ lines)))
    lines))

(defun multi-line-read-from-minibuffer (prompt &optional initial keymap read)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
Third arg KEYMAP is a keymap to use whilst reading; the default is
  minibuffer-local-map.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object  (ie  (car (read-from-string <input-string>)))"
  (or initial (setq initial ""))
  (or keymap (setq keymap minibuffer-local-map))
  (save-window-excursion
    (select-window (minibuffer-window))
    (let ((start (point))
	  input-begin
	  (scroll-step 1)
	  (lines (+ -1
		    (lines-in-string prompt (window-width))
		    (lines-in-string initial (window-width))))
	  result)
      (unwind-protect
	  (progn
	    (enlarge-window (1- lines))
	    (insert "*")
	    (insert prompt)
	    (setq input-begin (point))
	    (insert initial)
	    (use-local-map keymap)
	    (recursive-edit)
	    (setq result (buffer-substring input-begin (point-max))))
	(enlarge-window (- 1 lines))
	(delete-region start (point-max)))
      (if read
	  (car (read-from-string result))
	result))))


