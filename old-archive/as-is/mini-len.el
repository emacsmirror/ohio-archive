;Return-Path: <jbw%bucsf.BU.EDU@bu-cs.bu.edu>
;Date:  Wed, 19 Apr 89 18:25:59 edt
;From: Joe Wells <jbw%bucsf.BU.EDU@bu-cs.bu.edu>
;To: peck@sun.com
;Cc: bug-gnu-emacs@prep.ai.mit.edu
;Subject: Minibuf window coordinates
;
;Jeff Peck wrote this:
;
;   I don't know what the prompt string is, so we just guess...  This
;   code from sun-fns.el, the "common cases" are `Eval: ', ie 4 char
;   prompts.  If your prompt string is longer, the mouse will still be a
;   few more chars to the right.
;
;   (defun mini-move-point (window x y)
;     ;; -6 is good for most common cases
;     (mouse-move-point window (- x 6) 0))
;
;    If there is a variable holding the current prompt string,
;   the "6" could be replaced with (length prompt-string) + 1 or + 2
;   for the punctuation.
;
;    Does someone out in GNU-land know the appropriate transformation?
;
;Try this function:
;
(defun minibuffer-prompt-length ()
  "Returns the length of the current minibuffer prompt."
  (let ((window (selected-window))
	length)
    (select-window (minibuffer-window))
    (let ((point (point)))
      (goto-char (point-min))
      (insert-char ?a 200)
      (goto-char (point-min))
      (vertical-motion 1)
      (setq length (- (screen-width) (point)))
      (goto-char (point-min))
      (delete-char 200)
      (goto-char point))
    (select-window window)
    length))

;Enjoy!
;
;--
;Joe Wells <jbw@bucsf.bu.edu>
;jbw%bucsf.bu.edu@bu-it.bu.edu
;...!harvard!bu-cs!bucsf!jbw

