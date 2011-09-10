;To: unix-emacs@bbn.com
;Date: 17 Feb 89 13:59:45 GMT
;From: Mike Heavin <ogccse!littlei!omepd!intelob.biin.com!mgh@husc6.harvard.edu>
;Subject: Re: Copy From Above?
;
;
;Paul Houtz writes:
;
;Can anyone tell me how I can get gnuemacs to (with one keystroke) copy
;the character on the line above the cursor to the current cursor
;position?
;
;   For example, my screen looks like this:
;
;
;            This is the first  line of text.
;            This is the second line of text.
;            This is the third  line of text.
;            _
;
;
;   My cursor is sitting where the "_" character is shown.  After typing
;in the keystroke mapped to the functionality I want, you should see:
;
;            This is the first  line of text.
;            This is the second line of text.
;            This is the third  line of text.
;            T_
;
;    If I repeat it multiple times or use cntl-u (say 5) and then the
;keystroke, you should see:
;
;            This is the first  line of text.
;            This is the second line of text.
;            This is the third  line of text.
;            This i_
;
;    Any help would be appreciated.
;
;--------------------------
;how about the following:

(defun copy-char-above-cursor ()
"Copy the character immediately above the cursor onto the current line."
  (interactive)
  (point-to-register 1)
  (setq temp-column (current-column))
  (previous-line 1)
  (beginning-of-line)
  (forward-char temp-column)
  (setq temp-start-region (dot-marker))
  (forward-char 1)
  (setq temp-end-region (dot-marker))
  (copy-to-register 0 temp-start-region temp-end-region)
  (register-to-point 1)
  (insert-register 0 1)
)

