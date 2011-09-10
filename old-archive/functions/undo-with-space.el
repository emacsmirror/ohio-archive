; From: narten@cs.albany.edu (Thomas Narten)
; Subject: Undo-with-space
; Date: 18 Oct 90 02:06:11 GMT
; Organization: Computer Science Department, SUNY at Albany, Albany, NY 12222
; 
; All this chatter about undo prompts me to post the following undo
; code.  I find it much more convenient for undoing things than the
; standard undo. When invoked, just keep in hitting space until you've
; gone back as far as you want.  One of those things from gosmacs that
; caused enormous withdrawal pains. :-)
; 
(defun undo-with-space ()
  (interactive)
  (undo-start)
  (undo-more 2)
  (message "Hit <space> to undo more")
  (let ((char (read-char)))
    (while (= char 32)
      (message "undoing..")
      (undo-more 1)
      (message "Hit <space> to undo more")
      (setq char (read-char)))
    (message "Finished undoing")
    (if (not (= char search-exit-char))	; make exit character the same as isearch
	(undo-with-space-execute-key-binding char)))
)

(defun undo-with-space-execute-key-binding (ch) "execute command associated with keystroke"
    (interactive "cWhat Key? ")		; only asks question if interactive
    (setq str (char-to-string ch))
    (cond
        ((equal ch 24)			; ^X prefix
	    (progn
	        (message "C-x-")
		(setq str (concat str (char-to-string (read-char))))))
        ((equal ch 3)			; ^C prefix
	    (progn
	        (message "C-c-")
		(setq str (concat str (char-to-string (read-char))))))
        ((equal ch 27)			; ESC prefix
	    (progn
	        (message "ESC-")
		(setq str (concat str (char-to-string (read-char))))))
    )
    (setq cmd (key-binding str))
    ;(print (symbol-name cmd))          ; for debugging
    (if (equal (symbol-name cmd) "self-insert-command")
        (insert-char ch 1)		; doesn't work right otherwise
        (command-execute (symbol-function cmd) t) ; put cmd in command-history
    )
)
