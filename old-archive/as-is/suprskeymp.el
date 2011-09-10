;To: bug-gnu-emacs@prep.ai.mit.edu
;Subject: a better suppress-keymap
;Date: Mon, 13 Feb 89 02:02:49 -0500
;From: Stephen Gildea <gildea@bbn.com>
;
;The 18.52 suppress-keymap clobbers all printing characters on the
;assumption that they will modify the buffer.  This assumption is not
;always correct.  For example, when working on a certain class of
;poorly-laid-out keyboards, I rebind ` to run a function that simulates
;ESC.  Naturally I don't want Dired, Info, Rmail, etc. to clobber that.
;
;The following version of suppress-keymap is intelligent about what
;keys to clobber.
;
; < Stephen


(defun suppress-keymap (map &optional arg)
  "Make MAP override all character-inserting commands to be undefined.
MAP must be a keymap array, not a sparse keymap.
Normally also makes digits set numeric arg,
but optional second arg NODIGITS non-nil prevents this."
  (let ((i 0))
    (while (<= i 127)
      (if (eql (key-binding (char-to-string i)) 'self-insert-command)
	  (aset map i 'undefined))
      (setq i (1+ i))))
  (or arg
      (let (loop)
	(aset map ?- 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (aset map loop 'digit-argument)
	  (setq loop (1+ loop))))))

