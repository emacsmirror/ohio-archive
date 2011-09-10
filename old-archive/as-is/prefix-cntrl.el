;;;Date: 10 Feb 88 14:32:28 GMT
;;;From: Neal Ziring <uflorida!codas!mtune!whuts!homxb!genesis!hotlg!nz@GATECH.EDU>
;;;Organization: AT&T-BL Dept. 54245
;;;Subject: Re: prefix-character for gnuemacs

;;;In article <8802062136.AA08932@skakke> obh@IFI.UIO.NO writes:
;;; > 	A student here at the University of Oslo  uses Gnuemacs.  He has
;;; > 	a special problem: He can only use one of his fingers; thus he's
;;; > 	unable to type control-characters.
;;; > 
;;; > 	He  wants a  character  he can  type   to  `prefix`  the  next
;;; > 	character as a control-character. (He uses a vt100 and a SUN).
;;; > 
;;; > Internet: obh@ifi.uio.no				(OBH)

;;;I happened to have some code around that I could fix up to be a quick
;;;solution to this problem.  I hope your student finds it helpful -- it
;;;works fine for me on my terminal (a VT-100 clone).  The key-bindings at
;;;the end are there only to serve as an example, you and your friend may
;;;want to change them.  NOTE: I wrote the original version of this code
;;;while at college, so AT&T has no rights to it.

;;;------------------------- cut here ------------------------
(defun symbol-ok (s)
  "if arbitrary sexp S is a symbol, return it, otherwise return nil."
  (if (symbolp s) s nil))

(defun C-M-command (char)
 "execute control-meta command without having to hold down 3 keys,
	as in old Twenex emacs.
	take argument CHAR, controlify it, and execute it
	from esc-map"
    (interactive "cC-M-")
    (if (< ?! char)   (setq char (- (upcase char) 64)))
    (let ((def (or
		(and (current-local-map)
		     (symbol-ok (lookup-key (current-local-map) 
				 (concat "" (char-to-string char)))))
		(symbol-ok (lookup-key esc-map (char-to-string char))))))
      (if def
        (command-execute def)
	(progn (fset def (concat "" (char-to-string char)))
	       (execute-kbd-macro def))
      )
))


(defun C-command (char)
  "execute control key command without having to hold down 2 keys.
	take argument CHAR, controlify it, and execute it
	from local and global maps"
  (interactive "cC-")
  (if (= ?x char)
      (call-interactively 'C-X-command)
    (if (< ?! char)   (setq char (- (upcase char) 64)))
    (let ((def (or
		(and (current-local-map)
		     (symbol-ok (lookup-key (current-local-map) 
				 (char-to-string char)))))
		(symbol-ok (lookup-key global-map (char-to-string char)))))
      (if def
	  (call-interactively def)
	(progn (fset def (char-to-string char))
	       (execute-kbd-macro def))
	)
)))



(defun C-X-command (char)
 "execute control x + key command without having to hold down 2 keys.
	take argument CHAR, add ctl-x, and execute it
	from ctl-x-map.  Note, this will not allow things like C-X C-X,
        unfortunately.  It does work for C-X o and like commands."
    (interactive "cC-X-")
    (let ((def (or
		(and (current-local-map)
		     (symbol-ok (lookup-key (current-local-map) 
				 (concat "" (char-to-string char)))))
		(symbol-ok (lookup-key ctl-x-map (char-to-string char))))))
      (if def
        (call-interactively def)
	(progn (fset def (concat "" (char-to-string char)))
	       (execute-kbd-macro def))
      )
))


(define-key global-map "\\" 'C-command)
(define-key global-map "`" 'C-M-command)

;;;----------------------------------- cut here --------------------

;;;I know this isn't particularly elegant, but I am not sure that the elegant
;;;approaches mentioned in the original posting can be done without hacking
;;;some of the original Gnumacs source.  BTW, the code above was written and
;;;works for 18.35.  It should have no trouble under and 18.xxx.

;;;-- 
;;;...nz  (Neal Ziring  @  ATT-BL Holmdel, x2354, 3G-309)
;;;	"You can fit an infinite number of wires into this junction box,
;;;	but we usually don't go that far in practice."
;;;					London Electric Co. Worker, 1880s
