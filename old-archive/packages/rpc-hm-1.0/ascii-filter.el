;Author: Eyvind Ness (eyvind) 
;Date:   Saturday, March 14 1992 11:04 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/ascii-filter.el

;;;
;;; A set of conversion functions for transferring strings to and from
;;; Lisp-machines and Emacs on Unix boxes. The Lispms uses 8 bits ASCII
;;; characters while Emacs feels more comfortable with 7 bits.
;;;
;;; Note that this conversion can be avioded altogether if the
;;; :ASCII-TRANSLATION option for the server definitions on the Lispms
;;; is set, but then the Lispms wouldn't send correctly between
;;; each other.

(provide 'ascii-filter)


(defun ascii-filter-string-from-lispm (str)
  "Takes an 8bits string (e.g. from a Lispm) and strips off the 8th bit.
If the string contains RET, it is substituted with LFD."

  (let (b7ch)
    (mapconcat (function (lambda (ch) 
			   (setq b7ch (logand ?\177 ch))
			   (char-to-string
			    (if (= b7ch ?\r) ?\n b7ch))))
	       str "")))


(defun ascii-filter-string-for-lispm (str)
  "Takes a 7bits string (e.g. from Emacs) and converts it to a Lispm
string. LFD is replaced with M-RET, TAB with SPC."

  (mapconcat (function (lambda (ch)
			  (char-to-string
			   (cond ((= ch ?\n)
				  (logior ?\200 ?\r))
				 ((= ch ?\t) ?\  )
				 (t ch)))))
	      str ""))


(defun ascii-filter (str direction &optional hosttype)
  "Converts STR between different Ascii representations. Keywords are:
DIRECTION, which is either :input or :output, and (optional) HOSTTYPE,
with :lispm as the only significant value."

  (cond ((eq hosttype ':lispm)
	 (cond ((eq direction ':output)
		(ascii-filter-string-for-lispm str))
	       ((eq direction ':input)
		(ascii-filter-string-from-lispm str))
	       (t (error "Illegal DIRECTION specification. %s"
			 "Possible values are :input and :output"))))
	(t str)))
