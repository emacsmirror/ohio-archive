;To: unix-emacs@bbn.com
;Date: 13 Feb 89 18:57:09 GMT
;From: Charles Buckley <mcvax!cernvax!ethz!ceb@uunet.uu.net>
;Subject: Re: ARGH!  Ahem.  Umm, answering prompts & abbreviating
;
;
;I have seen a lot of postings asking about this, so here's my hack.
;This presumes you have some terminal with so-called function keys
;which transmit some otherwise absolutely useless escape sequence which
;you can decode, like esc [ M,or something.  You define the following
;functions, and then put in commands which bind these codes to the
;sequences transmitted by your function keys.  
;
;On terminals without such keys, you can bind it to simple control
;keys, to the extend that you don't walk on something already bound
;there.  

;;
;;     yes-or-no-keys.el
;;
;;     C. Buckley
;;
;;     5 August 1988
;;
;;     We have gotten absolutely used to answering these yes-or-no
;;     questions by hitting special keys marked "yes" or "no".  This
;;     was formerly arranged by loading the terminal with pre-defined 
;;     strings.  Under mux this is not possible.
;;     Here we write code to do the same thing.
;;

(defun answer-yes-in-minibuffer ()
  "answers yes in minibuffer and terminates minibuffer input."
  (interactive)
  (insert "yes")
  (setq unread-command-char 10)   ;a newline
  )

(defun answer-no-in-minibuffer ()
  "answers no in minibuffer and terminates minibuffer input."
  (interactive)
  (insert "no")
  (setq unread-command-char 10)  ;a newline
  )

