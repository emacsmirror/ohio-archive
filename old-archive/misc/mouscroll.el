;From ark1!uakari.primate.wisc.edu!aplcen!uunet!brunix!tac Sat Mar  3 17:13:27 EST 1990
;Article 1510 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!aplcen!uunet!brunix!tac
;From: tac@cs.brown.edu (Theodore A. Camus)
;Newsgroups: comp.emacs
;Subject: mouse scrolling
;Message-ID: <30868@brunix.UUCP>
;Date: 27 Feb 90 23:09:59 GMT
;Sender: news@brunix.UUCP
;Reply-To: tac@cs.brown.edu (Theodore A. Camus)
;Organization: Brown University Department of Computer Science
;Lines: 50
;
;
;Here's a fast way to scroll in a large file.
;Just position mouse in the window you wish to scroll,
;and shift right click.  The file in that window will
;scroll to a position in the file that is the same 
;percentage as the mouse's position horizontally in the
;window in question.
;
;Btw, you may not be able to put it in your .emacs.
;Just put the define-key's or global-set-mouse's in some
;other function and call it at the eval prompt after you
;start up emacs.
;
;X version:
;==========

(define-key mouse-map x-button-s-right 'x-col-jump)

(defun x-col-jump (arg)
  "Will move cursor to pos in buffer as a mouse % of window width.
   Will work for any window mouse is in, not just current-buffer."
  (let ((OriginallySelectedWindow (selected-window))
        (line ())
        (old-perc ())) 
    (unwind-protect 
      (progn (x-mouse-set-point arg)
        (let* ((wd  (1- (window-width)))
               (size (buffer-size))
               (perc (perc (- (car arg) (car (window-edges))) wd))
               (new-pos (perc-of perc size)))
           (goto-char new-pos)
           (setq old-perc perc line (what-line))))
      (select-window OriginallySelectedWindow))
      (message " %s = %s%s" line old-perc '%)))

;Suntool version:
;================

(global-set-mouse '(text shift right)  'mouse-fast-col-jump)

(defun mouse-fast-col-jump (w x y)
  (if (not (eq last-command 'mouse-fast-col-jump))
      (setq *column* (current-column)))
  (mouse-scroll-proportional w x y)
  (move-to-column *column*))

;  CSnet:     tac@cs.brown.edu                          Ted Camus  
;  ARPAnet:   tac%cs.brown.edu@relay.cs.net             Box 1910 CS Dept
;  BITnet:    tac@browncs.BITNET                        Brown University
;  "An ounce of example is worth a pound of theory."    Providence, RI 02912


