;From utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ri.osf.fr!macrakis Wed Jun 20 09:30:18 EDT 1990
;Article 2111 of gnu.emacs.bug:
;Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ri.osf.fr!macrakis
;>From: macrakis@ri.osf.fr
;Newsgroups: gnu.emacs.bug
;Subject: Window resizing
;Message-ID: <9006201248.AA22931@.dupuis.gr.osf.org.>
;Date: 20 Jun 90 12:49:06 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 42
;X-Unparsable-Date: Wed, 20 Jun 90 13:53:11 GMT+0100
;
;GNU Emacs 18.53.11 on NeXT 1.0.1
;
;Emacs pretends that window size is just a question of proportions.
;Thus, when a window goes away, its lines get allocated among other
;windows.  Or if you resize a window, windows below it get smaller.
;
;But in practice, some windows logically have a fixed size.  This is
;the case of windows that are "just the right size" for a piece of
;information.  (See below for a little command that shrink-wraps a
;region in a window.)  However, Emacs doesn't understand this.  In
;particular, if you try to shrink-wrap two windows, the shrinking of
;the second is likely to change the size of the first, which rather
;defeats the point....
;
;I have no specific suggestions -- and perhaps the problem will go away
;at the millenium (v19) -- but I thought I'd document the issue.
;
;	-s


;; Bound to ^X6 (mnemonic is that ^X^ is shrink-window)

(defun shrink-window-to-region (beg end)
  "Makes window just small or large enough to hold region.\n\
If only one window on screen, does nothing."
   (interactive "r")
  (if (eq (selected-window) (next-window (selected-window) 0))
      ;;Is there a better way to test whether we have only 1 window?
      nil
    (save-excursion
      ;; Make sure full lines are included on-screen
      (goto-char beg)
      (beginning-of-line 1)
      (setq beg (point))
      (goto-char end)
      (beginning-of-line 2)
      (setq end (point))
      (let ((window-min-height 2))	;Sometimes 1 line is enough
	(shrink-window (- (window-height (selected-window))
			  (count-lines beg end)
			  1)))		;Window height includes mode line
      (set-window-start (selected-window) beg))))


