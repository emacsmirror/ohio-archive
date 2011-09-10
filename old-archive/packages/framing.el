;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!swrinde!cs.utexas.edu!rice!bbc Mon May 14 23:34:34 EDT 1990
;Article 1638 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!swrinde!cs.utexas.edu!rice!bbc
;>From: bbc@rice.edu (Benjamin Chase)
;Newsgroups: gnu.emacs
;Subject: Re: line-to-top-of-window
;Message-ID: <BBC.90May9223629@sicilia.rice.edu>
;Date: 10 May 90 03:36:28 GMT
;References: <ROBERTS.90May9152931@studguppy.lanl.gov>
;Sender: root@rice.edu
;Reply-To: Benjamin Chase <bbc@rice.edu>
;Distribution: gnu
;Organization: Center for Research on Parallel Computations
;Lines: 64
;In-reply-to: roberts@studguppy.lanl.gov's message of 9 May 90 21:29:31 GMT
;
;roberts@studguppy.lanl.gov (Doug Roberts) writes:
;
;>There used to be a function of this name in release 18.54 & previous.
;>It would move (scroll) the line at the current cursor position to the
;>top of the window.
;>Does anybody know what it's replacement is?
;
;[Studguppy?]
;
;Yeah, here's some stuff I wrote when I first "went GNU".  I called the
;file "framing.el", although it may be more obvious to call it
;"point-to.el" or something like that.  I just wrote point-to-left and
;point-to-right tonight.  You may or may not agree with my choice of
;scroll amounts in those two functions.  Comments welcome...
;
;	Ben

; Copyright (C) 1989 Benjamin B. Chase
; Permission is granted to copy, modify, and redistribute this
; software, but only under the conditions described in the
; GNU General Public License, Version 1, as published by the
; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; This software is distributed without any warranty, and without any implied
; warranty of merchantibility or fitness.  See the GNU General Public License
; for more details.

; framing.el
;
; History:
;	9 May 1990 Benjamin B. Chase <bbc@rice.edu>
;		Cleaned up my old code for posting to gnu.emacs, and added
;		point-to-left and point-to-right for completeness.

(provide 'point-to-top)
(provide 'point-to-bottom)

(defun point-to-top ()
  "Recenter the window so that point is on the top line."
  (interactive)
  (recenter 0))

(defun point-to-bottom ()
  "Recenter the window so that point is on the bottom line."
   (interactive)
   (recenter -1))

(defun point-to-left ()
  "Scroll the window so that point is at the left edge of the window."
   (interactive)
   (scroll-left (- (current-column) (window-hscroll))))

(defun point-to-right ()
  "Scroll the window so that point is at the right edge of the window.
If the window cannot be scrolled that far rightwards, scroll the window
as far as possible.
The character under cursor will be on the right edge of the
screen, but obscured by the overstrike character (ie. '$')."
   (interactive)
   (scroll-right (- (window-width)
		    (- (current-column) (window-hscroll))
		    2)))
;--
;	Ben Chase <bbc@rice.edu>, Rice University, Houston, Texas
