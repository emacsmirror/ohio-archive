;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!rice!brazos!rich Fri Apr 20 11:02:19 EDT 1990
;Article 1809 of comp.emacs:
;Xref: ark1 comp.emacs:1809 gnu.emacs:1543
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!rice!brazos!rich
;From: rich@Rice.edu (Rich Murphey)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: Re: GNU Emacs X Windows cut
;Message-ID: <RICH.90Apr19231705@kalliope.Rice.edu>
;Date: 20 Apr 90 04:17:05 GMT
;References: <1990Apr19.023817.8046@ctr.columbia.edu>
;	<DAVIDE.90Apr19122828@maverick.cad.mcc.com>
;Sender: root@rice.edu
;Reply-To: Rich@Rice.edu (Rich Murphey)
;Followup-To: comp.emacs
;Organization: Department of Electrical and Computer Engineering, Rice
;	University
;Lines: 60
;In-reply-to: davide@maverick.cad.mcc.com's message of 19 Apr 90 17:28:28 GMT
;
;
;In article <DAVIDE.90Apr19122828@maverick.cad.mcc.com> davide@maverick.cad.mcc.com (David Eckelkamp) writes:
;   The
;   thing that most like is that it makes the emacs mouse work very
;   similar to xterm. Press mouse-left, drag then release mouse-left to
;   mark a region and put the region in the cut-buffer. A mouse middle
;   will paste the cut-buffer *at the Emacs point*, not the mouse cursor
;   point.
;
;
;David points out cutting text in the xterm style -- point and drag --
;is useful.  Here's another version of x-cut-text-if-moved which, like
;xterm, doesn't move the cursor while you are dragging text.  One
;obvious use is in a shell buffer, where you generally want leave the
;cursor on the command line and paste text onto it.  Rich@Rice.edu


;; Cut and paste text using the mouse without distubing the cursor.
;; Based on x-mouse.el, part of GNU Emacs.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

(defun x-mouse-get-point-and-window (arg)
  "Get the point and window at the current mouse position."
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (list (point) (selected-window)))))

(defvar button-down-point-and-window nil
  "A list containing the point (buffer position) and window where the
button down was saved.  See x-set-button-down-point-and-window.")

(defun x-set-button-down-point-and-window (arg)
  "Save the location of mouse the variable `button-down-point-and-window'.
The location is a list containing the point (buffer position) and window."
  (setq button-down-point-and-window (x-mouse-get-point-and-window arg)))

(defun x-cut-text-if-moved (arg)
  "If the mouse has moved between button down and button up, copy text
between button down and button up points into the window system cut
buffer.  Save the text in Emacs kill ring also."
  (let* ((button-up-point-and-window (x-mouse-get-point-and-window arg))
	(beg (car button-down-point-and-window))
	(end (car button-up-point-and-window))
	(down-win (car (cdr button-down-point-and-window)))
	(up-win (car (cdr button-up-point-and-window))))
    (if (and (not (equal beg end)))
	(if (equal down-win up-win)
	    (progn
	      (save-window-excursion
		(save-excursion
		  (select-window down-win)
		  (x-store-cut-buffer (buffer-substring beg end))
		  (copy-region-as-kill beg end))))
	  (message "You cannot drag across window boundaries.")))))

(define-key mouse-map x-button-left-up 'x-cut-text-if-moved)
(define-key mouse-map x-button-left 'x-set-button-down-point-and-window)
;--
;Rich@Rice.edu
