;Date:    18 Jan 89 02:19:09 +0000 
;From:    "Eric L. Raible" <csd4.milw.wisc.edu!bionet!agate!eos!amelia!orville.n
;	  as.nasa.gov!raible@bbn.com>
;To:      unix-emacs@bbn.com
;Subject: Re: GNU Emacs POLL: where should killed window space go?
;
;In article <8758@bloom-beacon.MIT.EDU> wolfgang@mgm.mit.edu.UUCP (Wolfgang Rupp
;recht) writes:
;>Usually when I delete 1 of 3 windows, I usually end up resizing the 2
;>resulting windows to be equal size again.  I therefor would vote for
;>\C-x 0 to resize all remaining windows to be *equally* sized.
;>
;
;I got an initial copy of this code from someone else (but can't remember who).
;[Ashwin Ram is the original author. -des]
;It doesn't work too well with horizontally-split windows, but is quite useful
;otherwise.
;
(global-set-key "\eB"      'balance-windows)

(defun balance-windows ()
  "Makes all visible windows the same size."
  (interactive)
  (let ((size (/ (screen-height) (count-windows))))
    (walk-windows (function (lambda ()
			      (enlarge-window (- size (window-height)))))
		  'no-mini)))

(defun walk-windows (proc &optional no-mini)
   "Applies PROC to each visible window (after selecting it, for convenience).
Optional arg NO-MINI non-nil means don't apply PROC to the minibuffer
even if it is active."
   (let* ((start (selected-window))
	  (current start)
	  (done nil))
     (while (not done)
       (select-window current)
       (funcall proc)
       (setq current (next-window current no-mini))
       (setq done (eq current start)))
     (select-window start)))

(defun count-windows (&optional no-mini)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda ()
			       (setq count (+ count 1))))
		   no-mini)
     count))
