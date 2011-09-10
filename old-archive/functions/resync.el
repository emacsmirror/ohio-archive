;From UNAUTHENTICATED Mon Jun 11 11:49:49 EDT 1990
;Article 2929 of gnu.emacs:
;Path: utkcs2!emory!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!pt.cs.cmu.edu!andrew.cmu.edu!<UNAUTHENTICATED>+
;>From: shivers@cs.cmu.edu (Olin Shivers)
;Newsgroups: gnu.emacs
;Subject: Re: Emacsdisplay, as opposed to emacsclient
;Message-ID: <UaPnlYC00htF0wn14m@cs.cmu.edu>
;Date: 8 Jun 90 05:49:24 GMT
;Organization: Carnegie Mellon, Pittsburgh, PA
;Lines: 34
;
;fred@cadence.com (Fred Cox) writes:
;> I work all day in epoch or emacs, then I go home without exiting.
;> 
;> Now I log in from home, and want to change a file that is currently
;> being changed in my emacs at work.  Bummer!
;> 
;> What do you people think of the idea of having a program called
;> something like emacsdisplay that connects to a running emacs or epoch,
;> and lets you edit all your buffers as though it were a new window?
;
;I use instead the following low-tech hack:
(defun resync-files ()
  "Use if you've been editing files elsewhere."
  (interactive)
  (let ((bufs (buffer-list)))
    (while bufs
      (let* ((buf (car bufs))
	     (bname (buffer-name buf))
	     (fname (buffer-file-name buf)))
	(setq bufs (cdr bufs))
	(cond ((and fname
		    (not (verify-visited-file-modtime buf))
		    (y-or-n-p (format "Resync %s: %s " bname fname))
		    (or (not (buffer-modified-p buf))
			(y-or-n-p (format "Buffer %s modified! Are you sure? "
					  bname))))
	       (set-buffer buf)
	       (revert-buffer nil t)))))) ; nil=ask if autosave is newer.
  (message "Resync done."))		  ; t=no confirmation otherwise.

;When I leave work, I say c-x s to save all my buffers.  Then I can go home
;and hack away. When I come back to work, I just say M-x resync-files and
;refresh any stale buffers.
;	-Olin


