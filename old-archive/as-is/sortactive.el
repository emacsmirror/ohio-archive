;To: unix-emacs@BBN.COM
;Date: 6 Jun 89 21:28:31 GMT
;From: David C Lawrence <tut.cis.ohio-state.edu!rpi!rpi.edu!tale@ucbvax.berkeley.edu>
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: A little function for USENET admins
;Reply-To: tale@pawl.rpi.edu
;Source-Info:  From (or Sender) name not authenticated.
;
;Here's a short Emacs lisp function which I use to sort the active file
;when a new groups come along.  I rather dislike doing it by hand when
;newgroups come in  and this makes it easy to change just one or two
;things in the function to make the order whatever I want.  The
;advantages are that new users can get a pre-sorted, somewhat coherent
;.newsrc that puts things the way you might want them (it was easier
;than editing newsetup -- I modified newsetup to just sed the active
;file into a newsrc without any of the /tmp files) and things are much
;more organized when I go to look at the active file.
;
;This uses GNU Emacs to do the brunt of the work.  I call it from the
;following three line script run from /usenet/lib:
;--
;lockf active 'emacs -batch -q -l ./sort-active.el -kill'
;diff -c active~ active | more
;rm -i active~
;--
;The lockf is a local programme here that just locks a file while I run
;some command.  Though the whole thing will run in under 30 seconds on
;a Sun 3 for even the most extensive active files, better safe than
;sorry.   I check it with diff to see what happened with the file and
;if everything is okay I just trash the original.  I hope someone finds
;it useful; the only changes to this that should be necessary are path
;names for the active file, the removal or substitution of a file
;locking mechanism and the changing of newsgroups for the "order"
;variable.   (Ie, make the local groups correct and add whatever
;alternative hierarchies you have.)
;
;---/usenet/lib/sort-active.el---
(progn
  (find-file "/usenet/lib/active")
  (let (groups-yanked lines beg end (in-line 1)
	(order '("news.announce.newusers" "news.announce.important" "rpi" "capdist" "ny" "news" "comp" "gnu" "sci" "rec" "soc" "talk" "misc" "alt" "arpa" "mail" "unix-pc" "ddn" "general" "control" "pmdf" "junk" "to")))
    (while order
      (message "Putting \"%s\" in place..." (car order))
      (setq groups-yanked "" lines 0)
      (goto-line in-line)
      (while (re-search-forward (concat "^" (regexp-quote (car order))) nil t)
	(beginning-of-line) (setq beg (point))
	(next-line 1) (setq end (point))
	(setq groups-yanked (concat groups-yanked (buffer-substring beg end))
	      lines (1+ lines))
	(delete-region beg end))
      (goto-line in-line) (setq beg (point))
      (insert groups-yanked)
      (sort-lines nil beg (point))
      (setq in-line (+ in-line lines) order (cdr order))))
  (goto-char (point-min))
  (save-buffer)
  (message "Done; check active against active~."))
;---end----
;As an aside, how many elisp hackers have wanted to do
;(setq beg (beginning-of-line)) to get the same results as
;(progn (beginning-of-line) (setq beg (point)))?
;
;I find similar examples in lots of elisp code and it just seems so
;distinctly un-lispy to have a function like (end-of-line) which never
;returns anything besides nil and has only a side-effect.  It seems
;that most movement commands which don't already have meaningful return
;values would be improved slightly if they returned point instead.
;
;Dave
;--
; (setq mail '("tale@pawl.rpi.edu" "tale@itsgw.rpi.edu" "tale@rpitsmts.bitnet"))
