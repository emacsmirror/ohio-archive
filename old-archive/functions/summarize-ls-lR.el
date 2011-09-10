; Newsgroups: gnu.emacs.sources
; Path: hal.com!decwrl!spool.mu.edu!howland.reston.ans.net!sol.ctr.columbia.edu!sol.ctr.columbia.edu!kfogel
; From: kfogel@occs.cs.oberlin.edu (Karl Fogel)
; Subject: summarize-ls-lR.el, for sorting through large ls-lR's
; Organization: Oberlin College Computer Science
; Date: Sun, 24 Jan 1993 00:19:38 GMT
; 
; 
; 	Here is the least useful elisp program I know of, written to
; prove that no task, no matter how trivial, is too minor to warrant a
; special elisp function. I needed it to sort through a large (10000+
; line) ls-lR file of my hard drive. I wanted a way to sort out all
; those things that were in directories named "bin" or subdirs thereof
; (say, /usr/bin/X11 or something) and list just them, and then do dirs
; named "etc" and subdirs, then "lib", and so on, so that I could have a
; more or less complete listing of all the executable binaries on my
; system, and all the libs, etc. But I didn't want to do it by hand, no
; sir, I wanted the machine to do it for me. So here it is, and if
; anyone out there actually uses it, I will be shocked. But, SOMEDAY,
; you will be sorting through an ls-lR file, and then you'll be glad you
; downloaded this. Enjoy!

;; LCD Archive Entry:
;; summarize-ls-lR|Karl Fogel|kfogel@occs.cs.oberlin.edu|
;; Prune useful information for an ls -lR listing.|
;; 1993-01-24||~/functions/summarize-ls-lR.el.Z|

(defun summarize-ls-lR (lsr-search-pat lsr-summary-buffer)
  "Copies specified directories from an ls-lR file and pastes them 
into specified buffer. Directories may be specified by any criterion, 
but \"foo:^J\" is recommended, since ls-lR format uses that for the 
basename of directories. To get dirs named \"foo\" and subdirs
thereof, search for regexp \"foo\\(:\\|/.*:\\)\"."
  (interactive "sEnter summary criterion: \nsEnter buffer to paste to: ")
  (save-excursion
    (re-search-forward lsr-search-pat)
    (beginning-of-line)
    (let ((lsr-pos1 (point)))
      (search-forward "\C-j\C-j")
      (let ((lsr-pos2 (point)))
	(let ((lsr-string (buffer-substring lsr-pos1 lsr-pos2))
	      (lsr-orig-buffer (current-buffer)))
	  (set-buffer (get-buffer-create lsr-summary-buffer))
	  (insert lsr-string)
	  (set-buffer lsr-orig-buffer))))
    (summarize-ls-lR lsr-search-pat lsr-summary-buffer)))

