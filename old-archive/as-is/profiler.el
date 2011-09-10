;Date: Thu, 11 May 89 13:47:30 EDT
;From: Root Boy Jim <rbj@dsys.icst.nbs.gov>
;Organization: National Institute of Standards and Technology
;	formerly National Bureau of Standards
;Disclaimer: Opinions expressed are those of the sender
;	and do not reflect NIST policy or agreement.
;To: merlyn@intelob.intel.com
;Cc: unix-emacs@bbn.com
;Subject: updated tools
;
;? Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;
;? rbj@dsys (Root Boy Jim) writes:
;? | [a bunch of stuff about how to do it faster and better]
;
;? After I posted the crude code, I did a huge number of iterative
;? optimizations (as thought experiments, since I don't have a good way
;? of profiling Elisp code).  The result is attached after the signature.
;? No new functionality, unless you think speed is a function :-).
;
;Hmmm, that got me to thinking, so I wrote a (crude profiler). It seems
;that emacs only provides (current-time-string), which returns ascii.
;Some related function should return an integer of sec/msec/usec.
;
;My only observation now is that the (downcase str) seems to be unneeded
;as the regular expression matching doesn't care about case. If you
;really want to be safe, wrap it in (let ((case-fold-search t)) ... ).

(defun time-of-day ()
  "Return seconds since midnight. Emacs needs something like this."
  (let ((time (current-time-string)))
    (+ 
     (* 36000 (logand 15 (aref time 11))) ;tens hours
     (* 3600  (logand 15 (aref time 12))) ;unit hours
     (* 600   (logand 15 (aref time 14))) ;tens minutes
     (* 60    (logand 15 (aref time 15))) ;unit minutes
     (* 10    (logand 15 (aref time 17))) ;tens seconds
     (logand 15 (aref time 18)))))      ;  unit seconds

(defun prof (count exp)
  "Return the number of seconds required to execute COUNT
iterations of EXP. Don't use this around midnight!"
  (let ((beg (time-of-day)))
    (while (> count 0)
      (eval exp)
      (setq count (1- count)))
    (- (time-of-day) beg)))

(defun profile (count exp)
  "Interactive version of prof. Do COUNT iterations of EXP."
  (interactive "p\nxProfile Expression: ")
  (message "%d iterations of %s took %s seconds."
	   count exp (prof count exp)))

;	Root Boy Jim is what I am
;	Are you what you are or what?

