;From ark1!uakari.primate.wisc.edu!uflorida!mephisto!prism!hydra.gatech.edu!ashwin Wed Dec  6 08:01:23 1989
;Article 990 of comp.emacs:
;Xref: ark1 comp.emacs:990 gnu.emacs:821
;Path: ark1!uakari.primate.wisc.edu!uflorida!mephisto!prism!hydra.gatech.edu!ashwin
;>From ashwin@gatech.edu (Ashwin Ram)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: Re: Filling of paragraphs in GNU emacs.
;Message-ID: <ASHWIN.89Dec5104526@pravda.gatech.edu>
;Date: 5 Dec 89 15:45:26 GMT
;References: <5074@nigel.udel.EDU> <49028@bbn.COM>
;Sender: news@prism.gatech.EDU
;Reply-To: ashwin@pravda.gatech.edu (Ashwin Ram)
;Followup-To: comp.emacs
;Organization: Georgia Tech, School of ICS
;Lines: 51
;
;In article <49028@bbn.COM> John Robinson writes:
;>   Ashwin Ram has a much improved version of justify-current-line, which
;>   is due to get into the distribution some day.  He sent it to me but I
;>   didn't keep a copy.  Perhaps he will post it here.
;
;I've gotten a couple of requests for this, so here goes:
;------------------------------------------------------------------------------
; Rewrote to spread blanks evenly across inter-word spaces in line.  Randomly
; decide whether to put extra spaces from the right or the left, to avoid
; "rivers" as far as possible.  Don't modify original interword spaces.
; Ashwin Ram, 11/16/87.
(defun justify-current-line ()
   "Add spaces to line point is in, so it ends at fill-column."
   (interactive)
   (save-excursion
      (save-restriction
         (beginning-of-line)
         (skip-chars-forward " \t")
         (let ((begin (point)))
            (end-of-line)
            (narrow-to-region begin (point))
            (let ((blanks (- fill-column (current-column))))
               (if (<= blanks 0)
                   nil
                   (goto-char begin)
                   (let ((holes 0))
                      (while (re-search-forward "[ \t][^ \t]" nil t)
                         (setq holes (+ holes 1)))
                      (let ((each (/ blanks holes))
                            (extra (% blanks holes))
                            (dir (> (random) 0))
                            (count 1))
                         (end-of-line)
                         (while (<= count holes)
                            (re-search-backward "[ \t][^ \t]")
                            (let ((n 1))
                               (while (<= n each)
                                  (insert ? )
                                  (setq n (+ n 1))))
                            (if dir
                                (if (<= count extra) (insert ? ))
                                (if (>= count (- holes (- extra 1))) (insert ? )))
                            (setq count (+ count 1)))))))))))
;------------------------------------------------------------------------------
;
;-- Ashwin (ashwin@gatech.edu)
;--
;Ashwin Ram
;Georgia Institute of Technology, Atlanta, Georgia 30332-0280
;UUCP:	  ...!{decvax,hplabs,ncar,purdue,rutgers}!gatech!prism!ar17
;Internet: ashwin@gatech.edu, ashwin@pravda.gatech.edu, ar17@prism.gatech.edu


