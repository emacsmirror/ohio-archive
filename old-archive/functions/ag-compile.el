;Path: ark1!nap1!ames!uakari.primate.wisc.edu!csd4.csd.uwm.edu!mailrus!ncar!asuvax!mcdphx!udc!chant!aglew
;From: aglew@urbana.mcd.mot.com (Andy-Krazy-Glew)
;Newsgroups: comp.emacs
;Subject: Re: dired sorting by date
;Message-ID: <AGLEW.89Aug30191000@chant.urbana.mcd.mot.com>
;Date: 30 Aug 89 23:10:00 GMT
;References: <44719@bbn.COM> <572C02cp4emH01@amdahl.uts.amdahl.com> <10204@csli.Stanford.EDU> <KIM.89Aug29080137@kannel.lut.fi>
;Sender: aglew@urbana.mcd.mot.com
;Organization: Work: Motorola MCD, Urbana Design Center; School: University of Illinois at Urbana-Champaign
;Lines: 85
;In-reply-to: kim@kannel.lut.fi's message of 29 Aug 89 12:01:37 GMT
;
;   From: ceb@csli.Stanford.EDU (Charles Buckley)
;   Newsgroups: comp.emacs
;   Date: 28 Aug 89 09:38:50 GMT
;   Organization: Center for the Study of Language and Information, Stanford U.
;
;   In the spirit of the recently posted dired-by-date patch, I seem to
;   remember that it used to be that when you did M-x grep, and got your
;   results in the *compilation* buffer, that hitting 'e' or somehing on a
;   partiucular line would cause that file to be edited, and move the
;   cursor to the line in question (One could even do this by character -
;   enough information is present).  
;
;   Does someone still have the code which does this, or could perhaps
;   some FSF member dig it out of the code locker?
;
;This isn't the old code (I wasn't aware of the old code) but it does
;part of the trick:
;
;;; Added functions to the compile package


(defun ag-reparse-compilation-errors ()
  "Move to a position in the compilation window,
skipping stuff you don't want,
and recreate list.
INTERIM: maybe long delay since whole list is redone.
but so far I have found it tolerable"
  (interactive)
  (compilation-forget-errors)
  (save-excursion
    (set-buffer "*compilation*")
    (setq compilation-parsing-end (point)))
  (next-error))

(make-global-binding "\^x\\" 'ag-reparse-compilation-errors)


;As I mention inline, it can be slow since it reparses the *compilation*
;buffer -- this was right for me, since I occasionally edit the compilation
;buffer, but it isn't always the best. Or, you could move down the already
;parsed list of marks until you found the one corresponding to your
;position in the *compilation* buffer.
;
;--
;Andy "Krazy" Glew,  Motorola MCD,    	    	    aglew@urbana.mcd.mot.com
;1101 E. University, Urbana, IL 61801, USA.          {uunet!,}uiucuxc!udc!aglew
;   
;My opinions are my own; I indicate my company only so that the reader
;may account for any possible bias I may have towards our products.
