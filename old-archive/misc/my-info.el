;From utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!sdd.hp.com!elroy.jpl.nasa.gov!ames!dftsrv!setzer Tue Jul  3 07:51:11 EDT 1990
;Article 4559 of comp.emacs:
;Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!sdd.hp.com!elroy.jpl.nasa.gov!ames!dftsrv!setzer
;>From: setzer@nails.gsfc.nasa.gov (William Setzer)
;Newsgroups: comp.emacs
;Subject: A tiny info front end
;Message-ID: <SETZER.90Jul2180819@nails.gsfc.nasa.gov>
;Date: 2 Jul 90 22:08:19 GMT
;Sender: news@dftsrv.gsfc.nasa.gov
;Organization: Goddard Space Flight Center
;Lines: 42
;
;I have written a tiny front end to info that allows you to view
;info files that are not in the system path.  I post it for two
;reasons -- first, I hope it will be of use to others; and
;secondly, I would like comments on it, as I believe it needs
;some improvements (most notably, it needs some way to recognize
;the validity of info format files -- try ^U - M-x my-info on
;a file that is not in info format).
;
;8<-8<- Cut 8<-8<-
;  A tiny front end to info.

(defvar original-Info-directory Info-directory
  "Holds the pathname to the system's info tree.")

(defun my-info (&optional arg)
  "Enter Info, the documentation browser.  With positive argument,
prompt for directory containing 'dir' file.  With argument of zero,
use system default info directory.  With negative argument,
prompt for file in info format."
  (interactive "P")
  (and arg (setq arg (prefix-numeric-value arg)))
  (cond
   ((null arg)
    (info))
   ((> arg 0)
    (setq Info-directory (read-file-name "Info directory: "
					 nil default-directory 0))
    (Info-directory))
   ((= arg 0)
    (setq Info-directory original-Info-directory)
    (Info-directory))
   ((< arg 0)
    (Info-find-node (read-file-name "Info filename: "
				    nil default-directory 0) "Top"))
   (t (message "Wow!  Trichotomy doesn't exist!"))))
	 
(global-set-key "\C-hi" 'my-info)

;8<-8<- Cut 8<-8<-
;--
;William Setzer
;setzer@nails.gsfc.nasa.gov


