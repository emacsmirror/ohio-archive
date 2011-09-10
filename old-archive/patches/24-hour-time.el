; Path: hal.com!olivea!charnel!rat!usc!zaphod.mps.ohio-state.edu!sol.ctr.columbia.edu!ira.uka.de!math.fu-berlin.de!unidui!Germany.EU.net!mcsun!sun4nl!mhres!pronto!news
; From: jv@mh.nl (Johan Vromans)
; Newsgroups: gnu.emacs.sources,gnu.emacs.help
; Subject: 24-hour time display
; Date: 18 Nov 92 16:23:44 GMT
; Organization: Multihouse Automation, the Netherlands
; 
; The following patch adds the variable display-time-ampm to time.el.
; Setting this variable to nil enables 24-hour time display.
; 
; Enjoy!

; LCD Archive Entry:
; 24-hour-time|Johan Vromans|jv@mh.nl|
; Adds 24 time display capability to time.el.|
; 92-11-18||~/patches/24-hour-time.el.Z|

; 
; *** emacs-18.59/lisp/time.el	Tue Nov  3 10:04:55 1992
; --- /usr/tmp/time.el	Wed Nov 18 17:08:29 1992
; ***************
; *** 29,34 ****
; --- 29,37 ----
;   
  (defvar display-time-string nil)
  
+ (defvar display-time-ampm t
+   "*If t, display time in AM/PM format. Otherwise use 24-hour format.")
+ 
  (defun display-time ()
    "Display current time and load level in mode line of each buffer.
  Updates automatically every minute.
***************
*** 94,107 ****
  					 (user-login-name)))))
  	hour pm)
      (setq hour (read (substring time 11 13)))
!     (setq pm (>= hour 12))
!     (if (> hour 12)
! 	(setq hour (- hour 12))
!       (if (= hour 0)
! 	  (setq hour 12)))
      (setq display-time-string
  	  (concat (format "%d" hour) (substring time 13 16)
! 		  (if pm "pm" "am")
  		  (if (string= load "")
  		      ""
  		    (concat " " (substring load 0 -2) "." (substring load -2)))
--- 97,112 ----
  					 (user-login-name)))))
  	hour pm)
      (setq hour (read (substring time 11 13)))
!     (if display-time-ampm
! 	(progn
! 	  (setq pm (>= hour 12))
! 	  (if (> hour 12)
! 	      (setq hour (- hour 12))
! 	    (if (= hour 0)
! 		(setq hour 12)))))
      (setq display-time-string
  	  (concat (format "%d" hour) (substring time 13 16)
! 		  (if display-time-ampm (if pm "pm" "am") "")
  		  (if (string= load "")
  		      ""
  		    (concat " " (substring load 0 -2) "." (substring load -2)))
