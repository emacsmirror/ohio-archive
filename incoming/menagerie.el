;; menagerie.el 
;; aka the sonic menagerie
;;
;; (c) 1999 Julian Assange <proff@iq.org>
;;
;; This program keeps a pool of different speech synthesiser
;; (festival) voices and doles them out intelligently given
;; appropriate context of who said what when.
;;
;; Presently it only supports ZenIRC/ZenICB (the IRC/ICB clients
;; for emacs) as an input vector but the code should be easy to adapt
;; to other clients or even other mediums.
;;
;; You will need both zenirc/zenicb and either my NetBSD festival package
;; (which should be quite easy to port to FreeBSD/OpenBSD)
;; from:
;;
;;	 ftp://ftp.netbsd.org/pub/NetBSD/packages/pkgsrc/fest*
;;
;; or the debian/redhat festival packages, which unfortunately
;; contain slightly fewer voices. There are links to them on
;; the festival homepage at CSTR:
;;
;;	 http://www.cstr.ed.ac.uk/projects/festival.html
;;
;; You can also install festival by hand for most unix platforms
;; (at least for NAS audio), although fetching all lexicons
;; and diphone data you want can be *very* time consuming
;;

; zenirc support
(require 'zenirc)
(require 'zenirc-trigger)

; zenicb support
(require 'zenicb)
(require 'zenicb-trigger)

(defvar menagerie-program "festival"
  "Festival program binary")

(defvar menagerie-process nil)

(defvar managerie-intros
  '("says "))

(defvar menagerie-default-regexp ".*"
  "menagerie trigger regexp")

(defvar menagerie-nick-hash (make-hashtable 256 'equal)
  "Hash table of recently seen menagerie nicknames (key),
   and properties (val)")

(defvar menagerie-voiceprints '
  (("karen" . "(voice_tll_diphone)")
   ("kadiya" . "(voice_tll_diphone)(Parameter.set 'Duration_Stretch .8)"))
  "Hand picked list of regex's and voices for your
favourite hacker sluts^W^Wpeople")

;; The entries contained duration_Stretch definitions below
;; are commented out by default. Remove the comments if you
;; would like a greater number of voices -- they're not
;; voices of their own right but can often be different
;; sounding enough to distinguish.
;;
;; I've be *very interested* in seeing any other
;; distinguishable  yet intelligible scheme only
;; voice contortions. Producing random unique voices
;; from a reference diphone set is quite an interesting
;; field of research. What we are doing here with
;; diphone stretching is of course simply a hack.

(defvar menagerie-voice-queue '
  ("(voice_en1_mbrola)"
   "(voice_us1_mbrola)"	  ;; female
   "(voice_us2_mbrola)"
   "(voice_us3_mbrola)"
   "(voice_aec_diphone)"
   "(voice_don_diphone)"
   "(voice_jph_diphone)"
   "(voice_kal_diphone)"
   "(voice_ked_diphone)"
   "(voice_mwm_diphone)"
;   "(voice_mwm_diphone)(Parameter.set 'Duration_Stretch .8)"
   "(voice_ogirab_diphone)"
   "(voice_rab_diphone)"
   "(voice_tll_diphone)")
  "List of festival voices, each represented by a string of
scheme commands used to modify voice properties within
festival. This list is re-ordered, but otherwise unmolested
by `menagerie'")

(defvar menagerie-nick-queue nil
  "List of recently seen menagerie nick names")

(defvar menagerie-nick-queue-size 9
  "Keep this many nick names in the recently seen nick
name queue. The queue is maintained in such a way that
prevents duplicates. When we observe a name not in the
queue we presume that it is time to re-introduce
this nickname / voice combination")

(defvar menagerie-nick-intro-timeout '(0 30)
  "Re-introduce a given nick name if it has been at
least this long since its last introduction. The time
value is in seconds and is stored as a list of two
integers. The first integer is the number of seconds
divided by 65536, and the second the remainder")

(defun menagerie-time< (a b)
  "Compare two times, and return t if the first is earlier than the second."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
	   (< (nth 1 a) (nth 1 b)))))

(defun menagerie-time-diff (a b)
  "Return the difference between two times. This function requires
the second argument to be earlier in time than the first argument."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
	((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
				       (- (+ 65536 (nth 1 a)) (nth 1 b))))
	(t (list (- (nth 0 a) (nth 0 b))
		 (- (nth 1 a) (nth 1 b))))))

(defun menagerie-start-process ()
  "Start festival sub-process, if not already
running"
  (if (and menagerie-process
	   (eq (process-status menagerie-process) 'run))
       t
    (message "Starting new festival process...")
    (sit-for 0)
    (let ((process-connection-type nil))
      (setq menagerie-process
	    (start-process "festival" (get-buffer-create "*festival*")
			   menagerie-program)))))

(defun menagerie-send-string (string)
  "Send an string unmolested to the festival process"
  (menagerie-start-process)
  (process-send-string menagerie-process string))
    
(defun menagerie-queue (queue thing &optional size)
  "add THING to the end of QUEUE, deleting all other
references on QUEUE to THING first. If SIZE is specified,
and the queue length has grown greater than SIZE, trim
QUEUE from the head by one element. The function operates
by side-effects on QUEUE, however because the first element
may change should use the value returned to refer to QUEUE
subsequently"
  (setq queue (delete thing queue))
  (setq queue (nconc queue (list thing)))
  (if (and size (> (length queue) size))
      (setq queue (cdr queue))
    queue))

(defun menagerie-voiceprint (prints nick)
  "Match NICK within PRINTS. PRINTS should be an
assoc style list. The car of each association
contains a regex to match against NICK and the cdr
contains the value returned if the match was successful.
The function returns the first match or nil"
  (and prints
       (if (string-match (caar prints) nick)
	   (cdar prints)
	 (and 
	  (menagerie-voiceprint (cdr prints) nick)))))

(defun menagerie-speak (match nick fullnick)
  "Turn MATCH into voice data, using NICK and FULLNICK for context"
  (let ((now (current-time))
	(nickinfo (gethash nick menagerie-nick-hash))
	(intro)
	(voice))
    (if nickinfo
	(progn
	  (setq voice (plist-get nickinfo 'voice))
	  (and (menagerie-time< menagerie-nick-intro-timeout
			     (menagerie-time-diff
			      now (plist-get nickinfo 'time)))
	       (setq intro t)))
      (setq voice (or (menagerie-voiceprint menagerie-voiceprints fullnick)
		      (car menagerie-voice-queue)))
      (setq nickinfo (plist-put nickinfo 'voice voice))
      (setq intro t))
    (setq nickinfo (plist-put nickinfo 'time now))
    (puthash nick nickinfo menagerie-nick-hash)
    (setq menagerie-voice-queue
	  (menagerie-queue menagerie-voice-queue voice))
    (or (member nick menagerie-nick-queue)
	(setq intro t))
    (setq menagerie-nick-queue
	  (menagerie-queue menagerie-nick-queue
				 nick
				 menagerie-nick-queue-size))
    (menagerie-send-string (concat voice "\n"))
    (and intro
	 (setq match (concat nick
			     (nth (random (length managerie-intros))
				  managerie-intros)
			     match)))
    (menagerie-send-string (concat
				  (format "(SayText %S)\n" match)))))

(defun menagerie-zenirc-trigger (match)
  "Process zenirc-trigger data"
  (and match (> (length match) 1)
    (let*  ((fullnick (aref parsedmsg 1))
	    (nick (zenirc-extract-nick fullnick)))
      (menagerie-speak match nick fullnick))))
    
(zenirc-trigger-register "festival" 'menagerie-zenirc-trigger
			 menagerie-default-regexp t)

(defun menagerie-zenicb-trigger (match)
  "Process zenicb-trigger data"
  (and match (> (length match) 1)
    (let*  ((fullnick (first parsedmsg))
	    (nick fullnick))
      (menagerie-speak match nick fullnick))))

(zenicb-trigger-register "festival" 'menagerie-zenicb-trigger
			 menagerie-default-regexp t)

(provide 'menagerie)

