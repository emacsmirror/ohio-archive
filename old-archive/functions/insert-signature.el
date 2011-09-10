; Path: utkcs2!emory!swrinde!ucsd!ucbvax!CHAOS.CS.BRANDEIS.EDU!morgan
; >From: morgan@CHAOS.CS.BRANDEIS.EDU ("Dylan Kaufman")
; Newsgroups: comp.emacs
; Subject: appending .signature to outgoing mail in RMAIL [Shameful C-c C-w]
; Date: 15 Aug 90 01:28:38 GMT
; References: <1990Aug14.135931.8296@cbnewse.att.com>
; 
; -> Date: 14 Aug 90 13:59:31 GMT
; -> From: att!cbnewse!danj1@cis.ohio-state.edu
; -> 
; -> In gnu.emacs, kondaman@aludra.usc.edu (Arjun Krishna Kondamani) writes:
; -> >I am thinking of the easiest way that I can append my .signature file
; -> >to any mail I send. Any tips/pointers will be greatly appreciated.
; -> 
; -> [I think this is just a mail-mode thing, not an RMAIL thing, I don't
; -> know, I'm a VM kind of guy.]
; -> 
; -> But: Isn't is a shame that C-c C-w insists on sticking your .signature at
; -> the end of the mail message, no matter what argument you give it, etc.
; -> 
; -> About 37% of the time I want to stick it, say, above a chunk of
; -> forwarded material, but, no, no go... I have to type C-x i ~/.signature .
; -> 
; -> [version 18.55]
; ->
; 
; Here is something which I worked up which will put the .signature file
; into the message you are trying to send.  However, it does a little
; bit of processing first.  
; 
; My idea was that sometimes I put a P.S. or an enclosure at the end of
; the message.  As with a 'normal' handwritten letter I would send to a
; friend, I would want the P.S. or enclosure to follow my signature.
; 
; (As I describe what this does, I am not entirely sure of the order)
; First it will try to determine whether it is being sent to 'friends'
; or not.  The definition of a 'friend' is someone who is either on your
; local system or aliases (ie. with no '@' in the address).  All I have
; done to make this determination is to search in the header for '@'.
; Needless to say, that is not terribly reliable, but I didn't know
; anything about regexps then and know very little now.. someone want to
; make it more reliable?  The purpose of this definition is that it uses
; .signature-local for 'friends' and .signature for non-'friends'
; (enemies? ;> ).
; 
; Once it knows what signature to use, the procedure decides WHERE to
; put the signature.  The first thing it does is search for P.S. at the
; beginning of a line.  If it finds it, it will put the signature there.
; If it does not, it searches for E.F. (enclosure follows) at the
; beginning of a line.  
; 
; Having decided WHERE to put the signature, it goes through and changes
; the first occurence of \^JE.F. with a header which says something to
; the effect of Enclosure Follows.
; 
; This is a very incomplete set of functions.  
; 
; 	First of all, it ought to do some kind of REAL searching to
; determine whether the message is being sent to any non-'friend's.
; 
; 	Second, I want to have a marker which doesn't put in a
; separator like E.F.  This would be for cases in which you only wanted
; to put the sig in at a specific place.
; 
; 	Third, I want to have it search more than once for E.F. to
; have more than one 'enclosure'.  Next, I want to set it up so that if
; I put a filename or some sort of description on the line with the
; ---------------------------An enclosure follows---------------------------
; 
; I have the function send-out-mail bound to C-c C-e.
; 
; 	Anyone else have any suggestions?
; 
; 
; 
; 			     -<>Dylan<>-
;  
; 		     morgan@chaos.cs.brandeis.edu
; 
; P.S.  It searches for the first E.F. _after_ a P.S., so the signature
; will always be before the P.S., not the E.F., if it exists.
; 
; P.P.S.  It ignores postscripts after the first. ;)
; 
; P.P.P.S.  If it finds neither \^JP.S. nor \^JE.F., it will put the
; signature at the end... (thus the idea listed above)
; 
; P.P.P.P.S. Yes, there will be an end! ;)  The function allows three
; lines of separator for the E.F. as you will notice below.
; 
; P.P.P.P.P.S. There is a variable called to-cc-separator which I put
; (when I remember) after the To: and (if it's there) CC: files which
; marks the end of the area to search.  This will make the '@' search
; faintly more reliable.
; 
; E.F.
;

;;; Mail sending functions

(defun send-out-mail ()
  "This function places the signature file into a mail file before sending it out."
  (interactive)
  (beginning-of-buffer)
  (if (search-forward "\^JP.S." (buffer-size) t)
      (set-sig-mark)
      (if (search-forward "\^JE.F." (buffer-size) t)
	  (set-sig-mark)
	  (sig-at-end)))
  (beginning-of-buffer)
  (if (search-forward "\^JE.F." (buffer-size) t)
      (set-up-enclosure))
  (send-it))

(defvar sig-mark-string "\^J-insert signature here-")

(defun set-sig-mark () 
  (beginning-of-line)
  (insert sig-mark-string "\n"))

(defun sig-at-end ()
  (interactive)
  (end-of-buffer)
  (set-sig-mark))

(defvar separator-string "---------------------------An enclosure follows---------------------------")
(defvar encl-head-1 "")
(defvar encl-head-2 "")

(defun set-up-enclosure ()
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  (insert separator-string "\n" encl-head-1 encl-head-2))

(defvar to-cc-separator "\^JPlace the to and cc fields above this line")

(defun determine-sig-file ()
  (interactive)
  (beginning-of-buffer)
  (if (not (search-forward to-cc-separator (buffer-size) t))
      (search-forward mail-header-separator (buffer-size) t)
      (remove-separator))
  (if (search-backward "@" '0 t)
      (setq sig-file "~/.signature")
      (setq sig-file "~/.signature-local")))

(defun remove-separator ()
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defvar sig-file nil)

(defun send-it ()
  (interactive)
  (beginning-of-buffer)
  (determine-sig-file)
  (search-forward sig-mark-string (buffer-size) t)
  (beginning-of-line)
  (kill-line 1)
  (insert-file sig-file)
  (mail-send-and-exit ()))


