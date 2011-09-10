; From: dnb@meshugge.media.mit.edu (David N. Blank)
; Subject: Re: Okay, it's an FAQ (dealing with ugly underlines)
; Date: 5 Aug 91 04:10:24 GMT
; Organization: M.I.T. Media Laboratory
; In-Reply-To: ckd@eff.org's message of 3 Aug 91 23: 30:15 GMT
; 
; > So, what is the canonical very-bestest way to get rid of those annoying
; > underlines in, say, clari.* groups?
; 
; I make no claims of bestest, and the only canonical thing I deal with
; on a regular basis was written by Pachelbel in the 1600's.  Now if you
; are talking quick and dirty hacks that work, well here's something I
; posted a while back:
;            Peace,
;                dNb
; ------
;Howdy-
;  If you both get the ClariNet newsgroups (ClariNews) and read your
;news in Gnus, this might be of interest to you.  I slapped this
;together a while back when I got frustrated at ClariNews' use of
;backspaced underscores for underlining.  I also was tired of seeing
;headers that I didn't really have a use for in my daily news reads.
;It's worked for me for several weeks now, I hope it makes you happy.
;Note: I am not affiliated to ClariNet Communications Corp (except as
;a contented subscriber to ClariNews). Bon Chance!
;         Peace,
;            dNb
;---------
;;; -*- Mode:Emacs-Lisp -*-
;;;
;;; clari-clean.el - a set of functions cobbled together to present more 
;;;                  aesthetic and concise Gnus buffers when reading
;;;                  ClariNews groups (tm ClariNet Communications Corp.)
;;;
;;; Place these two lines in your .emacs to use these functions:
;;;  (autoload 'gnus-clarinews-fun "clari-clean" "Clean ClariNews articles" t)
;;;  (setq gnus-Article-prepare-hook 'gnus-clarinews-fun)
;;;  
;;; Change the value of clarinews-ignored-headers below to flavor to taste.
;;;
;;;                  -- David N. Blank (dnb@meshugge.media.mit.edu) 1/31/91

;;; LCD Archive Entry:
;;; clari-clean|David N. Blank|dnb@meshugge.media.mit.edu
;;; |Clean up buffers when reading ClariNet news groups
;;; |91-01-31||~/functions/clari-clean.el.Z|

(defun gnus-clarinews-fun ()
  ;; only do the voodoo, that you do, so well, if in a ClariNews newsgroup
  (if (string-match "^clari\\.*" gnus-newsgroup-name) 
      (progn 
	(clarinews-clean-article-bs)
	(clarinews-clear-headers)  
	(set-buffer-modified-p nil))))

; function to clean all of the nasty backspaces in ClariNews news articles
; (for use with GNUS reader).  Unabashedly snarfed & hacked from nuke-nroff-bs 
; in the standard distribution's (18.55) man.el -- dNb 1/6/90
(defun clarinews-clean-article-bs ()
  "Nuke the underlining via backspaces found in a ClariNews article."
  (interactive "*")
  ;;Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      (cond ((= preceding following)
	     ;; x\bx
	     (delete-char -2))
	    ((= preceding ?\_)
	     ;; _\b
	     (delete-char -2))
	    ((= following ?\_)
	     ;; \b_
	     (delete-region (1- (point)) (1+ (point)))))))
    (goto-char (point-min))
  ;; Crunch blank lines 
    (while (re-search-forward "\n\n\n\n*" nil t)
      (replace-match "\n\n")))

; remove headers I'd rather not see from ClariNews Gnus buffers.  Snarfed from 
; rmail-clear-headers in standard distribution's (18.55) rmail.el -- dNb 1/6/90
(defun clarinews-clear-headers ()
  "Nuke the headers one would rather not see in a ClariNews article.
   The list of headers is located in clarinews-ignored-headers."
  (goto-char (point-min))
  (if (search-forward "\n\n" nil t)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(let ((buffer-read-only nil))
	  (while (let ((case-fold-search t))
		   (goto-char (point-min))
		   (re-search-forward clarinews-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

  (defvar clarinews-ignored-headers
   "^Location:\\|^ACategory:\\|^Slugword:\\|^Priority:\\|^Format:\\|^ANPA:\\|^Codes:\\|^Note:\\|^X-Supersedes:" 
   "*Gubbish headers one would rather not see in a ClariNews article.")
