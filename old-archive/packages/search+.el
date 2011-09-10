;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!ucbvax!hplabs!hp-pcd!hpcvca!charles Thu Jan 11 09:52:44 1990
;Article 1162 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!ucbvax!hplabs!hp-pcd!hpcvca!charles
;From charles@hpcvca.CV.HP.COM (Charles Brown)
;Newsgroups: comp.emacs
;Subject: Re: Forward and Backward Searches with Full Editing.
;Message-ID: <640013@hpcvca.CV.HP.COM>
;Date: 9 Jan 90 00:10:18 GMT
;References: <640012@hpcvca.CV.HP.COM>
;Organization: Hewlett-Packard Co., Corvallis, Oregon
;Lines: 96
;
;I received several excellent suggestions from tale@turing.cs.rpi.edu
;(David C Lawrence).  Most of these have been incorporated into the
;following improved version.  This code should behave identically with
;the old code.
;
;I release any copyright from this code.  It is fully public domain.
;--
;	Charles Brown	charles@cv.hp.com or charles%hpcvca@hplabs.hp.com
;			or hplabs!hpcvca!charles or "Hey you!"
;---------------------------------------------------------------------
; Charles Brown  date="Mon Jan  8 16:10:12 1990"

; The algorithm for each buffer is
;   if (a search has been done in this buffer) then
;     pull up the old search string for this buffer (search-last)
;   else
;     if (a search has been done in any buffer) then
;	pull up the old search string not specific this buffer (*search-last*)

(defvar *re-search-last* "")
(defvar re-search-last "")
(make-variable-buffer-local 're-search-last)
(defvar *search-last* "")
(defvar search-last "")
(make-variable-buffer-local 'search-last)

(defun stringnzp (str)
  "Return STR if it is a string of non-zero length."
  (and (stringp str) (string< "" str) str))

(defun
  re-search-forward-command (regexp &optional repeat)
  "Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return t.
Optional second argument REPEAT (prefix argument interactively)
is the number of times to repeat the search."
  (interactive (list (read-string
		       "RE search: "
		       (or (stringnzp re-search-last)
			   (stringnzp *re-search-last*)))
		     (prefix-numeric-value current-prefix-arg)))
  (if (or (null regexp) (equal "" regexp))
      (error "Search string is null")
    (setq re-search-last (setq *re-search-last* regexp))
    (re-search-forward regexp nil nil repeat))
  t)

(defun re-search-backward-command (regexp &optional repeat)
  "Search backward from point for match for regular expression REGEXP.
Set point to the beginning of the match, and return t.
Optional second argument REPEAT (prefix argument interactively)
is the number of times to repeat the search."
  (interactive (list (read-string
		       "RE search backward: "
		       (or (stringnzp re-search-last)
			   (stringnzp *re-search-last*)))
		     (prefix-numeric-value current-prefix-arg)))
  (if (or (null regexp) (equal "" regexp))
      (error "Search string is null")
    (setq re-search-last (setq *re-search-last* regexp))
    (re-search-backward regexp nil nil repeat))
  t)

(defun
  search-forward-command (regexp &optional repeat)
  "Search forward from point for STRING.
Set point to the end of the occurrence found, and return t.
Optional second argument REPEAT (prefix argument interactively)
is the number of times to repeat the search."
  (interactive (list (read-string
		       "search: "
		       (or (stringnzp search-last)
			   (stringnzp *search-last*)))
		     (prefix-numeric-value current-prefix-arg)))
  (if (or (null regexp) (equal "" regexp))
      (error "Search string is null")
    (setq search-last (setq *search-last* regexp))
    (search-forward regexp nil nil repeat))
  t)

(defun
  search-backward-command (regexp &optional repeat)
  "Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return t.
Optional second argument REPEAT (prefix argument interactively)
is the number of times to repeat the search."
  (interactive (list (read-string
		       "RE search backward: "
		       (or (stringnzp search-last)
			   (stringnzp *search-last*)))
		     (prefix-numeric-value current-prefix-arg)))
  (if (or (null regexp) (equal "" regexp))
      (error "Search string is null")
    (setq search-last (setq *search-last* regexp))
    (search-backward regexp nil nil repeat))
  t)
