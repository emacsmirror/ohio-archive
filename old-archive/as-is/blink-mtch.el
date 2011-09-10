;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: Re: Parenthesis matching when positioned over curser 
;Date: Fri, 10 Mar 89 09:31:47 -0500
;From: Stephen Gildea <gildea@bbn.com>
;
;I've seen two paren-matching routines posted here, and neither used
;the obvious way to test for a paren.
;
;Simple test for an open paren:	(looking-at "\\s(")
;Simple test for a close paren:	(looking-at "\\s)")
;
;Also, you might find the following one-liner does what you want:

(defun interactive-blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive)
  (let ((blink-matching-paren t))
    (blink-matching-open)))

; < Stephen

;Date: 9 Mar 89 20:52:19 GMT
;From: apple!oliveb!intelca!mipos3!nate@bloom-beacon.mit.edu  (Nate Hess)
;Subject: Re: Parenthesis matching when positioned over curser
;To: info-gnu-emacs@prep.ai.mit.edu
;
;In article <11782@shamash.cdc.com>, tciaccio@opus (Tom Ciaccio) writes:
;>At the very least I'm looking for a function to match the paren at my
;>request.  Currently, I am deleting the paren and retyping it in so as
;>to see the match.  This is alot of extra keystokes.  Any help would be
;>appreciated.
;
;
;Give this code a try:


;;
;; Lifted *this* code from vip-mode.el, and chopped out the
;; vi-keeping-track-of-region stuff.
;;
(defun paren-match ()
  "Jumps to the paren matching the one under point,
and does nothing if there isn't one."
  (interactive)
  (cond ((looking-at "[\(\[{]")
	 (forward-sexp 1)
	 (backward-char))
	((looking-at "[])}]")
	 (forward-char)
	 (backward-sexp 1))
	(t (message "Could not find matching paren.")))
) ;paren-match


;	   "What I like is when you're looking and thinking and looking
;	   and thinking...and suddenly you wake up."   - Hobbes

;woodstock@hobbes.intel.com   ...!{decwrl|hplabs!oliveb}!intelca!mipos3!nate 

;Date: Thu, 9 Mar 89 09:41:42 PST
;From: mrspoc!guinan!kayvan@apple.com
;To: tciaccio@opus.cdc.com
;Cc: info-gnu-emacs@prep.ai.mit.edu
;Subject: Parenthesis matching when positioned over curser
;
;Here's my contribution to this. It's a function that matches the character
;that the cursor is on when invoked (it checks the syntax table, so it's
;general across all modes).
;
;In my .emacs file I have:

(global-set-key "\C-x%" 'match-it)
(autoload 'match-it "match" "" t)

;------------------------------ match.el ---------------------------------
; match parenthesis according to syntax tables. Like % in vi.
; matches the character following dot by jumping to matching character.
;
; By Kayvan Sylvan

(defun match-it ()
  "Match character after dot to its corresponding open or close."
  (interactive)
  (let ((syntax (char-syntax (following-char))))
    (cond ((= syntax ?\() (match-it-forward))
	  ((= syntax ?\)) (match-it-backward))
	  (t (error "%c is not an open or close." (following-char))))))

(defun matching-char (char table)
  "return the matching char of CHAR from TABLE. TABLE must be a syntax-table.
returns nil if none is found"
  (cond ((and (syntax-table-p table) (char-or-string-p char))
	 (if (stringp char) (setq char (string-to-char char)))
	 (if (> (setq char (lsh (aref table char) -8)) 0) char nil))
	(t (error "Not syntax table or bad char"))))

(defun go-error (pos err)
  "Goto position POS of file, while signalling error ERROR"
  (goto-char pos)
  (error err))

(defun match-it-forward ()
  "Find match for an open."
  (let ((table (syntax-table)) (pos (dot)))
    (setq char (matching-char (following-char) table))
    (forward-sexp 1)
    (backward-char)
    (if (not (= char (following-char))) (go-error pos "mismatch"))))

(defun match-it-backward ()
  "Find match for a close."
  (let ((table (syntax-table)) (pos (dot)))
    (setq char (matching-char (following-char) table))
    (forward-char)
    (backward-sexp 1)
    (if (not (= char (following-char))) (go-error pos "mismatch"))))

