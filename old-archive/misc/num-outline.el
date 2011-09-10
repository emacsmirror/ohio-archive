;From utkcs2!emory!samsung!usc!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ansa.co.uk!ajw Thu Jul 12 13:37:24 EDT 1990
;Article 3184 of gnu.emacs:
;Path: utkcs2!emory!samsung!usc!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ansa.co.uk!ajw
;>From: ajw@ansa.co.uk (Andrew Watson)
;Newsgroups: gnu.emacs
;Subject: outline-regexp still broken
;Message-ID: <9007121207.AA12843%crippen@ansa.co.uk>
;Date: 12 Jul 90 12:07:41 GMT
;References: <487@exodus.Eng.Sun.COM>
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 312
;
;Brian Holtz (holtz@netcord.Eng.Sun.COM) asks:
;
;> 					       Has anyone even seen
;> outline mode working for anything other than the default heading-line
;> pattern of 1 or more asterisks?
;
;Yes. I'm apending some code that I wrote, and frequently use, to provide
;numbered outline headings. All of the hide-subtree/show-subtree things work OK.
;It doesn't answer Brian's main question, but perhaps people will find it of
;interest.
;
;To use: Go into outline-mode, then (load-library "numbered-outline").
;Alternatively, I'm also appending a file called document-mode.el, which I use
;to pull together numbered-outline and Martin Neitzel's gin-mode.
;M-x document-mode will then do all the work for you. The general roundaboutness
;of approach is because outline-mode doesn't provide a feature to say whether
;it's loaded or not - to my mind this is an oversight.
;
;       .                           Regards,
;      / \^
;     / / \ \                           Andrew
;    / /   \ \
;   '=========`        Andrew Watson                Tel:      +44 223 323010
;  /| |\ | <  |\	      APM Ltd, Poseidon House,     Fax:      +44 223 359779
; / | | \| _> | \      Castle Park,                 UUCP:     mcvax!ukc!ansa!ajw
; ---------------      Cambridge CB3 0RD, UK        Internet: ajw@ansa.co.uk
;
;--First File--First File--First File--First File--First File--First File--First File--
;;; numbered-outline.el
;;;
;;; Andrew Watson (ajw@ansa.co.uk) 3/3/90
;;;
;;; This code is distributed without any warranty of any sort.
;;;
;;; This code augments outline mode by changing the regexps and things to support the
;;; notion that a heading is of the form 1.2.3, and by adding  new commands to
;;; automagically generate next heading labels:
;;;
;;; outline-next-heading-along (bound to c-c c-c)
;;;    Searches back for the previous heading, and insert the next one in
;;;    sequence, so that 1.2.4.5 -> 1.2.4.6 -> 1.2.4.7 etc
;;;    Given a prefix argument, generates the next heading at that level.
;;;    Example: ESC-2 c-c c-c when last heading was 1.4.5.9.2 gives 1.5
;;;
;;; outline-next-heading-down (bound to c-c c-e)
;;;    Inserts a heading at the next nesting level, so that 1.2.4.5 ->
;;;    1.2.4.5.1 -> 1.2.4.5.1.1
;;;
;;; outline-next-heading-up (bound to c-c c-d)
;;;    Inserts a heading at the next level up, so that 1.2.4.5 -> 1.2.5 -> 1.3
;;;    With a prefix arg goes that many levels out. Example: ESC-2 c-c c-d when
;;;    last heading was 1.4.5.9.2 gives 1.4.6
;;;
;;; renumber-heading
;;;    Change the number on this heading without changing the text. The heading
;;;    number is supplied as an argument, with the default being to search back
;;;    for the last heading and use the next sequential heading at the level
;;;    of the current heading.
;;;
;;; renumber-region
;;;    Renumber all the headings in a region, keeping them at the same relative
;;;    levels as they are at present. The first heading is either supplied as
;;;    an argument, or inferred in the same fashion as for renumber-heading.
;;;    (Q: What if supplied argument is at wrong level?)
;;;
;;; To do:
;;;
;;; renumber-buffer (easy)
;;;    Renumber all the headings in the buffer. Supplied arg is first heading,
;;;    defaults to first heading at current level.
;;;
;;; subordinate-region
;;;    Renumber the region so that the levels of the headings relative to each
;;;    other remains the same, but the first one is subordinated to the
;;;    immediately preceeding heading.
;;;
;;; running-region (bad name)
;;;    Renumber the region so that the levels of the headings relative to each
;;;    other remains the same, but the highest level headings in the region run
;;;    on from the previous heading before the region.
;;;
;;; mark-heading
;;;    Place the region around all the text in the heading containing point.
;;;    With a heading argument, find and mark that heading and all its children.

(provide 'numbered-outline)

(defvar component-regexp)
(defvar seperator-regexp)

(defvar heading-leader "\n" "String to insert before a heading")
(defvar heading-trailer " " "String to insert after a heading")
(defvar heading-seperator "." "String to use as seperator when inserting heading")

(setq component-regexp "[0-9]+")
(setq seperator-regexp "\\.")
(setq outline-regexp (concat component-regexp
			     "\\(" seperator-regexp component-regexp "\\)*"))


;;; Basic representation - a heading is represented as a list, each element
;;; being one component. Hence (length x) = depth of x. List is stored reversed
;;; to save on garbageful operations when we're bashing the last element.

;;; What heading are we looking-at?
  
(defun outline-heading ()
  (save-excursion
    (if (looking-at component-regexp)
	(nreverse
	 (cons (read (current-buffer))
	       (progn (backward-char 1) (outline-heading1)))))))

(defun outline-heading1 ()
  (if (looking-at seperator-regexp)
      (progn
	(goto-char (match-end 0))      ; end of the seperator
	(if (looking-at component-regexp)
	    (progn (goto-char (match-end 0))
		   (cons (car (read-from-string     ; do this with setcdr?
			       (buffer-substring (match-beginning 0)
						 (match-end 0))))
			 (outline-heading1)))))))

(defun outline-level ()
  (length (outline-heading)))

;;; Something to insert a heading at the current point

(defun insert-heading (heading where)
  (princ heading-leader where)
  (insert-heading1 heading where)
  (princ heading-trailer where))

(defun insert-heading1 (heading where)
  (let ((first (car heading))
	(rest (cdr heading)))
    (if rest
	(progn
	  (insert-heading1 rest where)
	  (princ heading-seperator where)))
    (princ first where)))

;;; AJW 8/2/90 Change outline-newline-maybe to throw a newline only if point is
;;; on a line with text. Blank lines get deleted. This may or may not be right ...

(defun outline-newline-maybe ()
  (beginning-of-line 1)
  (if (looking-at "^[ \t]*$")    ; if line is blank
      (delete-region (point) (progn (end-of-line 1) (point))) ; delete all chars on it
    (progn (end-of-line 1)
	   (newline))))

;;; The three commands at present introduce new headings

(defun outline-next-heading-along (arg)
  "Do next heading"
  (interactive "P")
  (if arg
      (outline-insert-absolute-heading arg)
    (outline-insert-relative-heading 0)))

(defun outline-next-heading-up (arg)
  "Do next heading up. With prefix arg, go that many levels up."
  (interactive "p")
  (outline-insert-relative-heading arg))

(defun outline-next-heading-down ()
  "Insert next most nested heading"
  (interactive)
  (outline-insert-next-heading-down))

(define-key outline-mode-map "\C-c\C-c" 'outline-next-heading-along)
(define-key outline-mode-map "\C-c\C-d" 'outline-next-heading-up)
(define-key outline-mode-map "\C-c\C-e" 'outline-next-heading-down)

;;; Thing to find the previous heading and return it

(defun outline-previous-heading ()
  (catch 'outline-heading-found
    (save-excursion
      (while (not (bobp))
	(beginning-of-line 0)   ; start of previous line
	(if (looking-at outline-regexp)
	    (throw 'outline-heading-found (outline-heading)))))

    ;; Nothing found means that the "previous" heading was the zeroth

    (list 0)))

(defun outline-insert-relative-heading (arg)
  (outline-newline-maybe)
  (let* ((last-heading (outline-previous-heading))
	 (last-at-this-level (nthcdr arg last-heading)))
    (if last-at-this-level
	(progn
	  (insert-heading (increment-heading last-at-this-level)
			  (current-buffer))
	  (princ "Last heading was " t)
	  (insert-heading1 last-heading t))
      (error "You can't go that far out!"))))

(defun outline-insert-absolute-heading (arg)
  (let* ((last-heading (outline-previous-heading))
	 (last-depth (length last-heading))
	 (x (- last-depth arg)))
    (outline-insert-relative-heading x))) ; h-e-a-v-y !


(defun outline-insert-next-heading-down ()
  (outline-newline-maybe)
  (let ((last-heading (outline-previous-heading)))
    (insert-heading (cons 1 last-heading) (current-buffer))
    (princ "Last heading was " t)
    (insert-heading1 last-heading t)))

;;; Renumbering things. Note the level of the heading on this line, then look
;;; back for the last heading and renmber this one to be at the same level as
;;; it was before, but consistent with the last one. Note that this will quite
;;; happily make the successor to 2.4 be 2.4.1.1.1.1.1.1.1 if that's what you
;;; want ... If called with an argument, use that as the values of the last
;;; heading and increment it to make this one. Otherwise, go find the last one.
;;; Return this heading as result.

(defun renumber-heading ()
  (interactive)
  (renumber-heading1 nil))

(defun renumber-heading1 (arg)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at outline-regexp)
	(let* ((x1 (match-beginning 0))     ; snatch the match
	       (x2 (match-end 0))
	       (last-heading (or arg (outline-previous-heading)))
	       (delta (- (length last-heading) (outline-level)))
	       (new-hdg (if (natnump delta)		; that's >= 0 to you, squire
			    (increment-heading (nthcdr delta last-heading))
			  (append (make-list (- delta) 1) last-heading))))
	  (delete-region x1 x2)
	  (insert-heading1 new-hdg (current-buffer))
	  new-hdg)
      (error "No heading on this line"))))

;;; Put this in a function ...

(defun increment-heading (x)
  (cons (1+ (car x)) (cdr x)))

;;; Renumber a region - might not work yet ... got to make this interactive, too.
;;; Must also factor out that nasty concat  put it in a global variable?

(defun renumber-region1 (hdg start end)
  (if (> start end)
      (let ((tmp end))
	(setq end start start tmp)))
  (save-excursion
    (goto-char start)
    (while (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)") end t)
      (goto-char (match-beginning 1))
      (setq hdg (renumber-heading1 hdg)))))

(defun renumber-region (from to)
  (interactive "r")
  (renumber-region1 nil from to))

--Second file--Second file--Second file--Second file--Second file--Second file--

;;; Quick hack to hold together all the disparate bits of emacs lisp that I use
;;; for writing documents.
;;;
;;; This code is distributed without any warranty of any sort.
;;;
;;; AJW 14/3/89


(defun autoloadp (x)
  "T if the symbol-function of x is an autoload"
  (and (fboundp x)
       (consp (symbol-function x))
       (eq (car (symbol-function x)) 'autoload)))

;;; Outline-mode hacks the paragraph-start and paragraph-separate regexps
;;; unmercifully

(defun document-mode ()
  "Edit documents with numbered headings, outlining and automatic indentation"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (setq paragraph-separate "^\\s *$")
  (setq paragraph-start "^\\s *$")
  (if (and (featurep 'numbered-outline)
	   (autoloadp 'outline-mode))
      (progn
	(outline-mode)  ; will autoload
	(load-library "numbered-outline"))  ; re-load over the top
    (outline-mode)
    (require 'numbered-outline))
  (require 'gin-mode)
  (gin-mode 1) ; switch it on
  (setq gin-left-hang-indent-re 
	"\\s *\\([0-9]+[.)]\\|[ivx]+)\\|[-*]\\|Editorial:\\|Action:\\)\\s +")
  (setq gin-retain-indent-re "[-+>]+[ \t]*\\|[ \t]+")
  (setq paragraph-ignore-fill-prefix t)
  (setq paragraph-separate "^\\s *$")
  (setq paragraph-start "^\\s *$"))  

(defun forward-document-paragraph ()
  (interactive)
  (re-search-forward "^\\s *$"))


