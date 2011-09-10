;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!moose.crd.ge.com!montnaro Thu Dec 14 11:46:14 1989
;Article 845 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!moose.crd.ge.com!montnaro
;From montnaro@moose.crd.ge.com (Skip Montanaro)
;Newsgroups: gnu.emacs
;Subject: Grow and shrink minibuffer window as minibuffer grows and shrinks
;Message-ID: <8912140405.AA00554@moose.crd.Ge.Com>
;Date: 14 Dec 89 04:05:25 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Reply-To: <montanaro@crdgw1.ge.com> (Skip Montanaro)
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 128
;
;One thing that's always bugged me is that only a single line of a minibuffer
;is displayed when it is visible in a window. I took the first crude steps
;toward solving that dilemma this evening. The ELisp code that follows
;rebinds the keys DEL, C-d, C-k, C-o, C-n, and C-q so that the minibuffer
;grows or shrinks as necessary (up to at most minibuffer-max-window-height
;lines) to display the entire minibuffer contents.
;
;The code has only been minimally tested, but what's there appears to work.
;
;Feedback is appreciated. Note that I left the point positioning commands for
;another time. Feel free to send me any suggestions, bugs, whatever ...
;
;Skip (montanaro@crdgw1.ge.com)
;
;------------------------------------------------------------------------------
;;; minibuf.el - grow/shrink minibuffer window so all lines show
;;; Skip Montanaro - 12/13/89
;;; This file is not (yet) part of GNU Emacs, however, GNU copyleft applies


(defvar minibuffer-max-window-height 1
  "*Maximum number of lines a minibuffer window can normally display.
Defaults to 1.")


;;; functions that can grow the minibuffer window

(defun minibuffer-open-line (arg)
  "Open line in current minibuffer and enlarge if not all are visible."
  (interactive "P")
  (open-line (or arg 1))
  (minibuffer-enlarge-window))


(defun minibuffer-next-line (arg)
  "Goto next line in current minibuffer and enlarge if not all are visible."
  (interactive "P")
  (next-line (or arg 1))
  (minibuffer-enlarge-window))


(defun minibuffer-quoted-insert (c)
  "Read next input char and insert it. If ^J, enlarge window if not all visible."
  (interactive "c")
  (insert c)
  (if (eq c ?\C-j) (minibuffer-enlarge-window)))


(defun minibuffer-enlarge-window ()
  (if (and (< (window-height) minibuffer-max-window-height)
	   (or (not (pos-visible-in-window-p (point-min) nil))
	       (not (pos-visible-in-window-p (point-max) nil))))
      (enlarge-window 1))
  (goto-char (prog1 (point) (goto-char (point-min)) (recenter 0))))


;;; functions that can shrink the minibuffer window

(defun minibuffer-delete-char (arg)
  "Like delete-char, but shrinks minibuffer window if possible."
  (interactive "P")
  (delete-char (or arg 1))
  (minibuffer-shrink-window))


(defun minibuffer-kill-line (arg)
  "Like kill-line, but shrinks minibuffer window if possible."
  (interactive "P")
  (kill-line arg)
  (minibuffer-shrink-window))


(defun minibuffer-backward-delete-char (arg)
  "Like backward-delete-char, but may shrink minibuffer window."
  (interactive "P")
  (backward-delete-char (or arg 1))
  (minibuffer-shrink-window))


(defun minibuffer-shrink-window ()
  "Shrink wrap minibuffer window about (point-min) and (point-max)."
  (if (and (> (window-height) 1)
	   (< (count-lines (point-min) (point-max)) (window-height)))
      (shrink-window (- (window-height) (count-lines (point-min) (point-max))))))


;;; common functions that move around in the minibuffer

;;; minibuffer replacements for

;;; previous-line
;;; end-of-buffer
;;; beginning-of-buffer
;;; scroll-up
;;; scroll-down

;;; are left as an exercise to the reader. I'm going to bed.


;;; and the key bindings. 
;;; (should be a loop over the various minibuffer maps)
(define-key minibuffer-local-map "\C-o" 'minibuffer-open-line)
(define-key minibuffer-local-map "\C-n" 'minibuffer-next-line)
(define-key minibuffer-local-map "\C-q" 'minibuffer-quoted-insert)
(define-key minibuffer-local-map "\C-d" 'minibuffer-delete-char)
(define-key minibuffer-local-map "\C-k" 'minibuffer-kill-line)
(define-key minibuffer-local-map "\C-?" 'minibuffer-backward-delete-char)

(define-key minibuffer-local-completion-map "\C-o" 'minibuffer-open-line)
(define-key minibuffer-local-completion-map "\C-n" 'minibuffer-next-line)
(define-key minibuffer-local-completion-map "\C-q" 'minibuffer-quoted-insert)
(define-key minibuffer-local-completion-map "\C-d" 'minibuffer-delete-char)
(define-key minibuffer-local-completion-map "\C-k" 'minibuffer-kill-line)
(define-key minibuffer-local-completion-map "\C-?" 'minibuffer-backward-delete-char)

(define-key minibuffer-local-must-match-map "\C-o" 'minibuffer-open-line)
(define-key minibuffer-local-must-match-map "\C-n" 'minibuffer-next-line)
(define-key minibuffer-local-must-match-map "\C-q" 'minibuffer-quoted-insert)
(define-key minibuffer-local-must-match-map "\C-d" 'minibuffer-delete-char)
(define-key minibuffer-local-must-match-map "\C-k" 'minibuffer-kill-line)
(define-key minibuffer-local-must-match-map "\C-?" 'minibuffer-backward-delete-char)

(define-key minibuffer-local-ns-map "\C-o" 'minibuffer-open-line)
(define-key minibuffer-local-ns-map "\C-n" 'minibuffer-next-line)
(define-key minibuffer-local-ns-map "\C-q" 'minibuffer-quoted-insert)
(define-key minibuffer-local-ns-map "\C-d" 'minibuffer-delete-char)
(define-key minibuffer-local-ns-map "\C-k" 'minibuffer-kill-line)
(define-key minibuffer-local-ns-map "\C-?" 'minibuffer-backward-delete-char)


