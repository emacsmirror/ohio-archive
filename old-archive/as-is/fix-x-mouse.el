;To: arpa-unix-emacs@chips.bbn.com
;Subject: [lamy@ai.utoronto.ca: X11 mouse bindings a la xterm]
;Date: Mon, 06 Feb 89 00:12:15 -0500
;From: John Robinson <jr@chips.bbn.com>
;
;Thought the rest of the net might like to see this note.  /jr
;------- Forwarded Message
;From: lamy@ai.utoronto.ca (Jean-Francois Lamy)
;Subject: X11 mouse bindings a la xterm
;Newsgroups: gnu.emacs
;Message-ID: <89Feb4.161333est.38070@neat.ai.toronto.edu>
;
;The following functions make the X11 mouse behave under Emacs in a way that
;won't throw off xterm users.
;
;- clicking left sets the point.  
;- dragging left (i.e. releasing the button at a different position) sets
;  the mark at the release position and copies to the X11 cut buffer.
;- clicking right sets the mark and copies the region to the X11 cut buffer.
;- clicking middle pastes the X11 cut-buffer *at the Emacs point*.  For some
;  reason everybody around here much prefers that behaviour (in fact the curren
;t
;  behaviour is absolutely maddening :-)
;
;The fact that the X11 cut buffer is set as a side-effect of setting
;the mark is no annoyance.  The only thing missing is highlighting the region
;like ZWEI does...
;
;To invoke, add somthing like what follows to your .emacs
;
;(if (eq window-system 'x)
;   (progn
;   (setq term-setup-hook 'my-x-setup)
;   (defun my-x-setup () ; hook to run after terminal is initialized
;     (load "fix-x-mouse")
;     (message " "))))
;
;
;Jean-Francois Lamy               lamy@ai.utoronto.ca, uunet!ai.utoronto.ca!lam
;y
;AI Group, Department of Computer Science, University of Toronto, Canada M5S 1A
;4
;
;----------------------------8< couper ici 8<----------------------------------
;- -
;;; fix-x-mouse.el
;;;
;;; make mouse-based paste ignore the mouse position and use the emacs point
;;; instead.  Make cut-text set the mark so the mouse can be used naturally
;;; to select a region (e.g. to indent code).
;;;
;;; Jean-Francois Lamy 1989-09-21
;;; lamy@ai.utoronto.ca

(defun x-cut-text (arg &optional kill)
  "Copy text between point and mouse into window system cut buffer.
Set mark to current mouse position. Save in Emacs kill ring also."
  (if (coordinates-in-window-p arg (selected-window))
      (progn
       (x-mouse-set-mark arg)
	 (let ((beg (point)) (end (mark)))
	   (x-store-cut-buffer (buffer-substring beg end))
	   (if kill (delete-region beg end))))
    (message "Mouse not in selected window")))

(defun x-paste-text (arg)
  "Insert window system cut buffer contents at current point."
  (insert (x-get-cut-buffer)))

(defun x-cut-and-wipe-text (arg)
  "Kill text between point and mark; also copy to window system cut buffer."
  (x-cut-text arg t))

(defun x-cut-text-if-moved (arg &optional kill)
  (let ((opoint (point)))
    (x-mouse-set-point arg)
    (cond 
     ((not (equal (point) opoint))
      (goto-char opoint)
      (x-cut-text arg kill)))))

(define-key mouse-map x-button-left-up 'x-cut-text-if-moved)

;------- End of Forwarded Message


