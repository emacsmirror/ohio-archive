; Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ELF.TN.CORNELL.EDU!eirik
; >From: eirik@ELF.TN.CORNELL.EDU ("Eirik Fuller")
; Newsgroups: gnu.emacs
; Subject: Tek 4404 mouse support
; Date: 24 Jul 90 03:09:11 GMT
; Organization: GNUs Not Usenet
; 
; The enclosed lisp code provides the machine-specific details necessary
; to use the mouse support in my previous posting with the terminal
; emulator of a Tektronix 4404 (and its various descendants like the
; 4315, 4316, and 4317).  On my system it is installed as
; /usr/local/emacs/lisp/term/4404.el; also included is a support
; function it uses.
; 
; ---------------- term/4404.el ----------------
; 
(require 'add-hook)
(require 'mouse)

(global-set-key "\M-P" 'tek4404-handle-mouse-event)

(defun tek4404-enable-mouse ()
  "Send escape sequence which enables mouse events and graphics cursor"
  (send-string-to-terminal "\C-[Q3;1J\C-[Q1M"))

(defun tek4404-disable-mouse ()
  "Send escape sequence which disables mouse events and graphics cursor"
  (send-string-to-terminal "\C-[QJ\C-[QM"))

(defun tek4404-handle-mouse-event ()
  "Parse the escape sequence, and call the appropriate function"
  (interactive)
  (let ((state (read-char)) (button (read-char)) (way (read-char))
	point index)
    (setq point (- (read-number) 1))
    (setq point (list (- (read-number) 1) point))
    (setq index (+ (- ?3 button) (* (/ (- way ?D) 17) 4)))
    (if (= (read-char) ?\\ )
	(funcall (aref mouse-map index) point))))

(defun read-number ()
  "Read digits from input and keep tally. Lose last character"
  (let ((total 0) digit)
    (while (progn 
	     (setq digit (read-char))
	     (and (>= digit ?0) (<= digit ?9)))
      (setq total (+ (* total 10) (- digit ?0))))
    total))

(add-hook 'kill-emacs-hook 'tek4404-disable-mouse)
(add-hook 'suspend-hook 'tek4404-disable-mouse)
(add-hook 'suspend-resume-hook 'tek4404-enable-mouse)
(tek4404-enable-mouse)

;; ---------------- add-hook.el ----------------

(provide 'add-hook)

(defun add-hook (hook-var hook-function)
  "Prepend hook-function to hook-var's value, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (and (not (memq hook-function value))
		 (set hook-var
		      (if value (cons hook-function value) hook-function)))
	  (and (not (eq hook-function value))
	       (set hook-var
		    (list hook-function value)))))
    (set hook-var hook-function)
    ))
