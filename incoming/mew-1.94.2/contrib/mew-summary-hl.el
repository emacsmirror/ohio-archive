;; -*- Mode: Emacs-Lisp -*-
;;  $Id: mew-summary-hl.el,v 1.2.2.1 1999/10/20 11:20:55 kazu Exp $
;;
;;                                "Hideyuki SHIRAI" <shirai@rdmg.mgcs.mei.co.jp>
;;
;;;; Mew Summary buffer $B$r(B font-lock $B$r;H$C$F?'IU$1$9$k(B
;;
;;;; $B;H$$J}(B: ~/.emacs $B$K=q$$$F$M(B
;;
;;; $B$3$l$,$"$k$HAGE((B ($B$H$$$&$+L5$$$HCY$/$F;H$$J*$K$J$i$J$$(B)
;; (cond
;;  ((locate-library "lazy-shot")	;; for XEmacs
;;   (require 'font-lock)
;;   (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)
;;   (setq lazy-shot-verbose nil)
;;   (setq lazy-shot-stealth-verbose nil))
;;  ((locate-library "lazy-lock")	;; for Emacs
;;   (require 'font-lock)
;;   (setq font-lock-support-mode 'lazy-lock-mode)))
;;
;;; $BFbIt$G(B window-system $B$H$+$NH=Dj$O$7$F$$$^$;$s!#(B
;; (if (and (or window-system (string-match "XEmacs" emacs-version))
;; 	    (locate-library "mew-summary-hl"))
;;     (eval-after-load "mew" '(require 'mew-summary-hl)))
;;
;;;; $B;HMQ>e$NCm0U(B
;;; XEmacs $B$ONI$/$o$+$i$J$$$N$G!"JQ$@$C$?$i65$($F2<$5$$!#(B(_ _)
;;

(eval-when-compile (require 'mew))
(defconst mew-summary-hl-version "mew-summary-hl 0.10")

;; default $B$GBP>]$H$7$F$$$k(B ~/.im/Config $B$N(B 'From' $B$O0J2<$NDL$j(B
;;; imget.Form=%+4n %m%d %h:%E %+2KK %-24A %S || %b
;;; Form=%+4n %m%d/%y %+3KK %-24A %S || %b
;; PC $B$K$h$C$F$O(B %+2n $B$d(B %2Kk $B$H$$$&$N$b$"$k$N$G(B 'K' $B$H(B '24' $B$G7h$a$&$A(B
(defvar mew-summary-hl-start-regex "^ *\\([1-9][0-9]* [^K]+K\\) ")
;;                                                   ~
;; $B$3$3$N(B space $B$rK:$l$J$$$G$M!#$3$l$G!"(Bmark $B$,$"$k$+$J$$$+H=Dj$7$F$$$^$9!#(B
(defvar mew-summary-hl-from-width 24)
(defvar	mew-summary-hl-ml " *\\([\[(][^])\n\r]*[\])]\\)")
(defvar	mew-summary-hl-subject-regex1 " *\\(.*\\) +\\(\|\|[^\n\r]*\\)")
(defvar mew-summary-hl-subject-regex2 " *\\([^\n\r]*\\)")
;; $B",(B $B:G8e$,(B '|' $B$G=*$k$H%@%a$@$1$I$40&7I(B

;;; Form=%+5n %m%d %-14A %S || %b
;; $B$H$$$&(B IM $B$N(B default $B$@$C$?$i!"$3$s$J46$8!)(B
;; (setq mew-summary-hl-start-regex "^ *\\([1-9][0-9]* [^/]*[0-9]+/[0-9]+\\) ")
;; (setq mew-summary-hl-from-width 14)
;; $B$"$H$O<+J,$N4D6-$K9g$o$;$F2<$5$$!#(B(_ _)

;; face $B$N=qBN$H?'$O$*9%$_$GJQ$($h$&!#(B
;; $B$3$N@_Dj$@$H(B http://www.netlaputa.ne.jp/~hshirai/Image/summary1.png 
;; $B$N$h$&$K$J$j$^$9!#(B
(defvar mew-sumamry-hl-face-list '("num" "from" "to" "ml" "subject" "body"))

(defvar mew-summary-hl-face-num-type 'italic)
(defvar mew-summary-hl-face-num-color "Maroon")

(defvar mew-summary-hl-face-from-type 'bold)
(defvar mew-summary-hl-face-from-color "Purple")

(defvar mew-summary-hl-face-to-type 'bold-italic)
(defvar mew-summary-hl-face-to-color "DarkOrange3")

(defvar mew-summary-hl-face-ml-type 'italic)
(defvar mew-summary-hl-face-ml-color "DarkGreen")

(defvar mew-summary-hl-face-subject-type 'bold)
(defvar mew-summary-hl-face-subject-color "DarkGreen")

(defvar mew-summary-hl-face-body-type 'italic)
(defvar mew-summary-hl-face-body-color "Grey50")

;; MUE/MHC $B$J$I$N?'$E$1MQ4X?t$rDj5A$9$k(B
(defvar mew-summary-hl-external-function nil)

;; hook $B$NDI2C(B
(add-hook 'mew-summary-mode-hook 'mew-summary-hl-enable)
(add-hook 'mew-virtual-mode-hook 'mew-summary-hl-enable)
(add-hook 'mew-summary-inc-sentinel-hook 'mew-summary-hl-block)
(add-hook 'mew-summary-scan-sentinel-hook 'mew-summary-hl-block)

(if (locate-library "mew-refile-view")
    ;; $B$H$j$"$($:!#(B
    (add-hook 'mew-refile-view-mode-hook (lambda () (font-lock-mode 0))))

;; $B<+A0$G$d$k$+$i(B nil $B$K$9$k(B
(setq mew-use-highlight-mark nil)

(defun mew-summary-hl-enable ()
  (make-local-variable 'font-lock-fontify-buffer-function)
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-buffer-function 'mew-summary-hl-buffer)
  (setq font-lock-fontify-region-function 'mew-summary-hl-region)
  (font-lock-mode 1))

(cond
 ((fboundp 'font-lock-fontify-block)
  (fset 'mew-summary-hl-block (symbol-function 'font-lock-fontify-block)))
 (t
  ;; $B$$$$2C8:$@$1$I(B XEmacs $BMQ(B
  (defun mew-summary-hl-block ()
    (font-lock-mode 1))))

(defun mew-summary-hl-buffer ()
  "Mew summary buffer highlight with font-lock-mode."
  (interactive)
  (mew-summary-hl-region (point-min) (point-max)))

(defun mew-summary-hl-region (beg end &optional loudly)
  "Mew summary region highlight with font-lock-mode."
  (interactive "r")
  (if (or (eq major-mode 'mew-summary-mode)
	  (eq major-mode 'mew-virtual-mode))
      (mew-elet
       (goto-char beg)
       (beginning-of-line)
       (setq beg (point))
       (remove-text-properties beg end '(face nil))
       (while (< (point) end)
	 (cond
	  ;; $BIaDL$N9T(B
	  ((looking-at mew-summary-hl-start-regex)
	   (put-text-property (match-beginning 1) (match-end 1)
			      'face 'mew-summary-hl-face-num)
	   (goto-char (match-end 0))
	   (if (looking-at "To:")
	       ;; $B<+J,$N%a!<%k(B
	       (put-text-property (point)
				  (progn (move-to-column
					  (+ (current-column)
					     mew-summary-hl-from-width))
					 (point))
				  'face 'mew-summary-hl-face-to)
	     ;; $BB>$N?M$N%a!<%k(B
	     (put-text-property (point)
				(progn (move-to-column
					(+ (current-column)
					   mew-summary-hl-from-width))
				       (point))
				'face 'mew-summary-hl-face-from))
	   (if (not (looking-at mew-summary-hl-ml))
	       ()
	     ;; [mew-dist 0123] $B$d(B (pgp-users 1234) $B$,$"$C$?(B
	     (put-text-property (match-beginning 1) (match-end 1)
				'face 'mew-summary-hl-face-ml)
	     (goto-char (match-end 0)))
	   (if (not (looking-at mew-summary-hl-subject-regex1))
	       (if (looking-at mew-summary-hl-subject-regex2)
		   (put-text-property (match-beginning 1) (match-end 1)
				      'face 'mew-summary-hl-face-subject))
	     ;; || $B$N$"$H$N(B body $B$,$"$C$?(B
	     (put-text-property (match-beginning 1) (match-end 1)
				'face 'mew-summary-hl-face-subject)
	     (put-text-property (match-beginning 2) (match-end 2)
				'face 'mew-summary-hl-face-body)))
	  ;; mark $B$,IU$$$F$$$k9T(B
	  ((looking-at (concat mew-summary-message-regex "\\([^ ]\\)"))
	   (let (face)
	     (setq face (cdr (assoc (string-to-char (mew-match 2))
				    mew-highlight-mark-keywords)))
	     (if face
		 (put-text-property (point) (progn (end-of-line) (point))
				    'face face)
	       ;; multipart part 2
	       (put-text-property (point) (progn (end-of-line) (point))
				  'face 'mew-summary-hl-face-num))))
	  ;; $B$=$NB>$N9T(B($BIaDL$N?M$O(B multi part $B$rE83+$7$?9T(B)
	  (t
	   (or (and mew-summary-hl-external-function
		    ;; MUE $B$N(B subject $B9T$d(B MHC $B$N(B Virtual folder $B$N?'$E$1$r$9$k(B
		    ;; $B?'$E$1BP>]9T$G$J$+$C$?$i(B 'nil' $B$rJV$7$F$b$i$&(B
		    (funcall mew-summary-hl-external-function))
	       (put-text-property (point) (progn (end-of-line) (point))
				  'face 'mew-summary-hl-face-num))))
	 (forward-line)))))

(defun mew-summary-hl-setup ()
  (let ((flist mew-sumamry-hl-face-list)
	fname type color)
    (mapcar
     '(lambda (face)
	(setq type (intern-soft
		    (concat "mew-summary-hl-face-" face "-type")))
	(setq color (intern-soft
		     (concat "mew-summary-hl-face-" face "-color")))
	(setq fname (intern (concat "mew-summary-hl-face-" face)))
	(copy-face (symbol-value type) fname)
	(set-face-foreground fname (symbol-value color)))
     flist)))

;; load $B$7$?$H$-$K(B face $B$r:n$C$F$7$^$&!#(B
(mew-summary-hl-setup)

(provide 'mew-summary-hl)

;; ends here.
