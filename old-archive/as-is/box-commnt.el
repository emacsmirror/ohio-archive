
;To: unix-emacs@bbn.com
;Date: 16 May 89 15:08:05 GMT
;From: John Sturdy <mcvax!ukc!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: boxed comments
;Organization: Harlequin Ltd, Cambridge, England
;Source-Info:  From (or Sender) name not authenticated.
;
;This program draws "boxed comments" suitable for headings in programs.
;--------------------------------cut here--------------------------------
;;; Time stamp <89/05/16 15:15:23 jcgs>

(provide 'box-comments)

(defun insert-box-border (len char)
  "Make a top or bottom border of a box comment."
  (insert comment-start)
  (let ((i (- len (+ (length comment-start) (length comment-end)))))
    (while (not (zerop i))
      (setq i (1- i))
      (insert char)))
  (insert comment-end))

(defun box-comment ()
  "Make a box comment of the current line."
  (interactive)
  (let* ((comment-start
	  (substring comment-start 0 (string-match "[ 	]+" comment-start)))
	 (comment-end
	  (if (zerop (length comment-end))
		   comment-start
		 (substring comment-end
			    (1+ (string-match "[ 	]+" comment-end)))))
	 (border
	  (substring comment-start (1- (length comment-start)))))
    (beginning-of-line 1)
    (insert comment-start " ")
    (capitalize-word 1)
    (end-of-line 1)
    (insert " " comment-end)
    (let ((width (current-column)))
      (insert "\n")
      (beginning-of-line 0)
      (insert-box-border width border)
      (insert "\n")
      (beginning-of-line 2)
      (insert-box-border width border))))

;;; end of box-comments.el
;--------------------------------cut here--------------------------------

