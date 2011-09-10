;Date: 22 Mar 89 17:13:29 GMT
;From: garfield!eppstein@columbia.edu  (David Eppstein)
;Subject: Finding undeleted messages after a delete
;To: info-gnu-emacs@prep.ai.mit.edu

;; Find undeleted messages after a delete
;; David Eppstein / Columbia University / 22 Mar 1989
;;
;; In Babyl (the rmail equivalent for TOPS-20 and ITS Emacs), when a
;; message is deleted and there is no later undeleted message, it moves
;; back to a previous undeleted message instead.
;;
;; Instead, rmail just complains that it can't find anything.  So I
;; wrote the following, to make it more like Babyl.

(defun rmail-delete-forward ()
  "Delete this message and move on to next or previous nondeleted one."
  (interactive)
  (rmail-set-attribute "deleted" t)
  (rmail-find-undeleted-msg))

(defun rmail-find-undeleted-msg ()
  "Show following non-deleted message, or previous if no following."
  (rmail-maybe-set-message-counters)
  (let ((increment 1)
	(current rmail-current-message))
    (while (if (<= current rmail-total-messages)
	       (rmail-message-deleted-p current)
	     (if (= increment 1)
		 (setq increment -1)
	       (error "Header of rmail file is deleted")))
      (setq current (+ increment current)))
    (rmail-show-message current)))
;-- 
;David Eppstein  eppstein@garfield.cs.columbia.edu  Columbia U. Computer Science

