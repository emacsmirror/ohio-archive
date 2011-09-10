;To: unix-emacs@bbn.com
;Date: 31 Oct 88 15:08:02 GMT
;From: John Sturdy <mcvax!ukc!eagle.ukc.ac.uk!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Subject: improvements to tags-helper
;
;Here are some improvements for the tags-helper program I posted a while ago.
;They make it handle multiple tags files quite transparently.
;
;----------------------------------------------------------------
;;; Put this near the top of the file:
(defvar tag-lists-list nil
  "alist of tag file names to tag alists.")

;;; Put this at the end of make-tags-name-list:
(setq tag-lists-list (cons
                        (cons tags-file-name tags-name-list)
                        (delequal tags-file-name
                                  tag-lists-list)))

;;; Put this just after (interactive) in insert-tag-name:
(setq tags-name-list
        (cdr (assoc tags-file-name tag-lists-list)))

;;; put this in somewhere:
(defun delequal (item in-list)
  "Remove all occurrences of ITEM from IN-LIST, taking a copy, and not
altering anything."
  (if (null in-list)
      nil
    (if (equal item (car in-list))
        (delequal item (cdr in-list))
      (cons (car in-list) (delequal item (cdr in-list))))))
;----------------------------------------------------------------
;__John
;                      All manner of things will be well (St. Julian of Norwich)
;
;         jcgs@uk.co.harlqn Harlequin Ltd,Barrington,Cambridge,UK +44-223-872522

