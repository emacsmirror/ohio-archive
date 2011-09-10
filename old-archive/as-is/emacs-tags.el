;To: unix-emacs@chips.bbn.com
;Subject: emacs-tags package
;Date: Fri, 20 Jan 89 09:11:10 -0500
;From: John Robinson <jr@chips.bbn.com>
;
;  Date:    Fri, 20 Jan 89 01:23:38 -0800 
;  From:    cwitty@csli.Stanford.EDU (Carl Witty)
;  To:      jr@bbn.com (John Robinson)
;  Subject: Re: how to change backup file names?
;
;  The function find-emacs-tag, which you mentioned, is not in versions
;  18.50 or 18.52, so you might not want to use it in examples.
;
;  Carl Witty
;  cwitty@csli.Stanford.EDU
;
;I knew this, but forgot to mention it in my posting.  find-emacs-tags
;is about the most useful of the various bits and pieces I have snarfed
;from the net.  It is short, so I just appended it to this message.
;And it oughtter be in the distribution (added to tags.el)!
;
;Suggested filename: emacs-tags.el.  Suggested binding (in your .emacs):
;
;  (autoload 'find-emacs-tag "emacs-tags"
;	    "Package for finding tags in emacs sources themselves.")
;
;  (global-set-key "." 'find-emacs-tag)
;
;Thanks, Carl!
;
;/jr
;jr@bbn.com or bbn!jr
;--------
;;; Little function to enable finding tags in the emacs tags file itself.
;;; Credit Bob Webber, Wayne Mesard, Ashwin Ram
;;; Build the tags file with:
;;; % cd /usr/local/emacs/lisp
;;; % ../etc/etags *.el ../src/*.[ch]

(defvar last-emacs-tag nil
  "Tag found by the last find-emacs-tag.")

(autoload 'find-tag-tag "tags"
	  "This hack is here  because there's no provide in tags.el!")

(defun find-emacs-tag (emacs-tagname &optional next other-window)
  "Invoke find-tag on EMACS-TAGNAME using the Emacs tag
table.  The state of the tags variables (tags-file-name and
last-tag) are preserved so that a user can interleave calls
to find-tag and find-emacs-tag.
 If second arg NEXT is non-nil (interactively, with prefix
arg), searches for the next tag in the tag table that
matches the tagname used in the previous find-emacs-tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find Emacs tag: ")))
  (let ((tags-file-name "/usr/local/emacs/lisp/TAGS")
	(last-tag last-emacs-tag))
    (find-tag emacs-tagname next other-window)
    (setq last-emacs-tag last-tag)))
