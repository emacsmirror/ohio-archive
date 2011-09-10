;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Maintainer: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Created: April 1992
;; Version: 1.0
;; Last-modified: 20 Jul 1994
;; Keywords: news

;; LCD Archive Entry:
;; chastise|Michael Ernst|mernst@theory.lcs.mit.edu|
;; Remind posters of inappropriate netnews of proper netiquette (was chastize.el)|
;; 20-Jul-1994|1.1|~/misc/chastise.el.Z|

;; This file is distributed under the same conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify;; it under the terms of the GNU General Public License as published by;; the Free Software Foundation; either version 1, or (at your option);; any later version.;; GNU Emacs is distributed in the hope that it will be useful,;; but WITHOUT ANY WARRANTY; without even the implied warranty of;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the;; GNU General Public License for more details.;; You should have received a copy of the GNU General Public License;; along with GNU Emacs; see the file COPYING.  If not, write to;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Inappropriate netnews postings are usually unintentional, caused by
;; ignorance of where the information can be found (in the case of
;; frequently asked questions) or of a newsgroup's charter (in the case of
;; messages directed to the wrong newsgroup).  Most posters appreciate
;; being given this information.

;; This package creates replies to inappropriate netnews postings; with
;; two keystrokes, you can remind the poster of proper netiquette.
;; Presently it's only set up for gnu.emacs.sources and gnu.emacs.help.
;; (Additions for other newsgroups are welcome.)
;; It was inspired by Devon McCullough's chastise-twit package for RMAIL.

;; To install, put this file on your load-path and, optionally,
;; byte-compile it.  Add something like this to your gnus initialization:
;;  (define-key gnus-Subject-mode-map "\C-c\C-t" 'gnus-Subject-chastise-twit)
;;  (autoload 'gnus-Subject-chastise-twit "chastise"
;;  	  "Chastise poster of inappropriate netnews message." t nil)
;;  (define-key gnus-Subject-mode-map "\C-c\C-f" 'gnus-Subject-chastise-faq)
;;  (autoload 'gnus-Subject-chastise-faq "chastise"
;;  	  "Chastise LCD-ignorant poster of netnews message." t nil)
;;  ;; Emacs Lisp specific
;;  (define-key gnus-Subject-mode-map "\C-c\C-l" 'gnus-Subject-chastise-lcd)
;;  (autoload 'gnus-Subject-chastise-lcd "chastise"
;;  	  "Chastise LCD-ignorant poster of netnews message." t nil)
;;  (define-key gnus-Subject-mode-map "\C-cl" 'gnus-Subject-lcd-contribute)
;;  (autoload 'gnus-Subject-lcd-contribute "chastise"
;; 	  "Suggest that the poster submit this code to the GELCD." t nil)

;; Then, in GNUS, keystroke C-c C-t (chastise-twit) will produce a reply like
;;   Your message is inappropriate for newsgroup gnu.emacs.sources...
;; Keystroke C-c C-l (chastise-lcd) will tell where to find the GNU Emacs
;; Lisp Code Directory, answering the frequently asked question, "Where can
;; I find Emacs code to do XYZ?"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNUS version
;;;

(defvar running-gnus-4 (string-match "^GNUS 4" gnus-version))
(defmacro gnus-3 (&rest body)
  (` (if (not running-gnus-4)
	 (progn
	   (,@ body)))))
(defmacro gnus-4 (&rest body)
  (` (if running-gnus-4
	 (progn
	   (,@ body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;

(defvar chastise-signature nil
  "A signature to append to chastisements, similar to variable `mail-signature'.")

(defvar chastise-article-separator
  "\n\n---------------------- your message ------------------------------------\n\n"
  "A string which separates the chastisement from the original posting.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNUS interface
;;;

(defvar chastise-newsgroup-message-assoc-list
  '(("gnu.emacs.help"
     "Your message is inappropriate for newsgroup gnu.emacs.help (mailing list
help-gnu-emacs@prep.ai.mit.edu), which is intended for discussions about GNU
Emacs.  Please direct discussion of freed software, the GNU Project, the
GNU Manifesto, and their implications to newsgroup gnu.misc.discuss (mailing
list gnu-misc-discuss@cis.ohio-state.edu).")
    )
  "Assoc list of newsgroup names and chastisement messages (both strings).")

(defvar chastise-newsgroup-method-assoc-list
  '(("gnu.emacs.sources" gnu-emacs-sources-chastisement))
  "Assoc list of newsgroup names (strings) and functions (of no arguments)
to be called to produce a chastisement message.")

(defun gnu-emacs-sources-chastisement ()
  (let ((appropriate
	 (cond ((y-or-n-p "Does this message belong in gnu.emacs.help? ")
		"  Your message\nbelongs in gnu.emacs.help.")
	       ((y-or-n-p "Does this message belong in gnu.emacs.bug? ")
		"  Your message\nbelongs in gnu.emacs.bug.")
	       ((y-or-n-p "Does this message belong in gnu.misc.discuss? ")
		"  Your message\nbelongs in gnu.misc.discuss.")
	       (t ""))))
    (concat "Your message is inappropriate for newsgroup gnu.emacs.sources (mailing list
gnu-emacs-sources@prep.ai.mit.edu), which is intended only for GNU Emacs
source code, typically GNU Emacs Lisp packages or functions."
	    appropriate
	    "\n
Please be more careful in the future.  Direct all requests, questions,
answers, and discussions regarding Emacs or Emacs sources to
help-gnu-emacs@prep.ai.mit.edu (newsgroup gnu.emacs.help).  Send bug
reports for Emacs to bug-gnu-emacs@prep.ai.mit.edu.  File etc/MAILINGLISTS
in your Emacs distribution gives more details about the intended use of
each list; please abide by the wishes of those who established these fora,
even if others fail to do so or you personally disagree with their criteria
for appropriateness.\n\n")))

(defun gnus-Subject-chastise-twit ()
  "Chastise the poster of this inappropriate netnews message."
  (interactive)
  (let ((message (or (car (cdr (assoc gnus-newsgroup-name
				      chastise-newsgroup-message-assoc-list)))
		     (funcall (or (car (cdr (assoc gnus-newsgroup-name
						   chastise-newsgroup-method-assoc-list)))
				  (function
				   (lambda ()
				     (error "No chastisement prepared for %s."
					    gnus-newsgroup-name))))))))
    (if (not message)
	(error "Couldn't find a chastisement message for this newsgroup."))
    (gnus-3 (gnus-Subject-mail-reply t))
    (gnus-4 (gnus-summary-reply t))
    (insert message)
    (cond ((eq chastise-signature t)
	   (if (file-exists-p "~/.signature")
	       (insert-file-contents "~/.signature")))
	  (chastise-signature
	   (insert chastise-signature)))
    (insert chastise-article-separator)
    (untabify (point-min) (point-max))
    ;; Clean up the inserted text a bit (assumes quoting via "> ").
    (save-excursion
      (while (re-search-forward "^\\(   \\|$\\)" nil t)
	(replace-match "> " nil nil))
      (goto-char (point-max))
      (while (re-search-backward "^> \\'" nil t)
	(replace-match "" nil nil)))
    ;; Kill BCC: line, if it exists.
    (mail-bcc)
    (beginning-of-line 1)
    (kill-line 1)))


(defun gnus-Subject-chastise-lcd (subject &optional news)
  "Chastise poster of netnews message who should have checked the LCD first.
Send personal mail; if optional prefix argument NEWS is supplied, post netnews."
  (interactive "sLCD subject: \nP")
  ;;   (let (method
  ;; 	message
  ;; 	(cursor-in-echo-area t))
  ;;     (message "LCD-inform via mail or news? ")
  ;;     (while (not method)
  ;;       (setq method (downcase (read-char)))
  ;;       (if (not (member method '(?m ?n)))
  ;; 	  (progn
  ;; 	    (message "Please answer m or n.  LCD-inform via mail or news? ")
  ;; 	    (setq method nil))))
  ;;     (setq message (chastise-make-lcd-message subject))
  ;;    ...)
  (if news
      (progn
	(gnus-3 (gnus-Subject-post-reply nil))
	(gnus-4 (gnus-summary-followup nil))
	(insert (chastise-make-lcd-message subject))
	(cond ((eq chastise-signature t)
	       (if (file-exists-p "~/.signature")
		   (insert-file-contents "~/.signature")))
	      (chastise-signature
	       (insert chastise-signature)))	   
	(insert chastise-article-separator)
	(news-reply-yank-original nil))
    (progn
      (gnus-3 (gnus-Subject-mail-reply nil))
      (gnus-4 (gnus-summary-reply nil))
      (insert (chastise-make-lcd-message subject))
      (cond ((eq chastise-signature t)
	     (if (file-exists-p "~/.signature")
		 (insert-file-contents "~/.signature")))
	    (chastise-signature
	     (insert chastise-signature)))
      (insert chastise-article-separator)
      (mail-yank-quote nil))))


(defun gnus-Subject-lcd-contribute ()
  "Suggest that the poster submit this code to the GNU Emacs Lisp Code Directory."
  (interactive)
  (if (save-excursion
	(set-buffer gnus-Article-buffer)
	(goto-char (point-min))
	(search-forward "LCD Archive Entry:" nil t))
      (error "This posting already has an LCD Archive Entry.")
    (progn
      (gnus-3 (gnus-Subject-mail-reply nil))
      (gnus-4 (gnus-summary-reply nil))
      (insert "Thanks for contributing this.  Could you submit it to the GNU Emacs Lisp
Code Directory as well?  (I presume you haven't done so yet because I don't
see an LCD archive entry.)  That will permit more people to find it.

Thanks.\n\n")
      (if chastise-signature (insert chastise-signature "\n\n"))
      (insert "\n")
      (insert-file "~/emacs/lcd-guidelines")
      (emacs-18
       (search-backward mail-header-separator))
      (emacs-19
       (mail-text))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for specific messages
;;;

(defun chastise-make-lcd-message (subject)
  (require 'lispdir)
  (if (get-buffer "*GNU Emacs Lisp Code Directory Apropos*")
      (kill-buffer "*GNU Emacs Lisp Code Directory Apropos*"))
  (lisp-dir-apropos subject)
  (if (get-buffer "*GNU Emacs Lisp Code Directory Apropos*")
      (concat "I suggest you get a copy of lispdir.doc, lispdir.el.Z, and perhaps
LCD-datafile.Z from archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive.
\(This is the GNU Emacs Lisp Code Directory, which describes all known Emacs
packages.\)  Then you'll be able to type M-x lisp-dir-apropos <RET> "
	      subject
	      "
<RET> yourself, and see the following.  (You can also use the searchable
index http://www.cs.indiana.edu/LCD/cover.html.)\n\n"
	      (save-excursion
		(set-buffer "*GNU Emacs Lisp Code Directory Apropos*")
		(buffer-substring (point-min) (point-max)))
	      "\n
If you learn of any other appropriate Emacs Lisp code, please send mail
to the GELCD maintainers at elisp-archive@cis.ohio-state.edu so that they
can include it in their list and perhaps provide an ftp site for it.\n\n")
    (concat "I could not find such a package in the GNU Emacs Lisp Code Directory, which
describes all known Emacs code.  If you find one, please send mail to
  GNU Emacs Lisp Code Directory <elisp-archive@cis.ohio-state.edu>
so that it can be added.  In the meanwhile, I suggest you get a copy of
lispdir.doc, lispdir.el.Z, and perhaps LCD-datafile.Z from
archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive.  Then you'll be
able to type M-x lisp-dir-apropos <RET> "
	    subject
	    " <RET> yourself to see the
packages relating to "
	    subject
	    ".  (Alternately, you can use the
searchable index http://www.cs.indiana.edu/LCD/cover.html.)\n\n")))
