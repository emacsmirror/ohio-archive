;;; news-hack.el --- create newsrc buffers for foreign groups
;; This file is not part of GNU Emacs
;; This is released under the GNU Public License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This is poorly documented, and is likely to stay that way for the
;; forseeable future.  Any help appreciated.
;; Mail me for help, with comments etc.  I like getting mail (that is
;; actually to me, spam I can't stand.)

;; Instructions i)  Run netscape-news-create-newsrc-buffers
;;              ii) Save the buffers

;; This uses *a lot* of recursive functions.  If you get errors, you may need
;; to increase max-lisp-eval-depth to something higher.
;; Put
;; (setq max-lisp-eval-depth 10000)
;; in your .emacs file.  (Replace 10000 with some larger number if necessary)

;; "Author": gowen+usenet@ma.man.ac.uk
;;  URL    : http://www.ma.man.ac.uk/~gowen/lisp/

;;; Bugs
;;
;; If your local server is called `dribble' it won't work, since
;; `.newsrc-dribble' is a special buffer in gnus.
;;

(defvar netscape-news-nuke-buffers t
  "Should netscape-news-create-newsrc-buffers overwrite existing buffers?")

(defun netscape-news-parse (newsrc-tmp)
  "Do all that jazz."
  (if newsrc-tmp
      (progn
	(let (newsrc-list-element)
	  (setq newsrc-list-element (car newsrc-tmp))
	  (let (select-method)
	    (progn
	      (setq
	       select-method (car (cdr (cdr (cdr (cdr newsrc-list-element))))))
	      (if select-method
		  (netscape-news-write-buffer
		   newsrc-list-element select-method))
	      (netscape-news-parse (cdr newsrc-tmp))))))))

(defun netscape-news-write-buffer (list-element method)
  "Write newsrc style entries into a suitably named buffer."
  (let ((name (concat ".newsrc-" (car (cdr method)))))
    (get-buffer-create name)
    (set-buffer name)
    (insert-string
     (concat (netscape-news-get-group-name (car list-element)) ": "
	     (netscape-news-get-read-string (car (cdr (cdr list-element))))))))

(defun netscape-news-get-group-name (string)
  "Return newsgroup name from a previously found foreign group name."
  (string-match "[^:]*$" string)
  (match-string 0 string))

(defun netscape-news-get-read-string (read-list)
  "Convert the read list into a string for newsrc-files."
  (let (read-string)
    (while read-list
      (if (listp (car read-list))
	  (setq read-string
		(concat read-string
			(car (car read-list)) "-" (cdr (car read-list)) ","))
	;; else
	(setq read-string (concat read-string (car read-list) ",")))
      (setq read-list (cdr read-list)))
    (concat read-string "\n")))

(defun netscape-news-create-newsrc-buffers ()
  "Parse the variable gnus-newsrc-alist and create buffers with .newsrc files."
  (interactive)
  (netscape-news-nuke-newsrc-buffers)
  (netscape-news-parse gnus-newsrc-alist))

(defun netscape-news-nuke-newsrc-buffers ()
  "Delete the present contents of all .newsrc buffers"
  (let ((buffers (buffer-list)))
    (while buffers
      (if (and (string-match ".newsrc-"(buffer-name (car buffers)))
	       (not (string-match ".newsrc-dribble"
				  (buffer-name (car buffers)))))
	  (netscape-news-nuke-this-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun netscape-news-nuke-this-buffer (buf)
  (set-buffer buf) (widen) (delete-region (point-min) (point-max)))
Path: news.cis.ohio-state.edu!news.ems.psu.edu!newsfeed.stanford.edu!news-spur1.maxwell.syr.edu!news.maxwell.syr.edu!newsfeed.icl.net!nntp.news.xara.net!xara.net!gxn.net!server6.netnews.ja.net!news.keele.ac.uk!not-for-mail
Message-ID: <84r97s88lv.fsf@orr.maths.keele.ac.uk>
From: Gareth Owen <usenet@gwowen.freeserve.co.uk>
Newsgroups: gnu.emacs.sources
Subject: news-hack.el --- create newsrc buffers for foreign groups
Date: 14 Aug 2000 11:01:16 +0100
Lines: 111
Organization: Sirius Cybernetics Corp.
Distribution: world
NNTP-Posting-Host: orr.maths.keele.ac.uk
Mime-Version: 1.0
Content-Type: text/plain; charset=us-ascii
X-Trace: www1.kis.keele.ac.uk 966247314 17116 160.5.82.202 (14 Aug 2000 10:01:54 GMT)
X-Complaints-To: usenet@news.keele.ac.uk
NNTP-Posting-Date: 14 Aug 2000 10:01:54 GMT
x-no-productlinks: Yes
Microsoft: Making the world a better place... for Microsoft.
User-Agent: Gnus/5.0803 (Gnus v5.8.3) Emacs/20.5
Xref: neutral.verbum.org gnu.emacs.sources:464

I upgraded to Gnus 5.8.3 and this broke, so I unbroke it again...

I'm sure there must be a built in way to do this, but I don't know what it is,
and this works just fine for me...

;;; news-hack.el --- create newsrc buffers for foreign groups
;; This file is not part of GNU Emacs
;; This is released under the GNU Public License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This is poorly documented, and is likely to stay that way for the
;; forseeable future.  Any help appreciated.
;; Mail me for help, with comments etc.  I like getting mail (that is
;; actually to me, spam I can't stand.)

;; Instructions i)  Run netscape-news-create-newsrc-buffers
;;              ii) Save the buffers

;; This uses *a lot* of recursive functions.  If you get errors, you may need
;; to increase max-lisp-eval-depth to something higher.
;; Put
;; (setq max-lisp-eval-depth 10000)
;; in your .emacs file.  (Replace 10000 with some larger number if necessary)

;; "Author": usenet@gwowen.freeserve.co.uk
;;  URL    : http://www.ma.man.ac.uk/~gowen/lisp/

;;; Bugs
;;
;; If your local server is called `dribble' it won't work, since
;; `.newsrc-dribble' is a special buffer in gnus.
;;

(defvar netscape-news-nuke-buffers t
  "Should netscape-news-create-newsrc-buffers overwrite existing buffers?")

(defun netscape-news-parse (newsrc-tmp)
  "Do all that jazz."
  (if newsrc-tmp
      (progn
	(let (newsrc-list-element)
	  (setq newsrc-list-element (car newsrc-tmp))
	  (let (select-method)
	    (progn
	      (setq
	       select-method (car (cdr (cdr (cdr (cdr newsrc-list-element))))))
	      (if select-method
		  (netscape-news-write-buffer
		   newsrc-list-element select-method))
	      (netscape-news-parse (cdr newsrc-tmp))))))))

(defun netscape-news-write-buffer (list-element method)
  "Write newsrc style entries into a suitably named buffer."
  (let ((name (concat ".newsrc-"
		      (if (listp method) (car (cdr method))
			method))))
    (get-buffer-create name)
    (set-buffer name)
    (insert-string
     (concat (netscape-news-get-group-name (car list-element)) ": "
	     (netscape-news-get-read-string (car (cdr (cdr list-element))))))))

(defun netscape-news-get-group-name (string)
  "Return newsgroup name from a previously found foreign group name."
  (string-match "[^:]*$" string)
  (match-string 0 string))

(defun netscape-news-get-read-string (read-list)
  "Convert the read list into a string for newsrc-files."
  (let (read-string)
    (while read-list
      (if (listp (car read-list))
	  (setq read-string
		(concat read-string
			(car (car read-list)) "-" (cdr (car read-list)) ","))
	;; else
	(setq read-string (concat read-string (car read-list) ",")))
      (setq read-list (cdr read-list)))
    (concat read-string "\n")))

(defun netscape-news-create-newsrc-buffers ()
  "Parse the variable gnus-newsrc-alist and create buffers with .newsrc files."
  (interactive)
  (netscape-news-nuke-newsrc-buffers)
  (netscape-news-parse gnus-newsrc-alist))

(defun netscape-news-nuke-newsrc-buffers ()
  "Delete the present contents of all .newsrc buffers"
  (let ((buffers (buffer-list)))
    (while buffers
      (if (and (string-match ".newsrc-"(buffer-name (car buffers)))
	       (not (string-match ".newsrc-dribble"
				  (buffer-name (car buffers)))))
	  (netscape-news-nuke-this-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun netscape-news-nuke-this-buffer (buf)
  (set-buffer buf) (widen) (delete-region (point-min) (point-max)))

