;From utkcs2!emory!samsung!usc!snorkelwacker!paperboy!meissner Tue Jun 19 12:06:21 EDT 1990
;Article 3008 of gnu.emacs:
;Xref: utkcs2 comp.emacs:4466 gnu.emacs:3008
;Path: utkcs2!emory!samsung!usc!snorkelwacker!paperboy!meissner
;>From: meissner@osf.org (Michael Meissner)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: Re: cmushell cannot handle cdpath
;Message-ID: <MEISSNER.90Jun19104034@curley.osf.org>
;Date: 19 Jun 90 14:40:34 GMT
;References: <88@ttrnds.UUCP>
;Sender: news@OSF.ORG
;Organization: Open Software Foundation
;Lines: 114
;In-reply-to: dave@ttrnds.UUCP's message of 18 Jun 90 16:23:03 GMT
;
;In article <88@ttrnds.UUCP> dave@ttrnds.UUCP (David M. Karr) writes:
;
;| I just installed cmushell.el.  It seems to work pretty well, except
;| for one really huge problem.  The directory tracker does not know
;| about directories reached through the "cdpath" variable.  I believe
;| there was a released solution for this problem to use with shell.el,
;| but I don't know if that solution will work for cmushell.  I assume it
;| would have to be SOMETHING like what is done for shell.el.
;
;I posted this last year.  If you load the following lisp file, it
;replaces the 'cd' elisp function, with one that does know about
;CDPATH.  You have to use load and not autoload, since cd is already
;bound into emacs.  I use cmushell all of the time, and it works for
;me.
;
;; File input and output commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Replacement for "cd" that knows about CDPATH.

(defun pwd ()
  "Show the current default directory."
  (interactive nil)
  (message "Directory %s" default-directory))

(defun cd-orig (dir)
  "Make DIR become the current buffer's default directory."
  (interactive "DChange default directory: ")
  (setq dir (expand-file-name dir))
  (if (not (eq system-type 'vax-vms))
      (setq dir (file-name-as-directory dir)))
  (if (not (file-directory-p dir))
      (error "%s is not a directory" dir)
    (setq default-directory dir))
  (pwd))


(defun cd-expand-cdpath (dir)
  "Expand DIR like the shells would, by using the environment variable CDPATH
for directory prefixes.  If the directory is not found, the original directory
is returned."

  (interactive "FDirectory (uses CDPATH): ")
  (if (string-match "^[/~]" dir)
      dir				; directory already expanded
    (progn
      (let (done dir2 cd-prefix cd-dir cd-start cd-colon cd-path)

	(setq cd-start 0)
	(setq dir2 (substitute-in-file-name dir))
	(if (not (setq cd-path (getenv "CDPATH")))
	    (setq cd-path ""))
	
	(while (and cd-path (not done))
	  (progn
	    (setq cd-colon (string-match ":" cd-path cd-start))
	    (setq cd-prefix (substring cd-path cd-start cd-colon))
	    (if cd-colon
		(setq cd-start (+ cd-colon 1))
	      (setq cd-path nil)
	      )
	    
	    (if (> (length cd-prefix) 0)
		(progn
		  (setq cd-dir cd-prefix)
		  (if (or (string-equal cd-prefix "~")
			  (not (string-equal (substring cd-dir -1) "/")))
		      (setq cd-dir (concat cd-dir "/"))
		    )
		  )
	      (setq cd-dir "")
	      )
	    
	    (setq cd-dir (expand-file-name (concat cd-dir dir2)))
	    (setq done (file-directory-p cd-dir))
	    )
	  )

	(if done cd-dir dir)
      )
    )
  )
)


(defun cd (dir)
  "Make DIR become the current buffer's default directory.  Searches the
environment variable CDPATH for directory prefixes, just like the shells."

  (interactive "FChange default directory (uses CDPATH): ")
  (cd-orig (cd-expand-cdpath dir))
)
;--
;Michael Meissner	email: meissner@osf.org		phone: 617-621-8861
;Open Software Foundation, 11 Cambridge Center, Cambridge, MA
;
;Do apple growers tell their kids money doesn't grow on bushes?


