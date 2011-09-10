;Date: 13 Feb 89 20:33:20 GMT
;From: rti!xyzzy!tiktok!meissner@mcnc.org  (Michael Meissner)
;Subject: Re: shell mode & $cdpath
;To: bug-gnu-emacs@prep.ai.mit.edu
;
;In article <8902092136.AA16863@sunfs3.camex.uucp> lloyd!sunfs3!geoff@HSCFVAX.HARVARD.EDU (Geoffrey Knauth) writes:
;| 
;|    Date: Thu, 9 Feb 89 16:30:46 EST
;|    From: tod@sunfs2 (Tod Hagan)
;| 
;|    The manual indicates that in shell mode emacs goes to great lengths to
;|    recognize cd, pushd and popd commands so that it may keep the
;|    '*shell*' buffer's default directory the same as the working directory
;|    of the shell.  I see that it can deal with aliases for those commands
;|    but I see nothing about handling the 'cdpath' variable in the c-shell.
;|    Is there an undocumented feature for handling 'cdpath'?
;| 
;| Emacs recognizes all my cd / pushd / popd commands, so long as they
;| are typed right after the prompt.  I don't know about $cdpath.  I'll
;| have to look or ask.  Note that:
;| 
;|   % do_something_here_and_then; cd somewhere_else
;| 
;| leaves emacs thinking it's still in the old directory.  (Try "M-x pwd".)
;| 
;
;Funny you should ask....  I recently had the same frustration with the
;Bourne Shell's CDPATH environment variable.  I wrote the following
;script to compensate (note, it must be LOADed rather than autoloading,
;since the 'cd' function is normally already loaded in the initial
;emacs image).  Dynamic binding is fairly useful for these kinds of
;things....  Strip off the header and my signature line, byte compile
;it, set up your .emacs file to load it, and off you go.  For the csh,
;if it doesn't use CDPATH like /bin/sh, just hack the place where I do
;the getenv to use the appropriate definition.
;
;-------------------- cd.el --------------------
;; File input and output commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is not yet a part of GNU Emacs, but FSF is welcome to it.

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
;; Replacement for "cd" that knows about CDPATH.  Provided by
;; Michael Meissner, Data General Corporation.
;; meissner@dg-rtp.DG.COM

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
;Michael Meissner, Data General.
;Uucp:	...!mcnc!rti!xyzzy!meissner
;Arpa:	meissner@dg-rtp.DG.COM   (or) meissner%dg-rtp.DG.COM@relay.cs.net

