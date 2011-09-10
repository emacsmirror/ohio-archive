;From ark1!uakari.primate.wisc.edu!gem.mps.ohio-state.edu!tut.cis.ohio-state.edu!tds.kth.se!juha Thu Oct 26 11:00:57 1989
;Article 591 of gnu.emacs
;Path: ark1!uakari.primate.wisc.edu!gem.mps.ohio-state.edu!tut.cis.ohio-state.edu!tds.kth.se!juha
;>From juha@tds.kth.se (Juha Sarlin)
;Newsgroups: gnu.emacs
;Subject: Another way of finding a directory in Emacs
;Message-ID: <8910251857.AA09126@ttds.tds.kth.se>
;Date: 25 Oct 89 20:57:39 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 104
;
;Below is Emacs Lisp code for my version of a searching "cd" command.
;It differs from other similar versions I've seen in that it does not
;change the default directory of the current buffer, but instead
;creates a buffer in dired-mode for the directory.
;
;There are two versions; one that reads in the directory list and one
;that doesn't.  The latter can be useful because the reading is usually
;rather slow.
;
;-------------------- find-dir.el --------------------
;; Find directory using cd-path.
;; Copyright (C) 1989 Juha Sarlin	<juha@tds.kth.se>

;; This file is an unofficial part of GNU Emacs.

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

;; To use this you can for example add the following lines to your .emacs file:
;;	(autoload 'find-directory-noread "find-dir" nil t)
;;	(global-set-key "\C-C\C-D" 'find-directory-noread)
;; If you prefer to read in the directory list by default use this instead:
;;	(autoload 'find-directory "find-dir" nil t)
;;	(global-set-key "\C-C\C-D" 'find-directory)
;;
;; To define the search path you can either define the environment
;; variable CDPATH before starting emacs. In bash, for example:
;;	export CDPATH=.:/usr/gnu:/usr/local/src
;; Or you can define cd-path in your .emacs file:
;;	(setq cd-path '(nil "/usr/gnu" "/usr/local/src"))
;; Note the use of nil instead of ".". This is explained in the
;; documentation for cd-path below.

(defvar cd-path nil
  "*List of directories to search through in dir-search.
Use nil instead of \".\", because expand-file-name doesn't expand \".\"
into an absolute path name.  If you don't set this variable yourself,
its initialized from the CDPATH environment variable.")

(defun substring-list (regexp string)
  "Return list of substrings matched by REGEXP in STRING."
  (let (list)
    (string-match "" "")		;initialize match-end to 0
    (while (and (string-match regexp string (match-end 0))
		;; avoid infinite looping when the match is an empty string
		(/= (match-beginning 0) (match-end 0)))
      (setq list (cons (substring string (match-beginning 0) (match-end 0))
		       list)))
    (nreverse list)))

(defun dir-search (dir)
  "Search for DIR in the directories listed in cd-path.
If cd-path is nil, its initialized from a colon separated list of
directories in the CDPATH environment variable.
Returns directory name if one was found, else nil."
  (if (not cd-path)
      ;; Parse CDPATH and change "." to nil.
      (setq cd-path (mapcar
		     '(lambda (name) (if (string-equal name ".") nil name))
		     (substring-list "[^:]+" (or (getenv "CDPATH") "")))))
  (if (string-equal dir "")		; Empty string means home directory
      (expand-file-name "~")
    (let ((path cd-path) name found)
      (while (and (not found) path)
	(setq name (expand-file-name dir (car path)))
	(if (file-directory-p name)	
	    (setq found name))
	(setq path (cdr path)))
      found)))

(defun find-directory-noread (dir)
  "Search for DIR in the directories listed in cd-path.
If found, switch to a buffer in dired-mode for that directory.
The directory list is not read in by default; you can read it
with the normal dired-mode commands."
  (interactive "sFind directory: ")
  (require 'dired)
  (let ((dirname (dir-search dir)))
    (if dirname
	(progn (setq dirname (file-name-as-directory dirname))
	       (switch-to-buffer (dired-find-buffer dirname))
	       (dired-mode dirname))
      (error "Directory %s not found" dir))))

(defun find-directory (dir)
  "Search for DIR in the directories listed in cd-path.
If found, switch to a buffer in dired-mode for that directory."
  (interactive "sFind directory: ")
  (find-directory-noread dir)
  (dired-revert))
;--
;Juha Sarlin	juha@tds.kth.se
