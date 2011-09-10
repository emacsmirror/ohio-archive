;; buffi.el ---  BUildFile FInder, a smart compile wrapper to build multiple [java] projects

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: buffi.el
;; Created:  10 May 1999
;; Version: 0.1
;; Author: Raphael Pierquin <raphael.pierquin@agisphere.com>, Court Demas <court@acm.org>
;; URL: http://www.agisphere.com/~pierquin/emacs
;; Description: wraps 'compile' to build multiple [java] projects

;; Compatibility: tested on Emacs20.7

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; buffi allows you to easily work on multiple projects with different
;; buildfiles at the same time.  (Note : I call a 'buildfile' any file
;; that drive a compilation sequence, such as Makefiles, or ant's
;; build.xml files)  It's especially useful for Java projects where
;; you're usually working with source code in subdirectories with the
;; buildfiles somewhere up the directory tree.  The idea is that you
;; may have a dozen source files (including buildfiles) open from
;; different projects at the same time.  If I'm editing a file from
;; Project A and execute 'buffi-build' (bound to C-c c) then I
;; probably want the buildfile from that project to be executed.  The
;; buildfile is most likely either in the current directory or one of
;; its parents, and the code below will perform that search and
;; execute the "make" (or "ant") command in the appropriate directory.
;; The other case is when you execute 'buffi-build' and are NOT on a
;; regular source file (maybe you're in some random documentation
;; buffer or something).  In this case it will do the best it can and
;; execute the make on the first buildfile buffer it can find.

;; Here's what is does :
;; buffi-build():
;;  if current buffer is buildfile, build upon it
;;  if not,
;;  if current directory has a buildfile, build upon it
;;    if not recurse to parent
;;  if none of the parents has a buildfile, 
;;     find a buildfile buffer and build upon it
;;  otherwise, 
;;     report an error.

;;; Install :

;; put buffi.el in you load path, and add the following in your .emacs :

;; (require 'buffi.el)

;; Tested with emacs 20.7



;; here's the UI :)
(defun buffi-build nil 
  "runs compile with buildfile found by buffi-find-buildfile"
  (interactive)
  ( progn
    (setq buffi-buildfile nil)
    (setq buffi-buildfile (buffi-find-buildfile))
    ( if buffi-buildfile
	(buffi-build-with-this buffi-buildfile )
      (message '"buffi found nothing to build !"))))

(defun buffi-find-buildfile ()
  "find the buildfile"
  (interactive)
  ( or
    ( ; first try current buffer
      buffi-buildbufferp   (current-buffer) )
    ( ; then try current and upper directories
      ; (current directory is where current buffer file is, if there is one.)
      if
	 ( buffer-file-name (current-buffer))
	 ( buffi-find-here  (file-name-directory (buffer-file-name (current-buffer)))))
    ( ; then try every buffer
      buffi-find-first-in-list 'buffi-buildbufferp  (buffer-list))))


;; internals
(defun buffi-build-with-this (file-or-buffer)
  "switch to file-or-buffer if file-or-buffer is a buffer, and run the build command with file-or-buffer."
  (if
      (bufferp file-or-buffer)
      ( progn
	(switch-to-buffer file-or-buffer t)
	(buffi-build-with-file (buffer-file-name file-or-buffer)))
    (buffi-build-with-file file-or-buffer)))


(defun buffi-build-with-file (filename)
  "run the build command with buildfile filename"
  (compile (concat 
	    "cd "
	    (file-name-directory filename)
	    " ; "
	    ( buffi-get-command filename )
	    " "
	    (file-name-nondirectory filename))))


(defun buffi-find-here (path)
  "recursively search the buildfile"
  ( or
      ( ; first try every file in path 
        buffi-find-first-in-list 'buffi-buildfilep  (directory-files path t))
      ( unless 
	; give up if we're in root directory
	(member path '("/" "//" "/../" ))
	( ; else try in the parent directory 
	 buffi-find-here
	 (expand-file-name (concat (file-name-as-directory path )
				   "../"))))))

(defun buffi-find-first-in-list (afunction elements)
  "returns first element out of elements, for which function returns true"
  (cond 
   ( ; terminal condition : return nil if there's no more element
    (not elements)
    nil )
   ( ; if running function on first element returns t , return the element too 
    ( funcall afunction ( car elements ))
    ( car elements ))
   ( ; else deal with the rest of elements
    t
    ( buffi-find-first-in-list afunction (cdr elements)))))


(defun buffi-buildbufferp (buffer)
  "returns buffer if buffer file is a buildfile"
  ( if 
      ( and
	( buffer-file-name buffer )
	( buffi-buildfilep (buffer-file-name buffer )))
      buffer
    nil ))    

(defun buffi-buildfilep (filename)
  "returns filename if it's a buildfile" 
  ( if
      ( member 
	(file-name-nondirectory filename)
	(mapcar 'car buffi-buildfiles))
    filename 
    nil))

;; one might need to tune this
(defun buffi-get-command (filename)
  "returns command line to build with buildfile filename"
  ( eval ( cadr (assoc (file-name-nondirectory filename) buffi-buildfiles ))))

;; Here you can add your buildfile here, and the command to run to
;; build upon.
(setq buffi-buildfiles
      '(
	( "build.xml"   '"ant -emacs -buildfile" )
	( "build.sh"    '"sh")
	( "GNUmakefile" '"make -w -f")
	( "makefile"    '"make -w -f")
	( "Makefile"    '"make -w -f")))

(global-set-key "\C-cc" 'buffi-build)

(provide 'buffi)
