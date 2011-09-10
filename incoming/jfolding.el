;; jfolding.el - quick and dirty folding package for emacs
;; $Revision: 1.8 $
;; $Date: 2000/06/26 15:46:18 $

;; This file is not part of Emacs
;; Nor is it part of the JDE. 
;; It is an add-on to both of them however!

;; Author: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord
;; Keywords: java, tools, folding

;; Copyright (c) 1999 Phillip Lord

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Status
;; 
;; It should be made clear that this software is a hack. It works
;; for me, and I release it in the hope that it will work for you
;; also. But it may not. I would hope some day that I do this 
;; a lot better, or probably re-write the entire package a lot 
;; more nicely, but its potentially a large task so I dont know when
;; I will get to it. 
;;
;; If you use this software I would be interested to hear from 
;; your experiences, and will be happy to incorporate bug fixes, 
;; on the basis that somethings are going to be beyond fixing!
;;
;; The software was designed, written and tested on win 95, using
;; NTEmacs and JDE2.1.5, and a 2.1.6 beta reliease. Please let
;; me know if it works elsewhere. The current version should
;; be available at http://genetics.ich.ucl.ac.uk/plord

;;;
;;   Installation
;;
;; This uses one non standard emacs packages.This is called
;; "folding mode" which you can get at http://www.csd.uu.se/~andersl/emacs.shtml#folding
;; If you are lucky you should be able to install this package
;; by placing this file somewhere in your load path and then 
;; inserting (require 'jfolding) somewhere in your .emacs. It used to
;; also require the JDE (Java development environment), for what I
;; have since realised was a silly reason. I would say however though
;; it you are editing java files and you are not using the JDE then
;; this you should be. Its very good, its very easy to install, and
;; all of its extra functionality can be accessed from a menu so you
;; dont have to spend a week learning the keybindings. Though you can
;; if you want to. And as you are an emacs user you probably will want to!
;; If you have problems with compile-mode its probably this, as I take
;; the dubious route of over-writing one of its standard functions. 

;;;
;; Usage:
;; 
;; This package folds away java methods and block comments. 
;; There is a method jfolding-hide-all which hides all of the
;; methods. The normal "hide-all" from hide-show does the wrong
;; thing. The folding works fine using the folding package commands, 
;; in the "fld" method. You can also fold and unfold with mouse 
;; clicks (most of the time). The right mouse button is bound to
;; toggle a folded block. The way to get this to work is to 
;; point your mouse at say an open brace, left click, to move point 
;; to the brace and right click to fold. 
;;
;; Its not as hard as it sounds
;;

;;; 
;;  User visible changes 
;;
;;  1.4 no longer needs regexp for hiding all command. Jump to source
;;  from compile mode causes auto un-hiding. 

;;; 
;; Limitations:
;; 1) This is a hack
;; 2) It puts an extra two menus up which dont do much
;; 3) initialiser blocks confuse the hell out of it. 
;; 4) although it now auto-unhides methods jumped to from the compile 
;; buffer it gets the position of point wrong. This should be fixed in
;; the next version of Emacs!
;; 5) There is a problem with sexp matching in cc-mode which I cant
;; quite figure out. I have the bug down quite well sorted though. The
;; problem is in JDE mode if you have a comment with for instance
;; (blah 's) the two brackets will fail to match. If you have for
;; instance (blah 's)(blah 's) then the first of the four brackets
;; will match the last and the middle two willl both be identified as
;; mismatched. Alll of this occurs because of the backquote because if
;; its not there or if its replaced with a forward quote it all goes
;; wrong. Confusing eh?



;;folding does all of the selective display of the comments
(require 'folding)
;;hide-show does all the selective display of the methods
(require 'hideshow)

;;This variable controls whether auto hiding is on or off
(defvar jfolding-auto-hide-on-load nil )
(defvar jfolding-auto-fold-on-load nil )

(add-hook 'jde-mode-hook 'jfolding-jde-mode-hook)


(defun jfolding-jde-mode-hook()
  "Initialisation hook for JDE. At the moment this switches on folding mode"
  (interactive)
  ;;we want to disable this if its a back up file name as folding is
  ;;annoying when looking at cvs files
  (if (not (or (not buffer-file-name)
	       (backup-file-name-p 
		buffer-file-name)))
      (progn
	;;Now calling folding mode is okay
	(if jfolding-auto-fold-on-load
	    (folding-mode 1))
	;;Load hideshow to hide program blocks. 
	;;If hs-minor-mode is switched off then turn it on
	(if (not hs-minor-mode)
	    (hs-minor-mode))
	;;Auto hide down to function levels
	(if jfolding-auto-hide-on-load
	    (jfolding-hide-all-on-load)))))


;;This means that the right mouse button will operate to open and
;;close java methods.
(define-key java-mode-map [mouse-3] 'jfolding-hideshow-mouse-operation)


;;The following defines folding operations. Its done rather clumsily at the moment
;;with folding.el being used to close and open comments, and hideshow being used to
;;and open methods, and classes. Rather a kludge. But it works for the moment, till
;;I get around to pushing the two together.  
(defun jfolding-hideshow-mouse-operation (event)
  "Cheesy Mouse handling function for hideshow mode. If we are on a { 
near a hidden block, open it, otherwise close it"
  (interactive "e")
  ;;If we are at the right spot
  (save-excursion
    (if (looking-at "{")
	;;Toggle hide/show
	(progn (goto-char (+ 1 point))
	       (if (hs-already-hidden-p)
		   (hs-show-block)
		 (hs-hide-block))))))

(defun jfolding-hide-all-on-load()
  "Calls `jfolding-hide-all' on loading  depending
on the variable `jfolding-auto-hide-on-load'"
  (if jfolding-auto-hide-on-load 
      (jfolding-hide-all)))


(defun jfolding-hide-all()
  "Specialised version of hide-all, which hides all of the text
in code, and then expands the first block. The practical upshot
of all this is that only the function names are displayed."
(interactive)
(save-excursion
(progn
  ;;Make sure that everything is expanded first
  ;;This is in case the main class has been hidden
  (hs-show-all)
  ;;Move to the first char
  (goto-char(point-min))
  ;;This code was suggested by Jesper Nordenberg!
  (hs-hide-level 2)
  (goto-char(point-min)))))


(defun jfolding-toggle-autohide()
"Toggle the status of the `jfolding-auto-hide-on-load' variable"
(interactive)
(if jfolding-auto-hide-on-load
    (progn
      (setq jfolding-auto-hide-on-load nil )
      (message "Auto hide on load toggled off" ))
  (progn
    (setq jfolding-auto-hide-on-load t )
    (message "Auto hide on load toggled on" ))))



;;This is the line for loading the folding mode, which I am going 
;;to try out to see whether I can make it fold away JavaDoc comments
;;More wonders from the font-lock-man. 
;;This little folding mode addition covers up comments, of which 
;;I write too many anyway
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)


(folding-add-to-marks-list 'c-mode "/* {{{ " "/* }}} */" " */" t)
(folding-add-to-marks-list 'java-mode "//{{{ " "//}}}" nil t)
(folding-add-to-marks-list 'jde-mode "/*" "*/"  nil  t )

(provide 'jfolding) 

;;
;;
;; $Log: jfolding.el,v $
;; Revision 1.8  2000/06/26 15:46:18  lord
;; Updated mouse hide show operation to cope with other coding styles.
;; Thanks to Marc Grushcow <Marc.Grushcow@NTT.ca> for the bug report
;;
;; Revision 1.7  2000/06/19 21:55:05  phil
;; cosmetic
;;
;; Revision 1.6  2000/03/29 16:00:03  lord
;; Lots of changes
;;
;; Revision 1.5  2000/01/25 14:04:42  lord
;; Modified compile.el hack.
;;
;; Revision 1.4  1999-10-05 19:05:46+01  phillip2
;; Tidied things up somewhat.
;; New code has removed need for regexp recognising begining of methods.
;; Thanks to  Jesper Nordenberg <jesper@nnl.se> for contributing this code
;;
;; Revision 1.3  1999-10-04 18:01:28+01  phillip2
;; Changed hook function slightly. Added new limitation!
;;
;; Revision 1.2  1999-09-19 21:42:31+01  phillip2
;; fold-add-to-marks-list updated to folding-add-to-marks due to change in folding.el
;; Thanks to Spencer Marks <smarks@digisolutions.com> for the bug report
;;
;; Revision 1.1  1999-09-19 21:40:11+01  phillip2
;; Initial revision
;;
;;
;;